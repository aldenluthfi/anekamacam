//! # state.rs
//!
//! Defines game state representation and management.
//!
//! This file contains the implementation of game state tracking, including
//! the current position, turn information, move history, and any other
//! state-related data needed for game progression and rule enforcement.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use crate::*;

pub type Square = u16;

/*----------------------------------------------------------------------------*\
                          SPECIAL RULES REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Special-rules bitmask accessor/encoder macros.
///
/// The `special_rules` field in [`State`] uses one bit per optional rule.
/// For each rule there is a pair of macros:
/// - reader    : `rule_name!(state)`
/// - writer    : `enc_rule_name!(rules)`
///
#[macro_export]
macro_rules! castling {
    ($state:expr) => {
        ($state.statics.special_rules & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_castling {
    ($rules:expr) => {
        $rules |= 1;
    };
}

#[macro_export]
macro_rules! en_passant {
    ($state:expr) => {
        ($state.statics.special_rules >> 1 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_en_passant {
    ($rules:expr) => {
        $rules |= 1 << 1;
    };
}

#[macro_export]
macro_rules! promotions {
    ($state:expr) => {
        ($state.statics.special_rules >> 2 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_promotions {
    ($rules:expr) => {
        $rules |= 1 << 2;
    };
}

#[macro_export]
macro_rules! drops {
    ($state:expr) => {
        ($state.statics.special_rules >> 3 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_drops {
    ($rules:expr) => {
        $rules |= 1 << 3;
    };
}

#[macro_export]
macro_rules! forbidden_zones {
    ($state:expr) => {
        ($state.statics.special_rules >> 4 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_forbidden_zones {
    ($rules:expr) => {
        $rules |= 1 << 4;
    };
}

#[macro_export]
macro_rules! promote_to_captured {
    ($state:expr) => {
        ($state.statics.special_rules >> 5 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_promote_to_captured {
    ($rules:expr) => {
        $rules |= 1 << 5;
    };
}

#[macro_export]
macro_rules! stalemate_loss {
    ($state:expr) => {
        ($state.statics.special_rules >> 6 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_stalemate_loss {
    ($rules:expr) => {
        $rules |= 1 << 6;
    };
}

#[macro_export]
macro_rules! setup_phase {
    ($state:expr) => {
        ($state.statics.special_rules >> 7 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_setup_phase {
    ($rules:expr) => {
        $rules |= 1 << 7;
    };
}

#[macro_export]
macro_rules! stand_offs {
    ($state:expr) => {
        ($state.statics.special_rules >> 8 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_stand_offs {
    ($rules:expr) => {
        $rules |= 1 << 8;
    };
}

#[macro_export]
macro_rules! halfmove_clock {
    ($state:expr) => {
        ($state.statics.special_rules >> 9 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_halfmove_clock {
    ($rules:expr) => {
        $rules |= 1 << 9;
    };
}

#[macro_export]
macro_rules! repetition_limit {
    ($state:expr) => {
        ($state.statics.special_rules >> 10 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_repetition_limit {
    ($rules:expr) => {
        $rules |= 1 << 10;
    };
}

/*----------------------------------------------------------------------------*\
                            EN PASSANT REPRESENTATION
\*----------------------------------------------------------------------------*/

pub type EnPassantSquare = u32;

/// En passant packed-field accessor macros.
///
/// [`EnPassantSquare`] stores:
/// - bits 0-11   : target square
/// - bits 12-23  : captured square
/// - bits 24-31  : captured piece index
#[macro_export]
macro_rules! enp_square {
    ($en_passant:expr) => {
        $en_passant & 0xFFF
    };
}

#[macro_export]
macro_rules! enp_captured {
    ($en_passant:expr) => {
        ($en_passant >> 12) & 0xFFF
    };
}

#[macro_export]
macro_rules! enp_piece {
    ($en_passant:expr) => {
        ($en_passant >> 24) & 0xFF
    };
}

/*----------------------------------------------------------------------------*\
                              SNAPSHOT REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Captures reversible state needed to undo a move.
///
/// Each snapshot stores move payload and dynamic counters/flags so `undo_move!`
/// can restore the exact pre-move position, including hash-dependent state.
/// It is appended to `State::history` during move execution.
#[derive(Clone)]
pub struct Snapshot {
    pub move_ply: Move,

    pub castling_state: u8,
    pub halfmove_clock: u8,
    pub en_passant_square: EnPassantSquare,
    pub game_over: bool,
    pub game_phase: u8,
    pub phase_score: u32,

    pub position_hash: u128,
}

impl Default for Snapshot {
    fn default() -> Self {
        Snapshot {
            move_ply: null_move(),
            castling_state: 0,
            halfmove_clock: 0,
            en_passant_square: EnPassantSquare::MAX,
            game_over: false,
            game_phase: OPENING,
            phase_score: 0,
            position_hash: u128::default(),
        }
    }
}

/// Returns whether a snapshot corresponds to a pass move.
///
/// This is used in repetition / stand-off flow where pass detection is needed
/// while reading from undo history rather than the active move stream.
#[macro_export]
macro_rules! pass_snapshot {
    ($snapshot:expr) => {
        is_pass!($snapshot.move_ply)
    };
}

#[macro_export]
macro_rules! game_phase_score {
    ($state:expr) => {{
        let mut phase_score = 0;

        for (piece_idx, piece) in $state.statics.pieces.iter().enumerate() {
            if p_is_big!(piece) && !p_is_royal!(piece) {
                phase_score +=
                    (
                        p_ovalue!(piece) as u32) *
                        ($state.piece_list[piece_idx].len() as u32
                    );
            }
        }

        phase_score
    }};
}

/*----------------------------------------------------------------------------*\
                            GAME STATE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Immutable variant configuration, shared across threads via Arc.
///
/// All 27 static fields that are fixed after `precompute()` live here.
/// `State::clone()` shares this via `Arc::clone` instead of deep-copying.
pub struct StaticState {
    pub title: String,
    pub startpos: String,

    pub pieces: Vec<Piece>,
    pub special_rules: u32,

    pub initial_setup: Vec<Board>,                                              /* piece index to board               */

    pub forbidden_zones: Vec<Board>,                                            /* piece to forbidden zone bitboard   */
    pub promotion_zones_optional: Vec<Board>,                                   /* piece to promotion zone bitboard   */
    pub promotion_zones_mandatory: Vec<Board>,                                  /* piece to promotion zone bitboard   */
    pub critical_castling: [Board; 4],                                          /* KQkq critical squares for each     */

    pub halfmove_limit: u8,                                                     /* halfmoves before draw              */
    pub repetition_limit: u8,                                                   /* number of repetitions for draw     */

    pub halfmove_pieces: Vec<bool>,                                             /* moving these pieces resets clock   */
    pub castling_pieces: Vec<bool>,                                             /* moving/capturing voids rights      */

    pub files: u8,
    pub ranks: u8,
    pub board_size: usize,

    pub relevant_moves: Vec<MoveSet>,                                           /* idx = piece * board size + square  */
    pub relevant_captures: Vec<MoveSet>,                                        /* flattened because of cache         */
    pub relevant_drops: Vec<DropSet>,                                           /* optimization                       */
    pub relevant_setup: Vec<DropSet>,
    pub relevant_stand_offs: Vec<PatternSet>,
    pub relevant_attacks: [Vec<Vec<AttackMask>>; 2],
    pub relevant_castling: [Vec<Move>; 4],                                      /* KQkq precomputed moves             */

    pub piece_swap_map: Vec<PieceIndex>,                                        /* piece index to swap color (if any) */
    pub piece_demotion_map: Vec<PieceIndex>,                                    /* piece index to demotion piece idx  */
    pub piece_char_map: HashMap<char, PieceIndex>,                              /* char to piece index map            */

/*----------------------------------------------------------------------------*\
                                 SEARCH FIELDS
\*----------------------------------------------------------------------------*/

    pub futility_margin: [[i32; MAX_FUTILITY_DEPTH]; 3],                        /* [phase 0-2][depth 0-4]             */
    pub rfp_margin: [[i32; MAX_RFP_DEPTH]; 2],                                  /* [improving 0-1][depth 0-8]         */
    pub razor_margin: [i32; MAX_RAZOR_DEPTH],                                   /* [depth 0-3]                        */
    pub see_capture_margin: i32,                                                /* per-ply SEE prune threshold        */
    pub quiesce_lmr: Vec<u8>,                                                   /* [depth * MAX_LMR_DEPTH + moves]    */
    pub quiesce_lmr_check: Vec<u8>,                                             /* check-adjusted variant             */
    pub capture_lmr: Vec<u8>,                                                   /* [depth * MAX_LMR_DEPTH + moves]    */
    pub capture_lmr_check: Vec<u8>,                                             /* check-adjusted variant             */
    pub opening_score: u32,                                                     /* opening threshold                  */
    pub endgame_score: u32,                                                     /* endgame threshold                  */
    pub pst_opening: Vec<Vec<i32>>,                                             /* piece index to opening/middlegame  */
    pub pst_endgame: Vec<Vec<i32>>,                                             /* piece index to endgame PST         */
}

/// Main state of the game.
///
/// The special rules field is a bitmask representing enabled special rules.
/// (read configs/example.conf for more information)
///
/// The bits are defined as follows:
/// - bit 0     : castling allowed
/// - bit 1     : en passant allowed
/// - bit 2     : Promotions allowed
/// - bit 3     : Drops allowed
/// - bit 4     : Some pieces have forbidden zones
/// - bit 5     : Can only promote to captured friendly pieces by the enemy
/// - bit 6     : Stalemate is a loss for the stalemated player
/// - bit 7     : Game begins with a setup phase
/// - bit 8     : A player can make a move that creates a stand-off
/// - bit 9     : Halfmove clock draw rule is enabled
/// - bit 10    : There is a limit on the number of repetitions of a position
/// - bit 11-31 : reserved for future use
///
/// Static configuration lives in `static_data: Arc<StaticState>`, shared
/// cheaply across threads. `State::clone()` calls `Arc::clone` for static_data
/// and deep-copies only the 29 dynamic fields.
pub struct State {

    pub statics: Arc<StaticState>,

/*----------------------------------------------------------------------------*\
                                 DYNAMIC FIELDS
\*----------------------------------------------------------------------------*/

    pub game_over: bool,
    pub game_phase: u8,                                                         /* SETUP/OPENING/MIDDLEGAME/ENDGAME   */
    pub phase_score: u32,                                                       /* game phase score for transition    */

    pub playing: u8,
    pub main_board: Vec<u8>,                                                    /* standard mailbox approach          */

    pub pieces_board: [Board; 2],
    pub virgin_board: Board,

    pub castling_state: u8,                                                     /* 4 bits for representing KQkq       */
    pub halfmove_clock: u8,
    pub en_passant_square: EnPassantSquare,

    pub position_hash: u128,
    pub history: Vec<Snapshot>,

    pub search_ply: u32,                                                        /* the number of plies in the search  */
    pub ply_counter: u32,                                                       /* the number of plies in the game    */

    pub opening_material: [u32; 2],                                             /* color to opening material          */
    pub endgame_material: [u32; 2],                                             /* color to endgame material          */
    pub opening_pst_bonus: [i32; 2],                                            /* color to opening pst bonus         */
    pub endgame_pst_bonus: [i32; 2],                                            /* color to endgame pst bonus         */
    pub big_pieces: [u32; 2],
    pub major_pieces: [u32; 2],
    pub minor_pieces: [u32; 2],
    pub royal_pieces: [u32; 2],
    pub royal_list: [Vec<Square>; 2],                                           /* color to royal piece square list   */

    pub piece_count: Vec<u32>,                                                  /* piece index to count               */
    pub piece_list: Vec<HashSet<Square>>,                                       /* piece index to square set          */
    pub piece_in_hand: [Vec<u16>; 2],                                           /* color to pieces in hand list       */

/*----------------------------------------------------------------------------*\
                                 SEARCH FIELDS
\*----------------------------------------------------------------------------*/

    pub position_hash_map: HashMap<PositionHash, u8>,                           /* position hash to repetition count  */
    pub pv_line: [Move; MAX_DEPTH],                                             /* principal variation line for search*/
    pub pv_table: Vec<Move>,                                                    /* flat triangular PV table           */
    pub pv_length: Vec<usize>,                                                  /* PV length per ply                  */

    pub search_hist: Vec<i16>,                                                  /* [piece*B*B + start*B + end]        */
    pub killer_hist: Vec<[Move; 2]>,                                            /* search ply to killer moves         */
    pub static_eval: Vec<i32>,                                                  /* static eval per ply; -INF in check */
}

impl Clone for State {
    fn clone(&self) -> Self {
        State {
            statics: Arc::clone(&self.statics),

            game_over: self.game_over,
            game_phase: self.game_phase,
            phase_score: self.phase_score,

            playing: self.playing,
            main_board: self.main_board.clone(),

            pieces_board: self.pieces_board,
            virgin_board: self.virgin_board,

            castling_state: self.castling_state,
            halfmove_clock: self.halfmove_clock,
            en_passant_square: self.en_passant_square,

            position_hash: self.position_hash,
            history: self.history.clone(),

            search_ply: self.search_ply,
            ply_counter: self.ply_counter,

            opening_material: self.opening_material,
            endgame_material: self.endgame_material,
            opening_pst_bonus: self.opening_pst_bonus,
            endgame_pst_bonus: self.endgame_pst_bonus,
            big_pieces: self.big_pieces,
            major_pieces: self.major_pieces,
            minor_pieces: self.minor_pieces,
            royal_pieces: self.royal_pieces,
            royal_list: self.royal_list.clone(),

            piece_count: self.piece_count.clone(),
            piece_list: self.piece_list.clone(),
            piece_in_hand: self.piece_in_hand.clone(),

            position_hash_map: self.position_hash_map.clone(),
            pv_line: self.pv_line.clone(),
            pv_table: self.pv_table.clone(),
            pv_length: self.pv_length.clone(),

            search_hist: self.search_hist.clone(),
            killer_hist: self.killer_hist.clone(),
            static_eval: self.static_eval.clone(),
        }
    }
}

impl State {
    pub fn new(
        title: String,
        startpos: String,
        files: u8,
        ranks: u8,
        pieces: Vec<Piece>,
        special_rules: u32,
    ) -> Self {

        let piece_count: usize = pieces.len();
        let board_size: usize = (files as usize) * (ranks as usize);

        let statics = Arc::new(StaticState {
            title,
            startpos,
            pieces,
            special_rules,

            initial_setup: vec![board!(files, ranks); piece_count],

            forbidden_zones: vec![board!(files, ranks); piece_count],
            promotion_zones_optional: vec![board!(files, ranks); piece_count],
            promotion_zones_mandatory: vec![board!(files, ranks); piece_count],
            critical_castling: [board!(files, ranks); 4],

            halfmove_limit: u8::MAX,
            repetition_limit: u8::MAX,

            halfmove_pieces: vec![false; piece_count],
            castling_pieces: vec![false; piece_count],

            files,
            ranks,
            board_size,

            relevant_moves: vec![MoveSet::new(); board_size * piece_count],
            relevant_captures: vec![MoveSet::new(); board_size * piece_count],
            relevant_drops: vec![DropSet::new(); board_size * piece_count],
            relevant_setup: vec![DropSet::new(); board_size * piece_count],
            relevant_stand_offs: vec![
                PatternSet::new(); board_size * piece_count
            ],
            relevant_attacks: [
                vec![Vec::new(); board_size],
                vec![Vec::new(); board_size],
            ],
            relevant_castling: array::from_fn(|_| Vec::new()),

            piece_swap_map: vec![NO_PIECE; piece_count],
            piece_demotion_map: vec![NO_PIECE; piece_count],
            piece_char_map: HashMap::new(),

            futility_margin: [[0; MAX_FUTILITY_DEPTH]; 3],
            rfp_margin: [[0; MAX_RFP_DEPTH]; 2],
            razor_margin: [0; MAX_RAZOR_DEPTH],
            see_capture_margin: 0,
            quiesce_lmr: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).ln() * (moves as f64).sqrt();
                (1.35 + base / 2.75)
                    .clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            quiesce_lmr_check: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).ln() * (moves as f64).sqrt();
                (1.35 + base / 2.75 - 1.25)
                    .clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            capture_lmr: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).ln() * (moves as f64).sqrt();
                (0.20 + base / 3.35)
                    .clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            capture_lmr_check: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).ln() * (moves as f64).sqrt();
                (0.20 + base / 3.35 - 1.25)
                    .clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            opening_score: 0,
            endgame_score: 0,
            pst_opening: vec![vec![0; board_size]; piece_count],
            pst_endgame: vec![vec![0; board_size]; piece_count],
        });

        State {
            statics,

            game_over: false,
            game_phase: OPENING,
            phase_score: 0,

            playing: WHITE,
            main_board: vec![NO_PIECE; board_size],

            pieces_board: [board!(files, ranks); 2],
            virgin_board: board!(files, ranks),

            castling_state: 0,
            halfmove_clock: 0,
            en_passant_square: NO_EN_PASSANT,

            position_hash: u128::default(),
            history: Vec::with_capacity(8192),

            search_ply: 0,
            ply_counter: 0,

            opening_material: [0; 2],
            endgame_material: [0; 2],
            opening_pst_bonus: [0; 2],
            endgame_pst_bonus: [0; 2],
            big_pieces: [0; 2],
            major_pieces: [0; 2],
            minor_pieces: [0; 2],
            royal_pieces: [0; 2],
            royal_list: [Vec::new(), Vec::new()],

            piece_count: vec![0u32; piece_count],
            piece_list: vec![HashSet::new(); piece_count],
            piece_in_hand: [vec![0; piece_count], vec![0; piece_count]],

            position_hash_map: HashMap::with_capacity(128),
            pv_line: array::from_fn(|_| null_move()),
            pv_table: vec![null_move(); PV_STRIDE * PV_STRIDE],
            pv_length: vec![0; PV_STRIDE],

            search_hist: vec![0i16; piece_count * board_size * board_size],
            killer_hist: vec![array::from_fn(|_| null_move()); MAX_DEPTH],
            static_eval: vec![i32::MIN; MAX_DEPTH],
        }
    }

    #[inline]
    pub fn static_mut(&mut self) -> &mut StaticState {
        unsafe { Arc::get_mut(&mut self.statics).unwrap_unchecked() }
    }

    pub fn reset(&mut self) {
        let piece_count = self.statics.pieces.len();
        let board_size = self.statics.board_size;

        self.playing = WHITE;
        self.main_board = vec![NO_PIECE; board_size];

        self.pieces_board = [board!(
            self.statics.files, self.statics.ranks
        ); 2];
        self.virgin_board = board!(
            self.statics.files, self.statics.ranks
        );

        self.castling_state = 0;
        self.halfmove_clock = 0;
        self.en_passant_square = NO_EN_PASSANT;

        self.position_hash = u128::default();
        self.history = Vec::with_capacity(8192);

        self.search_ply = 0;
        self.ply_counter = 0;

        self.game_phase = OPENING;
        self.game_over = false;

        self.opening_material = [0; 2];
        self.endgame_material = [0; 2];
        self.opening_pst_bonus = [0; 2];
        self.endgame_pst_bonus = [0; 2];
        self.big_pieces = [0; 2];
        self.major_pieces = [0; 2];
        self.minor_pieces = [0; 2];
        self.royal_pieces = [0; 2];
        self.royal_list = [Vec::new(), Vec::new()];

        self.piece_count = vec![0u32; piece_count];
        self.piece_list = vec![HashSet::new(); piece_count];
        self.piece_in_hand = [vec![0; piece_count], vec![0; piece_count]];

        self.position_hash_map.clear();
        self.pv_line = array::from_fn(|_| null_move());
        self.pv_table = vec![null_move(); PV_STRIDE * PV_STRIDE];
        self.pv_length = vec![0; PV_STRIDE];

        self.search_hist =
            vec![0i16; piece_count * board_size * board_size];
        self.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];
        self.static_eval = vec![-INF; MAX_DEPTH];
    }

    pub fn load_fen(&mut self, fen: &str, dict: Option<&Translator>) {
        self.reset();
        parse_fen(self, fen, dict);
    }

    fn generate_piece_moves(&self, expr_set: &Vec<String>) -> Vec<MoveSet> {
        let mut piece_moves = Vec::with_capacity(self.statics.pieces.len());
        for expr in expr_set {
            let move_vectors = generate_move_vectors(expr, self);
            let moves_for_piece = move_vectors
                .iter()
                .map(|multi_leg_vector: &Vec<LegVector>| {
                    multi_leg_vector
                        .iter()
                        .map(|leg_vector| leg!(leg_vector))
                        .collect::<Vec<u32>>()
                })
                .collect::<Vec<Vec<u32>>>();
            piece_moves.push(moves_for_piece);
        }
        piece_moves
    }

    fn generate_piece_drops(&self, expr_set: &[String]) -> Vec<DropSet> {
        self.statics.pieces.iter().map(
            |piece| generate_drop_vectors(piece, self, expr_set)
        ).collect::<Vec<DropSet>>()
    }

    fn generate_piece_stand_off(
        &self, expr_set: Vec<String>
    ) -> Vec<PatternSet> {
        expr_set.iter().map(
            |expr| generate_stand_off_patterns(expr, self)
        ).collect::<Vec<PatternSet>>()
    }

    fn populate_relevant_moves(&mut self, piece_moves: &[MoveSet]) {
        let board_size = self.statics.board_size;
        let piece_count = self.statics.pieces.len();

        let mut results = vec![MoveSet::new(); piece_count * board_size];
        for (index, piece) in self.statics.pieces.iter().enumerate() {
            for square in 0..board_size {
                results[index * board_size + square] =
                    generate_relevant_moves(
                        piece, square as u32, self, piece_moves
                    );
            }
        }
        self.static_mut().relevant_moves = results;
    }

    fn populate_relevant_captures(&mut self, piece_moves: &[MoveSet]) {
        let board_size = self.statics.board_size;
        let piece_count = self.statics.pieces.len();

        let mut results = vec![MoveSet::new(); piece_count * board_size];
        for (index, piece) in self.statics.pieces.iter().enumerate() {
            for square in 0..board_size {
                results[index * board_size + square] =
                    generate_relevant_captures(
                        piece, square as u32, self, piece_moves
                    );
            }
        }
        self.static_mut().relevant_captures = results;
    }

    fn populate_relevant_drops(&mut self, piece_setup_drops: &[DropSet]) {
        let board_size = self.statics.board_size;
        let piece_count = self.statics.pieces.len();

        let mut results = vec![DropSet::new(); piece_count * board_size];
        for (index, piece) in self.statics.pieces.iter().enumerate() {
            for square in 0..board_size {
                results[index * board_size + square] =
                    generate_relevant_drops(
                        piece, square as u32, self, piece_setup_drops
                    );
            }
        }
        self.static_mut().relevant_drops = results;
    }

    fn populate_relevant_setup(&mut self, piece_setup_drops: &[DropSet]) {
        let board_size = self.statics.board_size;
        let piece_count = self.statics.pieces.len();

        let mut results = vec![DropSet::new(); piece_count * board_size];
        for (index, piece) in self.statics.pieces.iter().enumerate() {
            for square in 0..board_size {
                results[index * board_size + square] =
                    generate_relevant_drops(
                        piece, square as u32, self, piece_setup_drops
                    );
            }
        }
        self.static_mut().relevant_setup = results;
    }

    fn populate_relevant_stand_offs(
        &mut self, piece_stand_off: &[PatternSet]
    ) {
        let board_size = self.statics.board_size;
        let piece_count = self.statics.pieces.len();

        let mut results = vec![PatternSet::new(); piece_count * board_size];
        for (index, piece) in self.statics.pieces.iter().enumerate() {
            for square in 0..board_size {
                results[index * board_size + square] =
                    generate_relevant_stand_offs(
                        piece, square as u32, self, piece_stand_off
                    );
            }
        }
        self.static_mut().relevant_stand_offs = results;
    }

    fn populate_relevant_attacks(&mut self) {
        for square in 0..self.statics.board_size {
            generate_attack_masks(square as Square, self);
        }
    }

    pub fn precompute(
        &mut self,
        moves_expr_set: Vec<String>,
        drops_expr_set: Vec<String>,
        setup_expr_set: Vec<String>,
        stand_off_expr_set: Vec<String>
    ) {
        let piece_count = self.statics.pieces.len();
        let piece_moves = self.generate_piece_moves(&moves_expr_set);

        let mut piece_drops = vec![Vec::new(); piece_count];
        if drops!(self) {
            piece_drops = self.generate_piece_drops(&drops_expr_set);
        }

        let mut piece_setup = vec![Vec::new(); piece_count];
        if setup_phase!(self) {
            piece_setup = self.generate_piece_drops(&setup_expr_set);
        }

        let mut piece_stand_off = vec![Vec::new(); piece_count];
        if stand_offs!(self) {
            piece_stand_off = self.generate_piece_stand_off(stand_off_expr_set);
        }

        self.populate_relevant_moves(&piece_moves);
        self.populate_relevant_captures(&piece_moves);

        if drops!(self) {
            self.populate_relevant_drops(&piece_drops);
        }

        if setup_phase!(self) {
            self.populate_relevant_setup(&piece_setup);
        }

        if stand_offs!(self) {
            self.populate_relevant_stand_offs(&piece_stand_off);
        }

        self.populate_relevant_attacks();
    }
}
