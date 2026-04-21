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
        ($state.special_rules & 1) == 1
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
        ($state.special_rules >> 1 & 1) == 1
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
        ($state.special_rules >> 2 & 1) == 1
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
        ($state.special_rules >> 3 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_drops {
    ($rules:expr) => {
        $rules |= 1 << 3;
    };
}

#[macro_export]
macro_rules! count_limits {
    ($state:expr) => {
        ($state.special_rules >> 4 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_count_limits {
    ($rules:expr) => {
        $rules |= 1 << 4;
    };
}

#[macro_export]
macro_rules! forbidden_zones {
    ($state:expr) => {
        ($state.special_rules >> 5 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_forbidden_zones {
    ($rules:expr) => {
        $rules |= 1 << 5;
    };
}

#[macro_export]
macro_rules! promote_to_captured {
    ($state:expr) => {
        ($state.special_rules >> 6 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_promote_to_captured {
    ($rules:expr) => {
        $rules |= 1 << 6;
    };
}

#[macro_export]
macro_rules! demote_upon_capture {
    ($state:expr) => {
        ($state.special_rules >> 7 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_demote_upon_capture {
    ($rules:expr) => {
        $rules |= 1 << 7;
    };
}

#[macro_export]
macro_rules! stalemate_loss {
    ($state:expr) => {
        ($state.special_rules >> 8 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_stalemate_loss {
    ($rules:expr) => {
        $rules |= 1 << 8;
    };
}

#[macro_export]
macro_rules! setup_phase {
    ($state:expr) => {
        ($state.special_rules >> 9 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_setup_phase {
    ($rules:expr) => {
        $rules |= 1 << 9;
    };
}

#[macro_export]
macro_rules! stand_offs {
    ($state:expr) => {
        ($state.special_rules >> 10 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_stand_offs {
    ($rules:expr) => {
        $rules |= 1 << 10;
    };
}

#[macro_export]
macro_rules! halfmove_clock {
    ($state:expr) => {
        ($state.special_rules >> 11 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_halfmove_clock {
    ($rules:expr) => {
        $rules |= 1 << 11;
    };
}

#[macro_export]
macro_rules! repetition_limit {
    ($state:expr) => {
        ($state.special_rules >> 12 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_repetition_limit {
    ($rules:expr) => {
        $rules |= 1 << 12;
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
pub struct Snapshot {
    pub move_ply: Move,

    pub castling_state: u8,
    pub halfmove_clock: u8,
    pub en_passant_square: EnPassantSquare,
    pub setup_phase: bool,
    pub game_over: bool,
    pub game_phase: u8,                                                         /* OPENING/MIDDLEGAME/ENDGAME         */

    pub position_hash: u128
}

impl Default for Snapshot {
    fn default() -> Self {
        Snapshot {
            move_ply: null_move(),
            castling_state: 0,
            halfmove_clock: 0,
            en_passant_square: EnPassantSquare::MAX,
            setup_phase: false,
            game_over: false,
            game_phase: OPENING,
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
        let mut game_phase_score = 0;

        for (piece_idx, piece) in $state.pieces.iter().enumerate() {
            if p_is_big!(piece) && !p_is_royal!(piece) {
                game_phase_score += (p_ovalue!(piece) as u32)
                    * ($state.piece_list[piece_idx].len() as u32);
            }
        }

        game_phase_score
    }};
}

/*----------------------------------------------------------------------------*\
                            GAME STATE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Main state of the game.
///
/// The special rules field is a bitmask representing enabled special rules.
///
/// The bits are defined as follows:
/// - bit 0     : castling allowed
/// - bit 1     : en passant allowed
/// - bit 2     : Promotions allowed
/// - bit 3     : Drops allowed
/// - bit 4     : Some pieces have a count limit
/// - bit 5     : Some pieces have forbidden zones
/// - bit 6     : Can only promote to captured friendly pieces by the enemy
/// - bit 7     : Demote piece in hand upon capture
/// - bit 8     : Stalemate is a loss for the stalemated player
/// - bit 9     : Game begins with a setup phase
/// - bit 10    : A player can make a move that creates a stand-off that the
///   opponent must break on their next turn
/// - bit 11    : Halfmove clock draw rule is enabled
/// - bit 12    : There is a limit on the number of repetitions of a position
/// - bit 13-31 : reserved for future use
///
pub struct State {

/*----------------------------------------------------------------------------*\
                                 STATIC FIELDS
\*----------------------------------------------------------------------------*/

    pub title: String,
    pub pieces: Vec<Piece>,
    pub special_rules: u32,

    pub initial_setup: Vec<Board>,                                              /* piece index to board               */

    pub forbidden_zones: Vec<Board>,                                            /* piece to forbidden zone bitboard   */
    pub promotion_zones_optional: Vec<Board>,                                   /* piece to promotion zone bitboard   */
    pub promotion_zones_mandatory: Vec<Board>,                                  /* piece to promotion zone bitboard   */

    pub piece_limit: Vec<u32>,                                                  /* piece index to count limit         */
    pub halfmove_limit: u8,                                                     /* halfmoves before draw              */
    pub halfmove_pieces: Vec<bool>,                                             /* moving these pieces resets clock   */
    pub repetition_limit: u8,                                                   /* number of repetitions for draw     */
    pub opening_score: u32,                                                     /* opening threshold                  */
    pub endgame_score: u32,                                                     /* endgame threshold                  */
    pub pst_opening: Vec<Vec<i32>>,                                             /* piece index to opening/middlegame  */
    pub pst_endgame: Vec<Vec<i32>>,                                             /* piece index to endgame PST         */

    pub files: u8,
    pub ranks: u8,

    pub relevant_moves: Vec<Vec<MoveSet>>,
    pub relevant_captures: Vec<Vec<MoveSet>>,
    pub relevant_drops: Vec<Vec<DropSet>>,
    pub relevant_setup: Vec<Vec<DropSet>>,
    pub relevant_stand_offs: Vec<Vec<PatternSet>>,
    pub relevant_attacks: [Vec<Vec<AttackMask>>; 2],

    pub piece_swap_map: HashMap<u8, u8>,                                        /* piece index to swap color (if any) */
    pub piece_demotion_map: HashMap<u8, Vec<u8>>,                               /* piece index to demotion piece idx  */
    pub piece_char_map: HashMap<char, u8>,                                      /* char to piece index map            */

    pub most_valuable: u16,                                                     /* value of the most valuable piece   */

/*----------------------------------------------------------------------------*\
                                 DYNAMIC FIELDS
\*----------------------------------------------------------------------------*/

    pub setup_phase: bool,
    pub game_over: bool,
    pub game_phase: u8,                                                         /* OPENING/MIDDLEGAME/ENDGAME         */

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

    pub position_hash_map: HashMap<PositionHash, u8>,                           /* position hash to repetition count  */
    pub transposition_table: TTTable,                                           /* transposition table for search     */
    pub pv_line: [Move; MAX_DEPTH],                                             /* principal variation line for search*/


    pub search_hist: Vec<Vec<u16>>,                                             /* piece index to square to score      */
    pub killer_hist: Vec<[Move; 2]>                                             /* search ply to killer moves          */
}

impl State {
    pub fn new(
        title: String,
        files: u8,
        ranks: u8,
        pieces: Vec<Piece>,
        special_rules: u32,
    ) -> Self {

        let piece_count: usize = pieces.len();
        let most_valuable: u16 =
            pieces.iter().map(|p| p_ovalue!(p)).max().unwrap();
        let board_size: usize = (files as usize) * (ranks as usize);

        State {
            title,
            pieces,
            special_rules,

            initial_setup: vec![board!(files, ranks); piece_count],

            forbidden_zones: vec![board!(files, ranks); piece_count],
            promotion_zones_optional: vec![board!(files, ranks); piece_count],
            promotion_zones_mandatory: vec![board!(files, ranks); piece_count],

            piece_limit: vec![u32::MAX; piece_count],
            halfmove_limit: u8::MAX,
            halfmove_pieces: vec![false; piece_count],
            repetition_limit: u8::MAX,
            opening_score: 0,
            endgame_score: 0,
            pst_opening: vec![vec![0; board_size]; piece_count],
            pst_endgame: vec![vec![0; board_size]; piece_count],

            files,
            ranks,

            relevant_moves:
                vec![vec![MoveSet::new(); board_size]; piece_count],
            relevant_captures:
                vec![vec![MoveSet::new(); board_size]; piece_count],
            relevant_drops:
                vec![vec![DropSet::new(); board_size]; piece_count],
            relevant_setup:
                vec![vec![DropSet::new(); board_size]; piece_count],
            relevant_stand_offs:
                vec![vec![PatternSet::new(); board_size]; piece_count],
            relevant_attacks:
                [
                    vec![Vec::new(); board_size],
                    vec![Vec::new(); board_size]
                ],

            piece_swap_map: HashMap::new(),
            piece_demotion_map: HashMap::new(),
            piece_char_map: HashMap::new(),

            most_valuable,

            setup_phase: false,
            game_over: false,
            game_phase: OPENING,

            playing: WHITE,
            main_board: vec![NO_PIECE; (files as usize) * (ranks as usize)],

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

            transposition_table: TTTable::new(),
            pv_line: array::from_fn(|_| null_move()),

            search_hist: vec![vec![0u16; board_size]; piece_count],
            killer_hist: vec![array::from_fn(|_| null_move()); MAX_DEPTH],
        }
    }

    /// Resets all dynamic position/search fields while keeping static config.
    ///
    /// This clears board occupancy, counters, caches, histories, and search
    /// bookkeeping while preserving static variant definitions.
    /// It prepares the state for loading or initializing a fresh position.
    pub fn reset(&mut self) {
        let piece_count: usize = self.pieces.len();
        let board_size: usize = (self.files as usize) * (self.ranks as usize);

        self.playing = WHITE;
        self.main_board = vec![NO_PIECE; board_size];

        self.pieces_board = [board!(self.files, self.ranks); 2];
        self.virgin_board = board!(self.files, self.ranks);

        self.castling_state = 0;
        self.halfmove_clock = 0;
        self.en_passant_square = NO_EN_PASSANT;

        self.position_hash = u128::default();
        self.history = Vec::with_capacity(8192);

        self.ply_counter = 0;

        self.game_phase = OPENING;

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

        self.search_ply = 0;
        self.position_hash_map.clear();

        self.transposition_table = TTTable::new();
        self.pv_line = array::from_fn(|_| null_move());

        self.search_hist = vec![vec![0u16; board_size]; piece_count];
        self.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];
    }

    /// Resets the state and loads a new position from a FEN-like string.
    ///
    /// This is a convenience wrapper that performs `reset()` first, then parses
    /// and applies the supplied position text.
    /// It guarantees stale runtime state is not carried into the new position.
    pub fn load_fen(&mut self, fen: &str) {
        self.reset();
        parse_fen(self, fen);
    }

    fn populate_char_map(&mut self) {
        for (index, piece) in self.pieces.iter().enumerate() {
            self.piece_char_map.insert(piece.char, index as u8);
        }
    }

    fn generate_piece_moves(&mut self, expr_set: &Vec<String>) -> Vec<MoveSet> {
        let mut piece_moves = Vec::with_capacity(self.pieces.len());
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

    fn generate_piece_drops(&mut self, expr_set: &[String]) -> Vec<DropSet> {
        self.pieces.iter().map(
            |piece| generate_drop_vectors(piece, self, expr_set)
        ).collect::<Vec<DropSet>>()
    }

    fn generate_piece_stand_off(
        &mut self, expr_set: Vec<String>
    ) -> Vec<PatternSet> {
        expr_set.iter().map(
            |expr| generate_stand_off_patterns(expr, self)
        ).collect::<Vec<PatternSet>>()
    }

    fn populate_relevant_moves(&mut self, piece_moves: &[MoveSet]) {
        for (index, piece) in self.pieces.iter().enumerate() {
            for square in 0..(self.files as u32 * self.ranks as u32) {
                self.relevant_moves[index][square as usize]
                    = generate_relevant_moves(
                        piece,
                        square,
                        self,
                        piece_moves
                    );
            }
        }
    }

    fn populate_relevant_captures(&mut self, piece_moves: &[MoveSet]) {
        for (index, piece) in self.pieces.iter().enumerate() {
            for square in 0..(self.files as u32 * self.ranks as u32) {
                self.relevant_captures[index][square as usize]
                    = generate_relevant_captures(
                        piece,
                        square,
                        self,
                        piece_moves
                    );
            }
        }
    }

    fn populate_relevant_drops(&mut self, piece_setup_drops: &[DropSet]) {
        for (index, piece) in self.pieces.iter().enumerate() {
            for square in 0..(self.files as u32 * self.ranks as u32) {
                self.relevant_drops[index][square as usize]
                    = generate_relevant_drops(
                        piece,
                        square,
                        self,
                        piece_setup_drops
                    );
            }
        }
    }

    fn populate_relevant_setup(&mut self, piece_setup_drops: &[DropSet]) {
        for (index, piece) in self.pieces.iter().enumerate() {
            for square in 0..(self.files as u32 * self.ranks as u32) {
                self.relevant_setup[index][square as usize]
                    = generate_relevant_drops(
                        piece,
                        square,
                        self,
                        piece_setup_drops
                    );
            }
        }
    }

    fn populate_relevant_stand_offs(
        &mut self, piece_stand_off: &[PatternSet]
    ) {
        for (index, piece) in self.pieces.iter().enumerate() {
            for square in 0..(self.files as u32 * self.ranks as u32) {
                self.relevant_stand_offs[index][square as usize]
                    = generate_relevant_stand_offs(
                        piece,
                        square,
                        self,
                        piece_stand_off
                    );
            }
        }
    }

    fn populate_relevant_attacks(&mut self) {
        for square in 0..(self.files as u32 * self.ranks as u32) {
            generate_attack_masks(square as Square, self);
        }
    }

    /// Builds cached move/drop/pattern/attack tables from parsed expressions.
    ///
    /// The enabled special-rule flags determine which optional caches are
    /// populated.
    pub fn precompute(
        &mut self,
        moves_expr_set: Vec<String>,
        drops_expr_set: Vec<String>,
        setup_expr_set: Vec<String>,
        stand_off_expr_set: Vec<String>
    ) {
        let piece_count = self.pieces.len();
        self.populate_char_map();

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
