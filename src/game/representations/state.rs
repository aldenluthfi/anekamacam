//! # state.rs
//!
//! Defines game state representation and management.
//!
//! Everything the engine does reads or mutates one position, so the shape of
//! that value decides how cheap search, evaluation, and make/undo can be.
//! This file defines that centre of gravity: the immutable per-variant
//! configuration shared across threads, and the mutable per-position state
//! that search clones, advances a ply, and rolls back.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use crate::*;

/// Square
///
/// A single board square addressed by its flat index, computed as
/// `rank * files + file`. Sixteen bits cover every supported board size,
/// up to the 4096 squares a `U4096` bitboard can address.
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

/// EnPassantSquare
///
/// Packed en passant descriptor holding the capture target square, the
/// square of the capturable piece, and that piece's index. The sentinel
/// `NO_EN_PASSANT` marks the absence of an en passant opportunity.
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

/// Snapshot
///
/// Captures reversible state needed to undo a move.
/// Each snapshot stores move payload and dynamic counters/flags so `undo_move!`
/// can restore the exact pre-move position, including hash-dependent state.
/// It is appended to `State::history` during move execution.
#[derive(Clone)]
pub struct Snapshot {
    pub move_ply: Move,                                                         /* the move that was played           */

    pub castling_state: u8,                                                     /* castling rights before move        */
    pub halfmove_clock: u8,                                                     /* halfmove clock before move         */
    pub en_passant_square: EnPassantSquare,                                     /* en passant sq before move          */
    pub game_over: bool,                                                        /* game-over flag before move         */
    pub game_phase: u8,                                                         /* game phase before move             */
    pub phase_score: u32,                                                       /* phase score before move            */

    pub position_hash: u128,                                                    /* Zobrist hash before move           */
    pub pawn_hash: u128,                                                        /* pawn-only Zobrist key before move  */
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
            pawn_hash: u128::default(),
        }
    }
}

/// Flat piece-list accessors.
///
/// `piece_list` is one flat `Vec<Square>` holding `board_size` slots per
/// piece index; each row keeps its occupied squares packed at the front,
/// `piece_count` gives the occupied length, and the tail stays filled
/// with `NO_SQUARE`. The push and remove macros own the `piece_count`
/// update, so call sites never touch the count themselves.
///
/// - `piece_squares!` -> iterator over a piece's occupied squares
/// - `piece_list_push!` -> appends after the last occupied slot
/// - `piece_list_remove!` -> swap-removes a square within its row; a
///   missing square is ignored, matching set semantics
///
/// Params:
/// - state      : &State / &mut State -> position whose piece list is used
/// - piece_index: usize               -> piece whose row is accessed
/// - square     : Square              -> square to insert or remove
///
#[macro_export]
macro_rules! piece_squares {
    ($state:expr, $piece_index:expr) => {{
        let row_start = $piece_index * $state.statics.board_size;
        let count = $state.piece_count[$piece_index] as usize;

        $state.piece_list[row_start..row_start + count].iter()
    }};
}

#[macro_export]
macro_rules! piece_list_push {
    ($state:expr, $piece_index:expr, $square:expr) => {
        let pushed_square = $square;
        let pushed_piece = $piece_index;
        let count = $state.piece_count[pushed_piece] as usize;

        $state.piece_list[
            pushed_piece * $state.statics.board_size + count
        ] = pushed_square;
        $state.piece_count[pushed_piece] += 1;
    };
}

#[macro_export]
macro_rules! piece_list_remove {
    ($state:expr, $piece_index:expr, $square:expr) => {
        let removed_square = $square;
        let removed_piece = $piece_index;
        let row_start = removed_piece * $state.statics.board_size;
        let count = $state.piece_count[removed_piece] as usize;
        let row = &mut $state.piece_list[row_start..row_start + count];

        if let Some(found) =
            row.iter().position(|&square| square == removed_square)
        {
            row[found] = row[count - 1];
            row[count - 1] = NO_SQUARE;
            $state.piece_count[removed_piece] -= 1;
        }
    };
}

/// pass_snapshot!
///
/// Returns whether a snapshot corresponds to a pass move.
/// This is used in repetition / stand-off flow where pass detection is needed
/// while reading from undo history rather than the active move stream.
///
/// Params:
/// - snapshot: Snapshot -> the history entry whose move is inspected
///
/// Return:
/// bool                 -> true if the snapshotted move is a pass
///
#[macro_export]
macro_rules! pass_snapshot {
    ($snapshot:expr) => {
        is_pass!($snapshot.move_ply)
    };
}

/// game_phase_score!
///
/// Recomputes the phase score of a position from scratch by summing the
/// opening values of every non-royal "big" piece still on the board. The
/// result is compared against the variant's opening/endgame thresholds to
/// decide which game phase the position belongs to.
///
/// Params:
/// - state: State -> position whose remaining material is tallied
///
/// Return:
/// u32            -> summed opening value of all non-royal big pieces
///
#[macro_export]
macro_rules! game_phase_score {
    ($state:expr) => {{
        let mut phase_score = 0;

        for (piece_idx, piece) in $state.statics.pieces.iter().enumerate() {
            if p_is_big!(piece) && !p_is_royal!(piece) {
                phase_score +=
                    p_ovalue!(piece) as u32 * $state.piece_count[piece_idx];
            }
        }

        phase_score
    }};
}

/*----------------------------------------------------------------------------*\
                            GAME STATE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// StaticState
///
/// Immutable variant configuration, shared across threads via Arc.
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
    pub adjacency_mask: Vec<Board>,                                             /* square to adjacent-square bitboard */
    pub royal_shield_mask: Vec<Board>,                                          /* color * board size + royal square  */
    pub royal_front_mask: Vec<Board>,                                           /* color * board size + royal square  */
    pub zone_attack: Vec<u8>,                                                   /* (royal * P + piece) * B + from     */

    pub piece_swap_map: Vec<PieceIndex>,                                        /* piece index to swap color (if any) */
    pub piece_demotion_map: Vec<PieceIndex>,                                    /* piece index to demotion piece idx  */
    pub piece_char_map: HashMap<char, PieceIndex>,                              /* char to piece index map            */

/*----------------------------------------------------------------------------*\
                                 SEARCH FIELDS
\*----------------------------------------------------------------------------*/

    pub futility_margin: [[i32; MAX_FUTILITY_DEPTH]; 3],                        /* [phase 0-2][depth 0-4]             */
    pub rfp_margin: [[i32; MAX_RFP_DEPTH]; 2],                                  /* [improving 0-1][depth 0-8]         */
    pub see_margin: Vec<i32>,                                                   /* [depth 0-7] SEE prune threshold    */
    pub delta_margin: i32,                                                      /* qsearch delta pruning safety margin*/
    pub aspiration_delta: i32,                                                  /* initial aspiration half-window     */
    pub razor_margin: [i32; MAX_RZR_DEPTH],                                     /* [depth 0-3]                        */
    pub quiesce_lmr: Vec<u8>,                                                   /* [depth * MAX_LMR_DEPTH + moves]    */
    pub quiesce_lmr_check: Vec<u8>,                                             /* check-adjusted variant             */
    pub capture_lmr: Vec<u8>,                                                   /* [depth * MAX_LMR_DEPTH + moves]    */
    pub capture_lmr_check: Vec<u8>,                                             /* check-adjusted variant             */
    pub opening_score: u32,                                                     /* opening threshold                  */
    pub endgame_score: u32,                                                     /* endgame threshold                  */
    pub pst_opening: Vec<Vec<i32>>,                                             /* piece index to opening/middlegame  */
    pub pst_endgame: Vec<Vec<i32>>,                                             /* piece index to endgame PST         */
    pub nmp_min_material: u32,                                                  /* NMP zugzwang guard                 */
    pub nmp_eval_div: i32,                                                      /* NMP eval-surplus reduction divisor */
    pub capt_hist_div: i32,                                                     /* capture-history victim bucket div  */
    pub singular_margin: i32,                                                   /* singular beta margin per depth     */
    pub tempo_bonus: i32,                                                       /* tempo advantage bonus              */
    pub draw_bias: i32,                                                         /* draw contempt clamp                */
    pub pawn_shield_bonus: i32,                                                 /* per-shield-pawn royal cover bonus  */
    pub king_shelter_bonus: i32,                                                /* per-adjacent-piece shelter bonus   */
    pub castled_bonus: i32,                                                     /* bonus once a side has castled      */
    pub castling_rights_bonus: i32,                                             /* bonus for still holding rights     */
    pub king_danger_scale: i32,                                                 /* quadratic zone-attack danger scale */
    pub open_shield_penalty: i32,                                               /* no-pawn-ahead-of-royal penalty     */
    pub imbalance_major: i32,                                                   /* major piece imbalance weight       */
    pub imbalance_minor: i32,                                                   /* minor piece imbalance weight       */
    pub pair_bonus: Vec<i32>,                                                   /* pair bonus per piece index         */

    pub pawn_path_mask: Vec<Board>,                                             /* idx = piece * board size + square  */
    pub pawn_interference_mask: Vec<Board>,                                     /* enemy squares that stop a passer   */
    pub pawn_support_mask: Vec<Board>,                                          /* friendly squares that defend it    */
    pub pawn_advancement: Vec<i32>,                                             /* fixed-point advancement^2 * 256    */
    pub pawn_passed_opening: Vec<i32>,                                          /* passed bonus, opening, per square  */
    pub pawn_passed_endgame: Vec<i32>,                                          /* passed bonus, endgame, per square  */
    pub pawn_connected_opening: i32,                                            /* phalanx/defended bonus, opening    */
    pub pawn_connected_endgame: i32,                                            /* phalanx/defended bonus, endgame    */
    pub pawn_doubled_penalty: i32,                                              /* doubled pawn penalty, both phases  */
    pub pawn_isolated_penalty: i32,                                             /* isolated pawn penalty, both phases */
    pub pawn_backward_penalty: i32,                                             /* backward pawn penalty, both phases */
    pub pawn_backward_mask: Vec<Board>,                                         /* enemy squares contesting the stop  */
    pub pawn_support_offsets: Vec<Vec<i32>>,                                    /* friendly support file offsets      */
    pub pawn_passed_support_opening: Vec<i32>,                                  /* passer support bonus, opening      */
    pub pawn_passed_support_endgame: Vec<i32>,                                  /* passer support bonus, endgame      */
}

/// State
///
/// Main state of the game.
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
/// and deep-copies only the 30 dynamic fields.
pub struct State {

    pub statics: Arc<StaticState>,

/*----------------------------------------------------------------------------*\
                                 DYNAMIC FIELDS
\*----------------------------------------------------------------------------*/

    pub game_over: bool,                                                        /* true once the game has ended       */
    pub game_phase: u8,                                                         /* SETUP/OPENING/MIDDLEGAME/ENDGAME   */
    pub phase_score: u32,                                                       /* game phase score for transition    */

    pub playing: u8,                                                            /* side to move (WHITE / BLACK)       */
    pub main_board: Vec<u8>,                                                    /* standard mailbox approach          */

    pub pieces_board: [Board; 2],                                               /* per-color occupancy bitboards      */
    pub pawn_board: [Board; 2],                                                 /* per-color pawn-only bitboards      */
    pub virgin_board: Board,                                                    /* squares whose piece is unmoved     */

    pub castling_state: u8,                                                     /* 4 bits for representing KQkq       */
    pub has_castled: [bool; 2],                                                 /* per-color castled-this-game flag   */
    pub halfmove_clock: u8,                                                     /* plies since pawn move/capture      */
    pub en_passant_square: EnPassantSquare,                                     /* active en passant square           */

    pub position_hash: u128,                                                    /* incremental Zobrist key            */
    pub pawn_hash: u128,                                                        /* incremental pawn-only Zobrist key  */
    pub history: Vec<Snapshot>,                                                 /* undo stack of snapshots            */

    pub search_ply: u32,                                                        /* the number of plies in the search  */
    pub ply_counter: u32,                                                       /* the number of plies in the game    */

    pub opening_material: [u32; 2],                                             /* color to opening material          */
    pub endgame_material: [u32; 2],                                             /* color to endgame material          */
    pub opening_pst_bonus: [i32; 2],                                            /* color to opening pst bonus         */
    pub endgame_pst_bonus: [i32; 2],                                            /* color to endgame pst bonus         */
    pub big_pieces: [u32; 2],                                                   /* per-color big-piece counts         */
    pub major_pieces: [u32; 2],                                                 /* per-color major-piece counts       */
    pub minor_pieces: [u32; 2],                                                 /* per-color minor-piece counts       */
    pub royal_pieces: [u32; 2],                                                 /* per-color royal-piece counts       */
    pub royal_list: [Vec<Square>; 2],                                           /* color to royal piece square list   */

    pub piece_count: Vec<u32>,                                                  /* piece index to count               */
    pub piece_list: Vec<Square>,                                                /* board_size slots per piece, packed */
    pub piece_in_hand: [Vec<u16>; 2],                                           /* color to pieces in hand list       */

/*----------------------------------------------------------------------------*\
                                 SEARCH FIELDS
\*----------------------------------------------------------------------------*/

    pub position_hash_map: HashMap<PositionHash, u8>,                           /* position hash to repetition count  */
    pub pv_line: [Move; MAX_DEPTH],                                             /* principal variation line for search*/
    pub pv_table: Vec<Move>,                                                    /* flat triangular PV table           */
    pub pv_length: Vec<usize>,                                                  /* PV length per ply                  */

    pub cont_hist: Vec<i16>,                                                    /* [1-ply | 2-ply] (piece*B+end)^2    */
    pub capt_hist: Vec<i16>,                                                    /* [piece*B*8 + end*8 + victim_bkt]   */
    pub corr_hist: Vec<i16>,                                                    /* per-side pawn-hash eval correction */

    pub search_hist: Vec<i16>,                                                  /* [piece*B*B + start*B + end]        */
    pub killer_hist: Vec<[Move; 2]>,                                            /* search ply to killer moves         */
    pub static_eval: Vec<i32>,                                                  /* static eval per ply; -INF in check */

    pub excluded: Vec<PseudoMove>,                                              /* singular-search exclusion per ply  */
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
            pawn_board: self.pawn_board,
            virgin_board: self.virgin_board,

            castling_state: self.castling_state,
            has_castled: self.has_castled,
            halfmove_clock: self.halfmove_clock,
            en_passant_square: self.en_passant_square,

            position_hash: self.position_hash,
            pawn_hash: self.pawn_hash,
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

            cont_hist: self.cont_hist.clone(),
            capt_hist: self.capt_hist.clone(),
            corr_hist: self.corr_hist.clone(),

            search_hist: self.search_hist.clone(),
            killer_hist: self.killer_hist.clone(),
            static_eval: self.static_eval.clone(),

            excluded: self.excluded.clone(),
        }
    }
}

impl State {
    /// State::new
    ///
    /// Builds a blank engine state for a variant: every static table is
    /// allocated at its final size but zeroed, and all dynamic fields are
    /// set to their empty-board defaults. The result is unusable for play
    /// until the config loader fills the static tables and `precompute`
    /// derives the relevant-move caches.
    ///
    /// Params:
    /// - title        : String     -> display name of the variant
    /// - startpos     : String     -> FEN of the variant's starting position
    /// - files        : u8         -> number of board files
    /// - ranks        : u8         -> number of board ranks
    /// - pieces       : Vec<Piece> -> piece definitions, indexed by PieceIndex
    /// - special_rules: u32        -> special-rules bitmask (see [`State`])
    ///
    /// Return:
    /// Self                        -> a fresh state with empty boards and
    ///                                zeroed search tables
    ///
    /// Notes:
    /// The LMR reduction tables are the only values computed here, since
    /// they depend on nothing but depth and move-count indices.
    ///
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
            adjacency_mask: vec![board!(files, ranks); board_size],
            royal_shield_mask: vec![board!(files, ranks); 2 * board_size],
            royal_front_mask: vec![board!(files, ranks); 2 * board_size],
            zone_attack: Vec::new(),

            piece_swap_map: vec![NO_PIECE; piece_count],
            piece_demotion_map: vec![NO_PIECE; piece_count],
            piece_char_map: HashMap::new(),

            futility_margin: [[0; MAX_FUTILITY_DEPTH]; 3],
            rfp_margin: [[0; MAX_RFP_DEPTH]; 2],
            razor_margin: [0; MAX_RZR_DEPTH],
            see_margin: vec![0; MAX_SEE_DEPTH],
            delta_margin: 0,
            aspiration_delta: 50,
            quiesce_lmr: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).sqrt() * (moves as f64).sqrt();

                (1.5 + base / 3.5).clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            quiesce_lmr_check: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).sqrt() * (moves as f64).ln();

                (1.0 + base / 4.0).clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            capture_lmr: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).ln() * (moves as f64).sqrt();

                (1.0 + base / 4.0).clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            capture_lmr_check: (0..MAX_DEPTH * MAX_LMR_DEPTH).map(|i| {
                let depth = i / MAX_LMR_DEPTH + 1;
                let moves = i % MAX_LMR_DEPTH;
                let base = (depth as f64).ln() * (moves as f64).ln();

                (base / 4.5).clamp(0.0, depth as f64 - 1.0) as u8
            }).collect(),
            opening_score: 0,
            endgame_score: 0,
            pst_opening: vec![vec![0; board_size]; piece_count],
            pst_endgame: vec![vec![0; board_size]; piece_count],
            nmp_min_material: 1,
            nmp_eval_div: 1,
            capt_hist_div: 1,
            singular_margin: 1,
            tempo_bonus: 0,
            draw_bias: 0,
            pawn_shield_bonus: 0,
            king_shelter_bonus: 0,
            castled_bonus: 0,
            castling_rights_bonus: 0,
            king_danger_scale: 0,
            open_shield_penalty: 0,
            imbalance_major: 0,
            imbalance_minor: 0,
            pair_bonus: Vec::new(),

            pawn_path_mask:
                vec![board!(files, ranks); board_size * piece_count],
            pawn_interference_mask:
                vec![board!(files, ranks); board_size * piece_count],
            pawn_support_mask:
                vec![board!(files, ranks); board_size * piece_count],
            pawn_advancement: vec![0; board_size * piece_count],
            pawn_passed_opening: vec![0; board_size * piece_count],
            pawn_passed_endgame: vec![0; board_size * piece_count],
            pawn_connected_opening: 0,
            pawn_connected_endgame: 0,
            pawn_doubled_penalty: 0,
            pawn_isolated_penalty: 0,
            pawn_backward_penalty: 0,
            pawn_backward_mask:
                vec![board!(files, ranks); board_size * piece_count],
            pawn_support_offsets: vec![Vec::new(); piece_count],
            pawn_passed_support_opening: vec![0; board_size * piece_count],
            pawn_passed_support_endgame: vec![0; board_size * piece_count],
        });

        Self::from_statics(statics)
    }

    /// State::from_statics
    ///
    /// Builds the dynamic half of a state around an already-precomputed
    /// static configuration, sharing it through the `Arc` instead of
    /// rebuilding it. Every board, piece list, and search table is
    /// allocated at its final size in the empty-board default, ready for a
    /// position to be loaded. This is the cheap path `fork` takes to branch
    /// a fresh game from a loaded variant without copying the template's
    /// history or search tables.
    ///
    /// Params:
    /// - statics: Arc<StaticState> -> precomputed configuration to share
    ///
    /// Return:
    /// State                       -> an empty-board state over the shared
    ///                                configuration
    ///
    fn from_statics(statics: Arc<StaticState>) -> State {
        let piece_count = statics.pieces.len();
        let board_size = statics.board_size;
        let cont_dim = piece_count * board_size;
        let files = statics.files;
        let ranks = statics.ranks;

        State {
            statics,

            game_over: false,
            game_phase: OPENING,
            phase_score: 0,

            playing: WHITE,
            main_board: vec![NO_PIECE; board_size],

            pieces_board: [board!(files, ranks); 2],
            pawn_board: [board!(files, ranks); 2],
            virgin_board: board!(files, ranks),

            castling_state: 0,
            has_castled: [false; 2],
            halfmove_clock: 0,
            en_passant_square: NO_EN_PASSANT,

            position_hash: u128::default(),
            pawn_hash: u128::default(),
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
            piece_list: vec![NO_SQUARE; piece_count * board_size],
            piece_in_hand: [vec![0; piece_count], vec![0; piece_count]],

            position_hash_map: HashMap::with_capacity(128),
            pv_line: array::from_fn(|_| null_move()),
            pv_table: vec![null_move(); PV_STRIDE * PV_STRIDE],
            pv_length: vec![0; PV_STRIDE],

            cont_hist: vec![0i16; 2 * cont_dim * cont_dim],
            search_hist: vec![0i16; piece_count * board_size * board_size],
            capt_hist: vec![
                0i16; piece_count * board_size * CAPT_HIST_BUCKETS
            ],
            corr_hist: vec![0i16; 2 * CORR_HIST_SIZE],
            killer_hist: vec![array::from_fn(|_| null_move()); MAX_DEPTH],
            static_eval: vec![-INF; MAX_DEPTH],
            excluded: vec![null_pseudo_move(); MAX_DEPTH],
        }
    }

    /// State::static_mut
    ///
    /// Grants mutable access to the shared static configuration during the
    /// single-threaded setup phase (config parsing and precomputation).
    ///
    /// Return:
    /// &mut StaticState -> exclusive reference into the statics Arc
    ///
    /// Notes:
    /// Uses `unwrap_unchecked`: callers must guarantee no other Arc clone
    /// exists yet, which holds because search threads are only spawned
    /// after setup completes.
    ///
    #[inline]
    pub fn static_mut(&mut self) -> &mut StaticState {
        unsafe { Arc::get_mut(&mut self.statics).unwrap_unchecked() }
    }

    /// State::reset
    ///
    /// Returns every dynamic field to its empty-board default while leaving
    /// the shared static configuration untouched, so a new game or FEN can
    /// be loaded without re-deriving the precomputed tables.
    ///
    pub fn reset(&mut self) {
        let piece_count = self.statics.pieces.len();
        let board_size = self.statics.board_size;
        let cont_dim = piece_count * board_size;

        self.playing = WHITE;
        self.main_board = vec![NO_PIECE; board_size];

        self.pieces_board = [board!(
            self.statics.files, self.statics.ranks
        ); 2];
        self.pawn_board = [board!(
            self.statics.files, self.statics.ranks
        ); 2];
        self.virgin_board = board!(
            self.statics.files, self.statics.ranks
        );

        self.castling_state = 0;
        self.has_castled = [false; 2];
        self.halfmove_clock = 0;
        self.en_passant_square = NO_EN_PASSANT;

        self.position_hash = u128::default();
        self.pawn_hash = u128::default();
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
        self.piece_list = vec![NO_SQUARE; piece_count * board_size];
        self.piece_in_hand = [vec![0; piece_count], vec![0; piece_count]];

        self.position_hash_map.clear();
        self.pv_line = array::from_fn(|_| null_move());
        self.pv_table = vec![null_move(); PV_STRIDE * PV_STRIDE];
        self.pv_length = vec![0; PV_STRIDE];

        self.cont_hist = vec![0i16; 2 * cont_dim * cont_dim];
        self.search_hist =
            vec![0i16; piece_count * board_size * board_size];
        self.capt_hist =
            vec![0i16; piece_count * board_size * CAPT_HIST_BUCKETS];
        self.corr_hist = vec![0i16; 2 * CORR_HIST_SIZE];
        self.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];
        self.static_eval = vec![-INF; MAX_DEPTH];
        self.excluded = vec![null_pseudo_move(); MAX_DEPTH];
    }

    /// State::load_fen
    ///
    /// Resets the dynamic state and repopulates it from a FEN string,
    /// optionally translating piece letters through a variant dictionary.
    ///
    /// Params:
    /// - fen : &str                -> FEN string to load
    /// - dict: Option<&Translator> -> optional piece-letter translator
    ///
    pub fn load_fen(&mut self, fen: &str, dict: Option<&Translator>) {
        self.reset();
        parse_fen(self, fen, dict);
    }

    /// State::fork
    ///
    /// Branches a fresh game from a loaded variant: a new state over the
    /// same shared `statics`, wound to the variant's start position with
    /// its evaluation caches refreshed. Cheaper than a `clone` followed by
    /// `reset`, since the template's move history and search tables are
    /// never copied.
    ///
    /// Return:
    /// State -> a fresh, ready-to-play state at the start position
    ///
    /// Notes:
    /// The start FEN is parsed with no translator: it is the engine's own
    /// internal notation, and a protocol dictionary can corrupt an internal
    /// FEN round-trip.
    ///
    pub fn fork(&self) -> State {
        let mut state = State::from_statics(Arc::clone(&self.statics));
        state.load_fen(&self.statics.startpos, None);
        refresh_eval_state(&mut state);
        state
    }

    /// State::play_random_opening
    ///
    /// Advances the position by up to `plies` uniformly random legal moves,
    /// stopping early if a position has no legal move. Each choice is drawn
    /// from the shared seeded RNG so self-play and match openings vary
    /// between runs; the moves are recorded in the history like any other.
    ///
    /// Params:
    /// - plies: usize -> number of random plies to apply
    ///
    pub fn play_random_opening(&mut self, plies: usize) {
        for _ in 0..plies {
            let legal = legal_moves!(self);
            if legal.is_empty() {
                break;
            }

            let choice = {
                let mut rng = RNG.lock().unwrap_or_else(|e| {
                    panic!("Failed to lock RNG for random opening: {e}")
                });
                legal.choose(&mut *rng).unwrap_or_else(|| {
                    panic!("Empty legal moves after non-empty check")
                }).clone()
            };

            make_move!(self, choice);
        }
    }

    /// State::generate_piece_moves / _drops / _stand_off
    ///
    /// Expression-compilation helpers run once at precompute time. Each takes
    /// one raw expression string per piece (in config order) and compiles it
    /// into that piece's runtime structure, leaving board-aware expansion to
    /// the moves module:
    ///
    /// - `generate_piece_moves`:
    ///   packed `Leg` lists (`MoveSet`), via `generate_move_vectors`
    ///
    /// - `generate_piece_drops`:
    ///   `DropSet`s, via `generate_drop_vectors`
    ///
    /// - `generate_piece_stand_off`:
    ///   `PatternSet`s, via `generate_stand_off_patterns`
    ///
    /// Params:
    /// - expr_set -> one expression string per piece, in config order
    ///
    /// Return:
    /// one compiled set per piece, indexed by PieceIndex
    ///
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

    /// State::populate_relevant_moves / _captures / _drops / _setup /
    /// _stand_offs
    ///
    /// Precompute-time table fillers. Each walks every (piece, square) pair
    /// and stores, at `piece * board_size + square`, the compiled entries that
    /// stay on the board when played from that square, turning the per-piece
    /// sets from the `generate_piece_*` helpers into flat, square-indexed
    /// lookup tables the generator reads at runtime:
    ///
    /// - `populate_relevant_moves`:
    ///   fills `relevant_moves` from move sets
    ///
    /// - `populate_relevant_captures`:
    ///   fills `relevant_captures` from move sets
    ///
    /// - `populate_relevant_drops`:
    ///   fills `relevant_drops` from drop sets
    ///
    /// - `populate_relevant_setup`:
    ///   fills `relevant_setup` from setup drops
    ///
    /// - `populate_relevant_stand_offs`:
    ///   fills `relevant_stand_offs` from patterns
    ///
    /// Params:
    /// - compiled -> the matching per-piece compiled set list
    ///
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

    /// State::populate_relevant_attacks
    ///
    /// Fills the reverse attack tables: for every square, records which
    /// (piece, origin, vector) triples could attack it, split by color.
    /// Check detection uses these to scan only plausible attackers rather
    /// than every enemy piece on the board.
    ///
    fn populate_relevant_attacks(&mut self) {
        for square in 0..self.statics.board_size {
            generate_attack_masks(square as Square, self);
        }
    }

    /// State::populate_adjacency_mask
    ///
    /// Builds, for every square, a bitboard of its up-to-eight neighbours,
    /// clipped at the board edges:
    ///
    /// ```text
    /// ┌────┬────┬────┐
    /// │ ## │ ## │ ## │
    /// ├────┼────┼────┤
    /// │ ## │ sq │ ## │
    /// ├────┼────┼────┤
    /// │ ## │ ## │ ## │
    /// └────┴────┴────┘
    /// ```
    ///
    /// The masks accelerate king-safety and pawn-connectivity tests during
    /// evaluation, replacing per-use neighbour arithmetic with one lookup.
    ///
    fn populate_adjacency_mask(&mut self) {
        let file_count = self.statics.files;
        let rank_count = self.statics.ranks;
        let board_size = self.statics.board_size;
        let files = file_count as i32;
        let ranks = rank_count as i32;

        let mut results = vec![board!(file_count, rank_count); board_size];

        for (square, board) in results.iter_mut().enumerate() {
            let start_file = square as i32 % files;
            let start_rank = square as i32 / files;

            for file_offset in -1..=1 {
                for rank_offset in -1..=1 {
                    if file_offset == 0 && rank_offset == 0 {
                        continue;
                    }

                    let neighbour_file = start_file + file_offset;
                    let neighbour_rank = start_rank + rank_offset;

                    if neighbour_file >= 0 && neighbour_file < files
                    && neighbour_rank >= 0 && neighbour_rank < ranks {
                        let neighbour = neighbour_rank * files + neighbour_file;
                        set!(board, neighbour as u32);
                    }
                }
            }
        }

        self.static_mut().adjacency_mask = results;
    }

    /// State::precompute
    ///
    /// One-off derivation pass that turns the variant's raw expression
    /// strings into every runtime lookup table: relevant moves, captures,
    /// drops, setup drops, stand-offs, reverse attack masks, and adjacency
    /// masks. Runs once after config parsing and before any search thread
    /// is spawned; optional tables are skipped when their special rule is
    /// disabled.
    ///
    /// Params:
    /// - moves_expr_set    : Vec<String> -> per-piece move expressions
    /// - drops_expr_set    : Vec<String> -> per-piece drop expressions
    /// - setup_expr_set    : Vec<String> -> per-piece setup expressions
    /// - stand_off_expr_set: Vec<String> -> per-piece stand-off expressions
    ///
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
        self.populate_adjacency_mask();
    }
}
