//! state.rs
//!
//! Defines game state representation and management.
//!
//! Everything the engine does reads or mutates one position, so the shape of
//! that value decides how cheap search, evaluation, and make/undo can be.
//! This file defines that centre of gravity: the immutable per-variant
//! configuration shared across threads, and the mutable per-position state
//! that search clones, advances a ply, and rolls back.
//!
//! Created: 25/01/2025
//! Author : Alden Luthfi

use crate::*;

/// Square
///
/// A board square addressed by the flat index `rank * files + file`.
///
/// Sixteen bits cover every supported board size, including every square a
/// `U4096` bitboard can address.
pub type Square = u16;

/*----------------------------------------------------------------------------*\
                          SPECIAL RULES REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Special-rules bitmask accessor/encoder macros.
///
/// The `special_rules` field in [`State`] uses one bit per optional rule.
/// Each pair contains a reader, `rule_name!(state)`, and a writer,
/// `enc_rule_name!(rules)`, for the same bit.
///
/// Reader params (every reader):
///
/// - state: &State -> position whose rule flags are read
///
/// castling!
///
///   Return:
///   bool -> castling enabled (bit 0)
///
/// en_passant!
///
///   Return:
///   bool -> en passant enabled (bit 1)
///
/// promotions!
///
///   Return:
///   bool -> promotions enabled (bit 2)
///
/// drops!
///
///   Return:
///   bool -> drops enabled (bit 3)
///
/// forbidden_zones!
///
///   Return:
///   bool -> forbidden zones enabled (bit 4)
///
/// promote_to_captured!
///
///   Return:
///   bool -> captures promote the capturer's pool piece (bit 5)
///
/// setup_phase!
///
///   Return:
///   bool -> game starts with a setup phase (bit 6)
///
/// stand_offs!
///
///   Return:
///   bool -> a move may create a stand-off (bit 7)
///
/// enc_castling! .. enc_stand_offs!
///
///   Params:
///
///   - rules: &mut u8
///     rules byte being built; each writer sets the bit its reader tests
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
macro_rules! setup_phase {
    ($state:expr) => {
        ($state.statics.special_rules >> 6 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_setup_phase {
    ($rules:expr) => {
        $rules |= 1 << 6;
    };
}

#[macro_export]
macro_rules! stand_offs {
    ($state:expr) => {
        ($state.statics.special_rules >> 7 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_stand_offs {
    ($rules:expr) => {
        $rules |= 1 << 7;
    };
}

/*----------------------------------------------------------------------------*\
                       SEARCH CAPABILITY REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Search-capability bitmask accessor/encoder macros.
///
/// The `capabilities` field in [`StaticState`] answers, once per variant and
/// before a game starts, which search shortcuts this rule set still permits.
/// Every shortcut here is a claim about the game that a variant may simply not
/// make: that material decides, that passing is bad, that a quiet move cannot
/// win on the spot. `derive_search_capabilities` sets a bit only when the rules
/// establish the claim, so a rule nobody has thought about leaves its bit
/// clear and the search plays the position out instead.
///
/// Each pair contains a reader, `capability!(state)`, and a writer,
/// `enc_capability!(mask)`, for the same bit.
///
/// Reader params (every reader):
///
/// - state: &State -> position whose capability flags are read
///
/// see_valid!
///
///   Return:
///   bool -> an exchange on one square is worth its material swing (bit 0)
///
/// see_pruning!
///
///   Return:
///   bool -> a capture priced as losing may be skipped outright (bit 1)
///
/// forward_pruning!
///
///   Return:
///   bool -> a static evaluation may stand in for a search (bit 2)
///
/// null_pruning!
///
///   Return:
///   bool -> giving up the move is a concession worth measuring (bit 3)
///
/// recapture_order!
///
///   Return:
///   bool -> capture ordering is monotone enough to stop early (bit 4)
///
/// quiet_pruning!
///
///   Return:
///   bool -> a late quiet move may be dropped unsearched (bit 5)
///
/// static_movement!
///
///   Return:
///   bool -> a piece's reach never needs another piece present (bit 6)
///
/// enc_see_valid! .. enc_static_movement!
///
///   Params:
///
///   - mask: &mut u16
///     capability mask being built; each writer sets the bit its reader tests
#[macro_export]
macro_rules! see_valid {
    ($state:expr) => {
        ($state.statics.capabilities & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_see_valid {
    ($mask:expr) => {
        $mask |= 1;
    };
}

#[macro_export]
macro_rules! see_pruning {
    ($state:expr) => {
        ($state.statics.capabilities >> 1 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_see_pruning {
    ($mask:expr) => {
        $mask |= 1 << 1;
    };
}

#[macro_export]
macro_rules! forward_pruning {
    ($state:expr) => {
        ($state.statics.capabilities >> 2 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_forward_pruning {
    ($mask:expr) => {
        $mask |= 1 << 2;
    };
}

#[macro_export]
macro_rules! null_pruning {
    ($state:expr) => {
        ($state.statics.capabilities >> 3 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_null_pruning {
    ($mask:expr) => {
        $mask |= 1 << 3;
    };
}

#[macro_export]
macro_rules! recapture_order {
    ($state:expr) => {
        ($state.statics.capabilities >> 4 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_recapture_order {
    ($mask:expr) => {
        $mask |= 1 << 4;
    };
}

#[macro_export]
macro_rules! quiet_pruning {
    ($state:expr) => {
        ($state.statics.capabilities >> 5 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_quiet_pruning {
    ($mask:expr) => {
        $mask |= 1 << 5;
    };
}

#[macro_export]
macro_rules! static_movement {
    ($state:expr) => {
        ($state.statics.capabilities >> 6 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_static_movement {
    ($mask:expr) => {
        $mask |= 1 << 6;
    };
}

/*----------------------------------------------------------------------------*\
                            EN PASSANT REPRESENTATION
\*----------------------------------------------------------------------------*/

/// EnPassantSquare
///
/// Packed en-passant descriptor for a target square, captured square, and
/// captured piece index. `NO_EN_PASSANT` represents the absence of a legal
/// en-passant opportunity.
///
/// ```text
///   0                       12                      24              31
///   ┌───────────────────────┬───────────────────────┬────────────────┐
///   │     target square     │    captured square    │     piece      │
///   └───────────────────────┴───────────────────────┴────────────────┘
/// ```
///
/// - Bits 0..11  : capture target square
/// - Bits 12..23 : square of the capturable piece
/// - Bits 24..31 : captured piece index
pub type EnPassantSquare = u32;

/// En passant packed-field accessor macros.
///
/// Every accessor takes the same single parameter:
///
/// - en_passant: EnPassantSquare -> packed descriptor read
///
/// enp_square!
///
///   Return:
///   u32 -> capture target square (bits 0-11)
///
/// enp_captured!
///
///   Return:
///   u32 -> square of the capturable piece (bits 12-23)
///
/// enp_piece!
///
///   Return:
///   u32 -> captured piece index (bits 24-31)
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

    pub in_check: Option<bool>,                                                 /* side to move checked after move    */
    pub in_stand_off: Option<bool>,                                             /* stand-off remained after move      */

    pub castling_state: u8,                                                     /* castling rights before move        */
    pub halfmove_clock: u8,                                                     /* halfmove clock before move         */
    pub repetition_clock: u16,                                                  /* reversible-ply clock before move   */
    pub counting: Option<(u16, u16)>,                                           /* bare-king (count, limit) before mv */
    pub en_passant_square: EnPassantSquare,                                     /* en passant sq before move          */
    pub game_result: u8,                                                        /* terminal result before move        */
    pub check_count: [u8; 2],                                                   /* checks delivered before move       */
    pub game_phase: u8,                                                         /* game phase before move             */
    pub phase_score: u32,                                                       /* phase score before move            */

    pub position_hash: u128,                                                    /* canonical hash before move         */
    pub virgin_hash: u128,                                                      /* virgin-state hash before move      */
}

impl Default for Snapshot {
    fn default() -> Self {
        Snapshot {
            move_ply: null_move(),
            in_check: None,
            in_stand_off: None,
            castling_state: 0,
            halfmove_clock: 0,
            repetition_clock: 0,
            counting: None,
            en_passant_square: EnPassantSquare::MAX,
            game_result: ONGOING,
            check_count: [0; 2],
            game_phase: OPENING,
            phase_score: 0,
            position_hash: u128::default(),
            virgin_hash: u128::default(),
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
/// piece_squares!
///
///   Params:
///   - state      : &State    -> position whose piece list is read
///   - piece_index: usize     -> piece whose row is walked
///
///   Return:
///   Iterator<Item = &Square> -> the piece's occupied squares
///
/// piece_list_push!
///
///   Params:
///   - state      : &mut State -> position whose piece list is grown
///   - piece_index: usize      -> piece whose row gains the square
///   - square     : Square     -> square appended after the last slot
///
/// piece_list_remove!
///
///   Params:
///   - state      : &mut State -> position whose piece list shrinks
///   - piece_index: usize      -> piece whose row loses the square
///   - square     : Square     -> square swap-removed
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
/// - snapshot: &Snapshot -> the history entry whose move is inspected
///
/// Return:
/// bool                  -> true if the snapshotted move is a pass
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
/// - state: &State -> position whose remaining material is tallied
///
/// Return:
/// u32             -> summed opening value of all non-royal big pieces
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

/// is_terminal!
///
/// Tests whether an eager, position-local terminal result has been stored.
/// Any value other than `ONGOING` means move generation and search must stop;
/// on-demand repetition/perpetual game truth comes from `game_outcome`.
///
/// Params:
/// - state: &State -> position whose result field is tested
///
/// Return:
/// bool            -> true once the game has reached a terminal outcome
#[macro_export]
macro_rules! is_terminal {
    ($state:expr) => {
        $state.termination.game_result != ONGOING
    };
}

/*----------------------------------------------------------------------------*\
                            GAME STATE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// StaticState
///
/// Immutable variant configuration, shared across threads via Arc.
/// All fields fixed after `precompute()` live here. `State::clone()` shares
/// this via `Arc::clone` instead of deep-copying.
///
/// The special rules field is a bitmask representing enabled special rules.
/// (read configs/example.conf for more information)
///
/// ```text
///   0               7
///   ┌─┬─┬─┬─┬─┬─┬─┬─┐
///   │c│e│p│d│f│t│s│o│
///   └─┴─┴─┴─┴─┴─┴─┴─┘
/// ```
///
/// The bits are defined as follows:
///
/// - bit 0      : castling allowed
/// - bit 1      : en passant allowed
/// - bit 2      : promotions allowed
/// - bit 3      : drops allowed
/// - bit 4      : some pieces have forbidden zones
/// - bit 5      : promotes only to friendly pieces captured by the enemy
/// - bit 6      : game begins with a setup phase
/// - bit 7      : a move may create a stand-off
///
pub struct StaticState {
    pub title: String,
    pub startpos: String,

    pub pieces: Vec<Piece>,
    pub special_rules: u8,
    pub capabilities: u16,                                                      /* search shortcuts the rules allow   */

    pub initial_setup: Vec<Board>,                                              /* piece index to board               */

    pub forbidden_zones: Vec<Board>,                                            /* piece to forbidden zone bitboard   */
    pub promotion_zones_optional: Vec<Board>,                                   /* piece to promotion zone bitboard   */
    pub promotion_zones_mandatory: Vec<Board>,                                  /* piece to promotion zone bitboard   */
    pub critical_castling: [Board; 4],                                          /* KQkq critical squares for each     */

    pub castling_pieces: Vec<bool>,                                             /* moving/capturing voids rights      */

    pub files: u8,
    pub ranks: u8,
    pub board_size: usize,

    pub relevant_moves: Vec<MoveSet>,                                           /* idx = piece * board size + square  */
    pub relevant_captures: Vec<MoveSet>,                                        /* flattened because of cache         */
    pub relevant_drops: Vec<DropSet>,                                           /* optimization                       */
    pub relevant_setup: Vec<DropSet>,
    pub relevant_stand_offs: Vec<PatternSet>,                                   /* facing-config veto patterns        */
    pub relevant_attacks: [Vec<Vec<AttackMask>>; 2],
    pub relevant_castling: [Vec<Move>; 4],                                      /* KQkq precomputed moves             */

    pub piece_swap_map: Vec<PieceIndex>,                                        /* piece index to swap color (if any) */
    pub piece_demotion_map: Vec<PieceIndex>,                                    /* piece index to demotion piece idx  */
    pub piece_char_map: HashMap<char, PieceIndex>,                              /* char to piece index map            */

/*----------------------------------------------------------------------------*\
                               EVALUATION FIELDS
\*----------------------------------------------------------------------------*/

    pub opening_score: u32,                                                     /* opening threshold                  */
    pub endgame_score: u32,                                                     /* endgame threshold                  */
    pub pst_opening: Vec<Vec<i32>>,                                             /* piece index to opening/middlegame  */
    pub pst_endgame: Vec<Vec<i32>>,                                             /* piece index to endgame PST         */

    pub shield_pieces: Vec<bool>,                                               /* piece index to shield-like role    */
    pub shelter_squares: [Vec<Square>; 2],                                      /* color to forward local squares     */
    pub shelter_counts: [Vec<u8>; 2],                                           /* squares stored per origin above    */
    pub ring_squares: Vec<Square>,                                              /* every local square, colour-blind   */
    pub ring_counts: Vec<u8>,                                                   /* squares stored per origin above    */
    pub local_stride: usize,                                                    /* slots each origin owns             */
    pub forward_steps: [i32; 2],                                                /* rank step each colour advances by  */
    pub shelter_value: i32,                                                     /* worth of one sheltering piece      */
    pub guard_value: i32,                                                       /* worth of one piece beside a royal  */
    pub castled_value: i32,                                                     /* worth of having castled already    */
    pub castling_right_value: i32,                                              /* worth of still being able to       */

    pub zone_attack: Vec<u8>,                                                   /* royal, piece, origin to pressure   */
    pub zone_attack_best: Vec<u8>,                                              /* pressure from its dearest origin   */
    pub king_danger_scale: i32,                                                 /* worth of a fully pressed zone      */
    pub king_danger_cap: i32,                                                   /* most a pressed zone may ever cost  */
    pub open_shield_penalty: i32,                                               /* cost of a royal nothing covers     */

    pub pawn_slots: Vec<usize>,                                                 /* piece index to pawn slot, or NONE  */
    pub pawn_pieces: Vec<usize>,                                                /* pawn slot to piece index           */
    pub pawn_stride: usize,                                                     /* squares each pawn slot owns        */
    pub pawn_path: Vec<Board>,                                                  /* slot, square to advance squares    */
    pub pawn_interference: Vec<Board>,                                          /* slot, square to passer stoppers    */
    pub pawn_support: Vec<Board>,                                               /* slot, square to defending squares  */
    pub pawn_backward: Vec<Board>,                                              /* slot, square to stop attackers     */
    pub pawn_support_files: Vec<Vec<i32>>,                                      /* slot to supporting file offsets    */
    pub pawn_passed_opening: Vec<i32>,                                          /* slot, square to passer worth       */
    pub pawn_passed_endgame: Vec<i32>,
    pub pawn_connected_opening: Vec<i32>,                                       /* slot to worth of being defended    */
    pub pawn_connected_endgame: Vec<i32>,
    pub pawn_doubled_penalty: Vec<i32>,                                         /* slot to cost of blocking itself    */
    pub pawn_isolated_penalty: Vec<i32>,                                        /* slot to cost of standing alone     */
    pub pawn_backward_penalty: Vec<i32>,                                        /* slot to cost of a contested stop   */

    pub draw_contempt: i32,                                                     /* a draw's cost one span ahead       */
    pub draw_span: i32,                                                         /* lead at which that cost saturates  */

/*----------------------------------------------------------------------------*\
                                 SEARCH FIELDS
\*----------------------------------------------------------------------------*/

    pub reduction_quiet: Vec<u8>,                                               /* plies given up, depth major, one   */
    pub reduction_quiet_check: Vec<u8>,                                         /* surface per class of move: quiet   */
    pub reduction_tactical: Vec<u8>,                                            /* or tactical, in check or not       */
    pub reduction_tactical_check: Vec<u8>,

    pub aspiration_delta: u32,                                                  /* half-width the root opens at       */
    pub rfp_margin: Vec<i32>,                                                   /* cushion, improving major, by depth */
    pub futility_margin: Vec<i32>,                                              /* alpha cushion, improving major     */
    pub lmp_count: Vec<usize>,                                                  /* moves ordered, improving major     */
    pub see_allowance: Vec<i32>,                                                /* loss a capture may show, by depth  */
    pub qsearch_delta: i32,                                                     /* gain a leaf capture must promise   */
}

/// State
///
/// Main state of the game.
///
/// Terminal rules (stalemate/checkmate outcome, repetition, counter, ...)
/// are not bits here; each position owns them in `State::termination`.
///
/// Static configuration lives in `statics: Arc<StaticState>`, shared
/// cheaply across threads. `State::clone()` calls `Arc::clone` for the
/// statics and deep-copies only the dynamic fields.
pub struct State {

    pub statics: Arc<StaticState>,
    pub termination: Termination,                                               /* rules, result, progress            */

/*----------------------------------------------------------------------------*\
                                 DYNAMIC FIELDS
\*----------------------------------------------------------------------------*/

    pub game_phase: u8,                                                         /* SETUP/OPENING/MIDDLEGAME/ENDGAME   */
    pub phase_score: u32,                                                       /* game phase score for transition    */

    pub playing: u8,                                                            /* side to move (WHITE / BLACK)       */
    pub main_board: Vec<u8>,                                                    /* standard mailbox approach          */

    pub pieces_board: [Board; 2],                                               /* per-color occupancy bitboards      */
    pub virgin_board: Board,                                                    /* squares whose piece is unmoved     */

    pub castling_state: u8,                                                     /* 4 bits for representing KQkq       */
    pub has_castled: [bool; 2],                                                 /* color to castled once already      */
    pub en_passant_square: EnPassantSquare,                                     /* active en passant square           */

    pub position_hash: u128,                                                    /* canonical incremental key          */
    pub virgin_hash: u128,                                                      /* virgin-state incremental key       */
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
    pub royal_list: [Vec<Square>; 2],                                           /* color to royal piece square list   */

    pub piece_count: Vec<u32>,                                                  /* piece index to count               */
    pub piece_list: Vec<Square>,                                                /* board_size slots per piece, packed */
    pub piece_in_hand: [Vec<u16>; 2],                                           /* color to pieces in hand list       */
}

impl Clone for State {
    fn clone(&self) -> Self {
        State {
            statics: Arc::clone(&self.statics),
            termination: self.termination.clone(),

            game_phase: self.game_phase,
            phase_score: self.phase_score,

            playing: self.playing,
            main_board: self.main_board.clone(),

            pieces_board: self.pieces_board,
            virgin_board: self.virgin_board,

            castling_state: self.castling_state,
            has_castled: self.has_castled,
            en_passant_square: self.en_passant_square,

            position_hash: self.position_hash,
            virgin_hash: self.virgin_hash,
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
            royal_list: self.royal_list.clone(),

            piece_count: self.piece_count.clone(),
            piece_list: self.piece_list.clone(),
            piece_in_hand: self.piece_in_hand.clone(),
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
    /// - special_rules: u8         -> special-rules bitmask (see [`State`])
    ///
    /// Return:
    ///
    /// Self
    /// a fresh state with empty boards and zeroed search tables
    pub fn new(
        title: String,
        startpos: String,
        files: u8,
        ranks: u8,
        pieces: Vec<Piece>,
        special_rules: u8,
    ) -> Self {

        let piece_count: usize = pieces.len();
        let board_size: usize = (files as usize) * (ranks as usize);

        assert!(
            board_size <= MAX_SQUARES,
            "Board {}x{} needs {} squares, but this build caps at {}",
            files, ranks, board_size, MAX_SQUARES
        );

        let statics = Arc::new(StaticState {
            title,
            startpos,
            pieces,
            special_rules,
            capabilities: 0,                                                    /* nothing is allowed until derived   */

            initial_setup: vec![board!(files, ranks); piece_count],

            forbidden_zones: vec![board!(files, ranks); piece_count],
            promotion_zones_optional: vec![board!(files, ranks); piece_count],
            promotion_zones_mandatory: vec![board!(files, ranks); piece_count],
            critical_castling: [board!(files, ranks); 4],

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

            opening_score: 0,
            endgame_score: 0,
            pst_opening: vec![vec![0; board_size]; piece_count],
            pst_endgame: vec![vec![0; board_size]; piece_count],

            shield_pieces: vec![false; piece_count],
            shelter_squares: [Vec::new(), Vec::new()],
            shelter_counts: [Vec::new(), Vec::new()],
            ring_squares: Vec::new(),
            ring_counts: Vec::new(),
            local_stride: 0,
            forward_steps: [1, -1],
            shelter_value: 0,
            guard_value: 0,
            castled_value: 0,
            castling_right_value: 0,
            zone_attack: Vec::new(),
            zone_attack_best: Vec::new(),
            king_danger_scale: 0,
            king_danger_cap: 0,
            open_shield_penalty: 0,

            pawn_slots: Vec::new(),
            pawn_pieces: Vec::new(),
            pawn_stride: 0,
            pawn_path: Vec::new(),
            pawn_interference: Vec::new(),
            pawn_support: Vec::new(),
            pawn_backward: Vec::new(),
            pawn_support_files: Vec::new(),
            pawn_passed_opening: Vec::new(),
            pawn_passed_endgame: Vec::new(),
            pawn_connected_opening: Vec::new(),
            pawn_connected_endgame: Vec::new(),
            pawn_doubled_penalty: Vec::new(),
            pawn_isolated_penalty: Vec::new(),
            pawn_backward_penalty: Vec::new(),

            draw_contempt: 0,
            draw_span: 1,                                                       /* a divisor before derivation runs   */

            reduction_quiet: Vec::new(),
            reduction_quiet_check: Vec::new(),
            reduction_tactical: Vec::new(),
            reduction_tactical_check: Vec::new(),

            aspiration_delta: 0,
            rfp_margin: Vec::new(),
            futility_margin: Vec::new(),
            lmp_count: Vec::new(),
            see_allowance: Vec::new(),
            qsearch_delta: 0,
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
    ///
    /// State
    /// an empty-board state over the shared configuration
    fn from_statics(statics: Arc<StaticState>) -> State {
        let piece_count = statics.pieces.len();
        let board_size = statics.board_size;
        let files = statics.files;
        let ranks = statics.ranks;

        State {
            statics,
            termination: Termination::default(),

            game_phase: OPENING,
            phase_score: 0,

            playing: WHITE,
            main_board: vec![NO_PIECE; board_size],

            pieces_board: [board!(files, ranks); 2],
            virgin_board: board!(files, ranks),

            castling_state: 0,
            has_castled: [false; 2],
            en_passant_square: NO_EN_PASSANT,

            position_hash: u128::default(),
            virgin_hash: u128::default(),
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
            royal_list: [Vec::new(), Vec::new()],

            piece_count: vec![0u32; piece_count],
            piece_list: vec![NO_SQUARE; piece_count * board_size],
            piece_in_hand: [vec![0; piece_count], vec![0; piece_count]],
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
    #[inline]
    pub fn static_mut(&mut self) -> &mut StaticState {
        unsafe { Arc::get_mut(&mut self.statics).unwrap_unchecked() }
    }

    /// State::reset
    ///
    /// Returns every dynamic field to its empty-board default while leaving
    /// the shared static configuration untouched, so a new game or FEN can
    /// be loaded without re-deriving the precomputed tables.
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
        self.has_castled = [false; 2];
        self.en_passant_square = NO_EN_PASSANT;

        self.position_hash = u128::default();
        self.virgin_hash = u128::default();
        self.history = Vec::with_capacity(8192);

        self.search_ply = 0;
        self.ply_counter = 0;

        self.game_phase = OPENING;
        self.termination.reset_progress();

        self.opening_material = [0; 2];
        self.endgame_material = [0; 2];
        self.opening_pst_bonus = [0; 2];
        self.endgame_pst_bonus = [0; 2];
        self.big_pieces = [0; 2];
        self.major_pieces = [0; 2];
        self.minor_pieces = [0; 2];
        self.royal_list = [Vec::new(), Vec::new()];

        self.piece_count = vec![0u32; piece_count];
        self.piece_list = vec![NO_SQUARE; piece_count * board_size];
        self.piece_in_hand = [vec![0; piece_count], vec![0; piece_count]];
    }

    /// State::load_fen
    ///
    /// Resets the dynamic state and repopulates it from a FEN string,
    /// optionally translating piece letters through a variant dictionary.
    ///
    /// Params:
    /// - fen : &str                -> FEN string to load
    /// - dict: Option<&Translator> -> optional piece-letter translator
    pub fn load_fen(&mut self, fen: &str, dict: Option<&Translator>) {
        self.reset();
        parse_fen(self, fen, dict)
            .unwrap_or_else(|error| panic!("{}", error));
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
    pub fn fork(&self) -> State {
        let mut state = State::from_statics(Arc::clone(&self.statics));
        state.termination = self.termination.clone();
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
    /// the moves module. Each returns one compiled set per piece, indexed
    /// by `PieceIndex`.
    ///
    /// generate_piece_moves
    ///
    ///   Params:
    ///   - expr_set: &Vec<String> -> one move expression per piece
    ///
    ///   Return:
    ///   Vec<MoveSet>             -> move sets from `generate_move_vectors`
    ///
    /// generate_piece_drops
    ///
    ///   Params:
    ///   - expr_set: &[String] -> one drop expression per piece
    ///
    ///   Return:
    ///   Vec<DropSet>          -> drop sets, via `generate_drop_vectors`
    ///
    /// generate_piece_stand_off
    ///
    ///   Params:
    ///   - expr_set: Vec<String> -> one stand-off expression per piece
    ///
    ///   Return:
    ///   Vec<PatternSet>         -> patterns, via `generate_stand_off_patterns`
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
    /// lookup tables the generator reads at runtime. None return a value;
    /// each writes its static table through `static_mut`.
    ///
    /// populate_relevant_moves
    ///
    ///   Params:
    ///
    ///   - piece_moves: &[MoveSet]
    ///     compiled move sets, one per piece; fills `relevant_moves`
    ///
    /// populate_relevant_captures
    ///
    ///   Params:
    ///
    ///   - piece_moves: &[MoveSet]
    ///     compiled move sets, one per piece; fills `relevant_captures`
    ///
    /// populate_relevant_drops
    ///
    ///   Params:
    ///
    ///   - piece_setup_drops: &[DropSet]
    ///     compiled drop sets, one per piece; fills `relevant_drops`
    ///
    /// populate_relevant_setup
    ///
    ///   Params:
    ///
    ///   - piece_setup_drops: &[DropSet]
    ///     compiled setup drops, one per piece; fills `relevant_setup`
    ///
    /// populate_relevant_stand_offs
    ///
    ///   Params:
    ///
    ///   - piece_stand_off: &[PatternSet]
    ///     compiled patterns, one per piece; fills `relevant_stand_offs`
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
    fn populate_relevant_attacks(&mut self) {
        for square in 0..self.statics.board_size {
            generate_attack_masks(square as Square, self);
        }
    }

    /// State::precompute
    ///
    /// One-off derivation pass that turns the variant's raw expression
    /// strings into every runtime lookup table: relevant moves, captures,
    /// drops, setup drops, stand-offs, and reverse attack masks. Runs once
    /// after config parsing and before any search thread is spawned;
    /// optional tables are skipped when their special rule is disabled.
    ///
    /// Params:
    /// - moves_expr_set    : Vec<String> -> per-piece move expressions
    /// - drops_expr_set    : Vec<String> -> per-piece drop expressions
    /// - setup_expr_set    : Vec<String> -> per-piece setup expressions
    /// - stand_off_expr_set: Vec<String> -> per-piece stand-off expressions
    pub fn precompute(
        &mut self,
        moves_expr_set: Vec<String>,
        drops_expr_set: Vec<String>,
        setup_expr_set: Vec<String>,
        stand_off_expr_set: Vec<String>,
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
