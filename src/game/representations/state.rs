//! state.rs
//!
//! Defines the game state.
//!
//! All engine work reads or changes one position, so its layout sets the
//! cost of search, evaluation, make and undo. This file defines the fixed
//! variant configuration that threads share, and the position state that
//! the search clones, moves forward and moves back.
//!
//! Created: 25/01/2025
//! Author : Alden Luthfi

use crate::*;

/// Square
///
/// A board square as the flat index `rank * files + file`. Sixteen bits
/// cover all `MAX_SQUARES` squares of a board.
///
pub type Square = u16;

/*----------------------------------------------------------------------------*\
                         SPECIAL RULES REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Special rule macros
///
/// Read and write the `special_rules` byte of [`StaticState`]. Each rule
/// has one bit, so the hot paths skip a full mechanism with one mask. Each
/// rule has a reader `rule!(state)` and a writer `enc_rule!(rules)`.
///
/// castling! .. stand_offs!
///
///   Params:
///   - state: &State -> position with the rule flags
///
///   Return:
///   bool            -> the rule below is on
///
/// - castling!            : castling, bit 0
/// - en_passant!          : en passant, bit 1
/// - promotions!          : promotion, bit 2
/// - drops!               : drops from the hand, bit 3
/// - forbidden_zones!     : forbidden zones, bit 4
/// - promote_to_captured! : promote only to pieces in the enemy hand, bit 5
/// - setup_phase!         : setup phase at the start, bit 6
/// - stand_offs!          : a move can make a stand-off, bit 7
///
/// enc_castling! .. enc_stand_offs!
///
///   Params:
///   - rules: &mut u8 -> rules byte to build at load time
///
/// Notes:
/// With bit 5, a piece can promote to a type only while the enemy has a
/// captured piece of that type in the hand. The promotion takes that piece
/// from the hand.
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

/// Promotion trigger macros
///
/// Read and write the `promotion_triggers` byte of [`StaticState`].
/// `promotions!` tells if a variant promotes. These tell which moves can
/// promote. A variant without a trigger gets the two, as in shogi.
///
/// promote_on_entry! / promote_on_exit!
///
///   Params:
///   - state: &State -> position with the triggers
///
///   Return:
///   bool            -> the trigger below is on
///
/// - promote_on_entry! : the move goes from outside into the zone, bit 0
/// - promote_on_exit!  : the move starts in the zone, bit 1
///
/// enc_promote_on_entry! / enc_promote_on_exit!
///
///   Params:
///   - triggers: &mut u8 -> trigger byte to build at load time
///
/// Notes:
/// A move inside the zone belongs to `exit`, so `entry` means only a move
/// into the zone. Shogi uses the two bits. Chu shogi uses only `entry`. A
/// promotion by capture is a piece rule: an `r` capture leg and a `!r`
/// capture leg.
///
#[macro_export]
macro_rules! promote_on_entry {
    ($state:expr) => {
        ($state.statics.promotion_triggers & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_promote_on_entry {
    ($triggers:expr) => {
        $triggers |= 1;
    };
}

#[macro_export]
macro_rules! promote_on_exit {
    ($state:expr) => {
        ($state.statics.promotion_triggers >> 1 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_promote_on_exit {
    ($triggers:expr) => {
        $triggers |= 1 << 1;
    };
}


/*----------------------------------------------------------------------------*\
                       SEARCH CAPABILITY REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Search capability macros
///
/// Read and write the `capabilities` mask of [`StaticState`]. Each bit
/// allows one search shortcut. A shortcut is a claim about the game, for
/// example that material decides. `derive_search_capabilities` sets a bit
/// only when the rules prove the claim. Else the bit stays clear.
///
/// see_valid! .. free_drops!
///
///   Params:
///   - state: &State -> position with the capability flags
///
///   Return:
///   bool            -> the shortcut below is allowed
///
/// - see_valid!       : an exchange is worth its material swing, bit 0
/// - see_pruning!     : a losing capture can be skipped, bit 1
/// - forward_pruning! : a static score can replace a search, bit 2
/// - null_pruning!    : a pass is never good, bit 3
/// - recapture_order! : capture order is monotone enough to cut, bit 4
/// - quiet_pruning!   : a late quiet move can be skipped, bit 5
/// - static_movement! : reach does not depend on other pieces, bit 6
/// - wide_quiescence! : a leaf can search any capture, bit 7
/// - free_drops!      : a hand piece drops on any empty square, bit 8
///
/// enc_see_valid! .. enc_free_drops!
///
///   Params:
///   - mask: &mut u16 -> capability mask to build at derive time
///
/// Notes:
/// Quiescence settles the exchange on one square. A search of all captures
/// is correct only while each capture costs a piece. If one move removes
/// many pieces, `wide_quiescence!` is false and the leaf follows the
/// contested square.
///
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

#[macro_export]
macro_rules! wide_quiescence {
    ($state:expr) => {
        ($state.statics.capabilities >> 7 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_wide_quiescence {
    ($mask:expr) => {
        $mask |= 1 << 7;
    };
}

#[macro_export]
macro_rules! free_drops {
    ($state:expr) => {
        ($state.statics.capabilities >> 8 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_free_drops {
    ($mask:expr) => {
        $mask |= 1 << 8;
    };
}

/*----------------------------------------------------------------------------*\
                           EN PASSANT REPRESENTATION
\*----------------------------------------------------------------------------*/

/// EnPassantSquare
///
/// Packed en passant descriptor: the target square, the captured square
/// and the captured piece index. `NO_EN_PASSANT` means no en passant.
///
/// ```text
///   0                     11                      22                31
///   ┌─────────────────────┬─────────────────────┬────────────────────┐
///   │    target square    │   captured square   │        piece       │
///   └─────────────────────┴─────────────────────┴────────────────────┘
/// ```
///
/// - Bits 0..10  : capture target square
/// - Bits 11..21 : square of the capturable piece
/// - Bits 22..31 : captured piece index
///
/// The three fields are necessary, because no field gives the others. The
/// distance between the target and the victim is the span of the `p` leg.
/// It is one square in FIDE chess, but not in all variants.
///
/// Notes:
/// Eleven bits cover `MAX_SQUARES` and ten bits cover `MAX_PIECES`, and the
/// load asserts both. The top piece index is reserved, so no real
/// descriptor is all ones, the value of `NO_EN_PASSANT`.
///
pub type EnPassantSquare = u32;

/// En passant field accessors
///
/// Read one field of an [`EnPassantSquare`] as a `u32`.
///
/// Params:
/// - en_passant: EnPassantSquare -> packed descriptor to read
///
/// Return:
/// u32                           -> the field below
///
/// - enp_square!   : capture target square, bits 0..10
/// - enp_captured! : square of the capturable piece, bits 11..21
/// - enp_piece!    : captured piece index, bits 22..31
///
#[macro_export]
macro_rules! enp_square {
    ($en_passant:expr) => {
        $en_passant & 0x7FF
    };
}

#[macro_export]
macro_rules! enp_captured {
    ($en_passant:expr) => {
        ($en_passant >> 11) & 0x7FF
    };
}

#[macro_export]
macro_rules! enp_piece {
    ($en_passant:expr) => {
        ($en_passant >> 22) & 0x3FF
    };
}

/*----------------------------------------------------------------------------*\
                            SNAPSHOT REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Snapshot
///
/// The state that `undo_move!` needs to restore a position. `make_move!`
/// adds one snapshot to `State::history` for each move.
///
/// Each field except the move has the value from before the move. Undo is a
/// copy, because a reset clock or a spent right cannot come from the board.
///
/// - `in_check`     : set only with a check count rule
/// - `in_stand_off` : set only with stand-offs
///
/// `None` means that the rule does not exist, not that the value is
/// unknown. Thus only these variants pay for the test.
///
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
    pub pawn_hash: u128,                                                        /* pawn placement before move         */
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
            pawn_hash: u128::default(),
        }
    }
}

/// Flat piece list accessors
///
/// Access the flat piece list. `piece_list` is one `Vec<Square>` with
/// `board_size` slots for each piece index. Each row has its squares at
/// the front, `piece_count` is their number, and the rest is `NO_SQUARE`.
/// Only push and remove change `piece_count`.
///
/// piece_squares!
///
///   Params:
///   - state      : &State    -> position with the piece list
///   - piece_index: usize     -> piece row to read
///
///   Return:
///   Iterator<Item = &Square> -> the squares of the piece
///
/// piece_list_push!
///
///   Params:
///   - state      : &mut State -> position with the piece list
///   - piece_index: usize      -> piece row that gets the square
///   - square     : Square     -> square to add after the last one
///
/// piece_list_remove!
///
///   Params:
///   - state      : &mut State -> position with the piece list
///   - piece_index: usize      -> piece row that loses the square
///   - square     : Square     -> square to swap-remove
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
/// Tells if a snapshot has a pass move. The repetition and stand-off tests
/// use it on the history.
///
/// Params:
/// - snapshot: &Snapshot -> history entry to test
///
/// Return:
/// bool                  -> true when the move is a pass
///
#[macro_export]
macro_rules! pass_snapshot {
    ($snapshot:expr) => {
        is_pass!($snapshot.move_ply)
    };
}

/// game_phase_score!
///
/// Calculates the phase score from scratch. It is the sum of the opening
/// values of all big pieces that are not royal, on the board and, in a
/// drop variant, in the hands. A piece in hand can come back at once, so
/// a capture does not move the game toward its end. `game_phase!` compares
/// the sum with the thresholds of the variant.
///
/// Params:
/// - state: &State -> position to count
///
/// Return:
/// u32             -> sum of the opening values of the big pieces
///
#[macro_export]
macro_rules! game_phase_score {
    ($state:expr) => {{
        let mut phase_score = 0;

        for (piece_idx, piece) in $state.statics.pieces.iter().enumerate() {
            if p_is_big!(piece) && !p_is_royal!(piece) {
                let in_hand = drops!($state) as u32
                    * $state.piece_in_hand[p_color!(piece) as usize][piece_idx]
                        as u32;

                phase_score += p_ovalue!(piece) as u32
                    * ($state.piece_count[piece_idx] + in_hand);
            }
        }

        phase_score
    }};
}

/// game_phase!
///
/// Gives the phase of a position from `phase_score` and the two variant
/// thresholds. A promotion or a drop can move the phase back. `SETUP`
/// stays until the move that empties the two hands.
///
/// Params:
/// - state: &State -> position to examine
///
/// Return:
/// u8              -> SETUP, OPENING, MIDDLEGAME or ENDGAME
///
#[macro_export]
macro_rules! game_phase {
    ($state:expr) => {
        if $state.game_phase == SETUP {
            SETUP
        } else if $state.phase_score > $state.statics.opening_score {
            OPENING
        } else if $state.phase_score < $state.statics.endgame_score {
            ENDGAME
        } else {
            MIDDLEGAME
        }
    };
}

/// is_terminal!
///
/// Tells if the position has a stored game result. A value other than
/// `ONGOING` stops move generation and search. `game_outcome` gives the
/// repetition and perpetual results.
///
/// Params:
/// - state: &State -> position to test
///
/// Return:
/// bool            -> true when the game has ended
///
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
/// The fixed variant configuration, shared between threads with an `Arc`.
/// It has all fields that do not change after `precompute()`.
/// `State::clone()` shares it with `Arc::clone`.
///
/// The special rules bitmask (see res/config/example.conf):
///
/// ```text
///   0               7
///   ┌─┬─┬─┬─┬─┬─┬─┬─┐
///   │c│e│p│d│f│t│s│o│
///   └─┴─┴─┴─┴─┴─┴─┴─┘
/// ```
///
/// - bit 0 : castling
/// - bit 1 : en passant
/// - bit 2 : promotion
/// - bit 3 : drops
/// - bit 4 : some pieces have forbidden zones
/// - bit 5 : promotion only to own pieces in the enemy hand
/// - bit 6 : setup phase at the start
/// - bit 7 : a move can make a stand-off
///
/// `capabilities` is a second mask, nine bits in a `u16`, of the allowed
/// search shortcuts. Derivation sets it, and the `see_valid!` macro group
/// gives the bits.
///
pub struct StaticState {
    pub title: String,                                                          /* name the variant is shown as       */
    pub startpos: String,                                                       /* FEN the game begins from           */

    pub pieces: Vec<Piece>,
    pub special_rules: u8,                                                      /* declared rule bits                 */
    pub capabilities: u16,                                                      /* search shortcuts the rules allow   */

    pub initial_setup: Vec<Board>,                                              /* piece index to board               */

    pub forbidden_zones: Vec<Board>,                                            /* piece to forbidden zone bitboard   */
    pub promotion_zones_optional: Vec<Board>,                                   /* piece to promotion zone bitboard   */
    pub promotion_zones_mandatory: Vec<Board>,                                  /* piece to promotion zone bitboard   */
    pub promotion_triggers: u8,                                                 /* what earns a promotion offer       */
    pub critical_castling: [Board; 4],                                          /* KQkq critical squares for each     */

    pub castling_pieces: Vec<bool>,                                             /* moving/capturing voids rights      */

    pub files: u8,                                                              /* squares across the board           */
    pub ranks: u8,                                                              /* squares up the board               */
    pub board_size: usize,                                                      /* files * ranks                      */

    pub relevant_moves: Vec<MoveSet>,                                           /* idx = piece * board size + square  */
    pub relevant_captures: Vec<MoveSet>,                                        /* flattened because of cache         */
    pub capture_reach: Vec<u64>,                                                /* piece, square to capture leg reach */
    pub capture_destroys: Vec<bool>,                                            /* piece to an own-piece capture leg  */
    pub relevant_drops: Vec<DropSet>,                                           /* optimization                       */
    pub relevant_setup: Vec<DropSet>,                                           /* setup-phase army placement         */
    pub relevant_stand_offs: Vec<PatternSet>,                                   /* facing-config veto patterns        */
    pub relevant_attacks: [Vec<Vec<AttackMask>>; 2],                            /* [side][square] to its attackers    */
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

    pub eval: EvalParams,                                                       /* everything else eval reads         */
    pub search: SearchParams,                                                   /* everything search reads            */
}

/// State
///
/// The main game state. The end rules are in `State::termination`, not in
/// the rule bits.
///
/// - `statics`     : the shared configuration, `Arc::clone` on a clone
/// - `termination` : the end rules, result and progress
/// - dynamic       : the position data, copied on a clone
/// - `scratch`     : work memory of search and evaluation
///
/// `scratch` is in the state, because the macros that fill it already have
/// the state. Each copy gets its own.
///
pub struct State {

    pub statics: Arc<StaticState>,
    pub termination: Termination,                                               /* rules, result, progress            */

/*----------------------------------------------------------------------------*\
                                 DYNAMIC FIELDS
\*----------------------------------------------------------------------------*/

    pub game_phase: u8,                                                         /* SETUP/OPENING/MIDDLEGAME/ENDGAME   */
    pub phase_score: u32,                                                       /* game phase score for transition    */

    pub playing: u8,                                                            /* side to move (WHITE / BLACK)       */
    pub main_board: Vec<PieceIndex>,                                            /* standard mailbox approach          */

    pub pieces_board: [Board; 2],                                               /* per-color occupancy bitboards      */
    pub virgin_board: Board,                                                    /* squares whose piece is unmoved     */

    pub castling_state: u8,                                                     /* KQkq rights, castled marks above   */
    pub en_passant_square: EnPassantSquare,                                     /* active en passant square           */

    pub position_hash: u128,                                                    /* canonical incremental key          */
    pub virgin_hash: u128,                                                      /* virgin-state incremental key       */
    pub pawn_hash: u128,                                                        /* pawn-placement incremental key     */
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

    pub scratch: Scratch,                                                       /* what reading this position costs   */
}

/*----------------------------------------------------------------------------*\
                             SCRATCH REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Scratch
///
/// The work memory of search and evaluation:
///
/// - `node_lists`   : one list set for each ply
/// - `see_moves`    : attackers that `see!` fills for each exchange
/// - `see_scratch`  : multi-capture data of those attackers
/// - `pawn_rosters` : pawn lists that `pawn_structure!` fills
/// - `pawn_table`   : the pawn structure cache
/// - `eval_table`   : the static score cache of `evaluate_position!`
///
/// Code that keeps a vector across `make_move!` takes it out of the
/// [`State`] and puts it back, because a field borrow across a move borrows
/// the full position. `pawn_structure!` makes no move, so it borrows in
/// place.
///
/// Notes:
/// A clone gets empty memory, except the two caches. Their keys are the
/// pawn hash and the position hash, so they are correct for all positions.
/// `State::reset` and new parameters clear them.
///
pub struct Scratch {

    pub node_lists: Vec<NodeLists>,                                             /* one set per ply, MAX_DEPTH deep    */
    pub see_moves: Vec<Move>,                                                   /* attackers of one square, popped    */
    pub see_scratch: Vec<u64>,                                                  /* multi-capture data of see_moves    */
    pub pawn_rosters: [Vec<PawnEntry>; 2],                                      /* colour to its pawns, one sweep old */
    pub pawn_table: PTable,                                                     /* arrangement to its two scores      */
    pub eval_table: Vec<EvalEntry>,                                             /* position to its static score       */
}

/// NodeLists
///
/// The work lists of one node: its moves, their ordering scores and the
/// multi-capture records. A node keeps them until it returns, while its
/// children fill their own. Thus [`Scratch`] has one set for each ply.
///
/// Notes:
/// The depth guards of `alpha_beta` and `quiescence_search` return first,
/// so the ply is at most `MAX_DEPTH`. Each set keeps its largest capacity,
/// so a search allocates only once for each ply.
///
#[derive(Default)]
pub struct NodeLists {

    pub moves: Vec<Move>,                                                       /* this node's pseudo-legal moves     */
    pub scores: Vec<usize>,                                                     /* ordering score, filled lazily      */
    pub payload: Vec<u64>,                                                      /* multi-capture records under them   */
}

/// PawnEntry
///
/// One pawn for the two passes of `pawn_structure!`: `(slot, square, file,
/// passed)`. The first pass sets `passed`. The second pass reads only this
/// list, not the piece lists.
///
pub type PawnEntry = (usize, Square, i32, bool);

/// EvalEntry
///
/// One cached static score: `(check, score)`. The low bits of the position
/// key select the slot and the high 64 bits are the check, so a slot that
/// another position took gives no score. An empty slot has check zero.
///
pub type EvalEntry = (u64, i32);

impl Default for Scratch {
    /// Scratch::default
    ///
    /// Makes empty work memory: one node list set for each ply, the two
    /// exchange vectors, the two pawn lists and the two empty caches. A
    /// new, cloned or reset state gets it.
    ///
    /// Return:
    /// Self -> empty work memory with start capacities
    ///
    /// Notes:
    /// The capacities are not limits. The vectors grow when necessary.
    ///
    fn default() -> Self {
        Scratch {
            node_lists: (0..=MAX_DEPTH)
                .map(|_| NodeLists::default()).collect(),
            see_moves: Vec::with_capacity(64),                                  /* one square's worth of attackers    */
            see_scratch: Vec::with_capacity(32),                                /* and their multi-capture payload    */
            pawn_rosters: [
                Vec::with_capacity(32), Vec::with_capacity(32),
            ],
            pawn_table: PTable::default(),
            eval_table: vec![(0, 0); EVAL_TABLE_ENTRIES],
        }
    }
}

impl Clone for State {
    /// State::clone
    ///
    /// Copies the position data: boards, hashes, piece lists, hands,
    /// history and evaluation totals. It shares the static configuration
    /// through its `Arc`.
    ///
    /// Return:
    /// Self -> an independent position with the same configuration
    ///
    /// Notes:
    /// The copy gets an empty [`Scratch`], except `pawn_table`. The pawn
    /// cache depends only on the pawns, so it stays correct.
    ///
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
            en_passant_square: self.en_passant_square,

            position_hash: self.position_hash,
            virgin_hash: self.virgin_hash,
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
            royal_list: self.royal_list.clone(),

            piece_count: self.piece_count.clone(),
            piece_list: self.piece_list.clone(),
            piece_in_hand: self.piece_in_hand.clone(),

            scratch: Scratch {
                pawn_table: self.scratch.pawn_table.clone(),                    /* the two parts true of any board    */
                eval_table: self.scratch.eval_table.clone(),
                ..Scratch::default()
            },
        }
    }
}

/*----------------------------------------------------------------------------*\
                       STATE CONSTRUCTION AND PRECOMPUTE
\*----------------------------------------------------------------------------*/

impl State {
    /// State::new
    ///
    /// Makes an empty engine state for a variant. The static tables have
    /// their final size but are zero, and the dynamic fields are empty. The
    /// config loader and `precompute` must fill the tables before play.
    ///
    /// Params:
    /// - title        : String     -> display name of the variant
    /// - startpos     : String     -> FEN of the start position
    /// - files        : u8         -> number of files
    /// - ranks        : u8         -> number of ranks
    /// - pieces       : Vec<Piece> -> piece types, in PieceIndex order
    /// - special_rules: u8         -> special rules mask, see StaticState
    ///
    /// Return:
    /// Self                        -> new state with empty boards and tables
    ///
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

        assert!(
            piece_count < MAX_PIECES,                                           /* the last index is `NO_PIECE`, so a */
            "Variant declares {} piece entries, but this build caps at {}",     /* full field is one entry short of   */
            piece_count, MAX_PIECES - 1                                         /* what the field could name          */
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
            promotion_triggers: 0b11,                                           /* entry and exit, the shogi rule     */
            critical_castling: [board!(files, ranks); 4],

            castling_pieces: vec![false; piece_count],

            files,
            ranks,
            board_size,

            relevant_moves: vec![MoveSet::new(); board_size * piece_count],
            relevant_captures: vec![MoveSet::new(); board_size * piece_count],
            capture_reach: Vec::new(),
            capture_destroys: vec![false; piece_count],
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

            eval: EvalParams {
                shield_pieces: vec![false; piece_count],
                pawn_slots: vec![NO_PAWN; piece_count],
                forward_steps: [1, -1],                                         /* a rank each way before derivation  */
                draw_span: 1,                                                   /* a divisor before derivation runs   */
                ..Default::default()
            },
            search: SearchParams::default(),
        });

        Self::from_statics(statics)
    }

    /// State::from_statics
    ///
    /// Makes the dynamic part of a state for a precomputed configuration,
    /// shared through the `Arc`. All boards and lists have their final size
    /// and are empty. `fork` uses this path, so it does not copy history.
    ///
    /// Params:
    /// - statics: Arc<StaticState> -> precomputed configuration to share
    ///
    /// Return:
    /// State                       -> empty state with that configuration
    ///
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
            en_passant_square: NO_EN_PASSANT,

            position_hash: u128::default(),
            virgin_hash: u128::default(),
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
            royal_list: [Vec::new(), Vec::new()],

            piece_count: vec![0u32; piece_count],
            piece_list: vec![NO_SQUARE; piece_count * board_size],
            piece_in_hand: [vec![0; piece_count], vec![0; piece_count]],

            scratch: Scratch::default(),
        }
    }

    /// State::static_mut
    ///
    /// Gives mutable access to the static configuration during the setup,
    /// which has one thread. The config parser and `precompute` write the
    /// tables then.
    ///
    /// Return:
    /// &mut StaticState -> exclusive reference into the statics `Arc`
    ///
    /// Notes:
    /// It uses `unwrap_unchecked`. No other `Arc` clone can exist yet,
    /// because search threads start only after the setup.
    ///
    #[inline]
    pub fn static_mut(&mut self) -> &mut StaticState {
        unsafe { Arc::get_mut(&mut self.statics).unwrap_unchecked() }
    }

    /// State::reset
    ///
    /// Resets all dynamic fields to an empty board. The static configuration
    /// does not change, so a new game or FEN needs no new derivation.
    ///
    /// Notes:
    /// It also resets the end rule progress and clears the pawn cache. The
    /// pawn cache keeps its size.
    ///
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
        self.en_passant_square = NO_EN_PASSANT;

        self.position_hash = u128::default();
        self.virgin_hash = u128::default();
        self.pawn_hash = u128::default();
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

        self.scratch.pawn_table.table.fill(PTEntry::default());                 /* keep Hash size, clear old answers  */
        self.scratch.eval_table.fill((0, 0));
    }

    /// State::load_fen
    ///
    /// Resets the dynamic state and loads a FEN, with an optional
    /// dictionary. The static configuration does not change.
    ///
    /// Params:
    /// - fen : &str                -> FEN string to load
    /// - dict: Option<&Translator> -> optional notation translator
    ///
    /// Notes:
    /// A bad FEN causes a panic. No caller can use a half-loaded board.
    ///
    #[hotpath::measure]
    pub fn load_fen(&mut self, fen: &str, dict: Option<&Translator>) {
        self.reset();
        parse_fen(self, fen, dict)
            .unwrap_or_else(|error| panic!("{}", error));
    }

    /// State::fork
    ///
    /// Makes a new game from a loaded variant: a new state with the same
    /// `statics`, at the start position, with new evaluation caches. It is
    /// faster than `clone` and `reset`, because it copies no history.
    ///
    /// Return:
    /// State -> a new state at the start position, ready to play
    ///
    /// Notes:
    /// The pawn cache is empty but keeps the size of the template. The start
    /// FEN has no translator, because it is in engine notation.
    ///
    pub fn fork(&self) -> State {
        let mut state = State::from_statics(Arc::clone(&self.statics));
        state.scratch.pawn_table.table.resize(
            self.scratch.pawn_table.len(), PTEntry::default(),
        );
        state.scratch.pawn_table.table.shrink_to_fit();
        state.termination = self.termination.clone();
        state.load_fen(&self.statics.startpos, None);
        refresh_eval_state(&mut state);
        state
    }

    /// State::play_random_opening
    ///
    /// Plays up to `plies` uniform random legal moves. It stops when there
    /// is no legal move. The shared seeded RNG selects the moves, so the
    /// openings of self-play and matches are different.
    ///
    /// Params:
    /// - plies: usize -> number of random plies to play
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

    /// Expression compilation helpers
    ///
    /// Compile the expressions at precompute time. Each takes one expression
    /// for each piece, in config order, and gives one compiled set for each
    /// piece, indexed by `PieceIndex`. The board expansion comes later.
    ///
    /// generate_piece_moves
    ///
    ///   Params:
    ///   - expr_set: &Vec<String> -> one move expression for each piece
    ///
    ///   Return:
    ///   Vec<MoveSet>             -> move sets from `generate_move_set`
    ///
    /// generate_piece_drops
    ///
    ///   Params:
    ///   - expr_set: &[String] -> one drop expression for each piece
    ///
    ///   Return:
    ///   Vec<DropSet>          -> drop sets from `generate_drop_vectors`
    ///
    /// generate_piece_stand_off
    ///
    ///   Params:
    ///   - expr_set: Vec<String> -> one stand-off expression for each piece
    ///
    ///   Return:
    ///   Vec<PatternSet>         -> one pattern for each `|` branch
    ///
    #[hotpath::measure]
    fn generate_piece_moves(&self, expr_set: &Vec<String>) -> Vec<MoveSet> {
        expr_set.par_iter().map(|expr| generate_move_set(expr, self)).collect()
    }

    fn generate_piece_drops(&self, expr_set: &[String]) -> Vec<DropSet> {
        self.statics.pieces.iter().map(
            |piece| generate_drop_vectors(piece, self, expr_set)
        ).collect::<Vec<DropSet>>()
    }

    fn generate_piece_stand_off(
        &self, expr_set: Vec<String>
    ) -> Vec<PatternSet> {
        expr_set.iter().map(|expr| if expr.is_empty() {
            PatternSet::new()
        } else {
            expr.split('|').map(
                |branch| parse_pattern(branch, self)
            ).collect::<PatternSet>()
        }).collect::<Vec<PatternSet>>()
    }

    /// State::populate_relevant
    ///
    /// Fills a table at precompute time. For each (piece, square) pair, it
    /// keeps the compiled entries that stay on the board from that square,
    /// at `piece * board_size + square`. All `generate_relevant_*`
    /// functions have the same signature, so all tables use this function.
    ///
    /// Params:
    ///
    ///     source: &[Vec<T>]
    ///     compiled set for each piece, indexed by PieceIndex
    ///
    ///     generator: fn(&Piece, u32, &State, &[Vec<T>]) -> Vec<T>
    ///     square filter, keeps the entries that stay on the board
    ///
    /// Return:
    ///
    ///     Vec<Vec<T>>
    ///     one entry for each piece and square
    ///
    #[hotpath::measure]
    fn populate_relevant<T: Clone + Send + Sync>(
        &self,
        source: &[Vec<T>],
        generator: fn(&Piece, u32, &State, &[Vec<T>]) -> Vec<T>,
    ) -> Vec<Vec<T>> {
        let board_size = self.statics.board_size;
        let piece_count = self.statics.pieces.len();

        (0..piece_count * board_size).into_par_iter().map(|slot| {
            let piece = &self.statics.pieces[slot / board_size];

            generator(piece, (slot % board_size) as u32, self, source)
        }).collect()
    }

    /// State::precompute
    ///
    /// Makes all runtime tables from the variant expressions: moves,
    /// captures, drops, setup drops, stand-offs and attack masks. It runs
    /// once after the config parse, before the search threads. It skips a
    /// table when its special rule is off.
    ///
    /// 1. compile the expressions, one set for each piece
    /// 2. `populate_relevant` puts the sets on each square
    /// 3. `generate_attack_masks` stores each move under its target square
    /// 4. the capture reach, the squares that the capture legs land on
    ///
    /// The reach lets the capture generation skip a piece that has no enemy
    /// piece in it. It is kept only for a board of at most
    /// `CAPTURE_REACH_WORDS` words, as its size grows with the square of the
    /// board area.
    ///
    /// Params:
    /// - moves_expr_set    : Vec<String> -> move expression of each piece
    /// - drops_expr_set    : Vec<String> -> drop expression of each piece
    /// - setup_expr_set    : Vec<String> -> setup expression of each piece
    /// - stand_off_expr_set: Vec<String> -> stand-off expression of each piece
    ///
    #[hotpath::measure]
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

        let moves =
            self.populate_relevant(&piece_moves, generate_relevant_moves);
        self.static_mut().relevant_moves = moves;

        let captures =
            self.populate_relevant(&piece_moves, generate_relevant_captures);
        self.static_mut().relevant_captures = captures;

        if drops!(self) {
            let drops =
                self.populate_relevant(&piece_drops, generate_relevant_drops);
            self.static_mut().relevant_drops = drops;
        }

        if setup_phase!(self) {
            let setup =
                self.populate_relevant(&piece_setup, generate_relevant_drops);
            self.static_mut().relevant_setup = setup;
        }

        if stand_offs!(self) {
            let stand_offs = self.populate_relevant(
                &piece_stand_off, generate_relevant_stand_offs
            );
            self.static_mut().relevant_stand_offs = stand_offs;
        }

        let attack_writes: Vec<Vec<(usize, usize, AttackMask)>> =
            (0..self.statics.board_size).into_par_iter()
                .map(|square| generate_attack_masks(square as Square, self))
                .collect();

        let statics = self.static_mut();

        for (color, square, mask) in attack_writes.into_iter().flatten() {
            statics.relevant_attacks[color][square].push(mask);
        }

        let files = statics.files as i32;
        let ranks = statics.ranks as i32;
        let board_size = statics.board_size;
        let words = (board_size + 63) >> 6;

        if words > CAPTURE_REACH_WORDS {
            return;
        }

        let mut reach = vec![0u64; piece_count * board_size * words];

        for (entry, vectors) in statics.relevant_captures.iter().enumerate() {
            let piece_index = entry / board_size;
            let piece = &statics.pieces[piece_index];
            let sign = -2 * p_color!(piece) as i32 + 1;
            let origin = (entry % board_size) as i32;

            for vector in vectors {
                let mut file = origin % files;
                let mut rank = origin / files;

                for leg in vector.legs.iter() {
                    statics.capture_destroys[piece_index] |= d!(*leg);
                    file += x!(*leg) as i32 * sign;
                    rank += y!(*leg) as i32 * sign;

                    if file < 0 || file >= files || rank < 0 || rank >= ranks {
                        break;
                    }

                    let square = (rank * files + file) as usize;

                    reach[entry * words + (square >> 6)] |=
                        1u64 << (square & 63);
                }
            }
        }

        statics.capture_reach = reach;
    }
}
