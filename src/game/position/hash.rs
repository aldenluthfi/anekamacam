//! hash.rs
//!
//! Zobrist hashing of game positions.
//!
//! The search must find repeated and transposed positions in O(1). Thus
//! each position has one integer key, and each move updates the key. This
//! file calculates the keys from scratch and defines the update macros.
//! Repetition detection and the hash tables use these keys.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             POSITION KEY BUILDERS
\*----------------------------------------------------------------------------*/

/// PositionHash
///
/// The Zobrist key of a position. With 128 bits, collisions are very rare,
/// also on large boards with many piece types.
///
pub type PositionHash = u128;

/// hash_pawns
///
/// Calculates the key of the pawn placement from scratch. It uses the same
/// random values as the position key. Derivation finds the pawn pieces
/// (`eval.pawn_pieces`) from the rules. A variant without pawns gets 0.
///
/// - `pawn_structure!`    : caches its result under this key
/// - correction history   : keeps its evaluation error under this key
///
/// Params:
/// - state: &State -> position with the pawns to hash
///
/// Return:
/// u128            -> pawn placement Zobrist key
///
/// Notes:
/// A FEN load calls this function. After that, `hash_in_or_out_piece!`
/// updates the key for each move.
///
pub fn hash_pawns(state: &State) -> u128 {
    let mut hash = u128::default();

    for &index in &state.statics.eval.pawn_pieces {
        for &square in piece_squares!(state, index) {
            hash ^= PIECE_HASHES[index][square as usize];
        }
    }

    hash
}

/// hash_position
///
/// Calculates the full Zobrist key of a position from scratch. The key is
/// the XOR of independent random values:
///
/// ```text
/// hash = PIECE_HASHES[piece][sq]      (every occupied square)
///      ^ SIDE_HASHES                  (when white is to move)
///      ^ CASTLING_HASHES[rights]      (current KQkq bits)
///      ^ EN_PASSANT_HASHES[sq]        (when a capture is available)
///      ^ IN_HAND_HASHES[piece][count] (every in-hand pool, both sides)
/// ```
///
/// A second XOR with the same value removes it. Thus a move updates the key
/// with only the values that change.
///
/// Params:
/// - state: &State -> position to hash
///
/// Return:
/// u128            -> full Zobrist key of the position
///
/// Notes:
/// The key includes each hand count, also a count of zero. The update
/// macros do the same. The key uses only the rights bits of the castling
/// byte. The castled marks are for evaluation only.
///
pub fn hash_position(state: &State) -> u128 {
    let mut hash = u128::default();

    if state.playing == WHITE {
        hash ^= &*SIDE_HASHES;
    }

    hash ^= &CASTLING_HASHES[(state.castling_state & CASTLE_RIGHTS) as usize];

    if state.en_passant_square != NO_EN_PASSANT {
        hash ^=
            &EN_PASSANT_HASHES[enp_square!(state.en_passant_square) as usize];
    }

    for index in 0..state.statics.pieces.len() {
        for &square in piece_squares!(state, index) {
            hash ^= &PIECE_HASHES[index][square as usize];
        }
    }

    for color in [WHITE, BLACK] {
        for (index, &count) in
            state.piece_in_hand[color as usize].iter().enumerate()
        {
            hash ^= &IN_HAND_HASHES[index][count as usize];
        }
    }

    hash
}

/// hash_virgin_board
///
/// Calculates the key of the unmoved pieces from scratch. It has one random
/// value for each square with an unmoved piece. The first move of a piece
/// can be different from its later moves, so the search must know it.
///
/// Params:
/// - state: &State -> position to hash
///
/// Return:
/// u128            -> unmoved piece key of the position
///
/// Notes:
/// This key is not in `position_hash`. Thus repetition compares only the
/// board, also when a rook moved for the first time. [`search_key`] adds
/// this key, so the hash tables keep the two positions separate.
///
pub fn hash_virgin_board(state: &State) -> u128 {
    let mut hash = u128::default();

    for square in set_indices!(state.virgin_board) {
        hash ^= &VIRGIN_HASHES[square];
    }

    hash
}

/*----------------------------------------------------------------------------*\
                           SEARCH IDENTITY COMPONENTS
\*----------------------------------------------------------------------------*/

/// Context byte slots
///
/// Row numbers of [`CONTEXT_HASHES`]. Each row keys one byte of search
/// context. The two counting values have 16 bits, so they also use the
/// next row for the high byte. Thus rows 3 and 5 have no name.
///
/// -  0 : `COUNTER_CLOCK`, clock of the counter
/// -  1 : `COUNTER_LIMIT`, limit of the counter
/// -  2 : `COUNTING_COUNT`, low byte
/// -  3 : the same count, high byte
/// -  4 : `COUNTING_LIMIT`, low byte
/// -  5 : the same limit, high byte
/// -  6 : `CHECKS_WHITE`, checks given by White
/// -  7 : `CHECKS_BLACK`, checks given by Black
/// -  8 : `CHECKS_COUNT`, checks necessary to win
/// -  9 : `REPETITION_COUNT`, number of repetitions
/// - 10 : `PASS_CLASS`, pass and stand-off class
/// - 11 : `QSEARCH_CLASS`, quiescence move set class
///
/// `CONTEXT_SLOTS` is the number of rows, not a row.
///
const COUNTER_CLOCK: usize = 0;
const COUNTER_LIMIT: usize = 1;
const COUNTING_COUNT: usize = 2;
const COUNTING_LIMIT: usize = 4;
const CHECKS_WHITE: usize = 6;
const CHECKS_BLACK: usize = 7;
const CHECKS_COUNT: usize = 8;
const REPETITION_COUNT: usize = 9;
const PASS_CLASS: usize = 10;
const QSEARCH_CLASS: usize = 11;
const CONTEXT_SLOTS: usize = 12;

lazy_static! {
    /// CONTEXT_HASHES
    ///
    /// Zobrist rows for the search context, one row of 256 values for each
    /// byte slot. The seeded shared RNG fills them once, at first use.
    ///
    /// Notes:
    /// Only [`search_key`] and [`qsearch_key`] read these rows. Thus the
    /// position key stays a key of the board only.
    ///
    static ref CONTEXT_HASHES: Vec<[u128; 256]> = {
        let mut result: Vec<[u128; 256]> = Vec::with_capacity(CONTEXT_SLOTS);

        for _ in 0..CONTEXT_SLOTS {
            let slot_hashes = array::from_fn(|_| random_u128());
            result.push(slot_hashes);
        }

        result
    };
}

/// wide_context
///
/// Keys a 16-bit context value with two byte rows. The low byte uses
/// `slot` and the high byte uses the next row, so no value is clamped.
///
/// ```text
/// value 0xABCD    slot     reads 0xCD
///                 slot + 1 reads 0xAB
/// ```
///
/// Params:
/// - slot : usize -> first of the two rows
/// - value: u16   -> context value to key
///
/// Return:
/// u128           -> the combined context value
///
/// Notes:
/// With one row, a count and the same count plus 256 would have the same
/// key. The config can give counting limits above 255.
///
fn wide_context(slot: usize, value: u16) -> u128 {
    CONTEXT_HASHES[slot][(value & 0xFF) as usize]
        ^ CONTEXT_HASHES[slot + 1][(value >> 8) as usize]
}

/// eval_key
///
/// Gives the key of the static score cache: all that the evaluation reads
/// and the board does not show.
///
/// ```text
/// key = position_hash            (placement, side, rights, hands)
///     ^ virgin_hash              (which pieces are still unmoved)
///     ^ checks made and required (while an N-check rule is declared)
/// ```
///
/// Params:
/// - state: &State -> position to key
///
/// Return:
/// u128            -> score cache key of the position
///
/// Notes:
/// The score of an N-check variant counts the checks still needed, so two
/// boards with other check counts must not share a score.
///
pub fn eval_key(state: &State) -> u128 {
    let mut key = state.position_hash ^ state.virgin_hash;

    if let Some(checks) = &state.termination.checks {
        let made = checks.delivered;

        key ^= &CONTEXT_HASHES[CHECKS_WHITE][made[WHITE as usize] as usize];
        key ^= &CONTEXT_HASHES[CHECKS_BLACK][made[BLACK as usize] as usize];
        key ^= &CONTEXT_HASHES[CHECKS_COUNT][checks.count as usize];
    }

    key
}

/// search_key
///
/// Gives the hash table key of a node. It is the position key plus each
/// context that changes the legal moves, the game end or the repetition
/// result. The position key does not change, so repetition still compares
/// boards only. The key is this XOR:
///
/// ```text
/// key = eval_key                 (all that the static score reads)
///     ^ counter clock and limit  (while a counter rule is declared)
///     ^ counting count and limit (while a bare-king count runs)
///     ^ repetition occurrences   (while a repetition rule is declared)
///     ^ pass and stand-off bits  (always)
/// ```
///
/// The pass and stand-off bits are the history data that
/// `position_terminal` reads:
///
/// - bit 0 : the last ply was a pass
/// - bit 1 : the ply before it was also a pass
/// - bit 2 : the last pass left a stand-off
///
/// A position without history uses class 0.
///
/// Params:
/// - state  : &State -> position to key
/// - repeats: u8     -> occurrences of this position on the search path
///
/// Return:
/// u128              -> hash table key of the node
///
/// Notes:
/// A context that the variant does not declare is not in the key. The
/// caller gives the repetition count, because it depends on the path to
/// the node, not on the position.
///
pub fn search_key(state: &State, repeats: u8) -> u128 {
    let mut key = eval_key(state);

    if let Some(counter) = &state.termination.counter {
        key ^= &CONTEXT_HASHES[COUNTER_CLOCK][counter.clock as usize];
        key ^= &CONTEXT_HASHES[COUNTER_LIMIT][counter.limit as usize];
    }

    if let Some(counting) = &state.termination.counting
        && let Some((count, limit)) = counting.progress
    {
        key ^= wide_context(COUNTING_COUNT, count);
        key ^= wide_context(COUNTING_LIMIT, limit);
    }

    if state.termination.repetition.is_some() {
        key ^= &CONTEXT_HASHES[REPETITION_COUNT][repeats as usize];
    }

    let pass_class = state.history.last().map_or(0, |last| {
        let passed = pass_snapshot!(last);
        let twice = passed
            && state.history.len() >= 2
            && pass_snapshot!(state.history[state.history.len() - 2]);
        let stand_off = passed && last.in_stand_off == Some(true);

        passed as u8 | (twice as u8) << 1 | (stand_off as u8) << 2
    });

    key ^= &CONTEXT_HASHES[PASS_CLASS][pass_class as usize];

    key
}

/// qsearch_key
///
/// Gives the quiescence table key of a node. It is the search key plus the
/// move set class. In check, quiescence searches all evasions, else only
/// captures. In the endgame, delta pruning is off.
///
/// - bit 0 : the side to move is in check
/// - bit 1 : the position is in the endgame phase
///
/// Params:
/// - state   : &State -> position to key
/// - repeats : u8     -> occurrences of this position on the search path
/// - in_check: bool   -> true when the side to move is in check
///
/// Return:
/// u128               -> quiescence table key of the node
///
pub fn qsearch_key(state: &State, repeats: u8, in_check: bool) -> u128 {
    let endgame = state.game_phase == ENDGAME;
    let class = (in_check as u8 | (endgame as u8) << 1) as usize;

    search_key(state, repeats) ^ &CONTEXT_HASHES[QSEARCH_CLASS][class]
}

/*----------------------------------------------------------------------------*\
                        INCREMENTAL HASH UPDATE HELPERS
\*----------------------------------------------------------------------------*/

/// Incremental hash update macros
///
/// Update the keys for one change, without a full hash of the board. Only
/// `make_move!` uses them. `undo_move!` restores the keys from the snapshot.
/// No macro returns a value.
///
/// - hash_in_or_out_piece!     : position_hash, pawn_hash
/// - hash_toggle_side!         : position_hash
/// - hash_update_castling!     : position_hash
/// - hash_update_en_passant!   : position_hash
/// - hash_update_in_hand!      : position_hash
/// - set_virgin! clear_virgin! : virgin_board, virgin_hash
///
/// hash_in_or_out_piece!
///
///   Adds or removes one piece. A mask, not a branch, updates the pawn
///   key. For a piece that is not a pawn, the mask is zero.
///
///   Params:
///   - state       : &mut State -> position to update
///   - piece_index : usize      -> piece to add or remove
///   - square_index: Square     -> square of the piece
///
/// hash_toggle_side!
///
///   Params:
///   - state: &mut State -> position to update
///
/// hash_update_castling!
///
///   Uses only the `CASTLE_RIGHTS` bits. The castled marks are for
///   evaluation only, so they do not change the key.
///
///   Params:
///   - state             : &mut State -> position to update
///   - old_castling_state: u8         -> castling byte before the move
///   - new_castling_state: u8         -> castling byte after the move
///
/// hash_update_en_passant!
///
///   Params:
///   - state        : &mut State      -> position to update
///   - old_ep_square: EnPassantSquare -> descriptor before the move
///   - new_ep_square: EnPassantSquare -> descriptor after the move
///
/// hash_update_in_hand!
///
///   Params:
///   - state      : &mut State -> position to update
///   - piece_index: usize      -> piece with the changed hand count
///   - old_count  : u16        -> hand count before the change
///   - new_count  : u16        -> hand count after the change
///
/// set_virgin! / clear_virgin!
///
///   Set or clear the unmoved mark of a square and update
///   `state.virgin_hash`. Each macro tests the bit first, so the key
///   stays correct.
///
///   Params:
///   - state       : &mut State -> position to update
///   - square_index: Square     -> square with the unmoved mark
///
#[macro_export]
macro_rules! hash_in_or_out_piece {
    ($state:expr, $piece_index:expr, $square_index:expr) => {{
        let piece_index = $piece_index;
        let piece_hash = PIECE_HASHES[piece_index][$square_index as usize];
        let pawn_mask = (0u128).wrapping_sub(
            ($state.statics.eval.pawn_slots[piece_index] != usize::MAX) as u128
        );

        $state.position_hash ^= piece_hash;
        $state.pawn_hash ^= piece_hash & pawn_mask;
    }};
}

#[macro_export]
macro_rules! hash_toggle_side {
    ($state:expr) => {
        $state.position_hash ^= &*SIDE_HASHES;
    };
}

#[macro_export]
macro_rules! hash_update_castling {
    ($state:expr, $old_castling_state:expr, $new_castling_state:expr) => {{
        let old_rights = $old_castling_state & CASTLE_RIGHTS;
        let new_rights = $new_castling_state & CASTLE_RIGHTS;

        if old_rights != new_rights {
            $state.position_hash ^= &CASTLING_HASHES[old_rights as usize];
            $state.position_hash ^= &CASTLING_HASHES[new_rights as usize];
        }
    }};
}

#[macro_export]
macro_rules! hash_update_en_passant {
    ($state:expr, $old_ep_square:expr, $new_ep_square:expr) => {
        if $old_ep_square != $new_ep_square {
            if $old_ep_square != NO_EN_PASSANT {
                let index = enp_square!($old_ep_square) as usize;
                $state.position_hash ^= &EN_PASSANT_HASHES[index];
            }

            if $new_ep_square != NO_EN_PASSANT {
                let index = enp_square!($new_ep_square) as usize;
                $state.position_hash ^= &EN_PASSANT_HASHES[index];
            }
        }
    };
}

#[macro_export]
macro_rules! hash_update_in_hand {
    ($state:expr, $piece_index:expr, $old_count:expr, $new_count:expr) => {
        if $old_count != $new_count {
            $state.position_hash ^=
                &IN_HAND_HASHES[$piece_index][$old_count as usize];
            $state.position_hash ^=
                &IN_HAND_HASHES[$piece_index][$new_count as usize];
        }
    };
}

#[macro_export]
macro_rules! set_virgin {
    ($state:expr, $square_index:expr) => {
        if !get!($state.virgin_board, $square_index as u32) {
            set!($state.virgin_board, $square_index);
            $state.virgin_hash ^= &VIRGIN_HASHES[$square_index as usize];
        }
    };
}

#[macro_export]
macro_rules! clear_virgin {
    ($state:expr, $square_index:expr) => {
        if get!($state.virgin_board, $square_index as u32) {
            clear!($state.virgin_board, $square_index);
            $state.virgin_hash ^= &VIRGIN_HASHES[$square_index as usize];
        }
    };
}
