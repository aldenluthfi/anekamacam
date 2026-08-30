//! hash.rs
//!
//! Implements Zobrist hashing for game positions.
//!
//! Search must recognise repeated and transposed positions in O(1), so every
//! position has to collapse to a single integer key that updates incrementally
//! as moves are made. This file owns that key: it seeds the random component
//! tables and folds them together, giving repetition detection and the
//! transposition tables a stable position identity.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi
use crate::*;

/// PositionHash
///
/// Full-width Zobrist key of a position. 128 bits keeps the collision
/// probability negligible even on large boards with many piece types,
/// where 64-bit keys would start to saturate.
pub type PositionHash = u128;

/// hash_position
///
/// Computes the full Zobrist hash for one state from scratch.
///
/// The key includes side to move, special-state fields, on-board placement,
/// and every in-hand piece count. Repetition and transposition tables use it.
///
/// The key is one XOR fold of independent random components:
///
/// ```text
/// hash = PIECE_HASHES[piece][sq]      (every occupied square)
///      ^ SIDE_HASHES                  (when white is to move)
///      ^ CASTLING_HASHES[rights]      (current KQkq bits)
///      ^ EN_PASSANT_HASHES[sq]        (when a capture is available)
///      ^ IN_HAND_HASHES[piece][count] (every in-hand pool, both sides)
/// ```
///
/// XOR is self-inverse, so applying the same component twice removes it.
///
/// Params:
/// - state: &State -> position to hash from scratch
///
/// Return:
/// u128            -> the position's full Zobrist key
pub fn hash_position(state: &State) -> u128 {
    let mut hash = u128::default();

    if state.playing == WHITE {
        hash ^= &*SIDE_HASHES;
    }

    hash ^= &CASTLING_HASHES[state.castling_state as usize];

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
/// Computes the unmoved-piece key for one state from scratch, folding one
/// random component per square whose piece has yet to move. A piece's first
/// move is frequently not its later ones, so two boards alike in placement
/// but apart in unmoved pieces answer different questions.
///
/// Params:
/// - state: &State -> position to hash from scratch
///
/// Return:
/// u128            -> the position's unmoved-piece key
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

/// Context byte slots.
///
/// One row of [`CONTEXT_HASHES`] each. `COUNTING_COUNT` and `COUNTING_LIMIT`
/// carry 16-bit values and own the row after theirs for the high byte.
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
    /// Search-context Zobrist rows.
    ///
    /// One 256-entry row per context byte slot, filled once from the seeded
    /// RNG then read-only. Only the search keys read them, so the canonical
    /// position hash is untouched by everything folded here.
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
/// Folds a 16-bit context value across two byte rows, low byte in `slot` and
/// high byte in the row after it, so the whole range keys exactly and no
/// value has to be clamped away.
///
/// Params:
/// - slot : usize -> first of the two rows the value spends
/// - value: u16   -> context value to fold
///
/// Return:
/// u128           -> the value's combined context component
fn wide_context(slot: usize, value: u16) -> u128 {
    CONTEXT_HASHES[slot][(value & 0xFF) as usize]
        ^ CONTEXT_HASHES[slot + 1][(value >> 8) as usize]
}

/// pass_class
///
/// The pass and stand-off progress a position carries, as the three facts
/// `position_terminal` reads off history: the last ply passed, the ply before
/// it passed, and the last ply left an accepted stand-off. Two boards alike
/// in everything else still end differently when these differ.
///
/// Params:
/// - state: &State -> position to classify
///
/// Return:
/// u8              -> pass and stand-off bits of the position
fn pass_class(state: &State) -> u8 {
    let Some(last) = state.history.last() else {
        return 0;
    };

    let passed = pass_snapshot!(last);
    let twice = passed
        && state.history.len() >= 2
        && pass_snapshot!(state.history[state.history.len() - 2]);
    let stand_off = passed && last.in_stand_off == Some(true);

    passed as u8 | (twice as u8) << 1 | (stand_off as u8) << 2
}

/// search_key
///
/// The transposition identity of a node: the canonical position hash plus
/// every mutable context that changes which moves are legal from here, which
/// terminal rule fires, or how a repetition reads. The canonical hash itself
/// stays untouched, so repetition matching keeps comparing boards while two
/// boards apart in progress never share a table entry.
///
/// The key is one XOR fold of the canonical hash and the live context:
///
/// ```text
/// key = position_hash            (placement, side, rights, hands)
///     ^ virgin_hash              (which pieces are still unmoved)
///     ^ counter clock and limit  (while a counter rule is declared)
///     ^ counting count and limit (while a bare-king count runs)
///     ^ checks made and required (while an N-check rule is declared)
///     ^ repetition occurrences   (while a repetition rule is declared)
///     ^ pass and stand-off bits  (always)
/// ```
///
/// Params:
/// - state  : &State -> position whose search identity is wanted
/// - repeats: u8     -> occurrences of this position on the search path
///
/// Return:
/// u128              -> the node's transposition key
pub fn search_key(state: &State, repeats: u8) -> u128 {
    let mut key = state.position_hash ^ state.virgin_hash;

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

    if let Some(checks) = &state.termination.checks {
        let made = checks.delivered;

        key ^= &CONTEXT_HASHES[CHECKS_WHITE][made[WHITE as usize] as usize];
        key ^= &CONTEXT_HASHES[CHECKS_BLACK][made[BLACK as usize] as usize];
        key ^= &CONTEXT_HASHES[CHECKS_COUNT][checks.count as usize];
    }

    if state.termination.repetition.is_some() {
        key ^= &CONTEXT_HASHES[REPETITION_COUNT][repeats as usize];
    }

    key ^= &CONTEXT_HASHES[PASS_CLASS][pass_class(state) as usize];

    key
}

/// qsearch_key
///
/// The quiescence identity of a node: its search key plus the move-set class
/// the leaf search answers under. A checked node searches every evasion while
/// an unchecked one searches captures alone, and delta pruning stands down
/// once the board thins to an endgame, so the classes must not share entries.
///
/// Params:
/// - state   : &State -> position whose quiescence identity is wanted
/// - repeats : u8     -> occurrences of this position on the search path
/// - in_check: bool   -> whether the side to move stands in check
///
/// Return:
/// u128               -> the node's quiescence table key
pub fn qsearch_key(state: &State, repeats: u8, in_check: bool) -> u128 {
    let endgame = state.game_phase == ENDGAME;
    let class = (in_check as u8 | (endgame as u8) << 1) as usize;

    search_key(state, repeats) ^ &CONTEXT_HASHES[QSEARCH_CLASS][class]
}

/*----------------------------------------------------------------------------*\
                         INCREMENTAL HASH UPDATE HELPERS
\*----------------------------------------------------------------------------*/

/// Incremental Zobrist hash update helpers.
///
/// These macros keep `state.position_hash` in sync with mutable state updates
/// during make/undo flow without recomputing from scratch. None return a
/// value; each XORs its component in or out of the running key.
///
/// hash_in_or_out_piece!
///
///   Params:
///   - state       : &mut State -> position whose key is updated
///   - piece_index : usize      -> piece being placed or removed
///   - square_index: Square     -> square the piece enters or leaves
///
/// hash_toggle_side!
///
///   Params:
///   - state: &mut State -> position whose side-to-move key flips
///
/// hash_update_castling!
///
///   Params:
///   - state             : &mut State -> position whose key is updated
///   - old_castling_state: u8         -> rights bits before the move
///   - new_castling_state: u8         -> rights bits after the move
///
/// hash_update_en_passant!
///
///   Params:
///   - state        : &mut State      -> position whose key is updated
///   - old_ep_square: EnPassantSquare -> descriptor before the move
///   - new_ep_square: EnPassantSquare -> descriptor after the move
///
/// hash_update_in_hand!
///
///   Params:
///   - state      : &mut State -> position whose key is updated
///   - piece_index: usize      -> piece whose pool count changed
///   - old_count  : u16        -> in-hand count before the change
///   - new_count  : u16        -> in-hand count after the change
///
/// set_virgin! / clear_virgin!
///
///   Mark a square as holding an unmoved piece, or stop marking it, keeping
///   `state.virgin_hash` in step. Each checks the bit first, so a call that
///   changes nothing costs nothing and never desynchronises the key.
///
///   Params:
///   - state       : &mut State -> position whose board and key are updated
///   - square_index: Square     -> square whose unmoved mark changes
#[macro_export]
macro_rules! hash_in_or_out_piece {
    ($state:expr, $piece_index:expr, $square_index:expr) => {
        $state.position_hash ^=
            &PIECE_HASHES[$piece_index][$square_index as usize];
    };
}

#[macro_export]
macro_rules! hash_toggle_side {
    ($state:expr) => {
        $state.position_hash ^= &*SIDE_HASHES;
    };
}

#[macro_export]
macro_rules! hash_update_castling {
    ($state:expr, $old_castling_state:expr, $new_castling_state:expr) => {
        if $old_castling_state != $new_castling_state {
            $state.position_hash ^=
                &CASTLING_HASHES[$old_castling_state as usize];
            $state.position_hash ^=
                &CASTLING_HASHES[$new_castling_state as usize];
        }
    };
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
