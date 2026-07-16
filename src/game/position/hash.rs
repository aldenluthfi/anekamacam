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

/// hash_pawns
///
/// Computes the pawn-like-piece Zobrist key for one state from scratch.
///
/// The key ignores all non-pawn-like pieces, allowing evaluation to cache
/// pawn-structure scores across positions with unchanged pawn placement.
///
/// Params:
/// - state: &State -> position whose pawns are hashed
///
/// Return:
/// u128            -> the position's pawn-only Zobrist key
pub fn hash_pawns(state: &State) -> u128 {
    let mut hash = u128::default();

    for index in 0..state.statics.pieces.len() {
        if !p_is_pawn!(state.statics.pieces[index]) {
            continue;
        }
        for &square in piece_squares!(state, index) {
            hash ^= &PIECE_HASHES[index][square as usize];
        }
    }

    hash
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
/// `hash_in_or_out_piece!` also keeps the per-color `pawn_board` bitboards
/// in sync: every pawn placement or removal flows through this macro during
/// move execution, so toggling the square bit alongside the pawn hash keeps
/// the board exact under the same pairing that keeps the hash exact.
///
/// hash_in_or_out_piece!
///
///   Params:
///   - state       : &mut State -> position whose keys are updated
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
#[macro_export]
macro_rules! hash_in_or_out_piece {
    ($state:expr, $piece_index:expr, $square_index:expr) => {
        $state.position_hash ^=
            &PIECE_HASHES[$piece_index][$square_index as usize];

        $state.pawn_hash ^=
            p_is_pawn!($state.statics.pieces[$piece_index]) as u128 *
            &PIECE_HASHES[$piece_index][$square_index as usize];

        if p_is_pawn!($state.statics.pieces[$piece_index]) {
            let pawn_color =
                p_color!($state.statics.pieces[$piece_index]) as usize;

            toggle!($state.pawn_board[pawn_color], $square_index as u32);
        }
    };
}

/// pawn_board_in_or_out!
///
/// Toggles one square of the per-color `pawn_board` for a pawn-flagged
/// piece, and is a no-op for every other piece. `undo_move!` restores the
/// hashes wholesale from the snapshot instead of re-toggling them, so it
/// calls this at each board-placement reversal to give `pawn_board` the
/// exact inverse of the toggles `hash_in_or_out_piece!` applied on make.
///
/// Params:
/// - state       : &mut State -> position whose pawn board is updated
/// - piece_index : usize      -> piece being placed or removed
/// - square_index: Square     -> square whose pawn bit is toggled
#[macro_export]
macro_rules! pawn_board_in_or_out {
    ($state:expr, $piece_index:expr, $square_index:expr) => {
        if p_is_pawn!($state.statics.pieces[$piece_index]) {
            let pawn_color =
                p_color!($state.statics.pieces[$piece_index]) as usize;

            toggle!($state.pawn_board[pawn_color], $square_index as u32);
        }
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
