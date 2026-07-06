//! # hash.rs
//!
//! Implements Zobrist hashing for game positions.
//!
//! Search must recognise repeated and transposed positions in O(1), so every
//! position has to collapse to a single integer key that updates incrementally
//! as moves are made. This file owns that key: it seeds the random component
//! tables and folds them together, giving repetition detection and the
//! transposition tables a stable position identity.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026
use crate::*;

/// PositionHash
///
/// Full-width Zobrist key of a position. 128 bits keeps the collision
/// probability negligible even on large boards with many piece types,
/// where 64-bit keys would start to saturate.
pub type PositionHash = u128;

/// hash_position
///
/// Computes the Zobrist hash for the given game state.
/// The hash includes side to move, castling state, en passant square,
/// on-board piece placement, and in-hand piece counts.
/// It is used for repetition tracking and transposition table indexing.
///
/// Params:
/// - state: &State -> position to hash from scratch
///
/// Return:
/// u128 -> the position's full Zobrist key
///
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

    for (index, piece_positions) in state.piece_list.iter().enumerate() {
        for &square in piece_positions {
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
/// Computes the pawn-only Zobrist key for the given game state from
/// scratch. The key folds in only the on-board placement of pawn-flagged
/// pieces, so it stays stable across non-pawn moves and lets evaluation
/// cache pawn structure scores between positions.
///
/// Params:
/// - state: &State -> position whose pawns are hashed
///
/// Return:
/// u128 -> the position's pawn-only Zobrist key
///
pub fn hash_pawns(state: &State) -> u128 {
    let mut hash = u128::default();

    for (index, piece_positions) in state.piece_list.iter().enumerate() {
        if !p_is_pawn!(state.statics.pieces[index]) {
            continue;
        }
        for &square in piece_positions {
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
/// during make/undo flow without recomputing from scratch.
///
/// Supported updates:
/// - piece toggles (`hash_in_or_out_piece!`)
/// - side-to-move toggle (`hash_toggle_side!`)
/// - castling rights transitions (`hash_update_castling!`)
/// - en passant transitions (`hash_update_en_passant!`)
/// - in-hand count transitions (`hash_update_in_hand!`)
#[macro_export]
macro_rules! hash_in_or_out_piece {
    ($state:expr, $piece_index:expr, $square_index:expr) => {
        $state.position_hash ^=
            &PIECE_HASHES[$piece_index][$square_index as usize];

        $state.pawn_hash ^=
            p_is_pawn!($state.statics.pieces[$piece_index]) as u128 *
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
