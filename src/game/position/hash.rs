//! # hash.rs
//!
//! Implements Zobrist hashing for game positions.
//!
//! This file contains functionality for generating and managing hash values
//! for chess positions using Zobrist hashing. It provides random hash values
//! for pieces on squares, castling rights, en passant squares, and side to
//! move. These hashes enable efficient position comparison and transposition
//! table lookups in game tree search algorithms.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026
use crate::*;

lazy_static! {
    pub static ref CASTLING_HASHES: [u128; 16] =
        array::from_fn(|_| random_u128());
    pub static ref EN_PASSANT_HASHES: [u128; MAX_SQUARES] =
        array::from_fn(|_| random_u128());
    pub static ref SIDE_HASHES: u128 = random_u128();
    pub static ref PIECE_HASHES: Vec<[u128; MAX_SQUARES]> = {
        let mut result: Vec<[u128; MAX_SQUARES]> = Vec::with_capacity(256);

        for _ in 0..256 {
            let piece_hashes = array::from_fn(|_| random_u128());
            result.push(piece_hashes);
        }

        result
    };
    pub static ref IN_HAND_HASHES: Vec<[u128; MAX_SQUARES]> = {
        let mut result: Vec<[u128; MAX_SQUARES]> = Vec::with_capacity(256);

        for _ in 0..256 {
            let drop_hashes = array::from_fn(|_| random_u128());
            result.push(drop_hashes);
        }

        result
    };
}

pub type PositionHash = u128;

/// Computes the Zobrist hash for the given game state.
///
/// The hash includes side to move, castling state, en passant square,
/// on-board piece placement, and in-hand piece counts.
/// It is used for repetition tracking and transposition table indexing.
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
    ($game_state:expr, $piece_index:expr, $square_index:expr) => {
        $game_state.position_hash ^=
            &PIECE_HASHES[$piece_index][$square_index as usize];
    };
}

#[macro_export]
macro_rules! hash_toggle_side {
    ($game_state:expr) => {
        $game_state.position_hash ^= &*SIDE_HASHES;
    };
}

#[macro_export]
macro_rules! hash_update_castling {
    ($game_state:expr, $old_castling_state:expr, $new_castling_state:expr) => {
        if $old_castling_state != $new_castling_state {
            $game_state.position_hash ^=
                &CASTLING_HASHES[$old_castling_state as usize];
            $game_state.position_hash ^=
                &CASTLING_HASHES[$new_castling_state as usize];
        }
    };
}

#[macro_export]
macro_rules! hash_update_en_passant {
    ($game_state:expr, $old_ep_square:expr, $new_ep_square:expr) => {
        if $old_ep_square != $new_ep_square {
            if $old_ep_square != NO_EN_PASSANT {
                let index = enp_square!($old_ep_square) as usize;
                $game_state.position_hash ^= &EN_PASSANT_HASHES[index];
            }

            if $new_ep_square != NO_EN_PASSANT {
                let index = enp_square!($new_ep_square) as usize;
                $game_state.position_hash ^= &EN_PASSANT_HASHES[index];
            }
        }
    };
}

#[macro_export]
macro_rules! hash_update_in_hand {
    ($game_state:expr, $piece_index:expr, $old_count:expr, $new_count:expr) => {
        if $old_count != $new_count {
            $game_state.position_hash ^=
                &IN_HAND_HASHES[$piece_index][$old_count as usize];
            $game_state.position_hash ^=
                &IN_HAND_HASHES[$piece_index][$new_count as usize];
        }
    };
}
