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

use lazy_static::lazy_static;

use crate::{
    constants::*, enp_square, game::{
        representations::state::State,
        util::random_u256
    }
};

lazy_static! {
    pub static ref CASTLING_HASHES: [u128; 16] =
        [random_u256(); 16];

    pub static ref EN_PASSANT_HASHES: [u128; MAX_SQUARES] =
        [random_u256(); MAX_SQUARES];

    pub static ref SIDE_HASHES: u128 = random_u256();

    pub static ref PIECE_HASHES: Vec<[u128; MAX_SQUARES]>
        = {
            let mut result: Vec<[u128; MAX_SQUARES]> =
                Vec::with_capacity(256);

            for _ in 0..256 {
                let piece_hashes = [random_u256(); MAX_SQUARES];
                result.push(piece_hashes);
            }

            result
        };
}

/// Computes the Zobrist hash for the given game state.
pub fn hash_position(state: &State) -> u128 {
    let mut hash = u128::default();

    if state.playing == WHITE {
        hash ^= &*SIDE_HASHES;
    }

    hash ^= &CASTLING_HASHES[state.castling_state as usize];

    if state.en_passant_square != NO_EN_PASSANT {
        hash ^= &EN_PASSANT_HASHES
            [enp_square!(state.en_passant_square) as usize];
    }

    for (index, piece_positions) in state.piece_list.iter().enumerate() {
        for &square in piece_positions {
            hash ^= &PIECE_HASHES[index][square as usize];
        }
    }

    hash
}

#[macro_export]
macro_rules! hash_in_or_out_piece {
    ($game_state:expr, $piece_index:expr, $square_index:expr) => {
        $game_state.position_hash ^= &PIECE_HASHES
            [$piece_index][$square_index as usize];
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
            $game_state.position_hash ^= &CASTLING_HASHES
                [$old_castling_state as usize];
            $game_state.position_hash ^= &CASTLING_HASHES
                [$new_castling_state as usize];
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
