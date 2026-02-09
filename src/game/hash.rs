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
use bnum::types::U256;

use crate::{
    constants::*,
    game::{
        representations::state::State,
        util::random_u256
    }
};

lazy_static! {
    pub static ref CASTLING_HASHES: [U256; 16] =
        [random_u256(); 16];

    pub static ref EN_PASSANT_HASHES: [U256; MAX_SQUARES] =
        [random_u256(); MAX_SQUARES];

    pub static ref SIDE_HASHES: U256 = random_u256();

    pub static ref PIECE_HASHES: Vec<[U256; MAX_SQUARES]>
        = {
            let mut result: Vec<[U256; MAX_SQUARES]> =
                Vec::with_capacity(256);

            for _ in 0..256 {
                let piece_hashes = [random_u256(); MAX_SQUARES];
                result.push(piece_hashes);
            }

            result
        };
}

/// Computes the Zobrist hash for the given game state.
#[hotpath::measure]
pub fn hash_position(state: &State) -> U256 {
    let mut hash = U256::default();

    if state.playing == WHITE {
        hash ^= &*SIDE_HASHES;
    }

    hash ^= &CASTLING_HASHES[state.castling_state as usize];

    if state.en_passant_square < MAX_SQUARES as u32 {
        hash ^= &EN_PASSANT_HASHES[(state.en_passant_square & 0xFFF) as usize];
    }

    for (index, piece_positions) in state.piece_list.iter().enumerate() {
        for &square in piece_positions {
            hash ^= &PIECE_HASHES[index][square as usize];
        }
    }

    hash
}

#[inline(always)]
#[hotpath::measure]
pub fn hash_in_or_out_piece(
    game_state: &mut State,
    piece_index: usize,
    square_index: u16
) {
    game_state.position_hash ^=
        &PIECE_HASHES[piece_index][square_index as usize];
}

#[inline(always)]
#[hotpath::measure]
pub fn hash_toggle_side(game_state: &mut State) {
    game_state.position_hash ^= &*SIDE_HASHES;
}

#[inline(always)]
#[hotpath::measure]
pub fn hash_update_castling(
    game_state: &mut State,
    old_castling_state: u8,
    new_castling_state: u8
) {
    if old_castling_state != new_castling_state {
        game_state.position_hash ^=
            &CASTLING_HASHES[old_castling_state as usize];
        game_state.position_hash ^=
            &CASTLING_HASHES[new_castling_state as usize];
    }
}

#[inline(always)]
#[hotpath::measure]
pub fn hash_update_en_passant(
    game_state: &mut State,
    old_ep_square: Option<u32>,
    new_ep_square: Option<u32>
) {
    if old_ep_square != new_ep_square {
        if let Some(old_square) = old_ep_square {
            let index = (old_square & 0xFFF) as usize;
            game_state.position_hash ^= &EN_PASSANT_HASHES[index];
        }

        if let Some(new_square) = new_ep_square {
            let index = (new_square & 0xFFF) as usize;
            game_state.position_hash ^= &EN_PASSANT_HASHES[index];
        }
    }
}
