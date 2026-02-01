//! # hash.rs
//!
//! Implements Zobrist hashing for game positions.
//!
//! This file contains functionality for generating and managing hash values
//! for chess positions using Zobrist hashing. It provides random hash values
//! for pieces on squares, castling rights, en passant squares, and side to move.
//! These hashes enable efficient position comparison and transposition table
//! lookups in game tree search algorithms.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026

use lazy_static::lazy_static;
use std::sync::OnceLock;
use bnum::types::U256;

use crate::{
    constants::*,
    game::representations::state::State,
    game::util::random_u256
};

lazy_static! {
    static ref CASTLING_HASHES: [U256; 16] = {
        let mut hashes = [U256::default(); 16];
        for i in 0..16 {
            hashes[i] = random_u256();
        }
        hashes
    };

    static ref EN_PASSANT_HASHES: [U256; MAX_SQUARES] = {
        let mut hashes = [U256::default(); MAX_SQUARES];
        for i in 0..MAX_SQUARES {
            hashes[i] = random_u256();
        }
        hashes
    };

    static ref SIDE_HASHES: U256 = random_u256();

    static ref PIECE_SQUARE_HASHES: OnceLock<Vec<[[U256; MAX_SQUARES]; 2]>>
        = OnceLock::new();
}

fn init_piece_square_hashes(
    num_piece_types: usize
) -> Vec<[[U256; MAX_SQUARES]; 2]> {
    let mut piece_square_hashes = Vec::with_capacity(num_piece_types);

    for _ in 0..num_piece_types {
        let mut white_hashes = [U256::default(); MAX_SQUARES];
        let mut black_hashes = [U256::default(); MAX_SQUARES];

        for square in 0..MAX_SQUARES {
            white_hashes[square] = random_u256();
            black_hashes[square] = random_u256();
        }

        piece_square_hashes.push([white_hashes, black_hashes]);
    }

    piece_square_hashes
}

/// Computes the Zobrist hash for the given game state.
#[hotpath::measure]
pub fn hash_position(state: &State) -> U256 {
    let piece_hashes = PIECE_SQUARE_HASHES.get_or_init(|| {
        init_piece_square_hashes(state.pieces.len())
    });

    let mut hash = U256::default();

    if state.current_move == WHITE {
        hash ^= &*SIDE_HASHES;
    }

    hash ^= &CASTLING_HASHES[state.castling_state as usize];

    if let Some(ep_square) = state.en_passant_square {
        hash ^= &EN_PASSANT_HASHES[ep_square as usize];
    }

    for (i, piece) in state.pieces.iter().enumerate() {
        let piece_board = &state.pieces_board[i];
        let piece_indices = piece_board.bit_indices();

        for index in piece_indices {
            hash ^= piece_hashes[i][piece.color() as usize][index as usize];
        }
    }

    hash
}