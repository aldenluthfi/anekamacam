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

use bnum::types::U256;
use lazy_static::lazy_static;
use rand::{RngCore, SeedableRng};
use std::sync::Mutex;
use std::sync::OnceLock;

use crate::{
    constants::*,
    representations::state::State,
};

fn random_u256() -> U256 {
    let mut rng = RNG.lock().unwrap();
    U256::from(rng.next_u64()) << 192 |
    U256::from(rng.next_u64()) << 128 |
    U256::from(rng.next_u64()) << 64  |
    U256::from(rng.next_u64())
}

lazy_static! {
    static ref RNG: Mutex<rand::rngs::StdRng> = {
        Mutex::new(rand::rngs::StdRng::seed_from_u64(RNG_SEED))
    };

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

pub fn init_piece_square_hashes(
    num_piece_types: usize
) {
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

    let _ = PIECE_SQUARE_HASHES.set(piece_square_hashes);
}

/// Computes the Zobrist hash for the given game state.
pub fn hash_position(state: &State) -> U256 {
    let piece_hashes = PIECE_SQUARE_HASHES.get_or_init(|| {
        let mut piece_square_hashes = Vec::with_capacity(state.pieces.len());

        for _ in 0..state.pieces.len() {
            let mut white_hashes = [U256::default(); MAX_SQUARES];
            let mut black_hashes = [U256::default(); MAX_SQUARES];

            for square in 0..MAX_SQUARES {
                white_hashes[square] = random_u256();
                black_hashes[square] = random_u256();
            }

            piece_square_hashes.push([white_hashes, black_hashes]);
        }

        piece_square_hashes
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
        let piece_indices = piece_board.set_bit_indices();

        for index in piece_indices {
            hash ^= piece_hashes[i][piece.color() as usize][index as usize];
        }
    }

    hash
}