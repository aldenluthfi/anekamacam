//! # util.rs
//!
//! Provides utility functions and patterns for move expression processing.
//!
//! This file contains shared utilities used across the move parsing and
//! matching modules, including regex patterns for normalization, range
//! expansion, and cardinal direction parsing. It also provides the core
//! expression evaluation logic using a stack-based algorithm for handling
//! operators (^ for concatenation, | for alternation), Betza atom mappings,
//! and parallel processing helpers for splitting and processing expressions.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use lazy_static::lazy_static;
use rand::{RngCore, SeedableRng};
use std::sync::Mutex;
use bnum::types::U256;

use crate::{
    constants::*,
    game::{hash::{CASTLING_HASHES, EN_PASSANT_HASHES, PIECE_HASHES, SIDE_HASHES}, representations::{
        board::Board,
        state::State
    }},
};

lazy_static!{
    static ref RNG: Mutex<rand::rngs::StdRng> = {
        Mutex::new(rand::rngs::StdRng::seed_from_u64(RNG_SEED))
    };
}

#[hotpath::measure]
pub fn verify_game_state(state: &State) {
    let mut temp_white_board = Board::new(state.files, state.ranks);
    let mut temp_black_board = Board::new(state.files, state.ranks);

    for (i, piece) in state.pieces.iter().enumerate() {
        let piece_board = &state.pieces_board[i];

        if piece.color() == WHITE {
            temp_white_board.or_assign(piece_board);
        } else {
            temp_black_board.or_assign(piece_board);
        }
    }

    assert_eq!(
        &temp_white_board,
        &state.white_board,
        "Computed white board doesn't match state white board"
    );

    assert_eq!(
        &temp_black_board,
        &state.black_board,
        "Computed black board doesn't match state black board"
    );

    let mut temp_pieces_board = Board::new(state.files, state.ranks);

    temp_pieces_board.or_assign(&temp_white_board);
    temp_pieces_board.or_assign(&temp_black_board);

    let mut temp_big_pieces = [0; 2];
    let mut temp_major_pieces = [0; 2];
    let mut temp_minor_pieces = [0; 2];

    for (i, piece) in state.pieces.iter().enumerate() {
        let piece_board = &state.pieces_board[i];
        let piece_indices = piece_board.bit_indices();

        for _index in piece_indices {
            if piece.is_big() {
                temp_big_pieces[piece.color() as usize] += 1;
            }
            if piece.is_major() {
                temp_major_pieces[piece.color() as usize] += 1;
            }
            if piece.is_minor() {
                temp_minor_pieces[piece.color() as usize] += 1;
            }
        }
    }

    assert_eq!(
        temp_big_pieces,
        state.big_pieces,
        "Computed big pieces count doesn't match state big pieces count"
    );

    assert_eq!(
        temp_major_pieces,
        state.major_pieces,
        "Computed major pieces count doesn't match state major pieces count"
    );

    assert_eq!(
        temp_minor_pieces,
        state.minor_pieces,
        "Computed minor pieces count doesn't match state minor pieces count"
    );

    let mut temp_monarch_board = Board::new(state.files, state.ranks);
    let mut temp_white_monarch_board = Board::new(state.files, state.ranks);
    let mut temp_black_monarch_board = Board::new(state.files, state.ranks);

    for (i, piece) in state.pieces.iter().enumerate() {
        if piece.is_royal() {
            let piece_board = &state.pieces_board[i];
            let royal_indices = piece_board.bit_indices();

            for index in royal_indices {

                assert!(
                    state.monarch_board.get_bit(index),
                    "Royal piece not set in monarch board at index {}",
                    index
                );

                if piece.color() == WHITE {
                    temp_white_monarch_board.set_bit(index);
                } else {
                    temp_black_monarch_board.set_bit(index);
                }

                temp_monarch_board.set_bit(index);
            }
        }
    }

    temp_black_monarch_board |= temp_white_monarch_board;

    assert_eq!(
        &temp_black_monarch_board,
        &state.monarch_board,
        "Computed monarch board from colors doesn't match state monarch board"
    );

    assert_eq!(
        &temp_monarch_board,
        &state.monarch_board,
        "Computed monarch board doesn't match state monarch board"
    );

    let mut temp_hash = U256::default();

    if state.playing == WHITE {
        temp_hash ^= &*SIDE_HASHES;
    }

    temp_hash ^= &CASTLING_HASHES[state.castling_state as usize];

    if let Some(ep_square) = state.en_passant_square {
        temp_hash ^= &EN_PASSANT_HASHES[(ep_square & 0xFFF) as usize];
    }

    for piece in &state.pieces {
        let i = piece.index() as usize;
        let color = piece.color() as usize;

        let piece_board = &state.pieces_board[i];
        let piece_indices = piece_board.bit_indices();

        for index in piece_indices {
            temp_hash ^= PIECE_HASHES[i][color][index as usize];
        }
    }

    assert_eq!(
        temp_hash,
        state.position_hash,
        "Computed hash doesn't match state position hash"
    );
}

#[hotpath::measure]
pub fn random_u256() -> U256 {
    let mut rng = RNG.lock().unwrap();
    U256::from(rng.next_u64()) << 192 |
    U256::from(rng.next_u64()) << 128 |
    U256::from(rng.next_u64()) << 64  |
    U256::from(rng.next_u64())
}

pub fn format_square(index: u16, game_state: &State) -> String {
    let file = (index % game_state.files as u16) as u8;
    let rank = (index / game_state.files as u16) as u8;

    if game_state.files <= 26  {
        let file_char = (b'a' + file) as char;
        let rank_char = (b'1' + rank) as char;
        format!("{}{}", file_char, rank_char).trim().to_string()
    } else {
        format!("{:02}{:02}", file, rank).trim().to_string()
    }
}