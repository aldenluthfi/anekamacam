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
use bnum::types::U2048;

use crate::{
    board, constants::*, enp_square, game::{
        hash::{CASTLING_HASHES, EN_PASSANT_HASHES, PIECE_HASHES, SIDE_HASHES},
        representations::state::State
    }, io::board_io::{format_board, format_square}, or, p_color, p_index, p_is_big, p_is_major, p_is_minor, p_is_royal, set
};

lazy_static!{
    static ref RNG: Mutex<rand::rngs::StdRng> = {
        Mutex::new(rand::rngs::StdRng::seed_from_u64(RNG_SEED))
    };
}

pub fn verify_game_state(state: &State) {
    let mut temp_white_board = board!(state.files, state.ranks);
    let mut temp_black_board = board!(state.files, state.ranks);

    for (index, square) in state.main_board.iter().enumerate() {
        if *square != NO_PIECE {
            let piece = &state.pieces[*square as usize];

            if p_color!(piece) == WHITE {
                set!(temp_white_board, index as u32);
            } else {
                set!(temp_black_board, index as u32);
            }
        }
    }

    assert_eq!(
        &temp_white_board,
        &state.pieces_board[WHITE as usize],
        "Computed white board doesn't match state white board\n{}\n{}",
        format_board(&temp_white_board, None),
        format_board(&state.pieces_board[WHITE as usize], None)
    );

    assert_eq!(
        &temp_black_board,
        &state.pieces_board[BLACK as usize],
        "Computed black board doesn't match state black board\n{}\n{}",
        format_board(&temp_black_board, None),
        format_board(&state.pieces_board[BLACK as usize], None)
    );

    let mut temp_pieces_board = board!(state.files, state.ranks);

    or!(temp_pieces_board, &temp_white_board);
    or!(temp_pieces_board, &temp_black_board);

    let mut temp_big_pieces = [0; 2];
    let mut temp_major_pieces = [0; 2];
    let mut temp_minor_pieces = [0; 2];

    for (i, piece) in state.pieces.iter().enumerate() {
        let piece_indices = &state.piece_list[i];

        for _ in piece_indices {
            if p_is_big!(piece) {
                temp_big_pieces[p_color!(piece) as usize] += 1;
            }
            if p_is_major!(piece) {
                temp_major_pieces[p_color!(piece) as usize] += 1;
            }
            if p_is_minor!(piece) {
                temp_minor_pieces[p_color!(piece) as usize] += 1;
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

    let mut temp_royal_list = [Vec::new(), Vec::new()];

    for (index, square) in state.main_board.iter().enumerate() {
        if *square != NO_PIECE && p_is_royal!(state.pieces[*square as usize]) {
            let piece = &state.pieces[*square as usize];

            temp_royal_list[p_color!(piece) as usize].push(index as u16);
        }
    }

    assert_eq!(
        &temp_royal_list,
        &state.royal_list,
        "Computed royal list doesn't match state royal list"
    );

    let mut temp_hash = u128::default();

    if state.playing == WHITE {
        temp_hash ^= &*SIDE_HASHES;
    }

    temp_hash ^= &CASTLING_HASHES[state.castling_state as usize];

    if state.en_passant_square != NO_EN_PASSANT {
        temp_hash ^=
            &EN_PASSANT_HASHES[enp_square!(state.en_passant_square) as usize];
    }

    for piece in &state.pieces {
        let i = p_index!(piece) as usize;

        let piece_indices = &state.piece_list[i];

        for index in piece_indices {
            temp_hash ^= PIECE_HASHES[i][*index as usize];
        }
    }

    if temp_hash != state.position_hash {
        let missing_hash = temp_hash ^ state.position_hash;

        for (piece_idx, positions) in PIECE_HASHES.iter().enumerate() {
            for (pos_idx, &hash) in positions.iter().enumerate() {
                if hash == missing_hash {
                    panic!(
                        concat!(
                            "Hash mismatch! Missing/extra piece at ",
                            "position {} for piece {}"
                        ),
                        format_square(pos_idx as u16, state),
                        state.pieces[piece_idx].name,
                    );
                }
            }
        }

        for (idx, &hash) in CASTLING_HASHES.iter().enumerate() {
            if hash == missing_hash {
                panic!(
                    "Hash mismatch! Castling state mismatch at index {}", idx
                );
            }
        }

        for (idx, &hash) in EN_PASSANT_HASHES.iter().enumerate() {
            if hash == missing_hash {
                panic!(
                    "Hash mismatch! En passant square mismatch at index {}", idx
                );
            }
        }

        if missing_hash == *SIDE_HASHES {
            panic!("Hash mismatch! Side to move mismatch");
        }

        panic!("Hash mismatch! Could not find source of difference");
    }

    assert_eq!(
        temp_hash,
        state.position_hash,
        "Computed hash doesn't match state position hash"
    );
}

pub fn random_u128() -> u128 {
    let mut rng = RNG.lock().unwrap();
    u128::from(rng.next_u64()) << 64  |
    u128::from(rng.next_u64())
}
