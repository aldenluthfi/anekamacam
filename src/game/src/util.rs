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

use crate::{
    constants::WHITE,
    representations::{
        board::Board,
        state::State
    }
};

pub fn verify_game_state(state: &State) {
    let mut temp_white_board = Board::new(state.files, state.ranks);
    let mut temp_black_board = Board::new(state.files, state.ranks);

    for (i, piece) in state.pieces.iter().enumerate() {
        let piece_board = &state.pieces_board[i];

        if piece.color() == WHITE {
            temp_white_board = &temp_white_board | piece_board;
        } else {
            temp_black_board = &temp_black_board | piece_board;
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

    let mut temp_big_pieces = [0; 2];
    let mut temp_major_pieces = [0; 2];
    let mut temp_minor_pieces = [0; 2];

    for (i, piece) in state.pieces.iter().enumerate() {
        let piece_board = &state.pieces_board[i];
        let piece_indices = piece_board.set_bit_positions();

        for (_file, _rank) in piece_indices {
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
            let royal_indices = piece_board.set_bit_positions();

            for (file, rank) in royal_indices {

                assert!(
                    state.monarch_board.get_bit(file, rank),
                    "Royal piece not set in monarch board at ({}, {})",
                    file,
                    rank
                );

                if piece.color() == WHITE {
                    temp_white_monarch_board.set_bit(file, rank);
                } else {
                    temp_black_monarch_board.set_bit(file, rank);
                }

                temp_monarch_board.set_bit(file, rank);
            }
        }
    }

    assert_eq!(
        &(&temp_white_monarch_board | &temp_black_monarch_board),
        &state.monarch_board,
        "Computed monarch board from colors doesn't match state monarch board"
    );

    assert_eq!(
        &temp_monarch_board,
        &state.monarch_board,
        "Computed monarch board doesn't match state monarch board"
    );
}