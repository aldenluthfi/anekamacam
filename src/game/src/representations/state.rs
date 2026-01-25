//! # state.rs
//!
//! Defines game state representation and management.
//!
//! This file contains the implementation of game state tracking, including
//! the current position, turn information, move history, and any other
//! state-related data needed for game progression and rule enforcement.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use crate::representations::board::Board;

pub enum Player {
    White,
    Black,
}

pub struct State {
    current_move: Player,

    enemies_board: Board,
    friends_board: Board,
    monarch_board: Board,
    unmoved_board: Board,
}

impl State {
    pub fn new(
        rank: u8,
        files: u8,
    ) -> Self {
        State {
            current_move: Player::White,
            enemies_board: Board::new(rank, files),
            friends_board: Board::new(rank, files),
            monarch_board: Board::new(rank, files),
            unmoved_board: Board::new(rank, files),
        }
    }
}