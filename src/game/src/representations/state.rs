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

use crate::constants::*;

use bnum::types::U256;

use crate::representations::{
    board::Board,
    piece::Piece,
    moves::Move,
};

pub struct MoveState {
    pub move_ply: Move,
    pub castling_state: u8,
    pub halfmove_clock: u8,
    pub en_passant_square: Option<u16>,

    pub position_hash: U256
}

pub struct State {
    pub current_move: usize,

    pub pieces: Vec<Piece>,
    pub pieces_board: Vec<Board>,

    pub enemies_board: Board,
    pub friends_board: Board,
    pub monarch_board: Board,
    pub unmoved_board: Board,

    pub castling_state: u8,
    pub halfmove_clock: u8,
    pub en_passant_square: Option<u16>,

    pub position_hash: U256,
    pub history: Vec<MoveState>,
}

impl State {
    pub fn new(
        ranks: u8,
        files: u8,
        pieces: Vec<Piece>,
    ) -> Self {
        let piece_types = pieces.len();
        State {
            current_move: WHITE,

            pieces,
            pieces_board: Vec::with_capacity(piece_types),

            enemies_board: Board::new(ranks, files),
            friends_board: Board::new(ranks, files),
            monarch_board: Board::new(ranks, files),
            unmoved_board: Board::new(ranks, files),

            castling_state: 0,
            halfmove_clock: 0,
            en_passant_square: None,

            position_hash: U256::default(),
            history: Vec::with_capacity(4096),
        }
    }
}