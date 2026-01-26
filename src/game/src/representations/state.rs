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
    pub title: String,

    pub files: u8,
    pub ranks: u8,

    pub current_move: u8,

    pub pieces: Vec<Piece>,
    pub pieces_board: Vec<Board>,

    pub black_board: Board,
    pub white_board: Board,
    pub monarch_board: Board,
    pub unmoved_board: Board,

    pub castling_state: u8,
    pub halfmove_clock: u8,
    pub en_passant_square: Option<u16>,

    pub position_hash: U256,
    pub history: Vec<MoveState>,

    pub ply: u32,
    pub ply_counter: u32,

    pub big_pieces: [u32; 2],
    pub major_pieces: [u32; 2],
    pub minor_pieces: [u32; 2],

    pub material: [u32; 2]
}

impl State {
    pub fn new(
        title: String,
        files: u8,
        ranks: u8,
        pieces: Vec<Piece>,
    ) -> Self {
        let piece_types = pieces.len();
        State {
            title,
            files,
            ranks,
            pieces,

            current_move: WHITE,
            pieces_board: Vec::with_capacity(piece_types),

            black_board: Board::new(files, ranks),
            white_board: Board::new(files, ranks),
            monarch_board: Board::new(files, ranks),
            unmoved_board: Board::new(files, ranks),

            castling_state: 0,
            halfmove_clock: 0,
            en_passant_square: None,

            position_hash: U256::default(),
            history: Vec::with_capacity(4096),

            ply: 0,
            ply_counter: 0,

            big_pieces: [0; 2],
            major_pieces: [0; 2],
            minor_pieces: [0; 2],

            material: [0; 2],
        }
    }

    pub fn index_to_square(&self, index: u16) -> (u8, u8) {
        let file = (index % self.files as u16) as u8;
        let rank = (index / self.files as u16) as u8;
        assert!(file < self.files, "File {file} out of bounds.");
        assert!(rank < self.ranks, "Rank {rank} out of bounds.");

        (file, rank)
    }

    pub fn square_to_index(&self, file: u8, rank: u8) -> u16 {
        assert!(file < self.files, "File {file} out of bounds.");
        assert!(rank < self.ranks, "Rank {rank} out of bounds.");

        (rank as u16) * (self.files as u16) + (file as u16)
    }

    pub fn reset(&mut self) {
        self.current_move = WHITE;
        self.castling_state = 0;
        self.halfmove_clock = 0;
        self.en_passant_square = None;
        self.position_hash = U256::default();
        self.history.clear();
        self.ply = 0;
        self.ply_counter = 0;

        self.black_board = Board::new(self.files, self.ranks);
        self.white_board = Board::new(self.files, self.ranks);
        self.monarch_board = Board::new(self.files, self.ranks);
        self.unmoved_board = Board::new(self.files, self.ranks);

        self.big_pieces = [0; 2];
        self.major_pieces = [0; 2];
        self.minor_pieces = [0; 2];

        self.material = [0; 2];
    }
}