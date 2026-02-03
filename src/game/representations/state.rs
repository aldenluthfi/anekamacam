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

use crate::{
    constants::*,
    game::{
        moves::{
            move_list::generate_relevant_boards,
            move_parse::generate_move_vectors
        },
        representations::{
            board::Board, moves::MoveType,
            piece::Piece, vector::MultiLegVector
        }
    },
    io::game_io::{parse_config_file, parse_fen}
};

use bnum::types::U256;


#[derive(Debug)]
pub struct Snapshot {
    pub move_ply: MoveType,
    pub castling_state: u8,
    pub halfmove_clock: u8,
    pub en_passant_square: Option<u32>,

    pub position_hash: U256
}

#[derive(Debug)]
pub struct State {
    pub title: String,

    pub files: u8,
    pub ranks: u8,

    pub playing: u8,

    pub pieces: Vec<Piece>,
    pub pieces_board: Vec<Board>,

    pub black_board: Board,
    pub white_board: Board,
    pub monarch_board: Board,
    pub unmoved_board: Board,

    pub castling_state: u8,                                                     /* 4 bits for representing KQkq       */
    pub halfmove_clock: u8,
    pub en_passant_square: Option<u32>,                                         /* 12 bits for capture square         */
                                                                                /* 12 bits for captured piece square  */
                                                                                /* 8 bits for captured piece index    */
    pub position_hash: U256,
    pub history: Vec<Snapshot>,

    pub ply: u32,
    pub ply_counter: u32,

    pub big_pieces: [u32; 2],
    pub major_pieces: [u32; 2],
    pub minor_pieces: [u32; 2],

    pub material: [u32; 2],

    pub piece_move_vectors: Vec<Vec<MultiLegVector>>,
    pub piece_relevant_boards: Vec<Vec<Board>>,
}

impl State {
    pub fn new(
        title: String,
        files: u8,
        ranks: u8,
        pieces: Vec<Piece>,
    ) -> Self {
        let piece_types = pieces.len();
        let mut result = State {
            title,
            files,
            ranks,
            pieces,

            playing: WHITE,
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

            piece_move_vectors: vec![Vec::new(); piece_types],
            piece_relevant_boards: vec![Vec::new(); piece_types],
        };

        result.precompute_state();

        result
    }

    pub fn from_config(path: &str) -> Self {
        parse_config_file(path)
    }

    pub fn reset(&mut self) {
        self.playing = WHITE;
        self.castling_state = 0;
        self.halfmove_clock = 0;
        self.en_passant_square = None;
        self.position_hash = U256::default();
        self.history.clear();
        self.ply = 0;
        self.ply_counter = 0;

        self.pieces_board.clear();
        self.pieces_board.resize(
            self.pieces.len(), Board::new(self.files, self.ranks)
        );

        self.black_board = Board::new(self.files, self.ranks);
        self.white_board = Board::new(self.files, self.ranks);
        self.monarch_board = Board::new(self.files, self.ranks);
        self.unmoved_board = Board::new(self.files, self.ranks);

        self.big_pieces = [0; 2];
        self.major_pieces = [0; 2];
        self.minor_pieces = [0; 2];

        self.material = [0; 2];
    }

    pub fn load_fen(&mut self, fen: &str) {
        self.reset();
        parse_fen(self, fen);
    }

    fn init_piece_moves(&mut self) {
        for piece in &self.pieces {
            let i = piece.index() as usize;
            self.piece_move_vectors[i] = generate_move_vectors(
                &piece.movement, self
            );
        }
    }

    fn init_relevant_boards(&mut self) {
        let squares = (self.files as u16) * (self.ranks as u16);

        for square in 0..squares {
            for piece in &self.pieces {
                let i = piece.index() as usize;

                let relevant_moves = generate_relevant_boards(
                    piece,
                    square as u32,
                    self
                );

                self.piece_relevant_boards[i].push(relevant_moves);
            }
        }
    }

    fn precompute_state(&mut self) {
        self.init_piece_moves();
        self.init_relevant_boards();
    }
}