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
    board, constants::*,
    game::{
        moves::{
            move_list::{
                generate_relevant_boards,
                generate_relevant_moves,
            },
            move_parse::generate_move_vectors,
        },
        representations::{
            board::Board,
            moves::Move,
            piece::Piece,
            vector::{Leg, LegVector, MoveSet},
        },
    },
    io::game_io::parse_fen,
    leg,
};

use bnum::types::U2048;

pub type EnPassantSquare = u32;
pub type Square = u16;

#[macro_export]
macro_rules! enp_square {
    ($en_passant:expr) => {
        $en_passant & 0xFFF
    };
}

#[macro_export]
macro_rules! enp_captured {
    ($en_passant:expr) => {
        ($en_passant >> 12) & 0xFFF
    };
}

#[macro_export]
macro_rules! enp_piece {
    ($en_passant:expr) => {
        ($en_passant >> 24) & 0xFF
    };
}

pub struct Snapshot {
    pub move_ply: Move,

    pub castling_state: u8,
    pub halfmove_clock: u8,
    pub en_passant_square: u32,

    pub position_hash: u128
}

impl Default for Snapshot {
    fn default() -> Self {
        Snapshot {
            move_ply: (0u128, vec![]),
            castling_state: 0,
            halfmove_clock: 0,
            en_passant_square: u32::MAX,
            position_hash: u128::default(),
        }
    }
}

pub struct State {
    pub title: String,

    pub files: u8,
    pub ranks: u8,

    pub playing: u8,

    pub pieces: Vec<Piece>,
    pub main_board: Vec<u8>,                                                    /* standard mailbox approach          */

    pub pieces_board: [Board; 2],
    pub virgin_board: Board,

    pub castling_state: u8,                                                     /* 4 bits for representing KQkq       */
    pub halfmove_clock: u8,
    pub en_passant_square: EnPassantSquare,

    pub position_hash: u128,
    pub history: Vec<Snapshot>,

    pub ply: u32,
    pub ply_counter: u32,

    pub big_pieces: [u32; 2],
    pub major_pieces: [u32; 2],
    pub minor_pieces: [u32; 2],
    pub royal_pieces: [u32; 2],

    pub material: [u32; 2],
    pub promotion_ranks: [u8; 2],
    pub piece_count: Vec<u32>,                                                  /* piece index to count               */
    pub piece_list: Vec<Vec<Square>>,                                           /* piece index to square list         */
    pub royal_list: [Vec<Square>; 2],                                           /* color to royal piece square list   */

    pub relevant_board: Vec<Vec<Board>>,
    pub relevant_moves: Vec<Vec<MoveSet>>,
    pub piece_moves: Vec<MoveSet>,
}

impl State {
    pub fn new(
        title: String,
        files: u8,
        ranks: u8,
        pieces: Vec<Piece>,
        promotion_ranks: [u8; 2],
    ) -> Self {

        let piece_count: usize = pieces.len();
        let board_size: usize = (files as usize) * (ranks as usize);

        let mut result = State {
            title,
            files,
            ranks,
            pieces,
            playing: WHITE,
            main_board: vec![NO_PIECE; (files as usize) * (ranks as usize)],
            pieces_board: [board!(files, ranks); 2],
            virgin_board: board!(files, ranks),
            castling_state: 0,
            halfmove_clock: 0,
            en_passant_square: NO_EN_PASSANT,
            position_hash: u128::default(),
            history: Vec::with_capacity(8192),
            ply: 0,
            ply_counter: 0,
            big_pieces: [0; 2],
            major_pieces: [0; 2],
            minor_pieces: [0; 2],
            royal_pieces: [0; 2],
            material: [0; 2],
            promotion_ranks,
            piece_count: vec![0u32; piece_count],
            piece_list: vec![Vec::new(); piece_count],
            royal_list: [Vec::new(), Vec::new()],
            relevant_board:
                vec![vec![board!(files, ranks); board_size]; piece_count],
            relevant_moves:
                vec![vec![MoveSet::new(); board_size]; piece_count],
            piece_moves: vec![MoveSet::new(); piece_count],
        };

        result.precompute();
        result
    }

    pub fn reset(&mut self) {
        let piece_count: usize = self.pieces.len();
        let board_size: usize = (self.files as usize) * (self.ranks as usize);

        self.playing = WHITE;
        self.castling_state = 0;
        self.halfmove_clock = 0;
        self.en_passant_square = NO_EN_PASSANT;
        self.position_hash = u128::default();
        self.history = Vec::with_capacity(8192);
        self.ply = 0;
        self.ply_counter = 0;
        self.main_board = vec![NO_PIECE; board_size];
        self.pieces_board = [board!(self.files, self.ranks); 2];
        self.virgin_board = board!(self.files, self.ranks);
        self.big_pieces = [0; 2];
        self.major_pieces = [0; 2];
        self.minor_pieces = [0; 2];
        self.royal_pieces = [0; 2];
        self.material = [0; 2];
        self.piece_count = vec![0u32; piece_count];
        self.piece_list = vec![Vec::new(); piece_count];
        self.royal_list = [Vec::new(), Vec::new()];
    }

    pub fn load_fen(&mut self, fen: &str) {
        self.reset();
        parse_fen(self, fen);
    }

    fn populate_piece_moves(&mut self) {
        for (index, piece) in self.pieces.iter().enumerate() {
            self.piece_moves[index] =
                generate_move_vectors(&piece.movement, self)
                    .iter()
                    .map(
                        |multi_leg_vector: &Vec<LegVector>| {
                            multi_leg_vector
                                .iter()
                                .map(|leg_vector| leg!(leg_vector))
                                .collect::<Vec<u32>>()
                        }
                    )
                    .collect::<Vec<Vec<u32>>>();
        }
    }

    fn populate_relevant_boards(&mut self) {
        for (index, piece) in self.pieces.iter().enumerate() {
            for square in 0..(self.files as u32 * self.ranks as u32) {
                self.relevant_board[index][square as usize]
                    = generate_relevant_boards(
                        piece,
                        square,
                        self
                    );
            }
        }
    }

    fn populate_relevant_moves(&mut self) {
        for (index, piece) in self.pieces.iter().enumerate() {
            for square in 0..(self.files as u32 * self.ranks as u32) {
                self.relevant_moves[index][square as usize]
                    = generate_relevant_moves(
                        piece,
                        square,
                        self
                    );
            }
        }
    }

    fn precompute(&mut self) {
        self.populate_piece_moves();
        self.populate_relevant_boards();
        self.populate_relevant_moves();
    }
}