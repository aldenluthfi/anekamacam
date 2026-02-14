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
                generate_attack_masks, generate_relevant_moves
            },
            move_parse::generate_move_vectors,
        },
        representations::{
            board::Board,
            moves::{AttackMask, Move},
            piece::Piece,
            vector::{Leg, LegVector, MoveSet},
        },
    },
    io::game_io::parse_fen,
    leg,
};

use bnum::types::U2048;
pub type Square = u16;

/*----------------------------------------------------------------------------*\
                          SPECIAL RULES REPRESENTATIONS
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! castling {
    ($state:expr) => {
        ($state.special_rules & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_castling {
    ($rules:expr) => {
        $rules |= 1;
    };
}

#[macro_export]
macro_rules! en_passant {
    ($state:expr) => {
        ($state.special_rules >> 1 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_en_passant {
    ($rules:expr) => {
        $rules |= 1 << 1;
    };
}

#[macro_export]
macro_rules! promotions {
    ($state:expr) => {
        ($state.special_rules >> 2 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_promotions {
    ($rules:expr) => {
        $rules |= 1 << 2;
    };
}

#[macro_export]
macro_rules! drops {
    ($state:expr) => {
        ($state.special_rules >> 3 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_drops {
    ($rules:expr) => {
        $rules |= 1 << 3;
    };
}

#[macro_export]
macro_rules! count_limit {
    ($state:expr) => {
        ($state.special_rules >> 4 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_count_limit {
    ($rules:expr) => {
        $rules |= 1 << 4;
    };
}

#[macro_export]
macro_rules! forbidden_zones {
    ($state:expr) => {
        ($state.special_rules >> 5 & 1) == 1
    };
}

#[macro_export]
macro_rules! enc_forbidden_zones {
    ($rules:expr) => {
        $rules |= 1 << 5;
    };
}

/*----------------------------------------------------------------------------*\
                            EN PASSANT REPRESENTATION
\*----------------------------------------------------------------------------*/

pub type EnPassantSquare = u32;

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

/*----------------------------------------------------------------------------*\
                              SNAPSHOT REPRESENTATION
\*----------------------------------------------------------------------------*/

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

/*----------------------------------------------------------------------------*\
                            GAME STATE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Main state of the game
///
/// The special rules field is a bitmask representing enabled special rules.
///
/// The bits are defined as follows:
/// - but 0: Castling allowed
/// - bit 1: En passant allowed
/// - bit 2: Promotions allowed
/// - bit 3: Drops allowed
/// - bit 4: Some pieces have a count limit
/// - bit 5: Some pieces have forbidden zones
/// - but 2-31: reserved for future use
///
pub struct State {

/*----------------------------------------------------------------------------*\
                                 STATIC FIELDS
\*----------------------------------------------------------------------------*/

    pub title: String,
    pub pieces: Vec<Piece>,
    pub special_rules: u32,

    pub initial_setup: Vec<Board>,                                              /* piece index to board               */

    pub forbidden_zones: Vec<Board>,                                            /* piece to forbidden zone bitboard   */
    pub promotion_zones_optional: Vec<Board>,                                   /* piece to promotion zone bitboard   */
    pub promotion_zones_mandatory: Vec<Board>,                                  /* piece to promotion zone bitboard   */
    pub piece_count_limit: Vec<u32>,                                            /* piece index to count limit         */

    pub files: u8,
    pub ranks: u8,

    pub relevant_moves: Vec<Vec<MoveSet>>,
    pub relevant_attacks: [Vec<Vec<AttackMask>>; 2],
    pub piece_moves: Vec<MoveSet>,

/*----------------------------------------------------------------------------*\
                                 DYNAMIC FIELDS
\*----------------------------------------------------------------------------*/

    pub playing: u8,
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

    pub material: [u32; 2],
    pub big_pieces: [u32; 2],
    pub major_pieces: [u32; 2],
    pub minor_pieces: [u32; 2],
    pub royal_pieces: [u32; 2],
    pub royal_list: [Vec<Square>; 2],                                           /* color to royal piece square list   */

    pub piece_count: Vec<u32>,                                                  /* piece index to count               */
    pub piece_list: Vec<Vec<Square>>,                                           /* piece index to square list         */
    pub piece_in_hand: [Vec<u8>; 2],                                            /* color to pieces in hand list       */
}

impl State {
    pub fn new(
        title: String,
        files: u8,
        ranks: u8,
        pieces: Vec<Piece>,
        special_rules: u32,
    ) -> Self {

        let piece_count: usize = pieces.len();
        let board_size: usize = (files as usize) * (ranks as usize);

        let mut result = State {
            title,
            pieces,
            special_rules,

            initial_setup: vec![board!(files, ranks); piece_count],

            forbidden_zones: vec![board!(files, ranks); piece_count],
            promotion_zones_optional: vec![board!(files, ranks); piece_count],
            promotion_zones_mandatory: vec![board!(files, ranks); piece_count],
            piece_count_limit: vec![u32::MAX; piece_count],

            files,
            ranks,

            relevant_moves:
                vec![vec![MoveSet::new(); board_size]; piece_count],
            relevant_attacks:
                [vec![Vec::new(); board_size], vec![Vec::new(); board_size]],
            piece_moves: vec![MoveSet::new(); piece_count],

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
            royal_list: [Vec::new(), Vec::new()],

            piece_count: vec![0u32; piece_count],
            piece_list: vec![Vec::new(); piece_count],
            piece_in_hand: [Vec::new(), Vec::new()],
        };

        result.precompute();
        result
    }

    pub fn reset(&mut self) {
        let piece_count: usize = self.pieces.len();
        let board_size: usize = (self.files as usize) * (self.ranks as usize);

        self.playing = WHITE;
        self.main_board = vec![NO_PIECE; board_size];

        self.pieces_board = [board!(self.files, self.ranks); 2];
        self.virgin_board = board!(self.files, self.ranks);

        self.castling_state = 0;
        self.halfmove_clock = 0;
        self.en_passant_square = NO_EN_PASSANT;

        self.position_hash = u128::default();
        self.history = Vec::with_capacity(8192);

        self.ply = 0;
        self.ply_counter = 0;

        self.big_pieces = [0; 2];
        self.major_pieces = [0; 2];
        self.minor_pieces = [0; 2];
        self.royal_pieces = [0; 2];
        self.material = [0; 2];
        self.royal_list = [Vec::new(), Vec::new()];

        self.piece_count = vec![0u32; piece_count];
        self.piece_list = vec![Vec::new(); piece_count];
        self.piece_in_hand = [Vec::new(), Vec::new()];
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

    fn populate_relevant_attacks(&mut self) {
        for square in 0..(self.files as u32 * self.ranks as u32) {
            generate_attack_masks(square as u16, self);
        }
    }

    fn precompute(&mut self) {
        self.populate_piece_moves();
        self.populate_relevant_moves();
        self.populate_relevant_attacks();
    }
}