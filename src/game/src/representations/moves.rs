//! # moves.rs
//!
//! Implements compact move encoding for chess-like games.
//!
//! This file contains a structure for efficiently representing moves using
//! bit-packed encoding. It supports multiple move types including single moves
//! (with or without capture), multi-capture sequences, and hopper captures.
//! The encoding uses a 64-bit integer to store move information including
//! source/destination squares, promotion data, and capture details. Additional
//! captured pieces in multi-capture moves are stored separately.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 26/01/2026

use std::fmt::{
    Debug,
    Formatter,
    Result
};

use crate::util::format_square;

/// A structure representing a move in the game.
///
/// A move is represented with 66 bits at most but compressed with huffman
/// coding so that it fits in a u64 (64 bits).
///
/// The worst case scenario is 66 bits but it is infinitely rare to have that.
///
/// Theres multiple ways to represent a move whether it is a sigle capture,
/// a multi-capture, or a hopper capture.
///
/// The first two bits is used to represent the type of format used:
/// - 00: Single move without capture
/// - 01: Single move with capture
/// - 10: Multi-capture move
/// - 11: Single Hopper capture move
///
/// The next 12 bits represent the starting square index (0-4095).
/// The following 12 bits represent the ending square index (0-4095).
/// The following bit represents if the move is an initial move (1) or not (0).
/// The following bit represents if the move is a promotion (1) or not (0).
/// The following 8 bits represent the promotion piece type (if applicable).
/// The following 12 bits represents the en passant square (if applicable).
///
/// The remaining 20 bits are reserved for additional information based on the
/// move type:
///
/// Single move without capture (00):
/// - Next bit represents if the move is a castling move (1) or not (0).
/// - The remaining 19 bits are unused.
///
/// Single move with capture (01):
/// - Next 8 bits represent the captured piece type.
/// - The remaining 12 bits are unused.
///
/// Multi-capture move (10):
/// - The remaining 20 bits are unused.
///
/// Single Hopper capture move (11):
/// - Next 8 bits represent the captured piece type.
/// - Next 12 bits represent the captured piece square index.
///
/// The captured_pieces field is used to store the indices of all captured
/// pieces.
///
/// Each captured piece is represented in 20 bits:
/// - First 8 bits: Piece type
/// - Next 12 bits: Square index
pub struct Move {
    pub encoded_move: u64,
    pub captured_pieces: Option<Vec<u32>>
}

pub enum MoveType {
    SingleNoCapture { is_castling: bool },
    SingleCapture { captured_piece: u8 },
    MultiCapture { captured_pieces: Vec<(u8, u16)> },
    HopperCapture { captured_piece: u8, captured_square: u16 },
}

impl Move {
    const START_SQUARE_SHIFT: u32 = 2;
    const END_SQUARE_SHIFT: u32 = 14;
    const IS_INITIAL_SHIFT: u32 = 26;
    const IS_PROMOTION_SHIFT: u32 = 27;
    const PROMOTION_PIECE_SHIFT: u32 = 28;
    const EXTRA_DATA_SHIFT: u32 = 36;

    const PROMOTION_PIECE_MASK: u64 = 0xFF << Self::PROMOTION_PIECE_SHIFT;
    const END_SQUARE_MASK: u64 = 0xFFF << Self::END_SQUARE_SHIFT;
    const START_SQUARE_MASK: u64 = 0xFFF << Self::START_SQUARE_SHIFT;
    const MOVE_TYPE_MASK: u64 = 0b11;

    pub const SINGLE_NO_CAPTURE: u64 = 0b00;
    pub const SINGLE_CAPTURE: u64 = 0b01;
    pub const MULTI_CAPTURE: u64 = 0b10;
    pub const HOPPER_CAPTURE: u64 = 0b11;

    pub fn new_single_no_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        is_castling: bool,
    ) -> Self {
        let mut encoded = Self::SINGLE_NO_CAPTURE;
        encoded |= (start as u64) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u64) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u64) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u64) << Self::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u64) << Self::PROMOTION_PIECE_SHIFT;
        }
        encoded |= (is_castling as u64) << Self::EXTRA_DATA_SHIFT;

        Self {
            encoded_move: encoded,
            captured_pieces: None,
        }
    }

    pub fn new_single_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        captured_piece: u8,
    ) -> Self {
        let mut encoded = Self::SINGLE_CAPTURE;
        encoded |= (start as u64) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u64) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u64) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u64) << Self::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u64) << Self::PROMOTION_PIECE_SHIFT;
        }
        encoded |= (captured_piece as u64) << Self::EXTRA_DATA_SHIFT;

        Self {
            encoded_move: encoded,
            captured_pieces: None,
        }
    }

    pub fn new_multi_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        captured_pieces: Vec<(u8, u16)>,
    ) -> Self {
        let mut encoded = Self::MULTI_CAPTURE;
        encoded |= (start as u64) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u64) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u64) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u64) << Self::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u64) << Self::PROMOTION_PIECE_SHIFT;
        }

        let captured = captured_pieces
            .iter()
            .map(|(piece_type, square)| {
                ((*piece_type as u32) << 12) | (*square as u32)
            })
            .collect();

        Self {
            encoded_move: encoded,
            captured_pieces: Some(captured),
        }
    }

    pub fn new_hopper_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        captured_piece: u8,
        captured_square: u16,
    ) -> Self {
        let mut encoded = Self::HOPPER_CAPTURE;
        encoded |= (start as u64) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u64) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u64) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u64) << Self::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u64) << Self::PROMOTION_PIECE_SHIFT;
        }
        encoded |= (captured_piece as u64) << Self::EXTRA_DATA_SHIFT;
        encoded |= (captured_square as u64) << (Self::EXTRA_DATA_SHIFT + 8);

        Self {
            encoded_move: encoded,
            captured_pieces: None,
        }
    }

    pub fn encode(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        move_type: MoveType,
    ) -> Self {
        match move_type {
            MoveType::SingleNoCapture { is_castling } => {
                Self::new_single_no_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    is_castling,
                )
            }
            MoveType::SingleCapture { captured_piece } => {
                Self::new_single_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    captured_piece,
                )
            }
            MoveType::MultiCapture { captured_pieces } => {
                Self::new_multi_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    captured_pieces,
                )
            }
            MoveType::HopperCapture {
                captured_piece,
                captured_square,
            } => {
                Self::new_hopper_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    captured_piece,
                    captured_square,
                )
            }
        }
    }

    pub fn move_type(&self) -> u8 {
        (self.encoded_move & Self::MOVE_TYPE_MASK) as u8
    }

    pub fn start_square(&self) -> u16 {
        (
            (self.encoded_move & Self::START_SQUARE_MASK) >>
            Self::START_SQUARE_SHIFT
        ) as u16
    }

    pub fn end_square(&self) -> u16 {
        (
            (self.encoded_move & Self::END_SQUARE_MASK) >>
            Self::END_SQUARE_SHIFT
        ) as u16
    }

    pub fn is_initial(&self) -> bool {
        (self.encoded_move >> Self::IS_INITIAL_SHIFT) & 1 == 1
    }

    pub fn is_promotion(&self) -> bool {
        (self.encoded_move >> Self::IS_PROMOTION_SHIFT) & 1 == 1
    }

    pub fn promotion_piece(&self) -> Option<u8> {
        if self.is_promotion() {
            Some(
                (
                    (self.encoded_move & Self::PROMOTION_PIECE_MASK) >>
                    Self::PROMOTION_PIECE_SHIFT
                ) as u8
            )
        } else {
            None
        }
    }

    pub fn is_castling(&self) -> bool {
        self.move_type() == Self::SINGLE_NO_CAPTURE as u8
            && (self.encoded_move >> Self::EXTRA_DATA_SHIFT) & 1 == 1
    }

    pub fn captured_piece_type(&self) -> Option<u8> {
        let move_type = self.move_type();
        if
            move_type == Self::HOPPER_CAPTURE as u8 ||
            move_type == Self::SINGLE_CAPTURE as u8
        {
            Some((self.encoded_move >> Self::EXTRA_DATA_SHIFT) as u8)
        } else {
            None
        }
    }

    pub fn captured_square(&self) -> Option<u16> {
        if self.move_type() == Self::HOPPER_CAPTURE as u8 {
            Some(
                (self.encoded_move >>
                (Self::EXTRA_DATA_SHIFT + 8)) as u16 & 0xFFF
            )
        } else {
            None
        }
    }

    pub fn get_captured_pieces(&self) -> Option<Vec<(u8, u16)>> {
        self.captured_pieces.as_ref().map(|pieces| {
            pieces
                .iter()
                .map(|encoded| {
                    let piece_type = (encoded >> 12) as u8;
                    let square = (encoded & 0xFFF) as u16;
                    (piece_type, square)
                })
                .collect()
        })
    }
}

impl Debug for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let move_type = match self.move_type() as u64 {
            Self::SINGLE_NO_CAPTURE => "SingleNoCapture",
            Self::SINGLE_CAPTURE => "SingleCapture",
            Self::MULTI_CAPTURE => "MultiCapture",
            Self::HOPPER_CAPTURE => "HopperCapture",
            _ => "Unknown",
        };

        let start = self.start_square();
        let end = self.end_square();
        let is_initial = self.is_initial();
        let is_promotion = self.is_promotion();
        let promotion_piece = self.promotion_piece();
        let is_castling = self.is_castling();
        let captured_piece_type = self.captured_piece_type();
        let captured_square = self.captured_square();
        let captured_pieces = self.get_captured_pieces();

        f.debug_struct("Move")
            .field("move_type", &move_type)
            .field("start_square", &format_square(start).trim())
            .field("end_square", &format_square(end).trim())
            .field("is_initial", &is_initial)
            .field("is_promotion", &is_promotion)
            .field("promotion_piece", &promotion_piece)
            .field("is_castling", &is_castling)
            .field("captured_piece_type", &captured_piece_type)
            .field(
                "captured_square",
                &{
                    if let Some(square) = captured_square {
                        format_square(square).trim().to_string()
                    } else {
                        "None".to_string()
                    }
                }
            )
            .field("captured_pieces", &captured_pieces)
            .field("encoded_move", &self.encoded_move)
            .finish()
    }
}