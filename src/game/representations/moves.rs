//! # moves.rs
//!
//! Implements compact move encoding for chess-like games.
//!
//! This file contains structures for efficiently representing moves using
//! bit-packed encoding. It supports multiple move types including single moves
//! (with or without capture), multi-capture sequences, and hopper captures.
//! The encoding uses a 128-bit integer to store move information including
//! source/destination squares, promotion data, and capture details. Additional
//! captured pieces in multi-capture moves are stored separately in MultiMove.
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

/// A structure representing a single move in the game.
///
/// This structure is used for moves without multiple captures (SingleNoCapture,
/// SingleCapture, and HopperCapture).
///
/// The first two bits is used to represent the type of format used:
/// - 00: Single move without capture
/// - 01: Single move with capture or unload
/// - 11: Single Hopper capture move
///
/// The next 12 bits represent the starting square index (0-4095).
/// The following 12 bits represent the ending square index (0-4095).
/// The following bit represents if the move is an initial move (1) or not (0).
/// The following bit represents if the move is a promotion (1) or not (0).
/// The following 8 bits represent the promotion piece type (if applicable).
/// The following 12 bits represents the en passant square (if applicable).
///
/// The next 35 bits are reserved for additional information based on the
/// move type:
///
/// Single move without capture (00):
/// - Next bit represents if the move is a castling move (1) or not (0).
/// - The remaining 34 bits are unused.
///
/// Single move with capture (01):
/// - Next bit indicates if this move is a capture or unload (1) or not (0).
/// - Next bit indicates en passant capture (1) or normal capture (0).
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next 8 bits represent the captured piece type.
/// - Next 12 bits are the unload square index (if applicable).
/// - The remaining 13 bits are unused.
///
/// Single Hopper capture move (11):
/// - Next bit indicates if this move is a capture or unload (1) or not (0).
/// - Next bit indicates en passant capture (1) or normal capture (0).
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next 8 bits represent the captured piece type.
/// - Next 12 bits represent the captured piece square index.
/// - The remainining 12 bits are the unload square index (if applicable).
#[derive(Hash)]
pub struct Move {
    pub encoded_move: u128,
}

/// A structure representing a multi-capture move in the game.
///
/// This structure is used for moves with multiple captures (MultiCapture).
///
/// The first two bits is used to represent the type of format used:
/// - 10: Multi-capture move
///
/// The next 12 bits represent the starting square index (0-4095).
/// The following 12 bits represent the ending square index (0-4095).
/// The following bit represents if the move is an initial move (1) or not (0).
/// The following bit represents if the move is a promotion (1) or not (0).
/// The following 8 bits represent the promotion piece type (if applicable).
/// The following 12 bits represents the en passant square (if applicable).
///
/// Multi-capture move (10):
/// - The remaining 35 bits are unused.
///
/// The taken_pieces field is used to store the indices of all captured
/// pieces.
///
/// Each captured piece is represented in 35 bits:
/// - Next bit indicates if this move is a capture or unload (1) or not (0).
/// - First bit indicates en passant capture (1) or normal capture (0).
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next 8 bits: Piece type
/// - Next 12 bits: Square index
/// - Next 12 bits: Unload square index (if applicable).
#[derive(Hash)]
pub struct MultiMove {
    pub encoded_move: u128,
    pub taken_pieces: Vec<u64>,
}

pub enum MoveType {
    SingleNoCapture(Move),
    SingleCapture(Move),
    MultiCapture(MultiMove),
    HopperCapture(Move),
}

impl Debug for MoveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MoveType::SingleNoCapture(m) => {
                f.debug_tuple("SingleNoCapture").field(m).finish()
            }
            MoveType::SingleCapture(m) => {
                f.debug_tuple("SingleCapture").field(m).finish()
            }
            MoveType::MultiCapture(m) => {
                f.debug_tuple("MultiCapture").field(m).finish()
            }
            MoveType::HopperCapture(m) => {
                f.debug_tuple("HopperCapture").field(m).finish()
            }
        }
    }
}

impl Move {
    const START_SQUARE_SHIFT: u32 = 2;
    const END_SQUARE_SHIFT: u32 = 14;
    const IS_INITIAL_SHIFT: u32 = 26;
    const IS_PROMOTION_SHIFT: u32 = 27;
    const PROMOTION_PIECE_SHIFT: u32 = 28;
    const EN_PASSANT_SQUARE_SHIFT: u32 = 36;
    const EXTRA_DATA_SHIFT: u32 = 48;

    const PROMOTION_PIECE_MASK: u128 = 0xFF << Self::PROMOTION_PIECE_SHIFT;
    const END_SQUARE_MASK: u128 = 0xFFF << Self::END_SQUARE_SHIFT;
    const START_SQUARE_MASK: u128 = 0xFFF << Self::START_SQUARE_SHIFT;
    const EN_PASSANT_SQUARE_MASK: u128 = 0xFFF << Self::EN_PASSANT_SQUARE_SHIFT;
    const MOVE_TYPE_MASK: u128 = 0b11;

    pub const SINGLE_NO_CAPTURE: u128 = 0b00;
    pub const SINGLE_CAPTURE: u128 = 0b01;
    pub const MULTI_CAPTURE: u128 = 0b10;
    pub const HOPPER_CAPTURE: u128 = 0b11;

    pub fn new_single_no_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        en_passant_square: Option<u16>,
        is_castling: bool,
    ) -> Self {
        let mut encoded = Self::SINGLE_NO_CAPTURE;
        encoded |= (start as u128) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Self::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u128) << Self::PROMOTION_PIECE_SHIFT;
        }
        if let Some(ep_square) = en_passant_square {
            encoded |= (ep_square as u128) << Self::EN_PASSANT_SQUARE_SHIFT;
        }
        encoded |= (is_castling as u128) << Self::EXTRA_DATA_SHIFT;

        Self {
            encoded_move: encoded,
        }
    }

    pub fn new_single_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        en_passant_square: Option<u16>,
        captured_piece: u8,
        is_en_passant: bool,
        can_capture_royal: bool,
        is_unload: bool,
        unload_square: Option<u16>,
    ) -> Self {
        let mut encoded = Self::SINGLE_CAPTURE;
        encoded |= (start as u128) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Self::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u128) << Self::PROMOTION_PIECE_SHIFT;
        }
        if let Some(ep_square) = en_passant_square {
            encoded |= (ep_square as u128) << Self::EN_PASSANT_SQUARE_SHIFT;
        }
        encoded |= (is_unload as u128) << Self::EXTRA_DATA_SHIFT;
        encoded |= (is_en_passant as u128) << (Self::EXTRA_DATA_SHIFT + 1);
        encoded |= (can_capture_royal as u128) << (Self::EXTRA_DATA_SHIFT + 2);
        encoded |= (captured_piece as u128) << (Self::EXTRA_DATA_SHIFT + 3);
        if let Some(unload) = unload_square {
            encoded |= (unload as u128) << (Self::EXTRA_DATA_SHIFT + 11);
        }

        Self {
            encoded_move: encoded,
        }
    }

    pub fn new_hopper_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        en_passant_square: Option<u16>,
        captured_piece: u8,
        captured_square: u16,
        is_en_passant: bool,
        can_capture_royal: bool,
        is_unload: bool,
        unload_square: Option<u16>,
    ) -> Self {
        let mut encoded = Self::HOPPER_CAPTURE;
        encoded |= (start as u128) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Self::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u128) << Self::PROMOTION_PIECE_SHIFT;
        }
        if let Some(ep_square) = en_passant_square {
            encoded |= (ep_square as u128) << Self::EN_PASSANT_SQUARE_SHIFT;
        }
        encoded |= (is_unload as u128) << Self::EXTRA_DATA_SHIFT;
        encoded |= (is_en_passant as u128) << (Self::EXTRA_DATA_SHIFT + 1);
        encoded |= (can_capture_royal as u128) << (Self::EXTRA_DATA_SHIFT + 2);
        encoded |= (captured_piece as u128) << (Self::EXTRA_DATA_SHIFT + 3);
        encoded |= (captured_square as u128) << (Self::EXTRA_DATA_SHIFT + 11);
        if let Some(unload) = unload_square {
            encoded |= (unload as u128) << (Self::EXTRA_DATA_SHIFT + 23);
        }

        Self {
            encoded_move: encoded,
        }
    }

    pub fn encode(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        en_passant_square: Option<u16>,
        move_type: MoveType,
        is_castling: Option<bool>,
        captured_piece: Option<u8>,
        is_en_passant: Option<bool>,
        can_capture_royal: Option<bool>,
        captured_square: Option<u16>,
        taken_pieces: Option<Vec<(u8, u16, bool, bool, bool, Option<u16>)>>,
        is_unload: Option<bool>,
        unload_square: Option<u16>,
    ) -> MoveType {
        match move_type {
            MoveType::SingleNoCapture(_) => {
                MoveType::SingleNoCapture(Self::new_single_no_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    en_passant_square,
                    is_castling.unwrap_or(false),
                ))
            }
            MoveType::SingleCapture(_) => {
                MoveType::SingleCapture(Self::new_single_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    en_passant_square,
                    captured_piece.unwrap_or(0),
                    is_en_passant.unwrap_or(false),
                    can_capture_royal.unwrap_or(false),
                    is_unload.unwrap_or(true),
                    unload_square,
                ))
            }
            MoveType::MultiCapture(_) => {
                MoveType::MultiCapture(MultiMove::new_multi_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    en_passant_square,
                    taken_pieces.unwrap_or_else(Vec::new),
                ))
            }
            MoveType::HopperCapture(_) => {
                MoveType::HopperCapture(Self::new_hopper_capture(
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    en_passant_square,
                    captured_piece.unwrap_or(0),
                    captured_square.unwrap_or(0),
                    is_en_passant.unwrap_or(false),
                    can_capture_royal.unwrap_or(false),
                    is_unload.unwrap_or(true),
                    unload_square,
                ))
            }
        }
    }

    pub fn move_type(&self) -> u8 {
        (self.encoded_move & Move::MOVE_TYPE_MASK) as u8
    }

    pub fn start_square(&self) -> u16 {
        (
            (self.encoded_move & Move::START_SQUARE_MASK) >>
            Move::START_SQUARE_SHIFT
        ) as u16
    }

    pub fn end_square(&self) -> u16 {
        (
            (self.encoded_move & Move::END_SQUARE_MASK) >>
            Move::END_SQUARE_SHIFT
        ) as u16
    }

    pub fn is_initial(&self) -> bool {
        (self.encoded_move >> Move::IS_INITIAL_SHIFT) & 1 == 1
    }

    pub fn is_promotion(&self) -> bool {
        (self.encoded_move >> Move::IS_PROMOTION_SHIFT) & 1 == 1
    }

    pub fn promotion_piece(&self) -> Option<u8> {
        if self.is_promotion() {
            Some(
                (
                    (self.encoded_move & Move::PROMOTION_PIECE_MASK) >>
                    Move::PROMOTION_PIECE_SHIFT
                ) as u8
            )
        } else {
            None
        }
    }

    pub fn en_passant_square(&self) -> Option<u16> {
        let square = (
            (self.encoded_move & Move::EN_PASSANT_SQUARE_MASK) >>
            Move::EN_PASSANT_SQUARE_SHIFT
        ) as u16;
        if square == 0 {
            None
        } else {
            Some(square)
        }
    }

    pub fn is_castling(&self) -> bool {
        self.move_type() == Self::SINGLE_NO_CAPTURE as u8
            && (self.encoded_move >> Self::EXTRA_DATA_SHIFT) & 1 == 1
    }

    pub fn is_unload(&self) -> bool {
        let move_type = self.move_type();
        (
            move_type == Self::SINGLE_CAPTURE as u8 ||
            move_type == Self::HOPPER_CAPTURE as u8
        )
            && (self.encoded_move >> Self::EXTRA_DATA_SHIFT) & 1 == 1
    }

    pub fn is_en_passant_capture(&self) -> bool {
        let move_type = self.move_type();
        (
            move_type == Self::SINGLE_CAPTURE as u8 ||
            move_type == Self::HOPPER_CAPTURE as u8
        )
            && (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 1)) & 1 == 1
    }

    pub fn can_capture_royal(&self) -> bool {
        let move_type = self.move_type();
        (
            move_type == Self::SINGLE_CAPTURE as u8 ||
            move_type == Self::HOPPER_CAPTURE as u8
        )
            && (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 2)) & 1 == 1
    }

    pub fn captured_piece_type(&self) -> Option<u8> {
        let move_type = self.move_type();
        if
            move_type == Self::HOPPER_CAPTURE as u8 ||
            move_type == Self::SINGLE_CAPTURE as u8
        {
            Some((self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 3)) as u8)
        } else {
            None
        }
    }

    pub fn unload_square(&self) -> Option<u16> {
        let move_type = self.move_type();
        if move_type == Self::SINGLE_CAPTURE as u8 {
            let square = (
                (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 11)) & 0xFFF
            ) as u16;
            if square == 0 { None } else { Some(square) }
        } else if move_type == Self::HOPPER_CAPTURE as u8 {
            let square = (
                (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 23)) & 0xFFF
            ) as u16;
            if square == 0 { None } else { Some(square) }
        } else {
            None
        }
    }

    pub fn captured_square(&self) -> Option<u16> {
        if self.move_type() == Self::HOPPER_CAPTURE as u8 {
            Some(
                (self.encoded_move >>
                (Self::EXTRA_DATA_SHIFT + 11)) as u16 & 0xFFF
            )
        } else {
            None
        }
    }

    pub fn get_captured_pieces(&self) -> Option<Vec<(u8, u16, bool, bool, bool, Option<u16>)>> {
        None
    }
}

impl MultiMove {
    pub fn new_multi_capture(
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promotion_piece: Option<u8>,
        en_passant_square: Option<u16>,
        captured_pieces: Vec<(u8, u16, bool, bool, bool, Option<u16>)>,
    ) -> Self {
        let mut encoded = Move::MULTI_CAPTURE;
        encoded |= (start as u128) << Move::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Move::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Move::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Move::IS_PROMOTION_SHIFT;
        if let Some(piece) = promotion_piece {
            encoded |= (piece as u128) << Move::PROMOTION_PIECE_SHIFT;
        }
        if let Some(ep_square) = en_passant_square {
            encoded |= (ep_square as u128) << Move::EN_PASSANT_SQUARE_SHIFT;
        }

        let captured = captured_pieces
            .iter()
            .map(|
                    (
                        piece_type, square, is_unload,
                        is_en_passant, can_capture_royal, unload_square
                    )
                | {
                let mut encoded: u64 = 0;
                encoded |= (*is_unload as u64) << 34;
                encoded |= (*is_en_passant as u64) << 33;
                encoded |= (*can_capture_royal as u64) << 32;
                encoded |= (*piece_type as u64) << 24;
                encoded |= (*square as u64) << 12;
                if let Some(unload) = unload_square {
                    encoded |= *unload as u64;
                }
                encoded
            })
            .collect();

        Self {
            encoded_move: encoded,
            taken_pieces: captured,
        }
    }

    pub fn move_type(&self) -> u8 {
        (self.encoded_move & Move::MOVE_TYPE_MASK) as u8
    }

    pub fn start_square(&self) -> u16 {
        (
            (self.encoded_move & Move::START_SQUARE_MASK) >>
            Move::START_SQUARE_SHIFT
        ) as u16
    }

    pub fn end_square(&self) -> u16 {
        (
            (self.encoded_move & Move::END_SQUARE_MASK) >>
            Move::END_SQUARE_SHIFT
        ) as u16
    }

    pub fn is_initial(&self) -> bool {
        (self.encoded_move >> Move::IS_INITIAL_SHIFT) & 1 == 1
    }

    pub fn is_promotion(&self) -> bool {
        (self.encoded_move >> Move::IS_PROMOTION_SHIFT) & 1 == 1
    }

    pub fn promotion_piece(&self) -> Option<u8> {
        if self.is_promotion() {
            Some(
                (
                    (self.encoded_move & Move::PROMOTION_PIECE_MASK) >>
                    Move::PROMOTION_PIECE_SHIFT
                ) as u8
            )
        } else {
            None
        }
    }

    pub fn en_passant_square(&self) -> Option<u16> {
        let square = (
            (self.encoded_move & Move::EN_PASSANT_SQUARE_MASK) >>
            Move::EN_PASSANT_SQUARE_SHIFT
        ) as u16;
        if square == 0 {
            None
        } else {
            Some(square)
        }
    }

    pub fn get_captured_pieces(&self) -> Vec<(u8, u16, bool, bool, bool, Option<u16>)> {
        self.taken_pieces
            .iter()
            .map(|encoded| {
                let is_unload = (encoded >> 34) & 1 == 1;
                let is_en_passant = (encoded >> 33) & 1 == 1;
                let can_capture_royal = (encoded >> 32) & 1 == 1;
                let piece_type = ((encoded >> 24) & 0xFF) as u8;
                let square = ((encoded >> 12) & 0xFFF) as u16;
                let unload_square = if is_unload {
                    Some((encoded & 0xFFF) as u16)
                } else {
                    None
                };

                (
                    piece_type, square, is_en_passant, can_capture_royal,
                    is_unload, unload_square
                )
            })
            .collect()
    }
}

impl Debug for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let move_type = match self.move_type() as u128 {
            Move::SINGLE_NO_CAPTURE => "SingleNoCapture",
            Move::SINGLE_CAPTURE => "SingleCapture",
            Move::MULTI_CAPTURE => "MultiCapture",
            Move::HOPPER_CAPTURE => "HopperCapture",
            _ => "Unknown",
        };

        let start = self.start_square();
        let end = self.end_square();
        let is_initial = self.is_initial();
        let is_promotion = self.is_promotion();
        let promotion_piece = self.promotion_piece();
        let en_passant_square = self.en_passant_square();
        let is_castling = self.is_castling();
        let is_capture_or_unload = self.is_unload();
        let is_en_passant_capture = self.is_en_passant_capture();
        let can_capture_royal = self.can_capture_royal();
        let captured_piece_type = self.captured_piece_type();
        let captured_square = self.captured_square();
        let unload_square = self.unload_square();

        f.debug_struct("Move")
            .field("move_type", &move_type)
            .field("start_square", &start)
            .field("end_square", &end)
            .field("is_initial", &is_initial)
            .field("is_promotion", &is_promotion)
            .field("promotion_piece", &promotion_piece)
            .field("en_passant_square", &en_passant_square)
            .field("is_castling", &is_castling)
            .field("is_capture_or_unload", &is_capture_or_unload)
            .field("is_en_passant_capture", &is_en_passant_capture)
            .field("can_capture_royal", &can_capture_royal)
            .field("captured_piece_type", &captured_piece_type)
            .field("captured_square", &captured_square)
            .field("unload_square", &unload_square)
            .field("encoded_move", &self.encoded_move)
            .finish()
        }
    }

    impl Debug for MultiMove {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let start = self.start_square();
        let end = self.end_square();
        let is_initial = self.is_initial();
        let is_promotion = self.is_promotion();
        let promotion_piece = self.promotion_piece();
        let en_passant_square = self.en_passant_square();
        let captured_pieces = self.get_captured_pieces();

        f.debug_struct("MultiMove")
            .field("move_type", &"MultiCapture")
            .field("start_square", &start)
            .field("end_square", &end)
            .field("is_initial", &is_initial)
            .field("is_promotion", &is_promotion)
            .field("promotion_piece", &promotion_piece)
            .field("en_passant_square", &en_passant_square)
            .field("captured_pieces", &captured_pieces)
            .field("encoded_move", &self.encoded_move)
            .finish()
    }
}