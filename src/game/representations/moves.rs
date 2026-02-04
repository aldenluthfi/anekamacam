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
/// - 10: Single Hopper capture move
/// - 11: Multi-capture move
///
/// The first 8 bits indicates the piece index of the piece making the move.
/// The next 12 bits represent the starting square index (0-4095).
/// The following 12 bits represent the ending square index (0-4095).
/// The following bit represents if the move is an initial move (1) or not (0).
/// The following bit represents if the move is a promotion (1) or not (0).
/// The following bit represents if the move creates an en passant square (1)
/// or not (0).
/// the following 8 bits represent the promoting piece type (if applicable).
/// The following 8 bits represent the promoted piece type (if applicable).
/// The following 24 bits represents the en passant square (if applicable).
///
/// The next 35 bits are reserved for additional information based on the
/// move type:
///
/// Single move without capture (00):
/// - The remaining 35 bits are unused.
///
/// Single move with capture (01):
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next 8 bits represent the captured piece type.
/// - Next bit indicates if this move is a capture or unload (1) or not (0).
/// - Next 12 bits are the unload square index (if applicable).
/// - Next bit indicates if the piece captured is unmoved (1) or not (0).
/// - The remaining 12 bits are unused.
///
/// Single Hopper capture move (11):
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next 8 bits represent the captured piece type.
/// - Next 12 bits represent the captured piece square index.
/// - Next bit indicates if this move is a capture or unload (1) or not (0).
/// - Next 12 bits are the unload square index (if applicable).
/// - Next bit indicates if the piece captured is unmoved (1) or not (0).
/// - The remaining bit is unused.

#[derive(Clone)]
pub struct Move {
    pub encoded_move: u128,
}

/// A structure representing a multi-capture move in the game.
///
/// This structure is used for moves with multiple captures (MultiCapture).
///
/// The first two bits is used to represent the type of format used:
/// - 11: Multi-capture move
///
/// The first 8 bits indicates the piece index of the piece making the move.
/// The next 12 bits represent the starting square index (0-4095).
/// The following 12 bits represent the ending square index (0-4095).
/// The following bit represents if the move is an initial move (1) or not (0).
/// The following bit represents if the move is a promotion (1) or not (0).
/// The following bit represents if the move creates an en passant square (1)
/// or not (0).
/// the following 8 bits represent the promoting piece type (if applicable).
/// The following 8 bits represent the promoted piece type (if applicable).
/// The following 24 bits represents the en passant square (if applicable).
///
/// Multi-capture move (11):
/// - The remaining 35 bits are unused.
///
/// The taken_pieces field is used to store the indices of all captured
/// pieces.
///
/// Each captured piece is represented in 35 bits:
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next 8 bits: Piece type
/// - Next 12 bits: Square index
/// - Next bit indicates if this move is a capture or unload (1) or not (0).
/// - Next 12 bits: Unload square index (if applicable).
/// - the last bit indicates if the piece captured is unmoved (1) or not (0).

#[derive(Clone)]
pub struct MultiMove {
    pub encoded_move: u128,
    pub taken_pieces: Vec<u64>,
}

#[derive(Clone)]
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

#[hotpath::measure_all]
impl Move {
    const PIECE_INDEX_SHIFT: u32 = 2;
    const START_SQUARE_SHIFT: u32 = 10;
    const END_SQUARE_SHIFT: u32 = 22;
    const IS_INITIAL_SHIFT: u32 = 34;
    const IS_PROMOTION_SHIFT: u32 = 35;
    const CREATES_EN_PASSANT_SHIFT: u32 = 36;
    const PROMOTING_PIECE_SHIFT: u32 = 37;
    const PROMOTED_PIECE_SHIFT: u32 = 45;
    const EN_PASSANT_SHIFT: u32 = 53;
    const EXTRA_DATA_SHIFT: u32 = 77;

    const PIECE_INDEX_MASK: u128 = 0xFF << Self::PIECE_INDEX_SHIFT;
    const PROMOTING_PIECE_MASK: u128 = 0xFF << Self::PROMOTING_PIECE_SHIFT;
    const PROMOTED_PIECE_MASK: u128 = 0xFF << Self::PROMOTED_PIECE_SHIFT;
    const END_SQUARE_MASK: u128 = 0xFFF << Self::END_SQUARE_SHIFT;
    const START_SQUARE_MASK: u128 = 0xFFF << Self::START_SQUARE_SHIFT;
    const EN_PASSANT_SQUARE_MASK: u128 = 0xFFFFFFFF << Self::EN_PASSANT_SHIFT;
    const MOVE_TYPE_MASK: u128 = 0b11;

    pub const SINGLE_NO_CAPTURE: u128 = 0b00;
    pub const SINGLE_CAPTURE: u128 = 0b01;
    pub const HOPPER_CAPTURE: u128 = 0b10;
    pub const MULTI_CAPTURE: u128 = 0b11;

    pub fn new_single_no_capture(
        piece_index: u8,
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promoting_piece: Option<u8>,
        promoted_piece: Option<u8>,
        en_passant_square: Option<u32>,
    ) -> Self {
        let mut encoded = Self::SINGLE_NO_CAPTURE;
        encoded |= (piece_index as u128) << Self::PIECE_INDEX_SHIFT;
        encoded |= (start as u128) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Self::IS_PROMOTION_SHIFT;
        if let Some(ep_square) = en_passant_square {
            encoded |= 1u128 << Self::CREATES_EN_PASSANT_SHIFT;
            encoded |= (ep_square as u128) << Self::EN_PASSANT_SHIFT;
        }
        if let Some(piece) = promoting_piece {
            encoded |= (piece as u128) << Self::PROMOTING_PIECE_SHIFT;
        }
        if let Some(piece) = promoted_piece {
            encoded |= (piece as u128) << Self::PROMOTED_PIECE_SHIFT;
        }

        Self {
            encoded_move: encoded,
        }
    }

    pub fn new_single_capture(
        piece_index: u8,
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promoting_piece: Option<u8>,
        promoted_piece: Option<u8>,
        en_passant_square: Option<u32>,
        captured_piece: u8,
        can_capture_royal: bool,
        is_unload: bool,
        unload_square: Option<u16>,
        captures_unmoved: bool,
    ) -> Self {
        let mut encoded = Self::SINGLE_CAPTURE;
        encoded |= (piece_index as u128) << Self::PIECE_INDEX_SHIFT;
        encoded |= (start as u128) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Self::IS_PROMOTION_SHIFT;
        if let Some(ep_square) = en_passant_square {
            encoded |= 1u128 << Self::CREATES_EN_PASSANT_SHIFT;
            encoded |= (ep_square as u128) << Self::EN_PASSANT_SHIFT;
        }
        if let Some(piece) = promoting_piece {
            encoded |= (piece as u128) << Self::PROMOTING_PIECE_SHIFT;
        }
        if let Some(piece) = promoted_piece {
            encoded |= (piece as u128) << Self::PROMOTED_PIECE_SHIFT;
        }
        encoded |= (can_capture_royal as u128) << Self::EXTRA_DATA_SHIFT;
        encoded |= (captured_piece as u128) << (Self::EXTRA_DATA_SHIFT + 1);
        encoded |= (is_unload as u128) << (Self::EXTRA_DATA_SHIFT + 9);
        if let Some(unload) = unload_square {
            encoded |= (unload as u128) << (Self::EXTRA_DATA_SHIFT + 10);
        }
        encoded |= (captures_unmoved as u128) << (Self::EXTRA_DATA_SHIFT + 22);

        Self {
            encoded_move: encoded,
        }
    }

    pub fn new_hopper_capture(
        piece_index: u8,
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promoting_piece: Option<u8>,
        promoted_piece: Option<u8>,
        en_passant_square: Option<u32>,
        captured_piece: u8,
        captured_square: u16,
        can_capture_royal: bool,
        is_unload: bool,
        unload_square: Option<u16>,
        captures_unmoved: bool,
    ) -> Self {
        let mut encoded = Self::HOPPER_CAPTURE;
        encoded |= (piece_index as u128) << Self::PIECE_INDEX_SHIFT;
        encoded |= (start as u128) << Self::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Self::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Self::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Self::IS_PROMOTION_SHIFT;
        if let Some(ep_square) = en_passant_square {
            encoded |= 1u128 << Self::CREATES_EN_PASSANT_SHIFT;
            encoded |= (ep_square as u128) << Self::EN_PASSANT_SHIFT;
        }
        if let Some(piece) = promoting_piece {
            encoded |= (piece as u128) << Self::PROMOTING_PIECE_SHIFT;
        }
        if let Some(piece) = promoted_piece {
            encoded |= (piece as u128) << Self::PROMOTED_PIECE_SHIFT;
        }
        encoded |= (can_capture_royal as u128) << Self::EXTRA_DATA_SHIFT;
        encoded |= (captured_piece as u128) << (Self::EXTRA_DATA_SHIFT + 1);
        encoded |= (captured_square as u128) << (Self::EXTRA_DATA_SHIFT + 9);
        encoded |= (is_unload as u128) << (Self::EXTRA_DATA_SHIFT + 21);
        if let Some(unload) = unload_square {
            encoded |= (unload as u128) << (Self::EXTRA_DATA_SHIFT + 22);
        }
        encoded |= (captures_unmoved as u128) << (Self::EXTRA_DATA_SHIFT + 34);

        Self {
            encoded_move: encoded,
        }
    }

    pub fn encode(
        piece_index: u8,
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promoting_piece: Option<u8>,
        promoted_piece: Option<u8>,
        en_passant_square: Option<u32>,
        move_type: u128,
        captured_piece: Option<u8>,
        can_capture_royal: Option<bool>,
        captured_square: Option<u16>,
        taken_pieces: Option<Vec<(u8, u16, bool, bool, Option<u16>, bool)>>,
        is_unload: Option<bool>,
        unload_square: Option<u16>,
        captures_unmoved: Option<bool>,
    ) -> MoveType {
        match move_type {
            Self::SINGLE_NO_CAPTURE => {
                MoveType::SingleNoCapture(Self::new_single_no_capture(
                    piece_index,
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promoting_piece,
                    promoted_piece,
                    en_passant_square,
                ))
            }
            Self::SINGLE_CAPTURE => {
                MoveType::SingleCapture(Self::new_single_capture(
                    piece_index,
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promoting_piece,
                    promoted_piece,
                    en_passant_square,
                    captured_piece.unwrap_or(0),
                    can_capture_royal.unwrap_or(false),
                    is_unload.unwrap_or(true),
                    unload_square,
                    captures_unmoved.unwrap_or(false),
                ))
            }
            Self::HOPPER_CAPTURE => {
                MoveType::HopperCapture(Self::new_hopper_capture(
                    piece_index,
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promoting_piece,
                    promoted_piece,
                    en_passant_square,
                    captured_piece.unwrap_or(0),
                    captured_square.unwrap_or(0),
                    can_capture_royal.unwrap_or(false),
                    is_unload.unwrap_or(true),
                    unload_square,
                    captures_unmoved.unwrap_or(false),
                ))
            }
            Self::MULTI_CAPTURE => {
                MoveType::MultiCapture(MultiMove::new_multi_capture(
                    piece_index,
                    start,
                    end,
                    is_initial,
                    is_promotion,
                    promoting_piece,
                    promoted_piece,
                    en_passant_square,
                    taken_pieces.expect(
                        "taken_pieces must be provided for multi-capture moves"
                    ),
                ))
            }
            _ => panic!("Invalid move type: {}", move_type),
        }
    }

    pub fn move_type(&self) -> u8 {
        (self.encoded_move & Move::MOVE_TYPE_MASK) as u8
    }

    pub fn piece_index(&self) -> u8 {
        (
            (self.encoded_move & Move::PIECE_INDEX_MASK) >>
            Move::PIECE_INDEX_SHIFT
        ) as u8
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

    pub fn creates_en_passant_square(&self) -> bool {
        (self.encoded_move >> Move::CREATES_EN_PASSANT_SHIFT) & 1 == 1
    }

    pub fn promoting_piece(&self) -> Option<u8> {
        if self.is_promotion() {
            Some(
                (
                    (self.encoded_move & Move::PROMOTING_PIECE_MASK) >>
                    Move::PROMOTING_PIECE_SHIFT
                ) as u8
            )
        } else {
            None
        }
    }

    pub fn promoted_piece(&self) -> Option<u8> {
        if self.is_promotion() {
            Some(
                (
                    (self.encoded_move & Move::PROMOTED_PIECE_MASK) >>
                    Move::PROMOTED_PIECE_SHIFT
                ) as u8
            )
        } else {
            None
        }
    }

    pub fn en_passant_square(&self) -> Option<u32> {
        if self.creates_en_passant_square() {
            Some(
                (
                    (self.encoded_move & Move::EN_PASSANT_SQUARE_MASK) >>
                    Move::EN_PASSANT_SHIFT
                ) as u32
            )
        } else {
            None
        }
    }

    pub fn is_castling(&self) -> bool {
        self.move_type() == Self::SINGLE_NO_CAPTURE as u8
            && (self.encoded_move >> Self::EXTRA_DATA_SHIFT) & 1 == 1
    }

    pub fn is_unload(&self) -> bool {
        let move_type = self.move_type();
        if move_type == Self::SINGLE_CAPTURE as u8 {
            (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 9)) & 1 == 1
        } else if move_type == Self::HOPPER_CAPTURE as u8 {
            (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 21)) & 1 == 1
        } else {
            false
        }
    }

    pub fn can_capture_royal(&self) -> bool {
        let move_type = self.move_type();
        (
            move_type == Self::SINGLE_CAPTURE as u8 ||
            move_type == Self::HOPPER_CAPTURE as u8
        )
            && (self.encoded_move >> Self::EXTRA_DATA_SHIFT) & 1 == 1
    }

    pub fn captured_piece(&self) -> Option<u8> {
        let move_type = self.move_type();
        if
            move_type == Self::HOPPER_CAPTURE as u8 ||
            move_type == Self::SINGLE_CAPTURE as u8
        {
            Some((self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 1)) as u8)
        } else {
            None
        }
    }

    pub fn set_captured_piece(&mut self, piece: u8) {
        let move_type = self.move_type();
        if
            move_type == Self::HOPPER_CAPTURE as u8 ||
            move_type == Self::SINGLE_CAPTURE as u8
        {
            self.encoded_move &=
                !(0xFFu128 << (Self::EXTRA_DATA_SHIFT + 1));
            self.encoded_move |=
                (piece as u128) << (Self::EXTRA_DATA_SHIFT + 1);
        }
    }

    pub fn unload_square(&self) -> Option<u16> {
        let move_type = self.move_type();
        if move_type == Self::SINGLE_CAPTURE as u8 && self.is_unload() {
            let square = (
                (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 10)) & 0xFFF
            ) as u16;
            if square == 0 { None } else { Some(square) }
        } else if move_type == Self::HOPPER_CAPTURE as u8 && self.is_unload() {
            let square = (
                (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 22)) & 0xFFF
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
                (Self::EXTRA_DATA_SHIFT + 9)) as u16 & 0xFFF
            )
        } else {
            None
        }
    }

    pub fn is_captured_piece_unmoved(&self) -> bool {
        let move_type = self.move_type();
        if move_type == Self::SINGLE_CAPTURE as u8 {
            (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 22)) & 1 == 1
        } else if move_type == Self::HOPPER_CAPTURE as u8 {
            (self.encoded_move >> (Self::EXTRA_DATA_SHIFT + 34)) & 1 == 1
        } else {
            false
        }
    }
}

#[hotpath::measure_all]
impl MultiMove {
    pub fn new_multi_capture(
        piece_index: u8,
        start: u16,
        end: u16,
        is_initial: bool,
        is_promotion: bool,
        promoting_piece: Option<u8>,
        promoted_piece: Option<u8>,
        en_passant_square: Option<u32>,
        captured_pieces: Vec<(u8, u16, bool, bool, Option<u16>, bool)>,
    ) -> Self {
        let mut encoded = Move::MULTI_CAPTURE;
        encoded |= (piece_index as u128) << Move::PIECE_INDEX_SHIFT;
        encoded |= (start as u128) << Move::START_SQUARE_SHIFT;
        encoded |= (end as u128) << Move::END_SQUARE_SHIFT;
        encoded |= (is_initial as u128) << Move::IS_INITIAL_SHIFT;
        encoded |= (is_promotion as u128) << Move::IS_PROMOTION_SHIFT;
        if let Some(ep_square) = en_passant_square {
            encoded |= 1u128 << Move::CREATES_EN_PASSANT_SHIFT;
            encoded |= (ep_square as u128) << Move::EN_PASSANT_SHIFT;
        }
        if let Some(piece) = promoting_piece {
            encoded |= (piece as u128) << Move::PROMOTING_PIECE_SHIFT;
        }
        if let Some(piece) = promoted_piece {
            encoded |= (piece as u128) << Move::PROMOTED_PIECE_SHIFT;
        }

        let captured = captured_pieces
            .iter()
            .map(|
                    (
                        piece_type, square, can_capture_royal,
                        is_unload, unload_square, captures_unmoved
                    )
                | {
                let mut encoded: u64 = 0;
                encoded |= (*can_capture_royal as u64) << 34;
                encoded |= (*piece_type as u64) << 26;
                encoded |= (*square as u64) << 14;
                encoded |= (*is_unload as u64) << 13;
                if let Some(unload) = unload_square {
                    encoded |= (*unload as u64) << 1;
                }
                encoded |= *captures_unmoved as u64;
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

    pub fn piece_index(&self) -> u8 {
        (
            (self.encoded_move & Move::PIECE_INDEX_MASK) >>
            Move::PIECE_INDEX_SHIFT
        ) as u8
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

    pub fn creates_en_passant_square(&self) -> bool {
        (self.encoded_move >> Move::CREATES_EN_PASSANT_SHIFT) & 1 == 1
    }

    pub fn promoting_piece(&self) -> Option<u8> {
        if self.is_promotion() {
            Some(
                (
                    (self.encoded_move & Move::PROMOTING_PIECE_MASK) >>
                    Move::PROMOTING_PIECE_SHIFT
                ) as u8
            )
        } else {
            None
        }
    }

    pub fn promoted_piece(&self) -> Option<u8> {
        if self.is_promotion() {
            Some(
                (
                    (self.encoded_move & Move::PROMOTED_PIECE_MASK) >>
                    Move::PROMOTED_PIECE_SHIFT
                ) as u8
            )
        } else {
            None
        }
    }

    pub fn en_passant_square(&self) -> Option<u32> {
        if self.creates_en_passant_square() {
            Some((
                (self.encoded_move & Move::EN_PASSANT_SQUARE_MASK) >>
                Move::EN_PASSANT_SHIFT
            ) as u32)
        } else {
            None
        }
    }

    pub fn get_taken_pieces(
        &self
    ) -> Vec<(u8, u16, bool, bool, Option<u16>, bool)> {
        self.taken_pieces
            .iter()
            .map(|encoded| {
                let can_capture_royal = (encoded >> 34) & 1 == 1;
                let piece_type = ((encoded >> 26) & 0xFF) as u8;
                let square = ((encoded >> 14) & 0xFFF) as u16;
                let is_unload = (encoded >> 13) & 1 == 1;
                let unload_square = if is_unload {
                    Some(((encoded >> 1) & 0xFFF) as u16)
                } else {
                    None
                };
                let captures_unmoved = encoded & 1 == 1;

                (
                    piece_type, square, can_capture_royal,
                    is_unload, unload_square, captures_unmoved
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
            Move::HOPPER_CAPTURE => "HopperCapture",
            Move::MULTI_CAPTURE => "MultiCapture",
            _ => "Unknown",
        };

        let piece_index = self.piece_index();
        let start = self.start_square();
        let end = self.end_square();
        let is_initial = self.is_initial();
        let is_promotion = self.is_promotion();
        let promoting_piece = self.promoting_piece();
        let promoted_piece = self.promoted_piece();
        let en_passant_square = self.en_passant_square();
        let creates_en_passant = self.creates_en_passant_square();
        let is_castling = self.is_castling();
        let is_capture_or_unload = self.is_unload();
        let can_capture_royal = self.can_capture_royal();
        let captured_piece_type = self.captured_piece();
        let captured_square = self.captured_square();
        let unload_square = self.unload_square();
        let captures_unmoved = self.is_captured_piece_unmoved();

        f.debug_struct("Move")
            .field("move_type", &move_type)
            .field("piece_index", &piece_index)
            .field("start_square", &start)
            .field("end_square", &end)
            .field("is_initial", &is_initial)
            .field("is_promotion", &is_promotion)
            .field("promoting_piece", &promoting_piece)
            .field("promoted_piece", &promoted_piece)
            .field("creates_en_passant_square", &creates_en_passant)
            .field("en_passant_square", &en_passant_square)
            .field("is_castling", &is_castling)
            .field("is_capture_or_unload", &is_capture_or_unload)
            .field("can_capture_royal", &can_capture_royal)
            .field("captured_piece_type", &captured_piece_type)
            .field("captured_square", &captured_square)
            .field("unload_square", &unload_square)
            .field("captures_unmoved", &captures_unmoved)
            .field("encoded_move", &self.encoded_move)
            .finish()
        }
    }

    impl Debug for MultiMove {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let piece_index = self.piece_index();
        let start = self.start_square();
        let end = self.end_square();
        let is_initial = self.is_initial();
        let is_promotion = self.is_promotion();
        let promoting_piece = self.promoting_piece();
        let promoted_piece = self.promoted_piece();
        let en_passant_square = self.en_passant_square();
        let creates_en_passant = self.creates_en_passant_square();
        let captured_pieces = self.get_taken_pieces();

        f.debug_struct("MultiMove")
            .field("move_type", &"MultiCapture")
            .field("piece_index", &piece_index)
            .field("start_square", &start)
            .field("end_square", &end)
            .field("is_initial", &is_initial)
            .field("is_promotion", &is_promotion)
            .field("promoting_piece", &promoting_piece)
            .field("promoted_piece", &promoted_piece)
            .field("creates_en_passant_square", &creates_en_passant)
            .field("en_passant_square", &en_passant_square)
            .field("captured_pieces", &captured_pieces)
            .field("encoded_move", &self.encoded_move)
            .finish()
    }
}