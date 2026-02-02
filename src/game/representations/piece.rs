//! # piece.rs
//!
//! Defines piece representation and properties.
//!
//! This file contains the implementation of the `Piece` struct, which represents
//! a chess piece type with its movement patterns, display characters for both
//! colors, and special properties such as royal status. The movement string uses
//! Cheesy King Notation to describe how the piece moves on the board.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026

use bnum::{cast::As, types::U2048};

/// A structure representing a game piece with its properties.
///
/// A piece is encoded in 32 bits:
/// - Bits 0-7: Piece index
/// - Bit 8: Color (0 for white, 1 for black)
/// - Bit 9: Royal status (1 if royal, 0 otherwise)
/// - Bit 10: Big piece status (1 if can be promoted to, 0 if can promote)
/// - Bit 11: Major piece status (1 if major, 0 if minor)
/// - Bits 12-27: Piece value
/// - Bits 28-31: Unused
///
///
/// the promotions field is a 2048-bit number representing which pieces this
/// piece can promote to. it consists of:
/// - A byte to represent how many pieces can be promoted to (up to 255 pieces)
/// - the next 255 bytes represents a piece index that this piece can promote to

#[derive(Debug)]
pub struct Piece {
    pub name: String,
    pub movement: String,
    pub symbol: char,
    pub promotions: U2048,

    pub encoded_piece: u32,
}

impl Piece {
    /// Creates a new piece with the specified properties.
    ///
    /// # Arguments
    /// * `name` - The name of the piece
    /// * `movement` - Movement pattern in Cheesy King Notation
    /// * `symbol` - Display character for the piece
    /// * `promotions` - Bitset of pieces this can promote to
    /// * `piece_type` - Index of the piece type (0-255)
    /// * `color` - Color of the piece (0 for white, 1 for black)
    /// * `is_royal` - Whether this is a royal piece
    /// * `is_big` - Whether can be promoted to (true) or can promote (false)
    /// * `is_major` - Whether this is a major piece (true) or minor (false)
    /// * `value` - The piece value (0-65535, stored in 16 bits)
    pub fn new(
        name: String,
        movement: String,
        symbol: char,
        promotions: U2048,
        index: u8,
        color: u8,
        is_royal: bool,
        is_big: bool,
        is_major: bool,
        value: u16,
    ) -> Self {
        let mut encoded_piece = index as u32;
        encoded_piece |= (color as u32) << 8;

        if is_royal {
            encoded_piece |= 1 << 9;
        }

        if is_big {
            encoded_piece |= 1 << 10;
        }

        if is_major {
            encoded_piece |= 1 << 11;
        }

        encoded_piece |= (value as u32 & 0xFFFF) << 12;

        Self {
            name,
            movement,
            symbol,
            promotions,
            encoded_piece,
        }
    }

    pub fn index(&self) -> u8 {
        (self.encoded_piece & 0xFF) as u8
    }

    pub fn color(&self) -> u8 {
        ((self.encoded_piece >> 8) & 1) as u8
    }

    pub fn is_royal(&self) -> bool {
        (self.encoded_piece & (1 << 9)) != 0
    }

    pub fn is_big(&self) -> bool {
        (self.encoded_piece & (1 << 10)) != 0
    }

    pub fn can_promote(&self) -> bool {
        !self.is_big()
    }

    pub fn is_major(&self) -> bool {
        (self.encoded_piece & (1 << 11)) != 0
    }

    pub fn is_minor(&self) -> bool {
        !self.is_major()
    }

    pub fn value(&self) -> u16 {
        ((self.encoded_piece >> 12) & 0xFFFF) as u16
    }

    pub fn get_promotion_pieces(&self) -> Vec<u8> {
        if !self.can_promote() {
            return Vec::new();
        }

        let promotions_count = (
            self.promotions & U2048::from(0xFFu32)
        ).as_::<u8>();
        let mut result = Vec::with_capacity(promotions_count as usize);

        for i in 0..promotions_count {
            let byte_index = i + 1;
            let shift = byte_index * 8;
            let piece_index = (
                (self.promotions >> shift) & U2048::from(0xFFu32)
            ).as_::<u8>();
            result.push(piece_index);
        }

        result
    }
}