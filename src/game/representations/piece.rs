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

use std::fmt::Debug;

#[macro_export]
macro_rules! p_index {
    ($piece:expr) => {
        $piece.encoded_piece as u8
    };
}

#[macro_export]
macro_rules! p_color {
    ($piece:expr) => {
        (($piece.encoded_piece >> 8) & 1) as u8
    };
}

#[macro_export]
macro_rules! p_can_promote {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 9)) != 0
    };
}

#[macro_export]
macro_rules! p_is_royal {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 10)) != 0
    };
}

#[macro_export]
macro_rules! p_is_big {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 11)) != 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_is_major {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 12)) != 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_is_minor {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 12)) == 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_value {
    ($piece:expr) => {
        (($piece.encoded_piece >> 13) & 0xFFFF) as u16
    };
}

#[macro_export]
macro_rules! p_castle_right {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 29)) != 0
    };
}

#[macro_export]
macro_rules! p_castle_left {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 30)) != 0
    };
}

pub type PieceIndex = u8;

/// A structure representing a game piece with its properties.
///
/// A piece can have id from 0 - 254, with 255 reserved for "no piece".
///
/// A piece is encoded in 32 bits:
/// - Bits 0-7: Piece index
/// - Bit 8 : Color (0 for white, 1 for black)
/// - Bit 9 : Piece can promote (1 if can promote, 0 otherwise)
/// - Bit 10: Royal status (1 if royal, 0 otherwise)
/// - Bit 11: Big piece status
/// - Bit 12: Major piece status (1 if major, 0 if minor)
/// - Bits 13-28: Piece value
/// - Bit 29: Can castle kingside (right)
/// - Bit 30: Can castle queenside (left)
/// - Bit 31: Unused
///
///
/// the promotions field is a Vec<u8> that encodes the pieces this piece can 
/// promote to
pub struct Piece {
    pub name: String,
    pub char: char,

    pub promotions: Vec<PieceIndex>,
    pub encoded_piece: u32,
    pub rank: u8,                                                               /* used for move modifiers           */
}

impl Piece {
    /// Creates a new piece with the specified properties.
    ///
    /// # Arguments
    /// * `name` - The name of the piece
    /// * `symbol` - Display character for the piece
    /// * `promotions` - Bitset of pieces this can promote to
    /// * `piece_type` - Index of the piece type (0-254)
    /// * `color` - Color of the piece (0 for white, 1 for black)
    /// * `is_royal` - Whether this is a royal piece
    /// * `is_big` - Whether can be promoted to (true) or can promote (false)
    /// * `is_major` - Whether this is a major piece (true) or minor (false)
    /// * `can_castle_right` - Whether can castle to the right (kingside)
    /// * `can_castle_left` - Whether can castle to the left (queenside)
    /// * `value` - The piece value (0-65535, stored in 16 bits)
    pub fn new(
        name: String,
        char: char,
        promotions: Vec<PieceIndex>,
        index: u8,
        color: u8,
        is_royal: bool,
        is_big: bool,
        is_major: bool,
        can_castle_right: bool,
        can_castle_left: bool,
        value: u16,
    ) -> Self {
        let mut encoded_piece = index as u32;
        encoded_piece |= (color as u32) << 8;

        if is_royal {
            encoded_piece |= 1 << 10;
        }

        if is_big {
            encoded_piece |= 1 << 11;
        }

        if is_major {
            encoded_piece |= 1 << 12;
        }

        encoded_piece |= (value as u32 & 0xFFFF) << 13;

        if can_castle_right {
            encoded_piece |= 1 << 29;
        }

        if can_castle_left {
            encoded_piece |= 1 << 30;
        }

        if !promotions.is_empty() {
            encoded_piece |= 1 << 9;
        }

        let rank = 0;

        Self {
            name,
            char,
            promotions,
            encoded_piece,
            rank,
        }
    }
}

impl Debug for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Piece")
            .field("name", &self.name)
            .field("char", &self.char)
            .field("promotions", &self.promotions)
            .field("encoded_piece", &format_args!("{:#034b}", self.encoded_piece))
            .finish()
    }
}