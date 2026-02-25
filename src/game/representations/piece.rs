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

use bnum::cast::As;
use bnum::types::U2048;

use crate::constants::DEFAULT_DROP;

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
        ($piece.encoded_piece & (1 << 11)) != 0
    };
}

#[macro_export]
macro_rules! p_is_major {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 12)) != 0
    };
}

#[macro_export]
macro_rules! p_is_minor {
    ($piece:expr) => {
        ($piece.encoded_piece & (1 << 12)) == 0
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

#[macro_export]
macro_rules! p_promotions {
    ($piece:expr) => {
        {

            let promotions_count = (
                $piece.promotions & U2048::from(0xFFu32)
            ).as_::<u8>();

            if !p_can_promote!($piece) || promotions_count == 0 {
                Vec::new()
            } else {
                (1..=promotions_count)
                    .map(|i| (
                        ($piece.promotions >> (i * 8)) & U2048::from(0xFFu32)
                    ).as_::<usize>())
                    .collect::<Vec<usize>>()
            }
        }
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
/// the promotions field is a 2048-bit number representing which pieces this
/// piece can promote to. it consists of:
/// - A byte to represent how many pieces can be promoted to (up to 255 pieces)
/// - the next 255 bytes represents a piece index that this piece can promote to
pub struct Piece {
    pub name: String,
    pub movement: String,
    pub drop: String,                                                           /* Drop rule in CDN                   */
    pub stand_off: String,                                                      /* Stand-off rule in CDN              */
    pub setup: String,                                                          /* Setup rule in CDN                  */
    pub char: char,

    pub promotions: U2048,
    pub encoded_piece: u32,
    pub rank: u8,                                                               /* used for move modifiers           */
}

impl Piece {
    /// Creates a new piece with the specified properties.
    ///
    /// # Arguments
    /// * `name` - The name of the piece
    /// * `movement` - Movement pattern in Cheesy King Notation
    /// * `symbol` - Display character for the piece
    /// * `promotions` - Bitset of pieces this can promote to
    /// * `piece_type` - Index of the piece type (0-254)
    /// * `color` - Color of the piece (0 for white, 1 for black)
    /// * `is_royal` - Whether this is a royal piece
    /// * `is_big` - Whether can be promoted to (true) or can promote (false)
    /// * `is_major` - Whether this is a major piece (true) or minor (false)
    /// * `value` - The piece value (0-65535, stored in 16 bits)
    /// * `castle_right` - This piece can castle kingside (right)
    /// * `castle_left` - This piece can castle queenside (left)
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
            encoded_piece |= 1 << 10;
        }

        if is_big {
            encoded_piece |= 1 << 11;
        }

        if is_major {
            encoded_piece |= 1 << 12;
        }

        encoded_piece |= (value as u32 & 0xFFFF) << 13;

        if movement.contains('o') {
            encoded_piece |= 1 << 29;
        }

        if movement.contains('O') {
            encoded_piece |= 1 << 30;
        }

        if promotions != U2048::ZERO {
            encoded_piece |= 1 << 9;
        }

        let drop = DEFAULT_DROP.to_string();
        let setup = DEFAULT_DROP.to_string();
        let stand_off = "".to_string();
        let rank = 0;

        Self {
            name,
            movement,
            drop,
            setup,
            stand_off,
            char: symbol,
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
            .field("movement", &self.movement)
            .field("drop", &self.drop)
            .field("setup", &self.setup)
            .field("char", &self.char)
            .field("promotions", &p_promotions!(self))
            .field("encoded_piece", &format_args!("{:#034b}", self.encoded_piece))
            .finish()
    }
}