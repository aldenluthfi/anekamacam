//! # piece.rs
//!
//! Defines piece representation and properties.
//!
//! This file contains the implementation of the `Piece` struct,
//! which represents
//! a chess piece type with static and dynamic encoded attributes.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026

use crate::*;

/*----------------------------------------------------------------------------*\
                        PIECE BITFIELD REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Piece bitfield accessor macros.
///
/// These macros decode `Piece::encoded_static` and `Piece::encoded_dynamic`
/// into readable attributes used throughout move generation and evaluation.
///
/// Static accessors:
/// `p_index!`, `p_color!`, `p_can_promote!`, `p_is_royal!`, `p_rank!`,
/// `p_castle_right!`, `p_castle_left!`
///
/// Dynamic accessors:
/// `p_is_big!`, `p_is_major!`, `p_is_minor!`, `p_ovalue!`, `p_evalue!`
#[macro_export]
macro_rules! p_index {
    ($piece:expr) => {
        ($piece.encoded_static & 0xFF) as PieceIndex
    };
}

#[macro_export]
macro_rules! p_color {
    ($piece:expr) => {
        (($piece.encoded_static >> 8) & 1) as u8
    };
}

#[macro_export]
macro_rules! p_can_promote {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 9)) != 0
    };
}

#[macro_export]
macro_rules! p_is_royal {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 10)) != 0
    };
}

#[macro_export]
macro_rules! p_rank {
    ($piece:expr) => {
        (($piece.encoded_static >> 11) & 0xFF) as u8
    };
}

#[macro_export]
macro_rules! p_is_big {
    ($piece:expr) => {
        ($piece.encoded_dynamic & 1) != 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_is_major {
    ($piece:expr) => {
        ($piece.encoded_dynamic & (1 << 1)) != 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_is_minor {
    ($piece:expr) => {
        ($piece.encoded_dynamic & (1 << 1)) == 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_ovalue {
    ($piece:expr) => {
        (($piece.encoded_dynamic >> 2) & 0x3FFF) as u16
    };
}

#[macro_export]
macro_rules! p_evalue {
    ($piece:expr) => {
        (($piece.encoded_dynamic >> 16) & 0x3FFF) as u16
    };
}

#[macro_export]
macro_rules! p_castle_right {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 29)) != 0
    };
}

#[macro_export]
macro_rules! p_castle_left {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 30)) != 0
    };
}

pub type PieceIndex = u8;

/// A structure representing a game piece with its properties.
///
/// A piece can have id from 0 - 254, with 255 reserved for "no piece".
///
/// Static data (`encoded_static`) is encoded in 32 bits:
/// - Bits 0-7      : Piece index
/// - Bit 8         : Color (0 for white, 1 for black)
/// - Bit 9         : Piece can promote (1 if can promote, 0 otherwise)
/// - Bit 10        : Royal status (1 if royal, 0 otherwise)
/// - Bits 11-18    : Piece rank
/// - Bit 29        : Can castle kingside (right)
/// - Bit 30        : Can castle queenside (left)
/// - Other bits    : Unused
///
/// Dynamic data (`encoded_dynamic`) is encoded in 32 bits:
/// - Bit 0         : Big piece status
/// - Bit 1         : Major piece status (1 if major, 0 if minor)
/// - Bits 2-15     : Opening piece value (14-bit)
/// - Bits 16-29    : Endgame piece value (14-bit)
/// - Other bits    : Unused
///
/// The `promotions` field is a `Vec<u8>` that encodes the pieces this piece can
/// promote to.
pub struct Piece {
    pub name: String,
    pub char: char,

    pub promotions: Vec<PieceIndex>,
    pub encoded_static: u32,
    pub encoded_dynamic: u32,
}

impl Piece {
    /// Creates a new piece with the specified properties.
    ///
    /// # Arguments
    /// - `name`            : The name of the piece
    /// - `char`            : Display character for the piece
    /// - `promotions`      : Pieces this can promote to
    /// - `index`           : Index of the piece type (0-254)
    /// - `color`           : Color of the piece (0 for white, 1 for black)
    /// - `is_royal`        : Whether this is a royal piece
    /// - `castle_right`    : Whether can castle to the right (kingside)
    /// - `castle_left`     : Whether can castle to the left (queenside)
    /// - `rank`            : The piece rank used for move modifiers
    pub fn new(
        name: String,
        char: char,
        promotions: Vec<PieceIndex>,
        index: u8,
        color: u8,
        is_royal: bool,
        castle_right: bool,
        castle_left: bool,
        rank: u8,
    ) -> Self {
        let mut encoded_static = index as u32;
        encoded_static |= (color as u32) << 8;

        if !promotions.is_empty() {
            encoded_static |= 1 << 9;
        }

        if is_royal {
            encoded_static |= 1 << 10;
        }

        if castle_right {
            encoded_static |= 1 << 29;
        }

        if castle_left {
            encoded_static |= 1 << 30;
        }

        encoded_static |= (rank as u32) << 11;

        let encoded_dynamic = 0u32;

        Self {
            name,
            char,
            promotions,
            encoded_static,
            encoded_dynamic,
        }
    }
}

impl Debug for Piece {
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        f.debug_struct("Piece")
            .field("name", &self.name)
            .field("char", &self.char)
            .field("promotions", &self.promotions)
            .field(
                "encoded_static",
                &format_args!("{:#034b}", self.encoded_static),
            )
            .field(
                "encoded_dynamic",
                &format_args!("{:#034b}", self.encoded_dynamic),
            )
            .finish()
    }
}
