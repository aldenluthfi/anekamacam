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

pub type PieceIndex = u8;

/*----------------------------------------------------------------------------*\
                               UTILTY PIECE MACROS
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! p_value {
    ($piece:expr, $state:expr) => {{
        let piece = &$state.statics.pieces[$piece as usize];

        let ovalue = p_ovalue!(piece) as u32;
        let evalue = p_evalue!(piece) as u32;

        let opening_score = $state.statics.opening_score;
        let endgame_score = $state.statics.endgame_score;
        let current_score =
            $state.phase_score.clamp(endgame_score, opening_score);

        match $state.game_phase {
            OPENING | SETUP => ovalue,
            ENDGAME => evalue,
            MIDDLEGAME => {
                (
                    (ovalue * (current_score - endgame_score)) +
                    (evalue * (opening_score - current_score))
                ) / (opening_score - endgame_score)
            }
            _ => unreachable!(),
        }
    }};
}

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
#[derive(Clone)]
pub struct Piece {
    pub name: String,
    pub char: char,

    pub promotions: Vec<PieceIndex>,
    pub encoded_static: u32,
    pub encoded_dynamic: u32,
}

impl Piece {
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
