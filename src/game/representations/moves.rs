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

/// A type representing a single move in the game.
///
/// This structure is used for moves without multiple captures (SingleNoCapture,
/// SingleCapture, and HopperCapture).
///
/// The first three bits is used to represent the type of format used:
/// - 000: Single move without capture
/// - 001: Single move with capture or unload
/// - 010: Multi-capture move
///
/// - The next 8 bits indicates the piece index of the piece making the move.
/// - The next 12 bits represent the starting square index (0-4095).
/// - The following 12 bits represent the ending square index (0-4095).
/// - The following bit represents if the move is an initial move (1) or not (0)
/// - Next bit indicates if this move can be performed as an initial move (1)
///   or not (0).
/// - The following bit represents if the move is a promotion (1) or not (0).
/// - The following bit represents if the move creates an en passant square (1)
/// - or not (0).
/// - the following 8 bits represent the promoting piece type (if applicable).
/// - The following 8 bits represent the promoted piece type (if applicable).
/// - The following 24 bits represents the en passant square (if applicable).
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next bit indicates if this move can be used to capture en passant (1) or
///   not (0).
/// - Next bit indicates if this move is a unload (1) or regular capture (0).
/// - Next 12 bits are the unload square index (if applicable).
/// - Next 8 bits represent the captured piece type.
/// - Next 12 bits represent the captured piece square index.
/// - Next bit indicates if the piece captured is unmoved (1) or not (0).
/// - The remaining bits are unused.
///
/// The second array is used to store the indices of all captured pieces.
///
/// Each captured piece is represented in 35 bits:
/// - Next bit indicates this capture can be used to capture a royal piece (1)
///   or not (0).
/// - Next bit indicates if this move can be used to capture en passant (1) or
///   not (0).
/// - Next bit indicates if this move is a unload (1) or regular capture (0).
/// - Next 12 bits: Unload square index (if applicable).
/// - Next 8 bits: bits represent the captured piece type.
/// - Next 12 bits: represent the captured piece square index.
/// - the last bit indicates if the piece captured is unmoved (1) or not (0).
pub type Move = (u128, Vec<u64>);

/*----------------------------------------------------------------------------*\
                          MOVE REPRESENTATION ENCODING
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! enc_move_type {
    ($mv:expr, $val:expr) => {
        $mv.0 |= $val & 0x7;
    };
}

#[macro_export]
macro_rules! enc_piece {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFF) << 3;
    };
}

#[macro_export]
macro_rules! enc_start {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 11;
    };
}

#[macro_export]
macro_rules! enc_end {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 23;
    };
}

#[macro_export]
macro_rules! enc_must_initial {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 35;
    };
}

#[macro_export]
macro_rules! enc_must_not_initial {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 36;
    };
}

#[macro_export]
macro_rules! enc_promotion {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 37;
    };
}

#[macro_export]
macro_rules! enc_creates_enp {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 38;
    };
}

#[macro_export]
macro_rules! enc_promoting {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFF) << 39;
    };
}

#[macro_export]
macro_rules! enc_promoted {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFF) << 47;
    };
}

#[macro_export]
macro_rules! enc_created_enp {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 55;
    };
}
#[macro_export]
macro_rules! enc_can_check {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 79;
    };
}

#[macro_export]
macro_rules! enc_can_enp {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 80;
    };
}

#[macro_export]
macro_rules! enc_is_unload {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 81;
    };
}

#[macro_export]
macro_rules! enc_unload_square {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 82;
    };
}

#[macro_export]
macro_rules! enc_captured_piece {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFF) << 94;
    };
}

#[macro_export]
macro_rules! clear_captured_piece {
    ($mv:expr) => {
        $mv.0 &= !(0xFF << 94);
    };
}

#[macro_export]
macro_rules! enc_captured_square {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 102;
    };
}

#[macro_export]
macro_rules! enc_captured_unmoved {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 114;
    };
}

/*----------------------------------------------------------------------------*\
                          MOVE REPRESENTATION DECODING
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! move_type {
    ($mv:expr) => {
        $mv.0 & 0x7
    };
}

#[macro_export]
macro_rules! piece {
    ($mv:expr) => {
        ($mv.0 >> 3) & 0xFF
    };
}

#[macro_export]
macro_rules! start {
    ($mv:expr) => {
        ($mv.0 >> 11) & 0xFFF
    };
}

#[macro_export]
macro_rules! end {
    ($mv:expr) => {
        ($mv.0 >> 23) & 0xFFF
    };
}

#[macro_export]
macro_rules! must_initial {
    ($mv:expr) => {
        ($mv.0 >> 35) & 1
    };
}

#[macro_export]
macro_rules! must_not_initial {
    ($mv:expr) => {
        ($mv.0 >> 36) & 1
    };
}

#[macro_export]
macro_rules! promotion {
    ($mv:expr) => {
        ($mv.0 >> 37) & 1 == 1
    };
}

#[macro_export]
macro_rules! creates_enp {
    ($mv:expr) => {
        ($mv.0 >> 38) & 1 == 1
    };
}

#[macro_export]
macro_rules! promoting {
    ($mv:expr) => {
        ($mv.0 >> 39) & 0xFF
    };
}

#[macro_export]
macro_rules! promoted {
    ($mv:expr) => {
        ($mv.0 >> 47) & 0xFF
    };
}

#[macro_export]
macro_rules! created_enp {
    ($mv:expr) => {
        ($mv.0 >> 55) & 0xFFF
    };
}

#[macro_export]
macro_rules! can_check {
    ($mv:expr) => {
        ($mv.0 >> 79) & 1 == 1
    };
}

#[macro_export]
macro_rules! can_enp {
    ($mv:expr) => {
        ($mv.0 >> 80) & 1 == 1
    };
}

#[macro_export]
macro_rules! is_unload {
    ($mv:expr) => {
        ($mv.0 >> 81) & 1 == 1
    };
}

#[macro_export]
macro_rules! unload_square {
    ($mv:expr) => {
        ($mv.0 >> 82) & 0xFFF
    };
}

#[macro_export]
macro_rules! captured_piece {
    ($mv:expr) => {
        ($mv.0 >> 94) & 0xFF
    };
}

#[macro_export]
macro_rules! captured_square {
    ($mv:expr) => {
        ($mv.0 >> 102) & 0xFFF
    };
}

#[macro_export]
macro_rules! captured_unmoved {
    ($mv:expr) => {
        ($mv.0 >> 114) & 1 == 1
    };
}

/*----------------------------------------------------------------------------*\
                        MOVE LIST REPRESENTATION DECODING
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! multi_move_can_check {
    ($mv:expr) => {
        $mv & 1
    };
}

#[macro_export]
macro_rules! multi_move_can_enp {
    ($mv:expr) => {
        ($mv >> 1) & 1 == 1
    };
}

#[macro_export]
macro_rules! multi_move_is_unload {
    ($mv:expr) => {
        ($mv >> 2) & 1 == 1
    };
}

#[macro_export]
macro_rules! multi_move_unload_square {
    ($mv:expr) => {
        ($mv >> 3) & 0xFFF
    };
}

#[macro_export]
macro_rules! multi_move_captured_piece {
    ($mv:expr) => {
        ($mv >> 15) & 0xFF
    };
}

#[macro_export]
macro_rules! multi_move_captured_square {
    ($mv:expr) => {
        ($mv >> 23) & 0xFFF
    };
}

#[macro_export]
macro_rules! multi_move_captured_unmoved {
    ($mv:expr) => {
        ($mv >> 35) & 1 == 1
    };
}

/*----------------------------------------------------------------------------*\
                      MOVE LIST REPRESENTATION ENCODING
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! enc_multi_move_can_check {
    ($mv:expr, $val:expr) => {
        $mv |= $val & 1;
    };
}

#[macro_export]
macro_rules! enc_multi_move_can_enp {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 1) << 1;
    };
}

#[macro_export]
macro_rules! enc_multi_move_is_unload {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 1) << 2;
    };
}

#[macro_export]
macro_rules! enc_multi_move_unload_square {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0xFFF) << 3;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_piece {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0xFF) << 15;
    };
}

#[macro_export]
macro_rules! clear_multi_move_captured_piece {
    ($mv:expr) => {
        $mv &= !(0xFF << 15);
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_square {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0xFFF) << 23;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_unmoved {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 1) << 35;
    };
}