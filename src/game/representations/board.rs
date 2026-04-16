//! # board.rs
//!
//! Defines a board structure and operations for bitboard manipulation.
//!
//! This file contains the implementation of a `Board`, which represents a board
//! using a bitboard. The bitboard type is  U4096 for larger boards. It provides
//! macros for setting, clearing, and querying bits, as well as utility
//! functions for bitboard manipulation.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 18/02/2024

use crate::*;

/// Represents a board as a compact `(files, ranks, bits)` triple.
///
/// The first element stores the file count and the second stores the rank
/// count.
/// The third element stores occupancy bits in a `U4096` bitboard.
pub type Board = (u8, u8, U4096);


/*----------------------------------------------------------------------------*\
                        BITBOARD HELPER REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Bitboard helper macros used across move generation and state updates.
/// 
/// These macros operate on the compact [`Board`] tuple representation:
/// `(files, ranks, bits)`, where `bits` is a `U4096` bitset.
/// 
/// Access helpers:
/// - `board!`, `files!`, `ranks!`, `get!`
/// 
/// Mutation helpers:
/// - `set!`, `clear!`, `or!`, `and!`, `xor!`, `not!`
#[macro_export]
macro_rules! board {
    ($files:expr, $ranks:expr) => {
        ($files, $ranks, U4096::ZERO)
    };
}

#[macro_export]
macro_rules! files {
    ($board:expr) => {
        $board.0
    };
}

#[macro_export]
macro_rules! ranks {
    ($board:expr) => {
        $board.1
    };
}

#[macro_export]
macro_rules! get {
    ($board:expr, $index:expr) => {
        $board.2.bit($index)
    };
}

#[macro_export]
macro_rules! set {
    ($board:expr, $index:expr) => {
        $board.2.set_bit($index, true);
    };
}

#[macro_export]
macro_rules! clear {
    ($board:expr, $index:expr) => {
        $board.2.set_bit($index, false);
    };
}

#[macro_export]
macro_rules! or {
    ($board1:expr, $board2:expr) => {
        $board1.2 |= &$board2.2;
    };
}

#[macro_export]
macro_rules! and {
    ($board1:expr, $board2:expr) => {
        $board1.2.and_assign(&$board2.2);
    };
}

#[macro_export]
macro_rules! xor {
    ($board1:expr, $board2:expr) => {
        $board1.2.xor_assign(&$board2.2);
    };
}

#[macro_export]
macro_rules! not {
    ($board:expr) => {
        $board.2 = !$board.2;
    };
}
