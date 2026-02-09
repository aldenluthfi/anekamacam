//! # board.rs
//!
//! Defines a board structure and operations for bitboard manipulation.
//!
//! This file contains the implementation of a `Board`, which represents a board
//! using a bitboard. The bitboard type is selected based on the board
//! dimensions: u64 for standard 8x8 boards, U256 for boards up to 16x16, U1024
//! for boards up to 32x32, and U4096 for larger boards. It provides methods
//! for setting, clearing, and querying bits, as well as utility functions for
//! bitboard manipulation such as finding the least significant bit (LSB), most
//! significant bit (MSB), and counting set bits.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 18/02/2024

use bnum::types::U4096;

pub type Board = (u8, u8, U4096);

#[macro_export]
macro_rules! board {
    ($ranks:expr, $files:expr) => {
        ($ranks, $files, U4096::ZERO)
    };
}

#[macro_export]
macro_rules! ranks {
    ($board:expr) => {
        $board.0
    };
}

#[macro_export]
macro_rules! files {
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