//! # board.rs
//!
//! Defines a board structure and operations for bitboard manipulation.
//!
//! The engine must track occupancy on boards far larger than the 64 squares a
//! `u64` can hold, since variants range up to very large grids. This file
//! gives the rest of the engine one compact board value backed by a wide
//! bitset, plus a vocabulary of cheap bit operations over it, so move
//! generation, masks, and state updates never touch raw bit arithmetic.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 18/02/2024

use crate::*;

/// Board
///
/// Compact board representation as a `(files, ranks, bits)` triple: the
/// file count, the rank count, and a `U4096` bitboard whose bit at index
/// `rank * files + file` marks occupancy of that square.
pub type Board = (u8, u8, U4096);

/*----------------------------------------------------------------------------*\
                        BITBOARD HELPER REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Bitboard helper macros over the compact [`Board`] tuple.
///
/// All operate on the `(files, ranks, bits)` representation, where `bits`
/// is a `U4096` bitset indexed by `rank * files + file`. They keep move
/// generation and state updates free of raw bit twiddling.
///
/// The index is file-fastest, `rank * files + file`; on a 4x3 board:
///
/// ```text
/// ┌────┬────┬────┬────┐
/// │ 8  │ 9  │ 10 │ 11 │   rank 2
/// ├────┼────┼────┼────┤
/// │ 4  │ 5  │ 6  │ 7  │   rank 1
/// ├────┼────┼────┼────┤
/// │ 0  │ 1  │ 2  │ 3  │   rank 0
/// └────┴────┴────┴────┘
///   f0   f1   f2   f3
/// ```
///
/// Construction and queries:
/// - board!       : builds an empty board of the given file and rank counts
/// - files!       : reads the file count
/// - ranks!       : reads the rank count
/// - get!         : tests whether the bit at an index is set
/// - count_bits!  : counts the set bits (piece/occupancy popcount)
/// - set_indices! : collects the indices of every set bit
/// - is_empty!    : tests whether no bit is set
///
/// Mutation:
/// - set!   : sets the bit at an index
/// - clear! : clears the bit at an index
/// - or!    : unions another board's bits into this one
/// - and!   : intersects this board with another board's bits
/// - xor!   : toggles this board's bits by another board's
/// - not!   : inverts every bit in place
#[macro_export]
macro_rules! board {
    ($files:expr, $ranks:expr) => {
        ($files, $ranks, U4096::MIN)
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
        $board1.2 |= &$board2.2
    };
}

#[macro_export]
macro_rules! and {
    ($board1:expr, $board2:expr) => {
        $board1.2 &= &$board2.2
    };
}

#[macro_export]
macro_rules! xor {
    ($board1:expr, $board2:expr) => {
        $board1.2 ^= &$board2.2
    };
}

#[macro_export]
macro_rules! not {
    ($board:expr) => {
        $board.2 = !$board.2
    };
}

#[macro_export]
macro_rules! count_bits {
    ($board:expr) => {
        $board.2.count_ones()
    }
}

#[macro_export]
macro_rules! set_indices {
    ($board:expr) => {{
        let mut indices = Vec::new();
        for index in 0..(files!($board) as usize * ranks!($board) as usize) {
            if get!($board, index as u32) {
                indices.push(index);
            }
        }
        indices
    }};
}

#[macro_export]
macro_rules! is_empty {
    ($board:expr) => {
        $board.2.is_zero()
    };
}
