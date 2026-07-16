//! board.rs
//!
//! Defines a board structure and operations for bitboard manipulation.
//!
//! The engine must track occupancy on boards far larger than the 64 squares a
//! `u64` can hold, since variants range up to very large grids. This file
//! gives the rest of the engine one compact board value backed by a wide
//! bitset, plus a vocabulary of cheap bit operations over it, so move
//! generation, masks, and state updates never touch raw bit arithmetic.
//!
//! Created: 18/02/2024
//! Author : Alden Luthfi

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
///
/// board!
///
///   Params:
///   - files : u8 -> file count of the new board
///   - ranks : u8 -> rank count of the new board
///
///   Return:
///   Board        -> empty board of the given size
///
/// files!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   u8               -> file count
///
/// ranks!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   u8               -> rank count
///
/// get!
///
///   Params:
///   - board : &Board -> board to read
///   - index : u32    -> square index tested
///
///   Return:
///   bool             -> whether the bit at the index is set
///
/// count_bits!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   u32              -> number of set bits (piece/occupancy popcount)
///
/// set_indices!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   Vec<usize>       -> indices of every set bit, ascending
///
/// is_empty!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   bool             -> whether no bit is set
///
/// Mutation (in place, no return value):
///
/// set!
///
///   Params:
///   - board : &mut Board -> board mutated
///   - index : u32        -> square index whose bit is set
///
/// clear!
///
///   Params:
///   - board : &mut Board -> board mutated
///   - index : u32        -> square index whose bit is cleared
///
/// toggle!
///
///   Params:
///   - board : &mut Board -> board mutated
///   - index : u32        -> square index whose bit is flipped
///
/// or!
///
///   Params:
///   - board1: &mut Board -> destination, unioned in place
///   - board2: &Board     -> source supplying the bits
///
/// and!
///
///   Params:
///   - board1: &mut Board -> destination, intersected in place
///   - board2: &Board     -> source supplying the bits
///
/// xor!
///
///   Params:
///   - board1: &mut Board -> destination, toggled in place
///   - board2: &Board     -> source supplying the bits
///
/// not!
///
///   Params:
///   - board : &mut Board -> board whose every bit is inverted
///
///   Return:
///   ()                   -> mutates the supplied board in place
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
macro_rules! toggle {
    ($board:expr, $index:expr) => {
        $board.2.set_bit($index, !$board.2.bit($index));
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
