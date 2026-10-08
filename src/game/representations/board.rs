//! board.rs
//!
//! Defines the board type and the bitboard macros.
//!
//! Some variants have boards with many more than 64 squares, so a `u64`
//! is too small. This file defines one board type on a wide bitset and the
//! bit operations on it. Other modules use these macros, not raw bits.
//!
//! Created: 18/02/2024
//! Author : Alden Luthfi

use crate::*;

/// BoardBits
///
/// The 4096-bit bitset of each [`Board`]. This width sets the cost of each
/// board copy, for all variants. It is wider than all supported boards.
///
/// Notes:
/// The Zobrist tables have `MAX_SQUARES` entries. Thus `MAX_SQUARES`, not
/// this width, is the limit on the board area.
///
pub type BoardBits = U4096;

/// Board
///
/// A board as a `(files, ranks, bits)` triple. The bit at the index
/// `rank * files + file` is set when that square is occupied.
///
pub type Board = (u8, u8, BoardBits);

/*----------------------------------------------------------------------------*\
                        BITBOARD HELPER REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Bitboard helper macros
///
/// Bitboard macros for the [`Board`] triple. The bit index is
/// `rank * files + file`, so the file changes fastest. On a 4x3 board:
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
///   - index : u32    -> square index to test
///
///   Return:
///   bool             -> true when the bit at the index is set
///
/// count_bits!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   u32              -> number of set bits
///
/// set_indices!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   Vec<usize>       -> indices of the set bits, in ascending order
///
/// is_empty!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   bool             -> true when no bit is set
///
/// meets_row!
///
///   Params:
///   - board : &Board -> board to read
///   - row   : &[u64] -> low words of a square set, the first word first
///
///   Return:
///   bool             -> true when a bit is set in both
///
/// Changes in place, no return value:
///
/// set!
///
///   Params:
///   - board : &mut Board -> board to change
///   - index : u32        -> square index of the bit to set
///
/// clear!
///
///   Params:
///   - board : &mut Board -> board to change
///   - index : u32        -> square index of the bit to clear
///
/// or!
///
///   Params:
///   - board1: &mut Board -> target, gets the union
///   - board2: &Board     -> source of the bits
///
/// and!
///
///   Params:
///   - board1: &mut Board -> target, gets the intersection
///   - board2: &Board     -> source of the bits
///
#[macro_export]
macro_rules! board {
    ($files:expr, $ranks:expr) => {
        ($files, $ranks, BoardBits::MIN)
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

#[macro_export]
macro_rules! meets_row {
    ($board:expr, $row:expr) => {{
        let row: &[u64] = $row;
        let bytes = $board.2.as_bytes();

        row.iter().enumerate().any(|(word, mask)| {
            let chunk = &bytes[word * 8..word * 8 + 8];

            u64::from_le_bytes(chunk.try_into().unwrap()) & mask != 0
        })
    }};
}
