//! board.rs
//!
//! Defines the board type and the bitboard macros.
//!
//! Some variants have boards with many more than 64 squares, so a `u64`
//! is too small. This file defines one board type on an array of 64-bit
//! words and the bit operations on it. An operation reads only the words
//! of the board's own area. Other modules use these macros, not raw bits.
//!
//! Created: 18/02/2024
//! Author : Alden Luthfi

use crate::*;

/// BoardBits
///
/// The bits of each [`Board`]: `MAX_SQUARES` bits in 64-bit words. A board
/// uses only the words that cover its own area, `files * ranks` bits:
///
/// - 8x8, 64 squares     : 1 word
/// - 10x10, 100 squares  : 2 words
/// - 36x36, 1296 squares : 21 words
///
/// Thus a union or a count on a small board costs one or two words, and
/// all board sizes stay in one build.
///
/// Notes:
/// The Zobrist tables have `MAX_SQUARES` entries, so `MAX_SQUARES` is the
/// limit on the board area.
///
pub type BoardBits = [u64; BOARD_WORDS];

/// BOARD_WORDS
///
/// The number of 64-bit words in [`BoardBits`], for `MAX_SQUARES` bits.
///
pub const BOARD_WORDS: usize = MAX_SQUARES / 64;

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
/// board_words!
///
///   Params:
///   - board : &Board -> board to read
///
///   Return:
///   usize            -> words that cover the area, `files * ranks` bits
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
        ($files, $ranks, [0u64; BOARD_WORDS])
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
macro_rules! board_words {
    ($board:expr) => {
        (files!($board) as usize * ranks!($board) as usize + 63) >> 6
    };
}

#[macro_export]
macro_rules! get {
    ($board:expr, $index:expr) => {{
        let bit_index = $index as usize;
        ($board.2[bit_index >> 6] >> (bit_index & 63)) & 1 != 0
    }};
}

#[macro_export]
macro_rules! set {
    ($board:expr, $index:expr) => {{
        let bit_index = $index as usize;
        $board.2[bit_index >> 6] |= 1u64 << (bit_index & 63);
    }};
}

#[macro_export]
macro_rules! clear {
    ($board:expr, $index:expr) => {{
        let bit_index = $index as usize;
        $board.2[bit_index >> 6] &= !(1u64 << (bit_index & 63));
    }};
}

#[macro_export]
macro_rules! or {
    ($board1:expr, $board2:expr) => {{
        let words = board_words!($board1);
        let source = &$board2;

        for word in 0..words {
            $board1.2[word] |= source.2[word];
        }
    }};
}

#[macro_export]
macro_rules! and {
    ($board1:expr, $board2:expr) => {{
        let words = board_words!($board1);
        let source = &$board2;

        for word in 0..words {
            $board1.2[word] &= source.2[word];
        }
    }};
}

#[macro_export]
macro_rules! count_bits {
    ($board:expr) => {{
        let source = &$board;

        source.2[..board_words!(source)].iter()
            .map(|word| word.count_ones())
            .sum::<u32>()
    }};
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
    ($board:expr) => {{
        let source = &$board;

        source.2[..board_words!(source)].iter().all(|word| *word == 0)
    }};
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
