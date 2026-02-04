//! # board.rs
//!
//! Defines a board structure and operations for bitboard manipulation.
//!
//! This file contains the implementation of a `Board` struct, which represents
//! a board using a dynamically-sized bitboard. The bitboard type is selected
//! based on the board dimensions: u64 for standard 8x8 boards, U256 for boards
//! up to 16x16, U1024 for boards up to 32x32, and U4096 for larger boards.
//! It provides methods for setting, clearing, and querying bits, as well as
//! utility functions for bitboard manipulation such as finding the least
//! significant bit (LSB), most significant bit (MSB), and counting set bits.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 18/02/2024

use bnum::types::U4096;
use crate::constants::*;
use std::ops::{BitAnd, BitAndAssign, BitOrAssign, BitXorAssign};

#[derive(Debug, PartialEq, Clone)]
pub struct Board {
    pub ranks: u8,
    pub files: u8,
    pub bits: U4096,
}

#[hotpath::measure_all]
impl Board {
    pub fn new(files: u8, ranks: u8) -> Board {
        #[cfg(debug_assertions)]
        {
            assert!(
            files <= MAX_FILES,
            "Number of files {files} exceeds maximum of {MAX_FILES}."
            );
            assert!(
            ranks <= MAX_RANKS,
            "Number of ranks {ranks} exceeds maximum of {MAX_RANKS}."
            );
        }

        Board {
            ranks,
            files,
            bits: U4096::from(0u32),
        }
    }

    pub fn set_bit(&mut self, index: u32) {
        #[cfg(debug_assertions)]
        {
            let size = (self.ranks as u32) * (self.files as u32);
            assert!(
                index < size,
                "Index {index} out of bounds for board of size {size}."
            );
        }

        self.bits.set_bit(index, true);
    }

    pub fn clear_bit(&mut self, index: u32) {
        #[cfg(debug_assertions)]
        {
            let size = (self.ranks as u32) * (self.files as u32);
            assert!(
                index < size,
                "Index {index} out of bounds for board of size {size}."
            );
        }

        self.bits.set_bit(index, false);
    }

    pub fn get_bit(&self, index: u32) -> bool {
        #[cfg(debug_assertions)]
        {
            let size = (self.ranks as u32) * (self.files as u32);
            assert!(
                index < size,
                "Index {index} out of bounds for board of size {size}."
            );
        }

        self.bits.bit(index)
    }

    pub fn lsb(&self) -> u32 {
        self.bits.trailing_zeros()
    }

    pub fn count_bits(&self) -> u32 {
        self.bits.count_ones()
    }

    #[inline(always)]
    pub fn bit_indices(&self) -> Vec<u32> {
        let count = self.bits.count_ones();
        if count == 0 {
            return Vec::new();
        }

        let mut bits = self.bits;
        let mut indices = Vec::with_capacity(count as usize);

        match count {                                                           /* Special case common scenarios      */
            1 => {
                indices.push(bits.trailing_zeros());
                return indices;
            }
            2 => {
                indices.push(bits.trailing_zeros());
                bits &= bits - U4096::ONE;
                indices.push(bits.trailing_zeros());
                return indices;
            }
            _ => {
                while bits != U4096::ZERO {                                     /* General case                       */
                    let lsb = bits.trailing_zeros();
                    indices.push(lsb);
                    bits &= bits - U4096::ONE;
                }
            }
        }

        indices
    }

    pub fn and_assign(&mut self, rhs: &Board) {
        self.bits &= rhs.bits;
    }

    pub fn or_assign(&mut self, rhs: &Board) {
        self.bits |= rhs.bits;
    }

    pub fn xor_assign(&mut self, rhs: &Board) {
        self.bits ^= rhs.bits;
    }

    pub fn not_assign(&mut self) {
        self.bits = !self.bits;
    }
}

impl BitAndAssign for Board {
    fn bitand_assign(&mut self, rhs: Self) {
        self.bits &= rhs.bits;
    }
}

impl BitOrAssign for Board {
    fn bitor_assign(&mut self, rhs: Self) {
        self.bits |= rhs.bits;
    }
}

impl BitXorAssign for Board {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.bits ^= rhs.bits;
    }
}

impl BitAnd for &Board {
    type Output = Board;

    fn bitand(self, rhs: Self) -> Self::Output {
        Board {
            bits: self.bits & rhs.bits,
            ranks: self.ranks,
            files: self.files,
        }
    }
}