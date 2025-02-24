//! # board.rs
//!
//! Defines a board structure and operations for bitboard manipulation.
//!
//! This file contains the implementation of a `Board` struct, which represents
//! a board using a bitboard of type `U4096`. It provides methods for setting,
//! clearing, and querying bits, as well as utility functions for bitboard
//! manipulation such as finding the least significant bit (LSB), most
//! significant bit (MSB), and counting set bits.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 18/02/2024

use bnum::types::U4096;

pub struct Board {
    pub ranks: u16,
    pub files: u16,
    pub bitboard: U4096,
}

impl Board {
    pub fn new(ranks: u16, files: u16) -> Board {
        Board {
            ranks,
            files,
            bitboard: U4096::from(0u32),
        }
    }

    pub fn set_bit(&mut self, rank: u16, files: u16) {
        let index = (rank * self.files + files) as u32;
        self.bitboard.set_bit(index, true);
    }

    pub fn clear_bit(&mut self, rank: u16, files: u16) {
        let index = (rank * self.files + files) as u32;
        self.bitboard.set_bit(index, false);
    }

    pub fn get_bit(&self, rank: u16, files: u16) -> bool {
        let index = (rank * self.files + files) as u32;
        self.bitboard.bit(index)
    }

    pub fn lsb(&self) -> u32 {
        self.bitboard.trailing_zeros()
    }

    pub fn msb(&self) -> u32 {
        self.bitboard.leading_zeros()
    }

    pub fn count_bits(&self) -> u32 {
        self.bitboard.count_ones()
    }

    pub fn to_string(&self) -> String {
        let mut result = String::new();
        for row in 0..self.ranks {
            for col in 0..self.files {
                if self.get_bit(row, col) {
                    result.push_str("1 ");
                } else {
                    result.push_str("0 ");
                }
            }
            result.push('\n');
        }
        result
    }
}