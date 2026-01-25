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

use bnum::types::{U256, U1024, U4096};

pub enum Bitboard {
    U64(u64),
    U256(U256),
    U1024(U1024),
    U4096(U4096),
}

impl Bitboard {
    pub fn new(size: u16) -> Self {
        match size {
            0..=64 => Bitboard::U64(0u64),
            65..=256 => Bitboard::U256(U256::from(0u32)),
            257..=1024 => Bitboard::U1024(U1024::from(0u32)),
            _ => Bitboard::U4096(U4096::from(0u32)),
        }
    }

    pub fn set_bit(&mut self, index: u32, value: bool) {
        match self {
            Bitboard::U64(b) => {
                if value {
                    *b |= 1u64 << index;
                } else {
                    *b &= !(1u64 << index);
                }
            }
            Bitboard::U256(b) => b.set_bit(index, value),
            Bitboard::U1024(b) => b.set_bit(index, value),
            Bitboard::U4096(b) => b.set_bit(index, value),
        }
    }

    pub fn bit(&self, index: u32) -> bool {
        match self {
            Bitboard::U64(b) => (b >> index) & 1 == 1,
            Bitboard::U256(b) => b.bit(index),
            Bitboard::U1024(b) => b.bit(index),
            Bitboard::U4096(b) => b.bit(index),
        }
    }

    pub fn trailing_zeros(&self) -> u32 {
        match self {
            Bitboard::U64(b) => b.trailing_zeros(),
            Bitboard::U256(b) => b.trailing_zeros(),
            Bitboard::U1024(b) => b.trailing_zeros(),
            Bitboard::U4096(b) => b.trailing_zeros(),
        }
    }

    pub fn leading_zeros(&self) -> u32 {
        match self {
            Bitboard::U64(b) => b.leading_zeros(),
            Bitboard::U256(b) => b.leading_zeros(),
            Bitboard::U1024(b) => b.leading_zeros(),
            Bitboard::U4096(b) => b.leading_zeros(),
        }
    }

    pub fn count_ones(&self) -> u32 {
        match self {
            Bitboard::U64(b) => b.count_ones(),
            Bitboard::U256(b) => b.count_ones(),
            Bitboard::U1024(b) => b.count_ones(),
            Bitboard::U4096(b) => b.count_ones(),
        }
    }
}

pub struct Board {
    pub ranks: u8,
    pub files: u8,
    bitboard: Bitboard,
}

impl Board {
    pub fn new(ranks: u8, files: u8) -> Board {
        let size = (ranks as u16) * (files as u16);
        Board {
            ranks,
            files,
            bitboard: Bitboard::new(size),
        }
    }

    pub fn set_bit(&mut self, rank: u8, file: u8) {
        assert!(rank < self.ranks, "Rank {rank} out of bounds.");
        assert!(file < self.files, "File {file} out of bounds.");

        let index = (rank as u32) * (self.files as u32) + (file as u32);
        self.bitboard.set_bit(index, true);
    }

    pub fn clear_bit(&mut self, rank: u8, file: u8) {
        assert!(rank < self.ranks, "Rank {rank} out of bounds.");
        assert!(file < self.files, "File {file} out of bounds.");

        let index = (rank as u32) * (self.files as u32) + (file as u32);
        self.bitboard.set_bit(index, false);
    }

    pub fn get_bit(&self, rank: u8, file: u8) -> bool {
        assert!(rank < self.ranks, "Rank {rank} out of bounds.");
        assert!(file < self.files, "File {file} out of bounds.");

        let index = (rank as u32) * (self.files as u32) + (file as u32);
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