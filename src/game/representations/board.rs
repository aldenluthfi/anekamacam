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
use crate::constants::*;
use std::ops::{BitAnd, BitOr, BitXor, Not};

#[derive(Debug, PartialEq, Clone)]
pub enum Bitboard {
    U64(u64),
    U256(U256),
    U1024(U1024),
    U4096(U4096),
}

#[hotpath::measure_all]
impl Bitboard {
    pub fn new(size: u16) -> Self {
        #[cfg(debug_assertions)]
        {
            assert!(
            size <= MAX_SQUARES as u16,
            "Bitboard size {size} exceeds maximum size of {MAX_SQUARES}."
            );
        }

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

    pub fn get_bit(&self, index: u32) -> bool {
        match self {
            Bitboard::U64(b) => (b & (1u64 << index)) != 0,
            Bitboard::U256(b) => b.bit(index),
            Bitboard::U1024(b) => b.bit(index),
            Bitboard::U4096(b) => b.bit(index),
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

    pub fn bit_indices(&self) -> Vec<u32> {
        let mut indices = Vec::with_capacity(self.count_ones() as usize);
        let mut temp = match self {
            Bitboard::U64(b) => Bitboard::U64(*b),
            Bitboard::U256(b) => Bitboard::U256(*b),
            Bitboard::U1024(b) => Bitboard::U1024(*b),
            Bitboard::U4096(b) => Bitboard::U4096(*b),
        };

        let mut index = 0u32;
        loop {
            match &temp {
                Bitboard::U64(b) if *b == 0 => break,
                Bitboard::U256(b) if *b == U256::ZERO => break,
                Bitboard::U1024(b) if *b == U1024::ZERO => break,
                Bitboard::U4096(b) if *b == U4096::ZERO => break,
                _ => {}
            }

            let is_zero = match &temp {
                Bitboard::U64(b) => ((*b) & 1) == 0,
                Bitboard::U256(b) => !b.bit(0),
                Bitboard::U1024(b) => !b.bit(0),
                Bitboard::U4096(b) => !b.bit(0),
            };

            if !is_zero {
                indices.push(index);
            }

            match &mut temp {
                Bitboard::U64(b) => *b >>= 1,
                Bitboard::U256(b) => *b >>= 1,
                Bitboard::U1024(b) => *b >>= 1,
                Bitboard::U4096(b) => *b >>= 1,
            }
            index += 1;
        }

        indices
    }

    pub fn lsb(&self) -> u32 {
        match self {
            Bitboard::U64(b) => b.trailing_zeros(),
            Bitboard::U256(b) => b.trailing_zeros(),
            Bitboard::U1024(b) => b.trailing_zeros(),
            Bitboard::U4096(b) => b.trailing_zeros(),
        }
    }
}

impl BitAnd for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bitboard::U64(a), Bitboard::U64(b)) => Bitboard::U64(a & b),
            (Bitboard::U256(a), Bitboard::U256(b)) => Bitboard::U256(a & b),
            (Bitboard::U1024(a), Bitboard::U1024(b)) => Bitboard::U1024(a & b),
            (Bitboard::U4096(a), Bitboard::U4096(b)) => Bitboard::U4096(a & b),
            _ => panic!("Bitboard types must match for bitwise AND operation"),
        }
    }
}

impl BitOr for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bitboard::U64(a), Bitboard::U64(b)) => Bitboard::U64(a | b),
            (Bitboard::U256(a), Bitboard::U256(b)) => Bitboard::U256(a | b),
            (Bitboard::U1024(a), Bitboard::U1024(b)) => Bitboard::U1024(a | b),
            (Bitboard::U4096(a), Bitboard::U4096(b)) => Bitboard::U4096(a | b),
            _ => panic!("Bitboard types must match for bitwise OR operation"),
        }
    }
}

impl BitXor for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bitboard::U64(a), Bitboard::U64(b)) => Bitboard::U64(a ^ b),
            (Bitboard::U256(a), Bitboard::U256(b)) => Bitboard::U256(a ^ b),
            (Bitboard::U1024(a), Bitboard::U1024(b)) => Bitboard::U1024(a ^ b),
            (Bitboard::U4096(a), Bitboard::U4096(b)) => Bitboard::U4096(a ^ b),
            _ => panic!("Bitboard types must match for bitwise XOR operation"),
        }
    }
}

impl Not for Bitboard {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Bitboard::U64(b) => Bitboard::U64(!b),
            Bitboard::U256(b) => Bitboard::U256(!b),
            Bitboard::U1024(b) => Bitboard::U1024(!b),
            Bitboard::U4096(b) => Bitboard::U4096(!b),
        }
    }
}

impl BitAnd for &Bitboard {
    type Output = Bitboard;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bitboard::U64(a), Bitboard::U64(b)) => Bitboard::U64(a & b),
            (Bitboard::U256(a), Bitboard::U256(b)) => Bitboard::U256(a & b),
            (Bitboard::U1024(a), Bitboard::U1024(b)) => Bitboard::U1024(a & b),
            (Bitboard::U4096(a), Bitboard::U4096(b)) => Bitboard::U4096(a & b),
            _ => panic!("Bitboard types must match for bitwise AND operation"),
        }
    }
}

impl BitOr for &Bitboard {
    type Output = Bitboard;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bitboard::U64(a), Bitboard::U64(b)) => Bitboard::U64(a | b),
            (Bitboard::U256(a), Bitboard::U256(b)) => Bitboard::U256(a | b),
            (Bitboard::U1024(a), Bitboard::U1024(b)) => Bitboard::U1024(a | b),
            (Bitboard::U4096(a), Bitboard::U4096(b)) => Bitboard::U4096(a | b),
            _ => panic!("Bitboard types must match for bitwise OR operation"),
        }
    }
}

impl BitXor for &Bitboard {
    type Output = Bitboard;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bitboard::U64(a), Bitboard::U64(b)) => Bitboard::U64(a ^ b),
            (Bitboard::U256(a), Bitboard::U256(b)) => Bitboard::U256(a ^ b),
            (Bitboard::U1024(a), Bitboard::U1024(b)) => Bitboard::U1024(a ^ b),
            (Bitboard::U4096(a), Bitboard::U4096(b)) => Bitboard::U4096(a ^ b),
            _ => panic!("Bitboard types must match for bitwise XOR operation"),
        }
    }
}

impl Not for &Bitboard {
    type Output = Bitboard;

    fn not(self) -> Self::Output {
        match self {
            Bitboard::U64(b) => Bitboard::U64(!b),
            Bitboard::U256(b) => Bitboard::U256(!b),
            Bitboard::U1024(b) => Bitboard::U1024(!b),
            Bitboard::U4096(b) => Bitboard::U4096(!b),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Board {
    pub ranks: u8,
    pub files: u8,
    pub bitboard: Bitboard,
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

        let size = (ranks as u16) * (files as u16);
        Board {
            ranks,
            files,
            bitboard: Bitboard::new(size),
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

        self.bitboard.set_bit(index, true);
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

        self.bitboard.set_bit(index, false);
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

        self.bitboard.get_bit(index)
    }

    pub fn lsb(&self) -> u32 {
        self.bitboard.lsb()
    }

    pub fn count_bits(&self) -> u32 {
        self.bitboard.count_ones()
    }

    pub fn bit_indices(&self) -> Vec<u32> {
        self.bitboard.bit_indices()
    }
}

impl BitAnd for Board {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Board {
            bitboard: self.bitboard & rhs.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}

impl BitOr for Board {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Board {
            bitboard: self.bitboard | rhs.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}

impl BitXor for Board {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Board {
            bitboard: self.bitboard ^ rhs.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}

impl Not for Board {
    type Output = Self;

    fn not(self) -> Self::Output {
        Board {
            bitboard: !self.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}

impl BitAnd for &Board {
    type Output = Board;

    fn bitand(self, rhs: Self) -> Self::Output {
        Board {
            bitboard: &self.bitboard & &rhs.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}

impl BitOr for &Board {
    type Output = Board;

    fn bitor(self, rhs: Self) -> Self::Output {
        Board {
            bitboard: &self.bitboard | &rhs.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}

impl BitXor for &Board {
    type Output = Board;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Board {
            bitboard: &self.bitboard ^ &rhs.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}

impl Not for &Board {
    type Output = Board;

    fn not(self) -> Self::Output {
        Board {
            bitboard: !&self.bitboard,
            ranks: self.ranks,
            files: self.files,
        }
    }
}