//! # constants.rs
//!
//! Defines game-wide constants and configuration values.
//!
//! This file contains constant definitions for board dimensions, piece colors,
//! castling rights, file/rank labels, and other game-related configuration
//! values. These constants are used throughout the codebase to ensure
//! consistency and allow for easy modification of game parameters.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026

pub const MAX_SQUARES: usize = 2048;
pub const MAX_PIECES: usize = 255;

pub const RNG_SEED: u64 = 0xDEADBEEFCAFEBABE;

pub const A: u8 = 0;
pub const B: u8 = 1;
pub const C: u8 = 2;
pub const D: u8 = 3;
pub const E: u8 = 4;
pub const F: u8 = 5;
pub const G: u8 = 6;
pub const H: u8 = 7;
pub const I: u8 = 8;
pub const J: u8 = 9;
pub const K: u8 = 10;
pub const L: u8 = 11;
pub const M: u8 = 12;
pub const N: u8 = 13;
pub const O: u8 = 14;
pub const P: u8 = 15;
pub const Q: u8 = 16;
pub const R: u8 = 17;
pub const S: u8 = 18;
pub const T: u8 = 19;
pub const U: u8 = 20;
pub const V: u8 = 21;
pub const W: u8 = 22;
pub const X: u8 = 23;
pub const Y: u8 = 24;
pub const Z: u8 = 25;

pub const WHITE : u8 = 0;
pub const BLACK : u8 = 1;

pub const WK_CASTLE : u8 = 0b0001;
pub const WQ_CASTLE : u8 = 0b0010;
pub const BK_CASTLE : u8 = 0b0100;
pub const BQ_CASTLE : u8 = 0b1000;

pub const NO_PIECE: u8 = u8::MAX;
pub const NO_EN_PASSANT: u32 = u32::MAX;

pub const QUIET_MOVE: u128 = 0;
pub const SINGLE_CAPTURE_MOVE: u128 = 1;
pub const MULTI_CAPTURE_MOVE: u128 = 2;