//! # piece.rs
//!
//! Defines piece representation and properties.
//!
//! This file contains the implementation of the `Piece` struct, which represents
//! a chess piece type with its movement patterns, display characters for both
//! colors, and special properties such as royal status. The movement string uses
//! Cheesy King Notation to describe how the piece moves on the board.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026

pub struct Piece {
    pub name: String,
    pub movement: String,

    pub white_char: char,
    pub black_char: char,

    pub royal: bool,
}