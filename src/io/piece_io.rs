//! # piece_io.rs
//!
//! Implements piece formatting and visualization functions.
//!
//! This file contains functionality for formatting piece information
//! into human-readable tables with Unicode box-drawing characters.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026

use crate::game::representations::piece::Piece;
use crate::constants::*;

/// Formats a piece as a column vector with 9 rows of information.
///
/// Returns a String formatted as a table column with box-drawing characters:
/// - Row 0: Piece name
/// - Row 1: Piece index
/// - Row 2: Piece symbol
/// - Row 3: Piece color ("White" or "Black")
/// - Row 4: Piece value
/// - Row 5: Royal status ("*" if royal, empty otherwise)
/// - Row 6: Promotion ability ("*" if can promote, empty otherwise)
/// - Row 7: Major piece status ("*" if major, empty otherwise)
/// - Row 8: Placeholder for promotion from (handled by format_piece_types)
pub fn format_piece(piece: &Piece) -> String {
    let mut rows = Vec::with_capacity(9);

    rows.push(piece.name.clone());
    rows.push(piece.index().to_string());
    rows.push(piece.symbol.to_string());
    rows.push(
        if piece.color() == BLACK { "Black" } else { "White" }.to_string()
    );
    rows.push(if piece.value() == u16::MAX {
        "∞".to_string()
    } else {
        piece.value().to_string()
    });
    rows.push(if piece.is_royal() { "*" } else { "" }.to_string());
    rows.push(if piece.can_promote() { "*" } else { "" }.to_string());
    rows.push(if piece.is_major() { "*" } else { "" }.to_string());
    rows.push("-".to_string());                                                 /* Placeholder for "promotion from"   */

    const FIXED_WIDTH: usize = 12;

    let mut result = String::new();
    result.push_str(&format!("┌{}┐\n", "─".repeat(FIXED_WIDTH + 2)));

    for row in rows {
        result.push_str(&format!("│ {:^FIXED_WIDTH$} │\n", row));
    }

    result.push_str(&format!("└{}┘", "─".repeat(FIXED_WIDTH + 2)));

    result
}
