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
use crate::*;

/// Formats a piece as a column vector with 9-10 rows of information.
///
/// Returns a String formatted as a table column with box-drawing characters:
/// - Row 0: Piece name
/// - Row 1: Piece index
/// - Row 2: Piece symbol
/// - Row 3: Piece color ("White" or "Black")
/// - Row 4: Opening piece value
/// - Row 5: Endgame piece value
/// - Row 6: Royal status ("*" if royal, empty otherwise)
/// - Row 7: Promotion ability ("*" if can promote, empty otherwise)
/// - Row 8: Major piece status ("*" if major, empty otherwise)
/// - Row 9: Piece count limit (if applicable, otherwise non-existent)
/// - Row 10: Can Promote (if applicable, otherwise non-existent)
/// - Row 11: Promotes To (if applicable, otherwise non-existent)
/// - Row 12: Promotes From (if applicable, otherwise non-existent)
///
pub fn format_piece(piece: &Piece, state: &State) -> String {
    let mut rows = Vec::with_capacity(9);

    rows.push(piece.name.clone());
    rows.push(p_index!(piece).to_string());
    rows.push(piece.char.to_string());
    rows.push(
        if p_color!(piece) == BLACK {
            "Black"
        } else {
            "White"
        }
        .to_string(),
    );
    rows.push(p_ovalue!(piece).to_string());
    rows.push(p_evalue!(piece).to_string());
    rows.push(
        if p_is_royal!(piece) {
            "*"
        } else {
            ""
        }
        .to_string(),
    );
    rows.push(
        if p_is_major!(piece) {
            "*"
        } else {
            ""
        }
        .to_string(),
    );
    if count_limits!(state) {
        rows.push(state.piece_limit[p_index!(piece) as usize].to_string());
    }
    if promotions!(state) {
        rows.push(
            if p_can_promote!(piece) {
                "*"
            } else {
                ""
            }
            .to_string(),
        );

        rows.push(
            if piece.promotions.is_empty() {
                "-".to_string()
            } else {
                piece
                    .promotions
                    .iter()
                    .map(|&idx| state.pieces[idx as usize].char.to_string())
                    .collect::<Vec<String>>()
                    .join("")
            },
        );

        rows.push(
            if !state.piece_demotion_map.contains_key(&p_index!(piece)) {
                "-".to_string()
            } else {
                state
                    .piece_demotion_map
                    .get(&p_index!(piece))
                    .unwrap()
                    .iter()
                    .filter(|&&idx| idx != p_index!(piece))
                    .map(|&idx| state.pieces[idx as usize].char.to_string())
                    .collect::<Vec<String>>()
                    .join("")
            },
        );
    }

    const FIXED_WIDTH: usize = 12;

    let mut result = String::new();
    result.push_str(&format!("┌{}┐\n", "─".repeat(FIXED_WIDTH + 2)));

    for row in rows {
        result.push_str(&format!("│ {:^FIXED_WIDTH$} │\n", row));
    }

    result.push_str(&format!("└{}┘", "─".repeat(FIXED_WIDTH + 2)));

    result
}
