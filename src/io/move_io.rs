//! # move_io.rs
//!
//! Implements move formatting, move parsing, and interactive move debugging.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 04/02/2026

use crate::*;

/*----------------------------------------------------------------------------*\
                          MOVE STRING REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Formats a move into the engine's canonical text representation.
///
/// Canonical grammar:
/// - `[drop_piece]@` (optional drop prefix)
/// - `[start]` (always present)
/// - `:[end]` for quiet and multi-capture moves
/// - `*[capture_square]` for capture segments (single or repeated)
/// - `=[promotion_piece]` for promotions
///
/// Effective shape:
/// `[drop_piece]@[start]:[end]*[captured_1]...*[captured_n]=[promotion]`
///
/// The resulting string is used for user-facing display and as the matching
/// key for `parse_move` when validating textual input.
pub fn format_move(mv: &Move, state: &State) -> String {
    let mut move_str = String::new();

    if mv == &null_move() {
        return "null".to_string();
    }

    let move_type = move_type!(mv);

    if move_type == DROP_MOVE {
        let drop_piece = piece!(mv) as usize;
        let piece_char = state.pieces[drop_piece].char;

        move_str.push_str(&format!("{}@", piece_char));
    }

    let start = start!(mv);
    let start_str = format_square(start as Square, state);

    move_str.push_str(&start_str);

    if move_type == QUIET_MOVE {
        let end = end!(mv);
        let end_str = format_square(end as Square, state);

        move_str.push_str(&format!(":{}", end_str));
    }

    if move_type == SINGLE_CAPTURE_MOVE {
        let end = end!(mv);
        let capt = captured_square!(mv);

        if capt != end {
            let end_str = format_square(end as Square, state);
            let capt_str = format_square(capt as Square, state);

            move_str.push_str(&format!(":{}*{}", end_str, capt_str));
        } else {
            let end_str = format_square(end as Square, state);
            move_str.push_str(&format!("*{}", end_str));
        }
    }

    if !mv.1.is_empty() {
        let end = end!(mv);
        let end_str = format_square(end as Square, state);

        move_str.push_str(&format!(":{}", end_str));

        for capture in mv.1.iter() {
            let capt_sq = multi_move_captured_square!(capture);
            let capt_sq_str = format_square(capt_sq as Square, state);

            move_str.push_str(&format!("*{}", capt_sq_str));
        }
    }

    if promotion!(mv) {
        let promo_piece = promoted!(mv) as usize;
        let promo_char = state.pieces[promo_piece].char;

        move_str.push_str(&format!("={}", promo_char));
    }

    move_str
}

/// Parses a textual move by matching against generated legal moves.
///
/// The input is compared to `format_move` output for each candidate and
/// returns the first exact match after trimming whitespace.
pub fn parse_move(move_str: &str, state: &State) -> Option<Move> {
    let all_moves = generate_all_moves_and_drops(state);

    all_moves
        .into_iter()
        .find(|mv| format_move(mv, state).trim() == move_str.trim())
}

pub fn format_move_history(state: &State) -> String {
    let mut history_strings = Vec::new();

    for snap in state.history.iter() {
        history_strings.push(format_move(&snap.move_ply, state));
    }

    let mut result = String::new();

    for (i, move_str) in history_strings.iter().enumerate() {
        if i % 2 == 0 {
            result.push_str(&format!("{}. {}", (i / 2) + 1, move_str));
        } else {
            result.push_str(&format!(" {}\n", move_str));
        }
    }

    result.trim().to_string()
}

