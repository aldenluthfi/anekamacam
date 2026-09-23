//! drop_parse.rs
//!
//! Parses drop expressions into packed drop templates.
//!
//! A drop expression has optional flags and a CPMN pattern. The pattern
//! tells where a piece in hand can go on the board. This file compiles each
//! `|` branch once at load time, so the move generator does not read text.
//!
//! Created: 29/01/2026
//! Author : Alden Luthfi

use crate::*;

lazy_static! {
    /// DROP_PATTERN
    ///
    /// Regex that splits one drop branch at its first `@`. The CPMN body
    /// must also contain an `@`, between its allower and stopper halves.
    ///
    /// - group 1 -> flag prefix, any sequence of `k` and `f`
    /// - group 2 -> CPMN body, compiled by `parse_pattern`
    ///
    static ref DROP_PATTERN: Regex =
        Regex::new(r"^([kf]*)@(.*@.*)$").unwrap_or_else(|e| {
            panic!("Failed to compile DROP_PATTERN regex: {e}")
        });
}

/*----------------------------------------------------------------------------*\
                          DROP EXPRESSION COMPILATION
\*----------------------------------------------------------------------------*/

/// generate_drop_vectors
///
/// Compiles the drop expression of a piece into packed drop templates, one
/// for each `|` branch. Each branch has this form:
///
/// ```text
/// [modifiers]@[CPMN]
/// ```
///
/// The CPMN body is the pattern that the target square must match. The
/// modifiers give the rules that a pattern cannot give:
///
/// - k -> the drop must not give checkmate
///
/// Each template is a [`DropMove`] with the piece index and the flags. The
/// square bits stay zero. `generate_relevant_drops` sets the square later.
///
/// Params:
/// - piece   : &Piece    -> piece type to compile
/// - state   : &State    -> piece dictionary and board dimensions
/// - expr_set: &[String] -> drop expressions, one for each piece
///
/// Return:
/// DropSet               -> one (drop, pattern) pair for each `|` branch
///
/// Notes:
/// The parser accepts an `f` flag but ignores it. A branch that does not
/// match [`DROP_PATTERN`] causes a panic at load time.
///
pub fn generate_drop_vectors(
    piece: &Piece,
    state: &State,
    expr_set: &[String],
) -> DropSet {
    let piece_index = p_index!(piece) as usize;
    let drop_expr = &expr_set[piece_index];

    let parts = drop_expr.split('|').collect::<Vec<&str>>();

    let mut drop_set = Vec::new();
    for part in parts {
        let captures = DROP_PATTERN
            .captures(part)
            .unwrap_or_else(|| panic!("Invalid drop format {}", part));


        log_4!(
            "Captured groups for piece {}: {:?}",
            piece.name, captures
        );

        let mut move_result = piece_index as u32;

        let modifiers = captures.get(1).map_or("", |m| m.as_str());

        if modifiers.contains('k') {
            move_result |= 1 << 20;
        }

        let pattern = captures.get(2).unwrap_or_else(|| {
            panic!("Drop pattern missing matcher body in: {}", part)
        }).as_str();

        drop_set.push((move_result, parse_pattern(pattern, state)));
    }

    drop_set
}
