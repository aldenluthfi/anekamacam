//! # drop_parse.rs
//!
//! Parses drop expressions into internal drop vectors and modifiers.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 29/01/2026

use crate::*;

lazy_static! {
    pub static ref DROP_PATTERN: Regex =
        Regex::new(r"^([kf]*)@(.*@.*)$").unwrap_or_else(|e| {
            panic!("Failed to compile DROP_PATTERN regex: {e}")
        });
}

/// Parses drop expressions into internal drop vectors and modifiers.
///
/// For drops, there are modifiers.
///
/// [modifiers]@[CPMN]
///
/// The modifiers are as follows:
/// - k : if set, this drop cannot deliver checkmate, otherwise, it can.
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
