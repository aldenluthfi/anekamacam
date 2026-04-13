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
        Regex::new(r"^([kfde]*)@(.*@.*)$").unwrap();
}

/// For drops, there are modifiers
///
/// [modifiers]@[CPMN]
///
/// The modifiers are as follows:
/// - k : if set, this drop cannot deliver checkmate, otherwise, it can.
/// - f : if set, this drop can be used to drop to forbidden squares
/// - d : if set, this drop will remove every non-royal piece detected by the
///   allowers
/// - e : this drop uses the enemy's hand rather than our own
pub fn generate_drop_vectors(
    piece: &Piece, state: &State, expr_set: &[String]
) -> DropSet {

    let piece_index = p_index!(piece) as usize;
    let drop_expr = &expr_set[piece_index];

    if drop_expr == NULL_DROP {
        return Vec::new();
    }

    let parts = drop_expr.split('|').collect::<Vec<&str>>();

    let mut drop_set = Vec::new();
    for part in parts {
        let captures = DROP_PATTERN
            .captures(part)
            .unwrap_or_else(|| panic!("Invalid drop format {}", part));

        #[cfg(debug_assertions)]
        println!(
            "[DEBUG] Captured groups for piece {}: {:?}",
            piece.name, captures
        );

        let mut move_result = piece_index as u32;

        let modifiers = captures.get(1).map_or("", |m| m.as_str());

        if modifiers.contains('k') {
            move_result |= 1 << 20;
        }

        if modifiers.contains('f') {
            move_result |= 1 << 21;
        }

        if modifiers.contains('d') {
            move_result |= 1 << 22;
        }

        if modifiers.contains('e') {
            move_result |= 1 << 23;
        }

        let pattern = captures.get(2).unwrap().as_str();
        let (allower_result, stopper_result) = parse_pattern(pattern, state);

        drop_set.push((move_result, allower_result,stopper_result));
    }

    drop_set
}