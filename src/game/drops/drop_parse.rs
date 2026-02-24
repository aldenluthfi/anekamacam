use lazy_static::lazy_static;
use regex::Regex;

use crate::{constants::{NULL_DROP}, game::{representations::{drop::DropSet, piece::Piece, state::State, vector::parse_pattern}}, p_index};


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
/// - e : this drop is from the enemy's hand not our hand
pub fn generate_drop_vectors(
    piece: &Piece, state: &State, setup: bool
) -> DropSet {

    let piece_index = p_index!(piece) as usize;
    let drop_expr = if setup {&piece.setup} else {&piece.drop};

    if drop_expr == NULL_DROP {
        return Vec::new();
    }

    let parts = drop_expr.split('|').collect::<Vec<&str>>();

    let mut drop_set = Vec::new();
    for part in parts {
        let captures = DROP_PATTERN
            .captures(part)
            .unwrap_or_else(|| panic!("Invalid drop format {}", piece.drop));

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