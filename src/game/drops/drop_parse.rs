use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

use crate::{constants::{NO_PIECE, NULL_DROP}, game::{moves::move_parse::generate_move_vectors, representations::{drop::{DropAllower, DropSet, DropStopper}, piece::Piece, state::State, vector::Leg}}, leg, p_index, x, y};


lazy_static! {
    pub static ref DROP_PATTERN: Regex =
        Regex::new(r"^([kfde]*)@(.+)~(.+)@(?:(.+)~(.+))?$").unwrap();
    pub static ref CHAIN_PATTERN: Regex =
    Regex::new(r"([^-]+)").unwrap();
}

/// the Cheesy Drop Notation (CDN) is as follows:
///
/// [modifiers][allower multi leg]~[pieces]@[stoppers multi leg]~[pieces]
///
/// The multi legs part wont be processed as a whole but each le will be
/// processed. so #-W~P-P would match a pawn on the dropping square and a pawn
/// on each W square. A drop is legal if all of the allowers are met and none
/// of the stoppers are met.
///
/// The modifiers are as follows:
/// - k : if set, this drop cannot deliver checkmate, otherwise, it can.
/// - f : if set, this drop can be used to drop to forbidden squares
/// - d : if set, this drop will remove every non-royal piece detected by the
///   allowers
/// - e : this drop is from the enemy's hand not our hand
///
/// Pieces are the chars of the pieces that are relevant to the allowers and
/// stoppers. * means all pieces including no piece. ? means no piece.
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

        let (allowers, allower_pieces) =
        match (captures.get(2), captures.get(3)) {
            (Some(a), Some(p)) => (a.as_str(), p.as_str()),
            (None, None) => ("", ""),
            _ => panic!(
                concat!(
                    "Invalid drop format for piece: {} ",
                    "allowers and allower_pieces must ",
                    "both be present or both absent"
                ),
                piece.name
            ),
        };

        let allowers_pieces = CHAIN_PATTERN
            .captures_iter(allower_pieces)
            .map(|cap| cap.get(1).unwrap().as_str())
            .collect::<Vec<&str>>();
        let allowers_vecs = if allowers.is_empty() {
            Vec::new()
        } else {
            CHAIN_PATTERN
                .captures_iter(allowers)
                .enumerate()
                .flat_map(|(idx, cap)| {
                    let compound = cap.get(1).unwrap().as_str();
                    generate_move_vectors(compound, state)
                        .into_iter()
                        .map(move |vec| (idx, vec))
                })
                .collect()
        };

        #[cfg(debug_assertions)]
        println!(
            "[DEBUG] Parsed drop for piece {}: \
            move_result = {}, allowers = {:#?}",
            piece.name, move_result, allowers_vecs
        );

        let allower_result = allowers_vecs.iter().map(
            |(index, multi_leg_vector)| {
                let leg = leg!(multi_leg_vector[0]);

                let x = x!(leg) as u16 & 0xFF;
                let y = y!(leg) as u16 & 0xFF;

                let mut piece_set = HashSet::new();
                for piece_char in allowers_pieces[*index].chars() {
                    let piece_index =
                    if piece_char == '?' {NO_PIECE as u16}
                    else {state.piece_char_map[&piece_char] as u16};
                    piece_set.insert(piece_index as u8);
                }

                ((y << 8) | x, piece_set)
            }
        ).collect::<DropAllower>();

        #[cfg(debug_assertions)]
        println!(
            "[DEBUG] Encoded allowers for piece {}: {:?}",
            piece.name, allower_result
        );

        let (stoppers, stopper_pieces) =
        match (captures.get(4), captures.get(5)) {
            (Some(s), Some(p)) => (s.as_str(), p.as_str()),
            (None, None) => ("", ""),
            _ => panic!(
            concat!(
                "Invalid drop format for piece: {} ",
                "stoppers and stopper_pieces must ",
                "both be present or both absent"
            ),
            piece.name
            ),
        };
        let stoppers_pieces = CHAIN_PATTERN
            .captures_iter(stopper_pieces)
            .map(|cap| cap.get(1).unwrap().as_str())
            .collect::<Vec<&str>>();
        let stoppers_vecs = if stoppers.is_empty() {
            Vec::new()
        } else {
            CHAIN_PATTERN
                .captures_iter(stoppers)
                .enumerate()
                .flat_map(|(idx, cap)| {
                    let compound = cap.get(1).unwrap().as_str();
                    generate_move_vectors(compound, state)
                        .into_iter()
                        .map(move |vec| (idx, vec))
                })
                .collect()
        };

        #[cfg(debug_assertions)]
        println!(
            "[DEBUG] Parsed drop for piece {}: \
            move_result = {}, stoppers = {:#?}",
            piece.name, move_result, stoppers_vecs
        );

        let stopper_result = stoppers_vecs.iter().map(
            |(index, multi_leg_vector)| {
            let leg = leg!(multi_leg_vector[0]);

            let x = x!(leg) as u16 & 0xFF;
            let y = y!(leg) as u16 & 0xFF;

            let mut piece_set = HashSet::new();
            for piece_char in stoppers_pieces[*index].chars() {
                let piece_index =
                if piece_char == '?' {NO_PIECE as u16}
                else {state.piece_char_map[&piece_char] as u16};
                piece_set.insert(piece_index as u8);
            }

            ((y << 8) | x, piece_set)
            }
        ).collect::<DropStopper>();

        #[cfg(debug_assertions)]
        println!(
            "[DEBUG] Encoded stoppers for piece {}: {:?}",
            piece.name, stopper_result
        );

        drop_set.push((move_result, allower_result, stopper_result));
    }

    drop_set
}