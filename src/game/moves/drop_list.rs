//! drop_list.rs
//!
//! Generates legal drop moves and relevant drop templates.
//!
//! Shogi-family variants let a captured piece re-enter from hand. This file
//! turns the compiled drop templates into concrete, legal placements: it
//! prunes templates against board bounds and forbidden zones per square, and
//! at generation time enforces the drop flags, hand counts, and the
//! allower/stopper neighbourhood patterns each placement requires.
//!
//! Created: 18/02/2026
//! Author : Alden Luthfi

use crate::*;

/// generate_relevant_drops
///
/// Relocates the compiled drop patterns for one target square.
///
/// It rejects forbidden or out-of-bounds placements and mirrors offsets for
/// the dropped piece's color.
///
/// Params:
/// - piece            : &Piece     -> piece type the drops belong to
/// - square_index     : u32        -> target square being precomputed
/// - state            : &State     -> board dimensions and forbidden zones
/// - piece_setup_drops: &[DropSet] -> compiled drops, one set per piece
///
/// Return:
///
/// DropSet
/// drops playable onto this square, with square encoded and out-of-board
/// stoppers pruned
pub fn generate_relevant_drops(
    piece: &Piece,
    square_index: u32,
    state: &State,
    piece_setup_drops: &[DropSet],
) -> DropSet {
    let piece_index = p_index!(piece) as usize;
    let piece_color = p_color!(piece) as usize;
    let drops = &piece_setup_drops[piece_index];

    if get!(state.statics.forbidden_zones[piece_index], square_index) {
        return DropSet::new();
    }

    drops
        .iter()
        .filter_map(|drop| {
            let new_drop_move = drop.0 | (square_index << 8);
            let mut new_drop_stoppers = Vec::new();
            let mut new_drop_allowers = Vec::new();

            let file = square_index as i32 % state.statics.files as i32;
            let rank = square_index as i32 / state.statics.files as i32;

            for allower in drop.1.0.iter() {
                let x = x!(allower.0) * (-2 * piece_color as i8 + 1);
                let y = y!(allower.0) * (-2 * piece_color as i8 + 1);

                let check_x = file + x as i32;
                let check_y = rank + y as i32;

                if check_x < 0
                    || check_x >= state.statics.files as i32
                    || check_y < 0
                    || check_y >= state.statics.ranks as i32
                {
                    return None;
                }

                new_drop_allowers.push(allower.clone());
            }

            for stopper in drop.1.1.iter() {
                let x = x!(stopper.0) * (-2 * piece_color as i8 + 1);
                let y = y!(stopper.0) * (-2 * piece_color as i8 + 1);

                let check_x = file + x as i32;
                let check_y = rank + y as i32;

                if check_x >= 0
                    && check_x < state.statics.files as i32
                    && check_y >= 0
                    && check_y < state.statics.ranks as i32
                {
                    new_drop_stoppers.push(stopper.clone());
                }
            }

            Some((new_drop_move, (new_drop_allowers, new_drop_stoppers)))
        })
        .collect()
}

/// generate_drop_list!
///
/// Generates every legal drop move for one held piece.
///
/// It enforces hand ownership, count limits, drop flags, and CPMN constraints.
/// Unlike movement-vector generation, it starts from a precomputed placement
/// template rather than an on-board piece and directional legs.
///
/// Params:
/// - piece: &Piece         -> piece type to drop from hand
/// - state: &State         -> current position providing hand and occupancy
/// - out  : &mut Vec<Move> -> output list receiving encoded drop moves
#[macro_export]
macro_rules! generate_drop_list {
    ($piece:expr, $state:expr, $out:expr) => {{
        let board_size = $state.statics.board_size as u32;
        let index = p_index!($piece) as usize;
        let color = p_color!($piece) as usize;

        for square in 0..board_size {

            if $state.piece_in_hand[color][index] == 0 {
                break;
            }

            let drops = if $state.game_phase == SETUP {
                &$state.statics.relevant_setup[
                    index * board_size as usize + square as usize
                ]
            } else {
                &$state.statics.relevant_drops[
                    index * board_size as usize + square as usize
                ]
            };

            if drops.is_empty() {
                continue;
            }

            'drop_loop: for drop in drops {
                let mut encoded_move = Move::default();

                let drop_k = drop_k!(drop);

                enc_move_type!(encoded_move, DROP_MOVE);
                enc_piece!(encoded_move, index as u128);
                enc_start!(encoded_move, square as u128);
                enc_can_checkmate!(encoded_move, !drop_k as u128);

                let drop_allowers = &drop.1.0;
                let drop_stoppers = &drop.1.1;

                let file = square % $state.statics.files as u32;
                let rank = square / $state.statics.files as u32;

                for allower in drop_allowers.iter() {
                    let ax = x!(allower.0) as i32
                        * (-2 * color as i32 + 1);
                    let ay = y!(allower.0) as i32
                        * (-2 * color as i32 + 1);
                    let allower_pieces = &allower.1;

                    let check_x = file as i32 + ax;
                    let check_y = rank as i32 + ay;
                    let check_index = (
                        check_y * $state.statics.files as i32 + check_x
                    ) as usize;

                    let piece_check = $state.main_board[check_index];

                    if !allower_pieces.contains(piece_check) {
                        continue 'drop_loop;
                    }
                }

                for stopper in drop_stoppers.iter() {
                    let sx = x!(stopper.0) as i32
                        * (-2 * color as i32 + 1);
                    let sy = y!(stopper.0) as i32
                        * (-2 * color as i32 + 1);
                    let stopper_pieces = &stopper.1;

                    let check_x = file as i32 + sx;
                    let check_y = rank as i32 + sy;
                    let check_index = (
                        check_y * $state.statics.files as i32 + check_x
                    ) as usize;

                    let piece_check = $state.main_board[check_index];

                    if stopper_pieces.contains(piece_check) {
                        continue 'drop_loop;
                    }
                }

                $out.push(encoded_move);
            }
        }
    }};
}
