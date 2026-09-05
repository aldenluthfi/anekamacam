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
/// Anchors a piece's compiled drop templates on one target square, stamping
/// that square into each packed word and re-reading every pattern offset from
/// where it now points. Offsets are mirrored by the dropped piece's colour
/// first, so the pattern is judged in the orientation it will be matched in.
///
/// The two halves of a pattern fall off the board differently, and the
/// difference is not a shortcut but the meaning of each half:
///
/// - an allower that points off the board asks for a piece on a square that
///   does not exist, so the drop can never be legal here and is dropped
/// - a stopper that points off the board vetoes on a square that does not
///   exist, so it can never fire and is dropped while the drop survives
///
/// A square in the piece's forbidden zone returns nothing at all, no pattern
/// being consulted about a placement the variant has already refused.
///
/// Params:
/// - piece            : &Piece     -> piece type the drops belong to
/// - square_index     : u32        -> target square being precomputed
/// - state            : &State     -> board dimensions and forbidden zones
/// - piece_setup_drops: &[DropSet] -> compiled drops, one set per piece
///
/// Return:
/// DropSet                         -> drops playable onto this square
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

/*----------------------------------------------------------------------------*\
                              DROP MOVE GENERATION
\*----------------------------------------------------------------------------*/

/// generate_drop_list!
///
/// Generates every drop of one held piece the position allows. A drop has no
/// origin to walk from, so this sweeps target squares instead of legs, and at
/// each one reads the templates `generate_relevant_drops` already anchored
/// there. A square with no template left is a square this piece can never be
/// dropped on, whatever stands around it.
///
/// Which table is read depends on the phase: a variant with a setup phase
/// places its army out of `relevant_setup` and drops captures out of
/// `relevant_drops` afterwards, the same mechanism answering both.
///
/// The checkmate ban is inverted on the way onto the move. The table stores
/// what the variant wrote — this drop may not mate — and the generated move
/// carries the permission the search asks for, so `illegal_mating_drop!` can
/// read a move without knowing which table it came from.
///
/// Params:
/// - piece: &Piece         -> piece type to drop from hand
/// - state: &State         -> current position providing hand and occupancy
/// - out  : &mut Vec<Move> -> output list receiving encoded drop moves
///
/// Notes:
/// The pattern scan is spelled out here rather than delegated to
/// [`match_pattern!`], which takes a `&Pattern` and would have to be handed
/// the halves this loop already holds. Both walks mirror by colour and index
/// unchecked for the same reason: precomputation kept only the offsets that
/// land on the board from this square.
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
                enc_end!(encoded_move, square as u128);
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
