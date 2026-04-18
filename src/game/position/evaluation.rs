//! # evaluation.rs
//!
//! Placeholder module for position evaluation logic.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 29/01/2026

use crate::*;

/// Evaluates the current position from the side-to-move perspective.
///
/// The evaluation is phase-aware:
/// - `OPENING` uses opening material + opening PST terms.
/// - `ENDGAME` uses endgame material + endgame PST terms.
/// - `MIDDLEGAME` linearly interpolates between the two scores using the
///   current opening-material total against `opening_score`.
///
/// Positive values favor the side to move, negative values favor the opponent.
#[inline(always)]
pub fn evaluate_position(state: &State) -> i32 {
    let white = WHITE as usize;
    let black = BLACK as usize;

    let opening_material_white = state.opening_material[white] as i32;
    let opening_material_black = state.opening_material[black] as i32;
    let endgame_material_white = state.endgame_material[white] as i32;
    let endgame_material_black = state.endgame_material[black] as i32;

    let score_opening = opening_material_white - opening_material_black
        + state.opening_pst_bonus[white]
        - state.opening_pst_bonus[black];

    let score_endgame = endgame_material_white - endgame_material_black
        + state.endgame_pst_bonus[white]
        - state.endgame_pst_bonus[black];

    let material_score = opening_material_white + opening_material_black;

    let blended_score = match state.game_phase {
        OPENING => score_opening,
        ENDGAME => score_endgame,
        MIDDLEGAME => {
            (score_opening * material_score
                + score_endgame * (state.opening_score as i32 - material_score))
                / state.opening_score as i32
        }
        _ => panic!("Invalid game phase {}", state.game_phase),
    };

    blended_score * (-2 * state.playing as i32 + 1)
}
