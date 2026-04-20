//! # evaluation.rs
//!
//! Position evaluation logic for alpha-beta search.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 19/04/2026

use crate::*;

/// Evaluates the current position from the side-to-move perspective.
///
/// Evaluation model:
/// - `OPENING`   : opening material delta + opening PST delta.
/// - `ENDGAME`   : endgame material delta + endgame PST delta.
/// - `MIDDLEGAME`: linear interpolation between opening and endgame scores.
///
/// Hot-path notes:
/// - Uses cached per-side material/PST totals from `State`.
/// - Uses a branch-light side-to-move sign (`WHITE => +1`, `BLACK => -1`).
/// - Handles `opening_score == 0` safely during interpolation.
#[inline(always)]
pub fn evaluate_position(state: &State) -> i32 {
    let white = WHITE as usize;
    let black = BLACK as usize;

    let opening_white = state.opening_material[white] as i32;
    let opening_black = state.opening_material[black] as i32;
    let endgame_white = state.endgame_material[white] as i32;
    let endgame_black = state.endgame_material[black] as i32;

    let side_sign = -2 * state.playing as i32 + 1;

    match state.game_phase {
        OPENING => {
            let score_opening = opening_white - opening_black
                + state.opening_pst_bonus[white]
                - state.opening_pst_bonus[black];
            score_opening * side_sign
        }
        ENDGAME => {
            let score_endgame = endgame_white - endgame_black
                + state.endgame_pst_bonus[white]
                - state.endgame_pst_bonus[black];
            score_endgame * side_sign
        }
        MIDDLEGAME => {
            let score_opening = opening_white - opening_black
                + state.opening_pst_bonus[white]
                - state.opening_pst_bonus[black];

            let score_endgame = endgame_white - endgame_black
                + state.endgame_pst_bonus[white]
                - state.endgame_pst_bonus[black];

            let opening_total = opening_white + opening_black;
            let opening_scale = state.opening_score as i32;

            let blended_score = if opening_scale == 0 {
                score_endgame
            } else {
                (score_opening * opening_total
                    + score_endgame * (opening_scale - opening_total))
                    / opening_scale
            };

            blended_score * side_sign
        }
        _ => panic!("Invalid game phase {}", state.game_phase),
    }
}
