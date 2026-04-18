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

pub fn evaluate_position(state: &State) -> i32 {

    let mut score_opening =
        state.opening_material[WHITE as usize] as i32 -
        state.opening_material[BLACK as usize] as i32;
    let mut score_endgame =
        state.endgame_material[WHITE as usize] as i32 -
        state.endgame_material[BLACK as usize] as i32;

    let material_score =
        state.opening_material[WHITE as usize] as i32 +
        state.opening_material[BLACK as usize] as i32;

    score_opening +=
        state.opening_pst_bonus[WHITE as usize] -
        state.opening_pst_bonus[BLACK as usize];
    score_endgame +=
        state.endgame_pst_bonus[WHITE as usize] -
        state.endgame_pst_bonus[BLACK as usize];

    let result = match state.game_phase {
        OPENING => score_opening,
        ENDGAME => score_endgame,
        MIDDLEGAME => {                                                         /* interpolate for middlegame         */
            (
                score_opening * material_score +
                score_endgame * (state.opening_score as i32 - material_score)
            ) / state.opening_score as i32

        }
        _ => panic!("Invalid game phase {}", state.game_phase),
    };

    result * (-2 * state.playing as i32 + 1)
}
