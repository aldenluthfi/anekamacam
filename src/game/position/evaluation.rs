//! evaluation.rs
//!
//! Static position evaluation for alpha-beta search.
//!
//! Material values and piece-square tables are derived from variant rules at
//! startup and maintained incrementally by make/undo. Opening and endgame
//! totals are blended by current material phase, with no additional positional
//! terms.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

/// terminal_score!
///
/// Scores a terminal position from side-to-move perspective. A decisive result
/// uses mate-scaled scores so shorter wins and longer losses are preferred.
/// Draws score zero.
///
/// Params:
/// - state: &State -> position whose terminal value is computed
///
/// Return:
/// i32             -> terminal score from side-to-move perspective
#[macro_export]
macro_rules! terminal_score {
    ($state:expr) => {{
        let stm_wins = ($state.termination.game_result == WHITE_WIN
            && $state.playing == WHITE)
            || ($state.termination.game_result == BLACK_WIN
                && $state.playing == BLACK);
        let stm_loses = ($state.termination.game_result == WHITE_WIN
            && $state.playing == BLACK)
            || ($state.termination.game_result == BLACK_WIN
                && $state.playing == WHITE);

        if stm_wins {
            INF - $state.search_ply as i32
        } else if stm_loses {
            -INF + $state.search_ply as i32
        } else {
            0
        }
    }};
}

/// evaluate_position!
///
/// Evaluates current position from side-to-move perspective using only cached
/// material and piece-square-table totals. Opening and setup use opening
/// values, endgame uses endgame values, and middlegame linearly blends both.
///
/// Params:
/// - state: &State -> position to evaluate
///
/// Return:
/// i32             -> score from side-to-move perspective
#[macro_export]
macro_rules! evaluate_position {
    ($state:expr) => {
        hotpath::measure_block!("eval::position", {
            let white = WHITE as usize;
            let black = BLACK as usize;
            let side_sign = -2 * $state.playing as i32 + 1;

            let opening = $state.opening_material[white] as i32
                - $state.opening_material[black] as i32
                + $state.opening_pst_bonus[white]
                - $state.opening_pst_bonus[black];
            let endgame = $state.endgame_material[white] as i32
                - $state.endgame_material[black] as i32
                + $state.endgame_pst_bonus[white]
                - $state.endgame_pst_bonus[black];

            let score = match $state.game_phase {
                OPENING | SETUP => opening,
                ENDGAME => endgame,
                MIDDLEGAME => {
                    let opening_score = $state.statics.opening_score as i32;
                    let endgame_score = $state.statics.endgame_score as i32;
                    let current_score = $state.phase_score as i32;

                    (
                        opening * (current_score - endgame_score)
                            + endgame * (opening_score - current_score)
                    ) / (opening_score - endgame_score)
                }
                _ => panic!("Invalid game phase {}", $state.game_phase),
            };

            score * side_sign
        })
    };
}
