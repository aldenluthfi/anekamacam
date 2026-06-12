//! # evaluation.rs
//!
//! Position evaluation logic for alpha-beta search.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 19/04/2026

/// Evaluates the current position from the side-to-move perspective.
///
/// Evaluation model:
/// - `OPENING` | `SETUP`   : opening material delta + opening PST delta.
/// - `ENDGAME`             : endgame material delta + endgame PST delta.
/// - `MIDDLEGAME`          : linear interpolation between opening and endgame.
///
/// Hot-path notes:
/// - Uses cached per-side material/PST totals from `State`.
/// - Uses a branch-light side-to-move sign (`WHITE => +1`, `BLACK => -1`).
/// - Handles `opening_score == 0` safely during interpolation.
#[macro_export]
macro_rules! evaluate_position {
    ($state:expr) => {{
        let white = WHITE as usize;
        let black = BLACK as usize;

        let opening_white = $state.opening_material[white] as i32;
        let opening_black = $state.opening_material[black] as i32;
        let endgame_white = $state.endgame_material[white] as i32;
        let endgame_black = $state.endgame_material[black] as i32;

        let side_sign = -2 * $state.playing as i32 + 1;

        let tempo = $state.statics.tempo_bonus;

        let major_diff = $state.major_pieces[white] as i32
            - $state.major_pieces[black] as i32;
        let minor_diff = $state.minor_pieces[white] as i32
            - $state.minor_pieces[black] as i32;
        let imbalance = major_diff * $state.statics.imbalance_major
            + minor_diff * $state.statics.imbalance_minor;

        let mut pair_bonus = 0i32;
        for &idx in $state.statics.pair_eligible_indices.iter() {
            if $state.piece_count[idx as usize] >= 2 {
                if p_color!(&$state.statics.pieces[idx as usize]) == WHITE {
                    pair_bonus += $state.statics.pair_bonus[idx as usize];
                } else {
                    pair_bonus -= $state.statics.pair_bonus[idx as usize];
                }
            }
        }

        match $state.game_phase {
            OPENING | SETUP => {
                let score_opening = opening_white - opening_black
                    + $state.opening_pst_bonus[white]
                    - $state.opening_pst_bonus[black]
                    + imbalance + pair_bonus;
                score_opening * side_sign + tempo
            }
            ENDGAME => {
                let score_endgame = endgame_white - endgame_black
                    + $state.endgame_pst_bonus[white]
                    - $state.endgame_pst_bonus[black]
                    + imbalance + pair_bonus;
                score_endgame * side_sign + tempo
            }
            MIDDLEGAME => {
                let score_opening = (opening_white - opening_black
                    + $state.opening_pst_bonus[white]
                    - $state.opening_pst_bonus[black]
                    + imbalance + pair_bonus);

                let score_endgame = (endgame_white - endgame_black
                    + $state.endgame_pst_bonus[white]
                    - $state.endgame_pst_bonus[black]
                    + imbalance + pair_bonus);

                let opening_score = $state.statics.opening_score as i32;
                let endgame_score = $state.statics.endgame_score as i32;
                let current_score = $state.phase_score as i32;

                let blended_score = (
                        (score_opening * (current_score - endgame_score)) +
                        (score_endgame * (opening_score - current_score))
                    ) / (opening_score - endgame_score);
                blended_score * side_sign + tempo
            }
            _ => panic!("Invalid game phase {}", $state.game_phase),
        }
    }};
}