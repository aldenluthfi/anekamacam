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

/// Per-side count of friendly non-royal pieces adjacent to each royal piece
/// (via the royal piece's own move geometry). Exposed royalty scores low; the
/// caller folds this into the opening/middlegame evaluation only, since the
/// king should be active in the endgame.
pub fn king_shelter(state: &State, color: u8) -> i32 {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * color as i32 + 1;

    let mut shelter = 0;

    for (piece_index, piece) in state.statics.pieces.iter().enumerate() {
        if p_color!(piece) != color || !p_is_royal!(piece) {
            continue;
        }

        for &king_square in &state.piece_list[piece_index] {
            let start_file = king_square as i32 % files;
            let start_rank = king_square as i32 / files;

            let neighbours = &state.statics.relevant_moves
                [piece_index * board_size + king_square as usize];

            for vector in neighbours {
                let mut file = start_file;
                let mut rank = start_rank;

                for leg in vector {
                    file += x!(leg) as i32 * sign;
                    rank += y!(leg) as i32 * sign;
                }

                if file < 0 || file >= files || rank < 0 || rank >= ranks {
                    continue;
                }

                let occupant =
                    state.main_board[(rank * files + file) as usize];

                if occupant != NO_PIECE
                && p_color!(&state.statics.pieces[occupant as usize]) == color
                && !p_is_royal!(&state.statics.pieces[occupant as usize]) {
                    shelter += 1;
                }
            }
        }
    }

    shelter
}

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

        let king_safety = KING_SHELTER_BONUS
            * (king_shelter($state, WHITE) - king_shelter($state, BLACK));

        match $state.game_phase {
            OPENING | SETUP => {
                let score_opening = opening_white - opening_black
                    + $state.opening_pst_bonus[white]
                    - $state.opening_pst_bonus[black]
                    + imbalance + pair_bonus + king_safety;
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
                    + imbalance + pair_bonus + king_safety);

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