//! evaluation.rs
//!
//! Static position evaluation for alpha-beta search.
//!
//! Material values and piece-square tables are derived from variant rules at
//! startup and maintained incrementally by make/undo. Opening and endgame
//! totals are blended by current material phase, with no additional positional
//! terms. Terminal positions are scored here too, and a drawn one is priced by
//! the material lead standing on the board rather than at a flat zero.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

/// draw_score!
///
/// What a draw is worth to the side to move, in place of a plain zero. A side
/// holding more material than its opponent has something to lose by agreeing
/// the game, so the lead is read back as a cost: ahead scores the draw below
/// zero and behind scores it above. The lead is measured in the material the
/// current phase prices, clamped to the derived `draw_span`, and paid at
/// `draw_contempt` for a full span.
///
/// Both halves are shares of this variant's own mean deployed piece, so a
/// variant playing in small units is not handed a large contempt, and no rule
/// is asked about beyond the material already on the board. A level position
/// scores zero, and the score a colour reads is the negation of what its
/// opponent reads from the same position.
///
/// Params:
/// - state: &State -> position whose draw value is computed
///
/// Return:
/// i32             -> draw value from side-to-move perspective
#[macro_export]
macro_rules! draw_score {
    ($state:expr) => {{
        let moving = $state.playing as usize;
        let waiting = ($state.playing ^ 1) as usize;

        let lead = if $state.game_phase == ENDGAME {
            $state.endgame_material[moving] as i32
                - $state.endgame_material[waiting] as i32
        } else {
            $state.opening_material[moving] as i32
                - $state.opening_material[waiting] as i32
        };

        let span = $state.statics.draw_span;

        -lead.clamp(-span, span) * $state.statics.draw_contempt / span
    }};
}

/// terminal_score!
///
/// Scores a terminal position from side-to-move perspective. A decisive result
/// uses mate-scaled scores so shorter wins and longer losses are preferred.
/// A draw is worth what [`draw_score!`] says the material on the board makes
/// it worth.
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
            draw_score!($state)
        }
    }};
}

/// royal_shelter!
///
/// Worth of shield-like friendly pieces standing ahead of one colour's royals.
/// Each royal reads one precomputed forward-square list. Count is capped, so a
/// royal buried in its own army stops earning once its shelter is full.
///
/// Params:
/// - state: &State -> position whose royals are read
/// - color: usize  -> colour whose royals are scored
///
/// Return:
/// i32             -> shelter worth, always non-negative
#[macro_export]
macro_rules! royal_shelter {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let stride = statics.local_stride;
        let cap = SHELTER_CAP as i32;
        let mut worth = 0;

        for royal_square in &$state.royal_list[$color] {
            let royal = *royal_square as usize;
            let mut shelter = 0;

            for slot in 0..statics.shelter_counts[$color][royal] as usize {
                let square = statics.shelter_squares[$color]
                    [royal * stride + slot] as usize;
                let piece = $state.main_board[square];

                if piece != NO_PIECE
                    && statics.shield_pieces[piece as usize]
                    && p_color!(&statics.pieces[piece as usize]) as usize
                        == $color {
                    shelter += 1;
                }
            }

            worth += statics.shelter_value * shelter.min(cap);
        }

        worth
    }};
}

/// evaluate_position!
///
/// Evaluates current position from side-to-move perspective using cached
/// material and piece-square-table totals plus the royal shelter each side
/// stands in. Opening and setup use opening values, endgame uses endgame
/// values, and middlegame linearly blends both. Shelter is carried by the
/// opening half alone, so it fades out as the board empties and is gone by
/// the endgame, where a royal wants to walk rather than hide.
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
                - $state.opening_pst_bonus[black]
                + royal_shelter!($state, white)
                - royal_shelter!($state, black);
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
