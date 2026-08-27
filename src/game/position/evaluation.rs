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

/// royal_shelter!
///
/// Worth of the friendly pieces standing around one colour's royals. Each
/// royal reads two precomputed square lists: the squares forward of it,
/// which pay only for shield-like friendly pieces, and every square around
/// it, which pays for any friendly piece at a lower rate. Both counts are
/// capped, so a royal buried in its own army stops earning once it is
/// covered and a variant with a crowded board cannot run the term away.
///
/// Params:
/// - state: &State -> position whose royals are read
/// - color: usize  -> colour whose royals are scored
///
/// Return:
/// i32             -> shelter and cover worth, always non-negative
#[macro_export]
macro_rules! royal_shelter {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let stride = statics.local_stride;
        let cap = statics.shelter_cap as i32;
        let mut worth = 0;

        for royal_square in &$state.royal_list[$color] {
            let royal = *royal_square as usize;
            let mut shelter = 0;
            let mut cover = 0;

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

            for slot in 0..statics.cover_counts[royal] as usize {
                let square =
                    statics.cover_squares[royal * stride + slot] as usize;

                cover += get!($state.pieces_board[$color], square as u32)
                    as i32;
            }

            worth += statics.shelter_value * shelter.min(cap)
                + statics.cover_value * cover.min(cap);
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
