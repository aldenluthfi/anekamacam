//! # quiescence.rs
//!
//! Placeholder module for quiescence search implementation.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 29/01/2026

use crate::*;

/// Capture-only extension search used at leaf nodes to reduce horizon effects.
///
/// Core flow:
/// 1. Evaluate stand-pat score.
/// 2. If stand-pat is above `beta`, fail-high immediately.
/// 3. Otherwise generate only tactical continuations from
///    `generate_all_captures`.
/// 4. Search legal captures with negamax alpha-beta recursion.
pub fn quiescence_search(
    state: &mut State,
    table: &TTable,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
) -> i32 {
    let mut alpha = alpha;

    #[cfg(debug_assertions)]
    verify_game_state(state);

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    let stand_pat = evaluate_position!(state);

    if stand_pat >= beta {
        return beta;
    }

    if stand_pat > alpha {
        alpha = stand_pat;
    }

    let ply = state.search_ply as usize;
    generate_all_captures(
        state, &mut info.move_buf[ply], &mut info.scratch_buf
    );
    let pv_move = probe_pv_move!(state, table);

    for i in 0..info.move_buf[ply].len() {
        pick_by_score(state, &mut info.move_buf[ply], i, &pv_move);

        let mv = info.move_buf[ply][i].clone();

        let move_type = move_type!(mv);
        let promotion = promotion!(mv);
        let captured_value = if move_type == SINGLE_CAPTURE_MOVE {
            p_ovalue!(
                state.statics.pieces[captured_piece!(mv.clone()) as usize]
            )
        } else {
            let mut total = 0;
            for cap in mv.1.iter() {
                total += p_ovalue!(state.statics.pieces[*cap as usize]);
            }
            total
        };

        if stand_pat + captured_value as i32 + 200 < alpha
        && state.game_phase != ENDGAME
        && !promotion
        {                                                                       /* delta pruning                      */
            continue;
        }

        if !make_move!(state, mv) {
            continue;
        }

        let score = -quiescence_search(state, table, -beta, -alpha, info);
        undo_move!(state);

        if info.interrupt {
            return alpha;
        }

        if score > alpha {
            if score >= beta {
                return beta;
            }

            alpha = score;
        }
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    alpha
}
