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
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
) -> i32 {
    let mut alpha = alpha;

    #[cfg(debug_assertions)]
    verify_game_state(state);

    info.nodes += 1;
    if info.nodes.is_multiple_of(2048) {
        check_interrupt(info);
    }

    let is_repetition = state
        .position_hash_map
        .get(&state.position_hash)
        .copied()
        .unwrap_or(0)
        >= state.repetition_limit;

    let is_halfmove_draw =
        halfmove_clock!(state) && state.halfmove_clock >= state.halfmove_limit;

    if state.search_ply > 0 && (is_repetition || is_halfmove_draw) {
        return 0;
    }

    if state.search_ply >= MAX_DEPTH as u32 {
        return evaluate_position!(state);
    }

    let stand_pat = evaluate_position!(state);

    if stand_pat >= beta {
        return beta;
    }

    if stand_pat > alpha {
        alpha = stand_pat;
    }

    let alpha_start = alpha;
    let mut best_move = null_move();

    let mut all_captures = generate_all_captures(state);
    let pv_move = probe_pv_move!(state);

    for i in 0..all_captures.len() {
        pick_by_score(state, &mut all_captures, i, &pv_move);

        let mv = all_captures[i].clone();

        let move_type = move_type!(mv);
        let promotion = promotion!(mv);
        let captured_value = if move_type == SINGLE_CAPTURE_MOVE {
            p_ovalue!(state.pieces[captured_piece!(mv.clone()) as usize])
        } else {
            let mut total = 0;
            for cap in mv.1.iter() {
                total += p_ovalue!(state.pieces[*cap as usize]);
            }
            total
        };

        if captured_value as i32 + 200 < alpha
        && state.game_phase != ENDGAME
        && !promotion
        {                                                                       /* delta pruning                      */
            continue;
        }

        if !make_move!(state, mv) {
            continue;
        }

        let score = -quiescence_search(state, -beta, -alpha, info);
        undo_move!(state);

        if info.interrupt {
            return alpha;
        }

        if score > alpha {
            if score >= beta {
                return beta;
            }

            alpha = score;
            best_move = all_captures[i].clone();
        }
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start {
        hash_tt_entry!(best_move, alpha, HFEXACT, 0, state);
    }

    alpha
}
