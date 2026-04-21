//! # search.rs
//!
//! Defines search-time control and accounting data used by the engine.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 22/03/2026

use crate::*;

/// Tracks limits, counters, and stop flags for an active search.
///
/// This struct groups time controls, depth/move constraints, node accounting,
/// and interruption controls used by iterative search routines.
/// It is mutated throughout one search invocation lifecycle.
pub struct SearchInfo {
    pub start_time: u128,                                                       /* Start time since search start.     */

    pub set_depth: usize,                                                       /* Maximum search depth.              */
    pub set_timed: u128,                                                        /* Time limit in ns (0 = inf)         */
    pub set_moves: usize,                                                       /* Moves to go until time control.    */

    pub nodes: u128,                                                            /* Total nodes searched so far.       */

    pub interrupt: bool,                                                        /* Flag set by external stop events.  */
}

impl Default for SearchInfo {
    fn default() -> Self {
        Self {
            start_time: 0,
            set_depth: 0,
            set_timed: 0,
            set_moves: 0,
            nodes: 0,
            interrupt: false,
        }
    }
}

pub struct SearchResult {
    pub best_score: i32,
    pub best_move: Move,
    pub total_nodes: u128,
    pub total_elapsed: u128,
}

/// Polls stop conditions and updates search interrupt state.
///
/// A timed search is interrupted when elapsed nanoseconds from `start_time`
/// reaches or exceeds `set_timed`.
#[inline(always)]
pub fn check_interrupt(info: &mut SearchInfo) {
    if info.interrupt || info.set_timed == 0 {
        return;
    }

    let elapsed = ENGINE_START
        .elapsed()
        .as_nanos()
        .saturating_sub(info.start_time);

    if elapsed >= info.set_timed {
        info.interrupt = true;
    }
}

/// Clears per-search history/table state before a fresh root search.
///
/// This resets node counters, interruption flags, PV/killer/history tables,
/// and ply tracking. It does not alter the current board position.
pub fn clear_search(state: &mut State, info: &mut SearchInfo) {
    info.start_time = ENGINE_START.elapsed().as_nanos();
    info.nodes = 0;
    info.interrupt = false;

    let piece_count: usize = state.pieces.len();
    let board_size: usize = (state.files as usize) * (state.ranks as usize);

    state.search_hist = vec![vec![0u16; board_size]; piece_count];
    state.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];

    state.transposition_table.over_write = 0;
    state.transposition_table.hit = 0;
    state.transposition_table.cut = 0;

    state.search_ply = 0;
}

/// Runs iterative deepening alpha-beta and prints the current best line.
///
/// The search depth increases from `1..=info.depth`. After each completed
/// iteration, the principal variation is extracted from the PV table and
/// reported in UCI-style informational logs.
pub fn search_position(
    state: &mut State, info: &mut SearchInfo
) -> SearchResult {

    let mut best_move = null_move();
    let mut best_score: i32 = 0;
    let start_time = ENGINE_START.elapsed().as_nanos();

    clear_search(state, info);

    for depth in 1..=info.set_depth {
        let depth_start_nodes = info.nodes;
        let depth_start_time = ENGINE_START.elapsed().as_nanos();

        let score = alpha_beta(
            state, depth, i32::MIN + 1, i32::MAX, info, true                    /* i32::MIN + 1 to avoid overflow     */
        );

        if info.interrupt {
            break;
        }

        best_score = score;

        fill_pv_line!(state, depth);
        best_move = state.pv_line[0].clone();

        let elapsed = ENGINE_START
            .elapsed()
            .as_nanos()
            .saturating_sub(depth_start_time);
        let nodes = info.nodes - depth_start_nodes;

        let depth_nps = if elapsed == 0 {
            0
        } else {
            nodes * 1_000_000_000 / elapsed
        };

        info!(
            concat!(
                "Depth {:>2} | Score: {:>6} | Best Move: {:<8} | ",
                "Depth Nodes: {:>12} | ",
                "Time: {:>10} | NPS: {:>12}",
            ),
            depth,
            best_score,
            format_move(&best_move, state),
            nodes,
            format_time(elapsed),
            depth_nps,
        );

        info!(
            "Best Line: {}",
            state.pv_line
                .iter()
                .take(depth)
                .take_while(|m| m != &&null_move())
                .map(|m| format_move(m, state))
                .collect::<Vec<String>>()
                .join(" ")
        );
    }

    let total_nodes = info.nodes;
    let total_elapsed =
        ENGINE_START.elapsed().as_nanos().saturating_sub(start_time);
    let nps = if total_elapsed == 0 {
        0
    } else {
        total_nodes * 1_000_000_000 / total_elapsed
    };

    info!(
        concat!(
            "Search complete | Final Score: {:>6} | Best Move: {:<8} | ",
            "Total Nodes: {:>12} | Total Time: {:>10} | NPS: {:>12}"
        ),
        best_score,
        format_move(&best_move, state),
        total_nodes,
        format_time(total_elapsed),
        nps
    );

    SearchResult {
        best_score,
        best_move,
        total_nodes,
        total_elapsed,
    }
}

/// Searches one node with negamax alpha-beta pruning.
///
/// Hot-path optimizations in this implementation:
/// - PV move is probed once and reused for in-place move selection.
/// - Remaining moves are selected in-place using MVV-LVA + killer/history.
/// - Expensive full-state verification runs only in debug builds.
///
/// Returns the best score for the current side to move.
pub fn alpha_beta(
    state: &mut State,
    depth: usize,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
    null: bool,
) -> i32 {
    let mut alpha = alpha;
    let mut depth = depth;

    #[cfg(debug_assertions)]
    verify_game_state(state);

    info.nodes += 1;
    check_interrupt(info);

    if depth == 0 {
        return quiescence_search(state, alpha, beta, info);
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

    let in_check = is_in_check!(state.playing, state);

    if in_check {
        depth += 1;
    }

    let mut pv_move = None;
    let tt_entry = probe_tt_entry(state, alpha, beta, depth);

    if tt_entry.1 != null_move() {
        pv_move = Some(tt_entry.1);
    }

    if tt_entry.0 {
        state.transposition_table.cut += 1;
        return tt_entry.2;
    }

    if null
    && !in_check
    && depth >= 3
    && state.search_ply > 0
    && state.big_pieces[state.playing as usize] > 0 {
        make_null_move!(state);
        let score = -alpha_beta(
            state, depth - 3, -beta, -beta + 1, info, false
        );
        undo_null_move!(state);

        if info.interrupt {
            return 0;
        }

        if score >= beta {
            return beta;
        }
    }

    let mut best_move = null_move();
    let mut best_score = i32::MIN + 1;                                          /* Avoid overflow                     */
    let mut legal_moves = 0;
    let alpha_start = alpha;

    let mut all_moves = generate_all_moves_and_drops(state);

    for i in 0..all_moves.len() {
        pick_by_score(state, &mut all_moves, i, &pv_move);

        let mv = all_moves[i].clone();
        let mv_type = move_type!(mv);
        let mv_piece = piece!(mv) as usize;
        let mv_end = end!(mv) as usize;
        let is_capture =
            mv_type == SINGLE_CAPTURE_MOVE || mv_type == MULTI_CAPTURE_MOVE;

        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;
        let score = -alpha_beta(state, depth - 1, -beta, -alpha, info, true);
        undo_move!(state);

        if info.interrupt {
            return 0;
        }

        if score > best_score {
            best_score = score;
            best_move = mv.clone();

            if score > alpha {
                if score >= beta {
                    if !is_capture {
                        state.killer_hist[state.search_ply as usize].swap(1, 0);
                        state.killer_hist[state.search_ply as usize][0] =
                            mv.clone();
                    }

                    hash_tt_entry!(best_move, beta, HFBETA, depth, state);

                    return beta;
                }

                alpha = score;

                if !is_capture {
                    state.search_hist[mv_piece][mv_end] += depth as u16;
                }
            }
        }
    }

    if legal_moves == 0 {
        if is_in_check!(state.playing, state) || stalemate_loss!(&state) {
            let mate_score = -MATE_SCORE + state.search_ply as i32;

            return if state.history.last().map_or(false, |s| {
                move_type!(&s.move_ply) == DROP_MOVE
                && !drop_can_checkmate!(&s.move_ply)
            }) {
                -mate_score
            } else {
                mate_score
            };
        }

        return 0;
    }


    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start {
        hash_tt_entry!(best_move, best_score, HFEXACT, depth, state);
    } else {
        hash_tt_entry!(best_move, alpha, HFALPHA, depth, state);
    }

    alpha
}
