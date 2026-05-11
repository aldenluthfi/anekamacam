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
#[derive(Default)]
pub struct SearchInfo {
    pub start_time: u128,                                                       /* Start time since search start.     */

    pub set_depth: usize,                                                       /* Maximum search depth.              */
    pub set_timed: u128,                                                        /* Time limit in ns (0 = inf)         */
    pub set_moves: usize,                                                       /* Moves to go until time control.    */

    pub nodes: u128,                                                            /* Total nodes searched so far.       */

    pub interrupt: bool,                                                        /* Flag set by external stop events.  */
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
pub fn clear_search(
    state: &mut State, table: &TTable, info: &mut SearchInfo
) {
    info.start_time = ENGINE_START.elapsed().as_nanos();
    info.nodes = 0;
    info.interrupt = false;

    let piece_count: usize = state.pieces.len();
    let board_size: usize = (state.files as usize) * (state.ranks as usize);

    state.search_hist = vec![vec![0u16; board_size]; piece_count];
    state.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];

    table.age.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
    state.search_ply = 0;
}

/// Runs iterative deepening alpha-beta and prints the current best line.
///
/// The search depth increases from `1..=info.depth`. After each completed
/// iteration, the principal variation is extracted from the PV table and
/// reported in UCI-style informational logs.
pub fn search_position(
    state: &mut State, table: Arc<TTable>, info: &mut SearchInfo,
    thread_num: usize,
) -> SearchResult {
    table.hit.store(0, Ordering::Relaxed);
    table.valid.store(0, Ordering::Relaxed);
    table.new_write.store(0, Ordering::Relaxed);
    table.over_write.store(0, Ordering::Relaxed);

    let result = if thread_num <= 1 {
        iterative_deepening(state, &table, info, 0)
    } else {
        let pool = ThreadPool::with_threads(
            state, Arc::clone(&table), thread_num
        );
        pool.run(info.set_depth, info.set_timed)
    };

    log_2!(
        "TT | new: {} | over: {} | hit: {} | valid: {}",
        table.new_write.load(Ordering::Relaxed),
        table.over_write.load(Ordering::Relaxed),
        table.hit.load(Ordering::Relaxed),
        table.valid.load(Ordering::Relaxed),
    );

    result
}

pub fn iterative_deepening(
    state: &mut State, table: &TTable, info: &mut SearchInfo,
    thread_num: usize,
) -> SearchResult {

    let mut best_move = null_move();
    let mut best_score: i32 = 0;
    let start_time = ENGINE_START.elapsed().as_nanos();

    clear_search(state, table, info);

    for depth in 1..=info.set_depth {
        let depth_start_nodes = info.nodes;
        let depth_start_time = ENGINE_START.elapsed().as_nanos();

        let score = alpha_beta(
            state, table, depth, -INFINITY, INFINITY, info, true
        );

        if info.interrupt {
            break;
        }

        best_score = score;

        fill_pv_line!(state, table, depth);
        best_move = state.pv_line[0].clone();

        let elapsed = ENGINE_START
            .elapsed()
            .as_nanos()
            .saturating_sub(depth_start_time);
        let nodes = info.nodes - depth_start_nodes;

        let depth_nps = nodes
            .checked_mul(1_000_000_000)
            .and_then(|n| n.checked_div(elapsed))
            .unwrap_or(0);

        if thread_num == 0 {
            log_4!(
                concat!(
                    "Score: {:>6} | Best Move: {:<8} | ",
                    "Depth Nodes: {:>12} | ",
                    "NPS: {:>12}",
                ),
                best_score,
                format_move(&best_move, state),
                nodes,
                depth_nps,
            );

            log_3!(
                "Depth {:>2} | Time: {:>10} | Best Line: {}",
                depth,
                format_time(elapsed),
                state.pv_line
                    .iter()
                    .take(depth)
                    .take_while(|m| m != &&null_move())
                    .map(|m| format_move(m, state))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
        }
    }

    let total_nodes = info.nodes;
    let total_elapsed =
        ENGINE_START.elapsed().as_nanos().saturating_sub(start_time);
    let nps = total_nodes
        .checked_mul(1_000_000_000)
        .and_then(|n| n.checked_div(total_elapsed))
        .unwrap_or(0);

    if thread_num == 0 {
        log_1!(
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
    }

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
    table: &TTable,
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

    if state.game_over {
        return 0;
    }

    let in_check = is_in_check!(state.playing, state);

    if in_check {
        depth += 1;
    }

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    if depth == 0 {
        return quiescence_search(state, table, alpha, beta, info);
    }


    let static_eval = evaluate_position!(state);

    if state.search_ply >= MAX_DEPTH as u32 {
        return static_eval;
    }

    let mut pv_move: Option<PseudoMove> = None;
    let tt_entry = probe_tt_entry!(state, table, alpha, beta, depth);           /* Transposition table lookup         */

    if tt_entry.2 != null_pseudo_move() {
        pv_move = Some(tt_entry.2);
    }

    if tt_entry.0 {
        return tt_entry.1;
    }

    let mut pv_capture = false;

    if let Some(pv_mv) = &pv_move
        && (move_type!(pv_mv) == SINGLE_CAPTURE_MOVE
        || move_type!(pv_mv) == MULTI_CAPTURE_MOVE)
        {
            pv_capture = true;
        }

    if depth < 3
    && (pv_move.is_none() && !pv_capture)
    && !in_check                                                                /* Static eval pruning                */
    {
        let eval_margin = 150 * depth as i32;
        if static_eval - eval_margin >= beta {
            return static_eval - eval_margin;
        }
    }

    if null
    && !in_check
    && pv_move.is_none()
    && depth >= 3
    && static_eval >= beta
    && state.search_ply > 0
    && state.game_phase != ENDGAME
    {                                                                           /* Null move pruning                  */
        let reduction = 3 + if depth >= 6 { 1 } else { 0 };
        make_null_move!(state);
        let score = -alpha_beta(
            state, table, depth - reduction, -beta, -beta + 1, info, false
        );
        undo_null_move!(state);

        if info.interrupt {
            return 0;
        }

        if score >= beta {
            return beta;
        }
    }

    let mut futile = false;
    let futility_margin = [ 0, 200, 300, 500 ];

    if depth < 4
    && !in_check
    && pv_move.is_none()
    && alpha.abs() < MATE_SCORE
    && static_eval + futility_margin[depth] <= alpha
    {
        futile = true;                                                          /* Futility pruning                   */
    }

    let mut best_move = null_move();
    let mut best_score = -INFINITY;
    let mut legal_moves = 0;
    let alpha_start = alpha;

    let mut all_moves = generate_all_moves_and_drops(state);

    for i in 0..all_moves.len() {
        pick_by_score(state, &mut all_moves, i, &pv_move);

        let mv = all_moves[i].clone();
        let mv_type = move_type!(mv);
        let mv_piece = piece!(mv) as usize;
        let mv_start = start!(mv);
        let mv_end = end!(mv);
        let is_capture =
            mv_type == SINGLE_CAPTURE_MOVE &&
            !is_unload!(mv) ||
            mv_type == MULTI_CAPTURE_MOVE &&
            mv.1.iter().all(|cap| !multi_move_is_unload!(cap));
        let is_promotion = promotion!(mv);
        let is_drop = mv_type == DROP_MOVE;

        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;

        let mut reduct = 1;
        let mut score;

        let opponent_in_check = is_in_check!(state.playing, state);

        if futile
        && !opponent_in_check
        && !is_capture
        && !is_promotion
        && !is_drop
        {                                                                       /* Futility pruning                   */
            undo_move!(state);
            continue;
        }

        if depth > 4
        && legal_moves > 3
        && !opponent_in_check
        && !in_check
        && (mv_end != end!(state.killer_hist[state.search_ply as usize][0])
        || mv_start != start!(state.killer_hist[state.search_ply as usize][0]))
        && (mv_end != end!(state.killer_hist[state.search_ply as usize][1])
        || mv_start != start!(state.killer_hist[state.search_ply as usize][1]))
        && !is_capture
        && !is_promotion
        && !is_drop
        {
            reduct += 1;                                                        /* Late move reduction                */

            if legal_moves > 6 {
                reduct += 1;
            }

            score =
                -alpha_beta(
                    state, table, depth - reduct, -alpha - 1, -alpha, info, true
                );

            if score > alpha {
                score =
                    -alpha_beta(
                        state, table, depth - 1, -beta, -alpha, info, true
                    );
            }
        } else {
            score =
                -alpha_beta(
                    state, table, depth - reduct, -beta, -alpha, info, true
                );
        }

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

                    hash_tt_entry!(best_move, beta, FBETA, depth, state, table);

                    return beta;
                }

                alpha = score;

                if !is_capture {
                    state.search_hist[mv_piece][mv_end as usize] +=
                        depth as u16;
                }
            }
        }
    }

    if legal_moves == 0 {
        if in_check || stalemate_loss!(&state) {
            let mate_score = -INFINITY + state.search_ply as i32;

            return if state.history.last().is_some_and(|s| {
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
        hash_tt_entry!(best_move, best_score, FEXACT, depth, state, table);
    } else {
        hash_tt_entry!(best_move, alpha, FALPHA, depth, state, table);
    }

    alpha
}
