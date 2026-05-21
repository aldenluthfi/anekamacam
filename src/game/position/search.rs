//! # search.rs
//!
//! Alpha-beta search with iterative deepening, aspiration windows, and
//! quiescence.
//!
//! SearchInfo and SearchBufs carry search-time limits, counters, and scratch
//! allocations; SearchResult packages the output.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 22/03/2026

use crate::*;

/// Search limits, counters, and stop flags for an active search.
///
/// Carries time controls, depth/move constraints, node count, and interrupt
/// flag. Scratch and move buffers live in SearchBufs.
#[derive(Default)]
pub struct SearchInfo {
    pub start_time: u128,                                                       /* start time since engine launch     */

    pub set_depth: usize,                                                       /* maximum search depth               */
    pub set_timed: u128,                                                        /* time limit in ns (0 = unlimited)   */
    pub set_moves: usize,                                                       /* moves to go until time control     */

    pub nodes: u128,                                                            /* total nodes searched so far        */

    pub interrupt: bool,                                                        /* flag set by external stop events   */
}

/// Per-thread scratch allocations reused across the full search tree.
///
/// move_buf holds pre-allocated per-ply move lists; scratch_buf is reused
/// for taken_pieces computations.
#[derive(Default)]
pub struct SearchBufs {
    pub move_buf: Vec<Vec<Move>>,                                               /* per-ply move lists, pre-allocated  */
    pub score_buf: Vec<Vec<usize>>,                                             /* pre-computed move ordering scores  */
    pub scratch_buf: Vec<u64>,                                                  /* reused scratch for taken_pieces    */
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
    if info.interrupt {
        return;
    }

    let elapsed = ENGINE_START
        .elapsed()
        .as_nanos()
        .saturating_sub(info.start_time);

    if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
        log_3!(
            "SIGINT | Elapsed Time: {} | Nodes: {} | ",
            format_time(elapsed),
            info.nodes,
        );
        info.interrupt = true;
        return;
    }

    if info.set_timed == 0 {
        return;
    }

    if elapsed >= info.set_timed {
        info.interrupt = true;
    }
}

/// Resets search state before a fresh root search.
///
/// Zeroes node counters, interrupt flag, killer and history tables, and ply.
/// Board position is unchanged.
pub fn clear_search(
    state: &mut State, ttable: &TTable, qtable: &QTable,
    info: &mut SearchInfo, bufs: &mut SearchBufs,
) {
    info.start_time = ENGINE_START.elapsed().as_nanos();
    info.nodes = 0;
    info.interrupt = false;

    if bufs.move_buf.len() < MAX_DEPTH * 2 {
        bufs.move_buf = (0..MAX_DEPTH * 2)
            .map(|_| Vec::with_capacity(64)).collect();
        bufs.score_buf = (0..MAX_DEPTH * 2)
            .map(|_| Vec::with_capacity(64)).collect();
        bufs.scratch_buf = Vec::with_capacity(32);
    }

    let piece_count: usize = state.statics.pieces.len();
    let board_size: usize =
        (state.statics.files as usize) * (state.statics.ranks as usize);

    state.search_hist = vec![vec![0u16; board_size]; piece_count];
    state.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];

    ttable.age.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
    qtable.age.fetch_add(1, core::sync::atomic::Ordering::Relaxed);

    let piece_count: usize = state.statics.pieces.len();
    let board_size: usize =
        (state.statics.files as usize) * (state.statics.ranks as usize);

    state.search_hist = vec![vec![0u16; board_size]; piece_count];
    state.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];

    state.search_ply = 0;
}

pub fn search_position(
    state: &mut State, table: Arc<TTable>, qtable: Arc<QTable>,
    info: &mut SearchInfo, bufs: &mut SearchBufs,
    thread_num: usize,
) -> SearchResult {
    table.hit.store(0, Ordering::Relaxed);
    table.valid.store(0, Ordering::Relaxed);
    table.new_write.store(0, Ordering::Relaxed);
    table.over_write.store(0, Ordering::Relaxed);

    qtable.hit.store(0, Ordering::Relaxed);
    qtable.valid.store(0, Ordering::Relaxed);
    qtable.new_write.store(0, Ordering::Relaxed);
    qtable.over_write.store(0, Ordering::Relaxed);

    let result = if thread_num <= 1 {
        iterative_deepening(state, &table, &qtable, info, bufs, 0)
    } else {
        let pool = ThreadPool::with_threads(
            state, Arc::clone(&table), Arc::clone(&qtable), thread_num
        );
        pool.run(info.set_depth, info.set_timed)
    };

    log_3!(
        "TT | new: {:<8} | over: {:<8} | hit: {:<8} | valid: {:<8}",
        table.new_write.load(Ordering::Relaxed),
        table.over_write.load(Ordering::Relaxed),
        table.hit.load(Ordering::Relaxed),
        table.valid.load(Ordering::Relaxed),
    );

    log_3!(
        "QT | new: {:<8} | over: {:<8} | hit: {:<8} | valid: {:<8}",
        qtable.new_write.load(Ordering::Relaxed),
        qtable.over_write.load(Ordering::Relaxed),
        qtable.hit.load(Ordering::Relaxed),
        qtable.valid.load(Ordering::Relaxed),
    );

    result
}

/*----------------------------------------------------------------------------*\
                              MTD(f) DRIVER
\*----------------------------------------------------------------------------*/

fn mtdf(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    depth: usize,
    f: i32,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
) -> i32 {
    let mut lower = -INF;
    let mut upper = INF;
    let mut g = f;

    for _ in 0..MAX_DEPTH {
        let bound = if g == lower { g + 1 } else { g };

        g = alpha_beta(
            state, ttable, qtable, depth, bound - 1, bound, info, bufs, true
        );

        if g < bound {
            upper = g;
        } else {
            lower = g;
        }

        if lower >= upper {
            break;
        }
    }

    g
}

/*----------------------------------------------------------------------------*\
                         ITERATIVE DEEPENING
\*----------------------------------------------------------------------------*/

pub fn iterative_deepening(
    state: &mut State, ttable: &TTable, qtable: &QTable,
    info: &mut SearchInfo, bufs: &mut SearchBufs,
    thread_num: usize,
) -> SearchResult {

    let mut best_move = null_move();
    let mut best_score: i32 = 0;
    let start_time = ENGINE_START.elapsed().as_nanos();

    clear_search(state, ttable, qtable, info, bufs);

    for depth in 1..=info.set_depth {
        let depth_start_nodes = info.nodes;
        let depth_start_time = ENGINE_START.elapsed().as_nanos();

        let score = if depth == 1 {
            alpha_beta(
                state, ttable, qtable, depth, -INF, INF, info, bufs, true
            )
        } else {
            mtdf(state, ttable, qtable, depth, best_score, info, bufs)
        };

        if info.interrupt {
            break;
        }

        best_score = score;

        fill_pv_line!(state, ttable, depth);
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

        log_3!(
            concat!(
                "(Thread {}) Score: {:>6} | Best Move: {:<8} | ",
                "Depth Nodes: {:>12} | ",
                "NPS: {:>12}",
            ),
            thread_num,
            best_score,
            format_move(&best_move, state),
            nodes,
            depth_nps,
        );

        log_2!(
            "(Thread {}) Depth {:>2} | Time: {:>10} | Best Line: {}",
            thread_num,
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

    let total_nodes = info.nodes;
    let total_elapsed =
        ENGINE_START.elapsed().as_nanos().saturating_sub(start_time);
    let nps = total_nodes
        .checked_mul(1_000_000_000)
        .and_then(|n| n.checked_div(total_elapsed))
        .unwrap_or(0);

    log_1!(
        concat!(
            "(Thread {}) ",
            "Search complete | Final Score: {:>6} | Best Move: {:<8} | ",
            "Total Nodes: {:>12} | Total Time: {:>10} | NPS: {:>12}"
        ),
        thread_num,
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

/// Capture-only search at leaf nodes; reduces horizon effects.
///
/// Stand-pat score provides a lower bound. Captures are ordered with
/// pick_by_score and searched with negamax, with delta pruning to skip
/// clearly losing captures unless in endgame or promotion.
fn quiescence_search(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
) -> i32 {
    let mut alpha = alpha;

    if state.game_over {
        return 0;
    }


    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    let stand_pat = evaluate_position!(state);

    if stand_pat >= beta {
        return beta;
    }

    if state.search_ply > MAX_DEPTH as u32 {
        return stand_pat
    }

    if stand_pat > alpha {
        alpha = stand_pat;
    }

    let ply = state.search_ply as usize;

    let tt_pv_move = probe_pv_move!(state, ttable);
    let qtt_entry = probe_qt_entry!(state, qtable, alpha, beta);

    if qtt_entry.0 {
        return qtt_entry.1;
    }

    let pv_move = match qtt_entry.2 {
        pm if pm != null_pseudo_move() => Some(pm),
        _ => tt_pv_move,
    };

    let mut best_move = null_move();
    let best_score = alpha;

    generate_all_captures(
        state, &mut bufs.move_buf[ply], &mut bufs.scratch_buf
    );

    bufs.score_buf[ply] = vec![usize::MAX; bufs.move_buf[ply].len()];

    for i in 0..bufs.move_buf[ply].len() {
        pick_by_score!(
            state,
            &mut bufs.move_buf[ply], &mut bufs.score_buf[ply],
            i, &pv_move
        );

        let mv = bufs.move_buf[ply][i].clone();

        if see!(state, mv.clone()) < 0 {
            continue;
        }

        let move_type = move_type!(mv.clone());
        let promotion = promotion!(mv.clone());
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

        if !make_move!(state, mv.clone()) {
            continue;
        }

        let score =
            -quiescence_search(
                state, ttable, qtable, -beta, -alpha, info, bufs
            );
        undo_move!(state);

        if info.interrupt {
            return alpha;
        }

        if score > alpha {
            if score >= beta {
                hash_qt_entry!(mv, beta, FBETA, state, qtable);
                return beta;
            }

            best_move = mv.clone();
            alpha = score;
        }
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != best_score && best_move != null_move() {
        hash_qt_entry!(best_move, alpha, FEXACT, state, qtable);
    }

    alpha
}


/// Heuristic reduction for late move reduction, based on depth, move count,
/// checks, and move type.
///
/// For captures, promotions, and drops:
///
/// reduction = 0.20 + (sqrt(depth) * ln(moves) / 3.35)
///
/// For quiet moves:
///
/// reduction = 1.35 + (sqrt(depth) * ln(moves) / 2.75)
///
/// In either case, if the side to move is in check or the opponent is in check,
/// reduction is reduced by 1.25.
#[macro_export]
macro_rules! reduction {
    (
        $state:expr,
        $depth:expr,
        $moves:expr,
        $in_check:expr,
        $opponent_in_check:expr,
        $is_capture:expr,
        $is_promotion:expr,
        $is_drop:expr
    ) => {{
        let mut reduction = if $is_capture || $is_promotion || $is_drop {
            $state.statics.capture_lmr
                [$depth * MAX_DEPTH + $moves.min(MAX_LMR_DEPTH - 1)]
        } else {
            $state.statics.quiesce_lmr
                [$depth * MAX_DEPTH + $moves.min(MAX_LMR_DEPTH - 1)]
        };

        if $in_check || $opponent_in_check {
            reduction -= 1.25;
        }

        reduction.clamp(0.0, $depth as f64 - 1.0) as usize
    }};
}

/// Negamax alpha-beta with transposition table, null-move, futility pruning,
/// late move reduction, IID, and check extension.
///
/// PV move probed once per node for move ordering. Captures ordered by
/// SEE + most_valuable; quiet moves by killer and history heuristics.
/// Full state verification runs only in debug builds.
pub fn alpha_beta(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    mut depth: usize,
    mut alpha: i32,
    mut beta: i32,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
    null: bool,
) -> i32 {

    if state.game_over {
        return 0;
    }

    let ply = state.search_ply as usize;

    alpha = alpha.max(-MATE_SCORE + ply as i32);
    beta  = beta.min(MATE_SCORE - ply as i32);

    if alpha >= beta {                                                          /* mate distance pruning               */
        return alpha;
    }

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    let in_check = is_in_check!(state.playing, state);

    if in_check {
        depth += 1;
    }

    if depth == 0 {
        return quiescence_search(
            state, ttable, qtable, alpha, beta, info, bufs
        );
    }

    let static_eval = evaluate_position!(state);

    let mut pv_move: Option<PseudoMove> = None;
    let tt_entry = probe_tt_entry!(state, ttable, alpha, beta, depth);          /* transposition table lookup         */

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
    && (pv_move.is_none() || !pv_capture)
    && !in_check                                                                /* reverse futility pruning           */
    {
        let eval_margin = 150 * depth as i32;
        if static_eval - eval_margin >= beta {
            return beta;
        }
    }

    if depth <= 4
    && !in_check
    && pv_move.is_none()
    && alpha.abs() < MATE_SCORE
    && state.game_phase != ENDGAME
    && static_eval + 200 + 150 * (depth as i32) < alpha
    {                                                                           /* razoring                           */
        let shallow = alpha_beta(
            state, ttable, qtable,
            depth.saturating_sub(2),
            alpha, alpha + 1,
            info, bufs, null
        );

        if shallow <= alpha {
            return alpha;
        }
    }

    if null
    && !in_check
    && pv_move.is_none()
    && depth >= 3
    && static_eval >= beta
    && state.search_ply > 0
    && state.game_phase != ENDGAME
    {                                                                           /* null move pruning                  */
        let reduct = 3 + if depth >= 6 { 1 } else { 0 };
        make_null_move!(state);
        let score = -alpha_beta(
            state, ttable, qtable, depth - reduct, -beta, -beta + 1, info, bufs,
            false
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
    let margin = state.statics.futility_margin[match state.game_phase {
        OPENING | SETUP => 0,
        MIDDLEGAME      => 1,
        ENDGAME         => 2,
        _ => unreachable!(),
    }];

    if depth < margin.len()
    && !in_check
    && pv_move.is_none()
    && alpha.abs() < MATE_SCORE
    && static_eval + margin[depth] <= alpha
    {
        futile = true;                                                          /* futility pruning                   */
    }

    if pv_move.is_none() && depth >= 5 {                                        /* IID: search shallower to seed TT   */
        alpha_beta(
            state, ttable, qtable, depth - 2, alpha, beta, info, bufs, true
        );

        if !info.interrupt {
            pv_move = probe_pv_move!(state, ttable);
        }
    }

    let mut best_move = null_move();
    let mut best_score = -INF;
    let mut legal_moves = 0;
    let alpha_start = alpha;

    generate_all_moves_and_drops(
        state, &mut bufs.move_buf[ply], &mut bufs.scratch_buf
    );

    bufs.score_buf[ply] = vec![usize::MAX; bufs.move_buf[ply].len()];

    for i in 0..bufs.move_buf[ply].len() {
        pick_by_score!(
            state,
            &mut bufs.move_buf[ply], &mut bufs.score_buf[ply],
            i, &pv_move
        );

        let mv = bufs.move_buf[ply][i].clone();
        let mv_type = move_type!(mv);

        let piece = piece!(mv) as usize;
        let end = end!(mv);

        let is_capture = is_capture!(mv);
        let is_promotion = promotion!(mv);
        let is_drop = mv_type == DROP_MOVE;

        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;

        let mut reduction = 1;
        let mut score;

        let opponent_in_check = is_in_check!(state.playing, state);

        if futile
        && !opponent_in_check
        && !is_capture
        && !is_promotion
        && !is_drop
        {                                                                       /* futility pruning                   */
            undo_move!(state);
            continue;
        }

        if depth > 3
        && legal_moves > 2
        && mv != state.killer_hist[state.search_ply as usize][0]
        && mv != state.killer_hist[state.search_ply as usize][1]
        {                                                                       /* late move reduction                */
            reduction += reduction!(
                state, depth, legal_moves,
                in_check, opponent_in_check,
                is_capture, is_promotion, is_drop
            );

            score = -alpha_beta(
                state, ttable, qtable, depth - reduction, -alpha - 1, -alpha,
                info, bufs, true
            );

            if score > alpha && beta - alpha > 1 {
                score = -alpha_beta(
                    state, ttable, qtable, depth - 1,
                    -beta, -alpha, info, bufs, true
                );
            }
        } else if legal_moves > 1 {                                             /* PVS: null window for non-first     */
            score = -alpha_beta(
                state, ttable, qtable, depth - 1,
                -alpha - 1, -alpha, info, bufs, true
            );

            if score > alpha && beta - alpha > 1 {
                score = -alpha_beta(
                    state, ttable, qtable, depth - 1,
                    -beta, -alpha, info, bufs, true
                );
            }
        } else {
            score = -alpha_beta(
                state, ttable, qtable, depth - 1,
                -beta, -alpha, info, bufs, true
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

                let bonus = (depth * depth) as u16;

                if score >= beta {

                    if !is_capture {
                        state.killer_hist[ply].swap(1, 0);
                        state.killer_hist[ply][0] = best_move.clone();
                    }

                    hash_tt_entry!(
                        mv, beta, FBETA, depth, state, ttable
                    );

                    return beta;
                }

                if !is_capture {
                    state.search_hist[piece][end as usize] += bonus;
                }

                alpha = score;
            }
        }
    }

    if legal_moves == 0 {
        if in_check || stalemate_loss!(&state) {
            let mate_score = -INF + state.search_ply as i32;

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
        hash_tt_entry!(best_move, best_score, FEXACT, depth, state, ttable);
    } else {
        hash_tt_entry!(best_move, alpha, FALPHA, depth, state, ttable);
    }

    alpha
}
