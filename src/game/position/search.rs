//! search.rs
//!
//! Iterative deepening, alpha-beta search, and quiescence.
//!
//! Search uses two transposition tables, null-move and late-move pruning,
//! SEE move ordering, killer moves, and history. SearchInfo carries limits,
//! counters, and stop state.
//!
//! Created: 22/03/2026
//! Author : Alden Luthfi

use crate::*;

/// SearchInfo
///
/// Everything one search worker owns: limits, counters, stop flags, and the
/// principal variation, killer, and history tables it orders moves with.
/// These tables are search scratch, not game state, so a worker allocates
/// them once in `clear_search` and no `State` clone ever carries them.
#[derive(Default)]
pub struct SearchInfo {
    pub start_time: u128,                                                       /* start time since engine launch     */

    pub set_depth: usize,                                                       /* maximum search depth               */
    pub set_nodes: u128,                                                        /* node limit (0 = unlimited)         */

    pub deadline: u128,                                                         /* ns since launch (0 = unlimited)    */

    pub thread_count: usize,                                                    /* threads active in this search      */

    pub nodes: u128,                                                            /* total nodes searched so far        */

    pub interrupt: bool,                                                        /* flag set by external stop events   */

    pub pv_line: Vec<Move>,                                                     /* reported principal variation       */
    pub pv_table: Vec<Move>,                                                    /* flat triangular PV table           */
    pub pv_length: Vec<usize>,                                                  /* PV length per ply                  */

    pub search_hist: Vec<i16>,                                                  /* [piece * board_size + end]         */
    pub killer_hist: Vec<[Move; 2]>,                                            /* search ply to killer moves         */
}

/// SearchResult
///
/// Packaged outcome of one root search.
pub struct SearchResult {
    pub best_score: i32,                                                        /* best score at the root             */
    pub best_move: Move,                                                        /* best root move found               */
    pub ponder_move: Move,                                                      /* expected reply to best move        */
    pub total_nodes: u128,                                                      /* nodes searched, all threads        */
    pub total_elapsed: u128,                                                    /* wall time in nanoseconds           */
}

/// check_interrupt
///
/// Polls stop conditions and updates search interrupt state.
///
/// Params:
/// - info: &mut SearchInfo -> search whose interrupt flag is updated
#[inline(always)]
pub fn check_interrupt(info: &mut SearchInfo) {
    if info.interrupt {
        return;
    }

    if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
        let elapsed = ENGINE_START
            .elapsed()
            .as_nanos()
            .saturating_sub(info.start_time);

        log_3!(
            "SIGINT | Elapsed Time: {} | Nodes: {} | ",
            format_time(elapsed),
            info.nodes,
        );
        info.interrupt = true;
        return;
    }

    if info.set_nodes != 0 && info.nodes >= info.set_nodes {
        info.interrupt = true;
        return;
    }

    if info.deadline == 0 {
        return;
    }

    if ENGINE_START.elapsed().as_nanos() >= info.deadline {
        info.interrupt = true;
    }
}

/// clear_search
///
/// Resets search counters and allocates this worker's ordering tables and
/// principal variation storage at the sizes the position calls for.
///
/// Params:
/// - state : &mut State      -> position the tables are sized from
/// - ttable: &TTable         -> main table, aged one generation
/// - qtable: &QTable         -> qsearch table, aged one generation
/// - info  : &mut SearchInfo -> worker whose counters and tables are reset
pub fn clear_search(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    info: &mut SearchInfo,
) {
    info.start_time = ENGINE_START.elapsed().as_nanos();
    info.nodes = 0;
    info.interrupt = false;

    let piece_count = state.statics.pieces.len();
    let board_size = state.statics.board_size;

    info.search_hist = vec![0i16; piece_count * board_size];
    info.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];

    info.pv_line = vec![null_move(); MAX_DEPTH];
    info.pv_table = vec![null_move(); PV_STRIDE * PV_STRIDE];
    info.pv_length = vec![0; PV_STRIDE];

    ttable.age.fetch_add(1, Ordering::Relaxed);
    qtable.age.fetch_add(1, Ordering::Relaxed);

    state.search_ply = 0;
}

/// search_position
///
/// Runs one single-threaded search or dispatches lazy-SMP workers.
///
/// Params:
/// - state     : &mut State          -> root position to search
/// - table     : Arc<TTable>         -> shared transposition table
/// - qtable    : Arc<QTable>         -> shared quiescence table
/// - info      : &mut SearchInfo     -> limits and counters
/// - thread_num: usize               -> worker count
/// - dict      : Option<&Translator> -> translator for printed moves
///
/// Return:
/// SearchResult -> best move, score, ponder move, nodes, and elapsed time
pub fn search_position(
    state: &mut State,
    table: Arc<TTable>,
    qtable: Arc<QTable>,
    info: &mut SearchInfo,
    thread_num: usize,
    dict: Option<&Translator>,
) -> SearchResult {
    table.hit.store(0, Ordering::Relaxed);
    table.valid.store(0, Ordering::Relaxed);
    table.new_write.store(0, Ordering::Relaxed);
    table.over_write.store(0, Ordering::Relaxed);

    qtable.hit.store(0, Ordering::Relaxed);
    qtable.valid.store(0, Ordering::Relaxed);
    qtable.new_write.store(0, Ordering::Relaxed);
    qtable.over_write.store(0, Ordering::Relaxed);

    if is_terminal!(state) {
        info.nodes = 0;
        info.interrupt = false;
        state.search_ply = 0;

        return SearchResult {
            best_score: terminal_score!(state),
            best_move: null_move(),
            ponder_move: null_move(),
            total_nodes: 0,
            total_elapsed: 0,
        };
    }

    if thread_num <= 1 {
        info.thread_count = thread_num.max(1);
        iterative_deepening(
            state, &table, &qtable, info, 0, dict,
        )
    } else {
        let pool = ThreadPool::with_threads(
            state, Arc::clone(&table), Arc::clone(&qtable), thread_num,
        );
        pool.run(info, dict)
    }
}

/// log_table_stats
///
/// Logs main and quiescence table counters after a search.
///
/// Params:
/// - table : &TTable -> main table whose counters are reported
/// - qtable: &QTable -> qsearch table whose counters are reported
pub fn log_table_stats(table: &TTable, qtable: &QTable) {
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
}

/*----------------------------------------------------------------------------*\
                         ITERATIVE DEEPENING
\*----------------------------------------------------------------------------*/

/// iterative_deepening
///
/// Searches depths 1 through the requested limit with a full alpha-beta window.
/// Thread zero reports protocol output; helper threads search silently.
///
/// Params:
/// - state     : &mut State          -> root position to search
/// - ttable    : &TTable             -> shared transposition table
/// - qtable    : &QTable             -> shared quiescence table
/// - info      : &mut SearchInfo     -> limits and counters
/// - thread_num: usize               -> worker index
/// - dict      : Option<&Translator> -> translator for printed moves
///
/// Return:
/// SearchResult -> best move, score, ponder move, nodes, and elapsed time
pub fn iterative_deepening(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    info: &mut SearchInfo,
    thread_num: usize,
    dict: Option<&Translator>,
) -> SearchResult {
    let mut best_move = null_move();
    let mut best_score = 0;
    let start_time = ENGINE_START.elapsed().as_nanos();

    let max_parallelism = thread::available_parallelism()
        .map(|count| count.get())
        .unwrap_or(1) as u64;

    let mut total_nodes = 0;
    let mut total_elapsed = 0;

    clear_search(state, ttable, qtable, info);

    for depth in 1..=info.set_depth {
        let depth_start_nodes = info.nodes;
        let depth_start_time = ENGINE_START.elapsed().as_nanos();

        let score = alpha_beta(
            state, ttable, qtable, depth, -INF, INF, info, true,
        );

        fill_pv_line!(state, info, ttable, depth);

        if info.interrupt {
            break;
        }

        best_score = score;
        best_move = info.pv_line[0].clone();

        let depth_elapsed = ENGINE_START
            .elapsed()
            .as_nanos()
            .saturating_sub(depth_start_time);
        total_elapsed = ENGINE_START
            .elapsed()
            .as_nanos()
            .saturating_sub(start_time)
            .checked_div(1_000_000)
            .unwrap_or(0);

        let depth_nodes = info.nodes - depth_start_nodes;
        total_nodes = info.nodes;

        let depth_nps = depth_nodes
            .checked_mul(1_000_000_000)
            .and_then(|nodes| nodes.checked_div(depth_elapsed))
            .unwrap_or(0);
        let total_nps = total_nodes
            .checked_mul(1_000)
            .and_then(|nodes| nodes.checked_div(total_elapsed))
            .unwrap_or(0);

        let pv_line = info.pv_line
            .iter()
            .take(depth)
            .take_while(|mv| mv != &&null_move())
            .map(|mv| format_move(mv, state, dict))
            .collect::<Vec<String>>()
            .join(" ");

        log_3!(
            concat!(
                "(Thread {}) Score: {:>6} | Best Move: {:<8} | ",
                "Depth Nodes: {:>12} | NPS: {:>12}",
            ),
            thread_num,
            best_score,
            format_move(&best_move, state, dict),
            depth_nodes,
            depth_nps,
        );

        log_2!(
            "(Thread {}) Depth {:>2} | Time: {:>10} | Best Line: {}",
            thread_num,
            depth,
            format_time(depth_elapsed),
            pv_line,
        );

        let table_length = (ttable.len() as u64).max(1);
        let hashfull = ttable.new_write
            .load(Ordering::Relaxed)
            .min(table_length) * 1000 / table_length;
        let cpuload = (info.thread_count as u64 * 1000)
            .min(max_parallelism * 1000) / max_parallelism;

        if thread_num == 0 {
            let score = if best_score.abs() >= MATE_SCORE {
                let moves = (INF - best_score.abs() + 1) / 2;
                EngineScore::Mate(if best_score > 0 { moves } else { -moves })
            } else {
                EngineScore::CP(best_score)
            };

            emit(EngineEvent::Info {
                hashfull,
                cpuload,
                depth,
                score,
                nodes: total_nodes,
                time_ms: total_elapsed,
                nps: total_nps,
                pv: pv_line,
            });
        }
    }

    log_1!(
        concat!(
            "(Thread {}) ",
            "Search complete | Final Score: {:>6} | Best Move: {:<8} | ",
            "Total Nodes: {:>12} | Total Time: {:>10}"
        ),
        thread_num,
        best_score,
        format_move(&best_move, state, None),
        total_nodes,
        format_time(total_elapsed),
    );

    fill_pv_line!(state, info, ttable, info.set_depth);

    let ponder_move = if info.pv_line.first() == Some(&best_move) {
        info.pv_line
            .get(1)
            .filter(|mv| *mv != &null_move())
            .cloned()
            .unwrap_or_else(null_move)
    } else {
        null_move()
    };

    SearchResult {
        best_score,
        best_move,
        ponder_move,
        total_nodes,
        total_elapsed,
    }
}

/*----------------------------------------------------------------------------*\
                         QUIESCENCE SEARCH
\*----------------------------------------------------------------------------*/

/// quiescence_search
///
/// Capture-only negamax leaf search. Checked positions search every evasion;
/// other positions may stand pat and search captures only.
///
/// Params:
/// - state : &mut State      -> position searched, restored on return
/// - ttable: &TTable         -> main table, read for table-move ordering
/// - qtable: &QTable         -> quiescence table probed and updated
/// - alpha : i32             -> lower search bound
/// - beta  : i32             -> upper search bound
/// - info  : &mut SearchInfo -> node counters and interrupt polling
///
/// Return:
/// i32 -> stand-pat or best capture score within the window
#[hotpath::measure]
fn quiescence_search(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
) -> i32 {
    let mut alpha = alpha;

    if is_terminal!(state) {
        return terminal_score!(state);
    }

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    let in_check = is_in_check!(state.playing, state);
    let stand_pat = evaluate_position!(state);

    if !in_check {
        if stand_pat >= beta {
            return beta;
        }

        if stand_pat > alpha {
            alpha = stand_pat;
        }
    }

    if state.search_ply >= MAX_DEPTH as u32 {
        return stand_pat;
    }

    let qtable_entry = probe_qt_entry!(state, qtable, alpha, beta);
    let table_entry = probe_tt_entry!(state, ttable, alpha, beta, 1);

    if qtable_entry.0 {
        return qtable_entry.1;
    }

    let table_move = if qtable_entry.2 != null_pseudo_move() {
        Some(qtable_entry.2)
    } else if table_entry.2 != null_pseudo_move() {
        Some(table_entry.2)
    } else {
        None
    };

    let mut best_move = null_move();
    let alpha_start = alpha;
    let mut legal_moves = 0;
    let mut moves = Vec::with_capacity(64);
    let mut scores = Vec::with_capacity(64);
    let mut scratch = Vec::with_capacity(32);

    if in_check {
        generate_all_moves_and_drops(state, &mut moves, &mut scratch);
    } else {
        generate_all_captures(state, &mut moves, &mut scratch);
    }

    scores.resize(moves.len(), usize::MAX);

    for index in 0..moves.len() {
        pick_by_score!(
            state, info, &mut moves, &mut scores, index, &table_move
        );

        if !make_move!(state, moves[index].clone()) {
            continue;
        }

        legal_moves += 1;

        let score = -quiescence_search(
            state, ttable, qtable, -beta, -alpha, info,
        );

        undo_move!(state);

        if info.interrupt {
            return alpha;
        }

        if score > alpha {
            if score >= beta {
                hash_qt_entry!(moves[index], beta, FBETA, state, qtable);
                return beta;
            }

            best_move = moves[index].clone();
            alpha = score;
        }
    }

    if in_check && legal_moves == 0 {
        let outcome = state.termination.checkmate;
        let score = outcome_score!(state, outcome);

        return if outcome == Outcome::Loss && illegal_mating_drop!(state) {
            -score
        } else {
            score
        };
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start && best_move != null_move() {
        hash_qt_entry!(best_move, alpha, FEXACT, state, qtable);
    }

    alpha
}

/*----------------------------------------------------------------------------*\
                           ALPHA-BETA SEARCH
\*----------------------------------------------------------------------------*/

/// alpha_beta
///
/// Principal variation search with TT/QT, NMP, LMP, killers, and history.
/// The first legal move takes the full window and later moves take the
/// scout window `(-alpha - 1, -alpha)`, which either confirms the first
/// move is best or fails high and costs a full-window re-search. A node
/// already entered on a narrow window scouts at no extra cost, since its
/// scout window is the window it was given.
///
/// Params:
/// - state          : &mut State      -> position searched, restored on return
/// - ttable         : &TTable         -> main table probed and updated
/// - qtable         : &QTable         -> qsearch table used at leaf nodes
/// - depth          : usize           -> remaining depth in plies
/// - alpha          : i32             -> lower search bound
/// - beta           : i32             -> upper search bound
/// - info           : &mut SearchInfo -> node counters and interrupt polling
/// - allow_null_move: bool            -> whether NMP may run at this node
///
/// Return:
/// i32 -> best score within the window from side-to-move view
#[hotpath::measure]
pub fn alpha_beta(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    depth: usize,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
    allow_null_move: bool,
) -> i32 {
    let ply = state.search_ply as usize;
    let mut alpha = alpha;
    info.pv_length[ply] = ply;

    if is_terminal!(state) {
        return terminal_score!(state);
    }

    let declared = state.termination.repetition
        .as_ref().map(|repetition| repetition.occurrences);

    if ply > 0 && let Some(occurrences) = declared {
        let scan_limit = 64;
        let repeats = count_repetitions(state, scan_limit);

        if repeats >= occurrences {
            return match repetition_outcome(
                state, occurrences, scan_limit,
            ) {
                Some((outcome, _)) => outcome_score!(state, outcome),
                None => 0,
            };
        }

        if repeats >= 2 {
            return 0;
        }
    }

    if state.search_ply >= MAX_DEPTH as u32 {
        return evaluate_position!(state);
    }

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    let in_check = is_in_check!(state.playing, state);

    if depth == 0 {
        return quiescence_search(
            state, ttable, qtable, alpha, beta, info,
        );
    }

    let table_entry = probe_tt_entry!(state, ttable, alpha, beta, depth);
    let table_move = if table_entry.2 != null_pseudo_move() {
        Some(table_entry.2)
    } else {
        None
    };

    if table_entry.0 {
        return table_entry.1;
    }

    if allow_null_move
    && !in_check
    && depth > 2
    && ply > 0
    && state.game_phase != ENDGAME
    && state.big_pieces[state.playing as usize] > 0
    && evaluate_position!(state) >= beta
    {
        let reduction = (4 + depth / 4).min(depth);

        make_null_move!(state);

        let score = -alpha_beta(
            state,
            ttable,
            qtable,
            depth - reduction,
            -beta,
            -beta + 1,
            info,
            false,
        );

        undo_null_move!(state);

        if score >= beta {
            return beta;
        }
    }

    let board_size = state.statics.board_size;
    let history_bonus = (depth * depth) as i32;

    let mut moves = Vec::with_capacity(64);
    let mut scores = Vec::with_capacity(64);
    let mut scratch = Vec::with_capacity(32);

    generate_all_moves_and_drops(state, &mut moves, &mut scratch);
    scores.resize(moves.len(), usize::MAX);

    let mut best_move = null_move();
    let mut best_score = -INF;
    let mut legal_moves = 0;
    let alpha_start = alpha;

    for index in 0..moves.len() {
        pick_by_score!(
            state, info, &mut moves, &mut scores, index, &table_move
        );

        let mv = &moves[index];
        let piece = piece!(mv) as usize;
        let end = end!(mv) as usize;
        let history_index = piece * board_size + end;

        let is_capture = m_capture!(mv);
        let is_promotion = m_promotion!(mv);
        let is_drop = m_drop!(mv);
        let is_quiet = m_quiet!(mv);

        if ply > 0
        && !in_check
        && legal_moves > 0
        && !is_capture
        && !is_promotion
        && !is_drop
        && alpha.abs() < MATE_SCORE
        && legal_moves >= 3 + depth * depth
        {
            continue;
        }

        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;

        let wide_window = beta - alpha > 1;                                     /* alpha is fixed for this iteration  */

        let mut score = if legal_moves == 1 {
            -alpha_beta(
                state,
                ttable,
                qtable,
                depth - 1,
                -beta,
                -alpha,
                info,
                true,
            )
        } else {
            -alpha_beta(
                state,
                ttable,
                qtable,
                depth - 1,
                -alpha - 1,
                -alpha,
                info,
                true,
            )
        };

        if wide_window
        && legal_moves > 1
        && score > alpha
        && !info.interrupt
        {
            score = -alpha_beta(
                state,
                ttable,
                qtable,
                depth - 1,
                -beta,
                -alpha,
                info,
                true,
            );
        }

        undo_move!(state);

        if info.interrupt {
            return 0;
        }

        if score > best_score {
            best_score = score;
            best_move = moves[index].clone();

            if score > alpha {
                if score >= beta {
                    if is_quiet {
                        if info.killer_hist[ply][0] != best_move {
                            info.killer_hist[ply][1] =
                                info.killer_hist[ply][0].clone();
                            info.killer_hist[ply][0] = best_move.clone();
                        }

                        update_history(
                            &mut info.search_hist[history_index],
                            history_bonus,
                        );
                    }

                    hash_tt_entry!(
                        moves[index], beta, FBETA, depth, state, ttable
                    );

                    return beta;
                }

                if is_quiet {
                    update_history(
                        &mut info.search_hist[history_index],
                        history_bonus,
                    );
                }

                alpha = score;

                let next_ply = ply + 1;
                let child_length = info.pv_length[next_ply];
                info.pv_length[ply] = child_length;

                let (parent_row, child_rows) = info.pv_table
                    .split_at_mut(next_ply * PV_STRIDE);

                parent_row[ply * PV_STRIDE + ply] = moves[index].clone();

                for pv_index in next_ply..child_length {
                    parent_row[ply * PV_STRIDE + pv_index] =
                        child_rows[pv_index].clone();
                }
            }
        } else if is_quiet {
            update_history(
                &mut info.search_hist[history_index],
                -history_bonus,
            );
        }
    }

    if legal_moves == 0 {
        let outcome = if in_check {
            state.termination.checkmate
        } else {
            state.termination.stalemate
        };
        let score = outcome_score!(state, outcome);

        return if outcome == Outcome::Loss && illegal_mating_drop!(state) {
            -score
        } else {
            score
        };
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start {
        hash_tt_entry!(
            best_move, best_score, FEXACT, depth, state, ttable
        );
    } else {
        hash_tt_entry!(best_move, alpha, FALPHA, depth, state, ttable);
    }

    alpha
}

/// update_history
///
/// Applies one signed depth-squared change to a history cell.
///
/// Params:
/// - entry: &mut i16 -> history cell updated in place
/// - bonus: i32      -> signed bonus or malus
#[inline(always)]
fn update_history(entry: &mut i16, bonus: i32) {
    *entry = (*entry as i32 + bonus)
        .clamp(-HISTORY_BOUND, HISTORY_BOUND) as i16;
}
