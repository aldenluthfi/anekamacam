//! search.rs
//!
//! Alpha-beta search with iterative deepening, aspiration windows, and
//! quiescence.
//!
//! This is the engine's search core: it walks the game tree with a negamax
//! alpha-beta driver, deepens iteratively under aspiration windows, and
//! resolves tactically unstable leaves with a capture-only quiescence
//! search, all guided by the transposition tables and move-ordering
//! heuristics. SearchInfo and SearchBufs carry search-time limits, counters,
//! and scratch allocations; SearchResult packages the output.
//!
//! Created: 22/03/2026
//! Author : Alden Luthfi

use crate::*;

/// SearchInfo
///
/// Search limits, counters, and stop flags for an active search.
/// Carries time controls, depth/move constraints, node count, and interrupt
/// flag. Scratch and move buffers live in SearchBufs.
#[derive(Default)]
pub struct SearchInfo {
    pub start_time: u128,                                                       /* start time since engine launch     */

    pub set_depth: usize,                                                       /* maximum search depth               */
    pub set_nodes: u128,                                                        /* node limit (0 = unlimited)         */

    pub soft_deadline: u128,                                                    /* ns since launch (no depth starts)  */
    pub hard_deadline: u128,                                                    /* ns since launch (search aborts)    */

    pub thread_count: usize,                                                    /* threads active in this search      */

    pub root_depth: usize,                                                      /* current iterative-deepening depth  */

    pub nodes: u128,                                                            /* total nodes searched so far        */

    pub interrupt: bool,                                                        /* flag set by external stop events   */
}

/// SearchBufs
///
/// Per-thread scratch allocations reused across the full search tree.
/// move_buf holds pre-allocated per-ply move lists; scratch_buf is reused
/// for taken_pieces computations.
#[derive(Default)]
pub struct SearchBufs {
    pub move_buf: Vec<Vec<Move>>,                                               /* per-ply move lists, pre-allocated  */
    pub score_buf: Vec<Vec<usize>>,                                             /* pre-computed move ordering scores  */
    pub scratch_buf: Vec<u64>,                                                  /* reused scratch for taken_pieces    */
    pub see_move_buf: Vec<Move>,                                                /* reused LVA candidate list for SEE  */
    pub see_scratch_buf: Vec<u64>,                                              /* reused LVA leg scratch for SEE     */
    pub pawn_entry_buf: [Vec<(usize, Square, i32)>; 2],                         /* reused per-side pawn lists in eval */
}

/// SearchResult
///
/// Packaged outcome of one root search.
/// Carries the best move and its score, the ponder move extracted from
/// the principal variation, and aggregate node/time counters for
/// reporting. Returned by `iterative_deepening` and `search_position`.
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
/// A search is interrupted by the global stop flag, by reaching its node
/// limit, or by the wall clock passing `hard_deadline` (absolute
/// nanoseconds since engine launch; 0 disables the deadline).
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

    if info.hard_deadline == 0 {
        return;
    }

    if ENGINE_START.elapsed().as_nanos() >= info.hard_deadline {
        info.interrupt = true;
    }
}

/// clear_search
///
/// Resets search state before a fresh root search.
/// Zeroes node counters, interrupt flag, killer and history tables, and ply.
/// Board position is unchanged.
///
/// Params:
/// - state : &mut State      -> position whose search tables are reset
/// - ttable: &TTable         -> main table, aged one generation
/// - qtable: &QTable         -> qsearch table, aged one generation
/// - info  : &mut SearchInfo -> counters and flags to reset
/// - bufs  : &mut SearchBufs -> scratch buffers, (re)allocated if needed
pub fn clear_search(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
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
        bufs.see_move_buf = Vec::with_capacity(32);
        bufs.see_scratch_buf = Vec::with_capacity(32);
        bufs.pawn_entry_buf =
            [Vec::with_capacity(32), Vec::with_capacity(32)];
    }

    let piece_count: usize = state.statics.pieces.len();
    let board_size: usize = state.statics.board_size;
    let cont_dim: usize = piece_count * board_size;

    state.cont_hist = vec![0i16; 2 * cont_dim * cont_dim];
    state.search_hist = vec![0i16; piece_count * board_size * board_size];
    state.capt_hist =
        vec![0i16; piece_count * board_size * CAPT_HIST_BUCKETS];
    state.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];
    state.static_eval = vec![-INF; MAX_DEPTH];
    state.excluded = vec![null_pseudo_move(); MAX_DEPTH];

    ttable.age.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
    qtable.age.fetch_add(1, core::sync::atomic::Ordering::Relaxed);

    state.search_ply = 0;
}

/// search_position
///
/// Runs a full search from the root position.
/// Resets TT/QT stats, then dispatches to a single iterative_deepening call
/// when thread_count ≤ 1, or a ThreadPool when thread_count > 1. Callers
/// report table stats afterwards via `log_table_stats`.
///
/// Params:
/// - state     : &mut State          -> root position to search
/// - table     : Arc<TTable>         -> shared transposition table
/// - qtable    : Arc<QTable>         -> shared quiescence table
/// - ptable    : Arc<PTable>         -> shared pawn structure table
/// - info      : &mut SearchInfo     -> limits and counters for this search
/// - bufs      : &mut SearchBufs     -> scratch buffers for thread 0
/// - thread_num: usize               -> worker thread count
/// - dict      : Option<&Translator> -> translator for printed move names
///
/// Return:
/// SearchResult                      -> best move, score, ponder, and totals
pub fn search_position(
    state: &mut State,
    table: Arc<TTable>,
    qtable: Arc<QTable>,
    ptable: Arc<PTable>,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
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

    ptable.hit.store(0, Ordering::Relaxed);
    ptable.valid.store(0, Ordering::Relaxed);
    ptable.new_write.store(0, Ordering::Relaxed);
    ptable.over_write.store(0, Ordering::Relaxed);

    if thread_num <= 1 {
        info.thread_count = thread_num.max(1);
        iterative_deepening(
            state, &table, &qtable, &ptable, info, bufs, 0, dict,
        )
    } else {
        let pool = ThreadPool::with_threads(
            state, Arc::clone(&table), Arc::clone(&qtable),
            Arc::clone(&ptable), thread_num
        );
        pool.run(info, dict)
    }
}

/// log_table_stats
///
/// Logs TT, QT, and PT stat lines (new/over/hit/valid) for a finished
/// search.
/// Kept separate from `search_position` so callers on the UCI path can
/// emit `bestmove` before any log-file I/O happens.
///
/// Params:
/// - table : &TTable -> main table whose counters are reported
/// - qtable: &QTable -> qsearch table whose counters are reported
/// - ptable: &PTable -> pawn table whose counters are reported
pub fn log_table_stats(
    table: &TTable, qtable: &QTable, ptable: &PTable,
) {
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

    log_3!(
        "PT | new: {:<8} | over: {:<8} | hit: {:<8} | valid: {:<8}",
        ptable.new_write.load(Ordering::Relaxed),
        ptable.over_write.load(Ordering::Relaxed),
        ptable.hit.load(Ordering::Relaxed),
        ptable.valid.load(Ordering::Relaxed),
    );
}

/*----------------------------------------------------------------------------*\
                         ITERATIVE DEEPENING
\*----------------------------------------------------------------------------*/

/// iterative_deepening
///
/// Iterative deepening with aspiration windows over alpha_beta.
/// Runs depth 1..=set_depth. Depths below 4, or when the previous score is a
/// mate, use a full window. Otherwise aspiration windows start at a value-
/// derived half-window and widen the failing bound by half of the
/// current delta on each fail until the search falls within bounds.
/// Thread 0 prints UCI info lines per depth; helper threads skip output.
///
/// Params:
/// - state     : &mut State          -> root position to search
/// - ttable    : &TTable             -> shared transposition table
/// - qtable    : &QTable             -> shared quiescence table
/// - ptable    : &PTable             -> shared pawn structure table
/// - info      : &mut SearchInfo     -> limits and counters for this search
/// - bufs      : &mut SearchBufs     -> per-thread scratch buffers
/// - thread_num: usize               -> this worker's index (0 reports)
/// - dict      : Option<&Translator> -> translator for printed move names
///
/// Return:
/// SearchResult                      -> best move, score, ponder, and totals
pub fn iterative_deepening(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    ptable: &PTable,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
    thread_num: usize,
    dict: Option<&Translator>,
) -> SearchResult {

    let mut best_move = null_move();
    let mut best_score: i32 = 0;
    let start_time = ENGINE_START.elapsed().as_nanos();

    let mut stability: usize = 0;
    let mut previous_best = null_move();
    let mut previous_score: i32 = 0;

    clear_search(state, ttable, qtable, info, bufs);

    let max_par = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1) as u64;

    let mut total_nodes = 0;
    let mut total_elapsed = 0;
    let mut total_nps = 0;

    for depth in 1..=info.set_depth {
        info.root_depth = depth;

        let depth_start_nodes = info.nodes;
        let depth_start_time = ENGINE_START.elapsed().as_nanos();

        let score = if depth < 4 || best_score.abs() >= MATE_SCORE {
            alpha_beta(
                state, ttable, qtable, ptable,
                depth, -INF, INF, info, bufs, true
            )
        } else {
            let mut asp_delta = state.statics.aspiration_delta;
            let mut asp_alpha = best_score - asp_delta;
            let mut asp_beta  = best_score + asp_delta;
            loop {
                let s = alpha_beta(
                    state,
                    ttable, qtable, ptable,
                    depth, asp_alpha, asp_beta, info, bufs, true
                );

                if info.interrupt || (s > asp_alpha && s < asp_beta) {
                    break s;
                }

                asp_delta = asp_delta
                    .saturating_add(asp_delta / 2)
                    .min(INF);

                if s <= asp_alpha {
                    asp_alpha = (asp_alpha - asp_delta).max(-INF);
                } else {
                    asp_beta = (asp_beta + asp_delta).min(INF);
                }
            }
        };

        fill_pv_line!(state, ttable, depth);

        if info.interrupt {
            break;
        }

        best_score = score;
        best_move = state.pv_line[0].clone();

        if depth > 1 && best_move == previous_best {
            stability += 1;
        } else {
            stability = 0;
        }

        let score_dropped = depth > 1
            && best_score < previous_score - state.statics.draw_bias;

        previous_best = best_move.clone();
        previous_score = best_score;

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
            .and_then(|n| n.checked_div(depth_elapsed))
            .unwrap_or(0);
        total_nps = total_nodes
            .checked_mul(1_000)
            .and_then(|n| n.checked_div(total_elapsed))
            .unwrap_or(0);

        let pv_line = state.pv_line
            .iter()
            .take(depth)
            .take_while(|m| m != &&null_move())
            .map(|m| format_move(m, state, dict))
            .collect::<Vec<String>>()
            .join(" ");

        log_3!(
            concat!(
                "(Thread {}) Score: {:>6} | Best Move: {:<8} | ",
                "Depth Nodes: {:>12} | ",
                "NPS: {:>12}",
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
            pv_line
        );

        let tt_len = (ttable.len() as u64).max(1);

        let hashfull = ttable.new_write
            .load(Ordering::Relaxed)
            .min(tt_len) * 1000 / tt_len;

        let cpuload = (info.thread_count as u64 * 1000)
            .min(max_par * 1000) / max_par;

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

        if info.soft_deadline != 0 {
            let budget = info.soft_deadline.saturating_sub(start_time);

            let mut scaled = budget
                * TM_STABILITY_PCT[stability.min(TM_STABILITY_PCT.len() - 1)]
                / 100;

            if score_dropped {
                scaled = scaled * TM_SCORE_DROP_PCT / 100;
            }

            let effective =
                (start_time + scaled).min(info.hard_deadline);

            if ENGINE_START.elapsed().as_nanos() >= effective {
                break;
            }
        }
    }

    log_1!(
        concat!(
            "(Thread {}) ",
            "Search complete | Final Score: {:>6} | Best Move: {:<8} | ",
            "Total Nodes: {:>12} | Total Time: {:>10} | NPS: {:>12}"
        ),
        thread_num,
        best_score,
        format_move(&best_move, state, None),
        total_nodes,
        format_time(total_elapsed),
        total_nps,
    );

    fill_pv_line!(state, ttable, info.set_depth);

    let ponder_move = if state.pv_line.first() == Some(&best_move) {
        state.pv_line
            .get(1)
            .filter(|m| *m != &null_move())
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

/// quiescence_search
///
/// Capture-only negamax search at leaf nodes; reduces horizon effects
/// through the following steps:
///
/// - in-check evasions:
///   when the side to move is in check the node cannot stand pat, since a
///   legal reply is forced. All moves and drops are generated as evasions,
///   capture-only pruning is disabled, and a node with no legal evasion
///   returns a mate score instead of stand-pat.
///
/// - static exchange evaluation pruning:
///   statically simulates the capture sequence on the target square and skips
///   captures whose net material outcome is negative. Disabled while in check,
///   where every legal evasion must be searched.
///
/// - delta pruning:
///   skips captures whose captured value plus a value-derived safety margin
///   still cannot raise the stand-pat score above `alpha`. Disabled in the
///   endgame, for promotions, and while in check.
///
/// Params:
/// - state : &mut State      -> position searched, restored on return
/// - ttable: &TTable         -> main table (read for PV move ordering)
/// - qtable: &QTable         -> qsearch table probed and updated
/// - ptable: &PTable         -> shared pawn structure table
/// - alpha : i32             -> lower search bound
/// - beta  : i32             -> upper search bound
/// - info  : &mut SearchInfo -> node counters and interrupt polling
/// - bufs  : &mut SearchBufs -> per-thread scratch buffers
///
/// Return:
///
/// i32
/// stand-pat or best capture score within the window
#[hotpath::measure]
fn quiescence_search(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    ptable: &PTable,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
) -> i32 {
    let mut alpha = alpha;

    if state.game_over {
        return draw_score!(state);
    }

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    let ply = state.search_ply as usize;

    let in_check = is_in_check!(state.playing, state);

    /*-----------------------------------------------------------------------*\
                                       STAND PAT
    \*-----------------------------------------------------------------------*/

    let stand_pat = evaluate_position!(state, bufs, ptable);

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

    /*-----------------------------------------------------------------------*\
                               TRANSPOSITION TABLE PROBE
    \*-----------------------------------------------------------------------*/

    let qt_entry = probe_qt_entry!(state, qtable, alpha, beta);
    let tt_entry = probe_tt_entry!(state, ttable, alpha, beta, 1);

    if qt_entry.0 {
        return qt_entry.1;
    }

    let pv_move = if qt_entry.2 != null_pseudo_move() {
        Some(qt_entry.2)
    } else if tt_entry.2 != null_pseudo_move() {
        Some(tt_entry.2)
    } else {
        None
    };

    /*-----------------------------------------------------------------------*\
                                CAPTURE GENERATION
    \*-----------------------------------------------------------------------*/

    let mut best_move = null_move();
    let alpha_start = alpha;
    let mut legal_moves = 0;

    if in_check {
        generate_all_moves_and_drops(
            state, &mut bufs.move_buf[ply], &mut bufs.scratch_buf
        );
    } else {
        generate_all_captures(
            state, &mut bufs.move_buf[ply], &mut bufs.scratch_buf
        );
    }

    let n = bufs.move_buf[ply].len();
    bufs.score_buf[ply].clear();
    bufs.score_buf[ply].resize(n, usize::MAX);

    for i in 0..bufs.move_buf[ply].len() {
        pick_by_score!(
            state,
            &mut bufs.move_buf[ply], &mut bufs.score_buf[ply],
            i, &pv_move,
            &mut bufs.see_move_buf, &mut bufs.see_scratch_buf,
            [usize::MAX; 2]
        );

        let mv = &bufs.move_buf[ply][i];

        /*-------------------------------------------------------------------*\
                                       SEE PRUNING
        \*-------------------------------------------------------------------*/

        if !in_check
        && bufs.score_buf[ply][i] < WINNING_CAPTURE_SCORE as usize {            /* score band encodes the SEE sign    */
            continue;
        }

        /*-------------------------------------------------------------------*\
                                     DELTA PRUNING
        \*-------------------------------------------------------------------*/

        if !in_check {
            let promotion = promotion!(mv);
            let captured_value = victim_value!(mv, state);

            if stand_pat + captured_value as i32 + state.statics.delta_margin
                < alpha
            && state.game_phase != ENDGAME
            && !promotion
            {
                continue;
            }
        }

        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;

        let score = -quiescence_search(
            state, ttable, qtable, ptable, -beta, -alpha, info, bufs
        );

        undo_move!(state);

        if info.interrupt {
            return alpha;
        }

        if score > alpha {
            if score >= beta {
                hash_qt_entry!(
                    bufs.move_buf[ply][i], beta, FBETA, state, qtable
                );
                return beta;
            }

            best_move = bufs.move_buf[ply][i].clone();
            alpha = score;
        }
    }

    /*-----------------------------------------------------------------------*\
                                  CHECKMATE DETECTION
    \*-----------------------------------------------------------------------*/

    if in_check && legal_moves == 0 {
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

    /*-----------------------------------------------------------------------*\
                               TRANSPOSITION TABLE STORE
    \*-----------------------------------------------------------------------*/

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start && best_move != null_move() {
        hash_qt_entry!(best_move, alpha, FEXACT, state, qtable);
    }

    alpha
}

/// alpha_beta
///
/// Negamax alpha-beta with transposition table and PVS. Applies the search
/// techniques below, gated by the node conditions each names:
///
/// - check extension:
///   increments `depth` by one when the side to move is in check. Forced
///   replies are usually narrow, and the extra ply prevents the search from
///   terminating on a tactically unstable position.
///
/// - mate distance pruning:
///   tightens `alpha` and `beta` to the best mate score still reachable from
///   the current ply. Once the window collapses, no line below this node can
///   improve on an already-known mate, so the node returns immediately.
///
/// - transposition eval reuse:
///   entries cache the static evaluation beside the score; a hit replaces
///   the fresh evaluation call, and the stored score sharpens it into a
///   pruning eval whenever its bound brackets the eval. The pruning stages
///   below (reverse futility, razoring, null move, futility) read the
///   sharpened value; the improving flag keeps the raw evaluation.
///
/// - correction history:
///   `corr_hist` tracks, per side and pawn structure, how far raw static
///   evaluations have trailed search scores (`update_corr_hist!` at every
///   node whose bound can tighten the evaluation). The scaled correction
///   is added to the pruning eval read by the fail-high stages (reverse
///   futility, null move) only; razoring and futility keep the plain
///   eval — corrected evals feeding fail-low pruning explode drop-game
///   trees. The evaluation stored in the transposition table stays raw.
///
/// - reverse futility pruning:
///   at shallow, non-PV, non-check nodes, if the static evaluation exceeds
///   `beta` by a depth-scaled margin, the node cuts on `beta` without
///   searching, since even a passive reply is unlikely to drag the score below
///   `beta`.
///
/// - razoring:
///   at shallow non-check nodes with no PV move, if the static evaluation
///   falls far enough below `alpha`, a zero-window search at reduced depth
///   verifies the position cannot recover. On fail-low the node returns
///   `alpha` without a full search.
///
/// - null move pruning:
///   skips the side to move and searches with a null-window around `beta` at
///   depth reduced by `4 + depth/4` plus a term scaled by how far the static
///   evaluation sits above `beta`. If the opponent cannot improve even given
///   a free move, the node cuts on `beta`. Endgame nodes only try it at high
///   depth and must confirm the cut with a reduced verification search
///   without the null move, guarding zugzwang blind spots.
///
/// - futility pruning:
///   at shallow non-check non-PV nodes, marks the node futile when
///   `plain_eval + margin <= alpha`. Inside the move loop, any quiet,
///   non-promotion, non-drop move after the first legal one is skipped,
///   since it cannot plausibly raise the score above `alpha`.
///
/// - internal iterative reduction:
///   when no PV move is cached and depth is high enough, drops `depth` by one
///   before searching, since ordering is poor without a PV move and a
///   shallower search wastes less effort.
///
/// - singular extension, multicut, and negative extension:
///   at high depth with a lower-bound TT entry near the current depth, a
///   reduced zero-window search around `tt_score - depth * margin` runs
///   with the TT move excluded (`state.excluded` per ply disables the TT
///   cutoff, TT stores, and the move itself at that node). If everything
///   else fails low the TT move is singular and its search deepens by
///   one; if the exclusion search still clears `beta` the node multicuts
///   to `singular_beta`; if only the TT entry clears `beta` the TT move
///   searches one ply shallower.
///
/// - static exchange evaluation pruning:
///   at shallow non-PV non-check nodes, skips captures whose static exchange
///   evaluation is more negative than a depth-scaled margin, filtering out
///   captures that lose material.
///
/// - late move pruning:
///   at shallow non-check nodes, once enough legal quiet moves have been tried
///   (per `LMP_THRESHOLD[improving][depth]`), the remaining quiet moves are
///   skipped; killer/history ordering makes the tail unlikely to hold the best
///   move.
///
/// - late move reduction:
///   for non-killer moves past the third legal move, reduces the child depth
///   via the `reduction!` table (indexed by depth, move count, check state,
///   and move type). A reduced search returning above `alpha` triggers a
///   full-depth re-search.
///
/// - principal variation search:
///   the first legal move uses the full `[alpha, beta]` window; later moves
///   use a zero-width `[alpha, alpha+1]` window, far cheaper, and re-search
///   with the full window on fail-high.
///
/// Params:
/// - state : &mut State      -> position searched, restored on return
/// - ttable: &TTable         -> main table probed and updated
/// - qtable: &QTable         -> qsearch table for the leaf search
/// - ptable: &PTable         -> shared pawn structure table
/// - depth : usize           -> remaining depth in plies
/// - alpha : i32             -> lower search bound
/// - beta  : i32             -> upper search bound
/// - info  : &mut SearchInfo -> node counters and interrupt polling
/// - bufs  : &mut SearchBufs -> per-thread scratch buffers
/// - null  : bool            -> whether null-move pruning is still allowed
///
/// Return:
///
/// i32
/// best score within the window from the side to move's view
#[hotpath::measure]
pub fn alpha_beta(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    ptable: &PTable,
    mut depth: usize,
    mut alpha: i32,
    mut beta: i32,
    info: &mut SearchInfo,
    bufs: &mut SearchBufs,
    null: bool,
) -> i32 {

    if state.game_over {
        return draw_score!(state);
    }

    let ply = state.search_ply as usize;
    state.pv_length[ply] = ply;

    /*-----------------------------------------------------------------------*\
                                  REPETITION SCORING
    \*-----------------------------------------------------------------------*/

    if repetition_limit!(state)
    && ply > 0
    && state.position_hash_map
        .get(&state.position_hash)
        .copied()
        .unwrap_or(0) >= 2 {
        return draw_score!(state);
    }

    /*-----------------------------------------------------------------------*\
                                 MATE DISTANCE PRUNING
    \*-----------------------------------------------------------------------*/

    alpha = alpha.max(-INF + ply as i32);
    beta  = beta.min(INF - ply as i32 - 1);

    if alpha >= beta {
        return alpha;
    }

    if state.search_ply >= MAX_DEPTH as u32 {
        return evaluate_position!(state, bufs, ptable);
    }

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    let in_check = is_in_check!(state.playing, state);

    /*-----------------------------------------------------------------------*\
                                    CHECK EXTENSION
    \*-----------------------------------------------------------------------*/

    if in_check
    && ply + depth < info.root_depth + MAX_CHECK_EXTENSION {
        depth += 1;
    }

    if depth == 0 {
        return quiescence_search(
            state, ttable, qtable, ptable, alpha, beta, info, bufs
        );
    }

    /*-----------------------------------------------------------------------*\
                               TRANSPOSITION TABLE PROBE
    \*-----------------------------------------------------------------------*/

    let excluded_here = state.excluded[ply] != null_pseudo_move();

    let mut pv_move: Option<PseudoMove> = None;
    let tt_entry = probe_tt_entry!(state, ttable, alpha, beta, depth);

    if tt_entry.2 != null_pseudo_move() {
        pv_move = Some(tt_entry.2);
    }

    if tt_entry.0 && !excluded_here {
        return tt_entry.1;
    }

    /*-----------------------------------------------------------------------*\
                                  STATIC EVALUATION
    \*-----------------------------------------------------------------------*/

    let static_eval = if in_check {
        -INF
    } else if tt_entry.3 != -INF {
        tt_entry.3
    } else {
        evaluate_position!(state, bufs, ptable)
    };

    state.static_eval[ply] = static_eval;

    let corr =
        !in_check as i32 *
        state.corr_hist[corr_hist_index!(state)] as i32 / CORR_HIST_GRAIN;

    let plain_eval = if tt_entry.4 != -INF { tt_entry.4 } else { static_eval };
    let prune_eval = plain_eval + corr;

    /*-----------------------------------------------------------------------*\
                                    IMPROVING FLAG
    \*-----------------------------------------------------------------------*/

    let improving = !in_check
        && ply >= 2
        && state.static_eval[ply - 2] != -INF
        && static_eval > state.static_eval[ply - 2];

    /*-----------------------------------------------------------------------*\
                                REVERSE FUTILITY PRUNING
    \*-----------------------------------------------------------------------*/

    if depth < MAX_RFP_DEPTH
    && beta - alpha == 1
    && !in_check
    && beta.abs() < MATE_SCORE
    && prune_eval - state.statics.rfp_margin[improving as usize][depth]
    >= beta
    {
        return beta;
    }

    /*-----------------------------------------------------------------------*\
                                       RAZORING
    \*-----------------------------------------------------------------------*/

    if depth < MAX_RZR_DEPTH
    && !in_check
    && beta - alpha == 1
    && alpha.abs() < MATE_SCORE
    && state.game_phase != ENDGAME
    && plain_eval + state.statics.razor_margin[depth] < alpha
    {
        let score = quiescence_search(
            state, ttable, qtable, ptable, alpha, alpha + 1, info, bufs
        );

        if score <= alpha {
            return alpha;
        }
    }

    /*-----------------------------------------------------------------------*\
                                  NULL MOVE PRUNING
    \*-----------------------------------------------------------------------*/

    if null
    && !in_check
    && depth > MIN_NMP_DEPTH
    && prune_eval >= beta
    && state.search_ply > 0
    && (state.game_phase != ENDGAME || depth >= MIN_NMP_ENDGAME_DEPTH)
    && state.big_pieces[state.playing as usize]
    >= state.statics.nmp_min_material
    {
        let reduct = (
            4 + depth / 4 +
            (
                (prune_eval - beta) / state.statics.nmp_eval_div
            ).clamp(0, 3) as usize
        ).min(depth);

        make_null_move!(state);

        let score = -alpha_beta(
            state,
            ttable, qtable, ptable,
            depth - reduct, -beta, -beta + 1, info, bufs, false
        );

        undo_null_move!(state);

        if score >= beta {
            if state.game_phase != ENDGAME {
                return beta;
            }

            let verify = alpha_beta(
                state,
                ttable, qtable, ptable,
                depth - reduct, beta - 1, beta, info, bufs, false
            );

            if verify >= beta {
                return beta;
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                                       PROBCUT
    \*-----------------------------------------------------------------------*/

    let probcut_beta = beta.saturating_add(state.statics.probcut_margin);

    if depth >= MIN_PROBCUT_DEPTH
    && !in_check
    && !excluded_here
    && beta - alpha == 1
    && beta.abs() < MATE_SCORE
    && prune_eval >= beta
    && !(tt_entry.5 + PROBCUT_DEPTH_REDUCTION > depth                           /* TT already refutes this cut        */
        && tt_entry.7 < probcut_beta
        && tt_entry.7 != -INF)
    {
        generate_all_captures(
            state, &mut bufs.move_buf[ply], &mut bufs.scratch_buf
        );

        let capture_count = bufs.move_buf[ply].len();
        bufs.score_buf[ply].clear();
        bufs.score_buf[ply].resize(capture_count, usize::MAX);

        let mut tried = 0usize;

        for i in 0..capture_count {
            if tried >= PROBCUT_MAX_CAPTURES {
                break;
            }

            pick_by_score!(
                state,
                &mut bufs.move_buf[ply], &mut bufs.score_buf[ply],
                i, &pv_move,
                &mut bufs.see_move_buf, &mut bufs.see_scratch_buf,
                [usize::MAX; 2]
            );

            if bufs.score_buf[ply][i] < WINNING_CAPTURE_SCORE as usize {        /* score band encodes the SEE sign    */
                break;
            }

            let mv = bufs.move_buf[ply][i].clone();

            if !make_move!(state, mv) {
                continue;
            }

            tried += 1;

            let mut score = -quiescence_search(
                state, ttable, qtable, ptable,
                -probcut_beta, -probcut_beta + 1, info, bufs
            );

            if score >= probcut_beta {
                score = -alpha_beta(
                    state,
                    ttable, qtable, ptable,
                    depth - PROBCUT_DEPTH_REDUCTION,
                    -probcut_beta, -probcut_beta + 1, info, bufs, true
                );
            }

            undo_move!(state);

            if info.interrupt {
                return alpha;
            }

            if score >= probcut_beta {
                return probcut_beta;
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                                   FUTILITY PRUNING
    \*-----------------------------------------------------------------------*/

    let mut futile = false;
    let margin = state.statics.futility_margin[match state.game_phase {
        OPENING | SETUP => 0,
        MIDDLEGAME      => 1,
        ENDGAME         => 2,
        _ => unreachable!(),
    }];

    if depth < MAX_FUTILITY_DEPTH
    && !in_check
    && beta - alpha == 1
    && alpha.abs() < MATE_SCORE
    && plain_eval + margin[depth] <= alpha
    {
        futile = true;
    }

    /*-----------------------------------------------------------------------*\
                            INTERNAL ITERATIVE REDUCTION
    \*-----------------------------------------------------------------------*/

    if pv_move.is_none() && depth >= MIN_IIR_DEPTH {
        depth -= 1;
    }

    /*-----------------------------------------------------------------------*\
                          SINGULAR EXTENSION AND MULTICUT
    \*-----------------------------------------------------------------------*/

    let mut negative_extension = false;

    if depth >= MIN_SINGULAR_DEPTH
    && state.search_ply > 0
    && (state.search_ply as usize) < 2 * info.root_depth
    && !excluded_here
    && tt_entry.2 != null_pseudo_move()
    && tt_entry.5 >= depth - SINGULAR_TT_DEPTH_SLACK
    && tt_entry.6 != FALPHA
    && tt_entry.7.abs() < MATE_SCORE
    {
        let singular_beta =
            tt_entry.7 - depth as i32 * state.statics.singular_margin;

        state.excluded[ply] = tt_entry.2;

        let singular_score = alpha_beta(
            state,
            ttable, qtable, ptable,
            depth / 2, singular_beta - 1, singular_beta, info, bufs, false
        );

        state.excluded[ply] = null_pseudo_move();

        if singular_score >= singular_beta {
            if singular_beta >= beta {
                return singular_beta;
            } else if tt_entry.7 >= beta {
                negative_extension = true;
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                                    MOVE GENERATION
    \*-----------------------------------------------------------------------*/

    let mut best_move = null_move();
    let mut best_score = -INF;
    let mut legal_moves = 0;
    let alpha_start = alpha;

    let board_size = state.statics.board_size;
    let bs_sq = board_size * board_size;
    let bonus = HIST_BONUS_TABLE[depth.min(MAX_DEPTH - 1)];

    let cont_dim = state.statics.pieces.len() * board_size;
    let history_len = state.history.len();

    let mut cont_bases = [usize::MAX; 2];

    for plies_back in 0..2 {
        if history_len <= plies_back {
            break;
        }

        let previous =
            &state.history[history_len - 1 - plies_back].move_ply;

        if previous.0 == u128::MAX {
            continue;
        }

        let key = piece!(previous) as usize * board_size
            + end!(previous) as usize;

        cont_bases[plies_back] =
            plies_back * cont_dim * cont_dim + key * cont_dim;
    }

    generate_all_moves_and_drops(
        state, &mut bufs.move_buf[ply], &mut bufs.scratch_buf
    );

    let n = bufs.move_buf[ply].len();
    bufs.score_buf[ply].clear();
    bufs.score_buf[ply].resize(n, usize::MAX);

    let moves_len = bufs.move_buf[ply].len();

    for i in 0..moves_len {

        pick_by_score!(
            state,
            &mut bufs.move_buf[ply], &mut bufs.score_buf[ply],
            i, &pv_move,
            &mut bufs.see_move_buf, &mut bufs.see_scratch_buf,
            cont_bases
        );

        let mv = &bufs.move_buf[ply][i];

        if excluded_here && m_matches!(mv, state.excluded[ply]) {
            continue;
        }

        let is_tt_move = negative_extension
            && pv_move.as_ref().is_some_and(|pm| m_matches!(mv, pm));

        let piece = piece!(mv) as usize;
        let start = start!(mv) as usize;
        let end = end!(mv) as usize;
        let hist_idx = piece * bs_sq + start * board_size + end;
        let cont_key = piece * board_size + end;

        let is_capture = m_capture!(mv);
        let is_promotion = m_promotion!(mv);
        let is_drop = m_drop!(mv);
        let is_quiet = m_quiet!(mv);

        let capt_idx =
            if is_capture { capt_hist_index!(mv, state) } else { 0 };

        let dangerous_push = p_is_pawn!(&state.statics.pieces[piece])
            && !is_capture
            && !is_drop
            && state.statics.pawn_advancement[piece * board_size + end]
                >= DANGEROUS_PUSH_THRESHOLD;

        /*-------------------------------------------------------------------*\
                                    FUTILITY PRUNING
        \*-------------------------------------------------------------------*/

        if futile
        && legal_moves > 0
        && !is_capture
        && !is_promotion
        && !is_drop
        && !dangerous_push
        {
            continue;
        }

        /*-------------------------------------------------------------------*\
                                       SEE PRUNING
        \*-------------------------------------------------------------------*/

        if depth < MAX_SEE_DEPTH
        && legal_moves > 0
        && !in_check
        && beta - alpha == 1
        && alpha.abs() < MATE_SCORE
        && is_capture
        {
            let see = bufs.score_buf[ply][i] as i32                             /* losing band: 1M - h + SEE + capt   */
                - (LOSING_CAPTURE_SCORE - MAX_HIST_VALUE as i32);
            if see < -state.statics.see_margin[depth] {
                continue;
            }
        }

        /*-------------------------------------------------------------------*\
                                    LATE MOVE PRUNING
        \*-------------------------------------------------------------------*/

        if depth < MAX_LMP_DEPTH
        && !in_check
        && !is_capture
        && !is_promotion
        && !is_drop
        && !dangerous_push
        && alpha.abs() < MATE_SCORE
        && beta - alpha == 1
        && legal_moves >= LMP_THRESHOLD[improving as usize][depth] as usize
        {
            continue;
        }

        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;

        let mut reduction = 1;
        let mut score;

        /*-------------------------------------------------------------------*\
                                   LATE MOVE REDUCTION
        \*-------------------------------------------------------------------*/

        if depth >= MIN_LMR_DEPTH
        && legal_moves > 2 + 2 * (beta - alpha > 1) as usize
        {
            let opponent_in_check = is_in_check!(state.playing, state);         /* lazy: only LMR needs gives-check   */

            reduction += reduction!(
                state,
                depth,
                legal_moves,
                in_check,
                opponent_in_check,
                is_capture,
                is_promotion,
                is_drop
            );

            let cont_sum: i32 = cont_bases.iter()
                .filter(|&&base| base != usize::MAX)
                .map(|&base| state.cont_hist[base + cont_key] as i32)
                .sum();

            let hist_score =
                state.search_hist[hist_idx] as i32 + cont_sum;
            let hist_cutoff = 3 * MAX_HIST_VALUE as i32 / 4;

            if hist_score > hist_cutoff {
                reduction = reduction.saturating_sub(2).max(1);
            } else if hist_score > 0 {
                reduction = reduction.saturating_sub(1).max(1);
            }
            if hist_score < -(3 * MAX_HIST_VALUE as i32) / 4 {
                reduction = (reduction + 2).min(depth - 1);
            }
            if improving as usize == 1 {
                reduction = reduction.saturating_sub(1).max(1);
            }
            if dangerous_push {
                reduction = reduction.saturating_sub(1).max(1);
            }

            score = -alpha_beta(
                state,
                ttable, qtable, ptable,
                depth - reduction, -alpha - 1, -alpha, info, bufs, true
            );

            if score > alpha && reduction > 1 {
                score = -alpha_beta(
                    state,
                    ttable, qtable, ptable,
                    depth - 1, -alpha - 1, -alpha, info, bufs, true
                );
            }

            if score > alpha && beta - alpha > 1 {
                score = -alpha_beta(
                    state,
                    ttable, qtable, ptable,
                    depth - 1, -beta, -alpha, info, bufs, true
                );
            }
        }

        /*-------------------------------------------------------------------*\
                                        PVS SEARCH
        \*-------------------------------------------------------------------*/

        else if legal_moves > 1 {
            score = -alpha_beta(
                state,
                ttable, qtable, ptable,
                depth - 1, -alpha - 1, -alpha, info, bufs, true
            );

            if score > alpha && beta - alpha > 1 {
                score = -alpha_beta(
                    state,
                    ttable, qtable, ptable,
                    depth - 1, -beta, -alpha, info, bufs, true
                );
            }
        }


        else {
            let ext_depth = if is_tt_move {
                depth - 2
            } else {
                depth - 1
            };

            score = -alpha_beta(
                state,
                ttable, qtable, ptable,
                ext_depth, -beta, -alpha, info, bufs, true
            );
        }

        undo_move!(state);

        if info.interrupt {
            return 0;
        }

        if score > best_score {
            best_score = score;
            best_move = bufs.move_buf[ply][i].clone();

            if score > alpha {
                if score >= beta {

                    /*-------------------------------------------------------*\
                                        KILLER + HISTORY BONUS
                    \*-------------------------------------------------------*/

                    if is_quiet {
                        state.killer_hist[ply].swap(1, 0);
                        state.killer_hist[ply][0] = best_move.clone();
                        apply_history_gravity!(
                            state.search_hist[hist_idx], bonus
                        );

                        for &base in &cont_bases {
                            if base != usize::MAX {
                                apply_history_gravity!(
                                    state.cont_hist[base + cont_key], bonus
                                );
                            }
                        }
                    } else if is_capture {
                        apply_history_gravity!(
                            state.capt_hist[capt_idx], bonus
                        );
                    }

                    if !excluded_here {
                        update_corr_hist!(
                            state, static_eval, beta, depth, FBETA, is_capture
                        );

                        hash_tt_entry!(
                            bufs.move_buf[ply][i], beta, FBETA, depth,
                            static_eval, state, ttable
                        );
                    }

                    return beta;
                }

                /*-----------------------------------------------------------*\
                                           HISTORY BONUS
                \*-----------------------------------------------------------*/

                if is_quiet {
                    apply_history_gravity!(
                        state.search_hist[hist_idx], bonus
                    );

                    for &base in &cont_bases {
                        if base != usize::MAX {
                            apply_history_gravity!(
                                state.cont_hist[base + cont_key], bonus
                            );
                        }
                    }
                } else if is_capture {
                    apply_history_gravity!(
                        state.capt_hist[capt_idx], bonus
                    );
                }

                alpha = score;

                let next_ply = ply + 1;
                let child_len = state.pv_length[next_ply];

                state.pv_length[ply] = child_len;

                let (a, b) = state.pv_table
                    .split_at_mut(next_ply * PV_STRIDE);

                a[ply * PV_STRIDE + ply] = bufs.move_buf[ply][i].clone();

                for pv_index in next_ply..child_len {
                    a[ply * PV_STRIDE + pv_index] = b[pv_index].clone();
                }

            }
        }

        /*-------------------------------------------------------------------*\
                                      HISTORY MALUS
        \*-------------------------------------------------------------------*/

        else if is_quiet {
            apply_history_gravity!(
                state.search_hist[hist_idx], -bonus
            );

            for &base in &cont_bases {
                if base != usize::MAX {
                    apply_history_gravity!(
                        state.cont_hist[base + cont_key], -bonus
                    );
                }
            }
        }

        else if is_capture {
            apply_history_gravity!(
                state.capt_hist[capt_idx], -bonus
            );
        }
    }

    /*-----------------------------------------------------------------------*\
                                  CHECKMATE DETECTION
    \*-----------------------------------------------------------------------*/

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

        return draw_score!(state);
    }

    /*-----------------------------------------------------------------------*\
                               TRANSPOSITION TABLE STORE
    \*-----------------------------------------------------------------------*/

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if excluded_here {
        return alpha;
    }

    if alpha != alpha_start {
        update_corr_hist!(
            state, static_eval, best_score, depth, FEXACT,
            m_capture!(&best_move)
        );

        hash_tt_entry!(
            best_move, best_score, FEXACT, depth, static_eval, state, ttable
        );
    } else {
        update_corr_hist!(
            state, static_eval, alpha, depth, FALPHA, m_capture!(&best_move)
        );

        hash_tt_entry!(
            best_move, alpha, FALPHA, depth, static_eval, state, ttable
        );
    }

    alpha
}

/// apply_history_gravity!
///
/// Gravity-style update for a single butterfly history entry.
/// Formula: `entry += bonus - entry * |bonus| / MAX_HIST_VALUE`. Naturally
/// damps as `|entry|` approaches `MAX_HIST_VALUE`, preserving signal at
/// deep depths without saturation. `bonus` is signed; negate for malus.
///
/// Params:
/// - entry: &mut i16 -> history table cell updated in place
/// - bonus: i32      -> signed reward applied to the cell
#[macro_export]
macro_rules! apply_history_gravity {
    ($entry:expr, $bonus:expr) => {{
        let bound = MAX_HIST_VALUE as i32;
        let entry = ($entry as i32 * (bound - $bonus.abs()) / bound) + $bonus;

        $entry = entry.clamp(-bound, bound) as i16;
    }};
}

/// corr_hist_index!
///
/// Maps the current position onto its correction-history slot: the side to
/// move selects the table half and the pawn hash, masked to
/// `CORR_HIST_SIZE`, selects the entry within it. Positions sharing a pawn
/// structure share a correction; collisions are accepted as noise.
///
/// Params:
/// - state: &State -> position providing the side to move and pawn hash
///
/// Return:
/// usize           -> index into `state.corr_hist`
#[macro_export]
macro_rules! corr_hist_index {
    ($state:expr) => {{
        $state.playing as usize * CORR_HIST_SIZE + (
            $state.pawn_hash as usize & (CORR_HIST_SIZE - 1)
        )
    }};
}

/// update_corr_hist!
///
/// Blends the gap between a node's search score and its raw static
/// evaluation into the correction-history entry as a depth-weighted moving
/// average: `entry = (entry * (SCALE - w) + gap * GRAIN * w) / SCALE` with
/// `w = min(depth + 1, CORR_HIST_MAX_WEIGHT)`, clamped to
/// `CORR_HIST_LIMIT`. Nodes whose best move captures blend at the minimum
/// weight instead: their gap is mostly tactical, not a structural
/// evaluation error. Skips nodes searched in check (`eval == -INF`),
/// mate-bound scores, and scores whose bound cannot tighten the evaluation
/// (a fail-high below the evaluation or a fail-low above it).
///
/// Params:
///
/// - state: &mut State
///   position providing the correction table and its index
///
/// - eval: i32
///   raw static evaluation recorded at the node
///
/// - score: i32
///   score the node's search returned
///
/// - depth: usize
///   remaining depth, weights the blend
///
/// - flag: u8
///   TT bound flag (FEXACT/FBETA/FALPHA)
///
/// - capture: bool
///   whether the move that set the score captures
#[macro_export]
macro_rules! update_corr_hist {
    (
        $state:expr,
        $eval:expr,
        $score:expr,
        $depth:expr,
        $flag:expr,
        $capture:expr
    ) => {{
        if $eval != -INF
        && $score.abs() < MATE_SCORE
        && match $flag {
            FBETA  => $score > $eval,
            FALPHA => $score < $eval,
            _      => true,
        } {
            let index = corr_hist_index!($state);
            let entry = $state.corr_hist[index] as i32;
            let gap = ($score - $eval) * CORR_HIST_GRAIN;

            let weight = (1 * $capture as i32 + $depth as i32)
                .min(CORR_HIST_MAX_WEIGHT);

            let mixed = (
                entry * (CORR_HIST_SCALE - weight) + gap * weight
            ) / CORR_HIST_SCALE;

            $state.corr_hist[index] =
                mixed.clamp(-CORR_HIST_LIMIT, CORR_HIST_LIMIT) as i16;
        }
    }};
}

/// reduction!
///
/// Heuristic reduction for late move reduction, based on depth, move count,
/// checks, and move type. Returns a usize reduction value clamped to
/// `[0, depth - 1]`. Uses pre-computed integer tables (no f64 at call site).
///
/// Params:
///
/// - state: &State
///   supplies the precomputed LMR tables
///
/// - depth: usize
///   remaining depth, one table index
///
/// - moves: usize
///   legal moves tried so far, the other table index
///
/// - in_check: bool
///   whether the side to move is in check
///
/// - opponent_in_check: bool
///   whether the move gives check
///
/// - is_capture: bool
///   whether the move captures
///
/// - is_promotion: bool
///   whether the move promotes
///
/// - is_drop: bool
///   whether the move is a drop
///
/// Return:
/// usize -> plies to reduce the child search by
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
        let depth = $depth.min(MAX_DEPTH - 1);
        let idx   = depth * MAX_LMR_DEPTH + $moves.min(MAX_LMR_DEPTH - 1);
        let check = $in_check || $opponent_in_check;

        let base: usize = if $is_capture || $is_promotion || $is_drop {
            if check {
                $state.statics.capture_lmr_check[idx]
            } else {
                $state.statics.capture_lmr[idx]
            }
        } else if check {
            $state.statics.quiesce_lmr_check[idx]
        } else {
            $state.statics.quiesce_lmr[idx]
        } as usize;

        base.min($depth - 1)
    }};
}
