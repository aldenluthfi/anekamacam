//! search.rs
//!
//! Iterative deepening, alpha-beta search, and quiescence.
//!
//! One search is three nested loops:
//!
//! ```text
//! iterative_deepening   depth 1, 2, 3 ... each under an aspiration window
//!   alpha_beta          the tree proper: pruning, reductions
//!     quiescence_search the leaves: captures until nothing is hanging
//! ```
//!
//! Two hash tables keep earlier results, one for the tree and one for the
//! leaves. Each ply takes one static evaluation. Null moves, move counts and
//! the exchange simulation prune moves, and the history tables order them.
//! [`SearchInfo`] has the limits, counters and ordering tables of one
//! worker. Lazy SMP workers share only the hash tables.
//!
//! Created: 22/03/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              SEARCH WORKER STATE
\*----------------------------------------------------------------------------*/

/// SearchInfo
///
/// All data of one search worker: limits, node count, stop flags and the
/// learned tables.
///
/// - search_hist : `[move key]`, moves that worked anywhere
/// - cont_hist   : `[plies back][reply][move key]`, good replies
/// - corr_hist   : `[side][pawn key]`, the evaluation error
/// - killer_hist : `[ply]`, two quiet moves that cut here
///
/// Correction history changes the static evaluation, not the move order.
/// `clear_search` allocates the tables with the variant sizes. A cloned
/// [`State`] has none, so each worker has its own.
///
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

    pub search_hist: Vec<i16>,                                                  /* [move key]                         */
    pub cont_hist: Vec<i16>,                                                    /* [plies back][reply key][move key]  */
    pub corr_hist: Vec<i16>,                                                    /* [side][pawn hash] eval correction  */
    pub killer_hist: Vec<[Move; 2]>,                                            /* search ply to killer moves         */

    pub eval_stack: Vec<i32>,                                                   /* static score standing at each ply  */

    pub candidate_move: Move,                                                   /* best root move proved so far       */
}

/// move_key!
///
/// Gives the history cell of a move, `piece * board_size + end`. This is
/// the "move key" of this file. Continuation history nests two keys.
///
/// Params:
/// - mv        : &Move -> move to index
/// - board_size: usize -> number of squares, the key stride
///
/// Return:
/// usize               -> flat index into a history table
///
/// Notes:
/// The caller gives `board_size`, so the scoring loop reads `statics` only
/// once.
///
#[macro_export]
macro_rules! move_key {
    ($mv:expr, $board_size:expr) => {{
        piece!($mv) as usize * $board_size + end!($mv) as usize
    }};
}

/// CONTINUATION_PLIES
///
/// The number of continuation history tables. One table follows the last
/// move, and one follows the previous move of the same side. Tests showed
/// that the two help and a third does not.
///
const CONTINUATION_PLIES: usize = 2;

/// CONT_HIST_CELLS
///
/// The maximum continuation history size of one worker: 2^29 cells, one
/// gibibyte of `i16`. The dense size is the square of `pieces * squares`.
/// That is a few megabytes for chess, but three tebibytes for taikyoku
/// shogi.
///
/// The table size is the smaller of the dense size and `CONT_HIST_CELLS`.
/// The cell is the dense index modulo the table length. Thus a small table
/// is dense, and in a large table some replies share a cell.
///
/// Notes:
/// Tests compared the wrap with a hash of the index, a hash of the overflow
/// and masked move keys, on ten positions of daishogi and daidaishogi. The
/// wrap stayed within a few percent of the dense table. The hash was 4 to
/// 5% slower, and the mask cost daidaishogi 58% more nodes.
///
const CONT_HIST_CELLS: usize = 1 << 29;

/// cont_cell!
///
/// Gives the continuation history cell of a dense index. It is the index
/// modulo the table length.
///
/// Params:
/// - info       : &SearchInfo -> worker with the table
/// - dense_index: usize       -> `base + key`, from `continuation_bases`
///
/// Return:
/// usize                      -> the cell index in the table
///
#[macro_export]
macro_rules! cont_cell {
    ($info:expr, $dense_index:expr) => {
        $dense_index % $info.cont_hist.len()
    };
}

/// Correction history sizing
///
/// Correction history sizes. The table keeps the mean error between the
/// static evaluation and the search score under the pawn key. The next
/// evaluation of those pawns moves by that error.
///
/// - `CORR_HIST_SIZE`       : 16384, cells for each side
/// - `CORR_HIST_GRAIN`      : 64, stored units for each point
/// - `CORR_HIST_SCALE`      : 256, divisor of the blend
/// - `CORR_HIST_MAX_WEIGHT` : 16, maximum weight of one result
/// - `CORR_HIST_LIMIT`      : 64 points, maximum correction
///
/// Notes:
/// The grain keeps fractions of a point. `LIMIT` is in grain units, so the
/// widest cell fits its `i16`. The two sides have separate rows. Collisions
/// in one side stay, because the correction is limited and decays.
///
const CORR_HIST_SIZE: usize = 1 << 14;
const CORR_HIST_GRAIN: i32 = 64;
const CORR_HIST_SCALE: i32 = 256;
const CORR_HIST_MAX_WEIGHT: i32 = 16;
const CORR_HIST_LIMIT: i32 = 64 * CORR_HIST_GRAIN;

/// Late move reduction gates
///
/// Late move reduction gates. A derived surface gives the reduction. These
/// constants select the moves that can use it:
///
/// - depth < 3   : no reduction
/// - move 1, 2   : full depth, at a zero window
/// - move 1 to 4 : full depth, at a wide window
/// - other moves : `surface[depth][move number]`, minimum one ply
///
/// A wide window is a PV node. There a bad reduction costs the full line,
/// so more moves get full depth.
///
/// - `REDUCTION_MINIMUM_DEPTH` : minimum depth for a reduction
/// - `REDUCTION_MOVE_BASE`     : full depth moves at a zero window
/// - `REDUCTION_MOVE_WIDE`     : extra full depth moves at a wide window
///
const REDUCTION_MINIMUM_DEPTH: u32 = 3;
const REDUCTION_MOVE_BASE: u32 = 2;
const REDUCTION_MOVE_WIDE: u32 = 2;

/// ProbCut settings
///
/// ProbCut settings. A node far above beta will probably fail high. The
/// probe tries some winning captures against beta plus a derived margin.
/// If one holds at a smaller depth, the node returns that result.
///
/// - `MIN_PROBCUT_DEPTH`       : 5, minimum node depth
/// - `PROBCUT_MAX_CAPTURES`    : 3, maximum winning captures to try
/// - `PROBCUT_DEPTH_REDUCTION` : 4, search depth is depth minus 4
///
/// Notes:
/// The probe walks the captures in score order and stops at the first
/// capture that does not win. Quiescence runs before the reduced search.
///
const MIN_PROBCUT_DEPTH: usize = 5;
const PROBCUT_DEPTH_REDUCTION: usize = 4;
const PROBCUT_MAX_CAPTURES: usize = 3;

/// MIN_IIR_DEPTH
///
/// Minimum depth of internal iterative reduction. A node without a table
/// move has only history for its move order. It searches one ply less and
/// stores a table move for the next visit.
///
const MIN_IIR_DEPTH: usize = 4;

/// Aspiration window settings
///
/// Aspiration window settings. From the start depth, an iteration opens a
/// small window around the previous score. A score outside the window is
/// not proved, so the failed side widens and the iteration runs again.
///
/// ```text
/// depth < 4    -INF ├──────────────────────────────────┤ +INF
/// depth >= 4         previous - delta ├───┤ previous + delta
/// each fail          delta doubles, the failing side reopening from there
/// past 16 delta      that side gives up and opens to infinity
/// ```
///
/// - `ASPIRATION_WIDEN`       : growth factor of delta, over the scale
/// - `ASPIRATION_CLAMP`       : give-up multiple of the start delta
/// - `ASPIRATION_START_DEPTH` : first depth with a window
///
/// The two ratios are relative to `COEFFICIENT_SCALE`. Derivation gives the
/// start delta for each variant.
///
/// Notes:
/// A mate score skips the window. Mate scores change one ply at a time and
/// would fail each window.
///
const ASPIRATION_CLAMP: u32 = 16000;
const ASPIRATION_WIDEN: u32 = 2000;
const ASPIRATION_START_DEPTH: u32 = 4;

/*----------------------------------------------------------------------------*\
                            SEARCH SETUP AND CONTROL
\*----------------------------------------------------------------------------*/

/// SearchResult
///
/// The result of one root search. `completed_depth` counts only completed
/// iterations. An interrupted iteration has no proved score, so it does
/// not change this result.
///
pub struct SearchResult {
    pub best_score: i32,                                                        /* best score at the root             */
    pub best_move: Move,                                                        /* best root move found               */
    pub ponder_move: Move,                                                      /* expected reply to best move        */
    pub completed_depth: usize,                                                 /* deepest iteration that finished    */
    pub total_nodes: u128,                                                      /* nodes searched, all threads        */
    pub total_elapsed: u128,                                                    /* wall time in nanoseconds           */
}

/// check_interrupt
///
/// Tests the three stop conditions and sets the interrupt flag that each
/// node reads. The two search loops call it each 2048 nodes.
///
/// - system interrupt : a signal came, the stop is logged
/// - node limit       : the search used its node limit
/// - deadline         : the time limit has passed
///
/// Params:
/// - info: &mut SearchInfo -> search with the interrupt flag
///
/// Notes:
/// The tests are in cost order. A limit of zero means no limit. If the flag
/// is already set, the function returns at once, without a clock read.
///
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
/// Resets the node counters and allocates the ordering tables and the PV
/// storage of the worker. A move key is `pieces * squares` wide, so the
/// variant sets the sizes.
///
/// - search_hist : one cell for each key
/// - cont_hist   : plies × keys × keys, at most `CONT_HIST_CELLS`
/// - corr_hist   : 2 × 16384 cells
/// - killer_hist : one move pair for each ply
/// - pv_table    : stride squared
/// - pv_length   : one length for each ply
/// - eval_stack  : one score for each ply, plus one
///
/// Params:
/// - state : &mut State      -> position that sets the table sizes
/// - ttable: &TTable         -> main table, one generation older
/// - qtable: &QTable         -> quiescence table, one generation older
/// - info  : &mut SearchInfo -> worker to reset
///
/// Notes:
/// The hash tables are not cleared, only aged. Old entries are still
/// useful, and the replacement prefers older entries.
///
pub fn clear_search(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    info: &mut SearchInfo,
) {
    info.start_time = ENGINE_START.elapsed().as_nanos();
    info.nodes = 0;
    info.interrupt = false;
    info.candidate_move = null_move();

    let piece_count = state.statics.pieces.len();
    let board_size = state.statics.board_size;

    let move_keys = piece_count * board_size;

    info.search_hist = vec![0i16; move_keys];
    let cont_dense = CONTINUATION_PLIES * move_keys * move_keys;

    info.cont_hist = vec![0i16; cont_dense.min(CONT_HIST_CELLS)];
    info.corr_hist = vec![0i16; 2 * CORR_HIST_SIZE];
    info.killer_hist = vec![array::from_fn(|_| null_move()); MAX_DEPTH];

    info.pv_line = vec![null_move(); MAX_DEPTH];
    info.pv_table = vec![null_move(); PV_STRIDE * PV_STRIDE];
    info.pv_length = vec![0; PV_STRIDE];

    info.eval_stack = vec![EVAL_NONE; MAX_DEPTH + 1];

    ttable.age.fetch_add(1, Ordering::Relaxed);
    qtable.age.fetch_add(1, Ordering::Relaxed);

    state.search_ply = 0;
}

/// search_position
///
/// The search entry point of all protocols. It clears the counters and
/// then selects one of these:
///
/// - terminal root : the terminal score, `null_move` and depth zero
/// - one worker    : iterative deepening in the calling thread
/// - many workers  : a pool that shares the two tables
///
/// Params:
/// - state     : &mut State          -> root position to search
/// - table     : Arc<TTable>         -> shared transposition table
/// - qtable    : Arc<QTable>         -> shared quiescence table
/// - info      : &mut SearchInfo     -> limits and counters
/// - thread_num: usize               -> number of workers
/// - dict      : Option<&Translator> -> translator for the move text
///
/// Return:
/// SearchResult                      -> move, score, ponder, nodes, time
///
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
            completed_depth: 0,
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
/// Logs the counters of the two tables for one search, one line each:
///
/// - new   : a write to an empty slot
/// - over  : a write over an old entry
/// - hit   : a probe with a matching key
/// - valid : a hit that passed the consistency test
///
/// Params:
/// - table : &TTable -> main table to report
/// - qtable: &QTable -> quiescence table to report
///
/// Notes:
/// Hits above valid mean torn writes by other workers. Much more `over`
/// than `new` means the table is too small.
///
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
/// Searches the root again with one more ply each time, until the depth
/// limit or the clock stops it. Each pass fills the tables, so the next
/// pass has a good move order.
///
/// - depth 1 to 3 : full window
/// - depth 4 on   : small window around the last score, wider on a fail
/// - stop         : at the depth limit or the clock
///
/// An iteration that the clock stops is discarded, because its score is not
/// proved. The move of the previous depth is played. Depth 1 is different,
/// because there is no previous move. It keeps the best root move whose
/// subtree completed, but not its score.
///
/// Params:
/// - state     : &mut State          -> root position to search
/// - ttable    : &TTable             -> shared transposition table
/// - qtable    : &QTable             -> shared quiescence table
/// - info      : &mut SearchInfo     -> limits and counters
/// - thread_num: usize               -> worker index
/// - dict      : Option<&Translator> -> translator for the move text
///
/// Return:
/// SearchResult                      -> move, score, ponder, nodes, time
///
/// Notes:
/// After each iteration, the PV comes from the table again. Its second move
/// is the ponder move only if its first move is the best move. Only worker
/// zero sends protocol output. All workers log their depths.
///
pub fn iterative_deepening(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    info: &mut SearchInfo,
    thread_num: usize,
    dict: Option<&Translator>,
) -> SearchResult {
    let mut best_move = null_move();
    let mut best_score: i32 = 0;
    let mut completed_depth = 0;
    let start_time = ENGINE_START.elapsed().as_nanos();

    let max_parallelism = thread::available_parallelism()
        .map(|count| count.get())
        .unwrap_or(1) as u64;

    let mut total_nodes = 0;
    let mut total_elapsed = 0;

    clear_search(state, ttable, qtable, info);

    let scale = COEFFICIENT_SCALE as i64;
    let start_depth = ASPIRATION_START_DEPTH as usize;
    let opening_delta = state.statics.search.aspiration_delta as i64;
    let widen = ASPIRATION_WIDEN as i64;
    let widest = opening_delta * ASPIRATION_CLAMP as i64 / scale;

    for depth in 1..=info.set_depth {
        let depth_start_nodes = info.nodes;
        let depth_start_time = ENGINE_START.elapsed().as_nanos();

        let mut delta = opening_delta;
        let mut alpha = -INF;
        let mut beta = INF;

        if depth >= start_depth && best_score.abs() < MATE_SCORE {
            alpha = (best_score as i64 - delta).max(-INF as i64) as i32;
            beta = (best_score as i64 + delta).min(INF as i64) as i32;
        }

        let score = loop {
            let score = alpha_beta(
                state, ttable, qtable, depth, alpha, beta, info, true,
            );

            if info.interrupt {
                break score;
            }

            let failed_low = score <= alpha && alpha > -INF;
            let failed_high = score >= beta && beta < INF;

            if !failed_low && !failed_high {
                break score;
            }

            delta = (delta * widen / scale).max(delta + 1);                     /* the floor must never stall a widen */

            if failed_low {
                alpha = if delta > widest {
                    -INF
                } else {
                    (score as i64 - delta).max(-INF as i64) as i32
                };
            } else {
                beta = if delta > widest {
                    INF
                } else {
                    (score as i64 + delta).min(INF as i64) as i32
                };
            }
        };

        fill_pv_line!(state, info, ttable, depth);

        if info.interrupt {
            if completed_depth == 0 && info.candidate_move != null_move() {
                best_move = info.candidate_move.clone();
            }

            break;
        }

        best_score = score;
        best_move = info.pv_line[0].clone();
        completed_depth = depth;

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
        completed_depth,
        total_nodes,
        total_elapsed,
    }
}

/*----------------------------------------------------------------------------*\
                               QUIESCENCE SEARCH
\*----------------------------------------------------------------------------*/

/// quiescence_search
///
/// The leaf search. The evaluation of a board in an exchange is not good,
/// so the leaves play the exchange and then evaluate.
///
/// - in check  : all evasions, also drops, no stand pat
/// - otherwise : stand pat first, then captures while they win
///
/// A side in check cannot stand pat, because it must move. Out of check,
/// the loop stops at the first losing capture, because the ordering puts
/// all later captures lower. It also skips a capture whose victim plus a
/// margin is below alpha. The margin does not apply to a promotion or in
/// the endgame.
///
/// The early stop needs a monotone order. The capability mask turns it off
/// in variants where a capture also fills a hand, counts a check, or takes
/// two pieces.
///
/// An exchange is a capture sequence on **one square**. The first leaf can
/// play any capture, and that capture sets the square. All deeper leaves
/// capture only on that square. A capture on another square is a new plan
/// for the main tree.
///
/// Params:
///
///     state: &mut State
///     position to search, restored at the end
///
///     ttable: &TTable
///     main table, read only for the move order
///
///     qtable: &QTable
///     quiescence table to probe and update
///
///     alpha: i32
///     lower search bound
///
///     beta: i32
///     upper search bound
///
///     info: &mut SearchInfo
///     node counters and interrupt test
///
///     contested: Option<Square>
///     square of the exchange, `None` at the horizon, where any capture can
///     start one
///
/// Return:
///
///     i32
///     stand pat or best capture score in the window
///
/// Notes:
/// Without the square rule, quiescence does not stop when a winning capture
/// is always available. In taikyoku shogi, the flying generals made 86-ply
/// leaves, and 85% of those captures replied to nothing. The limit is now
/// the number of pieces that reach one square, as in the exchange
/// simulation. A node in check without a legal move gets the variant
/// result.
///
#[hotpath::measure]
pub fn quiescence_search(
    state: &mut State,
    ttable: &TTable,
    qtable: &QTable,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
    contested: Option<Square>,
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

    let repeats = count_repetitions(state, SEARCH_REPETITION_CAP);
    let table_key = search_key(state, repeats);
    let qtable_key = qsearch_key(state, repeats, in_check);

    let qtable_entry = probe_qt_entry!(state, qtable_key, qtable, alpha, beta);
    let table_entry =
        probe_tt_entry!(state, table_key, ttable, alpha, beta, 1);

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
    let ply = state.search_ply as usize;
    let mut lists = mem::take(&mut state.scratch.node_lists[ply]);
    let NodeLists { moves, scores, payload } = &mut lists;

    if in_check {
        generate_all_moves_and_drops(state, moves, payload);
    } else {
        generate_all_captures(state, moves, payload);
    }

    scores.clear();                                                             /* last node's scores answer for it   */
    scores.resize(moves.len(), usize::MAX);

    let delta = state.statics.search.qsearch_delta;
    let delta_prunable = !in_check && state.game_phase != ENDGAME;              /* a thin board plays for one capture */

    for index in 0..moves.len() {
        pick_by_score!(
            state, info, moves, scores, index, &table_move,
            &[usize::MAX; CONTINUATION_PLIES]                                   /* evasions answer a capture, not a   */
        );                                                                      /* line worth learning a reply to     */

        if recapture_order!(state)
        && static_movement!(state)
        && !in_check
        && scores[index] < LOSING_CAPTURE_SCORE as usize
        {
            break;                                                              /* ordered: every later one loses too */
        }

        if !wide_quiescence!(state)
        && !in_check
        && contested
            .is_some_and(|square| !m_takes_square!(&moves[index], square))
        {
            continue;                                                           /* a plan, not a reply to this square */
        }

        if delta_prunable
        && !m_promotion!(&moves[index])
        && stand_pat + victim_value!(&moves[index], state) + delta <= alpha {
            continue;
        }

        if !make_move!(state, moves[index].clone()) {
            continue;
        }

        legal_moves += 1;

        let score = -quiescence_search(
            state, ttable, qtable, -beta, -alpha, info,
            contested.or(Some(end!(&moves[index]) as Square)),
        );

        undo_move!(state);

        if info.interrupt {
            state.scratch.node_lists[ply] = lists;

            return alpha;
        }

        if score > alpha {
            if score >= beta {
                hash_qt_entry!(
                    moves[index], beta, FBETA, state, qtable_key, qtable
                );
                state.scratch.node_lists[ply] = lists;

                return beta;
            }

            best_move = moves[index].clone();
            alpha = score;
        }
    }

    state.scratch.node_lists[ply] = lists;

    if in_check && legal_moves == 0 {
        let (outcome, inverted) = no_move_verdict!(state, in_check);

        return outcome_score!(state, outcome) * (1 - 2 * inverted as i32);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start && best_move != null_move() {
        hash_qt_entry!(
            best_move, alpha, FEXACT, state, qtable_key, qtable
        );
    }

    alpha
}

/*----------------------------------------------------------------------------*\
                               ALPHA-BETA SEARCH
\*----------------------------------------------------------------------------*/

/// alpha_beta
///
/// The main tree: principal variation search for all nodes above the
/// leaves. The first legal move gets the full window. Each later move gets
/// a window one point wide first. Only a move that beats alpha there gets
/// the full window again.
///
/// ```text
/// move 1        alpha ├────────────────────┤ beta   the window in full
/// move 2 on     alpha ├┤ alpha + 1                  can it beat move 1
/// it could      alpha ├────────────────────┤ beta   so it is asked again
/// ```
///
/// The steps of a node, in order:
///
/// - terminal, repetition, ply limit : return before other work
/// - table probe                     : a bound to cut, a move to try first
/// - static evaluation               : once, corrected, kept for the ply
/// - razoring, futility, null move   : shortcuts, each tests the rules
/// - ProbCut                         : some captures against a higher beta
/// - move loop                       : ordered, reduced, searched again
///
/// Each shortcut tests the capability mask first: static score cuts, null
/// move, ProbCut, internal iterative reduction, losing capture skips and
/// late quiet move skips. Each is a claim about the game. Late move
/// reduction is not a shortcut, because a reduced move that beats alpha
/// gets full depth again.
///
/// Params:
///
///     state: &mut State
///     position to search, restored at the end
///
///     ttable: &TTable
///     main table to probe and update
///
///     qtable: &QTable
///     quiescence table for the leaves
///
///     depth: usize
///     remaining depth in plies
///
///     alpha: i32
///     lower search bound
///
///     beta: i32
///     upper search bound
///
///     info: &mut SearchInfo
///     node counters and interrupt test
///
///     allow_null_move: bool
///     true when null move pruning can run at this node
///
/// Return:
///
///     i32
///     best score in the window, for the side to move
///
/// Notes:
/// A repetition gets the variant result at the first closed cycle, before
/// the rule count, because either side can repeat again. With a perpetual
/// rule that blames an offender, the search waits for the rule count.
///
/// A stored bound cuts only a scout node. A wide window must find a move,
/// and a bound has no move. The stored move is used at all nodes.
///
/// Each valid hit gives the raw static evaluation. Its bound refines a
/// separate pruning score. The improving test keeps the raw evaluation.
///
/// Correction history applies only to fail-high pruning, reverse futility
/// and null move. Fail-low futility uses the raw score, because a corrected
/// score made drop game trees much larger in tests. All bounds update the
/// table.
///
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

    let repeats = count_repetitions(state, SEARCH_REPETITION_CAP);
    let declared = state.termination.repetition
        .as_ref().map(|repetition| repetition.occurrences);

    if ply > 0 && let Some(occurrences) = declared {
        let enough = if state.termination.perpetual.is_some() {                 /* a rule naming an offender fires on */
            occurrences                                                         /* its own count and not before; one  */
        } else {                                                                /* closed cycle answers all the rest  */
            REPETITION_CYCLE
        };

        if repeats >= enough {
            return match repetition_outcome(
                state, enough, SEARCH_REPETITION_CAP,
            ) {
                Some((outcome, _)) => outcome_score!(state, outcome),
                None => draw_score!(state),
            };
        }
    }

    if state.search_ply >= MAX_DEPTH as u32 {
        return evaluate_position!(state);
    }

    info.nodes += 1;
    if info.nodes & 2047 == 0 {
        check_interrupt(info);
    }

    alpha = alpha.max(-INF + ply as i32);                                       /* mated here bounds this node below  */
    let beta = beta.min(INF - ply as i32);                                      /* and mating here bounds it above    */

    if alpha >= beta {
        return alpha;
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    let in_check = is_in_check!(state.playing, state);
    let mut depth = depth;

    if depth == 0 {
        return quiescence_search(
            state, ttable, qtable, alpha, beta, info, None,
        );
    }

    let pv_node = beta - alpha > 1;                                             /* a window this wide wants a move    */

    let table_key = search_key(state, repeats);
    let table_entry =
        probe_tt_entry!(state, table_key, ttable, alpha, beta, depth);
    let table_move = if table_entry.2 != null_pseudo_move() {
        Some(table_entry.2)
    } else {
        None
    };

    if table_entry.0 && !pv_node {
        return table_entry.1;
    }

    let static_eval = if in_check {                                             /* a checked king is worth no score   */
        EVAL_NONE
    } else if table_entry.3 != EVAL_NONE {
        table_entry.3
    } else {
        evaluate_position!(state)
    };

    info.eval_stack[ply] = static_eval;

    let corr_index = correction_index(state);
    let correction = info.corr_hist[corr_index] as i32
        / CORR_HIST_GRAIN * !in_check as i32;
    let plain_eval = if table_entry.4 != EVAL_NONE {
        table_entry.4
    } else {
        static_eval
    };
    let prune_eval = plain_eval + correction;

    let improving = static_eval != EVAL_NONE
        && ply >= 2
        && info.eval_stack[ply - 2] != EVAL_NONE
        && static_eval > info.eval_stack[ply - 2];

    let deepest = RFP_DEPTH as usize;
    let row = improving as usize * (deepest + 1);                               /* the rising side asks for less      */

    if forward_pruning!(state)
    && !in_check
    && ply > 0
    && depth <= deepest
    && beta - alpha == 1
    && beta.abs() < MATE_SCORE
    && prune_eval - state.statics.search.rfp_margin[row + depth] >= beta
    {
        return beta;
    }

    if forward_pruning!(state)
    && static_movement!(state)
    && !in_check
    && depth < state.statics.search.razor_margin.len()
    && beta - alpha == 1
    && alpha.abs() < MATE_SCORE
    && state.game_phase != ENDGAME
    && plain_eval + state.statics.search.razor_margin[depth] < alpha
    {
        let score = quiescence_search(
            state, ttable, qtable, alpha, alpha + 1, info, None,
        );

        if score <= alpha {
            return alpha;
        }
    }

    if null_pruning!(state)
    && allow_null_move
    && !in_check
    && depth > 2
    && ply > 0
    && state.game_phase != ENDGAME
    && state.big_pieces[state.playing as usize] > 0
    && prune_eval >= beta
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

    let probcut_beta = beta.saturating_add(
        state.statics.search.probcut_margin
    ).min(INF);

    if forward_pruning!(state)
    && see_pruning!(state)
    && see_valid!(state)
    && static_movement!(state)
    && !in_check
    && depth >= MIN_PROBCUT_DEPTH
    && beta - alpha == 1
    && beta.abs() < MATE_SCORE
    && prune_eval >= beta
    {
        let mut lists = mem::take(&mut state.scratch.node_lists[ply]);
        let NodeLists { moves, scores, payload } = &mut lists;

        generate_all_captures(state, moves, payload);
        scores.clear();
        scores.resize(moves.len(), usize::MAX);

        let mut tried = 0;

        for index in 0..moves.len() {
            if tried >= PROBCUT_MAX_CAPTURES {
                break;
            }

            pick_by_score!(
                state, info, moves, scores, index, &table_move,
                &[usize::MAX; CONTINUATION_PLIES]
            );

            if scores[index] < WINNING_CAPTURE_SCORE as usize {
                break;
            }

            if !make_move!(state, moves[index].clone()) {
                continue;
            }

            tried += 1;

            let mut score = -quiescence_search(
                state, ttable, qtable,
                -probcut_beta, -probcut_beta + 1, info, None,
            );

            if score >= probcut_beta {
                score = -alpha_beta(
                    state,
                    ttable,
                    qtable,
                    depth - PROBCUT_DEPTH_REDUCTION,
                    -probcut_beta,
                    -probcut_beta + 1,
                    info,
                    true,
                );
            }

            undo_move!(state);

            if info.interrupt {
                state.scratch.node_lists[ply] = lists;

                return alpha;
            }

            if score >= probcut_beta {
                state.scratch.node_lists[ply] = lists;

                return probcut_beta;
            }
        }

        state.scratch.node_lists[ply] = lists;
    }

    let futility_depth = depth;

    if forward_pruning!(state)
    && static_movement!(state)
    && table_move.is_none()
    && depth >= MIN_IIR_DEPTH
    {
        depth -= 1;
    }

    let board_size = state.statics.board_size;
    let history_bonus = (depth * depth) as i32;
    let cont_bases = continuation_bases(state);

    let minimum_depth = REDUCTION_MINIMUM_DEPTH as usize;
    let move_base = REDUCTION_MOVE_BASE as usize;
    let move_wide = REDUCTION_MOVE_WIDE as usize;

    let futility_deepest = FUTILITY_DEPTH as usize;
    let lmp_deepest = LMP_DEPTH as usize;
    let see_deepest = SEE_PRUNE_DEPTH as usize;

    let futility_row = improving as usize * (futility_deepest + 1);
    let lmp_row = improving as usize * (lmp_deepest + 1);
    let lmp_slot = depth.min(lmp_deepest);                                      /* deeper nodes reuse the last row    */

    let mut lists = mem::take(&mut state.scratch.node_lists[ply]);
    let NodeLists { moves, scores, payload } = &mut lists;

    generate_all_moves_and_drops(state, moves, payload);
    scores.clear();                                                             /* last node's scores answer for it   */
    scores.resize(moves.len(), usize::MAX);

    let mut best_move = null_move();
    let mut best_score = -INF;
    let mut legal_moves = 0;
    let alpha_start = alpha;

    for index in 0..moves.len() {
        pick_by_score!(
            state, info, moves, scores, index, &table_move,
            &cont_bases
        );

        let mv = &moves[index];
        let history_index = move_key!(mv, board_size);

        let is_capture = m_capture!(mv);
        let is_promotion = m_promotion!(mv);
        let is_drop = m_drop!(mv);
        let is_quiet = m_quiet!(mv);

        let prunable = ply > 0
            && !in_check
            && legal_moves > 0
            && beta - alpha == 1
            && alpha.abs() < MATE_SCORE;

        if prunable && !is_capture && !is_promotion && !is_drop {
            if quiet_pruning!(state)
            && legal_moves >= state.statics.search.lmp_count[lmp_row + lmp_slot]
            {
                continue;
            }

            if forward_pruning!(state)
            && futility_depth <= futility_deepest
            && plain_eval
                + state.statics.search.futility_margin[
                    futility_row + futility_depth
                ]
                <= alpha
            {
                continue;
            }
        }

        if prunable
        && see_pruning!(state)
        && see_valid!(state)
        && static_movement!(state)
        && is_capture
        && !is_promotion
        && !is_drop
        && depth <= see_deepest
        && scores[index] != UNMAKEABLE_CAPTURE_SCORE
        && scores[index] < LOSING_CAPTURE_SCORE as usize
        && scores[index] as i32 - LOSING_CAPTURE_SCORE
            < -state.statics.search.see_allowance[depth]
        {
            continue;
        }

        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;

        let wide_window = beta - alpha > 1;                                     /* alpha is fixed for this iteration  */
        let move_gate = move_base + move_wide * wide_window as usize;

        let reduction = if depth >= minimum_depth
        && legal_moves > move_gate
        {
            let surface = match (
                is_capture || is_promotion || is_drop, in_check
            ) {
                (false, false) => &state.statics.search.reduction_quiet,
                (false, true) => &state.statics.search.reduction_quiet_check,
                (true, false) => &state.statics.search.reduction_tactical,
                (true, true) => &state.statics.search.reduction_tactical_check,
            };

            let depth_slot = depth.min(MAX_DEPTH - 1);
            let move_slot = legal_moves.min(REDUCTION_MOVE_CAP - 1);

            (surface[depth_slot * REDUCTION_MOVE_CAP + move_slot] as usize)
                .min(depth - 2)                                                 /* one ply always survives the cut    */
        } else {
            0
        };

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
                depth - 1 - reduction,
                -alpha - 1,
                -alpha,
                info,
                true,
            )
        };

        if reduction > 0
        && score > alpha
        && !info.interrupt
        {
            score = -alpha_beta(
                state,
                ttable,
                qtable,
                depth - 1,
                -alpha - 1,
                -alpha,
                info,
                true,
            );
        }

        if wide_window
        && legal_moves > 1
        && score > alpha
        && score < beta                                                         /* a terminal child escapes the clamp */
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
            state.scratch.node_lists[ply] = lists;

            return 0;
        }

        if score > best_score {
            best_score = score;
            best_move = moves[index].clone();

            if ply == 0 {
                info.candidate_move = best_move.clone();                        /* its whole subtree was searched     */
            }

            if score > alpha {
                if score >= beta {
                    if is_quiet {
                        if info.killer_hist[ply][0] != best_move {
                            info.killer_hist[ply][1] =
                                info.killer_hist[ply][0].clone();
                            info.killer_hist[ply][0] = best_move.clone();
                        }

                        update_histories(
                            info, &cont_bases, history_index, history_bonus,
                        );
                    }

                    update_correction(
                        &mut info.corr_hist[corr_index],
                        static_eval, beta, depth, FBETA, is_capture,
                    );
                    hash_tt_entry!(
                        moves[index], beta, FBETA, depth, static_eval,
                        state, table_key, ttable
                    );
                    state.scratch.node_lists[ply] = lists;

                    return beta;
                }

                if is_quiet {
                    update_histories(
                        info, &cont_bases, history_index, history_bonus,
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
            update_histories(
                info, &cont_bases, history_index, -history_bonus,
            );
        }
    }

    state.scratch.node_lists[ply] = lists;

    if legal_moves == 0 {
        let (outcome, inverted) = no_move_verdict!(state, in_check);

        return outcome_score!(state, outcome) * (1 - 2 * inverted as i32);
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start {
        update_correction(
            &mut info.corr_hist[corr_index],
            static_eval, best_score, depth, FEXACT, m_capture!(&best_move),
        );
        hash_tt_entry!(
            best_move, best_score, FEXACT, depth, static_eval,
            state, table_key, ttable
        );
    } else {
        update_correction(
            &mut info.corr_hist[corr_index],
            static_eval, alpha, depth, FALPHA, m_capture!(&best_move),
        );
        hash_tt_entry!(
            best_move, alpha, FALPHA, depth, static_eval,
            state, table_key, ttable
        );
    }

    alpha
}

/*----------------------------------------------------------------------------*\
                         HISTORY AND CORRECTION UPDATES
\*----------------------------------------------------------------------------*/

/// update_history
///
/// Adds one signed change to a history cell and clamps it to the history
/// bound. The caller uses depth squared, so a deep node counts more. The
/// quiet move that cut gets the bonus. Each other quiet move tried gets the
/// same value as a malus.
///
/// Params:
/// - entry: &mut i16 -> history cell to update
/// - bonus: i32      -> signed bonus or malus
///
/// Notes:
/// The clamp lets a cell forget. A cell at the top gains no more, but can
/// fall through the full range. Thus no decay pass is necessary.
///
#[inline(always)]
fn update_history(entry: &mut i16, bonus: i32) {
    *entry = (*entry as i32 + bonus)
        .clamp(-HISTORY_BOUND, HISTORY_BOUND) as i16;
}

/// correction_index
///
/// Gives the correction cell of the side to move and the pawn key:
///
/// ```text
/// index = side * 16384 + (pawn key & 16383)
/// ```
///
/// Params:
/// - state: &State -> position with the side and the pawn key
///
/// Return:
/// usize           -> index into `SearchInfo::corr_hist`
///
/// Notes:
/// Two pawn structures can share a cell. The correction is limited, so a
/// collision gives only a slightly worse estimate.
///
#[inline(always)]
fn correction_index(state: &State) -> usize {
    state.playing as usize * CORR_HIST_SIZE
        + (state.pawn_hash as usize & (CORR_HIST_SIZE - 1))
}

/// update_correction
///
/// Blends one gap between the static evaluation and the search score into
/// the cell of the pawn structure:
///
/// - gap    : `(score - eval)`, in grain units
/// - weight : 1 + depth for a quiet best move, 1 for a capture, max 16
/// - cell   : `(cell * (256 - weight) + gap * weight) / 256`
///
/// The bound decides if the node updates the cell:
///
/// - FBETA  : only when the score is above the evaluation
/// - FALPHA : only when the score is below the evaluation
/// - FEXACT : always
/// - mate   : never
///
/// Params:
/// - entry  : &mut i16 -> correction history cell to update
/// - eval   : i32      -> raw static evaluation of the node
/// - score  : i32      -> search score
/// - depth  : usize    -> remaining depth, the weight of quiet moves
/// - flag   : u8       -> score bound, `FEXACT`, `FBETA` or `FALPHA`
/// - capture: bool     -> true when the best move is a capture
///
/// Notes:
/// A capture has weight 1, because its gap is tactical, not a pawn fact.
/// A node in check updates the cell, but reads no correction.
///
#[inline(always)]
fn update_correction(
    entry: &mut i16,
    eval: i32,
    score: i32,
    depth: usize,
    flag: u8,
    capture: bool,
) {
    if eval != EVAL_NONE
    && score.abs() < MATE_SCORE
    && match flag {
        FBETA => score > eval,
        FALPHA => score < eval,
        _ => true,
    } {
        let gap = (score - eval) * CORR_HIST_GRAIN;
        let weight = (1 + depth as i32 * !capture as i32)
            .min(CORR_HIST_MAX_WEIGHT);
        let mixed = (
            *entry as i32 * (CORR_HIST_SCALE - weight) + gap * weight
        ) / CORR_HIST_SCALE;

        *entry = mixed.clamp(-CORR_HIST_LIMIT, CORR_HIST_LIMIT) as i16;
    }
}

/// continuation_bases
///
/// Gives the continuation row offsets for a reply at this node:
///
/// - slot 0 : one ply back, the opponent move to answer
/// - slot 1 : two plies back, the previous move of this side
///
/// ```text
/// base = (slot * keys + key of that move) * keys
/// cell = base + key of the reply being credited
/// ```
///
/// Params:
/// - state: &State               -> position with the last plies
///
/// Return:
/// [usize; CONTINUATION_PLIES]   -> row offsets, `usize::MAX` if unset
///
/// Notes:
/// A slot is `usize::MAX` when its ply does not exist or is a null move.
///
fn continuation_bases(state: &State) -> [usize; CONTINUATION_PLIES] {
    let board_size = state.statics.board_size;
    let move_keys = state.statics.pieces.len() * board_size;
    let played = state.history.len();

    let mut bases = [usize::MAX; CONTINUATION_PLIES];

    for plies_back in 0..CONTINUATION_PLIES {
        if played <= plies_back {
            break;
        }

        let previous = &state.history[played - 1 - plies_back].move_ply;

        if previous.0 == u128::MAX {
            continue;
        }

        let key = move_key!(previous, board_size);

        bases[plies_back] = (plies_back * move_keys + key) * move_keys;
    }

    bases
}

/// update_histories
///
/// Applies one signed change to all history tables of a quiet move, as a
/// move and as a reply. All tables get the same change.
///
/// - `search_hist[move]`      : always
/// - `cont_hist[base + move]` : once for each set row
///
/// Params:
/// - info : &mut SearchInfo -> worker with the tables
/// - bases: &[usize]        -> continuation rows, `usize::MAX` if unset
/// - index: usize           -> move key of the move
/// - bonus: i32             -> signed bonus or malus
///
#[inline(always)]
fn update_histories(
    info: &mut SearchInfo,
    bases: &[usize],
    index: usize,
    bonus: i32,
) {
    update_history(&mut info.search_hist[index], bonus);

    for base in bases.iter().filter(|&&base| base != usize::MAX) {
        let cell = cont_cell!(info, base + index);

        update_history(&mut info.cont_hist[cell], bonus);
    }
}
