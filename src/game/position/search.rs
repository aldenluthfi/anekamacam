//! search.rs
//!
//! Iterative deepening, alpha-beta search, and quiescence.
//!
//! One search is three nested loops, each asking less of the one below it:
//!
//! ```text
//! iterative_deepening   depth 1, 2, 3 ... each under an aspiration window
//!   alpha_beta          the tree proper: pruning, reductions, extensions
//!     quiescence_search the leaves: captures until nothing is hanging
//! ```
//!
//! What a node spends is decided by what the position has already said. Two
//! transposition tables answer for positions met before, one for the tree and
//! one for the leaves. One static evaluation is taken per ply and reused,
//! cutting against either bound. Null moves, move counts, and the exchange
//! simulation drop what cannot repay its depth, and that same simulation with
//! the history tables orders whatever is left. Quiescence drops captures the
//! simulation prices as losing and captures too small to reach alpha.
//!
//! [`SearchInfo`] holds the limits, counters, stop state, and every ordering
//! table one worker owns alone, so lazy-SMP workers share the transposition
//! tables and nothing else.
//!
//! Created: 22/03/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              SEARCH WORKER STATE
\*----------------------------------------------------------------------------*/

/// SearchInfo
///
/// Everything one search worker owns: its limits, its node count, its stop
/// flags, and the tables it remembers the tree with. Four of those tables are
/// what the worker has learned, each answering a different question:
///
/// - search_hist : `[move key]`, what has worked anywhere
/// - cont_hist   : `[plies back][reply][move key]`, what has worked as a
///                 reply
/// - corr_hist   : `[side][pawn key]`, how wrong evaluation was here
/// - killer_hist : `[ply]`, two quiet moves that cut here
///
/// The first three carry scores that decay toward whatever the search keeps
/// seeing; the last is a pair of moves per ply and nothing more. Correction
/// history is the odd one out in what it corrects: it adjusts the static
/// evaluation rather than the move order.
///
/// None of this belongs to the position. `clear_search` allocates the tables
/// at the sizes the variant calls for, and a cloned [`State`] carries none of
/// them, which is what lets lazy-SMP workers keep their own.
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
}

/// move_key!
///
/// The cell a move occupies in every history table: `piece * board_size +
/// end`, which is what "move key" means everywhere in this file. Both
/// history tables are flat vectors indexed this way, and continuation
/// history nests two of these keys, so the four sites that build one had
/// better build it identically.
///
/// `board_size` is passed rather than read from the state because the
/// scoring loops hoist it out, and one `statics` dereference per move is
/// not free on this path.
///
/// Params:
/// - mv        : &Move -> move whose history cell is wanted
/// - board_size: usize -> squares on the board, the key's stride
///
/// Return:
/// usize               -> flat index into a history table
#[macro_export]
macro_rules! move_key {
    ($mv:expr, $board_size:expr) => {{
        piece!($mv) as usize * $board_size + end!($mv) as usize
    }};
}

/// How far back a move is credited to what it answers. Continuation history
/// asks which reply worked after a given move, so one table follows the move
/// just played and another the side's own previous move. `CONTINUATION_PLIES`
/// is how many such tables there are: both were measured as load-bearing, and
/// a third table back never was.
const CONTINUATION_PLIES: usize = 2;

/// Correction history sizing
///
/// Correction history files how far static evaluation stood from what search
/// answered under the pawn key, and shifts the next evaluation of that pawn
/// skeleton by the running average of its own past error.
///
/// - `CORR_HIST_SIZE`       : 16384, cells a side, the pawn key masked
///                            down to a row
/// - `CORR_HIST_GRAIN`      : 64, stored units per point, divided out on
///                            read
/// - `CORR_HIST_SCALE`      : 256, denominator of the blend, a weight out
///                            of this
/// - `CORR_HIST_MAX_WEIGHT` : 16, the most one deep quiet result may pull
///                            a cell
/// - `CORR_HIST_LIMIT`      : 64, points the correction is ever allowed
///                            to reach
///
/// The grain buys resolution the average would otherwise round off: a blend
/// that moves a cell by a fraction of a point keeps that fraction until
/// enough of them add up to one. `LIMIT` is written in grain units so the
/// ceiling reads in points, and the widest cell still fits its `i16`.
///
/// Two sides share no rows because the same pawn skeleton is worth opposite
/// things to them, and collisions inside a side are left alone: a wrong
/// correction is bounded and decays, while a bigger table would not.
const CORR_HIST_SIZE: usize = 1 << 14;
const CORR_HIST_GRAIN: i32 = 64;
const CORR_HIST_SCALE: i32 = 256;
const CORR_HIST_MAX_WEIGHT: i32 = 16;
const CORR_HIST_LIMIT: i32 = 64 * CORR_HIST_GRAIN;

/// Late move reduction gate
///
/// How much a reduced move loses is read off a derived surface; these three
/// decide which moves get to consult it at all. A shallow node reduces
/// nothing, having too little depth left to give away, and the first moves of
/// every node are searched whole because ordering believes in them.
///
/// - depth < 3   : nothing here reduces
/// - move 1, 2   : searched whole, at a zero window
/// - move 1 to 4 : searched whole, at a wide window
/// - beyond that : `surface[depth][move number]`, never below one ply
///
/// A wide window means a principal variation node, where a reduction that
/// hides the better move costs the whole line rather than one bound, so twice
/// as many moves are searched whole before the surface is asked.
///
/// `REDUCTION_MINIMUM_DEPTH` is the depth nothing reduces under,
/// `REDUCTION_MOVE_BASE` the moves searched whole at a zero window, and
/// `REDUCTION_MOVE_WIDE` the moves a wide window adds to that.
const REDUCTION_MINIMUM_DEPTH: u32 = 3;
const REDUCTION_MOVE_BASE: u32 = 2;
const REDUCTION_MOVE_WIDE: u32 = 2;

/// ProbCut probe
///
/// A node standing well above beta is usually about to fail high, and a
/// winning capture is the cheapest way to show it. A few are tried against a
/// beta raised by the derived margin, and one that survives that raised bound
/// at a fraction of the depth stands in for the search the node was owed.
///
/// - depth ≥ 5  : shallower than this the probe costs as much as the node
/// - 3 captures : the most tried, and only while they price as winning
/// - depth − 4  : what a surviving capture is searched to, quiescence
///                first
///
/// The probe stays cheap by giving up early: the capture list is walked in
/// score order and abandoned at the first move that is not winning, so a node
/// with nothing to show pays for one pick and nothing else.
///
/// `MIN_PROBCUT_DEPTH` is the depth it starts at, `PROBCUT_MAX_CAPTURES` the
/// captures it will try, and `PROBCUT_DEPTH_REDUCTION` the plies taken off
/// the node's own depth to search one of them.
const MIN_PROBCUT_DEPTH: usize = 5;
const PROBCUT_DEPTH_REDUCTION: usize = 4;
const PROBCUT_MAX_CAPTURES: usize = 3;

/// Shallowest node reduced for having no table move
///
/// A node this deep with nothing in the table has never been searched, so its
/// move order rests on history alone and the first move is a guess. Paying
/// full depth for a guessed order is the expensive way to find the right one;
/// the node gives up a ply instead and leaves a table move behind, which the
/// next visit orders on for less than the ply was worth. `MIN_IIR_DEPTH` is
/// the shallowest depth that trade is made at.
const MIN_IIR_DEPTH: usize = 4;

/// Aspiration window widening
///
/// Past the start depth an iteration opens around the previous score rather
/// than at the full bounds, so most of the tree is cut against a window a few
/// points wide. A score outside it is not wrong, only unproven: the failing
/// side widens and the iteration is searched again.
///
/// ```text
/// depth < 4    -INF ├──────────────────────────────────┤ +INF
/// depth >= 4         previous - delta ├───┤ previous + delta
/// each fail          delta doubles, the failing side reopening from there
/// past 16 delta      that side gives up and opens to infinity
/// ```
///
/// Both ratios are read against `COEFFICIENT_SCALE`, `ASPIRATION_WIDEN` as
/// the factor the delta grows by and `ASPIRATION_CLAMP` as the multiple of
/// the opening half-width it gives up at, that half-width being itself
/// derived per variant. `ASPIRATION_START_DEPTH` is the first iteration
/// opened around a previous score at all.
///
/// A previous score already in mate range skips the window outright: mate
/// scores step by a ply at a time and would fail every window on the way in.
const ASPIRATION_CLAMP: u32 = 16000;
const ASPIRATION_WIDEN: u32 = 2000;
const ASPIRATION_START_DEPTH: u32 = 4;

/*----------------------------------------------------------------------------*\
                            SEARCH SETUP AND CONTROL
\*----------------------------------------------------------------------------*/

/// SearchResult
///
/// Packaged outcome of one root search.
///
/// `completed_depth` counts only iterations that finished. An iteration cut
/// short by the clock leaves a score that no window ever confirmed, so it
/// updates nothing here and cannot be mistaken for a deeper answer than the
/// last one that was actually reached.
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
/// Asks the three things that end a search early and raises the flag every
/// node reads. Both search loops call this once every 2048 nodes, often
/// enough that a stop lands in milliseconds and rarely enough that the clock
/// reading behind it costs nothing measurable.
///
/// - system interrupt : a signal arrived, and the stop is logged
/// - node limit       : the search was given a node budget and spent it
/// - deadline         : the clock the time manager set has run out
///
/// Params:
/// - info: &mut SearchInfo -> search whose interrupt flag is updated
///
/// Notes:
/// The checks stand in cost order, and a limit left at zero means unlimited
/// rather than immediately exceeded. A flag already raised returns at once:
/// the search unwinds through many nodes after a stop, and none of them
/// should pay for a clock reading whose answer cannot change.
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
/// Resets node state and allocates this worker's ordering tables and principal
/// variation storage at the sizes the position calls for. Nothing here is
/// sized by a constant alone: a move key is `pieces * squares` wide, so a
/// variant decides how much a worker remembers.
///
/// - search_hist : one cell per key
/// - cont_hist   : plies × keys × keys
/// - corr_hist   : 2 × 16384 cells
/// - killer_hist : one move pair per ply
/// - pv_table    : stride squared
/// - pv_length   : one length per ply
/// - eval_stack  : one score per ply, and one past the deepest
///
/// Continuation history squares the key count, which is what makes it the
/// one table a large board pays real memory for, and why the ply count it
/// nests is kept at what was measured to earn its size.
///
/// The transposition tables are not cleared, only aged: what the last search
/// learned is still worth reading, and a generation older is enough for
/// replacement to prefer overwriting it.
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

    let move_keys = piece_count * board_size;

    info.search_hist = vec![0i16; move_keys];
    info.cont_hist = vec![0i16; CONTINUATION_PLIES * move_keys * move_keys];
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
/// The door every protocol reaches search through. It zeroes the counters a
/// search reports on, answers a root that is already over without searching
/// it, and then runs one worker here or hands the position to a pool.
///
/// - terminal root : its terminal score, no move, and depth zero
/// - one worker    : iterative deepening runs in the calling thread
/// - many workers  : a pool, sharing the two tables and nothing besides
///
/// A finished root reports `null_move` rather than a legal-looking move. It
/// has nothing legal to play and nothing to search, and a caller asking a
/// finished game for a move is better answered plainly than invented at.
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
/// Reports what both tables did over one search, a line each. The four
/// counters read as two pairs: what was written, and what came back.
///
/// - new   : a key landed in a slot that held nothing
/// - over  : a key replaced an entry that was already there
/// - hit   : a probe matched the key it was looking for
/// - valid : that hit survived the consistency check and was used
///
/// Hits above valid mean entries are being torn by other workers writing the
/// same slot, and overwrites far above new mean the table is too small for
/// the search it was asked to hold.
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
/// Searches the root again and again, one ply deeper each time, until the
/// depth limit or the clock ends it. Re-searching is cheaper than it sounds:
/// each pass leaves the tables full of what it learned, and the pass after it
/// spends most of its time confirming that order rather than finding it.
///
/// - depth 1 : opens at the full bounds and keeps the score
/// - depth 2 : the same, its order already improved by depth 1
/// - depth 3 : the same, and the score is now worth aspiring around
/// - depth 4 : opens narrow around the last score, widening on a fail
/// - onward  : until the depth limit, or the clock, ends the loop
///
/// An iteration the clock cuts through is thrown away whole. Its root move
/// was searched under a window that never closed, so the answer it holds is
/// unproven, and the previous depth's move is the one that gets played.
///
/// The line is refilled from the table after every iteration, and the second
/// move of it is offered as the ponder move, but only when the first is still
/// the move being played: a table that has moved on since would otherwise
/// hand back a reply to something else entirely.
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
///
/// Notes:
/// Only worker zero emits protocol information. The rest search the same root
/// under their own tables and report nothing, their whole contribution being
/// what they leave in the shared transposition tables for worker zero to
/// find. Logging is not so restricted: every worker logs its own depths, the
/// thread number leading the line.
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
/// The leaf search. Evaluation of a board mid-exchange is worth little, so
/// the leaves play the exchanges out and evaluate what is left standing.
///
/// - in check  : every evasion, drops among them, and no standing pat
/// - otherwise : stand pat first, then captures while they price as won
///
/// Standing pat is the claim that doing nothing is already worth at least
/// what the captures on offer are, which a checked side cannot make: it has
/// no option to do nothing. A position not in check stops at the first
/// capture ordering prices as losing, since every capture behind it is priced
/// no better, and skips a capture whose victim plus a margin still stands
/// under alpha. The margin is not applied to a promotion, nor in an endgame,
/// where a single capture is most of what is left to play for.
///
/// Stopping early is a claim that the ordering is monotone, and a variant
/// where a capture also fills a hand, counts toward a check tally, or takes
/// two pieces at once orders no such thing, so the capability mask decides
/// whether that stop is available and the rest of the list is searched where
/// it is not.
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
///
/// Notes:
/// Both tables are read here and only one is written. The quiescence table
/// answers for these nodes and stores their results; the main table is asked
/// for a move worth trying first and nothing else, since a bound proved over
/// captures alone would be a lie at any real depth.
///
/// A checked node that finds no legal move is over, and the verdict comes
/// from the variant rather than from here: what checkmate is worth, and to
/// whom, is a rule and not an assumption this file gets to make.
#[hotpath::measure]
pub fn quiescence_search(
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
/// The tree proper: principal variation search over a window, every node
/// above the leaves. The first legal move is searched on the whole window;
/// every move after it is asked one question instead, on a window one point
/// wide, and only a move that answers yes is searched properly.
///
/// ```text
/// move 1        alpha ├────────────────────┤ beta   the window in full
/// move 2 on           alpha ├┤ alpha + 1            can it beat move 1
/// it could      alpha ├────────────────────┤ beta   so it is asked again
/// ```
///
/// A node handed a narrow window scouts for nothing, its scout window being
/// the window it already has, which is why the scouting spreads downward.
///
/// What a node does, in the order it does it:
///
/// - terminal, repetition, ply cap : answered before a node is spent at
///                                   all
/// - table probe                   : a bound to cut on, a move to try
///                                   first
/// - static evaluation             : taken once, corrected, kept for the
///                                   ply
/// - razoring, futility, null move : the shortcuts, each asking the rules
/// - ProbCut                       : a few captures against a raised beta
/// - move loop                     : ordered, reduced, and re-searched
///
/// Razoring asks quiescence to rescue a shallow fail-low before the full node;
/// ProbCut asks at most three winning captures to prove a surplus above beta;
/// internal iterative reduction gives up one ply when no table move exists.
/// All three skip work and use the capability claims below.
///
/// A repeated position scores the outcome its own variant declares, and where
/// no rule names an offender it is scored from the first closed cycle rather
/// than from the occurrence count the rule states. A position standing for the
/// second time can be walked back to a third by whichever side wants it, so
/// the search reads the cycle's result as the result of the line, several
/// plies before the rule itself would fire. A variant whose perpetual rule
/// blames whoever sustained the cycle waits for that count instead, since
/// which side is at fault is not settled until the rule fires, and a line one
/// cycle short of it can still be won outright by either colour.
///
/// Every shortcut that answers without full search — standing on a static
/// score, giving up the move, proving beta through selected captures, reducing
/// a node with no table move, skipping a losing capture, dropping a late quiet
/// move — asks the capability mask first. Each is an argument about the game
/// rather than about the position, and a variant that never makes the
/// argument has its moves searched instead. The late-move reduction
/// below is not among them: a reduced search that beats alpha is repeated at
/// full depth, so it reorders work without ever dropping a move, and no rule
/// can make that unsound.
///
/// A stored bound cuts a scout node but not a node opened on a wide window.
/// A scout asks one question and a bound answers it; a wide window is asking
/// which move to play, and a bound names no move. Cutting there returns a
/// score with an empty principal variation and makes the answer depend on how
/// large the table is, which is not a property of the position. The stored
/// move is still read at every node, since ordering is what it was for.
///
/// Every valid table hit also carries the raw static evaluation, even when its
/// searched depth is too shallow for a cutoff. Its bound sharpens a separate
/// pruning score; the improving test keeps the raw evaluation, so a prior
/// search result never pretends the position itself got better.
///
/// Correction history learns, per side and pawn placement, how far raw static
/// evaluation trails searched scores. Its correction feeds only fail-high
/// pruning (reverse futility and null move); fail-low futility keeps the raw
/// score, since corrected fail-low pruning was measured to explode drop-game
/// trees. Every searched bound can teach the table, not only a fail-high.
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
            state, ttable, qtable, alpha, beta, info,
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
            state, ttable, qtable, alpha, alpha + 1, info,
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
                -probcut_beta, -probcut_beta + 1, info,
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
/// Adds one signed change to a history cell and holds it inside the bound
/// every ordering score is read against. The caller sizes the change by depth
/// squared, so a deep node's verdict outweighs a shallow node's guess, and
/// the sign says which verdict it was: the quiet move that cut earns the
/// bonus, and every quiet move tried and beaten earns the same as a malus.
///
/// Params:
/// - entry: &mut i16 -> history cell updated in place
/// - bonus: i32      -> signed bonus or malus
///
/// Notes:
/// Saturating at the bound is what lets a cell forget. A move pinned at the
/// ceiling gains nothing more from working again, while it still has the
/// whole range to fall through once it stops, so a move that has gone stale
/// sinks back down without any decay pass sweeping the table.
#[inline(always)]
fn update_history(entry: &mut i16, bonus: i32) {
    *entry = (*entry as i32 + bonus)
        .clamp(-HISTORY_BOUND, HISTORY_BOUND) as i16;
}

/// correction_index
///
/// Maps side to move and pawn placement onto one worker-local correction
/// cell, the pawn key masked down to a row and the side choosing the half.
///
/// ```text
/// index = side * 16384 + (pawn key & 16383)
/// ```
///
/// Collisions are left to share a cell. Two skeletons landing on one row
/// average their corrections, and the correction is bounded and outvoted by
/// whichever position visits more, so the cost of a collision is a slightly
/// worse guess rather than a wrong score.
///
/// Params:
/// - state: &State -> position providing side and pawn key
///
/// Return:
/// usize           -> index into `SearchInfo::corr_hist`
#[inline(always)]
fn correction_index(state: &State) -> usize {
    state.playing as usize * CORR_HIST_SIZE
        + (state.pawn_hash as usize & (CORR_HIST_SIZE - 1))
}

/// update_correction
///
/// Blends one gap between static evaluation and searched score into the cell
/// this pawn skeleton files under, weighted by how much the search behind it
/// is worth believing.
///
/// - gap    : `(score - eval)`, kept in grain units
/// - weight : 1 + depth for a quiet best move, 1 for a capture, 16 at
///            most
/// - cell   : `(cell * (256 - weight) + gap * weight) / 256`
///
/// A capture weighs one whatever its depth: the gap it opened is tactical,
/// something evaluation was never going to see, and teaching the skeleton
/// about it would blame the pawns for a hanging piece.
///
/// The bound decides whether a node teaches at all. A fail-high proves the
/// score is at least what it says, so it teaches only when it came out above
/// the evaluation; a fail-low proves the reverse and teaches only below it.
/// An exact score always teaches. Mate scores never do, being a distance to
/// the end of the game rather than a judgement of the position.
///
/// Params:
/// - entry  : &mut i16 -> correction-history cell updated in place
/// - eval   : i32      -> raw static evaluation at this node
/// - score  : i32      -> score returned by search
/// - depth  : usize    -> remaining depth, weights quiet evidence
/// - flag   : u8       -> score bound, `FEXACT`, `FBETA`, or `FALPHA`
/// - capture: bool     -> whether best move captures
///
/// Notes:
/// A checked node still teaches, but is never taught: the read side zeroes
/// the correction while in check, since what the evaluation is missing there
/// is the check itself and not anything the pawn skeleton knows.
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
/// Offsets of the continuation rows a reply at this node is credited to.
///
/// One row follows the move just played and one the mover's own previous
/// move, which are the two questions worth asking about a reply: what it
/// answers, and what it continues.
///
/// - slot 0 : one ply back, the opponent's move, the one being answered
/// - slot 1 : two plies back, this side's own move, the one followed
///
/// ```text
/// base = (slot * keys + key of that move) * keys
/// cell = base + key of the reply being credited
/// ```
///
/// A slot reads `usize::MAX` when the ply it would follow does not exist or
/// held a null move, which is the case where no move was answered and nothing
/// about a reply to it can be learned.
///
/// Params:
/// - state: &State -> position whose most recent plies are read
///
/// Return:
/// [usize; CONTINUATION_PLIES] -> row offsets, `usize::MAX` where unset
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
/// Applies one signed change everywhere a quiet move is remembered, so that
/// what worked here is credited both as a move and as a reply.
///
/// - `search_hist[move]`      : always, whatever the node was answering
/// - `cont_hist[base + move]` : once per row the node had a move to
///                              follow
///
/// Every table takes the same change, unscaled: the node's verdict is one
/// verdict, and it is filed once as a move and once for each move it
/// answered. Rows the node has nothing to follow are skipped rather than
/// credited to a move that was never played.
///
/// Params:
/// - info : &mut SearchInfo -> worker whose tables are updated
/// - bases: &[usize]        -> continuation rows, `usize::MAX` unset
/// - index: usize           -> piece and target cell of this move
/// - bonus: i32             -> signed bonus or malus
#[inline(always)]
fn update_histories(
    info: &mut SearchInfo,
    bases: &[usize],
    index: usize,
    bonus: i32,
) {
    update_history(&mut info.search_hist[index], bonus);

    for base in bases.iter().filter(|&&base| base != usize::MAX) {
        update_history(&mut info.cont_hist[base + index], bonus);
    }
}
