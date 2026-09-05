//! parallel.rs
//!
//! Parallel search with lock-free shared transposition table.
//!
//! More cores should mean a stronger search, but a single game tree does not
//! split cleanly across threads. This file takes the lazy-SMP route instead:
//! every worker runs its own full iterative deepening over the same position
//! and they cooperate only through the shared, XOR-encoded transposition
//! table, synchronizing on nothing but start and stop.
//!
//! Created: 11/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              LAZY SMP WORKER POOL
\*----------------------------------------------------------------------------*/

/// ThreadPool
///
/// Independent searchers over one position, sharing the tables and nothing
/// else. Each worker clones the root, runs its own iterative deepening, and
/// meets the others only in the two shared tables.
///
/// ```text
/// worker 0   own state, own tables, own counters  ┐
/// worker 1   own state, own tables, own counters  ├─ shared main table
/// worker n   own state, own tables, own counters  ┘  shared qsearch table
/// ```
///
/// Workers drift apart within a few nodes, since each writes what it finds
/// and reads what the others left, and that drift is the point: the same tree
/// searched in a different order finds cut moves at different times, and the
/// table hands every discovery to everyone.
pub struct ThreadPool {
    pub main_state: State,                                                      /* root position, cloned per worker   */
    pub tt: Arc<TTable>,                                                        /* shared main transposition table    */
    pub qt: Arc<QTable>,                                                        /* shared quiescence table            */
    thread_count: usize,                                                        /* number of worker threads           */
}

impl ThreadPool {

    /// ThreadPool::with_threads
    ///
    /// Prepares a pool over a snapshot of the root position; nothing is
    /// spawned until `run` is called. The snapshot is what every worker is
    /// cloned from, so the pool answers for the position as it stood when it
    /// was built and not as the caller's state may go on to stand.
    ///
    /// Params:
    /// - root : &State      -> root position, cloned per worker
    /// - tt   : Arc<TTable> -> shared transposition table
    /// - qt   : Arc<QTable> -> shared quiescence table
    /// - count: usize       -> requested worker count, clamped to >= 1
    ///
    /// Return:
    /// Self                 -> the configured pool
    pub fn with_threads(
        root: &State, tt: Arc<TTable>, qt: Arc<QTable>, count: usize,
    ) -> Self {
        let thread_count = count.max(1);

        log_3!("ThreadPool: {} threads", thread_count);

        let main_state = root.clone();

        Self { main_state, tt, qt, thread_count }
    }

    /// ThreadPool::run
    ///
    /// Spawns one named searcher thread per worker, each running full
    /// iterative deepening on its own state clone with a large stack
    /// (deep recursion) while sharing the lock-free tables. All workers
    /// inherit the caller's depth, node, and deadline limits.
    ///
    /// ```text
    /// depth    the worker that finished the most iterations wins
    /// index    the lowest one breaks a tie, the same way every run
    /// score    decides nothing at all
    /// nodes    summed over every worker, the whole search's cost
    /// time     the longest a worker ran, since they ran together
    /// ```
    ///
    /// Score is left out because a worker cut off inside an iteration holds a
    /// number no window ever confirmed, and picking on score lets it outrank
    /// a shallower answer that was actually proved. Depth is a fact about how
    /// much work finished, and breaking ties by index keeps two runs of one
    /// position on the same move rather than on whichever thread was quicker
    /// that time.
    ///
    /// Counters are the search's, not the winner's: reporting the winning
    /// worker's own nodes would report one share as the whole, and summing
    /// elapsed time across workers would count the same wall clock once per
    /// thread.
    ///
    /// Params:
    /// - info: &SearchInfo         -> limits shared by every worker
    /// - dict: Option<&Translator> -> translator for printed move names
    ///
    /// Return:
    /// SearchResult                -> the deepest finished result
    ///
    /// Notes:
    /// Threads are named after the executable and their index, so a profiler
    /// or a debugger attached mid-search can tell one worker from another,
    /// and each is given a stack far larger than the default: the search
    /// recurses a frame per ply and a deep line would otherwise land on the
    /// guard page. A worker that fails to spawn or panics takes the process
    /// with it, since a search missing a worker is no longer the search the
    /// caller asked for.
    pub fn run(
        self,
        info: &SearchInfo,
        dict: Option<&Translator>,
    ) -> SearchResult {
        let tt = Arc::clone(&self.tt);
        let qtable = Arc::clone(&self.qt);
        let total_threads = self.thread_count;
        let mut workers = Vec::with_capacity(total_threads);

        for i in 0..total_threads {
            let state_clone = self.main_state.clone();
            let tt_clone = Arc::clone(&tt);
            let qt_clone = Arc::clone(&qtable);
            let set_depth = info.set_depth;
            let set_nodes = info.set_nodes;
            let deadline = info.deadline;
            let dict_clone = dict.cloned();

            let handle = thread::Builder::new()
                .name(format!("searcher:{}:{}", exe_tag(), i))
                .stack_size(64 * 1024 * 1024)
                .spawn(move || {
                    let mut info = SearchInfo {
                        set_depth,
                        set_nodes,
                        deadline,
                        thread_count: total_threads,
                        ..Default::default()
                    };
                    let mut state = state_clone;
                    iterative_deepening(
                        &mut state,
                        &tt_clone, &qt_clone,
                        &mut info,
                        i, dict_clone.as_ref(),
                    )
            })
                .unwrap_or_else(|e| {
                    panic!("Failed to spawn searcher-{}: {e}", i)
                });

            workers.push(handle);
        }

        let mut main_result = SearchResult {
            best_score: -INF,
            best_move: null_move(),
            ponder_move: null_move(),
            completed_depth: 0,
            total_nodes: 0,
            total_elapsed: 0,
        };

        let mut total_nodes = 0;
        let mut total_elapsed = 0;
        let mut deepest = 0;

        for (i, worker) in workers.into_iter().enumerate() {
            let result = worker.join().unwrap_or_else(|_| {
                panic!("Thread {} panicked", i)
            });

            total_nodes += result.total_nodes;
            total_elapsed = total_elapsed.max(result.total_elapsed);

            log_3!(
                "Thread {} joined at completed depth {}",
                i, result.completed_depth
            );

            if result.completed_depth > deepest
            && result.best_move != null_move() {
                deepest = result.completed_depth;
                main_result = result;
            }
        }

        main_result.total_nodes = total_nodes;
        main_result.total_elapsed = total_elapsed;

        main_result
    }
}
