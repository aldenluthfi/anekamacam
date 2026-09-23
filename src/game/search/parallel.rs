//! parallel.rs
//!
//! Parallel search with a shared transposition table without locks.
//!
//! This file uses lazy SMP. Each worker runs its own iterative deepening
//! on the same position. The workers share only the XOR-encoded hash
//! tables. They synchronize only at start and stop.
//!
//! Created: 11/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              LAZY SMP WORKER POOL
\*----------------------------------------------------------------------------*/

/// ThreadPool
///
/// A pool of independent search workers for one position. Each worker
/// clones the root and runs its own iterative deepening.
///
/// ```text
/// worker 0   own state, own tables, own counters  ┐
/// worker 1   own state, own tables, own counters  ├─ shared main table
/// worker n   own state, own tables, own counters  ┘  shared qsearch table
/// ```
///
/// The workers read the entries of the other workers, so they search the
/// tree in different orders. Each cutoff that one worker finds goes to all
/// workers through the table.
///
pub struct ThreadPool {
    pub main_state: State,                                                      /* root position, cloned per worker   */
    pub tt: Arc<TTable>,                                                        /* shared main transposition table    */
    pub qt: Arc<QTable>,                                                        /* shared quiescence table            */
    thread_count: usize,                                                        /* number of worker threads           */
}

impl ThreadPool {

    /// ThreadPool::with_threads
    ///
    /// Makes a pool with a copy of the root position. No thread starts until
    /// `run`. Later changes to the caller state do not change the pool.
    ///
    /// Params:
    /// - root : &State      -> root position, cloned for each worker
    /// - tt   : Arc<TTable> -> shared transposition table
    /// - qt   : Arc<QTable> -> shared quiescence table
    /// - count: usize       -> number of workers, minimum 1
    ///
    /// Return:
    /// Self                 -> the configured pool
    ///
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
    /// Starts one named thread for each worker and joins them. Each worker
    /// runs iterative deepening on its own copy of the state, with the depth,
    /// node and deadline limits of the caller. The result is:
    ///
    /// - move  : from the worker with the most completed iterations
    /// - tie   : the lowest worker index wins, so runs are repeatable
    /// - score : not used to select, an interrupted score is not proved
    /// - nodes : sum of all workers
    /// - time  : longest time of one worker
    ///
    /// Params:
    /// - info: &SearchInfo         -> limits for all workers
    /// - dict: Option<&Translator> -> translator for printed move names
    ///
    /// Return:
    /// SearchResult                -> the deepest completed result
    ///
    /// Notes:
    /// Each thread has a 64 MB stack, because the search recurses once per
    /// ply. The thread name has the executable tag and the index, for
    /// profilers. If a worker cannot start or panics, the process stops.
    ///
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
