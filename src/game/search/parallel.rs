//! # parallel.rs
//!
//! Parallel search with lock-free shared transposition table.
//!
//! More cores should mean a stronger search, but a single game tree does not
//! split cleanly across threads. This file takes the lazy-SMP route instead:
//! every worker runs its own full iterative deepening over the same position
//! and they cooperate only through the shared, XOR-encoded transposition
//! table, synchronizing on nothing but start and stop.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 11/05/2026

use crate::*;

/// ThreadPool
///
/// Runs N independent iterative-deepening threads sharing a lock-free TT/QT.
/// Each thread gets its own state clone and SearchBufs; synchronization is
/// limited to start/stop. After all threads join, run() returns the result
/// with the highest score.
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
    /// spawned until `run` is called.
    ///
    /// Params:
    /// - root: &State    -> root position, cloned per worker
    /// - tt: Arc<TTable> -> shared transposition table
    /// - qt: Arc<QTable> -> shared quiescence table
    /// - count: usize    -> requested worker count, clamped to >= 1
    ///
    /// Return:
    /// Self -> the configured pool
    ///
    pub fn with_threads(
        root: &State, tt: Arc<TTable>, qt: Arc<QTable>, count: usize
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
    /// inherit the caller's depth, node, and deadline limits. After all
    /// workers join, the result with the highest score wins.
    ///
    /// Params:
    /// - info: &SearchInfo         -> limits shared by every worker
    /// - dict: Option<&Translator> -> translator for printed move names
    ///
    /// Return:
    /// SearchResult -> the best result across all workers
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
            let soft_deadline = info.soft_deadline;
            let hard_deadline = info.hard_deadline;
            let dict_clone = dict.cloned();

            let handle = thread::Builder::new()
                .name(format!("searcher-{}", i))
                .stack_size(64 * 1024 * 1024)
                .spawn(move || {
                    let mut info = SearchInfo {
                        set_depth,
                        set_nodes,
                        soft_deadline,
                        hard_deadline,
                        thread_count: total_threads,
                        ..Default::default()
                    };
                    let mut bufs = SearchBufs::default();
                    let mut state = state_clone;
                    iterative_deepening(
                        &mut state,
                        &tt_clone, &qt_clone,
                        &mut info, &mut bufs,
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
            total_nodes: 0,
            total_elapsed: 0,
        };

        for (i, worker) in workers.into_iter().enumerate() {
            let result = worker.join().unwrap_or_else(|_| {
                panic!("Thread {} panicked", i)
            });

            if result.best_score >= main_result.best_score
            && result.best_move != null_move() {
                main_result = result;
            }

            log_3!("Thread {} joined", i);
        }

        main_result
    }
}
