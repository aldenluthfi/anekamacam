//! # parallel.rs
//!
//! Parallel search with lock-free shared transposition table.
//!
//! Each worker thread runs fully independent iterative deepening,
//! sharing the XOR-encoded TT but synchronizing only on start/stop.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 11/05/2026

use crate::*;

/// Runs N independent iterative-deepening threads sharing a lock-free TT/QT.
///
/// Each thread gets its own state clone and SearchBufs; synchronization is
/// limited to start/stop. After all threads join, run() returns the result
/// with the highest score.
pub struct ThreadPool {
    pub main_state: State,
    pub tt: Arc<TTable>,
    pub qt: Arc<QTable>,
    thread_count: usize,
}

impl ThreadPool {

    pub fn with_threads(
        root: &State, tt: Arc<TTable>, qt: Arc<QTable>, count: usize
    ) -> Self {
        let thread_count = count.max(1);

        log_3!("ThreadPool: {} threads", thread_count);

        let main_state = root.clone();

        Self { main_state, tt, qt, thread_count }
    }

    pub fn run(
        self,
        depth: usize,
        timed: u128,
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
            let set_depth = depth;
            let set_timed = timed;
            let dict_clone = dict.cloned();

            let handle = thread::Builder::new()
                .name(format!("searcher-{}", i))
                .stack_size(64 * 1024 * 1024)
                .spawn(move || {
                    let mut info = SearchInfo {
                        set_depth,
                        set_timed,
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
