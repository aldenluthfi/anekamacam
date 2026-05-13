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

pub struct ThreadPool {
    pub main_state: State,
    pub tt: Arc<TTable>,
    thread_count: usize,
}

impl ThreadPool {

    pub fn with_threads(root: &State, tt: Arc<TTable>, count: usize) -> Self {
        let thread_count = count.max(1);

        log_2!("ThreadPool: {} threads", thread_count);

        let main_state = root.clone();

        Self { main_state, tt, thread_count }
    }

    pub fn run(self, depth: usize, timed: u128) -> SearchResult {
        let tt = Arc::clone(&self.tt);
        let mut workers = Vec::with_capacity(self.thread_count);

        for i in 0..self.thread_count {
            let state_clone = self.main_state.clone();
            let tt_clone = Arc::clone(&tt);
            let set_depth = depth;
            let set_timed = timed;

            let handle = thread::Builder::new()
                .name(format!("searcher-{}", i))
                .spawn(move || {
                    let mut info = SearchInfo {
                        set_depth,
                        set_timed,
                        ..Default::default()
                    };
                    let mut bufs = SearchBufs::default();
                    let mut state = state_clone;
                    iterative_deepening(
                        &mut state, &tt_clone, &mut info, &mut bufs, i
                    )
            })
                .unwrap_or_else(|e| {
                    panic!("Failed to spawn searcher-{}: {e}", i)
                });

            workers.push(handle);
        }

        let mut main_result = SearchResult {
            best_score: -INFINITY,
            best_move: null_move(),
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
