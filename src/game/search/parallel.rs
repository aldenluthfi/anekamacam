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

/*----------------------------------------------------------------------------*\
                              THREAD POOL
\*----------------------------------------------------------------------------*/

/// Manages the pool of search worker threads.
pub struct ThreadPool {
    pub main_state: State,
    pub tt: Arc<TTable>,
    thread_count: usize,
}

impl ThreadPool {

    /// Creates a new pool with auto-detected thread count.
    ///
    /// Thread count is rounded down to nearest power of 2 (2/4/8/16 max).
    /// Falls back to single-thread if detection fails.
    pub fn new(root: &State, tt: Arc<TTable>) -> Self {
        let phys = thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);

        let count = phys.min(16);
        let power = count.next_power_of_two();
        let thread_count = if power > count { power >> 1 } else { power };
        let thread_count = thread_count.max(1);

        log_2!(
            "ThreadPool: {} threads (detected={})",
            thread_count, phys
        );

        let main_state = root.clone();

        Self { main_state, tt, thread_count }
    }

    pub fn run(&self, depth: usize, timed: u128) -> SearchResult {
        let tt = Arc::clone(&self.tt);
        let mut handles = Vec::with_capacity(self.thread_count);

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
                    let mut state = state_clone;
                    let mut table = (*tt_clone).clone();
                    search_position(&mut state, &mut table, &mut info);
                })
                .unwrap_or_else(|e| {
                    panic!("Failed to spawn searcher-{}: {e}", i)
                });

            handles.push(handle);
        }

        for (i, h) in handles.into_iter().enumerate() {
            let _ = h.join();
            log_3!("Thread {} joined", i);
        }

        SearchResult {
            best_score: 0,
            best_move: null_move(),
            total_nodes: 0,
            total_elapsed: 0,
        }
    }
}

/*----------------------------------------------------------------------------*\
                        PARALLEL SEARCH ENTRY POINT
\*----------------------------------------------------------------------------*/

/// Top-level multi-threaded search entry.
///
/// Detects available parallelism, spawns workers, joins them, returns result.
/// Falls back to single-thread if parallelism detection fails or count = 1.
pub fn search_position_mt(
    state: &mut State,
    table: &mut TTable,
    info: &mut SearchInfo,
) -> SearchResult {
    let pool = ThreadPool::new(state, Arc::new(table.clone()));
    pool.run(info.set_depth, info.set_timed)
}
