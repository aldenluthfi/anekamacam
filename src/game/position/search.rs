//! # search.rs
//!
//! Defines search-time control and accounting data used by the engine.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 22/03/2026

use crate::*;

/// Tracks limits, counters, and stop flags for an active search.
///
/// This struct groups time controls, depth/move constraints, node accounting,
/// and interruption controls used by iterative search routines.
/// It is mutated throughout one search invocation lifecycle.
pub struct SearchInfo {
    pub start_time: u128,
    pub stop_time: u128,

    pub depth: usize,

    pub set_depth: usize,
    pub set_timed: u128,
    pub set_moves: usize,

    pub nodes: u128,

    pub interrupt: bool,
    pub infinite: bool,
}

pub fn check_interrupt() {}

pub fn clear_search_info(info: &mut SearchInfo) {
    info.start_time = 0;
    info.stop_time = 0;
    info.depth = 0;
    info.set_depth = 0;
    info.set_timed = 0;
    info.set_moves = 0;
    info.nodes = 0;
    info.interrupt = false;
    info.infinite = false;
}

pub fn search_position(state: &State, info: &mut SearchInfo) {
    // TODO: Implement search logic here.
}

pub fn alpha_beta(
    state: &State,
    depth: usize,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
) -> i32 {
    0
}
