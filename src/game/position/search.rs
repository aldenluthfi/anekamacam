//! # search.rs
//!
//! Defines search-time control and accounting data used by the engine.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 22/03/2026

/// Tracks limits, counters, and stop flags for an active search.
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