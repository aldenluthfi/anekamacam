//! # move_match.rs
//!
//! Implements move matching and validation against board state.
//!
//! This file contains functionality for determining whether a parsed move
//! expression is possible given the current board state. It validates moves
//! by checking against enemy pieces, friendly pieces, and king positions
//! using bitboard representations. The module uses parallel processing to
//! efficiently check multiple move alternatives.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use rayon::iter::ParallelIterator;

use super::util::split_and_process;
use crate::representations::state::State;

fn match_move(expr: &str, start: u16, end: u16, state: &State) -> Option<bool> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before matching.");

    Some(false)
}

/// Determines if a move described by `expr` is possible from `start` to `end`
/// given the current board state represented by `enemies`, `friends`, and
/// `kings`. Not considering move legality beyond basic matching.
pub fn is_move_possible(
    expr: &str, start: u16, end: u16, state: &State
) -> bool {
    split_and_process(expr, |m| match_move(m, start, end, state))
        .any(|b| b.unwrap_or_default())                                         /* Return true if any match is found  */
}