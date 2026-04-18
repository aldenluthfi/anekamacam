//! # move_ordering.rs
//!
//! Implements lightweight move-ordering helpers used by alpha-beta search.
//!
//! This module currently provides MVV-LVA scoring and in-place move selection
//! utilities. The goal is to improve pruning efficiency without changing
//! search semantics or adding new search features.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 19/04/2026

use crate::*;

/// Returns a static MVV-LVA score for one move in the current position.
///
/// MVV-LVA (Most Valuable Victim - Least Valuable Attacker) prioritizes
/// captures that win more material while risking less. Non-captures return 0,
/// allowing callers to keep quiet-move ordering policies separate.
///
/// Scoring rules:
/// - Quiet/drop moves: `0`
/// - Single capture : `captured_value + most_valuable - attacker_value`
/// - Multi-capture  : `sum(captured_values) + most_valuable - attacker_value`
///
/// A larger score means the move should be searched earlier.
#[inline(always)]
pub fn score_mvvlva(state: &State, mv: &Move) -> u16 {
    let move_type = move_type!(mv);

    if move_type != SINGLE_CAPTURE_MOVE && move_type != MULTI_CAPTURE_MOVE {
        return 0;
    }

    let attacker_value = p_ovalue!(state.pieces[piece!(mv) as usize]);

    if move_type == SINGLE_CAPTURE_MOVE {
        let captured_value =
            p_ovalue!(state.pieces[captured_piece!(mv) as usize]);
        return captured_value + state.most_valuable - attacker_value;
    }

    let mut total_value = 0;
    for cap in mv.1.iter() {
        total_value +=
            p_ovalue!(state.pieces[multi_move_captured_piece!(cap) as usize]);
    }

    total_value + state.most_valuable - attacker_value
}

/// Selects the best-scoring move in `moves[index..]` and swaps it into `index`.
///
/// This is a selection-sort step used inside alpha-beta's move loop so we can
/// avoid sorting the entire move list up front. Combined with cutoffs, this
/// typically performs fewer comparisons and allocations than full sorting.
#[inline(always)]
pub fn pick_by_mvvlva(state: &State, moves: &mut [Move], index: usize) {
    let best_index = (index..moves.len())
        .max_by_key(|&i| score_mvvlva(state, &moves[i]))
        .unwrap_or(index);

    moves.swap(index, best_index);
}

/// Moves the current PV move (if present) to the front of `moves`.
///
/// If `probe_pv_move` returns a move and that move exists in the generated list,
/// this function swaps it to index `0`. Searching the PV move first usually
/// increases early cutoffs in principal variation search windows.
#[inline(always)]
pub fn move_pv_to_front(state: &State, moves: &mut [Move]) {
    let Some(pv_move) = probe_pv_move(state) else {
        return;
    };

    if let Some(pv_index) = moves.iter().position(|mv| mv == &pv_move) {
        moves.swap(0, pv_index);
    }
}
