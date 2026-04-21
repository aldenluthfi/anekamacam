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

/// Returns a static ordering score for one move in the current position.
///
/// Scoring rules:
/// - Quiet/drop moves: killer/history priority
/// - Single capture : `captured_value + most_valuable - attacker_value`
/// - Multi-capture  : `sum(captured_values) + most_valuable - attacker_value`
/// - PV move        : score strictly higher than others
///                    (`2 * most_valuable + 1`)
///
/// `pv_move` should be probed once per node and forwarded into this function
/// to avoid repeated hash-table lookups while ordering the full move list.
///
/// A larger score means the move should be searched earlier.
#[inline(always)]
pub fn score_move(state: &State, mv: &Move, pv_move: &Option<Move>) -> u16 {
    let move_type = move_type!(mv);

    let mut score =
    if move_type != SINGLE_CAPTURE_MOVE
    && move_type != MULTI_CAPTURE_MOVE
    {
        let killers = &state.killer_hist[state.search_ply as usize];
        if *mv == killers[0] {
            state.most_valuable - 1
        } else if *mv == killers[1] {
            state.most_valuable - 2
        } else {
            state.search_hist[piece!(mv) as usize][end!(mv) as usize]
        }
    } else {
        let attacker_value = p_ovalue!(state.pieces[piece!(mv) as usize]);

        if move_type == SINGLE_CAPTURE_MOVE {
            let captured_value =
                p_ovalue!(state.pieces[captured_piece!(mv) as usize]);
            captured_value + state.most_valuable - attacker_value
        } else {
            let mut total_value = 0;
            for cap in mv.1.iter() {
                total_value +=
                    p_ovalue!(
                        state.pieces[multi_move_captured_piece!(cap) as usize]
                    );
            }

            total_value + state.most_valuable - attacker_value
        }
    };

    if pv_move.as_ref() == Some(mv) {
        score = (state.most_valuable + state.most_valuable).saturating_add(1);
    }

    score
}

/// Selects the best-scoring move in `moves[index..]` and swaps it into `index`.
///
/// This is a selection-sort step used inside alpha-beta's move loop so we can
/// avoid sorting the entire move list up front. Combined with cutoffs, this
/// typically performs fewer comparisons and allocations than full sorting.
///
/// `pv_move` should be the value probed once at the current node.
#[inline(always)]
pub fn pick_by_score(
    state: &State,
    moves: &mut [Move],
    index: usize,
    pv_move: &Option<Move>,
) {
    if index >= moves.len() {
        return;
    }

    let mut best_index = index;
    let mut best_score = score_move(state, &moves[index], pv_move);

    for i in (index + 1)..moves.len() {
        let score = score_move(state, &moves[i], pv_move);
        if score > best_score {
            best_score = score;
            best_index = i;
        }
    }

    if best_index != index {
        moves.swap(index, best_index);
    }
}
