//! # pv_table.rs
//!
//! Implements principal variation table probing, storage, and extraction.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 29/01/2026

use crate::*;

pub type PVElement = (Move, PositionHash);
pub type PVTable = Vec<PVElement>;

#[hotpath::measure]
pub fn hash_pv_move(pv_move: Move, state: &mut State) {
    let index = state.position_hash % PV_TABLE_SIZE as u128;
    state.pv_table[index as usize] = (pv_move, state.position_hash);
}

#[hotpath::measure]
pub fn probe_pv_move(state: &State) -> Option<Move> {
    let index = state.position_hash % PV_TABLE_SIZE as u128;
    let (pv_move, hash) = &state.pv_table[index as usize];

    if *hash == state.position_hash {
        Some(pv_move.clone())
    } else {
        None
    }
}

#[hotpath::measure]
/// Follows hash-linked PV moves up to `depth` and fills `state.pv_line`.
///
/// Stops on missing entries or illegal moves, then undoes all temporary
/// plies so the caller's position is restored.
pub fn fill_pv_line(state: &mut State, depth: usize) {
    let depth = depth.min(MAX_DEPTH);
    let mut filled = 0;

    for i in 0..depth {
        let Some(pv_move) = probe_pv_move(state) else {
            break;
        };

        if !is_move_legal!(state, pv_move.clone()) {
            break;
        }

        make_move!(state, pv_move.clone());
        state.pv_line[i] = pv_move;
        filled = i + 1;
    }

    for i in filled..depth {
        state.pv_line[i] = null_move();
    }

    while state.search_ply > 0 {
        undo_move!(state);
    }
}
