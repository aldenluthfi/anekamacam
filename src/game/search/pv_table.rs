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
    let index = state.position_hash as usize % PV_TABLE_SIZE;
    state.pv_table[index] = (pv_move, state.position_hash);
}

#[hotpath::measure]
pub fn probe_pv_move(state: &State) -> Option<Move> {
    let index = state.position_hash as usize % PV_TABLE_SIZE;
    let (pv_move, hash) = &state.pv_table[index];

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

    for i in 0..depth {
        if let Some(pv_move) = probe_pv_move(state) {

            if !is_move_legal!(state, pv_move.clone()) {
                break;
            }

            make_move!(state, pv_move.clone());

            state.pv_line[i] = pv_move;
        } else {
            break;
        }
    }

    while state.search_ply > 0 {
        undo_move!(state);
    }
}