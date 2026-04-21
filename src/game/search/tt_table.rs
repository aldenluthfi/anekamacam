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

pub struct TTEntry {
    pub position_hash: PositionHash,
    pub tt_move: Move,
    pub score: i32,
    pub depth: usize,
    pub flags: u8,
}

impl Default for TTEntry {
    fn default() -> Self {
        Self {
            position_hash: 0,
            tt_move: null_move(),
            score: 0,
            depth: 0,
            flags: HFNONE,
        }
    }
}

pub struct TTTable {
    pub p_table: Vec<TTEntry>,
    pub new_write: u64,
    pub over_write: u64,
    pub hit: u64,
    pub cut: u64,
}

impl TTTable {
    pub fn new() -> Self {
        Self {
            p_table: (0..TT_TABLE_SIZE).map(|_| TTEntry::default()).collect(),
            new_write: 0,
            over_write: 0,
            hit: 0,
            cut: 0,
        }
    }
}

/// Maps a 128-bit position hash directly into the TT table.
#[macro_export]
macro_rules! tt_index {
    ($hash:expr) => {{
        ($hash as usize) % TT_TABLE_SIZE
    }};
}

/// Stores one transposition-table entry in a direct-mapped fixed-size table.
#[macro_export]
macro_rules! hash_tt_entry {
    ($tt_move:expr, $score:expr, $flags:expr, $depth:expr, $state:expr) => {{
        let index = tt_index!($state.position_hash);

        if $state.transposition_table.p_table[index].position_hash == 0 {
            $state.transposition_table.new_write += 1;
        } else {
            $state.transposition_table.over_write += 1;
        }

        let mut adjusted_score = $score;

        if adjusted_score > MATE_SCORE {
            adjusted_score += $state.search_ply as i32;
        } else if adjusted_score < -MATE_SCORE {
            adjusted_score -= $state.search_ply as i32;
        }

        $state.transposition_table.p_table[index].tt_move = $tt_move;
        $state.transposition_table.p_table[index].position_hash = $state.position_hash;
        $state.transposition_table.p_table[index].flags = $flags;
        $state.transposition_table.p_table[index].score = adjusted_score;
        $state.transposition_table.p_table[index].depth = $depth;
    }};
}

/// Probes a TT entry and returns whether the entry can be used.
pub fn probe_tt_entry(
    state: &mut State,
    tt_move: &mut Move,
    score: &mut i32,
    alpha: i32,
    beta: i32,
    depth: usize,
) -> bool {
    let index = tt_index!(state.position_hash);

    debug_assert!(depth < MAX_DEPTH);
    debug_assert!(alpha < beta);
    debug_assert!(state.search_ply < MAX_DEPTH as u32);

    let (entry_move, mut entry_score, entry_depth, entry_flags) = {
        let entry = &state.transposition_table.p_table[index];

        if entry.position_hash != state.position_hash {
            return false;
        }

        (
            entry.tt_move.clone(),
            entry.score,
            entry.depth,
            entry.flags,
        )
    };

    *tt_move = entry_move;

    if entry_depth < depth {
        return false;
    }

    state.transposition_table.hit += 1;

    if entry_score > MATE_SCORE {
        entry_score -= state.search_ply as i32;
    } else if entry_score < -MATE_SCORE {
        entry_score += state.search_ply as i32;
    }

    match entry_flags {
        HFALPHA => {
            if entry_score <= alpha {
                *score = alpha;
                state.transposition_table.cut += 1;
                return true;
            }
        }
        HFBETA => {
            if entry_score >= beta {
                *score = beta;
                state.transposition_table.cut += 1;
                return true;
            }
        }
        HFEXACT => {
            *score = entry_score;
            state.transposition_table.cut += 1;
            return true;
        }
        _ => {}
    }

    false
}

/// Probes a PV move from a direct-mapped slot.
#[macro_export]
macro_rules! probe_pv_move {
    ($state:expr) => {{
        let entry = &$state.transposition_table.p_table[tt_index!($state.position_hash)];

        if entry.position_hash == $state.position_hash {
            Some(entry.tt_move.clone())
        } else {
            None
        }
    }};
}

/// Follows hash-linked PV moves up to `depth` and fills `state.pv_line`.
///
/// Stops on missing entries or illegal moves, then undoes all temporary
/// plies so the caller's position is restored.
#[macro_export]
macro_rules! fill_pv_line {
    ($state:expr, $depth:expr) => {{
        let depth = ($depth).min(MAX_DEPTH);
        let mut filled = 0;

        for i in 0..depth {
            let Some(pv_move) = probe_pv_move!($state) else {
                break;
            };

            let all_moves = generate_all_moves_and_drops($state);

            for mv in all_moves {
                let legal = make_move!($state, mv.clone());

                if legal {
                    undo_move!($state);
                }

                if mv == pv_move && legal {
                    break;
                }
            }

            make_move!($state, pv_move.clone());
            $state.pv_line[i] = pv_move;
            filled = i + 1;
        }

        for i in filled..depth {
            $state.pv_line[i] = null_move();
        }

        while $state.search_ply > 0 {
            undo_move!($state);
        }
    }};
}

