//! # transposition_table.rs
//!
//! Compact transposition-table storage, probing, and PV extraction.
//!
//! The table is direct-mapped and each slot keeps:
//! - full `position_hash` verification key (`u128`)
//! - best move (`Move`)
//! - packed metadata (`u32`) for flags, depth, and score
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 29/01/2026

use crate::*;

/*----------------------------------------------------------------------------*\
                      TRANSPOSITION TABLE ENTRY REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Packed transposition-table entry.
///
/// `encoded` layout:
/// - bits 0-1     : bound flags (`HFALPHA`, `HFBETA`, `HFEXACT`)
/// - bits 2-8     : searched depth (7 bits)
/// - bits 9-15    : entry age (7 bits)
/// - bits 16-31   : signed score stored as a 16-bit integer
/// `position_hash` keeps the full key for collision checks in direct-mapped
/// slots.
#[derive(Clone)]
pub struct TTEntry {
    pub position_hash: PositionHash,
    pub tt_move: Move,
    pub encoded: u32,
}

impl Default for TTEntry {
    fn default() -> Self {
        Self {
            position_hash: 0,
            tt_move: null_move(),
            encoded: 0,
        }
    }
}

pub struct TTable {
    pub table: Vec<TTEntry>,
    pub age: u8,
}

impl Default for TTable {
    fn default() -> Self {
        Self {
            table: vec![TTEntry::default(); T_TABLE_SIZE],
            age: 0
        }
    }
}

/*----------------------------------------------------------------------------*\
                       TRANSPOSITION TABLE PACKING HELPERS
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! tt_index {
    ($hash:expr) => {{
        ($hash as usize) % T_TABLE_SIZE
    }};
}

#[macro_export]
macro_rules! tt_enc_flags {
    ($entry:expr, $val:expr) => {{
        $entry.encoded |= ($val as u32) & 0x3;
    }};
}

#[macro_export]
macro_rules! tt_enc_depth {
    ($entry:expr, $val:expr) => {{
        let depth = (($val as u32).min(MAX_DEPTH as u32)) & 0x7F;
        $entry.encoded |= depth << 2;
    }};
}

#[macro_export]
macro_rules! tt_enc_age {
    ($entry:expr, $val:expr) => {{
        let age = (($val as u32).min(127)) & 0x7F;
        $entry.encoded |= age << 9;
    }};
}

#[macro_export]
macro_rules! tt_enc_score {
    ($entry:expr, $val:expr) => {{
        $entry.encoded |= ($val as u32 & 0xFFFF) << 16;
    }};
}

#[macro_export]
macro_rules! tt_flags {
    ($entry:expr) => {
        ($entry.encoded & 0x3) as u8
    };
}

#[macro_export]
macro_rules! tt_depth {
    ($entry:expr) => {
        (($entry.encoded >> 2) & 0x7F) as usize
    };
}

#[macro_export]
macro_rules! tt_age {
    ($entry:expr) => {
        (($entry.encoded >> 9) & 0x7F) as u8
    };
}

#[macro_export]
macro_rules! tt_score {
    ($entry:expr) => {
        (($entry.encoded >> 16) as i32) << 16 >> 16
    };
}


/*----------------------------------------------------------------------------*\
                        TRANSPOSITION TABLE STORE / PROBE
\*----------------------------------------------------------------------------*/

/// Stores one search result in TT with mate-distance normalization.
///
/// Mate-like scores are adjusted by `search_ply` before packing so probing at a
/// different ply restores the correct distance-to-mate semantics.
#[macro_export]
macro_rules! hash_tt_entry {
    ($tt_move:expr, $score:expr, $flags:expr, $depth:expr, $state:expr) => {{
        let hash = $state.position_hash;
        let index = tt_index!(hash);

        let mut replace = false;
        if $state.transposition_table.table[index].position_hash == 0 {
            replace = true
        } else if
        tt_age!($state.transposition_table.table[index]) <
        $state.transposition_table.age ||
        tt_depth!($state.transposition_table.table[index]) <=
        $depth
        {
            replace = true
        }

        let mut score = $score;

        if score > MATE_SCORE {
            score += $state.search_ply as i32;
        } else if score < -MATE_SCORE {
            score -= $state.search_ply as i32;
        }

        if replace {
            let entry = &mut $state.transposition_table.table[index];

            entry.position_hash = hash;
            entry.tt_move = $tt_move;
            entry.encoded = 0;

            tt_enc_flags!(entry, $flags);
            tt_enc_depth!(entry, $depth);
            tt_enc_age!(entry, $state.transposition_table.age);
            tt_enc_score!(entry, score);
        }
    }};
}

/// Probes TT and returns `(cutoff, tt_move, score)`.
///
/// A matching move can still be reused for ordering when depth/flag checks
/// reject the cached score for immediate alpha-beta cutoff.
#[macro_export]
macro_rules! probe_tt_entry {
    ($state:expr, $alpha:expr, $beta:expr, $depth:expr) => {{
        let index = tt_index!($state.position_hash);

        let (entry_move, mut entry_score, entry_depth, entry_flags) = {
            let entry = &$state.transposition_table.table[index];

            if entry.position_hash != $state.position_hash {
                (null_move(), i32::MIN, 0usize, HFNONE)
            } else {
                (
                    entry.tt_move.clone(),
                    tt_score!(entry),
                    tt_depth!(entry),
                    tt_flags!(entry),
                )
            }
        };

        if entry_score == i32::MIN {
            (false, null_move(), i32::MIN)
        } else if entry_depth < $depth {
            (false, entry_move, i32::MIN)
        } else {
            if entry_score > MATE_SCORE {
                entry_score -= $state.search_ply as i32;
            } else if entry_score < -MATE_SCORE {
                entry_score += $state.search_ply as i32;
            }

            match entry_flags {
                HFALPHA => {
                    if entry_score <= $alpha {
                        (true, entry_move, $alpha)
                    } else {
                        (false, entry_move, entry_score)
                    }
                }
                HFBETA => {
                    if entry_score >= $beta {
                        (true, entry_move, $beta)
                    } else {
                        (false, entry_move, entry_score)
                    }
                }
                HFEXACT => {
                    (true, entry_move, entry_score)
                }
                _ => (false, entry_move, entry_score),
            }
        }
    }};
}

#[macro_export]
macro_rules! probe_pv_move {
    ($state:expr) => {{
        let entry = &$state.transposition_table.table
            [tt_index!($state.position_hash)];

        if entry.position_hash == $state.position_hash {
            Some(entry.tt_move.clone())
        } else {
            None
        }
    }};
}

/// Reconstructs PV line by following TT moves from current position.
///
/// Writes up to `depth` moves into `state.pv_line`, clears unused tail slots,
/// then undoes all temporary moves performed during reconstruction.
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
