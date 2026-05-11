//! # transposition.rs
//!
//! Lock-free XOR-encoded transposition table for lazy SMP.
//!
//! Each entry stores 3 × u128 slots with XOR encoding (no locks):
//!   a = move.0 (128-bit)
//!   b = (move.1 << 32) | encoded_data (64-bit)
//!   c = position_hash (128-bit)
//!
//!   slot[0] = key   = a ^ b ^ c
//!   slot[1] = data1 = a ^ b
//!   slot[2] = data2 = b ^ c
//!   age     = plain u64 (separate field)
//!
//! Validation (2-step):
//!   (1) (slot[0] ^ slot[1]) == c          → verify hash
//!   (2) (slot[0] ^ slot[2]) == a'
//!       b' = slot[0] ^ c ^ a'
//!       check (b' ^ a') == slot[1]        → verify move
//!
//! Recovery:
//!   a = data2 ^ key
//!   b = data1 ^ a
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

/// Lock-free XOR-encoded transposition-table entry.
///
/// Slots layout:
/// - slot[0] = key   = a ^ b ^ c  (a=move.0, b=move.1<<32|encoded, c=hash)
/// - slot[1] = data1 = a ^ b
/// - slot[2] = data2 = b ^ c
/// - age     = plain u64 (age-based replacement, NOT in XOR slots)
#[derive(Clone, Default)]
pub struct TTEntry {
    pub slot: [u128; 3],                                                        /* [key, data1, data2]                */
    pub age: u64,                                                               /* search age for replacement policy  */
}

#[derive(Clone)]
pub struct TTable {
    pub table: Vec<TTEntry>,
    pub age: u64,
}

impl Default for TTable {
    fn default() -> Self {
        Self {
            table: vec![TTEntry::default(); T_TABLE_SIZE],
            age: 0,
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
    ($encoded:expr, $val:expr) => {{
        $encoded |= ($val as u32) & 0x3;
    }};
}

#[macro_export]
macro_rules! tt_enc_depth {
    ($encoded:expr, $val:expr) => {{
        let depth = (($val as u32).min(MAX_DEPTH as u32)) & 0x7F;
        $encoded |= depth << 2;
    }};
}

#[macro_export]
macro_rules! tt_enc_score {
    ($encoded:expr, $val:expr) => {{
        $encoded |= (($val as i16) as u32 & 0xFFFF) << 9;
    }};
}

#[macro_export]
macro_rules! tt_flags {
    ($encoded:expr) => {
        ($encoded & 0x3) as u8
    };
}

#[macro_export]
macro_rules! tt_depth {
    ($encoded:expr) => {
        (($encoded >> 2) & 0x7F) as usize
    };
}

#[macro_export]
macro_rules! tt_score {
    ($encoded:expr) => {
        (($encoded >> 9) as i32) << 16 >> 16
    };
}

/*----------------------------------------------------------------------------*\
                        TRANSPOSITION TABLE STORE / PROBE
\*----------------------------------------------------------------------------*/

/// Probes the lock-free XOR TT and returns (valid, score, move).
///
/// Validation:
///   Step 1: (slot[0] ^ slot[1]) == position_hash
///   Step 2: recover a', b' and check (b' ^ a') == slot[1]
///
/// On failure returns (false, i32::MIN, null_move()).
#[macro_export]
macro_rules! probe_tt_entry {
    ($state:expr, $table:expr, $alpha:expr, $beta:expr, $depth:expr) => {{
        let hash = $state.position_hash;
        let index = tt_index!(hash);
        let entry = &$table.table[index];

        let key_check = entry.slot[0] ^ entry.slot[1];                          /* Step 1: verify hash component      */
        if key_check != hash {
            (false, i32::MIN, null_move())
        } else {                                                                /* Step 2: verify move component      */
            let a_prime = entry.slot[0] ^ entry.slot[2];
            let b_prime = entry.slot[0] ^ hash ^ a_prime;

            if (b_prime ^ a_prime) != entry.slot[1] {
                (false, i32::MIN, null_move())
            } else {
                let encoded = (b_prime >> 32) as u32;
                let move1_raw = (b_prime & 0xFFFFFFFF) as u64;
                let move0 = a_prime;
                let move1_ptr = move1_raw;

                let cap_ptr = move1_ptr as *const Vec<u64>;
                let mv = if cap_ptr.is_null() || (move1_ptr & 0x7) != 0 {
                    null_move()
                } else {
                    Move(move0, unsafe { Arc::from_raw(cap_ptr) })
                };

                if tt_depth!(encoded) < $depth {
                    (false, i32::MIN, mv)
                } else {
                    let mut entry_score = tt_score!(encoded);

                    if entry_score > MATE_SCORE {
                        entry_score -= $state.search_ply as i32;
                    } else if entry_score < -MATE_SCORE {
                        entry_score += $state.search_ply as i32;
                    }

                    let entry_flags = tt_flags!(encoded);
                    let mut valid_cutoff = false;

                    match entry_flags {
                        FALPHA => {
                            if entry_score <= $alpha {
                                entry_score = $alpha;
                                valid_cutoff = true;
                            }
                        }
                        FBETA => {
                            if entry_score >= $beta {
                                entry_score = $beta;
                                valid_cutoff = true;
                            }
                        }
                        FEXACT => valid_cutoff = true,
                        _ => unreachable!(),
                    }

                    (valid_cutoff, entry_score, mv)
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! probe_pv_move {
    ($state:expr, $table:expr) => {{
        let hash = $state.position_hash;
        let index = tt_index!(hash);
        let entry = &$table.table[index];

        let key_check = entry.slot[0] ^ entry.slot[1];
        if key_check != hash {
            None
        } else {
            let a_prime = entry.slot[0] ^ entry.slot[2];
            let b_prime = entry.slot[0] ^ hash ^ a_prime;

            if (b_prime ^ a_prime) != entry.slot[1] {
                None
            } else {
                let move1_raw = (b_prime & 0xFFFFFFFF) as u64;
                let cap_ptr = move1_raw as *const Vec<u64>;
                if cap_ptr.is_null() || (move1_raw & 0x7) != 0 {
                    None
                } else {
                    Some(Move(a_prime, unsafe { Arc::from_raw(cap_ptr) }))
                }
            }
        }
    }};
}

/// Stores a search result in the lock-free XOR TT.
///
/// Write order: slot[0] → slot[1] → slot[2] → age (lazy SMP).
#[macro_export]
macro_rules! hash_tt_entry {
    (
        $tt_move:expr, $score:expr, $flags:expr,
        $depth:expr, $state:expr, $table:expr
    ) => {{
        let hash = $state.position_hash;
        let index = tt_index!(hash);
        let entry = &mut $table.table[index];

        let mut encoded = 0u32;
        tt_enc_flags!(encoded, $flags);
        tt_enc_depth!(encoded, $depth);

        let mut store_score = $score;
        if store_score > MATE_SCORE {
            store_score += $state.search_ply as i32;
        } else if store_score < -MATE_SCORE {
            store_score -= $state.search_ply as i32;
        }
        tt_enc_score!(encoded, store_score);

        let a = $tt_move.0;                                                     /* move.0 128-bit                     */
        let b_raw = Arc::as_ptr(&$tt_move.1) as u64;                            /* Arc ptr as u64                     */
        let b = ((b_raw as u128) << 32) | (encoded as u128);                    /* move.1 << 32 | encoded             */

        entry.slot[0] = a ^ b ^ hash;                                           /* key   = a ^ b ^ c                  */
        entry.slot[1] = a ^ b;                                                  /* data1 = a ^ b                      */
        entry.slot[2] = b ^ hash;                                               /* data2 = b ^ c                      */
        entry.age = $table.age;
    }};
}

#[macro_export]
macro_rules! fill_pv_line {
    ($state:expr, $table:expr, $depth:expr) => {{
        let depth = ($depth).min(MAX_DEPTH);
        let mut filled = 0;

        for i in 0..depth {
            let Some(pv_move) = probe_pv_move!($state, $table) else {
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
