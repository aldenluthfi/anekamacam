//! # qsearch_tt.rs
//!
//! Dedicated transposition table for quiescence search.
//!
//! The qsearch TT stores only capture/check/promotion moves (never quiet moves)
//! with a compact two-slot layout and seqlock for thread-safe reads/writes.
//! Entries carry an age field for staleness-aware replacement; no depth field
//! since all qsearch entries are treated as depth-1 equivalent for eviction.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 15/05/2026

use crate::*;

/*----------------------------------------------------------------------------*\
                 QSEARCH TT ENTRY REPRESENTATION & CONSTANTS
\*----------------------------------------------------------------------------*/

/// QSearch TT entry — compact two-slot layout with seqlock.
///
/// Layout mirrors the main TT but stores no depth field (all entries are
/// treated as depth=1 for replacement):
///   slot[0] = move.0 (128-bit, raw)
///   slot[1] = (sig << 32) | score_flags  (128-bit)
///   age     = plain u64 (excluded from version)
///   version = seqlock counter (odd = writing, even = readable)
///
/// Write order: version++ → slot[0] → slot[1] → age → version++
///
/// Validation: version is even at read time, and unchanged after both reads
/// (no torn write). The sig field guards against hash collisions on move.0
/// alone.
#[derive(Default)]
pub struct QTEntry {
    pub slot:   [u128; 2],                                                      /* [move.0, sig<<32|score_flags]      */
    pub age:     u64,                                                           /* search age for staleness eviction  */
    pub version: AtomicU64,                                                     /* seqlock: odd = writing, even = ok  */
}

impl Clone for QTEntry {
    fn clone(&self) -> Self {
        QTEntry {
            slot: self.slot,
            age: self.age,
            version: AtomicU64::new(self.version.load(Ordering::Relaxed)),
        }
    }
}

pub struct QTable {
    pub table: SyncUnsafeCell<Vec<QTEntry>>,                                    /* shared mutable access              */
    pub age: AtomicU64,                                                         /* search age; bump per search        */
    pub new_write: AtomicU64,                                                   /* writes to empty slots              */
    pub over_write: AtomicU64,                                                  /* writes replacing existing entries  */
    pub hit: AtomicU64,                                                         /* probes where slot matched          */
    pub valid: AtomicU64,                                                       /* probes where read was consistent   */
}

unsafe impl Sync for QTable {}
unsafe impl Send for QTable {}

impl Default for QTable {
    fn default() -> Self {
        Self {
            table: SyncUnsafeCell::new(vec![QTEntry::default(); Q_TABLE_SIZE]),
            age: AtomicU64::new(0),
            new_write: AtomicU64::new(0),
            over_write: AtomicU64::new(0),
            hit: AtomicU64::new(0),
            valid: AtomicU64::new(0),
        }
    }
}

/*----------------------------------------------------------------------------*\
                     QSEARCH TT PACKING / UNPACKING MACROS
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! qtt_index {
    ($hash:expr) => {{
        ($hash as usize) % Q_TABLE_SIZE
    }};
}

#[macro_export]
macro_rules! qtt_enc_score {
    ($score:expr) => {{
        (($score as i16) as u32) & 0xFFFF
    }};
}

#[macro_export]
macro_rules! qtt_enc_flags {
    ($flags:expr) => {{
        ((($flags as u32) & 0x3) << 16)
    }};
}

#[macro_export]
macro_rules! qtt_score {
    ($encoded:expr) => {{
        (($encoded & 0xFFFF) as i32) << 16 >> 16
    }};
}

#[macro_export]
macro_rules! qtt_flags {
    ($encoded:expr) => {{
        (($encoded >> 16) & 0x3) as u8
    }};
}

/*----------------------------------------------------------------------------*\
                     QSEARCH TT PROBE & STORE MACROS
\*----------------------------------------------------------------------------*/

/// Probes the qsearch TT with seqlock validation; returns (valid, score,
/// pseudo_move).
///
/// Validation:
///   Step 1: version is even (no write in progress)
///   Step 2: version unchanged after both reads (no torn write)
///   Step 3: sig field matches the stored MoveSignature (anti-collision)
#[macro_export]
macro_rules! probe_qtt_entry {
    ($state:expr, $qtable:expr, $alpha:expr, $beta:expr) => {{
        let index = qtt_index!($state.position_hash);
        let entry = &mut unsafe { &mut *($qtable.table.get()) }[index];

        let v1 = entry.version.load(Ordering::Acquire);
        if v1 & 1 != 0 {
            (false, i32::MIN, null_pseudo_move())                               /* write in progress                   */
        } else {
            let s0 = entry.slot[0];
            let s1 = entry.slot[1];
            let v2 = entry.version.load(Ordering::Acquire);

            if v1 != v2 {                                                       /* torn read                          */
                (false, i32::MIN, null_pseudo_move())
            } else {
                $qtable.valid.fetch_add(1, Ordering::Relaxed);
                let move_0  = s0;                                               /* move.0 raw                         */
                let encoded = (s1 & 0xFFFF_FFFF) as u32;                        /* bits  0–31 = score+flags          */
                let sig     = (s1 >> 32) as u64;                                /* bits 32–95 = MoveSignature         */
                let pseudo_mv: PseudoMove = (move_0, sig);

                if pseudo_mv == null_pseudo_move() {
                    $qtable.hit.fetch_add(1, Ordering::Relaxed);
                    (false, i32::MIN, null_pseudo_move())
                } else {
                    $qtable.hit.fetch_add(1, Ordering::Relaxed);
                    let entry_flags   = qtt_flags!(encoded);
                    let mut entry_score = qtt_score!(encoded);

                    if entry_score > MATE_SCORE {
                        entry_score -= $state.search_ply as i32;
                    } else if entry_score < -MATE_SCORE {
                        entry_score += $state.search_ply as i32;
                    }

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

                    (valid_cutoff, entry_score, pseudo_mv)
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! probe_qtt_move {
    ($state:expr, $qtable:expr) => {{
        let index = qtt_index!($state.position_hash);
        let entry = &mut unsafe { &mut *($qtable.table.get()) }[index];

        let v1 = entry.version.load(Ordering::Acquire);
        if v1 & 1 != 0 {
            None                                                                /* write in progress                  */
        } else {
            let s0 = entry.slot[0];
            let s1 = entry.slot[1];
            let v2 = entry.version.load(Ordering::Acquire);

            if v1 != v2 {
                None                                                            /* torn read                          */
            } else {
                $qtable.hit.fetch_add(1, Ordering::Relaxed);
                let move_0 = s0;
                let sig = (s1 >> 32) as u64;
                let pseudo_mv: PseudoMove = (move_0, sig);

                if pseudo_mv == null_pseudo_move() {
                    None
                } else {
                    Some(pseudo_mv)
                }
            }
        }
    }};
}

/// Stores a qsearch result in the dedicated TT.
///
/// Only capture/check/promotion moves are written; quiet moves are skipped.
/// Replacement policy: empty slot → always write; occupied → write if
/// entry is stale (age < cur_age - 1) or new entry is FEXACT.
#[macro_export]
macro_rules! hash_qtt_entry {
    ($tt_move:expr, $score:expr, $flags:expr, $state:expr, $qtable:expr) => {{
        let move_type = move_type!($tt_move);
        if move_type != QUIET_MOVE {
            let index = qtt_index!($state.position_hash);
            let table_vec: &mut Vec<QTEntry> =
                unsafe { &mut *($qtable.table.get()) };
            let entry = &mut table_vec[index];

            let mut store_score = $score;
            if store_score > MATE_SCORE {
                store_score += $state.search_ply as i32;
            } else if store_score < -MATE_SCORE {
                store_score -= $state.search_ply as i32;
            }

            let encoded = qtt_enc_score!(store_score) | qtt_enc_flags!($flags);
            let a = $tt_move.0;
            let sig = move_signature!($tt_move);
            let b = ((sig as u128) << 32) | (encoded as u128);

            let cur_age = $qtable.age.load(Ordering::Relaxed);
            let empty = entry.slot[0] == 0 && entry.slot[1] == 0;

            let should_write = empty
                || entry.age < cur_age.saturating_sub(1)
                || $flags == FEXACT;

            if should_write {
                if empty {
                    $qtable.new_write.fetch_add(1, Ordering::Relaxed);
                } else {
                    $qtable.over_write.fetch_add(1, Ordering::Relaxed);
                }

                entry.version.fetch_add(1, Ordering::Release);
                entry.slot[0] = a;
                entry.slot[1] = b;
                entry.age = cur_age;
                entry.version.fetch_add(1, Ordering::Release);
            }
        }
    }};
}
