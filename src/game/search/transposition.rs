//! # transposition.rs
//!
//! XOR-encoded transposition table with seqlock torn-read detection.
//!
//! Each entry stores 3 × u128 slots plus a seqlock:
//!   a = move.0 (128-bit, stored directly)
//!   b = (sig << 32) | encoded_data (128-bit, stored directly)
//!   c = position_hash (128-bit)
//!
//!   slot[0] = a       = move.0 (raw)
//!   slot[1] = b       = (sig << 32) | encoded_data (raw)
//!   slot[2] = parity  = a ^ b ^ c
//!   age     = plain u64 (separate field)
//!   version = seqlock counter (AtomicU64)
//!
//! Write order: version++ → slot[0] → slot[1] → slot[2] → age → version++
//!
//! Validation (2-step):
//!   (1) (slot[0] ^ slot[1] ^ slot[2]) == c    → parity: corruption detected
//!   (2) version unchanged during read         → seqlock: no torn write
//!
//! Decode (direct — no XOR recovery needed):
//!   a = slot[0]
//!   b = slot[1]
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

/// Transposition-table entry with parity-based corruption detection and seqlock.
///
/// Slots layout:
/// - slot[0] = a       = move.0 (128-bit, stored directly)
/// - slot[1] = b       = sig<<32|encoded (128-bit, stored directly)
/// - slot[2] = parity  = a ^ b ^ c (all-slot parity for corruption detection)
/// - age     = plain u64 (age-based replacement, NOT in parity)
/// - version = seqlock counter (odd = writing, even = readable)
pub struct TTEntry {
    pub slot: [u128; 3],                                                        /* [key, data1, data2]                */
    pub age: u64,                                                               /* search age for replacement policy  */
    pub version: AtomicU64,                                                     /* seqlock: odd = writing, even = ok  */
}

impl Clone for TTEntry {
    fn clone(&self) -> Self {
        TTEntry {
            slot: self.slot,
            age: self.age,
            version: AtomicU64::new(self.version.load(Ordering::Relaxed)),
        }
    }
}

impl Default for TTEntry {
    fn default() -> Self {
        TTEntry {
            slot: [0, 0, 0],
            age: 0,
            version: AtomicU64::new(0),
        }
    }
}

pub struct TTable {
    pub table: SyncUnsafeCell<Vec<TTEntry>>,                                    /* shared mutable access              */
    pub age: AtomicU64,                                                         /* search age; bump per search        */
    pub new_write: AtomicU64,                                                   /* writes to empty slots              */
    pub over_write: AtomicU64,                                                  /* writes replacing existing entries  */
    pub hit: AtomicU64,                                                         /* probes where hash matched          */
    pub valid: AtomicU64,                                                       /* probes where XOR decode succeeded  */
}

unsafe impl Sync for TTable {}
unsafe impl Send for TTable {}

impl Default for TTable {
    fn default() -> Self {
        Self {
            table: SyncUnsafeCell::new(vec![TTEntry::default(); T_TABLE_SIZE]),
            age: AtomicU64::new(0),
            new_write: AtomicU64::new(0),
            over_write: AtomicU64::new(0),
            hit: AtomicU64::new(0),
            valid: AtomicU64::new(0),
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

/// Probes the TT with parity + seqlock integrity check; returns (valid, score,
/// pseudo_move).
///
/// Validation:
///   Step 1: version is even (no write in progress)
///   Step 2: (slot[0] ^ slot[1] ^ slot[2]) == position_hash
///   Step 3: version unchanged during read (seqlock: no torn write)
///
/// On failure returns (false, i32::MIN, null_pseudo_move()).
#[macro_export]
macro_rules! probe_tt_entry {
    ($state:expr, $table:expr, $alpha:expr, $beta:expr, $depth:expr) => {{
        let hash = $state.position_hash;
        let index = tt_index!(hash);
        let entry = &mut unsafe { &mut *($table.table.get()) }[index];

        let v1 = entry.version.load(Ordering::Acquire);
        if v1 & 1 != 0 {                                                        /* write in progress: skip            */
            (false, i32::MIN, null_pseudo_move())
        } else {
            let s0 = entry.slot[0];
            let s1 = entry.slot[1];
            let s2 = entry.slot[2];

            if s0 ^ s1 ^ s2 != hash {                                           /* parity check: all slots covered    */
                (false, i32::MIN, null_pseudo_move())
            } else {
                $table.hit.fetch_add(1, Ordering::Relaxed);                     /* parity matched                     */
                let v2 = entry.version.load(Ordering::Acquire);

                if v1 != v2 {                                                   /* seqlock: torn read detected        */
                    (false, i32::MIN, null_pseudo_move())
                } else {
                    $table.valid.fetch_add(1, Ordering::Relaxed);               /* consistent read confirmed          */
                    let a_prime = s0;                                           /* a = slot[0] (direct)               */
                    let b_prime = s1;                                           /* b = slot[1] (direct)               */
                    let encoded = (b_prime & 0xFFFF_FFFF) as u32;               /* bits  0-31 = flags/depth/score     */
                    let sig     = (b_prime >> 32) as u64;                       /* bits 32-95 = MoveSignature         */
                    let pseudo_mv: PseudoMove = (a_prime, sig);

                    if tt_depth!(encoded) < $depth {
                        (false, i32::MIN, pseudo_mv)
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

                        (valid_cutoff, entry_score, pseudo_mv)
                    }
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
        let entry = &mut unsafe { &mut *($table.table.get()) }[index];

        let v1 = entry.version.load(Ordering::Acquire);
        if v1 & 1 != 0 {                                                        /* write in progress: skip            */
            None
        } else {
            let s0 = entry.slot[0];
            let s1 = entry.slot[1];
            let s2 = entry.slot[2];

            if s0 ^ s1 ^ s2 != hash {                                           /* parity check: all slots covered    */
                None
            } else {
                $table.hit.fetch_add(1, Ordering::Relaxed);                     /* parity matched                     */
                let v2 = entry.version.load(Ordering::Acquire);

                if v1 != v2 {                                                   /* seqlock: torn read detected        */
                    None
                } else {
                    $table.valid.fetch_add(1, Ordering::Relaxed);               /* consistent read confirmed          */

                    let a_prime = s0;                                           /* a = slot[0] (direct)               */
                    let b_prime = s1;                                           /* b = slot[1] (direct)               */
                    let sig = (b_prime >> 32) as u64;                           /* bits 32-95 = MoveSignature         */
                    let pseudo_mv: PseudoMove = (a_prime, sig);
                    if pseudo_mv == null_pseudo_move() {
                        None
                    } else {
                        Some(pseudo_mv)
                    }
                }
            }
        }
    }};
}

/// Stores a search result in the TT with seqlock write protection and parity.
///
/// Write order:
///
/// version++ (lock) → slot[0] → slot[1] → slot[2] → age → version++ (unlock).
///
/// Parity (slot[2]) is written last so a reader seeing fresh parity sees fresh
/// data too.
#[macro_export]
macro_rules! hash_tt_entry {
    (
        $tt_move:expr, $score:expr, $flags:expr,
        $depth:expr, $state:expr, $table:expr
    ) => {{
        let hash = $state.position_hash;
        let index = tt_index!(hash);
        let table_vec: &mut Vec<TTEntry> = unsafe { &mut *($table.table.get()) };
        let entry = &mut table_vec[index];

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
        let sig = move_signature!($tt_move);                                    /* XOR of move.1 entries              */
        let b = ((sig as u128) << 32) | (encoded as u128);                      /* sig << 32 | encoded                */

        let cur_age = $table.age.load(Ordering::Relaxed);
        let old_s0 = entry.slot[0];
        let old_s1 = entry.slot[1];
        let old_s2 = entry.slot[2];
        let empty = old_s0 == 0 && old_s1 == 0 && old_s2 == 0;
        let different = old_s0 ^ old_s1 ^ old_s2 != hash;
        let old_enc = (old_s1 & 0xFFFF_FFFF) as u32;
        let old_depth = tt_depth!(old_enc);
        let old_flags = tt_flags!(old_enc);
        let should_write = empty
            || different
            || entry.age < cur_age
            || $depth >= old_depth
            || ($flags == FEXACT && old_flags != FEXACT);

        if should_write {
            if empty {
                $table.new_write.fetch_add(1, Ordering::Relaxed);
            } else {
                $table.over_write.fetch_add(1, Ordering::Relaxed);
            }

            entry.version.fetch_add(1, Ordering::Release);                      /* seqlock lock: version now odd      */
            entry.slot[0] = a;                                                  /* a = move.0 (raw)                   */
            entry.slot[1] = b;                                                  /* b = sig<<32|encoded (raw)          */
            entry.slot[2] = a ^ b ^ hash;                                       /* parity = a ^ b ^ c (written last)  */
            entry.age = cur_age;
            entry.version.fetch_add(1, Ordering::Release);                      /* seqlock unlock: version now even   */
        }
    }};
}

#[macro_export]
macro_rules! fill_pv_line {
    ($state:expr, $table:expr, $depth:expr) => {{
        let depth = ($depth).min(MAX_DEPTH);
        let mut filled = 0;

        for i in 0..depth {
            let Some(pv_pseudo) = probe_pv_move!($state, $table) else {
                break;
            };

            let mut pv_buf: Vec<Move> = Vec::with_capacity(64);
            let mut pv_scratch: Vec<u64> = Vec::with_capacity(16);
            generate_all_moves_and_drops($state, &mut pv_buf, &mut pv_scratch);
            let mut pv_match: Option<Move> = None;

            for mv in pv_buf {
                let legal = make_move!($state, mv.clone());
                if legal { undo_move!($state); }

                if mv.0 == pv_pseudo.0
                && move_signature!(mv) == pv_pseudo.1
                && legal {
                    pv_match = Some(mv);
                    break;
                }
            }

            let Some(pv_move) = pv_match else { break; };

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
