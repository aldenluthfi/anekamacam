//! # transposition.rs
//!
//! Transposition table for caching and reusing search results across the tree.
//!
//! Positions are keyed by Zobrist hash. A cache hit at sufficient depth returns
//! the stored score directly, skipping the subtree. Entries record a bound
//! type (exact, alpha, beta), depth, best move, and age for replacement
//! policy. Thread safety uses a seqlock with parity verification across the
//! 3×u128 slot layout.
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

/// TTEntry
///
/// TT entry with a 3×u128 XOR-parity slot and a seqlock version counter.
///
/// Layout:
/// - slot[0] = move.0 (128-bit, raw)
/// - slot[1] = sig << 41 | score << 9 | flags_depth (128-bit, raw)
/// - slot[2] = slot[0] ^ slot[1] ^ hash (parity, written last)
/// - age     = plain u64, excluded from parity
/// - version = seqlock counter (odd = write in progress, even = readable)
///
/// Write order: version++ → slot[0] → slot[1] → slot[2] → age → version++
///
/// Validation:
///   (1) slot[0] ^ slot[1] ^ slot[2] == position_hash  →  parity intact
///   (2) version unchanged across read                  →  no torn write
#[derive(Default)]
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

/// TTable
///
/// Shared transposition table using seqlock+parity for lock-free thread safety.
/// Entries are read with a seqlock: readers check version parity before and
/// after the slot load and retry on mismatch. The XOR parity across slot[0..2]
/// catches cross-entry corruption. Age is bumped each search for replacement.
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

impl TTable {
    /// TTable method cluster.
    ///
    /// `with_mb` sizes the table to a memory budget in megabytes (the UCI
    /// Hash option), `len` reports the slot count, and `is_empty` scans
    /// for any written entry; the latter two exist mainly for tests and
    /// diagnostics.
    pub fn with_mb(mb: usize) -> Self {
        let size = (mb * 1024 * 1024 / size_of::<TTEntry>()).max(1);
        Self::with_entries(size)
    }

    pub fn with_entries(entries: usize) -> Self {
        Self {
            table: SyncUnsafeCell::new(
                vec![TTEntry::default(); entries]
            ),
            age: AtomicU64::new(0),
            new_write: AtomicU64::new(0),
            over_write: AtomicU64::new(0),
            hit: AtomicU64::new(0),
            valid: AtomicU64::new(0),
        }
    }

    pub fn len(&self) -> usize {
        unsafe { &*self.table.get() }.len()
    }

    pub fn is_empty(&self) -> bool {
        unsafe { &*self.table.get() }.iter().all(|entry| {
            entry.slot[0] == 0 && entry.slot[1] == 0 && entry.slot[2] == 0
        })
    }
}

/*----------------------------------------------------------------------------*\
                       TRANSPOSITION TABLE PACKING HELPERS
\*----------------------------------------------------------------------------*/

/// Main-table packing macros.
///
/// `tt_index!` maps a Zobrist hash onto a table slot; the `tt_enc_*`
/// writers pack bound flags (bits 0-1), clamped depth (bits 2-8), and
/// score (bits 9-40) into the encoded word stored in `slot[1]`; the
/// `tt_flags!` / `tt_depth!` / `tt_score!` readers extract them again.
#[macro_export]
macro_rules! tt_index {
    ($hash:expr, $size:expr) => {{
        ($hash as usize) % $size
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
        $encoded |= ($val as u32 as u128) << 9;
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
    ($b_prime:expr) => {
        (($b_prime >> 9) & 0xFFFF_FFFF) as u32 as i32
    };
}

/*----------------------------------------------------------------------------*\
                        TRANSPOSITION TABLE STORE / PROBE
\*----------------------------------------------------------------------------*/

/// probe_tt_entry!
///
/// Probes the TT with parity + seqlock integrity check; returns (valid,
/// score, pseudo_move).
///
/// Validation:
///   Step 1: version is even (no write in progress)
///   Step 2: (slot[0] ^ slot[1] ^ slot[2]) == position_hash
///   Step 3: version unchanged during read (seqlock: no torn write)
///
/// On failure returns (false, i32::MIN, null_pseudo_move()).
///
/// Params:
/// - state -> position whose hash is probed
/// - table -> the shared transposition table
/// - alpha -> lower search bound at this node
/// - beta  -> upper search bound at this node
/// - depth -> minimum stored depth for the score to be usable
///
/// Return:
/// (bool, i32, PseudoMove) -> (cutoff valid, score, stored best move)
///
#[macro_export]
macro_rules! probe_tt_entry {
    ($state:expr, $table:expr, $alpha:expr, $beta:expr, $depth:expr) => {
        hotpath::measure_block!("tt::probe", {
        let hash = $state.position_hash;
        let index = tt_index!(hash, $table.len());
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
                    let encoded = (b_prime & 0x1FF) as u32;                     /* bits  0-8  = flags/depth           */
                    let sig     = (b_prime >> 41) as u64;                       /* bits 41-104 = MoveSignature        */
                    let pseudo_mv: PseudoMove = (a_prime, sig);

                    if tt_depth!(encoded) < $depth {
                        (false, i32::MIN, pseudo_mv)
                    } else {
                        let mut entry_score = tt_score!(b_prime);

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
        })
    };
}

/// probe_pv_move!
///
/// Reduced probe used when extending the printed principal variation:
/// runs the same parity and seqlock validation as `probe_tt_entry!` but
/// ignores depth and bounds, returning only the stored best move.
///
/// Params:
/// - state -> position whose hash is probed
/// - table -> the shared transposition table
///
/// Return:
/// Option<PseudoMove> -> the stored move, or None on any miss
///
#[macro_export]
macro_rules! probe_pv_move {
    ($state:expr, $table:expr) => {{
        let hash = $state.position_hash;
        let index = tt_index!(hash, $table.len());
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
                    let sig = (b_prime >> 41) as u64;                           /* bits 41-104 = MoveSignature        */
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

/// hash_tt_entry!
///
/// Stores a search result in the TT with seqlock write protection and parity.
///
/// Write order:
///
/// version++ (lock) → slot[0] → slot[1] → slot[2] → age → version++ (unlock).
///
/// Parity (slot[2]) is written last so a reader seeing fresh parity sees fresh
/// data too.
///
/// Params:
/// - tt_move -> best move found at this node
/// - score   -> score to store (mate scores are ply-adjusted)
/// - flags   -> bound type: FEXACT, FALPHA, or FBETA
/// - depth   -> search depth the score is valid for
/// - state   -> position whose hash keys the entry
/// - table   -> the shared transposition table
///
#[macro_export]
macro_rules! hash_tt_entry {
    (
        $tt_move:expr,
        $score:expr,
        $flags:expr,
        $depth:expr,
        $state:expr,
        $table:expr
    ) => {
        hotpath::measure_block!("tt::store", {
        let hash = $state.position_hash;
        let index = tt_index!(hash, $table.len());
        let table_vec: &mut Vec<TTEntry> =
            unsafe { &mut *($table.table.get()) };
        let entry = &mut table_vec[index];

        let mut flags_depth = 0u32;
        tt_enc_flags!(flags_depth, $flags);
        tt_enc_depth!(flags_depth, $depth);

        let mut store_score = $score;

        if store_score > MATE_SCORE {
            store_score += $state.search_ply as i32;
        } else if store_score < -MATE_SCORE {
            store_score -= $state.search_ply as i32;
        }

        let mut encoded: u128 = flags_depth as u128;
        tt_enc_score!(encoded, store_score);

        let sig = m_signature!($tt_move);                                       /* XOR of move.1 entries              */
        let age = $table.age.load(Ordering::Relaxed);
        let a = $tt_move.0;                                                     /* move.0 128-bit                     */
        let b = ((sig as u128) << 41) | encoded;                                /* sig << 41 | score<<9 | flags_depth */

        let old_s0 = entry.slot[0];
        let old_s1 = entry.slot[1];
        let old_s2 = entry.slot[2];

        let empty = old_s0 == 0 && old_s1 == 0 && old_s2 == 0;
        let different = old_s0 ^ old_s1 ^ old_s2 != hash;

        let old_enc = (old_s1 & 0x1FF) as u32;
        let old_depth = tt_depth!(old_enc);
        let old_score = tt_score!(old_s1);
        let old_flags = tt_flags!(old_enc);

        let should_write = empty
            || different
            || entry.age < age
            || old_depth <= $depth
            || old_score <= $score
            || old_flags != FEXACT && $flags == FEXACT;

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
            entry.age = age;
            entry.version.fetch_add(1, Ordering::Release);                      /* seqlock unlock: version now even   */
        }
        })
    };
}

/// fill_pv_line!
///
/// Reconstructs the full principal variation for reporting: copies the
/// triangular PV table into `pv_line`, then walks the line on the board
/// and extends it move by move from TT probes until the table runs dry,
/// a probed move proves illegal, or the target depth is reached. All
/// moves are undone before returning, leaving the position unchanged.
///
/// Params:
/// - state -> position walked and restored
/// - table -> the shared transposition table
/// - depth -> maximum PV length to reconstruct
///
#[macro_export]
macro_rules! fill_pv_line {
    ($state:expr, $table:expr, $depth:expr) => {{
        let triangular_length = $state.pv_length[0].min(MAX_DEPTH);

        for index in 0..triangular_length {
            $state.pv_line[index] = $state.pv_table[index].clone();
        }

        for slot in 0..triangular_length {
            let pv_move = $state.pv_line[slot].clone();
            make_move!($state, pv_move);
        }

        let mut out: Vec<Move> = Vec::with_capacity(64);
        let mut scratch: Vec<u64> = Vec::with_capacity(16);

        for slot in triangular_length..$depth {
            let Some(pm) = probe_pv_move!($state, $table) else {
                break;
            };

            generate_all_moves_and_drops(
                $state, &mut out, &mut scratch
            );

            let mut pv_cand = None;

            for mv in out.iter() {

                if !make_move!($state, mv.clone()) {
                    continue;
                }

                if m_matches!(mv, pm) {
                    pv_cand = Some(mv.clone());
                    break;
                }

                undo_move!($state);
            }

            let Some(pv_move) = pv_cand else {
                break;
            };

            $state.pv_line[slot] = pv_move;
        }

        for index in ($state.search_ply as usize)..MAX_DEPTH {
            $state.pv_line[index] = null_move();
        }

        while $state.search_ply > 0 {
            undo_move!($state);
        }
    }};
}

/*----------------------------------------------------------------------------*\
              QSEARCH TT ENTRY REPRESENTATION & CONSTANTS
\*----------------------------------------------------------------------------*/

/// QTEntry
///
/// QSearch TT entry — 3×u128 XOR-parity slot with seqlock.
///
/// Layout:
/// - slot[0] = move.0 (128-bit, raw)
/// - slot[1] = sig << 32 | encoded (128-bit, raw)
/// - slot[2] = slot[0] ^ slot[1] ^ hash (parity, written last)
/// - age     = plain u64 (excluded from parity)
/// - version = seqlock counter (odd = writing, even = readable)
///
/// Write order: version++ → slot[0] → slot[1] → slot[2] → age → version++
///
/// Validation:
///   (1) slot[0] ^ slot[1] ^ slot[2] == position_hash  →  parity intact
///   (2) version unchanged across read                  →  no torn write
#[derive(Default)]
pub struct QTEntry {
    pub slot:   [u128; 3],                                                      /* [key, data1, data2]                */
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

/// QTable
///
/// Quiescence-search transposition table; same seqlock+parity scheme as
/// TTable. Uses QTEntry slots instead of TTEntry; otherwise identical
/// thread-safety
/// invariants and age-based replacement policy apply.
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

impl QTable {
    /// QTable method cluster.
    ///
    /// Mirror of the `TTable` methods: `with_mb` sizes the table to a
    /// megabyte budget, `len` reports slot count, and `is_empty` scans
    /// for any written entry.
    pub fn with_mb(mb: usize) -> Self {
        let size = (mb * 1024 * 1024 / size_of::<QTEntry>()).max(1);
        Self::with_entries(size)
    }

    pub fn with_entries(entries: usize) -> Self {
        Self {
            table: SyncUnsafeCell::new(
                vec![QTEntry::default(); entries]
            ),
            age: AtomicU64::new(0),
            new_write: AtomicU64::new(0),
            over_write: AtomicU64::new(0),
            hit: AtomicU64::new(0),
            valid: AtomicU64::new(0),
        }
    }

    pub fn len(&self) -> usize {
        unsafe { &*self.table.get() }.len()
    }

    pub fn is_empty(&self) -> bool {
        unsafe { &*self.table.get() }.iter().all(|entry| {
            entry.slot[0] == 0 && entry.slot[1] == 0 && entry.slot[2] == 0
        })
    }
}

/*----------------------------------------------------------------------------*\
                     QSEARCH TT PACKING / UNPACKING MACROS
\*----------------------------------------------------------------------------*/

/// Qsearch-table packing macros.
///
/// Counterparts of the `tt_*` packing family for the smaller qsearch
/// entry: `qt_index!` maps a hash onto a slot, `qt_enc_score!` packs a
/// sign-extended 16-bit score, `qt_enc_flags!` packs the bound type into
/// bits 16-17, and `qt_score!` / `qt_flags!` read them back.
#[macro_export]
macro_rules! qt_index {
    ($hash:expr, $size:expr) => {{
        ($hash as usize) % $size
    }};
}

#[macro_export]
macro_rules! qt_enc_score {
    ($score:expr) => {{
        (($score as i16) as u32) & 0xFFFF
    }};
}

#[macro_export]
macro_rules! qt_enc_flags {
    ($flags:expr) => {{
        ((($flags as u32) & 0x3) << 16)
    }};
}

#[macro_export]
macro_rules! qt_score {
    ($encoded:expr) => {{
        (($encoded & 0xFFFF) as i32) << 16 >> 16
    }};
}

#[macro_export]
macro_rules! qt_flags {
    ($encoded:expr) => {{
        (($encoded >> 16) & 0x3) as u8
    }};
}

/*----------------------------------------------------------------------------*\
                     QSEARCH TT PROBE & STORE MACROS
\*----------------------------------------------------------------------------*/

/// probe_qt_entry!
///
/// Probes the qsearch TT with seqlock validation; returns (valid, score,
/// pseudo_move).
///
/// Validation:
///   Step 1: version is even (no write in progress)
///   Step 2: version unchanged after both reads (no torn write)
///   Step 3: sig field matches the stored MoveSignature (anti-collision)
///
/// Params:
/// - state  -> position whose hash is probed
/// - qtable -> the shared qsearch table
/// - alpha  -> lower search bound at this node
/// - beta   -> upper search bound at this node
///
/// Return:
/// (bool, i32, PseudoMove) -> (cutoff valid, score, stored best move)
///
#[macro_export]
macro_rules! probe_qt_entry {
    ($state:expr, $qtable:expr, $alpha:expr, $beta:expr) => {
        hotpath::measure_block!("qt::probe", {
        let hash = $state.position_hash;
        let index = qt_index!(hash, $qtable.len());
        let entry = &mut unsafe { &mut *($qtable.table.get()) }[index];

        let v1 = entry.version.load(Ordering::Acquire);
        if v1 & 1 != 0 {
            (false, i32::MIN, null_pseudo_move())                               /* write in progress                  */
        } else {
            let s0 = entry.slot[0];
            let s1 = entry.slot[1];
            let s2 = entry.slot[2];

            if s0 ^ s1 ^ s2 != hash {                                           /* parity check: all slots covered    */
                (false, i32::MIN, null_pseudo_move())
            } else {
                $qtable.hit.fetch_add(1, Ordering::Relaxed);                    /* parity matched                     */
                let v2 = entry.version.load(Ordering::Acquire);

                if v1 != v2 {                                                   /* seqlock: torn read detected        */
                    (false, i32::MIN, null_pseudo_move())
                } else {
                    $qtable.valid.fetch_add(1, Ordering::Relaxed);
                    let move_0  = s0;                                           /* move.0 raw                         */
                    let encoded = (s1 & 0xFFFF_FFFF) as u32;                    /* bits  0–31 = score+flags           */
                    let sig     = (s1 >> 32) as u64;                            /* bits 32-95 = MoveSignature         */
                    let pseudo_mv: PseudoMove = (move_0, sig);

                    let entry_flags   = qt_flags!(encoded);
                    let mut entry_score = qt_score!(encoded);

                    if entry_score > MATE_SCORE {
                        entry_score -= $state.search_ply as i32;
                    } else if entry_score < -MATE_SCORE {
                        entry_score += $state.search_ply as i32;
                    }

                    let mut valid_cutoff = false;
                    match entry_flags {
                        FBETA => {
                            if entry_score >= $beta {
                                entry_score = $beta;
                                valid_cutoff = true;
                            }
                        }
                        FEXACT => valid_cutoff = true,
                        _ => unreachable!(),
                    }

                    if pseudo_mv == null_pseudo_move() {
                        valid_cutoff = false;
                    }

                    (valid_cutoff, entry_score, pseudo_mv)
                }
            }
        }
        })
    };
}

/// hash_qt_entry!
///
/// Stores a qsearch result in the dedicated TT.
/// Only capture/check/promotion moves are written; quiet moves are skipped.
/// Replacement policy: empty slot → always write; occupied → write if
/// entry is stale (age < cur_age - 1) or new entry is FEXACT.
///
/// Params:
/// - tt_move -> best move found at this qsearch node
/// - score   -> score to store (mate scores are ply-adjusted)
/// - flags   -> bound type: FEXACT or FBETA
/// - state   -> position whose hash keys the entry
/// - qtable  -> the shared qsearch table
///
#[macro_export]
macro_rules! hash_qt_entry {
    ($tt_move:expr, $score:expr, $flags:expr, $state:expr, $qtable:expr) => {
        hotpath::measure_block!("qt::store", {
        let hash = $state.position_hash;
        let index = qt_index!(hash, $qtable.len());
        let table_vec: &mut Vec<QTEntry> =
            unsafe { &mut *($qtable.table.get()) };
        let entry = &mut table_vec[index];

        let mut store_score = $score;
        if store_score > MATE_SCORE {
            store_score += $state.search_ply as i32;
        } else if store_score < -MATE_SCORE {
            store_score -= $state.search_ply as i32;
        }

        let encoded = qt_enc_score!(store_score) | qt_enc_flags!($flags);

        let age = $qtable.age.load(Ordering::Relaxed);
        let sig = m_signature!($tt_move);
        let a = $tt_move.0;
        let b = ((sig as u128) << 32) | (encoded as u128);

        let old_s0 = entry.slot[0];
        let old_s1 = entry.slot[1];
        let old_s2 = entry.slot[2];

        let empty = old_s0 == 0 && old_s1 == 0 && old_s2 == 0;
        let different = old_s0 ^ old_s1 ^ old_s2 != hash;

        let old_enc = (old_s1 & 0xFFFF_FFFF) as u32;
        let old_score = qt_score!(old_enc);
        let old_flags = qt_flags!(old_enc);

        let should_write = empty
            || different
            || entry.age < age
            || old_score <= $score
            || old_flags != FEXACT && $flags == FEXACT;

        if should_write {
            if empty {
                $qtable.new_write.fetch_add(1, Ordering::Relaxed);
            } else {
                $qtable.over_write.fetch_add(1, Ordering::Relaxed);
            }

            entry.version.fetch_add(1, Ordering::Release);
            entry.slot[0] = a;
            entry.slot[1] = b;
            entry.slot[2] = a ^ b ^ hash;
            entry.age = age;
            entry.version.fetch_add(1, Ordering::Release);
        }
        })
    };
}

/*----------------------------------------------------------------------------*\
                PAWN STRUCTURE TABLE ENTRY REPRESENTATION
\*----------------------------------------------------------------------------*/

/// PTEntry
///
/// Pawn structure table entry — 2×u128 XOR-parity slot with seqlock.
/// Keyed by the pawn-only Zobrist hash; the payload is the pawn structure
/// evaluation, a pure function of the key, so entries never go stale and
/// no age field is needed.
///
/// Layout:
/// - slot[0] = endgame << 32 | opening (u32 bit patterns, 128-bit raw)
/// - slot[1] = slot[0] ^ pawn_hash (parity, written last)
/// - version = seqlock counter (odd = write in progress, even = readable)
///
/// Write order: version++ → slot[0] → slot[1] → version++
///
/// Validation:
///   (1) slot[0] ^ slot[1] == pawn_hash  →  parity intact
///   (2) version unchanged across read   →  no torn write
#[derive(Default)]
pub struct PTEntry {
    pub slot:   [u128; 2],                                                      /* [payload, parity]                  */
    pub version: AtomicU64,                                                     /* seqlock: odd = writing, even = ok  */
}

impl Clone for PTEntry {
    fn clone(&self) -> Self {
        PTEntry {
            slot: self.slot,
            version: AtomicU64::new(self.version.load(Ordering::Relaxed)),
        }
    }
}

/// PTable
///
/// Shared pawn structure table; same seqlock+parity scheme as TTable and
/// QTable. Because stored scores derive purely from the pawn hash, the
/// replacement policy is write-always and the table needs no aging; it is
/// fixed-size and persists across searches.
pub struct PTable {
    pub table: SyncUnsafeCell<Vec<PTEntry>>,                                    /* shared mutable access              */
    pub new_write: AtomicU64,                                                   /* writes to empty slots              */
    pub over_write: AtomicU64,                                                  /* writes replacing existing entries  */
    pub hit: AtomicU64,                                                         /* probes where parity matched        */
    pub valid: AtomicU64,                                                       /* probes where read was consistent   */
}

unsafe impl Sync for PTable {}
unsafe impl Send for PTable {}

impl Default for PTable {
    fn default() -> Self {
        Self::with_entries(P_TABLE_SIZE)
    }
}

impl PTable {
    /// PTable method cluster.
    ///
    /// Reduced mirror of the `QTable` methods: `with_mb` sizes the table
    /// from a megabyte budget, `len` reports slot count, and `is_empty`
    /// scans for any written entry.
    pub fn with_mb(mb: usize) -> Self {
        let size = (mb * 1024 * 1024 / size_of::<PTEntry>()).max(1);
        Self::with_entries(size)
    }

    pub fn with_entries(entries: usize) -> Self {
        Self {
            table: SyncUnsafeCell::new(vec![PTEntry::default(); entries]),
            new_write: AtomicU64::new(0),
            over_write: AtomicU64::new(0),
            hit: AtomicU64::new(0),
            valid: AtomicU64::new(0),
        }
    }
    pub fn len(&self) -> usize {
        unsafe { &*self.table.get() }.len()
    }

    pub fn is_empty(&self) -> bool {
        unsafe { &*self.table.get() }.iter().all(|entry| {
            entry.slot[0] == 0 && entry.slot[1] == 0
        })
    }
}

/*----------------------------------------------------------------------------*\
                  PAWN STRUCTURE TABLE PROBE & STORE MACROS
\*----------------------------------------------------------------------------*/

/// Pawn-table access macros.
///
/// `pt_index!` maps a pawn hash onto a slot; `probe_pt_entry!` reads an
/// entry with seqlock validation and returns
/// `(hit, opening, endgame)`; `hash_pt_entry!` writes one
/// unconditionally. The two score halves travel as u32 bit patterns in
/// the low 64 bits of slot[0].
#[macro_export]
macro_rules! pt_index {
    ($hash:expr, $size:expr) => {{
        ($hash as usize) % $size
    }};
}

#[macro_export]
macro_rules! probe_pt_entry {
    ($state:expr, $ptable:expr) => {
        hotpath::measure_block!("pt::probe", {
        let hash = $state.pawn_hash;
        let index = pt_index!(hash, $ptable.len());
        let entry = &mut unsafe { &mut *($ptable.table.get()) }[index];

        let v1 = entry.version.load(Ordering::Acquire);
        if v1 & 1 != 0 {
            (false, 0i32, 0i32)                                                 /* write in progress                  */
        } else {
            let s0 = entry.slot[0];
            let s1 = entry.slot[1];

            if s0 ^ s1 != hash {                                                /* parity check: both slots covered   */
                (false, 0i32, 0i32)
            } else {
                $ptable.hit.fetch_add(1, Ordering::Relaxed);                    /* parity matched                     */
                let v2 = entry.version.load(Ordering::Acquire);

                if v1 != v2 {                                                   /* seqlock: torn read detected        */
                    (false, 0i32, 0i32)
                } else {
                    $ptable.valid.fetch_add(1, Ordering::Relaxed);
                    let opening = (s0 as u32) as i32;
                    let endgame = ((s0 >> 32) as u32) as i32;

                    (true, opening, endgame)
                }
            }
        }
        })
    };
}

#[macro_export]
macro_rules! hash_pt_entry {
    ($opening:expr, $endgame:expr, $state:expr, $ptable:expr) => {
        hotpath::measure_block!("pt::store", {
        let hash = $state.pawn_hash;
        let index = pt_index!(hash, $ptable.len());
        let table_vec: &mut Vec<PTEntry> =
            unsafe { &mut *($ptable.table.get()) };
        let entry = &mut table_vec[index];

        let a = (($endgame as u32) as u128) << 32
            | (($opening as u32) as u128);

        if entry.slot[0] == 0 && entry.slot[1] == 0 {
            $ptable.new_write.fetch_add(1, Ordering::Relaxed);
        } else {
            $ptable.over_write.fetch_add(1, Ordering::Relaxed);
        }

        entry.version.fetch_add(1, Ordering::Release);
        entry.slot[0] = a;
        entry.slot[1] = a ^ hash;
        entry.version.fetch_add(1, Ordering::Release);
        })
    };
}
