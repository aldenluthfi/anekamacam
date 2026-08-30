//! transposition.rs
//!
//! Transposition table for caching and reusing search results across the tree.
//!
//! Positions are keyed by Zobrist hash. A cache hit at sufficient depth returns
//! the stored score directly, skipping the subtree. Entries record a bound
//! type (exact, alpha, beta), depth, best move, and age for replacement
//! policy. Thread safety uses a seqlock with parity verification across the
//! 3×u128 slot layout.
//!
//! Created: 29/01/2026
//! Author : Alden Luthfi

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
/// - slot[1] = packed search payload (bit 0 = LSB):
///
///   Field widths are proportional; every row represents 32 bits.
///
///   Bits 0..31:
///
/// ```text
///   0   2             9                                             31
///   ┌───┬─────────────┬──────────────────────────────────────────────┐
///   │flg│    depth    │                   score →                    │
///   └───┴─────────────┴──────────────────────────────────────────────┘
///
///   Bits 32..63:
///
///   32                41                                            63
///   ┌─────────────────┬──────────────────────────────────────────────┐
///   │     ← score     │                 signature →                  │
///   └─────────────────┴──────────────────────────────────────────────┘
///
///   Bits 64..95:
///
///   64                                                              95
///   ┌────────────────────────────────────────────────────────────────┐
///   │                         ← signature →                          │
///   └────────────────────────────────────────────────────────────────┘
///
///   Bits 96..127:
///
///   96                105                                          127
///   ┌─────────────────┬──────────────────────────────────────────────┐
///   │   ← signature   │                   unused                     │
///   └─────────────────┴──────────────────────────────────────────────┘
/// ```
///
///
///   - flag       : bound type (FEXACT / FALPHA / FBETA)
///   - depth      : clamped search depth
///   - score      : ply-adjusted node score (32-bit)
///   - signature  : MoveSignature of the stored best move
///   - bits 105..127: unused
///
/// - slot[2] = slot[0] ^ slot[1] ^ hash (parity, written last)
/// - age     = plain u64, excluded from parity
/// - version = seqlock counter (odd = write in progress, even = readable)
///
/// Write order: version++ → slot[0] → slot[1] → slot[2] → age → version++
///
/// Validation:
///   (1) slot[0] ^ slot[1] ^ slot[2] == position_hash  →  parity intact
///   (2) version unchanged across read                 →  no torn write
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
        Self::with_mb(HASH_DEFAULT_MB * 2 / 3)
    }
}

impl TTable {
    /// TTable method cluster.
    ///
    /// `with_mb` sizes the table to a memory budget in megabytes (the UCI
    /// Hash option), `len` reports the slot count, and `is_empty` scans
    /// for any written entry; the latter two exist mainly for tests and
    /// diagnostics. Slot counts are floored to a power of two so index
    /// macros can mask instead of taking a modulo.
    ///
    /// with_mb
    ///
    ///   Params:
    ///   - mb: usize -> memory budget in megabytes
    ///
    ///   Return:
    ///   Self        -> zeroed table sized to the budget
    ///
    /// with_entries
    ///
    ///   Params:
    ///
    ///   - entries: usize
    ///     requested slot count, floored to a power of two
    ///
    ///   Return:
    ///   Self -> zeroed table with that many slots
    ///
    /// len
    ///
    ///   Return:
    ///   usize -> slot count
    ///
    /// is_empty
    ///
    ///   Return:
    ///   bool -> whether no slot has ever been written
    pub fn with_mb(mb: usize) -> Self {
        let size = (mb * 1024 * 1024 / size_of::<TTEntry>()).max(1);
        Self::with_entries(size)
    }

    pub fn with_entries(entries: usize) -> Self {
        Self {
            table: SyncUnsafeCell::new(
                vec![TTEntry::default(); 1 << entries.max(1).ilog2()]
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
/// `tt_index!` masks a Zobrist hash onto a power-of-two table slot. The
/// writers pack bound flags (bits 0-1), clamped depth (bits 2-8), and score
/// (bits 9-40) into `slot[1]`; the readers extract those fields again.
///
/// tt_index!
///
///   Params:
///   - hash   : PositionHash -> Zobrist key of the probed position
///   - size   : usize        -> table slot count (power of two)
///
///   Return:
///   usize                   -> slot index, `hash & (size - 1)`
///
/// The writers OR into place and return nothing:
///
/// tt_enc_flags!
///
///   Params:
///   - encoded: &mut u32 -> flags/depth word being built
///   - val    : u8       -> bound flag, masked into bits 0-1
///
/// tt_enc_depth!
///
///   Params:
///   - encoded: &mut u32 -> flags/depth word being built
///   - val    : usize    -> depth, clamped and masked into bits 2-8
///
/// tt_enc_score!
///
///   Params:
///   - encoded: &mut u128 -> slot[1] word being built
///   - val    : i32       -> score, masked into bits 9-40
///
#[macro_export]
macro_rules! tt_index {
    ($hash:expr, $size:expr) => {{
        ($hash as usize) & ($size - 1)
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
/// Probes the main table with parity and seqlock validation. Stored depth and
/// bound flags decide whether the score can cut; the stored move is returned
/// on every valid hash match for move ordering.
///
/// Params:
/// - state: &State  -> position the stored mate scores are relative to
/// - key  : u128    -> search key this node is filed under
/// - table: &TTable -> shared transposition table
/// - alpha: i32     -> lower search bound
/// - beta : i32     -> upper search bound
/// - depth: usize   -> minimum stored depth for a cutoff
///
/// Return:
/// (bool, i32, PseudoMove) -> cutoff validity, score, and stored move
#[macro_export]
macro_rules! probe_tt_entry {
    (
        $state:expr,
        $key:expr,
        $table:expr,
        $alpha:expr,
        $beta:expr,
        $depth:expr
    ) => {
        hotpath::measure_block!("tt::probe", {
        let hash = $key;
        let index = tt_index!(hash, $table.len());
        let entry = &mut unsafe { &mut *($table.table.get()) }[index];

        let first_version = entry.version.load(Ordering::Acquire);

        if first_version & 1 != 0 {
            (false, i32::MIN, null_pseudo_move())
        } else {
            let move_slot = entry.slot[0];
            let data_slot = entry.slot[1];
            let parity_slot = entry.slot[2];

            if move_slot ^ data_slot ^ parity_slot != hash {
                (false, i32::MIN, null_pseudo_move())
            } else {
                $table.hit.fetch_add(1, Ordering::Relaxed);
                let second_version = entry.version.load(Ordering::Acquire);

                if first_version != second_version {
                    (false, i32::MIN, null_pseudo_move())
                } else {
                    $table.valid.fetch_add(1, Ordering::Relaxed);

                    let encoded = (data_slot & 0x1FF) as u32;
                    let signature = (data_slot >> 41) as u64;
                    let pseudo_move = (move_slot, signature);
                    let entry_depth = tt_depth!(encoded);
                    let entry_flags = tt_flags!(encoded);
                    let mut entry_score = tt_score!(data_slot);

                    if entry_score > MATE_SCORE {
                        entry_score -= $state.search_ply as i32;
                    } else if entry_score < -MATE_SCORE {
                        entry_score += $state.search_ply as i32;
                    }

                    if entry_depth < $depth {
                        (false, i32::MIN, pseudo_move)
                    } else {
                        let mut valid_cutoff = false;
                        let mut cutoff_score = entry_score;

                        match entry_flags {
                            FALPHA => {
                                if cutoff_score <= $alpha {
                                    cutoff_score = $alpha;
                                    valid_cutoff = true;
                                }
                            }
                            FBETA => {
                                if cutoff_score >= $beta {
                                    cutoff_score = $beta;
                                    valid_cutoff = true;
                                }
                            }
                            FEXACT => valid_cutoff = true,
                            _ => unreachable!(),
                        }

                        (valid_cutoff, cutoff_score, pseudo_move)
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
/// - key  : u128      -> search key this node is filed under
/// - table: &TTable   -> the shared transposition table
///
/// Return:
/// Option<PseudoMove> -> the stored move, or None on any miss
#[macro_export]
macro_rules! probe_pv_move {
    ($key:expr, $table:expr) => {{
        let hash = $key;
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
/// Stores one main-search result with seqlock write protection and parity.
///
/// Params:
/// - tt_move: &Move   -> best move found at this node
/// - score  : i32     -> score to store
/// - flags  : u8      -> FEXACT, FALPHA, or FBETA
/// - depth  : usize   -> search depth the score is valid for
/// - state  : &State  -> position the stored mate scores are relative to
/// - key    : u128    -> search key this node is filed under
/// - table  : &TTable -> shared transposition table
#[macro_export]
macro_rules! hash_tt_entry {
    (
        $tt_move:expr,
        $score:expr,
        $flags:expr,
        $depth:expr,
        $state:expr,
        $key:expr,
        $table:expr
    ) => {
        hotpath::measure_block!("tt::store", {
        let hash = $key;
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

        let mut encoded = flags_depth as u128;
        tt_enc_score!(encoded, store_score);

        let signature = m_signature!($tt_move);
        let age = $table.age.load(Ordering::Relaxed);
        let move_slot = $tt_move.0;
        let data_slot = ((signature as u128) << 41) | encoded;

        let old_move = entry.slot[0];
        let old_data = entry.slot[1];
        let old_parity = entry.slot[2];

        let empty = old_move == 0 && old_data == 0 && old_parity == 0;
        let different = old_move ^ old_data ^ old_parity != hash;

        let old_encoded = (old_data & 0x1FF) as u32;
        let old_depth = tt_depth!(old_encoded);
        let old_score = tt_score!(old_data);
        let old_flags = tt_flags!(old_encoded);

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

            entry.version.fetch_add(1, Ordering::Release);
            entry.slot[0] = move_slot;
            entry.slot[1] = data_slot;
            entry.slot[2] = move_slot ^ data_slot ^ hash;
            entry.age = age;
            entry.version.fetch_add(1, Ordering::Release);
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
/// `pv_table` is a flat `PV_STRIDE * PV_STRIDE` upper triangle: row
/// `ply` starts at `ply * PV_STRIDE` and uses only the columns from its
/// own ply onward. When a move improves alpha at `ply`, search writes it
/// at `[ply][ply]` and copies the child row's tail up one row, so row 0
/// always carries the complete line (`pv_length[ply]` is the absolute
/// end column of row `ply`):
///
/// ```text
///          col 0  col 1  col 2  col 3
///        ┌──────┬──────┬──────┬──────┐
/// ply 0  │  m0  │  m1  │  m2  │  m3  │  pv_length[0] = 4
///        └──────┼──────┼──────┼──────┤
/// ply 1         │  m1  │  m2  │  m3  │  copied up after m0 improves
///               └──────┼──────┼──────┤
/// ply 2                │  m2  │  m3  │
///                      └──────┼──────┤
/// ply 3                       │  m3  │
///                             └──────┘
/// ```
///
/// Every walked move — triangular or probed — is validated against the
/// freshly generated move list before it is applied, so a stale
/// triangular row or a collided TT move truncates the reported line
/// instead of corrupting the position. A move that reaches a terminal
/// result is included (it is the terminal move) but the PV is never
/// extended past it: generate_all_moves_and_drops returns empty at a
/// terminal, so the next iteration finds no match and stops. TT probes
/// are also skipped once terminal to avoid extending past terminal state.
///
/// Params:
/// - state: &mut State      -> position walked and restored
/// - info : &mut SearchInfo -> worker holding the PV storage
/// - table: &TTable         -> the shared transposition table
/// - depth: usize           -> maximum PV length to reconstruct
#[macro_export]
macro_rules! fill_pv_line {
    ($state:expr, $info:expr, $table:expr, $depth:expr) => {{
        let pv_depth = $depth;
        let triangular_length = $info.pv_length[0].min(MAX_DEPTH);

        for index in 0..triangular_length {
            $info.pv_line[index] = $info.pv_table[index].clone();
        }

        let mut out: Vec<Move> = Vec::with_capacity(64);
        let mut scratch: Vec<u64> = Vec::with_capacity(16);
        let mut walk_complete = true;

        for slot in 0..triangular_length {
            let pv_move = $info.pv_line[slot].clone();

            generate_all_moves_and_drops(
                $state, &mut out, &mut scratch
            );

            if !out.iter().any(|mv| *mv == pv_move)
            || !make_move!($state, pv_move) {
                walk_complete = false;
                break;
            }

            if is_terminal!($state) {
                walk_complete = false;
                break;
            }
        }

        for slot in triangular_length..pv_depth {
            if !walk_complete {
                break;
            }

            if is_terminal!($state) {
                break;
            }

            let repeats = count_repetitions($state, SEARCH_REPETITION_CAP);
            let pv_key = search_key($state, repeats);

            let Some(pm) = probe_pv_move!(pv_key, $table) else {
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

            $info.pv_line[slot] = pv_move;

            if is_terminal!($state) {
                break;
            }
        }

        for index in ($state.search_ply as usize)..MAX_DEPTH {
            $info.pv_line[index] = null_move();
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
///
///   Field widths are proportional; every row represents 32 bits.
///
///   Bits 0..31:
///
///   0                               16  18                          31
/// ```text
///   ┌───────────────────────────────┬───┬────────────────────────────┐
///   │             score             │flg│           unused           │
///   └───────────────────────────────┴───┴────────────────────────────┘
/// ```
///
///   Bits 32..63:
///
///   32                                                              63
/// ```text
///   ┌────────────────────────────────────────────────────────────────┐
///   │                          signature →                           │
///   └────────────────────────────────────────────────────────────────┘
/// ```
///
///   Bits 64..95:
///
///   64                                                              95
/// ```text
///   ┌────────────────────────────────────────────────────────────────┐
///   │                          ← signature                           │
///   └────────────────────────────────────────────────────────────────┘
/// ```
///
///   Bits 96..127:
///
///   96                                                             127
/// ```text
///   ┌────────────────────────────────────────────────────────────────┐
///   │                             unused                             │
///   └────────────────────────────────────────────────────────────────┘
/// ```
///
///   - bits 0..15  : sign-extended score
///   - bits 16..17 : bound flags
///   - bits 18..31 : unused
///   - bits 32..95 : `MoveSignature`
///   - bits 96..127: unused
///
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
/// thread-safety invariants and age-based replacement policy apply.
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
        Self::with_mb(HASH_DEFAULT_MB / 3)
    }
}

impl QTable {
    /// QTable method cluster.
    ///
    /// Mirror of the `TTable` methods: `with_mb` sizes the table to a
    /// megabyte budget, `len` reports slot count, and `is_empty` scans
    /// for any written entry. Slot counts are floored to a power of two
    /// for mask indexing.
    ///
    /// with_mb
    ///
    ///   Params:
    ///   - mb: usize -> memory budget in megabytes
    ///
    ///   Return:
    ///   Self        -> zeroed table sized to the budget
    ///
    /// with_entries
    ///
    ///   Params:
    ///
    ///   - entries: usize
    ///     requested slot count, floored to a power of two
    ///
    ///   Return:
    ///   Self -> zeroed table with that many slots
    ///
    /// len
    ///
    ///   Return:
    ///   usize -> slot count
    ///
    /// is_empty
    ///
    ///   Return:
    ///   bool -> whether no slot has ever been written
    pub fn with_mb(mb: usize) -> Self {
        let size = (mb * 1024 * 1024 / size_of::<QTEntry>()).max(1);
        Self::with_entries(size)
    }

    pub fn with_entries(entries: usize) -> Self {
        Self {
            table: SyncUnsafeCell::new(
                vec![QTEntry::default(); 1 << entries.max(1).ilog2()]
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
/// entry: `qt_index!` masks a hash onto a power-of-two slot,
/// `qt_enc_score!` packs a sign-extended 16-bit score, `qt_enc_flags!`
/// packs the bound type into bits 16-17, and `qt_score!` / `qt_flags!`
/// read them back. Unlike the `tt_enc_*` writers these encoders return
/// their packed value instead of mutating in place.
///
/// qt_index!
///
///   Params:
///   - hash   : PositionHash -> Zobrist key of the probed position
///   - size   : usize        -> table slot count (power of two)
///
///   Return:
///   usize                   -> slot index, `hash & (size - 1)`
///
/// qt_enc_score!
///
///   Params:
///   - score  : i32 -> node score, truncated to i16
///
///   Return:
///   u32            -> score bit pattern in bits 0-15
///
/// qt_enc_flags!
///
///   Params:
///   - flags  : u8 -> bound type (FEXACT / FBETA)
///
///   Return:
///   u32           -> flag bits shifted into bits 16-17
///
/// qt_score!
///
///   Params:
///   - encoded: u32 -> packed score/flags word
///
///   Return:
///   i32            -> sign-extended stored score (bits 0-15)
///
/// qt_flags!
///
///   Params:
///   - encoded: u32 -> packed score/flags word
///
///   Return:
///   u8             -> bound flag (bits 16-17)
#[macro_export]
macro_rules! qt_index {
    ($hash:expr, $size:expr) => {{
        ($hash as usize) & ($size - 1)
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
/// - state : &State        -> position the stored mate scores are relative to
/// - key   : u128          -> qsearch key this node is filed under
/// - qtable: &QTable       -> the shared qsearch table
/// - alpha : i32           -> lower search bound at this node
/// - beta  : i32           -> upper search bound at this node
///
/// Return:
/// (bool, i32, PseudoMove) -> (cutoff valid, score, stored best move)
#[macro_export]
macro_rules! probe_qt_entry {
    ($state:expr, $key:expr, $qtable:expr, $alpha:expr, $beta:expr) => {
        hotpath::measure_block!("qt::probe", {
        let hash = $key;
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
/// - tt_move: &Move   -> best move found at this qsearch node
/// - score  : i32     -> score to store (mate scores are ply-adjusted)
/// - flags  : u8      -> bound type: FEXACT or FBETA
/// - state  : &State  -> position the stored mate scores are relative to
/// - key    : u128    -> qsearch key this node is filed under
/// - qtable : &QTable -> the shared qsearch table
#[macro_export]
macro_rules! hash_qt_entry {
    (
        $tt_move:expr,
        $score:expr,
        $flags:expr,
        $state:expr,
        $key:expr,
        $qtable:expr
    ) => {
        hotpath::measure_block!("qt::store", {
        let hash = $key;
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
