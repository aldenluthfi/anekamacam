//! transposition.rs
//!
//! Transposition tables that keep search results for later use.
//!
//! The Zobrist key is the table index. A hit with enough depth gives the
//! stored score, and the search skips the subtree. An entry has a bound
//! type, a depth, the best move, the static evaluation and an age. A
//! seqlock and a parity word over the three `u128` slots give thread safety.
//!
//! Created: 29/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                        SHARED HASH TABLE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// HashEntry
///
/// One slot of a shared search table. `age` is the search generation and
/// is not in the parity. `version` is the seqlock counter, odd during a
/// write.
///
/// - slot[0] : move.0, raw 128 bits
/// - slot[1] : packed data, the table macros give the layout
/// - slot[2] : slot[0] ^ slot[1] ^ hash, the parity, written last
///
/// ```text
/// write : version++ → slot[0] → slot[1] → slot[2] → age → version++
/// read  : slot[0] ^ slot[1] ^ slot[2] == hash, and version unchanged
/// ```
///
#[derive(Default)]
pub struct HashEntry {
    pub slot: [u128; 3],                                                        /* [key, data1, data2]                */
    pub age: u64,                                                               /* search age for replacement policy  */
    pub version: AtomicU64,                                                     /* seqlock: odd = writing, even = ok  */
}

/// Clone for HashEntry
///
/// A manual clone, because an atomic is not `Clone`. The copy reads the
/// counter relaxed and puts it in a new atomic.
///
/// Return:
/// Self -> copy with the same seqlock counter
///
/// Notes:
/// Only the table allocation `vec![HashEntry::default(); n]` uses it. A
/// clone from a live table would race the seqlock.
///
impl Clone for HashEntry {
    fn clone(&self) -> Self {
        HashEntry {
            slot: self.slot,
            age: self.age,
            version: AtomicU64::new(self.version.load(Ordering::Relaxed)),
        }
    }
}

/// HashTable
///
/// Shared search table without locks. A reader compares the version before
/// and after the slot read. The XOR parity finds mixed entries. The age
/// increases with each search, for replacement.
///
/// `NUM / DEN` is the share of the `Hash` option. The two tables differ
/// only in their slot packing:
///
/// - `TTable` : main table, two thirds of `Hash`
/// - `QTable` : quiescence table, one third of `Hash`
///
pub struct HashTable<const NUM: usize, const DEN: usize> {
    pub table: SyncUnsafeCell<Vec<HashEntry>>,                                  /* shared mutable access              */
    pub age: AtomicU64,                                                         /* search age; bump per search        */
    pub new_write: AtomicU64,                                                   /* writes to empty slots              */
    pub over_write: AtomicU64,                                                  /* writes replacing existing entries  */
    pub hit: AtomicU64,                                                         /* probes where hash matched          */
    pub valid: AtomicU64,                                                       /* probes where XOR decode succeeded  */
}

pub type TTable = HashTable<2, 3>;
pub type QTable = HashTable<1, 3>;

unsafe impl<const NUM: usize, const DEN: usize> Sync for HashTable<NUM, DEN> {}
unsafe impl<const NUM: usize, const DEN: usize> Send for HashTable<NUM, DEN> {}

/// Default for HashTable
///
/// Makes a table with the `NUM / DEN` share of `HASH_DEFAULT_MB`. When the
/// GUI sets `Hash`, the session makes new tables.
///
/// Return:
/// Self -> zeroed table of the default size
///
impl<const NUM: usize, const DEN: usize> Default for HashTable<NUM, DEN> {
    fn default() -> Self {
        Self::with_mb(HASH_DEFAULT_MB * NUM / DEN)
    }
}

impl<const NUM: usize, const DEN: usize> HashTable<NUM, DEN> {
    /// HashTable methods
    ///
    /// Make a table for a memory size, or give its slot count. The slot
    /// count rounds down to a power of two, so the index is a mask.
    ///
    /// with_mb
    ///
    ///   Params:
    ///   - mb: usize -> memory size in megabytes
    ///
    ///   Return:
    ///   Self        -> zeroed table of that size
    ///
    /// len
    ///
    ///   Return:
    ///   usize       -> slot count
    ///
    pub fn with_mb(mb: usize) -> Self {
        let entries = (mb * 1024 * 1024 / size_of::<HashEntry>()).max(1);

        Self {
            table: SyncUnsafeCell::new(
                vec![HashEntry::default(); 1 << entries.ilog2()]
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
}

/*----------------------------------------------------------------------------*\
                        SHARED HASH TABLE PROBE / STORE
\*----------------------------------------------------------------------------*/

/// Shared slot access macros
///
/// Slot access for the two tables. The steps before and after the packed
/// data are the same for the two tables.
///
/// table_index!
///
///   Params:
///   - hash: PositionHash -> Zobrist key of the probed position
///   - size: usize        -> table slot count (power of two)
///
///   Return:
///   usize                -> slot index, `hash & (size - 1)`
///
/// probe_hash_slot!
///
///   Finds the slot, rejects a slot during a write, tests the parity and
///   tests that the version did not change. It increments `hit` and
///   `valid`.
///
///   Params:
///   - table    : &HashTable -> table to probe
///   - hash     : u128       -> search key of the node
///   - miss     : expr       -> result of each rejection
///   - move_slot: ident      -> name of slot[0] in the body
///   - data_slot: ident      -> name of slot[1] in the body
///   - body     : block      -> reads the two words, gives the result
///
///   Return:
///   the value of the body, or `miss`
///
/// commit_hash_entry!
///
///   Writes the slot under the seqlock, the parity word last before `age`.
///   It increments the new or the replace counter.
///
///   Params:
///   - table    : &HashTable     -> table with the counters
///   - entry    : &mut HashEntry -> slot to write
///   - hash     : u128           -> key for the parity word
///   - empty    : bool           -> true when the slot was never written
///   - move_slot: u128           -> slot[0]
///   - data_slot: u128           -> slot[1]
///   - age      : u64            -> generation of the slot
///
#[macro_export]
macro_rules! table_index {
    ($hash:expr, $size:expr) => {{
        ($hash as usize) & ($size - 1)
    }};
}

#[macro_export]
macro_rules! probe_hash_slot {
    (
        $table:expr,
        $hash:expr,
        $miss:expr,
        |$move_slot:ident, $data_slot:ident| $body:block
    ) => {{
        let hash = $hash;
        let index = table_index!(hash, $table.len());
        let entry = &mut unsafe { &mut *($table.table.get()) }[index];

        let first_version = entry.version.load(Ordering::Acquire);

        if first_version & 1 != 0 {                                             /* write in progress: skip            */
            $miss
        } else {
            let $move_slot = entry.slot[0];
            let $data_slot = entry.slot[1];
            let parity_slot = entry.slot[2];

            if $move_slot ^ $data_slot ^ parity_slot != hash {                  /* parity check: all slots covered    */
                $miss
            } else {
                $table.hit.fetch_add(1, Ordering::Relaxed);                     /* parity matched                     */
                let second_version = entry.version.load(Ordering::Acquire);

                if first_version != second_version {                            /* seqlock: torn read detected        */
                    $miss
                } else {
                    $table.valid.fetch_add(1, Ordering::Relaxed);               /* consistent read confirmed          */
                    $body
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! commit_hash_entry {
    (
        $table:expr,
        $entry:expr,
        $hash:expr,
        $empty:expr,
        $move_slot:expr,
        $data_slot:expr,
        $age:expr
    ) => {{
        let move_slot = $move_slot;
        let data_slot = $data_slot;

        if $empty {
            $table.new_write.fetch_add(1, Ordering::Relaxed);
        } else {
            $table.over_write.fetch_add(1, Ordering::Relaxed);
        }

        $entry.version.fetch_add(1, Ordering::Release);
        $entry.slot[0] = move_slot;
        $entry.slot[1] = data_slot;
        $entry.slot[2] = move_slot ^ data_slot ^ $hash;
        $entry.age = $age;
        $entry.version.fetch_add(1, Ordering::Release);
    }};
}

/*----------------------------------------------------------------------------*\
                      TRANSPOSITION TABLE PACKING HELPERS
\*----------------------------------------------------------------------------*/

/// Main table packing macros
///
/// Write and read the `slot[1]` fields of the main table. Each row has 32
/// bits, and the field widths are to scale.
///
/// ```text
///   Bits 0..31:
///
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
///   │   ← signature   │              static evaluation               │
///   └─────────────────┴──────────────────────────────────────────────┘
/// ```
///
/// - flag        : bound type, FEXACT, FALPHA or FBETA
/// - depth       : clamped search depth
/// - score       : node score with ply correction, 32 bits
/// - signature   : MoveSignature of the stored best move
/// - static eval : signed 23-bit raw evaluation, `EVAL_NONE` in check
///
/// The writers OR into place and return nothing:
///
/// tt_enc_flags!
///
///   Params:
///   - encoded: &mut u32 -> flag and depth word to build
///   - val    : u8       -> bound flag, masked into bits 0-1
///
/// tt_enc_depth!
///
///   Params:
///   - encoded: &mut u32 -> flag and depth word to build
///   - val    : usize    -> depth, clamped and masked into bits 2-8
///
/// tt_enc_score!
///
///   Params:
///   - encoded: &mut u128 -> slot[1] word to build
///   - val    : i32       -> score, masked into bits 9-40
///
/// The readers give one field. Flag and depth come from the low word, the
/// score from the full slot[1]:
///
/// tt_flags!
///
///   Params:
///   - encoded: u32 -> flag and depth word to read
///
///   Return:
///   u8             -> bound flag in bits 0-1
///
/// tt_depth!
///
///   Params:
///   - encoded: u32 -> flag and depth word to read
///
///   Return:
///   usize          -> clamped depth in bits 2-8
///
/// tt_score!
///
///   Params:
///   - b_prime: u128 -> slot[1] word to read
///
///   Return:
///   i32             -> score in bits 9-40
///
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

/// probe_tt_bound!
///
/// Probes the main table as `probe_tt_entry!` does, but gives the stored
/// score, bound and depth as they are, for a test that needs the bound of
/// the table move: the singular test of `alpha_beta`.
///
/// Params:
/// - state: &State  -> position for the ply correction of mate scores
/// - key  : u128    -> search key of the node
/// - table: &TTable -> shared transposition table
///
/// Return:
/// Option<(i32, u8, usize)> -> score, bound flag and depth, or none
///
#[macro_export]
macro_rules! probe_tt_bound {
    ($state:expr, $key:expr, $table:expr) => {
        probe_hash_slot!($table, $key, None, |_move_slot, data_slot| {
            let encoded = (data_slot & 0x1FF) as u32;
            let mut score = tt_score!(data_slot);

            if score > MATE_SCORE {
                score -= $state.search_ply as i32;
            } else if score < -MATE_SCORE {
                score += $state.search_ply as i32;
            }

            Some((score, tt_flags!(encoded), tt_depth!(encoded)))
        })
    };
}

/// probe_tt_entry!
///
/// Probes the main table with the parity and seqlock tests. The stored
/// depth and bound decide if the score can cut. Each valid hit also gives
/// the move, the raw static evaluation and the evaluation refined by the
/// bound. A mate score does not refine the evaluation.
///
/// Params:
///
///     state: &State
///     position for the ply correction of mate scores
///
///     key: u128
///     search key of the node
///
///     table: &TTable
///     shared transposition table
///
///     alpha: i32
///     lower search bound
///
///     beta: i32
///     upper search bound
///
///     depth: usize
///     minimum stored depth for a cutoff
///
/// Return:
///
///     (bool, i32, PseudoMove, i32, i32)
///     cutoff, score, move, raw evaluation and refined evaluation
///
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
        probe_hash_slot!(
            $table,
            $key,
            (false, i32::MIN, null_pseudo_move(), EVAL_NONE, EVAL_NONE),
            |move_slot, data_slot| {
                let encoded = (data_slot & 0x1FF) as u32;
                let signature = (data_slot >> 41) as u64;
                let pseudo_move = (move_slot, signature);
                let entry_depth = tt_depth!(encoded);
                let entry_flags = tt_flags!(encoded);
                let mut entry_score = tt_score!(data_slot);
                let entry_eval = (
                    (((data_slot >> 105) as u32) << 9) as i32
                ) >> 9;

                if entry_score > MATE_SCORE {
                    entry_score -= $state.search_ply as i32;
                } else if entry_score < -MATE_SCORE {
                    entry_score += $state.search_ply as i32;
                }

                let mut bound_eval = entry_eval;

                if entry_eval != EVAL_NONE
                && entry_score.abs() < MATE_SCORE
                {
                    bound_eval = match entry_flags {
                        FALPHA => entry_eval.min(entry_score),
                        FBETA => entry_eval.max(entry_score),
                        FEXACT => entry_score,
                        _ => unreachable!(),
                    };
                }

                if entry_depth < $depth {
                    (
                        false, i32::MIN, pseudo_move, entry_eval, bound_eval
                    )
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

                    (
                        valid_cutoff, cutoff_score, pseudo_move,
                        entry_eval, bound_eval,
                    )
                }
            }
        )
        })
    };
}

/// probe_pv_move!
///
/// A small probe that extends the printed principal variation. It does the
/// same tests as `probe_tt_entry!`, but ignores depth and bounds and gives
/// only the stored move.
///
/// Params:
/// - key  : u128      -> search key of the node
/// - table: &TTable   -> shared transposition table
///
/// Return:
/// Option<PseudoMove> -> the stored move, or None on a miss
///
#[macro_export]
macro_rules! probe_pv_move {
    ($key:expr, $table:expr) => {{
        probe_hash_slot!($table, $key, None, |move_slot, data_slot| {
            let signature = (data_slot >> 41) as u64;                           /* bits 41-104 = MoveSignature        */
            let pseudo_move: PseudoMove = (move_slot, signature);

            if pseudo_move == null_pseudo_move() {
                None
            } else {
                Some(pseudo_move)
            }
        })
    }};
}

/// hash_tt_entry!
///
/// Stores one main search result with the seqlock and the parity. A mate
/// score is stored as the distance from this node, `search_ply` added. A
/// later probe subtracts its own ply.
///
/// The macro writes only if one of these is true:
///
/// - the slot is empty
/// - the slot has another position
/// - the slot is from an older search
/// - the new depth is equal or greater
/// - the new score is not lower
/// - the new bound is the first exact bound
///
/// Params:
/// - tt_move: &Move   -> best move of this node
/// - score  : i32     -> score to store
/// - flags  : u8      -> FEXACT, FALPHA or FBETA
/// - depth  : usize   -> search depth of the score
/// - eval   : i32     -> raw static evaluation, `EVAL_NONE` in check
/// - state  : &State  -> position for the ply correction of mate scores
/// - key    : u128    -> search key of the node
/// - table  : &TTable -> shared transposition table
///
#[macro_export]
macro_rules! hash_tt_entry {
    (
        $tt_move:expr,
        $score:expr,
        $flags:expr,
        $depth:expr,
        $eval:expr,
        $state:expr,
        $key:expr,
        $table:expr
    ) => {
        hotpath::measure_block!("tt::store", {
        let hash = $key;
        let index = table_index!(hash, $table.len());
        let table_vec: &mut Vec<HashEntry> =
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
        encoded |= (($eval as u32 as u128) & 0x7F_FFFF) << 105;

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
            commit_hash_entry!(
                $table, entry, hash, empty, move_slot, data_slot, age
            );
        }
        })
    };
}

/// fill_pv_line!
///
/// Makes the full principal variation for output. It copies the PV table
/// into `pv_line` and plays the line on the board. Then it extends the line
/// with table probes until a miss, an illegal move or the target depth. At
/// the end it undoes all moves.
///
/// `pv_table` is a flat `PV_STRIDE * PV_STRIDE` upper triangle. Row `ply`
/// starts at `ply * PV_STRIDE`. When a move improves alpha at `ply`, the
/// search writes it at `[ply][ply]` and copies the child row up. Thus row 0
/// has the full line. `pv_length[ply]` is the end column of row `ply`:
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
/// Params:
/// - state: &mut State      -> position to walk and restore
/// - info : &mut SearchInfo -> worker with the PV storage
/// - table: &TTable         -> shared transposition table
/// - depth: usize           -> maximum PV length
///
/// Notes:
/// The macro tests each move against the generated move list before it
/// plays it. Thus an old row or a hash collision only shortens the line.
/// The line includes a move that ends the game, but stops after it.
///
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
                       PAWN TABLE REPRESENTATION & PROBE
\*----------------------------------------------------------------------------*/

/// PTEntry
///
/// One cached pawn structure result: the pawn key and the opening and
/// endgame values. The result depends only on the pawns, so there is no
/// bound, depth or move, and an entry is always correct.
///
/// An empty slot has key zero, so there is no empty flag.
///
#[derive(Clone, Default)]
pub struct PTEntry {
    pub key: u128,                                                              /* Zobrist fold of the pawn roster    */
    pub opening: i32,                                                           /* cached opening worth               */
    pub endgame: i32,                                                           /* cached endgame worth               */
}

/// PTable
///
/// The private pawn structure cache of one [`State`]. It is not shared,
/// so it has no seqlock, parity or atomics. Each thread fills its own copy.
///
/// A write always replaces the slot. All entries are correct, and the most
/// recent pawn structure is the most likely to come again.
///
#[derive(Clone)]
pub struct PTable {
    pub table: Vec<PTEntry>,                                                    /* slot count is a power of two       */
}

/// Default for PTable
///
/// Makes a cache with `with_hash_mb(HASH_DEFAULT_MB)`. The `Hash` value
/// only scales `PAWN_TABLE_ENTRIES`. The pawn cache does not take a share
/// from the shared tables.
///
/// Return:
/// Self -> zeroed cache with the default entry count
///
impl Default for PTable {
    fn default() -> Self {
        Self::with_hash_mb(HASH_DEFAULT_MB)
    }
}

impl PTable {
    /// PTable methods
    ///
    /// Make a pawn cache or give its size. The slot count rounds down to a
    /// power of two, so the index is a mask.
    ///
    /// with_hash_mb
    ///
    ///   Params:
    ///   - hash_mb: usize -> the `Hash` option value
    ///
    ///   Return:
    ///   Self             -> zeroed table, entries scaled by `Hash`
    ///
    /// with_entries
    ///
    ///   Params:
    ///   - entries: usize -> requested slot count
    ///
    ///   Return:
    ///   Self             -> zeroed table with that slot count
    ///
    /// len
    ///
    ///   Return:
    ///   usize            -> slot count
    ///
    pub fn with_hash_mb(hash_mb: usize) -> Self {
        let entries = hash_mb.saturating_mul(PAWN_TABLE_ENTRIES)
            / HASH_DEFAULT_MB;

        Self::with_entries(entries.max(1))
    }

    pub fn with_entries(entries: usize) -> Self {
        Self {
            table: vec![PTEntry::default(); 1 << entries.max(1).ilog2()],
        }
    }

    pub fn len(&self) -> usize {
        self.table.len()
    }
}

/*----------------------------------------------------------------------------*\
                     QSEARCH TT PACKING / UNPACKING MACROS
\*----------------------------------------------------------------------------*/

/// Quiescence table packing macros
///
/// Write and read the `slot[1]` fields of the quiescence table. Unlike
/// `tt_enc_*`, the writers return the packed value. `slot[0]` has the raw
/// `move.0`. Each row has 32 bits, and the field widths are to scale.
///
/// ```text
///   Bits 0..31:
///
///   0                               16  18                          31
///   ┌───────────────────────────────┬───┬────────────────────────────┐
///   │             score             │flg│           unused           │
///   └───────────────────────────────┴───┴────────────────────────────┘
///
///   Bits 32..63:
///
///   32                                                              63
///   ┌────────────────────────────────────────────────────────────────┐
///   │                          signature →                           │
///   └────────────────────────────────────────────────────────────────┘
///
///   Bits 64..95:
///
///   64                                                              95
///   ┌────────────────────────────────────────────────────────────────┐
///   │                          ← signature                           │
///   └────────────────────────────────────────────────────────────────┘
///
///   Bits 96..127:
///
///   96                                                             127
///   ┌────────────────────────────────────────────────────────────────┐
///   │                             unused                             │
///   └────────────────────────────────────────────────────────────────┘
/// ```
///
/// - bits 0..15   : sign-extended score
/// - bits 16..17  : bound flag
/// - bits 18..31  : unused
/// - bits 32..95  : `MoveSignature`
/// - bits 96..127 : unused
///
/// qt_enc_score!
///
///   Params:
///   - score  : i32 -> node score, truncated to i16
///
///   Return:
///   u32            -> score bits in bits 0-15
///
/// qt_enc_flags!
///
///   Params:
///   - flags  : u8  -> bound type, FEXACT or FBETA
///
///   Return:
///   u32            -> flag bits in bits 16-17
///
/// qt_score!
///
///   Params:
///   - encoded: u32 -> packed score and flag word
///
///   Return:
///   i32            -> sign-extended stored score (bits 0-15)
///
/// qt_flags!
///
///   Params:
///   - encoded: u32 -> packed score and flag word
///
///   Return:
///   u8             -> bound flag (bits 16-17)
///
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
/// Probes the quiescence table with the parity and seqlock tests of
/// `probe_hash_slot!`. An entry without a move never cuts.
///
/// - FBETA  : cuts when the score is at or above beta
/// - FEXACT : always cuts
///
/// Params:
/// - state : &State        -> position for the ply correction of mates
/// - key   : u128          -> quiescence key of the node
/// - qtable: &QTable       -> shared quiescence table
/// - alpha : i32           -> lower search bound
/// - beta  : i32           -> upper search bound
///
/// Return:
/// (bool, i32, PseudoMove) -> (cutoff, score, stored best move)
///
#[macro_export]
macro_rules! probe_qt_entry {
    ($state:expr, $key:expr, $qtable:expr, $alpha:expr, $beta:expr) => {
        hotpath::measure_block!("qt::probe", {
        probe_hash_slot!(
            $qtable,
            $key,
            (false, i32::MIN, null_pseudo_move()),
            |move_slot, data_slot| {
                let encoded = (data_slot & 0xFFFF_FFFF) as u32;                 /* bits  0–31 = score+flags           */
                let signature = (data_slot >> 32) as u64;                       /* bits 32-95 = MoveSignature         */
                let pseudo_move: PseudoMove = (move_slot, signature);

                let entry_flags = qt_flags!(encoded);
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

                if pseudo_move == null_pseudo_move() {
                    valid_cutoff = false;
                }

                (valid_cutoff, entry_score, pseudo_move)
            }
        )
        })
    };
}

/// hash_qt_entry!
///
/// Stores one quiescence result. The macro writes only if one of these is
/// true:
///
/// - the slot is empty
/// - the slot has another position
/// - the slot is from an older search
/// - the new score is not lower
/// - the new bound is the first exact bound
///
/// Params:
/// - tt_move: &Move   -> best move of this node
/// - score  : i32     -> score to store, mates with ply correction
/// - flags  : u8      -> bound type, FEXACT or FBETA
/// - state  : &State  -> position for the ply correction of mates
/// - key    : u128    -> quiescence key of the node
/// - qtable : &QTable -> shared quiescence table
///
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
        let index = table_index!(hash, $qtable.len());
        let table_vec: &mut Vec<HashEntry> =
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
            commit_hash_entry!($qtable, entry, hash, empty, a, b, age);
        }
        })
    };
}
