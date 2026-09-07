//! transposition.rs
//!
//! Transposition table for caching and reusing search results across the tree.
//!
//! Positions are keyed by Zobrist hash. A cache hit at sufficient depth returns
//! the stored score directly, skipping the subtree. Entries record a bound
//! type (exact, alpha, beta), depth, best move, static evaluation, and age for
//! replacement policy. Thread safety uses a seqlock with parity across the
//! 3×u128 slot layout.
//!
//! Created: 29/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                        SHARED HASH TABLE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// HashEntry
///
/// One slot of a shared search table. Three `u128` words carry the
/// payload, the last being the XOR parity of the other two against the
/// position hash; `age` records the search generation and sits outside
/// the parity; `version` is the seqlock counter, odd while a write is in
/// flight.
///
/// Layout:
/// - slot[0] = move.0 (128-bit, raw)
/// - slot[1] = packed payload — the packing is the table's, not the
///   slot's, and is drawn on each table's packing cluster
/// - slot[2] = slot[0] ^ slot[1] ^ hash (parity, written last)
///
/// Write order: version++ → slot[0] → slot[1] → slot[2] → age → version++
///
/// Validation:
///   (1) slot[0] ^ slot[1] ^ slot[2] == position_hash  →  parity intact
///   (2) version unchanged across read                 →  no torn write
#[derive(Default)]
pub struct HashEntry {
    pub slot: [u128; 3],                                                        /* [key, data1, data2]                */
    pub age: u64,                                                               /* search age for replacement policy  */
    pub version: AtomicU64,                                                     /* seqlock: odd = writing, even = ok  */
}

/// Clone for HashEntry
///
/// Written by hand because `version` is an atomic, and an atomic is not
/// `Clone`. The counter is read relaxed and handed to a fresh atomic, so
/// the copy starts life with whatever parity the original had rather than
/// at zero.
///
/// The only caller is the `vec![HashEntry::default(); n]` that allocates a
/// table, where the source is a default entry and no thread is yet reading
/// it; cloning a slot out of a live table would race the seqlock and is
/// never done.
///
/// Return:
/// Self -> copy, its seqlock counter snapshotted relaxed
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
/// Shared search table using seqlock+parity for lock-free thread safety.
/// Readers check version parity before and after the slot load and retry
/// on mismatch; the XOR parity across `slot[0..2]` catches cross-entry
/// corruption. Age is bumped each search for replacement.
///
/// `NUM / DEN` is the table's share of the `Hash` option, and is the only
/// thing that separates the main table from the quiescence one — the two
/// differ in what they pack into a slot, never in how slots are stored —
/// so both are aliases of this type and no call site names it: `TTable`
/// takes two thirds of the option and `QTable` the third that is left.
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
/// Sizes a table nobody asked a size for: the compiled-in `Hash` default,
/// cut to this table's `NUM / DEN` share of it. A session that later sets
/// `Hash` throws these away and rebuilds at the size it was given, so this
/// only ever covers the window before a GUI speaks.
///
/// Return:
/// Self -> zeroed table at this table's share of the default budget
impl<const NUM: usize, const DEN: usize> Default for HashTable<NUM, DEN> {
    fn default() -> Self {
        Self::with_mb(HASH_DEFAULT_MB * NUM / DEN)
    }
}

impl<const NUM: usize, const DEN: usize> HashTable<NUM, DEN> {
    /// HashTable method cluster.
    ///
    /// `with_mb` sizes the table to a memory budget in megabytes — this
    /// table's `NUM / DEN` share of the UCI Hash option — flooring the
    /// slot count to a power of two so the index macro can mask instead
    /// of taking a modulo; `len` reports the count it masks against.
    ///
    /// with_mb
    ///
    ///   Params:
    ///   - mb: usize -> memory budget in megabytes
    ///
    ///   Return:
    ///   Self        -> zeroed table sized to the budget
    ///
    /// len
    ///
    ///   Return:
    ///   usize -> slot count
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

/// Shared slot access macros.
///
/// Everything a probe or a store does before and after it looks at the
/// packed payload is the same for both tables, so it lives here once.
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
///   Locates the slot, rejects a write in flight, checks the XOR parity
///   against the key, and confirms the seqlock did not move across the
///   read, bumping `hit` and `valid` as it goes. The caller names the two
///   payload words it wants bound and supplies the value every rejecting
///   path yields.
///
///   Params:
///   - table    : &HashTable -> table probed, whose counters are bumped
///   - hash     : u128       -> search key this node is filed under
///   - miss     : expr       -> value yielded by every rejecting path
///   - move_slot: ident      -> name bound to slot[0] inside the body
///   - data_slot: ident      -> name bound to slot[1] inside the body
///   - body     : block      -> reads the two words, yields the result
///
///   Return:
///   the body's value, or `miss`
///
/// commit_hash_entry!
///
///   Writes the slot under the seqlock, parity word last before `age`,
///   and bumps whichever replacement counter applies.
///
///   Params:
///   - table    : &HashTable     -> table whose counters are bumped
///   - entry    : &mut HashEntry -> slot being replaced
///   - hash     : u128           -> key the parity word is folded against
///   - empty    : bool           -> whether the slot was never written
///   - move_slot: u128           -> slot[0]
///   - data_slot: u128           -> slot[1]
///   - age      : u64            -> generation stamped on the slot
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

/// Main-table packing macros.
///
/// The writers pack bound flags (bits 0-1), clamped depth (bits 2-8), score
/// (bits 9-40), move signature (bits 41-104), and signed static evaluation
/// (bits 105-127) into `slot[1]`; the readers extract those fields again.
/// Field widths below are proportional; every row is 32 bits.
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
///   - flag         : bound type (FEXACT / FALPHA / FBETA)
///   - depth        : clamped search depth
///   - score        : ply-adjusted node score (32-bit)
///   - signature    : MoveSignature of the stored best move
///   - static eval  : signed 23-bit raw evaluation, `EVAL_NONE` in check
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
/// The readers take a word back apart and return the one field, so the
/// flags/depth pair reads out of the low word the writers built and the
/// score out of slot[1] whole:
///
/// tt_flags!
///
///   Params:
///   - encoded: u32 -> flags/depth word being read
///
///   Return:
///   u8 -> bound flag held in bits 0-1
///
/// tt_depth!
///
///   Params:
///   - encoded: u32 -> flags/depth word being read
///
///   Return:
///   usize -> clamped depth held in bits 2-8
///
/// tt_score!
///
///   Params:
///   - b_prime: u128 -> slot[1] word being read
///
///   Return:
///   i32 -> score held in bits 9-40
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

/// probe_tt_entry!
///
/// Probes the main table with parity and seqlock validation. Stored depth and
/// bound flags decide whether the score can cut; every valid hash match still
/// returns its move, raw static evaluation, and evaluation sharpened by the
/// stored bound. Mate-range scores never sharpen an evaluation.
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
/// (bool, i32, PseudoMove, i32, i32) -> cutoff, score, move, raw evaluation,
///                                      and bound-refined evaluation
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
/// Stores one main-search result with seqlock write protection and parity.
///
/// Params:
/// - tt_move: &Move   -> best move found at this node
/// - score  : i32     -> score to store
/// - flags  : u8      -> FEXACT, FALPHA, or FBETA
/// - depth  : usize   -> search depth the score is valid for
/// - eval   : i32     -> raw static evaluation, `EVAL_NONE` in check
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
                       PAWN TABLE REPRESENTATION & PROBE
\*----------------------------------------------------------------------------*/

/// PTEntry
///
/// One cached pawn-structure verdict: the key the roster hashed to and the
/// opening and endgame worth that roster is due. There is no bound, no
/// depth, and no move, because the answer is a pure function of where the
/// pawns stand — two positions sharing a pawn roster share this score
/// whatever else differs about them, and a stored entry never goes stale.
///
/// An untouched slot has a zero key, which no real roster can collide with
/// short of a 128-bit accident, so emptiness needs no separate flag.
#[derive(Clone, Default)]
pub struct PTEntry {
    pub key: u128,                                                              /* Zobrist fold of the pawn roster    */
    pub opening: i32,                                                           /* cached opening worth               */
    pub endgame: i32,                                                           /* cached endgame worth               */
}

/// PTable
///
/// One position's private pawn-structure cache, held on its [`State`].
/// Unlike [`TTable`] and [`QTable`] this one is never shared, so it carries
/// no seqlock, no parity word, and no atomics: whoever owns the state it
/// hangs off both writes it and reads it, and a thread searching a copy is
/// filling a copy.
/// A shared table would have to protect a 24-byte payload with the same
/// machinery that protects a 48-byte one, and pay it on a term evaluated at
/// nearly every node.
///
/// Replacement is unconditional. Every entry is equally true, so the only
/// thing a policy could preserve is the entry more likely to be asked for
/// again, and the most recent roster is exactly that during a search that
/// moves one pawn at a time.
#[derive(Clone)]
pub struct PTable {
    pub table: Vec<PTEntry>,                                                    /* slot count is a power of two       */
}

/// Default for PTable
///
/// Hands the whole default `Hash` budget to `with_hash_mb`, which reads it
/// as a scale rather than an allocation: this cache is sized in entries,
/// and the budget only says how far to scale `PAWN_TABLE_ENTRIES` from it.
/// So the pawn table takes no share away from the shared tables — it is a
/// per-state cache, and every state carries its own.
///
/// Return:
/// Self -> zeroed cache at the default entry count
impl Default for PTable {
    fn default() -> Self {
        Self::with_hash_mb(HASH_DEFAULT_MB)
    }
}

impl PTable {
    /// PTable method cluster.
    ///
    /// `with_hash_mb` scales the default entry count with the UCI Hash budget.
    /// `with_entries` builds a zeroed table whose slot count is floored to a
    /// power of two for mask indexing, and `len` reports it.
    ///
    /// with_hash_mb
    ///
    ///   Params:
    ///   - hash_mb: usize -> configured UCI Hash budget
    ///
    ///   Return:
    ///   Self             -> zeroed table scaled from the default budget
    ///
    /// with_entries
    ///
    ///   Params:
    ///   - entries: usize -> requested slot count
    ///
    ///   Return:
    ///   Self             -> zeroed table with that many slots
    ///
    /// len
    ///
    ///   Return:
    ///   usize -> slot count
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

/// Qsearch-table packing macros.
///
/// Counterparts of the `tt_*` packing family for the smaller qsearch
/// payload: `qt_enc_score!` packs a sign-extended 16-bit score,
/// `qt_enc_flags!` packs the bound type into bits 16-17, and `qt_score!`
/// / `qt_flags!` read them back. Unlike the `tt_enc_*` writers these
/// encoders return their packed value instead of mutating in place.
///
/// `slot[0]` holds `move.0` raw and `slot[1]` holds `sig << 32 | encoded`.
/// Field widths below are proportional; every row is 32 bits.
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
