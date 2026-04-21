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

pub type PVElement = (Move, PositionHash, u8);
pub type PVTable = Vec<PVElement>;

/// Maps a 128-bit position hash to the start index of a fixed-size PV bucket.
///
/// Algorithm notes:
/// - Applies xor-folding (`h ^= h >> 64`) so high and low halves influence each
///   other.
/// - Applies two multiplicative mix rounds with odd constants to spread nearby
///   keys more uniformly.
/// - Reduces to a bucket index (`bucket_count = table_size / bucket_size`) and
///   returns `bucket * bucket_size` so probing stays inside one small set.
///
/// This keeps the table contiguous and cache-friendly while reducing clustering
/// versus plain `hash % table_size`.
#[macro_export]
macro_rules! mixed_pv_index {
    ($hash:expr) => {{
        let mut mixed = $hash;
        mixed ^= mixed >> 64;
        mixed = mixed.wrapping_mul(0x9E3779B97F4A7C15u128);
        mixed ^= mixed >> 64;
        mixed = mixed.wrapping_mul(0xC2B2AE3D27D4EB4Fu128);
        mixed ^= mixed >> 64;

        let bucket_count = PV_TABLE_SIZE / PV_BUCKET_SIZE;
        let bucket = (mixed as usize) % bucket_count;

        bucket * PV_BUCKET_SIZE
    }};
}

/// Stores one PV move in a set-associative fixed-size table.
///
/// Replacement policy inside the bucket:
/// 1. Exact-hash hit  -> overwrite same slot.
/// 2. Empty slot      -> insert there.
/// 3. Otherwise       -> replace the lowest `age` entry.
///
/// `age` is currently `search_ply` (depth proxy), so deeper/current-line
/// entries are preferred to survive collisions.
#[macro_export]
macro_rules! hash_pv_move {
    ($pv_move:expr, $state:expr) => {{
        let hash = $state.position_hash;
        let bucket_base = mixed_pv_index!(hash);

        let mut replace_index = bucket_base;
        let mut lowest_age = u8::MAX;
        let mut target_index = None;

        for i in 0..PV_BUCKET_SIZE {
            let index = bucket_base + i;
            let (stored_move, stored_hash, age) = &$state.pv_table[index];

            if *stored_hash == hash {
                target_index = Some(index);
                break;
            }

            if *stored_hash == 0 || *stored_move == null_move() {
                target_index = Some(index);
                break;
            }

            if *age <= lowest_age {
                lowest_age = *age;
                replace_index = index;
            }
        }

        let target = target_index.unwrap_or(replace_index);
        $state.pv_table[target] = ($pv_move, hash, $state.search_ply as u8);
    }};
}

/// Probes a PV move by scanning the hashed bucket
/// (`PV_BUCKET_SIZE` slots).
///
/// This is O(bucket_size), which is constant and tiny, while providing much
/// lower effective collision loss than a single direct-mapped slot.
#[macro_export]
macro_rules! probe_pv_move {
    ($state:expr) => {{
        let hash = $state.position_hash;
        let bucket_base = mixed_pv_index!(hash);

        let mut result = None;

        for i in 0..PV_BUCKET_SIZE {
            let index = bucket_base + i;
            let (pv_move, stored_hash, _) = &$state.pv_table[index];

            if *stored_hash == hash {
                result = Some(pv_move.clone());
                break;
            }
        }

        result
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

