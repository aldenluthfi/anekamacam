//! # drop.rs
//!
//! Defines drop move encoding and helper macros for drop-related flags.
//!
//! Variants in the shogi family let captured pieces re-enter the board from
//! a player's hand. Those placements need their own compact representation:
//! unlike ordinary moves they have no origin square, but they do carry
//! placement constraints (such as checkmate-delivery bans) that must be
//! honored at generation time. This file defines the packed `DropMove`
//! word, the pattern-carrying `Drops` pairing used by the precomputed drop
//! tables, and the flag accessors shared by generation and execution.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 29/01/2026

use crate::*;

/*----------------------------------------------------------------------------*\
                          DROP REPRESENTATION ENCODING
\*----------------------------------------------------------------------------*/

/// Encoding helpers for drop-specific flags in packed moves.
///
/// These macros set bitfields in `Move.0` used by drop generation and move
/// execution paths.
#[macro_export]
macro_rules! enc_can_checkmate {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 23;
    };
}

/*----------------------------------------------------------------------------*\
                          DROP REPRESENTATION DECODING
\*----------------------------------------------------------------------------*/

/// Decoding helpers for drop-specific flags in packed moves.
///
/// These macros read the same drop-flag bits written by the encoder helpers
/// so drop legality and execution paths can branch on encoded options.
#[macro_export]
macro_rules! drop_can_checkmate {
    ($drop:expr) => {
        ($drop.0 >> 23) & 1 == 1
    };
}

/// DropMove / Drops / DropSet
///
/// A `DropMove` is a packed `u32` (bit 0 = LSB):
///
/// ┌───────┬────────┬───────────┐
/// │ 0..7  │ 8..19  │ 20..31    │
/// │ piece │ square │ modifiers │
/// └───────┴────────┴───────────┘
///
/// `piece` is the dropped piece index, `square` the target square index,
/// and `modifiers` the reserved drop-flag bits read by `drop_k!`.
///
/// `Drops` pairs a packed drop with the CPMN pattern that must match
/// around the target square for the drop to be legal, and `DropSet`
/// collects every such pairing for one (piece, square) table slot.
pub type DropMove = u32;
pub type Drops = (DropMove, Pattern);
pub type DropSet = Vec<Drops>;

/*----------------------------------------------------------------------------*\
                        DROP MODIFIER REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Accessors for modifier bits carried by [`DropMove`] entries.
///
/// These are consumed by drop generation and legality filtering while building
/// concrete drop moves.
#[macro_export]
macro_rules! drop_k {
    ($drop:expr) => {
        ($drop.0 >> 20) & 1 == 1
    };
}
