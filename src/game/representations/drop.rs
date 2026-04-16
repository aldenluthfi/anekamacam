//! # drop.rs
//!
//! Defines drop move encoding and helper macros for drop-related flags.
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

#[macro_export]
macro_rules! enc_from_enemy_hand {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 24;
    };
}

/*----------------------------------------------------------------------------*\
                          DROP REPRESENTATION ENCODING
\*----------------------------------------------------------------------------*/


/// Decoding helpers for drop-specific flags in packed moves.
///
/// These macros read the same drop-flag bits written by the encoder helpers
/// so drop legality and execution paths can branch on encoded options.
/// They mirror `enc_can_checkmate!` and `enc_from_enemy_hand!` semantics.
#[macro_export]
macro_rules! drop_can_checkmate {
    ($drop:expr) => {
        ($drop.0 >> 23) & 1 == 1
    };
}

#[macro_export]
macro_rules! drop_from_enemy_hand {
    ($drop:expr) => {
        ($drop.0 >> 24) & 1 == 1
    };
}

/// A `DropMove` consists of the following bits:
/// - The first 8 bits represent the piece index of the piece being dropped.
/// - The next 12 bits represent the square index where the piece is being
///   dropped.
/// - The next 12 bits are reserved for drop modifiers.
pub type DropMove = u32;
pub type Drops = (DropMove, PatternAllower, PatternStopper);
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

#[macro_export]
macro_rules! drop_f {
    ($drop:expr) => {
        ($drop.0 >> 21) & 1 == 1
    };
}

#[macro_export]
macro_rules! drop_d {
    ($drop:expr) => {
        ($drop.0 >> 22) & 1 == 1
    };
}

#[macro_export]
macro_rules! drop_e {
    ($drop:expr) => {
        ($drop.0 >> 23) & 1 == 1
    };
}


