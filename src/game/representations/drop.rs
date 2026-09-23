//! drop.rs
//!
//! Defines the drop move encoding and the macros for drop flags.
//!
//! In some variants a player can put a captured piece back on the board.
//! A drop has no origin square, but it can have rules, for example a ban
//! on checkmate. This file defines the packed drop word, its pattern pair
//! and the flag macros for generation and execution.
//!
//! Created: 29/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                          DROP REPRESENTATION ENCODING
\*----------------------------------------------------------------------------*/

/// enc_can_checkmate!
///
/// Writes the checkmate flag of a drop move. The drop table keeps the ban
/// from the config. The move generator writes the opposite value.
///
/// - [`DropMove`] bit 20 : the drop must not give checkmate
/// - [`Move`] bit 112    : the drop can give checkmate
///
/// Params:
/// - mv : &mut Move -> drop move to write
/// - val: u128      -> checkmate flag, masked into bit 112
///
#[macro_export]
macro_rules! enc_can_checkmate {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 112;
    };
}

/*----------------------------------------------------------------------------*\
                          DROP REPRESENTATION DECODING
\*----------------------------------------------------------------------------*/

/// drop_can_checkmate!
///
/// Reads the checkmate flag that `enc_can_checkmate!` writes. The search
/// uses it to decide if a drop that gives mate is legal.
///
/// Params:
/// - drop: &Move -> drop move to read
///
/// Return:
/// bool          -> true when the drop can give checkmate (bit 112)
///
#[macro_export]
macro_rules! drop_can_checkmate {
    ($drop:expr) => {
        ($drop.0 >> 112) & 1 == 1
    };
}

/// illegal_mating_drop!
///
/// Tells if the last move was a drop that must not give mate. If so, and
/// the side to move has no legal move, the player who dropped loses. The
/// search and `adjudicate_no_move` both use this macro, so they agree.
///
/// Params:
/// - state: &State -> position with the last move to examine
///
/// Return:
/// bool            -> true when the mating move was a banned drop
///
#[macro_export]
macro_rules! illegal_mating_drop {
    ($state:expr) => {
        $state.history.last().is_some_and(|snapshot| {
            move_type!(&snapshot.move_ply) == DROP_MOVE
                && !drop_can_checkmate!(&snapshot.move_ply)
        })
    };
}

/// Drop template types
///
/// Types for drop templates. A `DropMove` is a packed `u32` with the
/// piece, the target square and the modifiers (bit 0 = LSB):
///
/// ```text
///   0               8                       20                      31
///   ┌───────────────┬───────────────────────┬────────────────────────┐
///   │     piece     │        square         │       modifiers        │
///   └───────────────┴───────────────────────┴────────────────────────┘
/// ```
///
/// - Bits 0..7   : dropped piece index
/// - Bits 8..19  : target square index
/// - Bits 20..31 : drop modifiers, read by `drop_k!`
///
/// Other types:
///
/// - `Drops`   : a drop and the CPMN pattern its target square must match
/// - `DropSet` : all `Drops` of one (piece, square) table slot
///
pub type DropMove = u32;
pub type Drops = (DropMove, Pattern);
pub type DropSet = Vec<Drops>;

/*----------------------------------------------------------------------------*\
                         DROP MODIFIER REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// drop_k!
///
/// Reads the checkmate ban of a [`DropMove`] template. Drop generation
/// uses it when it makes the drop moves.
///
/// Params:
/// - drop: &Drops -> pair with the packed `DropMove` word to read
///
/// Return:
/// bool           -> true when the drop must not give mate (bit 20)
///
#[macro_export]
macro_rules! drop_k {
    ($drop:expr) => {
        ($drop.0 >> 20) & 1 == 1
    };
}
