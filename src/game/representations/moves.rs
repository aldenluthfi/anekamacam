//! # moves.rs
//!
//! Implements compact move encoding for chess-like games.
//!
//! Search generates and unmakes millions of moves, so a move must be small,
//! copyable, and self-describing without touching the board. This file gives
//! the engine that representation: a bit-packed word carrying everything
//! make/undo needs — origin, target, and the capture, promotion, drop, and
//! castling payloads — so the hot paths pass moves by value and decode them
//! with cheap shifts.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 26/01/2026

use crate::*;

/// AttackMask
///
/// Represents an attacking piece together with its target and attack path.
/// The tuple stores the attacking piece index, the attacked square, and the
/// move vector that realizes the attack.
/// It is used during move generation and legality validation.
pub type AttackMask = (PieceIndex, Square, MoveVector);

/// MoveSignature
///
/// XOR of every `u64` entry in `Move.1`, folding a move's capture list into
/// one integer. Used as a safe, pointer-free move identity token for
/// transposition table storage, where holding a raw list pointer would dangle.
pub type MoveSignature = u64;

/// PseudoMove
///
/// A compact move descriptor stored in and returned by the transposition table.
/// The first field mirrors `Move.0` verbatim; the second field is the
/// `MoveSignature` (XOR of all `u64` elements in `Move.1`).  A `PseudoMove`
/// can be matched against a `Move` without holding a reference to the captures
/// list, eliminating the dangling-pointer hazard of storing a raw `Arc`
/// pointer.
pub type PseudoMove = (u128, MoveSignature);

/// Move
///
/// A type representing a single move in the game.
/// This structure is used for moves without multiple captures (SingleNoCapture,
/// SingleCapture, and HopperCapture).
///
/// The low three bits select the packed format; the rest depend on it:
///
/// - `000` : quiet move, no capture
/// - `001` : single capture or unload
/// - `010` : multi-capture (extra captures spill into `Move.1`)
/// - `011` : drop
/// - `100` : castling
///
/// Formats `000`/`001`/`010` share this base word in `Move.0` (bit 0 = LSB):
///
/// ┌──────┬───────┬────────┬────────┬────┬────┬────┬──────────┬───────────────┐
/// │ 0..2 │ 3..10 │ 11..22 │ 23..34 │ 35 │ 36 │ 37 │ 38..45   │ 46..77        │
/// │ type │ piece │ start  │ end    │ in │ pr │ ep │ promoted │ created ep sq │
/// └──────┴───────┴────────┴────────┴────┴────┴────┴──────────┴───────────────┘
///
/// - in       : the move must be an initial move of the piece
/// - pr       : the move is a promotion
/// - ep       : the move creates an en passant square
/// - promoted : the piece type promoted to (only when `pr` is set)
///
/// A capture (`001`, and the first capture of a `010` move) adds a payload
/// higher in the same word:
///
/// ┌────┬───────────┬────────────┬─────────┬─────┐
/// │ 78 │ 79..90    │ 91..98     │ 99..110 │ 111 │
/// │ ul │ unload sq │ capt piece │ capt sq │ um  │
/// └────┴───────────┴────────────┴─────────┴─────┘
///
/// - ul : this is an unload (place the last capture back) not a real capture
/// - um : the captured piece was still unmoved
///
/// A multi-capture (`010`) keeps its first capture in the base word above and
/// each further capture as one 34-bit record in `Move.1`:
///
/// ┌────┬───────────┬────────────┬─────────┬────┐
/// │ 0  │ 1..12     │ 13..20     │ 21..32  │ 33 │
/// │ ul │ unload sq │ capt piece │ capt sq │ um │
/// └────┴───────────┴────────────┴─────────┴────┘
///
/// A drop (`011`) needs no origin square:
///
/// ┌──────┬───────┬─────────┬────┐
/// │ 0..2 │ 3..10 │ 11..22  │ 23 │
/// │ type │ piece │ drop sq │ cm │
/// └──────┴───────┴─────────┴────┘
///
/// - cm : whether the drop may deliver checkmate
///
/// Castling (`100`) keeps the king step in the base word's `start`/`end`
/// squares and packs the rook's move into the capture-payload region above:
/// its from-square in `capt sq`, its to-square in `unload sq`, and its piece
/// type in `capt piece`. `Move.1` then lists the squares the king passes,
/// one 13-bit entry each (a must-be-unattacked flag in bit 0, the square
/// index in bits 1..12) reusing the `ul` / `unload sq` positions.
///
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Move(pub u128, pub Option<Arc<Vec<u64>>>);

/*----------------------------------------------------------------------------*\
                               UTILITY MOVE MACROS
\*----------------------------------------------------------------------------*/

/// m_captures!
///
/// Borrows the capture/check list of a `Move` as a slice, yielding an empty
/// slice for the common no-payload case.
#[macro_export]
macro_rules! m_captures {
    ($mv:expr) => {
        $mv.1.as_deref().map_or(&[] as &[u64], |list| list.as_slice())
    };
}

/// m_signature!
///
/// Computes the `MoveSignature` for a `Move` by XOR-folding every element of
/// `move.1`.  The result is 0 for moves with no captures (empty list).
///
/// The 34th bit is set if there is an actual capture in the capture list (not
/// all unloads).
#[macro_export]
macro_rules! m_signature {
    ($mv:expr) => {
        m_captures!($mv).iter().fold(0u64, |acc, &x| acc ^ x) |
        (m_captures!($mv).iter().any(
            |&capture| !multi_move_is_unload!(capture)
        ) as u64) << 34                                                         /* Set if its an actual capture       */
    };
}

/// Move predicate macros.
///
/// `m_matches!` tests a `Move` against a stored `PseudoMove` without
/// touching the captures list pointer; `m_capture!` and `m_pseudocapture!`
/// detect real captures (from the captures list or from the precomputed
/// signature bit, respectively); `m_drop!`, `m_promotion!`, and `m_quiet!`
/// classify moves for ordering and pruning decisions during search.
#[macro_export]
macro_rules! m_matches {
    ($mv:expr, $pseudo:expr) => {
        $mv.0 == $pseudo.0 && m_signature!($mv) == $pseudo.1
    };
}

#[macro_export]
macro_rules! m_capture {
    ($mv:expr) => {
        move_type!($mv) == SINGLE_CAPTURE_MOVE && !is_unload!($mv) ||
        move_type!($mv) == MULTI_CAPTURE_MOVE  &&
        m_captures!($mv).iter().any(
            |&capture| !multi_move_is_unload!(capture)
        )
    };
}

#[macro_export]
macro_rules! m_pseudocapture {
    ($mv:expr) => {
        move_type!($mv) == SINGLE_CAPTURE_MOVE && !is_unload!($mv) ||
        move_type!($mv) == MULTI_CAPTURE_MOVE && ($mv.1 >> 34) & 1 == 1
    };
}

#[macro_export]
macro_rules! m_drop {
    ($mv:expr) => {
        move_type!($mv) == DROP_MOVE
    };
}

#[macro_export]
macro_rules! m_promotion {
    ($mv:expr) => {
        promotion!($mv)
    };
}

#[macro_export]
macro_rules! m_quiet {
    ($mv:expr) => {
        move_type!($mv) == QUIET_MOVE && !promotion!($mv)
    };
}

/*----------------------------------------------------------------------------*\
                          MOVE REPRESENTATION ENCODING
\*----------------------------------------------------------------------------*/

/// Primary move-bitfield encoder macros.
///
/// These macros write individual fields into `Move.0` (`u128`) using the
/// packed move layout described above the `Move` type alias.
///
/// They are intentionally low-level and composable: callers build a move in
/// stages by applying only the fields relevant for the current move format.
/// Capture payload bits (starting at bit 78) can be written either field-by-
/// field (`enc_is_unload!`, `enc_captured_piece!`, ...) or as a single packed
/// chunk using `enc_capture_part!`.
#[macro_export]
macro_rules! enc_move_type {
    ($mv:expr, $val:expr) => {
        $mv.0 |= $val & 0x7;
    };
}

#[macro_export]
macro_rules! enc_piece {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFF) << 3;
    };
}

#[macro_export]
macro_rules! enc_start {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 11;
    };
}

#[macro_export]
macro_rules! enc_end {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 23;
    };
}

#[macro_export]
macro_rules! enc_is_initial {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 35;
    };
}

#[macro_export]
macro_rules! enc_promotion {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 36;
    };
}

#[macro_export]
macro_rules! enc_creates_enp {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 37;
    };
}

#[macro_export]
macro_rules! enc_promoted {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFF) << 38;
    };
}

#[macro_export]
macro_rules! enc_created_enp {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFFFFFFF) << 46;
    };
}

#[macro_export]
macro_rules! enc_is_unload {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 78;
    };
}

#[macro_export]
macro_rules! enc_unload_square {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 79;
    };
}

#[macro_export]
macro_rules! enc_captured_piece {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFF) << 91;
    };
}

#[macro_export]
macro_rules! enc_captured_square {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFF) << 99;
    };
}

#[macro_export]
macro_rules! enc_captured_unmoved {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 111;
    };
}

#[macro_export]
macro_rules! enc_capture_part {
    ($mv:expr, $taken_piece:expr) => {
        $mv.0 |= ($taken_piece & 0x3_FFFF_FFFF) << 78;
    };
}

/*----------------------------------------------------------------------------*\
                          MOVE REPRESENTATION DECODING
\*----------------------------------------------------------------------------*/

/// Decoders for the primary packed `Move` representation.
///
/// These macros extract typed values and flags from `Move.0` for legality
/// checks, make/undo logic, and IO serialization.
///
/// `is_pass!` is a semantic helper built on top of raw fields: a quiet move
/// whose start and end squares are equal.
#[macro_export]
macro_rules! is_pass {
    ($mv:expr) => {
        move_type!($mv) == QUIET_MOVE && end!($mv) == start!($mv)
    };
}

#[macro_export]
macro_rules! move_type {
    ($mv:expr) => {
        $mv.0 & 0x7
    };
}

#[macro_export]
macro_rules! piece {
    ($mv:expr) => {
        ($mv.0 >> 3) & 0xFF
    };
}

#[macro_export]
macro_rules! start {
    ($mv:expr) => {
        ($mv.0 >> 11) & 0xFFF
    };
}

#[macro_export]
macro_rules! end {
    ($mv:expr) => {
        ($mv.0 >> 23) & 0xFFF
    };
}

#[macro_export]
macro_rules! is_initial {
    ($mv:expr) => {
        ($mv.0 >> 35) & 1
    };
}

#[macro_export]
macro_rules! promotion {
    ($mv:expr) => {
        ($mv.0 >> 36) & 1 == 1
    };
}

#[macro_export]
macro_rules! creates_enp {
    ($mv:expr) => {
        ($mv.0 >> 37) & 1 == 1
    };
}

#[macro_export]
macro_rules! promoted {
    ($mv:expr) => {
        ($mv.0 >> 38) & 0xFF
    };
}

#[macro_export]
macro_rules! created_enp {
    ($mv:expr) => {
        ($mv.0 >> 46) & 0xFFFFFFFF
    };
}

#[macro_export]
macro_rules! is_unload {
    ($mv:expr) => {
        ($mv.0 >> 78) & 1 == 1
    };
}

#[macro_export]
macro_rules! unload_square {
    ($mv:expr) => {
        ($mv.0 >> 79) & 0xFFF
    };
}

#[macro_export]
macro_rules! captured_piece {
    ($mv:expr) => {
        ($mv.0 >> 91) & 0xFF
    };
}

#[macro_export]
macro_rules! captured_square {
    ($mv:expr) => {
        ($mv.0 >> 99) & 0xFFF
    };
}

#[macro_export]
macro_rules! captured_unmoved {
    ($mv:expr) => {
        ($mv.0 >> 111) & 1 == 1
    };
}

/*----------------------------------------------------------------------------*\
                        MOVE LIST REPRESENTATION DECODING
\*----------------------------------------------------------------------------*/

/// Decoders for auxiliary multi-capture entries (`u64`) stored in `Move.1`.
///
/// Multi-capture moves keep their first capture in `Move.0` and any remaining
/// captures in `Move.1` as compact 34-bit packed records. These macros unpack
/// those records during make/undo and move display logic.
#[macro_export]
macro_rules! multi_move_is_unload {
    ($mv:expr) => {
        $mv & 1 == 1
    };
}

#[macro_export]
macro_rules! multi_move_unload_square {
    ($mv:expr) => {
        ($mv >> 1) & 0xFFF
    };
}

#[macro_export]
macro_rules! multi_move_captured_piece {
    ($mv:expr) => {
        ($mv >> 13) & 0xFF
    };
}

#[macro_export]
macro_rules! multi_move_captured_square {
    ($mv:expr) => {
        ($mv >> 21) & 0xFFF
    };
}

#[macro_export]
macro_rules! multi_move_captured_unmoved {
    ($mv:expr) => {
        ($mv >> 33) & 1 == 1
    };
}

/*----------------------------------------------------------------------------*\
                      MOVE LIST REPRESENTATION ENCODING
\*----------------------------------------------------------------------------*/

/// Encoders for auxiliary multi-capture entries (`u64`) stored in `Move.1`.
///
/// These macros mirror the `multi_move_*` decoders and are used when building
/// the variable-length captured-piece list for `MULTI_CAPTURE_MOVE`.
#[macro_export]
macro_rules! enc_multi_move_is_unload {
    ($mv:expr, $val:expr) => {
        $mv |= $val & 1;
    };
}

#[macro_export]
macro_rules! enc_multi_move_unload_square {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0xFFF) << 1;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_piece {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0xFF) << 13;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_square {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0xFFF) << 21;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_unmoved {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 1) << 33;
    };
}
