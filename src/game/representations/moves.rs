//! moves.rs
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
//! Created: 26/01/2026
//! Author : Alden Luthfi

use crate::*;

/// AttackMask
///
/// One attack candidate: `(piece index, target square, movement vector)`.
///
/// Move generation and legality validation use the vector to reconstruct the
/// exact path by which the piece attacks the target.
pub type AttackMask = (PieceIndex, Square, MoveVector);

/// MoveSignature
///
/// XOR of every `u64` entry in `Move.1`, folding a move's capture list into
/// one integer. Used as a safe, pointer-free move identity token for
/// transposition table storage, where holding a raw list pointer would dangle.
pub type MoveSignature = u64;

/// PseudoMove
///
/// Compact move descriptor stored in a transposition table.
///
/// The fields are `(Move.0, MoveSignature)`. It identifies a live [`Move`]
/// without retaining its auxiliary capture-list allocation.
pub type PseudoMove = (u128, MoveSignature);

/// Move
///
/// One executable move with an inline primary word and optional extra payload.
///
/// `Move.0` is the packed primary word. `Move.1` stores records needed only by
/// multi-capture and castling formats.
///
/// The low three bits select the packed format; the rest depend on it:
///
/// - `000`    : quiet move, no capture
/// - `001`    : single capture or unload
/// - `010`    : multi-capture (extra captures spill into `Move.1`)
/// - `011`    : drop
/// - `100`    : castling
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
/// - ul       : this is an unload (place the last capture back) not a real
///   capture
/// - um       : the captured piece was still unmoved
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
/// - cm       : whether the drop may deliver checkmate
///
/// Castling (`100`) keeps the king step in the base word's `start`/`end`
/// squares and packs the rook's move into the capture-payload region above:
/// its from-square in `capt sq`, its to-square in `unload sq`, and its piece
/// type in `capt piece`. `Move.1` then lists the squares the king passes,
/// one 13-bit entry each (a must-be-unattacked flag in bit 0, the square
/// index in bits 1..12) reusing the `ul` / `unload sq` positions.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Move(pub u128, pub Option<Arc<Vec<u64>>>);

/*----------------------------------------------------------------------------*\
                               UTILITY MOVE MACROS
\*----------------------------------------------------------------------------*/

/// m_captures!
///
/// Borrows the capture/check list of a `Move` as a slice, yielding an empty
/// slice for the common no-payload case.
///
/// Params:
/// - mv: &Move -> move whose auxiliary list is borrowed
///
/// Return:
/// &[u64]      -> packed multi-capture records, empty when `Move.1` is `None`
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
///
/// Params:
/// - mv: &Move   -> move whose auxiliary list is folded
///
/// Return:
/// MoveSignature -> XOR of all records, capture flag in bit 34
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
///
/// m_matches!
///   Params:
///   - mv    : &Move       -> live move under test
///   - pseudo: &PseudoMove -> stored word + signature to match against
///   Return:
///
///   bool
///   whether the move is the one the pseudo-move recorded
///
/// m_capture!
///   Params:
///   - mv    : &Move       -> move classified
///   Return:
///
///   bool
///   whether any real capture exists (unloads excluded)
///
/// m_pseudocapture!
///   Params:
///   - pseudo: &PseudoMove -> stored move word + signature
///   Return:
///
///   bool
///   whether the signature's bit 34 records a real capture
///
/// m_drop!
///   Params:
///   - mv    : &Move       -> move classified
///   Return:
///   bool                  -> whether the move is a drop
///
/// m_promotion!
///   Params:
///   - mv    : &Move       -> move classified
///   Return:
///   bool                  -> whether the move promotes
///
/// m_quiet!
///   Params:
///   - mv    : &Move       -> move classified
///   Return:
///   bool                  -> whether the move is quiet and not a promotion
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
///
/// All OR the masked value into place and return nothing; every entry
/// takes the same first parameter:
///
/// - mv: &mut Move
///   move whose packed word is written
///
/// Second parameter per member:
///
/// enc_move_type!
///   Params:
///   - val        : u128 -> format tag, masked into bits 0..2
///
/// enc_piece!
///   Params:
///   - val        : u128 -> moving piece index, masked into bits 3..10
///
/// enc_start!
///   Params:
///   - val        : u128 -> origin square, masked into bits 11..22
///
/// enc_end!
///   Params:
///   - val        : u128 -> target square, masked into bits 23..34
///
/// enc_is_initial!
///   Params:
///   - val        : u128 -> initial-move flag, masked into bit 35
///
/// enc_promotion!
///   Params:
///   - val        : u128 -> promotion flag, masked into bit 36
///
/// enc_creates_enp!
///   Params:
///   - val        : u128 -> creates-en-passant flag, masked into bit 37
///
/// enc_promoted!
///   Params:
///   - val        : u128 -> promoted piece index, masked into bits 38..45
///
/// enc_created_enp!
///   Params:
///   - val        : u128 -> created en passant square, masked into bits 46..77
///
/// enc_is_unload!
///   Params:
///   - val        : u128 -> unload flag, masked into bit 78
///
/// enc_unload_square!
///   Params:
///   - val        : u128 -> unload square, masked into bits 79..90
///
/// enc_captured_piece!
///   Params:
///   - val        : u128 -> captured piece index, masked into bits 91..98
///
/// enc_captured_square!
///   Params:
///   - val        : u128 -> captured square, masked into bits 99..110
///
/// enc_captured_unmoved!
///   Params:
///   - val        : u128 -> captured-was-unmoved flag, masked into bit 111
///
/// enc_capture_part!
///   Params:
///   - taken_piece: u128 -> whole 34-bit capture payload, bits 78..111
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
/// checks, make/undo logic, and IO serialization. Each takes the same
/// single parameter (a `PseudoMove` also works wherever only `.0` is
/// read, since its first field mirrors `Move.0`):
/// - mv: &Move -> move whose packed word is read
///
/// `is_pass!` is a semantic helper built on top of raw fields: a quiet move
/// whose start and end squares are equal.
///
/// is_pass!
///   Return:
///   bool      -> quiet move with equal start and end squares
///
/// move_type!
///   Return:
///   u128      -> format tag (bits 0..2)
///
/// piece!
///   Return:
///   u128      -> moving piece index (bits 3..10)
///
/// start!
///   Return:
///   u128      -> origin square (bits 11..22)
///
/// end!
///   Return:
///   u128      -> target square (bits 23..34)
///
/// is_initial!
///   Return:
///   u128      -> initial-move flag, 0 or 1 (bit 35)
///
/// promotion!
///   Return:
///   bool      -> promotion flag (bit 36)
///
/// creates_enp!
///   Return:
///   bool      -> creates-en-passant flag (bit 37)
///
/// promoted!
///   Return:
///   u128      -> promoted piece index (bits 38..45)
///
/// created_enp!
///   Return:
///   u128      -> created en passant square (bits 46..77)
///
/// is_unload!
///   Return:
///   bool      -> unload flag (bit 78)
///
/// unload_square!
///   Return:
///   u128      -> unload square (bits 79..90)
///
/// captured_piece!
///   Return:
///   u128      -> captured piece index (bits 91..98)
///
/// captured_square!
///   Return:
///   u128      -> captured square (bits 99..110)
///
/// captured_unmoved!
///   Return:
///   bool      -> captured-was-unmoved flag (bit 111)
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
/// those records during make/undo and move display logic. Each takes the
/// same single parameter:
/// - entry: u64 -> packed multi-capture record read
///
/// multi_move_is_unload!
///   Return:
///   bool       -> unload flag (bit 0)
///
/// multi_move_unload_square!
///   Return:
///   u64        -> unload square (bits 1..12)
///
/// multi_move_captured_piece!
///   Return:
///   u64        -> captured piece index (bits 13..20)
///
/// multi_move_captured_square!
///   Return:
///   u64        -> captured square (bits 21..32)
///
/// multi_move_captured_unmoved!
///   Return:
///   bool       -> captured-was-unmoved flag (bit 33)
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
/// the variable-length captured-piece list for `MULTI_CAPTURE_MOVE`. All OR
/// the masked value into place and return nothing; every entry takes the
/// same first parameter:
/// - entry: &mut u64 -> packed multi-capture record written
///
/// Second parameter per member:
///
/// enc_multi_move_is_unload!
///   Params:
///   - val: u64      -> unload flag, masked into bit 0
///
/// enc_multi_move_unload_square!
///   Params:
///   - val: u64      -> unload square, masked into bits 1..12
///
/// enc_multi_move_captured_piece!
///   Params:
///   - val: u64      -> captured piece index, masked into bits 13..20
///
/// enc_multi_move_captured_square!
///   Params:
///   - val: u64      -> captured square, masked into bits 21..32
///
/// enc_multi_move_captured_unmoved!
///   Params:
///   - val: u64      -> captured-was-unmoved flag, masked into bit 33
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
