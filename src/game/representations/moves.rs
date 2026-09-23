//! moves.rs
//!
//! Implements compact move encoding for square-board variants.
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

/*----------------------------------------------------------------------------*\
                              MOVE REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// AttackMask
///
/// One attack candidate: `(attacking piece, its origin square, the movement
/// vector that reaches out from it)`. The attacked square is not in the
/// tuple because it is the index the candidate is filed under: a row of
/// `relevant_attacks[side][square]` answers "who could be hitting this
/// square", which is the direction check detection asks in.
///
/// The vector is carried because arrival is not the whole question. A
/// variant may let the same piece reach one square by several routes with
/// different rules along them — one blockable, one hopping, one that may
/// only capture what it outranks — so a candidate is confirmed by walking
/// its own vector, not by a shared ray table.
pub type AttackMask = (PieceIndex, Square, MoveVector);

/// MoveSignature
///
/// XOR of every `u64` entry in `Move.1`, folding a move's capture list into
/// one integer. Used as a safe, pointer-free move identity token for
/// transposition table storage, where holding a raw list pointer would dangle.
///
/// The fold is lossy by construction — it is an identity check, not a
/// reconstruction — so bit 35 rides above it as a second discriminator,
/// separating a list that takes something from one that only unloads.
///
/// Bits 0..31:
///
/// ```text
///   0                                                               31
///   ┌────────────────────────────────────────────────────────────────┐
///   │                             XOR →                              │
///   └────────────────────────────────────────────────────────────────┘
/// ```
///
/// Bits 32..63:
///
/// ```text
///   32   35                                                         63
///          36
///   ┌────┬─┬─────────────────────────────────────────────────────────┐
///   │← X │c│                          unused                         │
///   └────┴─┴─────────────────────────────────────────────────────────┘
/// ```
///
/// - bits 0..34 (`XOR`): XOR-folded capture records
/// - bit 35     (`c`)  : at least one record is a real capture
/// - bits 36..63       : unused
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
/// multi-capture and castling formats. It is refcounted rather than owned
/// because search copies moves far more often than it makes them — into
/// ordered lists, principal variations, and killer slots — and almost every
/// move leaves it `None`, so the rare payload is shared instead of cloned.
///
/// The low three bits select the packed format; the rest depend on it:
///
/// - `000`    : quiet move, no capture
/// - `001`    : single capture or unload
/// - `010`    : multi-capture (extra captures spill into `Move.1`)
/// - `011`    : drop
/// - `100`    : castling
///
/// Formats `000`/`001`/`010` share this layout in `Move.0`.
/// Capture fields apply only to `001` and the first capture of `010`.
/// Field widths are proportional; every row represents 32 bits.
///
/// Bits 0..31:
///
/// ```text
///   0     3                  13                     24              31
///   ┌─────┬──────────────────┬──────────────────────┬────────────────┐
///   │type │       piece      │        start         │     end →      │
///   └─────┴──────────────────┴──────────────────────┴────────────────┘
/// ```
///
/// Bits 32..63:
///
/// ```text
///   32      35  37             48                                   63
///             36  38
///   ┌───────┬─┬─┬─┬────────────────┬─────────────────────────────────┐
///   │← end  │i│p│e│   promoted     │          created ep →           │
///   └───────┴─┴─┴─┴────────────────┴─────────────────────────────────┘
/// ```
///
/// Bits 64..95:
///
/// ```text
///   64                            80                      92        95
///                                   81
///   ┌─────────────────────────────┬─┬─────────────────────┬──────────┐
///   │        ← created ep         │u│      unload sq      │capt pc → │
///   └─────────────────────────────┴─┴─────────────────────┴──────────┘
/// ```
///
/// ```text
///   96      102                   113                              127
///                                   114
///   ┌───────┬─────────────────────┬─┬─┬──────────────────────────────┐
///   │← cap  │       capt sq       │m│o│            unused            │
///   └───────┴─────────────────────┴─┴─┴──────────────────────────────┘
/// ```
///
/// - bits 0..2     (`type`)      : packed move format
/// - bits 3..12    (`piece`)     : moving piece index
/// - bits 13..23   (`start`)     : origin square
/// - bits 24..34   (`end`)       : target square
/// - bit 35        (`i`)         : move must be initial for the piece
/// - bit 36        (`p`)         : the move is a promotion
/// - bit 37        (`e`)         : the move creates an en-passant square
/// - bits 38..47   (`promoted`)  : promoted piece index when `p` is set
/// - bits 48..79   (`created ep`): created en-passant square
/// - bit 80        (`u`)         : unload the last capture, not a capture
/// - bits 81..91   (`unload sq`) : unload square
/// - bits 92..101  (`capt pc`)   : captured piece index
/// - bits 102..112 (`capt sq`)   : captured square
/// - bit 113       (`m`)         : captured piece was unmoved
/// - bit 114       (`o`)         : captured piece was the mover's own
/// - bits 115..127               : unused
///
/// A piece index spends ten bits and a square eleven, which is the whole of
/// `MAX_SQUARES`. The two are neighbours everywhere they appear, so the bit
/// a square gives up is the bit the index beside it takes, and the three
/// flags between `end` and `promoted` keep the places they have always had.
///
/// A multi-capture (`010`) keeps its first capture above. Each further
/// capture is one 35-bit record in a `u64` stored in `Move.1`:
///
/// Bits 0..31:
///
/// ```text
///   0 1                     12                  22                  31
///   ┌─┬─────────────────────┬───────────────────┬────────────────────┐
///   │u│      unload sq      │      capt pc      │     capt sq →      │
///   └─┴─────────────────────┴───────────────────┴────────────────────┘
/// ```
///
/// Bits 32..63:
///
/// ```text
///   32  34                                                          63
///     33  35
///   ┌─┬─┬─┬──────────────────────────────────────────────────────────┐
///   │←│m│o│                          unused                          │
///   └─┴─┴─┴──────────────────────────────────────────────────────────┘
/// ```
///
/// - bit 0       (`u`): unload flag
/// - bits 1..11       : unload square
/// - bits 12..21      : captured piece index
/// - bits 22..32      : captured square
/// - bit 33      (`m`): captured piece was unmoved
/// - bit 34      (`o`): captured piece was the mover's own
/// - bits 35..63      : unused
///
/// The record is 35 bits and its `o` flag sits on bit 34. `enc_capture_part!`
/// shifts the whole of it to bit 80, which lands every field exactly where
/// the primary word reads it, so the first capture needs no separate
/// spelling from the rest.
///
/// The `o` flag is written where a destroying leg is resolved, which is the
/// one place in the engine that already knows whose piece stood on the
/// square. Recording it there keeps the question "did this move take
/// anything from the other side" answerable from the move alone, without a
/// piece table to read a colour out of.
///
/// A drop (`011`) has no origin square, so it repeats the placement square in
/// both `start` and `end`. Make/undo reads `start`, while every target-indexed
/// consumer — history and ordering — reads `end`, so the two have to agree
/// rather than leaving `end` unwritten:
///
/// Bits 0..31:
///
/// ```text
///   0     3                  13                     24              31
///   ┌─────┬──────────────────┬──────────────────────┬────────────────┐
///   │type │       piece      │       drop sq        │   drop sq →    │
///   └─────┴──────────────────┴──────────────────────┴────────────────┘
/// ```
///
/// Bits 32..63:
///
/// ```text
///   32      35                                                      63
///   ┌───────┬────────────────────────────────────────────────────────┐
///   │← sq   │                     unused →                           │
///   └───────┴────────────────────────────────────────────────────────┘
/// ```
///
/// Bits 64..95:
///
/// ```text
///   64                                                              95
///   ┌────────────────────────────────────────────────────────────────┐
///   │                           ← unused →                           │
///   └────────────────────────────────────────────────────────────────┘
/// ```
///
/// Bits 96..127:
///
/// ```text
///   96                            112                              127
///   ┌─────────────────────────────┬─┬────────────────────────────────┐
///   │          ← unused           │c│             unused             │
///   └─────────────────────────────┴─┴────────────────────────────────┘
/// ```
///
/// - bits 0..2    (`type`) : drop format tag
/// - bits 3..12   (`piece`): dropped piece index
/// - bits 13..23  (`start`): target square
/// - bits 24..34  (`end`)  : target square, repeated
/// - bits 35..111          : unused
/// - bit 112      (`c`)    : whether the drop may deliver checkmate
/// - bits 113..127         : unused
///
/// Castling (`100`) keeps the primary castling piece's step in the base
/// word's `start`/`end` squares. It packs the secondary castling piece's
/// from-square in `captured square`, its to-square in `unload square`, and
/// its piece type in `captured piece`. `Move.1` lists the primary piece's
/// path as `u64` entries:
///
/// Bits 0..31:
///
/// ```text
///   0 1                     12                                      31
///   ┌─┬─────────────────────┬────────────────────────────────────────┐
///   │u│       square        │                unused →                │
///   └─┴─────────────────────┴────────────────────────────────────────┘
/// ```
///
/// Bits 32..63:
///
/// ```text
///   32                                                              63
///   ┌────────────────────────────────────────────────────────────────┐
///   │                            ← unused                            │
///   └────────────────────────────────────────────────────────────────┘
/// ```
///
/// - bit 0 (`u`): the square must be unattacked
/// - bits 1..11 : square index
/// - bits 12..63: unused
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
/// `move.1`. The result is 0 for moves with no captures (empty list).
///
/// Bit 35 is set when at least one record in the list is a real capture, so
/// a list of nothing but unloads stays distinguishable from one that takes
/// a piece even where the fold happens to agree. It sits one bit above the
/// widest record rather than on a fixed number, so it never collides with
/// a field the fold itself carries.
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
        ) as u64) << 35                                                         /* set when a record really captures  */
    };
}

/// Move predicate macros.
///
/// `m_matches!` tests a `Move` against a stored `PseudoMove` without
/// touching the captures list pointer; `m_capture!` asks whether the move
/// takes anything belonging to the other side; `m_drop!`, `m_promotion!`,
/// and `m_quiet!` classify moves for ordering and pruning decisions during
/// search.
///
/// Every member takes the move it judges as its first parameter:
///
/// - mv: &Move -> move under test
///
/// m_matches!
///
///   Params:
///   - pseudo: &PseudoMove -> stored word and signature to match against
///
///   Return:
///   bool                  -> whether this is the move it recorded
///
/// The rest take no second parameter, and each answers one question about
/// the move with a `bool`:
///
/// - m_capture!   -> the other side loses a piece, unloads excluded
/// - m_drop!      -> the move places a piece from the hand
/// - m_promotion! -> the move promotes
/// - m_quiet!     -> the move is in quiet format and does not promote
///
/// A destroying leg removes a piece of the mover's own, and both kinds of
/// victim are written into one list under one encoding, which is why each
/// record carries the `o` flag saying which it was. A move that removes
/// nothing but its own army takes no more than a quiet move walking into a
/// loss does, and calling it a capture hands quiescence a position with no
/// quiet horizon to reach.
#[macro_export]
macro_rules! m_matches {
    ($mv:expr, $pseudo:expr) => {
        $mv.0 == $pseudo.0 && m_signature!($mv) == $pseudo.1
    };
}

#[macro_export]
macro_rules! m_capture {
    ($mv:expr) => {
        move_type!($mv) == SINGLE_CAPTURE_MOVE
        && !is_unload!($mv)
        && !captured_own!($mv) ||

        move_type!($mv) == MULTI_CAPTURE_MOVE
        && m_captures!($mv).iter().any(
            |&capture|
            !multi_move_is_unload!(capture) &&
            !multi_move_captured_own!(capture)
        )
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
/// packed move layout described above the `Move` type.
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
/// - mv: &mut Move -> move whose packed word is written
///
/// Every entry but the last also takes `val: u128`, masked into the field
/// its name spells:
///
/// - enc_move_type!       -> format tag, bits 0..2
/// - enc_piece!           -> moving piece index, bits 3..12
/// - enc_start!           -> origin square, bits 13..23
/// - enc_end!             -> target square, bits 24..34
/// - enc_is_initial!      -> initial-move flag, bit 35
/// - enc_promotion!       -> promotion flag, bit 36
/// - enc_creates_enp!     -> creates-en-passant flag, bit 37
/// - enc_promoted!        -> promoted piece index, bits 38..47
/// - enc_created_enp!     -> created en-passant square, bits 48..79
/// - enc_is_unload!       -> unload flag, bit 80
/// - enc_unload_square!   -> unload square, bits 81..91
/// - enc_captured_piece!  -> captured piece index, bits 92..101
/// - enc_captured_square! -> captured square, bits 102..112
///
/// enc_capture_part!
///
///   Params:
///   - taken_piece: u128 -> whole 35-bit capture payload, bits 80..114
///
/// Notes:
/// `enc_capture_part!` is how a single capture gets its bits 113 and 114.
/// Move generation builds every capture as a multi-capture payload word, so
/// the payload's bits 33 and 34 become those two under the shift; there is
/// no separate encoder for the captured-was-unmoved or captured-was-own
/// flags.
#[macro_export]
macro_rules! enc_move_type {
    ($mv:expr, $val:expr) => {
        $mv.0 |= $val & 0x7;
    };
}

#[macro_export]
macro_rules! enc_piece {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0x3FF) << 3;
    };
}

#[macro_export]
macro_rules! enc_start {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0x7FF) << 13;
    };
}

#[macro_export]
macro_rules! enc_end {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0x7FF) << 24;
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
        $mv.0 |= ($val & 0x3FF) << 38;
    };
}

#[macro_export]
macro_rules! enc_created_enp {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0xFFFFFFFF) << 48;
    };
}

#[macro_export]
macro_rules! enc_is_unload {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 1) << 80;
    };
}

#[macro_export]
macro_rules! enc_unload_square {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0x7FF) << 81;
    };
}

#[macro_export]
macro_rules! enc_captured_piece {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0x3FF) << 92;
    };
}

#[macro_export]
macro_rules! enc_captured_square {
    ($mv:expr, $val:expr) => {
        $mv.0 |= ($val & 0x7FF) << 102;
    };
}

#[macro_export]
macro_rules! enc_capture_part {
    ($mv:expr, $taken_piece:expr) => {
        $mv.0 |= ($taken_piece & 0x7_FFFF_FFFF) << 80;
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
///
/// - mv: &Move -> move whose packed word is read
///
/// Ten of them hand back the field their name spells, as `u128`:
///
/// - move_type!       -> format tag, bits 0..2
/// - piece!           -> moving piece index, bits 3..12
/// - start!           -> origin square, bits 13..23
/// - end!             -> target square, bits 24..34
/// - is_initial!      -> initial-move flag as 0 or 1, bit 35
/// - promoted!        -> promoted piece index, bits 38..47
/// - created_enp!     -> created en-passant square, bits 48..79
/// - unload_square!   -> unload square, bits 81..91
/// - captured_piece!  -> captured piece index, bits 92..101
/// - captured_square! -> captured square, bits 102..112
///
/// Five answer a yes-or-no question with `bool`:
///
/// - promotion!        -> the move promotes, bit 36
/// - creates_enp!      -> the move leaves an en-passant square, bit 37
/// - is_unload!        -> the payload drops a piece, takes none, bit 80
/// - captured_unmoved! -> the captured piece had never moved, bit 113
/// - captured_own!     -> the captured piece was the mover's, bit 114
///
/// `is_pass!` reads no field of its own. It recognises the shape a variant
/// that allows passing produces — a quiet move whose start and end squares
/// are the same — rather than spending a format tag on a move that does
/// nothing but hand over the turn.
///
/// is_pass!
///
///   Return:
///   bool -> whether the move only surrenders the turn
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
        ($mv.0 >> 3) & 0x3FF
    };
}

#[macro_export]
macro_rules! start {
    ($mv:expr) => {
        ($mv.0 >> 13) & 0x7FF
    };
}

#[macro_export]
macro_rules! end {
    ($mv:expr) => {
        ($mv.0 >> 24) & 0x7FF
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
        ($mv.0 >> 38) & 0x3FF
    };
}

#[macro_export]
macro_rules! created_enp {
    ($mv:expr) => {
        ($mv.0 >> 48) & 0xFFFFFFFF
    };
}

#[macro_export]
macro_rules! is_unload {
    ($mv:expr) => {
        ($mv.0 >> 80) & 1 == 1
    };
}

#[macro_export]
macro_rules! unload_square {
    ($mv:expr) => {
        ($mv.0 >> 81) & 0x7FF
    };
}

#[macro_export]
macro_rules! captured_piece {
    ($mv:expr) => {
        ($mv.0 >> 92) & 0x3FF
    };
}

#[macro_export]
macro_rules! captured_square {
    ($mv:expr) => {
        ($mv.0 >> 102) & 0x7FF
    };
}

#[macro_export]
macro_rules! captured_unmoved {
    ($mv:expr) => {
        ($mv.0 >> 113) & 1 == 1
    };
}

#[macro_export]
macro_rules! captured_own {
    ($mv:expr) => {
        ($mv.0 >> 114) & 1 == 1
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
///
/// - entry: u64 -> packed multi-capture record read
///
/// Three read a field as `u64`, in the layout diagrammed on [`Move`]:
///
/// - multi_move_unload_square!   -> unload square, bits 1..11
/// - multi_move_captured_piece!  -> captured piece index, bits 12..21
/// - multi_move_captured_square! -> captured square, bits 22..32
///
/// Three read a flag as `bool`:
///
/// - multi_move_is_unload!        -> the record drops a piece, bit 0
/// - multi_move_captured_unmoved! -> the captured piece never moved, bit 33
/// - multi_move_captured_own!     -> the piece was the mover's own, bit 34
#[macro_export]
macro_rules! multi_move_is_unload {
    ($mv:expr) => {
        $mv & 1 == 1
    };
}

#[macro_export]
macro_rules! multi_move_unload_square {
    ($mv:expr) => {
        ($mv >> 1) & 0x7FF
    };
}

#[macro_export]
macro_rules! multi_move_captured_piece {
    ($mv:expr) => {
        ($mv >> 12) & 0x3FF
    };
}

#[macro_export]
macro_rules! multi_move_captured_square {
    ($mv:expr) => {
        ($mv >> 22) & 0x7FF
    };
}

#[macro_export]
macro_rules! multi_move_captured_unmoved {
    ($mv:expr) => {
        ($mv >> 33) & 1 == 1
    };
}

#[macro_export]
macro_rules! multi_move_captured_own {
    ($mv:expr) => {
        ($mv >> 34) & 1 == 1
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
///
/// - entry: &mut u64 -> packed multi-capture record written
///
/// The second parameter is always `val: u64`, masked into the field the
/// name spells:
///
/// - enc_multi_move_is_unload!        -> unload flag, bit 0
/// - enc_multi_move_unload_square!    -> unload square, bits 1..11
/// - enc_multi_move_captured_piece!   -> captured piece index, bits 12..21
/// - enc_multi_move_captured_square!  -> captured square, bits 22..32
/// - enc_multi_move_captured_unmoved! -> captured-was-unmoved flag, bit 33
/// - enc_multi_move_captured_own!     -> captured-was-own flag, bit 34
#[macro_export]
macro_rules! enc_multi_move_is_unload {
    ($mv:expr, $val:expr) => {
        $mv |= $val & 1;
    };
}

#[macro_export]
macro_rules! enc_multi_move_unload_square {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0x7FF) << 1;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_piece {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0x3FF) << 12;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_square {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 0x7FF) << 22;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_unmoved {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 1) << 33;
    };
}

#[macro_export]
macro_rules! enc_multi_move_captured_own {
    ($mv:expr, $val:expr) => {
        $mv |= ($val & 1) << 34;
    };
}
