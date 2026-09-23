//! moves.rs
//!
//! Defines the packed move encoding.
//!
//! The search makes and undoes millions of moves. Thus a move must be small
//! and must have all data that make and undo need, without the board. This
//! file defines the packed move word and the macros that write and read its
//! fields.
//!
//! Created: 26/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              MOVE REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// AttackMask
///
/// One attack candidate: `(attacking piece, origin square, move vector)`.
/// The attacked square is the table index, `relevant_attacks[side][square]`.
///
/// One piece can go to a square by different paths with different rules.
/// Thus the check test walks the vector of the candidate.
///
/// Notes:
/// The vector is shared, not owned. A ray is in the table once for each
/// square it can capture on, so one copy saves much memory.
///
pub type AttackMask = (PieceIndex, Square, MoveVector);

/// MoveSignature
///
/// The XOR of all `u64` entries in `Move.1`. The hash table stores it as a
/// move identity without a pointer.
///
/// The XOR loses data, so bit 35 is a second test. It separates a list
/// with a capture from a list with only unloads.
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
/// - bits 0..34 (`XOR`): XOR of the capture records
/// - bit 35     (`c`)  : at least one record is a real capture
/// - bits 36..63       : unused
///
pub type MoveSignature = u64;

/// PseudoMove
///
/// The move form in the hash table, `(Move.0, MoveSignature)`. It
/// identifies a [`Move`] without its capture list.
///
pub type PseudoMove = (u128, MoveSignature);

/// Move
///
/// One move. `Move.0` is the packed main word. `Move.1` has the records of
/// the multi-capture and castling formats. It is shared (`Arc`), because
/// the search copies moves much and most moves have `None`.
///
/// The low three bits select the format:
///
/// - `000`    : quiet move, no capture
/// - `001`    : single capture or unload
/// - `010`    : multi-capture, more captures in `Move.1`
/// - `011`    : drop
/// - `100`    : castling
///
/// Formats `000`, `001` and `010` use this layout. The capture fields are
/// for `001` and for the first capture of `010`. Each row has 32 bits, and
/// the field widths are to scale.
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
/// Bits 96..127:
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
/// - bit 35        (`i`)         : the move is a first move of the piece
/// - bit 36        (`p`)         : the move is a promotion
/// - bit 37        (`e`)         : the move makes an en passant square
/// - bits 38..47   (`promoted`)  : promoted piece index, if `p` is set
/// - bits 48..79   (`created ep`): en passant descriptor that the move makes
/// - bit 80        (`u`)         : unload the last capture, not a capture
/// - bits 81..91   (`unload sq`) : unload square
/// - bits 92..101  (`capt pc`)   : captured piece index
/// - bits 102..112 (`capt sq`)   : captured square
/// - bit 113       (`m`)         : captured piece was unmoved
/// - bit 114       (`o`)         : captured piece was an own piece
/// - bits 115..127               : unused
///
/// A piece index has 10 bits. A square has 11 bits, enough for
/// `MAX_SQUARES`.
///
/// A multi-capture (`010`) keeps its first capture in the main word. Each
/// other capture is one 35-bit record in a `u64` in `Move.1`:
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
/// - bit 34      (`o`): captured piece was an own piece
/// - bits 35..63      : unused
///
/// `enc_capture_part!` shifts a full record to bit 80. Then each field is
/// at its place in the main word, so the first capture uses the same
/// record.
///
/// The move generator writes the `o` flag when it resolves a destroy leg.
/// Thus the move alone tells if it takes an enemy piece.
///
/// A drop (`011`) has no origin square, so `start` and `end` both have the
/// target square. Make and undo read `start`. History and ordering read
/// `end`:
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
/// - bits 24..34  (`end`)  : target square, again
/// - bits 35..111          : unused
/// - bit 112      (`c`)    : the drop can give checkmate
/// - bits 113..127         : unused
///
/// Castling (`100`) uses the main word as follows. `Move.1` has the path
/// of the main piece as `u64` entries:
///
/// - `start`, `end`   : squares of the main castling piece
/// - `captured sq`    : start square of the second piece
/// - `unload sq`      : end square of the second piece
/// - `captured piece` : piece type of the second piece
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
/// - bit 0 (`u`): the square must not be attacked
/// - bits 1..11 : square index
/// - bits 12..63: unused
///
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Move(pub u128, pub Option<Arc<Vec<u64>>>);

/*----------------------------------------------------------------------------*\
                              UTILITY MOVE MACROS
\*----------------------------------------------------------------------------*/

/// m_captures!
///
/// Borrows the record list of a `Move` as a slice. If `Move.1` is `None`,
/// the slice is empty.
///
/// Params:
/// - mv: &Move -> move with the list to borrow
///
/// Return:
/// &[u64]      -> packed records, empty when `Move.1` is `None`
///
#[macro_export]
macro_rules! m_captures {
    ($mv:expr) => {
        $mv.1.as_deref().map_or(&[] as &[u64], |list| list.as_slice())
    };
}

/// m_signature!
///
/// Calculates the `MoveSignature` of a `Move`, the XOR of all records in
/// `Move.1`. An empty list gives 0.
///
/// Bit 35 is set when one record or more is a real capture. It is above
/// the widest record, so it does not touch the XOR bits.
///
/// Params:
/// - mv: &Move   -> move with the list to fold
///
/// Return:
/// MoveSignature -> XOR of all records, capture flag in bit 35
///
#[macro_export]
macro_rules! m_signature {
    ($mv:expr) => {
        m_captures!($mv).iter().fold(0u64, |acc, &x| acc ^ x) |
        (m_captures!($mv).iter().any(
            |&capture| !multi_move_is_unload!(capture)
        ) as u64) << 35                                                         /* set when a record really captures  */
    };
}

/// Move predicate macros
///
/// Move tests for ordering and pruning. Each macro returns a `bool`.
///
/// m_matches!
///
///   Tests a `Move` against a stored `PseudoMove`, without the list
///   pointer.
///
///   Params:
///   - mv    : &Move       -> move to test
///   - pseudo: &PseudoMove -> stored word and signature
///
///   Return:
///   bool                  -> true when it is the stored move
///
/// m_capture! / m_drop! / m_promotion! / m_quiet!
///
///   Params:
///   - mv: &Move -> move to test
///
///   Return:
///   bool        -> the answer to the test below
///
/// - m_capture!   : the other side loses a piece, unloads not included
/// - m_drop!      : the move puts a piece from the hand on the board
/// - m_promotion! : the move promotes
/// - m_quiet!     : the move has the quiet format and does not promote
///
/// Notes:
/// A destroy leg removes an own piece, with the `o` flag set. `m_capture!`
/// does not count it. Else quiescence would search moves that only lose
/// own pieces.
///
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

/// m_takes_square!
///
/// Tells if the move captures the piece on one square. A move can capture
/// many pieces, so the macro searches the list. An unload is not a capture.
/// The exchange simulation and quiescence use it.
///
/// Params:
/// - mv    : &Move  -> move to test
/// - square: Square -> square to test
///
/// Return:
/// bool             -> true when the piece on the square is captured
///
#[macro_export]
macro_rules! m_takes_square {
    ($mv:expr, $square:expr) => {
        move_type!($mv) == SINGLE_CAPTURE_MOVE
        && !is_unload!($mv)
        && captured_square!($mv) as Square == $square
        || move_type!($mv) == MULTI_CAPTURE_MOVE
        && m_captures!($mv).iter().any(|&capture| {
            !multi_move_is_unload!(capture)
            && multi_move_captured_square!(capture) as Square == $square
        })
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

/// Primary move encoder macros
///
/// Write one field of `Move.0` with the layout on [`Move`]. Each macro ORs
/// the masked value into place and returns nothing. A caller writes only
/// the fields of the current format. The capture fields from bit 80 can
/// be written one at a time or as one record with `enc_capture_part!`.
///
/// enc_move_type! .. enc_captured_square!
///
///   Params:
///   - mv : &mut Move -> move to write
///   - val: u128      -> value for the field below
///
/// - enc_move_type!       : format tag, bits 0..2
/// - enc_piece!           : moving piece index, bits 3..12
/// - enc_start!           : origin square, bits 13..23
/// - enc_end!             : target square, bits 24..34
/// - enc_is_initial!      : first move flag, bit 35
/// - enc_promotion!       : promotion flag, bit 36
/// - enc_creates_enp!     : makes en passant flag, bit 37
/// - enc_promoted!        : promoted piece index, bits 38..47
/// - enc_created_enp!     : en passant descriptor, bits 48..79
/// - enc_is_unload!       : unload flag, bit 80
/// - enc_unload_square!   : unload square, bits 81..91
/// - enc_captured_piece!  : captured piece index, bits 92..101
/// - enc_captured_square! : captured square, bits 102..112
///
/// enc_capture_part!
///
///   Params:
///   - mv         : &mut Move -> move to write
///   - taken_piece: u128      -> full 35-bit capture record, bits 80..114
///
/// Notes:
/// There is no encoder for bits 113 and 114. The generator makes each
/// capture as a record, and `enc_capture_part!` shifts the record bits 33
/// and 34 to them.
///
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

/// Primary move decoder macros
///
/// Read one field of `Move.0`. A `PseudoMove` also works, because its
/// first field is the same word.
///
/// move_type! .. captured_square!
///
///   Params:
///   - mv: &Move -> move to read
///
///   Return:
///   u128        -> the field below
///
/// - move_type!       : format tag, bits 0..2
/// - piece!           : moving piece index, bits 3..12
/// - start!           : origin square, bits 13..23
/// - end!             : target square, bits 24..34
/// - is_initial!      : first move flag as 0 or 1, bit 35
/// - promoted!        : promoted piece index, bits 38..47
/// - created_enp!     : en passant descriptor, bits 48..79
/// - unload_square!   : unload square, bits 81..91
/// - captured_piece!  : captured piece index, bits 92..101
/// - captured_square! : captured square, bits 102..112
///
/// promotion! .. captured_own!, is_pass!
///
///   Params:
///   - mv: &Move -> move to read
///
///   Return:
///   bool        -> the flag below
///
/// - promotion!        : the move promotes, bit 36
/// - creates_enp!      : the move makes an en passant square, bit 37
/// - is_unload!        : the record puts a piece down, bit 80
/// - captured_unmoved! : the captured piece was unmoved, bit 113
/// - captured_own!     : the captured piece was an own piece, bit 114
/// - is_pass!          : a quiet move with equal start and end squares
///
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

/// Capture record decoder macros
///
/// Read one field of a 35-bit capture record in `Move.1`. The layout is on
/// [`Move`]. Make, undo and move output use them.
///
/// multi_move_unload_square! .. multi_move_captured_square!
///
///   Params:
///   - entry: u64 -> packed record to read
///
///   Return:
///   u64          -> the field below
///
/// - multi_move_unload_square!   : unload square, bits 1..11
/// - multi_move_captured_piece!  : captured piece index, bits 12..21
/// - multi_move_captured_square! : captured square, bits 22..32
///
/// multi_move_is_unload!, multi_move_captured_unmoved!,
/// multi_move_captured_own!
///
///   Params:
///   - entry: u64 -> packed record to read
///
///   Return:
///   bool         -> the flag below
///
/// - multi_move_is_unload!        : the record puts a piece down, bit 0
/// - multi_move_captured_unmoved! : the captured piece was unmoved, bit 33
/// - multi_move_captured_own!     : the captured piece was own, bit 34
///
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

/// Capture record encoder macros
///
/// Write one field of a capture record for `Move.1`. They are the inverse
/// of the `multi_move_*` readers. Each macro ORs the masked value into
/// place and returns nothing.
///
/// Params:
/// - entry: &mut u64 -> packed record to write
/// - val  : u64      -> value for the field below
///
/// - enc_multi_move_is_unload!        : unload flag, bit 0
/// - enc_multi_move_unload_square!    : unload square, bits 1..11
/// - enc_multi_move_captured_piece!   : captured piece index, bits 12..21
/// - enc_multi_move_captured_square!  : captured square, bits 22..32
/// - enc_multi_move_captured_unmoved! : captured unmoved flag, bit 33
/// - enc_multi_move_captured_own!     : captured own flag, bit 34
///
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
