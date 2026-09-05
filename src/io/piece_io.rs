//! piece_io.rs
//!
//! Utilities for reading and writing piece data during engine setup.
//!
//! Derivation prices one colour and hands the answer to the other, so it needs
//! to know which piece is whose twin and where a derived number goes once it
//! has one. This file answers both: it walks the swap map into colour pairs,
//! and it packs values and role flags into the word the evaluator reads.
//!
//! Created: 26/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                               PIECE TYPE PAIRING
\*----------------------------------------------------------------------------*/

/// collect_piece_type_pairs
///
/// Pairs each white piece type with its black counterpart through the swap
/// map. Derivation runs over white pieces alone and copies every answer onto
/// the twin, so a variant is priced once and read by both sides.
///
/// ```text
/// white index   the piece derivation actually looks at
/// black index   the swap map's answer for it, asserted to be black
/// ```
///
/// The colour assertion is not a sanity check on this walk but on the map:
/// a swap entry pointing at the wrong colour would have derivation write
/// white's values over white's own, and no later stage would notice.
///
/// Params:
/// - state: &State     -> variant whose swap map is walked
///
/// Return:
/// Vec<(usize, usize)> -> (white index, black index) per piece type
pub fn collect_piece_type_pairs(state: &State) -> Vec<(usize, usize)> {
    let mut type_pairs = Vec::new();

    for (white_idx, piece) in state.statics.pieces.iter().enumerate() {
        if p_color!(piece) != WHITE {
            continue;
        }

        let black_idx = state.statics.piece_swap_map[white_idx] as usize;

        assert!(
            p_color!(state.statics.pieces[black_idx]) == BLACK,
            "Invalid black counterpart mapping for white piece index {}",
            white_idx
        );

        type_pairs.push((white_idx, black_idx));
    }

    assert!(!type_pairs.is_empty(), "No white piece representatives found");

    type_pairs
}

/*----------------------------------------------------------------------------*\
                           DYNAMIC ATTRIBUTE PACKING
\*----------------------------------------------------------------------------*/

/// set_piece_dynamic_parameters
///
/// Packs derived evaluation attributes into a piece's dynamic word, using the
/// layout documented on [`Piece`]. Every writer of a derived value goes
/// through here, whether the value was derived from the rules or read out of
/// a tuned payload, so the packing is written down exactly once.
///
/// ```text
/// rewritten   bits 0 to 29, both role flags and both material values
/// preserved   bits 30 and up, whatever the word already carried
/// ```
///
/// A value wider than fourteen bits panics rather than truncating. A silently
/// wrapped material value would leave a piece cheaper than a pawn while every
/// table built on top of it stayed perfectly consistent, which is the kind of
/// fault that survives a whole tournament unnoticed.
///
/// Params:
/// - piece   : &mut Piece -> piece whose dynamic word is rewritten
/// - ovalue  : u16        -> derived opening value (14-bit)
/// - evalue  : u16        -> derived endgame value (14-bit)
/// - is_big  : bool       -> big-piece role flag
/// - is_major: bool       -> major-piece role flag
pub fn set_piece_dynamic_parameters(
    piece: &mut Piece,
    ovalue: u16,
    evalue: u16,
    is_big: bool,
    is_major: bool,
) {
    assert!(
        ovalue <= 0x3FFF,
        "Opening piece value out of 14-bit range: {}",
        ovalue
    );
    assert!(
        evalue <= 0x3FFF,
        "Endgame piece value out of 14-bit range: {}",
        evalue
    );

    let mut dynamic_bits = 0u32;

    if is_big {
        dynamic_bits |= 1;
    }

    if is_major {
        dynamic_bits |= 1 << 1;
    }

    dynamic_bits |= (ovalue as u32 & 0x3FFF) << 2;
    dynamic_bits |= (evalue as u32 & 0x3FFF) << 16;

    piece.encoded_dynamic =
        (piece.encoded_dynamic & !((1u32 << 30) - 1)) | dynamic_bits;
}
