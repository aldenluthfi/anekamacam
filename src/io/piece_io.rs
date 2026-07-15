//! piece_io.rs
//!
//! Utilities for reading and writing piece data during engine setup.
//!
//! Covers symmetric piece pairing from the swap map, and encoding of dynamic
//! piece attributes (material values, classification flags) into their packed
//! bit representations used by the evaluator.
//!
//! Created: 26/05/2026
//! Author : Alden Luthfi
use crate::*;

/// collect_piece_type_pairs
///
/// Pairs each white piece type with its black counterpart through the
/// swap map, asserting the mapping is color-consistent. Derivation runs
/// on white pieces only and copies results onto these twins.
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

/// set_piece_dynamic_parameters
///
/// Packs derived evaluation attributes into a piece's dynamic word using
/// the layout documented on [`Piece`]: role flags in bits 0-1, opening
/// value in bits 2-15, endgame value in bits 16-29. Values must fit in
/// 14 bits; out-of-range values panic rather than silently truncate.
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
