//! # piece_io.rs
//!
//! Utilities for reading and writing piece data during engine setup.
//!
//! Covers symmetric piece pairing from the swap map, and encoding of dynamic
//! piece attributes (material values, classification flags) into their packed
//! bit representations used by the evaluator.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 26/05/2026
use crate::*;

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
