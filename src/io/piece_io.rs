//! piece_io.rs
//!
//! Reads and writes piece data during engine setup.
//!
//! Derivation calculates values for white pieces and copies them to the
//! black pieces. This file finds the white and black pair of each piece
//! type. It also packs values and role flags into the piece word.
//!
//! Created: 26/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                               PIECE TYPE PAIRING
\*----------------------------------------------------------------------------*/

/// collect_piece_type_pairs
///
/// Pairs each white piece type with its black piece type through the swap
/// map. Derivation calculates the white piece and copies to the black one.
///
/// Params:
/// - state: &State     -> variant that has the swap map
///
/// Return:
/// Vec<(usize, usize)> -> (white index, black index) for each piece type
///
/// Notes:
/// The function asserts that each pair has a black piece. A bad swap entry
/// would make derivation write over white values without an error.
///
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
/// Packs derived evaluation values into the dynamic word of a piece. The
/// layout is on [`Piece`]. All derived and tuned values use this function.
///
/// - bits 0 to 29 : written, the two role flags and two material values
/// - bits 30 up   : kept as they are
///
/// Params:
/// - piece   : &mut Piece -> piece whose dynamic word is rewritten
/// - ovalue  : u16        -> derived opening value (14-bit)
/// - evalue  : u16        -> derived endgame value (14-bit)
/// - is_big  : bool       -> big-piece role flag
/// - is_major: bool       -> major-piece role flag
///
/// Notes:
/// A value wider than 14 bits causes a panic. A truncated value would give
/// a wrong material value without an error.
///
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
