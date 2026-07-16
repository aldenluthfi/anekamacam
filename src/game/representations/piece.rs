//! piece.rs
//!
//! Defines piece representation and properties.
//!
//! Every variant describes its army as a list of piece types, and the rest
//! of the engine refers to pieces only by index into that list. This file
//! defines the `Piece` record behind those indices: a bit-packed bundle of
//! the static attributes fixed by the config (identity, color, royalty,
//! rank) and the dynamic attributes derived at startup (material values,
//! role classes). Packing both into single words keeps the hot evaluation
//! and generation paths on cheap mask-and-shift reads.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi

/// PieceIndex
///
/// Index of a piece type in the variant's piece list, the engine's sole
/// runtime identity for a piece. The value 255 (`NO_PIECE`) is reserved
/// for "no piece" in mailbox boards and mapping tables.
pub type PieceIndex = u8;

/*----------------------------------------------------------------------------*\
                               UTILITY PIECE MACROS
\*----------------------------------------------------------------------------*/

/// p_value!
///
/// Returns the phase-interpolated material value of one piece type. Setup and
/// opening use the opening value, endgame uses the endgame value, and
/// middlegame linearly blends both values from the position phase score.
///
/// Params:
/// - piece_index: PieceIndex -> piece type whose value is queried
/// - state      : &State     -> position providing phase thresholds
///
/// Return:
/// u32                       -> interpolated material value
#[macro_export]
macro_rules! p_value {
    ($piece:expr, $state:expr) => {{
        let piece = &$state.statics.pieces[$piece as usize];

        let ovalue = p_ovalue!(piece) as u32;
        let evalue = p_evalue!(piece) as u32;

        let opening_score = $state.statics.opening_score;
        let endgame_score = $state.statics.endgame_score;
        let current_score =
            $state.phase_score.clamp(endgame_score, opening_score);

        match $state.game_phase {
            OPENING | SETUP => ovalue,
            ENDGAME => evalue,
            MIDDLEGAME => {
                (
                    (ovalue * (current_score - endgame_score)) +
                    (evalue * (opening_score - current_score))
                ) / (opening_score - endgame_score)
            }
            _ => unreachable!(),
        }
    }};
}

/*----------------------------------------------------------------------------*\
                        PIECE BITFIELD REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Piece bitfield accessor macros.
///
/// These decode `Piece::encoded_static` and `Piece::encoded_dynamic` into
/// readable attributes used throughout move generation and evaluation.
///
/// All take the same single parameter and read one field documented in
/// the bit layouts on [`Piece`].
///
/// Static accessors (encoded_static):
///
/// p_index!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   PieceIndex      -> piece type index (bits 0-7)
///
/// p_color!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   u8              -> owning side, 0 white / 1 black (bit 8)
///
/// p_can_promote!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   bool            -> whether the piece can promote (bit 9)
///
/// p_is_royal!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   bool            -> whether the piece must be mated (bit 10)
///
/// p_rank!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   u8              -> variant-defined capture rank (bits 11-18)
///
/// p_is_pawn!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   bool            -> pawn-like flag, set at derive time (bit 19)
///
/// Dynamic accessors (encoded_dynamic):
///
/// p_is_big!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   bool            -> big-piece role, royals excluded (bit 0)
///
/// p_is_major!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   bool            -> major-piece role, royals excluded (bit 1)
///
/// p_is_minor!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   bool            -> minor-piece role, royals excluded (bit 1 clear)
///
/// p_ovalue!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   u16             -> opening material value (bits 2-15)
///
/// p_evalue!
///
///   Params:
///   - piece: &Piece -> piece record read
///
///   Return:
///   u16             -> endgame material value (bits 16-29)
#[macro_export]
macro_rules! p_index {
    ($piece:expr) => {
        ($piece.encoded_static & 0xFF) as PieceIndex
    };
}

#[macro_export]
macro_rules! p_color {
    ($piece:expr) => {
        (($piece.encoded_static >> 8) & 1) as u8
    };
}

#[macro_export]
macro_rules! p_can_promote {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 9)) != 0
    };
}

#[macro_export]
macro_rules! p_is_royal {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 10)) != 0
    };
}

#[macro_export]
macro_rules! p_is_pawn {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 19)) != 0
    };
}

#[macro_export]
macro_rules! p_rank {
    ($piece:expr) => {
        (($piece.encoded_static >> 11) & 0xFF) as u8
    };
}

#[macro_export]
macro_rules! p_is_big {
    ($piece:expr) => {
        ($piece.encoded_dynamic & 1) != 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_is_major {
    ($piece:expr) => {
        ($piece.encoded_dynamic & (1 << 1)) != 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_is_minor {
    ($piece:expr) => {
        ($piece.encoded_dynamic & (1 << 1)) == 0 && !p_is_royal!($piece)
    };
}

#[macro_export]
macro_rules! p_ovalue {
    ($piece:expr) => {
        (($piece.encoded_dynamic >> 2) & 0x3FFF) as u16
    };
}

#[macro_export]
macro_rules! p_evalue {
    ($piece:expr) => {
        (($piece.encoded_dynamic >> 16) & 0x3FFF) as u16
    };
}

/// Piece
///
/// One configured piece type and its derived evaluation attributes.
///
/// Piece indices range from 0 through 254; 255 is reserved as `NO_PIECE`.
///
/// Static data (`encoded_static`) is encoded in 32 bits:
///
/// ```text
///   0               8 9 10                19                        31
///                         11                20
///   ┌───────────────┬─┬─┬─┬───────────────┬─┬────────────────────────┐
///   │     index     │c│p│r│     rank      │w│         unused         │
///   └───────────────┴─┴─┴─┴───────────────┴─┴────────────────────────┘
/// ```
///
/// - Bits 0..7     : piece index
/// - Bit 8         : color, 0 = White and 1 = Black
/// - Bit 9         : promotion capability
/// - Bit 10        : royal status
/// - Bits 11..18   : variant-defined rank
/// - Bit 19        : derived pawn-like status
/// - Bits 20..31   : unused
///
/// Dynamic data (`encoded_dynamic`) is encoded in 32 bits:
///
///   0 1 2                           16                          30  31
/// ```text
///   ┌─┬─┬───────────────────────────┬───────────────────────────┬────┐
///   │b│m│          opening          │          endgame          │ -- │
///   └─┴─┴───────────────────────────┴───────────────────────────┴────┘
/// ```
///
///
/// - Bit 0         : big-piece role
/// - Bit 1         : major-piece role; clear means minor when non-royal
/// - Bits 2..15    : 14-bit opening material value
/// - Bits 16..29   : 14-bit endgame material value
/// - Bits 30..31   : unused
///
/// The `promotions` field is a `Vec<u8>` that encodes the pieces this piece can
/// promote to.
#[derive(Clone)]
pub struct Piece {
    pub name: String,                                                           /* display name of the piece          */
    pub char: char,                                                             /* FEN / board letter                 */

    pub promotions: Vec<PieceIndex>,                                            /* piece types it can promote to      */
    pub encoded_static: u32,                                                    /* packed config attributes           */
    pub encoded_dynamic: u32,                                                   /* packed derived eval values         */
}

impl Piece {
    /// Piece::new
    ///
    /// Builds a piece type from its config-file attributes, packing them
    /// into `encoded_static` in the layout documented on [`Piece`]. The
    /// dynamic word starts zeroed and is filled in later, once parameter
    /// derivation has computed material values and role classes.
    ///
    /// Params:
    /// - name      : String          -> display name of the piece
    /// - char      : char            -> FEN/board letter for the piece
    /// - promotions: Vec<PieceIndex> -> piece types this can promote to
    /// - index     : u8              -> index in the variant's piece list
    /// - color     : u8              -> owning side (WHITE or BLACK)
    /// - is_royal  : bool            -> whether this piece must be mated
    /// - rank      : u8              -> variant-defined capture rank
    ///
    /// Return:
    ///
    /// Self
    /// the piece with static attributes encoded
    pub fn new(
        name: String,
        char: char,
        promotions: Vec<PieceIndex>,
        index: u8,
        color: u8,
        is_royal: bool,
        rank: u8,
    ) -> Self {
        let mut encoded_static = index as u32;
        encoded_static |= (color as u32) << 8;

        if !promotions.is_empty() {
            encoded_static |= 1 << 9;
        }

        if is_royal {
            encoded_static |= 1 << 10;
        }

        encoded_static |= (rank as u32) << 11;

        let encoded_dynamic = 0u32;

        Self {
            name,
            char,
            promotions,
            encoded_static,
            encoded_dynamic,
        }
    }
}
