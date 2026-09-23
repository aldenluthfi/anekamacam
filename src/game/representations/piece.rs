//! piece.rs
//!
//! Defines the piece type and its properties.
//!
//! Each variant has a list of piece types. The engine refers to a piece
//! only by its index in this list. This file defines the `Piece` record.
//! One packed word has the static data from the config. A second packed
//! word has the values that derivation calculates at startup.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi

/// PieceIndex
///
/// Index of a piece type in the piece list of the variant. The top value,
/// `NO_PIECE`, means "no piece" in mailbox boards and mapping tables.
///
/// Notes:
/// A packed word has only 10 bits for an index. `NO_PIECE` is never in a
/// packed word, so the wider type is safe. `MAX_PIECES` is the limit for a
/// config, not for the type.
///
pub type PieceIndex = u16;

/*----------------------------------------------------------------------------*\
                              UTILITY PIECE MACROS
\*----------------------------------------------------------------------------*/

/// p_value!
///
/// Gives the material value of one piece type for the current phase.
///
/// - setup, opening : the opening value
/// - endgame        : the endgame value
/// - middlegame     : a linear blend of the two, from the phase score
///
/// Params:
/// - piece_index: PieceIndex -> piece type to value
/// - state      : &State     -> position with the phase thresholds
///
/// Return:
/// u32                       -> interpolated material value
///
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

/// Piece bitfield accessors
///
/// Read one field of `Piece::encoded_static` or `Piece::encoded_dynamic`.
/// The bit layouts are on [`Piece`].
///
/// Static fields (encoded_static):
///
/// p_index!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   PieceIndex      -> piece type index (bits 0-9)
///
/// p_color!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   u8              -> side, 0 white and 1 black (bit 10)
///
/// p_can_promote!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   bool            -> true when the piece can promote (bit 11)
///
/// p_is_royal!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   bool            -> true when the piece is royal (bit 12)
///
/// p_rank!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   u8              -> rank that the variant defines (bits 13-20)
///
/// Dynamic fields (encoded_dynamic):
///
/// p_is_big!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   bool            -> big piece role, false for royals (bit 0)
///
/// p_is_major!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   bool            -> major piece role, false for royals (bit 1)
///
/// p_is_minor!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   bool            -> minor piece role, false for royals (bit 1 clear)
///
/// p_ovalue!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   u16             -> opening material value (bits 2-15)
///
/// p_evalue!
///
///   Params:
///   - piece: &Piece -> piece to read
///
///   Return:
///   u16             -> endgame material value (bits 16-29)
///
#[macro_export]
macro_rules! p_index {
    ($piece:expr) => {
        ($piece.encoded_static & 0x3FF) as PieceIndex
    };
}

#[macro_export]
macro_rules! p_color {
    ($piece:expr) => {
        (($piece.encoded_static >> 10) & 1) as u8
    };
}

#[macro_export]
macro_rules! p_can_promote {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 11)) != 0
    };
}

#[macro_export]
macro_rules! p_is_royal {
    ($piece:expr) => {
        ($piece.encoded_static & (1 << 12)) != 0
    };
}

#[macro_export]
macro_rules! p_rank {
    ($piece:expr) => {
        (($piece.encoded_static >> 13) & 0xFF) as u8
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
/// One piece type from the config and its derived evaluation values. A
/// variant can have a maximum of `MAX_PIECES` piece types.
///
/// Static data (`encoded_static`) has 32 bits:
///
/// ```text
///   0                  10 11 13              21                    31
///                         12
///   ┌──────────────────┬─┬─┬─┬───────────────┬────────────────────────┐
///   │       index      │c│p│r│     rank      │         unused         │
///   └──────────────────┴─┴─┴─┴───────────────┴────────────────────────┘
/// ```
///
/// - Bits 0..9     : piece index
/// - Bit 10        : colour, 0 = White and 1 = Black
/// - Bit 11        : piece can promote
/// - Bit 12        : piece is royal
/// - Bits 13..20   : rank that the variant defines
/// - Bits 21..31   : unused
///
/// Dynamic data (`encoded_dynamic`) has 32 bits:
///
/// ```text
///   0 1 2                           16                          30  31
///   ┌─┬─┬───────────────────────────┬───────────────────────────┬────┐
///   │b│m│          opening          │          endgame          │ ·· │
///   └─┴─┴───────────────────────────┴───────────────────────────┴────┘
/// ```
///
/// - Bit 0         : big piece role
/// - Bit 1         : major piece role, clear means minor if not royal
/// - Bits 2..15    : 14-bit opening material value
/// - Bits 16..29   : 14-bit endgame material value
/// - Bits 30..31   : unused
///
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
    /// Makes a piece type from its config data and packs it into
    /// `encoded_static`. The dynamic word starts at zero. Derivation
    /// writes it later.
    ///
    /// Params:
    /// - name      : String          -> display name of the piece
    /// - char      : char            -> FEN and board letter of the piece
    /// - promotions: Vec<PieceIndex> -> piece types it can promote to
    /// - index     : PieceIndex      -> index in the piece list
    /// - color     : u8              -> side, WHITE or BLACK
    /// - is_royal  : bool            -> true when the piece is royal
    /// - rank      : u8              -> rank that the variant defines
    ///
    /// Return:
    /// Self                          -> piece with the packed static word
    ///
    /// Notes:
    /// Bit 11 is set when `promotions` is not empty, so the flag and the
    /// list always agree.
    ///
    pub fn new(
        name: String,
        char: char,
        promotions: Vec<PieceIndex>,
        index: PieceIndex,
        color: u8,
        is_royal: bool,
        rank: u8,
    ) -> Self {
        let mut encoded_static = index as u32;
        encoded_static |= (color as u32) << 10;

        if !promotions.is_empty() {
            encoded_static |= 1 << 11;
        }

        if is_royal {
            encoded_static |= 1 << 12;
        }

        encoded_static |= (rank as u32) << 13;

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
