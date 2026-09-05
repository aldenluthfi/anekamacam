//! pattern_parse.rs
//!
//! Parses CPMN pattern expressions into allower and stopper pattern lists.
//!
//! CPMN encodes a neighbourhood rule as two multi-leg halves — allowers that
//! must be satisfied and stoppers that must not — each paired with the piece
//! set it applies to. This file compiles that text into the offset/piece-set
//! lists the matcher scans, expanding wildcards so later stages only ever see
//! concrete piece letters.
//!
//! Created: 24/02/2026
//! Author : Alden Luthfi

use crate::*;

lazy_static! {
    /// PATTERN_PATTERN
    ///
    /// Regex splitting a CPMN expression into its allower and stopper
    /// halves, each half being offsets and piece groups either side of a
    /// `~`, and the two halves either side of an `@`:
    ///
    /// - group 1 -> allower offsets
    /// - group 2 -> allower piece groups
    /// - group 3 -> stopper offsets, absent when there are no stoppers
    /// - group 4 -> stopper piece groups, absent with them
    ///
    /// The `@` is not optional even where the stopper half is, so every
    /// expression carries the separator and a pattern that vetoes nothing
    /// still reads as one that could have.
    static ref PATTERN_PATTERN: Regex =
        Regex::new("(.+)~(.+)@(?:(.+)~(.+))?").unwrap_or_else(|e| {
            panic!("Failed to compile PATTERN_PATTERN regex: {e}")
        });
}

/// parse_pattern
///
/// Compiles one Cheesy Pattern Match Notation (CPMN) expression.
///
/// CPMN has this shape:
///
/// ```text
/// [allower multi leg]~[pieces]@[stoppers multi leg]~[pieces]
/// ```
///
/// Each half is a `-` separated list of offsets beside a `-` separated list
/// of piece groups, paired by position: the third offset answers to the
/// third group. Offsets borrow move notation, so `nW{2}~Kk@nW{..1}~*` says a
/// king stands two steps up and nothing stands on the square between. The
/// pattern matches when all allowers hold and no stopper holds.
///
/// Piece lists name the pieces relevant to allowers and stoppers. `*` means
/// every real piece and is spelled out into the variant's piece alphabet
/// before anything else runs, so every later stage sees only concrete piece
/// characters; `?` means the empty-square sentinel.
///
/// Params:
/// - expr : &str   -> one CPMN pattern expression
/// - state: &State -> piece dictionary and board dimensions
///
/// Return:
/// Pattern         -> allower and stopper [`PatternUnit`] lists
///
/// Notes:
/// Only the first leg of each compiled offset survives. The move compiler is
/// borrowed for its notation, not for its walking: a pattern names a square
/// relative to the anchor, so where a move would carry on through further
/// legs, a pattern has already arrived.
///
/// An expression that does not match [`PATTERN_PATTERN`], or that pairs a
/// list of offsets with no list of pieces, panics. Patterns come from a
/// variant's config file and are compiled once at load time, so a malformed
/// one is a broken variant rather than a position the engine can play on.
pub fn parse_pattern(expr: &str, state: &State) -> Pattern {
    let all_pieces = state.statics.pieces.iter()
        .map(|piece| piece.char).collect::<String>();
    let expr = &expr.replace("~*", &format!("~{}", all_pieces))
        .replace("-*", &format!("-{}", all_pieces));
    let captures = PATTERN_PATTERN
        .captures(expr)
        .unwrap_or_else(|| panic!("Invalid pattern format {}", expr));

    log_4!("parse_pattern captures: {:?}", captures);

    let (allowers, allower_pieces) = match (captures.get(1), captures.get(2)) {
        (Some(a), Some(p)) => (a.as_str(), p.as_str()),
        (None, None) => ("", ""),
        _ => panic!(concat!(
            "Invalid pattern format: ",
            "allowers and allower_pieces must ",
            "both be present or both absent"
        ),),
    };

    let allowers_pieces = allower_pieces
        .split('-')
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<&str>>();
    let allowers_vecs = if allowers.is_empty() {
        Vec::new()
    } else {
        allowers
            .split('-')
            .filter(|segment| !segment.is_empty())
            .enumerate()
            .flat_map(|(idx, compound)| {
                generate_move_vectors(compound, state)
                    .into_iter()
                    .map(move |vec| (idx, vec))
            })
            .collect()
    };

    log_4!("Parsed allowers: {:?}", allowers_vecs);

    let allower_result = allowers_vecs
        .iter()
        .map(|(index, multi_leg_vector)| {
            let leg = leg!(multi_leg_vector[0]);

            let x = x!(leg) as u16 & 0xFF;
            let y = y!(leg) as u16 & 0xFF;

            let mut piece_set = PieceSet::new();
            for piece_char in allowers_pieces[*index].chars() {
                let piece_index = if piece_char == '?' {
                    NO_PIECE as Square
                } else {
                    state.statics.piece_char_map[&piece_char] as u16
                };
                piece_set.insert(piece_index as PieceIndex);
            }

            ((y << 8) | x, piece_set)
        })
        .collect::<PatternAllower>();

    log_4!("Encoded allowers: {:?}", allower_result);

    let (stoppers, stopper_pieces) = match (captures.get(3), captures.get(4)) {
        (Some(s), Some(p)) => (s.as_str(), p.as_str()),
        (None, None) => ("", ""),
        _ => panic!(concat!(
            "Invalid drop format: ",
            "stoppers and stopper_pieces must ",
            "both be present or both absent"
        ),),
    };
    let stoppers_pieces = stopper_pieces
        .split('-')
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<&str>>();
    let stoppers_vecs = if stoppers.is_empty() {
        Vec::new()
    } else {
        stoppers
            .split('-')
            .filter(|segment| !segment.is_empty())
            .enumerate()
            .flat_map(|(idx, compound)| {
                generate_move_vectors(compound, state)
                    .into_iter()
                    .map(move |vec| (idx, vec))
            })
            .collect()
    };

    log_4!("Parsed drop: {:?}", stoppers_vecs);

    let stopper_result = stoppers_vecs
        .iter()
        .map(|(index, multi_leg_vector)| {
            let leg = leg!(multi_leg_vector[0]);

            let x = x!(leg) as u16 & 0xFF;
            let y = y!(leg) as u16 & 0xFF;

            let mut piece_set = PieceSet::new();
            for piece_char in stoppers_pieces[*index].chars() {
                let piece_index = if piece_char == '?' {
                    NO_PIECE as Square
                } else {
                    state.statics.piece_char_map[&piece_char] as u16
                };
                piece_set.insert(piece_index as PieceIndex);
            }

            ((y << 8) | x, piece_set)
        })
        .collect::<PatternStopper>();

    log_4!("Encoded stoppers: {:?}", stopper_result);

    (allower_result, stopper_result)
}

/*----------------------------------------------------------------------------*\
                           STAND-OFF RELEVANCE FILTER
\*----------------------------------------------------------------------------*/

/// generate_relevant_stand_offs
///
/// Keeps, for one piece standing on one square, the stand-off patterns whose
/// every offset still lands on the board from there. Offsets are mirrored by
/// the piece's colour first, so a pattern is judged in the orientation it
/// will be matched in rather than the one it was written in.
///
/// A pattern is kept whole or dropped whole: a half-visible neighbourhood
/// answers a different question than the one the variant asked. What survives
/// is what [`match_pattern!`] may index without a bounds check of its own.
///
/// Params:
/// - piece          : &Piece        -> piece the patterns belong to
/// - square         : u32           -> origin square being precomputed
/// - state          : &State        -> board dimensions
/// - piece_stand_off: &[PatternSet] -> compiled patterns, one per piece
///
/// Return:
/// PatternSet                       -> patterns that fit the board here
pub fn generate_relevant_stand_offs(
    piece: &Piece,
    square: u32,
    state: &State,
    piece_stand_off: &[PatternSet],
) -> PatternSet {
    let piece_color = p_color!(piece) as usize;

    let pattern_set = &piece_stand_off[p_index!(piece) as usize];
    let mut result = Vec::new();

    'outer: for pattern in pattern_set {
        let (allowers, stoppers) = pattern;
        let file = square % state.statics.files as u32;
        let rank = square / state.statics.files as u32;

        for allower in allowers {
            let x = x!(allower.0) as i32 * (-2 * piece_color as i32 + 1);
            let y = y!(allower.0) as i32 * (-2 * piece_color as i32 + 1);

            let check_x = file as i32 + x;
            let check_y = rank as i32 + y;

            if check_x < 0
                || check_x >= state.statics.files as i32
                || check_y < 0
                || check_y >= state.statics.ranks as i32
            {
                continue 'outer;
            }
        }

        for stopper in stoppers {
            let x = x!(stopper.0) as i32 * (-2 * piece_color as i32 + 1);
            let y = y!(stopper.0) as i32 * (-2 * piece_color as i32 + 1);

            let check_x = file as i32 + x;
            let check_y = rank as i32 + y;

            if check_x < 0
                || check_x >= state.statics.files as i32
                || check_y < 0
                || check_y >= state.statics.ranks as i32
            {
                continue 'outer;
            }
        }

        result.push(pattern.clone());
    }

    result
}
