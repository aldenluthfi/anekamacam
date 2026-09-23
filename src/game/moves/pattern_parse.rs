//! pattern_parse.rs
//!
//! Parses CPMN pattern expressions into allower and stopper lists.
//!
//! A CPMN rule has two halves. The allowers must match and the stoppers
//! must not match. Each half has offsets and piece sets. This file compiles
//! the text into the lists that the matcher reads. It also expands the `*`
//! wildcard into all piece letters.
//!
//! Created: 24/02/2026
//! Author : Alden Luthfi

use crate::*;

lazy_static! {
    /// PATTERN_PATTERN
    ///
    /// Regex that splits a CPMN expression at the `@` into the allower and
    /// stopper halves. A `~` splits each half into offsets and piece groups.
    ///
    /// - group 1 -> allower offsets
    /// - group 2 -> allower piece groups
    /// - group 3 -> stopper offsets, absent when there are no stoppers
    /// - group 4 -> stopper piece groups, absent when there are no stoppers
    ///
    /// Notes:
    /// The `@` is mandatory, also when the stopper half is empty.
    ///
    static ref PATTERN_PATTERN: Regex =
        Regex::new("(.+)~(.+)@(?:(.+)~(.+))?").unwrap_or_else(|e| {
            panic!("Failed to compile PATTERN_PATTERN regex: {e}")
        });
}

/// parse_pattern
///
/// Compiles one Cheesy Pattern Match Notation (CPMN) expression into an
/// allower list and a stopper list. CPMN has this shape:
///
/// ```text
/// [allower multi leg]~[pieces]@[stoppers multi leg]~[pieces]
/// ```
///
/// Each half has a `-` separated list of offsets and a `-` separated list
/// of piece groups. The lists pair by position. The offsets use the move
/// notation. For example, `nW{2}~Kk@nW{..1}~*` means: a king is two steps
/// up, and the square between is empty.
///
/// - `*` : all pieces of the variant, expanded before the parse
/// - `?` : an empty square
///
/// Params:
/// - expr : &str   -> one CPMN pattern expression
/// - state: &State -> piece dictionary and board dimensions
///
/// Return:
/// Pattern         -> allower and stopper [`PatternUnit`] lists
///
/// Notes:
/// The function keeps only the first leg of each compiled offset, because
/// a pattern names one square. A bad expression causes a panic at load
/// time.
///
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
/// Keeps the stand-off patterns of a piece on one square where all offsets
/// stay on the board. The offsets are mirrored for the colour of the piece.
///
/// If one offset goes off the board, the function removes the full pattern.
/// Thus [`match_pattern!`] needs no bounds check.
///
/// Params:
/// - piece          : &Piece        -> piece type of the patterns
/// - square         : u32           -> origin square
/// - state          : &State        -> board dimensions
/// - piece_stand_off: &[PatternSet] -> compiled patterns, one for each piece
///
/// Return:
/// PatternSet                       -> patterns that fit on the board
///
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
