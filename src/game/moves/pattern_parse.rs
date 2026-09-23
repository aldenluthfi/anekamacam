//! pattern_parse.rs
//!
//! Parses CPMN pattern expressions into allower and stopper lists.
//!
//! A CPMN rule has two halves. The allowers must match and the stoppers
//! must not match. Each half has offsets and piece sets. This file compiles
//! the text into the lists that the matcher reads. It also expands the `*`
//! wildcard into all piece letters, and fits the lists to each square.
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
    /// - group 1 -> allower offsets, absent when there are no allowers
    /// - group 2 -> allower piece groups, absent when there are no allowers
    /// - group 3 -> stopper offsets, absent when there are no stoppers
    /// - group 4 -> stopper piece groups, absent when there are no stoppers
    ///
    /// Notes:
    /// The `@` is mandatory, also when one half is empty.
    ///
    static ref PATTERN_PATTERN: Regex =
        Regex::new("^(?:(.+)~(.+))?@(?:(.+)~(.+))?$").unwrap_or_else(|e| {
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
/// up, and the square between is empty. Each half can be empty, thus
/// `@sW~G` has only a stopper.
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
                             PATTERN BOARD CLIPPING
\*----------------------------------------------------------------------------*/

/// clip_pattern
///
/// Fits one pattern to one square for one colour. The offsets are mirrored
/// for the colour:
///
/// - allower off the board : the pattern never matches, give `None`
/// - stopper off the board : the stopper never matches, remove it
///
/// Thus [`match_pattern!`] needs no bounds check on the result.
///
/// Params:
/// - pattern: &Pattern -> compiled pattern to fit
/// - square : u32      -> square at the origin of the pattern
/// - color  : u8       -> colour that sets the offset direction
/// - state  : &State   -> board dimensions
///
/// Return:
/// Option<Pattern>     -> the fitted pattern, `None` when it cannot match
///
pub fn clip_pattern(
    pattern: &Pattern,
    square: u32,
    color: u8,
    state: &State,
) -> Option<Pattern> {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let direction = -2 * color as i32 + 1;

    let on_board = |unit: &&PatternUnit| {
        let file = square as i32 % files + x!(unit.0) as i32 * direction;
        let rank = square as i32 / files + y!(unit.0) as i32 * direction;

        file >= 0 && file < files && rank >= 0 && rank < ranks
    };

    let (allowers, stoppers) = pattern;

    if !allowers.iter().all(|allower| on_board(&allower)) {
        return None;
    }

    let stoppers_on_board = stoppers.iter().filter(on_board).cloned();

    Some((allowers.clone(), stoppers_on_board.collect()))
}

/// clip_move_vector
///
/// Fits the condition of one move vector to its origin square with
/// [`clip_pattern`]. A vector without a condition stays the same.
///
/// Params:
/// - vector: &MoveVector -> move vector to fit
/// - square: u32         -> origin square of the vector
/// - color : u8          -> colour of the moving piece
/// - state : &State      -> board dimensions
///
/// Return:
/// Option<MoveVector>    -> the fitted vector, `None` when no pattern fits
///
pub fn clip_move_vector(
    vector: &MoveVector,
    square: u32,
    color: u8,
    state: &State,
) -> Option<MoveVector> {
    let Some(patterns) = &vector.pattern else {
        return Some(vector.clone());
    };

    let clipped = patterns
        .iter()
        .filter_map(|pattern| clip_pattern(pattern, square, color, state))
        .collect::<PatternSet>();

    if clipped.is_empty() {
        return None;
    }

    Some(MoveVector {
        legs: vector.legs.clone(),
        pattern: Some(Arc::new(clipped)),
    })
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
