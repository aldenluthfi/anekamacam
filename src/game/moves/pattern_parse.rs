//! # pattern_parse.rs
//!
//! Parses CPMN pattern expressions into allower and stopper pattern lists.
//!
//! CPMN encodes a neighbourhood rule as two multi-leg halves — allowers that
//! must be satisfied and stoppers that must not — each paired with the piece
//! set it applies to. This file compiles that text into the offset/piece-set
//! lists the matcher scans, expanding wildcards so later stages only ever see
//! concrete piece letters.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 24/02/2026

use crate::*;

lazy_static! {
    /// PATTERN_PATTERN
    ///
    /// Regex splitting a CPMN expression into its allower and stopper
    /// halves: `(.+)~(.+)` captures each half's multi-leg offsets and its
    /// piece list, and the `@`-delimited optional group captures the
    /// stopper half when present.
    pub static ref PATTERN_PATTERN: Regex =
        Regex::new("(.+)~(.+)@(?:(.+)~(.+))?").unwrap_or_else(|e| {
            panic!("Failed to compile PATTERN_PATTERN regex: {e}")
        });
}

/// expand_wildcard
///
/// Replaces the `*` piece wildcard in a pattern's piece lists with the
/// full alphabet of the variant's piece characters, so later parsing only
/// ever sees concrete piece letters.
///
/// Params:
/// - expr : &str   -> raw CPMN pattern expression
/// - state: &State -> supplies the variant's piece characters
///
/// Return:
/// String          -> the expression with wildcards spelled out
///
fn expand_wildcard(expr: &str, state: &State) -> String {
    let all_pieces = state.statics.pieces.iter()
        .map(|p| p.char).collect::<String>();

    let expr = expr.replace("~*", &format!("~{}", all_pieces));
    expr.replace("-*", &format!("-{}", all_pieces))
}

/// parse_pattern
///
/// The Cheesy Pattern Match Notation (CPMN) is as follows.
///
/// [allower multi leg]~[pieces]@[stoppers multi leg]~[pieces]
///
/// The multi-leg part is not processed as a whole; each leg is processed
/// separately. So `#-W~P-P` matches a pawn on the drop square and a pawn on
/// each `W` square. A drop is legal if all allowers are met and no stopper
/// is met.
///
/// Pieces are the chars of the pieces that are relevant to the allowers and
/// stoppers. * means all pieces excluding no piece. ? means no piece.
///
/// Params:
/// - expr : &str   -> one CPMN pattern expression
/// - state: &State -> piece dictionary and board dimensions
///
/// Return:
/// Pattern         -> compiled (allower, stopper) offset lists with piece sets
///
pub fn parse_pattern(expr: &str, state: &State) -> Pattern {
    let expr = &expand_wildcard(expr, state);
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

/// generate_stand_off_patterns
///
/// Parses a `|`-separated stand-off expression into executable patterns.
/// Each branch is parsed independently via `parse_pattern` and collected into a
/// single vector.
/// Empty expressions produce no patterns.
///
/// Params:
/// - expr : &str   -> `|`-separated stand-off expression from the config
/// - state: &State -> piece dictionary and board dimensions
///
/// Return:
/// Vec<Pattern>    -> one compiled pattern per branch
///
pub fn generate_stand_off_patterns(
    expr: &str,
    state: &State,
) -> Vec<Pattern> {
    if expr.is_empty() {
        return Vec::new();
    }

    let parts = expr.split('|').collect::<Vec<&str>>();
    parts.into_iter().map(|part| parse_pattern(part, state)).collect()
}

/// generate_relevant_stand_offs
///
/// Filters stand-off patterns to those that stay in bounds from `square`.
/// Offsets are checked with color-relative orientation so runtime matching
/// only evaluates geometrically possible patterns.
///
/// Params:
/// - piece          : &Piece        -> piece the patterns belong to
/// - square         : u32           -> origin square being precomputed
/// - state          : &State        -> board dimensions
/// - piece_stand_off: &[PatternSet] -> compiled patterns, one per piece
///
/// Return:
/// PatternSet                       -> patterns whose offsets all fit on the
///                                     board from here
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
