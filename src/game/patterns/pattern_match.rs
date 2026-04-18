//! # pattern_match.rs
//!
//! Parses and evaluates CPMN patterns used by drop and stand-off rules.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 24/02/2026

use crate::*;

lazy_static! {
    pub static ref PATTERN_PATTERN: Regex =
        Regex::new("(.+)~(.+)@(?:(.+)~(.+))?").unwrap();
    pub static ref CHAIN_PATTERN: Regex = Regex::new(r"([^-]+)").unwrap();
}

/// Represents a compressed set of allowed or stopper pieces.
///
/// This structure provides O(1) membership checks to eliminate dynamic
/// mapping overhead during pattern matching. Memory overhead is strictly
/// bounded to 256 booleans, naturally fitting the maximum piece limit.
#[derive(Clone)]
pub struct PieceSet([bool; 256]);

impl Default for PieceSet {
    fn default() -> Self {
        Self([false; 256])
    }
}

impl PieceSet {
    /// Creates a new, empty piece set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a piece index to the set.
    pub fn insert(&mut self, piece: u8) {
        self.0[piece as usize] = true;
    }

    /// Returns `true` if the given piece index is in the set.
    pub fn contains(&self, piece: u8) -> bool {
        self.0[piece as usize]
    }
}

#[cfg(debug_assertions)]
impl std::fmt::Debug for PieceSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pieces = Vec::new();
        for i in 0..256 {
            if self.0[i] {
                pieces.push(i as u8);
            }
        }
        write!(f, "PieceSet({:?})", pieces)
    }
}

/// Represents one relative pattern offset with its allowed piece set.
///
/// The `u16` packs `(x, y)` displacement, and the `PieceSet` stores piece
/// indices accepted at that offset during drop/stand-off matching.
/// This compact unit is shared by allower and stopper pattern lists.
pub type PatternUnit = (u16, PieceSet);
pub type PatternAllower = Vec<PatternUnit>;
pub type PatternStopper = Vec<PatternUnit>;
pub type Pattern = (PatternAllower, PatternStopper);
pub type PatternSet = Vec<Pattern>;

fn expand_wildcard(expr: &str, state: &State) -> String {
    let all_pieces = state.pieces.iter().map(|p| p.char).collect::<String>();

    let expr = expr.replace("~*", &format!("~{}", all_pieces));
    expr.replace("-*", &format!("-{}", all_pieces))
}

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
pub fn parse_pattern(expr: &str, state: &State) -> Pattern {
    let expr = &expand_wildcard(expr, state);
    let captures = PATTERN_PATTERN
        .captures(expr)
        .unwrap_or_else(|| panic!("Invalid pattern format {}", expr));

    #[cfg(debug_assertions)]
    println!("[DEBUG] parse_pattern captures: {:?}", captures);

    let (allowers, allower_pieces) = match (captures.get(1), captures.get(2)) {
        (Some(a), Some(p)) => (a.as_str(), p.as_str()),
        (None, None) => ("", ""),
        _ => panic!(concat!(
            "Invalid pattern format: ",
            "allowers and allower_pieces must ",
            "both be present or both absent"
        ),),
    };

    let allowers_pieces = CHAIN_PATTERN
        .captures_iter(allower_pieces)
        .map(|cap| cap.get(1).unwrap().as_str())
        .collect::<Vec<&str>>();
    let allowers_vecs = if allowers.is_empty() {
        Vec::new()
    } else {
        CHAIN_PATTERN
            .captures_iter(allowers)
            .enumerate()
            .flat_map(|(idx, cap)| {
                let compound = cap.get(1).unwrap().as_str();
                generate_move_vectors(compound, state)
                    .into_iter()
                    .map(move |vec| (idx, vec))
            })
            .collect()
    };

    #[cfg(debug_assertions)]
    println!("[DEBUG] Parsed allowers: {:#?}", allowers_vecs);

    let allower_result = allowers_vecs
        .iter()
        .map(|(index, multi_leg_vector)| {
            let leg = leg!(multi_leg_vector[0]);

            let x = x!(leg) as u16 & 0xFF;
            let y = y!(leg) as u16 & 0xFF;

            let mut piece_set = PieceSet::new();
            for piece_char in allowers_pieces[*index].chars() {
                let piece_index = if piece_char == '?' {
                    NO_PIECE as u16
                } else {
                    state.piece_char_map[&piece_char] as u16
                };
                piece_set.insert(piece_index as u8);
            }

            ((y << 8) | x, piece_set)
        })
        .collect::<PatternAllower>();

    #[cfg(debug_assertions)]
    println!("[DEBUG] Encoded allowers: {:?}", allower_result);

    let (stoppers, stopper_pieces) = match (captures.get(3), captures.get(4)) {
        (Some(s), Some(p)) => (s.as_str(), p.as_str()),
        (None, None) => ("", ""),
        _ => panic!(concat!(
            "Invalid drop format: ",
            "stoppers and stopper_pieces must ",
            "both be present or both absent"
        ),),
    };
    let stoppers_pieces = CHAIN_PATTERN
        .captures_iter(stopper_pieces)
        .map(|cap| cap.get(1).unwrap().as_str())
        .collect::<Vec<&str>>();
    let stoppers_vecs = if stoppers.is_empty() {
        Vec::new()
    } else {
        CHAIN_PATTERN
            .captures_iter(stoppers)
            .enumerate()
            .flat_map(|(idx, cap)| {
                let compound = cap.get(1).unwrap().as_str();
                generate_move_vectors(compound, state)
                    .into_iter()
                    .map(move |vec| (idx, vec))
            })
            .collect()
    };

    #[cfg(debug_assertions)]
    println!("[DEBUG] Parsed drop: {:#?}", stoppers_vecs);

    let stopper_result = stoppers_vecs
        .iter()
        .map(|(index, multi_leg_vector)| {
            let leg = leg!(multi_leg_vector[0]);

            let x = x!(leg) as u16 & 0xFF;
            let y = y!(leg) as u16 & 0xFF;

            let mut piece_set = PieceSet::new();
            for piece_char in stoppers_pieces[*index].chars() {
                let piece_index = if piece_char == '?' {
                    NO_PIECE as u16
                } else {
                    state.piece_char_map[&piece_char] as u16
                };
                piece_set.insert(piece_index as u8);
            }

            ((y << 8) | x, piece_set)
        })
        .collect::<PatternStopper>();

    #[cfg(debug_assertions)]
    println!("[DEBUG] Encoded stoppers: {:?}", stopper_result);

    (allower_result, stopper_result)
}

/// Matches a parsed pattern against a square using the given color viewpoint.
///
/// If a concrete piece context exists, use that piece's color for orientation;
/// otherwise use White's orientation as the default perspective.
/// Returns `true` only when all allowers pass and no stopper matches.
pub fn match_pattern(
    pattern: &Pattern,
    square: u32,
    color: u8,
    state: &State,
) -> bool {
    let (allowers, stoppers) = pattern;

    let file = square % state.files as u32;
    let rank = square / state.files as u32;

    for allower in allowers {
        let x = x!(allower.0) as i32 * (-2 * color as i32 + 1);
        let y = y!(allower.0) as i32 * (-2 * color as i32 + 1);
        let allower_pieces = &allower.1;

        let check_x = file as i32 + x;
        let check_y = rank as i32 + y;
        let check_index = (check_y * state.files as i32 + check_x) as usize;

        let piece_check = state.main_board[check_index];

        if !allower_pieces.contains(piece_check) {
            return false;
        }
    }

    for stopper in stoppers {
        let x = x!(stopper.0) as i32 * (-2 * color as i32 + 1);
        let y = y!(stopper.0) as i32 * (-2 * color as i32 + 1);
        let stopper_pieces = &stopper.1;

        let check_x = file as i32 + x;
        let check_y = rank as i32 + y;
        let check_index = (check_y * state.files as i32 + check_x) as usize;

        let piece_check = state.main_board[check_index];

        if stopper_pieces.contains(piece_check) {
            return false;
        }
    }

    true
}

/// Parses a `|`-separated stand-off expression into executable patterns.
///
/// Each branch is parsed independently via `parse_pattern` and collected into a
/// single vector.
/// Empty expressions produce no patterns.
pub fn generate_stand_off_patterns(expr: &str, state: &State) -> Vec<Pattern> {
    if expr.is_empty() {
        return Vec::new();
    }

    let parts = expr.split('|').collect::<Vec<&str>>();
    parts.into_iter().map(|part| parse_pattern(part, state)).collect()
}

/// Filters stand-off patterns to those that stay in bounds from `square`.
///
/// Offsets are checked with color-relative orientation so runtime matching
/// only evaluates geometrically possible patterns.
pub fn generate_relevant_stand_offs(
    piece: &Piece,
    square: u32,
    state: &State,
    piece_stand_off: &[PatternSet],
) -> Vec<Pattern> {
    let piece_color = p_color!(piece) as usize;

    let pattern_set = &piece_stand_off[p_index!(piece) as usize];
    let mut result = Vec::new();

    'outer: for pattern in pattern_set {
        let (allowers, stoppers) = pattern;
        let file = square % state.files as u32;
        let rank = square / state.files as u32;

        for allower in allowers {
            let x = x!(allower.0) as i32 * (-2 * piece_color as i32 + 1);
            let y = y!(allower.0) as i32 * (-2 * piece_color as i32 + 1);

            let check_x = file as i32 + x;
            let check_y = rank as i32 + y;

            if check_x < 0
                || check_x >= state.files as i32
                || check_y < 0
                || check_y >= state.ranks as i32
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
                || check_x >= state.files as i32
                || check_y < 0
                || check_y >= state.ranks as i32
            {
                continue 'outer;
            }
        }

        result.push(pattern.clone());
    }

    result
}

/// Evaluates whether the current position contains any active
/// stand-off pattern.
///
/// Iterates every piece instance on the board, checks precomputed stand-off
/// candidate patterns for that piece-square pair, and returns `true` as soon as
/// one pattern matches.
#[macro_export]
macro_rules! is_in_stand_off {
    ($state:expr) => {{
        let mut found = false;

        'main: for (index, position) in $state.piece_list.iter().enumerate() {
            for &square in position {
                for pattern in
                    &$state.relevant_stand_offs[index][square as usize]
                {
                    if match_pattern(
                        pattern,
                        square as u32,
                        p_color!($state.pieces[index]),
                        &$state,
                    ) {
                        found = true;
                        break 'main;
                    }
                }
            }
        }

        found
    }};
}
