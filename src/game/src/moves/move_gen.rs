//! # move_gen.rs
//!
//! Implements move matching and validation against board state.
//!
//! This file contains functionality for determining whether a parsed move
//! expression is possible given the current board state. It validates moves
//! by checking against enemy pieces, friendly pieces, and king positions
//! using bitboard representations. The module uses parallel processing to
//! efficiently check multiple move alternatives.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use lazy_static::lazy_static;
use rayon::iter::ParallelIterator;
use regex::Regex;
use std::collections::{HashMap, HashSet};

use crate::constants::{MAX_FILES, MAX_RANKS};
use crate::moves::move_parse::parse_move;
use crate::representations::board::Board;
use crate::util::split_and_process;
use crate::representations::state::State;

lazy_static!{
    pub static ref ATOMIC_PARTS: Regex = Regex::new(
        r"^(n|ne|e|se|s|sw|w|nw)?(\[[\d]*\])?K$"
    ).unwrap();
    pub static ref ATOMIC: Regex = Regex::new(
        r"((?:n|ne|e|se|s|sw|w|nw)?(?:\[[\d]*\])?K)"
    ).unwrap();
    pub static ref LEG_PATTERN: Regex = Regex::new(
        r""
    ).unwrap();
    pub static ref CARDINAL_VECTORS: [(i8, i8); 8] = [
        ( 0,  1),                                                               /* n,ne,e,se,s,sw,w,nw                */
        ( 1,  1),
        ( 1,  0),
        ( 1, -1),
        ( 0, -1),
        (-1, -1),
        (-1,  0),
        (-1,  1)
    ];
    pub static ref DIRECTION_VECTOR_SETS:
        HashMap<&'static str, HashSet<(i8, i8)>> = {
        let mut m = HashMap::new();
        m.insert("n", HashSet::from([
            (-1,  1),
            ( 0,  1),
            ( 1,  1),
        ]));
        m.insert("e", HashSet::from([
            ( 1,  1),
            ( 1,  0),
            ( 1, -1),
        ]));
        m.insert("s", HashSet::from([
            ( 1, -1),
            ( 0, -1),
            (-1, -1),
        ]));
        m.insert("w", HashSet::from([
            (-1, -1),
            (-1,  0),
            (-1,  1),
        ]));
        m.insert("ne", HashSet::from([
            ( 1,  1),
        ]));
        m.insert("se", HashSet::from([
            ( 1, -1),
        ]));
        m.insert("sw", HashSet::from([
            (-1, -1),
        ]));
        m.insert("nw", HashSet::from([
            (-1,  1),
        ]));
        m
    };
    pub static ref CARDINAL_MAP: HashMap<&'static str, u32> = {
        let mut m = HashMap::new();
        m.insert("n", 0);
        m.insert("ne", 1);
        m.insert("e", 2);
        m.insert("se", 3);
        m.insert("s", 4);
        m.insert("sw", 5);
        m.insert("w", 6);
        m.insert("nw", 7);
        m
    };
    pub static ref INVERSE_CARDINAL_MAP: HashMap<usize, &'static str> = {
        let mut m = HashMap::new();
        m.insert(0, "n");
        m.insert(1, "ne");
        m.insert(2, "e");
        m.insert(3, "se");
        m.insert(4, "s");
        m.insert(5, "sw");
        m.insert(6, "w");
        m.insert(7, "nw");
        m
    };
    pub static ref VECTOR_MAP: HashMap<(i8, i8), usize> = {
        let mut m = HashMap::new();
        m.insert(( 0,  1), 0);
        m.insert(( 1,  1), 1);
        m.insert(( 1,  0), 2);
        m.insert(( 1, -1), 3);
        m.insert(( 0, -1), 4);
        m.insert((-1, -1), 5);
        m.insert((-1,  0), 6);
        m.insert((-1,  1), 7);
        m
    };
    pub static ref INVERSE_VECTOR_MAP: HashMap<usize, (i8, i8)> = {
        let mut m = HashMap::new();
        m.insert(0, ( 0,  1));
        m.insert(1, ( 1,  1));
        m.insert(2, ( 1,  0));
        m.insert(3, ( 1, -1));
        m.insert(4, ( 0, -1));
        m.insert(5, (-1, -1));
        m.insert(6, (-1,  0));
        m.insert(7, (-1,  1));
        m
    };
}

fn match_move(expr: &str, start: u16, end: u16, state: &State) -> Option<bool> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before matching.");

    Some(false)
}


/// Returns a vector of (x, y) tuples representing the directions of an atomic
/// with respect to the given rotation.
///
/// # Direction Numbering System
///
/// Directions are numbered 1-8 in clockwise order starting from north:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 08 │ 01 │ 02 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 07 │    │ 03 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 06 │ 05 │ 04 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
/// ```text
/// Mapping:
/// - 1: n  (north)      - vector (0, 1)
/// - 2: ne (northeast)  - vector (1, 1)
/// - 3: e  (east)       - vector (1, 0)
/// - 4: se (southeast)  - vector (1, -1)
/// - 5: s  (south)      - vector (0, -1)
/// - 6: sw (southwest)  - vector (-1, -1)
/// - 7: w  (west)       - vector (-1, 0)
/// - 8: nw (northwest)  - vector (-1, 1)
/// ```
///
/// # Examples
///
/// ```ignore
/// // K with no rotation: all 8 directions
/// atomic_to_vector("K", "n");
/// ```
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 08 │ 01 │ 02 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 07 │    │ 03 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 06 │ 05 │ 04 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// ```ignore
/// // nK with no rotation: only (0, 1), (1, 1), (-1, 1)
/// atomic_to_vector("nK", "n");
/// ```
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 08 │ 01 │ 02 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// ```ignore
/// // n[2468]K with rotation towards "ne": (0, 1), (1, 1)
/// atomic_to_vector("n[2468]K", "ne");
/// ```
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ 08 │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │ 02 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// An atomic is defined as having only one optional cardinal direction, an
/// optional range, and ending with K.
///
/// All inputs are sanitized by `move_parse::parse_move` before being passed
/// here.
fn atomic_to_vector(expr: &str, rotation: &str) -> Vec<(i8, i8)> {
    let mut set = HashSet::new();

    let cap = ATOMIC_PARTS.captures(expr)
        .expect(
            &format!("Invalid atomic expression: {}", expr)
        );

    let direction = cap.get(1).map(|m| m.as_str());
    let range = cap.get(2).map(|m| m.as_str());
    let rotation_index: u32 = *CARDINAL_MAP.get(rotation).unwrap_or(&0);

    if let Some(range_str) = range {
        let digits = &range_str[1..range_str.len()-1];
        for digit_char in digits.chars() {
            if let Some(digit) = digit_char.to_digit(10) {
                let index = (digit - 1 + rotation_index) as usize;
                set.insert(CARDINAL_VECTORS[index % CARDINAL_VECTORS.len()]);
            }
        }
    } else {
        for i in 0..CARDINAL_VECTORS.len() {
            let index = (i + rotation_index as usize) % CARDINAL_VECTORS.len();
            set.insert(CARDINAL_VECTORS[index]);
        }
    }

    if let Some(dir) = direction {
        if let Some(direction_set) = DIRECTION_VECTOR_SETS.get(dir) {
            let rotated_direction_set: HashSet<(i8, i8)> = direction_set.iter()
                .map(|(x, y)| {
                    let vec_index = *VECTOR_MAP.get(&(*x, *y)).unwrap();
                    let rotated_index = {
                        (
                            vec_index + rotation_index as usize
                        ) % CARDINAL_VECTORS.len()
                    };
                    CARDINAL_VECTORS[rotated_index]
                })
                .collect();
            set.intersection(&rotated_direction_set).copied().collect()
        } else {
            set.into_iter().collect()
        }
    } else {
        set.into_iter().collect()
    }

}

/// Returns a vector containing a list of 2 tuples:
/// - The first tuple represents the whole vector of the move.
/// - The second tuple represents the last applied vector of the move.
///
/// A chained atomic is defined as multiple atomics (see
/// `move_gen::atomic_to_vector` for atomic definition) concatenated together
/// each atomic is influenced by the last vector that was produced by the
/// previous atomic.
///
/// Example:
/// - N -> FnF -> [2468]Kn[2468]K, starting from the `S` square
///
/// 1. start with a Ferz (F -> [2468]K) move which produces the vectors:
///     - (1, 1)
///     - (1, -1)
///     - (-1, -1)
///     - (-1, 1)
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 08 │    │ 02 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ S  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 06 │    │ 04 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// 2. for each of the vector, apply nF -> n[2468]K with respect to the last
/// vector:
///    - for (1, 1): direction is ne so n[2468]K rotated by ne produces (0, 1)
///      and (1, 0)
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │ 08 │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │ S' │ 02 │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ S  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
/// etc.
pub fn chained_atomic_to_vector(expr: &str) -> Vec<[(i8, i8); 2]> {
    let mut result: Vec<[(i8, i8); 2]> = vec!([(0, 0), (0, 0)]);
    let mut stack: Vec<[(i8, i8); 2]>;

    let cap: Vec<_> = ATOMIC.captures_iter(expr).collect();
    assert!(!cap.is_empty(), "Invalid chained atomic expression: {}", expr);

    for i in 0..cap.len() {
        stack = result.clone();
        result.clear();
        let atomic_str = cap[i].get(0).unwrap().as_str();

        while !stack.is_empty() {
            let previous = stack.pop().unwrap();

            let rotation = if i == 0 {
                "n"
            } else {
                let last_vector = previous[1];
                let index = *VECTOR_MAP.get(&last_vector).unwrap_or(&0);
                *INVERSE_CARDINAL_MAP.get(&index).unwrap_or(&"n")
            };

            let vectors = atomic_to_vector(atomic_str, rotation);

            for vector in &vectors {
                let whole_vector = (
                    previous[0].0 + vector.0,
                    previous[0].1 + vector.1,
                );
                let last_vector = *vector;
                if
                    whole_vector.0.abs() <= MAX_FILES as i8 &&
                    whole_vector.1.abs() <= MAX_RANKS as i8
                {                                                               /* prevent moving outside the board   */
                    result.push([whole_vector, last_vector]);
                }
            }
        }
    }

    result
}

fn compound_atomic_to_vector(expr: &str) -> Vec<[(i8, i8); 2]> {
    let result = Vec::new();

    result
}

/// a leg is defined as a move that consists of a compound atomics (see
/// `move_gen::compond_atomic_to_vector` for compound atomic definition). It
/// can begin with (in order):
/// - optional move modifiers (like u, d, m, c, etc.)
/// - optional *one* cardinal directions (n, e, s, w, ne, nw, se, sw)
/// - optional range (like [1..8], [18], [1..6$2], etc.)
///
/// all inputs are sanitized by `move_parse::parse_move` before being passed
/// here.
pub fn leg_to_vector(expr: &str) -> Vec<(i8, i8)> {
    let result = Vec::new();

    result
}

pub fn move_blockers(expr: &str, start: u16, state: &State) -> Board {
    let parsed_expr = parse_move(expr);

    println!("Parsed Expr: {}", parsed_expr);

    let mut blockers = Board::new(state.files, state.ranks);

    blockers
}

/// Determines if a move described by `expr` is possible from `start` to `end`
/// given the current board state represented by `enemies`, `friends`, and
/// `kings`. Not considering move legality beyond basic matching.
pub fn is_move_possible(
    expr: &str, start: u16, end: u16, state: &State
) -> bool {
    split_and_process(expr, |m| match_move(m, start, end, state))
        .any(|b| b.unwrap_or_default())                                         /* Return true if any match is found  */
}