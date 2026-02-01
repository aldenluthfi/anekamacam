//! move_parse.rs
//!
//! This module provides functionality for parsing chess moves from various
//! string representations into internal move structures. It supports standard
//! algebraic notation, coordinate notation, and handles special moves such as
//! castling, promotion, and en passant. The parsing logic is designed to be
//! robust and extensible for different chess variants and input formats.
//!
//! Typical usage involves calling the main parsing function with a move string
//! and a board state, returning a validated move or an error if the input is
//! invalid.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 18/02/2024

use lazy_static::lazy_static;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use rayon::prelude::*;
use regex::Regex;
use core::panic;
use std::char;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

use crate::representations::vector::MultiLegElement;
use crate::representations::vector::{
    Token::{
        self, *
    },
    AtomicElement::{
        self, *
    },
    MultiLegElement::{
        *
    },
    AtomicGroup, AtomicVector, LegVector, MultiLegGroup, MultiLegVector,
};
use crate::representations::state::State;

lazy_static! {
    pub static ref NORMALIZE_PATTERN: Regex = Regex::new(
        r"[^(^|]\(|\)[^()^|]"
    ).unwrap();
    pub static ref RANGE_PATTERN: Regex = Regex::new(
        r"(-?)(?:\{(?:\.\.(\d+)|(\d+)\.\.|\.\.)\}|\*)"
    ).unwrap();
    pub static ref DIRECTION_PATTERN: Regex = Regex::new(
        r"\[(\d+)?\.\.(\d+)?(?:\$(\d+))?\]|\[(\d+)\$(\d+)\]"
    ).unwrap();
    pub static ref CARDINAL_PATTERN: Regex = Regex::new(
        r"([nsew]{1,2}\+[nsew]{1,2})+"
    ).unwrap();
    pub static ref ATOMIC: Regex = Regex::new(
        r"(ne|nw|se|sw|n|s|e|w)?(\[\d+\])?K"
    ).unwrap();
    pub static ref ATOMIC_TOKENS: Regex = Regex::new(
        concat!(
            r"(?:(?:ne|nw|se|sw|n|s|e|w)?(?:\[\d+\])?K)+|",
            r"(?:ne|nw|se|sw|n|s|e|w)|",
            r"\[\d+\]|",
            r"(?:\.+)|",
            r":?\{\d+(?:\.\.(?:\d+|\*))?\}|",
            r"<|>"
        )
    ).unwrap();
    pub static ref DOTS_TOKEN: Regex = Regex::new(
        r"^-?\.+$"
    ).unwrap();
    pub static ref DIRECTION_FILTER_TOKEN: Regex = Regex::new(
        r"^\[\d+\]$"
    ).unwrap();
    pub static ref RANGE_TOKEN: Regex = Regex::new(
        r"^-?\{(\d+)(?:\.\.(\d+|\*))?\}$"
    ).unwrap();
    pub static ref COLON_RANGE_TOKEN: Regex = Regex::new(
        r"^-?:\{(\d+)(?:\.\.(\d+|\*))?\}$"
    ).unwrap();
    pub static ref LEG: Regex = Regex::new(
        r"^([mciudpk!]+)?([^@mciudpk]+)@?([^@]+)?$"
    ).unwrap();
    pub static ref LEG_TOKENS: Regex = Regex::new(
        concat!(
            r"(?:(?:ne|nw|se|sw|n|s|e|w)?(?:\[\d+\])?K)+|",
            r"(?:ne|nw|se|sw|n|s|e|w)|",
            r"\[\d+\]|",
            r"(?:\.+)|",
            r"-(?:\.+)|",
            r"[mciudpk!]+|",
            r":?\{\d+(?:\.\.(?:\d+|\*))?\}|",
            r"-:\{\d+(?:\.\.(?:\d+|\*))?\}|",
            r"-\{\d+(?:\.\.(?:\d+|\*))?\}|",
            r"</?|/?>|-",
        )
    ).unwrap();
    pub static ref MODIFIERS: Regex = Regex::new(
        r"^[mciudpk!]+$"
    ).unwrap();
    pub static ref INDEX_TO_CARDINAL_VECTORS: [(i8, i8); 8] = [
        ( 0,  1),
        ( 1,  1),
        ( 1,  0),
        ( 1, -1),
        ( 0, -1),
        (-1, -1),
        (-1,  0),
        (-1,  1)
    ];
    pub static ref CARDINAL_VECTORS_TO_INDEX: HashMap<(i8, i8), usize> = {
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
    pub static ref CARDINAL_STR_TO_INDEX: HashMap<&'static str, i8> = {
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
    pub static ref CARDINAL_INDEX_TO_STR: HashMap<usize, &'static str> = {
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
}

/// Applies the operator to the two operands and returns the result.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(apply_operator('^', "a", "b"), "ab");
/// assert_eq!(apply_operator('|', "a", "b"), "a|b");
/// assert_eq!(apply_operator('^', "a|b", "c|d"), "ac|ad|bc|bd");
/// ```
#[hotpath::measure]
fn apply_operator(op: char, a: &str, b: &str) -> String {
    match op {
        '^' => {
            let a_parts: Vec<&str> = a.split('|').collect();
            let b_parts: Vec<&str> = b.split('|').collect();

            let mut expr = Vec::with_capacity(a_parts.len() * b_parts.len());

            for x in &a_parts {
                for y in &b_parts {
                    let combined = if *x != "#" && *y != "#" {
                        format!("{}{}", x, y)                                   /* Combine x and y in correct order   */
                    } else if *x == "#" {
                        y.to_string()
                    } else {
                        x.to_string()
                    };
                    expr.push(combined);
                }
            }
            expr.join("|")
        }
        '|' => format!("{}|{}", a, b),                                          /* just return as is                  */
        _ => unreachable!("Invalid operator: {}", op),
    }
}

fn precedence(op: char) -> usize {
    match op {
        '^' => 2,
        '|' => 1,
        _ => unreachable!("Invalid operator: {}", op),
    }
}

/// Maps a Betza atom representation to a Cheesy King Notation string.
#[hotpath::measure]
fn betza_atoms(piece: char) -> String {
    match piece {
        'W' => "<[1357]K>".to_string(),
        'F' => "<[2468]K>".to_string(),
        'A' => "<[2468]K.>".to_string(),
        'D' => "<[1357]K.>".to_string(),
        'S' => "<K.>".to_string(),
        'N' => "<[2468]Kn[2468]K>".to_string(),
        'C' => "<[2468]Kn[2468]K.>".to_string(),
        'Z' => "<[2468]K.n[2468]K>".to_string(),
        'G' => "<[2468]K..>".to_string(),
        'H' => "<[1357]K..>".to_string(),
        'T' => "<K..>".to_string(),
        'B' => "<[2468]K-*>".to_string(),
        'R' => "<[1357]K-*>".to_string(),
        'Q' => "<K-*>".to_string(),
        _ => piece.to_string(),
    }
}

/// Parses a move expression and returns a normalized version of it.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(evaluate("(cQ|dQ-u#)-mnW"), "cQ-mnW|dQ-u#-mnW");
/// ```
#[hotpath::measure]
fn evaluate(expr: &str) -> String {
    let mut operands: Vec<String> = Vec::new();
    let mut operators: Vec<char> = Vec::new();
    let mut i = 0;
    let chars: Vec<char> = expr.chars().collect();

    while i < chars.len() {
        let c = chars[i];

        match c {
            '(' => {
                operators.push(c);
                i += 1;
            }                                                                   /* Push '(' to denote subexpr start   */
            ')' => {
                while let Some(op) = operators.pop() {
                    if op == '(' {
                        break;
                    }                                                           /* Eval subexpr until '(' is found    */
                    let a = operands.pop().unwrap();
                    let b = operands.pop().unwrap();
                    let combined = apply_operator(op, &b, &a);
                    operands.push(combined);
                }
                i += 1;
            }
            '^' | '|' => {
                while let Some(&op) = operators.last() {
                    if op != '(' && precedence(op) >= precedence(c) {
                        let a = operands.pop().unwrap();
                        let b = operands.pop().unwrap();
                        let combined = apply_operator(op, &b, &a);
                        operands.push(combined);
                        operators.pop();
                    } else {
                        break;
                    }
                }
                operators.push(c);
                i += 1;                                                         /* Push op respecting precedence      */
            }
            _ => {
                let mut operand = String::new();
                while i < chars.len() && !"^|()".contains(chars[i]) {
                    operand.push(chars[i]);
                    i += 1;
                }
                operands.push(operand);
            }
        }
    }

    while let Some(op) = operators.pop() {
                                                                                /* Eval remaining operators           */
        let a = operands.pop().unwrap();
        let b = operands.pop().unwrap();
        let combined = apply_operator(op, &b, &a);
        operands.push(combined);
    }

    operands.pop().unwrap()                                                     /* Final result is the only operand   */
}

/// Normalizes the expression by removing unnecessary parentheses and
/// ensuring that the expression is in a canonical form. it also adds ^
/// between implicit concatenations.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(normalize("(a|b)^c"), Some("ac|bc".to_string()));
/// ```
#[hotpath::measure]
fn normalize(expr: &str) -> Option<String> {
    let indices: Vec<usize> = NORMALIZE_PATTERN
        .find_iter(expr)
        .map(|m| (m.end() + m.start()) / 2)
        .collect();

    if indices.is_empty() {
        return Some(expr.to_string());
    }

    let mut parts = Vec::with_capacity(indices.len() + 1);
    let mut prev = 0;
    for &idx in &indices {
        parts.push(&expr[prev..idx]);
        prev = idx;                                                             /* Split expr at indices              */
    }
    parts.push(&expr[prev..]);
    let processed_expr = parts.join("^");                                       /* Join parts with '^'                */

    Some(evaluate(&processed_expr))                                             /* Eval the processed expr            */
}

#[hotpath::measure]
fn atomize(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");

    let mut atoms = Vec::with_capacity(expr.len());
    for c in expr.chars() {
        atoms.push(betza_atoms(c));
    }
    Some(atoms.join(""))                                                        /* Return Some with joined atoms      */
}

/// Expands directions, it handles the following formats, for example:
/// - `[1..8]` and similar: expands to [12345678]
/// - `[..5]` and similar: expands to [12345]
/// - `[5..]` and similar: expands to [5678]
/// - `[1..7$25]` and similar: expands to [13467]
/// - `[1235678$2]` and similar: expands to [135678]
#[hotpath::measure]
fn expand_directions(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");

    let mut expanded = expr.to_string();

    while let Some(cap) = DIRECTION_PATTERN.captures(&expanded) {
        let digits = if cap.get(4).is_some() {
            let digits_str = cap.get(4).unwrap().as_str();
            let exclusions = cap.get(5).map_or("", |m| m.as_str());

            let mut result = String::new();
            for ch in digits_str.chars() {
                if !exclusions.contains(ch) {
                    result.push(ch);
                }
            }
            result
        } else {
            let start = cap
                .get(1).and_then(|m| m.as_str()
                .parse::<u8>().ok()).unwrap_or(1);
            let end = cap
                .get(2).and_then(|m| m.as_str()
                .parse::<u8>().ok()).unwrap_or(8);
            let exclusions = cap.get(3).map_or("", |m| m.as_str());

            let mut result = String::new();
            for i in start..=end {
                if !exclusions.contains(&i.to_string()) {
                    result.push_str(&i.to_string());
                }
            }
            result
        };

        let replacement = format!("[{}]", digits);
        let cap_str = cap.get(0).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    Some(expanded)
}

/// Expands ranges in the expression. It handles the following formats:
/// - `{..}`: expands to all possible values `{1..*}`
/// - `{n..}`: expands to n to * `{n..*}`
/// - `{..n}`: expands to 1 to n `{1..n}`
/// - `*`: same as `{..}`
///
/// # Examples
/// ```ignore
/// assert_eq!(expand_ranges("{..}"), Some("{1..*}".to_string()));
/// assert_eq!(expand_ranges("{..5}"), Some("{1..5}".to_string()));
/// assert_eq!(expand_ranges("{5..}"), Some("{5..*}".to_string()));
/// ```
#[hotpath::measure]
fn expand_ranges(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");

    if !expr.contains('{') && !expr.contains('*') {
        return Some(expr.to_string());
    }

    let mut expanded = expr.to_string();

    while let Some(cap) = RANGE_PATTERN.captures(&expanded) {
        let prefix = cap.get(1).map_or("", |m| m.as_str());
        let replacement = match (cap.get(2), cap.get(3)) {
            (Some(end), _) => {
                let end_str = end.as_str();
                format!("{}{{1..{}}}", prefix, end_str)                         /* Handle ..n format                  */
            }
            (_, Some(start)) => {
                let start_str = start.as_str();
                format!("{}{{{}..&}}", prefix, start_str)                       /* Handle n.. format                  */
            }
            _ => format!("{}{{1..&}}", prefix),                                 /* Handle .. format                   */
        };
        let cap_str = cap.get(0).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    Some(expanded.replace('&', "*"))                                            /* Return Some with expanded ranges   */
}

/// Expands cardinal directions in the expression. It handles the
/// following formats:
///
/// - `n+s+e+w`: expands to `n|s|e|w`
/// - `n+e`: expands to `n|e`
/// - `n+e+s`: expands to `n|e|s`
#[hotpath::measure]
fn expand_cardinals(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");

    if !expr.contains('+') {
        return Some(expr.to_string());
    }

    let mut stack = vec![expr.to_string()];
    let mut result_stack = HashSet::new();

    while !stack.is_empty() {
        let term = stack.pop().unwrap();

        #[cfg(debug_assertions)]
        println!("DEBUG: expand_cardinals processing term: {}", term);

        if !CARDINAL_PATTERN.is_match(&term) {
            result_stack.insert(term);
            continue;
        }

        let cap = CARDINAL_PATTERN.captures(&term).unwrap();

        let cardinals = cap
            .get(0)
            .unwrap()
            .as_str();

        let split = cardinals
            .split('+')
            .collect::<Vec<&str>>();

        for cardinal in &split {
            stack.push(term.replacen(cardinals, cardinal, 1));                  /* Replace combined cardinals         */
        }
    }

    Some(result_stack.into_iter().collect::<Vec<String>>().join("|"))           /* Return Some with expanded cardinals*/
}

/// Splits an expression by '|' delimiter and processes each part in parallel.
/// Returns a vector of results of applying function `f` to each part.
#[hotpath::measure]
fn split_and_process<'a, T>(
    expr: &str,
    f: impl Fn(&str) -> Option<T> + Sync
) -> Vec<Option<T>>
where
    T: Send + Sync,
{
    assert!(!expr.is_empty(), "{expr} must not empty.");

    expr.par_split('|')
        .map(|s| f(s.trim()))
        .collect()
}

/// This ensures movestrings that are suitable to be processed by functions
/// below:
/// - `atomic_to_vector`
/// - `chained_atomic_to_vector`
/// - `compound_atomic_to_vector`
/// - `leg_to_vector`
#[hotpath::measure]
fn parse_move(expr: &str) -> String {
    let expr = normalize(expr).expect(
        &format!("Failed to normalize expression: {}", expr)
    );

    let pipeline = [
        atomize,
        expand_directions,
        expand_ranges,
        expand_cardinals,
    ];

    pipeline
        .iter()
        .fold(expr, |acc, &step| {
            split_and_process(&acc, step)
                .into_par_iter()
                .flatten()
                .collect::<Vec<_>>()
                .join("|")
        })                                                                      /* Process each step in the pipeline  */
}

/// takes the irregular vector and determines the gratest magnitude direction
///
/// Examples:
/// - (2, 1) has +x as the greatest magnitude so it will influence the next
///   atomic as "e" or (1, 0).
/// - (1, 2) has +y as the greatest magnitude so it will influence the next
///   atomic as "n" or (0, 1).
/// - (2, 2) has the same magnitude so it will influence the next atomic
///   as "ne" or (1, 1).
#[hotpath::measure]
fn irregular_vector_direction(vector: &(i8, i8)) -> &str {
    let abs_x = vector.0.saturating_abs();
    let abs_y = vector.1.saturating_abs();

    let direction_vector = if abs_x > abs_y {
        (vector.0.signum(), 0)
    } else if abs_y > abs_x {
        (0, vector.1.signum())
    } else {
        (vector.0.signum(), vector.1.signum())
    };

    let index = *CARDINAL_VECTORS_TO_INDEX.get(&direction_vector).expect(
        &format!("Invalid direction vector: {:?}", vector)
    );

    *CARDINAL_INDEX_TO_STR.get(&index).expect(
        &format!(
            "Invalid index for inverse cardinal map: {}",
            index
        )
    )
}

/// sorts 8 cardinal direction vectors clockwise, starting from +y axis
/// uses atan2 to determine the angle of each vector from +y
#[hotpath::measure]
fn sort_atomic_clockwise(
    mut vectors: Vec<AtomicVector>
) -> Vec<AtomicVector> {

    assert_eq!(
        vectors.len(), 8,
        "Can only sort 8 cardinal directions clockwise"
    );

    vectors.sort_by(|a, b| {
        let a_tuple = a.as_tuple();
        let b_tuple = b.as_tuple();

        let a_angle = (-a_tuple[0].0 as f32).atan2(-a_tuple[0].1 as f32);
        let b_angle = (-b_tuple[0].0 as f32).atan2(-b_tuple[0].1 as f32);

        a_angle.partial_cmp(&b_angle).unwrap()
    });

    vectors
}

/// returns a function that returns a tuple of 4 bools to determine whether a
/// point lies:
///
/// (north, east, south, west) of the perpendicular axes after rotation
///
/// for diagonal directions for example:
///
/// - ne: "up" is right of the line x=-y -> x + y > 0
/// - se: "right" is left of the line x=-y -> x + y < 0
/// - nw: "up" is left of the line x=y -> x - y < 0
/// - sw: "right" is right of the line x=y -> x - y > 0
#[hotpath::measure]
fn quadrant_function(
    direction: &str
) -> impl Fn(i8, i8) -> (bool, bool, bool, bool) {
    match direction {
        "n" => |x: i8, y: i8| (y > 0, x > 0, y < 0, x < 0),
        "e" => |x: i8, y: i8| (x > 0, -y > 0, x < 0, -y < 0),
        "s" => |x: i8, y: i8| (y < 0, -x > 0, y > 0, -x < 0),
        "w" => |x: i8, y: i8| (-x > 0, y > 0, -x < 0, y < 0),
        "ne" => |x: i8, y: i8| (x + y > 0, x - y > 0, x + y < 0, x - y < 0),
        "sw" => |x: i8, y: i8| (x + y < 0, x - y < 0, x + y > 0, x - y > 0),
        "nw" => |x: i8, y: i8| (x - y < 0, x + y > 0, x - y > 0, x + y < 0),
        "se" => |x: i8, y: i8| (x - y > 0, x + y < 0, x - y < 0, x + y > 0),
        _ => panic!("Invalid rotation direction: {}", direction)
    }
}

#[hotpath::measure]
fn filter_atomic_by_index(
    mut vectors: Vec<AtomicVector>,
    index: Vec<usize>
) -> Vec<AtomicVector> {
    let mut result: Vec<AtomicVector> = Vec::new();

    #[cfg(debug_assertions)]
    println!("DEBUG: filter_atomic_by_index sorted indices: {:?}", index);

    assert_eq!(
        vectors.len(), 8,
        "Can only filter from 8 cardinal directions clockwise"
    );

    vectors = sort_atomic_clockwise(vectors);

    #[cfg(debug_assertions)]
    println!("DEBUG: filter_atomic_by_index sorted vectors: {:?}", vectors);

    for i in index {
        result.push(vectors[i]);
    }

    result
}

#[hotpath::measure]
fn filter_atomic_by_cardinal_direction(
    mut vectors: Vec<AtomicVector>,
    direction: &str,
    pov: & str
) -> Vec<AtomicVector> {
    #[cfg(debug_assertions)]
    println!(
        "DEBUG: filter_atomic_by_cardinal_direction direction: {} w.r.t {}",
        direction, pov
    );

    let pov_fn = quadrant_function(pov);

    vectors.retain(| vector | {
        let whole = vector.whole();
        let (is_north, is_east, is_south, is_west) = pov_fn(whole.0, whole.1);
        match direction {
            "n" => is_north,
            "e" => is_east,
            "s" => is_south,
            "w" => is_west,
            "ne" => is_north && is_east,
            "se" => is_south && is_east,
            "sw" => is_south && is_west,
            "nw" => is_north && is_west,
            _ => panic!(
                "Invalid rotation direction: {}",
                direction
            )
        }
    });

    vectors
}

#[hotpath::measure]
fn filter_atomic_out_of_bounds(vector: &mut Vec<AtomicVector>) {
    let game_state = State::global();

    vector.retain(|vector| {
        let whole = vector.whole();
        whole.0.saturating_abs() <= game_state.files as i8 &&
        whole.1.saturating_abs() <= game_state.ranks as i8
    });
}

#[hotpath::measure]
fn remove_duplicates_in_place<Element>(vectors: &mut Vec<Element>)
where
    Element: Clone + Eq + Hash,
{
    let mut seen = HashSet::new();
    vectors.retain(|vector| {
        if seen.contains(vector) {
            false
        } else {
            seen.insert(vector.clone());
            true
        }
    });
}

#[hotpath::measure]
fn process_atomic_dots_token(
    vector_set: Vec<AtomicVector>,
    token: &str
) -> Vec<AtomicVector> {
    let dots_count = token.len() as i8;

    let mut updated_vectors: Vec<AtomicVector> = vector_set
        .into_iter()
        .map(|vector| {
            let mut new_vector = vector.clone();
            new_vector.set(&new_vector.add_last(dots_count));
            new_vector
        })
        .collect();

    filter_atomic_out_of_bounds(&mut updated_vectors);
    remove_duplicates_in_place(&mut updated_vectors);
    updated_vectors
}

#[hotpath::measure]
fn process_atomic_range_token(
    vector_set: Vec<AtomicVector>,
    token: &str
) -> Vec<AtomicVector> {
    let captures = RANGE_TOKEN.captures(token).unwrap();

    #[cfg(debug_assertions)]
    println!("DEBUG: range token captures: {:?}", captures);

    let start = captures.get(1);
    let end = captures.get(2);

    match (start, end) {
        (Some(s), Some(e)) => {
            let start_count: i8 = s.as_str().parse().expect(
                "Invalid start range token."
            );
            let end_count: i8 = e.as_str().parse().unwrap_or(i8::MAX);

            let mut all_updated_vectors: HashSet<_> = HashSet::new();

            for count in start_count..=end_count {
                let updated_vectors: Vec<_> = vector_set
                    .iter()
                    .map(|vector| {
                        let mut new_vector = vector.clone();
                        new_vector.set(&new_vector.add_last(count - 1));
                        new_vector
                    })
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect();

                let prev_len = all_updated_vectors.len();
                all_updated_vectors.extend(updated_vectors);
                if all_updated_vectors.len() == prev_len {
                    break;                                                      /* No new vectors are added so break  */
                }
            }

            let mut result: Vec<AtomicVector> =
                all_updated_vectors.into_iter().collect();

            filter_atomic_out_of_bounds(&mut result);
            remove_duplicates_in_place(&mut result);
            result
        },
        (Some(s), None) => {
                        let count: i8 = s.as_str().parse().expect(
                "Invalid start range token."
            );

            let mut updated_vectors: Vec<AtomicVector> = vector_set
                .into_iter()
                .map(|vector| {
                    let mut new_vector = vector.clone();
                    new_vector.set(&new_vector.add_last(count - 1));
                    new_vector
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .collect();

            filter_atomic_out_of_bounds(&mut updated_vectors);
            remove_duplicates_in_place(&mut updated_vectors);
            updated_vectors
        },
        _ => {
            panic!("Invalid range token: {:?}", token);
        }
    }
}

#[hotpath::measure]
fn process_atomic_colon_range_token(
    vector_set: Vec<AtomicVector>,
    token: &str,
    element: Option<AtomicElement>,
    modifiers: &(Option<Token>, Option<Token>)
)  -> Vec<AtomicVector> {
    if element.is_none() {
        panic!(
            "Colon-range token must be preceded by an atomic element: {:?}",
            token
        );
    }

    if let Some(AtomicEval(_)) = element {
        panic!(
            "Colon-range token cant be preceded by an evaluated expr: {:?}",
            token
        );
    }

    let element = element.unwrap();
    let mut result: HashSet<AtomicVector> = HashSet::new();

    let captures = COLON_RANGE_TOKEN.captures(token).unwrap();

    #[cfg(debug_assertions)]
    println!("DEBUG: colon-range token captures: {:?}", captures);

    let start = captures.get(1);
    let end = captures.get(2);

    match (start, end) {
        (Some(s), Some(e)) => {
            let start_count: i8 = s.as_str().parse().expect(
                "Invalid start colon-range token."
            );
            let end_count: i8 = e.as_str().parse().unwrap_or(i8::MAX);
            let mut prev_len = 0;

            for count in start_count..=end_count {
                let multiplied_expr = VecDeque::from(
                    vec![element.clone(); count as usize]
                );

                for branch_vector in &vector_set {
                    let mut extension = Vec::new();
                    let eval = evaluate_atomic_subexpresion(
                        vector_set.clone(),
                        multiplied_expr.clone(),
                        modifiers,
                    );

                    for vector in eval {
                        extension.push(branch_vector.add(&vector))
                    }

                    result.extend(extension);
                }

                if prev_len == result.len() {
                    break;                                                      /* No new vectors are added so break  */
                }
                prev_len = result.len();
            }

            let mut result: Vec<AtomicVector> =
                result.into_iter().collect();

            filter_atomic_out_of_bounds(&mut result);
            result
        },
        (Some(s), None) => {
            let count: i8 = s.as_str().parse().expect(
                "Invalid start colon-range token."
            );

            let multiplied_expr = VecDeque::from(
                vec![element; count as usize]
            );

            for branch_vector in &vector_set {
                let mut extension = Vec::new();
                let eval = evaluate_atomic_subexpresion(
                    vector_set.clone(),
                    multiplied_expr.clone(),
                    modifiers,
                );

                for vector in eval {
                    extension.push(branch_vector.add(&vector))
                }

                result.extend(extension);
            }

            let mut result: Vec<AtomicVector> =
                result.into_iter().collect();

            filter_atomic_out_of_bounds(&mut result);
            result
        },
        _ => {
            panic!("Invalid colon-range token: {:?}", token);
        }
    }
}

#[hotpath::measure]
fn process_atomic_modifiers(
    mut vector_set: Vec<AtomicVector>,
    modifiers: &(Option<Token>, Option<Token>),
    rotation: &str
) -> Vec<AtomicVector> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: process_modifiers with modifiers: {:?}, rotation: {}",
        modifiers, rotation
    );

    match modifiers {
        (Some(Cardinal(direction)), Some(Filter(indices))) => {
            vector_set = filter_atomic_by_cardinal_direction(
                vector_set, direction, rotation
            );
            let index_vec: Vec<usize> = indices
                .chars()
                .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
                .collect();
            filter_atomic_by_index(vector_set, index_vec)
        }
        (Some(Cardinal(direction)), None) => {
            filter_atomic_by_cardinal_direction(vector_set, direction, rotation)
        }
        (None, Some(Filter(indices))) => {
            let index_vec: Vec<usize> = indices
                .chars()
                .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
                .collect();
            filter_atomic_by_index(vector_set, index_vec)
        }
        _ => {
            vector_set
        },                                                                      /* Do nothing                         */
    }
}

#[hotpath::measure]
fn evaluate_atomic_term (
    result: Vec<AtomicVector>,
    term: Token,
    modifiers: &(Option<Token>, Option<Token>),
) -> Vec<AtomicVector> {

    let atomic = match term {
        Atomic(atomic) => atomic,
        _ => {
            panic!(
                "Unexpected atomic term in evaluate_atomic_term: {:?}",
                term
            );
        }
    };

    let mut new_result: HashSet<AtomicVector> = HashSet::new();
    for branch_vector in &result {
        let rotation_vector = &branch_vector.last();
        let rotation = irregular_vector_direction(
            rotation_vector
        );

        let mut extension = Vec::new();
        let mut eval = chained_atomic_to_vector(&atomic, rotation);

        eval = process_atomic_modifiers(eval, &modifiers, rotation);

        for vector in eval {
            extension.push(branch_vector.add(&vector))
        }

        new_result.extend(extension);
    }
    new_result.into_iter().collect()
}

#[hotpath::measure]
fn evaluate_atomic_subexpresion(
    result: Vec<AtomicVector>,
    subexpr: AtomicGroup,
    modifiers: &(Option<Token>, Option<Token>),
) -> Vec<AtomicVector> {
    let mut new_result: HashSet<AtomicVector> = HashSet::new();
    for branch_vector in &result {
        let rotation_vector = &branch_vector.last();

        let branch_rotation = irregular_vector_direction(
            rotation_vector
        );

        let mut extension = Vec::new();
        let eval_result = evaluate_atomic_expression(
            subexpr.clone(),
            &branch_rotation,
        );

        let mut eval = match eval_result {
            AtomicEval(vectors) => {
                vectors
            }
            _ => {
                panic!(
                    "Unexpected element in nested expression: {:?}",
                    eval_result
                );
            }
        };

        eval = process_atomic_modifiers(eval, &modifiers, branch_rotation);
        for vector in eval {
            extension.push(branch_vector.add(&vector))
        }

        new_result.extend(extension);
    }

    new_result.into_iter().collect()
}

#[hotpath::measure]
fn evaluate_atomic_expression(
    expr: AtomicGroup,
    rotation: &str,
) -> AtomicElement {

    #[cfg(debug_assertions)]
    println!(
        concat!(
            "DEBUG: evaluate_atomic_expression with expression: {:?} ",
            "with rotation: {} "
        ),
        expr, rotation
    );

    let mut result: Vec<AtomicVector> = Vec::from(
        vec![
            AtomicVector::origin(
                *CARDINAL_STR_TO_INDEX.get(rotation).unwrap()
            )
        ]
    );
    let mut modifiers: (Option<Token>, Option<Token>) =
        (None, None);

    let mut i = 0;
    while i < expr.len() {
        let element = &expr[i];
        match element {
            AtomicTerm(Cardinal(direction)) => {
                modifiers.0 = Some(Cardinal(direction.to_string()));
            }
            AtomicTerm(Filter(directions)) => {
                modifiers.1 = Some(Filter(directions.to_string()));
            }
            AtomicTerm(Dots(token)) => {
                result = process_atomic_dots_token(result, &token);
            }
            AtomicTerm(Range(token)) => {
                result = process_atomic_range_token(result, &token);
            }
            AtomicTerm(Atomic(atomic)) => {                                     /* Base case                          */
                if i + 1 < expr.len() {
                    if let AtomicTerm(Colon(token)) = &expr[i + 1] {
                        result = process_atomic_colon_range_token(
                            result,
                            &token,
                            Some(AtomicTerm(Atomic(atomic.to_string()))),
                            &modifiers
                        );
                        i += 2;
                        continue;
                    }
                }

                result = evaluate_atomic_term(
                    result,
                    Atomic(atomic.to_string()),
                    &modifiers,
                );

                modifiers = (None, None);                                       /* Reset modifiers after use          */
            }
            AtomicExpr(group) => {                                              /* Recursive case                     */
                if i + 1 < expr.len() {
                    if let AtomicTerm(Colon(token)) = &expr[i + 1] {
                        result = process_atomic_colon_range_token(
                            result,
                            &token,
                            Some(AtomicExpr(group.clone())),
                            &modifiers
                        );
                        i += 2;
                        continue;
                    }
                }
                result = evaluate_atomic_subexpresion(
                    result,
                    group.clone(),
                    &modifiers,
                );

                modifiers = (None, None);                                       /* Reset modifiers after use          */
            }
            _ => {
                panic!(
                    "Unexpected atomic element in expression: {:?}",
                    element
                );
            }
        }

        i += 1;
    }

    filter_atomic_out_of_bounds(&mut result);

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: evaluate_atomic_expression {:?} final len: {}",
        expr, result.len()
    );
    AtomicEval(result)
}

#[hotpath::measure]
fn process_closing_bracket<Term, IsBracket, WrapResult>(
    stack: &mut VecDeque<Term>,
    is_bracket: IsBracket,
    wrap_result: WrapResult,
)
where
    IsBracket: Fn(&Term) -> bool,
    WrapResult: Fn(VecDeque<Term>) -> Term,
{
    let mut result: VecDeque<Term> = VecDeque::new();

    while let Some(term) = stack.pop_back() {
        if is_bracket(&term) {
            break;
        } else {
            result.push_front(term);
        }
    }

    stack.push_back(wrap_result(result));
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
/// // n[2468]K with rotation towards "ne": (0, 1), (1, 0)
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
/// All inputs are sanitized by `parse_move` before being passed
/// here.
#[hotpath::measure]
fn atomic_to_vector(expr: &str, rotation: &str) -> Vec<(i8, i8)> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let mut set = HashSet::new();

    let cap = ATOMIC.captures(expr)
        .expect(
            &format!("Invalid atomic expression: {}", expr)
        );

    let direction = cap.get(1).map(|m| m.as_str());
    let range = cap.get(2).map(|m| m.as_str());
    let rotation_index: i8 = *CARDINAL_STR_TO_INDEX.get(rotation)
        .expect(
            &format!("Invalid rotation direction: {}", rotation)
        );

    if let Some(range_str) = range {
        let digits = &range_str[1..range_str.len()-1];
        for digit_char in digits.chars() {
            if let Some(digit) = digit_char.to_digit(10) {
                let index = (digit - 1 + rotation_index as u32) as usize;
                set.insert(
                    INDEX_TO_CARDINAL_VECTORS[
                        index % INDEX_TO_CARDINAL_VECTORS.len()
                    ]
                );
            }
        }
    } else {
        for i in 0..INDEX_TO_CARDINAL_VECTORS.len() {
            let index = (
                i + rotation_index as usize
            ) % INDEX_TO_CARDINAL_VECTORS.len();
            set.insert(INDEX_TO_CARDINAL_VECTORS[index]);
        }
    }

    if let Some(direction_str) = direction {
        if let Some(direction_set) = DIRECTION_VECTOR_SETS.get(direction_str) {
            let rotated_direction_set: HashSet<(i8, i8)> = direction_set.iter()
                .map(|(x, y)| {
                    let vec_index = *CARDINAL_VECTORS_TO_INDEX.get(
                        &(*x, *y)
                    ).unwrap();
                    let rotated_index = {
                        (
                            vec_index + rotation_index as usize
                        ) % INDEX_TO_CARDINAL_VECTORS.len()
                    };
                    INDEX_TO_CARDINAL_VECTORS[rotated_index]
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
/// `atomic_to_vector` for atomic definition) concatenated together
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
/// │    │    │    │    │    │    │    │    │    │
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
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 08 │    │ 02 │    │    │    │
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
/// etc.
///
/// A chained atomic can also end in three ways:
/// - one or more dots (.) means to repeat last vector regardless of direction
///   for each dot.
/// - a range {i..j} or {i} is a shorthand for writing dots.
/// - a colon-range :{i..j} or :{i} means to repeat the last atomic,
///   influenced by the last vector.
///
/// Example 1:
///
/// W. means W followed by repeating the last vector of W move once more
/// 1. first do a W move
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ || │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ == │ S  │ == │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ || │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
/// 2. then for each vector produced by W, repeat the last vector once more (.)
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │ F  │    │ S  │    │ F  │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// Example 2:
///
/// the {range} does not have to be a range, it could just be a number and the
/// numbers mean "amount of dots + 1", so W{1} is W, W{2} is W., W{3} is W..,
/// etc.
///
/// Example 3:
///
/// W{1..4} will branch into three paths for each dot, so we need to return
/// results from W, W., W.., and W... (W{1}, W{2}, W{3}, W{4} respectively)
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ F  │ F  │ F  │ F  │ S  │ F  │ F  │ F  │ F  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ F  │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// Example 4:
/// W:{1} means W, W:{2} means WW, W:{3} means WWW, etc.
/// W:{1..3} means the combined results from W, WW, and WWW.
///
/// A compound atomic can also end with dots (.) or range {i..j} or {i} and
/// colon-range :{i..j} or :{i} just like chained atomics but it applies to the
/// whole atomic group, not just an atomic.
///
/// We differentiate between chained atomic and compound atomic by the presence
/// of the < and > symbols. if there is no < and >, then it is treated as a
/// chained atomic. so only the last atomic will influence the next atomic.
///
/// Example:
/// - <FnF>. means moving N followed by repeating the last vector of N once more
///   but N. will expand to FnF. so the final result is equivalent to N followed
///   by the last F move.
#[hotpath::measure]
fn chained_atomic_to_vector(
    expr: &str, rotation: &str
) -> Vec<AtomicVector> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: chained_atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let mut result: Vec<AtomicVector> = vec![
        AtomicVector::origin(*CARDINAL_STR_TO_INDEX.get(rotation).expect(
                &format!("Invalid rotation direction: {}", rotation)
            ) as i8
        )
    ];

    if expr == "#" {                                                            /* null move follows prev direction   */
        return result;
    }

    let mut stack: Vec<AtomicVector>;

    let atomics: Vec<_> = ATOMIC.find_iter(expr).collect();
    assert!(!atomics.is_empty(), "Invalid chained atomic expression: {}", expr);

    for i in 0..atomics.len() {
        stack = result.clone();
        result.clear();
        let atomic_str = atomics[i].as_str();

        while !stack.is_empty() {
            let previous = stack.pop().unwrap();
            let prev_tuple = previous.as_tuple();

            let current_rotation = irregular_vector_direction(&prev_tuple[1]);

            let vectors = atomic_to_vector(atomic_str, current_rotation);

            for vector in &vectors {                                            /* apply the resulting vectors branch */
                let whole_vector = (
                    prev_tuple[0].0.saturating_add(vector.0),
                    prev_tuple[0].1.saturating_add(vector.1),
                );
                let last_vector = *vector;
                result.push(AtomicVector::new(whole_vector, last_vector));
            }
        }
    }

    result
}

/// Returns a vector containing a list of 2 tuples:
/// - The first tuple represents the whole vector of the move.
/// - The second tuple represents the last applied vector of the move.
///
/// A compound atomic is defined as multiple chained atomics (see
/// `chained_atomic_to_vector` for chained atomic definition).
/// These chained atomics can be croub by enclosing it with <...>. If <...> is
/// used it will be treated as one vector set so it will influence the direction
/// of the next chained atomic.
///
/// in the case of non cardinal vectors, the direction influence will be the
/// greatest magnitude vector among the compound atomics. if its the same, the
/// direction will be diagonal. for example:
///
/// - (2, 1) has +x as the greatest magnitude so it will influence the next
///   atomic as "e" or (1, 0).
/// - (1, 2) has +y as the greatest magnitude so it will influence the next
///   atomic as "n" or (0, 1).
/// - (2, 2) has the same magnitude so it will influence the next atomic
///   as "ne" or (1, 1).
///
/// More examples:
///
/// W<nWnF>nW (following only one branch, starting from S)
///
/// 1. move like a W, for example (0, 1)
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ ^^ │    │    │    │    │
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
///
/// 2. nWnF is inside <...> so it will be treated as one vector set and
///   influence the next atomic as a whole. so we need to recursively process
///   nWnF first. So the move after processing nWnF (starting from S'):
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ \\ │    │ // │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ || │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │ S' │    │    │    │    │
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
/// Note that even though nWnF ends with diagonal vectors (-1, 1) and (1, 1). it
/// is enclosed by <...> so it will influence the next atomic as a whole so
/// since as a whole it creates vector (-1, 2) and (1, 2) the greatest
/// magnitude is +y so it will influence the next atomic as "n" or (0, 1).
///
/// 3. finally apply nW (with respect to "n" from the previous step):
/// Final squares of the move (F Squares):
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │ F  │    │ F  │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
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
#[hotpath::measure]
fn compound_atomic_to_vector(
    expr: &str, rotation: &str
) -> Vec<AtomicVector> {
    #[cfg(debug_assertions)]
    println!(
        "DEBUG: compound_atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let tokens: Vec<&str> = ATOMIC_TOKENS.find_iter(expr)
        .map(|m| m.as_str())
        .collect();

    #[cfg(debug_assertions)]
    println!("DEBUG: compound_atomic_to_vector tokens: {:?}", tokens);

    let mut stack: AtomicGroup = VecDeque::new();

    for token in tokens {                                                       /* Sort of like parsing infix exprs   */
        match token {
            ">" => {
                process_closing_bracket(
                    &mut stack,
                    |term| matches!(term, AtomicTerm(Bracket(_))),
                    |result| AtomicExpr(result),
                );
            },
            "<" => {
                stack.push_back(
                    AtomicTerm(
                        Bracket(token.to_string())
                    )
                );
            },
            "n" | "e" | "s" | "w" | "ne" | "nw" | "se" | "sw" => {
                stack.push_back(
                    AtomicTerm(
                        Cardinal(token.to_string())
                    )
                );
            },
            token if DIRECTION_FILTER_TOKEN.is_match(token) => {
                stack.push_back(
                    AtomicTerm(
                        Filter(token.to_string())
                    )
                );
            },
            token if DOTS_TOKEN.is_match(token) => {
                stack.push_back(
                    AtomicTerm(
                        Dots(token.to_string())
                    )
                );
            },
            token if RANGE_TOKEN.is_match(token) => {
                stack.push_back(
                    AtomicTerm(
                        Range(token.to_string())
                    )
                );
            },
            token if COLON_RANGE_TOKEN.is_match(token) => {
                stack.push_back(
                    AtomicTerm(
                        Colon(token.to_string())
                    )
                );
            },
            _ => {
                stack.push_back(
                    AtomicTerm(
                        Atomic(token.to_string())
                    )
                );
            }
        }
    }

    let result = evaluate_atomic_expression(stack, rotation);                   /* Evaluate recursively               */
    match result {
        AtomicEval(result) => result,
        _ => panic!(
            "Expected result for {} but got {:?}",
            expr, result
        ),
    }
}

#[hotpath::measure]
fn sum_multi_leg_vectors(
    vectors: &MultiLegVector
) -> (i8, i8) {
    let mut sum: (i8, i8) = (0, 0);

    for leg in vectors {
        let atomic = leg.get_atomic();
        let whole = atomic.whole();
        sum.0 = sum.0.saturating_add(whole.0);
        sum.1 = sum.1.saturating_add(whole.1);
    }

    sum
}

/// sorts 8 cardinal direction vectors clockwise, starting from +y axis
/// uses atan2 to determine the angle of each vector from +y
#[hotpath::measure]
fn sort_multi_leg_clockwise(
    mut vectors: Vec<MultiLegVector>
) -> Vec<MultiLegVector> {

    assert_eq!(
        vectors.len(), 8,
        "Can only sort 8 cardinal directions clockwise"
    );

    vectors.sort_by(|a, b| {
        let a_tuple = sum_multi_leg_vectors(a);
        let b_tuple = sum_multi_leg_vectors(b);

        let a_angle = (-a_tuple.0 as f32).atan2(-a_tuple.1 as f32);
        let b_angle = (-b_tuple.0 as f32).atan2(-b_tuple.1 as f32);

        a_angle.partial_cmp(&b_angle).unwrap()
    });

    vectors
}

#[hotpath::measure]
fn filter_multi_leg_by_index(
    mut vectors: Vec<MultiLegVector>,
    index: Vec<usize>
) -> Vec<MultiLegVector> {

    let mut result: Vec<MultiLegVector> = Vec::new();

    #[cfg(debug_assertions)]
    println!("DEBUG: filter_atomic_by_index sorted indices: {:?}", index);

    vectors = sort_multi_leg_clockwise(vectors);

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: filter_multi_leg_by_index sorted vectors: {:#?}",
        vectors
    );

    for i in index {
        result.push(vectors[i].clone());
    }

    result
}

#[hotpath::measure]
fn filter_multi_leg_by_cardinal_direction(
    mut vectors: Vec<MultiLegVector>,
    direction: &str,
    pov: & str
) -> Vec<MultiLegVector> {
    #[cfg(debug_assertions)]
    println!(
        "DEBUG: filter_by_cardinal_direction direction: {} w.r.t {}",
        direction, pov
    );

    let pov_fn = quadrant_function(pov);

    vectors.retain(| vector | {
        let sum = sum_multi_leg_vectors(vector);
        let (is_north, is_east, is_south, is_west) = pov_fn(sum.0, sum.1);
        match direction {
            "n" => is_north,
            "e" => is_east,
            "s" => is_south,
            "w" => is_west,
            "ne" => is_north && is_east,
            "se" => is_south && is_east,
            "sw" => is_south && is_west,
            "nw" => is_north && is_west,
            _ => panic!(
                "Invalid rotation direction: {}",
                direction
            )
        }
    });

    vectors
}

#[hotpath::measure]
fn filter_multi_leg_out_of_bounds(vectors: &mut Vec<MultiLegVector>) {
    let game_state = State::global();

    vectors.retain(| vector | {
        let sum = sum_multi_leg_vectors(vector);
        sum.0.saturating_abs() <= game_state.files as i8 &&
        sum.1.saturating_abs() <= game_state.ranks as i8
    });
}

#[hotpath::measure]
fn process_multi_leg_dots_token(
    vector_set: Vec<MultiLegVector>,
    token: &str,
) -> Vec<MultiLegVector> {
    let dot_count = token.len() as i8;

    let mut updated_vectors: Vec<MultiLegVector> = vector_set
        .into_iter()
        .map(|mut vector| {
            let last = vector.last().expect(
                "Expected at least one leg in multi leg vector."
            ).clone();

            for _ in 0..dot_count {
                vector.push(last.clone());
            }
            vector
        })
        .collect();

    filter_multi_leg_out_of_bounds(&mut updated_vectors);
    remove_duplicates_in_place(&mut updated_vectors);
    updated_vectors
}

#[hotpath::measure]
fn process_multi_leg_range_token(
    vector_set: Vec<MultiLegVector>,
    token: &str,
) -> Vec<MultiLegVector> {
let captures = RANGE_TOKEN.captures(token).unwrap();

    #[cfg(debug_assertions)]
    println!("DEBUG: range token captures: {:?}", captures);

    let start = captures.get(1);
    let end = captures.get(2);

    match (start, end) {
        (Some(s), Some(e)) => {
            let start_count: i8 = s.as_str().parse().expect(
                "Invalid start range token."
            );
            let end_count: i8 = e.as_str().parse().unwrap_or(i8::MAX);

            let mut all_updated_vectors: HashSet<_> = HashSet::new();

            for count in start_count..=end_count {
                let updated_vectors: Vec<_> = vector_set
                    .clone()
                    .into_iter()
                    .map(|mut vector| {
                        let last = vector.last().expect(
                            "Expected at least one leg in multi leg vector."
                        ).clone();

                        for _ in 0..count - 1 {
                            vector.push(last.clone());
                        }
                        vector
                    })
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect();

                let prev_len = all_updated_vectors.len();
                all_updated_vectors.extend(updated_vectors);
                if all_updated_vectors.len() == prev_len {
                    break;                                                      /* No new vectors are added so break  */
                }
            }

            let mut result: Vec<MultiLegVector> =
                            all_updated_vectors.into_iter().collect();

            filter_multi_leg_out_of_bounds(&mut result);
            remove_duplicates_in_place(&mut result);
            result
        },
        (Some(s), None) => {
            let count: i8 = s.as_str().parse().expect(
                "Invalid start range token."
            );

            let mut updated_vectors: Vec<MultiLegVector> = vector_set
                .into_iter()
                .map(|mut vector| {
                    let last = vector.last().expect(
                        "Expected at least one leg in multi leg vector."
                    ).clone();

                    for _ in 0..count - 1 {
                        vector.push(last.clone());
                    }
                    vector
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .collect();

            filter_multi_leg_out_of_bounds(&mut updated_vectors);
            remove_duplicates_in_place(&mut updated_vectors);
            updated_vectors
        },
        _ => {
            panic!("Invalid range token: {:?}", token);
        }
    }
}

#[hotpath::measure]
fn process_multi_leg_colon_range_token(
    vector_set: Vec<MultiLegVector>,
    token: &str,
    element: Option<MultiLegElement>,
    modifiers: &[Option<Token>; 3],
    rotation: &str,
)  -> Vec<MultiLegVector> {
    if element.is_none() {
        panic!(
            "Colon-range token must be preceded by an atomic element: {:?}",
            token
        );
    }

    if let Some(MultiLegEval(_)) = element {
        panic!(
            "Colon-range token cant be preceded by an evaluated expr: {:?}",
            token
        );
    }

    let element = element.unwrap();
    let mut result: HashSet<MultiLegVector> = HashSet::new();

    let captures = COLON_RANGE_TOKEN.captures(token).unwrap();

    #[cfg(debug_assertions)]
    println!("DEBUG: colon-range token captures: {:?}", captures);

    let start = captures.get(1);
    let end = captures.get(2);

    match (start, end) {
        (Some(s), Some(e)) => {
            let start_count: i8 = s.as_str().parse().expect(
                "Invalid start colon-range token."
            );
            let end_count: i8 = e.as_str().parse().unwrap_or(i8::MAX);
            let mut prev_len = 0;

            for count in start_count..=end_count {
                let multiplied_expr = VecDeque::from(
                    vec![element.clone(); count as usize]
                );

                if vector_set.is_empty() {
                    let eval = evaluate_multi_leg_subexpression(
                        vec![],
                        MultiLegSlashExpr(multiplied_expr.clone()),
                        modifiers,
                        rotation,
                    );

                    result.extend(eval);
                } else {
                    for branch_leg_vector in &vector_set {
                        let branch_vector = branch_leg_vector.last().expect(
                            "Expected at least one vector in branch leg vector."
                        ).get_atomic();
                        let rotation_vector = &branch_vector.last();
                        let branch_rotation = irregular_vector_direction(
                            rotation_vector
                        );

                        let mut extension: Vec<MultiLegVector> = Vec::new();
                        let eval = evaluate_multi_leg_subexpression(
                            vec![],
                            MultiLegSlashExpr(multiplied_expr.clone()),
                            modifiers,
                            branch_rotation,
                        );

                        for vector in eval {
                            let mut new_branch = branch_leg_vector.clone();
                            new_branch.extend(&vector);
                            extension.push(new_branch);
                        }

                        result.extend(extension);
                    }
                }

                if prev_len == result.len() {
                    break;                                                      /* No new vectors are added so break  */
                }
                prev_len = result.len();
            }

            let mut result: Vec<MultiLegVector> =
                result.into_iter().collect();

            filter_multi_leg_out_of_bounds(&mut result);
            result
        },
        (Some(s), None) => {
            let count: i8 = s.as_str().parse().expect(
                "Invalid start colon-range token."
            );

            let multiplied_expr = VecDeque::from(
                vec![element; count as usize]
            );

            if vector_set.is_empty() {
                let eval = evaluate_multi_leg_subexpression(
                    vec![],
                    MultiLegSlashExpr(multiplied_expr.clone()),
                    modifiers,
                    rotation,
                );

                result.extend(eval);
            } else {
                for branch_leg_vector in &vector_set {
                    let branch_vector = branch_leg_vector.last().expect(
                        "Expected at least one vector in branch leg vector."
                    ).get_atomic();
                    let rotation_vector = &branch_vector.last();
                    let branch_rotation = irregular_vector_direction(
                        rotation_vector
                    );

                    let mut extension: Vec<MultiLegVector> = Vec::new();
                    let eval = evaluate_multi_leg_subexpression(
                        vec![],
                        MultiLegSlashExpr(multiplied_expr.clone()),
                        modifiers,
                        branch_rotation,
                    );

                    for vector in eval {
                        let mut new_branch = branch_leg_vector.clone();
                        new_branch.extend(&vector);
                        extension.push(new_branch);
                    }

                    result.extend(extension);
                }
            }

            let mut result: Vec<MultiLegVector> =
                result.into_iter().collect();

            filter_multi_leg_out_of_bounds(&mut result);
            result
        },
        _ => {
            panic!("Invalid colon-range token: {:?}", token);
        }
    }
}

#[hotpath::measure]
fn process_multi_leg_modifiers(
    mut vector_set: Vec<MultiLegVector>,
    modifiers: &[Option<Token>; 3],
    rotation: &str
) -> Vec<MultiLegVector> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: process_modifiers with modifiers: {:?} ",
        modifiers
    );

    if let Some(Cardinal(directions)) = &modifiers[0] {
        vector_set = filter_multi_leg_by_cardinal_direction(
            vector_set,
            directions,
            rotation,
        );
    }

    if let Some(Filter(indices)) = &modifiers[1] {
        let index_vec = indices
            .chars()
            .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
            .collect();
        vector_set = filter_multi_leg_by_index(
            vector_set,
            index_vec,
        );
    }

    if let Some(MoveModifier(modifier)) = &modifiers[2] {
        for multi_leg_vector in &mut vector_set {
            let final_leg = multi_leg_vector.last_mut().expect(
                "Expected at least one leg in multi leg vector."
            );
            final_leg.add_modifier(modifier);
        }
    }

    vector_set
}

#[hotpath::measure]
fn evaluate_multi_leg_term_leg(
    result: Vec<MultiLegVector>,
    term: Token,
    modifiers: [Option<Token>; 3],
    rotation: &str,
) -> Vec<MultiLegVector> {

    #[cfg(debug_assertions)]
    println!(
        concat!(
            "DEBUG: evaluate_multi_leg_term_leg with term: {:#?} ",
            "modifiers: {:?}"
        ),
        term, modifiers
    );

    let atomic = match term {
        Leg(atomic) => atomic,
        _ => {
            panic!(
                "Unexpected atomic term in evaluate_atomic_term: {:?}",
                term
            );
        }
    };

    if result.is_empty() {
        let eval = leg_to_vector(
            &atomic.to_string(),
            rotation
        );

        let eval = process_multi_leg_modifiers(eval, &modifiers, rotation);

        for multi_leg_vector in &eval {                                         /* By default treat as one leap       */
            let sum = sum_multi_leg_vectors(multi_leg_vector);

            multi_leg_vector.last().expect(
                "Expected at least one vector in multi leg vector."
            )
            .get_atomic()
            .set_last(sum);
        }

        return eval;
    }

    let mut new_result: HashSet<MultiLegVector> = HashSet::new();
    for branch_leg_vector in &result {
        let branch_vector = branch_leg_vector.last().expect(
            "Expected at least one vector in branch leg vector."
        ).get_atomic();
        let rotation_vector = &branch_vector.last();
        let branch_rotation = irregular_vector_direction(
            rotation_vector
        );

        let mut extension: Vec<MultiLegVector> = Vec::new();
        let mut eval = leg_to_vector(&atomic, branch_rotation);

        eval = process_multi_leg_modifiers(eval, &modifiers, branch_rotation);

        for multi_leg_vector in &eval {                                         /* By default treat as one leap       */
            let sum = sum_multi_leg_vectors(multi_leg_vector);

            multi_leg_vector.last().expect(
                "Expected at least one vector in multi leg vector."
            )
            .get_atomic()
            .set_last(sum);
        }

        for multi_leg_vector in eval {
            let mut combined = branch_leg_vector.clone();
            combined.extend(multi_leg_vector);
            extension.push(combined);
        }

        new_result.extend(extension);
    }

    new_result.into_iter().collect()
}

#[hotpath::measure]
fn evaluate_multi_leg_subexpression(
    result: Vec<MultiLegVector>,
    expr: MultiLegElement,
    modifiers: &[Option<Token>; 3],
    rotation: &str
) -> Vec<MultiLegVector> {
    #[cfg(debug_assertions)]
    println!(
        concat!(
            "DEBUG: evaluate_multi_leg_subexpr with expr: {:#?} ",
            "modifiers: {:?}, rotation: {}"
        ),
        expr, modifiers, rotation
    );

    let is_slash_expr = matches!(
        expr,
        MultiLegSlashExpr(_)
    );
    let inner_expr = match expr {
        MultiLegExpr(inner) => inner,
        MultiLegSlashExpr(inner) => inner,
        _ => {
            panic!(
                "Unexpected expr in evaluate_multi_leg_subexpr: {:?}",
                expr
            );
        }
    };

    if result.is_empty() {
        let eval = evaluate_multi_leg_expression(inner_expr, rotation);
        match eval {
            MultiLegEval(vectors) => {

                let eval = process_multi_leg_modifiers(
                    vectors, modifiers, rotation
                );

                if !is_slash_expr {
                    for multi_leg_vector in &eval {
                        let sum = sum_multi_leg_vectors(multi_leg_vector);

                        multi_leg_vector.last().expect(
                            "Expected at least one vector in multi leg vector."
                        )
                        .get_atomic()
                        .set_last(sum);
                    }
                }

                return eval;
            },
            _ => panic!(
                "Expected MultiLegEval but got different result: {:?}",
                eval
            ),
        };
    }

    let mut new_result: HashSet<MultiLegVector> = HashSet::new();
    for branch_leg_vector in &result {
        let branch_vector = branch_leg_vector.last().expect(
            "Expected at least one vector in branch leg vector."
        ).get_atomic();
        let rotation_vector = &branch_vector.last();
        let branch_rotation = irregular_vector_direction(
            rotation_vector
        );

        let mut extension: Vec<MultiLegVector> = Vec::new();
        let eval_result = evaluate_multi_leg_expression(
            inner_expr.clone(),
            branch_rotation
        );

        let mut eval = match eval_result {
            MultiLegEval(vectors) => vectors,
            _ => panic!(
                "Expected MultiLegEval but got different result: {:?}",
                eval_result
            ),
        };

        eval = process_multi_leg_modifiers(
            eval, modifiers, branch_rotation
        );

        if !is_slash_expr {
            for multi_leg_vector in &eval {
                let sum = sum_multi_leg_vectors(multi_leg_vector);

                multi_leg_vector.last().expect(
                    "Expected at least one vector in multi leg vector."
                )
                .get_atomic()
                .set_last(sum);
            }
        }

        for multi_leg_vector in eval {
            let mut combined = branch_leg_vector.clone();
            combined.extend(multi_leg_vector);
            extension.push(combined);
        }

        new_result.extend(extension);
    }

    new_result.into_iter().collect()
}

#[hotpath::measure]
fn evaluate_multi_leg_expression(
    expr: MultiLegGroup,
    rotation: &str
) -> MultiLegElement {
    #[cfg(debug_assertions)]
    println!(
        concat!(
            "DEBUG: evaluate_multi_leg_expression with expression: {:#?} ",
            "with rotation: {} "
        ),
        expr, rotation
    );

    let mut result: Vec<MultiLegVector> = Vec::new();
    let mut modifiers: [Option<Token>; 3] = [None, None, None];

    let mut i = 0;
    while i < expr.len() {
        let element = &expr[i];
        match element {
            MultiLegTerm(Cardinal(direction)) => {
                modifiers[0] = Some(Cardinal(direction.to_string()));
            }
            MultiLegTerm(Filter(directions)) => {
                modifiers[1] = Some(Filter(directions.to_string()));
            }
            MultiLegTerm(MoveModifier(modifier)) => {
                modifiers[2] = Some(MoveModifier(modifier.to_string()));
            }
            MultiLegTerm(Leg(atomic)) => {
                if i + 1 < expr.len() {
                    if let MultiLegTerm(Colon(token)) = &expr[i + 1] {
                        result = process_multi_leg_colon_range_token(
                            result,
                            &token,
                            Some(MultiLegTerm(Leg(atomic.to_string()))),
                            &modifiers,
                            rotation,
                        );
                        i += 2;
                        continue;
                    }
                }
                result = evaluate_multi_leg_term_leg(
                    result,
                    Leg(atomic.to_string()),
                    modifiers,
                    rotation
                );

                modifiers = [None, None, None];
            }
            MultiLegTerm(Dots(token)) => {
                result = process_multi_leg_dots_token(result, token);
            }
            MultiLegTerm(Range(token)) => {
                result = process_multi_leg_range_token(result, token);
            }
            MultiLegExpr(group) => {
                if i + 1 < expr.len() {
                    if let MultiLegTerm(Colon(token)) = &expr[i + 1] {
                        result = process_multi_leg_colon_range_token(
                            result,
                            &token,
                            Some(MultiLegSlashExpr(group.clone())),
                            &modifiers,
                            rotation,
                        );
                        i += 2;
                        continue;
                    }
                }

                result = evaluate_multi_leg_subexpression(
                    result,
                    element.clone(),
                    &modifiers,
                    rotation,
                );

                modifiers = [None, None, None];
            },
            MultiLegSlashExpr(group) => {
                if i + 1 < expr.len() {
                    if let MultiLegTerm(Colon(token)) = &expr[i + 1] {
                        result = process_multi_leg_colon_range_token(
                            result,
                            &token,
                            Some(MultiLegSlashExpr(group.clone())),
                            &modifiers,
                            rotation,
                        );
                        i += 2;
                        continue;
                    }
                }
                result = evaluate_multi_leg_subexpression(
                    result,
                    element.clone(),
                    &modifiers,
                    rotation,
                );

                modifiers = [None, None, None];
            },
            _ => {
                panic!(
                    "Unexpected element in multi-leg expression: {:?}",
                    element
                );
            }
        }
        i += 1;
    }

    filter_multi_leg_out_of_bounds(&mut result);

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: evaluate_multi_leg_expression {:?} final len: {:#?} ",
        expr, result.len()
    );

    MultiLegEval(result)
}

#[hotpath::measure]
fn tokenize_multi_leg_expression(
    expr: &str
) -> Vec<String> {

    let token_matches: Vec<_> = LEG_TOKENS.find_iter(expr).collect();
    assert!(
        !token_matches.is_empty(), "Invalid compound leg expression: {}", expr
    );

    let mut tokens: Vec<String> = Vec::new();
    let mut current = 0;

    for matches in token_matches {
        let start = matches.start();

        if start > current {
            tokens.push(expr[current..start].to_string());
        }

        let end = matches.end();
        current = end;

        tokens.push(expr[start..end].to_string());
    }

    if current < expr.len() {
        tokens.push(expr[current..expr.len()].to_string());
    }

    #[cfg(debug_assertions)]
    println!("DEBUG: tokenize_multi_leg_expression raw tokens: {:?}", tokens);

    let mut prev_tokens: Vec<String> = vec![];

    while prev_tokens != tokens {
        prev_tokens = tokens.clone();
        tokens = tokens.into_iter().rev().collect();
        let mut result = vec![];
        while !tokens.is_empty() {
            let token = tokens.pop().unwrap();
            result.push(token.clone());

            if token == ">" {
                let mut candidate: Vec<String> = vec![];
                while let Some(last) = result.pop() {
                    candidate.push(last.clone());

                    if last == "<" {
                        candidate.reverse();
                        let combined = candidate.join("");
                        result.push(combined);
                        break;
                    }

                    if last.starts_with("-") || last == "</" {
                        for t in candidate.into_iter().rev() {
                            result.push(t);
                        }
                        break;
                    }
                }
            }
        }
        tokens = result;
    }

    #[cfg(debug_assertions)]
    println!("DEBUG: tokenize_multi_leg_expression first pass {:?}", tokens);

    let mut result: Vec<String> = vec![];
    tokens = tokens.into_iter().rev().collect();

    while !tokens.is_empty() {
        if result.is_empty() {
            result.push(tokens.pop().unwrap());
            continue;
        }

        let token2 = tokens.pop().unwrap();
        let token1 = result.pop().unwrap();

        if  !token1.starts_with("-") && token1 != "<" && token1 != ">" &&
            !token2.starts_with("-") && token2 != "<" && token2 != ">" &&
            token1 != "</" && token2 != "</" &&
            token1 != "/>" && token2 != "/>" &&
            !MODIFIERS.is_match(&token1) && !MODIFIERS.is_match(&token2)

        {
            result.push(format!("{}{}", token1, token2));
            continue;
        } else {
            result.push(token1);
            result.push(token2);
        }
    }

    result.into_iter().collect()
}

/// A leg is defined as a compound atomic that has move modifiers applied to it.
#[hotpath::measure]
fn leg_to_vector(
    expr: &str,
    rotation: &str
) -> Vec<MultiLegVector> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: leg_to_vector leg {} with rotation {}",
        expr, rotation
    );

    let captures = LEG.captures(expr).expect(
        &format!("Invalid leg expression: {}", expr)
    );

    #[cfg(debug_assertions)]
    println!("DEBUG: leg_to_vector captures: {:?}", captures);

    let modifiers = captures.get(1).map_or("", |m| m.as_str());
    let exclusion = captures.get(3).map_or("", |m| m.as_str());
    let exclusion_vectors = if !exclusion.is_empty() {
        compound_atomic_to_vector(exclusion, rotation)
            .into_iter()
            .map(|v| v.whole())
            .collect::<HashSet<_>>()
    } else {
        HashSet::new()
    };
    let compound_atomic = captures.get(2).expect(
        "Missing compound atomic in leg."
    ).as_str();
    let mut vectors = compound_atomic_to_vector(compound_atomic, rotation);

    vectors.retain(|vector| {
        let tuple = vector.as_tuple();
        !exclusion_vectors.contains(&tuple[0])
    });

    vectors.into_iter()
        .map(|v| vec![LegVector::new(v, modifiers)])
        .collect::<Vec<Vec<LegVector>>>()
}

#[hotpath::measure]
fn multi_leg_to_vector(
    expr: &str,
    rotation: &str
) -> Vec<MultiLegVector> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: multi_leg_to_vector leg {} with rotation {}",
        expr, rotation
    );

    if !expr.contains("-") {
        return leg_to_vector(expr, rotation);
    }

    let tokens = tokenize_multi_leg_expression(expr);

    #[cfg(debug_assertions)]
    println!("DEBUG: multi_leg_to_vector final tokens: {:?}", tokens);

    let mut stack: MultiLegGroup = VecDeque::new();
    for token in tokens {
        match token.as_str() {
            "-" => {},                                                          /* Semantically irrelevant            */
            ">" => {
                process_closing_bracket(
                    &mut stack,
                    |term| matches!(term, MultiLegTerm(Bracket(_))),
                    |result| MultiLegExpr(result),
                );
            },
            "/>" => {
                process_closing_bracket(
                    &mut stack,
                    |term| matches!(term, MultiLegTerm(SlashBracket(_))),
                    |result| MultiLegSlashExpr(result),
                );
            },
            "<" => {
                stack.push_back(
                    MultiLegTerm(
                        Bracket(token.to_string())
                    )
                );
            },
            "</" => {
                stack.push_back(
                    MultiLegTerm(
                        SlashBracket(token.to_string())
                    )
                );
            },
            token if MODIFIERS.is_match(token) => {
                stack.push_back(
                    MultiLegTerm(
                        MoveModifier(token.to_string())
                    )
                );
            },
            "n" | "e" | "s" | "w" | "ne" | "nw" | "se" | "sw" => {
                stack.push_back(
                    MultiLegTerm(
                        Cardinal(token.to_string())
                    )
                );
            },
            token if DIRECTION_FILTER_TOKEN.is_match(token) => {
                stack.push_back(
                    MultiLegTerm(
                        Filter(token.to_string())
                    )
                );
            },
            token if DOTS_TOKEN.is_match(token) => {
                stack.push_back(
                    MultiLegTerm(
                        Dots(token.to_string())
                    )
                );
            },
            token if RANGE_TOKEN.is_match(token) => {
                stack.push_back(
                    MultiLegTerm(
                        Range(token.to_string())
                    )
                );
            },
            token if COLON_RANGE_TOKEN.is_match(token) => {
                stack.push_back(
                    MultiLegTerm(
                        Colon(token.to_string())
                    )
                );
            },
            _ => {
                stack.push_back(
                    MultiLegTerm(
                        Leg(token.to_string())
                    )
                );
            }
        }
    }

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: multi_leg_to_vector parsed stack: {:#?}",
        stack
    );

    let result = evaluate_multi_leg_expression(stack, rotation);                /* Evaluate recursively               */

    match result {
        MultiLegEval(result) => result,
        _ => panic!(
            "Expected evaluated result for {} but got {:?}",
            expr, result
        ),
    }
}

/// Generates all possible move vectors from the given move expression
#[hotpath::measure]
pub fn generate_move_vectors(
    expr: &str
) -> Vec<MultiLegVector> {
    let parsed_expr = parse_move(expr);
    split_and_process(&parsed_expr, |m| Some(multi_leg_to_vector(m, "n")))
        .into_iter()
        .flatten()
        .flatten()
        .collect::<HashSet<_>>()                        /* Removes duplicates                  */
        .into_iter()
        .collect()
}
