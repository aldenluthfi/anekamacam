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

use crate::moves::vectors::AtomicToken;
use crate::moves::vectors::{
    AtomicElement::{
        self, *
    }, AtomicGroup, AtomicToken::*, AtomicVector, LegVector, MultiLegGroup
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
        r"([nsew]{1,2}\+)*"
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
        r"^\.+$"
    ).unwrap();
    pub static ref DIRECTION_FILTER_TOKEN: Regex = Regex::new(
        r"^\[\d+\]$"
    ).unwrap();
    pub static ref RANGE_TOKEN: Regex = Regex::new(
        r"^\{(\d+)(?:\.\.(\d+|\*))?\}$"
    ).unwrap();
    pub static ref COLON_RANGE_TOKEN: Regex = Regex::new(
        r"^:\{(\d+)(?:\.\.(\d+|\*))?\}$"
    ).unwrap();
    pub static ref LEG: Regex = Regex::new(
        r"^([mciudpk!]+)?([^@mciudpk]+)@?([^@]+)?$"
    ).unwrap();
    pub static ref LEG_TOKENS: Regex = Regex::new(
        concat!(
            r"(?:^)?</?|/?>(?:$)?|-",
        )
    ).unwrap();
    pub static ref MODIFIERS: Regex = Regex::new(
        r"[mciudpk!]+"
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
    let mut result_stack = Vec::with_capacity(stack.len() * 2);

    while !stack.is_empty() {
        let term = stack.pop().unwrap();

        if !CARDINAL_PATTERN.is_match(&term) {
            result_stack.push(term);
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
            result_stack.push(term.replacen(cardinals, cardinal, 1));           /* Replace combined cardinals         */
        }
    }

    Some(result_stack.join("|"))                                                /* Return Some with expanded cardinals*/
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
fn sort_clockwise(
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

    #[cfg(debug_assertions)]
    for v in &vectors {
        let tuple = v.as_tuple();
        let angle = (tuple[0].0 as f32).atan2(tuple[0].1 as f32);
        println!("DEBUG: Vector {:?} atan2\t: {}", tuple[0], angle);
    }

    vectors
}

#[hotpath::measure]
fn filter_by_index(
    mut vectors: Vec<AtomicVector>,
    index: Vec<usize>
) -> Vec<AtomicVector> {
    let mut result: Vec<AtomicVector> = Vec::new();

    #[cfg(debug_assertions)]
    println!("DEBUG: filter_by_index sorted indices: {:?}", index);

    assert_eq!(
        vectors.len(), 8,
        "Can only filter from 8 cardinal directions clockwise"
    );

    vectors = sort_clockwise(vectors);

    #[cfg(debug_assertions)]
    println!("DEBUG: filter_by_index sorted vectors: {:?}", vectors);

    for i in index {
        result.push(vectors[i]);
    }

    result
}

#[hotpath::measure]
fn filter_by_cardinal_direction(
    mut vectors: Vec<AtomicVector>,
    direction: &str
) -> Vec<AtomicVector> {
    vectors.retain(| vector | {
        match direction {
            "n" => vector.whole().1 > 0,
            "ne" => vector.whole().0 > 0 && vector.whole().1 > 0,
            "e" => vector.whole().0 > 0,
            "se" => vector.whole().0 > 0 && vector.whole().1 < 0,
            "s" => vector.whole().1 < 0,
            "sw" => vector.whole().0 < 0 && vector.whole().1 < 0,
            "w" => vector.whole().0 < 0,
            "nw" => vector.whole().0 < 0 && vector.whole().1 > 0,
            _ => true,
        }
    });

    vectors
}

#[hotpath::measure]
fn filter_out_of_bounds(vector: &mut Vec<AtomicVector>) {
    let game_state = State::global();

    vector.retain(|vector| {
        let whole = vector.whole();
        whole.0.saturating_abs() <= game_state.files as i8 &&
        whole.1.saturating_abs() <= game_state.ranks as i8
    });
}

#[hotpath::measure]
fn remove_duplicates_in_place(
    vectors: &mut Vec<AtomicVector>
) {
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

/// using the formula for clockwise rotation of theta (θ)
///
/// x' = xcos(θ) - ysin(θ)
/// y' = -xsin(θ) + ycos(θ)
///
/// but 45 degrees rotation is multiplied by sqrt(2) since this is a grid
#[hotpath::measure]
fn rotation_function(
    direction: &str
) -> impl Fn(i8, i8) -> (i8, i8) {
    match direction {
        "n" => |x: i8, y: i8| (x,  y),
        "ne" => |x: i8, y: i8| (x + y, -x + y),
        "e" => |x: i8, y: i8| (y, -x),
        "se" => |x:i8 , y: i8| (-x + y, -x - y),
        "s" => |x: i8, y: i8| (-x, -y),
        "sw" => |x: i8, y| (-x - y,  x - y),
        "w" => |x: i8, y: i8| (-y,  x),
        "nw" => |x: i8, y: i8| (x - y,  x + y),
        _ => panic!("Invalid rotation direction: {}", direction)
    }
}

#[hotpath::measure]
fn rotate_vector(
    vector: AtomicVector,
    rotation: &str
) -> AtomicVector {
    let rotate = rotation_function(rotation);
    let whole = vector.whole();
    let last = vector.last();

    AtomicVector::new(rotate(whole.0, whole.1), rotate(last.0, last.1))
}

#[hotpath::measure]
fn rotate_direction<'a>(
    initial: &'a str,
    rotation: &str
) -> &'a str {
    let initial_index = *CARDINAL_STR_TO_INDEX.get(initial).expect(
        &format!("Invalid initial direction: {}", initial)
    );

    let rotation_index = *CARDINAL_STR_TO_INDEX.get(rotation).expect(
        &format!("Invalid rotation direction: {}", rotation)
    );

    let new_index = (initial_index + rotation_index) % 8;

    *CARDINAL_INDEX_TO_STR.get(&(new_index as usize)).expect(
        &format!("Invalid new index for inverse cardinal map: {}", new_index)
    )
}

#[hotpath::measure]
fn process_dots_token(
    vector_set: Vec<AtomicVector>,
    token: &str
) -> Vec<AtomicVector> {
    let num_dots = token.len() as i8;

    let mut updated_vectors: Vec<AtomicVector> = vector_set
        .into_iter()
        .map(|vector| {
            let mut new_vector = vector.clone();
            new_vector.set(&new_vector.add_last(num_dots));
            new_vector
        })
        .collect();

    filter_out_of_bounds(&mut updated_vectors);
    remove_duplicates_in_place(&mut updated_vectors);
    updated_vectors
}

#[hotpath::measure]
fn process_range_token(
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

            filter_out_of_bounds(&mut result);
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

            filter_out_of_bounds(&mut updated_vectors);
            remove_duplicates_in_place(&mut updated_vectors);
            updated_vectors
        },
        _ => {
            panic!("Invalid range token: {:?}", token);
        }
    }
}

#[hotpath::measure]
fn process_colon_range_token(
    vector_set: Vec<AtomicVector>,
    token: &str,
    element: Option<AtomicElement>,
    modifiers: &(Option<AtomicToken>, Option<AtomicToken>)
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
                    let rotation_vector = &branch_vector.last();

                    if rotation_vector == &(0, 0) {
                        continue;
                    }

                    let rotation = irregular_vector_direction(
                        rotation_vector
                    );

                    let mut extension = Vec::new();
                    let eval_result = evaluate_atomic_expression(
                        multiplied_expr.clone(),
                        rotation,
                        modifiers.clone()
                    );

                    let eval = match eval_result {
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

            filter_out_of_bounds(&mut result);
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
                let rotation_vector = &branch_vector.last();

                if rotation_vector == &(0, 0) {
                    continue;
                }

                let rotation = irregular_vector_direction(
                    rotation_vector
                );

                let mut extension = Vec::new();
                let eval_result = evaluate_atomic_expression(
                    multiplied_expr.clone(),
                    rotation,
                    modifiers.clone()
                );

                let eval = match eval_result {
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

                for vector in eval {
                    extension.push(branch_vector.add(&vector))
                }

                result.extend(extension);
            }

            let mut result: Vec<AtomicVector> =
                result.into_iter().collect();

            filter_out_of_bounds(&mut result);
            result
        },
        _ => {
            panic!("Invalid colon-range token: {:?}", token);
        }
    }
}

#[hotpath::measure]
fn process_modifiers(
    mut vector_set: Vec<AtomicVector>,
    modifiers: &(Option<AtomicToken>, Option<AtomicToken>),
    rotation: &str
) -> Vec<AtomicVector> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: process_modifiers with modifiers: {:?} ",
        modifiers
    );

    match modifiers {
        (Some(Cardinal(direction)), Some(Filter(indices))) => {
            let rotated = rotate_direction(&direction, rotation);
            vector_set = filter_by_cardinal_direction(vector_set, rotated);
            let index_vec: Vec<usize> = indices
                .chars()
                .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
                .collect();
            filter_by_index(vector_set, index_vec)
        }
        (Some(Cardinal(direction)), None) => {
            let rotated = rotate_direction(&direction, rotation);
            filter_by_cardinal_direction(vector_set, rotated)
        }
        (None, Some(Filter(indices))) => {
            let index_vec: Vec<usize> = indices
                .chars()
                .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
                .collect();
            filter_by_index(vector_set, index_vec)
        }
        _ => {
            vector_set
        },                                                                      /* Do nothing                         */
    }
}

#[hotpath::measure]
fn evaluate_atomic_term (
    result: Vec<AtomicVector>,
    term: AtomicToken,
    modifiers: &(Option<AtomicToken>, Option<AtomicToken>),
    rotation: &str
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

        if rotation_vector == &(0, 0) {
            continue;
        }

        let rotation = irregular_vector_direction(
            rotation_vector
        );

        let mut extension = Vec::new();
        let eval = chained_atomic_to_vector(&atomic, rotation);

        for vector in eval {
            extension.push(branch_vector.add(&vector))
        }

        new_result.extend(extension);
    }

    let result = new_result.into_iter().collect();
    process_modifiers(result, &modifiers, rotation)
}

#[hotpath::measure]
fn evaluate_atomic_subexpression(
    result: Vec<AtomicVector>,
    subexpr: AtomicGroup,
    modifiers: &(Option<AtomicToken>, Option<AtomicToken>),
    rotation: &str
) -> Vec<AtomicVector> {
    let mut new_result: HashSet<AtomicVector> = HashSet::new();
    for branch_vector in &result {
        let rotation_vector = &branch_vector.last();

        if rotation_vector == &(0, 0) {
            continue;
        }

        let branch_rotation = irregular_vector_direction(
            rotation_vector
        );

        let mut extension = Vec::new();
        let eval_result = evaluate_atomic_expression(
            subexpr.clone(),
            "n",
            modifiers.clone()
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

        eval = process_modifiers(eval, &modifiers, rotation);

        for vector in eval {
            let rotated_vector = rotate_vector(vector, &branch_rotation);
            println!(
                "DEBUG: {:?} to rotated_vector {:?} from branch_rotation: {}",
                vector, rotated_vector, branch_rotation
            );
            extension.push(branch_vector.add(&rotated_vector))
        }

        new_result.extend(extension);
    }

    new_result.into_iter().collect()
}

#[hotpath::measure]
fn evaluate_atomic_expression(
    expr: AtomicGroup,
    rotation: &str,
    modifiers: (Option<AtomicToken>, Option<AtomicToken>)
) -> AtomicElement {

    #[cfg(debug_assertions)]
    println!(
        concat!(
            "DEBUG: evaluate_atomic_expression with expression: {:?} ",
            "with rotation: {} ",
            "and modifiers: {:?}"
        ),
        expr, rotation, modifiers
    );

    let mut result: Vec<AtomicVector> = Vec::from(
        vec![
            AtomicVector::origin(
                *CARDINAL_STR_TO_INDEX.get(rotation).unwrap()
            )
        ]
    );
    let mut next_modifiers: (Option<AtomicToken>, Option<AtomicToken>) =
        (None, None);

    let mut i = 0;
    while i < expr.len() {
        let element = &expr[i];
        match element {
            AtomicTerm(Cardinal(direction)) => {
                next_modifiers.0 = Some(Cardinal(direction.to_string()));
            }
            AtomicTerm(Filter(directions)) => {
                next_modifiers.1 = Some(Filter(directions.to_string()));
            }
            AtomicTerm(Dots(token)) => {
                result = process_dots_token(result, &token);
            }
            AtomicTerm(Range(token)) => {
                result = process_range_token(result, &token);
            }
            AtomicTerm(Atomic(atomic)) => {                                     /* Base case                          */
                if i + 1 < expr.len() {
                    if let AtomicTerm(Colon(token)) = &expr[i + 1] {
                        result = process_colon_range_token(
                            result,
                            &token,
                            Some(AtomicTerm(Atomic(atomic.to_string()))),
                            &next_modifiers
                        );
                        i += 2;
                        continue;
                    }
                }

                result = evaluate_atomic_term(
                    result,
                    Atomic(atomic.to_string()),
                    &next_modifiers,
                    rotation
                );

                next_modifiers = (None, None);                                  /* Reset modifiers after use          */
            }
            AtomicExpr(group) => {                                              /* Recursive case                     */
                if i + 1 < expr.len() {
                    if let AtomicTerm(Colon(token)) = &expr[i + 1] {
                        result = process_colon_range_token(
                            result,
                            &token,
                            Some(AtomicExpr(group.clone())),
                            &next_modifiers
                        );
                        i += 2;
                        continue;
                    }
                }
                result = evaluate_atomic_subexpression(
                    result,
                    group.clone(),
                    &next_modifiers,
                    rotation
                );

                next_modifiers = (None, None);                                  /* Reset modifiers after use          */
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

    filter_out_of_bounds(&mut result);

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: evaluate_atomic_expression {:?} final len: {}",
        expr, result.len()
    );
    AtomicEval(result)
}


#[hotpath::measure]
fn atomic_process_closing_bracket(
    stack: &mut AtomicGroup
) {
    let mut result: AtomicGroup = VecDeque::new();

    while stack.len() > 0 {
        let term = stack.pop_back().unwrap();
        match term {
            AtomicTerm(AtomicBracket(ref s)) if s == "<" => {
                break;
            },
            _ => {
                result.push_front(term);
            }
        }
    }


    stack.push_back(
        AtomicExpr(result)
    );
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

    if expr == "#" {
        return vec![AtomicVector::null()];
    }

    let tokens: Vec<&str> = ATOMIC_TOKENS.find_iter(expr)
        .map(|m| m.as_str())
        .collect();

    #[cfg(debug_assertions)]
    println!("DEBUG: compound_atomic_to_vector tokens: {:?}", tokens);

    let mut stack: AtomicGroup = VecDeque::new();

    for token in tokens {                                                       /* Sort of like parsing infix exprs   */
        match token {
            ">" => {
                atomic_process_closing_bracket(&mut stack);
            },
            "<" => {
                stack.push_back(
                    AtomicTerm(
                        AtomicBracket(token.to_string())
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

    println!("DEBUG: compound_atomic_to_vector final stack: {:?}", stack);

    let result = evaluate_atomic_expression(stack, rotation, (None, None));     /* Evaluate recursively               */

    match result {
        AtomicEval(result) => result,
        _ => panic!(
            "Expected result for {} but got {:?}",
            expr, result
        ),
    }
}

/// Generates all possible move vectors from the given move expression
#[hotpath::measure]
pub fn generate_move_vectors(
    expr: &str
) -> Vec<AtomicVector> {
    let parsed_expr = parse_move(expr);
    split_and_process(&parsed_expr, |m| Some(compound_atomic_to_vector(m, "n")))
        .into_iter()
        .flatten()
        .flatten()
        .collect::<HashSet<_>>()                        /* Removes duplicates                  */
        .into_iter()
        .collect()
}
