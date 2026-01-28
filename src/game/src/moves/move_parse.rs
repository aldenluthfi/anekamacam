//! # move_parse.rs
//!
//! Implements parsing, normalization, and move matching for move expressions.
//!
//! This file contains functionality for parsing Cheesy King Notations
//! move expressions, including normalization of algebraic expressions with
//! operators (^, |), expansion of range patterns, conversion of piece
//! symbols to atomic movement patterns, and move matching/validation against
//! board state. It uses parallel processing for efficiency when handling
//! complex move expressions with multiple alternatives.
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
use std::char;
use std::collections::{HashMap, HashSet, VecDeque};

use crate::representations::state::State;
use crate::moves::vector::AtomicVector;

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
    pub static ref ATOMIC_PARTS: Regex = Regex::new(
        r"^(n|ne|e|se|s|sw|w|nw)?(\[\d+\])?K$"
    ).unwrap();
    pub static ref ATOMIC: Regex = Regex::new(
        r"(?:n|ne|e|se|s|sw|w|nw)?(?:\[\d+\])?K"
    ).unwrap();
    pub static ref ATOMIC_TOKENS: Regex = Regex::new(
        concat!(
            r"(?:(?:n|ne|e|se|s|sw|w|nw)?(?:\[\d+\])?K)+|",
            r"(?:n|ne|e|se|s|sw|w|nw)|",
            r"\[\d+\]|",
            r"(?:\.+)|",
            r":?\{\d+(?:\.\.(?:\d+|\*))?\}|",
            r"<|>"
        )
    ).unwrap();
    pub static ref DOTS_TOKEN: Regex = Regex::new(
        r"^\.+$"
    ).unwrap();
    pub static ref DIRECTION_RANGE_TOKEN: Regex = Regex::new(
        r"^\[\d+\]$"
    ).unwrap();
    pub static ref RANGE_TOKEN: Regex = Regex::new(
        r"^\{(\d+)(?:\.\.(\d+|\*))?\}$"
    ).unwrap();
    pub static ref COLON_RANGE_TOKEN: Regex = Regex::new(
        r"^:\{(\d+)(?:\.\.(\d+|\*))?\}$"
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
    pub static ref CARDINAL_STR_TO_INDEX: HashMap<&'static str, u32> = {
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
fn betza_atoms(piece: char) -> String {
    match piece {
        'W' => "[1357]K".to_string(),
        'F' => "[2468]K".to_string(),
        'A' => "[2468]K.".to_string(),
        'D' => "[1357]K.".to_string(),
        'S' => "K.".to_string(),
        'N' => "[2468]Kn[2468]K".to_string(),
        'C' => "[2468]Kn[2468]K.".to_string(),
        'Z' => "[2468]K.n[2468]K".to_string(),
        'G' => "[2468]K..".to_string(),
        'H' => "[1357]K..".to_string(),
        'T' => "K..".to_string(),
        'B' => "[2468]K-*".to_string(),
        'R' => "[1357]K-*".to_string(),
        'Q' => "K-*".to_string(),
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
fn expand_directions(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");

    let mut expanded = expr.to_string();

    while let Some(cap) = DIRECTION_PATTERN.captures(&expanded) {
        println!("Direction Capture: {:?}", cap);

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
pub fn split_and_process<'a, T>(
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
pub fn parse_move(expr: &str) -> String {
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
fn atomic_to_vector(expr: &str, rotation: &str) -> Vec<(i8, i8)> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let mut set = HashSet::new();

    let cap = ATOMIC_PARTS.captures(expr)
        .expect(
            &format!("Invalid atomic expression: {}", expr)
        );

    let direction = cap.get(1).map(|m| m.as_str());
    let range = cap.get(2).map(|m| m.as_str());
    let rotation_index: u32 = *CARDINAL_STR_TO_INDEX.get(rotation)
        .expect(
            &format!("Invalid rotation direction: {}", rotation)
        );

    if let Some(range_str) = range {
        let digits = &range_str[1..range_str.len()-1];
        for digit_char in digits.chars() {
            if let Some(digit) = digit_char.to_digit(10) {
                let index = (digit - 1 + rotation_index) as usize;
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

/// takes the irregular vector and determines the gratest magnitude direction
///
/// Examples:
/// - (2, 1) has +x as the greatest magnitude so it will influence the next
///   atomic as "e" or (1, 0).
/// - (1, 2) has +y as the greatest magnitude so it will influence the next
///   atomic as "n" or (0, 1).
/// - (2, 2) has the same magnitude so it will influence the next atomic
///   as "ne" or (1, 1).
fn irregular_vector_direction(vector: &(i8, i8)) -> &str {
    let abs_x = vector.0.abs();
    let abs_y = vector.1.abs();

    let direction_vector = if abs_x > abs_y {
        (vector.0.signum(), 0)
    } else if abs_y > abs_x {
        (0, vector.1.signum())
    } else {
        (vector.0.signum(), vector.1.signum())
    };

    let index = *CARDINAL_VECTORS_TO_INDEX.get(&direction_vector).expect(
        &format!("Invalid direction vector: {:?}", direction_vector)
    );

    *CARDINAL_INDEX_TO_STR.get(&index).expect(
        &format!(
            "Invalid index for inverse cardinal map: {}",
            index
        )
    )
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
pub fn chained_atomic_to_vector(
    expr: &str, rotation: &str
) -> Vec<AtomicVector> {

    #[cfg(debug_assertions)]
    println!(
        "DEBUG: chained_atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let mut result: Vec<AtomicVector> = vec![AtomicVector::origin()];

    let mut stack: Vec<AtomicVector>;
    let game_state = State::global();

    let atomics: Vec<_> = ATOMIC.find_iter(expr).collect();
    assert!(!atomics.is_empty(), "Invalid chained atomic expression: {}", expr);

    for i in 0..atomics.len() {
        stack = result.clone();
        result.clear();
        let atomic_str = atomics[i].as_str();

        while !stack.is_empty() {
            let previous = stack.pop().unwrap();
            let prev_tuple = previous.as_tuple();

            let current_rotation = if i == 0 {
                rotation
            } else {
                irregular_vector_direction(&prev_tuple[1])
            };

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

    result.retain(|vector| {
        let whole = vector.whole();
        whole.0.abs() <= game_state.files as i8 &&
        whole.1.abs() <= game_state.ranks as i8
    });

    result
}

// using the formula for clockwise rotation of theta (θ)
//
// x' = xcos(θ) - ysin(θ)
// y' = -xsin(θ) + ycos(θ)
//
// but 45 degrees rotation is multiplied by sqrt(2)/2 since this is a grid
fn rotation_function(
    direction: &str
) -> impl Fn(i8, i8) -> (i8, i8) {
    match direction {
        "n" => |x: i8, y: i8| (x,  y),
        "ne" => |x: i8, y: i8| ((x + y) / 2, (-x + y) / 2),
        "e" => |x: i8, y: i8| (y, -x),
        "se" => |x:i8 , y: i8| ((-x + y) / 2, (-x - y) / 2),
        "s" => |x: i8, y: i8| (-x, -y),
        "sw" => |x: i8, y| ((-x - y) / 2,  (x - y) / 2),
        "w" => |x: i8, y: i8| (-y,  x),
        "nw" => |x: i8, y: i8| ((x - y) / 2,  (x + y) / 2),
        _ => panic!("Invalid rotation direction: {}", direction)
    }
}

fn rotate_vector(
    vector: AtomicVector,
    rotation: &str
) -> AtomicVector {
    let rotate = rotation_function(rotation);
    let whole = vector.whole();
    let last = vector.last();

    let rotated_whole = rotate(whole.0, whole.1);
    let rotated_last = rotate(last.0, last.1);

    AtomicVector::new(rotated_whole, rotated_last)
}

fn combine_vector_sets(
    set1: Vec<AtomicVector>,
    set2: Vec<AtomicVector>,
) -> Vec<AtomicVector> {
    let mut result: Vec<AtomicVector> = Vec::new();
    let game_state = State::global();

    for vec1 in set1 {
        let tuple1 = vec1.as_tuple();
        let last_direction = irregular_vector_direction(&tuple1[1]);
        for vec2 in &set2 {

            let rotated_vec2 = rotate_vector(*vec2, last_direction);

            #[cfg(debug_assertions)]
            println!(
                "DEBUG: combining {:?} with {:?} after rotating {:?} to {}",
                vec1, rotated_vec2, vec2, last_direction
            );

            result.push(vec1.add(&rotated_vec2));
        }
    }

    result.retain(|vector| {
        let whole = vector.whole();
        whole.0.abs() <= game_state.files as i8 &&
        whole.1.abs() <= game_state.ranks as i8
    });

    result
}

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

        let a_angle = (a_tuple[1].1 as f32).atan2(a_tuple[1].0 as f32);
        let b_angle = (b_tuple[1].1 as f32).atan2(b_tuple[1].0 as f32);

        a_angle.partial_cmp(&b_angle).unwrap()
    });

    vectors
}

fn filter_by_index(
    mut vectors: Vec<AtomicVector>,
    index: Vec<usize>
) -> Vec<AtomicVector> {
    let mut result: Vec<AtomicVector> = Vec::new();

    assert_eq!(
        vectors.len(), 8,
        "Can only filter from 8 cardinal directions clockwise"
    );

    vectors = sort_clockwise(vectors);

    for i in index {
        result.push(vectors[i - 1]);
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
pub fn compound_atomic_to_vector(
    expr: &str, rotation: &str
) -> Vec<AtomicVector> {
    #[cfg(debug_assertions)]
    println!(
        "DEBUG: compound_atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let game_state = State::global();
    let tokens: Vec<&str> = ATOMIC_TOKENS.find_iter(expr)
        .map(|m| m.as_str())
        .collect();

    #[cfg(debug_assertions)]
    println!("DEBUG: compound_atomic_to_vector tokens: {:?}", tokens);

    let mut stack: VecDeque<Vec<AtomicVector>> = VecDeque::new();
    for token in tokens {
        match token {
            ">" => {
                let mut substack: VecDeque<Vec<AtomicVector>> = VecDeque::new();

                while stack.len() > 0 {
                    let last_vector = stack.pop_back().unwrap();                /* need substack cuz not commutative  */

                    if let Some(special) = AtomicVector::get_special
                        (&last_vector[0]
                    ) {
                        if special == "<" {
                            break;
                        }
                    }

                    substack.push_back(last_vector);
                }

                #[cfg(debug_assertions)]
                println!(
                    "DEBUG: processing compound atomic substack: {:?}",
                    substack
                );

                let mut combined: Vec<AtomicVector> = vec![
                    AtomicVector::origin()
                ];

                while substack.len() > 0 {
                    let mut set = substack.pop_back().unwrap();

                    if set[0].is_special() {
                        let special = AtomicVector::get_special(&set[0]).expect(
                            "Expected special atomic vector."
                        );

                        #[cfg(debug_assertions)]
                        println!(
                            "DEBUG: processing special filter: {}",
                            special
                        );

                        if special.starts_with("[") && special.ends_with("]") {
                            let indices: &str = &special[1..special.len() - 1];
                            let vector_indices: Vec<usize> = indices
                                .chars()
                                .map(|c| c.to_digit(10).unwrap() as usize)
                                .collect();

                            let set2 = substack.pop_back().expect(
                                "Missing preceding vector for index filter."
                            );

                            set = filter_by_index(
                                set2, vector_indices
                            );
                        } else{
                            let set2 = substack.pop_back().expect(
                                "Missing preceding vector for cardinal filter."
                            );

                            set = filter_by_cardinal_direction(
                                set2, &special
                            );
                        }
                    }

                    combined = combine_vector_sets(combined, set);
                }

                stack.push_back(combined);
            },
            "<" | "n" | "e" | "s" | "w" |"ne" | "nw" | "se" | "sw" => {
                stack.push_back(vec![AtomicVector::special(token)]);
            },
            token if DIRECTION_RANGE_TOKEN.is_match(token) => {

                #[cfg(debug_assertions)]
                println!(
                    "DEBUG: direction-range token: {}",
                    token
                );

                stack.push_back(vec![AtomicVector::special(token)]);
            },
            token if DOTS_TOKEN.is_match(token) => {
                let num_dots = token.len() as i8;
                let last_vectors = stack.pop_back().expect(
                    "Missing preceding vector for dots."
                );

                let updated_vectors: Vec<AtomicVector> = last_vectors
                    .into_iter()
                    .map(|vector| {
                        let mut new_vector = vector.clone();
                        new_vector.set(&new_vector.add_last(num_dots));
                        new_vector
                    })
                    .collect();

                stack.push_back(updated_vectors);
            },
            token if RANGE_TOKEN.is_match(token) => {
                let captures = RANGE_TOKEN.captures(token).unwrap();

                #[cfg(debug_assertions)]
                println!(
                    "DEBUG: range token captures: {:?}",
                    captures
                );

                let start = captures.get(1);
                let end = captures.get(2);

                match (start, end) {
                    (Some(s), Some(e)) => {
                        let start_count: i8 = s.as_str().parse().expect(
                            "Invalid start range token."
                        );

                        let end_count: i8 = e.as_str().parse().unwrap_or(
                            i8::MAX
                        );

                        let last_vectors = stack.pop_back().expect(
                            "Missing preceding vector for range."
                        );

                        let mut all_updated_vectors: Vec<_> = Vec::new();

                        for count in start_count..=end_count {
                            let updated_vectors: Vec<_> = last_vectors
                                .iter()
                                .map(|vector| {
                                    let mut new_vector = vector.clone();
                                    new_vector.set(
                                        &new_vector.add_last(count - 1)
                                    );                                          /* count = number of dots - 1         */
                                    new_vector
                                })
                                .collect();

                            all_updated_vectors.extend(updated_vectors);
                        }

                        stack.push_back(all_updated_vectors);
                    },
                    (Some(s), None) => {
                        let count: i8 = s.as_str().parse().expect(
                            "Invalid start range token."
                        );

                        let last_vectors = stack.pop_back().expect(
                            "Missing preceding vector for range."
                        );

                        let updated_vectors: Vec<AtomicVector> = last_vectors
                            .into_iter()
                            .map(|vector| {
                                let mut new_vector = vector.clone();
                                new_vector.set(&new_vector.add_last(count - 1));/* count = number of dots - 1         */
                                new_vector
                            })
                            .collect();

                        stack.push_back(updated_vectors);
                    },
                    _ => {
                        panic!(
                            "Invalid range token: {:?}",
                            token
                        );
                    }
                }
            },
            token if COLON_RANGE_TOKEN.is_match(token) => {
                let captures = COLON_RANGE_TOKEN.captures(token).unwrap();

                #[cfg(debug_assertions)]
                println!(
                    "DEBUG: colon-range token captures: {:?}",
                    captures
                );

                let start = captures.get(1);
                let end = captures.get(2);

                match (start, end) {
                    (Some(s), Some(e)) => {
                        let start_count: i8 = s.as_str().parse().expect(
                            "Invalid start colon-range token."
                        );

                        let end_count: i8 = e.as_str().parse().unwrap_or(
                            i8::MAX
                        );

                        let mut all_accumulated_vectors: Vec<_> = Vec::new();

                        for count in start_count..=end_count {
                            let mut accumulated_vectors = stack.pop_back()
                                .expect(
                                    "Missing preceding vector for colon-range."
                                );

                            for _ in 1..count {
                                let set1 = accumulated_vectors.clone();
                                let set2 = accumulated_vectors.clone();

                                accumulated_vectors = combine_vector_sets(
                                    set1, set2
                                );

                            }

                            all_accumulated_vectors.extend(accumulated_vectors);
                        }

                        stack.push_back(all_accumulated_vectors);
                    },
                    (Some(s), None) => {
                        let count: i8 = s.as_str().parse().expect(
                            "Invalid start colon-range token."
                        );

                        let mut accumulated_vectors = stack.pop_back().expect(
                            "Missing preceding vector for colon-range."
                        );

                        for _ in 1..count {
                            let set1 = accumulated_vectors.clone();
                            let set2 = accumulated_vectors.clone();

                            accumulated_vectors = combine_vector_sets(
                                set1, set2
                            );

                            #[cfg(debug_assertions)]
                            println!(
                                "DEBUG: colon-range intermediate accumulated_vectors: {:?}",
                                accumulated_vectors
                            );
                        }

                        stack.push_back(accumulated_vectors);
                    },
                    _ => {
                        panic!(
                            "Invalid colon-range token: {:?}",
                            token
                        );
                    }
                }
            },
            _ => {
                stack.push_back(chained_atomic_to_vector(token, "n"));           /* default rotation "n" for now       */
            }
        }
    }


    #[cfg(debug_assertions)]
    println!(
        "DEBUG: compound_atomic_to_vector stack after parsing tokens: {:?}",
        stack
    );

    let mut combined: Vec<AtomicVector> = stack.pop_front().unwrap();

    if combined[0].is_special() {
        let special = AtomicVector::get_special(&combined[0]).expect(
            "Expected special atomic vector."
        );

        #[cfg(debug_assertions)]
        println!(
            "DEBUG: processing special filter: {}",
            special
        );

        if special.starts_with("[") && special.ends_with("]") {
            let indices: &str = &special[1..special.len() - 1];
            let vector_indices: Vec<usize> = indices
                .chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect();

            let set2 = stack.pop_front().expect(
                "Missing preceding vector for index filter."
            );

            combined = filter_by_index(
                set2, vector_indices
            );
        } else{
            let set2 = stack.pop_front().expect(
                "Missing preceding vector for cardinal filter."
            );

            combined = filter_by_cardinal_direction(
                set2, &special
            );
        }
    }

    while stack.len() > 0 {
        let mut set = stack.pop_front().unwrap();

        if set[0].is_special() {
            let special = AtomicVector::get_special(&set[0]).expect(
                "Expected special atomic vector."
            );

            #[cfg(debug_assertions)]
            println!(
                "DEBUG: processing special filter: {}",
                special
            );

            if special.starts_with("[") && special.ends_with("]") {
                let indices: &str = &special[1..special.len() - 1];
                let vector_indices: Vec<usize> = indices
                    .chars()
                    .map(|c| c.to_digit(10).unwrap() as usize)
                    .collect();

                let set2 = stack.pop_front().expect(
                    "Missing preceding vector for index filter."
                );

                set = filter_by_index(
                    set2, vector_indices
                );
            } else{
                let set2 = stack.pop_front().expect(
                    "Missing preceding vector for cardinal filter."
                );

                set = filter_by_cardinal_direction(
                    set2, &special
                );
            }
        }

        combined = combine_vector_sets(combined, set);
    }

    let mut result = combined;

    result.retain(|vector| {
        let whole = vector.whole();
        whole.0.abs() <= game_state.files as i8 &&
        whole.1.abs() <= game_state.ranks as i8
    });

    result
}

fn cleanup_atomic_vectors(vector_set: Vec<AtomicVector>) -> Vec<(i8, i8)> {
    let mut seen = HashSet::new();

    vector_set
        .into_iter()
        .filter_map(|vector| {
            let whole = vector.whole();
            if seen.insert(whole) {
                Some(whole)
            } else {
                None
            }
        })
        .collect()
}

/// Matches a move expression and chooses the right function to generate
/// move vectors based on the type of move expression.
///
/// All inputs are sanitized by `parse_move` before being passed
/// here.
fn match_move(expr: &str) -> Vec<(i8, i8)> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before matching.");

    vec![(0, 0)]
}

/// Determines if a move described by `expr` is possible from `start` to `end`
/// given the current board state. Not considering move legality beyond basic
/// matching.
pub fn generate_move_vectors(
    expr: &str
) -> Vec<(i8, i8)> {
    let parsed_expr = parse_move(expr);
    split_and_process(&parsed_expr, |m| Some(match_move(m)))
        .into_iter()
        .flatten()
        .flatten()
        .collect::<HashSet<_>>()                              /* Removes duplicates                  */
        .into_iter()
        .collect()
}
