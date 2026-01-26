//! # move_parse.rs
//!
//! Implements parsing and normalization of move expressions.
//!
//! This file contains functionality for parsing Cheesy King Notations
//! move expressions, including normalization of algebraic expressions with
//! operators (^, |), expansion of range patterns, and conversion of piece
//! symbols to atomic movement patterns. It uses parallel processing for
//! efficiency when handling complex move expressions with multiple
//! alternatives.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 18/02/2024

use lazy_static::lazy_static;
use rayon::iter::ParallelIterator;
use regex::Regex;

use crate::util::*;

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
        '|' => format!("{}|{}", a, b),                                          /* Concatenate with '|'               */
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

/// This ensures movestrings that are suitable to be processed by functions
/// below:
/// - `move_match::leg_to_vector`
/// - `move_match::atomic_to_vector`
pub fn parse_move(expr: &str) -> String {
    let expr = normalize(expr).unwrap_or_default();

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
                .flatten()
                .collect::<Vec<_>>()
                .join("|")
        })                                                                      /* Process each step in the pipeline  */
}
