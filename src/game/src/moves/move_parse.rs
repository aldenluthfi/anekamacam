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

use super::util::*;
use rayon::iter::ParallelIterator;

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

/// Expands ranges in the expression. It handles the following formats:
/// - `{..}`: expands to all possible values
/// - `{n..}`: expands to n to *
/// - `{..n}`: expands to 1 to n
/// - `*`: expands to 1 to *
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
                format!("{}{{{}..@}}", prefix, start_str)                       /* Handle n.. format                  */
            }
            _ => format!("{}{{1..@}}", prefix),                                 /* Handle .. format                   */
        };
        let cap_str = cap.get(0).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    Some(expanded.replace('@', "*"))                                            /* Return Some with expanded ranges   */
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

pub fn parse_move(expr: &str) -> String {
    let expr = normalize(expr).unwrap_or_default();

    let pipeline = [
        atomize,
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
