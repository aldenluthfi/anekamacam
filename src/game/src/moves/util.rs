//! # util.rs
//!
//! Provides utility functions and patterns for move expression processing.
//!
//! This file contains shared utilities used across the move parsing and
//! matching modules, including regex patterns for normalization, range
//! expansion, and cardinal direction parsing. It also provides the core
//! expression evaluation logic using a stack-based algorithm for handling
//! operators (^ for concatenation, | for alternation), Betza atom mappings,
//! and parallel processing helpers for splitting and processing expressions.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use rayon::prelude::*;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    pub static ref NORMALIZE_PATTERN: Regex = Regex::new(
        r"[^(^|]\(|\)[^()^|]"
    ).unwrap();
    pub static ref RANGE_PATTERN: Regex = Regex::new(
        r"(-?)(?:\{(?:\.\.(\d+)|(\d+)\.\.|\.\.)\}|\*)"
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
pub fn betza_atoms(piece: char) -> String {
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
pub fn evaluate(expr: &str) -> String {
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

/// Splits an expression by '|' delimiter and processes each part in parallel.
/// Returns a parallel iterator over the results of applying function `f` to
/// each part.
pub fn split_and_process<'a, T>(
    expr: &'a str,
    f: impl Fn(&str) -> Option<T> + Sync + Send + 'a
) -> impl ParallelIterator<Item = Option<T>> + 'a
where
    T: Send + Sync,
{
    assert!(!expr.is_empty(), "{expr} must not empty.");

    expr.par_split('|')
        .map(move |s| f(s.trim()))
        .into_par_iter()
}