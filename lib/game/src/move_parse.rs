//! # move_parse.rs
//!
//! Contains functions for expanding and evaluating the Cheesy King Notation.
//!
//! This file contains functions for parsing, evaluating, and expanding
//! expressions in the Cheesy King Notation. It also includes helper functions
//! for applying operators and managing precedence.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date Created
//! 24/02/2024

use regex::Regex;

/// Evaluates a given expression in Cheesy King Notation (CKN).
///
/// This function parses and evaluates an expression string, applying operators
/// (`^`, `|`) with proper precedence and handling subexpressions enclosed in
/// parentheses. It uses a stack-based approach to manage operands and
/// operators.
///
/// # Arguments
/// * `expr` - A string slice (`&str`) representing the expression to evaluate.
///
/// # Returns
/// A `String` containing the result of the evaluated expression.
///
/// # Panics
/// This function will panic if:
/// - The expression contains invalid characters or is malformed.
/// - There are mismatched parentheses.
/// - An operator is applied to insufficient operands.
///
/// # Notes
/// - The function assumes that the input expression is well-formed and valid.
/// - Operators `^` and `|` are supported, with `^` having higher precedence.
/// - Subexpressions are evaluated first due to parentheses handling.
///
/// # See Also
/// - `apply_operator`: Applies an operator to two operands.
/// - `precedence`: Determines the precedence of an operator.
fn evaluate(expr: &str) -> String {
    let mut operands: Vec<String> = Vec::new();
    let mut operators: Vec<char> = Vec::new();
    let mut i = 0;

    while i < expr.len() {
        let c = expr.chars().nth(i).unwrap();

        match c {
            '(' => { operators.push(c); i += 1; }                               /* Push '(' to denote subexpr start   */
            ')' => {
                while let Some(op) = operators.pop() {
                    if op == '(' { break; }                                     /* Eval subexpr until '(' is found    */
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
                    } else { break; }
                }
                operators.push(c); i += 1;                                      /* Push op respecting precedence      */
            }
            _ => {
                let mut operand = String::new();
                while i < expr.len()
                    && !"^|()".contains(expr.chars().nth(i).unwrap()) {         /* Parse operand                      */
                    operand.push(expr.chars().nth(i).unwrap());
                    i += 1;
                }
                operands.push(operand);
            }
        }
    }

    while let Some(op) = operators.pop() {                                      /* Eval remaining operators           */
        let a = operands.pop().unwrap();
        let b = operands.pop().unwrap();
        let combined = apply_operator(op, &b, &a);
        operands.push(combined);
    }

    operands.pop().unwrap()                                                     /* Final result is the only operand   */
}

/// Applies a given operator to two operands and returns the result.
///
/// This function handles two operators:
/// - `^`: Combines two operands by concatenating their parts in a specific way.
/// - `|`: Concatenates the two operands directly with a `|` separator.
///
/// # Arguments
/// * `op` - A `char` representing the operator to apply (`^` or `|`).
/// * `a` - The first operand as a string slice (`&str`).
/// * `b` - The second operand as a string slice (`&str`).
///
/// # Returns
/// A `String` representing the result of applying the operator to the operands.
///
/// # Panics
/// This function will panic if:
/// - An unsupported operator is provided (anything other than `^` or `|`).
///
/// # Notes
/// - The `^` operator splits the operands by `|` and combines them in a
///   pairwise manner.
/// - The `#` character is treated as a special case, where it acts as an
///   identity element.
/// - The `|` operator simply concatenates the operands with a `|` separator.
fn apply_operator(op: char, a: &str, b: &str) -> String {
    match op {
        '^' => {
            let a_parts: Vec<&str> = a.split('|').collect();
            let b_parts: Vec<&str> = b.split('|').collect();
            let mut expr = Vec::new();
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

/// Returns the precedence level of a given operator.
///
/// This function defines the precedence rules for operators used in the
/// Cheesy King Notation (CKN). Higher precedence values indicate higher priority.
///
/// # Arguments
/// * `op` - A `char` representing the operator (`^` or `|`).
///
/// # Returns
/// A `usize` representing the precedence level of the operator:
/// - `2` for `^` (highest precedence).
/// - `1` for `|` (lower precedence).
///
/// # Panics
/// This function will panic if:
/// - An unsupported operator is provided (anything other than `^` or `|`).
///
/// # Notes
/// - Precedence determines the order in which operators are evaluated in an
///   expression.
/// - The `^` operator has higher precedence than `|`.
fn precedence(op: char) -> usize {
    match op {
        '^' => 2,
        '|' => 1,
        _ => unreachable!("Invalid operator: {}", op),
    }
}

/// Expands a move notation string into its constituent parts.
///
/// This function takes a compact move notation string and expands it into a
/// more explicit representation by resolving all parentheses and alternatives.
/// It processes the notation by splitting it at parentheses boundaries and
/// joining the parts with appropriate operators.
///
/// # Arguments
/// * `notation` - A string slice (`&str`) containing the move notation to
///                expand.
///
/// # Returns
/// A `String` containing the expanded move notation with all alternatives
/// separated by `|`.
///
/// # Panics
/// This function will panic if:
/// - The notation contains invalid or malformed expressions.
/// - There are mismatched parentheses.
///
/// # Notes
/// - The function converts parenthesized expressions into operator form.
/// - Each alternative in parentheses is processed according to operator rules.
/// - The final result is a flattened string of all possible moves
pub fn expand(expr: &str) -> String {
    let pattern = Regex::new(r"[^()^|]\(|\)[^()^|]").unwrap();
    let indices: Vec<usize> = pattern
        .find_iter(expr)
        .map(|m| (m.end() + m.start()) / 2)
        .collect();

    let mut parts = Vec::new();
    let mut prev = 0;
    for &idx in &indices {
        parts.push(&expr[prev..idx]); prev = idx;                               /* Split expr at indices              */
    }
    parts.push(&expr[prev..]);
    let processed_expr = parts.join("^");                                       /* Join parts with '^'                */
    println!("{}", processed_expr);

    evaluate(&processed_expr)                                                   /* Eval the processed expr            */
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rose() {
        let expanded = expand("Wn(eF|wF)-*");
        assert_eq!(
            expanded,
            "WneF-*|WnwF-*"
        );
    }

    #[test]
    fn crooked_bishop() {
        let expanded = expand("F(#|((#|eW)-<wW-eW>|(#|wW)-<eW-wW>)-*)");

        assert_eq!(
            expanded,
            "F|F-<wW-eW>-*|FeW-<wW-eW>-*|F-<eW-wW>-*|FwW-<eW-wW>-*"
        );
    }
}