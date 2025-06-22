use std::{char, vec};
use regex::Regex;
use lazy_static::lazy_static;
use rayon::prelude::*;

lazy_static! {
    static ref NORMALIZE_PATTERN: Regex = Regex::new(
        r"[^(^|]\(|\)[^()^|]"
    ).unwrap();
    static ref RANGE_PATTERN: Regex = Regex::new(
        r"(-?)(?:\{(?:\.\.(\d+)|(\d+)\.\.|\.\.)\}|\*)"
    ).unwrap();
    static ref CARDINAL_PATTERN: Regex = Regex::new(
        r"([nsew]{1,2}\+)*"
    ).unwrap();
    static ref K_PATTERN: Regex = Regex::new(
        r"(?:^|[^]])(K)"
    ).unwrap();
    static ref K_PATTERN_DIR: Regex = Regex::new(
        r"(\[[1-8]+\]K)"
    ).unwrap();
    static ref DIR_RANGE_PATTERN: Regex = Regex::new(
        r"\[(?:\.\.(\d+)|(\d+)\.\.|(\d+)\.\.(\d+)|\.\.)\]"
    ).unwrap();
    static ref MULTI_DIR_PATTERN: Regex = Regex::new(
        r"\[([0-9]{2,})\]"
    ).unwrap();
    static ref INVALID_CARDINAL_PATTERN: Regex = Regex::new(
        &[
            r"ne(?:</|<)*?\((?:-\d+|0), (?:-\d+|0)\)|",
            r"ne(?:</|<)*?\((?:-\d+|0), (?:\d+|0)\)|",
            r"ne(?:</|<)*?\((?:\d+|0), (?:-\d+|0)\)|",
            r"se(?:</|<)*\((?:-\d+|0), (?:\d+|0)\)|",
            r"se(?:</|<)*\((?:\d+|0), (?:\d+|0)\)|",
            r"se(?:</|<)*\((?:-\d+|0), (?:-\d+|0)\)|",
            r"nw(?:</|<)*\((?:\d+|0), (?:-\d+|0)\)|",
            r"nw(?:</|<)*\((?:\d+|0), (?:\d+|0)\)|",
            r"nw(?:</|<)*\((?:-\d+|0), (?:-\d+|0)\)|",
            r"sw(?:</|<)*\((?:\d+|0), (?:\d+|0)\)|",
            r"sw(?:</|<)*\((?:\d+|0), (?:-\d+|0)\)|",
            r"sw(?:</|<)*\((?:-\d+|0), (?:\d+|0)\)|",
            r"n(?:</|<)*\([^)]*?, (?:-\d+|0)\)|",
            r"s(?:</|<)*\([^)]*?, (?:\d+|0)\)|",
            r"e(?:</|<)*\((?:-\d+|0)[^)]*?\)|",
            r"w(?:</|<)*\((?:\d+|0)[^)]*?\)"
        ].join("")
    ).unwrap();
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

fn atomize(expr: &str) -> Option<String> {
    let mut atoms = Vec::with_capacity(expr.len());
    for c in expr.chars() {
        atoms.push(betza_atoms(c));
    }
    Some(atoms.join(""))                                                        /* Return Some with joined atoms      */
}


/// Expands ranges in the expression. It handles the following formats:
/// - `{..}`: expands to all possible values
/// - `{n..}`: expands to n to 64
/// - `{..n}`: expands to 1 to n
/// - `*`: expands to 1 to 64
///
/// # Examples
/// ```ignore
/// assert_eq!(expand_ranges("{..}"), Some("{1..64}".to_string()));
/// assert_eq!(expand_ranges("{..5}"), Some("{1..5}".to_string()));
/// assert_eq!(expand_ranges("{5..}"), Some("{5..64}".to_string()));
/// ```
fn expand_ranges(expr: &str) -> Option<String> {
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
                format!("{}{{{}..64}}", prefix, start_str)                      /* Handle n.. format                  */
            }
            _ => format!("{}{{1..64}}", prefix),                                /* Handle .. format                   */
        };
        let cap_str = cap.get(0).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    expanded = expanded.replace(":{1..64}", ":{1..8}");                         /* Limit repetitions to 8             */
    expanded = expanded.replace(":-{1..64}", ":-{1..8}");

    Some(expanded)                                                              /* Return Some with expanded ranges   */
}

/// Expands cardinal directions in the expression. It handles the
/// following formats:
///
/// - `n+s+e+w`: expands to `n|s|e|w`
/// - `n+e`: expands to `n|e`
/// - `n+e+s`: expands to `n|e|s`
fn expand_cardinals(expr: &str) -> Option<String> {
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

/// Expands directions in the expression. It handles the following formats:
/// - `K`: expands to all directions
/// - `K{n}`: expands to all directions with a range
fn expand_directions(expr: &str) -> Option<String> {
    if !expr.contains('K') {
        return Some(expr.to_string());
    }

    let mut expanded = expr.to_string();

    while let Some(cap) = K_PATTERN_DIR.captures(&expanded) {
        let cap_str = cap.get(1).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &cap_str.replace("K", "&"), 1);   /* Replace bare K with all directions */
    }

    while let Some(cap) = K_PATTERN.captures(&expanded) {
        let cap_str = cap.get(1).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &"[12345678]&", 1);   /* Replace bare K with all directions */
    }

    expanded = expanded.replace("&", "K");                                      /* Replace &s with Ks                 */

    while let Some(cap) = DIR_RANGE_PATTERN.captures(&expanded) {
        let range = match (cap.get(1), cap.get(2), cap.get(3), cap.get(4)) {
            (Some(end), _, _, _) => {
                let e = end.as_str().parse().unwrap();
                1..=e                                                           /* Handle ..n range format            */
            },
            (_, Some(start), _, _) => {
                let s = start.as_str().parse().unwrap();
                s..=8                                                           /* Handle n.. range format            */
            },
            (_, _, Some(start), Some(end)) => {
                let s = start.as_str().parse::<usize>().unwrap();
                let e = end.as_str().parse().unwrap();
                s..=e                                                           /* Handle n..m range format           */
            },
            _ => 1..=8,                                                         /* Handle .. (all) range format       */
        };

        let range_str = range.map(|n| n.to_string()).collect::<String>();
        let replacement = format!("[{}]", range_str);
        let cap_str = cap.get(0).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    let mut stack = Vec::with_capacity(4);
    stack.push(expanded);
    let mut result_stack = Vec::with_capacity(8);

    while !stack.is_empty() {
        let term = stack.pop().unwrap();

        if !MULTI_DIR_PATTERN.is_match(&term) {
            result_stack.push(term);
            continue;
        }

        let cap = MULTI_DIR_PATTERN.captures(&term).unwrap();
        let range = cap.get(1).unwrap().as_str();

        for char in range.chars() {
            let n = format!("[{}]", char);
            stack.push(term.replacen(&format!("[{}]", range), &n, 1));          /* Split directions into single       */
        }
    }

    Some(result_stack.join("|"))                                                /* Return  expanded directions        */
}

/// Vectorizes the expression by replacing cardinal directions with
/// their vector representations.
fn vectorize(expr: &str) -> Option<String> {
    if !expr.contains("K") && !expr.contains('#') {
        return Some(expr.to_string());
    }

    Some(expr.to_string()
        .replace("[1]K", "(0, 1)[0]")
        .replace("[2]K", "(1, 1)[1]")
        .replace("[3]K", "(1, 0)[2]")
        .replace("[4]K", "(1, -1)[3]")
        .replace("[5]K", "(0, -1)[4]")
        .replace("[6]K", "(-1, -1)[5]")
        .replace("[7]K", "(-1, 0)[6]")
        .replace("[8]K", "(-1, 1)[7]")
        .replace("#", "(0, 0)"))                                                /* Return Some with vectorized result */
}

fn collapse_cardinals(expr: &str) -> Option<String> {
    if !expr.contains('n') && !expr.contains('s') &&
       !expr.contains('e') && !expr.contains('w') {
        return Some(expr.to_string());
    }

    if INVALID_CARDINAL_PATTERN.is_match(expr) {
        return None;
    }

    Some(expr.to_string()
        .replace("n", "")
        .replace("s", "")
        .replace("e", "")
        .replace("w", ""))
}

fn split_and_process(expr: &str, f: fn(&str) -> Option<String>) -> String {
    if expr.is_empty() {
        return String::new();
    }

    if !expr.contains('|') {
        return f(expr).unwrap_or_default();
    }

    expr.split('|')                                             /* Process in parallel                 */
        .par_bridge()
        .filter_map(f)
        .collect::<Vec<_>>()
        .join("|")
}

pub fn parse_move(expr: &str) -> String {
    let expr = normalize(expr).unwrap_or_default();

    let pipeline = [
        atomize,
        expand_ranges,
        expand_cardinals,
        expand_directions,
        vectorize,
        collapse_cardinals,
    ];

    pipeline
        .iter()
        .fold(expr, |acc, &step| {split_and_process(&acc, step)})               /* Process each step in the pipeline  */
}
