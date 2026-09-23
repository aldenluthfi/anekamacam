//! move_parse.rs
//!
//! Compiles the move notation of a piece into move vectors.
//!
//! A variant gives the moves of each piece in a short Betza-like string.
//! Move generation needs explicit leg vectors. This file expands the
//! directions, ranges, cardinals, chained legs and compound legs into those
//! vectors. It runs once at load time, so move generation reads no text.
//!
//! Each expression goes through one fixed text pipeline. Each stage changes
//! the `|` branches in place:
//!
//! ```text
//!  raw expression     "nW{1..3}|cK"
//!        ↓
//!  normalize          strip redundant parens, canonical spacing
//!        ↓
//!  atomize            rewrite Betza atoms (W, F, N, ...) as `K` forms
//!        ↓
//!  expand_directions  unroll `[a..b$s]` direction spans into copies
//!        ↓
//!  expand_ranges      unroll `{n..m}` / `*` repetition ranges
//!        ↓
//!  expand_cardinals   resolve `n+e`-style cardinal sums to offsets
//!        ↓
//!  atomic / chained / compound / leg conversion into vectors
//! ```
//!
//! A top-level branch can end with a CPMN condition. `generate_move_set`
//! removes it before the pipeline and attaches it to the vectors.
//!
//! Created: 18/02/2024
//! Author : Alden Luthfi

use crate::*;

lazy_static! {
    /// Movement notation lexer tables
    ///
    /// The regexes and cardinal tables of the parse pipeline. Each is made
    /// once, at first use:
    ///
    /// - NORMALIZE_PATTERN      : parens needing explicit chain operators
    /// - RANGE_PATTERN          : `{n..m}` / `*` repetition spans + sign
    /// - DIRECTION_PATTERN      : `[a..b$s]` spans and `[a$s]` steps
    /// - CARDINAL_PATTERN       : `n+e`-style cardinal sum chains
    /// - ATOMIC                 : one `K`-form atom with direction/filter
    /// - ATOMIC_TOKENS          : token alphabet of atomic expressions
    /// - DOTS_TOKEN             : `...` chaining token, optional `-`
    /// - DIRECTION_FILTER_TOKEN : `[n]` direction filter token
    /// - RANGE_TOKEN            : `{n..m}` range token, optional `-`
    /// - COLON_RANGE_TOKEN      : `:{n..m}` colon-range token
    /// - LEG                    : one leg: modifiers, body, `@` tail
    /// - LEG_TOKENS             : token alphabet of multi-leg expressions
    /// - MODIFIERS              : bare modifier-letter run
    ///
    /// - CARDINAL_VECTORS_TO_INDEX : unit (x, y) vector to index 0-7
    /// - DIRECTION_VECTOR_SETS     : direction letter to unit vector set
    /// - CARDINAL_STR_TO_INDEX     : cardinal name ("n".."nw") to index
    /// - CARDINAL_INDEX_TO_STR     : index 0-7 to cardinal name
    ///
    /// The cardinal maps use the index of the prelude table: north is 0 and
    /// the others go clockwise. Thus a rotation is an addition.
    ///
    static ref NORMALIZE_PATTERN: Regex =
        Regex::new(r"[^(^|]\(|\)[^()^|]").unwrap_or_else(|e| {
            panic!("Failed to compile NORMALIZE_PATTERN regex: {e}")
        });
    static ref RANGE_PATTERN: Regex =
        Regex::new(r"(-?)(?:\{(?:\.\.(\d+)|(\d+)\.\.|\.\.)\}|\*)")
            .unwrap_or_else(|e| {
                panic!("Failed to compile RANGE_PATTERN regex: {e}")
            });
    static ref DIRECTION_PATTERN: Regex =
        Regex::new(r"\[(\d+)?\.\.(\d+)?(?:\$(\d+))?\]|\[(\d+)\$(\d+)\]")
            .unwrap_or_else(|e| {
                panic!("Failed to compile DIRECTION_PATTERN regex: {e}")
            });
    static ref CARDINAL_PATTERN: Regex =
        Regex::new(r"([nsew]{1,2}\+[nsew]{1,2})+").unwrap_or_else(|e| {
            panic!("Failed to compile CARDINAL_PATTERN regex: {e}")
        });
    static ref ATOMIC: Regex =
        Regex::new(r"(ne|nw|se|sw|n|s|e|w)?(\[\d+\])?K").unwrap_or_else(|e| {
            panic!("Failed to compile ATOMIC regex: {e}")
        });
    static ref ATOMIC_TOKENS: Regex = Regex::new(concat!(
        r"(?:(?:ne|nw|se|sw|n|s|e|w)?(?:\[\d+\])?K)+|",
        r"(?:ne|nw|se|sw|n|s|e|w)|",
        r"\[\d+\]|",
        r"(?:\.+)|",
        r":?\{\d+(?:\.\.(?:\d+|\*))?\}|",
        r"<|>|#"
    ))
    .unwrap_or_else(|e| {
        panic!("Failed to compile ATOMIC_TOKENS regex: {e}")
    });
    static ref DOTS_TOKEN: Regex = Regex::new(r"^-?\.+$")
        .unwrap_or_else(|e| panic!("Failed to compile DOTS_TOKEN regex: {e}"));
    static ref DIRECTION_FILTER_TOKEN: Regex =
        Regex::new(r"^\[\d+\]$").unwrap_or_else(|e| {
            panic!("Failed to compile DIRECTION_FILTER_TOKEN regex: {e}")
        });
    static ref RANGE_TOKEN: Regex =
        Regex::new(r"^-?\{(\d+)(?:\.\.(\d+|\*))?\}$").unwrap_or_else(|e| {
            panic!("Failed to compile RANGE_TOKEN regex: {e}")
        });
    static ref COLON_RANGE_TOKEN: Regex =
        Regex::new(r"^-?:\{(\d+)(?:\.\.(\d+|\*))?\}$")
            .unwrap_or_else(|e| {
                panic!("Failed to compile COLON_RANGE_TOKEN regex: {e}")
            });
    static ref LEG: Regex =
        Regex::new(r"^([mcdukvgtipr!]+)?([^@mcdukvgtipr]+)@?([^@]+)?$")
            .unwrap_or_else(|e| panic!("Failed to compile LEG regex: {e}"));
    static ref LEG_TOKENS: Regex = Regex::new(concat!(
        r"(?:(?:ne|nw|se|sw|n|s|e|w)?(?:\[\d+\])?K)+|",
        r"(?:ne|nw|se|sw|n|s|e|w)|",
        r"\[\d+\]|",
        r"(?:\.+)|",
        r"-(?:\.+)|",
        r"[mcdukvgtipr!]+|",
        r":?\{\d+(?:\.\.(?:\d+|\*))?\}|",
        r"-:\{\d+(?:\.\.(?:\d+|\*))?\}|",
        r"-\{\d+(?:\.\.(?:\d+|\*))?\}|",
        r"</?|/?>|-",
    ))
    .unwrap_or_else(|e| {
        panic!("Failed to compile LEG_TOKENS regex: {e}")
    });
    static ref MODIFIERS: Regex =
        Regex::new(r"^[mcdukvgtipr!]+$").unwrap_or_else(|e| {
            panic!("Failed to compile MODIFIERS regex: {e}")
        });
    static ref CARDINAL_VECTORS_TO_INDEX: HashMap<(i8, i8), usize> = {
        let mut m = HashMap::new();
        m.insert((0, 1), 0);
        m.insert((1, 1), 1);
        m.insert((1, 0), 2);
        m.insert((1, -1), 3);
        m.insert((0, -1), 4);
        m.insert((-1, -1), 5);
        m.insert((-1, 0), 6);
        m.insert((-1, 1), 7);
        m
    };
    static ref DIRECTION_VECTOR_SETS:
        HashMap<&'static str, HashSet<(i8, i8)>> = {
        let mut m = HashMap::new();
        m.insert("n", HashSet::from([(-1, 1), (0, 1), (1, 1)]));
        m.insert("e", HashSet::from([(1, 1), (1, 0), (1, -1)]));
        m.insert("s", HashSet::from([(1, -1), (0, -1), (-1, -1)]));
        m.insert("w", HashSet::from([(-1, -1), (-1, 0), (-1, 1)]));
        m.insert("ne", HashSet::from([(1, 1)]));
        m.insert("se", HashSet::from([(1, -1)]));
        m.insert("sw", HashSet::from([(-1, -1)]));
        m.insert("nw", HashSet::from([(-1, 1)]));
        m
    };
    static ref CARDINAL_STR_TO_INDEX: HashMap<&'static str, i8> = {
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
    static ref CARDINAL_INDEX_TO_STR: HashMap<usize, &'static str> = {
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

/// apply_operator
///
/// Joins two expressions with one operator:
///
/// - `|` : alternatives, side by side
/// - `^` : concatenation, each left branch with each right branch
/// - `#` : the empty expression, a concatenation with it has no effect
///
/// Params:
/// - op: char -> operator, `^` (concat) or `|` (alternation)
/// - a : &str -> left operand, can have `|` branches
/// - b : &str -> right operand, can have `|` branches
///
/// Return:
/// String     -> the combined expression
///
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

/// precedence
///
/// Gives the precedence of an operator for the stack evaluator.
/// Concatenation (`^`) binds more strongly than alternation (`|`).
///
/// Params:
/// - op: char -> operator, `^` or `|`
///
/// Return:
/// usize      -> precedence, higher binds more strongly
///
fn precedence(op: char) -> usize {
    match op {
        '^' => 2,
        '|' => 1,
        _ => unreachable!("Invalid operator: {}", op),
    }
}

/// betza_atoms
///
/// Converts a Betza atom into the same move in Cheesy King Notation (CKN).
/// Thus all later stages see only king steps. Betza names the target
/// square. CKN names the route. A route digit selects a heading on the
/// cardinal circle, clockwise from the current direction. The first atom
/// points north, so 1 is north, odd digits are orthogonal and even digits
/// are diagonal. After a diagonal step, the circle turns, so `N` is a
/// knight, not a double ferz:
///
/// ```text
/// ┌───┬──────────────────────┬───────────────────────────────────┐
/// │ W │ <[1357]K>            │ wazir, one step orthogonally      │
/// │ F │ <[2468]K>            │ ferz, one step diagonally         │
/// │ A │ <[2468]K.>           │ alfil, the ferz step doubled      │
/// │ D │ <[1357]K.>           │ dabbabah, the wazir step doubled  │
/// │ S │ <K.>                 │ alibaba, either of them doubled   │
/// │ N │ <[2468]Kn[2468]K>    │ knight                            │
/// │ C │ <[2468]Kn<[2468]K>.> │ camel                             │
/// │ Z │ <[2468]K.n[2468]K>   │ zebra                             │
/// │ G │ <[2468]K..>          │ griffin, the ferz step tripled    │
/// │ H │ <[1357]K..>          │ hawk, the wazir step tripled      │
/// │ T │ <K..>                │ grawk, either of them tripled     │
/// │ B │ <[2468]K-*>          │ bishop, the ferz step repeated    │
/// │ R │ <[1357]K-*>          │ rook, the wazir step repeated     │
/// │ Q │ <K-*>                │ queen, either of them repeated    │
/// └───┴──────────────────────┴───────────────────────────────────┘
/// ```
///
/// Other characters stay the same, so a variant can write CKN directly.
///
/// Params:
/// - piece: char -> Betza atom letter, e.g. 'N', 'R', 'Q'
///
/// Return:
/// String        -> the CKN form, or the letter if unknown
///
fn betza_atoms(piece: char) -> String {
    match piece {
        'W' => "<[1357]K>".to_string(),
        'F' => "<[2468]K>".to_string(),
        'A' => "<[2468]K.>".to_string(),
        'D' => "<[1357]K.>".to_string(),
        'S' => "<K.>".to_string(),
        'N' => "<[2468]Kn[2468]K>".to_string(),
        'C' => "<[2468]Kn<[2468]K>.>".to_string(),
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

/// evaluate
///
/// Flattens a normalized expression into `|` branches. It uses an operand
/// stack and an operator stack. It applies an operator when an operator of
/// equal or higher precedence arrives. Thus no parentheses stay at the end.
///
/// Params:
/// - expr: &str -> normalized expression with explicit `^` operators
///
/// Return:
/// String       -> flat `|` form without parentheses
///
/// Notes:
/// A bad expression causes a panic at load time, with the expression text.
///
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
                    let a = operands.pop().unwrap_or_else(|| {
                        panic!(
                            "Malformed expression '{}': missing right operand",
                            expr
                        )
                    });
                    let b = operands.pop().unwrap_or_else(|| {
                        panic!(
                            "Malformed expression '{}': missing left operand",
                            expr
                        )
                    });
                    let combined = apply_operator(op, &b, &a);
                    operands.push(combined);
                }
                i += 1;
            }
            '^' | '|' => {
                while let Some(&op) = operators.last() {
                    if op != '(' && precedence(op) >= precedence(c) {
                        let a = operands.pop().unwrap_or_else(|| {
                            panic!(
                                "Malformed expr '{}': missing right operand",
                                expr
                            )
                        });
                        let b = operands.pop().unwrap_or_else(|| {
                            panic!(
                                "Malformed expr '{}': missing left operand",
                                expr
                            )
                        });
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

    while let Some(op) = operators.pop() {                                      /* Eval remaining operators           */
        let a = operands.pop().unwrap_or_else(|| {
            panic!(
                "Malformed expression '{}': missing right operand",
                expr
            )
        });
        let b = operands.pop().unwrap_or_else(|| {
            panic!(
                "Malformed expression '{}': missing left operand",
                expr
            )
        });
        let combined = apply_operator(op, &b, &a);
        operands.push(combined);
    }

    operands.pop().unwrap_or_else(|| {
        panic!("Malformed expression '{}': no final operand", expr)
    })                                                                          /* Final result is the only operand   */
}

/// normalize
///
/// Converts a raw expression into the form of the evaluator. Two parts
/// next to each other mean "and then", so the function writes each implied
/// concatenation at a parenthesis as `^`. Then it evaluates the result. An
/// expression without parentheses does not change.
///
/// Params:
/// - expr: &str   -> raw move expression from the config
///
/// Return:
/// Option<String> -> canonical `|` expression
///
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

/// atomize
///
/// Sends each character of a branch through [`betza_atoms`]. Each Betza
/// letter becomes its CKN route. Other characters stay the same. CKN uses
/// `K`, lower case headings and punctuation, and none of these is an atom.
///
/// Params:
/// - expr: &str   -> one clean branch, without `|`
///
/// Return:
/// Option<String> -> the branch with all atoms expanded
///
fn atomize(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");


    log_4!("Starting atomization of expression: {}", expr);

    let mut atoms = Vec::with_capacity(expr.len());
    for c in expr.chars() {
        atoms.push(betza_atoms(c));
    }
    Some(atoms.join(""))                                                        /* Return Some with joined atoms      */
}

/// expand_directions
///
/// Writes each direction filter of a branch as its explicit direction set.
/// The digits count the circle clockwise from north. `..` gives a range
/// and `$` removes digits:
///
/// - `[1..8]`      : full range `[12345678]`
/// - `[..5]`       : open low end `[12345]`
/// - `[5..]`       : open high end `[5678]`
/// - `[1..7$25]`   : range minus exclusions `[13467]`
/// - `[1235678$2]` : explicit set minus exclusions `[135678]`
///
/// An open end uses the circle, not the atom. `[..]` is all eight. A
/// later stage removes the directions that the atom does not have.
///
/// Params:
/// - expr: &str   -> one clean branch, without `|`
///
/// Return:
/// Option<String> -> branch with each direction filter written in full
///
/// Notes:
/// The loop changes one filter each turn and reads the result again. It
/// stops, because a plain digit list has no `..` or `$`.
///
fn expand_directions(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");


    log_4!("Starting direction expansion of expression: {}", expr);

    let mut expanded = expr.to_string();

    while let Some(cap) = DIRECTION_PATTERN.captures(&expanded) {
        let digits = if cap.get(4).is_some() {
            let digits_str = cap.get(4).unwrap_or_else(|| {
                panic!("Direction capture is missing explicit digit group")
            }).as_str();
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
                .get(1)
                .and_then(|m| m.as_str().parse::<u8>().ok())
                .unwrap_or(1);
            let end = cap
                .get(2)
                .and_then(|m| m.as_str().parse::<u8>().ok())
                .unwrap_or(8);
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
        let cap_str = cap.get(0).unwrap_or_else(|| {
            panic!("Direction pattern capture has no full-match group")
        }).as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    Some(expanded)
}

/// expand_ranges
///
/// Writes each repetition range in its explicit form:
///
/// - `{..}`  : all counts, `{1..*}`
/// - `{n..}` : n and more, `{n..*}`
/// - `{..n}` : 1 to n, `{1..n}`
/// - `*`     : short form of `{..}`
///
/// Params:
/// - expr: &str   -> one clean branch, without `|`
///
/// Return:
/// Option<String> -> branch with each range in explicit form
///
/// Notes:
/// During the loop, the open upper bound is `&`. At the end it becomes `*`.
/// Else the loop would expand the new `*` again.
///
fn expand_ranges(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");


    log_4!("Starting range expansion of expression: {}", expr);

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
        let cap_str = cap.get(0).unwrap_or_else(|| {
            panic!("Range pattern capture has no full-match group")
        }).as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    Some(expanded.replace('&', "*"))                                            /* Return Some with expanded ranges   */
}

/// expand_cardinals
///
/// Splits the `+` cardinal sums of one branch into separate branches. Then
/// each term has one heading:
///
/// - `n+eK`     : `nK`, `eK`
/// - `n+e+sK`   : `nK`, `eK`, `sK`
/// - `n+eKs+wK` : `nKsK`, `nKwK`, `eKsK`, `eKwK`
///
/// Params:
/// - expr: &str   -> one clean branch, without `|`
///
/// Return:
/// Option<String> -> `|` branches, one for each cardinal combination
///
/// Notes:
/// The split uses a work stack. Each turn splits one `+`, and both halves go
/// back onto the stack. Thus two sums give the full cross product. The
/// order of the branches can change, and duplicates are removed.
///
fn expand_cardinals(expr: &str) -> Option<String> {
    assert!(!expr.contains("|"), "{expr} must be sanitized before parsing.");


    log_4!("Starting cardinal expansion of expression: {}", expr);

    if !expr.contains('+') {
        return Some(expr.to_string());
    }

    let mut stack = vec![expr.to_string()];
    let mut result_stack = Vec::new();

    while let Some(term) = stack.pop() {

        log_4!("expand_cardinals processing term: {}", term);

        if !CARDINAL_PATTERN.is_match(&term) {
            result_stack.push(term);
            continue;
        }

        let cap = CARDINAL_PATTERN
            .captures(&term)
            .unwrap_or_else(|| {
                panic!(
                    "Cardinal pattern failed to match \
                     term '{}', despite pre-check",
                    term
                )
            });

        let cardinals = cap.get(0).unwrap_or_else(|| {
            panic!("Cardinal capture missing full-match group for '{term}'")
        }).as_str();

        let split = cardinals.split('+').collect::<Vec<&str>>();

        for cardinal in &split {
            stack.push(term.replacen(cardinals, cardinal, 1));                  /* Replace combined cardinals         */
        }
    }

    remove_duplicates_in_place(&mut result_stack);
    Some(result_stack.join("|"))                                                /* Return Some with expanded cardinals*/
}

/// split_and_process
///
/// Runs one pipeline stage on each `|` branch of an expression. Each stage
/// asserts that it gets one branch, so only this function splits the
/// alternatives. A stage gets the trimmed branch.
///
/// Params:
/// - expr: &str                       -> expression with `|` branches
/// - f   : impl Fn(&str) -> Option<T> -> stage for each branch
///
/// Return:
/// Vec<Option<T>>                     -> one result for each branch, in order
///
/// Notes:
/// A rejected branch gives `None` in its slot, so the list keeps its
/// positions. The `Sync` and `Send` bounds allow a parallel walk later.
///
fn split_and_process<T>(
    expr: &str,
    f: impl Fn(&str) -> Option<T> + Sync,
) -> Vec<Option<T>>
where
    T: Send + Sync,
{
    assert!(!expr.is_empty(), "expr must not be empty.");

    expr.split('|').map(|s| f(s.trim())).collect()
}

/// parse_move_string
///
/// Sends one raw move expression through the full text pipeline. The
/// result is a `|` list of branches with all atoms, directions, ranges and
/// headings written in full:
///
/// ```text
/// normalize → atomize → expand_directions → expand_ranges → expand_cardinals
/// ```
///
/// `normalize` reads the full expression. Each later stage reads one branch
/// and can give many. The branches join with `|` before the next stage. The
/// vector compilers read the result: `atomic_to_vector`,
/// `chained_atomic_to_vector`, `compound_atomic_to_vector` and
/// `leg_to_vector`.
///
/// Params:
/// - expr: &str -> raw move expression from the config
///
/// Return:
/// String       -> the normalized and expanded `|` expression
///
/// Notes:
/// A stage that gives `None` would remove its branch. No stage does this.
///
fn parse_move_string(expr: &str) -> String {

    log_4!("Starting parse_move_string with expression: {}", expr);

    let expr = normalize(expr)
        .unwrap_or_else(|| panic!("Failed to normalize expression: {}", expr));


    log_4!("Normalized expression: {}", expr);

    let pipeline =
        [atomize, expand_directions, expand_ranges, expand_cardinals];

    pipeline.iter().fold(expr, |acc, &step| {
        let parts: Vec<String> =
            split_and_process(&acc, step).into_iter().flatten().collect();
        parts.join("|")
    })                                                                          /* Process each step in the pipeline  */
}

/// irregular_vector_direction
///
/// Gives the main direction of an irregular vector. The axis with the
/// larger size wins. Equal sizes give the diagonal between them. On a 9x9
/// field around the origin `O`:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │ nw │ n  │ n  │ n  │ n  │ n  │ n  │ n  │ ne │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ w  │ nw │ n  │ n  │ n  │ n  │ n  │ ne │ e  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ w  │ w  │ nw │ n  │ n  │ n  │ ne │ e  │ e  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ w  │ w  │ w  │ nw │ n  │ ne │ e  │ e  │ e  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ w  │ w  │ w  │ w  │ O  │ e  │ e  │ e  │ e  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ w  │ w  │ w  │ sw │ s  │ se │ e  │ e  │ e  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ w  │ w  │ sw │ s  │ s  │ s  │ se │ e  │ e  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ w  │ sw │ s  │ s  │ s  │ s  │ s  │ se │ e  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ sw │ s  │ s  │ s  │ s  │ s  │ s  │ s  │ se │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// Examples:
///
/// - (2, 1) : "e", (1, 0)
/// - (1, 2) : "n", (0, 1)
/// - (2, 2) : "ne", (1, 1)
///
/// Params:
/// - vector: &(i8, i8) -> displacement to classify
///
/// Return:
/// &'static str        -> main direction name ("n", "ne", ...)
///
/// Notes:
/// The name comes from a static map, so the return is `'static`. Thus a
/// caller can give a temporary vector.
///
fn irregular_vector_direction(vector: &(i8, i8)) -> &'static str {
    let abs_x = vector.0.saturating_abs();
    let abs_y = vector.1.saturating_abs();

    let direction_vector = if abs_x > abs_y {
        (vector.0.signum(), 0)
    } else if abs_y > abs_x {
        (0, vector.1.signum())
    } else {
        (vector.0.signum(), vector.1.signum())
    };

    let index = *CARDINAL_VECTORS_TO_INDEX
        .get(&direction_vector)
        .unwrap_or_else(|| panic!("Invalid direction vector: {:?}", vector));

    *CARDINAL_INDEX_TO_STR.get(&index).unwrap_or_else(|| {
        panic!("Invalid index for inverse cardinal map: {}", index)
    })
}

/// sort_atomic_clockwise
///
/// Sorts the eight cardinal results in one fixed clockwise order, so a
/// direction filter can name them by number. The sort key is the `atan2`
/// angle. The order is NE, E, SE, S, SW, W, NW, N, with north last. On a
/// 9x9 field around the origin `O`:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │ 6  │    │    │    │ 7  │    │    │    │ 0  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ 5  │    │    │    │ O  │    │    │    │ 1  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ 4  │    │    │    │ 3  │    │    │    │ 2  │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - vectors: Vec<AtomicVector> -> the eight cardinal results to sort
///
/// Return:
/// Vec<AtomicVector>            -> the eight vectors in clockwise order
///
/// Notes:
/// A filter digit is the index plus one, so `[1]` on a group selects
/// north-east. A `[1]` directly on a `K` does not come here.
/// [`atomic_to_vector`] reads it with the prelude table, where north is
/// first. Fewer than eight vectors cause a panic.
///
fn sort_atomic_clockwise(mut vectors: Vec<AtomicVector>) -> Vec<AtomicVector> {
    assert_eq!(
        vectors.len(),
        8,
        "Can only sort 8 cardinal directions clockwise"
    );

    vectors.sort_by(|a, b| {
        let a_tuple = a.as_tuple();
        let b_tuple = b.as_tuple();

        let a_angle = (-a_tuple[0].0 as f32).atan2(-a_tuple[0].1 as f32);
        let b_angle = (-b_tuple[0].0 as f32).atan2(-b_tuple[0].1 as f32);

        a_angle.partial_cmp(&b_angle).unwrap_or_else(|| {
            panic!(
                "Failed to compare atomic angles: {a_angle} vs {b_angle}"
            )
        })
    });

    vectors
}

/// quadrant_function
///
/// Gives the four half-plane tests of one rotated frame. Thus a filter `n`
/// means north of the frame, not north of the board. An orthogonal frame
/// tests the axes. A diagonal frame tests the two diagonals:
///
/// - ne : "up" is right of the line x=-y, so x + y > 0
/// - se : "right" is left of the line x=-y, so x + y < 0
/// - nw : "up" is left of the line x=y, so x - y < 0
/// - sw : "right" is right of the line x=y, so x - y > 0
///
/// The `ne` frame on a 9x9 field. `n` is north (x + y > 0), `s` is south,
/// and the empty anti-diagonal through `O` is on no side:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │ n  │ n  │ n  │ n  │ n  │ n  │ n  │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │    │ n  │ n  │ n  │ n  │ n  │ n  │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │ s  │    │ n  │ n  │ n  │ n  │ n  │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │ s  │ s  │    │ n  │ n  │ n  │ n  │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │ s  │ s  │ s  │ O  │ n  │ n  │ n  │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │ s  │ s  │ s  │ s  │    │ n  │ n  │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │ s  │ s  │ s  │ s  │ s  │    │ n  │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │ s  │ s  │ s  │ s  │ s  │ s  │    │ n  │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │ s  │ s  │ s  │ s  │ s  │ s  │ s  │ s  │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// Params:
///
///     direction: &str
///     cardinal name of the rotated frame
///
/// Return:
///
///     impl Fn(i8, i8) -> (bool, bool, bool, bool)
///     the north, east, south and west tests of that frame, for one point
///
/// Notes:
/// A quadrant uses two tests, for example `ne` is north and east. An
/// unknown name causes a panic.
///
fn quadrant_function(
    direction: &str,
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
        _ => panic!("Invalid rotation direction: {}", direction),
    }
}

/// Repetition token macros
///
/// Read the bounds of the two repetition forms. They are private to this
/// file.
///
/// - `{i..j}`  : repeats the atom before it
/// - `:{i..j}` : evaluates the element before it again
///
/// range_bounds!
///
///   Matches a repetition token and gives its bounds. An open end is
///   `i8::MAX`, and the callers walk until a round adds no new vector.
///   `label` names the form in the messages.
///
///   Params:
///   - regex: expr    -> the compiled token regex
///   - label: literal -> the form name in messages
///   - token: &str    -> the token to expand
///
///   Return:
///   (Option<i8>, Option<i8>) -> start bound, end bound
///
/// colon_range_head!
///
///   Rejects a `:{i..j}` with nothing before it, then reads its bounds. The
///   atomic and multi-leg element enums are different types, so the variant
///   name is a parameter. `element` is used three times, so give a
///   binding.
///
///   Params:
///   - token  : &str            -> the `:{i..j}` to expand
///   - element: Option<Element> -> element to repeat
///   - eval   : ident           -> the evaluated variant of that enum
///
///   Return:
///   (Element, Option<i8>, Option<i8>) -> element, start bound, end bound
///
macro_rules! range_bounds {
    ($regex:expr, $label:literal, $token:expr) => {{
        let captures = $regex.captures($token).unwrap();

        log_4!(concat!($label, " token captures: {:?}"), captures);

        (
            captures.get(1).map(|start| start.as_str().parse::<i8>()
                .expect(concat!("Invalid start ", $label, " token."))),
            captures.get(2).map(|end| end.as_str()
                .parse::<i8>().unwrap_or(i8::MAX)),
        )
    }};
}

macro_rules! colon_range_head {
    ($token:expr, $element:expr, $eval:ident) => {{
        if $element.is_none() {
            panic!(
                "Colon-range token must be preceded by an atomic element: {:?}",
                $token
            );
        }

        if let Some($eval(_)) = $element {
            panic!(
                "Colon-range token cant be preceded by an evaluated expr: {:?}",
                $token
            );
        }

        let (start, end) =
            range_bounds!(COLON_RANGE_TOKEN, "colon-range", $token);

        ($element.unwrap(), start, end)
    }};
}

/*----------------------------------------------------------------------------*\
                          ATOMIC EXPRESSION EVALUATION
\*----------------------------------------------------------------------------*/

/// filter_atomic_by_index
///
/// Keeps the vectors that a direction filter names. The function sorts the
/// set with [`sort_atomic_clockwise`] first, so the input order is not
/// important. The caller converts the digits into indices (digit - 1).
///
/// Params:
/// - vectors: Vec<AtomicVector> -> the eight cardinal candidates
/// - index  : Vec<usize>        -> clockwise indices to keep
///
/// Return:
/// Vec<AtomicVector>            -> the selected vectors, in index order
///
/// Notes:
/// A repeated index gives the vector two times. `[9]` causes a panic. A set
/// of fewer than eight vectors causes a panic.
///
fn filter_atomic_by_index(
    mut vectors: Vec<AtomicVector>,
    index: Vec<usize>,
) -> Vec<AtomicVector> {
    let mut result: Vec<AtomicVector> = Vec::new();


    log_4!("filter_atomic_by_index sorted indices: {:?}", index);

    assert_eq!(
        vectors.len(),
        8,
        "Can only filter from 8 cardinal directions clockwise"
    );

    vectors = sort_atomic_clockwise(vectors);


    log_4!("filter_atomic_by_index sorted vectors: {:?}", vectors);

    for i in index {
        result.push(vectors[i]);
    }

    result
}

/// filter_atomic_by_cardinal_direction
///
/// Keeps the vectors in the half-plane or quadrant that `direction` names,
/// in the frame `pov`. The test uses the net displacement of each branch,
/// not its route. A vector on a fold line is in no quadrant next to it.
///
/// Params:
/// - vectors  : Vec<AtomicVector> -> candidate set
/// - direction: &str              -> cardinal or diagonal to keep
/// - pov      : &str              -> rotation frame of the test
///
/// Return:
/// Vec<AtomicVector>              -> vectors that pass the test
///
fn filter_atomic_by_cardinal_direction(
    mut vectors: Vec<AtomicVector>,
    direction: &str,
    pov: &str,
) -> Vec<AtomicVector> {

    log_4!(
        "filter_atomic_by_cardinal_direction direction: {} w.r.t {}",
        direction, pov
    );

    let pov_fn = quadrant_function(pov);

    vectors.retain(|vector| {
        let sum = vector.whole();
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
            _ => panic!("Invalid rotation direction: {}", direction),
        }
    });

    vectors
}

/// filter_atomic_out_of_bounds
///
/// Removes displacements that are too large for the board from any square.
/// This also stops an open repetition. The bound is the board width and
/// height, because the origin is not known yet.
///
/// Params:
/// - vector: &mut Vec<AtomicVector> -> candidate set, changed in place
/// - state : &State                 -> board dimensions for the bound
///
fn filter_atomic_out_of_bounds(
    vector: &mut Vec<AtomicVector>,
    state: &State,
) {
    vector.retain(|vector| {
        let whole = vector.whole();
        whole.0.saturating_abs() <= state.statics.files as i8
            && whole.1.saturating_abs() <= state.statics.ranks as i8
    });
}

/// remove_duplicates_in_place
///
/// Removes duplicates from a set and keeps the first copy. Two routes can
/// give the same displacement, and a duplicate would give a second equal
/// move.
///
/// Params:
/// - vectors: &mut Vec<Element> -> set to change in place
///
/// Notes:
/// The first order stays. Thus the repetition loops see no new vector when
/// the length does not change.
///
fn remove_duplicates_in_place<Element>(vectors: &mut Vec<Element>)
where
    Element: Clone + Eq + Hash,
{
    let mut seen = HashSet::new();
    vectors.retain(|vector| seen.insert(vector.clone()));
}

/// process_atomic_dots_token
///
/// Extends each branch along its own last heading, one square for each dot.
/// Thus one token extends all headings. The wazir step to the east of `S`,
/// with more dots:
///
/// ```text
/// ┌─────┬─────┬─────┬─────┐
/// │  S  │  K  │ K.  │ K.. │
/// └─────┴─────┴─────┴─────┘
/// ```
///
/// Params:
/// - vector_set: Vec<AtomicVector> -> branches until now
/// - token     : &str              -> the dots to apply
/// - state     : &State            -> board dimensions for the clip
///
/// Return:
/// Vec<AtomicVector>               -> the extended and clipped set
///
/// Notes:
/// A branch past the board is removed, not shortened. Two branches on one
/// square become one.
///
fn process_atomic_dots_token(
    vector_set: Vec<AtomicVector>,
    token: &str,
    state: &State,
) -> Vec<AtomicVector> {
    let dots_count = token.len() as i8;

    let mut updated_vectors: Vec<AtomicVector> = vector_set
        .into_iter()
        .map(|vector| {
            let mut new_vector = vector;
            new_vector.set(&new_vector.add_last(dots_count));
            new_vector
        })
        .collect();

    filter_atomic_out_of_bounds(&mut updated_vectors, state);
    remove_duplicates_in_place(&mut updated_vectors);
    updated_vectors
}

/// process_atomic_range_token
///
/// Repeats the atom before the token for the counts of the range. A count
/// is the total number of steps:
///
/// - `{1}` : `K`
/// - `{2}` : `K.`
/// - `{3}` : `K..`
///
/// A single count gives one branch for each input. A span gives the union,
/// so a slide gives each square where it can stop.
///
/// Params:
/// - vector_set: Vec<AtomicVector> -> branches until now
/// - token     : &str              -> the `{i..j}` range token
/// - state     : &State            -> board dimensions for the clip
///
/// Return:
/// Vec<AtomicVector>               -> the new and clipped set
///
/// Notes:
/// An open end runs until a round adds no new vector. The byte arithmetic
/// saturates. The board clip runs once at the end. A token without a lower
/// bound causes a panic, because [`expand_ranges`] always writes one.
///
fn process_atomic_range_token(
    vector_set: Vec<AtomicVector>,
    token: &str,
    state: &State,
) -> Vec<AtomicVector> {
    let (start, end) = range_bounds!(RANGE_TOKEN, "range", token);

    match (start, end) {
        (Some(start_count), Some(end_count)) => {
            let mut all_updated_vectors: Vec<AtomicVector> = Vec::new();

            for count in start_count..=end_count {
                let updated_vectors: Vec<_> = vector_set
                    .iter()
                    .map(|vector| {
                        let mut new_vector = *vector;
                        new_vector.set(&new_vector.add_last(count - 1));
                        new_vector
                    })
                    .collect();

                let prev_len = all_updated_vectors.len();
                all_updated_vectors.extend(updated_vectors);
                remove_duplicates_in_place(&mut all_updated_vectors);
                if all_updated_vectors.len() == prev_len {
                    break;                                                      /* No new vectors are added so break  */
                }
            }

            let mut result = all_updated_vectors;

            filter_atomic_out_of_bounds(&mut result, state);
            result
        }
        (Some(count), None) => {
            let mut updated_vectors: Vec<AtomicVector> = vector_set
                .into_iter()
                .map(|vector| {
                    let mut new_vector = vector;
                    new_vector.set(&new_vector.add_last(count - 1));
                    new_vector
                })
                .collect();

            filter_atomic_out_of_bounds(&mut updated_vectors, state);
            remove_duplicates_in_place(&mut updated_vectors);
            updated_vectors
        }
        _ => {
            panic!("Invalid range token: {:?}", token);
        }
    }
}

/// process_atomic_colon_range_token
///
/// Repeats the element before the token. It evaluates the element again
/// for each count and adds the result to each branch.
///
/// - `{i..j}`  : extends along the last step, the heading does not change
/// - `:{i..j}` : runs the element again, with its filters and rotation
///
/// Thus a repeated route can turn, but a repeated step cannot. A span adds
/// the branches of each count and stops at the first count that adds
/// nothing new.
///
/// Params:
/// - vector_set: Vec<AtomicVector>               -> branches until now
/// - token     : &str                            -> the `:{i..j}` range
/// - element   : Option<AtomicElement>           -> element before it
/// - modifiers : &(Option<Token>, Option<Token>) -> waiting filters
/// - state     : &State                          -> board dimensions
///
/// Return:
/// Vec<AtomicVector>                             -> the repeated set
///
/// Notes:
/// With nothing to repeat before it, [`colon_range_head!`] panics. A count
/// without a lower bound panics here.
///
fn process_atomic_colon_range_token(
    vector_set: Vec<AtomicVector>,
    token: &str,
    element: Option<AtomicElement>,
    modifiers: &(Option<Token>, Option<Token>),
    state: &State,
) -> Vec<AtomicVector> {
    let (element, start, end) =
        colon_range_head!(token, element, AtomicEval);
    let mut result: Vec<AtomicVector> = Vec::new();

    match (start, end) {
        (Some(start_count), Some(end_count)) => {
            let mut prev_len = 0;

            for count in start_count..=end_count {
                let multiplied_expr =
                    VecDeque::from(vec![element.clone(); count as usize]);

                for branch_vector in &vector_set {
                    let mut extension = Vec::new();
                    let eval = evaluate_atomic_subexpression(
                        vector_set.clone(),
                        multiplied_expr.clone(),
                        modifiers,
                        state,
                    );

                    for vector in eval {
                        extension.push(branch_vector.add(&vector))
                    }

                    result.extend(extension);
                }

                remove_duplicates_in_place(&mut result);
                if prev_len == result.len() {
                    break;                                                      /* No new vectors are added so break  */
                }
                prev_len = result.len();
            }

            filter_atomic_out_of_bounds(&mut result, state);
            result
        }
        (Some(count), None) => {
            let multiplied_expr = VecDeque::from(vec![element; count as usize]);

            for branch_vector in &vector_set {
                let mut extension = Vec::new();
                let eval = evaluate_atomic_subexpression(
                    vector_set.clone(),
                    multiplied_expr.clone(),
                    modifiers,
                    state,
                );

                for vector in eval {
                    extension.push(branch_vector.add(&vector))
                }

                result.extend(extension);
            }

            remove_duplicates_in_place(&mut result);
            filter_atomic_out_of_bounds(&mut result, state);
            result
        }
        _ => {
            panic!("Invalid colon-range token: {:?}", token);
        }
    }
}

/// process_atomic_modifiers
///
/// Applies the filters before the current element:
///
/// - cardinal : keeps the half-plane or quadrant that it names
/// - index    : keeps the clockwise positions of its digits
/// - none     : keeps the full set
///
/// Params:
/// - vector_set: Vec<AtomicVector>               -> set to filter
/// - modifiers : &(Option<Token>, Option<Token>) -> waiting (cardinal, index)
/// - rotation  : &str                            -> current rotation frame
///
/// Return:
/// Vec<AtomicVector>                             -> the filtered set
///
/// Notes:
/// The cardinal filter runs first. The index filter needs all eight vectors
/// and asserts it, so the two together stop there. Each digit minus one is
/// an index. Other characters are skipped.
///
fn process_atomic_modifiers(
    mut vector_set: Vec<AtomicVector>,
    modifiers: &(Option<Token>, Option<Token>),
    rotation: &str,
) -> Vec<AtomicVector> {

    log_4!(
        "process_modifiers with modifiers: {:?}, rotation: {}",
        modifiers, rotation
    );

    match modifiers {
        (Some(CardinalToken(direction)), Some(FilterToken(indices))) => {
            vector_set = filter_atomic_by_cardinal_direction(
                vector_set, direction, rotation,
            );
            let index_vec: Vec<usize> = indices
                .chars()
                .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
                .collect();
            filter_atomic_by_index(vector_set, index_vec)
        }
        (Some(CardinalToken(direction)), None) => {
            filter_atomic_by_cardinal_direction(vector_set, direction, rotation)
        }
        (None, Some(FilterToken(indices))) => {
            let index_vec: Vec<usize> = indices
                .chars()
                .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
                .collect();
            filter_atomic_by_index(vector_set, index_vec)
        }
        _ => vector_set,                                                        /* Do nothing                         */
    }
}

/// evaluate_atomic_term
///
/// Applies one atom token to each branch and extends each branch with the
/// result. The frame of the atom is the last heading of the branch. The
/// filters use the same frame, so `n` after a diagonal step means north of
/// that step.
///
/// Params:
/// - result   : Vec<AtomicVector>               -> branches until now
/// - term     : Token                           -> the atom token to expand
/// - modifiers: &(Option<Token>, Option<Token>) -> waiting (cardinal, index)
///
/// Return:
/// Vec<AtomicVector>                            -> the extended set
///
/// Notes:
/// Another token type causes a panic. Two branches on one square become
/// one.
///
fn evaluate_atomic_term(
    result: Vec<AtomicVector>,
    term: Token,
    modifiers: &(Option<Token>, Option<Token>),
) -> Vec<AtomicVector> {
    let atomic = match term {
        AtomicToken(atomic) => atomic,
        _ => {
            panic!(
                "Unexpected atomic term in evaluate_atomic_term: {:?}",
                term
            );
        }
    };

    let mut new_result: Vec<AtomicVector> = Vec::new();
    for branch_vector in &result {
        let rotation = irregular_vector_direction(&branch_vector.last());

        let mut extension = Vec::new();
        let mut eval = chained_atomic_to_vector(&atomic, rotation);

        eval = process_atomic_modifiers(eval, modifiers, rotation);

        for vector in eval {
            extension.push(branch_vector.add(&vector))
        }

        new_result.extend(extension);
    }
    remove_duplicates_in_place(&mut new_result);
    new_result
}

/// evaluate_atomic_subexpression
///
/// Evaluates a `<...>` group once for each branch, in the frame of that
/// branch, and adds the result to it. The next heading is the net
/// displacement of the group, not its last step. A chain without brackets
/// gives only its last step.
///
/// Params:
/// - result   : Vec<AtomicVector>               -> branches until now
/// - subexpr  : AtomicGroup                     -> the `<...>` token group
/// - modifiers: &(Option<Token>, Option<Token>) -> waiting (cardinal, index)
/// - state    : &State                          -> board dimensions
///
/// Return:
/// Vec<AtomicVector>                            -> the extended set
///
/// Notes:
/// A group result that is not vectors causes a panic. Two branches on one
/// square become one.
///
fn evaluate_atomic_subexpression(
    result: Vec<AtomicVector>,
    subexpr: AtomicGroup,
    modifiers: &(Option<Token>, Option<Token>),
    state: &State,
) -> Vec<AtomicVector> {
    let mut new_result: Vec<AtomicVector> = Vec::new();
    for branch_vector in &result {
        let branch_rotation =
            irregular_vector_direction(&branch_vector.last());

        let mut extension = Vec::new();
        let eval_result = evaluate_atomic_expression(
            subexpr.clone(),
            branch_rotation,
            state,
        );

        let mut eval = match eval_result {
            AtomicEval(vectors) => vectors,
            _ => {
                panic!(
                    "Unexpected element in nested expression: {:?}",
                    eval_result
                );
            }
        };

        eval = process_atomic_modifiers(eval, modifiers, branch_rotation);
        for vector in eval {
            extension.push(branch_vector.add(&vector))
        }

        new_result.extend(extension);
    }

    remove_duplicates_in_place(&mut new_result);
    new_result
}

/// evaluate_atomic_expression
///
/// Walks a token group from left to right with one branch set. The set
/// starts as one empty branch with the heading `rotation`. A cardinal or
/// index filter waits and goes to the next atom or group. A colon range
/// after that atom gets it first, so each atom is read with the token
/// after it.
///
/// Params:
/// - expr    : AtomicGroup -> the token group to evaluate
/// - rotation: &str        -> rotation of the expression
/// - state   : &State      -> board dimensions for the clip
///
/// Return:
/// AtomicElement           -> an `AtomicEval` with the branches
///
/// Notes:
/// A colon range does not clear the filter, so the atom after the
/// repetition gets it again. The clips test only the net displacement. Move
/// generation tests the squares between, when the origin is known.
///
fn evaluate_atomic_expression(
    expr: AtomicGroup,
    rotation: &str,
    state: &State,
) -> AtomicElement {

    log_4!(
        concat!(
            "evaluate_atomic_expression with expression: {:?} ",
            "with rotation: {} "
        ),
        expr, rotation
    );

    let mut result: Vec<AtomicVector> = vec![AtomicVector::origin(
        *CARDINAL_STR_TO_INDEX.get(rotation).unwrap(),
    )];

    let mut modifiers: (Option<Token>, Option<Token>) = (None, None);

    let mut i = 0;
    while i < expr.len() {
        let element = &expr[i];
        match element {
            AtomicTerm(CardinalToken(direction)) => {
                modifiers.0 = Some(CardinalToken(direction.to_string()));
            }
            AtomicTerm(FilterToken(directions)) => {
                modifiers.1 = Some(FilterToken(directions.to_string()));
            }
            AtomicTerm(DotsToken(token)) => {
                result = process_atomic_dots_token(result, token, state);
            }
            AtomicTerm(RangeToken(token)) => {
                result = process_atomic_range_token(result, token, state);
            }
            AtomicTerm(AtomicToken(atomic)) => {                                /* Base case                          */
                if i + 1 < expr.len()
                    && let AtomicTerm(ColonToken(token)) = &expr[i + 1]
                {
                    result = process_atomic_colon_range_token(
                        result,
                        token,
                        Some(AtomicTerm(AtomicToken(atomic.to_string()))),
                        &modifiers,
                        state,
                    );
                    i += 2;
                    continue;
                }

                result = evaluate_atomic_term(
                    result,
                    AtomicToken(atomic.to_string()),
                    &modifiers,
                );

                modifiers = (None, None);                                       /* Reset modifiers after use          */
            }
            AtomicExpr(group) => {                                              /* Recursive case                     */
                if i + 1 < expr.len()
                    && let AtomicTerm(ColonToken(token)) = &expr[i + 1]
                {
                    result = process_atomic_colon_range_token(
                        result,
                        token,
                        Some(AtomicExpr(group.clone())),
                        &modifiers,
                        state,
                    );
                    i += 2;
                    continue;
                }

                result = evaluate_atomic_subexpression(
                    result,
                    group.clone(),
                    &modifiers,
                    state,
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

    filter_atomic_out_of_bounds(&mut result, state);


    log_4!(
        "evaluate_atomic_expression {:?} final len: {}",
        expr,
        result.len()
    );
    AtomicEval(result)
}

/// process_closing_bracket
///
/// Closes the innermost bracket of a parser stack. It pops terms from the
/// back to the opening bracket and pushes them to the front of a queue, so
/// the order stays. Then it wraps the group and pushes it back as one term.
///
/// - before : `…  (  A  B  C`, the closing bracket arrives
/// - after  : `…  (A B C)`, the group is one term
///
/// The two parsers give only the bracket test and the wrap constructor.
///
/// Params:
/// - stack      : &mut VecDeque<Term> -> parser stack with waiting terms
/// - is_bracket : IsBracket           -> test for the opening bracket
/// - wrap_result: WrapResult          -> constructor for the group
///
/// Notes:
/// Without an opening bracket, the function wraps the full stack. It does
/// not reject an unbalanced expression.
///
fn process_closing_bracket<Term, IsBracket, WrapResult>(
    stack: &mut VecDeque<Term>,
    is_bracket: IsBracket,
    wrap_result: WrapResult,
) where
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

/// atomic_to_vector
///
/// Reads one atom, with an optional cardinal filter, optional digits and
/// the final `K`. It gives the selected unit steps in the frame `rotation`.
/// The eight steps are numbered clockwise from the frame heading, so 1 is
/// that heading. Without rotation, the heading is north:
///
/// ```text
/// ┌────┬────┬────┐
/// │ 08 │ 01 │ 02 │      1  n   ( 0,  1)     5  s   ( 0, -1)
/// ├────┼────┼────┤      2  ne  ( 1,  1)     6  sw  (-1, -1)
/// │ 07 │    │ 03 │      3  e   ( 1,  0)     7  w   (-1,  0)
/// ├────┼────┼────┤      4  se  ( 1, -1)     8  nw  (-1,  1)
/// │ 06 │ 05 │ 04 │
/// └────┴────┴────┘
/// ```
///
/// An atom without digits takes all eight steps. A cardinal filter keeps
/// the steps in that direction of the frame, not of the board.
///
/// Examples: `K` takes all steps, and `nK` keeps the three north steps. In
/// `n[2468]K` with rotation north-east, the digits select the four
/// orthogonal steps of that frame. The `n` is north-east there, so it keeps
/// two of them:
///
/// ```text
/// K             nK            n[2468]K, ne
/// ┌───┬───┬───┐ ┌───┬───┬───┐ ┌───┬───┬───┐
/// │ ● │ ● │ ● │ │ ● │ ● │ ● │ │   │ ● │   │
/// ├───┼───┼───┤ ├───┼───┼───┤ ├───┼───┼───┤
/// │ ● │   │ ● │ │   │   │   │ │   │   │ ● │
/// ├───┼───┼───┤ ├───┼───┼───┤ ├───┼───┼───┤
/// │ ● │ ● │ ● │ │   │   │   │ │   │   │   │
/// └───┴───┴───┘ └───┴───┴───┘ └───┴───┴───┘
/// ```
///
/// Params:
/// - expr    : &str -> one atomic expression, e.g. `n[26]K`
/// - rotation: &str -> rotation direction of the atom
///
/// Return:
/// Vec<(i8, i8)>    -> the selected unit displacements
///
/// Notes:
/// A digit above 8 wraps, so 9 is 1. A direction named two times is kept
/// once. A 0 underflows, because there is no step 0. [`parse_move_string`]
/// normalized the input. A bad atom or rotation causes a panic.
///
fn atomic_to_vector(expr: &str, rotation: &str) -> Vec<(i8, i8)> {

    log_4!(
        "atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let mut vectors = Vec::new();

    let cap = ATOMIC
        .captures(expr)
        .unwrap_or_else(|| panic!("Invalid atomic expression: {}", expr));

    let direction = cap.get(1).map(|m| m.as_str());
    let range = cap.get(2).map(|m| m.as_str());
    let rotation_index: i8 = *CARDINAL_STR_TO_INDEX
        .get(rotation)
        .unwrap_or_else(|| panic!("Invalid rotation direction: {}", rotation));

    if let Some(range_str) = range {
        let digits = &range_str[1..range_str.len() - 1];
        for digit_char in digits.chars() {
            if let Some(digit) = digit_char.to_digit(10) {
                let index = (digit - 1 + rotation_index as u32) as usize;
                vectors.push(
                    INDEX_TO_CARDINAL_VECTORS
                        [index % INDEX_TO_CARDINAL_VECTORS.len()],
                );
            }
        }
    } else {
        for i in 0..INDEX_TO_CARDINAL_VECTORS.len() {
            let index =
                (i + rotation_index as usize) % INDEX_TO_CARDINAL_VECTORS.len();
            vectors.push(INDEX_TO_CARDINAL_VECTORS[index]);
        }
    }

    remove_duplicates_in_place(&mut vectors);

    if let Some(direction_str) = direction {
        if let Some(direction_set) = DIRECTION_VECTOR_SETS.get(direction_str) {
            let rotated_direction_set: HashSet<(i8, i8)> = direction_set
                .iter()
                .map(|(x, y)| {
                    let vec_index =
                        *CARDINAL_VECTORS_TO_INDEX.get(&(*x, *y)).unwrap();
                    let rotated_index = {
                        (vec_index + rotation_index as usize)
                            % INDEX_TO_CARDINAL_VECTORS.len()
                    };
                    INDEX_TO_CARDINAL_VECTORS[rotated_index]
                })
                .collect();
            vectors.retain(|vector| {
                rotated_direction_set.contains(vector)
            });
        }
    }

    vectors
}

/// chained_atomic_to_vector
///
/// Walks a sequence of atoms from left to right. Each atom uses the frame
/// of the last step before it. Each branch keeps its position and its last
/// step. A chain gives only its last heading. A `<...>` group gives its
/// net displacement, see [`evaluate_atomic_subexpression`].
///
/// A knight is a chain of two atoms, `[2468]Kn[2468]K`. The first atom
/// steps diagonally. In the frame of that step, the digits select the
/// orthogonal steps, and `n` keeps the two in the direction of the branch:
///
/// ```text
///  [2468]K        one branch      [2468]Kn[2468]K
/// ┌───┬───┬───┐  ┌───┬───┬───┐   ┌───┬───┬───┬───┬───┐
/// │ ● │   │ ● │  │   │ ● │   │   │   │ ● │   │ ● │   │
/// ├───┼───┼───┤  ├───┼───┼───┤   ├───┼───┼───┼───┼───┤
/// │   │ S │   │  │   │ ↗ │ ● │   │ ● │   │   │   │ ● │
/// ├───┼───┼───┤  ├───┼───┼───┤   ├───┼───┼───┼───┼───┤
/// │ ● │   │ ● │  │ S │   │   │   │   │   │ S │   │   │
/// └───┴───┴───┘  └───┴───┴───┘   ├───┼───┼───┼───┼───┤
///                                │ ● │   │   │   │ ● │
///                                ├───┼───┼───┼───┼───┤
///                                │   │ ● │   │ ● │   │
///                                └───┴───┴───┴───┴───┘
/// ```
///
/// Params:
/// - expr    : &str  -> chained atomic expression, e.g. `[2468]Kn[2468]K`
/// - rotation: &str  -> rotation of the chain
///
/// Return:
/// Vec<AtomicVector> -> one (whole, last) pair for each branch
///
/// Notes:
/// `#` is the null step. It adds nothing and keeps the frame, for a leg
/// that only turns. The byte arithmetic saturates. The token handlers read
/// dots and ranges. An expression without an atom asserts.
///
fn chained_atomic_to_vector(expr: &str, rotation: &str) -> Vec<AtomicVector> {

    log_4!(
        "chained_atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let mut result: Vec<AtomicVector> = vec![AtomicVector::origin(
        *CARDINAL_STR_TO_INDEX.get(rotation).unwrap_or_else(|| {
            panic!("Invalid rotation direction: {}", rotation)
        }),
    )];

    if expr == "#" {                                                            /* null move follows prev direction   */
        return result;
    }

    let mut stack: Vec<AtomicVector>;

    let atomics: Vec<_> = ATOMIC.find_iter(expr).collect();
    assert!(!atomics.is_empty(), "Invalid chained atomic expression: {}", expr);

    for atomic_match in atomics {
        stack = result.clone();
        result.clear();
        let atomic_str = atomic_match.as_str();

        while let Some(previous) = stack.pop() {
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

/// compound_atomic_to_vector
///
/// Compiles one full atomic expression. It stacks the tokens in order. `<`
/// opens a group, and `>` folds all terms back to the `<` into one nested
/// term. [`evaluate_atomic_expression`] then walks the stack. The first
/// matching pattern gives the token type:
///
/// - `<  >`       : group open and close
/// - `n … sw`     : cardinal filter
/// - `[1357]`     : index filter
/// - `.  ..  {i}` : repetition of the last step
/// - `:{i}`       : repetition of the last atom
/// - `K  nK  N`   : an atom, if no other pattern matches
///
/// In `nW<nWnF>nW`, the group arrives at (±1, 2). The last `nW` uses that
/// displacement, so its heading is north by [`irregular_vector_direction`],
/// although the last step of the group is diagonal:
///
/// ```text
/// ┌───┬───┬───┐
/// │ ● │   │ ● │   the last nW, read in the group's heading
/// ├───┼───┼───┤
/// │ ↖ │   │ ↗ │   where <nWnF> arrives, (±1, 2) from where it began
/// ├───┼───┼───┤
/// │   │ ↑ │   │
/// ├───┼───┼───┤
/// │   │ ↑ │   │   the first nW
/// ├───┼───┼───┤
/// │   │ S │   │
/// └───┴───┴───┘
/// ```
///
/// Params:
/// - expr    : &str   -> compound atomic expression, can contain `<...>`
/// - rotation: &str   -> rotation of the expression
/// - state   : &State -> board dimensions for the clip
///
/// Return:
/// Vec<AtomicVector>  -> one (whole, last) pair for each branch
///
/// Notes:
/// A `>` without a `<` wraps the full stack, not an error. A result that is
/// not branches causes a panic.
///
fn compound_atomic_to_vector(
    expr: &str,
    rotation: &str,
    state: &State,
) -> Vec<AtomicVector> {

    log_4!(
        "compound_atomic_to_vector expr {} with rotation {}",
        expr, rotation
    );

    let tokens: Vec<&str> =
        ATOMIC_TOKENS.find_iter(expr).map(|m| m.as_str()).collect();


    log_4!("compound_atomic_to_vector tokens: {:?}", tokens);

    let mut stack: AtomicGroup = VecDeque::new();

    for token in tokens {                                                       /* Sort of like parsing infix exprs   */
        match token {
            ">" => {
                process_closing_bracket(
                    &mut stack,
                    |term| matches!(term, AtomicTerm(BracketToken(_))),
                    AtomicExpr,
                );
            }
            "<" => {
                stack.push_back(AtomicTerm(BracketToken(token.to_string())));
            }
            "n" | "e" | "s" | "w" | "ne" | "nw" | "se" | "sw" => {
                stack.push_back(AtomicTerm(CardinalToken(token.to_string())));
            }
            token if DIRECTION_FILTER_TOKEN.is_match(token) => {
                stack.push_back(AtomicTerm(FilterToken(token.to_string())));
            }
            token if DOTS_TOKEN.is_match(token) => {
                stack.push_back(AtomicTerm(DotsToken(token.to_string())));
            }
            token if RANGE_TOKEN.is_match(token) => {
                stack.push_back(AtomicTerm(RangeToken(token.to_string())));
            }
            token if COLON_RANGE_TOKEN.is_match(token) => {
                stack.push_back(AtomicTerm(ColonToken(token.to_string())));
            }
            _ => {
                stack.push_back(AtomicTerm(AtomicToken(token.to_string())));
            }
        }
    }

    let result = evaluate_atomic_expression(stack, rotation, state);            /* Evaluate recursively               */
    match result {
        AtomicEval(result) => result,
        _ => panic!("Expected result for {} but got {:?}", expr, result),
    }
}

/*----------------------------------------------------------------------------*\
                        MULTI-LEG EXPRESSION EVALUATION
\*----------------------------------------------------------------------------*/

/// sum_multi_leg_vectors
///
/// Gives the end square of a branch, the sum of the net displacements of
/// its legs. The later filters test only the end square, not the route.
///
/// Params:
/// - vectors: &MultiLegVector -> legs of one branch, in order
///
/// Return:
/// (i8, i8)                   -> net (x, y) of the branch
///
/// Notes:
/// The sum saturates. A wrap would put a far square near the origin.
///
fn sum_multi_leg_vectors(vectors: &MultiLegVector) -> (i8, i8) {
    let mut sum: (i8, i8) = (0, 0);

    for leg in vectors {
        let atomic = leg.get_atomic();
        let whole = atomic.whole();
        sum.0 = sum.0.saturating_add(whole.0);
        sum.1 = sum.1.saturating_add(whole.1);
    }

    sum
}

/// sort_multi_leg_clockwise
///
/// Sorts the eight rotations of a multi-leg expression clockwise, so an
/// index filter selects by position. The order is the same as in
/// [`sort_atomic_clockwise`], NE first and N last.
///
/// The sort uses the end square of a branch, not its rotation frame. For
/// example, a north-east leg pair that ends east goes with east.
///
/// Params:
/// - vectors: Vec<MultiLegVector> -> the eight rotations to sort
///
/// Return:
/// Vec<MultiLegVector>            -> the same eight, clockwise from NE
///
/// Notes:
/// Fewer than eight branches cause a panic. The sort is stable, so two
/// branches on one square keep their order.
///
fn sort_multi_leg_clockwise(
    mut vectors: Vec<MultiLegVector>,
) -> Vec<MultiLegVector> {
    assert_eq!(
        vectors.len(),
        8,
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

/// filter_multi_leg_by_index
///
/// Keeps the branches that a direction filter names. The function sorts
/// the set with [`sort_multi_leg_clockwise`] first. The caller converts the
/// digits into indices (digit - 1).
///
/// Params:
/// - vectors: Vec<MultiLegVector> -> the eight rotations
/// - index  : Vec<usize>          -> clockwise indices to keep
///
/// Return:
/// Vec<MultiLegVector>            -> the selected branches, in index order
///
/// Notes:
/// A repeated index gives the branch two times. An index past 7 causes a
/// panic. The sort needs all eight, so it cannot follow a cardinal filter.
///
fn filter_multi_leg_by_index(
    mut vectors: Vec<MultiLegVector>,
    index: Vec<usize>,
) -> Vec<MultiLegVector> {
    let mut result: Vec<MultiLegVector> = Vec::new();


    log_4!("filter_atomic_by_index sorted indices: {:?}", index);

    vectors = sort_multi_leg_clockwise(vectors);


    log_5!(
        "filter_multi_leg_by_index sorted vectors: {:?}",
        vectors
    );

    for i in index {
        result.push(vectors[i].clone());
    }

    result
}

/// filter_multi_leg_by_cardinal_direction
///
/// Keeps the branches in the half-plane or quadrant that `direction` names,
/// in the frame `pov`. The test uses the end square of each branch. A
/// branch on a fold line is in no quadrant next to it.
///
/// Params:
/// - vectors  : Vec<MultiLegVector> -> branch set
/// - direction: &str                -> cardinal or diagonal to keep
/// - pov      : &str                -> rotation frame of the test
///
/// Return:
/// Vec<MultiLegVector>              -> branches that pass the test
///
/// Notes:
/// An unknown direction causes a panic.
///
fn filter_multi_leg_by_cardinal_direction(
    mut vectors: Vec<MultiLegVector>,
    direction: &str,
    pov: &str,
) -> Vec<MultiLegVector> {

    log_4!(
        "filter_by_cardinal_direction direction: {} w.r.t {}",
        direction, pov
    );

    let pov_fn = quadrant_function(pov);

    vectors.retain(|vector| {
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
            _ => panic!("Invalid rotation direction: {}", direction),
        }
    });

    vectors
}

/// filter_multi_leg_out_of_bounds
///
/// Removes branches whose end square is too far for the board from any
/// square. This also stops an open repetition. The bound is the same as in
/// [`filter_atomic_out_of_bounds`].
///
/// Params:
/// - vectors: &mut Vec<MultiLegVector> -> branch set, changed in place
/// - state  : &State                   -> board dimensions for the bound
///
/// Notes:
/// Only the end square is tested. Move generation tests the middle squares,
/// when the origin is known.
///
fn filter_multi_leg_out_of_bounds(
    vectors: &mut Vec<MultiLegVector>,
    state: &State,
) {
    vectors.retain(|vector| {
        let sum = sum_multi_leg_vectors(vector);
        sum.0.saturating_abs() <= state.statics.files as i8
            && sum.1.saturating_abs() <= state.statics.ranks as i8
    });
}

/// process_multi_leg_dots_token
///
/// Adds a copy of the last leg of each branch for each dot. The atomic
/// stage adds a repeat to the displacement. Here each repeat is its own
/// leg, so move generation can find a blocker on each middle square.
///
/// Params:
/// - vector_set: Vec<MultiLegVector> -> branches until now
/// - token     : &str                -> the dots to apply
/// - state     : &State              -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector>               -> the extended and clipped set
///
/// Notes:
/// A leading `-` marks the leg start, so it is not counted. A branch past
/// the board is removed. Branches compare leg by leg, so two routes to one
/// square are two moves.
///
fn process_multi_leg_dots_token(
    vector_set: Vec<MultiLegVector>,
    token: &str,
    state: &State,
) -> Vec<MultiLegVector> {
    let dot_count = token.replace("-", "").len() as i8;

    let mut updated_vectors: Vec<MultiLegVector> = vector_set
        .into_iter()
        .map(|mut vector| {
            let last = *vector
                .last()
                .expect("Expected at least one leg in multi leg vector.");

            for _ in 0..dot_count {
                vector.push(last);
            }
            vector
        })
        .collect();

    filter_multi_leg_out_of_bounds(&mut updated_vectors, state);
    remove_duplicates_in_place(&mut updated_vectors);
    updated_vectors
}

/// process_multi_leg_range_token
///
/// Repeats the last leg of each branch for the counts of the range. A
/// count is the total number of legs:
///
/// - `nW-{1}` : `nW`
/// - `nW-{2}` : `nW-nW`
/// - `nW-{3}` : `nW-nW-nW`
///
/// A single count gives one branch for each input. A span gives the union,
/// so a slide gives each square where it can stop.
///
/// Params:
/// - vector_set: Vec<MultiLegVector> -> branches until now
/// - token     : &str                -> the `{i..j}` range token
/// - state     : &State              -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector>               -> the new and clipped set
///
/// Notes:
/// An open end runs until a round adds no new branch. The board clip runs
/// once at the end. A token without a lower bound causes a panic.
///
fn process_multi_leg_range_token(
    vector_set: Vec<MultiLegVector>,
    token: &str,
    state: &State,
) -> Vec<MultiLegVector> {
    let (start, end) = range_bounds!(RANGE_TOKEN, "range", token);

    match (start, end) {
        (Some(start_count), Some(end_count)) => {
            let mut all_updated_vectors: Vec<MultiLegVector> = Vec::new();

            for count in start_count..=end_count {
                let updated_vectors: Vec<_> = vector_set
                    .clone()
                    .into_iter()
                    .map(|mut vector| {
                        let last = *vector.last().expect(
                            "Expected at least one leg in multi leg vector.",
                        );

                        for _ in 0..count - 1 {
                            vector.push(last);
                        }
                        vector
                    })
                    .collect();

                let prev_len = all_updated_vectors.len();
                all_updated_vectors.extend(updated_vectors);
                remove_duplicates_in_place(&mut all_updated_vectors);
                if all_updated_vectors.len() == prev_len {
                    break;                                                      /* No new vectors are added so break  */
                }
            }

            let mut result = all_updated_vectors;

            filter_multi_leg_out_of_bounds(&mut result, state);
            result
        }
        (Some(count), None) => {
            let mut updated_vectors: Vec<MultiLegVector> = vector_set
                .into_iter()
                .map(|mut vector| {
                    let last = *vector.last().expect(
                        "Expected at least one leg in multi leg vector.",
                    );

                    for _ in 0..count - 1 {
                        vector.push(last);
                    }
                    vector
                })
                .collect();

            filter_multi_leg_out_of_bounds(&mut updated_vectors, state);
            remove_duplicates_in_place(&mut updated_vectors);
            updated_vectors
        }
        _ => {
            panic!("Invalid range token: {:?}", token);
        }
    }
}

/// process_multi_leg_colon_range_token
///
/// Repeats the element before the token. It evaluates the element again
/// for each count, in the current heading of each branch, and adds the
/// result to the branch.
///
/// - `{i..j}`  : extends along the last leg, the heading does not change
/// - `:{i..j}` : runs the element again, so the route can turn
///
/// The repetitions go as a slash group, so the next repetition reads the
/// last leg, not the net displacement. A span adds the branches of each
/// count and stops at the first count that adds nothing new.
///
/// Params:
/// - vector_set: Vec<MultiLegVector>     -> branches until now
/// - token     : &str                    -> the `:{i..j}` colon range
/// - element   : Option<MultiLegElement> -> element to repeat
/// - modifiers : &[Option<Token>; 3]     -> waiting (cardinal, index, move)
/// - rotation  : &str                    -> fallback rotation frame
/// - state     : &State                  -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector>                   -> the repeated and clipped set
///
/// Notes:
/// With nothing to repeat before it, [`colon_range_head!`] panics. Without
/// branches, the repetitions use `rotation`.
///
fn process_multi_leg_colon_range_token(
    vector_set: Vec<MultiLegVector>,
    token: &str,
    element: Option<MultiLegElement>,
    modifiers: &[Option<Token>; 3],
    rotation: &str,
    state: &State,
) -> Vec<MultiLegVector> {
    let (element, start, end) =
        colon_range_head!(token, element, MultiLegEval);
    let mut result: Vec<MultiLegVector> = Vec::new();

    match (start, end) {
        (Some(start_count), Some(end_count)) => {
            let mut prev_len = 0;

            for count in start_count..=end_count {
                let multiplied_expr =
                    VecDeque::from(vec![element.clone(); count as usize]);

                if vector_set.is_empty() {
                    let eval = evaluate_multi_leg_subexpression(
                        vec![],
                        MultiLegSlashExpr(multiplied_expr.clone()),
                        modifiers,
                        rotation,
                        state,
                    );

                    result.extend(eval);
                } else {
                    for branch_leg_vector in &vector_set {
                        let branch_vector = branch_leg_vector.last().expect(
                            "Expected at least one vector in branch leg vector."
                        ).get_atomic();
                        let branch_rotation =
                            irregular_vector_direction(&branch_vector.last());

                        let mut extension: Vec<MultiLegVector> = Vec::new();
                        let eval = evaluate_multi_leg_subexpression(
                            vec![],
                            MultiLegSlashExpr(multiplied_expr.clone()),
                            modifiers,
                            branch_rotation,
                            state,
                        );

                        for vector in eval {
                            let mut new_branch = branch_leg_vector.clone();
                            new_branch.extend(&vector);
                            extension.push(new_branch);
                        }

                        result.extend(extension);
                    }
                }

                remove_duplicates_in_place(&mut result);
                if prev_len == result.len() {
                    break;                                                      /* No new vectors are added so break  */
                }
                prev_len = result.len();
            }

            filter_multi_leg_out_of_bounds(&mut result, state);
            result
        }
        (Some(count), None) => {
            let multiplied_expr = VecDeque::from(vec![element; count as usize]);

            if vector_set.is_empty() {
                let eval = evaluate_multi_leg_subexpression(
                    vec![],
                    MultiLegSlashExpr(multiplied_expr.clone()),
                    modifiers,
                    rotation,
                    state,
                );

                result.extend(eval);
            } else {
                for branch_leg_vector in &vector_set {
                    let branch_vector = branch_leg_vector.last().expect(
                        "Expected at least one vector in branch leg vector."
                    ).get_atomic();
                    let branch_rotation =
                        irregular_vector_direction(&branch_vector.last());

                    let mut extension: Vec<MultiLegVector> = Vec::new();
                    let eval = evaluate_multi_leg_subexpression(
                        vec![],
                        MultiLegSlashExpr(multiplied_expr.clone()),
                        modifiers,
                        branch_rotation,
                        state,
                    );

                    for vector in eval {
                        let mut new_branch = branch_leg_vector.clone();
                        new_branch.extend(&vector);
                        extension.push(new_branch);
                    }

                    result.extend(extension);
                }
            }

            remove_duplicates_in_place(&mut result);
            filter_multi_leg_out_of_bounds(&mut result, state);
            result
        }
        _ => {
            panic!("Invalid colon-range token: {:?}", token);
        }
    }
}

/// process_multi_leg_modifiers
///
/// Applies the three modifiers before the current element:
///
/// - cardinal      : keeps the half-plane or quadrant that it names
/// - index         : keeps the clockwise positions of its digits
/// - move modifier : is written onto the last leg of each branch
///
/// The move modifier goes on the last leg only, because capture or quiet
/// is about the end square.
///
/// Params:
/// - vector_set: Vec<MultiLegVector> -> set to filter
/// - modifiers : &[Option<Token>; 3] -> waiting (cardinal, index, move)
/// - rotation  : &str                -> current rotation frame
///
/// Return:
/// Vec<MultiLegVector>               -> the filtered set
///
/// Notes:
/// The cardinal filter runs first. The index filter needs all eight and
/// asserts it. Each digit minus one is an index. A branch without legs
/// causes a panic.
///
fn process_multi_leg_modifiers(
    mut vector_set: Vec<MultiLegVector>,
    modifiers: &[Option<Token>; 3],
    rotation: &str,
) -> Vec<MultiLegVector> {

    log_4!(
        "process_multi_leg_modifiers with modifiers: {:?} ",
        modifiers
    );

    if let Some(CardinalToken(directions)) = &modifiers[0] {
        vector_set = filter_multi_leg_by_cardinal_direction(
            vector_set, directions, rotation,
        );
    }

    if let Some(FilterToken(indices)) = &modifiers[1] {
        let index_vec = indices
            .chars()
            .filter_map(|ch| ch.to_digit(10).map(|d| d as usize - 1))
            .collect();
        vector_set = filter_multi_leg_by_index(vector_set, index_vec);
    }

    if let Some(MoveModifierToken(modifier)) = &modifiers[2] {
        for multi_leg_vector in &mut vector_set {
            let final_leg = multi_leg_vector
                .last_mut()
                .expect("Expected at least one leg in multi leg vector.");
            final_leg.add_modifier(modifier);
        }
    }

    vector_set
}

/// evaluate_multi_leg_term_leg
///
/// Adds one leg to each branch. The leg and its filters use the heading of
/// the branch. The last step of the new leg is its full displacement, so
/// the next leg takes its heading from the leg end, not from its small last
/// step. A slash group keeps the last step, see
/// [`evaluate_multi_leg_subexpression`].
///
/// Params:
/// - result   : Vec<MultiLegVector> -> branches until now
/// - term     : Token               -> the leg token to expand
/// - modifiers: [Option<Token>; 3]  -> waiting (cardinal, index, move)
/// - rotation : &str                -> direction of the expansion
/// - state    : &State              -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector>              -> the extended set
///
/// Notes:
/// Another token type causes a panic. Without branches, the leg uses
/// `rotation`. Branches with equal legs become one.
///
fn evaluate_multi_leg_term_leg(
    result: Vec<MultiLegVector>,
    term: Token,
    modifiers: [Option<Token>; 3],
    rotation: &str,
    state: &State,
) -> Vec<MultiLegVector> {

    log_5!(
        concat!(
            "evaluate_multi_leg_term_leg with term: {:?} ",
            "modifiers: {:?}"
        ),
        term, modifiers
    );

    let atomic = match term {
        LegToken(atomic) => atomic,
        _ => {
            panic!(
                "Unexpected atomic term in evaluate_atomic_term: {:?}",
                term
            );
        }
    };

    if result.is_empty() {
        let eval = leg_to_vector(&atomic.to_string(), rotation, state);

        let eval = process_multi_leg_modifiers(eval, &modifiers, rotation);

        for multi_leg_vector in &eval {                                         /* By default treat as one leap       */
            let sum = sum_multi_leg_vectors(multi_leg_vector);

            multi_leg_vector
                .last()
                .expect("Expected at least one vector in multi leg vector.")
                .get_atomic()
                .set_last(sum);
        }

        return eval;
    }

    let mut new_result: Vec<MultiLegVector> = Vec::new();
    for branch_leg_vector in &result {
        let branch_vector = branch_leg_vector.last().expect(
            "Expected at least one vector in branch leg vector."
        ).get_atomic();
        let branch_rotation =
            irregular_vector_direction(&branch_vector.last());

        let mut extension: Vec<MultiLegVector> = Vec::new();
        let mut eval = leg_to_vector(&atomic, branch_rotation, state);

        eval = process_multi_leg_modifiers(eval, &modifiers, branch_rotation);

        for multi_leg_vector in &eval {                                         /* By default treat as one leap       */
            let sum = sum_multi_leg_vectors(multi_leg_vector);

            multi_leg_vector
                .last()
                .expect("Expected at least one vector in multi leg vector.")
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

    remove_duplicates_in_place(&mut new_result);
    new_result
}

/// evaluate_multi_leg_subexpression
///
/// Evaluates a bracket group for each branch, in the heading of that
/// branch, and adds the result to it. The two bracket forms differ only in
/// the heading for the next element:
///
/// - `<  >`   : the net direction of the group
/// - `</  />` : the direction of its last leg
///
/// `<nWnF>` arrives at (±1, 2), which is north, but its last leg is
/// diagonal. The plain form gives north, the slash form gives the diagonal.
/// A crooked slider needs the slash form, so each repetition turns again.
///
/// Params:
/// - result   : Vec<MultiLegVector> -> branches until now
/// - expr     : MultiLegElement     -> the bracket element
/// - modifiers: &[Option<Token>; 3] -> waiting (cardinal, index, move)
/// - rotation : &str                -> direction of the expansion
/// - state    : &State              -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector>              -> the extended set
///
/// Notes:
/// Another element type, or an inner group that is not evaluated, causes a
/// panic. Without branches, the group uses `rotation` and becomes the set.
///
fn evaluate_multi_leg_subexpression(
    result: Vec<MultiLegVector>,
    expr: MultiLegElement,
    modifiers: &[Option<Token>; 3],
    rotation: &str,
    state: &State,
) -> Vec<MultiLegVector> {

    log_5!(
        concat!(
            "evaluate_multi_leg_subexpr with expr: {:?} ",
            "modifiers: {:?}, rotation: {}"
        ),
        expr, modifiers, rotation
    );

    let is_slash_expr = matches!(expr, MultiLegSlashExpr(_));
    let inner_expr = match expr {
        MultiLegExpr(inner) => inner,
        MultiLegSlashExpr(inner) => inner,
        _ => {
            panic!("Unexpected expr in evaluate_multi_leg_subexpr: {:?}", expr);
        }
    };

    if result.is_empty() {
        let eval =
            evaluate_multi_leg_expression(inner_expr, rotation, state);
        match eval {
            MultiLegEval(vectors) => {
                let eval =
                    process_multi_leg_modifiers(vectors, modifiers, rotation);

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
            }
            _ => panic!(
                "Expected MultiLegEval but got different result: {:?}",
                eval
            ),
        };
    }

    let mut new_result: Vec<MultiLegVector> = Vec::new();
    for branch_leg_vector in &result {
        let branch_vector = branch_leg_vector.last().expect(
            "Expected at least one vector in branch leg vector."
        ).get_atomic();
        let branch_rotation =
            irregular_vector_direction(&branch_vector.last());

        let mut extension: Vec<MultiLegVector> = Vec::new();
        let eval_result = evaluate_multi_leg_expression(
            inner_expr.clone(),
            branch_rotation,
            state,
        );

        let mut eval = match eval_result {
            MultiLegEval(vectors) => vectors,
            _ => panic!(
                "Expected MultiLegEval but got different result: {:?}",
                eval_result
            ),
        };

        eval = process_multi_leg_modifiers(eval, modifiers, branch_rotation);

        if !is_slash_expr {
            for multi_leg_vector in &eval {
                let sum = sum_multi_leg_vectors(multi_leg_vector);

                multi_leg_vector
                    .last()
                    .expect("Expected at least one vector in multi leg vector.")
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

    remove_duplicates_in_place(&mut new_result);
    new_result
}

/// evaluate_multi_leg_expression
///
/// Walks a token group from left to right with one branch set. The set
/// starts empty, because a route has no length before its first leg. A
/// cardinal, index or move modifier waits and goes to the next leg or
/// group. A colon range after that element gets it first.
///
/// A group before a colon range always uses the slash form, so the
/// repetitions follow its last leg. An `@` exclusion is evaluated alone and
/// subtracted at the end. Only the last exclusion counts.
///
/// Params:
/// - expr    : MultiLegGroup -> the token group to evaluate
/// - rotation: &str          -> rotation of the expression
/// - state   : &State        -> board dimensions for the clip
///
/// Return:
/// MultiLegElement           -> a `MultiLegEval` with the branches
///
/// Notes:
/// A colon range does not clear the filter, so the element after the
/// repetition gets it again. The board clip runs once at the end on the end
/// squares. Move generation tests the middle squares.
///
fn evaluate_multi_leg_expression(
    expr: MultiLegGroup,
    rotation: &str,
    state: &State,
) -> MultiLegElement {

    log_4!(
        concat!(
            "evaluate_multi_leg_expression with expression: {:?} ",
            "with rotation: {} "
        ),
        expr, rotation
    );

    let mut result: Vec<MultiLegVector> = Vec::new();
    let mut exclusion: Vec<MultiLegVector> = Vec::new();
    let mut modifiers: [Option<Token>; 3] = [None, None, None];

    let mut i = 0;
    while i < expr.len() {
        let element = &expr[i];
        match element {
            MultiLegTerm(CardinalToken(direction)) => {
                modifiers[0] = Some(CardinalToken(direction.to_string()));
            }
            MultiLegTerm(FilterToken(directions)) => {
                modifiers[1] = Some(FilterToken(directions.to_string()));
            }
            MultiLegTerm(MoveModifierToken(modifier)) => {
                modifiers[2] = Some(MoveModifierToken(modifier.to_string()));
            }
            MultiLegTerm(LegToken(atomic)) => {
                if i + 1 < expr.len()
                    && let MultiLegTerm(ColonToken(token)) = &expr[i + 1]
                {
                    result = process_multi_leg_colon_range_token(
                        result,
                        token,
                        Some(MultiLegTerm(LegToken(atomic.to_string()))),
                        &modifiers,
                        rotation,
                        state,
                    );
                    i += 2;
                    continue;
                }

                result = evaluate_multi_leg_term_leg(
                    result,
                    LegToken(atomic.to_string()),
                    modifiers,
                    rotation,
                    state,
                );

                modifiers = [None, None, None];
            }
            MultiLegTerm(DotsToken(token)) => {
                result =
                    process_multi_leg_dots_token(result, token, state);
            }
            MultiLegTerm(RangeToken(token)) => {
                result =
                    process_multi_leg_range_token(result, token, state);
            }
            MultiLegTerm(ExclusionToken(token)) => {
                exclusion =
                    multi_leg_to_vector(&token[1..], rotation, state);
            }
            MultiLegExpr(group) => {
                if i + 1 < expr.len()
                    && let MultiLegTerm(ColonToken(token)) = &expr[i + 1]
                {
                    result = process_multi_leg_colon_range_token(
                        result,
                        token,
                        Some(MultiLegSlashExpr(group.clone())),
                        &modifiers,
                        rotation,
                        state,
                    );
                    i += 2;
                    continue;
                }

                result = evaluate_multi_leg_subexpression(
                    result,
                    element.clone(),
                    &modifiers,
                    rotation,
                    state,
                );

                modifiers = [None, None, None];
            }
            MultiLegSlashExpr(group) => {
                if i + 1 < expr.len()
                    && let MultiLegTerm(ColonToken(token)) = &expr[i + 1]
                {
                    result = process_multi_leg_colon_range_token(
                        result,
                        token,
                        Some(MultiLegSlashExpr(group.clone())),
                        &modifiers,
                        rotation,
                        state,
                    );
                    i += 2;
                    continue;
                }

                result = evaluate_multi_leg_subexpression(
                    result,
                    element.clone(),
                    &modifiers,
                    rotation,
                    state,
                );

                modifiers = [None, None, None];
            }
            _ => {
                panic!(
                    "Unexpected element in multi-leg expression: {:?}",
                    element
                );
            }
        }
        i += 1;
    }

    filter_multi_leg_out_of_bounds(&mut result, state);

    result.retain(|vector| !exclusion.contains(vector));


    log_5!(
        "evaluate_multi_leg_expression {:?} final len: {:?} ",
        expr,
        result.len()
    );

    MultiLegEval(result)
}

/// tokenize_multi_leg_expression
///
/// Splits one branch into the tokens of the stack parser, then joins the
/// parts that are one token. A bracket group without a leg boundary is one
/// compound atomic, so the parser needs it as one word:
///
/// ```text
/// m<[1357]K>-c<[2468]K>.
/// m  <  [1357]K  >  -  c  <  [2468]K  >  .    cut on the alphabet
/// m  <[1357]K>      -  c  <[2468]K>      .    groups closed up
/// m  <[1357]K>      -  c  <[2468]K>.          suffix rejoined
/// ```
///
/// A group stays open if it has a leg boundary, a slash bracket or move
/// modifiers, because the parser must see these. The joining repeats until
/// nothing changes. A suffix directly after a group repeats a step in the
/// leg. A suffix after `-` is its own token and repeats the full leg.
///
/// Params:
/// - expr: &str -> one clean multi-leg branch
///
/// Return:
/// Vec<String>  -> tokens for the multi-leg stack parser
///
/// Notes:
/// An expression without any match causes a panic. Text between two matches
/// is its own token, so the parser rejects a stray character.
///
fn tokenize_multi_leg_expression(expr: &str) -> Vec<String> {
    let token_matches: Vec<_> = LEG_TOKENS.find_iter(expr).collect();
    assert!(
        !token_matches.is_empty(),
        "Invalid compound leg expression: {}",
        expr
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


    log_5!("tokenize_multi_leg_expression raw tokens: {:?}", tokens);

    let mut prev_tokens: Vec<String> = vec![];

    while prev_tokens != tokens {
        prev_tokens = tokens.clone();
        tokens = tokens.into_iter().rev().collect();
        let mut result = vec![];
        while let Some(token) = tokens.pop() {
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

                    if last.starts_with("-")
                        || last == "</"
                        || last == "/>"
                        || MODIFIERS.is_match(&last)
                    {
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


    log_5!("tokenize_multi_leg_expression first pass {:?}", tokens);

    let mut result: Vec<String> = vec![];
    tokens = tokens.into_iter().rev().collect();

    while !tokens.is_empty() {
        if result.is_empty() {
            result.push(tokens.pop().unwrap());
            continue;
        }

        let token2 = tokens.pop().unwrap();
        let token1 = result.pop().unwrap();

        if !token1.starts_with("-")
            && token1 != "<"
            && token1 != ">"
            && !token2.starts_with("-")
            && token2 != "<"
            && token2 != ">"
            && token1 != "</"
            && token2 != "</"
            && token1 != "/>"
            && token2 != "/>"
            && !MODIFIERS.is_match(&token1)
            && !MODIFIERS.is_match(&token2)
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

/// leg_to_vector
///
/// Reads one leg into branches. A leg has three parts, and only the middle
/// part is mandatory:
///
/// ```text
/// mc     [26]K            @nK
/// what   where it goes    where it may not end
/// ```
///
/// The exclusion compares only end squares. `mc[26]K@nK` is that step on
/// all squares except the squares that `nK` reaches. The two use the same
/// rotation.
///
/// The result has one branch with one leg for each displacement. Each leg
/// has the modifier letters as text. Move generation reads them.
///
/// Params:
/// - expr    : &str    -> one leg expression, e.g. `mc[26]K@nK`
/// - rotation: &str    -> rotation of the leg
/// - state   : &State  -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector> -> one-leg branches, one for each displacement
///
/// Notes:
/// A bad leg, or a leg without a compound atomic, causes a panic with the
/// leg text.
///
fn leg_to_vector(
    expr: &str,
    rotation: &str,
    state: &State,
) -> Vec<MultiLegVector> {

    log_5!("leg_to_vector leg {} with rotation {}", expr, rotation);

    let captures = LEG
        .captures(expr)
        .unwrap_or_else(|| panic!("Invalid leg expression: {}", expr));


    log_5!("leg_to_vector captures: {:?}", captures);

    let modifiers = captures.get(1).map_or("", |m| m.as_str());
    let exclusion = captures.get(3).map_or("", |m| m.as_str());
    let exclusion_vectors = if !exclusion.is_empty() {
        compound_atomic_to_vector(exclusion, rotation, state)
            .into_iter()
            .map(|v| v.whole())
            .collect::<HashSet<_>>()
    } else {
        HashSet::new()
    };
    let compound_atomic =
        captures.get(2).expect("Missing compound atomic in leg.").as_str();
    let mut vectors =
        compound_atomic_to_vector(compound_atomic, rotation, state);

    vectors.retain(|vector| {
        let tuple = vector.as_tuple();
        !exclusion_vectors.contains(&tuple[0])
    });

    vectors
        .into_iter()
        .map(|v| vec![LegVector::new(v, modifiers)])
        .collect::<Vec<Vec<LegVector>>>()
}

/// multi_leg_to_vector
///
/// Compiles one full branch of a move expression. It stacks the tokens in
/// order. `<` and `</` open a group, and `>` and `/>` fold all terms back
/// to the opening into one nested term. Then it evaluates the stack:
///
/// - `<  >  </  />` : group open and close
/// - `n … sw`       : cardinal filter
/// - `[1357]`       : index filter
/// - `-.  -..`      : repetition of the last leg
/// - `-{i}`         : the same, with a count
/// - `-:{i}`        : the last leg or group, evaluated again each time
/// - `@expr`        : end squares to exclude
/// - `mcd … !`      : what the leg can do
/// - `-`            : a leg boundary
///
/// Each leg has its own stop square. In `eK-{4}-nK`, the piece goes four
/// squares east and then one north. Move generation tests the modifiers of
/// each leg at its stop square:
///
/// ```text
/// ┌───┬───┬───┬───┬───┐
/// │   │   │   │   │ ● │
/// ├───┼───┼───┼───┼───┤
/// │ S │ → │ → │ → │ ↑ │
/// └───┴───┴───┴───┴───┘
/// ```
///
/// A branch without `-` is one leg and goes directly to [`leg_to_vector`].
///
/// Params:
/// - expr    : &str    -> one clean multi-leg branch
/// - rotation: &str    -> rotation of the expression
/// - state   : &State  -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector> -> all branches of the expression
///
/// Notes:
/// A close without an open wraps the full stack. The `-` tokens are removed
/// after the split. An unknown token is a leg, and [`leg_to_vector`] panics
/// on a bad leg.
///
fn multi_leg_to_vector(
    expr: &str,
    rotation: &str,
    state: &State,
) -> Vec<MultiLegVector> {

    log_5!(
        "multi_leg_to_vector leg {} with rotation {}",
        expr, rotation
    );

    if !expr.contains("-") {
        return leg_to_vector(expr, rotation, state);
    }

    let tokens = tokenize_multi_leg_expression(expr);


    log_5!("multi_leg_to_vector final tokens: {:?}", tokens);

    let mut stack: MultiLegGroup = VecDeque::new();
    for token in tokens {
        match token.as_str() {
            "-" => {}                                                           /* Semantically irrelevant            */
            ">" => {
                process_closing_bracket(
                    &mut stack,
                    |term| matches!(term, MultiLegTerm(BracketToken(_))),
                    MultiLegExpr,
                );
            }
            "/>" => {
                process_closing_bracket(
                    &mut stack,
                    |term| matches!(term, MultiLegTerm(SlashBracketToken(_))),
                    MultiLegSlashExpr,
                );
            }
            "<" => {
                stack.push_back(MultiLegTerm(BracketToken(token.to_string())));
            }
            "</" => {
                stack.push_back(MultiLegTerm(SlashBracketToken(
                    token.to_string(),
                )));
            }
            token if MODIFIERS.is_match(token) => {
                stack.push_back(MultiLegTerm(MoveModifierToken(
                    token.to_string(),
                )));
            }
            "n" | "e" | "s" | "w" | "ne" | "nw" | "se" | "sw" => {
                stack.push_back(MultiLegTerm(CardinalToken(token.to_string())));
            }
            token if DIRECTION_FILTER_TOKEN.is_match(token) => {
                stack.push_back(MultiLegTerm(FilterToken(token.to_string())));
            }
            token if DOTS_TOKEN.is_match(token) => {
                stack.push_back(MultiLegTerm(DotsToken(token.to_string())));
            }
            token if RANGE_TOKEN.is_match(token) => {
                stack.push_back(MultiLegTerm(RangeToken(token.to_string())));
            }
            token if COLON_RANGE_TOKEN.is_match(token) => {
                stack.push_back(MultiLegTerm(ColonToken(token.to_string())));
            }
            token if token.starts_with("@") => {
                stack
                    .push_back(MultiLegTerm(ExclusionToken(token.to_string())));
            }
            _ => {
                stack.push_back(MultiLegTerm(LegToken(token.to_string())));
            }
        }
    }


    log_5!("multi_leg_to_vector parsed stack: {:?}", stack);

    let result = evaluate_multi_leg_expression(stack, rotation, state);         /* Evaluate recursively               */

    match result {
        MultiLegEval(result) => result,
        _ => panic!(
            "Expected evaluated result for {} but got {:?}",
            expr, result
        ),
    }
}

/// generate_move_vectors
///
/// The entry point of this file. It compiles one config move expression
/// into all its routes, once at load time. [`parse_move_string`] makes plain
/// `|` branches, and [`multi_leg_to_vector`] compiles each branch:
///
/// ```text
/// text  →  normalized  →  branch  →  legs  →  displacements
/// ```
///
/// Each branch uses the north frame. Move generation mirrors the offsets
/// for each colour, so one compiled set works for the two players.
///
/// Params:
/// - expr : &str       -> raw move expression from the config
/// - state: &State     -> board dimensions for the clip
///
/// Return:
/// Vec<MultiLegVector> -> the branches of the expression, no duplicates
///
/// Notes:
/// Two branches with equal legs become one. Two branches with different
/// legs to one square stay, because they are different moves.
///
#[hotpath::measure]
pub fn generate_move_vectors(
    expr: &str,
    state: &State,
) -> Vec<MultiLegVector> {
    let parsed_expr = parse_move_string(expr);


    log_4!(
        "generate_move_vectors parsed expression for {}: {:?}",
        expr, parsed_expr
    );

    let mut result: Vec<MultiLegVector> =
        split_and_process(&parsed_expr, |m| {
            Some(multi_leg_to_vector(m, "n", state))
        })
        .into_iter()
        .flatten()
        .flatten()
        .collect();

    remove_duplicates_in_place(&mut result);
    result
}

/*----------------------------------------------------------------------------*\
                                MOVE CONDITIONS
\*----------------------------------------------------------------------------*/

/// split_top_level_branches
///
/// Splits a move expression at each `|` outside of all brackets. A
/// bracketed `|` stays in its branch, because it is part of a group:
///
/// ```text
/// mnW|(nR|nB)@@sW~Q@   ->   mnW   (nR|nB)@@sW~Q@
/// ```
///
/// Params:
/// - expr: &str -> raw move expression from the config
///
/// Return:
/// Vec<&str>    -> the top-level branches, in order
///
fn split_top_level_branches(expr: &str) -> Vec<&str> {
    let mut branches = Vec::new();
    let mut depth = 0;
    let mut branch_start = 0;

    for (index, character) in expr.char_indices() {
        match character {
            '(' | '<' => depth += 1,
            ')' | '>' => depth -= 1,
            '|' if depth == 0 => {
                branches.push(expr[branch_start..index].trim());
                branch_start = index + 1;
            }
            _ => {}
        }
    }

    branches.push(expr[branch_start..].trim());
    branches
}

/// split_move_condition
///
/// Splits one top-level branch into its move notation and its CPMN
/// condition. The exclusion slot of a leg is positional. Thus the `@` after
/// the exclusion opens the condition:
///
/// ```text
/// nR          no exclusion, no condition
/// nR@nW       exclusion nW, no condition
/// nR@@P       empty exclusion, condition P
/// nR@nW@P     exclusion nW, condition P
/// ```
///
/// The condition starts after the first `@` that follows another `@` with
/// no `-` between them. An exclusion has no `-`, so an exclusion of an
/// earlier leg does not open a condition.
///
/// Params:
/// - branch: &str       -> one top-level branch
///
/// Return:
/// (&str, Option<&str>) -> move notation and condition, if there is one
///
fn split_move_condition(branch: &str) -> (&str, Option<&str>) {
    let mut in_exclusion = false;

    for (index, character) in branch.char_indices() {
        match character {
            '-' => in_exclusion = false,
            '@' if in_exclusion => {
                let move_expr = &branch[..index];
                let move_expr =
                    move_expr.strip_suffix('@').unwrap_or(move_expr);

                return (move_expr, Some(&branch[index + 1..]));
            }
            '@' => in_exclusion = true,
            _ => {}
        }
    }

    (branch, None)
}

/// strip_move_conditions
///
/// Removes the CPMN condition of each top-level branch. A condition has
/// piece letters, and a test that reads modifier letters must not see
/// them. For example, a black pawn `p` is not the en passant modifier.
///
/// Params:
/// - expr: &str -> raw move expression from the config
///
/// Return:
/// String       -> the same branches, move notation only
///
pub fn strip_move_conditions(expr: &str) -> String {
    split_top_level_branches(expr)
        .into_iter()
        .map(|branch| split_move_condition(branch).0)
        .collect::<Vec<&str>>()
        .join("|")
}

/// generate_move_set
///
/// Compiles the full move expression of a piece into packed move vectors
/// with their CPMN conditions. Each top-level branch goes through
/// [`generate_move_vectors`], and its condition through [`parse_pattern`].
///
/// Two branches can give equal legs. They become one vector, and their
/// conditions join as alternatives:
///
/// - two conditions        : the vector keeps the two patterns
/// - one without condition : the vector has no condition
///
/// Thus move generation never makes one move two times.
///
/// Params:
/// - expr : &str   -> raw move expression from the config
/// - state: &State -> piece dictionary and board dimensions
///
/// Return:
/// MoveSet         -> the vectors of the expression, first copy order
///
pub fn generate_move_set(expr: &str, state: &State) -> MoveSet {
    let mut move_options: Vec<(Arc<[Leg]>, Option<PatternSet>)> = Vec::new();
    let mut option_slots: HashMap<Arc<[Leg]>, usize> = HashMap::new();

    for branch in split_top_level_branches(expr) {
        let (move_expr, condition_expr) = split_move_condition(branch);
        let condition =
            condition_expr.map(|pattern| parse_pattern(pattern, state));

        for multi_leg_vector in generate_move_vectors(move_expr, state) {
            let legs = multi_leg_vector
                .iter()
                .map(|leg_vector| leg!(leg_vector))
                .collect::<Arc<[Leg]>>();

            let Some(&slot) = option_slots.get(&legs) else {
                option_slots.insert(legs.clone(), move_options.len());
                move_options.push(
                    (legs, condition.clone().map(|pattern| vec![pattern]))
                );
                continue;
            };

            match (&mut move_options[slot].1, &condition) {
                (Some(patterns), Some(pattern)) => {
                    patterns.push(pattern.clone());
                }
                (patterns, _) => *patterns = None,
            }
        }
    }

    move_options
        .into_iter()
        .map(|(legs, patterns)| MoveVector {
            legs,
            pattern: patterns.map(Arc::new),
        })
        .collect()
}
