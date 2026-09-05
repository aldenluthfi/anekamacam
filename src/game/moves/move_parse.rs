//! move_parse.rs
//!
//! Compiles a piece's movement notation into concrete move vectors.
//!
//! A variant describes how each piece moves in a compact Betza-style string,
//! but move generation needs explicit per-square leg vectors. This file is
//! the bridge: it tokenises and expands that notation — directions, ranges,
//! cardinals, chained and compound legs — into the atomic and multi-leg
//! vectors the generator walks, done once at load time so the hot path never
//! re-parses text.
//!
//! Every expression passes through one fixed text pipeline before vector
//! conversion, each stage rewriting the `|`-separated options in place:
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
//! Created: 18/02/2024
//! Author : Alden Luthfi

use crate::*;

lazy_static! {
    /// Movement-notation lexer tables.
    ///
    /// The shared regexes and cardinal lookup tables every stage of the parse
    /// pipeline reads. All are compiled or built once at first use:
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
    /// - CARDINAL_VECTORS_TO_INDEX: unit (x, y) vector to index 0-7
    /// - DIRECTION_VECTOR_SETS    : direction letter to unit vector set
    /// - CARDINAL_STR_TO_INDEX    : cardinal name ("n".."nw") to index
    /// - CARDINAL_INDEX_TO_STR    : index 0-7 back to cardinal name
    ///
    /// The index all three cardinal maps agree on is the one the prelude's
    /// cardinal vector table counts from: north is 0 and the rest run
    /// clockwise, so a rotation is an addition and a reflection a negation.
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
/// Joins two expressions with one operator. `|` sets them side by side as
/// alternatives; `^` concatenates them, and since either side may already
/// carry alternatives of its own, concatenation multiplies out, every branch
/// on the left against every branch on the right.
///
/// `#` is the empty expression, and concatenating with it gives back the
/// other side unchanged. That is how a stage with nothing to contribute stays
/// out of the result rather than leaving a hole in it.
///
/// Params:
/// - op: char -> operator to apply, `^` (concat) or `|` (alternation)
/// - a : &str -> left operand, possibly already `|`-branched
/// - b : &str -> right operand, possibly already `|`-branched
///
/// Return:
/// String     -> the combined expression with branches distributed
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
/// Ranks the two expression operators so the stack evaluator knows when
/// to reduce: concatenation (`^`) binds tighter than alternation (`|`).
///
/// Params:
/// - op: char -> operator character, `^` or `|`
///
/// Return:
/// usize      -> binding strength, higher binds tighter
fn precedence(op: char) -> usize {
    match op {
        '^' => 2,
        '|' => 1,
        _ => unreachable!("Invalid operator: {}", op),
    }
}

/// betza_atoms
///
/// Rewrites a Betza atom as the Cheesy King Notation that means the same
/// thing, so every later stage sees king steps and nothing else. Betza names
/// a piece by the square it lands on; CKN names it by the route taken to get
/// there, and the digits in a route pick headings off the cardinal circle,
/// counting clockwise from wherever the route already points. An atom
/// standing first points north, so there 1 is north, the odd digits are
/// orthogonal and the even ones diagonal; behind a diagonal step the whole
/// ring turns with it, which is what makes `N` below a knight rather than a
/// doubled ferz:
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
/// Anything else is handed back untouched, which is what lets a variant write
/// its own atoms in CKN directly and pass them through here unharmed.
///
/// Params:
/// - piece: char -> Betza atom symbol, e.g. 'N', 'R', 'Q'
///
/// Return:
/// String        -> the CKN expansion, or the symbol itself if unknown
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
/// Flattens a normalized expression into plain `|`-separated branches. Two
/// stacks do the work, one holding operands and one holding operators, and an
/// operator is applied as soon as something of at least its own precedence
/// arrives, which is what leaves the parentheses with nothing left to say by
/// the time the walk reaches the end.
///
/// Params:
/// - expr: &str -> normalized expression with explicit `^` operators
///
/// Return:
/// String       -> flat `|`-separated form with parentheses eliminated
///
/// Notes:
/// An expression that runs out of operands, or ends with none at all, panics
/// naming the expression. Movement notation is config text compiled once at
/// load time, so a malformed one is a broken variant and not something a
/// position can produce.
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
/// Puts a raw expression into the shape the evaluator expects. Writing one
/// thing after another is how the notation says "and then", so every implied
/// concatenation around a parenthesis is spelled out as `^` and the result is
/// evaluated down to plain `|`-separated branches.
///
/// An expression with no parenthesis to read that way is already canonical
/// and comes back as it arrived.
///
/// Params:
/// - expr: &str   -> raw move expression from the config
///
/// Return:
/// Option<String> -> canonical `|`-separated expression
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
/// Walks a branch one character at a time through [`betza_atoms`], so every
/// Betza letter becomes the CKN route that means the same thing while
/// everything around it comes through as it was written.
///
/// The map reads a character without regard for what surrounds it, which is
/// what lets a branch already written in CKN pass unharmed: a route spells
/// itself with `K`, lower-case headings and punctuation, and none of those is
/// the name of an atom.
///
/// Params:
/// - expr: &str   -> single sanitized branch (no `|` alternation)
///
/// Return:
/// Option<String> -> the branch with all atoms expanded
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
/// Writes out every direction filter in a branch as the explicit set of
/// directions it stands for. The digits count the cardinal circle clockwise
/// from north, `..` spans a run of them and `$` takes some of the run back
/// out:
///
/// - `[1..8]`      : full range `[12345678]`
/// - `[..5]`       : open low end `[12345]`
/// - `[5..]`       : open high end `[5678]`
/// - `[1..7$25]`   : range minus exclusions `[13467]`
/// - `[1235678$2]` : explicit set minus exclusions `[135678]`
///
/// An open end is filled in from the circle and not from the atom it
/// qualifies: `[..]` always means all eight, and a direction the atom cannot
/// take is discarded later, where the filter meets that atom's own vectors.
///
/// Params:
/// - expr: &str   -> single sanitized branch (no `|` alternation)
///
/// Return:
/// Option<String> -> branch with every direction filter fully listed
///
/// Notes:
/// One filter is rewritten per turn and the loop re-reads what it wrote. It
/// still ends, a bare list of digits carrying neither `..` nor `$` and so
/// matching nothing the pattern looks for.
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
/// Expands a repetition range into its explicit bounded form. The accepted
/// forms are:
///
/// - `{..}`  : all counts, `{1..*}`
/// - `{n..}` : n and up, `{n..*}`
/// - `{..n}` : 1 through n, `{1..n}`
/// - `*`     : shorthand for `{..}`
///
/// Params:
/// - expr: &str   -> single sanitized branch (no `|` alternation)
///
/// Return:
/// Option<String> -> branch with every range written in explicit form
///
/// Notes:
/// The open upper bound is written as `&` while the rewriting runs and turned
/// back into `*` at the end. `*` is itself one of the forms being matched, so
/// writing it straight back in would leave the loop expanding what it had
/// just expanded.
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
/// Splits the `+`-joined cardinal sums in one branch into branches of their
/// own, leaving every term with a single heading for the stages that follow.
///
/// ```text
/// n+eK      →  nK    eK
/// n+e+sK    →  nK    eK    sK
/// n+eKs+wK  →  nKsK  nKwK  eKsK  eKwK
/// ```
///
/// The split runs as a worklist rather than as a single pass. One sum is
/// rewritten per turn and every half goes back onto the stack, so a term
/// carrying two sums comes apart into their full cross product, and a
/// three-way sum comes apart over two turns, the pattern gripping one `+` at
/// a time. The branches come out in the order the stack drains rather than
/// the order they were written, which costs nothing when they are
/// alternatives, and repeats are dropped before the join.
///
/// Params:
/// - expr: &str   -> single sanitized branch (no `|` alternation)
///
/// Return:
/// Option<String> -> `|`-joined branches, one per cardinal combination
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
/// Runs one pipeline stage over each `|` branch of an expression separately.
/// Every stage rewrites a single branch and asserts that it was handed one, so
/// this is the only place alternation is taken apart, and the trimmed branch
/// text is what each stage actually reads.
///
/// A branch the stage rejects comes back as `None` in the slot the branch
/// occupied, leaving the caller a list it can still read positionally rather
/// than a shorter one it cannot.
///
/// Params:
/// - expr: &str                       -> expression containing `|` branches
/// - f   : impl Fn(&str) -> Option<T> -> stage applied to each branch
///
/// Return:
/// Vec<Option<T>>                     -> one result per branch, in input order
///
/// Notes:
/// The stage and its result are bound `Sync` and `Send` though the walk is
/// sequential. Branches never read one another, so the bounds are what keep a
/// parallel walk a change to this one line rather than to every caller.
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
/// Puts one raw move expression through the whole text pipeline, so what comes
/// back is a `|`-separated list of branches spelling out every atom,
/// direction, range and heading the expression only implied:
///
/// ```text
/// normalize → atomize → expand_directions → expand_ranges → expand_cardinals
/// ```
///
/// `normalize` reads the expression whole, alternation being the thing it
/// produces. Each of the four stages behind it reads one branch and may answer
/// with several, so every stage runs per branch and the pieces are rejoined
/// with `|` before the next stage starts.
///
/// What comes out is what the vector compilers read:
///
/// - `atomic_to_vector`
/// - `chained_atomic_to_vector`
/// - `compound_atomic_to_vector`
/// - `leg_to_vector`
///
/// Params:
/// - expr: &str -> raw move expression from the config
///
/// Return:
/// String       -> fully normalized and expanded `|`-separated expression
///
/// Notes:
/// The fold flattens, so a stage answering `None` would take its branch out of
/// the expression rather than stop the parse. None of the four does: each
/// answers `Some` for every branch it is handed.
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
/// Takes an irregular vector and determines its dominant direction: the axis
/// with the greater magnitude wins, and equal magnitudes resolve to the
/// diagonal between them.
///
/// On a 9x9 field around the origin `O`, every square snaps to the nearest
/// of the eight directions (`ne`/`nw`/`se`/`sw` cells are the diagonals):
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
/// - (2, 1) has +x as the greatest magnitude, influencing the next atomic
///   as "e", or (1, 0)
/// - (1, 2) has +y as the greatest magnitude, influencing the next atomic
///   as "n", or (0, 1)
/// - (2, 2) has equal magnitude, influencing the next atomic as "ne", or
///   (1, 1)
///
/// The name is one of eight string literals held in a static map, so it
/// outlives the vector it was read from. Saying so in the signature is what
/// lets a caller pass a temporary — every one of them wants the heading of
/// a displacement it computed on the spot, not of one it holds.
///
/// Params:
/// - vector: &(i8, i8) -> displacement whose heading is classified
///
/// Return:
/// &'static str        -> dominant cardinal direction name ("n", "ne", ...)
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
/// Puts the eight cardinal outcomes into one fixed clockwise order, which is
/// what lets a direction filter name them by number. The key is each vector's
/// angle taken with `atan2`, and it lands them NE, E, SE, S, SW, W, NW, N:
/// clockwise from the first diagonal, with north last.
///
/// Index of each direction, on a 9x9 field around the origin `O`
/// (NE=0, E=1, SE=2, S=3, SW=4, W=5, NW=6, N=7):
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
/// - vectors: Vec<AtomicVector> -> the eight cardinal outcomes to sort
///
/// Return:
/// Vec<AtomicVector>            -> the eight vectors in clockwise order
///
/// Notes:
/// A filter digit is one more than the index it selects, so `[1]` reaching a
/// group through here picks north-east. The same `[1]` written straight onto
/// a `K` never arrives: [`atomic_to_vector`] reads it against the prelude's
/// cardinal table, which counts north first. Anything other than the full
/// circle of eight panics, an order over part of a circle saying nothing.
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
/// Hands back the four half-plane tests of one rotated frame, so that a
/// direction filter written as `n` asks about north of the frame the
/// expression was turned into rather than north of the board.
///
/// An orthogonal frame tests the axes themselves, a diagonal frame tests the
/// two diagonals in their place:
///
/// - ne : "up" is right of the line x=-y, so x + y > 0
/// - se : "right" is left of the line x=-y, so x + y < 0
/// - nw : "up" is left of the line x=y, so x - y < 0
/// - sw : "right" is right of the line x=y, so x - y > 0
///
/// The `ne` frame on a 9x9 field: `n` counts as north (x + y > 0), `s` as
/// south, and the blank anti-diagonal through `O` is the fold line, where
/// both tests answer no and a vector lying on it belongs to neither side:
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
/// - direction: &str -> cardinal name of the rotated reference frame
///
/// Return:
///
///     impl Fn(i8, i8) -> (bool, bool, bool, bool)
///     the north, east, south and west tests of that frame, answered for one
///     point at a time
///
/// Notes:
/// A quadrant is asked for as two of the four, `ne` being north and east
/// together, so the four tests cover every filter the notation can write. A
/// name outside the eight panics, the frame being config text compiled once.
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

/// Repetition-token macros.
///
/// The two repetition forms — `{i..j}`, which repeats the atom it follows,
/// and `:{i..j}`, which re-evaluates the element it follows — match against
/// different regexes but are written the same way, so all four of their
/// handlers read the bounds out identically. File-private: nothing outside
/// the parser reads Betza notation.
///
/// range_bounds!
///
///   Matches a repetition token and returns its bounds as numbers. An open
///   end reads as `i8::MAX`, which the callers walk until a round adds no
///   new vectors. `label` names the form in both messages, so neither form
///   can end up reporting the other's name.
///
///   Params:
///   - regex: expr    -> the compiled token regex
///   - label: literal -> the form's name, as it appears in messages
///   - token: &str    -> the token being expanded
///
///   Return:
///   (Option<i8>, Option<i8>) -> start bound, end bound
///
/// colon_range_head!
///
///   Rejects a `:{i..j}` with nothing repeatable in front of it, then reads
///   its bounds. The variant name is a parameter because the atomic and
///   multi-leg element enums are distinct types; nothing else about the
///   check differs. `element` is named three times, so pass a binding.
///
///   Params:
///   - token  : &str            -> the `:{i..j}` being expanded
///   - element: Option<Element> -> element the token repeats
///   - eval   : ident           -> that enum's evaluated-expr variant
///
///   Return:
///   (Element, Option<i8>, Option<i8>) -> element, start bound, end bound
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
/// Keeps the vectors a direction filter names, reading its numbers against
/// the clockwise order [`sort_atomic_clockwise`] imposes here. The order is
/// imposed rather than assumed, so a caller may hand its working set over
/// however that set happened to be built.
///
/// The numbers arrive already turned into indices: the filter is written from
/// one and counted from zero, and the caller does the subtracting.
///
/// Params:
/// - vectors: Vec<AtomicVector> -> the eight cardinal candidates
/// - index  : Vec<usize>        -> clockwise indices to keep
///
/// Return:
/// Vec<AtomicVector>            -> the selected vectors, in index order
///
/// Notes:
/// A repeated number is honoured twice, that being what the filter asked for,
/// and a `[9]` indexes past the circle and panics there. A working set that is
/// not the full circle of eight panics before either can happen.
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
/// Keeps the vectors falling in the half-plane or quadrant `direction` names,
/// judged inside the frame `pov` turned the expression into. Each branch is
/// placed by its net displacement, so a route that wanders on the way is
/// judged by where it arrives rather than by where it went.
///
/// A quadrant asks for two half-planes at once, which is what leaves a vector
/// lying on a fold line out of every quadrant touching it: it is neither north
/// nor south of a line it sits on.
///
/// Params:
/// - vectors  : Vec<AtomicVector> -> working set of candidates
/// - direction: &str              -> cardinal/diagonal to keep
/// - pov      : &str              -> rotation frame the test is relative to
///
/// Return:
/// Vec<AtomicVector>              -> vectors passing the directional test
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
/// Drops displacements too large to land on this board from anywhere, which
/// is also what stops an unbounded repetition growing for ever: a slide is
/// walked until the clip leaves nothing new behind it.
///
/// The bound is the board's own width and height, a square looser than the
/// longest displacement that could ever land. Which square a vector starts
/// from is unknown here, so this is the strictest bound the stage can honestly
/// apply, and the exact judging waits for the origin to be known.
///
/// Params:
/// - vector: &mut Vec<AtomicVector> -> working set, pruned in place
/// - state : &State                 -> board dimensions for the bound
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
/// Drops repeats from a working set, keeping the copy that arrived first.
/// Expansion lands on the same displacement by more than one route often
/// enough — two branches of a range meeting at the board edge, two headings of
/// a chain folding together — and a repeat left in becomes a second identical
/// move at generation time.
///
/// Keeping first-seen order is what lets the repetition loops read saturation
/// off the length alone: a round that adds nothing new leaves the set exactly
/// as long as it found it.
///
/// Params:
/// - vectors: &mut Vec<Element> -> collection deduplicated in place
fn remove_duplicates_in_place<Element>(vectors: &mut Vec<Element>)
where
    Element: Clone + Eq + Hash,
{
    let mut seen = HashSet::new();
    vectors.retain(|vector| seen.insert(vector.clone()));
}

/// process_atomic_dots_token
///
/// Carries every branch on along its own last heading, one square for each dot
/// in the token. Branches spread rather than shift: each grows out of where it
/// already points, so a single token lengthens all eight headings at once.
///
/// Going east out of `S`, the same wazir step with more dots behind it:
///
/// ```text
/// ┌─────┬─────┬─────┬─────┐
/// │  S  │  K  │ K.  │ K.. │
/// └─────┴─────┴─────┴─────┘
/// ```
///
/// Params:
/// - vector_set: Vec<AtomicVector> -> branches accumulated so far
/// - token     : &str              -> the run of dots being applied
/// - state     : &State            -> board dimensions for clipping
///
/// Return:
/// Vec<AtomicVector>               -> the extended, clipped working set
///
/// Notes:
/// A branch reaching past the board is dropped whole rather than shortened,
/// and two branches landing on one square come back as one.
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
/// Repeats the atom in front of the token as many times as the range names.
/// A count is the number of steps in total, so `{1}` is the atom as written
/// and every count above it adds one more step of the last heading:
///
/// ```text
/// {1}  is  K      {2}  is  K.      {3}  is  K..
/// ```
///
/// A lone count answers with one branch per input, a spanned count with the
/// union over the span, which is how a slide arrives as every square it could
/// stop on rather than only the far one.
///
/// An open upper end is walked until the arithmetic stops moving. Steps are
/// added into a byte, which saturates, and a round that adds nothing new ends
/// the walk. The board clip runs once afterwards and takes out everything that
/// could never land.
///
/// Params:
/// - vector_set: Vec<AtomicVector> -> branches accumulated so far
/// - token     : &str              -> the `{i..j}` range token
/// - state     : &State            -> board dimensions for clipping
///
/// Return:
/// Vec<AtomicVector>               -> the branched, clipped working set
///
/// Notes:
/// A token carrying no lower bound panics. [`expand_ranges`] writes one into
/// every form the notation allows, so a token arriving without one never went
/// through the pipeline.
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
/// Repeats the element standing in front of the token, evaluating it again
/// for every count in the range and adding what comes back onto each branch
/// in hand.
///
/// This is where the two repetition forms part company. `{i..j}` lengthens a
/// branch along the step it last took, so the heading is fixed at the first
/// one; `:{i..j}` runs the element itself again, so whatever filters and
/// rotation the element carries are answered once per repetition. A repeated
/// route may therefore turn where a repeated step never can.
///
/// A spanned count accumulates, every count in the span contributing its own
/// branches, and the walk ends on the first count that adds nothing new. A
/// lone count contributes only itself.
///
/// Params:
/// - vector_set: Vec<AtomicVector>               -> branches so far
/// - token     : &str                            -> the `:{i..j}` range
/// - element   : Option<AtomicElement>           -> preceding element
/// - modifiers : &(Option<Token>, Option<Token>) -> pending filters
/// - state     : &State                          -> board dimensions
///
/// Return:
/// Vec<AtomicVector>                             -> the repeated, clipped set
///
/// Notes:
/// The token wants something repeatable in front of it: standing first in an
/// expression, or behind a bracket group already evaluated, it panics in
/// [`colon_range_head!`]. A count with no lower bound panics here.
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
/// Answers whichever filters were written in front of the element now being
/// evaluated. A cardinal token keeps the half-plane or quadrant it names,
/// an index filter keeps the clockwise positions its digits name, and an
/// element written with neither comes back whole.
///
/// Written together, the cardinal is answered first. The index filter counts
/// through a full circle of eight and asserts as much, so a pair where the
/// cardinal has already taken vectors away stops there rather than counting
/// through part of a circle. Digits become indices on the way, one subtracted
/// from each, and a character that is not a digit is passed over.
///
/// Params:
/// - vector_set: Vec<AtomicVector>               -> working set to filter
/// - modifiers : &(Option<Token>, Option<Token>) -> pending (cardinal, index)
/// - rotation  : &str                            -> current rotation frame
///
/// Return:
/// Vec<AtomicVector>                             -> the filtered working set
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
/// Runs one atom token against every branch in hand and grows each branch by
/// what comes back. The frame the atom is read in is that branch's own last
/// heading, so an atom standing behind something else is written relative to
/// where the route is already going rather than to the board.
///
/// The pending filters are answered inside that same frame, which is what
/// lets an `n` behind a diagonal step mean north of the step and not north of
/// the board.
///
/// Params:
/// - result   : Vec<AtomicVector>               -> branches so far
/// - term     : Token                           -> the atom token to expand
/// - modifiers: &(Option<Token>, Option<Token>) -> pending (cardinal, index)
///
/// Return:
/// Vec<AtomicVector>                            -> the extended working set
///
/// Notes:
/// Only an atom token belongs here and anything else panics, the caller having
/// sorted its tokens by kind already. Branches meeting on one square come back
/// as one.
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
/// Evaluates a `<...>` group once for every branch in hand, inside that
/// branch's own frame, and adds what comes back onto it.
///
/// The heading a group hands on is its net displacement and not the step it
/// ended with, so what follows a group is steered by where the group went as
/// a whole. That is what the brackets buy: a bare chain hands on only its
/// last step, and a bracketed one hands on the shape.
///
/// Params:
/// - result   : Vec<AtomicVector>               -> branches so far
/// - subexpr  : AtomicGroup                     -> the `<...>` token group
/// - modifiers: &(Option<Token>, Option<Token>) -> pending (cardinal, index)
/// - state    : &State                          -> board dimensions
///
/// Return:
/// Vec<AtomicVector>                            -> the extended working set
///
/// Notes:
/// The group has to answer with vectors; an element of any other kind panics
/// here rather than being carried further. Branches meeting on one square
/// come back as one.
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
/// Walks a token group from left to right, carrying one working set of
/// branches through it. The set begins as a single branch of no length already
/// pointing at `rotation`, so the first atom is read in the frame the whole
/// expression was turned into.
///
/// A filter is not answered where it is written but where it is spent: a
/// cardinal or index token is remembered, handed to the next atom or group,
/// and forgotten once handed over. A colon-range behind that atom takes it
/// first, which is why each atom is read together with the token after it.
///
/// Params:
/// - expr    : AtomicGroup -> the token group to evaluate
/// - rotation: &str        -> direction the expression is rotated toward
/// - state   : &State      -> board dimensions for bounds clipping
///
/// Return:
/// AtomicElement           -> an `AtomicEval` wrapping the evaluated branches
///
/// Notes:
/// A filter spent by a colon-range is handed over without being forgotten, so
/// the atom after the repetition is filtered by it a second time. Every clip
/// here judges the net displacement, so a route is asked where it lands and
/// never where it passed. Whether the squares in between exist is settled at
/// generation time, the origin being known only there.
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
/// Closes the innermost bracket sitting on a parser stack. Terms come off the
/// back until the opening mark is met and go onto the front of a second queue,
/// so the group is rebuilt in the order it was written, and what comes out is
/// wrapped once and pushed back as a single term.
///
/// ```text
/// before   …  (  A  B  C      the closing mark arrives
/// after    …  (A B C)
/// ```
///
/// Both parsers stack their terms this way and differ only in what an opening
/// mark looks like and what a group is called, so each hands those two answers
/// in and shares everything else.
///
/// Params:
/// - stack      : &mut VecDeque<Term> -> parser stack holding pending terms
/// - is_bracket : IsBracket           -> predicate matching the opening bracket
/// - wrap_result: WrapResult          -> constructor wrapping the popped group
///
/// Notes:
/// A closing mark with no opening one empties the stack and wraps all of it,
/// the walk ending on an empty stack rather than on a mark. Nothing refuses it
/// here, an unbalanced expression arriving further down as one oversized group
/// instead of as an error.
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
/// Reads one atom — an optional cardinal filter, an optional list of digits,
/// and the `K` that closes it — and answers with the unit steps it selects,
/// every one of them read in the frame `rotation` names.
///
/// The eight steps around a square are numbered clockwise from wherever the
/// frame points, so 1 is the frame's own heading and the ring turns with it.
/// Unrotated, that heading is north:
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
/// Written without digits an atom takes the whole ring. A cardinal in front
/// keeps only the steps leaning that way, itself turned by the frame first, so
/// an `n` there asks for the frame's north and not the board's.
///
/// Examples:
///
/// `K` takes the ring whole and `nK` keeps the three steps leaning north. In
/// `n[2468]K` turned toward north-east the digits pick out the four
/// orthogonals of that frame, and the `n` — north there pointing north-east —
/// keeps two of them:
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
/// - expr    : &str -> single atomic expression, e.g. `n[26]K`
/// - rotation: &str -> cardinal direction the atomic is rotated toward
///
/// Return:
/// Vec<(i8, i8)>    -> unit displacement vectors selected by the atomic
///
/// Notes:
/// A digit past 8 comes back round the ring, 9 selecting what 1 selects, and a
/// direction named twice is kept once. A 0 counts below the first index and
/// underflows instead of wrapping, the ring having no zeroth step to name.
/// Expressions arrive here already normalized by [`parse_move_string`]; one
/// the atom pattern cannot read panics, as does a rotation outside the eight
/// cardinal names.
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
/// Walks a run of atoms from left to right, each one read in the frame the
/// atom before it ended in. One branch goes in and as many come out as that
/// atom selects steps, every branch carrying both where it has arrived and
/// the step it arrived by.
///
/// The frame comes off that last step alone, so a chain hands on nothing but
/// its final heading. Brackets are what change this: a `<...>` group hands on
/// its net displacement instead, and that is [`evaluate_atomic_subexpression`].
///
/// A knight is a chain of two atoms. `N` normalizes to `[2468]Kn[2468]K`: the
/// first atom steps diagonally, and the second is read in the frame that step
/// left behind, where the digits fall on the four orthogonals and the `n`
/// keeps the two of them leaning the way the branch already points.
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
/// - expr    : &str -> chained atomic expression, e.g. `[2468]Kn[2468]K`
/// - rotation: &str -> cardinal direction the chain is rotated toward
///
/// Return:
/// Vec<AtomicVector> -> one (whole, last) pair per branch the chain reaches
///
/// Notes:
/// A `#` is the null step: it adds nothing and hands the frame on untouched,
/// which is how a leg that only turns is written. Displacement is added by
/// saturating byte arithmetic, so a chain long enough to run past the byte
/// stops growing rather than wrapping around it. Dots and ranges are not read
/// here — they suffix a chain and are answered by the token handlers. An
/// expression holding no atom at all asserts.
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
/// Compiles one whole atomic expression. Tokens are read off in order and
/// stacked, `<` marking where a group opens and `>` folding everything back
/// to that mark into a single nested term, so groups written inside groups
/// come out nested the same way. What the stack holds at the end is handed to
/// [`evaluate_atomic_expression`], which walks it and answers with branches.
///
/// Each token is placed by the first pattern that claims it, an atom being
/// what is left when none of the others do:
///
/// ```text
/// <  >          a group opens, and folds shut
/// n … sw        cardinal filter
/// [1357]        index filter
/// .  ..  {i}    repetition of the last step
/// :{i}          repetition of the last atom
/// K  nK  N      an atom
/// ```
///
/// Grouping is what a chain cannot say. In `nW<nWnF>nW` the bracketed pair
/// arrives at (±1, 2) as a whole, and it is that displacement the last `nW`
/// is read against — heading north by [`irregular_vector_direction`], even
/// though the step the group ended on was diagonal:
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
/// - expr    : &str   -> compound atomic expression, may contain `<...>`
/// - rotation: &str   -> cardinal direction the compound is rotated toward
/// - state   : &State -> board dimensions for bounds clipping
///
/// Return:
/// Vec<AtomicVector>  -> one (whole, last) pair per branch of the expression
///
/// Notes:
/// A `>` arriving with no `<` behind it wraps everything stacked so far, so
/// an unbalanced expression compiles to one oversized group rather than to an
/// error. The walk answering with anything but branches panics, no other kind
/// being reachable from a token stack.
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
/// Collapses a branch to the one square it ends on. Each leg already carries
/// the net displacement of the atom it was built from, so adding those up is
/// adding up the route: what every filter downstream asks a branch is where it
/// arrives, never which way it went there.
///
/// Params:
/// - vectors: &MultiLegVector -> legs of one branch, in the order walked
///
/// Return:
/// (i8, i8)                   -> net (x, y) the branch lands on
///
/// Notes:
/// The running sum saturates instead of wrapping, a long repetition otherwise
/// folding a far landing back around next to the origin.
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
/// Lays the eight rotations of a multi-leg expression out in a circle, so an
/// index filter behind them selects by position rather than by the order they
/// happened to be evaluated in. The circle is the one drawn in
/// [`sort_atomic_clockwise`], read the same way and starting at the same
/// place.
///
/// A branch is placed by the square it lands on, not by the frame it was
/// rotated into. The two agree for a `K` and part company as soon as the
/// expression bends: a leg pair heading north-east that ends due east sits
/// with east, which is where a reader following the piece would look for it.
///
/// Params:
/// - vectors: Vec<MultiLegVector> -> the eight rotations to lay out
///
/// Return:
/// Vec<MultiLegVector>            -> the same eight, clockwise from north
///
/// Notes:
/// Anything other than the full circle of eight panics, an order over part of
/// a circle saying nothing. Two rotations landing on the same square keep
/// whichever order they arrived in, the sort being stable.
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
/// Keeps the branches a direction filter names, reading its numbers against
/// the clockwise order [`sort_multi_leg_clockwise`] imposes here. The order is
/// imposed rather than assumed, so a caller may hand its working set over
/// however that set happened to be built.
///
/// The numbers arrive already turned into indices: the filter is written from
/// one and counted from zero, and the caller does the subtracting.
///
/// Params:
/// - vectors: Vec<MultiLegVector> -> the eight rotations to choose from
/// - index  : Vec<usize>          -> clockwise indices to keep
///
/// Return:
/// Vec<MultiLegVector>            -> the selected branches, in index order
///
/// Notes:
/// A repeated number is honoured twice, that being what the filter asked for,
/// and a number past the circle indexes out of range and panics there. The
/// sort behind this wants the whole circle of eight, so a set a cardinal
/// filter has already thinned cannot be indexed after it.
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
/// Keeps the branches falling in the half-plane or quadrant `direction` names,
/// judged inside the frame `pov` turned the expression into. A branch is placed
/// by the square it ends on, so one that wanders on the way is judged by where
/// it arrives and not by any leg it took to get there.
///
/// A quadrant asks for two half-planes at once, which is what leaves a branch
/// landing on a fold line out of every quadrant touching it: it is neither
/// north nor south of a line it sits on.
///
/// Params:
/// - vectors  : Vec<MultiLegVector> -> working set of branches
/// - direction: &str                -> cardinal/diagonal to keep
/// - pov      : &str                -> rotation frame the test is relative to
///
/// Return:
/// Vec<MultiLegVector>              -> branches passing the directional test
///
/// Notes:
/// A direction outside the eight panics, the name having come from config text
/// that is compiled once and read many times.
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
/// Drops branches landing too far away to fit on this board from anywhere,
/// which is also what stops an unbounded repetition growing for ever: a leg is
/// repeated until the clip leaves nothing new behind it.
///
/// Only the landing square is measured, the same bound the atomic stage
/// applies in [`filter_atomic_out_of_bounds`]. A branch is therefore kept on
/// the strength of where it ends even when a leg in the middle reaches further
/// out, and whether that middle square exists is settled at generation time,
/// where the origin is finally known.
///
/// Params:
/// - vectors: &mut Vec<MultiLegVector> -> working set, pruned in place
/// - state  : &State                   -> board dimensions for the bound
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
/// Walks every branch on along the leg it last took, once for each dot in the
/// token. Where the atomic stage folds a repeat into the displacement it is
/// already carrying, a repeat here is pushed on as a leg of its own: a leg is
/// what the runtime steps through, and only keeping them apart leaves it the
/// squares in between to find a blocker standing on.
///
/// Params:
/// - vector_set: Vec<MultiLegVector> -> branches accumulated so far
/// - token     : &str                -> the run of dots being applied
/// - state     : &State              -> board dimensions for clipping
///
/// Return:
/// Vec<MultiLegVector>               -> the extended, clipped working set
///
/// Notes:
/// A leading `-` says where the leg begins rather than that it repeats, so it
/// is taken off before the dots are counted. A branch landing past the board is
/// dropped whole rather than shortened, and branches are compared leg by leg,
/// so two that reach one square by different routes are two moves and both
/// stay.
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
/// Repeats the leg a branch last took as many times as the range names. A
/// count is the number of legs in total, so `{1}` is the branch as written and
/// every count above it lays one more copy of that leg behind it:
///
/// ```text
/// nW-{1}  is  nW      nW-{2}  is  nW-nW      nW-{3}  is  nW-nW-nW
/// ```
///
/// A lone count answers with one branch per input, a spanned count with the
/// union over the span, which is how a slide arrives as every square it could
/// stop on rather than only the far one.
///
/// An open upper end is walked until the arithmetic stops moving. Legs add into
/// a byte, which saturates, and a round that adds nothing new ends the walk.
/// The board clip runs once afterwards and takes out every branch that could
/// never land.
///
/// Params:
/// - vector_set: Vec<MultiLegVector> -> branches accumulated so far
/// - token     : &str                -> the `{i..j}` range token
/// - state     : &State              -> board dimensions for clipping
///
/// Return:
/// Vec<MultiLegVector>               -> the branched, clipped working set
///
/// Notes:
/// A token carrying no lower bound panics. [`expand_ranges`] writes one into
/// every form the notation allows, so a token arriving without one never went
/// through the pipeline.
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
/// Repeats the element standing in front of the token, evaluating it again for
/// every count in the range and hanging what comes back off each branch in
/// hand.
///
/// This is where the two repetition forms part company. `{i..j}` lengthens a
/// branch along the leg it last took, so the heading is fixed at the first one;
/// `:{i..j}` runs the element itself again, and does so in the heading the
/// branch has reached by then, so a repeated route may turn where a repeated
/// leg never can.
///
/// The repetitions go over as a slash group, which is what keeps that heading
/// honest: the legs of the group are left standing as they were walked, so the
/// next repetition reads the last of them rather than the straight line the
/// group happens to add up to.
///
/// A spanned count accumulates, every count in the span contributing its own
/// branches, and the walk ends on the first count that adds nothing new. A lone
/// count contributes only itself.
///
/// Params:
/// - vector_set: Vec<MultiLegVector>     -> branches accumulated so far
/// - token     : &str                    -> the `:{i..j}` colon-range
/// - element   : Option<MultiLegElement> -> preceding element to repeat
/// - modifiers : &[Option<Token>; 3]     -> pending (cardinal, index, move)
/// - rotation  : &str                    -> fallback rotation frame
/// - state     : &State                  -> board dimensions for clipping
///
/// Return:
/// Vec<MultiLegVector>                   -> the repeated, clipped working set
///
/// Notes:
/// The token wants something repeatable in front of it: standing first in an
/// expression, or behind a bracket group already evaluated, it panics in
/// [`colon_range_head!`]. With no branches in hand yet the repetitions are read
/// against `rotation`, that being the only heading there is to read them in.
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
/// Answers whichever of the three modifiers were written in front of the
/// element now being evaluated. Two of them thin the working set, the third
/// changes what a branch that survived is allowed to do:
///
/// - a cardinal keeps the half-plane or quadrant it names
/// - an index keeps the clockwise positions its digits name
/// - a move modifier is written onto the last leg of every branch left
///
/// The move modifier lands on the last leg alone because that is the leg a
/// move is played on: a letter saying capture or quiet is about the square the
/// piece ends on, the legs before it having only carried it there.
///
/// Written together, the cardinal is answered first. The index filter counts
/// through a full circle of eight and asserts as much, so a pair where the
/// cardinal has already taken branches away stops there rather than counting
/// through part of a circle. Digits become indices on the way, one subtracted
/// from each, and a character that is not a digit is passed over.
///
/// Params:
/// - vector_set: Vec<MultiLegVector> -> working set to filter
/// - modifiers : &[Option<Token>; 3] -> pending (cardinal, index, move)
/// - rotation  : &str                -> current rotation frame
///
/// Return:
/// Vec<MultiLegVector>               -> the filtered working set
///
/// Notes:
/// A branch with no legs at all cannot be written on and panics, every route
/// reaching here having been grown from at least one atom.
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
/// Grows every branch in hand by one leg. The leg is read in that branch's own
/// heading, so a leg standing behind another is written relative to where the
/// route is already going rather than to the board, and the filters waiting in
/// front of it are answered in that same frame.
///
/// What the leg leaves behind is one leap. However many steps the term expanded
/// into, the leg's last step is set to the whole leg's displacement, so the leg
/// after it takes its heading from where this one arrived and not from the
/// small step it happened to end on. A slash group is how the other reading is
/// asked for, [`evaluate_multi_leg_subexpression`] leaving those legs alone.
///
/// Params:
/// - result   : Vec<MultiLegVector> -> branches accumulated so far
/// - term     : Token               -> the leg token to expand
/// - modifiers: [Option<Token>; 3]  -> pending (cardinal, index, move)
/// - rotation : &str                -> direction context for expansion
/// - state    : &State              -> board dimensions for clipping
///
/// Return:
/// Vec<MultiLegVector>              -> the extended working set
///
/// Notes:
/// Only a leg token belongs here and anything else panics, the caller having
/// sorted its tokens by kind already. With nothing in hand yet the leg is read
/// against `rotation`, that being the only heading there is, and branches that
/// walked the very same legs come back as one.
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
/// Evaluates a bracketed group against every branch in hand and hangs what
/// comes back off the end of each. The group is read in that branch's own
/// heading, exactly as a bare leg is, so a group written behind a step turns
/// with the route instead of pointing back at the board.
///
/// The two bracket forms are evaluated alike and part company only in what
/// they leave behind for the next element to read:
///
/// ```text
/// <  >     the group ends pointed the way it went as a whole
/// </  />   the group ends pointed the way its last leg went
/// ```
///
/// A group `<nWnF>` arrives at (±1, 2), which reads as north, while the leg it
/// ended on went diagonally. The plain form hands north to whatever follows,
/// the slash form hands over the diagonal. A route that bends needs the second
/// reading: each repetition carries on from the leg it actually ended on, which
/// is how a crooked slider keeps turning instead of straightening out after the
/// first group.
///
/// Params:
/// - result   : Vec<MultiLegVector> -> branches accumulated so far
/// - expr     : MultiLegElement     -> the bracketed element
/// - modifiers: &[Option<Token>; 3] -> pending (cardinal, index, move)
/// - rotation : &str                -> direction context for expansion
/// - state    : &State              -> board dimensions for clipping
///
/// Return:
/// Vec<MultiLegVector>              -> the extended working set
///
/// Notes:
/// Only the two bracket forms belong here and anything else panics, as does an
/// inner group that comes back unevaluated. With nothing in hand yet the group
/// is read against `rotation` and becomes the working set itself.
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
/// Walks a token group from left to right, carrying one working set of branches
/// through it. The set begins empty rather than at an origin, a multi-leg route
/// having no length until its first leg is read, and every leg and group after
/// that hangs off the branches already in hand.
///
/// A filter is not answered where it is written but where it is spent: a
/// cardinal, an index or a move modifier is remembered, handed to the next leg
/// or group, and cleared once handed over. A colon-range behind that element
/// takes it first, which is why each element is read together with the token
/// after it.
///
/// A group met by a colon-range goes over in its slash form whichever way it
/// was written, so the repetitions follow the leg the group ended on. An `@`
/// exclusion is evaluated on its own and subtracted at the end, and the last
/// one written is the one that counts.
///
/// Params:
/// - expr    : MultiLegGroup -> the token group to evaluate
/// - rotation: &str          -> direction the expression is rotated toward
/// - state   : &State        -> board dimensions for bounds clipping
///
/// Return:
/// MultiLegElement           -> a `MultiLegEval` wrapping the branches
///
/// Notes:
/// A filter spent by a colon-range is handed over without being cleared, so the
/// element after the repetition is filtered by it a second time. The board clip
/// runs once at the end over whole branches, so a route is asked where it lands
/// and never where it passed. Whether the squares in between exist is settled
/// at generation time, the origin being known only there.
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
/// Cuts one branch into the tokens the stack parser reads, then glues back
/// together what only looked like several. A bracket group with no leg boundary
/// inside it is one compound atomic, and the parser wants it as one word rather
/// than as the letters it was cut into:
///
/// ```text
/// m<[1357]K>-c<[2468]K>.
/// m  <  [1357]K  >  -  c  <  [2468]K  >  .    cut on the alphabet
/// m  <[1357]K>      -  c  <[2468]K>      .    groups closed up
/// m  <[1357]K>      -  c  <[2468]K>.          suffix rejoined
/// ```
///
/// What keeps a group open is whatever makes it more than a compound atomic: a
/// leg boundary inside it, a slash bracket, or a run of move modifiers, each of
/// which the parser has to see for itself. Closing runs to a fixpoint, since
/// closing one group can leave two fragments adjacent that were not before.
///
/// The last pass is where a `-` earns its keep. A suffix written straight onto
/// a group joins it and repeats a step inside that leg; the same suffix written
/// behind a `-` stays a token of its own and repeats the whole leg.
///
/// Params:
/// - expr: &str -> one sanitized multi-leg branch
///
/// Return:
/// Vec<String>  -> tokens ready for the multi-leg stack parser
///
/// Notes:
/// An expression the alphabet matches nowhere panics. Text between two matches
/// is kept as a token of its own, so a stray character travels on to the parser
/// and is refused there rather than here.
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
/// Reads one leg into branches. A leg is written in three parts, and each part
/// is optional except the middle one:
///
/// ```text
/// mc     [26]K            @nK
/// what   where it goes    where it may not end
/// ```
///
/// The exclusion is answered on landings alone: `mc[26]K@nK` is that step
/// everywhere except where it would arrive on the square `nK` reaches. How
/// either of them got there does not enter into it, only that the two agree on
/// a square, and both are read in the same rotation.
///
/// What comes back is one branch per surviving displacement, each holding the
/// single leg, and each leg carrying the modifier letters written in front of
/// it. The letters are copied as text and read at generation time, so a leg
/// says what it is allowed to do without this stage knowing what any of the
/// letters mean.
///
/// Params:
/// - expr    : &str    -> one leg expression, e.g. `mc[26]K@nK`
/// - rotation: &str    -> cardinal direction the leg is rotated toward
/// - state   : &State  -> board dimensions for bounds clipping
///
/// Return:
/// Vec<MultiLegVector> -> single-leg branches, one per displacement
///
/// Notes:
/// A leg the grammar does not fit panics naming it, and so does one with no
/// compound atomic in it, nothing being left to say where the piece goes.
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
/// Compiles one whole branch of a movement expression, legs and all. Tokens are
/// read off in order and stacked, `<` and `</` marking where a group opens and
/// `>` and `/>` folding everything back to that mark into a single nested term,
/// and the stack that comes out is handed on to be evaluated:
///
/// ```text
/// <  >  </  />   a group opens, and folds shut
/// n … sw         cardinal filter
/// [1357]         index filter
/// -.  -..        repetition of the last leg
/// -{i}           the same, counted
/// -:{i}          repetition of the last leg or group, re-read each time
/// @expr          landings to leave out
/// mcd … !        what the leg may do
/// -              a leg boundary
/// ```
///
/// A leg keeps its own stop square, which is what an expression of one leg
/// cannot say. In `eK-{4}-nK` the piece walks four squares east and turns north
/// for a fifth, and each of those stops is a square where the leg's modifiers
/// are answered at generation time:
///
/// ```text
/// ┌───┬───┬───┬───┬───┐
/// │   │   │   │   │ ● │
/// ├───┼───┼───┼───┼───┤
/// │ S │ → │ → │ → │ ↑ │
/// └───┴───┴───┴───┴───┘
/// ```
///
/// A branch with no `-` in it is one leg and goes straight to
/// [`leg_to_vector`], the stack having nothing to fold.
///
/// Params:
/// - expr    : &str    -> one sanitized multi-leg branch
/// - rotation: &str    -> direction the whole expression is rotated toward
/// - state   : &State  -> board dimensions for bounds clipping
///
/// Return:
/// Vec<MultiLegVector> -> every concrete branch of the expression
///
/// Notes:
/// A closing mark arriving with no opening one wraps everything stacked so far,
/// [`process_closing_bracket`] ending on an empty stack rather than refusing.
/// The `-` itself is dropped once the tokens are cut, having said all it had to
/// say by keeping the legs apart. Anything the classification does not
/// recognise is taken for a leg, and a leg the grammar refuses panics further
/// down in [`leg_to_vector`].
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
/// Turns one movement expression from a config into every route it names. This
/// is the way in to the whole file: what a variant wrote goes in as text and
/// what comes out is the piece's movement, compiled once at load time.
///
/// Two halves do the work. [`parse_move_string`] rewrites the expression until
/// nothing is left but plain branches divided by `|`, and each of those is
/// compiled on its own by [`multi_leg_to_vector`]:
///
/// ```text
/// text  →  normalized  →  branch  →  legs  →  displacements
/// ```
///
/// Every branch is read in the north frame, that being the direction a piece is
/// taken to face. Which way north lies for the side to move is settled at
/// generation time, where the offsets are mirrored by colour, so one compiled
/// set of routes serves both players.
///
/// Params:
/// - expr : &str       -> raw move expression from the config
/// - state: &State     -> board dimensions for bounds clipping
///
/// Return:
/// Vec<MultiLegVector> -> deduplicated branches for the whole expression
///
/// Notes:
/// Two branches that walked the very same legs come back as one, which is what
/// keeps an expression naming a route twice from generating it twice. Branches
/// reaching one square by different legs are different moves and both stay.
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
