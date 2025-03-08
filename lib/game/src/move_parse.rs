use std::{char, vec};
use regex::Regex;
use lazy_static::lazy_static;
use timed::timed;

lazy_static! {
    static ref NORMALIZE_PATTERN: Regex = Regex::new(
        r"[^(^|]\(|\)[^()^|]"
    ).unwrap();
    static ref RANGE_PATTERN: Regex = Regex::new(
        r"(-?)(?:\{(?:\.\.(\d+)|(\d+)\.\.|\.\.)\}|\*)"
    ).unwrap();
    static ref EXPANDED_RANGE_PATTERN: Regex = Regex::new(
        r"([^:-]+)(:?)(-?)(?:\{([0-9]+)(?:\.\.([0-9]+))?\})(.*)"
    ).unwrap();
    static ref CARDINAL_PATTERN: Regex = Regex::new(
        r"([nsew]{1,2}\+)*"
    ).unwrap();
    static ref K_PATTERN: Regex = Regex::new(
        r"(?:^|[^\]])(K)"
    ).unwrap();
    static ref DIR_RANGE_PATTERN: Regex = Regex::new(
        r"\[(?:\.\.(\d+)|(\d+)\.\.|(\d+)\.\.(\d+)|\.\.)\]"
    ).unwrap();
    static ref MULTI_DIR_PATTERN: Regex = Regex::new(
        r"\[([0-9]{2,})\]"
    ).unwrap();
    static ref INVALID_CARDINAL_PATTERN: Regex = Regex::new(
        &[
            r"ne<?\((?:-\d+|0), (?:-\d+|0)\)|",
            r"se<?\((?:-\d+|0), (?:\d+|0)\)|",
            r"nw<?\((?:\d+|0), (?:-\d+|0)\)|",
            r"sw<?\((?:\d+|0), (?:\d+|0)\)|",
            r"n<?\([^)]*?, (?:-\d+|0)\)|",
            r"s<?\([^)]*?, (?:\d+|0)\)|",
            r"e<?\((?:-\d+|0)[^)]*?\)|",
            r"w<?\((?:\d+|0)[^)]*?\)"
        ].join("")
    ).unwrap();
    static ref VECTOR_PATTERN: Regex = Regex::new(
        r"\((-?\d+), (-?\d+)\)"
    ).unwrap();
    static ref OFFSET_PATTERN: Regex = Regex::new(
        r"\[([0-7])\]"
    ).unwrap();
}

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

    while let Some(cap) = EXPANDED_RANGE_PATTERN.captures(&expanded) {
        let (prefix, suffix) = (
            cap.get(1).unwrap().as_str(),
            cap.get(6).unwrap().as_str()
        );

        let colon = cap.get(2).unwrap().as_str();
        let dash = cap.get(3).unwrap().as_str();
        let start = cap[4].parse::<usize>().unwrap();
        let end = cap.get(5).map_or(start, |m| m.as_str().parse().unwrap());

        let mut vec_rep = Vec::new();
        for n in start..=end {
            vec_rep.push(match (!colon.is_empty(), !dash.is_empty()) {
                (true, true) => {
                    if suffix.is_empty() {
                        format!("{}-", prefix).repeat(n - 1) + &prefix          /* Handle move:-{n} repetition        */
                    } else {
                        format!("{}-", prefix).repeat(n) + suffix               /* Handle move:-{n}suffix             */
                    }
                }
                (true, false) => {
                    prefix.replacen(prefix, &prefix.repeat(n), 1) + suffix      /* Handle move:{n} piece repetition   */
                }
                (false, true) => {
                    prefix.to_owned() + &"-.".repeat(n - 1) + &suffix           /* Handle move-{n} dash repetition    */
                }
                _ => format!("{}{}{}", prefix, ".".repeat(n - 1), suffix),      /* Handle normal {n} dot repetition   */
            });
        }

        expanded = expanded.replacen(&cap[0], &vec_rep.join("|"), 1);
    }

    Some(expanded)                                                              /* Return Some with expanded ranges   */
}

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

fn expand_directions(expr: &str) -> Option<String> {
    if !expr.contains('K') {
        return Some(expr.to_string());
    }

    let mut expanded = expr.to_string();

    while let Some(cap) = K_PATTERN.captures(&expanded) {
        let cap_str = cap.get(1).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &"[12345678]&", 1);               /* Replace bare K with all directions */
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

fn tokenize(expr: &str) -> Vec<String> {
    let mut result = Vec::with_capacity(expr.len() / 2);
    let mut remaining = expr;

    'parse: while !remaining.is_empty() {
        for pattern in &[
            &Regex::new(r"(^</?)").unwrap(),
            &Regex::new(r"(^/?>)").unwrap(),
            &Regex::new(r"(^/)").unwrap(),
            &Regex::new(r"(^-\.?)").unwrap(),
            &Regex::new(r"(^\.)").unwrap(),
            &Regex::new(r"(^\(-?\d+, -?\d+\)\[[0-7]\])").unwrap(),
        ] {
            if let Some(cap) = pattern.find(remaining) {
                result.push(cap.as_str().to_string());
                remaining = &remaining[cap.end()..];
                continue 'parse;
            }
        }
        break;
    }

    result.push("$".to_string());
    result
}

fn parse_vector(expr: &str) -> (i32, i32) {
    if let Some(cap) = VECTOR_PATTERN.captures(expr) {
        let x = cap[1].parse::<i32>().unwrap_or(0);
        let y = cap[2].parse::<i32>().unwrap_or(0);
        (x, y)
    } else {
        (0, 0)
    }
}

fn parse_offset(expr: &str) -> usize {
    if let Some(cap) = OFFSET_PATTERN.captures(expr) {
        cap[1].parse::<usize>().unwrap_or(0)
    } else {
        0
    }
}

fn add_vectors(a: (i32, i32), b: (i32, i32)) -> (i32, i32) {
    (a.0 + b.0, a.1 + b.1)
}

fn transpose_vector(vector: (i32, i32), offset: usize) -> (i32, i32) {
    let directions = [
        (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)
    ];

    println!("{:?} {:?}", vector, offset);

    let base_idx = directions.iter().position(|&s| s == vector).unwrap();

    directions[(base_idx + offset) % 8]
}

fn major_axis(vector: (i32, i32)) -> usize {
    match (
        vector.0.abs() < vector.1.abs(),
        vector.0.abs() > vector.1.abs(),
        vector.0.is_negative(),
        vector.1.is_negative()
    ) {
        (false, false, true, true) => 5,
        (false, false, true, false) => 7,
        (false, false, false, true) => 3,
        (false, false, false, false) => 1,
        (true, false, _, false) => 0,
        (false, true, false, _) => 2,
        (true, false, _, true) => 4,
        (false, true, true, _) => 6,
        _ => unreachable!("Invalid vector: {:?}", vector),
    }
}

fn collapse_directions(expr: &str) -> Option<String> {
    let mut tokens = tokenize(expr);
    let mut result_stack: Vec<String> = Vec::new();
    let mut offset_stack: Vec<(usize, usize)> = Vec::new();
    let mut vector_stack: Vec<(i32, i32)> = Vec::new();

    tokens.reverse();

    while !tokens.is_empty() {
        println!("{:?} {:?} {:?} {:?}", tokens, offset_stack, vector_stack, result_stack);
        let current = tokens.pop().unwrap();

        if current == "$" {
            break;
        }

        else if current == "-" {
            let (_, prev_offset) = offset_stack.last().unwrap();
            result_stack.push(format!("(0, 0)[{}]", prev_offset));
        }

        else if current == "<" || current == "</" {

            if
                vector_stack.is_empty() &&
                offset_stack.is_empty() &&
                result_stack.is_empty()

            {
                vector_stack.push((0, 0));
                offset_stack.push((0, 0));
                result_stack.push(current);
                continue;
            }

            let (_, prev_offset) = offset_stack.last().unwrap();
            offset_stack.push((*prev_offset, *prev_offset));
            result_stack.push(current);
        }

        else if current == ">" || current == "/>" {
            let prev_vector1 = vector_stack.pop().unwrap();
            let prev_vector2 = vector_stack.pop().unwrap();

            let prev_offset = if current == ">" {
                let (prev_offset, _) = offset_stack.pop().unwrap();
                prev_offset
            } else {
                let (_, prev_offset) = offset_stack.pop().unwrap();
                prev_offset
            };

            let new_vector = add_vectors(prev_vector1, prev_vector2);
            let new_offset = major_axis(new_vector);

            vector_stack.push(new_vector);
            offset_stack.push((new_offset, prev_offset));
            result_stack.push(format!("{}[{}]", current, prev_offset));
        }

        else {
            let vector = parse_vector(&current);
            let offset = parse_offset(&current);

            if
                vector_stack.is_empty() &&
                offset_stack.is_empty() &&
                result_stack.is_empty() ||
                result_stack.last().unwrap() == "<" ||
                result_stack.last().unwrap() == "</"

            {
                vector_stack.push(vector);
                offset_stack.push((offset, offset));
                result_stack.push(current);
                continue;
            }

            let prev_token = result_stack.pop().unwrap();
            let prev_global_vec = vector_stack.pop().unwrap();
            let _ = offset_stack.pop().unwrap();

            let prev_vector = parse_vector(&prev_token);
            let prev_offset = parse_offset(&prev_token);

            let transposed = transpose_vector(vector, prev_offset);

            let new_global_vec = add_vectors(prev_global_vec, transposed);
            vector_stack.push(new_global_vec);
            offset_stack.push((major_axis(new_global_vec), major_axis(vector)));

            let new_vector = add_vectors(prev_vector, transposed);
            let new_offset = major_axis(new_vector);

            result_stack.push(format!("{:?}[{}]", new_vector, new_offset));
        }
    }

    let result = result_stack
        .join("-")
        .replace("<-", "<")
        .replace("</-", "</")
        .replace("->", ">")
        .replace("-/>", "/>");

    println!();

    Some(result)
}

fn split_and_process(expr: &str, f: fn(&str) -> Option<String>) -> String {
    expr.split('|')
        .filter_map(f)
        .collect::<Vec<String>>()
        .join("|")                                                              /* Process each term separately       */
}

#[timed]
pub fn parse_move(expr: &str) -> String {
    if expr.is_empty() {
        return String::new();
    }

    let expr = normalize(expr).unwrap_or_default();
    if expr.is_empty() {
        return String::new();
    }

    let pipeline = [
        atomize,
        expand_ranges,
        expand_cardinals,
        expand_directions,
        vectorize,
        collapse_cardinals,
        collapse_directions,
    ];

    pipeline
        .iter()
        .fold(expr, |acc, &step| {split_and_process(&acc, step)})               /* Process each step in the pipeline  */
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn test_advancer() {
    //     let expanded = expand("Q-nW");
    //     assert_eq!(expanded, "Q-nW");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "K-*-n[1357]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "K-*-n[1357]K");
    // }

    // #[test]
    // fn test_amazon() {
    //     let expanded = expand("Q|N");
    //     assert_eq!(expanded, "Q|N");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "K-*|[2468]Kn[2468]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "K-*|[2468]Kn[2468]K");
    // }

    // #[test]
    // fn test_antelope() {
    //     let expanded = expand("WnF{3}");
    //     assert_eq!(expanded, "WnF{3}");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[1357]Kn[2468]K{3}");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[1357]Kn[2468]K...");
    // }

    // #[test]
    // fn test_archbishop() {
    //     let expanded = expand("B|N");
    //     assert_eq!(expanded, "B|N");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[2468]K-*|[2468]Kn[2468]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[2468]K-*|[2468]Kn[2468]K");
    // }

    // #[test]
    // fn test_barc() {
    //     let expanded = expand("[2367]N");
    //     assert_eq!(expanded, "[2367]N");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[2367][2468]Kn[2468]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[2367][2468]Kn[2468]K");
    // }

    // #[test]
    // fn test_bede() {
    //     let expanded = expand("B|D");
    //     assert_eq!(expanded, "B|D");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[2468]K-*|[1357]K.");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[2468]K-*|[1357]K.");
    // }

    // #[test]
    // fn test_berolina_pawn() {
    //     let expanded = expand("mFi<F-pF>cnW");
    //     assert_eq!(expanded, "mFi<F-pF>cnW");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "m[2468]Ki<[2468]K-p[2468]K>cn[1357]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "m[2468]Ki<[2468]K-p[2468]K>cn[1357]K");
    // }

    // #[test]
    // fn test_berolina_plus_pawn() {
    //     let expanded = expand("mFi<F-pF>c[137]K");
    //     assert_eq!(expanded, "mFi<F-pF>c[137]K");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "m[2468]Ki<[2468]K-p[2468]K>c[137]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "m[2468]Ki<[2468]K-p[2468]K>c[137]K");
    // }

    // #[test]
    // fn test_bishopper() {
    //     let expanded = expand("(cB|dB-u#)-mnW");
    //     assert_eq!(expanded, "cB-mnW|dB-u#-mnW");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(
    //         atomized,
    //         "c[2468]K-*-mn[1357]K|d[2468]K-*-u#-mn[1357]K"
    //     );
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(
    //         ranged,
    //         "c[2468]K-*-mn[1357]K|d[2468]K-*-u#-mn[1357]K"
    //     );
    // }

    // #[test]
    // fn test_grasshopper() {
    //     let expanded = expand("(cQ|dQ-u#)-mnW");
    //     assert_eq!(expanded, "cQ-mnW|dQ-u#-mnW");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "cK-*-mn[1357]K|dK-*-u#-mn[1357]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "cK-*-mn[1357]K|dK-*-u#-mn[1357]K");
    // }

    // #[test]
    // fn test_shogi_lion() {
    //     let expanded = expand("#|N|S|cKK");
    //     assert_eq!(expanded, "#|N|S|cKK");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "#|[2468]Kn[2468]K|K.|cKK");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "#|[2468]Kn[2468]K|K.|cKK");
    // }

    // #[test]
    // fn test_locust() {
    //     let expanded = expand("cQ-mnW");
    //     assert_eq!(expanded, "cQ-mnW");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "cK-*-mn[1357]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "cK-*-mn[1357]K");
    // }

    // #[test]
    // fn test_mao() {
    //     let expanded = expand("W-nF");
    //     assert_eq!(expanded, "W-nF");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[1357]K-n[2468]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[1357]K-n[2468]K");
    // }

    // #[test]
    // fn test_mao_hopper() {
    //     let expanded = expand("cdW-u#-nF");
    //     assert_eq!(expanded, "cdW-u#-nF");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "cd[1357]K-u#-n[2468]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "cd[1357]K-u#-n[2468]K");
    // }

    // #[test]
    // fn test_moa() {
    //     let expanded = expand("F-nF");
    //     assert_eq!(expanded, "F-nF");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[2468]K-n[2468]K");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[2468]K-n[2468]K");
    // }

    // #[test]
    // fn test_rose() {
    //     let expanded = expand("Wn(eF|wF):-*");
    //     assert_eq!(expanded, "WneF:-*|WnwF:-*");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[1357]Kne[2468]K:-*|[1357]Knw[2468]K:-*");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[1357]Kne[2468]K:-*|[1357]Knw[2468]K:-*");
    // }

    // #[test]
    // fn test_ubi_ubi() {
    //     let expanded = expand("N:*");
    //     assert_eq!(expanded, "N:*");
    //     let atomized = atomize(&expanded);
    //     assert_eq!(atomized, "[2468]Kn[2468]K:*");
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(ranged, "[2468]Kn[2468]K:*");
    // }

    // #[test]
    // fn test_crooked_bishop() {
    //     let expanded = expand("F(#|((#|eW)-<wW-eW>|(#|wW)-<eW-wW>):-*)");
    //     assert_eq!(
    //         expanded,
    //         "F|F-<wW-eW>:-*|FeW-<wW-eW>:-*|F-<eW-wW>:-*|FwW-<eW-wW>:-*"
    //     );
    //     let atomized = atomize(&expanded);
    //     assert_eq!(
    //         atomized,
    //         "[2468]K\
    //         |[2468]K-<w[1357]K-e[1357]K>:-*\
    //         |[2468]Ke[1357]K-<w[1357]K-e[1357]K>:-*\
    //         |[2468]K-<e[1357]K-w[1357]K>:-*\
    //         |[2468]Kw[1357]K-<e[1357]K-w[1357]K>:-*"
    //     );
    //     let ranged = expand_ranges(&atomized);
    //     assert_eq!(
    //         ranged,
    //         "[2468]K\
    //         |[2468]K-<w[1357]K-e[1357]K>:-*\
    //         |[2468]Ke[1357]K-<w[1357]K-e[1357]K>:-*\
    //         |[2468]K-<e[1357]K-w[1357]K>:-*\
    //         |[2468]Kw[1357]K-<e[1357]K-w[1357]K>:-*"
    //     );
    // }
}
