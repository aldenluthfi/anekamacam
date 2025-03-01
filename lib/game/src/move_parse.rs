use std::char;

use regex::Regex;

fn evaluate(expr: &str) -> String {
    let mut operands: Vec<String> = Vec::new();
    let mut operators: Vec<char> = Vec::new();
    let mut i = 0;

    while i < expr.len() {
        let c = expr.chars().nth(i).unwrap();

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
                while i < expr.len() && !"^|()".contains(expr.chars().nth(i).unwrap()) {
                    /* Parse operand                      */
                    operand.push(expr.chars().nth(i).unwrap());
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

    operands.pop().unwrap() /* Final result is the only operand   */
}

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

fn precedence(op: char) -> usize {
    match op {
        '^' => 2,
        '|' => 1,
        _ => unreachable!("Invalid operator: {}", op),
    }
}

fn expand(expr: &str) -> String {
    let pattern = Regex::new(r"[^(^|]\(|\)[^()^|]").unwrap();
    let indices: Vec<usize> = pattern
        .find_iter(expr)
        .map(|m| (m.end() + m.start()) / 2)
        .collect();

    let mut parts = Vec::new();
    let mut prev = 0;
    for &idx in &indices {
        parts.push(&expr[prev..idx]);
        prev = idx;                                                             /* Split expr at indices              */
    }
    parts.push(&expr[prev..]);
    let processed_expr = parts.join("^");                                       /* Join parts with '^'                */

    evaluate(&processed_expr)                                                   /* Eval the processed expr            */
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

fn atomize(expr: &str) -> String {
    let mut atoms = Vec::new();
    for c in expr.chars() {
        atoms.push(betza_atoms(c));
    }
    atoms.join("")
}

fn expand_ranges(expr: &str) -> String {
    let mut expanded = expr.to_string();

    let pattern = Regex::new(
        r"(-?)(?:\{(?:\.\.(\d+)|(\d+)\.\.|\.\.)\}|\*)"
    ).unwrap();
    while let Some(cap) = pattern.captures(&expanded) {
        let prefix = cap.get(1).map_or("", |m| m.as_str());
        let replacement = match (cap.get(2), cap.get(3)) {
            (Some(end), _) => {
                let end_str = end.as_str();
                format!("{}{{1..{}}}", prefix, end_str)
            }
            (_, Some(start)) => {
                let start_str = start.as_str();
                format!("{}{{{}..64}}", prefix, start_str)
            }
            _ => format!("{}{{1..64}}", prefix),
        };
        let cap_str = cap.get(0).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    let pattern = Regex::new(
        r"([^:-]+)(:?)(-?)(?:\{([0-9]+)(?:\.\.([0-9]+))?\})(.*)"
    ).unwrap();
    while let Some(cap) = pattern.captures(&expanded) {
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
                        format!("{}-", prefix).repeat(n - 1) + &prefix
                    } else {
                        format!("{}-", prefix).repeat(n) + suffix
                    }
                }
                (true, false) => {
                    let pattern = Regex::new(r"([^>])$|(<.*>)$").unwrap();
                    let capture = pattern.captures(prefix).unwrap();
                    let piece = capture
                        .get(1)
                        .or(capture.get(2))
                        .unwrap()
                        .as_str();

                    prefix.replacen(piece, &piece.repeat(n), 1) + suffix
                }
                (false, true) => {
                    prefix.to_owned() + &"-.".repeat(n - 1) + &suffix
                }
                _ => format!("{}{}{}", prefix, ".".repeat(n - 1), suffix),
            });
        }

        expanded = expanded.replacen(&cap[0], &vec_rep.join("|"), 1);
    }


    expanded
}

fn expand_directions(expr: &str) -> String {
    let mut expanded = expr.to_string();

    let pattern = Regex::new(
        r"(?:\[(?:\.\.(\d+)|(\d+)\.\.|(\d+)\.\.(\d+)|\.\.)\]|\*)"
    ).unwrap();
    while let Some(cap) = pattern.captures(&expanded) {
        let range = match (cap.get(1), cap.get(2), cap.get(3), cap.get(4)) {
            (Some(end), _, _, _) => {
                let e = end.as_str().parse().unwrap();
                1..=e
            },
            (_, Some(start), _, _) => {
                let s = start.as_str().parse().unwrap();
                s..=8
            },
            (_, _, Some(start), Some(end)) => {
                let s = start.as_str().parse::<usize>().unwrap();
                let e = end.as_str().parse().unwrap();
                s..=e
            },
            _ => 1..=8,
        };

        let range_str = range.map(|n| n.to_string()).collect::<String>();
        let replacement = format!("[{}]", range_str);
        let cap_str = cap.get(0).unwrap().as_str();
        expanded = expanded.replacen(cap_str, &replacement, 1);
    }

    println!("{}", expanded);

    let mut stack = vec![expanded];
    let mut result_stack: Vec<String> = Vec::new();

    let pattern = Regex::new(
        r"\[([0-9]{2,})\]"
    ).unwrap();

    while !stack.is_empty() {
        let term = stack.pop().unwrap();

        if !pattern.is_match(&term) {
            result_stack.push(term);
            continue;
        }

        let cap = pattern.captures(&term).unwrap();
        let range = cap.get(1).unwrap().as_str();

        for char in range.chars() {
            let n = format!("[{}]", char);
            stack.push(term.replacen(&format!("[{}]", range), &n, 1));
        }
    }

    result_stack.join("|")
}

#[cfg(test)]
mod tests {
    use super::*;
    use ntest::timeout;

    #[test]
    fn expansion() {
        assert_eq!("abd|abef|cd|cef", expand("(ab|c)(d|ef)"));
        assert_eq!("ace|acf|ade|adf|bce|bcf|bde|bdf", expand("(a|b)(c|d)(e|f)"));
    }

    #[test]
    #[timeout(3000)]
    fn range_expansion() {
        println!("{}", expand_directions("[2468]Kn[2468]K"));
        assert_eq!(1, 2)
    }

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
