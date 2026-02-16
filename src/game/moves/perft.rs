use std::fs;

use crate::{
    constants::{MULTI_CAPTURE_MOVE, SINGLE_CAPTURE_MOVE}, game::{
        moves::move_list::{generate_all_moves, is_in_check, make_move, undo_move},
        representations::state::State
    }, io::{game_io::format_game_state, move_io::format_move}, move_type
};

fn parse_perft_file(
    path: &str
) -> Vec<(String, u64, u64, u64, u64, u64, u64)> {                              /* until perft 6                      */
    let contents = fs::read_to_string(path).expect("Failed to read perft file");
    contents.lines().map(|line| {
        let parts: Vec<&str> = line.split(",").collect();
        (
            parts[0].to_string(),
            parts[1].parse().unwrap(),
            parts[2].parse().unwrap(),
            parts[3].parse().unwrap(),
            parts[4].parse().unwrap(),
            parts[5].parse().unwrap(),
            parts[6].parse().unwrap(),
        )
    }).collect()
}

fn format_time(nanos: u128) -> String {
    if nanos < 1_000 {
        format!("{} ns", nanos)
    } else if nanos < 1_000_000 {
        format!("{:.3} µs", nanos as f64 / 1_000.0)
    } else if nanos < 1_000_000_000 {
        format!("{:.3} ms", nanos as f64 / 1_000_000.0)
    } else {
        format!("{:.3}  s", nanos as f64 / 1_000_000_000.0)
    }
}

pub fn start_perft(
    state: &mut State,
    path: &str,
    depth: u8,
    debug: bool,
    branch: Option<u8>,
) {
    let perft_cases = parse_perft_file(path);

    let mut successful_cases = 0;
    let mut total_moves = 0;
    let total_cases = perft_cases.len() * depth as usize;

    let longest_fen: usize = perft_cases
        .iter()
        .max_by_key(
            |(fen, _, _, _, _, _, _)|
            fen.len()
        )
        .unwrap()
        .0
        .len();

    for (i, (fen, perft_1, perft_2, perft_3, perft_4, perft_5, perft_6)) in
        perft_cases.iter().enumerate()
    {
        state.load_fen(fen);
        println!("{}", format_game_state(state, false));

        let expected_perfts = [
            perft_1, perft_2, perft_3, perft_4, perft_5, perft_6,
        ];

        for d in 1..=depth {
            let start_time = std::time::Instant::now();
            let result = perft(state, d, debug, branch);
            let elapsed = start_time.elapsed().as_nanos();

            let expected = expected_perfts[(d - 1) as usize];
            if result.0 == *expected {
                successful_cases += 1;
                total_moves += result.0;
                println!(
                    "{:04}. FEN: {:<width$} | Depth: {} | Expected: {:>12} | \
                    Result: {:>12} | Time      : {:>12} [PASSED]",
                    i, fen, d, expected, result.0, format_time(elapsed),
                    width = longest_fen
                );
            } else {
                println!(
                    "{:04}. FEN: {:<width$} | Depth: {} | Expected: {:>12} | \
                    Result: {:>12} | Time      : {:>12} [FAILED]",
                    i, fen, d, expected, result.0, format_time(elapsed),
                    width = longest_fen
                );
            }
            println!(
                "           {:<width$}              \
                Captures: {:>12} | Checks: {:>12} | Checkmates: {:>9}",
                "", result.1, result.2, result.3, width = longest_fen
            );
        }
    }

    println!(
        "Perft testing completed: {}/{} cases passed.",
        successful_cases,
        total_cases
    );
    println!("Total moves generated: {}", total_moves);
}

fn perft(
    state: &mut State,
    depth: u8,
    debug: bool,
    branch: Option<u8>,
) -> (u64, u64, u64, u64) {
    perft_impl(state, depth, debug, branch, String::new())
}

// returns the tuple of (nodes, captures, checks, checkmates)
fn perft_impl(
    state: &mut State,
    depth: u8,
    debug: bool,
    branch: Option<u8>,
    prefix: String,
) -> (u64, u64, u64, u64) {

    if depth == 0 {
        if debug {
            println!("{} Reached leaf node", prefix);
        }

        let is_check = is_in_check(state.playing, state);
        return (1, 0, is_check as u64, 0);
    }

    let possible_moves = generate_all_moves(state);
    let mut nodes = 0;
    let mut captures = 0;
    let mut checks = 0;
    let mut checkmates = 0;

    for mv in possible_moves {
        let move_type = move_type!(mv);

        let mut is_capture = false;
        if depth == 1 {
            is_capture =
                move_type == SINGLE_CAPTURE_MOVE ||
                move_type == MULTI_CAPTURE_MOVE;
        }

        if debug {
            let formatted_move = &format_move(&mv, state);

            if make_move(state, mv) {
                let (n, c, ch, cm) = perft_impl(
                    state,
                    depth - 1,
                    debug && depth > branch.unwrap_or(0),
                    branch,
                    format!("{} {}", prefix, formatted_move),
                );
                nodes += n;
                captures += c + is_capture as u64;
                checks += ch;
                checkmates += cm;
                undo_move(state);
            }
        } else if make_move(state, mv) {
            let (n, c, ch, cm) = perft_impl(
                state,
                depth - 1,
                false,
                None,
                String::new(),
            );
            nodes += n;
            captures += c + is_capture as u64;
            checks += ch;
            checkmates += cm;
            undo_move(state);
        }
    }

    if debug {
        println!("{} moves | Nodes: {}", prefix, nodes);
    }

    if depth == 1 && nodes == 0 && is_in_check(state.playing, state){
        checkmates += 1;
    }

    (nodes, captures, checks, checkmates)
}
