use std::fs;

use crate::{
    game::{
        moves::move_list::{generate_all_moves, make_move, undo_move},
        representations::state::State
    },
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

pub fn start_perft(state: &mut State, path: &str, depth: u8) {
    let perft_cases = parse_perft_file(path);

    let mut successful_cases = 0;
    let mut total_moves = 0;
    let total_cases = perft_cases.len() * depth as usize;

    for (
        fen, perft_1, perft_2, perft_3, perft_4, perft_5, perft_6
    ) in perft_cases {
        state.load_fen(&fen);

        let expected_perfts = [
            perft_1, perft_2, perft_3, perft_4, perft_5, perft_6
        ];

        for d in 1..=depth {

            let result = perft(state, d);
            let expected = expected_perfts[(d - 1) as usize];

            if result == expected {
                successful_cases += 1;
                total_moves += result;
                println!(
                    "FEN: {} | Depth: {} | Expected: {} | Result: {} [PASSED]",
                    fen, d, expected, result
                );
            } else {
                println!(
                    "FEN: {} | Depth: {} | Expected: {} | Result: {} [FAILED]",
                    fen, d, expected, result
                );
            }
        }
    }

    println!(
        "Perft testing completed: {}/{} cases passed.",
        successful_cases, total_cases
    );
    println!("Total moves generated: {}", total_moves);
}

#[hotpath::measure]
fn perft(state: &mut State, depth: u8) -> u64 {

    if depth == 0 {
        return 1;
    }

    let possible_moves = generate_all_moves(state);

    let mut nodes = 0;

    for mv in possible_moves {
        if make_move(state, mv) {
            nodes += perft(state, depth - 1);
            undo_move(state);
        }
    }

    nodes
}