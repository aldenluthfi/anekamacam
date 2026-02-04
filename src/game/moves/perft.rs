use std::fs;

use crate::{
    game::{
        moves::move_list::{make_move, undo_move, generate_all_moves},
        representations::state::State
    },
    io::{
        game_io::format_game_state,
        move_io::format_move
    }
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

pub fn start_perft(
    mut state: &mut State, path: &str,
    depth: u8, debug: bool, branch: Option<u8>
) {
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
            let result = perft(&mut state, d, debug, branch, Some(depth), None);
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
                #[cfg(debug_assertions)]
                println!("{}", format_game_state(state, false))
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
fn perft(
    mut state: &mut State, depth: u8, debug: bool,
    branch: Option<u8>, max_depth: Option<u8>, prefix: Option<String>
) -> u64 {

    if depth == 0 {
        return 1;
    }

    let possible_moves = generate_all_moves(state);

    let mut nodes = 0;

    if !debug {
        for mv in possible_moves {
            if make_move(&mut state, mv) {
                nodes += perft(state, depth - 1, false, None, None, None);
                undo_move(&mut state);
            }
        }
        return nodes;
    } else {
        for mv in possible_moves {
            if make_move(&mut state, mv.clone()) {
                nodes += perft(
                    state,
                    depth - 1,
                    (max_depth.expect(
                        "Max depth must be provided for debug mode"
                    ) - depth) <= branch.expect(
                        "Branching must be provided for debug mode"
                    ),
                    branch,
                    max_depth,
                    Some(
                        format!(
                            "     {} {} ",
                            prefix.as_deref().unwrap_or(""),
                            format_move(&mv, state)
                        )
                    )
                );
                undo_move(&mut state);
            }
        }


        println!(
            "{}moves | Nodes: {}",
            prefix.as_deref().unwrap_or(""),
            nodes
        );
    }

    nodes
}