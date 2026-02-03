use std::fs;

use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    constants::WHITE,
    game::{
        moves::move_list::generate_move_list,
        representations::{
            moves::MoveType,
            state::State
        }
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

pub fn start_perft(state: &mut State, path: &str, depth: u8) {
    let perft_cases = parse_perft_file(path);

    let mut successful_cases = 0;
    let total_cases = perft_cases.len();

    for (
        fen, perft_1, perft_2, perft_3, perft_4, perft_5, perft_6
    ) in perft_cases {
        state.load_fen(&fen);

        let expected_perfts = [
            perft_1, perft_2, perft_3, perft_4, perft_5, perft_6
        ];

        for d in 1..=depth {
            let result = perft(&state, d);

            let expected = expected_perfts[(d - 1) as usize];

            if result == expected {
                successful_cases += 1;
            } else {
                #[cfg(debug_assertions)]
                println!(
                    concat!(
                        "[FAIL] Perft test failed for FEN {} \n",
                        "at depth {}: expected {}, got {}"
                    ),
                    fen, d, expected, result
                );
            }
        }
    }

    println!(
        "Perft testing completed: {}/{} cases passed.",
        successful_cases, total_cases
    );
}

pub fn generate_all_moves(state: &State) -> Vec<MoveType> {
    let mut possible_moves: Vec<MoveType> = vec![];

    let piece_count = state.pieces.len() / 2;
    let start_index = if state.playing == WHITE { 0 } else { piece_count };
    let end_index = start_index + piece_count;

        possible_moves = (start_index..end_index)
            .into_par_iter()
            .flat_map(|piece_index| {
                let piece = &state.pieces[piece_index];
                let piece_board = &state.pieces_board[piece_index];
                let piece_indices = piece_board.bit_indices();

                piece_indices.into_iter().flat_map(move |index| {
                    let relevant_friendly_board = if piece.color() == WHITE {
                        &state.white_board
                    } else {
                        &state.black_board
                    };

                    let relevant_enemy_board = if piece.color() == WHITE {
                        &state.black_board
                    } else {
                        &state.white_board
                    };

                    generate_move_list(
                        index, &piece, &relevant_friendly_board,
                        &relevant_enemy_board, &state.unmoved_board, &state
                    )
                }).collect::<Vec<_>>()
            })
            .collect();

        possible_moves
}

#[hotpath::measure]
pub fn perft(state: &State, depth: u8) -> u64 {

    if depth == 1 {
        return generate_all_moves(state).len() as u64
    }

    unimplemented!()
}