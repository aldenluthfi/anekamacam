use std::fs;
use rand::seq::SliceRandom;

#[cfg(debug_assertions)]
use crate::game::util::verify_game_state;
use crate::{
    captured_piece, captured_square, captured_unmoved, castling, clear,
    constants::{
        BK_CASTLE, BQ_CASTLE, DROP_MOVE, MULTI_CAPTURE_MOVE, NO_EN_PASSANT,
        NO_PIECE, QUIET_MOVE, SINGLE_CAPTURE_MOVE, WK_CASTLE, WQ_CASTLE,
    },
    created_enp, creates_enp, demote_upon_capture, drops,
    end, enp_square, drop_from_enemy_hand,
    game::{
        hash::zobrist::{
            CASTLING_HASHES, EN_PASSANT_HASHES, IN_HAND_HASHES,
            PIECE_HASHES, SIDE_HASHES,
        },
        moves::move_list::{
            generate_all_moves_and_drops, validate_attack_vector
        },
        representations::state::{
            EnPassantSquare, Snapshot, Square, State,
        },
        util::RNG,
    },
    get, hash_in_or_out_piece, hash_toggle_side, hash_update_castling,
    hash_update_en_passant, hash_update_in_hand, io::{
        game_io::{COMMENT_PATTERN, format_game_state},
        move_io::format_move,
    },
    is_initial, is_unload, make_move, move_type, multi_move_captured_piece,
    multi_move_captured_square, multi_move_captured_unmoved,
    multi_move_is_unload, multi_move_unload_square, p_can_promote,
    p_castle_left, p_castle_right, p_color, p_is_big, p_is_major,
    p_is_minor, p_is_royal, p_value, piece, promote_to_captured, promoted,
    promotion, set, start, undo_move, unload_square, is_null,
    null_snapshot, is_in_check, is_square_attacked
};

fn parse_perft_file(
    path: &str
) -> Vec<(String, u64, u64, u64, u64, u64, u64)> {                              /* until perft 6                      */
    let contents = fs::read_to_string(path).expect("Failed to read perft file");
    let uncommented = COMMENT_PATTERN.replace_all(&contents, "");
    uncommented
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
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
        })
        .collect()
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
    branch: i8,
    limit: usize
) {
    let mut perft_cases = parse_perft_file(path);
    let limit = limit.min(perft_cases.len());
    perft_cases.shuffle(&mut RNG.lock().unwrap());

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
        perft_cases.into_iter().take(limit).enumerate()
    {
        state.load_fen(&fen);
        println!("\n{}", format_game_state(state, true));

        let expected_perfts = [
            perft_1, perft_2, perft_3, perft_4, perft_5, perft_6
        ];

        for d in 1..=depth {
            let start_time = std::time::Instant::now();
            let result = perft(state, d, branch, "");
            let elapsed = start_time.elapsed().as_nanos();

            let expected = expected_perfts[(d - 1) as usize];

            if result == expected {
                successful_cases += 1;
                total_moves += result;
                println!(
                    "{:04}. FEN: {:<width$} | Depth: {} | Expected: {:>12} | \
                    Result: {:>12} | Time: {:>12} [PASSED]",
                    i, fen, d, expected, result, format_time(elapsed),
                    width = longest_fen
                );
            } else {
                println!(
                    "{:04}. FEN: {:<width$} | Depth: {} | Expected: {:>12} | \
                    Result: {:>12} | Time: {:>12} [FAILED]",
                    i, fen, d, expected, result, format_time(elapsed),
                    width = longest_fen
                );
            }
        }
    }

    println!(
        "Perft testing completed: {}/{} cases passed.",
        successful_cases,
        total_cases
    );
    println!("Total moves generated: {}", total_moves);
}

pub fn perft(
    state: &mut State,
    depth: u8,
    branch: i8,
    prefix: &str,
) -> u64 {
    if depth == 0 {
        if branch >= 0 {
            println!("{}moves | Nodes: 1", prefix);
        }
        return 1;
    }

    let possible_moves = generate_all_moves_and_drops(state);
    let mut nodes = 0;

    for mv in possible_moves {
        if branch >= 0 {
            let formatted_move = format_move(&mv, state);
            let new_prefix = format!("{}{}", prefix, formatted_move);

            if make_move!(state, mv) {
                nodes += perft(state, depth - 1, branch - 1, &new_prefix);
                undo_move!(state);
            }
        } else if make_move!(state, mv) {
            nodes += perft(state, depth - 1, branch - 1, "");
            undo_move!(state);
        }
    }

    if branch >= 0 {
        println!("{}moves | Nodes: {}", prefix, nodes);
    }

    nodes
}
