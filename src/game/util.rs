//! # util.rs
//!
//! Provides utility functions and patterns for move expression processing.
//!
//! This file contains shared utilities used across the move parsing and
//! matching modules, including regex patterns for normalization, range
//! expansion, and cardinal direction parsing. It also provides the core
//! expression evaluation logic using a stack-based algorithm for handling
//! operators (^ for concatenation, | for alternation), Betza atom mappings,
//! and parallel processing helpers for splitting and processing expressions.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2025

use crate::*;

lazy_static! {
    pub static ref RNG: Mutex<rand::rngs::StdRng> =
        { Mutex::new(rand::rngs::StdRng::seed_from_u64(RNG_SEED)) };
}

pub fn random_u128() -> u128 {
    let mut rng = RNG.lock().unwrap();
    u128::from(rng.next_u64()) << 64 | u128::from(rng.next_u64())
}

/// Recomputes opening/endgame eval caches and current game phase.
///
/// This function is used after position-level changes (load, tune import,
/// make/undo move) to keep material, pst bonus, and phase classification in
/// sync with `piece_list` and PST tables.
pub fn refresh_eval_state(state: &mut State) {
    state.opening_material = [0; 2];
    state.endgame_material = [0; 2];
    state.opening_pst_bonus = [0; 2];
    state.endgame_pst_bonus = [0; 2];

    for (piece_idx, piece) in state.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;

        for square in &state.piece_list[piece_idx] {
            state.opening_material[color] += p_ovalue!(piece) as u32;
            state.endgame_material[color] += p_evalue!(piece) as u32;

            state.opening_pst_bonus[color] +=
                state.pst_opening[piece_idx][*square as usize];
            state.endgame_pst_bonus[color] +=
                state.pst_endgame[piece_idx][*square as usize];
        }
    }

    let game_phase_score = state.opening_material[WHITE as usize]
        + state.opening_material[BLACK as usize];

    state.game_phase = if game_phase_score > state.opening_phase_score {
        OPENING
    } else if game_phase_score < state.endgame_phase_score {
        ENDGAME
    } else {
        MIDDLEGAME
    };
}

/// Recomputes derived state and asserts it matches the stored caches.
///
/// This is a debug integrity check for boards, piece lists, material counts,
/// royal lists, and the incremental Zobrist hash. On hash mismatch it also
/// attempts to pinpoint likely sources before panicking.
pub fn verify_game_state(state: &State) {
    let mut temp_white_board = board!(state.files, state.ranks);
    let mut temp_black_board = board!(state.files, state.ranks);
    let mut temp_piece_list = vec![HashSet::new(); state.pieces.len()];

    for (index, square) in state.main_board.iter().enumerate() {
        if *square != NO_PIECE {
            let piece = &state.pieces[*square as usize];

            if p_color!(piece) == WHITE {
                set!(temp_white_board, index as u32);
            } else {
                set!(temp_black_board, index as u32);
            }

            temp_piece_list[p_index!(piece) as usize].insert(index as u16);
        }
    }

    assert_eq!(
        &temp_piece_list, &state.piece_list,
        "Computed piece list doesn't match state piece list",
    );

    assert_eq!(
        &temp_white_board,
        &state.pieces_board[WHITE as usize],
        "Computed white board doesn't match state white board\n{}\n{}\n{}",
        format_board(&temp_white_board, None),
        format_board(&state.pieces_board[WHITE as usize], None),
        format_game_state(state, false)
    );

    assert_eq!(
        &temp_black_board,
        &state.pieces_board[BLACK as usize],
        "Computed black board doesn't match state black board\n{}\n{}\n{}",
        format_board(&temp_black_board, None),
        format_board(&state.pieces_board[BLACK as usize], None),
        format_game_state(state, false)
    );

    let mut temp_pieces_board = board!(state.files, state.ranks);

    or!(temp_pieces_board, &temp_white_board);
    or!(temp_pieces_board, &temp_black_board);

    let mut temp_big_pieces = [0; 2];
    let mut temp_major_pieces = [0; 2];
    let mut temp_minor_pieces = [0; 2];
    let mut temp_opening_material = [0; 2];
    let mut temp_endgame_material = [0; 2];
    let mut temp_opening_pst_bonus = [0; 2];
    let mut temp_endgame_pst_bonus = [0; 2];

    for (i, piece) in state.pieces.iter().enumerate() {
        let piece_indices = &state.piece_list[i];

        for square in piece_indices {
            let color = p_color!(piece) as usize;
            if p_is_big!(piece) {
                temp_big_pieces[color] += 1;
            }
            if p_is_major!(piece) {
                temp_major_pieces[color] += 1;
            }
            if p_is_minor!(piece) {
                temp_minor_pieces[color] += 1;
            }

            temp_opening_material[color] += p_ovalue!(piece) as u32;
            temp_endgame_material[color] += p_evalue!(piece) as u32;
            temp_opening_pst_bonus[color] +=
                state.pst_opening[i][*square as usize];
            temp_endgame_pst_bonus[color] +=
                state.pst_endgame[i][*square as usize];
        }
    }

    assert_eq!(
        temp_big_pieces, state.big_pieces,
        "Computed big pieces count doesn't match state big pieces count"
    );

    assert_eq!(
        temp_major_pieces, state.major_pieces,
        "Computed major pieces count doesn't match state major pieces count"
    );

    assert_eq!(
        temp_minor_pieces, state.minor_pieces,
        "Computed minor pieces count doesn't match state minor pieces count"
    );

    assert_eq!(
        temp_opening_material, state.opening_material,
        concat!(
            "Computed opening material count doesn't match state ",
            "opening material count"
        )
    );

    assert_eq!(
        temp_endgame_material, state.endgame_material,
        concat!(
            "Computed endgame material count doesn't match state ",
            "endgame material count"
        )
    );

    assert_eq!(
        temp_opening_pst_bonus, state.opening_pst_bonus,
        concat!(
            "Computed opening pst bonus doesn't match state ",
            "opening pst bonus"
        )
    );

    assert_eq!(
        temp_endgame_pst_bonus, state.endgame_pst_bonus,
        concat!(
            "Computed endgame pst bonus doesn't match state ",
            "endgame pst bonus"
        )
    );

    let mut temp_royal_list = [Vec::new(), Vec::new()];

    for (index, square) in state.main_board.iter().enumerate() {
        if *square != NO_PIECE && p_is_royal!(state.pieces[*square as usize]) {
            let piece = &state.pieces[*square as usize];

            temp_royal_list[p_color!(piece) as usize].push(index as u16);
        }
    }

    assert_eq!(
        &temp_royal_list, &state.royal_list,
        "Computed royal list doesn't match state royal list"
    );

    let mut temp_hash = u128::default();

    if state.playing == WHITE {
        temp_hash ^= &*SIDE_HASHES;
    }

    temp_hash ^= &CASTLING_HASHES[state.castling_state as usize];

    if state.en_passant_square != NO_EN_PASSANT {
        temp_hash ^=
            &EN_PASSANT_HASHES[enp_square!(state.en_passant_square) as usize];
    }

    for piece in &state.pieces {
        let i = p_index!(piece) as usize;

        let piece_indices = &state.piece_list[i];

        for index in piece_indices {
            temp_hash ^= PIECE_HASHES[i][*index as usize];
        }
    }

    for color in [WHITE, BLACK] {
        for (index, &count) in
            state.piece_in_hand[color as usize].iter().enumerate()
        {
            temp_hash ^= &IN_HAND_HASHES[index][count as usize];
        }
    }

    if temp_hash != state.position_hash {
        let missing_hash = temp_hash ^ state.position_hash;

        for (piece_idx, positions) in PIECE_HASHES.iter().enumerate() {
            for (pos_idx, &hash) in positions.iter().enumerate() {
                if hash == missing_hash {
                    panic!(
                        concat!(
                            "Hash mismatch! Missing/extra piece at ",
                            "position {} for piece {}"
                        ),
                        format_square(pos_idx as u16, state),
                        state.pieces[piece_idx].name,
                    );
                }
            }
        }

        for (idx, &hash) in CASTLING_HASHES.iter().enumerate() {
            if hash == missing_hash {
                panic!(
                    "Hash mismatch! Castling state mismatch at index {}",
                    idx
                );
            }
        }

        for (idx, &hash) in EN_PASSANT_HASHES.iter().enumerate() {
            if hash == missing_hash {
                panic!(
                    "Hash mismatch! En passant square mismatch at index {}",
                    idx
                );
            }
        }

        if missing_hash == *SIDE_HASHES {
            panic!("Hash mismatch! Side to move mismatch");
        }

        panic!("Hash mismatch! Could not find source of difference");
    }

    assert_eq!(
        temp_hash, state.position_hash,
        "Computed hash doesn't match state position hash"
    );
}

fn parse_perft_file(path: &str) -> Vec<(String, u64, u64, u64, u64, u64, u64)> {/* until perft 6                      */
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

/// Runs a perft suite loaded from `path` and reports pass/fail per depth.
///
/// Cases are shuffled, capped by `limit`, and each position is tested from
/// depth 1 up to `depth`. `branch` controls diagnostic line printing when
/// passed through to `perft`.
pub fn start_perft(
    state: &mut State,
    path: &str,
    depth: u8,
    branch: i8,
    limit: usize,
) {
    let mut perft_cases = parse_perft_file(path);
    let limit = limit.min(perft_cases.len());
    perft_cases.shuffle(&mut RNG.lock().unwrap());

    println!(
        "Perft testing {} positions with depth {} and branching {}...",
        limit, depth, branch
    );

    let mut successful_cases = 0;
    let mut total_moves = 0;
    let total_cases = limit * depth as usize;

    let longest_fen: usize = perft_cases
        .iter()
        .max_by_key(|(fen, _, _, _, _, _, _)| fen.len())
        .unwrap()
        .0
        .len();

    for (i, (fen, perft_1, perft_2, perft_3, perft_4, perft_5, perft_6)) in
        perft_cases.into_iter().take(limit).enumerate()
    {
        state.load_fen(&fen);
        println!("\n{}", format_game_state(state, true));

        let expected_perfts =
            [perft_1, perft_2, perft_3, perft_4, perft_5, perft_6];

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
                    i,
                    fen,
                    d,
                    expected,
                    result,
                    format_time(elapsed),
                    width = longest_fen
                );
            } else {
                println!(
                    "{:04}. FEN: {:<width$} | Depth: {} | Expected: {:>12} | \
                    Result: {:>12} | Time: {:>12} [FAILED]",
                    i,
                    fen,
                    d,
                    expected,
                    result,
                    format_time(elapsed),
                    width = longest_fen
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

/// Counts legal move tree nodes from the current state up to `depth`.
///
/// When `branch >= 0`, this also prints the explored move prefixes for the
/// first levels to help inspect branching behavior.
pub fn perft(state: &mut State, depth: u8, branch: i8, prefix: &str) -> u64 {
    if depth == 0 {
        if branch >= 0 {
            println!("{} moves | Nodes: 1", prefix);
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
        println!("{} moves | Nodes: {}", prefix, nodes);
    }

    nodes
}
