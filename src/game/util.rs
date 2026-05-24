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

pub fn random_u128() -> u128 {
    let mut rng = RNG.lock().unwrap_or_else(|e| {
        panic!("Failed to lock RNG mutex for random_u128: {e}")
    });
    u128::from(rng.next_u64()) << 64 | u128::from(rng.next_u64())
}

/// Recomputes opening/endgame eval caches and current game phase.
///
/// Used after position-level changes (load, tune import, make/undo move) to
/// keep material, PST bonus, and phase classification in sync with `piece_list`
/// and PST tables.
pub fn refresh_eval_state(state: &mut State) {
    state.opening_material = [0; 2];
    state.endgame_material = [0; 2];
    state.opening_pst_bonus = [0; 2];
    state.endgame_pst_bonus = [0; 2];

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;

        for square in &state.piece_list[piece_idx] {
            state.opening_material[color] += p_ovalue!(piece) as u32;
            state.endgame_material[color] += p_evalue!(piece) as u32;

            state.opening_pst_bonus[color] +=
                state.statics.pst_opening[piece_idx][*square as usize];
            state.endgame_pst_bonus[color] +=
                state.statics.pst_endgame[piece_idx][*square as usize];
        }
    }

    state.phase_score = game_phase_score!(state);

    state.game_phase = if state.game_phase == SETUP {
        SETUP
    } else if state.phase_score > state.statics.opening_score {
        OPENING
    } else if state.phase_score < state.statics.endgame_score {
        ENDGAME
    } else {
        MIDDLEGAME
    };
}

/// Calculates the distance between two squares on the board.
pub fn square_distance(state: &State, sq1: Square, sq2: Square) -> f64 {
    let file1 = sq1 % state.statics.files as Square;
    let rank1 = sq1 / state.statics.files as Square;
    let file2 = sq2 % state.statics.files as Square;
    let rank2 = sq2 / state.statics.files as Square;

    let df = (file1 as i32 - file2 as i32).abs() as f64;
    let dr = (rank1 as i32 - rank2 as i32).abs() as f64;

    (df.powi(2) + dr.powi(2)).sqrt()
}

/// Recomputes derived state and asserts it matches the stored caches.
///
/// Debug integrity check for boards, piece lists, material counts, royal
/// lists, and the incremental Zobrist hash. On mismatch it also attempts to
/// pinpoint the source before panicking.
pub fn verify_game_state(state: &State) {

    assert_eq!(
        state.phase_score, game_phase_score!(state),
        "Game phase score doesn't match expected value based on material counts"
    );

    let mut temp_white_board = board!(
        state.statics.files, state.statics.ranks
    );
    let mut temp_black_board = board!(
        state.statics.files, state.statics.ranks
    );
    let mut temp_piece_list =
        vec![HashSet::new(); state.statics.pieces.len()];

    for (square, piece_idx) in state.main_board.iter().enumerate() {
        if *piece_idx != NO_PIECE {
            let piece = &state.statics.pieces[*piece_idx as usize];

            if p_color!(piece) == WHITE {
                set!(temp_white_board, square as u32);
            } else {
                set!(temp_black_board, square as u32);
            }

            temp_piece_list[p_index!(piece) as usize].insert(square as Square);
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
        format_game_state(state)
    );

    assert_eq!(
        &temp_black_board,
        &state.pieces_board[BLACK as usize],
        "Computed black board doesn't match state black board\n{}\n{}\n{}",
        format_board(&temp_black_board, None),
        format_board(&state.pieces_board[BLACK as usize], None),
        format_game_state(state)
    );

    let mut temp_pieces_board = board!(
        state.statics.files, state.statics.ranks
    );

    or!(temp_pieces_board, &temp_white_board);
    or!(temp_pieces_board, &temp_black_board);

    let mut temp_big_pieces = [0; 2];
    let mut temp_major_pieces = [0; 2];
    let mut temp_minor_pieces = [0; 2];
    let mut temp_opening_material = [0; 2];
    let mut temp_endgame_material = [0; 2];
    let mut temp_opening_pst_bonus = [0; 2];
    let mut temp_endgame_pst_bonus = [0; 2];

    for (i, piece) in state.statics.pieces.iter().enumerate() {
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
                state.statics.pst_opening[i][*square as usize];
            temp_endgame_pst_bonus[color] +=
                state.statics.pst_endgame[i][*square as usize];
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

    for (square, piece_index) in state.main_board.iter().enumerate() {
        if *piece_index != NO_PIECE
        && p_is_royal!(state.statics.pieces[*piece_index as usize])
        {
            let piece = &state.statics.pieces[*piece_index as usize];

            temp_royal_list[p_color!(piece) as usize]
                .push(square as Square);
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

    for piece in &state.statics.pieces {
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
                        format_square(pos_idx as Square, state),
                        state.statics.pieces[piece_idx].name,
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
    let file = fs::read_to_string(path);

    if let Err(ref e) = file {
        log_3!("Failed to read perft file at '{}': {e}", path);
        return Vec::new();
    } else {
        log_3!("Successfully read perft file at '{}'", path);
    }

    let contents = file.unwrap_or_else(|e| {
        panic!("Failed to read perft file at '{}': {e}", path)
    });
    let uncommented = COMMENT_PATTERN.replace_all(&contents, "");

    uncommented
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let mut parts = line.split(',').map(str::trim);

            let fen = parts.next().expect("Missing FEN column").to_string();
            let p1 = parts
                .next()
                .expect("Missing perft depth 1")
                .parse()
                .unwrap_or_else(|e| {
                    panic!("Invalid perft depth 1 value in line '{line}': {e}")
                });
            let p2 = parts
                .next()
                .expect("Missing perft depth 2")
                .parse()
                .unwrap_or_else(|e| {
                    panic!("Invalid perft depth 2 value in line '{line}': {e}")
                });
            let p3 = parts
                .next()
                .expect("Missing perft depth 3")
                .parse()
                .unwrap_or_else(|e| {
                    panic!("Invalid perft depth 3 value in line '{line}': {e}")
                });
            let p4 = parts
                .next()
                .expect("Missing perft depth 4")
                .parse()
                .unwrap_or_else(|e| {
                    panic!("Invalid perft depth 4 value in line '{line}': {e}")
                });
            let p5 = parts
                .next()
                .expect("Missing perft depth 5")
                .parse()
                .unwrap_or_else(|e| {
                    panic!("Invalid perft depth 5 value in line '{line}': {e}")
                });
            let p6 = parts
                .next()
                .expect("Missing perft depth 6")
                .parse()
                .unwrap_or_else(|e| {
                    panic!("Invalid perft depth 6 value in line '{line}': {e}")
                });

            (fen, p1, p2, p3, p4, p5, p6)
        })
        .collect()
}

pub fn format_time(nanos: u128) -> String {
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

/// Runs a perft test without a truth suite, reporting total nodes and elapsed
/// time for a given depth. For quick sanity checks and performance profiling
/// without needing a full suite of expected results.
pub fn benchmark_headless_perft(
    state: &mut State, depth: u8, branch: i8, dict: Option<&Translator>
) {
    log_3!(
        "Headless perft started with depth {} and branching {}...",
        depth,
        branch
    );

    let mut total_nodes = 0;
    let total_start_time = ENGINE_START.elapsed().as_nanos();

    for d in 1..=depth {
        let start_time = ENGINE_START.elapsed().as_nanos();
        let nodes = perft(state, d, branch, "", dict);
        let elapsed = ENGINE_START
            .elapsed()
            .as_nanos()
            .saturating_sub(start_time);


        log_2!(
            "Depth {} | Nodes: {:>12} | Elapsed Time: {:>12}",
            d,
            nodes,
            format_time(elapsed)
        );

        total_nodes += nodes;
    }

    let total_elapsed = ENGINE_START
        .elapsed()
        .as_nanos()
        .saturating_sub(total_start_time);

    log_1!(
        "Total moves generated: {:>12} | Elapsed Time: {:>12}",
        total_nodes,
        format_time(total_elapsed)
    );
}

/// Runs a perft suite loaded from `path` and reports pass/fail per depth.
///
/// Cases are shuffled, capped by `limit`, and each position is tested from
/// depth 1 up to `depth`. `branch` controls diagnostic line printing when
/// passed through to `perft`.
pub fn benchmark_perft(
    state: &mut State,
    path: &str,
    depth: u8,
    branch: i8,
    limit: usize,
    dict: Option<&Translator>,
) {
    let mut perft_cases = parse_perft_file(path);

    if perft_cases.is_empty() {
        return;
    }

    let limit = limit.min(perft_cases.len());
    perft_cases.shuffle(&mut RNG.lock().unwrap_or_else(|e| {
        panic!("Failed to lock RNG mutex for perft shuffle: {e}")
    }));

    log_3!(
        "Perft testing {} positions with depth {} and branching {}...",
        limit, depth, branch
    );

    let mut successful_cases = 0;
    let mut total_moves = 0;
    let total_cases = limit * depth as usize;

    let longest_fen: usize = perft_cases
        .iter()
        .max_by_key(|(fen, _, _, _, _, _, _)| fen.len())
        .unwrap_or_else(|| {
            panic!("Perft benchmark requires at least one test case")
        })
        .0
        .len();

    for (i, (fen, perft_1, perft_2, perft_3, perft_4, perft_5, perft_6)) in
        perft_cases.into_iter().take(limit).enumerate()
    {

        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            log_3!("SIGINT | Aborting perft benchmark at case {}", i);
            break;
        }

        state.load_fen(&fen, None);

        let expected_perfts =
            [perft_1, perft_2, perft_3, perft_4, perft_5, perft_6];

        for d in 1..=depth {
            let start_time = ENGINE_START.elapsed().as_nanos();
            let result = perft(state, d, branch, "", dict);
            let elapsed = ENGINE_START
                .elapsed()
                .as_nanos()
                .saturating_sub(start_time);

            let expected = expected_perfts[(d - 1) as usize];

            if result == expected {
                successful_cases += 1;
                total_moves += result;
                log_5!(
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
                log_5!(
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

    log_1!(
        "Perft testing completed: {}/{} cases passed.",
        successful_cases, total_cases
    );
    log_1!("Total moves generated: {}", total_moves);
}

/// Runs a fixed-depth search benchmark from the current position.
///
/// Reports the current position, executes search, and logs total nodes, elapsed
/// wall time, and aggregate nodes-per-second.
pub fn benchmark_search(
    state: &mut State, ttable: Arc<TTable>, qtable: Arc<QTable>, depth: usize,
    thread_num: usize, dict: Option<&Translator>,
) {
    log_3!("Search benchmark started with depth {}...", depth);

    let mut info = SearchInfo { set_depth: depth, ..Default::default() };
    let mut bufs = SearchBufs::default();

    search_position(
        state, ttable, qtable,
        &mut info, &mut bufs, thread_num, dict, PROTOCOL_TUI,
    );
}

/// Counts legal move tree nodes from the current state up to `depth`.
///
/// When `branch >= 0`, prints the explored move prefixes for the first levels
/// to help inspect branching behavior.
pub fn perft(
    state: &mut State, depth: u8, branch: i8, prefix: &str,
    dict: Option<&Translator>,
) -> u64 {

    if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
        log_3!(
            "SIGINT | Aborting perft at depth {} with prefix '{}'",
            depth, prefix
        );
        return 0;
    }

    if depth == 0 {
        if branch >= 0 {
            log_5!("{} moves | Nodes: 1", prefix);
        }
        return 1;
    }

    let mut possible_moves = Vec::with_capacity(64);
    let mut scratch = Vec::with_capacity(16);
    generate_all_moves_and_drops(state, &mut possible_moves, &mut scratch);
    let mut nodes = 0;

    if branch < 0 {
        for mv in possible_moves {
            if make_move!(state, mv) {
                nodes += perft(state, depth - 1, branch - 1, "", dict);
                undo_move!(state);
            }
        }

        return nodes;
    }

    for mv in possible_moves {
        let formatted_move = format_move(&mv, state, dict);
        let new_prefix = format!("{}{}", prefix, formatted_move);

        if make_move!(state, mv) {
            nodes += perft(state, depth - 1, branch - 1, &new_prefix, dict);
            undo_move!(state);
        }
    }

    log_5!("{} moves | Nodes: {}", prefix, nodes);

    nodes
}

