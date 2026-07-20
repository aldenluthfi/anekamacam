//! util.rs
//!
//! Cross-cutting engine utilities that belong to no single subsystem.
//!
//! The file groups four concerns: randomness for Zobrist seeding, state
//! integrity helpers (recomputing cached evaluation terms and asserting
//! that incrementally maintained caches still match a from-scratch
//! recount), the perft/benchmark harness used to validate move generation
//! against known node counts and to profile search speed, and rolling a
//! `latest.*` output file to a numbered backup.
//!
//! Created: 25/01/2025
//! Author : Alden Luthfi

use crate::*;

/// exe_tag
///
/// Short identity of the running binary, taken from the invoking path
/// (argv[0]) reduced to its file name. Used to label search threads so a
/// panic that names a thread also names which engine binary raised it —
/// essential when the SPRT runner drives two different builds through one
/// terminal and both would otherwise spawn a thread called `search`.
///
/// Return:
/// String -> the executable's file name, or "?" if it cannot be read
pub fn exe_tag() -> String {
    env::args()
        .next()
        .as_deref()
        .and_then(|arg| Path::new(arg).file_name())
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| "?".to_string())
}

/// random_u128
///
/// Draws a full-width random number from the engine's shared seeded RNG
/// by concatenating two 64-bit draws. Used to seed the Zobrist hash
/// tables at startup.
///
/// Return:
/// u128 -> uniformly random 128-bit value
pub fn random_u128() -> u128 {
    let mut rng = RNG.lock().unwrap_or_else(|e| {
        panic!("Failed to lock RNG mutex for random_u128: {e}")
    });
    u128::from(rng.next_u64()) << 64 | u128::from(rng.next_u64())
}

/// roll_latest
///
/// Rolls a directory's `latest.{extension}` file to the next free
/// numbered backup, so a fresh export or dataset run never overwrites or
/// appends to the previous one. Scans the directory for existing
/// `{N}.{extension}` files, renames `latest.{extension}` to
/// `{N + 1}.{extension}`, and does nothing when there is no current file
/// to roll. Shared by the parameter export and the datagen dataset.
///
/// Params:
/// - dir      : &str -> directory holding the `latest` file and backups
/// - extension: &str -> file extension without the dot (e.g. "param")
pub fn roll_latest(dir: &str, extension: &str) {
    let suffix = format!(".{}", extension);
    let latest = format!("latest{}", suffix);
    let path = format!("{}/{}", dir, latest);

    if !Path::new(&path).exists() {
        return;
    }

    let mut last_epoch = 0usize;
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            if let Ok(name) = entry.file_name().into_string()
            && name.ends_with(&suffix) && name != latest
            && let Ok(num) = name[..name.len() - suffix.len()].parse::<usize>()
            {
                last_epoch = last_epoch.max(num);
            }
        }
    }

    let archive = format!("{}/{}{}", dir, last_epoch + 1, suffix);
    fs::rename(&path, &archive).unwrap_or_else(|e| {
        panic!("Failed to roll file {}: {}", archive, e)
    });
}

/// refresh_eval_state
///
/// Recomputes opening/endgame eval caches and current game phase.
/// Used after position-level changes (load, tune import, make/undo move) to
/// keep material, PST bonus, pawn occupancy, and phase classification in
/// sync with `piece_list` and PST tables.
///
/// Params:
/// - state: &mut State -> position whose eval caches are rebuilt
pub fn refresh_eval_state(state: &mut State) {
    state.opening_material = [0; 2];
    state.endgame_material = [0; 2];
    state.opening_pst_bonus = [0; 2];
    state.endgame_pst_bonus = [0; 2];
    state.pair_score = 0;
    state.pawn_board = [board!(state.statics.files, state.statics.ranks); 2];

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;

        for &square in piece_squares!(state, piece_idx) {
            state.opening_material[color] += p_ovalue!(piece) as u32;
            state.endgame_material[color] += p_evalue!(piece) as u32;

            state.opening_pst_bonus[color] +=
                state.statics.pst_opening[piece_idx][square as usize];
            state.endgame_pst_bonus[color] +=
                state.statics.pst_endgame[piece_idx][square as usize];
        }
    }

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        if state.piece_count[piece_idx] >= 2 {
            let sign = 1 - 2 * p_color!(piece) as i32;
            state.pair_score += state.statics.pair_bonus[piece_idx] * sign;
        }
    }

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        if !p_is_pawn!(piece) {
            continue;
        }

        let color = p_color!(piece) as usize;

        for &square in piece_squares!(state, piece_idx) {
            set!(state.pawn_board[color], square as u32);
        }
    }

    if drops!(state) || state.game_phase == SETUP {
        for side in [WHITE as usize, BLACK as usize] {
            for (index, count) in
                state.piece_in_hand[side].iter().enumerate()
            {
                let piece = &state.statics.pieces[index];
                let count = *count as u32;

                state.opening_material[side] +=
                    p_ovalue!(piece) as u32 * count;
                state.endgame_material[side] +=
                    p_evalue!(piece) as u32 * count;
            }
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

/// square_distance
///
/// Calculates the euclidean distance between two squares on the board,
/// treating file and rank deltas as cartesian coordinates.
///
/// Params:
/// - state: &State -> supplies the board width for index decomposition
/// - sq1  : Square -> first square
/// - sq2  : Square -> second square
///
/// Return:
/// f64             -> euclidean distance in square units
pub fn square_distance(state: &State, sq1: Square, sq2: Square) -> f64 {
    let file1 = sq1 % state.statics.files as Square;
    let rank1 = sq1 / state.statics.files as Square;
    let file2 = sq2 % state.statics.files as Square;
    let rank2 = sq2 / state.statics.files as Square;

    let df = (file1 as i32 - file2 as i32).abs() as f64;
    let dr = (rank1 as i32 - rank2 as i32).abs() as f64;

    (df.powi(2) + dr.powi(2)).sqrt()
}

/// verify_game_state
///
/// Recomputes derived state and asserts it matches the stored caches.
/// Debug integrity check for boards, piece lists, material counts, royal
/// lists, and the incremental Zobrist hash. On mismatch it also attempts to
/// pinpoint the source before panicking.
///
/// Params:
/// - state: &State -> position whose incremental caches are validated
pub fn verify_game_state(state: &State) {

    assert_eq!(
        state.phase_score, game_phase_score!(state),
        "Game phase score doesn't match expected value based on material counts"
    );

    let expected_pair_score = state.statics.pieces.iter()
        .enumerate()
        .filter(|(index, _)| state.piece_count[*index] >= 2)
        .map(|(index, piece)| {
            let sign = 1 - 2 * p_color!(piece) as i32;
            state.statics.pair_bonus[index] * sign
        })
        .sum::<i32>();
    assert_eq!(
        state.pair_score, expected_pair_score,
        "Incremental pair score doesn't match piece counts"
    );

    let mut temp_white_board = board!(
        state.statics.files, state.statics.ranks
    );
    let mut temp_black_board = board!(
        state.statics.files, state.statics.ranks
    );
    let mut temp_pawn_board = [board!(
        state.statics.files, state.statics.ranks
    ); 2];
    let mut temp_piece_list: Vec<Vec<Square>> =
        vec![Vec::new(); state.statics.pieces.len()];

    for (square, piece_idx) in state.main_board.iter().enumerate() {
        if *piece_idx != NO_PIECE {
            let piece = &state.statics.pieces[*piece_idx as usize];

            if p_color!(piece) == WHITE {
                set!(temp_white_board, square as u32);
            } else {
                set!(temp_black_board, square as u32);
            }

            if p_is_pawn!(piece) {
                set!(
                    temp_pawn_board[p_color!(piece) as usize],
                    square as u32
                );
            }

            temp_piece_list[p_index!(piece) as usize].push(square as Square);
        }
    }

    let mut sorted_piece_list: Vec<Vec<Square>> =
        (0..state.statics.pieces.len())
            .map(|index| piece_squares!(state, index).copied().collect())
            .collect();

    for squares in temp_piece_list.iter_mut() {
        squares.sort_unstable();
    }
    for squares in sorted_piece_list.iter_mut() {
        squares.sort_unstable();
    }

    assert_eq!(
        &temp_piece_list, &sorted_piece_list,
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

    for color in [WHITE as usize, BLACK as usize] {
        assert_eq!(
            &temp_pawn_board[color],
            &state.pawn_board[color],
            "Computed pawn board doesn't match state pawn board\n{}\n{}\n{}",
            format_board(&temp_pawn_board[color], None),
            format_board(&state.pawn_board[color], None),
            format_game_state(state)
        );
    }

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

    for piece in state.statics.pieces.iter() {
        let index = p_index!(piece) as usize;

        for &square in piece_squares!(state, index) {
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
                state.statics.pst_opening[index][square as usize];
            temp_endgame_pst_bonus[color] +=
                state.statics.pst_endgame[index][square as usize];
        }
    }

    if drops!(state) || state.game_phase == SETUP {
        for side in [WHITE as usize, BLACK as usize] {
            for (index, count) in
                state.piece_in_hand[side].iter().enumerate()
            {
                let piece = &state.statics.pieces[index];
                let count = *count as u32;

                temp_opening_material[side] +=
                    p_ovalue!(piece) as u32 * count;
                temp_endgame_material[side] +=
                    p_evalue!(piece) as u32 * count;
            }
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

        for &index in piece_squares!(state, i) {
            temp_hash ^= PIECE_HASHES[i][index as usize];
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

/// parse_perft_content
///
/// Parses a perft suite file into test cases: each non-comment line holds
/// a FEN followed by the expected node counts for depths one through six,
/// comma-separated.
///
/// Params:
/// - content: &str -> raw text of the .perft suite file
///
/// Return:
///
/// Vec<(String, u64, ...)>
/// FEN plus expected node counts for depths 1-6, one tuple per suite line
fn parse_perft_content(                                                         /* until perft 6                      */
    content: &str,
) -> Vec<(String, u64, u64, u64, u64, u64, u64)> {
    let uncommented = COMMENT_PATTERN.replace_all(content, "");

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

/// format_time
///
/// Renders a nanosecond duration using the largest unit that keeps the
/// value readable, from raw nanoseconds up to seconds.
///
/// Params:
/// - nanos: u128 -> duration in nanoseconds
///
/// Return:
/// String        -> human-readable duration such as "1.234 ms"
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

/// benchmark_headless_perft
///
/// Runs a perft test without a truth suite, reporting total nodes and elapsed
/// time for a given depth. For quick sanity checks and performance profiling
/// without needing a full suite of expected results.
///
/// Params:
/// - state : &mut State          -> starting position, mutated during walk
/// - depth : u8                  -> maximum perft depth to run
/// - branch: i8                  -> diagnostic branch-printing depth
/// - dict  : Option<&Translator> -> translator for printed move names
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

/// benchmark_perft
///
/// Runs a perft suite loaded from `content` and reports pass/fail per depth.
/// Cases are shuffled, capped by `limit`, and each position is tested from
/// depth 1 up to `depth`. `branch` controls diagnostic line printing when
/// passed through to `perft`.
///
/// Params:
/// - state  : &mut State          -> reused for every loaded FEN
/// - content: &str                -> raw perft suite text
/// - depth  : u8                  -> maximum depth tested per position
/// - branch : i8                  -> diagnostic branch-printing depth
/// - limit  : usize               -> maximum number of positions to test
/// - dict   : Option<&Translator> -> translator for printed move names
///
/// Return:
/// (usize, usize)                 -> (passed cases, total cases)
pub fn benchmark_perft(
    state: &mut State,
    content: &str,
    depth: u8,
    branch: i8,
    limit: usize,
    dict: Option<&Translator>,
) -> (usize, usize) {
    let mut perft_cases = parse_perft_content(content);

    if perft_cases.is_empty() {
        return (0, 0);
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

        log_5!("Loading FEN: {}", fen);

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
                log_4!(
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
                log_4!(
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

    (successful_cases, total_cases)
}

/// benchmark_search
///
/// Runs a fixed-depth search benchmark from the current position.
/// Reports the current position, executes search, and logs total nodes, elapsed
/// wall time, and aggregate nodes-per-second.
///
/// Params:
/// - state     : &mut State          -> position searched
/// - ttable    : Arc<TTable>         -> shared transposition table
/// - qtable    : Arc<QTable>         -> shared quiescence table
/// - ptable    : Arc<PTable>         -> shared pawn structure table
/// - depth     : usize               -> fixed search depth
/// - thread_num: usize               -> number of worker threads
/// - dict      : Option<&Translator> -> translator for printed move names
pub fn benchmark_search(
    state: &mut State, ttable: Arc<TTable>, qtable: Arc<QTable>,
    ptable: Arc<PTable>, depth: usize, thread_num: usize,
    dict: Option<&Translator>,
) {
    log_3!("Search benchmark started with depth {}...", depth);

    let mut info = SearchInfo { set_depth: depth, ..Default::default() };
    let mut bufs = SearchBufs::default();

    search_position(
        state, Arc::clone(&ttable), Arc::clone(&qtable),
        Arc::clone(&ptable), &mut info, &mut bufs, thread_num, dict
    );
    log_table_stats(&ttable, &qtable, &ptable);
}

/// perft
///
/// Counts legal move tree nodes from the current state up to `depth`.
/// When `branch >= 0`, prints the explored move prefixes for the first levels
/// to help inspect branching behavior.
///
/// Params:
/// - state : &mut State          -> position explored, restored on return
/// - depth : u8                  -> remaining depth to expand
/// - branch: i8                  -> levels of move-prefix printing left
/// - prefix: &str                -> accumulated move prefix for diagnostics
/// - dict  : Option<&Translator> -> translator for printed move names
///
/// Return:
/// u64                           -> number of leaf nodes at the requested depth
pub fn perft(
    state: &mut State, depth: u8, branch: i8, prefix: &str,
    dict: Option<&Translator>,
) -> u64 {

    if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
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
        let new_prefix = format!("{} {}", prefix, formatted_move);

        if make_move!(state, mv) {
            nodes += perft(state, depth - 1, branch - 1, &new_prefix, dict);
            undo_move!(state);
        }
    }

    log_4!("{} moves | Nodes: {}", prefix, nodes);

    nodes
}

/// run_derive_headless
///
/// Loads every embedded variant config in turn through the same
/// embedded-first parameter path the engine uses at startup. Variants
/// without a parameter payload derive one and export it to
/// `res/param/{variant}/latest.param`, so a delete-then-derive cycle
/// regenerates every shipped file without the interactive console.
pub fn run_derive_headless() {
    for config in EMBEDDED_CONFIGS.files() {
        let Some(filename) = config.path().to_str() else {
            continue;
        };

        if !filename.ends_with(".conf") || filename == "example.conf" {
            continue;
        }

        emit(EngineEvent::Print(format!("deriving {}\n", filename)));
        let _ = parse_config_file(filename);
    }
}

/// run_datagen_headless
///
/// Headless entry point for the `datagen` subcommand. Loads the named
/// variant through the embedded-first config path, drains the event
/// channel the interactive console would own, and generates a self-play
/// dataset into `res/data/{variant}/latest.data`. Games run sequentially
/// per process, so dataset volume comes from launching many single-thread
/// processes rather than one many-thread process.
///
/// Params:
/// - args: &[String] -> full argv, expects
///                      `datagen <variant> <games> <movetime_ms> [threads=1]`
pub fn run_datagen_headless(args: &[String]) {
    let Some(variant) = args.get(2) else {
        log_2!("Usage: datagen <variant> <games> <movetime (ms)> [threads = 1]");
        return;
    };

    let Some(Ok(games)) = args.get(3).map(|value| value.parse::<usize>()) else {
        log_2!("Usage: datagen <variant> <games> <movetime (ms)> [threads = 1]");
        return;
    };

    let Some(Ok(movetime)) = args.get(4).map(|value| value.parse::<u128>())
    else {
        log_2!("Usage: datagen <variant> <games> <movetime (ms)> [threads = 1]");
        return;
    };

    let threads = args.get(5)
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(1)
        .max(1);

    let state = parse_config_file(&format!("{}.conf", variant));

    run_datagen(
        &state, variant, None,
        Arc::new(TTable::default()), Arc::new(QTable::default()),
        Arc::new(PTable::default()), threads, games, movetime,
    );
}

/// run_tune_headless
///
/// Headless entry point for the `tune` subcommand. Loads the named variant
/// and Texel-tunes its evaluation from `res/data/{variant}/latest.data`,
/// exporting the best validation epoch to `res/param/{variant}/latest.param`
/// through the same startup parameter pipeline the console uses.
///
/// Params:
/// - args: &[String] -> full argv, expects `tune <variant> <epochs> [rate=1.0]`
pub fn run_tune_headless(args: &[String]) {
    let Some(variant) = args.get(2) else {
        log_2!("Usage: tune <variant> <epochs> [learning rate = 1.0]");
        return;
    };

    let Some(Ok(epochs)) = args.get(3).map(|value| value.parse::<usize>())
    else {
        log_2!("Usage: tune <variant> <epochs> [learning rate = 1.0]");
        return;
    };

    let learning_rate = args.get(4)
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(1.0);

    let mut state = parse_config_file(&format!("{}.conf", variant));
    run_tuning(&mut state, variant, epochs, learning_rate);
}

