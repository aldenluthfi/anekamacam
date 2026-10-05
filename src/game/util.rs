//! util.rs
//!
//! Shared helpers that do not belong to one subsystem.
//!
//! This file reads tool arguments, loads variants, rolls output files and
//! rebuilds position caches. It also plays full games, counts move trees
//! with perft, and tests that the incremental state agrees with the board.
//!
//! Created: 25/01/2025
//! Author : Alden Luthfi

use crate::*;

/// ARCHIVE_STAMP_FMT
///
/// The timestamp format of a rolled file. The largest unit is first and
/// each field has zeros, so the alphabetical order is the time order.
///
const ARCHIVE_STAMP_FMT: &str = "%Y-%m-%d_%H-%M-%S";

/*----------------------------------------------------------------------------*\
                             ARGUMENTS AND STARTUP
\*----------------------------------------------------------------------------*/

/// parse_number
///
/// Reads one optional positional argument. If it is absent, the function
/// gives the default. For index 1 with the name `mb` and default 1:
///
/// - `["standard", "64"]` : gives 64
/// - `["standard"]`       : gives the default
/// - `["standard", "x"]`  : error `Invalid mb: x`
///
/// Params:
/// - values : &[S]   -> the positional arguments
/// - index  : usize  -> index of the argument to read
/// - default: T      -> value when the argument is absent
/// - name   : &str   -> argument name for the error message
///
/// Return:
/// Result<T, String> -> the parsed or default value, or the error
///
pub fn parse_number<T, S>(
    values: &[S],
    index: usize,
    default: T,
    name: &str,
) -> Result<T, String>
where
    T: std::str::FromStr,
    S: AsRef<str>,
{
    values
        .get(index)
        .map(|value| {
            let value = value.as_ref();
            value
                .parse::<T>()
                .map_err(|_| format!("Invalid {}: {}", name, value))
        })
        .unwrap_or(Ok(default))
}

/// load_variant
///
/// Loads a variant by name with the same configuration path as a session.
/// Only embedded configurations are available. An unknown name is an
/// error.
///
/// Params:
/// - variant: &str       -> embedded configuration name
///
/// Return:
/// Result<State, String> -> loaded state, or an unknown variant error
///
#[hotpath::measure]
pub fn load_variant(variant: &str) -> Result<State, String> {
    let config_name = format!("{}.conf", variant);
    if EMBEDDED_CONFIGS.get_file(&config_name).is_none() {
        return Err(format!("Unknown variant: {}", variant));
    }

    Ok(parse_config_file(&config_name))
}

/// exe_tag
///
/// Gives the file name of the running binary. Each search thread has it in
/// its name, so a panic in an SPRT match tells which build failed:
///
/// - `search:engine-a`     : protocol thread of that build
/// - `searcher:engine-a:3` : its fourth worker
///
/// Return:
/// String -> file name of the binary, or "?" if the path is not readable
///
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
/// Gives a random 128-bit value from the shared seeded generator, as two
/// 64-bit draws. All Zobrist tables use it. With `ANEKAMACAM_SEED` set,
/// two runs give the same key to the same position.
///
/// Return:
/// u128 -> uniform random value from the shared generator
///
pub fn random_u128() -> u128 {
    let mut rng = RNG.lock().unwrap_or_else(|e| {
        panic!("Failed to lock RNG mutex for random_u128: {e}")
    });
    u128::from(rng.next_u64()) << 64 | u128::from(rng.next_u64())
}

/*----------------------------------------------------------------------------*\
                                 ROLLING FILES
\*----------------------------------------------------------------------------*/

/// roll_latest
///
/// Renames the current `latest` file with a timestamp, so the caller can
/// write a new one. All rolling outputs use it: logs, parameters, datasets
/// and match results.
///
/// - timestamp : file creation time, else modification time, else now
/// - same name : add `-2`, `-3` and so on until the name is free
/// - no file   : do nothing
///
/// Params:
/// - dir      : &str -> directory with the current file and backups
/// - prefix   : &str -> name prefix before `latest` or the timestamp
/// - extension: &str -> file extension without the dot, e.g. "param"
///
/// Notes:
/// The prefix is empty except in SPRT. There the logs of the two engines
/// go to one directory, as `engine-a_` and `engine-b_`.
///
pub fn roll_latest(dir: &str, prefix: &str, extension: &str) {
    let current = format!("{}/{}latest.{}", dir, prefix, extension);

    if !Path::new(&current).exists() {
        return;
    }

    let moment: chrono::DateTime<chrono::Local> = fs::metadata(&current)
        .and_then(|meta| meta.created().or_else(|_| meta.modified()))
        .map(Into::into)
        .unwrap_or_else(|_| chrono::Local::now());

    let stamp = moment.format(ARCHIVE_STAMP_FMT).to_string();

    let mut archive = format!("{}/{}{}.{}", dir, prefix, stamp, extension);
    let mut discriminator = 2usize;
    while Path::new(&archive).exists() {
        archive = format!(
            "{}/{}{}-{}.{}", dir, prefix, stamp, discriminator, extension,
        );
        discriminator += 1;
    }

    fs::rename(&current, &archive).unwrap_or_else(|error| {
        panic!("Failed to roll file {}: {}", archive, error)
    });
}

/// prune_backups
///
/// Keeps only the `keep` newest rolled files of one family and deletes the
/// others. It never deletes the `latest` file. A `keep` of zero deletes all
/// backups. The name sort gives the age, because of `ARCHIVE_STAMP_FMT`.
///
/// Params:
/// - dir      : &str  -> directory with the backups
/// - prefix   : &str  -> name prefix of the backup family
/// - extension: &str  -> file extension without the dot
/// - keep     : usize -> number of newest backups to keep
///
/// Notes:
/// Each file with the prefix and extension is part of the family, also
/// other files. The function ignores read and delete errors.
///
pub fn prune_backups(dir: &str, prefix: &str, extension: &str, keep: usize) {
    let latest = format!("{}latest.{}", prefix, extension);
    let suffix = format!(".{}", extension);

    let mut backups = Vec::new();
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            if let Ok(name) = entry.file_name().into_string()
            && name.starts_with(prefix) && name.ends_with(&suffix)
            && name != latest
            {
                backups.push(name);
            }
        }
    }

    if backups.len() <= keep {
        return;
    }

    backups.sort();
    let remove_count = backups.len() - keep;
    for name in backups.into_iter().take(remove_count) {
        let _ = fs::remove_file(format!("{}/{}", dir, name));
    }
}

/*----------------------------------------------------------------------------*\
                               DERIVED EVAL STATE
\*----------------------------------------------------------------------------*/

/// refresh_eval_state
///
/// Calculates the evaluation caches from the board. A move updates them
/// one piece at a time. A FEN load or new parameters need a full rebuild:
///
/// - material : opening and endgame totals for each colour
/// - bonus    : the piece-square pair, summed over occupied squares
/// - roles    : big, major and minor piece counts
/// - phase    : the phase score and its phase
///
/// Params:
/// - state: &mut State -> position with the caches to rebuild
///
/// Notes:
/// Pieces in hand count as material in drop variants and in the setup
/// phase. The phase is last. It counts only big pieces that are not royal.
///
pub fn refresh_eval_state(state: &mut State) {
    state.opening_material = [0; 2];
    state.endgame_material = [0; 2];
    state.opening_pst_bonus = [0; 2];
    state.endgame_pst_bonus = [0; 2];
    state.big_pieces = [0; 2];
    state.major_pieces = [0; 2];
    state.minor_pieces = [0; 2];

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let count = state.piece_count[piece_idx];

        state.big_pieces[color] += count * (p_is_big!(piece) as u32);
        state.major_pieces[color] += count * (p_is_major!(piece) as u32);
        state.minor_pieces[color] += count * (p_is_minor!(piece) as u32);

        for &square in piece_squares!(state, piece_idx) {
            state.opening_material[color] += p_ovalue!(piece) as u32;
            state.endgame_material[color] += p_evalue!(piece) as u32;

            state.opening_pst_bonus[color] +=
                state.statics.pst_opening[piece_idx][square as usize];
            state.endgame_pst_bonus[color] +=
                state.statics.pst_endgame[piece_idx][square as usize];
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

    state.game_phase = game_phase!(state);
}

/// adjudicate_no_move
///
/// Decides a position without legal moves with the checkmate or stalemate
/// outcome of the variant. It stores and returns the game result.
///
/// Params:
/// - state: &mut State -> position where the side to move has no move
///
/// Return:
/// u8                  -> game result
///
pub fn adjudicate_no_move(state: &mut State) -> u8 {
    let in_check = is_in_check!(state.playing, state);
    let (outcome, inverted) = no_move_verdict!(state, in_check);
    let subject = state.playing ^ inverted as u8;                               /* the barred dropper loses instead   */
    let result = resolve_outcome!(subject, outcome);

    state.termination.game_result = result;
    result
}

/// game_result_score
///
/// Converts a game result into the score of White, for datagen and engine
/// matches.
///
/// Params:
/// - result: u8 -> game result
///
/// Return:
/// f64          -> 1.0 White win, 0.0 Black win, else 0.5
///
pub fn game_result_score(result: u8) -> f64 {
    match result {
        WHITE_WIN => 1.0,
        BLACK_WIN => 0.0,
        _ => 0.5,
    }
}

/// play_search_game
///
/// Plays the two sides with a fixed depth and time for each move. The game
/// stops at a game end, no legal move, an interrupt or the ply limit. The
/// tables stay for all moves. `on_move` gets the state and the move text
/// after each move.
///
/// Params:
///
///     state: &mut State
///     current game position
///
///     ttable: Arc<TTable>
///     shared main table
///
///     qtable: Arc<QTable>
///     shared quiescence table
///
///     depth: usize
///     fixed search depth
///
///     time_limit_ns: u128
///     time limit for each move, in nanoseconds
///
///     threads: usize
///     number of search workers
///
///     max_plies: usize
///     maximum number of plies to play
///
///     dict: Option<&Translator>
///     move translator
///
///     on_move: F
///     callback after each move
///
/// Return:
///
///     Result<(u8, Option<String>), String>
///     game result and reason, or an error for an illegal search move
///
pub fn play_search_game<F>(
    state: &mut State,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    depth: usize,
    time_limit_ns: u128,
    threads: usize,
    max_plies: usize,
    dict: Option<&Translator>,
    mut on_move: F,
) -> Result<(u8, Option<String>), String>
where
    F: FnMut(&State, &str),
{
    let mut info = SearchInfo {
        set_depth: depth,
        ..Default::default()
    };

    for _ in 0..max_plies {
        let terminal = game_outcome(state);
        if terminal.0 != ONGOING
        || SYSTEM_INTERRUPT.load(Ordering::Relaxed)
        {
            return Ok(terminal);
        }

        if time_limit_ns > 0 {
            let now = ENGINE_START.elapsed().as_nanos();
            info.deadline = now + time_limit_ns;
        }

        let result = search_position(
            state,
            Arc::clone(&ttable),
            Arc::clone(&qtable),
            &mut info,
            threads.max(1),
            dict,
        );
        log_table_stats(&ttable, &qtable);

        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            return Ok(game_outcome(state));
        }

        if result.best_move == null_move() {
            if info.interrupt {
                return Ok(game_outcome(state));
            }
            adjudicate_no_move(state);
            return Ok(game_outcome(state));
        }
        if result.best_score == -INF {
            adjudicate_no_move(state);
            return Ok(game_outcome(state));
        }

        let move_text = format_move(&result.best_move, state, dict);
        if !make_move!(state, result.best_move) {
            return Err(format!(
                "Search returned illegal move: {}", move_text
            ));
        }
        on_move(state, &move_text);
    }

    Ok(game_outcome(state))
}

/// square_distance
///
/// Calculates the Euclidean distance between two squares. The file and
/// rank deltas are Cartesian coordinates.
///
/// Params:
/// - state: &State -> position with the board width
/// - sq1  : Square -> first square
/// - sq2  : Square -> second square
///
/// Return:
/// f64             -> Euclidean distance in squares
///
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
/// Calculates the derived state again and asserts that it is equal to the
/// stored caches. It is a debug test of the boards, piece lists, material
/// counts, royal lists and the position, pawn and unmoved piece keys. On a
/// difference, it tries to find the cause before the panic.
///
/// Params:
/// - state: &State -> position with the caches to test
///
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

    let mut computed = temp_royal_list;
    let mut tracked = state.royal_list.clone();

    for side in 0..2 {                                                          /* order is not part of the meaning   */
        computed[side].sort_unstable();                                         /* here: promoting into royalty       */
        tracked[side].sort_unstable();                                          /* appends where a rebuild inserts    */
    }

    assert_eq!(
        &computed, &tracked,
        "Computed royal list doesn't match state royal list"
    );

    let mut temp_hash = u128::default();
    let mut temp_pawn_hash = u128::default();

    if state.playing == WHITE {
        temp_hash ^= &*SIDE_HASHES;
    }

    temp_hash ^=
        &CASTLING_HASHES[(state.castling_state & CASTLE_RIGHTS) as usize];

    if state.en_passant_square != NO_EN_PASSANT {
        temp_hash ^=
            &EN_PASSANT_HASHES[enp_square!(state.en_passant_square) as usize];
    }

    for piece in &state.statics.pieces {
        let i = p_index!(piece) as usize;

        for &index in piece_squares!(state, i) {
            let piece_hash = PIECE_HASHES[i][index as usize];

            temp_hash ^= piece_hash;
            if state.statics.eval.pawn_pieces.contains(&i) {
                temp_pawn_hash ^= piece_hash;
            }
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
    assert_eq!(
        temp_pawn_hash, state.pawn_hash,
        "Computed pawn hash doesn't match state pawn hash"
    );

    let temp_virgin_hash = hash_virgin_board(state);

    if temp_virgin_hash != state.virgin_hash {
        let missing_hash = temp_virgin_hash ^ state.virgin_hash;

        for (index, &hash) in VIRGIN_HASHES.iter().enumerate() {
            if hash == missing_hash {
                panic!(
                    "Hash mismatch! Unmoved-piece mark differs at {}",
                    format_square(index as Square, state),
                );
            }
        }

        panic!("Hash mismatch! Unmoved-piece key differs on many squares");
    }
}

/// parse_perft_content
///
/// Parses a perft suite file into test cases. Each line has a FEN and the
/// node counts for depths 1 to 6, separated by commas:
///
/// ```text
/// <FEN>,<depth 1>,<depth 2>,<depth 3>,<depth 4>,<depth 5>,<depth 6>
/// ```
///
/// Params:
///
///     content: &str
///     raw text of the .perft suite file
///
/// Return:
///
///     Vec<(String, u64, u64, u64, u64, u64, u64)>
///     FEN and node counts for depths 1-6, one tuple for each line
///
/// Notes:
/// The parser removes comments and empty lines first. All columns are
/// mandatory. A missing or bad column causes a panic with its name.
///
pub fn parse_perft_content(                                                     /* until perft 6                      */
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
/// Writes a duration in nanoseconds with the largest unit that keeps the
/// value readable, from nanoseconds to seconds.
///
/// Params:
/// - nanos: u128 -> duration in nanoseconds
///
/// Return:
/// String        -> readable duration, for example "1.234 ms"
///
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
/// Runs perft without expected counts and prints the total nodes and the
/// time. Use it for quick tests and profiling.
///
/// Params:
/// - state : &mut State          -> start position, changed during the walk
/// - depth : u8                  -> maximum perft depth
/// - branch: i8                  -> depth of the branch output
/// - dict  : Option<&Translator> -> translator for the move text
///
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
/// Runs a perft suite from `content` and prints pass or fail for each
/// depth. The cases are shuffled and limited to `limit`. Each position is
/// tested from depth 1 to `depth`.
///
/// Params:
/// - state  : &mut State          -> state for each loaded FEN
/// - content: &str                -> raw perft suite text
/// - depth  : u8                  -> maximum depth for each position
/// - branch : i8                  -> depth of the branch output
/// - limit  : usize               -> maximum number of positions
/// - dict   : Option<&Translator> -> translator for the move text
///
/// Return:
/// (usize, usize)                 -> (passed cases, total cases)
///
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

/// perft
///
/// Counts the legal move tree nodes from the current state to `depth`.
/// When `branch >= 0`, it prints the move prefixes of the first levels.
///
/// Params:
/// - state : &mut State          -> position, restored at the end
/// - depth : u8                  -> remaining depth
/// - branch: i8                  -> remaining levels of prefix output
/// - prefix: &str                -> move prefix for the output
/// - dict  : Option<&Translator> -> translator for the move text
///
/// Return:
/// u64                           -> number of leaf nodes at the depth
///
pub fn perft(
    state: &mut State, depth: u8, branch: i8, prefix: &str,
    dict: Option<&Translator>,
) -> u64 {

    if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
        return 0;
    }

    if depth == 0 {
        if branch == 0 {
            emit(EngineEvent::Print(format!("{}: 1\n", prefix.trim())));
        }
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

    if branch == 0 {
        emit(EngineEvent::Print(format!(
            "{}: {}\n", prefix.trim(), nodes
        )));
    }
    log_4!("{} moves | Nodes: {}", prefix, nodes);

    nodes
}

/// run_derive_headless
///
/// Loads each embedded variant config with the startup parameter path. A
/// variant without parameters derives them and exports them to
/// `res/param/{variant}/latest.param`. Thus delete and derive makes all
/// shipped files again.
///
/// Each output line also has the search capabilities of the variant, in
/// the bit order on [`StaticState`].
///
pub fn run_derive_headless() {
    for config in EMBEDDED_CONFIGS.files() {
        let Some(filename) = config.path().to_str() else {
            continue;
        };

        if !filename.ends_with(".conf") || filename == "example.conf" {
            continue;
        }

        emit(EngineEvent::Print(format!("deriving {}", filename)));
        let derived = parse_config_file(filename);
        emit(EngineEvent::Print(format!(
            " capabilities {:09b}\n", derived.statics.capabilities
        )));
    }
}

