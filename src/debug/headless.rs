//! headless.rs
//!
//! Debug command dispatcher without graphics.
//!
//! One command line frontend runs perft, bench, search, evaluation,
//! self-play, derivation, data generation, tuning and SPRT. Each call runs
//! one command and exits.
//!
//! Created: 30/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             COMMAND REPRESENTATION
\*----------------------------------------------------------------------------*/

/// HeadlessPosition
///
/// The position of one command and the other arguments of its line. Eight
/// commands need a position, so one parser reads the line for all.
///
/// - state           : the board, at the requested position
/// - translator      : the notation for input and output, if given
/// - variant         : variant name, for suite and file lookups
/// - values          : the positional arguments, in order
/// - custom_position : true when a FEN or a move list was given
/// - suite           : perft only, run the embedded suite
/// - limit           : perft and bench only, number of cases
/// - branch          : perft only, number of tree levels to show
///
/// The last three are `None` when the flag is not on the line.
///
struct HeadlessPosition {
    state: State,                                                               /* the board the command works on     */
    translator: Option<Translator>,                                             /* the notation both ends read        */
    variant: String,                                                            /* its name, for later lookups        */
    values: Vec<String>,                                                        /* whatever was not a flag            */
    custom_position: bool,                                                      /* a FEN or moves were given          */
    suite: bool,                                                                /* perft: run the whole suite         */
    limit: Option<usize>,                                                       /* perft, bench: how many cases       */
    branch: Option<i8>,                                                         /* perft: levels of divide to print   */
}

/*----------------------------------------------------------------------------*\
                                ARGUMENT PARSING
\*----------------------------------------------------------------------------*/

/// headless_usage
///
/// Makes the help text. The frontend prints it for `help`, for an empty
/// line and after each error. The three position options are in one group,
/// because eight commands use them.
///
/// Return:
/// String -> all commands, flags and arguments of the frontend
///
fn headless_usage() -> String {
    [
        "Usage: anekamacam debug-headless <command> ...\n\n",

        "Commands:\n",
        "  state <variant>\n",
        "  movegen <variant>\n",
        "  evaluate <variant>\n",
        "  see <variant> <capture>\n",
        "  search <variant> [depth] [threads]\n",
        "  play <variant> <depth> <seconds> [threads] [max-plies]\n",
        "  perft <variant> <depth> [--branch n] [--suite] [--limit n]\n",
        "  bench <variant> <depth> [--limit n]\n",
        "  derive\n",
        "  datagen <variant> <games> <movetime-ms> [threads]\n",
        "  tune <variant> <epochs> [learning-rate]\n",
        "  sprt <variant> <bin-a> <bin-b> <ms|base+inc> [games] [h0] [h1]\n",
        "       [--concurrency n] [--option-a name=value]...\n",
        "       [--option-b name=value]...\n\n",

        "Position options:\n",
        "  --protocol <uci|usi|ucci>\n",
        "  --fen \"<position>\"\n",
        "  --moves <move>...\n",
    ]
    .concat()
}

/// parse_position_arguments
///
/// Reads the line of a position command and gives the board it names. The
/// variant must be first. After it, flags and values can be in any order.
///
/// ```text
/// perft standard 5 --fen "8/8/8/8/8/8/8/K6k w" --limit 20
/// ^     ^        ^ ^                           ^
/// |     |        | |                           perft and bench only
/// |     |        | any position command takes this one
/// |     |        a positional value, read by the command itself
/// |     the variant, which has to come first
/// the command, taken off the line before this is called
/// ```
///
/// - `--protocol` : notation of the FEN and the moves
/// - `--fen`      : start position, in place of the variant start
/// - `--moves`    : moves to play, all words up to the next flag
/// - `--suite`    : perft only
/// - `--limit`    : perft and bench only
/// - `--branch`   : perft only
///
/// Params:
/// - command  : &str                -> command of the line
/// - arguments: &[String]           -> all words after the command
///
/// Return:
/// Result<HeadlessPosition, String> -> the loaded position, or the error
///
/// Notes:
/// A flag of another command is an error, not ignored. The translator
/// applies only to a given FEN, because the variant start position is
/// already in engine notation.
///
#[hotpath::measure]
fn parse_position_arguments(
    command: &str,
    arguments: &[String],
) -> Result<HeadlessPosition, String> {
    let variant = arguments
        .first()
        .ok_or_else(|| "Missing variant".to_string())?
        .clone();
    let mut protocol = None;
    let mut fen = None;
    let mut moves = Vec::new();
    let mut values = Vec::new();
    let mut suite = false;
    let mut limit = None;
    let mut branch = None;
    let mut index = 1usize;

    while index < arguments.len() {
        match arguments[index].as_str() {
            "--protocol" => {
                protocol = Some(
                    arguments
                        .get(index + 1)
                        .ok_or_else(|| {
                            "Missing protocol value".to_string()
                        })?
                        .clone(),
                );
                index += 2;
            }
            "--fen" => {
                fen = Some(
                    arguments
                        .get(index + 1)
                        .ok_or_else(|| "Missing FEN value".to_string())?
                        .clone(),
                );
                index += 2;
            }
            "--moves" => {
                index += 1;
                let move_start = index;
                while index < arguments.len()
                    && !arguments[index].starts_with("--")
                {
                    moves.push(arguments[index].clone());
                    index += 1;
                }
                if index == move_start {
                    return Err("Missing moves".to_string());
                }
            }
            "--suite" if command == "perft" => {
                suite = true;
                index += 1;
            }
            "--limit" if command == "perft" || command == "bench" => {
                limit = Some(
                    arguments
                        .get(index + 1)
                        .ok_or_else(|| "Missing limit value".to_string())?
                        .parse::<usize>()
                        .map_err(|_| {
                            format!(
                                "Invalid limit: {}",
                                arguments[index + 1]
                            )
                        })?,
                );
                index += 2;
            }
            "--branch" if command == "perft" => {
                branch = Some(
                    arguments
                        .get(index + 1)
                        .ok_or_else(|| "Missing branch value".to_string())?
                        .parse::<i8>()
                        .map_err(|_| {
                            format!(
                                "Invalid branch: {}",
                                arguments[index + 1]
                            )
                        })?,
                );
                index += 2;
            }
            value if value.starts_with("--") => {
                return Err(format!("Unknown option: {}", value));
            }
            _ => {
                values.push(arguments[index].clone());
                index += 1;
            }
        }
    }

    let translator = match protocol {
        Some(name) => Some(
            Translator::find(&variant, &name).ok_or_else(|| {
                format!("Variant {} does not support {}", variant, name)
            })?,
        ),
        None => None,
    };
    let supplied_fen = fen.is_some();
    let custom_position = supplied_fen || !moves.is_empty();
    let mut state = load_variant(&variant)?;
    let position = fen.unwrap_or_else(|| state.statics.startpos.clone());
    let position_translator = if supplied_fen {
        translator.as_ref()
    } else {
        None
    };
    state.reset();
    parse_fen(&mut state, &position, position_translator)?;

    {
        let state_reference = &mut state;

        for (ply, move_text) in moves.iter().enumerate() {
            let parsed_move = parse_move(
                move_text,
                state_reference,
                translator.as_ref(),
            )
            .ok_or_else(|| {
                format!(
                    "Move {} failed to parse: {}", ply + 1, move_text
                )
            })?;

            if !make_move!(state_reference, parsed_move) {
                return Err(format!(
                    "Move {} is illegal: {}", ply + 1, move_text
                ));
            }
        }
    }

    Ok(HeadlessPosition {
        state,
        translator,
        variant,
        values,
        custom_position,
        suite,
        limit,
        branch,
    })
}

/*----------------------------------------------------------------------------*\
                               POSITION COMMANDS
\*----------------------------------------------------------------------------*/

/// run_state_command
///
/// Prints the engine state of one position, without a search.
///
/// - board  : the board diagram and the variant data
/// - FEN    : the position as FEN
/// - Hash   : the position hash, for repetition
/// - Keys   : the search and quiescence keys of the tables
/// - Result : the game result, Ongoing if not ended
/// - Reason : the rule that ended the game, only after an end
///
/// Params:
/// - position: HeadlessPosition -> the position
///
/// Return:
/// Result<(), String>           -> Ok, or the rejected value
///
#[hotpath::measure]
fn run_state_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    if !position.values.is_empty() {
        return Err("state accepts no positional values".to_string());
    }

    let (result, reason) = game_outcome(&mut position.state);
    let mut output = format!(
        "{}\nFEN: {}\nHash: {}\nKeys: {}\nResult: {}\n",
        format_game_state(&position.state),
        format_fen(&position.state, position.translator.as_ref()),
        format_position_hash(&position.state),
        format_search_keys(&position.state),
        format_game_result(result),
    );
    if let Some(name) = reason {
        output.push_str(&format!("Reason: {}\n", name));
    }

    emit(EngineEvent::Print(output));
    Ok(())
}

/// run_movegen_command
///
/// Prints the number of legal moves, then one move on each line. The moves
/// are sorted by their text, so two builds can be compared line by line.
///
/// Params:
/// - position: HeadlessPosition -> the position
///
/// Return:
/// Result<(), String>           -> Ok, or the rejected value
///
fn run_movegen_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    if !position.values.is_empty() {
        return Err("movegen accepts no positional values".to_string());
    }

    let state = &mut position.state;
    let mut formatted = legal_moves!(state)
        .iter()
        .map(|candidate| {
            format_move(
                candidate,
                state,
                position.translator.as_ref(),
            )
        })
        .collect::<Vec<_>>();
    formatted.sort();

    let body = if formatted.is_empty() {
        String::new()
    } else {
        format!("{}\n", formatted.join("\n"))
    };
    emit(EngineEvent::Print(format!(
        "{} legal moves\n{}", formatted.len(), body
    )));
    Ok(())
}

/// run_evaluate_command
///
/// Prints the static evaluation of one position for the side to move. It
/// also prints the phase, because the score depends much on it.
///
/// Params:
/// - position: HeadlessPosition -> the position
///
/// Return:
/// Result<(), String>           -> Ok, or the rejected value
///
fn run_evaluate_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    if !position.values.is_empty() {
        return Err("evaluate accepts no positional values".to_string());
    }

    let state = &mut position.state;
    let score = evaluate_position!(state);

    emit(EngineEvent::Print(format!(
        "Variant: {}\nPhase: {}\nEvaluation: {} cp\n",
        position.variant,
        format_game_phase(&position.state),
        score,
    )));
    Ok(())
}

/// run_see_command
///
/// Prints the static exchange result of one capture. The move must be a
/// capture. A quiet move has no exchange, and zero would look like an equal
/// trade.
///
/// Params:
/// - position: HeadlessPosition -> the position and the capture
///
/// Return:
/// Result<(), String>           -> Ok, or why the move is not accepted
///
fn run_see_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    let move_text = position.values.first()
        .ok_or_else(|| "see requires one move".to_string())?;

    if position.values.len() > 1 {
        return Err("see accepts one move".to_string());
    }

    let state = &mut position.state;
    let candidate = parse_move(
        move_text, state, position.translator.as_ref(),
    ).ok_or_else(|| format!("Invalid move: {}", move_text))?;

    if !m_capture!(&candidate) {
        return Err("see requires a capture move".to_string());
    }

    let formatted = format_move(
        &candidate, state, position.translator.as_ref(),
    );
    let score = see!(state, &candidate);

    emit(EngineEvent::Print(format!(
        "SEE {}: {}\n", formatted, score
    )));
    Ok(())
}

/// run_search_command
///
/// Searches one position to a fixed depth and prints the result. The
/// defaults are depth 4 and one thread.
///
/// - Best move : the selected move, or `(none)`
/// - Score     : the score for the side to move
/// - Nodes     : number of searched positions
/// - Elapsed   : search time
///
/// Params:
/// - position: HeadlessPosition -> the position, depth and threads
///
/// Return:
/// Result<(), String>           -> Ok, or the rejected limit
///
/// Notes:
/// The command makes new 1 MB tables each time. Old table entries would
/// change the node count of the same position.
///
fn run_search_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    let depth = parse_number(&position.values, 0, 4usize, "depth")?;
    let threads = parse_number(&position.values, 1, 1usize, "threads")?;
    if depth == 0 {
        return Err("search depth must be positive".to_string());
    }
    if threads == 0 {
        return Err("search threads must be positive".to_string());
    }
    if position.values.len() > 2 {
        return Err(
            "search accepts at most depth and threads".to_string()
        );
    }

    let ttable = Arc::new(TTable::with_mb(1));
    let qtable = Arc::new(QTable::with_mb(1));
    let mut information = SearchInfo {
        set_depth: depth,
        ..Default::default()
    };
    let state = &mut position.state;
    let result = search_position(
        state,
        ttable,
        qtable,
        &mut information,
        threads,
        position.translator.as_ref(),
    );
    let best_move = if result.best_move == null_move() {
        "(none)".to_string()
    } else {
        format_move(
            &result.best_move,
            state,
            position.translator.as_ref(),
        )
    };

    emit(EngineEvent::Print(format!(
        "Best move: {}\nScore: {}\nNodes: {}\nElapsed: {}\n",
        best_move,
        result.best_score,
        result.total_nodes,
        format_time(result.total_elapsed),
    )));
    Ok(())
}

/// run_play_command
///
/// Plays a self-play game from the position. It prints each move on one
/// line, then the result and the last position.
///
/// - depth     : search depth for each move, must be positive
/// - time      : seconds for each move, 0 means depth only
/// - threads   : search threads, 1 by default
/// - max-plies : ply limit of the game, 2048 by default
///
/// Params:
/// - position: HeadlessPosition -> the position and the four limits
///
/// Return:
/// Result<(), String>           -> Ok, or the rejected limit
///
/// Notes:
/// The ply limit stops games in variants without a progress rule. The two
/// tables stay for the full game, as in a real game.
///
fn run_play_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    let depth = parse_number(&position.values, 0, 0usize, "depth")?;
    let time_seconds = parse_number(&position.values, 1, 0.0f64, "time")?;
    let threads = parse_number(&position.values, 2, 1usize, "threads")?;
    let max_plies = parse_number(
        &position.values,
        3,
        2048usize,
        "max plies",
    )?;
    if depth == 0 {
        return Err("play requires positive depth".to_string());
    }
    if time_seconds < 0.0 {
        return Err("play time cannot be negative".to_string());
    }
    if threads == 0 {
        return Err("play threads must be positive".to_string());
    }
    if max_plies == 0 {
        return Err("play max plies must be positive".to_string());
    }
    if position.values.len() > 4 {
        return Err(
            "play accepts depth, time, threads, and max plies".to_string()
        );
    }

    let ttable = Arc::new(TTable::with_mb(1));
    let qtable = Arc::new(QTable::with_mb(1));
    let time_limit_ns =
        (time_seconds * 1_000_000_000.0) as u128;
    let translator = position.translator.as_ref();
    let (result, reason) = play_search_game(
        &mut position.state,
        ttable,
        qtable,
        depth,
        time_limit_ns,
        threads,
        max_plies,
        translator,
        |_, move_text| {
            emit(EngineEvent::Print(format!("{}\n", move_text)));
        },
    )?;

    let mut output = format!(
        "Result: {}\nFEN: {}\n",
        format_game_result(result),
        format_fen(&position.state, translator),
    );
    if let Some(name) = reason {
        output.push_str(&format!("Reason: {}\n", name));
    }
    emit(EngineEvent::Print(output));
    Ok(())
}

/// run_perft_command
///
/// Counts the legal move tree of one position or of the embedded suite of
/// the variant.
///
/// - `perft standard 5`                    : one position, divided at root
/// - `perft standard 5 --branch 2`         : the same, two levels shown
/// - `perft standard 5 --suite`            : all cases of `standard.perft`
/// - `perft standard 5 --suite --limit 20` : the first 20 cases
///
/// Params:
/// - position: HeadlessPosition -> the position and the perft options
///
/// Return:
/// Result<(), String>           -> Ok, or the rejected option
///
/// Notes:
/// The suite does not accept a FEN or moves, because each case has its own
/// position. Suite depths are 1 to 6. `--limit` without `--suite` is an
/// error.
///
fn run_perft_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    let depth = parse_number(&position.values, 0, 0u8, "depth")?;
    if depth == 0 {
        return Err("perft requires positive depth".to_string());
    }
    if position.values.len() > 1 {
        return Err("perft accepts one positional depth".to_string());
    }
    if !position.suite && position.limit.is_some() {
        return Err("--limit requires --suite".to_string());
    }

    if position.suite {
        if position.custom_position {
            return Err(
                "suite perft does not accept FEN or moves".to_string()
            );
        }
        if depth > 6 {
            return Err(
                "suite perft supports depths one through six".to_string()
            );
        }

        let suite_name = format!("{}.perft", position.variant);
        let content = EMBEDDED_PERFT
            .get_file(&suite_name)
            .and_then(|file| file.contents_utf8())
            .ok_or_else(|| {
                format!("No perft suite for {}", position.variant)
            })?;
        let branch = position.branch.unwrap_or(-1);
        let limit = position.limit.unwrap_or(usize::MAX);
        if limit == 0 {
            return Err("perft limit must be positive".to_string());
        }
        let (passed, total) = benchmark_perft(
            &mut position.state,
            content,
            depth,
            branch,
            limit,
            position.translator.as_ref(),
        );
        emit(EngineEvent::Print(format!(
            "Perft: {}/{} cases passed\n", passed, total
        )));
        return Ok(());
    }

    let nodes = perft(
        &mut position.state,
        depth,
        position.branch.unwrap_or(1),
        "",
        position.translator.as_ref(),
    );
    emit(EngineEvent::Print(format!(
        "\nNodes searched: {}\n", nodes
    )));
    Ok(())
}

/// bench_walk_fen
///
/// Plays some pseudo-random moves from the start position and gives the
/// FEN of the result. The bench uses these walks when the perft suite has
/// too few cases. The seed and the index fix each walk:
///
/// - length : `6 + index mod 10` plies
/// - mixing : `seed ^ index * 0x9E3779B97F4A7C15`, one LCG step per ply
/// - choice : from the legal moves sorted by their text
///
/// Params:
/// - state: &mut State -> variant state, used again for each walk
/// - seed : u64        -> mixing seed, same for the compared binaries
/// - index: usize      -> walk number, sets the length and the mixing
///
/// Return:
/// String              -> FEN of the reached position, not terminal
///
/// Notes:
/// The sort makes a walk the same for two builds with a different move
/// order. If a move ends the game, the walk undoes it and stops.
///
fn bench_walk_fen(
    state: &mut State,
    seed: u64,
    index: usize,
) -> String {
    let startpos = state.statics.startpos.clone();
    state.load_fen(&startpos, None);

    let plies = 6 + index % 10;
    let mut mix = seed
        ^ (index as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);

    for _ in 0..plies {
        let mut choices = legal_moves!(state)
            .iter()
            .map(|candidate| {
                (format_move(candidate, state, None), candidate.clone())
            })
            .collect::<Vec<_>>();
        if choices.is_empty() {
            break;
        }
        choices.sort_by(|left, right| left.0.cmp(&right.0));

        mix = mix
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        let chosen = choices[(mix >> 33) as usize % choices.len()]
            .1
            .clone();
        if !make_move!(state, chosen) {
            break;
        }
        if is_terminal!(state) {
            undo_move!(state);
            break;
        }
    }

    format_fen(state, None)
}

/// run_bench_command
///
/// Searches a fixed set of positions to a fixed depth and prints the speed.
/// It prints one line for each position and one line for the total:
///
/// ```text
/// position 001 nodes       412988 time_ns      93214000 nps   4430536
/// position 002 nodes       938114 time_ns     201880000 nps   4646887
/// total nodes 1351102 time_ns 295094000 nps 4578549
/// ```
///
/// The positions come from the perft suite of the variant, without cases
/// of zero nodes. If the suite is too short, `bench_walk_fen` adds walks.
/// The default limit is 16 positions.
///
/// Params:
/// - position: HeadlessPosition -> the depth and the case limit
///
/// Return:
/// Result<(), String>           -> Ok, or the rejected argument
///
/// Notes:
/// All settings are fixed, so two runs differ only by the build: one
/// thread, the same depth, and new tables for each position. A FEN or a
/// move list is an error, because all runs must use the same positions.
///
fn run_bench_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    let depth = parse_number(&position.values, 0, 0usize, "depth")?;
    if depth == 0 {
        return Err("bench requires positive depth".to_string());
    }
    if position.values.len() > 1 {
        return Err("bench accepts one positional depth".to_string());
    }
    if position.custom_position {
        return Err("bench does not accept FEN or moves".to_string());
    }

    let suite_name = format!("{}.perft", position.variant);
    let content = EMBEDDED_PERFT
        .get_file(&suite_name)
        .and_then(|file| file.contents_utf8())
        .ok_or_else(|| {
            format!("No perft suite for {}", position.variant)
        })?;
    let limit = position.limit.unwrap_or(16);
    if limit == 0 {
        return Err("bench limit must be positive".to_string());
    }

    let mut fens = parse_perft_content(content)
        .into_iter()
        .filter(|case| case.1 > 0)
        .take(limit)
        .map(|case| case.0)
        .collect::<Vec<_>>();
    let mut walk = 0usize;

    while fens.len() < limit {
        fens.push(bench_walk_fen(&mut position.state, *SEED, walk));
        walk += 1;
    }

    let mut total_nodes = 0u128;
    let mut total_time = 0u128;
    let mut output = String::new();

    for (index, fen) in fens.iter().enumerate() {
        position.state.load_fen(fen, None);

        let ttable = Arc::new(TTable::with_mb(16));
        let qtable = Arc::new(QTable::with_mb(1));
        let mut information = SearchInfo {
            set_depth: depth,
            ..Default::default()
        };
        let start = ENGINE_START.elapsed().as_nanos();
        let result = search_position(
            &mut position.state,
            ttable,
            qtable,
            &mut information,
            1,
            position.translator.as_ref(),
        );
        let elapsed = ENGINE_START
            .elapsed()
            .as_nanos()
            .saturating_sub(start)
            .max(1);
        let speed = result.total_nodes * 1_000_000_000 / elapsed;

        total_nodes += result.total_nodes;
        total_time += elapsed;
        output.push_str(&format!(
            "position {:03} nodes {:>12} time_ns {:>13} nps {:>9}\n",
            index + 1, result.total_nodes, elapsed, speed,
        ));
    }

    let total_speed = total_nodes * 1_000_000_000 / total_time.max(1);
    output.push_str(&format!(
        "total nodes {} time_ns {} nps {}\n",
        total_nodes, total_time, total_speed,
    ));
    emit(EngineEvent::Print(output));
    Ok(())
}

/*----------------------------------------------------------------------------*\
                                 TOOL COMMANDS
\*----------------------------------------------------------------------------*/

/// run_datagen_command
///
/// Reads the data generation arguments and starts `run_datagen`.
///
/// - variant  : variant of the games
/// - games    : number of games, for all threads
/// - movetime : milliseconds for each move
/// - threads  : number of search threads, 1 by default
///
/// Params:
/// - arguments: &[String] -> the variant and the three limits
///
/// Return:
/// Result<(), String>     -> Ok, or the rejected argument
///
/// Notes:
/// A zero number is an error, because the run would make no data.
///
fn run_datagen_command(arguments: &[String]) -> Result<(), String> {
    if arguments.len() < 3 || arguments.len() > 4 {
        return Err(
            "datagen requires variant, games, and movetime".to_string()
        );
    }

    let variant = &arguments[0];
    let state = load_variant(variant)?;
    let games = parse_number(arguments, 1, 0usize, "games")?;
    let movetime = parse_number(arguments, 2, 0u128, "movetime")?;
    let threads = parse_number(arguments, 3, 1usize, "threads")?;
    if games == 0 || movetime == 0 || threads == 0 {
        return Err(
            "datagen games, movetime, and threads must be positive".to_string()
        );
    }

    run_datagen(
        &state,
        variant,
        None,
        Arc::new(TTable::default()),
        Arc::new(QTable::default()),
        threads,
        games,
        movetime,
    );
    Ok(())
}

/// run_tune_command
///
/// Reads the tuning arguments and starts a tuning run on the data of the
/// variant.
///
/// - variant       : variant with the parameters to fit
/// - epochs        : number of passes over the data
/// - learning rate : step size, 1.0 by default
///
/// Params:
/// - arguments: &[String] -> the variant, the epochs and the rate
///
/// Return:
/// Result<(), String>     -> Ok, or the rejected argument
///
/// Notes:
/// The two numbers must be above zero, else the run fits nothing.
///
fn run_tune_command(arguments: &[String]) -> Result<(), String> {
    if arguments.len() < 2 || arguments.len() > 3 {
        return Err("tune requires variant and epochs".to_string());
    }

    let variant = &arguments[0];
    let mut state = load_variant(variant)?;
    let epochs = parse_number(arguments, 1, 0usize, "epochs")?;
    let learning_rate = parse_number(
        arguments,
        2,
        1.0f64,
        "learning rate",
    )?;
    if epochs == 0 || learning_rate <= 0.0 {
        return Err(
            "tune epochs and learning rate must be positive".to_string()
        );
    }

    run_tuning(&mut state, variant, epochs, learning_rate);
    Ok(())
}

/// run_sprt_command
///
/// Reads the arguments of a match between two builds and starts it. The
/// arguments give the two binaries by path.
///
/// - variant  : variant of the match
/// - binary a : the build to test
/// - binary b : the reference build
/// - control  : 1000 for a fixed move time, 8000+80 for a clock
/// - games    : game limit if no bound is reached, 2000 by default
/// - h0       : Elo to reject, 0.0 by default
/// - h1       : Elo to accept, 5.0 by default
///
/// Flags can be anywhere after the variant:
///
/// - `--concurrency n`         : games at the same time, 1 by default
/// - `--option-a name=value`   : `setoption` for engine A, repeatable
/// - `--option-b name=value`   : `setoption` for engine B, repeatable
///
/// ```text
/// sprt xiangqi ./anekamacam fairy-stockfish 10000+100 2000 -5 5
///      --concurrency 8 --option-a Hash=64 --option-b Hash=64
///      --option-b UCI_LimitStrength=true --option-b UCI_Elo=1500
/// ```
///
/// Params:
/// - arguments: &[String] -> the pair, the control, the bounds and flags
///
/// Return:
/// Result<(), String>     -> Ok, or the rejected argument
///
/// Notes:
/// The minimum is two games. The match plays each opening with the two
/// colours.
///
fn run_sprt_command(arguments: &[String]) -> Result<(), String> {
    let mut values = Vec::new();
    let mut concurrency = 1usize;
    let mut options_a = Vec::new();
    let mut options_b = Vec::new();
    let mut index = 0usize;

    while index < arguments.len() {
        let flag = arguments[index].as_str();
        if !flag.starts_with("--") {
            values.push(arguments[index].clone());
            index += 1;
            continue;
        }

        let value = arguments
            .get(index + 1)
            .ok_or_else(|| format!("Missing value for {}", flag))?;
        match flag {
            "--concurrency" => {
                concurrency = value.parse::<usize>().map_err(|_| {
                    format!("Invalid concurrency: {}", value)
                })?;
            }
            "--option-a" => options_a.push(parse_sprt_option(value)?),
            "--option-b" => options_b.push(parse_sprt_option(value)?),
            _ => return Err(format!("Unknown sprt flag: {}", flag)),
        }
        index += 2;
    }

    if values.len() < 4 || values.len() > 7 {
        return Err(
            "sprt requires variant, two binaries, and time control".to_string()
        );
    }

    let state = load_variant(&values[0])?;
    let settings = SPRTMatch {
        variant: values[0].clone(),
        engine_a: SPRTEngine {
            binary: values[1].clone(),
            options: options_a,
        },
        engine_b: SPRTEngine {
            binary: values[2].clone(),
            options: options_b,
        },
        time_control: parse_sprt_time_control(&values[3])?,
        max_games: parse_number(&values, 4, 2000usize, "games")?,
        h0: parse_number(&values, 5, 0.0f64, "h0")?,
        h1: parse_number(&values, 6, 5.0f64, "h1")?,
        concurrency,
    };
    if settings.max_games < 2 {
        return Err("sprt requires at least two games".to_string());
    }
    if settings.concurrency == 0 {
        return Err("sprt requires at least one slot".to_string());
    }

    run_sprt(&state, &settings);
    Ok(())
}

/*----------------------------------------------------------------------------*\
                                   DISPATCHER
\*----------------------------------------------------------------------------*/

/// run_debug_headless
///
/// The entry point of the headless tools. It reads the command word and
/// gives the rest of the line to that command.
///
/// - `derive`             : no arguments
/// - datagen, tune, sprt  : read their own arguments
/// - position commands    : `parse_position_arguments` reads the line
/// - help, `--help`, `-h` : print the usage text
///
/// The position commands are state, movegen, evaluate, see, search, play,
/// perft and bench.
///
/// Params:
/// - arguments: &[String] -> values after `debug-headless`
///
/// Notes:
/// Each command returns its error. The function prints the error and the
/// usage text. An empty line prints only the usage. The function clears
/// the interrupt flag first, so an earlier signal does not stop the
/// command.
///
#[hotpath::measure]
pub fn run_debug_headless(arguments: &[String]) {
    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let Some(command) = arguments.first().map(String::as_str) else {
        emit(EngineEvent::Print(headless_usage()));
        return;
    };

    let result = match command {
        "derive" => {
            if arguments.len() != 1 {
                Err("derive accepts no arguments".to_string())
            } else {
                run_derive_headless();
                Ok(())
            }
        }
        "datagen" => run_datagen_command(&arguments[1..]),
        "tune" => run_tune_command(&arguments[1..]),
        "sprt" => run_sprt_command(&arguments[1..]),
        "state" | "movegen" | "evaluate" | "see" | "search"
        | "play" | "perft" | "bench" => {
            parse_position_arguments(command, &arguments[1..]).and_then(
                |position| match command {
                    "state" => run_state_command(position),
                    "movegen" => run_movegen_command(position),
                    "evaluate" => run_evaluate_command(position),
                    "see" => run_see_command(position),
                    "search" => run_search_command(position),
                    "play" => run_play_command(position),
                    "perft" => run_perft_command(position),
                    "bench" => run_bench_command(position),
                    _ => unreachable!(),
                },
            )
        }
        "help" | "--help" | "-h" => {
            emit(EngineEvent::Print(headless_usage()));
            Ok(())
        }
        _ => Err(format!("Unknown headless command: {}", command)),
    };

    if let Err(error) = result {
        emit(EngineEvent::Print(format!(
            "Error: {}\n\n{}", error, headless_usage()
        )));
    }
}
