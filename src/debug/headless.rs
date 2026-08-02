//! headless.rs
//!
//! Non-graphical debug command dispatcher for engine inspection and tooling.
//! Keeps perft, bench, search, evaluation, self-play, parameter work, dataset
//! generation, tuning, and SPRT under one nested one-shot CLI frontend.
//!
//! Created: 30/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              COMMAND REPRESENTATION
\*----------------------------------------------------------------------------*/

/// HeadlessPosition
///
/// Loaded position and parsed values shared by position-based commands.
/// Keeps common command state together after argument parsing finishes.
struct HeadlessPosition {
    state: State,
    translator: Option<Translator>,
    variant: String,
    values: Vec<String>,
    custom_position: bool,
    suite: bool,
    limit: Option<usize>,
    branch: Option<i8>,
}

/*----------------------------------------------------------------------------*\
                                ARGUMENT PARSING
\*----------------------------------------------------------------------------*/

/// headless_usage
///
/// Builds command and position-option help for the nested headless CLI.
/// Covers every command, shared flag, and positional argument.
///
/// Return:
/// String -> complete headless command usage
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
        "  sprt <variant> <bin-a> <bin-b> <ms|base+inc> [games] [h0] [h1]\n\n",

        "Position options:\n",
        "  --protocol <uci|usi|ucci>\n",
        "  --fen \"<position>\"\n",
        "  --moves <move>...\n",
    ]
    .concat()
}

/// parse_number
///
/// Parses one optional positional value with a supplied default.
/// Reports invalid values using the supplied diagnostic field name.
///
/// Params:
/// - values : &[String] -> positional values
/// - index  : usize     -> value index
/// - default: T         -> fallback when absent
/// - name   : &str      -> field name for diagnostics
///
/// Return:
/// Result<T, String>    -> parsed/default value or diagnostic
fn parse_number<T>(
    values: &[String],
    index: usize,
    default: T,
    name: &str,
) -> Result<T, String>
where
    T: std::str::FromStr,
{
    values
        .get(index)
        .map(|value| {
            value
                .parse::<T>()
                .map_err(|_| format!("Invalid {}: {}", name, value))
        })
        .unwrap_or(Ok(default))
}

/// load_variant
///
/// Loads one embedded variant through the normal configuration pipeline.
/// Returns a diagnostic when its configuration cannot be found.
///
/// Params:
/// - variant: &str       -> embedded configuration stem
///
/// Return:
/// Result<State, String> -> loaded state or unknown-variant diagnostic
fn load_variant(variant: &str) -> Result<State, String> {
    let config_name = format!("{}.conf", variant);
    if EMBEDDED_CONFIGS.get_file(&config_name).is_none() {
        return Err(format!("Unknown variant: {}", variant));
    }

    Ok(parse_config_file(&config_name))
}

/// parse_position_arguments
///
/// Parses variant, command values, protocol, quoted FEN, move list, and perft
/// options. Flags may follow values; `--moves` consumes through the next flag.
///
/// Params:
/// - command  : &str                -> position command name
/// - arguments: &[String]           -> values after the command name
///
/// Return:
/// Result<HeadlessPosition, String> -> loaded position or diagnostic
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
/// Prints board, FEN, game result, and active termination reason.
/// Rejects extra positional values before producing state output.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position
///
/// Return:
/// Result<(), String>           -> success or argument diagnostic
fn run_state_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    if !position.values.is_empty() {
        return Err("state accepts no positional values".to_string());
    }

    let (result, reason) = game_outcome(&mut position.state);
    let mut output = format!(
        "{}\nFEN: {}\nHash: {}\nResult: {}\n",
        format_game_state(&position.state),
        format_fen(&position.state, position.translator.as_ref()),
        format_position_hash(&position.state),
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
/// Generates, formats, sorts, and prints every legal move.
/// Uses the selected protocol translator when formatting move names.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position
///
/// Return:
/// Result<(), String>           -> success or argument diagnostic
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
/// Evaluates one position from side-to-move perspective.
/// Prints variant name, game phase, and resulting centipawn score.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position
///
/// Return:
/// Result<(), String>           -> success or argument diagnostic
fn run_evaluate_command(
    position: HeadlessPosition,
) -> Result<(), String> {
    if !position.values.is_empty() {
        return Err("evaluate accepts no positional values".to_string());
    }

    let pawn_table = PTable::with_mb(1);
    let mut buffers = SearchBufs::default();
    let score = evaluate_position!(
        &position.state,
        &mut buffers,
        &pawn_table
    );

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
/// Parses one capture and reports static exchange evaluation.
/// Rejects invalid moves and legal non-captures before calculation.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position and move
///
/// Return:
/// Result<(), String>           -> success or move diagnostic
fn run_see_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    let move_text = position
        .values
        .first()
        .ok_or_else(|| "see requires one move".to_string())?;
    if position.values.len() > 1 {
        return Err("see accepts one move".to_string());
    }

    let state = &mut position.state;
    let candidate = parse_move(
        move_text,
        state,
        position.translator.as_ref(),
    )
    .ok_or_else(|| format!("Invalid move: {}", move_text))?;
    if !matches!(
        move_type!(candidate),
        SINGLE_CAPTURE_MOVE | MULTI_CAPTURE_MOVE
    ) {
        return Err("see requires a capture move".to_string());
    }

    let formatted = format_move(
        &candidate,
        state,
        position.translator.as_ref(),
    );
    let mut moves = Vec::with_capacity(32);
    let mut scratch = Vec::with_capacity(32);
    let score = see!(state, &candidate, &mut moves, &mut scratch);

    emit(EngineEvent::Print(format!(
        "SEE {}: {}\n", formatted, score
    )));
    Ok(())
}

/// run_search_command
///
/// Runs fixed-depth search with isolated one-megabyte tables.
/// Validates depth and thread limits before dispatching the search.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position and limits
///
/// Return:
/// Result<(), String>           -> success or limit diagnostic
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
    let ptable = Arc::new(PTable::with_mb(1));
    let mut information = SearchInfo {
        set_depth: depth,
        ..Default::default()
    };
    let mut buffers = SearchBufs::default();
    let state = &mut position.state;
    let result = search_position(
        state,
        ttable,
        qtable,
        ptable,
        &mut information,
        &mut buffers,
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
/// Plays both sides until termination, interrupt, or requested ply cap.
/// Uses supplied depth, move time, thread count, and maximum plies.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position and limits
///
/// Return:
/// Result<(), String>           -> success or game diagnostic
fn run_play_command(
    mut position: HeadlessPosition,
) -> Result<(), String> {
    let depth = parse_number(&position.values, 0, 0usize, "depth")?;
    let time_seconds = parse_number(&position.values, 1, 0.0f64, "time")?;
    let threads = parse_number(&position.values, 2, 1usize, "threads")?;
    let max_plies = parse_number(
        &position.values,
        3,
        512usize,
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
    let ptable = Arc::new(PTable::with_mb(1));
    let time_limit_ns =
        (time_seconds * 1_000_000_000.0) as u128;
    let translator = position.translator.as_ref();
    let (result, reason) = play_search_game(
        &mut position.state,
        ttable,
        qtable,
        ptable,
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
/// Runs direct perft with root divide by default, or an embedded suite when
/// `--suite` is present.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position and perft options
///
/// Return:
/// Result<(), String>           -> success or perft diagnostic
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
/// Builds one deterministic random-walk position from the variant start
/// position. Moves are drawn through a fixed linear congruential generator
/// over the sorted formatted legal moves, so the walk depends only on the
/// seed, the walk index, and the legal move set — never on move generation
/// order. Walks stop before entering a terminal position.
///
/// Params:
/// - state: &mut State -> variant state, reused for every walk
/// - seed : u64        -> mixing seed shared across compared binaries
/// - index: usize      -> walk number driving ply count and mixing
///
/// Return:
/// String              -> FEN of the reached non-terminal position
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
/// Searches a fixed position set at fixed depth on one thread with fresh
/// tables per position: live suite positions first, deterministic
/// random-walk positions when the suite runs short. Prints one
/// nodes/time/speed line per position plus a machine-readable totals line
/// for speed-suite tooling.
///
/// Params:
/// - position: HeadlessPosition -> loaded command position and options
///
/// Return:
/// Result<(), String>           -> success or argument diagnostic
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
        let ptable = Arc::new(PTable::with_mb(1));
        let mut information = SearchInfo {
            set_depth: depth,
            ..Default::default()
        };
        let mut buffers = SearchBufs::default();
        let start = ENGINE_START.elapsed().as_nanos();
        let result = search_position(
            &mut position.state,
            ttable,
            qtable,
            ptable,
            &mut information,
            &mut buffers,
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
/// Parses nested datagen arguments and starts existing generator.
/// Requires positive game, move-time, and thread values.
///
/// Params:
/// - arguments: &[String] -> values after `datagen`
///
/// Return:
/// Result<(), String>     -> success or argument diagnostic
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
        Arc::new(PTable::default()),
        threads,
        games,
        movetime,
    );
    Ok(())
}

/// run_tune_command
///
/// Parses nested tuning arguments and starts existing tuner.
/// Requires positive epoch and learning-rate values.
///
/// Params:
/// - arguments: &[String] -> values after `tune`
///
/// Return:
/// Result<(), String>     -> success or argument diagnostic
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
/// Parses nested SPRT arguments and starts existing match runner.
/// Accepts optional game-count and Elo-hypothesis values.
///
/// Params:
/// - arguments: &[String] -> values after `sprt`
///
/// Return:
/// Result<(), String>     -> success or argument diagnostic
fn run_sprt_command(arguments: &[String]) -> Result<(), String> {
    if arguments.len() < 4 || arguments.len() > 7 {
        return Err(
            "sprt requires variant, two binaries, and time control".to_string()
        );
    }

    let variant = &arguments[0];
    let state = load_variant(variant)?;
    let time_control = parse_sprt_time_control(&arguments[3])?;
    let max_games = parse_number(arguments, 4, 2000usize, "games")?;
    let h0 = parse_number(arguments, 5, 0.0f64, "h0")?;
    let h1 = parse_number(arguments, 6, 5.0f64, "h1")?;
    if max_games < 2 {
        return Err("sprt requires at least two games".to_string());
    }

    run_sprt(
        &state,
        variant,
        &arguments[1],
        &arguments[2],
        time_control,
        max_games,
        h0,
        h1,
    );
    Ok(())
}

/*----------------------------------------------------------------------------*\
                                  DISPATCHER
\*----------------------------------------------------------------------------*/

/// run_debug_headless
///
/// Dispatches nested non-graphical debug commands. Errors print usage and
/// return cleanly.
///
/// Params:
/// - arguments: &[String] -> values after `debug-headless`
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
