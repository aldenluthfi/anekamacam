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
/// One command's board, and everything read off the line beside it. Eight of
/// the commands want a position and all eight want it the same way, so the
/// line is read once, here, rather than eight times over.
///
/// - state           : the board, already at the asked-for position
/// - translator      : the notation to read and write in, where one was
///                     named
/// - variant         : its name, still wanted for suite and file lookups
/// - values          : the positional arguments, in the order they came
/// - custom_position : whether a FEN or a move list was given at all, as
///                     against the variant's own start position
/// - suite           : perft: run the embedded suite, not one position
/// - limit           : perft and bench: how many cases to get through
/// - branch          : perft: how many levels of the tree to name
///
/// The last three belong to one command each and are meaningless to the
/// rest, which is why they are options: `None` is not a default so much as
/// a flag that was never offered on that command's line.
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
/// Builds the help text, which is printed on `help`, on no command at all,
/// and after any error, so a mistyped line answers itself rather than only
/// saying no. The three position options sit in their own group because
/// they belong to eight commands at once and would be repeated eight times
/// otherwise.
///
/// Return:
/// String -> every command, flag, and argument the frontend takes
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

/// parse_position_arguments
///
/// Reads one position command's line and hands back the board it names. The
/// variant has to come first; everything after it may be written in any
/// order at all, flags and values interleaved as the typist pleases.
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
/// - `--protocol` : the dialect the FEN and the moves are written in
/// - `--fen`      : the position to start from, in place of the
///                  variant's own
/// - `--moves`    : moves to play onto it, taking every word up to the
///                  next flag, so a move list needs nothing to close it
/// - `--suite`    : perft alone
/// - `--limit`    : perft and bench
/// - `--branch`   : perft alone
///
/// A flag belonging to some other command is not quietly dropped. The three
/// narrow ones are guarded on the command name, so `--suite` written on a
/// `bench` line falls past its own arm into the unknown-option arm and is
/// named there, rather than being read and then never used.
///
/// The translator is put on the FEN only where a FEN was actually given.
/// The variant's own start position is already in the engine's notation,
/// and translating it as though it had come from the protocol would rewrite
/// a board nobody asked to change.
///
/// Params:
/// - command  : &str                -> which command's line is being read
/// - arguments: &[String]           -> everything after the command word
///
/// Return:
/// Result<HeadlessPosition, String> -> the loaded position, or the fault
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
/// Prints everything the engine believes about one position, and nothing
/// about what it would do with it.
///
/// - the board : drawn, with whatever the variant hangs off it
/// - FEN       : the same position written back out
/// - Hash      : the position hash, which repetition is judged on
/// - Keys      : the search and quiescence keys, which index the tables
/// - Result    : the outcome, Ongoing while it is
/// - Reason    : the rule that ended it, printed only once one has
///
/// Params:
/// - position: HeadlessPosition -> the position, and nothing besides
///
/// Return:
/// Result<(), String>           -> printed, or the value it refused
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
/// Prints the count of legal moves, then the moves themselves, one to a
/// line. They are sorted by their written form rather than left in the
/// order they were generated, so two builds can be compared line by line
/// and a difference in generation order is not read as a difference in the
/// moves that were found.
///
/// Params:
/// - position: HeadlessPosition -> the position, and nothing besides
///
/// Return:
/// Result<(), String>           -> printed, or the value it refused
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
/// Scores one position without searching it, from the side to move's point
/// of view. The phase is printed alongside because so much of the score
/// depends on it that the number is hard to read without it.
///
/// Params:
/// - position: HeadlessPosition -> the position, and nothing besides
///
/// Return:
/// Result<(), String>           -> printed, or the value it refused
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
/// Plays one capture's whole exchange out statically and reports what it
/// wins or loses. The move is required to be a capture rather than merely
/// accepted as one: a quiet move has no exchange to evaluate, and reporting
/// zero for it would read as a fair trade rather than as no trade at all.
///
/// Params:
/// - position: HeadlessPosition -> the position, and the one capture
///
/// Return:
/// Result<(), String>           -> printed, or why the move will not do
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
/// Searches one position to a fixed depth and says what it found.
///
/// - Best move : the move it settled on, or `(none)` where there was
///               none
/// - Score     : what that move is worth, from the side to move's view
/// - Nodes     : how many positions it looked at getting there
/// - Elapsed   : how long that took
///
/// The tables are built here, a megabyte apiece, and go away with the
/// command. A search that inherited another run's table would report a
/// different node count for the very same position, and the node count is
/// most of what anyone runs this command to compare.
///
/// Both figures have defaults, depth four on one thread, small enough to
/// answer at once and still deep enough to be a search.
///
/// Params:
/// - position: HeadlessPosition -> the position, its depth and threads
///
/// Return:
/// Result<(), String>           -> printed, or the limit it refused
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
/// Plays the engine against itself from the position and prints the game as
/// it goes, a move to a line, so a long one can be watched rather than only
/// waited on. The result and the final position follow it.
///
/// - depth     : how deep each move is searched
/// - time      : seconds a move; zero means depth is the only limit
/// - threads   : how many threads to search with, one by default
/// - max-plies : where to stop and leave it unfinished, 512 by default
///
/// The ply cap is nobody's rule. It is there because two copies of one
/// engine will shuffle forever in a variant that counts nothing, and the
/// command has to end whether or not the game does.
///
/// The two tables are made once and used all game, unlike `bench` where
/// each position gets its own. A self-play game is one game and its later
/// positions follow from its earlier ones, so carrying the table forward is
/// what an engine playing that game would actually do.
///
/// Params:
/// - position: HeadlessPosition -> the position, and the four limits
///
/// Return:
/// Result<(), String>           -> played, or the limit it refused
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
/// Counts the legal move tree, either from the one position on the line or
/// across the whole of the variant's embedded suite.
///
/// - `perft standard 5`                    : one position, divided at
///                                           the root
/// - `perft standard 5 --branch 2`         : the same, named two levels
///                                           deep
/// - `perft standard 5 --suite`            : every case in
///                                           `standard.perft`
/// - `perft standard 5 --suite --limit 20` : the first twenty of those
///
/// The suite takes neither a FEN nor a move list: its cases carry their own
/// positions, and one given on the line would be thrown away by the first
/// of them without a word. Its depths run one through six, which is as far
/// as the embedded cases are counted.
///
/// `--limit` without `--suite` is refused rather than ignored, there being
/// no list of cases to cut short when only one position is being counted.
///
/// Params:
/// - position: HeadlessPosition -> the position, and the perft options
///
/// Return:
/// Result<(), String>           -> counted, or the option it refused
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
/// Wanders a few moves out from the variant's start position and hands back
/// where it landed. The bench wants more positions than most variants keep
/// perft cases for, and these make up the difference.
///
/// Random here means fixed. Nothing about a walk comes from the clock or
/// the machine, only from the seed and which walk it is:
///
/// - length : 6 + index mod 10 plies, so walks end at differing depths
/// - mixing : seed xor index · 0x9E3779B97F4A7C15, then one linear
///            congruential step per ply
/// - choice : over the legal moves sorted by written form, and never in
///            the order they were generated
///
/// That sort is what makes a walk mean the same thing twice. Two builds
/// that find the same moves in a different order would otherwise wander to
/// different positions, and their bench figures would be measuring two
/// different searches rather than two different builds.
///
/// A walk stops short where the move it drew ends the game, backing that
/// move out first: a terminal position has nothing left to search.
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
/// Searches a fixed set of positions to a fixed depth and reports how fast
/// it got through them. One line a position, then one line for the lot:
///
/// ```text
/// position 001 nodes       412988 time_ns      93214000 nps   4430536
/// position 002 nodes       938114 time_ns     201880000 nps   4646887
/// total nodes 1351102 time_ns 295094000 nps 4578549
/// ```
///
/// The positions come out of the variant's perft suite, skipping cases that
/// count no nodes at all, and are made up to the limit with random walks
/// where the suite is the shorter of the two. Sixteen by default.
///
/// Everything else is pinned so that two runs differ only where the builds
/// do: one thread, the same depth, and a fresh transposition and quiescence
/// table for every position. A position handed the last one's table would
/// come out faster for a reason that has nothing to do with the build.
///
/// A FEN or a move list is refused. The point of the command is that
/// everyone runs the same positions, and a given one would not be.
///
/// Params:
/// - position: HeadlessPosition -> the depth and the case limit
///
/// Return:
/// Result<(), String>           -> benched, or the argument it refused
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
/// Reads the arguments for a run of self-play data generation and starts
/// one. The generating is `run_datagen`'s work; the reading is all that
/// happens here.
///
/// - variant  : which variant the games are played in
/// - games    : how many to play, counted across every thread
/// - movetime : milliseconds a move, the same for both sides
/// - threads  : how many games are played at once, one by default
///
/// None of the three numbers may be zero. Zero of any of them describes a
/// run that would produce nothing, and refusing it reads better than
/// starting a job that finishes at once and writes an empty file.
///
/// Params:
/// - arguments: &[String] -> the variant and the three limits
///
/// Return:
/// Result<(), String>     -> started, or the argument it refused
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
/// Reads the arguments for a tuning run and starts one over whatever data
/// generation has already written for that variant.
///
/// - variant       : whose parameters are being fitted
/// - epochs        : how many passes over the data are made
/// - learning rate : how far a pass may move them, 1.0 by default
///
/// Both numbers have to be above zero. No epochs is no fitting, and a rate
/// of zero fits with steps of no length, which is the same thing said in a
/// way that takes longer to say.
///
/// Params:
/// - arguments: &[String] -> the variant, the epochs, and the rate
///
/// Return:
/// Result<(), String>     -> started, or the argument it refused
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
/// Reads the arguments for a match between two builds and starts it. The
/// two binaries are named by path, and either may be the one that is
/// running: the test is between whatever is at those two paths.
///
/// - variant  : the variant both of them are made to play
/// - binary a : the build under test
/// - binary b : the build it is being measured against
/// - control  : 1000 for a fixed movetime, 8000+80 for a clock
/// - games    : where to give up if neither bound is reached, 2000
/// - h0       : the Elo the test tries to reject, 0.0
/// - h1       : the Elo it tries to accept, 5.0
///
/// Two games is the floor. The test plays colours in pairs so that a lucky
/// opening cannot be handed to one side alone, and a single game is half a
/// pair.
///
/// Params:
/// - arguments: &[String] -> the pairing, the control, and the bounds
///
/// Return:
/// Result<(), String>     -> started, or the argument it refused
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
/// The way in. Takes the rest of the command line, picks the command off
/// the front of it, and hands the remainder to whoever owns that command.
///
/// - `derive`             : no arguments at all
/// - datagen, tune, sprt  : read their own arguments
/// - the eight positional : one shared read, by
///                          `parse_position_arguments`
/// - help, `--help`, `-h` : the usage text, and nothing else
///
/// The eight positional commands are state, movegen, evaluate, see,
/// search, play, perft, and bench.
///
/// Nothing raises here. Every command reports what went wrong by returning
/// it, and the last thing this does is print that alongside the usage: a
/// caller who got the line wrong wants to see the right line, not only that
/// theirs was not it. A line with no command at all prints the same usage
/// without calling it an error, since asking is not a mistake.
///
/// The interrupt flag is cleared on the way in. One command having been cut
/// short by a signal is no reason for the next one to refuse to start.
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
