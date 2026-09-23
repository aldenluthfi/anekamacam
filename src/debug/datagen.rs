//! datagen.rs
//!
//! Generates self-play training data for Texel tuning.
//!
//! The engine plays against itself with a fixed time for each move. Each
//! game starts with some random moves. The file records the quiet positions
//! of completed games, with a game ID and the result for White. `tuning.rs`
//! uses the game IDs to keep training and validation games separate.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                SELF-PLAY GAMES
\*----------------------------------------------------------------------------*/

/// GeneratedGame
///
/// One completed game with only the data that the tuner uses. All quiet
/// positions of the game get the same result.
///
struct GeneratedGame {
    positions: Vec<String>,                                                     /* the quiet ones, in order           */
    result: f64,                                                                /* 1.0, 0.5, or 0.0, White's view     */
}

/// play_one_game
///
/// Plays one self-play game and keeps the quiet positions. The game starts
/// with some random plies, so the games are different.
///
/// A position is quiet when no side is in check and the selected move is
/// not a capture. Other positions depend on tactics, so the tuner must not
/// use them.
///
/// - rules end the game  : keep, with the rule result
/// - no move, no stop    : keep, with the adjudicated result
/// - interrupted search  : remove the full game
/// - move does not apply : remove the full game
///
/// The function sends each board to the event sink, so a user interface
/// can show the game.
///
/// Params:
/// - template   : &State              -> loaded variant to start games from
/// - dict       : Option<&Translator> -> translator for search move logs
/// - ttable     : Arc<TTable>         -> shared main table for the searches
/// - qtable     : Arc<QTable>         -> shared quiescence table
/// - threads    : usize               -> number of workers for each search
/// - movetime_ms: u128                -> fixed time for each move
///
/// Return:
/// Option<GeneratedGame>              -> completed game, or None if removed
///
fn play_one_game(
    template: &State,
    dict: Option<&Translator>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    threads: usize,
    movetime_ms: u128,
) -> Option<GeneratedGame> {
    let state = &mut template.fork();
    state.play_random_opening(OPENING_RANDOM_PLIES);

    let mut fens = Vec::new();
    let mut info = SearchInfo {
        set_depth: MAX_DEPTH,
        ..Default::default()
    };
    let movetime_ns = movetime_ms * 1_000_000;

    loop {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            return None;
        }

        let terminal = game_outcome(state).0;
        if terminal != ONGOING {
            return Some(GeneratedGame {
                positions: fens,
                result: game_result_score(terminal),
            });
        }

        let now = ENGINE_START.elapsed().as_nanos();
        info.deadline = now + movetime_ns;

        let outcome = search_position(
            state, Arc::clone(&ttable), Arc::clone(&qtable),
            &mut info, threads, dict,
        );

        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            return None;
        }

        if outcome.best_move == null_move() && info.interrupt {
            return None;
        }
        if outcome.best_move == null_move() || outcome.best_score == -INF {
            let result = adjudicate_no_move(state);
            return Some(GeneratedGame {
                positions: fens,
                result: game_result_score(result),
            });
        }

        let is_capture = matches!(
            move_type!(outcome.best_move),
            SINGLE_CAPTURE_MOVE | MULTI_CAPTURE_MOVE
        );

        if !is_capture && !is_in_check!(state.playing, state) {
            fens.push(format_fen(state, None));
        }

        if !make_move!(state, outcome.best_move) {
            return None;
        }

        emit(EngineEvent::Board(BoardState::from_state(state, dict)));
    }
}

/*----------------------------------------------------------------------------*\
                                 DATASET OUTPUT
\*----------------------------------------------------------------------------*/

/// run_datagen
///
/// Plays the self-play games and writes one row for each position. The
/// row has this format:
///
/// ```text
/// 12;8/8/4k3/8/4K3/8/8/8 w - - 0 1;0.5
/// ^  ^                             ^
/// |  |                             how the game ended
/// |  the position, as a FEN
/// which game it came out of
/// ```
///
/// The tuner uses the game number to put full games in the validation set.
/// Thus two positions of one game are never in different sets.
///
/// The output file is `res/data/{variant}/latest.data`. The function first
/// renames the old file with a timestamp. It writes the rows of a game
/// only after the game ends, so an interrupted run has no partial game.
/// Each ten games, it flushes the file and logs the count.
///
/// Params:
/// - template   : &State              -> loaded variant to start games from
/// - variant    : &str                -> variant name, selects the data dir
/// - dict       : Option<&Translator> -> translator for search move logs
/// - ttable     : Arc<TTable>         -> shared main table for the searches
/// - qtable     : Arc<QTable>         -> shared quiescence table
/// - threads    : usize               -> number of workers for each search
/// - games      : usize               -> number of self-play games
/// - movetime_ms: u128                -> fixed time for each move
///
pub fn run_datagen(
    template: &State,
    variant: &str,
    dict: Option<&Translator>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    threads: usize,
    games: usize,
    movetime_ms: u128,
) {
    let dir = format!("{}/{}", DATA_DIR, variant);
    fs::create_dir_all(&dir).unwrap_or_else(|e| {
        panic!("Failed to create dataset directory {}: {}", dir, e)
    });

    let path = format!("{}/latest.data", dir);
    roll_latest(&dir, "", "data");

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .unwrap_or_else(|e| {
            panic!("Failed to open dataset file {}: {}", path, e)
        });

    let mut total_rows = 0usize;
    let mut played = 0usize;
    let mut discarded = 0usize;

    for game_index in 0..games {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            log_2!("Datagen interrupted after {} games", played);
            break;
        }

        let Some(game) = play_one_game(
            template, dict, Arc::clone(&ttable), Arc::clone(&qtable),
            threads, movetime_ms,
        ) else {
            discarded += 1;
            if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
                break;
            }
            continue;
        };

        for fen in &game.positions {
            writeln!(file, "{};{};{}", game_index, fen, game.result)
                .unwrap_or_else(|error| {
                    panic!(
                        "Failed to write dataset row to {}: {}", path, error
                    )
                });
        }

        total_rows += game.positions.len();
        played += 1;

        if (game_index + 1) % 10 == 0 {
            file.flush().unwrap_or_else(|e| {
                panic!("Failed to flush dataset file {}: {}", path, e)
            });
            log_1!(
                "Datagen: {}/{} games, {} positions",
                game_index + 1, games, total_rows,
            );
        }
    }

    file.flush().unwrap_or_else(|e| {
        panic!("Failed to flush dataset file {}: {}", path, e)
    });

    log_1!(
        "Datagen complete: {} games, {} discarded, {} positions -> {}",
        played, discarded, total_rows, path,
    );
}
