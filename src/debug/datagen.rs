//! datagen.rs
//!
//! Self-play training-data generation for Texel tuning.
//!
//! Plays the engine against itself under a fixed per-move wall-clock
//! budget, opening each game with a short random line for variety, and
//! records quiet positions from completed games with game IDs and final
//! White-view results. `tuning.rs` uses those IDs for game-disjoint training
//! and validation sets.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                SELF-PLAY GAMES
\*----------------------------------------------------------------------------*/

/// GeneratedGame
///
/// One finished game, cut down to what a tuner has any use for: the quiet
/// positions it went through, and how it ended. Every position in a game
/// carries that one result, which is what makes a game the unit here
/// rather than a position.
struct GeneratedGame {
    positions: Vec<String>,                                                     /* the quiet ones, in order           */
    result: f64,                                                                /* 1.0, 0.5, or 0.0, White's view     */
}

/// play_one_game
///
/// Plays one game out against itself and keeps the positions worth
/// learning from. It opens on a few random plies, so that a thousand games
/// are not one game played a thousand times.
///
/// A position is kept when it is quiet: nobody in check, and the move the
/// engine picked there taking nothing. The rest are positions whose worth
/// hangs on an exchange that has not happened yet, and an evaluation
/// fitted against those is being taught to guess at tactics.
///
/// ```text
/// the rules ended it            kept, scored by the result
/// no move, and no interrupt     kept, scored by adjudication
/// interrupted mid-search        dropped, the game is unfinished
/// a move that would not make    dropped, the same way
/// ```
///
/// A dropped game is dropped whole. A position labelled with a result
/// nobody ever reached is worse than a position nobody recorded.
///
/// Every move made is announced, so an interface watching along can show
/// the game being played rather than only its count.
///
/// Params:
/// - template   : &State              -> loaded variant to start games from
/// - dict       : Option<&Translator> -> translator for search move-name logs
/// - ttable     : Arc<TTable>         -> shared main table for the searches
/// - qtable     : Arc<QTable>         -> shared quiescence table
/// - threads    : usize               -> worker count per search
/// - movetime_ms: u128                -> fixed wall-clock budget per move
///
/// Return:
/// Option<GeneratedGame> -> completed labelled game, or None when interrupted
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
/// Plays the games and writes them down, a row to a position:
///
/// ```text
/// 12;8/8/4k3/8/4K3/8/8/8 w - - 0 1;0.5
/// ^  ^                             ^
/// |  |                             how the game ended
/// |  the position, as a FEN
/// which game it came out of
/// ```
///
/// The game number is what keeps the tuning honest. It lets the tuner
/// hold out whole games rather than scattered positions, so that a
/// position and the position after it never sit on opposite sides of the
/// split, telling the validation set what the training set already knows.
///
/// Each run writes `res/data/{variant}/latest.data`, rolling whatever was
/// there to a timestamped name first, the same way exported parameters
/// are kept. A game's rows are written once it has finished, so a run cut
/// short leaves whole games behind it rather than half of one.
///
/// The file is flushed and the count logged every ten games, which is
/// often enough to watch a long run and rarely enough not to slow it.
///
/// Params:
/// - template   : &State              -> loaded variant to generate games from
/// - variant    : &str                -> variant name, selects the dataset dir
/// - dict       : Option<&Translator> -> translator for search move-name logs
/// - ttable     : Arc<TTable>         -> shared main table for the searches
/// - qtable     : Arc<QTable>         -> shared quiescence table
/// - threads    : usize               -> worker count per search
/// - games      : usize               -> number of self-play games to play
/// - movetime_ms: u128                -> fixed wall-clock budget per move
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
