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

/// GeneratedGame
///
/// One completed self-play result and every quiet position collected from it.
struct GeneratedGame {
    positions: Vec<String>,
    result: f64,
}

/// play_one_game
///
/// Plays a single self-play game to completion and collects the FEN of
/// every quiet position it passed through. A position is quiet when the
/// side to move is not in check and the move the engine chose there is
/// not a capture, keeping the static evaluation a faithful label target.
/// Completed mate and draw-rule results use White's perspective (1.0 win,
/// 0.5 draw, 0.0 loss); interrupted or invalid games return None.
///
/// Params:
/// - template   : &State              -> loaded variant to start games from
/// - dict       : Option<&Translator> -> translator for search move-name logs
/// - ttable     : Arc<TTable>         -> shared main table for the searches
/// - qtable     : Arc<QTable>         -> shared quiescence table
/// - ptable     : Arc<PTable>         -> shared pawn structure table
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
    ptable: Arc<PTable>,
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
    let mut bufs = SearchBufs::default();
    let movetime_ns = movetime_ms * 1_000_000;

    loop {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            return None;
        }

        if state.game_over {
            let result = if is_in_check!(state.playing, state)
            || stalemate_loss!(&state)
            {
                state.playing as f64
            } else {
                0.5
            };

            return Some(GeneratedGame { positions: fens, result });
        }

        let now = ENGINE_START.elapsed().as_nanos();
        info.soft_deadline = now + movetime_ns;
        info.hard_deadline = now + movetime_ns;

        let outcome = search_position(
            state, Arc::clone(&ttable), Arc::clone(&qtable),
            Arc::clone(&ptable), &mut info, &mut bufs, threads, dict,
        );

        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            return None;
        }

        if outcome.best_move == null_move() || outcome.best_score == -INF {
            return Some(GeneratedGame {
                positions: fens,
                result: state.playing as f64,
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

/// run_datagen
///
/// Console entry point for the `datagen` command. Plays `games`
/// self-play games on the loaded variant and writes each game's quiet
/// positions, labelled with its White-view result, to a fresh
/// `res/data/{variant}/latest.data` — any dataset from a previous run is
/// first rolled to a numbered backup, mirroring the parameter export.
/// Completed games write `game;FEN;result` rows atomically from their buffered
/// positions; interrupted or invalid games write nothing.
///
/// Params:
/// - template   : &State              -> loaded variant to generate games from
/// - variant    : &str                -> variant name, selects the dataset dir
/// - dict       : Option<&Translator> -> translator for search move-name logs
/// - ttable     : Arc<TTable>         -> shared main table for the searches
/// - qtable     : Arc<QTable>         -> shared quiescence table
/// - ptable     : Arc<PTable>         -> shared pawn structure table
/// - threads    : usize               -> worker count per search
/// - games      : usize               -> number of self-play games to play
/// - movetime_ms: u128                -> fixed wall-clock budget per move
pub fn run_datagen(
    template: &State,
    variant: &str,
    dict: Option<&Translator>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    ptable: Arc<PTable>,
    threads: usize,
    games: usize,
    movetime_ms: u128,
) {
    let dir = format!("{}/{}", DATA_DIR, variant);
    fs::create_dir_all(&dir).unwrap_or_else(|e| {
        panic!("Failed to create dataset directory {}: {}", dir, e)
    });

    let path = format!("{}/latest.data", dir);
    roll_latest(&dir, "data");

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
            Arc::clone(&ptable), threads, movetime_ms,
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
