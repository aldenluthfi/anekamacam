//! # datagen.rs
//!
//! Self-play training-data generation for Texel tuning.
//!
//! Plays the engine against itself under a fixed per-move wall-clock
//! budget, opening each game with a short random line for variety, and
//! records the quiet positions it visits — labelled with the final game
//! result from White's perspective — into a reusable on-disk dataset
//! that `tuning.rs` later optimises the evaluation against.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 05/07/2026

use crate::*;

/// play_one_game
///
/// Plays a single self-play game to completion and collects the FEN of
/// every quiet position it passed through. A position is quiet when the
/// side to move is not in check and the move the engine chose there is
/// not a capture, keeping the static evaluation a faithful label target.
/// The game ends on mate, a draw rule, or the ply cap; the result is
/// returned from White's perspective (1.0 win, 0.5 draw, 0.0 loss).
///
/// Params:
/// - template: &State          -> loaded variant to start games from
/// - dict: Option<&Translator> -> translator for search move-name logs
/// - ttable: Arc<TTable>       -> shared main table for the searches
/// - qtable: Arc<QTable>       -> shared quiescence table
/// - threads: usize            -> worker count per search
/// - movetime_ms: u128         -> fixed wall-clock budget per move
/// - sender: &Sender<TuiEvent> -> channel for live board updates
///
/// Return:
/// (Vec<String>, f64) -> quiet-position FENs and the White-view result
///
fn play_one_game(
    template: &State,
    dict: Option<&Translator>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    threads: usize,
    movetime_ms: u128,
    sender: &Sender<TuiEvent>,
) -> (Vec<String>, f64) {
    let state = &mut template.fork();
    state.play_random_opening(OPENING_RANDOM_PLIES);

    let mut fens = Vec::new();
    let mut info = SearchInfo {
        set_depth: MAX_DEPTH,
        ..Default::default()
    };
    let mut bufs = SearchBufs::default();
    let movetime_ns = movetime_ms * 1_000_000;

    let result;

    loop {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) || state.game_over {

            result = if is_in_check!(state.playing, state)
            || stalemate_loss!(&state)
            {
                state.playing as f64
            } else {
                0.5
            };

            break;
        }

        let now = ENGINE_START.elapsed().as_nanos();
        info.soft_deadline = now + movetime_ns;
        info.hard_deadline = now + movetime_ns;

        let outcome = search_position(
            state, Arc::clone(&ttable), Arc::clone(&qtable),
            &mut info, &mut bufs, threads, dict,
        );

        if outcome.best_move == null_move() || outcome.best_score == -INF {
            result = state.playing as f64;
            break;
        }

        let is_capture = matches!(
            move_type!(outcome.best_move),
            SINGLE_CAPTURE_MOVE | MULTI_CAPTURE_MOVE
        );

        if !is_capture && !is_in_check!(state.playing, state) {
            fens.push(format_fen(&state, None));
        }

        make_move!(state, outcome.best_move);

        sender.send(TuiEvent::StateUpdate(
            BoardState::from_state(&state, dict)
        )).unwrap_or_else(|e| {
            panic!("Failed to send TuiEvent::StateUpdate: {e}")
        });
    }

    (fens, result)
}

/// run_datagen
///
/// Console entry point for the `datagen` command. Plays `games`
/// self-play games on the loaded variant and writes each game's quiet
/// positions, labelled with its White-view result, to a fresh
/// `res/data/{variant}/latest.data` — any dataset from a previous run is
/// first rolled to a numbered backup, mirroring the parameter export.
/// Rows are flushed and progress is logged periodically, and the loop
/// stops early on interrupt so a long run is never lost.
///
/// Params:
/// - template: &State          -> loaded variant to generate games from
/// - variant: &str             -> variant name, selects the dataset dir
/// - dict: Option<&Translator> -> translator for search move-name logs
/// - ttable: Arc<TTable>       -> shared main table for the searches
/// - qtable: Arc<QTable>       -> shared quiescence table
/// - threads: usize            -> worker count per search
/// - games: usize              -> number of self-play games to play
/// - movetime_ms: u128         -> fixed wall-clock budget per move
/// - sender: &Sender<TuiEvent> -> channel for live board updates
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
    sender: &Sender<TuiEvent>,
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

    for game_index in 0..games {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            log_2!("Datagen interrupted after {} games", played);
            break;
        }

        let (fens, result) = play_one_game(
            template, dict, Arc::clone(&ttable), Arc::clone(&qtable),
            threads, movetime_ms, sender,
        );

        for fen in &fens {
            writeln!(file, "{};{}", fen, result).unwrap_or_else(|e| {
                panic!("Failed to write dataset row to {}: {}", path, e)
            });
        }

        total_rows += fens.len();
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
        "Datagen complete: {} games, {} positions -> {}",
        played, total_rows, path,
    );
}
