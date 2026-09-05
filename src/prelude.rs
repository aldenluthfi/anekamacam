//! prelude.rs
//!
//! Project-wide prelude for the anekamacam engine.
//!
//! This file re-exports the most commonly used types, macros, and functions
//! from the project for convenient use in all modules. Import this prelude
//! to avoid repetitive imports and enable ergonomic access to core engine
//! functionality. To use it, add `use crate::*;` at the top of the module.
//!
//! Created: 25/02/2026
//! Author : Alden Luthfi

/*----------------------------------------------------------------------------*\
                              CORE REPRESENTATIONS
\*----------------------------------------------------------------------------*/
pub use crate::game::representations::{
    board::{Board, BoardBits},
    drop::DropSet,
    termination::{
        Adjudicate, Checks, Counter, Counting, Termination, Extinct, Goal,
        Outcome, Perpetual, Repetition,
    },
    moves::{AttackMask, Move, MoveSignature, PseudoMove},
    pattern::{
        Pattern, PatternAllower, PatternSet, PatternStopper, PatternUnit,
        PieceSet,
    },
    piece::{Piece, PieceIndex},
    state::{
        EnPassantSquare, NodeLists, PawnEntry, Scratch, Snapshot, Square,
        State,
    },
    vector::{
        AtomicElement::{self, AtomicEval, AtomicExpr, AtomicTerm},
        AtomicGroup, AtomicVector, Leg, LegVector, MoveSet, MoveVector,
        MultiLegElement::{
            self, MultiLegEval, MultiLegExpr, MultiLegSlashExpr,
            MultiLegTerm,
        },
        MultiLegGroup, MultiLegVector,
        Token::{
            self, AtomicToken, BracketToken, CardinalToken, ColonToken,
            DotsToken, ExclusionToken, FilterToken, LegToken,
            MoveModifierToken, RangeToken, SlashBracketToken,
        },
    },
};

/*----------------------------------------------------------------------------*\
                                GAME LOGIC API
\*----------------------------------------------------------------------------*/
pub use crate::game::moves::drop_list::generate_relevant_drops;
pub use crate::game::moves::drop_parse::generate_drop_vectors;
pub use crate::game::moves::move_list::{
    generate_all_captures, generate_all_moves_and_drops,
    generate_attack_masks,
    generate_relevant_captures, generate_relevant_castling,
    generate_relevant_moves,
};
pub use crate::game::representations::termination::{
    adjudicate_outcome, count_repetitions, counting_limit, counting_progress,
    game_outcome, position_terminal, repetition_outcome, side_is_bare,
};
pub use crate::game::moves::move_parse::generate_move_vectors;

pub use crate::game::moves::pattern_parse::{
    generate_relevant_stand_offs, parse_pattern,
};
pub use crate::game::position::{
    hash::{
        hash_pawns, hash_position, hash_virgin_board, qsearch_key, search_key,
        PositionHash,
    },
    search::{
        alpha_beta, check_interrupt, clear_search, iterative_deepening,
        log_table_stats, search_position, SearchInfo, SearchResult,
    },
};
pub use crate::game::search::{
    parallel::ThreadPool,
    parameters::{
        derive_advantage_parameters, derive_base_pst, derive_danger_parameters,
        derive_eval_parameters, derive_eval_products, derive_parameters,
        derive_pawn_parameters, derive_search_capabilities,
        derive_search_parameters, derive_shelter_parameters,
        reduction_surface, EvalParams, SearchParams,
    },
    transposition::{HashEntry, HashTable, PTable, PTEntry, QTable, TTable},
};

pub use crate::game::util::{
    adjudicate_no_move, benchmark_headless_perft, benchmark_perft,
    exe_tag, format_time, game_result_score,
    load_variant, parse_perft_content, parse_number, perft,
    play_search_game, prune_backups, random_u128, refresh_eval_state,
    roll_latest, run_derive_headless, square_distance, verify_game_state,
};

/*----------------------------------------------------------------------------*\
                                   IO API
\*----------------------------------------------------------------------------*/
pub use crate::io::board_io::{
    determine_board_dimensions, format_board, format_numeric_board,
    format_square, mirror_pst_across_horizontal_axis, parse_square,
};
pub use crate::io::game_io::{
    combine_board_strings, export_tuned_parameters_file,
    format_castling_rights,
    format_en_passant_square, format_fen, format_game_phase, format_game_result,
    format_game_state, format_hand, format_position_hash,
    format_search_keys, format_special_rules,
    parse_config_file,
    parse_config_preview, parse_fen, parse_tuned_parameters, split_sections
};
pub use crate::io::logger::{
    configured_verbosity_level, dec_verbosity, inc_verbosity, init_logging,
};
pub use crate::io::move_io::{format_move, parse_move, format_move_history};
pub use crate::io::piece_io::{
    collect_piece_type_pairs, set_piece_dynamic_parameters
};
pub use crate::io::protocols::{
    translation::Translator,
    protocol::{
        find_protocol, Protocol, Session, run, start_search, new_game,
        print_handshake,
    },
    uci::Uci,
    usi::Usi,
    ucci::Ucci,
};

/*----------------------------------------------------------------------------*\
                                   DEBUG API
\*----------------------------------------------------------------------------*/
pub use crate::debug::graphics::{run_debug_graphics, BoardState};
pub use crate::debug::headless::run_debug_headless;
pub use crate::debug::datagen::run_datagen;
pub use crate::debug::sprt::{
    parse_sprt_time_control, run_sprt, SPRTTimeControl,
};
pub use crate::debug::tuning::run_tuning;

/*----------------------------------------------------------------------------*\
                             EXTERNAL DEPENDENCIES
\*----------------------------------------------------------------------------*/
pub use arboard::Clipboard;
pub use bnum::types::U4096;
pub use chrono;
pub use core::cell::SyncUnsafeCell;
pub use crossterm::{
    event,
    event::{
        DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent,
        KeyEventKind, KeyModifiers,
    },
    execute,
    terminal::{
        disable_raw_mode, enable_raw_mode, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
};
pub use env_logger::{
    fmt::{style as log_style},
    Builder as LoggerBuilder, Target as LoggerTarget
};
pub use hashbrown::{HashMap, HashSet};
pub use hotpath;
pub use include_dir::{include_dir, Dir};
pub use lazy_static::lazy_static;
pub use log::{debug, error, info, trace, warn};
pub use rand::{
    rngs::StdRng, seq::IndexedRandom, seq::SliceRandom, Rng, SeedableRng,
};
pub use ratatui::{
    backend::CrosstermBackend,
    buffer::Buffer,
    layout::{
        Alignment, Constraint, Direction, Flex, Layout, Margin, Rect, Spacing,
    },
    style::{Color, Modifier, Style},
    symbols::merge::MergeStrategy,
    text::{Line, Span, Text},
    widgets::{
        Block, Borders, Cell, Clear, List, ListItem, ListState, Padding,
        Paragraph, Row, Table, TableState, Tabs, Widget, Wrap,
    },
    Frame, DefaultTerminal,
};
pub use rayon::iter::{
    IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
};
pub use regex::Regex;
pub use std::{
    array, cmp, env,
    collections::VecDeque,
    fmt::{Debug, Display, Formatter as FmtFormatter, Result as FmtResult},
    fs::{self, OpenOptions},
    hash::Hash,
    io::{stdin, stdout, BufRead, BufReader, Read, Result as IoResult, Write},
    iter::zip,
    mem::{self, size_of},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, Stdio},
    sync::{
        atomic::{AtomicBool, AtomicU64, AtomicU8, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{self, Duration, Instant, SystemTime},
};

/*----------------------------------------------------------------------------*\
                                  CONSTANTS
\*----------------------------------------------------------------------------*/

/// Engine-wide constants.
///
/// Board and search bounds, colour and castling codes, and representation
/// sentinels shared across otherwise independent subsystems. `MAX_SQUARES`
/// tracks [`BoardBits`] and sizes its Zobrist tables.
pub const MAX_SQUARES: usize = 2048;
pub const MAX_DEPTH: usize = 128;
pub const PV_STRIDE: usize = MAX_DEPTH + 1;

pub const WHITE: u8 = 0;
pub const BLACK: u8 = 1;

pub const WK_INDEX: u8 = 0;
pub const WQ_INDEX: u8 = 1;
pub const BK_INDEX: u8 = 2;
pub const BQ_INDEX: u8 = 3;
pub const WK_CASTLE: u8 = 0b0001;
pub const WQ_CASTLE: u8 = 0b0010;
pub const BK_CASTLE: u8 = 0b0100;
pub const BQ_CASTLE: u8 = 0b1000;
pub const CASTLE_RIGHTS: u8 = 0b0000_1111;                                      /* the four bits CASTLING_HASHES is   */
pub const CASTLED: u8 = 0b0001_0000;                                            /* keyed on; the two above them mark  */
                                                                                /* a side that has already castled,   */
                                                                                /* shifted left by its colour         */

pub const NO_PIECE: PieceIndex = PieceIndex::MAX;
pub const NO_PAWN: usize = usize::MAX;
pub const NO_SQUARE: Square = Square::MAX;
pub const NO_EN_PASSANT: u32 = u32::MAX;

lazy_static! {
    /// Process-wide lazy statics.
    ///
    /// The Zobrist tables (`*_HASHES`, `SIDE_HASHES`) are filled once from
    /// the seeded RNG then stay read-only.
    ///
    /// The rest are shared runtime state:
    ///
    /// - `ENGINE_START` fixes the time origin
    /// - `SEED` fixes all randomness: the `ANEKAMACAM_SEED` environment
    ///   variable pins it for reproducible runs, unset falls back to a
    ///   per-process time-based value; `RNG` draws from it
    /// - `RUNTIME_VERBOSITY` / `DEBUG_FLAG` drive logging
    /// - `SYSTEM_INTERRUPT` / `LOG_MESSAGES` bridge the signal handler and TUI
    /// - `COMMENT_PATTERN` / `SECTION_PATTERN` are shared config-parse regexes
    pub static ref CASTLING_HASHES: [u128; 16] =
        array::from_fn(|_| random_u128());
    pub static ref COMMENT_PATTERN: Regex = Regex::new(r"//[^\n\r]*")
        .unwrap_or_else(|e| {
            panic!("Failed to compile COMMENT_PATTERN regex: {e}")
        });
    pub static ref SECTION_PATTERN: Regex =
        Regex::new(r"= (.+) =").unwrap();
    pub static ref ENGINE_START: Instant = Instant::now();
    pub static ref EN_PASSANT_HASHES: [u128; MAX_SQUARES] =
        array::from_fn(|_| random_u128());
    pub static ref IN_HAND_HASHES: Vec<[u128; MAX_SQUARES]> = {
        let mut result: Vec<[u128; MAX_SQUARES]> = Vec::with_capacity(256);

        for _ in 0..256 {
            let drop_hashes = array::from_fn(|_| random_u128());
            result.push(drop_hashes);
        }

        result
    };
    pub static ref LOG_MESSAGES: Mutex<VecDeque<String>> =
        Mutex::new(VecDeque::new());
    pub static ref PIECE_HASHES: Vec<[u128; MAX_SQUARES]> = {
        let mut result: Vec<[u128; MAX_SQUARES]> = Vec::with_capacity(256);

        for _ in 0..256 {
            let piece_hashes = array::from_fn(|_| random_u128());
            result.push(piece_hashes);
        }

        result
    };
    pub static ref SEED: u64 = env::var("ANEKAMACAM_SEED")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or_else(|| ENGINE_START.elapsed().as_nanos() as u64);
    pub static ref RNG: Mutex<StdRng> =
        Mutex::new(StdRng::seed_from_u64(*SEED));
    pub static ref RUNTIME_VERBOSITY: AtomicU8 = AtomicU8::new(5);
    pub static ref SIDE_HASHES: u128 = random_u128();
    pub static ref VIRGIN_HASHES: [u128; MAX_SQUARES] =
        array::from_fn(|_| random_u128());
    pub static ref SYSTEM_INTERRUPT: AtomicBool = AtomicBool::new(false);
    pub static ref DEBUG_FLAG: AtomicBool = AtomicBool::new(false);
    pub static ref ENGINE_SINK: Mutex<Option<Sender<EngineEvent>>> =
        Mutex::new(None);
}

/*----------------------------------------------------------------------------*\
                             OBSERVER ARCHITECTURE
\*----------------------------------------------------------------------------*/

/// EngineScore
///
/// A search score as data, not a formatted string, so each consumer prints
/// the `cp` / `mate` wording (and sign) in its own dialect.
pub enum EngineScore {
    CP(i32),                                                                    /* centipawn evaluation               */
    Mate(i32),                                                                  /* signed distance to mate, in moves  */
}

/// EngineEvent
///
/// The one protocol- and render-agnostic message the whole engine
/// broadcasts. Producers (search, sprt, datagen, derive, the protocol
/// command loop) `emit` these; the single active frontend — a protocol
/// printer thread, the debug TUI, or a headless printer — drains them and
/// renders each variant however it wishes. Every field is owned so the event
/// is `Send` across the worker-thread boundary.
pub enum EngineEvent {
    Info {                                                                      /* one iterative-deepening report     */
        hashfull: u64,
        cpuload: u64,
        depth: usize,
        score: EngineScore,
        nodes: u128,
        time_ms: u128,
        nps: u128,
        pv: String,
    },
    BestMove {                                                                  /* search result for the GUI          */
        best: String,
        ponder: Option<String>,
    },
    Print(String),                                                              /* verbatim stdout line(s), flushed   */
    Board(BoardState),                                                          /* live position snapshot for the TUI */
    StateInit(Arc<Mutex<State>>),                                               /* install a freshly loaded game      */
    PlaygroundUpdate(Box<State>),                                               /* new playground position            */
    SwitchDict(Option<Translator>),                                             /* swap the active translator         */
    Unlock,                                                                     /* release the TUI input lock         */
}

/// set_sink / clear_sink / emit
///
/// The producer side of the broadcast. `set_sink` installs the channel the
/// active frontend drains; `clear_sink` removes it on shutdown so a late emit
/// after the receiver is gone is a silent no-op; `emit` sends one event to
/// the installed sink if any. `emit` never blocks (the channel is unbounded)
/// and never fails outward, so a producer deep in the search need not know or
/// care whether anyone is listening.
///
/// set_sink
///   Params:
///   - sender: Sender<EngineEvent> -> the frontend's receiving channel
///
/// clear_sink
///   no parameters, no return value
///
/// emit
///   Params:
///   - event : EngineEvent         -> the state to broadcast
pub fn set_sink(sender: Sender<EngineEvent>) {
    *ENGINE_SINK.lock().unwrap() = Some(sender);
}

pub fn clear_sink() {
    *ENGINE_SINK.lock().unwrap() = None;
}

pub fn emit(event: EngineEvent) {
    if let Some(sender) = ENGINE_SINK.lock().unwrap().as_ref() {
        let _ = sender.send(event);
    }
}

/// spawn_printer
///
/// The single stdout writer for a text-protocol or headless run. Owns the
/// receiving end of the event channel and, for every event, formats the
/// engine's data into the protocol's line and flushes it, so search threads
/// and the command loop share one ordered, race-free output path. `Board` and
/// TUI-control events are ignored — those matter only to debug graphics,
/// which installs its own receiver instead.
///
/// Params:
/// - receiver: Receiver<EngineEvent> -> events from every producer
///
/// Return:
/// JoinHandle<()>                    -> join to flush the tail on shutdown
pub fn spawn_printer(receiver: Receiver<EngineEvent>) -> JoinHandle<()> {
    thread::spawn(move || {
        for event in receiver {
            match event {
                EngineEvent::Info {
                    hashfull, cpuload, depth, score,
                    nodes, time_ms, nps, pv,
                } => {
                    let score = match score {
                        EngineScore::CP(value) => format!("cp {}", value),
                        EngineScore::Mate(value) => format!("mate {}", value),
                    };
                    println!(
                        "info hashfull {} cpuload {} depth {} score {} \
                        nodes {} time {} nps {} pv {}",
                        hashfull, cpuload, depth, score,
                        nodes, time_ms, nps, pv,
                    );
                }
                EngineEvent::BestMove { best, ponder } => match ponder {
                    Some(ponder) => {
                        println!("bestmove {} ponder {}", best, ponder)
                    }
                    None => println!("bestmove {}", best),
                },
                EngineEvent::Print(text) => print!("{}", text),
                _ => {}
            }
            stdout().flush().ok();
        }
    })
}

/// with_stdout_sink
///
/// Runs a headless body with a temporary stdout printer installed as the
/// active sink, so `derive` / `tune` style tools that only `emit` still
/// produce output. Installs the sink, runs `body`, then clears the sink and
/// joins the printer so the final line flushes before returning.
///
/// Params:
/// - body: F -> the headless routine to run while the printer is live
pub fn with_stdout_sink<F: FnOnce()>(body: F) {
    let (sender, receiver) = channel::<EngineEvent>();
    set_sink(sender);
    let printer = spawn_printer(receiver);
    body();
    clear_sink();
    let _ = printer.join();
}

/// Null-move sentinels.
///
/// `null_move` and `null_pseudo_move` build the all-ones sentinel values
/// that mark "no move" in PV tables, killer slots, and TT entries. They
/// are functions rather than constants because `Move` holds a non-const
/// `Option<Arc<..>>` payload.
///
/// null_move
///
///   Return:
///   Move -> all-ones sentinel move with no capture payload
///
/// null_pseudo_move
///
///   Return:
///   PseudoMove -> all-ones packed move with a zero signature
pub fn null_move() -> Move {
    Move(!0u128, None)
}

pub fn null_pseudo_move() -> PseudoMove {
    (!0u128, 0u64)
}

/// Shared move-format tags.
pub const QUIET_MOVE: u128 = 0;
pub const SINGLE_CAPTURE_MOVE: u128 = 1;
pub const MULTI_CAPTURE_MOVE: u128 = 2;
pub const DROP_MOVE: u128 = 3;
pub const CASTLING_MOVE: u128 = 4;

pub const INDEX_TO_CARDINAL_VECTORS: [(i8, i8); 8] = [
    (0, 1), (1, 1), (1, 0), (1, -1),
    (0, -1), (-1, -1), (-1, 0), (-1, 1),
];

/// Shared game-phase and result tags.
pub const SETUP: u8 = 0;
pub const OPENING: u8 = 1;
pub const MIDDLEGAME: u8 = 2;
pub const ENDGAME: u8 = 3;

pub const ONGOING: u8 = 0;
pub const DRAW: u8 = 1;
pub const BLACK_WIN: u8 = 2;
pub const WHITE_WIN: u8 = 3;

/// Shared search score bands and transposition bound tags.
///
/// A quiet move is scored by summing `HISTORY_TABLES` cells, each clamped to
/// `HISTORY_BOUND`, so the quiet band has to be that many bounds wide on both
/// sides of its centre. Killers sit one bound above the widest quiet score and
/// losing captures one bound below the narrowest, which keeps the bands apart
/// however full the tables are.
pub const INF: i32 = 2_000_000;
pub const MATE_SCORE: i32 = INF - MAX_DEPTH as i32;
pub const EVAL_NONE: i32 = INF;

pub const HISTORY_BOUND: i32 = i16::MAX as i32 / 2;
pub const HISTORY_TABLES: i32 = 3;
pub const TABLE_MOVE_SCORE: usize = 5_000_000;
pub const WINNING_CAPTURE_SCORE: i32 = 4_000_000 + HISTORY_BOUND;
pub const KILLER_MOVE_SCORE: usize =
    (1_000_000 + (2 * HISTORY_TABLES + 1) * HISTORY_BOUND) as usize;
pub const QUIET_MOVE_SCORE: i32 = 1_000_000 + HISTORY_TABLES * HISTORY_BOUND;
pub const LOSING_CAPTURE_SCORE: i32 = 1_000_000 - HISTORY_BOUND;
pub const UNMAKEABLE_CAPTURE_SCORE: usize = 0;

pub const FALPHA: u8 = 0;
pub const FBETA: u8 = 1;
pub const FEXACT: u8 = 2;

/// Derivation and search constants read by multiple files.
pub const COEFFICIENT_SCALE: f64 = 1000.0;
pub const REDUCTION_MOVE_CAP: usize = 64;
pub const RFP_DEPTH: u32 = 6;
pub const FUTILITY_DEPTH: u32 = 6;
pub const LMP_DEPTH: u32 = 12;
pub const SEE_PRUNE_DEPTH: u32 = 5;
pub const SHELTER_CAP: u32 = 3;
pub const ZONE_ATTACK_UNIT: i32 = 16;
pub const ZONE_ATTACK_FULL: i32 = 16;
pub const SEARCH_REPETITION_CAP: usize = 64;
pub const REPETITION_CYCLE: u8 = 2;

/// Shared protocol, storage, and debug constants.
///
/// `*_DIR` are working-directory paths written to at runtime; `EMBEDDED_*`
/// are the same resources baked in at compile time, read when the
/// directory is absent. `OPT_*` are the names `setoption` matches.
pub const DATA_DIR: &str = "res/data";
pub const PARAMS_DIR: &str = "res/param";
pub const LOG_DIR: &str = "logs";

pub const OPT_THREADS: &str = "Threads";
pub const OPT_PROTOCOL: &str = "Protocol";
pub const OPT_PONDER: &str = "Ponder";
pub const OPT_HASH: &str = "Hash";
pub const OPT_CLEAR_HASH: &str = "Clear Hash";
pub const OPT_MOVE_OVERHEAD: &str = "Move Overhead";

pub const HASH_DEFAULT_MB: usize = 256;
pub const HASH_MAX_MB: usize = 65536;
pub const PAWN_TABLE_ENTRIES: usize = 1 << 13;
pub const OPENING_RANDOM_PLIES: usize = 8;

pub static EMBEDDED_CONFIGS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../configs");
pub static EMBEDDED_DICTS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/dicts");
pub static EMBEDDED_PERFT: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/perft");
pub static EMBEDDED_PARAMS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/param");
