//! prelude.rs
//!
//! Common prelude of the anekamacam engine.
//!
//! This file exports again the types, macros and functions that all
//! modules use. It also has the shared constants, statics and the event
//! sink. A module imports it with `use crate::*;`.
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
pub use crate::game::moves::move_parse::{
    generate_move_set, generate_move_vectors, strip_move_conditions,
};

pub use crate::game::moves::pattern_parse::{
    clip_move_vector, clip_pattern, generate_relevant_stand_offs,
    parse_pattern,
};
pub use crate::game::position::{
    hash::{
        hash_pawns, hash_position, hash_virgin_board, qsearch_key, search_key,
        PositionHash,
    },
    search::{
        alpha_beta, check_interrupt, clear_search, iterative_deepening,
        log_table_stats, quiescence_search, search_position,
        SearchInfo, SearchResult,
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
    parse_sprt_option, parse_sprt_time_control, run_sprt, SPRTEngine,
    SPRTMatch, SPRTTimeControl,
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
        atomic::{AtomicBool, AtomicU64, AtomicU8, AtomicUsize, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{self, Duration, Instant, SystemTime},
};

/*----------------------------------------------------------------------------*\
                                   CONSTANTS
\*----------------------------------------------------------------------------*/

/// Engine constants
///
/// Shared constants for board and search limits, colours, castling and
/// empty values.
///
/// - `MAX_SQUARES`     : length of the Zobrist tables, the board area limit
/// - `MAX_PIECES`      : range of the 10-bit piece field, two per type
/// - `MAX_PIECE_VALUE` : largest value of the 14-bit material field
/// - `MAX_DEPTH`       : length of each array with one entry per ply
/// - `PV_STRIDE`       : `MAX_DEPTH + 1`, a PV row also at the deepest ply
///
/// One castling byte has the four rights and, above them, the castled
/// marks. Evaluation reads the marks after the rights are gone.
///
/// ```text
///     7    6    5    4    3    2    1    0
///   ┌────┬────┬────┬────┬────┬────┬────┬────┐
///   │ ·  │ ·  │ B  │ W  │ BQ │ BK │ WQ │ WK │
///   └────┴────┴────┴────┴────┴────┴────┴────┘
///             └─CASTLED┘└── CASTLE_RIGHTS ──┘
/// ```
///
/// - `WHITE`, `BLACK`           : colour codes, also the `CASTLED` shift
/// - `WK_INDEX` .. `BQ_INDEX`   : slot index of each right
/// - `WK_CASTLE` .. `BQ_CASTLE` : bit of each right, as in the diagram
/// - `CASTLE_RIGHTS`            : the four right bits, keys of the hash
/// - `CASTLED`                  : castled mark, shifted by the colour
///
/// Each `NO_*` sentinel is the maximum of its type, so it is never a real
/// index. A sentinel is never in a packed field.
///
/// Notes:
/// Derivation can give a value above `MAX_PIECE_VALUE` on a large board.
/// Then it scales the full table down to fit.
///
pub const MAX_SQUARES: usize = 2048;
pub const MAX_PIECES: usize = 1024;
pub const MAX_PIECE_VALUE: u16 = 0x3FFF;
pub const MAX_DEPTH: usize = 256;
pub const PV_STRIDE: usize = MAX_DEPTH + 1;

/// MAX_LOG_HISTORY
///
/// Maximum number of log lines in the console queue. The log file keeps
/// all lines. A taikyoku shogi load writes about 78000 lines.
///
pub const MAX_LOG_HISTORY: usize = 1 << 16;

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
pub const CASTLE_RIGHTS: u8 = 0b0000_1111;                                      /* rights, keyed by CASTLING_HASHES   */
pub const CASTLED: u8 = 0b0001_0000;                                            /* castled mark, shifted by colour    */

pub const NO_PIECE: PieceIndex = PieceIndex::MAX;
pub const NO_PAWN: usize = usize::MAX;
pub const NO_SQUARE: Square = Square::MAX;
pub const NO_EN_PASSANT: u32 = u32::MAX;

lazy_static! {
    /// Process statics
    ///
    /// Statics for the full process. The seeded RNG fills the Zobrist
    /// tables (`*_HASHES`) once. After that, they do not change.
    ///
    /// - `ENGINE_START`      : origin of the engine time
    /// - `SEED`              : `ANEKAMACAM_SEED` if set, else a time value
    /// - `RNG`               : random generator from `SEED`
    /// - `RUNTIME_VERBOSITY` : current log level
    /// - `DEBUG_FLAG`        : true in the debug console
    /// - `SYSTEM_INTERRUPT`  : set by the signal handler
    /// - `LOG_MESSAGES`      : log queue of the debug console
    /// - `ENGINE_SINK`       : sender of the active event sink
    /// - `COMMENT_PATTERN`   : config comment regex
    /// - `SECTION_PATTERN`   : config section regex
    ///
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
        let mut result: Vec<[u128; MAX_SQUARES]> =
            Vec::with_capacity(MAX_PIECES);

        for _ in 0..MAX_PIECES {
            let drop_hashes = array::from_fn(|_| random_u128());
            result.push(drop_hashes);
        }

        result
    };
    pub static ref LOG_MESSAGES: Mutex<VecDeque<String>> =
        Mutex::new(VecDeque::new());
    pub static ref PIECE_HASHES: Vec<[u128; MAX_SQUARES]> = {
        let mut result: Vec<[u128; MAX_SQUARES]> =
            Vec::with_capacity(MAX_PIECES);

        for _ in 0..MAX_PIECES {
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
/// A search score as data, not as text. Each consumer writes the `cp` or
/// `mate` form in its own notation.
///
pub enum EngineScore {
    CP(i32),                                                                    /* centipawn evaluation               */
    Mate(i32),                                                                  /* signed distance to mate, in moves  */
}

/// EngineEvent
///
/// The message that the engine sends to the active frontend. It does not
/// depend on a protocol or a display.
///
/// - producers : search, sprt, datagen, derive, the protocol command loop
/// - consumers : protocol printer thread, debug console, headless printer
///
/// Each field is owned, so the event can go to other threads (`Send`).
///
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

/// Event sink functions
///
/// The producer side of the event channel. `emit` does not block and does
/// not fail, so a producer does not have to know if a frontend listens.
///
/// - set_sink   : installs the channel of the active frontend
/// - clear_sink : removes the channel, later events do nothing
/// - emit       : sends one event to the channel, if there is one
///
/// set_sink
///
///   Params:
///   - sender: Sender<EngineEvent> -> channel of the frontend
///
/// clear_sink
///
///   No parameters and no return value.
///
/// emit
///
///   Params:
///   - event : EngineEvent         -> event to send
///
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
/// Starts the only stdout writer of a protocol or headless run. It writes
/// each event as a protocol line and flushes it. Thus all threads share one
/// ordered output.
///
/// Params:
/// - receiver: Receiver<EngineEvent> -> events from all producers
///
/// Return:
/// JoinHandle<()>                    -> join it to flush the last lines
///
/// Notes:
/// The printer ignores `Board` and console events. The debug console has
/// its own receiver.
///
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
/// Runs a headless function with a temporary stdout printer as the sink.
/// Thus tools such as `derive` and `tune`, which only `emit`, write output.
///
/// 1. install the sink and start the printer
/// 2. run `body`
/// 3. clear the sink and join the printer, so the last line flushes
///
/// Params:
/// - body: F -> the headless function to run
///
pub fn with_stdout_sink<F: FnOnce()>(body: F) {
    let (sender, receiver) = channel::<EngineEvent>();
    set_sink(sender);
    let printer = spawn_printer(receiver);
    body();
    clear_sink();
    let _ = printer.join();
}

/// Null move constructors
///
/// Make the all-ones values that mean "no move" in PV tables, killer slots
/// and hash entries. They are functions, because the `Option<Arc<..>>` of
/// `Move` cannot be a constant.
///
/// null_move
///
///   Return:
///   Move       -> all-ones move without capture data
///
/// null_pseudo_move
///
///   Return:
///   PseudoMove -> all-ones packed move with a zero signature
///
pub fn null_move() -> Move {
    Move(!0u128, None)
}

pub fn null_pseudo_move() -> PseudoMove {
    (!0u128, 0u64)
}

/// Move type tags
///
/// Move type tags. The low three bits of [`Move`]`.0` select the packed
/// layout of the word. `moves.rs` shows each layout.
///
/// - `QUIET_MOVE`          : a piece moves without capture
/// - `SINGLE_CAPTURE_MOVE` : one captured piece
/// - `MULTI_CAPTURE_MOVE`  : many captured pieces, each with its own square
/// - `DROP_MOVE`           : a piece goes from the hand to the board
/// - `CASTLING_MOVE`       : two pieces move, to squares that the rule gives
///
pub const QUIET_MOVE: u128 = 0;
pub const SINGLE_CAPTURE_MOVE: u128 = 1;
pub const MULTI_CAPTURE_MOVE: u128 = 2;
pub const DROP_MOVE: u128 = 3;
pub const CASTLING_MOVE: u128 = 4;

/// INDEX_TO_CARDINAL_VECTORS
///
/// The eight unit vectors in clockwise order from north. Each entry is
/// `(file, rank)`, with east and north positive, for the first player.
///
/// ```text
///   ┌───────────┬───────────┬───────────┐
///   │  7  nw    │  0  n     │  1  ne    │
///   │  (-1, 1)  │  ( 0, 1)  │  ( 1, 1)  │
///   ├───────────┼───────────┼───────────┤
///   │  6  w     │           │  2  e     │
///   │  (-1, 0)  │  origin   │  ( 1, 0)  │
///   ├───────────┼───────────┼───────────┤
///   │  5  sw    │  4  s     │  3  se    │
///   │  (-1,-1)  │  ( 0,-1)  │  ( 1,-1)  │
///   └───────────┴───────────┴───────────┘
/// ```
///
/// The index plus `k`, modulo eight, turns the vector 45 * k degrees
/// clockwise. The move parser uses this for all rotations.
///
pub const INDEX_TO_CARDINAL_VECTORS: [(i8, i8); 8] = [
    (0, 1), (1, 1), (1, 0), (1, -1),
    (0, -1), (-1, -1), (-1, 0), (-1, 1),
];

/// Game phase and result tags
///
/// Game phase tags and game result tags.
///
/// ```text
///   SETUP -- empty --> OPENING <--> MIDDLEGAME <--> ENDGAME
///     0                  1              2              3
/// ```
///
/// - `SETUP`   : a variant with a setup starts here, if a royal is in hand
/// - `empty`   : the move that empties the two hands ends the setup
/// - other     : the phase score and the two variant thresholds select it
///
/// The phase depends only on the position. A promotion or a drop can move
/// the phase back. The hash key has no phase, so equal positions must have
/// equal phases.
///
/// The result tags do not depend on the side to move:
///
/// - `ONGOING`   : 0, the game did not end
/// - `DRAW`      : 1, any draw
/// - `BLACK_WIN` : 2
/// - `WHITE_WIN` : 3
///
pub const SETUP: u8 = 0;
pub const OPENING: u8 = 1;
pub const MIDDLEGAME: u8 = 2;
pub const ENDGAME: u8 = 3;

pub const ONGOING: u8 = 0;
pub const DRAW: u8 = 1;
pub const BLACK_WIN: u8 = 2;
pub const WHITE_WIN: u8 = 3;

/// Search score constants
///
/// Search score limits, move ordering bands and hash bound tags.
///
/// - `INF`        : larger than all engine scores
/// - `MATE_SCORE` : `INF - MAX_DEPTH`, room for the mate distance in plies
/// - `EVAL_NONE`  : `INF`, "no static evaluation", written in check
///
/// Each move class has its own band. A quiet score is the sum of
/// `HISTORY_TABLES` cells, each in `-HISTORY_BOUND..=HISTORY_BOUND`. Thus
/// the bands never overlap:
///
/// ```text
///   5_000_000            TABLE_MOVE_SCORE          table move
///   4_000_000 + b        WINNING_CAPTURE_SCORE     winning capture
///   1_000_000 + 7b       KILLER_MOVE_SCORE         killer
///   1_000_000 + 6b   ┐
///   1_000_000 + 3b   ┤   QUIET_MOVE_SCORE          centre, plus history
///   1_000_000        ┘
///   1_000_000 - b        LOSING_CAPTURE_SCORE      losing capture
///           0            UNMAKEABLE_CAPTURE_SCORE  cannot be made at all
/// ```
///
/// `b` is `HISTORY_BOUND`. `7b` is `2 * HISTORY_TABLES + 1` bounds, one
/// bound above the largest quiet score.
///
/// The bound tags tell how a stored score relates to its search window:
///
/// - `FALPHA` : upper bound, no move was better than alpha
/// - `FBETA`  : lower bound, a move caused a cutoff
/// - `FEXACT` : exact value, inside the window
///
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

/// Derivation and search constants
///
/// Derivation and search constants. `parameters.rs` sizes tables with them
/// and `search.rs` or `evaluation.rs` index the tables with them. One copy
/// keeps the two sides equal.
///
/// - `COEFFICIENT_SCALE`     : divisor that makes derived values integers
/// - `REDUCTION_MOVE_CAP`    : move axis width of the reduction surface
/// - `RFP_DEPTH`             : maximum depth of reverse futility pruning
/// - `FUTILITY_DEPTH`        : maximum depth of futility pruning
/// - `LMP_DEPTH`             : maximum depth of late move pruning
/// - `SEE_PRUNE_DEPTH`       : maximum depth of exchange pruning
/// - `SHELTER_CAP`           : maximum shelter units for each royal
/// - `ZONE_ATTACK_UNIT`      : danger units for one expected attack
/// - `ZONE_ATTACK_FULL`      : attacks for a fully attacked royal zone
/// - `SEARCH_REPETITION_CAP` : plies that the repetition scan examines
/// - `REPETITION_CYCLE`      : occurrences for one cycle, without perpetual
///
/// Notes:
/// A move index above `REDUCTION_MOVE_CAP` uses the last column. The danger
/// sum is divided by `ZONE_ATTACK_UNIT * ZONE_ATTACK_FULL`.
///
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

/// Protocol, storage and debug constants
///
/// Protocol, storage and debug constants. The engine writes to the
/// `*_DIR` paths at runtime. The `EMBEDDED_*` files are in the binary, so
/// a copied binary plays all variants.
///
/// - `DATA_DIR`         : self-play positions for tuning, for each variant
/// - `PARAMS_DIR`       : tuned parameters, for each variant
/// - `LOG_DIR`          : log of this run and of earlier runs
/// - `EMBEDDED_CONFIGS` : rules of each variant
/// - `EMBEDDED_DICTS`   : notation dictionaries, one section per protocol
/// - `EMBEDDED_PERFT`   : perft suites
/// - `EMBEDDED_PARAMS`  : shipped tuned parameters
///
/// The `OPT_*` names are the `setoption` names:
///
/// - `OPT_THREADS`       : number of search workers
/// - `OPT_PROTOCOL`      : protocol dialect of the session
/// - `OPT_PONDER`        : accepted for a GUI, but not used
/// - `OPT_HASH`          : size of the shared tables in megabytes
/// - `OPT_CLEAR_HASH`    : no value, makes new tables of the same size
/// - `OPT_MOVE_OVERHEAD` : milliseconds kept back from each clock
///
/// Other constants:
///
/// - `HASH_DEFAULT_MB`      : default `Hash` value
/// - `HASH_MAX_MB`          : maximum `Hash` value
/// - `PAWN_TABLE_ENTRIES`   : pawn cache size for each worker at default Hash
/// - `OPENING_RANDOM_PLIES` : random plies at the start of a self-play game
///
/// Notes:
/// The pawn cache size scales with Hash and rounds down to a power of two.
///
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
    include_dir!("$CARGO_MANIFEST_DIR/../res/config");
pub static EMBEDDED_DICTS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/dicts");
pub static EMBEDDED_PERFT: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/perft");
pub static EMBEDDED_PARAMS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/param");
