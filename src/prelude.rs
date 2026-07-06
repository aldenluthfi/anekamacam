//! # prelude.rs
//!
//! Project-wide prelude for the anekamacam engine.
//!
//! This file re-exports the most commonly used types, macros, and functions
//! from the project for convenient use in all modules. Import this prelude
//! to avoid repetitive imports and enable ergonomic access to core engine
//! functionality.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/02/2026
//!
//! # Usage
//! Add `use crate::*;` at the top of the module.

/*----------------------------------------------------------------------------*\
                              CORE REPRESENTATIONS
\*----------------------------------------------------------------------------*/
pub use crate::game::representations::{
    board::Board,
    drop::DropSet,
    moves::{AttackMask, Move, MoveSignature, PseudoMove},
    pattern::{
        Pattern, PatternAllower, PatternSet, PatternStopper, PatternUnit,
        PieceSet,
    },
    piece::{Piece, PieceIndex},
    state::{EnPassantSquare, Snapshot, Square, State},
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
    generate_all_captures, generate_all_moves_and_drops, generate_attack_masks,
    generate_relevant_captures, generate_relevant_castling,
    generate_relevant_moves,
};
pub use crate::game::moves::move_parse::{
    INDEX_TO_CARDINAL_VECTORS, generate_move_vectors,
};

pub use crate::game::moves::pattern_parse::{
    generate_relevant_stand_offs, generate_stand_off_patterns,
    parse_pattern,
};
pub use crate::game::position::{
    hash::{hash_pawns, hash_position, PositionHash},
    search::{
        alpha_beta, check_interrupt, clear_search, iterative_deepening,
        log_table_stats, search_position, SearchBufs, SearchInfo,
        SearchResult,
    },
};
pub use crate::game::search::{
    parallel::ThreadPool,
    parameters::{
        derive_eval_parameters, derive_parameters, derive_search_parameters,
    },
    transposition::{PTable, PTEntry, QTable, QTEntry, TTEntry, TTable},
};

pub use crate::game::util::{
    benchmark_headless_perft, benchmark_perft, benchmark_search, format_time,
    perft, random_u128, refresh_eval_state, roll_latest, square_distance,
    verify_game_state,
};

/*----------------------------------------------------------------------------*\
                                   IO API
\*----------------------------------------------------------------------------*/
pub use crate::io::board_io::{
    determine_board_dimensions, format_board, format_numeric_board,
    format_square, mirror_pst_across_horizontal_axis, parse_square,
};
pub use crate::io::game_io::{
    combine_board_strings, export_tuned_parameters_file, format_castling_rights,
    format_en_passant_square, format_fen, format_game_phase, format_game_state,
    format_hand, format_position_hash, format_special_rules, parse_config_file,
    parse_config_preview, parse_fen, parse_tuned_parameters
};
pub use crate::io::logger::{
    configured_log_level, configured_verbosity_level, dec_verbosity,
    inc_verbosity, init_logging,
};
pub use crate::io::move_io::{format_move, parse_move, format_move_history};
pub use crate::io::piece_io::{
    collect_piece_type_pairs, set_piece_dynamic_parameters
};
pub use crate::io::protocols::{
    translation::Translator,
    uci::uci,
};

/*----------------------------------------------------------------------------*\
                                   DEBUG API
\*----------------------------------------------------------------------------*/
pub use crate::debug::console::debug_console;
pub use crate::debug::console::{BoardState, TuiEvent};
pub use crate::debug::datagen::run_datagen;
pub use crate::debug::sprt::run_sprt;
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
    fmt::{Debug, Formatter as FmtFormatter, Result as FmtResult},
    fs::{self, OpenOptions},
    hash::Hash,
    io::{stdin, stdout, BufRead, BufReader, Result as IoResult, Write},
    iter::zip,
    mem::{self, size_of},
    panic::{catch_unwind, AssertUnwindSafe},
    path::Path,
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
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
/// Board and search bounds (`MAX_*`, `*_DEPTH`), search-tuning thresholds
/// and tables (LMP/RFP/razor/IID/LMR limits, `HIST_BONUS_TABLE`), phase
/// occupancies, colour and castling codes, piece/en-passant sentinels, and
/// the move-type tags. Values are fixed at compile time and shared through
/// the prelude.
pub const MAX_SQUARES: usize = 2048;
pub const MAX_PIECES: usize = 255;
pub const MAX_DEPTH: usize = 128;
pub const PV_STRIDE: usize = MAX_DEPTH + 1;
pub const MAX_LMR_DEPTH: usize = 32;
pub const MAX_LOGS_LEN: usize = u16::MAX as usize;
pub const MAX_HIST_VALUE: i16 = 16384;
pub const HIST_BONUS_SCALE: i32 = 32;
pub const HIST_BONUS_TABLE: [i32; MAX_DEPTH] = {
    let cap = MAX_HIST_VALUE as i32;
    let mut table = [0i32; MAX_DEPTH];
    let mut depth = 0;
    while depth < MAX_DEPTH {
        let value = (depth * depth) as i32 * HIST_BONUS_SCALE;
        table[depth] = if value < cap { value } else { cap };
        depth += 1;
    }
    table
};
pub const MAX_FUTILITY_DEPTH: usize = 5;
pub const MAX_LMP_DEPTH: usize = 9;
pub const LMP_THRESHOLD: [[u8; MAX_LMP_DEPTH]; 2] = [
    [0, 4, 5, 9,  14, 22, 34, 46, 60],                                          /* not improving                      */
    [0, 5, 8, 14, 22, 36, 48, 66, 90],                                          /* improving                          */
];
pub const MAX_RFP_DEPTH: usize = 9;
pub const MAX_RAZOR_DEPTH: usize = 4;
pub const MAX_SEE_PRUNE_DEPTH: usize = 8;
pub const MIN_IID_DEPTH: usize = 7;
pub const MIN_IIR_DEPTH: usize = 4;
pub const MIN_LMR_DEPTH: usize = 4;
pub const MIN_LMP_DEPTH: usize = 3;

pub const DANGEROUS_PUSH_THRESHOLD: i32 = 92;

pub const PAWN_MIN_START_COUNT: usize = 5;

pub const OPENING_OCCUPANCY: f64 = 0.36;
pub const ENDGAME_OCCUPANCY: f64 = 0.12;

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
pub const CASTLING: [u8; 4] = [WK_CASTLE, WQ_CASTLE, BQ_CASTLE, BK_CASTLE];

pub const NO_PIECE: u8 = u8::MAX;
pub const NO_EN_PASSANT: u32 = u32::MAX;

pub const QUIET_MOVE: u128 = 0;
pub const SINGLE_CAPTURE_MOVE: u128 = 1;
pub const MULTI_CAPTURE_MOVE: u128 = 2;
pub const DROP_MOVE: u128 = 3;
pub const CASTLING_MOVE: u128 = 4;

lazy_static! {
    /// Process-wide lazy statics.
    ///
    /// The Zobrist tables (`*_HASHES`, `SIDE_HASHES`) are filled once from
    /// the seeded RNG then stay read-only.
    ///
    /// The rest are shared runtime state:
    ///
    /// - `ENGINE_START` fixes the time origin
    /// - `RNG` seeds all randomness
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
    pub static ref RNG: Mutex<StdRng> =
        Mutex::new(
            StdRng::seed_from_u64(ENGINE_START.elapsed().as_nanos() as u64)
        );
    pub static ref RUNTIME_VERBOSITY: AtomicU8 = AtomicU8::new(5);
    pub static ref SIDE_HASHES: u128 = random_u128();
    pub static ref SYSTEM_INTERRUPT: AtomicBool = AtomicBool::new(false);
    pub static ref DEBUG_FLAG: AtomicBool = AtomicBool::new(false);
}

/// Null-move sentinels.
///
/// `null_move` and `null_pseudo_move` build the all-ones sentinel values
/// that mark "no move" in PV tables, killer slots, and TT entries. They
/// are functions rather than constants because `Move` holds a non-const
/// `Option<Arc<..>>` payload.
pub fn null_move() -> Move {
    Move(!0u128, None)
}

pub fn null_pseudo_move() -> PseudoMove {
    (!0u128, 0u64)
}

pub const DEFAULT_DROP: &str = "@#~?@";

pub const INF: i32 = 2_000_000;
pub const MATE_SCORE: i32 = INF - MAX_DEPTH as i32;

pub const FALPHA: u8 = 0;
pub const FBETA: u8 = 1;
pub const FEXACT: u8 = 2;

pub const SETUP: u8 = 0;
pub const OPENING: u8 = 1;
pub const MIDDLEGAME: u8 = 2;
pub const ENDGAME: u8 = 3;

pub const HASH_T_PARTS: usize = 16;
pub const HASH_Q_PARTS: usize = 8;
pub const HASH_P_PARTS: usize = 1;
pub const HASH_PARTS: usize =
    HASH_T_PARTS + HASH_Q_PARTS + HASH_P_PARTS;

pub const T_TABLE_SIZE: usize =
    (HASH_DEFAULT_MB * HASH_T_PARTS / HASH_PARTS * 0x100000)
    / size_of::<TTEntry>();
pub const Q_TABLE_SIZE: usize =
    (HASH_DEFAULT_MB * HASH_Q_PARTS / HASH_PARTS * 0x100000)
    / size_of::<QTEntry>();
pub const P_TABLE_SIZE: usize =
    (HASH_DEFAULT_MB * HASH_P_PARTS / HASH_PARTS * 0x100000)
    / size_of::<PTEntry>();

pub const WINNING_CAPTURE_SCORE: i32 = 4_000_000;                               /* ordering band for SEE >= 0 moves   */
pub const LOSING_CAPTURE_SCORE: i32 = 1_000_000;                                /* ordering band for SEE < 0          */

pub const LOG_DIR: &str = "logs";
pub const PARAMS_DIR: &str = "res/param";
pub const DATA_DIR: &str = "res/data";
pub const SPRT_DIR: &str = "res/sprt";

pub const TIME_OVERHEAD_MS: u128 = 50;
pub const MAX_OVERHEAD_MS: u128 = 1000;
pub const MIN_TIME_BUDGET_NS: u128 = 1_000_000;                                 /* timed searches never budget below  */
pub const HARD_BUDGET_FACTOR: u128 = 4;                                         /* hard limit = soft budget x factor  */

pub const OPT_VARIANT: &str = "UCI_Variant";
pub const OPT_THREADS: &str = "Threads";
pub const OPT_PONDER: &str = "Ponder";
pub const OPT_HASH: &str = "Hash";
pub const OPT_CLEAR_HASH: &str = "Clear Hash";
pub const OPT_MOVE_OVERHEAD: &str = "Move Overhead";

pub const HASH_DEFAULT_MB: usize = 256;
pub const HASH_MAX_MB: usize = 65536;

/// SPRT and Texel-tuning tool constants.
///
/// Fixed knobs for the debug self-play tools shared across `datagen`,
/// `tuning`, and `sprt`:
///
/// - `OPENING_RANDOM_PLIES` -> random-opening depth
///
/// - `ADAM_BETA_ONE` / `ADAM_BETA_TWO` / `ADAM_EPSILON` -> the Adam
///   optimiser moment decay rates and denominator floor for tuning.
///
/// - `TEXEL_K_MIN` / `TEXEL_K_MAX` / `TEXEL_K_ITERATIONS` -> the search
///   bounds and step count for fitting the sigmoid scaling constant `K`.
///
/// - `SPRT_ALPHA` / `SPRT_BETA` -> the SPRT type-one and type-two error
///   rates that set the log-likelihood acceptance bounds.
///
pub const OPENING_RANDOM_PLIES: usize = 8;
pub const ADAM_BETA_ONE: f64 = 0.9;
pub const ADAM_BETA_TWO: f64 = 0.999;
pub const ADAM_EPSILON: f64 = 1e-8;

pub const TEXEL_K_MIN: f64 = 0.01;
pub const TEXEL_K_MAX: f64 = 3.0;
pub const TEXEL_K_ITERATIONS: usize = 32;

pub const SPRT_ALPHA: f64 = 0.05;
pub const SPRT_BETA: f64 = 0.05;

pub static EMBEDDED_CONFIGS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../configs");
pub static EMBEDDED_DICTS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/dicts");
pub static EMBEDDED_PERFT: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/perft");
pub static EMBEDDED_PARAMS: Dir<'static> =
    include_dir!("$CARGO_MANIFEST_DIR/../res/param");
