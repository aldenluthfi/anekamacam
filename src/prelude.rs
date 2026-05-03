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
    moves::{AttackMask, Move},
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
pub use crate::game::drops::drop_list::{
    generate_drop_list, generate_relevant_drops,
};
pub use crate::game::drops::drop_parse::generate_drop_vectors;
pub use crate::game::moves::move_list::{
    generate_all_captures, generate_all_moves_and_drops, generate_attack_masks,
    generate_relevant_captures, generate_relevant_moves,
};
pub use crate::game::moves::move_parse::{
    INDEX_TO_CARDINAL_VECTORS, generate_move_vectors,
};

pub use crate::game::patterns::pattern_match::{
    PatternAllower, PatternSet, PatternStopper, generate_relevant_stand_offs,
    generate_stand_off_patterns, match_pattern, parse_pattern,
};
pub use crate::game::position::{
    hash::{PositionHash, hash_position},
    search::{
        SearchInfo, alpha_beta, check_interrupt, clear_search,
        search_position,
    },
};
pub use crate::game::search::{
    move_ordering::{pick_by_score, score_move},
    quiescence::quiescence_search,
    transposition::{TTEntry, TTable},
    parameters::{derive_parameters}
};

pub use crate::game::util::{
    benchmark_perft, benchmark_search, format_time, perft, random_u128,
    refresh_eval_state, verify_game_state,
};

/*----------------------------------------------------------------------------*\
                                   IO API
\*----------------------------------------------------------------------------*/
pub use crate::io::board_io::{
    format_board, format_square,
};
pub use crate::io::game_io::{
    export_tuned_parameters_file, format_game_state,
    parse_config_file, parse_fen, parse_tuned_parameters_file,
    format_castling_rights, format_fen, format_hand, format_numeric_board,
    format_position_hash, format_special_rules, format_game_phase,
    format_en_passant_square, set_piece_dynamic_parameters
};
pub use crate::io::logger::{
    configured_log_level, configured_verbosity_level, init_logging,
    push_log_message, verbosity_enabled,
};
pub use crate::io::move_io::{format_move, parse_move, format_move_history};
pub use crate::io::tui::tui;
pub use crate::{log_1, log_2, log_3, log_4};

/*----------------------------------------------------------------------------*\
                             EXTERNAL DEPENDENCIES
\*----------------------------------------------------------------------------*/
pub use bnum::types::U4096;
pub use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode,
        KeyEvent, KeyEventKind, KeyModifiers,
    },
    execute,
    terminal::{
        EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode,
        enable_raw_mode,
    },
};
pub use env_logger::fmt::{style as log_style};
pub use hashbrown::{HashMap, HashSet};
pub use hotpath;
pub use lazy_static::lazy_static;
pub use log::{debug, error, info, warn};
pub use mpsc::{channel, Sender, Receiver, TryRecvError};
pub use rand::{RngCore, SeedableRng, seq::SliceRandom, rngs::StdRng};
pub use ratatui::{
    Frame, DefaultTerminal,
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
        Paragraph, Row, Table, TableState, Tabs, Wrap, Widget
    },
};
pub use regex::Regex;
pub use std::{
    array, cmp,
    collections::VecDeque,
    fmt::{Debug, Formatter as FmtFormatter, Result as FmtResult},
    fs,
    hash::Hash,
    io::{Write, stdin, stdout, Result as IoResult},
    mem::size_of,
    path::Path,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
        mpsc,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

/*----------------------------------------------------------------------------*\
                                  CONSTANTS
\*----------------------------------------------------------------------------*/
pub const FORMAT_VERBOSITY_1: u8 = 1;
pub const FORMAT_VERBOSITY_2: u8 = 2;
pub const FORMAT_VERBOSITY_3: u8 = 3;
pub const FORMAT_VERBOSITY_4: u8 = 4;
pub const FORMAT_VERBOSITY_5: u8 = 5;

pub const MAX_SQUARES: usize = 2048;
pub const MAX_PIECES: usize = 255;

pub const RNG_SEED: u64 = 0xDEADBEEFCAFEBABE;

pub const A: u8 = 0;
pub const B: u8 = 1;
pub const C: u8 = 2;
pub const D: u8 = 3;
pub const E: u8 = 4;
pub const F: u8 = 5;
pub const G: u8 = 6;
pub const H: u8 = 7;
pub const I: u8 = 8;
pub const J: u8 = 9;
pub const K: u8 = 10;
pub const L: u8 = 11;
pub const M: u8 = 12;
pub const N: u8 = 13;
pub const O: u8 = 14;
pub const P: u8 = 15;
pub const Q: u8 = 16;
pub const R: u8 = 17;
pub const S: u8 = 18;
pub const T: u8 = 19;
pub const U: u8 = 20;
pub const V: u8 = 21;
pub const W: u8 = 22;
pub const X: u8 = 23;
pub const Y: u8 = 24;
pub const Z: u8 = 25;

pub const WHITE: u8 = 0;
pub const BLACK: u8 = 1;
pub const BOTH: u8 = 2;

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

lazy_static! {
    pub static ref RNG: Mutex<StdRng> =
        Mutex::new(StdRng::seed_from_u64(RNG_SEED));
    pub static ref EMPTY_CAPTURE_LIST: Arc<Vec<u64>> = Arc::new(Vec::new());
    pub static ref ENGINE_START: Instant = Instant::now();
    pub static ref COMMENT_PATTERN: Regex = Regex::new(r"//[^\n\r]*")
        .unwrap_or_else(|e| {
            panic!("Failed to compile COMMENT_PATTERN regex: {e}")
        });
    pub static ref CASTLING_HASHES: [u128; 16] =
        array::from_fn(|_| random_u128());
    pub static ref EN_PASSANT_HASHES: [u128; MAX_SQUARES] =
        array::from_fn(|_| random_u128());
    pub static ref SIDE_HASHES: u128 = random_u128();
    pub static ref PIECE_HASHES: Vec<[u128; MAX_SQUARES]> = {
        let mut result: Vec<[u128; MAX_SQUARES]> = Vec::with_capacity(256);

        for _ in 0..256 {
            let piece_hashes = array::from_fn(|_| random_u128());
            result.push(piece_hashes);
        }

        result
    };
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
}

pub fn null_move() -> Move {
    Move(!0u128, Arc::clone(&EMPTY_CAPTURE_LIST))
}
pub const DEFAULT_DROP: &str = "@#~?@";
pub const NULL_DROP: &str = "@#~?@#~?";

pub const INFINITY: i32 = 20000;
pub const MATE_SCORE: i32 = INFINITY - MAX_DEPTH as i32;

pub const HFNONE: u8 = 0;
pub const HFALPHA: u8 = 1;
pub const HFBETA: u8 = 2;
pub const HFEXACT: u8 = 3;

pub const OPENING: u8 = 0;
pub const MIDDLEGAME: u8 = 1;
pub const ENDGAME: u8 = 2;

pub const T_TABLE_SIZE: usize = (0x1000000 * 256) / size_of::<TTEntry>();       /* 256MB                              */
pub const MAX_DEPTH: usize = 64;

pub const TUI_NORMAL_MODE: u8 = 0;
pub const TUI_INPUT_MODE: u8 = 1;

pub const MAX_LOGS_LEN: usize = 4096;
pub const CONFIGS_DIR: &str = "configs";
