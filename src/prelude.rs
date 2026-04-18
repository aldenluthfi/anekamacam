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
            self, MultiLegEval, MultiLegExpr, MultiLegSlashExpr, MultiLegTerm,
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
    generate_all_moves_and_drops, generate_attack_masks, generate_move_list,
    generate_relevant_moves,
};
pub use crate::game::moves::move_parse::{
    INDEX_TO_CARDINAL_VECTORS, generate_move_vectors,
};
pub use crate::game::patterns::pattern_match::{
    PatternAllower, PatternSet, PatternStopper, generate_relevant_stand_offs,
    generate_stand_off_patterns, match_pattern, parse_pattern,
};
pub use crate::game::position::{
    hash::{
        CASTLING_HASHES, EN_PASSANT_HASHES, IN_HAND_HASHES, PIECE_HASHES,
        PositionHash, SIDE_HASHES, hash_position,
    },
    search::{
        SearchInfo, alpha_beta, check_interrupt, clear_search_info,
        search_position,
    },
};
pub use crate::game::search::{
    pv_table::{PVElement, PVTable, fill_pv_line, hash_pv_move, probe_pv_move},
    quiescence::quiescence_search,
};
pub use crate::game::util::{
    perft, random_u128, refresh_eval_state, start_perft, verify_game_state,
};

/*----------------------------------------------------------------------------*\
                                   IO API
\*----------------------------------------------------------------------------*/
pub use crate::io::board_io::{
    debug_print_relevant_moves, format_board, format_square,
};
pub use crate::io::game_io::{
    COMMENT_PATTERN, export_tuned_parameters_file, format_entire_game,
    format_game_state, parse_config_file, parse_fen,
    parse_tuned_parameters_file,
};
pub use crate::io::move_io::{
    debug_interactive, format_move, parse_move
};
pub use crate::io::piece_io::format_piece;
pub use crate::io::protocols::uci;

/*----------------------------------------------------------------------------*\
                             EXTERNAL DEPENDENCIES
\*----------------------------------------------------------------------------*/
pub use bnum::types::U4096;
pub use hashbrown::{HashMap, HashSet};
pub use lazy_static::lazy_static;
pub use rand::{RngCore, SeedableRng, seq::SliceRandom};
pub use regex::Regex;
pub use std::sync::Arc;
pub use std::{
    array, collections::VecDeque, fmt::Debug, fs, hash::Hash, io::stdin,
    mem::size_of, sync::Mutex,
};

/*----------------------------------------------------------------------------*\
                               FEATURE EXPORTS
\*----------------------------------------------------------------------------*/
#[cfg(feature = "hotpath")]
pub use hotpath;

/*----------------------------------------------------------------------------*\
                                  CONSTANTS
\*----------------------------------------------------------------------------*/
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

pub fn null_move() -> Move {
    (!0u128, Arc::new(Vec::new()))
}
pub const DEFAULT_DROP: &str = "@#~?@";
pub const NULL_DROP: &str = "@#~?@#~?";

pub const BLACK_WIN: i8 = -1;
pub const WHITE_WIN: i8 = 1;
pub const DRAW: i8 = 0;

pub const OPENING: u8 = 0;
pub const MIDDLEGAME: u8 = 1;
pub const ENDGAME: u8 = 2;

pub const PV_TABLE_SIZE: usize = (1000000 * 8) / size_of::<PVElement>();        /* 8MB                                */
pub const MAX_DEPTH: usize = 64;
