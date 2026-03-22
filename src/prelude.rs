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

/// Core types
pub use crate::game::representations::{
    board::Board,
    piece::{Piece, PieceIndex},
    state::{State, Square, EnPassantSquare, Snapshot},
    drop::DropSet,
    moves::{Move, AttackMask},
    vector::{
        Leg, LegVector, MultiLegVector, MoveSet, MoveVector,
        AtomicElement::{self, AtomicEval, AtomicExpr, AtomicTerm},
        AtomicVector, AtomicGroup,
        Token::{
            self, CardinalToken, ColonToken, FilterToken, AtomicToken,
            DotsToken, RangeToken, BracketToken, SlashBracketToken,
            ExclusionToken, MoveModifierToken, LegToken
        },
        MultiLegElement::{
            self, MultiLegEval, MultiLegExpr, MultiLegSlashExpr, MultiLegTerm
        },
        MultiLegGroup,
    },
};

/// Game logic
pub use crate::game::util::{
    random_u128, start_perft, perft, verify_game_state
};
pub use crate::game::position::hash::{
    hash_position, CASTLING_HASHES, EN_PASSANT_HASHES, SIDE_HASHES,
    PIECE_HASHES, IN_HAND_HASHES, PositionHash
};
pub use crate::game::moves::move_list::{
    generate_move_list, generate_relevant_moves, generate_attack_masks,
    generate_all_moves_and_drops
};
pub use crate::game::moves::move_parse::generate_move_vectors;
pub use crate::game::drops::drop_list::{
    generate_drop_list, generate_relevant_drops
};
pub use crate::game::drops::drop_parse::generate_drop_vectors;
pub use crate::game::patterns::pattern_match::{
    PatternSet, PatternAllower, PatternStopper, generate_relevant_stand_offs,
    generate_stand_off_patterns, parse_pattern, match_pattern
};
pub use crate::game::search::pv_table::{
    PVTable, PVElement,
    hash_pv_move, probe_pv_move, fill_pv_line
};

/// IO
pub use crate::io::board_io::{
    format_board, format_square, debug_print_relevant_moves
};
pub use crate::io::piece_io::format_piece;
pub use crate::io::game_io::{
    parse_config_file, parse_fen, format_game_state, format_entire_game,
    COMMENT_PATTERN
};
pub use crate::io::move_io::{
    format_move, parse_move as parse_move_io, debug_interactive
};
pub use crate::io::protocols::uci;

/// Macros
pub use crate::{
    board, files, ranks, get, set, clear, or, and, xor, not,
    p_index, p_color, p_can_promote, p_is_royal, p_is_big, p_is_major,
    p_is_minor, p_value, p_castle_right, p_castle_left,
    castling, en_passant, promotions, drops, count_limits, forbidden_zones,
    promote_to_captured, demote_upon_capture, stalemate_loss, setup_phase,
    stand_offs, enp_square, enp_captured, enp_piece, null_snapshot,
    drop_k, drop_f, drop_d, drop_e,
    hash_in_or_out_piece, hash_toggle_side, hash_update_castling,
    hash_update_en_passant, hash_update_in_hand,
    is_square_attacked, is_in_check, make_move, validate_attack_vector,
};

/// External crates
pub use lazy_static::lazy_static;
pub use bnum::types::U4096;
pub use regex::Regex;
pub use hashbrown::{HashMap, HashSet};
pub use std::{
    fs, array, sync::Mutex, io::stdin, hash::Hash, collections::VecDeque,
    mem::size_of
};
pub use rand::{RngCore, SeedableRng, seq::SliceRandom};

/// Hotpath feature
#[cfg(feature = "hotpath")]
pub use hotpath;

// Constant definitions for board dimensions, piece colors, castling rights,
// file/rank labels, and other game-related configuration values. These
// constants are used throughout the codebase to ensure consistency and allow
// for easy modification of game parameters.
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

pub const WHITE : u8 = 0;
pub const BLACK : u8 = 1;
pub const BOTH : u8 = 2;

pub const WK_CASTLE : u8 = 0b0001;
pub const WQ_CASTLE : u8 = 0b0010;
pub const BK_CASTLE : u8 = 0b0100;
pub const BQ_CASTLE : u8 = 0b1000;
pub const CASTLING: [u8; 4] = [WK_CASTLE, WQ_CASTLE, BQ_CASTLE, BK_CASTLE];

pub const NO_PIECE: u8 = u8::MAX;
pub const NO_EN_PASSANT: u32 = u32::MAX;

pub const QUIET_MOVE: u128 = 0;
pub const SINGLE_CAPTURE_MOVE: u128 = 1;
pub const MULTI_CAPTURE_MOVE: u128 = 2;
pub const DROP_MOVE: u128 = 3;

pub const NULL_MOVE: Move = (!0u128, Vec::new());
pub const DEFAULT_DROP: &str = "@#~?@";
pub const NULL_DROP: &str = "@#~?@#~?";

pub const BLACK_WIN: i8 = -1;
pub const WHITE_WIN: i8 = 1;
pub const DRAW: i8 = 0;

pub const PV_TABLE_SIZE: usize = (1000000 * 8) / size_of::<PVElement>();        /* 8MB                                */
pub const MAX_DEPTH: usize = 64;
