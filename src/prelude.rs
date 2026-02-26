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
pub use crate::constants::*;
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
pub use crate::game::hash::zobrist::{
    hash_position, CASTLING_HASHES, EN_PASSANT_HASHES, SIDE_HASHES,
    PIECE_HASHES, IN_HAND_HASHES
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
    format_move, parse_move as parse_move_io, interactive_debug
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
    fs, array, sync::Mutex, io::stdin, hash::Hash, collections::VecDeque
};
pub use rand::{RngCore, SeedableRng, seq::SliceRandom};

/// Hotpath feature
#[cfg(feature = "hotpath")]
pub use hotpath;