//! main.rs
//!
//! Entry point for the anekamacam engine and the root of its module tree.
//! Reads the first CLI argument to pick a run mode -- the UCI protocol loop
//! (also the default), the interactive debug console, or the derive / bench /
//! perft utilities -- initializing logging before handing off to that mode.
//!
//! Created: 01/02/2026
//! Author : Alden Luthfi

#![feature(sync_unsafe_cell)]
use prelude::*;

pub mod game {
    pub mod representations {
        pub mod board;

        pub mod drop;
        pub mod moves;
        pub mod pattern;

        pub mod piece;
        pub mod state;
        pub mod vector;
    }

    pub mod moves {
        pub mod move_list;
        pub mod move_parse;

        pub mod drop_list;
        pub mod drop_parse;

        pub mod pattern_match;
        pub mod pattern_parse;
    }

    pub mod search {
        pub mod move_ordering;
        pub mod parallel;
        pub mod parameters;
        pub mod transposition;
    }

    pub mod position {
        pub mod evaluation;
        pub mod hash;
        pub mod search;
    }

    pub mod util;
}

pub mod io {
    pub mod board_io;
    pub mod piece_io;
    pub mod game_io;
    pub mod move_io;

    pub mod protocols {
        pub mod translation;
        pub mod uci;
    }

    pub mod logger;
}

pub mod debug {
    pub mod console;

    pub mod datagen;
    pub mod sprt;
    pub mod tuning;
}

pub mod prelude;

/// main
///
/// Dispatches on the first CLI argument to one of the engine's modes:
/// - `uci`    : the UCI protocol loop (also the no-argument default)
/// - `debug`  : the interactive ratatui debug console
#[hotpath::main]
fn main() {
    init_logging();

    let args: Vec<String> = env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("uci") => { let _ = uci(); }
        Some("debug") => {
            DEBUG_FLAG.store(true, Ordering::Relaxed);
            let _ = debug_console();
        }
        _ => { let _ = uci(); }
    }
}
