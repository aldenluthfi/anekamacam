//! main.rs
//!
//! Entry point for the anekamacam engine and the root of its module tree.
//! Reads the first CLI argument to pick protocol, graphical debug, or nested
//! headless debug tooling. Initializes logging before handing control to the
//! selected frontend while protocol mode remains the default.
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
        pub mod termination;

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
        pub mod protocol;

        pub mod uci;
        pub mod usi;
        pub mod ucci;
    }

    pub mod logger;
}

pub mod debug {
    pub mod graphics;
    pub mod headless;

    pub mod datagen;
    pub mod sprt;
    pub mod tuning;
}

pub mod prelude;

/// main
///
/// Dispatches on the first CLI argument to one of the engine's modes:
///
/// - (default)        : text-protocol loop
/// - `debug-graphics` : interactive ratatui debug frontend
/// - `debug-headless` : nested non-graphical debug and tooling commands
#[hotpath::main]
fn main() {
    init_logging();

    let arguments: Vec<String> = env::args().collect();
    match arguments.get(1).map(|value| value.as_str()) {
        Some("debug-graphics") => {
            DEBUG_FLAG.store(true, Ordering::Relaxed);
            let _ = run_debug_graphics();
        }
        Some("debug-headless") => {
            with_stdout_sink(|| run_debug_headless(&arguments[2..]));
        }
        _ => {
            let _ = run();
        }
    }
}
