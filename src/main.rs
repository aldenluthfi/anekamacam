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
        pub mod protocol;
        pub mod uci;
        pub mod usi;
        pub mod ucci;
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
/// - `usi`    : the USI protocol loop (shogi family)
/// - `ucci`   : the UCCI protocol loop (xiangqi family)
/// - `debug`  : the interactive ratatui debug console
/// - `derive` : headless parameter derivation for every embedded config
#[hotpath::main]
fn main() {
    init_logging();

    let args: Vec<String> = env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("uci") => { let _ = uci(); }
        Some("usi") => { let _ = usi(); }
        Some("ucci") => { let _ = ucci(); }
        Some("debug") => {
            DEBUG_FLAG.store(true, Ordering::Relaxed);
            let _ = debug_console();
        }
        Some("derive") => derive_all_variants(),
        _ => { let _ = uci(); }
    }
}

/// derive_all_variants
///
/// Loads every embedded variant config in turn through the same
/// embedded-first parameter path the engine uses at startup. Variants
/// without a parameter payload derive one and export it to
/// `res/param/{variant}/latest.param`, so a delete-then-derive cycle
/// regenerates every shipped file without the interactive console.
fn derive_all_variants() {
    for config in EMBEDDED_CONFIGS.files() {
        let Some(filename) = config.path().to_str() else {
            continue;
        };

        if !filename.ends_with(".conf") || filename == "example.conf" {
            continue;
        }

        println!("deriving {}", filename);
        let _ = parse_config_file(filename);
    }
}
