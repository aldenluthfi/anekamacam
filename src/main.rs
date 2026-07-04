//! # main.rs
//!
//! Entry point and top-level module declarations for the anekamacam engine.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 01/02/2026

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
}

pub mod prelude;

/// main
///
/// Dispatches on the first CLI argument to one of the engine's modes:
/// - `uci`    : the UCI protocol loop (also the no-argument default)
/// - `debug`  : the interactive ratatui debug console
/// - `derive` : derive and export parameters for a variant
/// - `bench`  : fixed-depth search benchmark on a variant's startpos
/// - `perft`  : run a variant's embedded perft suite with an optional
///   depth and position limit
///
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
        Some("derive") => {
            let variant = args.get(2).map(|s| s.as_str()).unwrap_or("fide");
            let mut state = parse_config_file(&format!("{}.conf", variant));
            derive_parameters(&mut state);
            export_tuned_parameters_file(&state, variant);
        }
        Some("bench") => {
            let variant = args.get(2).map(|s| s.as_str()).unwrap_or("fide");
            let depth = args.get(3)
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(10);
            let mut state = parse_config_file(&format!("{}.conf", variant));
            let startpos = state.statics.startpos.clone();
            state.reset();
            parse_fen(&mut state, &startpos, None);
            refresh_eval_state(&mut state);
            let ttable = Arc::new(TTable::with_mb(HASH_DEFAULT_MB * 2 / 3));
            let qtable = Arc::new(QTable::with_mb(HASH_DEFAULT_MB / 3));
            benchmark_search(&mut state, ttable, qtable, depth, 1, None);
        }
        Some("perft") => {
            let variant = args.get(2).map(|s| s.as_str()).unwrap_or("fide");
            let depth = args.get(3)
                .and_then(|s| s.parse::<u8>().ok())
                .unwrap_or(4);
            let limit = args.get(4)
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(usize::MAX);
            let mut state = parse_config_file(&format!("{}.conf", variant));
            let content = EMBEDDED_PERFT
                .get_file(format!("{}.perft", variant))
                .and_then(|f| f.contents_utf8())
                .unwrap_or_else(|| {
                    panic!("no embedded perft suite for {variant}")
                });
            let (passed, total) = benchmark_perft(
                &mut state, content, depth, -1, limit, None
            );
            println!("perft {}: {}/{} cases passed", variant, passed, total);
        }
        _ => { let _ = uci(); }
    }
}
