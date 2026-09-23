//! main.rs
//!
//! Entry point of the anekamacam engine and root of the module tree.
//! The first command line argument selects the protocol loop, the graphic
//! debug console or the headless debug tools. The protocol loop is the
//! default.
//!
//! Created: 01/02/2026
//! Author : Alden Luthfi

#![feature(sync_unsafe_cell)]
use prelude::*;

/*----------------------------------------------------------------------------*\
                                  MODULE TREE
\*----------------------------------------------------------------------------*/

/// game
///
/// The engine core: positions, moves, evaluation and search. No module
/// here knows about a terminal, a protocol or one specific variant.
///
/// - representations : boards, pieces, moves, drops, patterns, termination
/// - moves           : move and drop generation, CPMN and drop parsers
/// - search          : move ordering, threads, parameters, hash table
/// - position        : hashing, evaluation and search of a position
/// - util            : small helpers for the other modules
///
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

/// io
///
/// Converts engine state to text and text to engine state. Each reader
/// has a related writer.
///
/// - board_io  : squares, boards and board diagrams
/// - piece_io  : piece colour pairs and packed piece values
/// - game_io   : configuration files, parameters, FEN, state display
/// - move_io   : move and drop notation
/// - protocols : session loop, protocol dialects and notation dictionaries
/// - logger    : log output for all modes
///
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

/// debug
///
/// Tools that are not part of game play. Two tools let a person control
/// the engine. Three tools make and test the engine parameters.
///
/// - graphics : ratatui console with a board and diagnostics
/// - headless : the same commands without a screen
/// - datagen  : self-play games written as training positions
/// - sprt     : match between two builds until one is better
/// - tuning   : fit of evaluation parameters to training positions
///
pub mod debug {
    pub mod graphics;
    pub mod headless;

    pub mod datagen;
    pub mod sprt;
    pub mod tuning;
}

/// prelude
///
/// The common import of all other files. It exports the module tree and
/// the external crates again, so no file writes the full paths.
///
pub mod prelude;

/*----------------------------------------------------------------------------*\
                                  ENTRY POINT
\*----------------------------------------------------------------------------*/

/// main
///
/// Selects a mode from the first command line argument and runs it. The
/// function starts logging first, so each mode can write output at once.
///
/// - `debug-graphics` : interactive console, with the debug flag set
/// - `debug-headless` : the same tools without a screen, rest is a command
/// - other or none    : protocol loop, the normal mode
///
/// Notes:
/// The headless tools write to the event sink, not to stdout. Thus the
/// headless call runs in `with_stdout_sink`, which prints the events and
/// flushes the last line at the end.
///
#[hotpath::main(functions_limit = 0)]
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
