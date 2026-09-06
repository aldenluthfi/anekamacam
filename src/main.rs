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

/*----------------------------------------------------------------------------*\
                                  MODULE TREE
\*----------------------------------------------------------------------------*/

/// game
///
/// The engine proper: what a position is, what moves it offers, what those
/// moves are worth, and which one to play. Nothing under here knows about a
/// terminal or a protocol, and nothing under here is written for one variant.
///
/// ```text
/// representations   boards, pieces, moves, drops, patterns, termination
/// moves             generating moves and drops, and parsing what defines
///                   them: the pattern, drop, and move-notation grammars
/// search            move ordering, threads, tuned parameters, the table
/// position          hashing, evaluating, and searching a single position
/// util              the small helpers the rest of the engine leans on
/// ```
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
/// Everything that turns engine state into text and text back into engine
/// state. Each reader has a writer beside it, so a value that went out one
/// way comes back in the same way.
///
/// ```text
/// board_io    boards, squares, and the diagrams they are drawn as
/// piece_io    piece letters and the names behind them
/// game_io     configuration files, tuned parameters, CFEN, state display
/// move_io     moves and drops, in notation and back
/// protocols   the session loop, the dialects that drive it, and the
///             dictionaries that let one engine answer in three notations
/// logger      levelled output, shared by every mode below
/// ```
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
/// Tooling that is not part of playing a game: two ways to drive the engine
/// by hand, and three long-running jobs that produce and test the numbers it
/// plays with.
///
/// ```text
/// graphics   the ratatui frontend, a board and its diagnostics
/// headless   the same command set with no screen, one line at a time
/// datagen    self-play games written out as training positions
/// sprt       two builds played against each other until one is better
/// tuning     fitting the evaluation's parameters to those positions
/// ```
pub mod debug {
    pub mod graphics;
    pub mod headless;

    pub mod datagen;
    pub mod sprt;
    pub mod tuning;
}

/// prelude
///
/// The one import every other file makes. It re-exports the whole module
/// tree above along with the externals the engine leans on, so no file has
/// to spell out a path that some other file has already spelled out.
pub mod prelude;

/*----------------------------------------------------------------------------*\
                                  ENTRY POINT
\*----------------------------------------------------------------------------*/

/// main
///
/// Picks a mode from the first command-line argument and hands control over.
/// Logging is started before the argument is even read, so whichever mode
/// takes over has somewhere to write from its opening line onward.
///
/// ```text
/// debug-graphics   the interactive board, with the debug flag set so the
///                  engine reports what it is doing while it does it
/// debug-headless   the same tooling with no screen, the rest of the line
///                  passed on as the command
/// anything else    the text protocol loop, and no argument at all lands
///                  here too, that being how the engine is normally run
/// ```
///
/// Notes:
///
/// The headless arm wraps its call in `with_stdout_sink` because the tools
/// behind it speak through the engine's event sink rather than printing.
/// Without a printer listening on the other end their output would go
/// nowhere at all, so one is installed for the length of the call and joined
/// afterwards to be sure the last line has been flushed.
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
