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
    }

    pub mod search {
        pub mod move_ordering;
        pub mod parallel;
        pub mod transposition;
        pub mod qsearch_tt;
        pub mod parameters;
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
    pub mod game_io;
    pub mod move_io;

    pub mod logger;
    pub mod tui;
}

pub mod prelude;

#[hotpath::main]
fn main() {
    init_logging();
    let _ = tui();
}
