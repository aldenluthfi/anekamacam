//! # main.rs
//!
//! Entry point and top-level module declarations for the anekamacam engine.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 01/02/2026

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

    pub mod patterns {
        pub mod pattern_match;
    }

    pub mod drops {
        pub mod drop_list;
        pub mod drop_parse;
    }

    pub mod moves {
        pub mod move_list;
        pub mod move_parse;
    }

    pub mod search {
        pub mod move_ordering;
        pub mod tt_table;
        pub mod quiescence;
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
    pub mod piece_io;

    pub mod logger;
}

pub mod prelude;

#[hotpath::main]
fn main() {
    init_logging();

    let variant = "fide";
    let config_path = format!("configs/{}.conf", variant);

    info!("Loading variant config: {}", config_path);
    let mut state = parse_config_file(&config_path);

    info!("{}", format_entire_game(&state, FORMAT_VERBOSITY_DEBUG));

    debug_interactive(&mut state);
}
