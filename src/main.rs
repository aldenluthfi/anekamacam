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
        pub mod pv_table;
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

    pub mod protocols {
        pub mod uci;
    }
}

pub mod prelude;

fn configured_log_level() -> log::LevelFilter {
    if cfg!(feature = "log-level-debug") {
        log::LevelFilter::Debug
    } else if cfg!(feature = "log-level-info") {
        log::LevelFilter::Info
    } else if cfg!(feature = "log-level-warn") {
        log::LevelFilter::Warn
    } else if cfg!(feature = "log-level-error") {
        log::LevelFilter::Error
    } else {
        log::LevelFilter::Info
    }
}

#[hotpath::main]
fn main() {
    env_logger::Builder::new()
        .filter_level(configured_log_level())
        .format_timestamp_millis()
        .init();

    let variant = "fide";
    let config_path = format!("configs/{}.conf", variant);
    let perft_path = format!("res/{}.perft", variant);

    info!("Loading variant config: {}", config_path);
    let mut state = parse_config_file(&config_path);

    info!("{}", format_entire_game(&state, FORMAT_VERBOSITY_DEBUG));

    // start_perft(&mut state, &perft_path, 6, -1, 3);
    debug_interactive(&mut state);
}
