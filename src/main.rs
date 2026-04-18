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

#[hotpath::main]
fn main() {
    let variant = "fide";
    let config_path = format!("configs/{}.conf", variant);
    let perft_path = format!("res/{}.perft", variant);

    let mut state = parse_config_file(&config_path);

    println!("{}", format_entire_game(&state));

    // start_perft(&mut state, &perft_path, 6, -1, 3);
    debug_interactive(&mut state);
}
