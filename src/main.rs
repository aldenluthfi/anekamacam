use crate::{
    game::moves::perft::{perft, start_perft},
    io::game_io::{debug_perform_moves, format_entire_game, format_game_state, parse_config_file},
    game::patterns::pattern_match::match_pattern,
};

pub mod game {
    pub mod representations {
        pub mod board;
        pub mod piece;
        pub mod state;
        pub mod moves;
        pub mod vector;
        pub mod drop;
    }

    pub mod patterns {
        pub mod pattern_match;
    }

    pub mod drops {
        pub mod drop_list;
        pub mod drop_parse;
    }

    pub mod moves {
        pub mod move_parse;
        pub mod move_list;
        pub mod perft;
    }

    pub mod search {
        pub mod quiescence;
    }

    pub mod evaluation {
        pub mod r#static;
    }

    pub mod hash {
        pub mod zobrist;
    }
    pub mod util;
}

pub mod io {
	pub mod board_io;
	pub mod game_io;
	pub mod piece_io;
    pub mod move_io;

    pub mod protocols {
        pub mod uci;
    }
}

pub mod constants;

#[hotpath::main(limit=0)]
fn main() {
    let variant = "janggi";
    let config_path = format!("configs/{}.conf", variant);
    let perft_path = format!("res/{}.perft", variant);

    let mut state = parse_config_file(&config_path);
    // state.load_fen("rhea1aehr/4k4/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/4K4/RHEA1AEHR w -/-");

    // debug_perform_moves(&mut state, &["e4d4", "e7d7"]);
    // println!("State after moves:");
    // println!("{}", format_game_state(&state, false));

    // perft(&mut state, 2, 1, "");

    start_perft(&mut state, &perft_path, 5, -1, 100);
}
