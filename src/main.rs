use crate::{
    game::moves::perft::{perft, start_perft},
    io::game_io::{debug_perform_moves, format_entire_game, format_game_state, parse_config_file},
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
    state.load_fen("3k5/3ca4/3a5/9/2P6/9/7p1/2p1C4/4A4/4KC3 b -/-");

    debug_perform_moves(&mut state, &["e9f9a", "f1f10", "d10e9", "e1f1Q"]);
    println!("State after moves:");
    println!("{}", format_game_state(&state, false));

    perft(&mut state, 1, 1, "");

    // start_perft(&mut state, &perft_path, 5, -1, 100);
}
