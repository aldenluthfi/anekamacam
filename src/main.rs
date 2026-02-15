use crate::{
    game::moves::perft::start_perft,
    io::game_io::{format_entire_game, parse_config_file},
};

pub mod game {
    pub mod representations {
        pub mod board;
        pub mod piece;
        pub mod state;
        pub mod moves;
        pub mod vector;
    }

    pub mod moves {
        pub mod move_parse;
        pub mod move_list;
        pub mod perft;
    }

    pub mod hash;
    pub mod util;
}

pub mod io {
	pub mod board_io;
	pub mod game_io;
	pub mod piece_io;
    pub mod move_io;
}

pub mod constants;

#[hotpath::main(limit=0)]
fn main() {
    let mut state = parse_config_file("configs/xianqi.toml");

    // println!("{}", format_entire_game(&state));

    start_perft(&mut state, "res/xianqi.perft", 5, false, Some(1));
}
