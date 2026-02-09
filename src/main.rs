use crate::{
    game::{moves::move_list::generate_move_list, util::verify_game_state}, io::{board_io::format_board, game_io::{format_entire_game, parse_config_file}, move_io::format_move_template}
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
    let state = parse_config_file("configs/fide.anm");
}
