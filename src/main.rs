use crate::{constants::*, game::{moves::{move_list::generate_move_list, perft::start_peft}, representations::{moves::MoveType, state::State}}};

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
}

pub mod constants;

#[hotpath::main(limit=0)]
fn main() {
    let state = State::from_config("configs/fide.anm");

    start_peft(&state, "res/fide.perft", 1);
}
