use crate::game::{
    moves::perft::start_perft,
    representations::state::State
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

#[hotpath::main(limit=30)]
fn main() {
    let mut state = State::from_config("configs/fide.anm");

    start_perft(&mut state, "res/fide-kiwipete.perft", 5, true, Some(2));
}
