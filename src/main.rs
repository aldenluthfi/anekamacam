use crate::game::{moves::move_parse::generate_move_vectors, representations::state::State};

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

#[hotpath::main]
fn main() {
    let state = State::from_config("configs/fide.anm");

    println!("Knight Moves: {}", state.piece_relevant_boards[6].len());
}
