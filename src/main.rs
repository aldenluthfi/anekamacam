use crate::{
    game::moves::perft::start_perft,
    io::game_io::parse_config_file,
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
    let mut state = parse_config_file("configs/fide.anm");

    start_perft(&mut state, "res/fide-initial.perft", 6);

    // let moves = generate_all_moves(&state);

    // let mut count = 0;
    // for mv in moves {
    //     if make_move(&mut state, mv.clone()) {
    //         if piece!(mv) == 5 {
    //             count += 1;
    //         }

    //         undo_move(&mut state);
    //     }

    // }
    // println!("{}", count);
}
