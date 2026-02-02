use crate::{constants::*, game::{moves::move_list::generate_move_list, representations::{moves::MoveType, state::State}}};

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

#[hotpath::main(limit=0)]
fn main() {
    let state = State::from_config("configs/fide.anm");

    let mut possible_moves: Vec<MoveType> = vec![];

    for piece in &state.pieces {
        let i = piece.index() as usize;
        let piece_board = &state.pieces_board[i];
        let piece_indices = piece_board.bit_indices();

        for index in piece_indices {
            let relevant_friendly_board = if piece.color() == WHITE {
                &state.white_board
            } else {
                &state.black_board
            };

            let relevant_enemy_board = if piece.color() == WHITE {
                &state.black_board
            } else {
                &state.white_board
            };

            let square = state.index_to_square(index as u16);

            let mut piece_moves = generate_move_list(square, &piece, &relevant_friendly_board, &relevant_enemy_board, &state.unmoved_board, &state);

            possible_moves.append(&mut piece_moves);
        }
    }

    println!("Total possible moves: {}", possible_moves.len());
}
