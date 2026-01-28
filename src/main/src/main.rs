use game::{
    constants::I, moves::move_parse::{
        chained_atomic_to_vector, cleanup_atomic_vectors, compound_atomic_to_vector, parse_move
    }, representations::{board::Board, state::State}
};

use io::{board_io::format_board, game_io::parse_config_file};

fn main() {
    State::init_global(parse_config_file("configs/test.anm"));
    let state = State::global();

    let expr: String = parse_move(&"<n<N>.nW>");

    println!("Expr: {}", expr);
    let raw_vecs = compound_atomic_to_vector(&expr, "n");
    println!("Raw Vectors: {:?}", raw_vecs);
    let vecs = cleanup_atomic_vectors(raw_vecs);
    println!("Cleaned Vectors: {:?}", vecs);

    let mut board = Board::new(state.files, state.ranks);

    board.set_bit(I, 7);

    for (f, r) in vecs {
        println!("Setting bit at file {}, rank {}", f, r);
        board.set_bit((I as i8 + f) as u8, (7 + r) as u8);
    }

    println!("{}", format_board(&board, Some('X')));


}
