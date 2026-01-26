
use game::{
    constants::*, moves::{move_gen::{chained_atomic_to_vector, move_blockers}, move_parse::parse_move},
    representations::state::State,
    util::verify_game_state,
};

use io::{
    board_io::format_board, game_io::parse_config_file
};

fn init() -> State {
    let state = parse_config_file("configs/test.anm");
    state
}

fn main() {
    // let state = init();

    // verify_game_state(&state);
    // let start: u16 = state.square_to_index(I, 7);
    // let piece = &state.pieces[0];
    // println!(
    //     "{}",
    //     format_board(
    //         &move_blockers(state.pieces[0].movement.as_str(), start, &state),
    //         Some(piece.symbol)
    //     )
    // );
    let expr: String = parse_move(&"nWnWnWnWnWnF");
    println!("Expr: {}", expr);
    println!("{:?}", chained_atomic_to_vector(&expr))
}
