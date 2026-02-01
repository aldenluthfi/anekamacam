use game::{
    constants::{C, D, E, I}, moves::{move_list::{generate_move_list, generate_relevant_boards}, move_parse::generate_move_vectors}, representations::{board::Board, state::State}
};

use io::{board_io::format_board, game_io::{combine_board_strings, parse_config_file}};

#[hotpath::main]
fn main() {
    State::init_global(parse_config_file("configs/test.anm"));
    let state= State::global();
    // println!("{:?}", generate_move_vectors(&"mF|im<nF-pnF>|c[137]K"));
    // println!("{:?}", generate_move_vectors(&"F(#|((#|eW)-<wW-eW>|(#|wW)-<eW-wW>)-*)"));
    // println!("{:#?}", generate_move_vectors(&"FnF"));
    // println!("{:?}", generate_move_vectors(&"nWnF"));
    // let vecs = generate_move_vectors(&"<WneF-WneF>");
    // let vecs = generate_move_vectors(&"m<W:{1..2}neF..-<mn<[1]N>-N>>-:{3}-N");
    // let vecs = generate_move_vectors(&"WnWe+wW");
    let vecs = generate_move_vectors(&"cdW-nF");
    println!("{:#?}", vecs);
    let mut enemy = Board::new(state.files, state.ranks);
    let mut friendly = Board::new(state.files, state.ranks);

    enemy.set_bit(E, 3);
    enemy.set_bit(C, 3);

    let square = (D, 3);

    let relevant = generate_relevant_boards(&vecs, square);

    let masked_enemy = &relevant & &enemy;
    let masked_friendly = &relevant & &friendly;

    println!("{}", format_board(&relevant, Some('R')));
    println!("{}", format_board(&masked_friendly, Some('F')));
    println!("{}", format_board(&masked_enemy, Some('E')));

    let moves = generate_move_list(&vecs, square, &masked_friendly, &masked_enemy);

    println!("{:#?}", moves);
}
