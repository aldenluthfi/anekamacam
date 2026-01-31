use game::{
    constants::I, moves::move_parse::generate_move_vectors, representations::{board::Board, state::State}
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
    let vecs = generate_move_vectors(&"R");
    println!("{:#?}", vecs);
    let mut initialb = Board::new(state.files, state.ranks);
    initialb.set_bit(I, 7);
    let mut moveb = Board::new(state.files, state.ranks);

    let files = I as i8;
    let ranks = 7 as i8;
    for v in vecs {
        let mut zero = (0, 0);
        for v_ in v {
            zero.0 += v_.get_atomic().whole().0;
            zero.1 += v_.get_atomic().whole().1;
        }

        let (df, dr) = (zero.0, zero.1);
        let new_file = files + df;
        let new_rank = ranks + dr;

        if new_file >= 0 && new_file < state.files as i8 && new_rank >= 0 && new_rank < state.ranks as i8 {
            moveb.set_bit(new_file as u8, new_rank as u8);
        }
    }

    let i_str = format_board(&initialb, Some('O'));
    let m_str = format_board(&moveb, Some('X'));

    println!("{}", combine_board_strings(&i_str, &m_str));
}
