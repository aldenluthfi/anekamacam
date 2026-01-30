use game::{
    constants::I, moves::move_parse::generate_move_vectors, representations::{board::Board, state::State}
};

use io::{board_io::format_board, game_io::{combine_board_strings, parse_config_file}};

#[hotpath::main]
fn main() {
    State::init_global(parse_config_file("configs/test.anm"));
    let state= State::global();
    // println!("{:?}", generate_move_vectors(&"mF|im<nF-pnF>|c[137]K"));
    // println!("{:?}", generate_move_vectors(&"F(#|((#|eW)-</wW-eW/>|(#|wW)-</eW-wW/>)-*)"));
    // println!("{:?}", generate_move_vectors(&"<<W-</W-nC/>>>"));
    // println!("{:?}", generate_move_vectors(&"</WneF/>-</WneF/>"));
    let vecs = generate_move_vectors(&"WneF<[18]N>");
    let mut initialb = Board::new(state.files, state.ranks);
    initialb.set_bit(I, 7);
    let mut moveb = Board::new(state.files, state.ranks);

    let files = I as i8;
    let ranks = 7 as i8;
    for v in vecs {
        let whole = v.whole();
        let (df, dr) = (whole.0, whole.1);
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
