use crate::representations::{
    board::Board, piece::Piece, state::State
};

fn generate_magic_candidate() -> Board {
    let game_state = State::global();

    let files = game_state.files;
    let ranks = game_state.ranks;

    Board::random_board(files, ranks) &
    Board::random_board(files, ranks) &
    Board::random_board(files, ranks)
}

fn find_magic_number(
    square: (i8, i8),
    piece: Piece,
) -> Board {
    unimplemented!()
}
