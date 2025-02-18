use game::board::Board;

pub fn print_board(board: &Board) {
    println!("{}", board.to_string());
}
