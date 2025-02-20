use io;
use game::board::Board;

fn main() {
    let mut board: Board = Board::new(8, 8);

    board.set_bit(0, 0);
    board.set_bit(7, 7);

    io::print_board(&board);
}
