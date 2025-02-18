use io;
use game::board::Board;

fn main() {
    let mut board: Board = Board::new(10, 12);

    board.set_bit(0, 0);
    board.set_bit(0, 1);

    io::print_board(&board);
}
