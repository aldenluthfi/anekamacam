use game::move_parse::parse_move;

fn main() {
    println!("{}", parse_move("(cQ|dQ-u#)-mnW"));
}
