use game::moves::move_parse::parse_move;

fn main() {
    println!("{}", parse_move("F(#|((#|eW)-</wW-eW/>|(#|wW)-</eW-wW/>)-*)"));
}
