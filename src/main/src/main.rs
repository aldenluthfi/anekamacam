use game::{
    moves::move_parse::{
        parse_move,
        compound_atomic_to_vector,
        chained_atomic_to_vector,
    },
    representations::state::State,
};

use io::game_io::parse_config_file;

fn main() {
    State::init_global(parse_config_file("configs/test.anm"));

    // let expr: String = parse_move(&"n<<nNnW>nF>");
    let expr: String = parse_move(&"<FnF>nW");

    println!("Expr: {}", expr);
    for vec in compound_atomic_to_vector(&expr, "n") {
        println!("{:?}", vec);
    }
}
