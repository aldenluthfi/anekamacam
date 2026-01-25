
use game::representations::state::State;
use io::game_io::{format_game_state, parse_config_file};

fn init() -> State {
    let state = parse_config_file("configs/fide.anm");
    state
}

fn main() {
    let state = init();

    println!("{}", format_game_state(&state, true));
}
