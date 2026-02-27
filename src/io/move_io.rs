use crate::*;

/// a move is uniformly formatted as follows:
/// [drop piece]@:[start]:[end]*[captured_1]...*[captured_n]=[promotion piece]
pub fn format_move(mv: &Move, state: &State) -> String {
    let mut move_str = String::new();

    let move_type = move_type!(mv);

    if move_type == DROP_MOVE {
        let drop_piece = piece!(mv) as usize;
        let piece_char = state.pieces[drop_piece].char;

        move_str.push_str(&format!("{}@", piece_char));
    }

    let start = start!(mv);
    let start_str = format_square(start as u16, state);

    move_str.push_str(&start_str);

    if move_type == QUIET_MOVE {
        let end = end!(mv);
        let end_str = format_square(end as u16, state);

        move_str.push_str(&format!(":{}", end_str));
    }

    if move_type == SINGLE_CAPTURE_MOVE {
        let end = end!(mv);
        let capt = captured_square!(mv);

        if capt != end {
            let end_str = format_square(end as u16, state);
            let capt_str = format_square(capt as u16, state);

            move_str.push_str(&format!(":{}*{}", end_str, capt_str));
        } else {
            let end_str = format_square(end as u16, state);
            move_str.push_str(&format!("*{}", end_str));
        }
    }

    if !mv.1.is_empty() {
        for capture in mv.1.iter() {
            let capt_sq = multi_move_captured_square!(capture);
            let capt_sq_str = format_square(capt_sq as u16, state);

            move_str.push_str(&format!("*{}", capt_sq_str));
        }
    }

    if promotion!(mv) {
        let promo_piece = promoted!(mv) as usize;
        let promo_char = state.pieces[promo_piece].char;

        move_str.push_str(&format!("={}", promo_char));
    }

    move_str
}

pub fn parse_move(move_str: &str, state: &State) -> Option<Move> {
    let all_moves = generate_all_moves_and_drops(state);

    all_moves.into_iter().find(
        |mv| format_move(mv, state).trim() == move_str.trim()
    )
}

pub fn debug_interactive(state: &mut State) {
    let mut input = String::new();

    loop {
        input.clear();
        println!("\n{}", format_game_state(state, true));

        if stdin().read_line(&mut input).is_err() {
            eprintln!("Error reading stdin");
            break;
        }

        match input.trim() {
            "u" => undo_move!(state),
            "q" => break,
            "pv" => {
                fill_pv_line(state, 4);

                for pv_move in &state.pv_line {
                    if pv_move == &NULL_MOVE {
                        break;
                    }

                    let pv_move_str = format_move(pv_move, state);

                    print!("{} ", pv_move_str);
                }

                println!();
            }
            _ => match parse_move(&input, state) {
                Some(mv) => {
                    hash_pv_move(mv.clone(), state);
                    make_move!(state, mv);
                },
                None => eprintln!("Invalid move: {}", input.trim()),
            },
        }
    }
}