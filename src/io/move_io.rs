//! # move_io.rs
//!
//! Implements move formatting, move parsing, and interactive move debugging.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 04/02/2026


use crate::*;

/*----------------------------------------------------------------------------*\
                          MOVE STRING REPRESENTATION
\*----------------------------------------------------------------------------*/

/// Formats a move into the engine's canonical text representation.
///
/// Canonical grammar:
/// - `[drop_piece]@` (optional drop prefix)
/// - `[start]` (always present)
/// - `:[end]` for quiet and multi-capture moves
/// - `*[capture_square]` for capture segments (single or repeated)
/// - `=[promotion_piece]` for promotions
///
/// Effective shape:
/// `[drop_piece]@[start]:[end]*[captured_1]...*[captured_n]=[promotion]`
///
/// The resulting string is used for user-facing display and as the matching
/// key for `parse_move` when validating textual input.
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

        let end = end!(mv);
        let end_str = format_square(end as u16, state);

        move_str.push_str(&format!(":{}", end_str));

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

/// Parses a textual move by matching against generated legal moves.
///
/// The input is compared to `format_move` output for each candidate and
/// returns the first exact match after trimming whitespace.
pub fn parse_move(move_str: &str, state: &State) -> Option<Move> {
    let all_moves = generate_all_moves_and_drops(state);

    all_moves.into_iter().find(
        |mv| format_move(mv, state).trim() == move_str.trim()
    )
}

/// Runs a simple stdin-driven debug loop for making and undoing moves.
///
/// Supported commands include normal move strings, `u` (undo), `q` (quit),
/// and `pv` subcommands for PV hash probing/display.
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
            input if input.starts_with("pv") => {
                let parts = input.split_whitespace().collect::<Vec<_>>();

                if parts.len() < 3 {
                    eprintln!("Usage: pv [hash/show] [args]");
                    continue;
                }

                let command = *parts.get(1).unwrap();
                let args = *parts.get(2).unwrap();

                match command {
                    "hash" => {
                        if let Some(mv) = parse_move(args, state) {
                            hash_pv_move(mv.clone(), state);
                            make_move!(state, mv);
                        } else {
                            eprintln!("Invalid move for hashing: {}", args);
                        }
                    }
                    "show" => {
                        let depth = args.parse::<usize>().unwrap_or(0);
                        fill_pv_line(state, depth);

                        for pv_move in &state.pv_line {
                            if pv_move == &null_move() {
                                break;
                            }

                            let pv_move_str = format_move(pv_move, state);

                            print!("{} ", pv_move_str);
                        }

                        println!();
                        state.pv_line = std::array::from_fn(|_| null_move());
                    }
                    _ => {
                        eprintln!("Unknown pv command: {}", command);
                    }
                }
            }
            _ => match parse_move(&input, state) {
                Some(mv) => {
                    make_move!(state, mv);
                },
                None => eprintln!("Invalid move: {}", input.trim()),
            },
        }
    }
}
