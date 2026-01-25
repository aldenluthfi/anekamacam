use std::fs;
use game::hash::hash_position;
use game::representations::{
    state::State,
    piece::Piece,
    board::Board,
};
use game::constants::*;
use crate::board_io::format_board;

pub fn parse_config_file(path: &str) -> State {
    let contents = fs::read_to_string(path)
        .expect(&format!("Failed to read config file: {}", path));

    let lines: Vec<&str> = contents.lines().collect();
    assert!(lines.len() >= 2, "Config file must have at least 2 lines");

    let first_line: Vec<&str> = lines[0].split(',').collect();                  /* first line is rank,file,piecetypes */
    assert!(
        first_line.len() == 3,
        "First line must have 3 comma-separated values."
    );

    let ranks: u8 =
        first_line[0]
            .trim()
            .parse()
            .expect("Invalid ranks");
    let files: u8 =
        first_line[1]
            .trim()
            .parse()
            .expect("Invalid files");
    let pieces_num: usize =
        first_line[2]
            .trim()
            .parse()
            .expect("Invalid piece types");

    assert!(
        lines.len() >= pieces_num + 2,
        "Not enough lines for piece definitions and FEN"
    );

    let mut pieces = Vec::with_capacity(pieces_num);
    for i in 1..=pieces_num {
        let parts: Vec<&str> = lines[i].split(',').collect();
        assert!(
            parts.len() == 4,
            "Piece definition must have 4 comma-separated values"
        );

        pieces.push(Piece {
            name: parts[0]
                .trim().trim_start_matches("#").to_string(),
            movement: parts[3].trim().to_string(),

            white_char: parts[1]
                .trim().chars().next().expect("Invalid white char"),
            black_char: parts[2]
                .trim().chars().next().expect("Invalid black char"),

            royal: parts[0].starts_with("#"),
        });
    }

    let mut state = State::new(ranks, files, pieces);

    for _ in 0..pieces_num {
        state.pieces_board.push(Board::new(ranks, files));
    }

    let fen_line = lines[pieces_num + 1];
    parse_fen(&mut state, fen_line);

    state
}

fn parse_fen(state: &mut State, fen: &str) {
    let parts: Vec<&str> = fen.split_whitespace().collect();
    assert!(parts.len() >= 4, "FEN must have at least 4 parts");

    let position = parts[0];
    let mut rank = (state.pieces_board[0].ranks - 1) as i32;
    let mut file = 0u8;

    for c in position.chars() {
        match c {
            '/' => {
                rank -= 1;
                file = 0;
            }
            '1'..='9' => {
                file += c.to_digit(10).unwrap() as u8;
            }
            _ => {
                let (piece_idx, is_white) = state.pieces.iter().enumerate()
                    .find_map(|(idx, piece)| {
                        if piece.white_char == c {
                            Some((idx, true))
                        } else if piece.black_char == c {
                            Some((idx, false))
                        } else {
                            None
                        }
                    })
                    .expect(&format!("Unknown piece character: {}", c));

                state.pieces_board[piece_idx].set_bit(file, rank as u8);

                if is_white {
                    state.friends_board.set_bit(file, rank as u8);
                } else {
                    state.enemies_board.set_bit(file, rank as u8);
                }

                if state.pieces[piece_idx].name.to_lowercase() == "king" {
                    state.monarch_board.set_bit(file, rank as u8);
                }

                state.unmoved_board.set_bit(file, rank as u8);

                file += 1;
            }
        }
    }

    state.current_move = match parts[1] {
        "w" => WHITE,
        "b" => BLACK,
        _ => panic!("Invalid active color: {}", parts[1]),
    };

    let castling = parts[2];
    state.castling_state = 0;
    if castling.contains('K') {
        state.castling_state |= WK_CASTLE;
    }
    if castling.contains('Q') {
        state.castling_state |= WQ_CASTLE;
    }
    if castling.contains('k') {
        state.castling_state |= BK_CASTLE;
    }
    if castling.contains('q') {
        state.castling_state |= BQ_CASTLE;
    }

    let en_passant = parts[3];
    state.en_passant_square = if en_passant == "-" {
        None
    } else {
        let file = en_passant[0..2].parse::<u8>()
            .expect("Invalid en passant file");
        let rank = en_passant[2..4].parse::<u8>()
            .expect("Invalid en passant rank");
        Some(
            (rank as u16) * (state.friends_board.files as u16) + (file as u16)
        )
    };

    if parts.len() >= 5 {
        state.halfmove_clock = parts[4].parse().unwrap_or(0);
    }

    state.position_hash = hash_position(state);
}

fn combine_board_strings(
    board1: &str,
    board2: &str,
) -> String {
    let mut result = String::new();

    for ch in board1.chars().zip(board2.chars()) {
        result.push(match ch {
            (c1, c2) if c1 == c2 => c1,
            (c1, c2) if c1.is_whitespace() => c2,
            (c1, _) => c1,
        });
    }

    result
}

fn index_to_square(index: u32, files: u8, ranks: u8) -> String {
    let rank = (index / (files as u32)) as u8;
    let file = (index % (files as u32)) as u8;

    if files <= 26  {
        let file_char = (b'a' + file) as char;
        let rank_char = (b'1' + rank) as char;
        format!("{}{:02}", file_char, rank_char)
    } else {
        format!("{:02}{:02}", file, rank)
    }
}

pub fn format_game_state(state: &State, verbose: bool) -> String {
    let mut result = String::new();

    let null_board = Board::new(
        state.friends_board.ranks,
        state.friends_board.files
    );
    let mut all_boards = Vec::new();

    for (i, piece) in state.pieces.iter().enumerate() {
        let friend_board = &state.pieces_board[i] & &state.friends_board;
        let enemy_board = &state.pieces_board[i] & &state.enemies_board;

        all_boards.push(
            format_board(&friend_board, Some(piece.white_char))
        );

        all_boards.push(
            format_board(&enemy_board, Some(piece.black_char))
        );
    }

    result.push_str(
        &all_boards
            .iter()
            .fold(format_board(&null_board, None), |acc, board_str| {
                combine_board_strings(&acc, board_str)
            })
    );

    if verbose {
        result.push_str(
            &format!("\nPosition hash: {:32X}\n", state.position_hash)
        );
        result.push_str(
            &format!(
                "Current move: {}\n",
                if state.current_move == WHITE { "White" } else { "Black" }
            )
        );

        result.push_str(&format!("Castling rights: {}\n", {
            let mut rights = String::new();
            if state.castling_state & WK_CASTLE != 0 { rights.push('K'); }
            if state.castling_state & WQ_CASTLE != 0 { rights.push('Q'); }
            if state.castling_state & BK_CASTLE != 0 { rights.push('k'); }
            if state.castling_state & BQ_CASTLE != 0 { rights.push('q'); }
            if rights.is_empty() { "-".to_string() } else { rights }
        }));

        result.push_str(&format!("En passant: {}\n",
            state.en_passant_square
                .map_or(
                    "-".to_string(),
                    |sq| format!("{}", index_to_square(
                        sq as u32,
                        state.friends_board.files,
                        state.friends_board.ranks
                    )
                )
            )
        ));

        result.push_str(
            &format!("Halfmove clock: {}\n", state.halfmove_clock)
        );
    }

    result
}