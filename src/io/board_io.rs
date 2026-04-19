//! # board_io.rs
//!
//! Implements board formatting and visualization functions.
//!
//! This file contains functionality for converting bitboard representations
//! into human-readable ASCII art displays. It provides methods for formatting
//! individual bitboards and complete boards with Unicode
//! box-drawing characters,
//! supporting various board sizes and piece symbols. The formatting includes
//! rank and file labels for easy reference.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026
use crate::*;

pub fn format_square(index: u16, game_state: &State) -> String {
    let file = (index % game_state.files as u16) as u8;
    let rank = (index / game_state.files as u16) as u8;

    if game_state.files <= 26 {
        let file_char = (b'a' + file) as char;
        format!("{}{}", file_char, rank).trim().to_string()
    } else {
        format!("{:02}{:02}", file, rank).trim().to_string()
    }
}

fn format_bitboard(board: &U4096, files: u8, ranks: u8) -> String {
    let mut result = String::new();

    for row in (0..ranks).rev() {
        for col in 0..files {
            let index: u32 = row as u32 * files as u32 + col as u32;
            if board.bit(index) {
                result.push_str("1  ");
            } else {
                result.push_str("0  ");
            }
        }
        result.push('\n');
    }
    result
}

pub fn format_board(board: &Board, piece_char: Option<char>) -> String {
    let ranks = ranks!(board);
    let files = files!(board);
    let mut bitboard_str = format_bitboard(&board.2, files, ranks);

    if let Some(piece) = piece_char {
        bitboard_str = bitboard_str.replace("1", piece.to_string().as_str());
    }

    let mut result = String::new();
    result.push_str(&format!(
        "   ╔{}╗\n",
        "═══╤".repeat(files as usize - 1) + "═══"
    ));

    for (i, line) in bitboard_str.lines().enumerate() {
        result.push_str(&format!(
            "{:02} ║ {} ║\n",
            ranks as usize - i - 1,
            line.trim().replace("  ", " │ ")
        ));

        result.push_str(
            &(if i == (ranks as usize - 1) {
                "".to_string()
            } else {
                format!("   ╟{}╢\n", "───┼".repeat(files as usize - 1) + "───")
            }),
        );
    }

    result.push_str(&format!(
        "   ╚{}╝\n     ",
        "═══╧".repeat(files as usize - 1) + "═══"
    ));

    for col in 0..files {
        let file_label = if files < 26 {
            ((b'A' + col) as char).to_string()
        } else {
            format!("{:02}", col)
        };
        result.push_str(&format!("{:3} ", file_label));
    }
    result.push('\n');
    result = result.replace(" 0 ", "   ");

    result
}

/// Prints a board mask of destination squares from precomputed move vectors.
///
/// This is a debugging helper and does not check occupancy or legality;
/// it only visualizes geometric reach from cached relevant moves.
pub fn debug_print_relevant_moves(state: &State, piece_index: u8, square: u16) {
    let moves = &state.relevant_moves[piece_index as usize][square as usize];

    let piece_color = p_color!(state.pieces[piece_index as usize]);

    let mut result = board!(state.files, state.ranks);

    for multi_leg_vector in moves {
        let mut file = (square % state.files as u16) as i32;
        let mut rank = (square / state.files as u16) as i32;
        for leg in multi_leg_vector {
            let x = x!(leg);
            let y = y!(leg);

            file += x as i32 * (-2 * piece_color as i32 + 1);
            rank += y as i32 * (-2 * piece_color as i32 + 1);
            let accumulated_index = rank * (state.files as i32) + file;

            set!(result, accumulated_index as u32);
        }
    }

    debug!("{}", format_board(&result, Some('*')));
}
