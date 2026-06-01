//! # board_io.rs
//!
//! Implements board formatting and visualization functions.
//!
//! This file contains functionality for converting bitboard representations
//! into human-readable ASCII art displays. It provides methods for formatting
//! individual bitboards and complete boards with Unicode
//! box-drawing characters,
//!
//! supporting various board sizes and piece symbols. The formatting includes
//! rank and file labels for easy reference.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026
use crate::*;

/// Formats the square index into an algebraic square format.
///
/// {file letter/number}{rank number}
///
/// If the board width is more than 26 then it switches to numeric file. Both
/// file and rank in both formats are 1-indexed
pub fn format_square(index: u16, state: &State) -> String {
    let file = (index % state.statics.files as u16) as u8;
    let rank = (index / state.statics.files as u16) as u8;

    if state.statics.files <= 26 {
        format!("{}{}", (b'a' + file) as char, rank + 1).trim().to_string()
    } else {
        format!("{:02}{:02}", file + 1, rank + 1).trim().to_string()
    }
}

pub fn parse_square(square_str: &str, state: &State) -> Option<u16> {
    let files = state.statics.files as u16;
    let ranks = state.statics.ranks as u16;

    if square_str.len() < 2 {
        return None;
    }

    if state.statics.files <= 26 {

        let mut file = square_str[0..1].chars().next()? as u16;
        let mut rank = square_str[1..].parse::<u16>().ok()?;

        file = file.wrapping_sub('a' as u16);
        rank -= 1;

        if file < files && rank < ranks {
            return Some(rank * files + file);
        }

    } else {
        let Ok(mut file) = square_str[..2].parse::<u16>() else {
            return None;
        };

        let Ok(mut rank) = square_str[2..].parse::<u16>() else {
            return None;
        };

        file -= 1;
        rank -= 1;

        if file < files && rank < ranks {
            return Some(rank * files + file);
        }
    }

    None
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
        bitboard_str = bitboard_str.replace('1', &piece.to_string());
    }

    let mut result = String::new();
    result.push_str(&format!(
        "   ╔{}╗\n",
        "═══╤".repeat(files as usize - 1) + "═══"
    ));

    for (i, line) in bitboard_str.lines().enumerate() {
        result.push_str(&format!(
            "{:>2} ║ {} ║\n",
            ranks as usize - i,
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
            ((b'a' + col) as char).to_string()
        } else {
            format!("{:02}", col)
        };
        if col < files - 1 {
            result.push_str(&format!("{:3} ", file_label));
        } else {
            result.push_str(&format!("{:3}", file_label));
        }
    }
    result.push('\n');
    result = result.replace(" 0 ", "   ");

    result
}

pub fn format_numeric_board(values: &[i32], files: u8, ranks: u8) -> String {
    let mut result = String::new();
    let width = 4;

    result.push_str(&format!(
        "   ╔{}╗\n",
        (0..files)
            .map(|_| "═".repeat(width + 2))
            .collect::<Vec<String>>()
            .join("╤")
    ));

    for rank in (0..ranks).rev() {
        result.push_str(&format!("{:>2} ║", rank + 1));
        for file in 0..files {
            let idx = rank as usize * files as usize + file as usize;
            result.push_str(
                &format!(" {:>width$} ", values[idx], width = width)
            );
            if file + 1 < files {
                result.push('│');
            }
        }
        result.push_str("║\n");

        if rank > 0 {
            result.push_str(&format!(
                "   ╟{}╢\n",
                (0..files)
                    .map(|_| "─".repeat(width + 2))
                    .collect::<Vec<String>>()
                    .join("┼")
            ));
        }
    }

    result.push_str(&format!(
        "   ╚{}╝\n",
        (0..files)
            .map(|_| "═".repeat(width + 2))
            .collect::<Vec<String>>()
            .join("╧")
    ));

    result.push_str("     ");
    for file in 0..files {
        if files <= 26 {
            result.push_str(&format!(
                " {:^width$} ",
                (b'a' + file) as char,
                width = width
            ));
        } else {
            result.push_str(&format!(" {:^width$} ", file, width = width));
        }
        if file + 1 < files {
            result.push(' ');
        }
    }

    result.push('\n');
    result
}

pub fn mirror_pst_across_horizontal_axis(
    pst: &[i32],
    files: usize,
    ranks: usize,
) -> Vec<i32> {
    assert!(
        pst.len() == files * ranks,
        "PST length ({}) doesn't match board size ({})",
        pst.len(),
        files * ranks
    );

    let mut mirrored = vec![0i32; pst.len()];

    for rank in 0..ranks {
        let src_rank = ranks - 1 - rank;
        let dst_start = rank * files;
        let src_start = src_rank * files;

        mirrored[dst_start..dst_start + files]
            .copy_from_slice(&pst[src_start..src_start + files]);
    }

    mirrored
}

pub fn determine_board_dimensions(fen: &str) -> (u8, u8) {
    let ranks_data: Vec<&str> = fen.split('/').collect();
    let rank_count = ranks_data.len() as u8;
    let mut file_count = 0u8;
    let mut chars = ranks_data[0].chars().peekable();

    while let Some(c) = chars.next() {
        if c.is_ascii_digit() {
            let mut run = c.to_digit(10).unwrap() as u16;
            while let Some(next) = chars.peek() {
                if next.is_ascii_digit() {
                    run =
                        run * 10 +
                        chars.next().unwrap().to_digit(10).unwrap() as u16;
                } else {
                    break;
                }
            }
            file_count += run as u8;
        } else {
            file_count += 1;
        }
    }
    (file_count, rank_count)
}
