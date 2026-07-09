//! # board_io.rs
//!
//! Implements board formatting and visualization functions.
//!
//! Boards and per-square tables are far easier to reason about when a human
//! can see them, and coordinates must round-trip the same way at every board
//! width. This file is the engine's visual and coordinate layer: it renders
//! boards and value grids as labelled diagrams, and keeps algebraic and
//! numeric square names in agreement with their flat indices.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026
use crate::*;

/// format_square
///
/// Formats the square index into an algebraic square name of the form
/// {file letter/number}{rank number}. If the board width is more than 26
/// it switches to a numeric file. Both file and rank in both formats are
/// 1-indexed.
///
/// Params:
/// - index: u16    -> flat square index to format
/// - state: &State -> supplies the board width
///
/// Return:
/// String          -> algebraic ("e4") or numeric ("0504") square name
///
pub fn format_square(index: u16, state: &State) -> String {
    let file = (index % state.statics.files as u16) as u8;
    let rank = (index / state.statics.files as u16) as u8;

    if state.statics.files <= 26 {
        format!("{}{}", (b'a' + file) as char, rank + 1).trim().to_string()
    } else {
        format!("{:02}{:02}", file + 1, rank + 1).trim().to_string()
    }
}

/// parse_square
///
/// Inverse of `format_square`: reads an algebraic or numeric square name
/// (chosen by board width, matching the formatter) back into a flat
/// index, rejecting coordinates that fall off the board.
///
/// Params:
/// - square_str: &str   -> square name, e.g. "e4" or "0504"
/// - state     : &State -> supplies the board dimensions
///
/// Return:
/// Option<u16>          -> the flat square index, or None if invalid
///
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

/// format_bitboard
///
/// Renders the raw bits of a bitboard as a plain 0/1 grid, top rank
/// first — the unstyled core that `format_board` decorates.
///
/// Params:
/// - board: &U4096 -> raw bit storage to render
/// - files: u8     -> board width
/// - ranks: u8     -> board height
///
/// Return:
/// String          -> newline-separated 0/1 grid
///
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

/// format_board
///
/// Pretty-prints one bitboard as a box-drawn grid with rank numbers and
/// file letters. Set bits render as `1`, or as `piece_char` when given;
/// clear bits render as blanks.
///
/// Params:
/// - board     : &Board       -> bitboard to display
/// - piece_char: Option<char> -> optional glyph for set bits
///
/// Return:
/// String                     -> the framed, labelled board diagram
///
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

/// format_numeric_board
///
/// Pretty-prints one integer per square as a box-drawn grid — used to
/// inspect piece-square tables and other derived per-square values.
///
/// Params:
/// - values: &[i32] -> per-square values in board order
/// - files : u8     -> board width
/// - ranks : u8     -> board height
///
/// Return:
/// String           -> the framed, labelled value grid
///
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

/// mirror_pst_across_horizontal_axis
///
/// Flips a piece-square table rank-wise so a table derived for one color
/// can serve its twin: rank 0 swaps with the top rank, files unchanged.
///
/// Params:
/// - pst  : &[i32] -> source table in board order
/// - files: usize  -> board width
/// - ranks: usize  -> board height
///
/// Return:
/// Vec<i32>        -> the mirrored table
///
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

/// determine_board_dimensions
///
/// Infers a board's size from a FEN placement field alone: the rank
/// count is the number of `/`-separated segments and the file count is
/// summed from the first segment's letters and digit runs.
///
/// Params:
/// - fen: &str -> the piece placement portion of a FEN
///
/// Return:
/// (u8, u8)    -> (files, ranks)
///
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
