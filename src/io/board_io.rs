//! board_io.rs
//!
//! Formats boards and square names.
//!
//! This file writes boards and value tables as labelled diagrams. It also
//! converts between square names and flat square indices, the same way at
//! all board widths.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                               SQUARE COORDINATES
\*----------------------------------------------------------------------------*/

/// format_square
///
/// Writes a flat square index as a square name. The board width selects
/// the form, so one variant always uses the same form.
///
/// - 26 files or fewer : file letter and rank, `e4`
/// - more than 26      : file and rank as two digits each, `0504`
///
/// The two parts start at 1. The flat index starts at 0.
///
/// Params:
/// - index: u16    -> flat square index
/// - state: &State -> position with the board width
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
/// Reads a square name into a flat index. This is the inverse of
/// `format_square`, and the board width selects the form the same way.
///
/// - `e4`   : file `'e' - 'a'`, rank `4 - 1`, index `rank * files + file`
/// - `0504` : the same square, the two parts minus 1
///
/// Params:
/// - square_str: &str   -> square name, e.g. "e4" or "0504"
/// - state     : &State -> position with the board dimensions
///
/// Return:
/// Option<u16>          -> the flat square index, or None if not valid
///
/// Notes:
/// A file letter below `'a'` wraps to a large number. Thus one bound test
/// rejects the two ends.
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

/*----------------------------------------------------------------------------*\
                                 BOARD DIAGRAMS
\*----------------------------------------------------------------------------*/

/// format_board
///
/// Writes one bitboard as a grid, with rank numbers on the left and file
/// names below. A set bit shows as `1`, or as `piece_char` if given. A
/// clear bit shows as a blank.
///
/// ```text
///    ╔═══╤═══╤═══╗
///  3 ║ 1 │   │   ║
///    ╟───┼───┼───╢
///  2 ║   │ 1 │   ║
///    ╟───┼───┼───╢
///  1 ║   │   │ 1 ║
///    ╚═══╧═══╧═══╝
///      a   b   c
/// ```
///
/// The first rank is at the bottom. A board with more than 26 files has
/// two-digit file numbers, not letters.
///
/// Params:
/// - board     : &Board       -> bitboard to show
/// - piece_char: Option<char> -> optional character for set bits
///
/// Return:
/// String                     -> the labelled board diagram
///
pub fn format_board(board: &Board, piece_char: Option<char>) -> String {
    let ranks = ranks!(board);
    let files = files!(board);
    let mut bitboard_str = String::new();

    for row in (0..ranks).rev() {
        for col in 0..files {
            let index: u32 = row as u32 * files as u32 + col as u32;
            bitboard_str.push_str(
                ["0  ", "1  "][board.2.bit(index) as usize]
            );
        }
        bitboard_str.push('\n');
    }

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
        let file_label = if files <= 26 {
            ((b'a' + col) as char).to_string()
        } else {
            format!("{:02}", col + 1)
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
/// Writes one integer for each square as a grid. Use it for derived tables
/// such as piece-square tables, shelter rings and danger weights.
///
/// ```text
///    ╔══════╤══════╤══════╗
///  2 ║   12 │   -4 │    0 ║
///    ╟──────┼──────┼──────╢
///  1 ║   -8 │    3 │    7 ║
///    ╚══════╧══════╧══════╝
///        a      b      c
/// ```
///
/// The numbers are right-aligned in four columns. Zero shows as `0`, not
/// as a blank.
///
/// Params:
/// - values: &[i32] -> values for each square, in board order
/// - files : u8     -> board width
/// - ranks : u8     -> board height
///
/// Return:
/// String           -> the labelled value grid
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
            result.push_str(&format!(
                " {:^width$} ",
                format!("{:02}", file + 1),
                width = width
            ));
        }
        if file + 1 < files {
            result.push(' ');
        }
    }

    result.push('\n');
    result
}

/*----------------------------------------------------------------------------*\
                            TABLE AND BOARD GEOMETRY
\*----------------------------------------------------------------------------*/

/// mirror_pst_across_horizontal_axis
///
/// Reverses the rank order of a piece-square table, so the table of one
/// colour is correct for the other colour. Each square keeps its file.
///
/// ```text
/// before   rank 3   rank 2   rank 1   rank 0
/// after    rank 0   rank 1   rank 2   rank 3
/// ```
///
/// Params:
/// - pst  : &[i32] -> source table in board order
/// - files: usize  -> board width
/// - ranks: usize  -> board height
///
/// Return:
/// Vec<i32>        -> the mirrored table
///
/// Notes:
/// The function asserts that the length is `files * ranks`. Without this
/// test, a wrong size would give a wrong table without an error.
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
/// Finds the board size from the FEN piece placement only. Thus the
/// function works before the variant is known.
///
/// ```text
/// "4k3/8/8/4K3"   four segments, so four ranks
///  ^^^            4 + 1 + 3 in the first segment, so eight files
/// ```
///
/// Params:
/// - fen: &str -> the piece placement part of a FEN
///
/// Return:
/// (u8, u8)    -> (files, ranks)
///
/// Notes:
/// The function reads a digit sequence as one number. Thus `10` is ten
/// empty squares, not one and zero.
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
