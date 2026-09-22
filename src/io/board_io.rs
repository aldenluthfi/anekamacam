//! board_io.rs
//!
//! Implements board formatting and visualization functions.
//!
//! Boards and per-square tables are far easier to reason about when a human
//! can see them, and coordinates must round-trip the same way at every board
//! width. This file is the engine's visual and coordinate layer: it renders
//! boards and value grids as labelled diagrams, and keeps algebraic and
//! numeric square names in agreement with their flat indices.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                               SQUARE COORDINATES
\*----------------------------------------------------------------------------*/

/// format_square
///
/// Formats a flat square index as a board coordinate. Which of the two forms
/// is used is decided by the board and never by the caller, so one variant
/// always spells a square one way.
///
/// - 26 files or fewer : a lettered file and a rank, `e4`
/// - anything wider    : both halves as two digits, `0504` for that
///                       same square
///
/// Both displayed components are one-indexed, the flat index they come from
/// is not, and the letters run out at 26 — which is where the numeric form
/// starts rather than where a wider board stops being addressable.
///
/// Params:
/// - index: u16    -> flat square index to format
/// - state: &State -> supplies the board width
///
/// Return:
/// String          -> algebraic ("e4") or numeric ("0504") square name
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
/// back into a flat index. The form is chosen by board width exactly as the
/// formatter chooses it, so a name this engine printed is a name it reads.
///
/// - `e4`   : `'e'` minus `'a'` as the file, 4 minus 1 as the rank,
///            read back as `rank × files + file`
/// - `0504` : that same square, both halves parsed and both decremented
///
/// A file letter below `'a'` wraps to a huge number instead of going
/// negative, which the width comparison then rejects, so the bound check
/// covers both ends of the board with one test.
///
/// Params:
/// - square_str: &str   -> square name, e.g. "e4" or "0504"
/// - state     : &State -> supplies the board dimensions
///
/// Return:
/// Option<u16>          -> the flat square index, or None if invalid
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
/// Pretty-prints one bitboard as a box-drawn grid with rank numbers along the
/// left and file names underneath. Set bits render as `1`, or as `piece_char`
/// when one is given, and clear bits render as blanks.
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
/// Rank 0 prints last so the diagram reads the way a board is set up rather
/// than the way its bits are laid out, and boards past 26 files label their
/// files with two-digit numbers instead of letters.
///
/// Params:
/// - board     : &Board       -> bitboard to display
/// - piece_char: Option<char> -> optional glyph for set bits
///
/// Return:
/// String                     -> the framed, labelled board diagram
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
        let file_label = if files < 26 {
            ((b'a' + col) as char).to_string()
        } else {
            format!("{:>2}", col)
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
/// Pretty-prints one integer per square as a box-drawn grid, the way a whole
/// derived table is read at once: piece-square tables, shelter rings, danger
/// weights, anything the deriver produces a number per square for.
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
/// Cells are four columns wide and right-aligned, so a negative number and
/// the number above it still line up under each other. Zero prints as `0`
/// here rather than as a blank: a table's zeros are values, not emptiness.
///
/// Params:
/// - values: &[i32] -> per-square values in board order
/// - files : u8     -> board width
/// - ranks : u8     -> board height
///
/// Return:
/// String           -> the framed, labelled value grid
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

/*----------------------------------------------------------------------------*\
                            TABLE AND BOARD GEOMETRY
\*----------------------------------------------------------------------------*/

/// mirror_pst_across_horizontal_axis
///
/// Flips a piece-square table rank-wise so a table derived for one colour can
/// serve its twin. Only ranks move: a square keeps its file, which is what
/// makes the flip the right one for a board both sides look at from opposite
/// ends.
///
/// ```text
/// before   rank 3   rank 2   rank 1   rank 0
/// after    rank 0   rank 1   rank 2   rank 3
/// ```
///
/// Whole rows are copied rather than squares, and the length assertion is the
/// only thing standing between a mismatched table and a silently rotated one,
/// since any `files × ranks` split of the same slice looks equally plausible.
///
/// Params:
/// - pst  : &[i32] -> source table in board order
/// - files: usize  -> board width
/// - ranks: usize  -> board height
///
/// Return:
/// Vec<i32>        -> the mirrored table
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
/// Infers a board's size from a FEN placement field alone, so a position can
/// be read before anything knows what variant it belongs to.
///
/// ```text
/// "4k3/8/8/4K3"   four segments, so four ranks
///  ^^^            4 + 1 + 3 across the first, so eight files
/// ```
///
/// Digits are consumed as whole runs rather than one at a time, since a board
/// wider than nine files spells an empty row as `10`, and reading that as a
/// one and a zero would lose a file every time.
///
/// Params:
/// - fen: &str -> the piece placement portion of a FEN
///
/// Return:
/// (u8, u8)    -> (files, ranks)
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
