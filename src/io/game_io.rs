//! # game_io.rs
//!
//! Implements game state parsing and formatting functions.
//!
//! This file contains functionality for reading game configuration files,
//! parsing FEN (Forsyth-Edwards Notation) strings, and formatting game state
//! for display. It handles piece definitions, board setup, and state information
//! such as castling rights, en passant squares, and move counters. The module
//! provides both compact and verbose output formats for game visualization.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026

use std::collections::HashMap;
use bnum::types::{U2048};
use std::fs;

use crate::{
    board, constants::*, p_can_promote, p_color, p_index, p_is_big, p_is_major, p_is_minor, p_is_royal, p_value, set
};
use crate::game::{
    hash::hash_position,
    representations::{
        state::State,
        piece::Piece,
    }
};
use crate::io::{
    board_io::{format_board, format_square},
    piece_io::format_piece
};

/// Parses a game configuration file and initializes a game state.
///
/// The configuration file must have the following format:
/// - First line: `ranks,files,piece_types` (comma-separated integers)
/// - Next `piece_types` lines: piece definitions in format
///   `name,white_char,black_char,cheesy_king_notation_movement`
/// - Prefix piece name with `#` to mark it as royal (e.g., `#King`)
/// - Prefix piece name with `^` if a piece can promote (non-big piece)
/// - Prefix piece name with "!" if a piece is major
/// - A non-big piece is minor by default
/// - A royal piece is exclusive since it cannot be captured or promoted
/// - Next line: a number m=[0..piece_type] the number of pieces that can promote
/// - the next `m` lines: promotion mappings in format
///   `from_piece_char->to_piece_chars` (e.g., `P->RNBQ`)
/// - Last line: FEN (Forsyth-Edwards Notation) string for initial position
///
/// # Arguments
///
/// * `path` - Path to the configuration file
///
/// # Returns
///
/// A fully initialized `State` with pieces, boards, and position parsed from
/// the config file.
///
/// # Panics
///
/// Panics if:
/// - The file cannot be read
/// - The file has fewer than 2 lines
/// - The first line doesn't have exactly 3 comma-separated values
/// - Any piece definition doesn't have exactly 4 comma-separated values
/// - The FEN string is invalid
///
/// # Examples
///
/// ```plaintext
/// FIDE Chess
/// 8,8,6
/// ^Pawn,P,p,1000,m!cnW|ip!cnW-.|cnF
/// !Rook,R,r,5000,R
/// Bishop,B,b,3375,B
/// Knight,N,n,3000,N
/// !Queen,Q,q,8375,Q
/// #King,K,k,6375,N|B
/// 2
/// P->RNBQ
/// p->rbnq
/// rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
/// ```
pub fn parse_config_file(path: &str) -> State {
    let contents = fs::read_to_string(path)
        .unwrap_or_else(|_| panic!("Failed to read config file: {}", path));

    let lines: Vec<&str> = contents.lines().collect();
    let mut current_line = 0;
    assert!(lines.len() >= 5, "Config file must have at least 5 lines");

    let title: String = lines[current_line].to_string();
    current_line += 1;

    let line: Vec<&str> = lines[current_line].split(',').collect();
    assert_eq!(
        line.len(), 3,
        "First line must have 3 comma-separated values."
    );
    current_line += 1;

    let files: u8 =
        line[0]
            .trim()
            .parse()
            .unwrap_or_else(
                |_| panic!("Invalid files: {}", line[0].trim())
            );
    let ranks: u8 =
        line[1]
            .trim()
            .parse()
            .unwrap_or_else(
                |_| panic!("Invalid ranks: {}", line[1].trim())
            );
    let pieces_num: u8 =
        line[2]
            .trim()
            .parse()
            .unwrap_or_else(
                |_| panic!("Invalid piece types: {}", line[2].trim())
            );

    assert!(
        lines.len() >= (pieces_num as usize) + 2,
        "Not enough lines for piece definitions and FEN"
    );

    let mut pieces = Vec::with_capacity((pieces_num * 2) as usize);
    let mut piece_char_to_idx = HashMap::new();

    for line in lines.iter().skip(current_line).take(pieces_num as usize) {
        let parts: Vec<&str> = line.split(',').collect();
        assert_eq!(
            parts.len(), 5,
            "Piece definition must have 5 comma-separated values"
        );

        let raw_name = parts[0].trim();
        let is_royal = raw_name.starts_with("#");
        let is_big = !raw_name.starts_with("^");
        let is_major = raw_name.starts_with("!");

        let name = raw_name
            .trim_start_matches("#")
            .trim_start_matches("^")
            .trim_start_matches("!")
            .to_string();
        assert!(
            !name.is_empty() && name.len() <= 12,
            "Invalid piece name length: {} ({})",
            name,
            name.len()
        );

        let white_char = parts[1]
            .trim().chars().next().unwrap_or_else(
                || panic!("Invalid white char: {}", parts[1].trim())
            );
        let black_char = parts[2]
            .trim().chars().next().unwrap_or_else(
                || panic!("Invalid black char: {}", parts[2].trim())
            );
        let movement = parts[4].trim().to_string();

        let value = if parts[3].eq("inf") {
            u16::MAX
        } else {
            parts[3].trim().parse().unwrap_or_else(
                |_| panic!("Invalid piece value: {}", parts[3].trim())
            )
        };

        if white_char != '_' {
            pieces.push(Piece::new(
                name.clone(),
                movement.clone(),
                white_char,
                U2048::ZERO,
                0,                                                              /* placeholde: will be set later      */
                WHITE,
                is_royal,
                is_big,
                is_major,
                value,
                movement.contains("o"),                                         /* can castle kingside                */
                movement.contains("O")                                          /* can castle queenside               */
            ));
        }
        if black_char != '_' {
            pieces.push(Piece::new(
                name.clone(),
                movement.clone(),
                black_char,
                U2048::ZERO,
                0,                                                              /* placeholde: will be set later      */
                BLACK,
                is_royal,
                is_big,
                is_major,
                value,
                movement.contains("o"),                                         /* can castle kingside                */
                movement.contains("O")                                          /* can castle queenside               */
            ));
        }

        current_line += 1;
    }

    let pieces_len = pieces.len() as u8;
    pieces.sort_by_key(|piece| (p_color!(piece) * pieces_len) + p_index!(piece));

    for (i, piece) in pieces.iter_mut().enumerate() {
        piece.encoded_piece |= i as u32;                                        /* set the piece index correctly       */
        piece_char_to_idx.insert(piece.symbol, p_index!(piece));
    }

    let promotion_count: usize = lines[current_line]
        .trim()
        .parse()
        .unwrap_or_else(
            |_| panic!("Invalid promotion count: {}", lines[current_line].trim())
        );
    current_line += 1;

    assert!(
        lines.len() > current_line + promotion_count,
        "Not enough lines for promotion mappings and FEN"
    );

    for _ in 0..promotion_count {
        let parts: Vec<&str> = lines[current_line].split("->").collect();
        assert_eq!(
            parts.len(), 2,
            "Promotion mapping must be in format 'from->to'"
        );

        let from_char = parts[0]
            .trim()
            .chars()
            .next()
            .unwrap_or_else(
                || panic!("Invalid source piece character: {}", parts[0].trim())
            );
        let to_chars: Vec<char> = parts[1].trim().chars().collect();

        let from_idx = *piece_char_to_idx
            .get(&from_char)
            .unwrap_or_else(|| panic!("Unknown source piece: {}", from_char));

        assert!(p_can_promote!(pieces[from_idx as usize]),
            "Piece '{}' cannot promote, but promotion mapping provided",
            from_char
        );

        let mut promotion_bits = U2048::ZERO;
        promotion_bits |= U2048::from(to_chars.len() as u8);

        for (i, &to_char) in to_chars.iter().enumerate() {
            let to_idx = *piece_char_to_idx
                .get(&to_char)
                .unwrap_or_else(|| panic!("Unknown target piece: {}", to_char));

            assert!(p_is_big!(pieces[to_idx as usize]),
                "Piece '{}' cannot be a promotion target",
                to_char
            );

            promotion_bits |= U2048::from(to_idx) << ((i + 1) * 8);
        }

        pieces[from_idx as usize].promotions = promotion_bits;
        current_line += 1;
    }

    let promotion_ranks: Vec<&str> = lines[current_line].split(',').collect();

    assert_eq!(
        promotion_ranks.len(), 2,
        "Promotion ranks line must have 2 comma-separated values"
    );

    current_line += 1;

    let white_promotion_rank: u8 = promotion_ranks[0]
        .trim()
        .parse()
        .unwrap_or_else(
            |_| panic!(
                "Invalid white promotion rank: {}", promotion_ranks[0].trim()
            )
        );
    let black_promotion_rank: u8 = promotion_ranks[1]
        .trim()
        .parse()
        .unwrap_or_else(
            |_| panic!(
                "Invalid black promotion rank: {}", promotion_ranks[1].trim()
            )
        );

    let mut state = State::new(
        title, files, ranks, pieces,
        [white_promotion_rank, black_promotion_rank]
    );

    let fen_line = lines[current_line];
    parse_fen(&mut state, fen_line);

    state
}

/// Parses a FEN string and updates the game state accordingly. Note that the
/// FEN implementation is slightly modified to accommodate arbitrary board
/// sizes, the en passant square is represented as `xxyy` where `xx` is the
/// file and `yy` is the rank (both 0-indexed).
pub fn parse_fen(state: &mut State, fen: &str) {
    let parts: Vec<&str> = fen.split_whitespace().collect();
    assert!(parts.len() >= 4, "FEN must have at least 4 parts");

    let position = parts[0];
    assert!(!position.contains("_"), "_ is not a valid FEN character");

    let ranks_data: Vec<&str> = position.split('/').collect();
    assert!(
        ranks_data.len() == state.ranks as usize,
        "FEN rank count ({}) doesn't match board ranks ({})",
        ranks_data.len(),
        state.ranks
    );                                                                          /* assert number of ranks in the FEN  */

    for (rank_idx, rank_data) in ranks_data.iter().enumerate() {                /* assert number of files in each rank*/
        let mut file_count = 0u8;
        let mut chars = rank_data.chars().peekable();
        while let Some(c) = chars.next() {
            if c.is_ascii_digit() {
                let mut num_str = c.to_string();
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_ascii_digit() {
                        num_str.push(next_c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                file_count += num_str.parse::<u8>().unwrap();
            } else {
                file_count += 1;
            }
        }
        assert!(
            file_count == state.files,
            "FEN rank {} has {} files but expected {}",
            rank_idx,
            file_count,
            state.files
        );
    }

    let mut rank = state.ranks - 1;
    let mut file = 0u8;

    let mut position_chars = position.chars().peekable();
    while let Some(c) = position_chars.next() {
        match c {
            '/' => {
                rank -= 1;
                file = 0;
            }
            '0'..='9' => {
                let mut num_str = c.to_string();
                while let Some(&next_c) = position_chars.peek() {
                    if next_c.is_ascii_digit() {
                        num_str.push(next_c);
                        position_chars.next();
                    } else {
                        break;
                    }
                }
                file += num_str.parse::<u8>().unwrap();
            }
            _ => {
                let piece_idx = state.pieces.iter().position(
                    |piece| piece.symbol == c
                ).unwrap_or_else(|| panic!("Unknown piece character: {}", c));

                let piece = &state.pieces[piece_idx];
                let square_index =
                    (rank as u32) * (state.files as u32) + (file as u32);

                state.main_board[square_index as usize] = p_index!(piece);

                state.piece_list[p_index!(piece) as usize]
                    .push(square_index as u16);
                state.piece_count[p_index!(piece) as usize] += 1;

                set!(
                    state.pieces_board[p_color!(piece) as usize], square_index
                );
                state.material[p_color!(piece) as usize] +=
                    p_value!(piece) as u32;

                if p_is_royal!(piece) {
                    state.royal_list[p_color!(piece) as usize].push(
                        square_index as u16
                    );
                    state.royal_pieces[p_color!(piece) as usize] += 1;
                }

                if p_is_major!(piece) {
                    state.major_pieces[p_color!(piece) as usize] += 1;
                }

                if p_is_minor!(piece) {
                    state.minor_pieces[p_color!(piece) as usize] += 1;
                }

                if p_is_big!(piece) {
                    state.big_pieces[p_color!(piece) as usize] += 1;
                }

                if (c == 'P' && rank == 1)
                    || (c == 'p' && rank == (state.ranks - 2))
                    || (c == 'R' && rank == 0 && (file == 0 || file == state.files - 1))
                    || (c == 'r' && rank == state.ranks - 1 && (file == 0 || file == state.files - 1))
                    || (c == 'K' && rank == 0 && file == 4)
                    || (c == 'k' && rank == state.ranks - 1 && file == 4)
                {
                    set!(state.virgin_board, square_index);
                }

                file += 1;
            }
        }
    }

    state.playing = match parts[1] {
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
        u32::MAX
    } else {                                                                    /* enpassant square is a bit different*/
        let file = en_passant[0..2].parse::<u8>()
            .unwrap_or_else(
                |_| panic!(
                    "Invalid en passant file: {}", en_passant[0..2].trim()      /* xxyyppqq -> [filefilerankrank; 2]  */
                )
            );                                                                  /* x and y are capturing square       */
        let rank = en_passant[2..4].parse::<u8>()
            .unwrap_or_else(
                |_| panic!(
                    "Invalid en passant rank: {}", en_passant[2..4].trim()
                )                                                               /* p and q are captured piece square  */
            );

        let piece_file = en_passant[4..6].parse::<u8>()
            .unwrap_or_else(
                |_| panic!(
                    "Invalid en passant file: {}", en_passant[4..6].trim()      /* xxyyppqq -> [filefilerankrank; 2]  */
                )
            );                                                                  /* x and y are capturing square       */
        let piece_rank = en_passant[6..8].parse::<u8>()
            .unwrap_or_else(
                |_| panic!(
                    "Invalid en passant rank: {}", en_passant[6..8].trim()      /* p and q are captured piece square  */
                )
            );
        let piece_index = u32::from_str_radix(&en_passant[8..10], 16)
            .unwrap_or_else(                                                    /* last 2 digit hex num is piece indx */
                |_| panic!(
                    "Invalid en passant rank: {}", en_passant[8..10].trim()
                )
            );

        let square_index = (rank as u32) * (state.files as u32) + (file as u32);
        let piece_square_index =
            (piece_rank as u32) * (state.files as u32) + (piece_file as u32);

        square_index | (piece_square_index << 12) | piece_index << 24
    };

    if parts.len() >= 5 {
        state.halfmove_clock = parts[4].parse()
            .unwrap_or_else(
                |_| panic!("Invalid halfmove clock: {}", parts[4].trim())
            );
    }

    state.position_hash = hash_position(state);
}

/// Combines two board string representations by overlaying non-whitespace
/// characters from the first board onto the second board.
///
/// This function is used to merge multiple piece-specific board visualizations
/// into a single composite board display. It performs a character-by-character
/// merge where:
/// - If both characters are identical, use that character
/// - If the first board has whitespace, use the character from the second board
/// - Otherwise, use the character from the first board
///
/// # Arguments
///
/// * `board1` - The first board string
/// * `board2` - The second board string
///
/// # Returns
///
/// A combined board string where pieces from both boards are merged together
/// while preserving the ASCII art borders and structure.
///
/// # Examples
///
/// **Before** - Two separate board strings (white pieces and black pieces):
///
/// ```text
/// Board 1 (White pieces):        Board 2 (Black pieces):
///    ╔═══╤═══╤═══╤═══╗              ╔═══╤═══╤═══╤═══╗
/// 08 ║   │   │   │   ║           08 ║ r │ n │ b │ k ║
///    ╟───┼───┼───┼───╢              ╟───┼───┼───┼───╢
/// 07 ║   │   │   │   ║           07 ║ p │ p │ p │ p ║
///    ╟───┼───┼───┼───╢              ╟───┼───┼───┼───╢
/// 06 ║   │   │   │   ║           06 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢              ╟───┼───┼───┼───╢
/// 05 ║   │   │   │   ║           05 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢              ╟───┼───┼───┼───╢
/// 04 ║   │   │   │   ║           04 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢              ╟───┼───┼───┼───╢
/// 03 ║   │   │   │   ║           03 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢              ╟───┼───┼───┼───╢
/// 02 ║ P │ P │ P │ P ║           02 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢              ╟───┼───┼───┼───╢
/// 01 ║ R │ N │ B │ K ║           01 ║   │   │   │   ║
///    ╚═══╧═══╧═══╧═══╝              ╚═══╧═══╧═══╧═══╝
///      a   b   c   d                  a   b   c   d
/// ```
///
/// **After** - Combined result:
///
/// ```text
///    ╔═══╤═══╤═══╤═══╗
/// 08 ║ r │ n │ b │ k ║
///    ╟───┼───┼───┼───╢
/// 07 ║ p │ p │ p │ p ║
///    ╟───┼───┼───┼───╢
/// 06 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢
/// 05 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢
/// 04 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢
/// 03 ║   │   │   │   ║
///    ╟───┼───┼───┼───╢
/// 02 ║ P │ P │ P │ P ║
///    ╟───┼───┼───┼───╢
/// 01 ║ R │ N │ B │ K ║
///    ╚═══╧═══╧═══╧═══╝
///      a   b   c   d
/// ```
pub fn combine_board_strings(
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

pub fn format_game_state(state: &State, verbose: bool) -> String {
    let mut result = String::new();

    let board_size =
        (state.files as usize) * (state.ranks as usize);
    let piece_count = state.pieces.len();

    let mut all_boards = vec![board!(state.files, state.ranks); piece_count];

    for square in 0..board_size {
        let piece_idx = state.main_board[square];
        if piece_idx != NO_PIECE {
            set!(all_boards[piece_idx as usize], square as u32);
        }
    }

    result.push_str(
        &all_boards.iter().enumerate().map(
            |(i, b)| format_board(b, Some(state.pieces[i].symbol))
        ).reduce(|board_str, next_board| {
            combine_board_strings(&board_str, &next_board)
        }).expect(
            "Failed to format combined board string"
        )
    );


    if verbose {
        result.push_str(
            &format!("\nPosition hash\t: {:32X}\n", state.position_hash)
        );
        result.push_str(
            &format!(
                "Current move\t: {}\n",
                if state.playing == WHITE { "White" } else { "Black" }
            )
        );

        result.push_str(&format!("Castling rights\t: {}\n", {
            let mut rights = String::new();
            if state.castling_state & WK_CASTLE != 0 { rights.push('K'); }
            if state.castling_state & WQ_CASTLE != 0 { rights.push('Q'); }
            if state.castling_state & BK_CASTLE != 0 { rights.push('k'); }
            if state.castling_state & BQ_CASTLE != 0 { rights.push('q'); }
            if rights.is_empty() { "-".to_string() } else { rights }
        }));

        result.push_str(&format!("En passant\t: {}\n",
            if state.en_passant_square == u32::MAX {
                "-".to_string()
            } else {
                format_square(
                    (state.en_passant_square & 0xFFF) as u16, state
                )
            }
        ));

        result.push_str(
            &format!("Halfmove clock\t: {}\n", state.halfmove_clock)
        );
    }

    result
}

/// Formats all piece types in the game state as tables.
///
/// Creates tables with box-drawing characters, each table containing up to 6
/// pieces.
/// If there are more than 6 piece types, additional tables are created.
///
/// Each column represents one piece with the following rows:
/// Name, Index, Symbol, Color, Value, Royal, Can Promote, Major, Promotes From
///
/// # Arguments
///
/// * `state` - The game state containing all piece definitions
///
/// # Returns
///
/// A formatted string containing one or more piece type tables.
pub fn format_piece_types(state: &State) -> String {
    let headers = vec![
        "Name",
        "Index",
        "Symbol",
        "Color",
        "Value",
        "Royal",
        "Promote",
        "Major",
        "Promo From"
    ];

    let mut result = String::new();
    let pieces_per_table = 6;
    let num_tables = state.pieces.len().div_ceil(pieces_per_table);

    const HEADER_WIDTH: usize = 10;
    const PIECE_WIDTH: usize = 12;

    for table_idx in 0..num_tables {
        let start_idx = table_idx * pieces_per_table;
        let end_idx = (
            (table_idx + 1) * pieces_per_table).min(state.pieces.len()
        );
        let pieces_in_table = end_idx - start_idx;

        let mut piece_columns: Vec<Vec<String>> = Vec::new();

        for piece_idx in start_idx..end_idx {
            let formatted = format_piece(&state.pieces[piece_idx]);
            let lines: Vec<&str> = formatted.lines().collect();

            let mut piece_data = Vec::new();
            for line in lines.iter().skip(1).take(9) {
                let content = line
                    .trim_start_matches("│ ").trim_end_matches(" │");
                piece_data.push(content.to_string());
            }

            let mut promo_from_symbols = Vec::new();
            for other_piece in state.pieces.iter() {
                let promotion_count = (
                    other_piece.promotions & bnum::types::U2048::from(0xFFu8)
                )
                    .to_string()
                    .parse::<usize>()
                    .expect(
                        "Failed to parse promotion count"
                    );

                for i in 0..promotion_count {
                    let target_idx = (
                        (other_piece.promotions >> (
                            (i + 1) * 8)
                        ) & bnum::types::U2048::from(0xFFu8)
                    )
                        .to_string()
                        .parse::<u8>()
                        .expect(
                            "Failed to parse promotion target index"
                        );

                    if target_idx == piece_idx as u8 {
                        promo_from_symbols.push(other_piece.symbol);
                        break;
                    }
                }
            }

            piece_data[8] = if promo_from_symbols.is_empty() {
                "-".to_string()
            } else {
                promo_from_symbols.iter().collect::<String>()
            };

            piece_columns.push(piece_data);
        }

        result.push('╔');
        result.push_str(&"═".repeat(HEADER_WIDTH + 2));
        result.push('╤');
        for i in 0..pieces_in_table {
            result.push_str(&"═".repeat(PIECE_WIDTH + 2));
            if i < pieces_in_table - 1 {
                result.push('╤');
            }
        }
        result.push_str("╗\n");

        for row_idx in 0..9 {
            result.push('║');

            result.push_str(&format!(" {:<HEADER_WIDTH$} ", headers[row_idx]));
            result.push('│');

            for (col_idx, piece_col) in piece_columns.iter().enumerate() {
                result.push_str(
                    &format!(" {:^PIECE_WIDTH$} ", piece_col[row_idx])
                );
                if col_idx < piece_columns.len() - 1 {
                    result.push('│');
                }
            }

            result.push_str("║\n");

            if row_idx < 8 {
                result.push('╟');
                result.push_str(&"─".repeat(HEADER_WIDTH + 2));
                result.push('┼');
                for i in 0..pieces_in_table {
                    result.push_str(&"─".repeat(PIECE_WIDTH + 2));
                    if i < pieces_in_table - 1 {
                        result.push('┼');
                    }
                }
                result.push_str("╢\n");
            }
        }

        result.push('╚');
        result.push_str(&"═".repeat(HEADER_WIDTH + 2));
        result.push('╧');
        for i in 0..pieces_in_table {
            result.push_str(&"═".repeat(PIECE_WIDTH + 2));
            if i < pieces_in_table - 1 {
                result.push('╧');
            }
        }
        result.push_str("╝\n");

        if table_idx < num_tables - 1 {
            result.push('\n');
        }
    }

    result
}

pub fn format_entire_game(state: &State) -> String {
    let mut result = String::new();

    result.push_str(
        &format!("{} ({}x{})\n", state.title, state.files, state.ranks)
    );
    result.push_str("\n---------- Piece Types\n");
    result.push_str(&format_piece_types(state));
    result.push_str("\n---------- Initial Position\n");
    result.push_str(&format_game_state(state, true));

    result
}