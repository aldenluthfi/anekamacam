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

use bnum::types::{U2048};
use toml::Table;
use std::collections::HashMap;
use std::fs;

use crate::game::representations::board::Board;
use crate::{
    board, constants::*, enc_castling, enc_count_limit, enc_drops, enc_en_passant, enc_forbidden_zones, enc_promotions, get, p_color, p_index, p_is_big, p_is_major, p_is_minor, p_is_royal, p_value, set
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
pub fn parse_config_file(path: &str) -> State {
    let file_str = fs::read_to_string(path)
        .expect("Failed to read configuration file");
    let config = file_str.parse::<Table>()
        .expect("Failed to parse configuration file as TOML");

    let mut piece_char_to_idx = HashMap::new();
    let mut pieces: Vec<Piece> = Vec::new();
    let pieces_table = config["pieces"].as_array().expect(
        "Pieces section not found in configuration file"
    );

    for piece_table in pieces_table.iter() {
        let piece_data = piece_table.as_table().expect(
            "Piece entry is not a valid table"
        );

        let name = piece_data["name"].as_str().unwrap_or_else(
            || panic!("Piece name not found in configuration file")
        ).to_string();

        let charset = piece_data["charset"].as_str().unwrap_or_else(
            || panic!("Piece char not found in configuration file")
        );

        assert!(
            charset.len() == 2,
            "Piece char must be a single character"
        );

        let white_char = charset.chars().next().unwrap();
        let black_char = charset.chars().nth(1).unwrap();

        let value = match &piece_data["value"] {
            v if v.is_integer() => v.as_integer().unwrap() as u16,
            v if v.is_float() => u16::MAX,
            _ => panic!(
                "Piece value not found or invalid in configuration file"
            ),
        };

        let movement = piece_data["movement"].as_str().unwrap_or_else(
            || panic!("Piece movement not found in configuration file")
        ).to_string();

        let is_royal = piece_data["royal"].as_bool().unwrap_or(false);
        let is_big = piece_data["big"].as_bool().unwrap_or(false);
        let is_major = piece_data["major"].as_bool().unwrap_or(false);

        if white_char != '_' {
            pieces.push(
                Piece::new(
                    name.clone(),
                    movement.clone(),
                    white_char,
                    U2048::ZERO,
                    0,                                                          /* placeholder                        */
                    WHITE,
                    is_royal,
                    is_big,
                    is_major,
                    value,
                    movement.contains("o"),
                    movement.contains("O"),
                )
            );
        }

        if black_char != '_' {
            pieces.push(
                Piece::new(
                    name.clone(),
                    movement.clone(),
                    black_char,
                    U2048::ZERO,
                    0,                                                          /* placeholder                        */
                    BLACK,
                    is_royal,
                    is_big,
                    is_major,
                    value,
                    movement.contains("o"),
                    movement.contains("O"),
                )
            );
        }
    }

    pieces.sort_by_key(|piece| p_color!(piece));

    for (i, piece) in pieces.iter_mut().enumerate() {
        piece.encoded_piece |= i as u32;                                        /* set the piece index correctly       */
        piece_char_to_idx.insert(piece.char, p_index!(piece));
    }

    let mut special_rules = 0u32;
    let special_rules_table = config.get("special_rules").expect(
        "Special rules section not found in configuration file"
    ).as_table().expect(
        "Special rules section is not a valid table"
    );

    let castling = special_rules_table.get("castling")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let en_passant = special_rules_table.get("en_passant")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let promotions = special_rules_table.get("promotion")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let drops = special_rules_table.get("drop")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let count_limit = special_rules_table.get("count_limit")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let forbidden_zones = special_rules_table.get("forbidden_zones")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    if castling {
        enc_castling!(special_rules);
    }

    if en_passant {
        enc_en_passant!(special_rules);
    }

    if promotions {
        enc_promotions!(special_rules);
    }

    if drops {
        enc_drops!(special_rules);
    }

    if count_limit {
        enc_count_limit!(special_rules);
    }

    if forbidden_zones {
        enc_forbidden_zones!(special_rules);
    }

    let mut result = State::new(
        config["title"].as_str().unwrap_or_else(
            || panic!("Game title not found in configuration file")
        ).to_string(),
        config["files"].as_integer().unwrap_or_else(
            || panic!("Files not found in configuration file")
        ) as u8,
        config["ranks"].as_integer().unwrap_or_else(
            || panic!("Ranks not found in configuration file")
        ) as u8,
        pieces,
        special_rules,
    );

    let default_initial_setup = Table::new();
    let initial_setup_table = config.get("initial_setup")
        .and_then(|v| v.as_table())
        .unwrap_or(&default_initial_setup);

    for piece_char in initial_setup_table.keys() {
        let piece_index = piece_char_to_idx.get(
            &piece_char.chars().next().unwrap()
        ).unwrap_or_else(
            || panic!("Unknown promotion piece character: {}", piece_char)
        );

        let bit_fen = initial_setup_table[piece_char]
            .as_str();

        result.initial_setup[*piece_index as usize] =
            parse_bit_fen(bit_fen, &result);
    }

    if promotions {
        let piece_promotions_table = config.get("promotions").expect(
            "Piece promotions section not found in configuration file"
        ).as_table().expect(
            "Piece promotions section is not a valid table"
        );

        let default_mandatory_table = Table::new();
        let promotion_mandatory_table = config.get("promotion_zones_mandatory")
            .and_then(|v| v.as_table())
            .unwrap_or(&default_mandatory_table);

        let default_optional_table = Table::new();
        let promotion_optional_table = config.get("promotion_zones_optional")
            .and_then(|v| v.as_table())
            .unwrap_or(&default_optional_table);

        for piece_char in piece_promotions_table.keys() {
            let mut promotions_encoded = U2048::ZERO;

            let piece_index = piece_char_to_idx.get(
                &piece_char.chars().next().unwrap()
            ).unwrap_or_else(
                || panic!("Unknown promotion piece character: {}", piece_char)
            );

            let promoted_chars = piece_promotions_table[piece_char]
                .as_str()
                .unwrap_or_else(
                    || panic!("Piece promotions entry is not a valid string")
                );

            promotions_encoded |= U2048::from(promoted_chars.len());

            for (i, char) in promoted_chars.chars().enumerate() {
                let promoted_idx = piece_char_to_idx.get(&char).unwrap_or_else(
                    || panic!("Unknown promotion piece character: {}", char)
                );

                promotions_encoded |=
                    U2048::from(*promoted_idx as u32) << ((i + 1) * 8);
            }

            let piece = &mut result.pieces[*piece_index as usize];
            piece.promotions = promotions_encoded;

            let bit_fen_mandatory = promotion_mandatory_table
                .get(piece_char)
                .and_then(|v| v.as_str());

            result.promotion_zones_mandatory[*piece_index as usize] =
                parse_bit_fen(bit_fen_mandatory, &result);

            let bit_fen_optional = promotion_optional_table
                .get(piece_char)
                .and_then(|v| v.as_str());

            result.promotion_zones_optional[*piece_index as usize] =
                parse_bit_fen(bit_fen_optional, &result);
        };
    }

    let initial_position = config["initial_position"].as_str().unwrap_or_else(
        || panic!("Initial position not found in configuration file")
    );

    result.load_fen(initial_position);

    hash_position(&result);

    result
}

fn parse_bit_fen(fen: Option<&str>, state: &State) -> Board {
    if fen.is_none() {
        return board!(state.files, state.ranks);
    }

    let fen = fen.unwrap();

    let mut result = board!(state.files, state.ranks);

    let mut rank = state.ranks - 1;
    let mut file = 0u8;

    let mut position_chars = fen.chars().peekable();

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
                set!(
                    result,
                    (rank as u32) * (state.files as u32) + (file as u32)
                );
                file += 1;
            }
        }
    }

    result
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
                    |piece| piece.char == c
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

                if get!(
                    state.initial_setup[p_index!(piece) as usize], square_index
                ) {
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
            |(i, b)| format_board(b, Some(state.pieces[i].char))
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
                        promo_from_symbols.push(other_piece.char);
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

pub fn format_promotion_zones(state: &State) -> String {
    let mut result = String::new();

    for piece in &state.pieces {
        if piece.promotions != U2048::ZERO {
            let mandatory_zone = format_board(
                &state.promotion_zones_mandatory[p_index!(piece) as usize],
                piece.char.into()
            );
            let optional_zone = format_board(
                &state.promotion_zones_optional[p_index!(piece) as usize],
                piece.char.into()
            );

            let mandatory_lines: Vec<&str> = mandatory_zone.lines().collect();
            let optional_lines: Vec<&str> = optional_zone.lines().collect();
            let max_lines = mandatory_lines.len().max(optional_lines.len());

            result.push_str(&format!(
                "Promotion zones for {} ({}):\n", piece.name, piece.char
            ));
            result.push_str(
                &format!("{:<width$}{}\n",
                    "Mandatory Zone",
                    "Optional Zone",
                    width = mandatory_lines.iter().map(
                        |l| l.chars().count()
                    ).max().unwrap_or(0) + 4
                )
            );

            for i in 0..max_lines {
                let left = mandatory_lines.get(i).unwrap_or(&"");
                let right = optional_lines.get(i).unwrap_or(&"");
                let width = left.chars().count().max(right.chars().count()) + 4;
                result.push_str(
                    &format!("{:<width$}{}\n", left, right, width = width)
                );
            }
            result.push('\n');
        }
    }

    result
}

pub fn format_intial_setup(state: &State) -> String {
    let mut result = String::new();

    for piece in &state.pieces {
        let piece_setup = state.initial_setup[p_index!(piece) as usize];

        if piece_setup.2 == U2048::ZERO {
            continue;
        }

        let setup_board = format_board(
            &piece_setup,
            piece.char.into()
        );

        result.push_str(&format!(
            "Initial setup for {} ({}):\n", piece.name, piece.char
        ));
        result.push_str(&setup_board);
        result.push('\n');
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
    result.push_str("\n---------- Static Parameters\n");
    result.push_str(&format_promotion_zones(state));
    result.push_str(&format_intial_setup(state));
    result.push_str("\n---------- Initial Position\n");
    result.push_str(&format_game_state(state, true));

    result
}