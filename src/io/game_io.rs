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

use bnum::types::{U2048, U4096};
use bnum::cast::As;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::fs;

use crate::game::representations::board::Board;
use crate::{
    board, castling, clear, constants::*, count_limits, demote_upon_capture, drops, en_passant, enc_castling, enc_count_limits, enc_demote_upon_capture, enc_drops, enc_en_passant, enc_forbidden_zones, enc_promote_to_captured, enc_promotions, forbidden_zones, get, p_can_promote, p_castle_left, p_castle_right, p_color, p_index, p_is_big, p_is_major, p_is_minor, p_is_royal, p_promotions, p_value, promote_to_captured, promotions, set
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

lazy_static!{
    pub static ref CASTLING_PATTERN: Regex =
        Regex::new(r"^([KQkq]+)$|^-$").unwrap();
    pub static ref ENP_PATTERN: Regex =
        Regex::new(r"^([0-9a-fA-F]{3})([0-9a-fA-F]{3})(.)$|^\*$").unwrap();
    pub static ref HAND_PATTERN: Regex =
        Regex::new(r"^(.*)/(.*)$").unwrap();
    pub static ref COMMENT_PATTERN: Regex =
        Regex::new(r"//[^\n\r]*").unwrap();
    pub static ref SECTION_PATTERN: Regex =
        Regex::new(r"\[(.+)\][^\[\]]+").unwrap();
    pub static ref IN_HAND_PATTERN: Regex =
        Regex::new(r"(\d*)(.)").unwrap();
}

fn determine_board_dimensions(fen: &str) -> (u8, u8) {
    let ranks_data: Vec<&str> = fen.split('/').collect();
    let rank_count = ranks_data.len() as u8;
    let file_count = ranks_data[0].chars().fold(0u8, |acc, c| {
        if c.is_ascii_digit() {
            acc + c.to_digit(10).unwrap() as u8
        } else {
            acc + 1
        }
    });
    (file_count, rank_count)
}

// returns (castling, en_passant, pieces in hand)
fn extract_fen_components(fen: &str) -> (bool, bool, bool) {
    let parts = &fen.split_whitespace()
        .map(str::to_string)
        .collect::<Vec<String>>()[2..];

    let castling = parts.iter().any(|part| CASTLING_PATTERN.is_match(part));
    let en_passant = parts.iter().any(|part| ENP_PATTERN.is_match(part));
    let in_hand = parts.iter().any(|part| HAND_PATTERN.is_match(part));

    (castling, en_passant, in_hand)
}

/// Parses a game configuration file and initializes a game state.
/// See example.conf for the expected format of the configuration file.
///
/// pieces are parsed into a tuple first of:
/// (string, string, char, U2048, u8, u8, bool, bool, bool, u16)
///
/// where the fields are:
/// - string: the name of the piece
/// - string: the movement pattern of the piece
/// - char: the character representing the piece on the board
/// - U2048: the promotions bitset representing which pieces this piece can
///   promote to
/// - u8: the piece index (0-255, with 255 reserved for "no piece")
/// - u8: the piece color (0 for white, 1 for black)
/// - bool: whether the piece is royal
/// - bool: whether the piece is big
/// - bool: whether the piece is major
/// - u16: the value of the piece
pub fn parse_config_file(path: &str) -> State {
    let file_str = fs::read_to_string(path)
        .expect("Failed to read configuration file");

    let uncommented_str = COMMENT_PATTERN.replace_all(&file_str, "");
    let cleaned = uncommented_str
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");
    let sections = SECTION_PATTERN.captures_iter(&cleaned)
        .map(|cap| {
            let section_name = cap[1].trim().to_string();
            let section_body = cap[0]
            .lines()
            .skip(1)
            .map(str::to_string)
            .filter(|line| !line.trim().is_empty())
            .collect::<Vec<String>>();
            (section_name, section_body)
        })
        .collect::<HashMap<_, _>>();

    let mandatory_sections = [
        "general", "pieces", "piece moves", "piece values", "piece roles"
    ];

    let missing: Vec<_> = mandatory_sections
        .iter()
        .filter(|s| !sections.contains_key(**s))
        .cloned()
        .collect();

    assert!(
        missing.is_empty(),
        "Missing mandatory sections: {}",
        missing.join(", ")
    );

/*----------------------------------------------------------------------------*\
                             PARSE GENERAL SECTION
\*----------------------------------------------------------------------------*/

    let title = sections["general"][0].trim();
    let initial_position = sections["general"][1].trim();
    let initial_board = initial_position.split_whitespace().next().unwrap();
    let (files, ranks) = determine_board_dimensions(initial_board);

/*----------------------------------------------------------------------------*\
                              PARSE RULES SECTION
\*----------------------------------------------------------------------------*/

    let castling = sections["rules"]
        .contains(&"castling".to_string());
    let en_passant = sections["rules"]
        .contains(&"en passant".to_string());
    let promotions = sections["rules"]
        .contains(&"promotions".to_string());
    let drops = sections["rules"]
        .contains(&"drops".to_string());
    let piece_count_limits = sections["rules"]
        .contains(&"piece count limits".to_string());
    let forbidden_zones = sections["rules"]
        .contains(&"forbidden zones".to_string());
    let promote_to_captured = sections["rules"]
        .contains(&"promote to captured".to_string());
    let demote_upon_capture = sections["rules"]
        .contains(&"demote upon capture".to_string());

    let (fen_castling, fen_en_passant, fen_in_hand) =
        extract_fen_components(initial_position);

    if castling {
        assert!(
            fen_castling,
            "No castling rights found in FEN"
        );
    }

    if !castling {
        assert!(
            !fen_castling,
            "Castling rights found in FEN"
        );
    }

    if en_passant {
        assert!(
            fen_en_passant,
            "No en passant square found in FEN"
        );
    }

    if !en_passant {
        assert!(
            !fen_en_passant,
            "En passant square found in FEN"
        );
    }

    if drops || promote_to_captured || demote_upon_capture {
        assert!(
            fen_in_hand,
            "No pieces in hand found in FEN"
        );
    }

    if promotions {
        assert!(
            sections.contains_key("promotions"),
            "[promotions] section is missing"
        );
        assert!(
            sections.contains_key("mandatory promotion zones") ||
            sections.contains_key("optional promotion zones"),
            "No promotion zones section found"
        );
    }

    if piece_count_limits {
        assert!(
            sections.contains_key("piece count limits"),
            "[piece count limits] section is missing"
        );
    }

    if forbidden_zones {
        assert!(
            sections.contains_key("forbidden zones"),
            "[forbidden zones] section is missing"
        );
    }

    let mut special_rules = 0u32;

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

    if piece_count_limits {
        enc_count_limits!(special_rules);
    }

    if forbidden_zones {
        enc_forbidden_zones!(special_rules);
    }

    if promote_to_captured {
        enc_promote_to_captured!(special_rules);
    }

    if demote_upon_capture {
        enc_demote_upon_capture!(special_rules);
    }

/*----------------------------------------------------------------------------*\
                                  PARSE PIECES
\*----------------------------------------------------------------------------*/

    let mut pieces = Vec::with_capacity(sections["pieces"].len());
    let mut char_to_index: HashMap<char, usize> = HashMap::new();
    for bare_piece in &sections["pieces"] {
        let parts: Vec<&str> = bare_piece.split(':').map(str::trim).collect();

        assert!(
            parts.len() == 2,
            "Invalid piece definition: {}",
            bare_piece
        );

        let white_char = parts[0].chars().next().unwrap();
        let black_char = parts[0].chars().nth(1).unwrap();
        let name = parts[1].to_string();

        pieces.push((
            name.clone(),
            String::new(),
            white_char,
            U2048::ZERO,
            0,
            WHITE,
            false,
            false,
            false,
            0,
        ));

        pieces.push((
            name.clone(),
            String::new(),
            black_char,
            U2048::ZERO,
            0,
            BLACK,
            false,
            false,
            false,
            0,
        ));
    }

    pieces.sort_by_key(|piece| piece.5);

    for (i, piece) in pieces.iter_mut().enumerate() {
        piece.4 = i as u8;
        char_to_index.insert(piece.2, i);
    }

    for piece_values in &sections["piece values"] {
        let parts: Vec<&str> = piece_values.split(':').map(str::trim).collect();

        assert!(
            parts.len() == 2,
            "Invalid piece value definition: {}",
            piece_values
        );

        let piece_chars = parts[0];
        let value_str = parts[1];

        if piece_chars.len() == 2 {
            let white_char = piece_chars.chars().next().unwrap();
            let black_char = piece_chars.chars().nth(1).unwrap();

            let value = value_str.parse::<u16>().unwrap_or_else(
                |_| panic!("Invalid piece value: {}", value_str.trim())
            );

            if let Some(&white_index) = char_to_index.get(&white_char) {
                pieces[white_index].9 = value;
            } else {
                panic!("Unknown piece character: {}", white_char);
            }

            if let Some(&black_index) = char_to_index.get(&black_char) {
                pieces[black_index].9 = value;
            } else {
                panic!("Unknown piece character: {}", black_char);
            }
        } else if piece_chars.len() == 1 {
            let piece_char = piece_chars.chars().next().unwrap();

            let value = value_str.parse::<u16>().unwrap_or_else(
                |_| panic!("Invalid piece value: {}", value_str.trim())
            );

            if let Some(&index) = char_to_index.get(&piece_char) {
                pieces[index].9 = value;
            } else {
                panic!("Unknown piece character: {}", piece_char);
            }
        } else {
            panic!("Invalid piece character(s): {}", piece_chars);
        }
    }

    for piece_moves in &sections["piece moves"] {
        let parts: Vec<&str> = piece_moves.split(':').map(str::trim).collect();

        assert!(
            parts.len() == 2,
            "Invalid piece move definition: {}",
            piece_moves
        );

        let piece_chars = parts[0];
        let move_pattern = parts[1].to_string();

        if piece_chars.len() == 2 {
            let white_char = piece_chars.chars().next().unwrap();
            let black_char = piece_chars.chars().nth(1).unwrap();

            if let Some(&white_index) = char_to_index.get(&white_char) {
                pieces[white_index].1 = move_pattern.clone();
            } else {
                panic!("Unknown piece character: {}", white_char);
            }

            if let Some(&black_index) = char_to_index.get(&black_char) {
                pieces[black_index].1 = move_pattern.clone();
            } else {
                panic!("Unknown piece character: {}", black_char);
            }
        } else if piece_chars.len() == 1 {
            let piece_char = piece_chars.chars().next().unwrap();

            if let Some(&index) = char_to_index.get(&piece_char) {
                pieces[index].1 = move_pattern.clone();
            } else {
                panic!("Unknown piece character: {}", piece_char);
            }
        } else {
            panic!("Invalid piece character(s): {}", piece_chars);
        }
    }

    for piece_roles in &sections["piece roles"] {
        let parts: Vec<&str> = piece_roles.split(':').map(str::trim).collect();

        assert!(
            parts.len() == 2,
            "Invalid piece move definition: {}",
            piece_roles
        );

        match parts[0] {
            "royal" => {
                for piece_char in parts[1].chars() {
                    if let Some(&index) = char_to_index.get(&piece_char) {
                        pieces[index].6 = true;
                    } else {
                        panic!("Unknown piece character: {}", piece_char);
                    }
                }
            }
            "big" => {
                for piece_char in parts[1].chars() {
                    if let Some(&index) = char_to_index.get(&piece_char) {
                        pieces[index].7 = true;
                    } else {
                        panic!("Unknown piece character: {}", piece_char);
                    }
                }
            }
            "major" => {
                for piece_char in parts[1].chars() {
                    if let Some(&index) = char_to_index.get(&piece_char) {
                        pieces[index].8 = true;
                    } else {
                        panic!("Unknown piece character: {}", piece_char);
                    }
                }
            }
            _ => panic!("Invalid piece role type: {}", parts[0])
        }
    }

    if promotions {
        for piece_promotion in &sections["promotions"] {
            let parts: Vec<&str> =
                piece_promotion.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid piece promotion definition: {}",
                piece_promotion
            );

            let piece_chars = parts[0];

            if piece_chars.len() == 2 {
                let white_char = piece_chars.chars().next().unwrap();
                let black_char = piece_chars.chars().nth(1).unwrap();

                let white_index = char_to_index
                    .get(&white_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", white_char)
                    );

                let black_index = char_to_index
                    .get(&black_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", black_char)
                    );

                let promotions_str = parts[1];
                let promotion_count = promotions_str.chars().count();

                let mut promotions_bitset = U2048::from(promotion_count);

                for (index, promo_char) in promotions_str.chars().enumerate() {
                    if let Some(&promo_index) = char_to_index.get(&promo_char) {
                        promotions_bitset |=
                            U2048::from(promo_index) << ((index + 1) * 8);
                    } else {
                        panic!(
                            "Unknown promotion piece character: {}", promo_char
                        );
                    }
                }

                pieces[white_index].3 = promotions_bitset;
                pieces[black_index].3 = promotions_bitset;
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                let piece_index = char_to_index
                    .get(&piece_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", piece_char)
                    );

                let promotions_str = parts[1];
                let promotion_count = promotions_str.chars().count();

                let mut promotions_bitset = U2048::from(promotion_count);

                for (index, promo_char) in promotions_str.chars().enumerate() {
                    if let Some(&promo_index) = char_to_index.get(&promo_char) {
                        promotions_bitset |=
                            U2048::from(promo_index) << ((index + 1) * 8);
                    } else {
                        panic!(
                            "Unknown promotion piece character: {}", promo_char
                        );
                    }
                }

                pieces[piece_index].3 = promotions_bitset;
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

/*----------------------------------------------------------------------------*\
                             POPULATE STATIC FIELDS
\*----------------------------------------------------------------------------*/

    let mut result = State::new(
        title.to_string(),
        files,
        ranks,
        pieces.into_iter().map(|p| Piece::new(
            p.0, p.1, p.2, p.3, p.4, p.5, p.6, p.7, p.8, p.9
        )).collect(),
        special_rules,
    );

    let template_bit_fen = initial_position.split_whitespace().next().unwrap();
    for (index, piece) in result.pieces.iter().enumerate() {
        let bit_fen = template_bit_fen
            .chars()
            .map(|c| {
            if c == piece.char {
                'X'
            } else if c.is_ascii_alphabetic() {
                'O'
            } else {
                c
            }
            })
            .collect::<String>();
        result.initial_setup[index] = parse_bit_fen(Some(&bit_fen), &result);
    }

    for (i, piece) in result.pieces.iter().enumerate() {
        if let Some(other_idx) = result.pieces.iter().position(|p| {
            p.name == piece.name && p_color!(p) != p_color!(piece)
        }) {
            result.piece_swap_map.insert(i as u8, other_idx as u8);
        }
    }

    for piece in &result.pieces {
        if !p_can_promote!(piece) {
            continue;
        }
        for promotion_index in p_promotions!(piece) {
            result.piece_demotion_map
                .entry(promotion_index as u8)
                .or_default()
                .push(p_index!(piece));
        }
    }

    if castling {
        assert!(
            result.pieces.iter().any(
                |p| p_castle_left!(p) || p_castle_right!(p)
            ),
            "No castling rights found in piece definitions"
        );
    }

    if en_passant {
        assert!(
            result.pieces.iter().any(
                |p| p.movement.contains('p') || p.movement.contains('t')
            ),
            "No en passant movement found in piece definitions"
        );
    }

    if !castling {
        assert!(
            result.pieces.iter().all(
                |p| !p_castle_left!(p) && !p_castle_right!(p)
            ),
            "Castling rights found in piece definitions"
        );
    }

    if !en_passant {
        assert!(
            result.pieces.iter().all(
                |p| !p.movement.contains('p') && !p.movement.contains('t')
            ),
            "En passant movement found in piece definitions"
        );
    }

    if promotions {
        if sections.contains_key("mandatory promotion zones") {
            for mandatory in &sections["mandatory promotion zones"] {
                let parts: Vec<&str> =
                    mandatory.split(':').map(str::trim).collect();

                assert!(
                    parts.len() == 2,
                    "Invalid mandatory promotion zone definition: {}",
                    mandatory
                );

                let piece_chars = parts[0];

                if piece_chars.len() == 2 {
                    let white_char = piece_chars.chars().next().unwrap();
                    let black_char = piece_chars.chars().nth(1).unwrap();

                    let white_index = char_to_index
                        .get(&white_char)
                        .copied()
                        .unwrap_or_else(
                            || panic!("Unknown piece character: {}", white_char)
                        );

                    let black_index = char_to_index
                        .get(&black_char)
                        .copied()
                        .unwrap_or_else(
                            || panic!("Unknown piece character: {}", black_char)
                        );

                    let zone_str = parts[1];

                    result.promotion_zones_mandatory[white_index] =
                        parse_bit_fen(Some(zone_str), &result);

                    result.promotion_zones_mandatory[black_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else if piece_chars.len() == 1 {
                    let piece_char = piece_chars.chars().next().unwrap();

                    let piece_index = char_to_index
                        .get(&piece_char)
                        .copied()
                        .unwrap_or_else(
                            || panic!("Unknown piece character: {}", piece_char)
                        );

                    let zone_str = parts[1];

                    result.promotion_zones_mandatory[piece_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else {
                    panic!("Invalid piece character(s): {}", piece_chars);
                }
            }
        }

        if sections.contains_key("optional promotion zones") {
            for optional in &sections["optional promotion zones"] {
                let parts: Vec<&str> =
                    optional.split(':').map(str::trim).collect();

                assert!(
                    parts.len() == 2,
                    "Invalid optional promotion zone definition: {}",
                    optional
                );

                let piece_chars = parts[0];

                if piece_chars.len() == 2 {
                    let white_char = piece_chars.chars().next().unwrap();
                    let black_char = piece_chars.chars().nth(1).unwrap();

                    let white_index = char_to_index
                        .get(&white_char)
                        .copied()
                        .unwrap_or_else(
                            || panic!("Unknown piece character: {}", white_char)
                        );

                    let black_index = char_to_index
                        .get(&black_char)
                        .copied()
                        .unwrap_or_else(
                            || panic!("Unknown piece character: {}", black_char)
                        );

                    let zone_str = parts[1];

                    result.promotion_zones_optional[white_index] =
                        parse_bit_fen(Some(zone_str), &result);

                    result.promotion_zones_optional[black_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else if piece_chars.len() == 1 {
                    let piece_char = piece_chars.chars().next().unwrap();

                    let piece_index = char_to_index
                        .get(&piece_char)
                        .copied()
                        .unwrap_or_else(
                            || panic!("Unknown piece character: {}", piece_char)
                        );

                    let zone_str = parts[1];

                    result.promotion_zones_optional[piece_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else {
                    panic!("Invalid piece character(s): {}", piece_chars);
                }
            }
        }
    }

    if piece_count_limits {
        for limit in &sections["piece count limits"] {
            let parts: Vec<&str> = limit.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid piece count limit definition: {}",
                limit
            );

            let piece_chars = parts[0];

            if piece_chars.len() == 2 {
                let white_char = piece_chars.chars().next().unwrap();
                let black_char = piece_chars.chars().nth(1).unwrap();

                let white_index = char_to_index
                    .get(&white_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", white_char)
                    );

                let black_index = char_to_index
                    .get(&black_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", black_char)
                    );

                let limit_str = parts[1];

                let limit_value = limit_str.parse::<u32>().unwrap_or_else(
                    |_| panic!("Invalid piece count limit: {}", limit_str.trim())
                );

                result.piece_limit[white_index] = limit_value;
                result.piece_limit[black_index] = limit_value;
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                let piece_index = char_to_index
                    .get(&piece_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", piece_char)
                    );

                let limit_str = parts[1];

                let limit_value = limit_str.parse::<u32>().unwrap_or_else(
                    |_| panic!("Invalid piece count limit: {}", limit_str.trim())
                );

                result.piece_limit[piece_index] = limit_value;
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

    if forbidden_zones {
        for forbidden in &sections["forbidden zones"] {
            let parts: Vec<&str> =
                forbidden.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid forbidden zone definition: {}",
                forbidden
            );

            let piece_chars = parts[0];

            if piece_chars.len() == 2 {
                let white_char = piece_chars.chars().next().unwrap();
                let black_char = piece_chars.chars().nth(1).unwrap();

                let white_index = char_to_index
                    .get(&white_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", white_char)
                    );

                let black_index = char_to_index
                    .get(&black_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", black_char)
                    );

                let zone_str = parts[1];

                result.forbidden_zones[white_index] =
                    parse_bit_fen(Some(zone_str), &result);

                result.forbidden_zones[black_index] =
                    parse_bit_fen(Some(zone_str), &result);
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                let piece_index = char_to_index
                    .get(&piece_char)
                    .copied()
                    .unwrap_or_else(
                        || panic!("Unknown piece character: {}", piece_char)
                    );

                let zone_str = parts[1];

                result.forbidden_zones[piece_index] =
                    parse_bit_fen(Some(zone_str), &result);
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

    result.load_fen(initial_position);

    hash_position(&result);
    result.precompute();

    result
}

fn parse_bit_fen(fen: Option<&str>, state: &State) -> Board {
    if fen.is_none() {
        return board!(state.files, state.ranks);
    }

    let fen = fen.unwrap();

    let ranks_data: Vec<&str> = fen.split('/').collect();
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
            'O' => {
                clear!(
                    result,
                    (rank as u32) * (state.files as u32) + (file as u32)
                );
                file += 1;
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
/// sizes, the en passant square is represented as `xxyyzz` where `xx` is the
/// file and `yy` is the rank (both 0-indexed) and zz is the piece index in hex.
///
/// For variants where there is pieces in hand, it is in the format
/// (white)/(black) where each part is formatted as follows:
/// 3P2N means 3 pawns and 2 knights in hand.
/// RQ means 1 rook and 1 queen in hand.
/// - means no pieces in hand. so both empty would be -/-
///
/// The piece characters are the same as the ones used in the board
/// representation part of the FEN.
///
/// the order is as follows:
/// 1. Board representation (ranks separated by '/')
/// 2. Active color ('w' or 'b')
/// 3. Castling rights (e.g. 'KQkq' or '-')
/// 4. En passant square (e.g. 'e3' or '-')
/// 5. Pieces in hand (e.g. '3P2N/1p' or '-')
///
/// Optional:
/// 6. Halfmove clock (number of halfmoves since last capture or pawn move)
/// 7. Fullmove number (starting at 1 and incremented after
pub fn parse_fen(state: &mut State, fen: &str) {

    let mut needed_parts = 2;

    if castling!(state) {
        needed_parts += 1;
    }

    if en_passant!(state) {
        needed_parts += 1;
    }

    if drops!(state) || promote_to_captured!(state) {
        needed_parts += 1;
    }

    let parts: Vec<&str> = fen.split_whitespace().collect();
    assert!(
        parts.len() >= needed_parts,
        "FEN must have at least {needed_parts} parts"
    );
    let mut part_index = 0;

    let position = parts[part_index];
    part_index += 1;
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

    state.playing = match parts[part_index] {
        "w" => WHITE,
        "b" => BLACK,
        _ => panic!("Invalid active color: {}", parts[1]),
    };
    part_index += 1;

    if castling!(state) {
        let castling = parts[part_index];
        part_index += 1;
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
    }

    if en_passant!(state) {
        let en_passant = parts[part_index];
        part_index += 1;
        state.en_passant_square = if en_passant == "*" {
            NO_EN_PASSANT
        } else {
            let s = &en_passant[0..3];
            let e = &en_passant[3..6];
            let z = &en_passant[6..7];

            let square_index = u32::from_str_radix(s, 16)
                .unwrap_or_else(
                    |_| panic!("Invalid en passant square: {}", s)
                );
            let piece_square_index = u32::from_str_radix(e, 16)
                .unwrap_or_else(
                    |_| panic!("Invalid en passant captured square: {}", e)
                );
            let piece_index = state.pieces.iter().position(
                |piece| piece.char == z.chars().next().unwrap()
            ).unwrap_or_else(
                || panic!("Unknown piece character: {}", z)
            ) as u32;

            square_index | (piece_square_index << 12) | piece_index << 24
        };
    }

    if drops!(state)
    || promote_to_captured!(state)
    || demote_upon_capture!(state)
    {
        let hands = parts[part_index];
        part_index += 1;

        let char_to_index: HashMap<char, u8> = state.pieces.iter()
            .map(|p| (p.char, p_index!(p)))
            .collect();
        let hand_parts: Vec<&str> = hands.split('/').collect();
        assert!(
            hand_parts.len() == 2,
            "Invalid pieces in hand format: {}",
            hands
        );

        for (color_idx, hand_part) in hand_parts.iter().enumerate() {
            if hand_part == &"-" {
                continue;
            }

            for m in IN_HAND_PATTERN.captures_iter(hand_part) {
                let count_str = m.get(1)
                    .unwrap().as_str();
                let piece_char = m.get(2)
                    .unwrap().as_str().chars().next().unwrap();

                let count = if count_str.is_empty() {
                    1
                } else {
                    count_str.parse::<u16>().unwrap_or_else(
                        |_| panic!("Invalid piece count: {}", count_str.trim())
                    )
                };

                let piece_index = char_to_index.get(&piece_char).unwrap_or_else(
                    || panic!("Unknown piece character in hand: {}", piece_char)
                );

                state.piece_in_hand[color_idx][*piece_index as usize] = count;
            }
        }
    }

    if parts.len() > part_index {
        state.halfmove_clock = parts[part_index].parse()
            .unwrap_or_else(
                |_| panic!(
                    "Invalid halfmove clock: {}", parts[part_index].trim()
                )
            );
        part_index += 1;
    }

    if parts.len() > part_index {
        state.ply_counter = parts[part_index].parse()
            .unwrap_or_else(
                |_| panic!(
                    "Invalid ply number: {}", parts[part_index].trim()
                )
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
                "Side to move\t: {}\n",
                if state.playing == WHITE { "White" } else { "Black" }
            )
        );

        if castling!(state) {
            result.push_str(&format!("Castling rights\t: {}\n", {
                let mut rights = String::new();
                if state.castling_state & WK_CASTLE != 0 { rights.push('K'); }
                if state.castling_state & WQ_CASTLE != 0 { rights.push('Q'); }
                if state.castling_state & BK_CASTLE != 0 { rights.push('k'); }
                if state.castling_state & BQ_CASTLE != 0 { rights.push('q'); }
                if rights.is_empty() { "-".to_string() } else { rights }
            }));
        }

        if en_passant!(state) {
            result.push_str(&format!("En passant\t: {}\n",
                if state.en_passant_square == u32::MAX {
                    "-".to_string()
                } else {
                    format_square(
                        (state.en_passant_square & 0xFFF) as u16, state
                    )
                }
            ));
        }

        result.push_str(
            &format!("Halfmove clock\t: {}\n", state.halfmove_clock)
        );

        if drops!(state) || promote_to_captured!(state) {
            let pieces_in_white = &state.piece_in_hand[WHITE as usize];
            let pieces_in_black = &state.piece_in_hand[BLACK as usize];

            result.push_str("White's hand\t: ");
            for (i, piece) in state.pieces.iter().enumerate() {
                let count = pieces_in_white[i];
                if count == 1 {
                    result.push_str(&format!("{}", piece.char));
                } else if count > 1 {
                    result.push_str(&format!("{}{}", count, piece.char));
                }
            }

            result.push_str("\nBlack's hand\t: ");
            for (i, piece) in state.pieces.iter().enumerate() {
                let count = pieces_in_black[i];
                if count == 1 {
                    result.push_str(&format!("{}", piece.char));
                } else if count > 1 {
                    result.push_str(&format!("{}{}", count, piece.char));
                }
            }
        }
    }

    result.push('\n');
    result
}

/// Formats all piece types in the game state as tables.
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
    let mut headers = vec![
        "Name",
        "Index",
        "Symbol",
        "Color",
        "Value",
        "Royal",
        "Major",
    ];

    if count_limits!(state) {
        headers.push("Count Limit");
    }

    if promotions!(state) {
        headers.push("Can Promote");
        headers.push("Promotes From");
    }

    let mut result = String::new();
    let mut pieces_per_table = state.pieces.len();
    while pieces_per_table > 9 {
        pieces_per_table = pieces_per_table.div_ceil(2);
    }
    let num_tables = state.pieces.len().div_ceil(pieces_per_table);

    const HEADER_WIDTH: usize = 16;
    const PIECE_WIDTH: usize = 12;

    for table_idx in 0..num_tables {
        let start_idx = table_idx * pieces_per_table;
        let end_idx = (
            (table_idx + 1) * pieces_per_table).min(state.pieces.len()
        );
        let pieces_in_table = end_idx - start_idx;

        let mut piece_columns: Vec<Vec<String>> = Vec::new();

        for piece_idx in start_idx..end_idx {
            let formatted = format_piece(&state.pieces[piece_idx], state);
            let lines: Vec<&str> = formatted.lines().collect();

            let mut piece_data = Vec::new();
            let data_lines: Vec<_> = lines
                .iter()
                .filter(
                    |line|
                    line.trim_start().starts_with('│') &&
                    line.trim_end().ends_with('│')
                )
                .collect();

            for line in data_lines {
                let content = line
                    .trim_start_matches("│ ")
                    .trim_end_matches(" │");
                piece_data.push(content.to_string());
            }

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

        for row_idx in 0..headers.len() {
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

            if row_idx < headers.len() - 1 {
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

    result.push('\n');
    result
}

fn format_promotion_zones(state: &State) -> String {
    let mut result = String::new();
    let mut seen_names = HashSet::new();

    for piece in &state.pieces {
        if !p_can_promote!(piece) || !seen_names.insert(piece.name.clone()) {
            continue;
        }

        let white = state.pieces
            .iter().find(|p| p.name == piece.name && p_color!(p) == WHITE);
        let black = state.pieces
            .iter().find(|p| p.name == piece.name && p_color!(p) == BLACK);

        let (w_label, w_mand, w_opt) = if let Some(w) = white {
            let mand = &state.promotion_zones_mandatory[p_index!(w) as usize];
            let opt = &state.promotion_zones_optional[p_index!(w) as usize];
            (
                format!("White ({})", w.char),
                if mand.2 == U4096::ZERO { "".to_string() }
                else { format_board(mand, Some(w.char)) },
                if opt.2 == U4096::ZERO { "".to_string() }
                else { format_board(opt, Some(w.char)) },
            )
        } else {
            ("White (-)".to_string(), "".to_string(), "".to_string())
        };

        let (b_label, b_mand, b_opt) = if let Some(b) = black {
            let mand = &state.promotion_zones_mandatory[p_index!(b) as usize];
            let opt = &state.promotion_zones_optional[p_index!(b) as usize];
            (
                format!("Black ({})", b.char),
                if mand.2 == U4096::ZERO { "".to_string() }
                else { format_board(mand, Some(b.char)) },
                if opt.2 == U4096::ZERO { "".to_string() }
                else { format_board(opt, Some(b.char)) },
            )
        } else {
            ("Black (-)".to_string(), "".to_string(), "".to_string())
        };

        if w_label == "White (-)" && b_label == "Black (-)" {
            continue;
        }

        let w_mand_lines: Vec<_> = w_mand.lines().collect();
        let w_opt_lines: Vec<_> = w_opt.lines().collect();
        let b_mand_lines: Vec<_> = b_mand.lines().collect();
        let b_opt_lines: Vec<_> = b_opt.lines().collect();

        let max_lines = [
            w_mand_lines.len(),
            w_opt_lines.len(),
            b_mand_lines.len(),
            b_opt_lines.len(),
        ]
        .into_iter()
        .max()
        .unwrap();

        let board_width = state.files as usize * 4 + 4;

        if !w_mand_lines.is_empty()
        || !b_mand_lines.is_empty()
        || !w_opt_lines.is_empty()
        || !b_opt_lines.is_empty()
        {
            result.push_str(&format!(
                "{:^width$}\n",
                format!("Promotion Zones for the {}\n", piece.name),
                width = board_width * 2 + 8
            ));
            result.push_str(&format!(
                "   {:<label_width$}    {:<label_width$}",
                w_label, b_label, label_width = board_width + 2
            ));
        }
        if !w_mand_lines.is_empty() || !b_mand_lines.is_empty() {
            result.push_str(&format!(
            "\n   {:<label_width$}    {:<label_width$}\n",
            "Mandatory", "Mandatory", label_width = board_width + 2
            ));
            for i in 0..max_lines {
                let wl = w_mand_lines.get(i).unwrap_or(&"");
                let bl = b_mand_lines.get(i).unwrap_or(&"");
                result.push_str(&format!(
                    "{:<label_width$}    {:<label_width$}\n",
                    wl, bl, label_width = board_width + 2
                ));
            }
        }

        if !w_opt_lines.is_empty() || !b_opt_lines.is_empty() {
            result.push_str(&format!(
            "\n   {:<label_width$}    {:<label_width$}\n",
            "Optional", "Optional", label_width = board_width + 2
            ));
            for i in 0..max_lines {
                let wl = w_opt_lines.get(i).unwrap_or(&"");
                let bl = b_opt_lines.get(i).unwrap_or(&"");
                result.push_str(&format!(
                    "{:<label_width$}    {:<label_width$}\n",
                    wl, bl, label_width = board_width + 2
                ));
            }
        }
        result.push('\n');
    }
    result
}

fn format_intial_setup(state: &State) -> String {
    let mut result = String::new();
    let mut seen_names = HashSet::new();

    for piece in &state.pieces {
        if !seen_names.insert(piece.name.clone()) {
            continue;
        }

        if state.initial_setup[p_index!(piece) as usize].2 == U4096::ZERO {
            continue;
        }

        let white = state.pieces
            .iter().find(|p| p.name == piece.name && p_color!(p) == WHITE);
        let black = state.pieces
            .iter().find(|p| p.name == piece.name && p_color!(p) == BLACK);

        let (w_label, w_board) = if let Some(w) = white {
            (
                format!("White ({})", w.char),
                format_board(
                    &state.initial_setup[p_index!(w) as usize], Some(w.char)
                ),
            )
        } else {
            ("White (-)".to_string(), "".to_string())
        };
        let (b_label, b_board) = if let Some(b) = black {
            (
                format!("Black ({})", b.char),
                format_board(
                    &state.initial_setup[p_index!(b) as usize], Some(b.char)
                ),
            )
        } else {
            ("Black (-)".to_string(), "".to_string())
        };

        let w_lines: Vec<_> = w_board.lines().collect();
        let b_lines: Vec<_> = b_board.lines().collect();
        let max_lines = w_lines.len().max(b_lines.len());

        let board_width = state.files as usize * 4 + 4;

        result.push_str(&format!(
            "{:^width$}\n",
            format!("Initial Setup for the {}\n", piece.name),
            width = board_width * 2 + 8
        ));
        result.push_str(&format!(
            "   {:<label_width$}    {:<label_width$}\n",
            w_label, b_label, label_width = board_width + 2
        ));
        for i in 0..max_lines {
            let wl = w_lines.get(i).unwrap_or(&"");
            let bl = b_lines.get(i).unwrap_or(&"");
            result.push_str(&format!(
            "{:<label_width$}    {:<label_width$}\n",
            wl, bl, label_width = board_width + 2
            ));
        }
        result.push('\n');
    }
    result
}

fn format_forbidden_zones(state: &State) -> String {
    let mut result = String::new();
    let mut seen_names = HashSet::new();

    for piece in &state.pieces {
        if !seen_names.insert(piece.name.clone()) {
            continue;
        }

        let white = state.pieces.iter().find(|p| p.name == piece.name && p_color!(p) == WHITE);
        let black = state.pieces.iter().find(|p| p.name == piece.name && p_color!(p) == BLACK);

        let (w_label, w_board) = if let Some(w) = white {
            let forbidden_zone = &state.forbidden_zones[p_index!(w) as usize];
            if forbidden_zone.2 == U4096::ZERO {
                ("White (-)".to_string(), "".to_string())
            } else {
                (
                    format!("White ({})", w.char),
                    format_board(forbidden_zone, Some('X'))
                )
            }
        } else {
            ("White (-)".to_string(), "".to_string())
        };

        let (b_label, b_board) = if let Some(b) = black {
            let forbidden_zone = &state.forbidden_zones[p_index!(b) as usize];
            if forbidden_zone.2 == U4096::ZERO {
                ("Black (-)".to_string(), "".to_string())
            } else {
                (
                    format!("Black ({})", b.char),
                    format_board(forbidden_zone, Some('X'))
                )
            }
        } else {
            ("Black (-)".to_string(), "".to_string())
        };

        if w_label == "White (-)" && b_label == "Black (-)" {
            continue;
        }

        let w_lines: Vec<_> = w_board.lines().collect();
        let b_lines: Vec<_> = b_board.lines().collect();
        let max_lines = w_lines.len().max(b_lines.len());

        let board_width = state.files as usize * 4 + 4;

        result.push_str(&format!(
            "{:^width$}\n",
            format!("Forbidden Zones for the {}\n", piece.name),
            width = board_width * 2 + 8
        ));
        result.push_str(&format!(
            "   {:<label_width$}    {:<label_width$}\n",
            w_label, b_label, label_width = board_width + 2
        ));
        for i in 0..max_lines {
            let wl = w_lines.get(i).unwrap_or(&"");
            let bl = b_lines.get(i).unwrap_or(&"");
            result.push_str(&format!(
                "{:<label_width$}    {:<label_width$}\n",
                wl, bl, label_width = board_width + 2
            ));
        }
        result.push('\n');
    }
    result
}

fn format_special_rules(state: &State) -> String {
    if state.special_rules == 0 {
        "".to_string()
    } else {
        let mut rules = Vec::new();
        if castling!(state) { rules.push("Castling"); }
        if en_passant!(state) { rules.push("En Passant"); }
        if promotions!(state) { rules.push("Promotions"); }
        if drops!(state) { rules.push("Drops"); }
        if count_limits!(state) { rules.push("Count Limits"); }
        if forbidden_zones!(state) { rules.push("Forbidden Zones"); }
        if promote_to_captured!(state) { rules.push("Promote to Captured"); }
        rules.join(", ")
    }
}

pub fn format_entire_game(state: &State) -> String {
    let mut result = String::new();

    result.push_str(
        &format!("{} ({}x{})\n", state.title, state.files, state.ranks)
    );
    result.push_str(
        &format!("Special rules: {}\n\n", format_special_rules(state))
    );
    result.push_str(&format_piece_types(state));
    result.push_str(&format_promotion_zones(state));
    result.push_str(&format_forbidden_zones(state));
    result.push_str(&format_intial_setup(state));
    result.push_str(&format_game_state(state, true));

    result
}