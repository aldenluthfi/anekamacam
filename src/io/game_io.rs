//! # game_io.rs
//!
//! Implements game state parsing and formatting functions.
//!
//! This file contains functionality for reading game configuration files,
//! parsing FEN (Forsyth-Edwards Notation) strings, and formatting game state
//! for display. It handles piece definitions, board setup, and state
//! information such as castling rights, en passant squares, and configurable
//! halfmove-clock/repetition counters.
//! The module provides both compact and verbose output formats for game
//! visualization.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 25/01/2026
use crate::*;

lazy_static! {
    pub static ref CASTLING_PATTERN: Regex =
        Regex::new(r"^([KQkq]+)$|^-$").unwrap();
    pub static ref ENP_PATTERN: Regex =
        Regex::new(r"^([0-9a-fA-F]{3})([0-9a-fA-F]{3})(.)$|^\*$").unwrap();
    pub static ref HAND_PATTERN: Regex = Regex::new(r"^(.*)/(.*)$").unwrap();
    pub static ref IN_HAND_PATTERN: Regex = Regex::new(r"(\d*)(.)").unwrap();
}

fn determine_board_dimensions(fen: &str) -> (u8, u8) {
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

fn extract_fen_components(fen: &str) -> (bool, bool, bool) {
    let mut castling = false;
    let mut en_passant = false;
    let mut in_hand = false;

    for part in fen.split_whitespace().skip(2) {
        castling |= CASTLING_PATTERN.is_match(part);
        en_passant |= ENP_PATTERN.is_match(part);
        in_hand |= HAND_PATTERN.is_match(part);

        if castling && en_passant && in_hand {
            break;
        }
    }

    (castling, en_passant, in_hand)
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

fn collect_piece_type_pairs(state: &State) -> Vec<(usize, usize)> {
    let mut type_pairs = Vec::new();

    for (white_idx, piece) in state.statics.pieces.iter().enumerate() {
        if p_color!(piece) != WHITE {
            continue;
        }

        let black_idx = state.statics.piece_swap_map[white_idx] as usize;

        assert!(
            p_color!(state.statics.pieces[black_idx]) == BLACK,
            "Invalid black counterpart mapping for white piece index {}",
            white_idx
        );

        type_pairs.push((white_idx, black_idx));
    }

    assert!(!type_pairs.is_empty(), "No white piece representatives found");

    type_pairs
}

pub fn set_piece_dynamic_parameters(
    piece: &mut Piece,
    ovalue: u16,
    evalue: u16,
    is_big: bool,
    is_major: bool,
) {
    assert!(
        ovalue <= 0x3FFF,
        "Opening piece value out of 14-bit range: {}",
        ovalue
    );
    assert!(
        evalue <= 0x3FFF,
        "Endgame piece value out of 14-bit range: {}",
        evalue
    );

    let mut dynamic_bits = 0u32;

    if is_big {
        dynamic_bits |= 1;
    }

    if is_major {
        dynamic_bits |= 1 << 1;
    }

    dynamic_bits |= (ovalue as u32 & 0x3FFF) << 2;
    dynamic_bits |= (evalue as u32 & 0x3FFF) << 16;

    piece.encoded_dynamic =
        (piece.encoded_dynamic & !((1u32 << 30) - 1)) | dynamic_bits;
}


/// Parses tuned parameters from a flat space-separated file.
///
/// Token order:
///
/// 1. opening phase score, endgame phase score,
/// 2. opening values (piece-type count), endgame values (piece-type count),
/// 3. big flags, major flags,
/// 4. then white opening/middlegame PST rows (piece-type count × board_size),
/// 5. then white endgame PST rows (piece-type count × board_size),
/// 6. then futility margins (3 phases × 5 depths = 15 tokens).
///
/// Black PST rows are derived by mirroring white rows across the
/// horizontal axis.
pub fn parse_tuned_parameters_file(state: &mut State, path: &str) {
    let raw = fs::read_to_string(path).unwrap_or_else(|e| {
        panic!("Failed to read parameter file {}: {}", path, e)
    });

    let tokens: Vec<i32> = raw
        .split_whitespace()
        .map(|token| {
            token.parse::<i32>().unwrap_or_else(|_| {
                panic!("Invalid parameter value: {}", token)
            })
        })
        .collect();

    let piece_type_pairs = collect_piece_type_pairs(state);
    let piece_type_count = piece_type_pairs.len();
    let board_size = state.main_board.len();
    let expected_count =
        2 + piece_type_count * 4 + piece_type_count * board_size * 2 + 15;

    assert!(
        tokens.len() == expected_count,
        "Parameter count mismatch: got {}, expected {}",
        tokens.len(),
        expected_count
    );

    let mut cursor = 0usize;

    state.static_mut().opening_score = tokens[cursor]
        .unsigned_abs();
    cursor += 1;

    state.static_mut().endgame_score = tokens[cursor]
        .unsigned_abs();
    cursor += 1;

    let ovalues = &tokens[cursor..cursor + piece_type_count];
    cursor += piece_type_count;

    let evalues = &tokens[cursor..cursor + piece_type_count];
    cursor += piece_type_count;

    let big_flags = &tokens[cursor..cursor + piece_type_count];
    cursor += piece_type_count;

    let major_flags = &tokens[cursor..cursor + piece_type_count];
    cursor += piece_type_count;

    for piece_type_idx in 0..piece_type_count {
        let ovalue_i32 = ovalues[piece_type_idx];
        let evalue_i32 = evalues[piece_type_idx];
        let abs_ovalue = ovalue_i32.unsigned_abs();
        let abs_evalue = evalue_i32.unsigned_abs();

        assert!(
            abs_ovalue <= 0x3FFF,
            "Opening piece value out of range at index {}: {}",
            piece_type_idx,
            ovalue_i32
        );

        assert!(
            abs_evalue <= 0x3FFF,
            "Endgame piece value out of range at index {}: {}",
            piece_type_idx,
            evalue_i32
        );

        let big_flag = big_flags[piece_type_idx];
        assert!(
            big_flag == 0 || big_flag == 1,
            "Big flag must be 0 or 1 at index {}, got {}",
            piece_type_idx,
            big_flag
        );

        let major_flag = major_flags[piece_type_idx];
        assert!(
            major_flag == 0 || major_flag == 1,
            "Major flag must be 0 or 1 at index {}, got {}",
            piece_type_idx,
            major_flag
        );

        let white_pst_opening = &tokens[cursor..cursor + board_size];
        cursor += board_size;

        let white_pst_endgame = &tokens[cursor..cursor + board_size];
        cursor += board_size;

        let (white_idx, black_idx) = piece_type_pairs[piece_type_idx];

        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[white_idx],
            abs_ovalue as u16,
            abs_evalue as u16,
            big_flag == 1,
            major_flag == 1,
        );

        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[black_idx],
            abs_ovalue as u16,
            abs_evalue as u16,
            big_flag == 1,
            major_flag == 1,
        );

        state.static_mut().pst_opening[white_idx] = white_pst_opening.to_vec();
        state.static_mut().pst_opening[black_idx] =
            mirror_pst_across_horizontal_axis(
                white_pst_opening,
                state.statics.files as usize,
                state.statics.ranks as usize,
            );

        state.static_mut().pst_endgame[white_idx] = white_pst_endgame.to_vec();
        state.static_mut().pst_endgame[black_idx] =
            mirror_pst_across_horizontal_axis(
                white_pst_endgame,
                state.statics.files as usize,
                state.statics.ranks as usize,
            );
    }

    for phase in 0..3 {
        for depth in 0..5 {
            state.static_mut().futility_margin[phase][depth] = tokens[cursor];
            cursor += 1;
        }
    }

    state.big_pieces = [0; 2];
    state.major_pieces = [0; 2];
    state.minor_pieces = [0; 2];

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let count = state.piece_count[piece_idx];

        state.big_pieces[color] += count * (p_is_big!(piece) as u32);
        state.major_pieces[color] += count * (p_is_major!(piece) as u32);
        state.minor_pieces[color] += count * (p_is_minor!(piece) as u32);
    }

    refresh_eval_state(state);
}

fn find_last_epoch_in_dir(dir_path: &str) -> usize {
    let mut max_epoch = 0usize;
    if let Ok(entries) = fs::read_dir(dir_path) {
        for entry in entries.flatten() {
            if let Ok(file_name) = entry.file_name().into_string()
            && file_name.ends_with(".param") && file_name != "latest.param"
            && let Ok(num) = file_name[..file_name.len() - 6].parse::<usize>() {
                max_epoch = max_epoch.max(num);
            }
        }
    }
    max_epoch
}

/// Exports tuned parameters to `parameters/{variant}/latest.param`.
/// If latest.param already has content, writes to an incremented epoch
/// number instead.
pub fn export_tuned_parameters_file(
    state: &State,
    variant: &str,
) {
    assert!(!variant.trim().is_empty(), "Variant name cannot be empty");

    let piece_type_pairs = collect_piece_type_pairs(state);
    let mut output_tokens = Vec::new();

    output_tokens.push(state.statics.opening_score.to_string());
    output_tokens.push(state.statics.endgame_score.to_string());

    for (white_idx, _) in &piece_type_pairs {
        output_tokens.push(
            p_ovalue!(state.statics.pieces[*white_idx]).to_string()
        );
    }

    for (white_idx, _) in &piece_type_pairs {
        output_tokens.push(
            p_evalue!(state.statics.pieces[*white_idx]).to_string()
        );
    }

    for (white_idx, _) in &piece_type_pairs {
        output_tokens.push(
            (p_is_big!(&state.statics.pieces[*white_idx]) as u8).to_string()
        );
    }

    for (white_idx, _) in &piece_type_pairs {
        output_tokens.push(
            (p_is_major!(&state.statics.pieces[*white_idx])
                as u8).to_string()
        );
    }

    for (white_idx, _) in &piece_type_pairs {
        for value in &state.statics.pst_opening[*white_idx] {
            output_tokens.push(value.to_string());
        }
    }

    for (white_idx, _) in &piece_type_pairs {
        for value in &state.statics.pst_endgame[*white_idx] {
            output_tokens.push(value.to_string());
        }
    }

    for phase in 0..3 {
        for depth in 0..5 {
            output_tokens.push(
                state.statics.futility_margin[phase][depth].to_string()
            );
        }
    }

    let dir_path = format!("{}/{}", PARAM_DIR, variant);
    fs::create_dir_all(&dir_path).unwrap_or_else(|e| {
        panic!("Failed to create directory {}: {}", dir_path, e)
    });

    let file_path = format!("{}/latest.param", dir_path);

    if Path::new(&file_path).exists() {
        let last_epoch = find_last_epoch_in_dir(&dir_path);
        let new_path = format!("{}/{}.param", dir_path, last_epoch + 1);

        fs::rename(&file_path, &new_path).unwrap_or_else(|e| {
            panic!("Failed to rename parameter file {}: {}", new_path, e)
        });

        fs::write(&file_path, output_tokens.join(" ")).unwrap_or_else(|e| {
            panic!("Failed to write parameter file {}: {}", file_path, e)
        });
    } else {
        fs::write(&file_path, output_tokens.join(" ")).unwrap_or_else(|e| {
            panic!("Failed to write parameter file {}: {}", file_path, e)
        });
    }
}

/// Parses a game configuration file for previewing purposes, without fully
/// populating the `State` struct. Only parsing the:
///
/// - title
/// - initial position
/// - piece chars
///
/// Returns (title, board) to be shown in the TUI
pub fn parse_config_preview(path: &str) -> (String, String) {
    let file_str =
        fs::read_to_string(path).expect("Failed to read configuration file");

    let uncommented_str = COMMENT_PATTERN.replace_all(&file_str, "");
    let cleaned = uncommented_str
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");
    let section_titles = SECTION_PATTERN
        .captures_iter(&cleaned);
    let section_contents = SECTION_PATTERN
        .split(&cleaned)
        .filter(|content| !content.trim().is_empty());

    let mut sections = HashMap::new();

    for (title, content) in section_titles.zip(section_contents) {
        let section_name = title[1].trim().to_string();
        let section_body = content
            .lines()
            .map(str::to_string)
            .filter(|line| !line.trim().is_empty())
            .collect::<Vec<String>>();
        sections.insert(section_name, section_body);
    }

    let mandatory_sections = [
        "general",
        "piece order",
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

    let title = sections["general"][0].trim().to_string();
    let position = sections["general"][1].split_whitespace().next().unwrap();
    let pieces = sections["piece order"][0].chars().collect::<Vec<char>>();

    let mut piece_index = HashMap::new();
    for (index, char) in pieces.iter().enumerate() {
        piece_index.insert(char, index);
    }

    let (files, ranks) = determine_board_dimensions(position);

    let mut boards = vec![board!(files, ranks); piece_index.len()];

    let ranks_data: Vec<&str> = position.split('/').collect();
    assert!(
        ranks_data.len() == ranks as usize,
        "{}: FEN rank count ({}) doesn't match board ranks ({})",
        title,
        ranks_data.len(),
        ranks
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
            file_count == files,
            "FEN rank {} has {} files but expected {}",
            rank_idx,
            file_count,
            files
        );
    }

    let mut rank = ranks - 1;
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
                let piece_idx = piece_index.get(&c).unwrap_or_else(|| {
                    panic!("Unknown piece character in FEN: {}", c)
                });

                let square_index =
                    (rank as u32) * (files as u32) + (file as u32);

                set!(&mut boards[*piece_idx], square_index);

                file += 1;
            }
        }
    }

    let board_str = boards
        .iter()
        .enumerate()
        .map(
            | (index,  board) |
            format_board(board, Some(pieces[index]))
        )
        .fold(
            format_board(&board!(files, ranks), None),
            |acc, board| combine_board_strings(&acc, &board)
        );

    (title, board_str)
 }

/// Parses a game configuration file and initializes a game state.
/// See `example.conf` for the expected format of the configuration file.
///
/// Pieces are first parsed into a tuple of:
/// (string, char, Vec<u8>, u8, u8, bool, bool, bool, u16, u16, u8)
///
/// where the fields are:
/// [0] string: the name of the piece
/// [1] char: the character representing the piece on the board
/// [2] Vec<u8>: the vector of pieces this piece can promote to
/// [3] u8: the piece index (0-255, with 255 reserved for "no piece")
/// [4] u8: the piece color (0 for white, 1 for black)
/// [5] bool: whether the piece is royal
/// [6] u8: the piece rank
///
/// Which are then converted into `Piece` structs and stored in the `State`
/// struct.
pub fn parse_config_file(path: &str) -> State {
    let file_str =
        fs::read_to_string(path).expect("Failed to read configuration file");

    let uncommented_str = COMMENT_PATTERN.replace_all(&file_str, "");
    let cleaned = uncommented_str
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");
    let section_titles = SECTION_PATTERN
        .captures_iter(&cleaned);
    let section_contents = SECTION_PATTERN
        .split(&cleaned)
        .filter(|content| !content.trim().is_empty());

    let mut sections = HashMap::new();

    for (title, content) in section_titles.zip(section_contents) {
        let section_name = title[1].trim().to_string();
        let section_body = content
            .lines()
            .map(str::to_string)
            .filter(|line| !line.trim().is_empty())
            .collect::<Vec<String>>();
        sections.insert(section_name, section_body);
    }

    let mandatory_sections = [
        "general",
        "pieces",
        "piece order",
        "piece moves",
        "piece roles",
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

    /*-----------------------------------------------------------------------*\
                                 PARSE GENERAL SECTION
    \*-----------------------------------------------------------------------*/

    let title = sections["general"][0].trim();
    let initial_position = sections["general"][1].trim();
    let initial_board = initial_position.split_whitespace().next().unwrap();
    let (files, ranks) = determine_board_dimensions(initial_board);

    /*-----------------------------------------------------------------------*\
                                  PARSE RULES SECTION
    \*-----------------------------------------------------------------------*/

    let castling = sections["rules"].contains(&"castling".to_string());
    let en_passant = sections["rules"].contains(&"en passant".to_string());
    let promotions = sections["rules"].contains(&"promotions".to_string());
    let drops = sections["rules"].contains(&"drops".to_string());
    let piece_count_limits =
        sections["rules"].contains(&"piece count limits".to_string());
    let forbidden_zones =
        sections["rules"].contains(&"forbidden zones".to_string());
    let promote_to_captured =
        sections["rules"].contains(&"promote to captured".to_string());
    let demote_upon_capture =
        sections["rules"].contains(&"demote upon capture".to_string());
    let stalemate_loss =
        sections["rules"].contains(&"stalemate loss".to_string());
    let setup_phase = sections["rules"].contains(&"setup phase".to_string());
    let stand_offs = sections["rules"].contains(&"stand-offs".to_string());
    let halfmove_clock =
        sections["rules"].contains(&"halfmove clock".to_string());
    let repetition_limit =
        sections["rules"].contains(&"repetition limit".to_string());

    let (fen_castling, fen_en_passant, fen_in_hand) =
        extract_fen_components(initial_position);

    if castling {
        assert!(fen_castling, "No castling rights found in FEN");
    }

    if !castling {
        assert!(!fen_castling, "Castling rights found in FEN");
    }

    if en_passant {
        assert!(fen_en_passant, "No en passant square found in FEN");
    }

    if !en_passant {
        assert!(!fen_en_passant, "En passant square found in FEN");
    }

    if drops || promote_to_captured || demote_upon_capture || setup_phase {
        assert!(fen_in_hand, "No pieces in hand found in FEN");
    }

    if promotions {
        assert!(
            sections.contains_key("promotions"),
            "[promotions] section is missing"
        );
        assert!(
            sections.contains_key("mandatory promotion zones")
                || sections.contains_key("optional promotion zones"),
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

    if stand_offs {
        assert!(
            sections.contains_key("stand-off patterns"),
            "[stand-off patterns] section is missing"
        )
    }

    if halfmove_clock {
        assert!(
            sections.contains_key("halfmove clock"),
            "[halfmove clock] section is missing"
        )
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

    if stalemate_loss {
        enc_stalemate_loss!(special_rules);
    }

    if setup_phase {
        enc_setup_phase!(special_rules);
    }

    if stand_offs {
        enc_stand_offs!(special_rules);
    }

    if halfmove_clock {
        enc_halfmove_clock!(special_rules);
    }

    if repetition_limit {
        enc_repetition_limit!(special_rules);
    }

    /*-----------------------------------------------------------------------*\
                                      PARSE PIECES
    \*-----------------------------------------------------------------------*/

    let mut unordered_pieces = Vec::with_capacity(sections["pieces"].len());

    let mut pieces_moves;
    let mut pieces_drops;
    let mut pieces_setup;
    let mut pieces_stand_off;

    let mut char_to_unordered_index: HashMap<char, usize> = HashMap::new();
    let mut char_to_type_index: HashMap<char, usize> = HashMap::new();
    for bare_piece in &sections["pieces"] {
        let parts: Vec<&str> = bare_piece.split(':').map(str::trim).collect();

        assert!(parts.len() == 2, "Invalid piece definition: {}", bare_piece);

        let chars = parts[0];
        assert!(
            chars.chars().count() == 2,
            "Piece definition must have exactly 2 chars: {}",
            bare_piece
        );

        let white_char = chars.chars().next().unwrap();
        let black_char = chars.chars().nth(1).unwrap();
        let name = parts[1].to_string();

        let white_index = unordered_pieces.len();
        unordered_pieces.push((
            name.clone(),
            white_char,
            Vec::new(),
            0,
            WHITE,
            false,
            0,
        ));
        char_to_unordered_index.insert(white_char, white_index);
        let piece_type_index = white_index / 2;
        char_to_type_index.insert(white_char, piece_type_index);

        let black_index = unordered_pieces.len();
        unordered_pieces.push((
            name,
            black_char,
            Vec::new(),
            0,
            BLACK,
            false,
            0,
        ));
        char_to_unordered_index.insert(black_char, black_index);
        char_to_type_index.insert(black_char, piece_type_index);
    }

    let piece_order = sections["piece order"][0].trim();
    let piece_order_chars: Vec<char> = piece_order.chars().collect();

    assert!(
        piece_order_chars.len() == unordered_pieces.len(),
        "Piece order count ({}) doesn't match piece count ({})",
        piece_order_chars.len(),
        unordered_pieces.len()
    );

    let mut seen_order_chars = HashSet::new();
    for ch in &piece_order_chars {
        assert!(
            seen_order_chars.insert(*ch),
            "Duplicate piece in piece order: {}",
            ch
        );
        assert!(
            char_to_unordered_index.contains_key(ch),
            "Unknown piece in piece order: {}",
            ch
        );
    }

    let mut pieces = Vec::with_capacity(unordered_pieces.len());
    let mut piece_type_indices = Vec::with_capacity(unordered_pieces.len());
    let mut char_to_index: HashMap<char, usize> = HashMap::new();

    for (i, &piece_char) in piece_order_chars.iter().enumerate() {
        let old_index = *char_to_unordered_index.get(&piece_char).unwrap();
        let mut piece_data = unordered_pieces[old_index].clone();
        piece_data.3 = i as PieceIndex;
        pieces.push(piece_data);
        piece_type_indices.push(
            *char_to_type_index.get(&piece_char).unwrap_or_else(|| {
                panic!("Unknown piece in piece order: {}", piece_char)
            }),
        );
        char_to_index.insert(piece_char, i);
    }

    let piece_count = pieces.len();
    let piece_type_count = sections["pieces"].len();
    assert!(
        piece_type_count > 0 && piece_type_count <= piece_count,
        "Invalid piece type count ({}) for piece count ({})",
        piece_type_count,
        piece_count
    );

    let mut royal_flags = vec![false; piece_count];
    let piece_roles = &sections["piece roles"];

    for role_entry in piece_roles {
        let parts: Vec<&str> = role_entry.split(':').map(str::trim).collect();
        assert!(
            parts.len() == 2,
            "Invalid piece role definition: {}",
            role_entry
        );

        match parts[0] {
            "royal" => {
                for piece_char in parts[1].chars() {
                    if let Some(&piece_idx) = char_to_index.get(&piece_char) {
                        royal_flags[piece_idx] = true;
                    } else {
                        panic!(
                            "Unknown piece character in royal role: {}",
                            piece_char
                        );
                    }
                }
            }
            _ => panic!(
                concat!(
                    "Unsupported piece role: {}. ",
                    "Only 'royal' is allowed in [piece roles]"
                ),
                parts[0]
            ),
        }
    }

    for i in 0..piece_count {
        pieces[i].5 = royal_flags[i];
    }

    pieces_moves = vec![String::new(); pieces.len()];
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
                pieces_moves[white_index] = move_pattern.clone();
            } else {
                panic!("Unknown piece character: {}", white_char);
            }

            if let Some(&black_index) = char_to_index.get(&black_char) {
                pieces_moves[black_index] = move_pattern.clone();
            } else {
                panic!("Unknown piece character: {}", black_char);
            }
        } else if piece_chars.len() == 1 {
            let piece_char = piece_chars.chars().next().unwrap();

            if let Some(&index) = char_to_index.get(&piece_char) {
                pieces_moves[index] = move_pattern.clone();
            } else {
                panic!("Unknown piece character: {}", piece_char);
            }
        } else {
            panic!("Invalid piece character(s): {}", piece_chars);
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

                let white_index =
                    char_to_index.get(&white_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", white_char),
                    );

                let black_index =
                    char_to_index.get(&black_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", black_char),
                    );

                let promotions_str = parts[1];

                for promo_char in promotions_str.chars() {
                    if let Some(&promo_index) = char_to_index.get(&promo_char) {
                        pieces[white_index].2.push(promo_index as PieceIndex);
                        pieces[black_index].2.push(promo_index as PieceIndex);
                    } else {
                        panic!(
                            "Unknown promotion piece character: {}",
                            promo_char
                        );
                    }
                }
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                let piece_index =
                    char_to_index.get(&piece_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", piece_char),
                    );

                let promotions_str = parts[1];

                for promo_char in promotions_str.chars() {
                    if let Some(&promo_index) = char_to_index.get(&promo_char) {
                        pieces[piece_index].2.push(promo_index as PieceIndex);
                    } else {
                        panic!(
                            "Unknown promotion piece character: {}",
                            promo_char
                        );
                    }
                }
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

    if sections.contains_key("piece ranks") {
        for piece_rank in &sections["piece ranks"] {
            let parts: Vec<&str> =
                piece_rank.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid piece rank definition: {}",
                piece_rank
            );

            let rank_str = parts[0];
            let pieces_str = parts[1];

            let rank_value = rank_str.parse::<u8>().unwrap_or_else(|_| {
                panic!("Invalid piece rank: {}", rank_str.trim())
            });
            for piece_char in pieces_str.chars() {
                if let Some(&index) = char_to_index.get(&piece_char) {
                    pieces[index].6 = rank_value;
                } else {
                    panic!("Unknown piece character: {}", piece_char);
                }
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                                 POPULATE STATIC FIELDS
    \*-----------------------------------------------------------------------*/

    let mut result = State::new(
        title.to_string(),
        initial_position.to_string(),
        files,
        ranks,
        pieces
            .iter()
            .enumerate()
            .map(|(i, p)| {
                Piece::new(
                    p.0.clone(),
                    p.1,
                    p.2.clone(),
                    p.3,
                    p.4,
                    p.5,
                    pieces_moves[i].contains("o"),
                    pieces_moves[i].contains("O"),
                    p.6,
                )
            })
            .collect(),
        special_rules,
    );

    let template_bit_fen = initial_position.split_whitespace().next().unwrap();
    let bit_fens: Vec<String> = result.statics.pieces.iter().map(|piece| {
        template_bit_fen.chars().map(|c| {
            if c == piece.char { 'X' }
            else if c.is_ascii_alphabetic() { 'O' }
            else { c }
        }).collect::<String>()
    }).collect();
    for (index, bit_fen) in bit_fens.iter().enumerate() {
        let setup_board = parse_bit_fen(Some(bit_fen), &result);
        result.static_mut().initial_setup[index] = setup_board;
    }

    let swap_entries: Vec<(PieceIndex, PieceIndex)> = result.statics.pieces
        .iter()
        .enumerate()
        .filter_map(|(i, piece)| {
            result.statics.pieces.iter().position(|p| {
                p.name == piece.name && p_color!(p) != p_color!(piece)
            }).map(|other_idx| (i as PieceIndex, other_idx as PieceIndex))
        })
        .collect();

    for (i, j) in swap_entries {
        result.static_mut().piece_swap_map[i as usize] = j;
    }

    let pieces_promos: Vec<(PieceIndex, Vec<u8>)> = result.statics.pieces
        .iter()
        .map(|p| (p_index!(p), p.promotions.clone()))
        .collect();
    for (pi, promotions) in &pieces_promos {
        for &promotion_index in promotions {
            result.static_mut().piece_demotion_map[promotion_index as usize]
                .push(*pi);
        }
    }

    for piece in &result.statics.pieces.clone() {
        let pi = p_index!(piece) as usize;

        if result.statics.piece_demotion_map[pi].is_empty() {
            result.static_mut().piece_demotion_map[pi].push(pi as PieceIndex);
        }
    }

    if castling {
        assert!(
            result.statics
                .pieces
                .iter()
                .any(|p| p_castle_left!(p) || p_castle_right!(p)),
            "No castling rights found in piece definitions"
        );
    }

    if en_passant {
        assert!(
            pieces_moves.iter().any(|mv| mv.contains('p') || mv.contains('t')),
            "No en passant movement found in piece definitions"
        );
    }

    if !castling {
        assert!(
            result.statics
                .pieces
                .iter()
                .all(|p| !p_castle_left!(p) && !p_castle_right!(p)),
            "Castling rights found in piece definitions"
        );
    }

    if !en_passant {
        assert!(
            pieces_moves
                .iter()
                .all(|mv| !mv.contains('p') || !mv.contains('t')),
            "En passant movement found in piece definitions"
        );
    }

    if repetition_limit {
        assert!(
            sections.contains_key("repetition limit"),
            "[repetition limit] section is missing"
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
                        .unwrap_or_else(|| {
                            panic!("Unknown piece character: {}", white_char)
                        });

                    let black_index = char_to_index
                        .get(&black_char)
                        .copied()
                        .unwrap_or_else(|| {
                            panic!("Unknown piece character: {}", black_char)
                        });

                    let zone_str = parts[1];

                    result.static_mut().promotion_zones_mandatory[white_index] =
                        parse_bit_fen(Some(zone_str), &result);

                    result.static_mut().promotion_zones_mandatory[black_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else if piece_chars.len() == 1 {
                    let piece_char = piece_chars.chars().next().unwrap();

                    let piece_index = char_to_index
                        .get(&piece_char)
                        .copied()
                        .unwrap_or_else(|| {
                            panic!("Unknown piece character: {}", piece_char)
                        });

                    let zone_str = parts[1];

                    result.static_mut().promotion_zones_mandatory[piece_index] =
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
                        .unwrap_or_else(|| {
                            panic!("Unknown piece character: {}", white_char)
                        });

                    let black_index = char_to_index
                        .get(&black_char)
                        .copied()
                        .unwrap_or_else(|| {
                            panic!("Unknown piece character: {}", black_char)
                        });

                    let zone_str = parts[1];

                    result.static_mut().promotion_zones_optional[white_index] =
                        parse_bit_fen(Some(zone_str), &result);

                    result.static_mut().promotion_zones_optional[black_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else if piece_chars.len() == 1 {
                    let piece_char = piece_chars.chars().next().unwrap();

                    let piece_index = char_to_index
                        .get(&piece_char)
                        .copied()
                        .unwrap_or_else(|| {
                            panic!("Unknown piece character: {}", piece_char)
                        });

                    let zone_str = parts[1];

                    result.static_mut().promotion_zones_optional[piece_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else {
                    panic!("Invalid piece character(s): {}", piece_chars);
                }
            }
        }
    }

    pieces_drops = vec![
        DEFAULT_DROP.to_string(); result.static_mut().pieces.len()
    ];
    if drops && sections.contains_key("drop rules") {
        for drop in &sections["drop rules"] {
            let parts: Vec<&str> = drop.split(':').map(str::trim).collect();

            assert!(parts.len() == 2, "Invalid drop definition: {}", drop);

            let piece_chars = parts[0];
            let drop_pattern = parts[1].to_string();

            if piece_chars.len() == 2 {
                let white_char = piece_chars.chars().next().unwrap();
                let black_char = piece_chars.chars().nth(1).unwrap();

                if let Some(&white_index) = char_to_index.get(&white_char) {
                    pieces_drops[white_index] = drop_pattern.clone();
                } else {
                    panic!("Unknown piece character: {}", white_char);
                }

                if let Some(&black_index) = char_to_index.get(&black_char) {
                    pieces_drops[black_index] = drop_pattern.clone();
                } else {
                    panic!("Unknown piece character: {}", black_char);
                }
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                if let Some(&index) = char_to_index.get(&piece_char) {
                    pieces_drops[index] = drop_pattern.clone();
                } else {
                    panic!("Unknown piece character: {}", piece_char);
                }
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
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

                let white_index =
                    char_to_index.get(&white_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", white_char),
                    );

                let black_index =
                    char_to_index.get(&black_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", black_char),
                    );

                let limit_str = parts[1];

                let limit_value =
                    limit_str.parse::<u32>().unwrap_or_else(|_| {
                        panic!(
                            "Invalid piece count limit: {}",
                            limit_str.trim()
                        )
                    });

                result.static_mut().piece_limit[white_index] = limit_value;
                result.static_mut().piece_limit[black_index] = limit_value;
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                let piece_index =
                    char_to_index.get(&piece_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", piece_char),
                    );

                let limit_str = parts[1];

                let limit_value =
                    limit_str.parse::<u32>().unwrap_or_else(|_| {
                        panic!(
                            "Invalid piece count limit: {}",
                            limit_str.trim()
                        )
                    });

                result.static_mut().piece_limit[piece_index] = limit_value;
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

                let white_index =
                    char_to_index.get(&white_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", white_char),
                    );

                let black_index =
                    char_to_index.get(&black_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", black_char),
                    );

                let zone_str = parts[1];

                result.static_mut().forbidden_zones[white_index] =
                    parse_bit_fen(Some(zone_str), &result);

                result.static_mut().forbidden_zones[black_index] =
                    parse_bit_fen(Some(zone_str), &result);
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                let piece_index =
                    char_to_index.get(&piece_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", piece_char),
                    );

                let zone_str = parts[1];

                result.static_mut().forbidden_zones[piece_index] =
                    parse_bit_fen(Some(zone_str), &result);
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

    pieces_setup = vec![
        DEFAULT_DROP.to_string(); result.static_mut().pieces.len()
    ];
    if setup_phase && sections.contains_key("setup rules") {
        for setup in &sections["setup rules"] {
            let parts: Vec<&str> = setup.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid setup phase definition: {}",
                setup
            );

            let piece_chars = parts[0];
            let setup_pattern = parts[1].to_string();

            if piece_chars.len() == 2 {
                let white_char = piece_chars.chars().next().unwrap();
                let black_char = piece_chars.chars().nth(1).unwrap();

                if let Some(&white_index) = char_to_index.get(&white_char) {
                    pieces_setup[white_index] = setup_pattern.clone();
                } else {
                    panic!("Unknown piece character: {}", white_char);
                }

                if let Some(&black_index) = char_to_index.get(&black_char) {
                    pieces_setup[black_index] = setup_pattern.clone();
                } else {
                    panic!("Unknown piece character: {}", black_char);
                }
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                if let Some(&index) = char_to_index.get(&piece_char) {
                    pieces_setup[index] = setup_pattern.clone();
                } else {
                    panic!("Unknown piece character: {}", piece_char);
                }
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

    pieces_stand_off = vec![String::new(); result.static_mut().pieces.len()];
    if stand_offs {
        for pattern in &sections["stand-off patterns"] {
            let parts: Vec<&str> = pattern.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid stand-off pattern definition: {}",
                pattern
            );

            let piece_chars = parts[0];
            let stand_off_patterns = parts[1].to_string();

            if piece_chars.len() == 2 {
                let white_char = piece_chars.chars().next().unwrap();
                let black_char = piece_chars.chars().nth(1).unwrap();

                if let Some(&white_index) = char_to_index.get(&white_char) {
                    pieces_stand_off[white_index] = stand_off_patterns.clone();
                } else {
                    panic!("Unknown piece character: {}", white_char);
                }

                if let Some(&black_index) = char_to_index.get(&black_char) {
                    pieces_stand_off[black_index] = stand_off_patterns.clone();
                } else {
                    panic!("Unknown piece character: {}", black_char);
                }
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                if let Some(&index) = char_to_index.get(&piece_char) {
                    pieces_stand_off[index] = stand_off_patterns.clone();
                } else {
                    panic!("Unknown piece character: {}", piece_char);
                }
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

    if halfmove_clock {
        let mut parsed_limit: Option<u8> = None;
        let mut parsed_pieces: Option<Vec<bool>> = None;

        for entry in &sections["halfmove clock"] {
            let parts: Vec<&str> = entry.split(':').map(str::trim).collect();
            assert!(
                parts.len() == 2,
                "Invalid halfmove clock definition: {}",
                entry
            );

            match parts[0] {
                "limit" => {
                    let limit_value =
                        parts[1].parse::<u8>().unwrap_or_else(|_| {
                        panic!("Invalid halfmove limit: {}", parts[1].trim())
                    });
                    parsed_limit = Some(limit_value);
                }
                "pieces" => {
                    let mut reset_mask = vec![
                        false; result.static_mut().pieces.len()
                    ];
                    for piece_char in parts[1].chars() {
                        let piece_index = char_to_index
                            .get(&piece_char)
                            .copied()
                            .unwrap_or_else(|| {
                                panic!(
                                    concat!(
                                        "Unknown piece character in ",
                                        "halfmove clock pieces: {}"
                                    ),
                                    piece_char
                                )
                            });
                        reset_mask[piece_index] = true;
                    }
                    parsed_pieces = Some(reset_mask);
                }
                _ => panic!(
                    "Unknown halfmove clock field: {}",
                    parts[0]
                ),
            }
        }

        result.static_mut().halfmove_limit = parsed_limit
            .expect("Halfmove clock limit is missing");
        result.static_mut().halfmove_pieces = parsed_pieces
            .expect("Halfmove clock pieces are missing");
    }

    if repetition_limit {
        let limit_parts: Vec<&str> =
            sections["repetition limit"][0].split(':').collect();
        let limit_str =
            limit_parts.get(1).expect("Invalid repetition limit format");
        let limit_value = limit_str.parse::<u8>().unwrap_or_else(|_| {
            panic!("Invalid repetition limit: {}", limit_str.trim())
        });
        result.static_mut().repetition_limit = limit_value;
    }

    result.precompute(
        pieces_moves,
        pieces_drops,
        pieces_setup,
        pieces_stand_off,
    );
    result.load_fen(initial_position, None);

    let variant = Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or_default();
    let param_file = format!("{}/{}/latest.param", PARAM_DIR, variant);

    if Path::new(&param_file).is_file() {
        log_3!("Loading parameters from file");
        parse_tuned_parameters_file(&mut result, &param_file);
    } else {
        derive_parameters(&mut result);
        export_tuned_parameters_file(&result, variant);
    }

    hash_position(&result);

    result
}

fn parse_bit_fen(fen: Option<&str>, state: &State) -> Board {
    if fen.is_none() {
        return board!(state.statics.files, state.statics.ranks);
    }

    let fen = fen.unwrap();

    let ranks_data: Vec<&str> = fen.split('/').collect();
    assert!(
        ranks_data.len() == state.statics.ranks as usize,
        "FEN rank count ({}) doesn't match board ranks ({}) for fen {}",
        ranks_data.len(),
        state.statics.ranks,
        fen
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
            file_count == state.statics.files,
            "FEN rank {} has {} files but expected {}",
            rank_idx,
            file_count,
            state.statics.files
        );
    }

    let mut result = board!(state.statics.files, state.statics.ranks);

    let mut rank = state.statics.ranks - 1;
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
                    (rank as u32) * (state.statics.files as u32)
                + (file as u32)
                );
                file += 1;
            }
            _ => {
                set!(
                    result,
                    (rank as u32) * (state.statics.files as u32)
                + (file as u32)
                );
                file += 1;
            }
        }
    }

    result
}

/// Parses a FEN string and updates the game state accordingly. Applies
/// protocol translation if needed.
///
/// Cheesy Forsyth-Edwards Notation (CFEN)
///
/// The dimensions of the board are determined by the number of ranks and files
/// in this section.
///
/// The format is similar to FEN depending on the ruleset there is 3-5 parts,
/// the order is as follows:
///
/// (position) (side) (castling rights) (en passant square) (in hand pieces)
///
/// 1. position           : Same as normal FEN
/// 2. side               : Same as normal FEN
/// 3. castling rights    : Same as normal FEN
/// 4. en passant square  : Formatted 'ssseeez'*
/// 5. in hand pieces     : Formatted '(w)/(b)'**
///
/// Optional:
///
/// 6. Halfmove clock     : number of halfmoves towards the halfmove-clock rule
/// 7. Fullmove number    : starting at 1 and incremented after
///
/// *: s is the en passant square index itself in hex, e is the square index
///   of the piece that can be captured en passant in hex, and z is the char
///   of the piece that can be captured en passant. "*" if no en passant square.
///
/// **: where each w and b is formatted with the pieces in hand. e.g. P2N means
///   a pawn and two knights in hand. "-" if no pieces in hand for that color
///   so an empty hand for both is -/-.
///
pub fn parse_fen(state: &mut State, fen: &str, dict: Option<&Translator>) {
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

    let mut translated = fen.to_string();
    if let Some(translator) = dict {
        for (k, v) in &translator.inverse_fen {
            translated = k.replace_all(&translated, v).into_owned();
        }
    }
    let fen = &translated;

    let parts: Vec<&str> = fen.split_whitespace().collect();
    assert!(
        parts.len() >= needed_parts,
        "FEN must have at least {needed_parts} parts"
    );
    let mut part_index = 0;

    let position = parts[part_index];
    part_index += 1;

    let ranks_data: Vec<&str> = position.split('/').collect();
    assert!(
        ranks_data.len() == state.statics.ranks as usize,
        "FEN rank count ({}) doesn't match board ranks ({})",
        ranks_data.len(),
        state.statics.ranks
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
            file_count == state.statics.files,
            "FEN rank {} has {} files but expected {}",
            rank_idx,
            file_count,
            state.statics.files
        );
    }

    let mut rank = state.statics.ranks - 1;
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
                let piece_idx =
                    *state.statics.piece_char_map
                    .get(&c).unwrap_or_else(|| {
                        panic!("Unknown piece character: {}", c)
                    }) as usize;

                let mut piece = &state.statics.pieces[piece_idx];
                let mut piece_index = p_index!(piece);
                let mut piece_color = p_color!(piece);
                let square_index =
                    (rank as u32) * (state.statics.files as u32)
                + (file as u32);

                if promotions!(state)
                    && piece.promotions.len() == 1
                    && get!(
                        state.statics
                            .promotion_zones_mandatory[piece_index as usize],
                        square_index
                    )
                {
                    piece = &state.statics.pieces[
                        piece.promotions[0] as usize
                    ];
                    piece_index = p_index!(piece);
                    piece_color = p_color!(piece);
                }

                state.main_board[square_index as usize] = piece_index;

                state.piece_list[piece_index as usize]
                    .insert(square_index as Square);
                state.piece_count[piece_index as usize] += 1;

                set!(state.pieces_board[piece_color as usize], square_index);



                if p_is_royal!(piece) {
                    state.royal_list[piece_color as usize]
                        .push(square_index as Square);
                    state.royal_pieces[piece_color as usize] += 1;
                }

                if p_is_major!(piece) {
                    state.major_pieces[piece_color as usize] += 1;
                }

                if p_is_minor!(piece) {
                    state.minor_pieces[piece_color as usize] += 1;
                }

                if p_is_big!(piece) {
                    state.big_pieces[piece_color as usize] += 1;
                }

                if get!(
                    state.statics.initial_setup[piece_index as usize],
                    square_index
                )
                {
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
                .unwrap_or_else(|_| panic!("Invalid en passant square: {}", s));
            let piece_square_index =
                u32::from_str_radix(e, 16).unwrap_or_else(|_| {
                    panic!("Invalid en passant captured square: {}", e)
                });
            let piece_char = z.chars().next().unwrap();
            let piece_index = *state.statics
                .piece_char_map
                .get(&piece_char)
                .unwrap_or_else(|| panic!("Unknown piece character: {}", z))
                as u32;

            square_index | (piece_square_index << 12) | piece_index << 24
        };
    }

    if drops!(state)
        || promote_to_captured!(state)
        || demote_upon_capture!(state)
        || setup_phase!(state)
    {
        let hands = parts[part_index];
        part_index += 1;

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
                let count_str = m.get(1).unwrap().as_str();
                let piece_char =
                    m.get(2).unwrap().as_str().chars().next().unwrap();

                let count = if count_str.is_empty() {
                    1
                } else {
                    count_str.parse::<u16>().unwrap_or_else(|_| {
                        panic!("Invalid piece count: {}", count_str.trim())
                    })
                };

                let piece_index = *state.statics
                    .piece_char_map
                    .get(&piece_char)
                    .unwrap_or_else(|| {
                        panic!(
                            "Unknown piece character in hand: {}",
                            piece_char
                        )
                    }) as usize;

                state.piece_in_hand[color_idx][piece_index] = count;
            }
        }
    }

    if setup_phase!(state)
    && (state.royal_list[0].is_empty() || state.royal_list[1].is_empty()) {
        state.game_phase = SETUP;
    }

    if parts.len() > part_index {
        state.halfmove_clock = parts[part_index].parse().unwrap_or_else(|_| {
            panic!("Invalid halfmove clock: {}", parts[part_index].trim())
        });
        part_index += 1;
    }

    if parts.len() > part_index {
        state.ply_counter = match parts[part_index].trim().parse::<u32>() {
            Ok(ply_num) => (ply_num - 1) * 2 + (state.playing as u32),
            Err(_) => panic!("Invalid ply count: {}", parts[part_index].trim()),
        };
    }

    refresh_eval_state(state);

    state.position_hash = hash_position(state);
}

/// Combines two board string representations by overlaying
/// non-whitespace characters from the first board onto the second board.
///
/// This function is used to merge multiple piece-specific board visualizations
/// into a single composite board display. It performs a character-by-character
/// merge rules:
/// - If both characters are identical, use that character
/// - If the first board has whitespace, use the character from the second board
/// - Otherwise, use the character from the first board
///
/// # Arguments
///
/// - `board1`  : The first board string
/// - `board2`  : The second board string
///
/// # Returns
///
/// A combined board string where pieces from both boards are merged together
/// while preserving the ASCII art borders and structure.
///
/// # Examples
///
/// Before: two separate board strings (white pieces and black pieces)
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
/// After: combined result
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
pub fn combine_board_strings(board1: &str, board2: &str) -> String {
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

pub fn format_game_state(state: &State) -> String {
    let board_size = state.main_board.len();
    let piece_count = state.statics.pieces.len();

    let mut all_boards = vec![
        board!(state.statics.files, state.statics.ranks);
        piece_count
    ];

    for square in 0..board_size {
        let piece_idx = state.main_board[square];
        if piece_idx != NO_PIECE {
            set!(all_boards[piece_idx as usize], square as u32);
        }
    }

    all_boards
        .iter()
        .enumerate()
        .map(|(i, b)| format_board(b, Some(state.statics.pieces[i].char)))
        .reduce(|board_str, next_board| {
            combine_board_strings(&board_str, &next_board)
        })
        .expect("Failed to format combined board string")
}

pub fn format_fen(state: &State, dict: Option<&Translator>) -> String {
    let mut fen = String::new();

    for rank in (0..state.statics.ranks).rev() {
        let mut empty_count = 0;

        for file in 0..state.statics.files {
            let square_index =
                (rank as u32) * (state.statics.files as u32)
                + (file as u32);
            let piece_idx = state.main_board[square_index as usize];
            if piece_idx == NO_PIECE {
                empty_count += 1;
            } else {
                if empty_count > 0 {
                    fen.push_str(&empty_count.to_string());
                    empty_count = 0;
                }
                fen.push(state.statics.pieces[piece_idx as usize].char);
            }
        }

        if empty_count > 0 {
            fen.push_str(&empty_count.to_string());
        }

        if rank > 0 {
            fen.push('/');
        }
    }

    if state.playing == WHITE {
        fen.push_str(" w");
    } else {
        fen.push_str(" b");
    }

    if castling!(state) {
        fen.push(' ');
        fen.push_str(&format_castling_rights(state));
    }

    if en_passant!(state) {
        fen.push(' ');

        let mut out = Vec::new();
        let mut scratch = Vec::new();

        generate_all_captures(state, &mut out, &mut scratch);

        if out
            .iter()
            .find(
                |m|
                captured_square!(m) as u32 ==
                enp_captured!(state.en_passant_square) &&
                captured_piece!(m) as u32 ==
                enp_piece!(state.en_passant_square) &&
                end!(m) as u32 ==
                enp_square!(state.en_passant_square) ||
                m.1.iter().any(|&cap|
                    multi_move_captured_square!(cap) as u32 ==
                    enp_captured!(state.en_passant_square) &&
                    multi_move_captured_piece!(cap) as u32 ==
                    enp_piece!(state.en_passant_square)
                ) &&
                end!(m) as u32 ==
                enp_square!(state.en_passant_square)
        ).is_some() {
            fen.push_str(&format_en_passant_square(state));
        } else {
            fen.push('*');
        }
            }

    if drops!(state)
    || promote_to_captured!(state)
    || demote_upon_capture!(state)
    || setup_phase!(state)
    {
        fen.push(' ');
        fen.push_str(&format_hand(state, WHITE));
        fen.push('/');
        fen.push_str(&format_hand(state, BLACK));
    }

    if halfmove_clock!(state) {
        fen.push(' ');
        fen.push_str(&state.halfmove_clock.to_string());
    }

    fen.push(' ');
    fen.push_str(&(state.ply_counter / 2 + 1).to_string());

    if let Some(translator) = dict {
        for (k, v) in &translator.fen {
            fen = k.replace_all(&fen, v).into_owned();
        }
    }

    fen
}

pub fn format_castling_rights(state: &State) -> String {
    let mut rights = String::new();

    if state.castling_state & WK_CASTLE != 0 {
        rights.push('K');
    }
    if state.castling_state & WQ_CASTLE != 0 {
        rights.push('Q');
    }
    if state.castling_state & BK_CASTLE != 0 {
        rights.push('k');
    }
    if state.castling_state & BQ_CASTLE != 0 {
        rights.push('q');
    }

    if rights.is_empty() {
        "-".to_string()
    } else {
        rights
    }
}

pub fn format_en_passant_square(state: &State) -> String {
    if state.en_passant_square == NO_EN_PASSANT {
        "*".to_string()
    } else {
        let en_passant_sq = enp_square!(state.en_passant_square);
        let en_passant_piece_idx = enp_piece!(state.en_passant_square);
        let en_passant_piece_sq = enp_captured!(state.en_passant_square);

        let en_passant_piece_char =
            state.statics.pieces[en_passant_piece_idx as usize].char;

        format!(
            "{:03x}{:03x}{}",
            en_passant_sq, en_passant_piece_sq, en_passant_piece_char
        )
    }
}

pub fn format_hand(state: &State, color: u8) -> String {
    let pieces_in_hand = &state.piece_in_hand[color as usize];
    let mut hand = String::new();

    for (i, piece) in state.statics.pieces.iter().enumerate() {
        let count = pieces_in_hand[i];
        if count == 1 {
            hand.push(piece.char);
        } else if count > 1 {
            hand.push_str(&format!("{}{}", count, piece.char));
        }
    }

    if hand.is_empty() {
        "-".to_string()
    } else {
        hand
    }
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
        result.push_str(&format!("{:02} ║", rank));
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
                (b'A' + file) as char,
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

pub fn format_position_hash(state: &State) -> String {
    format!("{:>016X}", state.position_hash)
}

pub fn format_game_phase(state: &State) -> String {
    if state.game_over {
        "Game Over".to_string()
    } else if state.game_phase == SETUP {
        "Setup Phase".to_string()
    } else if state.game_phase == OPENING {
        "Opening".to_string()
    } else if state.game_phase == MIDDLEGAME {
        "Middlegame".to_string()
    } else if state.game_phase == ENDGAME {
        "Endgame".to_string()
    } else {
        panic!("Unknown game phase: {}", state.game_phase);
    }
}

pub fn format_special_rules(state: &State) -> String {
    if state.statics.special_rules == 0 {
        return "-".to_string();
    }

    let mut rules = Vec::new();

    if castling!(state) {
        rules.push("Castling");
    }
    if en_passant!(state) {
        rules.push("En Passant");
    }
    if promotions!(state) {
        rules.push("Promotions");
    }
    if drops!(state) {
        rules.push("Drops");
    }
    if count_limits!(state) {
        rules.push("Count Limits");
    }
    if forbidden_zones!(state) {
        rules.push("Forbidden Zones");
    }
    if promote_to_captured!(state) {
        rules.push("Promote to Captured");
    }
    if demote_upon_capture!(state) {
        rules.push("Demote Upon Capture");
    }
    if stalemate_loss!(state) {
        rules.push("Stalemate Loss");
    }
    if setup_phase!(state) {
        rules.push("Setup Phase");
    }
    if stand_offs!(state) {
        rules.push("Stand-Offs");
    }
    if halfmove_clock!(state) {
        rules.push("Halfmove Clock");
    }
    if repetition_limit!(state) {
        rules.push("Repetition Limit");
    }

    rules.join(", ")
}
