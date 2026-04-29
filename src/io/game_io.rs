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
    pub static ref SECTION_PATTERN: Regex =
        Regex::new(r"= (.+) =[^=]+").unwrap();
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

fn parse_numeric_parameter_list(line: &str, label: &str) -> Vec<i32> {
    let trimmed = line.trim();
    assert!(
        trimmed.starts_with('[') && trimmed.ends_with(']'),
        "Invalid {} list format: {}",
        label,
        line
    );

    let inner = &trimmed[1..trimmed.len() - 1];
    if inner.trim().is_empty() {
        return Vec::new();
    }

    inner
        .split(',')
        .map(|value| {
            value.trim().parse::<i32>().unwrap_or_else(|_| {
                panic!("Invalid {} entry: {}", label, value.trim())
            })
        })
        .collect()
}

fn parse_binary_parameter_flags(
    line: &str,
    expected_len: usize,
    label: &str,
) -> Vec<bool> {
    let parsed = parse_numeric_parameter_list(line, label);

    assert!(
        parsed.len() == expected_len,
        "{} count ({}) doesn't match expected count ({})",
        label,
        parsed.len(),
        expected_len
    );

    parsed
        .iter()
        .map(|&value| {
            assert!(
                value == 0 || value == 1,
                "{} entries must be 0 or 1, got {}",
                label,
                value
            );
            value == 1
        })
        .collect()
}

fn mirror_pst_across_horizontal_axis(
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

    for (white_idx, piece) in state.pieces.iter().enumerate() {
        if p_color!(piece) != WHITE {
            continue;
        }

        let black_idx = state
            .piece_swap_map
            .get(&(white_idx as u8))
            .copied()
            .unwrap_or_else(|| {
                panic!(
                    "Missing black counterpart for white piece index {} ({})",
                    white_idx, piece.char
                )
            }) as usize;

        assert!(
            p_color!(state.pieces[black_idx]) == BLACK,
            "Invalid black counterpart mapping for white piece index {}",
            white_idx
        );

        type_pairs.push((white_idx, black_idx));
    }

    assert!(!type_pairs.is_empty(), "No white piece representatives found");

    type_pairs
}

fn set_piece_dynamic_parameters(
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
/// opening phase score, endgame phase score,
/// opening values (piece-type count), endgame values (piece-type count),
/// big flags, major flags,
/// then white opening/middlegame PST rows (piece-type count × board_size),
/// then white endgame PST rows (piece-type count × board_size).
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
    let board_size = (state.files as usize) * (state.ranks as usize);
    let expected_count =
        2 + piece_type_count * 4 + piece_type_count * board_size * 2;

    assert!(
        tokens.len() == expected_count,
        "Parameter count mismatch: got {}, expected {}",
        tokens.len(),
        expected_count
    );

    let mut cursor = 0usize;

    state.opening_score = tokens[cursor]
        .unsigned_abs();
    cursor += 1;

    state.endgame_score = tokens[cursor]
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
            &mut state.pieces[white_idx],
            abs_ovalue as u16,
            abs_evalue as u16,
            big_flag == 1,
            major_flag == 1,
        );

        set_piece_dynamic_parameters(
            &mut state.pieces[black_idx],
            abs_ovalue as u16,
            abs_evalue as u16,
            big_flag == 1,
            major_flag == 1,
        );

        state.pst_opening[white_idx] = white_pst_opening.to_vec();
        state.pst_opening[black_idx] = mirror_pst_across_horizontal_axis(
            white_pst_opening,
            state.files as usize,
            state.ranks as usize,
        );

        state.pst_endgame[white_idx] = white_pst_endgame.to_vec();
        state.pst_endgame[black_idx] = mirror_pst_across_horizontal_axis(
            white_pst_endgame,
            state.files as usize,
            state.ranks as usize,
        );
    }

    state.big_pieces = [0; 2];
    state.major_pieces = [0; 2];
    state.minor_pieces = [0; 2];

    for (piece_idx, piece) in state.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let count = state.piece_count[piece_idx];

        state.big_pieces[color] += count * (p_is_big!(piece) as u32);
        state.major_pieces[color] += count * (p_is_major!(piece) as u32);
        state.minor_pieces[color] += count * (p_is_minor!(piece) as u32);
    }

    refresh_eval_state(state);
}

/// Exports tuned parameters to `parameters/{variant}/{epoch}.param`
/// in a flat space-separated format compatible with
/// `parse_tuned_parameters_file`.
pub fn export_tuned_parameters_file(
    state: &State,
    variant: &str,
    epoch: usize,
) {
    assert!(!variant.trim().is_empty(), "Variant name cannot be empty");

    let piece_type_pairs = collect_piece_type_pairs(state);
    let mut output_tokens = Vec::new();

    output_tokens.push(state.opening_score.to_string());
    output_tokens.push(state.endgame_score.to_string());

    for (white_idx, _) in &piece_type_pairs {
        output_tokens.push(p_ovalue!(state.pieces[*white_idx]).to_string());
    }

    for (white_idx, _) in &piece_type_pairs {
        output_tokens.push(p_evalue!(state.pieces[*white_idx]).to_string());
    }

    for (white_idx, _) in &piece_type_pairs {
        output_tokens
            .push((p_is_big!(&state.pieces[*white_idx]) as u8).to_string());
    }

    for (white_idx, _) in &piece_type_pairs {
        output_tokens.push(
            (p_is_major!(&state.pieces[*white_idx]) as u8).to_string(),
        );
    }

    for (white_idx, _) in &piece_type_pairs {
        for value in &state.pst_opening[*white_idx] {
            output_tokens.push(value.to_string());
        }
    }

    for (white_idx, _) in &piece_type_pairs {
        for value in &state.pst_endgame[*white_idx] {
            output_tokens.push(value.to_string());
        }
    }

    let dir_path = format!("parameters/{}", variant);
    fs::create_dir_all(&dir_path).unwrap_or_else(|e| {
        panic!("Failed to create directory {}: {}", dir_path, e)
    });

    let file_path = format!("{}/{}.param", dir_path, epoch);
    fs::write(&file_path, output_tokens.join(" ")).unwrap_or_else(|e| {
        panic!("Failed to write parameter file {}: {}", file_path, e)
    });
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
/// [6] bool: whether the piece is big
/// [7] bool: whether the piece is major
/// [8] u16: the opening value of the piece
/// [9] u16: the endgame value of the piece
/// [10] u8: the piece rank
///
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
    let sections = SECTION_PATTERN
        .captures_iter(&cleaned)
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
        "general",
        "pieces",
        "piece order",
        "piece moves",
        "evaluation parameters",
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
    let mut pst_opening;
    let mut pst_endgame;

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
            false,
            false,
            0,
            0,
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
            false,
            false,
            0,
            0,
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
        piece_data.3 = i as u8;
        pieces.push(piece_data);
        piece_type_indices.push(
            *char_to_type_index.get(&piece_char).unwrap_or_else(|| {
                panic!("Unknown piece in piece order: {}", piece_char)
            }),
        );
        char_to_index.insert(piece_char, i);
    }

    let eval_parameters = &sections["evaluation parameters"];
    let piece_count = pieces.len();
    let board_size = (files as usize) * (ranks as usize);

    let piece_type_count = sections["pieces"].len();
    assert!(
        piece_type_count > 0 && piece_type_count <= piece_count,
        "Invalid piece type count ({}) for piece count ({})",
        piece_type_count,
        piece_count
    );

    assert!(
        eval_parameters.len() >= 5,
        concat!(
            "[evaluation parameters] must define ",
            "phase scores, opening values, ",
            "endgame values, big, and major"
        )
    );

    let parsed_phase_scores = parse_numeric_parameter_list(
        &eval_parameters[0],
        "phase scores",
    );
    assert!(
        parsed_phase_scores.len() == 2,
        concat!(
            "Phase score row must contain exactly 2 entries: ",
            "[opening, endgame]"
        )
    );

    let opening_phase_score = parsed_phase_scores[0].unsigned_abs();
    let endgame_phase_score = parsed_phase_scores[1].unsigned_abs();

    let parsed_ovalues =
        parse_numeric_parameter_list(&eval_parameters[1], "opening value");
    let parsed_evalues =
        parse_numeric_parameter_list(&eval_parameters[2], "endgame value");

    let evaluation_row_len = parsed_ovalues.len();
    assert!(
        evaluation_row_len == piece_type_count,
        concat!(
            "Opening value count ({}) doesn't match piece type count ({})"
        ),
        evaluation_row_len,
        piece_type_count
    );
    assert!(
        parsed_evalues.len() == piece_type_count,
        concat!(
            "Endgame value count ({}) doesn't match piece type count ({})"
        ),
        parsed_evalues.len(),
        piece_type_count
    );

    let parsed_big_flags = parse_binary_parameter_flags(
        &eval_parameters[3],
        evaluation_row_len,
        "big flag",
    );
    let parsed_major_flags = parse_binary_parameter_flags(
        &eval_parameters[4],
        evaluation_row_len,
        "major flag",
    );

    let mut ovalues = vec![0i32; piece_count];
    let mut evalues = vec![0i32; piece_count];
    let mut big_flags = vec![false; piece_count];
    let mut major_flags = vec![false; piece_count];

    for (piece_idx, piece_type_idx) in piece_type_indices.iter().enumerate() {
        ovalues[piece_idx] = parsed_ovalues[*piece_type_idx];
        evalues[piece_idx] = parsed_evalues[*piece_type_idx];
        big_flags[piece_idx] = parsed_big_flags[*piece_type_idx];
        major_flags[piece_idx] = parsed_major_flags[*piece_type_idx];
    }

    assert!(
        eval_parameters.len() == 5 + piece_type_count * 2,
        concat!(
            "[evaluation parameters] must contain phase scores, opening ",
            "values, endgame values, big, major, and exactly two PST rows ",
            "per piece type (opening/middlegame + endgame)"
        )
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

    let pst_opening_start_row = 5;
    let pst_endgame_start_row = pst_opening_start_row + piece_type_count;

    for i in 0..piece_count {
        let abs_ovalue = ovalues[i].unsigned_abs();
        let abs_evalue = evalues[i].unsigned_abs();

        assert!(
            abs_ovalue <= 0x3FFF,
            "Opening piece value out of range for index {}: {}",
            i,
            ovalues[i]
        );

        assert!(
            abs_evalue <= 0x3FFF,
            "Endgame piece value out of range for index {}: {}",
            i,
            evalues[i]
        );

        pieces[i].8 = abs_ovalue as u16;
        pieces[i].9 = abs_evalue as u16;
        pieces[i].6 = big_flags[i];
        pieces[i].7 = major_flags[i];
        pieces[i].5 = royal_flags[i];
    }

    pst_opening = vec![vec![0i32; board_size]; piece_count];
    pst_endgame = vec![vec![0i32; board_size]; piece_count];

    let mut piece_type_pst_opening =
        vec![vec![0i32; board_size]; piece_type_count];
    let mut piece_type_pst_endgame =
        vec![vec![0i32; board_size]; piece_type_count];
    for piece_type_idx in 0..piece_type_count {
        let pst_opening_values = parse_numeric_parameter_list(
            &eval_parameters[pst_opening_start_row + piece_type_idx],
            "piece square table (opening/middlegame)",
        );
        assert!(
            pst_opening_values.len() == board_size,
            concat!(
                "Opening/middlegame PST length ({}) for piece type index {} ",
                "doesn't match board size ({})"
            ),
            pst_opening_values.len(),
            piece_type_idx,
            board_size
        );
        piece_type_pst_opening[piece_type_idx] = pst_opening_values;

        let pst_endgame_values = parse_numeric_parameter_list(
            &eval_parameters[pst_endgame_start_row + piece_type_idx],
            "piece square table (endgame)",
        );

        assert!(
            pst_endgame_values.len() == board_size,
            concat!(
                "Endgame PST length ({}) for piece type index {} ",
                "doesn't match board size ({})"
            ),
            pst_endgame_values.len(),
            piece_type_idx,
            board_size
        );
        piece_type_pst_endgame[piece_type_idx] = pst_endgame_values;
    }

    for (piece_idx, piece_type_idx) in piece_type_indices.iter().enumerate() {
        let base_pst_opening = &piece_type_pst_opening[*piece_type_idx];
        let base_pst_endgame = &piece_type_pst_endgame[*piece_type_idx];

        if pieces[piece_idx].4 == BLACK {
            pst_opening[piece_idx] = mirror_pst_across_horizontal_axis(
                base_pst_opening,
                files as usize,
                ranks as usize,
            );
            pst_endgame[piece_idx] = mirror_pst_across_horizontal_axis(
                base_pst_endgame,
                files as usize,
                ranks as usize,
            );
        } else {
            pst_opening[piece_idx] = base_pst_opening.clone();
            pst_endgame[piece_idx] = base_pst_endgame.clone();
        }
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
                        pieces[white_index].2.push(promo_index as u8);
                        pieces[black_index].2.push(promo_index as u8);
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
                        pieces[piece_index].2.push(promo_index as u8);
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
                    pieces[index].10 = rank_value;
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
                    p.6,
                    p.7,
                    pieces_moves[i].contains("o"),
                    pieces_moves[i].contains("O"),
                    p.8,
                    p.9,
                    p.10,
                )
            })
            .collect(),
        special_rules,
    );

    result.opening_score = opening_phase_score;
    result.endgame_score = endgame_phase_score;
    result.pst_opening = pst_opening;
    result.pst_endgame = pst_endgame;

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
        for promotion_index in &piece.promotions {
            result
                .piece_demotion_map
                .entry(*promotion_index)
                .or_default()
                .push(p_index!(piece));
        }
    }

    for (index, _) in result.pieces.iter().enumerate() {
        result
            .piece_demotion_map
            .entry(index as u8)
            .or_insert_with(|| vec![index as u8]);
    }

    if castling {
        assert!(
            result
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
            result
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

                    result.promotion_zones_mandatory[white_index] =
                        parse_bit_fen(Some(zone_str), &result);

                    result.promotion_zones_mandatory[black_index] =
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

                    result.promotion_zones_optional[white_index] =
                        parse_bit_fen(Some(zone_str), &result);

                    result.promotion_zones_optional[black_index] =
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

                    result.promotion_zones_optional[piece_index] =
                        parse_bit_fen(Some(zone_str), &result);
                } else {
                    panic!("Invalid piece character(s): {}", piece_chars);
                }
            }
        }
    }

    pieces_drops = vec![DEFAULT_DROP.to_string(); result.pieces.len()];
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

                result.piece_limit[white_index] = limit_value;
                result.piece_limit[black_index] = limit_value;
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

                let white_index =
                    char_to_index.get(&white_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", white_char),
                    );

                let black_index =
                    char_to_index.get(&black_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", black_char),
                    );

                let zone_str = parts[1];

                result.forbidden_zones[white_index] =
                    parse_bit_fen(Some(zone_str), &result);

                result.forbidden_zones[black_index] =
                    parse_bit_fen(Some(zone_str), &result);
            } else if piece_chars.len() == 1 {
                let piece_char = piece_chars.chars().next().unwrap();

                let piece_index =
                    char_to_index.get(&piece_char).copied().unwrap_or_else(
                        || panic!("Unknown piece character: {}", piece_char),
                    );

                let zone_str = parts[1];

                result.forbidden_zones[piece_index] =
                    parse_bit_fen(Some(zone_str), &result);
            } else {
                panic!("Invalid piece character(s): {}", piece_chars);
            }
        }
    }

    pieces_setup = vec![DEFAULT_DROP.to_string(); result.pieces.len()];
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

    pieces_stand_off = vec![String::new(); result.pieces.len()];
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
                    let mut reset_mask = vec![false; result.pieces.len()];
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

        result.halfmove_limit = parsed_limit
            .expect("Halfmove clock limit is missing");
        result.halfmove_pieces = parsed_pieces
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
        result.repetition_limit = limit_value;
    }

    result.precompute(
        pieces_moves,
        pieces_drops,
        pieces_setup,
        pieces_stand_off,
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

    let ranks_data: Vec<&str> = fen.split('/').collect();
    assert!(
        ranks_data.len() == state.ranks as usize,
        "FEN rank count ({}) doesn't match board ranks ({}) for fen {}",
        ranks_data.len(),
        state.ranks,
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

/// Parses a FEN string and updates the game state accordingly.
///
/// Note that the FEN implementation is slightly modified to accommodate
/// arbitrary board sizes. The en passant square is represented as `xxyyzz`
/// where `xx` is the file and `yy` is the rank (both 0-indexed), and `zz`
/// is the piece index in hex.
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
/// The order is as follows:
/// 1. Board representation (ranks separated by '/')
/// 2. Active color ('w' or 'b')
/// 3. Castling rights (e.g. 'KQkq' or '-')
/// 4. En passant square (e.g. '008018P' or '*')
/// 5. Pieces in hand (e.g. '3P2N/1p' or '-/-')
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
                let piece_idx =
                    *state.piece_char_map.get(&c).unwrap_or_else(|| {
                        panic!("Unknown piece character: {}", c)
                    }) as usize;

                let mut piece = &state.pieces[piece_idx];
                let mut piece_index = p_index!(piece);
                let mut piece_color = p_color!(piece);
                let square_index =
                    (rank as u32) * (state.files as u32) + (file as u32);

                if promotions!(state)
                    && piece.promotions.len() == 1
                    && get!(
                        state.promotion_zones_mandatory[piece_index as usize],
                        square_index
                    )
                {
                    piece = &state.pieces[piece.promotions[0] as usize];
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

                if get!(state.initial_setup[piece_index as usize], square_index)
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
            let piece_index = *state
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

                let piece_index = *state
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
        && !state.royal_list[0].is_empty()
        && !state.royal_list[1].is_empty()
    {
        state.setup_phase = false;
    } else if state.royal_list[0].is_empty() || state.royal_list[1].is_empty() {
        state.setup_phase = true;
    }

    if parts.len() > part_index {
        state.halfmove_clock = parts[part_index].parse().unwrap_or_else(|_| {
            panic!("Invalid halfmove clock: {}", parts[part_index].trim())
        });
        part_index += 1;
    }

    if parts.len() > part_index {
        state.ply_counter = parts[part_index].parse().unwrap_or_else(|_| {
            panic!("Invalid ply number: {}", parts[part_index].trim())
        });
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
    let board_size = (state.files as usize) * (state.ranks as usize);
    let piece_count = state.pieces.len();

    let mut all_boards = vec![board!(state.files, state.ranks); piece_count];

    for square in 0..board_size {
        let piece_idx = state.main_board[square];
        if piece_idx != NO_PIECE {
            set!(all_boards[piece_idx as usize], square as u32);
        }
    }

    all_boards
        .iter()
        .enumerate()
        .map(|(i, b)| format_board(b, Some(state.pieces[i].char)))
        .reduce(|board_str, next_board| {
            combine_board_strings(&board_str, &next_board)
        })
        .expect("Failed to format combined board string")
}

pub fn format_fen(state: &State) -> String {
    let mut fen = String::new();

    for rank in (0..state.ranks).rev() {
        let mut empty_count = 0;

        for file in 0..state.files {
            let square_index =
                (rank as u32) * (state.files as u32) + (file as u32);
            let piece_idx = state.main_board[square_index as usize];
            if piece_idx == NO_PIECE {
                empty_count += 1;
            } else {
                if empty_count > 0 {
                    fen.push_str(&empty_count.to_string());
                    empty_count = 0;
                }
                fen.push(state.pieces[piece_idx as usize].char);
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
        fen.push_str(&format_en_passant_square(state));
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
            state.pieces[en_passant_piece_idx as usize].char;

        format!(
            "{:03x}{:03x}{}",
            en_passant_sq, en_passant_piece_sq, en_passant_piece_char
        )
    }
}

pub fn format_hand(state: &State, color: u8) -> String {
    let pieces_in_hand = &state.piece_in_hand[color as usize];
    let mut hand = String::new();

    for (i, piece) in state.pieces.iter().enumerate() {
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
    let width = 3;

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
    } else if state.setup_phase {
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
    if state.special_rules == 0 {
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
