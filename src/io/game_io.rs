//! game_io.rs
//!
//! Parses and formats the game state.
//!
//! This file converts between the in-memory position and its text forms.
//! It makes a playable state from a variant config, reads and writes FEN,
//! and writes the state as readable text. The other modules use only
//! `State` and do not parse notation.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             PATTERNS AND DEFAULTS
\*----------------------------------------------------------------------------*/

/// DEFAULT_DROP
///
/// The default drop expression of a piece when the variant has drops or a
/// setup phase but no rule for that piece:
///
/// ```text
/// @  #  ~  ?  @
/// ^  ^     ^  ^
/// |  |     |  no stoppers, so no neighbour can refuse the square
/// |  |     the empty-square sentinel
/// |  the target square itself, at no offset from it
/// no modifiers, so the drop carries no flags
/// ```
///
/// Thus a drop can go on any empty square. A variant with more rules, for
/// example no two pawns on a file, writes them in `= drop rules =` or
/// `= setup rules =`.
///
const DEFAULT_DROP: &str = "@#~?@";

lazy_static! {
    /// CFEN field patterns
    ///
    /// Anchored regexes of the three optional CFEN fields. The config load
    /// uses them to test the start position, and the FEN load uses them to
    /// test each field.
    ///
    /// - CASTLING_PATTERN : `KQkq`, any of the four rights, or `-`
    /// - ENP_PATTERN      : `034044P`, square, captured square, piece, or `*`
    /// - HAND_PATTERN     : `PNN/-`, White hand, `/`, Black hand
    ///
    /// Notes:
    /// The two en passant squares have three hex digits, for boards up to
    /// 4096 squares. The hand pattern only splits the field. The parser tests
    /// the piece letters later.
    ///
    static ref CASTLING_PATTERN: Regex =
        Regex::new(r"^([KQkq]+)$|^-$").unwrap();
    static ref ENP_PATTERN: Regex =
        Regex::new(r"^([0-9a-fA-F]{3})([0-9a-fA-F]{3})(.)$|^\*$").unwrap();
    static ref HAND_PATTERN: Regex = Regex::new(r"^(.*)/(.*)$").unwrap();
}

/*----------------------------------------------------------------------------*\
                           CASTLING LAYOUT VALIDATION
\*----------------------------------------------------------------------------*/

/// validate_castling
///
/// Tests one castling layout against the start position of the variant.
/// A config can list layouts for a family of variants. A layout stays only
/// when each square that it names a piece on is occupied at the start.
///
/// ```text
/// startpos   R N B Q K B N R    standard's own first rank
/// layout     R + * * K . . .    the queen-side pair names a1 and e1
///            ^       ^          both occupied at the start, so kept
///
/// startpos   . N B Q K B N R    a variant starting without that rook
/// layout     R + * * K . . .    the same layout, offered by the same
///            ^                  family, now names an empty a1: dropped
/// ```
///
/// Params:
/// - fen  : &str   -> the castling layout to test, as a board
/// - state: &State -> variant with the start position
///
/// Return:
/// bool            -> true when each named square is occupied at the start
///
/// Notes:
/// Only occupancy is compared, not the piece type. The layout uses the
/// `parse_bit_fen` grammar. `*` and `+` mark squares that the king crosses
/// or that must be empty, so they name no piece.
///
fn validate_castling(fen: &str, state: &State) -> bool {
    let startpos = &state.statics.startpos
        .split_whitespace()
        .collect::<Vec<_>>()[0];

    let mut a = vec![NO_PIECE; state.statics.board_size];
    let mut b = vec![NO_PIECE; state.statics.board_size];

    let mut rank = state.statics.ranks - 1;
    let mut file = 0u8;

    let mut position_chars = startpos.chars().peekable();
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
                let piece =
                    *state.statics.piece_char_map
                    .get(&c).unwrap_or_else(|| {
                        panic!("Unknown piece character: {}", c)
                    });
                let square_index = (rank as u32) * (state.statics.files as u32)
                    + (file as u32);

                a[square_index as usize] = piece;

                file += 1;
            }
        }
    }

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
            _ => {
                if c != '*' && c != '+' {
                    let piece =
                        *state.statics.piece_char_map
                        .get(&c).unwrap_or_else(|| {
                            panic!("Unknown piece character: {}", c)
                        });
                    let square_index =
                        (rank as u32) * (state.statics.files as u32) +
                        (file as u32);

                    b[square_index as usize] = piece;
                }

                file += 1;
            }
        }
    }

    let mut valid = true;

    for (a_e, b_e) in zip(a, b) {
        if b_e != NO_PIECE && a_e == NO_PIECE {
            valid = false;
        }
    }

    valid
}

/*----------------------------------------------------------------------------*\
                             TUNED PARAMETER FILES
\*----------------------------------------------------------------------------*/

/// parse_tuned_parameters
///
/// Loads the evaluation parameters of a variant from one list of integers.
/// The list has no names, so its length identifies it. With `T` piece
/// types and `S` squares, it must have `2·T + 2·T·S + 11` tokens. Another
/// count means a list of another variant.
///
/// ```text
/// │ material │ piece-square tables │ scalars │
///     2·T             2·T·S            11
/// ```
///
/// - material : all opening values, then all endgame values
/// - tables   : type 0 opening, type 0 endgame, type 1 opening, and so on
/// - scalars  : the eleven scalars below, in this order
///
/// The file has only White tables. Black tables are the White tables
/// mirrored across the ranks. The scalar order:
///
/// - 1 tempo bonus, 2 major imbalance, 3 minor imbalance, 4 pair bonus
/// - 5 shelter value, 6 guard value, 7 castled value, 8 castling right
/// - 9 king danger scale, 10 king danger cap, 11 open shield penalty
///
/// Params:
/// - state  : &mut State -> variant that gets the parameters
/// - content: &str       -> parameter list, separated by spaces
///
/// Notes:
/// The install order is fixed, because each step reads the step before:
///
/// 1. material, the opening and endgame values of each piece type
/// 2. products derived from that material
/// 3. tables, White as loaded, Black mirrored
/// 4. derivation of search, shelter, danger, pawn, advantage, capability
/// 5. scalars, then the pawn cache is cleared
///
/// Each value must be within ±0x3FFF, the width of the packed piece word.
/// A value out of range causes a panic, because the file is corrupt.
///
#[hotpath::measure]
pub fn parse_tuned_parameters(state: &mut State, content: &str) {
    let piece_type_pairs = collect_piece_type_pairs(state);
    let piece_type_count = piece_type_pairs.len();
    let board_size = state.statics.board_size;
    let tokens: Vec<i32> = content.split_whitespace().map(|token| {
        token.parse::<i32>().unwrap_or_else(|_| {
            panic!("Invalid parameter value: {}", token)
        })
    }).collect();
    let expected_count = 2 * piece_type_count
        + 2 * piece_type_count * board_size + 11;

    assert_eq!(
        tokens.len(), expected_count,
        concat!(
            "Parameter count mismatch: expected {} tokens for {} piece ",
            "types on {} squares, found {}."
        ),
        expected_count, piece_type_count, board_size, tokens.len()
    );

    let mut cursor = 0usize;
    let opening_values = &tokens[cursor..cursor + piece_type_count];
    cursor += piece_type_count;
    let endgame_values = &tokens[cursor..cursor + piece_type_count];
    cursor += piece_type_count;
    let mut rows = Vec::with_capacity(piece_type_count);

    for piece_type_index in 0..piece_type_count {
        let opening_value = opening_values[piece_type_index];
        let endgame_value = endgame_values[piece_type_index];
        let absolute_opening = opening_value.unsigned_abs();
        let absolute_endgame = endgame_value.unsigned_abs();

        assert!(
            absolute_opening <= 0x3FFF,
            "Opening piece value out of range at index {}: {}",
            piece_type_index,
            opening_value
        );
        assert!(
            absolute_endgame <= 0x3FFF,
            "Endgame piece value out of range at index {}: {}",
            piece_type_index,
            endgame_value
        );

        let opening = tokens[cursor..cursor + board_size].to_vec();
        cursor += board_size;
        let endgame = tokens[cursor..cursor + board_size].to_vec();
        cursor += board_size;
        rows.push((opening, endgame));

        let (white_index, black_index) = piece_type_pairs[piece_type_index];

        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[white_index],
            absolute_opening as u16,
            absolute_endgame as u16,
            false,
            false,
        );
        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[black_index],
            absolute_opening as u16,
            absolute_endgame as u16,
            false,
            false,
        );
    }

    derive_eval_products(state);

    for (piece_type_index, (white_index, black_index)) in
        piece_type_pairs.iter().copied().enumerate()
    {
        let (opening, endgame) = &rows[piece_type_index];
        assert!(opening.iter().chain(endgame.iter())
            .all(|value| (-0x3FFF..=0x3FFF).contains(value)),
            "Piece-square value out of range");

        state.static_mut().pst_opening[white_index] = opening.clone();
        state.static_mut().pst_opening[black_index] =
            mirror_pst_across_horizontal_axis(
                &opening,
                state.statics.files as usize,
                state.statics.ranks as usize,
            );
        state.static_mut().pst_endgame[white_index] = endgame.clone();
        state.static_mut().pst_endgame[black_index] =
            mirror_pst_across_horizontal_axis(
                &endgame,
                state.statics.files as usize,
                state.statics.ranks as usize,
            );
    }

    derive_search_parameters(state);
    derive_shelter_parameters(state);
    derive_danger_parameters(state);
    derive_pawn_parameters(state);
    derive_advantage_parameters(state);
    derive_search_capabilities(state);

    let values = &tokens[cursor..];

    assert!(
        values.iter().all(|value| (-0x3FFF..=0x3FFF).contains(value)),
        "Evaluation scalar out of range"
    );

    let eval = &mut state.static_mut().eval;
    [
        eval.tempo_bonus, eval.imbalance_major,
        eval.imbalance_minor, eval.pair_bonus,
        eval.shelter_value, eval.guard_value,
        eval.castled_value, eval.castling_right_value,
        eval.king_danger_scale, eval.king_danger_cap,
        eval.open_shield_penalty,
    ] = values.try_into().unwrap();

    state.scratch.pawn_table.table.fill(PTEntry::default());
    refresh_eval_state(state);
}

/// export_tuned_parameters_file
///
/// Writes the evaluation parameters of a variant in the order that
/// `parse_tuned_parameters` reads. Only White tables are written. The load
/// mirrors them for Black.
///
/// - res/param/<variant>/2026-09-06_14-02-11.param : the old file
/// - res/param/<variant>/latest.param              : the new file
///
/// Params:
/// - state  : &State -> variant with the parameters
/// - variant: &str   -> variant name, selects the directory
///
/// Notes:
/// The function makes the directory if necessary. It first rolls the old
/// `latest.param` to a timestamp, so a person can restore it.
///
pub fn export_tuned_parameters_file(
    state: &State,
    variant: &str,
) {
    assert!(!variant.trim().is_empty(), "Variant name cannot be empty");

    let piece_type_pairs = collect_piece_type_pairs(state);
    let mut output_tokens = Vec::new();

    for (white_index, _) in &piece_type_pairs {
        output_tokens.push(
            p_ovalue!(state.statics.pieces[*white_index]).to_string()
        );
    }

    for (white_index, _) in &piece_type_pairs {
        output_tokens.push(
            p_evalue!(state.statics.pieces[*white_index]).to_string()
        );
    }

    for (white_index, _) in &piece_type_pairs {
        for square in 0..state.statics.board_size {
            output_tokens.push(
                state.statics.pst_opening[*white_index][square].to_string()
            );
        }

        for square in 0..state.statics.board_size {
            output_tokens.push(
                state.statics.pst_endgame[*white_index][square].to_string()
            );
        }
    }

    let eval = &state.statics.eval;
    output_tokens.extend([
        eval.tempo_bonus, eval.imbalance_major,
        eval.imbalance_minor, eval.pair_bonus,
        eval.shelter_value, eval.guard_value,
        eval.castled_value, eval.castling_right_value,
        eval.king_danger_scale, eval.king_danger_cap,
        eval.open_shield_penalty,
    ].iter().map(ToString::to_string));

    let dir_path = format!("{}/{}", PARAMS_DIR, variant);

    if !Path::new(&dir_path).exists() {
        fs::create_dir_all(&dir_path).unwrap_or_else(|error| {
            panic!("Failed to create directory {}: {}", dir_path, error)
        });
    }

    let file_path = format!("{}/latest.param", dir_path);

    roll_latest(&dir_path, "", "param");

    fs::write(&file_path, output_tokens.join(" ")).unwrap_or_else(|error| {
        panic!("Failed to write parameter file {}: {}", file_path, error)
    });
}

/*----------------------------------------------------------------------------*\
                             CONFIGURATION PARSING
\*----------------------------------------------------------------------------*/

/// parse_config_preview
///
/// Reads only a small part of a config to show a variant in a list. A full
/// `State` needs compilation, derivation and precomputation, which is too
/// slow for a list.
///
/// - `= general =`     : the title and the start position
/// - `= piece order =` : the piece letters, in index order
///
/// The function fills one bitboard for each piece letter and puts them on
/// one diagram, as `format_game_state` does:
///
/// ```text
///    ╔═══╤═══╗       ╔═══╤═══╗       ╔═══╤═══╗
///  2 ║ k │   ║       ║   │   ║       ║ k │   ║
///    ╟───┼───╢   +   ╟───┼───╢   =   ╟───┼───╢
///  1 ║   │   ║       ║   │ K ║       ║   │ K ║
///    ╚═══╧═══╝       ╚═══╧═══╝       ╚═══╧═══╝
///      a   b           a   b           a   b
/// ```
///
/// Params:
/// - path: &str     -> config file name in the embedded configs
///
/// Return:
/// (String, String) -> the variant title and the start board diagram
///
/// Notes:
/// The function still tests that each rank exists and has the same width.
///
pub fn parse_config_preview(path: &str) -> (String, String) {
    let sections = split_sections(&config_text(path));

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

/// config_text
///
/// Reads a config. The embedded copy comes first, so a released binary
/// needs no `res/config` directory.
///
/// - path    : res/config/standard.conf, or standard.conf
/// - lookup  : the file name only, in the embedded set
/// - found   : read from the binary
/// - missing : read from the path on disk
///
/// Params:
/// - path: &str -> config file name, e.g. "standard.conf"
///
/// Return:
/// String       -> the config text
///
/// Notes:
/// The disk fallback lets a person try a new config without a rebuild.
///
fn config_text(path: &str) -> String {
    Path::new(path)
        .file_name()
        .and_then(|filename| filename.to_str())
        .and_then(|filename| EMBEDDED_CONFIGS.get_file(filename))
        .and_then(|file| file.contents_utf8())
        .map(str::to_string)
        .unwrap_or_else(|| {
            fs::read_to_string(path)
                .expect("Failed to read configuration file")
        })
}

/// split_sections
///
/// Splits `= section =` text into a table of titles and bodies. The
/// `.conf` and `.dict` files both use this format:
///
/// ```text
/// // the standard game       stripped, wherever the // starts
/// = general =                a title
/// Standard Chess             its body, blank lines dropped
///
/// = piece order =            the next title ends the previous body
/// PRNBQKprnbqk
/// ```
///
/// The result is a table with the section names as keys:
///
/// ```text
/// "general"       ["Standard Chess", "rnbqkbnr/... w KQkq * 1"]
/// "piece order"   ["PRNBQKprnbqk"]
/// ```
///
/// Params:
/// - content: &str              -> raw `.conf` or `.dict` file text
///
/// Return:
/// HashMap<String, Vec<String>> -> section title to body lines
///
/// Notes:
/// Titles and bodies pair by position. The text before the first title is
/// not a body, and an empty section keeps its title with no lines. Comments
/// are removed before the split.
///
pub fn split_sections(content: &str) -> HashMap<String, Vec<String>> {
    let uncommented = COMMENT_PATTERN.replace_all(content, "");
    let cleaned = uncommented
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    let titles = SECTION_PATTERN.captures_iter(&cleaned);
    let bodies = SECTION_PATTERN.split(&cleaned).skip(1);

    titles
        .zip(bodies)
        .map(|(title, body)| {
            let lines = body
                .lines()
                .map(str::to_string)
                .filter(|line| !line.trim().is_empty())
                .collect();

            (title[1].trim().to_string(), lines)
        })
        .collect()
}

/// piece_indices
///
/// Converts the piece letter key of a config row into piece indices. All
/// piece sections use this key. Its length gives the colours:
///
/// - `Pp:mnW|im<nW-pnW>` : two letters, the rule is for the two pawns
/// - `P:RBNQ`            : one letter, the rule is for the White pawn
///
/// Params:
/// - piece_chars  : &str                  -> the key letters of the row
/// - char_to_index: &HashMap<char, usize> -> piece letter to piece index
///
/// Return:
/// Vec<usize>                             -> one index for each letter
///
/// Notes:
/// Another key length or an unknown letter causes a panic.
///
fn piece_indices(
    piece_chars: &str,
    char_to_index: &HashMap<char, usize>,
) -> Vec<usize> {
    let key_length = piece_chars.chars().count();

    assert!(
        key_length == 1 || key_length == 2,
        "Invalid piece character(s): {}",
        piece_chars
    );

    piece_chars
        .chars()
        .map(|piece_char| {
            char_to_index.get(&piece_char).copied().unwrap_or_else(|| {
                panic!("Unknown piece character: {}", piece_char)
            })
        })
        .collect()
}

/// parse_config_file
///
/// Makes a playable variant from one config file. The board, the pieces,
/// their moves and the game end all come from here. `example.conf` gives
/// the grammar of each section.
///
/// No single section has all the data of a piece, so the parser first
/// collects a tuple:
///
/// - name       : "Pawn", the same for the two colours
/// - char       : 'P', the letter of the piece
/// - promotions : types it can promote to, from `= promotions =`
/// - index      : its place in `= piece order =`
/// - color      : WHITE or BLACK
/// - royal      : true when its loss can end the game
/// - rank       : its rank, from `= piece ranks =`
///
/// Each `= pieces =` line adds two entries, White then Black. Then
/// `= piece order =` gives the final indices:
///
/// - `= pieces =`      : Pp:Pawn, two entries, one for each colour
/// - `= piece order =` : PRNBQKprnbqk, the index of each entry
///
/// Each section fills its part of the state:
///
/// - general     : the title, the start position and the board size
/// - rules       : the special rules of the variant
/// - pieces      : the letters, the order, the roles, the ranks
/// - castling    : the layouts, filtered by `validate_castling`
/// - promotions  : promotion types, zones and triggers
/// - drops       : where a piece in hand can go
/// - forbidden   : squares that a piece can never occupy
/// - setup       : where a piece can go before play starts
/// - stand-offs  : patterns that a position must not have
/// - termination : the game end rules and their results
///
/// The parameters come last, from the first source that exists:
///
/// - embedded : res/param/<variant>/latest.param, in the binary
/// - on disk  : the same path on the file system
/// - neither  : derived from the rules, then exported
///
/// Params:
/// - path: &str -> config file name in the embedded configs
///
/// Return:
/// State        -> the initialized variant, ready to play
///
/// Notes:
/// The parser first tests the rules against the start position. A variant
/// with castling must have rights in its FEN and a `= castling =` section.
/// A variant without castling must have neither.
///
#[hotpath::measure]
pub fn parse_config_file(path: &str) -> State {
    let sections = split_sections(&config_text(path));

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
    let forbidden_zones =
        sections["rules"].contains(&"forbidden zones".to_string());
    let promote_to_captured =
        sections["rules"].contains(&"promote to captured".to_string());
    let setup_phase = sections["rules"].contains(&"setup phase".to_string());
    let stand_offs = sections["rules"].contains(&"stand-offs".to_string());

    let mut fen_castling = false;
    let mut fen_en_passant = false;
    let mut fen_in_hand = false;

    for part in initial_position.split_whitespace().skip(2) {
        fen_castling |= CASTLING_PATTERN.is_match(part);
        fen_en_passant |= ENP_PATTERN.is_match(part);
        fen_in_hand |= HAND_PATTERN.is_match(part);

        if fen_castling && fen_en_passant && fen_in_hand {
            break;
        }
    }

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

    if drops || promote_to_captured || setup_phase {
        assert!(fen_in_hand, "No pieces in hand found in FEN");
    }

    if castling {
        assert!(
            sections.contains_key("castling"),
            "= castling = section is missing"
        )
    }

    if promotions {
        assert!(
            sections.contains_key("promotions"),
            "= promotions = section is missing"
        );
        assert!(
            sections.contains_key("mandatory promotion zones") ||
            sections.contains_key("optional promotion zones"),
            "No promotion zones section found"
        );
    }

    if forbidden_zones {
        assert!(
            sections.contains_key("forbidden zones"),
            "= forbidden zones = section is missing"
        );
    }

    if stand_offs {
        assert!(
            sections.contains_key("stand-off patterns"),
            "= stand-off patterns = section is missing"
        );
    }

    let mut special_rules = 0u8;

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

    if forbidden_zones {
        enc_forbidden_zones!(special_rules);
    }

    if promote_to_captured {
        enc_promote_to_captured!(special_rules);
    }

    if setup_phase {
        enc_setup_phase!(special_rules);
    }

    if stand_offs {
        enc_stand_offs!(special_rules);
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

        let move_pattern = parts[1].to_string();

        for index in piece_indices(parts[0], &char_to_index) {
            pieces_moves[index] = move_pattern.clone();
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

            let indices = piece_indices(parts[0], &char_to_index);

            for promo_char in parts[1].chars() {
                let Some(&promo_index) = char_to_index.get(&promo_char) else {
                    panic!("Unknown promotion piece character: {}", promo_char);
                };

                for &index in &indices {
                    pieces[index].2.push(promo_index as PieceIndex);
                }
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
            .map(|p| {
                Piece::new(
                    p.0.clone(),
                    p.1,
                    p.2.clone(),
                    p.3,
                    p.4,
                    p.5,
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
            else if char_to_index.contains_key(&c) { 'O' }
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

    let move_notations = pieces_moves
        .iter()
        .map(|mv| strip_move_conditions(mv))
        .collect::<Vec<String>>();

    if en_passant {
        assert!(
            move_notations
                .iter()
                .any(|mv| mv.contains('p') || mv.contains('t')),
            "No en passant movement found in piece definitions"
        );
    }

    if !en_passant {
        assert!(
            move_notations
                .iter()
                .all(|mv| !mv.contains('p') || !mv.contains('t')),
            "En passant movement found in piece definitions"
        );
    }

    result.static_mut().piece_char_map = result.statics.pieces
        .iter()
        .enumerate()
        .map(|(index, piece)| (piece.char, index as PieceIndex))
        .collect();

    result.static_mut().piece_demotion_map = if sections.contains_key(
        "demotions"
    ) {
        let mut map = vec![NO_PIECE; piece_count];

        for demotion in &sections["demotions"] {
            let parts: Vec<&str> =
                demotion.split(':').map(str::trim).collect();

            assert_eq!(parts.len(), 2, "demotions line incorrectly formatted");
            assert_eq!(parts[1].chars().count(), 1, "must be n-to-1");

            let demoted_piece = parts[1].chars().next().unwrap();
            let demoted_index = result.statics.piece_char_map[&demoted_piece];

            for c in parts[0].chars() {
                let index = result.statics.piece_char_map[&c] as usize;
                map[index] = demoted_index;
            }
        }

        map
    } else {
        result.statics.pieces
            .iter()
            .map(|p| p_index!(p))
            .collect()
    };

    for (index, entry) in result.static_mut().piece_demotion_map
        .iter_mut()
        .enumerate()
    {
        if *entry == NO_PIECE {
            *entry = index as PieceIndex;
        }
    }

    /*-----------------------------------------------------------------------*\
                                 PARSE CASTLING
    \*-----------------------------------------------------------------------*/

    if castling {
        let pieces_line = &sections["castling"][0];

        assert!(
            pieces_line.starts_with("pieces"),
            "castling pieces part not formatted correctly"
        );

        let castling_pieces = pieces_line
            .split(":")
            .collect::<Vec<_>>()[1]
            .trim();

        let mut castling_leaders: Vec<usize> = Vec::new();

        for p_char in castling_pieces.chars() {
            let p_index = result.statics.piece_char_map[&p_char] as usize;
            let p_color = p_color!(&result.statics.pieces[p_index]);

            if castling_leaders.iter().all(|leader| {
                p_color!(&result.statics.pieces[*leader]) != p_color
            }) {
                castling_leaders.push(p_index);
            }

            result.static_mut().castling_pieces[p_index] = true;
        }

        let start_lines = &sections["castling"][1..5];
        let mut startings: [Vec<String>; 4] = array::from_fn(|_| Vec::new());
        let parsed_start_lines = start_lines
            .iter()
            .map(
                |line| {
                    let l = line.split(":").collect::<Vec<_>>();
                    (
                        match l[0] {
                            "K" => WK_INDEX as usize,
                            "Q" => WQ_INDEX as usize,
                            "k" => BK_INDEX as usize,
                            "q" => BQ_INDEX as usize,
                            _ => unreachable!()
                        },
                        l[1]
                        .split('|')
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>()
                    )
                }
            )
            .collect::<Vec<(usize, Vec<String>)>>();

        for (index, parsed) in parsed_start_lines {
            startings[index] = parsed.clone();
        }

        let end_lines = &sections["castling"][5..];
        let mut endings: [Vec<String>; 4] = array::from_fn(|_| Vec::new());
        let parsed_end_lines = end_lines
            .iter()
            .map(
                |line| {
                    let l = line.split(":").collect::<Vec<_>>();
                    (
                        match l[0] {
                            "K" => WK_INDEX as usize,
                            "Q" => WQ_INDEX as usize,
                            "k" => BK_INDEX as usize,
                            "q" => BQ_INDEX as usize,
                            _ => unreachable!()
                        },
                        l[1]
                        .split('|')
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>()
                    )
                }
            )
            .collect::<Vec<(usize, Vec<String>)>>();

        for (index, parsed) in parsed_end_lines {
            endings[index] = parsed.clone();
        }

        let possible_pairs: [(Vec<String>, Vec<String>); 4] =
            zip(startings, endings)
            .map(
                |(start, end)|

                {
                    let mut new_pairs = (Vec::new(), Vec::new());

                    for (s, e) in zip(start, end) {
                        if validate_castling(&s, &result) {
                            new_pairs.0.push(s);
                            new_pairs.1.push(e);
                        }
                    }

                    new_pairs
                }
            )
            .collect::<Vec<_>>()
            .try_into()
            .expect("Expected exactly 4 entries");

        result.static_mut().critical_castling = possible_pairs
            .iter()
            .map(
                |(start, _)|

                start.iter()
                    .map(
                        |s|
                        parse_bit_fen(Some(&s.chars()
                            .map(
                                |c|
                                if c.is_numeric() || c == '/' {
                                    c
                                } else if c == '*' || c == '+' {
                                    'O'
                                } else {
                                    'X'
                                }
                            )
                            .collect::<String>()),
                            &result
                        )
                    )
                    .fold(
                        board!(files, ranks),
                        |mut acc, n| {
                            or!(acc, n);
                            acc
                        }
                    )
            )
            .collect::<Vec<_>>()
            .try_into()
            .expect("Expected exactly 4 entries");

        result.static_mut().relevant_castling = possible_pairs
            .iter()
            .map(|(start, end)| {
                generate_relevant_castling(
                    start, end, &castling_leaders, &result
                )
            })
            .collect::<Vec<_>>()
            .try_into()
            .expect("Expected exactly 4 entries");

    }

    /*-----------------------------------------------------------------------*\
                                PARSE PROMOTIONS
    \*-----------------------------------------------------------------------*/

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

                for index in piece_indices(parts[0], &char_to_index) {
                    result.static_mut().promotion_zones_mandatory[index] =
                        parse_bit_fen(Some(parts[1]), &result);
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

                for index in piece_indices(parts[0], &char_to_index) {
                    result.static_mut().promotion_zones_optional[index] =
                        parse_bit_fen(Some(parts[1]), &result);
                }
            }
        }

        let mut triggers = 0u8;

        if let Some(names) = sections.get("promotion triggers") {
            for name in names {
                match name.trim() {
                    "entry" => { enc_promote_on_entry!(triggers); }
                    "exit" => { enc_promote_on_exit!(triggers); }
                    other => panic!("Unknown promotion trigger: {}", other),
                }
            }
        } else {
            enc_promote_on_entry!(triggers);                                    /* what every variant written before  */
            enc_promote_on_exit!(triggers);                                     /* the section existed already meant  */
        }

        result.static_mut().promotion_triggers = triggers;
    }

    /*-----------------------------------------------------------------------*\
                                  PARSE DROPS
    \*-----------------------------------------------------------------------*/

    pieces_drops = vec![
        DEFAULT_DROP.to_string(); result.static_mut().pieces.len()
    ];
    if drops && sections.contains_key("drop rules") {
        for drop in &sections["drop rules"] {
            let parts: Vec<&str> = drop.split(':').map(str::trim).collect();

            assert!(parts.len() == 2, "Invalid drop definition: {}", drop);

            let drop_pattern = parts[1].to_string();

            for index in piece_indices(parts[0], &char_to_index) {
                pieces_drops[index] = drop_pattern.clone();
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                             PARSE FORBIDDEN ZONES
    \*-----------------------------------------------------------------------*/

    if forbidden_zones {
        for forbidden in &sections["forbidden zones"] {
            let parts: Vec<&str> =
                forbidden.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid forbidden zone definition: {}",
                forbidden
            );

            for index in piece_indices(parts[0], &char_to_index) {
                result.static_mut().forbidden_zones[index] =
                    parse_bit_fen(Some(parts[1]), &result);
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                               PARSE SETUP PHASE
    \*-----------------------------------------------------------------------*/

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

            let setup_pattern = parts[1].to_string();

            for index in piece_indices(parts[0], &char_to_index) {
                pieces_setup[index] = setup_pattern.clone();
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                                PARSE STAND-OFFS
    \*-----------------------------------------------------------------------*/

    pieces_stand_off = vec![String::new(); result.statics.pieces.len()];
    if stand_offs {
        for pattern in &sections["stand-off patterns"] {
            let parts: Vec<&str> =
                pattern.split(':').map(str::trim).collect();

            assert!(
                parts.len() == 2,
                "Invalid stand-off pattern definition: {}",
                pattern
            );

            let stand_off_patterns = parts[1].to_string();

            for index in piece_indices(parts[0], &char_to_index) {
                pieces_stand_off[index] = stand_off_patterns.clone();
            }
        }
    }

    /*-----------------------------------------------------------------------*\
                               PARSE TERMINATION
    \*-----------------------------------------------------------------------*/

    let parse_outcome = |token: &str| -> Outcome {
        match token {
            "draw" => Outcome::Draw,
            "win" => Outcome::Win,
            "loss" => Outcome::Loss,
            other => panic!("Unknown end-condition outcome: {}", other),
        }
    };

    let set_piece_count = result.statics.pieces.len();
    let parse_set = |set_str: &str| -> Vec<bool> {
        let mut set = vec![false; set_piece_count];
        let listed = set_str.strip_prefix('!').unwrap_or(set_str);              /* `!` names the complement instead   */

        if listed == "*" {
            set.iter_mut().for_each(|flag| *flag = true);
        } else {
            for piece_char in listed.chars() {
                let index = char_to_index.get(&piece_char).copied()
                    .unwrap_or_else(|| panic!(
                        "Unknown piece character in end condition: {}",
                        piece_char
                    ));
                set[index] = true;
            }
        }

        if listed.len() != set_str.len() {
            set.iter_mut().for_each(|flag| *flag = !*flag);
        }

        set
    };

    let mut termination = Termination::default();

    if let Some(entries) = sections.get("termination") {
        for entry in entries {
            let parts: Vec<&str> =
                entry.splitn(2, ':').map(str::trim).collect();
            assert!(
                parts.len() == 2,
                "Invalid end condition definition: {}",
                entry
            );

            let arguments: Vec<&str> = parts[1].split_whitespace().collect();
            let name = parts[0].to_string();

            match parts[0] {
                "checkmate" => {
                    termination.checkmate = parse_outcome(arguments[0]);
                }
                "stalemate" => {
                    termination.stalemate = parse_outcome(arguments[0]);
                }
                "repetition" => {
                    let occurrences =
                        arguments[0].parse::<u8>().unwrap_or_else(|_| {
                            panic!(
                                "Invalid repetition count: {}", arguments[0]
                            )
                        });
                    let outcome = arguments.get(1)
                        .map(|&token| parse_outcome(token))
                        .unwrap_or(Outcome::Draw);
                    
                    termination.repetition = Some(Repetition {
                        occurrences, outcome, name, clock: 0,
                    });
                }
                "counter" => {
                    let limit =
                        arguments[0].parse::<u8>().unwrap_or_else(|_| {
                            panic!("Invalid counter limit: {}", arguments[0])
                        });
                    let mut reset_pieces = vec![false; set_piece_count];
                    
                    if arguments[1] != "-" {
                        for piece_char in arguments[1].chars() {
                            let piece_index = char_to_index
                                .get(&piece_char)
                                .copied()
                                .unwrap_or_else(|| {
                                    panic!(
                                        "Unknown piece in counter: {}",
                                        piece_char
                                    )
                                });
                            reset_pieces[piece_index] = true;
                        }
                    }
                    
                    let outcome = arguments.get(2)
                        .map(|&token| parse_outcome(token))
                        .unwrap_or(Outcome::Draw);
                    
                    termination.counter = Some(Counter {
                        clock: 0,
                        limit, reset_pieces, outcome, name,
                    });
                }
                "counting" => {
                    let section = format!("counting {}", arguments[0]);
                    let lines = sections.get(&section).unwrap_or_else(|| {
                        panic!("Missing = {} = section for counting", section)
                    });
                    let outcome = arguments.get(1)
                        .map(|&token| parse_outcome(token))
                        .unwrap_or(Outcome::Draw);
                    let mut table = Vec::new();
                    let mut default = 64u16;

                    for line in lines {
                        let parts: Vec<&str> =
                            line.splitn(2, ':').map(str::trim).collect();
                        let limit =
                            parts[1].parse::<u16>().unwrap_or_else(|_| {
                                panic!("Invalid counting limit: {}", parts[1])
                            });
                        if parts[0] == "default" {
                            default = limit;
                            continue;
                        }
                        let mut counts: HashMap<char, u32> = HashMap::new();
                        for piece_char in parts[0].chars() {
                            *counts
                                .entry(piece_char.to_ascii_uppercase())
                                .or_insert(0) += 1;
                        }
                        let requirements = counts.into_iter()
                            .map(|(piece_char, minimum)| {
                                let mut set = vec![false; set_piece_count];
                                for cased in [
                                    piece_char.to_ascii_uppercase(),
                                    piece_char.to_ascii_lowercase(),
                                ] {
                                    if let Some(&index) =
                                        char_to_index.get(&cased)
                                    {
                                        set[index] = true;
                                    }
                                }
                                (set, minimum)
                            })
                            .collect();
                        table.push((requirements, limit));
                    }

                    termination.counting = Some(Counting {
                        progress: None,
                        table, default, outcome, name,
                    });
                }
                "checks" => {
                    let count =
                        arguments[0].parse::<u8>().unwrap_or_else(|_| {
                            panic!("Invalid check count: {}", arguments[0])
                        });
                    let outcome = arguments.get(1)
                        .map(|&token| parse_outcome(token))
                        .unwrap_or(Outcome::Win);

                    termination.checks = Some(Checks {
                        delivered: [0; 2], count, outcome, name,
                    });
                }
                "extinct" => {
                    let set = parse_set(arguments[0]);
                    let (threshold, outcome) = if arguments.len() >= 3 {
                        (
                            arguments[1].parse::<u8>().unwrap_or_else(|_| {
                                panic!("Invalid extinct count: {}",
                                    arguments[1])
                            }),
                            parse_outcome(arguments[2]),
                        )
                    } else {
                        (0, parse_outcome(arguments[1]))
                    };

                    termination.extinct.push(Extinct {
                        set, threshold, outcome, name, lone: [None; 2],
                    });
                }
                "goal" => {
                    let set = parse_set(arguments[0]);
                    let zone_section = format!("zone {}", arguments[1]);
                    let zone_fen = sections.get(&zone_section)
                        .and_then(|lines| lines.first())
                        .unwrap_or_else(|| panic!(
                            "Missing = {} = section for goal", zone_section
                        ));
                    let zone = parse_bit_fen(Some(zone_fen.as_str()), &result);
                    let outcome = parse_outcome(arguments[2]);

                    termination.goal = Some(Goal { set, zone, outcome, name });
                }
                "perpetual" => {
                    let mut check = None;
                    let mut chase = None;
                    let mut chasers = vec![true; set_piece_count];
                    let mut index = 0;
                    while index < arguments.len() {
                        match arguments[index] {
                            "check" => {
                                check = Some(
                                    parse_outcome(arguments[index + 1])
                                );
                                index += 2;
                            }
                            "chase" => {
                                chase = Some(
                                    parse_outcome(arguments[index + 1])
                                );
                                index += 2;
                            }
                            "exempt" => {
                                for (piece, exempt) in parse_set(
                                    arguments[index + 1]
                                ).iter().enumerate() {
                                    if *exempt {
                                        chasers[piece] = false;
                                    }
                                }
                                index += 2;
                            }
                            other => panic!(
                                "Unknown perpetual offence: {}", other
                            ),
                        }
                    }
                    termination.perpetual =
                        Some(Perpetual { check, chase, chasers, name });
                }
                "adjudicate" => {
                    let section = format!("adjudicate {}", arguments[0]);
                    let lines = sections.get(&section).unwrap_or_else(|| {
                        panic!(
                            "Missing = {} = section for adjudicate", section
                        )
                    });
                    let mut weights = vec![0i32; set_piece_count];
                    let mut handicap = [0i32; 2];
                    for line in lines {
                        let parts: Vec<&str> =
                            line.splitn(2, ':').map(str::trim).collect();

                        if parts[0] == "handicap" {
                            let tokens: Vec<&str> =
                                parts[1].split_whitespace().collect();
                            let color =
                                if tokens[0] == "w" { WHITE } else { BLACK };

                            handicap[color as usize] =
                                tokens[1].parse::<i32>().unwrap_or_else(|_| {
                                    panic!("Invalid handicap: {}", tokens[1])
                                });
                        } else {
                            let weight =
                                parts[1].parse::<i32>().unwrap_or_else(|_| {
                                    panic!(
                                        "Invalid adjudicate weight: {}",
                                        parts[1]
                                    )
                                });

                            for piece_char in parts[0].chars() {
                                for cased in [
                                    piece_char.to_ascii_uppercase(),
                                    piece_char.to_ascii_lowercase(),
                                ] {
                                    if let Some(&index) =
                                        char_to_index.get(&cased)
                                    {
                                        weights[index] = weight;
                                    }
                                }
                            }
                        }
                    }
                    termination.adjudicate = Some(
                        Adjudicate { weights, handicap, name }
                    );
                }
                other => panic!("Unknown end condition: {}", other),
            }
        }
    }

    if termination.perpetual.is_some() && termination.repetition.is_none() {
        panic!(
            "end condition `perpetual` requires `repetition`"
        );
    }

    result.termination = termination;

    /*-----------------------------------------------------------------------*\
                              POST-PARSING COMPUTE
    \*-----------------------------------------------------------------------*/

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
    let param_path = format!("{}/{}/latest.param", PARAMS_DIR, variant);

    if let Some(content) = EMBEDDED_PARAMS
        .get_file(format!("{}/latest.param", variant))
        .and_then(|f| f.contents_utf8())
    {
        log_3!("Loading embedded default parameters");
        parse_tuned_parameters(&mut result, content);
    } else if let Ok(content) = fs::read_to_string(&param_path) {
        log_3!("Loading parameters from disk");
        parse_tuned_parameters(&mut result, &content);
    } else {
        derive_parameters(&mut result);
        export_tuned_parameters_file(&result, variant);
    }

    result.position_hash = hash_position(&result);
    result.pawn_hash = hash_pawns(&result);

    result
}

/*----------------------------------------------------------------------------*\
                           ZONE AND POSITION LOADING
\*----------------------------------------------------------------------------*/

/// parse_bit_fen
///
/// Reads a board description into a bitboard. It is the board part of a
/// FEN, but only occupancy matters, not the letter. Promotion zones,
/// forbidden zones, castling layouts and setup masks use it.
///
/// - XXXXXXXX/8/8/8/8/8/8/8 : a promotion zone, the full eighth rank
/// - 8/8/8/8/8/8/8/4XOOX    : a castling layout, e1 and h1, not f1 or g1
///
/// Each letter is one square. A digit is that number of squares:
///
/// - digit : skip that number of squares, no change
/// - /     : end the rank, go to the rank below
/// - O     : clear the square
/// - other : set the square, for any letter
///
/// Params:
/// - fen  : Option<&str> -> the board description, None for an empty mask
/// - state: &State       -> position with the board dimensions
///
/// Return:
/// Board                 -> a bitboard with the described squares set
///
/// Notes:
/// `parse_config_file` makes one setup mask for each piece from the start
/// FEN: `X` where that piece is and `O` where another piece is. Thus `O`
/// means "another piece", not "empty". A description that does not fit the
/// board causes a panic at load time.
///
#[hotpath::measure]
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

/// parse_fen
///
/// Loads a position in Cheesy Forsyth-Edwards Notation (CFEN) into the
/// state. A GUI or a harness writes the input, so a bad field gives an
/// error, not a panic.
///
/// CFEN is FEN without the fields that the variant does not use. Thus the
/// variant rules give the field count:
///
/// - position   : as in FEN, it also gives the board size
/// - side       : w or b
/// - castling   : KQkq or -, only with castling
/// - en passant : ssseeez or *, only with en passant
/// - in hand    : white/black, only with pieces in hand
/// - halfmove   : optional, read only with a counter rule
/// - fullmove   : optional, starts at one
///
/// The en passant field names the full capture, because the captured piece
/// can be far from the target square:
///
/// ```text
/// 034 044 P
/// ^   ^   ^
/// |   |   the piece on that square
/// |   the square of the piece to capture, in hex
/// the target square of the capturing piece, in hex
/// ```
///
/// The hand field has one piece list for each side:
///
/// - PNN/- : White has a pawn and two knights, Black has nothing
/// - -/-   : no side has pieces in hand
///
/// The number of fields after the last mandatory field selects the clocks:
///
/// - two : the halfmove clock, then the fullmove number
/// - one : the fullmove number only
/// - none: the counters do not change
///
/// Params:
/// - state: &mut State          -> position to load
/// - fen  : &str                -> the CFEN string to load
/// - dict : Option<&Translator> -> optional protocol translation first
///
/// Return:
/// Result<(), String>           -> Ok, or the first bad field error
///
/// Notes:
/// A dictionary translates the input into engine notation first. After the
/// load, the function tests the game end rules, because a loaded position
/// can already be over.
///
pub fn parse_fen(
    state: &mut State, fen: &str, dict: Option<&Translator>
) -> Result<(), String> {
    let mut needed_parts = 2;

    if castling!(state) {
        needed_parts += 1;
    }

    if en_passant!(state) {
        needed_parts += 1;
    }

    if drops!(state)
    || promote_to_captured!(state)
    || setup_phase!(state)
    {
        needed_parts += 1;
    }

    let mut translated = fen.to_string();
    if let Some(translator) = dict {
        for (pattern, replacement) in &translator.inverse_fen {
            translated = pattern
                .replace_all(&translated, replacement)
                .into_owned();
        }
    }
    let fen = &translated;

    let parts: Vec<&str> = fen.split_whitespace().collect();
    if parts.len() < needed_parts {
        return Err(format!(
            "FEN must have at least {} parts", needed_parts
        ));
    }
    if parts.len() > needed_parts + 2 {
        return Err(format!(
            "FEN has {} parts but expected at most {}",
            parts.len(),
            needed_parts + 2,
        ));
    }
    let mut part_index = 0;

    let position = parts[part_index];
    part_index += 1;

    let ranks_data: Vec<&str> = position.split('/').collect();
    if ranks_data.len() != state.statics.ranks as usize {
        return Err(format!(
            "FEN rank count ({}) doesn't match board ranks ({})",
            ranks_data.len(),
            state.statics.ranks,
        ));
    }

    for (rank_index, rank_data) in ranks_data.iter().enumerate() {
        let mut file_count = 0u32;
        let mut chars = rank_data.chars().peekable();
        while let Some(character) = chars.next() {
            let width = if character.is_ascii_digit() {
                let mut number = character.to_string();
                while let Some(&next_character) = chars.peek() {
                    if next_character.is_ascii_digit() {
                        number.push(next_character);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let width = number.parse::<u32>().map_err(|_| {
                    format!("Invalid empty-square count: {}", number)
                })?;
                if width == 0 {
                    return Err(
                        "FEN empty-square count must be positive".to_string()
                    );
                }
                width
            } else {
                1
            };
            file_count = file_count.checked_add(width).ok_or_else(|| {
                format!("FEN rank {} file count overflow", rank_index)
            })?;
        }
        if file_count != state.statics.files as u32 {
            return Err(format!(
                "FEN rank {} has {} files but expected {}",
                rank_index,
                file_count,
                state.statics.files,
            ));
        }
    }

    let mut rank = u32::from(state.statics.ranks)
        .checked_sub(1)
        .ok_or_else(|| "FEN board has no ranks".to_string())?;
    let mut file = 0u32;
    let board_files = u32::from(state.statics.files);

    let mut position_chars = position.chars().peekable();
    while let Some(character) = position_chars.next() {
        match character {
            '/' => {
                rank = rank.checked_sub(1)
                    .ok_or_else(|| "FEN has too many ranks".to_string())?;
                file = 0;
            }
            '0'..='9' => {
                let mut number = character.to_string();
                while let Some(&next_character) = position_chars.peek() {
                    if next_character.is_ascii_digit() {
                        number.push(next_character);
                        position_chars.next();
                    } else {
                        break;
                    }
                }
                let width = number.parse::<u32>().map_err(|_| {
                    format!("Invalid empty-square count: {}", number)
                })?;
                file = file.checked_add(width).ok_or_else(|| {
                    "FEN file index overflow".to_string()
                })?;
            }
            _ => {
                let piece_index = state.statics.piece_char_map
                    .get(&character)
                    .copied()
                    .ok_or_else(|| {
                        format!("Unknown piece character: {}", character)
                    })? as usize;

                let piece = &state.statics.pieces[piece_index];
                let piece_index = p_index!(piece);
                let piece_color = p_color!(piece);
                let square_index = rank
                    .checked_mul(board_files)
                    .and_then(|base| base.checked_add(file))
                    .ok_or_else(|| "FEN square index overflow".to_string())?;
                if square_index >= state.main_board.len() as u32 {
                    return Err(format!(
                        "FEN square index {} is outside the board",
                        square_index,
                    ));
                }

                state.main_board[square_index as usize] = piece_index;

                piece_list_push!(
                    state, piece_index as usize, square_index as Square
                );

                set!(state.pieces_board[piece_color as usize], square_index);

                if p_is_royal!(piece) {
                    state.royal_list[piece_color as usize]
                        .push(square_index as Square);
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
                ) {
                    set!(state.virgin_board, square_index);
                }

                file = file.checked_add(1)
                    .ok_or_else(|| "FEN file index overflow".to_string())?;
            }
        }
    }

    state.playing = match parts[part_index] {
        "w" => WHITE,
        "b" => BLACK,
        active => return Err(format!("Invalid active color: {}", active)),
    };
    part_index += 1;

    if castling!(state) {
        let castling = parts[part_index];
        part_index += 1;
        if !CASTLING_PATTERN.is_match(castling) {
            return Err(format!("Invalid castling rights: {}", castling));
        }
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
            let captures = ENP_PATTERN.captures(en_passant)
                .ok_or_else(|| {
                    format!("Invalid en passant field: {}", en_passant)
                })?;
            let square_text = captures.get(1)
                .ok_or_else(|| {
                    format!("Invalid en passant square: {}", en_passant)
                })?
                .as_str();
            let captured_text = captures.get(2)
                .ok_or_else(|| {
                    format!(
                        "Invalid en passant captured square: {}",
                        en_passant,
                    )
                })?
                .as_str();
            let piece_text = captures.get(3)
                .ok_or_else(|| {
                    format!("Invalid en passant piece: {}", en_passant)
                })?
                .as_str();

            let square_index = u32::from_str_radix(square_text, 16)
                .map_err(|_| {
                    format!("Invalid en passant square: {}", square_text)
                })?;
            let captured_index = u32::from_str_radix(captured_text, 16)
                .map_err(|_| {
                    format!(
                        "Invalid en passant captured square: {}",
                        captured_text,
                    )
                })?;
            let board_size = state.main_board.len() as u32;
            if square_index >= board_size {
                return Err(format!(
                    "En passant square {} is outside the board",
                    square_text,
                ));
            }
            if captured_index >= board_size {
                return Err(format!(
                    "En passant captured square {} is outside the board",
                    captured_text,
                ));
            }
            let piece_character = piece_text.chars().next()
                .ok_or_else(|| {
                    format!("Invalid en passant piece: {}", piece_text)
                })?;
            let piece_index = state.statics.piece_char_map
                .get(&piece_character)
                .copied()
                .ok_or_else(|| {
                    format!("Unknown piece character: {}", piece_text)
                })? as u32;

            square_index | (captured_index << 11) | piece_index << 22
        };
    }

    if drops!(state)
    || promote_to_captured!(state)
    || setup_phase!(state)
    {
        let hands = parts[part_index];
        part_index += 1;

        let hand_parts: Vec<&str> = hands.split('/').collect();
        if hand_parts.len() != 2 {
            return Err(format!("Invalid pieces in hand format: {}", hands));
        }

        for (color_index, hand_part) in hand_parts.iter().enumerate() {
            if hand_part == &"-" {
                continue;
            }

            for character in hand_part.chars() {
                let piece_index = state.statics.piece_char_map
                    .get(&character)
                    .copied()
                    .ok_or_else(|| {
                        format!(
                            "Unknown piece character in hand: {}",
                            character,
                        )
                    })? as usize;
                let count = &mut state.piece_in_hand[color_index][piece_index];
                *count = count.checked_add(1).ok_or_else(|| {
                    format!(
                        "Too many {} pieces in hand",
                        character,
                    )
                })?;
            }
        }
    }

    if setup_phase!(state)
    && (state.royal_list[0].is_empty() || state.royal_list[1].is_empty()) {
        state.game_phase = SETUP;
    }

    let trailing = parts.len() - part_index;                                    /* the field count selects the clocks */
    if trailing > 2 {                                                           /* two: halfmove and fullmove         */
        return Err(format!(                                                     /* one: fullmove only                 */
            "Unexpected FEN field: {}",                                         /* format_fen writes the halfmove     */
            parts[part_index + 2]                                               /* only with a counter rule           */
        ));
    }

    if trailing == 2 {
        let halfmove = parts[part_index].trim().parse::<u32>()
            .map_err(|_| {
                format!("Invalid halfmove clock: {}", parts[part_index].trim())
            })?;

        if let Some(counter) = &mut state.termination.counter {
            counter.clock = halfmove.min(u8::MAX as u32) as u8;
        }

        part_index += 1;
    }

    if trailing >= 1 {
        let fullmove = parts[part_index].trim().parse::<u32>()
            .map_err(|_| {
                format!("Invalid fullmove number: {}", parts[part_index])
            })?;
        state.ply_counter = fullmove.checked_sub(1)
            .and_then(|number| number.checked_mul(2))
            .and_then(|number| number.checked_add(state.playing as u32))
            .ok_or_else(|| {
                format!("Invalid fullmove number: {}", fullmove)
            })?;
    }

    refresh_eval_state(state);

    state.position_hash = hash_position(state);
    state.virgin_hash = hash_virgin_board(state);
    state.pawn_hash = hash_pawns(state);

    if let Some((color, outcome, _)) = position_terminal(state) {
        state.termination.game_result = resolve_outcome!(color, outcome);       /* a loaded position can already be   */
    }                                                                           /* over; only make_move! knew before  */

    Ok(())
}

/*----------------------------------------------------------------------------*\
                                STATE RENDERING
\*----------------------------------------------------------------------------*/

/// combine_board_strings
///
/// Puts one board diagram on another, character by character.
/// `format_board` shows one bitboard, so a position is drawn one piece
/// type at a time, and the diagrams are merged:
///
/// ```text
///    ╔═══╤═══╗       ╔═══╤═══╗       ╔═══╤═══╗
///  2 ║ k │   ║       ║   │   ║       ║ k │   ║
///    ╟───┼───╢   +   ╟───┼───╢   =   ╟───┼───╢
///  1 ║   │   ║       ║   │ K ║       ║   │ K ║
///    ╚═══╧═══╝       ╚═══╧═══╝       ╚═══╧═══╝
///      a   b           a   b           a   b
/// ```
///
/// The two diagrams have the same layout, so the merge is by position:
///
/// - equal       : keep the character, all borders and labels
/// - first blank : take the character of the second
/// - otherwise   : keep the character of the first
///
/// Params:
/// - board1: &str -> the board on top
/// - board2: &str -> the board below
///
/// Return:
/// String         -> the merged diagram
///
/// Notes:
/// The merge stops at the end of the shorter diagram. All callers use the
/// dimensions of one variant, so the two lengths are equal.
///
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

/// format_game_state
///
/// Writes a position as one board diagram. A diagram shows one bitboard,
/// so the function works in three steps:
///
/// 1. make one bitboard for each piece type
/// 2. draw one diagram for each bitboard, with the piece letter
/// 3. merge the diagrams in piece index order
///
/// Params:
/// - state: &State -> position to show
///
/// Return:
/// String          -> the position as one board diagram
///
/// Notes:
/// The field formatters below write the rights, hands, phase and result.
///
pub fn format_game_state(state: &State) -> String {
    let board_size = state.statics.board_size;
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

/// format_fen
///
/// Writes a position as CFEN, the inverse of `parse_fen`. It writes the same
/// fields in the same order, only the fields of the variant rules.
///
/// - position   : ranks from the top, empty squares counted together
/// - side       : w or b
/// - castling   : only with castling
/// - en passant : only with en passant
/// - in hand    : only with pieces in hand
/// - halfmove   : only with a counter rule
/// - fullmove   : always, starts at one
///
/// Params:
/// - state: &State              -> position to write
/// - dict : Option<&Translator> -> the output dialect, if any
///
/// Return:
/// String                       -> the position as CFEN
///
/// Notes:
/// The en passant field is written only if a legal capture takes that
/// piece, else `*`. Thus two positions with the same play have the same
/// FEN. The dictionary translates the complete string at the end.
///
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
                m_captures!(m).iter().any(|&cap|
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
    || setup_phase!(state)
    {
        fen.push(' ');
        fen.push_str(&format_hand(state, WHITE));
        fen.push('/');
        fen.push_str(&format_hand(state, BLACK));
    }

    if let Some(counter) = &state.termination.counter {
        fen.push(' ');
        fen.push_str(&counter.clock.to_string());
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

/// State field formatters
///
/// Write one state field as text, for CFEN and the debug board. The
/// caller decides where the text goes.
///
/// - format_castling_rights   : KQkq, or - when none are left
/// - format_en_passant_square : ssseeez, or * when there is no square
/// - format_hand              : a list of piece letters, or - when empty
/// - format_position_hash     : the position hash, in hexadecimal
/// - format_search_keys       : the search and quiescence keys, in hex
/// - format_game_result       : the result in words, Ongoing if not ended
/// - format_game_phase        : the phase name, Game Over after the end
/// - format_special_rules     : the rule names, separated by commas
///
/// format_castling_rights, format_en_passant_square,
/// format_position_hash, format_search_keys, format_game_phase,
/// format_special_rules
///
///   Params:
///   - state: &State -> position with the field
///
///   Return:
///   String          -> the field as text
///
/// format_hand
///
///   Params:
///   - state: &State -> position with the hands
///   - color: u8     -> side of the hand
///
///   Return:
///   String          -> the hand as text
///
/// format_game_result
///
///   Params:
///   - result: u8 -> the result to write
///
///   Return:
///   String       -> the result in words
///
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
        hand.push_str(
            &piece.char.to_string().repeat(pieces_in_hand[i] as usize)
        );
    }

    if hand.is_empty() {
        "-".to_string()
    } else {
        hand
    }
}

pub fn format_position_hash(state: &State) -> String {
    format!("{:>016X}", state.position_hash)
}

pub fn format_search_keys(state: &State) -> String {
    let repeats = count_repetitions(state, SEARCH_REPETITION_CAP);
    let in_check = is_in_check!(state.playing, state);

    format!(
        "{:>016X} / {:>016X}",
        search_key(state, repeats),
        qsearch_key(state, repeats, in_check),
    )
}

pub fn format_game_result(result: u8) -> String {
    match result {
        DRAW => "Draw".to_string(),
        WHITE_WIN => "White wins".to_string(),
        BLACK_WIN => "Black wins".to_string(),
        _ => "Ongoing".to_string(),
    }
}

pub fn format_game_phase(state: &State) -> String {
    if is_terminal!(state) {
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
    if forbidden_zones!(state) {
        rules.push("Forbidden Zones");
    }
    if promote_to_captured!(state) {
        rules.push("Promote to Captured");
    }
    if setup_phase!(state) {
        rules.push("Setup Phase");
    }
    if stand_offs!(state) {
        rules.push("Stand-Offs");
    }

    rules.join(", ")
}
