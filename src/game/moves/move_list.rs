//! move_list.rs
//!
//! Generates moves and attack data for the current position.
//!
//! This is the runtime part of move generation. The parse modules compile
//! the move expressions into vectors once. This file walks those vectors on
//! the current board to make `Move`s, test attacks, and make and undo
//! moves with all incremental updates. All of it is on the search hot
//! path, so the leg loops are macros.
//!
//! Created: 01/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                          ATTACK QUERY REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// is_square_attacked!
///
/// Tells if one precomputed attack mask can attack `$square` now. It tests
/// the direction, occupancy and modifier rules with
/// `validate_attack_vector!`.
///
/// Params:
/// - square          : Square -> target square to test
/// - attacked_side   : u8     -> side of the piece on the square
/// - attacked_unmoved: bool   -> true when that piece is unmoved
/// - attacked_royal  : bool   -> true when the target is royal
/// - attacked_rank   : u8     -> rank of the target piece
/// - state           : &State -> current position with the attack tables
///
/// Return:
/// bool                       -> true when a legal attack reaches the square
///
#[macro_export]
macro_rules! is_square_attacked {
    (
        $square:expr,
        $attacked_side:expr,
        $attacked_unmoved:expr,
        $attacked_royal:expr,
        $attacked_rank:expr,
        $state:expr
    ) => {{
        let possible_attacks = &$state.statics.relevant_attacks
            [$attacked_side as usize][$square as usize];

        possible_attacks.iter().any(|(piece_index, start, move_vector)| {
            $state.main_board[*start as usize] == *piece_index
                && validate_attack_vector!(
                    move_vector,
                    *start,
                    &$state.statics.pieces[*piece_index as usize],
                    $attacked_unmoved,
                    $attacked_royal,
                    $attacked_rank,
                    $square,
                    $state
                )
        })
    }};
}

/// is_in_check!
///
/// Tells if `$side` is in check. It tests each royal square with
/// `is_square_attacked!`. A side with many royals is in check only when all
/// are attacked. In the setup phase or without a royal, it is false.
///
/// Params:
/// - side : u8     -> side of the royal pieces
/// - state: &State -> current position with the royal list and tables
///
/// Return:
/// bool            -> true when the side is in check
///
#[macro_export]
macro_rules! is_in_check {
    ($side:expr, $state:expr) => {
        hotpath::measure_block!("state::is_in_check", {
        let monarch_indices = &$state.royal_list[$side as usize];

        (!monarch_indices.is_empty() && $state.game_phase != SETUP) && {
            monarch_indices.iter().all(|&idx| {
                let royal_piece = &$state.main_board[idx as usize];
                let royal_rank =
                    p_rank!($state.statics.pieces[*royal_piece as usize]);

                is_square_attacked!(
                    idx as u32,
                    $side,
                    get!($state.virgin_board, idx as u32),
                    true,
                    royal_rank,
                    $state
                )
            })
        }
        })
    };
}

/// legal_moves!
///
/// Collects all legal moves of the position. It generates the pseudo-legal
/// moves and drops and keeps each move that `make_move!` accepts. Then it
/// undoes the move, so the position does not change.
///
/// Params:
/// - state: &mut State -> position to examine, restored at the end
///
/// Return:
/// Vec<Move>           -> the legal moves, empty in a terminal position
///
#[macro_export]
macro_rules! legal_moves {
    ($state:expr) => {{
        let mut moves = Vec::with_capacity(64);
        let mut scratch = Vec::with_capacity(16);
        generate_all_moves_and_drops($state, &mut moves, &mut scratch);

        moves.into_iter().filter(|mv| {
            if make_move!($state, mv.clone()) {
                undo_move!($state);
                true
            } else {
                false
            }
        }).collect::<Vec<Move>>()
    }};
}

/// generate_relevant_castling
///
/// Compiles the config castling layouts into precomputed castling `Move`s.
/// Each option is a pair of layouts. The start layout has the pieces and
/// the squares that must be empty (`+`) or empty and not attacked (`*`).
/// The end layout has the same pieces on their end squares:
///
/// ```text
/// start                            end
/// ┌────┬────┬────┬────┬────┐      ┌────┬────┬────┬────┬────┐
/// │ R  │ +  │ *  │ *  │ K  │  ->  │    │ K  │ R  │    │    │
/// └────┴────┴────┴────┴────┘      └────┴────┴────┴────┴────┘
/// ```
///
/// The royal piece is in the main slot of the move, and the partner in the
/// capture and unload slots. The `+` and `*` squares go into the move list
/// for the runtime test.
///
/// Params:
/// - start: &Vec<String> -> start layouts, one for each castling option
/// - end  : &Vec<String> -> matching end layouts
/// - state: &State       -> piece dictionary and board dimensions
///
/// Return:
/// Vec<Move>             -> one castling move for each layout pair
///
pub fn generate_relevant_castling(
    start: &Vec<String>, end: &Vec<String>, state: &State
) -> Vec<Move> {

    let mut result = Vec::new();

    for (s, e) in zip(start, end) {
        let mut encoded_move = Move::default();

        enc_move_type!(encoded_move, CASTLING_MOVE);
        enc_is_unload!(encoded_move, 1);

        let mut start_map = HashMap::new();
        let mut end_map = HashMap::new();
        let mut check_list = Vec::new();

        let mut rank = state.statics.ranks - 1;
        let mut file = 0u8;

        let mut position_chars = s.chars().peekable();
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
                    let piece = if c != '*' && c != '+' {
                        let piece =
                            *state.statics.piece_char_map
                            .get(&c).unwrap_or_else(|| {
                                panic!("Unknown piece character: {}", c)
                            }) as usize;

                        assert!(
                            state.statics.castling_pieces[piece],
                            "Piece is {} is not a castling participant piece",
                            c
                        );

                        piece
                    } else {
                        NO_PIECE as usize
                    };

                    let square_index =
                        (rank as u32) * (state.statics.files as u32) +
                        (file as u32);

                    if c == '*' || c == '+' {
                        let mut check_square = 0u64;

                        if c == '*' {
                            enc_multi_move_is_unload!(check_square, 1u64);
                        }

                        enc_multi_move_unload_square!(
                            check_square, square_index as u64
                        );

                        check_list.push(check_square);
                    }

                    if c != '*' && c != '+' {
                        start_map.insert(piece, square_index);
                    }

                    file += 1;
                }
            }
        }

        let mut rank = state.statics.ranks - 1;
        let mut file = 0u8;

        let mut position_chars = e.chars().peekable();
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
                        }) as usize;

                    assert!(
                        state.statics.castling_pieces[piece],
                        "Piece is {} is not a castling participant piece",
                        c
                    );

                    let square_index =
                        (rank as u32) * (state.statics.files as u32) +
                        (file as u32);

                    end_map.insert(piece, square_index);

                    file += 1;
                }
            }
        }

        assert_eq!(
            start_map.keys().collect::<HashSet<_>>(),
            end_map.keys().collect::<HashSet<_>>(),
            "start and end map are not in sync"
        );

        let zipped = start_map
            .into_iter()
            .filter_map(|(k, v1)| {
                end_map.get(&k).map(|&v2| (k, v1, v2))
            })
            .collect::<Vec<_>>();

        for (piece, start_sq, end_sq) in zipped {
            let is_royal = p_is_royal!(&state.statics.pieces[piece]);

            if is_royal {
                enc_piece!(encoded_move, piece as u128);
                enc_start!(encoded_move, start_sq as u128);
                enc_end!(encoded_move, end_sq as u128);
            } else {
                enc_captured_piece!(encoded_move, piece as u128);
                enc_captured_square!(encoded_move, start_sq as u128);
                enc_unload_square!(encoded_move, end_sq as u128);
            }
        }

        encoded_move.1 = Some(Arc::new(mem::take(&mut check_list)));

        result.push(encoded_move);
    }

    result
}

/// generate_relevant_moves
///
/// Finds the compiled move vectors of a piece that can go from one origin
/// square. It walks each vector leg by leg, mirrored for Black. A leg off
/// the board or into the forbidden zone removes the vector. From a corner,
/// only the vectors on the board stay:
///
/// ```text
/// ┌────┬────┬────┬────┐
/// │ S  │ == │ == │ == │
/// ├────┼────┼────┼────┤
/// │ || │ \\ │    │    │
/// ├────┼────┼────┼────┤
/// │ || │    │ \\ │    │
/// └────┴────┴────┴────┘
/// ```
///
/// The function ignores occupancy, because move generation tests it. Thus
/// the result is a static table entry. The vectors are sorted longest
/// first.
///
/// Params:
/// - piece       : &Piece     -> piece type of the vectors
/// - square_index: u32        -> origin square
/// - state       : &State     -> board dimensions and forbidden zones
/// - piece_moves : &[MoveSet] -> compiled vector sets, one for each piece
///
/// Return:
/// MoveSet                    -> playable vectors, longest first
///
#[hotpath::measure]
pub fn generate_relevant_moves(
    piece: &Piece,
    square_index: u32,
    state: &State,
    piece_moves: &[MoveSet],
) -> MoveSet {
    let piece_index = p_index!(piece) as usize;
    let piece_color = p_color!(piece);
    let vector_set = &piece_moves[piece_index];

    let mut result = MoveSet::new();
    'multi_leg: for multi_leg_vector in vector_set {
        let mut accumulated_index = square_index as i32;

        let mut file = accumulated_index % (state.statics.files as i32);
        let mut rank = accumulated_index / (state.statics.files as i32);

        for leg in multi_leg_vector.iter() {
            let file_offset = x!(leg);
            let rank_offset = y!(leg);

            let bypass = v!(leg) && not_v!(leg);

            file += file_offset as i32 * (-2 * piece_color as i32 + 1);
            rank += rank_offset as i32 * (-2 * piece_color as i32 + 1);
            accumulated_index = rank * (state.statics.files as i32) + file;

            if file < 0
                || file >= state.statics.files as i32
                || rank < 0
                || rank >= state.statics.ranks as i32
                || (forbidden_zones!(state)
                    && get!(
                        state.statics.forbidden_zones[piece_index],
                        accumulated_index as u32
                    )
                    && !bypass)
            {
                continue 'multi_leg;
            }
        }

        result.push(multi_leg_vector.clone());
    }

    result.sort_by_key(|v| -(v.len() as isize));
    result
}

/// generate_relevant_captures
///
/// Finds the vectors of a piece from one origin that can capture or
/// destroy. It uses the same bounds and zone tests as
/// `generate_relevant_moves`, and keeps a vector only if a leg has:
///
/// - explicit capture (`c`)
/// - destroy (`d`)
/// - implicit last leg capture
///
/// Thus capture generation uses the full move pipeline on a smaller set.
///
/// Params:
/// - piece       : &Piece     -> piece type of the vectors
/// - square_index: u32        -> origin square
/// - state       : &State     -> board dimensions and forbidden zones
/// - piece_moves : &[MoveSet] -> compiled vector sets, one for each piece
///
/// Return:
/// MoveSet                    -> playable capture vectors
///
#[hotpath::measure]
pub fn generate_relevant_captures(
    piece: &Piece,
    square_index: u32,
    state: &State,
    piece_moves: &[MoveSet],
) -> MoveSet {
    let piece_index = p_index!(piece) as usize;
    let piece_color = p_color!(piece);
    let vector_set = &piece_moves[piece_index];

    let mut result = MoveSet::new();
    'multi_leg: for multi_leg_vector in vector_set {
        let mut accumulated_index = square_index as i32;

        let mut file = accumulated_index % (state.statics.files as i32);
        let mut rank = accumulated_index / (state.statics.files as i32);

        let mut has_capture_leg = false;

        for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
            let last_leg = leg_index + 1 == multi_leg_vector.len();

            let file_offset = x!(leg);
            let rank_offset = y!(leg);

            let bypass = v!(leg) && not_v!(leg);

            file += file_offset as i32 * (-2 * piece_color as i32 + 1);
            rank += rank_offset as i32 * (-2 * piece_color as i32 + 1);
            accumulated_index = rank * (state.statics.files as i32) + file;

            if file < 0
                || file >= state.statics.files as i32
                || rank < 0
                || rank >= state.statics.ranks as i32
                || (forbidden_zones!(state)
                    && get!(
                        state.statics.forbidden_zones[piece_index],
                        accumulated_index as u32
                    )
                    && !bypass)
            {
                continue 'multi_leg;
            }

            let c = c!(leg) || (last_leg && !m!(leg));
            let d = d!(leg);

            if c || d {
                has_capture_leg = true;
            }
        }

        if has_capture_leg {
            result.push(multi_leg_vector.clone());
        }
    }

    result.sort_by_key(|v| -(v.len() as isize));
    result
}

/// generate_attack_masks
///
/// Collects the `relevant_attacks` entries from one origin square. For
/// each move vector, it records if each target is attacked by capture (`c`)
/// or destroy (`d`).
///
/// Params:
/// - square_index: u16    -> origin square of the attacks
/// - state       : &State -> engine state with the move tables
///
/// Return:
///
///     Vec<(usize, usize, AttackMask)>
///     entries to write, as (attacked side, square, entry)
///
/// Notes:
/// The function only reads, so each square can run on its own thread. The
/// caller stores the entries in square order, as a serial pass would.
///
#[hotpath::measure]
pub fn generate_attack_masks(
    square_index: u16,
    state: &State,
) -> Vec<(usize, usize, AttackMask)> {
    let board_size = state.statics.board_size;
    let files = state.statics.files;

    let mut pending: Vec<(usize, usize, AttackMask)> = Vec::new();

    for piece in &state.statics.pieces {
        let piece_index = p_index!(piece);
        let piece_color = p_color!(piece);

        let vector_set = &state.statics.relevant_moves
            [piece_index as usize * board_size + square_index as usize];

        for multi_leg_vector in vector_set {
            let mut accumulated_index = square_index as i16;

            let leg_count = multi_leg_vector.len();

            for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
                let last_leg = leg_index + 1 == leg_count;

                let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
                let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

                accumulated_index += rank_offset as i16 * files as i16          /* widened: a leg reaching four ranks */
                    + file_offset as i16;                                       /* on a wide board overflows a byte   */

                let c = c!(leg) || (last_leg && !m!(leg));
                let d = d!(leg);

                let mask = (
                    piece_index, square_index, Arc::clone(multi_leg_vector)
                );

                if d {
                    pending.push((
                        piece_color as usize,
                        accumulated_index as usize,
                        mask.clone(),
                    ));
                }

                if c {
                    pending.push((
                        1 - piece_color as usize,
                        accumulated_index as usize,
                        mask,
                    ));
                }
            }
        }
    }

    pending
}

/// validate_attack_vector!
///
/// Tells if an attack vector can legally reach a target square. It
/// simulates each leg: move, capture, destroy and unload rules, occupancy,
/// rank, royal and unmoved filters, and the special modifier pairs. It
/// tests the attack candidates of `relevant_attacks`.
///
/// Params:
/// - multi_leg_vector: &MoveVector -> attack vector to simulate
/// - square_index    : Square      -> origin square of the attacker
/// - attacking_piece : &Piece      -> attacking piece
/// - attacked_unmoved: bool        -> true when the target is unmoved
/// - attacked_royal  : bool        -> true when the target is royal
/// - attacked_rank   : u8          -> rank of the target
/// - attacked_square : Square      -> square of the capture leg
/// - state           : &State      -> current position
///
/// Return:
/// bool                            -> true when the vector attacks the square
///
/// Notes:
/// The colour of the attacker scales the offsets, so one vector works for
/// the two sides.
///
#[macro_export]
macro_rules! validate_attack_vector {
    (
        $multi_leg_vector:expr,
        $square_index:expr,
        $attacking_piece:expr,
        $attacked_unmoved:expr,
        $attacked_royal:expr,
        $attacked_rank:expr,
        $attacked_square:expr,
        $state:expr
    ) => {{
        let piece_color = p_color!($attacking_piece);
        let piece_rank = p_rank!($attacking_piece);
        let piece_unmoved =
            get!($state.virgin_board, $square_index as u32);
        let piece_index = p_index!($attacking_piece);

        let mut accumulated_index = $square_index as i16;
        let mut target_was_last_captured = false;

        let leg_count = $multi_leg_vector.len();

        let promotable =
            promotions!($state) && p_can_promote!($attacking_piece);

        let mut valid = true;

        for (leg_index, leg) in $multi_leg_vector.iter().enumerate() {
            let last_leg = leg_index + 1 == leg_count;

            let start_square = accumulated_index as u32;

            let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
            let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

            accumulated_index += rank_offset as i16                             /* widened: a leg reaching four       */
                * ($state.statics.files as i16) + file_offset as i16;           /* ranks on a wide board overflows    */

            let end_square = accumulated_index as u32;

            let m = m!(leg) || (!c!(leg) && !d!(leg));
            let c = c!(leg) || (last_leg && !m!(leg));
            let d = d!(leg);
            let u = u!(leg);
            let k = k!(leg);
            let g = g!(leg);
            let v = v!(leg);
            let t = t!(leg);
            let i = i!(leg);
            let r = r!(leg);
            let not_k = not_k!(leg);
            let not_g = not_g!(leg);
            let not_v = not_v!(leg);
            let not_i = not_i!(leg);
            let not_r = not_r!(leg);
            let special_v = v && not_v;

            if i && !piece_unmoved || not_i && piece_unmoved {
                valid = false;
                break;
            }

            if promotable {
                let start_mandatory = get!(
                    $state.statics.promotion_zones_mandatory[
                        piece_index as usize
                    ],
                    start_square
                );
                let start_optional = get!(
                    $state.statics.promotion_zones_optional[
                        piece_index as usize
                    ],
                    start_square
                );
                let end_mandatory = get!(
                    $state.statics.promotion_zones_mandatory[
                        piece_index as usize
                    ],
                    end_square
                );
                let end_optional = get!(
                    $state.statics.promotion_zones_optional[
                        piece_index as usize
                    ],
                    end_square
                );

                if r
                && !start_mandatory && !end_mandatory
                && !start_optional  && !end_optional
                || not_r && start_mandatory
                || not_r && end_mandatory
                {
                    valid = false;
                    break;
                }
            }

            let friendly = get!(
                $state.pieces_board[piece_color as usize],
                end_square
            ) && end_square != $square_index as u32;
            let enemy = get!(
                $state.pieces_board[1 - piece_color as usize],
                end_square
            );
            let empty = !friendly && !enemy;

            let pass_move = file_offset == 0 && rank_offset == 0;
            let imaginary_move = end_square == $attacked_square && (c || d);

            if u && target_was_last_captured {
                valid = false;
                break;
            }

            if imaginary_move {
                if k && !$attacked_royal
                || not_k && $attacked_royal
                || g && piece_rank >= $attacked_rank
                || not_g && piece_rank < $attacked_rank
                || (v && !$attacked_unmoved || not_v && $attacked_unmoved)
                && !special_v
                || u
                {
                    valid = false;
                    break;
                }

                target_was_last_captured = true;
                continue;
            }

            if empty && !pass_move {
                if t && enp_square!($state.en_passant_square) == end_square
                {
                    let capt_piece_index =
                        enp_piece!($state.en_passant_square);
                    let capt_piece_color =
                        p_color!(
                            $state.statics.pieces[capt_piece_index as usize]
                        );

                    if d && capt_piece_color == piece_color
                        || c && capt_piece_color != piece_color
                    {
                        let capt_piece =
                            &$state.statics.pieces[capt_piece_index as usize];
                        let capt_unmoved = get!(
                            $state.virgin_board,
                            enp_captured!($state.en_passant_square)
                        );
                        let capt_rank = p_rank!(capt_piece);
                        let capt_royal = p_is_royal!(capt_piece);

                        if k && !capt_royal
                        || not_k && capt_royal
                        || g && piece_rank >= capt_rank
                        || not_g && piece_rank < capt_rank
                        || (v && !capt_unmoved || not_v && capt_unmoved)
                        && !special_v
                        {
                            valid = false;
                            break;
                        }

                        target_was_last_captured = false;
                    } else {
                        valid = false;
                        break;
                    }
                } else if !m {
                    valid = false;
                    break;
                }
            } else if friendly && !pass_move {
                if !d {
                    valid = false;
                    break;
                }

                let capt_piece_index =
                    $state.main_board[end_square as usize];
                let capt_piece =
                    &$state.statics.pieces[capt_piece_index as usize];
                let capt_unmoved = get!($state.virgin_board, end_square);
                let capt_rank = p_rank!(capt_piece);
                let capt_royal = p_is_royal!(capt_piece);

                if k && !capt_royal
                || not_k && capt_royal
                || g && piece_rank >= capt_rank
                || not_g && piece_rank < capt_rank
                || (v && !capt_unmoved || not_v && capt_unmoved)
                && !special_v
                {
                    valid = false;
                    break;
                }

                target_was_last_captured = false;
            } else if enemy && !pass_move {
                if !c {
                    valid = false;
                    break;
                }

                let capt_piece_index =
                    $state.main_board[end_square as usize];
                let capt_piece =
                    &$state.statics.pieces[capt_piece_index as usize];
                let capt_unmoved = get!($state.virgin_board, end_square);
                let capt_rank = p_rank!(capt_piece);
                let capt_royal = p_is_royal!(capt_piece);

                if k && !capt_royal
                || not_k && capt_royal
                || g && piece_rank >= capt_rank
                || not_g && piece_rank < capt_rank
                || (v && !capt_unmoved || not_v && capt_unmoved)
                && !special_v
                {
                    valid = false;
                    break;
                }

                target_was_last_captured = false;
            }
        }

        valid
    }};
}

/// process_multi_leg_vector!
///
/// The core of move construction. It simulates one vector leg by leg on
/// the current board. If all legs are legal, it adds the encoded moves:
/// captures, multi-captures, unloads, en passant, castling right changes
/// and one move for each legal promotion. A blocked leg, a failed capture
/// rule or a first move rule stops it without a move.
///
/// Each leg starts at the end of the last leg. `S` is the origin, `1` and
/// `2` are leg ends, and `T` is the target. The macro tests occupancy and
/// modifiers at each leg end:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │ T  │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │ 1  │    │    │ 2  │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │ S  │    │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - vector      : &MoveVector    -> compiled vector to simulate
/// - state       : &State         -> current position
/// - out         : &mut Vec<Move> -> list that gets the moves
/// - scratch     : &mut Vec<u64>  -> reused multi-capture buffer
///
/// Notes:
/// The piece colour scales the offsets. The macro clears `scratch` first and
/// moves it into a multi-capture move only if it has extra records.
///
#[macro_export]
macro_rules! process_multi_leg_vector {
    (
        $square_index:expr, $piece:expr, $vector:expr,
        $state:expr, $out:expr, $scratch:expr
    ) => {{

        let mut invalid = false;

        let piece_index = p_index!($piece);
        let piece_color = p_color!($piece);
        let piece_rank = p_rank!($piece);
        let piece_unmoved = get!($state.virgin_board, $square_index as u32);

        let mut encoded_move = Move::default();
        enc_start!(encoded_move, $square_index as u128);
        enc_piece!(encoded_move, piece_index as u128);

        $scratch.clear();

        let mut accumulated_index = $square_index as i16;

        let leg_count = $vector.len();

        let promotable = promotions!($state) && p_can_promote!($piece);

        let origin_mandatory = promotable && get!(                              /* the square the whole move left,    */
            $state.statics.promotion_zones_mandatory[piece_index as usize],     /* which is what tells crossing into  */
            $square_index as u32                                                /* the zone from starting inside it   */
        );
        let origin_optional = promotable && get!(
            $state.statics.promotion_zones_optional[piece_index as usize],
            $square_index as u32
        );

        let mut mandatory = false;
        let mut optionals = false;

        for (leg_index, leg) in $vector.iter().enumerate() {
            let last_leg = leg_index + 1 == leg_count;
            let mut taken_piece = 0u64;

            let start_square = accumulated_index as u32;

            let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
            let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

            accumulated_index += rank_offset as i16                             /* widened: a leg reaching four       */
                * ($state.statics.files as i16) + file_offset as i16;           /* ranks on a wide board overflows    */

            let end_square = accumulated_index as u32;

            let m = m!(leg) || (!c!(leg) && !d!(leg));
            let c = c!(leg) || (last_leg && !m!(leg));
            let d = d!(leg);
            let u = u!(leg);
            let k = k!(leg);
            let g = g!(leg);
            let v = v!(leg);
            let t = t!(leg);
            let i = i!(leg);
            let p = p!(leg);
            let r = r!(leg);
            let not_k = not_k!(leg);
            let not_g = not_g!(leg);
            let not_v = not_v!(leg);
            let not_i = not_i!(leg);
            let not_r = not_r!(leg);
            let special_v = v && not_v;

            if i && !piece_unmoved || not_i && piece_unmoved {
                invalid = true;
                break;
            }

            if promotable {
                let start_mandatory = get!(
                    $state.statics.promotion_zones_mandatory[
                        piece_index as usize
                    ],
                    start_square
                );
                let start_optional = get!(
                    $state.statics.promotion_zones_optional[
                        piece_index as usize
                    ],
                    start_square
                );
                let end_mandatory = get!(
                    $state.statics.promotion_zones_mandatory[
                        piece_index as usize
                    ],
                    end_square
                );
                let end_optional = get!(
                    $state.statics.promotion_zones_optional[
                        piece_index as usize
                    ],
                    end_square
                );

                if r
                && !start_mandatory && !end_mandatory
                && !start_optional  && !end_optional
                || not_r && start_mandatory
                || not_r && end_mandatory
                {
                    invalid = true;
                    break;
                }

                let entry_mandatory = promote_on_entry!($state)
                    && end_mandatory && !origin_mandatory;
                let entry_optional = promote_on_entry!($state)
                    && end_optional && !origin_optional;
                let exit_mandatory =
                    promote_on_exit!($state) && origin_mandatory;
                let exit_optional =
                    promote_on_exit!($state) && origin_optional;

                if entry_mandatory || entry_optional
                || exit_mandatory  || exit_optional {
                    optionals = !not_r;
                }

                if r || entry_mandatory || exit_mandatory {
                    mandatory = true;
                }
            }

            enc_is_initial!(encoded_move, (i | piece_unmoved) as u128);

            let friendly = get!(
                $state.pieces_board[piece_color as usize], end_square
            ) && end_square != $square_index as u32;
            let enemy = get!(
                $state.pieces_board[1 - piece_color as usize], end_square
            );
            let empty = !friendly && !enemy;

            let pass_move = file_offset == 0 && rank_offset == 0;

            if empty && !pass_move {
                if t && enp_square!($state.en_passant_square)
                    == end_square
                {
                    let capt_piece_index =
                        enp_piece!($state.en_passant_square);
                    let capt_piece_color = p_color!(
                        $state.statics.pieces[capt_piece_index as usize]
                    );

                    if d && capt_piece_color == piece_color
                        || c && capt_piece_color != piece_color
                    {
                        enc_multi_move_captured_piece!(
                            taken_piece, capt_piece_index as u64
                        );
                        enc_multi_move_captured_square!(
                            taken_piece,
                            enp_captured!($state.en_passant_square) as u64
                        );

                        let capt_piece = &$state.statics.pieces[
                            capt_piece_index as usize
                        ];
                        let capt_unmoved = get!(
                            $state.virgin_board,
                            enp_captured!($state.en_passant_square)
                        );
                        let capt_rank = p_rank!(capt_piece);
                        let capt_royal = p_is_royal!(capt_piece);

                        if k || not_k && capt_royal
                        || g && piece_rank >= capt_rank
                        || not_g && piece_rank < capt_rank
                        || (v && !capt_unmoved || not_v && capt_unmoved)
                        && !special_v
                        {
                            invalid = true;
                            break;
                        }

                        enc_multi_move_captured_unmoved!(
                            taken_piece, capt_unmoved as u64
                        );
                        enc_multi_move_captured_own!(
                            taken_piece,
                            (capt_piece_color == piece_color) as u64
                        );

                        $scratch.push(taken_piece);
                    } else {
                        invalid = true;
                        break;
                    }
                } else if !m {
                    invalid = true;
                    break;
                }
            } else if friendly && !pass_move {
                if !d {
                    invalid = true;
                    break;
                }

                let capt_piece_index =
                    $state.main_board[end_square as usize];
                let capt_piece = &$state.statics.pieces[
                    capt_piece_index as usize
                ];
                let capt_unmoved =
                    get!($state.virgin_board, end_square);
                let capt_rank = p_rank!(capt_piece);
                let capt_royal = p_is_royal!(capt_piece);

                if k || not_k && capt_royal
                || g && piece_rank >= capt_rank
                || not_g && piece_rank < capt_rank
                || (v && !capt_unmoved || not_v && capt_unmoved)
                && !special_v
                {
                    invalid = true;
                    break;
                }

                enc_multi_move_captured_piece!(
                    taken_piece, capt_piece_index as u64
                );
                enc_multi_move_captured_square!(
                    taken_piece, end_square as u64
                );
                enc_multi_move_captured_unmoved!(
                    taken_piece, capt_unmoved as u64
                );
                enc_multi_move_captured_own!(taken_piece, 1);                   /* only a destroying leg reaches here */

                $scratch.push(taken_piece);
            } else if enemy && !pass_move {
                if !c {
                    invalid = true;
                    break;
                }

                let capt_piece_index =
                    $state.main_board[end_square as usize];
                let capt_piece = &$state.statics.pieces[
                    capt_piece_index as usize
                ];
                let capt_unmoved =
                    get!($state.virgin_board, end_square);
                let capt_rank = p_rank!(capt_piece);
                let capt_royal = p_is_royal!(capt_piece);

                if k || not_k && capt_royal
                || g && piece_rank >= capt_rank
                || not_g && piece_rank < capt_rank
                || (v && !capt_unmoved || not_v && capt_unmoved)
                && !special_v
                {
                    invalid = true;
                    break;
                }

                enc_multi_move_captured_piece!(
                    taken_piece, capt_piece_index as u64
                );
                enc_multi_move_captured_square!(
                    taken_piece, end_square as u64
                );
                enc_multi_move_captured_unmoved!(
                    taken_piece, capt_unmoved as u64
                );

                $scratch.push(taken_piece);
            }

            if u {
                let mut last_captured =
                    $scratch.pop().unwrap_or_else(|| {
                        panic!(
                            "Unload flag is set but no captured piece \
                             is available"
                        )
                    });
                let captured_square =
                    multi_move_captured_square!(last_captured);

                if start_square != captured_square as u32 {
                    enc_multi_move_is_unload!(last_captured, 1);
                    enc_multi_move_unload_square!(
                        last_captured, start_square as u64
                    );

                    $scratch.push(last_captured);
                }
            }

            enc_creates_enp!(encoded_move, p as u128);
            enc_created_enp!(
                encoded_move,
                p as u128
                    * (
                        (start_square as u128 & 0x7FF) |
                        (accumulated_index as u128) << 11 |
                        (piece_index as u128) << 22
                    )
            );
        }


        if !invalid {
            enc_end!(encoded_move, accumulated_index as u128);

            if $scratch.is_empty() {
                enc_move_type!(encoded_move, QUIET_MOVE);
            } else if $scratch.len() == 1 {
                enc_move_type!(encoded_move, SINGLE_CAPTURE_MOVE);
                enc_capture_part!(encoded_move, $scratch[0] as u128);
            } else {
                enc_move_type!(encoded_move, MULTI_CAPTURE_MOVE);
                encoded_move.1 = Some(Arc::new(mem::take($scratch)));
            }

            if mandatory || optionals {
                for promo_piece_index in &$piece.promotions {
                    let mut can_promote = true;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.statics.piece_swap_map[
                            *promo_piece_index as usize
                        ];

                        can_promote &= $state.piece_in_hand
                            [1 - piece_color as usize]
                            [enemy_equiv as usize] > 0;
                    }

                    if can_promote {
                        let mut promo_move = encoded_move.clone();

                        enc_promotion!(promo_move, 1);
                        enc_promoted!(
                            promo_move, *promo_piece_index as u128
                        );

                        $out.push(promo_move);
                    }
                }
            }

            if !mandatory {
                $out.push(encoded_move);
            }
        }
    }};
}

/// generate_move_list_from_vectors!
///
/// Generates all pseudo-legal moves of `$piece` from `$square_index` for
/// each vector of `$vector_set`, with `process_multi_leg_vector!`. The
/// two list macros give the source table:
///
/// - `relevant_moves`    : full pseudo-legal move list
/// - `relevant_captures` : capture list
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - vector_set  : &MoveSet       -> compiled vectors to expand
/// - state       : &State         -> current position
/// - out         : &mut Vec<Move> -> list that gets the moves
/// - scratch     : &mut Vec<u64>  -> reused multi-capture buffer
///
#[macro_export]
macro_rules! generate_move_list_from_vectors {
    (
        $square_index:expr, $piece:expr, $vector_set:expr,
        $state:expr, $out:expr, $scratch:expr
    ) => {{
        for multi_leg_vector in $vector_set {
            process_multi_leg_vector!(
                $square_index, $piece, multi_leg_vector,
                $state, $out, $scratch
            );
        }
    }};
}

/// generate_move_list!
///
/// Generates all pseudo-legal moves of `$piece` from `$square_index`, from
/// its `relevant_moves` vectors, and adds them to `$out`.
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - state       : &State         -> current position
/// - out         : &mut Vec<Move> -> list that gets the moves
/// - scratch     : &mut Vec<u64>  -> reused multi-capture buffer
///
#[macro_export]
macro_rules! generate_move_list {
    (
        $square_index:expr, $piece:expr, $state:expr, $out:expr, $scratch:expr
    ) => {{
        let piece_index = p_index!($piece) as usize;
        let board_size = $state.statics.board_size;
        let vector_set =
            &$state.statics.relevant_moves
                [piece_index * board_size + $square_index as usize];

        generate_move_list_from_vectors!(
            $square_index, $piece, vector_set, $state, $out, $scratch
        )
    }};
}

/// generate_capture_list!
///
/// Generates only the pseudo-legal captures of `$piece` from
/// `$square_index`. It uses `relevant_captures` and then removes the moves
/// that do not capture.
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - state       : &State         -> current position
/// - out         : &mut Vec<Move> -> list that gets the captures
/// - scratch     : &mut Vec<u64>  -> reused multi-capture buffer
///
/// Notes:
/// The two tables use the same stable sort, so the captures have the same
/// order as in `generate_move_list!`. The staged generation of `alpha_beta`
/// needs this.
///
#[macro_export]
macro_rules! generate_capture_list {
    (
        $square_index:expr, $piece:expr, $state:expr, $out:expr, $scratch:expr
    ) => {{
        let piece_index = p_index!($piece) as usize;
        let board_size = $state.statics.board_size;
        let vector_set =
            &$state.statics.relevant_captures
                [piece_index * board_size + $square_index as usize];

        let start = $out.len();
        generate_move_list_from_vectors!(
            $square_index, $piece, vector_set, $state, $out, $scratch
        );

        retain_captures!($out, start, true);
    }};
}

/// retain_captures!
///
/// Keeps in `$out[$start..]` only the moves whose capture status is
/// `$keep`, in their order. It splits one generation pass into captures
/// and quiet moves.
///
/// Params:
/// - out  : &mut Vec<Move> -> list with the tail to compact
/// - start: usize          -> first index of the filter
/// - keep : bool           -> true keeps captures, false keeps quiets
///
#[macro_export]
macro_rules! retain_captures {
    ($out:expr, $start:expr, $keep:expr) => {{
        let mut write = $start;

        for read in $start..$out.len() {
            if m_capture!(&$out[read]) == $keep {
                $out.swap(write, read);
                write += 1;
            }
        }

        $out.truncate(write);
    }};
}

/// generate_castling_list!
///
/// Adds the legal castling moves of the side to move. Each precomputed
/// castling move needs:
///
/// - the right of that side and wing
/// - the two pieces unmoved on their start squares
/// - empty end and path squares
/// - no attack on each `*` square of the move list
///
/// Params:
/// - state: &State         -> current position with rights and occupancy
/// - out  : &mut Vec<Move> -> list that gets the castling moves
///
#[macro_export]
macro_rules! generate_castling_list {
    (
        $state:expr, $out:expr
    ) => {{
        let color = $state.playing as usize;
        let indexs = [(WK_INDEX, WQ_INDEX), (BK_INDEX, BQ_INDEX)]
            [color];
        let rights = [(WK_CASTLE, WQ_CASTLE), (BK_CASTLE, BQ_CASTLE)]
            [color];

        if $state.castling_state & rights.0 != 0 {
            let moves = &$state.statics.relevant_castling[indexs.0 as usize];

            for mv in moves {

                let piece = &$state.statics.pieces[piece!(mv) as usize];
                let piece_rank = p_rank!(piece);

                let start = start!(mv) as usize;
                let end = end!(mv) as usize;

                let piece_index = piece!(mv) as PieceIndex;
                let captured_piece = captured_piece!(mv) as PieceIndex;
                let captured_square = captured_square!(mv) as usize;
                let unload_square = unload_square!(mv) as usize;

                if $state.main_board[start] != piece_index
                || $state.main_board[end] != NO_PIECE
                || $state.main_board[captured_square] != captured_piece
                || $state.main_board[unload_square] != NO_PIECE
                || is_square_attacked!(
                    start as u32,
                    color as u8,
                    true,
                    true,
                    piece_rank,
                    $state
                )
                || is_square_attacked!(
                    end as u32,
                    color as u8,
                    true,
                    true,
                    piece_rank,
                    $state
                ) {
                    continue;
                }

                if m_captures!(mv).iter().all(
                    |cap|
                    {
                        let square = multi_move_unload_square!(cap) as usize;
                        let attack = multi_move_is_unload!(cap) as bool;

                        $state.main_board[square] == NO_PIECE &&
                        (
                            !attack ||
                            !is_square_attacked!(
                                square as u32,
                                color as u8,
                                true,
                                true,
                                piece_rank,
                                $state
                            )
                        )
                    }
                ) {
                    $out.push(mv.clone())
                }
            }
        }

        if $state.castling_state & rights.1 != 0 {
            let moves = &$state.statics.relevant_castling[indexs.1 as usize];

            for mv in moves {

                let piece = &$state.statics.pieces[piece!(mv) as usize];
                let piece_rank = p_rank!(piece);

                let start = start!(mv) as usize;
                let end = end!(mv) as usize;

                let piece_index = piece!(mv) as PieceIndex;
                let captured_piece = captured_piece!(mv) as PieceIndex;
                let captured_square = captured_square!(mv) as usize;
                let unload_square = unload_square!(mv) as usize;

                if $state.main_board[start] != piece_index
                || $state.main_board[end] != NO_PIECE
                || $state.main_board[captured_square] != captured_piece
                || $state.main_board[unload_square] != NO_PIECE
                || is_square_attacked!(
                    start as u32,
                    color as u8,
                    true,
                    true,
                    piece_rank,
                    $state
                )
                || is_square_attacked!(
                    end as u32,
                    color as u8,
                    true,
                    true,
                    piece_rank,
                    $state
                ) {
                    continue;
                }

                if m_captures!(mv).iter().all(
                    |cap|
                    {
                        let square = multi_move_unload_square!(cap) as usize;
                        let attack = multi_move_is_unload!(cap) as bool;

                        $state.main_board[square] == NO_PIECE &&
                        (
                            !attack ||
                            !is_square_attacked!(
                                square as u32,
                                color as u8,
                                true,
                                true,
                                piece_rank,
                                $state
                            )
                        )
                    }
                ) {
                    $out.push(mv.clone())
                }
            }
        }

    }};
}

/*----------------------------------------------------------------------------*\
                          MOVE STATE TRANSITION MACROS
\*----------------------------------------------------------------------------*/

/// make_move!
///
/// Applies a move to the game state with all incremental updates:
///
/// - increments the ply counters
/// - updates occupancy, piece lists, unmoved flags, castling, en passant
/// - handles quiet, capture, multi-capture, unload, promotion and drop
/// - updates material, piece role counts and hands
/// - updates the Zobrist keys and the end rule progress
/// - pushes a [`Snapshot`] and rejects a move that leaves own check
///
/// ```text
/// save before-state -> apply -> push Snapshot -> legal?
///                                                | yes: pass
///                                                | no : undo move
/// ```
///
/// Params:
/// - state: &mut State -> position to change
/// - mv   : Move       -> encoded move to play
///
/// Return:
/// bool                -> true if legal, false after the rollback
///
/// Notes:
/// Call `undo_move!` only after true. After false, the position is already
/// restored and the snapshot is removed.
///
#[macro_export]
macro_rules! make_move {
    ($state:expr, $mv:expr) => {
        hotpath::measure_block!("state::make_move", {
            let applied_move: Move = $mv;                                       /* bind once: $mv expands per use     */

            #[cfg(debug_assertions)]
            verify_game_state($state);

            $state.search_ply += 1;
            $state.ply_counter += 1;

            let last_en_passant_square = $state.en_passant_square;
            let last_halfmove_clock = $state.termination.counter
                .as_ref().map_or(0, |counter| counter.clock);
            let last_repetition_clock = $state.termination.repetition
                .as_ref().map_or(0, |repetition| repetition.clock);
            let last_counting = $state.termination.counting
                .as_ref().and_then(|counting| counting.progress);
            let last_castling_state = $state.castling_state;
            let last_position_hash = $state.position_hash;
            let last_virgin_hash = $state.virgin_hash;
            let last_pawn_hash = $state.pawn_hash;
            let last_game_result = $state.termination.game_result;
            let last_check_count = $state.termination.checks
                .as_ref().map_or([0; 2], |checks| checks.delivered);
            let last_game_phase = $state.game_phase;
            let last_phase_score = $state.phase_score;

            let move_type = move_type!(applied_move);
            let piece_index = piece!(applied_move) as usize;
            let creates_enp = creates_enp!(applied_move);
            let enp_square = created_enp!(applied_move) as u32;
            let pass_move = is_pass!(applied_move);

            let stand_off_before = if stand_offs!($state) {
                $state.history.last()
                    .and_then(|snapshot| snapshot.in_stand_off)
                    .unwrap_or_else(|| is_in_stand_off!($state))
            } else {
                false
            };

            if move_type == QUIET_MOVE {
                let start_square = start!(applied_move) as u32;
                let end_square = end!(applied_move) as u32;
                let is_promotion = promotion!(applied_move);
                let promoted_piece = promoted!(applied_move) as usize;

                let piece_color =
                    p_color!($state.statics.pieces[piece_index]);
                let piece_unmoved =
                    get!($state.virgin_board, start_square);

                clear!(
                    $state.pieces_board[piece_color as usize],
                    start_square
                );
                set!(
                    $state.pieces_board[piece_color as usize],
                    end_square
                );

                if p_is_royal!($state.statics.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != start_square
                    );
                }

                if p_is_royal!($state.statics.pieces[                           /* the piece that lands is the one    */
                    if is_promotion { promoted_piece } else { piece_index }     /* the list must name, and promoting  */
                ]) {                                                            /* into royalty makes the two differ  */
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear_virgin!($state, start_square);

                hash_in_or_out_piece!(
                    $state, piece_index, start_square as Square
                );
                hash_in_or_out_piece!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                $state.main_board[start_square as usize] = NO_PIECE;
                $state.main_board[end_square as usize] =
                    if is_promotion { promoted_piece as PieceIndex }
                    else { piece_index as PieceIndex };

                $state.opening_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_opening
                    [piece_index][start_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_endgame
                    [piece_index][start_square as usize];
                $state.opening_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_opening[
                        if is_promotion { promoted_piece } else { piece_index }
                    ][end_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_endgame[
                        if is_promotion { promoted_piece } else { piece_index }
                    ][end_square as usize];

                if is_promotion {
                    let old_piece = &$state.statics.pieces[piece_index];
                    let new_piece = &$state.statics.pieces[promoted_piece];

                    $state.big_pieces[piece_color as usize] -=
                        p_is_big!(old_piece) as u32;
                    $state.major_pieces[piece_color as usize] -=
                        p_is_major!(old_piece) as u32;
                    $state.minor_pieces[piece_color as usize] -=
                        p_is_minor!(old_piece) as u32;

                    $state.big_pieces[piece_color as usize] +=
                        p_is_big!(new_piece) as u32;
                    $state.major_pieces[piece_color as usize] +=
                        p_is_major!(new_piece) as u32;
                    $state.minor_pieces[piece_color as usize] +=
                        p_is_minor!(new_piece) as u32;

                    $state.opening_material[piece_color as usize] -=
                        p_ovalue!(old_piece) as u32;
                    $state.endgame_material[piece_color as usize] -=
                        p_evalue!(old_piece) as u32;
                    $state.opening_material[piece_color as usize] +=
                        p_ovalue!(new_piece) as u32;
                    $state.endgame_material[piece_color as usize] +=
                        p_evalue!(new_piece) as u32;

                    $state.phase_score -= p_ovalue!(
                        old_piece
                    ) as u32 * p_is_big!(
                        old_piece
                    ) as u32 * !p_is_royal!(
                        old_piece
                    ) as u32;

                    $state.phase_score += p_ovalue!(
                        new_piece
                    ) as u32 * p_is_big!(
                        new_piece
                    ) as u32 * !p_is_royal!(
                        new_piece
                    ) as u32;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.statics.piece_swap_map
                            [promoted_piece];

                        let hand = &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];

                        hash_update_in_hand!(
                            $state,
                            enemy_equiv as usize,
                            *hand,
                            *hand - 1
                        );

                        *hand -= 1;

                        if drops!($state) {
                            let opponent = 1 - piece_color as usize;

                            $state.opening_material[opponent] -=
                                p_ovalue!(
                                    $state.statics.pieces
                                        [enemy_equiv as usize]
                                ) as u32;
                            $state.endgame_material[opponent] -=
                                p_evalue!(
                                    $state.statics.pieces
                                        [enemy_equiv as usize]
                                ) as u32;
                        }
                    }
                }

                piece_list_remove!($state, piece_index, start_square as Square);
                piece_list_push!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                if piece_unmoved && $state.statics.castling_pieces[piece_index]
                {
                    $state.castling_state &=
                        [
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [WK_INDEX as usize],
                                    start_square
                                ) as u8 *
                                WK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [WQ_INDEX as usize],
                                    start_square
                                ) as u8 *
                                WQ_CASTLE
                            },
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [BK_INDEX as usize],
                                    start_square
                                ) as u8 *
                                BK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [BQ_INDEX as usize],
                                    start_square
                                ) as u8 *
                                BQ_CASTLE
                            }
                        ][piece_color as usize]
                }

                hash_update_castling!(
                    $state, last_castling_state, $state.castling_state
                );

            } else if move_type == SINGLE_CAPTURE_MOVE {
                let start_square = start!(applied_move) as u32;
                let end_square = end!(applied_move) as u32;
                let is_promotion = promotion!(applied_move);
                let promoted_piece = promoted!(applied_move) as usize;
                let captured_piece = captured_piece!(applied_move) as usize;
                let captured_square = captured_square!(applied_move) as u32;
                let is_unload = is_unload!(applied_move);
                let unload_square = unload_square!(applied_move) as u32;

                let piece_color = p_color!($state.statics.pieces[piece_index]);
                let piece_unmoved = get!(
                    $state.virgin_board, start_square
                );
                let captured_color = p_color!(
                    $state.statics.pieces[captured_piece]
                );

                clear!(
                    $state.pieces_board[piece_color as usize], start_square
                );
                set!(
                    $state.pieces_board[piece_color as usize], end_square
                );

                if p_is_royal!($state.statics.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != start_square
                    );
                }

                if p_is_royal!($state.statics.pieces[                           /* the piece that lands is the one    */
                    if is_promotion { promoted_piece } else { piece_index }     /* the list must name, and promoting  */
                ]) {                                                            /* into royalty makes the two differ  */
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear_virgin!($state, start_square);

                hash_in_or_out_piece!(
                    $state, piece_index, start_square as Square
                );
                hash_in_or_out_piece!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                $state.main_board[start_square as usize] = NO_PIECE;
                $state.main_board[end_square as usize] =
                    if is_promotion { promoted_piece as PieceIndex }
                    else { piece_index as PieceIndex };

                $state.opening_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_opening
                    [piece_index][start_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_endgame
                    [piece_index][start_square as usize];
                $state.opening_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_opening[
                        if is_promotion { promoted_piece } else { piece_index }
                    ][end_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_endgame[
                        if is_promotion { promoted_piece } else { piece_index }
                    ][end_square as usize];

                if is_promotion {
                    let old_piece = &$state.statics.pieces[piece_index];
                    let new_piece = &$state.statics.pieces[promoted_piece];

                    $state.big_pieces[piece_color as usize] -=
                        p_is_big!(old_piece) as u32;
                    $state.major_pieces[piece_color as usize] -=
                        p_is_major!(old_piece) as u32;
                    $state.minor_pieces[piece_color as usize] -=
                        p_is_minor!(old_piece) as u32;

                    $state.big_pieces[piece_color as usize] +=
                        p_is_big!(new_piece) as u32;
                    $state.major_pieces[piece_color as usize] +=
                        p_is_major!(new_piece) as u32;
                    $state.minor_pieces[piece_color as usize] +=
                        p_is_minor!(new_piece) as u32;

                    $state.opening_material[piece_color as usize] -=
                        p_ovalue!(old_piece) as u32;
                    $state.endgame_material[piece_color as usize] -=
                        p_evalue!(old_piece) as u32;
                    $state.opening_material[piece_color as usize] +=
                        p_ovalue!(new_piece) as u32;
                    $state.endgame_material[piece_color as usize] +=
                        p_evalue!(new_piece) as u32;

                    $state.phase_score -= p_ovalue!(
                        old_piece
                    ) as u32 * p_is_big!(
                        old_piece
                    ) as u32 * !p_is_royal!(
                        old_piece
                    ) as u32;

                    $state.phase_score += p_ovalue!(
                        new_piece
                    ) as u32 * p_is_big!(
                        new_piece
                    ) as u32 * !p_is_royal!(
                        new_piece
                    ) as u32;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.statics.piece_swap_map
                            [promoted_piece];

                        let hand = &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];

                        hash_update_in_hand!(
                            $state,
                            enemy_equiv as usize,
                            *hand,
                            *hand - 1
                        );

                        *hand -= 1;

                        if drops!($state) {
                            let opponent = 1 - piece_color as usize;

                            $state.opening_material[opponent] -=
                                p_ovalue!(
                                    $state.statics.pieces
                                        [enemy_equiv as usize]
                                ) as u32;
                            $state.endgame_material[opponent] -=
                                p_evalue!(
                                    $state.statics.pieces
                                        [enemy_equiv as usize]
                                ) as u32;
                        }
                    }
                }

                piece_list_remove!($state, piece_index, start_square as Square);
                piece_list_push!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                if piece_unmoved && $state.statics.castling_pieces[piece_index]
                {
                    $state.castling_state &=
                        [
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [WK_INDEX as usize],
                                    start_square
                                ) as u8 *
                                WK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [WQ_INDEX as usize],
                                    start_square
                                ) as u8 *
                                WQ_CASTLE
                            },
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [BK_INDEX as usize],
                                    start_square
                                ) as u8 *
                                BK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [BQ_INDEX as usize],
                                    start_square
                                ) as u8 *
                                BQ_CASTLE
                            }
                        ][piece_color as usize]
                }

                if castling!($state) {
                    $state.castling_state &= [
                        !{
                            get!(
                                $state.statics.critical_castling
                                [WK_INDEX as usize],
                                captured_square
                            ) as u8 *
                            WK_CASTLE
                            |
                            get!(
                                $state.statics.critical_castling
                                [WQ_INDEX as usize],
                                captured_square
                            ) as u8 *
                            WQ_CASTLE
                        },
                        !{
                            get!(
                                $state.statics.critical_castling
                                [BK_INDEX as usize],
                                captured_square
                            ) as u8 *
                            BK_CASTLE
                            |
                            get!(
                                $state.statics.critical_castling
                                [BQ_INDEX as usize],
                                captured_square
                            ) as u8 *
                            BQ_CASTLE
                        }
                    ][captured_color as usize];
                }

                if drops!($state) || promote_to_captured!($state) {

                    let demoted_piece = $state.statics.piece_demotion_map
                        [captured_piece] as usize;

                    let hand_piece = $state.statics.piece_swap_map
                        [demoted_piece] as usize;

                    let hand = &mut $state.piece_in_hand
                        [piece_color as usize][hand_piece];

                    hash_update_in_hand!(
                        $state,
                        hand_piece,
                        *hand,
                        *hand + 1
                    );

                    *hand += 1;

                    if drops!($state) {
                        $state.opening_material[piece_color as usize] +=
                            p_ovalue!(
                                $state.statics.pieces[hand_piece]
                            ) as u32;
                        $state.endgame_material[piece_color as usize] +=
                            p_evalue!(
                                $state.statics.pieces[hand_piece]
                            ) as u32;
                    }
                }

                hash_update_castling!(
                    $state, last_castling_state, $state.castling_state
                );

                if captured_square != end_square {
                    $state.main_board[captured_square as usize] =
                        NO_PIECE;
                }

                if captured_square != end_square
                || captured_color != piece_color {
                    clear!(
                        $state.pieces_board[captured_color as usize],
                        captured_square
                    );
                }

                hash_in_or_out_piece!(
                    $state,
                    captured_piece,
                    captured_square as Square
                );

                clear_virgin!($state, captured_square);

                if is_unload {
                    set!(
                        $state.pieces_board[captured_color as usize],
                        unload_square
                    );

                    hash_in_or_out_piece!(
                        $state,
                        captured_piece,
                        unload_square as Square
                    );

                    set_virgin!($state, unload_square);
                }

                if is_unload {
                    $state.main_board[unload_square as usize] =
                        captured_piece as PieceIndex;
                    piece_list_remove!(
                        $state, captured_piece, captured_square as Square
                    );
                    piece_list_push!(
                        $state, captured_piece, unload_square as Square
                    );

                    $state.opening_pst_bonus[captured_color as usize] -=
                        $state.statics.pst_opening[captured_piece]
                        [captured_square as usize];
                    $state.endgame_pst_bonus[captured_color as usize] -=
                        $state.statics.pst_endgame[captured_piece]
                        [captured_square as usize];
                    $state.opening_pst_bonus[captured_color as usize] +=
                        $state.statics.pst_opening[captured_piece]
                        [unload_square as usize];
                    $state.endgame_pst_bonus[captured_color as usize] +=
                        $state.statics.pst_endgame[captured_piece]
                        [unload_square as usize];
                } else {
                    piece_list_remove!(
                        $state, captured_piece, captured_square as Square
                    );
                }

                if p_is_royal!($state.statics.pieces[captured_piece]) {
                    $state.royal_list[captured_color as usize].retain(
                        |&sq| sq as u32 != captured_square
                    );

                    if is_unload {
                        $state.royal_list[captured_color as usize]
                            .push(unload_square as Square);
                    }
                }

                if !is_unload {
                    $state.opening_pst_bonus[captured_color as usize] -=
                        $state.statics.pst_opening[captured_piece]
                        [captured_square as usize];
                    $state.endgame_pst_bonus[captured_color as usize] -=
                        $state.statics.pst_endgame[captured_piece]
                        [captured_square as usize];

                    $state.big_pieces[captured_color as usize] -=
                        p_is_big!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;
                    $state.major_pieces[captured_color as usize] -=
                        p_is_major!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;
                    $state.minor_pieces[captured_color as usize] -=
                        p_is_minor!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;

                    $state.opening_material[captured_color as usize] -=
                        p_ovalue!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;
                    $state.endgame_material[captured_color as usize] -=
                        p_evalue!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;

                    $state.phase_score -= p_ovalue!(
                        $state.statics.pieces[captured_piece]
                    ) as u32 * p_is_big!(
                        $state.statics.pieces[captured_piece]
                    ) as u32 * !p_is_royal!(
                        $state.statics.pieces[captured_piece]
                    ) as u32;

                }
            } else if move_type == MULTI_CAPTURE_MOVE {
                let start_square = start!(applied_move) as u32;
                let end_square = end!(applied_move) as u32;
                let is_promotion = promotion!(applied_move);
                let promoted_piece = promoted!(applied_move) as usize;

                let piece_color = p_color!($state.statics.pieces[piece_index]);
                let piece_unmoved = get!(
                    $state.virgin_board, start_square
                );

                clear!(
                    $state.pieces_board[piece_color as usize], start_square
                );
                set!(
                    $state.pieces_board[piece_color as usize], end_square
                );

                if p_is_royal!($state.statics.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != start_square
                    );
                }

                if p_is_royal!($state.statics.pieces[                           /* the piece that lands is the one    */
                    if is_promotion { promoted_piece } else { piece_index }     /* the list must name, and promoting  */
                ]) {                                                            /* into royalty makes the two differ  */
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear_virgin!($state, start_square);

                hash_in_or_out_piece!(
                    $state, piece_index, start_square as Square
                );
                hash_in_or_out_piece!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                $state.main_board[start_square as usize] = NO_PIECE;
                $state.main_board[end_square as usize] =
                    if is_promotion { promoted_piece as PieceIndex }
                    else { piece_index as PieceIndex };

                $state.opening_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_opening
                    [piece_index][start_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_endgame
                    [piece_index][start_square as usize];
                $state.opening_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_opening[
                        if is_promotion { promoted_piece } else { piece_index }
                    ][end_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_endgame[
                        if is_promotion { promoted_piece } else { piece_index }
                    ][end_square as usize];

                if is_promotion {
                    let old_piece = &$state.statics.pieces[piece_index];
                    let new_piece = &$state.statics.pieces[promoted_piece];

                    $state.big_pieces[piece_color as usize] -=
                        p_is_big!(old_piece) as u32;
                    $state.major_pieces[piece_color as usize] -=
                        p_is_major!(old_piece) as u32;
                    $state.minor_pieces[piece_color as usize] -=
                        p_is_minor!(old_piece) as u32;

                    $state.big_pieces[piece_color as usize] +=
                        p_is_big!(new_piece) as u32;
                    $state.major_pieces[piece_color as usize] +=
                        p_is_major!(new_piece) as u32;
                    $state.minor_pieces[piece_color as usize] +=
                        p_is_minor!(new_piece) as u32;

                    $state.opening_material[piece_color as usize] -=
                        p_ovalue!(old_piece) as u32;
                    $state.endgame_material[piece_color as usize] -=
                        p_evalue!(old_piece) as u32;
                    $state.opening_material[piece_color as usize] +=
                        p_ovalue!(new_piece) as u32;
                    $state.endgame_material[piece_color as usize] +=
                        p_evalue!(new_piece) as u32;

                    $state.phase_score -= p_ovalue!(
                        old_piece
                    ) as u32 * p_is_big!(
                        old_piece
                    ) as u32 * !p_is_royal!(
                        old_piece
                    ) as u32;

                    $state.phase_score += p_ovalue!(
                        new_piece
                    ) as u32 * p_is_big!(
                        new_piece
                    ) as u32 * !p_is_royal!(
                        new_piece
                    ) as u32;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.statics.piece_swap_map
                            [promoted_piece];

                        let hand = &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];

                        hash_update_in_hand!(
                            $state,
                            enemy_equiv as usize,
                            *hand,
                            *hand - 1
                        );

                        *hand -= 1;

                        if drops!($state) {
                            let opponent = 1 - piece_color as usize;

                            $state.opening_material[opponent] -=
                                p_ovalue!(
                                    $state.statics.pieces
                                        [enemy_equiv as usize]
                                ) as u32;
                            $state.endgame_material[opponent] -=
                                p_evalue!(
                                    $state.statics.pieces
                                        [enemy_equiv as usize]
                                ) as u32;
                        }
                    }
                }

                piece_list_remove!($state, piece_index, start_square as Square);
                piece_list_push!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                if piece_unmoved && $state.statics.castling_pieces[piece_index]
                {
                    $state.castling_state &=
                        [
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [WK_INDEX as usize],
                                    start_square
                                ) as u8 *
                                WK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [WQ_INDEX as usize],
                                    start_square
                                ) as u8 *
                                WQ_CASTLE
                            },
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [BK_INDEX as usize],
                                    start_square
                                ) as u8 *
                                BK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [BQ_INDEX as usize],
                                    start_square
                                ) as u8 *
                                BQ_CASTLE
                            }
                        ][piece_color as usize]
                }

                for cap in m_captures!(applied_move).iter() {
                    let captured_piece =
                        multi_move_captured_piece!(cap) as usize;
                    let captured_square =
                        multi_move_captured_square!(cap) as u32;
                    let is_unload = multi_move_is_unload!(cap);
                    let unload_square = multi_move_unload_square!(cap) as u32;
                    let captured_color = p_color!(
                        $state.statics.pieces[captured_piece]
                    );

                    if castling!($state) {
                        $state.castling_state &= [
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [WK_INDEX as usize],
                                    captured_square
                                ) as u8 *
                                WK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [WQ_INDEX as usize],
                                    captured_square
                                ) as u8 *
                                WQ_CASTLE
                            },
                            !{
                                get!(
                                    $state.statics.critical_castling
                                    [BK_INDEX as usize],
                                    captured_square
                                ) as u8 *
                                BK_CASTLE
                                |
                                get!(
                                    $state.statics.critical_castling
                                    [BQ_INDEX as usize],
                                    captured_square
                                ) as u8 *
                                BQ_CASTLE
                            }
                        ][captured_color as usize];
                    }

                    if drops!($state) || promote_to_captured!($state) {

                        let demoted_piece = $state.statics.piece_demotion_map
                            [captured_piece] as usize;
                        let hand_piece = $state.statics.piece_swap_map
                            [demoted_piece] as usize;

                        let hand = &mut $state.piece_in_hand
                            [piece_color as usize][hand_piece];

                        hash_update_in_hand!(
                            $state,
                            hand_piece,
                            *hand,
                            *hand + 1
                        );

                        *hand += 1;

                        if drops!($state) {
                            $state.opening_material[piece_color as usize] +=
                                p_ovalue!(
                                    $state.statics.pieces[hand_piece]
                                ) as u32;
                            $state.endgame_material[piece_color as usize] +=
                                p_evalue!(
                                    $state.statics.pieces[hand_piece]
                                ) as u32;
                        }
                    }

                    hash_update_castling!(
                        $state, last_castling_state,
                        $state.castling_state
                    );

                    if captured_square != end_square {
                        $state.main_board[captured_square as usize] =
                            NO_PIECE;
                    }

                    if captured_square != end_square
                    || captured_color != piece_color {
                        clear!(
                            $state.pieces_board[captured_color as usize],
                            captured_square
                        );
                    }

                    hash_in_or_out_piece!(
                        $state,
                        captured_piece,
                        captured_square as Square
                    );

                    clear_virgin!($state, captured_square);

                    if is_unload {
                        set!(
                            $state.pieces_board[captured_color as usize],
                            unload_square
                        );

                        hash_in_or_out_piece!(
                            $state,
                            captured_piece,
                            unload_square as Square
                        );

                        set_virgin!($state, unload_square);
                    }

                    if is_unload {
                        $state.main_board[unload_square as usize] =
                            captured_piece as PieceIndex;
                        piece_list_remove!(
                            $state, captured_piece, captured_square as Square
                        );
                        piece_list_push!(
                            $state, captured_piece, unload_square as Square
                        );

                        $state.opening_pst_bonus[captured_color as usize] -=
                            $state.statics.pst_opening[captured_piece]
                            [captured_square as usize];
                        $state.endgame_pst_bonus[captured_color as usize] -=
                            $state.statics.pst_endgame[captured_piece]
                            [captured_square as usize];
                        $state.opening_pst_bonus[captured_color as usize] +=
                            $state.statics.pst_opening[captured_piece]
                            [unload_square as usize];
                        $state.endgame_pst_bonus[captured_color as usize] +=
                            $state.statics.pst_endgame[captured_piece]
                            [unload_square as usize];
                    } else {
                        piece_list_remove!(
                            $state, captured_piece, captured_square as Square
                        );
                    }

                    if p_is_royal!($state.statics.pieces[captured_piece]) {
                        $state.royal_list[captured_color as usize].retain(
                            |&sq| sq as u32 != captured_square
                        );

                        if is_unload {
                            $state.royal_list[captured_color as usize]
                                .push(unload_square as Square);
                        }
                    }

                    if !is_unload {
                        $state.opening_pst_bonus[captured_color as usize] -=
                            $state.statics.pst_opening[captured_piece]
                            [captured_square as usize];
                        $state.endgame_pst_bonus[captured_color as usize] -=
                            $state.statics.pst_endgame[captured_piece]
                            [captured_square as usize];

                        $state.big_pieces[captured_color as usize] -=
                            p_is_big!(
                                $state.statics.pieces[captured_piece]
                            ) as u32;
                        $state.major_pieces[captured_color as usize] -=
                            p_is_major!(
                                $state.statics.pieces[captured_piece]
                            ) as u32;
                        $state.minor_pieces[captured_color as usize] -=
                            p_is_minor!(
                                $state.statics.pieces[captured_piece]
                            ) as u32;

                        $state.opening_material[captured_color as usize] -=
                            p_ovalue!(
                                $state.statics.pieces[captured_piece]
                            ) as u32;
                        $state.endgame_material[captured_color as usize] -=
                            p_evalue!(
                                $state.statics.pieces[captured_piece]
                            ) as u32;

                        $state.phase_score -= p_ovalue!(
                            $state.statics.pieces[captured_piece]
                        ) as u32 * p_is_big!(
                            $state.statics.pieces[captured_piece]
                        ) as u32 * !p_is_royal!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;

                        }
                }
            } else if move_type == DROP_MOVE {
                let drop_square = start!(applied_move) as u32;

                let piece_color = p_color!($state.statics.pieces[piece_index]);

                set!(
                    $state.pieces_board[piece_color as usize], drop_square
                );

                if p_is_royal!($state.statics.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize]
                        .push(drop_square as Square);
                }

                hash_in_or_out_piece!(
                    $state, piece_index, drop_square as Square
                );

                $state.main_board[drop_square as usize] =
                    piece_index as PieceIndex;
                piece_list_push!($state, piece_index, drop_square as Square);

                if get!(
                    $state.statics.initial_setup[piece_index],
                    drop_square
                ) {
                    set_virgin!($state, drop_square);
                }

                $state.opening_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_opening
                    [piece_index][drop_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_endgame
                    [piece_index][drop_square as usize];

                $state.big_pieces[piece_color as usize] +=
                    p_is_big!($state.statics.pieces[piece_index]) as u32;
                $state.major_pieces[piece_color as usize] +=
                    p_is_major!($state.statics.pieces[piece_index]) as u32;
                $state.minor_pieces[piece_color as usize] +=
                    p_is_minor!($state.statics.pieces[piece_index]) as u32;

                $state.phase_score += p_ovalue!(
                    $state.statics.pieces[piece_index]
                ) as u32 * p_is_big!(
                    $state.statics.pieces[piece_index]
                ) as u32 * !p_is_royal!(
                    $state.statics.pieces[piece_index]
                ) as u32;


                let hand = &mut $state.piece_in_hand
                    [piece_color as usize][piece_index];

                hash_update_in_hand!(
                    $state,
                    piece_index,
                    *hand,
                    *hand - 1
                );

                *hand -= 1;

                if $state.game_phase == SETUP
                && $state.piece_in_hand[0].iter().all(|&count| count == 0)
                && $state.piece_in_hand[1].iter().all(|&count| count == 0)
                {
                    $state.game_phase = OPENING;
                }
             } else if move_type == CASTLING_MOVE {
                let start_square = start!(applied_move) as u32;
                let end_square = end!(applied_move) as u32;
                let captured_piece = captured_piece!(applied_move) as usize;
                let captured_square = captured_square!(applied_move) as u32;
                let unload_square = unload_square!(applied_move) as u32;

                let piece_color = p_color!($state.statics.pieces[piece_index]);
                let captured_color = p_color!(
                    $state.statics.pieces[captured_piece]
                );

                clear!(
                    $state.pieces_board[piece_color as usize], start_square
                );
                set!(
                    $state.pieces_board[piece_color as usize], end_square
                );

                if p_is_royal!($state.statics.pieces[piece_index]) {            /* castling never promotes            */
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != start_square
                    );
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear_virgin!($state, start_square);

                hash_in_or_out_piece!(
                    $state, piece_index, start_square as Square
                );
                hash_in_or_out_piece!(
                    $state,
                    piece_index,
                    end_square as Square
                );

                $state.main_board[start_square as usize] = NO_PIECE;
                $state.main_board[end_square as usize] =
                    piece_index as PieceIndex;

                $state.opening_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_opening
                    [piece_index][start_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] -=
                    $state.statics.pst_endgame
                    [piece_index][start_square as usize];
                $state.opening_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_opening
                    [piece_index][end_square as usize];
                $state.endgame_pst_bonus[piece_color as usize] +=
                    $state.statics.pst_endgame
                    [piece_index][end_square as usize];

                piece_list_remove!($state, piece_index, start_square as Square);
                piece_list_push!($state, piece_index, end_square as Square);

                if captured_square != end_square {
                    $state.main_board[captured_square as usize] =
                        NO_PIECE;

                    clear!(
                        $state.pieces_board[captured_color as usize],
                        captured_square
                    );
                }

                hash_in_or_out_piece!(
                    $state,
                    captured_piece,
                    captured_square as Square
                );

                clear_virgin!($state, captured_square);

                set!(
                    $state.pieces_board[captured_color as usize],
                    unload_square
                );

                hash_in_or_out_piece!(
                    $state,
                    captured_piece,
                    unload_square as Square
                );

                set_virgin!($state, unload_square);

                $state.main_board[unload_square as usize] =
                    captured_piece as PieceIndex;
                piece_list_remove!(
                    $state, captured_piece, captured_square as Square
                );
                piece_list_push!(
                    $state, captured_piece, unload_square as Square
                );

                $state.opening_pst_bonus[captured_color as usize] -=
                    $state.statics.pst_opening[captured_piece]
                    [captured_square as usize];
                $state.endgame_pst_bonus[captured_color as usize] -=
                    $state.statics.pst_endgame[captured_piece]
                    [captured_square as usize];
                $state.opening_pst_bonus[captured_color as usize] +=
                    $state.statics.pst_opening[captured_piece]
                    [unload_square as usize];
                $state.endgame_pst_bonus[captured_color as usize] +=
                    $state.statics.pst_endgame[captured_piece]
                    [unload_square as usize];

                $state.castling_state &= ![
                    WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE
                ][piece_color as usize];

                $state.castling_state |= CASTLED << piece_color;

                hash_update_castling!(
                    $state, last_castling_state,
                    $state.castling_state
                );
            }

            $state.en_passant_square = if creates_enp {
                enp_square as EnPassantSquare
            } else {
                NO_EN_PASSANT
            };

            hash_update_en_passant!(
                $state,
                last_en_passant_square,
                $state.en_passant_square
            );

            let resets_halfmove = move_type == SINGLE_CAPTURE_MOVE
                || move_type == MULTI_CAPTURE_MOVE
                || move_type == DROP_MOVE
                || $state.termination.counter.as_ref().is_some_and(
                    |counter| counter.reset_pieces[piece_index]
                );

            if let Some(counter) = &mut $state.termination.counter {
                counter.clock = if resets_halfmove {
                    0
                } else {
                    last_halfmove_clock.saturating_add(1)
                };
            }

            if $state.termination.counting.is_some() {
                let progress = counting_progress($state, last_counting);

                if let Some(counting) = &mut $state.termination.counting {
                    counting.progress = progress;
                }
            }

            $state.game_phase = game_phase!($state);

            let this_player = $state.playing;
            let next_player = 1 - this_player;

            let still_in_check = is_in_check!(this_player, $state);

            let in_stand_off = stand_offs!($state)
                .then(|| is_in_stand_off!($state));

            let in_check = $state.termination.checks.as_ref().map(
                |_| is_in_check!(next_player, $state)
            );

            if in_check == Some(true)
            && let Some(checks) = &mut $state.termination.checks
            {
                checks.delivered[this_player as usize] =
                checks.delivered[this_player as usize].saturating_add(1);
            }

            $state.playing = next_player;
            hash_toggle_side!($state);

            if let Some(repetition) = &mut $state.termination.repetition {
                repetition.clock = if move_type == QUIET_MOVE
                && !promotion!(applied_move) {
                    last_repetition_clock.saturating_add(1)
                } else {
                    0
                };
            }

            let snapshot: Snapshot = Snapshot {
                move_ply: applied_move,
                in_check,
                in_stand_off,
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                repetition_clock: last_repetition_clock,
                counting: last_counting,
                en_passant_square: last_en_passant_square,
                game_result: last_game_result,
                check_count: last_check_count,
                game_phase: last_game_phase,
                phase_score: last_phase_score,
                position_hash: last_position_hash,
                virgin_hash: last_virgin_hash,
                pawn_hash: last_pawn_hash,
            };

            $state.history.push(snapshot);

            let legal = stand_off_before && pass_move || (
                !still_in_check && (
                    in_stand_off != Some(true) || !stand_off_before
                )
            );

            if !legal {
                undo_move!($state);
                false
            } else {
                if let Some((color, outcome, _)) = position_terminal($state) {
                    $state.termination.game_result = resolve_outcome!(
                        color, outcome
                    );
                }

                #[cfg(debug_assertions)]
                verify_game_state($state);

                true
            }
        })
    };
}

/// undo_move!
///
/// Undoes the last move with the last [`Snapshot`]. It restores all dynamic
/// fields and reverses the changes of `make_move!`: occupancy, piece lists
/// and end rule progress.
///
/// Params:
/// - state: &mut State -> position with the move to undo
///
/// Notes:
/// Call it only after `make_move!` returns true. A rejected move already
/// removed its snapshot.
///
#[macro_export]
macro_rules! undo_move {
    ($state:expr) => {
        hotpath::measure_block!("state::undo_move", {

        #[cfg(debug_assertions)]
        verify_game_state($state);

        $state.search_ply = $state.search_ply.saturating_sub(1);
        $state.ply_counter -= 1;

        let snapshot =
            $state.history.pop().unwrap_or_else(|| panic!("No move to undo!"));

        $state.playing = 1 - $state.playing;
        $state.castling_state = snapshot.castling_state;

        if let Some(counter) = &mut $state.termination.counter {
            counter.clock = snapshot.halfmove_clock;
        }
        if let Some(repetition) = &mut $state.termination.repetition {
            repetition.clock = snapshot.repetition_clock;
        }
        if let Some(counting) = &mut $state.termination.counting {
            counting.progress = snapshot.counting;
        }
        if let Some(checks) = &mut $state.termination.checks {
            checks.delivered = snapshot.check_count;
        }

        $state.en_passant_square = snapshot.en_passant_square;
        $state.position_hash = snapshot.position_hash;
        $state.virgin_hash = snapshot.virgin_hash;
        $state.pawn_hash = snapshot.pawn_hash;
        $state.termination.game_result = snapshot.game_result;
        $state.game_phase = snapshot.game_phase;
        $state.phase_score = snapshot.phase_score;

        let mv = snapshot.move_ply;
        let move_type = move_type!(mv);

        if move_type == QUIET_MOVE {
            let piece_index = piece!(mv) as usize;
            let start_square = start!(mv) as u32;
            let end_square = end!(mv) as u32;
            let piece_unmoved = is_initial!(mv) == 1;
            let is_promotion = promotion!(mv);
            let promoted_piece = promoted!(mv) as usize;

            let piece_color = p_color!($state.statics.pieces[piece_index]);

            clear!($state.pieces_board[piece_color as usize], end_square);
            set!($state.pieces_board[piece_color as usize], start_square);

            if p_is_royal!($state.statics.pieces[                               /* undo drops the piece that landed   */
                if is_promotion { promoted_piece } else { piece_index }         /* and restores the one that left, so */
            ]) {                                                                /* each end reads its own piece       */
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != end_square);
            }

            if p_is_royal!($state.statics.pieces[piece_index]) {
                $state.royal_list[piece_color as usize]
                    .push(start_square as Square);
            }

            if piece_unmoved {
                clear!($state.virgin_board, end_square);
                set!($state.virgin_board, start_square);
            }

            $state.main_board[end_square as usize] = NO_PIECE;
            $state.main_board[start_square as usize] =
                piece_index as PieceIndex;

            $state.opening_pst_bonus[piece_color as usize] -=
                $state.statics.pst_opening[
                    if is_promotion { promoted_piece } else { piece_index }
                ][end_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] -=
                $state.statics.pst_endgame[
                    if is_promotion { promoted_piece } else { piece_index }
                ][end_square as usize];
            $state.opening_pst_bonus[piece_color as usize] +=
                $state.statics.pst_opening[piece_index][start_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] +=
                $state.statics.pst_endgame[piece_index][start_square as usize];

            if is_promotion {
                piece_list_remove!(
                    $state, promoted_piece, end_square as Square
                );

                $state.big_pieces[piece_color as usize] -=
                    p_is_big!($state.statics.pieces[promoted_piece]) as u32;
                $state.major_pieces[piece_color as usize] -=
                    p_is_major!($state.statics.pieces[promoted_piece]) as u32;
                $state.minor_pieces[piece_color as usize] -=
                    p_is_minor!($state.statics.pieces[promoted_piece]) as u32;

                $state.big_pieces[piece_color as usize] +=
                    p_is_big!($state.statics.pieces[piece_index]) as u32;
                $state.major_pieces[piece_color as usize] +=
                    p_is_major!($state.statics.pieces[piece_index]) as u32;
                $state.minor_pieces[piece_color as usize] +=
                    p_is_minor!($state.statics.pieces[piece_index]) as u32;

                $state.opening_material[piece_color as usize] +=
                    p_ovalue!($state.statics.pieces[piece_index]) as u32;
                $state.endgame_material[piece_color as usize] +=
                    p_evalue!($state.statics.pieces[piece_index]) as u32;
                $state.opening_material[piece_color as usize] -=
                    p_ovalue!($state.statics.pieces[promoted_piece]) as u32;
                $state.endgame_material[piece_color as usize] -=
                    p_evalue!($state.statics.pieces[promoted_piece]) as u32;


                if promote_to_captured!($state) {
                    let enemy_equiv = $state.statics.piece_swap_map
                        [promoted_piece];

                    let hand = &mut $state.piece_in_hand
                        [1 - piece_color as usize][enemy_equiv as usize];

                    *hand += 1;

                    if drops!($state) {
                        let opponent = 1 - piece_color as usize;

                        $state.opening_material[opponent] +=
                            p_ovalue!(
                                $state.statics.pieces[enemy_equiv as usize]
                            ) as u32;
                        $state.endgame_material[opponent] +=
                            p_evalue!(
                                $state.statics.pieces[enemy_equiv as usize]
                            ) as u32;
                    }
                }
            } else {
                piece_list_remove!($state, piece_index, end_square as Square);
            }

            piece_list_push!($state, piece_index, start_square as Square);
        } else if move_type == SINGLE_CAPTURE_MOVE {
            let piece_index = piece!(mv) as usize;
            let start_square = start!(mv) as u32;
            let end_square = end!(mv) as u32;
            let piece_unmoved = is_initial!(mv) == 1;
            let is_promotion = promotion!(mv);
            let promoted_piece = promoted!(mv) as usize;
            let captured_piece = captured_piece!(mv) as usize;
            let captured_square = captured_square!(mv) as u32;
            let captured_unmoved = captured_unmoved!(mv);
            let is_unload = is_unload!(mv);
            let unload_square = unload_square!(mv) as u32;

            let piece_color = p_color!($state.statics.pieces[piece_index]);
            let captured_color = p_color!(
                $state.statics.pieces[captured_piece]
            );

            clear!($state.pieces_board[piece_color as usize], end_square);
            set!($state.pieces_board[piece_color as usize], start_square);

            if p_is_royal!($state.statics.pieces[                               /* undo drops the piece that landed   */
                if is_promotion { promoted_piece } else { piece_index }         /* and restores the one that left, so */
            ]) {                                                                /* each end reads its own piece       */
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != end_square);
            }

            if p_is_royal!($state.statics.pieces[piece_index]) {
                $state.royal_list[piece_color as usize]
                    .push(start_square as Square);
            }

            if piece_unmoved {
                clear!($state.virgin_board, end_square);
                set!($state.virgin_board, start_square);
            }

            $state.main_board[end_square as usize] = NO_PIECE;
            $state.main_board[start_square as usize] =
                piece_index as PieceIndex;

            $state.opening_pst_bonus[piece_color as usize] -=
                $state.statics.pst_opening[
                    if is_promotion { promoted_piece } else { piece_index }
                ][end_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] -=
                $state.statics.pst_endgame[
                    if is_promotion { promoted_piece } else { piece_index }
                ][end_square as usize];
            $state.opening_pst_bonus[piece_color as usize] +=
                $state.statics.pst_opening[piece_index][start_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] +=
                $state.statics.pst_endgame[piece_index][start_square as usize];

            if is_promotion {
                piece_list_remove!(
                    $state, promoted_piece, end_square as Square
                );

                $state.big_pieces[piece_color as usize] -=
                    p_is_big!($state.statics.pieces[promoted_piece]) as u32;
                $state.major_pieces[piece_color as usize] -=
                    p_is_major!($state.statics.pieces[promoted_piece]) as u32;
                $state.minor_pieces[piece_color as usize] -=
                    p_is_minor!($state.statics.pieces[promoted_piece]) as u32;

                $state.big_pieces[piece_color as usize] +=
                    p_is_big!($state.statics.pieces[piece_index]) as u32;
                $state.major_pieces[piece_color as usize] +=
                    p_is_major!($state.statics.pieces[piece_index]) as u32;
                $state.minor_pieces[piece_color as usize] +=
                    p_is_minor!($state.statics.pieces[piece_index]) as u32;

                $state.opening_material[piece_color as usize] +=
                    p_ovalue!($state.statics.pieces[piece_index]) as u32;
                $state.endgame_material[piece_color as usize] +=
                    p_evalue!($state.statics.pieces[piece_index]) as u32;
                $state.opening_material[piece_color as usize] -=
                    p_ovalue!($state.statics.pieces[promoted_piece]) as u32;
                $state.endgame_material[piece_color as usize] -=
                    p_evalue!($state.statics.pieces[promoted_piece]) as u32;


                if promote_to_captured!($state) {
                    let enemy_equiv = $state.statics.piece_swap_map
                        [promoted_piece];

                    let hand = &mut $state.piece_in_hand
                        [1 - piece_color as usize][enemy_equiv as usize];

                    *hand += 1;

                    if drops!($state) {
                        let opponent = 1 - piece_color as usize;

                        $state.opening_material[opponent] +=
                            p_ovalue!(
                                $state.statics.pieces[enemy_equiv as usize]
                            ) as u32;
                        $state.endgame_material[opponent] +=
                            p_evalue!(
                                $state.statics.pieces[enemy_equiv as usize]
                            ) as u32;
                    }
                }
            } else {
                piece_list_remove!($state, piece_index, end_square as Square);
            }

            piece_list_push!($state, piece_index, start_square as Square);

            if is_unload {
                clear!(
                    $state.pieces_board[captured_color as usize],
                    unload_square
                );
                set!(
                    $state.pieces_board[captured_color as usize],
                    captured_square
                );
            } else {
                set!(
                    $state.pieces_board[captured_color as usize],
                    captured_square
                );
            }


            if captured_unmoved {
                set!($state.virgin_board, captured_square);

                if is_unload {
                    clear!($state.virgin_board, unload_square);
                }
            }

            if is_unload {
                $state.main_board[unload_square as usize] = NO_PIECE;
            }
            $state.main_board[captured_square as usize] =
                captured_piece as PieceIndex;

            if is_unload {
                piece_list_remove!(
                    $state, captured_piece, unload_square as Square
                );

                $state.opening_pst_bonus[captured_color as usize] -=
                    $state.statics.pst_opening[captured_piece]
                    [unload_square as usize];
                $state.endgame_pst_bonus[captured_color as usize] -=
                    $state.statics.pst_endgame[captured_piece]
                    [unload_square as usize];
                $state.opening_pst_bonus[captured_color as usize] +=
                    $state.statics.pst_opening[captured_piece]
                    [captured_square as usize];
                $state.endgame_pst_bonus[captured_color as usize] +=
                    $state.statics.pst_endgame[captured_piece]
                    [captured_square as usize];
            }
            piece_list_push!(
                $state, captured_piece, captured_square as Square
            );

            if p_is_royal!($state.statics.pieces[captured_piece]) {
                if is_unload {
                    $state.royal_list[captured_color as usize].retain(
                        |&sq| sq as u32 != unload_square
                    );
                }

                $state.royal_list[captured_color as usize]
                    .push(captured_square as Square);
            }

            if !is_unload {
                $state.opening_pst_bonus[captured_color as usize] +=
                    $state.statics.pst_opening[captured_piece]
                    [captured_square as usize];
                $state.endgame_pst_bonus[captured_color as usize] +=
                    $state.statics.pst_endgame[captured_piece]
                    [captured_square as usize];

                $state.big_pieces[captured_color as usize] +=
                    p_is_big!($state.statics.pieces[captured_piece]) as u32;
                $state.major_pieces[captured_color as usize] +=
                    p_is_major!($state.statics.pieces[captured_piece]) as u32;
                $state.minor_pieces[captured_color as usize] +=
                    p_is_minor!($state.statics.pieces[captured_piece]) as u32;

                $state.opening_material[captured_color as usize] +=
                    p_ovalue!(
                        $state.statics.pieces[captured_piece]
                    ) as u32;
                $state.endgame_material[captured_color as usize] +=
                    p_evalue!(
                        $state.statics.pieces[captured_piece]
                    ) as u32;

            }

            if drops!($state) || promote_to_captured!($state) {
                let demoted_piece = $state.statics.piece_demotion_map
                    [captured_piece] as usize;
                let hand_piece = $state.statics.piece_swap_map
                    [demoted_piece] as usize;

                let hand = &mut $state.piece_in_hand
                    [piece_color as usize][hand_piece];

                *hand -= 1;

                if drops!($state) {
                    $state.opening_material[piece_color as usize] -=
                        p_ovalue!(
                            $state.statics.pieces[hand_piece]
                        ) as u32;
                    $state.endgame_material[piece_color as usize] -=
                        p_evalue!(
                            $state.statics.pieces[hand_piece]
                        ) as u32;
                }
            }
        } else if move_type == MULTI_CAPTURE_MOVE {
            let piece_index = piece!(mv) as usize;
            let start_square = start!(mv) as u32;
            let end_square = end!(mv) as u32;
            let piece_unmoved = is_initial!(mv) == 1;
            let is_promotion = promotion!(mv);
            let promoted_piece = promoted!(mv) as usize;

            let piece_color = p_color!($state.statics.pieces[piece_index]);

            clear!($state.pieces_board[piece_color as usize], end_square);
            set!($state.pieces_board[piece_color as usize], start_square);

            if p_is_royal!($state.statics.pieces[                               /* undo drops the piece that landed   */
                if is_promotion { promoted_piece } else { piece_index }         /* and restores the one that left, so */
            ]) {                                                                /* each end reads its own piece       */
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != end_square);
            }

            if p_is_royal!($state.statics.pieces[piece_index]) {
                $state.royal_list[piece_color as usize]
                    .push(start_square as Square);
            }

            if piece_unmoved {
                clear!($state.virgin_board, end_square);
                set!($state.virgin_board, start_square);
            }

            $state.main_board[end_square as usize] = NO_PIECE;
            $state.main_board[start_square as usize] =
                piece_index as PieceIndex;

            $state.opening_pst_bonus[piece_color as usize] -=
                $state.statics.pst_opening[
                    if is_promotion { promoted_piece } else { piece_index }
                ][end_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] -=
                $state.statics.pst_endgame[
                    if is_promotion { promoted_piece } else { piece_index }
                ][end_square as usize];
            $state.opening_pst_bonus[piece_color as usize] +=
                $state.statics.pst_opening[piece_index][start_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] +=
                $state.statics.pst_endgame[piece_index][start_square as usize];

            if is_promotion {
                piece_list_remove!(
                    $state, promoted_piece, end_square as Square
                );

                $state.big_pieces[piece_color as usize] -=
                    p_is_big!($state.statics.pieces[promoted_piece]) as u32;
                $state.major_pieces[piece_color as usize] -=
                    p_is_major!($state.statics.pieces[promoted_piece]) as u32;
                $state.minor_pieces[piece_color as usize] -=
                    p_is_minor!($state.statics.pieces[promoted_piece]) as u32;

                $state.big_pieces[piece_color as usize] +=
                    p_is_big!($state.statics.pieces[piece_index]) as u32;
                $state.major_pieces[piece_color as usize] +=
                    p_is_major!($state.statics.pieces[piece_index]) as u32;
                $state.minor_pieces[piece_color as usize] +=
                    p_is_minor!($state.statics.pieces[piece_index]) as u32;

                $state.opening_material[piece_color as usize] +=
                    p_ovalue!($state.statics.pieces[piece_index]) as u32;
                $state.endgame_material[piece_color as usize] +=
                    p_evalue!($state.statics.pieces[piece_index]) as u32;
                $state.opening_material[piece_color as usize] -=
                    p_ovalue!($state.statics.pieces[promoted_piece]) as u32;
                $state.endgame_material[piece_color as usize] -=
                    p_evalue!($state.statics.pieces[promoted_piece]) as u32;


                if promote_to_captured!($state) {
                    let enemy_equiv =
                        $state.statics.piece_swap_map[promoted_piece];
                    let hand = &mut $state.piece_in_hand
                        [1 - piece_color as usize]
                        [enemy_equiv as usize];
                    *hand += 1;

                    if drops!($state) {
                        let opponent = 1 - piece_color as usize;

                        $state.opening_material[opponent] +=
                            p_ovalue!(
                                $state.statics.pieces[enemy_equiv as usize]
                            ) as u32;
                        $state.endgame_material[opponent] +=
                            p_evalue!(
                                $state.statics.pieces[enemy_equiv as usize]
                            ) as u32;
                    }
                }
            } else {
                piece_list_remove!($state, piece_index, end_square as Square);
            }

            piece_list_push!($state, piece_index, start_square as Square);

            for cap in m_captures!(mv).iter() {
                let captured_piece = multi_move_captured_piece!(cap) as usize;
                let captured_square = multi_move_captured_square!(cap) as u32;
                let captured_unmoved = multi_move_captured_unmoved!(cap);
                let is_unload = multi_move_is_unload!(cap);
                let unload_square = multi_move_unload_square!(cap) as u32;
                let captured_color =
                    p_color!($state.statics.pieces[captured_piece]);

                if is_unload {
                    clear!(
                        $state.pieces_board[captured_color as usize],
                        unload_square
                    );
                    set!(
                        $state.pieces_board[captured_color as usize],
                        captured_square
                    );
                } else {
                    set!(
                        $state.pieces_board[captured_color as usize],
                        captured_square
                    );
                }


                if captured_unmoved {
                    set!($state.virgin_board, captured_square);

                    if is_unload {
                        clear!($state.virgin_board, unload_square);
                    }
                }

                if is_unload {
                    $state.main_board[unload_square as usize] = NO_PIECE;
                }
                $state.main_board[captured_square as usize] =
                    captured_piece as PieceIndex;

                if is_unload {
                    piece_list_remove!(
                        $state, captured_piece, unload_square as Square
                    );

                    $state.opening_pst_bonus[captured_color as usize] -=
                        $state.statics.pst_opening[captured_piece]
                        [unload_square as usize];
                    $state.endgame_pst_bonus[captured_color as usize] -=
                        $state.statics.pst_endgame[captured_piece]
                        [unload_square as usize];
                    $state.opening_pst_bonus[captured_color as usize] +=
                        $state.statics.pst_opening[captured_piece]
                        [captured_square as usize];
                    $state.endgame_pst_bonus[captured_color as usize] +=
                        $state.statics.pst_endgame[captured_piece]
                        [captured_square as usize];
                }
                piece_list_push!(
                    $state, captured_piece, captured_square as Square
                );

                if p_is_royal!($state.statics.pieces[captured_piece]) {
                    if is_unload {
                        $state.royal_list[captured_color as usize].retain(
                            |&sq| sq as u32 != unload_square
                        );
                    }

                    $state.royal_list[captured_color as usize]
                        .push(captured_square as Square);
                }

                if !is_unload {
                    $state.opening_pst_bonus[captured_color as usize] +=
                        $state.statics.pst_opening[captured_piece]
                        [captured_square as usize];
                    $state.endgame_pst_bonus[captured_color as usize] +=
                        $state.statics.pst_endgame[captured_piece]
                        [captured_square as usize];

                    $state.big_pieces[captured_color as usize] +=
                        p_is_big!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;
                    $state.major_pieces[captured_color as usize] +=
                        p_is_major!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;
                    $state.minor_pieces[captured_color as usize] +=
                        p_is_minor!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;

                    $state.opening_material[captured_color as usize] +=
                        p_ovalue!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;
                    $state.endgame_material[captured_color as usize] +=
                        p_evalue!(
                            $state.statics.pieces[captured_piece]
                        ) as u32;

                    }

                if drops!($state) || promote_to_captured!($state) {
                    let demoted_piece = $state.statics.piece_demotion_map
                        [captured_piece] as usize;
                    let hand_piece = $state.statics.piece_swap_map
                        [demoted_piece] as usize;

                    let hand = &mut $state.piece_in_hand
                        [piece_color as usize][hand_piece];

                    *hand -= 1;

                    if drops!($state) {
                        $state.opening_material[piece_color as usize] -=
                            p_ovalue!(
                                $state.statics.pieces[hand_piece]
                            ) as u32;
                        $state.endgame_material[piece_color as usize] -=
                            p_evalue!(
                                $state.statics.pieces[hand_piece]
                            ) as u32;
                    }
                }
            }
        } else if move_type == DROP_MOVE {
            let piece_index = piece!(mv) as usize;
            let drop_square = start!(mv) as u32;

            let piece_color = p_color!($state.statics.pieces[piece_index]);

            clear!($state.pieces_board[piece_color as usize], drop_square);

            if p_is_royal!($state.statics.pieces[piece_index]) {
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != drop_square);
            }

            $state.main_board[drop_square as usize] = NO_PIECE;
            piece_list_remove!($state, piece_index, drop_square as Square);

            clear!($state.virgin_board, drop_square);

            $state.opening_pst_bonus[piece_color as usize] -=
                $state.statics.pst_opening[piece_index][drop_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] -=
                $state.statics.pst_endgame[piece_index][drop_square as usize];


            $state.big_pieces[piece_color as usize] -=
                p_is_big!($state.statics.pieces[piece_index]) as u32;
            $state.major_pieces[piece_color as usize] -=
                p_is_major!($state.statics.pieces[piece_index]) as u32;
            $state.minor_pieces[piece_color as usize] -=
                p_is_minor!($state.statics.pieces[piece_index]) as u32;

            let hand = &mut $state.piece_in_hand[piece_color as usize]
                [piece_index];
            *hand += 1;
        } else if move_type == CASTLING_MOVE {
            let piece_index = piece!(mv) as usize;
            let start_square = start!(mv) as u32;
            let end_square = end!(mv) as u32;
            let captured_piece = captured_piece!(mv) as usize;
            let captured_square = captured_square!(mv) as u32;
            let unload_square = unload_square!(mv) as u32;

            let piece_color = p_color!($state.statics.pieces[piece_index]);
            let captured_color = p_color!(
                $state.statics.pieces[captured_piece]
            );

            clear!($state.pieces_board[piece_color as usize], end_square);
            set!($state.pieces_board[piece_color as usize], start_square);

            if p_is_royal!($state.statics.pieces[piece_index]) {                /* castling never promotes            */
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != end_square);
                $state.royal_list[piece_color as usize]
                    .push(start_square as Square);
            }

            clear!($state.virgin_board, end_square);
            set!($state.virgin_board, start_square);

            $state.main_board[end_square as usize] = NO_PIECE;
            $state.main_board[start_square as usize] =
                piece_index as PieceIndex;

            $state.opening_pst_bonus[piece_color as usize] -=
                $state.statics.pst_opening[piece_index][end_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] -=
                $state.statics.pst_endgame[piece_index][end_square as usize];
            $state.opening_pst_bonus[piece_color as usize] +=
                $state.statics.pst_opening[piece_index][start_square as usize];
            $state.endgame_pst_bonus[piece_color as usize] +=
                $state.statics.pst_endgame[piece_index][start_square as usize];

            piece_list_remove!($state, piece_index, end_square as Square);
            piece_list_push!($state, piece_index, start_square as Square);

            clear!(
                $state.pieces_board[captured_color as usize],
                unload_square
            );
            set!(
                $state.pieces_board[captured_color as usize],
                captured_square
            );

            set!($state.virgin_board, captured_square);
            clear!($state.virgin_board, unload_square);

            $state.main_board[unload_square as usize] = NO_PIECE;
            $state.main_board[captured_square as usize] =
                captured_piece as PieceIndex;

            piece_list_remove!($state, captured_piece, unload_square as Square);

            $state.opening_pst_bonus[captured_color as usize] -=
                $state.statics.pst_opening[captured_piece]
                [unload_square as usize];
            $state.endgame_pst_bonus[captured_color as usize] -=
                $state.statics.pst_endgame[captured_piece]
                [unload_square as usize];
            $state.opening_pst_bonus[captured_color as usize] +=
                $state.statics.pst_opening[captured_piece]
                [captured_square as usize];
            $state.endgame_pst_bonus[captured_color as usize] +=
                $state.statics.pst_endgame[captured_piece]
                [captured_square as usize];
            piece_list_push!(
                $state, captured_piece, captured_square as Square
            );
        }

        #[cfg(debug_assertions)]
        verify_game_state(&$state);
        })
    };
}

/// make_null_move!
///
/// Plays a null move for the side to move, for null move pruning. It does
/// not change occupancy or piece lists.
///
/// - increments `search_ply` and `ply_counter`
/// - clears en passant and updates the key
/// - changes `playing` and the side key
/// - pushes a `Snapshot` to the history
/// - does not change the halfmove clock
///
/// Params:
/// - state: &mut State -> position that gives the turn to the opponent
///
#[macro_export]
macro_rules! make_null_move {
    ($state:expr) => {
        {

            #[cfg(debug_assertions)]
            verify_game_state($state);

            $state.search_ply += 1;
            $state.ply_counter += 1;

            let last_en_passant_square = $state.en_passant_square;
            let last_halfmove_clock = $state.termination.counter
                .as_ref().map_or(0, |counter| counter.clock);
            let last_repetition_clock = $state.termination.repetition
                .as_ref().map_or(0, |repetition| repetition.clock);
            let last_counting = $state.termination.counting
                .as_ref().and_then(|counting| counting.progress);
            let last_castling_state = $state.castling_state;
            let last_position_hash = $state.position_hash;
            let last_virgin_hash = $state.virgin_hash;
            let last_pawn_hash = $state.pawn_hash;
            let last_game_result = $state.termination.game_result;
            let last_check_count = $state.termination.checks
                .as_ref().map_or([0; 2], |checks| checks.delivered);
            let last_game_phase = $state.game_phase;
            let last_phase_score = $state.phase_score;

            if let Some(repetition) = &mut $state.termination.repetition {
                repetition.clock = last_repetition_clock.saturating_add(1);
            }

            hash_update_en_passant!(
                $state,
                last_en_passant_square,
                NO_EN_PASSANT
            );
            $state.en_passant_square = NO_EN_PASSANT;

            $state.playing = 1 - $state.playing;

            hash_toggle_side!($state);

            let snapshot: Snapshot = Snapshot {
                move_ply: null_move(),
                in_check: None,
                in_stand_off: if stand_offs!($state) {
                    Some($state.history.last()
                        .and_then(|snapshot| snapshot.in_stand_off)
                        .unwrap_or_else(|| is_in_stand_off!($state)))
                } else {
                    None
                },
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                repetition_clock: last_repetition_clock,
                counting: last_counting,
                en_passant_square: last_en_passant_square,
                game_result: last_game_result,
                check_count: last_check_count,
                game_phase: last_game_phase,
                phase_score: last_phase_score,
                position_hash: last_position_hash,
                virgin_hash: last_virgin_hash,
                pawn_hash: last_pawn_hash,
            };

            $state.history.push(snapshot);

            #[cfg(debug_assertions)]
            verify_game_state($state);
        }
    };
}

/// undo_null_move!
///
/// Undoes the last null move. It restores the `Snapshot` of
/// `make_null_move!`: turn, clocks, castling, en passant, phase and key.
///
/// Params:
/// - state: &mut State -> position with the null move to undo
///
/// Notes:
/// An empty history causes a panic.
///
#[macro_export]
macro_rules! undo_null_move {
    ($state:expr) => {{

        #[cfg(debug_assertions)]
        verify_game_state($state);

        $state.search_ply -= 1;
        $state.ply_counter -= 1;

        let snapshot =
            $state.history.pop().unwrap_or_else(|| panic!("No move to undo!"));

        $state.playing = 1 - $state.playing;
        $state.castling_state = snapshot.castling_state;
        if let Some(counter) = &mut $state.termination.counter {
            counter.clock = snapshot.halfmove_clock;
        }
        if let Some(repetition) = &mut $state.termination.repetition {
            repetition.clock = snapshot.repetition_clock;
        }
        if let Some(counting) = &mut $state.termination.counting {
            counting.progress = snapshot.counting;
        }
        if let Some(checks) = &mut $state.termination.checks {
            checks.delivered = snapshot.check_count;
        }
        $state.en_passant_square = snapshot.en_passant_square;
        $state.position_hash = snapshot.position_hash;
        $state.virgin_hash = snapshot.virgin_hash;
        $state.pawn_hash = snapshot.pawn_hash;
        $state.termination.game_result = snapshot.game_result;
        $state.game_phase = snapshot.game_phase;
        $state.phase_score = snapshot.phase_score;

        #[cfg(debug_assertions)]
        verify_game_state($state);
    }};
}

/// generate_all_moves_and_drops
///
/// Generates all pseudo-legal moves of the side to move:
///
/// - piece moves : not in the setup phase
/// - drops       : with the drop rule or in the setup phase
/// - castling    : with the castling rule
///
/// Params:
/// - state  : &State         -> position to examine
/// - out    : &mut Vec<Move> -> cleared, then filled with the moves
/// - scratch: &mut Vec<u64>  -> reused multi-capture buffer
///
/// Notes:
/// A terminal position gives an empty list, so `legal_moves!` is empty too.
///
#[hotpath::measure]
pub fn generate_all_moves_and_drops(
    state: &State,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
) {
    out.clear();

    if is_terminal!(state) {
        return;
    }

    let piece_count = state.statics.pieces.len() / 2;
    let start_index = piece_count * state.playing as usize;
    let end_index = start_index + piece_count;

    if state.game_phase != SETUP {
        for piece_index in start_index..end_index {
            let piece = &state.statics.pieces[piece_index];
            for &index in piece_squares!(state, piece_index) {
                generate_move_list!(index, piece, state, out, scratch);
            }
        }
    }

    if drops!(state) || state.game_phase == SETUP {
        for piece_index in start_index..end_index {
            let piece = &state.statics.pieces[piece_index];
            generate_drop_list!(piece, state, out);
        }
    }

    if castling!(state) {
        generate_castling_list!(state, out);
    }
}

/// generate_all_captures
///
/// The capture version of `generate_all_moves_and_drops`, for quiescence.
/// It uses the `relevant_captures` tables and keeps only real captures. It
/// never makes quiet moves, drops or castling.
///
/// Params:
/// - state  : &State         -> position to examine
/// - out    : &mut Vec<Move> -> cleared, then filled with the captures
/// - scratch: &mut Vec<u64>  -> reused multi-capture buffer
///
#[hotpath::measure]
pub fn generate_all_captures(
    state: &State,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
) {
    out.clear();
    if is_terminal!(state) || state.game_phase == SETUP {
        return;
    }

    let piece_count = state.statics.pieces.len() / 2;
    let start_index = piece_count * state.playing as usize;
    let end_index = start_index + piece_count;

    for piece_index in start_index..end_index {
        let piece = &state.statics.pieces[piece_index];
        for &index in piece_squares!(state, piece_index) {
            generate_capture_list!(index, piece, state, out, scratch);
        }
    }
}
