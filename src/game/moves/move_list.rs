//! # move_list.rs
//!
//! Generates legal moves and attack data for pieces in the current position.
//!
//! This is the runtime half of move generation: the parse modules compile
//! move expressions into displacement vectors once, and this file walks
//! those vectors against live board occupancy to produce encoded `Move`s,
//! answer attack queries, and apply/undo moves with full incremental
//! bookkeeping (hashes, material counters, castling and en passant state).
//! Everything here sits on the search hot path, hence the macro-heavy
//! style that keeps the leg-walking loops monomorphized and inlined.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 01/02/2026

use crate::*;

/*---------------------------------------------------------------------------*\
                        ATTACK QUERY REPRESENTATIONS
\*---------------------------------------------------------------------------*/

/// is_square_attacked!
///
/// Reports whether at least one precomputed attack mask can currently
/// realize an attack on `$square` against the given side, applying the
/// directional, occupancy, and modifier constraints via
/// `validate_attack_vector!`.
///
/// Params:
/// - square           -> target square being tested
/// - attacked_side    -> side whose piece stands on the square
/// - attacked_unmoved -> whether that piece is still unmoved (virgin)
/// - attacked_royal   -> whether the target counts as royal
/// - attacked_rank    -> capture rank of the target piece
/// - state            -> current position providing attack tables
///
/// Return:
/// bool -> true if any legal attack reaches the square
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
/// Reports whether `$side`'s position is in check: each royal piece's
/// square is tested with `is_square_attacked!`, so a side with multiple
/// royals is in check only when all of them are attacked. Always false
/// during the setup phase or when the side has no royal piece.
///
/// Params:
/// - side  -> side whose royals are tested
/// - state -> current position providing royal list and attack tables
///
/// Return:
/// bool -> true if the side is in check
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
/// Collects every fully legal move in the position: it generates the
/// pseudo-legal moves and drops, then keeps only those that survive a
/// make/undo legality probe, so no move that leaves its own royal exposed
/// ever reaches the caller. The position is restored before the vector is
/// yielded, so callers can enumerate legality without disturbing state.
///
/// Params:
/// - state -> position to enumerate; unchanged after expansion
///
/// Return:
/// Vec<Move> -> the legal moves, empty in a terminal position
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
/// Compiles the config's castling descriptions into precomputed castling
/// `Move`s. Each castling option is given as a pair of board layouts: the
/// start layout places the participating pieces and marks the squares
/// that must be empty (`+`) or empty and unattacked (`*`), and the end
/// layout places the same pieces on their destination squares.
///
/// ```text
/// start                            end
/// ┌────┬────┬────┬────┬────┐      ┌────┬────┬────┬────┬────┐
/// │ R  │ +  │ *  │ *  │ K  │  ->  │    │ K  │ R  │    │    │
/// └────┴────┴────┴────┴────┘      └────┴────┴────┴────┴────┘
/// ```
///
/// The royal piece is stored in the move's primary slot and the partner
/// piece in the capture/unload slot; the `+`/`*` squares are packed into
/// the move's auxiliary list for runtime validation.
///
/// Params:
/// - start: &Vec<String> -> start layouts, one per castling option
/// - end  : &Vec<String> -> matching destination layouts
/// - state: &State       -> piece dictionary and board dimensions
///
/// Return:
/// Vec<Move>             -> one precomputed castling move per layout pair
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
/// Precomputes which of a piece's compiled move vectors can physically be
/// played from one origin square: each vector is walked leg by leg (with
/// offsets mirrored for black) and discarded as soon as any leg steps off
/// the board or into the piece's forbidden zone. From a corner square,
/// for example, only the on-board subset of a piece's vectors survives:
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
/// Occupancy is deliberately ignored — that is checked at generation time
/// — so the result is a static per-(piece, square) table entry. Vectors
/// are sorted longest-first so deeper lines are probed before short ones.
///
/// Params:
/// - piece       : &Piece     -> piece type whose vectors are filtered
/// - square_index: u32        -> origin square being precomputed
/// - state       : &State     -> board dimensions and forbidden zones
/// - piece_moves : &[MoveSet] -> compiled vector sets, one per piece
///
/// Return:
/// MoveSet                    -> vectors playable from this square, longest
///                               first
///
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

        for leg in multi_leg_vector {
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
/// Precomputes vector candidates that can produce at least one capture/destroy
/// action for a given piece and origin square.
/// This mirrors `generate_relevant_moves` in structure (same bounds and
/// forbidden-zone checks), but keeps only multi-leg vectors containing a leg
/// with effective capture semantics:
/// - explicit capture (`c`)
/// - destroy (`d`)
/// - implicit last-leg capture
///
/// Result: capture-only generation can reuse the full normal move-construction
/// pipeline while starting from a narrower prefiltered vector set.
///
/// Params:
/// - piece       : &Piece     -> piece type whose vectors are filtered
/// - square_index: u32        -> origin square being precomputed
/// - state       : &State     -> board dimensions and forbidden zones
/// - piece_moves : &[MoveSet] -> compiled vector sets, one per piece
///
/// Return:
/// MoveSet                    -> capture-capable vectors playable from this
///                               square
///
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
/// Populates `relevant_attacks` entries originating from one start square.
/// For each prefiltered move vector, this records whether each traversed
/// target is attacked as enemy capture (`c`) and/or friendly destroy (`d`).
///
/// Two-phase: collect pending writes while holding only shared borrows, then
/// apply via Arc::get_mut after all borrows expire.
///
/// Params:
/// - square_index: u16        -> origin square of the outgoing attacks
/// - state       : &mut State -> engine state receiving the rev attack table
///
pub fn generate_attack_masks(square_index: u16, state: &mut State) {
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

                accumulated_index += (rank_offset * (files as i8)
                    + file_offset) as i16;

                let c = c!(leg) || (last_leg && !m!(leg));
                let d = d!(leg);

                let mask = (
                    piece_index, square_index, multi_leg_vector.to_vec()
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

    let static_data = Arc::get_mut(&mut state.statics)
        .expect("static_data has multiple Arc references during precompute");
    for (color, sq, mask) in pending {
        static_data.relevant_attacks[color][sq].push(mask);
    }
}

/// validate_attack_vector!
///
/// Validates whether an attack vector can legally reach a target square.
/// This macro executes the full per-leg simulation with movement/capture/
/// destroy/unload semantics, occupancy checks, rank/royalty/virgin filters,
/// and special modifier combinations. It is used as the runtime validator for
/// precomputed attack candidates gathered in `relevant_attacks`.
///
/// Params:
/// - multi_leg_vector -> candidate attack vector to simulate
/// - square_index     -> square the attacking piece stands on
/// - attacking_piece  -> the piece attempting the attack
/// - attacked_unmoved / attacked_royal / attacked_rank -> target traits
///   checked against the vector's capture modifiers
/// - attacked_square  -> square that must be reached with a capture leg
/// - state            -> current position for occupancy checks
///
/// Return:
/// bool -> true if the vector currently realizes the attack
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

        let mut accumulated_index = $square_index as i16;
        let mut target_was_last_captured = false;

        let leg_count = $multi_leg_vector.len();

        let mut valid = true;

        for (leg_index, leg) in $multi_leg_vector.iter().enumerate() {
            let last_leg = leg_index + 1 == leg_count;

            let start_square = accumulated_index as u32;

            let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
            let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

            accumulated_index += (
                rank_offset * ($state.statics.files as i8) + file_offset
            ) as i16;

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
            let not_k = not_k!(leg);
            let not_g = not_g!(leg);
            let not_v = not_v!(leg);
            let not_i = not_i!(leg);
            let special_v = v && not_v;

            if i && !piece_unmoved || not_i && piece_unmoved {
                valid = false;
                break;
            }

            let friendly = get!(
                $state.pieces_board[piece_color as usize],
                end_square
            ) && end_square != start_square;
            let enemy = get!(
                $state.pieces_board[1 - piece_color as usize],
                end_square
            );
            let empty = !friendly && !enemy;

            let pass_move = file_offset == 0 && rank_offset == 0;
            let imaginary_move = end_square == $attacked_square;

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
/// The move-construction core: simulates one compiled vector leg by leg
/// against current occupancy and, when every leg is satisfiable, emits
/// the encoded `Move`(s) it produces — including capture and multi-
/// capture payloads, unloads, en passant creation/consumption, castling-
/// rights effects, and promotion branching (one move per legal target).
/// Illegal combinations (blocked legs, violated capture modifiers,
/// initial-move constraints) abort without emitting.
///
/// Each leg starts where the last ended; `S` is the origin, `1`/`2` the
/// intermediate leg endpoints, and `T` the final target a move is emitted
/// for (occupancy and modifiers are checked at every endpoint):
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
/// - square_index -> origin square of the moving piece
/// - piece        -> the moving piece type
/// - vector       -> one compiled multi-leg vector to simulate
/// - state        -> current position for occupancy and rule checks
/// - out          -> output list receiving the encoded moves
/// - scratch      -> reusable buffer for multi-capture payloads
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
        let piece_unmoved =
            get!($state.virgin_board, $square_index as u32);

        let mut encoded_move = Move::default();
        enc_start!(encoded_move, $square_index as u128);
        enc_piece!(encoded_move, piece_index as u128);

        $scratch.clear();

        let mut accumulated_index = $square_index as i16;

        let leg_count = $vector.len();

        for (leg_index, leg) in $vector.iter().enumerate() {
            let last_leg = leg_index + 1 == leg_count;
            let mut taken_piece = 0u64;

            let start_square = accumulated_index as u32;

            let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
            let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

            accumulated_index += (
                rank_offset * ($state.statics.files as i8) + file_offset
            ) as i16;

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
            let not_k = not_k!(leg);
            let not_g = not_g!(leg);
            let not_v = not_v!(leg);
            let not_i = not_i!(leg);
            let special_v = v && not_v;

            if i && !piece_unmoved || not_i && piece_unmoved {
                invalid = true;
                break;
            }

            enc_is_initial!(encoded_move, (i | piece_unmoved) as u128);

            let friendly = get!(
                $state.pieces_board[piece_color as usize], end_square
            ) && end_square != start_square;
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
                    * ((start_square as u128 & 0xFFF)
                        | (accumulated_index as u128) << 12
                        | (piece_index as u128) << 24)
            );
        }

        enc_end!(encoded_move, accumulated_index as u128);

        if promotions!($state) && p_can_promote!($piece) && !invalid {
            if $scratch.is_empty() {
                enc_move_type!(encoded_move, QUIET_MOVE);
            } else if $scratch.len() == 1 {
                enc_move_type!(encoded_move, SINGLE_CAPTURE_MOVE);
                enc_capture_part!(encoded_move, $scratch[0] as u128);
            } else {
                enc_move_type!(encoded_move, MULTI_CAPTURE_MOVE);
                encoded_move.1 = Some(Arc::new(mem::take($scratch)));
            }

            let entered_mandatory = get!(
                $state.statics.promotion_zones_mandatory[
                    piece_index as usize
                ],
                accumulated_index as u32
            );
            let left_mandatory = get!(
                $state.statics.promotion_zones_mandatory[
                    piece_index as usize
                ],
                $square_index as u32
            );
            let entered_optional = get!(
                $state.statics.promotion_zones_optional[
                    piece_index as usize
                ],
                accumulated_index as u32
            );
            let left_optional = get!(
                $state.statics.promotion_zones_optional[
                    piece_index as usize
                ],
                $square_index as u32
            );

            let mandatory = entered_mandatory || left_mandatory;
            let optional = entered_optional || left_optional;

            if mandatory || optional {
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

                if !mandatory {
                    $out.push(encoded_move);
                }
            } else {
                $out.push(encoded_move);
            }
        } else if !invalid {
            if $scratch.is_empty() {
                enc_move_type!(encoded_move, QUIET_MOVE);
            } else if $scratch.len() == 1 {
                enc_move_type!(encoded_move, SINGLE_CAPTURE_MOVE);
                enc_capture_part!(encoded_move, $scratch[0] as u128);
            } else {
                enc_move_type!(encoded_move, MULTI_CAPTURE_MOVE);
                encoded_move.1 = Some(Arc::new(mem::take($scratch)));
            }

            $out.push(encoded_move);
        }
    }};
}

/// generate_move_list_from_vectors!
///
/// Generates all pseudo-legal encoded moves for `$piece` from `$square_index`.
/// Resolves multi-leg constraints, captures/unloads, en-passant flags, castling
/// side conditions, and promotion branching for all vectors in `$vector_set`.
/// Shared move constructor used by `generate_move_list!` and
/// `generate_capture_list!`, which select the appropriate precomputed source:
/// - `relevant_moves`    -> full pseudo-legal move list
/// - `relevant_captures` -> capture-focused pseudo-legal list
///
/// Unlike `validate_attack_vector!`, which only answers whether a single
/// vector realizes an attack, this macro builds complete `Move` objects
/// for all vectors in the set.
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
/// Generates all pseudo-legal encoded moves for `$piece` from
/// `$square_index`, appending them to `$out`. Resolves multi-leg
/// constraints, captures/unloads, en-passant flags, castling conditions,
/// and promotion branching from the piece's `relevant_moves` vectors.
///
/// Params:
/// - square_index -> origin square the piece moves from
/// - piece        -> piece type being moved
/// - state        -> current position providing occupancy and tables
/// - out          -> output list receiving the encoded moves
/// - scratch      -> reusable buffer for multi-capture payloads
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
/// Generates only the pseudo-legal capture moves for `$piece` from
/// `$square_index`. Uses precomputed `relevant_captures` so generation
/// follows the same pipeline as normal move generation, then drops any
/// non-capturing moves it produced.
///
/// Params:
/// - square_index -> origin square the piece moves from
/// - piece        -> piece type being moved
/// - state        -> current position providing occupancy and tables
/// - out          -> output list receiving the capture moves
/// - scratch      -> reusable buffer for multi-capture payloads
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

        let mut i = start;
        while i < $out.len() {
            if m_capture!(&$out[i]) { i += 1; } else { $out.swap_remove(i); }
        }
    }};
}

/// generate_castling_list!
///
/// Emits the currently legal castling moves for the side to move. Each
/// precomputed castling move is validated against live state: both
/// participants must stand unmoved on their start squares, destination
/// and path squares must be empty, and every `*`-marked square (packed in
/// the move's auxiliary list) must not be attacked. Castling rights bits
/// gate the whole check per side and wing.
///
/// Params:
/// - state -> current position providing rights and occupancy
/// - out   -> output list receiving the legal castling moves
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

/*---------------------------------------------------------------------------*\
                           MOVE STATE TRANSITION MACROS
\*---------------------------------------------------------------------------*/

/// make_move!
///
/// Applies a move to the game state with full incremental bookkeeping.
/// This macro performs a complete state transition:
/// - advances ply counters
/// - updates board occupancy, piece lists, virgin flags, castling/en-passant
/// - handles quiet, capture, multi-capture, unload, promotion, and drop flows
/// - updates material/piece-class counters and in-hand inventories
/// - updates Zobrist hash and repetition map
/// - pushes a reversible [`Snapshot`] and rejects illegal self-check outcomes
///
/// Params:
/// - state -> position the move is applied to
/// - mv    -> the encoded `Move` to play
///
/// Return:
/// bool -> true if the move was legal; false means it exposed the mover
/// to check and has already been undone
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
            let last_halfmove_clock = $state.halfmove_clock;
            let last_castling_state = $state.castling_state;
            let last_position_hash = $state.position_hash;
            let last_pawn_hash = $state.pawn_hash;
            let last_game_over = $state.game_over;
            let last_game_phase = $state.game_phase;
            let last_phase_score = $state.phase_score;

            let move_type = move_type!(applied_move);
            let pass_move = is_pass!(applied_move);

            let stand_off_before =
                stand_offs!($state) && is_in_stand_off!($state);

            if move_type == QUIET_MOVE {
                let piece_index = piece!(applied_move) as usize;
                let start_square = start!(applied_move) as u32;
                let end_square = end!(applied_move) as u32;
                let is_promotion = promotion!(applied_move);
                let creates_enp = creates_enp!(applied_move);
                let promoted_piece = promoted!(applied_move) as usize;
                let enp_square = created_enp!(applied_move) as u32;

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
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear!($state.virgin_board, start_square);

                if creates_enp {
                    $state.en_passant_square =
                        enp_square as EnPassantSquare;
                } else {
                    $state.en_passant_square = NO_EN_PASSANT;
                }

                hash_update_en_passant!(
                    $state,
                    last_en_passant_square,
                    $state.en_passant_square
                );

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
                    }
                }

                piece_list_remove!($state, piece_index, start_square as Square);
                piece_list_push!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                if halfmove_clock!($state)
                && $state.statics.halfmove_pieces[piece_index] {
                    $state.halfmove_clock = 0;
                } else {
                    $state.halfmove_clock += 1;
                }

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
                let piece_index = piece!(applied_move) as usize;
                let start_square = start!(applied_move) as u32;
                let end_square = end!(applied_move) as u32;
                let is_promotion = promotion!(applied_move);
                let creates_enp = creates_enp!(applied_move);
                let promoted_piece = promoted!(applied_move) as usize;
                let enp_square = created_enp!(applied_move) as u32;
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
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear!($state.virgin_board, start_square);

                if creates_enp {
                    $state.en_passant_square =
                        enp_square as EnPassantSquare;
                } else {
                    $state.en_passant_square = NO_EN_PASSANT;
                }

                hash_update_en_passant!(
                    $state,
                    last_en_passant_square,
                    $state.en_passant_square
                );

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
                    }
                }

                piece_list_remove!($state, piece_index, start_square as Square);
                piece_list_push!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                $state.halfmove_clock = 0;

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

                    $state.opening_material[piece_color as usize] +=
                        p_ovalue!($state.statics.pieces[hand_piece]) as u32;
                    $state.endgame_material[piece_color as usize] +=
                        p_evalue!($state.statics.pieces[hand_piece]) as u32;
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

                clear!($state.virgin_board, captured_square);

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

                    set!($state.virgin_board, unload_square);
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
                let piece_index = piece!(applied_move) as usize;
                let start_square = start!(applied_move) as u32;
                let end_square = end!(applied_move) as u32;
                let is_promotion = promotion!(applied_move);
                let creates_enp = creates_enp!(applied_move);
                let promoted_piece = promoted!(applied_move) as usize;
                let enp_square = created_enp!(applied_move) as u32;

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
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear!($state.virgin_board, start_square);

                if creates_enp {
                    $state.en_passant_square =
                        enp_square as EnPassantSquare;
                } else {
                    $state.en_passant_square = NO_EN_PASSANT;
                }

                hash_update_en_passant!(
                    $state,
                    last_en_passant_square,
                    $state.en_passant_square
                );

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
                    }
                }

                piece_list_remove!($state, piece_index, start_square as Square);
                piece_list_push!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as Square
                );

                $state.halfmove_clock = 0;

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

                        $state.opening_material[piece_color as usize] +=
                            p_ovalue!($state.statics.pieces[hand_piece]) as u32;
                        $state.endgame_material[piece_color as usize] +=
                            p_evalue!($state.statics.pieces[hand_piece]) as u32;
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

                    clear!($state.virgin_board, captured_square);

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

                        set!($state.virgin_board, unload_square);
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
                let piece_index = piece!(applied_move) as usize;
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

                $state.halfmove_clock = 0;

                if $state.game_phase == SETUP
                && $state.piece_in_hand[0].iter().all(|&count| count == 0)
                && $state.piece_in_hand[1].iter().all(|&count| count == 0)
                {
                    $state.game_phase = OPENING;
                }
             } else if move_type == CASTLING_MOVE {
                let piece_index = piece!(applied_move) as usize;
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

                if p_is_royal!($state.statics.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != start_square
                    );
                    $state.royal_list[piece_color as usize]
                        .push(end_square as Square);
                }

                clear!($state.virgin_board, start_square);

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

                $state.halfmove_clock = 0;

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

                clear!($state.virgin_board, captured_square);

                set!(
                    $state.pieces_board[captured_color as usize],
                    unload_square
                );

                hash_in_or_out_piece!(
                    $state,
                    captured_piece,
                    unload_square as Square
                );

                set!($state.virgin_board, unload_square);

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

                hash_update_castling!(
                    $state, last_castling_state,
                    $state.castling_state
                );
            }

            $state.game_phase =
                if $state.game_phase == SETUP {
                    SETUP
                } else if $state.phase_score > $state.statics.opening_score {
                    cmp::max(OPENING, $state.game_phase)
                } else if $state.phase_score < $state.statics.endgame_score {
                    cmp::max(ENDGAME, $state.game_phase)
                } else {
                    cmp::max(MIDDLEGAME, $state.game_phase)
                };

            $state.playing = 1 - $state.playing;

            let in_check = is_in_check!(1 - $state.playing, $state);
            let stand_off_after =
                stand_offs!($state) && is_in_stand_off!($state);

            let legal =
                !in_check && (
                    !stand_off_after  ||
                    !stand_off_before ||
                    stand_off_after && stand_off_before && pass_move
                );

            hash_toggle_side!($state);

            let repetition_count = $state.position_hash_map
                .entry($state.position_hash)
                .or_insert(0);
            *repetition_count += 1;

            let double_pass = pass_move && $state.history.last().map_or(
                false, |snapshot| pass_snapshot!(snapshot)
            );
            let passing_in_stand_off = pass_move && stand_off_before;
            let is_repetition =
                repetition_limit!($state) && $state
                .position_hash_map
                .get(&$state.position_hash)
                .unwrap_or(&1)
                >= &$state.statics.repetition_limit;
            let is_halfmove_draw =
                halfmove_clock!($state) && $state.halfmove_clock
                >= $state.statics.halfmove_limit;

            if double_pass
            || passing_in_stand_off
            || is_repetition
            || is_halfmove_draw {
                $state.game_over = true;
            }

            let snapshot: Snapshot = Snapshot {
                move_ply: applied_move,
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                en_passant_square: last_en_passant_square,
                game_over: last_game_over,
                game_phase: last_game_phase,
                phase_score: last_phase_score,
                position_hash: last_position_hash,
                pawn_hash: last_pawn_hash
            };

            $state.history.push(snapshot);
            if !legal {
                undo_move!($state);
                false
            } else {
                #[cfg(debug_assertions)]
                verify_game_state($state);

                true
            }
        })
    };
}

/// undo_move!
///
/// Reverts the last applied move using the most recent [`Snapshot`].
/// This macro restores all dynamic state fields and reverses side effects made
/// by `make_move!`, including board occupancy, piece lists, counters, and the
/// position repetition map.
///
/// Params:
/// - state -> position whose most recent move is reverted
///
#[macro_export]
macro_rules! undo_move {
    ($state:expr) => {
        hotpath::measure_block!("state::undo_move", {

        #[cfg(debug_assertions)]
        verify_game_state($state);

        $state.search_ply = $state.search_ply.saturating_sub(1);
        $state.ply_counter -= 1;

        let repetition_count = $state
            .position_hash_map
            .get_mut(&$state.position_hash)
            .unwrap_or_else(|| {
                panic!(
                    concat!(
                        "Missing repetition entry for current ",
                        "position hash {} during undo"
                    ),
                    $state.position_hash
                )
            });
        *repetition_count -= 1;

        if *repetition_count == 0 {
            $state.position_hash_map.remove(&$state.position_hash);
        }

        let snapshot =
            $state.history.pop().unwrap_or_else(|| panic!("No move to undo!"));

        $state.playing = 1 - $state.playing;
        $state.castling_state = snapshot.castling_state;
        $state.halfmove_clock = snapshot.halfmove_clock;
        $state.en_passant_square = snapshot.en_passant_square;
        $state.position_hash = snapshot.position_hash;
        $state.pawn_hash = snapshot.pawn_hash;
        $state.game_over = snapshot.game_over;
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

            if p_is_royal!($state.statics.pieces[piece_index]) {
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != end_square);
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

            if p_is_royal!($state.statics.pieces[piece_index]) {
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != end_square);
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

                $state.opening_material[piece_color as usize] -=
                    p_ovalue!($state.statics.pieces[hand_piece]) as u32;
                $state.endgame_material[piece_color as usize] -=
                    p_evalue!($state.statics.pieces[hand_piece]) as u32;
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

            if p_is_royal!($state.statics.pieces[piece_index]) {
                $state.royal_list[piece_color as usize]
                    .retain(|&sq| sq as u32 != end_square);
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

                    $state.opening_material[piece_color as usize] -=
                        p_ovalue!($state.statics.pieces[hand_piece]) as u32;
                    $state.endgame_material[piece_color as usize] -=
                        p_evalue!($state.statics.pieces[hand_piece]) as u32;
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

            if p_is_royal!($state.statics.pieces[piece_index]) {
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
/// Applies a null move for the side to move.
/// A null move:
/// - Advances `search_ply` and `ply_counter`.
/// - Flips `playing` and updates side-to-move hash.
/// - Pushes a `Snapshot` containing reversible state to history.
///
/// This is used by null-move pruning in search and does not modify board
/// occupancy or piece lists.
///
/// Params:
/// - state -> position whose turn is passed to the opponent
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
            let last_halfmove_clock = $state.halfmove_clock;
            let last_castling_state = $state.castling_state;
            let last_position_hash = $state.position_hash;
            let last_pawn_hash = $state.pawn_hash;
            let last_game_over = $state.game_over;
            let last_game_phase = $state.game_phase;
            let last_phase_score = $state.phase_score;

            $state.playing = 1 - $state.playing;

            hash_toggle_side!($state);

            let snapshot: Snapshot = Snapshot {
                move_ply: null_move(),
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                en_passant_square: last_en_passant_square,
                game_over: last_game_over,
                game_phase: last_game_phase,
                phase_score: last_phase_score,
                position_hash: last_position_hash,
                pawn_hash: last_pawn_hash
            };

            $state.history.push(snapshot);

            #[cfg(debug_assertions)]
            verify_game_state($state);
        }
    };
}

/// undo_null_move!
///
/// Reverts the most recent null move.
/// This restores the `Snapshot` saved by `make_null_move!`, including turn,
/// clocks, castling/en-passant data, phase flags, and position hash.
///
/// Notes:
/// Panics if no history snapshot exists to undo.
///
/// Params:
/// - state -> position whose most recent null move is reverted
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
        $state.halfmove_clock = snapshot.halfmove_clock;
        $state.en_passant_square = snapshot.en_passant_square;
        $state.position_hash = snapshot.position_hash;
        $state.pawn_hash = snapshot.pawn_hash;
        $state.game_over = snapshot.game_over;
        $state.game_phase = snapshot.game_phase;
        $state.phase_score = snapshot.phase_score;

        #[cfg(debug_assertions)]
        verify_game_state($state);
    }};
}

/// generate_all_moves_and_drops
///
/// Generates all pseudo-legal moves for the side to move, including drops.
/// Normal moves are skipped during setup phase; drop generation may use
/// either own-hand or enemy-hand inventory depending on drop flags.
///
/// Params:
/// - state  : &State         -> position to generate for
/// - out    : &mut Vec<Move> -> cleared, then filled with the moves
/// - scratch: &mut Vec<u64>  -> reusable multi-capture payload buffer
///
#[hotpath::measure]
pub fn generate_all_moves_and_drops(
    state: &State,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
) {
    out.clear();

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
/// Capture-only counterpart of `generate_all_moves_and_drops`, used by
/// quiescence search. Walks the narrower `relevant_captures` tables and
/// keeps only moves that actually capture; quiet moves, drops, and
/// castling are never generated.
///
/// Params:
/// - state  : &State         -> position to generate for
/// - out    : &mut Vec<Move> -> cleared, then filled with the captures
/// - scratch: &mut Vec<u64>  -> reusable multi-capture payload buffer
///
#[hotpath::measure]
pub fn generate_all_captures(
    state: &State,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
) {
    out.clear();
    if state.game_over || state.game_phase == SETUP {
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
