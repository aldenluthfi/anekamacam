//! move_list.rs
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
//! Created: 01/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                          ATTACK QUERY REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// is_square_attacked!
///
/// Reports whether anything currently attacks `$square`. The candidates come
/// from `relevant_attacks[attacked_side][square]`, which precomputation filed
/// under the square that can be reached rather than the square a piece moves
/// from, so the whole question is one table row long. Each candidate names an
/// attacker and an origin; the origin is checked against the board first,
/// since the table says what could stand there and only the position says
/// what does, and the survivors are walked by `validate_attack_vector!`.
///
/// What the target is counts as much as where it is, because a variant may
/// let a piece capture only what is royal, unmoved, or of lower rank. Those
/// properties are passed in rather than read off the board, which is what
/// lets the question be asked about a square the piece has not reached yet —
/// castling asks it of every square its king crosses — or asked as though the
/// occupant were other than it is, the way chase detection asks with royalty
/// denied to find a piece that is merely hounded rather than checked.
///
/// Params:
/// - square          : Square -> target square being tested
/// - attacked_side   : u8     -> side whose piece stands on the square
/// - attacked_unmoved: bool   -> whether that piece is unmoved (virgin)
/// - attacked_royal  : bool   -> whether the target counts as royal
/// - attacked_rank   : u8     -> capture rank of the target piece
/// - state           : &State -> current position providing attack tables
///
/// Return:
/// bool                       -> true if any legal attack reaches the square
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
/// Reports whether `$side` stands in check, by asking `is_square_attacked!`
/// of every square one of its royal pieces occupies. A side with several
/// royals is in check only when all of them are attacked at once: the piece
/// that must be saved is whichever one is not yet lost, so a variant handing
/// a player two kings has handed them a spare rather than a second liability.
///
/// A side with no royal piece is never in check, there being nothing the
/// rule can be about, and neither is anyone during the setup phase, where
/// the armies are still being placed and no capture is on offer.
///
/// Params:
/// - side : u8     -> side whose royal pieces are tested
/// - state: &State -> current position providing royal list and attack tables
///
/// Return:
/// bool            -> true if the side is in check
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
/// Always empty when the position is terminal, because
/// generate_all_moves_and_drops returns immediately in that case.
///
/// Params:
/// - state: &mut State -> position to enumerate; unchanged after expansion
///
/// Return:
/// Vec<Move>           -> the legal moves, empty in a terminal position
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

/*----------------------------------------------------------------------------*\
                             MOVE TABLE PRECOMPUTE
\*----------------------------------------------------------------------------*/

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
/// Notes:
/// A layout naming a character no piece answers to, or a piece the variant
/// never listed as a castling participant, panics. Layouts are config text
/// compiled once at load time, so a bad one is a broken variant rather than
/// a position the engine could be asked to play.
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
/// Occupancy is deliberately ignored, being the one thing about a square
/// that changes between visits, so what remains is a static per-(piece,
/// square) table entry. Vectors are sorted longest first, so the deepest
/// line out of a square is walked before the short ones sharing its
/// opening legs.
///
/// A leg written with both `v` and `!v` is the one pairing that means
/// something other than a contradiction: it bypasses forbidden zones, so
/// only the edge of the board can discard it.
///
/// Params:
/// - piece       : &Piece     -> piece type whose vectors are filtered
/// - square_index: u32        -> origin square being precomputed
/// - state       : &State     -> board dimensions and forbidden zones
/// - piece_moves : &[MoveSet] -> compiled vector sets, one per piece
///
/// Return:
/// MoveSet                    -> vectors playable here, longest first
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
/// The same filter as [`generate_relevant_moves`], run again over the same
/// vectors and keeping only those that can take something. A vector earns
/// its place on the strength of any one leg:
///
/// - a leg marked `c`, which takes an enemy piece
/// - a leg marked `d`, which destroys whatever stands there, friendly
///   pieces included
/// - a closing leg that cannot move quietly, so arriving is taking
///
/// Quiescence searches read this table instead of generating everything and
/// throwing the quiet moves away, and it costs a second table rather than a
/// second pipeline: what comes out are ordinary vectors, built into moves by
/// the same code that builds the rest.
///
/// Params:
/// - piece       : &Piece     -> piece type whose vectors are filtered
/// - square_index: u32        -> origin square being precomputed
/// - state       : &State     -> board dimensions and forbidden zones
/// - piece_moves : &[MoveSet] -> compiled vector sets, one per piece
///
/// Return:
/// MoveSet                    -> capture-capable vectors playable here
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
/// Files the reverse attack table for everything leaving one origin square.
/// Move tables answer where a piece can go; the question the search asks far
/// more often is the opposite one, who can reach here. Both are the same
/// walk read from different ends, so precomputation walks the vectors once
/// and files each under the square it arrives at:
///
/// ```text
/// relevant_moves  [piece][origin] → vectors leaving that origin
/// relevant_attacks[side][target]  → vectors arriving at that target
/// ```
///
/// An entry names the attacking piece, the origin it sets out from and the
/// vector itself, which is everything `validate_attack_vector!` needs to walk
/// the line again against a real position. Every leg contributes, not only
/// the last, so a square a slider merely passes over is listed as well: the
/// table says what can be reached from where, and only the position says what
/// actually is.
///
/// Which side's row an entry joins follows the harm the leg does. A capturing
/// leg threatens the other colour and is filed under it; a destroying leg
/// takes whatever stands on the square, friendly pieces included, and is
/// filed under the mover's own. A leg doing both is filed under each.
///
/// Params:
/// - square_index: u16        -> origin square of the outgoing attacks
/// - state       : &mut State -> engine state receiving reverse attack table
///
/// Notes:
/// Every write is gathered before any is applied. The walk reads the static
/// tables through a shared borrow while `Arc::get_mut` needs to be the only
/// reference alive to hand them back mutably, so the two cannot overlap.
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

/*----------------------------------------------------------------------------*\
                            ATTACK VECTOR VALIDATION
\*----------------------------------------------------------------------------*/

/// validate_attack_vector!
///
/// Walks one candidate out of `relevant_attacks` against the position and
/// answers whether it really does attack the square asked about. The table
/// knows geometry and nothing else, so everything a position decides — what
/// stands in the way, what is being taken, whether the piece has moved before
/// — is settled here, leg by leg, under the same modifier rules that generate
/// moves.
///
/// The target is described by arguments rather than read off the board, which
/// is what lets the question be asked about a square nothing stands on. The
/// leg arriving at `attacked_square` counts as a capture regardless of what
/// occupies it, and `k`, `g` and `v` are judged against the royalty, rank and
/// virginity handed in: castling asks about the empty squares its king crosses
/// and chase detection asks with royalty denied, both through this one path.
///
/// The legs either side of that one are walked in full. A line blocked before
/// the target attacks nothing, and a vector that cannot finish the legs beyond
/// it never arrives at all.
///
/// Params:
/// - multi_leg_vector: &MoveVector -> candidate attack vector to walk
/// - square_index    : Square      -> origin square of the attacking piece
/// - attacking_piece : &Piece      -> piece attempting the attack
/// - attacked_unmoved: bool        -> virginity to judge `v` legs against
/// - attacked_royal  : bool        -> royalty to judge `k` legs against
/// - attacked_rank   : u8          -> rank to judge `g` legs against
/// - attacked_square : Square      -> square the attack has to reach
/// - state           : &State      -> current position for occupancy checks
///
/// Return:
/// bool                            -> true when the vector realizes the attack
///
/// Notes:
/// Offsets scale by the attacking piece's colour, reversing both axes for the
/// opposite side, so one precomputed vector answers for either orientation
/// without a second table. A leg marked `t` may arrive by en passant, and an
/// unload leg is refused where it would hand the piece just taken straight
/// back, an attack that undoes itself being no attack.
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

/*----------------------------------------------------------------------------*\
                               MOVE CONSTRUCTION
\*----------------------------------------------------------------------------*/

/// process_multi_leg_vector!
///
/// The move-construction core. One compiled vector is walked leg by leg
/// against the position, and where every leg holds up, the move it makes is
/// encoded and pushed. What the walk collects decides what comes out: taken
/// pieces pile into `scratch`, and an empty pile leaves a quiet move, one
/// record a single capture, more than one a multi-capture carrying its
/// records beside the word. A leg that cannot be played, blocked or refused
/// by its own modifiers, abandons the vector with nothing emitted.
///
/// Each leg starts where the last one ended, so a vector is a route rather
/// than an offset. `S` is the origin, `1` and `2` the landings between, `T`
/// the square the move is finally emitted for; occupancy and modifiers are
/// asked at every one of them and not only at the last:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │ T  │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │ 1  │    │    │    │ 2  │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │ S  │    │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┘
/// ```
///
/// Promotion zones are read as the walk crosses them. Touching an optional
/// zone puts one move per promotion target beside the plain move; touching a
/// mandatory one, or using a leg marked `r`, emits the promotions alone, the
/// piece having no way left to stay as it is. Where the variant promotes only
/// into what the enemy has taken, a target the enemy holds no copy of is left
/// out of that fan.
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - vector      : &MoveVector    -> compiled multi-leg vector to simulate
/// - state       : &State         -> current position for rule checks
/// - out         : &mut Vec<Move> -> output list receiving encoded moves
/// - scratch     : &mut Vec<u64>  -> reusable multi-capture payload buffer
///
/// Notes:
/// Offsets scale by the piece's colour, reversing both axes for the opposite
/// side. `scratch` is cleared before the walk and moved into the encoded move
/// only where more than one record survives it; an unload leg takes the last
/// record back out and re-files it as a placement on that leg's start square.
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

        let mut mandatory = false;
        let mut optionals = false;

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

                if start_mandatory || end_mandatory
                || start_optional  || end_optional {
                    optionals = !not_r;
                }

                if r || start_mandatory || end_mandatory {
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
                    * (
                        (start_square as u128 & 0xFFF) |
                        (accumulated_index as u128) << 12 |
                        (piece_index as u128) << 24
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
/// Runs [`process_multi_leg_vector!`] over a whole set of vectors, which is
/// the only thing between one move and every move a piece has from a square.
/// Which set arrives is the caller's business, and it is also the whole
/// difference between generating everything and generating captures alone:
///
/// - `relevant_moves`    -> every vector playable from the square
/// - `relevant_captures` -> only those able to take something
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - vector_set  : &MoveSet       -> compiled vectors to expand
/// - state       : &State         -> current position providing occupancy
/// - out         : &mut Vec<Move> -> output list receiving encoded moves
/// - scratch     : &mut Vec<u64>  -> reusable multi-capture payload buffer
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
/// Every pseudo-legal move one piece has from one square, appended to `$out`.
/// The lookup into `relevant_moves` is all this adds: the vectors that survive
/// it are handed to [`generate_move_list_from_vectors!`], which turns each
/// into whatever moves the position allows it to make.
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - state       : &State         -> current position providing occupancy
/// - out         : &mut Vec<Move> -> output list receiving encoded moves
/// - scratch     : &mut Vec<u64>  -> reusable multi-capture payload buffer
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
/// The capturing half of the same generation, taken from the narrower
/// `relevant_captures` table so that nothing about the pipeline changes. A
/// capture-capable vector can still come out quiet, the square it aimed at
/// being empty when it arrives, so what the walk produced is filtered
/// afterwards rather than trusted.
///
/// The surviving captures keep the order they were generated in, which is
/// the order `generate_move_list!` would have produced them: both vector
/// tables filter one piece's vector set and then stable-sort it by the
/// same key, so the captures are a subsequence of the full move list.
/// Staged generation in `alpha_beta` relies on that correspondence.
///
/// Params:
/// - square_index: Square         -> origin square of the moving piece
/// - piece       : &Piece         -> moving piece type
/// - state       : &State         -> current position providing occupancy
/// - out         : &mut Vec<Move> -> output list receiving encoded captures
/// - scratch     : &mut Vec<u64>  -> reusable multi-capture payload buffer
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
/// Compacts `$out[$start..]` down to the moves whose capture status matches
/// `$keep`, preserving their generated order. Used to split one generation
/// pass into its capturing and quiet halves without disturbing move
/// ordering.
///
/// Params:
/// - out  : &mut Vec<Move> -> list whose tail is compacted in place
/// - start: usize          -> first index the filter applies to
/// - keep : bool           -> true keeps captures, false keeps quiets
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
/// Emits the castling moves the position currently allows the side to move.
/// A wing's rights bit is asked first, and behind it stand the moves already
/// compiled from the variant's layouts, so all that remains is whether the
/// board still looks the way its layout described:
///
/// - the royal stands on its start square and the partner on its own
/// - both destination squares are empty
/// - every square the layout marked `+` or `*` is empty
/// - the start square, the destination, and every `*` square are unattacked
///
/// That last line is why castling has to ask about squares nobody stands on,
/// and why [`is_square_attacked!`] takes the target's properties as arguments:
/// each empty square is judged as though the royal already stood on it, which
/// is exactly the question a king walking through them asks.
///
/// Params:
/// - state: &State         -> current position providing rights and occupancy
/// - out  : &mut Vec<Move> -> output list receiving castling moves
///
/// Notes:
/// Both wings run the same check against different table slots, spelled out
/// twice rather than looped over, the rights bit and the table index being
/// the only things that differ.
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
/// Plays a move and every consequence it has, then asks whether it was
/// allowed. Generation hands over pseudo-legal moves, so the answer cannot be
/// known before the board has changed: a move that turns out to leave its own
/// royal attacked takes itself back and reports false, and a caller therefore
/// only ever sees a position that has either advanced legally or not moved at
/// all.
///
/// ```text
/// save reversible fields → apply → push snapshot → legal?
///                                                  ├ yes: record a terminal
///                                                  │      result if any
///                                                  └ no : undo, return false
/// ```
///
/// The fields saved first are the ones no amount of replaying could recover:
/// castling rights, the en-passant square, the position, virgin and pawn
/// hashes, the halfmove, repetition and counting clocks, the game phase and
/// its score, the delivered-check tallies and the standing result. Everything
/// else about the position is rebuilt by undoing what was done.
///
/// Applying itself branches by move type — quiet, single capture, multi
/// capture, castling, drop — and each branch carries the whole position
/// forward as it goes: occupancy boards, royal lists, virgin flags, hands,
/// phase score, and the hashes, all updated by difference rather than
/// recomputed, since a search that recomputed them would spend its time here.
///
/// Legality is a single expression saying two things. The ordinary one is
/// that a move leaving the mover's own royal attacked is no move. The other
/// belongs to variants with stand-offs: standing in one and leaving it
/// standing is refused, entering a fresh one is fine, and passing while in one
/// is legal whatever else holds, that pass being how such a game is ended.
///
/// Params:
/// - state: &mut State -> position the move is applied to
/// - mv   : Move       -> encoded move to play
///
/// Return:
/// bool                -> true if legal; false after self-check rollback
///
/// Notes:
/// Call `undo_move!` only after a true return. A false return has already
/// restored the position and removed its temporary snapshot. Debug builds
/// check the whole state for internal agreement on the way in and on the way
/// out, so a bookkeeping slip surfaces at the move that caused it rather than
/// wherever the corruption is first read.
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

                if p_is_royal!($state.statics.pieces[piece_index]) {
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
/// Puts the position back the way `make_move!` found it. The fields it saved
/// come straight off the snapshot, which is the reason they were saved at all;
/// everything else is undone by running the move backwards. The piece returns
/// to its start square, anything taken is put back where it stood with the
/// virginity it had, a promotion is demoted to what promoted, a drop goes back
/// into the hand it came out of, and an unloaded piece is picked up again.
///
/// The snapshot is popped as it is read, so make and undo are a stack: one
/// undo per successful make, in the order they were made.
///
/// Params:
/// - state: &mut State -> position whose most recent move is reverted
///
/// Notes:
/// Call only after a successful `make_move!`; a rejected move has already
/// removed its own snapshot. Undoing with nothing left to undo panics rather
/// than rewinding into a position that never occurred.
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
/// Hands the turn over without moving anything. The search plays one to ask
/// what happens when a side does nothing at all: a position that still fails
/// high after the opponent has been given a free move is good enough to stop
/// looking at, so the board, the piece lists and the hands are left exactly
/// as they stand.
///
/// What does change is what passing changes. The en-passant square goes away,
/// the capture it offered having belonged to a move nobody made, and the side
/// to move flips, both written into the hash as they happen. The halfmove
/// clock is left alone, no move having been played for it to count, while the
/// repetition clock advances, a null move being as quiet as a ply can be.
///
/// The snapshot goes onto the same history stack real moves use, carrying a
/// null ply, so undoing is the same operation and plies are counted the same
/// way whether or not anything was played.
///
/// Params:
/// - state: &mut State -> position whose turn is passed to the opponent
///
/// Notes:
/// Legality is not asked here. Passing out of check is not something to prune
/// with, and keeping that judgement at the call site is what lets this stay a
/// bookkeeping macro.
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
/// Takes the turn back. Nothing was moved, so nothing has to be unmoved: the
/// saved fields are copied back off the snapshot, the side flips again, and
/// the position is the one the search was looking at before it asked its
/// question.
///
/// The snapshot is popped from the stack `undo_move!` reads, so a null move
/// has to be undone in its turn like any other ply.
///
/// Params:
/// - state: &mut State -> position whose most recent null move is reverted
///
/// Notes:
/// Panics when there is no snapshot left to pop.
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

/*----------------------------------------------------------------------------*\
                              FULL MOVE GENERATION
\*----------------------------------------------------------------------------*/

/// generate_all_moves_and_drops
///
/// Every pseudo-legal move the side to move has, board moves and drops alike.
/// Three sources feed the list, and which of them contribute depends on the
/// variant and on where the game is:
///
/// - board moves, one sweep per piece standing on a square, unless the game
///   is still in its setup phase and the board is being filled rather than
///   played on
/// - drops, wherever the variant has them, and during setup regardless, the
///   same mechanism placing an army that elsewhere returns a capture
/// - castling, wherever the variant has it
///
/// A terminal position generates nothing at all, which is what leaves
/// [`legal_moves!`] empty at the end of a game rather than full of moves
/// nobody is allowed to play.
///
/// Params:
/// - state  : &State         -> position to generate for
/// - out    : &mut Vec<Move> -> cleared, then filled with the moves
/// - scratch: &mut Vec<u64>  -> reusable multi-capture payload buffer
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
/// The capture-only counterpart, read by quiescence search where only forcing
/// moves are worth looking at. It walks the narrower `relevant_captures`
/// tables, so the quiet moves cost nothing to leave out: they were never
/// generated. Drops and castling take nothing and are not asked for at all.
///
/// Nothing comes out of a terminal position, and nothing out of the setup
/// phase either, where the moves on offer are placements rather than captures.
///
/// Params:
/// - state  : &State         -> position to generate for
/// - out    : &mut Vec<Move> -> cleared, then filled with the captures
/// - scratch: &mut Vec<u64>  -> reusable multi-capture payload buffer
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
