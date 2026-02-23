use bnum::types::U2048;
use bnum::cast::As;

use crate::{
    c, constants::{
        CASTLING, MULTI_CAPTURE_MOVE, QUIET_MOVE, SINGLE_CAPTURE_MOVE,
    }, count_limits, d, drop_e, drops, enc_capture_part, enc_created_enp, enc_creates_enp, enc_end, enc_is_initial, enc_move_type, enc_multi_move_captured_piece, enc_multi_move_captured_square, enc_multi_move_captured_unmoved, enc_multi_move_is_unload, enc_multi_move_unload_square, enc_piece, enc_promoted, enc_promotion, enc_start, enp_captured, enp_piece, enp_square, forbidden_zones, g, game::{
        drops::drop_list::generate_drop_list,
        representations::{
            moves::Move,
            piece::Piece,
            state::State,
            vector::{MoveSet, MoveVector},
        },
    }, get, i, k, l, m, multi_move_captured_square, not_g, not_i, not_k, not_v, p, p_can_promote, p_color, p_index, p_is_royal, p_promotions, promote_to_captured, promotions, r, t, u, v, x, y
};

#[macro_export]
macro_rules! is_square_attacked {
    (
        $square:expr,
        $attacked_side:expr,
        $attacked_unmoved:expr,
        $attacked_royal:expr,
        $attacked_rank:expr,
        $game_state:expr
    ) => {{
        let possible_attacks = &$game_state.relevant_attacks
            [$attacked_side as usize][$square as usize];

        let mut attacked = false;
        for (piece_index, start, move_vector) in possible_attacks {
            if $game_state.main_board[*start as usize] != *piece_index {
                continue;
            }

            let piece = &$game_state.pieces[*piece_index as usize];

            if validate_attack_vector(
                move_vector, *start, piece,
                $attacked_unmoved, $attacked_royal, $attacked_rank,
                $square, $game_state,
            )
            {
                attacked = true;
                break;
            }
        }

        attacked
    }};
}

#[macro_export]
macro_rules! is_in_check {
    ($side:expr, $game_state:expr) => {{
        let monarch_indices = &$game_state.royal_list[$side as usize];

        if monarch_indices.len() > 1 || $game_state.setup_phase {
            false
        } else {
            let royal_piece = &$game_state.main_board[monarch_indices[0] as usize];
            let royal_rank = &$game_state.pieces[*royal_piece as usize].rank;

            is_square_attacked!(
                monarch_indices[0] as u32,
                $side,
                get!($game_state.virgin_board, monarch_indices[0] as u32),
                true,
                *royal_rank,
                $game_state
            )
        }
    }};
}

pub fn generate_relevant_moves(
    piece: &Piece,
    square_index: u32,
    game_state: &State
) -> MoveSet {
    let piece_index = p_index!(piece) as usize;
    let piece_color = p_color!(piece);
    let vector_set =
        &game_state.piece_moves[piece_index];

    let mut result = MoveSet::new();
    'multi_leg: for multi_leg_vector in vector_set {
        let mut accumulated_index = square_index as i32;

        let mut file = accumulated_index % (game_state.files as i32);
        let mut rank = accumulated_index / (game_state.files as i32);
        let mut index_to_set = Vec::new();

        for leg in multi_leg_vector {
            let file_offset = x!(leg);
            let rank_offset = y!(leg);

            let bypass = v!(leg) && not_v!(leg);

            file += file_offset as i32 * (-2 * piece_color as i32 + 1);
            rank += rank_offset as i32 * (-2 * piece_color as i32 + 1);
            accumulated_index = rank * (game_state.files as i32) + file;

            if  file < 0 || file >= game_state.files as i32 ||
                rank < 0 || rank >= game_state.ranks as i32 ||
                (forbidden_zones!(game_state) &&
                get!(
                    game_state.forbidden_zones[piece_index],
                    accumulated_index as u32
                ) && !bypass)
            {
                continue 'multi_leg;
            }

            index_to_set.push(accumulated_index as u32);
        }

        result.push(multi_leg_vector.clone());
    }

    result.sort_by_key(|v| -(v.len() as isize));
    result
}

pub fn generate_attack_masks(
    square_index: u16,
    game_state: &mut State,
) {
    for piece in &game_state.pieces {
        let piece_index = p_index!(piece);
        let piece_color = p_color!(piece);

        let vector_set =
            &game_state.relevant_moves
            [piece_index as usize][square_index as usize];

        for multi_leg_vector in vector_set {
            let mut accumulated_index = square_index as i16;

            let leg_count = multi_leg_vector.len();

            for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
                let last_leg = leg_index + 1 == leg_count;

                let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
                let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

                accumulated_index += (
                    rank_offset * (game_state.files as i8) + file_offset
                ) as i16;

                let c = c!(leg) || (last_leg && !m!(leg));
                let d = d!(leg);

                if d {
                    game_state.relevant_attacks[piece_color as usize]
                        [accumulated_index as usize]
                        .push(
                            (
                                piece_index,
                                square_index,
                                multi_leg_vector.to_vec()
                            )
                        );
                }

                if c {
                    game_state.relevant_attacks[1 - piece_color as usize]
                        [accumulated_index as usize]
                        .push(
                            (
                                piece_index,
                                square_index,
                                multi_leg_vector.to_vec()
                            )
                        );
                }
            }
        }
    }
}

#[inline(always)]
pub fn validate_attack_vector(
    multi_leg_vector: &MoveVector,
    square_index: u16,
    attacking_piece: &Piece,
    attacked_unmoved: bool,
    attacked_royal: bool,
    attacked_rank: u8,
    attacked_square: u32,
    game_state: &State,
) -> bool {

    let piece_color = p_color!(attacking_piece);
    let piece_rank = attacking_piece.rank;
    let piece_unmoved = get!(game_state.virgin_board, square_index as u32);

    let mut accumulated_index = square_index as i16;
    let mut target_was_last_captured = false;

    let leg_count = multi_leg_vector.len();

    for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
        let last_leg = leg_index + 1 == leg_count;

        let start_square = accumulated_index as u32;

        let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
        let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

        accumulated_index += (
            rank_offset * (game_state.files as i8) + file_offset
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
        let special_i = i && not_i;
        let special_v = v && not_v;

        if (i && !piece_unmoved || not_i && piece_unmoved) && !special_i
        {
            return false;
        }

        let friendly = get!(
            game_state.pieces_board[piece_color as usize],
            end_square
        ) && end_square != start_square;
        let enemy = get!(
            game_state.pieces_board[1 - piece_color as usize],
            end_square
        );
        let empty = !friendly && !enemy;

        let null_move = file_offset == 0 && rank_offset == 0;
        let imaginary_move = end_square == attacked_square;

        if u && target_was_last_captured {
            return false;
        }

        if imaginary_move {
            if k && !attacked_royal|| not_k && attacked_royal
            || g && piece_rank >= attacked_rank
            || not_g && piece_rank < attacked_rank
            || (v && !attacked_unmoved || not_v && attacked_unmoved)
            && !special_v
            || u
            {
                return false;
            }

            target_was_last_captured = true;
            continue;
        }

        if empty && !null_move {
            if t && enp_square!(game_state.en_passant_square) == end_square
            {
                let capt_piece_index =
                    enp_piece!(game_state.en_passant_square);
                let capt_piece_color =
                    p_color!(game_state.pieces[capt_piece_index as usize]);

                if d && capt_piece_color == piece_color
                || c && capt_piece_color != piece_color
                {
                    let capt_piece =
                        &game_state.pieces[capt_piece_index as usize];
                    let capt_unmoved =
                        get!(
                            game_state.virgin_board,
                            enp_captured!(game_state.en_passant_square)
                        );
                    let capt_rank = capt_piece.rank;
                    let capt_royal =
                        p_is_royal!(capt_piece);

                    if k && !capt_royal || not_k && capt_royal
                    || g && piece_rank >= capt_rank
                    || not_g && piece_rank < capt_rank
                    || (v && !capt_unmoved || not_v && capt_unmoved)
                    && !special_v
                    {
                        return false;
                    }

                    target_was_last_captured = false;
                } else {
                    return false;
                }
            } else if !m {
                return false;
            }
        } else if friendly && !null_move{
            if !d {
                return false;
            }

            let capt_piece_index =
                game_state.main_board[end_square as usize];
            let capt_piece =
                &game_state.pieces[capt_piece_index as usize];
            let capt_unmoved =
                get!(game_state.virgin_board, end_square);
            let capt_rank = capt_piece.rank;
            let capt_royal =
                p_is_royal!(capt_piece);

            if k && !capt_royal || not_k && capt_royal
            || g && piece_rank >= capt_rank
            || not_g && piece_rank < capt_rank
            || (v && !capt_unmoved || not_v && capt_unmoved)
            && !special_v
            {
                return false;
            }

            target_was_last_captured = false;
        } else if enemy && !null_move {
            if !c {
                return false;
            }

            let capt_piece_index =
                game_state.main_board[end_square as usize];
            let capt_piece =
                &game_state.pieces[capt_piece_index as usize];
            let capt_unmoved =
                get!(game_state.virgin_board, end_square);
            let capt_rank = capt_piece.rank;
            let capt_royal =
                p_is_royal!(capt_piece);

            if k && !capt_royal || not_k && capt_royal
            || g && piece_rank >= capt_rank
            || not_g && piece_rank < capt_rank
            || (v && !capt_unmoved || not_v && capt_unmoved)
            && !special_v
            {
                return false;
            }

            target_was_last_captured = false;
        }
    }

    true
}

#[inline(always)]
pub fn generate_move_list(
    square_index: u16,
    piece: &Piece,
    game_state: &State,
) -> Vec<Move> {
    let mut result = Vec::new();

    let piece_index = p_index!(piece);
    let piece_color = p_color!(piece);
    let piece_rank = piece.rank;
    let piece_unmoved = get!(game_state.virgin_board, square_index as u32);

    let vector_set =
        &game_state.relevant_moves
        [piece_index as usize][square_index as usize];

    'multi_leg: for multi_leg_vector in vector_set {

        let mut encoded_move = Move::default();
        enc_start!(encoded_move, square_index as u128);
        enc_piece!(encoded_move, piece_index as u128);

        let mut taken_pieces: Vec<u64> = Vec::new();

        let mut accumulated_index = square_index as i16;

        let leg_count = multi_leg_vector.len();

        for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
            let last_leg = leg_index + 1 == leg_count;
            let mut taken_piece = 0u64;

            let start_square = accumulated_index as u32;

            let file_offset = x!(leg) * (-2 * piece_color as i8 + 1);
            let rank_offset = y!(leg) * (-2 * piece_color as i8 + 1);

            accumulated_index += (
                rank_offset * (game_state.files as i8) + file_offset
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
            let l = l!(leg);
            let r = r!(leg);
            let special_i = i && not_i;
            let special_v = v && not_v;
            let castling_rights = piece_color as usize * 2 + l as usize;

            if (i && !piece_unmoved || not_i && piece_unmoved) && !special_i
            || (special_i
            && is_square_attacked!(
                accumulated_index as u32,
                piece_color,
                piece_unmoved,
                p_is_royal!(piece),
                piece_rank,
                game_state
            ))
            || ((l || r)
            && game_state.castling_state & CASTLING[castling_rights] == 0)
            {
                continue 'multi_leg;
            }

            enc_is_initial!(
                encoded_move, (i | piece_unmoved) as u128
            );

            let friendly = get!(
                game_state.pieces_board[piece_color as usize],
                end_square
            ) && end_square != start_square;
            let enemy = get!(
                game_state.pieces_board[1 - piece_color as usize],
                end_square
            );
            let empty = !friendly && !enemy;

            let null_move = file_offset == 0 && rank_offset == 0;

            if empty && !null_move {
                if t && enp_square!(game_state.en_passant_square) == end_square
                {
                    let capt_piece_index =
                        enp_piece!(game_state.en_passant_square);
                    let capt_piece_color =
                        p_color!(game_state.pieces[capt_piece_index as usize]);

                    if d && capt_piece_color == piece_color
                    || c && capt_piece_color != piece_color
                    {
                        enc_multi_move_captured_piece!(
                            taken_piece,
                            capt_piece_index as u64
                        );

                        enc_multi_move_captured_square!(
                            taken_piece,
                            enp_captured!(game_state.en_passant_square) as u64
                        );

                        let capt_piece =
                            &game_state.pieces[capt_piece_index as usize];
                        let capt_unmoved =
                            get!(
                                game_state.virgin_board,
                                enp_captured!(game_state.en_passant_square)
                            );
                        let capt_rank = capt_piece.rank;
                        let capt_royal =
                            p_is_royal!(capt_piece);

                        if k && !capt_royal || not_k && capt_royal
                        || g && piece_rank >= capt_rank
                        || not_g && piece_rank < capt_rank
                        || (v && !capt_unmoved || not_v && capt_unmoved)
                        && !special_v
                        {
                            continue 'multi_leg;
                        }

                        enc_multi_move_captured_unmoved!(
                            taken_piece,
                            capt_unmoved as u64
                        );

                        taken_pieces.push(taken_piece);
                    } else {
                        continue 'multi_leg;
                    }
                } else if !m {
                    continue 'multi_leg;
                }
            } else if friendly && !null_move {
                if !d {
                    continue 'multi_leg;
                }

                let capt_piece_index =
                    game_state.main_board[end_square as usize];
                let capt_piece =
                    &game_state.pieces[capt_piece_index as usize];
                let capt_unmoved =
                    get!(game_state.virgin_board, end_square);
                let capt_rank = capt_piece.rank;
                let capt_royal =
                    p_is_royal!(capt_piece);

                if k && !capt_royal || not_k && capt_royal
                || g && piece_rank >= capt_rank
                || not_g && piece_rank < capt_rank
                || (v && !capt_unmoved || not_v && capt_unmoved)
                && !special_v
                {
                    continue 'multi_leg;
                }

                enc_multi_move_captured_piece!(
                     taken_piece,
                     capt_piece_index as u64
                 );

                enc_multi_move_captured_square!(
                    taken_piece,
                    end_square as u64
                );

                enc_multi_move_captured_unmoved!(
                    taken_piece,
                    capt_unmoved as u64
                );

                taken_pieces.push(taken_piece);
            } else if enemy && !null_move {
                if !c {
                    continue 'multi_leg;
                }

                let capt_piece_index =
                    game_state.main_board[end_square as usize];
                let capt_piece =
                    &game_state.pieces[capt_piece_index as usize];
                let capt_unmoved =
                    get!(game_state.virgin_board, end_square);
                let capt_rank = capt_piece.rank;
                let capt_royal =
                    p_is_royal!(capt_piece);

                if k && !capt_royal || not_k && capt_royal
                || g && piece_rank >= capt_rank
                || not_g && piece_rank < capt_rank
                || (v && !capt_unmoved || not_v && capt_unmoved)
                && !special_v
                {
                    continue 'multi_leg;
                }

                 enc_multi_move_captured_piece!(
                     taken_piece,
                     capt_piece_index as u64
                 );

                enc_multi_move_captured_square!(
                    taken_piece,
                    end_square as u64
                );

                enc_multi_move_captured_unmoved!(
                    taken_piece,
                    capt_unmoved as u64
                );

                taken_pieces.push(taken_piece);
            }

            if u {
                let mut last_captured = taken_pieces.pop().unwrap();
                let captured_square =
                    multi_move_captured_square!(last_captured);

                if start_square != captured_square as u32 {
                    enc_multi_move_is_unload!(last_captured, 1);
                    enc_multi_move_unload_square!(
                        last_captured,
                        start_square as u64
                    );

                    taken_pieces.push(last_captured);
                }
            }

            enc_creates_enp!(encoded_move, p as u128);
            enc_created_enp!(encoded_move, p as u128 *
                ((start_square as u128 & 0xFFF)
                | (accumulated_index as u128) << 12
                | (piece_index as u128) << 24)
            );
        }

        enc_end!(encoded_move, accumulated_index as u128);

        if promotions!(game_state) && p_can_promote!(piece) {
            if taken_pieces.is_empty() {
                enc_move_type!(encoded_move, QUIET_MOVE);
            } else if taken_pieces.len() == 1 {
                enc_move_type!(encoded_move, SINGLE_CAPTURE_MOVE);
                enc_capture_part!(encoded_move, taken_pieces[0] as u128);
            } else {
                enc_move_type!(encoded_move, MULTI_CAPTURE_MOVE);
                encoded_move.1 = taken_pieces;
            }

            let entered_mandatory_promotion =
                get!(
                    game_state.promotion_zones_mandatory[piece_index as usize],
                    accumulated_index as u32
                );
            let moved_from_mandatory_zone =
                get!(
                    game_state.promotion_zones_mandatory[piece_index as usize],
                    square_index as u32
                );

            let entered_optional_promotion =
                get!(
                    game_state.promotion_zones_optional[piece_index as usize],
                    accumulated_index as u32
                );
            let moved_from_optional_zone =
                get!(
                    game_state.promotion_zones_optional[piece_index as usize],
                    square_index as u32
                );

            let mandatory =
                entered_mandatory_promotion || moved_from_mandatory_zone;
            let optional =
                entered_optional_promotion || moved_from_optional_zone;

            if mandatory || optional {
                for promo_piece_index in p_promotions!(piece) {

                    let mut can_promote = true;

                    if count_limits!(game_state) {
                        can_promote = can_promote &&
                        (game_state.piece_count[promo_piece_index] as i32) <
                        game_state.piece_limit[promo_piece_index];
                    }

                    if promote_to_captured!(game_state) {
                        let enemy_equiv =
                            game_state.piece_swap_map[
                                &(promo_piece_index as u8)
                            ];

                        can_promote = can_promote &&
                            game_state.piece_in_hand
                            [1 - piece_color as usize]
                            [enemy_equiv as usize] > 0;
                    }

                    if can_promote {
                        let mut promo_move = Move::default();
                        promo_move.0 = encoded_move.0;
                        promo_move.1 = Vec::new();

                        for taken_piece in &encoded_move.1 {
                            promo_move.1.push(*taken_piece);
                        }

                        enc_promotion!(promo_move, 1);
                        enc_promoted!(promo_move, promo_piece_index as u128);
                        result.push(promo_move);
                    }
                }

                if !mandatory {
                    result.push(encoded_move);
                }
            } else {
                result.push(encoded_move);
            }

        } else {
            if taken_pieces.is_empty() {
                enc_move_type!(encoded_move, QUIET_MOVE);
            } else if taken_pieces.len() == 1 {
                enc_move_type!(encoded_move, SINGLE_CAPTURE_MOVE);
                enc_capture_part!(encoded_move, taken_pieces[0] as u128);
            } else {
                enc_move_type!(encoded_move, MULTI_CAPTURE_MOVE);
                encoded_move.1 = taken_pieces;
            }

            result.push(encoded_move);
        }
    }

    result
}

#[macro_export]
macro_rules! make_move {
    ($state:expr, $mv:expr) => {
        {
            $state.ply += 1;
            $state.ply_counter += 1;

            let last_en_passant_square = $state.en_passant_square;
            let last_halfmove_clock = $state.halfmove_clock;
            let last_castling_state = $state.castling_state;
            let last_position_hash = $state.position_hash;
            let last_setup_phase = $state.setup_phase;
            let last_game_over = $state.game_over;

            #[cfg(debug_assertions)]
            verify_game_state($state);

            let move_type = move_type!($mv);
            let null_move = is_null!($mv);

            if move_type == QUIET_MOVE {
                let piece_index = piece!($mv) as usize;
                let start_square = start!($mv) as u32;
                let end_square = end!($mv) as u32;
                let is_promotion = promotion!($mv);
                let creates_enp = creates_enp!($mv);
                let promoted_piece = promoted!($mv) as usize;
                let enp_square = created_enp!($mv) as u32;

                let piece_color =
                    p_color!($state.pieces[piece_index]);
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

                if p_is_royal!($state.pieces[piece_index]) {
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
                    $state, piece_index, start_square as u16
                );
                hash_in_or_out_piece!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as u16
                );

                $state.main_board[start_square as usize] = NO_PIECE;
                $state.main_board[end_square as usize] =
                    if is_promotion { promoted_piece as u8 }
                    else { piece_index as u8 };

                if is_promotion {
                    let old_piece = &$state.pieces[piece_index];
                    let new_piece = &$state.pieces[promoted_piece];

                    if p_is_big!(old_piece) {
                    $state.big_pieces[piece_color as usize] -= 1;
                    }
                    if p_is_major!(old_piece) {
                    $state.major_pieces[piece_color as usize] -= 1;
                    } else if p_is_minor!(old_piece) {
                    $state.minor_pieces[piece_color as usize] -= 1;
                    }

                    if p_is_big!(new_piece) {
                    $state.big_pieces[piece_color as usize] += 1;
                    }
                    if p_is_major!(new_piece) {
                    $state.major_pieces[piece_color as usize] += 1;
                    } else if p_is_minor!(new_piece) {
                    $state.minor_pieces[piece_color as usize] += 1;
                    }

                    $state.material[piece_color as usize] -=
                        p_value!(old_piece) as u32;
                    $state.material[piece_color as usize] +=
                        p_value!(new_piece) as u32;

                    $state.piece_count[promoted_piece] += 1;
                    $state.piece_count[piece_index] -= 1;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.piece_swap_map
                            [&(promoted_piece as u8)];
                        let hand =
                            &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];
                        *hand -= 1;

                        hash_update_in_hand!(
                            $state,
                            enemy_equiv as usize,
                            *hand + 1,
                            *hand
                        );
                    }
                }

                $state.piece_list[piece_index]
                    .retain(|&sq| sq != start_square as u16);
                $state.piece_list[
                    if is_promotion { promoted_piece } else { piece_index }
                ].push(end_square as u16);

                if p_can_promote!($state.pieces[piece_index]) {
                    $state.halfmove_clock = 0;
                } else {
                    $state.halfmove_clock += 1;
                }

                if piece_unmoved {
                    match (
                        p_castle_right!($state.pieces[piece_index]),
                        p_castle_left!($state.pieces[piece_index])
                    ) {
                        (true, true) => {
                            $state.castling_state &= !(
                                [WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE]
                                [piece_color as usize]
                            );
                        },
                        (true, false) => {
                            let rights = [!WK_CASTLE, !BK_CASTLE]
                            [piece_color as usize];
                            $state.castling_state &= rights;
                        },
                        (false, true) => {
                            let rights = [!WQ_CASTLE, !BQ_CASTLE]
                            [piece_color as usize];
                            $state.castling_state &= rights;
                        },
                        (false, false) => {
                            let start_rank =
                                (start_square as u16) / $state.files as u16;
                            let start_file =
                                (start_square as u16) % $state.files as u16;

                            let is_queenside = start_file == 0;
                            let rights = [
                                WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE
                            ][piece_color as usize * 2 + is_queenside as usize];

                            if  start_rank == 0
                            && (start_file == 0
                            || start_file == $state.files as u16 - 1)
                            || start_rank == $state.ranks as u16 - 1
                            && (start_file == 0
                            || start_file == $state.files as u16 - 1)
                            {
                                $state.castling_state &= !rights;
                            }
                        }
                    }
                }

                hash_update_castling!(
                    $state, last_castling_state, $state.castling_state
                );

            } else if move_type == SINGLE_CAPTURE_MOVE {
                let piece_index = piece!($mv) as usize;
                let start_square = start!($mv) as u32;
                let end_square = end!($mv) as u32;
                let is_promotion = promotion!($mv);
                let creates_enp = creates_enp!($mv);
                let promoted_piece = promoted!($mv) as usize;
                let enp_square = created_enp!($mv) as u32;
                let captured_piece_index = captured_piece!($mv) as usize;
                let captured_square = captured_square!($mv) as u32;
                let is_unload = is_unload!($mv);
                let unload_square = unload_square!($mv) as u32;

                let piece_color = p_color!($state.pieces[piece_index]);
                let piece_unmoved = get!(
                    $state.virgin_board, start_square
                );
                let captured_color = p_color!(
                    $state.pieces[captured_piece_index]
                );

                clear!(
                    $state.pieces_board[piece_color as usize], start_square
                );
                set!(
                    $state.pieces_board[piece_color as usize], end_square
                );

                if p_is_royal!($state.pieces[piece_index]) {
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
                    $state, piece_index, start_square as u16
                );
                hash_in_or_out_piece!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as u16
                );

                $state.main_board[start_square as usize] = NO_PIECE;
                $state.main_board[end_square as usize] =
                    if is_promotion { promoted_piece as u8 }
                    else { piece_index as u8 };

                if is_promotion {
                    let old_piece = &$state.pieces[piece_index];
                    let new_piece = &$state.pieces[promoted_piece];

                    if p_is_big!(old_piece) {
                    $state.big_pieces[piece_color as usize] -= 1;
                    }
                    if p_is_major!(old_piece) {
                    $state.major_pieces[piece_color as usize] -= 1;
                    } else if p_is_minor!(old_piece) {
                    $state.minor_pieces[piece_color as usize] -= 1;
                    }

                    if p_is_big!(new_piece) {
                    $state.big_pieces[piece_color as usize] += 1;
                    }
                    if p_is_major!(new_piece) {
                    $state.major_pieces[piece_color as usize] += 1;
                    } else if p_is_minor!(new_piece) {
                    $state.minor_pieces[piece_color as usize] += 1;
                    }

                    $state.material[piece_color as usize] -=
                        p_value!(old_piece) as u32;
                    $state.material[piece_color as usize] +=
                        p_value!(new_piece) as u32;

                    $state.piece_count[promoted_piece] += 1;
                    $state.piece_count[piece_index] -= 1;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.piece_swap_map
                            [&(promoted_piece as u8)];
                        let hand =
                            &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];
                        let old_hand = *hand;
                        *hand -= 1;

                        hash_update_in_hand!(
                            $state,
                            enemy_equiv as usize,
                            old_hand,
                            *hand
                        );
                    }
                }

                $state.piece_list[piece_index]
                    .retain(|&sq| sq != start_square as u16);
                $state.piece_list[
                    if is_promotion { promoted_piece } else { piece_index }
                ].push(end_square as u16);

                $state.halfmove_clock = 0;

                if piece_unmoved {
                    match (
                        p_castle_right!($state.pieces[piece_index]),
                        p_castle_left!($state.pieces[piece_index])
                    ) {
                        (true, true) => {
                            $state.castling_state &= !(
                                [WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE]
                                [piece_color as usize]
                            );
                        },
                        (true, false) => {
                            let rights = [!WK_CASTLE, !BK_CASTLE]
                            [piece_color as usize];
                            $state.castling_state &= rights;
                        },
                        (false, true) => {
                            let rights = [!WQ_CASTLE, !BQ_CASTLE]
                            [piece_color as usize];
                            $state.castling_state &= rights;
                        },
                        (false, false) => {
                            let start_rank =
                                (start_square as u16) / $state.files as u16;
                            let start_file =
                                (start_square as u16) % $state.files as u16;

                            let is_queenside = start_file == 0;
                            let rights = [
                                WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE
                            ][piece_color as usize * 2 + is_queenside as usize];

                            if  start_rank == 0
                            && (start_file == 0
                            || start_file == $state.files as u16 - 1)
                            || start_rank == $state.ranks as u16 - 1
                            && (start_file == 0
                            || start_file == $state.files as u16 - 1)
                            {
                                $state.castling_state &= !rights;
                            }
                        }
                    }
                }

                if drops!($state) || promote_to_captured!($state) {
                    let mut captured_index_u8 = captured_piece_index as u8;

                    if demote_upon_capture!($state) {
                        captured_index_u8 = $state.piece_demotion_map
                            .get(&(captured_index_u8))
                            .unwrap()[0];                                       /* assume 1 to 1 mapping              */
                    }

                    let swap_index =
                        *$state.piece_swap_map
                        .get(&captured_index_u8).unwrap() as usize;
                    let hand =
                        &mut $state.piece_in_hand
                        [piece_color as usize][swap_index];
                    let old_hand = *hand;
                    *hand += 1;

                    hash_update_in_hand!(
                        $state,
                        captured_index_u8 as usize,
                        old_hand,
                        *hand
                    );
                }

                let end_rank = (captured_square as u16) / $state.files as u16;
                let end_file = (captured_square as u16) % $state.files as u16;

                if castling!($state)
                    && get!($state.virgin_board, captured_square)
                    && (end_rank == 0 || end_rank == $state.ranks as u16 - 1)
                    && (end_file == 0 || end_file == $state.files as u16 - 1)
                {
                    let is_queenside = end_file == 0;
                    let rights = [WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE]
                    [
                        (end_rank == $state.ranks as u16 - 1) as usize * 2
                        + is_queenside as usize
                    ];
                    $state.castling_state &= !rights;
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
                    captured_piece_index,
                    captured_square as u16
                );

                clear!($state.virgin_board, captured_square);

                if is_unload {
                    set!(
                        $state.pieces_board[captured_color as usize],
                        unload_square
                    );

                    hash_in_or_out_piece!(
                        $state,
                        captured_piece_index,
                        unload_square as u16
                    );

                    set!($state.virgin_board, unload_square);
                }

                if is_unload {
                    $state.main_board[unload_square as usize] =
                        captured_piece_index as u8;
                    $state.piece_list[captured_piece_index].retain(
                        |&sq| sq != captured_square as u16
                    );
                    $state.piece_list[captured_piece_index]
                        .push(unload_square as u16);
                } else {
                    $state.piece_list[captured_piece_index].retain(
                        |&sq| sq != captured_square as u16
                    );
                }

                if !is_unload {
                    if p_is_big!($state.pieces[captured_piece_index]) {
                        $state.big_pieces[captured_color as usize] -= 1;
                    }
                    if p_is_major!($state.pieces[captured_piece_index]) {
                        $state.major_pieces[captured_color as usize] -= 1;
                    } else if p_is_minor!(
                        $state.pieces[captured_piece_index]
                    ) {
                        $state.minor_pieces[captured_color as usize] -= 1;
                    }

                    $state.material[captured_color as usize] -=
                        p_value!(
                            $state.pieces[captured_piece_index]
                        ) as u32;

                    $state.piece_count[captured_piece_index] -= 1;
                }
            } else if move_type == MULTI_CAPTURE_MOVE {
                let piece_index = piece!($mv) as usize;
                let start_square = start!($mv) as u32;
                let end_square = end!($mv) as u32;
                let is_promotion = promotion!($mv);
                let creates_enp = creates_enp!($mv);
                let promoted_piece = promoted!($mv) as usize;
                let enp_square = created_enp!($mv) as u32;

                let piece_color = p_color!($state.pieces[piece_index]);
                let piece_unmoved = get!(
                    $state.virgin_board, start_square
                );

                clear!(
                    $state.pieces_board[piece_color as usize], start_square
                );
                set!(
                    $state.pieces_board[piece_color as usize], end_square
                );

                if p_is_royal!($state.pieces[piece_index]) {
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
                    $state, piece_index, start_square as u16
                );
                hash_in_or_out_piece!(
                    $state,
                    if is_promotion { promoted_piece } else { piece_index },
                    end_square as u16
                );

                $state.main_board[start_square as usize] = NO_PIECE;
                $state.main_board[end_square as usize] =
                    if is_promotion { promoted_piece as u8 }
                    else { piece_index as u8 };

                if is_promotion {
                    let old_piece = &$state.pieces[piece_index];
                    let new_piece = &$state.pieces[promoted_piece];

                    if p_is_big!(old_piece) {
                    $state.big_pieces[piece_color as usize] -= 1;
                    }
                    if p_is_major!(old_piece) {
                    $state.major_pieces[piece_color as usize] -= 1;
                    } else if p_is_minor!(old_piece) {
                    $state.minor_pieces[piece_color as usize] -= 1;
                    }

                    if p_is_big!(new_piece) {
                    $state.big_pieces[piece_color as usize] += 1;
                    }
                    if p_is_major!(new_piece) {
                    $state.major_pieces[piece_color as usize] += 1;
                    } else if p_is_minor!(new_piece) {
                    $state.minor_pieces[piece_color as usize] += 1;
                    }

                    $state.material[piece_color as usize] -=
                        p_value!(old_piece) as u32;
                    $state.material[piece_color as usize] +=
                        p_value!(new_piece) as u32;

                    $state.piece_count[promoted_piece] += 1;
                    $state.piece_count[piece_index] -= 1;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.piece_swap_map
                            [&(promoted_piece as u8)];
                        let hand =
                            &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];
                        let old_hand = *hand;
                        *hand -= 1;

                        hash_update_in_hand!(
                            $state,
                            enemy_equiv as usize,
                            old_hand,
                            *hand
                        );
                    }
                }

                $state.piece_list[piece_index]
                    .retain(|&sq| sq != start_square as u16);
                $state.piece_list[
                    if is_promotion { promoted_piece } else { piece_index }
                ].push(end_square as u16);

                $state.halfmove_clock = 0;

                if piece_unmoved {
                    match (
                        p_castle_right!($state.pieces[piece_index]),
                        p_castle_left!($state.pieces[piece_index])
                    ) {
                        (true, true) => {
                            $state.castling_state &= !(
                                [WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE]
                                [piece_color as usize]
                            );
                        },
                        (true, false) => {
                            let rights = [!WK_CASTLE, !BK_CASTLE]
                            [piece_color as usize];
                            $state.castling_state &= rights;
                        },
                        (false, true) => {
                            let rights = [!WQ_CASTLE, !BQ_CASTLE]
                            [piece_color as usize];
                            $state.castling_state &= rights;
                        },
                        (false, false) => {
                            let start_rank =
                                (start_square as u16) /
                                $state.files as u16;
                            let start_file =
                                (start_square as u16) %
                                $state.files as u16;

                            let is_queenside = start_file == 0;
                            let rights = [
                                WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE
                            ][piece_color as usize * 2 + is_queenside as usize];

                            if  start_rank == 0
                            && (start_file == 0
                            || start_file == $state.files as u16 - 1)
                            || start_rank == $state.ranks as u16 - 1
                            && (start_file == 0
                            || start_file == $state.files as u16 - 1)
                            {
                                $state.castling_state &= !rights;
                            }
                        }
                    }
                }

                for cap in &$mv.1 {
                    let captured_piece_index =
                        multi_move_captured_piece!(cap) as usize;
                    let captured_square =
                        multi_move_captured_square!(cap) as u32;
                    let is_unload = multi_move_is_unload!(cap);
                    let unload_square = multi_move_unload_square!(cap) as u32;
                    let captured_color = p_color!(
                        $state.pieces[captured_piece_index]
                    );

                    if drops!($state)
                    || promote_to_captured!($state)
                    {
                        let mut captured_index_u8 = captured_piece_index as u8;

                        if demote_upon_capture!($state) {
                            captured_index_u8 = $state.piece_demotion_map
                                .get(&(captured_index_u8))
                                .unwrap()[0];                                   /* assume 1 to 1 mapping              */
                        }

                        let swap_index =
                            *$state.piece_swap_map
                            .get(&captured_index_u8)
                            .unwrap() as usize;
                        let hand =
                            &mut $state.piece_in_hand
                            [piece_color as usize][swap_index];
                        let old_hand = *hand;
                        *hand += 1;

                        hash_update_in_hand!(
                            $state,
                            captured_index_u8 as usize,
                            old_hand,
                            *hand
                        );
                    }

                    let end_rank = (captured_square as u16) /
                        $state.files as u16;
                    let end_file = (captured_square as u16) %
                        $state.files as u16;

                    if castling!($state)
                        && get!($state.virgin_board, captured_square)
                        && (end_rank == 0
                        || end_rank == $state.ranks as u16 - 1)
                        && (end_file == 0
                        || end_file == $state.files as u16 - 1)
                    {
                        let is_queenside = end_file == 0;
                        let rights = [
                            WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE
                        ]
                        [
                            (
                                end_rank == $state.ranks as u16 - 1
                            ) as usize * 2
                            + is_queenside as usize
                        ];
                        $state.castling_state &= !rights;
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
                        captured_piece_index,
                        captured_square as u16
                    );

                    clear!($state.virgin_board, captured_square);

                    if is_unload {
                        set!(
                            $state.pieces_board[captured_color as usize],
                            unload_square
                        );

                        hash_in_or_out_piece!(
                            $state,
                            captured_piece_index,
                            unload_square as u16
                        );

                        set!($state.virgin_board, unload_square);
                    }

                    if is_unload {
                        $state.main_board[unload_square as usize] =
                            captured_piece_index as u8;
                        $state.piece_list[captured_piece_index].retain(
                            |&sq| sq != captured_square as u16
                        );
                        $state.piece_list[captured_piece_index]
                            .push(unload_square as u16);
                    } else {
                        $state.piece_list[captured_piece_index].retain(
                            |&sq| sq != captured_square as u16
                        );
                    }

                    if !is_unload {
                        if p_is_big!($state.pieces[captured_piece_index]) {
                            $state.big_pieces[captured_color as usize] -= 1;
                        }
                        if p_is_major!($state.pieces[captured_piece_index]) {
                            $state.major_pieces[captured_color as usize] -= 1;
                        } else if p_is_minor!(
                            $state.pieces[captured_piece_index]
                        ) {
                            $state.minor_pieces[captured_color as usize] -= 1;
                        }

                        $state.material[captured_color as usize] -=
                            p_value!(
                                $state.pieces[captured_piece_index]
                            ) as u32;

                        $state.piece_count[captured_piece_index] -= 1;
                    }
                }
            } else if move_type == DROP_MOVE {
                let piece_index = piece!($mv) as usize;
                let drop_square = start!($mv) as u32;

                let piece_color = p_color!($state.pieces[piece_index]);

                set!(
                    $state.pieces_board[piece_color as usize], drop_square
                );

                if p_is_royal!($state.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize]
                        .push(drop_square as Square);
                }

                hash_in_or_out_piece!(
                    $state, piece_index, drop_square as u16
                );

                $state.main_board[drop_square as usize] = piece_index as u8;
                $state.piece_list[piece_index].push(drop_square as u16);
                $state.material[piece_color as usize] +=
                    p_value!($state.pieces[piece_index]) as u32;
                $state.piece_count[piece_index] += 1;

                if p_is_big!($state.pieces[piece_index]) {
                    $state.big_pieces[piece_color as usize] += 1;
                }

                if p_is_major!($state.pieces[piece_index]) {
                    $state.major_pieces[piece_color as usize] += 1;
                } else if p_is_minor!($state.pieces[piece_index]) {
                    $state.minor_pieces[piece_color as usize] += 1;
                }

                if !drop_from_enemy_hand!($mv) {
                    let hand =
                        &mut $state.piece_in_hand
                        [piece_color as usize][piece_index];
                    let old_hand = *hand;
                    *hand -= 1;


                    hash_update_in_hand!(
                        $state,
                        piece_index,
                        old_hand,
                        *hand
                    );
                } else {
                    let enemy_equiv = $state.piece_swap_map
                        [&(piece_index as u8)] as usize;
                    let hand =
                        &mut $state.piece_in_hand
                        [1 - piece_color as usize][enemy_equiv];
                    let old_hand = *hand;
                    *hand -= 1;

                    hash_update_in_hand!(
                        $state,
                        enemy_equiv,
                        old_hand,
                        *hand
                    );
                }

                $state.halfmove_clock = 0;

                if $state.setup_phase
                && $state.piece_in_hand[0].is_empty()
                && $state.piece_in_hand[1].is_empty() {
                    $state.setup_phase = false;
                }

                for cap in &$mv.1 {
                    let captured_piece_index =
                        multi_move_captured_piece!(cap) as usize;
                    let captured_square =
                        multi_move_captured_square!(cap) as u32;
                    let captured_color = p_color!(
                        $state.pieces[captured_piece_index]
                    );

                    let end_rank = (captured_square as u16) /
                        $state.files as u16;
                    let end_file = (captured_square as u16) %
                        $state.files as u16;

                    if castling!($state)
                        && get!($state.virgin_board, captured_square)
                        && (end_rank == 0
                        || end_rank == $state.ranks as u16 - 1)
                        && (end_file == 0
                        || end_file == $state.files as u16 - 1)
                    {
                        let is_queenside = end_file == 0;
                        let rights = [
                            WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE
                        ]
                        [
                            (
                                end_rank == $state.ranks as u16 - 1
                            ) as usize * 2
                            + is_queenside as usize
                        ];
                        $state.castling_state &= !rights;
                    }

                    hash_update_castling!(
                        $state, last_castling_state,
                        $state.castling_state
                    );

                    if captured_square != drop_square {
                        $state.main_board[captured_square as usize] =
                            NO_PIECE;
                    }

                    if captured_square != drop_square
                    || captured_color != piece_color {
                        clear!(
                            $state.pieces_board[captured_color as usize],
                            captured_square
                        );
                    }

                    hash_in_or_out_piece!(
                        $state,
                        captured_piece_index,
                        captured_square as u16
                    );

                    clear!($state.virgin_board, captured_square);

                    $state.piece_list[captured_piece_index].retain(
                        |&sq| sq != captured_square as u16
                    );

                    if p_is_big!($state.pieces[captured_piece_index]) {
                        $state.big_pieces[captured_color as usize] -= 1;
                    }
                    if p_is_major!($state.pieces[captured_piece_index]) {
                        $state.major_pieces[captured_color as usize] -= 1;
                    } else if p_is_minor!(
                        $state.pieces[captured_piece_index]
                    ) {
                        $state.minor_pieces[captured_color as usize] -= 1;
                    }

                    $state.material[captured_color as usize] -=
                        p_value!(
                            $state.pieces[captured_piece_index]
                        ) as u32;

                    $state.piece_count[captured_piece_index] -= 1;
                }
            }

            $state.playing = 1 - $state.playing;

            hash_toggle_side!($state);

            let last_is_null = if let Some(last_mv) = $state.history.last() {
                null_snapshot!(last_mv)
            } else {
                false
            };

            if null_move && last_is_null {
                $state.game_over = true;
            }

            let snapshot: Snapshot = Snapshot {
                move_ply: $mv,
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                en_passant_square: last_en_passant_square,
                setup_phase: last_setup_phase,
                game_over: last_game_over,
                position_hash: last_position_hash
            };

            $state.history.push(snapshot);
            if !$state.setup_phase && is_in_check!(1 - $state.playing, $state) {
                undo_move!($state);
                false
            } else {
                #[cfg(debug_assertions)]
                verify_game_state($state);

                true
            }
        }
    };
}

#[macro_export]
macro_rules! undo_move {
    ($state:expr) => {
        {

            $state.ply -= 1;
            $state.ply_counter -= 1;

            let snapshot = $state.history.pop().unwrap_or_else(
                || panic!("No move to undo!")
            );

            #[cfg(debug_assertions)]
            verify_game_state($state);

            $state.playing = 1 - $state.playing;
            $state.castling_state = snapshot.castling_state;
            $state.halfmove_clock = snapshot.halfmove_clock;
            $state.en_passant_square = snapshot.en_passant_square;
            $state.position_hash = snapshot.position_hash;
            $state.setup_phase = snapshot.setup_phase;
            $state.game_over = snapshot.game_over;

            let mv = snapshot.move_ply;
            let move_type = move_type!(mv);

            if move_type == QUIET_MOVE {
                let piece_index = piece!(mv) as usize;
                let start_square = start!(mv) as u32;
                let end_square = end!(mv) as u32;
                let piece_unmoved = is_initial!(mv) == 1;
                let is_promotion = promotion!(mv);
                let promoted_piece = promoted!(mv) as usize;

                let piece_color = p_color!($state.pieces[piece_index]);

                clear!($state.pieces_board[piece_color as usize], end_square);
                set!($state.pieces_board[piece_color as usize], start_square);

                if p_is_royal!($state.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != end_square
                    );
                    $state.royal_list[piece_color as usize]
                        .push(start_square as Square);
                }

                if piece_unmoved {
                    set!($state.virgin_board, start_square);
                }

                $state.main_board[end_square as usize] = NO_PIECE;
                $state.main_board[start_square as usize] = piece_index as u8;

                if is_promotion {
                    $state.piece_list[promoted_piece]
                        .retain(|&sq| sq != end_square as u16);

                    if p_is_big!($state.pieces[promoted_piece]) {
                        $state.big_pieces[piece_color as usize] -= 1;
                    }
                    if p_is_major!($state.pieces[promoted_piece]) {
                        $state.major_pieces[piece_color as usize] -= 1;
                    } else if p_is_minor!($state.pieces[promoted_piece]) {
                        $state.minor_pieces[piece_color as usize] -= 1;
                    }

                    if p_is_big!($state.pieces[piece_index]) {
                        $state.big_pieces[piece_color as usize] += 1;
                    }
                    if p_is_major!($state.pieces[piece_index]) {
                        $state.major_pieces[piece_color as usize] += 1;
                    } else if p_is_minor!($state.pieces[piece_index]) {
                        $state.minor_pieces[piece_color as usize] += 1;
                    }

                    $state.material[piece_color as usize] +=
                        p_value!($state.pieces[piece_index]) as u32;
                    $state.material[piece_color as usize] -=
                        p_value!($state.pieces[promoted_piece]) as u32;

                    $state.piece_count[promoted_piece] -= 1;
                    $state.piece_count[piece_index] += 1;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.piece_swap_map
                            [&(promoted_piece as u8)];
                        let hand =
                            &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];
                        *hand += 1;
                    }
                } else {
                    $state.piece_list[piece_index]
                        .retain(|&sq| sq != end_square as u16);
                }

                $state.piece_list[piece_index]
                    .push(start_square as u16);
            } else if move_type == SINGLE_CAPTURE_MOVE {
                let piece_index = piece!(mv) as usize;
                let start_square = start!(mv) as u32;
                let end_square = end!(mv) as u32;
                let piece_unmoved = is_initial!(mv) == 1;
                let is_promotion = promotion!(mv);
                let promoted_piece = promoted!(mv) as usize;
                let captured_piece_index = captured_piece!(mv) as usize;
                let captured_square = captured_square!(mv) as u32;
                let captured_unmoved = captured_unmoved!(mv);
                let is_unload = is_unload!(mv);
                let unload_square = unload_square!(mv) as u32;

                let piece_color = p_color!($state.pieces[piece_index]);
                let captured_color = p_color!($state.pieces[captured_piece_index]);

                clear!($state.pieces_board[piece_color as usize], end_square);
                set!($state.pieces_board[piece_color as usize], start_square);

                if p_is_royal!($state.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != end_square
                    );
                    $state.royal_list[piece_color as usize]
                        .push(start_square as Square);
                }

                if piece_unmoved {
                    set!($state.virgin_board, start_square);
                }

                $state.main_board[end_square as usize] = NO_PIECE;
                $state.main_board[start_square as usize] = piece_index as u8;

                if is_promotion {
                    $state.piece_list[promoted_piece]
                        .retain(|&sq| sq != end_square as u16);

                    if p_is_big!($state.pieces[promoted_piece]) {
                        $state.big_pieces[piece_color as usize] -= 1;
                    }
                    if p_is_major!($state.pieces[promoted_piece]) {
                        $state.major_pieces[piece_color as usize] -= 1;
                    } else if p_is_minor!($state.pieces[promoted_piece]) {
                        $state.minor_pieces[piece_color as usize] -= 1;
                    }

                    if p_is_big!($state.pieces[piece_index]) {
                        $state.big_pieces[piece_color as usize] += 1;
                    }
                    if p_is_major!($state.pieces[piece_index]) {
                        $state.major_pieces[piece_color as usize] += 1;
                    } else if p_is_minor!($state.pieces[piece_index]) {
                        $state.minor_pieces[piece_color as usize] += 1;
                    }

                    $state.material[piece_color as usize] +=
                        p_value!($state.pieces[piece_index]) as u32;
                    $state.material[piece_color as usize] -=
                        p_value!($state.pieces[promoted_piece]) as u32;

                    $state.piece_count[promoted_piece] -= 1;
                    $state.piece_count[piece_index] += 1;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.piece_swap_map
                            [&(promoted_piece as u8)];
                        let hand =
                            &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];
                        *hand += 1;
                    }
                } else {
                    $state.piece_list[piece_index]
                        .retain(|&sq| sq != end_square as u16);
                }

                $state.piece_list[piece_index]
                    .push(start_square as u16);

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
                    captured_piece_index as u8;

                if is_unload {
                    $state.piece_list[captured_piece_index].retain(
                        |&sq| sq != unload_square as u16
                    );
                }
                $state.piece_list[captured_piece_index]
                    .push(captured_square as u16);

                if !is_unload {
                    if p_is_big!($state.pieces[captured_piece_index]) {
                        $state.big_pieces[captured_color as usize] += 1;
                    }

                    if p_is_major!($state.pieces[captured_piece_index]) {
                        $state.major_pieces[captured_color as usize] += 1;
                    } else if p_is_minor!($state.pieces[captured_piece_index]) {
                        $state.minor_pieces[captured_color as usize] += 1;
                    }

                    $state.material[captured_color as usize] +=
                        p_value!($state.pieces[captured_piece_index]) as u32;

                    $state.piece_count[captured_piece_index] += 1;
                }

                if drops!($state) || promote_to_captured!($state) {
                    let mut captured_index_u8 = captured_piece_index as u8;

                    if demote_upon_capture!($state) {
                        captured_index_u8 = $state.piece_demotion_map
                            .get(&(captured_index_u8))
                            .unwrap()[0];                                       /* assume 1 to 1 mapping              */
                    }

                    $state.piece_in_hand
                        [piece_color as usize][
                            *$state.piece_swap_map
                            .get(&captured_index_u8)
                            .unwrap() as usize
                        ] -= 1;
                }
            } else if move_type == MULTI_CAPTURE_MOVE {
                let piece_index = piece!(mv) as usize;
                let start_square = start!(mv) as u32;
                let end_square = end!(mv) as u32;
                let piece_unmoved = is_initial!(mv) == 1;
                let is_promotion = promotion!(mv);
                let promoted_piece = promoted!(mv) as usize;

                let piece_color = p_color!($state.pieces[piece_index]);

                clear!($state.pieces_board[piece_color as usize], end_square);
                set!($state.pieces_board[piece_color as usize], start_square);

                if p_is_royal!($state.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize].retain(
                        |&sq| sq as u32 != end_square
                    );
                    $state.royal_list[piece_color as usize]
                        .push(start_square as Square);
                }

                if piece_unmoved {
                    set!($state.virgin_board, start_square);
                }

                $state.main_board[end_square as usize] = NO_PIECE;
                $state.main_board[start_square as usize] = piece_index as u8;

                if is_promotion {
                    $state.piece_list[promoted_piece]
                        .retain(|&sq| sq != end_square as u16);

                    if p_is_big!($state.pieces[promoted_piece]) {
                        $state.big_pieces[piece_color as usize] -= 1;
                    }
                    if p_is_major!($state.pieces[promoted_piece]) {
                        $state.major_pieces[piece_color as usize] -= 1;
                    } else if p_is_minor!($state.pieces[promoted_piece]) {
                        $state.minor_pieces[piece_color as usize] -= 1;
                    }

                    if p_is_big!($state.pieces[piece_index]) {
                        $state.big_pieces[piece_color as usize] += 1;
                    }
                    if p_is_major!($state.pieces[piece_index]) {
                        $state.major_pieces[piece_color as usize] += 1;
                    } else if p_is_minor!($state.pieces[piece_index]) {
                        $state.minor_pieces[piece_color as usize] += 1;
                    }

                    $state.material[piece_color as usize] +=
                        p_value!($state.pieces[piece_index]) as u32;
                    $state.material[piece_color as usize] -=
                        p_value!($state.pieces[promoted_piece]) as u32;

                    $state.piece_count[promoted_piece] -= 1;
                    $state.piece_count[piece_index] += 1;

                    if promote_to_captured!($state) {
                        let enemy_equiv = $state.piece_swap_map
                            [&(promoted_piece as u8)];
                        let hand =
                            &mut $state.piece_in_hand
                            [1 - piece_color as usize][enemy_equiv as usize];
                        *hand += 1;
                    }
                } else {
                    $state.piece_list[piece_index]
                        .retain(|&sq| sq != end_square as u16);
                }

                $state.piece_list[piece_index]
                    .push(start_square as u16);

                for cap in &mv.1 {
                    let captured_piece_index =
                        multi_move_captured_piece!(cap) as usize;
                    let captured_square =
                        multi_move_captured_square!(cap) as u32;
                    let captured_unmoved = multi_move_captured_unmoved!(cap);
                    let is_unload = multi_move_is_unload!(cap);
                    let unload_square = multi_move_unload_square!(cap) as u32;
                    let captured_color = p_color!(
                        $state.pieces[captured_piece_index]
                    );

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
                        captured_piece_index as u8;

                    if is_unload {
                        $state.piece_list[captured_piece_index].retain(
                            |&sq| sq != unload_square as u16
                        );
                    }
                    $state.piece_list[captured_piece_index]
                        .push(captured_square as u16);

                    if !is_unload {
                        if p_is_big!($state.pieces[captured_piece_index]) {
                            $state.big_pieces[captured_color as usize] += 1;
                        }

                        if p_is_major!($state.pieces[captured_piece_index]) {
                            $state.major_pieces[captured_color as usize] += 1;
                        } else if p_is_minor!(
                            $state.pieces[captured_piece_index]
                        ) {
                            $state.minor_pieces[captured_color as usize] += 1;
                        }

                        $state.material[captured_color as usize] +=
                            p_value!(
                                $state.pieces[captured_piece_index]
                            ) as u32;

                        $state.piece_count[captured_piece_index] += 1;
                    }

                    if drops!($state) || promote_to_captured!($state) {
                        let mut captured_index_u8 = captured_piece_index as u8;

                        if demote_upon_capture!($state) {
                            captured_index_u8 = $state.piece_demotion_map
                                .get(&(captured_index_u8))
                                .unwrap()[0];
                        }

                        $state.piece_in_hand
                            [piece_color as usize]
                            [
                                *$state.piece_swap_map
                                .get(&captured_index_u8)
                                .unwrap() as usize
                            ] -= 1;
                    }
                }
            } else if move_type == DROP_MOVE {
                let piece_index = piece!(mv) as usize;
                let drop_square = start!(mv) as u32;

                let piece_color = p_color!($state.pieces[piece_index]);

                clear!($state.pieces_board[piece_color as usize], drop_square);

                if p_is_royal!($state.pieces[piece_index]) {
                    $state.royal_list[piece_color as usize]
                        .retain(|&sq| sq as u32 != drop_square);
                }

                $state.main_board[drop_square as usize] = NO_PIECE;
                $state.piece_list[piece_index]
                    .retain(|&sq| sq != drop_square as u16);
                $state.material[piece_color as usize] -=
                    p_value!($state.pieces[piece_index]) as u32;
                $state.piece_count[piece_index] -= 1;

                if p_is_big!($state.pieces[piece_index]) {
                    $state.big_pieces[piece_color as usize] -= 1;
                }

                if p_is_major!($state.pieces[piece_index]) {
                    $state.major_pieces[piece_color as usize] -= 1;
                } else if p_is_minor!($state.pieces[piece_index]) {
                    $state.minor_pieces[piece_color as usize] -= 1;
                }

                if !drop_from_enemy_hand!(mv) {
                    let hand =
                        &mut $state.piece_in_hand
                        [piece_color as usize][piece_index];
                    let old_hand = *hand;
                    *hand += 1;

                    hash_update_in_hand!(
                        $state,
                        piece_index,
                        old_hand,
                        *hand
                    );
                } else {
                    let enemy_equiv = $state.piece_swap_map
                        [&(piece_index as u8)] as usize;
                    let hand =
                        &mut $state.piece_in_hand
                        [1 - piece_color as usize][enemy_equiv];
                    let old_hand = *hand;
                    *hand += 1;

                    hash_update_in_hand!(
                        $state,
                        enemy_equiv,
                        old_hand,
                        *hand
                    );
                }

                for cap in &mv.1 {
                    let captured_piece_index =
                        multi_move_captured_piece!(cap) as usize;
                    let captured_square =
                        multi_move_captured_square!(cap) as u32;
                    let captured_unmoved = multi_move_captured_unmoved!(cap);
                    let captured_color = p_color!(
                        $state.pieces[captured_piece_index]
                    );

                    set!(
                        $state.pieces_board[captured_color as usize],
                        captured_square
                    );

                    if captured_unmoved {
                        set!($state.virgin_board, captured_square);
                    }

                    $state.main_board[captured_square as usize] =
                        captured_piece_index as u8;

                    $state.piece_list[captured_piece_index]
                        .push(captured_square as u16);

                    if p_is_big!($state.pieces[captured_piece_index]) {
                        $state.big_pieces[captured_color as usize] += 1;
                    }

                    if p_is_major!($state.pieces[captured_piece_index]) {
                        $state.major_pieces[captured_color as usize] += 1;
                    } else if p_is_minor!(
                        $state.pieces[captured_piece_index]
                    ) {
                        $state.minor_pieces[captured_color as usize] += 1;
                    }

                    $state.material[captured_color as usize] +=
                        p_value!(
                            $state.pieces[captured_piece_index]
                        ) as u32;

                    $state.piece_count[captured_piece_index] += 1;
                }
            }

            #[cfg(debug_assertions)]
            verify_game_state($state);
        }
    };
}

#[inline(always)]
pub fn generate_all_moves_and_drops(state: &State) -> Vec<Move> {

    if state.game_over {
        return Vec::new();
    }

    let piece_count = state.pieces.len() / 2;
    let start_index = piece_count * state.playing as usize;
    let end_index = start_index + piece_count;

    let mut moves = Vec::with_capacity(256);

    if !state.setup_phase {
        for piece_index in start_index..end_index {
            let piece = &state.pieces[piece_index];
            for &index in &state.piece_list[piece_index] {
                moves.extend(generate_move_list(index, piece, state));
            }
        }
    }

    if drops!(state) || state.setup_phase {
        for piece_index in start_index..end_index {
            let piece = &state.pieces[piece_index];
            let enemy_equiv = state.piece_swap_map[&(piece_index as u8)] as usize;

            let drop_from_own =
                state.piece_in_hand[state.playing as usize][piece_index] > 0;

            if drop_from_own {
                moves.extend(generate_drop_list(piece, state));
            } else {
                let drop_from_enemy =
                    state.piece_drops[piece_index]
                        .iter().any(|drop| drop_e!(drop)) &&
                    state.piece_in_hand
                    [1 - state.playing as usize][enemy_equiv] > 0;

                if drop_from_enemy {
                    moves.extend(generate_drop_list(piece, state));
                }
            }
        }
    }

    moves
}