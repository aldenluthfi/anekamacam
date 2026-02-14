use std::collections::HashMap;

#[cfg(debug_assertions)]
use crate::game::util::verify_game_state;
use crate::{
    c, captured_piece, captured_square, captured_unmoved, castling, clear,
    constants::{
        BK_CASTLE, BQ_CASTLE, MULTI_CAPTURE_MOVE, NO_EN_PASSANT, NO_PIECE,
        QUIET_MOVE, SINGLE_CAPTURE_MOVE, WK_CASTLE, WQ_CASTLE,
    },
    created_enp, creates_enp, d, enc_capture_part, enc_created_enp,
    enc_creates_enp, enc_end, enc_is_initial, enc_move_type,
    enc_multi_move_captured_piece, enc_multi_move_captured_square,
    enc_multi_move_captured_unmoved, enc_multi_move_is_unload,
    enc_multi_move_unload_square, enc_piece, enc_promoted, enc_promotion,
    enc_start, end, enp_captured, enp_piece, enp_square, g,
    game::{
        hash::{
            CASTLING_HASHES, EN_PASSANT_HASHES, PIECE_HASHES, SIDE_HASHES,
        },
        representations::{
            moves::Move,
            piece::Piece,
            state::{EnPassantSquare, Snapshot, Square, State},
            vector::{MoveSet, MoveVector},
        },
    },
    get, hash_in_or_out_piece, hash_toggle_side, hash_update_castling,
    hash_update_en_passant, i, is_initial, is_unload, k, l, m, move_type,
    multi_move_captured_square, multi_move_is_unload, multi_move_unload_square,
    not_g, not_i, not_k, not_v, p, p_can_promote, p_castle_left,
    p_castle_right, p_color, p_index, p_is_big, p_is_major, p_is_minor,
    p_is_royal, p_value, piece, promoted, promotion, promotions, r, set,
    start, t, u, unload_square, v, x, y,
};

fn is_square_attacked(
    square: u32,
    attacked_side: u8,
    attacked_unmoved: bool,
    attacked_royal: bool,
    attacked_value: u16,
    game_state: &State
) -> bool {
    let possible_attacks = &game_state.relevant_attacks
        [attacked_side as usize][square as usize];

    for (piece_index, start, move_vector) in possible_attacks {
        if game_state.main_board[*start as usize] != *piece_index {
            continue;
        }

        let piece = &game_state.pieces[*piece_index as usize];

        if validate_attack_vector(
            move_vector, *start, piece,
            attacked_unmoved, attacked_royal, attacked_value,
            square, game_state,
        ).is_some()
        {
            return true;
        }
    }

    false
}

fn is_in_check(
    side: u8,
    game_state: &State
) -> bool {
    let monarch_indices = &game_state.royal_list[side as usize];

    for square in monarch_indices {
        if is_square_attacked(
            *square as u32,
            side,
            get!(game_state.virgin_board, *square as u32),
            true,
            u16::MAX,
            game_state
        ) {
            return true;
        }
    }

    false
}

pub fn generate_relevant_moves(
    piece: &Piece,
    square_index: u32,
    game_state: &State
) -> MoveSet {
    let vector_set = &game_state.piece_moves[p_index!(piece) as usize];
    let side = p_color!(piece);

    let mut result = MoveSet::new();
    'multi_leg: for multi_leg_vector in vector_set {
        let mut accumulated_index = square_index as i32;

        let mut file = accumulated_index % (game_state.files as i32);
        let mut rank = accumulated_index / (game_state.files as i32);
        let mut index_to_set = Vec::new();

        for leg in multi_leg_vector {
            let file_offset = x!(leg);
            let rank_offset = y!(leg);

            file += file_offset as i32 * (-2 * side as i32 + 1);
            rank += rank_offset as i32 * (-2 * side as i32 + 1);

            if  file < 0 || file >= game_state.files as i32 ||
                rank < 0 || rank >= game_state.ranks as i32
            {
                continue 'multi_leg;
            }

            accumulated_index = rank * (game_state.files as i32) + file;
            index_to_set.push(accumulated_index as u32);
        }

        result.push(multi_leg_vector.clone());
    }

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
            let mut accumulated_index = square_index as i32;

            let mut file = accumulated_index % (game_state.files as i32);
            let mut rank = accumulated_index / (game_state.files as i32);

            let leg_count = multi_leg_vector.len();

            for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
                let last_leg = leg_index + 1 == leg_count;

                let file_offset = x!(leg);
                let rank_offset = y!(leg);

                file += file_offset as i32 * (-2 * piece_color as i32 + 1);
                rank += rank_offset as i32 * (-2 * piece_color as i32 + 1);

                accumulated_index = rank * (game_state.files as i32) + file;

                let m = m!(leg);
                let c = c!(leg) || (last_leg && !m);
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
    attacked_value: u16,
    attacked_square: u32,
    game_state: &State,
) -> Option<Move> {
    let encoded_move = Move::default();

    let piece_color = p_color!(attacking_piece);
    let piece_value = p_value!(attacking_piece);
    let piece_unmoved = get!(game_state.virgin_board, square_index as u32);

    let mut accumulated_index = square_index as i32;

    let mut file = accumulated_index % (game_state.files as i32);
    let mut rank = accumulated_index / (game_state.files as i32);

    let leg_count = multi_leg_vector.len();

    for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
        let last_leg = leg_index + 1 == leg_count;

        let start_square = accumulated_index as u32;

        let file_offset = x!(leg);
        let rank_offset = y!(leg);

        file += file_offset as i32 * (-2 * piece_color as i32 + 1);
        rank += rank_offset as i32 * (-2 * piece_color as i32 + 1);

        accumulated_index = rank * (game_state.files as i32) + file;
        let end_square = accumulated_index as u32;

        let m = m!(leg) || (!c!(leg) && !d!(leg));
        let c = c!(leg) || (last_leg && !m!(leg));
        let d = d!(leg);
        let k = k!(leg);
        let g = g!(leg);
        let v = v!(leg);
        let t = t!(leg);
        let i = i!(leg);
        let not_k = not_k!(leg);
        let not_g = not_g!(leg);
        let not_v = not_v!(leg);
        let not_i = not_i!(leg);

        if ((i && !piece_unmoved) || (not_i && piece_unmoved)) &&
            !(i && not_i)
        {
            return None;
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

        if end_square == attacked_square &&
            (
                k && !attacked_royal
                || not_k && attacked_royal
                || g && piece_value <= attacked_value
                || not_g && piece_value > attacked_value
                || v && !attacked_unmoved
                || not_v && attacked_unmoved
            )
        {
            return None;
        } else if end_square == attacked_square {
            continue;
        } else if empty {
            if enp_square!(game_state.en_passant_square) == end_square
            && t
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
                    let capt_value =
                        p_value!(capt_piece);
                    let capt_royal =
                        p_is_royal!(capt_piece);

                    if k && !capt_royal
                    || not_k && capt_royal
                    || g && piece_value <= capt_value
                    || not_g && piece_value > capt_value
                    || v && !capt_unmoved
                    || not_v && capt_unmoved
                    {
                        return None;
                    }
                } else {
                    return None;
                }
            } else if !m {
                return None;
            }
        } else if friendly {
            if !d {
                return None;
            }

            let capt_piece_index =
                game_state.main_board[end_square as usize];
            let capt_piece =
                &game_state.pieces[capt_piece_index as usize];
            let capt_unmoved =
                get!(game_state.virgin_board, end_square);
            let capt_value =
                p_value!(capt_piece);
            let capt_royal =
                p_is_royal!(capt_piece);

            if k && !capt_royal
            || not_k && capt_royal
            || g && piece_value <= capt_value
            || not_g && piece_value > capt_value
            || v && !capt_unmoved
            || not_v && capt_unmoved
            {
                return None;
            }
        } else if enemy {
            if !c {
                return None;
            }

            let capt_piece_index =
                game_state.main_board[end_square as usize];
            let capt_piece =
                &game_state.pieces[capt_piece_index as usize];
            let capt_unmoved =
                get!(game_state.virgin_board, end_square);
            let capt_value =
                p_value!(capt_piece);
            let capt_royal =
                p_is_royal!(capt_piece);

            if k && !capt_royal
            || not_k && capt_royal
            || g && piece_value <= capt_value
            || not_g && piece_value > capt_value
            || v && !capt_unmoved
            || not_v && capt_unmoved
            {
                return None;
            }
        }
    }
    Some(encoded_move)
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
    let piece_value = p_value!(piece);
    let piece_unmoved = get!(game_state.virgin_board, square_index as u32);

    let vector_set =
        &game_state.relevant_moves
        [piece_index as usize][square_index as usize];

    'multi_leg: for multi_leg_vector in vector_set {

        let mut encoded_move = Move::default();
        enc_start!(encoded_move, square_index as u128);
        enc_piece!(encoded_move, piece_index as u128);

        let mut taken_pieces: Vec<u64> = Vec::new();

        let mut accumulated_index = square_index as i32;

        let mut file = accumulated_index % (game_state.files as i32);
        let mut rank = accumulated_index / (game_state.files as i32);

        let leg_count = multi_leg_vector.len();

        for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
            let last_leg = leg_index + 1 == leg_count;
            let mut taken_piece = 0u64;

            let start_square = accumulated_index as u32;

            let file_offset = x!(leg);
            let rank_offset = y!(leg);

            file += file_offset as i32 * (-2 * piece_color as i32 + 1);
            rank += rank_offset as i32 * (-2 * piece_color as i32 + 1);

            accumulated_index = rank * (game_state.files as i32) + file;

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
            let l = l!(leg);
            let r = r!(leg);
            let not_k = not_k!(leg);
            let not_g = not_g!(leg);
            let not_v = not_v!(leg);
            let not_i = not_i!(leg);
            let special_i = i && not_i;

            if ((i && !piece_unmoved) || (not_i && piece_unmoved)) &&
                !special_i
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

            if empty {
                if enp_square!(game_state.en_passant_square) == end_square
                && t
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
                        let capt_value =
                            p_value!(capt_piece);
                        let capt_royal =
                            p_is_royal!(capt_piece);

                        if k && !capt_royal
                        || not_k && capt_royal
                        || g && piece_value <= capt_value
                        || not_g && piece_value > capt_value
                        || v && !capt_unmoved
                        || not_v && capt_unmoved
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
            } else if friendly {
                if !d {
                    continue 'multi_leg;
                }

                let capt_piece_index =
                    game_state.main_board[end_square as usize];
                let capt_piece =
                    &game_state.pieces[capt_piece_index as usize];
                let capt_unmoved =
                    get!(game_state.virgin_board, end_square);
                let capt_value =
                    p_value!(capt_piece);
                let capt_royal =
                    p_is_royal!(capt_piece);

                if k && !capt_royal
                || not_k && capt_royal
                || g && piece_value <= capt_value
                || not_g && piece_value > capt_value
                || v && !capt_unmoved
                || not_v && capt_unmoved
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
            } else if enemy {
                if !c {
                    continue 'multi_leg;
                }

                let capt_piece_index =
                    game_state.main_board[end_square as usize];
                let capt_piece =
                    &game_state.pieces[capt_piece_index as usize];
                let capt_unmoved =
                    get!(game_state.virgin_board, end_square);
                let capt_value =
                    p_value!(capt_piece);
                let capt_royal =
                    p_is_royal!(capt_piece);

                if k && !capt_royal
                    || not_k && capt_royal
                    || g && piece_value <= capt_value
                    || not_g && piece_value > capt_value
                    || v && !capt_unmoved
                    || not_v && capt_unmoved
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
                let last_idx = taken_pieces.len() - 1;
                let mut last_captured = taken_pieces[last_idx];

                enc_multi_move_is_unload!(last_captured, 1);
                enc_multi_move_unload_square!(
                    last_captured,
                    start_square as u64
                );

                taken_pieces[last_idx] = last_captured;
            }

            enc_creates_enp!(encoded_move, p as u128);
            enc_created_enp!(encoded_move, p as u128 *
                ((start_square as u128 & 0xFFF)
                | (accumulated_index as u128) << 12
                | (piece_index as u128) << 24)
            );

            if special_i &&
                is_square_attacked(
                    accumulated_index as u32,
                    piece_color,
                    piece_unmoved,
                    p_is_royal!(piece),
                    p_value!(piece),
                    game_state
                )
            {
                continue 'multi_leg;
            }

            if castling!(game_state)
            && (l || r)
            && game_state.castling_state &
            [WK_CASTLE, WQ_CASTLE, BQ_CASTLE, BK_CASTLE]
            [(piece_color * 2 + l as u8) as usize] == 0
            {
                continue 'multi_leg;
            }
        }

        if taken_pieces.len() > 1 {
            taken_pieces.retain(|encoded_capt| {
                let is_unload = multi_move_is_unload!(*encoded_capt);
                let unload_square =
                    multi_move_unload_square!(*encoded_capt);
                let captured_square =
                    multi_move_captured_square!(*encoded_capt);

                !is_unload || captured_square != unload_square
            });
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
                for promo_piece_index in piece.get_promotion_pieces() {
                    let mut promo_move = encoded_move.clone();
                    enc_promotion!(promo_move, 1);
                    enc_promoted!(promo_move, promo_piece_index as u128);
                    result.push(promo_move);
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

pub fn make_move(game_state: &mut State, mv: Move) -> bool {

    game_state.ply += 1;
    game_state.ply_counter += 1;

    let last_en_passant_square = game_state.en_passant_square;
    let last_halfmove_clock = game_state.halfmove_clock;
    let last_castling_state = game_state.castling_state;
    let last_position_hash = game_state.position_hash;

    #[cfg(debug_assertions)]
    verify_game_state(game_state);

    let move_type = move_type!(mv);

    if move_type == QUIET_MOVE {
        let piece_index = piece!(mv) as usize;
        let start_square = start!(mv) as u32;
        let end_square = end!(mv) as u32;
        let is_promotion = promotion!(mv);
        let creates_enp = creates_enp!(mv);
        let promoted_piece = promoted!(mv) as usize;
        let enp_square = created_enp!(mv) as u32;

        let piece_color = p_color!(game_state.pieces[piece_index]);
        let piece_unmoved = get!(game_state.virgin_board, start_square);

        clear!(game_state.pieces_board[piece_color as usize], start_square);
        set!(game_state.pieces_board[piece_color as usize], end_square);

        if p_is_royal!(game_state.pieces[piece_index]) {
            game_state.royal_list[piece_color as usize].retain(
                |&sq| sq as u32 != start_square
            );
            game_state.royal_list[piece_color as usize]
                .push(end_square as Square);
        }

        clear!(game_state.virgin_board, start_square);

        if creates_enp {
            game_state.en_passant_square = enp_square as EnPassantSquare;
        } else {
            game_state.en_passant_square = NO_EN_PASSANT;
        }

        hash_update_en_passant!(
            game_state,
            last_en_passant_square,
            game_state.en_passant_square
        );

        hash_in_or_out_piece!(game_state, piece_index, start_square as u16);
        hash_in_or_out_piece!(
            game_state,
            if is_promotion { promoted_piece } else { piece_index },
            end_square as u16
        );

        game_state.main_board[start_square as usize] = NO_PIECE;
        game_state.main_board[end_square as usize] =
            if is_promotion { promoted_piece as u8 } else { piece_index as u8 };

        if is_promotion {
            let old_piece = &game_state.pieces[piece_index];
            let new_piece = &game_state.pieces[promoted_piece];

            if p_is_big!(old_piece) {
            game_state.big_pieces[piece_color as usize] -= 1;
            }
            if p_is_major!(old_piece) {
            game_state.major_pieces[piece_color as usize] -= 1;
            } else if p_is_minor!(old_piece) {
            game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if p_is_big!(new_piece) {
            game_state.big_pieces[piece_color as usize] += 1;
            }
            if p_is_major!(new_piece) {
            game_state.major_pieces[piece_color as usize] += 1;
            } else if p_is_minor!(new_piece) {
            game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
            p_value!(new_piece) as u32 -  p_value!(old_piece) as u32;

            game_state.piece_count[promoted_piece] += 1;
            game_state.piece_count[piece_index] -= 1;
        }

        game_state.piece_list[piece_index]
            .retain(|&sq| sq != start_square as u16);
        game_state.piece_list[
            if is_promotion { promoted_piece } else { piece_index }
        ].push(end_square as u16);

        if p_can_promote!(game_state.pieces[piece_index]) {
            game_state.halfmove_clock = 0;
        } else {
            game_state.halfmove_clock += 1;
        }

        if piece_unmoved {
            match (
                p_castle_right!(game_state.pieces[piece_index]),
                p_castle_left!(game_state.pieces[piece_index])
            ) {
                (true, true) => {
                    game_state.castling_state &= !(
                        [WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE]
                        [piece_color as usize]
                    );
                },
                (true, false) => {
                    let rights = [!WK_CASTLE, !BK_CASTLE][piece_color as usize];
                    game_state.castling_state &= rights;
                },
                (false, true) => {
                    let rights = [!WQ_CASTLE, !BQ_CASTLE][piece_color as usize];
                    game_state.castling_state &= rights;
                },
                (false, false) => {
                    let start_rank =
                        (start_square as u16) / game_state.files as u16;
                    let start_file =
                        (start_square as u16) % game_state.files as u16;

                    let is_queenside = start_file == 0;
                    let rights = [WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE]
                        [piece_color as usize * 2 + is_queenside as usize];

                    if  start_rank == 0 ||
                        start_rank == game_state.ranks as u16 - 1
                    {
                        game_state.castling_state &= !rights;
                    }
                }
            }
        }

        hash_update_castling!(
            game_state, last_castling_state, game_state.castling_state
        );

    } else if move_type == SINGLE_CAPTURE_MOVE {
        let piece_index = piece!(mv) as usize;
        let start_square = start!(mv) as u32;
        let end_square = end!(mv) as u32;
        let is_promotion = promotion!(mv);
        let creates_enp = creates_enp!(mv);
        let promoted_piece = promoted!(mv) as usize;
        let enp_square = created_enp!(mv) as u32;
        let captured_piece_index = captured_piece!(mv) as usize;
        let captured_square = captured_square!(mv) as u32;
        let is_unload = is_unload!(mv);
        let unload_square = unload_square!(mv) as u32;

        let piece_color = p_color!(game_state.pieces[piece_index]);
        let piece_unmoved = get!(game_state.virgin_board, start_square);
        let captured_color = p_color!(game_state.pieces[captured_piece_index]);

        clear!(game_state.pieces_board[piece_color as usize], start_square);
        set!(game_state.pieces_board[piece_color as usize], end_square);

        if p_is_royal!(game_state.pieces[piece_index]) {
            game_state.royal_list[piece_color as usize].retain(
                |&sq| sq as u32 != start_square
            );
            game_state.royal_list[piece_color as usize]
                .push(end_square as Square);
        }

        clear!(game_state.virgin_board, start_square);

        if creates_enp {
            game_state.en_passant_square = enp_square as EnPassantSquare;
        } else {
            game_state.en_passant_square = NO_EN_PASSANT;
        }

        hash_update_en_passant!(
            game_state,
            last_en_passant_square,
            game_state.en_passant_square
        );

        hash_in_or_out_piece!(game_state, piece_index, start_square as u16);
        hash_in_or_out_piece!(
            game_state,
            if is_promotion { promoted_piece } else { piece_index },
            end_square as u16
        );

        game_state.main_board[start_square as usize] = NO_PIECE;
        game_state.main_board[end_square as usize] =
            if is_promotion { promoted_piece as u8 } else { piece_index as u8 };

        if is_promotion {
            let old_piece = &game_state.pieces[piece_index];
            let new_piece = &game_state.pieces[promoted_piece];

            if p_is_big!(old_piece) {
            game_state.big_pieces[piece_color as usize] -= 1;
            }
            if p_is_major!(old_piece) {
            game_state.major_pieces[piece_color as usize] -= 1;
            } else if p_is_minor!(old_piece) {
            game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if p_is_big!(new_piece) {
            game_state.big_pieces[piece_color as usize] += 1;
            }
            if p_is_major!(new_piece) {
            game_state.major_pieces[piece_color as usize] += 1;
            } else if p_is_minor!(new_piece) {
            game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
             p_value!(new_piece) as u32 -  p_value!(old_piece) as u32;

            game_state.piece_count[promoted_piece] += 1;
            game_state.piece_count[piece_index] -= 1;
        }

        game_state.piece_list[piece_index]
            .retain(|&sq| sq != start_square as u16);
        game_state.piece_list[
            if is_promotion { promoted_piece } else { piece_index }
        ].push(end_square as u16);

        game_state.halfmove_clock = 0;

        if piece_unmoved {
            match (
                p_castle_right!(game_state.pieces[piece_index]),
                p_castle_left!(game_state.pieces[piece_index])
            ) {
                (true, true) => {
                    game_state.castling_state &= !(
                        [WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE]
                        [piece_color as usize]
                    );
                },
                (true, false) => {
                    let rights = [!WK_CASTLE, !BK_CASTLE][piece_color as usize];
                    game_state.castling_state &= rights;
                },
                (false, true) => {
                    let rights = [!WQ_CASTLE, !BQ_CASTLE][piece_color as usize];
                    game_state.castling_state &= rights;
                },
                (false, false) => {
                    let start_rank =
                        (start_square as u16) / game_state.files as u16;
                    let start_file =
                        (start_square as u16) % game_state.files as u16;

                    let is_queenside = start_file == 0;
                    let rights = [WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE]
                        [piece_color as usize * 2 + is_queenside as usize];

                    if  start_rank == 0 ||
                        start_rank == game_state.ranks as u16 - 1
                    {
                        game_state.castling_state &= !rights;
                    }
                }
            }
        }

        let end_rank = (captured_square as u16) / game_state.files as u16;
        let end_file = (captured_square as u16) % game_state.files as u16;

        if get!(game_state.virgin_board, captured_square)
            && (end_rank == 0 || end_rank == game_state.ranks as u16 - 1)
            && (end_file == 0 || end_file == game_state.files as u16 - 1)
        {
            let is_queenside = end_file == 0;
            let rights = [WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE]
            [
                (end_rank == game_state.ranks as u16 - 1) as usize * 2
                + is_queenside as usize
            ];
            game_state.castling_state &= !rights;
        }

        hash_update_castling!(
            game_state, last_castling_state, game_state.castling_state
        );

        clear!(
            game_state.pieces_board[captured_color as usize], captured_square
        );

        hash_in_or_out_piece!(
            game_state,
            captured_piece_index,
            captured_square as u16
        );

        clear!(game_state.virgin_board, captured_square);

        if is_unload {
            set!(
                game_state.pieces_board[captured_color as usize], unload_square
            );

            hash_in_or_out_piece!(
                game_state,
                captured_piece_index,
                unload_square as u16
            );

            set!(game_state.virgin_board, unload_square);
        }

        if captured_square != end_square {
            game_state.main_board[captured_square as usize] = NO_PIECE;
        }

        if is_unload {
            game_state.main_board[unload_square as usize] =
                captured_piece_index as u8;
            game_state.piece_list[captured_piece_index].retain(
                |&sq| sq != captured_square as u16
            );
            game_state.piece_list[captured_piece_index]
                .push(unload_square as u16);
        } else {
            game_state.piece_list[captured_piece_index].retain(
                |&sq| sq != captured_square as u16
            );
        }

        if !is_unload {
            if p_is_big!(game_state.pieces[captured_piece_index]) {
                game_state.big_pieces[captured_color as usize] -= 1;
            }
            if p_is_major!(game_state.pieces[captured_piece_index]) {
                game_state.major_pieces[captured_color as usize] -= 1;
            } else if p_is_minor!(game_state.pieces[captured_piece_index]) {
                game_state.minor_pieces[captured_color as usize] -= 1;
            }

            game_state.material[captured_color as usize] -=
                 p_value!(game_state.pieces[captured_piece_index]) as u32;

            game_state.piece_count[captured_piece_index] -= 1;
        }

    } else if move_type == MULTI_CAPTURE_MOVE {
        unimplemented!()
    }

    game_state.playing = 1 - game_state.playing;
    hash_toggle_side!(game_state);

    let snapshot: Snapshot = Snapshot {
        move_ply: mv,
        castling_state: last_castling_state,
        halfmove_clock: last_halfmove_clock,
        en_passant_square: last_en_passant_square,
        position_hash: last_position_hash
    };

    game_state.history.push(snapshot);

    if is_in_check(1 - game_state.playing, game_state) {
        undo_move(game_state);
        return false;
    }

    #[cfg(debug_assertions)]
    verify_game_state(game_state);

    true
}

pub fn undo_move(game_state: &mut State) {

    game_state.ply -= 1;
    game_state.ply_counter -= 1;

    let snapshot = game_state.history.pop().unwrap_or_else(
        || panic!("No move to undo!")
    );

    #[cfg(debug_assertions)]
    verify_game_state(game_state);

    game_state.playing = 1 - game_state.playing;
    game_state.castling_state = snapshot.castling_state;
    game_state.halfmove_clock = snapshot.halfmove_clock;
    game_state.en_passant_square = snapshot.en_passant_square;
    game_state.position_hash = snapshot.position_hash;

    let mv = snapshot.move_ply;
    let move_type = move_type!(mv);

    if move_type == QUIET_MOVE {
        let piece_index = piece!(mv) as usize;
        let start_square = start!(mv) as u32;
        let end_square = end!(mv) as u32;
        let piece_unmoved = is_initial!(mv) == 1;
        let is_promotion = promotion!(mv);
        let promoted_piece = promoted!(mv) as usize;

        let piece_color = p_color!(game_state.pieces[piece_index]);

        clear!(game_state.pieces_board[piece_color as usize], end_square);
        set!(game_state.pieces_board[piece_color as usize], start_square);

        if p_is_royal!(game_state.pieces[piece_index]) {
            game_state.royal_list[piece_color as usize].retain(
                |&sq| sq as u32 != end_square
            );
            game_state.royal_list[piece_color as usize]
                .push(start_square as Square);
        }

        if piece_unmoved {
            set!(game_state.virgin_board, start_square);
        }

        game_state.main_board[end_square as usize] = NO_PIECE;
        game_state.main_board[start_square as usize] = piece_index as u8;

        if is_promotion {
            game_state.piece_list[promoted_piece]
                .retain(|&sq| sq != end_square as u16);

            if p_is_big!(game_state.pieces[promoted_piece]) {
                game_state.big_pieces[piece_color as usize] -= 1;
            }
            if p_is_major!(game_state.pieces[promoted_piece]) {
                game_state.major_pieces[piece_color as usize] -= 1;
            } else if p_is_minor!(game_state.pieces[promoted_piece]) {
                game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if p_is_big!(game_state.pieces[piece_index]) {
                game_state.big_pieces[piece_color as usize] += 1;
            }
            if p_is_major!(game_state.pieces[piece_index]) {
                game_state.major_pieces[piece_color as usize] += 1;
            } else if p_is_minor!(game_state.pieces[piece_index]) {
                game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
                 p_value!(game_state.pieces[piece_index]) as u32;
            game_state.material[piece_color as usize] -=
                 p_value!(game_state.pieces[promoted_piece]) as u32;

            game_state.piece_count[promoted_piece] -= 1;
            game_state.piece_count[piece_index] += 1;
        } else {
            game_state.piece_list[piece_index]
                .retain(|&sq| sq != end_square as u16);
        }

        game_state.piece_list[piece_index]
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

        let piece_color = p_color!(game_state.pieces[piece_index]);
        let captured_color = p_color!(game_state.pieces[captured_piece_index]);

        clear!(game_state.pieces_board[piece_color as usize], end_square);
        set!(game_state.pieces_board[piece_color as usize], start_square);

        if p_is_royal!(game_state.pieces[piece_index]) {
            game_state.royal_list[piece_color as usize].retain(
                |&sq| sq as u32 != end_square
            );
            game_state.royal_list[piece_color as usize]
                .push(start_square as Square);
        }

        if piece_unmoved {
            set!(game_state.virgin_board, start_square);
        }

        game_state.main_board[end_square as usize] = NO_PIECE;
        game_state.main_board[start_square as usize] = piece_index as u8;

        if is_promotion {
            game_state.piece_list[promoted_piece]
                .retain(|&sq| sq != end_square as u16);

            if p_is_big!(game_state.pieces[promoted_piece]) {
                game_state.big_pieces[piece_color as usize] -= 1;
            }
            if p_is_major!(game_state.pieces[promoted_piece]) {
                game_state.major_pieces[piece_color as usize] -= 1;
            } else if p_is_minor!(game_state.pieces[promoted_piece]) {
                game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if p_is_big!(game_state.pieces[piece_index]) {
                game_state.big_pieces[piece_color as usize] += 1;
            }
            if p_is_major!(game_state.pieces[piece_index]) {
                game_state.major_pieces[piece_color as usize] += 1;
            } else if p_is_minor!(game_state.pieces[piece_index]) {
                game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
                 p_value!(game_state.pieces[piece_index]) as u32;
            game_state.material[piece_color as usize] -=
                 p_value!(game_state.pieces[promoted_piece]) as u32;

            game_state.piece_count[promoted_piece] -= 1;
            game_state.piece_count[piece_index] += 1;
        } else {
            game_state.piece_list[piece_index]
                .retain(|&sq| sq != end_square as u16);
        }

        game_state.piece_list[piece_index]
            .push(start_square as u16);

        if is_unload {
            clear!(
                game_state.pieces_board[captured_color as usize], unload_square
            );
            set!(
                game_state.pieces_board[captured_color as usize], captured_square
            );
        } else {
            set!(
                game_state.pieces_board[captured_color as usize], captured_square
            );
        }

        if captured_unmoved {
            set!(game_state.virgin_board, captured_square);

            if is_unload {
                clear!(game_state.virgin_board, unload_square);
            }
        }

        if is_unload {
            game_state.main_board[unload_square as usize] = NO_PIECE;
        }
        game_state.main_board[captured_square as usize] =
            captured_piece_index as u8;

        if is_unload {
            game_state.piece_list[captured_piece_index].retain(
                |&sq| sq != unload_square as u16
            );
        }
        game_state.piece_list[captured_piece_index]
            .push(captured_square as u16);

        if !is_unload {
            if p_is_big!(game_state.pieces[captured_piece_index]) {
                game_state.big_pieces[captured_color as usize] += 1;
            }

            if p_is_major!(game_state.pieces[captured_piece_index]) {
                game_state.major_pieces[captured_color as usize] += 1;
            } else if p_is_minor!(game_state.pieces[captured_piece_index]) {
                game_state.minor_pieces[captured_color as usize] += 1;
            }

            game_state.material[captured_color as usize] +=
                 p_value!(game_state.pieces[captured_piece_index]) as u32;

            game_state.piece_count[captured_piece_index] += 1;
        }
    } else if move_type == MULTI_CAPTURE_MOVE {
        unimplemented!()
    }

    #[cfg(debug_assertions)]
    verify_game_state(game_state);
}

pub fn generate_all_moves(state: &State) -> Vec<Move> {
    let piece_count = state.pieces.len() / 2;
    let start_index = piece_count * state.playing as usize;
    let end_index = start_index + piece_count;

    let moves: Vec<Move> = (start_index..end_index)
        .flat_map(|piece_index| {
            let piece = &state.pieces[piece_index];
            state.piece_list[piece_index]
                .iter()
                .flat_map(|index| {
                    generate_move_list(
                        *index,
                        piece,
                        state,
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect();

    moves
}