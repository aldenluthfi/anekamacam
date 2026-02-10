use bnum::types::U2048;

#[cfg(debug_assertions)]
use crate::game::util::verify_game_state;
use crate::{
    board, c, captured_piece, captured_square, captured_unmoved,
    castling, clear,
    constants::{
        BK_CASTLE, BQ_CASTLE, MULTI_CAPTURE_MOVE, NO_EN_PASSANT,
        NO_PIECE, QUIET_MOVE, SINGLE_CAPTURE_MOVE, WHITE, WK_CASTLE,
        WQ_CASTLE,
    },
    created_enp, creates_enp, d,
    enc_can_check, enc_can_enp, enc_captured_piece,
    enc_captured_square, enc_captured_unmoved, enc_created_enp,
    enc_creates_enp, enc_end, enc_is_castling, enc_is_unload,
    enc_move_type, enc_multi_move_can_check, enc_multi_move_can_enp,
    enc_multi_move_captured_piece, enc_multi_move_captured_square,
    enc_multi_move_captured_unmoved, enc_multi_move_is_unload,
    enc_multi_move_unload_square, enc_must_initial, enc_must_not_initial,
    enc_piece, enc_promoted, enc_promoting, enc_promotion, enc_start,
    enc_unload_square, end, enp_captured, enp_piece, enp_square,
    game::{
        hash::{CASTLING_HASHES, EN_PASSANT_HASHES, PIECE_HASHES,
               SIDE_HASHES},
        representations::{
            board::Board, moves::Move, piece::Piece,
            state::{EnPassantSquare, Snapshot, Square, State},
            vector::MoveSet,
        },
    },
    get, i, is_castling, is_unload, k, m, move_type,
    multi_move_can_check, multi_move_can_enp,
    multi_move_captured_piece, multi_move_captured_square,
    multi_move_captured_unmoved, multi_move_is_unload,
    multi_move_unload_square, must_initial, not_c, not_i, not_k, not_m,
    not_p, p, piece, promoted, promotion, queenside, set, start, u,
    unload_square, x, y, hash_in_or_out_piece, hash_toggle_side,
    hash_update_castling, hash_update_en_passant,
};

#[hotpath::measure]
fn is_square_attacked(
    square_index: u32,
    side: u8,
    game_state: &State
) -> bool {
    let side_piece_count = game_state.pieces.len() / 2;
    let index = side_piece_count * side as usize;

    for i in index..index + side_piece_count {
        let piece = &game_state.pieces[i];
        let relevant_boards = &game_state.relevant_board[i];

        for &attacker_index in &game_state.piece_list[i] {
            if !get!(relevant_boards[attacker_index as usize], square_index) {
                continue;
            }

            let move_list = generate_move_list(
                attacker_index, piece, game_state, true
            );

            for moves in move_list {
                if move_type!(moves) == SINGLE_CAPTURE_MOVE
                    && captured_square!(moves) == square_index as u128
                    && !is_unload!(moves)
                {
                    return true;
                }

                if move_type!(moves) == MULTI_CAPTURE_MOVE
                    && moves.1.iter().any(|&mc| {
                        multi_move_captured_square!(mc) == square_index as u64
                        && !multi_move_is_unload!(mc)
                    })
                {
                    return true;
                }
            }
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
            1 - side,
            game_state
        ) {
            return true;
        }
    }

    false
}

#[hotpath::measure]
pub fn generate_relevant_boards(
    piece: &Piece,
    square_index: u32,
    game_state: &State
) -> Board {
    let vector_set = &game_state.piece_moves[piece.index() as usize];
    let side = piece.color();

    let mut result = board!(game_state.files, game_state.ranks);
    'multi_leg: for multi_leg_vector in vector_set {
        let mut accumulated_index = square_index as i32;

        let mut file = accumulated_index % (game_state.files as i32);
        let mut rank = accumulated_index / (game_state.files as i32);
        let mut index_to_set = Vec::new();

        for leg in multi_leg_vector {
            let file_offset = x!(leg);
            let rank_offset = y!(leg);

            file += file_offset as i32 * (-2 * side as i32 + 1);
            if !castling!(leg) {
                rank += rank_offset as i32 * (-2 * side as i32 + 1);
            }

            if  file < 0 || file >= game_state.files as i32 ||
                rank < 0 || rank >= game_state.ranks as i32
            {
                continue 'multi_leg;
            }

            accumulated_index = rank * (game_state.files as i32) + file;
            index_to_set.push(accumulated_index as u32);
        }

        for index in index_to_set {
            set!(result, index);
        }
    }

    result
}

#[hotpath::measure]
pub fn generate_relevant_moves(
    piece: &Piece,
    square_index: u32,
    game_state: &State
) -> MoveSet {
    let vector_set = &game_state.piece_moves[piece.index() as usize];
    let side = piece.color();

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
            if !castling!(leg) {
                rank += rank_offset as i32 * (-2 * side as i32 + 1);
            }

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

pub fn generate_move_list(
    square_index: u16,
    piece: &Piece,
    game_state: &State,
    imaginary: bool
) -> Vec<Move> {

    let piece_index = piece.index() as usize;
    let side = piece.color() as usize;
    let piece_unmoved = get!(game_state.virgin_board, square_index as u32);
    let friendly_board = &game_state.pieces_board[side];
    let enemy_board = &game_state.pieces_board[1 - side];
    let virgin_board = &game_state.virgin_board;
    let promotion_rank = game_state.promotion_ranks[side];

    let vector_set =
        &game_state.relevant_moves[piece_index][square_index as usize];

    let mut result: Vec<Move> = Vec::new();
    'multi_leg: for multi_leg_vector in vector_set {
        let mut encoded_move = Move::default();
        enc_start!(encoded_move, square_index as u128);
        enc_piece!(encoded_move, piece_index as u128);

        let mut accumulated_index = square_index as i32;

        let mut file = accumulated_index % (game_state.files as i32);
        let mut rank = accumulated_index / (game_state.files as i32);

        let leg_count = multi_leg_vector.len();

        if castling!(multi_leg_vector[0]) {
            let leg = &multi_leg_vector[0];

            let offset = x!(leg);
            let castling_queenside = queenside!(leg);

            let needed_rights = [WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE]
                [side * 2 + castling_queenside as usize];

            let start_file = file;
            file += offset as i32;

            let end_index =
                (rank * (game_state.files as i32) + file) as u16;

            let rook_file =
                (game_state.files as i32 - 1) * !castling_queenside as i32;
            let rook_square =
                (rank * (game_state.files as i32) + rook_file) as u16;

            let start_check = start_file.min(rook_file) + 1;
            let end_check = start_file.max(rook_file);

            let path_clear = !(start_check..end_check).any(|f| {
                let check_sq =
                    (rank as u32) * (game_state.files as u32) + f as u32;

                get!(friendly_board, check_sq) || get!(enemy_board, check_sq)
            });

            if game_state.castling_state & needed_rights == 0 ||
               !get!(virgin_board, rook_square as u32) ||
               !path_clear || !piece_unmoved
            {
                continue 'multi_leg;
            }

            let unload_file = file + !castling_queenside as i32 * (-2) + 1;
            let unload_square =
                (rank * (game_state.files as i32) + unload_file) as u16;

            enc_end!(encoded_move, end_index as u128);
            enc_move_type!(encoded_move, SINGLE_CAPTURE_MOVE);
            enc_is_unload!(encoded_move, 1);
            enc_unload_square!(encoded_move, unload_square as u128);
            enc_captured_piece!(
                encoded_move,
                game_state.main_board[rook_square as usize] as u128
            );
            enc_captured_square!(encoded_move, rook_square as u128);
            enc_captured_unmoved!(encoded_move, 1);
            enc_must_initial!(encoded_move, 1);
            enc_is_castling!(encoded_move, 1);

            result.push(encoded_move);
            continue 'multi_leg;
        }

        let mut taken_pieces: Vec<u64> = Vec::new();
        for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
            let last_leg = leg_index + 1 == leg_count;
            let mut taken_piece = 0u64;

            let m = m!(leg);
            let c = c!(leg);
            let d = d!(leg);
            let i = i!(leg);
            let p = p!(leg);
            let k = k!(leg);
            let u = u!(leg);
            let not_m = not_m!(leg);
            let not_c = not_c!(leg);
            let not_i = not_i!(leg);
            let not_p = not_p!(leg);
            let not_k = not_k!(leg);
            let unset_m = !m && !not_m;
            let unset_c = !c && !not_c;
            let unset_p = !p && !not_p;
            let unset_k = !k && !not_k;

            let can_move = m || (unset_m && !d && !c);
            let can_capture = c || (unset_c && last_leg && !m);
            let can_destroy = d;
            let can_unload = u;
            let creates_enp = p;
            let can_enp = !not_p || unset_p && can_capture;
            let can_check = !not_k || unset_k && can_capture;

            enc_multi_move_can_check!(
                taken_piece,
                can_check as u64
            );

            enc_multi_move_can_enp!(
                taken_piece,
                can_enp as u64
            );

            if (i && !piece_unmoved) || (not_i && piece_unmoved) {
                continue 'multi_leg;
            }

            enc_must_initial!(
                encoded_move, (i | piece_unmoved) as u128
            );

            enc_must_not_initial!(
                encoded_move, not_i as u128
            );

            let start_index = (rank * (game_state.files as i32) + file) as u16;

            let file_offset = x!(leg);
            let rank_offset = y!(leg);

            file += file_offset as i32 * (-2 * side as i32 + 1);
            rank += rank_offset as i32 * (-2 * side as i32 + 1);

            accumulated_index = rank * (game_state.files as i32) + file;

            let has_friendly =
                get!(friendly_board, accumulated_index as u32);
            let has_enemy =
                get!(enemy_board, accumulated_index as u32);
            let is_empty =
                !has_friendly && !has_enemy;

            let must_capture = can_capture && !can_move && !can_destroy;
            let must_move = !can_capture && can_move && !can_destroy;
            let must_destroy = !can_capture && !can_move && can_destroy;
            let can_move_capture = can_capture && can_move && !can_destroy;
            let can_move_destroy = !can_capture && can_move && can_destroy;
            let can_capture_destroy = can_capture && !can_move && can_destroy;
            let can_do_all_three = can_capture && can_move && can_destroy;

            if must_destroy {
                if !has_friendly {
                    if game_state.en_passant_square != NO_EN_PASSANT {
                        let game_en_passant = game_state.en_passant_square;
                        let enp_piece_idx =
                            enp_piece!(game_en_passant) as usize;
                        if can_enp
                            && accumulated_index ==
                                enp_square!(game_en_passant) as i32
                            && is_empty
                            && game_state.pieces[enp_piece_idx].color() ==
                                side as u8
                        {
                            let enp_sq = enp_captured!(game_en_passant) as u16;
                            let enp_unmoved = get!(virgin_board, enp_sq as u32);

                            enc_multi_move_captured_piece!(
                                taken_piece,
                                game_state.main_board[enp_sq as usize] as u64
                            );

                            enc_multi_move_captured_square!(
                                taken_piece,
                                enp_sq as u64
                            );

                            enc_multi_move_captured_unmoved!(
                                taken_piece,
                                enp_unmoved as u64
                            );

                            taken_pieces.push(taken_piece);
                            continue;
                        }
                    }

                    if !imaginary {
                        continue 'multi_leg;
                    }
                }
                enc_multi_move_captured_piece!(
                    taken_piece,
                    game_state.main_board[accumulated_index as usize] as u64
                );

                enc_multi_move_captured_square!(
                    taken_piece,
                    accumulated_index as u64
                );

                enc_multi_move_captured_unmoved!(
                    taken_piece,
                    get!(virgin_board, accumulated_index as u32) as u64
                );

                taken_pieces.push(taken_piece);
            } else if must_capture {
                if !has_enemy {
                    if game_state.en_passant_square != NO_EN_PASSANT {
                        let game_en_passant = game_state.en_passant_square;
                        let enp_piece_idx =
                            enp_piece!(game_en_passant) as usize;

                        if can_enp
                            && accumulated_index ==
                                enp_square!(game_en_passant) as i32
                            && is_empty
                            && game_state.pieces[enp_piece_idx].color() !=
                                side as u8
                        {
                            let enp_sq = enp_captured!(game_en_passant) as u16;
                            let enp_unmoved = get!(virgin_board, enp_sq as u32);

                            enc_multi_move_captured_piece!(
                                taken_piece,
                                game_state.main_board[enp_sq as usize] as u64
                            );

                            enc_multi_move_captured_square!(
                                taken_piece,
                                enp_sq as u64
                            );

                            enc_multi_move_captured_unmoved!(
                                taken_piece,
                                enp_unmoved as u64
                            );

                            taken_pieces.push(taken_piece);
                            continue;
                        }
                    }

                    if !imaginary {
                        continue 'multi_leg;
                    }
                }
                enc_multi_move_captured_piece!(
                    taken_piece,
                    game_state.main_board[accumulated_index as usize] as u64
                );

                enc_multi_move_captured_square!(
                    taken_piece,
                    accumulated_index as u64
                );

                enc_multi_move_captured_unmoved!(
                    taken_piece,
                    get!(virgin_board, accumulated_index as u32) as u64
                );

                taken_pieces.push(taken_piece);
            } else if must_move {
                if !is_empty {
                    continue 'multi_leg;
                }
            } else if can_capture_destroy {
                if is_empty {
                    if game_state.en_passant_square != NO_EN_PASSANT {
                        let game_en_passant = game_state.en_passant_square;
                        if can_enp
                            && accumulated_index ==
                                enp_square!(game_en_passant) as i32
                            && is_empty
                        {
                            let enp_sq = enp_captured!(game_en_passant) as u16;
                            let enp_unmoved = get!(virgin_board, enp_sq as u32);

                            enc_multi_move_captured_piece!(
                                taken_piece,
                                game_state.main_board[enp_sq as usize] as u64
                            );

                            enc_multi_move_captured_square!(
                                taken_piece,
                                enp_sq as u64
                            );

                            enc_multi_move_captured_unmoved!(
                                taken_piece,
                                enp_unmoved as u64
                            );

                            taken_pieces.push(taken_piece);
                            continue;
                        }
                    }

                    if !imaginary {
                        continue 'multi_leg;
                    }
                }
                enc_multi_move_captured_piece!(
                    taken_piece,
                    game_state.main_board[accumulated_index as usize] as u64
                );

                enc_multi_move_captured_square!(
                    taken_piece,
                    accumulated_index as u64
                );

                enc_multi_move_captured_unmoved!(
                    taken_piece,
                    get!(virgin_board, accumulated_index as u32) as u64
                );

                taken_pieces.push(taken_piece);
            } else if can_move_destroy {
                if has_enemy && !imaginary {
                    continue 'multi_leg;
                }
                if has_friendly || imaginary {
                    enc_multi_move_captured_piece!(
                        taken_piece,
                        game_state.main_board[accumulated_index as usize] as u64
                    );

                    enc_multi_move_captured_square!(
                        taken_piece,
                        accumulated_index as u64
                    );

                    enc_multi_move_captured_unmoved!(
                        taken_piece,
                        get!(virgin_board, accumulated_index as u32) as u64
                    );

                    taken_pieces.push(taken_piece);
                } else if game_state.en_passant_square != NO_EN_PASSANT {
                    let game_en_passant = game_state.en_passant_square;
                    let enp_piece_idx =
                        enp_piece!(game_en_passant) as usize;
                    if can_enp
                        && accumulated_index ==
                            enp_square!(game_en_passant) as i32
                        && is_empty
                        && game_state.pieces[enp_piece_idx].color() ==
                            side as u8
                    {
                        let enp_sq = enp_captured!(game_en_passant) as u16;
                        let enp_unmoved = get!(virgin_board, enp_sq as u32);

                        enc_multi_move_captured_piece!(
                            taken_piece,
                            game_state.main_board[enp_sq as usize] as u64
                        );

                        enc_multi_move_captured_square!(
                            taken_piece,
                            enp_sq as u64
                        );

                        enc_multi_move_captured_unmoved!(
                            taken_piece,
                            enp_unmoved as u64
                        );

                        taken_pieces.push(taken_piece);
                        continue;
                    }
                }
            } else if can_move_capture {
                if has_friendly && !imaginary {
                    continue 'multi_leg;
                }
                if has_enemy || imaginary {
                    enc_multi_move_captured_piece!(
                        taken_piece,
                        game_state.main_board[accumulated_index as usize] as u64
                    );

                    enc_multi_move_captured_square!(
                        taken_piece,
                        accumulated_index as u64
                    );

                    enc_multi_move_captured_unmoved!(
                        taken_piece,
                        get!(virgin_board, accumulated_index as u32) as u64
                    );

                    taken_pieces.push(taken_piece);
                } else if game_state.en_passant_square != NO_EN_PASSANT {
                    let game_en_passant = game_state.en_passant_square;
                    let enp_piece_idx =
                        enp_piece!(game_en_passant) as usize;
                    if can_enp
                        && accumulated_index ==
                            enp_square!(game_en_passant) as i32
                        && is_empty
                        && game_state.pieces[enp_piece_idx].color() !=
                            side as u8
                    {
                        let enp_sq = enp_captured!(game_en_passant) as u16;
                        let enp_unmoved = get!(virgin_board, enp_sq as u32);

                        enc_multi_move_captured_piece!(
                            taken_piece,
                            game_state.main_board[enp_sq as usize] as u64
                        );

                        enc_multi_move_captured_square!(
                            taken_piece,
                            enp_sq as u64
                        );

                        enc_multi_move_captured_unmoved!(
                            taken_piece,
                            enp_unmoved as u64
                        );

                        taken_pieces.push(taken_piece);
                        continue;
                    }
                }
            } else if can_do_all_three {
                if imaginary || has_enemy || has_friendly {
                    enc_multi_move_captured_piece!(
                        taken_piece,
                        game_state.main_board[accumulated_index as usize] as u64
                    );

                    enc_multi_move_captured_square!(
                        taken_piece,
                        accumulated_index as u64
                    );

                    enc_multi_move_captured_unmoved!(
                        taken_piece,
                        get!(virgin_board, accumulated_index as u32) as u64
                    );

                    taken_pieces.push(taken_piece);
                } else if game_state.en_passant_square != NO_EN_PASSANT {
                    let game_en_passant = game_state.en_passant_square;
                    if can_enp
                        && accumulated_index ==
                            enp_square!(game_en_passant) as i32
                        && is_empty
                    {
                        let enp_sq = enp_captured!(game_en_passant) as u16;
                        let enp_unmoved = get!(virgin_board, enp_sq as u32);

                        enc_multi_move_captured_piece!(
                            taken_piece,
                            game_state.main_board[enp_sq as usize] as u64
                        );

                        enc_multi_move_captured_square!(
                            taken_piece,
                            enp_sq as u64
                        );

                        enc_multi_move_captured_unmoved!(
                            taken_piece,
                            enp_unmoved as u64
                        );

                        taken_pieces.push(taken_piece);
                        continue;
                    }
                }
            }

            if can_unload {
                let last_idx = taken_pieces.len() - 1;
                let mut last_captured = taken_pieces[last_idx];

                enc_multi_move_is_unload!(last_captured, 1);
                enc_multi_move_unload_square!(
                    last_captured,
                    start_index as u64
                );

                taken_pieces[last_idx] = last_captured;
            }

            enc_creates_enp!(encoded_move, creates_enp as u128);
            enc_created_enp!(encoded_move, creates_enp as u128 *
                ((start_index as u128 & 0xFFF)
                | (accumulated_index as u128) << 12
                | (piece_index as u128) << 24)
            );
        }

        let reaches_promotion_rank =
            if side as u8 == WHITE {
                rank as u32 >= promotion_rank as u32
            } else {
                rank as u32 <= promotion_rank as u32
            };
        let can_promote = piece.can_promote() && reaches_promotion_rank;
        let promotion_pieces = if can_promote {
            piece.get_promotion_pieces()
        } else {
            Vec::new()
        };

        let taken_pieces_len = taken_pieces.len();

        if promotion_pieces.is_empty() {
            enc_end!(encoded_move, accumulated_index as u128);
            if taken_pieces_len == 0 {
                enc_move_type!(encoded_move, QUIET_MOVE);
                result.push(encoded_move);
            } else if taken_pieces_len == 1 {
                enc_move_type!(encoded_move, SINGLE_CAPTURE_MOVE);

                enc_can_check!(
                    encoded_move,
                    multi_move_can_check!(taken_pieces[0]) as u128
                );

                enc_can_enp!(
                    encoded_move,
                    multi_move_can_enp!(taken_pieces[0]) as u128
                );

                enc_is_unload!(
                    encoded_move,
                    multi_move_is_unload!(taken_pieces[0]) as u128
                );

                enc_unload_square!(
                    encoded_move,
                    multi_move_unload_square!(taken_pieces[0]) as u128
                );

                enc_captured_piece!(
                    encoded_move,
                    multi_move_captured_piece!(taken_pieces[0]) as u128
                );

                enc_captured_square!(
                    encoded_move,
                    multi_move_captured_square!(taken_pieces[0]) as u128
                );

                enc_captured_unmoved!(
                    encoded_move,
                    multi_move_captured_unmoved!(taken_pieces[0]) as u128
                );

                result.push(encoded_move);
            } else {
                enc_move_type!(encoded_move, MULTI_CAPTURE_MOVE);
                result.push((encoded_move.0, taken_pieces));
            }
        } else {
            for &promo_piece in &promotion_pieces {
                let mut promo_move = encoded_move.clone();
                enc_end!(promo_move, accumulated_index as u128);
                enc_promotion!(promo_move, 1);
                enc_promoted!(promo_move, promo_piece as u128);
                enc_promoting!(promo_move, 1);

                if taken_pieces_len == 0 {
                    enc_move_type!(promo_move, QUIET_MOVE);
                    result.push(promo_move);
                } else if taken_pieces_len == 1 {
                    enc_move_type!(promo_move, SINGLE_CAPTURE_MOVE);

                    enc_can_check!(
                        promo_move,
                        multi_move_can_check!(taken_pieces[0]) as u128
                    );

                    enc_can_enp!(
                        promo_move,
                        multi_move_can_enp!(taken_pieces[0]) as u128
                    );

                    enc_is_unload!(
                        promo_move,
                        multi_move_is_unload!(taken_pieces[0]) as u128
                    );

                    enc_unload_square!(
                        promo_move,
                        multi_move_unload_square!(taken_pieces[0]) as u128
                    );

                    enc_captured_piece!(
                        promo_move,
                        multi_move_captured_piece!(taken_pieces[0]) as u128
                    );

                    enc_captured_square!(
                        promo_move,
                        multi_move_captured_square!(taken_pieces[0]) as u128
                    );

                    enc_captured_unmoved!(
                        promo_move,
                        multi_move_captured_unmoved!(taken_pieces[0]) as u128
                    );

                    result.push(promo_move);
                } else {
                    enc_move_type!(promo_move, MULTI_CAPTURE_MOVE);
                    result.push((promo_move.0, taken_pieces.clone()));
                }
            }
        }
    }

    result
}

#[hotpath::measure]
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

        let piece_color = game_state.pieces[piece_index].color();
        let piece_unmoved = get!(game_state.virgin_board, start_square);

        clear!(game_state.pieces_board[piece_color as usize], start_square);
        set!(game_state.pieces_board[piece_color as usize], end_square);

        if game_state.pieces[piece_index].is_royal() {
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

            if old_piece.is_big() {
            game_state.big_pieces[piece_color as usize] -= 1;
            }
            if old_piece.is_major() {
            game_state.major_pieces[piece_color as usize] -= 1;
            } else if old_piece.is_minor() {
            game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if new_piece.is_big() {
            game_state.big_pieces[piece_color as usize] += 1;
            }
            if new_piece.is_major() {
            game_state.major_pieces[piece_color as usize] += 1;
            } else if new_piece.is_minor() {
            game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
            new_piece.value() as u32 - old_piece.value() as u32;

            game_state.piece_count[promoted_piece] += 1;
            game_state.piece_count[piece_index] -= 1;
        }

        game_state.piece_list[piece_index]
            .retain(|&sq| sq != start_square as u16);
        game_state.piece_list[
            if is_promotion { promoted_piece } else { piece_index }
        ].push(end_square as u16);

        if game_state.pieces[piece_index].can_promote() {
            game_state.halfmove_clock = 0;
        } else {
            game_state.halfmove_clock += 1;
        }

        if piece_unmoved {
            match (
                game_state.pieces[piece_index].can_castle_kingside(),
                game_state.pieces[piece_index].can_castle_queenside()
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
        let is_castling = is_castling!(mv);

        let piece_color = game_state.pieces[piece_index].color();
        let piece_unmoved = get!(game_state.virgin_board, start_square);
        let captured_color =
            game_state.pieces[captured_piece_index].color();

        if is_castling {
            let king_file = start_square % game_state.files as u32;
            let end_file = end_square % game_state.files as u32;

            let start_check = king_file.min(end_file) + 1;
            let end_check = king_file.max(end_file);

            let path_attacked = (start_check..end_check).any(|f| {
                let check_sq =
                    (start_square / game_state.files as u32)
                    * game_state.files as u32 + f;

                is_square_attacked(
                    check_sq,
                    1 - piece_color,
                    game_state,
                )
            });

            let is_in_check = is_square_attacked(
                start_square,
                1 - piece_color,
                game_state,
            );

            if path_attacked || is_in_check {
                return false;
            }
        }

        clear!(game_state.pieces_board[piece_color as usize], start_square);
        set!(game_state.pieces_board[piece_color as usize], end_square);

        if game_state.pieces[piece_index].is_royal() {
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

            if old_piece.is_big() {
            game_state.big_pieces[piece_color as usize] -= 1;
            }
            if old_piece.is_major() {
            game_state.major_pieces[piece_color as usize] -= 1;
            } else if old_piece.is_minor() {
            game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if new_piece.is_big() {
            game_state.big_pieces[piece_color as usize] += 1;
            }
            if new_piece.is_major() {
            game_state.major_pieces[piece_color as usize] += 1;
            } else if new_piece.is_minor() {
            game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
            new_piece.value() as u32 - old_piece.value() as u32;

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
                game_state.pieces[piece_index].can_castle_kingside(),
                game_state.pieces[piece_index].can_castle_queenside()
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
            if game_state.pieces[captured_piece_index].is_big() {
                game_state.big_pieces[captured_color as usize] -= 1;
            }
            if game_state.pieces[captured_piece_index].is_major() {
                game_state.major_pieces[captured_color as usize] -= 1;
            } else if game_state.pieces[captured_piece_index].is_minor() {
                game_state.minor_pieces[captured_color as usize] -= 1;
            }

            game_state.material[captured_color as usize] -=
                game_state.pieces[captured_piece_index].value() as u32;

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
        let piece_unmoved = must_initial!(mv) == 1;
        let is_promotion = promotion!(mv);
        let promoted_piece = promoted!(mv) as usize;

        let piece_color = game_state.pieces[piece_index].color();

        clear!(game_state.pieces_board[piece_color as usize], end_square);
        set!(game_state.pieces_board[piece_color as usize], start_square);

        if game_state.pieces[piece_index].is_royal() {
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

            if game_state.pieces[promoted_piece].is_big() {
                game_state.big_pieces[piece_color as usize] -= 1;
            }
            if game_state.pieces[promoted_piece].is_major() {
                game_state.major_pieces[piece_color as usize] -= 1;
            } else if game_state.pieces[promoted_piece].is_minor() {
                game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if game_state.pieces[piece_index].is_big() {
                game_state.big_pieces[piece_color as usize] += 1;
            }
            if game_state.pieces[piece_index].is_major() {
                game_state.major_pieces[piece_color as usize] += 1;
            } else if game_state.pieces[piece_index].is_minor() {
                game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
                game_state.pieces[piece_index].value() as u32;
            game_state.material[piece_color as usize] -=
                game_state.pieces[promoted_piece].value() as u32;

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
        let piece_unmoved = must_initial!(mv) == 1;
        let is_promotion = promotion!(mv);
        let promoted_piece = promoted!(mv) as usize;
        let captured_piece_index = captured_piece!(mv) as usize;
        let captured_square = captured_square!(mv) as u32;
        let captured_unmoved = captured_unmoved!(mv);
        let is_unload = is_unload!(mv);
        let unload_square = unload_square!(mv) as u32;

        let piece_color = game_state.pieces[piece_index].color();
        let captured_color =
            game_state.pieces[captured_piece_index].color();

        clear!(game_state.pieces_board[piece_color as usize], end_square);
        set!(game_state.pieces_board[piece_color as usize], start_square);

        if game_state.pieces[piece_index].is_royal() {
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

            if game_state.pieces[promoted_piece].is_big() {
                game_state.big_pieces[piece_color as usize] -= 1;
            }
            if game_state.pieces[promoted_piece].is_major() {
                game_state.major_pieces[piece_color as usize] -= 1;
            } else if game_state.pieces[promoted_piece].is_minor() {
                game_state.minor_pieces[piece_color as usize] -= 1;
            }

            if game_state.pieces[piece_index].is_big() {
                game_state.big_pieces[piece_color as usize] += 1;
            }
            if game_state.pieces[piece_index].is_major() {
                game_state.major_pieces[piece_color as usize] += 1;
            } else if game_state.pieces[piece_index].is_minor() {
                game_state.minor_pieces[piece_color as usize] += 1;
            }

            game_state.material[piece_color as usize] +=
                game_state.pieces[piece_index].value() as u32;
            game_state.material[piece_color as usize] -=
                game_state.pieces[promoted_piece].value() as u32;

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
            if game_state.pieces[captured_piece_index].is_big() {
                game_state.big_pieces[captured_color as usize] += 1;
            }

            if game_state.pieces[captured_piece_index].is_major() {
                game_state.major_pieces[captured_color as usize] += 1;
            } else if game_state.pieces[captured_piece_index].is_minor() {
                game_state.minor_pieces[captured_color as usize] += 1;
            }

            game_state.material[captured_color as usize] +=
                game_state.pieces[captured_piece_index].value() as u32;

            game_state.piece_count[captured_piece_index] += 1;
        }
    } else if move_type == MULTI_CAPTURE_MOVE {
        unimplemented!()
    }

    #[cfg(debug_assertions)]
    verify_game_state(game_state);
}

#[hotpath::measure]
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
                        false
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect();

    moves
}