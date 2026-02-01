use crate::game::representations::{
    board::Board,
    moves::{Move, MoveType, MultiMove},
    state::State,
    vector::MultiLegVector
};

fn check_out_of_bounds(
    square: (i8, i8),
    game_state: &State
) -> bool {
    square.0 < 0
        || square.0 >= game_state.files as i8
        || square.1 < 0
        || square.1 >= game_state.ranks as i8
}

/// Generates a board that are relevant to the given move vectors:
pub fn generate_relevant_boards(
    vector_set: &Vec<MultiLegVector>,
    square: (u8, u8),
    game_state: &State
) -> Board {
    assert!(!check_out_of_bounds((
        square.0 as i8,
        square.1 as i8,
    ), game_state), "Square {:?} is out of bounds", square);

    let mut result = Board::new(
        game_state.files, game_state.ranks
    );

    for multi_leg_vector in vector_set.clone() {
        let mut accumulated_offset: (i8, i8) = (square.0 as i8, square.1 as i8);

        for leg in multi_leg_vector {
            accumulated_offset.0 = accumulated_offset.0.saturating_add(
                leg.get_atomic().whole().0
            );

            accumulated_offset.1 = accumulated_offset.1.saturating_add(
                leg.get_atomic().whole().1
            );

            if check_out_of_bounds(accumulated_offset, game_state) {
                break;
            }

            result.set_bit(
                accumulated_offset.0 as u8,
                accumulated_offset.1 as u8
            );
        }
    }

    result
}

pub fn generate_move_list(
    vector_set: &Vec<MultiLegVector>,
    square: (u8, u8),
    relevant_friendly_board: &Board,
    relevant_enemy_board: &Board,
    game_state: &State
) -> Vec<MoveType> {

    assert!(!check_out_of_bounds((
        square.0 as i8,
        square.1 as i8
    ), game_state), "Square {:?} is out of bounds", square);

    let mut move_list: Vec<MoveType> = Vec::new();

    for multi_leg_vector in vector_set.clone() {
        let mut accumulated_offset: (i8, i8) = (square.0 as i8, square.1 as i8);
        let mut captured_pieces: Vec<(u8, u16, bool, bool, bool, Option<u16>)> =
            Vec::new();
        let mut early_fail = false;
        let mut en_passant_square: Option<u16> = None;
        let mut en_passant_count = 0;
        let mut move_is_initial = false;
        let mut can_capture_royal = true;                                       /* Track royal capture capability     */

        let total_legs = multi_leg_vector.len();

        for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
            let is_final_leg = leg_index == total_legs - 1;

            let has_move = leg.is_m();
            let has_capture = leg.is_c();
            let has_destroy = leg.is_d();
            let has_unload = leg.is_u();
            let has_initial = leg.is_i();
            let has_passant = leg.is_p();
            let has_check = leg.is_k();

            let can_move = match has_move {
                Some(true) => true,
                Some(false) => false,
                None => match (has_capture, has_destroy, is_final_leg) {
                    (Some(true), _, _) => false,
                    (_, Some(true), _) => false,
                    _ => true
                }
            };

            let can_capture = match has_capture {
                Some(true) => true,
                Some(false) => false,
                None => is_final_leg,                                           /* Implicit !c for non-final, c final */
            };

            let can_destroy = match has_destroy {
                Some(true) => true,
                Some(false) => false,
                None => false,                                                  /* Implicit !d for all legs           */
            };

            let can_unload = match has_unload {
                Some(true) => true,
                Some(false) => false,
                None => false,                                                  /* Implicit !u for all legs           */
            };

            let creates_en_passant = match has_passant {
                Some(true) => true,
                Some(false) => false,                                           /* !p means can capture en-passant    */
                None => false,                                                  /* Implicit !p (no creation)          */
            };

            can_capture_royal = match has_check {                               /* Update royal capture flag          */
                Some(true) => true,
                Some(false) => false,                                           /* !k means cannot capture royal      */
                None => true,                                                   /* Default allows royal capture       */
            };
                                                                                /* Mark move as initial if i present  */
            if has_initial == Some(true) {
                move_is_initial = true;
            }

            let start_square_index = game_state.square_to_index(
                accumulated_offset.0 as u8,
                accumulated_offset.1 as u8
            );

            accumulated_offset.0 = accumulated_offset.0.saturating_add(
                leg.get_atomic().whole().0
            );

            accumulated_offset.1 = accumulated_offset.1.saturating_add(
                leg.get_atomic().whole().1
            );

            let file = accumulated_offset.0 as u8;
            let rank = accumulated_offset.1 as u8;

            if check_out_of_bounds(accumulated_offset, game_state) {
                early_fail = true;
                break;
            }

            let square_index = game_state.square_to_index(file, rank);
            let has_friendly = relevant_friendly_board.get_bit(file, rank);
            let has_enemy = relevant_enemy_board.get_bit(file, rank);
            let is_empty = !has_friendly && !has_enemy;
                                                                                /* Handle unload modifier (u)         */
            if can_unload {
                if captured_pieces.is_empty() {
                    early_fail = true;
                    break;
                }
                let last_idx = captured_pieces.len() - 1;                       /* Update last captured piece unload  */

                let (
                    piece_type, cap_square, is_ep,
                    can_cap_royal, _, _
                ) = captured_pieces[last_idx];

                captured_pieces[last_idx] = (
                    piece_type, cap_square, is_ep, can_cap_royal,
                    true, Some(start_square_index)
                );
                continue;                                                       /* Unload doesnt add to captures      */
            }

            if creates_en_passant {                                             /* Handle en-passant creation (p)     */
                en_passant_count += 1;
                if en_passant_count > 1 {
                    panic!("More than one en-passant square in move vector!");
                }
                en_passant_square = Some(start_square_index);
            }

            let needs_capture = can_capture && !can_move;
            let needs_move = can_move && !can_capture;

            if can_destroy {
                if can_capture && !can_move {                                   /* cd: must capture (friend or foe)   */
                    if is_empty {
                        early_fail = true;
                        break;
                    }
                    captured_pieces.push(
                        (
                            3, square_index,
                            false, has_passant == Some(false),
                            can_capture_royal, None
                        )
                    );
                } else if !can_capture && !can_move {
                    if has_enemy || is_empty {
                        early_fail = true;
                        break;
                    }
                    captured_pieces.push(
                        (
                            2, square_index,
                            false, has_passant == Some(false),
                            can_capture_royal, None
                        )
                    );
                } else {                                                        /* mcd or d: move/capture/destroy     */
                    if has_friendly && has_enemy {
                        captured_pieces.push(
                            (
                                3, square_index,
                                false, has_passant == Some(false),
                                can_capture_royal, None
                            )
                        );
                    } else if has_friendly {
                        captured_pieces.push(
                            (
                                2, square_index,
                                false, has_passant == Some(false),
                                can_capture_royal, None
                            )
                        );
                    } else if has_enemy {
                        captured_pieces.push(
                            (
                                1, square_index,
                                false, has_passant == Some(false),
                                can_capture_royal, None
                            )
                        );
                    }
                }
            } else {
                if needs_capture {
                    if !has_enemy {
                        early_fail = true;
                        break;
                    }
                    captured_pieces.push(
                        (
                            1, square_index,
                            false, has_passant == Some(false),
                            can_capture_royal, None
                        )
                    );
                } else if needs_move {
                    if !is_empty {
                        early_fail = true;
                        break;
                    }
                } else {
                    if has_friendly{
                        early_fail = true;
                        break;
                    }

                    if has_enemy {
                        captured_pieces.push(
                            (
                                1, square_index,
                                false, has_passant == Some(false),
                                can_capture_royal, None
                            )
                        );
                    }
                }
            }
        }

        if early_fail {
            continue;
        }

        let file = accumulated_offset.0 as u8;
        let rank = accumulated_offset.1 as u8;

        let start_square = game_state.square_to_index(square.0, square.1);
        let end_square = game_state.square_to_index(file, rank);

        let is_initial = move_is_initial;
        let is_promotion = false;
        let promotion_piece = None;

        let encoded_move = match captured_pieces.len() {
            0 => {
                Move::encode(
                    start_square,
                    end_square,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    en_passant_square,
                    MoveType::SingleNoCapture(Move::new_single_no_capture(
                        start_square, end_square, is_initial, is_promotion,
                        promotion_piece, en_passant_square, false
                    )),
                    Some(false),                                                /* is_castling                        */
                    None,                                                       /* captured_piece                     */
                    None,                                                       /* is_en_passant                      */
                    None,                                                       /* can_capture_royal                  */
                    None,                                                       /* captured_square                    */
                    None,                                                       /* captured_pieces                    */
                    None,                                                       /* is_capture_or_unload               */
                    None,                                                       /* unload_square                      */
                )
            }
            1 => {
                let (
                    piece, square, is_en_passant_capture, _,
                    is_capture_or_unload, unload_square
                ) = captured_pieces[0];

                if square != end_square {
                    Move::encode(
                        start_square,
                        end_square,
                        is_initial,
                        is_promotion,
                        promotion_piece,
                        en_passant_square,
                        MoveType::HopperCapture(Move::new_hopper_capture(
                            start_square, end_square, is_initial, is_promotion,
                            promotion_piece, en_passant_square, piece, square,
                            is_en_passant_capture, can_capture_royal,
                            is_capture_or_unload, unload_square
                        )),
                        None,                                                   /* is_castling                        */
                        Some(piece),
                        Some(is_en_passant_capture),
                        Some(can_capture_royal),
                        Some(square),
                        None,                                                   /* captured_pieces                    */
                        Some(is_capture_or_unload),
                        unload_square,
                    )
                } else {
                    Move::encode(
                        start_square,
                        end_square,
                        is_initial,
                        is_promotion,
                        promotion_piece,
                        en_passant_square,
                        MoveType::SingleCapture(Move::new_single_capture(
                            start_square, end_square, is_initial, is_promotion,
                            promotion_piece, en_passant_square, piece,
                            is_en_passant_capture, can_capture_royal,
                            is_capture_or_unload, unload_square
                        )),
                        None,                                                   /* is_castling                        */
                        Some(piece),
                        Some(is_en_passant_capture),
                        Some(can_capture_royal),
                        None,                                                   /* captured_square                    */
                        None,                                                   /* captured_pieces                    */
                        Some(is_capture_or_unload),
                        unload_square,
                    )
                }
            }
            _ => {
                Move::encode(
                    start_square,
                    end_square,
                    is_initial,
                    is_promotion,
                    promotion_piece,
                    en_passant_square,
                    MoveType::MultiCapture(MultiMove::new_multi_capture(
                        start_square, end_square, is_initial,
                        is_promotion, promotion_piece,
                        en_passant_square, captured_pieces.clone()
                    )),
                    None,                                                       /* is_castling                        */
                    None,                                                       /* captured_piece                     */
                    None,                                                       /* is_en_passant                      */
                    None,                                                       /* can_capture_royal                  */
                    None,                                                       /* captured_square                    */
                    Some(captured_pieces),
                    None,                                                       /* is_capture_or_unload               */
                    None,                                                       /* unload_square                      */
                )
            }
        };

        move_list.push(encoded_move);
    }

    move_list
}