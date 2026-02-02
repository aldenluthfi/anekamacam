#[cfg(debug_assertions)]
use crate::game::util::verify_game_state;
use crate::{
    constants::*,
    game::{
        hash::{
            hash_in_or_out_piece,
            hash_toggle_side,
            hash_update_en_passant
        },
        representations::{
            board::Board,
            moves::{Move, MoveType},
            piece::Piece,
            state::{Snapshot, State}, vector::LegVector,
        }
    },
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

#[hotpath::measure]
pub fn is_square_attacked(
    square: (u8, u8),
    side: u8,
    game_state: &State
) -> bool {
    assert!(!check_out_of_bounds((
        square.0 as i8,
        square.1 as i8,
    ), game_state), "Square {:?} is out of bounds", square);

    assert!(
        side == WHITE || side == BLACK,
        "Side must be WHITE or BLACK"
    );


    let side_piece_count = game_state.pieces.len() / 2;
    let index = if side == WHITE {
        0
    } else {
        side_piece_count
    };
    let sq_index = game_state.square_to_index(square.0, square.1) as usize;
    let (file, rank) = square;

    let mut new_board = Board::new(
        game_state.files,
        game_state.ranks,
    );

    new_board.set_bit(file, rank);

    let mut friendly_board = if side == WHITE {
        &game_state.white_board
    } else {
        &game_state.black_board
    } ^ &new_board;                                                             /* make sure no enemy piece there     */

    if friendly_board.get_bit(file, rank) {
        friendly_board = &friendly_board ^ &new_board;
    }

    let enemy_board = if side == WHITE {
        &game_state.black_board
    } else {
        &game_state.white_board
    } | &new_board;                                                             /* As if theres a friendly piece there*/

    for i in index..index + side_piece_count {
        let piece = &game_state.pieces[i];
        let piece_board = &game_state.pieces_board[i];
        let piece_indices = piece_board.bit_indices();

        for attacker_index in piece_indices {
            let relevant_board =
                &game_state.piece_relevant_boards[i][attacker_index as usize];

            if !relevant_board.get_bit(file, rank) {
                continue;
            }

            let (attacker_file, attacker_rank) =
                game_state.index_to_square(attacker_index as u16);

            let move_list = generate_move_list(
                (attacker_file, attacker_rank),
                piece,
                &friendly_board,
                &enemy_board,
                &game_state.unmoved_board,
                game_state
            );

            for moves in move_list {
                match moves {
                    MoveType::SingleCapture(mv) => {
                        if mv.end_square() == sq_index as u16 {
                            return true;
                        }
                    }

                    MoveType::HopperCapture(mv) => {
                        if let Some(sq) = mv.captured_square() {
                            if sq == sq_index as u16 {
                                return true;
                            }
                        }
                    }

                    MoveType::MultiCapture(mv) => {
                        for (
                            _, cap_square, can_capture_royal, is_unload, _
                        ) in mv.get_taken_pieces() {
                            if  cap_square == sq_index as u16 &&
                                !is_unload &&                                   /* attacks defined as captures only   */
                                can_capture_royal                               /* up to change, only used for checks */
                            {
                                return true;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    false
}

#[hotpath::measure]
pub fn is_in_check(
    side: u8,
    game_state: &State
) -> bool {
    let monarch_board = &game_state.monarch_board & if side == WHITE {
        &game_state.white_board
    } else {
        &game_state.black_board
    };
    let monarch_indices = monarch_board.bit_indices();

    for index in monarch_indices {
        let (file, rank) = game_state.index_to_square(index as u16);

        if is_square_attacked(
            (file, rank),
            1 - side,
            game_state
        ) {
            return true;
        }
    }

    false
}

/// Generates a board that are relevant to the given move vectors
#[hotpath::measure]
pub fn generate_relevant_boards(
    piece: &Piece,
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

    let vector_set = &game_state.piece_move_vectors[piece.index() as usize];
    let side = piece.color();

    for multi_leg_vector in vector_set {
        let mut accumulated_offset: (i8, i8) = (square.0 as i8, square.1 as i8);

        for leg in multi_leg_vector {
            accumulated_offset.0 = accumulated_offset.0.saturating_add(
                leg.get_atomic().whole().0
            );

            accumulated_offset.1 = accumulated_offset.1.saturating_add(
                leg.get_atomic().whole().1.saturating_mul(
                    if side == WHITE {1} else {-1}
                )                                                               /* Adjust for side pov                */
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


/// generates possible move templates
/// placeholder capture pieces:
/// 1 -> enemy piece
/// 2 -> friendly piece
/// 3 -> either piece
/// 4 -> special for castling
#[hotpath::measure]
pub fn generate_move_list(
    square: (u8, u8),
    piece: &Piece,
    friendly_board: &Board,
    enemy_board: &Board,
    unmoved_board: &Board,
    game_state: &State
) -> Vec<MoveType> {

    assert!(!check_out_of_bounds((
        square.0 as i8,
        square.1 as i8
    ), game_state), "Square {:?} is out of bounds", square);

    let mut move_list: Vec<MoveType> = Vec::new();
    let unmoved_piece = unmoved_board.get_bit(square.0, square.1);
    let vector_set = &game_state.piece_move_vectors[piece.index() as usize];
    let side = piece.color();
    let piece_idx = piece.index();

    let promotion_rank = if side == WHITE {                                     /* Determine promotion rank           */
        game_state.ranks - 1
    } else {
        0
    };

    for multi_leg_vector in vector_set {
        let mut accumulated_offset: (i8, i8) = (square.0 as i8, square.1 as i8);
        let mut taken_pieces: Vec<(u8, u16, bool, bool, Option<u16>)> =
            Vec::new();
        let mut early_fail = false;
        let mut castling_leg: Option<LegVector> = None;
        let mut en_passant_square: Option<u32> = None;
        let mut en_passant_count = 0;
        let mut move_is_initial = false;
        let mut can_capture_royal;                                              /* Track royal capture capability     */
        let mut creates_en_passant;

        let total_legs = multi_leg_vector.len();

        for (leg_index, leg) in multi_leg_vector.iter().enumerate() {

            if leg.is_castling() {
                castling_leg = Some(*leg);
                break;
            }

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
                None => match (has_capture, has_destroy) {
                    (Some(true), _) => false,
                    (_, Some(true)) => false,
                    _ => true
                }
            };

            if has_initial == Some(true) {                                      /* Mark move as initial if i present  */
                if !unmoved_piece {
                    early_fail = true;
                    break;
                }

                move_is_initial = true;
            }

            if has_initial == Some(false) {                                     /* Fail if !i and piece is unmoved    */
                if unmoved_piece {
                    early_fail = true;
                    break;
                }
            }

            let can_capture = match has_capture {
                Some(true) => true,
                Some(false) => false,
                None => is_final_leg && has_move != Some(true),                 /* Implicit !c for non-final, c final */
            };                                                                  /* unless m is set explicitly         */

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

            creates_en_passant = match has_passant {
                Some(true) => true,
                Some(false) => false,                                           /* !p means cant capture en-passant   */
                None => false,                                                  /* Implicit !p (no creation)          */
            };

            let can_capture_en_passant = match has_passant {
                Some(true) => true,
                Some(false) => false,                                           /* !p means cant capture en-passant   */
                None => can_capture,                                            /* Default allows en-passant capture  */
            };

            can_capture_royal = match has_check {                               /* Update royal capture flag          */
                Some(true) => true,
                Some(false) => false,                                           /* !k means cannot capture royal      */
                None => can_capture,                                            /* Default allows royal capture       */
            };

            let start_square_index = game_state.square_to_index(
                accumulated_offset.0 as u8,
                accumulated_offset.1 as u8
            );

            accumulated_offset.0 = accumulated_offset.0.saturating_add(
                leg.get_atomic().whole().0
            );

            accumulated_offset.1 = accumulated_offset.1.saturating_add(
                leg.get_atomic().whole().1.saturating_mul(
                    if side == WHITE {1} else {-1}
                )                                                               /* Adjust for side pov                */
            );

            let file = accumulated_offset.0 as u8;
            let rank = accumulated_offset.1 as u8;

            if check_out_of_bounds(accumulated_offset, game_state) {
                early_fail = true;
                break;
            }

            let square_index = game_state.square_to_index(file, rank);
            let has_friendly = friendly_board.get_bit(file, rank);
            let has_enemy = enemy_board.get_bit(file, rank);
            let is_empty = !has_friendly && !has_enemy;

            if  piece.is_royal() &&
                is_square_attacked((file, rank), 1 - side, game_state)          /* Royal pieces cannot move into check*/
            {
                early_fail = true;
                break;
            }

            if creates_en_passant {                                             /* Handle en-passant creation (p)     */
                en_passant_count += 1;
                if en_passant_count > 1 {
                    panic!("More than one en-passant square in move vector!");
                }
                en_passant_square = Some(start_square_index as u32);            /* Store only capture square initially*/
            }

            let must_capture = can_capture && !can_move && !can_destroy;
            let must_move = !can_capture && can_move && !can_destroy;
            let must_destroy = !can_capture && !can_move && can_destroy;
            let can_move_capture = can_capture && can_move && !can_destroy;
            let can_move_destroy = !can_capture && can_move && can_destroy;
            let can_capture_destroy = can_capture && !can_move && can_destroy;
            let can_do_all_three = can_capture && can_move && can_destroy;

            if must_destroy {                                                   /* d: must destroy friendly           */
                if has_enemy || is_empty {
                    if let Some(game_en_passant) =
                        game_state.en_passant_square
                    {
                        let enp_piece_idx =
                            (game_en_passant >> 24) as usize & 0xFF;
                        if can_capture_en_passant
                            && square_index == (game_en_passant & 0xFFF) as u16
                            && is_empty
                            && game_state.pieces[enp_piece_idx].color() == side /* Must be friendly piece              */
                        {
                            taken_pieces.push(
                                (
                                    2, (game_en_passant >> 12) as u16 & 0xFFF,  /* Next 12 bits: captured piece square*/
                                    can_capture_royal, false, None
                                )
                            );
                            continue;
                        }
                    }
                    early_fail = true;
                    break;
                }
                taken_pieces.push(
                    (
                        2, square_index, can_capture_royal,
                        false, None
                    )
                );
            } else if must_capture {                                            /* c: must capture enemy              */
                if !has_enemy {
                    if let Some(game_en_passant) =
                        game_state.en_passant_square
                    {
                        let enp_piece_idx =
                            (game_en_passant >> 24) as usize & 0xFF;
                        if can_capture_en_passant
                            && square_index == (game_en_passant & 0xFFF) as u16
                            && is_empty
                            && game_state.pieces[enp_piece_idx].color() != side /* Must be friendly piece              */
                        {
                            taken_pieces.push(
                                (
                                    1, (game_en_passant >> 12) as u16 & 0xFFF,
                                    can_capture_royal, false, None
                                )
                            );
                            continue;
                        }
                    }
                    early_fail = true;
                    break;
                }

                taken_pieces.push(
                    (
                        1, square_index, can_capture_royal,
                        false, None
                    )
                );
            } else if must_move {                                               /* m: must move to empty              */
                if !is_empty {
                    early_fail = true;
                    break;
                }
            } else if can_capture_destroy {                                     /* cd: capture or destroy (any piece) */
                if is_empty {
                    if let Some(game_en_passant) =
                        game_state.en_passant_square
                    {
                        if can_capture_en_passant
                            && square_index == (game_en_passant & 0xFFF) as u16
                            && is_empty
                        {                                                       /* can be both color                  */
                            taken_pieces.push(
                                (
                                    3, (game_en_passant >> 12) as u16 & 0xFFF,
                                    can_capture_royal, false, None
                                )
                            );
                            continue;
                        }
                    }
                    early_fail = true;
                    break;
                }
                taken_pieces.push(
                    (
                        3, square_index, can_capture_royal,
                        false, None
                    )
                );
            } else if can_move_destroy {                                        /* md: move or destroy friendly       */
                if has_enemy {
                    early_fail = true;
                    break;
                }
                if has_friendly {
                    taken_pieces.push(
                        (
                            2, square_index, can_capture_royal,
                            false, None
                        )
                    );
                } else if let Some(game_en_passant) =
                    game_state.en_passant_square
                {
                    let enp_piece_idx = (game_en_passant >> 24) as usize & 0xFF;
                    if can_capture_en_passant
                        && square_index == (game_en_passant & 0xFFF) as u16
                        && is_empty
                        && game_state.pieces[enp_piece_idx].color() == side     /* Must be friendly piece             */
                    {
                        taken_pieces.push(
                            (
                                2, (game_en_passant >> 12) as u16 & 0xFFF,
                                can_capture_royal, false, None
                            )
                        );
                    }
                }
            } else if can_move_capture {                                        /* mc: move or capture enemy          */
                if has_friendly {
                    early_fail = true;
                    break;
                }
                if has_enemy {
                    taken_pieces.push(
                        (
                            1, square_index, can_capture_royal,
                            false, None
                        )
                    );
                } else if let Some(game_en_passant) =
                    game_state.en_passant_square
                {
                    let enp_piece_idx = (game_en_passant >> 24) as usize & 0xFF;
                    if can_capture_en_passant
                        && square_index == (game_en_passant & 0xFFF) as u16
                        && is_empty
                        && game_state.pieces[enp_piece_idx].color() != side     /* Must be enemy piece                */
                    {
                        taken_pieces.push(
                            (
                                1, (game_en_passant >> 12) as u16 & 0xFFF,
                                can_capture_royal, false, None
                            )
                        );
                    }
                }
            } else if can_do_all_three {                                        /* mcd: move/capture/destroy (any)    */
                if has_friendly {
                    taken_pieces.push(
                        (
                            2, square_index, can_capture_royal,
                            false, None
                        )
                    );
                } else if has_enemy {
                    taken_pieces.push(
                        (
                            1, square_index, can_capture_royal,
                            false, None
                        )
                    );
                } else if let Some(game_en_passant) =
                    game_state.en_passant_square
                {
                    if can_capture_en_passant
                        && square_index == (game_en_passant & 0xFFF) as u16
                        && is_empty
                    {
                        taken_pieces.push(
                            (
                                3, (game_en_passant >> 12) as u16 & 0xFFF,
                                can_capture_royal, false, None
                            )
                        );
                    }
                }
            }

                                                                                /* Handle unload modifier (u)         */
            if can_unload {
                if taken_pieces.is_empty() {
                    early_fail = true;
                    break;
                }
                let last_idx = taken_pieces.len() - 1;                          /* Update last captured piece unload  */

                let (
                    piece_type, cap_square, _,
                    can_cap_royal, _
                ) = taken_pieces[last_idx];

                taken_pieces[last_idx] = (
                    piece_type, cap_square, can_cap_royal, true,
                    Some(start_square_index)
                );
                continue;                                                       /* Unload doesnt add to captures      */
            }

            if let Some(en_passant_sq) = en_passant_square {
                en_passant_square = Some(
                    (en_passant_sq & 0xFFF)                                     /* Capture square                     */
                    | (square_index as u32) << 12                               /* Captured piece square              */
                    | (piece_idx as u32) << 24
                );
            }
        }

        if early_fail {
            continue;
        }

        if let Some(leg) = castling_leg {
            let king_rank = square.1;
            let king_file = square.0;

            let atomic = leg.get_atomic();
            let (offset, _) = atomic.whole();

            let is_castling_right = leg.is_castling_right();
            let rook_file = if is_castling_right {
                game_state.files - 1
            } else {
                0
            };

            let rook_square = game_state.square_to_index(rook_file, king_rank);

            if !unmoved_board.get_bit(rook_file, king_rank) {
                continue;                                                       /* Rook must be unmoved               */
            }

            let rook_occupied = if side == WHITE {
                friendly_board.get_bit(rook_file, king_rank)
            } else {
                friendly_board.get_bit(rook_file, king_rank)
            };

            if !rook_occupied {
                continue;                                                       /* Rook must exist on corner          */
            }

            let king_end_file = (king_file as i8).saturating_add(offset) as u8;
            if king_end_file >= game_state.files {
                continue;                                                       /* End square out of bounds           */
            }

            let start_check = king_file.min(rook_file);
            let end_check = king_file.max(rook_file);

            let mut path_clear = true;
            for f in start_check + 1..end_check {
                if friendly_board.get_bit(f, king_rank) ||
                   enemy_board.get_bit(f, king_rank) {
                    path_clear = false;
                    break;                                                      /* Path must be clear                 */
                }
            }

            if !path_clear {
                continue;
            }

            let attack_start = king_file.min(king_end_file);
            let attack_end = king_file.max(king_end_file);

            let mut king_path_safe = true;
            for f in attack_start..=attack_end {
                if is_square_attacked(
                    (f, king_rank),
                    1 - side,                                                   /* Check from enemy perspective       */
                    game_state
                ) {
                    king_path_safe = false;
                    break;                                                      /* King path must not be attacked     */
                }
            }

            if !king_path_safe {
                continue;
            }

            let start_square = game_state.square_to_index(
                king_file, king_rank
            );
            let end_square = game_state.square_to_index(
                king_end_file, king_rank
            );

            let unload_file = if is_castling_right {
                king_end_file - 1
            } else {
                king_end_file + 1
            };

            let unload_square = game_state.square_to_index(
                unload_file, king_rank
            );

            let encoded_move = Move::encode(
                piece_idx,
                start_square,
                end_square,
                true,                                                           /* is_initial                         */
                false,                                                          /* is_promotion                       */
                None,                                                           /* promoting_piece                    */
                None,                                                           /* promoted_piece                     */
                None,                                                           /* en_passant_square                  */
                Move::HOPPER_CAPTURE,                                           /* move_type                          */
                Some(4),                                                        /* captured_piece (special)           */
                Some(false),                                                    /* can_capture_royal                  */
                Some(rook_square),                                              /* captured_square                    */
                None,                                                           /* taken_pieces                       */
                Some(true),                                                     /* is_unload                          */
                Some(unload_square),                                            /* unload_square                      */
            );

            move_list.push(encoded_move);
            continue;
        }

        let file = accumulated_offset.0 as u8;
        let rank = accumulated_offset.1 as u8;

        let start_square = game_state.square_to_index(square.0, square.1);
        let end_square = game_state.square_to_index(file, rank);

        let is_initial = move_is_initial;
                                                                                /* Check for promotion                */
        let reaches_promotion_rank = rank == promotion_rank;
        let can_promote = piece.can_promote() && reaches_promotion_rank;
                                                                                /* Get promotion pieces if applicable */
        let promotion_pieces = if can_promote {
            piece.get_promotion_pieces()
        } else {
            Vec::new()
        };

        let is_promotion = can_promote && !promotion_pieces.is_empty();
                                                                                /* Generate move variants             */
        let moves_to_generate = if is_promotion {
            promotion_pieces.len()                                              /* One move per promotion piece       */
        } else {
            1                                                                   /* Single non-promotion move          */
        };

        for move_idx in 0..moves_to_generate {
            let promoting_piece = if is_promotion {
                Some(piece.index())
            } else {
                None
            };

            let promoted_piece = if is_promotion {
                Some(promotion_pieces[move_idx])
            } else {
                None
            };

            let encoded_move = match taken_pieces.len() {
                0 => {
                    Move::encode(
                        piece_idx,
                        start_square,
                        end_square,
                        is_initial,
                        is_promotion,
                        promoting_piece,
                        promoted_piece,
                        en_passant_square,
                        Move::SINGLE_NO_CAPTURE,                                /* move_type                          */
                        None,                                                   /* captured_piece                     */
                        None,                                                   /* can_capture_royal                  */
                        None,                                                   /* captured_square                    */
                        None,                                                   /* taken_pieces                       */
                        None,                                                   /* is_unload                          */
                        None,                                                   /* unload_square                      */
                    )
                }
                1 => {
                    let (
                        captured_piece, cap_square, can_capture_royal,
                        is_unload, unload_sq
                    ) = taken_pieces[0];

                    if cap_square != end_square {
                        Move::encode(
                            piece_idx,
                            start_square,
                            end_square,
                            is_initial,
                            is_promotion,
                            promoting_piece,
                            promoted_piece,
                            en_passant_square,
                            Move::HOPPER_CAPTURE,                               /* move_type                          */
                            Some(captured_piece),
                            Some(can_capture_royal),
                            Some(cap_square),
                            None,                                               /* taken_pieces                       */
                            Some(is_unload),
                            unload_sq,
                        )
                    } else {
                        Move::encode(
                            piece_idx,
                            start_square,
                            end_square,
                            is_initial,
                            is_promotion,
                            promoting_piece,
                            promoted_piece,
                            en_passant_square,
                            Move::SINGLE_CAPTURE,                               /* move_type                          */
                            Some(captured_piece),
                            Some(can_capture_royal),
                            None,                                               /* captured_square                    */
                            None,                                               /* taken_pieces                       */
                            Some(is_unload),
                            unload_sq,
                        )
                    }
                }
                _ => {
                    Move::encode(
                        piece_idx,
                        start_square,
                        end_square,
                        is_initial,
                        is_promotion,
                        promoting_piece,
                        promoted_piece,
                        en_passant_square,
                        Move::MULTI_CAPTURE,                                    /* move_type                          */
                        None,                                                   /* captured_piece                     */
                        None,                                                   /* can_capture_royal                  */
                        None,                                                   /* captured_square                    */
                        Some(taken_pieces.clone()),                             /* taken_pieces for multi-capture     */
                        None,                                                   /* is_unload                          */
                        None,                                                   /* unload_square                      */
                    )
                }
            };

            move_list.push(encoded_move);
        }
    }

    move_list
}


fn find_piece_at_square(
    game_state: &State,
    file: u8,
    rank: u8,
) -> Option<&Piece> {
    let piece_count = game_state.pieces.len() / 2;
    let start_index = if game_state.playing == WHITE {
        0
    } else {
        piece_count
    };

    for i in start_index..start_index + piece_count {
        let piece_board = &game_state.pieces_board[i];

        if piece_board.get_bit(file, rank) {
            return Some(&game_state.pieces[i]);
        }
    }

    None
}

fn remove_piece(piece_index: usize, square_index: u16, game_state: &mut State) {
    let color = game_state.pieces[piece_index].color();
    let (file, rank) = game_state.index_to_square(square_index);

    hash_in_or_out_piece(
        game_state,
        piece_index,
        color,
        square_index
    );

    #[cfg(debug_assertions)]
    {
        assert!(
            game_state.pieces_board[piece_index].get_bit(file, rank),
            "No piece of index {} at square {:?} to remove",
            piece_index,
            (file, rank)
        );
        assert!(
            if color == WHITE {
                game_state.white_board.get_bit(file, rank)
            } else {
                game_state.black_board.get_bit(file, rank)
            },
            "No piece of color {} at square {:?} to remove",
            if color == WHITE {"WHITE"} else {"BLACK"},
            (file, rank)
        );
    }

    game_state.pieces_board[piece_index].clear_bit(file, rank);

    if color == WHITE {
        game_state.white_board.clear_bit(file, rank);
    } else {
        game_state.black_board.clear_bit(file, rank);
    }

    if game_state.pieces[piece_index].is_big() {
        game_state.big_pieces[color as usize] -= 1;
    }

    if game_state.pieces[piece_index].is_major() {
        game_state.major_pieces[color as usize] -= 1;
    } else if game_state.pieces[piece_index].is_minor() {
        game_state.minor_pieces[color as usize] -= 1;
    }

    game_state.material[color as usize] -=
        game_state.pieces[piece_index].value() as u32;
}

fn add_piece(piece_index: usize, square_index: u16, game_state: &mut State) {
    let color = game_state.pieces[piece_index].color();
    let (file, rank) = game_state.index_to_square(square_index);

    hash_in_or_out_piece(
        game_state,
        piece_index,
        color,
        square_index
    );

    #[cfg(debug_assertions)]
    {
        assert!(
            !game_state.pieces_board[piece_index].get_bit(file, rank),
            "Piece of index {} already at square {:?}",
            piece_index,
            (file, rank)
        );
        assert!(
            if color == WHITE {
                !game_state.white_board.get_bit(file, rank)
            } else {
                !game_state.black_board.get_bit(file, rank)
            },
            "Square {:?} already occupied by a piece of color {}",
            (file, rank),
            if color == WHITE {"WHITE"} else {"BLACK"}
        );
    }

    game_state.pieces_board[piece_index].set_bit(file, rank);

    if color == WHITE {
        game_state.white_board.set_bit(file, rank);
    } else {
        game_state.black_board.set_bit(file, rank);
    }

    if game_state.pieces[piece_index].is_big() {
        game_state.big_pieces[color as usize] += 1;
    }

    if game_state.pieces[piece_index].is_major() {
        game_state.major_pieces[color as usize] += 1;
    } else if game_state.pieces[piece_index].is_minor() {
        game_state.minor_pieces[color as usize] += 1;
    }

    game_state.material[color as usize] +=
        game_state.pieces[piece_index].value() as u32;
}

fn move_piece(
    piece_index: usize,
    start_square: u16,
    end_square: u16,
    game_state: &mut State
) {
    let color = game_state.pieces[piece_index].color();
    let (start_file, start_rank) = game_state.index_to_square(start_square);
    let (end_file, end_rank) = game_state.index_to_square(end_square);

    hash_in_or_out_piece(
        game_state,
        piece_index,
        color,
        start_square
    );

    hash_in_or_out_piece(
        game_state,
        piece_index,
        color,
        end_square
    );

    #[cfg(debug_assertions)]
    {
        assert!(
            game_state.pieces_board[piece_index].get_bit(start_file, start_rank),
            "No piece of index {} at start square {:?} to move",
            piece_index,
            (start_file, start_rank)
        );
        assert!(
            !game_state.pieces_board[piece_index].get_bit(end_file, end_rank),
            "Piece of index {} already at end square {:?}",
            piece_index,
            (end_file, end_rank)
        );
        assert!(
            if color == WHITE {
                game_state.white_board.get_bit(start_file, start_rank)
            } else {
                game_state.black_board.get_bit(start_file, start_rank)
            },
            "No piece of color {} at start square {:?} to move",
            if color == WHITE {"WHITE"} else {"BLACK"},
            (start_file, start_rank)
        );
    }

    game_state.pieces_board[piece_index].clear_bit(start_file, start_rank);
    game_state.pieces_board[piece_index].set_bit(end_file, end_rank);

    if color == WHITE {
        game_state.white_board.clear_bit(start_file, start_rank);
        game_state.white_board.set_bit(end_file, end_rank);
    } else {
        game_state.black_board.clear_bit(start_file, start_rank);
        game_state.black_board.set_bit(end_file, end_rank);
    }
}

fn promote_piece(
    promoting_piece: usize,
    start_square: u16,
    end_square: u16,
    promoted_piece: usize,
    game_state: &mut State
) {
    remove_piece(promoting_piece, start_square, game_state);
    add_piece(promoted_piece, end_square, game_state);
}

#[hotpath::measure]
pub fn make_move(game_state: &mut State, mv: MoveType) -> bool{

    game_state.ply += 1;
    game_state.ply_counter += 1;

    let last_en_passant_square = game_state.en_passant_square;
    let last_halfmove_clock = game_state.halfmove_clock;
    let last_castling_state = game_state.castling_state;
    let last_position_hash = game_state.position_hash;

    let snapshot: Snapshot = match mv {
        MoveType::SingleNoCapture(mv) => {
            let piece_index = mv.piece_index() as usize;
            let start_square = mv.start_square();
            let end_square = mv.end_square();
            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();

            move_piece(piece_index, start_square, end_square, game_state);

            let (start_file, start_rank) =
                game_state.index_to_square(start_square);

            game_state.unmoved_board.clear_bit(start_file, start_rank);

            if is_initial {
                game_state.unmoved_board.clear_bit(start_file, start_rank);
            }

            if creates_en_passant {
                game_state.en_passant_square = en_passant_square;
            } else {
                game_state.en_passant_square = None;
            }

            hash_update_en_passant(
                game_state,
                last_en_passant_square,
                en_passant_square
            );

            if is_promotion {
                let promoting_piece = piece_index;
                let promoted_piece = promoted_piece.expect(
                    "Promoted piece must be provided for promotion moves"
                ) as usize;

                promote_piece(
                    promoting_piece,
                    end_square,
                    end_square,
                    promoted_piece,
                    game_state
                );
            }

            if game_state.pieces[piece_index].can_promote() {                   /* reset halfmove clock on pawn move  */
                game_state.halfmove_clock = 0;
            } else {
                game_state.halfmove_clock += 1;
            }

            Snapshot {
                move_ply: MoveType::SingleNoCapture(mv),
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                en_passant_square: last_en_passant_square,
                position_hash: last_position_hash,
            }
        }
        MoveType::SingleCapture(mut mv) => {                                    /* where capture square = end square  */
            let piece_index = mv.piece_index() as usize;
            let start_square = mv.start_square();
            let end_square = mv.end_square();
            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();
            let captured_piece_template = mv.captured_piece();
            let is_unload = mv.is_unload();
            let unload_square = mv.unload_square();

            let (end_file, end_rank) =
                game_state.index_to_square(end_square);

            let real_captured = find_piece_at_square(
                game_state,
                end_file,
                end_rank
            )
            .expect("No piece found at captured square");
            let real_captured_index = real_captured.index() as usize;

            #[cfg(debug_assertions)]
            {
                assert!(match captured_piece_template {
                    Some(1) => game_state.playing != real_captured.color(),
                    Some(2) => game_state.playing == real_captured.color(),
                    Some(3) => true,
                    Some(4) => game_state.pieces[piece_index].is_royal(),
                    _ => panic!(
                        "Invalid captured piece template: {:?}",
                        captured_piece_template
                    ),
                }, "Captured/moving piece type does not match the template");
                assert!(
                    if is_unload {
                        unload_square.is_some()
                    } else {
                        true
                    },
                    "Unload square must be provided for unload captures"
                );
                assert!(
                    !game_state.pieces[real_captured_index].is_royal(),
                    "Cannot capture royal pieces"
                )
            }
            game_state.unmoved_board.clear_bit(end_file, end_rank);

            if let Some(unload_sq) = unload_square {
                move_piece(
                    real_captured_index, end_square, unload_sq, game_state
                );
            } else {
                remove_piece(real_captured_index, end_square, game_state);
            }

            game_state.halfmove_clock = 0;
            mv.set_captured_piece(real_captured_index as u8);

            move_piece(piece_index, start_square, end_square, game_state);

            let (start_file, start_rank) =
                game_state.index_to_square(start_square);

            game_state.unmoved_board.clear_bit(start_file, start_rank);

            if is_initial {
                game_state.unmoved_board.clear_bit(start_file, start_rank);
            }

            if creates_en_passant {
                game_state.en_passant_square = en_passant_square;
            } else {
                game_state.en_passant_square = None;
            }

            hash_update_en_passant(
                game_state,
                last_en_passant_square,
                en_passant_square
            );

            if is_promotion {
                let promoting_piece = piece_index;
                let promoted_piece = promoted_piece.expect(
                    "Promoted piece must be provided for promotion moves"
                ) as usize;

                promote_piece(
                    promoting_piece,
                    end_square,
                    end_square,
                    promoted_piece,
                    game_state
                );
            }

            Snapshot {
                move_ply: MoveType::SingleCapture(mv),
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                en_passant_square: last_en_passant_square,
                position_hash: last_position_hash,
            }
        }
        MoveType::HopperCapture(mv) => {                                        /* where capture square != end square */
            let piece_index = mv.piece_index() as usize;
            let start_square = mv.start_square();
            let end_square = mv.end_square();
            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoting_piece = mv.promoting_piece();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();
            let can_capture_royal = mv.can_capture_royal();
            let captured_piece_type = mv.captured_piece();
            let captured_square = mv.captured_square();
            let is_unload = mv.is_unload();
            let unload_square = mv.unload_square();

            Snapshot {
                move_ply: MoveType::HopperCapture(mv),
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                en_passant_square: last_en_passant_square,
                position_hash: last_position_hash,
            }
        }
        MoveType::MultiCapture(mv) => {
            let piece_index = mv.piece_index() as usize;
            let start_square = mv.start_square();
            let end_square = mv.end_square();
            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoting_piece = mv.promoting_piece();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();
            let taken_pieces = mv.get_taken_pieces();

            Snapshot {
                move_ply: MoveType::MultiCapture(mv),
                castling_state: last_castling_state,
                halfmove_clock: last_halfmove_clock,
                en_passant_square: last_en_passant_square,
                position_hash: last_position_hash,
            }
        }
    };

    game_state.playing = 1 - game_state.playing;
    hash_toggle_side(game_state);

    game_state.history.push(snapshot);

    if is_in_check(1 - game_state.playing, game_state) {
        undo_move(game_state);
        return false;
    };                                                                          /* check if in check otherwise undo   */

    #[cfg(debug_assertions)]
    verify_game_state(game_state);

    true
}


// undoes the last move made
#[hotpath::measure]
pub fn undo_move(game_state: &mut State) {
    unimplemented!("Undo move not implemented yet.");
}