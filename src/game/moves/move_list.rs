#[cfg(debug_assertions)]
use crate::game::util::verify_game_state;
use crate::{
    constants::*,
    game::{hash::{hash_in_or_out_piece, hash_toggle_side, hash_update_en_passant}, representations::{
        board::Board,
        moves::{Move, MoveType, MultiMove},
        piece::Piece,
        state::{Snapshot, State}, vector::LegVector,
    }}
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

    let friendly_board = if side == WHITE {
        &game_state.white_board
    } else {
        &game_state.black_board
    };

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
                            piece_type, cap_square, can_capture_royal, is_unload, _
                        ) in mv.get_taken_pieces() {
                            if  cap_square == sq_index as u16 &&
                                !is_unload &&
                                can_capture_royal                               /* up to change, on ly used for checks*/
                            {                                                   /* attacks defined as captures only   */
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
                                                                                /* Determine promotion rank           */
    let promotion_rank = if side == WHITE {
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
        let mut en_passant_square: Option<u16> = None;
        let mut en_passant_count = 0;
        let mut move_is_initial = false;
        let mut can_capture_royal;                                              /* Track royal capture capability     */

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
                None => match (has_capture, has_destroy, is_final_leg) {
                    (Some(true), _, _) => false,
                    (_, Some(true), _) => false,
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
                                                                                /* Handle unload modifier (u)         */
            if can_unload {
                if taken_pieces.is_empty() {
                    early_fail = true;
                    break;
                }
                let last_idx = taken_pieces.len() - 1;                          /* Update last captured piece unload  */

                let (
                    piece_type, cap_square, can_cap_royal,
                    is_unload, _
                ) = taken_pieces[last_idx];

                taken_pieces[last_idx] = (
                    piece_type, cap_square, can_cap_royal, is_unload,
                    Some(start_square_index)
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
                    taken_pieces.push(
                        (
                            3, square_index, can_capture_royal,
                            has_passant == Some(false),
                            None
                        )
                    );
                } else if !can_capture && !can_move {
                    if has_enemy || is_empty {
                        early_fail = true;
                        break;
                    }
                    taken_pieces.push(
                        (
                            2, square_index, can_capture_royal,
                            has_passant == Some(false),
                            None
                        )
                    );
                } else {                                                        /* mcd or d: move/capture/destroy     */
                    if has_friendly && has_enemy {
                        taken_pieces.push(
                            (
                                3, square_index, can_capture_royal,
                                has_passant == Some(false),
                                None
                            )
                        );
                    } else if has_friendly {
                        taken_pieces.push(
                            (
                                2, square_index, can_capture_royal,
                                has_passant == Some(false),
                                None
                            )
                        );
                    } else if has_enemy {
                        taken_pieces.push(
                            (
                                1, square_index, can_capture_royal,
                                has_passant == Some(false),
                                None
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
                    taken_pieces.push(
                        (
                            1, square_index, can_capture_royal,
                            has_passant == Some(false),
                            None
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
                        taken_pieces.push(
                            (
                                1, square_index, can_capture_royal,
                                has_passant == Some(false),
                                None
                            )
                        );
                    }
                }
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
                start_square,
                end_square,
                true,                                                           /* is_initial                         */
                false,                                                          /* is_promotion                       */
                None,                                                           /* promoting_piece                    */
                None,                                                           /* promoted_piece                     */
                None,                                                           /* en_passant_square                  */
                MoveType::HopperCapture(Move::new_hopper_capture(
                    start_square,
                    end_square,
                    true,                                                       /* is_initial                         */
                    false,                                                      /* is_promotion                       */
                    None,                                                       /* promoting_piece                    */
                    None,                                                       /* promoted_piece                     */
                    None,                                                       /* en_passant_square                  */
                    0,                                                          /* captured_piece (rook)              */
                    rook_square,                                                /* captured_square                    */
                    false,                                                      /* can_capture_royal                  */
                    true,                                                       /* is_unload                          */
                    Some(unload_square),                                        /* unload_square                      */
                )),
                Some(0),                                                        /* captured_piece                     */
                Some(false),                                                    /* can_capture_royal                  */
                Some(rook_square),                                              /* captured_square                    */
                None,                                                           /* captured_pieces                    */
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
                        start_square,
                        end_square,
                        is_initial,
                        is_promotion,
                        promoting_piece,
                        promoted_piece,
                        en_passant_square,
                        MoveType::SingleNoCapture(Move::new_single_no_capture(
                            start_square, end_square, is_initial, is_promotion,
                            promoting_piece, promoted_piece, en_passant_square
                        )),
                        None,                                                   /* captured_piece                     */
                        None,                                                   /* can_capture_royal                  */
                        None,                                                   /* captured_square                    */
                        None,                                                   /* captured_pieces                    */
                        None,                                                   /* is_unload                          */
                        None,                                                   /* unload_square                      */
                    )
                }
                1 => {
                    let (
                        piece, square, can_cap_royal,
                        is_ep, unload_square
                    ) = taken_pieces[0];

                    if square != end_square {
                        Move::encode(
                            start_square,
                            end_square,
                            is_initial,
                            is_promotion,
                            promoting_piece,
                            promoted_piece,
                            en_passant_square,
                            MoveType::HopperCapture(Move::new_hopper_capture(
                                start_square, end_square, is_initial,
                                is_promotion, promoting_piece, promoted_piece,
                                en_passant_square, piece, square,
                                can_cap_royal,
                                is_ep, unload_square
                            )),
                            Some(piece),
                            Some(can_cap_royal),
                            Some(square),
                            None,                                               /* captured_pieces                    */
                            Some(is_ep),
                            unload_square,
                        )
                    } else {
                        Move::encode(
                            start_square,
                            end_square,
                            is_initial,
                            is_promotion,
                            promoting_piece,
                            promoted_piece,
                            en_passant_square,
                            MoveType::SingleCapture(Move::new_single_capture(
                                start_square, end_square, is_initial,
                                is_promotion, promoting_piece, promoted_piece,
                                en_passant_square, piece, can_cap_royal,
                                is_ep, unload_square
                            )),
                            Some(piece),
                            Some(can_cap_royal),
                            None,                                               /* captured_square                    */
                            None,                                               /* captured_pieces                    */
                            Some(is_ep),
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
                        promoting_piece,
                        promoted_piece,
                        en_passant_square,
                        MoveType::MultiCapture(MultiMove::new_multi_capture(
                            start_square, end_square, is_initial,
                            is_promotion, promoting_piece,
                            promoted_piece, en_passant_square,
                            taken_pieces.clone()
                        )),
                        None,                                                   /* captured_piece                     */
                        None,                                                   /* can_capture_royal                  */
                        None,                                                   /* captured_square                    */
                        Some(taken_pieces.clone()),                             /* captured_pieces for multi-capture  */
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

pub fn make_move(game_state: &mut State, mv: MoveType) -> bool{
    let prev_en_passant = game_state.en_passant_square;

    let snapshot: Snapshot = match mv {
        MoveType::SingleNoCapture(mv) => {
            let start_square = mv.start_square();
            let end_square = mv.end_square();

            let (start_file, start_rank) =
                game_state.index_to_square(start_square);
            let (end_file, end_rank) =
                game_state.index_to_square(end_square);

            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoting_piece = mv.promoting_piece();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();

            let piece_index = find_piece_at_square(
                game_state,
                start_file,
                start_rank
            ).expect(
                "No piece found at start square for single no-capture move!"
            ).index() as usize;

            #[cfg(debug_assertions)]
            verify_game_state(game_state);

            Snapshot {
                move_ply: MoveType::SingleNoCapture(mv),
                castling_state: game_state.castling_state,
                halfmove_clock: game_state.halfmove_clock,
                en_passant_square: game_state.en_passant_square,
                position_hash: game_state.position_hash,
            }
        }
        MoveType::SingleCapture(mv) => {                                        /* where capture square = end square  */
            let start_square = mv.start_square();
            let end_square = mv.end_square();

            let (start_file, start_rank) =
                game_state.index_to_square(start_square);
            let (end_file, end_rank) =
                game_state.index_to_square(end_square);

            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoting_piece = mv.promoting_piece();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();
            let can_capture_royal = mv.can_capture_royal();
            let captured_index = mv.captured_piece_type();
            let is_unload = mv.is_unload();
            let unload_square = mv.unload_square();

            let piece_index = find_piece_at_square(
                game_state,
                start_file,
                start_rank
            ).expect(
                "No piece found at start square for single capture move!"
            ).index() as usize;

            #[cfg(debug_assertions)]
            verify_game_state(game_state);

            Snapshot {
                move_ply: MoveType::SingleCapture(mv),
                castling_state: game_state.castling_state,
                halfmove_clock: game_state.halfmove_clock,
                en_passant_square: game_state.en_passant_square,
                position_hash: game_state.position_hash,
            }
        }
        MoveType::HopperCapture(mv) => {                                        /* where capture square != end square */
            let start_square = mv.start_square();
            let end_square = mv.end_square();

            let (start_file, start_rank) =
                game_state.index_to_square(start_square);
            let (end_file, end_rank) =
                game_state.index_to_square(end_square);

            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoting_piece = mv.promoting_piece();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();
            let can_capture_royal = mv.can_capture_royal();
            let captured_index = mv.captured_piece_type();
            let captured_square = mv.captured_square();
            let is_unload = mv.is_unload();
            let unload_square = mv.unload_square();

            let piece_index = find_piece_at_square(
                game_state,
                start_file,
                start_rank
            ).expect(
                "No piece found at start square for hopper capture move!"
            ).index() as usize;

            #[cfg(debug_assertions)]
            verify_game_state(game_state);

            Snapshot {
                move_ply: MoveType::HopperCapture(mv),
                castling_state: game_state.castling_state,
                halfmove_clock: game_state.halfmove_clock,
                en_passant_square: game_state.en_passant_square,
                position_hash: game_state.position_hash,
            }
        }
        MoveType::MultiCapture(mv) => {
            let start_square = mv.start_square();
            let end_square = mv.end_square();

            let (start_file, start_rank) =
                game_state.index_to_square(start_square);
            let (end_file, end_rank) =
                game_state.index_to_square(end_square);

            let is_initial = mv.is_initial();
            let is_promotion = mv.is_promotion();
            let creates_en_passant = mv.creates_en_passant_square();
            let promoting_piece = mv.promoting_piece();
            let promoted_piece = mv.promoted_piece();
            let en_passant_square = mv.en_passant_square();
            let taken_pieces = mv.get_taken_pieces();

            let piece_index = find_piece_at_square(
                game_state,
                start_file,
                start_rank
            ).expect(
                "No piece found at start square for multi-capture move!"
            ).index() as usize;

            #[cfg(debug_assertions)]
            verify_game_state(game_state);

            Snapshot {
                move_ply: MoveType::MultiCapture(mv),
                castling_state: game_state.castling_state,
                halfmove_clock: game_state.halfmove_clock,
                en_passant_square: game_state.en_passant_square,
                position_hash: game_state.position_hash,
            }
        }
        _ => {
            unimplemented!("Make move for non SingleNoCapture not implemented yet.");
        }
    };

    game_state.playing = 1 - game_state.playing;
    hash_toggle_side(game_state);

    game_state.ply += 1;
    game_state.ply_counter += 1;

    game_state.history.push(snapshot);

    if is_in_check(1 - game_state.playing, game_state) {
        undo_move(game_state);
        return false;
    };                                                                          /* check if in check otherwise undo   */

    true
}


// undoes the last move made
pub fn undo_move(game_state: &mut State) {
    unimplemented!("Undo move not implemented yet.");
}