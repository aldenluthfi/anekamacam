use crate::{
    representations::{
        board::Board, moves::{Move, MoveType},
        state::State,
        vector::{
            LegVector, MultiLegVector
        }
    }
};

fn check_out_of_bounds(
    square: (i8, i8)
) -> bool {
    let game_state = State::global();
    square.0 < 0
        || square.0 >= game_state.files as i8
        || square.1 < 0
        || square.1 >= game_state.ranks as i8
}

/// Generates a board that are relevant to the given move vectors:
pub fn generate_relevant_boards(
    vector_set: &Vec<MultiLegVector>,
    square: (u8, u8)
) -> Board {
    let game_state = State::global();

    assert!(!check_out_of_bounds((
        square.0 as i8,
        square.1 as i8
    )), "Square {:?} is out of bounds", square);

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

            if check_out_of_bounds(accumulated_offset) {
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
    relevant_enemy_board: &Board
) -> Vec<Move> {

    assert!(!check_out_of_bounds((
        square.0 as i8,
        square.1 as i8
    )), "Square {:?} is out of bounds", square);

    let game_state = State::global();
    let mut move_list: Vec<Move> = Vec::new();

    for mut multi_leg_vector in vector_set.clone() {
        let mut accumulated_offset: (i8, i8) = (square.0 as i8, square.1 as i8);
        let mut captured_pieces: Vec<(u8, u16)> = Vec::new();
        let mut early_fail = false;

        multi_leg_vector.reverse();

        while multi_leg_vector.len() > 1 {
            let leg: LegVector = multi_leg_vector.pop().expect(
                "Unexpected error: Leg vector should exist!"
            );

            accumulated_offset.0 = accumulated_offset.0.saturating_add(
                leg.get_atomic().whole().0
            );

            accumulated_offset.1 = accumulated_offset.1.saturating_add(
                leg.get_atomic().whole().1
            );

            let file = accumulated_offset.0 as u8;
            let rank = accumulated_offset.1 as u8;

            if check_out_of_bounds(accumulated_offset) {
                break;
            }

            match (leg.is_c(), leg.is_d()) {
                (Some(true), Some(true)) => {
                    if  !relevant_enemy_board.get_bit(file, rank) &&
                        !relevant_friendly_board.get_bit(file, rank)
                    {
                        early_fail = true;
                        break;
                    } else {
                        let square = game_state.square_to_index(file, rank);
                        let enemy = 3;                                          /* mark that this can be both         */
                        captured_pieces.push((enemy, square));
                    }
                }
                (Some(true), _) => {
                    if !relevant_enemy_board.get_bit(file, rank) {
                        early_fail = true;
                        break;
                    } else {
                        let square = game_state.square_to_index(file, rank);
                        let enemy = 1;                                          /* mark that this is an enemy         */
                        captured_pieces.push((enemy, square));
                    }
                }
                (_, Some(true)) => {
                    if !relevant_friendly_board.get_bit(file, rank) {
                        early_fail = true;
                        break;
                    } else {
                        let square = game_state.square_to_index(file, rank);
                        let friendly = 2;                                       /* mark that this is a friendly       */
                        captured_pieces.push((friendly, square));
                    }
                }
                _ => {                                                          /* default is empty squares only      */
                    if  relevant_enemy_board.get_bit(file, rank) ||
                        relevant_friendly_board.get_bit(file, rank)
                    {
                        early_fail = true;
                        break;
                    }
                }
            }
        }

        if early_fail {
            continue;
        }

        assert_eq!(
            multi_leg_vector.len(),
            1,
            "Multi-leg vector should have exactly one leg left!"
        );

        let final_leg: LegVector = multi_leg_vector.pop().expect(
            "Unexpected error: Final leg vector should exist!"
        );

        accumulated_offset.0 = accumulated_offset.0.saturating_add(
            final_leg.get_atomic().whole().0
        );

        accumulated_offset.1 = accumulated_offset.1.saturating_add(
            final_leg.get_atomic().whole().1
        );

        if check_out_of_bounds(accumulated_offset) {
            continue;
        }

        let file = accumulated_offset.0 as u8;
        let rank = accumulated_offset.1 as u8;

        match final_leg.is_d() {
            Some(true) => {
                if !relevant_friendly_board.get_bit(file, rank) {
                    break;
                }
            }
            _ => {
                if relevant_friendly_board.get_bit(file, rank) {                /* can move or capture by default     */
                    break;
                }
            }
        }

        let start_square = game_state.square_to_index(square.0, square.1);
        let end_square = game_state.square_to_index(file, rank);

        let is_initial = false;
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
                    MoveType::SingleNoCapture { is_castling: false }
                )
            }
            1 => {
                let (piece_type, square) = captured_pieces[0];

                if square != end_square {
                    Move::encode(
                        start_square,
                        end_square,
                        is_initial,
                        is_promotion,
                        promotion_piece,
                        MoveType::HopperCapture {
                            captured_piece: (piece_type),
                            captured_square: (square)
                        }
                    )
                } else {
                    Move::encode(
                        start_square,
                        end_square,
                        is_initial,
                        is_promotion,
                        promotion_piece,
                        MoveType::SingleCapture { captured_piece: piece_type }
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
                    MoveType::MultiCapture {
                        captured_pieces: captured_pieces.clone()
                    }
                )
            }
        };

        move_list.push(encoded_move);
    }

    move_list
}