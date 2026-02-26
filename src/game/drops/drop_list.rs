use crate::{constants::{DROP_MOVE, NO_PIECE}, count_limits, drop_d, drop_e, drop_f, drop_k, enc_can_checkmate, enc_from_enemy_hand, enc_move_type, enc_multi_move_captured_piece, enc_multi_move_captured_square, enc_multi_move_captured_unmoved, enc_piece, enc_start, game::representations::{drop::DropSet, moves::Move, piece::Piece, state::State}, get, p_color, p_index, p_is_royal, x, y};

pub fn generate_relevant_drops(
    piece: &Piece,
    square_index: u32,
    game_state: &State,
    piece_setup_drops: &[DropSet],
) -> DropSet {
    let piece_index = p_index!(piece) as usize;
    let piece_color = p_color!(piece) as usize;
    let drops = &piece_setup_drops[piece_index];

    drops.iter()
        .filter(|drop| {
            let drop_f = drop_f!(drop);
            !get!(game_state.forbidden_zones[piece_index], square_index)
            || drop_f
        })
        .flat_map(|drop| {
            let new_drop_move = drop.0 | (square_index << 8);
            let mut new_drop_stoppers = Vec::new();
            let mut new_drop_allowers = Vec::new();

            let file = square_index as i32 % game_state.files as i32;
            let rank = square_index as i32 / game_state.files as i32;

            for allower in drop.1.iter() {
                let x = x!(allower.0) * (-2 * piece_color as i8 + 1);
                let y = y!(allower.0) * (-2 * piece_color as i8 + 1);

                let check_x = file + x as i32;
                let check_y = rank + y as i32;

                if check_x < 0
                || check_x >= game_state.files as i32
                || check_y < 0
                || check_y >= game_state.ranks as i32 {
                    return Vec::new();
                } else {
                    new_drop_allowers.push(allower.clone());
                }
            }

            for stopper in drop.2.iter() {
                let x = x!(stopper.0) * (-2 * piece_color as i8 + 1);
                let y = y!(stopper.0) * (-2 * piece_color as i8 + 1);

                let check_x = file + x as i32;
                let check_y = rank + y as i32;

                if check_x >= 0
                && check_x < game_state.files as i32
                && check_y >= 0
                && check_y < game_state.ranks as i32 {
                    new_drop_stoppers.push(stopper.clone());
                }
            }

            vec![(new_drop_move, new_drop_allowers, new_drop_stoppers)]
        })
        .collect()
}

pub fn generate_drop_list(piece: &Piece, state: &State) -> Vec<Move> {

    let board_size = state.files as u32 * state.ranks as u32;
    let piece_index = p_index!(piece) as usize;
    let piece_color = p_color!(piece) as usize;
    let mut drop_list = Vec::with_capacity(128);

    if count_limits!(state)
    && state.piece_count[piece_index] >= state.piece_limit[piece_index] {
        return drop_list;
    }

    for square in 0..board_size {
        let drops = if state.setup_phase {
            &state.relevant_setup[piece_index][square as usize]
        } else {
            &state.relevant_drops[piece_index][square as usize]
        };

        if drops.is_empty() {
            continue;
        }

        'drop_loop: for drop in drops {

            if drop.1.len() > 1 {
                let drop_square = drop.1[0].0 as usize;

                if square == 0                                                  /* current square                     */
                && !drop.1[0].1.contains(&state.main_board[drop_square]) {      /* break as early as possible         */
                    continue 'drop_loop;
                }
            }

            let drop_k = drop_k!(drop);
            let drop_f = drop_f!(drop);
            let drop_d = drop_d!(drop);
            let drop_e = drop_e!(drop);

            let mut encoded_move = Move::default();
            enc_move_type!(encoded_move, DROP_MOVE);
            enc_piece!(encoded_move, piece_index as u128);
            enc_start!(encoded_move, square as u128);

            if get!(state.forbidden_zones[piece_index], square) && !drop_f
            || !drop_e && state.piece_in_hand[piece_color][piece_index] == 0
            || drop_e
            && state.piece_in_hand
            [1 - piece_color]
            [state.piece_swap_map[&(piece_index as u8)] as usize] == 0
            {
                continue 'drop_loop;
            }

            enc_can_checkmate!(encoded_move, !drop_k as u128);
            enc_from_enemy_hand!(encoded_move, drop_e as u128);

            let drop_allowers = &drop.1;
            let drop_stoppers = &drop.2;

            let file = square % state.files as u32;
            let rank = square / state.files as u32;

            for allower in drop_allowers.iter() {
                let x = x!(allower.0) as i32 * (-2 * piece_color as i32 + 1);
                let y = y!(allower.0) as i32 * (-2 * piece_color as i32 + 1);
                let allower_pieces = &allower.1;

                let check_x = file as i32 + x;
                let check_y = rank as i32 + y;
                let check_index =
                    (check_y * state.files as i32 + check_x) as usize;

                let piece_check = state.main_board[check_index];

                if !allower_pieces.contains(&piece_check) {
                    continue 'drop_loop;
                }

                if piece_check == NO_PIECE {
                    continue;
                }

                if drop_d && p_is_royal!(state.pieces[piece_check as usize]) {
                    continue 'drop_loop;
                } else if drop_d {
                    let mut take_piece = 0u64;

                    enc_multi_move_captured_piece!(
                        take_piece, piece_check as u64
                    );

                    enc_multi_move_captured_square!(
                        take_piece, check_index as u64
                    );

                    enc_multi_move_captured_unmoved!(
                        take_piece, get!(
                            state.virgin_board, check_index as u32
                        ) as u64
                    );

                    encoded_move.1.push(take_piece);
                }
            }

            for stopper in drop_stoppers.iter() {
                let x = x!(stopper.0) as i32 * (-2 * piece_color as i32 + 1);
                let y = y!(stopper.0) as i32 * (-2 * piece_color as i32 + 1);
                let stopper_pieces = &stopper.1;

                let check_x = file as i32 + x;
                let check_y = rank as i32 + y;
                let check_index =
                    (check_y * state.files as i32 + check_x) as usize;

                let piece_check = state.main_board[check_index];

                if stopper_pieces.contains(&piece_check) {
                    continue 'drop_loop;
                }
            }

            drop_list.push(encoded_move);
        }
    }

    drop_list
}