use crate::{constants::DROP_MOVE, count_limits, drop_d, drop_f, drop_k, drop_piece, enc_can_checkmate, enc_move_type, enc_multi_move_captured_piece, enc_multi_move_captured_square, enc_multi_move_captured_unmoved, enc_piece, enc_start, game::representations::{drop::DropSet, moves::Move, piece::Piece, state::State}, get, p_index, x, y};

pub fn generate_relevant_drops(
    piece: &Piece,
    square_index: u32,
    game_state: &State
) -> DropSet {
    let piece_index = p_index!(piece) as usize;
    let drops = &game_state.piece_drops[piece_index];

    drops.iter()
        .filter(|drop| {
            let drop_f = drop_f!(drop);
            !get!(game_state.forbidden_zones[piece_index], square_index)
            || drop_f
        })
        .map(|drop| {
            let new_drop_move = drop.0 | (square_index << 8);
            let mut new_drop_stoppers = Vec::new();
            let mut new_drop_allowers = Vec::new();

            let file = square_index % game_state.files as u32;
            let rank = square_index / game_state.files as u32;

            for allower in drop.1.iter() {
                let x = x!(allower) as i32;
                let y = y!(allower) as i32;

                let check_x = file as i32 + x;
                let check_y = rank as i32 + y;

                if check_x >= 0
                && check_x < game_state.files as i32
                && check_y >= 0
                && check_y < game_state.ranks as i32 {
                    new_drop_allowers.push(*allower);
                }
            }

            for stopper in drop.2.iter() {
                let x = x!(stopper) as i32;
                let y = y!(stopper) as i32;

                let check_x = file as i32 + x;
                let check_y = rank as i32 + y;

                if check_x >= 0
                && check_x < game_state.files as i32
                && check_y >= 0
                && check_y < game_state.ranks as i32 {
                    new_drop_stoppers.push(*stopper);
                }
            }

            (new_drop_move, new_drop_allowers, new_drop_stoppers)
        })
        .collect()
}

pub fn generate_drop_list(piece_index: usize, state: &State) -> Vec<Move> {

    let board_size = state.files as u32 * state.ranks as u32;
    let mut drop_list = Vec::with_capacity(128);

    if count_limits!(state)
    && state.piece_count[piece_index] >= state.piece_limit[piece_index] {
        return drop_list;
    }

    'square_loop: for square in 0..board_size {
        let drops = &state.relevant_drops[piece_index][square as usize];

        for drop in drops {
            let drop_k = drop_k!(drop);
            let drop_f = drop_f!(drop);
            let drop_d = drop_d!(drop);

            let mut encoded_move = Move::default();
            enc_move_type!(encoded_move, DROP_MOVE);
            enc_piece!(encoded_move, piece_index as u128);
            enc_start!(encoded_move, square as u128);

            if get!(state.forbidden_zones[piece_index], square) && !drop_f {
                continue 'square_loop;
            }

            enc_can_checkmate!(encoded_move, !drop_k as u128);

            let drop_allowers = &drop.1;
            let drop_stoppers = &drop.2;

            let file = square % state.files as u32;
            let rank = square / state.files as u32;

            for allower in drop_allowers.iter() {
                let x = x!(allower) as i32;
                let y = y!(allower) as i32;
                let allower_piece = drop_piece!(allower);

                let check_x = file as i32 + x;
                let check_y = rank as i32 + y;
                let check_index =
                    (check_y * state.files as i32 + check_x) as usize;

                if state.main_board[check_index] != allower_piece {
                    continue 'square_loop;
                }

                if drop_d {
                    let mut take_piece = 0u64;

                    enc_multi_move_captured_piece!(
                        take_piece, allower_piece as u64
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
                let x = x!(stopper) as i32;
                let y = y!(stopper) as i32;
                let stopper_piece = drop_piece!(stopper);

                let check_x = file as i32 + x;
                let check_y = rank as i32 + y;
                let check_index =
                    (check_y * state.files as i32 + check_x) as usize;

                if state.main_board[check_index] == stopper_piece {
                    continue 'square_loop;
                }
            }

            drop_list.push(encoded_move);
        }
    }

    drop_list
}