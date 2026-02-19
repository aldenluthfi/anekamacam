
#[cfg(debug_assertions)]
use crate::game::util::verify_game_state;
use crate::{
    constants::{DROP_MOVE, NO_PIECE, BK_CASTLE, BQ_CASTLE, MULTI_CAPTURE_MOVE,
        NO_EN_PASSANT, QUIET_MOVE, SINGLE_CAPTURE_MOVE, WK_CASTLE, WQ_CASTLE},
    drop_f, drop_k, enc_move_type, enc_piece, enc_start, get, make_move,
    p_index, stalemate_loss, x, y, is_initial,
    game::{
        moves::move_list::{is_in_check, there_is_legal_move},
        representations::{
            drop::Drops,
            moves::Move,
            piece::Piece,
            state::{State, EnPassantSquare, Snapshot, Square},
        },
        hash::zobrist::{
            CASTLING_HASHES, EN_PASSANT_HASHES, IN_HAND_HASHES, PIECE_HASHES,
            SIDE_HASHES,
        },
    },
    captured_piece, captured_square, captured_unmoved, clear,
    created_enp, creates_enp, demote_upon_capture, drops, end, enp_square,
    hash_in_or_out_piece, hash_toggle_side, hash_update_castling,
    hash_update_en_passant, hash_update_in_hand, is_unload, move_type,
    multi_move_captured_piece, multi_move_captured_square,
    multi_move_captured_unmoved, multi_move_is_unload,
    multi_move_unload_square, p_can_promote, p_castle_left, p_castle_right,
    p_color, p_is_big, p_is_major, p_is_minor, p_is_royal, p_value, piece,
    promote_to_captured, promoted, promotion, set, start, undo_move,
    unload_square,
};

pub fn generate_relevant_drops(
    piece: &Piece,
    square_index: u32,
    game_state: &State
) -> Drops {
    let drop = &game_state.piece_drops[p_index!(piece) as usize];

    let new_drop_move = drop.0 | (square_index << 8);
    let mut new_drop_stoppers = Vec::new();

    let file = square_index % game_state.files as u32;
    let rank = square_index / game_state.files as u32;

    for stopper in drop.1.iter() {
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

    (new_drop_move, new_drop_stoppers)
}

pub fn generate_drop_list(piece_index: usize, state: &mut State) -> Vec<Move> {

    let board_size = state.files as u32 * state.ranks as u32;
    let mut drop_list = Vec::with_capacity(128);

    let main_drop = &state.piece_drops[piece_index];
    let drop_k = drop_k!(main_drop);
    let drop_f = drop_f!(main_drop);

    'square_loop: for square in 0..board_size {

        if state.main_board[square as usize] != NO_PIECE {
            continue;
        }

        let drop_stoppers =
            &state.relevant_drops[piece_index][square as usize].1;

        let file = square % state.files as u32;
        let rank = square / state.files as u32;

        for stopper in drop_stoppers.iter() {
            let x = x!(stopper) as i32;
            let y = y!(stopper) as i32;

            let check_x = file as i32 + x;
            let check_y = rank as i32 + y;
            let check_index = (check_y * state.files as i32 + check_x) as usize;

            if state.main_board[check_index] == piece_index as u8 {
                continue 'square_loop;
            }
        }

        if get!(state.forbidden_zones[piece_index], square) && !drop_f {
            continue 'square_loop;
        }

        let mut encoded_move = Move::default();
        enc_move_type!(encoded_move, DROP_MOVE);
        enc_piece!(encoded_move, piece_index as u128);
        enc_start!(encoded_move, square as u128);

        if drop_k {
            let make = make_move!(state, encoded_move.clone());

            if !make {
                continue 'square_loop;
            }

            let is_in_check = is_in_check(state.playing, state);

            if !is_in_check && !stalemate_loss!(state) {
                undo_move!(state);
                drop_list.push(encoded_move);
                continue 'square_loop;
            }

            if !there_is_legal_move(state) {
                undo_move!(state);
                continue 'square_loop;
            }

            undo_move!(state);
        }

        drop_list.push(encoded_move);
    }

    drop_list
}