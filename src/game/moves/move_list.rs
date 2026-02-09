use bnum::types::U4096;
use rayon::{iter::{IntoParallelIterator, ParallelIterator}};

use crate::{board, captured_square, constants::{MULTI_CAPTURE_MOVE, SINGLE_CAPTURE_MOVE, WHITE}, enc_piece, game::representations::{board::Board, moves::Move, piece::Piece, state::State, vector::MoveSet}, get, move_type, multi_move_captured_square, multi_move_is_unload, set, x, y};

#[hotpath::measure]
fn check_out_of_bounds(
    square_index: i32,
    game_state: &State
) -> bool {
    square_index < 0
        || square_index >= (game_state.files as i32 * game_state.ranks as i32)
}

#[hotpath::measure]
fn is_square_attacked(
    square_index: u32,
    side: u8,
    game_state: &State
) -> bool {

    #[cfg(debug_assertions)]
    {
        use crate::constants::BLACK;

        assert!(!check_out_of_bounds(
            square_index as i32,
            game_state
        ), "Square index {} is out of bounds", square_index);

        assert!(
            side == WHITE || side == BLACK,
            "Side must be WHITE or BLACK"
        );
    }

    let side_piece_count = game_state.pieces.len() / 2;
    let index = side_piece_count * side as usize;

    let friendly_board = &game_state.pieces_board[side as usize];
    let enemy_board = &game_state.pieces_board[1 - side as usize];
    let unmoved_board = &game_state.virgin_board;

    for i in index..index + side_piece_count {
        let piece = &game_state.pieces[i];

        for attacker_index in &game_state.piece_list[i] {
            let relevant_board =
                &game_state.relevant_board[i][*attacker_index as usize];

            if !get!(relevant_board, square_index) {
                continue;
            }

            let move_list = generate_move_list(
                *attacker_index,
                piece,
                friendly_board,
                enemy_board,
                unmoved_board,
                game_state,
                true
            );

            for moves in move_list {
                let move_type = move_type!(moves);
                let captured_square = captured_square!(moves);

                if  move_type == SINGLE_CAPTURE_MOVE &&
                    captured_square == square_index as u128
                {
                    return true;
                } else if move_type == MULTI_CAPTURE_MOVE {
                    let multi_captures = &moves.1;

                    for &multi_capture in multi_captures {
                        let captured_square =
                            multi_move_captured_square!(multi_capture);

                        if captured_square == square_index as u64 &&
                           !multi_move_is_unload!(multi_capture)
                        {
                            return true;
                        }
                    }
                }
            }
        }
    }

    false
}

#[hotpath::measure]
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
    #[cfg(debug_assertions)]
    {
        assert!(!check_out_of_bounds(
            square_index as i32,
            game_state
        ), "Square index {} is out of bounds", square_index);
    }

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
            rank += rank_offset as i32 * (-2 * side as i32 + 1);

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
    #[cfg(debug_assertions)]
    {
        assert!(!check_out_of_bounds(
            square_index as i32,
            game_state
        ), "Square index {} is out of bounds", square_index);
    }

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

#[hotpath::measure]
pub fn generate_move_list(
    square_index: u16,
    piece: &Piece,
    friendly_board: &Board,
    enemy_board: &Board,
    unmoved_board: &Board,
    game_state: &State,
    imaginary: bool
) -> Vec<Move> {

    #[cfg(debug_assertions)]
    {
        assert!(!check_out_of_bounds(
            square_index as i32,
            game_state
        ), "Square index {} is out of bounds", square_index);
    }

    unimplemented!()
}

#[hotpath::measure]
pub fn make_move(game_state: &mut State, mv: Move) -> bool {
    false
}

#[hotpath::measure]
pub fn undo_move(game_state: &mut State) {
}

#[hotpath::measure]
pub fn generate_all_moves(state: &State) -> Vec<Move> {
    let piece_count = state.pieces.len() / 2;
    let start_index = piece_count * state.playing as usize;
    let end_index = start_index + piece_count;

    let moves: Vec<Move> = (start_index..end_index)
        .into_par_iter()
        .flat_map(|piece_index| {
            let piece = &state.pieces[piece_index];
            let friendly_board = &state.pieces_board[piece.color() as usize];
            let enemy_board = &state.pieces_board[1 - piece.color() as usize];
            state.piece_list[piece_index]
                .iter()
                .flat_map(|index| {
                    generate_move_list(
                        *index,
                        piece,
                        friendly_board,
                        enemy_board,
                        &state.virgin_board,
                        state,
                        false
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect();

    moves
}