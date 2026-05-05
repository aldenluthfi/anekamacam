use rand::Rng;

use crate::*;

type PieceRoles = (PieceIndex, bool, bool);
/// The peice roles are determined as follows:
///
/// 1. Sort all the values of pieces except for the royal pieces
/// 2. the bottom ceil(10%) of pieces are the non-big pieces,
/// 3. the top ceil(30%) of pieces are the major pieces,
/// 4. the rest are minor pieces.
///
fn derive_piece_roles(state: &mut State) -> Vec<PieceRoles> {
    let mut values = HashSet::new();

    for piece in state.pieces.iter() {
        let value = p_ovalue!(piece);
        values.insert(value);
    }

    let mut values = values.iter().collect::<Vec<_>>();

    values.sort_unstable();

    let non_big_threshold = (values.len() as f32 * 0.1).ceil() as usize;
    let major_threshold = (values.len() as f32 * 0.8).ceil() as usize;

    let mut piece_roles = Vec::new();

    for piece in state.pieces.iter() {
        let value = p_ovalue!(piece);

        let is_big = value > *values[non_big_threshold - 1];
        let is_major = value >= *values[major_threshold - 1];

        piece_roles.push((p_index!(piece), is_big, is_major));
    }

    piece_roles
}

/// The steps of deriving a piece's value are as follows:
///
/// We need to see if a piece is confined to only a few squares on the board,
/// to do this we can place a piece on a square and generate all ending
/// squares it can move in one turn except the ones we've already seen, we
/// repeat this until theres no new squares, if we cannot make move on the
/// initial we will mark that square as 0, once we check for all squares on
/// the board, we will average all the nonzero values and the ratio between
/// that and the total number of squares on the board will be the reach value
/// of the piece
///
/// Next we calculate the average mobility of a piece, on any given square,
/// we can take all the end squares that piece can move to and multiply all the
/// probabilities the piece can reach that square depending on the leg.
///
/// the final value of the piece will be:
///
/// Opening: 2 * board_size * (0.4 * reach_value + 0.6 * mobility_value)
///
/// Endgame:
/// - if piece is a major piece: Opening value * 1.2
/// - if piece is a minor piece: Opening value * 1.1
/// - if piece is a non-big piece: Opening value * 1.5
///
fn derive_piece_value(state: &State, piece: &Piece) -> f64 {
    let board_size = state.main_board.len();
    let mut reach_values = vec![-1; board_size];

    let piece_index = p_index!(piece) as usize;

    let mut i = 0;
    while i < 100.min(board_size) {

        let square = RNG.lock().unwrap().random_range(0..board_size);

        if reach_values[square] >= 0 {
            continue;
        }

        let mut reached_squares: HashSet<usize> = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(square);

        while let Some(square) = queue.pop_front() {
            reached_squares.insert(square);

            let relevant_moves =
                &state.relevant_moves[piece_index * board_size + square];

            for multi_leg_vector in relevant_moves {
                let mut accumulated_index = square as i32;

                let mut file = accumulated_index % (state.files as i32);
                let mut rank = accumulated_index / (state.files as i32);

                for leg in multi_leg_vector {
                    let file_offset = x!(leg);
                    let rank_offset = y!(leg);

                    file += file_offset as i32;
                    rank += rank_offset as i32;
                    accumulated_index = rank * (state.files as i32) + file;
                }

                if !reached_squares.contains(&(accumulated_index as usize))
                {
                    queue.push_back(accumulated_index as usize);
                }
            }
        }

        if reached_squares.len() == 1 {
            reach_values[square] = 0;
        } else {
            reach_values[square] = reached_squares.len() as i32;
        }

        i += 1;
    }

    let mean = reach_values.iter().filter(|&&v| v > 0).sum::<i32>() as f64
        / reach_values.iter().filter(|&&v| v > 0).count() as f64;

    let reach_value = mean / board_size as f64;

    let mut mobility_values = vec![-1.0; board_size];

    let mut i = 0;
    while i < 100.min(board_size) {
        let square = RNG.lock().unwrap().random_range(0..board_size);

        if mobility_values[square] >= 0.0 {
            continue;
        }
        i += 1;

        mobility_values[square] =
            derive_piece_mobility(state, piece_index as PieceIndex, square);
    }

    let mobility_value =
        mobility_values.iter().filter(|&&v| v >= 0.0).sum::<f64>()
        / mobility_values.iter().filter(|&&v| v >= 0.0).count() as f64;

    2.0 * board_size as f64 * (0.4 * reach_value + 0.6 * mobility_value)
}

fn derive_piece_mobility(
    state: &State, piece_index: PieceIndex, square: usize
) -> f64 {
    let board_size = state.main_board.len();

    let relevant_moves =
        &state.relevant_moves[piece_index as usize * board_size + square];

    let mut mobility_sum = 0.0;

    for multi_leg_vector in relevant_moves {
        let mut mobility_product = 1.0;

        for (leg_index, leg) in multi_leg_vector.iter().enumerate() {
            let last_leg = leg_index + 1 == multi_leg_vector.len();

            let m = m!(leg) || (!c!(leg) && !d!(leg));
            let d = d!(leg);
            let c = c!(leg) || (last_leg && !m!(leg) && !d);

            match (m, c, d) {
                (true, false, false) |
                (false, true, false) |
                (false, false, true) => {
                    mobility_product *= 0.75;
                },
                (true, true, false) |
                (true, false, true) |
                (false, true, true) => {
                    mobility_product *= 0.85;
                },
                (true, true, true) => {
                    mobility_product *= 1.0;
                },
                _ => {
                    panic!("Cannot move, destroy or capture with leg");
                }
            }
        }

        mobility_sum += mobility_product;
    }

    mobility_sum
}

fn derive_piece_opening_value_on_square(
    state: &State, piece_index: PieceIndex, square: usize
) -> i32 {

    let mobility = derive_piece_mobility(state, piece_index, square);
    let center_file = if state.files % 2 == 1 {
        vec![(state.files as f64 / 2.0).floor() as u8]
    } else {
        vec![state.files / 2, (state.files / 2) - 1]
    };
    let center_rank = if state.ranks % 2 == 1 {
        vec![(state.ranks as f64 / 2.0).floor() as u8]
    } else {
        vec![state.ranks / 2, (state.ranks / 2) - 1]
    };

    let mut center_squares = vec![];

    for file in &center_file {
        for rank in &center_rank {
            center_squares.push(*rank * state.files + *file);
        }
    }

    let distance_from_center = center_squares
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or_else(
            || panic!("Error while getting distance from center squares")
        );
    let piece = &state.pieces[piece_index as usize];

    if p_can_promote!(piece) {
        let closest_mandatory = set_indices!(
            state.promotion_zones_mandatory[piece_index as usize]
        )
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or(f64::INFINITY);

        let closest_optional = set_indices!(
            state.promotion_zones_optional[piece_index as usize]
        )
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or(f64::INFINITY);

        let closest_promotion = closest_optional.min(closest_mandatory);

        let opening_value =
            10.0 *
            (0.5 + 0.5 * mobility) *
            (1.0 - 0.7 * distance_from_center as f64) *
            (1.0 - 0.3 * closest_promotion as f64);

        if p_is_royal!(piece) { -opening_value.round() as i32 }
        else { opening_value.round() as i32 }
    } else {
        let opening_value =
            10.0 *
            (0.5 + 0.5 * mobility) *
            (1.0 - 0.5 * distance_from_center as f64);

        if p_is_royal!(piece) { -opening_value.round() as i32 }
        else { opening_value.round() as i32 }
    }
}

fn derive_opening_pst(index: PieceIndex, state: &State) -> Vec<i32> {
    let mut pst = Vec::new();
    let board_size = state.main_board.len();

    for square in 0..board_size {
        let value = derive_piece_opening_value_on_square(state, index, square);
        pst.push(value);
    }

    pst
}

fn derive_piece_endgame_value_on_square(
    state: &State, piece_index: PieceIndex, square: usize
) -> i32 {
    let mobility = derive_piece_mobility(state, piece_index, square);
    let center_file = if state.files % 2 == 1 {
        vec![(state.files as f64 / 2.0).floor() as u8]
    } else {
        vec![state.files / 2, (state.files / 2) - 1]
    };
    let center_rank = if state.ranks % 2 == 1 {
        vec![(state.ranks as f64 / 2.0).floor() as u8]
    } else {
        vec![state.ranks / 2, (state.ranks / 2) - 1]
    };

    let mut center_squares = vec![];

    for file in &center_file {
        for rank in &center_rank {
            center_squares.push(*rank * state.files + *file);
        }
    }

    let distance_from_center = center_squares
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or_else(
            || panic!("Error while getting distance from center squares")
        );

    let piece = &state.pieces[piece_index as usize];

    if p_can_promote!(piece) {
        let closest_mandatory = set_indices!(
            state.promotion_zones_mandatory[piece_index as usize]
        )
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or(f64::INFINITY);


        let closest_optional = set_indices!(
            state.promotion_zones_optional[piece_index as usize]
        )
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or(f64::INFINITY);

        let closest_promotion = closest_optional.min(closest_mandatory);

        let opening_value =
            10.0 *
            (0.5 + 0.5 * mobility) *
            (1.0 - 0.2 * distance_from_center as f64) *
            (1.0 - 0.8 * closest_promotion as f64);

        opening_value.round() as i32

    } else {
        let opening_value =
            10.0 *
            (0.5 + 0.5 * mobility) *
            (1.0 - 0.3 * distance_from_center as f64);

        opening_value.round() as i32
    }
}


fn derive_endgame_pst(index: PieceIndex, state: &State) -> Vec<i32> {
    let mut pst = Vec::new();
    let board_size = state.main_board.len();

    for square in 0..board_size {
        let value = derive_piece_endgame_value_on_square(state, index, square);
        pst.push(value);
    }

    pst
}

pub fn derive_parameters(state: &mut State) {
    let mut values = Vec::new();

    let mut min = f64::INFINITY;
    for (index, piece) in state.pieces.iter().enumerate() {
        if p_color!(piece) == BLACK {
            continue;
        }

        let value = derive_piece_value(state, piece);
        if value < min {
            min = value;
        }

        values.push((index as PieceIndex, value));
    }

    let offset = 100.0 - min;

    for (index, value) in values.clone() {
        let black_index = state.piece_swap_map[&index] as usize;
        let white_index = index as usize;
        let value = (value + offset).round() as u16;

        set_piece_dynamic_parameters(
            &mut state.pieces[black_index], value, 0, false, false);
        set_piece_dynamic_parameters(
            &mut state.pieces[white_index], value, 0, false, false);
    }

    let piece_roles = derive_piece_roles(state);

    for (piece_index, is_big, is_major) in piece_roles {
        let piece = &mut state.pieces[piece_index as usize];
        let value = p_ovalue!(piece);

        set_piece_dynamic_parameters(
            piece, value, 0, is_big, is_major
        );
    }

    for piece in state.pieces.iter_mut() {
        let ovalue = p_ovalue!(piece);
        let is_big = p_is_big!(piece);
        let is_major = p_is_major!(piece);

        let multiplier = if is_major {
            1.2
        } else if is_big {
            1.1
        } else {
            1.5
        };

        let evalue = (ovalue as f64 * multiplier).round() as u16;

        set_piece_dynamic_parameters(piece, ovalue, evalue, is_big, is_major);
    }

    let total_values = values.len() as f64;
    let average_value = values
        .into_iter()
        .map(|(_, value)| value)
        .sum::<f64>() / total_values;

    state.opening_score =
        average_value.round() as u32 * state.pieces.len() as u32 * 2;
    state.endgame_score =
        average_value.round() as u32 * 5;

    state.most_valuable = state
        .pieces
        .iter()
        .map(|piece| p_ovalue!(piece))
        .max()
        .unwrap_or(0);

    state.big_pieces = [0; 2];
    state.major_pieces = [0; 2];
    state.minor_pieces = [0; 2];

    for (piece_idx, piece) in state.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let count = state.piece_list[piece_idx].len() as u32;

        state.big_pieces[color] += count * (p_is_big!(piece) as u32);
        state.major_pieces[color] += count * (p_is_major!(piece) as u32);
        state.minor_pieces[color] += count * (p_is_minor!(piece) as u32);
    }

    for piece in state.pieces.iter() {
        let mut index = p_index!(piece);

        if p_color!(piece) == BLACK {
            index = state.piece_swap_map[&(index as u8)] as PieceIndex;
        }

        let mut opening_pst = derive_opening_pst(index, state);
        let mut endgame_pst = derive_endgame_pst(index, state);

        if p_color!(piece) == BLACK {
            opening_pst = mirror_pst_across_horizontal_axis(
                &opening_pst, state.files as usize, state.ranks as usize
            );
            endgame_pst = mirror_pst_across_horizontal_axis(
                &endgame_pst, state.files as usize, state.ranks as usize
            );

            index = state.piece_swap_map[&(index as u8)] as PieceIndex;
        }

        state.pst_opening[index as usize] = opening_pst;
        state.pst_endgame[index as usize] = endgame_pst;
    }

    refresh_eval_state(state);
}
