//! # parameters.rs
//!
//! Automatic derivation of dynamic evaluation parameters for pieces.
//!
//! This module implements heuristics to automatically score pieces based on
//! their movement capabilities and constraints. It calculates raw piece values
//! by analyzing board reach and mobility, derives piece roles (major, minor,
//! big) based on relative value thresholds, and generates nuanced Piece-Square
//! Tables (PSTs) for both opening and endgame phases, uniquely tailored to the
//! specific variant's board size and piece definitions.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 08/05/2026

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

    for piece in state.statics.pieces.iter() {
        let value = p_ovalue!(piece);
        values.insert(value);
    }

    let mut values = values.iter().collect::<Vec<_>>();

    values.sort_unstable();

    let non_big_threshold = (values.len() as f32 * 0.1).ceil() as usize;
    let major_threshold = (values.len() as f32 * 0.8).ceil() as usize;

    log_4!(
        "Role thresholds - Non-big: {}, Major: {}",
        non_big_threshold, major_threshold
    );

    let mut piece_roles = Vec::new();

    for piece in state.statics.pieces.iter() {
        let value = p_ovalue!(piece);

        let is_big = value > *values[non_big_threshold - 1];
        let is_major = value >= *values[major_threshold - 1];

        piece_roles.push((p_index!(piece), is_big, is_major));
    }

    piece_roles
}

fn derive_piece_reach(state: &State, piece: &Piece) -> f64 {
    let board_size = state.statics.board_size;
    let piece_index = p_index!(piece) as usize;

    let reach_values: Vec<i32> = (0..board_size).into_par_iter().map(|square| {
        let mut reached_squares: HashSet<usize> = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(square);

        while let Some(square) = queue.pop_front() {
            reached_squares.insert(square);

            let relevant_moves = &state.statics.relevant_moves
                [piece_index * board_size + square];

            for multi_leg_vector in relevant_moves {
                let mut accumulated_index = square as i32;

                let mut file =
                    accumulated_index % (state.statics.files as i32);
                let mut rank =
                    accumulated_index / (state.statics.files as i32);

                for leg in multi_leg_vector {
                    let file_offset = x!(leg);
                    let rank_offset = y!(leg);

                    file += file_offset as i32;
                    rank += rank_offset as i32;
                    accumulated_index =
                        rank * (state.statics.files as i32) + file;
                }

                if !reached_squares.contains(&(accumulated_index as usize))
                {
                    queue.push_back(accumulated_index as usize);
                }
            }
        }

        if reached_squares.len() == 1 { 0 } else { reached_squares.len() as i32 }
    }).collect();

    let mean = reach_values.iter().filter(|&&v| v > 0).sum::<i32>() as f64
        / reach_values.iter().filter(|&&v| v > 0).count() as f64;

    mean / board_size as f64
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
    log_4!("Deriving base value for piece '{}'", piece.char);

    let board_size = state.statics.board_size;
    let piece_index = p_index!(piece) as usize;

    let reach_values: Vec<i32> = (0..board_size).into_par_iter().map(|square| {
        let mut reached_squares: HashSet<usize> = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(square);

        while let Some(square) = queue.pop_front() {
            reached_squares.insert(square);

            let relevant_moves = &state.statics.relevant_moves
                [piece_index * board_size + square];

            for multi_leg_vector in relevant_moves {
                let mut accumulated_index = square as i32;

                let mut file =
                    accumulated_index % (state.statics.files as i32);
                let mut rank =
                    accumulated_index / (state.statics.files as i32);

                for leg in multi_leg_vector {
                    let file_offset = x!(leg);
                    let rank_offset = y!(leg);

                    file += file_offset as i32;
                    rank += rank_offset as i32;
                    accumulated_index =
                        rank * (state.statics.files as i32) + file;
                }

                if !reached_squares.contains(&(accumulated_index as usize))
                {
                    queue.push_back(accumulated_index as usize);
                }
            }
        }

        if reached_squares.len() == 1 {
            0
        } else {
            reached_squares.len() as i32
        }
    }).collect();

    let mean = reach_values.iter().filter(|&&v| v > 0).sum::<i32>() as f64
        / reach_values.iter().filter(|&&v| v > 0).count() as f64;

    let reach_value = mean / board_size as f64;

    log_4!("Reach value for piece '{}': {:.4}", piece.char, reach_value);

    let mobility_values: Vec<f64> = (0..board_size).into_par_iter().map(|square| {
        derive_piece_mobility(state, piece_index as PieceIndex, square)
    }).collect();

    let mobility_value =
        mobility_values.iter().filter(|&&v| v >= 0.0).sum::<f64>()
        / mobility_values.iter().filter(|&&v| v >= 0.0).count() as f64;

    log_4!(
        "Mobility value for piece '{}': {:.4}",
        piece.char, mobility_value
    );

    1.75 * board_size as f64 * (0.4 * reach_value + 0.6 * mobility_value)
}

fn derive_piece_mobility(
    state: &State, piece_index: PieceIndex, square: usize
) -> f64 {
    let board_size = state.statics.board_size;

    let relevant_moves =
        &state.statics.relevant_moves[piece_index as usize * board_size + square];

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

fn derive_distance_from_center(state: &State, square: usize) -> f64 {
    let center_file = if state.statics.files % 2 == 1 {
        vec![(state.statics.files as f64 / 2.0).floor() as u8]
    } else {
        vec![state.statics.files / 2, (state.statics.files / 2) - 1]
    };
    let center_rank = if state.statics.ranks % 2 == 1 {
        vec![(state.statics.ranks as f64 / 2.0).floor() as u8]
    } else {
        vec![state.statics.ranks / 2, (state.statics.ranks / 2) - 1]
    };

    let mut center_squares = vec![];

    for file in &center_file {
        for rank in &center_rank {
            center_squares.push(*rank * state.statics.files + *file);
        }
    }

    center_squares
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or_else(
            || panic!("Error while getting distance from center squares")
        )
}

fn derive_closest_promotion(
    state: &State, piece_index: PieceIndex, square: usize
) -> f64 {
    let closest_mandatory = set_indices!(
        state.statics.promotion_zones_mandatory[piece_index as usize]
    )
    .iter()
    .map(|&index| square_distance(state, square as Square, index as Square))
    .min_by(|a, b| a.partial_cmp(b).unwrap())
    .unwrap_or(f64::INFINITY);

    let closest_optional = set_indices!(
        state.statics.promotion_zones_optional[piece_index as usize]
    )
    .iter()
    .map(|&index| square_distance(state, square as Square, index as Square))
    .min_by(|a, b| a.partial_cmp(b).unwrap())
    .unwrap_or(f64::INFINITY);

    closest_optional.min(closest_mandatory)
}

fn derive_piece_value_on_square(
    state: &State, piece_index: PieceIndex, square: usize, is_endgame: bool
) -> i32 {
    let mobility = derive_piece_mobility(state, piece_index, square);
    let distance_from_center = derive_distance_from_center(state, square);

    let piece = &state.statics.pieces[piece_index as usize];

    let mobility_weight = if is_endgame { 0.25 } else { 0.5 };
    let center_weight = if is_endgame { 1.75 } else { 1.25 };
    let promotion_weight = if is_endgame { 1.1 } else { 1.6 };

    if p_can_promote!(piece) {
        let closest_promotion =
            derive_closest_promotion(state, piece_index, square);

        let value =
            (mobility_weight * mobility) +
            (
                state.statics.files as f64 / 2.0 - center_weight *
                distance_from_center
            ) +
            (
                state.statics.ranks as f64 - promotion_weight *
                closest_promotion
            );

        if !is_endgame && p_is_royal!(piece) {
            -value.round() as i32
        } else {
            value.round() as i32
        }
    } else {

        let value =
            (mobility_weight * mobility) +
            (
                state.statics.files as f64 / 2.0 - center_weight *
                distance_from_center
            );

        if !is_endgame && p_is_royal!(piece) {
            -value.round() as i32
        } else {
            value.round() as i32
        }
    }
}

fn derive_pst(index: PieceIndex, state: &State, is_endgame: bool) -> Vec<i32> {
    let board_size = state.statics.board_size;

    let pst: Vec<i32> = (0..board_size).into_par_iter().map(|square| {
        derive_piece_value_on_square(state, index, square, is_endgame)
    }).collect();

    pst
}

pub fn derive_parameters(state: &mut State) {
    log_3!("Deriving dynamic evaluation parameters...");

    let values = state.statics.pieces
        .par_iter()
        .filter_map(
            |piece| {
            let color = p_color!(piece);

            if color == BLACK {
                return None;
            }

            let index = p_index!(piece) as usize;
            let value = derive_piece_value(state, piece);

            Some((index, value))
        })
        .collect::<Vec<_>>();

    let offset = values
        .iter()
        .map(|(_, value)| *value)
        .fold(f64::INFINITY, f64::min) - 100.0;

    for (index, value) in values.clone() {
        let black_index = state.statics.piece_swap_map[index] as usize;
        let white_index = index;
        let value = (value - offset).round() as u16;

        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[black_index], value, 0, false, false
        );
        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[white_index], value, 0, false, false
        );
    }

    let piece_roles = derive_piece_roles(state);

    for (piece_index, is_big, is_major) in piece_roles {
        let piece = &mut state.static_mut().pieces[piece_index as usize];
        let value = p_ovalue!(piece);

        set_piece_dynamic_parameters(
            piece, value, 0, is_big, is_major
        );
    }

    for piece in state.static_mut().pieces.iter_mut() {
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
        .filter(
            |(index, _)|
                p_is_big!(&state.statics.pieces[*index]) &&
                !p_is_royal!(&state.statics.pieces[*index]) &&
                p_color!(&state.statics.pieces[*index]) == WHITE
        )
        .map(|(_, value)| value)
        .sum::<f64>() / total_values;

    log_3!(
        "Average piece value (for big non-royal pieces): {:.4}", average_value
    );

    state.static_mut().opening_score =
        average_value.round() as u32 * state.statics.pieces.len() as u32;
    state.static_mut().endgame_score =
        average_value.round() as u32 * 5;

    state.big_pieces = [0; 2];
    state.major_pieces = [0; 2];
    state.minor_pieces = [0; 2];

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let count = state.piece_list[piece_idx].len() as u32;

        state.big_pieces[color] += count * (p_is_big!(piece) as u32);
        state.major_pieces[color] += count * (p_is_major!(piece) as u32);
        state.minor_pieces[color] += count * (p_is_minor!(piece) as u32);
    }

    let pst_entries: Vec<(usize, Vec<i32>, Vec<i32>)> =
        state.statics.pieces.par_iter().map(|piece| {
            let mut index = p_index!(piece);

            if p_color!(piece) == BLACK {
                index = state.statics.piece_swap_map[index as usize];
            }

            let mut opening_pst = derive_pst(index, state, false);
            let mut endgame_pst = derive_pst(index, state, true);

            if p_color!(piece) == BLACK {
                opening_pst = mirror_pst_across_horizontal_axis(
                    &opening_pst,
                    state.statics.files as usize,
                    state.statics.ranks as usize
                );
                endgame_pst = mirror_pst_across_horizontal_axis(
                    &endgame_pst,
                    state.statics.files as usize,
                    state.statics.ranks as usize
                );

                index = state.statics.piece_swap_map[index as usize];
            }

            (index as usize, opening_pst, endgame_pst)
        }).collect();

    for (index, opening_pst, endgame_pst) in pst_entries {
        state.static_mut().pst_opening[index] = opening_pst;
        state.static_mut().pst_endgame[index] = endgame_pst;
    }

    log_3!("Derived Opening Score Threshold: {}", state.statics.opening_score);
    log_3!("Derived Endgame Score Threshold: {}", state.statics.endgame_score);

    let piece_values: Vec<i32> = state.statics.pieces.iter()
        .filter(|p| p_color!(p) == WHITE && !p_is_royal!(p))
        .map(|p| p_ovalue!(p) as i32)
        .collect();

    let avg = if piece_values.is_empty() {
        panic!("No pieces found to derive futility margins from")
    } else {
        piece_values.iter().sum::<i32>() / piece_values.len() as i32
    };

    state.static_mut().futility_margin = [
        [0, 200, avg,         10 * avg / 7, 15 * avg / 7],
        [0, 200, 5 * avg / 7,  9 * avg / 7, 13 * avg / 7],
        [0, 150, 4 * avg / 7,  7 * avg / 7, 10 * avg / 7],
    ];

    let rfp_base = avg / 15;
    state.static_mut().rfp_margin = [0, 1].map(|i| {
        (0..MAX_RFP_DEPTH)
            .map(|depth| rfp_base * depth as i32 * (2 - 2 * i))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    });

    state.static_mut().razor_margin = [
        0,
        avg / 3 + 100,
        avg / 2 + 200,
        avg + 300,
    ];

    let see_base = (avg / 8).max(1);
    state.static_mut().see_margin = (0..MAX_SEE_PRUNE_DEPTH)
        .map(|depth| see_base * depth as i32)
        .collect();

    log_3!(
        "Derived Futility Margins: {:?} | {:?} | {:?}",
        state.statics.futility_margin[0],
        state.statics.futility_margin[1],
        state.statics.futility_margin[2],
    );
    log_3!(
        "Derived RFP Margins: {:?} | {:?}",
        state.statics.rfp_margin[0],
        state.statics.rfp_margin[1],
    );
    log_3!(
        "Derived Razor Margins: {:?}", state.statics.razor_margin,
    );
    log_3!(
        "Derived SEE Capture Margins: {:?}",
        state.statics.see_margin,
    );

    let nmp_min_material = ((state.major_pieces[WHITE as usize]
        + state.major_pieces[BLACK as usize]) / 4).clamp(1, 4);
    state.static_mut().nmp_min_material = nmp_min_material;

    let tempo_bonus = (avg / 20).max(5) as i32;
    state.static_mut().tempo_bonus = tempo_bonus;

    let imbalance_major = (avg / 24).max(3) as i32;
    let imbalance_minor = (avg / 48).max(1) as i32;
    state.static_mut().imbalance_major = imbalance_major;
    state.static_mut().imbalance_minor = imbalance_minor;

    let mut pair_eligible = Vec::new();
    let mut pair_bonus = vec![0i32; state.statics.pieces.len()];

    for (idx, piece) in state.statics.pieces.iter().enumerate() {
        if p_color!(piece) == WHITE && !p_is_royal!(piece) && !p_is_big!(piece) {
            let reach_value = derive_piece_reach(state, piece);
            if (reach_value - 0.5).abs() < 0.02 {
                pair_eligible.push(idx as PieceIndex);
                let bonus = (avg / 8).max(10) as i32;
                pair_bonus[idx] = bonus;
                let black_idx = state.statics.piece_swap_map[idx] as usize;
                pair_bonus[black_idx] = bonus;
            }
        }
    }

    state.static_mut().pair_eligible_indices = pair_eligible;
    state.static_mut().pair_bonus = pair_bonus;

    log_3!("Dynamic evaluation parameters derived successfully.");

    refresh_eval_state(state);
}
