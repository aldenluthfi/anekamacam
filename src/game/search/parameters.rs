//! # parameters.rs
//!
//! Automatic derivation of dynamic evaluation parameters for pieces.
//!
//! A variant-agnostic engine cannot ship hand-tuned material values or
//! piece-square tables: it meets each army for the first time at load. This
//! file closes that gap, turning a piece's movement geometry into the numbers
//! evaluation needs -- a value from its board reach and mobility, a role from
//! where that value ranks in the army, and opening/endgame PSTs shaped to the
//! variant's board -- so every variant is scored on its own terms.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 08/05/2026

use crate::*;

/// PieceRoles
///
/// One derived role assignment: the piece index paired with its is-big
/// and is-major classification flags.
type PieceRoles = (PieceIndex, bool, bool);

/// derive_piece_roles
///
/// Classifies every non-royal piece as big/major/minor by ranking derived
/// values. The roles are determined as follows:
///
/// 1. sort all the piece values, excluding the royal pieces
/// 2. the bottom ceil(10%) are the non-big pieces
/// 3. the top ceil(30%) are the major pieces
/// 4. the rest are the minor pieces
///
/// Params:
/// - state: &mut State -> variant whose derived values are ranked
///
/// Return:
/// Vec<PieceRoles> -> (index, is_big, is_major) per piece
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

/// derive_piece_reach
///
/// Measures how much of the board a piece can eventually cover: from every
/// origin square, flood-fills through the symmetric closure of the piece's
/// move offsets (each offset is also applied reversed, so one-directional
/// movers are not punished twice) and averages the reached fraction.
/// Color-bound pieces like bishops or confined pieces like the xiangqi
/// elephant score well below 1.0.
///
/// Params:
/// - state: &State -> precomputed relevant-move tables
/// - piece: &Piece -> piece whose coverage is measured
///
/// Return:
/// f64 -> mean reachable fraction of the board, in (0, 1]
///
fn derive_piece_reach(state: &State, piece: &Piece) -> f64 {
    let board_size = state.statics.board_size;
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let piece_index = p_index!(piece) as usize;

    let reach_values: Vec<i32> = (0..board_size).into_par_iter().map(|square| {
        let mut reached_squares: HashSet<usize> = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(square);
        reached_squares.insert(square);

        while let Some(current) = queue.pop_front() {
            let start_file = current as i32 % files;
            let start_rank = current as i32 / files;

            let relevant_moves = &state.statics.relevant_moves
                [piece_index * board_size + current];

            for multi_leg_vector in relevant_moves {
                let mut file_offset = 0;
                let mut rank_offset = 0;

                for leg in multi_leg_vector {
                    file_offset += x!(leg) as i32;
                    rank_offset += y!(leg) as i32;
                }

                for (offset_file, offset_rank) in
                    [(file_offset, rank_offset), (-file_offset, -rank_offset)]
                {
                    let next_file = start_file + offset_file;
                    let next_rank = start_rank + offset_rank;

                    if next_file >= 0 && next_file < files
                    && next_rank >= 0 && next_rank < ranks {
                        let next = (next_rank * files + next_file) as usize;

                        if reached_squares.insert(next) {
                            queue.push_back(next);
                        }
                    }
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

    mean / board_size as f64
}

/// Fraction of a piece's distinct move offsets whose reverse offset is also a
/// move offset. Symmetric movers (knight, rook, bishop, queen) score 1.0;
/// one-directional movers (pawn, shogi lance) score near 0.0, capturing the
/// value penalty of being unable to retreat.
///
/// Params:
/// - state: &State -> precomputed relevant-move tables
/// - piece: &Piece -> piece whose offsets are examined
///
/// Return:
/// f64 -> reversible-offset fraction, in [0, 1]
///
fn derive_piece_maneuverability(state: &State, piece: &Piece) -> f64 {
    let board_size = state.statics.board_size;
    let piece_index = p_index!(piece) as usize;

    let mut offsets: HashSet<(i32, i32)> = HashSet::new();

    for square in 0..board_size {
        let relevant_moves = &state.statics.relevant_moves
            [piece_index * board_size + square];

        for multi_leg_vector in relevant_moves {
            let mut file_offset = 0;
            let mut rank_offset = 0;

            for leg in multi_leg_vector {
                file_offset += x!(leg) as i32;
                rank_offset += y!(leg) as i32;
            }

            if (file_offset, rank_offset) != (0, 0) {
                offsets.insert((file_offset, rank_offset));
            }
        }
    }

    if offsets.is_empty() {
        return 1.0;
    }

    let reversible = offsets
        .iter()
        .filter(|(file_offset, rank_offset)| {
            offsets.contains(&(-file_offset, -rank_offset))
        })
        .count();

    reversible as f64 / offsets.len() as f64
}

/// Derives a piece's phase value from its movement geometry:
///
/// - reach:
///   fraction of the board covered by the symmetric closure of the piece's
///   moves, capturing colour-boundedness and confinement.
///
/// - maneuverability:
///   fraction of move offsets whose reverse is also a move, penalising
///   one-directional pieces (pawns) that cannot retreat.
///
/// - mobility:
///   mean per-square move count under a board occupancy model.
///
/// - empty_mobility:
///   assumes an empty board, rewarding long-range sliders.
///
/// - occupied_mobility:
///   weights each slide by `(1 - occupancy)^(legs - 1)`, the odds its path is
///   clear, distinguishing sliders from leapers.
///
/// The value blends the two mobilities, then scales by coverage and
/// maneuverability. A larger `occupancy` (sparse board) yields endgame values,
/// letting sliders gain relative to leapers.
///
/// Params:
/// - state: &State  -> precomputed relevant-move tables
/// - piece: &Piece  -> piece to value
/// - occupancy: f64 -> assumed board fill ratio for the phase
///
/// Return:
/// f64 -> raw phase value, later offset-normalized across the army
///
fn derive_piece_value(state: &State, piece: &Piece, occupancy: f64) -> f64 {
    log_4!("Deriving base value for piece '{}'", piece.char);

    let board_size = state.statics.board_size;
    let piece_index = p_index!(piece);

    let reach = derive_piece_reach(state, piece);
    let maneuverability = derive_piece_maneuverability(state, piece);

    let empty_mobility = (0..board_size).into_par_iter().map(|square| {
        derive_piece_mobility(state, piece_index, square, 0.0)
    }).sum::<f64>() / board_size as f64;

    let occupied_mobility = (0..board_size).into_par_iter().map(|square| {
        derive_piece_mobility(state, piece_index, square, occupancy)
    }).sum::<f64>() / board_size as f64;

    let blended_mobility = 0.3 * empty_mobility
        + (1.0 - 0.3) * occupied_mobility;

    let coverage = 0.6 + (1.0 - 0.6) * reach;
    let maneuver = 0.5 + (1.0 - 0.5) * maneuverability;

    58.0 * blended_mobility * coverage * maneuver
}

/// derive_piece_mobility
///
/// Expected move count from one square under a random-fill occupancy
/// model: each vector contributes `(1 - occupancy)^(legs - 1)`, the
/// probability that all its intermediate stops are empty. Leapers are
/// unaffected by occupancy; long slides decay geometrically.
///
/// Params:
/// - state: &State         -> precomputed relevant-move tables
/// - piece_index: PieceIndex -> piece whose vectors are counted
/// - square: usize         -> origin square
/// - occupancy: f64        -> assumed board fill ratio
///
/// Return:
/// f64 -> expected number of playable vectors from the square
///
fn derive_piece_mobility(
    state: &State, piece_index: PieceIndex, square: usize, occupancy: f64
) -> f64 {
    let board_size = state.statics.board_size;

    let relevant_moves = &state.statics.relevant_moves
        [piece_index as usize * board_size + square];

    let mut mobility_sum = 0.0;

    for multi_leg_vector in relevant_moves {
        let intermediate = multi_leg_vector.len() as i32 - 1;
        mobility_sum += (1.0 - occupancy).powi(intermediate);
    }

    mobility_sum
}

/// derive_distance_from_center
///
/// Measures the euclidean distance from a square to the nearest of the (up
/// to four) central squares, handling even and odd board dimensions.
///
/// Params:
/// - state: &State -> board dimensions for locating the center
/// - square: usize -> square whose distance is measured
///
/// Return:
/// f64 -> distance in square units
///
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

/// derive_closest_promotion
///
/// Measures the distance to the nearest square of the piece's promotion
/// zones, mandatory or optional, returning infinity when the piece has
/// none.
///
/// Params:
/// - state: &State           -> board dimensions and promotion zones
/// - piece_index: PieceIndex -> piece whose promotion zones are queried
/// - square: usize           -> square whose distance is measured
///
/// Return:
/// f64 -> distance in square units, infinity when no zone exists
///
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

/// derive_square_score
///
/// Raw positional desirability of one square for one piece: mobility
/// from the square minus its distance from the center, with the weights
/// shifted by phase (mobility matters more in the opening, centrality
/// more in the endgame).
///
/// Params:
/// - state: &State           -> precomputed relevant-move tables
/// - piece_index: PieceIndex -> piece being placed
/// - square: usize           -> square being scored
/// - is_endgame: bool        -> selects phase occupancy and weights
///
/// Return:
/// f64 -> unnormalized square score, later scaled into the PST
///
fn derive_square_score(
    state: &State, piece_index: PieceIndex, square: usize, is_endgame: bool
) -> f64 {
    let occupancy = if is_endgame {
        ENDGAME_OCCUPANCY
    } else {
        OPENING_OCCUPANCY
    };

    let mobility =
        derive_piece_mobility(state, piece_index, square, occupancy);
    let distance_from_center = derive_distance_from_center(state, square);

    let mobility_weight = if is_endgame { 0.25 } else { 0.5 };
    let center_weight = if is_endgame { 1.75 } else { 1.25 };

    mobility_weight * mobility - center_weight * distance_from_center
}

/// derive_promotion_bonus
///
/// Advancement bonus for a promotable piece: a linear gradient toward the
/// promotion zone, scaled by the value it would gain on promotion. This is
/// where an advanced passed pawn earns most of its endgame worth.
///
/// Params:
/// - state: &State           -> board dimensions and promotion zones
/// - piece_index: PieceIndex -> piece being placed
/// - square: usize           -> square being scored
/// - is_endgame: bool        -> selects the phase's bonus fraction
/// - piece_value: f64        -> the piece's current phase value
/// - promoted_value: f64     -> best value reachable by promotion
///
/// Return:
/// f64 -> bonus added on top of the positional square score
///
fn derive_promotion_bonus(
    state: &State, piece_index: PieceIndex, square: usize,
    is_endgame: bool, piece_value: f64, promoted_value: f64
) -> f64 {
    let piece = &state.statics.pieces[piece_index as usize];

    if !p_can_promote!(piece) {
        return 0.0;
    }

    let closest_promotion =
        derive_closest_promotion(state, piece_index, square);
    let advancement =
        (1.0 - closest_promotion / state.statics.ranks as f64).max(0.0);

    let fraction = if is_endgame {
        0.40
    } else {
        0.06
    };

    fraction * (promoted_value - piece_value).max(0.0)
        * advancement.powf(2.0)
}

/// derive_pst
///
/// Builds one piece-square table: raw square scores are centered on
/// their mean and normalized to a fixed amplitude, then the promotion
/// gradient is added. For a compact board the positional part comes out
/// center-positive and edge-negative, e.g.:
///
/// ```text
/// ┌────┬────┬────┬────┐
/// │ -9 │ -4 │ -4 │ -9 │
/// ├────┼────┼────┼────┤
/// │ -4 │ +6 │ +6 │ -4 │
/// ├────┼────┼────┼────┤
/// │ -9 │ -4 │ -4 │ -9 │
/// └────┴────┴────┴────┘
/// ```
///
/// Royal pieces get the sign flipped in the opening (the king should
/// hide, not centralize) but keep it in the endgame.
///
/// Params:
/// - index: PieceIndex   -> piece the table is built for
/// - state: &State       -> precomputed relevant-move tables
/// - is_endgame: bool    -> selects phase weights and values
/// - promoted_value: f64 -> best value reachable by promotion
///
/// Return:
/// Vec<i32> -> per-square bonus table in board order
///
fn derive_pst(
    index: PieceIndex, state: &State, is_endgame: bool, promoted_value: f64
) -> Vec<i32> {
    let board_size = state.statics.board_size;
    let piece = &state.statics.pieces[index as usize];

    let piece_value = if is_endgame {
        p_evalue!(piece) as f64
    } else {
        p_ovalue!(piece) as f64
    };
    let royal_sign =
        if !is_endgame && p_is_royal!(piece) { -1.0 } else { 1.0 };

    let scores: Vec<f64> = (0..board_size).into_par_iter().map(|square| {
        derive_square_score(state, index, square, is_endgame)
    }).collect();

    let mean = scores.iter().sum::<f64>() / board_size as f64;
    let max_deviation = scores
        .iter()
        .map(|score| (score - mean).abs())
        .fold(0.0_f64, f64::max)
        .max(1.0);
    let amplitude = 24.0;

    (0..board_size).map(|square| {
        let positional =
            royal_sign * (scores[square] - mean) / max_deviation * amplitude;
        let promotion = derive_promotion_bonus(
            state, index, square, is_endgame, piece_value, promoted_value
        );

        (positional + promotion).round() as i32
    }).collect()
}

/// derive_parameters
///
/// Startup entry point for the whole derivation pass: computes the
/// evaluation parameters, then the search margins built on top of them,
/// and finally refreshes the incremental eval caches.
///
/// Params:
/// - state: &mut State -> freshly precomputed variant state
///
pub fn derive_parameters(state: &mut State) {
    derive_eval_parameters(state);
    derive_search_parameters(state);
    refresh_eval_state(state);
}

/// derive_eval_parameters
///
/// Drives the evaluation half of parameter derivation: values every
/// white piece for both phases (black twins copy them via the swap map),
/// normalizes against the cheapest piece, assigns big/major/minor roles,
/// derives the opening/endgame phase thresholds from average material,
/// and builds all piece-square tables (black tables are the white ones
/// mirrored across the horizontal axis).
///
/// Params:
/// - state: &mut State -> variant whose dynamic parameters are filled
///
pub fn derive_eval_parameters(state: &mut State) {
    log_3!("Deriving dynamic evaluation parameters...");

    let values = state.statics.pieces
        .par_iter()
        .filter_map(|piece| {
            if p_color!(piece) == BLACK {
                return None;
            }

            let index = p_index!(piece) as usize;
            let opening = derive_piece_value(state, piece, OPENING_OCCUPANCY);
            let endgame = derive_piece_value(state, piece, ENDGAME_OCCUPANCY);

            Some((index, opening, endgame))
        })
        .collect::<Vec<_>>();

    let offset = values
        .iter()
        .map(|(_, opening, _)| *opening)
        .fold(f64::INFINITY, f64::min) - 100.0;

    for (index, opening, endgame) in values.clone() {
        let black_index = state.statics.piece_swap_map[index] as usize;
        let white_index = index;
        let ovalue = (opening - offset).round() as u16;
        let evalue = (endgame - offset).round() as u16;

        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[black_index],
            ovalue, evalue, false, false
        );
        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[white_index],
            ovalue, evalue, false, false
        );
    }

    let piece_roles = derive_piece_roles(state);

    for (piece_index, is_big, is_major) in piece_roles {
        let piece = &mut state.static_mut().pieces[piece_index as usize];
        let ovalue = p_ovalue!(piece);
        let evalue = p_evalue!(piece);

        set_piece_dynamic_parameters(
            piece, ovalue, evalue, is_big, is_major
        );
    }

    let total_values = values.len() as f64;
    let average_value = values
        .into_iter()
        .filter(
            |(index, _, _)|
                p_is_big!(&state.statics.pieces[*index]) &&
                !p_is_royal!(&state.statics.pieces[*index]) &&
                p_color!(&state.statics.pieces[*index]) == WHITE
        )
        .map(|(_, opening, _)| opening)
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

    let promoted_opening = state.statics.pieces.iter()
        .filter(|p| p_color!(p) == WHITE && !p_is_royal!(p))
        .map(|p| p_ovalue!(p) as f64)
        .fold(0.0_f64, f64::max);
    let promoted_endgame = state.statics.pieces.iter()
        .filter(|p| p_color!(p) == WHITE && !p_is_royal!(p))
        .map(|p| p_evalue!(p) as f64)
        .fold(0.0_f64, f64::max);

    let pst_entries: Vec<(usize, Vec<i32>, Vec<i32>)> =
        state.statics.pieces.par_iter().map(|piece| {
            let mut index = p_index!(piece);

            if p_color!(piece) == BLACK {
                index = state.statics.piece_swap_map[index as usize];
            }

            let mut opening_pst =
                derive_pst(index, state, false, promoted_opening);
            let mut endgame_pst =
                derive_pst(index, state, true, promoted_endgame);

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
}

/// derive_pawn_like
///
/// Classifies which pieces are pawn-like, so the pawn-structure evaluation
/// knows which pieces to score. A piece is pawn-like when all of these hold:
///
/// - it never steps or captures backward
/// - it always keeps a quiet single-square forward step available
/// - its non-initial quiet moves stay within one square
/// - at least `PAWN_MIN_START_COUNT` copies stand on the board (or wait in
///   hand) at the start
///
/// These purely geometric and count conditions select the pawn/soldier of
/// any variant:
///
/// - the single-step rule and one-square bound exclude leapers like the
///   shogi knight and forward sliders like the lance
/// - the no-retreat rule excludes gold / silver / advisor / elephant
/// - the starting count excludes sparse forward steppers such as the lone
///   mini-shogi pawn or the two mini-xiangqi soldiers
///
/// Longer initial pushes (a two- to four-square first move) are exempt from
/// the one-square bound. The result is recorded as static bit 19 on each
/// piece and its colour twin, which is what `p_is_pawn!` reads.
///
/// Params:
/// - state: &mut State -> variant whose pieces are classified
///
fn derive_pawn_like(state: &mut State) {
    let board_size = state.statics.board_size;

    let mut marked: Vec<usize> = Vec::new();

    for piece in state.statics.pieces.iter() {
        if p_color!(piece) != WHITE || p_is_royal!(piece) {
            continue;
        }

        let index = p_index!(piece) as usize;
        let color = p_color!(piece) as usize;
        let mut steps_backward = false;
        let mut has_single_step = false;
        let mut ranges_far = false;

        for square in 0..board_size {
            let moves =
                &state.statics.relevant_moves[index * board_size + square];

            for vector in moves {
                let (file_offset, rank_offset) = vector_offset!(vector);
                let quiet = vector_moves_quietly!(vector);

                if rank_offset < 0 {
                    steps_backward = true;
                }
                if quiet && rank_offset == 1 && file_offset.abs() <= 1 {
                    has_single_step = true;
                }
                if quiet && !vector_is_initial!(vector)
                && (rank_offset > 1 || file_offset.abs() > 1) {
                    ranges_far = true;
                }
            }
        }

        let start_count =
            count_bits!(state.statics.initial_setup[index]) as usize
            + state.piece_in_hand[color][index] as usize;

        if !steps_backward && has_single_step && !ranges_far
        && start_count >= PAWN_MIN_START_COUNT {
            marked.push(index);
        }
    }

    for index in marked {
        let swapped = state.statics.piece_swap_map[index] as usize;
        state.static_mut().pieces[index].encoded_static |= 1 << 19;
        if swapped != NO_PIECE as usize {
            state.static_mut().pieces[swapped].encoded_static |= 1 << 19;
        }
    }
}

/// Closure of quiet-move landing squares reachable from `square`, i.e. the set
/// of squares the pawn can traverse toward its promotion zone. A friendly pawn
/// standing on any of these squares is doubled, since it blocks this pawn's
/// advance -- this is the `doubled` term's mask.
///
/// A straight mover (FIDE) traces its own file ahead; a diagonal mover
/// (Berolina) fans out across files, so `##` follows the move geometry rather
/// than assuming the same file.
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │    │ ## │    │    │   ## = a square on the forward path
/// ├────┼────┼────┼────┼────┤
/// │    │    │ ## │    │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ ## │    │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ PP │    │    │   PP = the pawn (FIDE, straight pushes)
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - state: &State -> precomputed relevant-move tables
/// - index: usize  -> pawn-like piece index
/// - square: usize -> square the pawn stands on
///
/// Return:
/// Board -> bitboard of squares on the pawn's forward path
///
fn derive_pawn_path(state: &State, index: usize, square: usize) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut path = board!(state.statics.files, state.statics.ranks);
    let mut queue = VecDeque::new();
    queue.push_back(square);

    while let Some(current) = queue.pop_front() {
        let current_file = current as i32 % files;
        let current_rank = current as i32 / files;

        let moves =
            &state.statics.relevant_moves[index * board_size + current];

        for vector in moves {
            if !vector_moves_quietly!(vector) {
                continue;
            }

            let (file_offset, rank_offset) = vector_offset!(vector);
            let next_file = current_file + file_offset * sign;
            let next_rank = current_rank + rank_offset * sign;

            if next_file < 0 || next_file >= files
            || next_rank < 0 || next_rank >= ranks {
                continue;
            }

            let next = (next_rank * files + next_file) as usize;
            if !get!(path, next as u32) {
                set!(path, next as u32);
                queue.push_back(next);
            }
        }
    }

    path
}

/// Enemy source squares from which a pawn-like piece could stop this pawn: any
/// square on its path (a blocker) plus any square whose capture reaches the
/// path or the pawn itself. A pawn is passed when none of these are occupied --
/// this is the `passed` term's mask.
///
/// For a FIDE white pawn: `##` are the path blockers on its own file, and `xx`
/// the adjacent-file squares ahead from where an enemy pawn could capture into
/// the path.
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │ xx │ ## │ xx │    │   ## = a blocker on the path
/// ├────┼────┼────┼────┼────┤
/// │    │ xx │ ## │ xx │    │   xx = an enemy capturing into the path
/// ├────┼────┼────┼────┼────┤
/// │    │ xx │ ## │ xx │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ PP │    │    │   PP = the pawn
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - state: &State -> precomputed relevant-capture tables
/// - index: usize  -> pawn-like piece index
/// - square: usize -> square the pawn stands on
/// - path: &Board  -> the pawn's forward path mask
///
/// Return:
/// Board -> bitboard of enemy squares that stop the passer
///
fn derive_pawn_interference(
    state: &State, index: usize, square: usize, path: &Board
) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let enemy = 1 - p_color!(&state.statics.pieces[index]);

    let mut mask = *path;

    for enemy_index in 0..state.statics.pieces.len() {
        if !p_is_pawn!(&state.statics.pieces[enemy_index])
        || p_color!(&state.statics.pieces[enemy_index]) != enemy {
            continue;
        }

        let sign = -2 * enemy as i32 + 1;

        for source in 0..board_size {
            let source_file = source as i32 % files;
            let source_rank = source as i32 / files;

            let captures = &state.statics.relevant_captures
                [enemy_index * board_size + source];

            for vector in captures {
                let (file_offset, rank_offset) = vector_offset!(vector);
                let target_file = source_file + file_offset * sign;
                let target_rank = source_rank + rank_offset * sign;

                if target_file < 0 || target_file >= files
                || target_rank < 0 || target_rank >= ranks {
                    continue;
                }

                let target = (target_rank * files + target_file) as usize;
                if target == square || get!(path, target as u32) {
                    set!(mask, source as u32);
                }
            }
        }
    }

    mask
}

/// Source squares from which a pawn-like piece of `color` captures onto any
/// square set in `targets`. Capture legs are stored in the white frame, so each
/// net offset is mirrored through the colour sign before it is applied. Shared
/// by the support, backward, and passer geometry below.
///
/// `SS` are the returned sources, `TT` a target; a `color` pawn on `SS` can
/// capture onto `TT`. The `connected` term passes `TT` = pawn square + stops
/// (own colour); the `backward` term passes the enemy colour with `TT` = the
/// pawn's stop square. Diagonal capture is shown; a straight capturer sits
/// directly below `TT` instead.
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │    │ TT │    │    │   TT = a requested target
/// ├────┼────┼────┼────┼────┤
/// │    │ SS │    │ SS │    │   SS = a returned capturing source
/// ├────┼────┼────┼────┼────┤
/// │    │    │    │    │    │
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - state: &State   -> precomputed relevant-capture tables
/// - color: u8       -> colour whose pawn-like captures are gathered
/// - targets: &Board -> squares a capture must land on
///
/// Return:
/// Board -> bitboard of capturing source squares
///
fn pawn_capture_sources(state: &State, color: u8, targets: &Board) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * color as i32 + 1;

    let mut mask = board!(state.statics.files, state.statics.ranks);

    for index in 0..state.statics.pieces.len() {
        if !p_is_pawn!(&state.statics.pieces[index])
        || p_color!(&state.statics.pieces[index]) != color {
            continue;
        }

        for source in 0..board_size {
            let source_file = source as i32 % files;
            let source_rank = source as i32 / files;

            let captures = &state.statics.relevant_captures
                [index * board_size + source];

            for vector in captures {
                let (file_offset, rank_offset) = vector_offset!(vector);
                let target_file = source_file + file_offset * sign;
                let target_rank = source_rank + rank_offset * sign;

                if target_file < 0 || target_file >= files
                || target_rank < 0 || target_rank >= ranks {
                    continue;
                }

                let target = (target_rank * files + target_file) as usize;
                if get!(targets, target as u32) {
                    set!(mask, source as u32);
                }
            }
        }
    }

    mask
}

/// Friendly source squares that defend this pawn: any friendly pawn-like
/// capture landing on the pawn itself or on the square it advances to. A pawn
/// is connected when one of these squares holds a friendly pawn-like piece; the
/// stop-square reach generalises the phalanx without assuming diagonal capture.
///
/// The pawn advances up the page: `PP` the pawn, `**` a square it pushes to,
/// `SS` a bit in the mask (a friendly pawn on `SS` connects `PP`). This is the
/// `connected` term's mask; the `backward` term fires only when it is empty.
///
/// FIDE keeps the diagonal defenders below plus the phalanx beside `PP`:
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │    │ ** │    │    │   ** = the stop square
/// ├────┼────┼────┼────┼────┤
/// │    │ SS │ PP │ SS │    │   SS beside PP guard the stop (phalanx)
/// ├────┼────┼────┼────┼────┤
/// │    │ SS │    │ SS │    │   SS below guard PP directly
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Berolina captures straight, so supporters share `PP`'s file or a
/// neighbour, never the same rank:
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │ ** │    │ ** │    │   two diagonal stops
/// ├────┼────┼────┼────┼────┤
/// │    │ SS │ PP │ SS │    │   SS guard the stops from straight below
/// ├────┼────┼────┼────┼────┤
/// │    │    │ SS │    │    │   SS guards PP from straight below
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Shogi moves and captures straight, so the only supporter is directly
/// behind and adjacent files never defend:
///
/// ```text
/// ┌────┬────┬────┐
/// │    │ ** │    │   the stop, guarded only by PP itself
/// ├────┼────┼────┤
/// │    │ PP │    │
/// ├────┼────┼────┤
/// │    │ SS │    │   the sole supporter, directly behind
/// └────┴────┴────┘
/// ```
///
/// Params:
/// - state: &State -> precomputed relevant-capture tables
/// - index: usize  -> pawn-like piece index
/// - square: usize -> square the pawn stands on
/// - stop: &Board  -> the pawn's immediate forward stop squares
///
/// Return:
/// Board -> bitboard of friendly squares that connect the pawn
///
fn derive_pawn_support(
    state: &State, index: usize, square: usize, stop: &Board
) -> Board {
    let own = p_color!(&state.statics.pieces[index]);

    let mut targets = *stop;
    set!(targets, square as u32);

    pawn_capture_sources(state, own, &targets)
}

/// Immediate quiet, non-initial, strictly-forward landing square(s) of a pawn:
/// the squares it reaches in a single step. Initial multi-square pushes are
/// excluded so second-rank support matches the one-square frame, and sideways
/// steps are dropped so only genuine advances count.
///
/// `PP` the pawn, `**` a stop. A straight mover has one stop; a diagonal mover
/// (Berolina) has two. These stops feed the `connected` and `backward` masks.
///
/// Straight mover (FIDE), one stop:
///
/// ```text
/// ┌────┬────┬────┐
/// │    │ ** │    │
/// ├────┼────┼────┤
/// │    │ PP │    │
/// └────┴────┴────┘
/// ```
///
/// Diagonal mover (Berolina), two stops:
///
/// ```text
/// ┌────┬────┬────┐
/// │ ** │    │ ** │
/// ├────┼────┼────┤
/// │    │ PP │    │
/// └────┴────┴────┘
/// ```
///
/// Params:
/// - state: &State -> precomputed relevant-move tables
/// - index: usize  -> pawn-like piece index
/// - square: usize -> square the pawn stands on
///
/// Return:
/// Board -> bitboard of the pawn's immediate forward stop squares
///
fn derive_pawn_stop(state: &State, index: usize, square: usize) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut stop = board!(state.statics.files, state.statics.ranks);

    let source_file = square as i32 % files;
    let source_rank = square as i32 / files;

    let moves = &state.statics.relevant_moves[index * board_size + square];

    for vector in moves {
        if !vector_moves_quietly!(vector) || vector_is_initial!(vector) {
            continue;
        }

        let (file_offset, rank_offset) = vector_offset!(vector);
        if rank_offset < 1 {
            continue;
        }

        let target_file = source_file + file_offset * sign;
        let target_rank = source_rank + rank_offset * sign;

        if target_file < 0 || target_file >= files
        || target_rank < 0 || target_rank >= ranks {
            continue;
        }

        let target = (target_rank * files + target_file) as usize;
        set!(stop, target as u32);
    }

    stop
}

/// File offsets at which a friendly pawn-like piece can defend this pawn or the
/// square it advances to. A pawn with no friendly pawn on any of these relative
/// files is isolated. Each capture leg yields its direct-defence offset and,
/// combined with each forward move leg, a phalanx offset.
///
/// Which files (relative to the pawn `PP`) may ever host a supporter, marked
/// `oo`. This drives the `isolated` term: a pawn with no friendly pawn on any
/// of these files (at any rank) is isolated.
///
/// - FIDE      : {-1,+1},
/// - Berolina  : {-1,0,+1}
/// - Shogi     : {0}
///
/// ```text
///       FIDE               Berolina               Shogi
/// ┌────┬────┬────┐     ┌────┬────┬────┐     ┌────┬────┬────┐
/// │ oo │    │ oo │     │ oo │ oo │ oo │     │    │ oo │    │
/// ├────┼────┼────┤     ├────┼────┼────┤     ├────┼────┼────┤
/// │    │ PP │    │     │    │ PP │    │     │    │ PP │    │
/// └────┴────┴────┘     └────┴────┴────┘     └────┴────┴────┘
/// ```
///
/// Params:
/// - state: &State -> precomputed relevant move and capture tables
/// - index: usize  -> pawn-like piece index
///
/// Return:
/// Vec<i32> -> sorted, de-duplicated supporting file offsets
///
fn derive_pawn_support_offsets(state: &State, index: usize) -> Vec<i32> {
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut capture_files: Vec<i32> = Vec::new();
    let mut move_files: Vec<i32> = Vec::new();

    for square in 0..board_size {
        let captures =
            &state.statics.relevant_captures[index * board_size + square];
        for vector in captures {
            let capture_file = vector_offset!(vector).0;
            if !capture_files.contains(&capture_file) {
                capture_files.push(capture_file);
            }
        }

        let moves = &state.statics.relevant_moves[index * board_size + square];
        for vector in moves {
            if !vector_moves_quietly!(vector) || vector_is_initial!(vector) {
                continue;
            }
            let (move_file, rank_offset) = vector_offset!(vector);
            if rank_offset < 1 {
                continue;
            }
            if !move_files.contains(&move_file) {
                move_files.push(move_file);
            }
        }
    }

    let mut offsets: Vec<i32> = Vec::new();
    for &capture_file in &capture_files {
        let direct = -capture_file * sign;
        if !offsets.contains(&direct) {
            offsets.push(direct);
        }
        for &move_file in &move_files {
            let phalanx = (move_file - capture_file) * sign;
            if !offsets.contains(&phalanx) {
                offsets.push(phalanx);
            }
        }
    }

    offsets.sort();
    offsets
}

/// Fixed-point advancement (`adv^2 * 256`) toward promotion, mirroring the PST
/// promotion gradient. Pieces without a promotion zone fall back to the rank
/// distance from the far edge in their forward direction.
///
/// Params:
/// - state: &State -> board geometry and promotion zones
/// - index: usize  -> pawn-like piece index
/// - square: usize -> square the pawn stands on
///
/// Return:
/// i32 -> fixed-point advancement, 0..=256
///
fn derive_pawn_advancement(state: &State, index: usize, square: usize) -> i32 {
    let ranks = state.statics.ranks as f64;
    let files = state.statics.files as i32;
    let closest = derive_closest_promotion(state, index as PieceIndex, square);

    let advancement = if closest.is_finite() {
        (1.0 - closest / ranks).max(0.0)
    } else {
        let target_rank = if p_color!(&state.statics.pieces[index]) == WHITE {
            state.statics.ranks as i32 - 1
        } else {
            0
        };
        let square_rank = square as i32 / files;
        let span = (state.statics.ranks as i32 - 1).max(1);
        (1.0 - (target_rank - square_rank).abs() as f64 / span as f64).max(0.0)
    };

    (advancement * advancement * 256.0) as i32
}

/// derive_pawn_parameters
///
/// Derives pawn-like classification and the precomputed passed/connected masks
/// and advancement gradient consumed by the pawn-structure evaluation term.
///
/// Params:
/// - state: &mut State -> variant whose pawn tables are filled
///
pub fn derive_pawn_parameters(state: &mut State) {
    let board_size = state.statics.board_size;
    let piece_count = state.statics.pieces.len();

    derive_pawn_like(state);

    let empty = board!(state.statics.files, state.statics.ranks);
    let mut path_mask = vec![empty; board_size * piece_count];
    let mut interference_mask = vec![empty; board_size * piece_count];
    let mut support_mask = vec![empty; board_size * piece_count];
    let mut advancement = vec![0i32; board_size * piece_count];
    let mut backward_mask = vec![empty; board_size * piece_count];
    let mut support_offsets: Vec<Vec<i32>> = vec![Vec::new(); piece_count];

    for (index, piece) in support_offsets.iter_mut().enumerate() {
        if !p_is_pawn!(&state.statics.pieces[index]) {
            continue;
        }

        *piece = derive_pawn_support_offsets(state, index);
        let enemy = 1 - p_color!(&state.statics.pieces[index]);

        for square in 0..board_size {
            let entry = index * board_size + square;
            let path = derive_pawn_path(state, index, square);
            let stop = derive_pawn_stop(state, index, square);

            support_mask[entry] =
                derive_pawn_support(state, index, square, &stop);
            backward_mask[entry] =
                pawn_capture_sources(state, enemy, &stop);
            interference_mask[entry] =
                derive_pawn_interference(state, index, square, &path);
            advancement[entry] =
                derive_pawn_advancement(state, index, square);
            path_mask[entry] = path;
        }
    }

    let opening_values: Vec<i32> = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_ovalue!(piece) as i32)
        .collect();
    let avg = opening_values.iter().sum::<i32>()
        / opening_values.len().max(1) as i32;
    let promoted_opening = opening_values.iter().copied().max().unwrap_or(0);
    let promoted_endgame = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_evalue!(piece) as i32)
        .max()
        .unwrap_or(0);

    let mut passed_opening = vec![0i32; board_size * piece_count];
    let mut passed_endgame = vec![0i32; board_size * piece_count];
    let mut passed_support_opening = vec![0i32; board_size * piece_count];
    let mut passed_support_endgame = vec![0i32; board_size * piece_count];

    for index in 0..piece_count {
        if !p_is_pawn!(&state.statics.pieces[index]) {
            continue;
        }

        let piece = &state.statics.pieces[index];
        let gain_opening = if p_can_promote!(piece) {
            (promoted_opening - p_ovalue!(piece) as i32).max(0)
        } else {
            avg
        };
        let gain_endgame = if p_can_promote!(piece) {
            (promoted_endgame - p_evalue!(piece) as i32).max(0)
        } else {
            avg
        };

        for square in 0..board_size {
            let entry = index * board_size + square;
            let scale = advancement[entry] as f64 / 256.0;

            passed_opening[entry] =
                (0.10 * gain_opening as f64 * scale).round() as i32;
            passed_endgame[entry] =
                (0.35 * gain_endgame as f64 * scale).round() as i32;
            passed_support_opening[entry] = passed_opening[entry] / 2;
            passed_support_endgame[entry] = passed_endgame[entry] / 2;
        }
    }

    let names: Vec<char> = state.statics.pieces.iter()
        .filter(|piece| p_is_pawn!(piece) && p_color!(piece) == WHITE)
        .map(|piece| piece.char)
        .collect();
    log_3!("Derived pawn-like pieces: {:?}", names);

    state.static_mut().pawn_path_mask = path_mask;
    state.static_mut().pawn_interference_mask = interference_mask;
    state.static_mut().pawn_support_mask = support_mask;
    state.static_mut().pawn_advancement = advancement;
    state.static_mut().pawn_backward_mask = backward_mask;
    state.static_mut().pawn_support_offsets = support_offsets;
    state.static_mut().pawn_passed_opening = passed_opening;
    state.static_mut().pawn_passed_endgame = passed_endgame;
    state.static_mut().pawn_passed_support_opening = passed_support_opening;
    state.static_mut().pawn_passed_support_endgame = passed_support_endgame;
    state.static_mut().pawn_connected_opening = avg / 20;
    state.static_mut().pawn_connected_endgame = avg / 12;
    state.static_mut().pawn_doubled_penalty = avg / 16;
    state.static_mut().pawn_isolated_penalty = avg / 16;
    state.static_mut().pawn_backward_penalty = avg / 24;
}

/// derive_search_parameters
///
/// Scales every search margin to the variant's material: futility,
/// reverse-futility, razoring, SEE-pruning, and delta margins, the null-
/// move zugzwang guard, tempo bonus, imbalance weights, and pair bonuses
/// are all expressed as fractions of the average derived piece value, so
/// pruning aggressiveness stays comparable across variants whose value
/// scales differ wildly.
///
/// Params:
/// - state: &mut State -> variant whose search margins are filled
///
pub fn derive_search_parameters(state: &mut State) {
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

    state.static_mut().delta_margin = (avg / 3).max(200);
    state.static_mut().aspiration_delta = (avg / 12).clamp(25, 80);

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

    let mut pair_bonus = vec![0i32; state.statics.pieces.len()];

    for (idx, piece) in state.statics.pieces.iter().enumerate() {
        if p_color!(piece) == WHITE
        && !p_is_royal!(piece)
        && !p_is_big!(piece) {
            let reach_value = derive_piece_reach(state, piece);
            if (reach_value - 0.5).abs() < 0.02 {
                let bonus = (avg / 8).max(10) as i32;
                pair_bonus[idx] = bonus;
                let black_idx = state.statics.piece_swap_map[idx] as usize;
                pair_bonus[black_idx] = bonus;
            }
        }
    }

    state.static_mut().pair_bonus = pair_bonus;

    derive_pawn_parameters(state);

    log_3!("Dynamic search parameters derived successfully.");
}
