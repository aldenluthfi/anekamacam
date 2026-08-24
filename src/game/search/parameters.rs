//! parameters.rs
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
//! Created: 08/05/2026
//! Author : Alden Luthfi

use crate::*;

/// Scale the fractional derivation coefficients are stored against, so
/// they survive a round trip through the all-integer parameter payload.
pub const COEFFICIENT_SCALE: f64 = 1000.0;

/// Board occupancy assumed when valuing a piece: the fraction of squares
/// a slider expects to find blocked in each phase, which is what makes an
/// opening value differ from an endgame one.
pub const OPENING_OCCUPANCY: u32 = 360;
pub const ENDGAME_OCCUPANCY: u32 = 120;

/// Where the ranked non-royal army is cut into roles: the cheapest share
/// that is not big, and the dearest share that is major.
pub const ROLE_NON_BIG_SPLIT: u32 = 100;
pub const ROLE_MAJOR_SPLIT: u32 = 200;

/// How small the big non-royal army has to get before play counts as an
/// endgame, measured in pieces of average deployed value.
pub const ENDGAME_ARMY_SIZE: u32 = 5;

/// Late-move reduction curves, one per class of move. Each surface is
/// `base + shape(depth, moves) / divisor`, and only the base and the
/// divisor are stored: which terms a curve mixes is fixed by the class,
/// because a quiet move buried in a long list and a capture that answers
/// a check do not respond to the same variable. Both are held against
/// `COEFFICIENT_SCALE` so they survive the all-integer payload.
pub const REDUCTION_QUIET_BASE: u32 = 750;
pub const REDUCTION_QUIET_DIVISOR: u32 = 2250;
pub const REDUCTION_QUIET_CHECK_BASE: u32 = 1000;
pub const REDUCTION_QUIET_CHECK_DIVISOR: u32 = 4000;
pub const REDUCTION_TACTICAL_BASE: u32 = 1000;
pub const REDUCTION_TACTICAL_DIVISOR: u32 = 4000;
pub const REDUCTION_TACTICAL_CHECK_BASE: u32 = 0;
pub const REDUCTION_TACTICAL_CHECK_DIVISOR: u32 = 4500;

/// Where reductions begin: the shallowest depth that may give up plies,
/// and the move-count gate `base + wide * wide_window` a move has to pass
/// before its curve applies at all. The gate is wider on a full window
/// because the moves it orders first have not yet been priced against a
/// bound worth trusting.
pub const REDUCTION_MINIMUM_DEPTH: u32 = 3;
pub const REDUCTION_MOVE_BASE: u32 = 2;
pub const REDUCTION_MOVE_WIDE: u32 = 2;

/// How many move slots each surface stores. A node ordering more moves
/// than this reuses the last slot: every curve has flattened well before
/// it, so further rows would repeat what the table already says.
pub const REDUCTION_MOVE_CAP: usize = 64;

/// The window the root reopens around the previous completed score: a
/// fraction of the dearest piece, that being the top of this variant's
/// score range and so the scale one iteration's swing away from the last
/// is drawn against. Only the side that failed widens, by `WIDEN` each
/// time, until it passes `CLAMP` times the width it opened at; past that
/// the root reopens fully instead of widening again. Every one of the
/// three is held against `COEFFICIENT_SCALE`.
pub const ASPIRATION_RATIO: u32 = 30;
pub const ASPIRATION_CLAMP: u32 = 16000;
pub const ASPIRATION_WIDEN: u32 = 2000;

/// The shallowest iteration allowed to narrow its window. Below it no
/// completed score exists that is worth centring one on.
pub const ASPIRATION_START_DEPTH: u32 = 4;

/// The cushion a node has to clear before its static evaluation alone is
/// trusted to beat beta: `RATIO` of the dearest non-royal piece per ply
/// still to search, the same anchor the aspiration window is priced off.
/// A side already standing better than it did two plies ago is believed
/// on less, so its row is the flat one scaled by `IMPROVING`. Both are
/// held against `COEFFICIENT_SCALE`.
pub const RFP_RATIO: u32 = 110;
pub const RFP_IMPROVING: u32 = 750;

/// The deepest node allowed to cut that way. Past it a static score has
/// too much search left under it to stand in for one.
pub const RFP_DEPTH: u32 = 6;

/// How far under alpha a node may stand and still search its late quiet
/// moves: `FLOOR` of the dearest non-royal piece, plus `RATIO` of it for
/// every ply still to search. A quiet move promises nothing immediate, so
/// a node further under alpha than that has none left worth ordering. The
/// side whose evaluation has not risen is believed least and so is given
/// the smaller margin, the risen side's row scaled by `IMPROVING`. All
/// three are held against `COEFFICIENT_SCALE`.
pub const FUTILITY_FLOOR: u32 = 100;
pub const FUTILITY_RATIO: u32 = 130;
pub const FUTILITY_IMPROVING: u32 = 700;

/// The deepest node whose late quiets may be skipped that way.
pub const FUTILITY_DEPTH: u32 = 6;

/// How many moves a node orders before the quiets after them are taken
/// for noise: `BASE`, plus `RATIO` of the square of the depth left. The
/// row for a side whose evaluation has not risen is the risen side's
/// scaled by `IMPROVING`, so the side already doing worse gives up on its
/// quiets first. `RATIO` and `IMPROVING` are held against
/// `COEFFICIENT_SCALE`.
pub const LMP_BASE: u32 = 3;
pub const LMP_RATIO: u32 = 1000;
pub const LMP_IMPROVING: u32 = 550;

/// The deepest row the count is built to. A node past it reuses that row
/// rather than losing the gate: the counts have already outgrown any real
/// move list, so no deeper row would say anything new.
pub const LMP_DEPTH: u32 = 12;

/// How much material a capture may already be seen to lose and still be
/// searched: `RATIO` of the dearest non-royal piece per ply still to
/// search, held against `COEFFICIENT_SCALE`. Ordering has priced every
/// capture by exchange simulation before the first is searched, so this
/// reads that price back rather than paying for it twice.
pub const SEE_PRUNE_RATIO: u32 = 250;

/// The deepest node allowed to discard a capture on that price alone.
pub const SEE_PRUNE_DEPTH: u32 = 5;

/// How much a capture has to promise at a quiet leaf before it is
/// searched: what it takes, plus `RATIO` of the dearest non-royal piece
/// held against `COEFFICIENT_SCALE`. A capture that cannot reach alpha
/// even at the full price of the piece it takes says nothing the score
/// already standing at that leaf does not. An endgame position is thin
/// enough that a single capture is most of what is left to play for, so
/// the margin is not applied there.
pub const QSEARCH_DELTA_RATIO: u32 = 100;

/// Bounds on the derive-time setup walk: how many distinct censuses may
/// be expanded, and how many completed setups are averaged. A placement
/// tree that outgrows either bound is referenced against the endings
/// already reached rather than being explored to exhaustion.
const SETUP_STATE_CAP: usize = 4096;
const SETUP_ENDING_CAP: usize = 256;

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
/// 3. the top ceil(20%) are the major pieces
/// 4. the rest are the minor pieces
///
/// Params:
/// - state: &mut State -> variant whose derived values are ranked
///
/// Return:
/// Vec<PieceRoles>     -> (index, is_big, is_major) per piece
fn derive_piece_roles(state: &mut State) -> Vec<PieceRoles> {
    let mut ranked = collect_piece_type_pairs(state)
        .into_iter()
        .filter(|(white_index, _)| {
            !p_is_royal!(&state.statics.pieces[*white_index])
        })
        .map(|(white_index, black_index)| {
            (
                p_ovalue!(&state.statics.pieces[white_index]),
                white_index,
                black_index,
            )
        })
        .collect::<Vec<_>>();
    ranked.sort_unstable_by_key(
        |(value, white_index, _)| (*value, *white_index)
    );

    let non_big_share =
        state.statics.role_non_big_split as f32 / COEFFICIENT_SCALE as f32;
    let major_share =
        state.statics.role_major_split as f32 / COEFFICIENT_SCALE as f32;

    let non_big_count = (ranked.len() as f32 * non_big_share).ceil() as usize;
    let major_count = (ranked.len() as f32 * major_share).ceil() as usize;

    log_4!(
        "Role counts - Non-big: {}, Major: {}",
        non_big_count, major_count
    );

    let mut piece_roles = Vec::with_capacity(2 * ranked.len());

    for (rank, (value, white_index, black_index)) in ranked.iter().enumerate() {
        let is_big = rank >= non_big_count;
        let is_major = is_big && rank >= ranked.len() - major_count;

        log_4!(
            "Role rank {} value {} -> big {}, major {}",
            rank, value, is_big, is_major
        );

        piece_roles.push((*white_index as PieceIndex, is_big, is_major));
        piece_roles.push((*black_index as PieceIndex, is_big, is_major));
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
/// f64             -> mean reachable fraction of the board, in (0, 1]
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

/// derive_piece_maneuverability
///
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
/// f64             -> reversible-offset fraction, in [0, 1]
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

/// derive_piece_value
///
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
///   weights each vector by the odds every per-leg stop requirement is met,
///   distinguishing sliders and hoppers from leapers.
///
/// The value blends the two mobilities, then scales by coverage and
/// maneuverability. A larger `occupancy` (sparse board) yields endgame values,
/// letting sliders gain relative to leapers.
///
/// Params:
/// - state    : &State -> precomputed relevant-move tables
/// - piece    : &Piece -> piece to value
/// - occupancy: f64    -> assumed board fill ratio for the phase
///
/// Return:
///
/// f64
/// raw phase value, later offset-normalized across the army
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
/// model, walking each vector leg by leg to mirror generator semantics:
/// pass legs (may move) need an empty stop and weigh `1 - occupancy`,
/// screen legs (capture-only intermediates, the hopper jump) need an
/// occupied stop and weigh `occupancy`. A hopping vector whose final leg
/// is capture-only also needs a target there, weighing `occupancy` once
/// more. Leapers are unaffected; long slides decay geometrically; hopper
/// captures contribute nothing on an empty board.
///
/// Params:
/// - state      : &State     -> precomputed relevant-move tables
/// - piece_index: PieceIndex -> piece whose vectors are counted
/// - square     : usize      -> origin square
/// - occupancy  : f64        -> assumed board fill ratio
///
/// Return:
///
/// f64
/// expected number of playable vectors from the square
fn derive_piece_mobility(
    state: &State, piece_index: PieceIndex, square: usize, occupancy: f64
) -> f64 {
    let board_size = state.statics.board_size;

    let relevant_moves = &state.statics.relevant_moves
        [piece_index as usize * board_size + square];

    relevant_moves
        .iter()
        .filter_map(|vector| derive_vector_chance(vector, occupancy))
        .map(|(chance, ..)| chance)
        .sum()
}

/// derive_vector_chance
///
/// Walks one multi-leg vector under the random-fill occupancy model and
/// returns the odds every per-leg stop requirement is met together with
/// the vector's total displacement. Pass legs (may move) need an empty
/// stop and weigh `1 - occupancy`, screen legs (capture-only
/// intermediates, the hopper jump) need an occupied stop and weigh
/// `occupancy`, and a hopping vector whose final leg is capture-only
/// also needs a target there, weighing `occupancy` once more.
/// Zero-displacement marker legs contribute no weight.
///
/// Params:
/// - multi_leg_vector: &[Leg] -> the vector's legs, final leg last
/// - occupancy       : f64    -> assumed board fill ratio
///
/// Return:
///
/// Option<(f64, i32, i32)>
/// (chance, file delta, rank delta), or None for an empty vector
fn derive_vector_chance(
    multi_leg_vector: &[Leg], occupancy: f64
) -> Option<(f64, i32, i32)> {
    let (final_leg, intermediate_legs) = multi_leg_vector.split_last()?;

    let mut chance = 1.0;
    let mut hopper = false;
    let mut file_delta = x!(final_leg) as i32;
    let mut rank_delta = y!(final_leg) as i32;

    for leg in intermediate_legs {
        file_delta += x!(leg) as i32;
        rank_delta += y!(leg) as i32;

        if x!(leg) == 0 && y!(leg) == 0 {
            continue;
        }

        let needs_piece = (c!(leg) || d!(leg)) && !m!(leg);

        chance *= if needs_piece {
            occupancy
        } else {
            1.0 - occupancy
        };

        hopper |= needs_piece;
    }

    if hopper && c!(final_leg) && !m!(final_leg) {
        chance *= occupancy;
    }

    Some((chance, file_delta, rank_delta))
}

/// derive_distance_from_center
///
/// Measures the euclidean distance from a square to the nearest of the (up
/// to four) central squares, handling even and odd board dimensions.
///
/// Params:
/// - state : &State -> board dimensions for locating the center
/// - square: usize  -> square whose distance is measured
///
/// Return:
/// f64              -> distance in square units
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
/// - state      : &State     -> board dimensions and promotion zones
/// - piece_index: PieceIndex -> piece whose promotion zones are queried
/// - square     : usize      -> square whose distance is measured
///
/// Return:
///
/// f64
/// distance in square units, infinity when no zone exists
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
/// - state      : &State     -> precomputed relevant-move tables
/// - piece_index: PieceIndex -> piece being placed
/// - square     : usize      -> square being scored
/// - is_endgame : bool       -> selects phase occupancy and weights
///
/// Return:
///
/// f64
/// unnormalized square score, later scaled into the PST
fn derive_square_score(
    state: &State, piece_index: PieceIndex, square: usize, is_endgame: bool
) -> f64 {
    let occupancy = if is_endgame {
        state.statics.endgame_occupancy
    } else {
        state.statics.opening_occupancy
    } as f64 / COEFFICIENT_SCALE;

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
/// - state         : &State     -> board dimensions and promotion zones
/// - piece_index   : PieceIndex -> piece being placed
/// - square        : usize      -> square being scored
/// - is_endgame    : bool       -> selects the phase's bonus fraction
/// - piece_value   : f64        -> the piece's current phase value
/// - promoted_value: f64        -> best value reachable by promotion
///
/// Return:
///
/// f64
/// bonus added on top of the positional square score
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
/// Royal pieces get a back-rank gradient in the opening instead of the
/// mobility-and-centrality score (the king should hide behind its own
/// lines, not march): every square scores the negated rank index in the
/// white frame, so rank 0 is best and each step forward is monotonically
/// worse. The endgame royal table keeps the centralizing score.
///
/// Params:
/// - index         : PieceIndex -> piece the table is built for
/// - state         : &State     -> precomputed relevant-move tables
/// - is_endgame    : bool       -> selects phase weights and values
/// - promoted_value: f64        -> best value reachable by promotion
///
/// Return:
/// Vec<i32>                     -> per-square bonus table in board order
fn derive_pst(
    index: PieceIndex, state: &State, is_endgame: bool, promoted_value: f64
) -> Vec<i32> {
    let board_size = state.statics.board_size;
    let files = state.statics.files as usize;
    let piece = &state.statics.pieces[index as usize];

    let piece_value = if is_endgame {
        p_evalue!(piece) as f64
    } else {
        p_ovalue!(piece) as f64
    };

    let scores: Vec<f64> = if !is_endgame && p_is_royal!(piece) {
        (0..board_size).map(|square| -((square / files) as f64)).collect()
    } else {
        (0..board_size).into_par_iter().map(|square| {
            derive_square_score(state, index, square, is_endgame)
        }).collect()
    };

    let mean = scores.iter().sum::<f64>() / board_size as f64;
    let max_deviation = scores
        .iter()
        .map(|score| (score - mean).abs())
        .fold(0.0_f64, f64::max)
        .max(1.0);
    let amplitude = 24.0;

    (0..board_size).map(|square| {
        let positional =
            (scores[square] - mean) / max_deviation * amplitude;
        let promotion = derive_promotion_bonus(
            state, index, square, is_endgame, piece_value, promoted_value
        );

        (positional + promotion).round() as i32
    }).collect()
}

/// setup_census_key
///
/// Builds the identity the setup walk memoizes on: the board census, both
/// hands, and the side to place. Two part-built setups differing only in
/// where equal pieces stand share a key, which is what keeps the walk
/// bounded on a variant whose placements are largely interchangeable.
///
/// Params:
/// - state: &State -> position part-way through its placement phase
///
/// Return:
/// Vec<u32>        -> census, both hands, and side to place, flattened
fn setup_census_key(state: &State) -> Vec<u32> {
    let mut key = state.piece_count.clone();

    for side in [WHITE as usize, BLACK as usize] {
        key.extend(state.piece_in_hand[side].iter().map(|held| *held as u32));
    }

    key.push(state.playing as u32);

    key
}

/// walk_setup_endings
///
/// Depth-first walk of the placement phase over one scratch position,
/// recording the board census every time the variant's own rules end
/// SETUP. Placements are made and unmade in place, so the walk costs one
/// position rather than one per node, and it never asks what ends a setup
/// -- it plays until the position says it has.
///
/// Params:
/// - probe  : &mut State             -> scratch position, restored on return
/// - visited: &mut HashSet<Vec<u32>> -> censuses already expanded
/// - endings: &mut Vec<Vec<u32>>     -> censuses of completed setups
fn walk_setup_endings(
    probe: &mut State,
    visited: &mut HashSet<Vec<u32>>,
    endings: &mut Vec<Vec<u32>>,
) {
    if endings.len() >= SETUP_ENDING_CAP
    || visited.len() >= SETUP_STATE_CAP
    || !visited.insert(setup_census_key(probe))
    {
        return;
    }

    let mut placements = Vec::new();
    let mut scratch = Vec::new();

    generate_all_moves_and_drops(probe, &mut placements, &mut scratch);

    for placement in placements {
        if !make_move!(probe, placement) {
            continue;
        }

        if probe.game_phase == SETUP {
            walk_setup_endings(probe, visited, endings);
        } else {
            endings.push(probe.piece_count.clone());
        }

        undo_move!(probe);

        if endings.len() >= SETUP_ENDING_CAP {
            break;
        }
    }
}

/// resolve_setup_army
///
/// Reports the army a variant actually begins play with. Most variants
/// already stand theirs on the board, so their live census answers
/// directly. A variant with a placement phase does not: what it starts
/// with is whatever that phase leaves behind, and a hand there is a menu
/// of what may be placed rather than a promise that all of it will be --
/// a variant offering a choice of armies holds every one of them and
/// deploys exactly one.
///
/// So the walk plays legal placements until the rules end SETUP and
/// averages the census over the completed setups it reaches, leaving a
/// variant that chooses between armies referenced against a
/// representative one rather than the union of every offer. It runs once,
/// at derivation, over a copy of the position; nothing here happens per
/// node.
///
/// Notes:
/// The copy has to be dropped before the caller writes any static, since
/// `static_mut` claims sole ownership of the shared static state.
///
/// Params:
/// - state: &State -> start position, piece values already derived
///
/// Return:
/// Vec<u32>        -> deployed count per piece index once play begins
fn resolve_setup_army(state: &State) -> Vec<u32> {
    if state.game_phase != SETUP {
        return state.piece_count.clone();
    }

    let mut probe = state.clone();
    let mut visited = HashSet::new();
    let mut endings = Vec::new();

    walk_setup_endings(&mut probe, &mut visited, &mut endings);

    log_4!(
        "Setup walk: {} censuses expanded, {} endings reached",
        visited.len(), endings.len()
    );

    if endings.is_empty() {
        return state.piece_count.clone();
    }

    (0..state.piece_count.len())
        .map(|piece_index| {
            let total = endings
                .iter()
                .map(|ending| ending[piece_index] as u64)
                .sum::<u64>();

            (total / endings.len() as u64) as u32
        })
        .collect()
}

/// derive_parameters
///
/// Startup entry point for the whole derivation pass: computes the
/// evaluation parameters, then the search margins built on top of them,
/// and finally refreshes the incremental eval caches.
///
/// Params:
/// - state: &mut State -> freshly precomputed variant state
pub fn derive_parameters(state: &mut State) {
    derive_eval_parameters(state);
    derive_search_parameters(state);
    refresh_eval_state(state);
}

/// reduction_surface
///
/// Builds one late-move reduction table: for every remaining depth and
/// every move number, how many plies a move of that class gives up on its
/// first search. Depth zero and move zero index nothing the search ever
/// reduces, so they hold zero rather than the logarithm of it.
///
/// Params:
/// - base   : u32 -> curve base, held against `COEFFICIENT_SCALE`
/// - divisor: u32 -> curve divisor, held against `COEFFICIENT_SCALE`
/// - shape  : F   -> the depth and move terms this curve mixes
///
/// Return:
/// Vec<u8> -> `MAX_DEPTH * REDUCTION_MOVE_CAP` plies, depth major
pub fn reduction_surface<F>(base: u32, divisor: u32, shape: F) -> Vec<u8>
where
    F: Fn(f64, f64) -> f64,
{
    let base = base as f64 / COEFFICIENT_SCALE;
    let divisor = divisor as f64 / COEFFICIENT_SCALE;
    let mut table = vec![0u8; MAX_DEPTH * REDUCTION_MOVE_CAP];

    for depth in 1..MAX_DEPTH {
        for moves in 1..REDUCTION_MOVE_CAP {
            let plies = base + shape(depth as f64, moves as f64) / divisor;

            table[depth * REDUCTION_MOVE_CAP + moves] =
                plies.clamp(0.0, u8::MAX as f64) as u8;
        }
    }

    table
}

/// derive_search_parameters
///
/// Drives the search half of derivation: rebuilds all four late-move
/// reduction surfaces and the root aspiration width from the coefficients
/// currently held in the static state. Every write of those coefficients
/// ends here, whether it came from a payload or from the defaults, so no
/// derived value can be left describing the variant before it.
///
/// The window is priced off the dearest non-royal piece rather than the
/// cheapest, which normalization pins at 100 in every variant and so says
/// nothing: a flat value range moves the score less per capture and earns
/// the narrower window that follows from it. The reverse futility margin
/// reads the same piece for the same reason, one flat row and one for a
/// side whose evaluation has risen, indexed by the depth left to search.
/// The futility margin and the exchange allowance are drawn against that
/// same piece, and the late-move count against depth alone, having no
/// material in it to price.
///
/// Every improving multiplier names the row that prunes harder, which is
/// not the same row throughout: a cut against beta believes a risen side
/// sooner, while both cuts against alpha give up on a side that has not
/// risen first. The row a node reads is always its improving flag, so the
/// choice lives here rather than at every use.
///
/// Params:
/// - state: &mut State -> variant whose derived search values are rebuilt
pub fn derive_search_parameters(state: &mut State) {
    let statics = &state.statics;

    let dearest = statics.pieces
        .iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_ovalue!(piece) as u64)
        .max()
        .unwrap_or(0);

    let delta = dearest * statics.aspiration_ratio as u64
        / COEFFICIENT_SCALE as u64;

    let deepest = statics.rfp_depth as usize;
    let step = dearest * statics.rfp_ratio as u64 / COEFFICIENT_SCALE as u64;
    let mut margins = vec![0i32; 2 * (deepest + 1)];

    for depth in 1..=deepest {
        let flat = step * depth as u64;

        margins[depth] = flat as i32;
        margins[deepest + 1 + depth] = (flat
            * statics.rfp_improving as u64
            / COEFFICIENT_SCALE as u64) as i32;
    }

    let futility_deepest = statics.futility_depth as usize;
    let futility_floor = dearest * statics.futility_floor as u64
        / COEFFICIENT_SCALE as u64;
    let futility_step = dearest * statics.futility_ratio as u64
        / COEFFICIENT_SCALE as u64;
    let mut futility = vec![0i32; 2 * (futility_deepest + 1)];

    for depth in 1..=futility_deepest {
        let risen = futility_floor + futility_step * depth as u64;

        futility[futility_deepest + 1 + depth] = risen as i32;
        futility[depth] = (risen
            * statics.futility_improving as u64
            / COEFFICIENT_SCALE as u64) as i32;
    }

    let lmp_deepest = statics.lmp_depth as usize;
    let mut counts = vec![0usize; 2 * (lmp_deepest + 1)];

    for depth in 0..=lmp_deepest {
        let risen = statics.lmp_base as u64
            + (depth * depth) as u64 * statics.lmp_ratio as u64
                / COEFFICIENT_SCALE as u64;

        counts[lmp_deepest + 1 + depth] = risen as usize;
        counts[depth] = (risen * statics.lmp_improving as u64
            / COEFFICIENT_SCALE as u64).max(1) as usize;                        /* one quiet always gets ordered      */
    }

    let see_deepest = statics.see_prune_depth as usize;
    let see_step = dearest * statics.see_prune_ratio as u64
        / COEFFICIENT_SCALE as u64;
    let mut allowance = vec![0i32; see_deepest + 1];

    for depth in 1..=see_deepest {
        allowance[depth] = (see_step * depth as u64) as i32;
    }

    let qsearch_delta = dearest * statics.qsearch_delta_ratio as u64
        / COEFFICIENT_SCALE as u64;

    assert!(
        futility.chunks(futility_deepest + 1)
            .all(|row| row.windows(2).all(|pair| pair[0] <= pair[1])),
        "Futility margins must not fall with the depth left to search."
    );

    assert!(
        counts.chunks(lmp_deepest + 1)
            .all(|row| row.windows(2).all(|pair| pair[0] <= pair[1])),
        "Late-move counts must not fall with the depth left to search."
    );

    assert!(
        allowance.windows(2).all(|pair| pair[0] <= pair[1]),
        "Exchange allowances must not fall with the depth left to search."
    );

    let quiet = reduction_surface(
        statics.reduction_quiet_base,
        statics.reduction_quiet_divisor,
        |depth, moves| depth.ln() * moves.ln(),
    );

    let quiet_check = reduction_surface(
        statics.reduction_quiet_check_base,
        statics.reduction_quiet_check_divisor,
        |depth, moves| depth.sqrt() * moves.ln(),
    );

    let tactical = reduction_surface(
        statics.reduction_tactical_base,
        statics.reduction_tactical_divisor,
        |depth, moves| depth.ln() * moves.sqrt(),
    );

    let tactical_check = reduction_surface(
        statics.reduction_tactical_check_base,
        statics.reduction_tactical_check_divisor,
        |depth, moves| depth.ln() * moves.ln(),
    );

    let statics = state.static_mut();

    statics.reduction_quiet = quiet;
    statics.reduction_quiet_check = quiet_check;
    statics.reduction_tactical = tactical;
    statics.reduction_tactical_check = tactical_check;

    statics.aspiration_delta = (delta as u32).max(1);                           /* a window has to hold two scores    */
    statics.rfp_margin = margins;
    statics.futility_margin = futility;
    statics.lmp_count = counts;
    statics.see_allowance = allowance;
    statics.qsearch_delta = qsearch_delta as i32;
}

/// derive_eval_parameters
///
/// Drives the evaluation half of parameter derivation: values every
/// white piece for both phases (black twins copy them via the swap map),
/// normalizes against the cheapest piece, assigns big/major/minor roles,
/// derives the opening/endgame phase thresholds from the army the variant
/// actually starts play with, and builds all piece-square tables (black
/// tables are the white ones mirrored across the horizontal axis).
///
/// Params:
/// - state: &mut State -> variant whose dynamic parameters are filled
pub fn derive_eval_parameters(state: &mut State) {
    log_3!("Deriving dynamic evaluation parameters...");

    let opening_occupancy =
        state.statics.opening_occupancy as f64 / COEFFICIENT_SCALE;
    let endgame_occupancy =
        state.statics.endgame_occupancy as f64 / COEFFICIENT_SCALE;

    let values = state.statics.pieces
        .par_iter()
        .filter_map(|piece| {
            if p_color!(piece) == BLACK {
                return None;
            }

            let index = p_index!(piece) as usize;
            let opening = derive_piece_value(state, piece, opening_occupancy);
            let endgame = derive_piece_value(state, piece, endgame_occupancy);

            Some((index, opening, endgame))
        })
        .collect::<Vec<_>>();

    let offset = values
        .iter()
        .map(|(_, opening, _)| *opening)
        .fold(f64::INFINITY, f64::min) - 100.0;

    for (index, opening, endgame) in values {
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

    let start_army = resolve_setup_army(state);

    let mut deployed_value = 0u64;
    let mut deployed_count = 0u64;
    let mut deployed_big = 0u64;

    for (piece_index, piece) in state.statics.pieces.iter().enumerate() {
        if p_is_royal!(piece) {
            continue;
        }

        let deployed = start_army[piece_index] as u64;

        deployed_value += p_ovalue!(piece) as u64 * deployed;
        deployed_count += deployed;
        deployed_big += deployed * p_is_big!(piece) as u64;
    }

    let mean_value = deployed_value.checked_div(deployed_count).unwrap_or(0);

    log_3!(
        "Mean deployed non-royal value: {} over {} pieces, {} of them big",
        mean_value, deployed_count, deployed_big
    );

    let opening_score = (mean_value * deployed_big).max(1);                     /* a variant with no big army still   */
    let endgame_score =                                                         /* needs a positive taper divisor     */
        (mean_value * state.statics.endgame_army_size as u64)
            .min(opening_score - 1);

    state.static_mut().opening_score = opening_score as u32;
    state.static_mut().endgame_score = endgame_score as u32;

    state.big_pieces = [0; 2];
    state.major_pieces = [0; 2];
    state.minor_pieces = [0; 2];

    for (piece_idx, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let count = state.piece_count[piece_idx];

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
