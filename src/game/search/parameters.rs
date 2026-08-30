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

/// Board occupancy assumed when valuing a piece: the fraction of squares
/// a slider expects to find blocked in each phase, which is what makes an
/// opening value differ from an endgame one.
const OPENING_OCCUPANCY: u32 = 360;
const ENDGAME_OCCUPANCY: u32 = 120;

/// Where the ranked non-royal army is cut into roles: the cheapest share
/// that is not big, and the dearest share that is major.
const ROLE_NON_BIG_SPLIT: u32 = 100;
const ROLE_MAJOR_SPLIT: u32 = 200;

/// How small the big non-royal army has to get before play counts as an
/// endgame, measured in pieces of average deployed value.
const ENDGAME_ARMY_SIZE: u32 = 5;

/// Late-move reduction curves, one per class of move. Each surface is
/// `base + shape(depth, moves) / divisor`. Which terms a curve mixes is fixed
/// by class because a quiet move buried in a long list and a capture answering
/// check do not respond to the same variable. Base and divisor are held against
/// `COEFFICIENT_SCALE`.
const REDUCTION_QUIET_BASE: u32 = 750;
const REDUCTION_QUIET_DIVISOR: u32 = 2250;
const REDUCTION_QUIET_CHECK_BASE: u32 = 1000;
const REDUCTION_QUIET_CHECK_DIVISOR: u32 = 4000;
const REDUCTION_TACTICAL_BASE: u32 = 1000;
const REDUCTION_TACTICAL_DIVISOR: u32 = 4000;
const REDUCTION_TACTICAL_CHECK_BASE: u32 = 0;
const REDUCTION_TACTICAL_CHECK_DIVISOR: u32 = 4500;

/// The window the root reopens around the previous completed score: a
/// fraction of the dearest piece, that being the top of this variant's
/// score range and so the scale one iteration's swing away from the last
/// is drawn against. Only the side that failed widens, by `WIDEN` each
/// time, until it passes `CLAMP` times the width it opened at; past that
/// the root reopens fully instead of widening again. Every one of the
/// three is held against `COEFFICIENT_SCALE`.
const ASPIRATION_RATIO: u32 = 30;

/// The cushion a node has to clear before its static evaluation alone is
/// trusted to beat beta: `RATIO` of the dearest non-royal piece per ply
/// still to search, the same anchor the aspiration window is priced off.
/// A side already standing better than it did two plies ago is believed
/// on less, so its row is the flat one scaled by `IMPROVING`. Both are
/// held against `COEFFICIENT_SCALE`.
const RFP_RATIO: u32 = 110;
const RFP_IMPROVING: u32 = 750;

/// How far under alpha a node may stand and still search its late quiet
/// moves: `FLOOR` of the dearest non-royal piece, plus `RATIO` of it for
/// every ply still to search. A quiet move promises nothing immediate, so
/// a node further under alpha than that has none left worth ordering. The
/// side whose evaluation has not risen is believed least and so is given
/// the smaller margin, the risen side's row scaled by `IMPROVING`. All
/// three are held against `COEFFICIENT_SCALE`.
const FUTILITY_FLOOR: u32 = 100;
const FUTILITY_RATIO: u32 = 130;
const FUTILITY_IMPROVING: u32 = 700;

/// How many moves a node orders before the quiets after them are taken
/// for noise: `BASE`, plus `RATIO` of the square of the depth left. The
/// row for a side whose evaluation has not risen is the risen side's
/// scaled by `IMPROVING`, so the side already doing worse gives up on its
/// quiets first. `RATIO` and `IMPROVING` are held against
/// `COEFFICIENT_SCALE`.
const LMP_BASE: u32 = 3;
const LMP_RATIO: u32 = 1000;
const LMP_IMPROVING: u32 = 550;

/// How much material a capture may already be seen to lose and still be
/// searched: `RATIO` of the dearest non-royal piece per ply still to
/// search, held against `COEFFICIENT_SCALE`. Ordering has priced every
/// capture by exchange simulation before the first is searched, so this
/// reads that price back rather than paying for it twice.
const SEE_PRUNE_RATIO: u32 = 250;

/// How much a capture has to promise at a quiet leaf before it is
/// searched: what it takes, plus `RATIO` of the dearest non-royal piece
/// held against `COEFFICIENT_SCALE`. A capture that cannot reach alpha
/// even at the full price of the piece it takes says nothing the score
/// already standing at that leaf does not. An endgame position is thin
/// enough that a single capture is most of what is left to play for, so
/// the margin is not applied there.
const QSEARCH_DELTA_RATIO: u32 = 100;

/// The ring a royal calls its own ground: every square within `RADIUS`
/// steps on both axes. Squares of that ring lying ahead of the royal are
/// its shelter, held by pieces that only ever advance. `CAP` is how many
/// sheltering pieces are still worth counting -- past it a royal is as
/// walled in as this term can say, and the next piece belongs elsewhere.
/// Shelter is priced as `RATIO` of the dearest non-royal piece, held
/// against `COEFFICIENT_SCALE` and never below `FLOOR` in raw units.
const SHELTER_RADIUS: u32 = 1;
const SHELTER_RATIO: u32 = 12;
const SHELTER_FLOOR: u32 = 4;

/// Share of the board a royal must be able to stand on before shelter is
/// worth pricing at all. A royal walled into a palace by its own forbidden
/// zones cannot be sheltered in the sense this term means: it never left
/// its own camp, its guards are pinned to it by their own move rules, and
/// the only way it can gather friendly pieces in front of itself is to
/// walk forward, which is exactly the move such variants punish. Below
/// this share the term is switched off for that colour.
const SHELTER_CONFINEMENT_DIVISOR: usize = 4;

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

    let non_big_share = ROLE_NON_BIG_SPLIT as f32 / COEFFICIENT_SCALE as f32;
    let major_share = ROLE_MAJOR_SPLIT as f32 / COEFFICIENT_SCALE as f32;

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

/// derive_piece_offsets
///
/// Every distinct net displacement a piece can make, summed over the legs
/// of each multi-leg vector and gathered across every square it could
/// stand on. A slider contributes one offset per distance it can travel,
/// so the set says how far a piece reaches as well as in which
/// directions.
///
/// Params:
/// - state: &State -> precomputed relevant-move tables
/// - piece: &Piece -> piece whose offsets are gathered
///
/// Return:
/// HashSet<(i32, i32)> -> file and rank displacements, origin excluded
fn derive_piece_offsets(state: &State, piece: &Piece) -> HashSet<(i32, i32)> {
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

    offsets
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
    let offsets = derive_piece_offsets(state, piece);

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
        ENDGAME_OCCUPANCY
    } else {
        OPENING_OCCUPANCY
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
/// `static_mut` claims sole ownership of the shared static state. Its eval
/// caches are rebuilt first because the roles the caller just assigned
/// invalidate the counts the position was loaded with.
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

    refresh_eval_state(&mut probe);                                             /* roles changed under the copy       */
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
/// evaluation parameters, then the search margins and the royal shelter
/// tables built on top of them, then the capabilities the rules permit, and
/// finally refreshes the incremental eval caches.
///
/// Params:
/// - state: &mut State -> freshly precomputed variant state
pub fn derive_parameters(state: &mut State) {
    derive_eval_parameters(state);
    derive_search_parameters(state);
    derive_shelter_parameters(state);
    derive_search_capabilities(state);
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
/// reduction surfaces, margins, move counts, and root aspiration width from
/// universal coefficients and loaded material.
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

    let delta = dearest * ASPIRATION_RATIO as u64
        / COEFFICIENT_SCALE as u64;

    let deepest = RFP_DEPTH as usize;
    let step = dearest * RFP_RATIO as u64 / COEFFICIENT_SCALE as u64;
    let mut margins = vec![0i32; 2 * (deepest + 1)];

    for depth in 1..=deepest {
        let flat = step * depth as u64;

        margins[depth] = flat as i32;
        margins[deepest + 1 + depth] = (flat
            * RFP_IMPROVING as u64
            / COEFFICIENT_SCALE as u64) as i32;
    }

    let futility_deepest = FUTILITY_DEPTH as usize;
    let futility_floor = dearest * FUTILITY_FLOOR as u64
        / COEFFICIENT_SCALE as u64;
    let futility_step = dearest * FUTILITY_RATIO as u64
        / COEFFICIENT_SCALE as u64;
    let mut futility = vec![0i32; 2 * (futility_deepest + 1)];

    for depth in 1..=futility_deepest {
        let risen = futility_floor + futility_step * depth as u64;

        futility[futility_deepest + 1 + depth] = risen as i32;
        futility[depth] = (risen
            * FUTILITY_IMPROVING as u64
            / COEFFICIENT_SCALE as u64) as i32;
    }

    let lmp_deepest = LMP_DEPTH as usize;
    let mut counts = vec![0usize; 2 * (lmp_deepest + 1)];

    for depth in 0..=lmp_deepest {
        let risen = LMP_BASE as u64
            + (depth * depth) as u64 * LMP_RATIO as u64
                / COEFFICIENT_SCALE as u64;

        counts[lmp_deepest + 1 + depth] = risen as usize;
        counts[depth] = (risen * LMP_IMPROVING as u64
            / COEFFICIENT_SCALE as u64).max(1) as usize;                        /* one quiet always gets ordered      */
    }

    let see_deepest = SEE_PRUNE_DEPTH as usize;
    let see_step = dearest * SEE_PRUNE_RATIO as u64
        / COEFFICIENT_SCALE as u64;
    let mut allowance = vec![0i32; see_deepest + 1];

    for depth in 1..=see_deepest {
        allowance[depth] = (see_step * depth as u64) as i32;
    }

    let qsearch_delta = dearest * QSEARCH_DELTA_RATIO as u64
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
        REDUCTION_QUIET_BASE,
        REDUCTION_QUIET_DIVISOR,
        |depth, moves| depth.ln() * moves.ln(),
    );

    let quiet_check = reduction_surface(
        REDUCTION_QUIET_CHECK_BASE,
        REDUCTION_QUIET_CHECK_DIVISOR,
        |depth, moves| depth.sqrt() * moves.ln(),
    );

    let tactical = reduction_surface(
        REDUCTION_TACTICAL_BASE,
        REDUCTION_TACTICAL_DIVISOR,
        |depth, moves| depth.ln() * moves.sqrt(),
    );

    let tactical_check = reduction_surface(
        REDUCTION_TACTICAL_CHECK_BASE,
        REDUCTION_TACTICAL_CHECK_DIVISOR,
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

/// derive_search_capabilities
///
/// Decides once, before any game starts, which of the search's shortcuts this
/// rule set still permits, and records them in `capabilities` for the readers
/// documented on [`StaticState`]. Each shortcut rests on a claim about the
/// game rather than about a position -- that material is the currency, that a
/// static score bounds a subtree, that giving up the move concedes something,
/// that a late quiet move is a bad one -- and a variant that never makes the
/// claim leaves the bit clear and has the position played out instead.
///
/// Two kinds of fact answer the questions. Movement facts come from the
/// generated vectors: a leg that unloads what it destroyed needs a second
/// piece standing where it stands, a leg that may take a royal is not trading
/// material, a vector that destroys twice wins more than its victim, a vector
/// that ends where it started having taken nothing is a pass the variant
/// already offers -- a lion returning home over a corpse is not one, which is
/// why the destroy flag disqualifies the shape rather than the displacement
/// alone -- and a piece with no quiet vector cannot give up a tempo at all.
/// Terminal facts
/// come from the declared rules: counting pieces, holding a zone, or tallying
/// checks all pay in a currency material does not convert to.
///
/// Nothing here reads a variant's name, and nothing asks whether a rule is
/// familiar. A rule set written tomorrow is judged by the same questions.
///
/// Params:
/// - state: &mut State -> variant whose capability mask is derived
pub fn derive_search_capabilities(state: &mut State) {
    let statics = &state.statics;
    let board_size = statics.board_size;

    let mut screened = false;
    let mut royal_capture = false;
    let mut multi_capture = false;
    let mut capture_only = false;
    let mut may_pass = false;

    for piece_index in 0..statics.pieces.len() {
        let mut vectors = 0;
        let mut quiet_vectors = 0;

        for square in 0..board_size {
            let slot = piece_index * board_size + square;

            for vector in statics.relevant_moves[slot].iter()
                .chain(statics.relevant_captures[slot].iter())
            {
                let mut destroyed = 0;
                let mut destroys = false;

                for leg in vector {
                    screened |= u!(leg);
                    royal_capture |= k!(leg);
                    destroys |= d!(leg);
                    destroyed += (d!(leg) && !u!(leg)) as usize;                /* an unloaded piece is put back      */
                }

                let (files_crossed, ranks_crossed) = vector_offset!(vector);
                let moves_quietly = vector_moves_quietly!(vector);

                multi_capture |= destroyed > 1;
                may_pass |= moves_quietly && !destroys
                    && files_crossed == 0 && ranks_crossed == 0;                /* nothing moved and nothing taken   */
                vectors += 1;
                quiet_vectors += moves_quietly as usize;
            }
        }

        capture_only |= vectors > 0 && quiet_vectors == 0;
    }

    let termination = &state.termination;

    let counts_pieces = !termination.extinct.is_empty();
    let holds_zone = termination.goal.is_some();
    let counts_checks = termination.checks.is_some();
    let counts_material = termination.counting.is_some();

    let misere = termination.checkmate == Outcome::Win
        || termination.stalemate == Outcome::Win
        || termination.extinct.iter()
            .any(|rule| rule.outcome == Outcome::Win);

    let recycles_captures = promote_to_captured!(state) || drops!(state);
    let places_army = setup_phase!(state);
    let vetoes_moves = stand_offs!(state);

    let mut capabilities = 0u16;

    if !royal_capture && !multi_capture && !misere
    && !counts_pieces && !promote_to_captured!(state)
    {
        enc_see_valid!(capabilities);
    }

    if !recycles_captures && !counts_checks
    && !counts_material && !holds_zone
    {
        enc_see_pruning!(capabilities);
    }

    if !misere && !counts_pieces && !holds_zone && !counts_checks {
        enc_forward_pruning!(capabilities);
    }

    if !misere && !holds_zone && !counts_checks && !counts_material
    && !may_pass && !vetoes_moves && !places_army && !capture_only
    {
        enc_null_pruning!(capabilities);
    }

    if !multi_capture && !recycles_captures
    && !counts_checks && !holds_zone
    {
        enc_recapture_order!(capabilities);
    }

    if !misere && !holds_zone && !counts_checks {
        enc_quiet_pruning!(capabilities);
    }

    if !screened {
        enc_static_movement!(capabilities);
    }

    state.static_mut().capabilities = capabilities;

    log_3!("Derived Search Capabilities: {:07b}", capabilities);
}

/// derive_base_pst
///
/// Builds rule-derived opening and endgame PST bases for every piece. Loaded
/// material must be final because promotion gradients use it. Black rows mirror
/// their White twins across the horizontal axis.
///
/// Params:
/// - state: &State -> variant whose rule-derived PST bases are built
///
/// Return:
/// (Vec<Vec<i32>>, Vec<Vec<i32>>) -> opening and endgame rows by piece index
pub fn derive_base_pst(state: &State) -> (Vec<Vec<i32>>, Vec<Vec<i32>>) {
    let promoted_opening = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_ovalue!(piece) as f64)
        .fold(0.0_f64, f64::max);
    let promoted_endgame = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_evalue!(piece) as f64)
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

    let piece_count = state.statics.pieces.len();
    let board_size = state.statics.board_size;
    let mut opening = vec![vec![0; board_size]; piece_count];
    let mut endgame = vec![vec![0; board_size]; piece_count];

    for (index, opening_pst, endgame_pst) in pst_entries {
        opening[index] = opening_pst;
        endgame[index] = endgame_pst;
    }

    (opening, endgame)
}

/// derive_material_values
///
/// Derives opening and endgame material from movement rules. Role flags stay
/// clear until the loaded-value post-pass ranks the finished material table.
///
/// Params:
/// - state: &mut State -> variant whose material values are derived
fn derive_material_values(state: &mut State) {
    let opening_occupancy = OPENING_OCCUPANCY as f64 / COEFFICIENT_SCALE;
    let endgame_occupancy = ENDGAME_OCCUPANCY as f64 / COEFFICIENT_SCALE;

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
}

/// derive_eval_parameters
///
/// Derives material from rules, then rebuilds every material-dependent
/// evaluation product through the same post-load path used by payloads.
///
/// Params:
/// - state: &mut State -> variant whose evaluation parameters are derived
pub fn derive_eval_parameters(state: &mut State) {
    derive_material_values(state);
    derive_eval_products(state);
}

/// derive_eval_products
///
/// Rebuilds roles, phase thresholds, dynamic role counts, and rule-derived PST
/// bases after final material values have loaded.
///
/// Params:
/// - state: &mut State -> variant whose loaded-material products are rebuilt
pub fn derive_eval_products(state: &mut State) {
    log_3!("Deriving dynamic evaluation parameters...");

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
        (mean_value * ENDGAME_ARMY_SIZE as u64)
            .min(opening_score - 1);

    state.static_mut().opening_score = opening_score as u32;
    state.static_mut().endgame_score = endgame_score as u32;

    let (pst_opening, pst_endgame) = derive_base_pst(state);
    state.static_mut().pst_opening = pst_opening;
    state.static_mut().pst_endgame = pst_endgame;

    refresh_eval_state(state);

    log_3!("Derived Opening Score Threshold: {}", state.statics.opening_score);
    log_3!("Derived Endgame Score Threshold: {}", state.statics.endgame_score);
}

/// derive_forward_directions
///
/// Rank step each colour advances by, read from the army the variant
/// starts play with: the side deployed on the lower ranks is the side that
/// moves up the board. A variant that deploys nothing, or deploys both
/// colours around the same rank, still has to name a direction and sends
/// white up.
///
/// Params:
/// - state: &State -> variant whose initial deployment is read
///
/// Return:
/// [i32; 2] -> colour to rank step, either 1 or -1
fn derive_forward_directions(state: &State) -> [i32; 2] {
    let files = state.statics.files as usize;
    let board_size = state.statics.board_size;

    let mut ranks = [0i64; 2];
    let mut counts = [0i64; 2];

    for (piece_index, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let setup = &state.statics.initial_setup[piece_index];

        for square in 0..board_size {
            if get!(setup, square as u32) {
                ranks[color] += (square / files) as i64;
                counts[color] += 1;
            }
        }
    }

    let white = ranks[WHITE as usize]
        .checked_div(counts[WHITE as usize])
        .unwrap_or(0);
    let black = ranks[BLACK as usize]
        .checked_div(counts[BLACK as usize])
        .unwrap_or(1);

    if black < white {
        [-1, 1]
    } else {
        [1, -1]
    }
}

/// derive_shield_pieces
///
/// Marks the piece types worth having in front of a royal: a non-royal
/// that never leaves the local neighbourhood, and whose offsets lean
/// forward on balance. A pawn, a shogi gold and a silver all qualify; a
/// piece leaping over the neighbourhood shelters nothing, so only a
/// straight step out of the deployment rank is allowed past the radius,
/// and a piece with no forward lean is not standing in front of anything.
///
/// Move vectors are stored in the mover's own frame, with the colour sign
/// applied only when a move is walked, so a rank offset is forward for
/// whichever colour owns the piece and needs no board direction here.
///
/// Params:
/// - state : &State -> precomputed relevant-move tables
/// - radius: i32    -> radius the local square lists are built at
///
/// Return:
/// Vec<bool> -> piece index to shield-like role
fn derive_shield_pieces(state: &State, radius: i32) -> Vec<bool> {
    state.statics.pieces.iter().map(|piece| {
        if p_is_royal!(piece) {
            return false;
        }

        let offsets = derive_piece_offsets(state, piece);

        if offsets.is_empty() {
            return false;
        }

        let local = offsets.iter().all(|(file_offset, rank_offset)| {
            let reach = file_offset.abs().max(rank_offset.abs());

            reach <= radius
                || (reach == radius + 1 && *file_offset == 0)                   /* a straight double step counts too  */
        });
        let lean: i32 = offsets
            .iter()
            .map(|(_, rank_offset)| rank_offset)
            .sum();

        local && lean > 0
    }).collect()
}

/// derive_royal_confinement
///
/// Whether each colour's royals are locked into a small corner of the board
/// by their own forbidden zones. Reach is read straight off the zone
/// bitboard rather than walked, since a zone already states every square
/// the piece may ever stand on.
///
/// Params:
/// - state: &State -> variant whose royal zones are read
///
/// Return:
/// [bool; 2]       -> per colour, whether shelter should be priced at all
fn derive_royal_confinement(state: &State) -> [bool; 2] {
    let board_size = state.statics.board_size;
    let mut confined = [false; 2];

    for piece in &state.statics.pieces {
        if !p_is_royal!(piece) {
            continue;
        }

        let zone = &state.statics.forbidden_zones[p_index!(piece) as usize];
        let reach = (0..board_size)
            .filter(|square| !get!(zone, *square as u32))
            .count();

        confined[p_color!(piece) as usize] |=
            reach * SHELTER_CONFINEMENT_DIVISOR <= board_size;
    }

    confined
}

/// derive_shelter_parameters
///
/// Builds everything the royal shelter term reads. One flat square list per
/// colour holds squares inside the royal's local ring that lie forward of its
/// origin. A count per origin records how many slots survive board edges, so
/// evaluation needs no bounds arithmetic.
///
/// Shelter is priced off the dearest non-royal piece, the same piece search
/// margins use, so a variant whose army is cheap pays a proportionate value.
///
/// Params:
/// - state: &mut State -> variant whose shelter tables are rebuilt
pub fn derive_shelter_parameters(state: &mut State) {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let radius = SHELTER_RADIUS as i32;
    let stride = (2 * radius + 1).pow(2) as usize - 1;                          /* the origin itself is never stored  */

    let forward = derive_forward_directions(state);
    let shield_pieces = derive_shield_pieces(state, radius);

    let mut shelter_squares = [
        vec![0 as Square; board_size * stride],
        vec![0 as Square; board_size * stride]
    ];
    let mut shelter_counts = [vec![0u8; board_size], vec![0u8; board_size]];

    for square in 0..board_size {
        let file = square as i32 % files;
        let rank = square as i32 / files;

        for rank_offset in -radius..=radius {
            for file_offset in -radius..=radius {
                let local_file = file + file_offset;
                let local_rank = rank + rank_offset;

                if (file_offset, rank_offset) == (0, 0)
                    || local_file < 0 || local_file >= files
                    || local_rank < 0 || local_rank >= ranks {
                    continue;
                }

                let local = (local_rank * files + local_file) as Square;

                for color in [WHITE as usize, BLACK as usize] {
                    if rank_offset * forward[color] <= 0 {
                        continue;
                    }

                    let slot = shelter_counts[color][square] as usize;

                    shelter_squares[color][square * stride + slot] = local;
                    shelter_counts[color][square] += 1;
                }
            }
        }
    }

    let confined = derive_royal_confinement(state);

    for color in [WHITE as usize, BLACK as usize] {
        if confined[color] {
            shelter_counts[color] = vec![0u8; board_size];                      /* a walled royal reads no squares    */
        }
    }

    let dearest = state.statics.pieces
        .iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_ovalue!(piece) as u64)
        .max()
        .unwrap_or(0);

    let shelter_value = (dearest * SHELTER_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(SHELTER_FLOOR as u64);

    log_3!(
        concat!(
            "Derived shelter worth {} per piece, {} of {} piece types ",
            "shield-like, royals confined {:?}"
        ),
        shelter_value,
        shield_pieces.iter().filter(|shield| **shield).count(),
        shield_pieces.len(),
        confined
    );

    let statics = state.static_mut();

    statics.shield_pieces = shield_pieces;
    statics.shelter_squares = shelter_squares;
    statics.shelter_counts = shelter_counts;
    statics.local_stride = stride;
    statics.shelter_value = shelter_value as i32;
}
