//! tuning.rs
//!
//! Texel tuning of the evaluation parameters by gradient descent.
//!
//! Reads completed self-play games, splits them into game-disjoint training
//! and validation sets, resolves each position through quiescence, then models
//! changes to that score as a linear function of tunable parameters. Adam
//! minimizes training error while validation selects the exported epoch.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                TUNING CONSTANTS
\*----------------------------------------------------------------------------*/

/// Tuning constants
///
/// The numbers the fit itself is run with, none of which come from the
/// variant. Adam's three are its published defaults, and the rest are
/// decisions about how hard to look and when to stop looking.
///
/// - `ADAM_BETA_ONE` : 0.9, decays the running mean of the gradient
/// - `ADAM_BETA_TWO` : 0.999, decays the running mean of its square
/// - `ADAM_EPSILON`  : 1e-8, floors the divisor against a zero step
///
/// K is what turns a score into a win probability, and it is searched for
/// rather than assumed. A variant whose pieces are written down ten times
/// larger than ours would otherwise read as ten times as decided, and the
/// gradient would spend itself flattening the sigmoid instead of moving
/// the parameters. Thirty-two golden-section steps cut the range below a
/// millionth of its width, finer than any dataset can tell apart.
///
/// - `TEXEL_K_MIN`        : 0.01, the flattest sigmoid considered
/// - `TEXEL_K_MAX`        : 3.0, the steepest, past anything a fit
///                          has wanted
/// - `TEXEL_K_ITERATIONS` : 32, how many steps are taken between them
///
/// One game in five is held out, whole. Positions from one game are very
/// nearly the same position, so holding out positions would let the
/// validation set grade an answer it had already been shown. Ten epochs
/// with no new best is taken as the fit having finished.
///
/// - `TUNING_VALIDATION_MODULUS`  : 5, holds out game IDs divisible by it
/// - `TUNING_VALIDATION_PATIENCE` : 10, how many epochs may pass without
///                                  a better one before the fit stops
const ADAM_BETA_ONE: f64 = 0.9;
const ADAM_BETA_TWO: f64 = 0.999;
const ADAM_EPSILON: f64 = 1e-8;

const TEXEL_K_MIN: f64 = 0.01;
const TEXEL_K_MAX: f64 = 3.0;
const TEXEL_K_ITERATIONS: usize = 32;
const TUNING_VALIDATION_MODULUS: u64 = 5;
const TUNING_VALIDATION_PATIENCE: usize = 10;

/*----------------------------------------------------------------------------*\
                                PARAMETER VECTOR
\*----------------------------------------------------------------------------*/

/// TuneShape
///
/// Where each tunable number lives in one long vector. The evaluation keeps
/// its parameters as pieces, tables, and named scalars, while a gradient
/// wants a single flat θ. This is the map between the two, measured off the
/// loaded variant rather than assumed, since the piece-type count T and the
/// board size S are different in every variant.
///
/// - `[0, T)`             : opening material, one per White piece type
/// - `[T, 2T)`            : endgame material, in that same order
/// - `[2T, 2T+T·S)`       : the opening PST, T tables of S squares
/// - `[2T+T·S, 2T+2·T·S)` : the endgame PST, the same again
/// - `[2T+2·T·S, D)`      : the eleven evaluation scalars
///
/// Only White's piece types are counted. Black's parameters are not free to
/// move on their own: a Black piece is tuned through the White piece it is
/// paired with, on the White square mirrored across the ranks.
struct TuneShape {
    pairs: Vec<(usize, usize)>,                                                 /* (white index, black index) per type*/
    piece_types: usize,                                                         /* number of White piece types (T)    */
    board_size: usize,                                                          /* squares per board (S)              */
    files: usize,                                                               /* board width, for PST mirroring     */
    ranks: usize,                                                               /* board height, for PST mirroring    */
    opening_material_base: usize,                                               /* offset of opening material block   */
    endgame_material_base: usize,                                               /* offset of endgame material block   */
    opening_pst_base: usize,                                                    /* offset of opening PST block        */
    endgame_pst_base: usize,                                                    /* offset of endgame PST block        */
    scalar_base: usize,                                                         /* evaluation scalar block            */
    dimension: usize,                                                           /* total tunable parameter count (D)  */
}

/// Sample
///
/// One dataset position, cut down to the only thing the fit reads: a
/// straight line. Every tunable parameter enters the tapered score
/// multiplied by something the position fixes — a net piece count, a phase
/// weight, a plus or minus one — so the score is exact, not approximated:
///
/// ```text
/// score(θ) = offset + Σ coefficient · θ[index]
/// ```
///
/// The offset is what quiescence said, less what the tunable terms are
/// worth at the parameters it was said under. Everything the tuner has no
/// parameter for stays inside it, along with whatever the captures that
/// quiescence resolved were worth, so the fit is correcting an evaluation
/// rather than writing one from nothing.
///
/// Notes:
///
/// The coefficients are taken once, at the phase the position was in, and
/// never retaken. A step large enough to move a position from middlegame
/// into endgame goes unnoticed until the next run. That is the price of
/// the model staying linear, and of an epoch costing a dot product rather
/// than a search.
struct Sample {
    features: Vec<(usize, f64)>,                                                /* sparse ∂score/∂θ coefficients      */
    offset: f64,                                                                /* qsearch score outside tuned terms  */
    label: f64,                                                                 /* White-view game result             */
}

/// TuneDataset
///
/// The dataset after it has been split, and the split holds for the whole
/// run. Training moves the parameters and fixes the scaling constant;
/// validation only ever grades them, and is the reason a run can tell
/// having learned something from having memorised the games it was given.
///
/// The two halves are separated by game, never by position, so no held-out
/// position is graded by a set that has already been shown the position
/// standing one move before it.
struct TuneDataset {
    training: Vec<Sample>,                                                      /* four games in five                 */
    validation: Vec<Sample>,                                                    /* the fifth, kept whole              */
}

/// build_shape
///
/// Measures the loaded variant and lays the blocks out end to end. None of
/// it is a constant of the engine: the piece types come from the variant's
/// own pairing of White pieces with their Black counterparts, and the block
/// sizes from the board it is played on, so a nine-by-ten board with seven
/// piece types builds a vector nothing like standard chess's.
///
/// The eleven scalars are put last, where a twelfth could be added without
/// moving anything already indexed.
///
/// Params:
/// - state: &State -> loaded variant whose geometry is measured
///
/// Return:
/// TuneShape       -> block offsets, dimensions, and the piece-type pairs
fn build_shape(state: &State) -> TuneShape {
    let pairs = collect_piece_type_pairs(state);
    let piece_types = pairs.len();
    let board_size = state.statics.board_size;

    let opening_material_base = 0;
    let endgame_material_base = piece_types;
    let opening_pst_base = 2 * piece_types;
    let endgame_pst_base = opening_pst_base + piece_types * board_size;
    let scalar_base = endgame_pst_base + piece_types * board_size;
    let dimension = scalar_base + 11;

    TuneShape {
        pairs,
        piece_types,
        board_size,
        files: state.statics.files as usize,
        ranks: state.statics.ranks as usize,
        opening_material_base,
        endgame_material_base,
        opening_pst_base,
        endgame_pst_base,
        scalar_base,
        dimension,
    }
}

/// initial_theta
///
/// Fills the vector with the parameters the engine is playing with right
/// now, so a run picks up where the last one left off rather than at zero.
/// From zero the fit would have to rediscover from game results alone that
/// a queen outweighs a pawn, which no dataset this size can say.
///
/// The scalars are read in this order, and every later index into the
/// scalar block counts from the same list:
///
/// - `+0`  : tempo_bonus
/// - `+1`  : imbalance_major
/// - `+2`  : imbalance_minor
/// - `+3`  : pair_bonus
/// - `+4`  : shelter_value
/// - `+5`  : guard_value
/// - `+6`  : castled_value
/// - `+7`  : castling_right_value
/// - `+8`  : king_danger_scale
/// - `+9`  : king_danger_cap
/// - `+10` : open_shield_penalty
///
/// Params:
/// - state: &State     -> loaded variant supplying current parameters
/// - shape: &TuneShape -> vector geometry to fill
///
/// Return:
/// Vec<f64>            -> the starting parameter vector θ₀
fn initial_theta(state: &State, shape: &TuneShape) -> Vec<f64> {
    let mut theta = vec![0.0f64; shape.dimension];
    let board_size = shape.board_size;

    for (type_index, (white_index, _)) in shape.pairs.iter().enumerate() {
        let piece = &state.statics.pieces[*white_index];
        theta[shape.opening_material_base + type_index] =
            p_ovalue!(piece) as f64;
        theta[shape.endgame_material_base + type_index] =
            p_evalue!(piece) as f64;

        for square in 0..board_size {
            let offset = type_index * board_size + square;
            theta[shape.opening_pst_base + offset] =
                state.statics.pst_opening[*white_index][square] as f64;
            theta[shape.endgame_pst_base + offset] =
                state.statics.pst_endgame[*white_index][square] as f64;
        }
    }

    let eval = &state.statics.eval;
    for (index, value) in [
        eval.tempo_bonus, eval.imbalance_major,
        eval.imbalance_minor, eval.pair_bonus,
        eval.shelter_value, eval.guard_value,
        eval.castled_value, eval.castling_right_value,
        eval.king_danger_scale, eval.king_danger_cap,
        eval.open_shield_penalty,
    ].iter().enumerate() {
        theta[shape.scalar_base + index] = *value as f64;
    }

    theta
}

/*----------------------------------------------------------------------------*\
                               FEATURE EXTRACTION
\*----------------------------------------------------------------------------*/

/// phase_weights
///
/// How much of each side of a tapered term this position is owed. The
/// split is the one `evaluate_position!` already makes, repeated here
/// because the tuner needs it as a pair of numbers rather than as a score:
///
/// - setup, opening : `(1, 0)`, the opening term whole
/// - middlegame     : `(w, 1 − w)`, for a weight `w` of
///                    `(phase − endgame) / (opening − endgame)`
/// - endgame        : `(0, 1)`, the endgame term whole
///
/// A variant whose opening and endgame phase scores are the same number
/// has no scale to interpolate along, and is split evenly rather than
/// divided by zero.
///
/// Notes:
///
/// The weights are read once, when the sample is extracted, and are not
/// touched again. Tuning material changes what a position's phase score
/// comes to, but the fit works with the weights it started with, which is
/// what keeps the model linear.
///
/// Params:
/// - state: &State -> position whose phase determines the weights
///
/// Return:
/// (f64, f64)      -> (opening weight, endgame weight), summing to one
fn phase_weights(state: &State) -> (f64, f64) {
    match state.game_phase {
        ENDGAME => (0.0, 1.0),
        MIDDLEGAME => {
            let opening = state.statics.opening_score as f64;
            let endgame = state.statics.endgame_score as f64;
            let current = state.phase_score as f64;
            let denominator = opening - endgame;

            if denominator == 0.0 {
                (0.5, 0.5)
            } else {
                let opening_weight = (current - endgame) / denominator;
                (opening_weight, 1.0 - opening_weight)
            }
        }
        _ => (1.0, 0.0),
    }
}

/// extract_sample
///
/// Turns one quiet position into the row of coefficients saying how its
/// score would move if each parameter moved. The tapered material and PST
/// score is linear in those parameters, so the coefficients are read off
/// the position rather than estimated from it:
///
/// - material : the net count of that type, times the phase weight
/// - PST      : the phase weight, at the square the piece stands on
/// - scalar   : the count its term was built from, opening-weighted
///
/// Pieces in hand are counted as material. A variant that drops them has
/// them worth something while they wait, and a tuner that ignored the
/// pocket would price a crazyhouse capture as a piece disappearing.
///
/// Black is not tuned separately. Its pieces enter their White partner's
/// entry with the sign flipped, on the square mirrored across the ranks:
///
/// - White on square 1  : gives `PST[1]` a coefficient of `+weight`
/// - Black on square 57 : that same file on rank 7, on eight files, so
///                        `PST[1]` is given `−weight`
///
/// A scalar's count is recovered by dividing the score that term produced
/// by the parameter that produced it. King danger is the one term that is
/// not a plain product: once it has hit its cap, moving the scale changes
/// nothing, so the coefficient goes to the cap instead.
///
/// Finally the offset is set to what quiescence said less what these
/// coefficients are worth at the current parameters, so the line passes
/// exactly through the score the engine actually gives this position.
///
/// Notes:
///
/// A scalar standing at zero cannot be divided back out and reads as a
/// count of zero. It then draws no gradient and stays at zero for the whole
/// run, which is worth knowing before tuning a variant whose configuration
/// has switched such a term off.
///
/// Params:
/// - state: &State     -> quiet position to reduce
/// - shape: &TuneShape -> vector geometry to index into
/// - label: f64        -> White-view game result for this position
/// - score: i32        -> White-view quiescence score
/// - theta: &[f64]     -> current values used for the tunable score
///
/// Return:
/// Sample              -> sparse features, qsearch offset, and label
fn extract_sample(
    state: &State,
    shape: &TuneShape,
    label: f64,
    score: i32,
    theta: &[f64],
) -> Sample {
    let (opening_weight, endgame_weight) = phase_weights(state);
    let board_size = shape.board_size;
    let mut features: Vec<(usize, f64)> = Vec::new();

    for (type_index, (white_index, black_index)) in
        shape.pairs.iter().enumerate()
    {
        let white_hand =
            state.piece_in_hand[WHITE as usize][*white_index] as f64;
        let black_hand =
            state.piece_in_hand[BLACK as usize][*black_index] as f64;
        let white_count = state.piece_count[*white_index] as f64 + white_hand;
        let black_count = state.piece_count[*black_index] as f64 + black_hand;
        let net = white_count - black_count;

        if net != 0.0 {
            if opening_weight != 0.0 {
                features.push((
                    shape.opening_material_base + type_index,
                    opening_weight * net,
                ));
            }
            if endgame_weight != 0.0 {
                features.push((
                    shape.endgame_material_base + type_index,
                    endgame_weight * net,
                ));
            }
        }

        for &square in piece_squares!(state, *white_index) {
            let offset = type_index * board_size + square as usize;
            if opening_weight != 0.0 {
                features.push((
                    shape.opening_pst_base + offset, opening_weight,
                ));
            }
            if endgame_weight != 0.0 {
                features.push((
                    shape.endgame_pst_base + offset, endgame_weight,
                ));
            }
        }

        for &square in piece_squares!(state, *black_index) {
            let index = square as usize;
            let mirror = (shape.ranks - 1 - index / shape.files)
                * shape.files + index % shape.files;
            let offset = type_index * board_size + mirror;
            if opening_weight != 0.0 {
                features.push((
                    shape.opening_pst_base + offset, -opening_weight,
                ));
            }
            if endgame_weight != 0.0 {
                features.push((
                    shape.endgame_pst_base + offset, -endgame_weight,
                ));
            }
        }
    }

    let eval = &state.statics.eval;
    let white = WHITE as usize;
    let black = BLACK as usize;
    let unit = |score: i32, value: i32| {
        if value == 0 { 0.0 } else { score as f64 / value as f64 }
    };
    let shelter = unit(royal_shelter!(state, white), eval.shelter_value)
        - unit(royal_shelter!(state, black), eval.shelter_value);
    let guard = unit(royal_guard!(state, white), eval.guard_value)
        - unit(royal_guard!(state, black), eval.guard_value);
    let rights = [WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE];
    let castled = |color: usize| {
        (castling!(state)
            && state.castling_state & (CASTLED << color) != 0) as u8 as f64
    };
    let holds = |color: usize| {
        (castling!(state)
            && state.castling_state & (CASTLED << color) == 0
            && state.castling_state & rights[color] != 0) as u8 as f64
    };
    let danger = |color: usize| {
        let score = king_danger!(state, color);
        if score > 0 && score == eval.king_danger_cap {
            (0.0, 1.0)
        } else {
            (unit(score, eval.king_danger_scale), 0.0)
        }
    };
    let white_danger = danger(white);
    let black_danger = danger(black);
    let open_shield = unit(
        open_shield!(state, black), eval.open_shield_penalty,
    ) - unit(open_shield!(state, white), eval.open_shield_penalty);
    let major = state.major_pieces[white] as f64
        - state.major_pieces[black] as f64;
    let minor = state.minor_pieces[white] as f64
        - state.minor_pieces[black] as f64;
    let mut pairs = 0.0;

    for index in &eval.pair_pieces {
        let color = p_color!(&state.statics.pieces[*index]) as f64;
        pairs += (-2.0 * color + 1.0)
            * (state.piece_count[*index] >= 2) as u8 as f64;
    }

    let opening = opening_weight;
    features.push((shape.scalar_base,
        -2.0 * state.playing as f64 + 1.0));
    features.push((shape.scalar_base + 1, major));
    features.push((shape.scalar_base + 2, minor));
    features.push((shape.scalar_base + 3, pairs));
    features.push((shape.scalar_base + 4, opening * shelter));
    features.push((shape.scalar_base + 5, opening * guard));
    features.push((shape.scalar_base + 6,
        opening * (castled(white) - castled(black))));
    features.push((shape.scalar_base + 7,
        opening * (holds(white) - holds(black))));
    features.push((shape.scalar_base + 8,
        opening * (black_danger.0 - white_danger.0)));
    features.push((shape.scalar_base + 9,
        opening * (black_danger.1 - white_danger.1)));
    features.push((shape.scalar_base + 10, opening * open_shield));

    let tuned = features.iter()
        .map(|(index, coeff)| theta[*index] * coeff).sum::<f64>();

    Sample { features, offset: score as f64 - tuned, label }
}

/*----------------------------------------------------------------------------*\
                               ERROR AND GRADIENT
\*----------------------------------------------------------------------------*/

/// Tuning math primitives
///
/// The three steps between a sample and a number the fit can act on. The
/// line is evaluated, bent into a win probability, and compared with how
/// the game the position came from actually ended:
///
/// ```text
/// score    = offset + f·θ
/// expected = 1 / (1 + 10^(−K·score/400))
/// error    = mean of (label − expected)² over the samples
/// ```
///
/// The logistic is base ten over four hundred, which puts K near one when
/// a pawn is worth about a hundred. That is a convenience rather than an
/// assumption: K is fitted, so a variant that writes its values on a wholly
/// different scale simply fits a different K.
///
/// Only the mean is spread across cores. It is the one call taking time
/// here, and both the scaling search and every epoch go through it.
///
/// sigmoid
///
///   Params:
///   - value: f64 -> logit input
///
///   Return:
///   f64          -> the base-ten logistic `1/(1+10⁻ˣ)`
///
/// model_score
///
///   Params:
///   - sample: &Sample -> position to score
///   - theta : &[f64]  -> parameter vector
///
///   Return:
///   f64               -> quiescence offset plus tunable score `o + f·θ`
///
/// mean_squared_error
///
///   Params:
///   - samples: &[Sample] -> dataset
///   - theta  : &[f64]    -> parameter vector
///   - scaling: f64       -> the sigmoid scaling constant K
///
///   Return:
///   f64                  -> average `(label − sigmoid(K·score/400))²`
fn sigmoid(value: f64) -> f64 {
    1.0 / (1.0 + 10f64.powf(-value))
}

fn model_score(sample: &Sample, theta: &[f64]) -> f64 {
    sample.offset + sample.features.iter()
        .map(|(index, coeff)| theta[*index] * coeff).sum::<f64>()
}

fn mean_squared_error(samples: &[Sample], theta: &[f64], scaling: f64) -> f64 {
    let total: f64 = samples
        .par_iter()
        .map(|sample| {
            let score = model_score(sample, theta);
            let expected = sigmoid(scaling * score / 400.0);
            let error = sample.label - expected;
            error * error
        })
        .sum();

    total / samples.len() as f64
}

/// fit_scaling
///
/// Finds the K under which the current parameters look as right as they
/// can, before a single step is taken. Left unfitted, the first epochs
/// would go on scaling the whole evaluation up or down to meet a sigmoid
/// that was never the thing worth arguing with.
///
/// Error against K falls towards one minimum and rises on both sides of
/// it, which is all golden-section search asks for. Two interior probes are
/// held, the half beyond the worse of them is dropped, and the better probe
/// survives as one end of the next pair, so a step costs one pass over the
/// training set instead of two:
///
/// ```text
/// low        left        right        high
///  ├──────────┼────────────┼───────────┤
///             ^ lower error, so [right, high] goes
/// ```
///
/// Thirty-two of those leave an interval far narrower than the dataset
/// could tell apart, and its midpoint is the answer. K is fitted on the
/// training samples alone, like everything else the run learns.
///
/// Params:
/// - samples: &[Sample] -> dataset
/// - theta  : &[f64]    -> parameter vector to evaluate against
///
/// Return:
/// f64                  -> the fitted scaling constant K
fn fit_scaling(samples: &[Sample], theta: &[f64]) -> f64 {
    let ratio = (5f64.sqrt() - 1.0) / 2.0;
    let mut low = TEXEL_K_MIN;
    let mut high = TEXEL_K_MAX;

    let mut left = high - ratio * (high - low);
    let mut right = low + ratio * (high - low);
    let mut left_error = mean_squared_error(samples, theta, left);
    let mut right_error = mean_squared_error(samples, theta, right);

    for _ in 0..TEXEL_K_ITERATIONS {
        if left_error < right_error {
            high = right;
            right = left;
            right_error = left_error;
            left = high - ratio * (high - low);
            left_error = mean_squared_error(samples, theta, left);
        } else {
            low = left;
            left = right;
            left_error = right_error;
            right = low + ratio * (high - low);
            right_error = mean_squared_error(samples, theta, right);
        }
    }

    (low + high) / 2.0
}

/// compute_gradient
///
/// Says which way every parameter wants to move. Because the model is a
/// line inside a logistic, the chain rule hands the answer over in closed
/// form, with no finite differences and no second search:
///
/// ```text
/// ∂E/∂θᵢ = 2(E − r) · E(1 − E) · (K·ln10/400) · fᵢ
/// ```
///
/// - `2(E − r)`   : how wrong the prediction was, and on which side
/// - `E(1 − E)`   : how much the sigmoid can still move here at all
/// - `K·ln10/400` : the constant the score was squeezed through
/// - `fᵢ`         : what this position said that parameter is worth
///
/// The middle factor is why a position the fit is already sure about
/// pulls almost nothing: a prediction near zero or one has run out of
/// sigmoid to move, and the run stops arguing over settled positions.
///
/// Each thread keeps a full-length accumulator of its own and they are
/// added at the end. The features are sparse, but two samples can name the
/// same parameter, and a shared vector would need a lock per feature to
/// say so.
///
/// Params:
/// - samples: &[Sample] -> dataset
/// - theta  : &[f64]    -> current parameters
/// - scaling: f64       -> the sigmoid scaling constant K
///
/// Return:
/// Vec<f64>             -> the averaged gradient, one entry per parameter
fn compute_gradient(
    samples: &[Sample],
    theta: &[f64],
    scaling: f64,
) -> Vec<f64> {
    let dimension = theta.len();
    let slope = scaling * 10f64.ln() / 400.0;

    let summed = samples
        .par_iter()
        .fold(
            || vec![0.0f64; dimension],
            |mut accumulator, sample| {
                let score = model_score(sample, theta);
                let expected = sigmoid(scaling * score / 400.0);
                let factor = 2.0
                    * (expected - sample.label)
                    * expected
                    * (1.0 - expected)
                    * slope;

                for (index, coeff) in &sample.features {
                    accumulator[*index] += factor * coeff;
                }
                accumulator
            },
        )
        .reduce(
            || vec![0.0f64; dimension],
            |mut left, right| {
                for index in 0..dimension {
                    left[index] += right[index];
                }
                left
            },
        );

    let count = samples.len() as f64;
    summed.iter().map(|value| value / count).collect()
}

/*----------------------------------------------------------------------------*\
                                DATASET LOADING
\*----------------------------------------------------------------------------*/

/// load_dataset
///
/// Reads back what datagen wrote and turns each row into a sample. A row is
/// three fields, and the first of them decides which half it lands in:
///
/// ```text
/// 12;8/8/4k3/8/4K3/8/8/8 w - - 0 1;0.5
/// ^  ^                             ^
/// |  |                             how that game ended, White's view
/// |  the position, as a FEN
/// the game, and every row sharing it goes the same way
/// ```
///
/// Each position is put through quiescence before it is measured. The rows
/// are quiet already — nobody in check, nothing captured — but quiet is not
/// the same as settled, and a score with a hanging piece still standing on
/// the board is a score the parameters would be blamed for. The result is
/// turned to White's view so that scores and labels agree on which way up
/// they are.
///
/// A row that cannot be read stops the run and names its line, as does a
/// game whose rows disagree about how it ended. A dataset half-read in
/// silence would fit something, and would look like a fit that merely went
/// badly. A dataset that is missing entirely is not fatal here: it logs and
/// returns nothing, and the caller says so.
///
/// The split, the result counts, and the phase counts are all logged before
/// any fitting starts. A dataset that is nine parts draws, or that never
/// reached an endgame, will still tune — this is where that shows.
///
/// Params:
/// - template: &State     -> loaded variant to clone scratch states
/// - variant : &str       -> variant name, selects the dataset file
/// - shape   : &TuneShape -> vector geometry for feature extraction
/// - theta   : &[f64]     -> current parameters behind qsearch and features
///
/// Return:
/// TuneDataset            -> game-disjoint training and validation samples
fn load_dataset(
    template: &State,
    variant: &str,
    shape: &TuneShape,
    theta: &[f64],
) -> TuneDataset {
    let path = format!("{}/{}/latest.data", DATA_DIR, variant);

    let content = match fs::read_to_string(&path) {
        Ok(content) => content,
        Err(error) => {
            log_2!("Cannot read dataset {}: {}", path, error);
            return TuneDataset {
                training: Vec::new(),
                validation: Vec::new(),
            };
        }
    };

    let mut scratch = template.clone();
    let ttable = TTable::with_mb(1);
    let qtable = QTable::with_mb(1);
    let mut info = SearchInfo::default();
    clear_search(&mut scratch, &ttable, &qtable, &mut info);
    let mut training = Vec::new();
    let mut validation = Vec::new();
    let mut game_results = HashMap::new();
    let mut phases = [0usize; 4];

    for (line_index, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let mut fields = trimmed.splitn(3, ';');
        let game_text = fields.next().unwrap_or_default();
        let fen = fields.next().unwrap_or_default();
        let result_text = fields.next().unwrap_or_default();
        assert!(
            !game_text.is_empty() && !fen.is_empty() && !result_text.is_empty(),
            "Invalid dataset row {}: expected game;FEN;result",
            line_index + 1,
        );

        let game_id = game_text.parse::<u64>().unwrap_or_else(|_| {
            panic!("Invalid game ID on dataset row {}", line_index + 1)
        });
        let label = result_text.parse::<f64>().unwrap_or_else(|_| {
            panic!("Invalid result on dataset row {}", line_index + 1)
        });
        assert!(
            label == 0.0 || label == 0.5 || label == 1.0,
            "Invalid result on dataset row {}: {}",
            line_index + 1, label,
        );

        if let Some(previous) = game_results.insert(game_id, label) {
            assert_eq!(
                previous, label,
                "Conflicting results for dataset game {}", game_id,
            );
        }

        scratch.reset();
        parse_fen(&mut scratch, fen, None).unwrap_or_else(|error| {
            panic!(
                "Invalid FEN on dataset row {}: {}",
                line_index + 1,
                error,
            )
        });
        refresh_eval_state(&mut scratch);

        let phase_index = match scratch.game_phase {
            SETUP => 0,
            OPENING => 1,
            MIDDLEGAME => 2,
            ENDGAME => 3,
            _ => unreachable!(),
        };
        phases[phase_index] += 1;

        info.nodes = 0;
        info.interrupt = false;
        let score = quiescence_search(
            &mut scratch, &ttable, &qtable, -INF, INF, &mut info,
        ) * (-2 * scratch.playing as i32 + 1);
        let sample = extract_sample(&scratch, shape, label, score, theta);
        if game_id % TUNING_VALIDATION_MODULUS == 0 {
            validation.push(sample);
        } else {
            training.push(sample);
        }
    }

    let mut results = [0usize; 3];
    let mut training_games = 0usize;
    let mut validation_games = 0usize;

    for (game_id, label) in game_results {
        if game_id % TUNING_VALIDATION_MODULUS == 0 {
            validation_games += 1;
        } else {
            training_games += 1;
        }

        if label == 0.0 {
            results[0] += 1;
        } else if label == 0.5 {
            results[1] += 1;
        } else {
            results[2] += 1;
        }
    }

    log_1!(
        concat!(
            "Tune split: {} train games/{} positions, ",
            "{} validation games/{} positions"
        ),
        training_games, training.len(), validation_games, validation.len(),
    );
    log_1!(
        "Tune results: {} losses, {} draws, {} wins",
        results[0], results[1], results[2],
    );
    log_1!(
        "Tune phases: {} setup, {} opening, {} middlegame, {} endgame",
        phases[0], phases[1], phases[2], phases[3],
    );

    TuneDataset { training, validation }
}

/*----------------------------------------------------------------------------*\
                                PARAMETER EXPORT
\*----------------------------------------------------------------------------*/

/// export_theta
///
/// Writes the fitted vector back out as the parameters the engine reads at
/// startup. The floats become integers here, which is where the fit stops
/// being exact: everything downstream of this is fixed point, and a piece
/// worth 331.6 is a piece worth 332.
///
/// The written order is not θ's order. Material leads, both phases of it,
/// and after that each piece type's two tables are kept together:
///
/// - T tokens of opening material, one per piece type
/// - T tokens of endgame material, in that same order
/// - then per piece type, its S opening squares followed by its S
///   endgame squares
/// - 11 tokens for the evaluation scalars
///
/// Values are clamped to what the parameter parser will accept, fourteen
/// bits either side of zero, and material to fourteen bits above it —
/// a piece worth less than nothing is a piece the side of the board wants
/// captured, which is not a thing the tuner is allowed to conclude.
///
/// The tokens are then parsed straight back into the running state before
/// the file is written. Reading its own output is what makes the exported
/// file and the engine that produced it agree by construction, rather than
/// by both being written carefully.
///
/// Params:
/// - state  : &mut State -> loaded variant, updated with the tuned vector
/// - variant: &str       -> variant name, selects the output directory
/// - shape  : &TuneShape -> vector geometry to serialise from
/// - theta  : &[f64]     -> the tuned parameter vector
fn export_theta(
    state: &mut State,
    variant: &str,
    shape: &TuneShape,
    theta: &[f64],
) {
    let board_size = shape.board_size;
    let rounded = |value: f64| value.round() as i32;
    let material = |value: f64| rounded(value).clamp(0, 0x3FFF) as u16;
    let mut opening_material = Vec::with_capacity(shape.piece_types);
    let mut endgame_material = Vec::with_capacity(shape.piece_types);

    for type_index in 0..shape.piece_types {
        opening_material.push(
            material(theta[shape.opening_material_base + type_index])
        );
        endgame_material.push(
            material(theta[shape.endgame_material_base + type_index])
        );
    }

    for (type_index, (white_index, black_index)) in
        shape.pairs.iter().copied().enumerate()
    {
        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[white_index],
            opening_material[type_index],
            endgame_material[type_index],
            false,
            false,
        );
        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[black_index],
            opening_material[type_index],
            endgame_material[type_index],
            false,
            false,
        );
    }

    derive_eval_products(state);

    let mut tokens: Vec<String> = opening_material
        .iter()
        .map(ToString::to_string)
        .collect();
    tokens.extend(endgame_material.iter().map(ToString::to_string));

    for type_index in 0..shape.piece_types {
        for square in 0..board_size {
            let offset = type_index * board_size + square;
            let target = rounded(theta[shape.opening_pst_base + offset])
                .clamp(-0x3FFF, 0x3FFF);
            tokens.push(target.to_string());
        }
        for square in 0..board_size {
            let offset = type_index * board_size + square;
            let target = rounded(theta[shape.endgame_pst_base + offset])
                .clamp(-0x3FFF, 0x3FFF);
            tokens.push(target.to_string());
        }
    }

    for index in shape.scalar_base..shape.dimension {
        tokens.push(
            rounded(theta[index]).clamp(-0x3FFF, 0x3FFF).to_string()
        );
    }

    parse_tuned_parameters(state, &tokens.join(" "));
    export_tuned_parameters_file(state, variant);
}

/*----------------------------------------------------------------------------*\
                                  TUNING LOOP
\*----------------------------------------------------------------------------*/

/// run_tuning
///
/// What `tune` runs. The vector is measured and filled from the parameters
/// in force, the dataset is read and split, K is fitted once, and then the
/// same four things happen every epoch:
///
/// - the gradient is taken, over the training half only
/// - an Adam step is applied, both moments corrected for their cold start
/// - the vector is clamped back into the range the parameter parser
///   accepts
/// - both halves are scored, and this θ is kept if validation improved
///
/// Clamping inside the loop rather than at the end is what keeps the run
/// honest: a parameter left free to wander outside the range would go on
/// earning gradient it could never spend, and the vector that was scored
/// would not be the vector that could be written.
///
/// What gets exported is the best validation epoch, never the last one.
/// Training error falls for as long as anyone is willing to watch it, and
/// past some point it falls by learning the games rather than the game.
/// Ten epochs without a new best is taken as that point.
///
/// A run stopped by hand still exports. It has a best epoch by then, and
/// throwing that away because the run was cut short would only make the
/// interrupt cost more than it saves.
///
/// Notes:
///
/// K is fitted once, against the starting parameters, and is not refitted
/// as they move. Refitting each epoch would let the error fall by making
/// the sigmoid flatter rather than by making the evaluation better, and
/// the two halves' scores would no longer be comparable across epochs.
///
/// Params:
/// - state        : &mut State -> loaded variant, tuned and exported
/// - variant      : &str       -> variant name, selects dataset/output
/// - epochs       : usize      -> number of Adam passes to run
/// - learning_rate: f64        -> Adam step size
pub fn run_tuning(
    state: &mut State,
    variant: &str,
    epochs: usize,
    learning_rate: f64,
) {
    let shape = build_shape(state);
    let mut theta = initial_theta(state, &shape);

    let dataset = load_dataset(state, variant, &shape, &theta);
    if dataset.training.is_empty() || dataset.validation.is_empty() {
        log_2!("Training and validation samples are both required");
        return;
    }

    let scaling = fit_scaling(&dataset.training, &theta);
    let start_training = mean_squared_error(
        &dataset.training, &theta, scaling
    );
    let start_validation = mean_squared_error(
        &dataset.validation, &theta, scaling
    );
    log_1!(
        "Tune: K {:.4}, train MSE {:.6}, validation MSE {:.6}",
        scaling, start_training, start_validation,
    );

    let mut first_moment = vec![0.0f64; shape.dimension];
    let mut second_moment = vec![0.0f64; shape.dimension];
    let mut best_theta = theta.clone();
    let mut best_validation = start_validation;
    let mut best_epoch = 0usize;
    let mut stale_epochs = 0usize;

    for epoch in 1..=epochs {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            log_2!("Tune interrupted after {} epochs", epoch - 1);
            break;
        }

        let gradient = compute_gradient(&dataset.training, &theta, scaling);

        let bias_one = 1.0 - ADAM_BETA_ONE.powi(epoch as i32);
        let bias_two = 1.0 - ADAM_BETA_TWO.powi(epoch as i32);

        for index in 0..shape.dimension {
            first_moment[index] = ADAM_BETA_ONE * first_moment[index]
                + (1.0 - ADAM_BETA_ONE) * gradient[index];
            second_moment[index] = ADAM_BETA_TWO * second_moment[index]
                + (1.0 - ADAM_BETA_TWO) * gradient[index] * gradient[index];

            let corrected_first = first_moment[index] / bias_one;
            let corrected_second = second_moment[index] / bias_two;

            theta[index] -= learning_rate * corrected_first
                / (corrected_second.sqrt() + ADAM_EPSILON);
        }

        for type_index in 0..shape.piece_types {
            let opening = shape.opening_material_base + type_index;
            let endgame = shape.endgame_material_base + type_index;

            theta[opening] = theta[opening].clamp(0.0, 0x3FFF as f64);
            theta[endgame] = theta[endgame].clamp(0.0, 0x3FFF as f64);
        }
        for value in &mut theta[shape.opening_pst_base..shape.scalar_base] {
            *value = value.clamp(-0x3FFF as f64, 0x3FFF as f64);
        }
        for value in &mut theta[shape.scalar_base..] {
            *value = value.clamp(-0x3FFF as f64, 0x3FFF as f64);
        }

        let training_error = mean_squared_error(
            &dataset.training, &theta, scaling
        );
        let validation_error = mean_squared_error(
            &dataset.validation, &theta, scaling
        );
        log_1!(
            "Tune epoch {}/{}: train {:.6}, validation {:.6}",
            epoch, epochs, training_error, validation_error,
        );

        if validation_error < best_validation {
            best_theta.clone_from(&theta);
            best_validation = validation_error;
            best_epoch = epoch;
            stale_epochs = 0;
        } else {
            stale_epochs += 1;
            if stale_epochs >= TUNING_VALIDATION_PATIENCE {
                log_2!(
                    "Tune stopped after {} stale validation epochs",
                    stale_epochs,
                );
                break;
            }
        }
    }

    export_theta(state, variant, &shape, &best_theta);
    log_1!(
        "Tune complete: exported epoch {} for {} at validation MSE {:.6}",
        best_epoch, variant, best_validation,
    );
}
