//! tuning.rs
//!
//! Texel tuning of the evaluation parameters by gradient descent.
//!
//! The tuner reads self-play games and puts full games in a training set or
//! a validation set. It scores each position with quiescence and models the
//! score as a linear function of the parameters. Adam decreases the training
//! error, and the best validation epoch is exported.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                TUNING CONSTANTS
\*----------------------------------------------------------------------------*/

/// Tuning constants
///
/// Constants of the fit. They do not depend on the variant. The Adam
/// values are the published defaults.
///
/// - `ADAM_BETA_ONE`              : 0.9, decay of the gradient mean
/// - `ADAM_BETA_TWO`              : 0.999, decay of the squared mean
/// - `ADAM_EPSILON`               : 1e-8, minimum of the divisor
/// - `TEXEL_K_MIN`                : 0.01, flattest sigmoid to try
/// - `TEXEL_K_MAX`                : 3.0, steepest sigmoid to try
/// - `TEXEL_K_ITERATIONS`         : 32, golden-section steps for K
/// - `TUNING_VALIDATION_MODULUS`  : 5, game IDs divisible by it validate
/// - `TUNING_VALIDATION_PATIENCE` : 10, epochs without a better result
///
/// Notes:
/// The tuner searches K, because each variant has its own value scale. 32
/// steps make the K range less than a millionth of its width. Full games
/// go to validation, because the positions of one game are almost equal.
///
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
/// The index map from the evaluation parameters to one flat vector θ. The
/// piece type count T and the board size S come from the loaded variant.
///
/// - `[0, T)`             : opening material, one for each White piece type
/// - `[T, 2T)`            : endgame material, in the same order
/// - `[2T, 2T+T·S)`       : opening PST, T tables of S squares
/// - `[2T+T·S, 2T+2·T·S)` : endgame PST, the same again
/// - `[2T+2·T·S, D)`      : the eleven evaluation scalars
///
/// Only White piece types have entries. A Black piece uses the entry of its
/// White pair, on the square mirrored across the ranks.
///
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
/// One dataset position as a linear function of θ. Each parameter adds to
/// the tapered score times a value that the position fixes, for example a
/// piece count or a phase weight. Thus the model is exact:
///
/// ```text
/// score(θ) = offset + Σ coefficient · θ[index]
/// ```
///
/// The offset is the quiescence score minus the value of the tuned terms.
/// It keeps all terms without a parameter and the quiescence captures.
///
/// Notes:
/// The coefficients use the phase at extraction and do not change. A step
/// that changes the phase has no effect until the next run. This keeps the
/// model linear, and an epoch is a dot product, not a search.
///
struct Sample {
    features: Vec<(usize, f64)>,                                                /* sparse ∂score/∂θ coefficients      */
    offset: f64,                                                                /* qsearch score outside tuned terms  */
    label: f64,                                                                 /* White-view game result             */
}

/// TuneDataset
///
/// The dataset after the split. The split does not change during the run.
///
/// - training   : changes the parameters and fits K
/// - validation : only scores the parameters, finds overfitting
///
/// The split is by game, not by position.
///
struct TuneDataset {
    training: Vec<Sample>,                                                      /* four games in five                 */
    validation: Vec<Sample>,                                                    /* the fifth, kept whole              */
}

/// build_shape
///
/// Measures the loaded variant and puts the blocks one after the other.
/// The piece type pairs and the board size come from the variant. The
/// eleven scalars are last, so a new scalar does not move other indices.
///
/// Params:
/// - state: &State -> loaded variant to measure
///
/// Return:
/// TuneShape       -> block offsets, dimensions and piece type pairs
///
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
/// Fills the vector with the current engine parameters. Thus a run starts
/// from the last result, not from zero. The scalar block has this order:
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
/// - state: &State     -> loaded variant with the current parameters
/// - shape: &TuneShape -> vector geometry to fill
///
/// Return:
/// Vec<f64>            -> the start parameter vector θ₀
///
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
/// Gives the opening and endgame weights of a position. The split is the
/// same as in `evaluate_position!`, but as two numbers:
///
/// - setup, opening : `(1, 0)`
/// - middlegame     : `(w, 1 - w)`, `w = (phase - end) / (open - end)`
/// - endgame        : `(0, 1)`
///
/// Params:
/// - state: &State -> position with the phase
///
/// Return:
/// (f64, f64)      -> (opening weight, endgame weight), sum is one
///
/// Notes:
/// If the two phase bounds are equal, the split is even. The weights do
/// not change after extraction, so the model stays linear.
///
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
/// Converts one quiet position into its coefficient row. The tapered score
/// is linear in the parameters, so the position gives the coefficients:
///
/// - material : net count of the type, times the phase weight
/// - PST      : the phase weight, at the square of the piece
/// - scalar   : the count of the term, times the opening weight
///
/// Pieces in hand count as material. A Black piece uses the entry of its
/// White pair with the opposite sign, on the mirrored square:
///
/// - White on square 1  : `PST[1]` gets `+weight`
/// - Black on square 57 : mirrored to square 1, `PST[1]` gets `-weight`
///
/// The scalar count is the term score divided by its parameter. After its
/// cap, king danger does not change with the scale, so its coefficient is
/// the cap. The offset is the quiescence score minus the tuned terms, so
/// the line goes through the real engine score.
///
/// Params:
/// - state: &mut State -> quiet position, with the attack scratch
/// - shape: &TuneShape -> vector geometry
/// - label: f64        -> game result for White
/// - score: i32        -> quiescence score for White
/// - theta: &[f64]     -> current parameter values
///
/// Return:
/// Sample              -> sparse features, offset and label
///
/// Notes:
/// A scalar at zero cannot be divided and gives a count of zero. It then
/// gets no gradient and stays at zero for the full run.
///
fn extract_sample(
    state: &mut State,
    shape: &TuneShape,
    label: f64,
    score: i32,
    theta: &[f64],
) -> Sample {
    let dangers = king_danger!(state);
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
        let score = dangers[color];
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
/// The three math steps of the fit. The score becomes a win probability,
/// and the error compares it with the game result:
///
/// ```text
/// score    = offset + f·θ
/// expected = 1 / (1 + 10^(−K·score/400))
/// error    = mean of (label − expected)² over the samples
/// ```
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
///   f64               -> offset plus tuned score, `o + f·θ`
///
/// mean_squared_error
///
///   Params:
///   - samples: &[Sample] -> dataset
///   - theta  : &[f64]    -> parameter vector
///   - scaling: f64       -> sigmoid scale K
///
///   Return:
///   f64                  -> mean of `(label − sigmoid(K·score/400))²`
///
/// Notes:
/// With base ten over 400, K is near one when a pawn is about 100. The
/// tuner fits K, so other value scales also work. Only the mean runs in
/// parallel, because it is the slow step.
///
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
/// Finds the K with the smallest error for the current parameters, before
/// the first step. Without it, the first epochs would only scale the full
/// evaluation.
///
/// The error has one minimum in K, so the function uses golden-section
/// search. It removes the part past the worse probe. The better probe
/// stays for the next step, so each step needs one pass, not two:
///
/// ```text
/// low        left        right        high
///  ├──────────┼────────────┼───────────┤
///             ^ lower error, so [right, high] goes
/// ```
///
/// After 32 steps, the midpoint of the interval is the result. Only the
/// training samples fit K.
///
/// Params:
/// - samples: &[Sample] -> dataset
/// - theta  : &[f64]    -> parameter vector
///
/// Return:
/// f64                  -> the fitted scale K
///
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
/// Calculates the gradient of the error. The model is a line in a
/// logistic, so the chain rule gives an exact formula:
///
/// ```text
/// ∂E/∂θᵢ = 2(E − r) · E(1 − E) · (K·ln10/400) · fᵢ
/// ```
///
/// - `2(E − r)`   : prediction error, with its sign
/// - `E(1 − E)`   : slope of the sigmoid, near 0 for sure predictions
/// - `K·ln10/400` : constant of the scale
/// - `fᵢ`         : coefficient of the parameter in this position
///
/// Params:
/// - samples: &[Sample] -> dataset
/// - theta  : &[f64]    -> current parameters
/// - scaling: f64       -> sigmoid scale K
///
/// Return:
/// Vec<f64>             -> mean gradient, one entry for each parameter
///
/// Notes:
/// Each thread has its own full accumulator, and the function adds them at
/// the end. A shared vector would need a lock for each feature.
///
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
/// Reads the datagen file and converts each row into a sample. The game
/// number of a row selects its set:
///
/// ```text
/// 12;8/8/4k3/8/4K3/8/8/8 w - - 0 1;0.5
/// ^  ^                             ^
/// |  |                             game result for White
/// |  the position, as a FEN
/// the game, all its rows go to the same set
/// ```
///
/// Each position gets a quiescence score first. A quiet position can still
/// have a hanging piece. The score is for White, as the label is.
///
/// Params:
/// - template: &State     -> loaded variant, cloned for scratch states
/// - variant : &str       -> variant name, selects the data file
/// - shape   : &TuneShape -> vector geometry for the features
/// - theta   : &[f64]     -> current parameters for quiescence and features
///
/// Return:
/// TuneDataset            -> training and validation samples, split by game
///
/// Notes:
/// A bad row, or a game with two different results, stops the run with the
/// line number. A missing file only logs and gives an empty set. The split,
/// result counts and phase counts go to the log before the fit.
///
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
            &mut scratch, &ttable, &qtable, -INF, INF, &mut info, None, false,
        ) * (-2 * scratch.playing as i32 + 1);
        let sample = extract_sample(&mut scratch, shape, label, score, theta);
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
/// Writes the fitted vector as the parameter file that the engine reads at
/// startup. The values are rounded to integers. The file order is not the
/// θ order:
///
/// - T tokens of opening material, one for each piece type
/// - T tokens of endgame material, in the same order
/// - for each piece type, S opening squares, then S endgame squares
/// - 11 tokens for the evaluation scalars
///
/// Params:
/// - state  : &mut State -> loaded variant, gets the tuned values
/// - variant: &str       -> variant name, selects the output directory
/// - shape  : &TuneShape -> vector geometry
/// - theta  : &[f64]     -> the tuned parameter vector
///
/// Notes:
/// Values are clamped to the parser range, 14 bits on each side of zero.
/// Material is clamped to 0 up to 14 bits. The function parses the tokens
/// into the state before it writes the file, so the file and the engine
/// agree.
///
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
/// The `tune` command. It builds the vector from the current parameters,
/// reads and splits the dataset and fits K once. Then each epoch does:
///
/// 1. calculate the gradient on the training set only
/// 2. apply an Adam step, with bias correction of the two moments
/// 3. clamp the vector to the range of the parameter parser
/// 4. score the two sets, keep θ if the validation error is better
///
/// The export is the best validation epoch, not the last. The run stops
/// after `TUNING_VALIDATION_PATIENCE` epochs without a better result.
///
/// Params:
/// - state        : &mut State -> loaded variant, tuned and exported
/// - variant      : &str       -> variant name, selects data and output
/// - epochs       : usize      -> number of Adam passes
/// - learning_rate: f64        -> Adam step size
///
/// Notes:
/// The clamp is in the loop, so the scored vector is the vector that can
/// be written. An interrupted run still exports its best epoch. K does not
/// change after the first fit. Else the error could decrease with a flatter
/// sigmoid, not with a better evaluation.
///
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
