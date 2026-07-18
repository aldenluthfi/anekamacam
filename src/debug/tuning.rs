//! tuning.rs
//!
//! Texel tuning of the evaluation parameters by gradient descent.
//!
//! Reads completed self-play games, splits them into game-disjoint training
//! and validation sets, and models each quiet position's tapered score as a
//! linear function of tunable parameters. Adam minimizes training error while
//! validation selects the exported epoch.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/// TuneShape
///
/// The fixed geometry of one variant's tunable parameter vector.
///
/// The vector is ordered as opening material, endgame material, opening PST,
/// then endgame PST. The four `*_base` fields are the offsets for those blocks.
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
    dimension: usize,                                                           /* total tunable parameter count (D)  */
}

/// Sample
///
/// One dataset position reduced to a linear tuning target.
///
/// `features` stores sparse White-view score derivatives, `base` stores the
/// frozen non-tuned residual, and `label` is the White-view game result.
struct Sample {
    features: Vec<(usize, f64)>,                                                /* sparse ∂score/∂θ coefficients      */
    base: f64,                                                                  /* frozen non-tuned score residual    */
    label: f64,                                                                 /* White-view game result             */
}

/// TuneDataset
///
/// Game-disjoint training and validation samples loaded from one dataset.
struct TuneDataset {
    training: Vec<Sample>,
    validation: Vec<Sample>,
}

/// build_shape
///
/// Derives the tunable-vector geometry for the loaded variant from its
/// piece-type pairing and board size, laying out the four parameter
/// blocks and computing the total dimension.
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
    let dimension = endgame_pst_base + piece_types * board_size;

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
        dimension,
    }
}

/// initial_theta
///
/// Reads the variant's current parameters into a float tuning vector in
/// `TuneShape` order, so optimisation starts from the values the engine
/// is presently using.
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

    theta
}

/// phase_weights
///
/// Returns the opening and endgame blend weights for a position,
/// matching the interpolation `evaluate_position!` performs: opening and
/// setup weight the opening term fully, endgame weights the endgame term
/// fully, and middlegame splits linearly by phase score. The weights are
/// frozen at extraction time to keep the tuning model linear.
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

/// mirror_square
///
/// Maps a square to its horizontal-axis mirror (same file, flipped
/// rank), the transform relating a Black piece's square to the White PST
/// parameter it reads, since Black PSTs are the mirror of White's.
///
/// Params:
/// - square: Square -> the square to mirror
/// - files : usize  -> board width
/// - ranks : usize  -> board height
///
/// Return:
/// usize            -> the mirrored square index
fn mirror_square(square: Square, files: usize, ranks: usize) -> usize {
    let index = square as usize;
    let file = index % files;
    let rank = index / files;
    (ranks - 1 - rank) * files + file
}

/// extract_sample
///
/// Reduces one position to a tuning `Sample`. It accumulates the sparse
/// White-view partial derivatives of the tapered material-and-PST score:
/// material types contribute their phase-weighted net count, White
/// pieces add their phase-weighted PST square, and Black pieces subtract
/// theirs at the mirrored square. The frozen `base` is the real
/// White-view evaluation minus the current linear part, capturing the
/// non-tuned derived terms.
///
/// Params:
/// - state : &State          -> quiet position to reduce
/// - shape : &TuneShape      -> vector geometry to index into
/// - theta : &[f64]          -> current parameters, for the base residual
/// - bufs  : &mut SearchBufs -> scratch for the evaluation macro
/// - ptable: &PTable         -> shared pawn structure table
/// - label : f64             -> White-view game result for this position
///
/// Return:
/// Sample                    -> sparse features, frozen base, and label
fn extract_sample(
    state: &State,
    shape: &TuneShape,
    theta: &[f64],
    bufs: &mut SearchBufs,
    ptable: &PTable,
    label: f64,
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
            let mirror =
                mirror_square(square, shape.files, shape.ranks);
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

    let stm_eval = evaluate_position!(state, bufs, ptable) as f64;
    let white_eval =
        if state.playing == WHITE { stm_eval } else { -stm_eval };
    let base = white_eval - dot(&features, theta);

    Sample { features, base, label }
}

/// Tuning math primitives.
///
/// A tight family of pure numeric helpers over the linear model.
/// `mean_squared_error` parallelises across the dataset with rayon.
///
/// dot
///
///   Params:
///   - features: &[(usize, f64)] -> sparse coefficients
///   - theta   : &[f64]          -> parameter vector
///
///   Return:
///   f64                         -> sparse features · θ
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
///   f64               -> the sample's modelled centipawn score `base + f·θ`
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
fn dot(features: &[(usize, f64)], theta: &[f64]) -> f64 {
    features.iter().map(|(index, coeff)| theta[*index] * coeff).sum()
}

fn sigmoid(value: f64) -> f64 {
    1.0 / (1.0 + 10f64.powf(-value))
}

fn model_score(sample: &Sample, theta: &[f64]) -> f64 {
    sample.base + dot(&sample.features, theta)
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
/// Finds the sigmoid scaling constant K that minimises the dataset
/// mean-squared error at the current parameters, by golden-section
/// search over the configured range. Fitting K once anchors the sigmoid
/// so the gradient step tunes shape rather than fighting the slope.
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
/// Computes the mean-squared-error gradient with respect to every
/// tunable parameter, summing each sample's sparse contribution
/// `2(E−r)·E(1−E)·(K·ln10/400)·feature` in parallel and averaging over
/// the dataset.
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

/// clamp_material
///
/// Clamps the opening and endgame material entries of the parameter
/// vector into the 14-bit range the parameter parser requires, leaving
/// the unbounded PST entries untouched. Applied after each Adam step so
/// the tuned vector always exports cleanly.
///
/// Params:
/// - theta: &mut [f64] -> parameter vector to constrain in place
/// - shape: &TuneShape -> block offsets identifying material entries
fn clamp_material(theta: &mut [f64], shape: &TuneShape) {
    for type_index in 0..shape.piece_types {
        let opening = shape.opening_material_base + type_index;
        let endgame = shape.endgame_material_base + type_index;
        theta[opening] = theta[opening].clamp(0.0, 0x3FFF as f64);
        theta[endgame] = theta[endgame].clamp(0.0, 0x3FFF as f64);
    }
}

/// load_dataset
///
/// Reads `res/data/{variant}/latest.data`, requiring `game;FEN;result` rows.
/// Every game is assigned wholly to training or validation by game ID, then
/// each position is reduced to a tuning sample. Reports game-result and phase
/// distributions so skew is visible before optimization.
///
/// Params:
/// - template: &State     -> loaded variant to clone scratch states
/// - variant : &str       -> variant name, selects the dataset file
/// - shape   : &TuneShape -> vector geometry for feature extraction
/// - theta   : &[f64]     -> starting parameters for the base term
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
    let mut bufs = SearchBufs::default();
    let ptable = PTable::default();
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
        parse_fen(&mut scratch, fen, None);
        refresh_eval_state(&mut scratch);

        let phase_index = match scratch.game_phase {
            SETUP => 0,
            OPENING => 1,
            MIDDLEGAME => 2,
            ENDGAME => 3,
            _ => unreachable!(),
        };
        phases[phase_index] += 1;

        let sample = extract_sample(
            &scratch, shape, theta, &mut bufs, &ptable, label
        );
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

/// export_theta
///
/// Serialises the tuned vector into the on-disk parameter layout,
/// carrying the untuned phase scores and role flags through unchanged,
/// then loads it into the variant and writes it out — reusing the exact
/// parse-and-derive path the engine runs at startup so the reloaded
/// parameters behave identically. `export_tuned_parameters_file` rolls
/// the previous `latest.param` to an epoch backup.
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
    let mut tokens: Vec<String> = Vec::new();

    tokens.push(state.statics.opening_score.to_string());
    tokens.push(state.statics.endgame_score.to_string());

    let rounded = |value: f64| value.round() as i64;
    let material = |value: f64| rounded(value).clamp(0, 0x3FFF).to_string();

    for type_index in 0..shape.piece_types {
        tokens.push(material(theta[shape.opening_material_base + type_index]));
    }

    for type_index in 0..shape.piece_types {
        tokens.push(material(theta[shape.endgame_material_base + type_index]));
    }

    for (white_index, _) in shape.pairs.iter() {
        let flag = p_is_big!(&state.statics.pieces[*white_index]) as u8;
        tokens.push(flag.to_string());
    }

    for (white_index, _) in shape.pairs.iter() {
        let flag = p_is_major!(&state.statics.pieces[*white_index]) as u8;
        tokens.push(flag.to_string());
    }

    for type_index in 0..shape.piece_types {
        for square in 0..board_size {
            let offset = type_index * board_size + square;
            tokens.push(rounded(theta[shape.opening_pst_base + offset])
                .to_string());
        }
        for square in 0..board_size {
            let offset = type_index * board_size + square;
            tokens.push(rounded(theta[shape.endgame_pst_base + offset])
                .to_string());
        }
    }

    parse_tuned_parameters(state, &tokens.join(" "));
    derive_search_parameters(state);
    export_tuned_parameters_file(state, variant);
}

/// run_tuning
///
/// Console entry point for the `tune` command. Loads the loaded
/// variant's game-disjoint dataset, fits scaling on training samples, then
/// runs Adam while tracking validation error. Training stops after sustained
/// validation stagnation, and the best validation epoch is exported through
/// the startup parameter pipeline.
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

        clamp_material(&mut theta, &shape);

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
