//! sprt.rs
//!
//! Built-in engine-versus-engine match runner with a Sequential
//! Probability Ratio Test.
//!
//! Drives two engine binaries as UCI subprocesses, plays them across
//! paired random openings while an in-process board acts as the neutral
//! referee, and accumulates a normalised pentanomial log-likelihood
//! ratio to decide — as early as the evidence allows — whether a patch
//! is a real strength gain. It replaces the manual GUI-and-Fairy-Stockfish
//! loop with a self-contained console command.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/// engine_sandbox
///
/// Maps an engine binary to its private working directory under the
/// system temp dir. Children run there so each binary resolves
/// `res/param` against its own exports or embedded defaults instead of
/// sharing the repository's parameter files, keeping param-changing
/// patches measurable.
///
/// Params:
/// - binary: &str -> path to the engine executable
///
/// Return:
/// PathBuf        -> per-binary sandbox directory path
fn engine_sandbox(binary: &str) -> PathBuf {
    let executable = fs::canonicalize(binary).unwrap_or_else(|e| {
        panic!("Failed to resolve engine {}: {}", binary, e)
    });

    let name = executable.to_string_lossy().replace(['/', '\\'], "_");

    env::temp_dir().join("anekamacam-sprt").join(name)
}

/// SPRTTimeControl
///
/// Per-move time budget for SPRT games: `MoveTime` searches every move
/// at a fixed wall-clock budget, while `Clock` gives each side a base
/// bank plus a per-move increment driven through `wtime`/`btime`/
/// `winc`/`binc`, so the engines' own time management decides how to
/// spend it. The referee tracks the clocks and scores an overstep as a
/// loss for the side that flagged.
#[derive(Clone, Copy)]
pub enum SPRTTimeControl {
    MoveTime(u128),                                                             /* fixed milliseconds per move        */
    Clock { base_ms: u128, inc_ms: u128 },                                      /* bank + increment, in milliseconds  */
}

impl Display for SPRTTimeControl {
    fn fmt(&self, formatter: &mut FmtFormatter<'_>) -> FmtResult {
        match self {
            SPRTTimeControl::MoveTime(movetime_ms) => {
                write!(formatter, "movetime {}ms", movetime_ms)
            }
            SPRTTimeControl::Clock { base_ms, inc_ms } => {
                write!(formatter, "clock {}+{}ms", base_ms, inc_ms)
            }
        }
    }
}

/// SPRTChild
///
/// A running engine subprocess spoken to over UCI.
/// Owns the child process and the piped handles used to send commands
/// and read replies; the driver methods keep the protocol handshake and
/// per-move exchange in one place, and the `Drop` impl sends `quit` and
/// reaps the process so no child is left behind.
struct SPRTChild {
    process: Child,                                                             /* the running engine subprocess      */
    input: ChildStdin,                                                          /* pipe carrying commands to it       */
    output: BufReader<ChildStdout>,                                             /* buffered pipe of its replies       */
    errors: ChildStderr,                                                        /* pipe of its stderr diagnostics     */
}

/// UciEngine protocol driver.
///
/// A tight family of methods that own one subprocess engine's UCI
/// conversation.
///
/// `spawn` configures the engine for the variant with a small hash and a
/// single thread for fair, reproducible games, and runs it inside its
/// `engine_sandbox` so each binary sees only its own parameter files;
/// `bestmove` returns `None` when the engine ends its stream (a crash),
/// which the referee scores as a loss; handshake failures panic since
/// they are setup errors.
///
/// spawn
///
///   Params:
///   - binary : &str -> path to the engine executable
///   - variant: &str -> UCI variant name to select
///
///   Return:
///   SPRTChild       -> the handshaken engine subprocess
///
/// send
///
///   Params:
///   - command: &str -> the command line to write and flush
///
/// read_line
///
///   Return:
///   Option<String> -> one reply line, None at end of stream
///
/// wait_for
///
///   Params:
///   - token: &str -> leading token that ends the wait
///
/// new_game
///   resets the engine between games; no parameters, no return value
///
/// bestmove
///
///   Params:
///   - startpos  : &str      -> the variant start-position FEN
///   - moves     : &[String] -> moves played so far, in UCI notation
///   - go_command: &str      -> the `go` line carrying the time control
///
///   Return:
///   Option<String>          -> the engine's move, None when its stream ended
///
/// drain_errors
///
///   Return:
///   String -> the child's collected stderr, for crash diagnostics
impl SPRTChild {
    fn spawn(binary: &str, variant: &str) -> SPRTChild {
        let executable = fs::canonicalize(binary).unwrap_or_else(|e| {
            panic!("Failed to resolve engine {}: {}", binary, e)
        });

        let sandbox = engine_sandbox(binary);

        fs::create_dir_all(&sandbox).unwrap_or_else(|e| {
            panic!(
                "Failed to create sandbox {}: {}", sandbox.display(), e
            )
        });

        let mut process = Command::new(executable)
            .arg("uci")
            .current_dir(&sandbox)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap_or_else(|e| {
                panic!("Failed to spawn engine {}: {}", binary, e)
            });

        let input = process.stdin.take().unwrap_or_else(|| {
            panic!("Engine {} exposed no stdin", binary)
        });
        let output = BufReader::new(process.stdout.take().unwrap_or_else(|| {
            panic!("Engine {} exposed no stdout", binary)
        }));
        let errors = process.stderr.take().unwrap_or_else(|| {
            panic!("Engine {} exposed no stderr", binary)
        });

        let mut engine = SPRTChild { process, input, output, errors };

        engine.send("uci");
        engine.wait_for("uciok");
        engine.send(&format!(
            "setoption name {} value {}", OPT_VARIANT, variant
        ));
        engine.send(&format!("setoption name {} value 1", OPT_THREADS));
        engine.send("isready");
        engine.wait_for("readyok");

        engine
    }

    fn send(&mut self, command: &str) {
        let written = writeln!(self.input, "{}", command);
        let flushed = self.input.flush();
        if let Err(e) = written.and(flushed) {
            panic!(
                "Failed to send '{}' to engine: {:?}\nstderr:\n{}",
                command, e.into_inner(), self.drain_errors()
            )
        }
    }

    fn drain_errors(&mut self) -> String {
        let mut errors = String::new();
        self.errors.read_to_string(&mut errors).ok();
        if errors.trim().is_empty() {
            "<engine emitted no stderr output>".to_string()
        } else {
            errors
        }
    }

    fn read_line(&mut self) -> Option<String> {
        let mut line = String::new();
        let read = self.output.read_line(&mut line);
        let bytes = read.unwrap_or_else(|e| {
            panic!(
                "Failed to read from engine: {}\n\
                stderr:\n{}",
                e, self.drain_errors()
            )
        });

        if bytes == 0 { None } else { Some(line) }
    }

    fn wait_for(&mut self, token: &str) {
        loop {
            let line = match self.read_line() {
                Some(line) => line,
                None => panic!(
                    "Engine closed its stream while awaiting '{}'\n\
                     stderr:\n{}",
                    token, self.drain_errors()
                ),
            };
            if line.split_whitespace().next() == Some(token) {
                return;
            }
        }
    }

    fn new_game(&mut self) {
        self.send("ucinewgame");
        self.send("isready");
        self.wait_for("readyok");
    }

    fn bestmove(
        &mut self,
        startpos: &str,
        moves: &[String],
        go_command: &str,
    ) -> Option<String> {
        let mut command = format!("position fen {}", startpos);
        if !moves.is_empty() {
            command.push_str(" moves");
            for played in moves {
                command.push(' ');
                command.push_str(played);
            }
        }

        self.send(&command);
        self.send(go_command);

        loop {
            let line = self.read_line()?;
            let mut tokens = line.split_whitespace();
            if tokens.next() == Some("bestmove") {
                return tokens.next().map(str::to_string);
            }
        }
    }
}

impl Drop for SPRTChild {
    fn drop(&mut self) {
        let _ = writeln!(self.input, "quit");
        let _ = self.input.flush();
        let _ = self.process.wait();
    }
}

/// opening_line
///
/// Forks a throwaway referee, walks a short random legal line on it, and
/// returns that line as moves — captured once per pair so both games of
/// the pair play the identical opening from opposite colours. The moves
/// come straight from the fork's own history.
///
/// Params:
/// - template: &State -> loaded variant to open from
/// - plies   : usize  -> number of random plies to play
///
/// Return:
/// Vec<Move>          -> the shared opening line, in play order
fn opening_line(template: &State, plies: usize) -> Vec<Move> {
    let mut state = template.fork();
    state.play_random_opening(plies);
    state.history.iter().map(|snap| snap.move_ply.clone()).collect()
}

/// GameManager
///
/// One refereed match slot between two subprocess engines. Owns the
/// neutral in-process `State` — whose move history *is* the game, so the
/// UCI move list handed to each engine is derived from it rather than
/// tracked in parallel — together with the two engine children. `white`
/// and `black` name the side each child is currently playing, and
/// `swap_colors` exchanges them between the two games of a pair so each
/// engine gets White once. The `State` is also the snapshot streamed to
/// the TUI so the live game is visible in the board view.
struct GameManager {
    state: State,                                                               /* neutral referee; history is game   */
    white: SPRTChild,                                                           /* child currently playing White      */
    black: SPRTChild,                                                           /* child currently playing Black      */
}

/// GameManager driver.
///
/// A tight family that runs one refereed match slot.
///
/// `play` rebuilds each engine's move list from the referee's own history
/// every ply (with `format_move`), so no parallel move-string list is
/// kept; a missing, unparsable, or illegal reply loses for that side,
/// while mate and the draw rules are adjudicated by the referee.
///
/// new
///
///   Params:
///   - template: &State -> loaded variant, forked as the referee
///   - binary_a: &str   -> path to the child that starts as White
///   - binary_b: &str   -> path to the child that starts as Black
///   - variant : &str   -> UCI variant name for both children
///
///   Return:
///   GameManager        -> both children spawned, referee at startpos
///
/// swap_colors
///   exchanges which child plays which colour; no parameters, no
///   return value
///
/// reset_to
///
///   Params:
///   - template: &State  -> loaded variant to fork the referee from
///   - opening : &[Move] -> shared opening line to replay
///
/// play
///
///   Params:
///
///   - dict: Option<&Translator>
///     UCI translator for move I/O
///
///   - startpos: &str
///     the variant start FEN
///
///   - time_control: SPRTTimeControl
///     per-move budget or clock bank
///
///   - sender: &Sender<TuiEvent>
///     channel for live board updates
///
///   Return:
///
///   f64
///   the White-perspective game score; under a clock control a side that
///   oversteps its remaining bank loses on time
impl GameManager {
    fn new(
        template: &State,
        binary_a: &str,
        binary_b: &str,
        variant: &str,
    ) -> GameManager {
        GameManager {
            state: template.fork(),
            white: SPRTChild::spawn(binary_a, variant),
            black: SPRTChild::spawn(binary_b, variant),
        }
    }

    fn swap_colors(&mut self) {
        std::mem::swap(&mut self.white, &mut self.black);
    }

    fn reset_to(&mut self, template: &State, opening: &[Move]) {
        self.state = template.fork();
        let state = &mut self.state;

        for played in opening {
            make_move!(state, played.clone());
        }

        self.white.new_game();
        self.black.new_game();
    }

    fn play(
        &mut self,
        dict: Option<&Translator>,
        startpos: &str,
        time_control: SPRTTimeControl,
        sender: &Sender<TuiEvent>,
    ) -> f64 {
        let state = &mut self.state;

        let mut clocks = match time_control {
            SPRTTimeControl::Clock { base_ms, .. } => [base_ms, base_ms],
            SPRTTimeControl::MoveTime(..) => [0, 0],
        };

        loop {
            if SYSTEM_INTERRUPT.load(Ordering::Relaxed)
            || state.game_over
            || legal_moves!(state).is_empty()
            {
                if is_in_check!(state.playing, state)
                || stalemate_loss!(state)
                {
                    return state.playing as f64;
                }

                return 0.5;
            }

            let moves: Vec<String> = state.history.iter()
                .map(|snap| format_move(&snap.move_ply, state, dict))
                .collect();

            let side = state.playing;
            let engine = if side == WHITE {
                &mut self.white
            } else {
                &mut self.black
            };

            let go_command = match time_control {
                SPRTTimeControl::MoveTime(movetime_ms) => {
                    format!("go movetime {}", movetime_ms)
                }
                SPRTTimeControl::Clock { inc_ms, .. } => {
                    format!(
                        "go wtime {} btime {} winc {} binc {}",
                        clocks[WHITE as usize], clocks[BLACK as usize],
                        inc_ms, inc_ms,
                    )
                }
            };

            let move_start = Instant::now();

            let move_string = match engine.bestmove(
                startpos, &moves, &go_command
            ) {
                Some(text) if text != "(none)" => text,
                _ => return side as f64,
            };

            if let SPRTTimeControl::Clock { inc_ms, .. } = time_control {
                let spent = move_start.elapsed().as_millis();
                let clock = &mut clocks[side as usize];

                if spent > *clock {
                    return side as f64;
                }

                *clock = *clock - spent + inc_ms;
            }

            let parsed = match parse_move(&move_string, state, dict) {
                Some(mv) => mv,
                None => return state.playing as f64,
            };

            if !make_move!(state, parsed) {
                return state.playing as f64;
            }

            sender.send(TuiEvent::StateUpdate(
                BoardState::from_state(state, dict)
            )).unwrap_or_else(|e| {
                panic!("Failed to send TuiEvent::StateUpdate: {e}")
            });
        }
    }
}

/// SPRT statistics.
///
/// A tight family of pure functions for the normalised pentanomial test.
/// The LLR uses the pair sample mean and population variance, so a pair's
/// variance reduction sharpens the test relative to counting single
/// games; it returns zero until the sample shows any variance.
///
/// expected_score
///
///   Params:
///   - elo     : f64 -> the Elo advantage to convert
///
///   Return:
///   f64             -> the logistic expected score for that Elo gap
///
/// elo_from_score
///
///   Params:
///   - score   : f64 -> the observed per-game score
///
///   Return:
///   f64             -> the Elo estimate implied by the score
///
/// log_likelihood_ratio
///
///   Params:
///   - pairs   : f64 -> number of game pairs played
///   - mean    : f64 -> mean normalised pair score
///   - variance: f64 -> population variance of pair scores
///   - mu_zero : f64 -> expected score under the null hypothesis
///   - mu_one  : f64 -> expected score under the alternative hypothesis
///
///   Return:
///   f64             -> the GSPRT log-likelihood ratio
fn expected_score(elo: f64) -> f64 {
    1.0 / (1.0 + 10f64.powf(-elo / 400.0))
}

fn elo_from_score(score: f64) -> f64 {
    let clamped = score.clamp(1e-6, 1.0 - 1e-6);
    -400.0 * (1.0 / clamped - 1.0).log10()
}

fn log_likelihood_ratio(
    pairs: f64,
    mean: f64,
    variance: f64,
    mu_zero: f64,
    mu_one: f64,
) -> f64 {
    if variance <= 1e-12 {
        return 0.0;
    }

    pairs * (mu_one - mu_zero) * (2.0 * mean - mu_zero - mu_one)
        / (2.0 * variance)
}

/// game_score_bucket
///
/// Buckets a White-perspective game score into a win/draw/loss increment
/// for the running tally, tolerating tiny float noise in the fixed
/// {0, 0.5, 1} outcomes.
///
/// Params:
/// - score: f64    -> a single game's score for the counted engine
///
/// Return:
/// (u32, u32, u32) -> a (win, draw, loss) increment to add
fn game_score_bucket(score: f64) -> (u32, u32, u32) {
    if score > 0.75 {
        (1, 0, 0)
    } else if score < 0.25 {
        (0, 0, 1)
    } else {
        (0, 1, 0)
    }
}

/// write_result_file
///
/// Appends a one-run summary to `res/sprt/{variant}/{timestamp}.log`,
/// recording the two binaries, the time control and Elo bounds, the
/// final win/draw/loss tally, the log-likelihood ratio, and the verdict,
/// so completed tests leave a durable record.
///
/// Params:
/// - variant          : &str            -> variant, selects the output dir
/// - binary_a         : &str            -> path of the first engine
/// - binary_b         : &str            -> path of the second engine
/// - time_control     : SPRTTimeControl -> per-move time control used
/// - h0               : f64             -> null-hypothesis Elo bound
/// - h1               : f64             -> alternative-hypothesis Elo bound
/// - wins/draws/losses: u32             -> tally from engine A's view
/// - llr              : f64             -> the final log-likelihood ratio
/// - verdict          : &str            -> the test outcome text
fn write_result_file(
    variant: &str,
    binary_a: &str,
    binary_b: &str,
    time_control: SPRTTimeControl,
    h0: f64,
    h1: f64,
    wins: u32,
    draws: u32,
    losses: u32,
    llr: f64,
    verdict: &str,
) {
    let dir = format!("{}/{}", SPRT_DIR, variant);
    fs::create_dir_all(&dir).unwrap_or_else(|e| {
        panic!("Failed to create SPRT directory {}: {}", dir, e)
    });

    let stamp = chrono::Local::now().format("%Y%m%d-%H%M%S");
    let path = format!("{}/{}.log", dir, stamp);

    let body = format!(
        "engine A: {}\nengine B: {}\nvariant: {}\ntime control: {}\n\
         elo bounds: [{}, {}]  alpha: {}  beta: {}\n\
         result (A): {}W {}L {}D\nLLR: {:.3}\nverdict: {}\n",
        binary_a, binary_b, variant, time_control,
        h0, h1, SPRT_ALPHA, SPRT_BETA,
        wins, losses, draws, llr, verdict,
    );

    fs::write(&path, body).unwrap_or_else(|e| {
        panic!("Failed to write SPRT result {}: {}", path, e)
    });

    log_1!("SPRT result written to {}", path);
}

/// run_sprt
///
/// Console entry point for the `sprt` command. Spawns both engine
/// binaries on the loaded variant — each in a freshly cleared
/// `engine_sandbox` so their parameter files stay isolated — plays
/// paired random openings (each engine gets White once), folds every
/// pair into the normalised pentanomial LLR, and stops as soon as the
/// LLR crosses an acceptance bound or the game budget is spent.
/// Progress is logged and the outcome is saved. Works for every
/// variant: the referee formats moves with the same translator the
/// subprocess uses — the UCI dictionary when one exists, the engine's
/// internal notation otherwise — so both ends always agree.
///
/// Params:
/// - template    : &State          -> loaded variant, refereed and named
/// - variant     : &str            -> variant name, for UCI setup and output
/// - binary_a    : &str            -> path to the first engine binary
/// - binary_b    : &str            -> path to the second engine binary
/// - time_control: SPRTTimeControl -> fixed movetime or clock per move
/// - max_games   : usize           -> maximum games before inconclusive
/// - h0          : f64             -> null-hypothesis Elo bound
/// - h1          : f64             -> alternative-hypothesis Elo bound
pub fn run_sprt(
    template: &State,
    variant: &str,
    binary_a: &str,
    binary_b: &str,
    time_control: SPRTTimeControl,
    max_games: usize,
    h0: f64,
    h1: f64,
    sender: &Sender<TuiEvent>,
) {
    let translator = Translator::find(variant, "uci");

    if translator.is_none() {
        log_4!("Variant {variant} doesn't support UCI for SPRT yet!");
        return;
    }

    let dict = translator.as_ref();

    let startpos = template.statics.startpos.clone();

    let _ = fs::remove_dir_all(engine_sandbox(binary_a));
    let _ = fs::remove_dir_all(engine_sandbox(binary_b));

    let mut manager = GameManager::new(template, binary_a, binary_b, variant);

    let mu_0 = expected_score(h0);
    let mu_1 = expected_score(h1);
    let upper = ((1.0 - SPRT_BETA) / SPRT_ALPHA).ln();
    let lower = (SPRT_BETA / (1.0 - SPRT_ALPHA)).ln();

    let (mut wins, mut draws, mut losses) = (0u32, 0u32, 0u32);
    let (mut pairs, mut sum, mut sum_squares) = (0.0f64, 0.0f64, 0.0f64);
    let mut llr = 0.0f64;
    let mut verdict = "inconclusive (game budget reached)";

    for pair_index in 0..(max_games / 2) {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            verdict = "cancelled";
            break;
        }

        let opening = opening_line(template, OPENING_RANDOM_PLIES);

        manager.reset_to(template, &opening);
        let first = manager.play(dict, &startpos, time_control, sender);

        manager.swap_colors();

        manager.reset_to(template, &opening);
        let second = manager.play(dict, &startpos, time_control, sender);

        manager.swap_colors();

        let score_first = first;
        let score_second = 1.0 - second;

        for score in [score_first, score_second] {
            let (win, draw, loss) = game_score_bucket(score);
            wins += win;
            draws += draw;
            losses += loss;
        }

        let pair_score = (score_first + score_second) / 2.0;
        pairs += 1.0;
        sum += pair_score;
        sum_squares += pair_score * pair_score;

        let mean = sum / pairs;
        let variance = sum_squares / pairs - mean * mean;
        llr = log_likelihood_ratio(pairs, mean, variance, mu_0, mu_1);

        if (pair_index + 1) % 5 == 0 {
            log_1!(
                "SPRT {}W {}L {}D | elo {:.1} | LLR {:.2} [{:.2}, {:.2}]",
                wins, losses, draws, elo_from_score(mean),
                llr, lower, upper,
            );
        }

        if llr >= upper {
            verdict = "H1 accepted (patch is stronger)";
            break;
        }
        if llr <= lower {
            verdict = "H0 accepted (no improvement)";
            break;
        }
    }

    log_1!(
        "SPRT done: {} | {}W {}L {}D | LLR {:.3}",
        verdict, wins, losses, draws, llr,
    );

    write_result_file(
        variant, binary_a, binary_b, time_control, h0, h1,
        wins, draws, losses, llr, verdict,
    );
}
