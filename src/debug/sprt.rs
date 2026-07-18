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
/// Result<PathBuf, String> -> sandbox path or path-resolution error
fn engine_sandbox(binary: &str) -> Result<PathBuf, String> {
    let executable = fs::canonicalize(binary).map_err(|error| {
        format!("Failed to resolve engine {}: {}", binary, error)
    })?;
    let name = executable.to_string_lossy().replace(['/', '\\'], "_");

    Ok(env::temp_dir().join("anekamacam-sprt").join(name))
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

/// SPRTChildError
///
/// Structured subprocess failure with enough context to identify the engine,
/// failed operation, process state, and emitted diagnostics.
struct SPRTChildError {
    binary: String,
    action: String,
    detail: String,
    status: String,
    stderr: String,
}

impl Display for SPRTChildError {
    fn fmt(&self, formatter: &mut FmtFormatter<'_>) -> FmtResult {
        write!(
            formatter,
            "engine {} failed during {}: {} (status: {})\nstderr:\n{}",
            self.binary, self.action, self.detail, self.status, self.stderr,
        )
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
    binary: String,                                                             /* executable path for diagnostics    */
    process: Child,                                                             /* the running engine subprocess      */
    input: ChildStdin,                                                          /* pipe carrying commands to it       */
    output: Receiver<Result<Option<String>, String>>,                           /* timed subprocess reply stream      */
    errors: ChildStderr,                                                        /* pipe of its stderr diagnostics     */
}

/// UciEngine protocol driver.
///
/// A tight family of methods that own one subprocess engine's UCI
/// conversation.
///
/// `spawn` configures the engine for the variant with a single thread and
/// runs it inside its `engine_sandbox` so each binary sees only its own
/// parameter files. Protocol and process failures return structured errors;
/// the referee scores an in-game child failure as a loss and stops cleanly.
///
/// spawn
///
///   Params:
///   - binary : &str -> path to the engine executable
///   - variant: &str -> UCI variant name to select
///
///   Return:
///   Result<SPRTChild, SPRTChildError> -> handshaken child or setup failure
///
/// send
///
///   Params:
///   - command: &str -> the command line to write and flush
///
///   Return:
///   Result<(), SPRTChildError> -> success or command-write failure
///
/// read_line
///
///   Params:
///   - timeout: Duration -> maximum wait for one response line
///
///   Return:
///   Result<Option<String>, SPRTChildError> -> reply, EOF, or read failure
///
/// wait_for
///
///   Params:
///   - token  : &str     -> leading token that ends the wait
///   - timeout: Duration -> absolute protocol deadline
///
///   Return:
///   Result<(), SPRTChildError> -> success or protocol failure
///
/// new_game
///   resets the engine between games and returns any protocol failure
///
/// bestmove
///
///   Params:
///   - startpos  : &str      -> the variant start-position FEN
///   - moves     : &[String] -> moves played so far, in UCI notation
///   - go_command: &str      -> the `go` line carrying the time control
///   - timeout   : Duration  -> absolute response deadline
///
///   Return:
///   Result<Option<String>, SPRTChildError> -> move, missing move, or failure
///
/// drain_errors
///
///   Return:
///   String -> the child's collected stderr, for crash diagnostics
impl SPRTChild {
    fn setup_error(
        binary: &str,
        action: &str,
        detail: String,
    ) -> SPRTChildError {
        SPRTChildError {
            binary: binary.to_string(),
            action: action.to_string(),
            detail,
            status: "not running".to_string(),
            stderr: "<engine was not started>".to_string(),
        }
    }

    fn output_reader(
        output: ChildStdout,
    ) -> Receiver<Result<Option<String>, String>> {
        let (sender, receiver) = channel();

        thread::spawn(move || {
            let mut output = BufReader::new(output);

            loop {
                let mut line = String::new();
                let message = match output.read_line(&mut line) {
                    Ok(0) => Ok(None),
                    Ok(_) => Ok(Some(line)),
                    Err(error) => Err(error.to_string()),
                };
                let done = !matches!(message, Ok(Some(_)));

                if sender.send(message).is_err() || done {
                    break;
                }
            }
        });

        receiver
    }

    fn spawn(binary: &str, variant: &str) -> Result<SPRTChild, SPRTChildError> {
        let executable = fs::canonicalize(binary).map_err(|error| {
            Self::setup_error(binary, "path resolution", error.to_string())
        })?;
        let sandbox = engine_sandbox(binary).map_err(|detail| {
            Self::setup_error(binary, "sandbox resolution", detail)
        })?;

        fs::create_dir_all(&sandbox).map_err(|error| {
            Self::setup_error(
                binary,
                "sandbox creation",
                format!("{}: {}", sandbox.display(), error),
            )
        })?;

        let mut process = Command::new(executable)
            .arg("uci")
            .current_dir(&sandbox)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|error| {
                Self::setup_error(binary, "process spawn", error.to_string())
            })?;

        let Some(input) = process.stdin.take() else {
            let _ = process.kill();
            let _ = process.wait();
            return Err(Self::setup_error(
                binary, "process setup", "missing stdin pipe".to_string()
            ));
        };
        let Some(output) = process.stdout.take() else {
            let _ = process.kill();
            let _ = process.wait();
            return Err(Self::setup_error(
                binary, "process setup", "missing stdout pipe".to_string()
            ));
        };
        let Some(errors) = process.stderr.take() else {
            let _ = process.kill();
            let _ = process.wait();
            return Err(Self::setup_error(
                binary, "process setup", "missing stderr pipe".to_string()
            ));
        };

        let mut engine = SPRTChild {
            binary: binary.to_string(),
            process,
            input,
            output: Self::output_reader(output),
            errors,
        };

        engine.send("uci")?;
        engine.wait_for(
            "uciok",
            Duration::from_millis(SPRT_HANDSHAKE_TIMEOUT_MS),
        )?;
        engine.send(&format!(
            "setoption name {} value {}", OPT_VARIANT, variant
        ))?;
        engine.send(&format!("setoption name {} value 1", OPT_THREADS))?;
        engine.send("isready")?;
        engine.wait_for(
            "readyok",
            Duration::from_millis(SPRT_HANDSHAKE_TIMEOUT_MS),
        )?;

        Ok(engine)
    }

    fn failure(&mut self, action: &str, detail: String) -> SPRTChildError {
        let (status, exited) = match self.process.try_wait() {
            Ok(Some(status)) => (status.to_string(), true),
            Ok(None) => ("running".to_string(), false),
            Err(error) => (format!("status unavailable: {}", error), false),
        };
        let stderr = if exited {
            self.drain_errors()
        } else {
            "<engine still running; stderr not drained>".to_string()
        };

        SPRTChildError {
            binary: self.binary.clone(),
            action: action.to_string(),
            detail,
            status,
            stderr,
        }
    }

    fn exited_error(&mut self, action: &str) -> Option<SPRTChildError> {
        match self.process.try_wait() {
            Ok(Some(status)) => Some(SPRTChildError {
                binary: self.binary.clone(),
                action: action.to_string(),
                detail: "process exited unexpectedly".to_string(),
                status: status.to_string(),
                stderr: self.drain_errors(),
            }),
            Ok(None) => None,
            Err(error) => Some(self.failure(action, error.to_string())),
        }
    }

    fn send(&mut self, command: &str) -> Result<(), SPRTChildError> {
        writeln!(self.input, "{}", command)
            .and_then(|_| self.input.flush())
            .map_err(|error| {
                self.failure(
                    "command write",
                    format!("{}: {}", command, error),
                )
            })
    }

    fn drain_errors(&mut self) -> String {
        let mut errors = String::new();
        let _ = self.errors.read_to_string(&mut errors);

        if errors.trim().is_empty() {
            "<engine emitted no stderr output>".to_string()
        } else {
            errors
        }
    }

    fn read_line(
        &mut self,
        timeout: Duration,
    ) -> Result<Option<String>, SPRTChildError> {
        match self.output.recv_timeout(timeout) {
            Ok(Ok(line)) => Ok(line),
            Ok(Err(error)) => Err(self.failure("response read", error)),
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                Err(self.failure(
                    "response timeout",
                    format!("no protocol response for {:?}", timeout),
                ))
            }
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                Err(self.failure(
                    "response read",
                    "output reader disconnected".to_string(),
                ))
            }
        }
    }

    fn wait_for(
        &mut self,
        token: &str,
        timeout: Duration,
    ) -> Result<(), SPRTChildError> {
        let deadline = Instant::now() + timeout;

        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return Err(self.failure(
                    "protocol wait",
                    format!("timed out awaiting {}", token),
                ));
            }
            let Some(line) = self.read_line(remaining)? else {
                return Err(self.failure(
                    "protocol wait",
                    format!("stream closed while awaiting {}", token),
                ));
            };

            if line.split_whitespace().next() == Some(token) {
                return Ok(());
            }
        }
    }

    fn new_game(&mut self) -> Result<(), SPRTChildError> {
        self.send("ucinewgame")?;
        self.send("isready")?;
        self.wait_for(
            "readyok",
            Duration::from_millis(SPRT_HANDSHAKE_TIMEOUT_MS),
        )
    }

    fn bestmove(
        &mut self,
        startpos: &str,
        moves: &[String],
        go_command: &str,
        timeout: Duration,
    ) -> Result<Option<String>, SPRTChildError> {
        let mut command = format!("position fen {}", startpos);
        if !moves.is_empty() {
            command.push_str(" moves");
            for played in moves {
                command.push(' ');
                command.push_str(played);
            }
        }

        self.send(&command)?;
        self.send(go_command)?;

        let deadline = Instant::now() + timeout;

        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return Err(self.failure(
                    "bestmove wait",
                    "timed out before bestmove".to_string(),
                ));
            }
            let Some(line) = self.read_line(remaining)? else {
                return Err(self.failure(
                    "bestmove wait",
                    "stream closed before bestmove".to_string(),
                ));
            };
            let mut tokens = line.split_whitespace();
            if tokens.next() == Some("bestmove") {
                return Ok(tokens.next().map(str::to_string));
            }
        }
    }
}

impl Drop for SPRTChild {
    fn drop(&mut self) {
        let _ = writeln!(self.input, "quit");
        let _ = self.input.flush();

        let deadline = Instant::now()
            + Duration::from_millis(SPRT_SHUTDOWN_TIMEOUT_MS);

        loop {
            match self.process.try_wait() {
                Ok(Some(_)) => break,
                Ok(None) if Instant::now() < deadline => {
                    thread::sleep(Duration::from_millis(10));
                }
                _ => {
                    let _ = self.process.kill();
                    let _ = self.process.wait();
                    break;
                }
            }
        }
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

/// SPRTGameOutcome
///
/// Referee result for one game. Normal results carry only score; a single
/// subprocess failure carries scored loss and diagnostics; failure of both
/// children aborts without inventing a winner.
enum SPRTGameOutcome {
    Score(f64),
    EngineLoss {
        score: f64,
        side: u8,
        error: SPRTChildError,
    },
    Aborted(String),
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
///   Result<GameManager, SPRTChildError> -> manager or setup failure
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
///   Return:
///   Result<(), String> -> success or one/both child reset failures
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
///   SPRTGameOutcome
///   White-perspective score, scored child loss, or unscored match abort
impl GameManager {
    fn new(
        template: &State,
        binary_a: &str,
        binary_b: &str,
        variant: &str,
    ) -> Result<GameManager, SPRTChildError> {
        let white = SPRTChild::spawn(binary_a, variant)?;
        let black = SPRTChild::spawn(binary_b, variant)?;

        Ok(GameManager {
            state: template.fork(),
            white,
            black,
        })
    }

    fn swap_colors(&mut self) {
        std::mem::swap(&mut self.white, &mut self.black);
    }

    fn restart(
        &mut self,
        side: u8,
        variant: &str,
    ) -> Result<(), SPRTChildError> {
        let binary = if side == WHITE {
            self.white.binary.clone()
        } else {
            self.black.binary.clone()
        };
        let child = SPRTChild::spawn(&binary, variant)?;

        if side == WHITE {
            self.white = child;
        } else {
            self.black = child;
        }

        Ok(())
    }

    fn reset_to(
        &mut self,
        template: &State,
        opening: &[Move],
    ) -> Result<(), String> {
        self.state = template.fork();
        let state = &mut self.state;

        for played in opening {
            make_move!(state, played.clone());
        }

        let white = self.white.new_game();
        let black = self.black.new_game();

        match (white, black) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(error), Ok(())) | (Ok(()), Err(error)) => {
                Err(error.to_string())
            }
            (Err(white_error), Err(black_error)) => Err(format!(
                "both engines failed during game reset:\n{}\n{}",
                white_error, black_error,
            )),
        }
    }

    fn play(
        &mut self,
        dict: Option<&Translator>,
        startpos: &str,
        time_control: SPRTTimeControl,
        sender: &Sender<TuiEvent>,
    ) -> SPRTGameOutcome {
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
                    return SPRTGameOutcome::Score(state.playing as f64);
                }

                return SPRTGameOutcome::Score(0.5);
            }

            let moves: Vec<String> = state.history.iter()
                .map(|snap| format_move(&snap.move_ply, state, dict))
                .collect();
            let side = state.playing;
            let (go_command, response_ms) = match time_control {
                SPRTTimeControl::MoveTime(movetime_ms) => (
                    format!("go movetime {}", movetime_ms),
                    movetime_ms + SPRT_RESPONSE_GRACE_MS,
                ),
                SPRTTimeControl::Clock { inc_ms, .. } => (
                    format!(
                        "go wtime {} btime {} winc {} binc {}",
                        clocks[WHITE as usize], clocks[BLACK as usize],
                        inc_ms, inc_ms,
                    ),
                    clocks[side as usize] + inc_ms + SPRT_RESPONSE_GRACE_MS,
                ),
            };
            let response_ms = response_ms.min(u64::MAX as u128) as u64;
            let timeout = Duration::from_millis(response_ms);
            let move_start = Instant::now();
            let result = if side == WHITE {
                self.white.bestmove(startpos, &moves, &go_command, timeout)
            } else {
                self.black.bestmove(startpos, &moves, &go_command, timeout)
            };

            let move_string = match result {
                Ok(Some(text)) if text != "(none)" => text,
                Ok(_) => return SPRTGameOutcome::Score(side as f64),
                Err(error) => {
                    let other = if side == WHITE {
                        self.black.exited_error("opponent status check")
                    } else {
                        self.white.exited_error("opponent status check")
                    };

                    if let Some(other_error) = other {
                        return SPRTGameOutcome::Aborted(format!(
                            "both engines failed:\n{}\n{}",
                            error, other_error,
                        ));
                    }

                    return SPRTGameOutcome::EngineLoss {
                        score: side as f64,
                        side,
                        error,
                    };
                }
            };

            if let SPRTTimeControl::Clock { inc_ms, .. } = time_control {
                let spent = move_start.elapsed().as_millis();
                let clock = &mut clocks[side as usize];

                if spent > *clock {
                    return SPRTGameOutcome::Score(side as f64);
                }

                *clock = *clock - spent + inc_ms;
            }

            let parsed = match parse_move(&move_string, state, dict) {
                Some(mv) => mv,
                None => return SPRTGameOutcome::Score(state.playing as f64),
            };

            if !make_move!(state, parsed) {
                return SPRTGameOutcome::Score(state.playing as f64);
            }

            if let Err(error) = sender.send(TuiEvent::StateUpdate(
                BoardState::from_state(state, dict)
            )) {
                return SPRTGameOutcome::Aborted(format!(
                    "failed to send TuiEvent::StateUpdate: {}", error,
                ));
            }
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
    let process_id = std::process::id();
    let path = format!("{}/{}-{}.log", dir, stamp, process_id);

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

    for binary in [binary_a, binary_b] {
        let sandbox = match engine_sandbox(binary) {
            Ok(path) => path,
            Err(error) => {
                let verdict = format!("aborted during setup: {}", error);
                log_1!("SPRT {}", verdict);
                write_result_file(
                    variant, binary_a, binary_b, time_control, h0, h1,
                    0, 0, 0, 0.0, &verdict,
                );
                return;
            }
        };
        let _ = fs::remove_dir_all(sandbox);
    }

    let mut manager = match GameManager::new(
        template, binary_a, binary_b, variant
    ) {
        Ok(manager) => manager,
        Err(error) => {
            let verdict = format!("aborted during setup: {}", error);
            log_1!("SPRT {}", verdict);
            write_result_file(
                variant, binary_a, binary_b, time_control, h0, h1,
                0, 0, 0, 0.0, &verdict,
            );
            return;
        }
    };

    let mu_0 = expected_score(h0);
    let mu_1 = expected_score(h1);
    let upper = ((1.0 - SPRT_BETA) / SPRT_ALPHA).ln();
    let lower = (SPRT_BETA / (1.0 - SPRT_ALPHA)).ln();

    let (mut wins, mut draws, mut losses) = (0u32, 0u32, 0u32);
    let (mut pairs, mut sum, mut sum_squares) = (0.0f64, 0.0f64, 0.0f64);
    let mut llr = 0.0f64;
    let mut verdict = "inconclusive (game budget reached)".to_string();

    'pairs: for pair_index in 0..(max_games / 2) {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            verdict = "cancelled".to_string();
            break;
        }

        let opening = opening_line(template, OPENING_RANDOM_PLIES);

        if let Err(error) = manager.reset_to(template, &opening) {
            verdict = format!("aborted during game setup: {}", error);
            break;
        }

        let first = manager.play(dict, &startpos, time_control, sender);
        let score_first = match first {
            SPRTGameOutcome::Score(score) => score,
            SPRTGameOutcome::EngineLoss { score, side, error } => {
                log_1!("SPRT scored engine loss: {}", error);
                if let Err(restart_error) = manager.restart(side, variant) {
                    let (win, draw, loss) = game_score_bucket(score);
                    wins += win;
                    draws += draw;
                    losses += loss;
                    verdict = format!(
                        "aborted after engine loss; restart failed: {}",
                        restart_error,
                    );
                    break 'pairs;
                }
                score
            }
            SPRTGameOutcome::Aborted(error) => {
                verdict = format!("aborted during game: {}", error);
                break 'pairs;
            }
        };
        let (win, draw, loss) = game_score_bucket(score_first);
        wins += win;
        draws += draw;
        losses += loss;

        manager.swap_colors();

        if let Err(error) = manager.reset_to(template, &opening) {
            verdict = format!("aborted during game setup: {}", error);
            break;
        }

        let mut abort_after_pair = None;
        let second = manager.play(dict, &startpos, time_control, sender);
        let score_second = match second {
            SPRTGameOutcome::Score(score) => 1.0 - score,
            SPRTGameOutcome::EngineLoss { score, side, error } => {
                log_1!("SPRT scored engine loss: {}", error);
                if let Err(restart_error) = manager.restart(side, variant) {
                    abort_after_pair = Some(format!(
                        "aborted after engine loss; restart failed: {}",
                        restart_error,
                    ));
                }
                1.0 - score
            }
            SPRTGameOutcome::Aborted(error) => {
                verdict = format!("aborted during game: {}", error);
                break 'pairs;
            }
        };
        let (win, draw, loss) = game_score_bucket(score_second);
        wins += win;
        draws += draw;
        losses += loss;

        manager.swap_colors();

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

        if let Some(error) = abort_after_pair {
            verdict = error;
            break;
        }

        if llr >= upper {
            verdict = "H1 accepted (patch is stronger)".to_string();
            break;
        }
        if llr <= lower {
            verdict = "H0 accepted (no improvement)".to_string();
            break;
        }
    }

    log_1!(
        "SPRT done: {} | {}W {}L {}D | LLR {:.3}",
        verdict, wins, losses, draws, llr,
    );

    write_result_file(
        variant, binary_a, binary_b, time_control, h0, h1,
        wins, draws, losses, llr, &verdict,
    );
}
