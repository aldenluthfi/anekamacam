//! sprt.rs
//!
//! Engine against engine match runner with a Sequential Probability Ratio
//! Test.
//!
//! The runner starts two engine binaries as UCI subprocesses. They play
//! pairs of random openings, and an internal board is the referee. A
//! normalised pentanomial log-likelihood ratio decides as early as possible
//! if a patch makes the engine stronger.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                HARNESS SETTINGS
\*----------------------------------------------------------------------------*/

/// SPRT harness settings
///
/// Fixed settings of the match runner. The time control, Elo bounds and
/// game limit are arguments.
///
/// - SPRT_DIR                  : result directory, one for each variant
/// - SPRT_HISTORY_KEEP         : number of rolled results to keep
/// - SPRT_PROTOCOL             : dialect for the two engines
/// - SPRT_ALPHA                : false positive rate
/// - SPRT_BETA                 : false negative rate
/// - SPRT_HANDSHAKE_TIMEOUT_MS : time limit for the engine setup
/// - SPRT_RESPONSE_GRACE_MS    : extra time after the clock for a reply
/// - SPRT_SHUTDOWN_TIMEOUT_MS  : time limit to quit, then kill
///
/// The two error rates set the stop bounds. With equal rates, the bounds
/// are symmetric:
///
/// ```text
/// upper = ln((1 - beta) / alpha)   = +2.944 at 0.05 and 0.05
/// lower = ln(beta / (1 - alpha))   = -2.944 at 0.05 and 0.05
/// ```
///
/// The test runs until the ratio is outside the band. If the game limit
/// comes first, the result is inconclusive.
///
const SPRT_DIR: &str = "res/sprt";
const SPRT_HISTORY_KEEP: usize = 64;                                            /* rolled sprt files kept per family  */
const SPRT_PROTOCOL: &str = "uci";                                              /* dialect the sprt harness speaks    */
const SPRT_ALPHA: f64 = 0.05;
const SPRT_BETA: f64 = 0.05;
const SPRT_HANDSHAKE_TIMEOUT_MS: u64 = 10_000;
const SPRT_RESPONSE_GRACE_MS: u128 = 5_000;
const SPRT_SHUTDOWN_TIMEOUT_MS: u64 = 1_000;

/// engine_sandbox
///
/// Gives the private work directory of one engine binary. Each engine runs
/// in it, so `res/param` has its own exports or its embedded defaults. With
/// shared files, the two engines would have the same parameters, and a
/// parameter patch could not be measured.
///
/// ```text
/// /tmp/anekamacam-sprt/_Users_me_build_release_main
///      ^               ^
///      |               the binary path, each separator is an underscore
///      one folder for the full harness
/// ```
///
/// Params:
/// - binary: &str          -> path of the engine binary
///
/// Return:
/// Result<PathBuf, String> -> the sandbox path, or the path error
///
fn engine_sandbox(binary: &str) -> Result<PathBuf, String> {
    let executable = fs::canonicalize(binary).map_err(|error| {
        format!("Failed to resolve engine {}: {}", binary, error)
    })?;
    let name = executable.to_string_lossy().replace(['/', '\\'], "_");

    Ok(env::temp_dir().join("anekamacam-sprt").join(name))
}

/*----------------------------------------------------------------------------*\
                                  TIME CONTROL
\*----------------------------------------------------------------------------*/

/// SPRTTimeControl
///
/// The time of each side in a game. It selects the `go` line of the
/// referee and whether the referee keeps clocks.
///
/// - `MoveTime(1000)`                      : `go movetime 1000`
/// - `Clock { base_ms: 8000, inc_ms: 80 }` : `go wtime 8000 ... binc 80`
///
/// With a clock, each engine manages its own time, and a flag is a loss.
/// Only the clock form tests time management. A fixed move time hides it.
///
#[derive(Clone, Copy)]
pub enum SPRTTimeControl {
    MoveTime(u128),                                                             /* fixed milliseconds per move        */
    Clock { base_ms: u128, inc_ms: u128 },                                      /* bank + increment, in milliseconds  */
}

/// parse_sprt_time_control
///
/// Reads a time control from the command line. A `+` selects the clock
/// form. All values are milliseconds.
///
/// - `1000`    : one second for each move
/// - `8000+80` : eight seconds, plus 80 milliseconds for each move
///
/// Params:
/// - value: &str                   -> move time, or base and increment
///
/// Return:
/// Result<SPRTTimeControl, String> -> the control, or the bad text
///
pub fn parse_sprt_time_control(
    value: &str,
) -> Result<SPRTTimeControl, String> {
    if let Some((base, increment)) = value.split_once('+') {
        let base_ms = base
            .parse::<u128>()
            .map_err(|_| format!("Invalid time control: {}", value))?;
        let inc_ms = increment
            .parse::<u128>()
            .map_err(|_| format!("Invalid time control: {}", value))?;

        return Ok(SPRTTimeControl::Clock { base_ms, inc_ms });
    }

    value
        .parse::<u128>()
        .map(SPRTTimeControl::MoveTime)
        .map_err(|_| format!("Invalid time control: {}", value))
}

/// SPRTTimeControl::fmt
///
/// Writes the control for the result file. The text is for a person, not
/// for the parser.
///
/// - `MoveTime(1000)`                      : movetime 1000ms
/// - `Clock { base_ms: 8000, inc_ms: 80 }` : clock 8000+80ms
///
/// Params:
/// - formatter: &mut FmtFormatter<'_> -> the output
///
/// Return:
/// FmtResult                          -> the result of the output
///
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

/*----------------------------------------------------------------------------*\
                               ENGINE SUBPROCESS
\*----------------------------------------------------------------------------*/

/// SPRTChildError
///
/// All data about a failed subprocess, collected at the failure. An exit
/// status alone does not explain all failures.
///
/// - binary : the engine
/// - action : the request: spawn, write, read or protocol wait
/// - detail : the failure text
/// - status : running, or the exit status
/// - stderr : the engine diagnostics, read after it exits
///
/// Notes:
/// Stderr is read only after the exit. A read to the end of a live pipe
/// would block.
///
struct SPRTChildError {
    binary: String,                                                             /* which engine, for the report       */
    action: String,                                                             /* what was being asked of it         */
    detail: String,                                                             /* what went wrong doing it           */
    status: String,                                                             /* running, or how it exited          */
    stderr: String,                                                             /* what it said, if it can be read    */
}

/// SPRTChildError::fmt
///
/// Writes the five fields as one message. The run logs it, and saves it as
/// the verdict if the failure ended the test.
///
/// ```text
/// engine ./main failed during bestmove wait: timed out before bestmove
/// (status: running)
/// stderr:
/// <engine still running; stderr not drained>
/// ```
///
/// Params:
/// - formatter: &mut FmtFormatter<'_> -> the output
///
/// Return:
/// FmtResult                          -> the result of the output
///
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
/// One running engine and its three pipes. The two sides are real binaries
/// on UCI, so the test measures the shipped engine.
///
/// - input  : harness → engine, commands flushed at once
/// - output : harness ← engine, replies from a reader thread
/// - errors : harness ← engine, diagnostics read after the exit
///
/// Notes:
/// A `BufReader` has no read timeout. Thus a thread reads the replies into
/// a channel, and the driver waits on it with a deadline. A hung engine
/// loses one game, not the full run.
///
struct SPRTChild {
    binary: String,                                                             /* executable path for diagnostics    */
    process: Child,                                                             /* the running engine subprocess      */
    input: ChildStdin,                                                          /* pipe carrying commands to it       */
    output: Receiver<Result<Option<String>, String>>,                           /* timed subprocess reply stream      */
    errors: ChildStderr,                                                        /* pipe of its stderr diagnostics     */
}

/// SPRTChild protocol driver
///
/// The side of one engine in the protocol, from the start to a move
/// request.
///
/// - setup_error   : error for an engine that did not start
/// - failure       : error for a running engine, with its state
/// - exited_error  : error only if the engine exited
/// - output_reader : thread that puts the reply pipe into a channel
/// - spawn         : start a binary, set the variant, wait for readyok
/// - send          : write one line and flush it
/// - drain_errors  : all stderr output of the engine
/// - read_line     : read the next reply, or stop at a deadline
/// - wait_for      : skip lines until a line starts with a token
/// - new_game      : reset between games and wait for the reset
/// - bestmove      : set the position, send `go`, give the move
///
/// setup_error
///
///   Params:
///   - binary: &str   -> path of the engine that did not start
///   - action: &str   -> the action that failed
///   - detail: String -> the failure text
///
///   Return:
///   SPRTChildError   -> the error, without process state
///
/// output_reader
///
///   Params:
///   - output: ChildStdout                    -> reply pipe of the engine
///
///   Return:
///   Receiver<Result<Option<String>, String>> -> lines, `None` at the end
///
/// spawn
///
///   Params:
///   - binary : &str                   -> path of the engine binary
///   - variant: &str                   -> variant to select on UCI
///
///   Return:
///   Result<SPRTChild, SPRTChildError> -> a ready engine, or the error
///
/// failure
///
///   Params:
///   - action: &str   -> the action that failed
///   - detail: String -> the failure text
///
///   Return:
///   SPRTChildError   -> the error, with the engine state
///
/// exited_error
///
///   Params:
///   - action: &str         -> the action for the error text
///
///   Return:
///   Option<SPRTChildError> -> an error if the engine exited, else None
///
/// send
///
///   Params:
///   - command: &str            -> the line to write and flush
///
///   Return:
///   Result<(), SPRTChildError> -> Ok, or the write error
///
/// drain_errors
///
///   Return:
///   String -> stderr of the engine, or a note that it is empty
///
/// read_line
///
///   Params:
///   - timeout: Duration                    -> time limit for one line
///
///   Return:
///   Result<Option<String>, SPRTChildError> -> a reply, `None`, or an error
///
/// wait_for
///
///   Params:
///   - token  : &str            -> first word that ends the wait
///   - timeout: Duration        -> time limit for the full wait
///
///   Return:
///   Result<(), SPRTChildError> -> Ok when the token came, else an error
///
/// new_game
///
///   Return:
///   Result<(), SPRTChildError> -> Ok when the reset worked, else an error
///
/// bestmove
///
///   Params:
///   - startpos  : &str                     -> start position of the variant
///   - moves     : &[String]                -> moves of the game, as text
///   - go_command: &str                     -> the `go` line with clocks
///   - timeout   : Duration                 -> time limit for a reply
///
///   Return:
///   Result<Option<String>, SPRTChildError> -> the move, `None`, or an error
///
/// Notes:
/// `spawn` runs each engine in its own `engine_sandbox` with one thread, so
/// the two do not compete for cores. A failure is returned, not raised. An
/// engine that fails loses the game and restarts. Only a failure of the two
/// engines at once ends the run.
///
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
            .arg(SPRT_PROTOCOL)
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

        engine.send(SPRT_PROTOCOL)?;
        engine.wait_for(
            &format!("{}ok", SPRT_PROTOCOL),
            Duration::from_millis(SPRT_HANDSHAKE_TIMEOUT_MS),
        )?;
        engine.send(&format!(
            "setoption name {}_Variant value {}",
            SPRT_PROTOCOL.to_uppercase(), variant,
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
        self.send(&format!("{}newgame", SPRT_PROTOCOL))?;
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

/// SPRTChild::drop
///
/// Stops an engine. It sends `quit` and waits `SPRT_SHUTDOWN_TIMEOUT_MS`.
/// If the engine does not stop, it kills it. A run plays hundreds of
/// games, so old engines must not stay.
///
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

/*----------------------------------------------------------------------------*\
                                  GAME REFEREE
\*----------------------------------------------------------------------------*/

/// SPRTGameOutcome
///
/// The end of one game. Each score is for White. `WHITE` is 0 and `BLACK`
/// is 1, so a side that loses by its own fault scores its own colour.
///
/// - `Score(1.0)` : White won, by a rule or because Black did not reply
/// - `Score(0.5)` : a draw by a rule of the variant
/// - `Score(0.0)` : Black won
/// - `EngineLoss` : one engine failed, it loses and restarts
/// - `Aborted`    : the two engines failed, no score, the run stops
///
enum SPRTGameOutcome {
    Score(f64),                                                                 /* a played game, White's view        */
    EngineLoss {
        score: f64,                                                             /* the loss, still White's view       */
        side: u8,                                                               /* which child has to be restarted    */
        error: SPRTChildError,                                                  /* why it needs restarting            */
    },
    Aborted(String),                                                            /* both gone; nothing to score        */
}

/// GameManager
///
/// One match slot: two engines and the referee board. The board is an
/// engine `State` forked from the loaded variant, and its history is the
/// game. Each ply, the move list for the engines comes from that history,
/// so there is no second copy.
///
/// `white` and `black` give the current colours. The two games of a pair
/// use one random opening and swap the engines:
///
/// - game 1 : A as White, B as Black, score for White
/// - game 2 : B as White, A as Black, score flipped back to A
///
/// The referee `State` also goes to the board view, so a person can watch
/// the match.
///
struct GameManager {
    state: State,                                                               /* neutral referee; history is game   */
    white: SPRTChild,                                                           /* child currently playing White      */
    black: SPRTChild,                                                           /* child currently playing Black      */
}

/// GameManager driver
///
/// Set up a match slot and play one game in it.
///
/// - new         : start the two engines and fork the referee board
/// - swap_colors : swap the colours of the two engines
/// - restart     : replace a failed engine with the same binary
/// - reset_to    : fork the board again and play the pair opening
/// - play        : play one game and give its end
///
/// `play` is the referee. All game ends go through it:
///
/// - rules         : `game_outcome`, or `adjudicate_no_move` if no move
/// - no valid move : `bestmove (none)`, no reply or illegal move loses
/// - flag          : with a clock, the measured wall time is charged
/// - failed engine : a loss for it, no score if the two failed
/// - interrupt     : loss if in check or no move loses, else a draw
///
/// new
///
///   Params:
///   - template: &State                  -> variant to fork as the referee
///   - binary_a: &str                    -> engine that starts as White
///   - binary_b: &str                    -> engine that starts as Black
///   - variant : &str                    -> variant of the two engines
///
///   Return:
///   Result<GameManager, SPRTChildError> -> the slot, or the setup error
///
/// swap_colors
///
///   No parameters and no return value.
///
/// restart
///
///   Params:
///   - side   : u8              -> colour of the engine to replace
///   - variant: &str            -> variant of the new engine
///
///   Return:
///   Result<(), SPRTChildError> -> Ok when ready, else the error
///
/// reset_to
///
///   Params:
///   - template: &State  -> variant to fork the board from
///   - opening : &[Move] -> the shared pair opening to play
///
///   Return:
///   Result<(), String>  -> Ok when the two reset, else the failed engine
///
/// play
///
///   Params:
///   - dict        : Option<&Translator> -> notation of the two engines
///   - startpos    : &str                -> start position of the variant
///   - time_control: SPRTTimeControl     -> time for each move or game
///
///   Return:
///   SPRTGameOutcome                     -> the end of the game
///
/// Notes:
/// The flag uses the wall time that the harness measures, so the engine
/// also pays for its I/O.
///
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
    ) -> SPRTGameOutcome {
        let state = &mut self.state;

        let mut clocks = match time_control {
            SPRTTimeControl::Clock { base_ms, .. } => [base_ms, base_ms],
            SPRTTimeControl::MoveTime(..) => [0, 0],
        };

        loop {
            let terminal = game_outcome(state).0;
            if terminal != ONGOING {
                return SPRTGameOutcome::Score(
                    game_result_score(terminal)
                );
            }
            if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
                let score = if is_in_check!(state.playing, state)
                    || state.termination.stalemate == Outcome::Loss
                {
                    state.playing as f64
                } else {
                    0.5
                };
                return SPRTGameOutcome::Score(score);
            }
            if legal_moves!(state).is_empty() {
                let result = adjudicate_no_move(state);
                return SPRTGameOutcome::Score(
                    game_result_score(result)
                );
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

            emit(EngineEvent::Board(BoardState::from_state(state, dict)));
        }
    }
}

/*----------------------------------------------------------------------------*\
                       SEQUENTIAL PROBABILITY RATIO TEST
\*----------------------------------------------------------------------------*/

/// SPRT statistics
///
/// The statistics of the test, three pure functions. Two convert between
/// Elo and score. The third calculates the evidence.
///
/// - expected_score       : `1 / (1 + 10 ^ (-elo / 400))`
/// - elo_from_score       : `-400 · log10(1 / score - 1)`
/// - log_likelihood_ratio : all evidence as one number
///
/// The hypotheses are in Elo, and `expected_score` converts them to scores
/// once. The ratio is the normalised form over game pairs:
///
/// ```text
///          pairs · (mu_1 - mu_0) · (2 · mean - mu_0 - mu_1)
///   LLR =  ───────────────────────────────────────────────
///                          2 · variance
/// ```
///
/// expected_score
///
///   Params:
///   - elo: f64 -> Elo advantage to convert
///
///   Return:
///   f64        -> expected score for each game, 0 to 1
///
/// elo_from_score
///
///   Params:
///   - score: f64 -> observed score for each game
///
///   Return:
///   f64          -> Elo difference, score clamped away from 0 and 1
///
/// log_likelihood_ratio
///
///   Params:
///   - pairs   : f64 -> number of played pairs
///   - mean    : f64 -> mean of the pair scores
///   - variance: f64 -> population variance of the pair scores
///   - mu_zero : f64 -> expected score if the patch changed nothing
///   - mu_one  : f64 -> expected score if the patch gained the claim
///
///   Return:
///   f64             -> the ratio, or zero without variance
///
/// Notes:
/// The two games of a pair use one opening with swapped colours. Thus the
/// opening quality cancels, the variance is smaller, and the ratio grows
/// faster. With zero variance, the ratio is zero, not a division by zero.
///
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
/// Converts the score of one game into win, draw and loss counts. The log
/// and the result file count games.
///
/// - above 0.75   : `(1, 0, 0)`, a win
/// - 0.25 to 0.75 : `(0, 1, 0)`, a draw
/// - below 0.25   : `(0, 0, 1)`, a loss
///
/// Params:
/// - score: f64    -> score of one game, for the counted engine
///
/// Return:
/// (u32, u32, u32) -> values to add to the win, draw and loss counts
///
/// Notes:
/// The bands are wide, because the scores went through float arithmetic.
///
fn game_score_bucket(score: f64) -> (u32, u32, u32) {
    if score > 0.75 {
        (1, 0, 0)
    } else if score < 0.25 {
        (0, 0, 1)
    } else {
        (0, 1, 0)
    }
}

/*----------------------------------------------------------------------------*\
                                  RESULT FILES
\*----------------------------------------------------------------------------*/

/// write_result_file
///
/// Saves the result of the run. The function first rolls the old result to
/// a timestamped name and keeps `SPRT_HISTORY_KEEP` backups:
///
/// ```text
/// res/sprt/standard/latest.sprt
/// res/sprt/standard/2026-09-06_14-02-11.sprt
/// res/sprt/standard/2026-09-05_09-31-40.sprt
/// ```
///
/// The file has all data to read the verdict without the command line:
///
/// ```text
/// engine A: ./old
/// engine B: ./new
/// variant: standard
/// time control: clock 8000+80ms
/// elo bounds: [0, 5]  alpha: 0.05  beta: 0.05
/// every figure below is from engine A's view
/// result (A): 118W 96L 214D
/// LLR: 2.951
/// verdict: H1 accepted (./old is stronger)
/// ```
///
/// Params:
/// - variant     : &str            -> variant, selects the folder
/// - binary_a    : &str            -> path of the first engine
/// - binary_b    : &str            -> path of the second engine
/// - time_control: SPRTTimeControl -> time control of the games
/// - h0          : f64             -> Elo difference to reject
/// - h1          : f64             -> Elo difference to accept
/// - wins        : u32             -> wins of engine A
/// - draws       : u32             -> draws of engine A
/// - losses      : u32             -> losses of engine A
/// - llr         : f64             -> last value of the ratio
/// - verdict     : &str            -> reason for the stop
///
/// Notes:
/// The function also writes a file when no game was played. Thus the old
/// file is never the current answer.
///
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

    let path = format!("{}/latest.sprt", dir);

    let body = format!(
        "engine A: {}\nengine B: {}\nvariant: {}\ntime control: {}\n\
         elo bounds: [{}, {}]  alpha: {}  beta: {}\n\
         every figure below is from engine A's view\n\
         result (A): {}W {}L {}D\nLLR: {:.3}\nverdict: {}\n",
        binary_a, binary_b, variant, time_control,
        h0, h1, SPRT_ALPHA, SPRT_BETA,
        wins, losses, draws, llr, verdict,
    );

    roll_latest(&dir, "", "sprt");
    fs::write(&path, body).unwrap_or_else(|e| {
        panic!("Failed to write SPRT result {}: {}", path, e)
    });
    prune_backups(&dir, "", "sprt", SPRT_HISTORY_KEEP);

    log_1!("SPRT result written to {}", path);
}

/// harvest_child_logs
///
/// Copies the log of each engine out of its sandbox before the next run
/// deletes it. An engine writes `logs/latest.log`, so this function adds
/// the engine label:
///
/// ```text
/// /tmp/anekamacam-sprt/_home_me_old/logs/latest.log
///     → res/sprt/standard/engine-a_latest.log
///
/// /tmp/anekamacam-sprt/_home_me_new/logs/latest.log
///     → res/sprt/standard/engine-b_latest.log
/// ```
///
/// Params:
/// - variant : &str -> variant, selects the folder
/// - binary_a: &str -> first engine, saved as `engine-a`
/// - binary_b: &str -> second engine, saved as `engine-b`
///
/// Notes:
/// Each label has its own rolled history. A sandbox without a log is
/// skipped: the engine did not start, or the two sides are one binary. The
/// function runs only after the two engines stop, so the logs are flushed.
///
fn harvest_child_logs(variant: &str, binary_a: &str, binary_b: &str) {
    let dir = format!("{}/{}", SPRT_DIR, variant);

    for (label, binary) in [("engine-a", binary_a), ("engine-b", binary_b)] {
        let Ok(sandbox) = engine_sandbox(binary) else {
            continue;
        };
        let source = sandbox.join("logs").join("latest.log");
        if !source.exists() {
            continue;
        }

        let prefix = format!("{}_", label);
        let destination = format!("{}/{}latest.log", dir, prefix);

        roll_latest(&dir, &prefix, "log");
        if let Err(error) = fs::copy(&source, &destination) {
            log_2!("Failed to harvest {} log: {}", label, error);
            continue;
        }
        prune_backups(&dir, &prefix, "log", SPRT_HISTORY_KEEP);
    }
}

/*----------------------------------------------------------------------------*\
                                  MATCH RUNNER
\*----------------------------------------------------------------------------*/

/// run_sprt
///
/// Runs the full test, from the sandbox clear to the saved verdict.
///
/// 1. clear the two sandboxes, so no old parameters stay
/// 2. start the two engines on the variant, one thread each
/// 3. make one random opening for each pair
/// 4. play the pair, with swapped colours
/// 5. add the pair score to the mean and variance
/// 6. stop at a bound, or start the next pair
///
/// Params:
/// - template    : &State          -> loaded variant, forked as referee
/// - variant     : &str            -> variant name, for setup and output
/// - binary_a    : &str            -> path of the first engine binary
/// - binary_b    : &str            -> path of the second engine binary
/// - time_control: SPRTTimeControl -> time control of each game
/// - max_games   : usize           -> game limit
/// - h0          : f64             -> Elo difference to reject
/// - h1          : f64             -> Elo difference to accept
///
/// Notes:
/// The referee uses the same translator as the engines. A variant without
/// a dictionary is an error. Each stop, also a setup failure, a double
/// engine failure, an interrupt or the game limit, writes a result file.
///
pub fn run_sprt(
    template: &State,
    variant: &str,
    binary_a: &str,
    binary_b: &str,
    time_control: SPRTTimeControl,
    max_games: usize,
    h0: f64,
    h1: f64,
) {
    let translator = Translator::find(variant, SPRT_PROTOCOL);

    if translator.is_none() {
        log_4!(
            "Variant {variant} doesn't support {SPRT_PROTOCOL} for SPRT yet!"
        );
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

        let mut opening_state = template.fork();                                /* one scratch board for each pair    */
        opening_state.play_random_opening(OPENING_RANDOM_PLIES);                /* the pair games share this opening  */
        let opening: Vec<Move> = opening_state.history
            .iter().map(|snapshot| snapshot.move_ply.clone()).collect();

        if let Err(error) = manager.reset_to(template, &opening) {
            verdict = format!("aborted during game setup: {}", error);
            break;
        }

        let first = manager.play(dict, &startpos, time_control);
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
        let second = manager.play(dict, &startpos, time_control);
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
                "SPRT A={} {}W {}L {}D | A elo {:.1} | LLR {:.2} [{:.2}, {:.2}]",
                binary_a, wins, losses, draws, elo_from_score(mean),
                llr, lower, upper,
            );
        }

        if let Some(error) = abort_after_pair {
            verdict = error;
            break;
        }

        if llr >= upper {
            verdict = format!("H1 accepted ({} is stronger)", binary_a);
            break;
        }
        if llr <= lower {
            verdict = format!("H0 accepted ({} is not stronger)", binary_a);
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

    drop(manager);
    harvest_child_logs(variant, binary_a, binary_b);
}
