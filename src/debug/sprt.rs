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

/// run_sandbox_root
///
/// Gives the folder of all engine sandboxes of this run. The process id
/// makes it private, so two runs at the same time do not share files.
///
/// Return:
/// PathBuf -> the folder, `/tmp/anekamacam-sprt/<pid>`
///
fn run_sandbox_root() -> PathBuf {
    env::temp_dir()
        .join("anekamacam-sprt")
        .join(std::process::id().to_string())
}

/// engine_sandbox
///
/// Gives the private work directory of one engine in one match slot. Each
/// engine runs in it, so `res/param` has its own exports or its embedded
/// defaults, and `logs/latest.log` has one writer. With shared files, the
/// two engines would have the same parameters, and two engines that roll
/// one log at the same time crash at startup.
///
/// ```text
/// /tmp/anekamacam-sprt/4242/3-engine-b
///                      ^    ^ ^
///                      |    | the side label
///                      |    the match slot
///                      the process id of the run
/// ```
///
/// Params:
/// - slot : usize -> index of the match slot
/// - label: &str  -> `engine-a` or `engine-b`
///
/// Return:
/// PathBuf        -> the sandbox path
///
fn engine_sandbox(slot: usize, label: &str) -> PathBuf {
    run_sandbox_root().join(format!("{}-{}", slot, label))
}

/*----------------------------------------------------------------------------*\
                                  ENGINE SPEC
\*----------------------------------------------------------------------------*/

/// SPRTEngine
///
/// One side of the match: a binary and the options it gets after the
/// variant. The options set a reference engine, for example a strength
/// limit, without a wrapper script.
///
/// - binary  : path of the engine binary
/// - options : `setoption` name and value pairs, sent in order
///
#[derive(Clone)]
pub struct SPRTEngine {
    pub binary: String,                                                         /* executable path                    */
    pub options: Vec<(String, String)>,                                         /* setoption pairs after the variant  */
}

/// parse_sprt_option
///
/// Reads one engine option from the command line. The first `=` separates
/// the name and the value.
///
/// - `Hash=64`      : `setoption name Hash value 64`
/// - `UCI_Elo=1965` : `setoption name UCI_Elo value 1965`
///
/// Params:
/// - value: &str                    -> the option as `name=value`
///
/// Return:
/// Result<(String, String), String> -> the name and value, or the bad text
///
pub fn parse_sprt_option(value: &str) -> Result<(String, String), String> {
    match value.split_once('=') {
        Some((name, option_value)) if !name.is_empty() => {
            Ok((name.to_string(), option_value.to_string()))
        }
        _ => Err(format!("Invalid engine option: {}", value)),
    }
}

/// SPRTEngine::fmt
///
/// Writes the binary and its options for the log and the result file.
///
/// ```text
/// fairy-stockfish (UCI_LimitStrength=true, UCI_Elo=1965)
/// ```
///
/// Params:
/// - formatter: &mut FmtFormatter<'_> -> the output
///
/// Return:
/// FmtResult                          -> the result of the output
///
impl Display for SPRTEngine {
    fn fmt(&self, formatter: &mut FmtFormatter<'_>) -> FmtResult {
        write!(formatter, "{}", self.binary)?;

        if self.options.is_empty() {
            return Ok(());
        }

        let options = self.options.iter()
            .map(|(name, value)| format!("{}={}", name, value))
            .collect::<Vec<_>>()
            .join(", ");

        write!(formatter, " ({})", options)
    }
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
/// - engine  : binary and options, to start it again after a failure
/// - sandbox : its work folder, kept for a restart
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
    engine: SPRTEngine,                                                        /* binary and options, for restarts   */
    sandbox: PathBuf,                                                           /* private work folder, for restarts  */
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
///   - engine : &SPRTEngine            -> binary and options to start
///   - variant: &str                   -> variant to select on UCI
///   - sandbox: &Path                  -> work folder of the engine
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
///   Result<(Option<String>, String), SPRTChildError> -> move, last score
///
/// Notes:
/// `spawn` runs each engine in its own `engine_sandbox` with one thread, so
/// the two do not compete for cores. The options of the engine come after
/// the thread count, so they can change it. A failure is returned, not
/// raised. An engine that fails loses the game and restarts. Only a failure
/// of the two engines at once ends the run.
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

    fn spawn(
        engine: &SPRTEngine,
        variant: &str,
        sandbox: &Path,
    ) -> Result<SPRTChild, SPRTChildError> {
        let binary = engine.binary.as_str();
        let executable = fs::canonicalize(binary).map_err(|error| {
            Self::setup_error(binary, "path resolution", error.to_string())
        })?;

        fs::create_dir_all(sandbox).map_err(|error| {
            Self::setup_error(
                binary,
                "sandbox creation",
                format!("{}: {}", sandbox.display(), error),
            )
        })?;

        let mut process = Command::new(executable)
            .current_dir(sandbox)
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

        let mut child = SPRTChild {
            engine: engine.clone(),
            sandbox: sandbox.to_path_buf(),
            process,
            input,
            output: Self::output_reader(output),
            errors,
        };

        child.send(SPRT_PROTOCOL)?;
        child.wait_for(
            &format!("{}ok", SPRT_PROTOCOL),
            Duration::from_millis(SPRT_HANDSHAKE_TIMEOUT_MS),
        )?;
        child.send(&format!(
            "setoption name {}_Variant value {}",
            SPRT_PROTOCOL.to_uppercase(), variant,
        ))?;
        child.send(&format!("setoption name {} value 1", OPT_THREADS))?;
        for (name, value) in &engine.options {
            child.send(&format!("setoption name {} value {}", name, value))?;
        }
        child.send("isready")?;
        child.wait_for(
            "readyok",
            Duration::from_millis(SPRT_HANDSHAKE_TIMEOUT_MS),
        )?;

        Ok(child)
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
            binary: self.engine.binary.clone(),
            action: action.to_string(),
            detail,
            status,
            stderr,
        }
    }

    fn exited_error(&mut self, action: &str) -> Option<SPRTChildError> {
        match self.process.try_wait() {
            Ok(Some(status)) => Some(SPRTChildError {
                binary: self.engine.binary.clone(),
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
    ) -> Result<(Option<String>, String), SPRTChildError> {
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
        let mut score = "-".to_string();

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
                return Ok((tokens.next().map(str::to_string), score));
            }
            if let Some((_, tail)) = line.split_once(" score ") {
                score = tail.split_whitespace().take(2)
                    .collect::<Vec<_>>().join(" ");
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
/// - `Stopped`    : the run has a verdict, the game has no use
///
enum SPRTGameOutcome {
    Score(f64),                                                                 /* a played game, White's view        */
    EngineLoss {
        score: f64,                                                             /* the loss, still White's view       */
        side: u8,                                                               /* which child has to be restarted    */
        error: SPRTChildError,                                                  /* why it needs restarting            */
    },
    Aborted(String),                                                            /* both gone; nothing to score        */
    Stopped,                                                                    /* verdict found in another slot      */
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
/// The referee `State` of slot 0 also goes to the board view, so a person
/// can watch one game of the match.
///
/// `record` has one `fen;move;score` row for each engine move of the game.
///
struct GameManager {
    slot: usize,                                                                /* index of the slot, 0 is watched    */
    state: State,                                                               /* neutral referee; history is game   */
    white: SPRTChild,                                                           /* child currently playing White      */
    black: SPRTChild,                                                           /* child currently playing Black      */
    record: Vec<String>,                                                        /* rows of the current game           */
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
/// - stop          : no score, the run already has a verdict
///
/// new
///
///   Params:
///   - slot    : usize                   -> index of the slot
///   - template: &State                  -> variant to fork as the referee
///   - engine_a: &SPRTEngine             -> engine that starts as White
///   - engine_b: &SPRTEngine             -> engine that starts as Black
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
///   - stop        : &AtomicBool         -> set when the run has a verdict
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
        slot: usize,
        template: &State,
        engine_a: &SPRTEngine,
        engine_b: &SPRTEngine,
        variant: &str,
    ) -> Result<GameManager, SPRTChildError> {
        let white = SPRTChild::spawn(
            engine_a, variant, &engine_sandbox(slot, "engine-a"),
        )?;
        let black = SPRTChild::spawn(
            engine_b, variant, &engine_sandbox(slot, "engine-b"),
        )?;

        Ok(GameManager {
            slot,
            state: template.fork(),
            white,
            black,
            record: Vec::new(),
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
        let failed = if side == WHITE {
            &mut self.white
        } else {
            &mut self.black
        };

        let _ = failed.process.kill();                                          /* one log writer for each sandbox    */
        let _ = failed.process.wait();

        *failed = SPRTChild::spawn(
            &failed.engine.clone(), variant, &failed.sandbox.clone(),
        )?;

        Ok(())
    }

    fn reset_to(
        &mut self,
        template: &State,
        opening: &[Move],
    ) -> Result<(), String> {
        self.state = template.fork();
        self.record.clear();
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
        stop: &AtomicBool,
    ) -> SPRTGameOutcome {
        let state = &mut self.state;

        let mut clocks = match time_control {
            SPRTTimeControl::Clock { base_ms, .. } => [base_ms, base_ms],
            SPRTTimeControl::MoveTime(..) => [0, 0],
        };

        loop {
            if stop.load(Ordering::Relaxed) {
                return SPRTGameOutcome::Stopped;
            }
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
                Ok((Some(text), score)) if text != "(none)" => {
                    self.record.push(format!(
                        "{};{};{}", format_fen(state, dict), text, score,
                    ));
                    text
                }
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
            let mover = if side == WHITE { &self.white } else { &self.black };

            if let SPRTTimeControl::Clock { inc_ms, .. } = time_control {
                let spent = move_start.elapsed().as_millis();
                let clock = &mut clocks[side as usize];

                if spent > *clock {
                    log_1!(
                        "SPRT slot {}: {} lost on time",
                        self.slot, mover.engine.binary,
                    );
                    return SPRTGameOutcome::Score(side as f64);
                }

                *clock = *clock - spent + inc_ms;
            }

            let parsed = parse_move(&move_string, state, dict);
            if !parsed.is_some_and(|played| make_move!(state, played)) {
                log_1!(
                    "SPRT slot {}: {} lost by illegal move {}",
                    self.slot, mover.engine.binary, move_string,
                );
                return SPRTGameOutcome::Score(side as f64);
            }

            if self.slot == 0 {
                emit(EngineEvent::Board(BoardState::from_state(state, dict)));
            }
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

/// elo_margin
///
/// Gives the half width of the 95% interval of the Elo difference. The
/// interval uses the pair scores, so it has the same variance as the ratio.
/// A rating benchmark reads the difference to a reference engine as
/// `elo ± margin`.
///
/// ```text
/// margin = (elo(mean + 1.96 · se) - elo(mean - 1.96 · se)) / 2
/// se     = sqrt(variance / pairs)
/// ```
///
/// Params:
/// - pairs   : f64 -> number of played pairs
/// - mean    : f64 -> mean of the pair scores
/// - variance: f64 -> population variance of the pair scores
///
/// Return:
/// f64             -> the half width in Elo
///
fn elo_margin(pairs: f64, mean: f64, variance: f64) -> f64 {
    let standard_error = (variance.max(0.0) / pairs.max(1.0)).sqrt();
    let high = elo_from_score(mean + 1.96 * standard_error);
    let low = elo_from_score(mean - 1.96 * standard_error);

    (high - low) / 2.0
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
/// engine B: fairy-stockfish (UCI_LimitStrength=true, UCI_Elo=1965)
/// variant: standard
/// time control: clock 10000+100ms
/// concurrency: 8
/// elo bounds: [0, 5]  alpha: 0.05  beta: 0.05
/// every figure below is from engine A's view
/// result (A): 118W 96L 214D
/// pentanomial (A): [9, 41, 82, 65, 17]
/// elo (A): 17.9 +/- 21.3
/// LLR: 2.951
/// verdict: H1 accepted (./old is stronger)
/// ```
///
/// Params:
/// - settings: &SPRTMatch -> engines, variant, control and bounds
/// - tally   : &SPRTTally -> games and pair scores of engine A
/// - llr     : f64        -> last value of the ratio
/// - verdict : &str       -> reason for the stop
///
/// Notes:
/// The function also writes a file when no game was played. Thus the old
/// file is never the current answer.
///
fn write_result_file(
    settings: &SPRTMatch,
    tally: &SPRTTally,
    llr: f64,
    verdict: &str,
) {
    let dir = format!("{}/{}", SPRT_DIR, settings.variant);
    fs::create_dir_all(&dir).unwrap_or_else(|e| {
        panic!("Failed to create SPRT directory {}: {}", dir, e)
    });

    let path = format!("{}/latest.sprt", dir);

    let body = format!(
        "engine A: {}\nengine B: {}\nvariant: {}\ntime control: {}\n\
         concurrency: {}\n\
         elo bounds: [{}, {}]  alpha: {}  beta: {}\n\
         every figure below is from engine A's view\n\
         result (A): {}W {}L {}D\npentanomial (A): {:?}\n\
         elo (A): {:.1} +/- {:.1}\nLLR: {:.3}\nverdict: {}\n",
        settings.engine_a, settings.engine_b, settings.variant,
        settings.time_control, settings.concurrency,
        settings.h0, settings.h1, SPRT_ALPHA, SPRT_BETA,
        tally.wins, tally.losses, tally.draws, tally.pentanomial,
        elo_from_score(tally.mean()), tally.margin(), llr, verdict,
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
/// Copies the log of each engine out of its sandbox, then deletes the
/// sandboxes of the run. An engine writes `logs/latest.log`, so this
/// function adds the slot and the engine label:
///
/// ```text
/// /tmp/anekamacam-sprt/4242/0-engine-a/logs/latest.log
///     → res/sprt/standard/0-engine-a_latest.log
///
/// /tmp/anekamacam-sprt/4242/3-engine-b/logs/latest.log
///     → res/sprt/standard/3-engine-b_latest.log
/// ```
///
/// Params:
/// - variant    : &str  -> variant, selects the folder
/// - concurrency: usize -> number of match slots of the run
///
/// Notes:
/// Each slot and label has its own rolled history. A sandbox without a log
/// is skipped: the engine did not start, or it does not write logs. The
/// function runs only after all engines stop, so the logs are flushed.
///
fn harvest_child_logs(variant: &str, concurrency: usize) {
    let dir = format!("{}/{}", SPRT_DIR, variant);

    for slot in 0..concurrency {
        for label in ["engine-a", "engine-b"] {
            let source = engine_sandbox(slot, label)
                .join("logs")
                .join("latest.log");
            if !source.exists() {
                continue;
            }

            let prefix = format!("{}-{}_", slot, label);
            let destination = format!("{}/{}latest.log", dir, prefix);

            roll_latest(&dir, &prefix, "log");
            if let Err(error) = fs::copy(&source, &destination) {
                log_2!("Failed to harvest {}{} log: {}", slot, label, error);
                continue;
            }
            prune_backups(&dir, &prefix, "log", SPRT_HISTORY_KEEP);
        }
    }

    let _ = fs::remove_dir_all(run_sandbox_root());
}

/*----------------------------------------------------------------------------*\
                                  MATCH RUNNER
\*----------------------------------------------------------------------------*/

/// SPRTMatch
///
/// All settings of one run. The command line fills it, and all match slots
/// read it at the same time.
///
/// - variant      : variant name, for setup and output
/// - engine_a     : the build to test
/// - engine_b     : the reference engine
/// - time_control : time control of each game
/// - max_games    : game limit
/// - h0           : Elo difference to reject
/// - h1           : Elo difference to accept
/// - concurrency  : number of games at the same time
///
pub struct SPRTMatch {
    pub variant: String,                                                        /* variant name, selects the folder   */
    pub engine_a: SPRTEngine,                                                   /* the build to test                  */
    pub engine_b: SPRTEngine,                                                   /* the reference engine               */
    pub time_control: SPRTTimeControl,                                          /* move time or clock of each game    */
    pub max_games: usize,                                                       /* stop without a verdict after this  */
    pub h0: f64,                                                                /* Elo difference to reject           */
    pub h1: f64,                                                                /* Elo difference to accept           */
    pub concurrency: usize,                                                     /* match slots, one game in each      */
}

/// SPRTTally
///
/// The counts of the run, from the view of engine A. The game counts are
/// for the person. The pentanomial counts are for the ratio and the Elo
/// interval.
///
/// A pair score is the mean of two games, so it has only five values. The
/// count of each value gives all pair statistics:
///
/// ```text
/// pentanomial[i] = pairs that scored i / 4, for i = 0 to 4
/// pairs          = Σ pentanomial[i]
/// mean           = Σ pentanomial[i] · (i / 4)     / pairs
/// variance       = Σ pentanomial[i] · (i / 4)²    / pairs - mean²
/// ```
///
/// - add_game : count one game as a win, a draw or a loss
/// - add_pair : count one pair in its pentanomial bucket
/// - pairs    : number of finished pairs
/// - mean     : mean pair score, 0.5 before the first pair
/// - variance : population variance of the pair scores
/// - margin   : half width of the 95% Elo interval
///
/// add_game, add_pair
///
///   Params:
///   - score: f64 -> score of engine A, 0 to 1
///
/// pairs, mean, variance, margin
///
///   Return:
///   f64 -> the statistic
///
#[derive(Default)]
struct SPRTTally {
    wins: u32,                                                                  /* games won by engine A              */
    draws: u32,                                                                 /* games drawn                        */
    losses: u32,                                                                /* games lost by engine A             */
    pentanomial: [u32; 5],                                                      /* pairs by score 0, 1/4, ..., 1      */
}

impl SPRTTally {
    fn add_game(&mut self, score: f64) {
        let (win, draw, loss) = game_score_bucket(score);

        self.wins += win;
        self.draws += draw;
        self.losses += loss;
    }

    fn add_pair(&mut self, score: f64) {
        self.pentanomial[(score * 4.0).round().clamp(0.0, 4.0) as usize] += 1;
    }

    fn pairs(&self) -> f64 {
        self.pentanomial.iter().sum::<u32>() as f64
    }

    fn mean(&self) -> f64 {
        if self.pairs() == 0.0 {
            return 0.5;
        }

        zip(0.., self.pentanomial)
            .map(|(bucket, count)| count as f64 * bucket as f64 / 4.0)
            .sum::<f64>()
            / self.pairs()
    }

    fn variance(&self) -> f64 {
        if self.pairs() == 0.0 {
            return 0.0;
        }

        zip(0.., self.pentanomial)
            .map(|(bucket, count)| {
                count as f64 * (bucket as f64 / 4.0).powi(2)
            })
            .sum::<f64>()
            / self.pairs()
            - self.mean().powi(2)
    }

    fn margin(&self) -> f64 {
        elo_margin(self.pairs(), self.mean(), self.variance())
    }
}

/// SPRTReport
///
/// A message from a match slot to the runner. The runner owns the tally,
/// so the slots do not share counts.
///
/// - `Game(1.0, 0.0, rows)` : engine A won as Black, with the game rows
/// - `Pair(0.75)`           : engine A scored 1.5 of 2 in one pair
/// - `Stop(reason)`         : the slot cannot continue, the verdict
///
enum SPRTReport {
    Game(f64, f64, Vec<String>),                                                /* score of A, of White, and the rows */
    Pair(f64),                                                                  /* mean score of the pair for A       */
    Stop(String),                                                               /* the slot failed, the run stops     */
}

/// run_slot
///
/// Plays pairs in one match slot until the game limit or a stop. The slots
/// take pair numbers from one counter, so the total does not go above the
/// limit.
///
/// 1. start the two engines in the sandboxes of the slot
/// 2. take the next pair number, or stop at the limit
/// 3. make one random opening for the pair
/// 4. play the pair, with swapped colours
/// 5. send each game score and the pair score to the runner
///
/// Params:
/// - slot     : usize               -> index of the slot
/// - template : &State              -> loaded variant, forked as referee
/// - settings : &SPRTMatch          -> engines, control and limit
/// - dict     : Option<&Translator> -> notation of the two engines
/// - startpos : &str                -> start position in engine notation
/// - next_pair: &AtomicUsize        -> number of the next pair to play
/// - stop     : &AtomicBool         -> set when the run has a verdict
/// - reports  : Sender<SPRTReport>  -> channel to the runner
///
/// Notes:
/// A pair that the stop cuts is not sent. Its first game is already in the
/// win, draw and loss counts, but not in the ratio.
///
fn run_slot(
    slot: usize,
    template: &State,
    settings: &SPRTMatch,
    dict: Option<&Translator>,
    startpos: &str,
    next_pair: &AtomicUsize,
    stop: &AtomicBool,
    reports: Sender<SPRTReport>,
) {
    let variant = settings.variant.as_str();
    let mut manager = match GameManager::new(
        slot, template, &settings.engine_a, &settings.engine_b, variant,
    ) {
        Ok(manager) => manager,
        Err(error) => {
            let _ = reports.send(SPRTReport::Stop(
                format!("aborted during setup: {}", error)
            ));
            return;
        }
    };

    while !stop.load(Ordering::Relaxed)
        && !SYSTEM_INTERRUPT.load(Ordering::Relaxed)
    {
        if next_pair.fetch_add(1, Ordering::Relaxed) >= settings.max_games / 2
        {
            return;
        }

        let mut opening_state = template.fork();                                /* one scratch board for each pair    */
        opening_state.play_random_opening(OPENING_RANDOM_PLIES);                /* the pair games share this opening  */
        let opening: Vec<Move> = opening_state.history
            .iter().map(|snapshot| snapshot.move_ply.clone()).collect();

        let mut pair_score = 0.0;
        for game in 0..2 {
            if let Err(error) = manager.reset_to(template, &opening) {
                let _ = reports.send(SPRTReport::Stop(
                    format!("aborted during game setup: {}", error)
                ));
                return;
            }

            let outcome = manager.play(
                dict, startpos, settings.time_control, stop,
            );
            let (score, restart) = match outcome {
                SPRTGameOutcome::Score(score) => (score, Ok(())),
                SPRTGameOutcome::EngineLoss { score, side, error } => {
                    log_1!("SPRT scored engine loss: {}", error);
                    (score, manager.restart(side, variant))
                }
                SPRTGameOutcome::Aborted(error) => {
                    let _ = reports.send(SPRTReport::Stop(
                        format!("aborted during game: {}", error)
                    ));
                    return;
                }
                SPRTGameOutcome::Stopped => return,
            };
            let score_a = if game == 0 { score } else { 1.0 - score };          /* game 2 has B as White              */

            let _ = reports.send(SPRTReport::Game(
                score_a, score, mem::take(&mut manager.record),
            ));
            pair_score += score_a / 2.0;
            manager.swap_colors();

            if let Err(error) = restart {
                let _ = reports.send(SPRTReport::Stop(format!(
                    "aborted after engine loss; restart failed: {}", error,
                )));
                return;
            }
        }

        let _ = reports.send(SPRTReport::Pair(pair_score));
    }
}

/// run_sprt
///
/// Runs the full test, from the engine start to the saved verdict. Each
/// match slot is one thread with its own two engines, so the run plays
/// `concurrency` games at the same time.
///
/// 1. start `concurrency` slots, each with `run_slot`
/// 2. add each reported game and pair to the tally
/// 3. after each pair, calculate the ratio again
/// 4. at a bound or a slot failure, stop all slots
/// 5. write the result file and copy the engine logs
///
/// Each game also goes to `res/sprt/{variant}/latest.games`, one row for
/// each engine move, in the `datagen` format with the move and the score of
/// the engine that moved between the position and the result:
///
/// ```text
/// 12;<fen>;e2e4;cp 34;1
/// ```
///
/// Params:
/// - template: &State     -> loaded variant, forked as referee
/// - settings: &SPRTMatch -> engines, variant, control, bounds and slots
///
/// Notes:
/// The referee uses the same translator as the engines. A variant without
/// a dictionary is an error. The start position goes through the
/// translator, so a reference engine reads it in its own notation. Each
/// stop, also a setup failure, a double engine failure, an interrupt or
/// the game limit, writes a result file. Each engine has one thread, so
/// `concurrency` must stay below the number of cores.
///
pub fn run_sprt(template: &State, settings: &SPRTMatch) {
    let variant = settings.variant.as_str();
    let translator = Translator::find(variant, SPRT_PROTOCOL);

    if translator.is_none() {
        log_4!(
            "Variant {variant} doesn't support {SPRT_PROTOCOL} for SPRT yet!"
        );
        return;
    }

    let dict = translator.as_ref();
    let startpos = format_fen(template, dict);

    let mu_0 = expected_score(settings.h0);
    let mu_1 = expected_score(settings.h1);
    let upper = ((1.0 - SPRT_BETA) / SPRT_ALPHA).ln();
    let lower = (SPRT_BETA / (1.0 - SPRT_ALPHA)).ln();

    let mut tally = SPRTTally::default();
    let mut llr = 0.0f64;
    let mut verdict = None;
    let next_pair = AtomicUsize::new(0);
    let stop = AtomicBool::new(false);
    let (sender, receiver) = channel();
    let dir = format!("{}/{}", SPRT_DIR, variant);
    let _ = fs::create_dir_all(&dir);
    roll_latest(&dir, "", "games");
    let mut game_file = fs::File::create(format!("{}/latest.games", dir))
        .unwrap_or_else(|e| panic!("Failed to open game record: {}", e));
    let mut games = 0usize;

    let _ = fs::remove_dir_all(run_sandbox_root());
    log_1!(
        "SPRT {} vs {} on {} | {} | {} slots",
        settings.engine_a, settings.engine_b, variant,
        settings.time_control, settings.concurrency,
    );

    thread::scope(|scope| {
        for slot in 0..settings.concurrency {
            let reports = sender.clone();
            let (startpos, next_pair, stop) = (&startpos, &next_pair, &stop);

            scope.spawn(move || run_slot(
                slot, template, settings, dict, startpos, next_pair, stop,
                reports,
            ));
        }
        drop(sender);

        for report in receiver {
            match report {
                SPRTReport::Game(score_a, white_score, rows) => {
                    tally.add_game(score_a);
                    for row in rows {
                        let _ = writeln!(
                            game_file, "{};{};{}", games, row, white_score,
                        );
                    }
                    games += 1;
                }
                SPRTReport::Pair(score) => {
                    tally.add_pair(score);
                    llr = log_likelihood_ratio(
                        tally.pairs(), tally.mean(), tally.variance(),
                        mu_0, mu_1,
                    );

                    if tally.pairs() as usize % 5 == 0 {
                        log_1!(
                            "SPRT {}W {}L {}D | A elo {:.1} +/- {:.1} | \
                             LLR {:.2} [{:.2}, {:.2}]",
                            tally.wins, tally.losses, tally.draws,
                            elo_from_score(tally.mean()), tally.margin(),
                            llr, lower, upper,
                        );
                    }

                    if llr >= upper {
                        verdict = Some(format!(
                            "H1 accepted ({} is stronger)",
                            settings.engine_a.binary,
                        ));
                    } else if llr <= lower {
                        verdict = Some(format!(
                            "H0 accepted ({} is not stronger)",
                            settings.engine_a.binary,
                        ));
                    }
                }
                SPRTReport::Stop(reason) => verdict = Some(reason),
            }

            if verdict.is_some() {
                stop.store(true, Ordering::Relaxed);
                break;
            }
        }
    });

    let verdict = verdict.unwrap_or_else(|| {
        if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
            "cancelled".to_string()
        } else {
            "inconclusive (game budget reached)".to_string()
        }
    });

    log_1!(
        "SPRT done: {} | {}W {}L {}D | elo {:.1} +/- {:.1} | LLR {:.3}",
        verdict, tally.wins, tally.losses, tally.draws,
        elo_from_score(tally.mean()), tally.margin(), llr,
    );

    write_result_file(settings, &tally, llr, &verdict);
    harvest_child_logs(variant, settings.concurrency);
}
