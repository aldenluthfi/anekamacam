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
//! loop with a self-contained debug command.
//!
//! Created: 05/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                HARNESS SETTINGS
\*----------------------------------------------------------------------------*/

/// SPRT harness settings
///
/// The fixed knobs of the match runner. Nothing here is per-run: the time
/// control, the Elo bounds, and the game budget all arrive as arguments,
/// while these stay the same from one test to the next.
///
/// ```text
/// SPRT_DIR                    where results land, one folder per variant
/// SPRT_HISTORY_KEEP           how many rolled results a folder keeps
/// SPRT_PROTOCOL               the dialect both children are spoken to in
/// SPRT_ALPHA                  odds of calling a patch good when it is not
/// SPRT_BETA                   odds of calling it not good when it is
/// SPRT_HANDSHAKE_TIMEOUT_MS   how long a child has to finish setting up
/// SPRT_RESPONSE_GRACE_MS      slack over the clock before a reply is late
/// SPRT_SHUTDOWN_TIMEOUT_MS    how long a child has to quit before it is
///                             killed instead
/// ```
///
/// The two error rates set the stopping bounds, which follow from them and
/// from nothing else, and are symmetric while the two rates are equal:
///
/// ```text
/// upper = ln((1 - beta) / alpha)   = +2.944 at 0.05 and 0.05
/// lower = ln(beta / (1 - alpha))   = -2.944 at 0.05 and 0.05
/// ```
///
/// The test runs until the ratio leaves that band, and reports the run as
/// inconclusive if the game budget runs out while it is still inside.
const SPRT_DIR: &str = "res/sprt";
const SPRT_HISTORY_KEEP: usize = 64;                                        /* rolled sprt files kept per family  */
const SPRT_PROTOCOL: &str = "uci";                                          /* dialect the sprt harness speaks    */
const SPRT_ALPHA: f64 = 0.05;
const SPRT_BETA: f64 = 0.05;
const SPRT_HANDSHAKE_TIMEOUT_MS: u64 = 10_000;
const SPRT_RESPONSE_GRACE_MS: u128 = 5_000;
const SPRT_SHUTDOWN_TIMEOUT_MS: u64 = 1_000;

/// engine_sandbox
///
/// Names the private working directory one engine binary runs in. Each child
/// is started with this as its current directory, so it resolves `res/param`
/// against its own exports, or against its embedded defaults where it has no
/// exports, rather than against the repository's. Sharing those files would
/// hand both engines the same numbers and make a parameter-changing patch
/// unmeasurable, which is the one thing the harness exists to measure.
///
/// ```text
/// /tmp/anekamacam-sprt/_Users_me_build_release_main
///      ^               ^
///      |               the binary's own path with every separator turned
///      |               into an underscore, so two builds never collide
///      one folder for the whole harness
/// ```
///
/// Params:
///
///     binary: &str
///     path to the engine executable
///
/// Return:
///
///     Result<PathBuf, String>
///     the sandbox path, or why the binary's own path could not be resolved
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
/// How much time a game gives each side. The choice decides which `go` line
/// the referee sends, and whether it has to keep clocks at all.
///
/// ```text
/// MoveTime(1000)
///     go movetime 1000
///     every move gets the same budget, and there is no clock to keep
///     because there is nothing that can run out
///
/// Clock { base_ms: 8000, inc_ms: 80 }
///     go wtime 8000 btime 8000 winc 80 binc 80
///     each side's own time management decides how to spend the bank, and
///     the referee scores an overstep as a loss for whoever flagged
/// ```
///
/// The second form is the one that measures time management, which is why
/// it exists at all: a fixed movetime hides every decision about when to
/// think longer, and a patch to that logic would test as no change.
#[derive(Clone, Copy)]
pub enum SPRTTimeControl {
    MoveTime(u128),                                                             /* fixed milliseconds per move        */
    Clock { base_ms: u128, inc_ms: u128 },                                      /* bank + increment, in milliseconds  */
}

/// parse_sprt_time_control
///
/// Reads a time control off the command line. A `+` is what tells the two
/// forms apart, and every figure is in milliseconds either way.
///
/// ```text
/// 1000      a fixed second a move
/// 8000+80   an eight-second bank, with eighty milliseconds added a move
/// ```
///
/// Params:
/// - value: &str                   -> fixed movetime, or base and increment
///
/// Return:
/// Result<SPRTTimeControl, String> -> the control, or the offending text
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
/// Names the control for the run's result file, so a saved test says what
/// it was played at. The wording is for a reader rather than for the parser
/// above, which never reads a result file back.
///
/// ```text
/// MoveTime(1000)                        movetime 1000ms
/// Clock { base_ms: 8000, inc_ms: 80 }   clock 8000+80ms
/// ```
///
/// Params:
/// - formatter: &mut FmtFormatter<'_> -> the sink being written into
///
/// Return:
/// FmtResult                          -> whatever the sink reported
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
/// Everything known about a subprocess that failed, gathered at the moment
/// it did. A child can go wrong in ways an exit status alone cannot explain,
/// so the error carries the engine, what was being asked of it, what went
/// wrong, whether it is even still alive, and whatever it said on the way.
///
/// ```text
/// binary   which of the two engines it was
/// action   what was being asked: spawn, write, read, protocol wait
/// detail   the failure itself, in the words of whatever reported it
/// status   running, or the exit status if it is not
/// stderr   the child's own diagnostics, drained once it has exited
/// ```
///
/// The stderr is drained only from a child that has actually exited: reading
/// a live one's pipe to end of file would block until it does.
struct SPRTChildError {
    binary: String,                                                             /* which engine, for the report       */
    action: String,                                                             /* what was being asked of it         */
    detail: String,                                                             /* what went wrong doing it           */
    status: String,                                                             /* running, or how it exited          */
    stderr: String,                                                             /* what it said, if it can be read    */
}

/// SPRTChildError::fmt
///
/// Lays the five fields out as the one message the run logs, and saves as
/// its verdict where the failure ended the test.
///
/// ```text
/// engine ./main failed during bestmove wait: timed out before bestmove
/// (status: running)
/// stderr:
/// <engine still running; stderr not drained>
/// ```
///
/// Params:
/// - formatter: &mut FmtFormatter<'_> -> the sink being written into
///
/// Return:
/// FmtResult                          -> whatever that sink reported
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
/// One running engine, and the three pipes that reach it. Neither side of a
/// test is linked in as a library: both are real binaries driven over UCI,
/// so what is measured is the engine as it will actually be shipped.
///
/// ```text
/// input    harness → engine   commands, written and flushed at once
/// output   harness ← engine   replies, handed over by a reader thread
/// errors   harness ← engine   diagnostics, read only once it has exited
/// ```
///
/// The reply pipe is never read straight. A `BufReader` offers no timed
/// read, so a hung engine would hang the harness with it; instead a thread
/// reads lines and passes them down a channel the driver can wait on with a
/// deadline, and a child that stops answering costs one game rather than
/// the whole run.
struct SPRTChild {
    binary: String,                                                             /* executable path for diagnostics    */
    process: Child,                                                             /* the running engine subprocess      */
    input: ChildStdin,                                                          /* pipe carrying commands to it       */
    output: Receiver<Result<Option<String>, String>>,                           /* timed subprocess reply stream      */
    errors: ChildStderr,                                                        /* pipe of its stderr diagnostics     */
}

/// SPRTChild protocol driver
///
/// The whole of one engine's side of the conversation, from starting it up
/// to asking it for a move. The methods fall into three groups: the ones
/// that build an error, the ones that start a child, and the ones that talk
/// to a child already running.
///
/// ```text
/// setup_error     an error about a child that never started at all
/// failure         an error about a running one, its state attached
/// exited_error    an error only if the child has quietly died
///
/// output_reader   the thread that turns the reply pipe into a channel
/// spawn           start a binary, set the variant, wait for readyok
///
/// send            write one line and flush it
/// drain_errors    everything the child has written to stderr
/// read_line       take the next reply, or give up at a deadline
/// wait_for        read past everything until a line opens with a token
/// new_game        reset between games, then wait until that has landed
/// bestmove        set the position, send `go`, return the move
/// ```
///
/// `spawn` runs each child in its own `engine_sandbox` on a single thread.
/// One thread apiece stops the two from competing for cores, which would
/// otherwise measure how loaded the machine was rather than the patch.
///
/// Every failure below is reported rather than raised: a child that breaks
/// mid-game loses that game and is restarted, and only a failure of both at
/// once ends the run, there being no honest way to score a game that way.
///
/// setup_error
///
///   Params:
///   - binary: &str   -> path of the engine that never started
///   - action: &str   -> what was being attempted when it did not
///   - detail: String -> the failure, in the words of whoever reported it
///
///   Return:
///   SPRTChildError   -> that failure, with no process state to attach
///
/// output_reader
///
///   Params:
///   - output: ChildStdout                     -> the child's reply pipe
///
///   Return:
///   Receiver<Result<Option<String>, String>>  -> lines as they arrive,
///                                                `None` at end of pipe
///
/// spawn
///
///   Params:
///   - binary : &str                   -> path to the engine executable
///   - variant: &str                   -> variant name to select over UCI
///
///   Return:
///   Result<SPRTChild, SPRTChildError> -> a child that answered `readyok`,
///                                        or what stopped it from doing so
///
/// failure
///
///   Params:
///   - action: &str   -> what was being asked when it went wrong
///   - detail: String -> the failure, in the words of whoever reported it
///
///   Return:
///   SPRTChildError   -> that failure, with the child's state attached
///
/// exited_error
///
///   Params:
///   - action: &str          -> what the check is being made on behalf of
///
///   Return:
///   Option<SPRTChildError>  -> an error if the child has already exited,
///                              nothing at all while it is still running
///
/// send
///
///   Params:
///   - command: &str            -> the line to write and flush
///
///   Return:
///   Result<(), SPRTChildError> -> success, or a write that did not land
///
/// drain_errors
///
///   Return:
///   String -> the child's stderr, or a note that it wrote none
///
/// read_line
///
///   Params:
///   - timeout: Duration                    -> how long one line may take
///
///   Return:
///   Result<Option<String>, SPRTChildError> -> a reply, `None` at end of
///                                             pipe, or the read failure
///
/// wait_for
///
///   Params:
///   - token  : &str            -> the opening word that ends the wait
///   - timeout: Duration        -> the deadline for the whole wait, not
///                                 for each line inside it
///
///   Return:
///   Result<(), SPRTChildError> -> the token arrived, or it never did
///
/// new_game
///
///   Return:
///   Result<(), SPRTChildError> -> the reset landed, or it never did
///
/// bestmove
///
///   Params:
///   - startpos  : &str                     -> the variant's start position
///   - moves     : &[String]                -> the game so far, in notation
///   - go_command: &str                     -> the `go` line, clocks and all
///   - timeout   : Duration                 -> the deadline for a reply
///
///   Return:
///   Result<Option<String>, SPRTChildError> -> the move, `None` where the
///                                             engine named none, or the
///                                             failure that came instead
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
/// Shuts a child down and makes sure it is gone. `quit` is asked for first
/// and the process given `SPRT_SHUTDOWN_TIMEOUT_MS` to take it; one that is
/// wedged, or that has stopped reading its input at all, is killed instead.
/// The wait happens here rather than being left to the operating system
/// because a run plays hundreds of games, and children that were only asked
/// to leave would pile up behind it.
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
/// How one game ended. Every score is from White's side of the board, and
/// since `WHITE` is 0 and `BLACK` is 1, a side that loses on its own account
/// scores exactly its own colour.
///
/// ```text
/// Score(1.0)     White won, by rule or because Black could not answer
/// Score(0.5)     drawn, by whichever of the variant's rules said so
/// Score(0.0)     Black won
///
/// EngineLoss     one child broke, which is a loss for it and a restart
///                before the next game, the error carried along to log
///
/// Aborted        both children broke at once, which is not a game and is
///                not scored as one; the run stops here instead
/// ```
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
/// One refereed match slot: two children, and the neutral board that decides
/// between them. That board is the engine's own `State`, forked from the
/// loaded variant, and its move history is the game itself. The move list
/// handed to each child every ply is rebuilt from that history rather than
/// kept beside it, so there is no second copy that could fall out of step
/// with the position being refereed.
///
/// `white` and `black` say which child is playing which colour now, not
/// which it started as. The two games of a pair share one random opening and
/// swap the children between them, so a lucky opening helps both equally:
///
/// ```text
/// game 1   A as White, B as Black   scored from White's side
/// game 2   B as White, A as Black   scored, then flipped back to A's
/// ```
///
/// The referee's `State` is also what is streamed to the board view, so a
/// running match can be watched rather than only counted.
struct GameManager {
    state: State,                                                               /* neutral referee; history is game   */
    white: SPRTChild,                                                           /* child currently playing White      */
    black: SPRTChild,                                                           /* child currently playing Black      */
}

/// GameManager driver
///
/// Setting up a match slot, and playing one game in it.
///
/// ```text
/// new           spawn both children and fork the referee's board
/// swap_colors   exchange which child is playing which colour
/// restart       replace one child that broke, on the same binary
/// reset_to      fork the board again and replay the pair's opening
/// play          play one game out and say how it ended
/// ```
///
/// `play` is the referee, and every way a game can end passes through it:
///
/// ```text
/// the rules       game_outcome, or adjudicate_no_move where a side has
///                 no legal move at all, both by the variant's own rules
/// no move named   a child that answers `bestmove (none)`, or nothing,
///                 loses; so does one whose move will not parse or is
///                 not legal in the position being refereed
/// the flag        under a clock, the wall time the harness measured is
///                 charged to the mover, so it pays for its own I/O too,
///                 and overstepping it loses
/// a broken child  a loss for that child, unless the other has died as
///                 well, in which case the game is not scored at all
/// an interrupt    scored where it stands: against the side to move if
///                 that side is in check or the variant loses on having
///                 no move, and drawn otherwise
/// ```
///
/// new
///
///   Params:
///   - template: &State                  -> the variant to fork as referee
///   - binary_a: &str                    -> the child starting as White
///   - binary_b: &str                    -> the child starting as Black
///   - variant : &str                    -> the variant both are set to
///
///   Return:
///   Result<GameManager, SPRTChildError> -> the slot, or why it could not
///                                          be set up
///
/// swap_colors
///
///   Takes no parameters and returns nothing: it exchanges the two
///   children, and which colour each is playing follows from that.
///
/// restart
///
///   Params:
///   - side   : u8              -> the colour whose child is replaced
///   - variant: &str            -> the variant the new one is set to
///
///   Return:
///   Result<(), SPRTChildError> -> the replacement is ready, or why not
///
/// reset_to
///
///   Params:
///   - template: &State  -> the variant to fork the board from again
///   - opening : &[Move] -> the pair's shared opening, replayed onto it
///
///   Return:
///   Result<(), String>  -> both children reset, or which of them failed
///
/// play
///
///   Params:
///   - dict        : Option<&Translator> -> the notation both ends read
///   - startpos    : &str                -> the variant's start position
///   - time_control: SPRTTimeControl     -> the budget or bank per move
///
///   Return:
///   SPRTGameOutcome                     -> how the game ended, in the
///                                          terms above
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
/// The test itself: three pure functions with nothing kept between them.
/// Two convert between an Elo gap and the score that gap is worth, and the
/// third weighs the evidence gathered so far.
///
/// ```text
/// expected_score         1 / (1 + 10 ^ (-elo / 400))
/// elo_from_score         -400 · log10(1 / score - 1)
/// log_likelihood_ratio   everything seen so far, as one number
/// ```
///
/// The two hypotheses are given in Elo but the ratio is worked in scores,
/// so `expected_score` converts them once before the first game is played.
///
/// The ratio is the normalised form, taken over pairs of games rather than
/// single games:
///
/// ```text
///          pairs · (mu_1 - mu_0) · (2 · mean - mu_0 - mu_1)
///   LLR =  ───────────────────────────────────────────────
///                          2 · variance
/// ```
///
/// Working in pairs is what makes the test cheap. Both games of a pair are
/// played from one opening with the colours swapped, so how good that
/// opening was cancels out of the pair's score instead of counting as
/// evidence about the engines. The variance falls with it, and a smaller
/// variance is a larger ratio for the same number of games played.
///
/// Zero comes back while the sample has no variance at all, every pair so
/// far having scored alike: the denominator would be zero, and the honest
/// reading of an unvarying sample is that it says nothing either way yet.
///
/// expected_score
///
///   Params:
///   - elo: f64 -> the Elo advantage to convert
///
///   Return:
///   f64        -> what that advantage is worth per game, from 0 to 1
///
/// elo_from_score
///
///   Params:
///   - score: f64 -> an observed score per game
///
///   Return:
///   f64          -> the Elo gap it implies, clamped just short of the
///                   ends, a perfect score having no finite answer
///
/// log_likelihood_ratio
///
///   Params:
///   - pairs   : f64 -> how many pairs have been played
///   - mean    : f64 -> the mean of their scores
///   - variance: f64 -> the population variance of those scores
///   - mu_zero : f64 -> the score expected if the patch changed nothing
///   - mu_one  : f64 -> the score expected if it gained what was claimed
///
///   Return:
///   f64             -> the ratio, or zero while there is no variance
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
/// Turns one game's score into something the running tally can add. The
/// ratio above works in scores, but the line the run logs and the file it
/// saves are counted in games, so the two are kept side by side.
///
/// ```text
/// score > 0.75    (1, 0, 0)   a win
/// 0.25 .. 0.75    (0, 1, 0)   a draw
/// score < 0.25    (0, 0, 1)   a loss
/// ```
///
/// The bands are wide where the values are only ever 0, 0.5, and 1 because
/// those values have been through a divide and a subtraction on the way
/// here, and an exact comparison would eventually miss one.
///
/// Params:
/// - score: f64    -> one game's score, from the counted engine's side
///
/// Return:
/// (u32, u32, u32) -> what to add to the win, draw, and loss counts
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
/// Saves the run so it outlives the terminal it was run in. Any previous
/// result is rolled to a timestamped name first and the folder trimmed to
/// `SPRT_HISTORY_KEEP`, so a variant keeps its recent history without the
/// folder growing forever:
///
/// ```text
/// res/sprt/standard/latest.sprt
/// res/sprt/standard/2026-09-06_14-02-11.sprt
/// res/sprt/standard/2026-09-05_09-31-40.sprt
/// ```
///
/// What lands in it is everything needed to read the verdict back later
/// without the command line that produced it:
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
/// A file is written even where no game was ever played: a run that died in
/// setup is as much a result as one that reached a bound, and saying so is
/// better than leaving the last run's file standing as the current answer.
///
/// Params:
/// - variant     : &str            -> the variant, which picks the folder
/// - binary_a    : &str            -> path of the first engine
/// - binary_b    : &str            -> path of the second engine
/// - time_control: SPRTTimeControl -> what the games were played at
/// - h0          : f64             -> the Elo gap the test can rule out
/// - h1          : f64             -> the Elo gap it can confirm
/// - wins        : u32             -> wins, from engine A's view
/// - draws       : u32             -> draws, likewise
/// - losses      : u32             -> losses, likewise
/// - llr         : f64             -> where the ratio finished
/// - verdict     : &str            -> why it stopped where it did
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
/// Copies each child's own log out of its sandbox before the next run wipes
/// it. A child writes plain `logs/latest.log` and knows nothing about the
/// harness that started it, so which engine it was is attached here, on the
/// way out, rather than being taught to the logger:
///
/// ```text
/// /tmp/anekamacam-sprt/_home_me_old/logs/latest.log
///     → res/sprt/standard/engine-a_latest.log
///
/// /tmp/anekamacam-sprt/_home_me_new/logs/latest.log
///     → res/sprt/standard/engine-b_latest.log
/// ```
///
/// The rolling is the result file's, one history per label. A sandbox with
/// no log in it is skipped: that engine either never started, or both sides
/// named the same binary and so shared the one sandbox between them.
///
/// This runs only once both children have stopped, since a log still being
/// written to would be copied half-flushed.
///
/// Params:
/// - variant : &str -> the variant, which picks the folder
/// - binary_a: &str -> the first engine, filed as `engine-a`
/// - binary_b: &str -> the second, filed as `engine-b`
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
/// Plays the whole test, from clearing the sandboxes to saving the verdict.
///
/// ```text
/// 1  clear    both sandboxes, so neither engine inherits old parameters
/// 2  spawn    both children, on the loaded variant, one thread apiece
/// 3  open     one random opening a pair, played on a throwaway board
/// 4  play     the pair, the children swapping colours between its games
/// 5  fold     that pair's score into the running mean and variance
/// 6  decide   stop at either bound, or open the next pair
/// ```
///
/// The referee formats moves with the same translator the children were
/// given, so what it sends them is by construction what they read back. A
/// variant with no dictionary of its own is refused rather than played in
/// the engine's internal notation, there being nothing to say the children
/// would read that notation the same way.
///
/// No path out of here goes unreported. A failure during setup, both
/// children dying at once, an interrupt, and the budget running out each
/// name themselves in the verdict and are written out exactly as a decided
/// test is, so a result file is never the previous run's answer.
///
/// Params:
/// - template    : &State          -> the loaded variant, forked as referee
/// - variant     : &str            -> its name, for setup and for output
/// - binary_a    : &str            -> path to the first engine binary
/// - binary_b    : &str            -> path to the second engine binary
/// - time_control: SPRTTimeControl -> what each game is played at
/// - max_games   : usize           -> how many games before giving up
/// - h0          : f64             -> the Elo gap the test can rule out
/// - h1          : f64             -> the Elo gap it can confirm
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

        let mut opening_state = template.fork();                                /* one throwaway referee per pair, so */
        opening_state.play_random_opening(OPENING_RANDOM_PLIES);                /* both games of the pair open the    */
                                                                                /* same way from opposite colours     */
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
