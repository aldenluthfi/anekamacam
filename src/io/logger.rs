//! logger.rs
//!
//! Logging initialization, numeric verbosity wrappers, and formatting.
//!
//! Search, parsing, and the protocol layers all emit diagnostics, at wildly
//! different urgencies and volumes. This file is the single place that decides
//! what actually reaches the log: it configures the backend once and exposes
//! numbered verbosity wrappers so callers say only how important a line is,
//! never how or whether it is printed.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             LOG MESSAGE MIRRORING
\*----------------------------------------------------------------------------*/

/// push_log_message!
///
/// Mirrors an already-formatted log line into the shared queue the debug TUI
/// renders. The log file is written by the logging backend regardless; this
/// is the second copy, and it exists only because a TUI cannot tail a file it
/// is busy drawing over.
///
/// ```text
/// [3] search depth 7 complete
///  ^  ^
///  |  the message, exactly as the log file receives it
///  the level, so the TUI can filter without re-reading the line
/// ```
///
/// Nothing is formatted, locked, or pushed while the debug flag is clear, so
/// a headless run pays only the flag read. A poisoned queue is recovered from
/// rather than propagated: losing the whole log display over one panicked
/// thread would hide the very output being used to find that panic.
///
/// The queue is capped at `MAX_LOG_HISTORY` and sheds its oldest line to stay
/// there. Only this mirror is trimmed; the log file keeps every line.
///
/// Params:
/// - level  : u8     -> numeric verbosity level stamped on the line
/// - message: String -> already-formatted log line to mirror
#[macro_export]
macro_rules! push_log_message {
    ($level:expr, $message:expr) => {
        if DEBUG_FLAG.load(Ordering::Relaxed) {
            let formatted = format!("[{}] {}", $level, $message);

            let mut queue = LOG_MESSAGES.lock().unwrap_or_else(|e| {
                e.into_inner()
            });

            queue.push_back(formatted.clone());

            while queue.len() > MAX_LOG_HISTORY {                               /* the file keeps the rest            */
                queue.pop_front();
            }
        }
    };
}

/*----------------------------------------------------------------------------*\
                                 LOGGING MACROS
\*----------------------------------------------------------------------------*/

/// Numeric logging macros, `log_1!` through `log_5!`.
///
/// Every diagnostic in the engine goes out through one of these five. Callers
/// name a level and nothing else, so how urgent a line is stays a property of
/// the line, never of where it happens to be written from.
///
/// - `log_1!` : error, critical results, game over, aborted operations
/// - `log_2!` : warn, command results, interrupts, invalid input
/// - `log_3!` : info, engine telemetry, thread lifecycle, per-depth
///              output
/// - `log_4!` : debug, parsing internals, search diagnostics, derivation
/// - `log_5!` : trace, deepest call traces, per-node output
///
/// Each mirrors the line into the TUI queue at its numeric level and forwards
/// to the matching `log` crate macro; `init_logging` documents what belongs at
/// each level in full. The arguments are formatted twice, once per
/// destination, so an expression with a side effect passed to one of these
/// runs twice.
///
/// Params:
/// - args: format! arguments -> format string plus interpolated values
#[macro_export]
macro_rules! log_1 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message!(1, message);
            error!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_2 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message!(2, message);
            warn!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_3 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message!(3, message);
            info!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_4 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message!(4, message);
            debug!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_5 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message!(5, message);
            trace!("{}", format!($($arg)*));
        }
    };
}

/*----------------------------------------------------------------------------*\
                               VERBOSITY CONTROL
\*----------------------------------------------------------------------------*/

/// Verbosity plumbing helpers.
///
/// All three read or step the shared `RUNTIME_VERBOSITY` atomic, on the same
/// 1-5 scale log lines are stamped with, and back the TUI's live verbosity
/// keys. The atomic is what makes them live: a running search raises or lowers
/// its own output without being stopped and restarted.
///
/// ```text
/// 1 quietest                                              5 loudest
/// │────────────────────────────────────────────────────────────────│
///   inc_verbosity steps right, dec_verbosity steps left
/// ```
///
/// Both steps clamp on the far side of the arithmetic rather than testing
/// first, since a test and a step are two operations and another thread can
/// land between them. `inc_verbosity` also clamps before adding, so a level
/// that somehow arrived above the top comes back down instead of climbing.
///
/// configured_verbosity_level
///
///   Return:
///   u8 -> current runtime verbosity 1-5
///
/// inc_verbosity / dec_verbosity
///
///   step the runtime verbosity one level toward 5 or toward 1, taking no
///   parameters and returning nothing
pub fn configured_verbosity_level() -> u8 {
    RUNTIME_VERBOSITY.load(Ordering::Acquire)
}

pub fn inc_verbosity() {
    RUNTIME_VERBOSITY.fetch_min(5, Ordering::Release);
    RUNTIME_VERBOSITY.fetch_add(1, Ordering::Release);
    RUNTIME_VERBOSITY.fetch_min(5, Ordering::Release);
}

pub fn dec_verbosity() {
    RUNTIME_VERBOSITY.fetch_sub(1, Ordering::Release);
    RUNTIME_VERBOSITY.fetch_max(1, Ordering::Release);
}

/*----------------------------------------------------------------------------*\
                             LOGGER INITIALIZATION
\*----------------------------------------------------------------------------*/

/// init_logging
///
/// Sets up file logging, once, at startup from `main`. Every run gets its own
/// log at a fixed path, and the run before it is kept rather than overwritten,
/// which is what makes it possible to compare a failing run against the last
/// one that worked.
///
/// - roll   : `logs/latest.log` aside under a timestamp, if one was
///            there
/// - prune  : the timestamped backups down to the 32 most recent
/// - open   : a fresh `logs/latest.log`, truncated
/// - format : each line with its level, time, and source location
///
/// ```text
/// [3]-[2026-09-06 14:02:11.418Z search.rs:214] depth 7 complete
/// ```
///
/// The backend filter is left wide open at trace and the numeric level is
/// written into the line instead, so a log holds everything the run produced
/// and any level can be read back out of a finished file after the fact.
///
/// Notes:
/// The engine uses 5 numeric verbosity levels, stamped on every line:
///
/// - log_1 : critical, benchmark and suite results, game-over states,
///           state-change failures that abort an operation
/// - log_2 : user-facing, command results, per-case perft output,
///           SIGINT, invalid-command feedback, TUI state messages
/// - log_3 : telemetry, table stats, thread lifecycle, perft and suite
///           summaries, derivation progress, per-depth output
/// - log_4 : debug, parsing internals, token captures, filter results,
///           search diagnostics, case pass/fail, piece values
/// - log_5 : trace, deepest call traces, atomic and coordinate
///           evaluation entry points, perft depth-0 nodes
pub fn init_logging() {

    if !Path::new(LOG_DIR).exists() {
        fs::create_dir_all(LOG_DIR).expect("Failed to create log directory");
    }

    let log_path = format!("{}/latest.log", LOG_DIR);
    roll_latest(LOG_DIR, "", "log");
    prune_backups(LOG_DIR, "", "log", 32);

    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&log_path)
        .expect("Failed to open log file");

    let target = Box::new(file);

    LoggerBuilder::new()
        .target(LoggerTarget::Pipe(target))
        .filter_level(log::LevelFilter::Trace)
        .format_target(false)
        .format_module_path(false)
        .format_source_path(false)
        .format(|buf, record| {
            let timestamp_raw = buf.timestamp_millis().to_string();
            let timestamp = timestamp_raw
                .trim_end_matches(".")
                .replace('T', " ");

            let file = record.file().and_then(|path| {
                Path::new(path).file_name().and_then(|name| name.to_str())
            }).unwrap_or("?");
            let line = record
                .line()
                .map_or("?".to_string(), |line_num| line_num.to_string());

            let level = match record.level() {
                log::Level::Error => 1,
                log::Level::Warn => 2,
                log::Level::Info => 3,
                log::Level::Debug => 4,
                log::Level::Trace => 5,
            };

            writeln!(
                buf,
                "[{}]-[{} {}:{}] {}",
                level,
                timestamp,
                file,
                line,
                record.args()
            )
        })
        .init();
}
