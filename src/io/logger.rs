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

/// TUI log-forwarding macros.
///
/// `push_log_message!` mirrors a formatted log line into the shared
/// message queue rendered by the debug TUI (only while the TUI's debug
/// flag is set), and `verbosity_enabled!` tests whether a numeric level
/// is currently visible so callers can skip expensive formatting.
///
/// push_log_message!
///
///   Params:
///   - level  : u8     -> numeric verbosity level stamped on the line
///   - message: String -> already-formatted log line to mirror
///
/// verbosity_enabled!
///
///   Params:
///   - level  : u8 -> numeric level to test
///
///   Return:
///   bool          -> whether lines at that level are currently visible
#[macro_export]
macro_rules! push_log_message {
    ($level:expr, $message:expr) => {
        if DEBUG_FLAG.load(Ordering::Relaxed) {
            let formatted = format!("[{}] {}", $level, $message);

            let mut queue = LOG_MESSAGES.lock().unwrap_or_else(|e| {
                e.into_inner()
            });

            queue.push_back(formatted.clone());
        }
    };
}

#[macro_export]
macro_rules! verbosity_enabled {
    ($level:expr) => {
        configured_verbosity_level() >= $level
    };
}

/// Numeric logging macros, `log_1!` through `log_5!`.
///
/// Each takes `format!`-style arguments, mirrors the line into the TUI
/// queue at its numeric level, and forwards to the matching `log` crate
/// macro: 1 = error (critical results), 2 = warn (user-facing output),
/// 3 = info (engine telemetry), 4 = debug (parsing/search internals),
/// 5 = trace (deepest call traces). See `init_logging` for the full
/// level semantics.
///
/// log_1! .. log_5!
///
///   Params:
///
///   - args: format! arguments
///     format string plus its interpolated values; no return value
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

/// Verbosity plumbing helpers.
///
/// `level_to_verbosity` maps `log` crate levels onto the numeric 1-5
/// scale used in log lines; `configured_log_level` translates the
/// runtime verbosity back into a `log` filter; and the remaining three
/// read or step the shared `RUNTIME_VERBOSITY` atomic, clamped to 1-5
/// (used by the TUI's live verbosity keys).
///
/// level_to_verbosity
///
///   Params:
///   - level: log::Level -> `log` crate level to translate
///
///   Return:
///   u8                  -> numeric verbosity 1-5
///
/// configured_log_level
///
///   Return:
///   log::LevelFilter -> filter matching the runtime verbosity
///
/// configured_verbosity_level
///
///   Return:
///   u8 -> current runtime verbosity 1-5
///
/// inc_verbosity / dec_verbosity
///   step the runtime verbosity up or down one level, clamped to 1-5;
///   no parameters, no return value
fn level_to_verbosity(level: log::Level) -> u8 {
    match level {
        log::Level::Error => 1,
        log::Level::Warn => 2,
        log::Level::Info => 3,
        log::Level::Debug => 4,
        log::Level::Trace => 5,
    }
}

pub fn configured_log_level() -> log::LevelFilter {
    match RUNTIME_VERBOSITY.load(Ordering::Relaxed) {
        1 => log::LevelFilter::Error,
        2 => log::LevelFilter::Warn,
        3 => log::LevelFilter::Info,
        4 => log::LevelFilter::Debug,
        5 => log::LevelFilter::Trace,
        _ => log::LevelFilter::Debug,
    }
}

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

/// init_logging
///
/// Initializes file logging: rolls any previous `logs/latest.log` to a
/// timestamped backup via `roll_latest`, prunes old backups to
/// `LOG_HISTORY_KEEP`, opens a fresh `logs/latest.log`, and installs a
/// formatter that stamps each line with its numeric verbosity level,
/// timestamp, and source location. Called once at startup from `main`.
///
/// Notes:
/// The engine uses 5 numeric verbosity levels, stamped on every line:
/// - log_1: critical — benchmark/suite results, game-over states,
///   state-change failures that abort an operation.
/// - log_2: user-facing results — command results, per-case perft output,
///   interrupts (SIGINT), invalid-command feedback, TUI state messages.
/// - log_3: engine telemetry — TT/QT stats, thread lifecycle, perft/suite
///   summaries, parameter-derivation progress, per-depth search output.
/// - log_4: debug — parsing internals, token captures, filter results,
///   search diagnostics, perft case pass/fail, piece-value derivation.
/// - log_5: trace — deepest call traces, atomic/coordinate evaluation
///   entry points, perft depth-0 node output.
pub fn init_logging() {

    if !Path::new(LOG_DIR).exists() {
        fs::create_dir_all(LOG_DIR).expect("Failed to create log directory");
    }

    let log_path = format!("{}/latest.log", LOG_DIR);
    roll_latest(LOG_DIR, "", "log");
    prune_backups(LOG_DIR, "", "log", LOG_HISTORY_KEEP);

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

            let level = level_to_verbosity(record.level());

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
