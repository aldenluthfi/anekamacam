//! logger.rs
//!
//! Starts the logger and defines the numbered log macros.
//!
//! Search, parsers and protocols all write diagnostics. This file sets up
//! the log backend once. The callers give only the level of a line. This
//! file decides how and where the line goes.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             LOG MESSAGE MIRRORING
\*----------------------------------------------------------------------------*/

/// push_log_message!
///
/// Copies a formatted log line into the queue that the debug console
/// shows. The log backend still writes the line to the log file.
///
/// ```text
/// [3] search depth 7 complete
///  ^  ^
///  |  the message, as in the log file
///  the level, for the console filter
/// ```
///
/// Params:
/// - level  : u8     -> verbosity level of the line
/// - message: String -> formatted log line to copy
///
/// Notes:
/// The macro does nothing when the debug flag is clear. It recovers a
/// poisoned queue, so a panic does not hide the log. The queue keeps the
/// last `MAX_LOG_HISTORY` lines. The log file keeps all lines.
///
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

/// Numeric logging macros
///
/// The numbered log macros. All diagnostics of the engine use them. Each
/// macro copies the line to the console queue and calls the related
/// `log` crate macro.
///
/// - `log_1!` : error, critical results, game over, stopped operations
/// - `log_2!` : warn, command results, interrupts, invalid input
/// - `log_3!` : info, telemetry, thread life cycle, output for each depth
/// - `log_4!` : debug, parser internals, search diagnostics, derivation
/// - `log_5!` : trace, deep call traces, output for each node
///
/// Params:
/// - args: format! arguments -> format string and values
///
/// Notes:
/// The macros format the arguments two times. An argument with a side
/// effect thus runs two times. `init_logging` gives the full level list.
///
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

/// Verbosity helpers
///
/// Read or change the shared `RUNTIME_VERBOSITY` atomic, on the 1 to 5
/// scale of the log levels. The console keys use them, also during a
/// search.
///
/// ```text
/// 1 quietest                                              5 loudest
/// │────────────────────────────────────────────────────────────────│
///   inc_verbosity steps right, dec_verbosity steps left
/// ```
///
/// configured_verbosity_level
///
///   Return:
///   u8 -> current runtime verbosity, 1 to 5
///
/// inc_verbosity
///
///   Increases the verbosity by one level, maximum 5.
///
/// dec_verbosity
///
///   Decreases the verbosity by one level, minimum 1.
///
/// Notes:
/// The steps clamp after the arithmetic, not with a test before it. Thus
/// another thread cannot change the value between a test and a step.
///
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
/// Sets up the file log. `main` calls it once at startup. Each run has its
/// own log file, and the log of the last run stays for comparison.
///
/// - roll   : rename `logs/latest.log` with a timestamp, if it exists
/// - prune  : keep only the 32 most recent timestamped logs
/// - open   : make a new empty `logs/latest.log`
/// - format : write the level, time and source location on each line
///
/// ```text
/// [3]-[2026-09-06 14:02:11.418Z search.rs:214] depth 7 complete
/// ```
///
/// Notes:
/// The backend filter is at trace, so the file has all lines. Each line
/// has its level, so a reader can filter the file later. The levels are:
///
/// - log_1 : critical, bench and suite results, game over, stopped work
/// - log_2 : user output, command results, perft cases, SIGINT, bad input
/// - log_3 : telemetry, table stats, threads, summaries, each search depth
/// - log_4 : debug, parser internals, filters, search diagnostics, values
/// - log_5 : trace, deep call traces, evaluation entries, perft leaf nodes
///
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
