//! # logger.rs
//!
//! Logging initialization, numeric verbosity wrappers, and formatting.
//!
//! This file centralizes logger configuration so startup code in `main.rs`
//! stays focused on engine bootstrapping.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 19/04/2026

use crate::*;

#[macro_export]
macro_rules! verbosity_enabled {
    ($level:expr) => {
        configured_verbosity_level() >= $level
    };
}

#[macro_export]
macro_rules! log_1 {
    ($($arg:tt)*) => {
        {
            error!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_2 {
    ($($arg:tt)*) => {
        {
            warn!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_3 {
    ($($arg:tt)*) => {
        {
            info!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_4 {
    ($($arg:tt)*) => {
        {
            debug!("{}", format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_5 {
    ($($arg:tt)*) => {
        {
            trace!("{}", format!($($arg)*));
        }
    };
}

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

/// # Verbosity Levels
///
/// This module uses 5 numeric verbosity levels. The semantics are:
///
/// - `log_1`: Critical. Benchmark/suite final results, game-over states,
///   state-change failures that abort an operation.
/// - `log_2`: User-facing results and confirmations. Command results,
///   per-case perft output, user-initiated interrupts (e.g. SIGINT),
///   invalid command feedback, TUI state messages.
/// - `log_3`: Engine telemetry. TT/QT stats, thread lifecycle, perft/suite
///   summary stats, parameter derivation progress, per-depth search output,
///   SIGINT diagnostics, perft file I/O.
/// - `log_4`: Debug. Parsing internals, token captures, filter results,
///   search diagnostics, mid-level diagnostic output, perft test case
///   pass/fail results, piece value derivation details.
/// - `log_5`: Trace. Deepest call-stack traces — atomic/coordinate evaluation
///   entry points and final-result logging, perft depth-0 node output.
pub fn init_logging() {

    if !Path::new(LOG_DIR).exists() {
        fs::create_dir_all(LOG_DIR).expect("Failed to create log directory");
    }

    let log_path = format!("{}/latest.log", LOG_DIR);
    let latest = Path::new(&log_path);

    if latest.exists()
    && let Ok(meta) = fs::metadata(latest)
    && let Ok(modified) = meta.created() {
        let datetime: chrono::DateTime<chrono::Local> =
            modified.into();
        let archive = format!(
            "logs/engine_{}.log",
            datetime.format("%Y-%m-%d_%H-%M-%S")
        );
        let _ = fs::rename(latest, &archive);
    }

    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(latest)
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
