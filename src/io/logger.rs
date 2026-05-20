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
macro_rules! push_log_message {
    ($level:expr, $message:expr) => {
        let formatted = format!("[{}] {}", $level, $message);

        let mut queue = LOG_MESSAGES.lock().unwrap_or_else(|e| {
            panic!("Failed to lock LOG_MESSAGES: {e}")
        });

        queue.push_back(formatted.clone());
    };
}

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

fn level_to_verbosity(level: log::Level) -> u8 {
    match level {
        log::Level::Error => 1,
        log::Level::Warn => 2,
        log::Level::Info => 3,
        log::Level::Debug => 4,
        log::Level::Trace => 5,
        _ => panic!("Unsupported log level: {level}"),
    }
}

fn verbosity_style(level: log::Level) -> log_style::Style {
    match level_to_verbosity(level) {
        1 => log_style::AnsiColor::Red.on_default(),
        2 => log_style::AnsiColor::Yellow.on_default(),
        3 => log_style::AnsiColor::Green.on_default(),
        4 => log_style::AnsiColor::Blue.on_default(),
        5 => log_style::AnsiColor::Magenta.on_default(),
        _ => panic!("Unsupported log level: {level}"),
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

pub fn init_logging() {
    let latest = Path::new(&*LATEST_LOG_PATH);

    if latest.exists()
    && let Ok(meta) = fs::metadata(latest)
    && let Ok(modified) = meta.created() {
        let datetime: chrono::DateTime<chrono::Local> =
            modified.into();
        let archive = format!(
            "logs/engine_{}.log",
            datetime.format("%Y-%m-%d_%H-%M-%S")
        );
        let _ = fs::rename(&*LATEST_LOG_PATH, &archive);
    }

    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&*LATEST_LOG_PATH)
        .expect("Failed to open log file");

    let target = Box::new(file);

    LoggerBuilder::new()
        .target(LoggerTarget::Pipe(target))
        .filter_level(configured_log_level())
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
            let level_style = verbosity_style(record.level());

            writeln!(
                buf,
                "[{}{}{}]-[{} {}:{}] {}",
                level_style.render(),
                level,
                level_style.render_reset(),
                timestamp,
                file,
                line,
                record.args()
            )
        })
        .init();
}
