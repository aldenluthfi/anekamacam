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

use env_logger::fmt::style;

lazy_static! {
    static ref LOG_MESSAGES: Mutex<VecDeque<String>> =
        Mutex::new(VecDeque::new());
}

pub fn push_log_message(level: u8, message: String) {
    let mut queue = LOG_MESSAGES.lock().unwrap_or_else(|e| {
        panic!("Failed to lock LOG_MESSAGES: {e}")
    });

    queue.push_back(format!("[{}] {}", level, message));

    while queue.len() > 1024 {
        queue.pop_front();
    }
}

pub fn take_log_messages() -> Vec<String> {
    let mut queue = LOG_MESSAGES.lock().unwrap_or_else(|e| {
        panic!("Failed to lock LOG_MESSAGES: {e}")
    });

    let mut drained = Vec::with_capacity(queue.len());

    while let Some(message) = queue.pop_front() {
        drained.push(message);
    }

    drained
}

#[macro_export]
macro_rules! log_1 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message(1, message);
        }
    };
}

#[macro_export]
macro_rules! log_2 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message(2, message);
        }
    };
}

#[macro_export]
macro_rules! log_3 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message(3, message);
        }
    };
}

#[macro_export]
macro_rules! log_4 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message(4, message);
        }
    };
}

#[macro_export]
macro_rules! log_5 {
    ($($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            push_log_message(5, message);
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

fn verbosity_style(level: log::Level) -> style::Style {
    match level_to_verbosity(level) {
        1 => style::AnsiColor::Green.on_default(),
        2 => style::AnsiColor::Cyan.on_default(),
        3 => style::AnsiColor::Yellow.on_default(),
        4 => style::AnsiColor::Magenta.on_default(),
        5 => style::AnsiColor::Red.on_default().effects(style::Effects::BOLD),
        _ => style::Style::new(),
    }
}

pub fn configured_log_level() -> log::LevelFilter {
    if cfg!(feature = "log-level-5") {
        log::LevelFilter::Trace
    } else if cfg!(feature = "log-level-4") {
        log::LevelFilter::Debug
    } else if cfg!(feature = "log-level-3") {
        log::LevelFilter::Info
    } else if cfg!(feature = "log-level-2") {
        log::LevelFilter::Warn
    } else if cfg!(feature = "log-level-1") {
        log::LevelFilter::Error
    } else {
        log::LevelFilter::Info
    }
}

pub fn configured_verbosity_level() -> u8 {
    configured_log_level().to_level().map_or(3, level_to_verbosity)
}

pub fn verbosity_enabled(required: u8) -> bool {
    configured_verbosity_level() >= required
}

pub fn init_logging() {
    env_logger::Builder::new()
        .filter_level(configured_log_level())
        .format_target(false)
        .format_module_path(false)
        .format_source_path(false)
        .format(|buf, record| {
            let timestamp_raw = buf.timestamp_millis().to_string();
            let timestamp = timestamp_raw
                .trim_end_matches('Z')
                .split('.')
                .next()
                .unwrap_or(&timestamp_raw)
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
