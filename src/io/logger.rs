//! # logger.rs
//!
//! Logging initialization and formatting.
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

pub fn configured_log_level() -> log::LevelFilter {
    if cfg!(feature = "log-level-debug") {
        log::LevelFilter::Debug
    } else if cfg!(feature = "log-level-info") {
        log::LevelFilter::Info
    } else if cfg!(feature = "log-level-warn") {
        log::LevelFilter::Warn
    } else if cfg!(feature = "log-level-error") {
        log::LevelFilter::Error
    } else {
        log::LevelFilter::Info
    }
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

            let file = record
                .file()
                .and_then(
                    |p| Path::new(p).file_name().and_then(|n| n.to_str())
                ).unwrap_or("?");
            let line = record.line().map_or("?".to_string(), |l| l.to_string());

            let level_style = buf.default_level_style(record.level());

            writeln!(
                buf,
                "[{}{}{}]-[{} {}:{}] {}",
                level_style.render(),
                record.level(),
                level_style.render_reset(),
                timestamp,
                file,
                line,
                record.args()
            )
        })
        .init();
}
