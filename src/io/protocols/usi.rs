//! usi.rs
//!
//! The Universal Shogi Interface (USI) dialect.
//!
//! USI is the shared session engine with two differences the common
//! dispatcher already absorbs -- the `usi`/`usiok` handshake and the `sfen`
//! position keyword both fall out of the protocol name and the `fen`/`sfen`
//! acceptance in `execute_common`. The one clause it must translate is the
//! `byoyomi` time control, mapped to a fixed per-move budget so no shogi
//! dialect token reaches the engine core.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi
use crate::*;

/// Usi
///
/// The USI dialect marker. Stateless: the session lives in the shared
/// `Session`, and USI intercepts only its `go` line.
pub struct Usi;

impl Protocol for Usi {
    /// Usi::name
    ///
    /// Return:
    /// &str -> the protocol name, "usi"
    fn name(&self) -> &str {
        "usi"
    }

    /// Usi::execute
    ///
    /// Handles the lines `execute_common` defers: the `usi` handshake,
    /// `usinewgame`, and `go`. The `go` renames USI's `byoyomi` clause to
    /// the engine's standard `movetime`, so the per-move Japanese overtime
    /// becomes a fixed budget the shared parser understands.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the whitespace-split input line
    ///
    /// Return:
    /// bool                    -> always false; USI never quits from here
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool {
        match tokens.first().copied().unwrap_or("") {
            "usi" => print_handshake(session),
            "usinewgame" => new_game(session),
            "go" => {
                let normalized: Vec<&str> = tokens
                    .iter()
                    .map(|&t| if t == "byoyomi" { "movetime" } else { t })
                    .collect();
                start_search(session, &normalized);
            }
            _ => {}
        }
        false
    }
}

/// usi
///
/// Entry point for the USI protocol loop.
///
/// Return:
/// IoResult<()> -> Ok on clean shutdown
pub fn usi() -> IoResult<()> {
    run(&Usi)
}
