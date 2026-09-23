//! usi.rs
//!
//! Universal Shogi Interface (USI) dialect.
//!
//! The shared dispatcher already handles the `usi`/`usiok` handshake and
//! the `sfen` position keyword. This file only translates the `byoyomi`
//! time control into a fixed time for each move. Thus no shogi word gets
//! to the engine core.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                  USI DIALECT
\*----------------------------------------------------------------------------*/

/// Usi
///
/// Marker type for the USI dialect. It has no state. The shared `Session`
/// keeps the state, and this type changes only the `go` line.
///
pub struct Usi;

impl Protocol for Usi {
    /// Usi::name
    ///
    /// Gives the dialect name. The dispatcher uses it to make the `usi` and
    /// `usiok` handshake words and to select the dictionary section.
    ///
    /// Return:
    /// &str -> the protocol name, "usi"
    ///
    fn name(&self) -> &str {
        "usi"
    }

    /// Usi::execute
    ///
    /// Handles the lines that the shared loop defers after the handshake.
    /// It renames the one USI time clause before the shared search runs.
    ///
    /// - `usinewgame`       : reset the session for a new game
    /// - `go ... byoyomi n` : run as `go ... movetime n`
    ///
    /// Byoyomi is a time allowance that resets each move. A fixed move time
    /// has the same effect, so the rename loses no data.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the input line, split on whitespace
    ///
    /// Return:
    /// bool                    -> always false, USI does not quit here
    ///
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool {
        match tokens.first().copied().unwrap_or("") {
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
