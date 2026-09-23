//! uci.rs
//!
//! Universal Chess Interface (UCI) dialect.
//!
//! UCI is the base dialect of the shared session in `protocol.rs`.
//! The shared parser reads the UCI clock tokens directly. This file only
//! connects `ucinewgame` and `go` to the shared helpers.
//!
//! Created: 24/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                  UCI DIALECT
\*----------------------------------------------------------------------------*/

/// Uci
///
/// Marker type for the UCI dialect. It has no state. The shared `Session`
/// keeps the state, and this type handles the lines the dispatcher defers.
///
pub struct Uci;

impl Protocol for Uci {
    /// Uci::name
    ///
    /// Gives the dialect name. The dispatcher uses it to make the `uci` and
    /// `uciok` handshake words and to select the dictionary section.
    ///
    /// Return:
    /// &str -> the protocol name, "uci"
    ///
    fn name(&self) -> &str {
        "uci"
    }

    /// Uci::execute
    ///
    /// Handles the lines that the shared loop defers after the handshake.
    /// The shared parser uses UCI syntax, so no translation is necessary.
    ///
    /// - `ucinewgame` : reset the session for a new game
    /// - `go`         : start a search with the standard clock tokens
    ///
    /// The function ignores all other words. The dispatcher already handled
    /// every word that the engine knows.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the input line, split on whitespace
    ///
    /// Return:
    /// bool                    -> always false, UCI does not quit here
    ///
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool {
        match tokens.first().copied().unwrap_or("") {
            "ucinewgame" => new_game(session),
            "go" => start_search(session, tokens),
            _ => {}
        }
        false
    }
}
