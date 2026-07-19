//! uci.rs
//!
//! The Universal Chess Interface (UCI) dialect.
//!
//! UCI is the baseline the shared session engine in `protocol.rs` already
//! implements: its `go` carries the standard clock tokens the shared parser
//! reads directly, so this file only wires its handshake, new-game word,
//! and `go` to the shared helpers.
//!
//! Created: 24/05/2026
//! Author : Alden Luthfi
use crate::*;

/// Uci
///
/// The UCI dialect marker. Carries no state: the whole session lives in the
/// shared `Session`.
pub struct Uci;

impl Protocol for Uci {
    /// Uci::name
    ///
    /// Return:
    /// &str -> the protocol name, "uci"
    fn name(&self) -> &str {
        "uci"
    }

    /// Uci::execute
    ///
    /// Handles the lines `execute_common` defers: the `uci` handshake,
    /// `ucinewgame`, and the standard `go`.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the whitespace-split input line
    ///
    /// Return:
    /// bool                    -> always false; UCI never quits from here
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool {
        match tokens.first().copied().unwrap_or("") {
            "uci" => print_handshake(session),
            "ucinewgame" => new_game(session),
            "go" => start_search(session, tokens),
            _ => {}
        }
        false
    }
}

/// uci
///
/// Entry point for the UCI protocol loop.
///
/// Return:
/// IoResult<()> -> Ok on clean shutdown
pub fn uci() -> IoResult<()> {
    run(&Uci)
}
