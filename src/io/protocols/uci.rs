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

/*----------------------------------------------------------------------------*\
                                  UCI DIALECT
\*----------------------------------------------------------------------------*/

/// Uci
///
/// The UCI dialect marker. Carries no state at all: the session lives in the
/// shared `Session`, and this type exists only to name the dialect and to be
/// asked what it does with a line the common dispatcher passed on.
pub struct Uci;

impl Protocol for Uci {
    /// Uci::name
    ///
    /// Names the dialect. The name is load-bearing rather than cosmetic: the
    /// common dispatcher builds the handshake words and picks the dictionary
    /// section from it, so "uci" is what makes `uci`/`uciok` work.
    ///
    /// Return:
    /// &str -> the protocol name, "uci"
    fn name(&self) -> &str {
        "uci"
    }

    /// Uci::execute
    ///
    /// Handles the lines the universal loop defers after the handshake step.
    /// UCI is the dialect the shared parser was written against, so there is
    /// nothing to translate and both lines go straight to the shared helpers.
    ///
    /// - `ucinewgame` : reset the session for a fresh game
    /// - `go`         : search under the standard clock tokens, unchanged
    ///
    /// Anything else is silently ignored rather than reported, because the
    /// common dispatcher has already handled every line this engine answers
    /// and an unknown word reaching here was never ours to complain about.
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
            "ucinewgame" => new_game(session),
            "go" => start_search(session, tokens),
            _ => {}
        }
        false
    }
}
