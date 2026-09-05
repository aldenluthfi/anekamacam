//! usi.rs
//!
//! The Universal Shogi Interface (USI) dialect.
//!
//! USI is the shared session engine with two differences the common
//! dispatcher already absorbs — the `usi`/`usiok` handshake and the `sfen`
//! position keyword both fall out of the protocol name and the `fen`/`sfen`
//! acceptance in `execute_common`. The one clause it must translate is the
//! `byoyomi` time control, mapped to a fixed per-move budget so no shogi
//! dialect token reaches the engine core.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                  USI DIALECT
\*----------------------------------------------------------------------------*/

/// Usi
///
/// The USI dialect marker. Stateless: the session lives in the shared
/// `Session`, and USI intercepts only its `go` line.
pub struct Usi;

impl Protocol for Usi {
    /// Usi::name
    ///
    /// Names the dialect. The common dispatcher builds `usi`/`usiok` and
    /// picks the dictionary section from this string, so the whole handshake
    /// and the shogi notation follow from the one word.
    ///
    /// Return:
    /// &str -> the protocol name, "usi"
    fn name(&self) -> &str {
        "usi"
    }

    /// Usi::execute
    ///
    /// Handles the lines the universal loop defers after the handshake step.
    /// USI's one dialect clause is renamed on the way past, so the engine
    /// core never learns a shogi word.
    ///
    /// ```text
    /// usinewgame        reset the session for a fresh game
    /// go ... byoyomi n  →  go ... movetime n, then the shared search
    /// ```
    ///
    /// Byoyomi is a per-move allowance that resets every move, which is what
    /// a fixed move time already is, so the rename loses nothing. The rewrite
    /// is a token-for-token map over the whole line rather than a search for
    /// the clause, since a token that spells `byoyomi` anywhere on a `go`
    /// line means the same thing wherever it sits.
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
