//! ucci.rs
//!
//! The Universal Chinese Chess Interface (UCCI) dialect.
//!
//! UCCI shares the whole session engine; the `ucci`/`ucciok` handshake and
//! the `fen` position keyword fall out of the common dispatcher. What
//! differs is the `go` line: UCCI states the side-to-move's clock directly
//! as `time`/`increment` rather than UCI's `wtime`/`btime` split. This file
//! translates that into the standard clock tokens the shared parser expects,
//! keeping the dialect out of the engine core.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi
use crate::*;

/// Ucci
///
/// The UCCI dialect marker. Stateless: the session lives in the shared
/// `Session`, and UCCI intercepts only its `go` line.
pub struct Ucci;

impl Protocol for Ucci {
    /// Ucci::name
    ///
    /// Return:
    /// &str -> the protocol name, "ucci"
    fn name(&self) -> &str {
        "ucci"
    }

    /// Ucci::execute
    ///
    /// Handles the lines the universal loop defers after the handshake step:
    /// `go` (UCCI has no new-game command). The `go` rewrites UCCI's
    /// side-relative clock into the engine's standard tokens: `time t`
    /// becomes `wtime t btime t` and `increment i` becomes `winc i binc i`,
    /// so `start_search` picks the clock for whichever side is to move.
    /// Opponent-clock and advisory clauses (`opptime`, `draw`, `mate`, ...)
    /// are dropped.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the whitespace-split input line
    ///
    /// Return:
    /// bool                    -> always false; UCCI never quits from here
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool {
        match tokens.first().copied().unwrap_or("") {
            "go" => {}
            _ => return false,
        }

        let mut normalized: Vec<String> = vec!["go".to_string()];
        let mut index = 1;

        while index < tokens.len() {
            match tokens[index] {
                "time" => {
                    if let Some(value) = tokens.get(index + 1) {
                        normalized.push("wtime".to_string());
                        normalized.push((*value).to_string());
                        normalized.push("btime".to_string());
                        normalized.push((*value).to_string());
                    }
                    index += 2;
                }
                "increment" => {
                    if let Some(value) = tokens.get(index + 1) {
                        normalized.push("winc".to_string());
                        normalized.push((*value).to_string());
                        normalized.push("binc".to_string());
                        normalized.push((*value).to_string());
                    }
                    index += 2;
                }
                "movestogo" | "depth" | "nodes" | "movetime" => {
                    normalized.push(tokens[index].to_string());
                    if let Some(value) = tokens.get(index + 1) {
                        normalized.push((*value).to_string());
                    }
                    index += 2;
                }
                "ponder" | "infinite" => {
                    normalized.push(tokens[index].to_string());
                    index += 1;
                }
                "opptime" | "oppincrement" | "oppmovestogo" | "mate" => {
                    index += 2;                                                 /* value-bearing, not used by search  */
                }
                _ => {
                    index += 1;                                                 /* flags such as `draw`, ignored      */
                }
            }
        }

        let refs: Vec<&str> = normalized.iter().map(String::as_str).collect();
        start_search(session, &refs);
        false
    }
}
