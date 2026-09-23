//! ucci.rs
//!
//! Universal Chinese Chess Interface (UCCI) dialect.
//!
//! The shared dispatcher already handles the `ucci`/`ucciok` handshake and
//! the `fen` position keyword. Only the `go` line is different. UCCI gives
//! the clock of the side to move as `time` and `increment`. This file
//! translates these into the standard clock tokens of the shared parser.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                  UCCI DIALECT
\*----------------------------------------------------------------------------*/

/// Ucci
///
/// Marker type for the UCCI dialect. It has no state. The shared `Session`
/// keeps the state, and this type changes only the `go` line.
///
pub struct Ucci;

impl Protocol for Ucci {
    /// Ucci::name
    ///
    /// Gives the dialect name. The dispatcher uses it to make the `ucci` and
    /// `ucciok` handshake words and to select the dictionary section.
    ///
    /// Return:
    /// &str -> the protocol name, "ucci"
    ///
    fn name(&self) -> &str {
        "ucci"
    }

    /// Ucci::execute
    ///
    /// Handles `go`, the only line that the shared loop defers to UCCI.
    /// UCCI has no new game command. UCCI gives only the clock of the side
    /// to move, so the function gives this clock to the two colours.
    ///
    /// - `time t`       : `wtime t btime t`
    /// - `increment i`  : `winc i binc i`
    /// - `movestogo n`  : kept, also `depth`, `nodes` and `movetime`
    /// - `ponder`       : kept, also `infinite`
    /// - `opptime t`    : removed, also `oppincrement`, `oppmovestogo`, `mate`
    /// - `draw`         : removed, also all other flags without a value
    ///
    /// The search reads only the clock of the side to move, so the copy is
    /// safe. The loop skips two tokens for a word with a value and one token
    /// for a flag. Thus a removed word also removes its value.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the input line, split on whitespace
    ///
    /// Return:
    /// bool                    -> always false, UCCI does not quit here
    ///
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
