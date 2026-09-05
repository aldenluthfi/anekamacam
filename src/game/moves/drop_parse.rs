//! drop_parse.rs
//!
//! Parses drop expressions into internal drop vectors and modifiers.
//!
//! A drop expression pairs optional flags with the CPMN neighbourhood
//! pattern that governs where a held piece may re-enter. This file compiles
//! those expressions into the packed drop templates the generator consumes,
//! splitting `|` branches and folding each flag into the drop word so the
//! hot path never re-parses text.
//!
//! Created: 29/01/2026
//! Author : Alden Luthfi

use crate::*;

lazy_static! {
    /// DROP_PATTERN
    ///
    /// Regex splitting one drop branch at its first `@`:
    ///
    /// - group 1 -> flag prefix, any run of `k` and `f`
    /// - group 2 -> CPMN body, compiled by `parse_pattern`
    ///
    /// The body is required to carry an `@` of its own, which is what keeps
    /// the split unambiguous: the first `@` ends the flags, the second is
    /// the one CPMN puts between its allower and stopper halves.
    static ref DROP_PATTERN: Regex =
        Regex::new(r"^([kf]*)@(.*@.*)$").unwrap_or_else(|e| {
            panic!("Failed to compile DROP_PATTERN regex: {e}")
        });
}

/*----------------------------------------------------------------------------*\
                          DROP EXPRESSION COMPILATION
\*----------------------------------------------------------------------------*/

/// generate_drop_vectors
///
/// Compiles a piece's drop expressions into packed drop templates, one per
/// `|` branch. Each branch has the form:
///
/// ```text
/// [modifiers]@[CPMN]
/// ```
///
/// The CPMN body is the neighbourhood the target square must present, and
/// the modifier prefix carries what a neighbourhood cannot say:
///
/// - k -> this drop may not be the move that delivers checkmate
///
/// Each branch leaves a [`DropMove`] carrying the piece index and that flag,
/// paired with its compiled pattern. The square half of the word stays zero:
/// a template is compiled once for the piece, and `generate_relevant_drops`
/// stamps a square into every copy it keeps.
///
/// Params:
/// - piece   : &Piece    -> piece type whose drop expression is compiled
/// - state   : &State    -> piece dictionary and board dimensions
/// - expr_set: &[String] -> drop expressions, one per piece
///
/// Return:
/// DropSet               -> one packed (drop, pattern) pair per `|` branch
///
/// Notes:
/// The prefix also admits an `f` that nothing reads, so a variant spelling
/// it gets the drop it would have got without it. A branch that does not
/// match [`DROP_PATTERN`] panics, drop rules being config text compiled once
/// at load time rather than anything a position can produce.
pub fn generate_drop_vectors(
    piece: &Piece,
    state: &State,
    expr_set: &[String],
) -> DropSet {
    let piece_index = p_index!(piece) as usize;
    let drop_expr = &expr_set[piece_index];

    let parts = drop_expr.split('|').collect::<Vec<&str>>();

    let mut drop_set = Vec::new();
    for part in parts {
        let captures = DROP_PATTERN
            .captures(part)
            .unwrap_or_else(|| panic!("Invalid drop format {}", part));


        log_4!(
            "Captured groups for piece {}: {:?}",
            piece.name, captures
        );

        let mut move_result = piece_index as u32;

        let modifiers = captures.get(1).map_or("", |m| m.as_str());

        if modifiers.contains('k') {
            move_result |= 1 << 20;
        }

        let pattern = captures.get(2).unwrap_or_else(|| {
            panic!("Drop pattern missing matcher body in: {}", part)
        }).as_str();

        drop_set.push((move_result, parse_pattern(pattern, state)));
    }

    drop_set
}
