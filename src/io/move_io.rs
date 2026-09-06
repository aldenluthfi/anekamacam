//! move_io.rs
//!
//! Implements move formatting, move parsing, and interactive move debugging.
//!
//! Moves live in the engine as opaque bit-packed words, but users and
//! protocols speak text. This file is that translation point for a single
//! move: it renders a move to the engine's canonical notation (optionally
//! through a protocol dictionary) and resolves typed move text back to the
//! one legal move it names.
//!
//! Created: 04/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                          MOVE STRING REPRESENTATION
\*----------------------------------------------------------------------------*/

/// format_move
///
/// Formats a move into the engine's canonical text representation, applying
/// protocol translation when a dictionary is supplied. The resulting string
/// is used for user-facing display and as the matching key for `parse_move`.
///
/// Cheesy Move Notation names every part of a move a variant might have,
/// and prints only the parts this move actually used:
///
/// - `[piece]@`  : the piece a drop places, dropped moves only
/// - `[start]`   : the square the move begins on, always printed
/// - `:[end]`    : where the mover lands, when that is not where it took
/// - `*[square]` : a square something was taken on, once per victim
/// - `@[square]` : where the taken piece was set back down, if it was
/// - `=[piece]`  : the piece the mover became
///
/// ```text
/// [piece]@[start]:[end]*[taken]@[unloaded]...*[taken]=[piece]
/// ```
///
/// A plain capture prints no `:[end]` at all, the mover having landed on
/// the square it took, so `e4*d5` and `e4:e5*d5` are different moves rather
/// than two spellings of one. A variant taking a piece without moving onto
/// it needs the longer form, and one where the two always coincide never
/// prints it.
///
/// Params:
///
///     mv: &Move
///     move to render
///
///     state: &State
///     board geometry for square names
///
///     dict: Option<&Translator>
///     protocol translator, or None for raw CMN
///
/// Return:
///
///     String
///     the move's CMN or translated text, "(none)" for a null move
pub fn format_move(
    mv: &Move, state: &State, dict: Option<&Translator>
) -> String {
    let mut move_str = String::new();

    if mv == &null_move() {
        return "(none)".to_string();
    }

    let move_type = move_type!(mv);

    if move_type == DROP_MOVE {
        let drop_piece = piece!(mv) as usize;
        let piece_char = state.statics.pieces[drop_piece].char;

        move_str.push_str(&format!("{}@", piece_char));
    }

    let start = start!(mv);
    let start_str = format_square(start as Square, state);

    move_str.push_str(&start_str);

    if move_type == QUIET_MOVE {
        let end = end!(mv);
        let end_str = format_square(end as Square, state);

        move_str.push_str(&format!(":{}", end_str));
    }

    if move_type == SINGLE_CAPTURE_MOVE || move_type == CASTLING_MOVE {
        let end = end!(mv);
        let capt = captured_square!(mv);

        if capt != end {
            let end_str = format_square(end as Square, state);
            let capt_str = format_square(capt as Square, state);

            move_str.push_str(&format!(":{}*{}", end_str, capt_str));
        } else {
            let end_str = format_square(end as Square, state);
            move_str.push_str(&format!("*{}", end_str));
        }

        if is_unload!(mv) {
            let unload = unload_square!(mv);
            let unload_str = format_square(unload as Square, state);

            move_str.push_str(&format!("@{}", unload_str));
        }
    }

    if !m_captures!(mv).is_empty() && move_type != CASTLING_MOVE {
        let end = end!(mv);
        let end_str = format_square(end as Square, state);

        move_str.push_str(&format!(":{}", end_str));

        for capture in m_captures!(mv).iter() {
            let capt_sq = multi_move_captured_square!(capture);
            let capt_sq_str = format_square(capt_sq as Square, state);

            move_str.push_str(&format!("*{}", capt_sq_str));

            if multi_move_is_unload!(capture) {
                let unload = multi_move_unload_square!(capture);
                let unload_str = format_square(unload as Square, state);

                move_str.push_str(&format!("@{}", unload_str));
            }
        }
    }

    if promotion!(mv) {
        let promo_piece = promoted!(mv) as usize;
        let promo_char = state.statics.pieces[promo_piece].char;

        move_str.push_str(&format!("={}", promo_char));
    }

    if let Some(translator) = dict {
        for (k, v) in &translator.moves {
            move_str = k.replace_all(&move_str, v).into_owned();
        }
    }

    move_str
}

/// parse_move
///
/// Resolves typed move text against the position by generating every move
/// the position offers, rendering each one, and returning the first whose
/// text matches. Parsing this way rather than by reading the string apart
/// means the notation is defined in exactly one place: whatever `format_move`
/// prints is what this accepts, translation included.
///
/// 1. generate every pseudo-legal move and drop the position offers
/// 2. render each of them through `format_move`, dictionary and all
/// 3. match the first whose text equals the input, both trimmed
///
/// Params:
///
///     move_str: &str
///     user move text to resolve
///
///     state: &State
///     position whose candidates are generated
///
///     dict: Option<&Translator>
///     translator applied to each candidate
///
/// Return:
///
///     Option<Move>
///     the matching pseudo-legal move, or None if none matches
///
/// Notes:
/// Candidates are pseudo-legal: the returned `Move` may leave the moving
/// side's royal pieces in check. Callers must validate with `make_move!`
/// and treat a false return as an illegal move.
pub fn parse_move(
    move_str: &str, state: &State, dict: Option<&Translator>
) -> Option<Move> {
    let mut out = Vec::with_capacity(64);
    let mut scratch = Vec::with_capacity(16);
    generate_all_moves_and_drops(state, &mut out, &mut scratch);

    out
        .into_iter()
        .find(|mv| format_move(mv, state, dict).trim() == move_str.trim())
}

/// format_move_history
///
/// Renders the game's move history as numbered move pairs, one full move
/// per line, using `format_move` for each entry.
///
/// ```text
/// 1. e2:e4 e7:e5
/// 2. g1:f3 b8:c6
/// ```
///
/// The number is the full move rather than the ply, so a history of odd
/// length ends on a half-finished line, which is what a reader expects to
/// see when it is the other side's turn.
///
/// Params:
/// - state: &State              -> position whose history is printed
/// - dict : Option<&Translator> -> translator for printed move names
///
/// Return:
/// String                       -> the numbered, newline-separated history
pub fn format_move_history(
    state: &State, dict: Option<&Translator>
) -> String {
    let mut history_strings = Vec::new();

    for snap in state.history.iter() {
        history_strings.push(format_move(&snap.move_ply, state, dict));
    }

    let mut result = String::new();

    for (i, move_str) in history_strings.iter().enumerate() {
        if i % 2 == 0 {
            result.push_str(&format!("{}. {}", (i / 2) + 1, move_str));
        } else {
            result.push_str(&format!(" {}\n", move_str));
        }
    }

    result.trim().to_string()
}

