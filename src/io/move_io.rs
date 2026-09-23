//! move_io.rs
//!
//! Formats and parses moves.
//!
//! The engine keeps moves as packed bit words. Users and protocols use
//! text. This file writes a move in the engine notation, with an optional
//! protocol dictionary. It also finds the move that a text names.
//!
//! Created: 04/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                           MOVE STRING REPRESENTATION
\*----------------------------------------------------------------------------*/

/// format_move
///
/// Writes a move in Cheesy Move Notation (CMN). If a dictionary is given,
/// the function translates the text. `parse_move` uses the same text.
///
/// CMN writes only the parts that the move has:
///
/// - `[piece]@`  : the dropped piece, only for drops
/// - `[start]`   : the start square, always written
/// - `:[end]`    : the end square, if it is not the capture square
/// - `*[square]` : a capture square, one for each captured piece
/// - `@[square]` : the square where a captured piece is put back
/// - `=[piece]`  : the piece after promotion
///
/// ```text
/// [piece]@[start]:[end]*[taken]@[unloaded]...*[taken]=[piece]
/// ```
///
/// Thus `e4*d5` and `e4:e5*d5` are two different moves.
///
/// Params:
/// - mv   : &Move               -> move to write
/// - state: &State              -> board geometry for square names
/// - dict : Option<&Translator> -> protocol translator, or None for CMN
///
/// Return:
/// String                       -> the move text, "(none)" for a null move
///
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
/// Finds the move that a text names. Thus `format_move` is the only
/// definition of the notation.
///
/// 1. generate all pseudo-legal moves and drops of the position
/// 2. write each move with `format_move` and the dictionary
/// 3. return the first move whose text is equal to the input
///
/// Params:
/// - move_str: &str                -> move text to find
/// - state   : &State              -> position that gives the moves
/// - dict    : Option<&Translator> -> translator for each move text
///
/// Return:
/// Option<Move>                    -> the pseudo-legal move, or None
///
/// Notes:
/// The move can leave a royal piece in check. The caller must apply it with
/// `make_move!` and treat a false result as an illegal move.
///
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
/// Writes the move history of the game as numbered move pairs. Each line
/// has one full move. If the length is odd, the last line has one move.
///
/// ```text
/// 1. e2:e4 e7:e5
/// 2. g1:f3 b8:c6
/// ```
///
/// Params:
/// - state: &State              -> position with the history to write
/// - dict : Option<&Translator> -> translator for the move text
///
/// Return:
/// String                       -> the numbered history, one line per move
///
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

