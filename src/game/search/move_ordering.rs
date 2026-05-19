//! # move_ordering.rs
//!
//! Static exchange evaluation and move scoring for search-time ordering.
//!
//! Captures are scored via a full exchange simulation (SEE); quiet moves via
//! killer and history heuristics. Moves are selected incrementally with a
//! selection-sort step, deferring scoring to avoid work at early cutoffs.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 19/04/2026

use crate::*;

/*----------------------------------------------------------------------------*\
                          STATIC EXCHANGE EVALUATION
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! attack_value {
    ($mv:expr, $state:expr) => {
        p_value!(piece!($mv), $state) as i32
    };
}

#[macro_export]
macro_rules! victim_value {
    ($mv:expr, $state:expr) => {{
        let move_type = move_type!($mv);

        if move_type == SINGLE_CAPTURE_MOVE {
            p_value!(captured_piece!($mv), $state) as i32
        } else if move_type == MULTI_CAPTURE_MOVE {
            $mv.1.iter().fold(
                0,
                |acc, &captured|
                {
                    let piece = multi_move_captured_piece!(captured);
                    acc + p_value!(piece, $state) as i32
                }
            )
        } else {
            0
        }
    }};
}

fn lva(
    state: &State,
    target: Square,
    color: u8,
) -> Move {
    let attacks = &state.statics.relevant_attacks
        [1 - color as usize][target as usize];

    let mut out = Vec::with_capacity(attacks.len());
    let scratch = &mut Vec::with_capacity(attacks.len());
    attacks.iter()
        .filter_map(|(piece, square, vector)| {
            let real_piece = state.main_board[*square as usize];
            if real_piece == *piece {
                Some((piece, square, vector))
            } else {
                None
            }
        })
        .for_each(|(p_index, s_index, vector)| {
            let piece = &state.statics.pieces[*p_index as usize];
            process_multi_leg_vector!(
                *s_index, piece, vector, state, out, scratch
            );
        });

    out.sort_by_cached_key(
        |mv| p_value!(piece!(mv), state)
    );

    out.first()
        .cloned()
        .unwrap_or_else(null_move)
}

/// Evaluates a capture sequence on a target square via negamax exchange
/// simulation, returning a signed material-gain score.
///
/// A positive result means the capture wins material; negative means it loses.
/// Non-capture moves return 0.
pub fn see(state: &mut State, mv: Move) -> i32 {
    let initial_attacker = attack_value!(mv, state);
    let initial_attackee = victim_value!(mv, state);

    if initial_attackee > initial_attacker {
        return initial_attackee - initial_attacker;
    }

    let mut gain = Vec::with_capacity(32);

    gain.push(initial_attackee);
    gain.push(initial_attacker - initial_attackee);
    make_move!(state, mv.clone());
    let mut moves_to_undo = 1;

    loop {
        let target = end!(mv) as Square;
        let mv = lva(state, target, state.playing);

        if mv == null_move() {
            break;
        }

        log_4!("SEE: {} captures on {}", format_move(&mv, state), target);

        make_move!(state, mv.clone());

        gain.push(attack_value!(mv, state) - gain.last().unwrap());
        moves_to_undo += 1;
    }

    gain.pop();                                                                 /* no recapture for last attacker     */

    if gain.len() > 1 {
        for i in (1..gain.len()).rev() {
            gain[i - 1] = -cmp::max(-gain[i - 1], gain[i]);
        }
    }

    while moves_to_undo > 0 {
        undo_move!(state);
        moves_to_undo -= 1;
    }

    gain[0]
}

/*----------------------------------------------------------------------------*\
                           MOVE SCORING AND ORDERING
\*----------------------------------------------------------------------------*/

/// Returns a static ordering score for one move in the current position.
///
/// Scoring order:
/// 1. PV move                  -> 50000
/// 2. Winning/Equal captures   -> 40000 + SEE score [0, MAX_PIECE_VALUE]
/// 3. Killer moves             -> 20000 + MAX_DEPTH^2 + 2 - killer rank [0,1]
/// 4. History heuristic        -> 20000 + history score [0, MAX_DEPTH^2]
/// 5. Losing captures          -> 20000 + SEE score [-MAX_PIECE_VALUE, -1]
///
/// `$pv_move` is the TT best move for this node. A larger score means the
/// move is searched earlier.
#[macro_export]
macro_rules! score_move {
    ($state:expr, $mv:expr, $pv_move:expr) => {{
        if $pv_move.as_ref().is_some_and(
            |pm| pm.0 == $mv.0 && pm.1 == move_signature!($mv)
        ) {
            50000                                                                /* PV move always ordered first        */
        } else if !is_capture!($mv) {
            let killers =
                &$state.killer_hist[$state.search_ply as usize];

            if $mv == killers[0] {
                10000 + (MAX_DEPTH * MAX_DEPTH) as u16 + 2                      /* killer scores above history         */
            } else if $mv == killers[1] {
                10000 + (MAX_DEPTH * MAX_DEPTH) as u16 + 1                      /* killer scores above history         */
            }  else {
                let piece = piece!($mv) as usize;
                let end = end!($mv) as usize;

                10000 + $state.search_hist[piece][end] as u16                   /* history score for quiet moves       */
            }
        } else {
            let see_score = see($state, $mv);

            if see_score >= 0 {
                (40000 + see_score) as u16                                      /* winning captures ordered second     */
            } else {
                (20000 + see_score) as u16                                      /* losing captures ordered last        */
            }
        }
    }};
}

/// Selects the best-scoring move in `moves[$index..]` and swaps it into
/// `$index`.
///
/// Selection-sort step; avoids sorting the full list up front. Combined with
/// alpha-beta cutoffs, only the highest-priority prefix is scored in practice.
///
/// `$pv_move` is the TT best move for this node.
#[macro_export]
macro_rules! pick_by_score {
    ($state:expr, $moves:expr, $index:expr, $pv_move:expr) => {{
        let moves: &mut Vec<Move> = $moves;
        let index = $index;

        let mut best_index = index;
        let mut best_score = score_move!(
            $state, moves[index].clone(), $pv_move
        );

        for (i, mv) in moves.iter().enumerate().skip(index + 1) {
            let score = score_move!($state, mv.clone(), $pv_move);
            if score > best_score {
                best_score = score;
                best_index = i;
            }
        }

        if best_index != index {
            moves.swap(index, best_index);
        }
    }};
}
