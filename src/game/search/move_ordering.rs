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
                    let is_unload = multi_move_is_unload!(captured);
                    let piece = multi_move_captured_piece!(captured);
                    acc + p_value!(piece, $state) as i32 * !is_unload as i32
                }
            )
        } else {
            unreachable!()
        }
    }};
}

#[macro_export]
macro_rules! lva {
    ($state:expr, $target:expr, $color:expr) => {{
        let state: &State = $state;
        let target: Square = $target;
        let color: u8 = $color;

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

        out = out.into_iter().filter_map(|mv| {
            if m_capture!(mv)
            && move_type!(mv) == SINGLE_CAPTURE_MOVE
            && captured_square!(mv) as u16 == $target
            && !is_unload!(mv) {
                Some(mv)
            } else if m_capture!(mv)
            && move_type!(mv) == MULTI_CAPTURE_MOVE
            && mv.1.iter().any({
                |captured| {
                    !multi_move_is_unload!(captured) &&
                    multi_move_captured_square!(captured) as u16 == $target
                }
            }) {
                Some(mv)
            } else {
                None
            }
        })
        .collect::<Vec<Move>>();

        out.sort_by_cached_key(
            |mv| -(p_value!(piece!(mv), state) as i32)
        );

        out
    }};
}

/// Evaluates a capture sequence on a target square via negamax exchange
/// simulation, returning a signed material-gain score.
///
/// A positive result means the capture wins material; negative means it loses.
/// Non-capture moves return 0.
#[macro_export]
macro_rules! see {
    ($state:expr, $mv:expr) => {{
        let state: &mut State = $state;
        let mv: Move = $mv.clone();

        let initial_attacker = attack_value!(mv, state);
        let initial_attackee = victim_value!(mv, state);

        let mut gain     = [0i32; 32];
        let mut gain_len = 0usize;

        gain[gain_len] = initial_attackee;     gain_len += 1;
        gain[gain_len] = initial_attacker - initial_attackee; gain_len += 1;

        if !make_move!(state, mv.clone()) {
            -20000
        } else if initial_attackee > initial_attacker {
            undo_move!(state);
            initial_attackee - initial_attacker
        } else {
            let mut moves_to_undo = 1;

            'main_loop: loop {
                let target = end!(mv) as Square;
                let mut move_list = lva!(state, target, state.playing);

                let Some(mut mv) = move_list.pop() else {
                    break;
                };

                while !make_move!(state, mv.clone()) {
                    if move_list.is_empty() {
                        break 'main_loop;
                    }

                    mv = move_list.pop().unwrap();
                }

                gain[gain_len] =
                    attack_value!(mv, state) - gain[gain_len - 1];
                gain_len += 1;
                moves_to_undo += 1;

                break;
            }

            gain_len -= 1;                                                      /* no recapture for last attacker     */

            if gain_len > 1 {
                for i in (1..gain_len).rev() {
                    gain[i - 1] = -cmp::max(-gain[i - 1], gain[i]);
                }
            }

            while moves_to_undo > 0 {
                undo_move!(state);
                moves_to_undo -= 1;
            }

            gain[0]
        }
    }};
}

/*----------------------------------------------------------------------------*\
                           MOVE SCORING AND ORDERING
\*----------------------------------------------------------------------------*/

/// Returns a static ordering score for one move in the current position.
///
/// Scoring order:
/// 1. PV move                  -> 5000000
/// 2. Captures (MVV-LVA only)  -> 4000000 + victim - attacker/16
/// 3. Killer moves             -> 1000000 + 2*MAX_HIST_VALUE + [1, 2]
/// 4. History heuristic        -> 1000000 + MAX_HIST_VALUE + history [-h, h]
/// 5. Losing captures          -> 1000000 + SEE score [-MAX_PIECE_VALUE, -1]
///
/// SEE is no longer called during initial scoring — that work is deferred
/// to `pick_by_score!`, which only runs SEE on the capture it is about to
/// return. Captures with verified `see < 0` are demoted to bucket 5.
///
/// History entries are signed i16; the `MAX_HIST_VALUE` offset shifts them
/// into a non-negative usize bucket strictly below killers. `$pv_move` is
/// the TT best move for this node.
#[macro_export]
macro_rules! score_move {
    ($state:expr, $mv:expr, $pv_move:expr) => {{
        if $pv_move.as_ref().is_some_and(
            |pm| m_matches!($mv, pm)
        ) {
            5_000_000                                                           /* PV move always ordered first        */
        } else if !m_capture!($mv) {
            let killers =
                &$state.killer_hist[$state.search_ply as usize];

            let killer_base =
                1_000_000 + 2 * MAX_HIST_VALUE as usize;

            if $mv == killers[0] {
                killer_base + 2                                                 /* killer scores above history         */
            } else if $mv == killers[1] {
                killer_base + 1                                                 /* killer scores above history         */
            } else {
                let piece = piece!($mv) as usize;
                let from  = start!($mv) as usize;
                let end   = end!($mv) as usize;
                let board_size = $state.statics.board_size;
                let idx =
                    piece * board_size * board_size
                    + from * board_size + end;
                let entry = $state.search_hist[idx] as i32;

                (1_000_000
                    + MAX_HIST_VALUE as i32
                    + entry) as usize                                           /* signed history offset to usize     */
            }
        } else {
            let victim   = victim_value!($mv, $state);
            let attacker = attack_value!($mv, $state);

            (4_000_000 + victim - attacker / 16) as usize                       /* MVV-LVA, SEE deferred to pick      */
        }
    }};
}

/// Selects the best-scoring move in `moves[$index..]` and swaps it into
/// `$index`.
///
/// Selection-sort step; avoids sorting the full list up front. Combined with
/// alpha-beta cutoffs, only the highest-priority prefix is scored in practice.
///
/// Lazy SEE: the picked move is verified with full SEE only if it is in the
/// MVV-LVA capture bucket `[4M, 5M)`. A capture with `see < 0` is demoted to
/// the losing-capture bucket `1M + see` and the pick is retried. This caps
/// SEE work at one call per move actually returned, even when many losing
/// captures are present.
///
/// `$pv_move` is the TT best move for this node.
#[macro_export]
macro_rules! pick_by_score {
    ($state:expr, $moves:expr, $scores:expr, $index:expr, $pv_move:expr) => {{
        let moves: &mut Vec<Move> = $moves;
        let scores: &mut Vec<usize> = $scores;
        let index = $index;

        loop {
            if scores[index] == usize::MAX {
                scores[index] = score_move!(
                    $state, moves[index].clone(), $pv_move
                );
            }

            let mut best_index = index;
            let mut best_score = scores[index];

            for i in (index + 1)..moves.len() {
                if scores[i] == usize::MAX {
                    scores[i] = score_move!(
                        $state, moves[i].clone(), $pv_move
                    );
                }

                if scores[i] > best_score {
                    best_score = scores[i];
                    best_index = i;
                }
            }

            if best_score >= 4_000_000 && best_score < 5_000_000 {
                let candidate = moves[best_index].clone();
                let see_score = see!($state, candidate);
                if see_score < 0 {
                    scores[best_index] =
                        (1_000_000 + see_score) as usize;
                    continue;                                                   /* demoted; re-pick best              */
                }
            }

            if best_index != index {
                moves.swap(index, best_index);
                scores.swap(index, best_index);
            }
            break;
        }
    }};
}
