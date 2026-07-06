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

/// SEE helper macros.
///
/// `attack_value!` prices the moving piece (using the promoted piece's
/// value for promotions), `victim_value!` prices everything a move
/// captures (summing multi-capture payloads, skipping unloads), and
/// `lva!` regenerates the current capture moves onto a target square
/// sorted cheapest-attacker-first. Together they feed the exchange
/// simulation in `see!`.
#[macro_export]
macro_rules! attack_value {
    ($mv:expr, $state:expr) => {{
        p_value!(
            if m_promotion!($mv) { promoted!($mv) } else { piece!($mv) },
            $state
        ) as i32
    }};
}

#[macro_export]
macro_rules! victim_value {
    ($mv:expr, $state:expr) => {{
        let move_type = move_type!($mv);

        if move_type == SINGLE_CAPTURE_MOVE {
            p_value!(captured_piece!($mv), $state) as i32
        } else if move_type == MULTI_CAPTURE_MOVE {
            m_captures!($mv).iter().fold(
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
    ($state:expr, $target:expr, $color:expr, $out:expr, $scratch:expr) => {
        hotpath::measure_block!("order::lva", {
        let state: &State = $state;
        let target: Square = $target;
        let color: u8 = $color;
        let out: &mut Vec<Move> = $out;
        let scratch: &mut Vec<u64> = $scratch;

        out.clear();

        let attacks = &state.statics.relevant_attacks
            [1 - color as usize][target as usize];

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

        out.retain(|mv| {
            m_capture!(mv)
            && (
                move_type!(mv) == SINGLE_CAPTURE_MOVE
                && captured_square!(mv) as u16 == target
                && !is_unload!(mv)
                || move_type!(mv) == MULTI_CAPTURE_MOVE
                && m_captures!(mv).iter().any(|captured| {
                    !multi_move_is_unload!(captured) &&
                    multi_move_captured_square!(captured) as u16 == target
                })
            )
        });

        out.sort_unstable_by_key(
            |mv| -(p_value!(piece!(mv), state) as i32)
        );
        })
    };
}

/// see!
///
/// Evaluates a capture sequence on a target square via negamax exchange
/// simulation, returning a signed material-gain score.
/// A positive result means the capture wins material; negative means it loses.
/// Non-capture moves return 0.
///
/// Params:
/// - state       -> position simulated on (restored before returning)
/// - mv          -> the capture move to evaluate
/// - see_moves   -> reusable buffer for attacker candidate moves
/// - see_scratch -> reusable buffer for capture payloads
///
/// Return:
/// i32 -> net material gain of the exchange for the moving side
///
#[macro_export]
macro_rules! see {
    ($state:expr, $mv:expr, $see_moves:expr, $see_scratch:expr) => {
        hotpath::measure_block!("order::see", {
        let state: &mut State = $state;
        let seen_move: &Move = $mv;

        let initial_attacker = attack_value!(seen_move, state);
        let initial_attackee = victim_value!(seen_move, state);

        let mut gain     = [0i32; 32];
        let mut gain_len = 0usize;

        gain[gain_len] = initial_attackee;
        gain_len += 1;

        gain[gain_len] = initial_attacker - initial_attackee;
        gain_len += 1;

        if !make_move!(state, seen_move.clone()) {
            -INF
        } else if initial_attacker <= initial_attackee {
            undo_move!(state);
            initial_attackee - initial_attacker
        } else {
            let target = end!(seen_move) as Square;
            let mut moves_to_undo = 1;

            'main_loop: loop {
                lva!(
                    state, target, state.playing, $see_moves, $see_scratch
                );

                let Some(mut attacker) = $see_moves.pop() else {
                    break;
                };
                let mut attacker_value = attack_value!(attacker, state);

                while !make_move!(state, attacker) {
                    if $see_moves.is_empty() {
                        break 'main_loop;
                    }

                    attacker = $see_moves.pop().unwrap();
                    attacker_value = attack_value!(attacker, state);
                }

                gain[gain_len] = attacker_value - gain[gain_len - 1];

                gain_len += 1;
                moves_to_undo += 1;

                if gain_len >= gain.len() {
                    break;
                }
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
        })
    };
}

/*----------------------------------------------------------------------------*\
                           MOVE SCORING AND ORDERING
\*----------------------------------------------------------------------------*/

/// score_move!
///
/// Returns a static ordering score for one move in the current position.
/// `pv_move` is the TT best move for this node, and a larger score means the
/// move is searched earlier. Scoring bands, highest priority first:
///
/// - pv move          : 5000000
/// - winning capture  : 4000000 + gain, in [0, MAX_PIECE_VALUE]
/// - killer move      : 1000000 + 2 * MAX_HIST_VALUE + [1, 2]
/// - history          : 1000000 + MAX_HIST_VALUE + history, in [-h, h]
/// - losing capture   : 1000000 + SEE score, in [-MAX_PIECE_VALUE, -1]
///
/// Captures whose victim value is at least the attacker's are provably
/// non-negative exchanges, so they skip SEE simulation and use the
/// victim-minus-attacker gain directly; the rest pay full SEE.
///
/// Params:
/// - state       -> position providing killers, history, and piece values
/// - mv          -> the move to score
/// - pv_move     -> TT best move for this node, if any
/// - see_moves   -> reusable buffer for attacker candidate moves
/// - see_scratch -> reusable buffer for capture payloads
///
/// Return:
/// usize -> ordering score, larger searched earlier
///
#[macro_export]
macro_rules! score_move {
    (
        $state:expr, $mv:expr, $pv_move:expr,
        $see_moves:expr, $see_scratch:expr
    ) => {{
        let scored_move: &Move = $mv;

        if $pv_move.as_ref().is_some_and(
            |pm| m_matches!(scored_move, pm)
        ) {
            5_000_000                                                           /* PV move always ordered first       */
        } else if !m_capture!(scored_move) {
            let killers =
                &$state.killer_hist[$state.search_ply as usize];

            let killer_base =
                1_000_000 + 2 * MAX_HIST_VALUE as usize;

            if *scored_move == killers[0] {
                killer_base + 2                                                 /* killer scores above history        */
            } else if *scored_move == killers[1] {
                killer_base + 1                                                 /* killer scores above history        */
            } else {
                let piece = piece!(scored_move) as usize;
                let start = start!(scored_move) as usize;
                let end = end!(scored_move) as usize;
                let board_size = $state.statics.board_size;
                let idx =
                    piece * board_size * board_size + start * board_size + end;
                let entry = $state.search_hist[idx] as i32;

                (1_000_000 + MAX_HIST_VALUE as i32 + entry) as usize
            }
        } else {
            let see_score = see!(
                $state, scored_move, $see_moves, $see_scratch
            );

            if see_score >= 0 {
                (WINNING_CAPTURE_SCORE + see_score) as usize                    /* winning captures ordered second    */
            } else {
                (LOSING_CAPTURE_SCORE + see_score) as usize                     /* losing captures ordered last       */
            }
        }
    }};
}

/// pick_by_score!
///
/// Selects the best-scoring move in `moves[index..]` and swaps it into
/// `index`. A selection-sort step that avoids sorting the whole list up
/// front; combined with alpha-beta cutoffs, only the highest-priority prefix
/// is scored in practice. `pv_move` is the TT best move for this node.
///
/// Params:
/// - state       -> position used for lazy scoring
/// - moves       -> move list, reordered in place
/// - scores      -> parallel score cache, filled lazily
/// - index       -> slot to fill with the best remaining move
/// - pv_move     -> TT best move for this node, if any
/// - see_moves   -> reusable buffer for attacker candidate moves
/// - see_scratch -> reusable buffer for capture payloads
///
/// Notes:
/// Modifies `moves` and `scores` in place; returns nothing.
///
#[macro_export]
macro_rules! pick_by_score {
    (
        $state:expr, $moves:expr, $scores:expr, $index:expr, $pv_move:expr,
        $see_moves:expr, $see_scratch:expr
    ) => {
        hotpath::measure_block!("order::pick", {
        let moves: &mut Vec<Move> = $moves;
        let scores: &mut Vec<usize> = $scores;
        let index = $index;

        if scores[index] == usize::MAX {
            scores[index] = score_move!(
                $state, &moves[index], $pv_move, $see_moves, $see_scratch
            );
        }

        let mut best_index = index;
        let mut best_score = scores[index];

        for i in (index + 1)..moves.len() {
            if scores[i] == usize::MAX {
                scores[i] = score_move!(
                    $state, &moves[i], $pv_move, $see_moves, $see_scratch
                );
            }

            if scores[i] > best_score {
                best_score = scores[i];
                best_index = i;
            }
        }

        if best_index != index {
            moves.swap(index, best_index);
            scores.swap(index, best_index);
        }
        })
    };
}
