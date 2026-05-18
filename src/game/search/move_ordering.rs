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

struct SimSnapshot {
    piece_idx: u8,
    piece_color: u8,
    start_sq: u32,
    end_sq: u32,
    prev_at_end: u8,
    is_royal: bool,
    caps: Vec<(u32, u8, u8)>,
}

fn find_lva(
    state: &State,
    target: u32,
    color: u8,
    caps_buf: &mut Vec<u64>,
) -> Option<(u8, u32, u32, Vec<u64>)> {
    let attacks = &state.statics.relevant_attacks[color as usize];
    if target as usize >= attacks.len() {
        return None;
    }

    let mut best_val = u16::MAX;
    let mut best: Option<(u8, u32, u32, Vec<u64>)> = None;

    for &(piece_idx, origin, ref vector)
        in &attacks[target as usize]
    {
        if state.main_board[origin as usize] != piece_idx {
            continue;
        }

        let mut end_sq = 0u32;
        let valid;
        process_capture_vector!(
            state,
            origin,
            &state.statics.pieces[piece_idx as usize],
            vector,
            caps_buf,
            end_sq,
            valid
        );

        if !valid {
            continue;
        }

        let hits_target = caps_buf.iter().any(|&cap| {
            !multi_move_is_unload!(cap)
                && multi_move_captured_square!(cap) == target as u64
        });
        if !hits_target {
            continue;
        }

        let val = p_ovalue!(state.statics.pieces[piece_idx as usize]);
        if val < best_val {
            best_val = val;
            best = Some((
                piece_idx,
                origin as u32,
                end_sq,
                caps_buf.clone(),
            ));
        }
    }

    best
}

/// Evaluates a capture sequence on a target square via negamax exchange
/// simulation, returning a signed material-gain score.
///
/// A positive result means the capture wins material; negative means it loses.
/// Non-capture moves return 0.
pub fn see_move(state: &mut State, mv: &Move) -> i32 {
    let mv_type = move_type!(mv);
    if mv_type != SINGLE_CAPTURE_MOVE && mv_type != MULTI_CAPTURE_MOVE {
        return 0;
    }

    let init_gain: i32 = if mv_type == SINGLE_CAPTURE_MOVE {
        if is_unload!(mv) {
            return 0;
        }
        p_ovalue!(
            state.statics.pieces[captured_piece!(mv) as usize]
        ) as i32
    } else {
        mv.1.iter()
            .filter(|&&cap| !multi_move_is_unload!(cap))
            .map(|&cap| {
                p_ovalue!(
                    state.statics.pieces[
                        multi_move_captured_piece!(cap) as usize
                    ]
                ) as i32
            })
            .sum()
    };

    if init_gain == 0 {
        return 0;
    }

    let start_sq = start!(mv) as u32;
    let piece_idx = piece!(mv) as u8;
    let end_sq = end!(mv) as u32;

    let initial_caps: Vec<u64> = if mv_type == SINGLE_CAPTURE_MOVE {
        let mut cap = 0u64;
        enc_multi_move_captured_piece!(
            cap,
            captured_piece!(mv) as u64
        );
        enc_multi_move_captured_square!(
            cap,
            captured_square!(mv) as u64
        );
        enc_multi_move_captured_unmoved!(
            cap,
            captured_unmoved!(mv) as u64
        );
        vec![cap]
    } else {
        mv.1.to_vec()
    };

    let mut undo_stack: Vec<SimSnapshot> = Vec::new();

    let applied = simulate_move!(
        state,
        &mut undo_stack,
        start_sq,
        piece_idx,
        &initial_caps,
        end_sq
    );
    if !applied {
        return 0;
    }

    let mut gain = [0i32; MAX_SEE_DEPTH];
    gain[0] = init_gain;

    let mut depth = 0usize;
    let mut target = end_sq;
    let mut val_here =
        p_ovalue!(state.statics.pieces[piece_idx as usize]) as i32;
    let mut color =
        1 - p_color!(state.statics.pieces[piece_idx as usize]);
    let mut nsim = 1usize;
    let mut caps_buf: Vec<u64> = Vec::with_capacity(8);

    loop {
        depth += 1;
        if depth >= MAX_SEE_DEPTH {
            break;
        }

        let lva = find_lva(&*state, target, color, &mut caps_buf);
        let Some((next_pidx, next_origin, next_end, next_caps)) = lva
        else {
            break;
        };

        let extra: i32 = next_caps
            .iter()
            .filter(|&&cap| {
                !multi_move_is_unload!(cap)
                    && multi_move_captured_square!(cap) != target as u64
            })
            .map(|&cap| {
                p_ovalue!(
                    state.statics.pieces[
                        multi_move_captured_piece!(cap) as usize
                    ]
                ) as i32
            })
            .sum();

        gain[depth] = val_here + extra - gain[depth - 1];

        if gain[depth].max(-gain[depth - 1]) < 0 {
            break;
        }

        let applied = simulate_move!(
            state,
            &mut undo_stack,
            next_origin,
            next_pidx,
            &next_caps,
            next_end
        );
        if !applied {
            break;
        }

        nsim += 1;
        target = next_end;
        val_here =
            p_ovalue!(state.statics.pieces[next_pidx as usize]) as i32;
        color = 1 - color;
    }

    for _ in 0..nsim {
        undo_simulate_move!(state, &mut undo_stack);
    }

    while depth > 0 {
        depth -= 1;
        gain[depth] = gain[depth].max(-gain[depth + 1]);
    }

    gain[0]
}

/*----------------------------------------------------------------------------*\
                           MOVE SCORING AND ORDERING
\*----------------------------------------------------------------------------*/

/// Returns a static ordering score for one move in the current position.
///
/// Scoring rules:
/// - Quiet/drop moves: killer heuristic, then history heuristic
/// - Captures        : SEE result + most_valuable, clamped to u16
/// - PV move         : u16::MAX (always searched first)
///
/// `$pv_move` is the TT best move for this node. A larger score means the
/// move is searched earlier.
#[macro_export]
macro_rules! score_move {
    ($state:expr, $mv:expr, $pv_move:expr) => {{
        let move_type = move_type!($mv);

        let mut score =
        if move_type != SINGLE_CAPTURE_MOVE
        && move_type != MULTI_CAPTURE_MOVE
        {
            let killers =
                &$state.killer_hist[$state.search_ply as usize];
            let countermove =
                &$state.countermove_hist[$state.search_ply as usize];
            if *$mv == killers[0] {
                10000 - 1 + MAX_DEPTH as u16                                    /* killer scores above history         */
            } else if *$mv == killers[1] {
                10000 - 2 + MAX_DEPTH as u16                                    /* killer scores above history         */
            } else if !countermove.is_empty()
                && countermove[0] != null_move()
                && *$mv == countermove[0]
                && piece!($mv) == piece!(countermove[0])
            {
                10000 - 3 + MAX_DEPTH as u16                                    /* countermove bonus                  */
            } else {
                $state.search_hist[piece!($mv) as usize][end!($mv) as usize]
            }
        } else {
            let capturing_piece_value = p_ovalue!(
                $state.statics.pieces[piece!($mv) as usize]
            ) as i32;
            let captured_piece_value = if move_type == SINGLE_CAPTURE_MOVE {
                if is_unload!($mv) {
                    0
                } else {
                    p_ovalue!(
                        $state.statics.pieces[captured_piece!($mv) as usize]
                    ) as i32
                }
            } else {
                $mv.1.iter()
                    .filter(|&&cap| !multi_move_is_unload!(cap))
                    .map(|&cap| {
                        p_ovalue!(
                            $state.statics.pieces[
                                multi_move_captured_piece!(cap) as usize
                            ]
                        ) as i32
                    })
                    .sum()
            };

            let see = if captured_piece_value > capturing_piece_value * 2 {
                captured_piece_value - capturing_piece_value
            } else {
                see_move($state, $mv)
            };

            let raw = 10000 as i32
                + see
                + MAX_DEPTH as i32;
            raw.clamp(0, u16::MAX as i32) as u16
        };

        if $pv_move.as_ref().is_some_and(
            |pm| pm.0 == $mv.0 && pm.1 == move_signature!($mv)
        ) {
            score = u16::MAX                                                    /* PV move always ordered first        */
        }

        score
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
        let mut best_score = score_move!($state, &moves[index], $pv_move);

        for (i, mv) in moves.iter().enumerate().skip(index + 1) {
            let score = score_move!($state, mv, $pv_move);
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
