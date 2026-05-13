//! # see.rs
//!
//! Static Exchange Evaluation for capture move ordering.
//!
//! Simulates the full capture exchange on a target square to determine
//! whether a capture move gains or loses material. Replaces raw MVV-LVA
//! captured values in `score_move` with a signed exchange score.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 13/05/2026

use crate::*;

pub struct SimSnapshot {
    pub piece_idx: u8,
    pub piece_color: u8,
    pub start_sq: u32,
    pub end_sq: u32,
    pub prev_at_end: u8,
    pub is_royal: bool,
    pub caps: Vec<(u32, u8, u8)>,
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
