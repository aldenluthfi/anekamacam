//! # search.rs
//!
//! Defines search-time control and accounting data used by the engine.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 22/03/2026

use crate::{game::position::evaluation::evaluate_position, *};

/// Tracks limits, counters, and stop flags for an active search.
///
/// This struct groups time controls, depth/move constraints, node accounting,
/// and interruption controls used by iterative search routines.
/// It is mutated throughout one search invocation lifecycle.
pub struct SearchInfo {
    pub start_time: u128,
    pub stop_time: u128,

    pub depth: usize,

    pub set_depth: usize,
    pub set_timed: u128,
    pub set_moves: usize,

    pub nodes: u128,

    pub interrupt: bool,
    pub infinite: bool,
}

impl Default for SearchInfo {
    fn default() -> Self {
        Self {
            start_time: 0,
            stop_time: 0,
            depth: 0,
            set_depth: 0,
            set_timed: 0,
            set_moves: 0,
            nodes: 0,
            interrupt: false,
            infinite: false,
        }
    }
}

pub fn check_interrupt() {}

pub fn clear_search(state: &mut State, info: &mut SearchInfo) {
    info.start_time = Instant::now().elapsed().as_nanos();
    info.nodes = 0;
    info.interrupt = false;

    let piece_count: usize = state.pieces.len();
    let board_size: usize = (state.files as usize) * (state.ranks as usize);

    state.search_hist = vec![vec![null_move(); board_size]; piece_count];
    state.killer_hist = vec![array::from_fn(|_| null_move()); piece_count];
    state.pv_table = vec![(null_move(), 0); PV_TABLE_SIZE];

    state.search_ply = 0;
}

pub fn search_position(state: &mut State, info: &mut SearchInfo) {

    let mut best_move: &Move;
    let mut best_score: i32;

    clear_search(state, info);

    for depth in 1..=info.depth {
        best_score = alpha_beta(
            state, depth, i32::MIN + 1, i32::MAX, info, true                    /* i32::MIN + 1 to avoid overflow     */
        );
        fill_pv_line(state, depth);
        best_move = &state.pv_line[0];

        if info.interrupt {
            break;
        }

        println!(
            "[INFO] Depth: {}, Score: {}, Nodes: {}, Best Move: {}",
            depth,
            best_score,
            info.nodes,
            format_move(best_move, state)
        );

        println!(
            "[INFO] Best Line: {}",
            state.pv_line
                .iter()
                .take(depth)
                .take_while(|m| m != &&null_move())
                .map(|m| format_move(m, state))
                .collect::<Vec<String>>()
                .join(" ")
        );

        for mv in state.pv_line.iter().take(depth) {
            if mv == &null_move() {
                break;
            }

            make_move!(state, mv.clone());
        }

        while state.search_ply > 0 {
            undo_move!(state);
        }
    }
}

pub fn alpha_beta(
    state: &mut State,
    depth: usize,
    alpha: i32,
    beta: i32,
    info: &mut SearchInfo,
    null: bool,
) -> i32 {
    let mut alpha = alpha;

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if depth == 0 {
        info.nodes += 1;
        return evaluate_position(state);

    }

    info.nodes += 1;

    let repetition_count = state
        .position_hash_map
        .get(&state.position_hash)
        .copied()
        .unwrap_or(0);

    if repetition_count >= state.repetition_limit
    || state.halfmove_clock >= 50
    {
        return 0;
    }

    if  state.search_ply >= MAX_DEPTH as u32 {
        return evaluate_position(state);
    }

    let mut legal_moves = 0;
    let alpha_start = alpha;
    let mut best_move: Move = null_move();

    for mv in generate_all_moves_and_drops(state) {
        if !make_move!(state, mv.clone()) {
            continue;
        }

        legal_moves += 1;
        let score = -alpha_beta(state, depth - 1, -beta, -alpha, info, true);
        undo_move!(state);

        if score > alpha {
            if score >= beta {
                return beta;
            }

            alpha = score;
            best_move = mv;
        }
    }

    if legal_moves == 0 {
        if is_in_check!(state.playing, state) {
            let mate_score = -MATE_SCORE + state.search_ply as i32;

            return if state.history.last().map_or(false, |s| {
                move_type!(&s.move_ply) == DROP_MOVE &&
                !drop_can_checkmate!(&s.move_ply)
            }) {
                -mate_score
            } else {
                mate_score
            };

        }

        return 0;
    }

    #[cfg(debug_assertions)]
    verify_game_state(state);

    if alpha != alpha_start {
        hash_pv_move(best_move, state);
    }

    alpha
}
