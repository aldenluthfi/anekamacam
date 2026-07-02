//! # evaluation.rs
//!
//! Position evaluation logic for alpha-beta search.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 19/04/2026

/// Per-side count of friendly pieces on squares adjacent to each royal piece,
/// via the precomputed `adjacency_mask` AND-ed with the side's occupancy.
/// Exposed royalty scores low; the caller folds this into the opening and
/// middlegame evaluation only, since the king should be active in the endgame.
#[macro_export]
macro_rules! king_shelter {
    ($state:expr, $color:expr) => {{
        let mut shelter = 0;

        for (piece_index, piece) in $state.statics.pieces.iter().enumerate() {
            if p_color!(piece) != $color || !p_is_royal!(piece) {
                continue;
            }

            for &king_square in &$state.piece_list[piece_index] {
                let mut adjacent =
                    $state.statics.adjacency_mask[king_square as usize];
                and!(adjacent, $state.pieces_board[$color as usize]);
                shelter += count_bits!(adjacent) as i32;
            }
        }

        shelter
    }}
}

/// Variant-agnostic pawn-structure term, returned as an `(opening, endgame)`
/// pair of white-minus-black centipawn deltas. Using the precomputed masks:
/// a pawn-like piece is passed when no enemy pawn-like piece lies on its
/// interference mask, connected when a friendly pawn-like piece lies on its
/// support mask, and doubled when a friendly pawn-like piece lies on its path.
#[macro_export]
macro_rules! pawn_structure {
    ($state:expr) => {{
        let board_size = $state.statics.board_size;

        let mut pawns = [
            board!($state.statics.files, $state.statics.ranks),
            board!($state.statics.files, $state.statics.ranks),
        ];
        for (piece_index, piece) in $state.statics.pieces.iter().enumerate() {
            if !$state.statics.pawn_like[piece_index] {
                continue;
            }
            let color = p_color!(piece) as usize;
            for &square in &$state.piece_list[piece_index] {
                set!(pawns[color], square as u32);
            }
        }

        let mut opening = 0i32;
        let mut endgame = 0i32;

        for (piece_index, piece) in $state.statics.pieces.iter().enumerate() {
            if !$state.statics.pawn_like[piece_index] {
                continue;
            }
            let color = p_color!(piece) as usize;
            let sign = -2 * color as i32 + 1;
            let own = pawns[color];
            let enemy = pawns[1 - color];

            for &square in &$state.piece_list[piece_index] {
                let entry = piece_index * board_size + square as usize;

                if disjoint!(
                    $state.statics.pawn_interference_mask[entry], enemy
                ) {
                    opening += sign * $state.statics.pawn_passed_opening[entry];
                    endgame += sign * $state.statics.pawn_passed_endgame[entry];
                }
                if !disjoint!($state.statics.pawn_support_mask[entry], own) {
                    opening += sign * $state.statics.pawn_connected_opening;
                    endgame += sign * $state.statics.pawn_connected_endgame;
                }
                if !disjoint!($state.statics.pawn_path_mask[entry], own) {
                    opening -= sign * $state.statics.pawn_doubled_penalty;
                    endgame -= sign * $state.statics.pawn_doubled_penalty;
                }
            }
        }

        (opening, endgame)
    }};
}

/// Evaluates the current position from the side-to-move perspective.
///
/// Evaluation model:
/// - `OPENING` | `SETUP`   : opening material delta + opening PST delta.
/// - `ENDGAME`             : endgame material delta + endgame PST delta.
/// - `MIDDLEGAME`          : linear interpolation between opening and endgame.
///
/// Hot-path notes:
/// - Uses cached per-side material/PST totals from `State`.
/// - Uses a branch-light side-to-move sign (`WHITE => +1`, `BLACK => -1`).
/// - Handles `opening_score == 0` safely during interpolation.
#[macro_export]
macro_rules! evaluate_position {
    ($state:expr) => {{
        let white = WHITE as usize;
        let black = BLACK as usize;

        let opening_white = $state.opening_material[white] as i32;
        let opening_black = $state.opening_material[black] as i32;
        let endgame_white = $state.endgame_material[white] as i32;
        let endgame_black = $state.endgame_material[black] as i32;

        let side_sign = -2 * $state.playing as i32 + 1;

        let tempo = $state.statics.tempo_bonus;

        let major_diff = $state.major_pieces[white] as i32
            - $state.major_pieces[black] as i32;
        let minor_diff = $state.minor_pieces[white] as i32
            - $state.minor_pieces[black] as i32;
        let imbalance = major_diff * $state.statics.imbalance_major
            + minor_diff * $state.statics.imbalance_minor;

        let mut pair_bonus = 0i32;
        for (piece_index, piece) in $state.statics.pieces.iter().enumerate() {
            if $state.piece_count[piece_index] >= 2 {
                if p_color!(piece) == WHITE {
                    pair_bonus += $state.statics.pair_bonus[piece_index];
                } else {
                    pair_bonus -= $state.statics.pair_bonus[piece_index];
                }
            }
        }

        let king_safety = 10
            * (king_shelter!($state, WHITE) - king_shelter!($state, BLACK));

        let (pawn_opening, pawn_endgame) = pawn_structure!($state);

        match $state.game_phase {
            OPENING | SETUP => {
                let score_opening = opening_white - opening_black
                    + $state.opening_pst_bonus[white]
                    - $state.opening_pst_bonus[black]
                    + imbalance + pair_bonus + king_safety + pawn_opening;
                score_opening * side_sign + tempo
            }
            ENDGAME => {
                let score_endgame = endgame_white - endgame_black
                    + $state.endgame_pst_bonus[white]
                    - $state.endgame_pst_bonus[black]
                    + imbalance + pair_bonus + pawn_endgame;
                score_endgame * side_sign + tempo
            }
            MIDDLEGAME => {
                let score_opening = (opening_white - opening_black
                    + $state.opening_pst_bonus[white]
                    - $state.opening_pst_bonus[black]
                    + imbalance + pair_bonus + king_safety + pawn_opening);

                let score_endgame = (endgame_white - endgame_black
                    + $state.endgame_pst_bonus[white]
                    - $state.endgame_pst_bonus[black]
                    + imbalance + pair_bonus + pawn_endgame);

                let opening_score = $state.statics.opening_score as i32;
                let endgame_score = $state.statics.endgame_score as i32;
                let current_score = $state.phase_score as i32;

                let blended_score = (
                        (score_opening * (current_score - endgame_score)) +
                        (score_endgame * (opening_score - current_score))
                    ) / (opening_score - endgame_score);
                blended_score * side_sign + tempo
            }
            _ => panic!("Invalid game phase {}", $state.game_phase),
        }
    }};
}
