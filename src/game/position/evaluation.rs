//! # evaluation.rs
//!
//! Position evaluation logic for alpha-beta search.
//!
//! The evaluator is fully variant-agnostic: it never assumes classic chess
//! material, instead reading the derived per-piece values, piece-square
//! tables, and pawn masks produced at startup by the parameter derivation
//! pass. Material and PST totals are maintained incrementally by make/undo
//! and only folded together here, keeping the hot leaf evaluation to a
//! handful of adds: material and PST deltas, imbalance and pair terms,
//! king shelter, pawn shield and castling incentives, and the mask-based
//! pawn-structure terms, phase-blended during the middlegame.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 19/04/2026

/// draw_score!
///
/// Draw value from the side to move's perspective, replacing the plain zero
/// so a winning side avoids draws and a losing side seeks them. The material
/// delta (endgame material in the endgame, opening material otherwise) is
/// scaled down by `DRAW_BIAS_DIV` and clamped to the derived `draw_bias`, then
/// negated: being ahead makes a draw look bad. Symmetric and variant-agnostic;
/// zero when material is level.
///
/// Params:
/// - state -> position whose draw value is computed
///
/// Return:
/// i32 -> draw score from the side to move's perspective
///
#[macro_export]
macro_rules! draw_score {
    ($state:expr) => {{
        let stm = $state.playing as usize;
        let opp = ($state.playing ^ 1) as usize;

        let delta = if $state.game_phase == ENDGAME {
            $state.endgame_material[stm] as i32
                - $state.endgame_material[opp] as i32
        } else {
            $state.opening_material[stm] as i32
                - $state.opening_material[opp] as i32
        };

        let max = $state.statics.draw_bias;

        -(delta / DRAW_BIAS_DIV).clamp(-max, max)
    }};
}

/// king_shelter!
///
/// Per-side count of friendly pieces on squares adjacent to each royal piece,
/// via the precomputed `adjacency_mask` AND-ed with the side's occupancy.
/// Exposed royalty scores low; the caller folds this into the opening and
/// middlegame evaluation only, since the king should be active in the endgame.
///
/// Params:
/// - state -> position whose royal adjacency is counted
/// - color -> side whose shelter is measured
///
/// Return:
/// i32 -> number of friendly pieces adjacent to the side's royals
///
#[macro_export]
macro_rules! king_shelter {
    ($state:expr, $color:expr) => {
        hotpath::measure_block!("eval::king_shelter", {
        let mut shelter = 0;

        for (piece_index, piece) in $state.statics.pieces.iter().enumerate() {
            if p_color!(piece) != $color || !p_is_royal!(piece) {
                continue;
            }

            for &king_square in piece_squares!($state, piece_index) {
                let mut adjacent =
                    $state.statics.adjacency_mask[king_square as usize];
                and!(adjacent, $state.pieces_board[$color as usize]);
                shelter += count_bits!(adjacent) as i32;
            }
        }

        shelter
        })
    }
}

/// pawn_shield!
///
/// Per-side pawn-shield bonus: counts friendly pawns standing on the derived
/// `royal_shield_mask` (the up-to-three squares one rank ahead of each royal
/// piece in the side's forward direction), capped at three per royal, and
/// scales the count by the derived `pawn_shield_bonus`. The caller folds this
/// into the opening and middlegame evaluation only.
///
/// Params:
/// - state -> position whose royal pawn cover is counted
/// - color -> side whose shield is measured
///
/// Return:
/// i32 -> pawn-shield bonus for the side, in centipawns
///
#[macro_export]
macro_rules! pawn_shield {
    ($state:expr, $color:expr) => {{
        let board_size = $state.statics.board_size;
        let mut shield = 0;

        for &royal_square in $state.royal_list[$color as usize].iter() {
            let mut cover = $state.statics.royal_shield_mask
                [$color as usize * board_size + royal_square as usize];
            and!(cover, $state.pawn_board[$color as usize]);
            shield += (count_bits!(cover) as i32).min(3);
        }

        shield * $state.statics.pawn_shield_bonus
    }};
}

/// castling_bonus!
///
/// Per-side castling incentive, active only in variants whose rules enable
/// castling: a side that has castled keeps the full derived `castled_bonus`;
/// one that still holds at least one castling right earns the smaller
/// `castling_rights_bonus`; one that burned its rights without castling gets
/// nothing. The caller folds this into the opening and middlegame evaluation
/// only.
///
/// Params:
/// - state -> position whose castling status is scored
/// - color -> side whose incentive is measured
///
/// Return:
/// i32 -> castling incentive for the side, in centipawns
///
#[macro_export]
macro_rules! castling_bonus {
    ($state:expr, $color:expr) => {{
        if !castling!($state) {
            0
        } else if $state.has_castled[$color as usize] {
            $state.statics.castled_bonus
        } else if $state.castling_state & [
            WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE
        ][$color as usize] != 0 {
            $state.statics.castling_rights_bonus
        } else {
            0
        }
    }};
}

/// pawn_structure!
///
/// Variant-agnostic pawn-structure term, returned as an `(opening, endgame)`
/// pair of white-minus-black centipawn deltas. It never assumes FIDE
/// geometry: every mask is built per-piece from that pawn's own quiet and
/// capture legs by the `derive_pawn_*` builders (each of which diagrams its
/// mask for FIDE, Berolina, and Shogi), and scoring here is only a handful
/// of O(1) `get!` bit tests against those masks.
///
/// Two sweeps over the per-side pawn lists:
///
/// 1. a pre-pass sets a bit in `passed_board[color]` for every pawn no enemy
///    pawn can reach, none sit on its interference mask
/// 2. a scoring pass tests each pawn against the masks below and adds the
///    phase deltas
///
/// Per pawn, every term is one bit test against a derived mask:
///
/// - passed    : no enemy pawn on its interference mask (set in sweep 1)
/// - connected : a friendly pawn on its support mask
/// - doubled   : a friendly pawn on its path mask (blocks the advance)
/// - isolated  : no friendly pawn on any supporting file offset
/// - backward  : not connected, and an enemy pawn captures its stop square
///
/// Passers are refined: an also-connected passer adds a protected-passer
/// bonus, and one whose supporter is itself passed adds a further
/// connected-passer bonus.
///
/// The masks take a different shape for each pawn geometry. In the overlays
/// below:
///
/// - PP : the pawn being scored
/// - pp : path: a friendly pawn here is doubled, blocking the advance
/// - ii : interference: an enemy pawn here stops the passer
/// - ss : support: a friendly pawn here connects the pawn
///
/// Support is read off one mask but has two kinds:
///
/// - behind : a friend that can capture onto the pawn itself
/// - beside : a friend that can capture onto the pawn's stop square
///
/// The beside/phalanx square only exists when a pawn captures in a different
/// direction than it moves, so a lateral neighbour's capture can reach the
/// stop. FIDE (diagonal capture, straight move) and Berolina (straight
/// capture, diagonal move) therefore gain side support, while a Shogi soldier
/// (captures and moves straight, the same way) is supported only from
/// directly behind.
///
/// A FIDE pawn captures diagonally: its path runs up its own file and its
/// supporters sit diagonally behind and beside it.
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │ ii │ pp │ ii │    │
/// ├────┼────┼────┼────┼────┤
/// │    │ ii │ pp │ ii │    │
/// ├────┼────┼────┼────┼────┤
/// │    │ ss │ PP │ ss │    │
/// ├────┼────┼────┼────┼────┤
/// │    │ ss │    │ ss │    │
/// └────┴────┴────┴────┴────┘
/// ```
///
/// A Shogi soldier moves and captures straight ahead: every mask collapses
/// onto its own file, and only the pawn directly behind can support it.
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │    │ pp │    │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ pp │    │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ PP │    │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ ss │    │    │
/// └────┴────┴────┴────┴────┘
/// ```
///
/// A Berolina pawn moves diagonally but captures straight: its path fans out
/// along both diagonals, while its supporters sit straight behind and beside.
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │ pp │ ii │ pp │ ii │ pp │
/// ├────┼────┼────┼────┼────┤
/// │    │ pp │ ii │ pp │    │
/// ├────┼────┼────┼────┼────┤
/// │    │ ss │ PP │ ss │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ ss │    │    │
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Worked examples, each on a 6x6 board with the scored pawn `P`, a friendly
/// pawn `p`, and an enemy pawn `x` (white advances up the page). The engine
/// runs identical code for all three; only the derived masks differ.
///
/// FIDE: `P` on c2, friendly `p` on b1, enemies far off on the a- and
/// f-files. No `x` touches `P`'s interference, so it is passed; `p` guards it
/// along the diagonal, so it is also connected, a protected passer. It is
/// not doubled (nothing on its file ahead) and not isolated (the `b`-file
/// friend sits on a support file):
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │ x  │    │    │    │    │ x  │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │ P  │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │ p  │    │    │    │    │
/// └────┴────┴────┴────┴────┴────┘
/// ```
///
/// Shogi: the soldier `P` on c2 has a friendly `p` directly behind on c1,
/// the only square that can defend a straight-capturing pawn, so it is
/// connected. An enemy `x` sits ahead on its own file, on the interference
/// mask, so it is not passed. A diagonal friend would not connect it here:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │ x  │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │ P  │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │ p  │    │    │    │
/// └────┴────┴────┴────┴────┴────┘
/// ```
///
/// Berolina: `P` on c2 captures straight, so the friendly `p` on c1 directly
/// behind connects it. Its path runs diagonally, so the other friendly `p`
/// on b3, forward along a diagonal, is doubled. The enemy `x` reaches its
/// interference, so it is not passed:
///
/// ```text
/// ┌────┬────┬────┬────┬────┬────┐
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │ x  │    │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │ p  │    │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │ P  │    │    │    │
/// ├────┼────┼────┼────┼────┼────┤
/// │    │    │ p  │    │    │    │
/// └────┴────┴────┴────┴────┴────┘
/// ```
///
/// Every mask is a full `Board`, so the whole term is bounded by pawn count,
/// not board area, and works at any board width. The `derive_pawn_*` builders
/// diagram how each mask is built from the piece's own legs.
///
/// Params:
/// - state -> position whose pawn-like pieces are scored
/// - pawns -> reusable per-side scratch lists of pawn entries
///
/// Return:
/// (i32, i32) -> (opening, endgame) white-minus-black structure deltas
///
#[macro_export]
macro_rules! pawn_structure {
    ($state:expr, $pawns:expr) => {
        hotpath::measure_block!("eval::pawn_structure", {
        let board_size = $state.statics.board_size;
        let files = $state.statics.files as i32;

        let interference = &$state.statics.pawn_interference_mask;
        let support = &$state.statics.pawn_support_mask;
        let path = &$state.statics.pawn_path_mask;
        let passed_opening = &$state.statics.pawn_passed_opening;
        let passed_endgame = &$state.statics.pawn_passed_endgame;
        let connected_opening = $state.statics.pawn_connected_opening;
        let connected_endgame = $state.statics.pawn_connected_endgame;
        let doubled_penalty = $state.statics.pawn_doubled_penalty;
        let isolated_penalty = $state.statics.pawn_isolated_penalty;
        let backward_penalty = $state.statics.pawn_backward_penalty;
        let backward = &$state.statics.pawn_backward_mask;
        let support_offsets = &$state.statics.pawn_support_offsets;
        let passed_support_opening =
            &$state.statics.pawn_passed_support_opening;
        let passed_support_endgame =
            &$state.statics.pawn_passed_support_endgame;

        let pawns: &mut [Vec<(usize, Square, i32)>; 2] = $pawns;
        pawns[0].clear();
        pawns[1].clear();
        for (index, piece) in $state.statics.pieces.iter().enumerate() {
            if !p_is_pawn!(piece) {
                continue;
            }
            let color = p_color!(piece) as usize;
            for &square in piece_squares!($state, index) {
                let file = square as i32 % files;
                pawns[color].push((index, square, file));
            }
        }

        let mut opening = 0i32;
        let mut endgame = 0i32;

        let mut passed_board = [
            board!($state.statics.files, $state.statics.ranks),
            board!($state.statics.files, $state.statics.ranks),
        ];
        for color in [WHITE as usize, BLACK as usize] {
            let own = &pawns[color];
            let enemy = &pawns[1 - color];
            for &(index, square, ..) in own.iter() {
                let entry = index * board_size + square as usize;
                if !enemy.iter().any(
                    |other| get!(interference[entry], other.1 as u32)
                ) {
                    set!(passed_board[color], square as u32);
                }
            }
        }

        for color in [WHITE as usize, BLACK as usize] {
            let sign = -2 * color as i32 + 1;
            let own = &pawns[color];
            let enemy = &pawns[1 - color];

            for &(index, square, file) in own.iter() {
                let entry = index * board_size + square as usize;

                let connected = own.iter().any(|other|
                    other.1 != square
                    && get!(support[entry], other.1 as u32)
                );

                if get!(passed_board[color], square as u32) {
                    opening += sign * passed_opening[entry];
                    endgame += sign * passed_endgame[entry];
                    if connected {
                        opening += sign * passed_support_opening[entry];
                        endgame += sign * passed_support_endgame[entry];
                        let chained = own.iter().any(|other|
                            other.1 != square
                            && get!(support[entry], other.1 as u32)
                            && get!(passed_board[color], other.1 as u32)
                        );
                        if chained {
                            opening += sign * passed_support_opening[entry];
                            endgame += sign * passed_support_endgame[entry];
                        }
                    }
                }

                if connected {
                    opening += sign * connected_opening;
                    endgame += sign * connected_endgame;
                }

                let doubled = own.iter().any(|other|
                    other.1 != square
                    && get!(path[entry], other.1 as u32)
                );
                if doubled {
                    opening -= sign * doubled_penalty;
                    endgame -= sign * doubled_penalty;
                }

                let has_adjacent = own.iter().any(|other|
                    other.1 != square
                    && support_offsets[index].contains(&(other.2 - file))
                );
                if !has_adjacent {
                    opening -= sign * isolated_penalty;
                    endgame -= sign * isolated_penalty;
                } else if !connected {
                    let contested = enemy.iter().any(
                        |other| get!(backward[entry], other.1 as u32)
                    );
                    if contested {
                        opening -= sign * backward_penalty;
                        endgame -= sign * backward_penalty;
                    }
                }
            }
        }

        (opening, endgame)
        })
    };
}

/// evaluate_position!
///
/// Evaluates the current position from the side-to-move perspective.
///
/// Evaluation model:
/// - `OPENING` | `SETUP`: opening material delta + opening PST delta.
/// - `ENDGAME`: endgame material delta + endgame PST delta.
/// - `MIDDLEGAME`: linear interpolation between opening and endgame.
///
/// Hot-path notes:
/// - Uses cached per-side material/PST totals from `State`.
/// - Uses a branch-light side-to-move sign (`WHITE => +1`, `BLACK => -1`).
/// - Handles `opening_score == 0` safely during interpolation.
///
/// Params:
/// - state  -> position to evaluate
/// - bufs   -> search scratch buffers (reused pawn entry lists)
/// - ptable -> shared pawn structure table
///
/// Return:
/// i32 -> score from the side to move's perspective
///
#[macro_export]
macro_rules! evaluate_position {
    ($state:expr, $bufs:expr, $ptable:expr) => {
        hotpath::measure_block!("eval::position", {
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

        let king_safety = $state.statics.king_shelter_bonus
            * (king_shelter!($state, WHITE) - king_shelter!($state, BLACK))
            + pawn_shield!($state, WHITE) - pawn_shield!($state, BLACK)
            + castling_bonus!($state, WHITE) - castling_bonus!($state, BLACK);

        let (pawn_hit, mut pawn_opening, mut pawn_endgame) =
            probe_pt_entry!($state, $ptable);

        if !pawn_hit {
            let scores = pawn_structure!($state, &mut $bufs.pawn_entry_buf);
            pawn_opening = scores.0;
            pawn_endgame = scores.1;
            hash_pt_entry!(pawn_opening, pawn_endgame, $state, $ptable);
        }

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
        })
    };
}
