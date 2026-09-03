//! evaluation.rs
//!
//! Static position evaluation for alpha-beta search.
//!
//! Material values and piece-square tables are derived from variant rules at
//! startup and maintained incrementally by make/undo. Opening and endgame
//! totals are blended by current material phase, with no additional positional
//! terms. Terminal positions are scored here too, and a drawn one is priced by
//! the material lead standing on the board rather than at a flat zero.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

/// draw_score!
///
/// What a draw is worth to the side to move, in place of a plain zero. A side
/// holding more material than its opponent has something to lose by agreeing
/// the game, so the lead is read back as a cost: ahead scores the draw below
/// zero and behind scores it above. The lead is measured in the material the
/// current phase prices, clamped to the derived `draw_span`, and paid at
/// `draw_contempt` for a full span.
///
/// Both halves are shares of this variant's own mean deployed piece, so a
/// variant playing in small units is not handed a large contempt, and no rule
/// is asked about beyond the material already on the board. A level position
/// scores zero, and the score a colour reads is the negation of what its
/// opponent reads from the same position.
///
/// Params:
/// - state: &State -> position whose draw value is computed
///
/// Return:
/// i32             -> draw value from side-to-move perspective
#[macro_export]
macro_rules! draw_score {
    ($state:expr) => {{
        let moving = $state.playing as usize;
        let waiting = ($state.playing ^ 1) as usize;

        let lead = if $state.game_phase == ENDGAME {
            $state.endgame_material[moving] as i32
                - $state.endgame_material[waiting] as i32
        } else {
            $state.opening_material[moving] as i32
                - $state.opening_material[waiting] as i32
        };

        let span = $state.statics.draw_span;

        -lead.clamp(-span, span) * $state.statics.draw_contempt / span
    }};
}

/// terminal_score!
///
/// Scores a terminal position from side-to-move perspective. A decisive result
/// uses mate-scaled scores so shorter wins and longer losses are preferred.
/// A draw is worth what [`draw_score!`] says the material on the board makes
/// it worth.
///
/// Params:
/// - state: &State -> position whose terminal value is computed
///
/// Return:
/// i32             -> terminal score from side-to-move perspective
#[macro_export]
macro_rules! terminal_score {
    ($state:expr) => {{
        let stm_wins = ($state.termination.game_result == WHITE_WIN
            && $state.playing == WHITE)
            || ($state.termination.game_result == BLACK_WIN
                && $state.playing == BLACK);
        let stm_loses = ($state.termination.game_result == WHITE_WIN
            && $state.playing == BLACK)
            || ($state.termination.game_result == BLACK_WIN
                && $state.playing == WHITE);

        if stm_wins {
            INF - $state.search_ply as i32
        } else if stm_loses {
            -INF + $state.search_ply as i32
        } else {
            draw_score!($state)
        }
    }};
}

/// royal_shelter!
///
/// Worth of shield-like friendly pieces standing ahead of one colour's royals.
/// Each royal reads one precomputed forward-square list. Count is capped, so a
/// royal buried in its own army stops earning once its shelter is full.
///
/// Params:
/// - state: &State -> position whose royals are read
/// - color: usize  -> colour whose royals are scored
///
/// Return:
/// i32             -> shelter worth, always non-negative
#[macro_export]
macro_rules! royal_shelter {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let stride = statics.local_stride;
        let cap = SHELTER_CAP as i32;
        let mut worth = 0;

        for royal_square in &$state.royal_list[$color] {
            let royal = *royal_square as usize;
            let mut shelter = 0;

            for slot in 0..statics.shelter_counts[$color][royal] as usize {
                let square = statics.shelter_squares[$color]
                    [royal * stride + slot] as usize;
                let piece = $state.main_board[square];

                if piece != NO_PIECE
                    && statics.shield_pieces[piece as usize]
                    && p_color!(&statics.pieces[piece as usize]) as usize
                        == $color {
                    shelter += 1;
                }
            }

            worth += statics.shelter_value * shelter.min(cap);
        }

        worth
    }};
}

/// royal_guard!
///
/// Worth of the friendly pieces standing on the ring around one colour's
/// royals, whatever they are and whichever side of the royal they stand on.
/// Each such piece blocks one line into the square its royal occupies, which
/// is worth having even from a piece that shelters nothing, so this is priced
/// below [`royal_shelter!`] and counted over the whole ring rather than its
/// forward half. The ring bounds the count on its own and needs no cap.
///
/// Params:
/// - state: &State -> position whose royal neighbours are read
/// - color: usize  -> colour whose guard is scored
///
/// Return:
/// i32             -> guard worth, always non-negative
#[macro_export]
macro_rules! royal_guard {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let stride = statics.local_stride;
        let mut guards = 0;

        for royal_square in &$state.royal_list[$color] {
            let royal = *royal_square as usize;

            for slot in 0..statics.ring_counts[royal] as usize {
                let square = statics.ring_squares[royal * stride + slot];

                guards += get!(
                    $state.pieces_board[$color], square as u32
                ) as i32;
            }
        }

        statics.guard_value * guards
    }};
}

/// king_danger!
///
/// What the enemy army's pressure on one colour's royals costs it. Every
/// enemy piece that is neither royal nor shield-like reads its precomputed
/// `zone_attack` pressure from the square it stands on, and where the rules
/// allow drops every copy held in hand reads `zone_attack_best`, the
/// pressure it would exert from the origin it would pick. A hand is the
/// whole attacking reserve of a drop variant, and leaving it out makes a
/// hand of two queens as harmless as an empty one; drop legality is
/// deliberately not checked, since over-stating a held attacker errs toward
/// caution.
///
/// The total is charged as its square, so one attacker barely registers and
/// several compound, and the charge is capped where a further attacker
/// would say more than winning the dearest piece outright says. The caller
/// subtracts this from the side standing under it.
///
/// Params:
/// - state: &State -> position whose royal zones are read
/// - color: usize  -> colour whose royals stand under the pressure
///
/// Return:
/// i32             -> danger charged against that colour, non-negative
#[macro_export]
macro_rules! king_danger {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let board_size = statics.board_size;
        let piece_count = statics.pieces.len();
        let hand = &$state.piece_in_hand[$color ^ 1];
        let drops = drops!($state);
        let mut units = 0i64;

        for royal_square in &$state.royal_list[$color] {
            let royal = *royal_square as usize;
            let zone = &statics.zone_attack[
                royal * piece_count * board_size
                    ..(royal + 1) * piece_count * board_size
            ];
            let best = &statics.zone_attack_best[
                royal * piece_count..(royal + 1) * piece_count
            ];

            for (piece_index, piece) in statics.pieces.iter().enumerate() {
                if p_color!(piece) as usize == $color
                    || p_is_royal!(piece)
                    || statics.shield_pieces[piece_index] {
                    continue;
                }

                for square in piece_squares!($state, piece_index) {
                    units += zone[
                        piece_index * board_size + *square as usize
                    ] as i64;
                }

                if drops {
                    units += hand[piece_index] as i64
                        * best[piece_index] as i64;
                }
            }
        }

        let full = (ZONE_ATTACK_UNIT * ZONE_ATTACK_FULL) as i64;

        (units * units * statics.king_danger_scale as i64 / (full * full))
            .min(statics.king_danger_cap as i64) as i32
    }};
}

/// open_shield!
///
/// What it costs a royal to have nothing of its own standing anywhere ahead
/// of it, on its own file or either neighbouring one. Shelter prices the
/// pieces immediately in front of a royal and guard the ones beside it;
/// neither can say that the ground ahead is empty all the way out, which is
/// the file an enemy rook or lance arrives on. Only shield-like pieces
/// count as cover, the same pieces shelter reads, since a piece that can
/// walk back the way it came is not holding a file. The caller subtracts
/// this from the side standing on it.
///
/// Params:
/// - state: &State -> position whose royal cover is read
/// - color: usize  -> colour whose uncovered royals are charged
///
/// Return:
/// i32             -> penalty charged against that colour, non-negative
#[macro_export]
macro_rules! open_shield {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let files = statics.files as i32;
        let forward = statics.forward_steps[$color];
        let mut penalty = 0;

        for royal_square in &$state.royal_list[$color] {
            let royal_file = *royal_square as i32 % files;
            let royal_rank = *royal_square as i32 / files;
            let mut covered = false;

            for (piece_index, piece) in statics.pieces.iter().enumerate() {
                if !statics.shield_pieces[piece_index]
                    || p_color!(piece) as usize != $color {
                    continue;
                }

                for square in piece_squares!($state, piece_index) {
                    let file = *square as i32 % files;
                    let rank = *square as i32 / files;

                    covered |= (file - royal_file).abs() <= 1
                        && (rank - royal_rank) * forward > 0;
                }
            }

            penalty += statics.open_shield_penalty * !covered as i32;
        }

        penalty
    }};
}

/// castling_bonus!
///
/// One colour's standing in the castling its variant offers: having castled
/// is worth the full derived value, still holding a right is worth the part
/// of it not yet taken, and having spent both rights without castling is
/// worth nothing. Ordered that way, the score prefers castling to sitting on
/// the right, and prefers sitting on it to losing it for nothing.
///
/// A variant whose rules never castle scores zero here, and one whose royal
/// has already castled keeps the value after the rights it spent are gone.
///
/// Params:
/// - state: &State -> position whose castling standing is read
/// - color: usize  -> colour whose standing is scored
///
/// Return:
/// i32             -> castling worth, always non-negative
#[macro_export]
macro_rules! castling_bonus {
    ($state:expr, $color:expr) => {{
        let rights = [
            WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE
        ][$color];

        if !castling!($state) {
            0
        } else if $state.has_castled[$color] {
            $state.statics.castled_value
        } else if $state.castling_state & rights != 0 {
            $state.statics.castling_right_value
        } else {
            0
        }
    }};
}

/// opening_score!
///
/// White-minus-black opening score: cached material and piece-square totals
/// plus every royal-safety term. Read by the opening and setup phases whole
/// and by the middlegame as one end of its blend, and never read by the
/// endgame, which is what keeps an emptied board from paying for a safety
/// family it does not price.
///
/// Params:
/// - state: &State -> position to evaluate
///
/// Return:
/// i32             -> opening score, white minus black
#[macro_export]
macro_rules! opening_score {
    ($state:expr) => {{
        let white = WHITE as usize;
        let black = BLACK as usize;

        $state.opening_material[white] as i32
            - $state.opening_material[black] as i32
            + $state.opening_pst_bonus[white]
            - $state.opening_pst_bonus[black]
            + royal_shelter!($state, white)
            - royal_shelter!($state, black)
            + royal_guard!($state, white)
            - royal_guard!($state, black)
            + castling_bonus!($state, white)
            - castling_bonus!($state, black)
            + king_danger!($state, black)
            - king_danger!($state, white)
            + open_shield!($state, black)
            - open_shield!($state, white)
    }};
}

/// endgame_score!
///
/// White-minus-black endgame score: cached material and piece-square totals
/// alone, no safety term among them. A royal on an empty board wants to walk
/// toward the fight rather than hide behind its own army, which the endgame
/// piece-square tables already say.
///
/// Params:
/// - state: &State -> position to evaluate
///
/// Return:
/// i32             -> endgame score, white minus black
#[macro_export]
macro_rules! endgame_score {
    ($state:expr) => {{
        let white = WHITE as usize;
        let black = BLACK as usize;

        $state.endgame_material[white] as i32
            - $state.endgame_material[black] as i32
            + $state.endgame_pst_bonus[white]
            - $state.endgame_pst_bonus[black]
    }};
}

/// evaluate_position!
///
/// Evaluates current position from side-to-move perspective using cached
/// material and piece-square-table totals plus the safety each side's royals
/// stand in: the shelter ahead of them, the guard around them, what each side
/// holds of its variant's castling, the enemy pressure bearing on their zone,
/// and whether anything covers the ground in front of them at all. Opening and
/// setup use opening values, endgame uses endgame values, and middlegame
/// linearly blends both. Every safety term is carried by the opening half
/// alone, so they fade out as the board empties and are gone by the endgame,
/// where a royal wants to walk rather than hide.
///
/// Params:
/// - state: &State -> position to evaluate
///
/// Return:
/// i32             -> score from side-to-move perspective
#[macro_export]
macro_rules! evaluate_position {
    ($state:expr) => {
        hotpath::measure_block!("eval::position", {
            let side_sign = -2 * $state.playing as i32 + 1;

            let score = match $state.game_phase {
                OPENING | SETUP => opening_score!($state),
                ENDGAME => endgame_score!($state),
                MIDDLEGAME => {
                    let opening = opening_score!($state);
                    let endgame = endgame_score!($state);

                    let opening_bound = $state.statics.opening_score as i32;
                    let endgame_bound = $state.statics.endgame_score as i32;
                    let current = $state.phase_score as i32;

                    (
                        opening * (current - endgame_bound)
                            + endgame * (opening_bound - current)
                    ) / (opening_bound - endgame_bound)
                }
                _ => panic!("Invalid game phase {}", $state.game_phase),
            };

            score * side_sign
        })
    };
}
