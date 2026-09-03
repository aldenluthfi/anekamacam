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

/// pawn_structure!
///
/// White-minus-black worth of how each side's pawns stand relative to one
/// another, returned as an opening and an endgame figure at once because a
/// passer is worth little while the board is full and a great deal once it
/// is empty. Seven statements are made about every pawn, each of them a bit
/// read against a precomputed mask:
///
/// - passed, when no enemy pawn stands on any square that could block or
///   capture its way forward, worth what promoting it would gain scaled by
///   how far along it already is
/// - protected passed, a passer another pawn defends, worth half again
/// - connected passed, a passer defended by a passer, worth half again once
///   more, since neither can be stopped by taking the other
/// - connected, a pawn another pawn defends
/// - doubled, a pawn standing on its own advance path and blocking it
/// - isolated, a pawn with no friendly pawn on any file that could ever
///   defend it or the square it steps to
/// - backward, a pawn that has a neighbour but no defender, whose stop
///   square an enemy pawn watches
///
/// Both sweeps read one roster gathered from the piece lists, so the cost is
/// the pawns on the board squared and not the width of the board. A variant
/// whose rules field no pawn returns at once.
///
/// The verdict depends on nothing but where the pawns stand, and most moves
/// a search makes move no pawn, so the pawn lists are folded into a key and
/// the answer is read back from this thread's [`PTable`] whenever that
/// arrangement has been seen before. Only a miss pays for the roster. Folding
/// the key from the piece lists rather than maintaining it across make and
/// undo costs one exclusive or per pawn and makes it impossible for the key
/// to disagree with the board it is supposed to describe.
///
/// Params:
/// - state: &State -> position whose pawns are read
///
/// Return:
/// (i32, i32)      -> opening and endgame worth, white minus black
#[macro_export]
macro_rules! pawn_structure {
    ($state:expr) => {
        hotpath::measure_block!("eval::pawn_structure", {
            let statics = &$state.statics;
            let stride = statics.pawn_stride;
            let files = statics.files as i32;

            if stride == 0 {
                (0, 0)
            } else {
                let mut key = 0u128;

                for &index in &statics.pawn_pieces {
                    for square in piece_squares!($state, index) {
                        key ^= PIECE_HASHES[index][*square as usize];
                    }
                }

                let cached = PAWN_TABLE.with(|table| {
                    let table = table.borrow();
                    let entry =
                        &table.table[key as usize & (table.len() - 1)];

                    match entry.key == key {
                        true => Some((entry.opening, entry.endgame)),
                        false => None,
                    }
                });

                PAWN_BUFFERS.with(|buffers| {
                    if let Some(scores) = cached {
                        return scores;
                    }

                    let mut pawns = buffers.borrow_mut();

                    pawns[WHITE as usize].clear();
                    pawns[BLACK as usize].clear();

                    for &index in &statics.pawn_pieces {
                        let slot = statics.pawn_slots[index];
                        let color =
                            p_color!(&statics.pieces[index]) as usize;

                        for square in piece_squares!($state, index) {
                            pawns[color].push((
                                slot, *square, *square as i32 % files, false
                            ));
                        }
                    }

                    for color in [WHITE as usize, BLACK as usize] {
                        for entry in 0..pawns[color].len() {
                            let (slot, square, ..) = pawns[color][entry];
                            let mask = &statics.pawn_interference[
                                slot * stride + square as usize
                            ];

                            let stopped = pawns[color ^ 1].iter().any(
                                |other| get!(mask, other.1 as u32)
                            );

                            pawns[color][entry].3 = !stopped;
                        }
                    }

                    let mut opening = 0;
                    let mut endgame = 0;

                    for color in [WHITE as usize, BLACK as usize] {
                        let sign = -2 * color as i32 + 1;

                        for entry in 0..pawns[color].len() {
                            let (slot, square, file, passed) =
                                pawns[color][entry];
                            let index = slot * stride + square as usize;
                            let support = &statics.pawn_support[index];
                            let path = &statics.pawn_path[index];
                            let stop = &statics.pawn_backward[index];
                            let neighbours =
                                &statics.pawn_support_files[slot];

                            let connected = pawns[color].iter().any(|other|
                                other.1 != square
                                    && get!(support, other.1 as u32)
                            );
                            let chained = connected && pawns[color].iter()
                                .any(|other| other.1 != square
                                    && other.3
                                    && get!(support, other.1 as u32)
                            );
                            let doubled = pawns[color].iter().any(|other|
                                other.1 != square
                                    && get!(path, other.1 as u32)
                            );
                            let neighboured = pawns[color].iter().any(|other|
                                other.1 != square
                                    && neighbours.contains(&(other.2 - file))
                            );
                            let contested = !connected && neighboured
                                && pawns[color ^ 1].iter().any(
                                    |other| get!(stop, other.1 as u32)
                                );

                            let passer_opening =
                                statics.pawn_passed_opening[index]
                                    * passed as i32;
                            let passer_endgame =
                                statics.pawn_passed_endgame[index]
                                    * passed as i32;
                            let bonus =
                                2 + connected as i32 + chained as i32;

                            opening += sign * (
                                passer_opening * bonus / 2
                                    + statics.pawn_connected_opening[slot]
                                        * connected as i32
                                    - statics.pawn_doubled_penalty[slot]
                                        * doubled as i32
                                    - statics.pawn_isolated_penalty[slot]
                                        * !neighboured as i32
                                    - statics.pawn_backward_penalty[slot]
                                        * contested as i32
                            );
                            endgame += sign * (
                                passer_endgame * bonus / 2
                                    + statics.pawn_connected_endgame[slot]
                                        * connected as i32
                                    - statics.pawn_doubled_penalty[slot]
                                        * doubled as i32
                                    - statics.pawn_isolated_penalty[slot]
                                        * !neighboured as i32
                                    - statics.pawn_backward_penalty[slot]
                                        * contested as i32
                            );
                        }
                    }

                    PAWN_TABLE.with(|table| {
                        let mut table = table.borrow_mut();
                        let index = key as usize & (table.len() - 1);
                        let entry = &mut table.table[index];

                        entry.key = key;
                        entry.opening = opening;
                        entry.endgame = endgame;
                    });

                    (opening, endgame)
                })
            }
        })
    };
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

/// material_advantage!
///
/// White-minus-black worth of the ways one side's material can be better
/// than the other's without being worth more. Two of them are counts: a
/// side holding more heavy or more light pieces than its opponent is
/// harder to trade back to level, whatever those pieces are individually
/// worth. The third is the pair, paid to a side holding two of a piece
/// bound to half the board, since the second copy covers exactly the half
/// the first cannot.
///
/// Read by both halves of the evaluation and so added once, outside the
/// blend: a term worth the same at either end of the taper blends to
/// itself.
///
/// Params:
/// - state: &State -> position whose material counts are read
///
/// Return:
/// i32             -> worth of the imbalance, white minus black
#[macro_export]
macro_rules! material_advantage {
    ($state:expr) => {{
        let statics = &$state.statics;
        let white = WHITE as usize;
        let black = BLACK as usize;

        let major = $state.major_pieces[white] as i32
            - $state.major_pieces[black] as i32;
        let minor = $state.minor_pieces[white] as i32
            - $state.minor_pieces[black] as i32;

        let mut pairs = 0;

        for index in &statics.pair_pieces {
            let color = p_color!(&statics.pieces[*index]) as i32;

            pairs += (-2 * color + 1)
                * ($state.piece_count[*index] >= 2) as i32;
        }

        major * statics.imbalance_major
            + minor * statics.imbalance_minor
            + pairs * statics.pair_bonus
    }};
}

/// evaluate_position!
///
/// Evaluates current position from side-to-move perspective using cached
/// material and piece-square-table totals plus the safety each side's royals
/// stand in: the shelter ahead of them, the guard around them, what each side
/// holds of its variant's castling, the enemy pressure bearing on their zone,
/// and whether anything covers the ground in front of them at all, plus how
/// each side's pawns stand relative to one another. Opening and setup use
/// opening values, endgame uses endgame values, and middlegame linearly blends
/// both. Every safety term is carried by the opening half alone, so they fade
/// out as the board empties and are gone by the endgame, where a royal wants to
/// walk rather than hide. Pawn structure is the one positional family both
/// halves price, since a passer is worth most exactly where safety is worth
/// nothing, and it is computed once per node whichever phase reads it.
/// Two things sit outside the blend entirely: the material imbalance, worth
/// the same at either end of the taper, and the tempo, added after the
/// score is turned to face the side to move, since holding the move is the
/// one advantage that belongs to whoever is about to spend it.
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
                OPENING | SETUP => {
                    opening_score!($state) + pawn_structure!($state).0
                }
                ENDGAME => {
                    endgame_score!($state) + pawn_structure!($state).1
                }
                MIDDLEGAME => {
                    let (pawn_opening, pawn_endgame) =
                        pawn_structure!($state);
                    let opening = opening_score!($state) + pawn_opening;
                    let endgame = endgame_score!($state) + pawn_endgame;

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

            (score + material_advantage!($state)) * side_sign
                + $state.statics.tempo_bonus
        })
    };
}
