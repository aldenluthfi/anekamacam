//! evaluation.rs
//!
//! Static position evaluation for the alpha-beta search.
//!
//! Derivation calculates the material values and piece-square tables from
//! the rules at startup. Make and undo update them. This file adds the
//! royal safety and pawn structure terms and blends the opening and endgame
//! totals by the phase. It also scores terminal positions and draws.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

/*----------------------------------------------------------------------------*\
                           TERMINAL AND DRAW SCORING
\*----------------------------------------------------------------------------*/

/// draw_score!
///
/// Gives the value of a draw for the side to move. A side with more
/// material loses by a draw, so the draw is below zero for it. The lead
/// uses the material of the current phase, clamped to `draw_span`:
///
/// ```text
/// lead        -span ─────────── 0 ─────────── +span
/// draw worth  +contempt         0        -contempt
/// ```
///
/// Params:
/// - state: &State -> position to score
///
/// Return:
/// i32             -> draw value for the side to move
///
/// Notes:
/// `draw_span` and `draw_contempt` are parts of the mean piece value of the
/// variant. Equal material gives zero. The two sides get opposite values.
///
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

        let span = $state.statics.eval.draw_span;

        -lead.clamp(-span, span) * $state.statics.eval.draw_contempt / span
    }};
}

/// terminal_score!
///
/// Gives the value of a finished position for the side to move. It reads
/// `termination.game_result`, so all game end rules give the same scale.
/// The ply distance makes a faster win and a slower loss better:
///
/// - win at ply 0  : `INF`
/// - win at ply 6  : `INF - 6`
/// - loss at ply 0 : `-INF`
/// - loss at ply 6 : `-INF + 6`
/// - draw          : [`draw_score!`]
///
/// Params:
/// - state: &State -> position to score
///
/// Return:
/// i32             -> terminal score for the side to move
///
/// Notes:
/// `ONGOING` also gives the draw score. The callers use it only after a
/// terminal test.
///
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

/*----------------------------------------------------------------------------*\
                               ROYAL SAFETY TERMS
\*----------------------------------------------------------------------------*/

/// guarded_squares!
///
/// Gives the squares of the pieces of one colour whose capture ends the
/// game: the royals, then the royal stand-in of an `extinct` rule. The
/// royal safety terms below read each of them.
///
/// Params:
///
///     state: &State
///     position to score
///
///     color: usize
///     colour of the pieces
///
/// Return:
///
///     impl Iterator<Item = Square>
///     the royal squares, then the squares of the stand-in
///
#[macro_export]
macro_rules! guarded_squares {
    ($state:expr, $color:expr) => {
        $state.royal_list[$color].iter().copied().chain(
            $state.termination.extinct.iter()
                .filter_map(|rule| rule.lone[$color])
                .flat_map(|index| piece_squares!($state, index).copied())
        )
    };
}

/// royal_shelter!
///
/// Gives the value of the own shield pieces in front of the royals of one
/// colour:
///
/// ```text
/// ┌───┬───┬───┐
/// │ ● │ ● │ ● │   the squares to read, forward is up for White
/// ├───┼───┼───┤
/// │   │ K │   │
/// ├───┼───┼───┤
/// │   │   │   │
/// └───┴───┴───┘
/// ```
///
/// Precomputation makes the square list for each royal square and colour,
/// so a royal near an edge reads fewer squares. Derivation finds the shield
/// pieces from the rules. A piece that can move back is not a shield. The
/// count for each royal stops at `SHELTER_CAP`.
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the royals
///
/// Return:
/// i32             -> shelter value, 0 or more
///
/// Notes:
/// A royal in a palace has an empty list and scores zero. Each royal on
/// the board adds its own value.
///
#[macro_export]
macro_rules! royal_shelter {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let stride = statics.eval.local_stride;
        let cap = SHELTER_CAP as i32;
        let mut worth = 0;

        for royal_square in guarded_squares!($state, $color) {
            let royal = royal_square as usize;
            let mut shelter = 0;

            for slot in 0..statics.eval.shelter_counts[$color][royal] as usize {
                let square = statics.eval.shelter_squares[$color]
                    [royal * stride + slot] as usize;
                let piece = $state.main_board[square];

                if piece != NO_PIECE
                    && statics.eval.shield_pieces[piece as usize]
                    && p_color!(&statics.pieces[piece as usize]) as usize
                        == $color {
                    shelter += 1;
                }
            }

            worth += statics.eval.shelter_value * shelter.min(cap);
        }

        worth
    }};
}

/// royal_guard!
///
/// Gives the value of all own pieces on the ring around the royals of one
/// colour. The piece type and the side of the royal are not important:
///
/// ```text
/// ┌───┬───┬───┐
/// │ ● │ ● │ ● │
/// ├───┼───┼───┤
/// │ ● │ K │ ● │   the squares to read, same for the two colours
/// ├───┼───┼───┤
/// │ ● │ ● │ ● │
/// └───┴───┴───┘
/// ```
///
/// Each piece blocks one line to the royal. The value is less than
/// [`royal_shelter!`]. The ring limits the count, so there is no cap. The
/// macro reads the own bitboard of the colour.
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the royals
///
/// Return:
/// i32             -> guard value, 0 or more
///
#[macro_export]
macro_rules! royal_guard {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let stride = statics.eval.local_stride;
        let mut guards = 0;

        for royal_square in guarded_squares!($state, $color) {
            let royal = royal_square as usize;

            for slot in 0..statics.eval.ring_counts[royal] as usize {
                let square = statics.eval.ring_squares[royal * stride + slot];

                guards += get!(
                    $state.pieces_board[$color], square as u32
                ) as i32;
            }
        }

        statics.eval.guard_value * guards
    }};
}

/// king_danger!
///
/// Gives the cost of the enemy pressure on the royals of one colour. Each
/// enemy piece that is not royal and not a shield reads its `zone_attack`
/// value from its square. In a drop variant, each piece in the hand reads
/// `zone_attack_best`, the pressure from its best drop square. With free
/// drops (`free_drops!`), the hand adds only its largest such value: a
/// side drops one piece in a move, and a hand of queens and rooks summed
/// over best squares grows past any use. With drop rules the hand is
/// weak, and the sum stays.
///
/// The cost is the square of the total pressure, so attackers compound:
///
/// - pressure : 1, 2, 3, 4
/// - cost     : 1, 4, 9, 16, before the cap
///
/// The cap is near the value of the most valuable piece. The caller
/// subtracts the cost from the colour.
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the royals under pressure
///
/// Return:
/// i32             -> danger cost for that colour, 0 or more
///
/// Notes:
/// The macro does not test drop legality for pieces in the hand. A value
/// that is too high is safer than a value that is too low.
///
#[macro_export]
macro_rules! king_danger {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let board_size = statics.board_size;
        let piece_count = statics.pieces.len();
        let hand = &$state.piece_in_hand[$color ^ 1];
        let drops = drops!($state);
        let mut units = 0i64;

        for royal_square in guarded_squares!($state, $color) {
            let royal = royal_square as usize;
            let zone = &statics.eval.zone_attack[
                royal * piece_count * board_size
                    ..(royal + 1) * piece_count * board_size
            ];
            let best = &statics.eval.zone_attack_best[
                royal * piece_count..(royal + 1) * piece_count
            ];

            let mut hand_best = 0i64;

            for (piece_index, piece) in statics.pieces.iter().enumerate() {
                if p_color!(piece) as usize == $color
                    || p_is_royal!(piece)
                    || statics.eval.shield_pieces[piece_index] {
                    continue;
                }

                for square in piece_squares!($state, piece_index) {
                    units += zone[
                        piece_index * board_size + *square as usize
                    ] as i64;
                }

                if drops && hand[piece_index] > 0 {
                    let pressure = best[piece_index] as i64;

                    match free_drops!($state) {
                        true => hand_best = hand_best.max(pressure),
                        false => units += hand[piece_index] as i64 * pressure,
                    }
                }
            }

            units += hand_best;
        }

        let full = (ZONE_ATTACK_UNIT * ZONE_ATTACK_FULL) as i64;

        (units * units * statics.eval.king_danger_scale as i64 / (full * full))
            .min(statics.eval.king_danger_cap as i64) as i32
    }};
}

/// royal_proximity!
///
/// Gives the cost of the enemy pieces near the royals of one colour. Each
/// enemy piece that is not royal and stands within two files and two
/// ranks of a royal costs `proximity_value`. Derivation gives a value only
/// in a variant with drops, so the others skip the count.
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the royals
///
/// Return:
/// i32             -> proximity cost for that colour, 0 or more
///
#[macro_export]
macro_rules! royal_proximity {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let files = statics.files as i32;
        let mut near = 0;

        if statics.eval.proximity_value != 0 {
            for royal_square in guarded_squares!($state, $color) {
                let royal_file = royal_square as i32 % files;
                let royal_rank = royal_square as i32 / files;

                for (piece_index, piece) in statics.pieces.iter().enumerate() {
                    if p_color!(piece) as usize == $color
                        || p_is_royal!(piece) {
                        continue;
                    }

                    for square in piece_squares!($state, piece_index) {
                        let file = *square as i32 % files;
                        let rank = *square as i32 / files;

                        near += ((file - royal_file).abs() <= 2
                            && (rank - royal_rank).abs() <= 2) as i32;
                    }
                }
            }
        }

        near * statics.eval.proximity_value
    }};
}

/// open_shield!
///
/// Gives the penalty for a royal with no own shield piece in front of it,
/// on its file or on the two files next to it:
///
/// ```text
/// ┌───┬───┬───┐
/// │   │   │   │   all ranks in front of the royal, to the far edge
/// │   │   │   │
/// ├───┼───┼───┤
/// │   │ K │   │   the file of the royal and the two files next to it
/// └───┴───┴───┘
/// ```
///
/// Shelter and guard read only the squares near the royal. This term finds
/// open files, where an enemy rook or lance can attack. Only shield pieces
/// give cover, as in [`royal_shelter!`]. The penalty is flat for each royal
/// without cover. The caller subtracts it from the colour.
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the royals
///
/// Return:
/// i32             -> penalty for that colour, 0 or more
///
#[macro_export]
macro_rules! open_shield {
    ($state:expr, $color:expr) => {{
        let statics = &$state.statics;
        let files = statics.files as i32;
        let forward = statics.eval.forward_steps[$color];
        let mut penalty = 0;

        for royal_square in guarded_squares!($state, $color) {
            let royal_file = royal_square as i32 % files;
            let royal_rank = royal_square as i32 / files;
            let mut covered = false;

            for (piece_index, piece) in statics.pieces.iter().enumerate() {
                if !statics.eval.shield_pieces[piece_index]
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

            penalty += statics.eval.open_shield_penalty * !covered as i32;
        }

        penalty
    }};
}

/// check_race!
///
/// Gives the worth of the checks one colour has given, in a variant where
/// a count of checks wins. The worth halves for each check that is still
/// missing past the first:
///
/// - 1 check left  : the `value` of the rule, the next check wins
/// - 2 checks left : half of it
/// - 3 checks left : a quarter of it
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour that gives the checks
///
/// Return:
/// i32             -> race worth for that colour, 0 or more
///
#[macro_export]
macro_rules! check_race {
    ($state:expr, $color:expr) => {{
        match $state.termination.checks.as_ref() {
            Some(checks) if checks.outcome == Outcome::Win => {
                let left = checks.count
                    .saturating_sub(checks.delivered[$color])
                    .max(1);

                checks.value >> (left - 1).min(31)
            }
            _ => 0,
        }
    }};
}

/// goal_race!
///
/// Gives the worth of the goal piece of one colour that is nearest to the
/// goal zone. The distance is the count of moves of that piece to the
/// zone, from the `steps` table of the rule. Within `GOAL_HOLD_STEPS`, a
/// walk along the `closer` squares adds one move for each step where all
/// the squares are attacked by the enemy or hold an own piece: a move must
/// first clear the way. The worth halves for each move past the first:
///
/// - 1 move  : the `value` of the rule, it arrives next move if not stopped
/// - 2 moves : half of it
/// - 3 moves : a quarter of it
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the goal pieces
///
/// Return:
/// i32             -> race worth for that colour, 0 or more
///
#[macro_export]
macro_rules! goal_race {
    ($state:expr, $color:expr) => {{
        match $state.termination.goal.as_ref() {
            Some(goal) => {
                let board_size = $state.statics.board_size;
                let mut nearest = u8::MAX;

                for (piece_index, piece) in
                    $state.statics.pieces.iter().enumerate()
                {
                    if !goal.set[piece_index]
                        || p_color!(piece) as usize != $color {
                        continue;
                    }

                    let row = piece_index * board_size;

                    for square in piece_squares!($state, piece_index) {
                        let steps = goal.steps[row + *square as usize];

                        if steps == 0 || steps > GOAL_HOLD_STEPS {
                            nearest = nearest.min(steps);
                            continue;
                        }

                        let mut frontier = vec![*square];
                        let mut held = 0u8;

                        for _ in 0..steps {
                            let mut next: Vec<Square> = Vec::new();

                            for from in &frontier {
                                for to in &goal.closer[row + *from as usize] {
                                    if !next.contains(to) {
                                        next.push(*to);
                                    }
                                }
                            }

                            let free: Vec<Square> = next.iter().copied()
                                .filter(|&to| {
                                    let holder = $state.main_board[
                                        to as usize
                                    ];

                                    (holder == NO_PIECE
                                        || p_color!(
                                            &$state.statics.pieces[
                                                holder as usize
                                            ]
                                        ) as usize != $color)
                                    && !is_square_attacked!(
                                        to as u32,
                                        $color,
                                        false,
                                        p_is_royal!(piece),
                                        p_rank!(piece),
                                        $state
                                    )
                                })
                                .collect();

                            held += free.is_empty() as u8;
                            frontier = match free.is_empty() {
                                true => next,
                                false => free,
                            };
                        }

                        nearest = nearest.min(steps + held);
                    }
                }

                match nearest {
                    0 | u8::MAX => 0,
                    steps => goal.value >> (steps - 1).min(31),
                }
            }
            None => 0,
        }
    }};
}

/// extinct_threat!
///
/// Gives the cost of the attacked set pieces of one colour under the
/// losing `extinct` rules. A capture of the last copies above the
/// threshold loses the game, so each attacked copy costs the `threat` of
/// the rule over `left` squared:
///
/// - 1 copy left  : `threat` for each attacked copy
/// - 2 copies left : a quarter of it
/// - more         : skipped, `EXTINCT_THREAT_LEFT` is the limit
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the set pieces
///
/// Return:
/// i32             -> threat cost for that colour, 0 or more
///
#[macro_export]
macro_rules! extinct_threat {
    ($state:expr, $color:expr) => {{
        let mut cost = 0;

        for rule in &$state.termination.extinct {
            if rule.threat == 0 {
                continue;
            }

            let members = || $state.statics.pieces.iter()
                .enumerate()
                .filter(|(index, piece)| {
                    rule.set[*index] && p_color!(piece) as usize == $color
                });
            let count = members()
                .map(|(index, _)| $state.piece_count[index])
                .sum::<u32>();
            let left = count.saturating_sub(rule.threshold as u32);

            if left == 0 || left > EXTINCT_THREAT_LEFT {
                continue;
            }

            let mut attacked = 0;

            for (index, piece) in members() {
                for square in piece_squares!($state, index) {
                    attacked += is_square_attacked!(
                        *square as u32,
                        $color,
                        get!($state.virgin_board, *square as u32),
                        p_is_royal!(piece),
                        p_rank!(piece),
                        $state
                    ) as i32;
                }
            }

            cost += rule.threat * attacked / (left * left) as i32;
        }

        cost
    }};
}

/// castling_bonus!
///
/// Gives the castling value of one colour. Thus castling is better than a
/// kept right, and a kept right is better than a lost right:
///
/// - castled             : `castled_value`
/// - has a right         : `castling_right_value`
/// - no right, no castle : 0
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour to score
///
/// Return:
/// i32             -> castling value, 0 or more
///
/// Notes:
/// A variant without castling gives 0. The castled mark in
/// `castling_state` stays after the rights are gone.
///
#[macro_export]
macro_rules! castling_bonus {
    ($state:expr, $color:expr) => {{
        let rights = [
            WK_CASTLE | WQ_CASTLE, BK_CASTLE | BQ_CASTLE
        ][$color];
        let castled = $state.castling_state & (CASTLED << $color) != 0;
        let holds = $state.castling_state & rights != 0;

        castling!($state) as i32 * [
            $state.statics.eval.castling_right_value * holds as i32,
            $state.statics.eval.castled_value,
        ][castled as usize]
    }};
}

/*----------------------------------------------------------------------------*\
                                 PAWN STRUCTURE
\*----------------------------------------------------------------------------*/

/// pawn_structure!
///
/// Gives the pawn structure value, White minus Black, for the opening and
/// the endgame. A passed pawn is worth more in the endgame. Each term is a
/// bit test against a precomputed mask:
///
/// - passed           : no enemy pawn can block or capture on its path
/// - protected passed : a passed pawn with a pawn defender, 1.5 times
/// - connected passed : a passed pawn with a passed defender, 2 times
/// - connected        : another own pawn defends it
/// - doubled          : an own pawn is on its path
/// - isolated         : no own pawn on a file that can defend it
/// - backward         : a neighbour but no defender, stop square attacked
///
/// The passed value is the promotion gain, scaled by the progress of the
/// pawn. Derivation makes each mask from the moves of the pawn, so the same
/// tests work for all pawn types:
///
/// - pawn_path         : squares that the pawn walks to promote
/// - pawn_interference : squares where an enemy pawn stops the walk
/// - pawn_support      : squares where an own pawn defends it
/// - pawn_backward     : squares where an enemy pawn attacks the stop square
///
/// The diagrams show one pawn in the middle of the board:
///
/// - PP : the pawn to score
/// - pp : path, an own pawn here is doubled
/// - ii : interference outside the path, an enemy pawn here stops it
/// - ss : support, an own pawn here connects it
///
/// Each path square is also interference. The backward mask is the part of
/// interference that attacks the stop square. The diagrams do not show
/// these two again.
///
/// A FIDE pawn steps straight and captures diagonally. Its path is its own
/// file. Its supporters are diagonally behind it and next to it:
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
/// A shogi pawn steps and captures straight ahead. All masks are on its own
/// file, and only the pawn directly behind it can defend it:
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
/// A Berolina pawn steps diagonally and captures straight. Its path goes to
/// more files. Its supporters are straight behind it and next to it:
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
/// The support mask has two types of defender:
///
/// - behind : a pawn that captures onto this pawn
/// - beside : a pawn that captures onto the stop square
///
/// A pawn has "beside" defenders only if it captures in a direction that
/// it does not step in. Thus the shogi pawn has none.
///
/// Params:
/// - state: &mut State -> position with the pawns
///
/// Return:
/// (i32, i32)          -> opening and endgame value, White minus Black
///
/// Notes:
/// The macro reads one pawn list from the piece lists, so the cost is the
/// square of the pawn count. A variant without pawns returns at once. The
/// pawn cache in [`Scratch`] keeps results under the pawn key, and most
/// moves do not move a pawn. The macro borrows `scratch` and `statics` as
/// separate fields.
///
#[macro_export]
macro_rules! pawn_structure {
    ($state:expr) => {
        hotpath::measure_block!("eval::pawn_structure", {
            let statics = &$state.statics;
            let stride = statics.eval.pawn_stride;
            let files = statics.files as i32;

            if stride == 0 {
                (0, 0)
            } else {
                let key = $state.pawn_hash;

                let cached = {
                    let table = &$state.scratch.pawn_table;
                    let entry =
                        &table.table[key as usize & (table.len() - 1)];

                    match entry.key == key {
                        true => Some((entry.opening, entry.endgame)),
                        false => None,
                    }
                };

                if let Some(scores) = cached {
                    scores
                } else {
                    let pawns = &mut $state.scratch.pawn_rosters;

                    pawns[WHITE as usize].clear();
                    pawns[BLACK as usize].clear();

                    for &index in &statics.eval.pawn_pieces {
                        let slot = statics.eval.pawn_slots[index];
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
                            let mask = &statics.eval.pawn_interference[
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
                            let support = &statics.eval.pawn_support[index];
                            let path = &statics.eval.pawn_path[index];
                            let stop = &statics.eval.pawn_backward[index];
                            let neighbours =
                                &statics.eval.pawn_support_files[slot];

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
                                statics.eval.pawn_passed_opening[index]
                                    * passed as i32;
                            let passer_endgame =
                                statics.eval.pawn_passed_endgame[index]
                                    * passed as i32;
                            let bonus =
                                2 + connected as i32 + chained as i32;

                            opening += sign * (
                                passer_opening * bonus / 2
                                    + statics.eval.pawn_connected_opening[slot]
                                        * connected as i32
                                    - statics.eval.pawn_doubled_penalty[slot]
                                        * doubled as i32
                                    - statics.eval.pawn_isolated_penalty[slot]
                                        * !neighboured as i32
                                    - statics.eval.pawn_backward_penalty[slot]
                                        * contested as i32
                            );
                            endgame += sign * (
                                passer_endgame * bonus / 2
                                    + statics.eval.pawn_connected_endgame[slot]
                                        * connected as i32
                                    - statics.eval.pawn_doubled_penalty[slot]
                                        * doubled as i32
                                    - statics.eval.pawn_isolated_penalty[slot]
                                        * !neighboured as i32
                                    - statics.eval.pawn_backward_penalty[slot]
                                        * contested as i32
                            );
                        }
                    }

                    let table = &mut $state.scratch.pawn_table;
                    let index = key as usize & (table.len() - 1);
                    let entry = &mut table.table[index];

                    entry.key = key;
                    entry.opening = opening;
                    entry.endgame = endgame;

                    (opening, endgame)
                }
            }
        })
    };
}

/*----------------------------------------------------------------------------*\
                             PHASE SCORE COMPONENTS
\*----------------------------------------------------------------------------*/

/// opening_score!
///
/// Gives the opening score, White minus Black. It is the material and
/// piece-square totals plus the royal safety terms that keep a royal at
/// home: shelter, guard, castling and open files. The opening, the setup
/// and the middlegame blend use it, each with [`shared_score!`] added.
/// The endgame does not.
///
/// Params:
/// - state: &State -> position to evaluate
///
/// Return:
/// i32             -> opening score, White minus Black
///
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
            + open_shield!($state, black)
            - open_shield!($state, white)
    }};
}

/// shared_score!
///
/// Gives the terms that the opening and the endgame score share, White
/// minus Black: the enemy pressure and nearness to the royals, the check
/// race, the goal race and the extinction threats. They do not depend on
/// the phase, so the evaluation calculates them once and adds the same
/// value to the two halves before the blend.
///
/// Params:
/// - state: &State -> position to evaluate
///
/// Return:
/// i32             -> shared score, White minus Black
///
#[macro_export]
macro_rules! shared_score {
    ($state:expr) => {{
        let white = WHITE as usize;
        let black = BLACK as usize;

        king_danger!($state, black)
            - king_danger!($state, white)
            + royal_proximity!($state, black)
            - royal_proximity!($state, white)
            + check_race!($state, white)
            - check_race!($state, black)
            + goal_race!($state, white)
            - goal_race!($state, black)
            + extinct_threat!($state, black)
            - extinct_threat!($state, white)
    }};
}

/// endgame_score!
///
/// Gives the endgame score, White minus Black. It is the material and
/// piece-square totals; [`shared_score!`] adds the pressure, nearness and
/// race terms. In the endgame, the royal must go to the center, and the
/// endgame tables already give this. Shelter, guard, open files and
/// castling keep a royal at home, so they stay out. The pressure falls by
/// itself when the attackers leave.
///
/// Params:
/// - state: &State -> position to evaluate
///
/// Return:
/// i32             -> endgame score, White minus Black
///
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
/// Gives the material imbalance value, White minus Black:
///
/// - major : difference in the count of major pieces
/// - minor : difference in the count of minor pieces
/// - pair  : two pieces of a type that can reach only half the board
///
/// A pair is good because the second piece covers the other half. The
/// value is the same in all phases, so it is added outside the blend.
///
/// Params:
/// - state: &State -> position with the material counts
///
/// Return:
/// i32             -> imbalance value, White minus Black
///
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

        for index in &statics.eval.pair_pieces {
            let color = p_color!(&statics.pieces[*index]) as i32;

            pairs += (-2 * color + 1)
                * ($state.piece_count[*index] >= 2) as i32;
        }

        major * statics.eval.imbalance_major
            + minor * statics.eval.imbalance_minor
            + pairs * statics.eval.pair_bonus
    }};
}

/*----------------------------------------------------------------------------*\
                              POSITION EVALUATION
\*----------------------------------------------------------------------------*/

/// evaluate_position!
///
/// Gives the static evaluation of a position for the side to move. A
/// position in the score cache takes its stored score. Else the macro
/// calculates it with [`position_score!`] and stores it. A position often
/// comes again: through a transposition, in the next iteration and in a
/// search again of a subtree. The quiescence search keeps no score.
///
/// Params:
/// - state: &mut State -> position to evaluate
///
/// Return:
/// i32                 -> score for the side to move
///
/// Notes:
/// The key is `eval_key`, all the inputs of the evaluation. A stored score
/// is equal to a new one, so the search makes the same tree with the cache
/// and without it.
///
#[macro_export]
macro_rules! evaluate_position {
    ($state:expr) => {
        hotpath::measure_block!("eval::position", {
            let key = eval_key(&$state);
            let slot = key as usize & ($state.scratch.eval_table.len() - 1);
            let check = (key >> 64) as u64;
            let (stored, score) = $state.scratch.eval_table[slot];

            match stored == check {
                true => score,
                false => {
                    let score = position_score!($state);

                    $state.scratch.eval_table[slot] = (check, score);
                    score
                }
            }
        })
    };
}

/// position_score!
///
/// Calculates the static evaluation of a position for the side to move.
/// The phase selects the parts:
///
/// - OPENING, SETUP : opening score and opening pawn value
/// - MIDDLEGAME     : blend of the two, from the material on the board
/// - ENDGAME        : endgame score and endgame pawn value
///
/// The middlegame blend is linear in `phase_score`, between the two
/// bounds that derivation gives for the variant:
///
/// ```text
/// opening_bound ├──────────── current ────────────┤ endgame_bound
///   full board        weight of the opening          bare board
///                     half falls to the right
/// ```
///
/// Where each term goes:
///
/// - material, piece-square : both halves, each with its own values
/// - royal safety           : opening half; enemy pressure in both
/// - shared terms           : calculated once, added to both halves
/// - pawn structure         : both halves, one value for each
/// - material imbalance     : outside the blend, added once
/// - tempo                  : outside, after the flip to the side to move
///
/// Params:
/// - state: &mut State -> position to evaluate
///
/// Return:
/// i32                 -> score for the side to move
///
/// Notes:
/// The pawn structure is calculated once for each node. A phase outside
/// the four causes a panic.
///
#[macro_export]
macro_rules! position_score {
    ($state:expr) => {{
        let side_sign = -2 * $state.playing as i32 + 1;

        let shared = shared_score!($state);

        let score = match $state.game_phase {
            OPENING | SETUP => {
                opening_score!($state)
                    + shared
                    + pawn_structure!($state).0
            }
            ENDGAME => {
                endgame_score!($state)
                    + shared
                    + pawn_structure!($state).1
            }
            MIDDLEGAME => {
                let (pawn_opening, pawn_endgame) =
                    pawn_structure!($state);
                let opening =
                    opening_score!($state) + shared + pawn_opening;
                let endgame =
                    endgame_score!($state) + shared + pawn_endgame;

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
            + $state.statics.eval.tempo_bonus
    }};
}
