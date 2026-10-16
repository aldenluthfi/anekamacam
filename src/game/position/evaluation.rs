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
/// - state: &State                 -> position to score
/// - color: usize                  -> colour of the pieces
///
/// Return:
/// impl Iterator<Item = Square>    -> royal squares, then stand-in squares
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
/// Gives the cost of the enemy pressure on the royals of one colour. The
/// cost is the square of the total pressure, so attackers compound:
///
/// - pressure : 1, 2, 3, 4
/// - cost     : 1, 4, 9, 16, before the cap
///
/// The pressure sources, each enemy piece that is not royal or a shield:
///
/// - on the board : its `zone_attack` value from its square
/// - in the hand  : its `zone_attack_best`, from its best drop square
/// - free drops   : only the largest hand value, one drop for each move
///
/// Params:
/// - state: &State -> position to score
/// - color: usize  -> colour of the royals under pressure
///
/// Return:
/// i32             -> danger cost for that colour, 0 or more
///
/// Notes:
/// The cap is near the value of the most valuable piece. A summed hand of
/// free drops grew past any use. The macro does not test drop legality,
/// as a value that is too high is safer than one too low.
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
/// Gives the worth of the goal pieces of one colour on their way to the
/// goal zone. A flood from the goal pieces of each type follows their
/// `landings`, one move for each step plus the hold of the square it
/// enters:
///
/// - `hold` : the enemy attacks on the square, if no own pawn is there
/// - 2      : an own pawn
/// - -      : an own pawn that an enemy pawn blocks, it cannot make way
///
/// Each zone square takes the earliest arrival of any goal piece. The `w`
/// zone squares first reached after `d` moves are worth:
///
/// ```text
///   value * w / (w + d * d)
/// ```
///
/// - 1 square, 1 move  : half the `value`, it arrives next move if not
///                       stopped
/// - 1 square, 2 moves : a fifth of it
/// - 2 squares, 3 moves: two elevenths of it
///
/// Params:
/// - state: &State                  -> position to score
/// - color: usize                   -> colour of the goal pieces
/// - hold : Fn(Square, usize) -> i32 -> moves the enemy attacks on a square
///                                     cost that colour: 0, 1 when attacked
///                                     once, 2 when twice or by a pawn
///
/// Return:
/// i32                              -> race worth for that colour, 0 or
///                                     more
///
/// Notes:
/// A goal piece on the zone has won, so it is not scored here. The flood
/// keeps one square set for each move count still to come, as one bit
/// for each square, and stops when it has reached each zone square or
/// nothing is left. The holds are Fairy-Stockfish's flag race: a piece
/// must wait for each attack on its way to go, and a pawn of its own must
/// move first. An enemy pawn blocks an own pawn when it stands on the
/// path next to it.
///
#[macro_export]
macro_rules! goal_race {
    ($state:expr, $color:expr, $hold:expr) => {{
        match $state.termination.goal.as_ref() {
            Some(goal) => {
                let statics = &$state.statics;
                let board_size = statics.board_size;
                let files = statics.files as i32;
                let zone_size = count_bits!(goal.zone) as usize;
                let mut arrivals: Vec<(Square, i32)> = Vec::new();              /* zone square, earliest move count   */

                let stuck = |pawn: usize, at: Square| {
                    let slot = statics.eval.pawn_slots[pawn];
                    let path = &statics.eval.pawn_path[
                        slot * statics.eval.pawn_stride + at as usize
                    ];
                    let near = |other: Square| {
                        (other as i32 % files - at as i32 % files).abs() <= 1
                            && (other as i32 / files - at as i32 / files)
                                .abs() <= 1
                    };

                    statics.eval.pawn_pieces.iter()
                        .filter(|&&enemy| {
                            p_color!(&statics.pieces[enemy]) as usize
                                != $color
                        })
                        .any(|&enemy| {
                            piece_squares!($state, enemy).any(|&other| {
                                get!(path, other as u32) && near(other)
                            })
                        })
                };

                let words = (board_size + 63) >> 6;

                for (piece_index, piece) in statics.pieces.iter().enumerate() {
                    if !goal.set[piece_index]
                        || p_color!(piece) as usize != $color {
                        continue;
                    }

                    let row = piece_index * board_size;
                    let hold = |to: Square| -> Option<usize> {
                        let holder = $state.main_board[to as usize];
                        let own_pawn = holder != NO_PIECE
                            && p_color!(&statics.pieces[holder as usize])
                                as usize == $color
                            && statics.eval.pawn_slots[holder as usize]
                                != NO_PAWN;

                        match own_pawn {
                            false => Some($hold(to, $color) as usize),
                            true if stuck(holder as usize, to) => None,
                            true => Some(2),
                        }
                    };
                    let mut waves = [                                           /* entered in 0, 1 and 2 more moves   */
                        vec![0u64; words], vec![0u64; words], vec![0u64; words]
                    ];
                    let mut current = vec![0u64; words];
                    let mut queued = vec![0u64; words];                         /* the first wave a square enters is  */
                    let mut found = 0;                                          /* its earliest, its hold never moves */

                    for &start in piece_squares!($state, piece_index) {
                        let bit = !get!(goal.zone, start as u32) as u64;

                        waves[0][start as usize >> 6] |= bit << (start & 63);
                        queued[start as usize >> 6] |= 1u64 << (start & 63);
                    }

                    for moves in 0.. {
                        if found == zone_size
                            || waves.iter().flatten().all(|word| *word == 0) {
                            break;
                        }

                        waves.rotate_left(1);
                        std::mem::swap(&mut waves[2], &mut current);            /* a clear set for 2 more moves       */

                        for word in 0..words {
                            let mut bits = current[word];

                            while bits != 0 {
                                let from = (word << 6
                                    | bits.trailing_zeros() as usize) as Square;

                                bits &= bits - 1;

                                if get!(goal.zone, from as u32) {
                                    found += 1;

                                    match arrivals.iter_mut()
                                        .find(|(at, _)| *at == from) {
                                        Some(entry) =>
                                            entry.1 = entry.1.min(moves),
                                        None => arrivals.push((from, moves)),
                                    }

                                    continue;
                                }

                                for &to in &goal.landings[row + from as usize] {
                                    let at = to as usize >> 6;
                                    let bit = 1u64 << (to & 63);

                                    if queued[at] & bit != 0 {
                                        continue;
                                    }

                                    queued[at] |= bit;

                                    if let Some(wait) = hold(to) {
                                        waves[wait][at] |= bit;
                                    }
                                }
                            }
                        }

                        current.fill(0);
                    }
                }

                arrivals.sort_unstable_by_key(|(_, moves)| *moves);

                arrivals.chunk_by(|a, b| a.1 == b.1)
                    .map(|group| {
                        let reached = group.len() as i32;
                        let moves = group[0].1;

                        goal.value * reached / (reached + moves * moves)
                    })
                    .sum::<i32>()
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
/// - 1 copy left   : `threat` for each attacked copy
/// - 2 copies left : a quarter of it
/// - more          : skipped, `EXTINCT_THREAT_LEFT` is the limit
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
                                  ATTACK TERMS
\*----------------------------------------------------------------------------*/

/// attack_terms!
///
/// Gives the threat and mobility value, White minus Black, for the opening
/// and the endgame. One pass makes the attack map of each piece and keeps
/// for each colour the squares it attacks once and twice, the squares its
/// pawns attack, and its pawn and royal squares. The pass maps the pawns
/// and the royals first, so the mobility area of each other piece is known
/// when its map is made:
///
/// - area     : no own pawn, no own royal, no square an enemy pawn attacks
/// - mobility : the squares of the area that the piece attacks, `m`; it
///              is worth `value * (m - 2) / (8 + m)` from `mobility_table`,
///              below zero under two squares and toward `value` above
///
/// Then for each colour, the threats:
///
/// - strong     : an enemy pawn attacks it, or two enemy pieces and fewer
///                than two of ours
/// - weak       : an enemy piece, not royal, that we attack and that is not
///                strong
/// - hanging    : a weak piece that no enemy piece attacks, or one that is
///                not a pawn and that two of ours attack
/// - pawn       : an enemy piece, not royal and not a pawn, that one of our
///                pawns attacks from a safe square: one the enemy does not
///                attack, or one we attack too
/// - restricted : a square both colours attack that is not strong
///
/// Each piece or square counts once, whatever the number of attackers.
///
/// Then for each colour with one royal that can be mated, the king danger
/// sum of the enemy, from the same maps. The ring is the royal square and
/// its local ring:
///
/// - attackers : enemy pieces whose map meets the ring, and the ring
///               squares enemy pawns attack; their count times the pieces
/// - weak      : ring squares the enemy attacks and we do not hold twice
/// - hits      : the enemy hits on the local ring, one for each piece
/// - checks    : an enemy piece type that reaches a check square that is
///               safe, one we do not attack or a weak one the enemy holds
///               twice; else each check square it reaches is unsafe
/// - no major  : the enemy has no major piece, less danger
///
/// A sum past the threshold costs the colour its square in the opening,
/// linear past the cap, and the sum itself in the endgame; the weights
/// and the race rise come from [`derive_advantage_parameters`].
///
/// Last, the goal race of [`goal_race!`] in both phases: a square the
/// enemy attacks holds a goal piece back one move, two when it attacks
/// twice or with a pawn.
///
/// Params:
/// - state: &mut State -> position to score, with the threat scratch
///
/// Return:
/// (i32, i32)          -> opening and endgame value, White minus Black
///
/// Notes:
/// The maps are those of `piece_attacks!`: a move condition is not tested,
/// so a map can hold more targets than the captures. A pawn is a piece
/// with a pawn slot. A piece in the hand attacks nothing. The pass keeps
/// the square and the map of each pawn for the pawn kind, as a safe square
/// is known only after the pass, and the union of the maps of each piece
/// type for the checks. The check squares are the map of the type of the
/// other colour cast from the royal, exact for leaps, lines and hops. A
/// pawn checks only from the hand, and a held piece drops on any empty
/// square. The mobility formula and the danger sum are Fairy-Stockfish's
/// for its fairy pieces.
///
#[macro_export]
macro_rules! attack_terms {
    ($state:expr) => {
        hotpath::measure_block!("eval::attack_terms", {
            let statics = &$state.statics;
            let board_size = statics.board_size;
            let words = (board_size + 63) >> 6;                                 /* one bit per square, 64 to a word   */
            let span = words.max(2);                                            /* the 128-bit path reads two words   */
            let maps = &mut $state.scratch.threat_maps;
            let pawn_maps = &mut $state.scratch.threat_pawns;
            let kinds = &mut $state.scratch.threat_kinds;
            let [occupied, attacks] = &mut $state.scratch.threat_boards;
            let region = |kind: usize, color: usize| (2 * kind + color) * words;
            let (once, twice, pawn_hits, pawns, royals, targets, area) =
                (0, 1, 2, 3, 4, 5, 6);
            let (guard, unsafe_checks) = (7, 8);
            let danger = statics.eval.danger_cost[0] > 0;

            if maps.len() != 18 * words {
                maps.clear();                                                   /* a new variant sizes it again       */
                maps.resize(18 * words, 0);
            }

            if danger {
                kinds.clear();
                kinds.resize(statics.pieces.len() * words, 0);
            }

            maps.fill(0);

            for color in (0..2).filter(|_| danger) {
                let [royal] = $state.royal_list[color][..] else {
                    continue;
                };
                let stride = statics.eval.local_stride;
                let count = statics.eval.ring_counts[royal as usize] as usize;

                for slot in 0..count {
                    let at = statics.eval.ring_squares[
                        royal as usize * stride + slot
                    ] as usize;

                    maps[region(guard, color) + (at >> 6)] |= 1 << (at & 63);
                }
            }
            pawn_maps.clear();
            (occupied.0, occupied.1) = (statics.files, statics.ranks);
            (attacks.0, attacks.1) = (statics.files, statics.ranks);

            for word in 0..span {
                occupied.2[word] = $state.pieces_board[0].2[word]
                    | $state.pieces_board[1].2[word];
            }

            let mut mobility = [[0i32; 2]; 2];                                  /* colour to opening, endgame         */
            let mut attackers = [0i64; 2];                                      /* pieces of a colour on the enemy    */
            let mut ring_hits = [0i64; 2];                                      /* ring, and their hits next to it    */

            for first in [true, false] {                                        /* pawns and royals, then the rest    */
                for color in (0..2).filter(|_| !first) {                        /* pawns and royals are all mapped    */
                    for word in 0..words {
                        maps[region(area, color) + word] =
                            !(maps[region(pawns, color) + word]
                                | maps[region(royals, color) + word]
                                | maps[region(pawn_hits, color ^ 1) + word]);
                    }
                }

                for (piece_index, piece) in statics.pieces.iter().enumerate() {
                    let color = p_color!(piece) as usize;
                    let pawn = statics.eval.pawn_slots[piece_index] != NO_PAWN;
                    let royal = p_is_royal!(piece);

                    if (pawn || royal) != first {
                        continue;
                    }

                    for &square in piece_squares!($state, piece_index) {
                        let at = square as usize;
                        let mut reach = 0usize;

                        attacks.2[..span].fill(0);
                        piece_attacks!(
                            $state, piece_index, square, occupied, attacks
                        );

                        for word in 0..words {
                            let hits = attacks.2[word];

                            maps[region(twice, color) + word] |=
                                maps[region(once, color) + word] & hits;
                            maps[region(once, color) + word] |= hits;
                            maps[region(pawn_hits, color) + word] |=
                                hits * pawn as u64;
                            reach +=(hits & maps[region(area, color) + word])
                                .count_ones() as usize;
                        }

                        if danger {
                            let enemy = color ^ 1;
                            let mut near = false;

                            for word in 0..words {
                                let hits = attacks.2[word];
                                let next = maps[region(guard, enemy) + word];
                                let ring = next
                                    | maps[region(royals, enemy) + word];

                                kinds[piece_index * words + word] |= hits;
                                near |= hits & ring != 0;
                                ring_hits[color] += (hits & next)
                                    .count_ones() as i64 * !first as i64;
                            }

                            attackers[color] += (near && !first) as i64;
                        }

                        maps[region(pawns, color) + (at >> 6)] |=
                            (pawn as u64) << (at & 63);
                        maps[region(royals, color) + (at >> 6)] |=
                            (royal as u64) << (at & 63);

                        if pawn {
                            pawn_maps.push((color * board_size + at) as u64);   /* colour and square, then the map    */
                            pawn_maps.extend_from_slice(&attacks.2[..words]);
                        } else if !first {
                            let worth = statics.eval.mobility_table[reach];

                            mobility[color][0] += worth[0];
                            mobility[color][1] += worth[1];
                        }
                    }
                }
            }

            for record in pawn_maps.chunks_exact(words + 1) {
                let color = record[0] as usize / board_size;
                let at = record[0] as usize % board_size;
                let bit = 1u64 << (at & 63);
                let attacked = maps[region(once, color ^ 1) + (at >> 6)];
                let covered = maps[region(once, color) + (at >> 6)];

                if attacked & bit != 0 && covered & bit == 0 {
                    continue;
                }

                for word in 0..words {
                    maps[region(targets, color) + word] |= record[1 + word];
                }
            }

            let mut counts = [[0i32; 3]; 2];                                    /* hanging, pawn, restricted          */

            for color in 0..2 {
                let them = color ^ 1;

                for word in 0..words {
                    let at = |kind: usize, side: usize| {
                        maps[region(kind, side) + word]
                    };
                    let strong = at(pawn_hits, them)
                        | (at(twice, them) & !at(twice, color));
                    let enemies =
                        $state.pieces_board[them].2[word] & !at(royals, them);
                    let weak = enemies & !strong & at(once, color);
                    let hanging = weak & (
                        !at(once, them) | (!at(pawns, them) & at(twice, color))
                    );
                    let hit = at(targets, color) & enemies & !at(pawns, them);
                    let restricted = at(once, them) & !strong & at(once, color);

                    counts[color][0] += hanging.count_ones() as i32;
                    counts[color][1] += hit.count_ones() as i32;
                    counts[color][2] += restricted.count_ones() as i32;
                }
            }

            let eval = &statics.eval;
            let mut cost = [[0i32; 2]; 2];                                      /* colour of the royal to its cost    */

            for color in (0..2).filter(|_| danger) {
                let [royal] = $state.royal_list[color][..] else {
                    continue;
                };
                let them = color ^ 1;
                let hand = &$state.piece_in_hand[them];
                let left = $state.termination.checks.as_ref()
                    .filter(|_| !eval.danger_race.is_empty())
                    .map(|checks| {
                        checks.count.saturating_sub(checks.delivered[them])
                            .max(1) as usize
                    });
                let race = left.map(|left| eval.danger_race[left]);
                let always_safe = left == Some(1);                              /* the next check wins the race       */
                let mut checks = 0i64;

                for (piece_index, piece) in statics.pieces.iter().enumerate() {
                    let mirror = statics.piece_swap_map[piece_index];
                    let held = drops!($state) && hand[piece_index] > 0;
                    let pawn = statics.eval.pawn_slots[piece_index] != NO_PAWN;
                    let reach = &kinds[piece_index * words..][..words];

                    if p_color!(piece) as usize != them
                        || p_is_royal!(piece)
                        || mirror == NO_PIECE
                        || !held && (pawn || reach.iter().all(|w| *w == 0)) {   /* a pawn checks only from the hand   */
                        continue;
                    }

                    attacks.2[..span].fill(0);
                    piece_attacks!($state, mirror, royal, occupied, attacks);

                    let mut safe = 0u32;

                    for pass in [false, true] {
                        for word in 0..words {
                            let board = reach[word] * !pawn as u64;
                            let open = !occupied.2[word] * held as u64;         /* a held piece drops on any empty    */
                            let hits = attacks.2[word] & (board | open)
                                & !$state.pieces_board[them].2[word];
                            let weak = maps[region(once, them) + word]
                                & !maps[region(twice, color) + word];
                            let shielded = !maps[region(once, color) + word]
                                | (weak & maps[region(twice, them) + word])
                                | (always_safe as u64).wrapping_neg();
                            let slot = region(unsafe_checks, color) + word;

                            match pass {
                                false => safe += (hits & shielded).count_ones(),
                                true => maps[slot] |= hits * (safe == 0) as u64,
                            }
                        }
                    }

                    checks += match safe {
                        0 => 0,
                        1 => eval.danger_check[0] as i64,
                        _ => eval.danger_check[1] as i64,
                    };
                }

                let mut weak_ring = 0i64;
                let mut pawn_hits_ring = 0i64;
                let mut unsafe_squares = 0i64;

                for word in 0..words {
                    let ring = maps[region(guard, color) + word]
                        | maps[region(royals, color) + word];
                    let weak = maps[region(once, them) + word]
                        & !maps[region(twice, color) + word];

                    weak_ring += (ring & weak).count_ones() as i64;
                    pawn_hits_ring += (ring
                        & maps[region(pawn_hits, them) + word])
                        .count_ones() as i64;
                    unsafe_squares += maps[region(unsafe_checks, color) + word]
                        .count_ones() as i64;
                }

                let scale = COEFFICIENT_SCALE as i64;
                let major_gone = $state.major_pieces[them] == 0;
                let sum = (attackers[them] + pawn_hits_ring)
                        * attackers[them] * eval.danger_attacker as i64
                    + weak_ring * eval.danger_weak as i64
                    + ring_hits[them] * eval.danger_hit as i64
                    + unsafe_squares * eval.danger_unsafe as i64
                    + checks * race.map_or(scale, |rise| rise[0] as i64) / scale
                    - major_gone as i64 * eval.danger_no_major as i64
                    + eval.danger_base as i64;

                if sum <= eval.danger_threshold as i64 {
                    continue;
                }

                let opening = sum.min(eval.danger_cap as i64) * sum
                    * eval.danger_cost[0] as i64 / (scale * scale);
                let endgame = sum * eval.danger_cost[1] as i64 / scale
                    + race.map_or(0, |rise| opening * rise[1] as i64 / scale);

                cost[color] = [opening as i32, endgame as i32];
            }

            let hold = |to: Square, color: usize| {
                let (word, bit) = (to as usize >> 6, 1u64 << (to & 63));
                let hit = |kind: usize| {
                    maps[region(kind, color ^ 1) + word] & bit != 0
                };

                hit(once) as i32 * (1 + (hit(twice) || hit(pawn_hits)) as i32)
            };
            let goal = goal_race!($state, 0, hold) - goal_race!($state, 1, hold);

            let value = |phase: usize| {
                (counts[0][0] - counts[1][0]) * eval.hanging_value[phase]
                    + (counts[0][1] - counts[1][1])
                        * eval.pawn_threat_value[phase]
                    + (counts[0][2] - counts[1][2])
                        * eval.restricted_value[phase]
                    + mobility[0][phase] - mobility[1][phase]
                    + cost[1][phase] - cost[0][phase]
                    + goal
            };

            (value(0), value(1))
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
/// race and the extinction threats. They do not depend on the phase, so
/// the evaluation calculates them once and adds the same value to the two
/// halves before the blend. The goal race needs the attack maps, so it is
/// in [`attack_terms!`].
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
            + extinct_threat!($state, black)
            - extinct_threat!($state, white)
    }};
}

/// endgame_score!
///
/// Gives the endgame score, White minus Black: the material and the
/// piece-square totals. The terms that keep a royal at home stay out, as
/// an endgame royal must go to the center.
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
/// Gives the static evaluation of a position for the side to move. The
/// phase selects the parts:
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
/// - threats, mobility      : both halves, one value for each
/// - goal race              : with the threats, the same in both halves
/// - material imbalance     : outside the blend, added once
/// - tempo                  : outside, after the flip to the side to move
///
/// With a window, the attack terms are left out when the score without
/// them is beyond the window by more than `threat_margin`: they would not
/// bring it back inside. Without a window, the score is always complete.
///
/// Params:
/// - state: &mut State -> position to evaluate
/// - alpha: i32        -> lower bound of the window, optional
/// - beta : i32        -> upper bound of the window, optional
///
/// Return:
/// i32                 -> score for the side to move
///
/// Notes:
/// The pawn structure is calculated once for each node. A phase outside
/// the four causes a panic.
///
#[macro_export]
macro_rules! evaluate_position {
    ($state:expr) => {
        evaluate_position!($state, -INF, INF)
    };
    ($state:expr, $alpha:expr, $beta:expr) => {
        hotpath::measure_block!("eval::position", {
            let side_sign = -2 * $state.playing as i32 + 1;
            let phase = $state.game_phase;
            let opening_bound = $state.statics.opening_score as i32;
            let endgame_bound = $state.statics.endgame_score as i32;
            let current = $state.phase_score as i32;
            let tempo = $state.statics.eval.tempo_bonus;
            let margin = $state.statics.eval.threat_margin;

            let shared = shared_score!($state);
            let (pawn_opening, pawn_endgame) = pawn_structure!($state);
            let opening = opening_score!($state) + shared + pawn_opening;
            let endgame = endgame_score!($state) + shared + pawn_endgame;
            let material = material_advantage!($state);

            let finish = |opening: i32, endgame: i32| {
                let score = match phase {
                    OPENING | SETUP => opening,
                    ENDGAME => endgame,
                    MIDDLEGAME => (
                        opening * (current - endgame_bound)
                            + endgame * (opening_bound - current)
                    ) / (opening_bound - endgame_bound),
                    _ => panic!("Invalid game phase {}", phase),
                };

                (score + material) * side_sign + tempo
            };
            let lazy = finish(opening, endgame);

            if lazy - margin >= $beta || lazy + margin <= $alpha {
                lazy
            } else {
                let (attack_opening, attack_endgame) = attack_terms!($state);

                finish(opening + attack_opening, endgame + attack_endgame)
            }
        })
    };
}
