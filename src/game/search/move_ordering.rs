//! move_ordering.rs
//!
//! Static exchange evaluation and move scoring for search-time ordering.
//!
//! Captures use full exchange simulation (SEE). Quiet moves use killer and
//! butterfly history scores. Incremental selection defers unused tail work.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

/*----------------------------------------------------------------------------*\
                          STATIC EXCHANGE EVALUATION
\*----------------------------------------------------------------------------*/

/// SEE helper macros.
///
/// `attack_value!` prices the moving piece, `victim_value!` prices captured
/// material, and `lva!` regenerates captures onto one target square.
///
/// attack_value!
///
///   Params:
///   - mv   : &Move  -> move whose attacker is priced
///   - state: &State -> position providing piece values
///
///   Return:
///   i32 -> attacker value, promoted value for promotions
///
/// victim_value!
///
///   Params:
///   - mv   : &Move  -> capture move whose victims are priced
///   - state: &State -> position providing piece values
///
///   Return:
///   i32 -> summed value of captured pieces, unloads skipped
///
/// lva!
///
///   Params:
///   - state  : &State        -> position providing attacks and board
///   - target : Square        -> exchange target square
///   - color  : u8            -> side owning the target piece
///   - out    : &mut Vec<Move> -> generated candidate captures
///   - scratch: &mut Vec<u64> -> multi-capture payload scratch
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
                |value, &captured| {
                    let is_unload = multi_move_is_unload!(captured);
                    let piece = multi_move_captured_piece!(captured);

                    value + p_value!(piece, $state) as i32
                        * !is_unload as i32
                },
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
            .for_each(|(piece_index, square_index, vector)| {
                let piece = &state.statics.pieces[*piece_index as usize];
                process_multi_leg_vector!(
                    *square_index, piece, vector, state, out, scratch
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
                    !multi_move_is_unload!(captured)
                    && multi_move_captured_square!(captured) as u16 == target
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
/// Evaluates a capture sequence on one target square. Positive scores win
/// material; negative scores lose material. Position is restored on return.
///
/// The candidate list and its multi-capture payload come from
/// [`SEE_BUFFERS`] rather than from a fresh allocation. Every scored capture
/// runs this once, so the pair was being asked of the allocator and handed
/// straight back hundreds of thousands of times a search, for two vectors
/// that carry nothing between calls.
///
/// Params:
/// - state: &mut State -> position simulated and restored
/// - mv   : &Move      -> capture move evaluated
///
/// Return:
/// i32 -> net material gain for moving side
#[macro_export]
macro_rules! see {
    ($state:expr, $mv:expr) => {
        hotpath::measure_block!("order::see", {
        SEE_BUFFERS.with(|buffers| {
        let borrowed = &mut *buffers.borrow_mut();
        let (moves, scratch) = (&mut borrowed.0, &mut borrowed.1);
        let state: &mut State = $state;
        let seen_move: &Move = $mv;
        let initial_attackee = victim_value!(seen_move, state);
        let mut gain = [0i32; 32];
        let mut gain_length = 0usize;

        gain[gain_length] = initial_attackee;                                   /* it leaves at this ply's phase     */
        gain_length += 1;

        if !make_move!(state, seen_move.clone()) {
            -INF
        } else {
            let initial_attacker = attack_value!(seen_move, state);             /* it leaves at the next one         */

            gain[gain_length] = initial_attacker - initial_attackee;
            gain_length += 1;

            let target = end!(seen_move) as Square;
            let mut moves_to_undo = 1;

            'main_loop: loop {
                lva!(state, target, state.playing, moves, scratch);

                let Some(mut attacker) = moves.pop() else {
                    break;
                };
                let mut attacker_piece = if m_promotion!(&attacker) {
                    promoted!(&attacker)
                } else {
                    piece!(&attacker)
                };

                while !make_move!(state, attacker) {
                    if moves.is_empty() {
                        break 'main_loop;
                    }

                    attacker = moves.pop().unwrap();
                    attacker_piece = if m_promotion!(&attacker) {
                        promoted!(&attacker)
                    } else {
                        piece!(&attacker)
                    };
                }

                let attacker_value = p_value!(attacker_piece, state) as i32;    /* priced once its capture is made   */

                gain[gain_length] =
                    attacker_value - gain[gain_length - 1];
                gain_length += 1;
                moves_to_undo += 1;

                if gain_length >= gain.len() {
                    break;
                }
            }

            gain_length -= 1;

            if gain_length > 1 {
                for index in (1..gain_length).rev() {
                    gain[index - 1] =
                        -cmp::max(-gain[index - 1], gain[index]);
                }
            }

            while moves_to_undo > 0 {
                undo_move!(state);
                moves_to_undo -= 1;
            }

            gain[0]
        }
        })
        })
    };
}

/*----------------------------------------------------------------------------*\
                           MOVE SCORING AND ORDERING
\*----------------------------------------------------------------------------*/

/// score_move!
///
/// Returns one ordering score. Priority: table move, winning capture,
/// killers, butterfly history, losing capture, then a capture the exchange
/// simulation could not make.
///
/// A capture is priced by the exchange simulation only where the variant's
/// rules leave that simulation meaning what it says: the swing has to be the
/// currency, and the attackers of a square must not depend on who is standing
/// nearby. Elsewhere the price is the plain difference between what the move
/// takes and what it risks, which claims less and stays inside the same score
/// bands, so every reader downstream keeps reading winning and losing the
/// same way.
///
/// Params:
/// - state     : &mut State          -> position the move is scored on
/// - info      : &SearchInfo         -> killer and history tables
/// - mv        : &Move               -> move to score
/// - table_move: &Option<PseudoMove> -> stored table move for this node
///
/// Return:
/// usize -> ordering score, larger searched earlier
#[macro_export]
macro_rules! score_move {
    ($state:expr, $info:expr, $mv:expr, $table_move:expr) => {{
        let scored_move: &Move = $mv;

        if $table_move.as_ref().is_some_and(
            |table_move| m_matches!(scored_move, table_move)
        ) {
            TABLE_MOVE_SCORE
        } else if !m_capture!(scored_move) {
            let killers =
                &$info.killer_hist[$state.search_ply as usize];

            if *scored_move == killers[0] {
                KILLER_MOVE_SCORE + 2
            } else if *scored_move == killers[1] {
                KILLER_MOVE_SCORE + 1
            } else {
                let piece = piece!(scored_move) as usize;
                let end = end!(scored_move) as usize;
                let board_size = $state.statics.board_size;
                let index = piece * board_size + end;
                let history = $info.search_hist[index] as i32;

                (QUIET_MOVE_SCORE + history) as usize
            }
        } else if see_valid!($state) && static_movement!($state) {
            let see_score = see!($state, scored_move);

            if see_score == -INF {
                UNMAKEABLE_CAPTURE_SCORE
            } else if see_score >= 0 {
                (WINNING_CAPTURE_SCORE + see_score) as usize
            } else {
                (LOSING_CAPTURE_SCORE + see_score) as usize
            }
        } else {
            let swing = victim_value!(scored_move, $state)
                - attack_value!(scored_move, $state);

            if swing >= 0 {
                (WINNING_CAPTURE_SCORE + swing) as usize
            } else {
                (LOSING_CAPTURE_SCORE + swing) as usize
            }
        }
    }};
}

/// pick_by_score!
///
/// Selects the best-scoring move in `moves[index..]` and swaps it into
/// `index`. Scores are filled lazily and cached in the parallel vector.
///
/// Params:
/// - state     : &mut State          -> position used for scoring
/// - info      : &SearchInfo         -> killer and history tables
/// - moves     : &mut Vec<Move>      -> move list reordered in place
/// - scores    : &mut Vec<usize>     -> lazily filled score cache
/// - index     : usize               -> slot receiving best remaining move
/// - table_move: &Option<PseudoMove> -> stored table move for this node
#[macro_export]
macro_rules! pick_by_score {
    (
        $state:expr,
        $info:expr,
        $moves:expr,
        $scores:expr,
        $index:expr,
        $table_move:expr
    ) => {
        hotpath::measure_block!("order::pick", {
        let moves: &mut Vec<Move> = $moves;
        let scores: &mut Vec<usize> = $scores;
        let index = $index;

        if index == 0 {
            if let Some(table_move) = $table_move.as_ref() {
                if let Some(table_index) = moves.iter()
                    .position(|mv| m_matches!(mv, table_move))
                {
                    moves.swap(0, table_index);
                    scores.swap(0, table_index);
                    scores[0] = TABLE_MOVE_SCORE;
                }
            }
        }

        if scores[index] == usize::MAX {
            scores[index] = score_move!(
                $state, $info, &moves[index], $table_move
            );
        }

        let mut best_index = index;
        let mut best_score = scores[index];

        if best_score != TABLE_MOVE_SCORE {
            for candidate in (index + 1)..moves.len() {
                if scores[candidate] == usize::MAX {
                    scores[candidate] = score_move!(
                        $state, $info, &moves[candidate], $table_move
                    );
                }

                if scores[candidate] > best_score {
                    best_score = scores[candidate];
                    best_index = candidate;
                }
            }
        }

        if best_index != index {
            moves.swap(index, best_index);
            scores.swap(index, best_index);
        }
        })
    };
}
