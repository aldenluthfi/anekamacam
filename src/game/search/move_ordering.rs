//! move_ordering.rs
//!
//! Static exchange evaluation and move scores for move ordering.
//!
//! Alpha-beta is fast when the move that cuts comes first. This file gives
//! each move a score before the search. A capture gets the result of the
//! exchange on the board. A quiet move gets its killer and history values.
//! The search gets one move at a time, because most nodes cut early.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

/*----------------------------------------------------------------------------*\
                           STATIC EXCHANGE EVALUATION
\*----------------------------------------------------------------------------*/

/// SEE helper macros
///
/// Helpers for the exchange simulation. They give the value of the moving
/// piece, the value of the captured pieces and the next attackers.
///
/// - attack_value! : value of the moving piece, or of its promotion
/// - victim_value! : value that the move wins, minus own pieces it removes
/// - lva!          : all legal captures on one square, cheapest last
///
/// attack_value!
///
///   Params:
///   - mv   : &Move  -> move with the attacker to value
///   - state: &State -> position with the piece values
///
///   Return:
///   i32             -> attacker value, promoted value for promotions
///
/// victim_value!
///
///   Params:
///   - mv   : &Move  -> move with the victims to value
///   - state: &State -> position with the piece values
///
///   Return:
///   i32             -> enemy value taken minus own value, 0 for no capture
///
/// lva!
///
///   Params:
///   - state  : &State         -> position with the attacks and the board
///   - target : Square         -> target square of the exchange
///   - color  : u8             -> side of the target piece
///   - out    : &mut Vec<Move> -> list that gets the captures
///   - scratch: &mut Vec<u64>  -> scratch for multi-capture data
///
/// Notes:
/// A move can capture many pieces, so `victim_value!` adds them. A capture
/// of an own piece counts as a loss. `lva!` sorts in descending order, and
/// the caller pops from the back, so the least valuable attacker is next.
///
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
        let valued_move: &Move = $mv;
        let move_type = move_type!(valued_move);

        if move_type == SINGLE_CAPTURE_MOVE {
            let piece = captured_piece!(valued_move);
            let own = captured_own!(valued_move);

            p_value!(piece, $state) as i32 * (1 - 2 * own as i32)
        } else if move_type == MULTI_CAPTURE_MOVE {
            m_captures!(valued_move).iter().fold(
                0,
                |value, &captured| {
                    let is_unload = multi_move_is_unload!(captured);
                    let is_own = multi_move_captured_own!(captured);
                    let piece = multi_move_captured_piece!(captured);

                    value + p_value!(piece, $state) as i32
                        * !is_unload as i32 * (1 - 2 * is_own as i32)
                },
            )
        } else {
            0
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

        out.retain(|mv| m_capture!(mv) && m_takes_square!(mv, target));

        out.sort_unstable_by_key(
            |mv| -(p_value!(piece!(mv), state) as i32)
        );
        })
    };
}

/// see!
///
/// Evaluates the capture sequence on one target square. A positive score
/// wins material. A quiet move takes nothing, so its score is zero or the
/// loss of the moved piece. The macro restores the position before it
/// returns.
///
/// Each side captures with its cheapest attacker until no attacker is
/// left. Each entry is the balance for the side that moved at that ply:
///
/// - `gain[0]` : value of the first capture
/// - `gain[1]` : value of the first attacker, minus `gain[0]`
/// - `gain[n]` : the same, one ply deeper each time
///
/// ```text
/// backward  gain[i - 1] = -max(-gain[i - 1], gain[i])
/// ```
///
/// A side does not have to recapture. The backward pass keeps the better
/// result of capture or no capture at each ply. `gain[0]` is the result
/// for the first side with best play.
///
/// Params:
/// - state: &mut State -> position to simulate and restore
/// - mv   : &Move      -> capture or quiet move to evaluate
///
/// Return:
/// i32                 -> net material gain for the side to move
///
/// Notes:
/// An illegal first move gives `-INF`. The sequence stops at the array
/// length. The move list and data vectors come from [`Scratch`], so there
/// is no allocation. The macro takes out only the two vectors, because
/// `make_move!` borrows the full state.
///
#[macro_export]
macro_rules! see {
    ($state:expr, $mv:expr) => {
        hotpath::measure_block!("order::see", {
        let state: &mut State = $state;
        let mut held_moves = mem::take(&mut state.scratch.see_moves);
        let mut held_payload = mem::take(&mut state.scratch.see_scratch);
        let (moves, scratch) = (&mut held_moves, &mut held_payload);
        let seen_move: &Move = $mv;
        let initial_attackee = victim_value!(seen_move, state);
        let mut gain = [0i32; 32];
        let mut gain_length = 0usize;

        gain[gain_length] = initial_attackee;                                   /* it leaves at this ply's phase      */
        gain_length += 1;

        let exchange = if !make_move!(state, seen_move.clone()) {
            -INF
        } else {
            let initial_attacker = attack_value!(seen_move, state);             /* it leaves at the next one          */

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

                let attacker_value = p_value!(attacker_piece, state) as i32;    /* priced once its capture is made    */

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
        };

        state.scratch.see_moves = held_moves;
        state.scratch.see_scratch = held_payload;

        exchange
        })
    };
}

/*----------------------------------------------------------------------------*\
                           MOVE SCORING AND ORDERING
\*----------------------------------------------------------------------------*/

/// score_move!
///
/// Gives one ordering score. The search tries a larger score first. The
/// bands, from high to low:
///
/// - table move         : the stored move of the hash table
/// - winning capture    : exchange result or simple swing, 0 or more
/// - killer             : two quiet moves that cut at this ply before
/// - quiet              : quiet band plus the history values
/// - losing capture     : exchange result or simple swing, below 0
/// - unmakeable capture : the exchange simulation cannot make the move
///
/// The quiet history is the butterfly cell plus one continuation cell for
/// each earlier move of this node. The prelude spaces the bands, so the
/// largest history is always below the lowest killer.
///
/// The exchange simulation applies only if `see_valid!` is true for the
/// variant. Else, the score is the victim value minus the attacker value.
/// A screened leg does not stop the order: the simulation makes each
/// capture and reads the attackers again. Only the skips that trust the
/// result also need `static_movement!`.
///
/// Params:
/// - state     : &mut State          -> position of the move
/// - info      : &SearchInfo         -> killer and history tables
/// - mv        : &Move               -> move to score
/// - table_move: &Option<PseudoMove> -> stored table move for this node
/// - cont_bases: &[usize]            -> continuation rows for this node
///
/// Return:
/// usize                             -> ordering score, larger is earlier
///
#[macro_export]
macro_rules! score_move {
    (
        $state:expr,
        $info:expr,
        $mv:expr,
        $table_move:expr,
        $cont_bases:expr
    ) => {{
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
                let board_size = $state.statics.board_size;
                let index = move_key!(scored_move, board_size);

                let continuation: i32 = $cont_bases.iter()
                    .filter(|&&base| base != usize::MAX)
                    .map(|&base| {
                        $info.cont_hist[cont_cell!($info, base + index)] as i32
                    })
                    .sum();

                let history =
                    $info.search_hist[index] as i32 + continuation;

                (QUIET_MOVE_SCORE + history) as usize
            }
        } else if see_valid!($state) {
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
/// Moves the best remaining move to `index`. The macro does one selection
/// pass for each move that the search asks for, not a full sort. Thus a
/// node that cuts early does not score all moves.
///
/// ```text
/// index 0   [. . . . . .]  score all, swap the best to slot 0
/// index 1   [x . . . . .]  score the rest, swap the best to slot 1
/// cut here  [x x . . . .]  the rest is never scored
/// ```
///
/// Params:
/// - state     : &mut State          -> position for the scores
/// - info      : &SearchInfo         -> killer and history tables
/// - moves     : &mut Vec<Move>      -> move list, reordered in place
/// - scores    : &mut Vec<usize>     -> score cache, filled when necessary
/// - index     : usize               -> slot that gets the best move
/// - table_move: &Option<PseudoMove> -> stored table move for this node
/// - cont_bases: &[usize]            -> continuation rows for this node
///
/// Notes:
/// `usize::MAX` marks a score that is not calculated yet, so each move gets
/// one score only. On the first call, the table move goes to slot 0. While
/// it is there, no other move gets a score.
///
#[macro_export]
macro_rules! pick_by_score {
    (
        $state:expr,
        $info:expr,
        $moves:expr,
        $scores:expr,
        $index:expr,
        $table_move:expr,
        $cont_bases:expr
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
                $state, $info, &moves[index], $table_move, $cont_bases
            );
        }

        let mut best_index = index;
        let mut best_score = scores[index];

        if best_score != TABLE_MOVE_SCORE {
            for candidate in (index + 1)..moves.len() {
                if scores[candidate] == usize::MAX {
                    scores[candidate] = score_move!(
                        $state, $info, &moves[candidate], $table_move,
                        $cont_bases
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
