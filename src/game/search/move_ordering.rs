//! move_ordering.rs
//!
//! Static exchange evaluation and move scoring for search-time ordering.
//!
//! Alpha-beta is paid for in move order: the move that cuts should be tried
//! first, and everything behind it should be cheap to reject. This file says
//! what a move is worth before it is searched — a capture by playing the
//! whole exchange out on the board, a quiet move by what the killer and
//! history tables remember of it — and hands the search one move at a time,
//! since most nodes never ask for the rest of the list.
//!
//! Created: 19/04/2026
//! Author : Alden Luthfi

/*----------------------------------------------------------------------------*\
                           STATIC EXCHANGE EVALUATION
\*----------------------------------------------------------------------------*/

/// SEE helper macros
///
/// The three things an exchange simulation needs: what the mover is worth,
/// what it takes, and who else can reach the square.
///
/// - attack_value! : the moving piece, or what it promotes into
/// - victim_value! : everything the move captures, unloads not counted
/// - lva!          : every legal capture onto one square, cheapest last
///
/// A move may take more than one piece, so the victim side is a sum rather
/// than a lookup, and a piece a move merely puts down is not a piece it took.
/// `lva!` sorts descending and the caller pops from the back, which is what
/// makes the least valuable attacker the next one to try.
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
///   - state  : &State         -> position providing attacks and board
///   - target : Square         -> exchange target square
///   - color  : u8             -> side owning the target piece
///   - out    : &mut Vec<Move> -> generated candidate captures
///   - scratch: &mut Vec<u64>  -> multi-capture payload scratch
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
/// The square is fought over until neither side has an attacker left, each
/// ply taking with its cheapest one, and the running balance is written down
/// from the side that moved at that ply:
///
/// - `gain[0]` : what the first capture takes
/// - `gain[1]` : the attacker it left there, less `gain[0]`
/// - `gain[n]` : the same again, one ply deeper each time
///
/// ```text
/// backward  gain[i - 1] = -max(-gain[i - 1], gain[i])
/// ```
///
/// The backward pass is where declining enters. A side is never obliged to
/// recapture, so reading from the last ply to the first keeps, at each step,
/// the better of taking and standing still, and `gain[0]` comes out as what
/// the exchange is worth to whoever started it against best play.
///
/// The sequence is capped at the array's length. A square fought over by
/// more than that many pieces is scored on the part that fit, which orders
/// no worse than a position nobody will reach in a real game.
///
/// The candidate list and its multi-capture payload are the state's own
/// [`Scratch`] rather than a fresh allocation. Every scored capture runs
/// this once, so the pair was being asked of the allocator and handed
/// straight back hundreds of thousands of times a search, for two vectors
/// that carry nothing between calls.
///
/// The two are taken out and put back because the body makes and unmakes
/// moves on the same state, and a field borrow held across `make_move!` is
/// a borrow of the entire position. Only the vectors move, never the whole
/// [`Scratch`]: its `Default` allocates a pawn table, which a take would
/// build and drop once per scored capture.
///
/// Params:
/// - state: &mut State -> position simulated and restored
/// - mv   : &Move      -> capture move evaluated
///
/// Return:
/// i32 -> net material gain for moving side
///
/// Notes:
/// A move that cannot legally be made at all returns `-INF` rather than a
/// number, which the scorer turns into the lowest band there is. The
/// simulation makes and unmakes real moves, so illegality is discovered the
/// same way the search discovers it, and the position is left exactly as it
/// was however early the sequence ended.
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

        gain[gain_length] = initial_attackee;                                   /* it leaves at this ply's phase     */
        gain_length += 1;

        let exchange = if !make_move!(state, seen_move.clone()) {
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
/// Returns one ordering score, larger meaning searched sooner.
///
/// - table move         : the move that already worked here
/// - winning capture    : by the exchange simulation, or by the plain swing
/// - killer             : two quiet moves that cut at this ply before
/// - quiet              : centre score plus what the history tables say
/// - losing capture     : still played, but after every quiet move
/// - unmakeable capture : the simulation could not even make it
///
/// The bands themselves live in the prelude, spaced so that the widest
/// history score a quiet move can reach still lands under the lowest killer,
/// which is what keeps a band from bleeding into its neighbour.
///
/// A quiet move's history is the butterfly cell plus one continuation cell
/// per ply this node has a move to answer. The butterfly cell says the move
/// worked somewhere; a continuation cell says it worked as the reply to the
/// move actually on the board, which is the narrower claim and the one worth
/// ordering by when the node has one.
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
/// - cont_bases: &[usize]            -> continuation rows for this node
///
/// Return:
/// usize -> ordering score, larger searched earlier
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
                    .map(|&base| $info.cont_hist[base + index] as i32)
                    .sum();

                let history =
                    $info.search_hist[index] as i32 + continuation;

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
/// Brings the best remaining move to `index`, one selection pass per move
/// the search actually asks for. Sorting the list would price every move in
/// it; a node that cuts on its second move should pay for two.
///
/// ```text
/// index 0   [ . . . . . . ]  score them all, swap the best to the front
/// index 1   [x . . . . . ]   score the rest, swap the best to slot one
/// cut here  [x x . . . . ]   the tail is never scored, let alone searched
/// ```
///
/// Scores live in a vector beside the moves, `usize::MAX` marking a slot not
/// yet priced, so a move that survives several passes is priced once. That
/// matters most for captures, where a price means running the whole exchange
/// simulation over the board.
///
/// The table move short-circuits both halves. It is swapped to the front on
/// the first call and given the band above everything else, so while it holds
/// the slot no other move is scored at all.
///
/// Params:
/// - state     : &mut State          -> position used for scoring
/// - info      : &SearchInfo         -> killer and history tables
/// - moves     : &mut Vec<Move>      -> move list reordered in place
/// - scores    : &mut Vec<usize>     -> lazily filled score cache
/// - index     : usize               -> slot receiving best remaining move
/// - table_move: &Option<PseudoMove> -> stored table move for this node
/// - cont_bases: &[usize]            -> continuation rows for this node
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
