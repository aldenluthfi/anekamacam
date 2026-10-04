//! termination.rs
//!
//! The game end rules of a variant, and the detectors that apply them.
//!
//! One `Termination` table has all end rules. Each optional rule is `Some`
//! only when the config declares it. `position_terminal` tests the rules of
//! the position after each move. `game_outcome` adds the repetition and
//! perpetual results, which it calculates from the history.
//!
//! Created: 26/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                               OUTCOME AND RULES
\*----------------------------------------------------------------------------*/

/// Outcome
///
/// The result of an end rule, for the side that the rule names. Each rule
/// doc gives its subject, and `position_terminal` returns that colour with
/// the outcome. Three rules calculate the colour from the position:
///
/// - `extinct`    : the colour that has no pieces of the set left
/// - `adjudicate` : the colour with more points
/// - `perpetual`  : the only offender
///
/// `resolve_outcome!` converts an outcome and its subject into a
/// `game_result`. `outcome_score!` converts an outcome for the side to
/// move into a search score.
///
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Outcome {
    Draw,                                                                       /* neither side wins                  */
    Win,                                                                        /* the evaluated side wins            */
    Loss,                                                                       /* the evaluated side loses           */
}

/// Counter
///
/// A progress counter. It fires after `limit` halfmoves without a reset.
/// A capture or a drop always resets it. A quiet move of a piece with
/// `reset_pieces[i]` set also resets it.
///
/// Subject: the side that made the move at the limit.
///
#[derive(Clone)]
pub struct Counter {
    pub clock: u8,                                                              /* current reversible halfmove count  */
    pub limit: u8,                                                              /* halfmoves before the outcome fires */
    pub reset_pieces: Vec<bool>,                                                /* moving these resets the counter    */
    pub outcome: Outcome,                                                       /* result once the limit is reached   */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Counting
///
/// A move limit for a bare king endgame. When one side has only a royal,
/// the other side must give mate within a limit, else the game ends with
/// `outcome`. Its material sets the limit.
///
/// - `progress` : count and limit, the count starts at the piece count
/// - `table`    : ordered `(requirements, limit)` rows, first match wins
/// - `default`  : limit when no row matches
///
/// A requirement is a piece set and the minimum number of those pieces.
/// The count increases by one each ply and a capture does not reset it.
///
/// Subject: the side with material, not the side to move.
///
#[derive(Clone)]
pub struct Counting {
    pub progress: Option<(u16, u16)>,                                           /* current count and frozen limit     */
    pub table: Vec<(Vec<(Vec<bool>, u32)>, u16)>,                               /* ordered (requirements, limit) rows */
    pub default: u16,                                                           /* limit when no row matches          */
    pub outcome: Outcome,                                                       /* result once the count hits limit   */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Extinct
///
/// A material extinction rule. When the count of `set` pieces of a colour
/// is `threshold` or less, that colour gets `outcome`. Derivation sets
/// `lone`, the piece of a colour that stands in for the royal in the
/// evaluation.
///
/// Subject: the colour with no pieces left. It is not always the side that
/// moved. A capture removes enemy pieces, and a promotion can remove own
/// pieces.
///
#[derive(Clone)]
pub struct Extinct {
    pub set: Vec<bool>,                                                         /* piece indices the rule counts      */
    pub threshold: u8,                                                          /* count at or below which it fires   */
    pub outcome: Outcome,                                                       /* result for the extinct colour      */
    pub name: String,                                                           /* reason reported when it fires      */
    pub lone: [Option<usize>; 2],                                               /* colour to its royal stand-in       */
}

/// Goal
///
/// A goal zone rule. When a colour moves a `set` piece to a `zone` square,
/// that colour gets `outcome`. The two colours share the zone. Derivation
/// sets `steps`, `closer` and `value` for the goal race of the evaluation.
/// The two tables have one row of squares for each piece, and only the
/// `set` pieces fill their rows.
///
/// Subject: the colour with the piece in the zone.
///
#[derive(Clone)]
pub struct Goal {
    pub set: Vec<bool>,                                                         /* piece indices that reach the zone  */
    pub zone: Board,                                                            /* target squares                     */
    pub outcome: Outcome,                                                       /* result for the arriving colour     */
    pub name: String,                                                           /* reason reported when it fires      */
    pub steps: Vec<u8>,                                                         /* piece, square to moves to the zone */
    pub closer: Vec<Vec<Square>>,                                               /* piece, square to next step squares */
    pub value: i32,                                                             /* worth one step from the zone       */
}

/// Adjudicate
///
/// A points rule. When the two sides pass one after the other, the points
/// decide the game. The points of a colour are the sum of `weights[i]` for
/// its pieces, plus `handicap[colour]`. The larger sum wins, and equal
/// sums draw. The config section `= adjudicate <name> =` gives the values.
///
/// Subject: the colour with the larger sum, or the side to move on a draw.
///
#[derive(Clone)]
pub struct Adjudicate {
    pub weights: Vec<i32>,                                                      /* per piece index point value        */
    pub handicap: [i32; 2],                                                     /* per colour standing point bonus    */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Perpetual
///
/// A cycle offence rule that changes `repetition`. When a repetition ends
/// the game and one side is the only offender, that side gets the outcome
/// of its offence:
///
/// - `check` : each cycle move of the offender gives check
/// - `chase` : each cycle move attacks the same undefended enemy piece
///
/// Only `chasers` pieces can chase, and the target is not royal. Check is
/// before chase. If the two sides offend, the repetition result stays.
///
/// Subject: the only offender.
///
/// Notes:
/// An offence applies only when the config declares it. `check draw` keeps
/// the cycle a draw.
///
#[derive(Clone)]
pub struct Perpetual {
    pub check: Option<Outcome>,                                                 /* sole perpetual checker's result    */
    pub chase: Option<Outcome>,                                                 /* sole perpetual chaser's result     */
    pub chasers: Vec<bool>,                                                     /* piece indices that commit a chase  */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Repetition
///
/// A repetition rule. The game ends with `outcome` when the position occurs
/// `occurrences` times. A `Perpetual` rule can change the result for one
/// offender.
///
/// The count scans the hashes in the history when necessary. In variants
/// without drops, `clock` limits the scan. A capture, drop, promotion or
/// castling move makes all earlier positions unreachable.
///
/// Subject: the side that made the repeating move.
///
#[derive(Clone)]
pub struct Repetition {
    pub occurrences: u8,                                                        /* occurrences that trigger the rule  */
    pub outcome: Outcome,                                                       /* result once reached                */
    pub name: String,                                                           /* reason reported when it fires      */
    pub clock: u16,                                                             /* plies since an irreversible move   */
}

/// Checks
///
/// An N-check rule. The side that gives its `count`-th check gets
/// `outcome`. Derivation sets `value`, the worth of the checks of a side
/// with one check left; the evaluation reads it.
///
/// Subject: the checking side, the side that moved.
///
#[derive(Clone)]
pub struct Checks {
    pub delivered: [u8; 2],                                                     /* checks delivered per colour        */
    pub count: u8,                                                              /* checks before the outcome fires    */
    pub outcome: Outcome,                                                       /* result for the checking side       */
    pub name: String,                                                           /* reason reported when it fires      */
    pub value: i32,                                                             /* worth one check from the win       */
}

/*----------------------------------------------------------------------------*\
                               TERMINATION TABLE
\*----------------------------------------------------------------------------*/

/// Termination
///
/// The end rule table and the rule progress of one position. `checkmate`
/// and `stalemate` are the outcomes when a side has no legal move. The
/// other rules are `Some` only when the variant declares them. A new game
/// resets the result and progress, but not the rules.
///
/// Three readers each read a different part of the table:
///
/// - `no_move_verdict!`  : `checkmate`, `stalemate`, when no legal move
/// - `position_terminal` : `checks` .. `counter`, after each move
/// - `game_outcome`      : `repetition`, `perpetual`, from the history
///
/// `position_terminal` tests `checks`, `goal`, `extinct`, `adjudicate`,
/// `counting` and `counter` in this order. The first match gives the
/// result.
///
#[derive(Clone)]
pub struct Termination {
    pub game_result: u8,                                                        /* eager position-local result        */
    pub checkmate: Outcome,                                                     /* no moves + in check   (dflt Loss)  */
    pub stalemate: Outcome,                                                     /* no moves, not in check(dflt Draw)  */

    pub repetition: Option<Repetition>,                                         /* repeated-position rule             */
    pub counter: Option<Counter>,                                               /* progress counter, if declared      */
    pub counting: Option<Counting>,                                             /* bare-king material count, if any   */

    pub checks: Option<Checks>,                                                 /* N-check rule                       */
    pub extinct: Vec<Extinct>,                                                  /* material-extinction rules          */
    pub goal: Option<Goal>,                                                     /* goal-zone rule                     */
    pub perpetual: Option<Perpetual>,                                           /* repetition-cycle offence rule      */
    pub adjudicate: Option<Adjudicate>,                                         /* points decision on double pass     */
}

impl Default for Termination {
    /// Termination::default
    ///
    /// Gives the rules before the `= termination =` section is read. With
    /// no legal move, check is a loss and no check is a draw. There are no
    /// optional rules.
    ///
    /// Return:
    /// Self -> the default table, no optional rule set
    ///
    fn default() -> Self {
        Termination {
            game_result: ONGOING,
            checkmate: Outcome::Loss,
            stalemate: Outcome::Draw,
            repetition: None,
            counter: None,
            counting: None,
            checks: None,
            extinct: Vec::new(),
            goal: None,
            perpetual: None,
            adjudicate: None,
        }
    }
}

impl Termination {
    /// Termination::reset_progress
    ///
    /// Clears the game result and the progress of each rule. The configured
    /// rules, limits, outcomes and names do not change.
    ///
    pub fn reset_progress(&mut self) {
        self.game_result = ONGOING;

        if let Some(repetition) = &mut self.repetition {
            repetition.clock = 0;
        }

        if let Some(counter) = &mut self.counter {
            counter.clock = 0;
        }

        if let Some(counting) = &mut self.counting {
            counting.progress = None;
        }

        if let Some(checks) = &mut self.checks {
            checks.delivered = [0; 2];
        }
    }
}

/// resolve_outcome!
///
/// Converts an [`Outcome`] for a colour into a `game_result`. Give the
/// subject colour of the rule, for example the side to move for stalemate.
///
/// Params:
/// - color  : u8      -> subject colour of the outcome
/// - outcome: Outcome -> the result to convert
///
/// Return:
/// u8                 -> DRAW, WHITE_WIN or BLACK_WIN
///
#[macro_export]
macro_rules! resolve_outcome {
    ($color:expr, $outcome:expr) => {
        match $outcome {
            Outcome::Draw => DRAW,
            Outcome::Win => {
                if $color == WHITE { WHITE_WIN } else { BLACK_WIN }
            }
            Outcome::Loss => {
                if $color == WHITE { BLACK_WIN } else { WHITE_WIN }
            }
        }
    };
}

/// outcome_score!
///
/// Converts an [`Outcome`] into a search score for the side to move. The
/// scale is the same as checkmate, so faster wins and slower losses are
/// better.
///
/// - draw : [`draw_score!`]
/// - win  : `INF - ply`
/// - loss : `-INF + ply`
///
/// Params:
/// - state  : &State  -> position with the ply and the draw value
/// - outcome: Outcome -> the result to score
///
/// Return:
/// i32                -> terminal score for the side to move
///
#[macro_export]
macro_rules! outcome_score {
    ($state:expr, $outcome:expr) => {
        match $outcome {
            Outcome::Draw => draw_score!($state),
            Outcome::Win  =>  INF - $state.search_ply as i32,
            Outcome::Loss => -INF + $state.search_ply as i32,
        }
    };
}

/// no_move_verdict!
///
/// Gives the result for a side with no legal move. In check, it is the
/// `checkmate` outcome, else the `stalemate` outcome. The result is
/// inverted when a banned mating drop gave the mate. Then the side that
/// dropped loses.
///
/// Params:
/// - state   : &mut State -> position where the side to move cannot move
/// - in_check: bool       -> true when that side is in check
///
/// Return:
/// (Outcome, bool)        -> the outcome, and true when it is inverted
///
/// Notes:
/// The two search leaves and the adjudication all use this macro, so they
/// agree.
///
#[macro_export]
macro_rules! no_move_verdict {
    ($state:expr, $in_check:expr) => {{
        let outcome = if $in_check {
            $state.termination.checkmate
        } else {
            $state.termination.stalemate
        };

        let inverted = outcome == Outcome::Loss
            && illegal_mating_drop!($state);

        (outcome, inverted)
    }};
}

/*----------------------------------------------------------------------------*\
                                   DETECTORS
\*----------------------------------------------------------------------------*/

/// side_is_bare
///
/// Tells if a colour has only one royal piece left. The major and minor
/// counts cover all pieces that are not royal. `royal_list` gives the
/// royal count.
///
/// Params:
/// - state: &State -> position to examine
/// - side : u8     -> colour to test
///
/// Return:
/// bool            -> true when the colour has only one royal
///
/// Notes:
/// The test needs exactly one royal. Only `counting` variants call it, and
/// each of them has one royal for each side.
///
pub fn side_is_bare(state: &State, side: u8) -> bool {
    state.major_pieces[side as usize] == 0 &&
    state.minor_pieces[side as usize] == 0 &&
    state.royal_list[side as usize].len() == 1
}

/// counting_limit
///
/// Gives the move limit of the side with material in a bare king endgame.
/// It is the limit of the first `Counting` row that the `winner` meets,
/// else the default. A requirement is met when the winner has at least the
/// minimum number of pieces from its set.
///
/// Params:
/// - state : &State -> position to examine
/// - winner: u8     -> colour with material, opponent of the bare king
///
/// Return:
/// u16              -> count limit for this material, 0 without a rule
///
pub fn counting_limit(state: &State, winner: u8) -> u16 {
    let Some(counting) = state.termination.counting.as_ref() else {
        return 0;
    };

    for (requirements, limit) in &counting.table {
        let met = requirements.iter().all(|(set, minimum)| {
            let owned = (0..state.statics.pieces.len())
                .filter(|&index| set[index]
                    && p_color!(state.statics.pieces[index]) == winner)
                .map(|index| state.piece_count[index])
                .sum::<u32>();
            owned >= *minimum
        });
        if met {
            return *limit;
        }
    }

    counting.default
}

/// counting_progress
///
/// Gives the bare king count after one ply. The limit does not change after
/// the count starts.
///
/// - no side or two sides bare : `None`
/// - count already started     : last count plus one, same limit
/// - count starts now          : `(piece total + 1, limit)`
///
/// Params:
/// - state: &State            -> position to examine
/// - last : Option<(u16,u16)> -> progress of the previous ply
///
/// Return:
/// Option<(u16, u16)>         -> (count, limit) while counting applies
///
/// Notes:
/// Only `make_move!` calls it, once for each ply. A FEN load does not start
/// a count, because the FEN has no count history. The count starts at the
/// first move after the load.
///
pub fn counting_progress(
    state: &State, last: Option<(u16, u16)>,
) -> Option<(u16, u16)> {
    let white_bare = side_is_bare(state, WHITE);
    let black_bare = side_is_bare(state, BLACK);

    if white_bare == black_bare {
        return None;                                                            /* neither or both bare: not counting */
    }

    if let Some((count, limit)) = last {
        return Some((count.saturating_add(1), limit));                          /* frozen limit, tick the count       */
    }

    let material = if white_bare { BLACK } else { WHITE };
    let pieces = state.piece_count.iter().sum::<u32>() as u16;

    Some((pieces + 1, counting_limit(state, material)))
}

/// extinct_outcome
///
/// Finds a material extinction end. For each colour, it adds the
/// `piece_count` of the set pieces. The rule fires when the sum is at or
/// below the threshold.
///
/// Params:
/// - state: &State             -> position to scan
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) if it fires
///
/// Notes:
/// `position_terminal` calls it only after a capture or a promotion, or
/// when there is no last move. Only these can decrease a piece count.
///
pub fn extinct_outcome(state: &State) -> Option<(u8, Outcome, &str)> {
    for extinct in &state.termination.extinct {
        for color in [WHITE, BLACK] {
            let mut count = 0u32;

            for (index, piece) in state.statics.pieces.iter().enumerate() {
                if extinct.set[index] && p_color!(piece) == color {
                    count += state.piece_count[index];
                }
            }

            if count <= extinct.threshold as u32 {
                return Some((color, extinct.outcome, &extinct.name));
            }
        }
    }

    None
}

/// goal_outcome
///
/// Finds a goal zone end. A colour with a goal piece on a zone square gets
/// the outcome. The function scans `mover` first, so the side that moved
/// wins when the two colours qualify.
///
/// Params:
/// - state: &State             -> position to scan
/// - mover: u8                 -> colour that moved, scanned first
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) if reached
///
/// Notes:
/// It scans the two colours. A FEN load or an unload can put a piece in
/// the zone without a move of that colour.
///
pub fn goal_outcome(state: &State, mover: u8) -> Option<(u8, Outcome, &str)> {
    let goal = state.termination.goal.as_ref()?;

    for color in [mover, 1 - mover] {
        for (index, piece) in state.statics.pieces.iter().enumerate() {
            if goal.set[index] && p_color!(piece) == color {
                for &square in piece_squares!(state, index) {
                    if get!(goal.zone, square as u32) {
                        return Some((color, goal.outcome, &goal.name));
                    }
                }
            }
        }
    }

    None
}

/// adjudicate_outcome
///
/// Decides a position after two passes by points. The points of a colour
/// are the weight sum of its pieces plus its handicap. The larger sum wins
/// and equal sums draw.
///
/// Params:
/// - state: &State             -> position after the second pass
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (winner, Win, name) or (mover, Draw, name)
///
/// Notes:
/// Without an `adjudicate` rule it returns `None`, and the caller keeps the
/// double pass draw.
///
pub fn adjudicate_outcome(state: &State) -> Option<(u8, Outcome, &str)> {
    let adjudicate = state.termination.adjudicate.as_ref()?;

    let mut sums = adjudicate.handicap;
    for (index, &count) in state.piece_count.iter().enumerate() {
        let color = p_color!(state.statics.pieces[index]) as usize;
        sums[color] += adjudicate.weights[index] * count as i32;
    }

    let (winner, outcome) = if sums[WHITE as usize] > sums[BLACK as usize] {
        (WHITE, Outcome::Win)
    } else if sums[BLACK as usize] > sums[WHITE as usize] {
        (BLACK, Outcome::Win)
    } else {
        (state.playing, Outcome::Draw)
    };

    Some((winner, outcome, &adjudicate.name))
}

/// position_terminal
///
/// Tests the position rules after the last move. The first rule that fires
/// gives the result, in this order:
///
/// 1. `checks`, the N-th check
/// 2. `goal`
/// 3. `extinct`
/// 4. double pass or accepted stand-off, with `adjudicate` if declared
/// 5. `counting`
/// 6. `counter`
///
/// `game_outcome` calculates repetition and perpetual. The side to move is
/// never the subject here, because it did not make the last move.
///
/// Params:
/// - state: &State             -> position after the last move
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) if it fires
///
/// Notes:
/// After a FEN load there is no last move. Then only `goal`, `extinct`,
/// `counting` and `counter` can fire. The other rules need move history.
///
pub fn position_terminal(state: &State) -> Option<(u8, Outcome, &str)> {
    let last = state.history.last();
    let accepted_stand_off = last.is_some_and(|snapshot| {
        pass_snapshot!(snapshot) && snapshot.in_stand_off == Some(true)
    });
    let double_pass = last.is_some_and(|snapshot| pass_snapshot!(snapshot))
        && state.history.len() >= 2
        && pass_snapshot!(state.history[state.history.len() - 2]);
    let retypes = last.is_none_or(|snapshot| {
        let move_type = move_type!(snapshot.move_ply);

        move_type == SINGLE_CAPTURE_MOVE
            || move_type == MULTI_CAPTURE_MOVE
            || promotion!(snapshot.move_ply)
    });
    let mover = 1 - state.playing;

    if let Some(checks) = &state.termination.checks
        && last.is_some_and(|snapshot| snapshot.in_check == Some(true))
        && checks.delivered[mover as usize] >= checks.count
    {
        Some((mover, checks.outcome, &checks.name))
    } else if let Some(hit) = goal_outcome(state, mover) {
        Some(hit)
    } else if !state.termination.extinct.is_empty()
        && retypes
        && let Some(hit) = extinct_outcome(state)
    {
        Some(hit)
    } else if double_pass || accepted_stand_off {
        Some(
            adjudicate_outcome(state).unwrap_or(
                (state.playing, Outcome::Draw, "")
            ),
        )
    } else if let Some(counting) = &state.termination.counting
        && let Some((count, limit)) = counting.progress
        && count >= limit
    {
        let material = if side_is_bare(state, WHITE) { BLACK } else { WHITE };

        Some((material, counting.outcome, &counting.name))
    } else if let Some(counter) = &state.termination.counter
        && counter.clock >= counter.limit
    {
        Some((mover, counter.outcome, &counter.name))
    } else {
        None
    }
}

/*----------------------------------------------------------------------------*\
                            REPETITION AND PERPETUAL
\*----------------------------------------------------------------------------*/

/// offence_set
///
/// Finds the offences of one ply by the side that moved:
///
/// - check : the royal of the side to move is attacked, also by discovery
/// - chase : enemy pieces, not royal, attacked by a chaser and undefended
///
/// Params:
/// - state: &State -> position after the ply, the target side to move
/// - mover: u8     -> colour that moved, the possible offender
///
/// Return:
/// (bool, Board)   -> (mover gave check, undefended chased squares)
///
/// Notes:
/// The chase board is empty without a perpetual chase rule.
///
pub fn offence_set(state: &State, mover: u8) -> (bool, Board) {
    let quarry = 1 - mover;                                                     /* side to move after the mover's ply */
    let did_check = is_in_check!(quarry, state);

    let mut chase = board!(state.statics.files, state.statics.ranks);

    let Some(perpetual) = state.termination.perpetual.as_ref()
        .filter(|perpetual| perpetual.chase.is_some())
    else {
        return (did_check, chase);
    };
    let chasers = &perpetual.chasers;

    for index in 0..state.statics.pieces.len() {
        let piece = &state.statics.pieces[index];
        if p_color!(piece) != quarry || p_is_royal!(piece) {
            continue;
        }

        let target_rank = p_rank!(piece);

        for &square in piece_squares!(state, index) {
            let unmoved = get!(state.virgin_board, square as u32);

            let attackers = &state.statics.relevant_attacks
                [quarry as usize][square as usize];
            let attacked = attackers.iter().any(
                |(piece_index, start, move_vector)| {
                    chasers[*piece_index as usize]
                        && state.main_board[*start as usize] == *piece_index
                        && validate_attack_vector!(
                            move_vector,
                            *start,
                            &state.statics.pieces[*piece_index as usize],
                            unmoved,
                            false,
                            target_rank,
                            square as u32,
                            state
                        )
                }
            );

            if attacked && !is_square_attacked!(
                square as u32, mover, unmoved, false, target_rank, state
            ) {
                set!(chase, square as u32);
            }
        }
    }

    (did_check, chase)
}

/// perpetual_offender
///
/// Finds the only offender of a repetition cycle that just closed. The
/// function walks back from the last move to the previous occurrence of
/// the position:
///
/// - perpetual checker : gave check on all its cycle moves
/// - perpetual chaser  : chased the same undefended piece on all of them
///
/// Check is before chase. If the two colours offend, there is no offender.
/// A cycle move is not a capture or a drop, so the function follows the
/// chased piece through each undone quiet move. `cap` limits the scan:
///
/// ```text
///   0              floor              start           plies
///   ├──── unread ────┼──── searched ────┼──── cycle ────┤
///                                       └ same position, one occurrence ago
/// ```
///
/// The result is the outcome that the rule declares for the offender. For
/// example, `check loss` makes the checker lose.
///
/// Params:
/// - state: &mut State   -> position after the closing move, restored
/// - cap  : usize        -> scan limit, `usize::MAX` for the real game
///
/// Return:
/// Option<(u8, Outcome)> -> the only offender and its result, if any
///
/// Notes:
/// The walk uses undo and redo. It restores `game_result` and `search_ply`
/// by hand, because `undo_move!` stops `search_ply` at zero. A cycle with a
/// null move gives `None`, because a null move is not a real move.
///
fn perpetual_offender(state: &mut State, cap: usize) -> Option<(u8, Outcome)> {
    let (check_outcome, chase_outcome) = {
        let perpetual = state.termination.perpetual.as_ref()?;
        (perpetual.check, perpetual.chase)
    };

    let hash = state.position_hash;
    let plies = state.history.len();
    let floor = plies.saturating_sub(cap);
    let start = (floor..plies).rev()
        .find(|&index| state.history[index].position_hash == hash)?;

    let passed = state.history[start..].iter()
        .any(|snapshot| snapshot.move_ply == null_move());

    if passed {
        return None;                                                            /* a passed turn is not a cycle move  */
    }

    let saved_result = state.termination.game_result;
    let saved_ply = state.search_ply;

    let mut check_all = [true; 2];
    let mut cycle_plies = [0u32; 2];
    let mut chase = [
        board!(state.statics.files, state.statics.ranks),
        board!(state.statics.files, state.statics.ranks),
    ];
    let mut chase_seen = [false; 2];
    let mut redo: Vec<Move> = Vec::new();
    let mut index = plies - 1;

    loop {
        let mover = (1 - state.playing) as usize;                               /* colour that made history[index]    */
        let (did_check, threats) = offence_set(state, mover as u8);
        cycle_plies[mover] += 1;
        check_all[mover] &= did_check;

        if chase_seen[mover] {
            and!(chase[mover], &threats);
        } else {
            chase[mover] = threats;
            chase_seen[mover] = true;
        }

        if index == start {
            break;
        }

        let cycle_move = state.history[index].move_ply.clone();
        undo_move!(state);

        let from = start!(cycle_move) as u32;                                   /* remap the quarry piece backward    */
        let to = end!(cycle_move) as u32;                                       /* through this quiet relocation      */
        let other = 1 - mover;
        if get!(chase[other], to) {
            clear!(chase[other], to);
            set!(chase[other], from);
        }

        redo.push(cycle_move);
        index -= 1;
    }

    for cycle_move in redo.iter().rev() {
        let replayed = make_move!(state, cycle_move.clone());

        debug_assert!(replayed, "cycle replay rejected a move it had made");
    }
    state.termination.game_result = saved_result;
    state.search_ply = saved_ply;

    let checker = |c: usize| cycle_plies[c] > 0 && check_all[c];
    let chaser = |c: usize| chase_seen[c] && !is_empty!(chase[c]);

    if let Some(outcome) = check_outcome {
        match (checker(WHITE as usize), checker(BLACK as usize)) {
            (true, false) => return Some((WHITE, outcome)),
            (false, true) => return Some((BLACK, outcome)),
            (true, true) => return None,                                        /* both check: repetition stands      */
            (false, false) => {}
        }
    }

    let chase_outcome = chase_outcome?;

    match (chaser(WHITE as usize), chaser(BLACK as usize)) {
        (true, false) => Some((WHITE, chase_outcome)),
        (false, true) => Some((BLACK, chase_outcome)),
        _ => None,
    }
}

/// repetition_scan_bound
///
/// Gives the number of last history entries that can have the current
/// hash. `cap` can make the number smaller.
///
/// - drop variant : the full history, a capture and a drop can repeat
/// - other        : the reversible ply clock of the repetition rule
///
/// Params:
/// - state: &State -> position with the history
/// - cap  : usize  -> scan limit, `usize::MAX` for the real game
///
/// Return:
/// usize           -> number of last history entries to scan
///
fn repetition_scan_bound(state: &State, cap: usize) -> usize {
    let length = state.history.len();
    let reversible = if drops!(state) {
        length
    } else {
        state.termination.repetition
            .as_ref().map_or(0, |repetition| repetition.clock as usize)
    };

    reversible.min(length).min(cap)
}

/// count_repetitions
///
/// Counts the occurrences of the current position, the current one
/// included, within `repetition_scan_bound`. Without a `repetition` rule,
/// the count is zero.
///
/// Params:
/// - state: &State -> current position
/// - cap  : usize  -> scan limit, `usize::MAX` for the real game
///
/// Return:
/// u8              -> occurrence count, current position included
///
/// Notes:
/// The scan stops at the last null move. A null move is only a search
/// tool, so a position before it is not a repetition.
///
pub fn count_repetitions(state: &State, cap: usize) -> u8 {
    if state.termination.repetition.is_none() {
        return 0;
    }

    let bound = repetition_scan_bound(state, cap);
    let start = state.history.len() - bound;
    let null = null_move();
    let matches = state.history[start..]
        .iter()
        .rev()
        .take_while(|snapshot| snapshot.move_ply != null)
        .filter(|snapshot| snapshot.position_hash == state.position_hash)
        .count();

    (matches + 1).min(u8::MAX as usize) as u8
}

/// repetition_outcome
///
/// Calculates the repetition or perpetual result. It gives `None` without
/// a `repetition` rule or with fewer than `min_count` occurrences.
///
/// The rule result is for the side that caused it: the side that closed
/// the repetition, or the only offender. The function converts it to the
/// view of the side to move, as `outcome_score!` and `game_outcome` need.
///
/// Params:
/// - state    : &mut State -> current position, restored after a walk
/// - min_count: u8         -> occurrences necessary to fire
/// - cap      : usize      -> scan limit, `usize::MAX` for the real game
///
/// Return:
/// Option<(Outcome, bool)> -> (outcome, true if perpetual) if it fires
///
/// Notes:
/// The game and the search both give the `occurrences` of the rule. Thus a
/// perpetual result comes at the same repetition as the rule, not before.
///
pub fn repetition_outcome(
    state: &mut State, min_count: u8, cap: usize,
) -> Option<(Outcome, bool)> {
    let neutral = state.termination.repetition.as_ref()?.outcome;

    let occurrences = count_repetitions(state, cap);
    if occurrences < min_count {
        return None;
    }

    let (subject, outcome, perpetual) = match perpetual_offender(state, cap) {
        Some((offender, offence)) => (offender, offence, true),
        None => (1 - state.playing, neutral, false),
    };

    let seen = if subject == state.playing {
        outcome
    } else {
        match outcome {
            Outcome::Draw => Outcome::Draw,
            Outcome::Win => Outcome::Loss,
            Outcome::Loss => Outcome::Win,
        }
    };

    Some((seen, perpetual))
}

/// game_outcome
///
/// Gives the real game result for output and self-play. It is
/// `game_result` if set, else the repetition or perpetual result. It also
/// gives the name of the rule, or `None` if the rule has no name.
///
/// Params:
/// - state: &mut State  -> current position, restored after a walk
///
/// Return:
/// (u8, Option<String>) -> (result, rule name), ONGOING if not ended
///
/// Notes:
/// The search does not use it. The search reads `is_terminal!`.
///
pub fn game_outcome(state: &mut State) -> (u8, Option<String>) {
    if state.termination.game_result != ONGOING {
        return (
            state.termination.game_result,
            position_terminal(state)
                .map(|(_, _, name)| name)
                .filter(|name| !name.is_empty())
                .map(str::to_string),
        );
    }

    let Some(occurrences) = state.termination.repetition
        .as_ref().map(|repetition| repetition.occurrences)
    else {
        return (ONGOING, None);
    };

    match repetition_outcome(state, occurrences, usize::MAX) {
        Some((outcome, perpetual)) => {
            let rule = if perpetual {
                state.termination.perpetual.as_ref().map(|p| &p.name)
            } else {
                state.termination.repetition.as_ref().map(|r| &r.name)
            };
            (resolve_outcome!(state.playing, outcome), rule.cloned())
        }
        None => (ONGOING, None),
    }
}
