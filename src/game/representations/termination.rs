//! termination.rs
//!
//! The terminal-rule table a variant declares, and the detectors that apply it.
//!
//! One flat `Termination` table holds every terminal rule as an `Option` that
//! is `Some` only when the config declares it, so a variant pays only for the
//! rules it uses. `position_terminal` checks the eager, position-local rules
//! after a move; `game_outcome` adds the on-demand repetition and perpetual
//! verdicts, computed from history rather than stored.
//!
//! Created: 26/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                OUTCOME AND RULES
\*----------------------------------------------------------------------------*/

/// Outcome
///
/// The result an end condition produces when it fires, named from the
/// perspective of the side that rule is about. **Which side that is belongs to
/// the rule, not to this type**: each rule's own doc names its subject, and
/// `position_terminal` returns that colour alongside the outcome. There is no
/// convention covering all of them, because three rules name a colour computed
/// from the position rather than read off the turn order -- `extinct` names
/// whichever colour ran out, `adjudicate` the points winner, `perpetual` the
/// sole offender.
///
/// `resolve_outcome!` maps an outcome plus its subject to an absolute
/// `game_result`; `outcome_score!` maps an outcome already named against the
/// side to move to a search score.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Outcome {
    Draw,                                                                       /* neither side wins                  */
    Win,                                                                        /* the evaluated side wins            */
    Loss,                                                                       /* the evaluated side loses           */
}

/// Counter
///
/// A progress counter that resolves once it reaches `limit` halfmoves without a
/// resetting move. `reset_pieces[i]` marks the piece indices whose quiet moves
/// reset it; captures and drops always reset it. Its running value is
/// `Counter::clock`.
///
/// Subject: the mover whose move reached the limit. The rule is symmetric, so
/// this matters only to a variant declaring a non-draw outcome.
#[derive(Clone)]
pub struct Counter {
    pub clock: u8,                                                              /* current reversible halfmove count */
    pub limit: u8,                                                              /* halfmoves before the outcome fires */
    pub reset_pieces: Vec<bool>,                                                /* moving these resets the counter    */
    pub outcome: Outcome,                                                       /* result once the limit is reached   */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Counting
///
/// A material-scaled move budget for a bare-king endgame. When exactly one side
/// is reduced to a lone royal, the side with material must mate within a limit
/// set by its strongest material or the game resolves to `outcome`. The running
/// value is a frozen `progress` clock: it starts at the piece count when the
/// bare king arises and climbs by one per ply, never resetting on a capture.
/// `table` is an ordered list of `(requirements, limit)` rows, the first
/// fully-matched winning; a requirement is a piece set and the minimum number
/// the material side must own. `default` applies when no row matches.
///
/// Subject: the material side, i.e. the colour that is not bare -- the one
/// that failed to mate inside the budget. Not the mover, whose identity at the
/// expiring ply is only parity.
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
/// A material-extinction rule: when a colour's count of the pieces in `set`
/// falls to `threshold` or below, that colour receives `outcome`. `set[i]`
/// marks the counted piece indices, matched per colour.
///
/// Subject: the colour that ran out, which need not be the mover -- a capture
/// empties the opponent's set, a promotion can empty your own. A rule meant to
/// end the game for the other side is the same rule with `win` and `loss`
/// swapped, so no opponent-facing flag is carried.
#[derive(Clone)]
pub struct Extinct {
    pub set: Vec<bool>,                                                         /* piece indices the rule counts      */
    pub threshold: u8,                                                          /* count at or below which it fires   */
    pub outcome: Outcome,                                                       /* result for the extinct colour      */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Goal
///
/// A goal-zone rule: when a colour lands one of its `set` pieces on a square of
/// `zone`, that colour receives `outcome`. The zone is one board shared by both
/// colours.
///
/// Subject: the colour whose piece stands in the zone.
#[derive(Clone)]
pub struct Goal {
    pub set: Vec<bool>,                                                         /* piece indices that reach the zone  */
    pub zone: Board,                                                            /* target squares                     */
    pub outcome: Outcome,                                                       /* result for the arriving colour     */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Adjudicate
///
/// A points rule: when both sides pass in succession the position is decided by
/// weighted material rather than drawn. `weights[i]` is a piece index's point
/// value; each colour's counted pieces are summed, `handicap[colour]` added,
/// and the greater sum wins (a tie draws). Weights and handicap come from a
/// named `= adjudicate <name> =` config section.
///
/// Subject: the colour with the greater sum, or the side to move on a tie,
/// where the outcome is `Draw` and the subject cannot matter.
#[derive(Clone)]
pub struct Adjudicate {
    pub weights: Vec<i32>,                                                      /* per piece index point value        */
    pub handicap: [i32; 2],                                                     /* per colour standing point bonus    */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Perpetual
///
/// A cycle-offence rule refining `repetition`: when a repetition closes, a sole
/// aggressor sustaining it receives its offence's `Outcome` instead of the
/// neutral repetition result. `check` is delivering a check on every one of the
/// offender's cycle moves; `chase` is keeping the same enemy non-royal piece
/// under an undefended capture threat on every one of them, with `chasers`
/// marking the piece indices whose threats count. Check outranks chase; when
/// both sides offend the repetition result stands.
///
/// Subject: the sole offender. Each offence carries its own `Outcome`, and
/// declaring one at all is what enables that offence -- `check draw` leaves
/// the cycle drawn rather than losing it for the checker.
#[derive(Clone)]
pub struct Perpetual {
    pub check: Option<Outcome>,                                                 /* sole perpetual checker's result    */
    pub chase: Option<Outcome>,                                                 /* sole perpetual chaser's result     */
    pub chasers: Vec<bool>,                                                     /* piece indices that commit a chase  */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Repetition
///
/// A repeated-position rule: the game resolves to `outcome` once the current
/// position has occurred `occurrences` times. A `Perpetual` rule may override
/// the result for a sole aggressor. Occurrences are read from the position
/// hash map this stage still maintains.
///
/// Subject: the mover that closed the repetition. Symmetric like `counter`, so
/// it matters only to a variant declaring a non-draw outcome.
#[derive(Clone)]
pub struct Repetition {
    pub occurrences: u8,                                                        /* occurrences that trigger the rule  */
    pub outcome: Outcome,                                                       /* result once reached                */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Checks
///
/// An N-check rule: the side delivering its `count`-th check receives
/// `outcome`, scored against that side.
///
/// Subject: the checking side, i.e. the mover.
#[derive(Clone)]
pub struct Checks {
    pub delivered: [u8; 2],                                                     /* checks delivered per colour        */
    pub count: u8,                                                              /* checks before the outcome fires    */
    pub outcome: Outcome,                                                       /* result for the checking side       */
    pub name: String,                                                           /* reason reported when it fires      */
}

/*----------------------------------------------------------------------------*\
                                TERMINATION TABLE
\*----------------------------------------------------------------------------*/

/// Termination
///
/// The flat, directly-named terminal-rule table and mutable progress for one
/// position. `checkmate` and `stalemate` name no-legal-move outcomes; remaining
/// rule fields are `Some` only when declared by the variant. Runtime result and
/// progress reset between games while configured rules remain intact.
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
    /// The behaviour every variant starts from before its `= termination =`
    /// section is read: a no-move position is a loss when in check (checkmate)
    /// and a draw otherwise (stalemate), with no repetition or counter rule.
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
    /// Clears eager result and mutable rule progress while preserving every
    /// configured rule, threshold, outcome, and reported name.
    pub fn reset_progress(&mut self) {
        self.game_result = ONGOING;

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
/// Maps an [`Outcome`] named against a colour to an absolute `game_result`.
/// Pass the colour the outcome is scored against (the side to move for a
/// stalemate-style rule, or the mover for a check-count rule).
///
/// Params:
/// - color  : u8      -> the colour the outcome is named against
/// - outcome: Outcome -> the result to resolve
///
/// Return:
/// u8                 -> DRAW / WHITE_WIN / BLACK_WIN
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
/// Maps an [`Outcome`] to a side-to-move search score: a draw yields the
/// contempt-adjusted `draw_score!`, a win `+INF - ply`, a loss `-INF + ply`,
/// matching the checkmate scale so shorter wins and longer losses are
/// preferred.
///
/// Params:
/// - state  : &State  -> position whose ply and draw value are read
/// - outcome: Outcome -> the result to score
///
/// Return:
/// i32                -> terminal score from the side to move's perspective
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

/*----------------------------------------------------------------------------*\
                                   DETECTORS
\*----------------------------------------------------------------------------*/

/// side_is_bare
///
/// Whether a colour has been reduced to a single royal piece. Existing major
/// and minor counters jointly cover every non-royal piece class, and the royal
/// count comes from `royal_list`, which make/undo maintain on every move type
/// and `verify_game_state` recomputes.
///
/// The royal count stays pinned at exactly one. Whether a bare-king rule
/// should also accept a colour holding two royals is a rules question this
/// does not answer -- `janggi.conf` declares `royal: KQkq`, so it is a real
/// one.
///
/// Params:
/// - state: &State -> position to inspect
/// - side : u8     -> the colour tested
///
/// Return:
/// bool            -> true when the colour has no non-royal pieces left
pub fn side_is_bare(state: &State, side: u8) -> bool {
    state.major_pieces[side as usize] == 0 &&
    state.minor_pieces[side as usize] == 0 &&
    state.royal_list[side as usize].len() == 1
}

/// counting_limit
///
/// The pieces'-honour move budget for the material side of a bare-king endgame:
/// the first `Counting` table row whose requirements the `winner` meets, else
/// the table default. A requirement is met when the winner owns at least its
/// minimum number of the requirement's piece set. Returns 0 if the variant
/// declares no counting rule (never reached, since the caller gates on it).
///
/// Params:
/// - state : &State -> position to inspect
/// - winner: u8     -> the colour holding material (opponent of the bare king)
///
/// Return:
/// u16              -> the frozen count limit for this material
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
/// The bare-king count after one ply: `None` while neither or both colours are
/// bare, the previous count plus one while the situation holds, and a fresh
/// `(piece total + 1, frozen limit)` when it first arises. The limit freezes at
/// the material present when counting starts and does not move again while
/// `last` keeps feeding back.
///
/// Only `make_move!` calls this, once per ply. A parsed FEN deliberately
/// does not seed it: the *condition* that starts a count is a position fact,
/// but how far the count has run is a history fact the FEN does not carry, so
/// seeding one at load would invent a number. Counting therefore begins on the
/// first move played from a loaded position, exactly as it did before.
///
/// Params:
/// - state: &State            -> position to inspect
/// - last : Option<(u16,u16)> -> the previous ply's progress
///
/// Return:
/// Option<(u16, u16)>         -> (count, frozen limit) while counting applies
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
/// Detects a material-extinction terminal. For each colour it sums
/// `piece_count` over the rule's set pieces of that colour and fires when the
/// count is at or below the threshold, naming the extinct colour.
///
/// This reads the position alone. `position_terminal` decides when it is worth
/// running: after a move that can drop a colour's count of a piece type, or at
/// a position with no move behind it at all. Only two move kinds retype --
/// a capture removes the piece outright, and a promotion retypes it, since
/// `piece_list_push!` takes the promoted index while `piece_list_remove!` took
/// the original. A drop only ever adds, and castling relocates two pieces of
/// types it does not change.
///
/// A rule that ends the game for the other side is the same rule with `win`
/// and `loss` swapped, so no opponent-facing flag is carried.
///
/// Params:
/// - state: &State             -> position to scan
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) when it fires
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
/// Detects a goal-zone terminal: a colour whose goal-set piece stands on a zone
/// square receives the rule's outcome. `mover` is scanned first so that when
/// both colours somehow qualify the side that just moved is credited.
///
/// Scanning both colours rather than only the mover covers the two cases where
/// a piece can be found on the zone without the last move having put it there:
/// a position loaded from a FEN, which has no last move at all, and a capture
/// whose unload or a castling partner relocates the *enemy* piece onto it.
///
/// Params:
/// - state: &State             -> position to scan
/// - mover: u8                 -> the colour that just moved, scanned first
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) when reached
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
/// Decides a passed-out position by weighted material. Each colour's counted
/// pieces are multiplied by their configured point weight and summed with that
/// colour's standing handicap; the greater sum wins and an equal sum draws.
/// Returns None when the variant declares no `adjudicate` rule, so the caller
/// keeps the neutral double-pass draw.
///
/// Params:
/// - state: &State             -> position at the second successive pass
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (winner, Win, name) / (mover, Draw, name)
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
/// The eager, position-local terminal check for the last move in history: the
/// first of accepted stand-off, N-check, goal, extinction, double-pass,
/// counting, and counter rules to fire, with the name and subject colour.
/// Repetition and perpetual are computed on demand by `game_outcome`. `None`
/// when no move has been made.
///
/// Each rule names its own subject, documented on the rule: `checks` and
/// `goal` name the mover, `extinct` the colour that ran out, `adjudicate` the
/// points winner, `counting` the material side, `counter` the mover whose move
/// reached the limit. The side to move is never a subject here -- it has not
/// moved, so it cannot have set off a rule the last move triggered.
///
/// A position with no move behind it -- one just parsed from a FEN -- is still
/// tested for the rules that read the position alone: `goal`, `extinct`,
/// `counting` and `counter`. `checks`, the double pass and the accepted
/// stand-off stay history-dependent and simply do not fire, because "the Nth
/// check was just delivered" and "both sides passed" are facts about moves and
/// a FEN does not carry them.
///
/// Params:
/// - state: &State             -> position after the ending move, if any
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) when firing
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

/// offence_set
///
/// The per-ply offence the mover commits against the side to move: a check (the
/// quarry's royal is attacked, full-board so discovered checks count) and the
/// enemy non-royal pieces it chases (attacked by a non-exempt piece and left
/// undefended). A check is the royal case of the same attacked-and-undefended
/// predicate, so both come from one read-only pass. The chase board is empty
/// unless a perpetual-chase rule is declared.
///
/// Params:
/// - state: &State -> position after the mover's ply (quarry is side to move)
/// - mover: u8     -> the colour that just moved (the potential offender)
///
/// Return:
/// (bool, Board)   -> (mover gave check, undefended enemy chase squares)
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
/// Adjudicates a just-closed repetition cycle for a sole aggressor. Walks the
/// plies from the previous occurrence of the current position to the closing
/// move: a colour is a perpetual checker if it checked on all of its cycle
/// moves, and a perpetual chaser if it kept the same enemy piece under an
/// undefended chase on all of them. The chased piece is tracked by identity
/// (its square remapped through each undone quiet move) since a cycle move is
/// capture-free and drop-free. Check outranks chase; when both colours share
/// an offence none is sole. The walk uses undo/redo and restores the position
/// exactly, including `game_result` and `search_ply`.
///
/// `search_ply` needs restoring by hand because `undo_move!` saturates it at
/// zero while `make_move!` counts up without a floor: a cycle reaching back
/// past the search root would otherwise leave the counter inflated by however
/// many plies the walk clipped, and every node below would read its ply, and so
/// its PV row, from the wrong slot.
///
/// A span holding a search null move is rejected outright: a passed turn is not
/// a move a cycle can be made of, and its snapshot has nothing to undo.
///
/// The reported result is the one the offending rule declares, named against
/// the offender: `perpetual: check loss` loses for the perpetual checker,
/// `check draw` leaves the cycle drawn. Each offence carries its own result,
/// and being declared at all is what enables it.
///
/// Params:
/// - state: &mut State  -> position after the cycle-closing move (restored)
///
/// Return:
/// Option<(u8, Outcome)> -> sole offender and its declared result, if any
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

/// repetition_outcome
///
/// The on-demand repetition/perpetual terminal, computed rather than stored:
/// `None` unless the variant declares a `repetition` rule and the current
/// position has occurred at least `min_count` times. The bool is true when a
/// perpetual offender decided it (for reason reporting).
///
/// Both results are declared against the player who triggered them -- the
/// mover who closed the repetition, or the sole offender who sustained the
/// cycle -- and are mirrored here into the side to move's view, which is the
/// footing `outcome_score!` and `game_outcome` both read them on. A draw
/// mirrors onto itself, so every shipped variant lands on the same result
/// either way.
///
/// Both game truth and search pass the rule's own `occurrences`, so a perpetual
/// verdict lands on exactly the repetition the rule names and not a ply sooner.
/// Search falls back to the twofold draw below that count: repeating a position
/// costs nothing, but an offence the rule has not yet recognised is not a
/// terminal and must not be scored as one.
///
/// Params:
/// - state    : &mut State -> current position (restored if a walk runs)
/// - min_count: u8         -> occurrences required before it fires
///
/// Return:
/// Option<(Outcome, bool)> -> (outcome, perpetual decided it) when it fires
pub fn repetition_outcome(
    state: &mut State, min_count: u8, cap: usize,
) -> Option<(Outcome, bool)> {
    let neutral = state.termination.repetition.as_ref()?.outcome;

    let root = state.history.first()
        .is_some_and(|snapshot| {
            snapshot.position_hash == state.position_hash
        });
    let occurrences = state.position_hash_map
        .get(&state.position_hash).copied().unwrap_or(0)
        .saturating_add(root as u8);
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

/// terminal_reason
///
/// The name of the eager rule that ended the current position, recomputed so no
/// reason need be stored. `None` when no eager rule applies (e.g. a
/// search-injected mate) or the rule is unnamed.
///
/// Params:
/// - state: &State -> position after the ending move
///
/// Return:
/// Option<String>  -> the fired rule's name, if any
pub fn terminal_reason(state: &State) -> Option<String> {
    position_terminal(state)
        .map(|(_, _, name)| name)
        .filter(|name| !name.is_empty())
        .map(str::to_string)
}

/// game_outcome
///
/// The single game-truth oracle for reporting and self-play paths: the eager,
/// position-local `game_result` when set, else the on-demand repetition /
/// perpetual verdict. Also reports the reason name of whichever rule decided
/// it. Search does not use this; it keeps the cheap `is_terminal!` read.
///
/// Params:
/// - state: &mut State  -> current position (restored if a walk runs)
///
/// Return:
/// (u8, Option<String>) -> (result, reason name) with result ONGOING when live
pub fn game_outcome(state: &mut State) -> (u8, Option<String>) {
    if state.termination.game_result != ONGOING {
        return (state.termination.game_result, terminal_reason(state));
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
