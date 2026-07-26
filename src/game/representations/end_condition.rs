//! end_condition.rs
//!
//! Defines the parametric terminal rules a variant can declare.
//!
//! A variant is won, lost, or drawn by more than checkmate: a threefold
//! repetition, a progress counter running out, a king reaching a goal zone,
//! a side going extinct. Rather than hard-code each as a named mode, the
//! engine keeps one flat table of `(detector -> Outcome)` rules; the config
//! supplies the parameters. This file is that table and the small vocabulary
//! it is built from: the `Outcome` a rule produces and the `Counter` a
//! progress rule counts with. Detectors live in the moves and search modules
//! and only consult a rule when its present-flag (an `Option` being `Some`)
//! is set, so a variant that declares nothing extra pays nothing.
//!
//! Created: 26/07/2026
//! Author : Alden Luthfi

use crate::game::representations::board::Board;

/*----------------------------------------------------------------------------*\
                               OUTCOME AND COUNTER
\*----------------------------------------------------------------------------*/

/// Outcome
///
/// The result an end condition produces when it fires, named from the
/// perspective of the side the condition is evaluated against (the side to
/// move at the terminal position). `resolve_outcome!` maps it to an absolute
/// `game_result`; `outcome_score!` maps it to a side-to-move search score.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Outcome {
    Draw,                                                                       /* neither side wins                  */
    Win,                                                                        /* the evaluated side wins            */
    Loss,                                                                       /* the evaluated side loses           */
}

/// Counter
///
/// A generic progress counter that draws (or otherwise resolves) once it
/// reaches its limit without a resetting move. `reset_pieces[i]` marks the
/// piece indices whose non-capturing moves reset the counter; captures and
/// drops always reset it. Its running value is `State::halfmove_clock`. When
/// `material` is set, the limit is only enforced once the total piece count on
/// the board has fallen to that threshold or below -- the makruk counting
/// family, where a bare-king endgame must be won within a move budget. This is
/// otherwise the 50-move family: standard chess, and any variant with a limit
/// and a set of progress pieces.
pub struct Counter {
    pub limit: u8,                                                              /* halfmoves before the outcome fires */
    pub reset_pieces: Vec<bool>,                                                /* moving these resets the counter    */
    pub outcome: Outcome,                                                       /* result once the limit is reached   */
    pub material: Option<u32>,                                                  /* enforce only when pieces <= this   */
}

/// Extinct
///
/// A material-extinction rule: when a colour's count of the pieces in `set`
/// falls to `threshold` or below, that colour receives `outcome` (or its
/// opponent, when `opponent` is set). `set[i]` marks the piece indices the
/// rule counts, matched per colour. Covers kinglet and horde (a side's pawns
/// or whole army gone -> loss) and antichess/losers (own army gone -> win).
pub struct Extinct {
    pub set: Vec<bool>,                                                         /* piece indices the rule counts      */
    pub threshold: u8,                                                          /* count at or below which it fires   */
    pub outcome: Outcome,                                                       /* result for the extinct colour      */
    pub opponent: bool,                                                         /* outcome applies to the other side  */
}

/// Goal
///
/// A goal-zone rule: when a colour lands one of its `set` pieces on a square
/// of `zone`, that colour receives `outcome`. Covers king-of-the-hill (king
/// to the centre) and racing kings (king to the far rank). The zone is a
/// single board shared by both colours.
pub struct Goal {
    pub set: Vec<bool>,                                                         /* piece indices that reach the zone  */
    pub zone: Board,                                                            /* target squares                     */
    pub outcome: Outcome,                                                       /* result for the arriving colour     */
}

/// Adjudicate
///
/// A points rule: when the game is agreed ended (both sides pass in
/// succession), the position is decided by weighted material rather than
/// drawn. `weights[i]` is a piece index's point value; each colour's counted
/// pieces are summed, `handicap[colour]` is added, and the greater sum wins
/// (a tie draws). Covers janggi points, where the second player carries a
/// standing handicap. The weights and handicap come from a named
/// `= adjudicate <name> =` config section, never a variant name in code.
pub struct Adjudicate {
    pub weights: Vec<i32>,                                                      /* per piece index point value        */
    pub handicap: [i32; 2],                                                     /* per colour standing point bonus    */
}

/// Perpetual
///
/// A cycle-offence rule: when a repetition closes, one side may be the sole
/// aggressor sustaining it, and rather than the neutral repetition outcome
/// that side receives its offence's `Outcome`. Two offences are recognised:
/// `check`, delivering a check on every one of the offender's cycle moves,
/// and `chase`, keeping the same enemy non-royal piece under an undefended
/// capture threat on every one of them; `chasers` marks the piece indices
/// whose threats count as a chase (the non-exempt attackers). Check outranks
/// chase, and when both sides offend the repetition outcome stands. This
/// refines the repetition rule, firing only once its occurrence bound is met.
/// Covers xiangqi, which forbids perpetual check and perpetual chase.
pub struct Perpetual {
    pub check: Option<Outcome>,                                                 /* sole perpetual checker's result    */
    pub chase: Option<Outcome>,                                                 /* sole perpetual chaser's result     */
    pub chasers: Vec<bool>,                                                     /* piece indices that commit a chase  */
}

/*----------------------------------------------------------------------------*\
                                 END CONDITIONS
\*----------------------------------------------------------------------------*/

/// EndConditions
///
/// The flat, directly-named terminal-rule table for one variant, shared
/// immutably through `StaticState`. `checkmate` and `stalemate` name the
/// outcome of a no-legal-move position (in check / not in check); the
/// remaining fields are `Option`s that double as present-flags, `Some` only
/// when the variant's `= end conditions =` section declared the rule.
pub struct EndConditions {
    pub checkmate: Outcome,                                                     /* no moves + in check   (dflt Loss)  */
    pub stalemate: Outcome,                                                     /* no moves, not in check(dflt Draw)  */

    pub repetition: Option<(u8, Outcome)>,                                      /* (Nth occurrence, outcome)          */
    pub counter: Option<Counter>,                                               /* progress counter, if declared      */

    pub checks: Option<(u8, Outcome)>,                                          /* (Nth check, checker's outcome)     */
    pub extinct: Vec<Extinct>,                                                  /* material-extinction rules          */
    pub goal: Option<Goal>,                                                     /* goal-zone rule                     */
    pub perpetual: Option<Perpetual>,                                           /* repetition-cycle offence rule      */
    pub adjudicate: Option<Adjudicate>,                                         /* points decision on double pass     */
}

impl Default for EndConditions {
    /// EndConditions::default
    ///
    /// The behaviour every variant starts from before its `= end conditions =`
    /// section is read: a no-move position is a loss when in check (checkmate)
    /// and a draw otherwise (stalemate), with no repetition or counter rule.
    fn default() -> Self {
        EndConditions {
            checkmate: Outcome::Loss,
            stalemate: Outcome::Draw,
            repetition: None,
            counter: None,
            checks: None,
            extinct: Vec::new(),
            goal: None,
            perpetual: None,
            adjudicate: None,
        }
    }
}

/// resolve_outcome!
///
/// Maps an [`Outcome`] evaluated against the side to move to an absolute
/// `game_result` constant. A win for the side to move is a win for its
/// colour; a loss is a win for the opponent's colour.
///
/// Params:
/// - state  : &State  -> position whose side to move anchors the perspective
/// - outcome: Outcome -> the result to resolve
///
/// Return:
/// u8                 -> DRAW / WHITE_WIN / BLACK_WIN
#[macro_export]
macro_rules! resolve_outcome {
    ($state:expr, $outcome:expr) => {
        resolve_outcome_for!($state.playing, $outcome)
    };
}

/// resolve_outcome_for!
///
/// Maps an [`Outcome`] evaluated against a named colour to an absolute
/// `game_result`. Use this when the outcome's subject is not the side to
/// move — e.g. a check-count rule resolved from the mover's perspective,
/// where the mover is the opponent of the side now to move.
///
/// Params:
/// - color  : u8      -> the colour the outcome is named against
/// - outcome: Outcome -> the result to resolve
///
/// Return:
/// u8                 -> DRAW / WHITE_WIN / BLACK_WIN
#[macro_export]
macro_rules! resolve_outcome_for {
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
            Outcome::Win => INF - $state.search_ply as i32,
            Outcome::Loss => -INF + $state.search_ply as i32,
        }
    };
}
