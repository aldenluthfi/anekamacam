//! termination.rs
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
/// A progress counter that resolves once it reaches `limit` halfmoves without a
/// resetting move. `reset_pieces[i]` marks the piece indices whose quiet moves
/// reset it; captures and drops always reset it. Its running value is
/// `State::halfmove_clock`.
pub struct Counter {
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
/// value is a frozen clock in `State::counting`: it starts at the piece count
/// when the bare king arises and climbs by one per ply, never resetting on a
/// capture. `table` is an ordered list of `(requirements, limit)` rows, the
/// first fully-matched winning; a requirement is a piece set and the minimum
/// number the material side must own. `default` applies when no row matches.
pub struct Counting {
    pub table: Vec<(Vec<(Vec<bool>, u32)>, u16)>,                               /* ordered (requirements, limit) rows */
    pub default: u16,                                                           /* limit when no row matches          */
    pub outcome: Outcome,                                                       /* result once the count hits limit   */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Extinct
///
/// A material-extinction rule: when a colour's count of the pieces in `set`
/// falls to `threshold` or below, that colour receives `outcome` (or its
/// opponent, when `opponent` is set). `set[i]` marks the counted piece indices,
/// matched per colour.
pub struct Extinct {
    pub set: Vec<bool>,                                                         /* piece indices the rule counts      */
    pub threshold: u8,                                                          /* count at or below which it fires   */
    pub outcome: Outcome,                                                       /* result for the extinct colour      */
    pub opponent: bool,                                                         /* outcome applies to the other side  */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Goal
///
/// A goal-zone rule: when a colour lands one of its `set` pieces on a square of
/// `zone`, that colour receives `outcome`. The zone is one board shared by both
/// colours.
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
/// the result for a sole aggressor.
pub struct Repetition {
    pub occurrences: u8,                                                        /* occurrences that trigger the rule  */
    pub outcome: Outcome,                                                       /* result once reached                */
    pub name: String,                                                           /* reason reported when it fires      */
}

/// Checks
///
/// An N-check rule: the side delivering its `count`-th check receives
/// `outcome`, scored against that side.
pub struct Checks {
    pub count: u8,                                                              /* checks before the outcome fires    */
    pub outcome: Outcome,                                                       /* result for the checking side       */
    pub name: String,                                                           /* reason reported when it fires      */
}

/*----------------------------------------------------------------------------*\
                                 END CONDITIONS
\*----------------------------------------------------------------------------*/

/// Termination
///
/// The flat, directly-named terminal-rule table for one variant, shared
/// immutably through `StaticState`. `checkmate` and `stalemate` name the
/// outcome of a no-legal-move position (in check / not in check); the
/// remaining fields are `Option`s that double as present-flags, `Some` only
/// when the variant's `= termination =` section declared the rule.
pub struct Termination {
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
            Outcome::Win => INF - $state.search_ply as i32,
            Outcome::Loss => -INF + $state.search_ply as i32,
        }
    };
}
