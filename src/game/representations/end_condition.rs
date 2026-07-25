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
/// drops always reset it. Its running value is `State::halfmove_clock`. This
/// is the 50-move family: standard chess, and any variant with a limit and a
/// set of progress pieces.
pub struct Counter {
    pub limit: u8,                                                              /* halfmoves before the outcome fires */
    pub reset_pieces: Vec<bool>,                                                /* moving these resets the counter    */
    pub outcome: Outcome,                                                       /* result once the limit is reached   */
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
    pub counter: Option<Counter>,                                              /* progress counter, if declared      */
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
        match $outcome {
            Outcome::Draw => DRAW,
            Outcome::Win => {
                if $state.playing == WHITE { WHITE_WIN } else { BLACK_WIN }
            }
            Outcome::Loss => {
                if $state.playing == WHITE { BLACK_WIN } else { WHITE_WIN }
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
