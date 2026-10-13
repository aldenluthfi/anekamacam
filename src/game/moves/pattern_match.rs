//! pattern_match.rs
//!
//! Macros that test CPMN patterns on a board position.
//!
//! A CPMN pattern tests the pieces on squares near one square. Drop rules,
//! stand-off rules and move conditions use these patterns. The macros here
//! test a compiled pattern on the current board and mirror the offsets for
//! each colour.
//!
//! Created: 24/02/2026
//! Author : Alden Luthfi

/*----------------------------------------------------------------------------*\
                            CPMN PATTERN EVALUATION
\*----------------------------------------------------------------------------*/

/// match_pattern!
///
/// Tests one compiled CPMN pattern at one square for one colour. A pattern
/// has two lists of `(offset, piece set)` pairs. Each allower must find one
/// of its pieces at its offset. No stopper can find one of its pieces.
///
/// The offsets are for White. For Black, the macro negates both deltas.
/// The macro reads the stoppers only if all allowers match. An offset off
/// the board holds no piece: an allower there fails the pattern, and a
/// stopper there is ignored, as `clip_pattern` fits a pattern at load.
///
/// Params:
/// - pattern: &Pattern -> compiled (allower, stopper) pattern to test
/// - square : u32      -> board square at the origin of the pattern
/// - color  : u8       -> colour that sets the offset direction
/// - state  : &State   -> current position
///
/// Return:
/// bool                -> true when all allowers match and no stopper does
///
#[macro_export]
macro_rules! match_pattern {
    ($pattern:expr, $square:expr, $color:expr, $state:expr) => {{
        let (allowers, stoppers) = $pattern;

        let files = $state.statics.files as i32;
        let ranks = $state.statics.ranks as i32;
        let file = ($square % files as u32) as i32;
        let rank = ($square / files as u32) as i32;
        let piece_at = |offset: u16| {
            let check_x = file + x!(offset) as i32 * (-2 * $color as i32 + 1);
            let check_y = rank + y!(offset) as i32 * (-2 * $color as i32 + 1);

            match check_x >= 0 && check_x < files
                && check_y >= 0 && check_y < ranks
            {
                true => Some(
                    $state.main_board[(check_y * files + check_x) as usize]
                ),
                false => None,
            }
        };

        allowers.iter().all(|allower| {
            piece_at(allower.0)
                .is_some_and(|piece_check| allower.1.contains(piece_check))
        })
        && !stoppers.iter().any(|stopper| {
            piece_at(stopper.0)
                .is_some_and(|piece_check| stopper.1.contains(piece_check))
        })
    }};
}

/// match_pattern_set!
///
/// Tests the condition of one move vector at the origin of the move. The
/// condition is a set of alternatives, so one match is sufficient. A vector
/// without a condition always matches.
///
/// Params:
/// - patterns: &Option<Arc<PatternSet>> -> condition of the move vector
/// - square  : u32                      -> origin square of the move
/// - color   : u8                       -> colour of the moving piece
/// - state   : &State                   -> current position
///
/// Return:
/// bool                                 -> true when the move can be played
///
/// Notes:
/// `match_pattern!` tests the bounds, so a condition fitted to its square
/// by `clip_move_vector` and the same condition before the fit give the
/// same answer.
///
#[macro_export]
macro_rules! match_pattern_set {
    ($patterns:expr, $square:expr, $color:expr, $state:expr) => {
        match $patterns {
            None => true,
            Some(patterns) => patterns.iter().any(|pattern| {
                match_pattern!(pattern, $square, $color, $state)
            }),
        }
    };
}

/*----------------------------------------------------------------------------*\
                              STAND-OFF DETECTION
\*----------------------------------------------------------------------------*/

/// is_in_stand_off!
///
/// Tells if the position has a stand-off. The macro examines each piece
/// type, each square of that type, and the stand-off patterns of that
/// square. It stops at the first match.
///
/// `make_move!` tests the position before and after the move:
///
/// - stand-off before and after : illegal, unless the move is a pass
/// - stand-off only after       : legal
///
/// Params:
/// - state: &State -> current position
///
/// Return:
/// bool            -> true when a stand-off pattern matches
///
#[macro_export]
macro_rules! is_in_stand_off {
    ($state:expr) => {{
        let mut found = false;
        let board_size = $state.statics.board_size;

        'main: for index in 0..$state.statics.pieces.len() {
            for &square in piece_squares!($state, index) {
                for pattern in
                    &$state.statics.relevant_stand_offs
                        [index * board_size + square as usize]
                {
                    if match_pattern!(
                        pattern,
                        square as u32,
                        p_color!($state.statics.pieces[index]),
                        &$state
                    ) {
                        found = true;
                        break 'main;
                    }
                }
            }
        }

        found
    }};
}
