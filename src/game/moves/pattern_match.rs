//! pattern_match.rs
//!
//! Macros that test CPMN patterns on a board position.
//!
//! A CPMN pattern tests the pieces on squares near one square. Drop rules
//! and stand-off rules use these patterns. The macros here test a compiled
//! pattern on the current board and mirror the offsets for each colour.
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
/// The macro reads the stoppers only if all allowers match.
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
/// Notes:
/// There is no bounds check. Precomputation removes each pattern that goes
/// off the board from its square.
///
#[macro_export]
macro_rules! match_pattern {
    ($pattern:expr, $square:expr, $color:expr, $state:expr) => {{
        let (allowers, stoppers) = $pattern;

        let file = $square % $state.statics.files as u32;
        let rank = $square / $state.statics.files as u32;

        let mut invalid = false;

        for allower in allowers {
            let x = x!(allower.0) as i32 * (-2 * $color as i32 + 1);
            let y = y!(allower.0) as i32 * (-2 * $color as i32 + 1);
            let allower_pieces = &allower.1;

            let check_x = file as i32 + x;
            let check_y = rank as i32 + y;
            let check_index =
                (check_y * $state.statics.files as i32 + check_x) as usize;

            let piece_check = $state.main_board[check_index];

            if !allower_pieces.contains(piece_check) {
                invalid = true;
            }
        }

        if !invalid {
            for stopper in stoppers {
                let x = x!(stopper.0) as i32 * (-2 * $color as i32 + 1);
                let y = y!(stopper.0) as i32 * (-2 * $color as i32 + 1);
                let stopper_pieces = &stopper.1;

                let check_x = file as i32 + x;
                let check_y = rank as i32 + y;
                let check_index =
                    (check_y * $state.statics.files as i32 + check_x) as usize;

                let piece_check = $state.main_board[check_index];

                if stopper_pieces.contains(piece_check) {
                    invalid = true;
                }
            }
        }

        !invalid
    }};
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
