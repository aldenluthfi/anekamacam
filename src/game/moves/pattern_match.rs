//! pattern_match.rs
//!
//! Macros for evaluating CPMN patterns against board positions.
//!
//! CPMN patterns express variant rules that read a square's neighbourhood
//! rather than a piece's movement: drop restrictions and stand-off rules
//! both accept or veto a square by which pieces occupy relative offsets
//! around it. These macros run a compiled pattern against a live board,
//! mirroring offsets by color and short-circuiting on the first failure.
//!
//! Created: 24/02/2026
//! Author : Alden Luthfi

/// match_pattern!
///
/// Tests a parsed pattern at one square from one color's orientation.
///
/// Relative offsets mirror for the supplied color. The result is true only if
/// every allower matches and every stopper fails to match.
///
/// Params:
/// - pattern: &Pattern -> compiled (allower, stopper) pattern to test
/// - square : u32      -> board square the pattern is anchored on
/// - color  : u8       -> orientation color for offset mirroring
/// - state  : &State   -> current position providing board occupancy
///
/// Return:
/// bool                -> every allower holds and no stopper matches
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

/// is_in_stand_off!
///
/// Tests whether any active stand-off pattern matches in the current position.
///
/// The scan visits each piece instance and its precomputed candidate patterns,
/// stopping as soon as one pattern matches.
///
/// Params:
/// - state: &State -> current position to scan for stand-offs
///
/// Return:
/// bool            -> true if any piece's stand-off pattern currently matches
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
