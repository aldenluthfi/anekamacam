//! # pattern_match.rs
//!
//! Macros for evaluating CPMN patterns against board positions.
//!
//! CPMN patterns express variant rules that read a square's neighbourhood
//! rather than a piece's movement: drop restrictions and stand-off rules
//! both accept or veto a square by which pieces occupy relative offsets
//! around it. These macros run a compiled pattern against a live board,
//! mirroring offsets by color and short-circuiting on the first failure.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 24/02/2026

/// match_pattern!
///
/// Matches a parsed pattern against a square using the given color viewpoint.
/// If a concrete piece context exists, use that piece's color for orientation;
/// otherwise use White's orientation as the default perspective.
/// Returns `true` only when all allowers pass and no stopper matches.
///
/// Params:
/// - pattern -> compiled (allower, stopper) pattern to test
/// - square  -> board square the pattern is anchored on
/// - color   -> orientation color for offset mirroring
/// - state   -> current position providing board occupancy
///
/// Return:
/// bool -> true if every allower holds and no stopper does
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

/// is_in_stand_off!
///
/// Evaluates whether the current position contains any active stand-off
/// pattern. Iterates every piece instance on the board, checks precomputed
/// candidate patterns for that piece-square pair, and returns `true` as soon as
/// one pattern matches.
///
/// Params:
/// - state -> current position to scan for stand-offs
///
/// Return:
/// bool -> true if any piece's stand-off pattern currently matches
///
#[macro_export]
macro_rules! is_in_stand_off {
    ($state:expr) => {{
        let mut found = false;
        let board_size = $state.statics.board_size;

        'main: for (index, position) in $state.piece_list.iter().enumerate() {
            for &square in position {
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
