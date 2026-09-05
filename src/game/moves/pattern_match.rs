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

/*----------------------------------------------------------------------------*\
                            CPMN PATTERN EVALUATION
\*----------------------------------------------------------------------------*/

/// match_pattern!
///
/// Tests one compiled CPMN pattern at one square, read from one color's side
/// of the board. A pattern is two lists of `(offset, piece set)` pairs: every
/// allower must find one of its pieces on the square it points at, and no
/// stopper may find one of its own. The square passes when both hold.
///
/// Offsets are stored as White sees them and mirrored for Black by negating
/// both deltas, so one compiled pattern answers for either color. Stoppers
/// are only looked at once the allowers have all been satisfied, there being
/// nothing left to veto otherwise.
///
/// Params:
/// - pattern: &Pattern -> compiled (allower, stopper) pattern to test
/// - square : u32      -> board square the pattern is anchored on
/// - color  : u8       -> orientation color for offset mirroring
/// - state  : &State   -> current position providing board occupancy
///
/// Return:
/// bool                -> every allower holds and no stopper matches
///
/// Notes:
/// Neighbour squares are indexed with no bounds check of their own. What
/// reaches here comes from `relevant_stand_offs`, and precomputation already
/// threw out every pattern that would point off the board from this square,
/// so the geometry has been settled before the position is ever asked.
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
/// Asks whether the position stands in a stand-off: some piece somewhere
/// meets a pattern its variant declares as one. The scan walks every piece
/// type, every square that type occupies, and the patterns precomputation
/// left standing for that pairing, and stops at the first match.
///
/// A stand-off is a property of the whole board rather than of a move, so
/// nothing here is filed under a mover or a target. `make_move!` asks it of
/// the position before and after, and legality falls out of the pair: a move
/// that leaves a stand-off standing is illegal, one that walks into a fresh
/// one is not, and a pass out of a standing one is both legal and the way a
/// variant that ends on stand-offs ends.
///
/// Params:
/// - state: &State -> current position to scan for stand-offs
///
/// Return:
/// bool            -> whether any piece's stand-off pattern matches
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
