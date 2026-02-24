use crate::{game::representations::{state::State, vector::Pattern}, x, y};

/// If associated with a piece, the POV of that piece's color will be used
/// otherwise it will use White's POV
pub fn match_pattern(
    pattern: &Pattern, square: u32, color_pov: u8, state: &State
) -> bool {

    let (allowers, stoppers) = pattern;

    let file = square % state.files as u32;
    let rank = square / state.files as u32;

    for allower in allowers.iter() {
        let x = x!(allower.0) as i32 * (-2 * color_pov as i32 + 1);
        let y = y!(allower.0) as i32 * (-2 * color_pov as i32 + 1);
        let allower_pieces = &allower.1;

        let check_x = file as i32 + x;
        let check_y = rank as i32 + y;

        if check_x < 0
            || check_x >= state.files as i32
            || check_y < 0
            || check_y >= state.ranks as i32
        {
            return false;
        }

        let check_index =
            (check_y * state.files as i32 + check_x) as usize;

        let piece_check = state.main_board[check_index];

        if !allower_pieces.contains(&piece_check) {
            return false;
        }
    }

    for stopper in stoppers.iter() {
        let x = x!(stopper.0) as i32 * (-2 * color_pov as i32 + 1);
        let y = y!(stopper.0) as i32 * (-2 * color_pov as i32 + 1);
        let stopper_pieces = &stopper.1;

        let check_x = file as i32 + x;
        let check_y = rank as i32 + y;

        if check_x < 0
            || check_x >= state.files as i32
            || check_y < 0
            || check_y >= state.ranks as i32
        {
            return false;
        }

        let check_index =
            (check_y * state.files as i32 + check_x) as usize;

        let piece_check = state.main_board[check_index];

        if stopper_pieces.contains(&piece_check) {
            return false;
        }
    }

    true
}

#[macro_export]
macro_rules! is_in_stand_off {
    ($state:expr) => {
        {
            let mut found = false;

            'main: for (index, position) in
                $state.piece_list.iter().enumerate()
            {
                for pattern in &$state.stand_off_patterns[index] {
                    for &square in position {
                        if match_pattern(
                            pattern, square as u32,
                            p_color!($state.pieces[index]), &$state
                        ) {
                            found = true;
                            break 'main;
                        }
                    }
                }
            }

            found
        }
    };
}