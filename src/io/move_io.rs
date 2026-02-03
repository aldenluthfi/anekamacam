use crate::game::{
    representations::{moves::MoveType, state::State},
    util::format_square
};

pub fn format_move(mv: &MoveType, state: &State) -> String {
    let start_square = match mv {
        MoveType::SingleNoCapture(mv) => mv.start_square(),
        MoveType::SingleCapture(mv) => mv.start_square(),
        MoveType::HopperCapture(mv) => mv.start_square(),
        MoveType::MultiCapture(mv) => mv.start_square(),
    };

    let end_square = match mv {
        MoveType::SingleNoCapture(mv) => mv.end_square(),
        MoveType::SingleCapture(mv) => mv.end_square(),
        MoveType::HopperCapture(mv) => mv.end_square(),
        MoveType::MultiCapture(mv) => mv.end_square(),
    };

    format!(
        "{}{}",
        format_square(start_square, state),
        format_square(end_square, state)
    )
}