use lazy_static::lazy_static;
use regex::Regex;
use crate::{game::{moves::move_parse::generate_move_vectors, representations::{piece::Piece, state::State, vector::{Leg, PlainVector}}}, leg, p_index, x, y};

lazy_static! {
    pub static ref DROP_PATTERN: Regex =
        Regex::new(r"^([kf]*)#@(.*)$").unwrap();
}

/// a DropMove cosnsists of the following bits:
/// - The first 8 bits represent the piece index of the piece being dropped.
/// - The next 12 bits represent the square index where the piece is being
///   dropped.
/// - The next 12 bits is reserved for drop modifiers which is as follows:
///   - k : if set, this drop cannot deliver checkmate, otherwise, it can.
///   - f : if set, this drop can be used to drop to forbidden squares
///   - the rest of the bits are reserved for future modifiers
pub type DropMove = u32;
pub type DropStoper = Vec<PlainVector>;
pub type Drops = (DropMove, DropStoper);

#[macro_export]
macro_rules! drop_k {
    ($drop:expr) => {
        ($drop.0 >> 20) & 1 == 1
    };
}

#[macro_export]
macro_rules! drop_f {
    ($drop:expr) => {
        ($drop.0 >> 21) & 1 == 1
    };
}

pub fn parse_drop(piece: &Piece, state: &State) -> Drops {

    if piece.drop == "#" {
        return (p_index!(piece) as u32, Vec::new());
    }

    let captures = DROP_PATTERN
        .captures(&piece.drop)
        .unwrap_or_else(|| panic!("Invalid drop format {}", piece.drop));

    let mut move_result = p_index!(piece) as u32;

    let modifiers = captures.get(1).map_or("", |m| m.as_str());

    if modifiers.contains('k') {
        move_result |= 1 << 20;
    }

    if modifiers.contains('f') {
        move_result |= 1 << 21;
    }

    let stoppers = captures.get(2).unwrap().as_str();
    let stoppers_vecs = generate_move_vectors(stoppers, state);

    #[cfg(debug_assertions)]
    println!(
        "[DEBUG] Parsed drop for piece {}: move_result = {}, stoppers = {:#?}",
        piece.name, move_result, stoppers_vecs
    );

    let stopper_result = stoppers_vecs.iter().map(
        |multi_leg_vector|
        {
            let leg = leg!(multi_leg_vector[0]);

            let x = x!(leg) as u16;
            let y = y!(leg) as u16;

            (y << 8) | x
        }
    ).collect();

    #[cfg(debug_assertions)]
    println!(
        "[DEBUG] Encoded stoppers for piece {}: {:?}",
        piece.name, stopper_result
    );

    (move_result, stopper_result)
}


