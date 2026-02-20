use crate::{game::{moves::move_parse::generate_move_vectors, representations::{drop::{DROP_PATTERN, Drops}, piece::Piece, state::State, vector::Leg}}, p_index, leg, x, y};


pub fn generate_drop_vectors(piece: &Piece, state: &State) -> Drops {

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