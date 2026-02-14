use crate::{
    captured_piece, captured_square,
    captured_unmoved, creates_enp, created_enp, end, is_unload,
    move_type,
    multi_move_captured_piece, multi_move_captured_square,
    multi_move_captured_unmoved, multi_move_is_unload,
    multi_move_unload_square, is_initial, piece,
    promotion, promoted, start, unload_square,
    game::representations::{moves::Move, state::State},
    io::board_io::format_square,
};

pub fn format_move_template(mv: &Move) -> String {
    let mut s = String::new();
    s.push_str(&format!("Move encoding: 0x{:032X}\n", mv.0));
    s.push_str(&format!("  move_type: {}\n", move_type!(mv)));
    s.push_str(&format!("  piece: {}\n", piece!(mv)));
    s.push_str(&format!("  start: {}\n", start!(mv)));
    s.push_str(&format!("  end: {}\n", end!(mv)));
    s.push_str(&format!("  must_initial: {}\n", is_initial!(mv)));
    s.push_str(&format!("  promotion: {}\n", promotion!(mv)));
    s.push_str(&format!("  creates_enp: {}\n", creates_enp!(mv)));
    s.push_str(&format!("  promoted: {}\n", promoted!(mv)));
    s.push_str(&format!("  created_enp: {}\n", created_enp!(mv)));
    s.push_str(&format!("  is_unload: {}\n", is_unload!(mv)));
    s.push_str(&format!("  unload_square: {}\n", unload_square!(mv)));
    s.push_str(&format!(
        "  captured_piece: {}\n",
        captured_piece!(mv)
    ));
    s.push_str(&format!(
        "  captured_square: {}\n",
        captured_square!(mv)
    ));
    s.push_str(&format!(
        "  captured_unmoved: {}\n",
        captured_unmoved!(mv)
    ));

    if !mv.1.is_empty() {
        s.push_str("Captured pieces (multi-capture):\n");
        for (i, cap) in mv.1.iter().enumerate() {
            s.push_str(&format!(
                "  [{}]: \
                 is_unload: {}, unload_square: {}, \
                 captured_piece: {}, captured_square: {}, \
                 captured_unmoved: {}\n",
                i,
                multi_move_is_unload!(*cap),
                multi_move_unload_square!(*cap),
                multi_move_captured_piece!(*cap),
                multi_move_captured_square!(*cap),
                multi_move_captured_unmoved!(*cap)
            ));
        }
    }
    s
}

pub fn format_move(mv: &Move, state: &State) -> String {
    let start_square = start!(mv);
    let end_square = end!(mv);

    format!(
        "{}{}",
        format_square(start_square as u16, state),
        format_square(end_square as u16, state)
    )
}