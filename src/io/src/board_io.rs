use game::representations::board::{
    Board,
    Bitboard,
};

fn format_bitboard(board: &Bitboard, ranks: u8, files: u8) -> String {

    let mut result = String::new();

    for row in (0..ranks).rev() {
        for col in 0..files {
            let index: u32 = row as u32 * files as u32 + col as u32;
            if board.get_bit(index) {
                result.push_str("1  ");
            } else {
                result.push_str("0  ");
            }
        }
        result.push('\n');
    }
    result
}

pub fn format_board(board: &Board, piece_char: Option<char>) -> String {
    let ranks = board.ranks;
    let files = board.files;
    let mut bitboard_str = format_bitboard(&board.bitboard, ranks, files);

    if piece_char.is_some() {
        bitboard_str = bitboard_str.replace(
            "1",
            piece_char
                .unwrap()
                .to_string()
                .as_str()
        );
    }

    let mut result = String::new();
    result.push_str(
        &format!("   ╔{}╗\n", "═══╤".repeat(files as usize - 1) + "═══")
    );

    for (i, line) in bitboard_str.lines().enumerate() {
        result.push_str(
            &format!(
                "{} ║ {} ║\n",
                format!("{:02}", ranks as usize - i),
                line.trim().replace("  ", " │ ")
            )
        );

        result.push_str(
            &(if i == (ranks as usize - 1) {
                "".to_string()
            } else {
                format!("   ╟{}╢\n", "───┼".repeat(files as usize - 1) + "───")
            })
        );
    }

    result.push_str(
        &format!("   ╚{}╝\n     ", "═══╧".repeat(files as usize - 1) + "═══")
    );

    for col in 0..files {
        let file_label = if files < 26 {
            ((b'A' + col) as char).to_string()
        } else {
            format!("{:02}", col)
        };
        result.push_str(&format!("{:3} ", file_label));
    }
    result.push('\n');
    result = result.replace("0", " ");

    result
}
