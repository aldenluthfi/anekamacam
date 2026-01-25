pub struct CapturedPiece {
    pub captured_at: u16,
    pub piece_index: u8,
}

pub struct Move {
    pub side: u8,
    pub start: u16,
    pub end: u16,
    pub captured_pieces: Vec<CapturedPiece>,
}