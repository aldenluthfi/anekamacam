use crate::game::representations::vector::DropVector;

/// a DropMove cosnsists of the following bits:
/// - The first 8 bits represent the piece index of the piece being dropped.
/// - The next 12 bits represent the square index where the piece is being
///   dropped.
/// - The next 12 bits is reserved for drop modifiers which is as follows:
///   - k : if set, this drop cannot deliver checkmate, otherwise, it can.
///   - f : if set, this drop can be used to drop to forbidden squares
///   - d : if set, this drop will remove every piece detected by the allowers
///   - the rest of the bits are reserved for future modifiers
///
/// note for c and d, it cannot destroy a royal piece, if it is a royal piece
/// its not legal
pub type DropMove = u32;
pub type DropAllower = Vec<DropVector>;
pub type DropStoper = Vec<DropVector>;
pub type Drops = (DropMove, DropAllower, DropStoper);
pub type DropSet = Vec<Drops>;

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

#[macro_export]
macro_rules! drop_d {
    ($drop:expr) => {
        ($drop.0 >> 22) & 1 == 1
    };
}

#[macro_export]
macro_rules! drop_piece {
    ($drop_vector:expr) => {
        ($drop_vector >> 16 & 0xFF) as u8
    };
}


