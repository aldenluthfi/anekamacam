use crate::game::representations::vector::DropVector;

/// a DropMove cosnsists of the following bits:
/// - The first 8 bits represent the piece index of the piece being dropped.
/// - The next 12 bits represent the square index where the piece is being
///   dropped.
/// - The next 12 bits is reserved for drop modifiers
pub type DropMove = u32;
pub type DropAllower = Vec<DropVector>;
pub type DropStopper = Vec<DropVector>;
pub type Drops = (DropMove, DropAllower, DropStopper);
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
macro_rules! drop_e {
    ($drop:expr) => {
        ($drop.0 >> 23) & 1 == 1
    };
}


