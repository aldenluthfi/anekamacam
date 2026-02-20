use lazy_static::lazy_static;
use regex::Regex;
use crate::game::representations::vector::DropVector;

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
pub type DropStoper = Vec<DropVector>;
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


