
pub struct SearchInfo {
    pub start_time: u128,
    pub stop_time: u128,

    pub depth: usize,

    pub set_depth: usize,
    pub set_timed: u128,
    pub set_moves: usize,

    pub nodes: u128,

    pub interrupt: bool,
    pub infinite: bool,
}