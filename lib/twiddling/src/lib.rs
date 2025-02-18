use bnum::types::U4096;

pub fn clear_bit(x: &mut U4096, i: u32) {
    x.set_bit(i, false);
}

pub fn set_bit(x: &mut U4096, i: u32) {
    x.set_bit(i, true);
}

pub fn get_bit(x: &U4096, i: u32) -> bool {
    x.bit(i)
}

pub fn lsb(x: &U4096) -> u32 {
    x.trailing_zeros()
}

pub fn msb(x: &U4096) -> u32 {
    x.leading_zeros()
}

pub fn count_bits(x: &U4096) -> u32 {
    x.count_ones()
}