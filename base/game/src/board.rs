use bnum::types::U4096;

pub struct Board {
    pub ranks: u8,
    pub files: u8,
    pub bitboard: U4096,
}

impl Board {
    pub fn new(ranks: u8, files: u8) -> Board {
        Board {
            ranks,
            files,
            bitboard: U4096::from(0u32),
        }
    }

    pub fn set_bit(&mut self, rank: u8, files: u8) {
        let index = rank * self.files + files;
        self.bitboard.set_bit(index as u32, true);
    }

    pub fn clear_bit(&mut self, rank: u8, files: u8) {
        let index = rank * self.files + files;
        self.bitboard.set_bit(index as u32, false);
    }

    pub fn get_bit(&self, rank: u8, files: u8) -> bool {
        let index = rank * self.files + files;
        self.bitboard.bit(index as u32)
    }

    pub fn lsb(&self) -> u32 {
        self.bitboard.trailing_zeros()
    }

    pub fn msb(&self) -> u32 {
        self.bitboard.leading_zeros()
    }

    pub fn count_bits(&self) -> u32 {
        self.bitboard.count_ones()
    }

    pub fn to_string(&self) -> String {
        let mut result = String::new();
        for row in 0..self.ranks {
            for col in 0..self.files {
                if self.get_bit(row, col) {
                    result.push_str("1 ");
                } else {
                    result.push_str("0 ");
                }
            }
            result.push('\n');
        }
        result
    }
}