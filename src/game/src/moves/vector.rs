use std::{char, fmt::Debug};

/// A 32 bit vector representation for atomic move vectors.
///
/// - each vector is repreented as [(x1, y1), (x2, y2)]
/// - (x1, y1) is the whole vector
/// - (x2, y2) is the last vector applied
///
/// each byte in the u32 represents a component of the vector:
/// - bits 0-7: x1
/// - bits 8-15: y1
/// - bits 16-23: x2
/// - bits 24-31: y2
///
/// each atomic token that is not a vector will have a special representation:
/// since the maximum board size is 64x64, we can use values outside of that
/// range to represent special tokens.
///
/// the special tokens are as follows:
/// - '<': [(127, 0), (_, _)], second vector unused
/// - 'n|ne|e|se|s|sw|w|nw': [(d, 127), (_, _)], second vector unused
///   with d = 0 for n, 1 for ne, 2 for e, 3 for se, 4 for s, 5 for sw, 6 for w,
///   7 for nw
#[derive(Clone, Copy)]
pub struct AtomicVector(u32);

impl AtomicVector {
    pub fn new(whole: (i8, i8), last: (i8, i8)) -> Self {
        let x1 = (whole.0 as u8) as u32;
        let y1 = (whole.1 as u8) as u32;
        let x2 = (last.0 as u8) as u32;
        let y2 = (last.1 as u8) as u32;

        AtomicVector(x1 | (y1 << 8) | (x2 << 16) | (y2 << 24))
    }

    pub fn origin() -> Self {
        AtomicVector::new((0, 0), (0, 1))
    }

    pub fn special(token: &str) -> Self {
        match token {
            "<" => AtomicVector::new((127, 0), (0, 1)),
            "n" => AtomicVector::new((0, 127), (0, 0)),
            "ne" => AtomicVector::new((1, 127), (0, 0)),
            "e" => AtomicVector::new((2, 127), (0, 0)),
            "se" => AtomicVector::new((3, 127), (0, 0)),
            "s" => AtomicVector::new((4, 127), (0, 0)),
            "sw" => AtomicVector::new((5, 127), (0, 0)),
            "w" => AtomicVector::new((6, 127), (0, 0)),
            "nw" => AtomicVector::new((7, 127), (0, 0)),
            token if token.starts_with("[") && token.ends_with("]") => {
                let indices = &token[1..token.len()-1];

                let mut y1 = 0;

                for char in indices.chars() {
                    y1 |= 1 << (char.to_digit(10).unwrap() as u8);
                }

                AtomicVector::new((-127, y1 as i8), (0, 1))
            }
            _ => panic!("Invalid special token for AtomicVector: {}", token),
        }
    }

    pub fn get_special(&self) -> Option<String> {
        let (wx, wy) = self.whole();
        if wy == 127 {
            return match wx {
                0 => Some("n".to_string()),
                1 => Some("ne".to_string()),
                2 => Some("e".to_string()),
                3 => Some("se".to_string()),
                4 => Some("s".to_string()),
                5 => Some("sw".to_string()),
                6 => Some("w".to_string()),
                7 => Some("nw".to_string()),
                _ => None,
            };
        } else if wx == 127 {
            return Some("<".to_string());
        } else if wx == -127 {
            let mut indices = String::new();
            for i in 0..8 {
                if (wy & (1 << i)) != 0 {
                    indices.push(char::from_digit(i as u32, 10).unwrap());
                }
            }
            return Some(format!("[{}]", indices));
        }
        None
    }

    pub fn whole(&self) -> (i8, i8) {
        let x1 = (self.0 & 0xFF) as i8;
        let y1 = ((self.0 >> 8) & 0xFF) as i8;
        (x1, y1)
    }

    pub fn last(&self) -> (i8, i8) {
        let x2 = ((self.0 >> 16) & 0xFF) as i8;
        let y2 = ((self.0 >> 24) & 0xFF) as i8;
        (x2, y2)
    }

    pub fn is_special(&self) -> bool {
        let (wx, wy) = self.whole();
        wy == 127 || wx == 127 || wx == -127
    }

    pub fn set(&mut self, other: &AtomicVector) {
        self.0 = other.0;
    }

    pub fn set_last(&mut self, last: (i8, i8)) {
        let x2 = (last.0 as u8) as u32;
        let y2 = (last.1 as u8) as u32;

        self.0 = (self.0 & 0x0000FFFF) | (x2 << 16) | (y2 << 24);
    }

    pub fn set_whole(&mut self, whole: (i8, i8)) {
        let x1 = (whole.0 as u8) as u32;
        let y1 = (whole.1 as u8) as u32;

        self.0 = (self.0 & 0xFFFF0000) | x1 | (y1 << 8);
    }

    pub fn as_tuple(&self) -> [(i8, i8); 2] {
        [self.whole(), self.last()]
    }

    pub fn from_tuple(vectors: [(i8, i8); 2]) -> Self {
        AtomicVector::new(vectors[0], vectors[1])
    }

    pub fn add(&self, other: &AtomicVector) -> AtomicVector {
        let (wx1, wy1) = self.whole();
        let (wx2, wy2) = other.whole();

        let new_whole = (
            wx1.saturating_add(wx2), wy1.saturating_add(wy2)
        );
        let new_last = other.whole();

        AtomicVector::new(new_whole, new_last)
    }

    pub fn add_last(&self, multiple: i8) -> AtomicVector {
        let (wx1, wy1) = self.whole();
        let (lx2, ly2) = self.last();

        let new_whole = (
            wx1.saturating_add(lx2.saturating_mul(multiple)),
            wy1.saturating_add(ly2.saturating_mul(multiple))
        );

        AtomicVector::new(new_whole, self.last())
    }
}

impl From<[(i8, i8); 2]> for AtomicVector {
    fn from(vectors: [(i8, i8); 2]) -> Self {
        AtomicVector::from_tuple(vectors)
    }
}

impl From<AtomicVector> for [(i8, i8); 2] {
    fn from(vector: AtomicVector) -> Self {
        vector.as_tuple()
    }
}

impl Debug for AtomicVector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (wx, wy) = self.whole();
        let (lx, ly) = self.last();
write!(f, "[({}, {}), ({}, {})]", wx, wy, lx, ly)
    }
}