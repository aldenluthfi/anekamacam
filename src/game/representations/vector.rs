use std::{collections::VecDeque, fmt::Debug};

use crate::game::moves::move_parse::INDEX_TO_CARDINAL_VECTORS;

/*----------------------------------------------------------------------------*\
                        MOVE GENERATION REPRESENTATIONS
\*----------------------------------------------------------------------------*/

#[macro_export]
macro_rules! leg {
    ($l:expr) => {
        ($l.get_atomic().whole().0 as u8 as Leg) |
        ($l.get_atomic().whole().1 as u8 as Leg) << 8 |
        ($l.get_modifiers() as Leg) << 16
    };
}

#[macro_export]
macro_rules! vector {
    ($l:expr) => {
        $l & 0xFFFF_FFFF                                                        /* lower 32 bits represent the vector */
    };
}

#[macro_export]
macro_rules! modifiers {
    ($l:expr) => {
        ($l >> 32) & 0xFFFF                                                     /* upper 16 bits represent modifiers  */
    };
}

#[macro_export]
macro_rules! x {
    ($l:expr) => {
        ($l & 0xFF) as i8
    };
}

#[macro_export]
macro_rules! y {
    ($l:expr) => {
        (($l >> 8) & 0xFF) as i8
    };
}

#[macro_export]
macro_rules! m {
    ($l:expr) => {
        ($l >> 16) & 1 == 1
    };
}

#[macro_export]
macro_rules! c {
    ($l:expr) => {
        ($l >> 17) & 1 == 1
    };
}

#[macro_export]
macro_rules! i {
    ($l:expr) => {
        ($l >> 18) & 1 == 1
    };
}

#[macro_export]
macro_rules! u {
    ($l:expr) => {
        ($l >> 19) & 1 == 1
    };
}

#[macro_export]
macro_rules! d {
    ($l:expr) => {
        ($l >> 20) & 1 == 1
    };
}

#[macro_export]
macro_rules! p {
    ($l:expr) => {
        ($l >> 21) & 1 == 1
    };
}

#[macro_export]
macro_rules! k {
    ($l:expr) => {
        ($l >> 22) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_m {
    ($l:expr) => {
        ($l >> 23) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_c {
    ($l:expr) => {
        ($l >> 24) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_i {
    ($l:expr) => {
        ($l >> 25) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_u {
    ($l:expr) => {
        ($l >> 26) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_d {
    ($l:expr) => {
        ($l >> 27) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_p {
    ($l:expr) => {
        ($l >> 28) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_k {
    ($l:expr) => {
        ($l >> 29) & 1 == 1
    };
}

#[macro_export]
macro_rules! kingside {
    ($l:expr) => {
        {
            let rank_offset = (($l >> 8) & 0xFF) as i8;
            rank_offset == i8::MAX
        }
    };
}

#[macro_export]
macro_rules! queenside {
    ($l:expr) => {
        {
            let rank_offset = (($l >> 8) & 0xFF) as i8;
            rank_offset == i8::MIN
        }
    };
}

#[macro_export]
macro_rules! castling {
    ($l:expr) => {
        {
            let rank_offset = (($l >> 8) & 0xFF) as i8;
            rank_offset == i8::MIN || rank_offset == i8::MAX
        }
    };
}

/// A single leg move representation. Similar to LegVector but in u32 form.
/// Only retaining the whole vector and modifier bits.
pub type Leg = u32;
pub type MoveVector = Vec<Leg>;
pub type MoveSet = Vec<MoveVector>;

/*----------------------------------------------------------------------------*\
                            MOVE PARSE REPRESENTATIONS
\*----------------------------------------------------------------------------*/

pub type MultiLegGroup = VecDeque<MultiLegElement>;

#[derive(Clone)]
pub enum MultiLegElement {
    MultiLegTerm(Token),
    MultiLegExpr(MultiLegGroup),
    MultiLegSlashExpr(MultiLegGroup),
    MultiLegEval(Vec<MultiLegVector>),
}

impl Debug for MultiLegElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MultiLegElement::MultiLegTerm(token) =>
                write!(f, "{:?}", token),
            MultiLegElement::MultiLegExpr(group) =>
                write!(f, "{:#?}", group),
            MultiLegElement::MultiLegSlashExpr(group) =>
                write!(f, "{:#?}#", group),
            MultiLegElement::MultiLegEval(vectors) =>
                write!(f, "{:#?}", vectors),
        }
    }
}

pub type MultiLegVector = Vec<LegVector>;

/// A 64 bit vector representation for leg move vectors.
/// the first 32 bits represent the whole AtomicVector,
/// the next bits are move modifiers
/// - `m` (must move without capture)
/// - `c` (must capture with this move)
/// - `i` (you can do this whole only on the initial move of the piece)
/// - `u` (unload, capturing then/or putting the latest captured piece on the
///   starting square of the current leg)
/// - `d` (destroy, must capture friendly peice)
/// - `p` (the starting square creates an en-passant square)
/// - `k` (gives check)
///
/// while ! can be used for negation of the above modifiers. `m` and `c` are
/// mutually exclusive, they cannot be used at the same time When chaining moves
/// or making a multi-leg move (moves with `-`), every atom has an implicit
/// `m!du` except the last one which has `!du` unless otherwise specified. Since
/// every move implies `!p`, `!p` would then mean “this move can be used to
/// capture to an en passant square”. The same goes for `!i` every move implies
/// `!i`, so `!i` would mean “this move can be made except for its initial move”
///
/// the m odifiers are stored in the next 14 bits after the first 32 bits:
///
/// Modifier bits layout (bits 32-45):
///
///  45  44  43  42  41  40  39  38  37  36  35  34  33  32
/// +---+---+---+---+---+---+---+---+---+---+---+---+---+---+
/// |!k |!p |!d |!u |!i |!c |!m | k | p | d | u | i | c | m |
/// +---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///
/// Legend:
/// - m  : must move without capture
/// - c  : must capture with this move
/// - i  : only on initial move
/// - u  : unload (capture and/or drop)
/// - d  : destroy (must capture friendly)
/// - p  : creates en-passant square
/// - k  : gives check
/// - !x : negation of modifier x
///
/// Castling moves are represented with a special rank offset in the
/// AtomicVector part of the LegVector:
/// - rank offset = i8::MAX for kingside castling
/// - rank offset = i8::MIN for queenside castling
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LegVector(u64);

#[derive(Clone, Copy, Debug)]
pub struct ModifierState {
    pub m: Option<bool>,
    pub c: Option<bool>,
    pub i: Option<bool>,
    pub u: Option<bool>,
    pub d: Option<bool>,
    pub p: Option<bool>,
    pub k: Option<bool>,
}

#[hotpath::measure_all]
impl LegVector {

    pub fn new(atomic: AtomicVector, modifiers: &str) -> Self {
        let bits = Self::parse_modifiers(modifiers);
        let atomic_bits = atomic.0 as u64;
        let modifier_bits = (bits as u64) << 32;
        let result = LegVector(atomic_bits | modifier_bits);

        result.check_discrepancy();
        result
    }

    fn parse_modifiers(mods: &str) -> u16 {
        let mut bits = 0u16;
        let chars = &mut mods.chars();
        for ch in &mut *chars {
            bits |= match ch {
                'm' => 1 << 0,
                'c' => 1 << 1,
                'i' => 1 << 2,
                'u' => 1 << 3,
                'd' => 1 << 4,
                'p' => 1 << 5,
                'k' => 1 << 6,
                '!' => break,
                _ => panic!("Invalid modifier character: {}", ch),
            };
        }

        for next in chars {
            bits |= match next {
                'm' => 1 << 7,
                'c' => 1 << 8,
                'i' => 1 << 9,
                'u' => 1 << 10,
                'd' => 1 << 11,
                'p' => 1 << 12,
                'k' => 1 << 13,
                _ => panic!("Invalid modifier character: {}", next),
            };
        }

        if bits & (1 << 1) == 0 && bits & (1 << 13) != 0 {
            bits |= 1 << 6;                                                     /* c implies k                        */
        }

        if (bits & (1 << 0) != 0) && (bits & (1 << 1) != 0) {                   /* mutually exclusive                 */
            panic!(
                "Invalid modifier state: both m and c are set"
            );
        }

        bits
    }

    #[inline(always)]
    pub fn get_modifier_state(&self) -> ModifierState {
        let mods = (self.0 >> 32) as u16;

        unsafe {
            ModifierState {
                m: match (mods & 1, mods & (1 << 7)) {
                    (0, 0) => None,
                    (_, 0) => Some(true),
                    (0, _) => Some(false),
                    _ => std::hint::unreachable_unchecked(),
                },
                c: match (mods & 2, mods & (1 << 8)) {
                    (0, 0) => None,
                    (_, 0) => Some(true),
                    (0, _) => Some(false),
                    _ => std::hint::unreachable_unchecked(),
                },
                i: match (mods & 4, mods & (1 << 9)) {
                    (0, 0) => None,
                    (_, 0) => Some(true),
                    (0, _) => Some(false),
                    _ => std::hint::unreachable_unchecked(),
                },
                u: match (mods & 8, mods & (1 << 10)) {
                    (0, 0) => None,
                    (_, 0) => Some(true),
                    (0, _) => Some(false),
                    _ => std::hint::unreachable_unchecked(),
                },
                d: match (mods & 16, mods & (1 << 11)) {
                    (0, 0) => None,
                    (_, 0) => Some(true),
                    (0, _) => Some(false),
                    _ => std::hint::unreachable_unchecked(),
                },
                p: match (mods & 32, mods & (1 << 12)) {
                    (0, 0) => None,
                    (_, 0) => Some(true),
                    (0, _) => Some(false),
                    _ => std::hint::unreachable_unchecked(),
                },
                k: match (mods & 64, mods & (1 << 13)) {
                    (0, 0) => None,
                    (_, 0) => Some(true),
                    (0, _) => Some(false),
                    _ => std::hint::unreachable_unchecked(),
                },
            }
        }
    }

    pub fn get_modifiers_str(&self) -> String {
        let mut s = "".to_string();

        let mods = [
            ('m', self.is_m()),
            ('c', self.is_c()),
            ('i', self.is_i()),
            ('u', self.is_u()),
            ('d', self.is_d()),
            ('p', self.is_p()),
            ('k', self.is_k()),
        ];

        for (ch, val) in mods.iter() {
            match val {
                Some(true) => s.push(*ch),
                Some(false) => {
                    if !s.contains('!') {
                        s.push('!');
                    }
                    s.push(*ch);
                }
                None => {}
            }
        }

        s
    }

    pub fn get_atomic(&self) -> AtomicVector {
        AtomicVector((self.0 & 0xFFFF_FFFF) as u32)
    }

    pub fn get_modifiers(&self) -> u16 {
        (self.0 >> 32) as u16
    }

    pub fn set_atomic(&mut self, atomic: AtomicVector) {
        self.0 = (self.0 & !0xFFFF_FFFFu64) | (atomic.0 as u64);
    }

    pub fn as_tuple(&self) -> (AtomicVector, u16) {
        (self.get_atomic(), self.get_modifiers())
    }

    pub fn add_modifier(&mut self, modifier: &str) {
        let mut bits = self.get_modifiers();

        let is_negated = modifier.starts_with('!');

        for ch in modifier.chars().filter(|&c| c != '!') {
            let bit = match ch {
                'm' => 0,
                'c' => 1,
                'i' => 2,
                'u' => 3,
                'd' => 4,
                'p' => 5,
                'k' => 6,
                _ => panic!("Invalid modifier character: {}", ch),
            };
            let bit = if is_negated { bit + 7 } else { bit };

            bits |= 1 << bit;

            self.check_discrepancy();

            self.0 = (self.0 & 0xFFFF_FFFF) | ((bits as u64) << 32);
        }
    }

    pub fn check_discrepancy(&self) {
        self.is_c();
        self.is_m();
        self.is_i();
        self.is_u();
        self.is_d();
        self.is_p();
        self.is_k();
    }

    pub fn is_m(&self) -> Option<bool> {
        let mods = self.get_modifiers();
        match (mods & (1 << 0) != 0, mods & (1 << 7) != 0) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (false, false) => None,
            (true, true) => panic!(
                "Invalid modifier state: both m and !m are set"
            ),
        }
    }

    pub fn is_c(&self) -> Option<bool> {
        let mods = self.get_modifiers();
        match (mods & (1 << 1) != 0, mods & (1 << 8) != 0) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (false, false) => None,
            (true, true) => panic!(
                "Invalid modifier state: both m and !m are set"
            ),
        }
    }

    pub fn is_i(&self) -> Option<bool> {
        let mods = self.get_modifiers();
        match (mods & (1 << 2) != 0, mods & (1 << 9) != 0) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (false, false) => None,
            (true, true) => panic!(
                "Invalid modifier state: both m and !m are set"
            ),
        }
    }

    pub fn is_u(&self) -> Option<bool> {
        let mods = self.get_modifiers();
        match (mods & (1 << 3) != 0, mods & (1 << 10) != 0) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (false, false) => None,
            (true, true) => panic!(
                "Invalid modifier state: both m and !m are set"
            ),
        }
    }

    pub fn is_d(&self) -> Option<bool> {
        let mods = self.get_modifiers();
        match (mods & (1 << 4) != 0, mods & (1 << 11) != 0) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (false, false) => None,
            (true, true) => panic!(
                "Invalid modifier state: both m and !m are set"
            ),
        }
    }

    pub fn is_p(&self) -> Option<bool> {
        let mods = self.get_modifiers();
        match (mods & (1 << 5) != 0, mods & (1 << 12) != 0) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (false, false) => None,
            (true, true) => panic!(
                "Invalid modifier state: both m and !m are set"
            ),
        }
    }

    pub fn is_k(&self) -> Option<bool> {
        let mods = self.get_modifiers();
        match (mods & (1 << 6) != 0, mods & (1 << 13) != 0) {
            (true, false) => Some(true),
            (false, true) => Some(false),
            (false, false) => None,
            (true, true) => panic!(
                "Invalid modifier state: both m and !m are set"
            ),
        }
    }

    #[inline(always)]
    pub fn is_castling(&self) -> bool {
        let rank_offset = ((self.0 >> 8) & 0xFF) as i8;
        rank_offset == i8::MIN || rank_offset == i8::MAX
    }

    #[inline(always)]
    pub fn is_castling_right(&self) -> bool {
        ((self.0 >> 8) & 0xFF) as i8 == i8::MAX
    }
}

impl Debug for LegVector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
                f,
                "LegVector {{ atomic: {:?}, modifiers: {} }}",
                self.get_atomic(),
                self.get_modifiers_str()
            )
    }
}

pub type AtomicGroup = VecDeque<AtomicElement>;

#[derive(Clone)]
pub enum AtomicElement {
    AtomicTerm(Token),
    AtomicExpr(AtomicGroup),
    AtomicEval(Vec<AtomicVector>),
}

impl Debug for AtomicElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AtomicElement::AtomicTerm(token) => write!(f, "{:?}", token),
            AtomicElement::AtomicExpr(group) => write!(f, "{:#?}", group),
            AtomicElement::AtomicEval(vectors) => write!(f, "{:#?}", vectors),
        }
    }
}

#[derive(Clone)]
pub enum Token {
    Bracket(String),
    SlashBracket(String),

    MoveModifier(String),
    Cardinal(String),
    Filter(String),

    Leg(String),
    Atomic(String),

    Colon(String),
    Range(String),
    Dots(String),
    Exclusion(String),
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Bracket(s) => write!(f, "{}", s),
            Token::SlashBracket(s) => write!(f, "{}", s),

            Token::MoveModifier(s) => write!(f, "{}", s),
            Token::Cardinal(s) => write!(f, "{}", s),
            Token::Filter(s) => write!(f, "{}", s),

            Token::Leg(s) => write!(f, "{}", s),
            Token::Atomic(s) => write!(f, "{}", s),

            Token::Colon(s) => write!(f, "{}", s),
            Token::Range(s) => write!(f, "{}", s),
            Token::Dots(s) => write!(f, "{}", s),
            Token::Exclusion(s) => write!(f, "{}", s),
        }
    }
}

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
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomicVector(u32);

impl AtomicVector {
    pub fn new(whole: (i8, i8), last: (i8, i8)) -> Self {
        let x1 = (whole.0 as u8) as u32;
        let y1 = (whole.1 as u8) as u32;
        let x2 = (last.0 as u8) as u32;
        let y2 = (last.1 as u8) as u32;

        AtomicVector(x1 | (y1 << 8) | (x2 << 16) | (y2 << 24))
    }

    pub fn origin(rotation: i8) -> Self {
        AtomicVector::new((0, 0), INDEX_TO_CARDINAL_VECTORS[rotation as usize])
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
        let new_last = if other.whole() != (0, 0) {
            other.whole()
        } else {
            self.last()
        };

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