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
macro_rules! d {
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
macro_rules! k {
    ($l:expr) => {
        ($l >> 20) & 1 == 1
    };
}

#[macro_export]
macro_rules! v {
    ($l:expr) => {
        ($l >> 21) & 1 == 1
    };
}

#[macro_export]
macro_rules! g {
    ($l:expr) => {
        ($l >> 22) & 1 == 1
    };
}

#[macro_export]
macro_rules! t {
    ($l:expr) => {
        ($l >> 23) & 1 == 1
    };
}

#[macro_export]
macro_rules! i {
    ($l:expr) => {
        ($l >> 24) & 1 == 1
    };
}

#[macro_export]
macro_rules! p {
    ($l:expr) => {
        ($l >> 25) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_k {
    ($l:expr) => {
        ($l >> 26) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_v {
    ($l:expr) => {
        ($l >> 27) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_g {
    ($l:expr) => {
        ($l >> 28) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_i {
    ($l:expr) => {
        ($l >> 29) & 1 == 1
    };
}

#[macro_export]
macro_rules! l {
    ($l:expr) => {
        ($l >> 30) & 1 == 1
    };
}

#[macro_export]
macro_rules! r {
    ($l:expr) => {
        ($l >> 31) & 1 == 1
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
///
/// the next 16 bits are move modifiers:
/// - main: m, c, d, u,
/// - capture/destroy modifier: k, v, g, s
/// - miscellaneous modifier: i, p
/// - negated capture modifiers: !k, !v, !g
/// - negated misc modifiers: !i
/// - castling modifiers l and r
///
/// Modifier bits layout (bits 32-45):
///
///  47  46  45  44  43  42  41  40  39  38  37  36  35  34  33  32
/// +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
/// | r | l |!i |!g |!v |!k | p | i | t | g | v | k | u | d | c | m |
/// +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
///
/// main modifiers:
/// - (m)ove:
///   can move with this leg.
/// - (c)apture:
///   can capture with this leg.
/// - (d)estroy:
///   can destroy with this leg. Destroying is capturing a friendly piece.
/// - (u)nload:
///   can unload with this leg. Unloading is placing the last captured piece
///   back on the board to the start square of this leg.
///
/// Capture/destroy modifiers:
/// - (k)ing:
///     - k:
///       indicates the capture must be royal.
///     - !k:
///       indicates the capture must not be royal.
///
/// usage of (k, !k):
/// - (false, false): this capture can be royal or not royal (regular capture).
/// - (false, true) : this capture must not be royal.
/// - (true, false) : this capture must be royal.
/// - (true, true)  : special modifier r!r (explained below).
///
/// - (v)irgin:
///     - v:
///       indicates the capture must be virgin (not moved yet).
///     - !v:
///       indicates the capture must not be virgin (has moved).
///
/// usage of (v, !v):
/// - (false, false): this capture can be virgin or not (regular capture).
/// - (false, true) : this capture must not be virgin.
/// - (true, false) : this capture must be virgin.
/// - (true, true)  : special modifier v!v (explained below).
///
/// - (g)reater:
///     - g:
///       indicates the capture must be of greater value than the capturing
///       piece (captured > capturing).
///     - !g:
///       indicates the capture must not be of greater value than the capturing
///       piece (captured <= capturing).
///
/// design note:
/// I chose (> and <=) instead of (>= and <) because it in a lot of chess
/// variants, capturing an equal value piece is often allowed, while there
/// are variants where its not alowed to capture a greater value piece.
///
/// usage of (g, !g):
/// - (false, false): this capture can be of any value (regular capture).
/// - (false, true) : this capture must not be of greater value.
/// - (true, false) : this capture must be of greater value.
/// - (true, true)  : special modifier g!g (explained below).
///
/// - en-passan(t):
///    - t: means this leg can capture en passant.
///
/// usage of (s):
/// - (true) : this leg can capture en passant.
/// - (false): this leg cannot capture en passant.
///
/// Miscellaneous modifiers:
///
/// - (i)nitial:
///     - i:
///       indicates this leg must be used as an initial move of the piece.
///     - !i:
///       indicates this leg must not be used as an initial move of the piece.
///
/// usage of (i, !i):
/// - (false, false): this leg can be used as initial or not (regular leg).
/// - (false, true) : this leg must not be used as initial.
/// - (true, false) : this leg must be used as initial.
/// - (true, true)  : special modifier i!i (explained below).
///
/// - (p)assant:
///     - p:
///       indicates this leg's start square creates an en passant square.
///
/// usage of (p):
/// - (true) : this leg's start square creates an en passant square.
/// - (false): this leg's start square does not create an en passant square.
///
/// additional modifier 'r' for 'right' can be used to indicate that the leg
/// is to the right side (for castling purposes)
///
/// similarly, modifier 'l' for 'left' can be used to indicate that the leg
/// is to the left side (for castling purposes)
///
/// Special modifiers (r!r, v!v, g!g, i!i):
///
/// - i!i: means that at the end of this leg, the moving piece cannot be in
///   attacked, i.e. (i)mmune
/// - k!k: TODO
/// - v!v: TODO
/// - g!g: TODO
///
/// Defaults:
/// - by default each leg will have m (can move) set. except for the last leg
///   which wll have mc (can move and capture) set by default.
///
/// Final notes:
/// - capture/destroy modifiers must be used if the leg has has c or d set
/// - you can combine negation like `mc!kvg` means a move/capture leg that must
///   not be royal, must be moved, and must be of lesser or equal value and
///   cannot capture en passant.
///
/// How to use (examples)
///
/// Xianqi "Cannon" move leg: cdR-u#-nR
/// 1. First, capture/destroy as a rook, then unload it back to that square
///    (hopping)
/// 2. Continuing the same direction, move as a rook (non-hopping)
///
/// Xiangqi "King": W|kcnR
/// 1. Move as a wazir OR capture/destroy a royal piece as a rook
///    (the flying generals rule)
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LegVector(u64);

impl LegVector {

    pub fn new(atomic: AtomicVector, modifiers: &str) -> Self {
        let bits = Self::parse_modifiers(modifiers);
        let atomic_bits = atomic.0 as u64;
        let modifier_bits = (bits as u64) << 32;
        LegVector(atomic_bits | modifier_bits)
    }

    fn parse_modifiers(mods: &str) -> u16 {
        let mut bits = 0u16;
        let chars = &mut mods.chars();

        for ch in &mut *chars {
            bits |= match ch {
                'm' => 1 << 0,
                'c' => 1 << 1,
                'd' => 1 << 2,
                'u' => 1 << 3,
                'k' => 1 << 4,
                'v' => 1 << 5,
                'g' => 1 << 6,
                't' => 1 << 7,
                'i' => 1 << 8,
                'p' => 1 << 9,
                'l' => 1 << 14,
                'r' => 1 << 15,
                '!' => break,
                _ => panic!("Invalid modifier character: {}", ch),
            };
        }

        for ch in &mut *chars {
            bits |= match ch {
                'k' => 1 << 10,
                'v' => 1 << 11,
                'g' => 1 << 12,
                'i' => 1 << 13,
                _ => panic!("Invalid modifier character: {}", ch),
            };
        }

        bits
    }

    pub fn get_modifiers_str(&self) -> String {
        let mut s = "".to_string();

        let mods = [
            ('m', m!(self.0 >> 16)),
            ('c', c!(self.0 >> 16)),
            ('d', d!(self.0 >> 16)),
            ('u', u!(self.0 >> 16)),
            ('k', k!(self.0 >> 16)),
            ('v', v!(self.0 >> 16)),
            ('g', g!(self.0 >> 16)),
            ('t', t!(self.0 >> 16)),
            ('i', i!(self.0 >> 16)),
            ('p', p!(self.0 >> 16)),
        ];

        let not_mods = [
            ('k', not_k!(self.0 >> 16)),
            ('v', not_v!(self.0 >> 16)),
            ('g', not_g!(self.0 >> 16)),
            ('i', not_i!(self.0 >> 16)),
        ];

        for (ch, val) in mods {
            if val {
                s.push(ch);
            }
        }

        for (ch, val) in not_mods {
            if val {
                if !s.contains('!') {
                    s.push('!');
                }
                s.push(ch);
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
        let mut bits = Self::parse_modifiers(modifier);
        let current_bits = self.get_modifiers();

        bits |= current_bits;

        self.0 = (self.0 & 0xFFFF_FFFF) | ((bits as u64) << 32);
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