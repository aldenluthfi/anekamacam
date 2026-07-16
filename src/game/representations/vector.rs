//! vector.rs
//!
//! Implements move vector and leg representations for chess-like games.
//!
//! A piece's movement is more than a set of destinations: each step can move,
//! capture, or be constrained in ways that vary by variant. This file defines
//! the vocabulary those rules compile down to — the packed leg and atomic
//! displacement words with their modifier bits, and the parse-tree types the
//! movement-notation compiler builds on the way there — so generation reasons
//! about movement uniformly across variants.
//!
//! Created: 12/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                        MOVE GENERATION REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Leg encoding/decoding helper macros used by move generation.
///
/// A `Leg` is a packed `u32` where:
///
/// Bits 0..31:
///
/// ```text
///   0               8               16                              31
///   ┌───────────────┬───────────────┬────────────────────────────────┐
///   │       x       │       y       │           modifiers            │
///   └───────────────┴───────────────┴────────────────────────────────┘
/// ```
///
///
/// - Bits 0..7     : signed file displacement
/// - Bits 8..15    : signed rank displacement
/// - Bits 16..31   : movement and capture modifiers
///
/// `leg!` packs a parsed `LegVector` into the compact `Leg`; the rest read
/// one field each; [`LegVector`] documents all modifier meanings.
///
/// leg!
///
///   Params:
///   - leg_vector: &LegVector -> parsed leg to pack
///
///   Return:
///   Leg                      -> packed `u32` leg word
///

/// Reader params (every reader):
/// - leg_word: Leg -> packed leg word read
///
/// x!
///
///   Return:
///   i8 -> signed `x` delta (bits 0-7)
///
/// y!
///
///   Return:
///   i8 -> signed `y` delta (bits 8-15)
///
/// m!
///
///   Return:
///   bool -> may move on this leg (bit 16)
///
/// c!
///
///   Return:
///   bool -> may capture on this leg (bit 17)
///
/// d!
///
///   Return:
///   bool -> may destroy (capture a friendly piece) on this leg (bit 18)
///
/// u!
///
///   Return:
///   bool -> may unload on this leg (bit 19)
///
/// k!
///
///   Return:
///   bool -> capture must be royal (bit 20)
///
/// v!
///
///   Return:
///   bool -> capture must be virgin (unmoved) (bit 21)
///
/// g!
///
///   Return:
///   bool -> capture must be of greater rank (bit 22)
///
/// t!
///
///   Return:
///   bool -> may capture en passant (bit 23)
///
/// i!
///
///   Return:
///   bool -> must be an initial move (bit 24)
///
/// p!
///
///   Return:
///   bool -> start square creates an en passant square (bit 25)
///
/// not_k!
///
///   Return:
///   bool -> capture must not be royal (bit 26)
///
/// not_v!
///
///   Return:
///   bool -> capture must not be virgin (bit 27)
///
/// not_g!
///
///   Return:
///   bool -> capture must not be of greater rank (bit 28)
///
/// not_i!
///
///   Return:
///   bool -> must not be an initial move (bit 29)
#[macro_export]
macro_rules! leg {
    ($l:expr) => {
        ($l.get_atomic().whole().0 as u8 as Leg)
            | ($l.get_atomic().whole().1 as u8 as Leg) << 8
            | ($l.get_modifiers() as Leg) << 16
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

/// Leg
///
/// One packed movement leg used during generation and validation.
///
/// The word stores a signed displacement and modifier flags, matching the
/// layout read by the leg accessors and written from [`LegVector`].
pub type Leg = u32;

/// MoveVector / MoveSet
///
/// A `MoveVector` is one complete movement option: its ordered legs are
/// visited from origin to destination. A `MoveSet` collects every option the
/// movement-expression parser produced for one piece type.
pub type MoveVector = Vec<Leg>;
pub type MoveSet = Vec<MoveVector>;

/// MoveVector queries
///
/// Whole-vector predicates over a `MoveVector`, reading its ordered `Leg`s
/// through the leg accessors above. Every member takes the same single
/// parameter:
/// - vector: &MoveVector -> ordered legs of one movement option
///
/// vector_offset!
///
///   Return:
///   (i32, i32) -> net (file, rank) displacement, summing every leg
///
/// vector_moves_quietly!
///
///   Return:
///   bool -> whether the final leg plays as a quiet move
///
/// vector_is_initial!
///
///   Return:
///   bool -> whether any leg is restricted to the first move
#[macro_export]
macro_rules! vector_offset {
    ($vector:expr) => {{
        let mut file_offset = 0i32;
        let mut rank_offset = 0i32;
        for leg in $vector {
            file_offset += x!(leg) as i32;
            rank_offset += y!(leg) as i32;
        }
        (file_offset, rank_offset)
    }};
}

#[macro_export]
macro_rules! vector_moves_quietly {
    ($vector:expr) => {
        (match $vector.last() {
            Some(leg) => m!(leg) || !(c!(leg) || d!(leg)),
            None => false,
        })
    };
}

#[macro_export]
macro_rules! vector_is_initial {
    ($vector:expr) => {
        ($vector.iter().any(|leg| i!(leg)))
    };
}

/*----------------------------------------------------------------------------*\
                            MOVE PARSE REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Multi-leg parse-tree types.
///
/// `MultiLegGroup` is the working stack of the multi-leg expression parser;
/// each `MultiLegElement` on it is either an unparsed token, a bracketed
/// subexpression (plain or slash-form), or an already-evaluated list of
/// `MultiLegVector`s. A `MultiLegVector` is one fully resolved move option:
/// the ordered `LegVector`s a piece traverses in a single multi-leg move.
pub type MultiLegGroup = VecDeque<MultiLegElement>;

#[derive(Clone)]
pub enum MultiLegElement {
    MultiLegTerm(Token),
    MultiLegExpr(MultiLegGroup),
    MultiLegSlashExpr(MultiLegGroup),
    MultiLegEval(Vec<MultiLegVector>),
}

impl Debug for MultiLegElement {
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        match self {
            MultiLegElement::MultiLegTerm(token) => write!(f, "{:?}", token),
            MultiLegElement::MultiLegExpr(group) => write!(f, "{:?}", group),
            MultiLegElement::MultiLegSlashExpr(group) => {
                write!(f, "{:?}#", group)
            }
            MultiLegElement::MultiLegEval(vectors) => {
                write!(f, "{:?}", vectors)
            }
        }
    }
}

pub type MultiLegVector = Vec<LegVector>;

/// LegVector
///
/// A 64-bit vector representation for leg move vectors.
///
/// Bits 0..31:
///
/// ```text
///   0                                                               31
///   ┌────────────────────────────────────────────────────────────────┐
///   │                          AtomicVector                          │
///   └────────────────────────────────────────────────────────────────┘
/// ```
/// Bits 32..63:
///
/// ```text
///   32  34  36  38  40  42  44  46                                  63
///     33  35  37  39  41  43  45
///   ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬────────────────────────────────────┐
///   │m│c│d│u│k│v│g│t│i│p│K│V│G│I│               unused               │
///   └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴────────────────────────────────────┘
/// ```
///
/// - Bits 0..31 : whole `AtomicVector`
/// - Bit 32     : `m`, may move
/// - Bit 33     : `c`, may capture
/// - Bit 34     : `d`, may destroy a friendly piece
/// - Bit 35     : `u`, may unload
/// - Bit 36     : `k`, capture must be royal
/// - Bit 37     : `v`, capture must be virgin
/// - Bit 38     : `g`, capture must have greater rank
/// - Bit 39     : `t`, may capture en passant
/// - Bit 40     : `i`, must be an initial move
/// - Bit 41     : `p`, creates an en-passant square
/// - Bit 42     : `K`, meaning `!k`
/// - Bit 43     : `V`, meaning `!v`
/// - Bit 44     : `G`, meaning `!g`
/// - Bit 45     : `I`, meaning `!i`
/// - Bits 46..63: unused
///
/// The 14 active modifier bits are grouped as follows:
/// - main                      : m, c, d, u
/// - capture/destroy modifiers : k, v, g, t
/// - miscellaneous modifiers   : i, p
/// - negated capture modifiers : !k, !v, !g
/// - negated misc modifiers    : !i
///
/// Main modifiers:
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
/// Usage of (k, !k):
/// - (false, false) : this capture can be royal or not royal, regular capture.
/// - (false, true)  : this capture must not be royal.
/// - (true, false)  : this capture must be royal.
/// - (true, true)   : special modifier k!k (explained below).
///
/// - (v)irgin:
///     - v:
///       indicates the capture must be virgin (not moved yet).
///     - !v:
///       indicates the capture must not be virgin (has moved).
///
/// Usage of (v, !v):
/// - (false, false) : this capture can be virgin or not (regular capture).
/// - (false, true)  : this capture must not be virgin.
/// - (true, false)  : this capture must be virgin.
/// - (true, true)   : special modifier v!v (explained below).
///
/// - (g)reater:
///     - g:
///       indicates the capture must be of greater rank than the capturing
///       piece (captured > capturing).
///     - !g:
///       indicates the capture must not be of greater rank than the capturing
///       piece (captured <= capturing).
///
/// Design note:
/// I chose (> and <=) instead of (>= and <) because in many chess
/// variants, capturing an equal-rank piece is often allowed, while there
/// are variants where it's not allowed to capture a greater-rank piece.
///
/// A rank is an arbitrary game rule defined when setting up a variant, if it
/// is not defined then all pieces will have the rank of 0
///
/// Usage of (g, !g):
/// - (false, false) : this capture can be of any rank (regular capture).
/// - (false, true)  : this capture must not be of greater rank.
/// - (true, false)  : this capture must be of greater rank.
/// - (true, true)   : special modifier g!g (explained below).
///
/// - en-passan(t):
///    - t : means this leg can capture en passant.
///
/// Usage of (t):
/// - (true)  : this leg can capture en passant.
/// - (false) : this leg cannot capture en passant.
///
/// Miscellaneous modifiers:
///
/// - (i)nitial:
///     - i:
///       indicates this leg must be used as an initial move of the piece.
///     - !i:
///       indicates this leg must not be used as an initial move of the piece.
///
/// Usage of (i, !i):
/// - (false, false) : this leg can be used as initial or not (regular leg).
/// - (false, true)  : this leg must not be used as initial.
/// - (true, false)  : this leg must be used as initial.
/// - (true, true)   : special modifier i!i (explained below).
///
/// - (p)assant:
///     - p:
///       indicates this leg's start square creates an en passant square.
///
/// Usage of (p):
/// - (true)  : this leg's start square creates an en passant square.
/// - (false) : this leg's start square does not create an en passant square.
///
/// Special modifiers (r!r, v!v, g!g, i!i):
///
/// - i!i : TBD
/// - k!k : TBD
/// - v!v : this leg can bypass forbidden zones.
/// - g!g : TBD
///
/// Defaults:
///
/// - by default each leg has m (can move) set, except for the last leg,
///   which has mc (can move and capture) set by default.
///
/// Final notes:
///
/// - capture/destroy modifiers must be used if the leg has c or d set
/// - combined negation like `mc!kvg` is a move/capture leg that must not be
///   royal, must be moved, must be of lesser or equal rank, and cannot
///   capture en passant
/// - because capturing a royal piece is not legal, legs with the k flag are
///   skipped during move-list generation but used when checking whether a
///   square is attacked
///
/// Examples:
///
/// - Xiangqi "Cannon" (`cdR-u#-nR`): capture/destroy as a rook then unload
///   it back to that square (hopping), then continue in the same direction
///   moving as a rook (non-hopping)
/// - Xiangqi "King" (`W|kcnR`): move as a wazir, or capture/destroy a royal
///   piece as a rook (the flying-generals rule)
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LegVector(u64);

impl LegVector {
    /// LegVector::new
    ///
    /// Packs an atomic displacement and a modifier string into the packed
    /// leg representation documented on [`LegVector`].
    ///
    /// Params:
    /// - atomic   : AtomicVector -> whole/last displacement pair
    /// - modifiers: &str         -> modifier letters, e.g. "mc!kv"
    ///
    /// Return:
    /// Self                      -> the packed leg vector
    pub fn new(atomic: AtomicVector, modifiers: &str) -> Self {
        let bits = Self::parse_modifiers(modifiers);
        let atomic_bits = atomic.0 as u64;
        let modifier_bits = (bits as u64) << 32;
        LegVector(atomic_bits | modifier_bits)
    }

    /// LegVector::parse_modifiers
    ///
    /// Translates a modifier string into its bitmask: letters before the
    /// `!` separator set positive-modifier bits, letters after it set the
    /// corresponding negated bits.
    ///
    /// Params:
    /// - mods: &str -> modifier letters with optional `!` separator
    ///
    /// Return:
    /// u16          -> modifier bitmask as laid out on [`LegVector`]
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

    /// LegVector::get_modifiers_str
    ///
    /// Reconstructs the human-readable modifier string ("mc!kv" style)
    /// from the packed bits; the inverse of `parse_modifiers`.
    ///
    /// Return:
    /// String -> modifier letters, negations prefixed by a single `!`
    pub fn get_modifiers_str(&self) -> String {
        let mut s = "".to_string();

        let mods = [
            ('m', m!(self.0 >> 32)),
            ('c', c!(self.0 >> 32)),
            ('d', d!(self.0 >> 32)),
            ('u', u!(self.0 >> 32)),
            ('k', k!(self.0 >> 32)),
            ('v', v!(self.0 >> 32)),
            ('g', g!(self.0 >> 32)),
            ('t', t!(self.0 >> 32)),
            ('i', i!(self.0 >> 32)),
            ('p', p!(self.0 >> 32)),
        ];

        let not_mods = [
            ('k', not_k!(self.0 >> 32)),
            ('v', not_v!(self.0 >> 32)),
            ('g', not_g!(self.0 >> 32)),
            ('i', not_i!(self.0 >> 32)),
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

    /// Packed-field accessors.
    ///
    /// `get_atomic` / `get_modifiers` read the two halves of the packed
    /// word (atomic displacement low, modifier bits high), `set_atomic`
    /// overwrites the displacement half, `as_tuple` returns both halves,
    /// and `add_modifier` ORs extra modifier letters into the current set.
    ///
    /// get_atomic
    ///
    ///   Return:
    ///   AtomicVector -> displacement half (bits 0..31)
    ///
    /// get_modifiers
    ///
    ///   Return:
    ///   u16 -> modifier bits (bits 32..47)
    ///
    /// set_atomic
    ///
    ///   Params:
    ///   - atomic  : AtomicVector -> displacement written into bits 0..31
    ///
    /// as_tuple
    ///
    ///   Return:
    ///   (AtomicVector, u16) -> both halves, displacement first
    ///
    /// add_modifier
    ///
    ///   Params:
    ///   - modifier: &str -> modifier letters ORed into the current set
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
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        write!(
            f,
            "LegVector {{ atomic: {:?}, modifiers: {} }}",
            self.get_atomic(),
            self.get_modifiers_str()
        )
    }
}

/// Atomic parse-tree types.
///
/// Mirror of the multi-leg parse types one level down: `AtomicGroup` is the
/// working stack of the atomic expression parser, and each `AtomicElement`
/// on it is a token, a bracketed subexpression, or an evaluated list of
/// `AtomicVector`s ready to be combined into legs.
pub type AtomicGroup = VecDeque<AtomicElement>;

#[derive(Clone)]
pub enum AtomicElement {
    AtomicTerm(Token),
    AtomicExpr(AtomicGroup),
    AtomicEval(Vec<AtomicVector>),
}

impl Debug for AtomicElement {
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        match self {
            AtomicElement::AtomicTerm(token) => write!(f, "{:?}", token),
            AtomicElement::AtomicExpr(group) => write!(f, "{:?}", group),
            AtomicElement::AtomicEval(vectors) => write!(f, "{:?}", vectors),
        }
    }
}

/// Token
///
/// Lexical categories shared by the atomic and multi-leg tokenizers. Each
/// variant wraps the raw source fragment so evaluation stages and Debug
/// output can echo the original expression text unchanged.
#[derive(Clone)]
pub enum Token {
    BracketToken(String),
    SlashBracketToken(String),

    MoveModifierToken(String),
    CardinalToken(String),
    FilterToken(String),

    LegToken(String),
    AtomicToken(String),

    ColonToken(String),
    RangeToken(String),
    DotsToken(String),
    ExclusionToken(String),
}

impl Debug for Token {
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        match self {
            Token::BracketToken(s) => write!(f, "{}", s),
            Token::SlashBracketToken(s) => write!(f, "{}", s),

            Token::MoveModifierToken(s) => write!(f, "{}", s),
            Token::CardinalToken(s) => write!(f, "{}", s),
            Token::FilterToken(s) => write!(f, "{}", s),

            Token::LegToken(s) => write!(f, "{}", s),
            Token::AtomicToken(s) => write!(f, "{}", s),

            Token::ColonToken(s) => write!(f, "{}", s),
            Token::RangeToken(s) => write!(f, "{}", s),
            Token::DotsToken(s) => write!(f, "{}", s),
            Token::ExclusionToken(s) => write!(f, "{}", s),
        }
    }
}

/// AtomicVector
///
/// A 32 bit vector representation for atomic move vectors.
/// - each vector is represented as [(x1, y1), (x2, y2)]
/// - (x1, y1) is the whole vector
/// - (x2, y2) is the last vector applied
///
/// Each byte in the `u32` carries one signed component.
///
/// Bits 0..31:
///
/// ```text
///   0               8               16              24              31
///   ┌───────────────┬───────────────┬───────────────┬────────────────┐
///   │    whole.x    │    whole.y    │    last.x     │     last.y     │
///   └───────────────┴───────────────┴───────────────┴────────────────┘
/// ```
///
/// - Bits 0..7   : `whole.x`
/// - Bits 8..15  : `whole.y`
/// - Bits 16..23 : `last.x`
/// - Bits 24..31 : `last.y`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomicVector(u32);

impl AtomicVector {
    /// AtomicVector constructors and accessors.
    ///
    /// `new` / `from_tuple` pack a (whole, last) displacement pair into the
    /// byte layout documented on [`AtomicVector`]; `whole`, `last`, and
    /// `as_tuple` unpack it; `set`, `set_whole`, and `set_last` overwrite
    /// components in place. `origin(rotation)` builds the zero displacement
    /// whose `last` field carries the cardinal unit vector of `rotation`,
    /// seeding direction-relative expression expansion.
    ///
    /// new
    ///
    ///   Params:
    ///   - whole: (i8, i8) -> full displacement, packed into bytes 0-1
    ///   - last : (i8, i8) -> final-step displacement, bytes 2-3
    ///
    ///   Return:
    ///   Self              -> the packed displacement pair
    ///
    /// origin
    ///
    ///   Params:
    ///   - rotation: i8 -> cardinal index selecting the unit vector
    ///
    ///   Return:
    ///   Self           -> zero displacement whose `last` is that unit vector
    ///
    /// whole
    ///
    ///   Return:
    ///   (i8, i8) -> full displacement (bytes 0-1)
    ///
    /// last
    ///
    ///   Return:
    ///   (i8, i8) -> final-step displacement (bytes 2-3)
    ///
    /// set
    ///
    ///   Params:
    ///   - other: &AtomicVector -> displacement copied wholesale
    ///
    /// set_last
    ///
    ///   Params:
    ///   - last: (i8, i8) -> final-step displacement written to bytes 2-3
    ///
    /// set_whole
    ///
    ///   Params:
    ///   - whole: (i8, i8) -> full displacement written to bytes 0-1
    ///
    /// as_tuple
    ///
    ///   Return:
    ///   [(i8, i8); 2] -> [whole, last] displacement pair
    ///
    /// from_tuple
    ///
    ///   Params:
    ///   - vectors: [(i8, i8); 2] -> [whole, last] pair to pack
    ///
    ///   Return:
    ///   Self                     -> the packed displacement pair
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

    /// AtomicVector::add
    ///
    /// Composes two displacements: the whole vectors are added with
    /// saturation, and `last` becomes the other vector's whole unless that
    /// is zero, in which case the current `last` direction is preserved.
    ///
    /// Params:
    /// - other: &AtomicVector -> displacement applied after `self`
    ///
    /// Return:
    /// AtomicVector           -> the combined displacement
    pub fn add(&self, other: &AtomicVector) -> AtomicVector {
        let (wx1, wy1) = self.whole();
        let (wx2, wy2) = other.whole();

        let new_whole = (wx1.saturating_add(wx2), wy1.saturating_add(wy2));
        let new_last = if other.whole() != (0, 0) {
            other.whole()
        } else {
            self.last()
        };

        AtomicVector::new(new_whole, new_last)
    }

    /// AtomicVector::add_last
    ///
    /// Extends the displacement along its own `last` direction by the
    /// given multiple — the primitive behind range repetition such as
    /// `{2..5}` in move expressions.
    ///
    /// Params:
    /// - multiple: i8 -> how many additional `last` steps to take
    ///
    /// Return:
    /// AtomicVector   -> extended displacement, `last` unchanged
    pub fn add_last(&self, multiple: i8) -> AtomicVector {
        let (wx1, wy1) = self.whole();
        let (lx2, ly2) = self.last();

        let new_whole = (
            wx1.saturating_add(lx2.saturating_mul(multiple)),
            wy1.saturating_add(ly2.saturating_mul(multiple)),
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
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        let (wx, wy) = self.whole();
        let (lx, ly) = self.last();
        write!(f, "[({}, {}), ({}, {})]", wx, wy, lx, ly)
    }
}
