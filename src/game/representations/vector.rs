//! vector.rs
//!
//! Implements move vector and leg representations for square-board variants.
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
/// A `Leg` is a packed `u32`:
///
/// ```text
///   0               8               16                              31
///   ┌───────────────┬───────────────┬────────────────────────────────┐
///   │       x       │       y       │           modifiers            │
///   └───────────────┴───────────────┴────────────────────────────────┘
/// ```
///
/// - Bits 0..7     : signed file displacement
/// - Bits 8..15    : signed rank displacement
/// - Bits 16..31   : movement and capture modifiers
///
/// The modifier half is [`LegVector`]'s modifier word moved down by 16
/// bits, so a compiled leg keeps every rule the notation gave it while
/// leaving the parse tree behind. Eleven of those bits assert a property
/// and five deny one, each capital denying the lowercase of its letter:
///
/// ```text
///   16  18  20  22  24  26  28  30
///     17  19  21  23  25  27  29  31
///   ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
///   │m│c│d│u│k│v│g│t│i│p│r│K│V│G│I│R│
///   └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
/// ```
///
/// `t` and `p` have no negated bit because they grant a capability rather
/// than constrain one: leaving them clear already says the leg cannot
/// capture en passant, or does not create an en-passant square.
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
///
/// - l: Leg -> packed leg word read
///
/// x!
///
///   Return:
///   i8 -> signed file delta (bits 0-7)
///
/// y!
///
///   Return:
///   i8 -> signed rank delta (bits 8-15)
///
/// Every remaining reader returns `bool` for the single modifier bit its
/// name spells, in the order the table above lays them out:
///
/// - m! -> the leg may move
/// - c! -> the leg may capture
/// - d! -> the leg may destroy a friendly piece
/// - u! -> the leg may unload a held piece
/// - k! -> what it captures must be royal
/// - v! -> what it captures must be virgin
/// - g! -> what it captures must be of greater rank
/// - t! -> the leg may capture en passant
/// - p! -> the leg's start square becomes an en-passant square
/// - i! -> the leg must be the piece's initial move
/// - r! -> the leg must be used to promote
///
/// The five remaining readers, `not_k!`, `not_v!`, `not_g!`, `not_i!` and
/// `not_r!`, ask the same questions of the denial bits: each is true when
/// the leg forbids what its lowercase demands.
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
macro_rules! r {
    ($l:expr) => {
        ($l >> 26) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_k {
    ($l:expr) => {
        ($l >> 27) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_v {
    ($l:expr) => {
        ($l >> 28) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_g {
    ($l:expr) => {
        ($l >> 29) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_i {
    ($l:expr) => {
        ($l >> 30) & 1 == 1
    };
}

#[macro_export]
macro_rules! not_r {
    ($l:expr) => {
        ($l >> 31) & 1 == 1
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
///
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
///
/// Bits 32..63:
///
/// ```text
///   32  34  36  38  40  42  44  46                                  63
///     33  35  37  39  41  43  45  47
///   ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬────────────────────────────────┐
///   │m│c│d│u│k│v│g│t│i│p│r│K│V│G│I│R│             unused             │
///   └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴────────────────────────────────┘
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
/// - Bit 42     : `r`, must promote on this leg
/// - Bit 43     : `K`, meaning `!k`
/// - Bit 44     : `V`, meaning `!v`
/// - Bit 45     : `G`, meaning `!g`
/// - Bit 46     : `I`, meaning `!i`
/// - Bit 47     : `R`, meaning `!r`
/// - Bits 48..63: unused
///
/// Main modifiers say what the leg may do:
///
/// - `m` : move to the leg's end square
/// - `c` : capture an enemy piece standing there
/// - `d` : destroy, which is capturing a friendly piece standing there
/// - `u` : unload, placing the last captured piece back on the board at
///         the leg's start square
///
/// Capture constraints say what the captured piece must be:
///
/// - `k` : royal
/// - `v` : virgin, having never moved
/// - `g` : of greater rank than the capturing piece
///
/// Leg constraints say when the leg may be used at all:
///
/// - `i` : only as the piece's initial move
/// - `r` : only to promote, so the leg is valid only while its own start
///         or end square lies in a promotion zone, and using it forces
///         the completed move to be a promotion
///
/// Two capabilities have no opposite to state:
///
/// - `t` : this leg may capture en passant
/// - `p` : this leg's start square creates an en-passant square
///
/// Each of `k`, `v`, `g`, `i`, and `r` also has a denial bit, and all five
/// pairs are read the same way:
///
/// ```text
///   ┌───────┬────────┬──────────────────────────────────────────┐
///   │   x   │   !x   │                 meaning                  │
///   ├───────┼────────┼──────────────────────────────────────────┤
///   │   0   │   0    │ unconstrained, the ordinary leg          │
///   │   0   │   1    │ the property must be absent              │
///   │   1   │   0    │ the property must be present             │
///   │   1   │   1    │ special combination, described below     │
///   └───────┴────────┴──────────────────────────────────────────┘
/// ```
///
/// Rank is whatever the variant declares it to be; with no rank definition
/// at all every piece has rank 0. The `g` pair compares with `>` and `<=`
/// rather than `>=` and `<`, which keeps equal-rank captures legal by
/// default while still letting a variant forbid capturing upward.
///
/// Setting both halves of a pair is defined for exactly one of them:
///
/// - `v!v`                      -> this leg bypasses forbidden zones
/// - `k!k`, `g!g`, `i!i`, `r!r` -> undefined
///
/// Defaults:
///
/// A leg written with no modifier letters carries `m`, except the last leg
/// of a vector, which carries `mc`: a piece that reaches the square it was
/// aiming at is assumed to be able to take whatever stands on it.
///
/// Final notes:
///
/// - the capture constraints are only consulted on a leg that captures or
///   destroys something, so putting them on a quiet leg says nothing
/// - `mc!kvg` is a move/capture leg whose target must not be royal, must
///   already have moved, and must not outrank the capturing piece
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
    ///
    /// Notes:
    /// Invalid modifier characters panic in `parse_modifiers`; callers must
    /// validate notation before constructing a leg.
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
                'r' => 1 << 10,
                '!' => break,
                _ => panic!("Invalid modifier character: {}", ch),
            };
        }

        for ch in &mut *chars {
            bits |= match ch {
                'k' => 1 << 11,
                'v' => 1 << 12,
                'g' => 1 << 13,
                'i' => 1 << 14,
                'r' => 1 << 15,
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
            ('r', r!(self.0 >> 32)),
        ];

        let not_mods = [
            ('k', not_k!(self.0 >> 32)),
            ('v', not_v!(self.0 >> 32)),
            ('g', not_g!(self.0 >> 32)),
            ('i', not_i!(self.0 >> 32)),
            ('r', not_r!(self.0 >> 32)),
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
    /// word (atomic displacement low, modifier bits high), `as_tuple`
    /// returns both halves, and `add_modifier` ORs extra modifier letters
    /// into the current set.
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
///
/// Grouping:
///
/// - `BracketToken`      : `<` and `>`, a group whose net displacement
///                         becomes the direction later legs continue in
/// - `SlashBracketToken` : `</` and `/>`, the same grouping without that
///                         rewrite, so later legs follow the group's own
///                         final step
///
/// Modifiers, held pending until the term they qualify arrives:
///
/// - `MoveModifierToken` : a run of `mcdukvgtipr!` letters, added to the
///                         branch's final leg
/// - `CardinalToken`     : one of the eight compass names, keeping only
///                         the branches that point that way
/// - `FilterToken`       : `[n]`, keeping only the branches at those
///                         1-based indices
///
/// Bodies:
///
/// - `LegToken`          : one leg of a multi-leg expression
/// - `AtomicToken`       : one run of `K` atoms inside an atomic
///                         expression
///
/// Repetition and set arithmetic:
///
/// - `DotsToken`         : `...`, repeating the final leg once per dot,
///                         each repetition its own runtime leg
/// - `RangeToken`        : `{i..j}`, branching into every repetition
///                         count the range allows
/// - `ColonToken`        : `:{i..j}`, the same over the whole preceding
///                         element rather than its final leg
/// - `ExclusionToken`    : `@expr`, dropping every vector `expr`
///                         produces from the result
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
/// One displacement in the movement-notation compiler, packed into four
/// signed bytes of a `u32`: `whole` is the total offset from the piece's
/// origin, and `last` is the step that got it there.
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
///
/// Carrying `last` is what lets repetition be expressed without the
/// notation that produced the vector: `{2..5}` repeats a step the compiler
/// no longer holds, and `add_last` recovers it from the vector itself.
/// `origin` seeds the same field with a cardinal unit vector, so a
/// direction-relative expression begins already pointing somewhere.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomicVector(u32);

impl AtomicVector {
    /// AtomicVector constructors and accessors.
    ///
    /// `new` packs a (whole, last) displacement pair into the byte layout
    /// documented on [`AtomicVector`]; `whole`, `last`, and
    /// `as_tuple` unpack it; `set` and `set_last` overwrite components in
    /// place. `origin(rotation)` builds the zero displacement
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
    /// as_tuple
    ///
    ///   Return:
    ///   [(i8, i8); 2] -> [whole, last] displacement pair
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

    pub fn as_tuple(&self) -> [(i8, i8); 2] {
        [self.whole(), self.last()]
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
    ///
    /// Notes:
    /// A zero `other.whole()` is a direction marker, not a displacement.
    /// Retaining `self.last()` keeps range expansion oriented correctly.
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
        AtomicVector::new(vectors[0], vectors[1])
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
