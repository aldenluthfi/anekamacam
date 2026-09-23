//! vector.rs
//!
//! Defines the move vector and leg types.
//!
//! Each step of a piece can move, capture or have a constraint, and these
//! rules change between variants. This file defines the packed leg and
//! displacement words with their modifier bits. It also defines the parse
//! tree types of the move notation compiler.
//!
//! Created: 12/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                        MOVE GENERATION REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Leg encoding and decoding macros
///
/// Pack and read the `Leg` word of move generation. A `Leg` is a packed
/// `u32`:
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
/// The modifier half is the modifier word of [`LegVector`], shifted down
/// by 16 bits. Eleven bits require a property. Five bits deny one, and each
/// capital letter denies its lowercase letter:
///
/// ```text
///   16  18  20  22  24  26  28  30
///     17  19  21  23  25  27  29  31
///   ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
///   │m│c│d│u│k│v│g│t│i│p│r│K│V│G│I│R│
///   └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
/// ```
///
/// `t` and `p` have no denial bit. They give a capability, so a clear bit
/// already denies it. [`LegVector`] gives the meaning of each modifier.
///
/// leg!
///
///   Params:
///   - leg_vector: &LegVector -> parsed leg to pack
///
///   Return:
///   Leg                      -> packed `u32` leg word
///
/// x!
///
///   Params:
///   - l: Leg -> packed leg word to read
///
///   Return:
///   i8       -> signed file delta (bits 0-7)
///
/// y!
///
///   Params:
///   - l: Leg -> packed leg word to read
///
///   Return:
///   i8       -> signed rank delta (bits 8-15)
///
/// m! .. not_r!
///
///   Params:
///   - l: Leg -> packed leg word to read
///
///   Return:
///   bool     -> the modifier bit below
///
/// - m!     : the leg can move
/// - c!     : the leg can capture
/// - d!     : the leg can destroy an own piece
/// - u!     : the leg can unload a captured piece
/// - k!     : the captured piece must be royal
/// - v!     : the captured piece must be unmoved
/// - g!     : the captured piece must have a greater rank
/// - t!     : the leg can capture en passant
/// - i!     : the leg must be the first move of the piece
/// - p!     : the start square of the leg becomes an en passant square
/// - r!     : the leg must promote
/// - not_k! : denial of `k`, and the same for not_v!, not_g!, not_i!, not_r!
///
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
/// One packed move leg for generation and validation. It has a signed
/// displacement and the modifier flags, as in the layout on `leg!`.
///
pub type Leg = u32;

/// Move option types
///
/// Move option types:
///
/// - `MoveVector` : one move option, its legs in order from the origin
/// - `MoveSet`    : all move options of one piece type
///
/// Notes:
/// A vector is shared, not owned. The table of each square keeps the
/// options that stay on the board. With owned copies, a 36 by 36 board used
/// almost ten gigabytes. No code changes a vector after the parse.
///
pub type MoveVector = Arc<[Leg]>;
pub type MoveSet = Vec<MoveVector>;

/// MoveVector queries
///
/// Tests on a full `MoveVector`. They read the legs with the leg macros.
///
/// vector_offset!
///
///   Params:
///   - vector: &MoveVector -> legs of one move option
///
///   Return:
///   (i32, i32)            -> net (file, rank) displacement of all legs
///
/// vector_moves_quietly!
///
///   Params:
///   - vector: &MoveVector -> legs of one move option
///
///   Return:
///   bool                  -> true when the last leg can be a quiet move
///
/// vector_is_initial!
///
///   Params:
///   - vector: &MoveVector -> legs of one move option
///
///   Return:
///   bool                  -> true when a leg is for the first move only
///
#[macro_export]
macro_rules! vector_offset {
    ($vector:expr) => {{
        let mut file_offset = 0i32;
        let mut rank_offset = 0i32;
        for leg in $vector.iter() {
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

/// Multi-leg parse tree types
///
/// Parse tree types of the multi-leg expression parser.
///
/// - `MultiLegGroup`   : the work stack of the parser
/// - `MultiLegElement` : a token, a bracket expression or a parsed result
/// - `MultiLegVector`  : one resolved move option, its legs in order
///
pub type MultiLegGroup = VecDeque<MultiLegElement>;

#[derive(Clone)]
pub enum MultiLegElement {
    MultiLegTerm(Token),                                                        /* an unparsed token                  */
    MultiLegExpr(MultiLegGroup),                                                /* a bracketed subexpression          */
    MultiLegSlashExpr(MultiLegGroup),                                           /* its slash-form counterpart         */
    MultiLegEval(Vec<MultiLegVector>),                                          /* resolved move options              */
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
/// One parsed leg as a 64-bit word. It has the displacement and the
/// modifier bits.
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
/// - Bit 32     : `m`, can move
/// - Bit 33     : `c`, can capture
/// - Bit 34     : `d`, can destroy an own piece
/// - Bit 35     : `u`, can unload
/// - Bit 36     : `k`, captured piece must be royal
/// - Bit 37     : `v`, captured piece must be unmoved
/// - Bit 38     : `g`, captured piece must have a greater rank
/// - Bit 39     : `t`, can capture en passant
/// - Bit 40     : `i`, must be a first move
/// - Bit 41     : `p`, makes an en passant square
/// - Bit 42     : `r`, must promote on this leg
/// - Bit 43     : `K`, meaning `!k`
/// - Bit 44     : `V`, meaning `!v`
/// - Bit 45     : `G`, meaning `!g`
/// - Bit 46     : `I`, meaning `!i`
/// - Bit 47     : `R`, meaning `!r`
/// - Bits 48..63: unused
///
/// Main modifiers, what the leg can do:
///
/// - `m` : move to the end square of the leg
/// - `c` : capture an enemy piece on that square
/// - `d` : destroy, capture an own piece on that square
/// - `u` : unload, put the last captured piece on the start square
///
/// Capture constraints, what the captured piece must be:
///
/// - `k` : royal
/// - `v` : unmoved
/// - `g` : of a greater rank than the capturing piece
///
/// Leg constraints, when the leg is legal:
///
/// - `i` : only as the first move of the piece
/// - `r` : only as a promotion, with its start or end in a promotion zone
///
/// Capabilities without a denial bit:
///
/// - `t` : the leg can capture en passant
/// - `p` : the start square of the leg becomes an en passant square
///
/// `k`, `v`, `g`, `i` and `r` each have a denial bit. All five pairs work
/// the same way:
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
/// The variant defines the rank. Without a rank definition, all pieces have
/// rank 0. The `g` pair uses `>` and `<=`, so a capture of equal rank is
/// legal by default.
///
/// When both bits of a pair are set:
///
/// - `v!v`                      : the leg ignores forbidden zones
/// - `k!k`, `g!g`, `i!i`, `r!r` : undefined
///
/// Defaults: a leg without modifier letters has `m`. The last leg of a
/// vector has `mc`.
///
/// Examples:
///
/// - `mc!kvg`             : capture only a moved, not royal, lower piece
/// - `cdR-u#-nR`          : xiangqi cannon, hop over a piece, then slide
/// - `W|kcnR`             : xiangqi king, wazir or flying general capture
/// - `mR|rcR|c!rR`        : chu shogi rook, capture in the zone promotes
/// - `<mcd!g[1357]K-*>`   : taikyoku great general, range capture
///
/// The great general captures all pieces on the ray. `!g` stops it at the
/// first piece of greater rank.
///
/// Notes:
/// The capture constraints apply only to a leg that captures or destroys.
/// A royal capture is not legal, so move generation skips `k` legs. The
/// attack test uses them.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LegVector(u64);

impl LegVector {
    /// LegVector::new
    ///
    /// Packs a displacement and a modifier string into a leg word, with the
    /// layout on [`LegVector`].
    ///
    /// Params:
    /// - atomic   : AtomicVector -> whole and last displacement pair
    /// - modifiers: &str         -> modifier letters, e.g. "mc!kv"
    ///
    /// Return:
    /// Self                      -> the packed leg vector
    ///
    /// Notes:
    /// A bad modifier letter causes a panic in `parse_modifiers`.
    ///
    pub fn new(atomic: AtomicVector, modifiers: &str) -> Self {
        let bits = Self::parse_modifiers(modifiers);
        let atomic_bits = atomic.0 as u64;
        let modifier_bits = (bits as u64) << 32;
        LegVector(atomic_bits | modifier_bits)
    }

    /// LegVector::parse_modifiers
    ///
    /// Converts a modifier string into its bitmask. Letters before `!` set
    /// the modifier bits. Letters after `!` set the denial bits.
    ///
    /// Params:
    /// - mods: &str -> modifier letters with an optional `!`
    ///
    /// Return:
    /// u16          -> modifier bitmask, as on [`LegVector`]
    ///
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
    /// Writes the modifier bits as a string, for example "mc!kv". This is
    /// the inverse of `parse_modifiers`.
    ///
    /// Return:
    /// String -> modifier letters, denials after one `!`
    ///
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

    /// LegVector field accessors
    ///
    /// Read the two halves of the leg word, or add modifier letters. The
    /// low half is the displacement. The high half is the modifier bits.
    ///
    /// get_atomic
    ///
    ///   Return:
    ///   AtomicVector        -> displacement half (bits 0..31)
    ///
    /// get_modifiers
    ///
    ///   Return:
    ///   u16                 -> modifier bits (bits 32..47)
    ///
    /// as_tuple
    ///
    ///   Return:
    ///   (AtomicVector, u16) -> the two halves, displacement first
    ///
    /// add_modifier
    ///
    ///   Params:
    ///   - modifier: &str -> modifier letters to OR into the current set
    ///
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

/// Atomic parse tree types
///
/// Parse tree types of the atomic expression parser, one level below the
/// multi-leg types.
///
/// - `AtomicGroup`   : the work stack of the parser
/// - `AtomicElement` : a token, a bracket expression or parsed vectors
///
pub type AtomicGroup = VecDeque<AtomicElement>;

#[derive(Clone)]
pub enum AtomicElement {
    AtomicTerm(Token),                                                          /* an unparsed token                  */
    AtomicExpr(AtomicGroup),                                                    /* a bracketed subexpression          */
    AtomicEval(Vec<AtomicVector>),                                              /* resolved atomic vectors            */
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
/// Token types of the atomic and multi-leg tokenizers. Each token keeps
/// its source text, so later stages and Debug can show it.
///
/// Grouping:
///
/// - `BracketToken`      : `<` `>`, the net displacement sets the direction
/// - `SlashBracketToken` : `</` `/>`, later legs follow the last step
///
/// Modifiers, which wait for the term after them:
///
/// - `MoveModifierToken` : `mcdukvgtipr!` letters for the last leg
/// - `CardinalToken`     : a compass name, keeps branches in that direction
/// - `FilterToken`       : `[n]`, keeps the branches at those indices
///
/// Bodies:
///
/// - `LegToken`          : one leg of a multi-leg expression
/// - `AtomicToken`       : one sequence of `K` atoms
///
/// Repetition and set operations:
///
/// - `DotsToken`         : `...`, one more copy of the last leg for each dot
/// - `RangeToken`        : `{i..j}`, one branch for each count in the range
/// - `ColonToken`        : `:{i..j}`, the same for the full element before
/// - `ExclusionToken`    : `@expr`, removes the vectors of `expr`
///
/// Notes:
/// `FilterToken` indices start at 1.
///
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
/// One displacement of the move notation compiler, as four signed bytes
/// of a `u32`. `whole` is the total offset from the origin. `last` is the
/// last step.
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
/// `last` lets `{2..5}` repeat a step without the source notation.
/// `add_last` reads it. `origin` sets it to a unit vector, so a relative
/// expression starts with a direction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomicVector(u32);

impl AtomicVector {
    /// AtomicVector constructors and accessors
    ///
    /// Make, read and write the packed displacement pair, with the layout
    /// on [`AtomicVector`].
    ///
    /// new
    ///
    ///   Params:
    ///   - whole: (i8, i8) -> full displacement, bytes 0-1
    ///   - last : (i8, i8) -> last step displacement, bytes 2-3
    ///
    ///   Return:
    ///   Self              -> the packed displacement pair
    ///
    /// origin
    ///
    ///   Params:
    ///   - rotation: i8 -> cardinal index of the unit vector
    ///
    ///   Return:
    ///   Self           -> zero displacement, `last` is that unit vector
    ///
    /// whole
    ///
    ///   Return:
    ///   (i8, i8) -> full displacement (bytes 0-1)
    ///
    /// last
    ///
    ///   Return:
    ///   (i8, i8) -> last step displacement (bytes 2-3)
    ///
    /// set
    ///
    ///   Params:
    ///   - other: &AtomicVector -> displacement to copy
    ///
    /// set_last
    ///
    ///   Params:
    ///   - last: (i8, i8) -> last step displacement for bytes 2-3
    ///
    /// as_tuple
    ///
    ///   Return:
    ///   [(i8, i8); 2] -> [whole, last] displacement pair
    ///
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
    /// Adds two displacements. The `whole` values add with saturation. The
    /// new `last` is `other.whole()`, but if that is zero, `last` stays.
    ///
    /// Params:
    /// - other: &AtomicVector -> displacement after `self`
    ///
    /// Return:
    /// AtomicVector           -> the combined displacement
    ///
    /// Notes:
    /// A zero `other.whole()` only marks a direction. Thus the range
    /// expansion keeps the correct direction.
    ///
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
    /// Extends the displacement by a multiple of its `last` step. Range
    /// repetition, for example `{2..5}`, uses it.
    ///
    /// Params:
    /// - multiple: i8 -> number of `last` steps to add
    ///
    /// Return:
    /// AtomicVector   -> extended displacement, `last` unchanged
    ///
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
