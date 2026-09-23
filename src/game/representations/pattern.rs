//! pattern.rs
//!
//! Defines the types for CPMN pattern matching.
//!
//! Drop rules and stand-off rules test the pieces on squares near one
//! square. This file defines the piece sets for each offset and the
//! allower and stopper lists. Precomputation compiles these lists once.
//!
//! Created: 24/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                        PATTERN MATCHING REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// PieceSet
///
/// The set of pieces that one pattern offset accepts. It has one flag for
/// each [`PieceIndex`], so a membership test is one array read.
///
/// `NO_PIECE` is a normal member. A pattern that needs an empty square
/// accepts `NO_PIECE`. The array has one more slot than `MAX_PIECES` for
/// this value, because `NO_PIECE` is not in the range of piece indices.
///
#[derive(Clone)]
pub struct PieceSet([bool; MAX_PIECES + 1]);

impl Default for PieceSet {
    fn default() -> Self {
        Self([false; MAX_PIECES + 1])
    }
}

impl PieceSet {
    /// PieceSet methods
    ///
    /// Methods of the piece set. Each method is a direct array operation
    /// without a hash.
    ///
    /// new
    ///
    ///   Return:
    ///   Self -> empty set, no index is a member
    ///
    /// insert
    ///
    ///   Params:
    ///   - piece: PieceIndex -> piece index to add
    ///
    /// contains
    ///
    ///   Params:
    ///   - piece: PieceIndex -> piece index to test
    ///
    ///   Return:
    ///   bool                -> true when the index is a member
    ///
    /// slot
    ///
    ///   Params:
    ///   - piece: PieceIndex -> index to place
    ///
    ///   Return:
    ///   usize               -> its array slot, `NO_PIECE` is the last
    ///
    pub fn new() -> Self {
        Self::default()
    }

    fn slot(piece: PieceIndex) -> usize {
        if piece == NO_PIECE { MAX_PIECES } else { piece as usize }
    }

    pub fn insert(&mut self, piece: PieceIndex) {
        self.0[Self::slot(piece)] = true;
    }

    pub fn contains(&self, piece: PieceIndex) -> bool {
        self.0[Self::slot(piece)]
    }
}

impl Debug for PieceSet {
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        let mut pieces = Vec::new();
        for i in 0..=MAX_PIECES {
            if self.0[i] {
                pieces.push(
                    if i == MAX_PIECES { NO_PIECE } else { i as PieceIndex }
                );
            }
        }
        write!(f, "PieceSet({:?})", pieces)
    }
}

/// PatternUnit
///
/// One offset from the tested square and the set of pieces for it. In an
/// allower, a piece from the set must be there. In a stopper, it must not.
///
/// The `u16` has two signed bytes, with the file in the low byte and the
/// rank in the high byte. Thus [`x!`] and [`y!`] read it as a move vector:
///
/// ```text
///   15                8 7                 0
///   ┌──────────────────┬──────────────────┐
///   │    rank, i8      │    file, i8      │
///   └──────────────────┴──────────────────┘
/// ```
///
/// The matcher negates both bytes for Black, so one pattern is correct for
/// the two colours.
///
pub type PatternUnit = (u16, PieceSet);

/// Pattern list types
///
/// The list types of a compiled CPMN pattern. A pattern matches when all
/// allowers match and no stopper matches.
///
/// - PatternAllower : offsets that must have a piece from their set
/// - PatternStopper : offsets that must not have a piece from their set
/// - Pattern        : an (allower, stopper) pair
/// - PatternSet     : all patterns of one (piece, square) table slot
///
pub type PatternAllower = Vec<PatternUnit>;
pub type PatternStopper = Vec<PatternUnit>;
pub type Pattern = (PatternAllower, PatternStopper);
pub type PatternSet = Vec<Pattern>;
