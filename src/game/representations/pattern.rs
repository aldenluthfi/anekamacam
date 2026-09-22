//! pattern.rs
//!
//! Defines pattern representation types for CPMN pattern matching.
//!
//! Some variant rules are not about how a piece moves but about what the
//! neighbourhood of a square must look like: drop restrictions and
//! stand-off rules both accept or reject a square based on which pieces
//! occupy relative offsets around it. This file defines the compact types
//! those checks run on — per-offset allowed-piece sets, plus the allower
//! and stopper pattern lists compiled once at precompute time so matching
//! is a linear scan with O(1) membership tests.
//!
//! Created: 24/02/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                        PATTERN MATCHING REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// PieceSet
///
/// The set of pieces one pattern offset accepts, stored as one flag per
/// possible [`PieceIndex`]. Membership is an array read rather than a hash
/// lookup, which matters because matching runs this test once per offset
/// per candidate square.
///
/// `NO_PIECE` is an ordinary member. A pattern that requires an empty
/// square says so by admitting that index, which is what lets one
/// mechanism express both "a friendly piece must stand here" and "this
/// square must be clear" without a second kind of test.
///
/// That membership is why the array is one longer than `MAX_PIECES`. A
/// piece index is bounded by what a config may declare, but the absence
/// marker is the top of [`PieceIndex`] rather than the top of that range,
/// so it is folded onto the extra slot on the end instead of being given a
/// seat among the real pieces.
#[derive(Clone)]
pub struct PieceSet([bool; MAX_PIECES + 1]);

impl Default for PieceSet {
    fn default() -> Self {
        Self([false; MAX_PIECES + 1])
    }
}

impl PieceSet {
    /// PieceSet method cluster.
    ///
    /// `new` builds an empty set, `insert` marks a piece index as member,
    /// and `contains` tests membership — all direct array operations with
    /// no hashing, keeping the pattern-matching inner loop branch-cheap.
    ///
    /// new
    ///
    ///   Return:
    ///   Self -> empty set, every index absent
    ///
    /// insert
    ///
    ///   Params:
    ///   - piece: PieceIndex -> piece index marked as member
    ///
    /// contains
    ///
    ///   Params:
    ///   - piece: PieceIndex -> piece index tested
    ///
    ///   Return:
    ///   bool                -> whether the index is a member
    ///
    /// slot
    ///
    ///   Params:
    ///   - piece: PieceIndex -> index to place
    ///
    ///   Return:
    ///   usize               -> its row, `NO_PIECE` folded onto the last
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
/// One relative offset from the square being tested, paired with the set of
/// pieces that offset accepts. The same unit serves both halves of a
/// pattern: in an allower the set is what must be there, in a stopper it is
/// what must not.
///
/// The `u16` is the offset packed as two signed bytes, file in the low byte
/// and rank in the high one, so the [`x!`] and [`y!`] accessors that read a
/// move vector read a pattern offset unchanged:
///
/// ```text
///   15                8 7                 0
///   ┌──────────────────┬──────────────────┐
///   │    rank, i8      │    file, i8      │
///   └──────────────────┴──────────────────┘
/// ```
///
/// Both bytes are signed because an offset points in every direction, and
/// matching negates the pair for black, so one compiled pattern serves both
/// sides of a board no variant is required to make symmetric.
pub type PatternUnit = (u16, PieceSet);

/// Pattern list types.
///
/// `PatternAllower` and `PatternStopper` are the two halves of a compiled
/// CPMN pattern: allowers enumerate offsets that must hold an accepted
/// piece, stoppers enumerate offsets that veto the match when occupied by
/// one of theirs. A `Pattern` pairs both halves, and a `PatternSet` holds
/// every pattern compiled for one (piece, square) table slot.
pub type PatternAllower = Vec<PatternUnit>;
pub type PatternStopper = Vec<PatternUnit>;
pub type Pattern = (PatternAllower, PatternStopper);
pub type PatternSet = Vec<Pattern>;
