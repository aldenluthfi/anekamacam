//! # pattern.rs
//!
//! Defines pattern representation types for CPMN pattern matching.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 24/02/2026

use crate::*;

/*----------------------------------------------------------------------------*\
                        PATTERN MATCHING REPRESENTATIONS
\*----------------------------------------------------------------------------*/

/// Represents a compressed set of allowed or stopper pieces.
///
/// This structure provides O(1) membership checks to eliminate dynamic
/// mapping overhead during pattern matching. Memory overhead is strictly
/// bounded to 256 booleans, naturally fitting the maximum piece limit.
#[derive(Clone)]
pub struct PieceSet([bool; 256]);

impl Default for PieceSet {
    fn default() -> Self {
        Self([false; 256])
    }
}

impl PieceSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, piece: u8) {
        self.0[piece as usize] = true;
    }

    pub fn contains(&self, piece: u8) -> bool {
        self.0[piece as usize]
    }
}

impl Debug for PieceSet {
    fn fmt(&self, f: &mut FmtFormatter<'_>) -> FmtResult {
        let mut pieces = Vec::new();
        for i in 0..256 {
            if self.0[i] {
                pieces.push(i as PieceIndex);
            }
        }
        write!(f, "PieceSet({:?})", pieces)
    }
}

/// Represents one relative pattern offset with its allowed piece set.
///
/// The `u16` packs `(x, y)` displacement, and the `PieceSet` stores piece
/// indices accepted at that offset during drop/stand-off matching.
/// This compact unit is shared by allower and stopper pattern lists.
pub type PatternUnit = (u16, PieceSet);
pub type PatternAllower = Vec<PatternUnit>;
pub type PatternStopper = Vec<PatternUnit>;
pub type Pattern = (PatternAllower, PatternStopper);
pub type PatternSet = Vec<Pattern>;
