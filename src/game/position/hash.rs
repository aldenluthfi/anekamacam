//! hash.rs
//!
//! Implements Zobrist hashing for game positions.
//!
//! Search must recognise repeated and transposed positions in O(1), so every
//! position has to collapse to a single integer key that updates incrementally
//! as moves are made. This file owns that key: it seeds the random component
//! tables and folds them together, giving repetition detection and the
//! transposition tables a stable position identity.
//!
//! Created: 25/01/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             POSITION KEY BUILDERS
\*----------------------------------------------------------------------------*/

/// PositionHash
///
/// Full-width Zobrist key of a position. 128 bits keeps the collision
/// probability negligible even on large boards with many piece types,
/// where 64-bit keys would start to saturate.
pub type PositionHash = u128;

/// hash_pawns
///
/// Folds the placement of every pawn-like piece into one key, reusing the
/// same random components the full position hash spends. Which pieces count
/// is a variant's own answer: `eval.pawn_pieces` is derived from the rules at
/// load time, so a variant whose pieces are nothing like pawns leaves an
/// empty roster and a key of zero.
///
/// Two consumers read the key. `pawn_structure!` caches its verdict under it,
/// most moves in a search moving no pawn at all, and correction history files
/// its evaluation error under it. Both want the same thing: an identity for
/// the pawn skeleton alone, blind to where every other piece stands.
///
/// This is the from-scratch fold, spent on a FEN load. Afterwards moves keep
/// the key in step, `hash_in_or_out_piece!` masking non-pawns out.
///
/// Params:
/// - state: &State -> position whose pawns are hashed
///
/// Return:
/// u128            -> pawn-placement Zobrist key
pub fn hash_pawns(state: &State) -> u128 {
    let mut hash = u128::default();

    for &index in &state.statics.eval.pawn_pieces {
        for &square in piece_squares!(state, index) {
            hash ^= PIECE_HASHES[index][square as usize];
        }
    }

    hash
}

/// hash_position
///
/// Computes the full Zobrist hash for one state from scratch.
///
/// The key includes side to move, special-state fields, on-board placement,
/// and every in-hand piece count. Repetition and transposition tables use it.
///
/// The key is one XOR fold of independent random components:
///
/// ```text
/// hash = PIECE_HASHES[piece][sq]      (every occupied square)
///      ^ SIDE_HASHES                  (when white is to move)
///      ^ CASTLING_HASHES[rights]      (current KQkq bits)
///      ^ EN_PASSANT_HASHES[sq]        (when a capture is available)
///      ^ IN_HAND_HASHES[piece][count] (every in-hand pool, both sides)
/// ```
///
/// XOR is self-inverse, so applying the same component twice removes it.
/// That is what lets a move maintain the key rather than recompute it: the
/// component of what left is folded out, the component of what arrived is
/// folded in, and the rest of the board is never touched.
///
/// Params:
/// - state: &State -> position to hash from scratch
///
/// Return:
/// u128            -> the position's full Zobrist key
///
/// Notes:
/// Every in-hand pool is folded, an empty one included, so the zero-count
/// component is part of the key rather than absent from it. The incremental
/// path agrees, folding the old count out before folding the new one in. Only
/// the rights bits of the castling byte are keyed on: the castled marks in
/// its upper bits are read by evaluation alone and two positions apart in
/// nothing else are the same position.
pub fn hash_position(state: &State) -> u128 {
    let mut hash = u128::default();

    if state.playing == WHITE {
        hash ^= &*SIDE_HASHES;
    }

    hash ^= &CASTLING_HASHES[(state.castling_state & CASTLE_RIGHTS) as usize];

    if state.en_passant_square != NO_EN_PASSANT {
        hash ^=
            &EN_PASSANT_HASHES[enp_square!(state.en_passant_square) as usize];
    }

    for index in 0..state.statics.pieces.len() {
        for &square in piece_squares!(state, index) {
            hash ^= &PIECE_HASHES[index][square as usize];
        }
    }

    for color in [WHITE, BLACK] {
        for (index, &count) in
            state.piece_in_hand[color as usize].iter().enumerate()
        {
            hash ^= &IN_HAND_HASHES[index][count as usize];
        }
    }

    hash
}

/// hash_virgin_board
///
/// Computes the unmoved-piece key for one state from scratch, folding one
/// random component per square whose piece has yet to move. A piece's first
/// move is frequently not its later ones, so two boards alike in placement
/// but apart in unmoved pieces answer different questions.
///
/// The mark is per square rather than per piece, which is all the rules ever
/// ask: what may move twice, castle, or drop is decided by whether the piece
/// standing there has moved, never by which piece it is.
///
/// Params:
/// - state: &State -> position to hash from scratch
///
/// Return:
/// u128            -> the position's unmoved-piece key
///
/// Notes:
/// This key is kept out of `position_hash`. Repetition and perpetual matching
/// compare that hash alone, and they mean it: a board reached twice repeats
/// even when a rook has spent its first move in between. [`search_key`] folds
/// this one in, so the tables still keep the two apart.
pub fn hash_virgin_board(state: &State) -> u128 {
    let mut hash = u128::default();

    for square in set_indices!(state.virgin_board) {
        hash ^= &VIRGIN_HASHES[square];
    }

    hash
}

/*----------------------------------------------------------------------------*\
                           SEARCH IDENTITY COMPONENTS
\*----------------------------------------------------------------------------*/

/// Context byte slots
///
/// One row of [`CONTEXT_HASHES`] each, a row keying one byte of live context.
/// Most contexts fit a byte and are named directly. The two counting values
/// are 16 bits wide and spend the row after theirs on the high byte, which is
/// why the numbering skips over 3 and 5 without naming them.
///
/// ```text
///  0  counter clock          6  checks made by white
///  1  counter limit          7  checks made by black
///  2  counting count, low    8  checks required to win
///  3  counting count, high   9  repetition occurrences
///  4  counting limit, low   10  pass and stand-off class
///  5  counting limit, high  11  quiescence move-set class
/// ```
///
/// `CONTEXT_SLOTS` closes the list rather than naming a slot: it is how many
/// rows the table is built with.
const COUNTER_CLOCK: usize = 0;
const COUNTER_LIMIT: usize = 1;
const COUNTING_COUNT: usize = 2;
const COUNTING_LIMIT: usize = 4;
const CHECKS_WHITE: usize = 6;
const CHECKS_BLACK: usize = 7;
const CHECKS_COUNT: usize = 8;
const REPETITION_COUNT: usize = 9;
const PASS_CLASS: usize = 10;
const QSEARCH_CLASS: usize = 11;
const CONTEXT_SLOTS: usize = 12;

lazy_static! {
    /// CONTEXT_HASHES
    ///
    /// Search-context Zobrist rows, one 256-entry row per byte slot. Filled
    /// once from the shared seeded RNG on first use and read-only after, so
    /// a run started under the same seed keys every context identically.
    ///
    /// Only [`search_key`] and [`qsearch_key`] read these rows. Whatever is
    /// folded here reaches the transposition tables and nothing else: the
    /// canonical position hash keeps meaning the board alone.
    static ref CONTEXT_HASHES: Vec<[u128; 256]> = {
        let mut result: Vec<[u128; 256]> = Vec::with_capacity(CONTEXT_SLOTS);

        for _ in 0..CONTEXT_SLOTS {
            let slot_hashes = array::from_fn(|_| random_u128());
            result.push(slot_hashes);
        }

        result
    };
}

/// wide_context
///
/// Folds a 16-bit context value across two byte rows, low byte in `slot` and
/// high byte in the row after it, so the whole range keys exactly and no
/// value has to be clamped away.
///
/// ```text
/// value 0xABCD    slot     reads 0xCD
///                 slot + 1 reads 0xAB
/// ```
///
/// Counting budgets are config text and the field holding them is 16 bits
/// wide, so nothing bounds one to a byte. On a single row a count and the
/// same count 256 further on would fold to one component, and a position
/// about to run out of budget would share an entry with one that is not.
///
/// Params:
/// - slot : usize -> first of the two rows the value spends
/// - value: u16   -> context value to fold
///
/// Return:
/// u128           -> the value's combined context component
fn wide_context(slot: usize, value: u16) -> u128 {
    CONTEXT_HASHES[slot][(value & 0xFF) as usize]
        ^ CONTEXT_HASHES[slot + 1][(value >> 8) as usize]
}

/// search_key
///
/// The transposition identity of a node: the canonical position hash plus
/// every mutable context that changes which moves are legal from here, which
/// terminal rule fires, or how a repetition reads. The canonical hash itself
/// stays untouched, so repetition matching keeps comparing boards while two
/// boards apart in progress never share a table entry.
///
/// The key is one XOR fold of the canonical hash and the live context:
///
/// ```text
/// key = position_hash            (placement, side, rights, hands)
///     ^ virgin_hash              (which pieces are still unmoved)
///     ^ counter clock and limit  (while a counter rule is declared)
///     ^ counting count and limit (while a bare-king count runs)
///     ^ checks made and required (while an N-check rule is declared)
///     ^ repetition occurrences   (while a repetition rule is declared)
///     ^ pass and stand-off bits  (always)
/// ```
///
/// The pass and stand-off bits are the three facts `position_terminal` reads
/// off history — the last ply passed, the ply before it passed, and the last
/// ply left an accepted stand-off. A position with no history folds in the
/// zero class rather than skipping the term, so a fresh board and a board
/// that has passed nothing still agree.
///
/// Params:
/// - state  : &State -> position whose search identity is wanted
/// - repeats: u8     -> occurrences of this position on the search path
///
/// Return:
/// u128              -> the node's transposition key
///
/// Notes:
/// A context a variant never declares is left out of the fold rather than
/// keyed as zero, which costs nothing: a rule that is not declared cannot
/// tell two positions apart, and every node in that run agrees to skip the
/// same terms. The repetition count comes in from the caller because it is a
/// fact about the path walked to this node and not about the position, two
/// searches reaching one board by different routes counting differently.
pub fn search_key(state: &State, repeats: u8) -> u128 {
    let mut key = state.position_hash ^ state.virgin_hash;

    if let Some(counter) = &state.termination.counter {
        key ^= &CONTEXT_HASHES[COUNTER_CLOCK][counter.clock as usize];
        key ^= &CONTEXT_HASHES[COUNTER_LIMIT][counter.limit as usize];
    }

    if let Some(counting) = &state.termination.counting
        && let Some((count, limit)) = counting.progress
    {
        key ^= wide_context(COUNTING_COUNT, count);
        key ^= wide_context(COUNTING_LIMIT, limit);
    }

    if let Some(checks) = &state.termination.checks {
        let made = checks.delivered;

        key ^= &CONTEXT_HASHES[CHECKS_WHITE][made[WHITE as usize] as usize];
        key ^= &CONTEXT_HASHES[CHECKS_BLACK][made[BLACK as usize] as usize];
        key ^= &CONTEXT_HASHES[CHECKS_COUNT][checks.count as usize];
    }

    if state.termination.repetition.is_some() {
        key ^= &CONTEXT_HASHES[REPETITION_COUNT][repeats as usize];
    }

    let pass_class = state.history.last().map_or(0, |last| {
        let passed = pass_snapshot!(last);
        let twice = passed
            && state.history.len() >= 2
            && pass_snapshot!(state.history[state.history.len() - 2]);
        let stand_off = passed && last.in_stand_off == Some(true);

        passed as u8 | (twice as u8) << 1 | (stand_off as u8) << 2
    });

    key ^= &CONTEXT_HASHES[PASS_CLASS][pass_class as usize];

    key
}

/// qsearch_key
///
/// The quiescence identity of a node: its search key plus the move-set class
/// the leaf search answers under. A checked node searches every evasion while
/// an unchecked one searches captures alone, and delta pruning stands down
/// once the board thins to an endgame, so the classes must not share entries.
///
/// ```text
/// bit 0   the side to move stands in check
/// bit 1   the position has reached its endgame phase
/// ```
///
/// Params:
/// - state   : &State -> position whose quiescence identity is wanted
/// - repeats : u8     -> occurrences of this position on the search path
/// - in_check: bool   -> whether the side to move stands in check
///
/// Return:
/// u128               -> the node's quiescence table key
pub fn qsearch_key(state: &State, repeats: u8, in_check: bool) -> u128 {
    let endgame = state.game_phase == ENDGAME;
    let class = (in_check as u8 | (endgame as u8) << 1) as usize;

    search_key(state, repeats) ^ &CONTEXT_HASHES[QSEARCH_CLASS][class]
}

/*----------------------------------------------------------------------------*\
                        INCREMENTAL HASH UPDATE HELPERS
\*----------------------------------------------------------------------------*/

/// Incremental hash update helpers
///
/// These macros fold one changed fact into the running keys rather than
/// hashing the board again, which is what keeps a node's bookkeeping down to
/// a handful of XORs. Making a move spends them. Undoing one does not: the
/// keys come back off the snapshot the move saved, one assignment each.
///
/// ```text
/// hash_in_or_out_piece!     position_hash, pawn_hash
/// hash_toggle_side!         position_hash
/// hash_update_castling!     position_hash
/// hash_update_en_passant!   position_hash
/// hash_update_in_hand!      position_hash
/// set_virgin! clear_virgin! virgin_board, virgin_hash
/// ```
///
/// Nothing here returns a value. A piece change keeps the pawn key in step
/// through a mask rather than a branch, a non-pawn folding a zero into it.
///
/// hash_in_or_out_piece!
///
///   Params:
///   - state       : &mut State -> position whose key is updated
///   - piece_index : usize      -> piece being placed or removed
///   - square_index: Square     -> square the piece enters or leaves
///
/// hash_toggle_side!
///
///   Params:
///   - state: &mut State -> position whose side-to-move key flips
///
/// hash_update_castling!
///
///   Both states are masked to `CASTLE_RIGHTS` first: the castled marks
///   riding in the upper bits are eval-only and are not keyed on, so a move
///   that sets one without spending a right leaves the key alone.
///
///   Params:
///   - state             : &mut State -> position whose key is updated
///   - old_castling_state: u8         -> castling byte before the move
///   - new_castling_state: u8         -> castling byte after the move
///
/// hash_update_en_passant!
///
///   Params:
///   - state        : &mut State      -> position whose key is updated
///   - old_ep_square: EnPassantSquare -> descriptor before the move
///   - new_ep_square: EnPassantSquare -> descriptor after the move
///
/// hash_update_in_hand!
///
///   Params:
///   - state      : &mut State -> position whose key is updated
///   - piece_index: usize      -> piece whose pool count changed
///   - old_count  : u16        -> in-hand count before the change
///   - new_count  : u16        -> in-hand count after the change
///
/// set_virgin! / clear_virgin!
///
///   Mark a square as holding an unmoved piece, or stop marking it, keeping
///   `state.virgin_hash` in step. Each checks the bit first, so a call that
///   changes nothing costs nothing and never desynchronises the key.
///
///   Params:
///   - state       : &mut State -> position whose board and key are updated
///   - square_index: Square     -> square whose unmoved mark changes
#[macro_export]
macro_rules! hash_in_or_out_piece {
    ($state:expr, $piece_index:expr, $square_index:expr) => {{
        let piece_index = $piece_index;
        let piece_hash = PIECE_HASHES[piece_index][$square_index as usize];
        let pawn_mask = (0u128).wrapping_sub(
            ($state.statics.eval.pawn_slots[piece_index] != usize::MAX) as u128
        );

        $state.position_hash ^= piece_hash;
        $state.pawn_hash ^= piece_hash & pawn_mask;
    }};
}

#[macro_export]
macro_rules! hash_toggle_side {
    ($state:expr) => {
        $state.position_hash ^= &*SIDE_HASHES;
    };
}

#[macro_export]
macro_rules! hash_update_castling {
    ($state:expr, $old_castling_state:expr, $new_castling_state:expr) => {{
        let old_rights = $old_castling_state & CASTLE_RIGHTS;
        let new_rights = $new_castling_state & CASTLE_RIGHTS;

        if old_rights != new_rights {
            $state.position_hash ^= &CASTLING_HASHES[old_rights as usize];
            $state.position_hash ^= &CASTLING_HASHES[new_rights as usize];
        }
    }};
}

#[macro_export]
macro_rules! hash_update_en_passant {
    ($state:expr, $old_ep_square:expr, $new_ep_square:expr) => {
        if $old_ep_square != $new_ep_square {
            if $old_ep_square != NO_EN_PASSANT {
                let index = enp_square!($old_ep_square) as usize;
                $state.position_hash ^= &EN_PASSANT_HASHES[index];
            }

            if $new_ep_square != NO_EN_PASSANT {
                let index = enp_square!($new_ep_square) as usize;
                $state.position_hash ^= &EN_PASSANT_HASHES[index];
            }
        }
    };
}

#[macro_export]
macro_rules! hash_update_in_hand {
    ($state:expr, $piece_index:expr, $old_count:expr, $new_count:expr) => {
        if $old_count != $new_count {
            $state.position_hash ^=
                &IN_HAND_HASHES[$piece_index][$old_count as usize];
            $state.position_hash ^=
                &IN_HAND_HASHES[$piece_index][$new_count as usize];
        }
    };
}

#[macro_export]
macro_rules! set_virgin {
    ($state:expr, $square_index:expr) => {
        if !get!($state.virgin_board, $square_index as u32) {
            set!($state.virgin_board, $square_index);
            $state.virgin_hash ^= &VIRGIN_HASHES[$square_index as usize];
        }
    };
}

#[macro_export]
macro_rules! clear_virgin {
    ($state:expr, $square_index:expr) => {
        if get!($state.virgin_board, $square_index as u32) {
            clear!($state.virgin_board, $square_index);
            $state.virgin_hash ^= &VIRGIN_HASHES[$square_index as usize];
        }
    };
}
