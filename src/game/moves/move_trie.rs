//! move_trie.rs
//!
//! Builds the move trie of each piece, and makes the moves of a piece on a
//! square by walking it.
//!
//! A piece moves the same on every square. Its vectors share their first
//! legs: the slides of a rook are copies of one leg, and each slide is a
//! prefix of the next. The trie keeps a shared leg once, and the walk goes
//! no deeper where a leg leaves the board, enters a forbidden zone, is
//! blocked, or where the condition of its branch fails.
//!
//! Created: 13/10/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              MOVE TRIE REPRESENTATION
\*----------------------------------------------------------------------------*/

/// WalkOrigin
///
/// The facts of `walk_origin!`: origin square, piece index, colour and
/// rank, unmoved, promotable, and whether the origin is in the mandatory
/// and in the optional promotion zone.
///
pub type WalkOrigin = (u32, PieceIndex, u8, u8, bool, bool, bool, bool);

/// WalkState
///
/// The state of a walk after a leg: the square it reached, its file and
/// its rank, and whether a promotion is mandatory or optional so far.
///
pub type WalkState = (i16, i16, i16, bool, bool);

/// TrieNode
///
/// One leg of the trie of a piece. A branch starts at a root node, and all
/// the nodes of the branch share the CPMN condition of its root. A vector
/// with another condition starts its own branch. The slides of a rook to
/// the east are one branch, and a slide ends on each of its nodes:
///
/// ```text
/// root (no condition)
///  └─ W ● ─ W ● ─ W ● ─ W ● ─ W ● ─ W ● ─ W ●
///
/// ● : a vector ends on this leg (`can_end`)
/// ```
///
pub struct TrieNode {
    pub leg: Leg,                                                               /* the step of this node              */
    pub condition: Option<Arc<PatternSet>>,                                     /* on a root: the branch's condition  */
    pub children: Vec<TrieNode>,                                                /* the next legs                      */
    pub can_end: bool,                                                          /* a vector ends on this leg          */
    pub has_capture: bool,                                                      /* a capture vector ends at or under  */
}

/// MoveTrie
///
/// The root nodes of the trie of each piece, by piece index.
///
pub type MoveTrie = Vec<Vec<TrieNode>>;

/*----------------------------------------------------------------------------*\
                                  TRIE BUILDING
\*----------------------------------------------------------------------------*/

/// build_move_trie
///
/// Makes the trie of each piece from its compiled vectors. Each vector goes
/// down from the roots: a root with its first leg and its condition, then a
/// child with each next leg, made when there is none. Its last node can end
/// a move. A vector that can capture marks each node on its way.
///
/// Params:
/// - piece_moves: &[MoveSet] -> compiled vectors, one set for each piece
///
/// Return:
/// MoveTrie                  -> the roots of the trie of each piece
///
#[hotpath::measure]
pub fn build_move_trie(piece_moves: &[MoveSet]) -> MoveTrie {
    piece_moves.iter().map(|vectors| {
        let mut roots: Vec<TrieNode> = Vec::new();

        for vector in vectors {
            let count = vector.legs.len();
            let captures = vector.legs.iter().enumerate().any(|(index, leg)| {
                c!(*leg) || d!(*leg) || index + 1 == count && !m!(*leg)
            });
            let mut level = &mut roots;

            for (index, &leg) in vector.legs.iter().enumerate() {
                let condition = match index {
                    0 => vector.pattern.clone(),
                    _ => None,
                };
                let found = level.iter().position(|node| {
                    node.leg == leg && match (&node.condition, &condition) {
                        (None, None) => true,
                        (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                        _ => false,
                    }
                });
                let at = found.unwrap_or_else(|| {
                    level.push(TrieNode {
                        leg,
                        condition,
                        children: Vec::new(),
                        can_end: false,
                        has_capture: false,
                    });
                    level.len() - 1
                });
                let node = &mut level[at];

                node.can_end |= index + 1 == count;
                node.has_capture |= captures;
                level = &mut node.children;
            }
        }

        roots
    }).collect()
}

/*----------------------------------------------------------------------------*\
                                  TRIE WALKING
\*----------------------------------------------------------------------------*/

/// walk_move_trie
///
/// Makes the moves of one piece on one square from its trie, depth first.
///
/// Params:
/// - state        : &State         -> current position
/// - piece        : &Piece         -> moving piece type
/// - square_index : Square         -> origin square of the piece
/// - out          : &mut Vec<Move> -> list that gets the moves
/// - scratch      : &mut Vec<u64>  -> pieces taken on the current path
/// - captures_only: bool           -> walk only the capture vectors
///
#[inline]
pub fn walk_move_trie(
    state: &State,
    piece: &Piece,
    square_index: Square,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
    captures_only: bool,
) {
    let roots = &state.statics.move_trie[p_index!(piece) as usize];
    let mover: WalkOrigin = walk_origin!(square_index, piece, state);
    let mut encoded_move = Move::default();

    enc_start!(encoded_move, square_index as u128);
    enc_piece!(encoded_move, mover.1 as u128);
    scratch.clear();

    walk_trie_nodes(
        state, piece, mover, roots, walk_start!(mover, state), &encoded_move,
        out, scratch, captures_only,
    );
}

/// walk_trie_nodes
///
/// Walks sibling nodes and their children. A node and all its children are
/// skipped when:
///
/// - captures_only : no capture vector ends on or under it
/// - edge or zone  : its leg leaves the board or enters a forbidden zone
/// - condition     : the condition of its branch fails at the origin
/// - blocked       : its leg fails as a middle leg
///
/// A node is read once as a middle leg. A vector that ends on it makes its
/// move from that read, unless the square holds a piece or is the en
/// passant square of the leg: there a last leg can capture where a middle
/// leg cannot, so the node is read again as a last leg.
///
/// Params:
/// - state        : &State         -> current position
/// - piece        : &Piece         -> moving piece type
/// - mover        : WalkOrigin     -> facts of the origin
/// - nodes        : &[TrieNode]    -> the siblings to walk
/// - walk         : WalkState      -> state before their leg
/// - encoded_move : &Move          -> move before their leg
/// - out          : &mut Vec<Move> -> list that gets the moves
/// - scratch      : &mut Vec<u64>  -> pieces taken on the path
/// - captures_only: bool           -> walk only capture vectors
///
#[allow(clippy::too_many_arguments)]
fn walk_trie_nodes(
    state: &State,
    piece: &Piece,
    mover: WalkOrigin,
    nodes: &[TrieNode],
    walk: WalkState,
    encoded_move: &Move,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
    captures_only: bool,
) {
    for node in nodes {
        if captures_only && !node.has_capture
        || !leg_lands!(&node.leg, mover, walk, state)
        || !match_pattern_set!(&node.condition, mover.0, mover.2, state)
        {
            continue;
        }

        let taken = scratch.len();
        let top = scratch.last().copied();
        let sign = -2 * mover.2 as i16 + 1;
        let square = ((walk.2 + y!(&node.leg) as i16 * sign)
            * state.statics.files as i16
            + walk.1 + x!(&node.leg) as i16 * sign) as u32;
        let reread = node.can_end && (
            get!(state.pieces_board[0], square)
                || get!(state.pieces_board[1], square)
                || t!(&node.leg)
                    && enp_square!(state.en_passant_square) == square
        );

        if reread {
            let mut end_walk = walk;
            let mut end_move = encoded_move.clone();

            if walk_leg!(
                &node.leg, true, mover, &mut end_walk,
                state, &mut end_move, scratch
            ) {
                push_walked_move!(
                    piece, state, end_move, end_walk, scratch, out, true
                );
            }

            restore_taken(scratch, taken, top);
        }

        let mut path_walk = walk;
        let mut path_move = encoded_move.clone();

        if walk_leg!(
            &node.leg, false, mover, &mut path_walk,
            state, &mut path_move, scratch
        ) {
            if node.can_end && !reread {
                push_walked_move!(
                    piece, state, path_move.clone(), path_walk,
                    scratch, out, true
                );
            }

            walk_trie_nodes(
                state, piece, mover, &node.children, path_walk, &path_move,
                out, scratch, captures_only,
            );
        }

        restore_taken(scratch, taken, top);
    }
}

/// restore_taken
///
/// Puts the taken pieces back as they were before a leg. A leg can add one
/// piece, and an unload can take the last piece off or change it.
///
/// Params:
/// - scratch: &mut Vec<u64> -> pieces taken on the path
/// - taken  : usize         -> their count before the leg
/// - top    : Option<u64>   -> the last of them before the leg
///
#[inline]
fn restore_taken(scratch: &mut Vec<u64>, taken: usize, top: Option<u64>) {
    scratch.truncate(taken);

    if let Some(top) = top {
        match scratch.len() < taken {
            true => scratch.push(top),
            false => scratch[taken - 1] = top,
        }
    }
}
