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
/// One leg of the trie of a piece. The nodes lie in depth-first order, so
/// the nodes under a node follow it, `size` of them with itself. A leg that
/// fails skips its subtree by a jump of `size`. The slides of a rook to the
/// east are one branch, and a slide ends on each of its nodes:
///
/// ```text
/// node   W ● ─ W ● ─ W ● ─ W ● ─ W ● ─ W ● ─ W ●
/// size   7     6     5     4     3     2     1
///
/// ● : a vector ends on this leg (`can_end`)
/// ```
///
pub struct TrieNode {
    pub leg: Leg,                                                               /* the step of this node              */
    pub size: u32,                                                              /* this node and the nodes under it   */
    pub can_end: bool,                                                          /* a vector ends on this leg          */
    pub has_capture: bool,                                                      /* a capture vector ends at or under  */
}

/// MoveTrie
///
/// The trie of one piece: its nodes in depth-first order, and its branches,
/// each the index of a root node with the CPMN condition that all the nodes
/// of the branch share. A vector with another condition starts its own
/// branch.
///
pub struct MoveTrie {
    pub nodes: Vec<TrieNode>,                                                   /* every leg, depth first             */
    pub branches: Vec<(u32, Option<Arc<PatternSet>>)>,                          /* root index and its condition       */
}

/*----------------------------------------------------------------------------*\
                                  TRIE BUILDING
\*----------------------------------------------------------------------------*/

/// build_move_trie
///
/// Makes the trie of one piece from its compiled vectors, straight into its
/// depth-first node list. Each vector goes down its branch (the root with
/// its first leg and its condition) through the children with its next
/// legs; the child of node `i` is `i + 1`, and the next sibling of a child
/// follows its subtree. The legs it does not find go in as one chain at the
/// end of the subtree it reached, and each node on its path grows by that
/// chain. A vector with no branch gets its own at the end of the list.
///
/// Params:
/// - vectors: &MoveSet -> compiled vectors of the piece
///
/// Return:
/// MoveTrie            -> the trie of the piece
///
pub fn build_move_trie(vectors: &MoveSet) -> MoveTrie {
    let mut trie = MoveTrie { nodes: Vec::new(), branches: Vec::new() };

    for vector in vectors {
        let legs = &vector.legs[..];
        let count = legs.len();

        if count == 0 {
            continue;
        }

        let captures = legs.iter().enumerate().any(|(index, leg)| {
            c!(*leg) || d!(*leg) || index + 1 == count && !m!(*leg)
        });
        let chain = |from: usize| legs[from..].iter().enumerate()
            .map(move |(index, &leg)| TrieNode {
                leg,
                size: (count - from - index) as u32,
                can_end: from + index + 1 == count,
                has_capture: captures,
            });
        let nodes = &mut trie.nodes;
        let root = trie.branches.iter().find(|(at, condition)| {
            nodes[*at as usize].leg == legs[0]
                && match (condition, &vector.pattern) {
                    (None, None) => true,
                    (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                    _ => false,
                }
        }).map(|(at, _)| *at as usize);

        let Some(root) = root else {
            trie.branches.push((nodes.len() as u32, vector.pattern.clone()));
            nodes.extend(chain(0));
            continue;
        };

        let mut path = vec![root];
        let mut depth = 1;

        while depth < count {
            let at = path[path.len() - 1];
            let end = at + nodes[at].size as usize;
            let mut child = at + 1;

            while child < end && nodes[child].leg != legs[depth] {
                child += nodes[child].size as usize;
            }

            if child >= end {
                break;
            }

            path.push(child);
            depth += 1;
        }

        let last = path[path.len() - 1];
        let start = last + nodes[last].size as usize;
        let grown = (count - depth) as u32;

        nodes.splice(start..start, chain(depth));

        for &at in &path {
            nodes[at].size += grown;
            nodes[at].has_capture |= captures;
        }

        nodes[last].can_end |= depth == count;

        for (at, _) in trie.branches.iter_mut() {
            if *at as usize >= start {
                *at += grown;
            }
        }
    }

    trie
}

/*----------------------------------------------------------------------------*\
                                  TRIE WALKING
\*----------------------------------------------------------------------------*/

/// walk_move_trie
///
/// Makes the moves of one piece on one square from its trie, depth first.
/// A branch whose condition fails at the origin is skipped whole.
///
/// Params:
/// - state        : &State         -> current position
/// - piece        : &Piece         -> moving piece type
/// - square_index : Square         -> origin square of the piece
/// - out          : &mut Vec<Move> -> list that gets the moves
/// - scratch      : &mut Vec<u64>  -> pieces taken on the current path
/// - captures_only: bool           -> walk only the capture vectors
///
/// Notes:
/// The set of moves is that of `process_multi_leg_vector!` for each vector
/// of `fitted_vectors` of the square: a leg off the board or into a
/// forbidden zone ends its vectors as the fit would leave them out, and
/// `match_pattern!` reads a condition with the bounds that `clip_pattern`
/// fits.
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
    let trie = &state.statics.move_tries[p_index!(piece) as usize];
    let mover: WalkOrigin = walk_origin!(square_index, piece, state);
    let walk = walk_start!(mover, state);
    let mut encoded_move = Move::default();

    enc_start!(encoded_move, square_index as u128);
    enc_piece!(encoded_move, mover.1 as u128);
    scratch.clear();

    for (root, condition) in &trie.branches {
        if match_pattern_set!(condition, mover.0, mover.2, state) {
            walk_trie_node(
                state, piece, mover, &trie.nodes, *root as usize, walk,
                &encoded_move, out, scratch, captures_only,
            );
        }
    }
}

/// walk_trie_node
///
/// Walks one node and the nodes under it. The node and all the nodes under
/// it are skipped when:
///
/// - captures_only : no capture vector ends on or under it
/// - edge or zone  : its leg leaves the board or enters a forbidden zone
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
/// - nodes        : &[TrieNode]    -> the nodes of the trie
/// - at           : usize          -> index of the node to walk
/// - walk         : WalkState      -> state before its leg
/// - encoded_move : &Move          -> move before its leg
/// - out          : &mut Vec<Move> -> list that gets the moves
/// - scratch      : &mut Vec<u64>  -> pieces taken on the path
/// - captures_only: bool           -> walk only capture vectors
///
/// Notes:
/// It is inlined into its callers, so a node without children costs no
/// call. Only a node with children calls `walk_trie_children`.
///
#[inline(always)]
fn walk_trie_node(
    state: &State,
    piece: &Piece,
    mover: WalkOrigin,
    nodes: &[TrieNode],
    at: usize,
    walk: WalkState,
    encoded_move: &Move,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
    captures_only: bool,
) {
    let node = &nodes[at];

    if captures_only && !node.has_capture
    || !leg_lands!(&node.leg, mover, walk, state)
    {
        return;
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
            || t!(&node.leg) && enp_square!(state.en_passant_square) == square
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

        if node.size > 1 {
            walk_trie_children(
                state, piece, mover, nodes, at, path_walk, &path_move,
                out, scratch, captures_only,
            );
        }
    }

    restore_taken(scratch, taken, top);
}

/// walk_trie_children
///
/// Walks the children of a node: the first follows it, and each next one
/// follows the subtree of the one before.
///
/// Params:
/// - state        : &State         -> current position
/// - piece        : &Piece         -> moving piece type
/// - mover        : WalkOrigin     -> facts of the origin
/// - nodes        : &[TrieNode]    -> the nodes of the trie
/// - parent       : usize          -> index of the node whose children run
/// - walk         : WalkState      -> state after the parent's leg
/// - encoded_move : &Move          -> move after the parent's leg
/// - out          : &mut Vec<Move> -> list that gets the moves
/// - scratch      : &mut Vec<u64>  -> pieces taken on the path
/// - captures_only: bool           -> walk only capture vectors
///
fn walk_trie_children(
    state: &State,
    piece: &Piece,
    mover: WalkOrigin,
    nodes: &[TrieNode],
    parent: usize,
    walk: WalkState,
    encoded_move: &Move,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
    captures_only: bool,
) {
    let end = parent + nodes[parent].size as usize;
    let mut at = parent + 1;

    while at < end {
        walk_trie_node(
            state, piece, mover, nodes, at, walk, encoded_move,
            out, scratch, captures_only,
        );
        at += nodes[at].size as usize;
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
