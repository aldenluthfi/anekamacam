//! move_trie.rs
//!
//! Builds the move trie of each piece, and makes the moves of a piece on a
//! square by walking it.
//!
//! The vectors of a piece share their first legs: the slides of a rook are
//! copies of one leg, and each slide is a prefix of the next. The trie
//! keeps a shared leg once. So a blocked leg ends all the vectors through
//! it at once, and an open line walks each square once. The walk tests the
//! board edge and the forbidden zones itself, so one trie serves all the
//! squares of the piece.
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

/// TrieEnd
///
/// One vector that ends on a trie node: whether it can capture, and its
/// CPMN condition.
///
pub type TrieEnd = (bool, Option<Arc<PatternSet>>);

/// TrieNode
///
/// One leg of a trie. The children and the ends of a node are runs of
/// `MoveTrie::nodes` and `MoveTrie::ends`.
///
#[derive(Clone)]
pub struct TrieNode {
    pub leg: Leg,                                                               /* the leg of all vectors through it  */
    pub children: u32,                                                          /* first child in `nodes`             */
    pub child_count: u32,                                                       /* children after it, side by side    */
    pub ends: u32,                                                              /* first end in `ends`                */
    pub end_count: u32,                                                         /* ends after it, side by side        */
    pub captures: bool,                                                         /* a capture vector ends at or under  */
}

/// MoveTrie
///
/// The tries of all pieces in one arena. `roots` holds, for each piece,
/// the run of its first legs.
///
#[derive(Default)]
pub struct MoveTrie {
    pub nodes: Vec<TrieNode>,                                                   /* every leg of every piece           */
    pub ends: Vec<TrieEnd>,                                                     /* every vector, on its last leg      */
    pub roots: Vec<(u32, u32)>,                                                 /* piece to first leg run and length  */
}

/*----------------------------------------------------------------------------*\
                                  TRIE BUILDING
\*----------------------------------------------------------------------------*/

/// build_move_trie
///
/// Makes the trie of each piece from its compiled vectors. Vectors with the
/// same first legs share those nodes. The slides of a rook to the east
/// become one line, and a slide ends on each of its nodes:
///
/// ```text
/// vectors                trie
/// W                      W ─ W ─ W ─ W ─ W ─ W ─ W
/// W W                    ●   ●   ●   ●   ●   ●   ●
/// W W W
/// . . .                  ● : a vector ends on this leg
/// W W W W W W W
/// ```
///
/// Params:
/// - piece_moves: &[MoveSet] -> compiled vectors, one set for each piece
///
/// Return:
/// MoveTrie                  -> the tries of all pieces
///
#[hotpath::measure]
pub fn build_move_trie(piece_moves: &[MoveSet]) -> MoveTrie {
    let mut trie = MoveTrie::default();

    for vectors in piece_moves {
        let (nodes, ends, root_count) = build_piece_trie(vectors);
        let node_base = trie.nodes.len() as u32;
        let end_base = trie.ends.len() as u32;

        trie.roots.push((node_base, root_count));
        trie.nodes.extend(nodes.into_iter().map(|mut node| {
            node.children += node_base;
            node.ends += end_base;
            node
        }));
        trie.ends.extend(ends);
    }

    log_3!(
        "Built move trie: {} legs, {} vector ends, {} pieces",
        trie.nodes.len(), trie.ends.len(), trie.roots.len()
    );

    trie
}

/// build_piece_trie
///
/// Makes the trie of one piece. Each vector goes down from the roots,
/// takes a node with its leg when there is one and adds one when there is
/// not, and ends on the node of its last leg. Then the nodes are laid out
/// breadth first, so the children of each node are side by side and the
/// roots come first. Siblings keep the order of their first vectors.
///
/// Params:
/// - vectors: &MoveSet -> compiled vectors of the piece
///
/// Return:
/// (Vec<TrieNode>, Vec<TrieEnd>, u32) -> nodes, ends and root count, with
///                                       indexes local to the piece
///
fn build_piece_trie(vectors: &MoveSet) -> (Vec<TrieNode>, Vec<TrieEnd>, u32) {
    let mut legs: Vec<Leg> = Vec::new();
    let mut kids: Vec<Vec<usize>> = Vec::new();
    let mut tips: Vec<Vec<TrieEnd>> = Vec::new();
    let mut takes: Vec<bool> = Vec::new();
    let mut roots: Vec<usize> = Vec::new();

    for vector in vectors {
        let count = vector.legs.len();
        let captures = vector.legs.iter().enumerate().any(|(index, leg)| {
            c!(*leg) || d!(*leg) || index + 1 == count && !m!(*leg)
        });
        let mut parent: Option<usize> = None;

        for (index, &leg) in vector.legs.iter().enumerate() {
            let siblings = match parent {
                Some(node) => &kids[node],
                None => &roots,
            };
            let node = match siblings.iter().find(|&&node| legs[node] == leg) {
                Some(&node) => node,
                None => {
                    legs.push(leg);
                    kids.push(Vec::new());
                    tips.push(Vec::new());
                    takes.push(false);

                    match parent {
                        Some(above) => kids[above].push(legs.len() - 1),
                        None => roots.push(legs.len() - 1),
                    }

                    legs.len() - 1
                }
            };

            takes[node] |= captures;

            if index + 1 == count {
                tips[node].push((captures, vector.pattern.clone()));
            }

            parent = Some(node);
        }
    }

    let root_count = roots.len() as u32;
    let mut order = roots;
    let mut nodes = Vec::with_capacity(legs.len());
    let mut ends = Vec::new();
    let mut cursor = 0;

    while cursor < order.len() {                                                /* the order grows as it is read, so  */
        let old = order[cursor];                                                /* a node's children follow as a run  */
        let children = order.len() as u32;

        order.extend_from_slice(&kids[old]);
        nodes.push(TrieNode {
            leg: legs[old],
            children,
            child_count: kids[old].len() as u32,
            ends: ends.len() as u32,
            end_count: tips[old].len() as u32,
            captures: takes[old],
        });
        ends.append(&mut tips[old]);
        cursor += 1;
    }

    (nodes, ends, root_count)
}

/*----------------------------------------------------------------------------*\
                                  TRIE WALKING
\*----------------------------------------------------------------------------*/

/// walk_move_trie
///
/// Makes the moves of one piece on one square from its trie, depth first:
///
/// - a node with ends     : its leg is read as a last leg, and each end
///                          whose condition holds gives its moves
/// - a node with children : its leg is read as a middle leg, and the walk
///                          goes down only when it passes
///
/// The moves come in walk order: each line from the origin outwards, and
/// the lines in the order of the first vector on each.
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
/// of `relevant_moves` of the square: a leg off the board or into a
/// forbidden zone ends its vectors as the table would leave them out, and
/// `match_pattern!` reads a condition with the bounds that `clip_pattern`
/// fits at load.
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
    let run = state.statics.move_trie.roots[p_index!(piece) as usize];
    let mover: WalkOrigin = walk_origin!(square_index, piece, state);
    let mut encoded_move = Move::default();

    enc_start!(encoded_move, square_index as u128);
    enc_piece!(encoded_move, mover.1 as u128);
    scratch.clear();

    walk_trie_run(
        state, piece, mover, run, walk_start!(mover, state), &encoded_move,
        out, scratch, captures_only,
    );
}

/// walk_trie_run
///
/// Walks one run of sibling nodes and, under each node that passes as a
/// middle leg, its children. The taken pieces go back to their state
/// before each node, so each sibling starts from the same path.
///
/// Params:
/// - state        : &State         -> current position
/// - piece        : &Piece         -> moving piece type
/// - mover        : WalkOrigin     -> facts of the origin
/// - run          : (u32, u32)     -> first node and count
/// - walk         : WalkState      -> state before the run
/// - encoded_move : &Move          -> move before the run
/// - out          : &mut Vec<Move> -> list that gets the moves
/// - scratch      : &mut Vec<u64>  -> pieces taken on the path
/// - captures_only: bool           -> walk only capture vectors
///
#[allow(clippy::too_many_arguments)]
fn walk_trie_run(
    state: &State,
    piece: &Piece,
    mover: WalkOrigin,
    run: (u32, u32),
    walk: WalkState,
    encoded_move: &Move,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
    captures_only: bool,
) {
    let trie = &state.statics.move_trie;

    for index in run.0..run.0 + run.1 {
        let node = &trie.nodes[index as usize];

        if captures_only && !node.captures {
            continue;
        }

        let taken = scratch.len();
        let top = scratch.last().copied();

        if node.end_count > 0 {
            let mut end_walk = walk;
            let mut end_move = encoded_move.clone();

            if walk_leg!(
                &node.leg, true, mover, &mut end_walk,
                state, &mut end_move, scratch
            ) {
                let first_end = node.ends as usize;
                let ends = first_end..first_end + node.end_count as usize;

                for (captures, pattern) in &trie.ends[ends] {
                    if captures_only && !captures
                    || !match_pattern_set!(pattern, mover.0, mover.2, state)
                    {
                        continue;
                    }

                    push_walked_move!(
                        piece, state, end_move.clone(), end_walk,
                        scratch, out, true
                    );
                }
            }

            restore_taken(scratch, taken, top);
        }

        if node.child_count > 0 {
            let mut path_walk = walk;
            let mut path_move = encoded_move.clone();

            if walk_leg!(
                &node.leg, false, mover, &mut path_walk,
                state, &mut path_move, scratch
            ) {
                walk_trie_run(
                    state, piece, mover, (node.children, node.child_count),
                    path_walk, &path_move, out, scratch, captures_only,
                );
            }

            restore_taken(scratch, taken, top);
        }
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
