//! move_trie.rs
//!
//! Builds the move trie of each piece and square, and makes the moves of a
//! piece by walking it.
//!
//! The vectors of a piece on a square share their first legs: the slides of
//! a rook are copies of one leg, and each slide is a prefix of the next. The
//! trie keeps a shared leg once. So a blocked leg ends all the vectors
//! through it at once, and an open line walks each square once.
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
/// The state of a walk after a leg: the square it reached, and whether a
/// promotion is mandatory or optional so far.
///
pub type WalkState = (i16, bool, bool);

/// TrieEnd
///
/// One vector that ends on a trie node: its rank in the `relevant_moves`
/// of its entry, whether it can capture, and its CPMN condition.
///
pub type TrieEnd = (usize, bool, Option<Arc<PatternSet>>);

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
/// The tries of all pieces and squares in one arena. `roots` holds, for
/// each entry `piece * board_size + square`, the run of its first legs.
///
#[derive(Default)]
pub struct MoveTrie {
    pub nodes: Vec<TrieNode>,                                                   /* every leg of every entry           */
    pub ends: Vec<TrieEnd>,                                                     /* every vector, on its last leg      */
    pub roots: Vec<(u32, u32)>,                                                 /* entry to first leg run and length  */
}

/*----------------------------------------------------------------------------*\
                                  TRIE BUILDING
\*----------------------------------------------------------------------------*/

/// build_move_trie
///
/// Makes the trie of each entry from its `relevant_moves`, which are fitted
/// to the board, the zones and the conditions of the square. Vectors with
/// the same first legs share those nodes. The slides of a rook to the east
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
/// - state: &State -> variant with the `relevant_moves` tables
///
/// Return:
/// MoveTrie        -> the tries of all entries
///
/// Notes:
/// Each entry is built on its own thread, and the entries are filed in
/// order.
///
#[hotpath::measure]
pub fn build_move_trie(state: &State) -> MoveTrie {
    let parts: Vec<(Vec<TrieNode>, Vec<TrieEnd>, u32)> = state.statics
        .relevant_moves
        .par_iter()
        .map(|vectors| build_entry_trie(vectors))
        .collect();

    let mut trie = MoveTrie::default();

    for (nodes, ends, root_count) in parts {
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

    trie
}

/// build_entry_trie
///
/// Makes the trie of one entry. Each vector goes down from the roots,
/// takes a node with its leg when there is one and adds one when there is
/// not, and ends on the node of its last leg. Then the nodes are laid out
/// breadth first, so the children of each node are side by side and the
/// roots come first.
///
/// Params:
/// - vectors: &MoveSet -> vectors of the entry, in their table order
///
/// Return:
/// (Vec<TrieNode>, Vec<TrieEnd>, u32) -> nodes, ends and root count, with
///                                       indexes local to the entry
///
fn build_entry_trie(vectors: &MoveSet) -> (Vec<TrieNode>, Vec<TrieEnd>, u32) {
    let mut legs: Vec<Leg> = Vec::new();
    let mut kids: Vec<Vec<usize>> = Vec::new();
    let mut tips: Vec<Vec<TrieEnd>> = Vec::new();
    let mut takes: Vec<bool> = Vec::new();
    let mut roots: Vec<usize> = Vec::new();

    for (rank, vector) in vectors.iter().enumerate() {
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
                tips[node].push((rank, captures, vector.pattern.clone()));
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
/// The walk makes the moves in its own order. Each move keeps the rank of
/// its vector in `ranks`, and the new part of `out` is then put in rank
/// order, the order of the vector tables, which the staged generation of
/// `alpha_beta` needs.
///
/// Params:
/// - state        : &State           -> current position
/// - piece        : &Piece           -> moving piece type
/// - square_index : Square           -> origin square of the piece
/// - out          : &mut Vec<Move>   -> list that gets the moves
/// - scratch      : &mut Vec<u64>    -> pieces taken on the current path
/// - ranks        : &mut Vec<usize>  -> rank of each new move, then cleared
/// - captures_only: bool             -> walk only the capture vectors
///
/// Notes:
/// The moves are those of `process_multi_leg_vector!` for each vector, in
/// the same order.
///
#[inline]
pub fn walk_move_trie(
    state: &State,
    piece: &Piece,
    square_index: Square,
    out: &mut Vec<Move>,
    scratch: &mut Vec<u64>,
    ranks: &mut Vec<usize>,
    captures_only: bool,
) {
    let trie = &state.statics.move_trie;
    let entry = p_index!(piece) as usize * state.statics.board_size
        + square_index as usize;
    let (first, count) = trie.roots[entry];
    let mover: WalkOrigin = walk_origin!(square_index, piece, state);
    let start = out.len();
    let mut encoded_move = Move::default();

    enc_start!(encoded_move, square_index as u128);
    enc_piece!(encoded_move, mover.1 as u128);
    scratch.clear();
    ranks.clear();

    walk_trie_run(
        state, piece, mover, (first, count),
        (square_index as i16, false, false), &encoded_move,
        (out, scratch, ranks), start, captures_only,
    );

    let moves = out.len() - start;

    if ranks.windows(2).any(|pair| pair[0] > pair[1]) {
        for (index, rank) in ranks.iter_mut().enumerate() {                     /* unique keys: rank first, then the  */
            *rank = *rank * moves + index;                                      /* walk order within one vector       */
        }

        ranks.sort_unstable();

        for cycle in 0..moves {                                                 /* slot j takes the move that key j   */
            if ranks[cycle] == usize::MAX {                                     /* names, one cycle at a time         */
                continue;
            }

            let mut slot = cycle;

            loop {
                let source = ranks[slot] % moves;

                ranks[slot] = usize::MAX;

                if source == cycle {
                    break;
                }

                out.swap(start + slot, start + source);
                slot = source;
            }
        }
    }

    ranks.clear();
}

/// walk_trie_run
///
/// Walks one run of sibling nodes and, under each node that passes as a
/// middle leg, its children. The taken pieces go back to their state
/// before each node, so each sibling starts from the same path.
///
/// Params:
/// - state        : &State                     -> current position
/// - piece        : &Piece                     -> moving piece type
/// - mover        : WalkOrigin                 -> facts of the origin
/// - run          : (u32, u32)                 -> first node and count
/// - walk         : WalkState                  -> state before the run
/// - encoded_move : &Move                      -> move before the run
/// - lists        : (&mut Vec<Move>,           -> moves, taken pieces and
///                   &mut Vec<u64>,               the rank of each move
///                   &mut Vec<usize>)
/// - start        : usize                      -> first move of the walk
/// - captures_only: bool                       -> walk only capture vectors
///
#[allow(clippy::too_many_arguments)]
fn walk_trie_run(
    state: &State,
    piece: &Piece,
    mover: WalkOrigin,
    run: (u32, u32),
    walk: WalkState,
    encoded_move: &Move,
    lists: (&mut Vec<Move>, &mut Vec<u64>, &mut Vec<usize>),
    start: usize,
    captures_only: bool,
) {
    let trie = &state.statics.move_trie;
    let (out, scratch, ranks) = lists;

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

                for (rank, captures, pattern) in &trie.ends[ends] {
                    if captures_only && !captures
                    || !match_pattern_set!(pattern, mover.0, mover.2, state)
                    {
                        continue;
                    }

                    push_walked_move!(
                        piece, state, end_move.clone(), end_walk,
                        scratch, out, true
                    );
                    ranks.resize(out.len() - start, *rank);
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
                    path_walk, &path_move,
                    (&mut *out, &mut *scratch, &mut *ranks), start,
                    captures_only,
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
