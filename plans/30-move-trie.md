# Move trie

## Status

Opened 2026-10-13 on build 22 (main 1db0677), branch `plan30-trie`.

## Context

Move generation walks each precomputed vector of a piece on its square one
by one: `generate_move_list!` and `generate_capture_list!` call
`process_multi_leg_vector!` for each vector of `relevant_moves` or
`relevant_captures` (`src/game/moves/move_list.rs`). A slide compiles as
copies of one leg (`process_multi_leg_dots_token`, `move_parse.rs`), so a
rook on a square holds, in each direction, seven vectors of 1 to 7 legs,
each a prefix of the next:

- first square blocked : seven vectors set up, each stops at its first leg
- open board           : 1 + 2 + ... + 7 = 28 legs walked where 7 do

Generation is the largest cost measured so far (build 20 profile, plan 29:
full generation 20.5, 24.4 and 30.9% of the time in standard, xiangqi and
shogi, capture generation 15.0, 9.2 and 5.7%).

The user's idea: a trie of the vectors of each piece and square, where the
children of a node are pruned when its leg fails. No earlier try of prefix
sharing is on record; CX and AF (plan 29) skip a whole piece, not a
prefix.

Goal: the same moves in the same order, so the search visits the same
nodes and the change merges on server nodes per second, with fewer legs
walked and fewer vector setups.

## Why pruning is exact

- The children of a node share its exact leg word. A failed leg at depth
  k fails the same way for every vector through that node, because the
  walk state there (square, captured list, promotion flags) comes only
  from the legs before it.
- A leg reads differently only as the last leg: `c = c!(leg) || (last &&
  !m!(leg))`. A node is therefore read as a middle leg to go down, and as
  a last leg for the vectors that end on it.
- The CPMN condition of a vector depends on the origin and the position,
  not on the walk. Tested at the end node before the move is made, it
  gives the same result as tested first.
- `enc_is_initial!`, `enc_creates_enp!` and `enc_created_enp!` OR their
  bits into the move at each leg, so the move gathers them over the whole
  path; with `mandatory` and `optionals` it is path state, saved before a
  node and put back for its next sibling. (The first version of this plan
  said only the last leg set them; the macros say otherwise.)

## Design

`src/game/moves/move_trie.rs`:

- `MoveTrie`: one flat arena for all (piece, square) entries.
  - `nodes`: every node; the children of a node are contiguous
  - `roots`: for each entry, its first child and child count, indexed
    `piece * board_size + square` as `relevant_moves`
  - `ends`: for each vector that ends on a node, its rank in
    `relevant_moves` of the entry (longest first, as now) and its CPMN
    condition; two vectors with the same legs and different conditions
    are two ends of one node
  - `captures` on each node: its subtree holds the end of a vector of
    `relevant_captures`, so capture generation skips the other subtrees
- `build_move_trie`: from the finished `relevant_moves`, one entry for
  each thread, filed in entry order, in `State::precompute` after
  `relevant_captures`.
- The walk goes depth first with a stack of saved path state (square,
  promotion flags, the move so far, the length and top of the taken
  list). At a node
  with ends, it reads the leg as a last leg and makes the move of each
  end whose condition holds. At a node with children, it reads the leg as
  a middle leg and goes down only when it passes.
- Order: each move keeps the rank of its end, and the new part of the
  list is put back in rank order after the walk. The staged generation of
  `alpha_beta` needs today's order.
- `walk_origin!`, `walk_leg!` and `push_walked_move!`: the origin facts,
  the leg body and the move output of `process_multi_leg_vector!`, shared
  by the trie walk and `process_multi_leg_vector!`, which `lva!` still
  uses for single attack vectors.

Not in this plan: the attack side (`relevant_attacks`,
`validate_attack_vector!`, `is_square_attacked!`, `lva!`), drops and
castling.

## Stages

1. `walk_leg!` taken out of `process_multi_leg_vector!`, no other change:
   the same nodes, the same speed.
2. `move_trie.rs`: types, `build_move_trie`, the walk with the rank
   order; generation switched to it.
3. If the rank order costs more than about 1% in nodes per second, a
   cheaper exact order (rank buckets). An order without it changes the
   trees and needs games against full FSF: a separate candidate.
4. Results here; merge as build 23 on the speed result.

## Verification

All on the server, in parallel, each run with a time limit.

- Exactness: all 17 perft suites; taikyokushogi perft 2 = 708347,
  chushogi 4 = 1774907, daishogi 4 = 25360784, daidaishogi 3 = 186662
  (plan 24); seeded 40-ply games at a fixed depth equal build 22 in all
  44 variants.
- Speed: seeded bench against build 22, interleaved, 16 seeds, in
  standard, xiangqi, grand and shogi; time for a fixed node count in all
  44 variants.
- Memory: taikyoku peak alone (build 22: 14.2 GB). If the trie raises it
  much, capture generation reads the `captures` flags and
  `relevant_captures` is not kept at run time.
- Merge: the same nodes, faster on the mean of the four key variants,
  and not slower beyond noise in any.
