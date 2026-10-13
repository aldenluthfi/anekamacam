# Move trie

## Status

Opened and closed 2026-10-13 on build 22 (main 1db0677), branch
`plan30-trie`. Not kept. The tries of the squares walk faster than the
vector tables but add their memory on top; one trie for each piece halves
taikyoku's memory but walks 4 to 9% slower than the tables, and in games
against full FSF it is -6.7, near ± 14. The user closed the plan; build 22
keeps its vector walk.

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
3. Decided by the user after stage 2 (see Results): the walk order is the
   move order, and the trie is one for each piece, not for each square.
   - No rank buffer and no reorder; the `ranks` argument goes again.
   - `build_move_trie` reads the compiled vectors of each piece, before
     they are fitted to the squares.
   - `walk_leg!` keeps the file and rank of the walk and ends a leg that
     leaves the board or enters a forbidden zone (unless it may pass
     one), the tests `generate_relevant_moves` makes at load. A vector
     fitted to its square passes them all, so `lva!` is unchanged.
   - `match_pattern!` tests the bounds itself: an allower off the board
     fails its pattern, a stopper off the board is ignored, as
     `clip_pattern` gives. A fitted pattern gives the same answer.
   - The trees change, so it merges on games against full FSF,
     interleaved with build 22, three rounds of 200 in each key variant
     (`nodestime` 14, 16, 29, 44), at +2σ on the mean; perft must still
     equal build 22 everywhere.
4. No per-square vector tables (asked by the user after stage 3). With
   the trie, `relevant_moves` and `relevant_captures` are read only at
   load: the attack masks, the attack recipes, the capture reach and the
   derivation in `parameters.rs` (again by `tune`). On taikyoku they hold
   71 M fitted vectors twice, most of its 13.4 GB.
   - `StaticState` keeps `piece_moves`, the compiled vectors of each piece,
     in place of the two tables.
   - `fitted_vectors` and `fitted_captures` give the vectors of a piece
     fitted to one square on demand, with `generate_relevant_moves` and
     `generate_relevant_captures` as now, so each reader sees the same
     vectors in the same order.
   - Check: the load logs (every derived value) equal stage 3 line for
     line in all 44 variants; perft and seeded games equal stage 3;
     taikyoku's peak memory and load time against stage 3.
5. Results here; merge as build 23.

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

## Results

The checks run on the server with `~/p27b/ident.sh` (seeded games and
perft against a base, each run in its own working folder) and
`~/p27b/nps.sh` (seeded bench, 16 seeds, alternating).

### Stage 1 (badad6e)

- `walk_origin!`, `walk_leg!` and `push_walked_move!` taken out of
  `process_multi_leg_vector!`.
- The same as build 22: 43 seeded games (taikyoku left out), 17 perft
  suites at depth 3, chushogi 4, daishogi 4, daidaishogi 3, taikyoku 2.
- Nodes per second against build 22: standard x1.019 ± 0.025, xiangqi
  x1.013 ± 0.015, grand x0.997 ± 0.014, shogi x1.005 ± 0.014.
- The first run of the check showed 28 games different. The engines
  shared one working folder and raced to roll the same log file, and the
  ones that lost panicked; each run now has its own folder, and a crash
  shows as a crash.

### Stage 2 (d4b8a5c)

- The same as build 22 in the 43 games and the 20 perfts on boards up to
  daidaishogi. Taikyoku perft 2 was killed for memory.
- Nodes per second against build 22: standard x0.958 ± 0.017, xiangqi
  x1.167 ± 0.018, grand x1.030 ± 0.022, shogi x0.965 ± 0.018. Not merged.
- Why standard and shogi lose: the rank order. A build without it (not
  exact, for the measure only) against stage 2: standard x1.047 ± 0.021,
  xiangqi x1.051 ± 0.013, grand x1.029 ± 0.015, shogi x1.044 ± 0.037. So
  the walk alone is near x1.00 in standard and shogi, x1.23 in xiangqi
  and x1.06 in grand, and keeping the order of build 22 costs near 4%:
  each slider square sorts and permutes its moves at each generation.
- Why the walk gains little in standard: 7836 legs for 7740 vector ends.
  A slide ends a vector on each of its nodes, so the trie saves leg
  reads, not nodes; the gain grows with long open lines (xiangqi).
- Memory, taikyoku loaded alone (`state`): build 22 peaks at 13.4 GB.
  With the trie the load was killed above 15.6 GB: 85.2 M legs and 71.4
  M vector ends, near 2.0 and 1.7 GB at 24 bytes each, plus the copies of
  the build. The per-square tables are what fill taikyoku's memory: 71 M
  vectors are kept once in `relevant_moves`, again in
  `relevant_captures`, and now in the trie.

### Stage 3 (0e7cabb, 0473c4b)

- One trie for each piece: 1752 legs in standard, against 7836 for the
  tries of all squares.
- Perft equals build 22 in all 21 runs (17 suites, chushogi 4, daishogi
  4, daidaishogi 3, taikyoku 2). The games differ, as the order does.
- A plain perft prints its root moves in move order, so the check first
  marked daidaishogi as different (186662 in both); `ident.sh` now sorts
  perft lines before it compares them.
- Nodes per second against build 22: standard x0.935 ± 0.018, xiangqi
  x1.102 ± 0.025, grand x1.025 ± 0.014, shogi x0.946 ± 0.032. The trees
  differ now, so this mixes speed with the make-up of the tree; perft
  time, where both walk the same tree, is measured with stage 4.
- Taikyoku loaded alone: build 22 13.6 GB, stage 3 13.9 GB; it fits.
- Queue167 plays build 22 and stage 3 against full FSF (results below).

### Stage 4 (8f9ff3b) and the landing test (016dbc7)

- `relevant_moves` and `relevant_captures` are gone; `fitted_vectors` and
  `fitted_captures` serve the load passes.
- The load logs (every derived value), sorted, equal stage 3 in all 44
  variants.
- Taikyoku loaded alone on the laptop: 7.3 GB and 22.6 s for stage 3,
  5.8 GB and 31.3 s for stage 4. The load fits again for each pass that
  asks, which costs time; macOS reads its peak lower than the server.
- One trie for each piece walks legs that leave the board, which the
  tries of the squares never held, and each such node failed twice (as a
  last leg and as a middle leg). `leg_lands!` now tests the edge and the
  zone once for each node, before either read; `walk_leg!` no longer
  tests them, as the vectors of `lva!` are fitted to their squares.
- Queue168 checks it against stage 3 (the same moves in the same order)
  and times perft against build 22 and stage 3 after queue167.

### Queue167 and queue168 results

Build 22 and stage 3 against full FSF, three interleaved rounds of 200
games in each key variant, no forfeits (Elo over 600 games):

| Variant | build 22 | stage 3 | difference |
| ------- | -------- | ------- | ---------- |
| standard (`nodestime` 14) | +54.3 | +73.0 | +18.7 |
| xiangqi (16) | +8.7 | +16.8 | +8.1 |
| grand (29) | +69.4 | +29.0 | -40.4 |
| shogi (44) | -8.1 | -21.4 | -13.3 |

- Mean -6.7, near ± 14 at 1σ: no gain, not merged.
- Stage 4 with the landing test against stage 3: the same in all 43
  seeded games and all 21 perfts.
- Perft time (the same tree for both builds), stage 4 over build 22:
  standard x1.127 ± 0.216, xiangqi x1.036 ± 0.045, grand x1.064 ± 0.050,
  shogi x1.088 ± 0.047. Over stage 3: x0.972 ± 0.258, x0.991 ± 0.008,
  x1.017 ± 0.015, x0.995 ± 0.009. Bench, over build 22: standard x0.942
  ± 0.016, xiangqi x1.079 ± 0.017, grand x1.040 ± 0.025, shogi x0.940 ±
  0.030.
- So one trie for each piece walks slower than the vector tables of build
  22, and the landing test gives back little. The xiangqi bench gain is
  the make-up of its trees, not speed: perft there is 3.6% slower. The
  tries of the squares (stage 2) were faster on build 22's own trees
  (xiangqi x1.167, grand x1.030) because they hold no leg that leaves the
  board and every condition there is fitted already.
- Taikyoku alone on the server: build 22 13.6 GB, stage 3 13.9 GB, stage
  4 7.4 GB with a load of 76 s.
