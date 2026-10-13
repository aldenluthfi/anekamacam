# Speed parity with Fairy-Stockfish

## Status

Opened 2026-10-13 on build 22 (main 78deaff), branch `plan32-speed`.

## Context

The user wants our search speed to match full Fairy-Stockfish (FSF).
Plan 29 measured build 14 at 84% of FSF in standard (1.32M against 1.57M
nodes per second), 80% in xiangqi, 64% in grand and 84% in shogi; later
builds gained some (CX, AF, LG, RL, the board and map line of build 22).
Plan 31 showed that move generation is near 16% of our time, so a faster
generation alone cannot close the gap.

A sampled profile of build 22 (standard bench, laptop, macOS `sample`):

| Cost | Share | FSF's way |
| ---- | ----- | --------- |
| `alpha_beta`, with make, undo, eval and more inlined | 48% | |
| check detection (`is_in_check!` over `relevant_attacks`) | near 22% | attackers of a square from bitboards, pins once per node |
| move generation | near 16% | bitboard attack sets |
| make, undo, piece lists | near 13.5% | |
| move picking (`pick_by_score!` 9.3%, `score_move!` 2.1%) | near 11% | score once, partial insertion sort |
| table probe and store | near 6% | |

The shares overlap: code inlined under `hotpath::measure_block!` is
charged to one macro line. Stage 0 splits it with the `hotpath` build.

Goal: our nodes per second at least FSF's (classical eval, one thread) in
standard, xiangqi, grand and shogi, with every rule still derived and
every variant still playable. Earlier tries are read first: plan 29 (LG,
RL, LS, CX, AF), plan 17 (the staged picker), plan 13 (the pick cost).

## Stages

0. Baseline and profile, no engine change.
   - Build 22 and FSF (`Use NNUE=false`) from the start position, `go
     infinite` for 10 s and `stop`, five runs each, server idle, in each
     key variant: nodes per second and the ratio.
   - The `hotpath` build of build 22 (queue169 of plan 31) on the seeded
     bench and the perft suites: time and calls of each measured block,
     to order stages 1 to 3 by cost.
1. Attackers of a square from the attack maps. `is_square_attacked!`
   walks each `relevant_attacks` entry of the square with
   `validate_attack_vector!`. For each enemy piece type on the board,
   cast the attack map of its colour mirror (`piece_swap_map`) from the
   target square with `piece_attacks!`, and test whether a piece of that
   type stands on it. That is exact when the attack reads the same both
   ways: leaps, lines and hops, but not gates (the xiangqi horse blocks
   next to itself), not a piece whose map misses captures, not a leg with
   a target rule (`k`, `g`, `v`, first move) and not a condition. A flag
   per piece, set at load, picks the path; the other pieces keep the list
   walk over a list filtered to them. The answer is the same.
2. Pick by one sort. `pick_by_score!` scores lazily and selects once for
   each pick, so its cost grows with the square of the list. After the
   first pass has scored every move, sort once by score (insertion sort,
   as FSF) and keep the exchange rule. Ties can move, so the trees change.
3. Next by the profile: make and undo, the table, the evaluation.

The flat move trie of plan 31 changes the move order, so it merges only
on games; it is played with stage 2's games if its speed holds.

## Rules

- A stage that keeps the search the same merges on server speed: perft
  and seeded games equal the base in all 44 variants, faster on the mean
  of the four key variants, and slower beyond noise in none.
- A stage that changes the trees merges on games against full FSF,
  interleaved with the base, three rounds of 200 in each key variant, at
  +2σ on the mean.
- After each merge, the FSF ratio of stage 0 is measured again.

## Verification

All on the server, each run time-bounded: `~/p27b/ident.sh` (perft
sorted, seeded games, each run in its own folder), `~/p27b/perftspeed.sh`
(perft time, the same tree), `~/p27b/nps.sh` (bench, 16 seeds), the
`hotpath` build, and `~/p27b/fsfspeed.sh` for stage 0. Stage 1 also runs
perft at depth 4 in the xiangqi, janggi and shogi suites (hoppers, lame
leapers, drops) and taikyokushogi perft 2.

## Results
