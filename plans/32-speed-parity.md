# Speed parity with Fairy-Stockfish

## Status

Opened 2026-10-13 on build 22 (main 78deaff), branch `plan32-speed`.
Closed 2026-10-14 without a merge: no change was faster beyond noise.

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

Stage 0, FSF speed on the server (queue170): ours 0.450M, 0.298M,
0.324M and 0.407M nodes per second against FSF 0.285M, 0.174M, 0.209M
and 0.256M. These ratios are not valid. The server CPU is an AMD EPYC
of family 23, model 1 (Zen 1), and its FSF is a BMI2 build. Zen 1 runs
`pext` and `pdep` in microcode, so FSF's slider attacks are very slow
there. The games do not change, because FSF plays on `nodestime`. The
laptop ratio (0.84 in standard, plan 29) stays the reference until FSF
is built without BMI2.

Stage 0, the `hotpath` profile of build 22 (queue169, server). The
timer cost of a block is not constant: the same `undo_move!` took 70 ns
in one run and 237 ns in the next. Only large blocks are usable. Share
of the bench time:

| Block | standard | xiangqi | grand | shogi |
| ----- | -------- | ------- | ----- | ----- |
| `order::pick`, with `see` | 5.6% | 12.9% | 23.0% | 22.2% |
| `order::see` | 0.3% | 7.6% | 13.8% | 7.0% |
| `state::make_move` | 17.3% | 16.6% | 16.9% | 14.4% |
| `generate_board_moves` | 3.1% | 11.9% | 14.2% | 4.2% |
| `state::is_in_check` | 8.9% | 8.8% | 6.5% | 4.8% |

`see!` makes and undoes each capture of the exchange, with all the
incremental updates, and finds each next attacker with a move walk.
One call costs 2 to 3.4 µs.

Stage 2 and plan 31 (queue171). Perft of the flat trie (f31) equals
build 22 in all 21 suites. Speed against build 22:

| | standard | xiangqi | grand | shogi |
| - | -------- | ------- | ----- | ----- |
| f31 perft time | x1.075 | x1.070 | x1.064 | x1.089 |
| f31 bench speed | x0.969 | x1.064 | x1.036 | x0.935 |
| s32 bench speed | x0.981 | x0.998 | x0.991 | x0.984 |

Games against full FSF, three rounds of 200 in each key variant (Elo
for us):

| Build | standard | xiangqi | grand | shogi | mean |
| ----- | -------- | ------- | ----- | ----- | ---- |
| build 22 | 77.7 | 44.8 | 82.6 | 0.0 | 51.3 ± 7.0 |
| f31 | 51.3 | 15.6 | 55.5 | -3.5 | 29.7 ± 7.0 |
| s32 | 47.2 | 34.3 | 77.7 | -0.6 | 39.6 ± 7.0 |

f31 is -21.5 ± 9.9 against build 22 and s32 is -11.6 ± 9.9 (1σ).
Neither merges.

- s32 is not faster, because the selection was a small part of the
  pick. The pick's cost is the exchange simulation inside it.
- f31 searches the same moves at near the same speed, but lost in
  standard, xiangqi and grand alike. The generation order is the
  change. Build 22 makes slides by length first (each length in all
  directions, from `process_multi_leg_range_token`); the depth-first
  trie walk gives each direction to its end first.

The f31 loss did not repeat. Three more rounds of f31 and build 22
(queue175, rounds 4 to 6) gave this:

| Build | standard | xiangqi | grand | shogi | mean |
| ----- | -------- | ------- | ----- | ----- | ---- |
| build 22 | 78.4 | 34.9 | 36.7 | 2.9 | 38.2 |
| f31 | 73.2 | 59.7 | 85.7 | 8.7 | 56.8 |

Over all six rounds, f31 is -1.5 ± 7.0 against build 22. The first
-21.5 was noise. The spread of the rounds is 1.26 times the spread
that the game counts give. Thus a gap under about 15 Elo on the mean
is not visible in three rounds. The links of the generation-order
explanation were each tested before the replication and refuted. The
trees have the same size. Quiet moves with equal scores are not more
frequent. No capture is missed.

Profile with `perf` on the server (symbols from a build with line
tables). The `hotpath` shares above are not correct for small nested
blocks:

- `see!` is about 0.25% of the time, not 7% to 14%. The timer of
  `hotpath` costs more than one exchange step.
- In grand, generation is 27% of the time. Most of it is the leg
  interpreter, which decodes each step of each vector. FSF reads a
  bitboard attack set. This cost comes from the variant-agnostic
  design, so it is not a lead for this plan.
- In shogi, drop generation is about 14% of the time, and about 60% of
  that is charged to the lines of `match_pattern!`.

Two changes to the pattern test, each node-identical (`ident.sh`: 64
same), speed against build 22:

| | standard | xiangqi | grand | shogi |
| - | -------- | ------- | ----- | ----- |
| pc perft time | x0.959 | x1.058 | x1.042 | x0.988 |
| pc bench speed | x0.981 | x1.006 | x0.989 | x1.000 |
| pt perft time | x1.056 | x1.034 | x1.068 | x1.078 |
| pt bench speed | x0.939 | x1.001 | x1.006 | x0.997 |

- pc stores a `PieceSet` as a bitset of `u64` words, not as 1025
  flags. One set is then 136 bytes, not 1 kilobyte.
- pt stops the test at the first allower that fails and at the first
  stopper that matches. The shogi pawn drop `#~?@n+sW*~P` has the
  empty target as its only allower, so an occupied square skips the
  file walk.

Neither is faster in shogi. Thus the cost under `match_pattern!` is
neither the size of the sets nor the number of units read, and the
`perf` share of the drops does not point to work that can be removed.

The speed scripts were tested for bias (queue178). Build 22 was built
again from the same commit, and the two binaries are identical. The
second binary against the first: perft time x1.033, x0.999, x1.001,
x1.010 and bench x0.995, x0.997, x0.995, x0.999. In the reversed order:
perft time x0.822, x0.984, x1.016, x0.977. The scripts have no bias
outside the standard perft, which is too short to be stable. The slower
perft of pc and pt in xiangqi and grand is a real effect of the code
layout.

Conclusion. Each part of our time is small: no block that a change can
remove is above a few percent of the time. The cost is spread over
the interpreter of rules (legs, patterns, attack lists), which FSF
replaces with code and bitboards written for each variant. A gain of a
few percent in speed is a few Elo, under what the games can detect.
The gap to FSF in speed comes from the design, and plan 32 closes
without a merge.
