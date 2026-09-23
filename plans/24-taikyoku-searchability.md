# Taikyoku shogi searchability

## Status

Opened 2026-09-22. Stages 0 to 7 implemented and measured; the acceptance
case passes. Nothing committed.

Baseline binary: `bin/base-stage0`, built from the working tree as it stood
before this document's first edit, 9,197,456 bytes. The depth-6 signature
table in `plans/18-frozen-baseline.md` is **stale for this tree** — the
restore ladder moved it, standard now reads `b1:c3 / 22 / 3026` at depth 6
against that table's `d2:d4 / 0 / 17255` — so every comparison below is
against `bin/base-stage0` rather than against plan 18.

## The defect

The engine priced and classified a move by **what it removes**, never by
**whose piece it removes**.

Taikyoku's six flying generals carry `<mcd!g[...]K-*>`, where `d` lets a leg
take a friendly piece. `process_multi_leg_vector!`
(`src/game/moves/move_list.rs:1076-1112`) writes a friendly victim through
exactly the same `enc_multi_move_captured_*!` path as the enemy branch at
`:1113-1149`, with nothing in the record to tell them apart. Downstream:

- `m_capture!` returned true for a move whose every victim was its own, so
  the capture-list retain kept it and quiescence was asked to resolve it.
- `victim_value!` summed every non-unload victim as positive value, so a
  general immolating eighteen of its own pieces scored as an eighteen-piece
  win, landed in the `WINNING_CAPTURE_SCORE` band, survived quiescence delta
  pruning, and was searched first.

Quiescence therefore had no horizon: the best-looking move at every node was
a self-massacre, and its child offered another one.

The capability mask was a consequence of the same defect rather than an
independent one. `derive_search_capabilities` set `multi_capture` from
`destroyed > 1`, and `destroyed` counts only legs carrying `d` and not `u`,
so the flag meant "some vector removes two or more of the mover's own
pieces". The two bits it cleared, `see_valid` and `recapture_order`, are
exactly the two that a swing counting friendly losses as gains would lie to.

## Stage 0. Confirm the mechanism

Cost nothing: `logs/2026-09-22_17-57-48.log` already held the full 842-move
root list with every victim square, and the start position is
`res/config/taikyokushogi.conf:32`. Classifying each victim against that FEN:

    842 root moves = 55 quiet + 787 captures
      693 of 787  (88%)  remove ONLY the mover's own pieces
       94 of 787         take an enemy, every one of them mixed own+enemy
        0                take an enemy without burning own pieces
    largest "capture": (6,3)->(36,3), 30 own destroyed, 0 enemy

The 55/787 split and the 64/723 single/multi split reproduce the figures in
the task statement exactly, which is what says the parse was right.

## Stage 1. A capture is a move that takes the opponent's piece

### What was built

The first implementation gave `m_capture!` a `$state` parameter and read the
victim's colour out of `state.statics.pieces`. That was rejected on review:
the question is a property of the capture event, and the one place that
already knows the answer is the generator branch that wrote the record.

The kept implementation records it instead. The capture record grows from 34
bits to 35, bit 34 being `o`, "the piece taken was the mover's own":

- `src/game/representations/moves.rs` — `o` documented in the record
  diagram, the primary-word diagram (bit 114), and the `MoveSignature`
  diagram, whose real-capture discriminator moves from bit 34 to bit 35 so it
  no longer collides with the fold. New `captured_own!`,
  `multi_move_captured_own!`, `enc_multi_move_captured_own!`.
  `enc_capture_part!` widens its mask from `0x3_FFFF_FFFF` to
  `0x7_FFFF_FFFF`. `m_capture!` keeps its single parameter and now requires a
  victim that is not an unload and not the mover's own.
- `src/game/moves/move_list.rs` — the flag is written in the friendly branch
  (always own) and in the en-passant branch (own when the victim's colour
  matches the mover's). The enemy branch leaves it zero.
- `src/game/search/move_ordering.rs` — `victim_value!` signs each term by
  that flag, so it returns enemy material taken less own material destroyed.

`see!` needed no change: it already seeds `gain[0]` from `victim_value!` and
then plays ordinary single recaptures on `end!(mv)`, so a signed sum makes
the first ply of the swap list the whole move's net removal.

Bit budget: the record had bits 34..63 free and the primary word bits
114..127, so nothing was displaced. The transposition tables store the
signature at full 64-bit width (main table bits 41-104, quiescence table bits
32-95) and the move word unmasked, so moving the discriminator to bit 35 and
writing primary bit 114 are both invisible to storage.

### Why it is inert elsewhere

A friendly victim record can only be produced by a leg carrying `d`. A grep
over `res/config/*.conf` for `<[mcdukvgtipr!]*d[mcdukvgtipr!]*[\[(]` matches
`taikyokushogi.conf` alone, eleven legs; no config has a `u` leg at all. So
in the other 41 variants the `o` flag is always zero and both macros are
bit-identical to what they replaced.

### Measured

- 41 configs, depth 6, seed 42, one thread: best move, score and node count
  **identical** to `bin/base-stage0` on every row.
- Deeper spot check at depth 9 on the variants that carry a `Move.1` payload
  or a hand: standard 43583, crazyhouse 45398, shogi 81343, chushogi 191704,
  grand 346045, sittuyin 32495637, xiangqi 142456 nodes — identical, same
  move, same score.
- All 17 `res/perft` suites at depth 3: berolina 3/3, capablanca 3/3,
  crazyhouse 12/12, euroshogi 12/12, grand 3/3, janggi 21/21, judkins 9/9,
  los-alamos 3/3, makruk 3/3, minishogi 9/9, minixiangqi 3/3, pocketknight
  9/9, shatranj 3/3, shogi 12/12, sittuyin 12/12, standard 20256/20256,
  xiangqi 33/33.
- chushogi 4 = 1774907, daishogi 4 = 25360784, daidaishogi 3 = 186662.
- taikyokushogi perft 1 = 842, perft 2 = 708347.

### Not sufficient on its own

`debug-headless search taikyokushogi 1 1` still did not finish depth 1 in ten
minutes. Expected: the capture set at a quiescence node falls from 787 to
about 94, but with `recapture_order` still withheld, quiescence iterates all
of them and recurses into every one that prices positive. Stage 2 is what
stops that.

## Stage 2. Re-derive the capability veto

`multi_capture` is renamed `multi_destroy` and removed from the
`recapture_order` condition (`src/game/search/parameters.rs`). It stays on
`see_valid`. The old name was a misnomer twice over: `destroyed` counts only
legs carrying `d`, which in this engine's vocabulary is taking the mover's
own piece rather than capturing, and the test is `> 1` rather than exactly
two. `multi_capture` is freed for the flag that earns it in stage 3.

The reasoning, recorded on the function: the quiescence break that
`recapture_order` gates reads a **score band**, not an exchange, and stage 1
makes the band truthful — a sweep that burns its own army now sorts below
every quiet move. The exchange simulation keeps the veto because what it
models past the first ply is one recapture on one square, which a sweep is
not; whether that still costs more than the plain swing on a 1296-square
board is a measurement, not an argument, and it has not been run yet.

Which other variants move: **none**. Capability masks dumped for all 41
non-taikyoku configs before and after are identical, which is the proof that
`multi_destroy` is false everywhere but taikyoku.

### Measured

Taikyoku's mask moved `1100110` to `1110110`: bit 4 and nothing else. Every
other config's mask is byte-identical.

Not sufficient either. `search taikyokushogi 1 1` still did not finish depth
1 in thirteen minutes.

## What was actually wrong, measured

Temporary counters in quiescence, removed after reading, over a 60-second
depth-1 search:

    nodes 1396736 | qnodes 1396734 | qgen 14219737 | qsearched 1396733
    qbreak 209886 | qstand 926460 | maxply 86
    recap 203805 | other 1192928 | ply histogram [0,1,1,1,1,1,1,1,1,7,...]

Read in order:

- **22.3k nodes per second**, not the 400 the task statement quotes. That
  figure is movegen in a perft context; search is fifty times faster.
- Every node but two is a quiescence node, and the ply histogram is one node
  at each of plies 1 to 14 with 1.4 million below that. The search was still
  inside the **first root move's** quiescence subtree after a minute.
- **Max quiescence ply 86.** The leaves were not resolving an exchange, they
  were playing the game out greedily.
- **85% of the captures they searched answered nothing that had just
  happened** — 1192928 against 203805 that took a piece on the square the
  previous move landed on.

That is the disease. Classical quiescence searches every capture and
terminates only because, in a normal variant, taking a piece costs a piece
and the winning captures run out. A taikyoku general sweeps a whole file, one
is nearly always worth playing somewhere on a 36x36 board, and the set never
runs out.

## Stage 3. Quiescence follows the exchange it was called to settle

An exchange is a sequence of captures contesting **one square**. The first
quiescence node is the horizon, so every capture is open to it, and whichever
it plays names the contested square. Every node below answers on that square
or not at all: a capture elsewhere is not a reply, it is a new plan, and a
new plan belongs to the tree rather than the horizon.

`quiescence_search` takes a `contested: Option<Square>`, `None` at the three
entry points and `contested.or(Some(end!(mv)))` on the recursive call. The
test reuses `m_takes_square!`, a new macro in
`src/game/representations/moves.rs` holding the predicate `lva!` already
spelled out inline; `lva!`'s retain now calls it too, so the exchange
simulation and quiescence ask the same question in the same words.

The bound this gives is not a depth limit. It is how many pieces can reach
one square, which is the bound the exchange simulation already assumes.

### Gated, because unconditionally it moved 17 variants

Applied to everything, the depth-6 signature moved on 17 of 41 configs —
`hoppelpoppel` by +61% nodes and 23 points of score, `horde` by -9 points.
That is not "strength intact", so it is gated on a new capability.

`wide_quiescence`, bit 7: *a leaf may answer any capture at all*. Granted
unless some vector can take more than one piece, since that is exactly the
claim the all-captures form rests on. Counting it needs two things movement
generation also does: a final leg that cannot move is a leg that takes, so a
plain slider is not mistaken for a sweep; and a leg carrying the unload flag
hands its victim back, so it cancels one rather than adding one. Without the
second, the xiangqi and janggi cannons and the sittuyin pawn read as
multi-victim and four more variants lost the bit for no reason.

Variants without the bit: **chushogi, daishogi, daidaishogi** (lions take two
at once) and **taikyokushogi**. Everything else keeps it and is untouched.

### Measured

Depth-6 signature against `bin/base-stage0`, 41 configs: **one row moves**,
chushogi `f4:f5 / 51 / 24300` to `f3:g5 / 69 / 27191`. The other 40 are
byte-identical.

The three lion variants at depth 9:

    variant      base                        candidate
    chushogi     f3:g5   95    191704        f3:g5   107   101694   -47%
    daishogi     b1:c3  -19   1028375        b1:c3   -37  1949943   +90%
    daidaishogi  c2:e3  257    134959        c2:e3   257   134957     0%

daishogi across depths 7/8/9/10: nodes +11%, +105%, +90%, +5%, best move
differing at 7 and 8, agreeing at 9 and 10. Mixed and noisy rather than a
clean regression, but it does move; a single startpos swings 30-50% by
itself and daishogi has no perft suite to bench a multi-position control
against. **An SPRT is owed before this is called free.**

Taikyoku, depth 1, one thread: **1788 nodes in 74.9 ms**, against a subtree
that had not finished after 60 seconds. Max quiescence ply 86 to 4.

## Stage 6. Load time

`derive_piece_reach` summed the legs of every vector afresh at each square a
fill arrived at, so a board of 1296 squares did that work 1296 times over per
origin — 1296 x 1296 x vectors per piece. The one-step landings of every
square are now gathered once per piece before any fill runs, and each fill
keeps its visited set in a flat `vec![false; board_size]` rather than a
`HashSet`. The per-origin fills are untouched: a step is not always answered
by a step back, so the component a square belongs to is not the ground it can
reach, and the rejected union-find shortcut stays rejected.

### Measured

`derive_advantage_parameters` on taikyoku: **54.2 s to 3.6 s**, 15x. Whole
taikyoku load 106 s to 57.5 s wall, and 519 s to 56 s of CPU — the rest of
the wall-clock gain was hidden by the fills being parallel. The derived
pair-piece list is character-identical on all 42 configs, taikyoku's included.

What is left of the load is 25.7 s of attack-mask precompute between the last
move parse and the first parameter line, which this stage did not touch.

## Stage 7. The rest of the load

Added 2026-09-23 on request, profiled with the project's `hotpath` feature
(`cargo build --release --features hotpath`, report widened with
`#[hotpath::main(functions_limit = 0)]`). The load path now carries
`#[hotpath::measure]` on `load_variant`, `parse_config_file`, `precompute`,
`generate_piece_moves`, `generate_move_vectors`, `populate_relevant`,
`generate_relevant_moves`, `generate_relevant_captures`,
`generate_attack_masks`, `load_fen`, `parse_bit_fen`,
`parse_tuned_parameters`, every `derive_*` step, and the headless dispatch;
they stay, and cost nothing without the feature.

### What the profile said, before

    run_debug_headless          ~48.7 s
      load_variant               39.2 s
        precompute               27.9 s
          generate_attack_masks  15.7 s   1296 calls, one thread
          populate_relevant       6.7 s   1.8 M generator calls, one thread
          generate_piece_moves    5.5 s   694 expressions, one thread
        parse_tuned_parameters   11.2 s
      (after the command)        ~7.2 s   freeing a 9.8 GB State

The last line is not a function. It is `run_debug_headless` less everything
inside it, and it is the drop of the position at the end of the command: the
per-square move tables held their own copy of every vector, millions of small
allocations, and freeing them took seven seconds.

### What changed

All exact by construction: nothing derived changes, only how often it is
built and on how many threads.

- **Attack masks shared, not copied.** `generate_attack_masks` cloned the
  whole vector once per leg that can take, so a 35-leg sweep was stored 35
  times over. `AttackMask` now carries the vector by reference count.
- **Move vectors shared, not copied.** `MoveVector` is now `Arc<[Leg]>`. The
  per-square tables keep, for each square, the options that stay on the board
  from there, which is nearly the whole set; they now point at the parsed
  copy. The same pointer hop as a `Vec`, so readers pay nothing. Four loops
  that iterated a `&Vec` directly now call `.iter()`.
- **The precompute passes run in parallel, in order.** `populate_relevant`
  and `generate_piece_moves` collect by index. `generate_attack_masks` only
  reads now and returns its writes; `precompute` files them in square order
  afterwards, so every square's attacker list is in exactly the order the
  serial pass built.
- **The capability scan runs a piece per thread** and joins its six facts by
  `or`, each fact being whether any vector anywhere has the shape.

### Measured

    wall, plain release, debug-headless state taikyokushogi
                          before stage 7    after
    best of runs               57.5 s        13.5 s
    runs drift upward       —             13.5 / 15.9 / 18.2 / 21.4 s
    peak RSS                   10.3 GB       7.5 GB
    precompute (hotpath)       27.9 s         6.4 s
    drop at exit                7.2 s         0.7 s

The upward drift across back-to-back runs is the machine, not the code: load
average sat at 11 to 14 with nothing else running.

Exactness, final binary against the post-stage-3 records: depth-6 signature
byte-identical on all 41 configs; capability masks identical; derived pair
lists identical, taikyoku's included; all 17 perft suites pass; chushogi 4 =
1774907, daishogi 4 = 25360784, daidaishogi 3 = 186662; taikyokushogi
perft 2 = 708347.

Speed of the search itself, `tools/speed-suite.sh` with `PROCS=4`, A =
`bin/base-stage0`, B = `bin/cand-stage7`, which carries every stage from 1
to 7, 16 bench positions each, seed 42, one thread:

    variant     depth  nodes      time
    standard      11   identical  -5.93%
    shogi          8   identical  -0.06%
    crazyhouse     8   identical  +1.43%
    xiangqi        9   identical  +1.50%
    grand          9   identical  +1.49%
    sittuyin       8   identical  +0.84%
    janggi         8   identical  +0.47%

Node counts are identical on all seven, over sixteen positions each rather
than one start position. Time sits within about 1.5% either way at four
interleaved passes, which is inside the noise a pass count that low gives;
nothing here reads as a regression, and nothing as a gain to claim.

### What is left

`parse_tuned_parameters` is now most of the load, about 10 s, and most of
that is `derive_piece_reach` again (5.5 s) and king danger (2.2 s). The reach
fills could collapse to one per strongly connected component — every square
in a component reaches exactly the same set — which is exact, unlike the
rejected union-find, because it follows the edges in their own direction.
Not done yet; it is the function with a failed attempt already on record,
and it wants its own exactness check on the raw reach values rather than
only the pair list they feed.

## Stage 4. Answering within the clock

Built in two halves; one was kept.

**Kept: an interrupted first iteration keeps its proven root move.**
`alpha_beta` records, at ply 0, each root move whose subtree finished and
beat the best so far, in `SearchInfo::candidate_move`. When the clock cuts
the first iteration, `iterative_deepening` returns that move instead of
`null_move()`. Later iterations are still thrown away whole, the previous
depth's move being the right answer there. The score is not kept, being the
best of a partial list rather than of the root.

It cannot be triggered by anything the engine plays today, and that is
recorded rather than hidden. The clock is first read at node 2048; depth 1
costs 1788 nodes on taikyoku's start position and 1543 in the middle game,
and tens to hundreds on every other variant, so depth 1 always finishes
before a deadline can be seen. The hole was real only while taikyoku's
quiescence never ended, which stage 3 removed. The fallback stays as ten
lines of insurance for a variant whose depth 1 passes 2048 nodes; whether
to keep it is still open.

**Reverted: adaptive interrupt polling.** Built to shorten the poll stride
when nodes are slow, on the task statement's figure of 400 nodes a second,
where 2048 nodes is five seconds. Measured search speed is 22,000 nodes a
second, so 2048 nodes is 93 ms and the worst overshoot a tenth of a second.
Three constants and two fields for that was not worth it; removed.

Fixed-depth behaviour is untouched: 41-config depth-6 signature identical.

## Stage 5. Continuation history sized to fit

`clear_search` allocated `2 x (pieces x squares)^2` cells per worker every
search: 2.4 MB on chess, 6.1 GiB on daidaishogi, 2.94 TiB on taikyoku.

Dense sizes, from the configs:

    taikyokushogi  694 pieces 36x36   3,085,951 MiB
    daidaishogi    140 pieces 17x17       6,245 MiB
    daishogi        90 pieces 15x15       1,564 MiB
    tjatoer         52 pieces 16x16         676 MiB   largest of the rest
    chushogi        74 pieces 12x12         433 MiB
    shogi           28 pieces  9x9           20 MiB

The table is now `min(dense, 2^29)` cells, one GiB of `i16` at most, and every
variant finds its cell as `(base + key) % len` through `cont_cell!`. Where the
dense table fits that is the index itself, so the three giants are the only
variants whose indexing changes. There is no flag and no second code path.

### How the fold was chosen

Four ways of fitting the giants were built and measured. Summed over ten
self-play positions per variant at depth 8, against the dense table:

    method                daishogi            daidaishogi
                          nodes    time       nodes    time
    hash whole index      -0.3%    +5.4%      +0.6%    +3.6%
    modulo (kept)         -1.7%    -0.9%      -4.9%    -4.4%
    mask the move keys    +0.0%    +0.1%     +58.2%   +41.3%
    hash only past cap    (single position: +58% nodes, move changed)

Four taikyoku positions at depth 3 read identical for all four. The small
variants were checked with `tools/speed-suite.sh` over seven variants, modulo
against the masked keys: identical nodes, time within 0.7% either way, so the
division modulo costs on every access does not show.

- Hashing the whole index scatters a row's cells across a gibibyte and loses
  the locality the dense layout had.
- Folding only what spills past the cap lands the rows that follow this
  side's own previous move on top of the busiest rows, those answering the
  opponent's.
- Masking the move keys aliases keys 16384 apart, and with keys laid out as
  `piece * squares + square` the same few move pairs collide over and over.
- Wrapping keeps rows contiguous and spreads the overflow evenly.

A single start position at fixed depth was useless for this: node counts
swung from -63% to +61% between methods with the best move changing, because
a different history table changes reductions and reshapes the tree. Only the
multi-position sums are quoted.

An all-variants version of the hash, forcing every variant through it, was
also measured, to price making the scheme universal the other way: identical
nodes, but +12% to +429% time, standard worst, from allocating and scattering
over a gibibyte where 2.4 MB had fit in cache.

### Measured

- 41-config depth-6 signature identical to before the stage.
- Clean build equals the measurement probe node for node on the positions
  where modulo differs from dense.
- `play taikyokushogi 2 5 4 10`: ten plies, a legal move each. Peak virtual
  size **12.2 TiB dense, 421 GiB wrapped**, most of that 421 GiB being the
  reservation macOS gives any process on this machine. Peak resident size is
  not improved, 7.1 GB against 7.9 GB, run-to-run noise over the seven
  gigabytes the load tables hold, since resident size only ever counted the
  pages touched.

## Acceptance

    debug-headless play taikyokushogi 2 5 4 10

plays ten plies, a legal move each, in 62 s wall of which 57 s is the load.
**Depth 2 completes in 299 ms**, depth 1 in 102 ms, and the principal
variation carries a real reply:

    3103:3136*3104*...*3136=Έ  3235*3136=ό

Perft, final binary: all 17 `res/perft` suites at depth 3 pass, standard
20256/20256; chushogi 4 = 1774907, daidaishogi 3 = 186662; taikyokushogi
perft 1 = 842 and perft 2 = 708347.

## Still owed

- daishogi SPRT, and a strength read on chushogi.
- daishogi 4 = 25360784 re-run on the final binary.
- Endgame fixtures, FEN round trip, and `tools/speed-suite.sh` A/B.
- Whether to keep `candidate_move` (stage 4). Stage 4's second half may no longer be needed:
  taikyoku now finishes depth 2 in a fraction of the budget, so the
  null-best-move hole is no longer reachable there, though the 2048-node
  interrupt poll and the 2.94 TiB continuation table both still stand.

## Observation, not acted on

`destroyed` in `derive_search_capabilities` counts the same screen legs that
misled the victim count, so xiangqi, janggi and sittuyin read as destroying
two of their own. It changes nothing today — each of them already loses
`see_valid` to a royal capture or to promotion into what was taken — but the
figure is wrong and would mislead the next reader of that function.
