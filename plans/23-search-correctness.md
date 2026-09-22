# Search correctness

## Authority and scope

S1 was authorized on 2026-09-07, measured and reverted as recorded below.
On 2026-09-08 the user authorized items 0 and 1 of the value-gate plan:
E bookkeeping and A baseline LMR/LMP measurements. This document governs
that execution. No schedule implementation, parameter changes, search
retuning, extensions, parser work, debloat implementation or commits.
B and C, clone profiling and D15 status changes remain outside this scope.

Temporary observational counters are authorized only to measure baseline
frequency and existing search work. Search decisions must remain unchanged;
remove counters immediately after reading their output. No new harness.
A measured frequency alone does not authorize a candidate implementation.

Baseline: `f7e2eaa`; saved executable `bin/base-f7e2eaa`. Source and release
binary were unchanged before bookkeeping; saved/release binaries were
byte-identical at 5,928,000 bytes.

## S1: detect no-move leaves in quiescence

Status: implemented, verified correct, measured negative, reverted. Not
committed. The underlying defect is still present.

### Finding and evidence

At baseline, `src/game/position/search.rs:776-891` returns stand pat in
non-check positions without proving that any legal move exists. Captures
alone cannot distinguish a quiet position from a no-move terminal. Checked
nodes already generate full evasions, but their ply-cap return also precedes
that generation. Use the configured verdict, including drop inversion, rather
than inventing a separate terminal score.

The recorded xiangqi line in `tools/endgame_fixtures.txt:67` ends in a
non-check no-move loss. Plan 21:592-616 records the horizon defect; its claim
that quiescence never generates legal moves is too broad. The actual gap is
the non-check case, not ordinary checked-node evasion generation.

### Implementation boundary

- Add a concrete early-exit legality probe beside existing move generation.
  Generate one occupied source at a time, not all sources of a piece type.
  Probe with existing make/undo; stop on the first legal move.
- Scan stable square indices, not a piece-list iterator across make/undo.
  Make/undo can reorder even quiet movers. Preserve active piece-list and
  royal-list order using the current ply's reusable score buffer; do not
  allocate another persistent state field or copy unused row capacity.
- Exhaust remaining sources, applicable drops/setup placements, and castling
  only when no witness exists. The existing drop generator accepts a target
  range, allowing one target at a time rather than a whole piece's drops.
  Generated passes count; synthetic null moves do not. Retain acceptance-pass
  priority and configured legality.
- Only probe when existence can change the answer. At a stand-pat cutoff,
  if the configured no-move score also reaches beta, both possible cases
  justify the returned beta bound. Otherwise witness a move before cutting.
  At a non-cutting ply cap, equal terminal/static scores need no probe.
- A searched legal capture or a valid move-bearing QT entry already proves
  existence. After capture/pruning exhaustion without a legal witness,
  compare the no-move score clamped to the original alpha with the current
  alpha: equal bounds need no probe; different answers require one. Checked
  nodes retain their full evasion search and no-move verdict.
- Score exhaustion through `no_move_verdict!` and `outcome_score!`, preserving
  the inversion. Restore scratch buffers and position on all return paths.
- This does not solve all forced-quiet horizons or make stand pat a legal
  pass. LMR and main-search pruning stay unchanged.

### Verification and acceptance

- Direct depth-1 predecessor searches must expose the no-move horizon fix.
  Check both configured loss and draw outcomes; also ordinary legal quiets,
  checked evasions, drops/setup, and real passes. Run debug builds so existing
  state verification covers successful and rejected make/undo probes.
- Run existing endgame fixtures and bounded affected perft suites. Reserve
  the full 6,752-position standard suite for the end of the larger campaign.
- Compare fresh seed-42, one-thread benches against `bin/base-f7e2eaa`:
  standard depth 11 / 16 positions; shogi 9/8; xiangqi 10/8; crazyhouse,
  grand, sittuyin, janggi each 9/8. Both binaries use the same embedded
  parameters and bench Hash budgets. Repeat timings, alternate binary order.
- Separate terminal correctness, node-count changes, and elapsed-time costs.
  A make/undo witness can change piece-list order; node changes alone prove
  neither extra terminal detections nor strength. NPS changes also include
  changed node mix, not just probe overhead.
- Use existing same-board EBF controls with matched Hash where measured.
  No Elo claim without candidate-as-A SPRT. This stage claims correctness,
  not strength. Never run derive during verification.
- No numeric slowdown budget was agreed. Do not invent one or call a slow
  implementation accepted merely because fixtures pass. Investigate large
  regressions; record rejected approaches and revert unsuccessful code.

### Earlier rejected attempt

The first implementation generated all sources of one piece type before
probing. Reverted before this authorization. One unpaired timing pass showed
8-40% lower NPS and changed node counts; those timings did not isolate the
cause or prove an acceptable cost. It also left checked cap handling outside
the probe. Its 38/38 fixture result did not establish complete S1 correctness.

### Results

S1 was implemented, verified correct, measured, and then reverted on
2026-09-07. The tree is back at `f7e2eaa` and nothing was committed. The
reverted code was `has_legal_move` and its two private helpers in
`src/game/moves/move_list.rs`, exported from `src/prelude.rs`; a target-range
arm on `generate_drop_list!` in `src/game/moves/drop_list.rs`; the ply-cap,
stand-pat, and exhaustion paths of `quiescence_search` in
`src/game/position/search.rs`; two fixtures in `tools/endgame_fixtures.txt`.
The bug itself is real and remains unfixed; the sections below record what it
is, what a fix costs, and why this one was not worth keeping.

Terminal correctness, release build, `ANEKAMACAM_SEED=42`:

    debug-headless search xiangqi 1 1 --fen '5R3/9/4k4/9/9/9/9/9/9/3K5 w'

reports `mate 1` with pv `f10f9`; `bin/base-f7e2eaa` reports `cp 915` on the
same position, and `perft xiangqi 1` on the successor returns zero nodes. The
standard case `7k/5K1q/5NB1/8/8/8/8/8 w - - 0 1` returns `cp -34` at depths 1
through 6 against a baseline `cp 716`; `-34` is the configured draw contempt,
and the successor `7k/5K1N/6B1/8/8/8/8/8 b` scores `cp 34` with no best move
and zero perft nodes. Drops witness mobility: shogi `9/9/9/9/9/6b2/9/6k2/8K b`
is `mate 1` with an empty hand and `cp 727` with a pawn in hand.

`tools/run_endgame_fixtures.sh` passes 40 of 40 at `GO_DEPTH=1` and again at
`GO_DEPTH=6`. Bounded `perft <variant> 3 --suite` passes standard 48/48
(`--limit 16`), shogi 12/12, xiangqi 33/33, crazyhouse 12/12, grand 3/3,
sittuyin 12/12, janggi 21/21. A debug build repeats the terminal cases plus a
setup-phase sittuyin search, a janggi stand-off pass, and depth-2 perft suites
for six variants without tripping state verification, so both accepted and
rejected probes restore the position.

Cost, `tools/speed-suite.sh` with interleaved A/B passes, one thread, seed 42:

    variant     nodes       time      nps
    standard    unchanged   +11.37%   -10.21%
    shogi       unchanged    +5.09%    -4.84%
    xiangqi     unchanged   +10.49%    -9.49%
    crazyhouse  unchanged    +2.86%    -2.78%
    grand       unchanged    +6.00%    -5.66%
    sittuyin    unchanged   +19.27%   -16.16%
    janggi      unchanged   +10.61%    -9.59%

Node counts are identical to baseline on all seven, so the entire cost is probe
time rather than a changed node mix. That equality is also what an earlier
source-square scan lacked: without restoring piece-list order it moved five
variants' node counts and cost xiangqi +191.56% time (304943 to 635301 nodes)
and sittuyin +56.12%. Restoring active list order through the ply's score
buffer, and probing only when the no-move score could change the returned
bound, removed both effects.

Strength, authorized 2026-09-07 after the cost was reported. Four matches run
with the built-in runner, candidate as engine A so the reported sign is the
candidate's, `TMPDIR` on disk, and `ANEKAMACAM_SEED` deliberately unset so
pairs draw varied openings:

    debug-headless sprt <variant> bin/cand-s1 bin/base-f7e2eaa 200 4000 -5 0

on standard, xiangqi, sittuyin, and crazyhouse. Fixed movetime is the control
this change wants: S1 trades nodes per second for leaf correctness and touches
no time management, and equal per-move budgets keep CPU contention from
deciding which side flags. Bounds [-5, 0] ask whether the candidate is a
regression, which is the open decision; they cannot confirm a gain.

Stopped by the user before any bound was reached, so no verdict file was
written. Last logged state, candidate as A:

    variant       W    L    D   games   A elo    LLR
    standard     65   84   61     210   -31.5   -0.39
    xiangqi      44   65   31     140   -52.5   -0.60
    sittuyin     18   17  155     190    +1.8   +0.18
    crazyhouse  165  167    8     340    -2.0   +0.01

Formally inconclusive: no LLR approached the +-2.94 boundary, and 140-340
games cannot separate a few Elo. The two variants that search deepest per move
carry negative point estimates; the other two are flat. No sample points at a
gain, which is what a pure loss of nodes per second with no offsetting benefit
looks like.

### Why this was reverted

The cost falls on a fraction of every quiescence node. The benefit collects
only on leaves that have no legal move and are not in check. Those are rare in
every configuration tested, so the trade was structurally unfavourable before
any code existed, and the frequency was estimable in minutes with a counter.
That estimate was never made; the stage was chosen because the bug is real,
which is a different claim from the bug being worth fixing at this price.

Identical bench node counts across all seven variants were reported as evidence
of undamaged move ordering. They are also evidence that not one benched node
reached the new path, which answers the strength question directly and was
available before the matches were run.

Order of work was also wrong: three implementations were built and the last two
were optimisations for cost, and only then was the sign measured. The cheapest
correct version should have been measured first, and optimised only on a
positive sign.

What survives: the defect is documented and reproducible. Baseline scores the
xiangqi position `5R3/9/4k4/9/9/9/9/9/9/3K5 w` at `cp 915` when the side to
move has no legal reply, and the standard position `7k/5K1q/5NB1/8/8/8/8/8 w`
at `cp 716` when the forcing capture only draws. Any future fix must beat the
frequency argument above before it is built, not after. A per-million-nodes
count of no-move non-check quiescence leaves is the gate.
