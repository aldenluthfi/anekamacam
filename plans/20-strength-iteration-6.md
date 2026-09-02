# Strength iteration 6

## Status

Roadmap drafted on 2026-09-01. On 2026-09-02, at explicit user direction
("minimum viable diff, minimal checks"), all eleven production phases A-6
through K-6 were implemented in one batch and committed together. Tier 0,
Tier 1, the falsification ladder, and every SPRT campaign in this document
were **skipped**. Nothing below is promoted: none of the eleven has any
Elo evidence, any signature diff, any node measurement, or any dead-arm
verdict behind it. The code is present and builds; its strength effect,
including its sign, is unknown. Anyone resuming this iteration should read
every "implemented, unvalidated" phase status as an open falsification
debt, not as a finished stage.

Verification actually run: one release build with no warnings, a scholar's
mate fixture found at depth 6, and depth-8 searches on standard, shogi,
crazyhouse, and threecheck that return without crashing.

Depth-8 node counts against `b971865`: standard 178406 to 17042, shogi
300726 to 20906, crazyhouse 274784 to 14832, xiangqi 1069881 to 16349.

One 40-game standard match at 100 ms against `b971865`, seed 7, run to
check that those reductions are selectivity and not blindness: the batch
scored 33W 4L 3D, LLR 1.28 against bounds [-2.94, 2.94], so the sign is
not in doubt at that time control even though the campaign this document
specifies -- five variants, dead-arm rule, per-phase decision floors --
never ran. Nothing here attributes the result to any individual phase.

Derived search scales measured on standard: mean deployed value 274,
branch scale 79, positional swing 826, board size 64. The late-move count
`3 + depth * depth * branch_scale / board_size` therefore reproduces the
frozen `3 + depth * depth` rule to within one move at every depth on
standard while widening on wider variants.

Source baseline is `main` at `b971865`. Behavioural reference is the 38-config
signature in plan 18. Iterations 4 and 5 remain ablated. Their deterministic
support measurements may define new falsifiers, but none of their game verdicts
is promotion evidence.

One baseline discrepancy must be named before work starts. The stripped search
still contains flat late-move pruning at
`src/game/position/search.rs:699-709`, using `3 + depth * depth`. Iteration 6
therefore does not treat all late-move pruning as absent. It treats the flat
rule as frozen baseline behaviour, measures it, and replaces it only if a
rule-derived form earns promotion.

## Goal

Rebuild search strength from the frozen baseline. Re-earn useful search
mechanisms from rules, movement geometry, piece values, and current state. Do
not restore old constants, old phase patches, or old verdicts.

Every production phase must be falsifiable before games. Every game campaign
must use an instrument and host that passed Tier 0 for the exact campaign
configuration.

Iteration 6 is search-first because the ablation left a measured search-cost
cliff: the standard 16-position depth-11 bench now uses 75,672,279 nodes,
against about 128,000 before the search ablation. This is a measured worklist,
not evidence that any removed implementation was correct.

## Non-goals

- Do not cherry-pick iteration-4 or iteration-5 search code.
- Do not restore capture history or positive singular extensions.
- Do not retune scalar payloads before the retained feature set is stable.
- Do not add a variant-name or protocol-dialect branch to engine code.
- Do not add a campaign wrapper or a second benchmark harness.
- Do not use fixed Standard-chess pawn, board, royal, or move-count constants.
- Do not use node count, EBF, NPS, or a verdict string alone to promote code.
- Do not add `#[test]`, `#[allow(...)]`, or silenced warnings.
- Do not run `cargo fmt`, including `cargo fmt --check`.
- Do not push. Accepted commits carry no attribution trailer.

## Evidence language

Every prior in this plan uses one label.

- **measured-here**: measured in this repository. Invalid game verdicts may be
  cited only as support measurements, never as strength evidence.
- **upstream-only**: established in another engine or literature, but not
  measured here with a valid instrument.
- **guess**: mechanism is plausible, but no valid local or upstream strength
  measurement supports its size or sign.

Expected-Elo bands remain withdrawn. A decision floor is a governance rule: the
minimum result that pays for retained complexity. It is not a prediction.
Numeric support floors are also governance thresholds, not expected effects.

## Change classes

Every production phase declares one class before implementation.

- **Class S**: search policy and tree are identical. Candidate must reproduce
  every row of the 38-config signature exactly. Only NPS or wall time may move.
- **Class T**: search tree or evaluation may change. Candidate must declare the
  direction expected from its mechanism and the measurements that can refute
  that direction.

A Class S phase that changes one signature row is misclassified. It is abandoned
before games. A later Class T proposal must begin again from the previous
accepted baseline with a new mechanism claim and new gates.

## Laws carried forward

1. Support can veto. Only valid games can promote a Class T phase.
2. A helper differential proves the helper, not the search tree.
3. A fixed-depth node reduction is useful only after correctness and mechanism
   oracles pass.
4. A correctness fix shared by comparison binaries is not strength evidence.
5. All production derivation runs on generated and loaded-parameter paths.
6. Temporary counters and oracle code leave before a promotion binary is built.
7. One accepted phase and its plan update form one commit.
8. Failed candidates remain documented and uncommitted.
9. An infrastructure death makes the campaign void. Partial results are not
   evidence.
10. If a cheap exact predicate cannot protect a shortcut, the shortcut stays
    disabled in that derived rule or state class.
11. Named variants may select test positions. They may not select engine logic.
12. Candidate is always engine A in promotion games. Every recorded number is
    read from engine A's view.

# Tier 0: blocking instrument and host validation

No phase code starts until all Tier-0 checks pass. Tier 0 reruns when any of
these change:

- SPRT code;
- protocol or translator code;
- referee or opening code;
- campaign binary defaults;
- campaign time control;
- campaign host;
- host filesystem used by `TMPDIR`;
- host memory, swap, or concurrency policy.

## T0.1 Current baseline identity

Build and record the current baseline without changing source or resources.

```sh
cargo build --release
cp target/release/anekamacam bin/base-6
cp target/release/anekamacam bin/referee-6
tools/provenance.sh record bin/base-6 b971865
tools/provenance.sh record bin/referee-6 b971865
tools/provenance.sh verify bin/base-6 bin/referee-6
```

Required result:

- release build emits no warnings;
- provenance names commit `b971865`;
- rebuilt `content_md5` matches;
- working tree is clean;
- 38 shipped configurations are listed by UCI;
- config, dictionary, and option hashes are recorded.

Raw MD5 is not a rebuild identity. `content_md5` is.

## T0.2 Frozen behaviour certificate

Run every shipped config with the plan-18 conditions:

```sh
ANEKAMACAM_SEED=42 bin/base-6 \
    debug-headless search <variant> 6 1
```

Conditions are Threads 1, depth 6, 1 MB TT, and 1 MB QT. Best move, score, and
nodes must reproduce all 38 rows in plan 18 exactly. `example.conf` is not a
shipped variant.

Then run the frozen correctness suites:

```sh
BIN=bin/base-6 tools/run_endgame_fixtures.sh
tools/run_fen_roundtrip.sh
tools/drop-integrity.sh bin/base-6 24 200 5 0.03

bin/base-6 debug-headless perft standard 3 --suite
bin/base-6 debug-headless perft crazyhouse 4 --suite
bin/base-6 debug-headless perft shogi 3 --suite
bin/base-6 debug-headless perft xiangqi 3 --suite
bin/base-6 debug-headless perft janggi 3 --suite
bin/base-6 debug-headless perft minishogi 3 --suite
bin/base-6 debug-headless perft judkins 3 --suite
bin/base-6 debug-headless perft euroshogi 3 --suite
bin/base-6 debug-headless perft pocketknight 3 --suite
bin/base-6 debug-headless perft sittuyin 3 --suite
bin/base-6 debug-headless perft minixiangqi 3 --suite
```

Required result matches plan 18:

- end conditions: 38 passed, 0 failed;
- FEN round trips: 44 passed, 0 failed, 0 skipped;
- drop integrity: 24 games, 0 mismatches;
- every listed perft suite passes.

Record baseline support measurements. They are reference data, not promotion
results.

```sh
HASH=64 tools/ebf-suite.sh bin/base-6
PROCS=10 tools/speed-suite.sh bin/base-6
HASH=64 tools/agree-suite.sh bin/base-6
```

If the signature or a frozen suite fails, stop. Repair or re-document the
baseline before iteration 6. Do not reinterpret the mismatch as normal drift.

## T0.3 Null test and host fitness

Run the same binary against itself with seed unset at the exact campaign time
control. Use a disk-backed `TMPDIR`.

```sh
env -u ANEKAMACAM_SEED \
    TMPDIR=/campaign-disk/anekamacam-tmp \
    bin/base-6 debug-headless sprt \
    standard bin/base-6 bin/base-6 5000+50 12000 0 5
```

Required result:

- H0 is accepted, or game budget ends inconclusive;
- H1 is never accepted;
- absolute engine-A Elo is at most 5;
- wins and losses are balanced within sampling noise;
- no response timeout, process exit, OOM, restart, or aborted game occurs;
- result file states both binaries and engine-A perspective correctly;
- no harvested engine log records starvation or protocol failure.

An H1 boundary crossing in an A/A test is a failure, not a surprising result to
explain away. Diagnose instrument or host, then rerun from zero.

## T0.4 Sign test in both argument orders

Use two historical binaries with a valid and large Standard strength ordering:
Stage Q at `2372ed7` is stronger than Stage N at `f20be48` by about 52 Elo in
the valid Standard round robin recorded in plans 03 and 04. Build both from
their own commits and resources in isolated targets. Record and verify each
binary from its own worktree before removing that worktree.

Run both argument orders with seed unset:

```sh
env -u ANEKAMACAM_SEED \
    TMPDIR=/campaign-disk/anekamacam-tmp \
    bin/base-6 debug-headless sprt \
    standard bin/tier0-q bin/tier0-n 5000+50 2000 0 20

env -u ANEKAMACAM_SEED \
    TMPDIR=/campaign-disk/anekamacam-tmp \
    bin/base-6 debug-headless sprt \
    standard bin/tier0-n bin/tier0-q 5000+50 2000 0 20
```

Required result:

- Q as engine A crosses H1 and names Q stronger;
- N as engine A crosses H0 and never names N stronger;
- engine-A Elo is positive in the first order and negative in the second;
- raw W/L reverses with the arguments;
- both result files state that every figure is from engine A's view;
- no arm aborts or scores infrastructure failure as strength evidence.

If either order reports the wrong sign, stop the iteration. No support or game
result from that instrument is admissible.

## T0.5 Host certificate

Built-in SPRT sets Threads to 1 and leaves both AnekaMacam binaries at their
256 MB default Hash. The live table reservation is therefore:

```text
2 engines * 1 thread * 256 MB Hash = 512 MB
4 * 512 MB = 2,048 MB minimum available RAM
```

Host requirements:

- exclusive host: no other campaign, datagen, tuning, or build;
- at least 2,048 MB available RAM before launch;
- with no swap, at least another 2,048 MB remains free after both engines start;
- `TMPDIR` resolves to a disk filesystem, never `tmpfs` or `ramfs`;
- at least 20 GB free on the `TMPDIR` filesystem;
- engine sandbox logs are truncated at 64 MB during the run;
- CPU count, RAM, swap, filesystem, free disk, and hostname are recorded;
- campaign concurrency is one arm per certified host.

Record these checks before the null test:

```sh
hostname
getconf _NPROCESSORS_ONLN
free -h
swapon --show
findmnt -T "$TMPDIR" -o TARGET,FSTYPE,SIZE,AVAIL
df -h "$TMPDIR"
```

Reject a filesystem reported as `tmpfs` or `ramfs`. Configure host log rotation
for:

```text
$TMPDIR/anekamacam-sprt/*/logs/latest.log
```

Use `copytruncate`, a 64 MB size limit, and no retained sandbox copies. Verify
rotation once during the null test. Project source gains no wrapper or watchdog.

The old host `upi@157.10.252.201` has no standing certificate. Reachability is
not fitness.

## T0.6 Dead-arm rule

Any one of these voids the complete phase campaign:

- `aborted` in SPRT output or result file;
- response timeout;
- engine process exit;
- restart after engine loss;
- OOM, full disk, full tmpfs, or I/O failure;
- host sharing or resource policy violation;
- missing or unverified binary provenance;
- log truncation failure that threatens host fitness.

Discard every arm from that phase campaign, including arms that finished.
Repair the host, rerun Tier 0, and restart every phase arm from zero. Do not
merge partial W/L/D, LLR, or result files across the death.

# Tier 1: pre-feature truth oracles

Tier 1 uses throwaway instrumentation in temporary builds. Instrumentation is
removed before any candidate binary is recorded. Tier 1 adds no wrapper.

## V1. Search-window and table truth

Before PVS, test full-window truth against zero-window probes at sampled depths.
Run with TT/QT disabled and enabled. For every sampled position:

1. obtain the full-window score and best move;
2. probe windows below, across, and above that score;
3. classify only fail-low, interior, or fail-high;
4. require the same class, completed-depth score, and legal PV with tables on;
5. repeat on terminal, repetition, mate-band, setup, pass, and hand positions.

Also construct same-board states that differ in search-relevant context:
virgin rights, monotone phase, delivered-check count, counting/counter progress,
repetition path, and prior-pass state. Canonical repetition identity may remain
equal where rules require it, but TT/QT reuse must not change score, best move,
or terminal kind.

Any mismatch is a baseline correctness defect. Fix and re-freeze it before A-6.
If no variant-agnostic state identity can make table-on and table-off agree,
stop iteration 6. PVS, aspiration, LMR, and every pruning result would be
confounded.

## V2. Ordering and existing-shortcut audit

Instrument successful legal ordinal, move class, ordering score, alpha raise,
and beta cutoff on `tools/ebf_positions.txt`. Pseudo-legal loop index is not a
legal ordinal.

Required observations:

- later legal nonforcing moves have lower alpha-raise and cutoff rates;
- TT moves, good captures, killers, and high history moves lead their classes;
- drops, promotions, castling, unloads, and pass do not borrow misleading quiet
  history confidence;
- the frozen flat LMP never skips an immediate terminal, real pass, castling,
  unload-only move, or other nonquiet special move;
- synthetic NMP never substitutes for a legal pass in setup, stand-off,
  double-pass, checks, goal, extinction, counting, or counter contexts.

If the late tail is not colder, B-6 and E-6 are deleted. F-6 is deleted unless
its own gain-bound oracle supplies an ordering-independent proof.

A discovered current LMP or NMP correctness defect becomes an unlettered
prerequisite. It lands separately, passes all frozen suites, creates a new
38-row reference, and is shared by every later comparison binary.

## V3. Currency oracle

This answers both questions left open by plan 19 before any affected pruning can
be promoted.

### Capture-to-hand and promotion-pool currency

Repeat the plan-19 exhaustive exchange oracle, stratified by:

- ordinary board capture;
- capture into a demoted hand type;
- capture that expands `promote_to_captured` options;
- promotion;
- multi-capture;
- unload;
- terminal capture;
- open and closed capture transactions.

Extend the exact exchange state with the real hand or promotion-pool update.
For each capture, compare three declared currencies:

1. board victim value only;
2. board victim plus mapped hand value;
3. board victim plus a declared promotion-option bound.

Use the actual capture mapping from make/undo. Do not use a variant name. For a
closed transaction, exact minimax over the extended state must preserve the
candidate currency's sign. For an open transaction, compare pruning-off child
A with child B, where only the acquired hand or pool resource is removed, at
fixed depths with fresh tables. This differential measures resource value; it
does not fit a production constant.

Hand value becomes eligible only if victim plus mapped hand value passes every
closed sign check and never understates the fixed-depth A/B differential in the
sample. Promotion-pool value becomes eligible only if a rule-derived option
bound passes the same tests. Otherwise G-6, H-6, and I-6 remain disabled for
that transaction class. This is an accepted answer, not a failed oracle.

### Material-orthogonal terminal currency

Build positions one action from declared `checks`, `goal`, and `extinct`
triggers. Pair boards that differ only in delivered-check progress, goal
distance, or extinction threshold. With tables fresh or disabled, compare
pruning off and on at depths 1 through 4.

Any shortcut that hides an immediate rule win, changes winner, changes mate or
terminal kind, or skips the full-search best move fails.

Production protection must be derived from `Termination` and current state:
checks remaining, exact goal reachability, extinction stock, counting/counter
distance, prior pass, and repetition context. For move-level pruning, making the
move and preserving an eager terminal child is the exact fallback. For
node-level pruning, an unavailable cheap proof disables the shortcut in that
state class.

If no cheap rule-derived predicate exists, D-6 through I-6 are deleted for the
affected state class. No blanket named-variant capability mask is allowed.

# Falsification ladder

Every production phase follows these gates in order.

## L0. Declare before code

Record:

- class S or T;
- mechanism claim;
- exact affected rule and move classes;
- expected support direction;
- falsifier;
- decision floor;
- prior label;
- descendants removed by failure.

Missing declaration means no implementation.

## L1. Build, scope, and provenance

- warning-free release build;
- no unused fields or silenced warnings;
- no temporary instrumentation;
- intended source and derived fields only;
- both parameter-load paths exercised;
- `tools/provenance.sh verify` passes for baseline and candidate.

Failure abandons the candidate before measurement.

## L2. Full 38-config signature

Run the plan-18 command for all 38 configs with seed 42, Threads 1, depth 6,
1 MB TT, and 1 MB QT.

For Class S, every best move, score, and node count must be identical. Any
mismatch abandons the phase outright.

For Class T, record every changed row. A config declared unaffected must remain
identical. An unexplained change outside the declared mechanism abandons the
phase outright.

## L3. Multi-position nodes and EBF

```sh
HASH=64 tools/ebf-suite.sh bin/<base> bin/<candidate>
```

Use the phase's declared affected pool and same-board controls. The phase must
meet its stated node, EBF, mate-depth, or agreement direction. One severe
unexplained outlier blocks games even if the geomean passes.

Failure abandons the phase outright. Fewer nodes alone never passes it.

## L4. Multi-position speed

```sh
PROCS=10 tools/speed-suite.sh bin/<base> bin/<candidate>
```

Class S requires at least +2.0% pooled NPS and no affected variant below -2.0%.
Class T must meet its phase-specific node and fixed-depth time floors. An NPS
loss above 3.0% is a veto unless its source was declared before code and the
affected-pool fixed-depth wall time still improves by at least 10%.

Failure abandons the phase outright.

## L5. Correctness vetoes

Run every affected existing suite:

```sh
BIN=bin/<candidate> tools/run_endgame_fixtures.sh
cmp -s target/release/anekamacam bin/<candidate>
tools/run_fen_roundtrip.sh
tools/drop-integrity.sh bin/<candidate> 24 200 5 0.03
```

`run_fen_roundtrip.sh` has no `BIN` override. The `cmp` must pass so it tests
exactly the recorded candidate, not a stale default-target binary.

Run bounded perft only when move generation, make/undo, hash identity, or rule
state changes. Run fixed-depth tactical and terminal fixtures for search-only
changes.

Any new failure abandons the phase outright. Do not spend games to decide a
correctness question.

## L6. Valid games

Only a candidate that passes L0 through L5 may enter the campaign protocol.
Support cannot promote it.

# Ordering by information per machine time

Order is dependency-first, not expected-Elo-first.

1. V1 can invalidate every windowed or table-backed result.
2. V2 can delete both late-move families before implementation.
3. V3 can delete most forward and exchange pruning in unsafe currencies.
4. A-6 is the window primitive needed by the chosen LMR shape.
5. B-6 attacks the largest measured node cliff. Its failure changes the whole
   iteration's premise.
6. C-6 freezes evaluation before static-eval margins are derived.
7. D-6 tests node-level static selectivity. Its residual oracle informs F-6.
8. E-6 tests whether a terminal-safe late tail can be omitted, including the
   frozen flat-LMP replacement.
9. F-6 tests move-level static selectivity with a different proof surface.
10. G-6 tests exchange sign only after both inherited currencies are settled.
11. H-6 and I-6 reuse the exchange and gain-bound proofs in quiescence.
12. J-6 waits until completed-score volatility stops changing.
13. K-6 waits until pruning stabilizes the cost of one extra ply.

A failed early premise deletes more descendants than a late failure. Cheap
oracle work therefore precedes every game campaign.

# Production phase ledger

## A-6. Principal variation search

**Status:** implemented 2026-09-02, unvalidated. Scout window is
`(-alpha - 1, -alpha)` for every move after the first, with a full-window
re-search whenever a scout raises alpha inside the window.

**Class:** T.

**Mechanism claim:** after the first legal move establishes alpha, later legal
moves usually need only a scout window. A scout alpha raise at a wide node is
re-searched at full window. Completed-depth minimax score remains unchanged
while nodes fall.

Use successful legal ordinal, not generated index. Keep NMP zero-window probes
separate. TT bounds retain their frozen rule: stored bounds cut scout nodes,
not wide nodes.

**Falsifier:** any V1 score, bound class, best move, legal PV, terminal kind, or
mate result differs from full-window reference with TT/QT off or on. After that
passes, abandon if EBF-suite total nodes do not fall by at least 10%, or any
case grows above 1.25 times baseline without a named cause.

**Decision floor:** +5 Elo.

**Prior:** upstream-only for PVS as a mechanism; measured-here support only from
iteration 4; valid local strength prior is guess.

**Failure removes:** PVS and the PVS-nested B-6 LMR design. Aspiration remains
possible only if V1 itself passed; PVS performance failure does not delete
aspiration.

**Campaign:** Standard is the gain arm. Shogi and crazyhouse are veto arms.
Xiangqi and grand remain all-config support controls and final arbiters.

## B-6. Rule-derived late-move reductions

**Status:** implemented 2026-09-02, unvalidated, with the shadow corpus
and the V2 ordering oracle both skipped.

The one dimensionless reduction formula this phase is measured against,
recorded here as the phase requires. With `ordinal` the one-based legal
ordinal and `branch_scale` the derived nonforcing fan-out:

```
lateness  = 8 * ordinal / branch_scale
reduction = 1 + floor(log2(depth) * log2(1 + lateness) / 2)
reduction = min(reduction, depth - 2)
```

Eligible moves are quiet, non-pass, non-terminal-risk moves at ordinal
above one and depth above two. Table, capture, promotion, drop, castling,
unload, pass, and in-check moves are never reduced. The `depth - 2` cap
leaves every reduced move at least one ply of child search, and an alpha
raiser is re-searched at full depth before any wide-window re-search.

**Class:** T.

**Mechanism claim:** late legal nonforcing moves have lower cutoff probability.
A reduced scout can reject most of them cheaply. Every reduced alpha raiser is
re-searched at full depth before any wide-window re-search.

Reduction surface is derived from depth, successful legal ordinal, and a
rule-derived nonforcing branch scale. Derive the branch scale through the
existing deterministic setup/rule simulation path. Do not import the old LMR
table or an 8x8 move-count constant. Checking, terminal, TT, promotion, capture,
pass, unload, and unresolved hand/pool moves start unreduced.

Write one dimensionless reduction formula in this document before running the
shadow corpus. Formula may use only depth, legal ordinal, and derived branch
scale. Shadow results accept or reject that formula; they do not select among
surfaces or fit constants. Require zero tactical-fixture misses and zero false
negatives in the fixed threshold-adjacent shadow corpus. History may adjust
reductions only after V2 proves its class keys and confidence signal.

**Falsifier:** V2 tail is not colder; a reduced scout misses a full-depth alpha
raiser in the frozen shadow corpus; full-depth re-search does not recover the
reference score; or EBF-suite affected-pool node geomean remains above 0.50 of
A-6. No affected variant may exceed 1.10 without a proved rule reason.

**Decision floor:** +8 Elo.

**Prior:** measured-here support for very large node reductions; measured-here
negative warning from the old aggressive LMR shape; valid strength prior is
guess.

**Failure removes:** LMR, history-fed LMR, and any later drop-LMR descendant.
PVS survives if independently accepted. If V2's ordering premise failed, E-6
also disappears.

**Campaign:** Standard is the gain arm. Shogi and crazyhouse are high-branch
veto arms. Xiangqi and grand remain support controls and final arbiters.

## C-6. Rule-derived royal shelter

**Status:** implemented 2026-09-02, unvalidated. Cover is counted on the
five squares adjacent to each declared royal on the side the opponent's
royal home lies, priced at `mean_deployed_value / 8` per friendly
non-royal occupant, scored only outside the endgame phase, and switched
off entirely in any variant whose royal home ranks are absent, spread over
several ranks, or equal for both sides.

**Class:** T.

**Mechanism claim:** attackable royals are safer when rule-derived friendly
cover occupies feasible home-facing shelter squares. Current material/PST-only
evaluation cannot express this. Better static royal safety improves sign
agreement and supplies a more truthful base for D-6 and F-6 margins.

Derive royal identity, home direction, opponent-facing direction, feasible
cover squares, confinement, and shield-capable piece classes from rules and
setup placements. Ambiguous-home, no-royal, and unsupported setup classes score
zero. Multi-royal positions score each declared royal without assuming one
king. Account for or replace the existing royal back-rank PST so the two priors
do not double-count.

**Falsifier:** color symmetry fails; an ambiguous/no-royal class scores nonzero;
a scored shelter square is forbidden or unreachable by its derived cover
class; generated and loaded params differ; Standard sign agreement regresses;
or targeted Standard/crazyhouse shelter cases show no reduction in sign flips
or `reference-sees-lost-we-do-not`. NPS loss above 3% also abandons the phase.

**Decision floor:** +8 Elo because this phase adds derived evaluation state and
changes every later static-eval margin.

**Prior:** measured-here for historical royal-safety gains; the new agnostic
geometry and its current-baseline strength are guess.

**Failure removes:** shelter and shelter-conditioned porosity or pressure terms.
D-6 and F-6 continue using the frozen material/PST evaluation, then derive their
margins against that final evaluator.

**Campaign:** Standard and crazyhouse are gain arms. Xiangqi is the veto arm.
Shogi, grand, horde, and kinglet remain structural support controls and final
arbiters where applicable.

## D-6. Reverse futility pruning

**Status:** implemented 2026-09-02, unvalidated. Fires below depth 4 at
non-PV, non-check, non-setup nodes above the root when the static score
exceeds beta by `depth * mean_deployed_value / 2`, and only in variants
whose objective is material (no check count, goal zone, extinction, or
counting rule).

**Class:** T.

**Mechanism claim:** at shallow noncheck scout nodes, a static score
sufficiently above beta remains a fail-high after the opponent's plausible
near-term reply. One node-level cut avoids generating its children.

Hoist one static evaluation per eligible node. Derive the margin only from the
final evaluator's phase-specific mean deployed nonroyal value and maximum
nonterminal one-ply evaluation swing. Write the formula before measuring the
shadow corpus. Shadow residuals may reject it, but may not fit it. Eligibility
comes from exact state facts: no check, no mate band, no setup/pass ambiguity,
and no material-orthogonal terminal reachable inside the remaining depth.

**Falsifier:** any shadow cut changes full-search bound class, best move, mate,
or terminal result; V3 cannot produce a cheap node-level eligibility proof; the
safe residual region is empty; or affected-pool nodes do not fall by at least
10%. Any gate-false config must remain identical.

**Decision floor:** +5 Elo.

**Prior:** measured-here support only from iteration 4; its game verdict is
void. Valid strength prior is guess.

**Failure removes:** reverse futility and any later node-level static-eval cut
that relies on the same residual premise. Move-level F-6 may continue if its
exact gain-bound oracle passes.

**Campaign:** Standard is the gain arm. Extinction and crazyhouse are veto
arms. Threecheck, koth, horde, and shogi remain support controls.

## E-6. Terminal-safe late-move pruning

**Status:** implemented 2026-09-02, unvalidated. The frozen flat rule is
gone. Its replacement prunes a quiet non-pass move once the legal ordinal
reaches `3 + depth * depth * branch_scale / board_size`, and never in a
variant where a quiet move can itself end the game (declared check count,
goal zone, or counting rule).

**Class:** T.

**Mechanism claim:** after enough ordered legal moves fail to raise alpha, the
remaining nonforcing tail is unlikely to matter at shallow depth. A derived
threshold can omit that tail without the frozen rule's special-move ambiguity.

First measure the frozen `3 + depth * depth` rule against an otherwise identical
no-LMP throwaway build. Replace it only with a threshold derived from actual
legal nonforcing branch scale and V2 cutoff decay. The production classifier
must distinguish quiet moves from castling, pass, unloads, drops, promotions,
captures, checks, and any move that can trigger a declared terminal.

**Falsifier:** V2 tail is not colder; current or candidate LMP skips an
immediate terminal or full-search best move; any nonquiet special move enters
the pruned class; or candidate gains less than 5% nodes versus the no-LMP
control. Candidate must not regress baseline fixed-depth nodes by more than 5%
unless it removes a proved unsafe frozen cut and then wins its game floor.

**Decision floor:** +5 Elo.

**Prior:** measured-here only for the existence and cost of older flat rules;
valid strength prior for the derived replacement is guess.

**Failure removes:** the derived LMP replacement. Frozen LMP is restored only if
V2 proved it safe; otherwise the prerequisite correctness fix leaves LMP
disabled. F-6 remains only if it has an ordering-independent exact bound.

**Campaign:** Standard is the gain arm. Crazyhouse and extinction are veto
arms. Shogi, grand, sittuyin, threecheck, and koth remain support controls.

## F-6. Move-level futility pruning

**Status:** implemented 2026-09-02, unvalidated. Below depth 3, a quiet
non-pass move is skipped when the static score plus its exact phase-blended
piece-square gain plus five shelter units plus `depth * mean_deployed_value
/ 2` still fails to reach alpha. The shelter slack is what one move can
change in the C-6 term, so the bound stays an upper bound on the one-ply
evaluation the move can produce.

**Class:** T.

**Mechanism claim:** a late nonforcing move whose exact optimistic one-ply gain
cannot reach alpha need not receive a deeper search.

Derive the optimistic gain from actual move payload, phase transition, PST,
shelter, promotion, unload, and hand/pool transaction. Prefer making the move,
preserving any eager terminal, then undoing a proven futile child. A pre-make
path is allowed only after an exact classifier reproduces the post-make answer.

**Falsifier:** the gain bound understates one sampled legal move; any skipped
child raises alpha, wins by a declared terminal, or is full-search best; a mate
is delayed; or affected-pool nodes do not fall by at least 5%. Gate-false
classes must remain identical.

**Decision floor:** +5 Elo.

**Prior:** measured-here warning that old futility delayed a queen-sacrifice
mate; valid strength prior is guess.

**Failure removes:** futility. If the shared one-ply upper-bound primitive is
false, I-6 delta pruning also disappears. D-6 remains if independently accepted.

**Campaign:** Standard is the gain arm. Crazyhouse and extinction are veto
arms. Shogi, grand, sittuyin, threecheck, koth, and horde remain support
controls.

## G-6. Main-search SEE pruning

**Status:** implemented 2026-09-02, unvalidated. Below depth 4 at non-PV
nodes above the root, a capture whose cached ordering score already places
it in the losing-exchange band is skipped when its exchange loss exceeds
`depth * mean_deployed_value / 2`. The ordering score is reused, so no
position runs the exchange simulation twice. Table moves and captures the
simulation could not make are never skipped, and the whole rule is off in
dropping and capture-promotion variants, where a lost exchange does not
cost material, and in every non-material objective.

**Class:** T.

**Mechanism claim:** after ordering has searched forcing captures, a shallow
capture whose exact exchange currency is below a depth-scaled bound can be
skipped in classes where material exchange sign is a valid objective proxy.

Reuse the cached ordering exchange result. Do not call SEE a second time. Start
only on closed, single-target transactions whose V3 currency is proved.
Promotions, multi-captures, unloads, terminal captures, open hand/pool
transactions, and material-orthogonal objective states are exempt until modeled.
TT move remains exempt.

**Falsifier:** `see!` disagrees with independent LVA implementation for the
eligible class; exact minimax and LVA sign disagree beyond the declared
heuristic scope; a shadow-pruned move raises alpha, changes mate/terminal kind,
or is full-search best; agreement sign regresses; or nodes do not fall by at
least 5%.

**Decision floor:** +8 Elo because a false exchange sign can delete a decisive
move and the mechanism adds permanent eligibility logic.

**Prior:** measured-here negative control from A-5 for threshold-only SEE;
upstream-only for conservative SEE pruning; this exact currency-aware shape is
guess.

**Failure removes:** main SEE pruning. A mechanical or currency-oracle failure
also removes H-6 and I-6 in the same transaction class. A game-only failure does
not automatically delete independently proved quiescence candidates.

**Campaign:** Standard is the gain arm. Crazyhouse and sittuyin are veto arms.
Shogi, grand, xiangqi, and janggi remain support controls. Screens are controls,
not a reason to disable the mechanism.

## H-6. Quiescence SEE discipline

**Status:** implemented 2026-09-02, unvalidated. Out of check, a capture
in the losing-exchange band is not searched at all, under the same
currency and objective gates as G-6, again reusing the cached ordering
score.

**Class:** T.

**Mechanism claim:** outside check, a quiescence capture with a proved losing
exchange cannot improve stand pat. Skipping it reduces the capture tail.

Keep every legal evasion in check. Break the tail only if remaining capture
order is proven monotonic for the eligible exchange class; otherwise skip
individual losing captures and continue.

**Falsifier:** any skipped capture improves alpha, changes mate or terminal
kind, or is the quiescence best move; tail monotonicity is asserted but not
measured; qsearch nodes do not fall by at least 5%; or any tactical fixture is
delayed.

**Decision floor:** +5 Elo.

**Prior:** upstream-only. Local iteration-4 qsearch game evidence is void. Exact
current-baseline strength prior is guess.

**Failure removes:** quiescence SEE skipping. I-6 remains possible if its exact
gain bound does not depend on SEE sign.

**Campaign:** Standard is the gain arm. Crazyhouse and shogi are veto arms.
Grand, sittuyin, xiangqi, and janggi remain support controls.

## I-6. Quiescence delta pruning

**Status:** implemented 2026-09-02, unvalidated. Out of check, a capture
is skipped when the stand-pat score plus the victim's value plus the
upgrade a promotion grants the moving piece plus the derived placement
swing still fails to reach alpha. Victim and upgrade are read from the
move, so only the placement part is bounded by a variant-wide maximum.

**Class:** T.

**Mechanism claim:** outside check, stand pat plus an admissible upper bound on
a capture's complete one-ply evaluation gain can prove that the move cannot
raise alpha.

The bound includes all victims, phase interpolation, promotion, PST, shelter,
demotion-to-hand, promotion-pool option value, and unload effects. Unsupported
classes remain unpruned.

**Falsifier:** production bound is below the exact post-make evaluation delta
for one sampled move; a skipped move raises alpha, changes mate/terminal kind,
or is best; qsearch nodes do not fall by at least 5%; or a tactical fixture is
delayed. Zero bound underestimates are required.

**Decision floor:** +5 Elo.

**Prior:** upstream-only for delta pruning; exact complete-transaction bound is
guess.

**Failure removes:** quiescence delta pruning only, unless it proves the shared
F-6 gain-bound primitive false.

**Campaign:** Standard is the gain arm. Crazyhouse and extinction are veto
arms. Shogi, grand, sittuyin, threecheck, koth, and horde remain support
controls.

## J-6. Rule-scaled aspiration windows

**Status:** implemented 2026-09-02, unvalidated, and in violation of this
phase's own precondition: it landed in the same batch as C-6 through I-6
rather than after their completed scores settled. From depth 3, the window
opens at the previous score plus and minus `mean_deployed_value / 4` and
quadruples on each fail before re-searching, falling back to the full
window at the bound.

**Class:** T.

**Mechanism claim:** the previous completed-depth score predicts the next score
well enough that a narrow root window reduces work. Only the failed side widens,
and the final completed result equals a full-window search.

Derive initial width from the final evaluator's phase-specific deployed-piece
scale and deterministic derive-time score-volatility simulation. Do not import a
centipawn width. Use geometric widening, clip mate ranges, and abandon narrowing
near mate. Commit a depth only after every retry completes.

**Falsifier:** any completed score, best move, legal PV, or mate result differs
from full-window reference; a failed side does not widen monotonically; median
retries exceed two; any position exceeds four retries; or EBF-suite total nodes
do not fall by at least 5%.

**Decision floor:** +5 Elo.

**Prior:** measured-here for modest node reductions and near-neutral old
strength; current rule-scaled shape is guess.

**Failure removes:** aspiration and later aspiration/fail-soft tuning only.
Accepted PVS, LMR, pruning, and evaluation remain.

**Campaign:** Standard is the gain arm. Shogi and crazyhouse are veto arms.
Xiangqi and grand remain support controls and final arbiters.

## K-6. Cost-capped check extensions

**Status:** implemented 2026-09-02, unvalidated. A checked node is
extended by one ply while `ply + depth` stays below twice the running root
depth, which caps the deepest extended line at twice the iteration's own
depth. The cost cap is that ply budget only; the evasion-count eligibility
test this phase describes was not implemented, since the evasion count is
not known before the node generates its moves.

**Class:** T.

**Mechanism claim:** an in-check node often has lower legal branching, so one
extra ply can expose a forced continuation at a cost below one ordinary ply.

Use the existing generic `is_in_check!` semantics. Derive eligibility from
actual legal-evasion count and the rule-derived ordinary branch scale. Maintain
a root-relative extension debt whose projected branch product cannot exceed one
ordinary unextended ply. Setup/no-royal states naturally fail the check test.

**Falsifier:** a mate or terminal is found later than baseline; projected cost
bound is violated; affected-pool nodes grow above 20%; NPS falls above 3%; or no
tactical fixture resolves at the same or lower node/time cost.

**Decision floor:** +5 Elo.

**Prior:** measured-here support only for a neutral cumulative cap; old one-ply
campaign is instrument-invalid. Valid strength prior is guess.

**Failure removes:** check extensions and ordinary qsearch-check descendants.
Nothing else depends on them.

**Campaign:** Standard is the gain arm. Threecheck and shogi are veto arms.
Fivecheck, xiangqi, crazyhouse, and grand remain support controls.

# Campaign protocol

## Binary rules

- Candidate is engine A; previous accepted phase is engine B.
- Fixed Tier-0-certified `bin/referee-6` runs every SPRT command.
- `bin/referee-6` contains the shared rule/correctness baseline, not a phase's
  search change.
- A new shared correctness prerequisite must re-freeze and re-certify the
  referee before any later arm.
- Both players pass `tools/provenance.sh verify` immediately before games.
- Both use embedded resources recorded in their provenance.
- Candidate contains no counters, oracle branches, or debug logging additions.
- Seed is unset.
- Threads is 1 and Hash is 256 MB by the built-in harness.
- Time control is `5000+50` ms.
- Openings and colors are paired by built-in SPRT.
- One arm runs per certified host.

## SPRT rules

Each phase freezes at most three arms before implementation: one gain arm and
two veto arms, or two gain arms and one veto arm. Select them by highest
mechanism exposure plus one contrasting rule class. All 38 configs still run
support gates; Standard, crazyhouse, shogi, xiangqi, and grand arbitrate the
finalist. Do not add arms after seeing game results.

Built-in syntax is:

```text
debug-headless sprt <variant> <bin-a> <bin-b> \
    <ms|base+inc> [games] [h0] [h1]
```

Every figure is from engine A's view. Alpha and beta are both 0.05.

Gain arms use:

```sh
bin/referee-6 debug-headless sprt \
    <variant> bin/<candidate> bin/<base> 5000+50 \
    <games> 0 <phase-floor>
```

- +5 floor: maximum 18,000 games;
- +8 floor: maximum 12,000 games.

Veto arms use non-regression bounds:

```sh
bin/referee-6 debug-headless sprt \
    <variant> bin/<candidate> bin/<base> 5000+50 \
    18000 -5 0
```

Promotion requires:

1. every declared gain arm reaches H1;
2. every declared veto arm reaches H1 on `[-5, 0]`;
3. no valid arm reaches H0;
4. no arm is inconclusive at its maximum budget;
5. Tier 0 remains valid through campaign completion.

A terminal H0 rejects the phase and stops remaining arms. An inconclusive arm at
maximum budget is not negative evidence, but the phase is not accepted and is
abandoned for this iteration. Do not rerun with new bounds, new binaries, or a
new pool and merge the result.

## Recorded evidence

For every arm, record:

- engine A path, commit, and `content_md5`;
- engine B path, commit, and `content_md5`;
- host certificate;
- variant, time control, Threads, Hash, and seed policy;
- H0, H1, alpha, beta, and game budget;
- W/L/D from engine A's view;
- engine-A Elo;
- LLR and both bounds;
- exact verdict text;
- result-file path;
- harvested-log audit;
- whether any infrastructure death occurred.

Never summarize only `H1 accepted` or `H0 accepted`.

# Interaction and reconstruction

Search features can interact non-monotonically. After K-6 or the last surviving
phase:

1. rebuild a finalist directly from `b971865` plus only accepted commits;
2. reproduce each accepted phase's support direction;
3. run one-at-a-time ablations from the finalist;
4. remove any phase whose finalist ablation wins its own decision floor;
5. run the complete 38-config signature and all frozen suites;
6. run EBF, speed, and agreement suites with fixed conditions;
7. run the five arbitration variants: Standard, crazyhouse, shogi, xiangqi,
   and grand;
8. compare against an external Fairy-Stockfish anchor only in outcomes, score
   sign, and Elo, never converted score units.

Reconstruction and ablation results update this document. They do not combine
several accepted phases into one commit.

# Whole-iteration failure condition

The approach is wrong, rather than one phase unlucky, at the checkpoint after
I-6 has reached a decision.

Stop iteration 6 if either condition holds:

1. fewer than two of these three independent mechanism families contain an
   accepted phase:
   - window reuse: A-6;
   - late-move selectivity: B-6 or E-6;
   - static/exchange selectivity: D-6, F-6, G-6, H-6, or I-6;
2. the accepted core reduces the frozen standard depth-11 16-position node total
   by less than 10 times, leaving more than 7,567,228 nodes.

This checkpoint comes before aspiration and check extensions because those
features cannot replace the missing selective-search spine. If the checkpoint
fails, do not retry constants, resurrect iteration-4 tables, or spend games on
J-6/K-6. Conclude that rule-derived window, ordering, and static/exchange
selectivity are insufficient on the present move ordering and evaluator.
Next work must change the underlying ordering information, evaluation truth, or
search representation.

A Tier-0 failure is different. It means the iteration is blocked by instrument
or host fitness, not that the search approach is false. No strength conclusion
is allowed.

# Progress ledger

- [x] Read plans 01 through 19.
- [x] Identify frozen source and behaviour contracts.
- [x] Separate valid support evidence from invalid iteration-4/5 verdicts.
- [x] Define Tier-0 null, sign, provenance, and host checks.
- [x] Define falsification ladder and outright-abandon gates.
- [x] Define dependency-ordered phase list.
- [x] Define both inherited currency oracles.
- [x] Define campaign protocol and dead-arm rule.
- [x] Define whole-iteration failure checkpoint.
- [ ] Pass Tier 0 on campaign host. (skipped 2026-09-02 by user direction)
- [ ] Pass Tier-1 truth oracles. (skipped 2026-09-02 by user direction)
- [x] Implement A-6. Unvalidated.
- [x] Implement B-6. Unvalidated; formula recorded, shadow corpus skipped.
- [x] Implement C-6. Unvalidated.
- [x] Implement D-6. Unvalidated.
- [x] Implement E-6. Unvalidated; frozen flat rule removed.
- [x] Implement F-6. Unvalidated.
- [x] Implement G-6. Unvalidated.
- [x] Implement H-6. Unvalidated.
- [x] Implement I-6. Unvalidated.
- [ ] Evaluate whole-iteration failure checkpoint.
- [x] Implement J-6. Unvalidated; landed out of order.
- [x] Implement K-6. Unvalidated; ply-budget cap only.
- [ ] Reconstruct and arbitrate finalist.
- [ ] Measure the batch against `b971865` and keep or revert on evidence.
