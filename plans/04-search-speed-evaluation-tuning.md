# Search Speed, Evaluation, and Strength Plan

## Context

FIDE round robin now settles current lineage:

- Stage Q: `86 +/- 6`
- Stage P: `82 +/- 6`
- Stage O: `61 +/- 6`
- Stage M: `36 +/- 6`
- Stage N: `34 +/- 6`
- Stages A-L: approximately `1` through `-19`

Full ranking:

```text
Rank Name        Elo  +/-  Games  Score  Draw
1    stageQ       86    6   8832  62.1%  32.8%
2    stageP       82    6   8831  61.6%  32.0%
3    stageO       61    6   8833  58.7%  34.7%
4    stageM       36    6   8833  55.2%  40.7%
5    stageN       34    6   8832  54.9%  40.6%
6    stageB        1    5   8832  50.1%  52.0%
7    stageF       -3    5   8832  49.6%  53.1%
8    stageD       -9    5   8832  48.7%  53.3%
9    stageJ       -9    5   8830  48.7%  52.8%
10   stageC       -9    5   8832  48.7%  52.3%
11   stageI      -10    5   8832  48.6%  53.0%
12   stageL      -12    5   8831  48.3%  52.9%
13   stageE      -12    5   8832  48.2%  53.7%
14   stageA      -13    5   8832  48.1%  50.5%
15   stageK      -14    5   8832  48.1%  53.0%
16   stageG      -14    5   8832  48.0%  52.6%
17   stageH      -19    5   8832  47.3%  53.0%
18   fsf-1800    -32    7   8832  45.4%   6.3%
19   fsf-1700   -150    8   8830  29.6%   4.7%
```

Termination counts use columns `R3`, `R50`, `IM`, `SM`, `LB`, `LW`, `WB`,
`WW`, and `DC` for threefold, fifty-move, insufficient material, stalemate,
Black-mates loss, White-mates loss, Black-mates win, White-mates win, and
all disconnects:

```text
Name       R3  R50   IM  SM   LB   LW   WB   WW  DC
stageA   4339   17   99   4 1078 1275  906 1114   0
stageB   4467   20  105   4  976 1134  973 1153   0
stageC   4466   20  123   7  996 1227  928 1065   0
stageD   4568   19  119   2  979 1194  933 1018   0
stageE   4601   16  128   2 1015 1185  881 1004   0
stageF   4547   20  120   5  943 1164  976 1057   0
stageG   4509   24  107   4 1065 1206  927  990   0
stageH   4557   12  107   2 1056 1259  869  970   0
stageI   4540   24  110   7 1002 1199  936 1014   0
stageJ   4506   24  126   3 1027 1171  961 1012   0
stageK   4540   14  116   8 1010 1239  859 1046   0
stageL   4526   21  117   4 1029 1201  923 1009   1
stageM   3260   81  245   9 1033 1127 1360 1718   0
stageN   3231   86  259   7 1035 1160 1397 1657   0
stageO   2719   75  259   8 1033 1085 1746 1908   0
stageP   2550   65  208   6  942 1030 1876 2153   1
stageQ   2587   71  229  11  933  962 1898 2141   0
fsf1700   349   41   26   1 2877 3129 1082 1325   0
fsf1800   442   58   45  10 2135 2408 1733 2001   0
```

Stage Q remains baseline. Its four-Elo lead over P is not independently
significant, but Q ranks first, has fewer mating losses, and gives no evidence
for reverting time management. O/P/Q gains are large. M remains proven. N stays
because its purpose is hopper correctness; FIDE does not distinguish it from M.
Current `main` contains Stage Q lineage plus later documentation fixes.

Goal: improve time-to-depth, NPS, evaluation quality, and strength while keeping
engine variant-agnostic. No FIDE geometry, dialect tokens, single-royal
assumptions, or special cases for named variants.

`gpt-codex` commit `5ae0cde` is reference material, not cherry-pick material.
Port only selected ideas in isolated stages. Reject its bundled 36-file refactor,
seed override, benchmark CLI, search telemetry, compatibility layer, unrelated
move refactors, and parameter-file header/fingerprint work.

## User-selected scope

Keep:

- SPRT subprocess and logging hardening;
- TT/PV move fast path;
- `royal_list` shelter iteration and incremental pair-score cache;
- root allocation reuse and profile-gated SEE repetition bypass;
- role derivation fix;
- actual-army phase thresholds and dynamic occupancy derivation;
- Stage R game-level validation and scalar tuning;
- capped check extensions;
- Stage S ProbCut;
- LMR/LMP retuning;
- Stage T first-ply qsearch checks;
- Stage U real mobility.

Exclude:

- deterministic Zobrist seed or `ANEKAMACAM_SEED`;
- new bench/derive/perft CLI;
- new search-statistics feature or sampled-hashfull work;
- `U4096` to `U2048` conversion;
- history preservation/aging across root searches;
- legacy parameter or dataset parsers;
- parameter cache version/fingerprint compatibility infrastructure;
- Stage V SMP, ponder, and seldepth work.

Evolution stays linear. New parameter and dataset schemas replace old schemas;
all shipped files regenerate atomically. No compatibility branches remain.

## Global execution rules

- Update `plans/04-search-speed-evaluation-tuning.md` first with round-robin
  results, selected scope, rejected work, and stage verdicts.
- One logical stage per accepted commit. Failed experiments leave no commit.
- Build only with `cargo build --release`.
- Never add `#[test]` modules to engine code.
- Always pass finite perft limits; never run all 6752 FIDE positions at depth.
- Use `parse_fen(..., None)` for internal FEN loads and round trips.
- Read `configs/example.conf` and `res/dicts/example.dict` before any schema edit.
- Preserve all-node history malus, correction-history fail-high-only reads, and
  correction-history capture blend.
- Verify candidate and baseline binary MD5 before every comparison.
- Accept search nondeterminism. Do not claim exact cross-process node identity.
  Use repeated independent multi-position runs and compare distributions.
- Cover FIDE, crazyhouse, shogi, and xiangqi. Shogi remains mandatory canary.

## Stage R: Speed, derivation, and trustworthy tuning

Stage R starts directly after shipped Stage Q. It contains isolated sub-stages;
each accepted sub-stage gets its own commit and verdict.

### R0 Record round robin

Edit `plans/04-search-speed-evaluation-tuning.md` and `plans/03-strength.md`:

- mark M-Q complete;
- record supplied FIDE table and termination counts;
- name Stage Q lineage as baseline;
- note Q/P overlap and why Q remains selected;
- note Shogi round robin still needed for cross-variant arbitration;
- replace old W/X/Y/Z/AA scope with user-selected stages below.

No engine behavior change.

### R1 Harden SPRT runner

Critical files:

- `src/debug/sprt.rs`
- `src/io/logger.rs`

Replace panic-prone child I/O with structured runner errors:

1. Setup failures during canonicalization, spawn, UCI handshake, option setup, or
   readiness stop match cleanly and identify binary.
2. Read/write/flush failure after game starts becomes engine failure.
3. Child death during game scores loss for dead engine; ambiguous double failure
   aborts match instead of inventing result.
4. Diagnostics include binary, game, command/token, exit status, and stderr.
5. Log names include process ID so concurrent runs cannot overwrite each other.
6. Preserve current per-binary sandboxes and candidate-first result convention.

Verification: deliberate child termination during setup and during a game must
neither hang nor panic whole runner; normal fixed-movetime and clock runs must
retain score orientation and time-control behavior.

Status 2026-07-18: implemented. Release build and concurrent PID-log smoke pass.
User chose to skip subprocess crash integration smoke.

Use existing `hotpath`/`hotpath-alloc` support only. Add no telemetry framework.
Before each optimization, profile repeated four-variant searches. Keep stage only
when aggregate geometric-mean NPS or short-search latency improves at least 2%
and no variant shows a stable regression over 2%.

### R2 TT/PV move fast path

Critical files:

- `src/game/search/move_ordering.rs`
- `src/game/position/search.rs`

Before scoring move tail, scan with `m_matches!` for TT/PV move, swap it into
first slot, assign existing maximum score, and let selection return immediately.
Apply in alpha-beta and qsearch. Preserve relative ordering of every other move.

Verification: repeated multi-position timing, legal PV checks, bounded perft,
release `d`, and self-play. Revert if chosen moves or score behavior expose an
ordering change beyond expected search nondeterminism.

### R3 Royal-list shelter loop

Critical file: `src/game/position/evaluation.rs`.

Replace full piece-type scan in `king_shelter!` with direct iteration over
`state.royal_list[color]`. Keep multi-royal support and adjacency semantics.
This is selected `gpt-codex` work and should be ported narrowly.

### R4 Incremental pair-score cache

Critical files:

- `src/game/representations/state.rs`
- `src/game/moves/move_list.rs`
- `src/game/position/evaluation.rs`
- `src/game/util.rs`

Add `State.pair_score`. Update it only on count transitions `1 -> 2` and
`2 -> 1`, with color sign and current derived pair bonus. Initialize it in
`refresh_eval_state`, copy it in forks/clones, and verify it in release `d` state
checks. Replace evaluation's piece-type loop with cached value.

Because make/undo changes, run finite perft across every available suite plus
self-play and FEN-sweep state verification.

### R5 Reuse root allocations

Critical file: `src/game/position/search.rs::clear_search`.

Use resize-once plus `fill` for continuation, butterfly, capture, killer,
static-eval, and exclusion arrays. Preserve full reset behavior; do not age or
retain histories. Measure repeated short searches separately from deep searches.

### R6 Profile-gated SEE repetition bypass

Critical files:

- `src/game/search/move_ordering.rs`
- `src/game/moves/move_list.rs`
- `src/game/representations/state.rs`

Proceed only if existing hotpath data shows SEE transition/bookkeeping cost.
Add explicit exchange-simulation transition mode that reuses normal arbitrary
move semantics but skips repetition-map and draw-clock bookkeeping only. Keep
board occupancy, piece lists, side to move, royal state, promotion, drops,
multi-captures, unloads, stand-offs, and legality data exact. Snapshot records
whether repetition was tracked so undo remains balanced.

Revert unless aggregate gain exceeds 2%. Run SEE corpus, all-suite finite perft,
release `d`, self-play walks, and FEN sweep.

Each derivation sub-stage regenerates affected `res/param/*/latest.param` files
through existing embedded-first flow. No new derive command. Delete current
files, build, load variants through existing console path, export, rebuild, and
verify MD5.

### R7 Fix role derivation

Critical files:

- `src/game/search/parameters.rs::derive_piece_roles`
- `src/io/piece_io.rs::collect_piece_type_pairs`
- `res/param/*/latest.param`

Rank one representative per color-paired non-royal piece type. Retain equal
values instead of collapsing them through `HashSet`. Bottom ceil(10%) becomes
non-big; top ceil(20%) becomes major. Apply roles to both colors. Force royals
outside big/major classification. Log ranked values and assignments.

### R8 Derive phase from actual army

Critical files:

- `src/game/search/parameters.rs::derive_eval_parameters`
- `src/game/representations/state.rs::game_phase_score`
- `src/game/position/evaluation.rs`

Compute initial phase from actual active non-royal big-piece material, including
pieces in hand where rules allow them. Use starting score for opening/endgame
boundaries, with explicit zero-phase handling and guaranteed positive blend
denominator. Start with 90% opening and 35% endgame values from `gpt-codex`, then
validate phase traces before strength testing. Do not import unrelated
`royal_front_mask` changes.

### R9 Derive occupancy from setup

Critical files:

- `src/game/search/parameters.rs`
- `src/prelude.rs`
- `res/param/*/latest.param`

Replace fixed opening/endgame occupancy constants with setup-derived board fill.
Opening occupancy equals occupied starting squares divided by board size.
Endgame occupancy starts at one third of opening and stays in `[0.05, 0.25]`.
Pass values explicitly through piece value, square score, PST, and zone-attack
derivation. Keep formulas generic for sparse, dense, drop, and hopper variants.

After R9, run FIDE and Shogi SPRT plus targeted Xiangqi validation, then a
Q/R round-robin checkpoint before scalar tuning.

### R10 Dataset integrity and split

Critical files:

- `src/debug/datagen.rs`
- `src/debug/tuning.rs`

Return explicit complete/interrupted game outcome. Write no positions from an
interrupted or incomplete game. New rows contain game ID, FEN, and result. Parser
accepts only new schema. Split train/validation by game ID, never position.
Report result and phase distributions, train and validation MSE, and best epoch.
Export best validation epoch rather than final epoch; stop or warn on sustained
validation regression.

### R11 Replace parameter schema with scalar-capable payload

Critical files:

- `src/io/game_io.rs`
- `src/game/search/parameters.rs`
- `src/game/representations/state.rs`
- `src/debug/tuning.rs`
- `res/param/*/latest.param`

Extend current flat payload with exact scalar tail and require exact new length.
Do not keep old-length parser, fallback defaults for old files, version branch,
or fingerprint layer. Regenerate every shipped parameter file in same stage.
Persist linear terms: tempo, shelter, shield, castling, open shield, king-danger
scale, imbalance, pawn-structure terms, passed-pawn scales, and pair bonus.
`draw_bias` and search margins remain derived-only.

### R12 Tune variants separately

Extend `TuneShape`, feature extraction, constraints, and export to match evaluator
signs and phase gates exactly. Tune FIDE, Shogi, and Xiangqi separately. Require
validation improvement before SPRT. Commit only parameter sets passing targeted
SPRT; leave other variants on newly regenerated derived values.

## Stage S: Stabilize tree, then test ProbCut

### S0 Cap cumulative check extensions

Critical file: `src/game/position/search.rs`.

Limit cumulative extension horizon using `ply + depth` relative to root depth.
Start with maximum two extra plies. Keep positive singular extension disabled.
Run legal-PV/FEN checks, four-variant time-to-depth, FIDE SPRT, and Shogi
non-regression SPRT.

### S1 Retune LMR then LMP

Critical files:

- `src/game/representations/state.rs`
- `src/game/position/search.rs`
- `src/prelude.rs`

Move current coefficients to named constants without behavior change. Compare a
small grid of current and consistent log-depth/log-move formulas using repeated
multi-position runs. Choose LMR first, then LMP. Preserve dangerous-push,
improving, check, promotion, drop, and history adjustments plus all-node malus.
Only final accepted constants enter commit.

### S2 ProbCut experiment

Critical files:

- `src/game/position/search.rs`
- `src/game/search/parameters.rs`
- `src/prelude.rs`

At non-PV, non-check, non-excluded, non-mate nodes after NMP and before futility,
derive `probcut_margin` from average piece value. Search at most three SEE-winning
captures: qsearch zero-window first, then reduced alpha-beta verification. Respect
TT evidence and singular-multicut overlap. Revert immediately without measured
node/time headroom or positive FIDE SPRT; require Shogi non-regression.

## Stage T: First-ply qsearch checks

Critical file: `src/game/position/search.rs::quiescence_search`.

Add qsearch depth. At first q-ply outside check, generate all moves; retain normal
captures plus legal quiet moves that give check after make. Recursive qsearch
returns to capture-only generation. No FIDE-specific gives-check shortcut.
Accept only with positive FIDE SPRT, Shogi non-regression, and less than 5%
four-variant time-to-depth cost.

## Stage U: Real mobility

Critical files:

- `src/game/position/evaluation.rs`
- `src/game/search/parameters.rs`
- shared movement-walk logic in `src/game/moves/move_list.rs`
- Stage R tuning schema

Extract one shared pseudo-mobility counter matching current per-leg movement,
screen, capture, promotion, hopper, and occupancy semantics. Verify it against
pseudo-legal destinations on a FEN corpus. Precompute expected piece/square
baseline from phase occupancy. Score actual minus expected mobility for
non-pawn, non-royal pieces. Start with big pieces; add lazy-eval gate if needed.
Persist and tune opening/endgame mobility scales through Stage R schema.

Reject whole stage if time-to-depth cost remains above 5%, validation MSE does
not improve, FIDE SPRT fails, or Shogi/Xiangqi regress. Re-run scalar tuning if U
lands.

## Stage V

Remain deferred. No SMP coordination, ponder, or seldepth work in this plan.

## Verification protocol

### Correctness

```bash
cargo build --release
target/release/anekamacam perft fide 4 200
target/release/anekamacam perft crazyhouse 3 200
target/release/anekamacam perft shogi 3 200
target/release/anekamacam perft xiangqi 3 200
```

Use existing console/perft paths if command syntax differs on current stage.
For make/undo changes, run every available suite with explicit finite limits,
release UCI `d`, self-play walks, and FEN sweep.

### Speed under nondeterministic search

- verify baseline/candidate MD5 first;
- use same multi-position corpus, depth, hash, threads, and process conditions;
- run at least ten independent process repetitions per variant;
- compare median and geometric-mean NPS, nodes, and elapsed time;
- use multi-position totals, never one start-position result;
- repeat suspicious effects rather than treating one run as authoritative.

### Strength

Candidate remains first argument:

```text
sprt bin/candidate bin/baseline 100 2000 0 5
sprt bin/candidate bin/baseline 100 2000 -5 0
sprt bin/candidate bin/baseline 3000+100 2000 0 5
```

Use `[0,5]` for expected gain and `[-5,0]` for non-regression. Run targeted
variant session after every derivation/evaluation/search behavior stage. Run
round-robin checkpoints after R9, R12, and accepted S/T/U block.

## Stop conditions

Stop and revert stage when:

- release build, bounded perft, legal PV, or state verification fails;
- speed gain stays below threshold after repeated multi-position measurement;
- any variant shows stable time-to-depth regression above stage limit;
- SPRT reaches regression boundary;
- tuning improves training but not game-level validation;
- implementation requires variant names, protocol dialects, or unsupported
  assumptions about board geometry, royals, pawns, captures, drops, or hoppers.
