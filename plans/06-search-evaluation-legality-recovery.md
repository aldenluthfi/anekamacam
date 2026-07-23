# Search, Evaluation, and Legality Recovery Plan

## Context

Current engine gained almost all measured FIDE strength from Stage M-Q:
repetition-aware draw scoring, royal safety, and time management. Search
micro-stages A-L cluster near neutral. Latest remote round robin, sampled on
2026-07-23, changes immediate priorities:

- Standard, about 6,690 games per engine: Stage U leads at `81 +/- 8`; Stage S
  has `74 +/- 7`; Stage R `66 +/- 7`; Stage V fell to `39 +/- 7`.
- Shogi and Grand samples remain small, but both are contaminated by frequent
  illegal terminations. They cannot arbitrate strength yet.
- Standard illegal move `c5b6` follows castling while an old en-passant right
  remains active.
- Shogi illegal moves occur after configured fourfold repetitions; root search
  returns a stale/fallback move instead of `bestmove (none)`.
- Grand Chess en-passant moves are emitted as strings such as `i5j4j5`; its
  dictionary strips CMN delimiters but not the extra captured-square payload.
- Other Grand failures return a move for the wrong side immediately after a
  replayed move was silently rejected. `handle_position` ignores both an
  unmatched token and a false `make_move!` result.

Goal: establish trustworthy legality and measurements, reset baseline to Stage
U behavior through Stage W, remove neutral search machinery, replace FIDE-shaped
evaluation with cheaper rule-derived terms, then tune only final small feature
set. Every gate covers all five target variants: Standard, Crazyhouse, Shogi,
Xiangqi, and Grand. Preserve current user work in `src/main.rs` and
`tools/datagen_local.sh`.

## A-V map and recommended verdicts

Stage C naming needs one qualification: its build ref is PV-walk fix `e9dc4f0`,
but LMR retune `b4f9af6` is its ancestor. Stage B receives `e9dc4f0` as a
cherry-pick, so effective B-to-C difference remains LMR retune.

| Stage | Cumulative addition | Verdict |
|---|---|---|
| A | remove IID, keep IIR | Keep. Simpler search and no evidence IID earns cost. |
| B | 1-ply and 2-ply continuation history | Isolate. Best A-L historical delta, but large table/allocation cost. |
| C | more aggressive quiet LMR | Isolate, likely remove. Both old RR and later V show reduction strength risk. |
| D | gentler aspiration widening | Keep provisionally. Tiny, neutral, low complexity; recheck after lean search. |
| E | eval-scaled NMP plus endgame verification | Keep provisionally. Large node gain, neutral strength, generic guards. |
| F | TT static-eval cache and mask indexing | Keep. Measured speed gain and later search depends on TT fields. |
| G | capture history | Remove candidate. Historical negative delta and extra hot-path state. |
| H | singular multicut and negative extension | Remove candidate. Worst A-L rank and high complexity. |
| I | pawn-hash correction history | Test only as I+L group. Never retain I alone. |
| J | direct continuation-history indexing | Keep only if B survives. Otherwise delete with B. |
| K | flat piece lists | Keep. Shared representation simplification and state-verification coverage. |
| L | correction-history read gating | Keep only if I survives; otherwise delete I+L together. |
| M | repetition scoring and material draw bias | Keep strength gain. Separate rule termination from search-cycle heuristic. |
| N | per-leg hopper mobility derivation | Keep correctness fix. |
| O | royal back-rank PST, pawn shield, castling incentives | Decompose. Preserve gain, replace FIDE-shaped parts only after ablation. |
| P | zone attack and open-shield penalty | Keep zone attack; ablate/replace open-shield term. |
| Q | stability-scaled soft deadline | Keep. Standard-positive, no fixed-depth search cost. |
| R | runner/data integrity, role fix, TT/PV and eval fast paths | Keep. Correctness and measured aggregate NPS gain. |
| S | cumulative check-extension cap | Keep provisionally; verify after legality fixes. |
| T | scalar-tail schema, PST export layout fix, regenerated params | Keep layout correctness, split data/schema effects, then shrink schema. |
| U | ProbCut | Keep provisional baseline. Current Standard leader; recheck legal Shogi/Grand. |
| V | sqrt/sqrt quiet-LMR retune | Revert. Current U-to-V Standard loss is large and directly isolates V. |

Progression remains cumulative after Stage V. New accepted commits start at
Stage W; history is not rewritten. Stage W explicitly reverts only Stage V's
quiet-LMR constants, restoring Stage U search behavior on top of the full
A-V lineage. Rejected experiments consume no stage letter.

## Stage progression from W

- **Stage W `lmr-revert`:** revert Stage V quiet-LMR constants only.
- **Stage X `move-state`:** normalize en-passant and halfmove transitions.
- **Stage Y `terminal-search`:** terminal move generation, PV, ponder, and
  `bestmove (none)` fixes.
- **Stage Z `position-replay`:** transactional protocol replay and fail-closed
  invalid-position handling.
- **Stage AA `move-translation`:** generic CMN payload stripping in dictionaries.
- **Stage AB `replay-fixtures`:** protocol replay and move-round-trip harness.
- Later letters name only accepted lean-search, evaluation, schema, perft,
  datagen, and tuning commits. Their exact letter is assigned after preceding
  experiments pass, so failed candidates leave no gaps.

## Phase 1: Make positions and protocol output trustworthy

### Stage X: Normalize transient move state once

Critical file: `src/game/moves/move_list.rs::make_move!`.

- Compute next en-passant state once for every non-null move.
- Default to `NO_EN_PASSANT`; only a move carrying `creates_enp!` installs a
  new value.
- Apply `hash_update_en_passant!` once after transition data is known.
- Remove branch-local duplication from quiet, single-capture, and
  multi-capture paths.
- This makes castling and drops clear stale rights too. It directly fixes
  Standard failures after `...d5 O-O O-O`.
- Consolidate halfmove-clock update under schema rule: captures and drops reset;
  a configured reset piece resets; every other move, including castling,
  increments. Current castling path incorrectly resets unconditionally.

### Stage Y: Stop search and PV at terminal positions

Critical files:

- `src/game/moves/move_list.rs::generate_all_moves_and_drops`
- `src/game/moves/move_list.rs::legal_moves!`
- `src/game/position/search.rs::{clear_search,search_position}`
- `src/game/search/transposition.rs::fill_pv_line!`
- `src/io/protocols/protocol.rs::print_bestmove`

Changes:

- Return no moves when `state.game_over` is true.
- Reset PV lengths/table/line at each root search, not only search histories.
- Fast-return a null `SearchResult` for a terminal root.
- Stop PV walking before TT probing when a walked move sets `game_over`; truncate
  line at that exact ply.
- Emit `bestmove (none)` with no ponder for terminal or invalid roots. Never use
  fallback move generation after a rule draw.

This fixes Shogi fourfold-repetition games currently ending with a repeated
legal-looking move reported as illegal.

### Stage Z: Make `position` replay transactional and fail closed

Critical files:

- `src/io/protocols/protocol.rs::{handle_position,start_search}`
- `src/io/move_io.rs::parse_move`

Changes:

- Replay FEN and move tokens into temporary `State`.
- Commit temporary state only after every token matches and every
  `make_move!` call returns true.
- Record generic `position_valid` state in protocol `Session`.
- On failure, log first failing ply, token, and whether failure was parse or
  legality. Refuse search and emit `(none)` until next valid `position`.
- Correct `parse_move` docs: generation is pseudo-legal; legality comes from
  `make_move!`.
- Apply same checked behavior to debug-console `move` command.

No UCI/USI/UCCI concepts enter engine state. Validation remains common protocol
logic.

### Stage AA: Fix dictionary move-payload translation

Read and update schema docs first:

- `configs/example.conf`
- `res/dicts/example.dict`

Then update relevant `res/dicts/*.dict`, especially `grand.dict`:

- Replace fixed two-character capture/unload payload regexes with square regexes
  supporting multi-digit ranks.
- Strip CMN captured-square metadata before generic `*`/`:` removal.
- Keep promotion and drop translation order unchanged.
- Do not change piece movement tokens.

Grand en-passant must format as protocol start/end, never `i5j4j5`.

### Stage AB: Turn observed failures into replay fixtures

Add external harness under `tools/`, not `#[test]` engine modules. Reuse protocol
commands and release `d` verification.

Fixture groups:

- Standard: `d4 Nf6 Bf4 c5 dxc5 e5 Bxe5 Nc6 Bd6 Be7 Nc3 Qa5 Qd2 b5
  O-O-O O-O`; next stale `c5b6` must not exist.
- Shogi: king/gold, king/silver, and promoted-bishop fourfold cycles from remote
  PGNs; engine must return `(none)` after rule threshold.
- Grand: immediate en-passant after `j5`; normal capture prefixes that currently
  make replay switch to wrong side.
- Drops, promotions, castling, unloads, and multi-captures.

For every prefix:

1. Send protocol `position` command.
2. Run release `d` state verification.
3. Compare side, FEN, hash, legal move set, and formatted move round-trip.
4. Ensure `format_move` then `parse_move` returns same complete `Move`.

## Phase 2: Rebuild trustworthy baseline

- Preserve existing working-tree edits before Stage W.
- Build verified Stage U and Stage V reference binaries before Stage W, then
  record commit, config/dict/param hashes, compiler version, and binary MD5.
- Stage W is the first new commit after V and reverts only `ba65b18` LMR
  constants. All later stages build cumulatively on W.
- After Stage AB, rebuild every comparison binary with legality/dictionary
  fixes. Old Shogi and Grand standings are invalid and must not be mixed with
  new games.
- Keep Stage T parser/export layout fix. Never restore accidentally scrambled
  PST layout for strength.

Split Stage T experimentally without rewriting history:

1. Schema plumbing with numerically identical derived scalars.
2. Correct interleaved PST payload.
3. Regenerated parameter contents.

If corrected PSTs lose strength, improve/tune PSTs. Do not reintroduce malformed
serialization.

## Phase 3: Remove search machinery that does not earn keep

Use fixed Stage U baseline after Phase 1. Perform compile-time ablations in
worktrees; failed experiments leave no commit.

### 3.1 Bundle-first lean search

Build:

- Lean-1: remove G capture history, H singular/multicut, and I+L correction
  history.
- Lean-2: Lean-1 plus restore pre-C quiet LMR.
- Lean-3: Lean-2 plus remove B+J continuation history.

If a lean bundle is neutral or stronger, add back removed mechanisms one at a
time. This tests interactions while minimizing matches. Natural field removal:

- G removal deletes `capt_hist`, `capt_hist_div`, victim buckets, updates, and
  ordering offsets.
- H removal deletes `excluded`, `singular_margin`, singular probe, multicut, and
  negative extension.
- I+L removal deletes `corr_hist`, correction update/read, and pawn-hash coupling.
- B+J removal deletes `cont_hist`, continuation bases, and per-search allocation.

### 3.2 Secondary search ablations

Only after bundle result:

- D aspiration widening versus prior widening.
- E NMP strengthening versus simpler NMP.
- S extension cap on/off.
- U ProbCut on/off.

Keep A, F, K, M, N, Q, and R unless direct evidence contradicts them.

### 3.3 Clarify repetition semantics

- Keep configured repetition limit as sole game-over rule.
- Keep early search cycle handling only if named and measured as search
  heuristic, separate from rule adjudication.
- Compare current two-occurrence cycle score against configured-threshold search
  on Standard, Shogi, and Xiangqi. Remove hardcoded value if no strength need.

## Phase 4: Simplify static evaluation by evidence

No new leaf evaluator may walk compiled move vectors. Rejected real mobility
already cost about 55% FIDE time-to-depth. New terms must be precomputed,
incremental, or bitboard-based.

### 4.1 Measure each accumulated term

On lean search baseline, ablate independently:

- opening royal back-rank PST;
- pawn shield;
- castled bonus;
- castling-rights bonus;
- generic adjacent shelter;
- zone attack;
- open-shield penalty;
- major/minor imbalance;
- pair bonus;
- passed, connected, doubled, isolated, and backward advancing-piece terms.

Record eval time, node count, phase distribution, and RR result. Remove terms
that are neutral across variants or only duplicate another retained term.

### 4.2 Replace FIDE-shaped royal safety with rule-derived safety

Recommended retained core:

- adjacent friendly cover from `adjacency_mask` and `royal_list`;
- dynamic enemy pressure from existing `zone_attack`;
- multi-royal support unchanged.

Candidate replacement:

- Derive opening royal PST from expected enemy zone pressure at each square,
  using `derive_vector_chance`, `zone_attack`, and initial enemy army.
- Add no explicit castling knowledge. Castling helps only when destination has
  lower exposure and better cover.
- Delete `has_castled`, castled/rights scalars, `royal_shield_mask`,
  `royal_front_mask`, pawn-shield scalar, and open-shield scalar only after this
  candidate recovers O/P strength.
- Keep current explicit terms until replacement passes. O/P delivered large
  Standard gains; simplification cannot discard them on aesthetics alone.

### 4.3 Collapse pawn-specific structure into advancing-piece path quality

Current geometry is mostly generic, but `PAWN_MIN_START_COUNT = 5` excludes
valid sparse soldiers and terms still encode FIDE structure vocabulary.

Recommended model:

- Classify any non-royal irreversible short forward mover from actual move
  geometry; remove starting-count threshold. This includes minishogi and
  minixiangqi soldiers while still excluding backward movers and lances.
- Retain only four precomputed arrays:
  - forward path;
  - enemy interference sources;
  - friendly support sources;
  - promotion/advance gain.
- Score one opening and one endgame path-quality feature per advancing piece:
  progress value, reduced by own blocker/enemy interference, increased by
  support.
- Remove isolated-file offsets, backward mask, separate protected/chained passer
  tables, and five independent structure scalars if compact model passes.
- Preserve advancement table needed by dangerous-push search logic.

### 4.4 Do not add generic material battery bonuses

Queen/rook and queen/bishop ownership alone is not a battery. Actual battery
value depends on alignment and blockers. Walking pair geometry at every leaf
would repeat rejected mobility cost.

Existing quadratic `king_danger` already rewards cross-type attackers whose
move components converge on royal zone. Keep same-type color-bound pair bonus
only if its ablation is positive. Otherwise delete it rather than expand it.

### 4.5 Revisit phase only after evaluator is cheaper

- Factor current threshold calculation into one shared function used by fresh
  derive, parameter load, and tuning export.
- Preserve current numerical behavior first; do not silently fix denominator or
  switch to actual-army thresholds.
- After royal/pawn simplification, compare current thresholds, true mean of big
  pieces, and actual initial phase material.
- Require Shogi speed gate. Prior actual-army experiment raised Shogi time about
  90%, so no phase rewrite lands without new evidence.

## Phase 5: Shrink and make parameter schema truthful

Critical files:

- `src/game/representations/state.rs`
- `src/game/search/parameters.rs`
- `src/io/game_io.rs`
- `src/debug/tuning.rs`
- `src/prelude.rs`
- `res/param/*/latest.param`

After final eval term selection:

- Remove dead `mobility_opening` and `mobility_endgame` immediately from runtime,
  parser/exporter, and every param file.
- Remove every scalar and table whose term failed ablation.
- Keep schema linear and exact-length; regenerate shipped params atomically. No
  legacy parser.
- Separate derived eval tables from search-margin derivation. Keep
  `derive_search_parameters` search-only; rebuild eval-dependent tables after
  both fresh derivation and tuned-param load.
- Extend `TuneShape`, `initial_theta`, and `extract_sample` with sparse features
  for retained linear scalars. Current 19-token tail is serialized but Adam does
  not tune it; final schema must not call fields tunable unless they enter
  `theta`.
- Recompute phase thresholds and piece roles from tuned material through shared
  functions, then rebuild dependent tables. Measure role reclassification as a
  distinct candidate because it changes NMP and phase behavior.

Expected final scalar set: tempo, one or two royal-safety scales, compact
advancing-piece opening/endgame scales, and optional pair scale only if proven.
Do not retain 19 slots by default.

## Phase 6: Expand legality coverage

Existing perft suites cover 14 variants. Add trusted suites for 16 newer shared
variants: `ai-wok`, `almost`, `amazon`, `asean`, `chancellor`, `chigorin`,
`embassy`, `euroshogi`, `gothic`, `hoppelpoppel`, `janus`, `judkins`,
`knightmate`, `modern`, `newzealand`, and `pocketknight`.

Rules:

- Source counts from matching Fairy-Stockfish rules; never bless engine output as
  reference.
- Include start position plus at least two positions exercising special rules.
- Run whole large suites only at low depth. Use `perft <depth> <branch>` for
  deeper targeted checks; Standard suite has 6,752 positions.
- Every piece-move line remains annotated; no new CKN token without manual user
  confirmation.

## Phase 7: Verification gates

### Correctness gate for every behavior stage

- `cargo build --release`, no warnings.
- Release `d` state verification after every replayed move.
- Low-depth all-suite perft plus deeper selected branches.
- Protocol move round-trip for every legal move in replay corpus.
- Self-play walks for Standard, Crazyhouse, Shogi, Xiangqi, and Grand.
- Zero actual illegal move, illegal PV, or illegal ponder warning in at least
  10,000 post-fix games per affected variant before strength claims.

### Speed gate

- Verify candidate/baseline MD5 and embedded asset provenance first.
- Same compiler, CPU affinity, hash, threads, depth, and corpus.
- At least ten independent process repetitions.
- Compare aggregate geometric-mean time-to-depth, NPS, and nodes over
  multi-position Standard, Crazyhouse, Shogi, Xiangqi, and Grand suites.
- Performance-only stage needs at least 2% aggregate gain and no stable variant
  regression above 2%.

### Strength gate

- Candidate first in SPRT.
- Use `[0,5]` for expected gain and `[-5,0]` for non-regression.
- Run Standard first, then mandatory Shogi and Grand arbitration.
- Restart Shogi and Grand round robins from clean result directories after
  legality fixes. Do not merge old games.
- Continue Standard current RR only as historical evidence; rebuild fixed
  candidate pool for final verdict.
- Add Fairy-Stockfish 2000, 2100, 2200, and 2300 anchors once FSF-1900 is beaten.
  FSF-1900 alone cannot validate 2300 target.

Rollback rule: revert any stage failing correctness, stable speed limit, or
variant non-regression. For cumulative X/Y/Z cases, compare Z and Z-without-Y,
not only consecutive deltas.

## Phase 8: Datagen and tuning last

Preserve and finish existing headless work in `src/main.rs`,
`tools/datagen_local.sh`, `tools/merge_shards.py`, and
`tools/colab_datagen.ipynb` after schema settles.

- Use many single-thread CPU datagen shards; GPU gives no benefit to HCE.
- Merge with globally unique game IDs.
- Build game-disjoint train/validation datasets for Standard, Shogi, and Grand.
- Tune material, PST, and retained scalar features only.
- Export best validation epoch, rebuild embedded params, verify load/export
  round-trip, then SPRT tuned versus derived.
- Reject tuning that improves training only, raises multi-variant nodes by more
  than 5%, or regresses Shogi/Grand.

## Critical files

- `src/game/moves/move_list.rs`
- `src/game/search/transposition.rs`
- `src/game/position/search.rs`
- `src/game/position/evaluation.rs`
- `src/game/search/parameters.rs`
- `src/game/representations/state.rs`
- `src/io/move_io.rs`
- `src/io/protocols/protocol.rs`
- `src/io/game_io.rs`
- `src/debug/tuning.rs`
- `src/debug/console.rs`
- `build-stages.sh`
- `round-robin.sh`
- `configs/example.conf`
- `res/dicts/example.dict`
- `res/dicts/grand.dict`
- `res/param/*/latest.param`
- `res/perft/*.perft`
- `plans/04-search-speed-evaluation-tuning.md`
- `plans/05-colab-compute-datagen-tuning.md`

## Final expected shape

- Legal protocol state or no move; never silent replay drift.
- Stage W-and-later lineage, with Stage V LMR reverted to Stage U behavior.
- Leaner search with only empirically useful histories/pruning.
- Royal safety from cover and rule-derived pressure, not castling flags.
- Compact advancing-piece path evaluation, not many FIDE pawn labels.
- Small parameter tail whose entries are genuinely tuned.
- Cross-variant perft/replay coverage broad enough to make Shogi and Grand RR
  trustworthy.
- Strength work driven by legal multi-variant results, not noisy single-position
  benches or contaminated round robins.

---

## Execution Status

### Status

Completed locally on 2026-07-23 through Stage AC. Search and evaluation
simplification, broad perft expansion, tuning, and final five-variant strength
round-robin remain pending.

### Execution summary

Stages W-AC established a trustworthy legality baseline: Stage V's LMR
regression was reverted; transient move state, terminal output, transactional
replay, variable-width move translation, Crazyhouse UCI, replay fixtures, and
fallible protocol FEN parsing were completed. Release checks passed 26 fixtures,
all embedded config derivations, and 2,024 Fairy-Stockfish legality-walk plies
across all five target variants. Aggregate fixed-depth time improved about 4%
with flat NPS. No final strength claim exists because current remote Shogi and
Grand results predate these fixes and Crazyhouse/Xiangqi lack clean candidate
round robins.

Final committed lineage:

| Stage | Commit | Result |
|---|---|---|
| W | `0abfedf` | Restored Stage U quiet-LMR behavior after Stage V regressed. |
| X | `f359293` | Normalized en-passant and halfmove transitions. |
| Y | `8de8a5a` | Stopped move generation, PV walking, and output at terminal states. |
| Z | `58d8aaf` | Made protocol move replay transactional and fail closed. |
| AA | `2691745` | Generalized move-payload translation and added Crazyhouse UCI. |
| AB | `6f8777e` | Added external five-variant replay fixtures. |
| AC | `773d04e` | Made the single `parse_fen` API fallible for protocol input. |

Stages continue after V without rewriting A-V history. Rejected experiments do
not consume stage letters.

### Problems fixed

#### Stage W: Stage V quiet-LMR regression

Remote Standard results isolated a large Stage U-to-V loss. Stage W restored
only Stage U's quiet-LMR constants and quiescence reduction formula. No other
A-V history changed.

#### Stage X: stale transient move state

`make_move!` previously cleared en-passant and updated the halfmove clock in
only selected move branches. Castling could preserve a stale en-passant target,
which generated illegal moves such as `c5b6` after the remote Standard replay.

Stage X now computes one post-move en-passant state and one halfmove transition
for every non-null move. Castling and drops clear stale en-passant rights.
Captures, drops, and configured reset pieces reset the halfmove clock; other
moves increment it.

#### Stage Y: terminal search and stale output

Terminal rule states could still generate moves, extend PVs through TT entries,
and return a stale fallback move or ponder move. Shogi fourfold repetitions then
ended with a legal-looking repeated move reported as illegal by the referee.

Stage Y now:

- returns no generated moves when `state.game_over` is set;
- clears root PV buffers before every search;
- returns a null result immediately at a terminal root;
- stops PV walking immediately after the terminal move;
- emits `bestmove (none)` without ponder at terminal roots.

#### Stage Z: partial position replay

Protocol `position` handling previously mutated live state while replaying and
ignored both unmatched move text and a false `make_move!` result. Later search
could therefore run from a partially replayed position with the wrong side to
move.

Stage Z replays into `State::fork()`, commits only after every token parses and
passes `make_move!`, and records generic `position_valid` state. Invalid replay
causes later `go` to emit `bestmove (none)` until a valid position replaces it.
Protocol switching now aborts active search first. Debug-console move handling
also checks `make_move!`.

#### Stage AA: variable-width move translation

Dictionary capture/unload rules assumed every square had exactly two
characters. Grand rank-10 moves and 9-rank variants therefore leaked captured
square payloads, producing text such as `i5j4j5` instead of `i5j4`.

Stage AA replaced fixed-width payload rules with file-plus-multi-digit-rank
patterns, ordered payload removal before promotion and generic delimiter
removal, and added missing rules to Grand, Xiangqi, Minixiangqi, and the example
schema. New `crazyhouse.dict` supports bracket pockets, drops, promotions,
castling, and en-passant in Fairy-Stockfish UCI format.

#### Stage AB: external replay fixtures

`tools/replay_fixtures.py` drives UCI, USI, and UCCI processes. Each case sends
a complete `position`, runs release `d` verification, and performs a depth-one
search. Successful move replay also exercises `format_move` and `parse_move`,
because protocol parsing matches input against formatted generated moves.

Fixture coverage includes:

- Standard castling after the stale-en-passant prefix and rejection of `c5b6`;
- Crazyhouse captures, drops, bracket pockets, promotion, en-passant, castling;
- Grand normal capture, valid en-passant, rejection of `i5j4j5`;
- Xiangqi UCCI cannon capture/unload;
- Shogi king/gold, king/silver, and promoted-bishop fourfold cycles;
- malformed Standard, Crazyhouse, Shogi, and Xiangqi FEN plus recovery.

#### Stage AC: malformed protocol FEN

Release uses `panic = "abort"`, so `catch_unwind` cannot protect protocol input.
The earlier panic-based parser aborted the process on malformed rank counts,
pieces, en-passant payloads, hands, counters, and zero fullmove numbers.

Stage AC keeps one API:

```rust
pub fn parse_fen(
    state: &mut State,
    fen: &str,
    dict: Option<&Translator>,
) -> Result<(), String>
```

Protocol handling consumes the `Result` and rejects invalid positions without
committing scratch state. Trusted config, perft, tuning, and internal callers
unwrap at their call sites and retain fail-fast behavior. No duplicate
`try_parse_fen` wrapper remains.

### Verification results

#### Build and parser checks

- `cargo check`: passed without warnings.
- `cargo build --release`: passed without warnings.
- Exact final Stage AC binary: commit `773d04e`.
- Exact final Stage AC binary MD5: `b8bec5b0450d38fd5e6f2b9f7ccb20dd`.
- All embedded variant configs completed headless derivation.
- Adversarial review found and fixed the setup-phase minimum-field omission.
- Follow-up review found no remaining concrete malformed-FEN panic path.

#### Replay fixtures

All 26 fixtures passed in release mode across Standard, Crazyhouse, Shogi,
Xiangqi, and Grand. Malformed positions returned `bestmove (none)`, and a later
valid `position` recovered the same process.

#### Fairy-Stockfish legality walks

Five candidate-versus-Fairy-Stockfish walks ran for each target variant. Both
engines received every full move prefix, candidate release `d` verification ran
at every ply, and terminal status had to agree.

| Variant | Games | Verified plies | Terminal positions |
|---|---:|---:|---:|
| Standard | 5 | 466 | 2 |
| Crazyhouse | 5 | 240 | 5 |
| Shogi | 5 | 494 | 2 |
| Xiangqi | 5 | 504 | 3 |
| Grand | 5 | 320 | 5 |
| **Total** | **25** | **2,024** | **17** |

No illegal replay, process abort, or terminal mismatch occurred.

#### Multi-position speed gate

Baseline was Stage W `0abfedf` with Stage AA dictionary patch `2691745`, so all
five protocols could load while baseline search code remained Stage W. Candidate
was pre-API-cleanup Stage AC `73574d8`; final `773d04e` changes only parser API
shape and trusted call-site unwrapping, outside the search hot path.

Provenance:

- compiler: `rustc 1.97.0-nightly (e96c36b6f 2026-05-21)`;
- host: `aarch64-apple-darwin`;
- baseline MD5: `5a4d7c457432f14b94c4c168987192cc`;
- candidate MD5: `1bff18b25e54204203fffbb9a38755ff`;
- baseline and candidate target-asset SHA-256:
  `018657d67893988a388ebcd97650d80ecd7524f356151d04f231064620afd719`.

Method: ten independent processes, three positions per variant, fixed depth,
one thread, 64 MiB hash, cleared hash per position, and alternating binary
order. Ratios are candidate divided by baseline geometric means.

| Variant | Time | NPS | Nodes |
|---|---:|---:|---:|
| Standard | 0.7731 | 1.0343 | 0.7931 |
| Crazyhouse | 0.8543 | 0.9901 | 0.8437 |
| Shogi | 0.9690 | 0.9866 | 1.0057 |
| Xiangqi | 1.1874 | 0.9845 | 1.1574 |
| Grand | 1.0741 | 1.0059 | 1.0780 |
| **Aggregate** | **0.9602** | **1.0001** | **0.9656** |

Aggregate fixed-depth time improved about 4%, with flat aggregate NPS.
Xiangqi and Grand time increases track changed node counts from corrected rule
and terminal semantics; their NPS remained within about 1.6% of baseline.
These are correctness-induced tree changes, not a stable hot-path throughput
loss.

### Remote round-robin snapshot

Historical A-V round robins remain running and are not valid final gates for the
fixed engine.

- Standard: about 7,056 games per engine; Stage U `+81 +/- 7`, Stage V
  `+38 +/- 7`.
- Shogi: about 636 games per engine; results remain contaminated by pre-fix
  repetition/terminal failures.
- Grand: about 644 games per engine; results remain contaminated by pre-fix
  replay and move-translation failures.
- Crazyhouse and Xiangqi have no clean final candidate round-robin yet.

Stage AC was not deployed into those result directories. Old Shogi and Grand
results must not be merged with future fixed-engine games.

### Gates not yet complete

- Full embedded perft suites were not automated in this execution because
  `perft <depth> [branch]` remains an interactive debug-console command.
- The 10,000-game zero-illegal requirement per affected variant remains for the
  clean final match campaign.
- No per-stage SPRT ran, by design.
- No final strength claim is made from historical A-V results.

### Remaining execution plan

Strength Iteration 2 is defined in `plans/07-strength-iteration-2.md`. Plan 07
labels Stage AC `773d04e` as Phase A-2 and decomposes the proposed Stage AD
lean-search work into phases B-2 through J-2.

1. Use Stage AC `773d04e` as the fixed legality baseline.
2. Run Phase 3 lean-search bundle ablations in isolated experiment branches:
   remove G+H+I+L first, then test pre-C LMR and B+J continuation history.
3. Recheck D, E, S, and U only after the winning lean bundle is known.
4. Run Phase 4 evaluation ablations and replace FIDE-shaped royal/pawn terms
   only when multi-variant evidence preserves O/P strength.
5. Shrink the parameter schema after final evaluator terms are selected.
6. Add trusted perft coverage for newer variants and a noninteractive way to
   run the low-depth suite gate.
7. Finish datagen and tuning only after schema settles.
8. Build exact final binaries, record MD5 and embedded-asset hashes, then start
   clean result directories for Standard, Crazyhouse, Shogi, Xiangqi, and
   Grand.
9. Run one final five-variant round-robin/SPRT campaign. Add Fairy-Stockfish
   2000, 2100, 2200, and 2300 anchors after FSF-1900 is beaten.

### Working-tree preservation

Existing user work in `src/main.rs` and `tools/datagen_local.sh` remained outside
all Stage W-AC commits. Generated `res/param/*` and Python cache artifacts were
removed after verification.
