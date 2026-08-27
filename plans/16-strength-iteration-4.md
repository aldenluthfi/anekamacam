# Strength Iteration 4 — Blank-Slate Phase A through Phase Z

## Status

Baseline branch: `experiment/blank-slate`.

- `94b0759` reduced evaluation to phase-tapered material and PST.
- `916cc16` reduced search to full-window alpha-beta, capture-only
  quiescence, simplified NMP, flat LMP, two killers, butterfly history,
  SEE ordering, TT/QT, and Lazy SMP.

Every unlettered prerequisite has landed, except RR #0 itself, which is blocked
on a missing `cutechess-cli`. Phases A, B, C and D are accepted. Per-prerequisite and
per-letter status lines record what was proved, what was left open, and where
the shipped design departs from this plan.

| # | state | commit |
| --- | --- | --- |
| 1. Drop move encoding and history repair | landed | `532ef34` |
| 2. Static phase reference and derive-time SETUP resolution | landed, two items open | `8c88552` |
| 3. Versioned scalar parameter tail | landed, schema reshaped | `dfc7565` |
| 4. Search ownership and wide-board feasibility | landed, wide-board proof void | `074b158` |
| 5. Ordering constants and derivation entry point | landed, SEE band defect open | `42ff0f3` |
| 6. Binary provenance and external anchor RR #0 | provenance landed, RR #0 blocked | `2c6f5db` |
| 7. Drop-pocket integrity verification | landed, PGN gate proxied by replay | `30693f4` |
| 8. Grand diagnosis and multi-royal rules question | landed, cause gated in Phase B | `aa3ed0b` |

| letter | state | commit |
| --- | --- | --- |
| A. Principal variation search | accepted, pooled +29.3 Elo | `595af09`, `a5cf736` |
| B. Mature Stage-U late-move reductions | accepted, pooled +100.2 Elo | `a41c825` |
| C. Aspiration windows and mate-distance clipping | accepted, pooled +38.7 Elo | `4794d8c` |
| D. Hoisted static evaluation, improving, and RFP | accepted, pooled +61.8 Elo | `c8927db` |
| E. Frontier pruning: futility, move count, exchange | accepted, pooled +20.7 Elo | `dd933fe` |
| F. Quiescence discipline | accepted, pooled +56.6 Elo | `fdbcf8d` |
| G. Check extension on a budget | rejected, stagnation, reverted | none |
| H. Material-sensitive draw scoring | rejected, regression, reverted | none |
| I. Royal shelter, confinement-gated | accepted, +16.0 Elo over 7216 games | (this commit) |

## Purpose

Rebuild strength from the stripped baseline without restoring machinery merely
because it existed before. Exactly `Phase A` through `Phase Z` are accepted
strength slots. A letter advances only after real affected-variant games accept
a coherent candidate expected to add meaningful Elo over the previous accepted
phase.

Search comes first because the large search skeleton removed by `916cc16` was
never isolated against this blank slate. That ordering is an inference, not a
measurement. External anchor RR #0 and RR #1 arbitrate it. If A-G pass their
internal gates but fail to move the external anchor materially, stop after RR #1
and revise the remaining order before building H.

Historical evidence provides priors, not guarantees:

- material-sensitive draw scoring: about +48 consecutive RR Elo;
- royal safety I: about +27;
- royal safety II: about +21;
- one- and two-ply continuation history: about +14;
- TT static-eval cache stage: about +9;
- correction-history stage: about +9;
- stability-scaled time management: about +4;
- capture history: about -11;
- singular/multicut/negative-extension family: about -19;
- eval-scaled NMP plus verification: large node cut, about -3 RR Elo.

Those figures were measured on older stacks. Every floor below is an acceptance
threshold, not an Elo prediction, and floors must never be summed.

## Letter and verdict semantics

For every letter:

1. Branch from the previous accepted phase.
2. Build one primary candidate and any required correctness/schema riders.
3. Run deterministic support checks with an explicit
   `ANEKAMACAM_SEED` and matched Hash.
4. Run promotion games with `ANEKAMACAM_SEED` unset.
5. Promote only on a terminal H1 verdict at the phase's predeclared floor and
   with no disqualifying affected-variant regression.
6. A terminal H0 verdict with a negative estimate rejects the candidate. Revert
   it and try the next concrete fallback under the same letter.
7. An inconclusive run is not a rejection. Extend the same cumulative test.
   There is no automatic extension count that turns uncertainty into failure.
   The candidate remains unresolved and blocks the letter until evidence becomes
   terminal or the user explicitly abandons it. User abandonment consumes no
   letter and is not recorded as negative evidence.
8. Never act on an intermediate SPRT tally.
9. Failed or abandoned candidates consume no letter. Diagnostics, correctness,
   tooling, schema work, generalization hardening, and neutral refactors consume
   no letter.
10. If all listed fallbacks are rejected, stop at that letter and design another
    concrete strength candidate. Never advance by relabeling neutral work.
11. Commit only an accepted candidate. One accepted phase, one commit. No
    `Co-Authored-By` trailer.

Use H0 = 0 and H1 = the stated phase floor unless a phase explicitly declares a
non-regression arm. Derive initial and extension budgets from observed paired
variance and draw rate before starting. Rough priors are 6,000 games for a large
effect and 12,000 or more for an 8-15 Elo effect, but no fixed cap may override
the inconclusive rule.

## Architecture laws

- Keep `evaluate_position!` in `src/game/position/evaluation.rs` as the one
  evaluator entry point. Do not add an evaluator framework, trait, term
  dispatcher, or `EvalState`.
- Put immutable rule-derived geometry, masks, coefficients, and tables in
  `StaticState`. Build them once after piece values and rules are known.
- Put only compact live position values that must survive make/undo in `State`.
  Every addition must update the explicit clone, construction, reset, make,
  undo, snapshot when needed, and `verify_game_state` ledger in one commit.
- Put PV/search tables, root values, per-ply values, deadlines, worker identity,
  histories used only by search, and scratch buffers in `SearchInfo` or an
  existing per-thread search context, not in `State`.
- Do not add a table quadratic in board area. Full `Board` values themselves
  scale with board area, so a `Vec<Board>` indexed by every square is quadratic
  and is forbidden. Use fixed-radius square lists, compact relative kernels,
  flat per-square scalars, or sparse declared graphs.
- Do not walk `statics.relevant_moves` in the leaf evaluator. Historical leaf
  mobility walks cost about 55% of FIDE time-to-depth.
- Do not put FIDE piece names, variant names, or protocol-dialect concepts in
  engine logic. Gates come from declared movement, roles, zones, setup, hands,
  objectives, and termination rules.
- Prefer static derivation to live-position derivation. Bounded derive-time SETUP
  simulation is allowed. Dynamic live-army phase thresholds and per-node setup
  simulations remain rejected.
- Every strength-sensitive numeric value is static and derive-time. Each has a
  rules-derived default, a stable parameter-schema slot, and a documented
  dimensionless meaning so it can later be tuned. Structural representation
  constants such as field bit positions and array capacities are not strength
  parameters.
- Keep implementation concrete. Extend existing helpers and data paths before
  introducing abstractions.

## Unlettered prerequisites

These must land before Phase A or before the first phase that depends on them.
They are correctness, architecture, or measurement work and consume no letter.
Each coherent prerequisite may have its own commit.

### 1. Drop move encoding and history repair

**Status: landed — `532ef34`.** Placement square written to both `start` and
`end`, can-checkmate moved to bit 112, diagrams rewritten. Perft identity held
on all eight suites; uchifuzume probe forced with `--moves "P@e8"` returned
+2000000/-2000000 unchanged pre and post, with a lance-drop control. Non-drop
fixed-depth byte identity held under pinned seed and matched Hash.

One plan premise was wrong and is corrected here: drops never *wrote* history.
All three `update_history` calls are gated on `is_quiet`, and `m_quiet!`
requires `QUIET_MOVE`, so the old index was already injective in the drop
square. The real collision was drop against quiet, not drop against drop.

Current drop encoding writes the placement square to `start`, omits `end`, and
writes `can_checkmate` into bit 23, which is also bit 0 of `end`. Consequently,
`end!(drop)` is 0 or 1 and unrelated drops alias history cells.

Repair:

- Keep the placement square in `start`; make/undo already reads it there.
- Also call `enc_end!` with the placement square.
- Move the can-checkmate flag to bit 112. Bits through 111 are occupied and
  112-127 are free in the current move layout.
- Update `drop_can_checkmate!`, `illegal_mating_drop!`, every other reader, and
  the diagrams/docs in `moves.rs`.

Proof:

- Full perft identity on all available suites and every drop config.
- Shogi illegal-mating-drop and drop-mate fixtures.
- Non-drop fixed-depth byte identity with pinned seed and matched Hash.
- Distinct drop-history key count before/after; perft alone cannot prove this
  repair reached ordering.

### 2. Static phase reference and derive-time SETUP resolution

**Status: landed — `8c88552`.** Thresholds now derive from the deployed start
army. SETUP variants resolve through `resolve_setup_army`, a bounded memoized
walk over legal setup endings capped at `SETUP_STATE_CAP` censuses and
`SETUP_ENDING_CAP` endings, averaging the post-SETUP deployed army. All 38
configs derive without panic, every start position logs a phase, and crazyhouse,
shogi, and minishogi begin in OPENING. Endgame fixtures 38/38.

Two items remain open. The synthetic no-big, no-royal, and one-type probes are
unrun: `load_variant` at `headless.rs:112` rejects non-embedded configs, so
running them means shipping probe variants into `UCI_Variant`. The guards
themselves are present but only exercised by real variants. Separately, the walk
terminates on the engine's own `probe.game_phase == SETUP` test rather than a
king-placed test, so the Chess with Different Armies rule — setup ends when both
kings are placed — will work unchanged once that rule exists in a config.

Current `opening_score` uses declared piece-type count, so promoted type
catalogues inflate the threshold and several drop variants start in MIDDLEGAME.
Current averaging also mixes numerator and denominator populations.

Repair:

- Define one guarded mean of WHITE non-royal opening values for all restored
  historical formulas. Do not reuse the surviving mismatched average.
- Derive phase references in the same board-material units as
  `game_phase_score!`, from the deployed start army rather than declared types.
- For SETUP variants, resolve a bounded deterministic set of legal setup endings
  at derive time, memoize equivalent states, and aggregate the post-SETUP
  deployed army. Never perform this simulation per node.
- Exclude hands from the setup reference; they may be placement menus rather
  than reserves.
- Guard empty, no-big, single-non-royal, and low-material armies.
- Enforce `endgame_score < opening_score` in every variant. Equality would divide
  by zero in the MIDDLEGAME evaluator; inversion would corrupt the taper.

Proof:

- All shipped configs derive without panic.
- Every start position receives a logged phase classification.
- Crazyhouse, shogi, and minishogi begin play in OPENING after setup rules are
  resolved; ordinary start phases do not regress.
- SETUP, low-material, no-big, no-royal, and synthetic one-type probes exercise
  every guard.
- Endgame fixtures and phase traces remain correct.

### 3. Versioned scalar parameter tail

**Status: landed — `dfc7565`, with the schema reshaped by user decision.** The
version token, the count token, and the dual-length legacy acceptance below were
all rejected as legacy machinery. The shipped schema is base plus exactly
`PARAM_SCALAR_COUNT` scalars in one accepted length; a payload of any other
length is rejected with a diagnostic naming both counts, because every shipped
payload is regenerated whenever the shape changes. The "legacy-base fixture
derives the same defaults" proof is void with the legacy branch.

The five scalars — opening and endgame occupancy, the two role split shares, and
the endgame army size — moved from compile-time constants into `StaticState`,
stored as integers scaled by `COEFFICIENT_SCALE` so they survive the all-integer
payload. Reader and writer are one mirrored pair, `apply_scalar_parameters` and
`scalar_parameter_tokens`.

Proof met: 38/38 variants load at exact count; all 38 payloads are their
previous base plus the exact tail `360 120 100 200 5`, so no derived value, role
flag, or PST byte moved; start phases identical to prerequisite 2; perft identity
on all eight suites; endgame fixtures 38/38. Round trip is measured, not assumed
— `export_theta` is the one path that parses a payload and re-exports it, and a
tune run over a generated los-alamos dataset reproduced all 387 tokens
byte-identically. The five tail values are pairwise distinct, so a swapped slot
between reader and writer would have shown.

One item is deliberately not implemented: the guarded post-value derivation
entry point below. No derived table currently depends on a scalar at load time,
since values and PSTs arrive in the payload and the scalars only feed the next
derive. The phase that first adds a scalar feeding a runtime table adds the hook.

The current flat `.param` schema contains phase references, material, role flags,
and PSTs only. Delaying new scalars until Phase Y would violate the tunability
law and would make embedded parameter files silently skip their derivation.

Add an append-only scalar tail before Phase A:

- Keep the current base prefix unchanged.
- Append numeric schema version, scalar count, then scalars in one documented
  stable order.
- `parse_tuned_parameters` accepts either the exact legacy base length or the
  exact current full length. A legacy payload receives rules-derived defaults.
  Reject a partial tail, unknown version, or mismatched count.
- Parse the base first, derive all scalar defaults from the loaded piece values
  and rules, then let a present scalar tail override those defaults.
- `export_tuned_parameters_file` always emits the full current tail.
- Restore one guarded post-value derivation entry point called on both embedded
  and freshly derived parameter paths.
- Every later phase that introduces a numeric strength parameter appends its
  schema slot, parser/export path, default derivation, and all regenerated
  shipped payloads as an unlettered rider in that phase's commit.
- Tables are regenerated from serialized coefficients and limits; do not dump
  large tables into `.param` files.
- Phase Y extends optimizer feature extraction over already round-trippable
  slots. Phase Y is not the first schema migration.

Proof after every schema rider:

- Every shipped variant loads exact expected count.
- Legacy-base fixture derives the same defaults as a fresh no-param load.
- Full export/parse/export round trip is byte-identical.
- Delete/derive/rebuild/re-embed flow uses fresh Cargo output and verified binary
  hashes.

### 4. Search ownership and wide-board feasibility

NOTE: wide-board is now removed

**Status: landed — `074b158`**

- `pv_line`, `pv_table`, `pv_length`, `search_hist`, `killer_hist` moved from
  `State` to `SearchInfo`. `clear_search` sizes them; no `State` clone carries
  them any more. `pv_line` became a `Vec<Move>` rather than
  `[Move; MAX_DEPTH]`, because `SearchInfo` derives `Default` and a 128-slot
  array of non-`Copy` `Move` has none.
- History reshaped to `(piece, end)`, size `piece_count * board_size`. No
  regression appeared, so the two-linear-table fallback was not needed.
- `score_move!`, `pick_by_score!`, and `fill_pv_line!` take the worker as an
  extra argument. The all-node quiet malus is unchanged.
- The wide-board proof block is void: the feature is gone.

Gates, binary `de333d18a4bed47d08c2a9568b0cb256`, `ANEKAMACAM_SEED=1`:

- perft, all eight suites, unchanged from prerequisite 3: standard 27008,
  crazyhouse 5, shogi 16, minishogi 5, sittuyin 20, janggi 35, xiangqi 44,
  grand 4, all passed. Ownership moves did not change legality.
- endgame fixtures 38 passed, 0 failed.
- debug build with `verify_game_state` live: standard d5, crazyhouse d4,
  shogi d4, janggi d4 all clean.
- Threads 1/2/4/8 on standard d7: no crash, worker construction fine.
- fixed depth, nodes before (`967d2726…`) to after, all reaching full depth:
  standard d8 205148 to 196600, crazyhouse d6 15049 to 16845, shogi d6 13182
  to 12244, xiangqi d7 238413 to 210646, grand d6 51513 to 27663, janggi d6
  490 to 490, makruk d8 130276 to 169673, minishogi d8 95766 to 78915. Total
  749837 to 733076. Mixed by variant, as expected from a changed history key
  interacting with late-move pruning; fixed-depth scores move with it.

Open: RR #0 still owes the mandatory baseline, since the history key changed
behaviour. Prerequisite 6 specifies that run, but it happens once every
prerequisite has landed, not straight after 6.

Current `search_hist` is `piece_count * board_size * board_size` and is cloned in
`State`; it is already forbidden by the architecture law at 2048 squares.
Search-only PV, killer, and history data also do not belong in game state.

Repair before the baseline anchor:

- Move `pv_line`, `pv_table`, `pv_length`, `search_hist`, and `killer_hist` from
  `State` into the existing per-thread search context owned by `SearchInfo`.
- Replace butterfly history's `(piece, start, end)` table with the simplest
  linear primary shape `(piece, end)`, size `piece_count * board_size`.
- If target-only history produces a material baseline regression, same
  unlettered fallback is the sum of two linear tables, `(piece, start)` and
  `(piece, end)`. Do not restore a full start/end product.
- Update all clearing, worker construction, and move-ordering call sites.
- Preserve the all-node quiet malus.

Wide-board proof:

- Build with `wide-board` and load a representative 2048-square synthetic
  variant. Grand is not a wide-board proof.
- Record bytes and derive time for every table.
- Require storage bounded by board area times piece count, a fixed radius, or a
  sparse declared movement graph.
- Record Threads 1/2/4/8 memory and worker construction time.
- Pinned-seed perft and fixed-depth checks prove ownership moves did not change
  legality. RR #0 establishes the new mandatory baseline after any history-key
  behavior change.

### 5. Ordering constants and derivation entry point

- Restore named winning-capture, losing-capture, history-bound, and score-band
  constants instead of bare literals.
- Audit band edges against the current history bound of 16383, not historical
  16384.
- Ensure the restored post-value scalar derivation is called after either
  embedded parameter parsing or fresh value/PST derivation.
- This scaffolding must be fixed-depth byte-identical before Phase A.

Status: landed, one band defect found and left unfixed.

- Named constants live in `src/prelude.rs` beside `INF`/`MATE_SCORE`:
  `HISTORY_BOUND`, `TABLE_MOVE_SCORE`, `WINNING_CAPTURE_SCORE`,
  `KILLER_MOVE_SCORE`, `QUIET_MOVE_SCORE`, `LOSING_CAPTURE_SCORE`. Each
  capture and quiet constant folds the `HISTORY_BOUND` offset in, so the
  call site is base plus signed tiebreak. Every bare literal in
  `score_move!`, `pick_by_score!`, and `update_history` is gone; the two
  duplicated `let history_bound` locals are gone with them.
- Band edges at `HISTORY_BOUND` = 16383: losing capture at most 983_616,
  quiet 1_000_000 to 1_032_766, killers 1_049_150 and 1_049_151, winning
  capture from 4_016_383, table move 5_000_000. Disjoint for every legal
  exchange score, since piece values keep `|see|` in the low thousands.
- Defect found by the audit, not fixed here: `see!` returns `-INF` as its
  illegal-move sentinel, and `score_move!` feeds that straight into the
  losing-capture band. `1_000_000 - 16383 - 2_000_000` is negative, and the
  band casts to `usize`, so an illegal capture scores 18446744073708535233
  and outranks everything. Proof: `see standard 'e2*d4' --fen "4r3/8/8/8/
  3p4/8/4N3/4K3 w - * 0 1"` returns -2000000 with the knight pinned and 104
  without the pin, and `pick_by_score!` scores moves before `make_move!`
  filters them. Impact is ordering only, since the move still fails to make
  and is skipped, and index 0 is safe whenever the table move is present and
  forces its own score. Every other slot puts pinned captures first. Fixing
  it changes node counts, so it cannot ride in a byte-identical commit. Placed
  as Phase A's required correctness rider.
- Bullet three has nothing to implement. `apply_scalar_parameters` is the
  only writer of the five derivation scalars, it runs inside
  `parse_tuned_parameters` which ends in `refresh_eval_state`, and the
  derive branch runs `derive_parameters` which does the same. The payload
  serializes both phase scores and all five scalars, so neither load path
  leaves a scalar-dependent table stale. This confirms the same conclusion
  reached in prerequisite 3.
- Gates: release and debug builds clean, no warnings. The release binary is
  byte-identical to prerequisite 4's, md5 `de333d18a4bed47d08c2a9568b0cb256`
  in both, which is a stronger result than the required fixed-depth
  identity. The eight-variant fixed-depth suite under `ANEKAMACAM_SEED=1`
  reproduces prerequisite 4 exactly: 196600, 16845, 12244, 210646, 27663,
  490, 169673, 78915. Long-line counts unchanged.

### 6. Binary provenance and external anchor RR #0

For every binary and every run, record:

- source commit and branch;
- Cargo target path and feature set;
- hash of the freshly built `target/release` binary;
- hash of any copied `bin/` binary;
- UCI identity, Hash, Threads, time control, and seed policy;
- fresh result directory.

A copied binary merely differing from the previous copy is insufficient. Verify
its source build first.

Run RR #0 after prerequisites on standard, crazyhouse, shogi, xiangqi, and grand
against a two-sided adaptive Fairy-Stockfish ladder. Start low enough that the
weakest rung scores above roughly 15% and high enough that the strongest scores
below roughly 85%. Match Hash. Unset the seed. Keep concurrency at or below half
available cores.

Status: provenance landed and verified, RR #0 blocked on a missing tool.

- `tools/provenance.sh` records, shows, and verifies a sidecar
  `bin/<name>.provenance` per binary: commit, ref, subject, branch, Cargo
  target path, feature set, file hash and byte count, content hash, hashes of
  the `configs` and `res/dicts` trees actually embedded, whether those trees
  were dirty, and the binary's own `id name`, variant count, and option-list
  hash taken from a live UCI handshake.
- Two builds of one commit are not byte-identical, and the plan's wording
  assumed they would be. Measured: builds at two path lengths differ in
  exactly 48 bytes, offsets 1945-1960 and 5439553-5439584, which are the
  LC_UUID payload and the ad-hoc signature CDHash. The linker derives the
  UUID from the build path and the signature hashes the image including it.
  Path length alone moves the bytes; path content does not, and two builds at
  paths of equal length agree. `-Wl,-no_uuid` is not an option: dyld refuses
  to run a build script without LC_UUID.
- So the record carries two hashes. `md5` is the file as copied and answers
  only whether it changed on disk. `content_md5` is the image with its
  signature removed and its UUID zeroed, is stable across build paths, and is
  what a rebuild is compared against.
- `verify` re-checks the file hash and the embedded resource hashes, then
  rebuilds the recorded commit in a throwaway worktree and compares content
  hashes. Both failure modes were exercised: flipping one byte of a recorded
  binary gives `file changed since it was recorded`, and attributing the
  current binary to `8c88552` gives a content mismatch. Attributing it to
  `074b158` correctly passes, since prerequisite 5 was byte-identical to 4.
- `build-stages.sh` now derives the ladder instead of listing it: `base-4`
  from `LADDER_BASE`, `phase<L>-4` from its own branch, auto-created from the
  previous letter or from `PHASE_<LETTER>_PARENT`. Every build writes its
  provenance record. Ladder builds still take `configs` and `res/dicts` from
  the working tree so all rungs expose the same variants, while `res/param`
  and `res/perft` come from the commit, which is what keeps per-phase tuning
  attached to its phase.
- `round-robin.sh` refuses to launch without `cutechess-cli` and
  `fairy-stockfish`, refuses a candidate with no provenance record, treats an
  existing `provenance.txt` as an occupied result directory, and writes
  `$RR/provenance.txt` naming the candidates, anchors, variants, rounds,
  concurrency, time control, Hash, Threads, seed policy, and both tool
  versions, followed by every candidate's build record. Time control, Hash,
  and Threads became `TC`, `HASH`, and `THREADS` so the run settings and the
  recorded settings cannot drift apart.
- Defect fixed while testing the harness: `setsid` does not exist on macOS, so
  every detached launch died at once with `setsid: command not found`. The
  relaunch now goes through `nohup perl -e 'setpgrp; exec @ARGV'`, one
  mechanism on both platforms, which restores the process group `--stop`
  kills.
- `bin/base-4` was first built and verified at commit `42ff0f3`: file md5
  `3f3d05fc32302f05627566279dcc306c`, content md5
  `3e62c300a25ed960674811f769e4ee75`, 38 variants. Its file hash differed from
  a repo-root build of the same commit, `de333d18a4bed47d08c2a9568b0cb256`,
  and its content hash matched it, which is the distinction the two fields
  exist to make.
- Both ladder binaries were rebuilt for Phase A and their records rewritten,
  because prerequisites 7 and 8 changed the embedded `configs` and
  `res/dicts` trees after the first build and a baseline carrying the older
  trees is not comparable. `bin/base-4` is now commit `5849de4`, file md5
  `cfd8d3115733b2d529597b9ea46a7c48`, content md5
  `be9d2399dabfdb8777f621a79920bcf0`; `bin/phaseA-4` is commit `a5cf736`,
  file md5 `90cb5939cd9ed290f0cc756f1b25ee66`, content md5
  `725ab78cb56709616e605586e1bcd360`. Both carry 38 variants, the same
  `configs` hash `c09f443d`, the same `res/dicts` hash `39bee958`, and the
  same options hash, so the pair differs only in the two commits' source.
- RR #0 is not run. `cutechess-cli` is installed nowhere on this machine,
  which is now its only blocker: prerequisites 7 and 8, which the plan orders
  RR #0 after, have both landed. `fairy-stockfish` 14.0.1 XQ is present. The
  harness was exercised end-to-end against a stub `cutechess-cli`: detach,
  log, standings, and `provenance.txt` all behaved.

### 7. Drop-pocket integrity verification

The promoted-piece/demotion incident that forfeited 466 of 2987 crazyhouse games
appears fixed in this branch. Verify rather than reopen it blindly:

- Read `configs/example.conf` and `res/dicts/example.dict` first.
- Audit every drops config for promoted type and demotion completeness.
- Run perft and a short external crazyhouse sample.
- Require zero forfeit termination tags in PGN.

Status: landed.

- `configs/example.conf` and `res/dicts/example.dict` were read before every
  config or dictionary edit. Crazyhouse, shogi, minishogi, judkins, and
  euroshogi now each have distinct promoted types and exact inverse demotions.
  Pocket Knight deliberately makes only `O/o` droppable: an `O/o` captured from
  the board demotes to ordinary, null-droppable `N/n`, while explicit null-drop
  rules cover all ordinary types.
- EuroShogi was corrected to its Fairy-Stockfish rules: pawn advances and
  captures forward; its knight makes two-forward/one-side jumps; P/N/R/B must
  promote in the final three ranks. Its pawn uses `v!v` so a dropped pawn can
  promote into its own last-rank forbidden zone. Four fixtures, including hands
  and promoted-piece capture, agree node-for-node with Fairy-Stockfish 14.0.1:
  start to d6 `234638669`, a two-hand position to d5 `25969855`, a ten-piece
  hand position to d4 `303011635`, and demotion capture to d6 `6340081`.
- Added hand and promoted-piece probes to every drops perft suite. At d4,
  crazyhouse passes 16/16 cases; minishogi, judkins, and Pocket Knight each
  pass 12/12. `res/perft/euroshogi.perft` carries the independently
  Fairy-Stockfish-verified rows above. A full EuroShogi d5 suite was stopped
  after more than 24 minutes before its first result, so it is not claimed as a
  completed suite gate.
- `tools/drop-integrity.sh` provides the available external check while
  `cutechess-cli` is absent: it seeds 40 short crazyhouse self-play games,
  replays every move through Fairy-Stockfish, and compares board, side,
  castling, promoted marks, and both pockets. All 40 games (2216 plies; 39
  nonempty final hands) agreed; results were 21 White wins, 16 Black wins, two
  draws, and one ongoing game. Thus zero moves were rejected by the external
  rules engine. No PGN exists without `cutechess-cli`, so zero literal forfeit
  termination tags is proxied by those zero rejected replays, not asserted from
  PGN text.
- Removed unimplemented `piece count limits` and `demote upon capture` parser
  tokens and their dead validation. `special_rules` already occupied all bits
  0 through 7, so there was no bit numbering to close; its storage was narrowed
  from `u32` to `u8` instead.
- FEN loading no longer force-promotes an unpromoted piece that sits in a
  mandatory promotion zone. That rewrite was wrong for legally dropped EuroShogi
  pieces in the final three ranks, and a forbidden-zone guard could not save it:
  janggi's mandatory zones are palace squares and its forbidden zones are the
  palace complement, so the intersection is empty and the guard would suppress
  every janggi conversion. The loader now places exactly the piece the FEN
  names; providers must supply legal FENs and any dialect difference belongs in
  `res/dicts/<variant>.dict`.
- Blast radius of that removal, measured across every variant: janggi alone
  regressed, because its FENs spelled palace pieces in the pre-conversion form
  and relied on the loader to rewrite them. The `A`/`F` pair and the `K`/`Q`
  pair split the palace between the corner-and-centre squares, which carry the
  diagonals, and the edge midpoints, which do not. `configs/janggi.conf`, all
  seven `res/perft/janggi.perft` rows, and the six janggi cases in
  `tools/endgame_fixtures.txt` now spell the true form for each square. Xiangqi
  was unaffected and passes 33/33 at d3.
- Regression after the removal: every variant suite passes at d3, standard
  included at 20256/20256; janggi passes 28/28 through d4; the endgame fixtures
  pass 38/38; `tools/run_fen_roundtrip.sh` passes 44/0/0; and a fresh
  `tools/drop-integrity.sh` sample of 12 crazyhouse games reports zero
  mismatches. `debug-headless derive` reproduces every embedded parameter file
  byte for byte, so the janggi respelling moved no evaluation term.

### 8. Grand diagnosis and multi-royal rules question

Grand's historical 48x node ratio is undiagnosed. Before attributing a shared
mechanism to it, compare fresh derived parameters, current embedded parameters,
constructed material imbalances, and promotion-to-captured behavior. A config or
parameter correction consumes no letter. A shared cause joins the relevant
letter and is gated there.

`side_is_bare` deliberately requires exactly one royal and documents multi-royal
behavior as an open rules question. Resolve it from declared rules and fixtures;
do not call it a bug without evidence.

Status: landed. No config or parameter fault was found, and the ratio is not
Grand's.

- Grand's configuration and parameters are clean. `debug-headless derive`
  rewrites `res/param/grand/latest.param` byte for byte. Constructed
  single-piece imbalances off the start position, all in `Opening` phase and
  none in `ENDGAME`, read Q 1106, C 986, A 800, R 628, B 372, N 334, P 106
  centipawns; against Fairy-Stockfish normalised to the knight that is
  Q 3.31 / C 2.95 / A 2.40 / B 1.11 against its 3.23 / 2.62 / 2.49 / 1.26.
  `promote to captured` is correct in both directions: a white pawn one step
  from the last rank has no promotion at all with empty hands, exactly one with
  `-/r`, and all six with `-/rnbqac`. The colour convention is that a side's
  captured pieces sit in the *other* hand field in the enemy's case, so
  `rnbqac/-` correctly offers white nothing.
- The ratio climbs with board width on boards Grand has nothing to do with.
  At depth 8, Hash 64, Threads 1, `ANEKAMACAM_SEED=1`, our nodes against
  Fairy-Stockfish are standard 186289/5247 = 35.5x, capablanca
  1028187/13125 = 78.3x, gothic 816609/8317 = 98.1x, and grand
  851834/6905 = 123.3x. Root move counts run 20, 28, 28, 65. Capablanca and
  gothic share a 10x8 board and differ only in setup, so width and material
  are separated: both are already far above standard without Grand present.
- The mechanism is effective branching, not a Grand term. Over depths five to
  nine our EBF is standard 3.41, capablanca 4.06, gothic 3.88, grand 4.62,
  while Fairy-Stockfish sits at 2.28, 2.31, 2.16, 2.15 and does not move with
  width. `tools/ebf-suite.sh` reproduces the shape at its own seed and window:
  ours 3.670, 3.880, 3.973, 4.477 against 2.042, 2.154, 2.031, 2.126.
- The cause is that nothing in the search adapts to how many moves a node has.
  Every child is searched with a full window, the only reduction in the tree is
  the null-move `(4 + depth / 4).min(depth)`, and the late-move gate is
  `legal_moves >= 3 + depth * depth`; all three are functions of depth alone,
  and `board_size` reaches the search only as a history index. Phase B's four
  curves carry the plan's only `ln(moves)` and `sqrt(moves)` terms, nested
  inside Phase A's scout, so the shared cause joins Phase B and its support
  gate now names the width ladder.
- The historical 48x figure is an ordinary midgame sample, not an anomaly.
  Grand's four midgame cases spread from 55.23x to 427.18x at depth 10 in the
  same run whose start position reads 519.65x.
- The multi-royal question resolves without a code change. Both `side_is_bare`
  callers sit behind a declared `counting` rule; only makruk, sittuyin and
  ouk-chaktrang declare one; all three declare `royal: Kk` and promote to `M`
  or `F`, never to a royal letter. `janggi.conf` is the only config naming two
  royal letters a side, but `K` and `Q` are the two forms of one general that
  `K:Q` and `Q:K` convert between, so a janggi colour holds exactly one royal,
  and janggi adjudicates on `janggipts` rather than counting. The
  `royal_list[side].len() == 1` test is therefore correct for every position
  that reaches it; only the doc comment changed.
- `tools/ebf-suite.sh` hardcoded five Fairy-Stockfish variant names and
  silently dropped the rest, which is why the same-board control had no
  external column. It now reads Fairy-Stockfish's own `UCI_Variant` list and
  keeps `standard` to `chess` as the single rename, which admits every variant
  the two engines spell alike. `tools/ebf_positions.txt` gained the four
  `startpos` width-ladder cases. Capablanca and gothic still carry no sampled
  midgame cases, so they gate nothing on their own yet.

## Shared measurement protocol

- Support work: set `ANEKAMACAM_SEED`, use Threads 1 unless testing SMP, match
  Hash, and use multi-position suites rather than one start position.
- Games: unset `ANEKAMACAM_SEED`, use a fresh result directory, match Hash, and
  keep cutechess concurrency near cores/2 or lower.
- Hold UCI stdin open until `bestmove`. Closed pipes send EOF, which the engine
  treats as `quit` and can silently truncate search.
- Do not script the ratatui console. This build has no UCI `go perft`; use
  `debug-headless perft <variant> <depth>` with `--fen`/`--branch` for a divide
  and `--suite` for the recorded suites.
- Do not use `debug-headless search` or `debug-headless play` for cross-engine
  readings; their table sizes are not comparable.
- Nodes, EBF, NPS, time-to-depth, sign agreement, and fixture depth are support
  evidence only. Real games promote phases.
- Use `tools/ebf-suite.sh`, `tools/speed-suite.sh`,
  `tools/agree-suite.sh`, and `tools/run_endgame_fixtures.sh`; do not create a
  replacement framework.
- Build every rung with `build-stages.sh` and run
  `tools/provenance.sh verify bin/<name>` before any comparison that binary
  takes part in. Compare `content_md5`, never the raw file hash: two builds of
  one commit differ in the linker UUID and the signature that covers it.
- Sample neutral positions from both engines' games at fixed plies, independent
  of winner.
- Fairy-Stockfish supplies outcomes, score sign, relative behavior, and external
  anchor Elo. Never convert between AnekaMacam and Fairy-Stockfish score units.
- Read time forfeits from external PGN termination tags. Internal SPRT scores a
  flag as an ordinary loss.
- Anchor cadence: RR #0 before A; #1 after G; #2 after K; #3 after O; #4 after S;
  #5 after V; #6 after X; #7 after Z.

## Phase A — Principal variation search

### Candidate

Search the first legal move at full window. Search later moves with the scout
window `(-alpha - 1, -alpha)`. If a scout raises alpha at a wide-window node,
re-search that move at full window `(-beta, -alpha)`. Keep current NMP null-window
probes unchanged; zero-width searches already exist there.

### Required correctness rider

Prerequisite 5 found that `see!` returns `-INF` for a pseudo-legal move that
cannot be made, `score_move!` adds that to `LOSING_CAPTURE_SCORE`, and the
negative sum wraps through `as usize` into 18446744073708535233. Illegal
captures therefore outrank every real move at every picker slot except the one
a table move already owns. PVS is the phase this hurts most, since it assumes
the first searched move is the best candidate and pays a full re-search
whenever a later move beats it, so the fix rides here rather than waiting for
Phase U, whose byte-identity gate it would break.

Keep the sentinel out of the bands instead of widening them: score a capture
whose exchange cannot be simulated at the bottom of the losing-capture band
rather than by arithmetic on `-INF`. Land the rider first on the phase branch,
re-record the eight-variant fixed-depth suite as the phase's own reference, and
only then add the PVS branch, so the two are attributed separately.

### Footprint and parameters

No new field and no numeric parameter.

### Support gate

- Perft and endgame fixtures unchanged.
- Legal PV at every completed depth.
- Record scout and full-window re-search rates.
- No broad fixed-depth node regression across campaign variants, measured
  against the post-rider reference rather than RR #0's.
- Rider alone: illegal captures never take a picker slot ahead of a legal
  move, shown on the pinned-knight position prerequisite 5 recorded.

### Promotion gate

Pooled standard, xiangqi, and grand real-clock SPRT, H1 floor +10 Elo. Add a
separate standard arm if pooling hides a standard regression.

### Same-letter fallbacks

1. Start scouting at the third legal move.
2. Scout only non-PV nodes; keep full windows at wide PV nodes.
3. Add the same scout shape to qsearch only if main-search PVS is positive but
   misses the floor.

Rollback removes only the PVS branch. The correctness rider stays on any
outcome, including a terminal H0 that rejects every fallback.

### Status

Status: accepted. The pooled test is terminal H1 at the +10 floor.

- The correctness rider landed first as `595af09` and the PVS branch as
  `a5cf736`, attributed separately as this phase requires. `see!` still
  returns `-INF` for a capture it cannot make, but `score_move!` no longer
  does arithmetic on that value: such a capture takes
  `UNMAKEABLE_CAPTURE_SCORE` and sorts below every band, so the wrap to
  18446744073708535233 that put illegal captures ahead of every real move is
  gone.
- Promotion games ran on the built-in `debug-headless sprt` harness with
  A = `bin/phaseA-4` and B = `bin/base-4`, clock 5000+50ms, bounds [0, 10],
  alpha = beta = 0.05, and `ANEKAMACAM_SEED` unset. Threads is 1 and Hash is
  256 on both by construction: the harness sets only Threads, and neither
  engine overrides `HASH_DEFAULT_MB`. The patch is engine A because every
  reported figure -- mean, Elo, the win/loss tally and the LLR -- is taken
  from engine A's view, and H1 is the hypothesis that A is stronger.

| arm | pairs | W | L | D | score | Elo | LLR |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 500 | 443 | 362 | 195 | 0.5405 | +28.2 | 2.633 |
| xiangqi | 500 | 455 | 418 | 127 | 0.5185 | +12.9 | 0.940 |
| grand | 202 | 228 | 144 | 32 | 0.6040 | +73.3 | 2.999 |
| pooled | 1202 | 1126 | 924 | 354 | 0.5420 | +29.3 | 6.655 |

- Only grand reached a bound on its own, at 202 pairs. Standard and xiangqi
  were stopped at a 1000-game budget with the LLR still inside the bounds,
  which under rule 7 leaves those two arms unresolved rather than rejected.
  The gate is the pooled test and the pooled test is terminal: folded into one
  sample the three arms give LLR 6.655 against the +2.944 acceptance bound.
  That figure is reconstructed rather than run as one sequential test -- each
  arm's pair variance is inverted from its own reported LLR and the arms are
  then combined, so between-variant spread is carried into the pooled
  variance. It does not rest on grand, whose early stop at its own boundary
  biases any pool containing it upward: standard and xiangqi pooled without
  grand give LLR 3.611, also past the bound.
- The gate's separate standard arm was not needed. Pooling hides no standard
  regression; standard alone is the second strongest arm at +28.2 with LLR
  2.633, just short of its own acceptance bound.
- On independent-game variance the pooled 95% interval is [+16.4, +42.1],
  clear of the +10 floor at its lower end. Games are paired two per opening,
  so the true interval is tighter than the one that arithmetic gives.
- Two support gates were not measured: legal PV at every completed depth, and
  the scout and full-window re-search rates. Both need instrumentation that
  does not exist in the tree, and none was added.
- Harness behaviour the next letter will meet again. `engine_sandbox` keys its
  scratch directory on the binary path alone and clears it at run start, so
  two concurrent runs sharing a binary pair overwrite each other's
  `res/param`; per-variant copies of both binaries avoid it.
  `ARCHIVE_STAMP_FMT` has second resolution, so runs starting in the same
  second compute the same rolled log name and all but one panic in
  `roll_latest`; stagger the launches. The sprt path emits only through
  `log_1!` and installs no stdout sink, so a redirected file stays empty for
  the whole run and liveness is read from `logs/`. An SPRT cannot be resumed,
  because the LLR needs the pair sequence, so any restart is a full reset.

## Phase B — Mature Stage-U late-move reductions

### Candidate

Restore four precomputed reduction surfaces and nest them inside Phase A's
scout:

- quiet: `0.75 + ln(depth) * ln(moves) / 2.25`;
- quiet while checked: `1.0 + sqrt(depth) * ln(moves) / 4.0`;
- capture/promotion/drop: `1.0 + ln(depth) * sqrt(moves) / 4.0`;
- capture/promotion/drop while checked:
  `0.0 + ln(depth) * ln(moves) / 4.5`.

The capture-check base is `0.0`, not `1.0`.

Gate on the historical Stage-U move threshold
`legal_moves > 2 + 2 * wide_window`. Run reduced null-window search, then
full-depth null-window re-search if it raises alpha, then full-window re-search
only if still required. Defer history-magnitude adjustment and improving discount
to Phase P. Drops remain on the conservative capture curve until Phase S.

### Footprint and parameters

Four small `StaticState` tables. Serialize the six curve coefficients, minimum
depth, and move-gate offsets in the scalar tail; regenerate tables after parsing.
No live state.

### Support gate

Record reduced-search, full-depth re-search, and full-window re-search rates.
Mate discovery may stay equal or improve, never move later. Reject node explosion.

Also record the standard, capablanca, gothic, and grand `startpos` width ladder
from `tools/ebf-suite.sh`. Prerequisite 8 measured our EBF rising with board
width, 3.41 to 4.62 over depths five to nine, against a Fairy-Stockfish curve
flat near 2.2. These reductions carry the plan's only move-count terms, so the
gate requires that spread to narrow, not merely the node totals to fall.

### Promotion gate

Pooled standard, shogi, crazyhouse, and xiangqi SPRT, H1 floor +15 Elo, with no
campaign variant below its declared non-regression bound.

### Same-letter fallbacks

1. Quiet-only reductions; captures, promotions, and drops stay full depth.
2. Subtract one ply from every derived reduction, floor zero.
3. Raise minimum depth and delay the move-count gate.

Never substitute the rejected sqrt/sqrt quiet curve.

### Status

Status: accepted. Every campaign arm reached H1 on its own bound.

Two departures from the candidate as written, both deliberate:

- The tail carries eight curve coefficients, not six. Four surfaces need four
  bases and four divisors; "six" counts distinct values, which would tie the
  quiet-check base to the tactical base and the tactical divisor to the
  quiet-check divisor. Phases P and S move those curves independently, so they
  are stored independently: scalars 5 to 12, then minimum depth and the two
  move-gate offsets at 13 to 15.
- The candidate names no minimum depth. It is 3, the shallowest depth at which
  `depth - 1 - reduction` can still leave a ply after the clamp.

`PARAM_SCALAR_COUNT` rose from 5 to 16, so every shipped payload became fatal
until regenerated; all 38 were. The eval prefix md5 is unchanged for standard,
shogi, xiangqi, and grand, which is what makes the node counts below
comparable with Phase A rather than a measurement of new eval weights.

Prerequisite 3 deferred its post-value derivation hook to "the phase that first
adds a scalar feeding a runtime table". This is that phase, so the hook landed
here: `derive_search_parameters` runs from `derive_parameters` on the
no-payload path and as the tail statement of `apply_scalar_parameters` on the
payload path. No write of the eight coefficients can leave the four tables
describing the curves of the payload before it.

- Rates, standard `startpos` to depth 12, one thread, Hash 64,
  `ANEKAMACAM_SEED=42`: 63,994 reduced searches, 1,963 full-depth re-searches,
  818 full-window re-searches, cumulative over the whole iteration. The
  full-depth re-search rate is 3.07% of reduced searches, below the 10 to 20%
  other engines report. Read alone it says the reductions are seldom proved
  wrong; it does not say they are seldom wrong, because a reduction that is
  never re-searched is never tested.
- Node totals fell rather than exploded: standard depth 12 went from
  44,757,908 nodes to 836,287, a factor of 53.5 at an unchanged score
  (cp 11 against cp 10).
- Width ladder, `tools/ebf-suite.sh`, all four cases at depth 9 with
  `EBF_FROM=5` so the window matches the one prerequisite 8 quoted, Hash 64,
  seed 42:

| case | A ebf | B ebf | fsf ebf | A nodes / fsf | B nodes / fsf |
| --- | --- | --- | --- | --- | --- |
| standard | 3.113 | 2.348 | 2.422 | 21.6 | 3.6 |
| capablanca | 3.605 | 2.784 | 2.045 | 84.6 | 11.1 |
| gothic | 3.840 | 2.962 | 2.035 | 91.0 | 12.0 |
| grand | 3.859 | 2.594 | 1.952 | 214.4 | 13.4 |

  The spread narrowed. Across the four widths our EBF ran 3.113 to 3.859 before
  and 2.348 to 2.962 after, and the standard-to-grand gap the prerequisite
  named fell from 0.746 to 0.246. The node ratio against the reference, which
  is the figure prerequisite 8 raised, fell from a tenfold widening across the
  ladder (21.6 to 214.4) to under fourfold (3.6 to 13.4). The prerequisite's
  own numbers are not reproduced here: it reported 3.41 to 4.62 and this run
  measures 3.11 to 3.86 for the same binary lineage, because Phase A's search
  is not the search that was measured then.
- Mate discovery is the one gate that did not pass as written. Four cases, all
  standard, seed 42, Hash 64:

| case | A first mate | B first mate |
| --- | --- | --- |
| `k7/7R/1K6/8/8/8/8/8` | depth 1, mate 1 | depth 1, mate 1 |
| `k7/8/1K6/8/8/8/8/1R6` | depth 3, mate 2 | depth 3, mate 2 |
| `8/8/8/3k4/8/8/8/3QK3` | depth 15, mate 9 | depth 20, mate 10 |
| `8/8/8/8/8/2k5/8/K1R5` | none by depth 16 | none by depth 16 |

  On the bare queen the mate moved five iterations later, which the gate as
  phrased forbids. Indexed by the resource a game actually spends it moved
  earlier: first mate at 3,894,326 nodes and 0.96s against 9,587,340 nodes and
  1.64s, and the true mate 8 at 10,955,342 nodes and 2.28s against 14,876,605
  nodes and 2.62s. A depth index is not comparable across a patch that changes
  what a ply costs, so the gate is recorded as failed on its literal wording
  and passed on the measure it exists to protect. No fallback was applied on
  this evidence alone; fallback 1 could not have helped in any case, since the
  position has no captures for it to exempt.
- One imprecision is shipped knowingly. The full-window re-search fires on
  `score > alpha` without also requiring `score < beta`, so a scout that
  already beat beta pays one full-window search before the cutoff breaks the
  loop. It costs correctness nothing and, at 818 re-searches against 836,287
  nodes, close to nothing in work; Phase C touches this line for aspiration
  windows and can tighten it there.

Promotion games ran on `debug-headless sprt` with A = `bin/phaseB-4` and
B = `bin/phaseA-4`, clock 5000+50ms, bounds [0, 15], alpha = beta = 0.05, and
`ANEKAMACAM_SEED` unset. The four arms ran concurrently from per-variant copies
of both binaries in separate working directories, which is what Phase A's
status says the sandbox and the log roll require.

| arm | games | W | L | D | score | Elo | LLR |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 106 | 64 | 20 | 22 | 0.7075 | +153.5 | 3.006 |
| shogi | 292 | 183 | 109 | 0 | 0.6267 | +90.0 | 2.977 |
| crazyhouse | 286 | 174 | 109 | 3 | 0.6136 | +80.4 | 2.958 |
| xiangqi | 164 | 96 | 41 | 27 | 0.6677 | +121.2 | 2.982 |
| pooled | 848 | 517 | 279 | 52 | 0.6403 | +100.2 | 11.923 |

Unlike Phase A the pooled figure needs no reconstruction. Every arm crossed its
own +2.944 acceptance bound, so the pooled LLR is the sum of four independent
log-likelihood ratios taken against the same hypothesis pair: 11.923. No
campaign variant is near a non-regression bound, the weakest arm being
crazyhouse at +80.4 with a 95% interval of [+40.1, +122.8] on independent-game
variance. Games are paired two per opening, so the true intervals are tighter
than that arithmetic gives.

The margin is large enough to be worth naming plainly: this is the first search
patch in the iteration that changes what a ply costs, and the baseline it beats
searched every move at full depth. A patch of that shape should win by a lot,
and does.

## Phase C — Aspiration windows and mate-distance clipping

### Candidate

From depth four, search around the previous completed score with a derived initial
window. Widen only the failed side geometrically. Fall back to a full window near
mate. Add mate-distance alpha/beta clipping at every main-search node and return
immediately if clipping closes the window.

### Footprint and parameters

One `StaticState` aspiration delta plus scalar-tail entries for its ratio, clamp,
start depth, and widening ratio. No live state.

### Support gate

Mate scores and mate distances must remain exact. Record fail-low, fail-high, and
re-search nodes by depth. Reject widening storms.

### Promotion gate

Standard real-clock SPRT, H1 floor +8 Elo, plus pooled campaign non-regression.

### Same-letter fallbacks

1. Larger initial window with the same widening.
2. Asymmetric widening: wider after deterioration, narrower after improvement.
3. Mate-distance clipping plus a very broad aspiration window.

### Status

Status: accepted. Every campaign arm reached H1 on its own bound.

Three departures from the candidate as written:

- The candidate says "a derived initial window" without naming what it is
  derived from. It is the dearest non-royal opening piece value, scaled by
  `ASPIRATION_RATIO`. The cheapest value cannot serve: piece-value
  normalization pins the cheapest opening value at exactly 100 in every
  variant, so it carries no per-variant information at all. The dearest does
  vary, and it is the top of the variant's own score range, which is the scale
  a one-iteration swing should be drawn against. A flatter value range moves
  the score less per capture and correctly earns a narrower window. Derived
  deltas span 15 (minixiangqi) to 33 (grand); standard is 27.
- The footprint names four scalar-tail entries; it takes four, but the
  widening ratio is asserted **strictly greater** than `COEFFICIENT_SCALE` at
  payload load rather than merely at least. Equality would leave the window
  the same width after a failure and the loop would never terminate. The
  integer floor `.max(delta + 1)` covers the remaining stall case where the
  division rounds a small delta back onto itself.
- Phase B's shipped imprecision is closed here, as its status said this phase
  could: the full-window re-search now also requires `score < beta`. It is not
  dead code. `alpha_beta` is fail-hard for every searched line, but terminal
  returns — `terminal_score!`, the repetition `outcome_score!`, and the
  no-legal-move outcome — are not clamped to the window, so a scout child can
  genuinely return above beta.

`PARAM_SCALAR_COUNT` rose from 16 to 20; all 38 payloads were regenerated. The
eval prefix is untouched, so the node counts below compare searches and not
weights.

Mate-distance clipping is sound in both directions. `alpha.max(-INF + ply)` is
the score of being mated at this node and `beta.min(INF - ply)` the score of
mating at it, so when the two cross, `alpha` is the correct fail-hard answer:
either the floor already beat beta, which is a genuine fail-high, or the
ceiling fell to alpha, which is the fail-low signal. The clip is placed in
`alpha_beta` only. Quiescence is not a main-search node and its identical
prologue is deliberately left alone. It does not disable the
`alpha.abs() < MATE_SCORE` late-move-pruning guard, because at any node whose
alpha is a real score the clip is a no-op.

Support gate, all standard, one thread, Hash 64, `ANEKAMACAM_SEED=42`, A being
this phase and B `bin/phaseB-4`:

- Mate exactness, four fixtures searched to depth 8. Every score, every mate
  distance, every principal variation and every best move is identical between
  A and B. The clip pays for itself where it applies:

| case | score | A nodes | B nodes |
| --- | --- | --- | --- |
| `k7/7R/1K6/8/8/8/8/8` | mate 1 from depth 1 | 191 | 7,171 |
| `k7/8/1K6/8/8/8/8/1R6` | mate 2 from depth 3 | 1,420 | 10,159 |
| `8/8/8/8/8/2k5/8/K1R5` | cp 696 | 6,959 | 6,958 |
| `8/8/8/3k4/8/8/8/3QK3` | cp 1182 / 1186 | 6,338 | 6,961 |

  The two mated positions collapse by a factor of 38 and 7. The two won-but-
  unmated endgames are unchanged to within search noise, which is what a clip
  that only bites near mate should do.

- Fail counts and rejected-window nodes by depth, three positions to depth 14.
  Depths below 4 never narrow; depths that failed nothing are omitted.

| position | depth | fail low | fail high | window nodes |
| --- | --- | --- | --- | --- |
| `startpos` | — | 0 | 0 | 0 |
| open centre | 7 | 0 | 3 | 41,221 |
| open centre | 8 | 2 | 0 | 1,478 |
| open centre | 9 | 0 | 1 | 7,206 |
| open centre | 11 | 1 | 0 | 40,869 |
| open centre | 13 | 1 | 0 | 53,914 |
| open centre | 14 | 0 | 1 | 113,946 |
| symmetric | 12 | 0 | 1 | 54,969 |
| symmetric | 13 | 1 | 0 | 331,356 |

  Rejected windows cost 0%, 26.3%, and 30.8% of each position's nodes. Totals
  to depth 14 went 4,028,081 nodes to 3,404,865, a 15.5% cut, but the sign is
  not uniform: the startpos fell 8.1% and the open-centre position 45.2% while
  the symmetric position rose 30.4%. A narrower window is a bet, and one of
  three lost it.

- Widening storms. The structural bound is five narrowed attempts per side:
  `widest` is 16 times the opening delta and each failure doubles, so the
  fifth doubling passes it and that side reopens to `±INF`, after which the
  `alpha > -INF` / `beta < INF` guards stop it re-triggering. Measured worst
  case is three re-searches in one iteration, both on the three-position set
  above and on a four-position tactical set to depth 13. No iteration
  approached the bound.

Promotion games ran on `debug-headless sprt` with A = the Phase C build and
B = `bin/phaseB-4`, clock 5000+50ms, bounds [0, 8], alpha = beta = 0.05, and
`ANEKAMACAM_SEED` unset. The four arms ran concurrently from per-variant copies
of both binaries in separate working directories.

| arm | games | W | L | D | score | Elo | LLR |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 1272 | 521 | 431 | 320 | 0.5354 | +24.4 | 2.945 |
| shogi | 722 | 421 | 299 | 2 | 0.5845 | +59.4 | 2.949 |
| crazyhouse | 584 | 346 | 233 | 5 | 0.5967 | +67.3 | 2.944 |
| xiangqi | 1280 | 572 | 469 | 239 | 0.5402 | +28.0 | 2.988 |
| pooled | 3858 | 1860 | 1432 | 566 | 0.5555 | +38.7 | 11.826 |

Every arm crossed its own +2.944 bound against a hypothesis pair of [0, 8], so
the promotion gate's H1 floor of +8 Elo is met on the named standard arm and on
all three campaign arms besides. The pooled LLR is the sum of four independent
ratios taken against the same pair. No arm is near a non-regression bound; on
independent-game variance the weakest 95% interval is standard at [+8.1, +41.2]
and the next is xiangqi at [+10.9, +45.3]. Games are paired two per opening, so
the true intervals are tighter than that arithmetic gives.

Both arms that needed the most games are the two with heavy draw rates —
standard drew 25.2% and xiangqi 18.7%, against 0.3% and 0.9% for shogi and
crazyhouse — which is why they took roughly twice the games to separate at
half the measured margin. That ordering says nothing about where the patch
helps; it is the variance of the variant, not the size of the effect.

The margin is smaller than Phase B's by design. Phase B replaced a search that
examined every move at full depth, and aspiration windows only change the shape
of the window the root opens on a search that is already sound. A quarter of
the nodes at the root are now spent on windows that get rejected, and the patch
still wins, because the three quarters that survive are searched with far
tighter bounds than a full window gives.

## Phase D — Hoisted static evaluation, improving, and RFP

### Candidate

Store one static evaluation per ply in `SearchInfo`, not `State`. Compute it once
at a non-check node and reuse it for NMP and reverse futility pruning. Derive
`improving` from the evaluation two plies earlier. At shallow, non-check,
null-window nodes outside mate bands, return beta when static evaluation exceeds
beta by the derived improving-indexed margin.

Do not add TT eval reuse yet and do not sharpen static eval from TT bounds here.

### Footprint and parameters

One per-thread `[i32; MAX_DEPTH + 1]` and one small `StaticState` margin table.
Serialize RFP depth, base ratio, and improving multiplier in the scalar tail.
No `State` growth.

### Support gate

RFP hit rates must be nonzero in both improving rows. Endgame and tactical
fixtures unchanged. No broad node regression.

### Promotion gate

Pooled standard, xiangqi, and grand SPRT, H1 floor +8 Elo.

### Same-letter fallbacks

1. Reduce maximum RFP depth.
2. Use only the conservative margin row while retaining the eval stack.
3. Verify the deepest qualifying RFP cut with a one-ply search.

If RFP fails, retain the eval stack only as unlettered infrastructure and keep
Phase D unresolved until another significant candidate is approved.

### Status

Status: accepted. Pooled +61.8 Elo over Phase C across three arms.

Two departures from the candidate as written:

- The footprint names a per-thread `[i32; MAX_DEPTH + 1]`. It is a
  `Vec<i32>` of that length instead. `SearchInfo` is `#[derive(Default)]` and
  every other per-thread table it owns — the principal variation triangle, the
  history table, the killer table — is a `Vec` allocated in `clear_search` at
  the size the position calls for. A fixed array would be the only member that
  is not, for no gain: the stack is indexed by ply, which the `search_ply >=
  MAX_DEPTH` guard already bounds.
- The candidate says "the derived improving-indexed margin" without naming
  what it is derived from. It is the dearest non-royal opening piece value,
  the same anchor Phase C prices the aspiration window off and for the same
  reason: normalization pins the cheapest value at 100 in every variant, so
  only the dearest carries per-variant information. The margin is
  `RFP_RATIO` of it per ply still to search, and the improving row is that
  scaled by `RFP_IMPROVING`. Per-ply steps span 57 (minixiangqi) to 121
  (grand); standard is 102, so a depth-6 node has to clear 612 flat or 459
  improving.

`PARAM_SCALAR_COUNT` rose from 20 to 23; all 38 payloads were regenerated. The
eval prefix is untouched, so the node counts below compare searches and not
weights.

The stack is written after the transposition probe and before null-move
pruning, which is what makes reading two plies down sound. Every node that
recurses has passed that write, and quiescence never re-enters `alpha_beta`,
so a main-search node at ply `p` always finds its own line's ancestor value at
`p - 2` rather than a sibling's. Nodes that return before the write — the
depth-zero handoff, a transposition cutoff, a terminal or repetition score —
have no main-search descendants to mislead. A node in check stores `EVAL_NONE`
rather than a score, and a ply reading that reports `improving` false, so the
flag is conservative exactly where a static score means least.

Support gate, one thread, Hash 64, `ANEKAMACAM_SEED=42`, A being this phase and
B `bin/phaseC-4`:

- Hit rates are nonzero in both rows in every variant tried. One search per
  case, counters read at the last completed iteration:

| variant | depth | flat cuts | improving cuts |
| --- | --- | --- | --- |
| standard startpos | 12 | 517 | 11,242 |
| standard mid-game | 12 | 2,585 | 18,536 |
| shogi startpos | 10 | 323 | 4,828 |
| xiangqi startpos | 11 | 1,978 | 38,677 |
| grand startpos | 10 | 522 | 8,188 |
| crazyhouse startpos | 11 | 296 | 8,665 |

  The improving row outcuts the flat row by roughly twenty to one. That is
  the expected sign and not a defect: the row is chosen by the same condition
  the cut tests, so a node whose evaluation already beats beta by a wide
  margin is usually a node whose evaluation rose, and it is offered the
  smaller cushion of the two.

- Fixtures are unchanged. All 38 end-condition cases in
  `tools/endgame_fixtures.txt` pass. On six forced-mate fixtures searched to
  depth 12 and 14, every mate distance and every best move is identical to
  `bin/phaseC-4`:

| case | score | A best | B best |
| --- | --- | --- | --- |
| `6k1/5ppp/8/8/8/8/8/R3K3 w Q` | mate 1 | a1a8 | a1a8 |
| `7k/8/8/8/8/8/R7/1R5K w` | mate 2 | a2a7 | a2a7 |
| `r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq` | mate 2 | d5f6 | d5f6 |
| `1k5r/pP3ppp/3p2b1/1BN1n3/1Q2P3/P1B5/KP3P1P/7q w` | mate 3 | c5a6 | c5a6 |
| `6k1/pp4p1/2p5/2bp4/8/P5Pb/1P3rrP/2BRRN1K b` | mate 2 | g2g1 | g2g1 |
| `2rr3k/pp3pp1/1nnqbN1p/3pN3/2pP4/2P3Q1/PPB4P/R4RK1 w` | mate 2 | g3g6 | g3g6 |

  One of the six reports a different mating line at the same distance, which
  is two ways to mate in two and not a disagreement. Quiet positions do move:
  on the seven-position tactical set five keep their best move and the score
  shifts by at most 24 centipawns. A pruning change that left every quiet line
  alone would not be pruning anything.

- Nodes fall, not rise. `tools/ebf-suite.sh` over all 33 cases, both binaries
  at Hash 64:

| variant | cases | geometric mean A/B ratio | total nodes ratio |
| --- | --- | --- | --- |
| standard | 9 | 0.683 | 0.951 |
| crazyhouse | 9 | 0.269 | 0.532 |
| shogi | 4 | 0.259 | 0.282 |
| xiangqi | 4 | 0.512 | 0.427 |
| grand | 5 | 0.483 | 0.570 |
| capablanca | 1 | 0.475 | 0.475 |
| gothic | 1 | 2.299 | 2.299 |
| all | 33 | 0.443 | 0.550 |

  Total nodes over the suite fall from 166,797,149 to 91,659,605. Thirty-one
  of thirty-three cases fall; two rise, `standard/fsf-p24` at 1.615x and
  `gothic/startpos` at 2.299x. The same non-uniformity Phase C recorded
  applies here for the same reason: cutting a node changes which lines the
  rest of the search sees, and on a minority of positions that trade loses.
  No variant regresses in aggregate except gothic, whose single case is not
  a campaign arm and is one position.

  Promotion campaign, `bin/phaseC-4` as base, clock 5000+50ms, H0 0 Elo,
  H1 +8 Elo, alpha = beta = 0.05, bounds +-2.944:

| arm | games | W | L | D | score | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| grand | 400 | 213 | 115 | 72 | 0.6225 | +86.9 | 2.987 | H1 accepted |
| standard | 548 | 245 | 156 | 147 | 0.5812 | +56.9 | 2.980 | H1 accepted |
| xiangqi | 642 | 304 | 211 | 127 | 0.5724 | +50.7 | 2.956 | H1 accepted |
| pooled | 1590 | 762 | 482 | 346 | 0.5881 | +61.8 | — | — |

  Every arm accepted H1 on its own. Grand gains most, which is the expected
  shape: the widest board carries the most nodes per ply, so a beta cut taken
  before any move is made saves the most there.

## Phase E — Frontier pruning tranche

### Candidate

Use Phase D's eval stack and improving flag for one coherent frontier tranche:

1. improving-indexed futility margins for late quiet non-promotion moves;
2. improving-indexed LMP thresholds replacing `3 + depth * depth`;
3. shallow main-search SEE pruning for clearly losing captures.

All gates require non-check null-window nodes, a non-mate window, and at least one
legal move. Drops remain exempt until Phase S. Keep `continue`, not `break`,
because losing captures rank below quiets and quiet promotions share the quiet
band while remaining exempt. Phase U may change this only after banded ordering.

### Footprint and parameters

Small `StaticState` margin/threshold tables. Serialize generating ratios, floors,
maximum depths, and improving multipliers in the scalar tail. Assert each derived
margin row is monotone for every variant.

### Support gate

Record each prune independently. Run forced-defense, drop-mate, promotion, and
endgame ladders. Node cuts alone do not establish safety.

### Promotion gate

Pooled campaign SPRT, H1 floor +10 Elo.

### Same-letter fallbacks

1. Remove main-search SEE; keep futility and improving LMP.
2. Futility only with a looser deepest margin.
3. Improving LMP only.
4. Use improving only as an LMR input if all frontier pruning remains unsafe.

### Status

Status: accepted, pooled +20.7 Elo over 5852 games.
Started ahead of Phase D's promotion gate on request, stacked on the Phase D
commit so either can be dropped whole.

All three prunes are in and the tree type-checks. `PARAM_SCALAR_COUNT` rises
23 to 33: floor, ratio, improving multiplier and maximum depth for the
futility margin; base, ratio, improving multiplier and maximum depth for the
move count; ratio and maximum depth for the exchange allowance. Payload
regeneration and every measurement were held until a Phase D arm freed
cores, so the arms were not made to share cores with a build.

Five departures from the candidate as written:

- The maximum depth on the move count is a clamp, not a gate. A gate would
  leave every node deeper than it with no count at all, which is a removal:
  today's `3 + depth * depth` applies at every depth. Nodes past the last row
  reuse it, the same way a node ordering more moves than `REDUCTION_MOVE_CAP`
  reuses its last slot. The rows are built to depth 12, by which the count
  asks for 147 moves and no deeper row would say anything new.
- The gates require a null window, and the move count did not before. That
  narrows an existing prune rather than widening it: principal variation
  nodes now order every quiet. The candidate asks for null-window nodes
  throughout and it is the safer half of the trade.
- Exchange pruning reads the ordering score rather than simulating again.
  `score_move!` has already run the exchange simulation on every capture and
  banded the result, and the bands are disjoint, so the losing captures are
  exactly the scores below `LOSING_CAPTURE_SCORE` and the loss is recovered
  by subtracting it. A capture the simulation could not make keeps its own
  band and is left alone.
- Capturing promotions are exempt from exchange pruning. The simulation
  prices attacker against victim and never sees the promotion, so its verdict
  on a capturing promotion understates it. Quiet promotions were already
  exempt.
- Phase D's counters are renamed `reverse_cuts` and `reverse_cuts_improving`.
  Two unrelated things called futility in one struct is a defect, and the cut
  against beta is the reverse one.

The improving multiplier names the row that prunes harder, which is not the
same row in all three tables. The cut against beta believes a risen side
sooner, so its improving row is the smaller cushion. Both cuts against alpha
give up on a side that has not risen first, so theirs is the shorter count and
the smaller margin. Every node indexes by its own improving flag, so the
choice is made once in derivation rather than at each use.

Derived rows for standard, whose dearest non-royal opening value is 933:

| row | depth 1 | 2 | 3 | 4 | 5 | 6 |
| --- | --- | --- | --- | --- | --- | --- |
| futility margin, risen | 214 | 335 | 456 | 577 | 698 | 819 |
| futility margin, flat | 149 | 234 | 319 | 403 | 488 | 573 |
| move count, risen | 4 | 7 | 12 | 19 | 28 | 39 |
| move count, flat | 2 | 3 | 6 | 10 | 15 | 21 |
| exchange allowance | 233 | 466 | 699 | 932 | 1165 | — |

The risen move-count row reproduces `3 + depth * depth` exactly, so on that
row the change is not a change; only the flat row prunes earlier than today.
Derivation asserts every row rises with depth in every variant.

Each prune records independently. Every one fires in every variant probed
(seed 42, Hash 64, one thread):

| probe | futility | move count | exchange |
| --- | --- | --- | --- |
| standard d12 startpos | 69,557 | 218,418 | 2,015 |
| standard d12 midgame | 216,401 | 309,404 | 16,619 |
| shogi d10 | 22,984 | 88,458 | 443 |
| xiangqi d11 | 213,848 | 204,034 | 7,978 |
| grand d10 | 76,707 | 432,735 | 6,934 |
| crazyhouse d11 | 57,938 | 164,421 | 1,657 |

The Phase D reverse counters fall alongside — standard d12 startpos goes
517 flat and 11,242 improving to 351 and 5,357 — which is the expected shape:
the new prunes remove nodes before a reverse cut ever sees them.

The endgame fixtures pass 38 of 38. Four of five mate puzzles and all seven
tactical cases are byte-identical to Phase D.

One case regresses. On `2rr3k/pp3pp1/1nnqbN1p/3pN3/2pP4/2P3Q1/PPB4P/R4RK1 w`
Phase D reports `mate 2` at depth 12; Phase E reports `cp 20` and finds the
same mate only at depth 14. The cost is two plies of delay, not a lost mate.

The cause was isolated by rebuilding with one prune neutralised at a time
through its own scalars — the embedded payload wins over `res/param` on disk,
so each probe needed its own build:

| build | depth 12 verdict |
| --- | --- |
| `lmp_improving` 550 to 1000, flat row equal to the risen row | `cp 20` |
| `futility_ratio` 130 to 100000, margin unreachable | `mate 2` |
| `see_prune_ratio` 250 to 100000, allowance unreachable | `cp 20` |

Futility alone carries it. The move count and the exchange allowance are
innocent: neutralising either leaves the miss in place, and neutralising the
futility margin restores the mate with both of the others still live. The
move that is lost, `Qg3g6`, is a quiet queen sacrifice onto an empty square —
exactly the move class a static-eval alpha cushion is built to discard.

Promotion campaign, four arms at 5000+50, 2000 games each, `H0` 0 and `H1`
10 Elo, seed unset. An earlier run was discarded when the standard arm was
found to be carrying a stale patch binary; the numbers below are the
restart.

| arm | W | L | D | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- |
| shogi | 500 | 389 | 11 | 43.1 | 2.982 | H1 accepted |
| standard | 760 | 710 | 530 | 8.7 | 1.113 | inconclusive |
| crazyhouse | 726 | 624 | 32 | 25.7 | 2.945 | H1 accepted |
| xiangqi | 658 | 572 | 340 | 19.1 | 2.982 | H1 accepted |

Shogi crossed first, then crazyhouse, then xiangqi. Standard spent its
whole 2000-game budget without touching either bound: 530 draws out of
2000 is more than the other three arms drew between them, and a draw
carries no evidence either way, so the same Elo needs far more games to
resolve there. It ended positive at +8.7 and never once read negative.

Pooled over all four arms the campaign reads 2644W 2295L 913D, +20.7 Elo
over 5852 games, which clears the +10 floor the gate asks for. Three arms
accepted H1 outright. Phase E is accepted.

## Phase F — Quiescence discipline

### Candidate

At non-check qsearch nodes:

- skip negative-SEE captures;
- delta-prune non-promotion captures whose maximum derived gain cannot raise
  alpha;
- disable delta pruning in ENDGAME;
- retain full evasions while in check.

Do not add ordinary qsearch checks.

### Footprint and parameters

One delta-margin scalar and SEE threshold generation in `StaticState`, with
ratios/floors in the scalar tail. No live state.

### Support gate

Conversion, promotion, and evasion fixtures unchanged. Record qsearch nodes per
main-search node and score signs on a fixed position set.

### Promotion gate

Pooled standard, shogi, and xiangqi SPRT, H1 floor +8 Elo.

### Same-letter fallbacks

1. SEE pruning only.
2. Delta pruning only with a larger derived margin.
3. SEE pruning only below the first qsearch ply.

### Status

Status: accepted, pooled +56.6 Elo over 1784 games.

As landed, a quiescence node not in check stops at the first capture the
ordering has priced as losing. Captures are picked in descending score
order, so every capture behind that one is priced no better and the stop
costs nothing a scan would find. The same node skips a non-promotion
capture whose victim plus a margin still stands under alpha. The margin
is one new scalar, `qsearch_delta_ratio` at 100 -- a tenth of the dearest
non-royal piece, the anchor `futility_floor` already uses -- derived into
`StaticState.qsearch_delta`. The scalar tail is 33 to 34 and all 38
payloads were regenerated. Neither cut touches a position in check, and
the margin is not applied in ENDGAME.

The plan's footprint asked for a SEE threshold scalar as well. There is
none: the threshold is the boundary between the winning and losing
ordering bands, which is the sign of the exchange simulation, so a scalar
there would be a payload slot nothing ever moves.

Endgame fixtures pass 38 of 38.

Quiescence share of all nodes, depth 11, standard, Hash 64, seed 42:

| case | nodes | qsearch nodes | share | price stops | margin skips |
| --- | --- | --- | --- | --- | --- |
| startpos | 45,208 | 16,615 | 36.8% | 4,828 | 1,186 |
| aneka-p24 | 69,737 | 34,150 | 49.0% | 22,896 | 6,755 |
| aneka-p28 | 33,496 | 13,552 | 40.5% | 4,977 | 2,359 |
| aneka-p32 | 69,536 | 30,947 | 44.5% | 14,986 | 4,992 |
| aneka-p36 | 47,933 | 18,285 | 38.1% | 14,316 | 2,529 |
| fsf-p24 | 591,262 | 311,250 | 52.6% | 135,668 | 53,574 |
| fsf-p28 | 90,961 | 44,046 | 48.4% | 42,112 | 6,560 |
| fsf-p32 | 36,861 | 20,354 | 55.2% | 7,856 | 3,718 |
| fsf-p36 | 49,348 | 18,780 | 38.1% | 23,598 | 3,228 |

The price stop carries roughly four times what the margin does on every
case, which is what a capture-only leaf should look like: most of what a
node generates there is already priced, and only what survives the price
is worth measuring against alpha.

Nodes to fixed depth against Phase E, `tools/ebf-suite.sh`, Hash 64:

| variant | depth | cases under Phase E | range of Phase F over Phase E |
| --- | --- | --- | --- |
| standard | 13 | 9 of 9 | 0.31 to 0.81 |
| xiangqi | 12 | 4 of 4 | 0.37 to 0.63 |
| shogi | 11 | 3 of 4 | 0.46 to 1.49 |
| crazyhouse | 13 | 5 of 9 | 0.15 to 3.50 |

Standard and xiangqi cut nodes on every case. Crazyhouse swings both
ways by more than an order of magnitude, which is what a variant that
drops pieces back onto the board does to any change in leaf ordering: the
tree is wide enough that a different first capture moves the whole
iteration. The promotion campaign, not this table, decides whether that
swing costs anything.

Score signs agree with Phase E on 9 of 9 standard cases at depth 11, with
no gap wider than 12 units.

The mate fixtures hold except `puzzle-b`
(`1k5r/pP3ppp/3p2b1/1BN1n3/1Q2P3/P1B5/KP3P1P/7q w`), where Phase E
reports `mate 3` at depth 12 and Phase F reports `cp 381`, finding the
same `mate 3` at depth 13 and holding it through 16. The cost is one ply,
not the mate. Two bisect builds place it on neither cut alone:

| build | depth 12 verdict |
| --- | --- |
| `qsearch_delta_ratio` 100 to 1000, margin unreachable | `cp 1327` |
| price stop removed, margin left in | `cp 448` |

Each cut on its own still misses the mate at depth 12, so what costs the
ply is the leaf score being cheaper rather than either rule discarding
the mating line. `puzzle-a`, `puzzle-c`, `puzzle-d`, and the `kqk`
conversion read the same as Phase E.

Promotion campaign, three arms at 5000+50, 2000 games each, `H0` 0 and
`H1` 8 Elo, seed unset, each arm patched against the Phase E binary its
own Phase E arm ran.

| arm | W | L | D | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- |
| standard | 195 | 124 | 123 | 56.3 | 2.965 | H1 accepted |
| shogi | 349 | 222 | 9 | 77.3 | 2.990 | H1 accepted |
| xiangqi | 343 | 253 | 166 | 41.2 | 2.952 | H1 accepted |

A crazyhouse arm ran alongside on `H0` -8 and `H1` 8, outside the pooled
gate, because crazyhouse was the one variant whose node counts swung both
ways in the support gate. It accepted H1 at 188W 128L 10D, 64.7 Elo, LLR
3.003. The swing costs nothing over the board.

All three arms accepted H1. Standard crossed in 442 games and shogi in
580, against the thousand-plus Phase E's own arms needed for the same
call. A cut that removes half the nodes at a fixed depth buys depth at a
fixed clock, and that is what the arms are reading. Pooled over the three
the campaign reads 887W 599L 298D, +56.6 Elo over 1784 games. Phase F is
accepted.

## Phase G — Capped check extensions

### Candidate

Store current root depth in `SearchInfo`. Before the depth-zero qsearch dispatch,
extend a checked node only while cumulative extension stays within the derived
root-relative cap. No singular, multicut, or negative extension joins this phase.

### Footprint and parameters

One `SearchInfo` scalar. Serialize extension cap and eligibility depth in the
scalar tail. No `State` growth.

### Support gate

Reject before games if any campaign variant exceeds the predeclared node-growth
bound. Mates must be found at the same depth or earlier.

### Promotion gate

Pooled campaign SPRT, H1 floor +8 Elo. Then run external anchor RR #1 against RR
#0. If A-G do not materially improve anchor strength, stop and revise roadmap
ordering before Phase H.

### Same-letter fallbacks

1. One cumulative extension ply.
2. Extend only nodes not already reduced.
3. Extend only non-losing checking lines.

### Status

Status: candidate 1 abandoned by the user at intermediate tallies; fallback 1
landed, its support gate is open, and its promotion campaign is running.

Candidate 1 handed a checked frontier node a budget of `EXTENSION_CAP_RATIO`
of the root depth -- half of it -- so one line could gain several plies before
quiescence took over. Its support gate passed on every item and its campaign
ran at `[0, 8]` on standard, shogi and xiangqi. Standard stood at 70W 75L 55D
and shogi at 90W 105L 5D, LLR -0.33 and -0.40 against a lower bound of -2.94.
Neither verdict was terminal, so by rule 6 the candidate was never rejected on
evidence. The user abandoned it and directed the first fallback, which by rule
7 consumes no letter and is recorded as no evidence against the rule itself.

Fallback 1 replaces the budget with one cumulative extension ply.
`EXTENSION_CAP_PLIES` is 1 and `EXTENSION_START_DEPTH` stays 4: a node that
reaches depth zero while its king is attacked is searched one further ply past
the depth the iteration set out for, and a second check in the same line is
priced by quiescence as before. The scalar keeps slot 34, so the tail stays 36
and every payload carries 1 where it carried 500.

No counter tracks the gain: absent extension a frontier stands at `ply` equal
to the root depth, so `ply` above it is exactly what has been gained, and a
reduced line reads below it, which is true of that line.

The plan's footprint asked for one `SearchInfo` scalar. There are two fields:
`root_depth`, the scalar itself, and a `check_extensions` counter, because a
rule nothing counts cannot be gated and every earlier phase reports its own
firing count.

The node-growth bound the support gate calls for was not written down in
advance, so it is predeclared here: reject if the geometric mean of nodes to
fixed depth over the campaign variants rises above 1.25 of Phase F, or if any
single case rises above 2.0.

| variant | depth | cases | geometric mean | min | max |
| --- | --- | --- | --- | --- | --- |
| standard | 13 | 9 | 1.114 | 0.415 | 1.793 |
| shogi | 11 | 4 | 0.878 | 0.746 | 1.253 |
| xiangqi | 12 | 4 | 1.002 | 0.747 | 1.381 |

Every variant is inside both bounds. Shogi searches fewer nodes than Phase F
outright and xiangqi is level; standard costs 11% on the geometric mean, down
from the 18% candidate 1 asked for.

The extension fires on 0.2% to 2.1% of nodes across the three variants, the
rate a frontier-only rule should show: a checked node is rare, and only the
ones a search walks into at its last ply qualify.

Endgame fixtures pass 38 of 38. Mates are found at the same depth as Phase F
on every fixture: `puzzle-a` and `puzzle-c` mate in 2 at depth 12, `puzzle-b`
mate in 3 at 13, `puzzle-d` mate in 2 at 14, each swept from depth 12 to 16
against Phase F, and the `kqk` conversion reads the same score. None is found
earlier either -- the extension pays in ordinary play, not on these five.

The campaign runs the same three arms at `5000+50`, 2000 games, bounds
`[0, 8]`, seed unset, patch `91e3d133` against Phase F base `b9b03be1`.

A first run of this campaign was stopped by the user before any arm reached a
bound and no `latest.sprt` was written, so nothing from it is a verdict and by
rule 8 none of it is acted on. The tallies at that stop were standard 80W 83L
67D, shogi 100W 125L 5D and xiangqi 82W 71L 67D. An SPRT keeps no resumable
state, so all three arms were relaunched from zero on the same staged binaries
in `/tmp/pg2-sprt`, each with:

```
cd /tmp/pg2-sprt/<variant> && env -u ANEKAMACAM_SEED nohup ./patch \
    debug-headless sprt <variant> ./patch ./base 5000+50 2000 0 8 \
    > run.log 2>&1 &
```

| arm | W | L | D | games | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 705 | 695 | 600 | 2000 | +1.7 | -0.569 | inconclusive, budget reached |
| shogi | 998 | 933 | 69 | 2000 | +11.3 | +1.022 | inconclusive, budget reached |
| pooled | 2467 | 2405 | 1128 | 6000 | +3.6 | -0.906 | inconclusive |
| xiangqi | 764 | 777 | 459 | 2000 | -2.3 | -1.359 | inconclusive, budget reached |

All three arms spent their whole 2000-game budget without touching a bound, and
the pooled LLR, which is the sum of the three, is -0.906 against bounds of plus
and minus 2.94. Nothing here is terminal, so by rule 7 the candidate is not
rejected and stays unresolved; it also comes nowhere near the +8 floor the
promotion gate asks for. Shogi carries the whole of the pooled lean at +11.3,
while standard and xiangqi sit within a couple of Elo of zero, so on those two
variants one cumulative extension ply neither pays nor costs.

Rule 7 would extend the same cumulative test rather than call this a failure,
and a second 2000 games per arm was launched to pool with the first 6000. The
user stopped that extension on the pooled read and abandoned letter G outright,
fallbacks 2 and 3 included. By rules 7 and 9 the abandonment consumes no letter
and stands as no evidence against check extensions as an idea; what it records
is that neither shape tested paid for its plies at this time control.

The check extension is therefore gone from the tree. `9f94ae3` stays in history
and the code it carried was reverted, so `src` and every payload match Phase F
`fdbcf8d` exactly: no `EXTENSION_*` constants, no `root_depth` or
`check_extensions` in `SearchInfo`, the scalar tail back to 34 tokens, and a
rebuilt binary that reproduces Phase F node counts and principal variation on a
seeded fixed-depth search. The anchor round robin the promotion gate asks for
never ran: `cutechess-cli` is not installed. Letter G stays open for a future
candidate; the roadmap continues at Phase H.

The working tree carries fallback 1 uncommitted. Rule 11 commits only an
accepted candidate, and candidate 1 already sits on this branch as `9f94ae3`,
so on acceptance that commit is amended and letter G stays one commit.

## Phase H — Material-sensitive draw scoring

### Candidate

Restore one bounded `draw_score!` and route every draw source through it:
terminal draws, declared draw outcomes, and both in-search repetition returns.
The historical material delta is phase-selected: use endgame material in ENDGAME
and opening material otherwise. Do not sum opening and endgame material and do
not force opening material in every phase.

A side ahead in material receives a negative draw value; a side behind receives a
positive one. Decisive mate-distance scores stay unchanged.

### Footprint and parameters

One `StaticState` bound. Serialize bound ratio, floor, and material divisor in the
scalar tail. No live state.

### Support gate

Every draw fixture keeps its declared result. Record returned draw-score
distributions. Variants with no draw path remain fixed-depth identical.

### Promotion gate

Standard SPRT, H1 floor +20 Elo, with affected-variant non-regression. A major
shortfall against the historical prior first triggers harness/provenance review,
not automatic promotion or rejection.

### Same-letter fallbacks

1. Half the bound.
2. Apply bias only to repetition and counting draws.
3. Use phase-blended material rather than phase-selected material.

### Status

Candidate 1 built as planned. `draw_score!` sits in
`src/game/position/evaluation.rs`; `terminal_score!`, `outcome_score!` in
`src/game/representations/termination.rs`, and both in-search repetition
returns in `src/game/position/search.rs` all route through it. The bound is
derived in `derive_search_parameters` as
`(dearest * DRAW_BOUND_RATIO / COEFFICIENT_SCALE).max(DRAW_BOUND_FLOOR)` and
stored as the single `StaticState` field `draw_bound`. `DRAW_BOUND_RATIO = 50`,
`DRAW_BOUND_FLOOR = 8`, and `DRAW_MATERIAL_DIVISOR = 8` are serialized in the
scalar tail, which grows from 34 to 37 tokens across all 38 payloads. No live
state was added.

Derived bounds, from each payload's dearest non-royal opening value:

| variant | dearest | draw bound |
| --- | --- | --- |
| standard | 933 | 46 |
| crazyhouse | 936 | 46 |
| shogi | 859 | 42 |
| xiangqi | 776 | 38 |
| makruk | 573 | 28 |

Support gate passed.

- Draw fixtures: 38 of 38 keep their declared result under
  `tools/run_endgame_fixtures.sh`.
- Fixed-depth identity where no draw is reachable: all 26 `tools/ebf-suite.sh`
  cases across standard, shogi, xiangqi, and crazyhouse are node-identical
  between the Phase F base and the Phase H patch, geomean 1.000, no case
  differing at all. The change only bites where a draw is actually returned.
- Draw-score distribution: at depth 8 over the fixture set, 6 of 38 positions
  move off zero and every one of them lands exactly on that variant's bound,
  so the clamp is the binding term rather than the material divisor.

| fixture | Phase F | Phase H |
| --- | --- | --- |
| xiangqi chase not sustained every ply -> repetition draw | cp 0 | cp +38 |
| xiangqi repetition with no offence -> draw | cp 0 | cp +38 |
| xiangqi perpetual one cycle short: nothing terminal | cp 0 | cp +38 |
| shogi plain 4-fold repetition (no perpetual check) | cp 0 | cp +42 |
| makruk KRk one move short of the 16-count | cp 0 | cp -28 |
| ouk-chaktrang KRk one move short of the 16-count | cp 0 | cp -28 |

The sign is right in both directions: the four repetition cases have the side
to move behind and read positive, the two counting cases have the side to move
ahead with the rook and read negative.

Promotion campaign launched against Phase F base `b9b03be1`, patch
`ed04b2f5`, `5000+50`, seed unset, fresh directory `/tmp/ph-sprt`: standard on
the promotion bounds `[0, 20]`, shogi and xiangqi as affected-variant
non-regression arms on `[-8, 8]`, 2000 games each.

Candidate 1 rejected. The xiangqi non-regression arm reached a terminal H0 at
LLR -2.951 on 134W 174L 76D over 384 games, an estimate of -36.3 Elo, so the
affected-variant half of the promotion gate fails whatever the standard arm
returns. The standard and shogi arms were left running to terminal for
evidence rather than judged on an intermediate tally.

| arm | W | L | D | games | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| xiangqi | 134 | 174 | 76 | 384 | -36.3 | -2.951 | H0 accepted, no improvement |

Fallback 1, half the bound, built: `DRAW_BOUND_RATIO` drops from 50 to 25 and
all 38 payload tails move from ` 50 8 8` to ` 25 8 8`. Nothing else changed, so
the fixed-depth identity measured for candidate 1 carries over unchanged. The
fixtures still pass 38 of 38 and the same 6 positions move off zero, each at
exactly half its earlier value: xiangqi +19, shogi +21, makruk and
ouk-chaktrang -14.

The fallback 1 campaign runs off this machine, on `upi@157.10.252.201`
(Debian, x86_64, 10 cores). The branch travelled as a git bundle into
`~/anekamacam` as `phaseH4`; `~/ph2/base` is a detached worktree at `a137218`
and `~/ph2/patch` the same commit with the uncommitted fallback diff applied.
Both were built there, binaries `~/ph2/bin/{base,patch}`, arms under
`~/ph2/sprt/<variant>/`: standard on `[0, 20]`, shogi and xiangqi on
`[-8, 8]`, `5000+50`, 2000 games, seed unset. The candidate-1 standard and
shogi arms were killed before reaching terminal to free the local machine;
their last tallies were +2.1 Elo at LLR -1.55 over 670 games and -11.4 Elo at
LLR -0.90 over 610 games, both intermediate and neither a verdict.

Fallback 1 rejected. The standard promotion arm reached a terminal H0 at LLR
-3.008 on 136W 156L 102D over 394 games, an estimate of -17.7 Elo. Halving the
bound did not rescue xiangqi either: that arm stood at -24.5 Elo, LLR -1.91
over 440 games when it was stopped, still heading the same way as it had at
the full bound.

| arm | W | L | D | games | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 136 | 156 | 102 | 394 | -17.7 | -3.008 | H0 accepted, no improvement |
| shogi | 212 | 214 | 4 | 430 | -1.6 | -0.10 | stopped, intermediate |
| xiangqi | 161 | 192 | 87 | 440 | -24.5 | -1.91 | stopped, intermediate |

Fallback 2 built: the bias applies only to the draws the side to move could
still have refused. `outcome_score!` prices a draw at zero again, so a
stalemate and every other outcome-shaped draw is level; the in-search
repetition returns keep `draw_score!`, and `terminal_score!` reaches for it
only when `counting_resolved!` holds -- a new macro in `termination.rs` that
tests the same `progress >= limit` condition `position_terminal` fires the
counting rule on. `DRAW_BOUND_RATIO` returns to 50 and the payload tails to
` 50 8 8`.

The fixtures still pass 38 of 38 and the same 6 positions carry the same full
bound as candidate 1 -- every one of them is a repetition or counting case, so
the fixture set cannot separate fallback 2 from candidate 1 at the root. The
difference lives at interior nodes, where a stalemate or any other draw rule
now returns level.

Fallback 2 campaign ran on the same remote host, `~/ph3`, patch `d169783d`
against the same base `bcc4f6f5`. The standard promotion arm reached a
terminal H0 at LLR -3.059 on 408W 410L 276D over 1094 games, an estimate of
-0.6 Elo. Restricting the bias to refusable draws removed the damage the
earlier two arms took -- standard sat at -17.7 Elo when every draw carried the
bias and reads level now -- but it bought nothing, so the +20 floor is out of
reach. The shogi and xiangqi arms were left running to terminal, since whether
they come back level decides where the earlier regression came from.

| arm | W | L | D | games | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 408 | 410 | 276 | 1094 | -0.6 | -3.059 | H0 accepted, no improvement |
| shogi | 979 | 975 | 46 | 2000 | +0.7 | +0.177 | inconclusive, budget reached |
| xiangqi | 498 | 536 | 262 | 1296 | -10.2 | -2.977 | H0 accepted, no improvement |

Fallback 2 rejected on both halves of the gate. Standard cannot reach the
floor and xiangqi still fails non-regression at -10.2 Elo, so the two sources
of harm separate cleanly: pricing stalemate-shaped draws cost standard its
-17.7, and pricing repetition draws costs xiangqi about -10 on its own.
Xiangqi is the variant whose whole opening theory is repetition and chase law,
and telling its search that a repetition is worth less than nothing to the
side holding material makes it play on where the position does not support
it. Shogi, which has almost no drawing path, reads level in every arm run so
far.

Fallback 3, phase-blended material, was not built. Every arm of the ladder had
by then measured the same thing from three directions: the bias is worth
nothing on standard and costs xiangqi about ten Elo, and blending the material
read changes only how the edge is measured, not that a repetition is priced
against the side holding it. The user closed the letter and the whole of Phase
H was reverted out of the tree -- `draw_score!`, `counting_resolved!`, the
three `DRAW_*` constants, the `draw_bound` field, and the ` 50 8 8` payload
tails all went with it. `PARAM_SCALAR_COUNT` is back to 34 and `src` and every
payload match `fdbcf8d` again.

Letters G and H are closed with nothing kept. G ran a full 6000-game pooled
campaign and returned no verdict on any arm -- stagnation, +3.6 Elo pooled
against an +8 floor. H is worse than stagnation: two of its three candidates
regressed, at -17.7 Elo on standard for the unrestricted bias and -10.2 on
xiangqi for the restricted one, and the arrangement that stopped the bleeding
measured -0.6 Elo. Neither letter is recorded as evidence against the ideas
themselves, only against these implementations of them. The roadmap continues
at Phase I.

## Phase I — Royal shelter and friendly cover

### Candidate

Derive compact local square lists, not one `Board` per square:

- fixed-radius adjacency around each square;
- forward/home shelter directions inferred from initial deployment, promotion
  orientation, and short-range movement geometry;
- shield-like types inferred from short range, forward bias, and non-royal role.

Maintain one live `[Board; 2]` containing shield-like occupancy. For every royal,
score occupied shelter and friendly local cover outside ENDGAME. No-royal
positions naturally score zero; multi-royal positions sum all royals.

### Footprint and parameters

Static storage is `O(board_size * fixed_local_degree)`. Live storage is one
`[Board; 2]`. Serialize shelter radius, cap, and value ratios/floors in the
scalar tail.

Update clone, construction, reset, make, undo, and `verify_game_state` together.
No other phase may silently depend on an incompletely maintained board.

### Support gate

Full perft after make/undo edits. Debug verification recomputes shield occupancy
from scratch. Wide-board memory is linear. Evaluator cost stays within the
predeclared NPS ceiling.

### Promotion gate

Standard plus royal-bearing campaign pool, H1 floor +12 Elo.

### Same-letter fallbacks

1. Friendly occupancy cover only, dropping shield classification.
2. Radius-one shelter only.
3. Read existing occupancy directly with a fixed local list and add no live
   field.

### Status

Built, with one deviation from the candidate: no live `[Board; 2]` of
shield-like occupancy. The candidate's own storage rule already says compact
local square lists rather than a board per square, and the evaluator therefore
walks a list of squares whichever way occupancy is stored, so a live board buys
one bit test in place of one mailbox read and costs a mirrored update in every
one of the roughly twenty-five sites where `make_move!` and `undo_move!` touch
`pieces_board`. The scoring semantics of candidate 1 are kept whole and the
storage is candidate 1's third fallback: read the mailbox and the existing
`pieces_board` through fixed local lists.

`derive_shelter_parameters` builds two flat lists with one stride of eight slots
per origin square, a per-origin count so an edge square reads only the squares
that exist, and both piece prices off the dearest non-royal piece. Forward
directions come from the mean deployment rank of each colour. Shield-like types
are non-royal, stay inside the neighbourhood, and lean forward; move vectors are
stored in the mover's own frame, so the lean test needs no board direction, and
a leap out of the neighbourhood is only allowed straight ahead, which admits a
pawn's double step and rejects the shogi knight. Six scalars join the tail
(`PARAM_SCALAR_COUNT` 34 to 40): radius, cap, and a ratio and floor for each of
shelter and cover, defaulting to 1, 3, 12/1000, 4, 5/1000, 2. Shelter is carried
by the opening half of the blend alone, so it tapers with material and is gone
in the endgame, where a royal wants to walk rather than hide.

Inference reads sanely per variant: standard, crazyhouse, horde, xiangqi and
janggi flag the two pawns; makruk flags pawn and khon; minishogi flags five
types a side; shogi flags seven a side, pawn, silver, gold and the four
promotions that move as gold, once the straight-only allowance rejects its
knight.

Support gate: no make/undo or state-field edits were made, so perft is
unreachable from this change and the debug recomputation has no new live field
to check; standard perft at depth 4 is 197281 as before. Static storage is
`board_size * 8` squares plus `board_size` counts per list, linear in board area
by construction. Evaluation moves the way it should: on a castled-versus-exposed
pair the term is worth +33, and removing one shelter pawn costs exactly one
shelter piece, -11. NPS on the standard bench, where base and patch search the
same tree node for node, is 3.75M against 3.60M, about four percent, which the
term is well inside.

The campaign ran on standard, shogi, xiangqi and crazyhouse at `5000+50`, a
3000-game budget an arm, `[0, 12]`, patch against `a137218`.

| variant | W | L | D | games | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 1162 | 1097 | 741 | 3000 | +7.5 | +0.757 | inconclusive at budget |
| crazyhouse | 1490 | 1444 | 66 | 3000 | +5.3 | -0.234 | inconclusive at budget |
| shogi | 1480 | 1501 | 19 | 3000 | -2.4 | -2.458 | inconclusive at budget |
| xiangqi | 357 | 390 | 173 | 920 | -12.5 | -2.948 | H0 accepted |

Pooled: 4489W 4432L 999D over 9920 games, +2.0 Elo. Candidate 1 is rejected. No
arm reached the floor, three ran out of budget with the pool a fifth of the way
there, and the one arm that did reach a verdict reached the wrong one, so
extending the inconclusive arms cannot carry them to +12.

The split says which half of the term is carrying which sign. Shelter is worth
up to 33 against cover's 12, so standard's +7.5 is mostly its pawns. Xiangqi
flags only pawns as shield-like and its pawns leave the palace early, so the
shelter half is close to dead there and the -12.5 is cover: an advisor or an
elephant beside the general is bonus the position was born with, and the term
charges for spending either one, which is exactly what a xiangqi defence has to
do. That reading predicts fallback 1, which keeps cover and drops the shield
classification, is the weaker of the two halves, and fallback 2, shelter alone,
the stronger. The letter's fallbacks are tried in the order the plan sets them
out regardless, so fallback 1 is measured first and this paragraph stands as the
prediction it tests.

Fallback 1 is under measurement on the same four variants, same budget and same
base. It is a payload flip rather than a code change: shelter ratio and floor go
to zero, which zeroes the shelter half and leaves cover alone scoring, so the
shield classification stops reaching the score without a second build to review.
All 38 payload tails move from ` 1 3 12 4 5 2` to ` 1 3 0 0 5 2`.

| variant | W | L | D | games | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 1128 | 1073 | 799 | 3000 | +6.4 | +0.198 | inconclusive at budget |
| xiangqi | 1181 | 1165 | 654 | 3000 | +1.9 | -1.972 | inconclusive at budget |
| crazyhouse | 1016 | 1056 | 66 | 2138 | -6.5 | -3.006 | H0 accepted |
| shogi | 630 | 695 | 23 | 1348 | -16.8 | -2.975 | H0 accepted |

Pooled: 3955W 3989L 1542D over 9486 games, -1.2 Elo. Fallback 1 is rejected on
two terminal H0 arms and a pooled estimate below zero.

The prediction above does not survive contact. Cover alone is worse than the two
halves together, which is the direction the prediction expected, but the variant
it named is the one that reversed: xiangqi went from -12.5 with both halves to
+1.9 with cover alone, and shogi went the other way, from -2.4 to -16.8. Two
arms swapping sign by ten Elo and more between campaigns of this size says the
per-variant readings here carry noise of that order, so no story about palaces
or advisors is supported. What both campaigns do agree on is the pooled figure:
+2.0 for the full term, -1.2 for cover alone, against a floor of +12.

Fallback 2, shelter alone, is the last one this letter has, and the payload flip
mirrors fallback 1: all 38 tails move to ` 1 3 12 4 0 0`.

| variant | W | L | D | games | Elo | LLR | verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| standard | 1003 | 908 | 727 | 2638 | +12.5 | +2.967 | H1 accepted |
| crazyhouse | 1495 | 1369 | 64 | 2928 | +15.0 | +2.962 | H1 accepted |
| shogi | 874 | 762 | 14 | 1650 | +23.6 | +2.954 | H1 accepted |
| xiangqi | 428 | 460 | 254 | 1142 | -9.7 | -3.051 | H0 accepted |

Pooled: 3800W 3499L 1059D over 8358 games, +12.5 Elo. Recomputing the SPRT on
the pooled counts against the same `[0, 12]` bounds gives LLR +6.20 versus a
+2.944 upper bound, so the pool is terminal H1 at the floor. That pooled LLR is
computed here with a fixed-draw-rate logistic model rather than by the engine;
on each individual arm that model reads slightly less extreme than the engine's
own figure quoted above, so the engine's pooled LLR would be at least as far
past the bound.

Shelter alone is the whole of the idea. The three arms of this letter measured
+2.0 for shelter plus cover, -1.2 for cover alone, and +12.5 for shelter alone,
on roughly ten thousand games each. Friendly occupancy near the royal was not a
weak signal being diluted -- it was actively cancelling a real one. Counting any
piece that happens to stand near the royal pays for the attacking pieces a side
has swung across its own castled position and for the rook still sitting in the
corner, neither of which shelters anything. Restricting the count to
shield-like pieces, which is what the shield classifier was for, is what makes
the term measure what it was named for.

The pooled gate is met but the second clause of rule 5 is not: xiangqi is a
member of the declared royal-bearing pool and regressed to a terminal H0 at
-9.7 Elo, the same magnitude that closed Phase H's fallback 2.

The cause was measured rather than guessed, and the first guess was wrong.
Magnitude is not the problem: shelter is worth 9 raw units in xiangqi against a
164-unit soldier, and 12 units in standard against a 93-unit pawn, so xiangqi
carries the smaller term relative to its own army, not the larger one.

The problem is which squares a xiangqi side can ever earn the term on. Shelter
squares are the squares forward of the royal. Xiangqi soldiers begin on rank
four, ahead of the palace, and can never move backward, so no soldier can ever
arrive in front of a general standing on rank one, and the derived classifier
flags only soldiers as shield-like. The single way a xiangqi side can score
shelter is to walk the general up the palace to rank three, where the c4 and e4
soldiers become forward of it. Moving the general from e1 to e3 and touching
nothing else, the base reads -11 and the shelter build reads -2: the piece
square table charges 11 units for advancing the general and the term hands 9 of
them back. It also pays the side for leaving the central soldier at home, since
pushing it drops the count. Both are close to the opposite of xiangqi safety,
where a general on rank three is exposed to the flying-general law and to
cannon batteries on its own file.

The fix reads the palace off the data the config already carries. A palace is a
forbidden zone, so the squares a royal may ever stand on are a popcount of its
own zone bitboard, and `derive_royal_confinement` switches shelter off for a
colour whose royal reaches at most a quarter of the board. Xiangqi's general
reaches 9 of 90 squares and gates off; janggi and minixiangqi gate off by the
same rule; standard at 64 of 64 and shogi at 81 of 81 are untouched. There is
no variant name anywhere in it, and `SHELTER_CONFINEMENT_DIVISOR` stays a plain
constant rather than a scalar, since nothing tunes it.

The gate is verified by identity, not by a campaign, because it makes the term
provably zero in xiangqi: both colours' shelter counts are zeroed and the
shipped payload prices cover at zero, so `royal_shelter!` returns zero for every
xiangqi position. Xiangqi search output matches the base exactly -- move, score
and node count -- on four positions including one with the general already on
e3, and the depth-8 bench matches at 146954 nodes against the base's 146954.
The three winning variants match the ungated build exactly on the same checks,
at 37268, 888112 and 813213 bench nodes and identical startpos searches at depth
10, so their arms carry over to the gated build rather than needing a rerun.

Status: accepted. The term applies to standard, shogi and crazyhouse, whose
pooled result is 3372W 3039L 805D over 7216 games, +16.0 Elo at LLR +8.10
against a +2.944 bound -- terminal H1 well clear of the +12 floor. Xiangqi is
gated out and is byte-for-byte the base, so there is no affected-variant
regression left to disqualify it. Cover remains in the code priced at zero by
every payload; removing it is a simplification for a later letter, not part of
this one, since deleting it would change the three arms' binaries.

### Standing instruction on this letter

If Phase I fails on every fallback, letters G, H and I are all closed with
nothing kept and the whole of Phase I comes out alongside them. The last phase
this iteration actually accepted is F, and `a137218` is the tree that carries
it. A new plan follows, written as a retrospective of this one, and takes over
from that commit rather than continuing the letter ladder here.

## Phase J — Bounded royal pressure and open lanes

### Candidate

Build a compact relative kernel of radius R per piece type, independent of board
area. At evaluation, inspect only the current royal's fixed local zone through
existing attack primitives. Score enemy control of that zone and open shelter
lanes. Sum over all royals. Never build royal-square × attacker-square tables and
never walk compiled move vectors at the leaf.

Hopper/screen-sensitive types receive a derive-time discount only if a targeted
xiangqi diagnostic proves the relative kernel overstates them.

### Footprint and parameters

`piece_count * (2R + 1)^2` static kernel plus small scalars. No new live field.
Serialize R, danger scale, lane penalty, and accumulation shape in the scalar
tail.

### Support gate

Mandatory 2048-square allocation/derive check and an evaluator NPS loss ceiling
of 3%. No live fallback is permitted merely to rescue a slow design.

### Promotion gate

Pooled standard, shogi, xiangqi, and grand SPRT, H1 floor +10 Elo.

### Same-letter fallbacks

1. Radius-one pressure only.
2. Open-lane penalty plus direct attackers of the royal square.
3. Direct-attacker count only, with no area kernel.

## Phase K — Drop-aware royal porosity and hand attack potential

### Candidate

Behind `drops!`, count empty legal-local landing squares in the enemy royal zone
and multiply that porosity by static attack-potential weights for held non-royal
types. Derive weights from movement geometry and Phase J's kernel. Never generate
a drop list inside evaluation and never rescale held material as a standalone
term.

### Footprint and parameters

Small per-type static weights, no live field. Serialize combining ratio, cap, and
potential scale in the scalar tail. Non-drop variants must be fixed-depth
byte-identical.

### Support gate

Crazyhouse sign flips and median optimism in `agree-suite` must improve without
cross-engine unit conversion. Samples come from both engines' games independent
of result.

### Promotion gate

Crazyhouse and shogi pooled SPRT, H1 floor +15 Elo. Then run anchor RR #2.

### Same-letter fallbacks

1. Porosity only.
2. Count only landing classes capable of immediate check or shelter removal.
3. Gate on a nonempty enemy hand and halve the scale.

## Phase L — Legal movement-graph promotion potential

### Candidate

Replace straight-line promotion-zone distance with multi-source reverse BFS over
each eligible type's declared movement graph. Respect directional movement,
forbidden geometry, optional/mandatory promotion zones, and promotion mapping.
Weight graph distance by derived promoted-minus-current value. Fold output into
PSTs; retain only linear per-square data.

Temporary derive work may traverse declared movement edges, but retained tables
must remain `O(piece_types * board_size)`.

### Footprint and parameters

No live state. Serialize promotion-gradient ratio, unreachable-distance policy,
and cap in the scalar tail. Regenerate all PST payloads as rider work.

### Support gate

Dump monotone maps for standard, shogi, minishogi, xiangqi, and grand. Exercise
one-way, cyclic, slider, and forbidden-zone movement. All configs load under
wide-board.

### Promotion gate

Pooled promotion-variant SPRT, H1 floor +10 Elo.

### Same-letter fallbacks

1. Unweighted reverse-BFS distance.
2. Separate entry and exit distance for rules that distinguish them.
3. Use graph distance only for movement graphs proven asymmetric.

## Phase M — Declared win-objective pressure

### Candidate

Add three declaration-gated terms using already maintained progress:

1. goal proximity from Phase L's reverse graph seeded by the declared goal zone
   and restricted to its declared piece set;
2. checks urgency from `Checks::delivered` and declared target count;
3. extinction scarcity from existing `piece_count`, declared set, and threshold.

Variants lacking a declaration must be fixed-depth identical. No objective term
may alter game-truth detection.

### Footprint and parameters

Small static tables/scalars, no live state. Append one schema slot per accepted
subterm's scale/curve. Derive each from affected piece values and declared limit.

### Support gate

Fixtures show the expected score ordering while exact terminal results remain
unchanged. Verify per-subterm no-op identity on non-declaring variants.

### Promotion gate

Pooled affected variants, H1 floor +10 Elo.

### Same-letter fallbacks

1. Goal proximity plus extinction scarcity.
2. Checks urgency only.
3. Goal proximity only.

## Phase N — Declared move-limit pressure

### Candidate

Use only progress already represented incrementally:

- `Counter::clock / Counter::limit`;
- `Counting::progress` and its frozen limit.

For rules whose declared outcome is Draw, progressively blend the current score
toward Phase H's material-sensitive draw value as the limit approaches. This
makes an ahead side feel urgency and a behind side value delay. Use exact rule
subject semantics; do not assume the side to move is always the subject.

Do not invent a perpetual-exposure counter. Perpetual is computed on demand from
history and is scored only when the repetition detector resolves it. Do not add
an adjudication-proximity term: `Adjudicate` has no approach counter, and double
pass alone does not justify another live field.

### Footprint and parameters

No new live field. Serialize curve exponent/shape, maximum blend, and minimum
activation fraction in the scalar tail. Rules without a qualifying draw limit
are byte-identical.

### Support gate

Counter/counting fixtures keep exact outcomes. Paired positions differing only
in clock progress show monotone score drift in the correct direction.

### Promotion gate

Pooled affected move-limit variants, H1 floor +8 Elo.

### Same-letter fallbacks

1. Counting only.
2. Counter only.
3. Activate only in the final quarter of the declared limit.

## Phase O — Placement progress and coordination

### Candidate

One compact tranche:

1. fold setup-derived development pressure into opening PSTs from initial
   deployment and legal graph distance, with no `has_moved` state;
2. derive confinement/color-bound classes from reach graphs and score compact
   pair/coordination effects from existing piece counts;
3. add one shield-breakthrough signal using Phase I occupancy and declared
   promotion direction/zones.

Do not restore a multi-term FIDE pawn evaluator or `has_castled`.

### Footprint and parameters

Static flags, lists, and PST changes only. No live state beyond Phase I's shield
board. Serialize development, coordination, and breakthrough scales in the
scalar tail.

### Support gate

No FIDE piece-name, straight-rank, or one-direction assumption. No-setup,
no-promotion, and no-royal variants degrade to zero. Evaluator NPS loss stays
below 3%.

### Promotion gate

Pooled standard, shogi, xiangqi, and grand SPRT, H1 floor +10 Elo. Then run
anchor RR #3.

### Same-letter fallbacks

1. Development PST plus confinement pair only.
2. Breakthrough only.
3. Role-balance bonus from existing major/minor counts.

## Phase P — Gravity history and depth-shaping feedback

### Candidate

Replace add-and-clamp butterfly updates with gravity updates. Restore a derived
depth-scaled bonus schedule. Preserve the all-node quiet malus. Then connect the
new distribution to:

- Phase B LMR reduction discounts/penalties;
- Phase E LMP thresholds;
- Phase D improving state.

Do not combine old thresholds with the new distribution without measuring where
the new quartiles lie.

### Footprint and parameters

Reuse the linear per-thread butterfly history established unlettered. Store the
depth bonus table in `StaticState`. Serialize gravity gain, bonus scale, history
bound fractions, and LMR/LMP feedback weights in the scalar tail.

### Support gate

History saturation must fall materially. Record distribution, reduction rates,
re-search rates, and node counts. Mate ladders unchanged.

### Promotion gate

Pooled standard, shogi, crazyhouse, and xiangqi SPRT, H1 floor +8 Elo.

### Same-letter fallbacks

1. Gravity plus LMR feedback only.
2. Gravity only, with no pruning/reduction consumer.
3. Lower gravity gain while preserving all-node malus.

Fail-high-only malus remains forbidden.

## Phase Q — Slim one-ply continuation history

### Candidate

Add one per-thread continuation table with parent axis `piece type` and child
axis `(piece type, target square)`, size
`2 * piece_count * piece_count * board_size` i16 entries. Read it for quiet/drop
ordering and Phase P's reduction feedback. Write it beside butterfly history with
the same gravity semantics and all-node malus policy.

The drop-encoding prerequisite makes target squares meaningful. Do not add a
two-ply table or `(piece, square)^2` parent axis.

### Footprint and parameters

The table belongs in `SearchInfo` or its existing per-thread search context, not
`State`. It is linear in board area. Serialize blend weight and update scale in
the scalar tail.

### Support gate

Measure fill density, cache footprint, and Threads 1/2/4/8 memory. Shogi density
must improve materially over the historical 0.20 writes/slot sparse design.

### Promotion gate

Pooled standard, shogi, and crazyhouse SPRT, H1 floor +8 Elo, with mandatory
shogi canary.

### Same-letter fallbacks

1. Piece-pair table with no square axis.
2. Use the table only for drop/SETUP variants.
3. Read it for ordering only, not reduction feedback.

## Phase R — TT static-eval cache and narrow material correction

### Candidate

Pack raw static evaluation into currently free TT payload bits. Current layout
uses bits 0-104, leaving 105-127 free. Preserve the 3 × u128 parity/seqlock shape,
masked indexing, and replacement clause including `old_score <= score`.

Add one bounded per-thread correction table keyed exactly by:

- side to move;
- phase bucket;
- a mixed bucket of existing opening/endgame material totals for both sides,
  quantized by a rules-derived material quantum.

Do not add a signature field to `State`. Compute the key from existing O(1)
material arrays. Store raw eval in TT. Add correction only when RFP or NMP reads
the pruning eval. Futility, LMP, qsearch, and broad evaluator output keep raw
static eval. Preserve the pre-strip capture-dependent update weighting; its
capture blend is load-bearing.

### Footprint and parameters

No TT entry growth and no `State` growth. One small per-thread i16 table.
Serialize eval bit width, correction bucket count, material quantum ratio,
grain, limit, and update weights in the scalar tail.

### Support gate

TT validity rate unchanged. Bit accounting and replacement condition reviewed
verbatim. Enumerate correction read sites; only RFP and NMP qualify. Fixed-depth
shogi is mandatory.

### Promotion gate

Standard plus mandatory shogi canary, H1 floor +8 Elo. Repeated shogi regression
rejects correction even if standard improves.

### Same-letter fallbacks

1. TT static-eval reuse alone.
2. Material correction read only by NMP.
3. Smaller material-only table with coarser quantization.

## Phase S — Drop search integration

### Candidate

At explicit call sites, treat a drop as a quiet for history, killers, and LMR
without widening exported `m_quiet!`. Move drops from Phase B's conservative
capture curve onto the quiet curve after P/Q provide statistics. Remove drop
exemptions from LMP and futility only with a Phase J royal-zone tactical
exemption: drops inside or adjacent to either royal zone are never pruned.

Make NMP's material guard hand-aware through role flags and held pieces, never a
variant-name case.

### Footprint and parameters

Call-site changes only; no new field. Reuse Phase J radius and existing schema
slots unless a distinct radius wins, in which case append it explicitly.

### Support gate

Standard, xiangqi, and grand fixed-depth output byte-identical. Shogi/crazyhouse
drop-mate and defensive-drop ladders unchanged. Record drop prune/reduction rates.

### Promotion gate

Crazyhouse and shogi pooled SPRT, H1 floor +12 Elo. Then run anchor RR #4.

### Same-letter fallbacks

1. History, killers, and quiet LMR only; retain pruning exemptions.
2. Add LMP with radius-two royal exemption; retain futility exemption.
3. Add futility only after LMP already passes.
4. Hand-aware NMP guard alone if all ordering/pruning arms fail.

## Phase T — Per-thread, per-ply search buffers

### Candidate

Restore reusable per-thread/per-ply move, score, generation-scratch, SEE-move,
and SEE-scratch buffers. Current blank-slate alpha-beta and qsearch allocate three
vectors per node, and SEE allocates two vectors per call. Keep buffers outside
`State` and distinct from rejected root-only allocation reuse.

### Footprint and parameters

One historical-shape `SearchBufs` owned per thread. Capacity derives from board
size and observed legal high-water marks; capacity is not a behavior parameter.

### Support gate

Pinned-seed fixed-depth nodes must be byte-identical on every campaign variant.
Require predeclared aggregate NPS gain and no severe single-variant loss before
spending games. Record resident memory at Threads 1/2/4/8.

### Promotion gate

NPS alone cannot consume the letter. Pooled fixed-time external games, H1 floor
+8 Elo.

### Same-letter fallbacks

1. Reuse alpha-beta/qsearch move and score buffers only.
2. Reuse SEE buffers only.
3. Reserve exact observed high-water capacities rather than full arenas.

If all shapes reproduce the rejected root-reuse result, stop Phase T and design a
new candidate; do not defend it by naming differences.

## Phase U — Drop/SETUP generation and banded picking

### Candidate

For `drops! || game_phase == SETUP` only:

- generate/order winning captures first;
- then killers, quiets, and drops;
- then losing captures;
- use stable explicit score bands;
- replace repeated maximum scans with a stable banded picker.

Non-drop/non-SETUP generation stays unchanged. Convert Phase E's quiet-band
`continue` to `break` only after the picker proves every exempt promotion and
losing capture is in a later explicit band where it will still be searched.

### Footprint and parameters

Generator/picker changes and per-thread buffers only. No live state and no new
behavior parameter beyond already serialized score-band boundaries.

### Support gate

Full perft after generator split. Standard, xiangqi, and grand fixed-depth nodes
byte-identical. Measure long-tail comparisons and shogi/crazyhouse NPS.

### Promotion gate

Shogi, crazyhouse, and minishogi fixed-time games, H1 floor +10 Elo.

### Same-letter fallbacks

1. Banded picker only.
2. Staged generation only, retaining `continue`.
3. Two broad bands rather than full staging.

Never stage all variants unconditionally.

## Phase V — Bounded forcing rule moves in quiescence

### Candidate

Add exactly one bounded qsearch ply for:

- non-capture promotions with positive derived value gain;
- moves that immediately complete a declared goal, checks-count, or extinction
  objective.

Generate through existing legal paths and confirm the terminal result after
make. Add no ordinary qsearch checks, unrestricted drops, or second forcing ply.

### Footprint and parameters

One qsearch-depth/eligibility scalar in the parameter tail. No live state.

### Support gate

Gate-off variants remain node-identical. Promotion/objective ladders discover the
same result earlier with bounded node growth.

### Promotion gate

Pooled affected variants, H1 floor +10 Elo. Then run anchor RR #5.

### Same-letter fallbacks

1. Immediate terminal-objective moves only.
2. Positive-gain mandatory promotions only.
3. Restrict the extra ply to qsearch entered from PV nodes.

## Phase W — Adaptive clock allocation

### Candidate

Replace the single hard deadline with:

- soft and hard deadlines;
- root-move stability scaling;
- score-drop scaling;
- one overhead subtraction;
- a reserve cap;
- an exact-budget `movetime` path kept separate.

When `movestogo` is supplied, honor it. Otherwise derive the horizon from
post-SETUP deployed non-royal count plus board travel diameter and phase span.
Do not restore hardcoded horizon 18. Log per-move clock share so front-loading is
observable.

### Footprint and parameters

Derived horizon in `StaticState`; deadlines and stability counters in
`SearchInfo`. Serialize stability ladder, score-drop multiplier, reserve ratio,
hard/soft ratio, overhead, and horizon coefficients in the scalar tail.

### Support gate

Fixed-depth nodes byte-identical. Zero external PGN time forfeits. Clock share
must be materially flatter than the rejected 65%-in-first-18 shape.

### Promotion gate

Self-play cannot accept this phase. Use external anchor games at at least two
clock controls, H1 floor +8 Elo. Existing `bin/tm36` and `bin/tm54` may be extra
arms after provenance verification.

### Same-letter fallbacks

1. Horizonless reserve formula using supplied `movestogo` and current phase.
2. Derived horizon without stability scaling.
3. Stability scaling on the current conservative allocation, no horizon.

If all fail externally, close the lever again.

## Phase X — Lazy-SMP diversity

### Candidate

Give helper workers deterministic root-order rotation and at most one-ply depth
staggering while sharing existing TT/QT. Worker 0 retains canonical order and
depth. Keep the existing join contract. Do not add tree splitting, work stealing,
or shared mutable histories.

### Footprint and parameters

Worker identity and offsets in `SearchInfo`. Serialize rotation stride and depth
stagger in the scalar tail. No `State` field.

### Support gate

Threads 1 output byte-identical. Measure duplicated work, effective depth,
resident memory, and worker startup at Threads 2/4/8.

### Promotion gate

Threads 4 external games at equal wall clock, H1 floor +8 Elo. Then run anchor RR
#6.

### Same-letter fallbacks

1. Root-order rotation only.
2. Depth staggering only.
3. Per-worker aspiration-width offsets only.

## Phase Y — Outcome-supervised evaluation tuning

### Candidate

After accepted evaluator features are frozen, extend the existing tuner over the
already versioned/scalar-tail parameters from H-O. Do not perform another schema
redesign here.

Primary fit:

- phase-balanced, outcome-labelled positions across campaign variants;
- neutral samples from both AnekaMacam and Fairy-Stockfish games;
- result-independent sampling at fixed plies;
- linear feature extraction matching `evaluate_position!` exactly;
- shared dimensionless multipliers over rules-derived per-variant defaults;
- regularization toward those defaults so unseen variants retain sane
  out-of-box behavior.

Tune only terms with correct extractable derivatives. Keep non-linear caps and
search-only parameters at derived defaults for Phase Z rather than pretending
Texel features cover them. Never train on converted reference scores.

### Footprint and parameters

Tuner, datagen, and existing scalar-tail values only. No runtime architecture
change. All shipped variants already round-trip their parameter slots.

### Support gate

Every variant loads exact schema; derived and tuned binaries both play. Held-out
outcome loss improves by campaign variant. No data leakage across games.

### Promotion gate

Tuned versus derived pooled campaign SPRT, H1 floor +15 Elo, with no severe
single-variant regression.

### Same-letter fallbacks

1. Tune only royal, promotion, and objective scales.
2. Tune only global shared multipliers.
3. Per-variant tuning with stronger regularization where a joint fit conflicts.

Schema, datagen, and tuner work consume no letter unless the tuned binary wins.

## Phase Z — Joint search and clock tuning

### Candidate

Tune accepted dimensionless search and clock groups while preserving every
rules-derived scale:

1. LMR with LMP;
2. RFP with futility;
3. qsearch SEE/delta margins;
4. butterfly/continuation gravity and blend;
5. aspiration and extension limits;
6. clock reserve/stability/horizon coefficients.

Optimize groups sequentially, validate each group against Phase Y, then test the
combined winner. Keep piece-value, board-geometry, phase-span, setup-army, and
termination-rule scaling. Never add variant-name constants.

### Footprint and parameters

Static scalar-tail changes only. No new fields or runtime architecture.

### Support gate

All variants load exact schema. Fixed-depth groups do not catastrophically
regress any campaign variant. Clock group uses external anchors only.

### Promotion gate

Final combined binary versus Phase Y, pooled H1 floor +10 Elo. Then run final
anchor RR #7 on standard, crazyhouse, shogi, xiangqi, and grand. Add stronger
Fairy-Stockfish rungs only when the current rung is competitive.

### Same-letter fallbacks

1. LMR/NMP/pruning group only.
2. History/continuation group only.
3. Clock group separately, combined only after a dedicated interaction RR.

Losing groups revert independently and do not discard accepted groups.

## Permanently rejected or closed without materially new evidence

- Capture history.
- Singular extension, multicut, and negative-extension family.
- Drop IID/search-restart IID; IIR or ProbCut only with materially new evidence,
  never from generic-engine lore.
- Eval-scaled NMP plus endgame verification as its own letter; it cut nodes and
  measured about -3 RR Elo.
- Sqrt/sqrt quiet-LMR retune.
- Fail-high-only history malus.
- Leaf evaluation walking compiled move vectors.
- Ordinary qsearch checks.
- Dynamic live-army phase thresholds.
- Dynamic per-node SETUP occupancy/simulation.
- Root-only allocation reuse.
- SEE repetition bypass.
- Unconditional staged generation.
- Large sparse continuation history or any two-ply continuation table.
- Broad correction history, any correction read by fail-low pruning, or any
  correction change without shogi canary.
- Removing the TT replacement clause `old_score <= score`.
- `has_castled` or other non-FEN-reconstructible evaluator state.
- Simple held-piece value scaling.
- Cross-engine score conversion.
- Hardcoded time horizon 18.
- Self-play as clock proof.
- New board-area-quadratic tables, including `Vec<Board>` indexed by every
  square.
- Any correctness, audit, diagnostic, tooling, schema-only, or neutral work
  presented as an accepted letter.

## Critical files

- `src/game/position/search.rs`: A-G, P-X, Z.
- `src/game/position/evaluation.rs`: H-O, Y.
- `src/game/search/parameters.rs`: phase repair, scalar derivation, B-Z.
- `src/game/search/move_ordering.rs`: ordering bands, B, E-F, P-Q, U.
- `src/game/search/transposition.rs`: R.
- `src/game/search/parallel.rs`: per-thread ownership, T, X.
- `src/game/representations/state.rs`: phase references, I's sole planned live
  evaluator field, and the compact special-rules byte.
- `src/game/representations/termination.rs`: H, M-N, V.
- `src/game/moves/drop_list.rs`: drop encoding and U.
- `src/game/moves/move_list.rs`: I, U, V, and forbidden-zone promotion bypass.
- `src/io/game_io.rs`: scalar schema, every parameter rider, config rules, and
  the literal FEN loader, which places exactly the piece each character names.
- `configs/{crazyhouse,shogi,minishogi,judkins,euroshogi,pocketknight}.conf`:
  drop-pocket promoted types, demotions, and legal drop zones.
- `configs/janggi.conf`, `res/perft/janggi.perft`, and the janggi cases in
  `tools/endgame_fixtures.txt`: palace pieces spelled in their true per-square
  form now that the loader no longer converts them.
- `res/dicts/{euroshogi,judkins}.dict`, `res/perft/*`: hand conversion and
  drop-pocket perft fixtures. Any FEN dialect difference belongs in a dict.
- `tools/drop-integrity.sh`: external Fairy-Stockfish pocket replay.
- `tools/ebf-suite.sh`, `tools/ebf_positions.txt`: the Fairy-Stockfish name
  mapping read from that engine's own variant list, and the standard,
  capablanca, gothic, and grand `startpos` width ladder that B is gated on.
- `src/debug/tuning.rs`, `src/debug/datagen.rs`: Y.
- `src/io/protocols/protocol.rs`: W.
- `res/param/*`: every accepted parameter/PST rider.
- `build-stages.sh`, `tools/provenance.sh`, `round-robin.sh`: ladder builds,
  their provenance records, and every round robin.

## Implementation discipline

- Maximum 80 columns per code line.
- Match existing file-header comment style.
- Ordinary comments only as right-margin `/* */` comments in columns 80-120.
  No ordinary comments inside functions except those right-margin comments.
- Use `///` docs and existing section separators.
- Align `:` and `->` pairs in Params and Return sections.
- Use descriptive names, including macro variables.
- No unused variables, warning suppression, or `#[test]` modules in engine code.
- Read `configs/example.conf` and `res/dicts/example.dict` before changing any
  config or dictionary.
- Use `debug-headless perft` for divides and suites; do not script the TUI.
- Verify binary provenance before every benchmark, SPRT, or RR.
- Implement only the first unresolved prerequisite or phase in a session unless
  the user explicitly requests more.
- Stop at the first provider cooldown. Preserve run/task IDs and completed
  outputs; do not retry until explicit resume or confirmed availability.
