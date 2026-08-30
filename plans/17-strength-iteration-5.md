# Strength Iteration 5

## Status

**P0-P3 complete. P4 is next. No strength campaign has started.**

Source baseline: `64fbf9a` on `main`.

Target baseline after neutral prerequisites: `base-5`.

Fairy-Stockfish reference: commit
`6d9d0f5724677dc3aba3c577b0b482b6ec11e44a`, dated 2026-08-23.

Remote campaign host `upi@157.10.252.201` became reachable on 2026-08-28 after
VPS restart. Check returned `remote-ok`; promotion campaigns and long remote
perft may use it after local prerequisites finish.

Expected Elo bands below are priors, not additive promises. Every accepted phase
must independently beat previous accepted phase at its declared floor.

## Purpose

Build stronger engine for every expressible perfect-information,
alternating-turn, square-tile-board variant while removing complexity displaced
by iteration 4 into permanent diagnostics, duplicated payload scalars,
derivation-only runtime fields, and global constant fan-out.

Order:

1. freeze current evidence;
2. remove dead and duplicated architecture;
3. fix semantic search identity and gates;
4. improve hot-path search and move ordering;
5. improve drop and SMP search;
6. replace FIDE-shaped PST components with bounded rule-derived geometry;
7. add only two declaration-specific features with working tuner parity;
8. retune existing material values last.

Fairy-Stockfish supplies search mechanism shape and safety evidence. It does not
supply local evaluation numbers. No Fairy-Stockfish bonus, margin, piece value,
or named-variant exception may be copied.

## Iteration-4 evidence and exclusions

Binding results:

- PVS, four-surface LMR, aspiration/mate clipping, hoisted evaluation with
  improving/RFP, frontier pruning, disciplined qsearch, and confinement-gated
  shelter gained roughly +307 pooled Elo.
- Check-extension fallback stalled at +3.6 Elo against +8 floor.
- Material-sensitive draw scoring lost badly in xiangqi and standard.
- Shelter plus cover and cover alone failed; shelter alone won only after a
  rule-derived confinement gate.
- Singular/multicut search previously lost about 19 Elo.
- Evaluation-scaled verified null move previously lost about 3 Elo.
- Capture history was worthless.
- Ordinary qsearch checks remain closed.
- All-node history malus remains load-bearing.
- Ordinary non-drop deferred generation lacks supporting speed evidence.
- Adaptive clock front-loading had wrong sign and weak prior evidence.
- Compound development, coordination, breakthrough, move-limit pressure, broad
  royal danger, generic mobility, and hand-priced bonus piles lack separable
  evidence.
- ProbCut stays omitted. Pinned upstream shape is better, but upstream prior is
  around +4 Elo, below iteration-5 floor, with no contrary local proof.

## Verdict and commit law

- One accepted strength phase per commit, in letter order.
- Terminal H1 at declared floor promotes.
- Terminal H0 with negative estimate rejects candidate and starts first
  same-letter fallback.
- Inconclusive extends and blocks letter. Budget exhaustion is not rejection.
- Failed, abandoned, reverted, correctness, cleanup, and tooling work consume no
  letter.
- If all fallbacks reject, letter remains open until new evidence supplies a
  candidate. Later letters do not advance around it.
- Revert complete candidate before starting fallback.
- No rejected helper, field, flag, constant, payload token, table, counter, or
  support output survives.
- One logical neutral prerequisite per unlettered commit when needed.
- No commit contains `Co-Authored-By`.
- Update this plan immediately after every support result, campaign boundary,
  commit, rejection, rollback, or blocker.

## Architecture law

1. No variant names or protocol-dialect concepts in engine behavior.
2. `StaticState` stores final runtime products, not startup inputs already
   consumed by derivation.
3. A constant or static used by one file stays private in that file. A value
   shared across files is defined once in `prelude.rs`.
4. `prelude.rs` is the registry for genuinely cross-file constants and statics,
   not single-file implementation details. Do not create `constants.rs`.
5. Payload contains values current runtime consumes and current tuner moves:
   material plus PST residuals only after P2.
6. Rule-derived base PST and tuned residual PST stay separate. Tuning cannot
   silently erase accepted geometric components.
7. Each phase adds at most one persistent runtime state family. Every new table
   states exact worst-case byte formula and ceiling per worker.
8. Temporary counters exist only in throwaway support diffs and leave before
   promotion build.
9. Search gates derive once into one compact capability mask. Do not spread
   descriptor booleans through `StaticState`.
10. Hot paths do not parse rules, derive movement graphs, or allocate per node
    when bounded worker/per-ply reuse suffices.
11. Structural evaluation replaces an existing component or adds one
    declaration-specific feature with exact tuner parity. No generic bonus pile.
12. Unaffected variants require fixed-depth identity. Gate-negative correctness
    changes compare against explicit conservative reference, not unsafe baseline.
13. Use current commands and scripts only. Add no benchmark, SPRT, perft,
    agreement, tuning, or provenance wrapper.
14. Never run `cargo fmt`; preserve repository formatting manually.

## Critical files

- `plans/17-strength-iteration-5.md`
- `src/prelude.rs`
- `src/game/position/search.rs`
- `src/game/position/hash.rs`
- `src/game/position/evaluation.rs`
- `src/game/search/move_ordering.rs`
- `src/game/search/transposition.rs`
- `src/game/search/parallel.rs`
- `src/game/search/parameters.rs`
- `src/game/moves/move_list.rs`
- `src/game/moves/drop_list.rs`
- `src/game/representations/moves.rs`
- `src/game/representations/state.rs`
- `src/game/representations/termination.rs`
- `src/io/game_io.rs`
- `src/io/protocols/protocol.rs`
- `src/debug/headless.rs`
- `src/debug/datagen.rs`
- `src/debug/tuning.rs`
- `res/param/*/latest.param`

## Unlettered prerequisites

All prerequisites land and pass checks before `A-5`. They claim no Elo.

### P0. Freeze `64fbf9a` evidence

Record HEAD, compiler/build mode, seed, Hash, threads, TT/QT size, derive output,
payload checksums, `content_md5`, fixed-depth signatures, perft, endgame, drop,
FEN round-trip, agreement, EBF, and speed evidence. Use only:

- `tools/provenance.sh`
- `tools/ebf-suite.sh`
- `tools/speed-suite.sh`
- `tools/agree-suite.sh`
- `tools/run_endgame_fixtures.sh`
- `tools/drop-integrity.sh`
- `tools/run_fen_roundtrip.sh`
- existing `debug-headless` commands

This snapshot proves neutral cleanup did not change policy.

#### P0 evidence, 2026-08-28

Status: complete. Correctness, identity, bounded perft, EBF, agreement, and
speed evidence is frozen. Detached full perft is informative only.

Build and provenance:

- HEAD: `64fbf9a609d78c38e1f72e62f93e8279bdf1c421` on `main`.
- Source baseline remains roadmap baseline `64fbf9a`.
- Build: `cargo build --release`, default features, no warnings.
- Compiler: `rustc 1.97.0-nightly (e96c36b6f 2026-05-21)`.
- Cargo: `cargo 1.97.0-nightly (4d1f98451 2026-05-15)`.
- Host: macOS 26.5.1, build 25F80, arm64.
- Binary: `target/release/anekamacam`, 5,515,152 bytes.
- Binary MD5: `2e918bfd9f186178c9fd754896d617ab`.
- `content_md5`: `8d1da2c0fdbb4adb8002b76c6d5544c9`.
- Config tree MD5: `c09f443db4fe35a46cec187ed619c637`.
- Dictionary tree MD5: `39bee9589de36c8d7562b8c42c09af44`.
- UCI option MD5: `f3f291236fa49c71c5569f2beec47411`.
- Embedded variants: 38; embedded resources clean.
- `tools/provenance.sh verify` rebuilt `64fbf9a` and matched
  `content_md5` exactly.
- Pre-existing `search.rs` diff changes only comment alignment at line 634.
  It remained untouched and does not change rebuilt binary content.

Deterministic search settings:

- Fixed-depth anchors: `ANEKAMACAM_SEED=42`, Threads 1.
- `debug-headless search` uses default Hash 256 MB.
- Default split: nominal TT 170 MB and QT 85 MB.
- Target entries are 64 bytes. Power-of-two capacities are 2,097,152 TT
  entries, 1,048,576 QT entries, and 192 MiB actual retained slots.
- EBF and agreement use explicit matched Hash 64 MB: 524,288 TT entries
  and 262,144 QT entries, 48 MiB actual retained slots.

Derivation and payload:

- `debug-headless derive`: 38/38 configs derived.
- Derive output MD5: `dba3a046d66a533729883c33829a0f60`.
- Payload manifest: 38 files.
- Payload manifest MD5: `aea926e3e35849ae097fbee69ec8b6a6`.

```text
ai-wok         b54d647c39e711fc75c6ba419654b23a
almost         3c2bed701e20c2850d44ab04b1fabfb2
amazon         9edd6eee8eeef9661bd83f5b1c9fc6cf
asean          3210a0862053fe094743efbbf14bc320
berolina       d65a7445f15c5a84bf770561277bc297
capablanca     695903aca2013f121af9d6625dc1dec5
chancellor     24e51bb4e4750c4147bc567fcd244110
chigorin       67cbcd8aa849261a157f7742949d9dea
crazyhouse     0597b10dfe9129087cf574f7592c033c
embassy        9c8689d5edb1e0d0fca26ce184d3d23c
euroshogi      a62b54362e2bdd9bf5a57877e4aa8d87
extinction     e13313b980e7b42f0cacb913da03976d
fivecheck      7fd9c9ac0c29eb225a0c83aaf5118d81
gothic         9c8689d5edb1e0d0fca26ce184d3d23c
grand          9ed36b2f8c9433bbf101930edd3deebc
hoppelpoppel   5cda55c9746d7e94efee18f9fe0cd6fb
horde          9c14aafeeea9c3a39d627c78fe4176d7
janggi         78ffc24d3c2550cb8a1361cfea9ab8f7
janus          2bee120e1a0c7a54555367b6a17a9e35
judkins        19529a7c399d0ab868b94245ec83faae
kinglet        1f026288e1d0a169dced6635443518e5
knightmate     ee7cc56a6da077c012d051245f75e439
koth           7fd9c9ac0c29eb225a0c83aaf5118d81
los-alamos     6aac59ecf474920b84cd939198d9e2e5
makruk         66eebab43b5e12f37bb3a94beaba06e8
minishogi      d8587a5b90f7a34b0dd1f84f31a84b50
minixiangqi    c798ab94478ceebfc060a439e03cb613
modern         db5d7741cff227e128d66de00bc87f62
newzealand     e86d4e69f42acf374379fcd077e5a1c1
ouk-chaktrang  c5becc75828e511e48e883db4b9632d5
pocketknight   0d5d3f3bb1c3c2a3dc65453362d074a3
shatranj       2e3877f6758c378f3d5ebfc981511765
shogi          4a81034232ff6baf905952b7be78f170
sittuyin       fc894891cdaa5cf49c781163c2a14877
standard       7fd9c9ac0c29eb225a0c83aaf5118d81
threecheck     7fd9c9ac0c29eb225a0c83aaf5118d81
tjatoer        f546c8215ce77b0f44943fa2f199a6a0
xiangqi        b665c388c58d1d85cbbc3cc1a3442f9b
```

Correctness support:

- Endgame fixtures at depth 6: 38 passed, 0 failed.
- FEN round trip: 44 passed, 0 failed, 0 skipped.
- Drop integrity: 0 mismatching games of 12, depth 5, 200 plies,
  0.03 seconds per move.

B10 fixed-depth signatures at depth 6:

- `standard`: best `c2:c4`, score 5, nodes 4,359.
  PV `c2:c4 b8:c6 b1:c3 g8:f6 g1:f3 d7:d5`.
- `shatranj`: best `b1:c3`, score 0, nodes 1,894.
  PV `b1:c3 b8:c6 g1:f3 g8:f6 c1:e3 c8:e6`.
- `grand`: best `b2:c4`, score 3, nodes 10,747.
  PV `b2:c4 i9:h7 h3:h5 b9:c7 i2:h4 a8:a6`.
- `xiangqi`: best `g1:e3`, score 12, nodes 17,108.
  PV `g1:e3 h8:e8 h1:f2 b10:c8 b3:c3 b8:b3`.
- `janggi`: best `Q@e2`, score 0, nodes 456.
  PV `Q@e2 q@e9 E@c1 e@c10 E@g1 e@g10`.
- `shogi`: best `c1:d2`, score 0, nodes 3,617.
  PV `c1:d2 c9:d8 g1:f2 f9:e8 f1:e2 g9:f8`.
- `crazyhouse`: best `b1:c3`, score 1, nodes 3,331.
  PV `b1:c3 b8:c6 g1:f3 g8:f6 e2:e4 a7:a5`.
- `koth`: best `c2:c4`, score 5, nodes 4,359.
  PV `c2:c4 b8:c6 b1:c3 g8:f6 g1:f3 d7:d5`.
- `threecheck`: best `c2:c4`, score 5, nodes 4,353.
  PV `c2:c4 b8:c6 b1:c3 g8:f6 g1:f3 d7:d5`.
- `extinction`: best `g1:f3`, score 0, nodes 3,267.
  PV `g1:f3 b8:c6 d2:d4 d7:d5 b1:c3 g8:f6`.

EBF suite, seed 42, Hash 64 MB, one thread:

- Log MD5: `30711d0a5ec93fecd76cc2d506a57509`.
- `standard@13`: 9 cases, geometric nodes 193,667, EBF 1.762.
- `crazyhouse@13`: 9 cases, geometric nodes 2,272,814, EBF 1.918.
- `shogi@11`: 4 cases, geometric nodes 158,761, EBF 1.873.
- `xiangqi@12`: 4 cases, geometric nodes 48,075, EBF 1.563.
- `capablanca@10`: 1 case, nodes 111,027, EBF 2.316.
- `gothic@10`: 1 case, nodes 102,139, EBF 2.325.
- `grand@10`: 5 cases, geometric nodes 84,546, EBF 1.849.
- Crazyhouse-mid / standard node ratio at depth 13: 13.68x.
- Suite exited 0; complete per-case output is identified by log MD5.

Agreement suite against `/opt/homebrew/bin/fairy-stockfish`, seed 42,
Hash 64 MB, one thread:

- Log MD5: `ba6753ed64e2ba3139c167d94a3256fd`.
- `standard`: 9 cases, median gap -53, 0 sign flips, 0 blind losses.
- `crazyhouse`: 21 cases, median gap +844, 5 sign flips,
  6 reference-sees-lost cases missed locally.
- Suite exited 0; these values are baseline evidence, not promotion gates.

Speed suite, seed 42, one thread, 10 passes per variant:

- Log MD5: `b40fe4cbfdb270809cea2c4e0c1764a2`.
- `standard`, depth 11, 16 positions: 119,679 nodes, 3,778,396 NPS.
- `shogi`, depth 8, 16 positions: 888,112 nodes, 1,723,892 NPS.
- `crazyhouse`, depth 8, 16 positions: 813,213 nodes, 1,312,672 NPS.
- `xiangqi`, depth 9, 16 positions: 223,320 nodes, 932,396 NPS.
- `grand`, depth 9, 16 positions: 1,346,591 nodes, 787,947 NPS.
- Node counts matched across all ten passes; suite exited 0.

Perft evidence and detached long run:

- Remote standard depth-6 suite recorded 249 passed depth rows, 0 failed,
  covering 41 complete positions and depths 1-3 of position 42.
- `logs/latest.log` then reached its 106,496-byte cap. Engine continued at
  99.9% CPU, but exact later position is unavailable from capped log.
- Extrapolated full 6,752-position depth-6 run is roughly 20 days and cannot
  block roadmap execution. User directed on 2026-08-29: do not kill remote run;
  leave it detached and continue P0 with bounded local perft evidence.
- Remote baseline worktree remains `~/p0-64fbf9a`, runner PID 2516, standard
  perft PID 2519 at last inspection. Completion monitor remains attached.

Bounded perft, seed 42, depth 4:

- Log MD5: `a4872e8f5f19141489c3c7cfb7fbc38e`.
- `standard`: first 64 positions, 256/256 depth rows passed.
- `crazyhouse`: 16/16 passed.
- `euroshogi`: 16/16 passed.
- `janggi`: 28/28 passed.
- `judkins`: 12/12 passed.
- `minishogi`: 12/12 passed.
- `minixiangqi`: 4/4 passed.
- `pocketknight`: 12/12 passed.
- `shogi`: 16/16 passed.
- `sittuyin`: 16/16 passed.
- `xiangqi`: 44/44 passed.
- Initial local depth-6 nonstandard run was stopped before a final summary after
  first crazyhouse fixture proved similarly unbounded. No result is claimed from
  that stopped run. Remote full standard run remains untouched as directed.

P0 result: accepted as neutral baseline evidence. Its documentation commit also
adds this previously untracked approved roadmap; no source change is included.

### P1. Remove permanent diagnostics

Delete all fourteen iteration-4 support counters from `SearchInfo` and their
reporting. Keep limits, stop state, protocol node count, thread count, PV,
history, and actual runtime state. Future counters remain temporary.

Touch:

- `src/game/position/search.rs`
- `src/game/search/parallel.rs`

Proof: debug/release build without warning suppression; pinned-seed nodes, score,
best move, PV, and protocol output unchanged.

#### P1 evidence, 2026-08-29

Status: complete.

- Removed all fourteen permanent diagnostic fields, resets, increments, and
  five reporting blocks from `SearchInfo` and iterative search.
- Kept limits, interrupt state, total protocol node count, thread count, PV,
  history, killers, evaluation stack, and table ages unchanged.
- `parallel.rs` had no diagnostic field or reporting use; its constructor uses
  `Default` and required no source edit.
- Debug and release builds completed without warnings or suppression.
- B10 depth-6 nodes, score, best move, and PV match P0 exactly.
- Semantic signature MD5:
  `5f7424ecea84734a3c48db0c6138ed8c`.
- Host-normalized UCI handshake matches P0 exactly. Normalized MD5:
  `6d09737f7b323b00b20b98f3f08a925f`.
- Runtime log contains zero removed diagnostic report labels.
- Pre-existing comment-alignment change in `search.rs` remains preserved but
  outside P1 candidate staging.

P1 result: accepted as neutral cleanup. No playing-policy code changed.

### P2. Replace scalar payload with material and PST residuals

Delete rejected cover evaluation, `cover_squares`, `cover_counts`, `cover_value`,
cover constants, and cover payload slots. Delete:

- identical 40-token scalar tail from all 38 payloads;
- `PARAM_SCALAR_COUNT`;
- `apply_scalar_parameters`;
- `scalar_parameter_tokens`;
- scalar parsing/export and mirrored tuning passthrough;
- serialized opening/endgame phase thresholds;
- serialized big/major role flags;
- all derivation-only scalar shadows in `StaticState`.

New payload shape contains only:

1. opening material values;
2. endgame material values;
3. opening PST residual rows;
4. endgame PST residual rows.

Rule-derived base PST and tuned residual stay separate:

`final PST = derived base PST + payload residual PST`.

Migrate every current full PST row to residual form by subtracting current derived
base, proving final PST byte identity. After material loads, derive phase
thresholds and roles from loaded values. Then derive search margins, reduction
surfaces, shelter, and final PST. This gives later U/V/Y phases one explicit
post-load derivation point and prevents payload from overwriting their geometry.

Keep only runtime products read during play: final material/roles/phases, final
PST, reduction surfaces, margins/counts, SEE allowances, qsearch delta, compact
masks/lists, shelter squares, and accepted shelter value.

Read `configs/example.conf` and `res/dicts/example.dict` first.

Touch:

- `src/io/game_io.rs`
- `src/game/search/parameters.rs`
- `src/game/representations/state.rs`
- `src/game/position/evaluation.rs`
- `src/debug/tuning.rs`
- `res/param/*/latest.param`

Proof: every payload parses and round-trips; every config derives; loaded material,
roles, phases, final PST, shelter, evaluation, and fixed-depth search match P0;
no cover, scalar-tail, phase-threshold, or role-flag token remains.

#### P2 evidence, 2026-08-29

Status: complete.

Payload migration:

- All 38 payloads now contain opening material, endgame material, opening PST
  residual rows, and endgame PST residual rows only.
- Shape validation proved each new token count from its piece-type count and
  board size. Shape manifest MD5:
  `6b1d06a5fe58db671c0aede770f05573`.
- Opening and endgame material blocks match P1 payload bytes for all variants.
- Residuals were generated as old final PST minus `derive_base_pst` under loaded
  material. Final parser uses that same base helper and adds residuals after
  post-load derivation.
- Engine-backed export of all 38 loaded payloads reproduced every payload byte.
  Payload and round-trip manifest MD5:
  `62ec66f6a3ffc7fd78f79fc570694e11`.
- All 38 previous scalar tails were identical before removal. Cover ratio and
  floor were `0 0`, so deleting rejected cover evaluation changes no score.

Architecture removal:

- Deleted 40-token scalar parser/export, scalar-count constant, phase
  thresholds, role flags, and all derivation-only scalar shadows from
  `StaticState`.
- Search now reads universal depth and gate constants directly. Derived
  surfaces, margins, counts, SEE allowances, and qsearch delta remain runtime
  products.
- Material loads before roles, phase thresholds, PST base, search parameters,
  shelter, and final incremental evaluation refresh.
- Deleted cover constants, fields, tables, derivation, and evaluation term.
- Tuner exports final PST targets as residuals against bases rederived under
  tuned material. No removed scalar, phase, or role passthrough remains.
- Removal search found zero forbidden scalar-tail or cover symbols.

Verification:

- Debug and release builds completed without warnings or suppression.
- `debug-headless derive`: 38/38 configs, output MD5 unchanged at
  `dba3a046d66a533729883c33829a0f60`.
- All 38 start-position phases and evaluations match P0 exactly. Manifest MD5:
  `a8dc4452a764d4010fead0cfdfdb8ef9`.
- B10 depth-6 nodes, score, best move, and PV match P0 exactly. Semantic MD5:
  `5f7424ecea84734a3c48db0c6138ed8c`.
- Bounded perft matches P0 exactly: MD5
  `a4872e8f5f19141489c3c7cfb7fbc38e`.
- Endgame fixtures: 38 passed, 0 failed.
- FEN round trip: 44 passed, 0 failed, 0 skipped.
- Drop integrity: 0 mismatching games of 12.
- `search.rs` and `prelude.rs` changed beyond original touch list only because
  runtime scalar-shadow readers and `PARAM_SCALAR_COUNT` had to disappear.
- Pre-existing `search.rs` comment alignment remains outside P2 staging.

P2 result: accepted as neutral payload and derivation cleanup. Final PST and
playing policy remain identical to P0.

### P3. Separate private and shared constants

User-directed architecture revision, 2026-08-29: visibility follows actual use,
not subsystem category.

- A constant or static used in one file stays private beside that behavior.
- A constant or static used by multiple files is defined once in `prelude.rs`.
- Do not use mismatched `pub`, `pub(crate)`, and private copies for one value.
- Do not create `constants.rs` or duplicate shared values beside each caller.
- Embedded resources follow the same rule: private when one file reads them,
  shared through prelude when multiple files read them.
- Keep wildcard imports where they still carry broad engine vocabulary. Remove
  only imports made redundant or unused by this separation.

This revision replaces the earlier owner-local P3 wording. It does not change
runtime values, payload bytes, or behavior.

Touch `src/prelude.rs`, files retaining private constants/statics, and direct
callers whose imports change.

Proof: every constant/static has one definition; single-file values are private;
cross-file values live in prelude; no duplicate or unused import; all builds,
protocols, derive, evaluation, perft, and fixed-depth signatures remain identical.
Payload builds use clean resource recompilation, not source edits for mtime.

#### P3 evidence, 2026-08-29

Status: complete.

Ownership:

- Audited 170 constant and static definitions; duplicate definitions: 0.
- Public constant/static definitions outside `prelude.rs`: 0.
- Private constants/statics referenced from another file: 0.
- Single-file tuner, SPRT, protocol, parser-regex, payload-resource, logger,
  archive, graphics, derivation, and search-gate values remain private.
- Cross-file values and macro expansion dependencies live once in prelude.
- `EMBEDDED_PARAMS` remains private to `game_io.rs`; shared config, dictionary,
  and perft resources live in prelude.
- `INDEX_TO_CARDINAL_VECTORS` became a shared constant rather than a public
  file-local lazy static. Other parser regex/maps became private.
- No `pub(crate)` constant/static visibility remains.

Verification:

- `cargo check`, debug build, and release build completed without warnings or
  suppression.
- UCI, USI, and UCCI handshakes match P0 after normalizing host thread maximum.
  Protocol manifest MD5: `1b5698fad5b7f19e7a2b215e7ff6cb1c`.
- `debug-headless derive`: 38/38 configs, output MD5 unchanged at
  `dba3a046d66a533729883c33829a0f60`.
- All 38 start-position phases and evaluations match P0 exactly. Manifest MD5:
  `a8dc4452a764d4010fead0cfdfdb8ef9`.
- B10 depth-6 nodes, score, best move, and PV match P0 exactly. Semantic MD5:
  `5f7424ecea84734a3c48db0c6138ed8c`.
- Bounded perft matches P0 exactly: MD5
  `a4872e8f5f19141489c3c7cfb7fbc38e`.
- FEN round trip: 44 passed, 0 failed, 0 skipped.
- Release resource embedding rebuilt through actual owner/prelude source
  changes; no unrelated source mtime edit was used.
- Pre-existing `search.rs` comment alignment remains outside P3 staging.

P3 result: accepted as neutral ownership cleanup under user-directed
private/shared rule. Runtime values, payload bytes, and playing policy remain
unchanged.

### P4. Separate canonical, search, and qsearch identity

Keep canonical `position_hash` unchanged for repetition matching. Add search-only
key mixing for state or path context that changes future legal moves, terminal
truth, or repetition interpretation:

- delivered-check counts;
- counter clock and active limit;
- counting progress and frozen limit;
- pass/double-pass, stand-off, or adjudication progress when represented;
- current declared-repetition occurrence count;
- bounded scan-state classification;
- perpetual-check/chase classification;
- any further demonstrated mutable termination context.

When path context cannot be represented compactly and exactly, disable TT/QT reuse
for that node. Synthetic null moves never enter repetition/perpetual context.
Do not add per-variant random tables to `StaticState`.

QTable key additionally carries qsearch move-set class. Any future filtered
recapture tail also carries recapture target, or disables QTable there.

Touch:

- `src/game/position/hash.rs`
- `src/game/position/search.rs`
- `src/game/search/transposition.rs`
- `src/game/representations/state.rs`
- `src/game/representations/termination.rs`

Proof: same board with different search-relevant progress/path gets different
search key while canonical repetition identity stays correct; make/undo restores
both; TT/QT-on and off fixtures agree.

### P5. Honor declared repetition and perpetual outcomes

Remove neutral return based merely on total `repeats >= 2`. Declared occurrence
threshold and declared repetition/perpetual outcome decide game score. A bounded
search-cycle guard may stop only an active ancestor cycle before threshold; it is
not adjudication, does not enter TT/QT, and does not use game-history twofold as a
shortcut.

Touch:

- `src/game/position/search.rs`
- `src/game/representations/termination.rs`

Proof: twofold game history remains searchable when declaration requires more;
threshold draw/win/loss agrees with `game_outcome`; shogi, minishogi, janggi, and
xiangqi perpetual fixtures agree; synthetic null history is absent.

### P6. Derive compact search capabilities

Derive one compact pre-game mask independently gating:

- ordinary threshold SEE;
- SEE-based pruning;
- forward pruning;
- null-move pruning;
- recapture prioritization/filtering;
- quiet/drop pruning and reductions;
- occupancy-independent movement graph.

Conservatively inspect mandatory capture, misère outcome direction,
check-forbidden behavior, explosive/multi-capture, extinction, petrification,
royal capture, flying-general/stand-off, cannon/hopper/screened movement,
capture-only and path-dependent multi-leg movement, drops, SETUP, and
pass-sensitive counters/counting/checks/repetition.

Touch:

- `src/game/search/parameters.rs`
- `src/game/representations/state.rs`
- `src/game/representations/termination.rs`
- `src/game/position/search.rs`
- `src/game/search/move_ordering.rs`

Proof: derive output records mask for every shipped config. Capability-enabled
ordinary variants retain baseline behavior. Capability-disabled variants compare
against an explicit pruning-disabled reference and declared-rule fixtures, not
unsafe baseline nodes. No variant-name branch.

### P7. Correct TT cutoff scope

Main-TT early bound cutoff applies only at non-PV nodes. PV nodes may reuse move,
static evaluation, and metadata. Eager terminals precede TT reuse. Preserve
replacement policy absent separate proof; preserve mate encoding.

Touch:

- `src/game/position/search.rs`
- `src/game/search/transposition.rs`

Proof: TT-on/off terminal and PV fixtures agree; root score/best move stable;
search-context keys never cross.

### P8. Exclude incomplete SMP results

Add completed depth to `SearchResult`. Partial iteration never counts. Select
greatest completed depth deterministically, then canonical worker-zero tie-break.
Do not group root moves or vote here; voting is S-5 strength work.

Touch:

- `src/game/position/search.rs`
- `src/game/search/parallel.rs`

Proof: one-thread identity; unfinished shallow/high score cannot beat completed
deeper result; shorter mate and longer loss ordering remains correct; deadline
and node accounting stay bounded.

### P9. Freeze `base-5` and capability ledger

Repeat P0 evidence. Record prerequisite commits, `content_md5`, and derived
capability result for every shipped configuration. This ledger defines every
affected-variant audit and every identity control. Every strength arm compares
against previous accepted phase, never directly against `64fbf9a`.

## Campaign protocol

### Support mode

- Pin `ANEKAMACAM_SEED`; match Hash; one thread except SMP phases.
- Verify `content_md5`, not raw binary MD5.
- Run affected perft, endgame, drop, FEN, EBF, speed, and agreement checks.
- Use multi-position suites, not one opening.
- Temporary counters leave before promotion binary.
- Support may veto. It cannot promote.
- Gate-false controls remain support-only and never dilute game SPRT.

### Game mode

- Unset `ANEKAMACAM_SEED`.
- Match Hash, threads, time control, openings, and colors.
- Use built-in `debug-headless sprt`; add no wrapper.
- Pentanomial H0 = 0, H1 = phase floor, alpha = beta = 0.05.
- Start 12,000 games for +8 to +15 floor; 18,000 where tuning variance or draw
  rate warrants it.
- Extend inconclusive result in 6,000-game blocks with unchanged binaries,
  openings, time control, pool, and SPRT state.
- Representative affected pool decides first. After pooled H1, audit every
  capability-positive shipped configuration from P9 ledger.
- Any terminal negative affected-variant result rejects candidate.

### Representative affected pools

- **B10 broad search:** `standard`, `shatranj`, `grand`, `xiangqi`, `janggi`,
  `shogi`, `crazyhouse`, `koth`, `threecheck`, `extinction`.
- **D8 drop/SETUP:** `crazyhouse`, `euroshogi`, `judkins`, `minishogi`,
  `pocketknight`, `shogi`, `sittuyin`, `janggi`.
- **P6 promotion:** `standard`, `grand`, `shogi`, `minishogi`, `euroshogi`,
  `sittuyin`.
- **C2 checks:** `threecheck`, `fivecheck`.
- **G1 goal:** `koth`.
- **E3 extinction:** `extinction`, `kinglet`, `horde`.
- **R5 royal-home:** `standard`, `capablanca`, `xiangqi`, `janggi`, `shogi`,
  filtered by derived confinement/home gate.
- **V7 final material:** `standard`, `xiangqi`, `shogi`, `crazyhouse`, `koth`,
  `extinction`, `horde`.

Names define campaign composition only. Engine behavior uses declarations.

## Phase status

| Phase | Candidate                       | Status   |
| ----- | ------------------------------- | -------- |
| A-5   | threshold SEE                   | proposed |
| B-5   | reusable buffers                | proposed |
| C-5   | staged sorted picker            | proposed |
| D-5   | deferred drop/SETUP generation  | proposed |
| E-5   | fail-soft propagation           | proposed |
| F-5   | complete QTable bounds          | proposed |
| G-5   | QTable probe before evaluation  | proposed |
| H-5   | TT static-evaluation cache      | proposed |
| I-5   | gravity reward                  | proposed |
| J-5   | compressed continuation history | proposed |
| K-5   | countermove                     | proposed |
| L-5   | strong-history LMR relief       | proposed |
| M-5   | pruning-only correction history | proposed |
| N-5   | internal iterative reduction    | proposed |
| O-5   | non-capture qsearch promotions  | proposed |
| P-5   | recapture-first qsearch         | proposed |
| Q-5   | first-class drop ordering       | proposed |
| R-5   | drop LMR                        | proposed |
| S-5   | completed-depth root voting     | proposed |
| T-5   | deterministic SMP diversity     | proposed |
| U-5   | promotion-graph PST             | proposed |
| V-5   | goal-zone PST redistribution    | proposed |
| W-5   | tuned check progress            | proposed |
| X-5   | tuned extinction exposure       | proposed |
| Y-5   | side-derived royal-home PST     | proposed |
| Z-5   | material-only retune            | proposed |

## Strength phases

Expected bands are priors. Low-confidence prior below floor is stated explicitly;
such candidate must clear floor or consume no letter.

### A-5 — Threshold early-exit SEE

**Primary / expected.** Replace exact SEE at threshold-only callers with
`see_ge(move, threshold)` following pinned Fairy-Stockfish exchange shape.
Expected +10 to +25 Elo.

**Touch / derive.** `src/game/search/move_ordering.rs`,
`src/game/position/search.rs`. Current piece values/attacker order; P6 ordinary
exchange capability; no payload.

**Support.** Differential temporary build compares `see_ge(m, t)` with
`see(m) >= t` over legal captures and thresholds on B10 plus unsafe families;
perft, SEE time, NPS, gate-off conservative behavior.

**Promotion.** B10 capability-positive subset, floor +8, 12,000 games.

**Fallbacks.** Current make/undo loop with early threshold proof; qsearch/pruning
only; `see_ge(0)` capture partition only.

**Rollback.** Remove predicate, scratch changes, callers, and capability use.

### B-5 — Bounded reusable search buffers

**Primary / expected.** Reuse per-ply move, score, and scratch buffers outside
`State`, retaining at most 64 `Move`, 64 `usize`, and 32 `u64` slots per ply.
Overflow may grow for current node but is released back to retained ceiling.
Expected +8 to +18 Elo from allocator reduction.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`, `src/game/search/parallel.rs`. Exact retained
ceiling per worker:

`MAX_DEPTH * (64 * size_of::<Move>() + 64 * size_of::<usize>() + 32 * size_of::<u64>())`.

Record evaluated byte count for target build before implementation. No board-sized
scratch in `State`.

**Support.** B10 nodes/score/PV/best move/SEE identity; no allocation after warmup
for nodes within ceiling; overflow release; repeated-search RSS bound; NPS.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Search/qsearch buffers only; SEE/LVA only; one worker arena with
bounded live-ply slices.

**Rollback.** Remove buffer family and all constructor/signature changes.

### C-5 — Staged one-sort move picker

**Primary / expected.** Partition full legal list into TT, good captures, killers,
scored quiets, and bad captures. Score each active band once and sort that band
once with deterministic tie-break; no repeated maximum scan inside a band.
Expected +8 to +20 Elo.

**Touch / derive.** `src/game/search/move_ordering.rs`,
`src/game/position/search.rs`. Move flags, A-5 threshold SEE, killers/history,
mandatory-capture gate.

**Support.** Every legal move once; deterministic sequence; TT first; bad captures
after quiet refutations; count score evaluations and comparisons; prove
O(n log n) band selection bound; perft and picker time.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Capture bands only; qsearch only; stable insertion sort for short
bands.

**Rollback.** Restore `score_move!`/`pick_by_score!`; remove picker state/bands.

### D-5 — Deferred drop and SETUP generation

**Primary / expected.** After C-5, defer drop/SETUP generation until its exact
picker-stage boundary. A TT drop/placement is directly validated or triggers
immediate relevant generation. Checking/high-history drops never wait behind a
lower-priority board-quiet stage. Ordinary non-drop variants keep full generation.
Expected +10 to +30 Elo on drop/setup trees.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`, `src/game/moves/move_list.rs`,
`src/game/moves/drop_list.rs`. Drop/SETUP, mandatory capture, castling, TT move,
and occupancy rules.

**Support.** Early and deferred lists partition full legal list exactly; TT drop
legality; forcing-drop stage order; all D8 perfts and `tools/drop-integrity.sh`;
gate-false identity; time-to-depth.

**Promotion.** D8, floor +8, 12,000 games.

**Fallbacks.** Defer drops only; defer after one good capture; SETUP only.

**Rollback.** Remove deferred generators/stage state; restore full generation.

### E-5 — Fail-soft score propagation

**Primary / expected.** Return actual values from RFP, NMP, beta cutoffs,
completed nodes, and qsearch; store correct TT bound. Expected +8 to +18 Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/transposition.rs`. Existing score bands and declared mate gate.

**Support.** Bound-direction assertions, mate round-trip, PV legality, aspiration
re-search count, fixed-depth agreement.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Main search only; qsearch only; fail-hard return with actual TT
cutoff score.

**Rollback.** Restore clipped returns and storage.

### F-5 — Complete QTable bounds

**Primary / expected.** Store qsearch fail-low upper, exact, and fail-high lower
bounds; use cutoff without stored move only when key and generated move-set class
match exactly. Expected +8 to +16 Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/transposition.rs`. P4 qsearch class; no entry growth unless spare
bits cannot encode class, in which case candidate is blocked before games.

**Support.** Encode/decode/direction checks; QT-on/off score comparison; each bound
reused only under identical capture/evasion/promotion move set; hit/use/qnodes.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Upper bounds outside check only; exact depth/class match; store
upper bounds while probing exact/lower only.

**Rollback.** Remove class/bound encoding, probes, and writes.

### G-5 — Probe QTable before static evaluation

**Primary / expected.** At non-check qnodes, probe valid F-5 bounds before fresh
evaluation. In-check nodes have no stand pat. Expected +8 to +15 Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/transposition.rs`, `src/game/position/evaluation.rs`.

**Support.** Evaluate-first score identity; calls saved, qnodes, NPS, qfixtures.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Exact hits only; exact plus lower cutoff; below top qply only.

**Rollback.** Restore evaluate-before-probe order.

### H-5 — Main-TT static-evaluation cache

**Primary / expected.** Pack signed raw static evaluation into spare TT bits
without entry growth; reuse only with matching search key and non-check node.
Expected +8 to +18 Elo.

**Touch / derive.** `src/game/search/transposition.rs`,
`src/game/position/search.rs`. Width/sentinel from current score range.

**Support.** Entry size/capacity unchanged; boundary/sentinel round-trip;
cached/fresh equality; calls saved; P4 key mandatory.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Non-PV only; exact entries only; cache store without bound
substitution.

**Rollback.** Clear bits and restore unconditional evaluation.

### I-5 — Gravity history reward

**Primary / expected.** Replace positive additive quiet cutoff reward with bounded
gravity reward. Preserve accepted all-node malus formula unchanged. Expected +8
to +18 Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`. Depth-scaled private bound; no table addition.

**Support.** Reward saturation/range, EBF, first-move cutoffs, NPS; malus sequence
identity.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Gravity malus with original reward; non-PV reward only; shallow
reward only.

**Rollback.** Restore positive update arithmetic exactly.

### J-5 — Compressed one-ply continuation history

**Primary / expected.** Score current quiet destination by previous move's four
rule-derived role classes. Maximum storage:

`4 * MAX_SQUARES * size_of::<i16>() = 2,048 bytes per worker`.

Expected +8 to +18 Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`, `src/game/search/parameters.rs`. Previous move
already on stack; no two-ply or piece-square tensor.

**Support.** Byte assertion, coverage/saturation/cutoff output, EBF/NPS.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Same table as tie-break only; successful-cutoff updates only;
previous-target/current-target table only if it fits same 2,048-byte ceiling.

**Rollback.** Delete table, lookup, score, update, initialization.

### K-5 — Countermove heuristic

**Primary / expected.** Store one best quiet reply keyed by previous destination;
insert after killers. Maximum storage:

`MAX_SQUARES * size_of::<PseudoMove>()`, evaluated and recorded for target build,
with hard ceiling 8 KiB per worker.

Upstream support exists but local prior is low-confidence +5 to +15 Elo. Phase
still requires +8.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`. Previous destination/legal reply only.

**Support.** Byte assertion, legality, duplicate suppression, cutoff rate,
fixed-depth search.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Non-PV beta-cutoff replies only; only when no killer legal;
non-drop replies only.

**Rollback.** Remove table, update, picker stage.

### L-5 — Strong-history LMR relief

**Primary / expected.** Reduce accepted LMR by at most one ply for strong
butterfly/continuation quiets. Do not add extra reduction for weak moves in
primary. Expected +8 to +18 Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`. Existing reduction surfaces remain unchanged.

**Support.** Legal depth bound, strong-only triggers, re-search/EBF output,
gate-false identity.

**Promotion.** B10 LMR-capable subset, floor +8, 12,000 games.

**Fallbacks.** Butterfly signal only; non-PV depth >= 6; weak-history extra
reduction as separate same-letter arm.

**Rollback.** Remove relief; retain base surfaces.

### M-5 — Pruning-only correction history

**Primary / expected.** Add one 16K `i16` search-key table per worker, exactly
32 KiB. Correct only improving, RFP, NMP, futility, and related pruning inputs.
Raw evaluation remains in TT/QT, qsearch stand pat, terminal scores, returned leaf
scores, and protocol output. Update only completed non-check, non-terminal nodes
whose best move is quiet and whose score is outside mate band. Expected +8 to +18
Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/position/hash.rs`. P4 search key; no side-table family or payload.

**Support.** Byte assertion; clamp; raw/corrected separation; exact update cases;
saturation, pruning decisions, EBF/NPS. RFP/NMP/futility uses measured separately.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** 8K table; RFP only; improving only.

**Rollback.** Delete table, index, updates, corrected pruning path.

### N-5 — Internal iterative reduction

**Primary / expected.** At sufficiently deep PV nodes lacking legal TT move,
reduce one ply to save work when TT guidance is absent. No exclusion or multicut
search. Upstream support exists; local prior is low-confidence +4 to +12 Elo.
Phase requires +8.

**Touch / derive.** `src/game/position/search.rs`. PV/depth/TT/check/objective
state; private threshold.

**Support.** Trigger/work saved/re-search output, no root truncation, mate
fixtures, EBF/agreement.

**Promotion.** B10, floor +8, 12,000 games.

**Fallbacks.** Depth >= 8; exclude aspiration recovery; require no matching TT key.

**Rollback.** Remove adjustment and threshold.

### O-5 — Non-capture promotions in qsearch

**Primary / expected.** Add legal non-capture promotions with positive derived
material change after captures. Do not add ordinary checks. Expected +10 to +25
Elo.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/moves/move_list.rs`, `src/game/search/move_ordering.rs`. Promotion
mapping/zones and loaded base/promoted material.

**Support.** Frontier equals full-generation eligible subset; no duplicates;
qsearch termination/growth/NPS; P6 perft/tactics. F-5 class distinguishes
capture-only from promotion-enabled qsearch.

**Promotion.** P6, floor +8, 12,000 games.

**Fallbacks.** Mandatory promotions only; top qply only; gain above delta margin.

**Rollback.** Remove frontier, ordering, qsearch integration/class.

### P-5 — Recapture-first qsearch

**Primary / expected.** Keep complete capture set but place legal recaptures on
previous capture destination before other captures when P6 capability says
ordinary recapture semantics. Expected +8 to +15 Elo from earlier tactical
cutoffs without omitting moves.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`. Previous capture target and recapture
capability.

**Support.** Full move-set identity; ordering sequence; qnodes/sign agreement;
unsafe-family gate behavior.

**Promotion.** B10 recapture-capable subset, floor +8, 12,000 games.

**Fallbacks.** Prioritize only SEE-nonnegative recaptures; filter to recaptures only
beyond explicit negative qdepth with stand-pat/SEE condition and QTable disabled
or keyed by class/target; third-qply filtering only.

**Rollback.** Remove target propagation and recapture priority/filter/class.

### Q-5 — First-class drop ordering

**Primary / expected.** Index drops by dropped role and destination for gravity,
continuation, and countermove ordering. No LMR/LMP/futility change in primary.
Expected +12 to +30 Elo on drop trees.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`, `src/game/moves/drop_list.rs`. Hand role,
legal drop zone, destination; no make/undo terminal classification in ordering.

**Support.** D8 perft/drop integrity, no role alias, ordering sequence, history
coverage/cutoff output, gate-false identity.

**Promotion.** D8, floor +10, 12,000 games.

**Fallbacks.** Gravity ordering only; continuation only; countermove only.

**Rollback.** Remove drop index/history integration and restore generic order.

### R-5 — Drop LMR

**Primary / expected.** Treat non-capturing drops as quiets for accepted LMR after
Q-5 ordering. Checking drops detected after make receive no reduction; terminal
moves remain eager. Do not apply LMP, futility, delta, or SEE pruning. Expected
+10 to +25 Elo on drop trees.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/move_ordering.rs`. Drop flag, post-make check state, Q-5 history,
P6 drop-reduction capability.

**Support.** D8 perft/drop integrity; checking/terminal full-depth proof;
reduction-only counters, EBF/NPS; ordering fixed to Q-5 baseline.

**Promotion.** D8, floor +8, 12,000 games.

**Fallbacks.** Depth >= 6 only; after first four drops only; one-ply maximum
reduction.

**Rollback.** Remove drop reduction path and retain Q-5 ordering.

### S-5 — Completed-depth root voting

**Primary / expected.** Among workers reaching greatest completed depth, group by
root move and select majority vote; tie by mate-distance-aware score then worker
zero. Shallower workers do not vote. Expected +8 to +20 Elo at multiple threads.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/parallel.rs`. Completed depth/root move/score only.

**Support.** Synthetic join cases through existing debug path, thread-one identity,
mate ordering, repeated two/four-thread determinism.

**Promotion.** B10 at production thread count, floor +8, 12,000 games.

**Fallbacks.** Deepest-depth majority with worker-zero tie only; deepest result
then score; worker-zero unless another move has strict deepest majority.

**Rollback.** Restore P8 deterministic deepest-result selection.

### T-5 — Deterministic SMP root diversity

**Primary / expected.** Rotate equal-priority root move order by worker index;
worker zero stays canonical. Same legal set, depth schedule, evaluation, and
pruning. Expected +8 to +20 Elo from less duplicated work.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/search/parallel.rs`, `src/game/search/move_ordering.rs`. Worker index and
root count only.

**Support.** Thread-one/worker-zero identity; full permutation per worker;
S-5 vote deterministic; shared-TT overlap/completed depth.

**Promotion.** B10 at production thread count, floor +8, 12,000 games.

**Fallbacks.** Equal-history quiets only; reverse ties on odd workers; root count

> = 8 only.

**Rollback.** Remove worker-specific ordering transform.

### U-5 — Occupancy-independent promotion-graph PST

**Primary / expected.** Replace `derive_closest_promotion` straight-line distance
inside existing promotion bonus with reverse occupancy-independent pseudo-legal
movement distance. Preserve current bonus scale. Use only movement families P6
graph capability models faithfully; cannon, hopper, screened, capture-only, and
path-dependent multi-leg families keep old component. Expected +12 to +30 Elo on
supported promotion variants.

**Touch / derive.** `src/game/search/parameters.rs`,
`src/game/representations/state.rs`. Movement vectors, orientation, forbidden and
promotion zones, mapping, optional/mandatory promotion. P2 base/residual PST keeps
payload from overwriting component.

**Support.** Target zero, unreachable sentinel, predecessor consistency,
capability ledger, side symmetry; unsupported and non-promotion final-PST identity;
speed unchanged.

**Promotion.** P6 capability-positive subset, floor +10, 12,000 games.

**Fallbacks.** Positive-value promotions only; mandatory zones only; pawn-like
forward roles only.

**Rollback.** Restore straight-line component and remove graph product.

### V-5 — Declared goal-zone PST redistribution

**Primary / expected.** For roles satisfying declared winning `Goal`, replace
existing centrality component in `derive_square_score` with normalized
occupancy-independent graph proximity to goal zone while preserving component
mean/range. Loss goals invert sign; draw/unsupported goals remain unchanged.
Expected +12 to +35 Elo on supported goal variants.

**Touch / derive.** `src/game/search/parameters.rs`,
`src/game/representations/termination.rs`,
`src/game/representations/state.rs`. Goal role/zone/outcome, orientation,
forbidden zones, P6 graph capability, P2 base/residual PST.

**Support.** Win/loss/draw sign fixtures, goal reachability, preserved mean/range,
unsupported/non-goal final-PST identity.

**Promotion.** G1 and any later gate-positive goal configs, floor +12,
12,000 games; 18,000 if draw rate exceeds 70%.

**Fallbacks.** Winning royal goals only; one-move goal region only; move-order
tie-break with no PST change.

**Rollback.** Restore generic centrality and remove goal graph product.

### W-5 — Tuned declared check progress

**Primary / expected.** Add one antisymmetric normalized delivered-check progress
feature for declared `checks: N`. Working tuner fits one universal coefficient
across C2; accepted value is private code constant beside evaluation, not payload.
Tuner/runtime feature extraction remains identical and active. Expected +12 to
+35 Elo on check-count variants.

**Touch / derive.** `src/game/position/evaluation.rs`,
`src/game/representations/termination.rs`, `src/debug/tuning.rs`,
`src/debug/datagen.rs`. Target/count/outcome from rules/state; no schema change.

**Support.** Runtime/tuner parity on legal FENs, zero on gate-false variants, P4
path key, held-out loss, coefficient stability across C2.

**Promotion.** C2, floor +12, 12,000 games.

**Fallbacks.** One-check-from-terminal universal feature; move-ordering-only
progress; abandon if tuner cannot move coefficient consistently.

**Rollback.** Remove feature, code constant, tuner extraction, datagen column.

### X-5 — Tuned extinction last-stock exposure

**Primary / expected.** Add one antisymmetric feature when declared extinction set
is exactly one member above terminal threshold. Working tuner fits one universal
coefficient across E3; accepted value is private code constant, not payload.
Expected +12 to +35 Elo.

**Touch / derive.** `src/game/position/evaluation.rs`,
`src/game/representations/termination.rs`, `src/debug/tuning.rs`,
`src/debug/datagen.rs`. Groups, wildcard, threshold, outcome, counts from rules.

**Support.** Runtime/tuner parity, threshold-adjacent enumeration, zero outside
declared groups, held-out loss, coefficient stability across E3.

**Promotion.** E3, floor +12, 12,000 games.

**Fallbacks.** Explicit groups only; loss-outcome groups only; final-target capture
ordering with no evaluation term.

**Rollback.** Remove feature, constant, tuner extraction, datagen column.

### Y-5 — Side-derived royal-home PST

**Primary / expected.** Replace hardcoded white-frame opening royal score
`-rank` in `derive_pst` with normalized pseudo-legal distance from each side's
rule-derived royal home/confinement region. Compute sides independently. Preserve
component mean/range; add no correction array or runtime field. Expected +8 to
+20 Elo on gate-positive royal variants.

**Touch / derive.** `src/game/search/parameters.rs`,
`src/game/representations/state.rs`. Initial royal squares after bounded SETUP
resolution, side orientation, forbidden zones, royal movement, and existing
confinement derivation. P2 base/residual PST applies post-load.

**Support.** Enumerate gate-positive shipped configs before games; symmetric
variants mirror exactly; asymmetric setup derives independent home regions;
unsupported/unconfined final-PST identity; no extra runtime bytes.

**Promotion.** R5 gate-positive pool plus every P9 gate-positive config, floor +8,
12,000 games.

**Fallbacks.** Initial royal square only; confinement-region centroid using plain
square distance; apply only where current shelter value is nonzero.

**Rollback.** Restore white-frame `-rank` component and remove home derivation.

### Z-5 — Material-only outcome retune

**Primary / expected.** Generate fresh legal self-play data from accepted Y-5 and
retune opening/endgame material only. PST residuals and W/X coefficients stay
frozen. Existing tuner must move exact runtime material values. Expected +8 to
+25 Elo.

**Touch / derive.** `src/debug/datagen.rs`, `src/debug/tuning.rs`,
`src/io/game_io.rs`, `res/param/*/latest.param`. P2 payload shape unchanged;
roles/phases rederive from tuned material. Clean resource-owner rebuild embeds
accepted payloads without source mtime edits.

**Support.** Binary provenance, legal FENs, train/held-out split, duplicate-game
check, held-out loss, promoted/demoted mapping, role/phase derivation, residual
PST reload, runtime/tuner equality.

**Promotion.** V7, floor +8, 18,000 games.

**Fallbacks.** Variants with sufficient clean data only; opening material only;
endgame material only. No joint PST fallback.

**Rollback.** Restore every payload byte and any active tuner/datagen mapping
change.

## Disposition of old iteration-4 J-Z roadmap

- Old J royal pressure: dropped; leaf attack-map pressure remains unjustified.
- Old K drop porosity: dropped; Q/R improve drop search without leaf hand pricing.
- Old L promotion graph: retained and narrowed as U-5 pseudo-legal gated PST.
- Old M objective pressure: split into V-5 goal, W-5 checks, X-5 extinction.
- Old N move-limit pressure: dropped with rejected draw/material logic.
- Old O development/coordination/breakthrough: dropped as compound bonus pile.
- Old P gravity history: retained as positive-only I-5.
- Old Q continuation history: compressed into J-5.
- Old R TT eval/correction: split into H-5 and pruning-only M-5.
- Old S drop integration: split into Q-5 ordering and R-5 reduction.
- Old T search buffers: retained with byte ceiling as B-5.
- Old U drop/SETUP picker: split into C-5 picker and D-5 deferred generation.
- Old V forcing qsearch: split into O-5 promotions and P-5 recapture priority.
- Old W adaptive clock: dropped; prior weak and front-loading sign wrong.
- Old X SMP diversity: split into prerequisite P8, S-5 voting, T-5 diversity.
- Old Y evaluation tuning: narrowed to W/X feature fitting and Z material only.
- Old Z joint tuning: dropped; no clock/search/PST compound campaign.

## Explicit omissions

No singular extension, multicut, ProbCut, eval-scaled verified NMP, ordinary
qsearch checks, capture history, material-sensitive draw scoring, move-limit
pressure, broad royal-pressure evaluator, drop porosity evaluator, legal-drop hand
pricing, generic mobility, adaptive clock, PST retune after derived transforms,
multiple correction tables, unbounded continuation tensors, full leaf attack
maps, copied Fairy-Stockfish values, variant-name gates, generic evaluation
framework, new constants dump, permanent diagnostics, duplicated scalar tail,
conditional payload scalar, serialized value no tuner moves, unit-test scaffold,
new harness, or `cargo fmt`.

## Implementation discipline

1. Complete P0-P9 before A-5.
2. Build each arm from previous accepted commit.
3. Implement one mechanism per arm.
4. Measure separable halves separately.
5. Remove temporary instrumentation before promotion binary.
6. Run deterministic support before games.
7. Run affected pool before favorable isolated variants.
8. Audit every P9 gate-positive shipped config after pooled H1.
9. Extend inconclusive tests; never impose failure cap.
10. Fully revert rejected arm before fallback.
11. Keep constants private and tables within declared byte ceiling.
12. Add no payload coefficient outside P2 material/PST-residual schema.
13. Update phase table and evidence immediately.
14. One accepted phase per commit; no attribution trailer.
15. Never run `cargo fmt`.

This document is roadmap only. Source implementation, campaigns, and commits
require later explicit instruction.
