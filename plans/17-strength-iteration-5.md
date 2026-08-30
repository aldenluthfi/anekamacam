# Strength Iteration 5

## Status

**All prerequisites complete. `base-5` is frozen at `a94018d`. `A-5` is next.
No strength campaign has started.**

Source baseline: `64fbf9a` on `main`.

Baseline after neutral prerequisites: `base-5`, commit `a94018d`,
`content_md5` `d2ab2a5b6b142c2a4aa3fd87eecfceff`.

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

#### P4 evidence, 2026-08-30

Status: complete.

Identity:

- `position_hash` is untouched: no component was added to `hash_position`, and
  repetition matching still compares canonical board keys alone.
- `virgin_hash` is a new incremental key over `virgin_board`, maintained by
  `set_virgin!` / `clear_virgin!` at the eleven make-path sites that move,
  capture, unload, or drop onto a square. Undo restores it from the snapshot.
- `search_key` folds the canonical hash with `virgin_hash`, counter clock and
  limit, counting count and frozen limit, delivered checks and required count,
  declared-repetition occurrences, and a pass/double-pass/stand-off class.
- `qsearch_key` adds the leaf move-set class (in check, endgame delta stand-down).
- Context rows live in a private `CONTEXT_HASHES` table in `hash.rs`;
  `VIRGIN_HASHES` lives in prelude because exported macros expand elsewhere.
  Nothing was added to `StaticState`.
- 16-bit counting values fold through two byte rows each, so every representable
  value keys exactly and the "disable TT/QT reuse" fallback is never needed.
- Synthetic null moves are excluded from repetition context: `has_repetition`
  and `count_repetitions` stop the scan at the newest null. Nested nulls restore
  side-to-move parity, which previously read as a repetition inside null search.

Separation, seed 1, canonical hash held equal:

- Unmoved pieces: crazyhouse (no counter rule) after `g1f3 a7a6 f3g1 b7b6`
  versus the same board loaded from FEN, where g1 is still virgin. Hash
  `302FC52A0ED990578E9A89C3C7085596` on both; search keys
  `1496EE96E62817F0EB96A989E7ECDA4F` versus `DC20DC3976DA6C098872D3FC87067517`.
  Search and qsearch keys differ by the identical component
  `C8B632AF90F27BF963E47A7560EAAF58`, one `VIRGIN_HASHES` row.
- Repetition occurrences: crazyhouse after one versus two knight round trips.
  Hash `C3BE45A0834CF291CCCB9397BCC9D333` on both; keys
  `293485D35F5FA0FEF289EA2516F5F742` versus `FF2E4B08B58F6A15A4FE2721C7A4B9BD`.
  Canonical identity stays correct: the same run flips Result from Ongoing to
  Draw at the third occurrence.
- Counter progress: standard start position at halfmove clock 0 versus 40. Hash
  `D3E0102EEF039F6EFBC4E476487A40BC` on both; keys
  `13180FE280217F2326C29CA11FB908AF` versus `8D75747BC754C38325F73185B2A9C6F6`.

Make/undo:

- `verify_game_state` now recomputes `hash_virgin_board` and names the differing
  square on mismatch. Debug builds assert it at every make and undo.
- Debug fixed-depth search, all 38 configs: 36 completed with no assertion.
  `janggi` and `sittuyin` abort on the pre-existing `Game phase score doesn't
  match expected value based on material counts` assertion at `util.rs:382`;
  the binary built from `c0c0a82` aborts identically on both, so this is not a
  P4 regression and is not fixed here.
- Bounded perft passes: crazyhouse 16/16 at depth 4; standard 1200/1200,
  shogi 12/12, minishogi 9/9, judkins 9/9, xiangqi 33/33, janggi 21/21 at
  depth 3.
- Crazyhouse drop integrity against fairy-stockfish: 16 games, 0 mismatches.
- FEN round trip: 44 passed, 0 failed, 0 skipped.

Table agreement:

- End-condition fixtures: 38 passed, 0 failed.
- Only two fixtures are search cases; the other 36 are `d` game-truth cases that
  never consult a table. Both search cases were rerun at Hash 1 and Hash 256 and
  agree exactly: `score cp 0` / `bestmove e10e9` one cycle short, `score mate 1`
  / `bestmove e9e10` on the closing cycle, with identical node counts.
- Fixed-depth start-position search does differ between Hash 1 and Hash 256, but
  it differs the same way at `c0c0a82`: table size changes move ordering, and
  that dependence is pre-existing, not introduced here. P7 owns the PV-node
  cutoff scope that this exposes.

  Corrected by the P7 evidence below: P7 does not own it and does not remove
  it. The dependence belongs to the stored move driving ordering, which no
  cutoff scope reaches. The sentence above was a guess and is retained as
  written so the correction has something to point at.

Cost, speed suite, 6 interleaved passes, `c0c0a82` versus P4:

| Variant    | Nodes A / B         | Time delta | NPS delta |
| ---------- | ------------------- | ---------- | --------- |
| standard   | 119679 / 125623     | +7.67%     | -2.52%    |
| shogi      | 888112 / 885436     | +2.26%     | -2.51%    |
| crazyhouse | 813213 / 671451     | -14.11%    | -3.87%    |
| xiangqi    | 223320 / 238822     | +9.45%     | -2.30%    |
| grand      | 1346591 / 1303166   | -4.65%     | +1.49%    |

Node counts move in both directions because entries that previously collided now
separate, and the null-move barrier removes false draws inside null search. The
per-node price of the wider key and the repetition count is 2.3% to 3.9% of
nps outside noise.

P4 result: accepted as a correctness prerequisite. It is not strength-neutral by
node count and was not claimed to be; the canonical hash, the rule set, and the
playing policy are unchanged.

#### Setup-probe cache fix, 2026-08-30

Fixes the pre-existing abort P4 recorded above, before P5 starts.

Defect: `resolve_setup_army` clones the position and immediately walks legal
placements with `make_move!`, but `derive_eval_products` calls it right after
`derive_piece_roles` has rewritten which pieces count as big, major, or minor,
and long before `derive_parameters` runs its closing `refresh_eval_state`. The
clone therefore carries the role counts and `phase_score` the position was
loaded with, which no longer describe the army standing on its board. Debug
builds catch it on the first make: `janggi` reports `right: 6398`, `sittuyin`
`right: 2464` (16 Feudal Lords, whose pawns are big under its derived
dynamics). Release builds walked the same inconsistent probe silently.

Fix: `refresh_eval_state` now owns every board-derived cache, role counts
included, instead of leaving `big_pieces` / `major_pieces` / `minor_pieces` to a
separate loop in `derive_eval_products`; that loop is gone and
`derive_eval_products` ends by refreshing, so each of its three callers leaves a
consistent position. `resolve_setup_army` refreshes its copy before walking.

Evidence:

- Debug fixed-depth search, all 38 configs: 38 completed, 0 assertions.
  `janggi` and `sittuyin` were the only two failing before.
- Depth-6 single-thread search, all 38 configs, `1755eea` versus fixed: every
  variant reports identical nodes, scores, and PVs. Six variants differ only in
  the `time` and `nps` fields of an `info` line. `janggi` and `sittuyin` are
  bit-identical too, so no derived parameter moved.
- `debug-headless derive` output identical across all 38 configs; `state` output
  identical for `janggi`, `sittuyin`, `standard`, `shogi`, `crazyhouse`,
  `xiangqi`.
- Only `janggi` and `sittuyin` declare a setup phase, so `resolve_setup_army`
  returns early for the other 36 and cannot change them.
- Debug perft suites: `sittuyin` 12/12, `janggi` 21/21 at depth 3. Release
  perft: standard 20256/20256, crazyhouse 16/16, shogi 12/12, xiangqi 33/33.
- End-condition fixtures 38 passed, FEN round trip 44 passed, crazyhouse drop
  integrity 24 games with 0 mismatches.

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

#### P5 evidence, 2026-08-30

Baseline `5074cef`. The `repeats >= 2` neutral is gone, and the declared
threshold is the only repetition verdict search reports. No cycle guard: the
measured cost of leaving sub-threshold repetitions searchable is under 2% of
nodes, so nothing needed mitigating, and any guard that returns a number is a
verdict wearing another name.

What the neutral was hiding:

- xiangqi `4k4/R8/9/9/9/9/9/9/9/3K5 w` after `a9a10 e10e9 a10a9 e9e10 a9a10`,
  one fold short of the declared three: baseline answered `score cp 0` after
  five nodes. It is chariot and general against a bare general. P5 answers
  `score mate -2`, stable from depth 6 through 12.
- The sign is the load-bearing part. Xiangqi punishes the perpetual checker, so
  an early perpetual verdict would score this position as a win for the side to
  move. It is mated instead, which is how the fixture now tells the two apart.
- shogi `9/9/9/9/4k4/9/9/9/R1K6 w -/-` declares four occurrences. Baseline
  answered `score cp 0` at both the second and third fold, two full folds before
  the rule fires; P5 answers `score cp -1021` at each. minishogi at the second
  fold: `score cp 0` becomes `score cp -618`.

Threshold verdicts against the `d` oracle, at the fold the rule names:

| Case                                | `d`        | Search        |
| ----------------------------------- | ---------- | ------------- |
| shogi perpetual check, 4th fold     | Black wins | `mate 1`      |
| shogi plain 4-fold                  | Draw       | `cp 0`        |
| minishogi perpetual check, 4th fold | Black wins | `mate 1`      |
| xiangqi repetition, no offence      | Draw       | `cp 0`        |

Fixtures: 38 passed, 0 failed. Two changed here. The xiangqi one-cycle-short
case asserted `score cp`, which only the neutral could produce, and now asserts
`score mate -2`; its closing partner asserts `score mate 1`. Both needed the
runner to compare a score's value and not only its kind, which is the one
harness change. The baseline binary fails the first of them, so the pair is a
real discriminator rather than a restatement.

Synthetic null history: `history` is pushed in exactly two places, `make_move!`
and `make_null_move!`, and nothing else in the tree fabricates a snapshot. The
null snapshot carries `move_ply: null_move()`, and both `count_repetitions` and
`has_repetition` stop their scan there, so a null never contributes an
occurrence.

Other suites: debug fixed-depth search over all 38 configs, 0 assertions; perft
standard 20256/20256, crazyhouse 16/16, shogi 12/12, xiangqi 33/33, janggi
21/21; FEN round trip 44/44; crazyhouse drop integrity 24 games, 0 mismatches.
Agreement against fairy-stockfish is unchanged in standard (9 cases, 0 sign
flips) and crazyhouse holds at 5 sign flips with `reference-sees-lost-we-do-not`
falling from 7 to 6.

Cost, speed suite, 6 interleaved passes, `5074cef` versus P5:

| Variant    | Nodes A / B         | Time delta | NPS delta |
| ---------- | ------------------- | ---------- | --------- |
| standard   | 125623 / 127797     | +0.61%     | +1.11%    |
| shogi      | 885436 / 885437     | +0.09%     | -0.09%    |
| crazyhouse | 671451 / 675247     | +0.26%     | +0.31%    |
| xiangqi    | 238822 / 240930     | +0.31%     | +0.57%    |
| grand      | 1303166 / 1298088   | -0.59%     | +0.20%    |

`newzealand` has no perft suite to bench, and from its start position alone it
costs 3.6x nodes at depth 8. Over the first 24 standard perft positions, which
share its board, the same depth costs +0.58%, so the start position is one
outlier and not the variant's price.

P5 result: accepted as a correctness prerequisite.

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

#### P6 evidence, 2026-08-30

Baseline `119dcc8`. `StaticState` carries a `capabilities: u16`;
`derive_search_capabilities` fills it from movement vectors and declared
terminal rules, and every shortcut that answers without searching reads it.
`debug-headless derive` prints the mask beside each config, most significant
bit first: static movement, quiet pruning, recapture order, null pruning,
forward pruning, exchange pruning, exchange validity.

Movement facts come from the generated vectors: a leg that unloads what it
destroyed needs a second piece standing where it stands, a leg that may take a
royal is not trading material, a vector that destroys twice wins more than its
victim, a vector that returns to its own square having destroyed nothing is a
pass the variant already offers, and a piece with no quiet vector cannot give
up a tempo. Terminal facts come from the declared rules: extinction, a goal
zone, a check tally, and a bare-king count all pay in a currency material does
not convert to; a hand or a promote-to-captured pool means a capture is not the
end of the transaction; a setup phase and a stand-off rule both make passing a
real decision.

Late-move reduction is deliberately not gated. A reduced search that beats
alpha is repeated at full depth, so it reorders work and never drops a move,
and no rule can make that unsound. Gating it was measured first and cost koth
40739% and sittuyin 30313% of baseline nodes for no soundness gained.

Nineteen of the 38 shipped configs derive a full mask. The other nineteen:

| Mask      | Configs                                              |
| --------- | ---------------------------------------------------- |
| `1101101` | crazyhouse, euroshogi, judkins, minishogi, shogi, pocketknight |
| `1111010` | extinction, horde, kinglet                           |
| `1110101` | makruk, ouk-chaktrang                                |
| `0111110` | minixiangqi, xiangqi                                 |
| `1000001` | fivecheck, threecheck, koth                          |
| `1101100` | grand                                                |
| `0110111` | janggi                                               |
| `0100100` | sittuyin                                             |

The mask is the only thing that changed. A scratch build of this tree that
reads the mask from the environment, run with every bit forced on, reproduces
`119dcc8` exactly on all 38 configs -- same nodes, scores, and PVs. With the
derived masks, exactly the nineteen full-mask configs stay byte-identical to
`119dcc8` and exactly the nineteen restricted ones differ. The partition is
the mask and nothing else.

Every bit is live. From all bits on, clearing one at a time changes the
standard start position at depth 8 (15591 nodes with everything on):

| Bit cleared     | Nodes |
| --------------- | ----- |
| see_valid       | 26167 |
| see_pruning     | 18627 |
| forward_pruning | 14923 |
| null_pruning    | 36336 |
| recapture_order | 17692 |
| quiet_pruning   | 68386 |
| static_movement | 26877 |

Cost, 16-position bench at depth 8 against `119dcc8`, nodes:

| Variant      | Base / P6           | Delta   |
| ------------ | ------------------- | ------- |
| standard     | 38618 / 38618       | 0.0%    |
| berolina     | 297315 / 297315     | 0.0%    |
| capablanca   | 364127 / 364127     | 0.0%    |
| los-alamos   | 110639 / 110639     | 0.0%    |
| shatranj     | 129319 / 129319     | 0.0%    |
| judkins      | 336245 / 349150     | +3.8%   |
| shogi        | 885437 / 994456     | +12.3%  |
| crazyhouse   | 675247 / 794715     | +17.7%  |
| euroshogi    | 692352 / 883372     | +27.6%  |
| pocketknight | 354219 / 474640     | +34.0%  |
| minishogi    | 202504 / 284606     | +40.5%  |
| sittuyin     | 270564 / 386832     | +43.0%  |
| makruk       | 268867 / 397712     | +47.9%  |
| minixiangqi  | 113123 / 170644     | +50.8%  |
| xiangqi      | 142360 / 220140     | +54.6%  |
| grand        | 684975 / 1199834    | +75.2%  |
| janggi       | 287914 / 722834     | +151.1% |

`sittuyin` ran at depth 6 over 12 positions; the rest at depth 8 over 16.
The five zero rows are the control: a full-mask config searches the same tree
it did before.

Configs without a perft suite were measured over the first 20 standard perft
positions at depth 7, which share their board and pieces: `koth` +52.8%,
`threecheck` +118.9%, `fivecheck` +118.9%. `extinction`, `horde`, and
`kinglet` reject those FENs, so they were swept from their own start position
at depths 6 through 9: extinction +89/+262/+216/+210%, kinglet +8/-5/+4/+219%,
horde -7/-25/-52/-85%. Horde searches less because the forward pruning it lost
was costing it nodes, not saving them.

Wall clock, speed suite, 5 interleaved passes: standard unchanged at +0.03%
nps, so the mask reads are free. The restricted variants pay in time but gain
in rate -- xiangqi +28.01% time at +29.12% nps, grand +42.18% time at +32.23%
nps -- because a node that no longer runs the exchange simulation is much
cheaper than one that does.

Suites: debug fixed-depth search over all 38 configs, 0 assertions; perft
standard 20256/20256, crazyhouse 16/16, shogi 12/12, xiangqi 33/33, janggi
21/21, sittuyin 12/12; end-condition fixtures 38 passed; FEN round trip 44/44;
crazyhouse drop integrity 24 games, 0 mismatches. Agreement against
fairy-stockfish holds its gate: standard 9 cases with 0 sign flips, crazyhouse
21 cases with 5 sign flips, unchanged. Crazyhouse's
`reference-sees-lost-we-do-not` rose from 6 to 8, which is the one number that
moved the wrong way and is recorded here rather than explained away.

No variant name appears anywhere in the change.

P6 result: accepted as a correctness prerequisite. It costs nodes in nineteen
variants and buys the right to keep the shortcuts in the other nineteen
without an argument that was never checked.

Where the cost is collected, decided 2026-08-30 after this evidence was
written. The node cost above is expected under P6's own proof clause and law
12, not a regression -- but the roadmap as first written scheduled nothing to
collect it, and several lettered phases are gated on the mask and so improve
only the configurations that never lost a bit. Corrected here: `Q-5`, `R-5`,
`V-5`, `W-5`, and `X-5` each gained a **Capability close** step that re-derives
the bit its own mechanism addresses; `AA-5` and `AB-5` were appended after
`Z-5` for the two mechanisms no letter owned, screened movement and the
pass-or-counting pair. Both are strength phases with real floors, since
restored search depth is Elo rather than node accounting. Nothing in the
`P0`-`P9` chain reopens: every phase after P6 already builds on it, and
re-basing the chain to collect this would cost more than the ground it
recovers.

### P7. Correct TT cutoff scope

Main-TT early bound cutoff applies only at non-PV nodes. PV nodes may reuse move,
static evaluation, and metadata. Eager terminals precede TT reuse. Preserve
replacement policy absent separate proof; preserve mate encoding.

Touch:

- `src/game/position/search.rs`
- `src/game/search/transposition.rs`

Proof: TT-on/off terminal and PV fixtures agree; root score/best move stable;
search-context keys never cross.

#### P7 evidence, 2026-08-30

Baseline `2571c01`. One condition: the main-table bound cutoff now requires
`!pv_node`, where `pv_node` is a window wider than one ply after the mate
clamps. The stored move is still read at every node, since ordering is what it
was for. Replacement policy and mate encoding are untouched.

Two of the four clauses were already satisfied and are recorded rather than
changed. Eager terminals precede table reuse: `is_terminal!` is the first
statement in `alpha_beta` and the declared repetition threshold has sat above
the probe since P5. Search contexts never cross: the main table is probed and
stored under `search_key` and the quiescence table under `qsearch_key`, and
quiescence's main-table probe reads only the stored move, never a bound.

What it fixes. A bound names a score and no move. Returning one at a node
opened on a wide window answers "which move" with something that cannot
answer it, and the printed line then comes from walking the table rather than
from the search. Across all 38 configs at depths 6 through 10, the baseline
printed a principal variation shorter than its depth in 12 of 190 lines. P7
prints 12 of 12 at full length and shortens none:

| Config        | Depth | Base PV | P7 PV |
| ------------- | ----- | ------- | ----- |
| makruk        | 6     | 4       | 6     |
| judkins       | 7     | 4       | 7     |
| almost        | 8     | 5       | 8     |
| amazon        | 8     | 5       | 8     |
| embassy       | 8     | 6       | 8     |
| asean         | 8     | 7       | 8     |
| minishogi     | 9     | 7       | 9     |
| standard      | 10    | 7       | 10    |
| shogi         | 10    | 8       | 10    |
| chancellor    | 10    | 9       | 10    |
| janus         | 10    | 9       | 10    |
| ouk-chaktrang | 10    | 9       | 10    |

Fixtures: 38 passed at `GO_DEPTH` 6 and again at 8. Both xiangqi search
fixtures are identical at Hash 1 and Hash 256 -- 826 nodes at `mate -2` with
`pv e10e9 a10f10 e9e8 f10f9`, and 13 nodes at `mate 1` with `pv e9e10` -- so
the terminal verdicts search reaches do not depend on table size.

Correction to the P4 evidence. That block guessed P7 owned the hash-size
dependence of fixed-depth search. It does not. Over the 30 agreement positions
at their declared depths, the baseline reports a different best move at Hash 1
than at Hash 256 on 15, and P7 on 17. The cause is the stored move driving
ordering: a different table size collides and replaces differently, a different
move is read first, and a different tree is searched. Every table that stores a
move for ordering has this, and no cutoff scope reaches it. Removing it would
mean not ordering on the stored move at all.

Cost. Node counts move both ways and mostly down: over 38 configs at depth 9
from the start position, 20 fall, 13 rise, 5 are unchanged, median -1.9%.
Largest falls are xiangqi -52.7%, judkins -46.7%, ai-wok -42.8%; largest rises
are embassy +146.4%, fivecheck +81.9%, janus +76.6%. Searching a PV node
instead of trusting a bound costs work at that node and saves it wherever the
bound was wrong about the move.

Speed suite, 5 interleaved passes, nodes and time:

| Variant    | Nodes A / B         | Time delta | NPS delta |
| ---------- | ------------------- | ---------- | --------- |
| standard   | 127797 / 133695     | +2.50%     | +2.07%    |
| shogi      | 994456 / 1007941    | +1.70%     | -0.34%    |
| crazyhouse | 794715 / 777281     | -2.69%     | +0.52%    |
| xiangqi    | 398210 / 493122     | +24.70%    | -0.69%    |
| grand      | 2440329 / 2400555   | -2.42%     | +0.81%    |

Other suites: debug fixed-depth search over all 38 configs, 0 assertions; perft
standard 20256/20256, crazyhouse 16/16, xiangqi 33/33; FEN round trip 44/44;
crazyhouse drop integrity 24 games, 0 mismatches. Agreement holds its gate:
standard 9 cases, 0 sign flips; crazyhouse 21 cases, 5 sign flips,
`reference-sees-lost-we-do-not` unchanged at 8.

P7 result: accepted as a correctness prerequisite.

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

#### P8 evidence, 2026-08-30

Baseline `a5bd5cd`. `SearchResult` carries `completed_depth`, written only where
`best_score` and `best_move` are, which is below the interrupt guard. The pool
selects the greatest completed depth and keeps the first worker holding it, so
worker zero wins a tie. Score decides nothing.

The phase name is half right and the correction matters. A partial iteration
never counted even before: `best_score` and `best_move` sit below the interrupt
guard, so an unfinished depth already updated nothing. What the pool did wrong
was compare scores reached at different depths. A worker that finished depth 15
holding +13 outranked three workers that finished depth 16 holding less, and
the deeper answer lost to the shallower one on a number the two never computed
about the same tree.

Measured on timed four-thread runs from the start position, 1500 ms, Hash 64:

| Variant  | Exposed  | Base wrong move | P8 wrong move |
| -------- | -------- | --------------- | ------------- |
| standard | 10 of 24 | 3 of 24         | 0 of 24       |
| shogi    | 9 of 24  | 2 of 24         | 0 of 24       |

Exposed counts trials where the highest final score was held only by a worker
that finished shallower than the deepest, which is the shape the old rule
mishandles. Wrong move counts trials where the returned move is one no deepest
worker chose. The two differ because a shallower worker often picks the same
move anyway, and because the old comparison used `>=`, so a deeper worker tying
the top score at a higher index still won. The hazard arises just as often
under P8 -- the same 10 and 9 -- and stops deciding anything.

One standard trial is the whole phase in one line: depths 16, 16, 16, 15, and
the depth-15 worker's `c2c4` was returned over three workers that finished
depth 16.

One-thread identity: all 38 configs byte-identical to `a5bd5cd` at depth 9,
including nodes, scores, and PVs. The single-thread path does not enter the
pool at all.

Node accounting. The pool used to copy the winning worker's counters into the
returned result, so a four-thread search reported one worker's share as the
whole. Nodes are now summed across workers and elapsed time is the longest any
worker ran, since they run concurrently. Standard at depth 12: one thread
329545 nodes before and after, four threads 199647 before and 851467 after.
Protocol `info` lines are unaffected -- those are thread zero's own counters,
emitted per iteration, and always were.

Mate ordering: end-condition fixtures pass, including the `mate 1` and
`mate -2` search cases. Choosing among equally deep workers by mate distance is
`S-5`; P8 takes worker zero by specification and does not vote.

Other suites: debug fixed-depth search over all 38 configs, 0 assertions; perft
standard 20256/20256, crazyhouse 16/16; end-condition fixtures 38 passed; FEN
round trip 44/44; crazyhouse drop integrity 24 games, 0 mismatches. Speed
suite, single thread, within noise on all five variants: time between -2.27%
and +0.34%.

P8 result: accepted as a correctness prerequisite.

### P9. Freeze `base-5` and capability ledger

Repeat P0 evidence. Record prerequisite commits, `content_md5`, and derived
capability result for every shipped configuration. This ledger defines every
affected-variant audit and every identity control. Every strength arm compares
against previous accepted phase, never directly against `64fbf9a`.

The ledger also records a fixed-depth node signature for all 38 shipped
configurations, gate-positive and gate-negative alike. Both halves are on the
record because both are audited: identity holds the gate-positive half in
place, and the gate-negative half needs a number to be measured against or it
drifts unwatched through every later phase.

#### P9 evidence, 2026-08-30

Status: complete. `base-5` is frozen. Every strength arm compares against the
previous accepted phase, and the first of them compares against this.

Build and provenance:

- HEAD: `a94018d996bd6bacd31450c7bfb79d8aede05dc9` on `main`.
- Build: `cargo build --release`, default features, no warnings.
- Compiler: `rustc 1.97.0-nightly (e96c36b6f 2026-05-21)`.
- Cargo: `cargo 1.97.0-nightly (4d1f98451 2026-05-15)`.
- Host: macOS 26.5.1, build 25F80, arm64.
- Binary: `bin/base-5`, 5,548,080 bytes.
- Binary MD5: `830bb671e67cae00674a4e1102f2676e`.
- `content_md5`: `d2ab2a5b6b142c2a4aa3fd87eecfceff`.
- Config tree MD5: `c09f443db4fe35a46cec187ed619c637`, unchanged from P0.
- Dictionary tree MD5: `39bee9589de36c8d7562b8c42c09af44`, unchanged from P0.
- UCI option MD5: `f3f291236fa49c71c5569f2beec47411`, unchanged from P0.
- Embedded variants: 38.
- `tools/provenance.sh verify bin/base-5` rebuilt `a94018d` and matched
  `content_md5` exactly.
- The pre-existing `src/game/position/search.rs` diff still changes only
  comment alignment on the `delta_prunable` line. It remained unstaged
  throughout and does not change rebuilt binary content.

Prerequisite commits, `64fbf9a..a94018d` in order:

| Commit    | Phase | Subject                                              |
| --------- | ----- | ---------------------------------------------------- |
| `a3c502d` | P0    | freeze strength iteration 5 baseline                 |
| `37be6c2` | P1    | remove permanent search diagnostics                  |
| `fd50650` | P2    | store material and PST residuals                     |
| `c0c0a82` | P3    | separate private and shared constants                |
| `1755eea` | P4    | separate canonical, search, and qsearch identity     |
| `5074cef` | fix   | rebuild the setup probe's caches before it plays     |
| `119dcc8` | P5    | let a repetition mean what its variant declares      |
| `1aa2511` | P6    | ask the rules before taking a search shortcut        |
| `2571c01` | docs  | schedule the ground P6 gave up                       |
| `a5bd5cd` | P7    | stop answering a wide window with a bound            |
| `54bc66d` | P8    | pick the deepest finished worker, not the score      |
| `a94018d` | docs  | say the P8 measurement in plain terms                |

Deterministic search settings:

- Fixed-depth anchors: `ANEKAMACAM_SEED=42`, Threads 1, depth 6, run through
  `debug-headless search <variant> 6 1`.
- Table entries are 64 bytes in both tables: three `u128` slots, one `u64`
  age, one `AtomicU64` seqlock version. Capacity is rounded down to a power of
  two.
- `debug-headless search` builds a 1 MB main table and a 1 MB quiescence
  table, which is 16,384 slots each and 2 MiB retained. The P0 evidence said
  this command used the 256 MB protocol default; it does not, and the anchors
  above are only reproducible at 1 MB. Corrected here.
- Protocol default Hash 256 MB splits two thirds to the main table and one
  third to quiescence: nominal 170 MB and 85 MB, rounded to 2,097,152 and
  1,048,576 slots, 192 MiB retained.
- EBF and agreement use matched Hash 64 MB: nominal 42 MB and 21 MB, rounded
  to 524,288 and 262,144 slots, 48 MiB retained.

Derivation and payload:

- `debug-headless derive`: 38/38 configs derived.
- Derive output MD5: `6994599453246c06ee1476255cedac3e`.
- Payload manifest: 38 files, manifest MD5 `e573893836904609d060584f6bc0a7c2`.

Capability ledger and fixed-depth signature. Mask bits read most significant
first: static movement, quiet pruning, recapture order, null pruning, forward
pruning, exchange pruning, exchange validity.

```text
config         mask     best      score     nodes
ai-wok         1111111  d3:d4        -1      7657
almost         1111111  b1:c3         0      4226
amazon         1111111  b1:c3         0      2772
asean          1111111  e1:e2         0      2620
berolina       1111111  g2:e4         6      5462
capablanca     1111111  b1:c3         1      5353
chancellor     1111111  b1:c3         5      4403
chigorin       1111111  b1:c3       -41      3943
crazyhouse     1101101  b1:c3         1      3749
embassy        1111111  b1:c3         0      7908
euroshogi      1101101  c1:d2        -1      5863
extinction     1111010  b1:c3         0      5857
fivecheck      1000001  b1:c3         0     10014
gothic         1111111  b1:c3         2      4836
grand          1101100  i2:h4         0     21614
hoppelpoppel   1111111  b1:c3         0      3625
horde          1111010  e4:e5      -540      4995
janggi         0110111  Q@e2          0       730
janus          1111111  b1:c3         0      5224
judkins        1101101  d1:c3        15      5620
kinglet        1111010  g1:f3         0      1482
knightmate     1111111  d2:d4        10      3645
koth           1000001  b1:c3         0     10014
los-alamos     1111111  a2:a3         0      2653
makruk         1110101  g1:e2         2      4905
minishogi      1101101  d1:c2         4      6897
minixiangqi    0111110  f1:f4        23      3510
modern         1111111  b1:c3         0      3297
newzealand     1111111  b1:c3         1      4643
ouk-chaktrang  1110101  c1:c2         0      6762
pocketknight   1101101  d2:d4         2     30319
shatranj       1111111  b1:c3         0      2036
shogi          1101101  c1:d2         0      4512
sittuyin       0100100  K@b2          0    139366
standard       1111111  c2:c4         5      4391
threecheck     1000001  b1:c3         0     10014
tjatoer        1111111  h4:h8        10     19700
xiangqi        0111110  b3:g3        16     14420
```

Nineteen configurations carry a full mask and are the identity half. The other
nineteen are the monotonic half: their node counts above are the ceiling every
later phase is measured against.

B10 principal variations at the same anchors:

- `standard`: c2:c4 b8:c6 b1:c3 g8:f6 g1:f3 d7:d5
- `shatranj`: b1:c3 b8:c6 g1:f3 g8:f6 c1:e3 c8:e6
- `grand`: i2:h4 i9:h7 b2:c4 b9:c7 a3:a5 g9:f7
- `xiangqi`: b3:g3 h8:g8 b1:c3 b8:c8 c1:e3 c10:e8
- `janggi`: Q@e2 q@e9 E@c1 e@c10 E@g1 e@g10
- `shogi`: c1:d2 c9:d8 g1:f2 f9:e8 f1:e2 g9:f8
- `crazyhouse`: b1:c3 b8:c6 g1:f3 g8:f6 e2:e4 a7:a5
- `koth`: b1:c3 b8:c6 g1:f3 g8:f6 a2:a4 a7:a5
- `threecheck`: b1:c3 b8:c6 g1:f3 g8:f6 a2:a4 a7:a5
- `extinction`: b1:c3 g8:f6 g1:f3 b8:c6 d2:d4 d7:d5

EBF suite, seed 42, Hash 64 MB, one thread:

- Log MD5: `766fcd0410223f54b503805ca6e78fd9`; suite exited 0.
- `standard@13`: 9 cases, geometric nodes 169,808, EBF 1.734.
- `crazyhouse@13`: 9 cases, geometric nodes 2,124,515, EBF 1.882.
- `shogi@11`: 4 cases, geometric nodes 221,523, EBF 1.952.
- `xiangqi@12`: 4 cases, geometric nodes 84,708, EBF 1.543.
- `capablanca@10`: 1 case, nodes 175,555, EBF 2.537.
- `gothic@10`: 1 case, nodes 91,143, EBF 2.273.
- `grand@10`: 5 cases, geometric nodes 123,677, EBF 1.849.
- Crazyhouse-mid / standard node ratio at depth 13: 14.62x, against 13.68x at
  P0.

Agreement suite against `/opt/homebrew/bin/fairy-stockfish`, seed 42, Hash
64 MB, one thread:

- Log MD5: `2b74de0c92a8fbbfea0862ecaeafe47b`; suite exited 0.
- `standard`: 9 cases, median gap -56, 0 sign flips, 0 blind losses.
- `crazyhouse`: 21 cases, median gap +814, 5 sign flips, 8 reference-sees-lost
  cases missed locally, against 6 at P0. The sign gate is unchanged; the blind
  count rose across P6 and is recorded there.

Speed suite, seed 42, one thread, 10 passes per variant:

- Log MD5: `b39429c049b5a48bd304fa2527e85c22`; suite exited 0.
- `standard`, depth 11, 16 positions: 133,695 nodes, 4,002,426 NPS.
- `shogi`, depth 8, 16 positions: 1,007,941 nodes, 1,776,062 NPS.
- `crazyhouse`, depth 8, 16 positions: 777,281 nodes, 1,281,091 NPS.
- `xiangqi`, depth 9, 16 positions: 493,122 nodes, 1,188,551 NPS.
- `grand`, depth 9, 16 positions: 2,400,555 nodes, 1,076,565 NPS.
- Node counts matched across all ten passes.

Correctness suites, all against `bin/base-5`:

- End-condition fixtures: 38 passed, 0 failed, log MD5
  `eed18fb511f87a487a70c2282ecdeaf1`.
- FEN round trip: 44 passed, 0 failed, 0 skipped, log MD5
  `270795e6d6eff2495253876fc8102a50`.
- Crazyhouse drop integrity: 24 games, 0 mismatches, log MD5
  `f779b4c771da3eb0f1678a7095cb2b2b`.

Bounded perft, seed 42, depth 4, log MD5
`b3101e3f201074069a22875a9f8e0c72`. Every count matches P0 exactly:

- `standard`: first 64 positions, 256/256 depth rows passed.
- `crazyhouse` 16/16, `euroshogi` 16/16, `janggi` 28/28, `judkins` 12/12,
  `minishogi` 12/12, `minixiangqi` 4/4, `pocketknight` 12/12, `shogi` 16/16,
  `sittuyin` 16/16, `xiangqi` 44/44.

The detached remote full standard depth-6 run recorded at P0 remains untouched
and informative only, as directed.

P9 result: accepted. `base-5` is `a94018d`, `content_md5`
`d2ab2a5b6b142c2a4aa3fd87eecfceff`. `A-5` may start.

## Campaign protocol

### Support mode

- Pin `ANEKAMACAM_SEED`; match Hash; one thread except SMP phases.
- Verify `content_md5`, not raw binary MD5.
- Run affected perft, endgame, drop, FEN, EBF, speed, and agreement checks.
- Use multi-position suites, not one opening.
- Temporary counters leave before promotion binary.
- Support may veto. It cannot promote.
- Gate-false controls remain support-only and never dilute game SPRT.
- Re-run the fixed-depth node signature against all 38 shipped configurations
  from the P9 ledger, not the gate-positive subset. Gate-positive configs must
  hold identity. Gate-negative configs are not held to identity, since a phase
  touching move ordering or pruning is expected to move their restricted paths;
  they are held to monotonic non-regression, so their fixed-depth node count
  must not rise against the previous accepted phase. A rise is a support veto,
  the authority support already has. No extra SPRT, no game-pool dilution.

### Game mode

- Unset `ANEKAMACAM_SEED`.
- Match Hash, threads, time control, openings, and colors.
- Give every arm a disk-backed `TMPDIR` and keep its engine sandbox logs
  truncated. The sandboxes sit under `env::temp_dir()` and grow about 400 MB
  per engine every two hours; where `/tmp` is tmpfs that is memory, and a
  starved host stalls engines into response timeouts that the referee scores
  as engine losses. An arm that aborts that way is not slow evidence, it is
  no evidence: the first A-5 wave lost five arms to it on 2026-08-30, and both
  aborts named the second-spawned engine, so the starvation picks a side.
- Use built-in `debug-headless sprt`; add no wrapper.
- Pentanomial H0 = 0, H1 = phase floor, alpha = beta = 0.05.
- Start 12,000 games for +8 to +15 floor; 18,000 where tuning variance or draw
  rate warrants it.
- Extend inconclusive result in 6,000-game blocks with unchanged binaries,
  openings, time control, pool, and SPRT state.
- Representative affected pool decides first. After pooled H1, audit every
  capability-positive shipped configuration from P9 ledger.
- Any terminal negative affected-variant result rejects candidate.

Raw node delta against the pre-P6 baseline is a diagnostic, never a pass or a
fail. Law 12 already refuses that comparison, and `horde` is why: its forward
pruning was cutting branches its extinction rule made decisive, so the faster
baseline number was wrong rather than better. Read the P6 cost table as a map
of where `Q-5`, `R-5`, `U-5`, `V-5`, `W-5`, `X-5`, `AA-5`, and `AB-5` recover
ground, not as a debt P6 owes back.

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
| AA-5  | screened-movement reachability  | proposed |
| AB-5  | pass/counting null and exchange | proposed |

## Strength phases

Expected bands are priors. Low-confidence prior below floor is stated explicitly;
such candidate must clear floor or consume no letter.

Bands for phases whose payoff sits inside the configurations P6 restricted --
`D-5`, `Q-5`, `R-5`, `U-5`, `V-5`, `W-5`, `X-5` -- are priors against `base-5`
and its measured mask cost, not against the pre-P6 engine those numbers were
first drafted against. A smaller observed gain there is a smaller starting
board, not a failed campaign.

A phase carrying a **Capability close** step ends by re-deriving the one
capability bit its own mechanism addresses, then re-running the node-cost
bench. Flipping a bit is never justified by "the feature shipped". It is
justified by the reason the bit was cleared: the bit named something the engine
did not know, the phase taught it, and re-enabling the shortcut is then the
same argument that justified the feature rather than a new claim. Before any
flip, gate-enabled tactical and fixed-depth fixtures on the affected pool must
show no win missed by pruning past the decisive move. The close belongs to its
own phase and its own campaign. It consumes no letter.

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

**Capability close.** A hand makes a capture pay twice, which is why drops and
`promote to captured` clear `see_pruning` and `recapture_order` for the shogi
family, `grand`, and `sittuyin`. Once role-and-destination indexing prices the
second payment, re-derive both bits for those declarations and re-measure.

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

**Capability close.** Finish what `Q-5` opened: with reductions proved on the
drop tree, re-derive whether `see_pruning` and `recapture_order` remain unsafe
for drop and `promote to captured` declarations, and re-measure. If `Q-5`
already restored them, this step records that nothing is left to restore.

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

**Capability close.** `koth` lost `forward_pruning` and `quiet_pruning` because
a positionally quiet king walk decides the game with no material signal, which
is exactly what a static score cannot bound and a late-move cut assumes cannot
happen. Graph proximity to the goal zone puts that signal in the score. Once it
is there, re-derive both bits for declared winning goals and re-measure.

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

**Capability close.** `threecheck` and `fivecheck` lost `see_pruning`,
`recapture_order`, `forward_pruning`, `null_pruning`, and `quiet_pruning`
because a check is currency the score does not hold: a quiet checking move and
a materially losing capture both buy progress the search could not see. The
progress feature makes that visible. Re-derive each of those five bits for
declared `checks` and re-measure, keeping any bit whose reason the feature does
not answer.

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

**Capability close.** `extinction`, `kinglet`, and `horde` lost `see_valid` and
`forward_pruning` because a set one member above its threshold makes the piece
that leaves it decisive at a value material never assigned it. The last-stock
feature prices exactly that. Re-derive both bits for declared extinction sets
and re-measure. The E3 pool already carries all three, so `horde` closes here
too -- and its close is the one that may find nothing to restore, since it
searched fewer nodes without forward pruning, not more.

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

### AA-5 — Occupancy-aware reachability for screened movement

**Primary / expected.** Give the exchange simulation and the movement graph a
reachability model that reads the pieces standing in the way, so a leg that
unloads what it destroyed is priced as what it is: an attack that needs a
screen present and that a capture can therefore take away, not only reveal.
Restores `static_movement` and, with it, `see_valid` where nothing else clears
it. This is the mechanism `U-5` deferred rather than overlooked -- its graph is
pseudo-legal and occupancy-independent by construction, and a screened family
cannot be modelled inside that choice. Largest single pool of ground P6 gave
up: `janggi` +151%, `xiangqi` +55%, `minixiangqi` +51%, `sittuyin` +43% in
nodes. Expected +15 to +40 Elo on screened variants.

**Touch / derive.** `src/game/search/move_ordering.rs`,
`src/game/representations/state.rs`, `src/game/search/parameters.rs`. Unload
legs, screen occupancy, attacker enumeration order; P6 capability derivation.

**Support.** Exchange results on screened positions checked against an explicit
enumeration; perft and endgame fixtures for `xiangqi`, `minixiangqi`, `janggi`,
`sittuyin`; identity for every configuration whose `static_movement` was
already set; node-cost bench against `base-5`; per-node cost of the wider
attacker model.

**Promotion.** Screened-declaration pool, floor +10, 12,000 games.

**Fallbacks.** Screen-aware attacker enumeration with exchange ordering only
and no exchange pruning; single-screen families only; movement graph left
untouched and only the exchange model widened.

**Rollback.** Restore the occupancy-independent enumeration and the P6
derivation of `static_movement`.

### AB-5 — Pass and counting aware null and exchange capability

**Primary / expected.** One mechanism for two declarations that break the same
two shortcuts. Where a variant offers a real pass, searching that pass is a
legal move already in the list, so giving up the move needs no null and
bypasses no rule. Where a counting clock is running, both the null and a
materially losing capture are measurable once the frozen clock and its distance
to the declared limit are carried into the decision. Restores `null_pruning`
for `janggi` and `sittuyin`, and `null_pruning` with `see_pruning` for `makruk`
and `ouk-chaktrang`. Expected +10 to +25 Elo on pass and counting variants.

**Touch / derive.** `src/game/position/search.rs`,
`src/game/representations/termination.rs`,
`src/game/search/parameters.rs`. Zero-displacement quiet vectors, setup phase,
stand-off vetoes, `Counting` progress and limit; P6 capability derivation.

**Support.** Pass-search and null-search agreement on positions where both are
available; counting fixtures at and near the declared limit; identity for every
configuration whose `null_pruning` was already set; node-cost bench against
`base-5`.

**Promotion.** Pass and counting declaration pool, floor +8, 12,000 games.

**Fallbacks.** Pass-move search only, leaving counting bits cleared; counting
distance only, leaving pass variants on the cleared bit; restore
`see_pruning` for counting without touching `null_pruning`.

**Rollback.** Remove the pass-move search path and the counting distance term,
and restore the P6 derivation of both bits.

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
