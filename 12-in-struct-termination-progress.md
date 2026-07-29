# Context

Plan 12 moves termination configuration and mutable rule progress from shared
`StaticState` and loose `State` fields into per-position `Termination`. Draft
predates restored CPMN stand-off support: it wrongly puts per-move
`gave_check` inside `Checks` and proposes deleting live pattern code.

Goal: execute Plan 12 while preserving rule behavior, search-local repetition,
stand-off legality, current dirty work, and allocation-free make/undo. Snapshot
flags become optional post-move facts so expensive checks run only when needed.

## Key decisions and invariants

- `Termination` owns configured rules, eager `game_result`, and cumulative rule
  progress.
- `Snapshot` owns per-move facts. `in_check` and `in_stand_off` are
  `Option<bool>`; `None` means deliberately not evaluated.
- Flag meanings when present:
  - `in_check`: side to move after the move is checked; the mover gave check.
  - `in_stand_off`: resulting board remained in declared stand-off.
- Mover self-check is a local optional legality result, not history metadata.
- `Checks` stores only cumulative `delivered: [u8; 2]`; no duplicate check
  flag.
- Accepted stand-off pass remains highest-priority eager terminal, but
  `position_terminal` decides it. `make_move!` has one terminal path.
- Keep CPMN stand-off modules, `PatternSet`, parser/precompute functions,
  exports, config sections, and bit 7. They now have active callers.
- Keep search's inline twofold repetition cutoff unchanged. Full configured
  repetition/perpetual remains on demand in `game_outcome`.
- Preserve eager priority as: accepted stand-off, N-check, goal, extinction,
  double pass/adjudication, counting, counter.

## Implementation

### 1. Update repo plans before changing ownership

Update this plan with optional snapshot flags, centralized stand-off
adjudication, `Checks::delivered`, retained CPMN pattern code, current fixture
count, and revised critical files. Remove stale pattern-shell deletion work.

Update matching move/terminal description in
`plans/10-standoff-rectification.md`: accepted pass still bypasses ordinary
self-check legality, but snapshot metadata feeds `position_terminal` instead of
`make_move!` assigning adjudication directly.

### 2. Make `Termination` per-position owner

Modify `src/game/representations/termination.rs`:

- Derive `Clone` for `Termination` and every contained rule type.
- Add `game_result: u8` to `Termination`.
- Add `clock: u8` to `Counter`.
- Add `progress: Option<(u16, u16)>` to `Counting`.
- Add `delivered: [u8; 2]` to `Checks`; keep configured `count` unchanged.
- Add `reset_progress`, resetting only result and runtime progress while
  retaining configured rule data.
- Rewrite all detectors and outcome helpers to use `state.termination`.
- In `position_terminal`, detect accepted stand-off from last pass plus
  `last.in_stand_off == Some(true)`, then use existing
  `adjudicate_outcome` with draw fallback. Read N-check fact from
  `last.in_check == Some(true)` and cumulative count from
  `checks.delivered`.
- Keep perpetual-check discovery recomputed on demand in `offence_set`; do not
  force eager snapshot `in_check` work for perpetual-only variants.
- Preserve repetition/perpetual history walking and result restoration, now via
  `state.termination.game_result`.
- Replace `side_is_bare` scan with existing major/minor counters.

### 3. Reshape `State` and `Snapshot`

Modify `src/game/representations/state.rs`:

- Remove `termination` from `StaticState`; add `termination: Termination` to
  `State`.
- Remove loose `game_result`, `halfmove_clock`, `counting`, `gave_check`, and
  `check_count` fields.
- Keep reversible result/counter/counting/check-count scalars in `Snapshot`.
  Add optional `in_check` and `in_stand_off` current-move metadata; these flags
  are not restored during undo.
- Initialize flag defaults to `None`.
- Clone `Termination` in `State::clone`.
- Build default termination in `from_statics`, call `reset_progress` from
  `reset`, and clone template termination in `fork` before `load_fen` resets
  runtime progress.
- Point `is_terminal!` at `state.termination.game_result` and tighten docs to
  eager, position-local truth.
- Delete `game_over!`; reporting/self-play callers use `game_outcome` directly.
- Update ownership docs and static-field descriptions without disturbing
  restored stand-off tables.

### 4. Update make/undo with optional post-move facts

Modify `src/game/moves/move_list.rs`:

- Save only prior termination result/progress scalars into snapshot fields.
- Update `Counter::clock`, `Counting::progress`, and `Checks::delivered` in
  place, preserving reset, saturation, frozen-limit, and start-count behavior.
- Compute board-derived values before mutable rule borrows.
- Read `stand_off_before` from previous snapshot's `in_stand_off`; only scan
  board when history is empty or snapshot has no cached value.
- After applying move, compute mover self-check and `stand_off_after` with same
  legality expression as restored stand-off implementation.
- Compute snapshot `in_check: Some(...)` only when N-check tracking requires
  it; otherwise store `None`. Increment `Checks::delivered` only on
  `Some(true)`.
- Store `stand_off_after` in snapshot when stand-offs are enabled, so next move
  reuses it as `stand_off_before`.
- Push snapshot, reject illegal moves through normal undo, then always call
  `position_terminal` for legal moves. Accepted stand-off remains first branch
  there and uses existing adjudication with draw fallback.
- Restore rule progress through `Termination` options during normal/null undo;
  per-move flags disappear naturally when snapshot is popped.
- Null snapshots use `None` for both flags and preserve cumulative termination
  progress.

### 5. Migrate I/O, search, evaluation, and debug paths

Modify `src/io/game_io.rs`:

- Assign parsed table to `result.termination` and seed new runtime fields in
  rule literals.
- Read/write FEN counter clock through `state.termination.counter`, while still
  consuming optional halfmove field positionally when no counter exists.
- Update termination formatters and presence checks to state-owned paths.

Update direct paths in:

- `src/game/position/evaluation.rs`
- `src/game/position/search.rs`
- `src/debug/console.rs`
- `src/debug/sprt.rs`

Use `state.termination.game_result` and `state.termination.<rule>`. Replace
console play-loop `game_over!` call with `game_outcome(state).0 == ONGOING`.
Keep search's no-move scoring, drop-checkmate reversal, and twofold repetition
cutoff semantically unchanged.

Do not delete or remove exports for:

- `src/game/moves/pattern_match.rs`
- `src/game/moves/pattern_parse.rs`
- `src/game/representations/pattern.rs`
- `src/main.rs`
- `src/prelude.rs`

### 6. Scope guard

Do not alter variant configs, outcome-score macros, unrelated no-move blocks,
config grammar, `.gitignore`, parameter files, or datagen tooling. Preserve all
pre-existing dirty changes, including current parameter deletion, without
restoring or folding them into this work.

## Verification

1. Run `git diff --check`; inspect executable lines for <=80 columns and preserve
   user edits.
2. Run `cargo build`; require clean compilation with no new warnings.
3. Run `cargo build --release`.
4. Run `./target/release/anekamacam derive` to parse every embedded config and
   restored CPMN stand-off table.
5. Run `tools/run_endgame_fixtures.sh`; require all 32 cases, including N-check,
   counting, repetition/perpetual, double-pass adjudication, and both accepted
   bikjang cases.
6. Through UCI, run `go perft <depth>` for janggi stand-off positions from
   `res/perft/janggi.perft` and compare expected totals; run representative
   standard and drop-variant divides. Do not script ratatui console.
7. Run a low-depth janggi search from stand-off FEN to catch terminal/parity
   regressions.
8. Search final tree for no `game_over!`, no `statics.termination`, and no loose
   termination progress fields on `State`.
9. Review final diff to confirm CPMN pattern files/exports remain live and
   unrelated config, fixture, search architecture, and parameter changes were
   not modified.

## Critical files

- `12-in-struct-termination-progress.md`
- `plans/10-standoff-rectification.md`
- `src/game/representations/termination.rs`
- `src/game/representations/state.rs`
- `src/game/moves/move_list.rs`
- `src/io/game_io.rs`
- `src/game/position/search.rs`
- `src/game/position/evaluation.rs`
- `src/debug/console.rs`
- `src/debug/sprt.rs`
