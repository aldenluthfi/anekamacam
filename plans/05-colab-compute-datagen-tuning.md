# Leverage Colab Pro compute for datagen + tuning

## Context

User has Colab Pro (~90 compute units) and wants to use it for the engine's
compute-heavy work (texel-tuning datasets, strength arbitration).

Two hard facts from exploration shape the whole plan:

1. **The engine is pure HCE — no NN, no GPU workload.** Search, eval, datagen,
   and the existing texel tuner (Adam, already rayon-parallel) are all CPU-bound.
   A GPU/TPU runtime accelerates *nothing* here and burns compute units fast.
   GPU units only pay off if an NNUE track is started (a separate, later stage).
   **Maximizing units = using Colab high-RAM CPU runtimes, not GPU.**

2. **`datagen`/`tune`/`sprt` are reachable only through the ratatui TUI**
   (`src/debug/console.rs:3574` `ratatui::init()`, raw mode, keyboard-driven
   variant picker). Colab has no TTY, so the TUI cannot init. The underlying
   `run_datagen` / `run_tuning` functions are already clean and `pub` — they
   just have no non-TUI caller.

Decisions (confirmed with user): pursue **both tracks, staged** — CPU datagen +
SPRT now, NNUE later; and **add headless subcommands** (not PTY-drive the TUI).

Throughput note: datagen plays games *sequentially*; `threads` only parallelizes
each search. For dataset *volume* you want many concurrent 1-thread processes,
not one many-thread process. Per-process game IDs are `0..N` (`datagen.rs:195`),
so parallel shards collide and must be reindexed on merge.

## Stage 1 — Headless entry points (engine change)

Precedent already exists: `main.rs:102` dispatches `derive` →
`derive_all_variants()`, a headless subcommand that loads variants via
`parse_config_file("{variant}.conf")`. Mirror it exactly.

### `src/main.rs`

Add two dispatch arms + two helpers next to `derive_all_variants` (`main.rs:114`):

```
Some("datagen") => run_datagen_headless(&args),   // datagen <variant> <games> <movetime_ms> [threads=1]
Some("tune")    => run_tune_headless(&args),      // tune <variant> <epochs> [lr=1.0]
```

`run_datagen_headless`:
1. Parse `variant`, `games`, `movetime_ms`, optional `threads` (default **1** —
   one game per process is the sharding unit; log usage + return on bad args).
2. `let mut state = parse_config_file(&format!("{variant}.conf"));` — this is the
   same loader the TUI and `derive` use; it derives/loads params into `State`.
3. Build tables: `Arc::new(TTable::default())`, `QTable`, `PTable`.
4. Build a **drained** event channel so `run_datagen`'s `sender.send(...)` never
   blocks or panics headless:
   ```
   let (sender, receiver) = channel::<TuiEvent>();
   thread::spawn(move || { while receiver.recv().is_ok() {} });
   ```
5. `run_datagen(&state, variant, None, ttable, qtable, ptable, threads, games,
   movetime, &sender);` — `dict = None` (dict only feeds move-name logs).

`run_tune_headless`: parse `variant`, `epochs`, optional `lr`; load `state` as
above (mut); call `run_tuning(&mut state, variant, epochs, lr)` (no tables/sender
needed). Reuses the existing `res/data/{variant}/latest.data` reader and
`res/param/{variant}/latest.param` exporter unchanged.

Keep both helpers in `main.rs` (matches `derive_all_variants` placement); all
needed items (`channel`, `thread`, `Arc`, `TuiEvent`, `run_datagen`,
`run_tuning`, `parse_config_file`, table types) are already in the prelude.

No change to `datagen.rs`, `tuning.rs`, or the TUI. Update the `main` doc comment
(`main.rs:83-88`) to list the new modes.

## Stage 2 — Parallel sharding + merge (no engine change)

Each shard process writes the fixed relative path `res/data/{variant}/latest.data`
and rolls the previous file, so shards must run in **separate working dirs**. The
release binary is self-contained (configs/params embedded via `include_dir!`), so
a shard only needs a writable cwd.

### `tools/merge_shards.py` (new)

Reindex + concatenate shard datasets into one game-disjoint corpus:
- Read each `shard_*/res/data/{variant}/latest.data`.
- Maintain a global counter; map each shard-local `game` id to a fresh unique id
  (first-seen order), rewrite rows as `newid;FEN;result`.
- This preserves the `game_id % TUNING_VALIDATION_MODULUS` (=5) train/validation
  split's ~1/5 ratio while guaranteeing no cross-shard id collision.
- Write merged file to `res/data/{variant}/latest.data` at repo root.
- Print per-variant row/game/result counts (sanity mirror of `load_dataset`).

## Stage 3 — Colab notebook (new)

### `tools/colab_datagen.ipynb` (new)

Cells, documented to use a **CPU high-RAM runtime (no GPU)** to conserve units:
1. Install rustup (edition-2024 needs a recent stable) + `git clone` the repo.
2. `cargo build --release` (~1-3 min parallel; 5 MB binary).
3. Detect cores `N`; for each variant, launch `N` shard processes:
   `mkdir -p shard_k && (cd shard_k && ../target/release/anekamacam datagen
   <variant> <games> <movetime> 1)` — backgrounded, then `wait`.
4. Run `tools/merge_shards.py <variant>` → root `res/data/{variant}/latest.data`.
5. Optional: `target/release/anekamacam tune <variant> <epochs>` to produce a
   tuned `latest.param` in-notebook.
6. Persist outputs: download / push `latest.data` + `latest.param` (or commit).

Notebook notes to include: Colab Pro **background execution** allows ~24 h
unattended sessions — ideal for overnight multi-variant datagen or the deferred
round-robin SPRT arbitration; multiple Google accounts multiply throughput since
each Colab session is a single VM (~2-8 vCPU), not a cluster.

(Optional, same track) a `tools/datagen_local.sh` doing the same shard+merge on
the user's Mac, so the workflow isn't Colab-exclusive.

## Stage 4 — NNUE / GPU track (later, sketch only — do NOT build now)

Recorded direction, deferred: the merged `latest.data` (FEN + white-view result)
is already a training corpus. A later branch can export it to PyTorch and train a
small NNUE on a Colab **GPU** runtime — the one workload that justifies GPU units.
This is a major departure from the variant-agnostic HCE design and is out of
scope for this change; keep it as a future exploratory branch once datasets exist.

## Critical files

- `src/main.rs` — new `datagen`/`tune` dispatch arms + two headless helpers
  (mirror `derive_all_variants`, `main.rs:114`).
- `tools/merge_shards.py` (new) — game-id-reindexing shard merge.
- `tools/colab_datagen.ipynb` (new) — build + parallel shard + merge + tune.
- `tools/datagen_local.sh` (new, optional) — local equivalent.

Reused unchanged: `run_datagen` (`datagen.rs:145`), `run_tuning`
(`tuning.rs:700`), `parse_config_file`, `load_dataset`'s `game;FEN;result`
schema and `latest.data` path, `roll_latest` backup rotation.

## Verification

```bash
cargo build --release
# headless datagen writes the dataset with no TTY:
target/release/anekamacam datagen fide 4 50 1 < /dev/null
wc -l res/data/fide/latest.data          # non-empty; rows are game;FEN;result
head -1 res/data/fide/latest.data        # e.g. 0;<fen>;1
# shogi canary (memory: shogi is mandatory canary):
target/release/anekamacam datagen shogi 4 50 1 < /dev/null
# headless tune consumes it and exports params:
target/release/anekamacam tune fide 3    # logs split/MSE/epoch, rolls latest.param backup
# merge: fabricate two shard files with overlapping ids, merge, assert unique
# ids and that validation-class (id%5==0) count is ~1/5:
python3 tools/merge_shards.py fide
```

End-to-end: run the notebook on a Colab CPU runtime for one small variant, confirm
merged `latest.data` and a tuned `latest.param` come back and load in the engine
(`target/release/anekamacam uci` → `position`/`go` on that variant, no panic).

Constraints honored: build only `--release`; no `#[test]` modules; shogi canary
covered; `parse_config_file`/`parse_fen` internal loads unchanged. Adding
`datagen`/`tune` subcommands is consistent with the existing `derive` headless
subcommand — it exposes existing debug tooling, adds no new engine capability.
