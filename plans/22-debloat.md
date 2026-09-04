# Plan 22 — debloat pass

Goal: large red diff, same behaviour, one revertible commit per stage.
Plan 21's restore ladder stays paused at R6 until this lands.

## Scope decisions

1. **Const rule** — shared up, families unified, trivia inlined. Prelude
   keeps cross-file consts; the four families straddling the
   prelude/module boundary (`OPT_*`, `*_DIR`, `EMBEDDED_*`, `HASH_*`) get
   unified into prelude; file-private detail stays local; single-use
   trivia is inlined and deleted.
2. **Behaviour** — every refactor commit must reproduce bench node counts
   exactly. Semantic fixes land as separate `[SEMANTIC]` commits.
3. **Ordering** — debloat first, then plan 21 R7-R11.
4. **`move_list.rs`** — out of scope, except D11's two mechanical lines.

## Baseline

Captured on `8e57c1a`, clean tree, release build.

    ANEKAMACAM_SEED=42 ./target/release/anekamacam \
        debug-headless bench <variant> <depth> --limit <n>

| variant    | depth | limit | nodes   | nps     |
|------------|-------|-------|---------|---------|
| standard   | 11    | 16    | 190760  | 4307297 |
| shogi      | 9     | 8     | 1391464 | 1697568 |
| xiangqi    | 10    | 8     | 369526  | 1237322 |
| crazyhouse | 9     | 8     | 1009750 | 1248544 |
| grand      | 9     | 8     | 1432374 | 1160025 |

Stages touching derivation, `StaticState`, or `State::new` — D3, D11,
D12, D13 — additionally run `sittuyin 9 --limit 8` and
`janggi 9 --limit 8`. Never run `derive` while verifying: it overwrites
`res/param/*/latest.param` and shifts every later bench.

## Ladder status

| Stage | What                                       | Δ lines | Status |
|-------|--------------------------------------------|---------|--------|
| D1    | delete unreachable code                    | −170    | done   |
| D2    | unify const families, inline trivia        | −40     | todo   |
| D3    | collapse five `populate_relevant_*`        | −80     | todo   |
| D4    | io dedupe                                  | −230    | todo   |
| D5    | TT/QT unification                          | −200    | todo   |
| D6    | search dedupe                              | −25     | todo   |
| D7    | `graphics.rs` idiom dedupe                 | −350    | todo   |
| D8    | `move_parse` small dedupe                  | −25     | todo   |
| D9    | fold single-caller helpers, `game/`        | −200    | todo   |
| D10   | fold single-caller helpers, `io/`+`debug/` | −180    | todo   |
| D11   | fold `has_castled` into `castling_state`   | −5      | todo   |
| D12   | group `StaticState` eval/search fields     | −25     | todo   |
| D13   | move param structs out of `state.rs`       | +1      | todo   |
| D14   | three `thread_local!`s onto `SearchInfo`   | −10     | todo   |
| D15   | `move_parse` atomic/multi_leg unification  | −500    | needs  |
|       |                                            |         | go-ahead |

Branchless rewrites are a running rule applied inside whichever stage
already touches the line, never a standalone sweep. Transform only when
both arms are side-effect-free, cannot panic, and are cheap; neither arm
guards an expensive call such as `see!`; and the result is shorter.
Prefer a mask over a multiply for wide types.

## D1 — delete unreachable code · done

Removed, with their prelude re-exports:

- `vector.rs` `set_atomic`, `set_whole`
- `termination.rs` `has_repetition` (its null-move paragraph merged into
  `count_repetitions`, which the deleted doc had cross-referenced)
- `util.rs` `benchmark_search`
- `logger.rs` `configured_log_level`, `verbosity_enabled!`
- `board.rs` `toggle!`, `xor!`, `not!`
- `moves.rs` `m_pseudocapture!`, `enc_captured_unmoved!`

197 deletions, 27 insertions across 7 files. All five bench variants
reproduce the baseline node counts exactly.

### Why `enc_captured_unmoved!` is dead despite a live decoder

Worth recording, because the bit-111 flag looks unwritten and is not.

`Snapshot` (`move_list.rs:2774-2789`) carries `virgin_hash` but **not**
`virgin_board`, so undo restores the board incrementally, guarded by the
captured-was-unmoved flag at `move_list.rs:3093` (single capture) and
`:3312` (multi). Only `enc_multi_move_captured_unmoved!` is ever called
— `:1047`, `:1092`, `:1129` — which sets bit 33 of a 34-bit payload
word. There is no single-capture call site, yet the flag still arrives:

- `move_list.rs:720-830` is a **validator** (`valid` / `break`); it never
  builds a move. Its three `capt_unmoved` reads only feed the Betza
  `v`/`not_v` capture predicate (`vector.rs:91`, bit 21).
- The builder is `process_multi_leg_vector!`, and it pushes a payload
  word per capture regardless of how many there turn out to be.
- At `:1173-1181` the move type is chosen by `$scratch.len()`. For one
  capture it emits `enc_capture_part!(encoded_move, $scratch[0])`, which
  is `|= (payload & 0x3_FFFF_FFFF) << 78`. Payload bit 33 lands at bit
  111 — exactly what `captured_unmoved!` reads.

So single captures inherit the flag by payload aliasing, and a dedicated
encoder would be a second way to write the same bit. The aliasing is now
stated in the `enc_capture_part!` doc block.

`legal_moves!` at `move_list.rs:317-318` is the castling builder and
needs no flag: castling undo (`:3501`) restores virgin unconditionally.

## Deferred, not resolved in this ladder

- PST-residual / param-schema question — stays in plan 21.
- `game_phase` ratchet-vs-refresh divergence (`move_list.rs:2732-2741`
  ratchets with `cmp::max`, `util.rs:260-268` recomputes without it) —
  real bug, own `[SEMANTIC]` commit.
- Pawn table ignoring `setoption Hash` and surviving `ucinewgame` — the
  fix moves node counts, so it follows D14 as `[SEMANTIC]`.
