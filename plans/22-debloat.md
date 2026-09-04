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
| D2    | unify const families, inline trivia        | −5      | done   |
| D3    | collapse five `populate_relevant_*`        | −86     | done   |
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

## D2 — unify split const families, inline trivia · done

Moved into the `prelude.rs` shared block so no family straddles the
prelude/module boundary:

- `*_DIR`: `PARAMS_DIR` (`game_io.rs`), `LOG_DIR` (`logger.rs`), joining
  `DATA_DIR`
- `EMBEDDED_*`: `EMBEDDED_PARAMS` (`game_io.rs`), joining the other three
- `OPT_*`: the five in `protocol.rs`, joining `OPT_THREADS`
- `HASH_*`: `HASH_MAX_MB` (`protocol.rs`), joining `HASH_DEFAULT_MB`

Inlined and deleted: `DEFAULT_PROTOCOL` and `LOG_HISTORY_KEEP`, one use
each. `DEFAULT_DROP` was on the list and is **kept** — it has two use
sites (`game_io.rs:1419`, `:1520`), so inlining would duplicate a magic
string. `SPRT_DIR` stays local: it belongs to `sprt.rs`'s cohesive
eight-const debug block, not to the runtime `*_DIR` family.

Only −5 lines. Moving a const does not delete it; this stage buys
coherence, and the ladder's line budget should not have counted it.

### Score-band retype: dropped, not deferred

The plan wanted `TABLE_MOVE_SCORE`, `KILLER_MOVE_SCORE`, and
`UNMAKEABLE_CAPTURE_SCORE` (`usize`) unified with
`WINNING_CAPTURE_SCORE`, `QUIET_MOVE_SCORE`, and `LOSING_CAPTURE_SCORE`
(`i32`), dropping the casts in `score_move!`. Not worth doing:

- The cast is not incidental. `score_move!` computes
  `(LOSING_CAPTURE_SCORE + see_score) as usize` with `see_score`
  negative, and correctness rests on the sum never going negative —
  `LOSING_CAPTURE_SCORE` is 983_617 and the `see_score == -INF` case is
  caught by the `UNMAKEABLE_CAPTURE_SCORE` branch above it. Under `i32`
  a negative sum would sort last; under `usize` it wraps and sorts
  first. Whether that is reachable depends on derived piece values, so
  the bound cannot be settled by inspection.
- `search.rs:917` does `scores[index] as i32 - LOSING_CAPTURE_SCORE` —
  a subtraction recovering the signed SEE score, which is exactly the
  "not a plain compare" case the plan said to split out.
- The win is six casts on existing lines and **zero deleted lines**.

Bad trade against a bit-identical gate. Skipped.

## D3 — collapse the five `populate_relevant_*` · done

The five bodies were identical modulo set type, generator, and target
field, and all four generators already share one signature:

    fn(&Piece, u32, &State, &[Vec<T>]) -> Vec<T>

so a generic `populate_relevant<T: Clone>` taking the generator as a
`fn` pointer replaces all five, with `precompute` naming the static it
fills. A macro was unnecessary. `MoveSet`, `DropSet`, and `PatternSet`
are all `Vec<_>` aliases, so `vec![Vec::new(); n]` is the same
initializer the five wrote out by hand.

`populate_relevant_attacks` folded into `precompute` at the same time —
three lines, one call site, and it sat inside the same doc region.

128 deletions, 42 insertions in `state.rs`.

Gates: the five bench variants plus `sittuyin` (12027791 nodes) and
`janggi` (301548) — new numbers, since neither had a prior baseline —
and `perft <variant> 3 --suite` across all seven, which is what actually
proves the tables: sittuyin 12/12, janggi 21/21, crazyhouse 12/12,
shogi 12/12, xiangqi 33/33, grand 3/3, standard 20256/20256.

## Deferred, not resolved in this ladder

- PST-residual / param-schema question — stays in plan 21.
- `game_phase` ratchet-vs-refresh divergence (`move_list.rs:2732-2741`
  ratchets with `cmp::max`, `util.rs:260-268` recomputes without it) —
  real bug, own `[SEMANTIC]` commit.
- Pawn table ignoring `setoption Hash` and surviving `ucinewgame` — the
  fix moves node counts, so it follows D14 as `[SEMANTIC]`.
