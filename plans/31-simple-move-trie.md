# Simple move trie

## Status

Opened 2026-10-13 on build 22 (main 78deaff), branch `plan31-trie`.

## Context

Plan 30 built a move trie and closed it: the tries of the squares added
memory, and one trie for each piece walked 4 to 9% slower than the
vector tables. The user read its design and found it over-built:

- a piece moves the same on every square, so one trie for each piece,
  pruned where a leg leaves the board
- the order of the moves does not matter; move ordering is the search's
- a node needs few fields: its children, whether a vector ends on it,
  and whether a capture ends on or under it
- a CPMN condition belongs to the traversal: a branch and all its
  children share one condition, tested once when the walk enters it

Plan 30 never profiled its walk, so its slowness has no measured cause.
This plan builds the user's design and profiles it before any games.

## Design

`src/game/moves/move_trie.rs`, one trie for each piece:

- `TrieNode { leg, children, can_end, has_capture }`
  - `leg`: the step of the node; children are told apart by it
  - `children: Vec<TrieNode>`
  - `can_end`: a vector ends on this node
  - `has_capture`: a capture vector ends on or under it
- `MoveTrie = Vec<(TrieNode, Option<Arc<PatternSet>>)>`: the trie of one
  piece, its branches, each a root with the CPMN condition all its nodes
  share (the user's layout); a vector with another condition starts its
  own branch. `StaticState::move_tries` holds one for each piece.
- The walk is a plain depth-first recursion. A node is skipped with all
  its children when the condition of its branch fails, when its leg
  leaves the board or enters a forbidden zone it may not pass, or when
  its leg is blocked. Capture generation also skips a node without
  `has_capture`.
- A leg reads differently only as the last leg of a vector (a last leg
  without `m` may capture). The walk reads a node once as a middle leg;
  a second read as a last leg runs only when a vector ends there and the
  square holds a piece or is the en passant square, where the two reads
  can differ.
- The leg rules stay in `walk_leg!` (plan 30 stage 1, badad6e), and
  `match_pattern!` tests the bounds (plan 30 stage 3).
- No per-square vector tables (asked by the user, from plan 30 stage 4):
  `StaticState` keeps `piece_moves`, and the load passes (attack masks,
  attack recipes, capture reach, the derivation in `parameters.rs`) ask
  `fitted_vectors` and `fitted_captures` for a piece's vectors fitted to a
  square. In plan 30 this gave the same load logs in all 44 variants and
  took taikyoku from 13.6 to 7.4 GB, with a slower load (76 s).
- `relevant_attacks` stays: it is read backwards from a target square,
  and a backward trie would be its own plan.

## Stages

1. Branch on build 22 with the leg macros of plan 30 stage 1, the walk
   state with file and rank, the bounds in `match_pattern!` and
   `leg_lands!`.
2. `move_trie.rs` as above; generation walks it.
3. Profile with the `hotpath` feature on the server: time in move
   generation, build 22 against the trie, on the same bench; perft time
   (the same tree for both) in the key variants.
4. Act on the profile, then games against full FSF if the walk is faster.

## Verification

- Perft equals build 22 in the 17 suites, chushogi 4, daishogi 4,
  daidaishogi 3, taikyokushogi 2 (sorted lines, as the order changes).
- Profile and perft time as in stage 3; bench speed.
- Games: build 22 and the trie against full FSF, three interleaved rounds
  of 200 in each key variant, merged at +2σ on the mean.
