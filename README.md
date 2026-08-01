<picture>
  <source media="(prefers-color-scheme: light)" srcset="/.github/meta/dark.png">
  <source media="(prefers-color-scheme: dark)" srcset="/.github/meta/light.png">
  <img alt="AnekaMacam">
</picture>

<pre>
[ ABOUT ]

A chess engine that can (hopefully) play most chess variants. It speaks UCI on the surface, but under the hood it is built on a stack of custom notations — a move notation, a modified move-pattern notation, a drop notation, and an expandable, fully data-driven variant configuration format. Define a board, a set of pieces and their movement, and a handful of rule flags, and the engine will play it.

Written in Rust as a Cargo workspace. "Aneka macam" is Indonesian for "all sorts" — which is the point: all sorts of chess.

[ FEATURES ]

Variant-agnostic core  --> board sizes, piece sets, drop rules, and
                           royal-piece definitions are all configurable;
                           no FIDE-specific assumptions baked in.
Data-driven variants   --> each variant is a plain .conf file; default
                           variants are embedded into the binary.
Custom notations       --> CFEN / CKN / CPMN / CDN (see below).
Search                 --> parallel search (rayon), transposition table,
                           move ordering, and per-variant tunable
                           pruning parameters.
Evaluation             --> piece-square tables and parameters derived
                           per-variant (avg piece value, board size,
                           game phase).
Protocol translation   --> dictionaries convert CFEN to/from target
                           protocol FENs (UCI, USI-style, etc.).
Debug tooling          --> graphical ratatui frontend, nested headless
                           commands, logging, and perft test suites.

[ NOTATIONS ]

┌──────────┬─────────────────────────────────┬────────────────────────────────────┐
│ Notation │ Expands To                      │ Purpose                            │
├──────────┼─────────────────────────────────┼────────────────────────────────────┤
│ CFEN     │ Cheesy Forsyth–Edwards Notation │ Board position &amp; full game state   │
│ CKN      │ Cheesy King Notation            │ Move patterns (Betza-style atoms)  │
│ CPMN     │ Cheesy Pattern Match Notation   │ Drop legality &amp; stand-off matching │
│ CDN      │ Cheesy Drop Notation            │ Per-piece drop rules               │
└──────────┴─────────────────────────────────┴────────────────────────────────────┘

CFEN extends ordinary FEN with board dimensions, pieces-in-hand, and the extra state that variants need (e.g. setup phase, drops). Move patterns are written in CKN, a Betza-like atom language. For example, the FIDE pawn's full move set is:

    Pp:mnW|im&lt;nW-pnW&gt;|tcnF

Drops and stand-offs are matched with CPMN; per-piece drop rules use CDN.

[ VARIANT CONFIGURATION ]

A variant lives in a single configs/&lt;name&gt;.conf file. It declares, in labelled sections, the title, the starting CFEN, the enabled rules, and the pieces with their CKN move patterns. Supported rule flags:

┌─────────────────────┬──────────────────────────────────────────────────────────┐
│ Rule                │ Effect                                                   │
├─────────────────────┼──────────────────────────────────────────────────────────┤
│ castling            │ Enables castling; rights are read from the CFEN          │
│ en passant          │ Enables en passant; target square read from the CFEN     │
│ promotions          │ Pieces may promote on reaching a promotion zone          │
│ drops               │ Pieces held in hand may be dropped instead of moving     │
│ forbidden zones     │ Marks squares no piece may enter (move/promo/drop)       │
│ promote to captured │ A promoting piece must be one the opponent captured      │
│ setup phase         │ Players drop pieces from hand before normal play starts  │
│ stand-offs          │ A facing/formation the mover must break, or pass to end  │
└─────────────────────┴──────────────────────────────────────────────────────────┘

How a game is won, lost, or drawn is declared separately, in the = termination = section: a flat table of parametric terminal rules — checkmate/stalemate outcomes, repetition, a generalized counter (50-move / makruk board's honour), a bare-king material count (makruk-family pieces' honour), N-check, material extinction, goal zones, repetition-cycle offences, and material adjudication. Each covers a family of variants rather than a single named mode:

┌─────────────┬────────────────────────────────────────────────────────────────┐
│ End rule    │ Effect                                                         │
├─────────────┼────────────────────────────────────────────────────────────────┤
│ checkmate   │ Outcome of a no-move position in check (default loss)          │
│ stalemate   │ Outcome of a no-move position not in check (default draw)      │
│ repetition  │ Outcome once a position occurs N times (default draw)          │
│ counter     │ 50-move / board's-honour progress count (default draw)         │
│ counting    │ Bare-king material budget: mate in time, else draw (makruk)    │
│ checks      │ The side giving its Nth check gets the outcome (three-check)   │
│ extinct     │ A colour losing all of a piece set gets the outcome            │
│ goal        │ A piece reaching a named zone gets the outcome (KotH, racing)  │
│ perpetual   │ Sole perpetual checker/chaser in a repetition loses (xiangqi)  │
│ adjudicate  │ Both sides pass: decide by weighted material (janggi points)   │
└─────────────┴────────────────────────────────────────────────────────────────┘

Each rule, when enabled, requires its matching section (e.g. castling geometry, promotion zones, drop rules) and a correctly-formatted CFEN — the parser validates this and errors out otherwise. See <a href="configs/example.conf">configs/example.conf</a> for a fully commented reference of every section. Protocol translation is configured per variant in res/dicts/&lt;name&gt;.dict; evaluation parameters live in res/param/.

[ SUPPORTED VARIANTS ]

Variants bundled in configs/ (the names double as their config files):

┌───────────────┬───────────────────────────────────┐
│ Config        │ Variant                           │
├───────────────┼───────────────────────────────────┤
│ standard      │ Standard (FIDE) Chess             │
│ capablanca    │ Capablanca Chess (10×8)           │
│ grand         │ Grand Chess (10×10)               │
│ los-alamos    │ Los Alamos Chess (6×6)            │
│ berolina      │ Berolina Chess (diagonal pawns)   │
│ crazyhouse    │ Crazyhouse (captured pieces drop) │
│ shatranj      │ Shatranj (medieval Persian chess) │
│ xiangqi       │ Xiangqi (Chinese chess)           │
│ minixiangqi   │ Mini Xiangqi (7×7)                │
│ janggi        │ Janggi (Korean chess)             │
│ shogi         │ Shogi (Japanese chess)            │
│ minishogi     │ Mini Shogi (5×5)                  │
│ makruk        │ Makruk (Thai chess)               │
│ sittuyin      │ Sittuyin (Burmese chess)          │
│ ouk-chaktrang │ Ouk Chaktrang (Cambodian chess)   │
│ tjatoer       │ Tjatoer                           │
│ threecheck    │ Three-Check Chess                 │
│ fivecheck     │ Five-Check Chess                  │
│ koth          │ King of the Hill                  │
│ kinglet       │ Kinglet (capture all enemy pawns) │
│ extinction    │ Extinction Chess                  │
│ horde         │ Horde (pawn army vs full side)    │
└───────────────┴───────────────────────────────────┘

example.conf is the documented template you copy when authoring a new variant. res/perft/ holds perft suites used to validate move generation.

[ USAGE ]

Build with Cargo (release is strongly recommended — the dev profile is much slower):

    cargo build --release

The workspace produces a binary named anekamacam. It takes a frontend command:

    anekamacam uci                       # protocol mode (also the default)
    anekamacam debug-graphics            # graphical debug frontend
    anekamacam debug-headless help       # non-graphical debug/tool commands

`debug-headless` uses nested one-shot commands. Position commands accept `--protocol`, a quoted `--fen`, and `--moves`:

    anekamacam debug-headless state standard
    anekamacam debug-headless movegen xiangqi --protocol ucci
    anekamacam debug-headless evaluate makruk
    anekamacam debug-headless search standard 8 1
    anekamacam debug-headless play minishogi 4 0.1 1 256
    anekamacam debug-headless see standard e4d5 --protocol uci \
        --fen "4k3/8/8/3p4/4P3/8/8/4K3 w - - 0 1"

Direct perft runs one position and prints a root divide by default. Add `--suite` to run the embedded reference suite and `--limit` to cap positions:

    anekamacam debug-headless perft standard 5
    anekamacam debug-headless perft standard 3 --suite --limit 100

Long-running tools use the same umbrella:

    anekamacam debug-headless derive
    anekamacam debug-headless datagen standard 100 50 1
    anekamacam debug-headless tune standard 100 1.0
    anekamacam debug-headless sprt standard old-bin new-bin 100

With no frontend command the engine starts in protocol mode. Default configs, dictionaries, perft suites, and parameters are embedded in the binary, so it runs standalone.

[ PROJECT LAYOUT ]

    src/
    ├── main.rs                  # Entry point + frontend dispatch
    ├── prelude.rs               # Project-wide re-exports
    ├── game/
    │   ├── representations/     # board, piece, move, drop, pattern, vector, state
    │   ├── moves/               # move & drop generation, pattern parse/match
    │   ├── search/              # parallel, transposition, ordering, parameters
    │   ├── position/            # evaluation, hash, search
    │   └── util.rs
    ├── io/
    │   ├── board_io / piece_io / game_io / move_io
    │   ├── logger.rs
    │   └── protocols/           # uci, translation
    └── debug/
        ├── graphics.rs          # graphical debug frontend
        ├── headless.rs          # nested headless command dispatcher
        ├── datagen.rs           # self-play dataset generation
        ├── tuning.rs            # Texel parameter tuning
        └── sprt.rs              # engine match runner
    configs/                     # per-variant .conf files
    res/
    ├── dicts/                   # protocol translation dictionaries
    ├── param/                   # evaluation parameter sets
    └── perft/                   # perft test suites
    util/                        # benchmarking & tuning notes

[ LICENSE ]

This repository is licensed under the <a href="LICENSE">GNU General Public License v3.0</a>.

With this license, you have the freedom to:
- Use, study, and run the software for any purpose
- Modify the software and adapt it to your needs
- Distribute copies of the original and your modified versions

However, if you distribute this software (modified or unmodified), you must:
- Provide the source code under the same GPL-3.0 license
- Include copyright notices and license information
- Make your modifications available under GPL-3.0
</pre>
