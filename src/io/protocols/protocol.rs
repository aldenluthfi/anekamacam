//! protocol.rs
//!
//! The shared engine loop behind every text protocol the engine speaks.
//!
//! UCI, USI, and UCCI differ only in a handshake word, a few dialect `go`
//! clauses, and the notation their dictionaries emit; everything else -- the
//! position, the variant list, the hash tables, the search threads, and the
//! ponder plumbing -- is identical. That common machinery lives here as the
//! `Session` and the `execute_common` dispatcher. A protocol is a tiny
//! `Protocol` implementor that intercepts only the lines that behave
//! differently and defers the rest.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi
use crate::*;

/// Protocol
///
/// One text protocol the engine speaks.
/// The whole session engine is shared; an implementor supplies only its
/// name (from which the handshake word, the `...ok` reply, and the variant
/// option name all derive) and an `execute` that consumes the handful of
/// lines whose behaviour is dialect-specific, deferring everything else to
/// `execute_common`.
pub trait Protocol {
    /// name
    ///
    /// The protocol's identifier: the dictionary section key passed to
    /// `Translator::find`, the handshake command, and the stem of both the
    /// `...ok` reply and the `<NAME>_Variant` option.
    ///
    /// Return:
    /// &str -> the protocol name, e.g. "uci"
    fn name(&self) -> &str;

    /// execute
    ///
    /// Handles one input line the universal dispatcher left unclaimed: the
    /// protocol's own handshake, its new-game word, and its `go` dialect.
    /// `execute_common` runs first and serves every protocol-independent
    /// command, so this only ever sees the lines that genuinely differ
    /// between dialects. Unknown lines are ignored, as the protocols
    /// require.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the whitespace-split input line
    ///
    /// Return:
    /// bool                    -> true when the session should terminate
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool;
}

/// PROTOCOLS
///
/// Every dialect the engine speaks, in handshake-listing order. The active
/// one is chosen at runtime — by the handshake word a GUI sends or by the
/// `Protocol` option — not by a launch flag, so one running process serves
/// any GUI. The markers are zero-sized, so this is a table of `'static`
/// trait objects with no allocation.
pub const PROTOCOLS: [&dyn Protocol; 3] = [&Uci, &Usi, &Ucci];

/// find_protocol
///
/// Resolves a name to its dialect. A protocol's handshake word and its
/// `Protocol` option value are both its own `name()`, so the lookup derives
/// from the trait rather than a second table of tokens.
///
/// Params:
/// - name: &str                  -> a protocol name, e.g. "usi"
///
/// Return:
/// Option<&'static dyn Protocol> -> the dialect, or None if unknown
pub fn find_protocol(name: &str) -> Option<&'static dyn Protocol> {
    PROTOCOLS.into_iter().find(|protocol| protocol.name() == name)
}

/// list_variants
///
/// Discovers which variants can be served over one protocol: a variant
/// qualifies when an embedded dictionary and config both exist for it and
/// the dictionary's `protocols` section lists the protocol. The result
/// feeds the `<NAME>_Variant` combo option.
///
/// Params:
/// - protocol: &str    -> the protocol name to match in `protocols`
///
/// Return:
/// Vec<String>         -> sorted names of variants serving `protocol`
fn list_variants(protocol: &str) -> Vec<String> {
    let mut variants = Vec::new();

    for dict_file in EMBEDDED_DICTS.files() {
        let path = dict_file.path();

        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };

        if path.extension().and_then(|e| e.to_str()) != Some("dict")
        || stem == "example" {
            continue;
        }

        if EMBEDDED_CONFIGS
            .get_file(format!("{}.conf", stem))
            .is_none()
        {
            continue;
        }

        let Some(content) = dict_file.contents_utf8() else {
            continue;
        };

        let uncommented = COMMENT_PATTERN.replace_all(content, "");
        let cleaned = uncommented
            .lines()
            .map(str::trim)
            .filter(|l| !l.is_empty())
            .collect::<Vec<_>>()
            .join("\n");

        let section_titles = SECTION_PATTERN.captures_iter(&cleaned);
        let section_contents = SECTION_PATTERN
            .split(&cleaned)
            .filter(|c| !c.trim().is_empty());


        let mut sections: HashMap<String, Vec<String>> = HashMap::new();

        for (title, body) in section_titles.zip(section_contents) {
            let name = title[1].trim().to_string();
            let lines = body
                .lines()
                .map(str::to_string)
                .filter(|l| !l.trim().is_empty())
                .collect();
            sections.insert(name, lines);
        }

        let has_protocol = sections
            .get("protocols")
            .map(|lines| lines.iter().any(|l| l.trim() == protocol))
            .unwrap_or(false);

        if has_protocol {
            variants.push(stem.to_string());
        }
    }

    variants.sort();
    variants
}

/// print_bestmove
///
/// Emits the final `bestmove` line for a completed search, appending the
/// ponder move when one was found, and flushes stdout so the GUI sees it
/// immediately. Terminal positions always emit `(none)` with no ponder,
/// regardless of the SearchResult (which may be stale). A null best move
/// at a non-terminal position (search interrupted before depth one or
/// crashed) falls back to any legal move, or `(none)` if none exist.
/// The line is identical across every protocol the engine speaks.
///
/// Params:
/// - result: &SearchResult       -> finished search outcome
/// - state : &mut State          -> position for move formatting
/// - dict  : Option<&Translator> -> translator for printed move names
fn print_bestmove(
    result: &SearchResult,
    state: &mut State,
    dict: Option<&Translator>,
) {
    if state.game_over {
        emit(EngineEvent::BestMove {
            best: "(none)".to_string(),
            ponder: None,
        });
        return;
    }

    let fallback = result.best_move == null_move();

    let best_move = if fallback {
        legal_moves!(state).first().unwrap_or(&null_move()).clone()
    } else {
        result.best_move.clone()
    };

    let best = format_move(&best_move, state, dict);

    let ponder = if !fallback && result.ponder_move != null_move() {
        Some(format_move(&result.ponder_move, state, dict))
    } else {
        None
    };

    emit(EngineEvent::BestMove { best, ponder });
}

/// SearchHandle
///
/// A running (possibly pondering) search thread.
/// Keeps the join handle together with the launch parameters needed to
/// restart the search on `ponderhit`: whether it was a ponder search,
/// its depth and node limits, and the soft/hard time budgets (as
/// durations) to apply from the moment the hit arrives.
struct SearchHandle {
    handle: JoinHandle<SearchResult>,                                           /* the running search thread          */
    is_ponder: bool,                                                            /* true if launched as a ponder       */
    search_depth: usize,                                                        /* depth limit for restart            */
    search_nodes: u128,                                                         /* node limit for restart             */
    ponderhit_soft_ns: u128,                                                    /* soft budget on ponderhit           */
    ponderhit_hard_ns: u128,                                                    /* hard budget on ponderhit           */
}

/// SearchLimits
///
/// Launch parameters for one search thread.
/// Bundles the ponder flag, depth/node limits, and absolute deadlines
/// (ns since engine launch, 0 = none) for a spawned search, plus the
/// budget durations to re-apply when a ponder search is converted to a
/// timed one by `ponderhit`.
struct SearchLimits {
    is_ponder: bool,                                                            /* launch as a ponder search          */
    depth: usize,                                                               /* search depth limit                 */
    nodes: u128,                                                                /* search node limit                  */
    soft_deadline: u128,                                                        /* soft deadline, ns since launch     */
    hard_deadline: u128,                                                        /* hard deadline, ns since launch     */
    ponderhit_soft_ns: u128,                                                    /* soft budget for ponderhit          */
    ponderhit_hard_ns: u128,                                                    /* hard budget for ponderhit          */
}

/// Session
///
/// The protocol session state, shared by every dialect.
/// Owns the engine position, the discovered variant list and active
/// variant, the protocol name and its notation translator, the
/// thread/hash/overhead option values, the shared tables (rebuilt when the
/// Hash option changes), and the active search handle if a `go` is in
/// flight.
///
/// `position_valid` tracks whether the current `state` is the result of a
/// fully successful `position` replay (or `ucinewgame` / variant reload).
/// It is set true at startup and after every clean position load; set false
/// when `handle_position` fails mid-replay. A false value causes `go` to
/// emit an immediate `bestmove (none)` and return without spawning.
pub struct Session {
    protocol: String,                                                           /* the protocol being spoken          */
    state: State,                                                               /* the engine's working position      */
    variants: Vec<String>,                                                      /* discovered protocol-capable list   */
    variant: String,                                                            /* the active variant name            */
    translator: Option<Translator>,                                             /* protocol notation translator       */
    max_threads: usize,                                                         /* hardware thread ceiling            */
    threads: usize,                                                             /* configured worker threads          */
    hash_mb: usize,                                                             /* hash budget in megabytes           */
    overhead_ms: u128,                                                          /* move overhead in milliseconds      */
    ttable: Arc<TTable>,                                                        /* shared main transposition table    */
    qtable: Arc<QTable>,                                                        /* shared quiescence table            */
    ptable: Arc<PTable>,                                                        /* shared pawn structure table        */
    active: Option<SearchHandle>,                                               /* in-flight search, if any           */
    position_valid: bool,                                                       /* false after a failed position cmd  */
}

impl Session {
    /// Session::new
    ///
    /// Boots a session for one protocol on its default variant (standard
    /// when the protocol serves it, otherwise the first discovered one): loads
    /// its config, sets up the start position, finds its translator, and
    /// allocates the shared tables at the default hash budget (2/3 main,
    /// 1/3 qsearch).
    ///
    /// Params:
    /// - protocol: &str -> the protocol name to serve
    ///
    /// Return:
    /// Self             -> a session ready to accept commands
    fn new(protocol: &str) -> Self {
        let variants = list_variants(protocol);
        let default_variant = variants
            .iter()
            .find(|v| v.as_str() == "standard")
            .or_else(|| variants.first())
            .cloned()
            .unwrap_or_else(|| "standard".to_string());

        let config_path = format!("{}.conf", default_variant);
        let mut state = parse_config_file(&config_path);
        let startpos = state.statics.startpos.clone();
        state.reset();
        parse_fen(&mut state, &startpos, None)
            .unwrap_or_else(|error| panic!("{}", error));
        refresh_eval_state(&mut state);

        let translator = Translator::find(&default_variant, protocol);
        let max_threads = thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);

        let (ttable, qtable, ptable) = spawn_hash_tables(HASH_DEFAULT_MB);

        Session {
            protocol: protocol.to_string(),
            state,
            variants,
            variant: default_variant,
            translator,
            max_threads,
            threads: 1,
            hash_mb: HASH_DEFAULT_MB,
            overhead_ms: TIME_OVERHEAD_MS,
            ttable,
            qtable,
            ptable,
            active: None,
            position_valid: true,
        }
    }

    /// Session::variant_option
    ///
    /// The name of this protocol's variant combo option, derived from the
    /// protocol name so the handshake listing and `setoption` never drift
    /// apart: `uci` yields `UCI_Variant`, `usi` yields `USI_Variant`.
    ///
    /// Return:
    /// String -> the `<NAME>_Variant` option name
    fn variant_option(&self) -> String {
        format!("{}_Variant", self.protocol.to_uppercase())
    }

    /// Session::set_protocol
    ///
    /// Switches the session to another dialect at runtime, the way the
    /// handshake word or the `Protocol` option asks. Re-discovers the
    /// variants that dialect serves and its notation translator; when the
    /// current variant is not among them it reseats onto the dialect's
    /// default variant (standard when served, otherwise the first) exactly
    /// as startup does, reloading the position and pawn table so nothing from
    /// the old variant leaks through. Any active search is stopped first.
    ///
    /// Params:
    /// - protocol: &str -> the dialect name to switch to
    fn set_protocol(&mut self, protocol: &str) {
        abort_search(self);
        self.protocol = protocol.to_string();
        self.variants = list_variants(protocol);

        if !self.variants.contains(&self.variant) {
            self.variant = self.variants
                .iter()
                .find(|variant| variant.as_str() == "standard")
                .or_else(|| self.variants.first())
                .cloned()
                .unwrap_or_else(|| "standard".to_string());

            let config_path = format!("{}.conf", self.variant);
            self.state = parse_config_file(&config_path);
            let startpos = self.state.statics.startpos.clone();
            self.state.reset();
            parse_fen(&mut self.state, &startpos, None)
                .unwrap_or_else(|error| panic!("{}", error));
            refresh_eval_state(&mut self.state);
            self.ptable = Arc::new(PTable::default());
            self.position_valid = true;
        }

        self.translator = Translator::find(&self.variant, protocol);
    }
}

/// spawn_hash_tables
///
/// Allocates the three shared hash tables from one megabyte budget, split
/// between them by the fixed `HASH_*_PARTS` ratio so a `Hash` change scales
/// all three consistently. Used at session startup and whenever `Hash` or
/// `Clear Hash` rebuilds the tables.
///
/// Params:
/// - hash_mb: usize                        -> total table budget in megabytes
///
/// Return:
/// (Arc<TTable>, Arc<QTable>, Arc<PTable>) -> the freshly sized tables
fn spawn_hash_tables(
    hash_mb: usize,
) -> (Arc<TTable>, Arc<QTable>, Arc<PTable>) {
    (
        Arc::new(TTable::with_mb(
            (hash_mb * HASH_T_PARTS / HASH_PARTS).max(1)
        )),
        Arc::new(QTable::with_mb(
            (hash_mb * HASH_Q_PARTS / HASH_PARTS).max(1)
        )),
        Arc::new(PTable::with_mb(
            (hash_mb * HASH_P_PARTS / HASH_PARTS).max(1)
        )),
    )
}

/// replay_moves
///
/// Replays move tokens onto `state`, stopping at first parse or legality
/// failure.
///
/// Params:
/// - state : &mut State          -> position replayed into
/// - tokens: &[&str]             -> move tokens without `moves`
/// - dict  : Option<&Translator> -> translator for notation lookup
///
/// Return:
/// Result<(), String>            -> success or first failing ply diagnostic
fn replay_moves(
    state: &mut State,
    tokens: &[&str],
    dict: Option<&Translator>,
) -> Result<(), String> {
    for (ply, &move_str) in tokens.iter().enumerate() {
        let Some(mv) = parse_move(move_str, state, dict) else {
            return Err(format!(
                "ply {} \"{}\" parse failed", ply + 1, move_str
            ));
        };

        if !make_move!(state, mv) {
            return Err(format!(
                "ply {} \"{}\" illegal", ply + 1, move_str
            ));
        }
    }

    Ok(())
}

/// Session command handlers
///
/// Each applies one command's side effects to the session.
/// `compute_budgets` and `spawn_search` interleave below as internal
/// helpers and keep their own docs.
///
/// handle_position
///
///   Params:
///
///   - session: &mut Session
///     session rebuilt from `startpos`/FEN
///
///   - tokens: &[&str]
///     whitespace-split command line; the FEN keyword may be `fen` or `sfen`,
///     and a trailing `moves` list is replayed onto the position
///
/// start_search
///
///   Params:
///
///   - session: &mut Session
///     session the `go` runs in; deadlines anchored, search thread spawned
///
///   - tokens: &[&str]
///     whitespace-split `go` limits (depth, nodes, movetime, clocks, ponder,
///     infinite), already normalized to standard tokens by the caller
///
/// abort_search
///
///   Params:
///
///   - session: &mut Session
///     session whose active search is interrupted and joined; a worker panic is
///     absorbed
///
///   Return:
///
///   Option<(SearchResult, bool)>
///   the joined result and its ponder flag, None when no search ran
///
/// stop_search
///
///   Params:
///
///   - session: &mut Session
///     session whose search is aborted; a ponder search prints its withheld
///     `bestmove`
///
/// handle_ponderhit
///
///   Params:
///
///   - session: &mut Session
///     session whose ponder search is joined and relaunched as a timed search
///
/// handle_setoption
///
///   Params:
///
///   - session: &mut Session
///     session receiving the option change
///
///   - tokens: &[&str]
///     whitespace-split `setoption` line; applies Variant / Threads / Hash /
///     Overhead changes
fn handle_position(session: &mut Session, tokens: &[&str]) {
    abort_search(session);

    let mut scratch = session.state.fork();
    let dict = session.translator.clone();
    let mut index = 1;

    if tokens.get(index).copied() == Some("startpos") {
        index += 1;
    } else if matches!(
        tokens.get(index).copied(),
        Some(keyword) if keyword != "moves"
    ) {
        index += 1;
        let fen_end = tokens[index..]
            .iter()
            .position(|&token| token == "moves")
            .map(|position| position + index)
            .unwrap_or(tokens.len());
        let fen = tokens[index..fen_end].join(" ");

        if fen.is_empty() {
            log_2!("position: empty FEN");
            session.position_valid = false;
            return;
        }

        scratch.reset();
        if let Err(error) = parse_fen(
            &mut scratch, &fen, dict.as_ref()
        ) {
            log_2!("position: invalid FEN: {}", error);
            session.position_valid = false;
            return;
        }
        refresh_eval_state(&mut scratch);
        index = fen_end;
    } else {
        log_2!("position: missing startpos or FEN");
        session.position_valid = false;
        return;
    }

    if index < tokens.len() {
        if tokens[index] != "moves" {
            log_2!("position: unexpected token \"{}\"", tokens[index]);
            session.position_valid = false;
            return;
        }

        if let Err(error) = replay_moves(
            &mut scratch,
            &tokens[index + 1..],
            dict.as_ref(),
        ) {
            log_2!("position: {}", error);
            session.position_valid = false;
            return;
        }
    }

    session.state = scratch;
    session.position_valid = true;
}

/// start_search
///
/// Parses a standard `go` line and launches the search thread. Dialect
/// clauses (USI `byoyomi`, UCCI `time`/`increment`) are normalized to the
/// standard depth/nodes/movetime/clock tokens by the protocol before they
/// reach here, so the shared parse -- and the engine core -- never sees a
/// dialect token.
///
/// Params:
/// - session: &mut Session -> session the `go` runs in
/// - tokens : &[&str]      -> normalized `go` limit tokens
pub fn start_search(session: &mut Session, tokens: &[&str]) {
    abort_search(session);

    if !session.position_valid {
        log_2!("go: position invalid, refusing search");
        emit(EngineEvent::BestMove {
            best: "(none)".to_string(),
            ponder: None,
        });
        return;
    }

    let go_time = ENGINE_START.elapsed().as_nanos();
    let is_ponder = tokens.contains(&"ponder");

    let mut depth = 0usize;
    let mut nodes = 0u128;
    let mut movetime_ms = 0u128;
    let mut wtime_ms = 0u128;
    let mut btime_ms = 0u128;
    let mut winc_ms = 0u128;
    let mut binc_ms = 0u128;
    let mut movestogo = 0usize;
    let mut infinite = false;

    let clamped = |token: Option<&&str>| -> u128 {
        token
            .and_then(|s| s.parse::<i64>().ok())
            .map(|v| v.max(0) as u128)
            .unwrap_or(0)
    };

    let mut index = 1;
    while index < tokens.len() {
        match tokens[index] {
            "depth" => {
                depth = clamped(tokens.get(index + 1)) as usize;
                index += 2;
            }
            "nodes" => {
                nodes = clamped(tokens.get(index + 1));
                index += 2;
            }
            "movetime" => {
                movetime_ms = clamped(tokens.get(index + 1));
                index += 2;
            }
            "wtime" => {
                wtime_ms = clamped(tokens.get(index + 1)).max(1);               /* present clock is never 0: a spent  */
                index += 2;                                                     /* or negative clock means move now,  */
            }                                                                   /* not search without any time limit  */
            "btime" => {
                btime_ms = clamped(tokens.get(index + 1)).max(1);
                index += 2;
            }
            "winc" => {
                winc_ms = clamped(tokens.get(index + 1));
                index += 2;
            }
            "binc" => {
                binc_ms = clamped(tokens.get(index + 1));
                index += 2;
            }
            "movestogo" => {
                movestogo = clamped(tokens.get(index + 1)) as usize;
                index += 2;
            }
            "infinite" => {
                infinite = true;
                index += 1;
            }
            _ => { index += 1; }
        }
    }

    let (time_ms, inc_ms) = if session.state.playing == WHITE {
        (wtime_ms, winc_ms)
    } else {
        (btime_ms, binc_ms)
    };

    let (soft_ns, hard_ns) = compute_budgets(
        movetime_ms, time_ms, inc_ms, movestogo, session.overhead_ms,
    );

    let (soft_deadline, hard_deadline) = if infinite || soft_ns == 0 {
        (0, 0)
    } else {
        (go_time + soft_ns, go_time + hard_ns)
    };

    let search_depth = if depth > 0 { depth } else { MAX_DEPTH };

    spawn_search(session, SearchLimits {
        is_ponder,
        depth: search_depth,
        nodes,
        soft_deadline,
        hard_deadline,
        ponderhit_soft_ns: soft_ns,
        ponderhit_hard_ns: hard_ns,
    });
}

/// compute_budgets
///
/// Derives the soft and hard time budgets for one `go` command as
/// durations in nanoseconds. `movetime` spends its whole allotment on
/// both budgets. Clock time allocates `time/divisor + inc*2/3` per
/// move: no new depth starts past half of that (soft), while a started
/// depth may run to twice it (hard), so the average spend stays near
/// the allocation but iterations finish instead of being cut. Both are
/// capped by the remaining clock minus the move overhead and floored
/// at MIN_TIME_BUDGET_NS, so a timed search never receives the untimed
/// sentinel of zero.
///
/// Params:
/// - movetime_ms: u128  -> fixed time per move (0 = unset)
/// - time_ms    : u128  -> remaining clock for the side to move
/// - inc_ms     : u128  -> increment per move
/// - movestogo  : usize -> moves to the next time control (0 = unset)
/// - overhead_ms: u128  -> per-move lag allowance
///
/// Return:
/// (u128, u128)         -> (soft, hard) budgets in ns, (0, 0) when untimed
fn compute_budgets(
    movetime_ms: u128,
    time_ms: u128,
    inc_ms: u128,
    movestogo: usize,
    overhead_ms: u128,
) -> (u128, u128) {
    if movetime_ms > 0 {
        let budget = (movetime_ms.saturating_sub(overhead_ms) * 1_000_000)
            .max(MIN_TIME_BUDGET_NS);
        return (budget, budget);
    }

    if time_ms == 0 {
        return (0, 0);
    }

    let divisor = if movestogo > 0 { movestogo as u128 } else { 40 };
    let raw = (time_ms / divisor + inc_ms * 2 / 3) * 1_000_000;
    let cap = (time_ms.saturating_sub(overhead_ms) * 1_000_000)
        .max(MIN_TIME_BUDGET_NS);

    let soft = (raw / 2).clamp(MIN_TIME_BUDGET_NS, cap);
    let hard = (soft * HARD_BUDGET_FACTOR).clamp(MIN_TIME_BUDGET_NS, cap);

    (soft, hard)
}

/// spawn_search
///
/// Launches the search thread for the given limits and records it as the
/// session's active handle, wiring in the shared tables and the stop and
/// interrupt plumbing.
///
/// Params:
/// - session: &mut Session -> the session being mutated
/// - limits : SearchLimits -> launch parameters for the thread
fn spawn_search(session: &mut Session, limits: SearchLimits) {
    let state_clone = session.state.clone();
    let tt_clone = Arc::clone(&session.ttable);
    let qt_clone = Arc::clone(&session.qtable);
    let pt_clone = Arc::clone(&session.ptable);
    let dict_clone = session.translator.clone();
    let tc = session.threads;
    let is_ponder = limits.is_ponder;

    let thread_depth = if is_ponder { MAX_DEPTH } else { limits.depth };
    let soft_deadline = if is_ponder { 0 } else { limits.soft_deadline };
    let hard_deadline = if is_ponder { 0 } else { limits.hard_deadline };
    let set_nodes = if is_ponder { 0 } else { limits.nodes };

    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let handle = thread::Builder::new()
        .name(format!("search:{}", exe_tag()))
        .stack_size(64 * 1024 * 1024)
        .spawn(move || {
            let mut s = state_clone;
            let mut info = SearchInfo {
                set_depth: thread_depth,
                set_nodes,
                soft_deadline,
                hard_deadline,
                ..Default::default()
            };
            let mut bufs = SearchBufs::default();

            let table = Arc::clone(&tt_clone);
            let qtable = Arc::clone(&qt_clone);
            let ptable = Arc::clone(&pt_clone);
            let result = catch_unwind(AssertUnwindSafe(|| {
                search_position(
                    &mut s, table, qtable, ptable,
                    &mut info, &mut bufs, tc,
                    dict_clone.as_ref(),
                )
            }))
            .unwrap_or_else(|_| SearchResult {
                best_score: 0,
                best_move: null_move(),
                ponder_move: null_move(),
                total_nodes: 0,
                total_elapsed: 0,
            });

            if !is_ponder {
                print_bestmove(&result, &mut s, dict_clone.as_ref());
            }

            log_table_stats(&tt_clone, &qt_clone, &pt_clone);

            result
        }).expect("failed to spawn search thread");

    session.active = Some(SearchHandle {
        handle,
        is_ponder,
        search_depth: limits.depth,
        search_nodes: limits.nodes,
        ponderhit_soft_ns: limits.ponderhit_soft_ns,
        ponderhit_hard_ns: limits.ponderhit_hard_ns,
    });
}

fn abort_search(session: &mut Session) -> Option<(SearchResult, bool)> {
    let sh = session.active.take()?;

    SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
    let joined = sh.handle.join();
    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    match joined {
        Ok(result) => Some((result, sh.is_ponder)),
        Err(_) => {
            log_1!("search thread panicked during join");
            None
        }
    }
}

fn stop_search(session: &mut Session) {
    if let Some((result, was_ponder)) = abort_search(session)
        && was_ponder
    {
        print_bestmove(&result, &mut session.state, session.translator.as_ref());
    }
}

fn handle_ponderhit(session: &mut Session) {
    if !session.active.as_ref().is_some_and(|sh| sh.is_ponder) {
        return;
    }

    let hit_time = ENGINE_START.elapsed().as_nanos();

    let Some(sh) = session.active.take() else {
        return;
    };

    SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
    let _ = sh.handle.join();
    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let (soft_deadline, hard_deadline) = if sh.ponderhit_soft_ns == 0 {
        (0, 0)
    } else {
        (
            hit_time + sh.ponderhit_soft_ns,
            hit_time + sh.ponderhit_hard_ns,
        )
    };

    spawn_search(session, SearchLimits {
        is_ponder: false,
        depth: sh.search_depth,
        nodes: sh.search_nodes,
        soft_deadline,
        hard_deadline,
        ponderhit_soft_ns: 0,
        ponderhit_hard_ns: 0,
    });
}

fn handle_setoption(session: &mut Session, tokens: &[&str]) {
    let Some(name_pos) = tokens.iter().position(|&t| t == "name") else {
        return;
    };
    let value_pos = tokens.iter().position(|&t| t == "value");

    let name = tokens[(name_pos + 1)..value_pos.unwrap_or(tokens.len())]
        .join(" ");
    let value = value_pos.map(|b| tokens[(b + 1)..].join(" "));

    let variant = session.variant_option();

    match (name.as_str(), value) {
        (OPT_PROTOCOL, Some(v)) if find_protocol(&v).is_some() => {
            session.set_protocol(&v);
        }
        (n, Some(v)) if n == variant && session.variants.contains(&v) => {
            abort_search(session);
            let conf = format!("{}.conf", v);
            session.state = parse_config_file(&conf);

            let position = session.state.statics.startpos.clone();

            session.state.reset();
            parse_fen(&mut session.state, &position, None)
                .unwrap_or_else(|error| panic!("{}", error));

            refresh_eval_state(&mut session.state);

            session.ptable = Arc::new(PTable::default());

            session.translator = Translator::find(&v, &session.protocol);
            session.variant = v;
            session.position_valid = true;
        }
        (OPT_THREADS, Some(v)) => {
            if let Ok(n) = v.parse::<usize>() {
                session.threads = n.clamp(1, session.max_threads);
            }
        }
        (OPT_HASH, Some(v)) => {
            if let Ok(mb) = v.parse::<usize>() {
                session.hash_mb = mb.clamp(1, HASH_MAX_MB);
                (session.ttable, session.qtable, session.ptable) =
                    spawn_hash_tables(session.hash_mb);
            }
        }
        (OPT_CLEAR_HASH, None) => {
            (session.ttable, session.qtable, session.ptable) =
                spawn_hash_tables(session.hash_mb);
        }
        (OPT_MOVE_OVERHEAD, Some(v)) => {
            if let Ok(ms) = v.parse::<u128>() {
                session.overhead_ms = ms.min(MAX_OVERHEAD_MS);
            }
        }
        _ => {}
    }
}

/// print_handshake
///
/// Emits the engine identity, the shared option list, and the protocol's
/// `...ok` terminator in response to the handshake command. The variant
/// combo, the `...ok` word, and (via the caller) the handshake command all
/// derive from the protocol name, so a new dialect needs no new tokens
/// here. Called from each protocol's `execute` when it recognizes its own
/// handshake word.
///
/// Params:
/// - session: &Session -> the session whose variants are listed
pub fn print_handshake(session: &Session) {
    let vars_str: String = session.variants
        .iter()
        .map(|v| format!(" var {}", v))
        .collect();

    let protocols_str: String = PROTOCOLS
        .iter()
        .map(|protocol| format!(" var {}", protocol.name()))
        .collect();

    emit(EngineEvent::Print(format!(
        "id name anekamacam\n\
         id author Alden Luthfi\n\
         option name {} type combo default {}{}\n\
         option name {} type combo default {}{}\n\
         option name {} type spin default 1 min 1 max {}\n\
         option name {} type check default false\n\
         option name {} type spin default {} min 1 max {}\n\
         option name {} type button\n\
         option name {} type spin default {} min 0 max {}\n\
         {}ok\n",
        OPT_PROTOCOL, session.protocol, protocols_str,
        session.variant_option(), session.variant, vars_str,
        OPT_THREADS, session.max_threads,
        OPT_PONDER,
        OPT_HASH, HASH_DEFAULT_MB, HASH_MAX_MB,
        OPT_CLEAR_HASH,
        OPT_MOVE_OVERHEAD, TIME_OVERHEAD_MS, MAX_OVERHEAD_MS,
        session.protocol,
    )));
}

/// new_game
///
/// Resets the session to the active variant's start position, aborting any
/// search in flight. Shared by every protocol's new-game command
/// (`ucinewgame`, `usinewgame`), which differ only in spelling.
///
/// Params:
/// - session: &mut Session -> the session to reset
pub fn new_game(session: &mut Session) {
    abort_search(session);
    let start = session.state.statics.startpos.clone();
    session.state.reset();
    parse_fen(&mut session.state, &start, None)
        .unwrap_or_else(|error| panic!("{}", error));
    refresh_eval_state(&mut session.state);
    session.position_valid = true;
}

/// execute_common
///
/// Serves the commands that behave identically in every protocol, with no
/// dialect token anywhere: readiness, position setup (the FEN keyword is
/// skipped whatever it is), stop, ponderhit, option changes, the debug `d`
/// command, and quit. It runs before the protocol's own `execute`, so a
/// line it does not recognize returns `None` and falls through to the
/// dialect handler (handshake, new-game, `go`).
///
/// Params:
/// - session: &mut Session -> the session being driven
/// - tokens : &[&str]      -> one trimmed, split line of input
///
/// Return:
/// Option<bool>            -> Some(should_quit) when served, None to defer
pub fn execute_common(
    session: &mut Session,
    tokens: &[&str],
) -> Option<bool> {
    match tokens.first().copied().unwrap_or("") {
        "isready" => {
            emit(EngineEvent::Print("readyok\n".to_string()));
            Some(false)
        }
        "position" => {
            handle_position(session, tokens);
            Some(false)
        }
        "stop" => {
            stop_search(session);
            Some(false)
        }
        "ponderhit" => {
            handle_ponderhit(session);
            Some(false)
        }
        "setoption" => {
            handle_setoption(session, tokens);
            Some(false)
        }
        "d" => {
            emit(EngineEvent::Print(format!(
                "{}\n", format_game_state(&session.state),
            )));
            verify_game_state(&session.state);
            Some(false)
        }
        "quit" => {
            abort_search(session);
            Some(true)
        }
        _ => None,
    }
}

/// run
///
/// The blocking protocol main loop, one process for every dialect. Starts on
/// the default protocol and, for each stdin line: a handshake word (any
/// protocol's `name()`) switches the active dialect and greets; otherwise
/// the line goes first to the universal `execute_common` and then, when it
/// defers, to the current dialect's `execute`, until `quit` or end of input.
/// The `Protocol` option switches the dialect the same way from `setoption`.
/// Any search still running on exit is stopped and joined so the process
/// never dies mid-search.
///
/// Return:
/// IoResult<()> -> Ok on clean shutdown
pub fn run() -> IoResult<()> {
    let (sender, receiver) = channel::<EngineEvent>();
    set_sink(sender);
    let printer = spawn_printer(receiver);

    emit(EngineEvent::Print(format!(
        "AnekaMacam {} by Alden Luthfi\n", env!("CARGO_PKG_VERSION"),
    )));

    let mut session = Session::new(DEFAULT_PROTOCOL);

    for line in stdin().lock().lines().map_while(Result::ok) {
        let trimmed = line.trim();
        let tokens: Vec<&str> = trimmed.split_whitespace().collect();

        log_3!("{} Command: {}", session.protocol, trimmed);

        if let Some(protocol) =
            find_protocol(tokens.first().copied().unwrap_or(""))
        {
            session.set_protocol(protocol.name());
            print_handshake(&session);
            continue;
        }

        let protocol = find_protocol(&session.protocol).unwrap_or(&Uci);
        let quit = execute_common(&mut session, &tokens)
            .unwrap_or_else(|| protocol.execute(&mut session, &tokens));

        if quit {
            break;
        }
    }

    abort_search(&mut session);

    clear_sink();
    let _ = printer.join();

    Ok(())
}
