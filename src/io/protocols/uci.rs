//! # uci.rs
//!
//! Implements the Universal Chess Interface (UCI) protocol loop.
//!
//! Handles position setup, search commands with time controls, option
//! configuration, and variant selection. Variants are discovered at runtime
//! from embedded resources; search runs on a configurable thread pool backed
//! by a shared transposition table.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 24/05/2026
use crate::*;

/// list_uci_variants
///
/// Discovers which variants can be served over UCI: a variant qualifies
/// when an embedded dictionary and config both exist for it and the
/// dictionary's `protocols` section lists `uci`. The result feeds the
/// UCI_Variant combo option.
///
/// Return:
/// Vec<String> -> sorted names of UCI-capable variants
///
fn list_uci_variants() -> Vec<String> {
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

        let has_uci = sections
            .get("protocols")
            .map(|lines| lines.iter().any(|l| l.trim() == "uci"))
            .unwrap_or(false);

        if has_uci {
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
/// immediately. A null best move (search interrupted before depth one or
/// crashed) falls back to any legal move, or `(none)` in a terminal
/// position, so the GUI never receives the unplayable "null" string.
///
/// Params:
/// - result: &SearchResult       -> finished search outcome
/// - state : &State              -> position for move formatting
/// - dict  : Option<&Translator> -> translator for printed move names
///
fn print_bestmove(
    result: &SearchResult,
    state: &mut State,
    dict: Option<&Translator>,
) {
    let fallback = result.best_move == null_move();

    let best_move = if fallback {
        legal_moves!(state).first().unwrap_or(&null_move()).clone()
    } else {
        result.best_move.clone()
    };

    let best = format_move(&best_move, state, dict);

    if !fallback && result.ponder_move != null_move() {
        let ponder = format_move(&result.ponder_move, state, dict);
        println!("bestmove {} ponder {}", best, ponder);
    } else {
        println!("bestmove {}", best);
    }
    stdout().flush().ok();
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

/// Uci
///
/// The UCI session state.
/// Owns the engine position, the discovered variant list and active
/// variant, the protocol translator, thread/hash/overhead option values,
/// the shared tables (rebuilt when the Hash option changes), and the
/// active search handle if a `go` is in flight.
struct Uci {
    state: State,                                                               /* the engine's working position      */
    variants: Vec<String>,                                                      /* discovered UCI-capable variants    */
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
}

impl Uci {
    /// Uci::new
    ///
    /// Boots a session on the default variant (fide when available,
    /// otherwise the first discovered one): loads its config, sets up
    /// the start position, finds its translator, and allocates the
    /// shared tables at the default hash budget (2/3 main, 1/3 qsearch).
    ///
    /// Return:
    /// Self -> a session ready to accept UCI commands
    ///
    fn new() -> Self {
        let variants = list_uci_variants();
        let default_variant = variants
            .iter()
            .find(|v| v.as_str() == "fide")
            .or_else(|| variants.first())
            .cloned()
            .unwrap_or_else(|| "fide".to_string());

        let config_path = format!("{}.conf", default_variant);
        let mut state = parse_config_file(&config_path);
        let startpos = state.statics.startpos.clone();
        state.reset();
        parse_fen(&mut state, &startpos, None);
        refresh_eval_state(&mut state);

        let translator = Translator::find(&default_variant, "uci");
        let max_threads = thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);

        let (ttable, qtable, ptable) = spawn_hash_tables(HASH_DEFAULT_MB);

        Uci {
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
        }
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
/// - hash_mb: usize -> total table budget in megabytes
///
/// Return:
/// (Arc<TTable>, Arc<QTable>, Arc<PTable>) -> the freshly sized tables
///
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

/// UCI command handlers
///
/// Each applies one command's side effects to the session. Every handler
/// takes `uci: &mut Uci`; the parsers additionally take `tokens: &[&str]`,
/// the whitespace-split command line. Highest level first:
///
/// - `handle_position`  -> rebuild from `startpos`/FEN, replay `moves`
/// - `start_search`     -> parse `go` limits, anchor deadlines, spawn thread
/// - `abort_search`     -> interrupt + join, drop the result; panic is safe
/// - `stop_search`      -> abort; a ponder prints its withheld `bestmove`
/// - `handle_ponderhit` -> join the ponder, relaunch it as a timed search
/// - `handle_setoption` -> apply Variant / Threads / Hash / Overhead changes
///
/// `compute_budgets` and `spawn_search` interleave below as internal
/// helpers and keep their own docs.
///
fn handle_position(uci: &mut Uci, tokens: &[&str]) {
    let startpos = uci.state.statics.startpos.clone();
    uci.state.reset();

    let mut index = 1;

    if tokens.get(index).copied() == Some("startpos") {
        parse_fen(&mut uci.state, &startpos, None);
        index += 1;
    } else if tokens.get(index).copied() == Some("fen") {
        index += 1;
        let fen_end = tokens[index..]
            .iter()
            .position(|&t| t == "moves")
            .map(|p| p + index)
            .unwrap_or(tokens.len());
        let fen_str = tokens[index..fen_end].join(" ");
        let dict = uci.translator.as_ref();
        parse_fen(&mut uci.state, &fen_str, dict);
        index = fen_end;
    }

    refresh_eval_state(&mut uci.state);

    if tokens.get(index).copied() == Some("moves") {
        let dict = uci.translator.as_ref();
        let state = &mut uci.state;
        for move_str in &tokens[index + 1..] {
            if let Some(mv) = parse_move(move_str, state, dict) {
                make_move!(state, mv);
            }
        }
    }
}

fn start_search(uci: &mut Uci, tokens: &[&str]) {
    abort_search(uci);

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

    let (time_ms, inc_ms) = if uci.state.playing == WHITE {
        (wtime_ms, winc_ms)
    } else {
        (btime_ms, binc_ms)
    };

    let (soft_ns, hard_ns) = compute_budgets(
        movetime_ms, time_ms, inc_ms, movestogo, uci.overhead_ms,
    );

    let (soft_deadline, hard_deadline) = if infinite || soft_ns == 0 {
        (0, 0)
    } else {
        (go_time + soft_ns, go_time + hard_ns)
    };

    let search_depth = if depth > 0 { depth } else { MAX_DEPTH };

    spawn_search(uci, SearchLimits {
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
///
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
/// - uci   : &mut Uci     -> the session being mutated
/// - limits: SearchLimits -> launch parameters for the thread
///
fn spawn_search(uci: &mut Uci, limits: SearchLimits) {
    let state_clone = uci.state.clone();
    let tt_clone = Arc::clone(&uci.ttable);
    let qt_clone = Arc::clone(&uci.qtable);
    let pt_clone = Arc::clone(&uci.ptable);
    let dict_clone = uci.translator.clone();
    let tc = uci.threads;
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

    uci.active = Some(SearchHandle {
        handle,
        is_ponder,
        search_depth: limits.depth,
        search_nodes: limits.nodes,
        ponderhit_soft_ns: limits.ponderhit_soft_ns,
        ponderhit_hard_ns: limits.ponderhit_hard_ns,
    });
}

fn abort_search(uci: &mut Uci) -> Option<(SearchResult, bool)> {
    let sh = uci.active.take()?;

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

fn stop_search(uci: &mut Uci) {
    if let Some((result, was_ponder)) = abort_search(uci)
        && was_ponder
    {
        print_bestmove(&result, &mut uci.state, uci.translator.as_ref());
    }
}

fn handle_ponderhit(uci: &mut Uci) {
    if !uci.active.as_ref().is_some_and(|sh| sh.is_ponder) {
        return;
    }

    let hit_time = ENGINE_START.elapsed().as_nanos();

    let Some(sh) = uci.active.take() else {
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

    spawn_search(uci, SearchLimits {
        is_ponder: false,
        depth: sh.search_depth,
        nodes: sh.search_nodes,
        soft_deadline,
        hard_deadline,
        ponderhit_soft_ns: 0,
        ponderhit_hard_ns: 0,
    });
}

fn handle_setoption(uci: &mut Uci, tokens: &[&str]) {
    let Some(name_pos) = tokens.iter().position(|&t| t == "name") else {
        return;
    };
    let value_pos = tokens.iter().position(|&t| t == "value");

    let name = tokens[(name_pos + 1)..value_pos.unwrap_or(tokens.len())]
        .join(" ");
    let value = value_pos.map(|b| tokens[(b + 1)..].join(" "));

    match (name.as_str(), value) {
        (OPT_VARIANT, Some(v)) if uci.variants.contains(&v) => {
            let conf = format!("{}.conf", v);
            uci.state = parse_config_file(&conf);

            let position = uci.state.statics.startpos.clone();

            uci.state.reset();
            parse_fen(&mut uci.state, &position, None);

            refresh_eval_state(&mut uci.state);

            uci.ptable = Arc::new(PTable::default());

            uci.translator = Translator::find(&v, "uci");
            uci.variant = v;
        }
        (OPT_THREADS, Some(v)) => {
            if let Ok(n) = v.parse::<usize>() {
                uci.threads = n.clamp(1, uci.max_threads);
            }
        }
        (OPT_HASH, Some(v)) => {
            if let Ok(mb) = v.parse::<usize>() {
                uci.hash_mb = mb.clamp(1, HASH_MAX_MB);
                (uci.ttable, uci.qtable, uci.ptable) =
                    spawn_hash_tables(uci.hash_mb);
            }
        }
        (OPT_CLEAR_HASH, None) => {
            (uci.ttable, uci.qtable, uci.ptable) =
                spawn_hash_tables(uci.hash_mb);
        }
        (OPT_MOVE_OVERHEAD, Some(v)) => {
            if let Ok(ms) = v.parse::<u128>() {
                uci.overhead_ms = ms.min(MAX_OVERHEAD_MS);
            }
        }
        _ => {}
    }
}

/// execute_command
///
/// Dispatches one line of UCI input: identification and option listing
/// for `uci`, readiness, new-game reset, and delegation to the handlers
/// above for position/go/stop/ponderhit/setoption. The nonstandard `d`
/// command prints and verifies the current position for debugging.
/// Unknown commands are ignored, as the protocol requires.
///
/// Params:
/// - uci : &mut Uci -> the session being driven
/// - line: &str     -> one trimmed line of input
///
/// Return:
/// bool             -> true when the session should terminate (`quit`)
///
fn execute_command(uci: &mut Uci, line: &str) -> bool {
    let tokens: Vec<&str> = line.split_whitespace().collect();
    let command = tokens.first().copied().unwrap_or("");

    log_3!("UCI Command: {}", line);

    match command {
        "uci" => {
            println!("id name anekamacam");
            println!("id author Alden Luthfi");
            let vars_str: String = uci.variants
                .iter()
                .map(|v| format!(" var {}", v))
                .collect();
            println!(
                "option name {} type combo default {}{}",
                OPT_VARIANT, uci.variant, vars_str,
            );
            println!(
                "option name {} type spin default 1 min 1 max {}",
                OPT_THREADS, uci.max_threads,
            );
            println!(
                "option name {} type check default false",
                OPT_PONDER,
            );
            println!(
                "option name {} type spin default {} min 1 max {}",
                OPT_HASH, HASH_DEFAULT_MB, HASH_MAX_MB,
            );
            println!("option name {} type button", OPT_CLEAR_HASH);
            println!(
                "option name {} type spin default {} min 0 max {}",
                OPT_MOVE_OVERHEAD, TIME_OVERHEAD_MS, MAX_OVERHEAD_MS,
            );
            println!("uciok");
            stdout().flush().ok();
        }
        "isready" => {
            println!("readyok");
            stdout().flush().ok();
        }
        "ucinewgame" => {
            abort_search(uci);
            let sp = uci.state.statics.startpos.clone();
            uci.state.reset();
            parse_fen(&mut uci.state, &sp, None);
            refresh_eval_state(&mut uci.state);
        }
        "position" => {
            handle_position(uci, &tokens);
        }
        "go" => {
            start_search(uci, &tokens);
        }
        "stop" => {
            stop_search(uci);
        }
        "quit" => {
            abort_search(uci);
            return true;
        }
        "ponderhit" => {
            handle_ponderhit(uci);
        }
        "setoption" => {
            handle_setoption(uci, &tokens);
        }
        "d" => {
            println!("{}", format_game_state(&uci.state));
            verify_game_state(&uci.state);
        }
        _ => {}
    }

    false
}

/// uci
///
/// The blocking UCI main loop: prints the engine banner, builds the
/// session, and feeds stdin lines to `execute_command` until `quit` or
/// end of input. Any search still running on exit is stopped and joined
/// so the process never dies mid-search.
///
/// Return:
/// IoResult<()> -> Ok on clean shutdown
///
pub fn uci() -> IoResult<()> {
    println!("AnekaMacam {} by Alden Luthfi", env!("CARGO_PKG_VERSION"));

    let mut uci = Uci::new();

    for line in stdin().lock().lines().map_while(Result::ok) {
        if execute_command(&mut uci, line.trim()) {
            break;
        }
    }

    abort_search(&mut uci);

    Ok(())
}
