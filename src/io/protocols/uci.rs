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
/// immediately.
///
/// Params:
/// - result: &SearchResult     -> finished search outcome
/// - state: &State             -> position for move formatting
/// - dict: Option<&Translator> -> translator for printed move names
///
fn print_bestmove(
    result: &SearchResult,
    state: &State,
    dict: Option<&Translator>,
) {
    let best = format_move(&result.best_move, state, dict);
    if result.ponder_move != null_move() {
        let ponder = format_move(&result.ponder_move, state, dict);
        println!("bestmove {} ponder {}", best, ponder);
    } else {
        println!("bestmove {}", best);
    }
    stdout().flush().ok();
}

/// A running (possibly pondering) search thread.
///
/// Keeps the join handle together with the launch parameters needed to
/// restart the search on `ponderhit`: whether it was a ponder search,
/// its depth limit, and the time budget to apply once the hit arrives.
struct SearchHandle {
    handle: JoinHandle<SearchResult>,
    is_ponder: bool,
    search_depth: usize,
    ponderhit_timed_ns: u128,
}

/// The UCI session state.
///
/// Owns the engine position, the discovered variant list and active
/// variant, the protocol translator, thread/hash option values, the
/// shared tables (rebuilt when the Hash option changes), and the active
/// search handle if a `go` is in flight.
struct Uci {
    state: State,
    variants: Vec<String>,
    variant: String,
    translator: Option<Translator>,
    max_threads: usize,
    threads: usize,
    hash_mb: usize,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    ponder: bool,
    active: Option<SearchHandle>,
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

        Uci {
            state,
            variants,
            variant: default_variant,
            translator,
            max_threads,
            threads: 1,
            hash_mb: HASH_DEFAULT_MB,
            ttable: Arc::new(TTable::with_mb(HASH_DEFAULT_MB * 2 / 3)),
            qtable: Arc::new(QTable::with_mb(HASH_DEFAULT_MB / 3)),
            ponder: false,
            active: None,
        }
    }
}

/// UCI command handlers.
///
/// Each takes the session plus the tokenized command line and applies
/// one command's side effects:
/// - `handle_position`  : rebuilds the position from `startpos` or a
///   FEN, then replays any trailing `moves` list.
/// - `start_search`     : parses `go` limits (depth, movetime, clock +
///   increment with movestogo, infinite/ponder), computes a time budget,
///   and spawns the search thread, recording it as active.
/// - `stop_search`      : interrupts and joins the active search; for a
///   ponder search the withheld `bestmove` is printed on stop.
/// - `handle_ponderhit` : joins the ponder search and relaunches it as a
///   normal timed search using the budget saved at launch.
/// - `handle_setoption` : applies UCI_Variant / Threads / Ponder / Hash
///   / Clear Hash option changes, rebuilding state or tables as needed.
///
/// Params:
/// - uci: &mut Uci  -> the session being mutated
/// - tokens: &[&str] -> whitespace-split command line (where taken)
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
    let is_ponder = uci.ponder && tokens.contains(&"ponder");

    let mut depth = 0usize;
    let mut movetime_ms = 0u128;
    let mut wtime_ms = 0u128;
    let mut btime_ms = 0u128;
    let mut winc_ms = 0u128;
    let mut binc_ms = 0u128;
    let mut movestogo = 0usize;
    let mut infinite = false;

    let mut index = 1;
    while index < tokens.len() {
        match tokens[index] {
            "depth" => {
                depth = tokens
                    .get(index + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                index += 2;
            }
            "movetime" => {
                movetime_ms = tokens
                    .get(index + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                index += 2;
            }
            "wtime" => {
                wtime_ms = tokens
                    .get(index + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                index += 2;
            }
            "btime" => {
                btime_ms = tokens
                    .get(index + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                index += 2;
            }
            "winc" => {
                winc_ms = tokens
                    .get(index + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                index += 2;
            }
            "binc" => {
                binc_ms = tokens
                    .get(index + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                index += 2;
            }
            "movestogo" => {
                movestogo = tokens
                    .get(index + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
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
    let divisor = if movestogo > 0 { movestogo as u128 } else { 40 };

    let timed_ns = if movetime_ms > 0 {
        movetime_ms.saturating_sub(TIME_OVERHEAD_MS) * 1_000_000
    } else if !infinite && !is_ponder && time_ms > 0 {
        let raw = time_ms / divisor + inc_ms * 2 / 3;
        let cap = time_ms.saturating_sub(TIME_OVERHEAD_MS);
        raw.min(cap) * 1_000_000
    } else {
        0
    };

    let ponderhit_timed_ns = if movetime_ms > 0 {
        movetime_ms.saturating_sub(TIME_OVERHEAD_MS) * 1_000_000
    } else if time_ms > 0 {
        let raw = time_ms / divisor + inc_ms * 2 / 3;
        let cap = time_ms.saturating_sub(TIME_OVERHEAD_MS);
        raw.min(cap) * 1_000_000
    } else {
        0
    };

    let search_depth = if depth > 0 { depth } else { MAX_DEPTH };

    let (thread_depth, thread_timed) = if is_ponder {
        (MAX_DEPTH, 0u128)
    } else {
        (search_depth, timed_ns)
    };

    let state_clone = uci.state.clone();
    let tt_clone = Arc::clone(&uci.ttable);
    let qt_clone = Arc::clone(&uci.qtable);
    let dict_clone = uci.translator.clone();
    let tc = uci.threads;

    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let handle = thread::Builder::new()
        .name("search".to_string())
        .stack_size(64 * 1024 * 1024)
        .spawn(move || {
            let mut s = state_clone;
            let mut info = SearchInfo {
                set_depth: thread_depth,
                set_timed: thread_timed,
                ..Default::default()
            };
            let mut bufs = SearchBufs::default();
            let result = search_position(
                &mut s, tt_clone, qt_clone,
                &mut info, &mut bufs, tc,
                dict_clone.as_ref(),
            );
            if !is_ponder {
                print_bestmove(&result, &s, dict_clone.as_ref());
            }
            result
        }).expect("failed to spawn search thread");

    uci.active = Some(SearchHandle {
        handle,
        is_ponder,
        search_depth,
        ponderhit_timed_ns,
    });
}

fn stop_search(uci: &mut Uci) {
    if let Some(sh) = uci.active.take() {
        SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
        let result = sh.handle
            .join()
            .unwrap_or_else(|_| panic!("search thread panicked"));
        SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);
        if sh.is_ponder {
            print_bestmove(&result, &uci.state, uci.translator.as_ref());
        }
    }
}

fn handle_ponderhit(uci: &mut Uci) {
    if let Some(sh) = uci.active.take()
        && sh.is_ponder
    {
        SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
        let _ = sh.handle.join();
        SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

        let state_clone = uci.state.clone();
        let tt_clone = Arc::clone(&uci.ttable);
        let qt_clone = Arc::clone(&uci.qtable);
        let dict_clone = uci.translator.clone();
        let tc = uci.threads;
        let search_depth = sh.search_depth;
        let ponderhit_timed_ns = sh.ponderhit_timed_ns;

        let handle = thread::Builder::new()
            .name("search".to_string())
            .stack_size(64 * 1024 * 1024)
            .spawn(move || {
                let mut s = state_clone;
                let mut info = SearchInfo {
                    set_depth: search_depth,
                    set_timed: ponderhit_timed_ns,
                    ..Default::default()
                };
                let mut bufs = SearchBufs::default();
                let result = search_position(
                    &mut s, tt_clone, qt_clone,
                    &mut info, &mut bufs, tc,
                    dict_clone.as_ref(),
                );
                print_bestmove(&result, &s, dict_clone.as_ref());
                result
            }).expect("failed to spawn search thread");

        uci.active = Some(SearchHandle {
            handle,
            is_ponder: false,
            search_depth,
            ponderhit_timed_ns: 0,
        });
    }
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

            uci.translator = Translator::find(&v, "uci");
            uci.variant = v;
        }
        (OPT_THREADS, Some(v)) => {
            if let Ok(n) = v.parse::<usize>() {
                uci.threads = n.clamp(1, uci.max_threads);
            }
        }
        (OPT_PONDER, Some(v)) => {
            uci.ponder = v.to_lowercase() == "true";
        }
        (OPT_HASH, Some(v)) => {
            if let Ok(mb) = v.parse::<usize>() {
                uci.hash_mb = mb.clamp(1, HASH_MAX_MB);
                uci.ttable = Arc::new(TTable::with_mb(uci.hash_mb * 2 / 3));
                uci.qtable = Arc::new(QTable::with_mb(uci.hash_mb / 3));
            }
        }
        (OPT_CLEAR_HASH, None) => {
            uci.ttable = Arc::new(TTable::with_mb(uci.hash_mb * 2 / 3));
            uci.qtable = Arc::new(QTable::with_mb(uci.hash_mb / 3));
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
/// - uci: &mut Uci -> the session being driven
/// - line: &str    -> one trimmed line of input
///
/// Return:
/// bool -> true when the session should terminate (`quit`)
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
            println!("uciok");
            stdout().flush().ok();
        }
        "isready" => {
            println!("readyok");
            stdout().flush().ok();
        }
        "ucinewgame" => {
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
            stop_search(uci);
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
/// session, and feeds stdin lines to `execute_command` until `quit`.
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

    Ok(())
}
