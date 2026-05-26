//! # uci.rs
//!
//! Implements the Universal Chess Interface (UCI) protocol.
//!
//! Supports uci, isready, ucinewgame, position, go (with ponder, movestogo,
//! wtime/btime/winc/binc, movetime, depth, infinite), ponderhit, stop,
//! setoption (UCI_Variant, Threads, Ponder, Hash), and quit. Available
//! variants are discovered by scanning res/dicts/ for dict files that list
//! "uci" under their = protocols = section and have a matching .conf file in
//! configs/. Per-depth info lines include hashfull and cpuload fields.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 24/05/2026
use crate::*;

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

fn handle_position(
    state: &mut State,
    tokens: &[&str],
    dict: Option<&Translator>,
) {
    let startpos = state.statics.startpos.clone();
    state.reset();

    let mut index = 1;

    if tokens.get(index).copied() == Some("startpos") {
        parse_fen(state, &startpos, None);
        index += 1;
    } else if tokens.get(index).copied() == Some("fen") {
        index += 1;
        let fen_end = tokens[index..]
            .iter()
            .position(|&t| t == "moves")
            .map(|p| p + index)
            .unwrap_or(tokens.len());
        let fen_str = tokens[index..fen_end].join(" ");
        parse_fen(state, &fen_str, dict);
        index = fen_end;
    }

    refresh_eval_state(state);

    if tokens.get(index).copied() == Some("moves") {
        for move_str in &tokens[index + 1..] {
            if let Some(mv) = parse_move(move_str, state, dict) {
                make_move!(state, mv);
            }
        }
    }
}

fn handle_go(
    state: &mut State,
    tokens: &[&str],
    dict: Option<&Translator>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    thread_count: usize,
    protocol: u8,
) -> SearchResult {
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

    let mut info = SearchInfo::default();

    if movetime_ms > 0 {
        let safe = movetime_ms.saturating_sub(TIME_OVERHEAD_MS);
        info.set_timed = safe * 1_000_000;
    } else if !infinite {
        let (time_ms, inc_ms) = if state.playing == WHITE {
            (wtime_ms, winc_ms)
        } else {
            (btime_ms, binc_ms)
        };
        if time_ms > 0 {
            let divisor =
                if movestogo > 0 { movestogo as u128 } else { 40 };
            let raw_alloc = time_ms / divisor + inc_ms * 2 / 3;
            let cap = time_ms.saturating_sub(TIME_OVERHEAD_MS);
            info.set_timed = raw_alloc.min(cap) * 1_000_000;
        }
    }

    info.set_depth = if depth > 0 { depth } else { MAX_DEPTH };
    info.set_moves = movestogo;

    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let mut bufs = SearchBufs::default();
    search_position(
        state, ttable, qtable,
        &mut info, &mut bufs, thread_count, dict, protocol,
    )
}

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

fn handle_ponder(
    state: &mut State,
    tokens: &[&str],
    dict: Option<&Translator>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    thread_count: usize,
    receiver: &Receiver<String>,
) -> bool {
    let mut wtime_ms = 0u128;
    let mut btime_ms = 0u128;
    let mut winc_ms = 0u128;
    let mut binc_ms = 0u128;
    let mut movestogo = 0usize;
    let mut movetime_ms = 0u128;

    let mut i = 1;
    while i < tokens.len() {
        match tokens[i] {
            "wtime" => {
                wtime_ms = tokens.get(i + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                i += 2;
            }
            "btime" => {
                btime_ms = tokens.get(i + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                i += 2;
            }
            "winc" => {
                winc_ms = tokens.get(i + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                i += 2;
            }
            "binc" => {
                binc_ms = tokens.get(i + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                i += 2;
            }
            "movestogo" => {
                movestogo = tokens.get(i + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                i += 2;
            }
            "movetime" => {
                movetime_ms = tokens.get(i + 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0);
                i += 2;
            }
            _ => { i += 1; }
        }
    }

    let (time_ms, inc_ms) = if state.playing == WHITE {
        (wtime_ms, winc_ms)
    } else {
        (btime_ms, binc_ms)
    };
    let divisor =
        if movestogo > 0 { movestogo as u128 } else { 40 };
    let ponder_time_ns = if movetime_ms > 0 {
        movetime_ms.saturating_sub(TIME_OVERHEAD_MS) * 1_000_000
    } else if time_ms > 0 {
        let raw = time_ms / divisor + inc_ms * 2 / 3;
        let cap = time_ms.saturating_sub(TIME_OVERHEAD_MS);
        raw.min(cap) * 1_000_000
    } else {
        0
    };

    let state_clone = state.clone();
    let tt_clone = Arc::clone(&ttable);
    let qt_clone = Arc::clone(&qtable);
    let dict_clone = dict.cloned();
    let tc = thread_count;

    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let ponder_thread = thread::Builder::new()
        .name("ponder".to_string())
        .stack_size(64 * 1024 * 1024)
        .spawn(move || {
            let mut s = state_clone;
            let mut info = SearchInfo {
                set_depth: MAX_DEPTH,
                ..Default::default()
            };
            let mut bufs = SearchBufs::default();
            search_position(
                &mut s, tt_clone, qt_clone,
                &mut info, &mut bufs, tc,
                dict_clone.as_ref(), PROTOCOL_UCI,
            )
        }).expect("failed to spawn ponder thread");

    loop {
        let ponder_line = match receiver.recv() {
            Ok(l) => l,
            Err(_) => {
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
                let _ = ponder_thread.join();
                return false;
            }
        };
        let ponder_toks: Vec<&str> =
            ponder_line.split_whitespace().collect();
        match ponder_toks.first().copied().unwrap_or("") {
            "ponderhit" => {
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
                let _ = ponder_thread.join();
                SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);
                let mut info = SearchInfo {
                    set_depth: MAX_DEPTH,
                    set_timed: ponder_time_ns,
                    ..Default::default()
                };
                let mut bufs = SearchBufs::default();
                let result = search_position(
                    state,
                    Arc::clone(&ttable),
                    Arc::clone(&qtable),
                    &mut info, &mut bufs, thread_count,
                    dict, PROTOCOL_UCI,
                );
                print_bestmove(&result, state, dict);
                return true;
            }
            "stop" => {
                let result = ponder_thread.join()
                    .unwrap_or_else(|_| {
                        panic!("ponder thread panicked")
                    });
                print_bestmove(&result, state, dict);
                return true;
            }
            "quit" => {
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
                let _ = ponder_thread.join();
                return false;
            }
            _ => {}
        }
    }
}

pub fn uci() -> IoResult<()> {
    println!("AnekaMacam {} by Alden Luthfi", env!("CARGO_PKG_VERSION"));

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

    let mut translator = Translator::find(&default_variant, "uci");
    let mut current_variant = default_variant.clone();

    let max_threads = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let mut thread_count: usize = 1;

    let mut ttable = Arc::new(TTable::with_mb(HASH_DEFAULT_MB * 2 / 3));
    let mut qtable = Arc::new(QTable::with_mb(HASH_DEFAULT_MB / 3));

    let mut ponder_enabled = false;

    let (sender, receiver) = channel::<String>();

    thread::spawn(move || {
        for line in stdin().lock().lines().map_while(Result::ok) {
            let trimmed = line.trim().to_string();
            if trimmed.split_whitespace().next() == Some("stop") {
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
            }
            if sender.send(trimmed).is_err() {
                break;
            }
        }
    });

    while let Ok(line) = receiver.recv() {
        let tokens: Vec<&str> = line.split_whitespace().collect();
        let command = tokens.first().copied().unwrap_or("");

        log_3!("UCI Command: {}", line);

        match command {
            "uci" => {
                println!("id name anekamacam");
                println!("id author Alden Luthfi");
                let vars_str: String = variants
                    .iter()
                    .map(|v| format!(" var {}", v))
                    .collect();
                println!(
                    "option name {} type combo default {}{}",
                    OPT_VARIANT, current_variant, vars_str,
                );
                println!(
                    "option name {} type spin default 1 min 1 max {}",
                    OPT_THREADS, max_threads,
                );
                println!(
                    "option name {} type check default false",
                    OPT_PONDER,
                );
                println!(
                    "option name {} type spin default {} min 1 max {}",
                    OPT_HASH, HASH_DEFAULT_MB, HASH_MAX_MB,
                );
                println!("uciok");
                stdout().flush().ok();
            }
            "isready" => {
                println!("readyok");
                stdout().flush().ok();
            }
            "ucinewgame" => {
                let sp = state.statics.startpos.clone();
                state.reset();
                parse_fen(&mut state, &sp, None);
                refresh_eval_state(&mut state);
            }
            "position" => {
                handle_position(
                    &mut state,
                    &tokens,
                    translator.as_ref(),
                );
            }
            "go" => {
                let is_ponder = tokens.contains(&"ponder");
                if is_ponder && ponder_enabled {
                    let keep_going = handle_ponder(
                        &mut state,
                        &tokens,
                        translator.as_ref(),
                        Arc::clone(&ttable),
                        Arc::clone(&qtable),
                        thread_count,
                        &receiver,
                    );
                    if !keep_going {
                        return Ok(());
                    }
                } else {
                    let result = handle_go(
                        &mut state,
                        &tokens,
                        translator.as_ref(),
                        Arc::clone(&ttable),
                        Arc::clone(&qtable),
                        thread_count,
                        PROTOCOL_UCI,
                    );
                    print_bestmove(
                        &result, &state, translator.as_ref()
                    );
                }
            }
            "setoption" => {
                let name = tokens
                    .iter()
                    .position(|&t| t == "name");
                let value = tokens
                    .iter()
                    .position(|&t| t == "value");

                if let (Some(a), Some(b)) = (name, value) {
                    let name  = tokens[(a + 1)..b].join(" ");
                    let value = tokens[(b + 1)..].join(" ");

                    match name.as_str() {
                        OPT_VARIANT if variants.contains(&value) => {
                            let conf = format!("{}.conf", value);
                            state = parse_config_file(&conf);

                            let position = state.statics.startpos.clone();

                            state.reset();
                            parse_fen(&mut state, &position, None);

                            refresh_eval_state(&mut state);

                            translator = Translator::find(&value, "uci");
                            current_variant = value;
                        }
                        OPT_THREADS => {
                            if let Ok(n) = value.parse::<usize>() {
                                thread_count = n.clamp(1, max_threads);
                            }
                        }
                        OPT_PONDER => {
                            ponder_enabled = value.to_lowercase() == "true";
                        }
                        OPT_HASH => {
                            if let Ok(mb) = value.parse::<usize>() {
                                let hash_mb = mb.clamp(1, HASH_MAX_MB);
                                ttable = Arc::new(
                                    TTable::with_mb(hash_mb * 2 / 3)
                                );
                                qtable = Arc::new(
                                    QTable::with_mb(hash_mb / 3)
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
            "quit" => break,
            _ => {}
        }
    }

    Ok(())
}
