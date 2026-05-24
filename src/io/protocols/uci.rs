//! # uci.rs
//!
//! Implements the Universal Chess Interface (UCI) protocol.
//!
//! Supports uci, isready, ucinewgame, position, go, setoption (Variant),
//! and quit. Available variants are discovered by scanning res/dicts/ for
//! dict files that list "uci" under their = protocols = section and have a
//! matching .conf file in configs/.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 24/05/2026
use crate::*;

fn list_uci_variants() -> Vec<String> {
    let mut variants = Vec::new();

    let Ok(entries) = fs::read_dir(DICTS_DIR) else {
        return variants;
    };

    for entry in entries.flatten() {
        let path = entry.path();

        if path.extension().and_then(|e| e.to_str()) != Some("dict") {
            continue;
        }

        let stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("")
            .to_string();

        let conf_path = format!("{}/{}.conf", CONFIGS_DIR, stem);
        if !Path::new(&conf_path).is_file() {
            continue;
        }

        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };

        let uncommented = COMMENT_PATTERN.replace_all(&content, "");
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
            variants.push(stem);
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
) {
    let mut depth = 0usize;
    let mut movetime_ms = 0u128;
    let mut wtime_ms = 0u128;
    let mut btime_ms = 0u128;
    let mut winc_ms = 0u128;
    let mut binc_ms = 0u128;
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
            "infinite" => {
                infinite = true;
                index += 1;
            }
            _ => { index += 1; }
        }
    }

    let mut info = SearchInfo::default();

    if movetime_ms > 0 {
        info.set_timed = movetime_ms * 1_000_000;
    } else if !infinite {
        let (time_ms, inc_ms) = if state.playing == WHITE {
            (wtime_ms, winc_ms)
        } else {
            (btime_ms, binc_ms)
        };
        if time_ms > 0 {
            info.set_timed = (time_ms / 20 + inc_ms) * 1_000_000;
        }
    }

    info.set_depth = if depth > 0 { depth } else { MAX_DEPTH };

    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let mut bufs = SearchBufs::default();
    let result = search_position(
        state, ttable, qtable,
        &mut info, &mut bufs, thread_count, dict, protocol,
    );

    let best = format_move(&result.best_move, state, dict);
    println!("bestmove {}", best);
    stdout().flush().ok();
}

pub fn uci() -> IoResult<()> {
    let variants = list_uci_variants();
    let default_variant = variants
        .iter()
        .find(|v| v.as_str() == "fide")
        .or_else(|| variants.first())
        .cloned()
        .unwrap_or_else(|| "fide".to_string());

    let config_path =
        format!("{}/{}.conf", CONFIGS_DIR, default_variant);
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

    let ttable = Arc::new(TTable::default());
    let qtable = Arc::new(QTable::default());

    let (sender, receiver) = channel::<String>();

    thread::spawn(move || {
        for line in stdin().lock().lines().flatten() {
            let trimmed = line.trim().to_string();
            if trimmed.split_whitespace().next() == Some("stop") {
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
            }
            if sender.send(trimmed).is_err() {
                break;
            }
        }
    });

    loop {
        let line = match receiver.recv() {
            Ok(l) => l,
            Err(_) => break,
        };

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
                    "option name Variant type combo default {}{}",
                    current_variant, vars_str,
                );
                println!(
                    "option name Threads type spin \
                     default 1 min 1 max {}",
                    max_threads,
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
                handle_go(
                    &mut state,
                    &tokens,
                    translator.as_ref(),
                    Arc::clone(&ttable),
                    Arc::clone(&qtable),
                    thread_count,
                    PROTOCOL_UCI,
                );
            }
            "setoption" => {
                let name_pos = tokens
                    .iter()
                    .position(|&t| t == "name");
                let value_pos = tokens
                    .iter()
                    .position(|&t| t == "value");

                if let (Some(ni), Some(vi)) = (name_pos, value_pos) {
                    let name = tokens[ni + 1..vi].join(" ");
                    let value = tokens[vi + 1..].join(" ");

                    match name.to_lowercase().as_str() {
                        "variant" if variants.contains(&value) => {
                            let conf = format!(
                                "{}/{}.conf",
                                CONFIGS_DIR,
                                value
                            );

                            state = parse_config_file(&conf);

                            let position = state.statics.startpos.clone();

                            state.reset();

                            parse_fen(&mut state, &position, None);
                            refresh_eval_state(&mut state);

                            translator = Translator::find(&value, "uci");
                            current_variant = value;
                        },
                        "threads" => {
                            if let Ok(n) = value.parse::<usize>() {
                                thread_count = n.clamp(1, max_threads);
                            }
                        },
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
