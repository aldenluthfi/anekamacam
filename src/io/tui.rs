//! # tui.rs
//!
//! Ratatui-based interactive interface for the engine.
//!
//! This module replaces plain CLI printing with an interactive, tabbed view.
//! It keeps command-driven gameplay while rendering board/logs/move history
//! and a detailed game-state tab.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 23/04/2026

use crate::*;

struct TuiApp {
    tab_idx: usize,
    detail_scroll: u16,
    log_scroll: u16,
    show_help: bool,
    logs: VecDeque<String>,
    input: String,
}

impl TuiApp {
    fn new() -> Self {
        Self {
            tab_idx: 0,
            detail_scroll: 0,
            log_scroll: 0,
            show_help: false,
            logs: VecDeque::with_capacity(1024),
            input: String::new(),
        }
    }

    fn push_logs(&mut self, entries: Vec<String>) {
        let has_new_entries = !entries.is_empty();

        for entry in entries {
            self.logs.push_back(entry);
        }

        while self.logs.len() > 512 {
            self.logs.pop_front();
        }

        if has_new_entries {
            self.log_scroll = self.logs.len() as u16;
        }
    }
}

const TAB_TITLES: [&str; 5] = ["Game", "Overview", "Pieces", "Zones", "PST"];

fn draw_tabs(frame: &mut Frame<'_>, area: Rect, app: &TuiApp) {
    let titles = TAB_TITLES
        .iter()
        .map(|title| Line::from(*title))
        .collect::<Vec<_>>();

    let tabs = Tabs::new(titles)
        .select(app.tab_idx)
        .style(Style::default().fg(Color::Gray))
        .highlight_style(
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        );

    frame.render_widget(tabs, area);
}

fn draw_game_tab(frame: &mut Frame<'_>, area: Rect, state: &State, app: &TuiApp) {
    let columns = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(70), Constraint::Percentage(30)])
        .split(area);

    let left = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(62), Constraint::Percentage(38)])
        .split(columns[0]);

    let board_text = format_game_state(state);
    let board = Paragraph::new(board_text)
        .style(Style::default().fg(Color::White))
        .block(Block::default().borders(Borders::ALL).title("Board"))
        .wrap(Wrap { trim: false });
    frame.render_widget(board, left[0]);

    let log_lines = app
        .logs
        .iter()
        .map(|line| {
            let level_color = if line.starts_with("[1]") {
                Color::LightRed
            } else if line.starts_with("[2]") {
                Color::Yellow
            } else if line.starts_with("[3]") {
                Color::LightGreen
            } else if line.starts_with("[4]") {
                Color::LightBlue
            } else if line.starts_with("[5]") {
                Color::Magenta
            } else {
                Color::Gray
            };

            if line.starts_with('[') {
                if let Some(end_idx) = line.find(']') {
                    let (level, rest) = line.split_at(end_idx + 1);
                    return Line::from(vec![
                        Span::styled(level.to_string(), Style::default().fg(level_color)),
                        Span::raw(rest.to_string()),
                    ]);
                }
            }

            Line::from(Span::raw(line.clone()))
        })
        .collect::<Vec<_>>();

    let log_view_height = left[1].height.saturating_sub(2);
    let max_log_scroll = (app.logs.len() as u16).saturating_sub(log_view_height);
    let log_scroll = app.log_scroll.min(max_log_scroll);

    let logs = Paragraph::new(Text::from(log_lines))
        .block(Block::default().borders(Borders::ALL).title("Logs"))
        .wrap(Wrap { trim: false })
        .scroll((log_scroll, 0));
    frame.render_widget(logs, left[1]);

    let move_items = collect_move_items(state);
    let moves = List::new(move_items).block(Block::default().borders(Borders::ALL).title("Moves"));
    frame.render_widget(moves, columns[1]);
}

fn game_phase_label(phase: u8) -> &'static str {
    match phase {
        OPENING => "Opening",
        MIDDLEGAME => "Middlegame",
        ENDGAME => "Endgame",
        _ => "Unknown",
    }
}

fn board_to_square_list(board: &Board, state: &State) -> String {
    let mut squares = Vec::new();
    let board_size = (state.files as usize) * (state.ranks as usize);

    for square_idx in 0..board_size {
        if get!(board, square_idx as u32) {
            squares.push(format_square(square_idx as Square, state));
        }
    }

    if squares.is_empty() {
        "-".to_string()
    } else {
        squares.join(", ")
    }
}

fn collect_piece_type_pairs(state: &State) -> Vec<(usize, usize)> {
    let mut type_pairs = Vec::new();

    for (white_idx, piece) in state.pieces.iter().enumerate() {
        if p_color!(piece) != WHITE {
            continue;
        }

        let black_idx = state
            .piece_swap_map
            .get(&(white_idx as u8))
            .copied()
            .unwrap_or_else(|| {
                panic!(
                    "Missing black counterpart for white piece index {} ({})",
                    white_idx, piece.char
                )
            }) as usize;

        if p_color!(state.pieces[black_idx]) != BLACK {
            panic!(
                "Invalid black counterpart mapping for white piece index {}",
                white_idx
            );
        }

        type_pairs.push((white_idx, black_idx));
    }

    type_pairs
}

fn render_scrollable_table(
    frame: &mut Frame<'_>,
    area: Rect,
    title: &str,
    headers: &[String],
    rows: &[Vec<String>],
    widths: &[Constraint],
    scroll: u16,
) {
    let header = Row::new(
        headers
            .iter()
            .map(|header| Cell::from((*header).to_string()))
            .collect::<Vec<_>>(),
    )
    .style(
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD),
    );

    let visible_rows = area.height.saturating_sub(4).max(1) as usize;
    let max_scroll = rows.len().saturating_sub(visible_rows) as u16;
    let start = scroll.min(max_scroll) as usize;
    let end = (start + visible_rows).min(rows.len());

    let visible = rows[start..end]
        .iter()
        .map(|row| {
            Row::new(
                row.iter()
                    .map(|cell| Cell::from(cell.clone()))
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();

    let title = format!("{} ({}/{})", title, end, rows.len());

    let table = Table::new(visible, widths.iter().copied())
        .header(header)
        .column_spacing(1)
        .block(Block::default().borders(Borders::ALL).title(title))
        .style(Style::default().fg(Color::White));

    frame.render_widget(table, area);
}

fn build_overview_rows(state: &State) -> Vec<Vec<String>> {
    let mut rows = vec![
        vec!["Variant".to_string(), state.title.clone()],
        vec![
            "Board".to_string(),
            format!("{}x{}", state.files, state.ranks),
        ],
        vec![
            "Side to move".to_string(),
            if state.playing == WHITE {
                "White".to_string()
            } else {
                "Black".to_string()
            },
        ],
        vec![
            "Game phase".to_string(),
            game_phase_label(state.game_phase).to_string(),
        ],
        vec!["Ply counter".to_string(), state.ply_counter.to_string()],
        vec!["Search ply".to_string(), state.search_ply.to_string()],
        vec!["Hash".to_string(), format!("{:032X}", state.position_hash)],
        vec![
            "Material (Opening)".to_string(),
            format!(
                "White {} | Black {}",
                state.opening_material[WHITE as usize], state.opening_material[BLACK as usize]
            ),
        ],
        vec![
            "Material (Endgame)".to_string(),
            format!(
                "White {} | Black {}",
                state.endgame_material[WHITE as usize], state.endgame_material[BLACK as usize]
            ),
        ],
        vec![
            "PST Bonus (Opening)".to_string(),
            format!(
                "White {} | Black {}",
                state.opening_pst_bonus[WHITE as usize], state.opening_pst_bonus[BLACK as usize]
            ),
        ],
        vec![
            "PST Bonus (Endgame)".to_string(),
            format!(
                "White {} | Black {}",
                state.endgame_pst_bonus[WHITE as usize], state.endgame_pst_bonus[BLACK as usize]
            ),
        ],
        vec![
            "Castling rights".to_string(),
            if castling!(state) {
                let mut rights = String::new();
                if state.castling_state & WK_CASTLE != 0 {
                    rights.push('K');
                }
                if state.castling_state & WQ_CASTLE != 0 {
                    rights.push('Q');
                }
                if state.castling_state & BK_CASTLE != 0 {
                    rights.push('k');
                }
                if state.castling_state & BQ_CASTLE != 0 {
                    rights.push('q');
                }

                if rights.is_empty() {
                    "-".to_string()
                } else {
                    rights
                }
            } else {
                "N/A".to_string()
            },
        ],
        vec![
            "En passant".to_string(),
            if en_passant!(state) {
                if state.en_passant_square == NO_EN_PASSANT {
                    "-".to_string()
                } else {
                    format_square(enp_square!(state.en_passant_square) as Square, state)
                }
            } else {
                "N/A".to_string()
            },
        ],
    ];

    if halfmove_clock!(state) {
        rows.push(vec![
            "Halfmove".to_string(),
            format!("{}/{}", state.halfmove_clock, state.halfmove_limit),
        ]);
    }

    if repetition_limit!(state) {
        let repetition_count = state
            .position_hash_map
            .get(&state.position_hash)
            .copied()
            .unwrap_or(1);

        rows.push(vec![
            "Repetition".to_string(),
            format!("{}/{}", repetition_count, state.repetition_limit),
        ]);
    }

    rows
}

fn build_piece_rows(state: &State) -> Vec<Vec<String>> {
    let mut rows = Vec::new();

    for (idx, piece) in state.pieces.iter().enumerate() {
        let mut promotes_to = Vec::new();
        for promoted in &piece.promotions {
            promotes_to.push(state.pieces[*promoted as usize].char.to_string());
        }

        let promotes_from = state
            .pieces
            .iter()
            .filter(|candidate| candidate.promotions.contains(&(idx as u8)))
            .map(|candidate| candidate.char.to_string())
            .collect::<Vec<_>>();

        let count_limit = if count_limits!(state) {
            let limit = state.piece_limit[idx];
            if limit == u32::MAX {
                "-".to_string()
            } else {
                limit.to_string()
            }
        } else {
            "N/A".to_string()
        };

        rows.push(vec![
            piece.name.clone(),
            idx.to_string(),
            piece.char.to_string(),
            if p_color!(piece) == WHITE {
                "White".to_string()
            } else {
                "Black".to_string()
            },
            p_ovalue!(piece).to_string(),
            p_evalue!(piece).to_string(),
            p_is_royal!(piece).to_string(),
            p_is_big!(piece).to_string(),
            p_is_major!(piece).to_string(),
            count_limit,
            p_can_promote!(piece).to_string(),
            if promotes_to.is_empty() {
                "-".to_string()
            } else {
                promotes_to.join(",")
            },
            if promotes_from.is_empty() {
                "-".to_string()
            } else {
                promotes_from.join(",")
            },
            state.piece_count[idx].to_string(),
            state.piece_in_hand[WHITE as usize][idx].to_string(),
            state.piece_in_hand[BLACK as usize][idx].to_string(),
        ]);
    }

    rows
}

fn build_zone_rows(state: &State) -> Vec<Vec<String>> {
    let mut rows = Vec::new();

    for (idx, piece) in state.pieces.iter().enumerate() {
        if promotions!(state) && p_can_promote!(piece) {
            let mandatory = board_to_square_list(&state.promotion_zones_mandatory[idx], state);
            let optional = board_to_square_list(&state.promotion_zones_optional[idx], state);

            if mandatory != "-" || optional != "-" {
                rows.push(vec![
                    "Promotion".to_string(),
                    piece.name.clone(),
                    piece.char.to_string(),
                    if p_color!(piece) == WHITE {
                        "White".to_string()
                    } else {
                        "Black".to_string()
                    },
                    mandatory,
                    optional,
                ]);
            }
        }

        if forbidden_zones!(state) {
            let forbidden = board_to_square_list(&state.forbidden_zones[idx], state);
            if forbidden != "-" {
                rows.push(vec![
                    "Forbidden".to_string(),
                    piece.name.clone(),
                    piece.char.to_string(),
                    if p_color!(piece) == WHITE {
                        "White".to_string()
                    } else {
                        "Black".to_string()
                    },
                    forbidden,
                    "-".to_string(),
                ]);
            }
        }

        if setup_phase!(state) {
            let setup = board_to_square_list(&state.initial_setup[idx], state);
            if setup != "-" {
                rows.push(vec![
                    "Initial Setup".to_string(),
                    piece.name.clone(),
                    piece.char.to_string(),
                    if p_color!(piece) == WHITE {
                        "White".to_string()
                    } else {
                        "Black".to_string()
                    },
                    setup,
                    "-".to_string(),
                ]);
            }
        }
    }

    if halfmove_clock!(state) {
        let resetters = state
            .pieces
            .iter()
            .enumerate()
            .filter_map(|(idx, piece)| state.halfmove_pieces[idx].then_some(piece.char.to_string()))
            .collect::<Vec<_>>();

        rows.push(vec![
            "Halfmove Rule".to_string(),
            "Global".to_string(),
            "-".to_string(),
            "-".to_string(),
            format!("Limit: {}", state.halfmove_limit),
            format!(
                "Resetters: {}",
                if resetters.is_empty() {
                    "-".to_string()
                } else {
                    resetters.join(",")
                }
            ),
        ]);
    }

    if rows.is_empty() {
        rows.push(vec![
            "-".to_string(),
            "No zones/rules configured".to_string(),
            "-".to_string(),
            "-".to_string(),
            "-".to_string(),
            "-".to_string(),
        ]);
    }

    rows
}

fn build_pst_rows(state: &State) -> (Vec<String>, Vec<Vec<String>>) {
    let mut headers = vec![
        "Piece".to_string(),
        "Sym".to_string(),
        "Color".to_string(),
        "Phase".to_string(),
        "Rank".to_string(),
    ];
    for file in 0..state.files {
        if state.files <= 26 {
            headers.push(((b'A' + file) as char).to_string());
        } else {
            headers.push(file.to_string());
        }
    }

    let mut rows = Vec::new();

    for (white_idx, black_idx) in collect_piece_type_pairs(state) {
        for (piece_idx, color_label) in [(white_idx, "White"), (black_idx, "Black")] {
            let piece = &state.pieces[piece_idx];

            for (phase_label, pst) in [
                ("Opening", &state.pst_opening[piece_idx]),
                ("Endgame", &state.pst_endgame[piece_idx]),
            ] {
                for rank in (0..state.ranks).rev() {
                    let mut row = vec![
                        piece.name.clone(),
                        piece.char.to_string(),
                        color_label.to_string(),
                        phase_label.to_string(),
                        format!("{:02}", rank),
                    ];

                    for file in 0..state.files {
                        let idx = rank as usize * state.files as usize + file as usize;
                        row.push(pst[idx].to_string());
                    }

                    rows.push(row);
                }
            }
        }
    }

    (headers, rows)
}

fn draw_overview_tab(frame: &mut Frame<'_>, area: Rect, state: &State, app: &TuiApp) {
    let rows = build_overview_rows(state);
    let widths = [Constraint::Length(22), Constraint::Min(20)];

    render_scrollable_table(
        frame,
        area,
        "Variant & Runtime Overview",
        &["Field".to_string(), "Value".to_string()],
        &rows,
        &widths,
        app.detail_scroll,
    );
}

fn draw_pieces_tab(frame: &mut Frame<'_>, area: Rect, state: &State, app: &TuiApp) {
    let rows = build_piece_rows(state);
    let widths = [
        Constraint::Length(14),
        Constraint::Length(4),
        Constraint::Length(4),
        Constraint::Length(6),
        Constraint::Length(6),
        Constraint::Length(6),
        Constraint::Length(5),
        Constraint::Length(5),
        Constraint::Length(5),
        Constraint::Length(10),
        Constraint::Length(7),
        Constraint::Length(10),
        Constraint::Length(12),
        Constraint::Length(8),
        Constraint::Length(8),
        Constraint::Length(8),
    ];

    render_scrollable_table(
        frame,
        area,
        "Piece Types",
        &[
            "Name".to_string(),
            "Idx".to_string(),
            "Sym".to_string(),
            "Color".to_string(),
            "OVal".to_string(),
            "EVal".to_string(),
            "Royal".to_string(),
            "Big".to_string(),
            "Major".to_string(),
            "CountLim".to_string(),
            "Promote".to_string(),
            "To".to_string(),
            "From".to_string(),
            "Board".to_string(),
            "W Hand".to_string(),
            "B Hand".to_string(),
        ],
        &rows,
        &widths,
        app.detail_scroll,
    );
}

fn draw_zones_tab(frame: &mut Frame<'_>, area: Rect, state: &State, app: &TuiApp) {
    let rows = build_zone_rows(state);
    let widths = [
        Constraint::Length(13),
        Constraint::Length(16),
        Constraint::Length(4),
        Constraint::Length(6),
        Constraint::Percentage(42),
        Constraint::Percentage(35),
    ];

    render_scrollable_table(
        frame,
        area,
        "Zones, Setup, and Halfmove Rules",
        &[
            "Section".to_string(),
            "Piece".to_string(),
            "Sym".to_string(),
            "Color".to_string(),
            "Primary".to_string(),
            "Secondary".to_string(),
        ],
        &rows,
        &widths,
        app.detail_scroll,
    );
}

fn draw_pst_tab(frame: &mut Frame<'_>, area: Rect, state: &State, app: &TuiApp) {
    let (headers, rows) = build_pst_rows(state);

    let mut widths = vec![
        Constraint::Length(14),
        Constraint::Length(4),
        Constraint::Length(6),
        Constraint::Length(8),
        Constraint::Length(5),
    ];

    for _ in 0..state.files {
        widths.push(Constraint::Length(5));
    }

    render_scrollable_table(
        frame,
        area,
        "Piece-Square Tables",
        &headers,
        &rows,
        &widths,
        app.detail_scroll,
    );
}

fn draw_input(frame: &mut Frame<'_>, area: Rect, app: &TuiApp) {
    let command = Paragraph::new(app.input.clone())
        .block(Block::default().borders(Borders::ALL).title("Command"));

    frame.render_widget(command, area);
}

fn draw_help_bar(frame: &mut Frame<'_>, area: Rect) {
    let help =
        Paragraph::new("? help | Tab switch tab | Up/Down scroll tables | Enter run | q quit")
            .style(Style::default().fg(Color::DarkGray));

    frame.render_widget(help, area);
}

fn centered_rect(width: u16, height: u16, area: Rect) -> Rect {
    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - height) / 2),
            Constraint::Percentage(height),
            Constraint::Percentage((100 - height) / 2),
        ])
        .split(area);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - width) / 2),
            Constraint::Percentage(width),
            Constraint::Percentage((100 - width) / 2),
        ])
        .split(vertical[1])[1]
}

fn draw_help_popup(frame: &mut Frame<'_>, area: Rect) {
    let popup = centered_rect(70, 74, area);

    let rows = [
        ("?", "Toggle this help popup"),
        ("Esc", "Close help popup or clear input"),
        ("Tab", "Switch tabs"),
        (
            "Up / Down",
            "Scroll logs (Game tab) or table rows (other tabs)",
        ),
        ("Enter", "Run command"),
        ("Backspace", "Delete last input character"),
        ("q", "Quit TUI (only when input is empty)"),
        ("u", "Undo one move"),
        ("r", "Reset game to root position"),
        ("fen <fen>", "Load a FEN position"),
        ("search <d>", "Run search at depth d"),
        ("go <d>", "Search and play best move at depth d"),
        ("<move>", "Play a legal move in engine notation"),
    ];

    let mut lines = vec![
        Line::from(Span::styled(
            "Keybinds",
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        )),
        Line::from(""),
    ];

    for (key, desc) in rows {
        let row = format!("{:<12} {}", key, desc);
        lines.push(Line::from(row));
    }

    lines.push(Line::from(""));
    lines.push(Line::from("Press ? or Esc to close"));

    frame.render_widget(Clear, popup);
    frame.render_widget(
        Paragraph::new(Text::from(lines))
            .style(Style::default().fg(Color::White))
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Help")
                    .border_style(Style::default().fg(Color::Yellow)),
            )
            .wrap(Wrap { trim: false }),
        popup,
    );
}

fn render(frame: &mut Frame<'_>, state: &State, app: &TuiApp) {
    let root = frame.area();

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Min(0),
            Constraint::Length(3),
            Constraint::Length(1),
        ])
        .split(root);

    draw_tabs(frame, chunks[0], app);

    match app.tab_idx {
        0 => draw_game_tab(frame, chunks[1], state, app),
        1 => draw_overview_tab(frame, chunks[1], state, app),
        2 => draw_pieces_tab(frame, chunks[1], state, app),
        3 => draw_zones_tab(frame, chunks[1], state, app),
        _ => draw_pst_tab(frame, chunks[1], state, app),
    }

    draw_input(frame, chunks[2], app);
    draw_help_bar(frame, chunks[3]);

    if app.show_help {
        draw_help_popup(frame, root);
    }
}

fn collect_move_items(state: &State) -> Vec<ListItem<'static>> {
    let mut moves = Vec::new();

    for snapshot in &state.history {
        if snapshot.move_ply == null_move() {
            continue;
        }

        moves.push(format_move(&snapshot.move_ply, state));
    }

    let mut items = Vec::new();

    for (idx, pair) in moves.chunks(2).enumerate() {
        let white = pair.first().cloned().unwrap_or_default();
        let black = pair.get(1).cloned().unwrap_or_default();
        let line = format!("{:>3}. {:<18} {}", idx + 1, white, black);
        items.push(ListItem::new(line));
    }

    if items.is_empty() {
        items.push(ListItem::new("No moves yet"));
    }

    items
}

fn execute_command(state: &mut State, command: &str) {
    let trimmed = command.trim();

    if trimmed.is_empty() {
        return;
    }

    match trimmed {
        "u" => {
            if state.ply_counter > 0 {
                undo_move!(state);
            } else {
                log_2!("Nothing to undo");
            }
        }
        "r" => {
            while state.ply_counter > 0 {
                undo_move!(state);
            }
        }
        _ if trimmed.starts_with("fen ") => {
            let fen = trimmed[4..].trim();
            state.load_fen(fen);
            log_2!("Loaded FEN");
        }
        _ if trimmed.starts_with("search ") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: search [depth]");
                return;
            }

            let depth = parts[1].parse::<usize>().unwrap_or_else(|_| {
                log_2!("Invalid depth: {}", parts[1]);
                0
            });

            let mut info = SearchInfo::default();
            info.set_depth = depth;
            search_position(state, &mut info);
        }
        _ if trimmed.starts_with("go ") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: go [depth]");
                return;
            }

            let depth = parts[1].parse::<usize>().unwrap_or_else(|_| {
                log_2!("Invalid depth: {}", parts[1]);
                0
            });

            let mut info = SearchInfo::default();
            info.set_depth = depth;
            let result = search_position(state, &mut info);

            if result.best_move == null_move() {
                log_2!("No legal move available");
                return;
            }

            log_1!(
                "Best Move: {} | Score: {} | Nodes: {} | Time: {}",
                format_move(&result.best_move, state),
                result.best_score,
                result.total_nodes,
                format_time(result.total_elapsed)
            );

            make_move!(state, result.best_move);
        }
        _ => {
            if let Some(mv) = parse_move(trimmed, state) {
                make_move!(state, mv);
            } else {
                log_2!("Invalid command or move: {}", trimmed);
            }
        }
    }
}

fn handle_key(app: &mut TuiApp, state: &mut State, key: KeyCode) -> bool {
    if app.show_help {
        match key {
            KeyCode::Char('?') | KeyCode::Esc => {
                app.show_help = false;
            }
            _ => {}
        }

        return false;
    }

    match key {
        KeyCode::Char('?') => {
            app.show_help = true;
        }
        KeyCode::Tab => {
            app.tab_idx = (app.tab_idx + 1) % TAB_TITLES.len();
            app.detail_scroll = 0;
        }
        KeyCode::Char('q') if app.input.is_empty() => return true,
        KeyCode::Esc => {
            app.input.clear();
        }
        KeyCode::Up => {
            if app.tab_idx == 0 {
                app.log_scroll = app.log_scroll.saturating_sub(1);
            } else {
                app.detail_scroll = app.detail_scroll.saturating_sub(1);
            }
        }
        KeyCode::Down => {
            if app.tab_idx == 0 {
                app.log_scroll = app.log_scroll.saturating_add(1);
            } else {
                app.detail_scroll = app.detail_scroll.saturating_add(1);
            }
        }
        KeyCode::Backspace => {
            app.input.pop();
        }
        KeyCode::Enter => {
            let command = app.input.clone();
            app.input.clear();
            execute_command(state, &command);
        }
        KeyCode::Char(c) => {
            app.input.push(c);
        }
        _ => {}
    }

    false
}

pub fn run_tui(state: &mut State) {
    if let Err(error) = run_tui_inner(state) {
        eprintln!("TUI terminated with error: {}", error);
    }
}

fn run_tui_inner(state: &mut State) -> Result<(), Box<dyn std::error::Error>> {
    enable_raw_mode()?;

    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut app = TuiApp::new();

    log_2!("TUI started");
    log_2!("Commands: move, u, r, fen <fen>, search <d>, go <d>");

    loop {
        app.push_logs(take_log_messages());

        terminal.draw(|frame| {
            render(frame, state, &app);
        })?;

        if event::poll(Duration::from_millis(80))? {
            if let Event::Key(key_event) = event::read()? {
                if key_event.kind == KeyEventKind::Press {
                    let quit = handle_key(&mut app, state, key_event.code);
                    if quit {
                        break;
                    }
                }
            }
        }
    }

    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    Ok(())
}
