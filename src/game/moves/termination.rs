//! termination.rs
//!
//! Detects terminal positions and computes their game outcomes.
//!
//! Two tiers of terminal live here. `position_terminal` gathers the eager,
//! position-local rules checked after every move (N-check, goal, extinction,
//! double-pass adjudication, the counters); `game_outcome` adds the on-demand
//! repetition and perpetual verdicts, which are computed from history rather
//! than stored. The rule table these consult is `Termination` in
//! representations/termination.rs.
//!
//! Created: 27/07/2026
//! Author : Alden Luthfi

use crate::*;

/// side_is_bare
///
/// Whether a colour has been reduced to only royal pieces (a lone king in the
/// makruk family). Scans `piece_count` for any non-royal of that colour.
///
/// Params:
/// - state: &State -> position to inspect
/// - side : u8     -> the colour tested
///
/// Return:
/// bool -> true when the colour has no non-royal pieces left
pub fn side_is_bare(state: &State, side: u8) -> bool {
    for (index, piece) in state.statics.pieces.iter().enumerate() {
        if p_color!(piece) == side
            && !p_is_royal!(piece)
            && state.piece_count[index] > 0
        {
            return false;
        }
    }
    true
}

/// counting_limit
///
/// The pieces'-honour move budget for the material side of a bare-king endgame:
/// the first `Counting` table row whose requirements the `winner` meets, else
/// the table default. A requirement is met when the winner owns at least its
/// minimum number of the requirement's piece set. Returns 0 if the variant
/// declares no counting rule (never reached, since the caller gates on it).
///
/// Params:
/// - state : &State -> position to inspect
/// - winner: u8     -> the colour holding material (opponent of the bare king)
///
/// Return:
/// u16 -> the frozen count limit for this material
pub fn counting_limit(state: &State, winner: u8) -> u16 {
    let Some(counting) = state.statics.termination.counting.as_ref() else {
        return 0;
    };

    for (requirements, limit) in &counting.table {
        let met = requirements.iter().all(|(set, minimum)| {
            let owned = (0..state.statics.pieces.len())
                .filter(|&index| set[index]
                    && p_color!(state.statics.pieces[index]) == winner)
                .map(|index| state.piece_count[index])
                .sum::<u32>();
            owned >= *minimum
        });
        if met {
            return *limit;
        }
    }

    counting.default
}

/// extinct_outcome
///
/// Detects a material-extinction terminal. Only a capturing move can drop a
/// colour's set-count, so the scan runs only after a capture; for each colour
/// it sums `piece_count` over the rule's set pieces of that colour and fires
/// when the count is at or below the threshold, naming the extinct colour (or
/// its opponent, when the rule is opponent-facing).
///
/// Params:
/// - state    : &State -> position after the move
/// - move_type: u128   -> the applied move's type tag
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) when it fires
pub fn extinct_outcome(
    state: &State, move_type: u128,
) -> Option<(u8, Outcome, &str)> {
    if state.statics.termination.extinct.is_empty() {
        return None;
    }

    if move_type != SINGLE_CAPTURE_MOVE && move_type != MULTI_CAPTURE_MOVE {
        return None;
    }

    for extinct in &state.statics.termination.extinct {
        for color in [WHITE, BLACK] {
            let mut count = 0u32;
            for (index, piece) in state.statics.pieces.iter().enumerate() {
                if extinct.set[index] && p_color!(piece) == color {
                    count += state.piece_count[index];
                }
            }

            if count <= extinct.threshold as u32 {
                let subject =
                    if extinct.opponent { 1 - color } else { color };
                return Some((subject, extinct.outcome, &extinct.name));
            }
        }
    }

    None
}

/// goal_outcome
///
/// Detects a goal-zone terminal for the side that just moved: if any of the
/// mover's goal-set pieces stands on a zone square, the mover receives the
/// rule's outcome. Because the check runs after every move, a piece can only
/// be found on the zone the move that placed it there.
///
/// Params:
/// - state: &State -> position after the move
/// - mover: u8     -> the colour that just moved
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (mover, outcome, name) when a goal is reached
pub fn goal_outcome(state: &State, mover: u8) -> Option<(u8, Outcome, &str)> {
    let goal = state.statics.termination.goal.as_ref()?;

    for (index, piece) in state.statics.pieces.iter().enumerate() {
        if goal.set[index] && p_color!(piece) == mover {
            for &square in piece_squares!(state, index) {
                if get!(goal.zone, square as u32) {
                    return Some((mover, goal.outcome, &goal.name));
                }
            }
        }
    }

    None
}

/// adjudicate_outcome
///
/// Decides a passed-out position by weighted material. Each colour's counted
/// pieces are multiplied by their configured point weight and summed with that
/// colour's standing handicap; the greater sum wins and an equal sum draws.
/// Returns None when the variant declares no `adjudicate` rule, so the caller
/// keeps the neutral double-pass draw.
///
/// Params:
/// - state: &State -> position at the second successive pass
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (winner, Win, name) / (mover, Draw, name)
pub fn adjudicate_outcome(state: &State) -> Option<(u8, Outcome, &str)> {
    let adjudicate = state.statics.termination.adjudicate.as_ref()?;

    let mut sums = adjudicate.handicap;
    for (index, &count) in state.piece_count.iter().enumerate() {
        let color = p_color!(state.statics.pieces[index]) as usize;
        sums[color] += adjudicate.weights[index] * count as i32;
    }

    let (winner, outcome) = if sums[WHITE as usize] > sums[BLACK as usize] {
        (WHITE, Outcome::Win)
    } else if sums[BLACK as usize] > sums[WHITE as usize] {
        (BLACK, Outcome::Win)
    } else {
        (state.playing, Outcome::Draw)
    };

    Some((winner, outcome, &adjudicate.name))
}

/// position_terminal
///
/// The eager, position-local terminal check for the last move in history: the
/// first of the N-check, goal, extinction, double-pass, counting, and counter
/// rules to fire, with the name of the rule and the colour its outcome is
/// scored against. Repetition and perpetual are not here -- `game_outcome`
/// computes them on demand. `None` when no move has been made.
///
/// Params:
/// - state: &State -> position with the ending move on top of history
///
/// Return:
/// Option<(u8, Outcome, &str)> -> (subject colour, outcome, name) when firing
pub fn position_terminal(state: &State) -> Option<(u8, Outcome, &str)> {
    let last = state.history.last()?;
    let move_type = move_type!(last.move_ply);
    let double_pass = pass_snapshot!(last)
        && state.history.len() >= 2
        && pass_snapshot!(state.history[state.history.len() - 2]);
    let mover = 1 - state.playing;

    if let Some(checks) = &state.statics.termination.checks
        && state.gave_check
        && state.check_count[mover as usize] >= checks.count
    {
        Some((mover, checks.outcome, &checks.name))
    } else if let Some(hit) = goal_outcome(state, mover) {
        Some(hit)
    } else if let Some(hit) = extinct_outcome(state, move_type) {
        Some(hit)
    } else if double_pass {
        Some(
            adjudicate_outcome(state)
                .unwrap_or((state.playing, Outcome::Draw, "")),
        )
    } else if let Some((count, limit)) = state.counting
        && count >= limit
        && let Some(counting) = &state.statics.termination.counting
    {
        Some((state.playing, counting.outcome, &counting.name))
    } else if let Some(counter) = &state.statics.termination.counter
        && state.halfmove_clock >= counter.limit
    {
        Some((state.playing, counter.outcome, &counter.name))
    } else {
        None
    }
}

/// offence_set
///
/// The unified per-ply offence a mover commits against the side to move after
/// its move: it delivers a **check** (the quarry's royal is attacked -- a full
/// board test, so discovered checks count) and/or **chases** enemy non-royal
/// pieces (attacked by a non-exempt mover piece and left undefended). A check
/// is just the royal case of the same "attacked and unprotected" predicate, so
/// check and chase share one read-only pass -- no SEE, no lva, no make/undo.
/// The chase board is empty unless the variant declares a perpetual-chase rule.
///
/// Params:
/// - state: &State -> position after the mover's ply (quarry is side to move)
/// - mover: u8     -> the colour that just moved (the potential offender)
///
/// Return:
/// (bool, Board) -> (mover gave check, enemy squares under an undefended chase)
pub fn offence_set(state: &State, mover: u8) -> (bool, Board) {
    let quarry = 1 - mover;                                                     /* side to move after the mover's ply */
    let did_check = is_in_check!(quarry, state);

    let mut chase = board!(state.statics.files, state.statics.ranks);

    let Some(perpetual) = state.statics.termination.perpetual.as_ref()
    else {
        return (did_check, chase);
    };
    if perpetual.chase.is_none() {
        return (did_check, chase);
    }
    let chasers = &perpetual.chasers;

    for index in 0..state.statics.pieces.len() {
        let piece = &state.statics.pieces[index];
        if p_color!(piece) != quarry || p_is_royal!(piece) {
            continue;
        }

        let target_rank = p_rank!(piece);

        for &square in piece_squares!(state, index) {
            let unmoved = get!(state.virgin_board, square as u32);

            let attackers = &state.statics.relevant_attacks
                [quarry as usize][square as usize];
            let attacked = attackers.iter().any(
                |(piece_index, start, move_vector)| {
                    chasers[*piece_index as usize]
                        && state.main_board[*start as usize] == *piece_index
                        && validate_attack_vector!(
                            move_vector,
                            *start,
                            &state.statics.pieces[*piece_index as usize],
                            unmoved,
                            false,
                            target_rank,
                            square as u32,
                            state
                        )
                }
            );

            if attacked && !is_square_attacked!(
                square as u32, mover, unmoved, false, target_rank, state
            ) {
                set!(chase, square as u32);
            }
        }
    }

    (did_check, chase)
}

/// perpetual_offender
///
/// Adjudicates a just-closed repetition cycle for a sole aggressor. Walking the
/// plies from the previous occurrence of the current position up to the closing
/// move, each colour's offences accumulate: it is a perpetual checker if it
/// gave check on every one of its cycle moves, and a perpetual chaser if it kept
/// the same enemy piece under an undefended chase on every one of them. Because
/// the chased piece dodges square to square, the chase board is tracked by
/// identity -- remapped through each undone quiet cycle move (a cycle move is
/// capture- and drop-free, hence a pure relocation). Check outranks chase; when
/// both colours commit the same offence no aggressor is sole. The walk uses
/// `undo_move!`/`make_move!` and restores the position exactly, including
/// `game_result` (a redone quiet move could otherwise trip an eager terminal).
///
/// Params:
/// - state: &mut State -> position after the cycle-closing move (restored)
///
/// Return:
/// Option<u8> -> the sole offender's colour, or None when none / both offend
fn perpetual_offender(state: &mut State) -> Option<u8> {
    let (check_enabled, chase_enabled) = {
        let perpetual = state.statics.termination.perpetual.as_ref()?;
        (perpetual.check.is_some(), perpetual.chase.is_some())
    };

    let hash = state.position_hash;
    let plies = state.history.len();
    let start = (0..plies).rev()
        .find(|&index| state.history[index].position_hash == hash)?;

    let saved_result = state.game_result;

    let mut check_all = [true; 2];
    let mut cycle_plies = [0u32; 2];
    let mut chase = [
        board!(state.statics.files, state.statics.ranks),
        board!(state.statics.files, state.statics.ranks),
    ];
    let mut chase_seen = [false; 2];
    let mut redo: Vec<Move> = Vec::new();
    let mut index = plies - 1;

    loop {
        let mover = (1 - state.playing) as usize;                               /* colour that made history[index]    */
        let (did_check, threats) = offence_set(state, mover as u8);
        cycle_plies[mover] += 1;
        check_all[mover] &= did_check;
        if chase_seen[mover] {
            and!(chase[mover], &threats);
        } else {
            chase[mover] = threats;
            chase_seen[mover] = true;
        }

        if index == start {
            break;
        }

        let cycle_move = state.history[index].move_ply.clone();
        undo_move!(state);

        let from = start!(cycle_move) as u32;                                   /* remap the quarry piece backward    */
        let to = end!(cycle_move) as u32;                                       /* through this quiet relocation      */
        let other = 1 - mover;
        if get!(chase[other], to) {
            clear!(chase[other], to);
            set!(chase[other], from);
        }

        redo.push(cycle_move);
        index -= 1;
    }

    for cycle_move in redo.iter().rev() {
        make_move!(state, cycle_move.clone());
    }
    state.game_result = saved_result;

    let checker =
        |c: usize| check_enabled && cycle_plies[c] > 0 && check_all[c];
    let chaser =
        |c: usize| chase_enabled && chase_seen[c] && !is_empty!(chase[c]);

    match (checker(WHITE as usize), checker(BLACK as usize)) {
        (true, false) => return Some(WHITE),
        (false, true) => return Some(BLACK),
        (true, true) => return None,                                            /* both check: repetition stands      */
        (false, false) => {}
    }

    match (chaser(WHITE as usize), chaser(BLACK as usize)) {
        (true, false) => Some(WHITE),
        (false, true) => Some(BLACK),
        _ => None,
    }
}

/// repetition_outcome
///
/// The on-demand repetition/perpetual terminal, computed rather than stored:
/// `None` unless the variant declares a `repetition` rule and the current
/// position has occurred at least `min_count` times. Otherwise the outcome is
/// from the side-to-move perspective -- the neutral repetition outcome unless a
/// sole perpetual offender is found, in which case that colour loses. The bool
/// is true when a perpetual offender decided it (for reason reporting).
///
/// Params:
/// - state    : &mut State -> current position (restored if a walk runs)
/// - min_count: u8         -> occurrences required before it fires
///
/// Return:
/// Option<(Outcome, bool)> -> (outcome, perpetual decided it) when it fires
pub fn repetition_outcome(
    state: &mut State, min_count: u8,
) -> Option<(Outcome, bool)> {
    let neutral = state.statics.termination.repetition.as_ref()?.outcome;

    let occurrences = state.position_hash_map
        .get(&state.position_hash).copied().unwrap_or(0);
    if occurrences < min_count {
        return None;
    }

    if state.statics.termination.perpetual.is_none() {
        return Some((neutral, false));
    }

    Some(match perpetual_offender(state) {
        Some(offender) if offender == state.playing => (Outcome::Loss, true),
        Some(_) => (Outcome::Win, true),
        None => (neutral, false),
    })
}

/// terminal_reason
///
/// The name of the eager rule that ended the current position, recomputed so no
/// reason need be stored. `None` when no eager rule applies (e.g. a
/// search-injected mate) or the rule is unnamed.
///
/// Params:
/// - state: &State -> position after the ending move
///
/// Return:
/// Option<String> -> the fired rule's name, if any
pub fn terminal_reason(state: &State) -> Option<String> {
    position_terminal(state)
        .map(|(_, _, name)| name)
        .filter(|name| !name.is_empty())
        .map(str::to_string)
}

/// game_outcome
///
/// The single game-truth oracle for reporting and self-play paths: the eager,
/// position-local `game_result` when set, else the on-demand repetition /
/// perpetual verdict. Also reports the reason name of whichever rule decided
/// it. Search does not use this; it keeps the cheap `is_terminal!` read.
///
/// Params:
/// - state: &mut State -> current position (restored if a walk runs)
///
/// Return:
/// (u8, Option<String>) -> (result, reason name) with result ONGOING when live
pub fn game_outcome(state: &mut State) -> (u8, Option<String>) {
    if state.game_result != ONGOING {
        return (state.game_result, terminal_reason(state));
    }

    let Some(occurrences) = state.statics.termination.repetition
        .as_ref().map(|repetition| repetition.occurrences)
    else {
        return (ONGOING, None);
    };

    match repetition_outcome(state, occurrences) {
        Some((outcome, perpetual)) => {
            let rule = if perpetual {
                state.statics.termination.perpetual.as_ref().map(|p| &p.name)
            } else {
                state.statics.termination.repetition.as_ref().map(|r| &r.name)
            };
            (resolve_outcome!(state.playing, outcome), rule.cloned())
        }
        None => (ONGOING, None),
    }
}
