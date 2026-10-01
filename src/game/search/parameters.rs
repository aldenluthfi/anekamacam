//! parameters.rs
//!
//! Derives the evaluation and search parameters from the variant rules.
//!
//! The engine sees each army for the first time at load, so it cannot have
//! hand-tuned values. This file converts the movement of each piece into
//! the numbers of the evaluation: a value from its reach and mobility, a
//! role from its value rank, and piece-square tables for the board.
//!
//! All code here runs once, at load. The search reads the results at each
//! node and never calculates them again.
//!
//! Created: 08/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              DERIVATION CONSTANTS
\*----------------------------------------------------------------------------*/

/// Board occupancy
///
/// The part of the squares that a slider finds blocked in each phase. This
/// makes the opening value different from the endgame value.
///
/// - `OPENING_OCCUPANCY` : 36% blocked, a slider stops early
/// - `ENDGAME_OCCUPANCY` : 12% blocked, the lines are open
///
const OPENING_OCCUPANCY: u32 = 360;
const ENDGAME_OCCUPANCY: u32 = 120;

/// USUAL_CONDITION_CHANCE
///
/// The minimum chance, at the opening occupancy, that the CPMN condition of
/// a vector matches for the vector to be a usual move. Only usual moves
/// shape the geometry of a piece: reach, offsets and the pawn terms. At
/// 50%, a move is usual when it is playable in most positions.
///
const USUAL_CONDITION_CHANCE: u32 = 500;

/// PST amplitude
///
/// The maximum difference between the best and worst square of a piece
/// square table, as a part of the piece value. A fixed value is too large
/// for a pawn and too small for a taikyoku great general.
///
/// - `PST_AMPLITUDE_RATIO` : 2.6% of the piece value, as for a chess queen
/// - `PST_AMPLITUDE_FLOOR` : 24, the minimum
///
/// Notes:
/// The floor applies below a value of about 923. Thus only the most
/// valuable pieces get a wider table.
///
const PST_AMPLITUDE_RATIO: u32 = 26;
const PST_AMPLITUDE_FLOOR: f64 = 24.0;

/// Role splits
///
/// The role splits of the army, sorted by value. Royals are not ranked.
///
/// ```text
/// non-big         minor             major
/// ├───│───────────────────────────│──────┤
///    10%                         80%   100%
/// ```
///
/// - `ROLE_NON_BIG_SPLIT` : the cheapest 10%, not big
/// - `ROLE_MAJOR_SPLIT`   : the most valuable 20%, major
///
/// The two are parts of the army, over `COEFFICIENT_SCALE`.
///
const ROLE_NON_BIG_SPLIT: u32 = 100;
const ROLE_MAJOR_SPLIT: u32 = 200;

/// ENDGAME_ARMY_SIZE
///
/// The size of the big army, in pieces of mean value, where the endgame
/// starts. The phase taper uses the real army of each variant:
///
/// - opening : all big pieces of the setup, at mean value
/// - endgame : `ENDGAME_ARMY_SIZE` pieces, or one less than the opening
///
const ENDGAME_ARMY_SIZE: u32 = 5;

/// Draw contempt
///
/// The draw value for a side with more material. The draw is below zero
/// for that side. The cost increases with the lead up to
/// `DRAW_CONTEMPT_SPAN` mean pieces, where it is `DRAW_CONTEMPT_RATIO` of
/// one mean piece.
///
/// - no lead   : zero
/// - one piece : half of the contempt
/// - two       : the full contempt, 12.5% of a mean piece
/// - more      : the same, the cost stops increasing
///
/// The two use the mean piece of the army, not the most valuable piece.
///
const DRAW_CONTEMPT_RATIO: u32 = 125;
const DRAW_CONTEMPT_SPAN: u32 = 2;

/// Late move reduction curves
///
/// Late move reduction curves, one for each move class. Each surface is
/// `base + shape(depth, moves) / divisor`, over `COEFFICIENT_SCALE`:
///
/// ```text
/// class            base   shape                       divisor
/// quiet             750   ln(depth) * ln(moves)          2250
/// quiet check      1000   sqrt(depth) * ln(moves)        4000
/// tactical         1000   ln(depth) * sqrt(moves)        4000
/// tactical check      0   ln(depth) * ln(moves)          4500
/// ```
///
/// Each row has a `_BASE` and a `_DIVISOR` constant. Tactical moves are
/// captures, promotions and drops. "Check" means the parent node is in
/// check, not that the move gives check.
///
/// Notes:
/// These shapes and values are fixed for all variants. They are not
/// derived from the rules.
///
const REDUCTION_QUIET_BASE: u32 = 750;
const REDUCTION_QUIET_DIVISOR: u32 = 2250;
const REDUCTION_QUIET_CHECK_BASE: u32 = 1000;
const REDUCTION_QUIET_CHECK_DIVISOR: u32 = 4000;
const REDUCTION_TACTICAL_BASE: u32 = 1000;
const REDUCTION_TACTICAL_DIVISOR: u32 = 4000;
const REDUCTION_TACTICAL_CHECK_BASE: u32 = 0;
const REDUCTION_TACTICAL_CHECK_DIVISOR: u32 = 4500;

/// ASPIRATION_RATIO
///
/// The start half-width of the aspiration window, 3% of the most valuable
/// piece, over `COEFFICIENT_SCALE`. The search widens only the failed
/// side, by `ASPIRATION_WIDEN`, up to `ASPIRATION_CLAMP` times this width.
///
const ASPIRATION_RATIO: u32 = 30;

/// Reverse futility margin
///
/// The reverse futility margin. The static evaluation must be this much
/// above beta to cut. An improving side needs less. The two are over
/// `COEFFICIENT_SCALE`.
///
/// - not improving : 11% of the most valuable piece for each ply
/// - improving     : 75% of that margin
///
const RFP_RATIO: u32 = 110;
const RFP_IMPROVING: u32 = 750;

/// Futility margin
///
/// The futility margin. A node further below alpha than this skips its
/// late quiet moves. A side that is not improving gets a smaller margin.
/// All three are over `COEFFICIENT_SCALE`.
///
/// - improving     : 10% of the most valuable piece, plus 13% for each ply
/// - not improving : 70% of that margin
///
const FUTILITY_FLOOR: u32 = 100;
const FUTILITY_RATIO: u32 = 130;
const FUTILITY_IMPROVING: u32 = 700;

/// Late move pruning count
///
/// The late move pruning count. After this number of moves, a node skips
/// the other quiet moves. `LMP_RATIO` and `LMP_IMPROVING` are over
/// `COEFFICIENT_SCALE`.
///
/// - improving     : 3 moves, plus the square of the remaining depth
/// - not improving : 55% of that count, minimum one move
///
const LMP_BASE: u32 = 3;
const LMP_RATIO: u32 = 1000;
const LMP_IMPROVING: u32 = 550;

/// SEE_PRUNE_RATIO
///
/// The maximum exchange loss of a capture that the search still searches:
/// 25% of the most valuable piece for each ply, over `COEFFICIENT_SCALE`.
/// The prune reads the exchange score of the move ordering.
///
const SEE_PRUNE_RATIO: u32 = 250;

/// QSEARCH_DELTA_RATIO
///
/// The quiescence delta margin: 10% of the most valuable piece, over
/// `COEFFICIENT_SCALE`. A capture whose victim plus this margin is below
/// alpha is skipped. The margin does not apply in the endgame.
///
const QSEARCH_DELTA_RATIO: u32 = 100;

/// Shelter ring
///
/// The ring of a royal is all squares within `SHELTER_RADIUS` steps on the
/// two axes.
///
/// ```text
/// s s s     s   ahead: a shield piece here is shelter, and a guard
/// g K g     g   beside or behind: any own piece here is a guard
/// g g g     K   the royal, radius 1 gives eight ring squares
/// ```
///
/// - `SHELTER_RADIUS` : 1, the ring radius
/// - `SHELTER_RATIO`  : 1.2% of the most valuable piece for each shield
/// - `SHELTER_FLOOR`  : 4, the minimum shelter value
///
/// `SHELTER_CAP` limits the counted shield pieces.
///
const SHELTER_RADIUS: u32 = 1;
const SHELTER_RATIO: u32 = 12;
const SHELTER_FLOOR: u32 = 4;

/// Guard value
///
/// The value of each own piece on the ring, of any type and on any side.
/// It is half the shelter value, because a piece beside or behind the
/// royal blocks fewer attacks. The ring limits the count.
///
/// - `GUARD_RATIO` : 0.6% of the most valuable piece
/// - `GUARD_FLOOR` : 2, the minimum guard value
///
const GUARD_RATIO: u32 = 6;
const GUARD_FLOOR: u32 = 2;

/// Castling values
///
/// The castling values, for variants with castling. The two are parts of
/// the most valuable piece, over `COEFFICIENT_SCALE`. Thus castling is the
/// best choice.
///
/// - `CASTLED_RATIO`        : 4% of the most valuable piece
/// - `CASTLING_RIGHT_RATIO` : 2%, half, for a kept right
/// - spent rights           : nothing
///
const CASTLED_RATIO: u32 = 40;
const CASTLING_RIGHT_RATIO: u32 = 20;

/// King danger
///
/// The cost of an attacked royal zone. The pressure is the expected number
/// of enemy moves onto the royal square or ring. The cost is the square of
/// the pressure, so attackers compound. At `ZONE_ATTACK_FULL` moves, the
/// cost is `DANGER_RATIO` of the most valuable piece.
///
/// - 4 moves  : 1/16 of the cost
/// - 8 moves  : 1/4 of the cost
/// - 16 moves : the full cost, 60% of the most valuable piece
/// - more     : up to the cap, 100% of the most valuable piece
///
/// Notes:
/// The cap stops the square from growing too much on large boards. An
/// attack is pressure, not a mate.
///
const DANGER_RATIO: u32 = 600;
const DANGER_CAP_RATIO: u32 = 1000;

/// Royal proximity
///
/// The cost of each enemy piece within two files and two ranks of a royal,
/// in a variant with drops. A piece in hand can join such a piece at once,
/// so a few attackers near the royal make a mate. Over `COEFFICIENT_SCALE`
/// of the most valuable piece:
///
/// - with drops    : 10% for each enemy piece near a royal
/// - without drops : nothing
///
const PROXIMITY_RATIO: u32 = 100;

/// Check race
///
/// The worth of the checks that a colour has given, in a variant with a
/// `checks` rule that wins. Over `COEFFICIENT_SCALE` of the most valuable
/// piece:
///
/// - one check left to win : 100%, the next check wins
/// - each check more       : half of the one before
///
const CHECK_RATIO: u32 = 1000;

/// Open shield penalty
///
/// The penalty for a royal with no own shield piece in front of it, on its
/// file or the two files next to it. Over `COEFFICIENT_SCALE`.
///
/// - covered   : a shield piece in front on those files, no penalty
/// - uncovered : 3.3% of the most valuable piece, minimum 12
///
const OPEN_SHIELD_RATIO: u32 = 33;
const OPEN_SHIELD_FLOOR: u32 = 12;

/// Tempo
///
/// The value of the move for the side to move. The other terms are the
/// same for the two sides. Without a tempo, a position and its mirror have
/// the same score.
///
/// - `TEMPO_RATIO` : 2.4% of the most valuable piece
/// - `TEMPO_FLOOR` : 5, the minimum
///
const TEMPO_RATIO: u32 = 24;
const TEMPO_FLOOR: u32 = 5;

/// Material imbalance
///
/// The value of a surplus piece count, in addition to the material value.
/// A heavy surplus is worth two times a light surplus.
///
/// - `IMBALANCE_MAJOR_RATIO` : 2% of the most valuable piece, each major
/// - `IMBALANCE_MAJOR_FLOOR` : 3, the minimum
/// - `IMBALANCE_MINOR_RATIO` : 1%, each minor
/// - `IMBALANCE_MINOR_FLOOR` : 1, the minimum
///
const IMBALANCE_MAJOR_RATIO: u32 = 20;
const IMBALANCE_MAJOR_FLOOR: u32 = 3;
const IMBALANCE_MINOR_RATIO: u32 = 10;
const IMBALANCE_MINOR_FLOOR: u32 = 1;

/// Half-board pair
///
/// The value of two pieces of a type that reaches only half the board. The
/// second piece covers the other half. The test is on the mean reach, not
/// on the name:
///
/// - 0.48 to 0.52 : half the board, the pair gets the bonus
/// - above        : reaches the full board, no bonus
/// - below        : reaches less, no bonus
///
/// - `PAIR_RATIO`       : 6% of the most valuable piece
/// - `PAIR_FLOOR`       : 10, the minimum
/// - `PAIR_REACH`       : 0.5, half the board
/// - `PAIR_REACH_SLACK` : 0.02, the tolerance
///
/// Royals are not tested.
///
const PAIR_RATIO: u32 = 60;
const PAIR_FLOOR: u32 = 10;
const PAIR_REACH: f64 = 0.5;
const PAIR_REACH_SLACK: f64 = 0.02;

/// PAWN_MIN_START_COUNT
///
/// The minimum start count of a piece type to be a pawn. The other pawn
/// tests are geometric: no backward step or capture, a quiet one-step
/// forward move, and no move longer than one square after the first move.
///
/// Notes:
/// Without the count, the minishogi pawn and the two minixiangqi soldiers
/// would be pawns. Pawn structure needs a rank of pawns.
///
const PAWN_MIN_START_COUNT: usize = 5;

/// Pawn structure values
///
/// Pawn structure values, as parts of the pawn value over
/// `COEFFICIENT_SCALE`. Other derived values use the most valuable piece.
/// These use the pawn, because each is a correction of one pawn value.
///
/// - `PAWN_CONNECTED_OPENING_RATIO` : +20% of the opening pawn value
/// - `PAWN_CONNECTED_ENDGAME_RATIO` : +35% of the endgame pawn value
/// - `PAWN_DOUBLED_RATIO`           : -25%, the same in the two phases
/// - `PAWN_ISOLATED_RATIO`          : -25%, the same in the two phases
/// - `PAWN_BACKWARD_RATIO`          : -17.5%, the same in the two phases
///
const PAWN_CONNECTED_OPENING_RATIO: u32 = 200;
const PAWN_CONNECTED_ENDGAME_RATIO: u32 = 350;
const PAWN_DOUBLED_RATIO: u32 = 250;
const PAWN_ISOLATED_RATIO: u32 = 250;
const PAWN_BACKWARD_RATIO: u32 = 175;

/// Passed pawn values
///
/// Passed pawn values, as parts of the promotion gain over
/// `COEFFICIENT_SCALE`. The gain is the most valuable promotion minus the
/// pawn value. The progress of the pawn scales the value.
///
/// - `PASSED_OPENING_RATIO`    : 10% of the gain
/// - `PASSED_ENDGAME_RATIO`    : 35% of the gain
/// - `PASSED_UNPROMOTED_RATIO` : 40% of the pawn value, if it cannot promote
///
const PASSED_OPENING_RATIO: u32 = 100;
const PASSED_ENDGAME_RATIO: u32 = 350;
const PASSED_UNPROMOTED_RATIO: u32 = 400;

/// SHELTER_CONFINEMENT_DIVISOR
///
/// A royal that can reach a quarter of the board or less is in a palace.
/// Then the shelter term is off for that colour. A palace royal cannot
/// collect own pieces in front of it without a forward walk, and such
/// variants punish that walk.
///
const SHELTER_CONFINEMENT_DIVISOR: usize = 4;

/// Setup walk limits
///
/// Limits of the setup walk at derive time. If the placement tree is
/// larger, the derivation uses the setups that it already found.
///
/// - `SETUP_STATE_CAP`  : 4096, maximum different piece counts to expand
/// - `SETUP_ENDING_CAP` : 256, maximum completed setups to average
///
const SETUP_STATE_CAP: usize = 4096;
const SETUP_ENDING_CAP: usize = 256;

/*----------------------------------------------------------------------------*\
                            DERIVED PARAMETER TABLES
\*----------------------------------------------------------------------------*/

/// PieceRoles
///
/// One derived role: `(piece index, is_big, is_major)`. A piece that is
/// big but not major is minor.
///
type PieceRoles = (PieceIndex, bool, bool);

/// EvalParams
///
/// The derived evaluation tables: shelter, danger, pawn, imbalance and
/// contempt. This file derives them once for each variant, and
/// `evaluation.rs` reads them. Each field has a `Default`.
///
/// Notes:
/// Make and undo read `pst_opening`, `pst_endgame`, `opening_score` and
/// `endgame_score`. These stay on `StaticState`, one access shorter.
///
#[derive(Default)]
pub struct EvalParams {
    pub shield_pieces: Vec<bool>,                                               /* piece index to shield-like role    */
    pub shelter_squares: [Vec<Square>; 2],                                      /* color to forward local squares     */
    pub shelter_counts: [Vec<u8>; 2],                                           /* squares stored per origin above    */
    pub ring_squares: Vec<Square>,                                              /* every local square, colour-blind   */
    pub ring_counts: Vec<u8>,                                                   /* squares stored per origin above    */
    pub local_stride: usize,                                                    /* slots each origin owns             */
    pub forward_steps: [i32; 2],                                                /* rank step each colour advances by  */
    pub shelter_value: i32,                                                     /* worth of one sheltering piece      */
    pub guard_value: i32,                                                       /* worth of one piece beside a royal  */
    pub castled_value: i32,                                                     /* worth of having castled already    */
    pub castling_right_value: i32,                                              /* worth of still being able to       */

    pub zone_attack: Vec<u8>,                                                   /* royal, piece, origin to pressure   */
    pub zone_attack_best: Vec<u8>,                                              /* pressure from its dearest origin   */
    pub king_danger_scale: i32,                                                 /* worth of a fully pressed zone      */
    pub king_danger_cap: i32,                                                   /* most a pressed zone may ever cost  */
    pub open_shield_penalty: i32,                                               /* cost of a royal nothing covers     */
    pub proximity_value: i32,                                                   /* cost of an enemy near a royal      */

    pub pawn_slots: Vec<usize>,                                                 /* piece index to pawn slot, or NONE  */
    pub pawn_pieces: Vec<usize>,                                                /* pawn slot to piece index           */
    pub pawn_stride: usize,                                                     /* squares each pawn slot owns        */
    pub pawn_path: Vec<Board>,                                                  /* slot, square to advance squares    */
    pub pawn_interference: Vec<Board>,                                          /* slot, square to passer stoppers    */
    pub pawn_support: Vec<Board>,                                               /* slot, square to defending squares  */
    pub pawn_backward: Vec<Board>,                                              /* slot, square to stop attackers     */
    pub pawn_support_files: Vec<Vec<i32>>,                                      /* slot to supporting file offsets    */
    pub pawn_passed_opening: Vec<i32>,                                          /* slot, square to passer worth,      */
    pub pawn_passed_endgame: Vec<i32>,                                          /* opening then ending                */
    pub pawn_connected_opening: Vec<i32>,                                       /* slot to worth of being defended,   */
    pub pawn_connected_endgame: Vec<i32>,                                       /* opening then ending                */
    pub pawn_doubled_penalty: Vec<i32>,                                         /* slot to cost of blocking itself    */
    pub pawn_isolated_penalty: Vec<i32>,                                        /* slot to cost of standing alone     */
    pub pawn_backward_penalty: Vec<i32>,                                        /* slot to cost of a contested stop   */

    pub tempo_bonus: i32,                                                       /* worth of holding the move          */
    pub imbalance_major: i32,                                                   /* worth of one heavy piece of extra  */
    pub imbalance_minor: i32,                                                   /* worth of one light piece of extra  */
    pub pair_pieces: Vec<usize>,                                                /* pieces a second copy completes     */
    pub pair_bonus: i32,                                                        /* worth of completing such a pair    */

    pub draw_contempt: i32,                                                     /* a draw's cost one span ahead       */
    pub draw_span: i32,                                                         /* lead at which that cost saturates  */
}

/// SearchParams
///
/// The derived search tables: reduction surfaces and pruning margins.
/// `search.rs` reads them at each node. The margins use the piece values.
/// The reduction curves are the same for all variants.
///
#[derive(Default)]
pub struct SearchParams {
    pub reduction_quiet: Vec<u8>,                                               /* plies given up, depth major, one   */
    pub reduction_quiet_check: Vec<u8>,                                         /* surface per class of move: quiet   */
    pub reduction_tactical: Vec<u8>,                                            /* or tactical, and each of those     */
    pub reduction_tactical_check: Vec<u8>,                                      /* either in check or not             */

    pub aspiration_delta: u32,                                                  /* half-width the root opens at       */
    pub rfp_margin: Vec<i32>,                                                   /* cushion, improving major, by depth */
    pub razor_margin: [i32; 4],                                                 /* qsearch rescue gap, by depth       */
    pub probcut_margin: i32,                                                    /* surplus a tactical cut must prove  */
    pub futility_margin: Vec<i32>,                                              /* alpha cushion, improving major     */
    pub lmp_count: Vec<usize>,                                                  /* moves ordered, improving major     */
    pub see_allowance: Vec<i32>,                                                /* loss a capture may show, by depth  */
    pub qsearch_delta: i32,                                                     /* gain a leaf capture must promise   */
}

/*----------------------------------------------------------------------------*\
                           PIECE GEOMETRY AND VALUES
\*----------------------------------------------------------------------------*/

/// derive_piece_roles
///
/// Gives each piece that is not royal a role from its derived value:
///
/// 1. sort the piece values, without the royal pieces
/// 2. the lowest ceil(10%) are not big
/// 3. the highest ceil(20%) are major
/// 4. the others are minor
///
/// Params:
/// - state: &mut State -> variant with the derived values
///
/// Return:
/// Vec<PieceRoles>     -> (index, is_big, is_major) for each piece
///
fn derive_piece_roles(state: &mut State) -> Vec<PieceRoles> {
    let mut ranked = collect_piece_type_pairs(state)
        .into_iter()
        .filter(|(white_index, _)| {
            !p_is_royal!(&state.statics.pieces[*white_index])
        })
        .map(|(white_index, black_index)| {
            (
                p_ovalue!(&state.statics.pieces[white_index]),
                white_index,
                black_index,
            )
        })
        .collect::<Vec<_>>();
    ranked.sort_unstable_by_key(
        |(value, white_index, _)| (*value, *white_index)
    );

    let non_big_share = ROLE_NON_BIG_SPLIT as f32 / COEFFICIENT_SCALE as f32;
    let major_share = ROLE_MAJOR_SPLIT as f32 / COEFFICIENT_SCALE as f32;

    let non_big_count = (ranked.len() as f32 * non_big_share).ceil() as usize;
    let major_count = (ranked.len() as f32 * major_share).ceil() as usize;

    log_4!(
        "Role counts - Non-big: {}, Major: {}",
        non_big_count, major_count
    );

    let mut piece_roles = Vec::with_capacity(2 * ranked.len());

    for (rank, (value, white_index, black_index)) in ranked.iter().enumerate() {
        let is_big = rank >= non_big_count;
        let is_major = is_big && rank >= ranked.len() - major_count;

        log_4!(
            "Role rank {} value {} -> big {}, major {}",
            rank, value, is_big, is_major
        );

        piece_roles.push((*white_index as PieceIndex, is_big, is_major));
        piece_roles.push((*black_index as PieceIndex, is_big, is_major));
    }

    piece_roles
}

/// derive_piece_reach
///
/// Measures the part of the board that a piece can reach with many moves.
/// From each origin, it flood-fills with the move offsets and their
/// negations, and it gives the mean reached part.
///
/// - rook     : all squares, reach one
/// - bishop   : the squares of its colour, about a half
/// - elephant : the seven points of its side, less than a tenth
///
/// A knight also has reach one. The mobility term tells that it is slower.
///
/// Params:
/// - state: &State -> precomputed move tables
/// - piece: &Piece -> piece to measure
///
/// Return:
/// f64             -> mean reached part of the board, in (0, 1]
///
/// Notes:
/// An origin with no move is not in the mean. The negated offsets stop a
/// pawn from reaching only the squares in front of it. The function
/// collects the one-step targets of each square once, before the fills.
/// Each origin still has its own fill, because a step does not always
/// have a step back.
///
fn derive_piece_reach(state: &State, piece: &Piece) -> f64 {
    let board_size = state.statics.board_size;
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let piece_index = p_index!(piece) as usize;

    let steps: Vec<Vec<usize>> = (0..board_size).map(|square| {
        let start_file = square as i32 % files;
        let start_rank = square as i32 / files;

        let relevant_moves = &state.statics.relevant_moves
            [piece_index * board_size + square];

        let mut landings: Vec<usize> = Vec::new();

        for multi_leg_vector in usual_vectors(state, relevant_moves) {
            let mut file_offset = 0;
            let mut rank_offset = 0;

            for leg in multi_leg_vector.legs.iter() {
                file_offset += x!(leg) as i32;
                rank_offset += y!(leg) as i32;
            }

            for (offset_file, offset_rank) in
                [(file_offset, rank_offset), (-file_offset, -rank_offset)]
            {
                let next_file = start_file + offset_file;
                let next_rank = start_rank + offset_rank;

                if next_file >= 0 && next_file < files
                && next_rank >= 0 && next_rank < ranks {
                    landings.push((next_rank * files + next_file) as usize);
                }
            }
        }

        landings.sort_unstable();
        landings.dedup();

        landings
    }).collect();

    let reach_values: Vec<i32> = (0..board_size).into_par_iter().map(|square| {
        let mut reached_squares = vec![false; board_size];
        let mut queue = VecDeque::new();
        let mut reached_count = 1;

        queue.push_back(square);
        reached_squares[square] = true;

        while let Some(current) = queue.pop_front() {
            for &next in &steps[current] {
                if !reached_squares[next] {
                    reached_squares[next] = true;
                    reached_count += 1;
                    queue.push_back(next);
                }
            }
        }

        if reached_count == 1 {
            0
        } else {
            reached_count
        }
    }).collect();

    let mean = reach_values.iter().filter(|&&v| v > 0).sum::<i32>() as f64
        / reach_values.iter().filter(|&&v| v > 0).count() as f64;

    mean / board_size as f64
}

/// derive_piece_offsets
///
/// Collects all different net displacements of a piece from all squares.
/// The net displacement is the sum of the legs of a vector. A slider gives
/// one offset for each distance.
///
/// Params:
/// - state: &State     -> precomputed move tables
/// - piece: &Piece     -> piece to examine
///
/// Return:
/// HashSet<(i32, i32)> -> file and rank displacements, not (0, 0)
///
fn derive_piece_offsets(state: &State, piece: &Piece) -> HashSet<(i32, i32)> {
    let board_size = state.statics.board_size;
    let piece_index = p_index!(piece) as usize;

    let mut offsets: HashSet<(i32, i32)> = HashSet::new();

    for square in 0..board_size {
        let relevant_moves = &state.statics.relevant_moves
            [piece_index * board_size + square];

        for multi_leg_vector in usual_vectors(state, relevant_moves) {
            let mut file_offset = 0;
            let mut rank_offset = 0;

            for leg in multi_leg_vector.legs.iter() {
                file_offset += x!(leg) as i32;
                rank_offset += y!(leg) as i32;
            }

            if (file_offset, rank_offset) != (0, 0) {
                offsets.insert((file_offset, rank_offset));
            }
        }
    }

    offsets
}

/// derive_piece_value
///
/// Gives the value of one piece for one phase from its moves only. It uses
/// five measurements:
///
/// - empty mobility    : moves for each square on an empty board
/// - occupied mobility : the same count with the phase occupancy
/// - families          : the same mobility for each direction family
/// - reach             : the part of the board it can reach
/// - maneuverability   : the part of its offsets that it can reverse
///
/// The mobility is 70% occupied and 30% empty. A piece that controls lines
/// of more than one family gets half the mobility of all families but its
/// largest again: it attacks two sets of squares that one enemy piece
/// cannot both avoid. Then reach scales it, with a minimum factor of 0.6,
/// and maneuverability scales it, with a minimum factor of 0.5. Thus a
/// colour-bound or one-way piece is cheaper, but not zero.
///
/// Params:
/// - state    : &State -> precomputed move tables
/// - piece    : &Piece -> piece to value
/// - occupancy: f64    -> board occupancy of the phase
///
/// Return:
/// f64                 -> raw value, normalized for the army later
///
/// Notes:
/// The opening has the higher occupancy, so sliders stop earlier there and
/// gain value in the endgame.
///
fn derive_piece_value(state: &State, piece: &Piece, occupancy: f64) -> f64 {
    log_4!("Deriving base value for piece '{}'", piece.char);

    let board_size = state.statics.board_size;
    let piece_index = p_index!(piece);

    let reach = derive_piece_reach(state, piece);
    let offsets = derive_piece_offsets(state, piece);
    let reversible = offsets
        .iter()
        .filter(|(file_offset, rank_offset)| {
            offsets.contains(&(-file_offset, -rank_offset))
        })
        .count();
    let maneuverability = if offsets.is_empty() {
        1.0
    } else {
        reversible as f64 / offsets.len() as f64
    };

    let empty_mobility = (0..board_size).into_par_iter().map(|square| {
        derive_piece_mobility(state, piece_index, square, 0.0)
    }).sum::<f64>() / board_size as f64;

    let occupied_mobility = (0..board_size).into_par_iter().map(|square| {
        derive_piece_mobility(state, piece_index, square, occupancy)
    }).sum::<f64>() / board_size as f64;

    let families = (0..board_size).map(|square| {
        let empty = derive_family_mobility(state, piece_index, square, 0.0);
        let occupied =
            derive_family_mobility(state, piece_index, square, occupancy);

        array::from_fn::<f64, 3, _>(|family| {
            0.3 * empty[family] + (1.0 - 0.3) * occupied[family]
        })
    }).fold([0.0; 3], |total, square| {
        array::from_fn(|family| total[family] + square[family])
    });
    let largest = families.iter().copied().fold(0.0, f64::max);
    let synergy = 0.5 * (families.iter().sum::<f64>() - largest)
        / board_size as f64;

    let blended_mobility = 0.3 * empty_mobility
        + (1.0 - 0.3) * occupied_mobility
        + synergy;

    let coverage = 0.6 + (1.0 - 0.6) * reach;
    let maneuver = 0.5 + (1.0 - 0.5) * maneuverability;

    58.0 * blended_mobility * coverage * maneuver
}

/// derive_piece_mobility
///
/// Gives the expected move count from one square on a board with random
/// occupancy. It uses `derive_vector_chance` for each vector.
///
/// Params:
/// - state      : &State     -> precomputed move tables
/// - piece_index: PieceIndex -> piece with the vectors
/// - square     : usize      -> origin square
/// - occupancy  : f64        -> board occupancy
///
/// Return:
/// f64                       -> expected playable vectors from the square
///
fn derive_piece_mobility(
    state: &State, piece_index: PieceIndex, square: usize, occupancy: f64
) -> f64 {
    let board_size = state.statics.board_size;

    let relevant_moves = &state.statics.relevant_moves
        [piece_index as usize * board_size + square];

    relevant_moves
        .iter()
        .filter_map(|vector| derive_vector_chance(state, vector, occupancy))
        .map(|(chance, ..)| chance)
        .sum()
}

/// derive_family_mobility
///
/// Gives the expected move count from one square for each direction
/// family. Only a vector whose last leg can move and take counts, because
/// that is a line the piece controls:
///
/// - orthogonal : no file change or no rank change
/// - diagonal   : the same file and rank change
/// - oblique    : any other change, as a knight leap
///
/// Params:
/// - state      : &State     -> precomputed move tables
/// - piece_index: PieceIndex -> piece with the vectors
/// - square     : usize      -> origin square
/// - occupancy  : f64        -> board occupancy
///
/// Return:
/// [f64; 3]                  -> expected vectors, one for each family
///
fn derive_family_mobility(
    state: &State, piece_index: PieceIndex, square: usize, occupancy: f64
) -> [f64; 3] {
    let board_size = state.statics.board_size;
    let mut families = [0.0; 3];

    for vector in &state.statics.relevant_moves
        [piece_index as usize * board_size + square]
    {
        let Some(final_leg) = vector.legs.last() else {
            continue;
        };

        if (c!(final_leg) || d!(final_leg)) != m!(final_leg) {                  /* a move-only or take-only last leg  */
            continue;
        }

        let Some((chance, file_delta, rank_delta)) =
            derive_vector_chance(state, vector, occupancy)
        else {
            continue;
        };

        let family = if file_delta == 0 || rank_delta == 0 {
            0
        } else if file_delta.abs() == rank_delta.abs() {
            1
        } else {
            2
        };

        families[family] += chance;
    }

    families
}

/// derive_vector_chance
///
/// Walks one vector on a board with random occupancy. It gives the chance
/// that all leg conditions are true, and the total displacement.
///
/// - pass         : needs an empty square, chance `1 - occupancy`
/// - screen       : needs an occupied square, chance `occupancy`
/// - final, move  : a move-only last leg, half the empty chance
/// - final, take  : a capture-only last leg, half of `occupancy`
/// - final, both  : a last leg that moves and takes lands always, 1
/// - marker       : has no displacement, skipped
///
/// The chance is the product, with the chance of the CPMN condition from
/// `derive_condition_chance`. Thus a long slide decreases with each
/// square, a pawn step and a pawn capture each land less often than a
/// leaper, and a hopper capture has 0 on an empty board. A single-purpose
/// last leg counts half: the piece moves to a square it cannot guard, or
/// guards a square it cannot move to.
///
/// Params:
///
///     state: &State
///     start census for the condition chance
///
///     vector: &MoveVector
///     the vector, the final leg last
///
///     occupancy: f64
///     board occupancy
///
/// Return:
///
///     Option<(f64, i32, i32)>
///     the chance and the file and rank displacement, or None without legs
///
fn derive_vector_chance(
    state: &State, vector: &MoveVector, occupancy: f64
) -> Option<(f64, i32, i32)> {
    let (final_leg, intermediate_legs) = vector.legs.split_last()?;

    let mut chance = 1.0;
    let mut file_delta = x!(final_leg) as i32;
    let mut rank_delta = y!(final_leg) as i32;

    for leg in intermediate_legs {
        file_delta += x!(leg) as i32;
        rank_delta += y!(leg) as i32;

        if x!(leg) == 0 && y!(leg) == 0 {
            continue;
        }

        let needs_piece = (c!(leg) || d!(leg)) && !m!(leg);

        chance *= if needs_piece {
            occupancy
        } else {
            1.0 - occupancy
        };
    }

    let final_flagged = c!(final_leg) || d!(final_leg);
    let final_moves = m!(final_leg) || !final_flagged;                          /* a plain last leg moves and takes   */
    let final_takes = final_flagged || !m!(final_leg);

    chance *= match (final_moves, final_takes) {
        (true, false) => 0.5 * (1.0 - occupancy),
        (false, true) => 0.5 * occupancy,
        _ => 1.0,
    };

    chance *= derive_condition_chance(state, vector, occupancy);

    Some((chance, file_delta, rank_delta))
}

/// derive_condition_chance
///
/// Gives the chance that the CPMN condition of a vector matches on a board
/// with random occupancy. A square holds a piece with chance `occupancy`,
/// and the piece is of one type with its share of the start census:
///
/// - piece set : the sum of the chances of its members
/// - empty `?` : adds `1 - occupancy`
/// - allower   : the chance of its set
/// - stopper   : one minus the chance of its set
/// - pattern   : the product of its allowers and stoppers
/// - condition : one minus the product of the pattern misses
///
/// A vector without a condition has chance one.
///
/// Params:
/// - state    : &State      -> start census of the pieces
/// - vector   : &MoveVector -> vector with the condition
/// - occupancy: f64         -> board occupancy
///
/// Return:
/// f64                      -> chance that the condition matches
///
/// Notes:
/// A census without pieces, as in a setup phase, gives each type the same
/// share.
///
fn derive_condition_chance(
    state: &State, vector: &MoveVector, occupancy: f64
) -> f64 {
    let Some(patterns) = &vector.pattern else {
        return 1.0;
    };

    let piece_types = state.piece_count.len();
    let census_total = state.piece_count.iter().sum::<u32>();

    let piece_share = |piece_index: usize| {
        if census_total == 0 {
            1.0 / piece_types as f64
        } else {
            state.piece_count[piece_index] as f64 / census_total as f64
        }
    };

    let set_chance = |pieces: &PieceSet| {
        let empty_chance = if pieces.contains(NO_PIECE) {
            1.0 - occupancy
        } else {
            0.0
        };

        (0..piece_types)
            .filter(|&piece_index| pieces.contains(piece_index as PieceIndex))
            .map(|piece_index| occupancy * piece_share(piece_index))
            .sum::<f64>()
            + empty_chance
    };

    let miss_chance = patterns
        .iter()
        .map(|(allowers, stoppers)| {
            let allower_chance = allowers
                .iter()
                .map(|(_, pieces)| set_chance(pieces))
                .product::<f64>();
            let stopper_chance = stoppers
                .iter()
                .map(|(_, pieces)| 1.0 - set_chance(pieces))
                .product::<f64>();

            1.0 - allower_chance * stopper_chance
        })
        .product::<f64>();

    1.0 - miss_chance
}

/// usual_vectors
///
/// Keeps the vectors that are usual moves: the vectors whose condition
/// matches with at least `USUAL_CONDITION_CHANCE` at the opening
/// occupancy. A vector without a condition is always usual. The geometry
/// of a piece reads only these vectors. Thus an Annan pawn with a rook
/// move, when a rook stands behind it, stays a pawn.
///
/// Params:
/// - state  : &State        -> start census of the pieces
/// - vectors: &[MoveVector] -> vectors of one piece on one square
///
/// Return:
/// impl Iterator            -> the usual vectors, in order
///
fn usual_vectors<'a>(
    state: &'a State, vectors: &'a [MoveVector]
) -> impl Iterator<Item = &'a MoveVector> + 'a {
    let occupancy = OPENING_OCCUPANCY as f64 / COEFFICIENT_SCALE;
    let usual_chance = USUAL_CONDITION_CHANCE as f64 / COEFFICIENT_SCALE;

    vectors.iter().filter(move |vector| {
        derive_condition_chance(state, vector, occupancy) >= usual_chance
    })
}

/// derive_distance_from_center
///
/// Measures the Euclidean distance from a square to the nearest center
/// square. A board has one to four center squares, for odd or even sizes.
///
/// Params:
/// - state : &State -> position with the board dimensions
/// - square: usize  -> square to measure
///
/// Return:
/// f64              -> distance in squares
///
fn derive_distance_from_center(state: &State, square: usize) -> f64 {
    let center_file = if state.statics.files % 2 == 1 {
        vec![(state.statics.files as f64 / 2.0).floor() as u8]
    } else {
        vec![state.statics.files / 2, (state.statics.files / 2) - 1]
    };
    let center_rank = if state.statics.ranks % 2 == 1 {
        vec![(state.statics.ranks as f64 / 2.0).floor() as u8]
    } else {
        vec![state.statics.ranks / 2, (state.statics.ranks / 2) - 1]
    };

    let mut center_squares = vec![];

    for file in &center_file {
        for rank in &center_rank {
            center_squares.push(*rank * state.statics.files + *file);
        }
    }

    center_squares
        .iter()
        .map(|&index| square_distance(state, square as Square, index as Square))
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap_or_else(
            || panic!("Error while getting distance from center squares")
        )
}

/// derive_promotion_field
///
/// Gives the distance from each square to the nearest promotion square of
/// the piece, mandatory or optional. Without a zone, the distance is
/// infinite.
///
/// Params:
/// - state      : &State     -> board dimensions and promotion zones
/// - piece_index: PieceIndex -> piece with the zones
///
/// Return:
/// Vec<f64>                  -> distance for each square, or infinity
///
/// Notes:
/// The function reads the zone once for the full board. A read for each
/// square would cost the square of the board size for a full-board zone.
///
fn derive_promotion_field(state: &State, piece_index: PieceIndex) -> Vec<f64> {
    let zone: Vec<usize> = set_indices!(
        state.statics.promotion_zones_mandatory[piece_index as usize]
    )
    .into_iter()
    .chain(set_indices!(
        state.statics.promotion_zones_optional[piece_index as usize]
    ))
    .collect();

    (0..state.statics.board_size).into_par_iter().map(|square| {
        zone.iter()
            .map(|&index|
                square_distance(state, square as Square, index as Square)
            )
            .fold(f64::INFINITY, f64::min)
    }).collect()
}

/// derive_promotion_target
///
/// Gives the value that a promotion of one piece can bring, for one phase.
/// Normally it is the most valuable of the types the piece can promote to.
/// When a piece can promote only to a type that was captured, the pool is
/// empty at the start and fills with the pieces that were traded, so only
/// the cheapest target is sure.
///
/// Params:
/// - state      : &State     -> variant with the piece values and rules
/// - piece_index: PieceIndex -> the piece that promotes
/// - is_endgame : bool       -> true for the endgame values
///
/// Return:
/// i32                       -> value of the promotion target, 0 if none
///
fn derive_promotion_target(
    state: &State, piece_index: PieceIndex, is_endgame: bool
) -> i32 {
    let values = state.statics.pieces[piece_index as usize].promotions.iter()
        .map(|target| &state.statics.pieces[*target as usize])
        .map(|piece| match is_endgame {
            true => p_evalue!(piece) as i32,
            false => p_ovalue!(piece) as i32,
        });

    match promote_to_captured!(state) {
        true => values.min(),
        false => values.max(),
    }
    .unwrap_or(0)
}

/// derive_promotion_span
///
/// Gives the spread of the promotion distance on the board for one piece.
/// The promotion bonus is a gradient. If all squares have the same
/// distance, there is no gradient and the bonus is zero.
///
/// Params:
/// - field: &[f64] -> promotion distance of each square
///
/// Return:
/// f64             -> spread of the finite distances, 0 if none
///
/// Notes:
/// A zone on the full board gives distance zero on all squares. Without
/// the span, that gave a flat extra material value in the table.
///
fn derive_promotion_span(field: &[f64]) -> f64 {
    let finite = field.iter().copied().filter(|d| d.is_finite());

    let (low, high) = finite.fold(
        (f64::INFINITY, f64::NEG_INFINITY),
        |(low, high), d| (low.min(d), high.max(d))
    );

    if low.is_finite() && high > low { high - low } else { 0.0 }
}

/// derive_promotion_bonus
///
/// Gives the progress bonus of a piece that can promote. It is a gradient
/// to the nearest promotion square: 6% of the promotion gain, times the
/// progress, in both phases.
///
/// The progress is squared, so the bonus is flat at the start and steep
/// near the zone. A piece without a zone, or with a zone on the full board,
/// gets zero. The gradient only orders the squares. The passed pawn term
/// of `pawn_structure!` gives the value of a free path, so a larger
/// endgame gradient would count the same race two times.
///
/// Params:
/// - state         : &State     -> board dimensions and promotion zones
/// - piece_index   : PieceIndex -> piece to place
/// - closest       : f64        -> promotion distance from this square
/// - span          : f64        -> spread of that distance on the board
/// - piece_value   : f64        -> value of the piece in this phase
/// - promoted_value: f64        -> value of its promotion target
///
/// Return:
/// f64                          -> bonus to add to the square score
///
fn derive_promotion_bonus(
    state: &State, piece_index: PieceIndex, closest: f64, span: f64,
    piece_value: f64, promoted_value: f64
) -> f64 {
    let piece = &state.statics.pieces[piece_index as usize];

    if !p_can_promote!(piece) || span <= 0.0 {                             /* no zone, or one that is everywhere */
        return 0.0;
    }

    let advancement =
        (1.0 - closest / state.statics.ranks as f64).max(0.0);

    0.06 * (promoted_value - piece_value).max(0.0)
        * advancement.powf(2.0)
}

/// derive_pst
///
/// Makes one piece-square table. The raw score of a square is the mobility
/// from it minus the distance from the center, with phase weights:
///
/// - opening : mobility 0.50, centrality 1.25, board 36% full
/// - endgame : mobility 0.25, centrality 1.75, board 12% full
///
/// The scores are centered on their mean and scaled to an amplitude from
/// the piece value. A more valuable piece gets a wider range. The promotion
/// gradient is added after the scaling. On a small board, the center is
/// positive and the edge negative:
///
/// ```text
/// ┌────┬────┬────┬────┐
/// │ -9 │ -4 │ -4 │ -9 │
/// ├────┼────┼────┼────┤
/// │ -4 │ +6 │ +6 │ -4 │
/// ├────┼────┼────┼────┤
/// │ -9 │ -4 │ -4 │ -9 │
/// └────┴────┴────┴────┘
/// ```
///
/// In the opening, a royal piece gets a back rank gradient in place of the
/// mobility and center score, so it stays behind its lines:
///
/// ```text
/// rank 3   -3  -3  -3  -3
/// rank 2   -2  -2  -2  -2
/// rank 1   -1  -1  -1  -1
/// rank 0    0   0   0   0   the rank it started on, and the best one
/// ```
///
/// Each square scores the negative rank index for White. No file is better
/// than another. Shelter and castling decide the file. The endgame table
/// keeps the center score, because the royal must be active.
///
/// Params:
/// - index         : PieceIndex -> piece of the table
/// - state         : &State     -> precomputed move tables
/// - is_endgame    : bool       -> selects the phase weights and values
/// - promoted_value: f64        -> value of its promotion target
///
/// Return:
/// Vec<i32>                     -> bonus for each square, in board order
///
fn derive_pst(
    index: PieceIndex, state: &State, is_endgame: bool, promoted_value: f64
) -> Vec<i32> {
    let board_size = state.statics.board_size;
    let files = state.statics.files as usize;
    let piece = &state.statics.pieces[index as usize];

    let piece_value = if is_endgame {
        p_evalue!(piece) as f64
    } else {
        p_ovalue!(piece) as f64
    };

    let occupancy = if is_endgame {
        ENDGAME_OCCUPANCY
    } else {
        OPENING_OCCUPANCY
    } as f64 / COEFFICIENT_SCALE;

    let mobility_weight = if is_endgame { 0.25 } else { 0.5 };
    let center_weight = if is_endgame { 1.75 } else { 1.25 };

    let stand_in = state.termination.extinct.iter().any(|rule| {
        rule.lone[p_color!(piece) as usize] == Some(index as usize)
    });

    let scores: Vec<f64> = if !is_endgame && (p_is_royal!(piece) || stand_in) {
        (0..board_size).map(|square| -((square / files) as f64)).collect()
    } else {
        (0..board_size).into_par_iter().map(|square| {
            mobility_weight
                * derive_piece_mobility(state, index, square, occupancy)
                - center_weight
                * derive_distance_from_center(state, square)
        }).collect()
    };

    let mean = scores.iter().sum::<f64>() / board_size as f64;
    let max_deviation = scores
        .iter()
        .map(|score| (score - mean).abs())
        .fold(0.0_f64, f64::max)
        .max(1.0);
    let amplitude = (piece_value * PST_AMPLITUDE_RATIO as f64                  /* a dearer piece earns a wider band, */
        / COEFFICIENT_SCALE).max(PST_AMPLITUDE_FLOOR);                         /* never a narrower one than before   */

    let promotion_field = derive_promotion_field(state, index);                /* read the zone once, not per square */
    let promotion_span = derive_promotion_span(&promotion_field);

    (0..board_size).map(|square| {
        let positional =
            (scores[square] - mean) / max_deviation * amplitude;
        let promotion = derive_promotion_bonus(
            state, index, promotion_field[square], promotion_span,
            piece_value, promoted_value
        );

        (positional + promotion).round() as i32
    }).collect()
}

/// walk_setup_endings
///
/// Walks the setup phase depth first on one scratch position. It records
/// the piece counts each time the variant rules end SETUP. It makes and
/// undoes the placements in place.
///
/// The visited key is the census, not the board:
///
/// - piece_count   : count of each type on the board
/// - piece_in_hand : pieces to place, White hand then Black hand
/// - playing       : side that places the next piece
///
/// Thus two setups that differ only in the squares of equal pieces are one
/// state.
///
/// Params:
/// - probe  : &mut State             -> scratch position, restored at end
/// - visited: &mut HashSet<Vec<u32>> -> censuses already expanded
/// - endings: &mut Vec<Vec<u32>>     -> censuses of completed setups
///
/// Notes:
/// The function tests the two caps at entry and stops at the ending cap
/// also inside the loop.
///
fn walk_setup_endings(
    probe: &mut State,
    visited: &mut HashSet<Vec<u32>>,
    endings: &mut Vec<Vec<u32>>,
) {
    let mut census = probe.piece_count.clone();

    for side in [WHITE as usize, BLACK as usize] {
        census.extend(
            probe.piece_in_hand[side].iter().map(|held| *held as u32)
        );
    }

    census.push(probe.playing as u32);

    if endings.len() >= SETUP_ENDING_CAP
    || visited.len() >= SETUP_STATE_CAP
    || !visited.insert(census)
    {
        return;
    }

    let mut placements = Vec::new();
    let mut scratch = Vec::new();

    generate_all_moves_and_drops(probe, &mut placements, &mut scratch);

    for placement in placements {
        if !make_move!(probe, placement) {
            continue;
        }

        if probe.game_phase == SETUP {
            walk_setup_endings(probe, visited, endings);
        } else {
            endings.push(probe.piece_count.clone());
        }

        undo_move!(probe);

        if endings.len() >= SETUP_ENDING_CAP {
            break;
        }
    }
}

/// resolve_setup_army
///
/// Gives the army of a variant at the start of play. In a setup variant,
/// the hand is a choice, not a promise that all pieces are placed.
///
/// - on the board : the current census, no walk
/// - setup phase  : the mean census of the completed setups of the walk
/// - no ending    : the current census
///
/// Params:
/// - state: &State -> start position, with derived piece values
///
/// Return:
/// Vec<u32>        -> count of each piece index at the start of play
///
/// Notes:
/// The walk uses a copy of the position. The copy must be dropped before
/// the caller writes a static, because `static_mut` needs sole ownership.
/// The copy rebuilds its eval caches first, because the new roles change
/// the counts.
///
fn resolve_setup_army(state: &State) -> Vec<u32> {
    if state.game_phase != SETUP {
        return state.piece_count.clone();
    }

    let mut probe = state.clone();
    let mut visited = HashSet::new();
    let mut endings = Vec::new();

    refresh_eval_state(&mut probe);                                             /* roles changed under the copy       */
    walk_setup_endings(&mut probe, &mut visited, &mut endings);

    log_4!(
        "Setup walk: {} censuses expanded, {} endings reached",
        visited.len(), endings.len()
    );

    if endings.is_empty() {
        return state.piece_count.clone();
    }

    (0..state.piece_count.len())
        .map(|piece_index| {
            let total = endings
                .iter()
                .map(|ending| ending[piece_index] as u64)
                .sum::<u64>();

            (total / endings.len() as u64) as u32
        })
        .collect()
}

/*----------------------------------------------------------------------------*\
                             DERIVATION ENTRY POINT
\*----------------------------------------------------------------------------*/

/// derive_parameters
///
/// The startup entry point of the derivation. Each step reads the steps
/// before it:
///
/// 1. eval         : piece values and roles
/// 2. search       : margins and reductions
/// 3. shelter      : the royal ring, and if shelter applies
/// 4. danger       : the zone attacks, on the shelter ring
/// 5. pawn         : pawn structure and passed pawn terms
/// 6. advantage    : imbalance and contempt
/// 7. capabilities : the pruning claims that the rules allow
/// 8. refresh      : the incremental caches
///
/// Params:
/// - state: &mut State -> new precomputed variant state
///
pub fn derive_parameters(state: &mut State) {
    derive_eval_parameters(state);
    derive_search_parameters(state);
    derive_shelter_parameters(state);
    derive_danger_parameters(state);
    derive_pawn_parameters(state);
    derive_advantage_parameters(state);
    derive_search_capabilities(state);
    refresh_eval_state(state);
}

/*----------------------------------------------------------------------------*\
                               SEARCH DERIVATION
\*----------------------------------------------------------------------------*/

/// reduction_surface
///
/// Makes one late move reduction table. For each remaining depth and move
/// number, it gives the plies that a move of that class loses in its first
/// search.
///
/// ```text
/// quiet surface   move 1   move 8   move 32   move 63
/// depth 2              0        1         1         2
/// depth 8              0        2         3         4
/// depth 32             0        3         6         7
/// ```
///
/// Params:
/// - base   : u32 -> curve base, over `COEFFICIENT_SCALE`
/// - divisor: u32 -> curve divisor, over `COEFFICIENT_SCALE`
/// - shape  : F   -> the depth and move terms of the curve
///
/// Return:
/// Vec<u8>        -> `MAX_DEPTH * REDUCTION_MOVE_CAP` plies, depth major
///
/// Notes:
/// A table read is faster than a logarithm at each node, and each cell fits
/// a byte. Depth zero and move zero are never reduced, so they are zero.
///
pub fn reduction_surface<F>(base: u32, divisor: u32, shape: F) -> Vec<u8>
where
    F: Fn(f64, f64) -> f64,
{
    let base = base as f64 / COEFFICIENT_SCALE;
    let divisor = divisor as f64 / COEFFICIENT_SCALE;
    let mut table = vec![0u8; MAX_DEPTH * REDUCTION_MOVE_CAP];

    for depth in 1..MAX_DEPTH {
        for moves in 1..REDUCTION_MOVE_CAP {
            let plies = base + shape(depth as f64, moves as f64) / divisor;

            table[depth * REDUCTION_MOVE_CAP + moves] =
                plies.clamp(0.0, u8::MAX as f64) as u8;
        }
    }

    table
}

/// dearest_piece_value
///
/// Gives the opening value of the most valuable piece that is not royal.
/// All derived margins and safety values are parts of it. The cheapest
/// piece is 100 in all variants, so only the top shows the value scale.
///
/// Params:
/// - state: &State -> variant with the piece values
///
/// Return:
/// u64             -> the largest value, or zero for only royal pieces
///
fn dearest_piece_value(state: &State) -> u64 {
    state.statics.pieces
        .iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_ovalue!(piece) as u64)
        .max()
        .unwrap_or(0)
}

/// derive_search_parameters
///
/// Makes the search tables: the four reduction surfaces, the margins, the
/// move counts and the aspiration width. The base value of each is:
///
/// - aspiration       : most valuable piece, half the root window
/// - reverse futility : most valuable piece, a step for each depth, 2 rows
/// - razoring         : mean piece, four depths, a tactical swing
/// - ProbCut          : mean piece, the same swing, one number
/// - futility         : most valuable piece, floor plus step, 2 rows
/// - late move count  : no material, depth squared
/// - exchange         : most valuable piece, a step for each depth, 1 row
/// - quiescence delta : most valuable piece
///
/// The cheapest piece is 100 in all variants, so the most valuable piece
/// shows the value scale. Razoring and ProbCut use the mean piece, because
/// they compare a full tactical swing.
///
/// The improving flag selects the row that prunes harder. A beta cut trusts
/// an improving side sooner. The alpha cuts stop earlier for a side that is
/// not improving. Each two-row table is one flat vector:
///
/// ```text
/// improving clear   [ 0  d1  d2  ...  dn ]   read at depth
/// improving set     [ 0  d1  d2  ...  dn ]   read at deepest + 1 + depth
/// ```
///
/// Params:
/// - state: &mut State -> variant with the search values to rebuild
///
/// Notes:
/// The function asserts that each table increases with depth. Thus a wrong
/// ratio fails at load. The aspiration width and the late move count are
/// at least one.
///
#[hotpath::measure]
pub fn derive_search_parameters(state: &mut State) {
    let dearest = dearest_piece_value(state);
    let (value_sum, value_count) = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .fold((0u64, 0u64), |(sum, count), piece| {
            (sum + p_ovalue!(piece) as u64, count + 1)
        });
    let mean = value_sum.checked_div(value_count).unwrap_or(0) as i32;

    let delta = dearest * ASPIRATION_RATIO as u64
        / COEFFICIENT_SCALE as u64;

    let deepest = RFP_DEPTH as usize;
    let step = dearest * RFP_RATIO as u64 / COEFFICIENT_SCALE as u64;
    let mut margins = vec![0i32; 2 * (deepest + 1)];

    for depth in 1..=deepest {
        let flat = step * depth as u64;

        margins[depth] = flat as i32;
        margins[deepest + 1 + depth] = (flat
            * RFP_IMPROVING as u64
            / COEFFICIENT_SCALE as u64) as i32;
    }

    let razor = [
        0, mean / 3 + 100, mean / 2 + 200, mean + 300,
    ];
    let probcut = (mean / 4).max(100);

    let futility_deepest = FUTILITY_DEPTH as usize;
    let futility_floor = dearest * FUTILITY_FLOOR as u64
        / COEFFICIENT_SCALE as u64;
    let futility_step = dearest * FUTILITY_RATIO as u64
        / COEFFICIENT_SCALE as u64;
    let mut futility = vec![0i32; 2 * (futility_deepest + 1)];

    for depth in 1..=futility_deepest {
        let risen = futility_floor + futility_step * depth as u64;

        futility[futility_deepest + 1 + depth] = risen as i32;
        futility[depth] = (risen
            * FUTILITY_IMPROVING as u64
            / COEFFICIENT_SCALE as u64) as i32;
    }

    let lmp_deepest = LMP_DEPTH as usize;
    let mut counts = vec![0usize; 2 * (lmp_deepest + 1)];

    for depth in 0..=lmp_deepest {
        let risen = LMP_BASE as u64
            + (depth * depth) as u64 * LMP_RATIO as u64
                / COEFFICIENT_SCALE as u64;

        counts[lmp_deepest + 1 + depth] = risen as usize;
        counts[depth] = (risen * LMP_IMPROVING as u64
            / COEFFICIENT_SCALE as u64).max(1) as usize;                        /* one quiet always gets ordered      */
    }

    let see_deepest = SEE_PRUNE_DEPTH as usize;
    let see_step = dearest * SEE_PRUNE_RATIO as u64
        / COEFFICIENT_SCALE as u64;
    let mut allowance = vec![0i32; see_deepest + 1];

    for depth in 1..=see_deepest {
        allowance[depth] = (see_step * depth as u64) as i32;
    }

    let qsearch_delta = dearest * QSEARCH_DELTA_RATIO as u64
        / COEFFICIENT_SCALE as u64;

    assert!(
        futility.chunks(futility_deepest + 1)
            .all(|row| row.windows(2).all(|pair| pair[0] <= pair[1])),
        "Futility margins must not fall with the depth left to search."
    );

    assert!(
        counts.chunks(lmp_deepest + 1)
            .all(|row| row.windows(2).all(|pair| pair[0] <= pair[1])),
        "Late-move counts must not fall with the depth left to search."
    );

    assert!(
        allowance.windows(2).all(|pair| pair[0] <= pair[1]),
        "Exchange allowances must not fall with the depth left to search."
    );

    let quiet = reduction_surface(
        REDUCTION_QUIET_BASE,
        REDUCTION_QUIET_DIVISOR,
        |depth, moves| depth.ln() * moves.ln(),
    );

    let quiet_check = reduction_surface(
        REDUCTION_QUIET_CHECK_BASE,
        REDUCTION_QUIET_CHECK_DIVISOR,
        |depth, moves| depth.sqrt() * moves.ln(),
    );

    let tactical = reduction_surface(
        REDUCTION_TACTICAL_BASE,
        REDUCTION_TACTICAL_DIVISOR,
        |depth, moves| depth.ln() * moves.sqrt(),
    );

    let tactical_check = reduction_surface(
        REDUCTION_TACTICAL_CHECK_BASE,
        REDUCTION_TACTICAL_CHECK_DIVISOR,
        |depth, moves| depth.ln() * moves.ln(),
    );

    let statics = state.static_mut();

    statics.search.reduction_quiet = quiet;
    statics.search.reduction_quiet_check = quiet_check;
    statics.search.reduction_tactical = tactical;
    statics.search.reduction_tactical_check = tactical_check;

    statics.search.aspiration_delta = (delta as u32).max(1);                    /* a window has to hold two scores    */
    statics.search.rfp_margin = margins;
    statics.search.razor_margin = razor;
    statics.search.probcut_margin = probcut;
    statics.search.futility_margin = futility;
    statics.search.lmp_count = counts;
    statics.search.see_allowance = allowance;
    statics.search.qsearch_delta = qsearch_delta as i32;
}

/// derive_search_capabilities
///
/// Decides once, before play, which search shortcuts the rules allow, and
/// writes them to `capabilities` (see [`StaticState`]). Each shortcut is a
/// claim about the game, for example that material decides. Each bit is
/// set, unless a rule removes it:
///
/// - exchange simulation : multi own destroy, misere, extinction
/// - pruning on it       : counting
/// - forward pruning     : misere, extinction
/// - null pruning        : misere, counting, a legal pass, stand-offs,
///                         setup, a piece without quiets
/// - recapture ordering  : none
/// - quiet pruning       : misere
/// - static movement     : a screened leg or a CPMN move condition
/// - wide quiescence     : a vector that captures more than one piece
///
/// Two fact types answer the tests:
///
/// - movement facts : from the generated vectors
/// - terminal facts : from the declared end rules
///
/// A vector that ends on its start square without a capture is a legal
/// pass. A lion that returns over a destroyed piece is not a pass, so the
/// destroy flag counts, not only the displacement. The final leg of a plain
/// slider has no capture flag, but move generation reads it as a capture,
/// so the count here does the same.
///
/// Params:
/// - state: &mut State -> variant with the capability mask to derive
///
/// Notes:
/// Drops keep the exchange bits. With drops, each capture of an exchange
/// also puts the piece in the hand of the taker, so each step is worth two
/// times as much. The sum doubles and its sign stays, so a losing capture
/// still loses.
///
/// Promotion to a captured type keeps the exchange bits. A capture gives
/// the piece back to the pool of its owner, not to the hand of the taker,
/// and the simulation makes real moves, so each promotion obeys the pool.
///
/// A leg that can take only a royal does not stop the exchange simulation.
/// An exchange square never holds a royal that a move can take.
///
/// Recapture ordering does not test multi own destroy now. The capture band
/// is the capture value minus the own destroyed value, so such a move sorts
/// below the quiet moves. The exchange simulation keeps the test.
///
/// The scan reads each piece on each square, because some legs exist only
/// near an edge. Each piece has its own thread, and the results join with
/// `or`. No test reads a variant name.
///
#[hotpath::measure]
pub fn derive_search_capabilities(state: &mut State) {
    let statics = &state.statics;
    let board_size = statics.board_size;

    let movement_facts = (0..statics.pieces.len()).into_par_iter()
        .map(|piece_index| {
            let mut screened = false;
            let mut conditioned = false;
            let mut multi_destroy = false;
            let mut multi_capture = false;
            let mut may_pass = false;
            let mut vectors = 0;
            let mut quiet_vectors = 0;

            for square in 0..board_size {
                let slot = piece_index * board_size + square;

                for vector in statics.relevant_moves[slot].iter()
                    .chain(statics.relevant_captures[slot].iter())
                {
                    let mut destroyed = 0;
                    let mut victims = 0;
                    let mut destroys = false;

                    for (leg_index, leg) in vector.legs.iter().enumerate() {
                        let last_leg = leg_index + 1 == vector.legs.len();
                        let takes = c!(leg) || d!(leg)
                            || (last_leg && !m!(leg));                          /* a plain slider takes on its last   */

                        screened |= u!(leg);
                        destroys |= d!(leg);
                        destroyed += (d!(leg) && !u!(leg)) as usize;            /* an unloaded piece is put back      */
                        victims += takes as usize;
                        victims = victims.saturating_sub(u!(leg) as usize);     /* a screen is taken and handed back  */
                    }

                    let (files_crossed, ranks_crossed) = vector_offset!(vector);
                    let moves_quietly = vector_moves_quietly!(vector);

                    conditioned |= vector.pattern.is_some();
                    multi_destroy |= destroyed > 1;
                    multi_capture |= victims > 1;
                    may_pass |= moves_quietly && !destroys
                        && files_crossed == 0 && ranks_crossed == 0;            /* nothing moved and nothing taken    */
                    vectors += 1;
                    quiet_vectors += moves_quietly as usize;
                }
            }

            let capture_only = vectors > 0 && quiet_vectors == 0;

            [
                screened, conditioned, multi_destroy, capture_only,
                multi_capture, may_pass,
            ]
        })
        .reduce(
            || [false; 6],
            |left, right| array::from_fn(|fact| left[fact] || right[fact]),
        );

    let [
        screened, conditioned, multi_destroy, capture_only, multi_capture,
        may_pass,
    ] = movement_facts;

    let termination = &state.termination;

    let counts_pieces = !termination.extinct.is_empty();
    let counts_material = termination.counting.is_some();

    let misere = termination.checkmate == Outcome::Win
        || termination.stalemate == Outcome::Win
        || termination.extinct.iter()
            .any(|rule| rule.outcome == Outcome::Win);

    let places_army = setup_phase!(state);
    let vetoes_moves = stand_offs!(state);

    let mut capabilities = 0u16;

    if !multi_destroy && !misere && !counts_pieces {
        enc_see_valid!(capabilities);
    }

    if !counts_material {
        enc_see_pruning!(capabilities);
    }

    if !misere && !counts_pieces {
        enc_forward_pruning!(capabilities);
    }

    if !misere && !counts_material
    && !may_pass && !vetoes_moves && !places_army && !capture_only
    {
        enc_null_pruning!(capabilities);
    }

    enc_recapture_order!(capabilities);

    if !misere {
        enc_quiet_pruning!(capabilities);
    }

    if !screened && !conditioned {
        enc_static_movement!(capabilities);
    }

    if !multi_capture {
        enc_wide_quiescence!(capabilities);
    }

    state.static_mut().capabilities = capabilities;

    log_3!("Derived Search Capabilities: {:08b}", capabilities);
}

/*----------------------------------------------------------------------------*\
                             EVALUATION DERIVATION
\*----------------------------------------------------------------------------*/

/// derive_base_pst
///
/// Makes the opening and endgame piece-square tables of each piece from
/// the rules. The material must be final first, because the promotion
/// gradient uses the promotion value. A Black piece uses its White pair:
///
/// 1. find the White pair index of the piece
/// 2. derive the scores there: mobility, center distance, promotion
/// 3. flip the rows across the horizontal axis
/// 4. store them under the index of the piece
///
/// The promotion value of each phase is the most valuable piece of that
/// phase that is not royal.
///
/// Params:
///
///     state: &State
///     variant to derive the tables for
///
/// Return:
///
///     (Vec<Vec<i32>>, Vec<Vec<i32>>)
///     opening and endgame rows by piece index, White and mirrored Black
///
pub fn derive_base_pst(state: &State) -> (Vec<Vec<i32>>, Vec<Vec<i32>>) {
    let pst_entries: Vec<(usize, Vec<i32>, Vec<i32>)> =
        state.statics.pieces.par_iter().map(|piece| {
            let mut index = p_index!(piece);

            if p_color!(piece) == BLACK {
                index = state.statics.piece_swap_map[index as usize];
            }

            let promoted_opening =
                derive_promotion_target(state, index, false) as f64;
            let promoted_endgame =
                derive_promotion_target(state, index, true) as f64;
            let mut opening_pst =
                derive_pst(index, state, false, promoted_opening);
            let mut endgame_pst =
                derive_pst(index, state, true, promoted_endgame);

            if p_color!(piece) == BLACK {
                opening_pst = mirror_pst_across_horizontal_axis(
                    &opening_pst,
                    state.statics.files as usize,
                    state.statics.ranks as usize
                );
                endgame_pst = mirror_pst_across_horizontal_axis(
                    &endgame_pst,
                    state.statics.files as usize,
                    state.statics.ranks as usize
                );

                index = state.statics.piece_swap_map[index as usize];
            }

            (index as usize, opening_pst, endgame_pst)
        }).collect();

    let piece_count = state.statics.pieces.len();
    let board_size = state.statics.board_size;
    let mut opening = vec![vec![0; board_size]; piece_count];
    let mut endgame = vec![vec![0; board_size]; piece_count];

    for (index, opening_pst, endgame_pst) in pst_entries {
        opening[index] = opening_pst;
        endgame[index] = endgame_pst;
    }

    (opening, endgame)
}

/// derive_material_values
///
/// Derives the opening and endgame material from the moves only, one value
/// for each phase occupancy. Then it shifts the table, so the cheapest
/// piece is 100.
///
/// 1. derive each White piece, once for each occupancy
/// 2. the offset is the cheapest opening value minus 100
/// 3. subtract the offset from the two values of each piece
/// 4. if the largest value is above 14 bits, scale the table down to fit
///
/// Params:
/// - state: &mut State -> variant with the material values to derive
///
/// Notes:
/// The shift makes variants comparable. Black gets the White values through
/// the swap map. The role flags stay clear, because the roles need the
/// completed table.
///
fn derive_material_values(state: &mut State) {
    let opening_occupancy = OPENING_OCCUPANCY as f64 / COEFFICIENT_SCALE;
    let endgame_occupancy = ENDGAME_OCCUPANCY as f64 / COEFFICIENT_SCALE;

    let values = state.statics.pieces
        .par_iter()
        .filter_map(|piece| {
            if p_color!(piece) == BLACK {
                return None;
            }

            let index = p_index!(piece) as usize;
            let opening = derive_piece_value(state, piece, opening_occupancy);
            let endgame = derive_piece_value(state, piece, endgame_occupancy);

            Some((index, opening, endgame))
        })
        .collect::<Vec<_>>();

    let offset = values
        .iter()
        .map(|(_, opening, _)| *opening)
        .fold(f64::INFINITY, f64::min) - 100.0;

    let peak = values
        .iter()
        .map(|(_, opening, endgame)| opening.max(*endgame))
        .fold(f64::NEG_INFINITY, f64::max) - offset;

    let squeeze = if peak > MAX_PIECE_VALUE as f64 {                            /* a board wide enough prices a piece */
        (MAX_PIECE_VALUE as f64 - 100.0) / (peak - 100.0)                       /* past the field it has to land in   */
    } else {
        1.0
    };

    for (index, opening, endgame) in values {
        let black_index = state.statics.piece_swap_map[index] as usize;
        let white_index = index;
        let ovalue =
            (100.0 + (opening - offset - 100.0) * squeeze).round() as u16;
        let evalue =
            (100.0 + (endgame - offset - 100.0) * squeeze).round() as u16;

        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[black_index],
            ovalue, evalue, false, false
        );
        set_piece_dynamic_parameters(
            &mut state.static_mut().pieces[white_index],
            ovalue, evalue, false, false
        );
    }
}

/// derive_eval_parameters
///
/// Derives the material from the rules, then rebuilds all products of the
/// material with `derive_eval_products`. A tuned file writes the material
/// and calls only the second step. Thus the roles, thresholds, contempt
/// and tables come from the current material.
///
/// Params:
/// - state: &mut State -> variant with the evaluation parameters to derive
///
pub fn derive_eval_parameters(state: &mut State) {
    derive_material_values(state);
    derive_eval_products(state);
}

/// derive_eval_products
///
/// Rebuilds the roles, phase thresholds, draw contempt and piece-square
/// tables after the final material load, derived or tuned. All values are
/// parts of the mean value of the start army, without royals:
///
/// - opening end : the mean, once for each big piece of the army
/// - endgame end : the mean times `ENDGAME_ARMY_SIZE`, below the opening
/// - span        : two times the mean, the contempt lead scale
/// - contempt    : 1/8 of the mean
///
/// Params:
/// - state: &mut State -> variant with the products to rebuild
///
/// Notes:
/// The endgame end is at most the opening end minus one, because the taper
/// divides by the difference. The roles come before the army walk, because
/// the walk copy counts pieces by role.
///
#[hotpath::measure]
pub fn derive_eval_products(state: &mut State) {
    log_3!("Deriving dynamic evaluation parameters...");

    let piece_roles = derive_piece_roles(state);

    for (piece_index, is_big, is_major) in piece_roles {
        let piece = &mut state.static_mut().pieces[piece_index as usize];
        let ovalue = p_ovalue!(piece);
        let evalue = p_evalue!(piece);

        set_piece_dynamic_parameters(
            piece, ovalue, evalue, is_big, is_major
        );
    }

    let start_army = resolve_setup_army(state);

    let mut deployed_value = 0u64;
    let mut deployed_count = 0u64;
    let mut deployed_big = 0u64;

    for (piece_index, piece) in state.statics.pieces.iter().enumerate() {
        if p_is_royal!(piece) {
            continue;
        }

        let deployed = start_army[piece_index] as u64;

        deployed_value += p_ovalue!(piece) as u64 * deployed;
        deployed_count += deployed;
        deployed_big += deployed * p_is_big!(piece) as u64;
    }

    let mean_value = deployed_value.checked_div(deployed_count).unwrap_or(0);

    log_3!(
        "Mean deployed non-royal value: {} over {} pieces, {} of them big",
        mean_value, deployed_count, deployed_big
    );

    let opening_score = (mean_value * deployed_big).max(1);                     /* a variant with no big army still   */
    let endgame_score =                                                         /* needs a positive taper divisor     */
        (mean_value * ENDGAME_ARMY_SIZE as u64)
            .min(opening_score - 1);

    let draw_span = (mean_value * DRAW_CONTEMPT_SPAN as u64).max(1);            /* a span of nothing still divides    */
    let draw_contempt = mean_value * DRAW_CONTEMPT_RATIO as u64
        / COEFFICIENT_SCALE as u64;

    state.static_mut().opening_score = opening_score as u32;
    state.static_mut().endgame_score = endgame_score as u32;
    state.static_mut().eval.draw_span = draw_span as i32;
    state.static_mut().eval.draw_contempt = draw_contempt as i32;

    derive_royal_stand_ins(state, &start_army);

    let (pst_opening, pst_endgame) = derive_base_pst(state);
    state.static_mut().pst_opening = pst_opening;
    state.static_mut().pst_endgame = pst_endgame;
    refresh_eval_state(state);

    log_3!("Derived Opening Score Threshold: {}", state.statics.opening_score);
    log_3!("Derived Endgame Score Threshold: {}", state.statics.endgame_score);
    log_3!(
        "Derived Draw Contempt: {} at a lead of {}",
        state.statics.eval.draw_contempt, state.statics.eval.draw_span
    );
}

/*----------------------------------------------------------------------------*\
                            ROYAL SAFETY DERIVATION
\*----------------------------------------------------------------------------*/

/// derive_royal_stand_ins
///
/// Sets the `lone` piece of the `extinct` rules. A candidate is not royal,
/// but its capture ends the game: it is the only piece of its colour in
/// the set of a rule that loses at zero. For each colour, the cheapest
/// candidate stands in for the royal: the royal terms of the evaluation
/// read it, so the king of an extinction variant keeps its shelter, and a
/// dear piece stays active.
///
/// - extinction chess : the king (the queen is dearer)
/// - kinglet          : none, eight pawns in the set
/// - standard         : none, no extinct rule
///
/// Params:
/// - state     : &mut State -> variant with the end rules
/// - start_army: &[u32]     -> count of each piece index at the start
///
fn derive_royal_stand_ins(state: &mut State, start_army: &[u32]) {
    let mut cheapest: [Option<(usize, usize)>; 2] = [None; 2];

    for (rule_index, rule) in state.termination.extinct.iter().enumerate() {
        if rule.threshold != 0 || rule.outcome != Outcome::Loss {
            continue;
        }

        for color in [WHITE, BLACK] {
            let members: Vec<usize> = state.statics.pieces.iter()
                .enumerate()
                .filter(|(index, piece)| {
                    rule.set[*index] && p_color!(piece) == color
                })
                .map(|(index, _)| index)
                .collect();
            let army = members.iter()
                .map(|index| start_army[*index])
                .sum::<u32>();

            for index in members {
                let piece = &state.statics.pieces[index];
                let cheaper = cheapest[color as usize]
                    .is_none_or(|(_, best)| {
                        p_ovalue!(piece)
                            < p_ovalue!(&state.statics.pieces[best])
                    });

                if army == 1
                    && start_army[index] == 1
                    && !p_is_royal!(piece)
                    && cheaper {
                    cheapest[color as usize] = Some((rule_index, index));
                }
            }
        }
    }

    for rule in &mut state.termination.extinct {
        rule.lone = [None; 2];
    }

    for (color, choice) in cheapest.into_iter().enumerate() {
        if let Some((rule_index, index)) = choice {
            state.termination.extinct[rule_index].lone[color] = Some(index);
        }
    }
}

/// derive_forward_directions
///
/// Gives the forward rank step of each colour from the start army. The
/// side on the lower ranks moves up.
///
/// - Black lower : Black goes up, White goes down
/// - other       : White goes up, Black goes down, also on a tie
///
/// Params:
/// - state: &State -> variant with the start army
///
/// Return:
/// [i32; 2]        -> rank step of each colour, 1 or -1
///
fn derive_forward_directions(state: &State) -> [i32; 2] {
    let files = state.statics.files as usize;
    let board_size = state.statics.board_size;

    let mut ranks = [0i64; 2];
    let mut counts = [0i64; 2];

    for (piece_index, piece) in state.statics.pieces.iter().enumerate() {
        let color = p_color!(piece) as usize;
        let setup = &state.statics.initial_setup[piece_index];

        for square in 0..board_size {
            if get!(setup, square as u32) {
                ranks[color] += (square / files) as i64;
                counts[color] += 1;
            }
        }
    }

    let white = ranks[WHITE as usize]
        .checked_div(counts[WHITE as usize])
        .unwrap_or(0);
    let black = ranks[BLACK as usize]
        .checked_div(counts[BLACK as usize])
        .unwrap_or(1);

    if black < white {
        [-1, 1]
    } else {
        [1, -1]
    }
}

/// derive_shield_pieces
///
/// Marks the shield piece types: pieces that are not royal, stay near
/// their square and move forward more than back. A pawn, a gold and a
/// silver are shields.
///
/// - inside the radius : any step is allowed
/// - one square past   : only a straight double step from the start rank
/// - further           : not a shield, the piece leaps away
/// - rank offsets      : the sum must be positive
///
/// Params:
/// - state : &State -> precomputed move tables
/// - radius: i32    -> radius of the local square lists
///
/// Return:
/// Vec<bool>        -> shield role of each piece index
///
/// Notes:
/// The move vectors are in the frame of the piece owner, so a positive
/// rank offset is forward for the two colours.
///
fn derive_shield_pieces(state: &State, radius: i32) -> Vec<bool> {
    state.statics.pieces.iter().map(|piece| {
        if p_is_royal!(piece) {
            return false;
        }

        let offsets = derive_piece_offsets(state, piece);

        if offsets.is_empty() {
            return false;
        }

        let local = offsets.iter().all(|(file_offset, rank_offset)| {
            let reach = file_offset.abs().max(rank_offset.abs());

            reach <= radius
                || (reach == radius + 1 && *file_offset == 0)                   /* a straight double step counts too  */
        });
        let lean: i32 = offsets
            .iter()
            .map(|(_, rank_offset)| rank_offset)
            .sum();

        local && lean > 0
    }).collect()
}

/// derive_royal_confinement
///
/// Tells for each colour if its royals are in a small area because of
/// their forbidden zones. The reach comes from the zone bitboard.
///
/// - a quarter or less : confined, shelter is off for that colour
/// - more              : free, shelter is on
///
/// Params:
/// - state: &State -> variant with the royal zones
///
/// Return:
/// [bool; 2]       -> for each colour, true when confined
///
/// Notes:
/// A colour is confined if one of its royals is confined.
///
fn derive_royal_confinement(state: &State) -> [bool; 2] {
    let board_size = state.statics.board_size;
    let mut confined = [false; 2];

    for piece in &state.statics.pieces {
        if !p_is_royal!(piece) {
            continue;
        }

        let zone = &state.statics.forbidden_zones[p_index!(piece) as usize];
        let reach = (0..board_size)
            .filter(|square| !get!(zone, *square as u32))
            .count();

        confined[p_color!(piece) as usize] |=
            reach * SHELTER_CONFINEMENT_DIVISOR <= board_size;
    }

    confined
}

/// derive_shelter_parameters
///
/// Makes the tables of the shelter, guard and castling terms:
///
/// - shelter list : for each colour, the ring squares in front of the royal
/// - ring list    : the full ring, the same for the two colours
/// - counts       : the number of ring squares on the board, each origin
///
/// Each list has `stride` slots for each origin. `stride` is the full ring
/// size, and the count gives the real squares. Thus the evaluation needs
/// no bounds test:
///
/// ```text
/// centre   s s s   eight ring squares, and the count reads eight
///          s K s
///          s s s
/// corner   s s     three of them land on the board, the count reads
///          K s     three, and the rest of the row is never touched
/// ```
///
/// The ring does not include the origin, so the stride is the square of the
/// ring width minus one.
///
/// Params:
/// - state: &mut State -> variant with the shelter tables to rebuild
///
/// Notes:
/// The four values are parts of the most valuable piece. Confinement turns
/// off only the shelter. The guard pieces of a palace royal still block
/// attack lines.
///
#[hotpath::measure]
pub fn derive_shelter_parameters(state: &mut State) {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let radius = SHELTER_RADIUS as i32;
    let stride = (2 * radius + 1).pow(2) as usize - 1;                          /* the origin itself is never stored  */

    let forward = derive_forward_directions(state);
    let shield_pieces = derive_shield_pieces(state, radius);

    let mut shelter_squares = [
        vec![0 as Square; board_size * stride],
        vec![0 as Square; board_size * stride]
    ];
    let mut shelter_counts = [vec![0u8; board_size], vec![0u8; board_size]];
    let mut ring_squares = vec![0 as Square; board_size * stride];
    let mut ring_counts = vec![0u8; board_size];

    for square in 0..board_size {
        let file = square as i32 % files;
        let rank = square as i32 / files;

        for rank_offset in -radius..=radius {
            for file_offset in -radius..=radius {
                let local_file = file + file_offset;
                let local_rank = rank + rank_offset;

                if (file_offset, rank_offset) == (0, 0)
                    || local_file < 0 || local_file >= files
                    || local_rank < 0 || local_rank >= ranks {
                    continue;
                }

                let local = (local_rank * files + local_file) as Square;
                let ring_slot = ring_counts[square] as usize;

                ring_squares[square * stride + ring_slot] = local;
                ring_counts[square] += 1;

                for color in [WHITE as usize, BLACK as usize] {
                    if rank_offset * forward[color] <= 0 {
                        continue;
                    }

                    let slot = shelter_counts[color][square] as usize;

                    shelter_squares[color][square * stride + slot] = local;
                    shelter_counts[color][square] += 1;
                }
            }
        }
    }

    let confined = derive_royal_confinement(state);

    for color in [WHITE as usize, BLACK as usize] {
        if confined[color] {
            shelter_counts[color] = vec![0u8; board_size];                      /* a walled royal reads no squares    */
        }
    }

    let dearest = dearest_piece_value(state);

    let shelter_value = (dearest * SHELTER_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(SHELTER_FLOOR as u64);
    let guard_value = (dearest * GUARD_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(GUARD_FLOOR as u64);
    let castled_value = dearest * CASTLED_RATIO as u64
        / COEFFICIENT_SCALE as u64;
    let castling_right_value = dearest * CASTLING_RIGHT_RATIO as u64
        / COEFFICIENT_SCALE as u64;

    log_3!(
        concat!(
            "Derived shelter worth {} per piece and {} per guard, {} of ",
            "{} piece types shield-like, royals confined {:?}"
        ),
        shelter_value,
        guard_value,
        shield_pieces.iter().filter(|shield| **shield).count(),
        shield_pieces.len(),
        confined
    );

    log_3!(
        "Derived Castling Worth: {} castled, {} holding the right",
        castled_value,
        castling_right_value,
    );

    let statics = state.static_mut();

    statics.eval.shield_pieces = shield_pieces;
    statics.eval.shelter_squares = shelter_squares;
    statics.eval.shelter_counts = shelter_counts;
    statics.eval.ring_squares = ring_squares;
    statics.eval.ring_counts = ring_counts;
    statics.eval.local_stride = stride;
    statics.eval.forward_steps = forward;
    statics.eval.shelter_value = shelter_value as i32;
    statics.eval.guard_value = guard_value as i32;
    statics.eval.castled_value = castled_value as i32;
    statics.eval.castling_right_value = castling_right_value as i32;
}

/// derive_danger_parameters
///
/// Makes the zone attack tables of `king_danger!` and the costs of
/// `king_danger!` and `open_shield!`. For each piece, origin square and
/// royal square, the table has the expected number of moves of that piece
/// onto the royal square or its ring. It uses the opening occupancy. Thus
/// the evaluation only adds bytes for the enemy pieces.
///
/// - landing square : gets the chance of the vector to arrive there
/// - ring squares   : each gets the same chance again
/// - index          : `(royal * pieces + piece) * squares + origin`
/// - best row       : the maximum over the origins, for pieces in hand
///
/// An entry is in units of `1 / ZONE_ATTACK_UNIT` of an expected move, and
/// stops at the byte maximum.
///
/// Params:
/// - state: &mut State -> variant with the danger tables to rebuild
///
/// Notes:
/// A piece in hand has no square, so it reads `zone_attack_best`. The
/// function uses the ring of `derive_shelter_parameters`, so that function
/// must run first.
///
#[hotpath::measure]
pub fn derive_danger_parameters(state: &mut State) {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let piece_count = state.statics.pieces.len();
    let stride = state.statics.eval.local_stride;
    let occupancy = OPENING_OCCUPANCY as f64 / COEFFICIENT_SCALE;

    let mut table = vec![0u8; board_size * piece_count * board_size];

    for piece_index in 0..piece_count {
        for from in 0..board_size {
            let mut pressure = vec![0.0f64; board_size];
            let vectors = &state.statics.relevant_moves
                [piece_index * board_size + from];

            for vector in vectors {
                let Some((chance, file_delta, rank_delta)) =
                    derive_vector_chance(state, vector, occupancy)
                else {
                    continue;
                };

                let file = from as i32 % files + file_delta;
                let rank = from as i32 / files + rank_delta;

                if file < 0 || file >= files || rank < 0 || rank >= ranks {
                    continue;
                }

                let landing = (rank * files + file) as usize;

                pressure[landing] += chance;

                let ring_count = state.statics.eval.ring_counts[landing];

                for slot in 0..ring_count as usize {
                    let ring = state.statics.eval.ring_squares[
                        landing * stride + slot
                    ] as usize;

                    pressure[ring] += chance;
                }
            }

            for (royal, chance) in pressure.iter().enumerate() {
                table[
                    (royal * piece_count + piece_index) * board_size + from
                ] = (chance * ZONE_ATTACK_UNIT as f64)
                    .round()
                    .min(u8::MAX as f64) as u8;
            }
        }
    }

    let mut best = vec![0u8; board_size * piece_count];

    for (entry, pressure) in best.iter_mut().enumerate() {
        *pressure = *table[entry * board_size..(entry + 1) * board_size]
            .iter()
            .max()
            .unwrap_or(&0);
    }

    let dearest = dearest_piece_value(state);

    let king_danger_scale = dearest * DANGER_RATIO as u64
        / COEFFICIENT_SCALE as u64;
    let king_danger_cap = dearest * DANGER_CAP_RATIO as u64
        / COEFFICIENT_SCALE as u64;
    let open_shield_penalty = (dearest * OPEN_SHIELD_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(OPEN_SHIELD_FLOOR as u64);
    let proximity_value = dearest * PROXIMITY_RATIO as u64
        * drops!(state) as u64 / COEFFICIENT_SCALE as u64;
    let check_value = dearest * CHECK_RATIO as u64 / COEFFICIENT_SCALE as u64;

    log_3!(
        concat!(
            "Derived king danger worth {} at {} landings, capped at {}, ",
            "and an uncovered royal at {}"
        ),
        king_danger_scale,
        ZONE_ATTACK_FULL,
        king_danger_cap,
        open_shield_penalty
    );

    if let Some(checks) = state.termination.checks.as_mut() {
        checks.value = check_value as i32;
    }

    let statics = state.static_mut();

    statics.eval.zone_attack = table;
    statics.eval.zone_attack_best = best;
    statics.eval.king_danger_scale = king_danger_scale as i32;
    statics.eval.king_danger_cap = king_danger_cap as i32;
    statics.eval.open_shield_penalty = open_shield_penalty as i32;
    statics.eval.proximity_value = proximity_value as i32;
}

/*----------------------------------------------------------------------------*\
                           PAWN STRUCTURE DERIVATION
\*----------------------------------------------------------------------------*/

/// derive_pawn_slots
///
/// Finds the pawn piece types and gives each one a slot in the pawn
/// tables. A pawn passes all four tests:
///
/// - never backward : no vector has a negative rank
/// - steps forward  : a quiet one-rank move, file change at most one
/// - short range    : no move longer than one square after the first move
/// - many copies    : at least `PAWN_MIN_START_COUNT`, board and hand
///
/// The step test rejects the shogi knight and lance. The backward test
/// rejects the gold, silver, advisor and elephant. The count rejects a
/// single forward stepper. The first move can be longer, so the FIDE pawn
/// keeps its double step.
///
/// Params:
/// - state: &State          -> variant with the pieces to test
///
/// Return:
/// (Vec<usize>, Vec<usize>) -> piece index to slot, slot to piece index
///
/// Notes:
/// The function tests White, and the Black pair gets the same result. The
/// slots are in piece order. The tables have one row for each slot, not
/// for each piece, so they stay small.
///
fn derive_pawn_slots(state: &State) -> (Vec<usize>, Vec<usize>) {
    let board_size = state.statics.board_size;
    let piece_count = state.statics.pieces.len();

    let mut slots = vec![NO_PAWN; piece_count];
    let mut pieces = Vec::new();

    for piece in state.statics.pieces.iter() {
        if p_color!(piece) != WHITE || p_is_royal!(piece) {
            continue;
        }

        let index = p_index!(piece) as usize;
        let color = p_color!(piece) as usize;
        let mut steps_backward = false;
        let mut steps_forward = false;
        let mut ranges_far = false;

        for square in 0..board_size {
            let vectors =
                &state.statics.relevant_moves[index * board_size + square];

            for vector in usual_vectors(state, vectors) {
                let (file_offset, rank_offset) = vector_offset!(vector);
                let quiet = vector_moves_quietly!(vector);

                steps_backward |= rank_offset < 0;
                steps_forward |=
                    quiet && rank_offset == 1 && file_offset.abs() <= 1;
                ranges_far |= quiet
                    && !vector_is_initial!(vector)
                    && (rank_offset > 1 || file_offset.abs() > 1);
            }
        }

        let fielded = count_bits!(state.statics.initial_setup[index]) as usize
            + state.piece_in_hand[color][index] as usize;

        if steps_backward
            || !steps_forward
            || ranges_far
            || fielded < PAWN_MIN_START_COUNT {
            continue;
        }

        for side in [index, state.statics.piece_swap_map[index] as usize] {
            if side == NO_PIECE as usize || slots[side] != NO_PAWN {
                continue;
            }

            slots[side] = pieces.len();
            pieces.push(side);
        }
    }

    (slots, pieces)
}

/// derive_pawn_path
///
/// Gives all squares that a pawn on `square` can reach with quiet moves.
/// An own pawn on this path is doubled. An enemy pawn on it stops a passed
/// pawn. The walk follows the moves of the piece, so a diagonal pawn gets
/// more files:
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │    │ ## │    │    │   ## = a square still ahead of the pawn
/// ├────┼────┼────┼────┼────┤
/// │    │    │ ## │    │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ ## │    │    │
/// ├────┼────┼────┼────┼────┤
/// │    │    │ PP │    │    │   PP = the pawn, pushing straight
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - state : &State -> precomputed move tables
/// - index : usize  -> pawn piece index
/// - square: usize  -> square of the pawn
///
/// Return:
/// Board            -> squares on the forward path of the pawn
///
fn derive_pawn_path(state: &State, index: usize, square: usize) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut path = board!(state.statics.files, state.statics.ranks);
    let mut pending = VecDeque::new();
    pending.push_back(square);

    while let Some(current) = pending.pop_front() {
        let vectors =
            &state.statics.relevant_moves[index * board_size + current];

        for vector in usual_vectors(state, vectors) {
            if !vector_moves_quietly!(vector) {
                continue;
            }

            let (file_offset, rank_offset) = vector_offset!(vector);
            let file = current as i32 % files + file_offset * sign;
            let rank = current as i32 / files + rank_offset * sign;

            if file < 0 || file >= files || rank < 0 || rank >= ranks {
                continue;
            }

            let next = (rank * files + file) as usize;

            if !get!(path, next as u32) {
                set!(path, next as u32);
                pending.push_back(next);
            }
        }
    }

    path
}

/// derive_pawn_stop
///
/// Gives the squares that a pawn reaches in one normal step: quiet, not a
/// first move only, and forward. First moves of two or more squares and
/// sideways steps are not stops. An own pawn that defends a stop connects
/// the pawn. An enemy pawn that attacks a stop holds it back:
///
/// ```text
/// ┌────┬────┬────┐          ┌────┬────┬────┐
/// │    │ ** │    │          │ ** │    │ ** │
/// ├────┼────┼────┤          ├────┼────┼────┤
/// │    │ PP │    │          │    │ PP │    │
/// └────┴────┴────┘          └────┴────┴────┘
///    straight mover            diagonal mover
/// ```
///
/// Params:
/// - state : &State -> precomputed move tables
/// - index : usize  -> pawn piece index
/// - square: usize  -> square of the pawn
///
/// Return:
/// Board            -> the forward stop squares of the pawn
///
fn derive_pawn_stop(state: &State, index: usize, square: usize) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut stop = board!(state.statics.files, state.statics.ranks);
    let vectors = &state.statics.relevant_moves[index * board_size + square];

    for vector in usual_vectors(state, vectors) {
        if !vector_moves_quietly!(vector) || vector_is_initial!(vector) {
            continue;
        }

        let (file_offset, rank_offset) = vector_offset!(vector);

        if rank_offset < 1 {
            continue;
        }

        let file = square as i32 % files + file_offset * sign;
        let rank = square as i32 / files + rank_offset * sign;

        if file < 0 || file >= files || rank < 0 || rank >= ranks {
            continue;
        }

        set!(stop, (rank * files + file) as u32);
    }

    stop
}

/// derive_pawn_captures
///
/// Gives all squares from which a pawn of `color` can capture onto one of
/// `targets`. The colour sign turns each capture offset.
///
/// - own colour, pawn and stops : the squares that connect the pawn
/// - enemy colour, stops only   : the squares that hold it back
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │    │ TT │    │    │   TT = a requested target
/// ├────┼────┼────┼────┼────┤
/// │    │ SS │    │ SS │    │   SS = a square that captures onto it
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - state  : &State -> precomputed capture tables
/// - color  : u8     -> colour of the capturing pawns
/// - targets: &Board -> target squares of the captures
///
/// Return:
/// Board             -> origin squares of such captures
///
fn derive_pawn_captures(state: &State, color: u8, targets: &Board) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * color as i32 + 1;

    let mut sources = board!(state.statics.files, state.statics.ranks);

    for index in 0..state.statics.pieces.len() {
        if state.statics.eval.pawn_slots[index] == NO_PAWN
            || p_color!(&state.statics.pieces[index]) != color {
            continue;
        }

        for source in 0..board_size {
            let vectors = &state.statics.relevant_captures
                [index * board_size + source];

            for vector in usual_vectors(state, vectors) {
                let (file_offset, rank_offset) = vector_offset!(vector);
                let file = source as i32 % files + file_offset * sign;
                let rank = source as i32 / files + rank_offset * sign;

                if file < 0 || file >= files || rank < 0 || rank >= ranks {
                    continue;
                }

                if get!(targets, (rank * files + file) as u32) {
                    set!(sources, source as u32);
                }
            }
        }
    }

    sources
}

/// derive_pawn_interference
///
/// Gives all enemy squares that can stop the pawn: the path, and each
/// square from which an enemy pawn can capture onto the path or the pawn.
/// A pawn with no enemy pawn on these squares is passed:
///
/// ```text
/// ┌────┬────┬────┬────┬────┐
/// │    │ xx │ ## │ xx │    │   ## = a blocker standing on the path
/// ├────┼────┼────┼────┼────┤
/// │    │ xx │ ## │ xx │    │   xx = a capture into the path
/// ├────┼────┼────┼────┼────┤
/// │    │    │ PP │    │    │   PP = the pawn
/// └────┴────┴────┴────┴────┘
/// ```
///
/// Params:
/// - state : &State -> precomputed capture tables
/// - index : usize  -> pawn piece index
/// - square: usize  -> square of the pawn
/// - path  : &Board -> forward path of the pawn
///
/// Return:
/// Board            -> enemy squares that stop the passed pawn
///
fn derive_pawn_interference(
    state: &State, index: usize, square: usize, path: &Board
) -> Board {
    let enemy = 1 - p_color!(&state.statics.pieces[index]);

    let mut reached = *path;
    set!(reached, square as u32);

    let mut mask = derive_pawn_captures(state, enemy, &reached);
    or!(mask, path);

    mask
}

/// derive_pawn_support_files
///
/// Gives the file offsets from which an own pawn can defend the pawn or
/// its stop square. A pawn with no own pawn on these files, on any rank,
/// is isolated.
///
/// - behind : the file of each capture leg
/// - beside : each capture leg combined with each forward step
///
/// FIDE gives {-1, +1}, Berolina {-1, 0, +1} and shogi {0}:
///
/// ```text
///       FIDE               Berolina               Shogi
/// ┌────┬────┬────┐     ┌────┬────┬────┐     ┌────┬────┬────┐
/// │ oo │    │ oo │     │ oo │ oo │ oo │     │    │ oo │    │
/// ├────┼────┼────┤     ├────┼────┼────┤     ├────┼────┼────┤
/// │    │ PP │    │     │    │ PP │    │     │    │ PP │    │
/// └────┴────┴────┘     └────┴────┴────┘     └────┴────┴────┘
/// ```
///
/// Params:
/// - state: &State -> precomputed move and capture tables
/// - index: usize  -> pawn piece index
///
/// Return:
/// Vec<i32>        -> sorted support file offsets, no duplicates
///
fn derive_pawn_support_files(state: &State, index: usize) -> Vec<i32> {
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut capture_files: Vec<i32> = Vec::new();
    let mut step_files: Vec<i32> = Vec::new();

    for square in 0..board_size {
        let captures =
            &state.statics.relevant_captures[index * board_size + square];

        for vector in usual_vectors(state, captures) {
            let capture_file = vector_offset!(vector).0;

            if !capture_files.contains(&capture_file) {
                capture_files.push(capture_file);
            }
        }

        let moves =
            &state.statics.relevant_moves[index * board_size + square];

        for vector in usual_vectors(state, moves) {
            if !vector_moves_quietly!(vector) || vector_is_initial!(vector) {
                continue;
            }

            let (step_file, rank_offset) = vector_offset!(vector);

            if rank_offset < 1 || step_files.contains(&step_file) {
                continue;
            }

            step_files.push(step_file);
        }
    }

    let mut offsets: Vec<i32> = Vec::new();

    for capture_file in capture_files.iter() {
        for offset in step_files.iter()
            .map(|step_file| (step_file - capture_file) * sign)
            .chain([-capture_file * sign]) {
            if !offsets.contains(&offset) {
                offsets.push(offset);
            }
        }
    }

    offsets.sort();
    offsets
}

/// derive_pawn_advancement
///
/// Gives the promotion progress of a pawn on `square`, squared, in 256ths.
/// It is the same gradient as in the piece-square tables. On eight ranks:
///
/// ```text
/// steps to promote    8    6    4    2    1
/// advancement         0   16   64  144  196
/// ```
///
/// Params:
/// - state         : &State -> board geometry and promotion zones
/// - index         : usize  -> pawn piece index
/// - square        : usize  -> square of the pawn
/// - closest       : f64    -> promotion distance from this square
/// - promotion_span: f64    -> spread of that distance on the board
///
/// Return:
/// i32                      -> progress in 256ths, squared
///
/// Notes:
/// Without a zone, or with a zone on the full board, the progress is the
/// distance from the far edge in the forward direction of the pawn.
///
fn derive_pawn_advancement(
    state: &State, index: usize, square: usize, closest: f64,
    promotion_span: f64
) -> i32 {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;

    let advancement = if closest.is_finite() && promotion_span > 0.0 {
        (1.0 - closest / ranks as f64).max(0.0)
    } else {
        let edge = (ranks - 1)
            * (p_color!(&state.statics.pieces[index]) == WHITE) as i32;
        let span = (ranks - 1).max(1);

        (1.0 - (edge - square as i32 / files).abs() as f64 / span as f64)
            .max(0.0)
    };

    (advancement * advancement * 256.0) as i32
}

/// derive_pawn_parameters
///
/// Makes all tables of `pawn_structure!`: the pawn pieces, four masks for
/// each pawn and square, the passed pawn values, and the structure values.
///
/// - path         : squares in front, an own pawn here is doubled
/// - interference : enemy squares that stop it, all empty means passed
/// - support      : own squares that connect it or defend its stop
/// - backward     : enemy squares that attack its stop
///
/// Each mask is a full board, so each test is one bit read. The tables use
/// the pawn slot, not the piece index, so they stay small. The values are:
///
/// - passed    : promotion gain, times the ratio, times the progress
/// - connected : part of the pawn value, separate for each phase
/// - doubled   : part of the opening pawn value, the same in both phases
/// - isolated  : the same part, from the support files
/// - backward  : the same part, from the attacked stop
///
/// Params:
/// - state: &mut State -> variant with the pawn tables to rebuild
///
/// Notes:
/// A passed pawn one square from promotion is worth most of a piece. A pawn
/// that cannot promote uses its own value. Only the connected term has an
/// endgame value. A fault is not less bad in the endgame.
///
#[hotpath::measure]
pub fn derive_pawn_parameters(state: &mut State) {
    let board_size = state.statics.board_size;

    let (slots, pieces) = derive_pawn_slots(state);
    let stride = board_size * (!pieces.is_empty()) as usize;

    state.static_mut().eval.pawn_slots = slots;
    state.static_mut().eval.pawn_pieces = pieces.clone();
    state.static_mut().eval.pawn_stride = stride;

    let empty = board!(state.statics.files, state.statics.ranks);
    let mut path = vec![empty; pieces.len() * stride];
    let mut interference = vec![empty; pieces.len() * stride];
    let mut support = vec![empty; pieces.len() * stride];
    let mut backward = vec![empty; pieces.len() * stride];
    let mut support_files = vec![Vec::new(); pieces.len()];
    let mut passed_opening = vec![0i32; pieces.len() * stride];
    let mut passed_endgame = vec![0i32; pieces.len() * stride];

    for (slot, index) in pieces.iter().copied().enumerate() {
        let piece = &state.statics.pieces[index];
        let promoted_opening =
            derive_promotion_target(state, index as PieceIndex, false);
        let promoted_endgame =
            derive_promotion_target(state, index as PieceIndex, true);
        let enemy = 1 - p_color!(piece);
        let opening_value = p_ovalue!(piece) as i32;
        let endgame_value = p_evalue!(piece) as i32;
        let promotes = p_can_promote!(piece);

        let (opening_gain, opening_ratio) = match promotes {
            true => (promoted_opening - opening_value, PASSED_OPENING_RATIO),
            false => (opening_value, PASSED_UNPROMOTED_RATIO),
        };
        let (endgame_gain, endgame_ratio) = match promotes {
            true => (promoted_endgame - endgame_value, PASSED_ENDGAME_RATIO),
            false => (endgame_value, PASSED_UNPROMOTED_RATIO),
        };

        support_files[slot] = derive_pawn_support_files(state, index);

        let promotion_field =                                                  /* read the zone once, not per square */
            derive_promotion_field(state, index as PieceIndex);
        let promotion_span = derive_promotion_span(&promotion_field);

        for square in 0..board_size {
            let entry = slot * stride + square;
            let stop = derive_pawn_stop(state, index, square);
            let advancement = derive_pawn_advancement(
                state, index, square, promotion_field[square], promotion_span
            ) as i64;

            let mut defended = stop;
            set!(defended, square as u32);

            path[entry] = derive_pawn_path(state, index, square);
            interference[entry] =
                derive_pawn_interference(state, index, square, &path[entry]);
            support[entry] =
                derive_pawn_captures(state, p_color!(piece), &defended);
            backward[entry] = derive_pawn_captures(state, enemy, &stop);

            passed_opening[entry] = (opening_gain.max(0) as i64
                * opening_ratio as i64 * advancement
                / (COEFFICIENT_SCALE as i64 * 256)) as i32;
            passed_endgame[entry] = (endgame_gain.max(0) as i64
                * endgame_ratio as i64 * advancement
                / (COEFFICIENT_SCALE as i64 * 256)) as i32;
        }
    }

    let share = |value: i32, ratio: u32| -> i32 {
        (value as i64 * ratio as i64 / COEFFICIENT_SCALE as i64) as i32
    };

    let opening_values: Vec<i32> = pieces.iter()
        .map(|index| p_ovalue!(&state.statics.pieces[*index]) as i32)
        .collect();
    let endgame_values: Vec<i32> = pieces.iter()
        .map(|index| p_evalue!(&state.statics.pieces[*index]) as i32)
        .collect();

    let connected_opening: Vec<i32> = opening_values.iter()
        .map(|value| share(*value, PAWN_CONNECTED_OPENING_RATIO))
        .collect();
    let connected_endgame: Vec<i32> = endgame_values.iter()
        .map(|value| share(*value, PAWN_CONNECTED_ENDGAME_RATIO))
        .collect();
    let doubled: Vec<i32> = opening_values.iter()
        .map(|value| share(*value, PAWN_DOUBLED_RATIO))
        .collect();
    let isolated: Vec<i32> = opening_values.iter()
        .map(|value| share(*value, PAWN_ISOLATED_RATIO))
        .collect();
    let backward_penalty: Vec<i32> = opening_values.iter()
        .map(|value| share(*value, PAWN_BACKWARD_RATIO))
        .collect();

    log_3!(
        concat!(
            "Derived {} pawn types {:?}, connected {:?} then {:?}, ",
            "doubled {:?}, isolated {:?}, backward {:?}"
        ),
        pieces.len(),
        pieces.iter()
            .map(|index| state.statics.pieces[*index].char)
            .collect::<Vec<char>>(),
        connected_opening,
        connected_endgame,
        doubled,
        isolated,
        backward_penalty
    );

    let statics = state.static_mut();

    statics.eval.pawn_path = path;
    statics.eval.pawn_interference = interference;
    statics.eval.pawn_support = support;
    statics.eval.pawn_backward = backward;
    statics.eval.pawn_support_files = support_files;
    statics.eval.pawn_passed_opening = passed_opening;
    statics.eval.pawn_passed_endgame = passed_endgame;
    statics.eval.pawn_connected_opening = connected_opening;
    statics.eval.pawn_connected_endgame = connected_endgame;
    statics.eval.pawn_doubled_penalty = doubled;
    statics.eval.pawn_isolated_penalty = isolated;
    statics.eval.pawn_backward_penalty = backward_penalty;
}

/*----------------------------------------------------------------------------*\
                              ADVANTAGE DERIVATION
\*----------------------------------------------------------------------------*/

/// derive_advantage_parameters
///
/// Gives the three advantages that material does not include:
///
/// - tempo     : part of the most valuable piece, with a floor
/// - imbalance : one part for a major surplus, one for a minor surplus
/// - pair      : a part again, only for pieces that reach half the board
///
/// The pair test uses the piece reach, not the name. Royals are not
/// tested. The list has the two colours of each pair piece, so the
/// evaluation needs no swap map.
///
/// Params:
/// - state: &mut State -> variant with the advantage values to fill
///
#[hotpath::measure]
pub fn derive_advantage_parameters(state: &mut State) {
    let dearest = dearest_piece_value(state);

    let tempo = (dearest * TEMPO_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(TEMPO_FLOOR as u64);
    let major = (dearest * IMBALANCE_MAJOR_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(IMBALANCE_MAJOR_FLOOR as u64);
    let minor = (dearest * IMBALANCE_MINOR_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(IMBALANCE_MINOR_FLOOR as u64);
    let pair = (dearest * PAIR_RATIO as u64
        / COEFFICIENT_SCALE as u64).max(PAIR_FLOOR as u64);

    let bound: Vec<usize> = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE)
        .filter(|piece| !p_is_royal!(piece))
        .filter(|piece|
            (derive_piece_reach(state, piece) - PAIR_REACH).abs()
                < PAIR_REACH_SLACK
        )
        .map(|piece| p_index!(piece) as usize)
        .collect();

    let pair_pieces: Vec<usize> = bound.iter()
        .flat_map(|index| [
            *index, state.statics.piece_swap_map[*index] as usize
        ])
        .collect();

    log_3!(
        "Derived tempo {}, imbalance {} then {}, pair {} for {:?}",
        tempo, major, minor, pair,
        bound.iter()
            .map(|index| state.statics.pieces[*index].char)
            .collect::<Vec<char>>()
    );

    let statics = state.static_mut();

    statics.eval.tempo_bonus = tempo as i32;
    statics.eval.imbalance_major = major as i32;
    statics.eval.imbalance_minor = minor as i32;
    statics.eval.pair_pieces = pair_pieces;
    statics.eval.pair_bonus = pair as i32;
}
