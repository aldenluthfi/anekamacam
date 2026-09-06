//! parameters.rs
//!
//! Automatic derivation of dynamic evaluation parameters for pieces.
//!
//! A variant-agnostic engine cannot ship hand-tuned material values or
//! piece-square tables: it meets each army for the first time at load. This
//! file closes that gap, turning a piece's movement geometry into the numbers
//! evaluation needs — a value from its board reach and mobility, a role from
//! where that value ranks in the army, and opening and endgame tables shaped
//! to the variant's board — so every variant is scored on its own terms.
//!
//! Everything here runs once, at load. What comes out is read on every node
//! of every search and never recomputed, so the cost of deriving carefully is
//! paid once and the saving is taken for the rest of the game.
//!
//! Created: 08/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                              DERIVATION CONSTANTS
\*----------------------------------------------------------------------------*/

/// Board occupancy assumed when valuing a piece: the fraction of squares
/// a slider expects to find blocked in each phase, which is what makes an
/// opening value differ from an endgame one.
///
/// - opening : 36% blocked, a slider is stopped early and reach is worth
///             less
/// - endgame : 12% blocked, the lines open and the same piece runs
///             further
const OPENING_OCCUPANCY: u32 = 360;
const ENDGAME_OCCUPANCY: u32 = 120;

/// Where the ranked non-royal army is cut into roles: the cheapest share
/// that is not big, and the dearest share that is major.
///
/// ```text
/// non-big         minor             major
/// ├───│───────────────────────────│──────┤
///    10%                         80%   100%
/// ```
///
/// A royal is asked for none of this: it is never traded, so where its
/// value would rank says nothing about what it does.
const ROLE_NON_BIG_SPLIT: u32 = 100;
const ROLE_MAJOR_SPLIT: u32 = 200;

/// How small the big non-royal army has to get before play counts as an
/// endgame, measured in pieces of average deployed value. The two ends of
/// the phase taper are set from this and from the opening army, so a
/// variant fielding a few heavy pieces and one fielding dozens both run
/// their taper over the material they actually have.
///
/// - opening : every big piece the setup deploys, at average value
/// - endgame : this many of them left, or one below opening if fewer
const ENDGAME_ARMY_SIZE: u32 = 5;

/// What a draw is worth to a side that is not level on material. A draw
/// agreed by the side already ahead gives up the lead it holds, so it is
/// scored below zero for that side and above zero for the other. The cost
/// rises with the lead and saturates once the lead reaches `SPAN` pieces
/// of average deployed value, where it is worth `RATIO` of one such piece
/// held against `COEFFICIENT_SCALE`.
///
/// - no lead   : nothing was given up, so a draw is worth zero
/// - one piece : half of it, half the lead handed back
/// - two       : the whole of it, 12.5% of an average piece
/// - beyond    : the same again, the charge saturates there
///
/// Both read the deployed mean rather
/// than the dearest piece, because a lead is an army's and not a single
/// exchange's, and both are shares of this variant's own material so a
/// variant playing in small units is not handed a large contempt.
const DRAW_CONTEMPT_RATIO: u32 = 125;
const DRAW_CONTEMPT_SPAN: u32 = 2;

/// Late-move reduction curves, one per class of move. Each surface is
/// `base + shape(depth, moves) / divisor`. Which terms a curve mixes is fixed
/// by class because a quiet move buried in a long list and a capture answering
/// check do not respond to the same variable. Base and divisor are held against
/// `COEFFICIENT_SCALE`.
///
/// ```text
/// class            base   shape                       divisor
/// quiet             750   ln(depth) * ln(moves)          2250
/// quiet check      1000   sqrt(depth) * ln(moves)        4000
/// tactical         1000   ln(depth) * sqrt(moves)        4000
/// tactical check      0   ln(depth) * ln(moves)          4500
/// ```
///
/// A quiet move is read off both logs, since being late in a long list and
/// having a lot of depth left both say the same thing about it. A quiet move
/// that gives check takes the root of the depth instead, so depth weighs
/// heavier and a deep line is not cut short on a forcing move. A capture
/// takes the root of the move count, being priced by the exchange
/// simulation already and so trusted on its own count rather than on where
/// ordering put it. A capture that gives check starts from no base at all:
/// at low depth it is searched in full, and only a long list and a deep
/// remainder together reduce it.
const REDUCTION_QUIET_BASE: u32 = 750;
const REDUCTION_QUIET_DIVISOR: u32 = 2250;
const REDUCTION_QUIET_CHECK_BASE: u32 = 1000;
const REDUCTION_QUIET_CHECK_DIVISOR: u32 = 4000;
const REDUCTION_TACTICAL_BASE: u32 = 1000;
const REDUCTION_TACTICAL_DIVISOR: u32 = 4000;
const REDUCTION_TACTICAL_CHECK_BASE: u32 = 0;
const REDUCTION_TACTICAL_CHECK_DIVISOR: u32 = 4500;

/// The window the root reopens around the previous completed score: a
/// fraction of the dearest piece, that being the top of this variant's
/// score range and so the scale one iteration's swing away from the last
/// is drawn against. Only the side that failed widens, by `WIDEN` each
/// time, until it passes `CLAMP` times the width it opened at; past that
/// the root reopens fully instead of widening again. Every one of the
/// three is held against `COEFFICIENT_SCALE`.
const ASPIRATION_RATIO: u32 = 30;

/// The cushion a node has to clear before its static evaluation alone is
/// trusted to beat beta: `RATIO` of the dearest non-royal piece per ply
/// still to search, the same anchor the aspiration window is priced off.
/// A side already standing better than it did two plies ago is believed
/// on less, so its row is the flat one scaled by `IMPROVING`. Both are
/// held against `COEFFICIENT_SCALE`.
///
/// - not improving : clears 11% of the dearest piece per ply still to
///                   search
/// - improving     : clears 75% of that row, a rising side being
///                   believed sooner
const RFP_RATIO: u32 = 110;
const RFP_IMPROVING: u32 = 750;

/// How far under alpha a node may stand and still search its late quiet
/// moves: `FLOOR` of the dearest non-royal piece, plus `RATIO` of it for
/// every ply still to search. A quiet move promises nothing immediate, so
/// a node further under alpha than that has none left worth ordering. The
/// side whose evaluation has not risen is believed least and so is given
/// the smaller margin, the risen side's row scaled by `IMPROVING`. All
/// three are held against `COEFFICIENT_SCALE`.
///
/// - improving     : 10% of the dearest piece, plus 13% of it per ply
///                   still to search
/// - not improving : 70% of that row, the sinking side giving up on its
///                   quiets first
const FUTILITY_FLOOR: u32 = 100;
const FUTILITY_RATIO: u32 = 130;
const FUTILITY_IMPROVING: u32 = 700;

/// How many moves a node orders before the quiets after them are taken
/// for noise: `BASE`, plus `RATIO` of the square of the depth left. The
/// row for a side whose evaluation has not risen is the risen side's
/// scaled by `IMPROVING`, so the side already doing worse gives up on its
/// quiets first. `RATIO` and `IMPROVING` are held against
/// `COEFFICIENT_SCALE`.
///
/// - improving     : 3 moves, plus the square of the depth left
/// - not improving : 55% of that row, and never fewer than one move
const LMP_BASE: u32 = 3;
const LMP_RATIO: u32 = 1000;
const LMP_IMPROVING: u32 = 550;

/// How much material a capture may already be seen to lose and still be
/// searched: `RATIO` of the dearest non-royal piece per ply still to
/// search, held against `COEFFICIENT_SCALE`. Ordering has priced every
/// capture by exchange simulation before the first is searched, so this
/// reads that price back rather than paying for it twice.
const SEE_PRUNE_RATIO: u32 = 250;

/// How much a capture has to promise at a quiet leaf before it is
/// searched: what it takes, plus `RATIO` of the dearest non-royal piece
/// held against `COEFFICIENT_SCALE`. A capture that cannot reach alpha
/// even at the full price of the piece it takes says nothing the score
/// already standing at that leaf does not. An endgame position is thin
/// enough that a single capture is most of what is left to play for, so
/// the margin is not applied there.
const QSEARCH_DELTA_RATIO: u32 = 100;

/// The ring a royal calls its own ground: every square within `RADIUS`
/// steps on both axes.
///
/// ```text
/// s s s     s   ahead: a shielding piece here is shelter, and a guard
/// g K g     g   beside or behind: any friendly piece here is a guard
/// g g g     K   the royal, radius 1 giving it eight ring squares
/// ```
///
/// Squares of that ring lying ahead of the royal are
/// its shelter, held by pieces that only ever advance. `CAP` is how many
/// sheltering pieces are still worth counting — past it a royal is as
/// walled in as this term can say, and the next piece belongs elsewhere.
/// Shelter is priced as `RATIO` of the dearest non-royal piece, held
/// against `COEFFICIENT_SCALE` and never below `FLOOR` in raw units.
const SHELTER_RADIUS: u32 = 1;
const SHELTER_RATIO: u32 = 12;
const SHELTER_FLOOR: u32 = 4;

/// What any friendly piece standing on that same ring is worth, whichever
/// side of the royal it stands on and whatever it is. A piece beside a
/// royal blocks a line into it, so it is priced like shelter but at half
/// the share, since a piece behind or beside the royal covers fewer of
/// the squares an attack arrives from than one in front of it. Uncapped:
/// the ring itself bounds the count.
const GUARD_RATIO: u32 = 6;
const GUARD_FLOOR: u32 = 2;

/// What castling is worth, for the variants whose rules offer it. Having
/// castled is priced as a full shelter, since it is what a side spends a
/// move to buy: the royal off the file it started on and a rook facing
/// the middle. Still holding a right is worth half of that, the same gain
/// still available but not yet taken. Both are shares of the dearest
/// non-royal piece against `COEFFICIENT_SCALE`, like every other term
/// standing beside them. A side that spent its rights without castling is
/// worth neither, which is what makes castling the move it prefers.
///
/// - castled : 4% of the dearest piece, a whole shelter's worth
/// - right   : 2% of it, the same gain still on offer
/// - spent   : nothing at all, having bought neither
const CASTLED_RATIO: u32 = 40;
const CASTLING_RIGHT_RATIO: u32 = 20;

/// What a pressed royal zone costs the side standing in it. Pressure is
/// counted in expected enemy landings on the royal's square or its ring,
/// and charged as its square, so one attacker barely registers while
/// several compound: `RATIO` of the dearest non-royal piece is charged at
/// `ZONE_ATTACK_FULL` landings, a quarter of it at half that many.
///
/// - 4 landings  : a sixteenth of the charge, barely a nudge
/// - 8 landings  : a quarter of it, the zone being genuinely watched
/// - 16 landings : the whole charge, 60% of the dearest piece
/// - beyond      : the cap, never past the dearest piece itself
///
/// The same quadratic runs away on a board that lets a whole army bear down
/// at once, so `CAP_RATIO` bounds the charge at the piece it is priced
/// off: an attack is never worth more than winning the dearest piece
/// outright, and the search should read it as pressure, not as a mate.
const DANGER_RATIO: u32 = 600;
const DANGER_CAP_RATIO: u32 = 1000;

/// What it costs a royal to stand with nothing of its own ahead of it, on
/// its file or either neighbouring one. Shelter prices the pieces that are
/// there; this prices their total absence, which no count of nearby
/// pieces can express. Held against `COEFFICIENT_SCALE` and never below
/// `FLOOR` in raw units.
///
/// - covered   : one shielding piece anywhere ahead of it on those
///               three files, and it costs nothing
/// - uncovered : 3.3% of the dearest piece, or 12 units when that is
///               the larger
const OPEN_SHIELD_RATIO: u32 = 33;
const OPEN_SHIELD_FLOOR: u32 = 12;

/// What having the move is worth. Every other term prices what stands on
/// the board, which both sides read the same way; this is the one thing
/// only the side to move owns, and without it a search reads a position
/// and its mirror as the same position. Held as a small share of the
/// dearest non-royal piece, since a move buys more in a variant with a
/// fierce army than in a quiet one, and never below `FLOOR`, so that
/// having the move is always worth something.
const TEMPO_RATIO: u32 = 24;
const TEMPO_FLOOR: u32 = 5;

/// What standing a piece ahead is worth beyond that piece's own value.
/// Material already says what each piece is; these say that the pieces are
/// not evenly matched, which is a fact about the position rather than
/// about any one of them. A side up a heavy piece is harder to trade back
/// to level than a side up a light one, so the heavy count is priced at
/// twice the light one. Both are shares of the dearest non-royal piece,
/// floored so a variant whose values sit close together still reads a
/// difference between the two counts.
///
/// - major : 2% of the dearest piece per heavy piece of surplus
/// - minor : 1% of it per light one, and never less than a unit
const IMBALANCE_MAJOR_RATIO: u32 = 20;
const IMBALANCE_MAJOR_FLOOR: u32 = 3;
const IMBALANCE_MINOR_RATIO: u32 = 10;
const IMBALANCE_MINOR_FLOOR: u32 = 1;

/// What holding two of a piece that can only ever reach half the board is
/// worth. Such a piece covers nothing of the half it is bound away from,
/// and a second one covers exactly what the first cannot, so the two
/// together are worth more than twice one of them. The test is geometric
/// rather than by name: mean reach within `SLACK` of half the board, which
/// no piece free of the board meets and no piece confined to a corner of
/// it comes near.
///
/// - 0.48 to 0.52 : bound to a half, and the pair is paid 6% of the
///                  dearest piece
/// - above        : free of the board, with no half left for a second
///                  copy to cover
/// - below        : confined already, and a second copy adds little
///
/// Royals are left out of the test entirely: a variant is free to field two
/// of them and forbid trading either, so holding both says nothing about
/// what they cover between them.
const PAIR_RATIO: u32 = 60;
const PAIR_FLOOR: u32 = 10;
const PAIR_REACH: f64 = 0.5;
const PAIR_REACH_SLACK: f64 = 0.02;

/// How many copies of a piece the opening army must field before it can be
/// this variant's pawn. The other conditions are geometric — never a step
/// or a capture backward, always a quiet single step forward, nothing
/// further than one square once the first move is spent — and those alone
/// would also catch a lone forward stepper such as the minishogi pawn or
/// the pair of minixiangqi soldiers. Structure is a statement about a rank
/// of pawns holding each other up, so a variant that fields too few of them
/// has no structure to price and scores none.
const PAWN_MIN_START_COUNT: usize = 5;

/// What a pawn's own structure is worth, as shares of that pawn's value
/// held against `COEFFICIENT_SCALE`. Every other derived value is priced
/// off the dearest non-royal piece, which is what says the units a variant
/// plays in; these are priced off the pawn instead, because each one is a
/// correction to what that single pawn is worth. A defect in a pawn costs
/// some part of a pawn whatever else stands on the board, and pricing it
/// off the dearest piece would charge a variant with a wide value range
/// several times over for the same structural fault.
///
/// Being defended is worth more once the board empties and the pawn is
/// closer to being the game, so connection is priced twice. The three
/// faults are priced once and read by both halves: doubled and isolated
/// cost near a quarter of the pawn, and a backward pawn less, since it is
/// only a pawn whose advance is watched rather than one already spent.
///
/// - connected : +20% of the opening pawn, +35% of the endgame one
/// - doubled   : -25% of the opening value, read at both ends
/// - isolated  : -25% of it, read at both ends as well
/// - backward  : -17.5%, an advance watched rather than one spent
const PAWN_CONNECTED_OPENING_RATIO: u32 = 200;
const PAWN_CONNECTED_ENDGAME_RATIO: u32 = 350;
const PAWN_DOUBLED_RATIO: u32 = 250;
const PAWN_ISOLATED_RATIO: u32 = 250;
const PAWN_BACKWARD_RATIO: u32 = 175;

/// What a passed pawn is worth, as a share of what promoting it would gain
/// — the dearest piece it could become, less what it is worth now — held
/// against `COEFFICIENT_SCALE` and scaled by how far along it already is.
///
/// - opening    : 10% of the promise, most of the board still ahead
/// - endgame    : 35% of it, with little left standing in the way
/// - no promise : 40% of the pawn's own value, that being ground and
///                no more
///
/// A passer is a promise rather than a piece, so the opening pays a tenth
/// of the promise while the endgame, where there is little left to stop it,
/// pays a third. A pawn that cannot promote at all is priced off its own
/// value instead, since advancing it wins ground and nothing more.
const PASSED_OPENING_RATIO: u32 = 100;
const PASSED_ENDGAME_RATIO: u32 = 350;
const PASSED_UNPROMOTED_RATIO: u32 = 400;

/// Share of the board a royal must be able to stand on before shelter is
/// worth pricing at all. A royal walled into a palace by its own forbidden
/// zones cannot be sheltered in the sense this term means: it never left
/// its own camp, its guards are pinned to it by their own move rules, and
/// the only way it can gather friendly pieces in front of itself is to
/// walk forward, which is exactly the move such variants punish. A royal
/// reaching a quarter of the board or less is taken for walled in, and the
/// term is switched off for that colour.
const SHELTER_CONFINEMENT_DIVISOR: usize = 4;

/// Bounds on the derive-time setup walk: how many distinct censuses may
/// be expanded, and how many completed setups are averaged. A placement
/// tree that outgrows either bound is referenced against the endings
/// already reached rather than being explored to exhaustion.
const SETUP_STATE_CAP: usize = 4096;
const SETUP_ENDING_CAP: usize = 256;

/*----------------------------------------------------------------------------*\
                            DERIVED PARAMETER TABLES
\*----------------------------------------------------------------------------*/

/// PieceRoles
///
/// One derived role assignment: the piece index paired with its is-big
/// and is-major classification flags.
type PieceRoles = (PieceIndex, bool, bool);

/// EvalParams
///
/// The static half of evaluation: the shelter, danger, pawn, imbalance and
/// contempt tables this file derives once per variant and `evaluation.rs`
/// reads once per leaf. Every field is `Default` at rest, so a fresh
/// variant costs one literal rather than one line per table.
///
/// The four tables the make/undo path touches incrementally —
/// `pst_opening`, `pst_endgame`, `opening_score`, `endgame_score` — stay
/// flat on `StaticState` in `state.rs`, so the hot set is one hop
/// shallower and `move_list.rs` never spells this type.
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

    pub pawn_slots: Vec<usize>,                                                 /* piece index to pawn slot, or NONE  */
    pub pawn_pieces: Vec<usize>,                                                /* pawn slot to piece index           */
    pub pawn_stride: usize,                                                     /* squares each pawn slot owns        */
    pub pawn_path: Vec<Board>,                                                  /* slot, square to advance squares    */
    pub pawn_interference: Vec<Board>,                                          /* slot, square to passer stoppers    */
    pub pawn_support: Vec<Board>,                                               /* slot, square to defending squares  */
    pub pawn_backward: Vec<Board>,                                              /* slot, square to stop attackers     */
    pub pawn_support_files: Vec<Vec<i32>>,                                      /* slot to supporting file offsets    */
    pub pawn_passed_opening: Vec<i32>,                                          /* slot, square to passer worth       */
    pub pawn_passed_endgame: Vec<i32>,
    pub pawn_connected_opening: Vec<i32>,                                       /* slot to worth of being defended    */
    pub pawn_connected_endgame: Vec<i32>,
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
/// The static half of search: the reduction surfaces and pruning margins
/// this file derives per variant and `search.rs` reads per node. Every
/// field is `Default` at rest.
#[derive(Default)]
pub struct SearchParams {
    pub reduction_quiet: Vec<u8>,                                               /* plies given up, depth major, one   */
    pub reduction_quiet_check: Vec<u8>,                                         /* surface per class of move: quiet   */
    pub reduction_tactical: Vec<u8>,                                            /* or tactical, in check or not       */
    pub reduction_tactical_check: Vec<u8>,

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
/// Classifies every non-royal piece as big/major/minor by ranking derived
/// values. The roles are determined as follows:
///
/// 1. sort all the piece values, excluding the royal pieces
/// 2. the bottom ceil(10%) are the non-big pieces
/// 3. the top ceil(20%) are the major pieces
/// 4. the rest are the minor pieces
///
/// Params:
/// - state: &mut State -> variant whose derived values are ranked
///
/// Return:
/// Vec<PieceRoles>     -> (index, is_big, is_major) per piece
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
/// Measures how much of the board a piece can eventually cover: from every
/// origin square, flood-fills through the symmetric closure of the piece's
/// move offsets (each offset is also applied reversed, so one-directional
/// movers are not punished twice) and averages the reached fraction.
///
/// - rook     : every square, given moves enough, so a reach of one
/// - bishop   : half of them, the colour it began on, so about a half
/// - elephant : the seven points of its own bank, well under a tenth
///
/// Reach is what a piece can eventually see rather than what it sees now,
/// so a knight scores the same as a rook: both go anywhere, and only the
/// mobility term says which one takes longer about it.
///
/// Params:
/// - state: &State -> precomputed relevant-move tables
/// - piece: &Piece -> piece whose coverage is measured
///
/// Return:
/// f64             -> mean reachable fraction of the board, in (0, 1]
///
/// Notes:
/// An origin the piece cannot leave at all is dropped from the mean rather
/// than counted as reaching the one square it stands on, so a piece is
/// measured over the ground it can actually use. The offsets are closed
/// under negation before the fill, which is why a pawn is not read as
/// covering only the squares ahead of where it happens to start.
fn derive_piece_reach(state: &State, piece: &Piece) -> f64 {
    let board_size = state.statics.board_size;
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let piece_index = p_index!(piece) as usize;

    let reach_values: Vec<i32> = (0..board_size).into_par_iter().map(|square| {
        let mut reached_squares: HashSet<usize> = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(square);
        reached_squares.insert(square);

        while let Some(current) = queue.pop_front() {
            let start_file = current as i32 % files;
            let start_rank = current as i32 / files;

            let relevant_moves = &state.statics.relevant_moves
                [piece_index * board_size + current];

            for multi_leg_vector in relevant_moves {
                let mut file_offset = 0;
                let mut rank_offset = 0;

                for leg in multi_leg_vector {
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
                        let next = (next_rank * files + next_file) as usize;

                        if reached_squares.insert(next) {
                            queue.push_back(next);
                        }
                    }
                }
            }
        }

        if reached_squares.len() == 1 {
            0
        } else {
            reached_squares.len() as i32
        }
    }).collect();

    let mean = reach_values.iter().filter(|&&v| v > 0).sum::<i32>() as f64
        / reach_values.iter().filter(|&&v| v > 0).count() as f64;

    mean / board_size as f64
}

/// derive_piece_offsets
///
/// Every distinct net displacement a piece can make, summed over the legs
/// of each multi-leg vector and gathered across every square it could
/// stand on. A slider contributes one offset per distance it can travel,
/// so the set says how far a piece reaches as well as in which
/// directions.
///
/// Params:
/// - state: &State     -> precomputed relevant-move tables
/// - piece: &Piece     -> piece whose offsets are gathered
///
/// Return:
/// HashSet<(i32, i32)> -> file and rank displacements, origin excluded
fn derive_piece_offsets(state: &State, piece: &Piece) -> HashSet<(i32, i32)> {
    let board_size = state.statics.board_size;
    let piece_index = p_index!(piece) as usize;

    let mut offsets: HashSet<(i32, i32)> = HashSet::new();

    for square in 0..board_size {
        let relevant_moves = &state.statics.relevant_moves
            [piece_index * board_size + square];

        for multi_leg_vector in relevant_moves {
            let mut file_offset = 0;
            let mut rank_offset = 0;

            for leg in multi_leg_vector {
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
/// Prices one piece for one phase out of its movement geometry alone. Four
/// measurements are taken and folded into a single number:
///
/// - empty mobility    : its moves per square with nothing in the way
/// - occupied mobility : the same count under the phase's fill model
/// - reach             : the share of the board it can eventually cover
/// - maneuverability   : the share of its offsets it can also reverse
///
/// The two mobilities are blended seven parts occupied to three parts
/// empty, so a long slider keeps some of what an open board would give it
/// without being priced as though the board were always open. The blend is
/// then scaled by the other two, each read as a floor plus what it
/// measures: coverage never falls below 0.6 and maneuver never below 0.5,
/// so a colour-bound piece or a one-way piece is discounted rather than
/// erased. The product is multiplied out to the units the rest of the
/// engine works in and returned.
///
/// A higher `occupancy` means a fuller board, so the opening value is the
/// one taken at the higher fill. Sliders are stopped early there and gain
/// on leapers as the same piece is priced again for the endgame.
///
/// Params:
/// - state    : &State -> precomputed relevant-move tables
/// - piece    : &Piece -> piece to value
/// - occupancy: f64    -> assumed board fill ratio for the phase
///
/// Return:
/// f64                 -> raw value, offset-normalized across the army later
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

    let blended_mobility = 0.3 * empty_mobility
        + (1.0 - 0.3) * occupied_mobility;

    let coverage = 0.6 + (1.0 - 0.6) * reach;
    let maneuver = 0.5 + (1.0 - 0.5) * maneuverability;

    58.0 * blended_mobility * coverage * maneuver
}

/// derive_piece_mobility
///
/// Expected move count from one square under a random-fill occupancy
/// model, walking each vector leg by leg to mirror generator semantics:
/// pass legs (may move) need an empty stop and weigh `1 - occupancy`,
/// screen legs (capture-only intermediates, the hopper jump) need an
/// occupied stop and weigh `occupancy`. A hopping vector whose final leg
/// is capture-only also needs a target there, weighing `occupancy` once
/// more. Leapers are unaffected; long slides decay geometrically; hopper
/// captures contribute nothing on an empty board.
///
/// Params:
/// - state      : &State     -> precomputed relevant-move tables
/// - piece_index: PieceIndex -> piece whose vectors are counted
/// - square     : usize      -> origin square
/// - occupancy  : f64        -> assumed board fill ratio
///
/// Return:
/// f64                       -> playable vectors expected from the square
fn derive_piece_mobility(
    state: &State, piece_index: PieceIndex, square: usize, occupancy: f64
) -> f64 {
    let board_size = state.statics.board_size;

    let relevant_moves = &state.statics.relevant_moves
        [piece_index as usize * board_size + square];

    relevant_moves
        .iter()
        .filter_map(|vector| derive_vector_chance(vector, occupancy))
        .map(|(chance, ..)| chance)
        .sum()
}

/// derive_vector_chance
///
/// Walks one multi-leg vector under the random-fill occupancy model and
/// returns the odds every per-leg stop requirement is met together with
/// the vector's total displacement.
///
/// - pass   : wants an empty stop, at odds of `1 - occupancy`
/// - screen : wants an occupied one, at odds of `occupancy`
/// - final  : wants `occupancy` once more, if it captures after a hop
/// - marker : displaces nothing at all, and is skipped
///
/// The odds are the product, so a long slide decays geometrically with
/// each square it has to find empty, a leaper answers 1 whatever the board
/// holds, and a hopper's capture is worth nothing at all on an empty one:
/// it needs both a screen to jump and a target to land on.
///
/// Params:
///
///     multi_leg_vector: &[Leg]
///     the vector's legs, final leg last
///
///     occupancy: f64
///     assumed board fill ratio
///
/// Return:
///
///     Option<(f64, i32, i32)>
///     the odds paired with the file and rank the vector displaces by,
///     or None for a vector with no legs at all
fn derive_vector_chance(
    multi_leg_vector: &[Leg], occupancy: f64
) -> Option<(f64, i32, i32)> {
    let (final_leg, intermediate_legs) = multi_leg_vector.split_last()?;

    let mut chance = 1.0;
    let mut hopper = false;
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

        hopper |= needs_piece;
    }

    if hopper && c!(final_leg) && !m!(final_leg) {
        chance *= occupancy;
    }

    Some((chance, file_delta, rank_delta))
}

/// derive_distance_from_center
///
/// Measures the euclidean distance from a square to the nearest of the (up
/// to four) central squares, handling even and odd board dimensions.
///
/// Params:
/// - state : &State -> board dimensions for locating the center
/// - square: usize  -> square whose distance is measured
///
/// Return:
/// f64              -> distance in square units
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

/// derive_closest_promotion
///
/// Measures the distance to the nearest square of the piece's promotion
/// zones, mandatory or optional, returning infinity when the piece has
/// none.
///
/// Params:
/// - state      : &State     -> board dimensions and promotion zones
/// - piece_index: PieceIndex -> piece whose promotion zones are queried
/// - square     : usize      -> square whose distance is measured
///
/// Return:
/// f64                       -> square units, or infinity with no zone
fn derive_closest_promotion(
    state: &State, piece_index: PieceIndex, square: usize
) -> f64 {
    let closest_mandatory = set_indices!(
        state.statics.promotion_zones_mandatory[piece_index as usize]
    )
    .iter()
    .map(|&index| square_distance(state, square as Square, index as Square))
    .min_by(|a, b| a.partial_cmp(b).unwrap())
    .unwrap_or(f64::INFINITY);

    let closest_optional = set_indices!(
        state.statics.promotion_zones_optional[piece_index as usize]
    )
    .iter()
    .map(|&index| square_distance(state, square as Square, index as Square))
    .min_by(|a, b| a.partial_cmp(b).unwrap())
    .unwrap_or(f64::INFINITY);

    closest_optional.min(closest_mandatory)
}

/// derive_promotion_bonus
///
/// Advancement bonus for a promotable piece: a gradient toward the nearest
/// promotion square, scaled by the value promoting would gain.
///
/// - opening : 6% of that gain, times how far along the piece already is
/// - endgame : 40% of it, a promotion being most of what is left to
///             play for
///
/// Advancement is squared, so the gradient is flat where the piece starts
/// and steep where it is nearly home: a piece one square short of the zone
/// is worth far more than one halfway, which is the shape a passed pawn
/// actually has. A piece with no promotion zone is worth nothing here.
///
/// Params:
/// - state         : &State     -> board dimensions and promotion zones
/// - piece_index   : PieceIndex -> piece being placed
/// - square        : usize      -> square being scored
/// - is_endgame    : bool       -> selects the phase's bonus fraction
/// - piece_value   : f64        -> the piece's current phase value
/// - promoted_value: f64        -> best value reachable by promotion
///
/// Return:
/// f64                          -> bonus added to the square's own score
fn derive_promotion_bonus(
    state: &State, piece_index: PieceIndex, square: usize,
    is_endgame: bool, piece_value: f64, promoted_value: f64
) -> f64 {
    let piece = &state.statics.pieces[piece_index as usize];

    if !p_can_promote!(piece) {
        return 0.0;
    }

    let closest_promotion =
        derive_closest_promotion(state, piece_index, square);
    let advancement =
        (1.0 - closest_promotion / state.statics.ranks as f64).max(0.0);

    let fraction = if is_endgame {
        0.40
    } else {
        0.06
    };

    fraction * (promoted_value - piece_value).max(0.0)
        * advancement.powf(2.0)
}

/// derive_pst
///
/// Builds one piece-square table. A square's raw score is its mobility
/// from that square minus its distance from the center, weighted by phase:
///
/// - opening : mobility 0.50 and centrality 1.25, on a board 36% full
/// - endgame : mobility 0.25 and centrality 1.75, on one 12% full
///
/// Those scores are centered on their mean and normalized to a fixed
/// amplitude, so every piece's table swings over the same range whatever
/// the raw numbers were, and material alone says which piece is worth
/// more. The promotion gradient is added afterwards, outside that
/// normalization, since it is a claim about the piece and not about the
/// square. For a compact board the positional part comes out
/// center-positive and edge-negative, e.g.:
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
/// Royal pieces get a back-rank gradient in the opening instead of the
/// mobility-and-centrality score, a royal being a piece that should sit
/// behind its own lines rather than march up the board:
///
/// ```text
/// rank 3   -3  -3  -3  -3
/// rank 2   -2  -2  -2  -2
/// rank 1   -1  -1  -1  -1
/// rank 0    0   0   0   0   the rank it started on, and the best one
/// ```
///
/// Every square scores the negated rank index in the white frame, so each
/// step forward is worse than the last and no file is preferred over
/// another: where along the back rank a royal sits is what shelter and
/// castling are for. The endgame table keeps the centralizing score, since
/// by then the royal is a piece that has to work.
///
/// Params:
/// - index         : PieceIndex -> piece the table is built for
/// - state         : &State     -> precomputed relevant-move tables
/// - is_endgame    : bool       -> selects phase weights and values
/// - promoted_value: f64        -> best value reachable by promotion
///
/// Return:
/// Vec<i32>                     -> per-square bonus table in board order
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

    let scores: Vec<f64> = if !is_endgame && p_is_royal!(piece) {
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
    let amplitude = 24.0;

    (0..board_size).map(|square| {
        let positional =
            (scores[square] - mean) / max_deviation * amplitude;
        let promotion = derive_promotion_bonus(
            state, index, square, is_endgame, piece_value, promoted_value
        );

        (positional + promotion).round() as i32
    }).collect()
}

/// walk_setup_endings
///
/// Depth-first walk of the placement phase over one scratch position,
/// recording the board census every time the variant's own rules end
/// SETUP. Placements are made and unmade in place, so the walk costs one
/// position rather than one per node, and it never asks what ends a setup
/// — it plays until the position says it has.
///
/// The walk memoizes on what a census is made of rather than on the board:
///
/// - piece_count   : how many of each type stand on the board
/// - piece_in_hand : what is left to place, White's hand then Black's
/// - playing       : whose turn it is to place the next one
///
/// Two part-built setups differing only in where equal pieces stand share
/// that identity, which is what keeps the walk bounded on a variant whose
/// placements are largely interchangeable.
///
/// Params:
/// - probe  : &mut State             -> scratch position, restored on return
/// - visited: &mut HashSet<Vec<u32>> -> censuses already expanded
/// - endings: &mut Vec<Vec<u32>>     -> censuses of completed setups
///
/// Notes:
/// Both caps are checked on entry, so a walk that has already gathered
/// enough endings stops descending rather than finishing the branch it is
/// on, and the loop breaks on the ending cap as well so a wide node does
/// not keep placing after the walk is done.
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
/// Reports the army a variant actually begins play with. Most variants
/// already stand theirs on the board, so their live census answers
/// directly. A variant with a placement phase does not: what it starts
/// with is whatever that phase leaves behind, and a hand there is a menu
/// of what may be placed rather than a promise that all of it will be —
/// a variant offering a choice of armies holds every one of them and
/// deploys exactly one.
///
/// - on the board : the live census, with no walk at all
/// - placed       : the placements played out, and the endings reached
///                  averaged over
/// - no ending    : the live census again, the walk having proved
///                  nothing
///
/// So the walk plays legal placements until the rules end SETUP and
/// averages the census over the completed setups it reaches, leaving a
/// variant that chooses between armies referenced against a
/// representative one rather than the union of every offer. It runs once,
/// at derivation, over a copy of the position; nothing here happens per
/// node.
///
/// Notes:
/// The copy has to be dropped before the caller writes any static, since
/// `static_mut` claims sole ownership of the shared static state. Its eval
/// caches are rebuilt first because the roles the caller just assigned
/// invalidate the counts the position was loaded with.
///
/// Params:
/// - state: &State -> start position, piece values already derived
///
/// Return:
/// Vec<u32>        -> deployed count per piece index once play begins
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
/// Startup entry point for the whole derivation pass. The order is a
/// dependency order, each stage reading what the ones above it wrote:
///
/// - eval         : the piece values and roles the rest is a share of
/// - search       : the margins and reductions off the dearest piece
/// - shelter      : the royal ring, and whether it is worth pricing
/// - danger       : the zone attacks, over the ring shelter just built
/// - pawn         : the fault and passer terms, over the pawns found
/// - advantage    : imbalance and contempt, over the values and roles
/// - capabilities : which prunings the variant's rules leave meaningful
/// - refresh      : the incremental caches, over all of the above
///
/// It runs once per variant load, so nothing here is on a search path and
/// nothing here is asked to be cheap.
///
/// Params:
/// - state: &mut State -> freshly precomputed variant state
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
/// Builds one late-move reduction table: for every remaining depth and
/// every move number, how many plies a move of that class gives up on its
/// first search.
///
/// ```text
/// quiet surface   move 1   move 8   move 32   move 63
/// depth 2              0        1         1         2
/// depth 8              0        2         3         4
/// depth 32             0        3         6         7
/// ```
///
/// The table is read rather than computed because a logarithm per node is
/// a logarithm the search cannot afford, and every cell it could ask for
/// fits in a byte. Depth zero and move zero index nothing the search ever
/// reduces, so they hold zero rather than the logarithm of it.
///
/// Params:
/// - base   : u32 -> curve base, held against `COEFFICIENT_SCALE`
/// - divisor: u32 -> curve divisor, held against `COEFFICIENT_SCALE`
/// - shape  : F   -> the depth and move terms this curve mixes
///
/// Return:
/// Vec<u8>        -> `MAX_DEPTH * REDUCTION_MOVE_CAP` plies, depth major
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
/// Opening value of the most valuable non-royal piece one colour deploys,
/// the unit every derived margin and safety value is a share of. Normalized
/// values pin the cheapest piece at the same number in every variant, so
/// only the top of the range says anything about the units a variant plays
/// in. Black twins carry the white values, so reading one colour reads all.
///
/// Params:
/// - state: &State -> variant whose piece values are read
///
/// Return:
/// u64             -> the dearest value, or zero for a royal-only army
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
/// Drives the search half of derivation: rebuilds all four late-move
/// reduction surfaces, margins, move counts, and root aspiration width from
/// universal coefficients and loaded material.
///
/// - aspiration       : off the dearest piece, half the root window,
///                      before it widens
/// - reverse futility : off the dearest piece, a step per depth, in two
///                      rows
/// - razoring         : off the mean, four depths, a whole swing and
///                      not one piece
/// - ProbCut          : off the mean, that same swing, as one number
/// - futility         : off the dearest piece, a floor plus a step per
///                      depth, in two rows
/// - late-move count  : off no material at all, depth squared
/// - exchange         : off the dearest piece, a step per depth, in one
///                      row
/// - quiescence delta : off the dearest piece, the swing a quiet node
///                      may still hope for
///
/// The window is priced off the dearest non-royal piece rather than the
/// cheapest, which normalization pins at 100 in every variant and so says
/// nothing: a flat value range moves the score less per capture and earns
/// the narrower window that follows from it. The reverse futility margin
/// reads the same piece for the same reason, one flat row and one for a
/// side whose evaluation has risen, indexed by the depth left to search.
/// The futility margin and the exchange allowance are drawn against that
/// same piece, and the late-move count against depth alone, having no
/// material in it to price. Razoring and ProbCut retain their iteration 3
/// anchor, the mean non-royal value: both compare a whole tactical swing,
/// not the top of the range one move may spend.
///
/// Every improving multiplier names the row that prunes harder, which is
/// not the same row throughout: a cut against beta believes a risen side
/// sooner, while both cuts against alpha give up on a side that has not
/// risen first. The row a node reads is always its improving flag, so the
/// choice lives here rather than at every use.
///
/// Every two-row table is one flat vector, the flag choosing the half:
///
/// ```text
/// improving clear   [ 0  d1  d2  ...  dn ]   read at depth
/// improving set     [ 0  d1  d2  ...  dn ]   read at deepest + 1 + depth
/// ```
///
/// Params:
/// - state: &mut State -> variant whose derived search values are rebuilt
///
/// Notes:
/// Each table is asserted to rise with the depth left to search before it
/// is stored. A margin that fell would prune a deeper node harder than a
/// shallow one, which no coefficient is meant to express, so a ratio
/// mistyped into an inversion fails at load rather than quietly costing
/// depth. The aspiration width is floored at one, a window of zero holding
/// no score at all, and the late-move count at one, so the ordering always
/// gets to try its first quiet move.
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
/// Decides once, before any game starts, which of the search's shortcuts this
/// rule set still permits, and records them in `capabilities` for the readers
/// documented on [`StaticState`]. Each shortcut rests on a claim about the
/// game rather than about a position — that material is the currency, that a
/// static score bounds a subtree, that giving up the move concedes something,
/// that a late quiet move is a bad one — and a variant that never makes the
/// claim leaves the bit clear and has the position played out instead.
///
/// Every bit is granted unless something in the rules takes it away:
///
/// - exchange simulation : a royal capture, a multi capture, misere,
///                         extinction, or promotion into what was taken
/// - pruning on it       : recycled captures, a check count, counting,
///                         or a goal
/// - forward pruning     : misere, extinction, a goal, or a check count
/// - null pruning        : misere, a goal, a check count, counting, a
///                         pass the rules already offer, stand-offs, a
///                         setup phase, or a piece with no quiet move
/// - recapture ordering  : a multi capture, recycled captures, a check
///                         count, or a goal
/// - quiet pruning       : misere, a goal, or a check count
/// - static movement     : a screened leg anywhere in the rules
///
/// Two kinds of fact answer the questions. Movement facts come from the
/// generated vectors: a leg that unloads what it destroyed needs a second
/// piece standing where it stands, a leg that may take a royal is not trading
/// material, a vector that destroys twice wins more than its victim, a vector
/// that ends where it started having taken nothing is a pass the variant
/// already offers — a lion returning home over a corpse is not one, which is
/// why the destroy flag disqualifies the shape rather than the displacement
/// alone — and a piece with no quiet vector cannot give up a tempo at all.
/// Terminal facts come from the declared rules: counting pieces, holding a
/// zone, or tallying checks all pay in a currency material does not convert
/// to.
///
/// Nothing here reads a variant's name, and nothing asks whether a rule is
/// familiar. A rule set written tomorrow is judged by the same questions.
///
/// Params:
/// - state: &mut State -> variant whose capability mask is derived
///
/// Notes:
/// The movement scan reads every piece on every square rather than the
/// piece's own template, since a leg that only appears near an edge is
/// still a leg the rules contain. It runs once at load, so the sweep costs
/// nothing a search will ever wait on.
pub fn derive_search_capabilities(state: &mut State) {
    let statics = &state.statics;
    let board_size = statics.board_size;

    let mut screened = false;
    let mut royal_capture = false;
    let mut multi_capture = false;
    let mut capture_only = false;
    let mut may_pass = false;

    for piece_index in 0..statics.pieces.len() {
        let mut vectors = 0;
        let mut quiet_vectors = 0;

        for square in 0..board_size {
            let slot = piece_index * board_size + square;

            for vector in statics.relevant_moves[slot].iter()
                .chain(statics.relevant_captures[slot].iter())
            {
                let mut destroyed = 0;
                let mut destroys = false;

                for leg in vector {
                    screened |= u!(leg);
                    royal_capture |= k!(leg);
                    destroys |= d!(leg);
                    destroyed += (d!(leg) && !u!(leg)) as usize;                /* an unloaded piece is put back      */
                }

                let (files_crossed, ranks_crossed) = vector_offset!(vector);
                let moves_quietly = vector_moves_quietly!(vector);

                multi_capture |= destroyed > 1;
                may_pass |= moves_quietly && !destroys
                    && files_crossed == 0 && ranks_crossed == 0;                /* nothing moved and nothing taken   */
                vectors += 1;
                quiet_vectors += moves_quietly as usize;
            }
        }

        capture_only |= vectors > 0 && quiet_vectors == 0;
    }

    let termination = &state.termination;

    let counts_pieces = !termination.extinct.is_empty();
    let holds_zone = termination.goal.is_some();
    let counts_checks = termination.checks.is_some();
    let counts_material = termination.counting.is_some();

    let misere = termination.checkmate == Outcome::Win
        || termination.stalemate == Outcome::Win
        || termination.extinct.iter()
            .any(|rule| rule.outcome == Outcome::Win);

    let recycles_captures = promote_to_captured!(state) || drops!(state);
    let places_army = setup_phase!(state);
    let vetoes_moves = stand_offs!(state);

    let mut capabilities = 0u16;

    if !royal_capture && !multi_capture && !misere
    && !counts_pieces && !promote_to_captured!(state)
    {
        enc_see_valid!(capabilities);
    }

    if !recycles_captures && !counts_checks
    && !counts_material && !holds_zone
    {
        enc_see_pruning!(capabilities);
    }

    if !misere && !counts_pieces && !holds_zone && !counts_checks {
        enc_forward_pruning!(capabilities);
    }

    if !misere && !holds_zone && !counts_checks && !counts_material
    && !may_pass && !vetoes_moves && !places_army && !capture_only
    {
        enc_null_pruning!(capabilities);
    }

    if !multi_capture && !recycles_captures
    && !counts_checks && !holds_zone
    {
        enc_recapture_order!(capabilities);
    }

    if !misere && !holds_zone && !counts_checks {
        enc_quiet_pruning!(capabilities);
    }

    if !screened {
        enc_static_movement!(capabilities);
    }

    state.static_mut().capabilities = capabilities;

    log_3!("Derived Search Capabilities: {:07b}", capabilities);
}

/*----------------------------------------------------------------------------*\
                             EVALUATION DERIVATION
\*----------------------------------------------------------------------------*/

/// derive_base_pst
///
/// Builds rule-derived opening and endgame PST bases for every piece, one
/// pair of rows per piece index. Loaded material has to be final before this
/// runs, since the promotion gradient inside a table is priced against the
/// value a promotion reaches.
///
/// A Black piece is not derived in its own orientation. It is derived on its
/// White twin's index, so the square scores are built once for a board seen
/// the same way up, and the finished rows are turned around afterwards:
///
/// - the piece is looked up under its White twin's index
/// - the square scores are derived there: mobility, distance from the
///   centre, and the promotion gradient
/// - the finished rows are flipped across the horizontal axis
/// - they are stored back under the piece's own index
///
/// The promotion ceiling handed to each phase is the dearest non-royal value
/// of that phase, so how far a promotion is worth walking towards is measured
/// against the best a promotion could possibly reach.
///
/// Params:
///
///     state: &State
///     variant whose rule-derived PST bases are built
///
/// Return:
///
///     (Vec<Vec<i32>>, Vec<Vec<i32>>)
///     opening and endgame rows by piece index, White then its mirror
pub fn derive_base_pst(state: &State) -> (Vec<Vec<i32>>, Vec<Vec<i32>>) {
    let promoted_opening = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_ovalue!(piece) as f64)
        .fold(0.0_f64, f64::max);
    let promoted_endgame = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_evalue!(piece) as f64)
        .fold(0.0_f64, f64::max);

    let pst_entries: Vec<(usize, Vec<i32>, Vec<i32>)> =
        state.statics.pieces.par_iter().map(|piece| {
            let mut index = p_index!(piece);

            if p_color!(piece) == BLACK {
                index = state.statics.piece_swap_map[index as usize];
            }

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
/// Derives opening and endgame material from movement rules alone, each
/// piece priced twice against the board fill its phase assumes, and then
/// shifts the whole table so the cheapest piece in the variant lands on 100.
///
/// - every White piece is derived, once at each occupancy
/// - the offset is taken as the cheapest opening value, less 100
/// - that offset is subtracted from both values of every piece
///
/// The shift is what makes two variants comparable: only the width of the
/// range says anything about a variant, its floor being an artefact of how
/// generous the mobility model happened to be. Black is never derived, its
/// twin being handed the White values through the swap map.
///
/// Role flags stay clear here. Big and major are a ranking over the finished
/// table, so they cannot be assigned while it is still being written.
///
/// Params:
/// - state: &mut State -> variant whose material values are derived
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

    for (index, opening, endgame) in values {
        let black_index = state.statics.piece_swap_map[index] as usize;
        let white_index = index;
        let ovalue = (opening - offset).round() as u16;
        let evalue = (endgame - offset).round() as u16;

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
/// Derives material from rules, then rebuilds every material-dependent
/// evaluation product through the same post-load path used by payloads.
///
/// The split is what lets a tuned payload replace the derived values
/// without replacing anything built on them: a payload writes the material
/// table and calls the second half alone, and the roles, thresholds,
/// contempt and PST bases are rebuilt from whatever the table now says.
///
/// Params:
/// - state: &mut State -> variant whose evaluation parameters are derived
pub fn derive_eval_parameters(state: &mut State) {
    derive_material_values(state);
    derive_eval_products(state);
}

/// derive_eval_products
///
/// Rebuilds roles, phase thresholds, draw contempt, and rule-derived PST
/// bases after final material values have loaded, whether they were derived
/// from the rules or read out of a tuned payload.
///
/// Everything here is a share of one number: the mean value of a non-royal
/// piece in the army the variant actually begins play with. Royals are left
/// out of that mean, a side never being able to trade one.
///
/// - opening end : the mean, once per big piece the army deploys
/// - endgame end : the mean `ENDGAME_ARMY_SIZE` times, kept under the
///                 opening end
/// - span        : the mean twice over, the lead contempt is measured
///                 against
/// - contempt    : an eighth of the mean, what a draw costs a side
///                 that is level
///
/// The endgame end is clamped a point below the opening end rather than
/// trusted to fall there: a variant deploying almost no big pieces would
/// otherwise leave the two ends crossed, and the taper divides by their
/// difference.
///
/// Params:
/// - state: &mut State -> variant whose loaded-material products are rebuilt
///
/// Notes:
/// Roles are assigned before the army is resolved, because the setup walk
/// plays real moves on a copy of the position and the copy's evaluation
/// caches count pieces by the roles they carry.
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

/// derive_forward_directions
///
/// Rank step each colour advances by, read from the army the variant
/// starts play with: the side deployed on the lower ranks is the side that
/// moves up the board. A variant that deploys nothing, or deploys both
/// colours around the same rank, still has to name a direction and sends
/// white up.
///
/// - Black lower   : Black is sent up the board, and White down
/// - anything else : White is sent up and Black down, ties included
///
/// Params:
/// - state: &State -> variant whose initial deployment is read
///
/// Return:
/// [i32; 2]        -> colour to rank step, either 1 or -1
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
/// Marks the piece types worth having in front of a royal: a non-royal
/// that never leaves the local neighbourhood, and whose offsets lean
/// forward on balance. A pawn, a shogi gold and a silver all qualify; a
/// piece leaping over the neighbourhood shelters nothing, so only a
/// straight step out of the deployment rank is allowed past the radius,
/// and a piece with no forward lean is not standing in front of anything.
///
/// - inside the radius : a step of any kind will do, the piece stays
///                       near home
/// - one square past   : only a straight double step off the deployment
///                       rank, with no file offset
/// - further           : a leap over the neighbourhood the piece was
///                       meant to hold
/// - rank offsets      : must sum positive, or the piece faces nowhere
///                       useful
///
/// Move vectors are stored in the mover's own frame, with the colour sign
/// applied only when a move is walked, so a rank offset is forward for
/// whichever colour owns the piece and needs no board direction here.
///
/// Params:
/// - state : &State -> precomputed relevant-move tables
/// - radius: i32    -> radius the local square lists are built at
///
/// Return:
/// Vec<bool>        -> piece index to shield-like role
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
/// Whether each colour's royals are locked into a small corner of the board
/// by their own forbidden zones. Reach is read straight off the zone
/// bitboard rather than walked, since a zone already states every square
/// the piece may ever stand on.
///
/// - a quarter or less : confined, and shelter is switched off for
///                       that colour
/// - more than that    : free, and the term is priced as usual
///
/// A colour is confined if any of its royals is, a variant fielding two of
/// them and walling one having walled the term the shelter table prices.
///
/// Params:
/// - state: &State -> variant whose royal zones are read
///
/// Return:
/// [bool; 2]       -> per colour, whether shelter should be priced at all
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
/// Builds everything the royal shelter, guard, and castling terms read. One
/// flat square list per colour holds squares inside the royal's local ring
/// that lie forward of its origin, and one colour-blind list holds the whole
/// ring. A count per origin records how many slots survive board edges, so
/// evaluation needs no bounds arithmetic.
///
/// Each list is a flat table of `stride` slots per origin square, `stride`
/// being the ring's full size and the count saying how much of it is real:
///
/// ```text
/// centre   s s s   eight ring squares, and the count reads eight
///          s K s
///          s s s
/// corner   s s     three of them land on the board, the count reads
///          K s     three, and the rest of the row is never touched
/// ```
///
/// The origin is skipped while the ring is built, a royal not standing in
/// front of itself, which is why the stride is one short of the square of
/// the ring's width.
///
/// All four values are priced off the dearest non-royal piece, the same piece
/// search margins use, so a variant whose army is cheap pays a proportionate
/// value. The confinement gate applies to shelter alone: a walled royal has no
/// forward ground to hold, but the pieces standing beside it still block the
/// lines an attack would arrive on.
///
/// Params:
/// - state: &mut State -> variant whose shelter tables are rebuilt
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
/// Builds the zone-attack tables `king_danger!` reads and prices what they
/// and `open_shield!` charge. For every piece, every origin it could stand
/// on, and every square a royal could stand on, the table holds how many of
/// that piece's vectors are expected to land on the royal's square or on
/// its ring, under the same opening-occupancy model piece values are
/// derived from. Storing the answer per triple keeps evaluation from
/// walking a single vector: it sums bytes over the enemy pieces actually on
/// the board, and squares that sum.
///
/// - the landing square is charged the vector's odds of arriving there
/// - its whole ring is charged that same amount again, square by square
/// - an entry is indexed `(royal * pieces + piece) * squares + origin`
/// - the best row holds the largest over that origin axis, which is what
///   a piece in hand reads
///
/// A vector is charged onto the ring as well as onto the square it lands
/// on because a royal is in danger from what surrounds it, not only from
/// what is aimed at it, and the ring is where its shelter has to stand.
///
/// Entries are `ZONE_ATTACK_UNIT`ths of an expected landing, saturating at
/// a byte, so a piece attacking the zone through open lines scores a full
/// unit per landing square and one attacking through a line that has to be
/// empty, or hopping one that has to be occupied, scores its odds of it.
/// The same pass reduces the table over its origin axis into
/// `zone_attack_best`, the pressure a piece would exert from the origin it
/// would pick. A piece held in hand stands on no square, so that reduction
/// is the only pressure a hand can be read at, and deriving both here makes
/// it impossible for one to exist without the other.
///
/// The ring this reads is the one `derive_shelter_parameters` already
/// built, so that pass must run first.
///
/// Params:
/// - state: &mut State -> variant whose danger tables are rebuilt
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
                    derive_vector_chance(vector, occupancy)
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

    let statics = state.static_mut();

    statics.eval.zone_attack = table;
    statics.eval.zone_attack_best = best;
    statics.eval.king_danger_scale = king_danger_scale as i32;
    statics.eval.king_danger_cap = king_danger_cap as i32;
    statics.eval.open_shield_penalty = open_shield_penalty as i32;
}

/*----------------------------------------------------------------------------*\
                           PAWN STRUCTURE DERIVATION
\*----------------------------------------------------------------------------*/

/// derive_pawn_slots
///
/// Picks out the piece types whose structure is worth pricing and gives
/// each one a slot in the pawn tables. A piece is this variant's pawn when
/// it never steps or captures backward, always keeps a quiet single step
/// forward available, never ranges further than one square once its first
/// move is spent, and stands in the opening army at least
/// `PAWN_MIN_START_COUNT` times over.
///
/// - never backward : no vector of it carries a negative rank
/// - steps forward  : a quiet one-rank push, its file within one
/// - never ranges   : past one square, the opening push once spent
/// - fielded often  : counting the board and the hand alike
///
/// Those four conditions are geometric and count-based, and between them
/// they name the pawn of every variant without naming a variant: the
/// single-step rule rejects leapers like the shogi knight and forward
/// sliders like the lance, the no-retreat rule rejects the gold, silver,
/// advisor and elephant, and the count rejects a variant's lone forward
/// stepper. A longer opening push is exempt from the one-square bound,
/// which is what lets a FIDE pawn keep its double step.
///
/// White is classified and its colour twin takes the same answer, so a
/// slot exists for both colours of every pawn. Slots are handed out in
/// piece order and the tables are sized by their count, not by the piece
/// count: most variants have exactly two, so the masks below stay small
/// even on a board that fields thirty piece types.
///
/// Params:
/// - state: &State          -> variant whose pieces are classified
///
/// Return:
/// (Vec<usize>, Vec<usize>) -> piece index to slot, and slot to piece index
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

            for vector in vectors {
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
/// Every square a pawn can still advance onto from `square`, walked as the
/// closure of its quiet moves. A friendly pawn standing anywhere on this
/// set blocks the advance, which is what the doubled penalty charges, and
/// it is also the ground an enemy has to hold to stop a passer.
///
/// A straight mover traces its own file, a diagonal one fans out across
/// files, so the walk follows the piece's own move geometry rather than
/// assuming a file.
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
/// - state : &State -> precomputed relevant-move tables
/// - index : usize  -> pawn-like piece index
/// - square: usize  -> square the pawn stands on
///
/// Return:
/// Board            -> squares on the pawn's forward path
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

        for vector in vectors {
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
/// The square or squares a pawn reaches in one ordinary step: quiet, not
/// restricted to its first move, and strictly forward. Opening pushes of
/// two or more are left out so a pawn on its starting rank is read at the
/// same one-square frame as every other, and sideways steps are left out
/// since they win no ground.
///
/// A straight mover has one stop, a diagonal mover two. These are the
/// squares a friendly pawn defends to connect this one, and the squares an
/// enemy pawn watches to hold it back.
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
/// - state : &State -> precomputed relevant-move tables
/// - index : usize  -> pawn-like piece index
/// - square: usize  -> square the pawn stands on
///
/// Return:
/// Board            -> the pawn's immediate forward stop squares
fn derive_pawn_stop(state: &State, index: usize, square: usize) -> Board {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut stop = board!(state.statics.files, state.statics.ranks);
    let vectors = &state.statics.relevant_moves[index * board_size + square];

    for vector in vectors {
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
/// Every square a pawn of `color` could capture onto one of `targets`
/// from. Capture legs are stored in the mover's own frame, so each net
/// offset is turned by the colour sign before it is applied.
///
/// Asked of the friendly colour with the pawn's own square and its stops
/// as targets, this is the set of squares that connect the pawn; asked of
/// the enemy colour with only the stops, it is the set that holds it back.
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
/// - state  : &State -> precomputed relevant-capture tables
/// - color  : u8     -> colour whose pawn captures are gathered
/// - targets: &Board -> squares a capture has to land on
///
/// Return:
/// Board             -> squares such a capture could come from
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

            for vector in vectors {
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
/// Every enemy square from which a pawn's advance could be stopped: the
/// path itself, where an enemy pawn stands in the way, and every square an
/// enemy pawn could capture from onto the path or onto the pawn. A pawn
/// none of whose interference squares is occupied is passed.
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
/// - state : &State -> precomputed relevant-capture tables
/// - index : usize  -> pawn-like piece index
/// - square: usize  -> square the pawn stands on
/// - path  : &Board -> the pawn's forward path
///
/// Return:
/// Board            -> enemy squares that stop the passer
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
/// The file offsets, relative to a pawn, at which a friendly pawn could
/// ever defend it or the square it advances to. A pawn with no friendly
/// pawn on any of these files, at any rank at all, is isolated — which is
/// a weaker statement than being undefended right now, and the reason the
/// two are priced apart.
///
/// Each capture leg gives the offset a defender sits at directly behind,
/// and each capture leg combined with each forward step gives the offset a
/// defender sits at beside, guarding the stop rather than the pawn. FIDE
/// yields {-1, +1}, Berolina {-1, 0, +1}, and a shogi soldier {0}: a piece
/// that captures the way it moves is only ever defended from its own file.
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
/// - state: &State -> precomputed relevant move and capture tables
/// - index: usize  -> pawn-like piece index
///
/// Return:
/// Vec<i32>        -> sorted, de-duplicated supporting file offsets
fn derive_pawn_support_files(state: &State, index: usize) -> Vec<i32> {
    let board_size = state.statics.board_size;
    let sign = -2 * p_color!(&state.statics.pieces[index]) as i32 + 1;

    let mut capture_files: Vec<i32> = Vec::new();
    let mut step_files: Vec<i32> = Vec::new();

    for square in 0..board_size {
        let captures =
            &state.statics.relevant_captures[index * board_size + square];

        for vector in captures {
            let capture_file = vector_offset!(vector).0;

            if !capture_files.contains(&capture_file) {
                capture_files.push(capture_file);
            }
        }

        let moves =
            &state.statics.relevant_moves[index * board_size + square];

        for vector in moves {
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
/// How far along toward promoting a pawn on `square` already is, squared
/// and held in 256ths, so the last ranks are worth far more than the
/// first. This is the same gradient the promotion half of the piece-square
/// tables lays down, read here to scale what a passer is worth rather than
/// what standing there is worth. A pawn with no promotion zone falls back
/// to its distance from the far edge in its own forward direction.
///
/// ```text
/// steps to promote    8    6    4    2    1
/// advancement         0   16   64  144  196
/// ```
///
/// The row above is an eight-rank board, and the squaring is what bends it:
/// the first half of the walk is worth a quarter of the second.
///
/// Params:
/// - state : &State -> board geometry and promotion zones
/// - index : usize  -> pawn-like piece index
/// - square: usize  -> square the pawn stands on
///
/// Return:
/// i32              -> advancement in 256ths, squared
fn derive_pawn_advancement(state: &State, index: usize, square: usize) -> i32 {
    let files = state.statics.files as i32;
    let ranks = state.statics.ranks as i32;
    let closest =
        derive_closest_promotion(state, index as PieceIndex, square);

    let advancement = if closest.is_finite() {
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
/// Builds everything `pawn_structure!` reads: which pieces are pawns, the
/// four masks each of them tests from every square it could stand on, what
/// a passer on each of those squares is worth, and the flat worth of being
/// connected and the flat cost of being doubled, isolated, or backward.
///
/// - path         : the squares still ahead, where a doubled pawn stands
/// - interference : the enemy squares that stop it, none of them held
///                  meaning the pawn is passed
/// - support      : the friendly squares that connect it or guard its
///                  stop
/// - backward     : the enemy squares watching that stop, holding the
///                  pawn back
///
/// Every mask is a full board, so the evaluation is bounded by how many
/// pawns are on the board rather than by how wide the board is, and every
/// test it runs is a single bit read. The tables are indexed by pawn slot
/// rather than piece index, so a variant fielding thirty piece types and
/// two pawns pays for two.
///
/// A passer is priced off what promoting it would gain — the dearest
/// piece it could become, less what it is worth standing there — scaled
/// by how far along it already is, so a passer one square from promoting
/// is worth most of a piece and one still at home is worth nearly
/// nothing. A pawn its rules never promote is priced off its own value
/// instead. The structure terms are shares of the pawn itself, since each
/// is a correction to that pawn's worth and not a statement about the
/// army standing behind it.
///
/// - passer    : the promotion gain, by ratio, by advancement
/// - connected : a share of the pawn, the two phases priced apart
/// - doubled   : a share of its opening value, both phases alike
/// - isolated  : that same share, taken over the support files
/// - backward  : that share again, taken over the watched stop
///
/// Only the connected term reads the endgame value. A fault is a fault at
/// either end of the taper, and pricing it twice would say a doubled pawn
/// matters less once the board empties, which is the opposite of true.
///
/// Params:
/// - state: &mut State -> variant whose pawn tables are rebuilt
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

    let promoted_opening = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_ovalue!(piece) as i32)
        .max()
        .unwrap_or(0);
    let promoted_endgame = state.statics.pieces.iter()
        .filter(|piece| p_color!(piece) == WHITE && !p_is_royal!(piece))
        .map(|piece| p_evalue!(piece) as i32)
        .max()
        .unwrap_or(0);

    for (slot, index) in pieces.iter().copied().enumerate() {
        let piece = &state.statics.pieces[index];
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

        for square in 0..board_size {
            let entry = slot * stride + square;
            let stop = derive_pawn_stop(state, index, square);
            let advancement =
                derive_pawn_advancement(state, index, square) as i64;

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
/// Prices the three advantages a material count does not already carry:
/// holding the move, holding more pieces than the other side rather than
/// dearer ones, and holding both copies of a piece that is worth more in
/// pairs than singly.
///
/// - tempo     : a small share of the dearest piece, never below its
///               floor
/// - imbalance : one share for a heavy piece of surplus, and another
///               for a light one
/// - pair      : a share again, but only for pieces bound to half the
///               board
///
/// The first two are scalars read straight off the dearest non-royal
/// piece. The third needs to know which pieces earn it, which is asked of
/// the rules rather than of a name: a piece bound to half the board covers
/// nothing of the other half, so a second copy is worth more than the
/// first was. Royals are skipped, a variant being free to field two of
/// them and forbid trading either. Both colours of a qualifying piece are
/// recorded, so evaluation reads the list without consulting the swap map.
///
/// Params:
/// - state: &mut State -> variant whose advantage scalars are filled
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
