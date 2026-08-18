#!/usr/bin/env bash
set -euo pipefail

# Drop-pocket integrity sample for crazyhouse, checked against fairy-stockfish.
#
# The engine self-plays a batch of short games, and every game's whole move list
# is replayed by fairy-stockfish, which then prints its own position. A game
# passes only when the board, side to move, castling rights, promoted-piece
# marks and both hands agree exactly. fairy-stockfish silently stops applying a
# move list at the first move it rejects, so a rejected move always shows up as
# a position mismatch -- this stands in for the forfeit tag an external match
# runner would write, and needs no cutechess-cli.
#
# Hand divergence is the failure this suite exists for: a drops variant whose
# promoted pieces do not demote back to their base type on capture accumulates a
# wrong pocket, and every later drop from it is illegal. That defect once
# forfeited 466 of 2987 crazyhouse games.
#
# The engine writes moves in its own notation, which is converted to UCI here:
#
#   e2:e4      -> e2e4      separator dropped
#   f3*e5      -> f3e5      capture mark dropped
#   e5:d6*d5   -> e5d6      en passant also names the captured square
#   e1:g1 h1@f1 -> e1g1     castling also names the rook move
#   d7:d8U     -> d7d8n     promoted types T/U/V/W are rook/knight/bishop/queen
#   b@g4       -> B@g4      drops are always spelled with the uppercase letter
#
# Games are seeded so a failing game can be replayed on its own, and so a batch
# is not one line repeated.
#
# Usage:
#   tools/drop-integrity.sh BIN [GAMES] [PLIES] [DEPTH] [SECONDS]

BIN=${1:?usage: tools/drop-integrity.sh BIN [GAMES] [PLIES] [DEPTH] [SECONDS]}
GAMES=${2:-24}
PLIES=${3:-200}
DEPTH=${4:-5}
SECONDS_PER_MOVE=${5:-0.03}

FSF=${FSF:-fairy-stockfish}
command -v "$FSF" >/dev/null || { echo "$FSF not on PATH" >&2; exit 2; }

mismatches=0

for game in $(seq 1 "$GAMES"); do
    played=$(ANEKAMACAM_SEED=$game "$BIN" debug-headless play crazyhouse \
        "$DEPTH" "$SECONDS_PER_MOVE" 1 "$PLIES" 2>&1)
    ours=$(grep '^FEN: ' <<<"$played" | sed 's/^FEN: //')
    result=$(grep '^Result: ' <<<"$played" | sed 's/^Result: //')

    moves=$(grep -Ev '^(info |FEN: |Result: )' <<<"$played" \
        | grep -E '^[A-Za-z0-9@:*=+]+$' \
        | sed 's/[:*=]//g' \
        | sed 's/^\([a-h][1-8][a-h][1-8]\)[a-h][1-8]$/\1/' \
        | sed 's/^\([a-h][1-8][a-h][1-8]\)[a-h][1-8]@[a-h][1-8]$/\1/' \
        | sed 's/[Tt]$/r/; s/[Uu]$/n/; s/[Vv]$/b/; s/[Ww]$/q/' \
        | awk '{ if (substr($0, 2, 1) == "@")
                     print toupper(substr($0, 1, 1)) substr($0, 2)
                 else print }' \
        | tr '\n' ' ')
    plies=$(wc -w <<<"$moves" | tr -d ' ')

    theirs=$(printf 'setoption name UCI_Variant value crazyhouse\n%s\nd\nquit\n' \
        "position startpos moves $moves" | "$FSF" 2>/dev/null \
        | grep '^Fen: ' | sed 's/^Fen: //')

    our_board=$(awk '{print $1}' <<<"$ours" \
        | sed 's/T/R~/g; s/U/N~/g; s/V/B~/g; s/W/Q~/g' \
        | sed 's/t/r~/g; s/u/n~/g; s/v/b~/g; s/w/q~/g')
    our_hand=$(awk '{print $5}' <<<"$ours" | tr -d '/-' \
        | fold -w1 | sort | tr -d '\n')
    their_board=$(awk '{print $1}' <<<"$theirs" | sed 's/\[.*\]//')
    their_hand=$(awk '{print $1}' <<<"$theirs" \
        | sed -n 's/.*\[\(.*\)\].*/\1/p' | fold -w1 | sort | tr -d '\n')

    if [[ "$our_board" == "$their_board" \
        && "$(awk '{print $2}' <<<"$ours")" == "$(awk '{print $2}' <<<"$theirs")" \
        && "$(awk '{print $3}' <<<"$ours")" == "$(awk '{print $3}' <<<"$theirs")" \
        && "$our_hand" == "$their_hand" ]]; then
        echo "game $game agrees: $plies plies, $result, hand ${our_hand:--}"
    else
        mismatches=$((mismatches + 1))
        echo "game $game DIFFERS: $plies plies, $result"
        echo "  ours:   $our_board hand ${our_hand:--}"
        echo "  theirs: $their_board hand ${their_hand:--}"
        echo "  moves:  $moves"
    fi
done

echo "mismatching games: $mismatches of $GAMES"
[[ $mismatches -eq 0 ]]
