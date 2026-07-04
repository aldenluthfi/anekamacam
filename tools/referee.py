#!/usr/bin/env python3
"""Minimal cutechess-like referee: fixed movetime + margin + pondering.

Mimics cutechess's UCI driving (verified against its uciengine.cpp):
normal think is `position` + `go [ponder] movetime N`; a ponder hit
sends bare `ponderhit`; a ponder miss sends `stop`, discards that
bestmove, then re-positions and goes. Records every think latency; a
think is a time loss when it exceeds movetime + margin. No board
validation -- engines keep their own state.

Usage: tools/referee.py [games] [variant] [opponent-cmd]
"""
import subprocess, sys, time, threading, queue

MOVETIME_MS = 1000
MARGIN_MS = 80
MAX_PLIES = 200
GAMES = int(sys.argv[1]) if len(sys.argv) > 1 else 2
VARIANT = sys.argv[2] if len(sys.argv) > 2 else "shogi"
OPPONENT = sys.argv[3] if len(sys.argv) > 3 else "fairy-stockfish"


class Engine:
    def __init__(self, name, cmd, options):
        self.name = name
        self.p = subprocess.Popen(cmd, stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, text=True,
                                  bufsize=1)
        self.q = queue.Queue()
        threading.Thread(target=self._reader, daemon=True).start()
        self.send("uci")
        self.wait_for("uciok", 10)
        for key, value in options.items():
            self.send(f"setoption name {key} value {value}")
        self.send("isready")
        self.wait_for("readyok", 30)
        self.pondering = False
        self.ponder_move = None
        self.lats = []

    def _reader(self):
        for line in self.p.stdout:
            self.q.put((time.monotonic(), line.strip()))

    def send(self, line):
        self.p.stdin.write(line + "\n")
        self.p.stdin.flush()

    def wait_for(self, prefix, timeout):
        deadline = time.monotonic() + timeout
        while True:
            remain = deadline - time.monotonic()
            if remain <= 0:
                return None, None
            try:
                ts, line = self.q.get(timeout=remain)
            except queue.Empty:
                return None, None
            if line.startswith(prefix):
                return ts, line

    def drain_bestmoves(self):
        extras = []
        while True:
            try:
                _, line = self.q.get_nowait()
                if line.startswith("bestmove"):
                    extras.append(line)
            except queue.Empty:
                return extras

    def newgame(self):
        self.pondering = False
        self.ponder_move = None
        self.send("ucinewgame")
        self.send("isready")
        self.wait_for("readyok", 30)

    def quit(self):
        self.send("quit")
        try:
            self.p.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.p.kill()


def position_cmd(moves):
    cmd = "position startpos"
    if moves:
        cmd += " moves " + " ".join(moves)
    return cmd


def think(eng, moves, opp_move):
    """Drive one think like cutechess; returns (move, ponder, lat, mode)."""
    if eng.pondering:
        eng.pondering = False
        if opp_move is not None and opp_move == eng.ponder_move:
            t0 = time.monotonic()
            eng.send("ponderhit")
            mode = "ponderhit"
        else:
            eng.send("stop")
            _, line = eng.wait_for("bestmove", 5)
            if line is None:
                return None, None, None, "stop-hang"
            eng.send(position_cmd(moves))
            t0 = time.monotonic()
            eng.send(f"go movetime {MOVETIME_MS}")
            mode = "miss-go"
    else:
        eng.send(position_cmd(moves))
        t0 = time.monotonic()
        eng.send(f"go movetime {MOVETIME_MS}")
        mode = "go"
    ts, line = eng.wait_for("bestmove", 15)
    if line is None:
        return None, None, None, mode + "-hang"
    lat = ts - t0
    parts = line.split()
    best = parts[1]
    ponder = parts[3] if len(parts) > 3 else None
    eng.lats.append((lat, mode))
    return best, ponder, lat, mode


aneka = Engine("anekamacam",
               ["./target/release/anekamacam", "uci"],
               {"UCI_Variant": VARIANT, "Ponder": "true"})
opp = Engine("opponent", [OPPONENT],
             {"UCI_Variant": VARIANT, "UCI_LimitStrength": "true",
              "UCI_Elo": 1700, "Ponder": "true"})

limit = (MOVETIME_MS + MARGIN_MS) / 1000
for game in range(GAMES):
    aneka.newgame()
    opp.newgame()
    players = [aneka, opp] if game % 2 == 0 else [opp, aneka]
    moves = []
    last_move = None
    result = f"ply-cap {MAX_PLIES}"
    ply = 0
    while ply < MAX_PLIES:
        eng = players[ply % 2]
        best, ponder, lat, mode = think(eng, moves, last_move)
        if best is None:
            result = f"{eng.name} HANG ({mode})"
            break
        if best in ("null", "(none)"):
            result = f"{eng.name} has no move (mated?) [{best}]"
            break
        if lat > limit:
            result = (f"{eng.name} LOSES ON TIME: {lat*1000:.0f}ms "
                      f"({mode}) at ply {ply}")
            break
        moves.append(best)
        last_move = best
        extras = eng.drain_bestmoves()
        if extras:
            print(f"  !! {eng.name} extra bestmoves: {extras}", flush=True)
        if ponder and ponder not in ("null", "(none)"):
            eng.send(position_cmd(moves + [ponder]))
            eng.send(f"go ponder movetime {MOVETIME_MS}")
            eng.pondering = True
            eng.ponder_move = ponder
        ply += 1
    for eng in (aneka, opp):
        if eng.pondering:
            eng.send("stop")
            eng.wait_for("bestmove", 5)
            eng.pondering = False
    print(f"game {game}: {result} after {ply} plies", flush=True)

for eng in (aneka, opp):
    lats = sorted(l for l, m in eng.lats)
    if not lats:
        continue
    over = [(l, m) for l, m in eng.lats if l > limit]
    by_mode = {}
    for l, m in eng.lats:
        by_mode.setdefault(m, []).append(l)
    print(f"\n{eng.name}: {len(lats)} thinks, max {lats[-1]*1000:.0f}ms, "
          f"p95 {lats[int(len(lats)*0.95)]*1000:.0f}ms, "
          f"overruns(> {limit*1000:.0f}ms): {len(over)}", flush=True)
    for m, ls in by_mode.items():
        ls.sort()
        print(f"  {m:10s} n={len(ls):3d} max={ls[-1]*1000:7.1f}ms",
              flush=True)

aneka.quit()
opp.quit()
