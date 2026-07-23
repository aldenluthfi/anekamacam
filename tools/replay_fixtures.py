#!/usr/bin/env python3
"""Replay protocol regression fixtures against a release engine.

Each fixture loads a full protocol position, runs release `d` state
verification, and starts a depth-one search. A normal position must produce a
move. A terminal or deliberately rejected position must produce exactly
`bestmove (none)`.

Because protocol move parsing matches text against `format_move`, successful
replay also checks protocol move formatting and parsing for every listed move.

Usage:
    tools/replay_fixtures.py [--binary PATH] [--timeout SECONDS]
"""

import argparse
import os
import queue
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass


ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DEFAULT_BINARY = os.path.join(ROOT, "target", "release", "anekamacam")


class ReplayError(RuntimeError):
    """Report a failed protocol fixture with its recent engine output."""


@dataclass(frozen=True)
class Fixture:
    """One position replay and its expected root-search result."""

    name: str
    protocol: str
    variant: str
    position: str
    expects_none: bool = False


class ProtocolEngine:
    """Drive one line-oriented engine protocol process."""

    def __init__(self, binary, protocol, variant, timeout, workdir):
        self.protocol = protocol
        self.variant = variant
        self.timeout = timeout
        self.output = queue.Queue()
        self.recent = []
        self.process = subprocess.Popen(
            [binary],
            cwd=workdir,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
        )
        self.reader = threading.Thread(target=self._read_output, daemon=True)
        self.reader.start()
        self._start()

    def _read_output(self):
        for line in self.process.stdout:
            self.output.put(line.rstrip("\n"))
        self.output.put(None)

    def _send(self, command):
        if self.process.poll() is not None:
            self._fail(f"engine exited with status {self.process.returncode}")
        try:
            self.process.stdin.write(command + "\n")
            self.process.stdin.flush()
        except BrokenPipeError as error:
            self._fail(f"engine pipe closed: {error}")

    def _wait_for(self, prefix):
        deadline = time.monotonic() + self.timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                self._fail(f"timed out waiting for {prefix!r}")
            try:
                line = self.output.get(timeout=remaining)
            except queue.Empty:
                self._fail(f"timed out waiting for {prefix!r}")
            if line is None:
                status = self.process.poll()
                self._fail(f"engine exited with status {status}")
            self.recent.append(line)
            self.recent = self.recent[-20:]
            if line.startswith(prefix):
                return line

    def _fail(self, message):
        recent = "\n".join(self.recent[-12:])
        if recent:
            message = f"{message}\nrecent output:\n{recent}"
        raise ReplayError(message)

    def _start(self):
        handshake = {
            "uci": ("uciok", "UCI_Variant"),
            "usi": ("usiok", "USI_Variant"),
            "ucci": ("ucciok", "UCCI_Variant"),
        }
        ready, option = handshake[self.protocol]
        self._send(self.protocol)
        self._wait_for(ready)
        self._send(f"setoption name {option} value {self.variant}")
        self._send("isready")
        self._wait_for("readyok")

    def run(self, fixture):
        self._send(fixture.position)
        self._send("d")
        self._send("isready")
        self._wait_for("readyok")
        self._send("go depth 1")
        bestmove = self._wait_for("bestmove ")

        if fixture.expects_none:
            if bestmove != "bestmove (none)":
                self._fail(
                    f"{fixture.name}: expected bestmove (none), got "
                    f"{bestmove!r}"
                )
        elif bestmove == "bestmove (none)":
            self._fail(f"{fixture.name}: expected a legal best move")

    def close(self):
        if self.process.poll() is not None:
            return
        try:
            self._send("quit")
            self.process.wait(timeout=5)
        except (ReplayError, subprocess.TimeoutExpired):
            self.process.kill()
            self.process.wait(timeout=5)


STANDARD_PREFIX = (
    "d2d4 g8f6 c1f4 c7c5 d4c5 e7e5 f4e5 b8c6 "
    "e5d6 f8e7 b1c3 d8a5 d1d2 b7b5 e1c1 e8g8"
)
GRAND_EN_PASSANT_PREFIX = "a3a4 i8i6 a4a5 i6i5 j3j5"
SHOGI_GOLD_CYCLE = "9h8h 5a6a 8h9h 6a5a"
SHOGI_SLIDER_CYCLE = "9h8g 5a6a 8g9h 6a5a"


def repeat_moves(moves, count):
    """Repeat one complete move cycle with single-space separators."""
    return " ".join([moves] * count)


def fixtures():
    """Return all five-variant protocol replay fixtures."""
    cases = [
        Fixture(
            "standard-castling-prefix",
            "uci",
            "standard",
            f"position startpos moves {STANDARD_PREFIX}",
        ),
        Fixture(
            "standard-reject-stale-en-passant",
            "uci",
            "standard",
            f"position startpos moves {STANDARD_PREFIX} c5b6",
            True,
        ),
        Fixture(
            "crazyhouse-captures-and-drops",
            "uci",
            "crazyhouse",
            "position startpos moves e2e4 d7d5 e4d5 d8d5 "
            "P@e6 P@e3",
        ),
        Fixture(
            "crazyhouse-bracket-pockets",
            "uci",
            "crazyhouse",
            "position fen 4k3/8/8/8/8/8/8/4K3[Pp] w - - 0 1 "
            "moves P@e4 P@e5",
        ),
        Fixture(
            "crazyhouse-promotion",
            "uci",
            "crazyhouse",
            "position fen 4k3/P7/8/8/8/8/8/4K3[] w - - 0 1 "
            "moves a7a8q",
        ),
        Fixture(
            "crazyhouse-en-passant",
            "uci",
            "crazyhouse",
            "position fen 4k3/8/8/3pP3/8/8/8/4K3[] w - d6 0 1 "
            "moves e5d6",
        ),
        Fixture(
            "crazyhouse-castling",
            "uci",
            "crazyhouse",
            "position startpos moves e2e4 e7e5 g1f3 b8c6 "
            "f1c4 g8f6 e1g1",
        ),
        Fixture(
            "grand-normal-capture",
            "uci",
            "grand",
            "position startpos moves e3e5 d8d6 e5d6",
        ),
        Fixture(
            "grand-en-passant",
            "uci",
            "grand",
            f"position startpos moves {GRAND_EN_PASSANT_PREFIX} i5j4",
        ),
        Fixture(
            "grand-reject-capture-payload",
            "uci",
            "grand",
            f"position startpos moves {GRAND_EN_PASSANT_PREFIX} i5j4j5",
            True,
        ),
        Fixture(
            "xiangqi-cannon-unload",
            "ucci",
            "xiangqi",
            "position fen "
            "4k4/9/9/9/r8/4P4/P8/9/C8/4K4 w - - 0 1 "
            "moves a1a5",
        ),
    ]

    shogi_positions = [
        (
            "king-gold",
            "4k4/9/9/9/9/9/9/G8/4K4 b - 1",
            SHOGI_GOLD_CYCLE,
        ),
        (
            "king-silver",
            "4k4/9/9/9/9/9/9/S8/4K4 b - 1",
            SHOGI_SLIDER_CYCLE,
        ),
        (
            "promoted-bishop",
            "4k4/9/9/9/9/9/9/+B8/4K4 b - 1",
            SHOGI_SLIDER_CYCLE,
        ),
    ]
    for name, sfen, cycle in shogi_positions:
        cases.append(
            Fixture(
                f"shogi-{name}-before-fourfold",
                "usi",
                "shogi",
                f"position sfen {sfen} moves {repeat_moves(cycle, 3)}",
            )
        )
        cases.append(
            Fixture(
                f"shogi-{name}-fourfold",
                "usi",
                "shogi",
                f"position sfen {sfen} moves {repeat_moves(cycle, 4)}",
                True,
            )
        )

    return cases


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", default=DEFAULT_BINARY)
    parser.add_argument("--timeout", type=float, default=30.0)
    return parser.parse_args()


def main():
    args = parse_args()
    binary = os.path.abspath(args.binary)
    if not os.path.isfile(binary) or not os.access(binary, os.X_OK):
        print(f"release engine not executable: {binary}", file=sys.stderr)
        print("run `cargo build --release` first", file=sys.stderr)
        return 1

    grouped = {}
    for fixture in fixtures():
        key = (fixture.protocol, fixture.variant)
        grouped.setdefault(key, []).append(fixture)

    try:
        with tempfile.TemporaryDirectory(
            prefix="anekamacam-replay-"
        ) as workdir:
            for (protocol, variant), cases in grouped.items():
                engine = ProtocolEngine(
                    binary, protocol, variant, args.timeout, workdir
                )
                try:
                    for fixture in cases:
                        engine.run(fixture)
                        print(f"ok {fixture.name}")
                finally:
                    engine.close()
    except ReplayError as error:
        print(f"FAILED: {error}", file=sys.stderr)
        return 1

    print(f"passed {len(fixtures())} replay fixtures")
    return 0


if __name__ == "__main__":
    sys.exit(main())
