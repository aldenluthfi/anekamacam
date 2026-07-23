#!/usr/bin/env python3
"""Standard-variant throughput suite for Strength Iteration 2 phase gates.

Drives each engine binary through a fixed set of standard positions at a
fixed search depth, one independent process per run, and reports geometric-mean
nodes, time, and NPS. The first binary is the baseline; later binaries report
their ratio against it. Search is nondeterministic (Zobrist keys reseed per
process), so several runs per position are averaged.

Usage:
    tools/speed_suite.py --depth 14 --runs 6 bin/phaseA-2 bin/phaseB-2
"""

import argparse
import math
import os
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

POSITIONS = [
    ("startpos",
     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
    ("ruy-lopez",
     "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"),
    ("kiwipete",
     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"),
    ("middlegame",
     "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 "
     "w - - 0 10"),
    ("endgame",
     "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"),
]


def run_once(binary, fen, depth, timeout):
    """Search one position to fixed depth; return (nodes, time_ms, nps)."""
    proc = subprocess.Popen(
        [binary],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
        cwd=ROOT,
    )
    script = (
        "uci\nisready\n"
        f"position fen {fen}\n"
        f"go depth {depth}\n"
    )
    proc.stdin.write(script)
    proc.stdin.flush()

    nodes = time_ms = nps = 0
    try:
        while True:
            line = proc.stdout.readline()
            if not line:
                break
            if line.startswith("info") and " nodes " in line:
                parts = line.split()
                for key in ("nodes", "time", "nps"):
                    if key in parts:
                        value = int(parts[parts.index(key) + 1])
                        if key == "nodes":
                            nodes = value
                        elif key == "time":
                            time_ms = value
                        else:
                            nps = value
            if line.startswith("bestmove"):
                break
    finally:
        proc.stdin.write("quit\n")
        proc.stdin.flush()
        proc.wait(timeout=timeout)

    return nodes, time_ms, nps


def geomean(values):
    values = [v for v in values if v > 0]
    if not values:
        return 0.0
    return math.exp(sum(math.log(v) for v in values) / len(values))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--depth", type=int, default=14)
    parser.add_argument("--runs", type=int, default=6)
    parser.add_argument("--timeout", type=int, default=120)
    parser.add_argument("binaries", nargs="+")
    args = parser.parse_args()

    summary = {}
    for binary in args.binaries:
        node_samples, nps_samples = [], []
        for _name, fen in POSITIONS:
            for _ in range(args.runs):
                nodes, _time_ms, nps = run_once(
                    binary, fen, args.depth, args.timeout
                )
                node_samples.append(nodes)
                nps_samples.append(nps)
        summary[binary] = (geomean(node_samples), geomean(nps_samples))
        print(
            f"{binary:24s} "
            f"geo-nodes {summary[binary][0]:12.0f} "
            f"geo-nps {summary[binary][1]:12.0f}"
        )

    baseline = args.binaries[0]
    base_nodes, base_nps = summary[baseline]
    print()
    print(f"baseline: {baseline}")
    for binary in args.binaries[1:]:
        nodes, nps = summary[binary]
        node_ratio = nodes / base_nodes if base_nodes else 0.0
        nps_ratio = nps / base_nps if base_nps else 0.0
        print(
            f"{binary:24s} "
            f"nodes x{node_ratio:6.3f}  nps x{nps_ratio:6.3f}"
        )


if __name__ == "__main__":
    main()
