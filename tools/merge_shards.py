#!/usr/bin/env python3
"""merge_shards.py

Merge parallel `datagen` shard datasets into one game-disjoint corpus.

Each shard process writes `res/data/{variant}/latest.data` inside its own
working directory, numbering games `0..N` locally. Concatenating shards
directly would collide those game IDs and break the tuner's game-level
train/validation split (`game_id % 5`, see `src/debug/tuning.rs`).

This script reindexes every shard-local game ID to a fresh globally unique
ID (first-seen order) and writes the merged rows to
`res/data/{variant}/latest.data` at the repo root. Because IDs are assigned
sequentially across all games, the ~1/5 validation ratio is preserved while
collisions are impossible.

Usage:
    tools/merge_shards.py <variant> [--shards GLOB] [--root DIR] [--out PATH]

    <variant>       variant name, e.g. standard, shogi, xiangqi
    --shards GLOB   glob for shard directories (default: shard_*)
    --root DIR      directory the glob is relative to (default: cwd)
    --out PATH      merged output file
                    (default: <root>/res/data/<variant>/latest.data)
"""

import argparse
import glob
import os
import sys


def shard_files(root, pattern, variant):
    """Return the shard dataset paths matching the glob, in sorted order."""
    hits = sorted(glob.glob(os.path.join(root, pattern)))
    files = []
    for shard in hits:
        path = os.path.join(shard, "res", "data", variant, "latest.data")
        if os.path.isfile(path):
            files.append(path)
    return files


def merge(files, out_path):
    """Reindex game IDs across shards and write the merged corpus.

    Returns (games, rows, results) where results counts losses/draws/wins.
    """
    next_id = 0
    games = 0
    rows = 0
    results = {0.0: 0, 0.5: 0, 1.0: 0}

    os.makedirs(os.path.dirname(out_path), exist_ok=True)

    with open(out_path, "w", encoding="utf-8") as out:
        for path in files:
            local_to_global = {}
            with open(path, "r", encoding="utf-8") as shard:
                for line in shard:
                    line = line.strip()
                    if not line:
                        continue
                    game, fen, result = line.split(";", 2)
                    if game not in local_to_global:
                        local_to_global[game] = next_id
                        next_id += 1
                        games += 1
                        results[float(result)] = (
                            results.get(float(result), 0) + 1
                        )
                    out.write(f"{local_to_global[game]};{fen};{result}\n")
                    rows += 1

    return games, rows, results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("variant")
    parser.add_argument("--shards", default="shard_*")
    parser.add_argument("--root", default=".")
    parser.add_argument("--out", default=None)
    args = parser.parse_args()

    out_path = args.out or os.path.join(
        args.root, "res", "data", args.variant, "latest.data"
    )

    files = shard_files(args.root, args.shards, args.variant)
    if not files:
        print(
            f"no shard datasets for '{args.variant}' matching "
            f"'{args.shards}' under '{args.root}'",
            file=sys.stderr,
        )
        sys.exit(1)

    games, rows, results = merge(files, out_path)

    validation = sum(1 for i in range(games) if i % 5 == 0)
    print(f"merged {len(files)} shards -> {out_path}")
    print(f"  games {games}, positions {rows}")
    print(
        f"  results: {results.get(0.0, 0)} losses, "
        f"{results.get(0.5, 0)} draws, {results.get(1.0, 0)} wins"
    )
    print(f"  split: {games - validation} train games, {validation} validation")


if __name__ == "__main__":
    main()
