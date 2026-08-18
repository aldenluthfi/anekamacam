#!/usr/bin/env bash
set -euo pipefail

# Records and checks where a binary in bin/ came from.
#
# A copied binary that merely differs from the previous copy proves nothing,
# so every build writes a sidecar record naming the commit it was built from,
# the configs and dicts that were embedded into it, and the hash of the file
# itself. `verify` rebuilds that commit and compares the result.
#
# Two builds of one commit are not byte-identical: the linker derives LC_UUID
# from the build path and the ad-hoc signature hashes the image including that
# UUID, so the files differ in exactly those 48 bytes and nowhere else. The
# record therefore carries two hashes. `md5` is the file as copied and only
# answers whether it changed on disk since the build. `content_md5` is the
# image with its signature removed and its UUID zeroed, so it is stable across
# build paths and is what a rebuild is compared against.
#
# Records live next to their binary as bin/<name>.provenance and are ignored
# by git along with bin/ itself.
#
# Usage:
#   provenance.sh record <binary> <ref>
#   provenance.sh show <binary> [...]
#   provenance.sh verify <binary> [...]

ROOT=$(git rev-parse --show-toplevel)
cd "$ROOT"

hash_file() {
	if command -v md5 >/dev/null 2>&1; then
		md5 -q "$1"
	else
		md5sum "$1" | awk '{print $1}'
	fi
}

hash_stream() {
	if command -v md5 >/dev/null 2>&1; then
		md5 -q
	else
		md5sum | awk '{print $1}'
	fi
}

# Hash of every file in a directory, name and content, so a changed or
# added resource shows up even though the directory itself is unchanged.
hash_tree() {
	local dir=$1

	find "$dir" -type f -print0 |
		sort -z |
		xargs -0 cat |
		hash_stream
}

# Hash of a Mach-O image with the two build-path-dependent regions taken out:
# the ad-hoc code signature, dropped whole, and the LC_UUID payload, zeroed
# where it appears. Everything the compiler produced is left alone.
content_hash() {
	local binary=$1 copy uuid

	copy=$(mktemp "${TMPDIR:-/tmp}/anekamacam-content.XXXXXX")
	cp "$binary" "$copy"
	codesign --remove-signature "$copy" 2>/dev/null || true

	uuid=$(otool -l "$copy" |
		awk '$1 == "uuid" { gsub(/-/, "", $2); print $2; exit }')

	if [[ -z "$uuid" ]]; then
		rm -f "$copy"
		echo "ERROR: no LC_UUID found in $binary" >&2
		exit 1
	fi

	UUID="$uuid" perl -0777 -pe '
		BEGIN { $uuid = pack "H*", $ENV{UUID} }
		s/\Q$uuid\E/"\0" x 16/ge
	' "$copy" | hash_stream

	rm -f "$copy"
}

field() {
	local record=$1 key=$2

	awk -v key="$key" '$1 == key { $1 = ""; sub(/^ +/, ""); print }' \
		"$record"
}

record_one() {
	local binary=$1 ref=$2 record="$1.provenance"
	local commit subject dirty handshake

	if [[ ! -x "$binary" ]]; then
		echo "ERROR: not an executable: $binary" >&2
		exit 1
	fi

	if ! commit=$(git rev-parse --verify -q "$ref^{commit}"); then
		echo "ERROR: cannot resolve ref: $ref" >&2
		exit 1
	fi

	subject=$(git log -1 --format=%s "$commit")
	dirty=no
	if [[ -n "$(git status --porcelain -- configs res/dicts)" ]]; then
		dirty=yes
	fi

	handshake=$(printf 'uci\nquit\n' | "$binary" uci 2>/dev/null)

	{
		echo "binary      $binary"
		echo "md5         $(hash_file "$binary")"
		echo "content_md5 $(content_hash "$binary")"
		echo "bytes       $(wc -c <"$binary" | tr -d ' ')"
		echo "built       $(date -u '+%Y-%m-%dT%H:%M:%SZ')"
		echo "ref         $ref"
		echo "commit      $commit"
		echo "subject     $subject"
		echo "branch      $(git rev-parse --abbrev-ref HEAD)"
		echo "target      ${CARGO_TARGET_DIR:-target}"
		echo "features    ${CARGO_FEATURES:-default}"
		echo "configs_md5 $(hash_tree configs)"
		echo "dicts_md5   $(hash_tree res/dicts)"
		echo "resources   $dirty"
		echo "uci_id      $(
			awk '$1 == "id" && $2 == "name" { $1 = ""; $2 = "";
				sub(/^ +/, ""); print }' <<<"$handshake"
		)"
		echo "variants    $(
			awk '/^option name UCI_Variant/ { print gsub(/ var /, "") }' \
				<<<"$handshake"
		)"
		echo "options_md5 $(
			awk '/^option name /' <<<"$handshake" | hash_stream
		)"
	} >"$record"

	echo "recorded $record"
}

show_one() {
	local record="$1.provenance"

	if [[ ! -f "$record" ]]; then
		echo "ERROR: no record for $1; build it with build-stages.sh" >&2
		exit 1
	fi

	echo "--- $1 ---"
	cat "$record"
}

# Rebuilds the recorded commit into a throwaway target directory and
# compares its content hash with the recorded one. Embedded resources are taken
# from the working tree, exactly as build-stages.sh takes them, so a
# resource edit since the build is reported as the cause of a mismatch
# rather than silently changing the answer.
verify_one() {
	local binary=$1 record="$1.provenance"
	local commit recorded rebuilt build_root worktree

	if [[ ! -f "$record" ]]; then
		echo "ERROR: no record for $binary" >&2
		exit 1
	fi

	commit=$(field "$record" commit)
	recorded=$(field "$record" content_md5)

	if [[ "$(hash_file "$binary")" != "$(field "$record" md5)" ]]; then
		echo "FAIL $binary: file changed since it was recorded"
		return 1
	fi

	if [[ "$(hash_tree configs)" != "$(field "$record" configs_md5)" ]] ||
		[[ "$(hash_tree res/dicts)" != "$(field "$record" dicts_md5)" ]]; then
		echo "FAIL $binary: embedded resources changed since the build"
		return 1
	fi

	build_root=$(mktemp -d "${TMPDIR:-/tmp}/anekamacam-verify.XXXXXX")
	worktree="$build_root/worktree"

	git worktree add --detach "$worktree" "$commit" >/dev/null
	mkdir -p "$worktree/res/dicts" "$worktree/configs"
	cp res/dicts/* "$worktree/res/dicts/"
	cp configs/* "$worktree/configs/"

	(
		cd "$worktree"
		CARGO_TARGET_DIR="$build_root/target" cargo build --release
	) >/dev/null 2>&1

	rebuilt=$(content_hash "$build_root/target/release/anekamacam")

	git worktree remove --force "$worktree" 2>/dev/null || true
	rm -rf "$build_root"

	if [[ "$rebuilt" != "$recorded" ]]; then
		echo "FAIL $binary: $commit rebuilds to content $rebuilt, not $recorded"
		return 1
	fi

	echo "OK   $binary: $commit rebuilds to content $recorded"
}

case "${1:-}" in
record)
	if [[ $# -ne 3 ]]; then
		echo "usage: provenance.sh record <binary> <ref>" >&2
		exit 1
	fi
	record_one "$2" "$3"
	;;
show)
	shift
	if [[ $# -eq 0 ]]; then
		echo "usage: provenance.sh show <binary> [...]" >&2
		exit 1
	fi
	for binary in "$@"; do show_one "$binary"; done
	;;
verify)
	shift
	if [[ $# -eq 0 ]]; then
		echo "usage: provenance.sh verify <binary> [...]" >&2
		exit 1
	fi
	failed=0
	for binary in "$@"; do verify_one "$binary" || failed=1; done
	exit "$failed"
	;;
*)
	echo "usage: provenance.sh record|show|verify ..." >&2
	exit 1
	;;
esac
