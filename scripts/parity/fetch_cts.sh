#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
PINS_PATH="${1:-$ROOT_DIR/test/parity/pins.json}"
DEST_DIR="${2:-$ROOT_DIR/test/parity/vendor/cts}"

if [[ ! -f "$PINS_PATH" ]]; then
  echo "fetch_cts: missing pins file: $PINS_PATH" >&2
  exit 1
fi

repo_url="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["cts_repo"])' "$PINS_PATH")"
cts_commit="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["cts_commit"])' "$PINS_PATH")"

if [[ -z "$cts_commit" || "$cts_commit" == "UNPINNED" ]]; then
  echo "fetch_cts: pins.json has no pinned cts_commit" >&2
  exit 1
fi

mkdir -p "$(dirname "$DEST_DIR")"

if [[ ! -d "$DEST_DIR/.git" ]]; then
  git clone --no-checkout "$repo_url" "$DEST_DIR"
fi

git -C "$DEST_DIR" fetch --depth 1 origin "$cts_commit"
git -C "$DEST_DIR" checkout --detach FETCH_HEAD

echo "fetch_cts: checked out $(git -C "$DEST_DIR" rev-parse HEAD) at $DEST_DIR"
