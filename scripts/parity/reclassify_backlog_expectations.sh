#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
MANIFEST_PATH="${1:-$ROOT_DIR/test/parity/manifest.tsv}"
TMP_BIN="${TMPDIR:-/tmp}/spirdo-reclassify-backlog"
TMP_OBJ_DIR="${TMPDIR:-/tmp}/spirdo-reclassify-backlog-obj"
mkdir -p "$TMP_OBJ_DIR"

cabal exec -- ghc -package spirdo -i"$ROOT_DIR" \
  "$ROOT_DIR/scripts/parity/reclassify_backlog_expectations.hs" \
  -odir "$TMP_OBJ_DIR" \
  -hidir "$TMP_OBJ_DIR" \
  -o "$TMP_BIN"

"$TMP_BIN" "$MANIFEST_PATH"
