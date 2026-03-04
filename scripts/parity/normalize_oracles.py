#!/usr/bin/env python3
"""Normalize manifest oracle lists for WGSL parity rows."""

from __future__ import annotations

import argparse
import pathlib
import sys

HEADER_COLUMNS = 13
MANIFEST_ORACLE_IDX = 9
MANIFEST_DOMAIN_IDX = 1
MANIFEST_KIND_IDX = 3
MANIFEST_EXPECTED_IDX = 4


def split_oracle_tokens(raw: str) -> list[str]:
    return [token.strip().lower() for token in raw.split(",") if token.strip()]


def normalize_oracle_field(raw: str, expected: str) -> str:
    tokens = split_oracle_tokens(raw)
    required = "naga-pass" if expected == "pass" else "naga-fail"
    opposite = "naga-fail" if required == "naga-pass" else "naga-pass"

    unique_non_naga: list[str] = []
    has_spirv_val = False

    for token in tokens:
        if token == "spirv-val":
            has_spirv_val = True
        elif token == required or token == opposite:
            # handled separately to enforce exactly one matching token
            continue
        elif token not in unique_non_naga:
            unique_non_naga.append(token)

    ordered: list[str] = []
    if has_spirv_val:
        ordered.append("spirv-val")
    ordered.append(required)
    ordered.extend(sorted(unique_non_naga))
    return ",".join(ordered)


def main() -> int:
    parser = argparse.ArgumentParser(description="Normalize WGSL oracle columns in manifest.tsv")
    parser.add_argument("--manifest", default="test/parity/manifest.tsv")
    parser.add_argument("--check", action="store_true", help="validate changes without writing")
    args = parser.parse_args()

    manifest_path = pathlib.Path(args.manifest)
    lines = manifest_path.read_text(encoding="utf-8").splitlines()

    updated_rows = 0
    updated_lines: list[str] = []

    for line_no, line in enumerate(lines, start=1):
        stripped = line.strip()
        if line_no == 1 or not stripped or stripped.startswith("#"):
            updated_lines.append(line)
            continue

        parts = line.split("\t")
        if len(parts) != HEADER_COLUMNS:
            updated_lines.append(line)
            continue

        domain = parts[MANIFEST_DOMAIN_IDX].strip().lower()
        expected = parts[MANIFEST_EXPECTED_IDX].strip().lower()
        kind = parts[MANIFEST_KIND_IDX].strip().lower()

        should_enforce_naga = (
            domain == "wgsl"
            and expected in {"pass", "fail"}
            and kind not in {"backlog", "backlog-unmapped"}
        )

        if should_enforce_naga:
            normalized_oracles = normalize_oracle_field(parts[MANIFEST_ORACLE_IDX], expected)
            if normalized_oracles != parts[MANIFEST_ORACLE_IDX]:
                updated_rows += 1
                parts[MANIFEST_ORACLE_IDX] = normalized_oracles
                updated_lines.append("\t".join(parts))
            else:
                updated_lines.append(line)
            continue

        updated_lines.append(line)

    if args.check:
        if updated_lines != lines:
            print(
                f"normalize_oracles: would update {updated_rows} row(s) in {manifest_path}",
                file=sys.stderr,
            )
            return 1
        print(f"normalize_oracles: no changes in {manifest_path}")
        return 0

    if updated_lines != lines:
        manifest_path.write_text("\n".join(updated_lines) + "\n", encoding="utf-8")
    print(
        f"normalize_oracles: updated {updated_rows} row(s) in {manifest_path}",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
