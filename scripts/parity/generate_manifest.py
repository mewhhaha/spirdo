#!/usr/bin/env python3
import argparse
import csv
import pathlib
import sys
from dataclasses import dataclass

MANIFEST_HEADER = [
    "id",
    "domain",
    "spec_ref",
    "kind",
    "expected",
    "source",
    "origin",
    "origin_ref",
    "error_contains",
    "oracles",
    "options",
    "owner",
    "exit_criteria",
]


@dataclass
class ManifestRow:
    id: str
    domain: str
    spec_ref: str
    kind: str
    expected: str
    source: str
    origin: str
    origin_ref: str
    error_contains: str
    oracles: str
    options: str
    owner: str
    exit_criteria: str


def read_tsv_rows(path: pathlib.Path) -> list[list[str]]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.reader(f, delimiter="\t")
        return [row for row in reader]


def write_tsv(path: pathlib.Path, header: list[str], rows: list[list[str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f, delimiter="\t", lineterminator="\n")
        writer.writerow(header)
        writer.writerows(rows)


def read_rules(path: pathlib.Path) -> list[str]:
    rows = read_tsv_rows(path)
    out: list[str] = []
    for row in rows:
        if not row:
            continue
        token = row[0].strip()
        if not token or token.startswith("#") or token == "spec_ref":
            continue
        out.append(token)
    return out


def read_manifest_origin_refs(path: pathlib.Path) -> set[str]:
    lines = path.read_text(encoding="utf-8").splitlines() if path.exists() else []
    refs: set[str] = set()
    for i, line in enumerate(lines, start=1):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        parts = line.split("\t")
        if i == 1 and parts[0] == "id":
            continue
        if len(parts) < len(MANIFEST_HEADER):
            continue
        if parts[6].strip() == "cts":
            refs.add(parts[7].strip())
    return refs


def build_candidates(cts_index: pathlib.Path, existing_origin_refs: set[str]) -> list[list[str]]:
    rows = read_tsv_rows(cts_index)
    out: list[list[str]] = []
    for idx, row in enumerate(rows, start=1):
        if idx == 1:
            continue
        if len(row) < 6:
            continue
        cts_id, rel_path, kind, expected, domain, spec_ref_hint = [c.strip() for c in row[:6]]
        if not cts_id or cts_id in existing_origin_refs:
            continue
        spec_ref = spec_ref_hint if spec_ref_hint else "WGSL.UNMAPPED"
        lower_rel_path = rel_path.lower()
        has_shader_text = lower_rel_path.endswith(".wgsl") or lower_rel_path.endswith(".wesl")
        oracles = "spirv-val" if has_shader_text and expected == "pass" else ""
        out.append(
            [
                f"cts:{cts_id}",
                domain if domain else "wgsl",
                spec_ref,
                kind if kind else "cts",
                expected if expected in {"pass", "fail", "xfail"} else "pass",
                rel_path,
                "cts",
                cts_id,
                "",
                oracles,
                "",
                "",
                "",
            ]
        )
    out.sort(key=lambda r: r[0])
    return out


def maybe_check(path: pathlib.Path, header: list[str], rows: list[list[str]], check: bool) -> int:
    rendered = "\n".join(["\t".join(header)] + ["\t".join(row) for row in rows]) + "\n"
    if check:
        existing = path.read_text(encoding="utf-8") if path.exists() else ""
        if existing != rendered:
            print(f"generate_manifest: drift detected in {path}", file=sys.stderr)
            return 1
        return 0
    write_tsv(path, header, rows)
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Generate deterministic parity support artifacts.")
    parser.add_argument("--manifest", default="test/parity/manifest.tsv")
    parser.add_argument("--rules", default="test/parity/rules.tsv")
    parser.add_argument("--cts-index", default="test/parity/generated/cts_index.tsv")
    parser.add_argument("--output-candidates", default="test/parity/generated/cts_manifest_candidates.tsv")
    parser.add_argument("--output-wgsl-rules", default="test/parity/generated/wgsl_rule_index.tsv")
    parser.add_argument("--output-wesl-rules", default="test/parity/generated/wesl_rule_index.tsv")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()

    manifest_path = pathlib.Path(args.manifest)
    rules_path = pathlib.Path(args.rules)
    cts_index_path = pathlib.Path(args.cts_index)

    rules = read_rules(rules_path)
    wgsl_rows = [[r] for r in rules if r.startswith("WGSL.")]
    wesl_rows = [[r] for r in rules if r.startswith("WESL.")]

    existing_refs = read_manifest_origin_refs(manifest_path)
    candidates = build_candidates(cts_index_path, existing_refs)

    code = 0
    code |= maybe_check(pathlib.Path(args.output_wgsl_rules), ["spec_ref"], wgsl_rows, args.check)
    code |= maybe_check(pathlib.Path(args.output_wesl_rules), ["spec_ref"], wesl_rows, args.check)
    code |= maybe_check(pathlib.Path(args.output_candidates), MANIFEST_HEADER, candidates, args.check)

    if not args.check:
        print(
            f"generate_manifest: wrote {len(candidates)} candidate rows, "
            f"{len(wgsl_rows)} WGSL rules, {len(wesl_rows)} WESL rules"
        )
    return 1 if code else 0


if __name__ == "__main__":
    sys.exit(main())
