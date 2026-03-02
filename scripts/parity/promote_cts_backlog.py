#!/usr/bin/env python3
import argparse
import csv
import pathlib
import re
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

DEFAULT_OWNER = "wesl-parity"
DEFAULT_EXIT_CRITERIA = "replace backlog row with executable fixture and concrete spec_ref mapping"
DEFAULT_BACKLOG_SECTION = "# Backlog (CTS candidates)"
BACKLOG_SOURCE = "inline:badConstAssertShader"


@dataclass(frozen=True)
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

    def to_tsv(self) -> list[str]:
        return [
            self.id,
            self.domain,
            self.spec_ref,
            self.kind,
            self.expected,
            self.source,
            self.origin,
            self.origin_ref,
            self.error_contains,
            self.oracles,
            self.options,
            self.owner,
            self.exit_criteria,
        ]


def read_manifest_rows(path: pathlib.Path) -> tuple[list[str], list[ManifestRow], set[str], set[str]]:
    raw = path.read_text(encoding="utf-8").splitlines()
    if not raw:
        raise RuntimeError(f"manifest is empty: {path}")

    header = raw[0].split("\t")
    if header != MANIFEST_HEADER:
        raise RuntimeError(f"manifest header mismatch: {path}")

    rows: list[ManifestRow] = []
    ids: set[str] = set()
    origin_refs: set[str] = set()
    for line_no, raw_line in enumerate(raw[1:], start=2):
        stripped = raw_line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        parts = raw_line.split("\t")
        if len(parts) != len(MANIFEST_HEADER):
            raise RuntimeError(f"manifest malformed line {line_no}: {path}")
        (
            case_id,
            case_domain,
            case_spec_ref,
            case_kind,
            case_expected,
            source,
            origin,
            origin_ref,
            error_contains,
            oracles,
            options,
            owner,
            exit_criteria,
        ) = parts
        row = ManifestRow(
            case_id,
            case_domain,
            case_spec_ref,
            case_kind,
            case_expected,
            source,
            origin,
            origin_ref,
            error_contains,
            oracles,
            options,
            owner,
            exit_criteria,
        )
        rows.append(row)
        ids.add(row.id)
        origin_refs.add(row.origin_ref)

    return raw, rows, ids, origin_refs


def read_tsv_rows(path: pathlib.Path) -> list[list[str]]:
    with path.open("r", encoding="utf-8", newline="") as f:
        return [row for row in csv.reader(f, delimiter="\t")]


def normalize_cts_id(case_id: str, origin_ref: str) -> str:
    ref = origin_ref.strip()
    if ref:
        return ref
    case = case_id.strip()
    if case.startswith("cts:"):
        return case[len("cts:") :]
    return case


def slugify_cts_id(cts_id: str) -> str:
    value = cts_id.lower().replace("::", ".").replace("/", ".")
    value = re.sub(r"[^a-z0-9.]+", ".", value)
    value = re.sub(r"\.+", ".", value).strip(".")
    return value if value else "unknown"


def derive_spec_ref(domain: str, cts_id: str, spec_ref: str) -> str:
    cleaned = spec_ref.strip()
    if cleaned and cleaned not in {"WGSL.UNMAPPED", "WESL.UNMAPPED"}:
        return cleaned
    prefix = "WESL" if domain.strip().lower() == "wesl" else "WGSL"
    return f"{prefix}.CTS.{slugify_cts_id(cts_id)}"


def is_unmapped_spec_ref(domain: str, spec_ref: str) -> bool:
    cleaned = spec_ref.strip()
    if cleaned in {"WGSL.UNMAPPED", "WESL.UNMAPPED"}:
        return True
    return (not cleaned) and (domain.strip().lower() != "wesl")


def infer_kind(domain: str, spec_ref: str) -> str:
    if is_unmapped_spec_ref(domain, spec_ref):
        return "backlog-unmapped"
    return "backlog"


def load_candidate_rows(path: pathlib.Path) -> list[ManifestRow]:
    out: list[ManifestRow] = []
    rows = read_tsv_rows(path)
    if not rows:
        return out
    for idx, row in enumerate(rows, start=1):
        if idx == 1:
            if row[: len(MANIFEST_HEADER)] != MANIFEST_HEADER:
                raise RuntimeError(f"candidates header mismatch: {path}")
            continue
        if not row:
            continue
        if len(row) < len(MANIFEST_HEADER):
            continue
        case = row[: len(MANIFEST_HEADER)]
        cts_id = normalize_cts_id(case[0], case[7])
        raw_spec_ref = case[2]
        row_kind = infer_kind(case[1], raw_spec_ref)
        derived_spec_ref = derive_spec_ref(case[1], cts_id, raw_spec_ref)
        effective_spec_ref = (
            raw_spec_ref.strip() if row_kind == "backlog-unmapped" and raw_spec_ref.strip() else derived_spec_ref
        )
        if row_kind == "backlog-unmapped" and not raw_spec_ref.strip():
            effective_spec_ref = (
                "WESL.UNMAPPED" if case[1].strip().lower() == "wesl" else "WGSL.UNMAPPED"
            )
        out.append(
            ManifestRow(
                id=case[0],
                domain=case[1] if case[1] else "wgsl",
                spec_ref=effective_spec_ref,
                kind=row_kind,
                expected="xfail",
                source=BACKLOG_SOURCE,
                origin="cts",
                origin_ref=cts_id,
                error_contains="",
                oracles="",
                options="",
                owner=case[11] if case[11] else DEFAULT_OWNER,
                exit_criteria=case[12] if case[12] else DEFAULT_EXIT_CRITERIA,
            )
        )
    return out


def read_rule_refs(path: pathlib.Path) -> tuple[set[str], list[str]]:
    lines = read_tsv_rows(path)
    seen: set[str] = set()
    ordered: list[str] = []
    for row in lines:
        if not row:
            continue
        token = row[0].strip()
        if not token or token == "spec_ref":
            continue
        if token in seen:
            continue
        seen.add(token)
        ordered.append(token)
    return seen, ordered


def rewrite_manifest(path: pathlib.Path, raw_lines: list[str], added_rows: list[ManifestRow]) -> int:
    if not added_rows:
        return 0
    added_rows = sorted(added_rows, key=lambda row: row.id)
    out_lines = list(raw_lines)
    if out_lines and out_lines[-1].strip() != "":
        out_lines.append("")

    # Avoid duplicating section header if script has already been applied in this file.
    section_header_present = any(line.strip() == DEFAULT_BACKLOG_SECTION for line in out_lines)
    if not section_header_present:
        out_lines.append(DEFAULT_BACKLOG_SECTION)
    out_lines.extend(["\t".join(r.to_tsv()) for r in added_rows])

    path.write_text("\n".join(out_lines) + "\n", encoding="utf-8")
    return len(added_rows)


def rewrite_rules(path: pathlib.Path, current_refs: set[str], ordered_refs: list[str], added_rows: list[ManifestRow]) -> int:
    add_refs = sorted({row.spec_ref for row in added_rows if row.kind == "backlog"})
    new_refs = sorted(current_refs | set(add_refs))
    if new_refs == ordered_refs:
        return 0
    path.write_text("spec_ref\n" + "".join(f"{r}\n" for r in new_refs), encoding="utf-8")
    return len(new_refs) - len(ordered_refs)


def promote_backlog(manifest_path: pathlib.Path, candidates_path: pathlib.Path, rules_path: pathlib.Path) -> int:
    raw_manifest_lines, manifest_rows, manifest_ids, manifest_origin_refs = read_manifest_rows(manifest_path)

    candidates = load_candidate_rows(candidates_path)
    to_promote = [
        row
        for row in candidates
        if row.id not in manifest_ids and row.origin_ref not in manifest_origin_refs
    ]
    if not to_promote:
        print("promote_cts_backlog: no new CTS candidate rows to promote")
        return 0

    existing_rules_set, existing_rules = read_rule_refs(rules_path)
    manifest_added = rewrite_manifest(manifest_path, raw_manifest_lines, to_promote)
    rules_added = rewrite_rules(rules_path, existing_rules_set, existing_rules, to_promote)
    print(
        f"promote_cts_backlog: wrote {manifest_added} manifest row(s), {rules_added} new rule ref(s)"
    )
    return manifest_added + rules_added


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Promote CTS candidate rows into manifest as backlog rows."
    )
    parser.add_argument("--manifest", default="test/parity/manifest.tsv")
    parser.add_argument("--candidates", default="test/parity/generated/cts_manifest_candidates.tsv")
    parser.add_argument("--rules", default="test/parity/rules.tsv")
    args = parser.parse_args()

    manifest_path = pathlib.Path(args.manifest)
    candidates_path = pathlib.Path(args.candidates)
    rules_path = pathlib.Path(args.rules)

    for p in (manifest_path, candidates_path, rules_path):
        if not p.exists():
            raise RuntimeError(f"missing required path: {p}")

    changed = promote_backlog(manifest_path, candidates_path, rules_path)
    return 0


if __name__ == "__main__":
    sys.exit(main())
