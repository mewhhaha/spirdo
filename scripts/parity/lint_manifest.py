#!/usr/bin/env python3
import argparse
import csv
import pathlib
import sys

HEADER = [
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

VALID_EXPECTED = {"pass", "fail", "xfail"}
VALID_ORACLES = {"spirv-val", "naga-pass", "naga-fail"}
VALID_ORIGINS = {"manual", "cts"}
UNMAPPED_SPEC_REFS = {"", "WGSL.UNMAPPED", "WESL.UNMAPPED"}


def trim(s: str) -> str:
    return s.strip()


def parse_rules(path: pathlib.Path) -> set[str]:
    if not path.exists():
        raise RuntimeError(f"missing rules file: {path}")
    out: set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as f:
        for row in csv.reader(f, delimiter="\t"):
            if not row:
                continue
            token = trim(row[0])
            if not token or token.startswith("#") or token == "spec_ref":
                continue
            out.add(token)
    if not out:
        raise RuntimeError(f"no spec_ref entries in {path}")
    return out


def parse_id_set(path: pathlib.Path) -> set[str]:
    if not path.exists():
        return set()
    out: set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as f:
        for i, row in enumerate(csv.reader(f, delimiter="\t"), start=1):
            if not row:
                continue
            token = trim(row[0])
            if not token or token.startswith("#"):
                continue
            if i == 1 and token == "cts_id":
                continue
            out.add(token)
    return out


def parse_option_tokens(raw: str) -> list[str]:
    return [trim(tok) for tok in raw.split(",") if trim(tok)]


def validate_options(tokens: list[str]) -> list[str]:
    errs: list[str] = []
    has_base = "base" in tokens
    has_no_base = "no-base" in tokens
    if has_base and has_no_base:
        errs.append("options cannot include both base and no-base")
    for tok in tokens:
        if tok in {"base", "no-base", "sampler:combined", "sampler:separate", "spec:strict", "spec:parity"}:
            continue
        if tok.startswith("feature:") and len(tok) > len("feature:"):
            continue
        errs.append(f"unknown option token: {tok}")
    return errs


def main() -> int:
    parser = argparse.ArgumentParser(description="Lint parity manifest schema and consistency.")
    parser.add_argument("--manifest", default="test/parity/manifest.tsv")
    parser.add_argument("--rules", default="test/parity/rules.tsv")
    parser.add_argument("--allowlist", default="test/parity/cts_allowlist.tsv")
    parser.add_argument("--blocklist", default="test/parity/cts_blocklist.tsv")
    args = parser.parse_args()

    manifest_path = pathlib.Path(args.manifest)
    rules = parse_rules(pathlib.Path(args.rules))
    allow = parse_id_set(pathlib.Path(args.allowlist))
    block = parse_id_set(pathlib.Path(args.blocklist))

    errors: list[str] = []
    if not manifest_path.exists():
        errors.append(f"missing manifest file: {manifest_path}")
    else:
        ids_seen: set[str] = set()
        refs_seen: set[str] = set()

        with manifest_path.open("r", encoding="utf-8") as f:
            lines = f.read().splitlines()

        if not lines:
            errors.append("manifest is empty")
        else:
            if lines[0] != "\t".join(HEADER):
                errors.append("manifest header does not match expected schema")

        for line_no, line in enumerate(lines, start=1):
            stripped = line.strip()
            if line_no == 1:
                continue
            if not stripped or stripped.startswith("#"):
                continue
            cols = line.split("\t")
            if len(cols) != len(HEADER):
                errors.append(f"line {line_no}: expected {len(HEADER)} columns, got {len(cols)}")
                continue

            row = {HEADER[i]: trim(cols[i]) for i in range(len(HEADER))}

            case_id = row["id"]
            if not case_id:
                errors.append(f"line {line_no}: id must not be empty")
            elif case_id in ids_seen:
                errors.append(f"line {line_no}: duplicate id: {case_id}")
            else:
                ids_seen.add(case_id)

            expected = row["expected"]
            if expected not in VALID_EXPECTED:
                errors.append(f"line {line_no}: invalid expected value: {expected}")

            origin = row["origin"]
            if origin not in VALID_ORIGINS:
                errors.append(f"line {line_no}: invalid origin: {origin}")

            source = row["source"]
            kind = row["kind"].lower()
            if origin == "cts" and kind not in {"backlog", "backlog-unmapped"} and source.startswith("inline:"):
                errors.append(f"line {line_no}: cts-origin rows must use file-backed source")

            origin_ref = row["origin_ref"]
            if not origin_ref:
                errors.append(f"line {line_no}: origin_ref must not be empty")

            spec_ref = row["spec_ref"]
            if kind == "backlog-unmapped":
                if spec_ref and spec_ref not in UNMAPPED_SPEC_REFS:
                    errors.append(f"line {line_no}: backlog-unmapped rows require spec_ref WGSL.UNMAPPED or WESL.UNMAPPED")
            else:
                if not spec_ref:
                    errors.append(f"line {line_no}: spec_ref must not be empty")
                else:
                    refs_seen.add(spec_ref)
                    if spec_ref not in rules:
                        errors.append(f"line {line_no}: spec_ref not found in rules.tsv: {spec_ref}")

            oracle_tokens = [trim(tok).lower() for tok in row["oracles"].split(",") if trim(tok)]
            bad_oracles = [tok for tok in oracle_tokens if tok not in VALID_ORACLES]
            if bad_oracles:
                errors.append(f"line {line_no}: unknown oracle token(s): {bad_oracles}")
            if "naga-pass" in oracle_tokens and "naga-fail" in oracle_tokens:
                errors.append(f"line {line_no}: cannot request both naga-pass and naga-fail")

            option_errs = validate_options(parse_option_tokens(row["options"]))
            for err in option_errs:
                errors.append(f"line {line_no}: {err}")

            if expected == "xfail":
                if not row["owner"]:
                    errors.append(f"line {line_no}: xfail rows require owner")
                if not row["exit_criteria"]:
                    errors.append(f"line {line_no}: xfail rows require exit_criteria")
            if kind in {"backlog", "backlog-unmapped"} and expected != "xfail":
                errors.append(f"line {line_no}: backlog/backlog-unmapped rows require expected=xfail")

        missing_rules = sorted(rules.difference(refs_seen))
        if missing_rules:
            errors.append(f"missing manifest rows for rules: {missing_rules}")

    overlap = sorted(allow.intersection(block))
    if overlap:
        errors.append(f"allowlist/blocklist overlap: {overlap}")

    if errors:
        for err in errors:
            print(f"lint_manifest: {err}", file=sys.stderr)
        return 1

    print("lint_manifest: ok")
    return 0


if __name__ == "__main__":
    sys.exit(main())
