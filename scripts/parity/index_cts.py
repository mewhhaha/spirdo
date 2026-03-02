#!/usr/bin/env python3
import argparse
import csv
import pathlib
import sys

HEADER = ["cts_id", "path", "kind", "expected", "domain", "spec_ref_hint"]


def read_id_set(path: pathlib.Path) -> set[str]:
    if not path.exists():
        return set()
    with path.open("r", encoding="utf-8") as f:
        rows = [line.rstrip("\n") for line in f]
    ids: set[str] = set()
    for idx, line in enumerate(rows, start=1):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        parts = line.split("\t")
        if idx == 1 and parts[0] == "cts_id":
            continue
        ids.add(parts[0].strip())
    return ids


def guess_kind(rel: str) -> str:
    lower = rel.lower()
    if lower.endswith(".spec.ts"):
        if "/validation/" in lower or "/api/validation/" in lower:
            return "validation"
        if "/execution/" in lower:
            return "execution"
        if "/compat/" in lower:
            return "compat"
        if "/shader_module/" in lower:
            return "shader-module"
        return "cts-spec"
    if "texture" in lower or "sampler" in lower:
        return "resources"
    if "const" in lower:
        return "const-eval"
    if "override" in lower:
        return "override"
    if "attr" in lower or "entry" in lower:
        return "entrypoint"
    if "type" in lower or "struct" in lower:
        return "types"
    if "parse" in lower or "syntax" in lower:
        return "parser"
    return "cts"


def guess_expected(rel: str) -> str:
    lower = rel.lower()
    if lower.endswith(".spec.ts"):
        return "pass"
    negative_markers = ["negative", "invalid", "error", "fail", "reject"]
    return "fail" if any(marker in lower for marker in negative_markers) else "pass"


def guess_domain(rel: str) -> str:
    return "wesl" if rel.lower().endswith(".wesl") else "wgsl"


def include_path(rel: str) -> bool:
    lower = rel.lower()
    if lower.endswith(".wgsl") or lower.endswith(".wesl"):
        return True
    if not lower.endswith(".spec.ts"):
        return False
    return (
        "/shader/" in lower
        or "shader_module" in lower
        or "wgsl" in lower
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Index WGSL/WESL-style source files from a pinned CTS checkout.")
    parser.add_argument("--cts-root", default="test/parity/vendor/cts")
    parser.add_argument("--allowlist", default="test/parity/cts_allowlist.tsv")
    parser.add_argument("--blocklist", default="test/parity/cts_blocklist.tsv")
    parser.add_argument("--output", default="test/parity/generated/cts_index.tsv")
    args = parser.parse_args()

    cts_root = pathlib.Path(args.cts_root)
    out_path = pathlib.Path(args.output)
    allow = read_id_set(pathlib.Path(args.allowlist))
    block = read_id_set(pathlib.Path(args.blocklist))

    rows: list[list[str]] = []
    if cts_root.exists():
        files = sorted(
            [
                p
                for p in cts_root.rglob("*")
                if p.is_file() and include_path(str(p.relative_to(cts_root)).replace("\\", "/"))
            ]
        )
        for file_path in files:
            rel = str(file_path.relative_to(cts_root)).replace("\\", "/")
            cts_id = rel.replace("/", "::")
            if allow and cts_id not in allow:
                continue
            if cts_id in block:
                continue
            rows.append(
                [
                    cts_id,
                    rel,
                    guess_kind(rel),
                    guess_expected(rel),
                    guess_domain(rel),
                    "",
                ]
            )

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f, delimiter="\t", lineterminator="\n")
        writer.writerow(HEADER)
        writer.writerows(rows)

    print(f"index_cts: wrote {len(rows)} rows to {out_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
