#!/usr/bin/env python3
import argparse
import pathlib
import re
import sys
from dataclasses import dataclass
from typing import Optional

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

TEMPLATE_LITERAL_RE = re.compile(r"`((?:\\.|[^`])*)`", re.S)
PLACEHOLDER_MARKERS = re.compile(
    r"\b(todo|tbd|placeholder|stub|fixme)\b",
    re.IGNORECASE,
)
WGSL_ATTR_RE = re.compile(r"@(compute|vertex|fragment|workgroup_size|builtin|location|id)\b")
FN_RE = re.compile(r"\bfn\s+[A-Za-z_][A-Za-z0-9_]*\s*\(")
TYPE_RE = re.compile(r"\b(vec\d+<|mat\d+x\d+<|array<|texture_|sampler_|ptr<|i32|u32|f16|f32|bool)\b")
DECL_RE = re.compile(r"\b(struct|var<|var\s+|let\s+|const\s+)\b")
COMMENT_RE = re.compile(r"//.*?$|/\*.*?\*/", re.S | re.M)

MAX_TEMPLATE_EXPRS = 3
MIN_SNIPPET_BYTES = 40
MAX_SNIPPET_BYTES = 120_000


@dataclass(frozen=True)
class ManifestRow:
    index: int
    id: str
    kind: str
    origin: str
    source: str
    origin_ref: str
    parts: list[str]


@dataclass(frozen=True)
class Candidate:
    snippet: str
    path: pathlib.Path
    template_expr_count: int


def read_manifest(path: pathlib.Path):
    lines = path.read_text(encoding="utf-8").splitlines()
    if not lines:
        raise RuntimeError(f"manifest is empty: {path}")
    if lines[0].split("\t") != MANIFEST_HEADER:
        raise RuntimeError(f"manifest header mismatch: {path}")
    rows: list[ManifestRow] = []
    for line_no, raw_line in enumerate(lines[1:], start=2):
        stripped = raw_line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        parts = raw_line.split("\t")
        if len(parts) != len(MANIFEST_HEADER):
            raise RuntimeError(f"manifest malformed line {line_no}: {path}")
        row = ManifestRow(
            index=line_no,
            id=parts[0],
            kind=parts[3].strip().lower(),
            origin=parts[6].strip().lower(),
            source=parts[5],
            origin_ref=parts[7],
            parts=parts,
        )
        rows.append(row)
    return lines, rows


def resolve_cts_path(vendor_root: pathlib.Path, origin_ref: str) -> pathlib.Path:
    rel = origin_ref.strip().replace("::", "/")
    if not rel:
        raise RuntimeError(f"invalid origin_ref: {origin_ref}")
    candidate = vendor_root / rel
    if candidate.suffix:
        return candidate
    for suffix in (".spec.ts", ".ts", ".wgsl", ".wesl"):
        alt = vendor_root / f"{rel}{suffix}"
        if alt.exists():
            return alt
    raise FileNotFoundError(f"missing cts file for origin_ref={origin_ref}: {candidate}")


def strip_template_expressions(source: str) -> str:
    out: list[str] = []
    idx = 0
    length = len(source)
    while idx < length:
        if source[idx:idx + 2] != "${":
            out.append(source[idx])
            idx += 1
            continue
        idx += 2
        depth = 0
        while idx < length:
            ch = source[idx]
            if ch == "{":
                depth += 1
            elif ch == "}":
                if depth == 0:
                    idx += 1
                    break
                depth -= 1
            idx += 1
    return "".join(out)


def score_snippet(snippet: str) -> int:
    s = snippet
    low = s.lower()
    score = 0
    score += 6 * low.count("@compute")
    score += 6 * low.count("@vertex")
    score += 6 * low.count("@fragment")
    score += 2 * low.count("@workgroup_size")
    score += 2 * low.count("@group")
    score += low.count("fn")
    score += low.count("var<")
    if "${" in s:
        score -= 2 * s.count("${")
    if "fn main" in low:
        score += 4
    return score


def strip_template_expressions(source: str) -> tuple[str, int]:
    out: list[str] = []
    idx = 0
    length = len(source)
    placeholder_count = 0
    while idx < length:
        if source[idx : idx + 2] != "${":
            out.append(source[idx])
            idx += 1
            continue
        idx += 2
        placeholder_count += 1
        depth = 0
        while idx < length:
            ch = source[idx]
            if ch == "{":
                depth += 1
            elif ch == "}":
                if depth == 0:
                    idx += 1
                    break
                depth -= 1
            idx += 1
        out.append("0")
    return "".join(out), placeholder_count


def remove_wgsl_comments(text: str) -> str:
    return COMMENT_RE.sub("", text)


def balanced_delimiters(text: str) -> bool:
    stack: list[str] = []
    pairs = {"}": "{", ")": "(", "]": "["}
    opens = {"{", "(", "["}
    closes = set(pairs)
    for ch in text:
        if ch in opens:
            stack.append(ch)
            continue
        if ch not in closes:
            continue
        if not stack or stack[-1] != pairs[ch]:
            return False
        stack.pop()
    return not stack


def is_probable_wgsl(snippet: str, template_expr_count: int) -> bool:
    if template_expr_count > MAX_TEMPLATE_EXPRS:
        return False

    normalized = remove_wgsl_comments(snippet)
    compact = normalized.strip()
    if not compact or len(compact) < MIN_SNIPPET_BYTES or len(compact) > MAX_SNIPPET_BYTES:
        return False
    if PLACEHOLDER_MARKERS.search(compact):
        return False
    if not balanced_delimiters(compact):
        return False

    low = compact.lower()
    if "..." in low:
        return False
    if not FN_RE.search(low) and not WGSL_ATTR_RE.search(low):
        return False
    if not (
        WGSL_ATTR_RE.search(low)
        or (FN_RE.search(low) and (TYPE_RE.search(low) or DECL_RE.search(low)))
    ):
        return False

    return True


def score_snippet(snippet: str, template_expr_count: int) -> int:
    s = snippet.lower()
    low = remove_wgsl_comments(s)
    score = 0
    score += 6 * low.count("@compute")
    score += 6 * low.count("@vertex")
    score += 6 * low.count("@fragment")
    score += 2 * low.count("@workgroup_size")
    score += 2 * low.count("@group")
    score += low.count("fn")
    score += low.count("var<")
    score += 2 * len(FN_RE.findall(low))
    score += len(DECL_RE.findall(low))
    score -= 3 * template_expr_count
    if "fn main" in low:
        score += 4
    if low.startswith("var") or "var " in low:
        score += 1
    if "export" in low:
        score += 1
    if "import" in low:
        score -= 2
    return score


def extract_template_snippets(text: str) -> list[tuple[str, int]]:
    candidates: list[tuple[str, int]] = []
    for match in TEMPLATE_LITERAL_RE.finditer(text):
        body = match.group(1)
        body = body.strip()
        if not body:
            continue
        candidate, template_exprs = strip_template_expressions(body)
        candidate = candidate.strip()
        if not candidate:
            continue
        if is_probable_wgsl(candidate, template_exprs):
            candidates.append((candidate, template_exprs))
    if not candidates:
        return []
    return candidates


def pick_wgsl_template(text: str) -> Optional[str]:
    candidates = extract_template_snippets(text)
    if not candidates:
        return None
    best: Optional[tuple[int, str]] = None
    for candidate, template_exprs in candidates:
        value = score_snippet(candidate, template_exprs)
        if value <= 0:
            continue
        if best is None or value > best[0]:
            best = (value, candidate)
    if best is None:
        return None
    return best[1]


IMPORT_RE = re.compile(
    r"""^\s*import(?:\s+type)?(?:\s+[^'"]+?\s+from\s+)?\s*['"]([^'"]+)['"]\s*;?""",
    re.M,
)


def resolve_local_imports(path: pathlib.Path, text: str) -> list[pathlib.Path]:
    out: list[pathlib.Path] = []
    for m in IMPORT_RE.finditer(text):
        ref = m.group(1).strip()
        if not ref.startswith("."):
            continue
        base = (path.parent / ref).resolve()
        candidates: list[pathlib.Path]
        if base.suffix:
            candidates = [base]
            if base.suffix == ".js":
                candidates.append(base.with_suffix(".ts"))
            elif base.suffix == ".ts":
                candidates.append(base.with_suffix(".js"))
        else:
            candidates = [
                pathlib.Path(f"{base}.ts"),
                pathlib.Path(f"{base}.js"),
                base / "index.ts",
                base / "index.js",
            ]
        for cand in candidates:
            if cand.exists():
                out.append(cand)
                break
    out.sort()
    return out


def extract_snippet_recursive(path: pathlib.Path, visited: set[pathlib.Path], depth: int) -> Optional[str]:
    if path in visited:
        return None
    visited.add(path)
    content = path.read_text(encoding="utf-8", errors="ignore")
    suffix = path.suffix.lower()
    candidates: list[Candidate] = []
    if suffix in {".wgsl", ".wesl"}:
        snippet = content.strip()
        if is_probable_wgsl(snippet, 0):
            return snippet
        return None
    if suffix not in {".ts", ".js"}:
        return None

    for snippet, template_expr_count in extract_template_snippets(content):
        candidates.append(Candidate(snippet, path, template_expr_count))
    if depth <= 0:
        pass
    else:
        for dep in resolve_local_imports(path, content):
            nested = extract_snippet_recursive(dep, visited, depth - 1)
            if nested:
                candidates.append(Candidate(nested, dep, 0))

    if not candidates:
        return None
    best: Optional[tuple[int, str]] = None
    for candidate in candidates:
        if candidate.path.suffix.lower() in {".ts", ".js"} and not is_probable_wgsl(candidate.snippet, candidate.template_expr_count):
            continue
        value = score_snippet(candidate.snippet, candidate.template_expr_count)
        if value <= 0:
            continue
        if best is None or value > best[0]:
            best = (value, candidate.snippet)
    return best[1] if best else None


def extract_snippet_from_file(path: pathlib.Path) -> Optional[str]:
    return extract_snippet_recursive(path.resolve(), set(), depth=3)


def slug_from_origin_ref(origin_ref: str) -> str:
    normalized = origin_ref.lower().replace("::", ".").replace("/", ".")
    normalized = re.sub(r"[^a-z0-9.]+", "-", normalized)
    normalized = re.sub(r"-+", "-", normalized).strip("-")
    if not normalized:
        return "unknown"
    return normalized


def materialize(
    manifest_path: pathlib.Path,
    cts_root: pathlib.Path,
    fixture_root: pathlib.Path,
) -> tuple[int, int, int]:
    manifest_lines, rows = read_manifest(manifest_path)
    manifest_dir = manifest_path.parent
    fixtures_dir = fixture_root
    fixtures_dir.mkdir(parents=True, exist_ok=True)

    used_names: set[str] = set()
    updates: dict[int, str] = {}
    total_backlog = 0
    extracted = 0

    for row in rows:
        if row.kind not in {"backlog", "backlog-unmapped"} or row.origin != "cts":
            continue
        total_backlog += 1

        source_path = pathlib.Path(row.source)
        if source_path.exists() and str(source_path).startswith("fixtures/cts_backlog/"):
            fixture_path = source_path
            if not fixture_path.is_absolute():
                fixture_path = (manifest_dir / source_path).resolve()
            if fixture_path.exists():
                extracted += 1
                continue

        try:
            cts_path = resolve_cts_path(cts_root, row.origin_ref)
        except FileNotFoundError:
            continue

        snippet = extract_snippet_from_file(cts_path)
        if snippet is None:
            continue

        slug = slug_from_origin_ref(row.origin_ref)
        if slug in used_names:
            n = 2
            while f"{slug}-{n}" in used_names:
                n += 1
            slug = f"{slug}-{n}"
        used_names.add(slug)

        source_ext = "wesl" if cts_path.suffix.lower() == ".wesl" else "wgsl"
        fixture_name = f"{slug}.{source_ext}"
        fixture_path = fixtures_dir / fixture_name

        fixture_text = snippet + "\n"
        existing = fixture_path.read_text(encoding="utf-8") if fixture_path.exists() else None
        if existing != fixture_text:
            fixture_path.write_text(fixture_text, encoding="utf-8")

        rel_fixture = str(fixture_path.relative_to(manifest_dir)).replace("\\", "/")
        updates[row.index] = rel_fixture
        extracted += 1

    if not updates:
        return total_backlog, extracted, total_backlog - extracted

    output_lines = list(manifest_lines)
    for line_no, source in updates.items():
        idx = line_no - 1
        parts = output_lines[idx].split("\t")
        parts[5] = source
        output_lines[idx] = "\t".join(parts)

    manifest_path.write_text("\n".join(output_lines) + "\n", encoding="utf-8")
    return total_backlog, extracted, total_backlog - extracted


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Materialize WGSL/WESL fixtures for CTS backlog manifest rows."
    )
    parser.add_argument("--manifest", default="test/parity/manifest.tsv")
    parser.add_argument("--cts-root", default="test/parity/vendor/cts")
    parser.add_argument(
        "--fixtures-dir",
        default="test/parity/fixtures/cts_backlog",
        help="directory to write extracted WGSL/WESL fixture files",
    )
    args = parser.parse_args()

    manifest_path = pathlib.Path(args.manifest)
    cts_root = pathlib.Path(args.cts_root)
    fixture_root = pathlib.Path(args.fixtures_dir)

    for required in (manifest_path, cts_root):
        if not required.exists():
            raise RuntimeError(f"missing required path: {required}")

    total_backlog, extracted, failed = materialize(manifest_path, cts_root, fixture_root)
    print(
        f"materialize_cts_backlog_fixtures: total_backlog={total_backlog} "
        f"extracted={extracted} failed={failed}"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
