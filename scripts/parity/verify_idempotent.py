#!/usr/bin/env python3
import argparse
import hashlib
import pathlib
import subprocess
import sys


def file_digest(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def snapshot(paths: list[pathlib.Path]) -> dict[str, str]:
    entries: dict[str, str] = {}
    for path in paths:
        if path.is_symlink():
            entries[str(path)] = "symlink:" + str(path.readlink())
            continue
        if path.is_file():
            entries[str(path)] = file_digest(path)
            continue
        if path.is_dir():
            entries[str(path) + "/"] = "directory"
            for candidate in sorted(path.rglob("*")):
                if candidate.is_symlink():
                    entries[str(candidate)] = "symlink:" + str(candidate.readlink())
                elif candidate.is_file():
                    entries[str(candidate)] = file_digest(candidate)
            continue
        entries[str(path)] = "missing"
    return entries


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run a command and fail if it changes any watched path."
    )
    parser.add_argument("--path", action="append", required=True, dest="paths")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()

    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not command:
        parser.error("a command is required after --")

    watched_paths = [pathlib.Path(raw_path) for raw_path in args.paths]
    before = snapshot(watched_paths)
    result = subprocess.run(command, check=False)
    if result.returncode != 0:
        return result.returncode

    after = snapshot(watched_paths)
    if before == after:
        return 0

    changed = sorted(key for key in before.keys() | after.keys() if before.get(key) != after.get(key))
    print("verify_idempotent: command changed watched paths:", file=sys.stderr)
    for path in changed:
        print(f"  {path}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
