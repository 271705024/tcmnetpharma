#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate a deterministic SHA-256 manifest for release artifacts / datasets.

Output format is `sha256sum -c` compatible:  <64-hex>  <two spaces>  <path>

Why this exists
---------------
The ownership-evidence chain for this project rests on three things: a frozen
commit (signed tag), a published DOI, and byte-level integrity of what was
distributed. The third one needs a hash list that can be re-verified later by
anyone, on any machine, without trusting us.

Usage
-----
    python tools/make-manifest.py inst/extdata -o ../../artifacts/ownership/MANIFEST-data.txt
    python tools/make-manifest.py tcmnetpharma_0.2.0.tar.gz -o MANIFEST-software.txt
    python tools/make-manifest.py D:/tcmnetpharma-build-p1/dist -g '*.zip' -o dist.txt

Notes
-----
- Paths are written relative to --base (default: parent of the first path that
  is a directory, else the file's own directory). Override with --base.
- Only ASCII/LF is emitted; no BOM (see project rule: LF only, no CRLF).
- Unreadable files are reported and make the run exit 1 — a partially written
  manifest is worse than none.
"""

from __future__ import annotations

import argparse
import hashlib
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

CHUNK = 4 * 1024 * 1024


def sha256_of(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for block in iter(lambda: fh.read(CHUNK), b""):
            h.update(block)
    return h.hexdigest()


def expand(paths: list[str], pattern: str) -> list[Path]:
    found: list[Path] = []
    for raw in paths:
        p = Path(raw).expanduser()
        if p.is_dir():
            found.extend(sorted(q for q in p.rglob(pattern) if q.is_file()))
        elif p.is_file():
            found.append(p)
        else:
            print(f"!! not found: {p}", file=sys.stderr)
            raise SystemExit(1)
    # de-duplicate while keeping order
    seen: set[str] = set()
    uniq: list[Path] = []
    for q in found:
        key = str(q.resolve()).lower()
        if key not in seen:
            seen.add(key)
            uniq.append(q)
    return uniq


def guess_base(paths: list[Path]) -> Path:
    if not paths:
        return Path.cwd()
    first = paths[0]
    if first.is_absolute() or True:
        # common prefix of all parent dirs
        common = os.path.commonpath([str(p.resolve().parent) for p in paths])
        return Path(common)
    return first.parent


def main() -> int:
    ap = argparse.ArgumentParser(description="Generate a sha256sum-compatible manifest.")
    ap.add_argument("paths", nargs="+", help="files or directories to hash")
    ap.add_argument("-o", "--out", required=True, help="manifest output path")
    ap.add_argument("-g", "--glob", default="*", help="glob used when a path is a directory (default: *)")
    ap.add_argument("-b", "--base", default=None, help="strip this prefix when writing paths")
    ap.add_argument("-l", "--label", default="", help="free-text label written in the header")
    args = ap.parse_args()

    targets = expand(args.paths, args.glob)
    if not targets:
        print("!! nothing matched", file=sys.stderr)
        return 1

    base = Path(args.base).expanduser() if args.base else guess_base(targets)

    rows: list[tuple[str, str, int]] = []
    failed = 0
    for p in targets:
        try:
            digest = sha256_of(p)
        except OSError as exc:
            print(f"!! cannot read {p}: {exc}", file=sys.stderr)
            failed += 1
            continue
        try:
            rel = p.resolve().relative_to(base.resolve())
        except ValueError:
            rel = Path(p.name)
        rows.append((digest, rel.as_posix(), p.stat().st_size))

    rows.sort(key=lambda r: r[1])
    total_bytes = sum(r[2] for r in rows)
    stamp = datetime.now(timezone.utc).astimezone().strftime("%Y-%m-%d %H:%M:%S %z")

    out = Path(args.out).expanduser()
    out.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        f"# SHA-256 manifest — {args.label or out.stem}",
        f"# generated: {stamp}",
        f"# base: {base.as_posix()}",
        f"# files: {len(rows)}  total: {total_bytes} bytes ({total_bytes / 1048576:.1f} MiB)",
        "# verify: sha256sum -c <this file>   (run from the base directory above)",
    ]
    # NOTE: no blank line before the rows — `sha256sum -c` reports a blank line
    # as "improperly formatted". Lines starting with '#' are skipped by coreutils.
    lines += [f"{d}  {p}" for d, p, _ in rows]
    out.write_text("\n".join(lines) + "\n", encoding="utf-8", newline="\n")

    print(f"manifest -> {out}")
    print(f"  files: {len(rows)}  total: {total_bytes} bytes ({total_bytes / 1048576:.1f} MiB)")
    if failed:
        print(f"  !! {failed} file(s) could not be read", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
