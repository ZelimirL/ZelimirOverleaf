#!/usr/bin/env python3
"""Basic structural checks for the generated dissertation PDF."""

import sys
from pathlib import Path

from pypdf import PdfReader


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: validate_accessibility.py dissertation.pdf")
        return 2

    path = Path(sys.argv[1])
    reader = PdfReader(path)
    root = reader.trailer["/Root"]
    metadata = reader.metadata or {}

    checks = {
        "structure tree": bool(root.get("/StructTreeRoot")),
        "marked-content declaration": bool(root.get("/MarkInfo")),
        "document language": bool(root.get("/Lang")),
        "document title": bool(metadata.get("/Title")),
        "document author": bool(metadata.get("/Author")),
    }

    for name, passed in checks.items():
        print(f"{'PASS' if passed else 'FAIL'}: {name}")

    return 0 if all(checks.values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
