#!/usr/bin/env python3

"""
Check recursively that each directory containing a Python module is a correct Python package.

A Python package requires an  __init__.py. Some tools (e.g., mypy) ignore all directories
which are not a correct Python package.
"""

import argparse
import sys
from pathlib import Path
from typing import Sequence


def main(argv: Sequence[str]) -> bool:
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("directories", metavar="DIRECTORY", type=Path, nargs="+")
    args = arg_parser.parse_args(argv[1:])

    return missing_init(args.directories)


def missing_init(dirs: Sequence[Path]) -> bool:
    if not dirs:
        return False

    missing = False

    for d in dirs:
        files = set(f.name for f in d.glob("*.py"))
        if files and "__init__.py" not in files:
            print(f"missing __init__.py in {d}")
            missing = True

    return (
        missing_init([s for d in dirs if d.is_dir() for s in d.iterdir() if s.is_dir()]) or missing
    )


if __name__ == "__main__":
    sys.exit(main(sys.argv))
