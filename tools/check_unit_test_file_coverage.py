#!/usr/bin/env python3

"""
Check one-to-one correspondence between unit tests and modules.

   - For every file x.py in the source directory a file x_test.py must exist in the test directory
   - No other Python files must exist in the test directory
   - Non-Python files shall be ignored
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path


class CheckUnitTestFileError(Exception):
    pass


def _ignored(file: Path, ignore_list: list[Path] | None = None) -> bool:
    return ignore_list is not None and any(
        i.parts == file.parts[: len(i.parts)] for i in ignore_list
    )


def check_file_coverage(
    source_dir: Path,
    test_dir: Path,
    ignore: list[Path] | None = None,
) -> None:

    if not source_dir.exists():
        raise CheckUnitTestFileError(f"missing source directory: {source_dir}")

    if not test_dir.exists():
        raise CheckUnitTestFileError(f"missing test directory: {test_dir}")

    ignore = ignore or []

    source_files = sorted(
        [
            s.relative_to(source_dir)
            for ext in ("**/*.py", "**/*.pyi")
            for s in source_dir.glob(ext)
        ],
    )
    test_files = sorted([t.relative_to(test_dir) for t in test_dir.glob("**/*.py")])

    excess_source_files = [
        str(source_file)
        for source_file in source_files
        if (
            not _ignored(source_file, ignore)
            and source_file.name not in ("__init__.py", "__init__.pyi")
            and source_file.with_name(source_file.stem + "_test.py") not in test_files
        )
    ]

    excess_test_files = [
        str(test_file)
        for test_file in test_files
        if (
            not _ignored(test_file, ignore)
            and test_file.name not in ("__init__.py", "__init__.pyi")
            and (
                not test_file.stem.endswith("_test")
                or (
                    test_file.with_name(test_file.stem[:-5] + ".py") not in source_files
                    and test_file.with_name(test_file.stem[:-5] + ".pyi") not in source_files
                )
            )
        )
    ]

    errors: list[str] = []
    if excess_source_files:
        errors.append(f"no tests for: {', '.join(excess_source_files)}")

    if excess_test_files:
        errors.append(f"no corresponding source for: {', '.join(excess_test_files)}")

    if errors:
        raise CheckUnitTestFileError("\n".join(errors))


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source-dir", type=Path, required=True, help="Source directory")
    parser.add_argument("--test-dir", type=Path, required=True, help="Unit test directory")
    parser.add_argument("--ignore", type=Path, action="append", help="Directory to ignore")
    args = parser.parse_args()

    try:
        check_file_coverage(source_dir=args.source_dir, test_dir=args.test_dir, ignore=args.ignore)
    except CheckUnitTestFileError as e:
        sys.exit(f"{e}")


if __name__ == "__main__":  # pragma: no cover
    main()
