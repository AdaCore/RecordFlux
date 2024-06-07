#!/usr/bin/env python3
"""
Check Rust `serial` and `parallel` attributes order.

This tool statically checks that the `serial` or `parallel` attributes, if present, are
always used last in Rust source code.
"""

from __future__ import annotations

import argparse
import itertools
import re
import sys
from pathlib import Path

from rflx.rapidflux import ErrorEntry, Location, RecordFluxError, Severity, source_code


def cli() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description='Check correct usage of "serial" and "parallel"')
    parser.add_argument("files", metavar="F", type=Path, nargs="+", help="files to process")
    return parser


def get_line_col(content: str, match_offset: int) -> tuple[int, int]:
    line = 1
    col = 1

    for i, char in enumerate(content):
        if i == match_offset:
            break

        if char == "\n":
            line += 1
            col = 1
        else:
            col += 1

    return line, col


def match_to_location(filename: Path, content: str, match: re.Match[str]) -> Location:
    return Location(
        get_line_col(content, match.start()),
        filename,
        get_line_col(content, match.end()),
    )


def check_file(filename: Path, error: RecordFluxError) -> None:
    content = filename.read_text()
    source_code.register(filename, content)

    error.extend(
        [
            ErrorEntry(
                (
                    'invalid order of attributes; "parallel" and "serial" must always be the last '
                    "attribute"
                ),
                Severity.ERROR,
                match_to_location(filename, content, match),
            )
            for match in itertools.chain(
                re.finditer(r"#\[serial\][\n\r\s]*#\[", content),
                re.finditer(r"#\[parallel\][\n\r\s]*#\[", content),
            )
        ],
    )


def main() -> int:
    files = cli().parse_args().files
    errors = RecordFluxError()

    for path in files:
        check_file(path, errors)

    errors.print_messages()
    return 1 if errors.has_errors() else 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
