from __future__ import annotations

import re
from enum import Enum
from pathlib import Path
from typing import Optional

from rflx.error import Location, RecordFluxError, Severity, Subsystem

INCORRECT_LINE_TERMINATORS = "\r"
ILLEGAL_WHITESPACE_CHARACTERS = "\t\x0b\x0c"
KEYWORD_INDENTATION = [
    ("begin", [3, 6]),
    ("end message", [6]),
    ("end", [0, 3, 6]),
    ("for", [3, 12, 15]),
    ("generic", [3]),
    ("goto", [9]),
    ("is", [3, 6]),
    ("message", [6]),
    ("package", [0]),
    ("session", [3]),
    ("state", [6]),
    ("then", [12]),
    ("transition", [6]),
    ("type", [3, 6]),
    ("with", [0, 3, 6, 9, 12, 15]),
]


class Check(Enum):
    ALL = "all"
    BLANK_LINES = "blank-lines"
    CHARACTERS = "characters"
    INDENTATION = "indentation"
    LINE_LENGTH = "line-length"
    TOKEN_SPACING = "token-spacing"
    TRAILING_SPACES = "trailing-spaces"


def check(spec_file: Path) -> RecordFluxError:
    with spec_file.open(encoding="utf-8", newline="") as f:
        return check_string(f.read(), spec_file)


def check_string(specification: str, spec_file: Path = Path("<stdin>")) -> RecordFluxError:
    error = RecordFluxError()

    if specification in ("", "\n"):
        return error

    lines = specification.split("\n")
    enabled_checks = _determine_enabled_checks(error, lines[0], spec_file)

    if not enabled_checks:
        return error

    blank_lines = 0

    for i, l in enumerate(lines, start=1):
        if Check.BLANK_LINES in enabled_checks:
            blank_lines = _check_blank_lines(error, l, i, spec_file, blank_lines, len(lines))
        if Check.CHARACTERS in enabled_checks:
            _check_characters(error, l, i, spec_file)
        if Check.INDENTATION in enabled_checks:
            _check_indentation(error, l, i, spec_file)
        if Check.LINE_LENGTH in enabled_checks:
            _check_line_length(error, l, i, spec_file)
        if Check.TOKEN_SPACING in enabled_checks:
            _check_token_spacing(error, l, i, spec_file)
        if Check.TRAILING_SPACES in enabled_checks:
            _check_trailing_spaces(error, l, i, spec_file)

    return error


def _append(
    error: RecordFluxError,
    message: str,
    row: int,
    col: int,
    spec_file: Path,
    check_type: Optional[Check] = None,
) -> None:
    error.extend(
        [
            (
                message + (f" [{check_type.value}]" if check_type else ""),
                Subsystem.STYLE,
                Severity.ERROR,
                Location((row, col), spec_file),
            ),
        ],
    )


def _determine_enabled_checks(error: RecordFluxError, line: str, spec_file: Path) -> set[Check]:
    checks = {c.value for c in Check.__members__.values()}
    disabled_checks = set()

    m = re.match(r"^\s*--\s*style\s*:\s*disable\s*=\s*([^.]*)$", line)
    if m:
        disabled_checks = {c.strip() for c in m.group(1).split(",")}
        for c in disabled_checks - checks:
            _append(error, f'invalid check "{c}"', 1, 1, spec_file)
    else:
        return set(Check.__members__.values())

    if Check.ALL.value in disabled_checks:
        return set()

    return {Check(c) for c in checks - disabled_checks}


def _check_blank_lines(
    error: RecordFluxError,
    line: str,
    row: int,
    spec_file: Path,
    blank_lines: int,
    row_count: int,
) -> int:
    if not line:
        if row == 1:
            _append(error, "leading blank line", row, 1, spec_file, Check.BLANK_LINES)
        if blank_lines > 0 and row == row_count:
            _append(error, "trailing blank line", row - 1, 1, spec_file, Check.BLANK_LINES)
        blank_lines += 1
    else:
        if blank_lines > 1:
            _append(error, "multiple blank lines", row - 1, 1, spec_file, Check.BLANK_LINES)
        blank_lines = 0

    return blank_lines


def _check_characters(error: RecordFluxError, line: str, row: int, spec_file: Path) -> None:
    for j, c in enumerate(line, start=1):
        if c == INCORRECT_LINE_TERMINATORS:
            s = repr(c).replace("'", '"')
            _append(error, f"incorrect line terminator {s}", row, j, spec_file, Check.CHARACTERS)
        if c in ILLEGAL_WHITESPACE_CHARACTERS:
            s = repr(c).replace("'", '"')
            _append(error, f"illegal whitespace character {s}", row, j, spec_file, Check.CHARACTERS)


def _check_indentation(error: RecordFluxError, line: str, row: int, spec_file: Path) -> None:
    match = re.match(r" *", line)
    assert match
    indentation = match.end()

    for keyword, indentations in KEYWORD_INDENTATION:
        if re.match(rf" *{keyword}", line) and indentation not in indentations:
            expected_indentation = (
                ", ".join(str(row) for row in indentations[:-1]) + f" or {indentations[-1]}"
                if len(indentations) > 1
                else str(indentations[0])
            )
            _append(
                error,
                f"unexpected keyword indentation (expected {expected_indentation})",
                row,
                match.end(),
                spec_file,
                Check.INDENTATION,
            )


def _check_token_spacing(  # noqa: PLR0912
    error: RecordFluxError,
    line: str,
    row: int,
    spec_file: Path,
) -> None:
    strings = [range(m.start(), m.end()) for m in re.finditer(r'"[^"]*"', line.split("--")[0])]

    for match in re.finditer(
        r"--+|/?=(?!>)|<=?|>=?|=>|:=|\+|[(]+\-\d+[)]+|\-|/|\*\*?|::?|'|;|,",
        line,
    ):
        if "--" in line and match.start() > line.find("--"):
            continue

        if any(match.start() in r for r in strings) or any(match.end() in r for r in strings):
            # https://github.com/nedbat/coveragepy/issues/772
            # A dummy statement is needed to disable the peephole optimizer, so that the continue
            # statement is detected during coverage analysis.
            # CPython 3.8 and 3.9 are affected. The issue is fixed in CPython 3.10.
            dummy = 0  # noqa: F841
            continue

        token = match.group(0)

        if token in [";", ","]:
            space_before = False
            space_after = True
        elif token in ["::", "'"]:
            space_before = False
            space_after = False
        else:
            space_before = True
            space_after = True

        if space_before:
            assert token != ";"
            if match.start() > 1 and line[match.start() - 1] not in " (":
                _append(
                    error,
                    f'missing space before "{token}"',
                    row,
                    match.start() + 1,
                    spec_file,
                    Check.TOKEN_SPACING,
                )
        else:
            if match.start() > 1 and line[match.start() - 1] == " ":
                _append(
                    error,
                    f'space before "{token}"',
                    row,
                    match.start() + 1,
                    spec_file,
                    Check.TOKEN_SPACING,
                )
        if space_after:
            if match.end() < len(line) and line[match.end()] not in " ;\n":
                _append(
                    error,
                    f'missing space after "{token}"',
                    row,
                    match.end() + 1,
                    spec_file,
                    Check.TOKEN_SPACING,
                )
        else:
            if match.end() < len(line) and line[match.end()] == " ":
                _append(
                    error,
                    f'space after "{token}"',
                    row,
                    match.end() + 2,
                    spec_file,
                    Check.TOKEN_SPACING,
                )


def _check_trailing_spaces(error: RecordFluxError, line: str, row: int, spec_file: Path) -> None:
    if line.endswith(" "):
        _append(error, "trailing whitespace", row, len(line), spec_file, Check.TRAILING_SPACES)


def _check_line_length(error: RecordFluxError, line: str, row: int, spec_file: Path) -> None:
    if len(line) > 120:
        _append(error, f"line too long ({len(line)}/120)", row, 121, spec_file, Check.LINE_LENGTH)
