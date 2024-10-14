from __future__ import annotations

import re
from pathlib import Path

from rflx.const import BASIC_STYLE_CHECKS, MODEL_STYLE_CHECKS, StyleCheck
from rflx.rapidflux import ErrorEntry, Location, RecordFluxError, Severity

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
    ("machine", [3]),
    ("state", [6]),
    ("then", [12]),
    ("transition", [6]),
    ("type", [3, 6]),
    ("with", [0, 3, 6, 9, 12, 15]),
]


def check(spec_file: Path, enabled_checks: frozenset[StyleCheck]) -> RecordFluxError:
    """
    Perform basic style checks on the given specification file.

    The function reads the specification file and performs style checks that can
    be made directly on its string representation (basic checks). The exact
    subset of checks to be performed must be passed via the `enabled_checks`
    parameter.
    """
    error = RecordFluxError()
    # Read the file without any newline normalizations so that their conformance
    # to the style rules can be checked.
    with spec_file.open(newline="") as f:
        source_code_str = f.read()
        check_string(error, source_code_str, enabled_checks, spec_file)
    return error


def check_string(
    error: RecordFluxError,
    specification: str,
    enabled_checks: frozenset[StyleCheck],
    spec_file: Path = Path("<stdin>"),
) -> None:
    """
    Perform basic style checks on the given specification string.

    The function performs style checks that can be made directly on the string
    representation of the specification (basic checks). The exact subset of
    checks to be performed must be passed via the `enabled_checks` parameter.
    """
    if specification in ("", "\n"):
        return

    if not enabled_checks:
        return

    lines = specification.split("\n")

    blank_lines = 0
    for i, l in enumerate(lines, start=1):
        if StyleCheck.BLANK_LINES in enabled_checks:
            blank_lines = _check_blank_lines(error, l, i, spec_file, blank_lines, len(lines))
        if StyleCheck.CHARACTERS in enabled_checks:
            _check_characters(error, l, i, spec_file)
        if StyleCheck.INDENTATION in enabled_checks:
            _check_indentation(error, l, i, spec_file)
        if StyleCheck.LINE_LENGTH in enabled_checks:
            _check_line_length(error, l, i, spec_file)
        if StyleCheck.TOKEN_SPACING in enabled_checks:
            _check_token_spacing(error, l, i, spec_file)
        if StyleCheck.TRAILING_SPACES in enabled_checks:
            _check_trailing_spaces(error, l, i, spec_file)


def determine_enabled_checks(
    error: RecordFluxError,
    specification: str,
    spec_file: Path,
) -> tuple[frozenset[StyleCheck], frozenset[StyleCheck]]:
    """
    Determine the enabled checks in a specification header.

    If the given specification contains a valid style check configuration, then
    determine the enabled style checks. Otherwise, it is assumed that all checks
    are enabled.

    The enabled checks are returned as a tuple where the first set of checks
    (basic checks) can be performed directly on the input string and the second
    set (model checks) must be performed at a model level.
    """

    header = specification.split("\n", 1)[0]

    all_checks = {c.value for c in StyleCheck.__members__.values()}
    disabled_checks = set()

    m = re.match(r"^\s*--\s*style\s*:\s*disable\s*=\s*([^.]*)$", header)
    if m:
        disabled_checks = {c.strip() for c in m.group(1).split(",")}
        for c in disabled_checks - all_checks:
            _append(error, f'invalid check "{c}"', 1, 1, spec_file)
        if StyleCheck.ALL.value in disabled_checks:
            return frozenset(), frozenset()
    else:
        disabled_checks = set()

    enabled_checks = {StyleCheck(c) for c in all_checks - disabled_checks}

    return frozenset(enabled_checks - MODEL_STYLE_CHECKS), frozenset(
        enabled_checks - BASIC_STYLE_CHECKS,
    )


def _append(
    error: RecordFluxError,
    message: str,
    row: int,
    col: int,
    spec_file: Path,
    check_type: StyleCheck | None = None,
) -> None:
    error.push(
        ErrorEntry(
            message + (f" [style:{check_type.value}]" if check_type else ""),
            Severity.ERROR,
            Location((row, col), spec_file),
        ),
    )


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
            _append(error, "leading blank line", row, 1, spec_file, StyleCheck.BLANK_LINES)
        if blank_lines > 0 and row == row_count:
            _append(error, "trailing blank line", row - 1, 1, spec_file, StyleCheck.BLANK_LINES)
        blank_lines += 1
    else:
        if blank_lines > 1:
            _append(error, "multiple blank lines", row - 1, 1, spec_file, StyleCheck.BLANK_LINES)
        blank_lines = 0

    return blank_lines


def _check_characters(error: RecordFluxError, line: str, row: int, spec_file: Path) -> None:
    for j, c in enumerate(line, start=1):
        if c == INCORRECT_LINE_TERMINATORS:
            s = repr(c).replace("'", '"')
            _append(
                error,
                f"incorrect line terminator {s}",
                row,
                j,
                spec_file,
                StyleCheck.CHARACTERS,
            )
        if c in ILLEGAL_WHITESPACE_CHARACTERS:
            s = repr(c).replace("'", '"')
            _append(
                error,
                f"illegal whitespace character {s}",
                row,
                j,
                spec_file,
                StyleCheck.CHARACTERS,
            )


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
                match.end() + 1,
                spec_file,
                StyleCheck.INDENTATION,
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
                    StyleCheck.TOKEN_SPACING,
                )
        else:
            if match.start() > 1 and line[match.start() - 1] == " ":
                _append(
                    error,
                    f'space before "{token}"',
                    row,
                    match.start() + 1,
                    spec_file,
                    StyleCheck.TOKEN_SPACING,
                )
        if space_after:
            if match.end() < len(line) and line[match.end()] not in " ;\n":
                _append(
                    error,
                    f'missing space after "{token}"',
                    row,
                    match.end() + 1,
                    spec_file,
                    StyleCheck.TOKEN_SPACING,
                )
        else:
            if match.end() < len(line) and line[match.end()] == " ":
                _append(
                    error,
                    f'space after "{token}"',
                    row,
                    match.end() + 2,
                    spec_file,
                    StyleCheck.TOKEN_SPACING,
                )


def _check_trailing_spaces(error: RecordFluxError, line: str, row: int, spec_file: Path) -> None:
    if line.endswith(" "):
        _append(error, "trailing whitespace", row, len(line), spec_file, StyleCheck.TRAILING_SPACES)


def _check_line_length(error: RecordFluxError, line: str, row: int, spec_file: Path) -> None:
    if len(line) > 120:
        _append(
            error,
            f"line too long ({len(line)}/120)",
            row,
            121,
            spec_file,
            StyleCheck.LINE_LENGTH,
        )
