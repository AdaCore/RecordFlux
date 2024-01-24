#!/usr/bin/env python3

"""
Check grammar embedded into a reStructuredText document.

The grammar is constructed from all Sphinx "productionlist" directives in that document.
A simple variant of Backus-Naur Form is used to describe the syntax.
See `doc/language_reference/language_reference.rst` for details.
Additional files passed on the command line are used to validate the grammar.
Invalid example can also be used to check that they are rejected by the extracted grammar.
"""

from __future__ import annotations

import argparse
import json
import logging
import re
import sys
from pathlib import Path
from typing import Final

import lark.grammar
import lark.visitors

import rflx.error

BNF_PARSER: Final = lark.Lark(
    r"""
       grammar:         rule ("\n" rule)* _END_OF_FILE
       _END_OF_FILE:    /\0/
       rule:            _name ":" _terms
       _name:            identifier
       identifier:      /[a-z]+(_[a-z]+)*/
       _terms:          verbal | _term+
       _term:           statement
                        | group
                        | repetition
                        | option
                        | literal
                        | literal_range
                        | alternation
       verbal:         "#" /[^\n]+/
       statement:       [ identifier "_" ] "`" identifier "`"
       repetition:      "{" _terms "}"
       option:          "[" _terms "]"
       group:           "(" _terms ")"
       literal_range:   literal ".." literal
       alternation:     _term "|" _term
       literal:        _quote_literal | _tick_literal
       _quote_literal:   "\"" /[^"]+/ "\""
       _tick_literal:    "'" /[^']+/ "'"

       %import common.WS
       %ignore /\n\s*:/
       %ignore WS

    """,
    start="grammar",
)


class TreeToLark(lark.Transformer[lark.lexer.Token, lark.Lark]):
    """
    Transform a Lark parse tree into a Lark parser object.

    This class implements the Lark transformer interface. As input, it takes a Lark parse tree of
    our custom BNF grammar (e.g. gathered from a reStructureText file). From this parse tree, it
    constructs a new Lark parser which accepts the language described in the BNF grammar.
    """

    def __init__(
        self,
        filename: Path,
        verbal_map: dict[str, str],
        errors: rflx.error.RecordFluxError,
    ):
        self._filename = filename
        self._verbal_map = verbal_map
        self._rules: set[str] = set()
        self._referenced: set[str] = set()
        self._errors = errors

    def grammar(self, data: list[str]) -> lark.Lark:
        grammar = "\n".join([*data, "%import common.WS", "%ignore WS", "%ignore /--.*/"])
        return lark.Lark(grammar, start=self._determin_start_rule)

    def rule(self, data: list[str]) -> str:
        self._rules.add(data[0])
        return f"{data[0]}: " + " ".join(data[1:])

    def identifier(self, data: list[lark.lexer.Token]) -> str:
        return str(data[0])

    def verbal(self, data: list[lark.lexer.Token]) -> str:
        verbal = data[0].value.strip()
        if verbal not in self._verbal_map:
            self._errors.extend(
                [
                    (
                        f"No mapping for verbal: '{verbal}'",
                        rflx.error.Subsystem.PARSER,
                        rflx.error.Severity.ERROR,
                        rflx.error.Location(
                            start=(data[0].line or 1, data[0].column or 1),
                            source=self._filename,
                        ),
                    ),
                ],
            )
            return ""
        return f"/{self._verbal_map[verbal]}/"

    def statement(self, data: list[str]) -> str:
        self._referenced.add(data[-1])
        # Just return identifier part and ignore prepended qualifier
        return str(data[-1])

    def repetition(self, data: str) -> str:
        return "( " + " ".join(data) + " )*"

    def option(self, data: str) -> str:
        return "[ " + " ".join(data) + " ]"

    def group(self, data: str) -> str:
        return "( " + " ".join(data) + " )"

    def literal_range(self, data: list[str]) -> str:
        return f"{data[0]} .. {data[1]}"

    def alternation(self, data: list[str]) -> str:
        return f"{data[0]} | {data[1]}"

    def literal(self, data: list[lark.lexer.Token]) -> str:
        result = re.sub(r'"', '\\"', str(data[0].value))
        return f'"{result}"'

    @property
    def _determin_start_rule(self) -> str:
        starts = self._rules - self._referenced
        num_starts = len(starts)
        assert num_starts > 0
        if num_starts > 1:
            starts_str = ", ".join(sorted(starts))
            self._errors.extend(
                [
                    (
                        f"Multiple start rules: {starts_str}",
                        rflx.error.Subsystem.PARSER,
                        rflx.error.Severity.ERROR,
                        rflx.error.Location(
                            start=(1, 1),
                            source=self._filename,
                        ),
                    ),
                ],
            )
        return starts.pop()


def check_spec(
    filename: Path,
    examples: list[Path],
    invalid: bool,
    verbal_map: dict[str, str],
    max_prod_line_len: int,
    errors: rflx.error.RecordFluxError,
) -> None:
    with filename.open() as infile:
        data = ""
        inside = False
        for lineno, line in enumerate(infile.readlines(), start=1):
            if inside:
                match = re.match(r"^\S", line)
                if match:
                    inside = False

            match = re.match(r"^\s*\.\. productionlist::$", line)
            if match:
                inside = True
                data += "\n"
                continue

            if inside:
                if not re.match(r"^(\s+[a-z:]|$)", line):
                    errors.extend(
                        [
                            (
                                "Invalid production line",
                                rflx.error.Subsystem.PARSER,
                                rflx.error.Severity.ERROR,
                                rflx.error.Location((lineno, 1), filename),
                            ),
                        ],
                    )
                if len(line) > max_prod_line_len:
                    errors.extend(
                        [
                            (
                                f"Line too long (expected <= {max_prod_line_len})",
                                rflx.error.Subsystem.PARSER,
                                rflx.error.Severity.ERROR,
                                rflx.error.Location((lineno, len(line) - 1), filename),
                            ),
                        ],
                    )
            data += f"{line}" if inside else "\n"

    if not data.strip():
        errors.extend(
            [
                (
                    "No grammar found",
                    rflx.error.Subsystem.PARSER,
                    rflx.error.Severity.ERROR,
                    rflx.error.Location((1, 1), filename),
                ),
            ],
        )

    if errors.errors:
        return

    try:
        tree = TreeToLark(
            filename=filename,
            verbal_map=verbal_map,
            errors=errors,
        ).transform(BNF_PARSER.parse(f"{data}\0"))
    except (lark.exceptions.UnexpectedCharacters, lark.exceptions.UnexpectedToken) as ue:
        lineno = max(ue.line, 1)
        column = max(ue.column, 1)
        errors.extend(
            [
                (
                    line,
                    rflx.error.Subsystem.PARSER,
                    rflx.error.Severity.ERROR if index == 0 else rflx.error.Severity.INFO,
                    rflx.error.Location(start=(lineno, column), source=filename),
                )
                for index, line in enumerate(str(ue).strip().split("\n"))
            ],
        )

    if errors.errors:
        return

    assert isinstance(tree, lark.Lark)

    for example in examples:
        with example.open() as ef:
            logging.log(logging.INFO, "Checking %s.", example)
            try:
                tree.parse(ef.read())
                if invalid:
                    errors.extend(
                        [
                            (
                                "Specification should be rejected",
                                rflx.error.Subsystem.PARSER,
                                rflx.error.Severity.ERROR,
                                rflx.error.Location(start=(1, 1), source=example),
                            ),
                        ],
                    )
            except (lark.exceptions.UnexpectedCharacters, lark.exceptions.UnexpectedEOF) as e:
                lineno = max(e.line, 1)
                column = max(e.column, 1)

                if not invalid:
                    errors.extend(
                        [
                            (
                                line,
                                rflx.error.Subsystem.PARSER,
                                (
                                    rflx.error.Severity.ERROR
                                    if index == 0
                                    else rflx.error.Severity.INFO
                                ),
                                rflx.error.Location(start=(lineno, column), source=example),
                            )
                            for index, line in enumerate(str(e).strip().split("\n"))
                        ],
                    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--invalid",
        action="store_true",
        help="Check that specifications are rejected by the grammar",
    )
    parser.add_argument(
        "--document",
        type=Path,
        help="reStructured text document to extract grammar from",
    )
    parser.add_argument(
        "--verbal-map",
        type=Path,
        help="JSON file containing verbal mapping",
    )
    parser.add_argument(
        "--max-prod-line-len",
        type=int,
        default=63,
        help="Maximum length of a line in a grammar production",
    )
    parser.add_argument(
        "specs",
        nargs="+",
        type=Path,
        help="RecordFlux specifications to check",
    )
    arguments = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format="%(message)s")
    errors = rflx.error.RecordFluxError()

    verbal_map = {}
    if arguments.verbal_map:
        with arguments.verbal_map.open() as mf:
            verbal_map = json.load(mf)

    check_spec(
        filename=arguments.document,
        examples=arguments.specs,
        invalid=arguments.invalid,
        verbal_map=verbal_map,
        max_prod_line_len=arguments.max_prod_line_len,
        errors=errors,
    )

    if errors.errors:
        sys.exit(str(errors))


if __name__ == "__main__":
    main()  # pragma: no cover
