from __future__ import annotations

from typing import Iterator, Optional

import pygments.lexer
import pygments.token

import rflx_lang as lang
from rflx.error import Location, RecordFluxError, Severity, Subsystem

# TODO(eng/recordflux/RecordFlux#1411): Obtain mapping from langkit parser
MAPPINGS = {
    pygments.token.Keyword: [
        "All",
        "Append",
        "Begin",
        "Byte_Order",
        "Case",
        "Channel",
        "Checksum",
        "Desc",
        "End",
        "Exception",
        "Extend",
        "First",
        "For",
        "Function",
        "Generic",
        "Goto",
        "Head",
        "Has_Data",
        "High_Order_First",
        "If",
        "Is",
        "Last",
        "Low_Order_First",
        "Message",
        "New",
        "Null",
        "Of",
        "Opaque",
        "Package",
        "Present",
        "Range",
        "Read",
        "Readable",
        "Renames",
        "Reset",
        "Return",
        "Sequence",
        "Session",
        "Size",
        "Some",
        "State",
        "Then",
        "Transition",
        "Type",
        "Use",
        "Valid",
        "Valid_Checksum",
        "When",
        "Where",
        "With",
        "Writable",
        "Write",
    ],
    pygments.token.Whitespace: ["Whitespace"],
    pygments.token.Number: ["Numeral"],
    pygments.token.Operator: [
        "Add",
        "Ampersand",
        "Arrow",
        "Assignment",
        "Div",
        "Double_Dot",
        "Exp",
        "Eq",
        "Ge",
        "Gt",
        "Le",
        "Leq",
        "Lt",
        "Minus",
        "Mul",
        "Neq",
        "Pipe",
        "Sub",
    ],
    pygments.token.Operator.Text: ["And", "In", "Mod", "Not", "Or"],
    pygments.token.Punctuation: [
        "Comma",
        "Colon",
        "Dot",
        "Double_Colon",
        "L_Brack",
        "L_Par",
        "R_Brack",
        "R_Par",
        "Semicolon",
        "Tick",
    ],
    pygments.token.String.Literal: ["String_Literal"],
    pygments.token.Text: ["Unqualified_Identifier"],
    pygments.token.Error: ["Lexing_Failure"],
    pygments.token.Comment: ["Comment"],
}

PYGMENTS_TOKENS = {v: k for k, l in MAPPINGS.items() for v in l}


def _calculate_offset(text: str, line: int, column: int) -> int:
    """
    Calculate byte offset from line/column.

    Given a text buffer, a (one-based) line number and a (one-based) column number,
    calculate the (zero-based) byte offset into that buffer.
    """

    split: list[tuple[int, int]] = []
    start = 0
    for cur in text.split("\n"):
        split.append((start, len(cur)))
        start += len(cur) + 1

    error = RecordFluxError()

    if column < 1:
        error.extend(
            [
                (
                    "column must be positive",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    Location((line, column)),
                ),
            ],
        )

    if line < 1:
        error.extend(
            [
                (
                    "line must be positive",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    Location((line, column)),
                ),
            ],
        )

    if line > len(split):
        error.extend(
            [
                (
                    "line out of range",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    Location((line, column)),
                ),
            ],
        )

    error.propagate()

    offset, linelen = split[line - 1]

    if column > linelen:
        error.extend(
            [
                (
                    "column out of range",
                    Subsystem.PARSER,
                    Severity.ERROR,
                    Location((line, column)),
                ),
            ],
        )

    error.propagate()

    return offset + column - 1


class RFLXLexer(pygments.lexer.Lexer):
    def get_tokens_unprocessed(
        self,
        text: str,
    ) -> Iterator[tuple[int, pygments.token._TokenType, str]]:  # noqa: SLF001
        unit = lang.AnalysisContext().get_from_buffer(
            "<stdin>",
            text,
            rule=lang.GrammarRule.main_rule_rule,
        )

        tokens = [
            (
                (
                    _calculate_offset(text, t.sloc_range.start.line, t.sloc_range.start.column)
                    if t.kind != "Termination"
                    else -1
                ),
                t,
            )
            for t in unit.iter_tokens()
        ]

        prev: Optional[tuple[int, lang.SlocRange]] = None
        result = []

        for start, t in tokens:
            if prev is not None:
                # Our langkit parser does not emit tokens for whitespace. As pygments
                # expects that, reconstruct whitespace from line/column information.
                prev_start, prev_end_line, prev_end_col = (
                    prev[0],
                    prev[1].end.line,
                    prev[1].end.column,
                )
                if prev_end_line < t.sloc_range.start.line:
                    num_newlines = t.sloc_range.start.line - prev_end_line
                    result.append(
                        (
                            prev_start,
                            pygments.token.Text,
                            num_newlines * "\n",
                        ),
                    )
                    if t.sloc_range.start.column > 1:
                        result.append(
                            (
                                prev_start + num_newlines,
                                pygments.token.Text.Whitespace,
                                (t.sloc_range.start.column - 1) * " ",
                            ),
                        )
                elif prev_end_col < t.sloc_range.start.column:
                    result.append(
                        (prev_start, pygments.token.Whitespace, (start - prev_start) * " "),
                    )

            prev = (start + len(t.text), t.sloc_range)

            # Termination is a special langkit token. We need it for whitespace
            # reconstruction above, but discard it here.
            if t.kind != "Termination":
                assert t.kind in PYGMENTS_TOKENS, f"Missing token {t.kind}"
                result.append((start, PYGMENTS_TOKENS[t.kind], t.text))

        return iter(result)
