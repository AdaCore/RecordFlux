from __future__ import annotations

import re
import textwrap
from collections.abc import Mapping, Sequence
from itertools import zip_longest
from pathlib import Path
from typing import Any

import pytest

from rflx import common, expr, lang, model
from rflx.const import RESERVED_WORDS
from rflx.error import fail
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    OPAQUE,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    State,
    declaration as decl,
    statement as stmt,
)
from rflx.model.message import ByteOrder
from rflx.rapidflux import Location, RecordFluxError, Severity
from rflx.specification import parser
from rflx.typing_ import UNDEFINED
from tests.const import SPEC_DIR
from tests.data import models
from tests.utils import (
    check_regex,
    parse,
    parse_bool_expression,
    parse_expression,
    parse_math_expression,
)

T = Integer("Test::T", expr.Number(0), expr.Number(255), expr.Number(8))

# Generated from the Ada 202x LRM source code (http://ada-auth.org/arm-files/ARM_SRC.zip) retrieved
# on 2022-09-16 using the following command:
#
#  $ sed -ne "s/.*@key{\([^} ]*\)}.*/\"\L\1\",/p" *.MSS *.mss | sort -u
#
ADA_KEYWORDS = [
    "abort",
    "abs",
    "abstract",
    "accept",
    "access",
    "aliased",
    "all",
    "and",
    "array",
    "at",
    "begin",
    "body",
    "case",
    "constant",
    "declare",
    "delay",
    "delta",
    "digits",
    "do",
    "else",
    "elsif",
    "end",
    "entry",
    "exception",
    "exit",
    "for",
    "function",
    "generic",
    "goto",
    "if",
    "in",
    "interface",
    "is",
    "limited",
    "loop",
    "mod",
    "new",
    "not",
    "null",
    "of",
    "or",
    "others",
    "out",
    "overriding",
    "package",
    "parallel",
    "pragma",
    "private",
    "procedure",
    "protected",
    "raise",
    "range",
    "record",
    "rem",
    "renames",
    "requeue",
    "return",
    "reverse",
    "select",
    "separate",
    "some",
    "subtype",
    "synchronized",
    "tagged",
    "task",
    "terminate",
    "then",
    "type",
    "until",
    "use",
    "when",
    "while",
    "with",
    "xor",
]

ILLEGAL_IDENTIFIERS = ["RFLX_Foo"]


def to_dict(node: Any) -> Any:  # type: ignore[misc]
    if node is None:
        return None
    if node.is_list_type:
        return [to_dict(e) for e in node.children]
    result = {name[2:]: to_dict(getattr(node, name)) for name in dir(node) if name.startswith("f_")}
    if result:
        result["_kind"] = node.kind_name
        return result
    return {"_kind": node.kind_name, "_value": node.text}


def assert_ast_files(  # type: ignore[misc]
    filenames: Sequence[str],
    expected: Mapping[str, Any],
) -> None:
    p = parser.Parser()
    p.parse(*[Path(f) for f in filenames])
    result = {f: to_dict(s) for f, s in p.specifications.items()}
    p.create_model()
    assert result == expected, filenames


def assert_ast_string(string: str, expected: Mapping[str, Any]) -> None:  # type: ignore[misc]
    p = parser.Parser()
    p.parse_string(string)
    p.create_model()
    assert to_dict(next(iter(p.specifications.items()))[1]) == expected


def assert_error_files(filenames: Sequence[str], regex: str) -> None:
    check_regex(regex)
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
        p.parse(*[Path(f) for f in filenames])


def assert_error_string(string: str, regex: str) -> None:
    check_regex(regex)
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):  # noqa: PT012
        p.parse_string(string)
        p.create_model()


def assert_messages_files(filenames: Sequence[str], messages: Sequence[model.Message]) -> None:
    p = parser.Parser()
    for filename in filenames:
        p.parse(Path(filename))
    m = p.create_model()
    assert_messages(m.messages, messages)


def assert_messages_string(string: str, messages: Sequence[model.Message]) -> None:
    p = parser.Parser()
    p.parse_string(string)
    m = p.create_model()
    assert_messages(m.messages, messages)


def assert_messages(
    actual_messages: Sequence[model.Message],
    expected_messages: Sequence[model.Message],
) -> None:
    for actual, expected in zip_longest(actual_messages, expected_messages):
        assert actual.full_name == expected.full_name
        assert actual.structure == expected.structure, expected.full_name
        assert actual.types == expected.types, expected.full_name
        assert actual.fields == expected.fields, expected.full_name
    assert actual_messages == expected_messages


def assert_refinements_string(string: str, refinements: Sequence[model.Refinement]) -> None:
    p = parser.Parser()
    p.parse_string(string)
    m = p.create_model()
    assert m.refinements == refinements


def raise_parser_error() -> None:
    fail("TEST", Severity.ERROR)


def parse_statement(data: str) -> stmt.Statement:
    parser_statement, filename = parse(data, lang.GrammarRule.action_rule)
    assert isinstance(parser_statement, lang.Statement)
    error = RecordFluxError()
    statement = parser.create_statement(error, parser_statement, filename)
    error.propagate()
    assert isinstance(statement, stmt.Statement)
    return statement


def parse_declaration(data: str) -> decl.Declaration:
    parser_declaration, filename = parse(data, lang.GrammarRule.declaration_rule)
    assert isinstance(parser_declaration, lang.LocalDecl)
    error = RecordFluxError()
    declaration = parser.create_declaration(error, parser_declaration, ID("Package"), filename)
    error.propagate()
    assert isinstance(declaration, decl.Declaration)
    return declaration


def parse_formal_declaration(data: str) -> decl.Declaration:
    error = RecordFluxError()
    parser_declaration, filename = parse(data, lang.GrammarRule.state_machine_parameter_rule)
    assert isinstance(parser_declaration, lang.FormalDecl)
    declaration = parser.create_formal_declaration(
        error,
        parser_declaration,
        ID("Package"),
        filename,
    )
    error.propagate()
    assert isinstance(declaration, decl.Declaration)
    return declaration


def parse_state(data: str) -> State:
    parser_state, source = parse(data, lang.GrammarRule.state_rule)
    assert isinstance(parser_state, lang.State)
    error = RecordFluxError()
    state = parser.create_state(error, parser_state, ID("Package"), source)
    error.propagate()
    assert isinstance(state, State)
    return state


def check_diagnostics_error(unit: lang.AnalysisUnit, error: RecordFluxError) -> None:
    if parser.diagnostics_to_error(unit.diagnostics, error, common.STDIN):
        error.propagate()


def parse_state_machine_error(string: str) -> None:
    unit = lang.AnalysisContext().get_from_buffer(
        "<stdin>",
        string,
        rule=lang.GrammarRule.state_machine_declaration_rule,
    )
    error = RecordFluxError()
    check_diagnostics_error(unit, error)
    assert isinstance(unit.root, lang.StateMachineDecl)
    result = parser.create_state_machine(error, unit.root, ID("Package"), Path("<stdin>"))
    error.propagate()
    assert isinstance(result, model.UncheckedStateMachine)


def parse_state_machine(string: str) -> model.UncheckedStateMachine:
    unit = lang.AnalysisContext().get_from_buffer(
        "<stdin>",
        string,
        rule=lang.GrammarRule.state_machine_declaration_rule,
    )
    error = RecordFluxError()
    check_diagnostics_error(unit, error)
    assert isinstance(unit.root, lang.StateMachineDecl)
    result = parser.create_state_machine(error, unit.root, ID("Package"), Path("<stdin>"))
    error.propagate()
    return result


def parse_id(data: str, rule: str) -> ID:
    unit = lang.AnalysisContext().get_from_buffer("<stdin>", data, rule=rule)
    assert isinstance(unit.root, lang.AbstractID)
    error = RecordFluxError()
    result = parser.create_id(error, unit.root, Path("<stdin>"))
    error.propagate()
    return result


# This test must fail to get full coverage of assert_error_files.
@pytest.mark.xfail()
def test_assert_error_files() -> None:
    assert_error_files([], "^ error: $")


def test_create_model() -> None:
    p = parser.Parser()
    p.parse(SPEC_DIR / "tlv.rflx")
    p.create_model()


def test_parse_string_error() -> None:
    p = parser.Parser()
    with pytest.raises(
        RecordFluxError,
        match=(
            "^{}$".format(
                re.escape(
                    'a/b.rflx:1:9: error: source file name does not match the package name "A"\n'
                    'a/b.rflx:1:9: help: either rename the file to "a.rflx" or change the package '
                    'name to "B"\n'
                    'a/b.rflx:1:9: help: rename to "B"\n'
                    'a/b.rflx:3:5: help: rename to "B"\n'
                    "a/b.rflx:2:1: error: unexpected keyword indentation (expected 3 or 6) "
                    "[indentation]",
                ),
            )
        ),
    ):
        p.parse_string(
            "package A is\ntype T is range 0 .. 0 with Size => 1;\nend A;",
            filename=Path("a/b.rflx"),
        )


@pytest.mark.parametrize(
    ("value", "error"),
    [
        ("", "<stdin>:1:1: error: Expected 'package', got Termination"),
        ("\n", "<stdin>:2:1: error: Expected 'package', got Termination"),
    ],
)
def test_parse_string_empty_file_error(value: str, error: str) -> None:
    p = parser.Parser()
    with pytest.raises(
        RecordFluxError,
        match=(rf"^{error}$"),
    ):
        p.parse_string(value)


def test_parse_duplicate_specifications() -> None:
    files = [f"{SPEC_DIR}/empty_package.rflx", f"{SPEC_DIR}/empty_package.rflx"]
    assert_ast_files(
        files,
        {
            "Empty_Package": {
                "_kind": "Specification",
                "context_clause": [],
                "package_declaration": {
                    "_kind": "PackageNode",
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
                    "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
                },
            },
        },
    )


def test_parse_empty_package_spec() -> None:
    assert_ast_files(
        [f"{SPEC_DIR}/empty_package.rflx"],
        {
            "Empty_Package": {
                "_kind": "Specification",
                "context_clause": [],
                "package_declaration": {
                    "_kind": "PackageNode",
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
                    "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
                },
            },
        },
    )


def test_parse_context_spec() -> None:
    assert_ast_files(
        [f"{SPEC_DIR}/context.rflx"],
        {
            "Context": {
                "_kind": "Specification",
                "context_clause": [
                    {
                        "_kind": "ContextItem",
                        "item": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
                    },
                ],
                "package_declaration": {
                    "_kind": "PackageNode",
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "Context"},
                    "identifier": {"_kind": "UnqualifiedID", "_value": "Context"},
                },
            },
            "Empty_Package": {
                "_kind": "Specification",
                "context_clause": [],
                "package_declaration": {
                    "_kind": "PackageNode",
                    "declarations": [],
                    "end_identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
                    "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_Package"},
                },
            },
        },
    )


def test_parse_integer_type_spec() -> None:
    spec = {
        "Integer_Type": {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "1"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "16"},
                            },
                            "last": {"_kind": "NumericLiteral", "_value": "2_000"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Page_Num"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "0"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "8"},
                            },
                            "last": {"_kind": "NumericLiteral", "_value": "255"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Line_Size"},
                        "parameters": None,
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Integer_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Integer_Type"},
            },
        },
    }

    assert_ast_files([f"{SPEC_DIR}/integer_type.rflx"], spec)


def test_parse_enumeration_type_spec() -> None:
    spec = {
        "Enumeration_Type": {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "EnumerationTypeDef",
                            "aspects": [
                                {
                                    "_kind": "Aspect",
                                    "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                    "value": {"_kind": "NumericLiteral", "_value": "3"},
                                },
                            ],
                            "elements": {
                                "_kind": "NamedEnumerationDef",
                                "elements": [
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Mon",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "1"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Tue",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "2"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Wed",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "3"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Thu",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "4"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Fri",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "5"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Sat",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "6"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Sun",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "7"},
                                    },
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Day"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "EnumerationTypeDef",
                            "aspects": [
                                {
                                    "_kind": "Aspect",
                                    "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                    "value": {"_kind": "NumericLiteral", "_value": "3"},
                                },
                                {
                                    "_kind": "Aspect",
                                    "identifier": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "Always_Valid",
                                    },
                                    "value": {
                                        "_kind": "Variable",
                                        "identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "True"},
                                            "package": None,
                                        },
                                    },
                                },
                            ],
                            "elements": {
                                "_kind": "NamedEnumerationDef",
                                "elements": [
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "P1"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "1"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "P2"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "2"},
                                    },
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Protocol"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "EnumerationTypeDef",
                            "aspects": [
                                {
                                    "_kind": "Aspect",
                                    "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                    "value": {"_kind": "NumericLiteral", "_value": "1"},
                                },
                                {
                                    "_kind": "Aspect",
                                    "identifier": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "Always_Valid",
                                    },
                                    "value": {
                                        "_kind": "Variable",
                                        "identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "False",
                                            },
                                            "package": None,
                                        },
                                    },
                                },
                            ],
                            "elements": {
                                "_kind": "PositionalEnumerationDef",
                                "elements": [
                                    {"_kind": "UnqualifiedID", "_value": "M"},
                                    {"_kind": "UnqualifiedID", "_value": "F"},
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Gender"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "EnumerationTypeDef",
                            "aspects": [
                                {
                                    "_kind": "Aspect",
                                    "identifier": {
                                        "_kind": "UnqualifiedID",
                                        "_value": "Always_Valid",
                                    },
                                    "value": None,
                                },
                                {
                                    "_kind": "Aspect",
                                    "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                    "value": {"_kind": "NumericLiteral", "_value": "8"},
                                },
                            ],
                            "elements": {
                                "_kind": "NamedEnumerationDef",
                                "elements": [
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Low",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "1"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Medium",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "4"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "High",
                                        },
                                        "literal": {"_kind": "NumericLiteral", "_value": "7"},
                                    },
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Priority"},
                        "parameters": None,
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Enumeration_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Enumeration_Type"},
            },
        },
    }
    assert_ast_files([f"{SPEC_DIR}/enumeration_type.rflx"], spec)


def test_parse_sequence_type_spec() -> None:
    spec = {
        "Sequence_Type": {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "0"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "8"},
                            },
                            "last": {"_kind": "NumericLiteral", "_value": "255"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Byte"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "SequenceTypeDef",
                            "element_type": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Byte"},
                                "package": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bytes"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "aspects": [],
                            "message_fields": {
                                "_kind": "MessageFields",
                                "fields": [
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Length",
                                        },
                                        "thens": [
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [
                                                    {
                                                        "_kind": "Aspect",
                                                        "identifier": {
                                                            "_kind": "UnqualifiedID",
                                                            "_value": "Size",
                                                        },
                                                        "value": {
                                                            "_kind": "BinOp",
                                                            "left": {
                                                                "_kind": "Variable",
                                                                "identifier": {
                                                                    "_kind": "ID",
                                                                    "name": {
                                                                        "_kind": "UnqualifiedID",
                                                                        "_value": "Length",
                                                                    },
                                                                    "package": None,
                                                                },
                                                            },
                                                            "op": {
                                                                "_kind": "OpMul",
                                                                "_value": "*",
                                                            },
                                                            "right": {
                                                                "_kind": "NumericLiteral",
                                                                "_value": "8",
                                                            },
                                                        },
                                                    },
                                                ],
                                                "condition": None,
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Bytes",
                                                },
                                            },
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Byte"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Bytes",
                                        },
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "Bytes",
                                            },
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                ],
                                "initial_field": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Foo"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "SequenceTypeDef",
                            "element_type": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Foo"},
                                "package": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                        "parameters": None,
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Sequence_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Sequence_Type"},
            },
        },
    }
    assert_ast_files([f"{SPEC_DIR}/sequence_type.rflx"], spec)


def test_parse_message_type_spec() -> None:
    spec = {
        "Message_Type": {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "0"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "8"},
                            },
                            "last": {"_kind": "NumericLiteral", "_value": "255"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "aspects": [],
                            "message_fields": {
                                "_kind": "MessageFields",
                                "fields": [
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Foo"},
                                        "thens": [
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Foo",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpLe", "_value": "<="},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "16#1E#",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Bar",
                                                },
                                            },
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Foo",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpGt", "_value": ">"},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "16#1E#",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Baz",
                                                },
                                            },
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [
                                            {
                                                "identifier": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Size",
                                                },
                                                "value": {
                                                    "left": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "8",
                                                    },
                                                    "op": {"_kind": "OpMul", "_value": "*"},
                                                    "right": {
                                                        "identifier": {
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Foo",
                                                            },
                                                            "package": None,
                                                            "_kind": "ID",
                                                        },
                                                        "_kind": "Variable",
                                                    },
                                                    "_kind": "BinOp",
                                                },
                                                "_kind": "Aspect",
                                            },
                                        ],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Opaque"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                ],
                                "initial_field": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "PDU"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "aspects": [],
                            "message_fields": {
                                "_kind": "MessageFields",
                                "fields": [
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [
                                            {
                                                "identifier": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Size",
                                                },
                                                "value": {
                                                    "left": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "8",
                                                    },
                                                    "op": {"_kind": "OpMul", "_value": "*"},
                                                    "right": {
                                                        "identifier": {
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Bar",
                                                            },
                                                            "package": None,
                                                            "_kind": "ID",
                                                        },
                                                        "_kind": "Variable",
                                                    },
                                                    "_kind": "BinOp",
                                                },
                                                "_kind": "Aspect",
                                            },
                                        ],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Opaque"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                ],
                                "initial_field": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Simple_PDU"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {"_kind": "NullMessageTypeDef", "_value": "null message"},
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_PDU"},
                        "parameters": None,
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
            },
        },
    }
    assert_ast_files([f"{SPEC_DIR}/message_type.rflx"], spec)


def test_parse_type_refinement_spec() -> None:
    spec = {
        "Message_Type": {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "0"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "8"},
                            },
                            "last": {"_kind": "NumericLiteral", "_value": "255"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "aspects": [],
                            "message_fields": {
                                "_kind": "MessageFields",
                                "fields": [
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Foo"},
                                        "thens": [
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Foo",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpLe", "_value": "<="},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "16#1E#",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Bar",
                                                },
                                            },
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Foo",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpGt", "_value": ">"},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "16#1E#",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Baz",
                                                },
                                            },
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [
                                            {
                                                "identifier": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Size",
                                                },
                                                "value": {
                                                    "left": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "8",
                                                    },
                                                    "op": {"_kind": "OpMul", "_value": "*"},
                                                    "right": {
                                                        "identifier": {
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Foo",
                                                            },
                                                            "package": None,
                                                            "_kind": "ID",
                                                        },
                                                        "_kind": "Variable",
                                                    },
                                                    "_kind": "BinOp",
                                                },
                                                "_kind": "Aspect",
                                            },
                                        ],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Opaque"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                ],
                                "initial_field": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "PDU"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "aspects": [],
                            "message_fields": {
                                "_kind": "MessageFields",
                                "fields": [
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [
                                            {
                                                "identifier": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Size",
                                                },
                                                "value": {
                                                    "left": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "8",
                                                    },
                                                    "op": {"_kind": "OpMul", "_value": "*"},
                                                    "right": {
                                                        "identifier": {
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Bar",
                                                            },
                                                            "package": None,
                                                            "_kind": "ID",
                                                        },
                                                        "_kind": "Variable",
                                                    },
                                                    "_kind": "BinOp",
                                                },
                                                "_kind": "Aspect",
                                            },
                                        ],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Opaque"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                ],
                                "initial_field": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Simple_PDU"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {"_kind": "NullMessageTypeDef", "_value": "null message"},
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_PDU"},
                        "parameters": None,
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
            },
        },
        "Type_Refinement": {
            "_kind": "Specification",
            "context_clause": [
                {
                    "_kind": "ContextItem",
                    "item": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                },
            ],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "RefinementDecl",
                        "condition": {
                            "_kind": "BinOp",
                            "left": {
                                "_kind": "Variable",
                                "identifier": {
                                    "_kind": "ID",
                                    "name": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                    "package": None,
                                },
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {"_kind": "NumericLiteral", "_value": "42"},
                        },
                        "field": {"_kind": "UnqualifiedID", "_value": "Baz"},
                        "pdu": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Simple_PDU"},
                            "package": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                        },
                        "sdu": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "PDU"},
                            "package": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                        },
                    },
                    {
                        "_kind": "RefinementDecl",
                        "condition": None,
                        "field": {"_kind": "UnqualifiedID", "_value": "Baz"},
                        "pdu": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "PDU"},
                            "package": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                        },
                        "sdu": {
                            "_kind": "ID",
                            "name": {"_kind": "UnqualifiedID", "_value": "Simple_PDU"},
                            "package": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                        },
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Type_Refinement"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Type_Refinement"},
            },
        },
    }
    assert_ast_files([f"{SPEC_DIR}/message_type.rflx", f"{SPEC_DIR}/type_refinement.rflx"], spec)


def test_parse_type_derivation_spec() -> None:
    assert_ast_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type Foo is
              message
                 N : T;
              end message;
           type Bar is new Foo;
        end Test;
        """,
        {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "0"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "8"},
                            },
                            "last": {"_kind": "NumericLiteral", "_value": "255"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "aspects": [],
                            "message_fields": {
                                "_kind": "MessageFields",
                                "fields": [
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "N"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                ],
                                "initial_field": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Foo"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "TypeDerivationDef",
                            "base": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Foo"},
                                "package": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                        "parameters": None,
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Test"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Test"},
            },
        },
    )


def test_parse_ethernet_spec() -> None:
    # black does not manage to honor the line limit here

    spec = {
        "Ethernet": {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "0"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "48"},
                            },
                            "last": {
                                "_kind": "BinOp",
                                "left": {
                                    "_kind": "BinOp",
                                    "left": {"_kind": "NumericLiteral", "_value": "2"},
                                    "op": {"_kind": "OpPow", "_value": "**"},
                                    "right": {"_kind": "NumericLiteral", "_value": "48"},
                                },
                                "op": {"_kind": "OpSub", "_value": "-"},
                                "right": {"_kind": "NumericLiteral", "_value": "1"},
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Address"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "46"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "16"},
                            },
                            "last": {
                                "_kind": "BinOp",
                                "left": {
                                    "_kind": "BinOp",
                                    "left": {"_kind": "NumericLiteral", "_value": "2"},
                                    "op": {"_kind": "OpPow", "_value": "**"},
                                    "right": {"_kind": "NumericLiteral", "_value": "16"},
                                },
                                "op": {"_kind": "OpSub", "_value": "-"},
                                "right": {"_kind": "NumericLiteral", "_value": "1"},
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Type_Length"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "16#8100#"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "16"},
                            },
                            "last": {"_kind": "NumericLiteral", "_value": "16#8100#"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "TPID"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "RangeTypeDef",
                            "first": {"_kind": "NumericLiteral", "_value": "0"},
                            "size": {
                                "_kind": "Aspect",
                                "identifier": {"_kind": "UnqualifiedID", "_value": "Size"},
                                "value": {"_kind": "NumericLiteral", "_value": "16"},
                            },
                            "last": {
                                "_kind": "BinOp",
                                "left": {
                                    "_kind": "BinOp",
                                    "left": {"_kind": "NumericLiteral", "_value": "2"},
                                    "op": {"_kind": "OpPow", "_value": "**"},
                                    "right": {"_kind": "NumericLiteral", "_value": "16"},
                                },
                                "op": {"_kind": "OpSub", "_value": "-"},
                                "right": {"_kind": "NumericLiteral", "_value": "1"},
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "TCI"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "aspects": [],
                            "message_fields": {
                                "_kind": "MessageFields",
                                "fields": [
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Destination",
                                        },
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "Address",
                                            },
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Source",
                                        },
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "Address",
                                            },
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Type_Length_TPID",
                                        },
                                        "thens": [
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [
                                                    {
                                                        "_kind": "Aspect",
                                                        "identifier": {
                                                            "_kind": "UnqualifiedID",
                                                            "_value": "First",
                                                        },
                                                        "value": {
                                                            "_kind": "Attribute",
                                                            "expression": {
                                                                "_kind": "Variable",
                                                                "identifier": {
                                                                    "_kind": "ID",
                                                                    "name": {
                                                                        "_kind": "UnqualifiedID",
                                                                        "_value": "Type_Length_TPID",  # noqa: E501
                                                                    },
                                                                    "package": None,
                                                                },
                                                            },
                                                            "kind": {
                                                                "_kind": "AttrFirst",
                                                                "_value": "First",
                                                            },
                                                        },
                                                    },
                                                ],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Type_Length_TPID",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpEq", "_value": "="},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "16#8100#",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "TPID",
                                                },
                                            },
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [
                                                    {
                                                        "_kind": "Aspect",
                                                        "identifier": {
                                                            "_kind": "UnqualifiedID",
                                                            "_value": "First",
                                                        },
                                                        "value": {
                                                            "_kind": "Attribute",
                                                            "expression": {
                                                                "_kind": "Variable",
                                                                "identifier": {
                                                                    "_kind": "ID",
                                                                    "name": {
                                                                        "_kind": "UnqualifiedID",
                                                                        "_value": "Type_Length_TPID",  # noqa: E501
                                                                    },
                                                                    "package": None,
                                                                },
                                                            },
                                                            "kind": {
                                                                "_kind": "AttrFirst",
                                                                "_value": "First",
                                                            },
                                                        },
                                                    },
                                                ],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Type_Length_TPID",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpNeq", "_value": "/="},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "16#8100#",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Type_Length",
                                                },
                                            },
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "Type_Length",
                                            },
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "TPID",
                                        },
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "TPID"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "TCI",
                                        },
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "TCI"},
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Type_Length",
                                        },
                                        "thens": [
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [
                                                    {
                                                        "_kind": "Aspect",
                                                        "identifier": {
                                                            "_kind": "UnqualifiedID",
                                                            "_value": "Size",
                                                        },
                                                        "value": {
                                                            "_kind": "BinOp",
                                                            "left": {
                                                                "_kind": "Variable",
                                                                "identifier": {
                                                                    "_kind": "ID",
                                                                    "name": {
                                                                        "_kind": "UnqualifiedID",
                                                                        "_value": "Type_Length",
                                                                    },
                                                                    "package": None,
                                                                },
                                                            },
                                                            "op": {
                                                                "_kind": "OpMul",
                                                                "_value": "*",
                                                            },
                                                            "right": {
                                                                "_kind": "NumericLiteral",
                                                                "_value": "8",
                                                            },
                                                        },
                                                    },
                                                ],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Type_Length",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpLe", "_value": "<="},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "1500",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Payload",
                                                },
                                            },
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "Variable",
                                                        "identifier": {
                                                            "_kind": "ID",
                                                            "name": {
                                                                "_kind": "UnqualifiedID",
                                                                "_value": "Type_Length",
                                                            },
                                                            "package": None,
                                                        },
                                                    },
                                                    "op": {"_kind": "OpGe", "_value": ">="},
                                                    "right": {
                                                        "_kind": "NumericLiteral",
                                                        "_value": "1536",
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Payload",
                                                },
                                            },
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "Type_Length",
                                            },
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                    {
                                        "_kind": "MessageField",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Payload",
                                        },
                                        "thens": [
                                            {
                                                "_kind": "ThenNode",
                                                "aspects": [],
                                                "condition": {
                                                    "_kind": "BinOp",
                                                    "left": {
                                                        "_kind": "BinOp",
                                                        "left": {
                                                            "_kind": "BinOp",
                                                            "left": {
                                                                "_kind": "Attribute",
                                                                "expression": {
                                                                    "_kind": "Variable",
                                                                    "identifier": {
                                                                        "_kind": "ID",
                                                                        "name": {
                                                                            "_kind": "UnqualifiedID",  # noqa: E501
                                                                            "_value": "Payload",
                                                                        },
                                                                        "package": None,
                                                                    },
                                                                },
                                                                "kind": {
                                                                    "_kind": "AttrSize",
                                                                    "_value": "Size",
                                                                },
                                                            },
                                                            "op": {
                                                                "_kind": "OpDiv",
                                                                "_value": "/",
                                                            },
                                                            "right": {
                                                                "_kind": "NumericLiteral",
                                                                "_value": "8",
                                                            },
                                                        },
                                                        "op": {"_kind": "OpGe", "_value": ">="},
                                                        "right": {
                                                            "_kind": "NumericLiteral",
                                                            "_value": "46",
                                                        },
                                                    },
                                                    "op": {"_kind": "OpAnd", "_value": "and"},
                                                    "right": {
                                                        "_kind": "BinOp",
                                                        "left": {
                                                            "_kind": "BinOp",
                                                            "left": {
                                                                "_kind": "Attribute",
                                                                "expression": {
                                                                    "_kind": "Variable",
                                                                    "identifier": {
                                                                        "_kind": "ID",
                                                                        "name": {
                                                                            "_kind": "UnqualifiedID",  # noqa: E501
                                                                            "_value": "Payload",
                                                                        },
                                                                        "package": None,
                                                                    },
                                                                },
                                                                "kind": {
                                                                    "_kind": "AttrSize",
                                                                    "_value": "Size",
                                                                },
                                                            },
                                                            "op": {
                                                                "_kind": "OpDiv",
                                                                "_value": "/",
                                                            },
                                                            "right": {
                                                                "_kind": "NumericLiteral",
                                                                "_value": "8",
                                                            },
                                                        },
                                                        "op": {"_kind": "OpLe", "_value": "<="},
                                                        "right": {
                                                            "_kind": "NumericLiteral",
                                                            "_value": "1500",
                                                        },
                                                    },
                                                },
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "null",
                                                },
                                            },
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {
                                                "_kind": "UnqualifiedID",
                                                "_value": "Opaque",
                                            },
                                            "package": None,
                                        },
                                        "type_arguments": [],
                                    },
                                ],
                                "initial_field": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Frame"},
                        "parameters": None,
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Ethernet"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Ethernet"},
            },
        },
    }

    assert_ast_files([f"{SPEC_DIR}/ethernet.rflx"], spec)


def test_parse_error_illegal_package_identifiers() -> None:
    assert_error_string(
        """\
        package RFLX_Types is
        end RFLX_Types;
        """,
        r'^<stdin>:1:9: error: illegal prefix "RFLX" in package identifier "RFLX_Types"$',
    )


def test_parse_error_inconsistent_package_identifiers() -> None:
    assert_error_string(
        """\
        package A is
        end B;
        """,
        r'^<stdin>:2:5: error: inconsistent package identifier "B"\n'
        r'<stdin>:1:9: note: previous identifier was "A"$',
    )


def test_parse_error_incorrect_name() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/invalid/incorrect_name.rflx"],
        f"^{SPEC_DIR}/invalid/incorrect_name.rflx:1:9: error: "
        'source file name does not match the package name "Test"\n'
        f'{SPEC_DIR}/invalid/incorrect_name.rflx:1:9: help: either rename the file to "test.rflx" '
        f'or change the package name to "Incorrect_Name"\n'
        f'{SPEC_DIR}/invalid/incorrect_name.rflx:1:9: help: rename to "Incorrect_Name"\n'
        f'{SPEC_DIR}/invalid/incorrect_name.rflx:3:5: help: rename to "Incorrect_Name"$',
    )


@pytest.mark.parametrize("filename", ["Tls", "TLS", "BadCasing"])
def test_parse_error_incorrect_name_should_rename_only(
    filename: str,
) -> None:
    assert_error_files(
        [f"{SPEC_DIR}/invalid/{filename}.rflx"],
        f'^{SPEC_DIR}/invalid/{filename}.rflx:1:9: error: source file name "{filename}.rflx" must '
        "be in lower case characters only\n"
        f"{SPEC_DIR}/invalid/{filename}.rflx:1:9: help: rename the file "
        f'to "{filename.lower()}.rflx"$',
    )


def test_parse_error_incorrect_specification() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/invalid/incorrect_specification.rflx"],
        f"^{SPEC_DIR}/invalid/incorrect_specification.rflx:3:10: error: Expected 'is', got ';'$",
    )


def test_parse_error_unexpected_exception_in_parser(monkeypatch: pytest.MonkeyPatch) -> None:
    p = parser.Parser()
    monkeypatch.setattr(parser, "check_naming", lambda _x, _e, _o: raise_parser_error())
    with pytest.raises(RecordFluxError, match=r"^error: TEST$"):
        p.parse_string(
            """\
            package Test is
               type T is range 0 .. 255 with Size => 8;
            end Test;
            """,
        )


def test_parse_error_context_dependency_cycle() -> None:
    p = parser.Parser()

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            f"{SPEC_DIR}/invalid/context_cycle.rflx:1:6: error: dependency cycle when "
            f'including "Context_Cycle_1"\n'
            f"{SPEC_DIR}/invalid/context_cycle_1.rflx:1:6: "
            'note: when including "Context_Cycle_2"\n'
            f"{SPEC_DIR}/invalid/context_cycle_2.rflx:1:6: "
            'note: when including "Context_Cycle_3"\n'
            f"{SPEC_DIR}/invalid/context_cycle_3.rflx:1:6: "
            'note: when including "Context_Cycle_1"'
            r"$"
        ),
    ):
        p.parse(Path(f"{SPEC_DIR}/invalid/context_cycle.rflx"))

    p.parse(Path(f"{SPEC_DIR}/integer_type.rflx"))


def test_parse_error_context_dependency_cycle_2() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/invalid/context_cycle_1.rflx"],
        f"^"
        f"{SPEC_DIR}/invalid/context_cycle_1.rflx:1:6: error: dependency cycle when "
        f'including "Context_Cycle_2"\n'
        f"{SPEC_DIR}/invalid/context_cycle_2.rflx:1:6: "
        'note: when including "Context_Cycle_3"\n'
        f"{SPEC_DIR}/invalid/context_cycle_3.rflx:1:6: "
        'note: when including "Context_Cycle_1"'
        f"$",
    )


def test_parse_string_error_context_dependency_cycle() -> None:
    p = parser.Parser()
    p.parse_string(Path(f"{SPEC_DIR}/invalid/context_cycle.rflx").read_text())
    p.parse_string(Path(f"{SPEC_DIR}/invalid/context_cycle_1.rflx").read_text())
    p.parse_string(Path(f"{SPEC_DIR}/invalid/context_cycle_2.rflx").read_text())

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:6: error: dependency cycle when including "Context_Cycle_1"\n'
            r'<stdin>:1:6: note: when including "Context_Cycle_2"\n'
            r'<stdin>:1:6: note: when including "Context_Cycle_3"'
            r"$"
        ),
    ):
        p.parse_string(Path(f"{SPEC_DIR}/invalid/context_cycle_3.rflx").read_text())

    p.parse_string(Path(f"{SPEC_DIR}/integer_type.rflx").read_text())


def test_parse_error_message_undefined_message_field() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 Foo : T
                    then Bar if Foo < 100
                    then null if Foo >= 100;
              end message;
        end Test;
        """,
        r'^<stdin>:6:18: error: undefined field "Bar"$',
    )


def test_parse_error_invalid_location_expression() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 Foo : T
                    then Bar
                       with Foo => 1;
                Bar : T;
              end message;
        end Test;
        """,
        r'^<stdin>:7:21: error: invalid aspect "Foo"$',
    )


def test_parse_error_sequence_undefined_type() -> None:
    assert_error_string(
        """\
        package Test is
           type T is sequence of Foo;
        end Test;
        """,
        r'^<stdin>:2:26: error: undefined element type "Test::Foo"$',
    )


def test_parse_error_refinement_undefined_message() -> None:
    assert_error_string(
        """\
        package Test is
           for PDU use (Foo => Bar);
        end Test;
        """,
        r'^<stdin>:2:8: error: undefined type "Test::PDU" in refinement\n'
        r'<stdin>:2:24: error: undefined type "Test::Bar" in refinement of "Test::PDU"$',
    )


def test_parse_error_refinement_undefined_sdu() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 Foo : T;
              end message;
           for PDU use (Foo => Bar);
        end Test;
        """,
        r'^<stdin>:7:24: error: undefined type "Test::Bar" in refinement of "Test::PDU"$',
    )


def test_parse_error_derivation_undefined_type() -> None:
    assert_error_string(
        """\
        package Test is
           type Bar is new Foo;
        end Test;
        """,
        r'^<stdin>:2:20: error: undefined base message "Test::Foo" in derived message$',
    )


def test_parse_error_derivation_unsupported_type() -> None:
    assert_error_string(
        """\
        package Test is
           type Foo is range 0 .. 255 with Size => 8;
           type Bar is new Foo;
        end Test;
        """,
        r"^"
        r"<stdin>:3:9: error: invalid derivation\n"
        r"<stdin>:2:9: note: base type must be a message"
        r"$",
    )


def test_parse_error_multiple_initial_node_edges() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 null
                    then Foo,
                    then Bar;
                 Foo : T;
                 Bar : T;
              end message;
        end Test;
        """,
        r"^<stdin>:6:21: error: Expected ';', got ','$",
    )


def test_parse_error_multiple_initial_nodes() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 null
                    then Foo;
                 null
                    then Bar;
                 Foo : T;
                 Bar : T;
              end message;
        end Test;
        """,
        r"^<stdin>:8:13: error: Expected ':', got 'then'$",
    )


@pytest.mark.parametrize("keyword", ADA_KEYWORDS)
def test_parse_error_reserved_word_in_type_name(keyword: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type {keyword.title()} is range 0 .. 255 with Size => 8;
        end Test;
        """,
        rf'^<stdin>:2:9: error: reserved word "{keyword.title()}" used as identifier$',
    )


@pytest.mark.parametrize("keyword", [*ADA_KEYWORDS, "message"])
def test_parse_error_reserved_word_in_message_field(keyword: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 {keyword.title()} : T;
              end message;
        end Test;
        """,
        rf'^<stdin>:5:10: error: reserved word "{keyword.title()}" used as identifier$',
    )


def test_parse_error_reserved_word_in_refinement_field() -> None:
    assert_error_string(
        """\
        package Test is
           type PDU is
              message
                 End : Opaque;
              end message;
           for PDU use (End => PDU);
        end Test;
        """,
        r"^"
        r'<stdin>:4:10: error: reserved word "End" used as identifier\n'
        r'<stdin>:6:17: error: reserved word "End" used as identifier'
        r"$",
    )


def test_parse_error_reserved_word_in_state_machine_identifier() -> None:
    assert_error_string(
        """\
        package Test is
           generic
           machine End is
           begin
              state S
              is
              begin
              transition
                 goto null
              end S;
           end End;
        end Test;
        """,
        r'^<stdin>:3:12: error: reserved word "End" used as identifier$',
    )


@pytest.mark.parametrize("identifier", ILLEGAL_IDENTIFIERS)
def test_parse_error_illegal_identifier_in_type_name(identifier: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type {identifier} is range 0 .. 255 with Size => 8;
        end Test;
        """,
        r"^"
        rf'<stdin>:2:9: error: illegal identifier "{identifier}"\n'
        r'<stdin>:2:9: note: identifiers starting with "RFLX_"'
        r" are reserved for internal use"
        r"$",
    )


@pytest.mark.parametrize("identifier", ILLEGAL_IDENTIFIERS)
def test_parse_error_illegal_identifier_in_message_field(identifier: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 {identifier} : T;
              end message;
        end Test;
        """,
        r"^"
        rf'<stdin>:5:10: error: illegal identifier "{identifier}"\n'
        r'<stdin>:5:10: note: identifiers starting with "RFLX_"'
        r" are reserved for internal use"
        r"$",
    )


@pytest.mark.parametrize("identifier", ILLEGAL_IDENTIFIERS)
def test_parse_error_illegal_identifier_in_refinement_field(identifier: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type PDU is
              message
                 {identifier} : Opaque;
              end message;
           for PDU use ({identifier} => PDU);
        end Test;
        """,
        r"^"
        rf'<stdin>:4:10: error: illegal identifier "{identifier}"\n'
        r'<stdin>:4:10: note: identifiers starting with "RFLX_"'
        r" are reserved for internal use\n"
        rf'<stdin>:6:17: error: illegal identifier "{identifier}"\n'
        r'<stdin>:6:17: note: identifiers starting with "RFLX_"'
        r" are reserved for internal use"
        r"$",
    )


@pytest.mark.parametrize("identifier", ILLEGAL_IDENTIFIERS)
def test_parse_error_illegal_identifier_in_state_machine_identifier(identifier: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           generic
           machine {identifier} is
           begin
              state S
              is
              begin
              transition
                 goto null
              end S;
           end {identifier};
        end Test;
        """,
        r"^"
        rf'<stdin>:3:12: error: illegal identifier "{identifier}"\n'
        r'<stdin>:3:12: note: identifiers starting with "RFLX_"'
        r" are reserved for internal use"
        r"$",
    )


def test_parse_error_invalid_context_clause(tmp_path: Path) -> None:
    test_file = tmp_path / "test.rflx"

    test_file.write_text("with invalid", encoding="utf-8")

    p = parser.Parser()

    with pytest.raises(
        RecordFluxError,
        match=rf"^{test_file}:1:13: error: Expected ';', got Termination$",
    ):
        p.parse(test_file)


@pytest.mark.parametrize("spec", ["empty_package", "context"])
def test_create_model_no_messages(spec: str) -> None:
    assert_messages_files([f"{SPEC_DIR}/{spec}.rflx"], [])


def test_create_model_message_type_message() -> None:
    simple_structure = [
        model.Link(model.INITIAL, model.Field("Bar"), location=Location((1, 1))),
        model.Link(
            model.Field("Bar"),
            model.Field("Baz"),
            size=expr.Mul(expr.Number(8), expr.Variable("Bar"), location=Location((2, 2))),
            location=Location((2, 2)),
        ),
        model.Link(
            model.Field("Baz"),
            model.FINAL,
        ),
    ]

    simple_types = {
        model.Field(ID("Bar", location=Location((1, 1)))): model.Integer(
            "Message_Type::T",
            expr.Number(0),
            expr.Number(255),
            expr.Number(8),
        ),
        model.Field(ID("Baz", location=Location((1, 1)))): model.Opaque(),
    }

    simple_message = model.Message(
        ID("Message_Type::Simple_PDU", Location((1, 1))),
        simple_structure,
        simple_types,
        location=Location((1, 1), end=(1, 2)),
    )

    structure = [
        model.Link(model.INITIAL, model.Field("Foo"), location=Location((1, 1))),
        model.Link(
            model.Field("Foo"),
            model.Field("Bar"),
            expr.LessEqual(expr.Variable("Foo"), expr.Number(30, 16), location=Location((3, 3))),
            location=Location((3, 3)),
        ),
        model.Link(
            model.Field("Foo"),
            model.Field("Baz"),
            expr.Greater(expr.Variable("Foo"), expr.Number(30, 16), location=Location((5, 5))),
            size=expr.Mul(expr.Number(8), expr.Variable("Foo"), location=Location((5, 5))),
            location=Location((5, 5)),
        ),
        model.Link(
            model.Field("Bar"),
            model.Field("Baz"),
            size=expr.Mul(expr.Number(8), expr.Variable("Foo"), location=Location((6, 6))),
            location=Location((6, 6)),
        ),
        model.Link(
            model.Field("Baz"),
            model.FINAL,
            location=Location((7, 7)),
        ),
    ]

    types = {
        **simple_types,
        model.Field(ID("Foo", location=Location((1, 1)))): model.Integer(
            "Message_Type::T",
            expr.Number(0),
            expr.Number(255),
            expr.Number(8),
        ),
    }

    message = model.Message(
        ID("Message_Type::PDU", Location((1, 1))),
        structure,
        types,
        location=Location((1, 1), end=(1, 2)),
    )

    empty_message = model.Message(
        ID("Message_Type::Empty_PDU", Location((1, 1))),
        [],
        {},
        location=Location((1, 1), end=(1, 2)),
    )

    assert_messages_files(
        [f"{SPEC_DIR}/message_type.rflx"],
        [message, simple_message, empty_message],
    )


def test_create_model_message_in_message() -> None:
    length = model.Integer(
        "Message_In_Message::Length",
        expr.Number(0),
        expr.Sub(expr.Pow(expr.Number(2), expr.Number(16)), expr.Number(1)),
        expr.Number(16),
    )

    length_value = model.Message(
        ID("Message_In_Message::Length_Value", Location((1, 1))),
        [
            model.Link(model.INITIAL, model.Field("Length"), location=Location((1, 1))),
            model.Link(
                model.Field("Length"),
                model.Field("Value"),
                size=expr.Mul(expr.Number(8), expr.Variable("Length"), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            model.Link(model.Field("Value"), model.FINAL, location=Location((3, 3))),
        ],
        {
            model.Field(ID("Length", location=Location((1, 1)))): length,
            model.Field(ID("Value", location=Location((2, 2)))): model.Opaque(),
        },
        location=Location((1, 1), end=(1, 2)),
    )

    derived_length_value = model.DerivedMessage(
        ID("Message_In_Message::Derived_Length_Value", Location((1, 1))),
        length_value,
    )

    message = model.Message(
        ID("Message_In_Message::Message", Location((1, 1))),
        [
            model.Link(model.INITIAL, model.Field("Foo_Length"), location=Location((1, 1))),
            model.Link(
                model.Field("Foo_Value"),
                model.Field("Bar_Length"),
                location=Location((2, 2)),
            ),
            model.Link(model.Field("Bar_Value"), model.FINAL, location=Location((3, 3))),
            model.Link(
                model.Field("Foo_Length"),
                model.Field("Foo_Value"),
                size=expr.Mul(
                    expr.Variable("Foo_Length"),
                    expr.Number(8),
                    location=Location((4, 4)),
                ),
                location=Location((4, 4)),
            ),
            model.Link(
                model.Field("Bar_Length"),
                model.Field("Bar_Value"),
                size=expr.Mul(
                    expr.Variable("Bar_Length"),
                    expr.Number(8),
                    location=Location((5, 5)),
                ),
                location=Location((5, 5)),
            ),
        ],
        {
            model.Field(ID("Foo_Length", location=Location((1, 1)))): length,
            model.Field(ID("Foo_Value", location=Location((2, 2)))): model.Opaque(),
            model.Field(ID("Bar_Length", location=Location((3, 3)))): length,
            model.Field(ID("Bar_Value", location=Location((4, 4)))): model.Opaque(),
        },
        location=Location((1, 1), end=(1, 2)),
    )

    derived_message = model.DerivedMessage(
        ID("Message_In_Message::Derived_Message", Location((1, 1))),
        message,
    )

    assert_messages_files(
        [f"{SPEC_DIR}/message_in_message.rflx"],
        [length_value, derived_length_value, message, derived_message],
    )


def test_create_model_ethernet_frame() -> None:
    assert_messages_files([f"{SPEC_DIR}/ethernet.rflx"], [models.ethernet_frame()])


def test_create_model_type_derivation_message() -> None:
    t = model.Integer("Test::T", expr.Number(0), expr.Number(255), expr.Number(8))

    structure = [
        model.Link(model.INITIAL, model.Field("Baz"), location=Location((1, 1))),
        model.Link(model.Field("Baz"), model.FINAL, location=Location((2, 2))),
    ]

    types = {model.Field(ID("Baz", location=Location((1, 1)))): t}

    message_foo = model.Message(
        ID("Test::Foo", Location((1, 1))),
        structure,
        types,
        location=Location((1, 1), end=(1, 2)),
    )
    message_bar = model.DerivedMessage(ID("Test::Bar", Location((1, 1))), message_foo)

    assert_messages_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type Foo is
              message
                 Baz : T;
              end message;
           type Bar is new Foo;
        end Test;
        """,
        [message_foo, message_bar],
    )


def test_create_model_type_derivation_refinements() -> None:
    message_foo = model.Message(
        ID("Test::Foo", Location((1, 1))),
        [
            model.Link(
                model.INITIAL,
                model.Field("Baz"),
                size=expr.Number(48, location=Location((1, 1))),
                location=Location((1, 1)),
            ),
            model.Link(model.Field("Baz"), model.FINAL, location=Location((2, 2))),
        ],
        {model.Field(ID("Baz", location=Location((1, 1)))): model.Opaque()},
        location=Location((1, 1), end=(1, 2)),
    )
    message_bar = model.DerivedMessage(ID("Test::Bar", Location((1, 1))), message_foo)

    assert_refinements_string(
        """\
        package Test is
           type Foo is
              message
                 null
                    then Baz
                       with Size => 48;
                 Baz : Opaque;
              end message;
           for Foo use (Baz => Foo);
           type Bar is new Foo;
           for Bar use (Baz => Bar);
        end Test;
        """,
        [
            model.Refinement("Test", message_foo, model.Field("Baz"), message_foo),
            model.Refinement("Test", message_bar, model.Field("Baz"), message_bar),
        ],
    )


def test_create_model_message_locations() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M is
              message
                 F1 : T;
                 F2 : T;
              end message;
        end Test;
        """,
    )
    m = p.create_model()
    assert [f.identifier.location for f in m.messages[0].fields] == [
        Location((5, 10), Path("<stdin>"), (5, 12)),
        Location((6, 10), Path("<stdin>"), (6, 12)),
    ]


def test_create_model_state_machine_locations() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is
           generic
              with function F return Boolean;
           machine S is
              Y : Boolean := F;
           begin
              state A is
                 Z : Boolean := Y;
              begin
                 Z := False;
              transition
                 goto null
                    if Z = False
                 goto A
              exception
                 goto null
              end A;
           end S;
        end Test;
        """,
    )
    m = p.create_model()
    assert [p.location for p in m.state_machines[0].parameters] == [
        Location((3, 21), Path("<stdin>"), (3, 22)),
    ]


def test_create_model_sequence_with_imported_element_type() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
           package Test is
              type T is range 0 .. 255 with Size => 8;
           end Test;
        """,
    )
    p.parse_string(
        """\
           with Test;
           package Sequence_Test is
              type T is sequence of Test::T;
           end Sequence_Test;
        """,
    )
    m = p.create_model()
    sequences = [t for t in m.types if isinstance(t, model.Sequence)]
    assert len(sequences) == 1
    assert sequences[0].identifier == ID("Sequence_Test::T")
    assert sequences[0].element_type == model.Integer(
        "Test::T",
        expr.Number(0),
        expr.Number(255),
        expr.Number(8),
    )


def test_create_model_checksum() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M is
              message
                 F1 : T;
                 F2 : T;
                 C1 : T;
                 F3 : T;
                 C2 : T
                    then F4
                       if C1'Valid_Checksum and C2'Valid_Checksum;
                 F4 : T;
              end message
                 with Checksum => (C1 => (F3, F1'First .. F2'Last),
                                   C2 => (C1'Last + 1 .. C2'First - 1));
        end Test;
        """,
    )
    m = p.create_model()

    assert m.messages[0].checksums == {
        ID("C1"): [expr.Variable("F3"), expr.ValueRange(expr.First("F1"), expr.Last("F2"))],
        ID("C2"): [
            expr.ValueRange(
                expr.Add(expr.Last("C1"), expr.Number(1)),
                expr.Sub(expr.First("C2"), expr.Number(1)),
            ),
        ],
    }


@pytest.mark.parametrize(
    ("spec", "byte_order"),
    [
        (
            """\
            package Test is
               type T is range 0 .. 2 ** 8 - 1 with Size => 8;
               type M is
                  message
                     F1 : T;
                     F2 : T;
                     F3 : T;
                  end message
                     with Byte_Order => Low_Order_First;
            end Test;
            """,
            model.ByteOrder.LOW_ORDER_FIRST,
        ),
        (
            """\
            package Test is
               type T is range 0 .. 2 ** 8 - 1 with Size => 8;
               type M is
                  message
                     F1 : T;
                     F2 : T;
                     F3 : T;
                  end message
                     with Byte_Order => High_Order_First;
            end Test;
            """,
            model.ByteOrder.HIGH_ORDER_FIRST,
        ),
        (
            """\
            package Test is
               type T is range 0 .. 2 ** 8 - 1 with Size => 8;
               type M is
                  message
                     F1 : T;
                     F2 : T;
                     F3 : T;
                  end message;
            end Test;
            """,
            model.ByteOrder.HIGH_ORDER_FIRST,
        ),
        (
            """\
            package Test is
               type T is range 0 .. 2 ** 8 - 1 with Size => 8;
               type M is
                  message
                     F1 : T;
                     F2 : T;
                     C1 : T;
                     F3 : T;
                     C2 : T
                        then F4
                           if C1'Valid_Checksum and C2'Valid_Checksum;
                     F4 : T;
                  end message
                     with Checksum => (C1 => (F3, F1'First .. F2'Last),
                                       C2 => (C1'Last + 1 .. C2'First - 1)),
                          Byte_Order => Low_Order_First;
            end Test;
            """,
            model.ByteOrder.LOW_ORDER_FIRST,
        ),
    ],
)
def test_create_model_byteorder(spec: str, byte_order: ByteOrder) -> None:
    p = parser.Parser()
    p.parse_string(spec)
    m = p.create_model()
    for v in m.messages[0].byte_order.values():
        assert v == byte_order


@pytest.mark.parametrize(
    "spec",
    [
        """\
        type M is
           message
              A : T
                 then B
                    if A > 10 and A < 100;
              B : T;
           end message;
        """,
    ],
)
def test_message_field_condition(spec: str) -> None:
    spec = textwrap.indent(textwrap.dedent(spec), "           ")
    assert_messages_string(
        f"""\
        package Test is

           type T is range 0 .. 255 with Size => 8;
{spec}
        end Test;
        """,
        [
            Message(
                ID("Test::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("A"), location=Location((1, 1))),
                    Link(
                        Field("A"),
                        Field("B"),
                        condition=expr.And(
                            expr.Greater(expr.Variable("A"), expr.Number(10)),
                            expr.Less(expr.Variable("A"), expr.Number(100)),
                            location=Location((2, 1)),
                        ),
                        location=Location((2, 2)),
                    ),
                    Link(Field("B"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("A", location=Location((1, 1)))): T,
                    Field(ID("B", location=Location((2, 2)))): T,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
        ],
    )


@pytest.mark.parametrize(
    "spec",
    [
        """\
        type M is
           message
              A : T
                 then B
                    with First => A'First;
              B : T;
           end message;
        """,
        """\
        type M is
           message
              A : T;
              B : T
                 with First => A'First;
           end message;
        """,
    ],
)
def test_message_field_first(spec: str) -> None:
    spec = textwrap.indent(textwrap.dedent(spec), "           ")
    assert_messages_string(
        f"""\
        package Test is
           type T is range 0 .. 255 with Size => 8;
{spec}
        end Test;
        """,
        [
            Message(
                ID("Test::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("A"), location=Location((1, 1))),
                    Link(Field("A"), Field("B"), first=expr.First("A"), location=Location((2, 2))),
                    Link(Field("B"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("A", location=Location((1, 1)))): T,
                    Field(ID("B", location=Location((2, 2)))): T,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
        ],
    )


@pytest.mark.parametrize(
    "spec",
    [
        """\
        type M is
           message
              A : T
                 then B
                    with Size => 8;
              B : Opaque;
           end message;
        """,
        """\
        type M is
           message
              A : T;
              B : Opaque
                 with Size => 8;
           end message;
        """,
    ],
)
def test_message_field_size(spec: str) -> None:
    spec = textwrap.indent(textwrap.dedent(spec), "           ")
    assert_messages_string(
        f"""\
        package Test is

           type T is range 0 .. 255 with Size => 8;

{spec}
        end Test;
        """,
        [
            Message(
                ID("Test::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("A"), location=Location((1, 1))),
                    Link(
                        Field("A"),
                        Field("B"),
                        size=expr.Number(8, location=Location((2, 2))),
                        location=Location((2, 2)),
                    ),
                    Link(Field("B"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("A", location=Location((1, 1)))): T,
                    Field(ID("B", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
        ],
    )


@pytest.mark.parametrize(
    ("link", "field_b"),
    [
        ("with First => A'First if A > 10 and A < 100", "with Size => 8"),
        ("with First => A'First, Size => 8 if A > 10 and A < 100", ""),
        ("with Size => 8 if A > 10 and A < 100", "with First => A'First"),
        ("if A > 10 and A < 100", "with First => A'First, Size => 8"),
    ],
)
def test_message_field_condition_and_aspects(link: str, field_b: str) -> None:
    link = f"\n                       {link}" if link else ""
    field_b = f"\n                    {field_b}" if field_b else ""
    assert_messages_string(
        f"""\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type M is
              message
                 A : T
                    then B{link};
                 B : Opaque{field_b};
              end message;
        end Test;
        """,
        [
            Message(
                ID("Test::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("A"), location=Location((1, 1))),
                    Link(
                        Field("A"),
                        Field("B"),
                        first=expr.First("A"),
                        size=expr.Number(8, location=Location((2, 2))),
                        condition=expr.And(
                            expr.Greater(expr.Variable("A"), expr.Number(10)),
                            expr.Less(expr.Variable("A"), expr.Number(100)),
                            location=Location((2, 3)),
                        ),
                        location=Location((2, 2)),
                    ),
                    Link(Field("B"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("A", location=Location((1, 1)))): T,
                    Field(ID("B", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
        ],
    )


def test_parameterized_messages() -> None:
    assert_messages_string(
        """\
        package Test is

           type T is range 0 .. 255 with Size => 8;

           type M (P : T) is
              message
                 F : Opaque
                    with Size => P * 8
                    then null
                       if P < 100;
              end message;

           type M_S is
              message
                 F : M (P => 16);
              end message;

           type M_D is
              message
                 L : T;
                 F : M (P => L);
              end message;

        end Test;
        """,
        [
            Message(
                ID("Test::M", Location((1, 1))),
                [
                    Link(
                        INITIAL,
                        Field("F"),
                        size=expr.Mul(
                            expr.Variable("P"),
                            expr.Number(8),
                            location=Location((1, 1)),
                        ),
                        location=Location((1, 1)),
                    ),
                    Link(
                        Field("F"),
                        FINAL,
                        condition=expr.Less(
                            expr.Variable("P"),
                            expr.Number(100),
                            location=Location((2, 1)),
                        ),
                        location=Location((2, 2)),
                    ),
                ],
                {
                    Field(ID("P", location=Location((1, 1)))): T,
                    Field(ID("F", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
            Message(
                ID("Test::M_S", Location((1, 1))),
                [
                    Link(
                        INITIAL,
                        Field("F_F"),
                        size=expr.Mul(expr.Number(16), expr.Number(8), location=Location((1, 1))),
                        location=Location((1, 1)),
                    ),
                    Link(Field("F_F"), FINAL, condition=expr.TRUE, location=Location((2, 2))),
                ],
                {Field(ID("F_F", location=Location((1, 1)))): OPAQUE},
                location=Location((1, 1), end=(1, 2)),
            ),
            Message(
                ID("Test::M_D", Location((1, 1))),
                [
                    Link(INITIAL, Field("L"), location=Location((1, 1))),
                    Link(
                        Field("L"),
                        Field("F_F"),
                        size=expr.Mul(
                            expr.Variable("L"),
                            expr.Number(8),
                            location=Location((2, 2)),
                        ),
                        location=Location((2, 2)),
                    ),
                    Link(
                        Field("F_F"),
                        FINAL,
                        condition=expr.Less(
                            expr.Variable("L"),
                            expr.Number(100),
                            location=Location((3, 2)),
                        ),
                        location=Location((3, 3)),
                    ),
                ],
                {
                    Field(ID("L", location=Location((1, 1)))): T,
                    Field(ID("F_F", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
        ],
    )


@pytest.mark.parametrize(
    ("parameters", "error"),
    [
        (
            "",
            r"^"
            r"<stdin>:15:14: error: missing argument\n"
            r'<stdin>:5:14: help: expected argument for parameter "P"'
            r"$",
        ),
        (
            " (Q => 16)",
            r"^"
            r'<stdin>:15:19: error: unexpected argument "Q"\n'
            r'<stdin>:15:19: help: expected argument for parameter "P"'
            r"$",
        ),
        (
            " (P => 16, Q => 16)",
            r"^"
            r'<stdin>:15:28: error: unexpected argument "Q"\n'
            r"<stdin>:15:28: help: expected no argument"
            r"$",
        ),
        (
            " (Q => 16, P => 16)",
            r"^"
            r'<stdin>:15:19: error: unexpected argument "Q"\n'
            r'<stdin>:15:19: help: expected argument for parameter "P"\n'
            r'<stdin>:15:28: error: unexpected argument "P"\n'
            r"<stdin>:15:28: help: expected no argument"
            r"$",
        ),
        (
            " (P => 16, P => 16)",
            r"^"
            r'<stdin>:15:28: error: unexpected argument "P"\n'
            r"<stdin>:15:28: help: expected no argument"
            r"$",
        ),
    ],
)
def test_parse_error_invalid_arguments_for_parameterized_messages(
    parameters: str,
    error: str,
) -> None:
    assert_error_string(
        f"""\
        package Test is

           type T is range 0 .. 255 with Size => 8;

           type M_P (P : T) is
              message
                 F : Opaque
                    with Size => P * 8
                    then null
                       if P < 100;
              end message;

           type M is
              message
                 F : M_P{parameters};
              end message;

        end Test;
        """,
        error,
    )


def test_parse_error_invalid_range_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 1 .. 200 with Invalid => 42;
        end Test;
        """,
        r'^<stdin>:2:14: error: invalid aspect "Invalid" for range type "Test::T"$',
    )


@pytest.mark.parametrize(
    ("spec", "error"),
    [
        (
            """\
        package Test is
           type T is (A, B) with Foo;
        end Test;
        """,
            r'^<stdin>:2:9: error: no size set for "Test::T"$',
        ),
        (
            """\
        package Test is
           type T is (A, B) with Always_Valid => Invalid;
        end Test;
        """,
            r"^"
            "<stdin>:2:42: error: invalid Always_Valid expression: Invalid\n"
            '<stdin>:2:9: error: no size set for "Test::T"'
            r"$",
        ),
    ],
)
def test_parse_error_invalid_enum(spec: str, error: str) -> None:
    assert_error_string(spec, error)


@pytest.mark.parametrize(
    "spec",
    [
        "type X (P : Y) is range 1 .. 100 with Size => 8",
        "type X (P : Y) is (A, B) with Size => 8",
        "type X (P : Y) is sequence of T",
        "type X (P : Y) is new M",
        "type X (P : Y) is null message",
    ],
)
def test_parse_error_invalid_parameterized_type(spec: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M is
              message
                 F : T;
              end message;
           {spec};
        end Test;
        """,
        r"^<stdin>:7:12: error: only message types can be parameterized$",
    )


def test_parse_error_undefined_parameter() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M (P : X) is
              message
                 F : T;
              end message;
        end Test;
        """,
        r'^<stdin>:3:16: error: undefined type "Test::X"$',
    )


def test_parse_error_name_conflict_between_parameters() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M (P : T; P : T) is
              message
                 F : T
                    then null
                       if P = F;
              end message;
        end Test;
        """,
        r"^"
        r'<stdin>:3:19: error: name conflict for "P"\n'
        r"<stdin>:3:12: note: conflicting name"
        r"$",
    )


def test_parse_error_name_conflict_between_field_and_parameter() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M (P : T) is
              message
                 P : T;
              end message;
        end Test;
        """,
        r"^"
        r'<stdin>:5:10: error: name conflict for "P"\n'
        r"<stdin>:3:12: note: conflicting name"
        r"$",
    )


def test_parse_error_duplicate_spec_file() -> None:
    p = parser.Parser()
    p.parse(SPEC_DIR / "message_type.rflx")
    with pytest.raises(
        RecordFluxError,
        match=(
            "^tests/data/specs/subdir/message_type.rflx:1:9: error: duplicate"
            " specification\n"
            "tests/data/specs/message_type.rflx:1:9: note: previous specification$"
        ),
    ):
        p.parse(SPEC_DIR / "subdir/message_type.rflx")


def test_parse_error_duplicate_spec_stdin_file() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Message_Type is
           type T is range 0 .. 2 ** 32 - 1 with Size => 32;
           type M is
              message
                 F : T;
              end message;
        end Message_Type;
        """,
    )
    with pytest.raises(
        RecordFluxError,
        match=(
            "^tests/data/specs/subdir/message_type.rflx:1:9: error: duplicate"
            " specification\n"
            "<stdin>:1:9: note: previous specification$"
        ),
    ):
        p.parse(SPEC_DIR / "subdir/message_type.rflx")


@pytest.mark.parametrize("keyword", ADA_KEYWORDS)
def test_parse_reserved_words_as_enum_literals(keyword: str) -> None:
    p = parser.Parser()
    p.parse_string(
        f"""\
        package Test is
           type A is (Reset => 1, {keyword.title()} => 2) with Size => 8;
        end Test;
        """,
    )


def test_parse_reserved_word_as_channel_name() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is
           generic
              Channel : Channel with Readable, Writable;
           machine S is
           begin
              state A
              is
                 Avail : Boolean := Channel'Has_Data;
              begin
              transition
                 goto A if Avail
                 goto null
              end A;
           end S;
        end Test;
        """,
    )


@pytest.mark.parametrize(
    "keyword",
    RESERVED_WORDS,
)
def test_parse_reserved_word_in_link_condition(keyword: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type I is range 0 .. 255 with Size => 8;
           type M (A : Boolean) is
              message
                 X : Boolean
                    then Z
                       if A and {keyword};
                 Z : Opaque
                    with Size => 8;
              end message;
        end Test;
        """,
        rf'^<stdin>:7:25: error: reserved word "{keyword}" used as identifier\n'
        rf'<stdin>:7:25: error: undefined variable "{keyword}"$',
    )


def test_parse_error_duplicate_message_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M is
              message
                 A : T;
                 B : Opaque
                    with First => A'First, Size => A'Size, First => A'First, Size => A'Size;
              end message;
        end Test;
        """,
        r'^<stdin>:7:52: error: duplicate aspect "First"\n'
        "<stdin>:7:18: note: previous location\n"
        '<stdin>:7:70: error: duplicate aspect "Size"\n'
        "<stdin>:7:36: note: previous location$",
    )


def test_parse_error_duplicate_channel_decl_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type Message is
              message
                 A : T;
              end message;
           generic
              C : Channel with Readable, Writable, Readable;
           machine S is
              M : Test::Message;
           begin
              state I
              is
              begin
                 C'Read (M);
              transition
                 goto null
              end I;
           end S;
        end Test;
        """,
        r'^<stdin>:8:44: error: duplicate aspect "Readable"\n'
        "<stdin>:8:24: note: previous location$",
    )


def test_parse_error_unsupported_binding() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type Message is
              message
                 A : T;
              end message;
           generic
              C : Channel with Writable;
           machine S is
              M : Test::Message;
           begin
              state I
              is
              begin
                 C'Write (M where M = Test::Message'(A => 1));
              transition
                 goto null
              end I;
           end S;
        end Test;
        """,
        r"^<stdin>:15:19: error: bindings are not supported$",
    )


def test_parse_error_unsupported_modular_integer_type() -> None:
    assert_error_string(
        """\
        package Test is
           type T is mod 2 ** 16;
        end Test;
        """,
        r"^"
        r"<stdin>:2:9: error: modular integer types are not supported\n"
        r'<stdin>:2:9: help: use "type T is range 0 .. 65535 with Size => 16" instead'
        r"$",
    )


def test_parse_error_duplicate_integer_size_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 1 .. 10 with Size => 8, Size => 16;
        end Test;
        """,
        r"^<stdin>:2:42: error: Expected ';', got ','$",
    )


def test_parse_error_duplicate_enumeration_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type E is (L1, L2, L3) with Size => 8, Always_Valid, Size => 16, Always_Valid;
        end Test;
        """,
        r'^<stdin>:2:57: error: duplicate aspect "Size"\n'
        r"<stdin>:2:32: note: previous location\n"
        r'<stdin>:2:69: error: duplicate aspect "Always_Valid"\n'
        r"<stdin>:2:43: note: previous location$",
    )


def test_parse_error_duplicate_message_checksum_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M is
              message
                 F1 : T;
                 F2 : T;
              end message
                 with Checksum => (F1 => (F1'First .. F1'Last)),
                      Checksum => (F1 => (F1'First .. F1'Last));
        end Test;
        """,
        r"^<stdin>:9:15: error: Expected 'Byte_Order', got 'First'$",
    )


def test_parse_error_duplicate_message_byte_order_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M is
              message
                 F1 : T;
                 F2 : T;
              end message
                 with Byte_Order => Low_Order_First,
                      Byte_Order => High_Order_First;
        end Test;
        """,
        r'^<stdin>:9:15: error: duplicate aspect "Byte_Order"\n'
        "<stdin>:8:15: note: previous location$",
    )


def test_parse_error_duplicate_state_desc() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type Message is
              message
                 A : T;
              end message;
           generic
              C : Channel with Readable, Writable;
           machine S is
              M : Test::Message;
           begin
              state I
                 with Desc => "D1", Desc => "D2"
              is
              begin
                 C'Read (M);
              transition
                 goto null
              end I;
           end S;
        end Test;
        """,
        r"^<stdin>:13:27: error: Expected 'is', got ','$",
    )


def test_parse_error_duplicate_transition_desc() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type Message is
              message
                 A : T;
              end message;
           generic
              C : Channel with Readable, Writable;
           machine S is
              M : Test::Message;
           begin
              state I
              is
              begin
                 C'Read (M);
              transition
                 goto null
                    with Desc => "D1", Desc => "D2"
              end I;
           end S;
        end Test;
        """,
        r"^<stdin>:18:30: error: Expected 'end', got ','$",
    )


def test_enumeration() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is
           type E is (E1 => 1, E2 => 2) with Always_Valid => False, Size => 8;
           type A is (A1 => 10) with Always_Valid => True, Size => 6;
        end Test;
        """,
    )
    m = p.create_model()
    assert m.types == [
        BOOLEAN,
        OPAQUE,
        Enumeration(
            "Test::E",
            [("E1", expr.Number(1)), ("E2", expr.Number(2))],
            size=expr.Number(8),
            always_valid=False,
        ),
        Enumeration(
            "Test::A",
            [("A1", expr.Number(10))],
            size=expr.Number(6),
            always_valid=True,
        ),
    ]


def test_parse_dependencies_given_as_argument(tmp_path: Path) -> None:
    a = tmp_path / "a" / "a.rflx"
    a.parent.mkdir()
    a.write_text("with B; package A is end A;")

    b = tmp_path / "b" / "b.rflx"
    b.parent.mkdir()
    b.write_text("package B is end B;")

    p = parser.Parser()
    p.parse(a, b)

    assert set(p.specifications.keys()) == {"A", "B"}

    p = parser.Parser()
    p.parse(b, a)

    assert set(p.specifications.keys()) == {"A", "B"}


def test_parse_dependencies_not_given_as_argument(tmp_path: Path) -> None:
    a = tmp_path / "a" / "a.rflx"
    a.parent.mkdir()
    a.write_text("with C; package A is end A;")

    b = tmp_path / "b" / "b.rflx"
    b.parent.mkdir()
    b.write_text("package B is end B;")

    (tmp_path / "b" / "c.rflx").write_text("with D; package C is end C;")

    (tmp_path / "a" / "d.rflx").write_text("package D is end D;")

    p = parser.Parser()
    p.parse(a, b)

    assert set(p.specifications.keys()) == {"A", "B", "C", "D"}

    p = parser.Parser()
    p.parse(b, a)

    assert set(p.specifications.keys()) == {"A", "B", "C", "D"}


def test_parse_order_of_include_paths(tmp_path: Path) -> None:
    a = tmp_path / "a" / "a.rflx"
    a.parent.mkdir()
    a.write_text("with C; package A is end A;")

    b = tmp_path / "b" / "b.rflx"
    b.parent.mkdir()
    b.write_text("package B is end B;")

    c = tmp_path / "b" / "c.rflx"
    c.write_text("with D; package C is end C;")

    (tmp_path / "a" / "d.rflx").write_text("package D is end D;")

    invalid = tmp_path / "b" / "d.rflx"
    invalid.write_text("INVALID")

    p = parser.Parser()
    p.parse(a, b)

    assert set(p.specifications.keys()) == {"A", "B", "C", "D"}

    with pytest.raises(
        RecordFluxError,
        match=(rf"^{invalid}:1:1: error: Expected 'package', got 'First'$"),
    ):
        parser.Parser().parse(b, a)


def test_parse_non_existent_dependencies(tmp_path: Path) -> None:
    a = tmp_path / "a" / "a.rflx"
    a.parent.mkdir()
    a.write_text("with C; package A is end A;")

    b = tmp_path / "b" / "b.rflx"
    b.parent.mkdir()
    b.write_text("package B is end B;")

    error = rf'^{a}:1:6: error: cannot find specification "C"$'

    with pytest.raises(RecordFluxError, match=error):
        parser.Parser().parse(a, b)

    with pytest.raises(RecordFluxError, match=error):
        parser.Parser().parse(b, a)


@pytest.mark.parametrize(
    ("expression", "message"),
    [
        ("A + 1", r"^<stdin>:7:19: error: math expression in boolean context$"),
        ("42", r"^<stdin>:7:19: error: math expression in boolean context$"),
    ],
)
def test_parse_error_math_expression_in_bool_context(expression: str, message: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type T is range 0 .. 2 ** 8 - 1 with Size => 8;
           type M is
              message
                 A : T
                    then null
                       if {expression};
              end message;
        end Test;
        """,
        message,
    )


@pytest.mark.parametrize(
    ("expression", "message"),
    [
        ("True", r"^<stdin>:5:26: error: boolean expression in math context$"),
        ("3 < 4", r"^<stdin>:5:26: error: boolean expression in math context$"),
        ("A = B", r"^<stdin>:5:26: error: boolean expression in math context$"),
        ("A /= False", r"^<stdin>:5:26: error: boolean expression in math context$"),
    ],
)
def test_parse_error_bool_expression_in_math_context(expression: str, message: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type M is
              message
                 A : Opaque
                    with Size => {expression};
              end message;
        end Test;
        """,
        message,
    )


def test_parse_error_short_form_condition() -> None:
    assert_error_string(
        """\
        package Test is
           type U8 is range 0 .. 255 with Size => 8;
           type M is
              message
                 A : U8
                    if A = 42;
              end message;
        end Test;
        """,
        r"^<stdin>:6:16: error: short form condition is not supported anymore\n"
        r'<stdin>:6:16: help: add condition to all outgoing links of field "A" instead$',
    )


def test_parse_error_missing_aspect_expression() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 1 with A;
        end Test;
        """,
        r'^<stdin>:2:14: error: invalid aspect "A" for range type "Test::T"$',
    )


def test_parse_error_unclosed_parenthesis() -> None:
    assert_error_string(
        """\
        package Test is

           type M is
              message
                 Payload : Opaque
                    with Size => (Length - () * 8;
              end message;
        end Test;
        """,
        r"^<stdin>:6:27: error: empty subexpression$",
    )


def test_negated_case_expression() -> None:
    assert_error_string(
        """\
        package Test is
           generic
           machine S is
           begin
              state A is
              begin
                 M := T'(E1 => - (case X is when Y => 42));
              transition
                 goto null
              end A;
           end S;
        end Test;
        """,
        r"^<stdin>:7:26: error: case expression unsupported in math expression context$",
    )


def test_parse_only() -> None:
    p = parser.Parser()
    p.parse(*(SPEC_DIR / "parse_only").glob("*"))


@pytest.mark.parametrize(
    ("string", "regex"),
    [
        (
            """\
            package Test is
               type M is
                  message
                     F : T
                        then null if X = (X = );
                  end message;
            end Test;
            """,
            "^<stdin>:5:31: error: missing right operand$",
        ),
        (
            """\
            package Message_Size is
               type T is range 0 .. 2 ** 8 with Size => 8;
               type M is
                  message
                     A : T;
                     B : Opaque
                        with Size => A * 8
                        then null
                           if Message'Size = (A + )'Size * 8
                        then C
                           if Message'Last = B'Last + 8;
                     C : T;
                  end message;
            end Message_Size;
            """,
            "^<stdin>:9:35: error: missing right operand$",
        ),
        (
            """\
            package Test is
               type T is range 0 .. 2 ** 8 - 1 with Size => (8 / )'Size;
            end Test;
            """,
            "^<stdin>:2:50: error: missing right operand$",
        ),
        (
            """\
            package Test is
               type T is range 0 .. (- + 1) with Size => 8;
            end Test;
            """,
            "^<stdin>:2:26: error: negation of non-expression$",
        ),
        (
            """\
            package Test is
               type M is
                  message
                     F : T then null if (A /= = B);
                  end message;
            end Test;
            """,
            "^<stdin>:4:30: error: missing right operand$",
        ),
    ],
    ids=range(1, 6),
)
def test_parse_error_duplicate_operator(string: str, regex: str) -> None:
    assert_error_string(string, regex)


@pytest.mark.parametrize(
    ("string", "regex"),
    [
        (
            """\
            package Test is
               type T is range 0 .. 2 ** 8 - 1 with Size;
            end Test;
            """,
            '^<stdin>:2:41: error: "Size" aspect has no value$',
        ),
        (
            """\
            package Test is
               type T is range 0 .. 255 with Size => 8;
               type M is
                  message
                     F1 : T
                        with First
                        then F2;
                     F2 : T
                        then null
                           if True;
                  end message;
            end Test;
            """,
            '^<stdin>:6:18: error: "First" aspect has no value$',
        ),
    ],
    ids=range(1, 3),
)
def test_parse_error_aspect_without_expression(string: str, regex: str) -> None:
    assert_error_string(string, regex)


@pytest.mark.parametrize(
    ("string", "expected"),
    [("X", ID("X")), ("X2", ID("X2")), ("X_Y", ID("X_Y")), ("X_Y_3", ID("X_Y_3"))],
)
def test_unqualified_identifier(string: str, expected: ID) -> None:
    actual = parse_id(string, lang.GrammarRule.unqualified_identifier_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X", ID("X")),
        ("X2", ID("X2")),
        ("X_Y", ID("X_Y")),
        ("X_Y_3", ID("X_Y_3")),
        ("X::Y", ID("X::Y")),
        ("X2::Y2", ID("X2::Y2")),
        ("X_Y::Z", ID("X_Y::Z")),
        ("X_Y_3::Z_4", ID("X_Y_3::Z_4")),
    ],
)
def test_qualified_identifier(string: str, expected: ID) -> None:
    actual = parse_id(string, lang.GrammarRule.qualified_identifier_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("1000", expr.Number(1000)),
        ("1_000", expr.Number(1000)),
        ("16#6664#", expr.Number(26212)),
        ("16#66_64#", expr.Number(26212)),
        ("-1000", expr.Neg(expr.Number(1000))),
        ("-1_000", expr.Neg(expr.Number(1000))),
        ("-16#6664#", expr.Neg(expr.Number(26212, base=16))),
        ("-16#66_64#", expr.Neg(expr.Number(26212, base=16))),
    ],
)
def test_expression_numeric_literal(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "error"),
    [
        ("3#100#", '<stdin>:1:2: error: End of input expected, got "Hash"'),
        ("2#555#", '<stdin>:1:2: error: End of input expected, got "Hash"'),
        ("50#123#", '<stdin>:1:3: error: End of input expected, got "Hash"'),
    ],
)
def test_invalid_expression_numeric_literal(string: str, error: expr.Expr) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_math_expression(string, extended=False)


@pytest.mark.parametrize(
    ("string", "expected"),
    [("X", expr.Variable("X")), ("X::Y", expr.Variable("X::Y"))],
)
def test_variable(string: str, expected: decl.Declaration) -> None:
    actual = parse_expression(string, lang.GrammarRule.variable_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "error"),
    [
        (
            "Double__Underscore",
            "<stdin>:1:7: error: Invalid token, ignored\n"
            "<stdin>:1:8: error: Invalid token, ignored\n"
            '<stdin>:1:9: error: End of input expected, got "Unqualified_Identifier"',
        ),
        (
            "Trailing_Underscore_ < 5",
            "<stdin>:1:20: error: Invalid token, ignored\n"
            '<stdin>:1:22: error: End of input expected, got "Lt"',
        ),
    ],
)
def test_invalid_variable(string: str, error: str) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_expression(string, lang.GrammarRule.variable_rule)


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X'First", expr.First(expr.Variable("X"))),
        ("X'Last", expr.Last(expr.Variable("X"))),
        ("X'Size", expr.Size(expr.Variable("X"))),
        ("X'Head", expr.Head(expr.Variable("X"))),
        ("X'Opaque", expr.Opaque(expr.Variable("X"))),
        ("X'Present", expr.Present(expr.Variable("X"))),
        ("X'Valid", expr.Valid(expr.Variable("X"))),
        ("X'Valid_Checksum", expr.ValidChecksum(expr.Variable("X"))),
        ("X'Has_Data", expr.HasData(expr.Variable("X"))),
        ("X'Head.Y", expr.Selected(expr.Head(expr.Variable("X")), "Y")),
    ],
)
def test_expression_suffix(string: str, expected: expr.Expr) -> None:
    actual = parse_expression(string, lang.GrammarRule.extended_expression_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        (
            "A - B * 2**3 - 1",
            expr.Sub(
                expr.Sub(
                    expr.Variable("A"),
                    expr.Mul(expr.Variable("B"), expr.Pow(expr.Number(2), expr.Number(3))),
                ),
                expr.Number(1),
            ),
        ),
        (
            "(A - B) * 2**3 - 1",
            expr.Sub(
                expr.Mul(
                    expr.Sub(expr.Variable("A"), expr.Variable("B")),
                    expr.Pow(expr.Number(2), expr.Number(3)),
                ),
                expr.Number(1),
            ),
        ),
        (
            "A - B * 2**(3 - 1)",
            expr.Sub(
                expr.Variable("A"),
                expr.Mul(
                    expr.Variable("B"),
                    expr.Pow(expr.Number(2), expr.Sub(expr.Number(3), expr.Number(1))),
                ),
            ),
        ),
        (
            "A - (B * 2)**3 - 1",
            expr.Sub(
                expr.Sub(
                    expr.Variable("A"),
                    expr.Pow(expr.Mul(expr.Variable("B"), expr.Number(2)), expr.Number(3)),
                ),
                expr.Number(1),
            ),
        ),
        (
            "A - (B * 2**3 - 1)",
            expr.Sub(
                expr.Variable("A"),
                expr.Sub(
                    expr.Mul(expr.Variable("B"), expr.Pow(expr.Number(2), expr.Number(3))),
                    expr.Number(1),
                ),
            ),
        ),
        (
            "A + B * (-8)",
            expr.Add(expr.Variable("A"), expr.Mul(expr.Variable("B"), expr.Neg(expr.Number(8)))),
        ),
        (
            "1 - - (2 + 3)",
            expr.Sub(
                expr.Number(1),
                expr.Neg(
                    expr.Add(
                        expr.Number(2),
                        expr.Number(3),
                    ),
                ),
            ),
        ),
        (
            "A + B mod 8 * 2",
            expr.Add(
                expr.Variable("A"),
                expr.Mul(expr.Mod(expr.Variable("B"), expr.Number(8)), expr.Number(2)),
            ),
        ),
    ],
)
def test_mathematical_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X + Y", expr.Add(expr.Variable("X"), expr.Variable("Y"))),
        (
            "X + Y (Z)",
            expr.Add(expr.Variable("X"), expr.Call("Y", UNDEFINED, [expr.Variable("Z")])),
        ),
    ],
)
def test_extended_mathematical_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_math_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X = Y", expr.Equal(expr.Variable("X"), expr.Variable("Y"))),
        ("X /= Y", expr.NotEqual(expr.Variable("X"), expr.Variable("Y"))),
        ("42 < X", expr.Less(expr.Number(42), expr.Variable("X"))),
        ("42 <= X", expr.LessEqual(expr.Number(42), expr.Variable("X"))),
        ("X > 42", expr.Greater(expr.Variable("X"), expr.Number(42))),
        ("X >= 42", expr.GreaterEqual(expr.Variable("X"), expr.Number(42))),
        ("((X = 42))", expr.Equal(expr.Variable("X"), expr.Number(42))),
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X or Y", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
        ("((X or Y))", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
    ],
)
def test_boolean_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=False)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        (
            "X and Y (Z)",
            expr.And(expr.Variable("X"), expr.Call("Y", UNDEFINED, [expr.Variable("Z")])),
        ),
    ],
)
def test_extended_boolean_expression(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "error", "extended"),
    [
        ("42 > X", "<stdin>:1:1: error: boolean expression in math context", False),
        ("42 > X", "<stdin>:1:1: error: boolean expression in math context", True),
        ("X and Y", "<stdin>:1:1: error: boolean expression in math context", False),
        ("X and Y", "<stdin>:1:1: error: boolean expression in math context", True),
        ("(5 + ()", "<stdin>:1:2: error: empty subexpression", False),
        (
            "(5 + ()",
            "<stdin>:1:2: error: Cannot parse <extended_simple_expr>\n"
            r"<stdin>:1:7: error: Expected 'case', got '\)'",
            True,
        ),
    ],
)
def test_mathematical_expression_error(string: str, error: expr.Expr, extended: bool) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_math_expression(string, extended=extended)


@pytest.mark.parametrize(
    ("string", "error", "extended"),
    [
        ("42", "<stdin>:1:1: error: math expression in boolean context", True),
        ("42", "<stdin>:1:1: error: math expression in boolean context", False),
        ("X * 3", "<stdin>:1:1: error: math expression in boolean context", True),
        ("X * 3", "<stdin>:1:1: error: math expression in boolean context", False),
        (
            "(True and ()",
            "<stdin>:1:2: error: Cannot parse <extended_expression>\n"
            r"<stdin>:1:12: error: Expected 'case', got '\)'",
            True,
        ),
        ("(True and ()", "<stdin>:1:2: error: empty subexpression", False),
    ],
)
def test_boolean_expression_error(string: str, error: expr.Expr, extended: bool) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_bool_expression(string, extended=extended)


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("42", expr.Number(42)),
        ('"Foo Bar"', expr.String("Foo Bar")),
        ("[1]", expr.Aggregate(expr.Number(1))),
        ("[1, 2]", expr.Aggregate(expr.Number(1), expr.Number(2))),
        (
            '[137] & "PNG" & [13, 10, 26, 10]',
            expr.Aggregate(
                expr.Number(137),
                expr.Number(80),
                expr.Number(78),
                expr.Number(71),
                expr.Number(13),
                expr.Number(10),
                expr.Number(26),
                expr.Number(10),
            ),
        ),
        (
            "for all X in Y => X = Z",
            expr.ForAllIn(
                "X",
                expr.Variable("Y"),
                expr.Equal(expr.Variable("X"), expr.Variable("Z")),
            ),
        ),
        (
            "for some X in Y => X = Z",
            expr.ForSomeIn(
                "X",
                expr.Variable("Y"),
                expr.Equal(expr.Variable("X"), expr.Variable("Z")),
            ),
        ),
        (
            "[for X in Y => X.A]",
            expr.Comprehension(
                "X",
                expr.Variable("Y"),
                expr.Selected(expr.Variable("X"), "A"),
                expr.TRUE,
            ),
        ),
        (
            "[for X in Y if X.B = Z => X.A]",
            expr.Comprehension(
                "X",
                expr.Variable("Y"),
                expr.Selected(expr.Variable("X"), "A"),
                expr.Equal(expr.Selected(expr.Variable("X"), "B"), expr.Variable("Z")),
            ),
        ),
        (
            'X (A, "S", 42)',
            expr.Call("X", UNDEFINED, [expr.Variable("A"), expr.String("S"), expr.Number(42)]),
        ),
        ("X::Y (A)", expr.Conversion("X::Y", expr.Variable("A"))),
        ("X'(Y => Z)", expr.MessageAggregate("X", {ID("Y"): expr.Variable("Z")})),
        (
            "X'(Y => Z, A => B)",
            expr.MessageAggregate("X", {ID("Y"): expr.Variable("Z"), ID("A"): expr.Variable("B")}),
        ),
        ("X'(null message)", expr.MessageAggregate("X", {})),
        ("X", expr.Variable("X")),
        ("X in Y", expr.In(expr.Variable("X"), expr.Variable("Y"))),
        ("X not in Y", expr.NotIn(expr.Variable("X"), expr.Variable("Y"))),
        ("X < Y", expr.Less(expr.Variable("X"), expr.Variable("Y"))),
    ],
)
def test_expression_base(string: str, expected: expr.Expr) -> None:
    actual = parse_expression(string, lang.GrammarRule.extended_expression_rule)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X'Valid = True", expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("True"))),
        ("X::Y /= Z", expr.NotEqual(expr.Variable("X::Y"), expr.Variable("Z"))),
        ("X.Y /= Z", expr.NotEqual(expr.Selected(expr.Variable("X"), "Y"), expr.Variable("Z"))),
        (
            "X = Y and Y /= Z",
            expr.And(
                expr.Equal(expr.Variable("X"), expr.Variable("Y")),
                expr.NotEqual(expr.Variable("Y"), expr.Variable("Z")),
            ),
        ),
        (
            "X'Valid and Y'Valid",
            expr.And(expr.Valid(expr.Variable("X")), expr.Valid(expr.Variable("Y"))),
        ),
        (
            "X'Valid = True and (Y'Valid = False or Z'Valid = False)",
            expr.And(
                expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("True")),
                expr.Or(
                    expr.Equal(expr.Valid(expr.Variable("Y")), expr.Variable("False")),
                    expr.Equal(expr.Valid(expr.Variable("Z")), expr.Variable("False")),
                ),
            ),
        ),
        (
            "X'Valid = False or X.A /= A or X.B /= B or (X.C = 0 and X.D not in X.E)",
            expr.Or(
                expr.Or(
                    expr.Or(
                        expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("False")),
                        expr.NotEqual(expr.Selected(expr.Variable("X"), "A"), expr.Variable("A")),
                    ),
                    expr.NotEqual(expr.Selected(expr.Variable("X"), "B"), expr.Variable("B")),
                ),
                expr.And(
                    expr.Equal(expr.Selected(expr.Variable("X"), "C"), expr.Number(0)),
                    expr.NotIn(
                        expr.Selected(expr.Variable("X"), "D"),
                        expr.Selected(expr.Variable("X"), "E"),
                    ),
                ),
            ),
        ),
        (
            "for some A in X.B => (A.T = P.E and (G::E not in P::S (A.D).V))",
            expr.ForSomeIn(
                "A",
                expr.Selected(expr.Variable("X"), "B"),
                expr.And(
                    expr.Equal(
                        expr.Selected(expr.Variable("A"), "T"),
                        expr.Selected(expr.Variable("P"), "E"),
                    ),
                    expr.NotIn(
                        expr.Variable("G::E"),
                        expr.Selected(
                            expr.Conversion("P::S", expr.Selected(expr.Variable("A"), "D")),
                            "V",
                        ),
                    ),
                ),
            ),
        ),
        ("X::Y (Z) = 42", expr.Equal(expr.Conversion("X::Y", expr.Variable("Z")), expr.Number(42))),
        ("X (Y).Z", expr.Selected(expr.Call("X", UNDEFINED, [expr.Variable("Y")]), "Z")),
        (
            "X (Y).Z'Size",
            expr.Size(expr.Selected(expr.Call("X", UNDEFINED, [expr.Variable("Y")]), "Z")),
        ),
        (
            "G::E not in P::S (E.D).V",
            expr.NotIn(
                expr.Variable("G::E"),
                expr.Selected(expr.Conversion("P::S", expr.Selected(expr.Variable("E"), "D")), "V"),
            ),
        ),
        (
            "[for E in L if E.T = A => E.B]'Head",
            expr.Head(
                expr.Comprehension(
                    "E",
                    expr.Variable("L"),
                    expr.Selected(expr.Variable("E"), "B"),
                    expr.Equal(expr.Selected(expr.Variable("E"), "T"), expr.Variable("A")),
                ),
            ),
        ),
        ("A'Head.D", expr.Selected(expr.Head(expr.Variable("A")), "D")),
        (
            "[for E in L if E.T = A => E.B]'Head.D",
            expr.Selected(
                expr.Head(
                    expr.Comprehension(
                        "E",
                        expr.Variable("L"),
                        expr.Selected(expr.Variable("E"), "B"),
                        expr.Equal(expr.Selected(expr.Variable("E"), "T"), expr.Variable("A")),
                    ),
                ),
                "D",
            ),
        ),
        (
            "(for some S in P::X ([for E in C.A if E.T = P.L => E]'Head.D).H => S.G = G) = False",
            expr.Equal(
                expr.ForSomeIn(
                    "S",
                    expr.Selected(
                        expr.Conversion(
                            "P::X",
                            expr.Selected(
                                expr.Head(
                                    expr.Comprehension(
                                        "E",
                                        expr.Selected(expr.Variable("C"), "A"),
                                        expr.Variable("E"),
                                        expr.Equal(
                                            expr.Selected(expr.Variable("E"), "T"),
                                            expr.Selected(expr.Variable("P"), "L"),
                                        ),
                                    ),
                                ),
                                "D",
                            ),
                        ),
                        "H",
                    ),
                    expr.Equal(expr.Selected(expr.Variable("S"), "G"), expr.Variable("G")),
                ),
                expr.Variable("False"),
            ),
        ),
        (
            "(case C is when V1 | V2 => 8, when V3 => 16)",
            expr.CaseExpr(
                expr.Variable("C"),
                [
                    ([ID("V1"), ID("V2")], expr.Number(8)),
                    ([ID("V3")], expr.Number(16)),
                ],
            ),
        ),
    ],
)
def test_expression_complex(string: str, expected: expr.Expr) -> None:
    actual = parse_bool_expression(string, extended=True)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("X : Channel with Readable", decl.ChannelDeclaration("X", readable=True)),
        ("X : Channel with Writable", decl.ChannelDeclaration("X", writable=True)),
        (
            "X : Channel with Readable, Writable",
            decl.ChannelDeclaration("X", readable=True, writable=True),
        ),
        ("with function X return Y", decl.FunctionDeclaration("X", [], "Package::Y")),
        ("with function X return Package::Y", decl.FunctionDeclaration("X", [], "Package::Y")),
        (
            "with function X (A : B; C : D) return Y",
            decl.FunctionDeclaration(
                "X",
                [decl.Argument("A", "Package::B"), decl.Argument("C", "Package::D")],
                "Package::Y",
            ),
        ),
        (
            "with function X (A : Boolean) return Boolean",
            decl.FunctionDeclaration(
                "X",
                [decl.Argument("A", str(model.BOOLEAN.identifier))],
                str(model.BOOLEAN.identifier),
            ),
        ),
    ],
)
def test_formal_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_formal_declaration(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("A : B", decl.VariableDeclaration("A", "Package::B")),
        ("A : B := C", decl.VariableDeclaration("A", "Package::B", expr.Variable("C"))),
        ("A : B := 1", decl.VariableDeclaration("A", "Package::B", expr.Number(1))),
    ],
)
def test_variable_declaration(string: str, expected: decl.Declaration) -> None:
    actual = parse_declaration(string)
    assert actual == expected
    assert actual.location


def test_renaming_declaration() -> None:
    expected = decl.RenamingDeclaration("A", "Package::B", expr.Selected(expr.Variable("C"), "D"))
    actual = parse_declaration("A : B renames C.D")
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("A := B", stmt.VariableAssignment("A", expr.Variable("B"))),
        ("A.B := C", stmt.MessageFieldAssignment("A", "B", expr.Variable("C"))),
    ],
)
def test_assignment_statement(string: str, expected: stmt.Statement) -> None:
    actual = parse_statement(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        ("A'Append (B)", stmt.Append("A", expr.Variable("B"))),
        ("A'Extend (B)", stmt.Extend("A", expr.Variable("B"))),
        ("A'Read (B)", stmt.Read("A", expr.Variable("B"))),
        ("A'Write (B)", stmt.Write("A", expr.Variable("B"))),
        ("A'Reset", stmt.Reset("A")),
        (
            "A'Reset (B => 1, C => 2)",
            stmt.Reset("A", {ID("B"): expr.Number(1), ID("C"): expr.Number(2)}),
        ),
    ],
)
def test_attribute_statement(string: str, expected: stmt.Statement) -> None:
    actual = parse_statement(string)
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    ("string", "expected"),
    [
        (
            """
               state A is
               begin
               transition
                  goto B
               end A
         """,
            model.State("A", transitions=[model.Transition("B")]),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
                     if X = Y
                  goto C
               end A
         """,
            model.State(
                "A",
                transitions=[
                    model.Transition("B", expr.Equal(expr.Variable("X"), expr.Variable("Y"))),
                    model.Transition("C"),
                ],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
                     with Desc => "rfc2549.txt+12:3-45:6"
                     if X = Y
                  goto C
                     with Desc => "rfc2549.txt+123:45-678:9"
               end A
         """,
            model.State(
                "A",
                transitions=[
                    model.Transition(
                        "B",
                        expr.Equal(expr.Variable("X"), expr.Variable("Y")),
                        description="rfc2549.txt+12:3-45:6",
                    ),
                    model.Transition("C", description="rfc2549.txt+123:45-678:9"),
                ],
            ),
        ),
        (
            """
               state A is
                  Z : Boolean := Y;
               begin
               transition
                  goto B
               end A
         """,
            model.State(
                "A",
                transitions=[model.Transition("B")],
                declarations=[
                    decl.VariableDeclaration("Z", "__BUILTINS__::Boolean", expr.Variable("Y")),
                ],
                actions=[],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
               exception
                  goto C
               end A
         """,
            model.State(
                "A",
                transitions=[model.Transition("B")],
                exception_transition=model.Transition("C"),
                declarations=[],
                actions=[],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  goto B
               exception
                  goto C
                     with Desc => "rfc2549.txt+12:3-45:6"
               end A
         """,
            model.State(
                "A",
                transitions=[model.Transition("B")],
                exception_transition=model.Transition("C", description="rfc2549.txt+12:3-45:6"),
                declarations=[],
                actions=[],
            ),
        ),
    ],
    ids=range(1, 7),
)
def test_state(string: str, expected: decl.Declaration) -> None:
    actual = parse_state(string)
    assert actual == expected
    assert actual.location


def test_state_error() -> None:
    string = """
       state A is
       begin
       transition
          goto B
       end C
    """
    error = (
        r'<stdin>:6:12: error: inconsistent state identifier "C"\n'
        r'<stdin>:2:14: note: previous identifier was "A"'
    )
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_state(string)


def test_state_machine_declaration() -> None:
    string = """
           generic
              X : Channel with Readable, Writable;
              with function F return Boolean;
           machine S is
              Y : Boolean := True;
           begin
              state A is
                 Z : Boolean := Y;
              begin
                 Z := False;
              transition
                 goto null
                    if Z = False
                 goto A
              end A;
           end S
    """
    actual = parse_state_machine(string)
    expected = model.UncheckedStateMachine(
        ID("Package::S"),
        [
            model.State(
                "A",
                declarations=[
                    decl.VariableDeclaration("Z", BOOLEAN.identifier, expr.Variable("Y")),
                ],
                actions=[stmt.VariableAssignment("Z", expr.FALSE)],
                transitions=[
                    model.Transition(
                        "null",
                        condition=expr.Equal(expr.Variable("Z"), expr.FALSE),
                    ),
                    model.Transition("A"),
                ],
            ),
        ],
        [decl.VariableDeclaration("Y", BOOLEAN.identifier, expr.TRUE)],
        [
            decl.ChannelDeclaration("X", readable=True, writable=True),
            decl.FunctionDeclaration("F", [], BOOLEAN.identifier),
        ],
        location=Location((2, 12), common.STDIN, (17, 17)),
    )
    assert actual == expected
    assert actual.location


def test_parse_state_machine() -> None:
    string = """
       generic
       machine X is
       begin
          state A is
          begin
          transition
             goto null
          end A;
       end X
    """
    parse_state_machine_error(string)


@pytest.mark.parametrize(
    ("string", "error"),
    [
        (
            """
               generic
               machine X is
               begin
                  state A is
                  begin
                  transition
                     goto null
                  end A;
               end Y
         """,
            r'<stdin>:10:20: error: inconsistent state machine identifier "Y"\n'
            r'<stdin>:3:24: note: previous identifier was "X"',
        ),
        (
            """
               generic
               machine X is
               begin
                  state A is
                  begin
                  transition
                     goto null
                  end A
               end Y
         """,
            "<stdin>:10:16: error: Expected ';', got 'end'",
        ),
    ],
)
def test_state_machine_error(string: str, error: str) -> None:
    with pytest.raises(RecordFluxError, match=rf"^{error}$"):
        parse_state_machine_error(string)


def test_state_machine() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is

           generic
           machine S is
           begin
              state A is
              begin
              transition
                 goto B
              end A;

              state B is
              begin
              transition
                 goto null
              end B;
           end S;

        end Test;
        """,
    )
    p.create_model()


def test_parse_error_deprecated_session_keyword() -> None:
    assert_error_string(
        """\
        package Test is

           generic
           session S is
           begin
              state A is
              begin
              transition
                 goto null
              end A;
           end S;

        end Test;
        """,
        r"^"
        r"<stdin>:4:4: error: \"session\" keyword is deprecated\n"
        r'<stdin>:4:4: help: use "machine" instead'
        r"$",
    )


def test_expression_aggregate_no_number() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(r"^<stdin>:1:5: error: Expected Numeral, got 'First'$"),
    ):
        parse_expression("[1, Foo]", lang.GrammarRule.expression_rule)
