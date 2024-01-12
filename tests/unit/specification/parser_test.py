from __future__ import annotations

import textwrap
from collections.abc import Mapping, Sequence
from itertools import zip_longest
from pathlib import Path
from typing import Any

import pytest

from rflx import expression as expr, model
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.identifier import ID
from rflx.model import BOOLEAN, FINAL, INITIAL, OPAQUE, Enumeration, Field, Integer, Link, Message
from rflx.model.message import ByteOrder
from rflx.specification import parser
from tests.const import SPEC_DIR
from tests.data import models
from tests.utils import check_regex

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
    assert result == expected, filenames


def assert_ast_string(string: str, expected: Mapping[str, Any]) -> None:  # type: ignore[misc]
    p = parser.Parser()
    p.parse_string(string)
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
    fail("TEST", Subsystem.PARSER, Severity.ERROR)


# This test must fail to get full coverage of assert_error_files.
@pytest.mark.xfail()
def test_assert_error_files() -> None:
    assert_error_files([], "^ parser: error: $")


def test_create_model() -> None:
    p = parser.Parser()
    p.parse(SPEC_DIR / "tlv.rflx")
    p.create_model()


def test_create_model_cached() -> None:
    p = parser.Parser(cached=True)
    p.parse(SPEC_DIR / "tlv.rflx")
    p.create_model()


def test_parse_string_error() -> None:
    p = parser.Parser()
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'a/b.rflx:1:9: parser: error: file name does not match unit name "A",'
            r' should be "a.rflx"\n'
            r"a/b.rflx:2:0: style: error: unexpected keyword indentation \(expected 3 or 6\)"
            r" \[indentation\]"
            r"$"
        ),
    ):
        p.parse_string(
            "package A is\ntype T is range 0 .. 0 with Size => 1;\nend A;",
            filename=Path("a/b.rflx"),
        )


def test_parse_string_empty() -> None:
    p = parser.Parser()
    p.parse_string("")


def test_parse_string_empty_file() -> None:
    p = parser.Parser()
    p.parse_string("\n")


@pytest.mark.parametrize("spec", ["empty_file", "comment_only"])
def test_parse_empty_specfication(spec: str) -> None:
    assert_ast_files([f"{SPEC_DIR}/{spec}.rflx"], {})


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
                        "item": {"_kind": "UnqualifiedID", "_value": "Empty_File"},
                    },
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
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Foo",
                                        },
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
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Bar",
                                        },
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
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Baz",
                                        },
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
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Bar",
                                        },
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
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Baz",
                                        },
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
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Simple_PDU"},
                        "parameters": None,
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "NullMessageTypeDef",
                            "_value": "null message",
                        },
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
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Foo",
                                        },
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
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Bar",
                                        },
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
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Baz",
                                        },
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
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Bar",
                                        },
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
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Baz",
                                        },
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
                                    "name": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                    "package": None,
                                },
                            },
                            "op": {"_kind": "OpEq", "_value": "="},
                            "right": {"_kind": "NumericLiteral", "_value": "42"},
                        },
                        "field": {"_kind": "UnqualifiedID", "_value": "Bar"},
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
                        "field": {"_kind": "UnqualifiedID", "_value": "Bar"},
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
        r'^<stdin>:1:9: parser: error: illegal prefix "RFLX" in package identifier "RFLX_Types"$',
    )


def test_parse_error_inconsistent_package_identifiers() -> None:
    assert_error_string(
        """\
        package A is
        end B;
        """,
        r'^<stdin>:2:5: parser: error: inconsistent package identifier "B"\n'
        r'<stdin>:1:9: parser: info: previous identifier was "A"$',
    )


def test_parse_error_incorrect_name() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/incorrect_name.rflx"],
        f"^{SPEC_DIR}/incorrect_name.rflx:1:9: parser: error: file name does not match unit name"
        r' "Test", should be "test.rflx"$',
    )


def test_parse_error_incorrect_specification() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/incorrect_specification.rflx"],
        f"^{SPEC_DIR}/incorrect_specification.rflx:3:10: parser: error: Expected 'is', got ';'$",
    )


def test_parse_error_unexpected_exception_in_parser(monkeypatch: pytest.MonkeyPatch) -> None:
    p = parser.Parser()
    monkeypatch.setattr(parser, "check_naming", lambda _x, _e, _o: raise_parser_error())
    with pytest.raises(RecordFluxError, match=r"^parser: error: TEST$"):
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
            f"{SPEC_DIR}/context_cycle.rflx:1:6: parser: error: dependency cycle when "
            f'including "Context_Cycle_1"\n'
            f'{SPEC_DIR}/context_cycle_1.rflx:1:6: parser: info: when including "Context_Cycle_2"\n'
            f'{SPEC_DIR}/context_cycle_2.rflx:1:6: parser: info: when including "Context_Cycle_3"\n'
            f'{SPEC_DIR}/context_cycle_3.rflx:1:6: parser: info: when including "Context_Cycle_1"'
            r"$"
        ),
    ):
        p.parse(Path(f"{SPEC_DIR}/context_cycle.rflx"))

    p.parse(Path(f"{SPEC_DIR}/integer_type.rflx"))


def test_parse_error_context_dependency_cycle_2() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/context_cycle_1.rflx"],
        f"^"
        f"{SPEC_DIR}/context_cycle_1.rflx:1:6: parser: error: dependency cycle when "
        f'including "Context_Cycle_2"\n'
        f'{SPEC_DIR}/context_cycle_2.rflx:1:6: parser: info: when including "Context_Cycle_3"\n'
        f'{SPEC_DIR}/context_cycle_3.rflx:1:6: parser: info: when including "Context_Cycle_1"'
        f"$",
    )


def test_parse_string_error_context_dependency_cycle() -> None:
    p = parser.Parser()
    p.parse_string(Path(f"{SPEC_DIR}/context_cycle.rflx").read_text())
    p.parse_string(Path(f"{SPEC_DIR}/context_cycle_1.rflx").read_text())
    p.parse_string(Path(f"{SPEC_DIR}/context_cycle_2.rflx").read_text())

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:6: parser: error: dependency cycle when including "Context_Cycle_1"\n'
            r'<stdin>:1:6: parser: info: when including "Context_Cycle_2"\n'
            r'<stdin>:1:6: parser: info: when including "Context_Cycle_3"'
            r"$"
        ),
    ):
        p.parse_string(Path(f"{SPEC_DIR}/context_cycle_3.rflx").read_text())

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
        r'^<stdin>:6:18: parser: error: undefined field "Bar"$',
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
        r'^<stdin>:7:21: parser: error: invalid aspect "Foo"$',
    )


def test_parse_error_sequence_undefined_type() -> None:
    assert_error_string(
        """\
        package Test is
           type T is sequence of Foo;
        end Test;
        """,
        r'^<stdin>:2:26: model: error: undefined element type "Test::Foo"$',
    )


def test_parse_error_refinement_undefined_message() -> None:
    assert_error_string(
        """\
        package Test is
           for PDU use (Foo => Bar);
        end Test;
        """,
        r'^<stdin>:2:8: model: error: undefined type "Test::PDU" in refinement\n'
        r'<stdin>:2:24: model: error: undefined type "Test::Bar" in refinement of "Test::PDU"$',
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
        r'^<stdin>:7:24: model: error: undefined type "Test::Bar" in refinement of "Test::PDU"$',
    )


def test_parse_error_derivation_undefined_type() -> None:
    assert_error_string(
        """\
        package Test is
           type Bar is new Foo;
        end Test;
        """,
        r'^<stdin>:2:20: model: error: undefined base message "Test::Foo" in derived message$',
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
        r'^<stdin>:3:9: model: error: illegal derivation "Test::Bar"\n'
        r'<stdin>:2:9: model: info: invalid base message type "Test::Foo"'
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
        r"^<stdin>:6:21: parser: error: Expected ';', got ','$",
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
        r"^<stdin>:8:13: parser: error: Expected ':', got 'then'$",
    )


@pytest.mark.parametrize("keyword", ADA_KEYWORDS)
def test_parse_error_reserved_word_in_type_name(keyword: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           type {keyword.title()} is range 0 .. 255 with Size => 8;
        end Test;
        """,
        rf'^<stdin>:2:9: parser: error: reserved word "{keyword.title()}" used as identifier$',
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
        rf'^<stdin>:5:10: parser: error: reserved word "{keyword.title()}" used as identifier$',
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
        r'<stdin>:4:10: parser: error: reserved word "End" used as identifier\n'
        r'<stdin>:6:17: parser: error: reserved word "End" used as identifier'
        r"$",
    )


def test_parse_error_reserved_word_in_session_identifier() -> None:
    assert_error_string(
        """\
        package Test is
           generic
           session End is
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
        r'^<stdin>:3:12: parser: error: reserved word "End" used as identifier$',
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
        rf'<stdin>:2:9: parser: error: illegal identifier "{identifier}"\n'
        r'<stdin>:2:9: parser: info: identifiers starting with "RFLX_"'
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
        rf'<stdin>:5:10: parser: error: illegal identifier "{identifier}"\n'
        r'<stdin>:5:10: parser: info: identifiers starting with "RFLX_"'
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
        rf'<stdin>:4:10: parser: error: illegal identifier "{identifier}"\n'
        r'<stdin>:4:10: parser: info: identifiers starting with "RFLX_"'
        r" are reserved for internal use\n"
        rf'<stdin>:6:17: parser: error: illegal identifier "{identifier}"\n'
        r'<stdin>:6:17: parser: info: identifiers starting with "RFLX_"'
        r" are reserved for internal use"
        r"$",
    )


@pytest.mark.parametrize("identifier", ILLEGAL_IDENTIFIERS)
def test_parse_error_illegal_identifier_in_session_identifier(identifier: str) -> None:
    assert_error_string(
        f"""\
        package Test is
           generic
           session {identifier} is
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
        rf'<stdin>:3:12: parser: error: illegal identifier "{identifier}"\n'
        r'<stdin>:3:12: parser: info: identifiers starting with "RFLX_"'
        r" are reserved for internal use"
        r"$",
    )


def test_parse_error_invalid_context_clause(tmp_path: Path) -> None:
    test_file = tmp_path / "test.rflx"

    test_file.write_text("with invalid", encoding="utf-8")

    p = parser.Parser()

    with pytest.raises(
        RecordFluxError,
        match=rf"^{test_file}:1:13: parser: error: Expected ';', got Termination$",
    ):
        p.parse(test_file)


@pytest.mark.parametrize("spec", ["empty_file", "comment_only", "empty_package", "context"])
def test_create_model_no_messages(spec: str) -> None:
    assert_messages_files([f"{SPEC_DIR}/{spec}.rflx"], [])


def test_create_model_message_type_message() -> None:
    simple_structure = [
        model.Link(model.INITIAL, model.Field("Bar")),
        model.Link(model.Field("Bar"), model.Field("Baz")),
        model.Link(model.Field("Baz"), model.FINAL),
    ]

    simple_types = {
        model.Field("Bar"): model.Integer(
            "Message_Type::T",
            expr.Number(0),
            expr.Number(255),
            expr.Number(8),
        ),
        model.Field("Baz"): model.Integer(
            "Message_Type::T",
            expr.Number(0),
            expr.Number(255),
            expr.Number(8),
        ),
    }

    simple_message = model.Message("Message_Type::Simple_PDU", simple_structure, simple_types)

    structure = [
        model.Link(model.INITIAL, model.Field("Foo")),
        model.Link(
            model.Field("Foo"),
            model.Field("Bar"),
            expr.LessEqual(expr.Variable("Foo"), expr.Number(30, 16)),
        ),
        model.Link(
            model.Field("Foo"),
            model.Field("Baz"),
            expr.Greater(expr.Variable("Foo"), expr.Number(30, 16)),
        ),
        model.Link(model.Field("Bar"), model.Field("Baz")),
        model.Link(model.Field("Baz"), model.FINAL),
    ]

    types = {
        **simple_types,
        model.Field("Foo"): model.Integer(
            "Message_Type::T",
            expr.Number(0),
            expr.Number(255),
            expr.Number(8),
        ),
    }

    message = model.Message("Message_Type::PDU", structure, types)

    empty_message = model.Message("Message_Type::Empty_PDU", [], {})

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
        "Message_In_Message::Length_Value",
        [
            model.Link(model.INITIAL, model.Field("Length")),
            model.Link(
                model.Field("Length"),
                model.Field("Value"),
                size=expr.Mul(expr.Number(8), expr.Variable("Length")),
            ),
            model.Link(model.Field("Value"), model.FINAL),
        ],
        {model.Field("Length"): length, model.Field("Value"): model.Opaque()},
    )

    derived_length_value = model.DerivedMessage(
        "Message_In_Message::Derived_Length_Value",
        length_value,
    )

    message = model.Message(
        "Message_In_Message::Message",
        [
            model.Link(model.INITIAL, model.Field("Foo_Length")),
            model.Link(model.Field("Foo_Value"), model.Field("Bar_Length")),
            model.Link(model.Field("Bar_Value"), model.FINAL),
            model.Link(
                model.Field("Foo_Length"),
                model.Field("Foo_Value"),
                size=expr.Mul(expr.Variable("Foo_Length"), expr.Number(8)),
            ),
            model.Link(
                model.Field("Bar_Length"),
                model.Field("Bar_Value"),
                size=expr.Mul(expr.Variable("Bar_Length"), expr.Number(8)),
            ),
        ],
        {
            model.Field("Foo_Length"): length,
            model.Field("Foo_Value"): model.Opaque(),
            model.Field("Bar_Length"): length,
            model.Field("Bar_Value"): model.Opaque(),
        },
    )

    derived_message = model.DerivedMessage("Message_In_Message::Derived_Message", message)

    assert_messages_files(
        [f"{SPEC_DIR}/message_in_message.rflx"],
        [length_value, derived_length_value, message, derived_message],
    )


def test_create_model_ethernet_frame() -> None:
    assert_messages_files([f"{SPEC_DIR}/ethernet.rflx"], [models.ethernet_frame()])


def test_create_model_type_derivation_message() -> None:
    t = model.Integer("Test::T", expr.Number(0), expr.Number(255), expr.Number(8))

    structure = [
        model.Link(model.INITIAL, model.Field("Baz")),
        model.Link(model.Field("Baz"), model.FINAL),
    ]

    types = {model.Field("Baz"): t}

    message_foo = model.Message("Test::Foo", structure, types)
    message_bar = model.DerivedMessage("Test::Bar", message_foo)

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
        "Test::Foo",
        [
            model.Link(model.INITIAL, model.Field("Baz"), size=expr.Number(48)),
            model.Link(model.Field("Baz"), model.FINAL),
        ],
        {model.Field("Baz"): model.Opaque()},
    )
    message_bar = model.DerivedMessage("Test::Bar", message_foo)

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


def test_create_model_session_locations() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is
           generic
              with function F return Boolean;
           session Session is
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
              end A;
           end Session;
        end Test;
        """,
    )
    m = p.create_model()
    assert [p.location for p in m.sessions[0].parameters] == [
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
                "Test::M",
                [
                    Link(INITIAL, Field("A")),
                    Link(
                        Field("A"),
                        Field("B"),
                        condition=expr.And(
                            expr.Greater(expr.Variable("A"), expr.Number(10)),
                            expr.Less(expr.Variable("A"), expr.Number(100)),
                        ),
                    ),
                    Link(Field("B"), FINAL),
                ],
                {Field("A"): T, Field("B"): T},
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
                "Test::M",
                [
                    Link(INITIAL, Field("A")),
                    Link(Field("A"), Field("B"), first=expr.First("A")),
                    Link(Field("B"), FINAL),
                ],
                {Field("A"): T, Field("B"): T},
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
                "Test::M",
                [
                    Link(INITIAL, Field("A")),
                    Link(Field("A"), Field("B"), size=expr.Number(8)),
                    Link(Field("B"), FINAL),
                ],
                {Field("A"): T, Field("B"): OPAQUE},
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
                "Test::M",
                [
                    Link(INITIAL, Field("A")),
                    Link(
                        Field("A"),
                        Field("B"),
                        first=expr.First("A"),
                        size=expr.Number(8),
                        condition=expr.And(
                            expr.Greater(expr.Variable("A"), expr.Number(10)),
                            expr.Less(expr.Variable("A"), expr.Number(100)),
                        ),
                    ),
                    Link(Field("B"), FINAL),
                ],
                {Field("A"): T, Field("B"): OPAQUE},
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
                "Test::M",
                [
                    Link(INITIAL, Field("F"), size=expr.Mul(expr.Variable("P"), expr.Number(8))),
                    Link(
                        Field("F"),
                        FINAL,
                        condition=expr.Less(expr.Variable("P"), expr.Number(100)),
                    ),
                ],
                {Field("P"): T, Field("F"): OPAQUE},
            ),
            Message(
                "Test::M_S",
                [
                    Link(INITIAL, Field("F_F"), size=expr.Mul(expr.Number(16), expr.Number(8))),
                    Link(Field("F_F"), FINAL, condition=expr.TRUE),
                ],
                {Field("F_F"): OPAQUE},
            ),
            Message(
                "Test::M_D",
                [
                    Link(INITIAL, Field("L")),
                    Link(
                        Field("L"),
                        Field("F_F"),
                        size=expr.Mul(expr.Variable("L"), expr.Number(8)),
                    ),
                    Link(
                        Field("F_F"),
                        FINAL,
                        condition=expr.Less(expr.Variable("L"), expr.Number(100)),
                    ),
                ],
                {Field("L"): T, Field("F_F"): OPAQUE},
            ),
        ],
    )


@pytest.mark.parametrize(
    ("parameters", "error"),
    [
        (
            "",
            r"^"
            r"<stdin>:15:14: model: error: missing argument\n"
            r'<stdin>:5:14: model: info: expected argument for parameter "P"'
            r"$",
        ),
        (
            " (Q => 16)",
            r"^"
            r'<stdin>:15:19: model: error: unexpected argument "Q"\n'
            r'<stdin>:15:19: model: info: expected argument for parameter "P"'
            r"$",
        ),
        (
            " (P => 16, Q => 16)",
            r"^"
            r'<stdin>:15:28: model: error: unexpected argument "Q"\n'
            r"<stdin>:15:28: model: info: expected no argument"
            r"$",
        ),
        (
            " (Q => 16, P => 16)",
            r"^"
            r'<stdin>:15:19: model: error: unexpected argument "Q"\n'
            r'<stdin>:15:19: model: info: expected argument for parameter "P"\n'
            r'<stdin>:15:28: model: error: unexpected argument "P"\n'
            r"<stdin>:15:28: model: info: expected no argument"
            r"$",
        ),
        (
            " (P => 16, P => 16)",
            r"^"
            r'<stdin>:15:28: model: error: unexpected argument "P"\n'
            r"<stdin>:15:28: model: info: expected no argument"
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
        r'^<stdin>:2:14: parser: error: invalid aspect "Invalid" for range type "Test::T"$',
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
            r'^<stdin>:2:9: parser: error: no size set for "Test::T"$',
        ),
        (
            """\
        package Test is
           type T is (A, B) with Always_Valid => Invalid;
        end Test;
        """,
            r"^"
            "<stdin>:2:42: parser: error: invalid Always_Valid expression: Invalid\n"
            '<stdin>:2:9: parser: error: no size set for "Test::T"'
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
        r"^<stdin>:7:12: parser: error: only message types can be parameterized$",
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
        r'^<stdin>:3:16: model: error: undefined type "Test::X"$',
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
        r'<stdin>:3:19: model: error: name conflict for "P"\n'
        r"<stdin>:3:12: model: info: conflicting name"
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
        r'<stdin>:5:10: model: error: name conflict for "P"\n'
        r"<stdin>:3:12: model: info: conflicting name"
        r"$",
    )


def test_parse_error_duplicate_spec_file() -> None:
    p = parser.Parser()
    p.parse(SPEC_DIR / "message_type.rflx")
    with pytest.raises(
        RecordFluxError,
        match=(
            "^tests/data/specs/subdir/message_type.rflx:1:9: parser: error: duplicate"
            " specification\n"
            "tests/data/specs/message_type.rflx:1:9: parser: info: previous specification$"
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
            "^tests/data/specs/subdir/message_type.rflx:1:9: parser: error: duplicate"
            " specification\n"
            "<stdin>:1:9: parser: info: previous specification$"
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
           session Session is
           begin
              state A
              is
                 Avail : Boolean := Channel'Has_Data;
              begin
              transition
                 goto A if Avail
                 goto null
              end A;
           end Session;
        end Test;
        """,
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
        r'^<stdin>:7:52: parser: error: duplicate aspect "First"\n'
        "<stdin>:7:18: parser: info: previous location\n"
        '<stdin>:7:70: parser: error: duplicate aspect "Size"\n'
        "<stdin>:7:36: parser: info: previous location$",
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
           session S is
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
        r'^<stdin>:8:44: parser: error: duplicate aspect "Readable"\n'
        "<stdin>:8:24: parser: info: previous location$",
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
           session S is
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
        r"^<stdin>:15:19: parser: error: bindings are not supported$",
    )


def test_parse_error_unsupported_modular_integer_type() -> None:
    assert_error_string(
        """\
        package Test is
           type T is mod 2 ** 16;
        end Test;
        """,
        r"^"
        r"<stdin>:2:9: parser: error: modular integer types are not supported\n"
        r'<stdin>:2:9: parser: info: use "type T is range 0 .. 65535 with Size => 16" instead'
        r"$",
    )


def test_parse_error_duplicate_integer_size_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 1 .. 10 with Size => 8, Size => 16;
        end Test;
        """,
        r"^<stdin>:2:42: parser: error: Expected ';', got ','$",
    )


def test_parse_error_duplicate_enumeration_aspect() -> None:
    assert_error_string(
        """\
        package Test is
           type E is (L1, L2, L3) with Size => 8, Always_Valid, Size => 16, Always_Valid;
        end Test;
        """,
        r'^<stdin>:2:57: parser: error: duplicate aspect "Size"\n'
        r"<stdin>:2:32: parser: info: previous location\n"
        r'<stdin>:2:69: parser: error: duplicate aspect "Always_Valid"\n'
        r"<stdin>:2:43: parser: info: previous location$",
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
        r"^<stdin>:9:15: parser: error: Expected 'Byte_Order', got 'First'$",
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
        r'^<stdin>:9:15: parser: error: duplicate aspect "Byte_Order"\n'
        "<stdin>:8:15: parser: info: previous location$",
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
           session S is
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
        r"^<stdin>:13:27: parser: error: Expected 'is', got ','$",
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
           session S is
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
        r"^<stdin>:18:30: parser: error: Expected 'end', got ','$",
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
        match=(
            r"^"
            rf'{invalid}:1:1: parser: error: End of input expected, got "Unqualified_Identifier"'
            r"$"
        ),
    ):
        parser.Parser().parse(b, a)


def test_parse_non_existent_dependencies(tmp_path: Path) -> None:
    a = tmp_path / "a" / "a.rflx"
    a.parent.mkdir()
    a.write_text("with C; package A is end A;")

    b = tmp_path / "b" / "b.rflx"
    b.parent.mkdir()
    b.write_text("package B is end B;")

    error = rf'^{a}:1:6: parser: error: cannot find specification "C"$'

    with pytest.raises(RecordFluxError, match=error):
        parser.Parser().parse(a, b)

    with pytest.raises(RecordFluxError, match=error):
        parser.Parser().parse(b, a)


@pytest.mark.parametrize(
    ("expression", "message"),
    [
        ("A + 1", r"^<stdin>:7:19: parser: error: math expression in boolean context$"),
        ("42", r"^<stdin>:7:19: parser: error: math expression in boolean context$"),
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
        ("True", r"^<stdin>:5:26: parser: error: boolean expression in math context$"),
        ("3 < 4", r"^<stdin>:5:26: parser: error: boolean expression in math context$"),
        ("A = B", r"^<stdin>:5:26: parser: error: boolean expression in math context$"),
        ("A /= False", r"^<stdin>:5:26: parser: error: boolean expression in math context$"),
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
        r"^<stdin>:6:16: parser: error: short form condition is not supported anymore\n"
        r'<stdin>:6:16: parser: info: add condition to all outgoing links of field "A" instead$',
    )


def test_parse_only() -> None:
    p = parser.Parser()
    p.parse(*(SPEC_DIR / "parse_only").glob("*"))
