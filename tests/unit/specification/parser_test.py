# pylint: disable=too-many-lines

from itertools import zip_longest
from pathlib import Path
from typing import Any, Dict, Sequence

import pytest

from rflx import expression as expr, model
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.identifier import ID
from rflx.model import FINAL, INITIAL, OPAQUE, Field, Link, Message, Model, ModularInteger
from rflx.specification import cache, parser
from tests.const import EX_SPEC_DIR, SPEC_DIR
from tests.data import models

T = ModularInteger("Test::T", expr.Number(256))


def to_dict(node: Any) -> Any:
    if node is None:
        return None
    if node.is_list_type:
        return [to_dict(e) for e in node.children]
    result = {name[2:]: to_dict(getattr(node, name)) for name in dir(node) if name.startswith("f_")}
    if result:
        result["_kind"] = node.kind_name
        return result
    return {"_kind": node.kind_name, "_value": node.text}


def assert_ast_files(filenames: Sequence[str], expected: Dict[str, Any]) -> None:
    p = parser.Parser()
    for filename in filenames:
        p.parse(Path(filename))
    result = {f: to_dict(s) for f, s in p.specifications.items()}
    assert result == expected, filenames


def assert_model_string(string: str, model: Model) -> None:
    p = parser.Parser()
    p.parse_string(string)
    assert p.create_model() == model


def assert_error_files(filenames: Sequence[str], regex: str) -> None:
    assert " parser: error: " in regex
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
        for filename in filenames:
            p.parse(Path(filename))
        p.create_model()


def assert_error_string(string: str, regex: str) -> None:
    assert " parser: error: " in regex
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
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
    actual_messages: Sequence[model.Message], expected_messages: Sequence[model.Message]
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


def test_create_model() -> None:
    p = parser.Parser()
    p.parse(SPEC_DIR / "tlv.rflx")
    p.create_model()


def test_create_model_cached() -> None:
    p = parser.Parser(cached=True)
    p.parse(SPEC_DIR / "tlv.rflx")
    p.create_model()


def test_create_proven_message(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache()
    assert parser.create_proven_message(models.VALID_MESSAGE, False, c)
    assert c.is_verified(models.VALID_MESSAGE)


def test_create_proven_message_error(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache()
    with pytest.raises(RecordFluxError):
        parser.create_proven_message(models.INVALID_MESSAGE, False, c)
    assert not c.is_verified(models.INVALID_MESSAGE)


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
            }
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
            }
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
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "ModularTypeDef",
                            "mod": {"_kind": "NumericLiteral", "_value": "256"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Byte"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "ModularTypeDef",
                            "mod": {"_kind": "NumericLiteral", "_value": "64"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Hash_Index"},
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Integer_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Integer_Type"},
            },
        }
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
                                }
                            ],
                            "elements": {
                                "_kind": "NamedEnumerationDef",
                                "elements": [
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Mon"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "1"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Tue"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "2"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Wed"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "3"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Thu"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "4"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Fri"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "5"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Sat"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "6"},
                                    },
                                    {
                                        "_kind": "ElementValueAssoc",
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Sun"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "7"},
                                    },
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Day"},
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
                                            "name": {"_kind": "UnqualifiedID", "_value": "False"},
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
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Low"},
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
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "High"},
                                        "literal": {"_kind": "NumericLiteral", "_value": "7"},
                                    },
                                ],
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Priority"},
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Enumeration_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Enumeration_Type"},
            },
        }
    }
    assert_ast_files([f"{SPEC_DIR}/enumeration_type.rflx"], spec)


def test_parse_array_type_spec() -> None:
    spec = {
        "Array_Type": {
            "_kind": "Specification",
            "context_clause": [],
            "package_declaration": {
                "_kind": "PackageNode",
                "declarations": [
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "ModularTypeDef",
                            "mod": {"_kind": "NumericLiteral", "_value": "256"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Byte"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "ArrayTypeDef",
                            "element_type": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Byte"},
                                "package": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bytes"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "checksums": None,
                            "components": {
                                "_kind": "Components",
                                "components": [
                                    {
                                        "_kind": "Component",
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
                                                            "op": {"_kind": "OpMul", "_value": "*"},
                                                            "right": {
                                                                "_kind": "NumericLiteral",
                                                                "_value": "8",
                                                            },
                                                        },
                                                    }
                                                ],
                                                "condition": None,
                                                "target": {
                                                    "_kind": "UnqualifiedID",
                                                    "_value": "Bytes",
                                                },
                                            }
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Byte"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bytes"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Bytes"},
                                            "package": None,
                                        },
                                    },
                                ],
                                "initial_component": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Foo"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "ArrayTypeDef",
                            "element_type": {
                                "_kind": "ID",
                                "name": {"_kind": "UnqualifiedID", "_value": "Foo"},
                                "package": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Array_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Array_Type"},
            },
        }
    }
    assert_ast_files([f"{SPEC_DIR}/array_type.rflx"], spec)


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
                            "_kind": "ModularTypeDef",
                            "mod": {"_kind": "NumericLiteral", "_value": "256"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "checksums": None,
                            "components": {
                                "_kind": "Components",
                                "components": [
                                    {
                                        "_kind": "Component",
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
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                ],
                                "initial_component": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "PDU"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "checksums": None,
                            "components": {
                                "_kind": "Components",
                                "components": [
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                ],
                                "initial_component": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Simple_PDU"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {"_kind": "NullMessageTypeDef", "_value": "null " "message"},
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_PDU"},
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Message_Type"},
            },
        }
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
                            "_kind": "ModularTypeDef",
                            "mod": {"_kind": "NumericLiteral", "_value": "256"},
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "T"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "checksums": None,
                            "components": {
                                "_kind": "Components",
                                "components": [
                                    {
                                        "_kind": "Component",
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
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                ],
                                "initial_component": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "PDU"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "checksums": None,
                            "components": {
                                "_kind": "Components",
                                "components": [
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Bar"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "Baz"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "T"},
                                            "package": None,
                                        },
                                    },
                                ],
                                "initial_component": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Simple_PDU"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {"_kind": "NullMessageTypeDef", "_value": "null message"},
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Empty_PDU"},
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
                }
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
    foo_message = model.Message(
        identifier=ID("Test::Foo"),
        structure=[
            Link(Field("Initial"), Field("N")),
            Link(Field("N"), Field("Final")),
        ],
        types={Field("N"): model.ModularInteger("Test::T", expr.Number(256))},
    )
    assert_model_string(
        """
            package Test is
               type T is mod 256;
               type Foo is
                  message
                     N : T;
                  end message;
               type Bar is new Foo;
            end Test;
        """,
        Model(
            types=[
                model.Enumeration(
                    identifier=ID("__BUILTINS__::Boolean"),
                    literals=[
                        (ID("False"), expr.Number(0)),
                        (ID("True"), expr.Number(1)),
                    ],
                    size=expr.Number(1),
                    always_valid=False,
                ),
                model.Opaque(),
                model.ModularInteger("Test::T", expr.Number(256)),
                foo_message,
                model.DerivedMessage(
                    identifier=ID("Test::Bar"),
                    base=foo_message,
                    structure=[
                        Link(Field("Initial"), Field("N")),
                        Link(Field("N"), Field("Final")),
                    ],
                    types={Field("N"): model.ModularInteger("Test::T", expr.Number(256))},
                ),
            ],
        ),
    )


def test_parse_ethernet_spec() -> None:
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
                            "_kind": "ModularTypeDef",
                            "mod": {
                                "_kind": "BinOp",
                                "left": {"_kind": "NumericLiteral", "_value": "2"},
                                "op": {"_kind": "OpPow", "_value": "**"},
                                "right": {"_kind": "NumericLiteral", "_value": "48"},
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Address"},
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
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "ModularTypeDef",
                            "mod": {
                                "_kind": "BinOp",
                                "left": {"_kind": "NumericLiteral", "_value": "2"},
                                "op": {"_kind": "OpPow", "_value": "**"},
                                "right": {"_kind": "NumericLiteral", "_value": "16"},
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "TCI"},
                    },
                    {
                        "_kind": "TypeDecl",
                        "definition": {
                            "_kind": "MessageTypeDef",
                            "checksums": None,
                            "components": {
                                "_kind": "Components",
                                "components": [
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Destination",
                                        },
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Address"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {
                                            "_kind": "UnqualifiedID",
                                            "_value": "Source",
                                        },
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Address"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
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
                                                                        "_value": "Type_Length_TPID",
                                                                    },
                                                                    "package": None,
                                                                },
                                                            },
                                                            "kind": {
                                                                "_kind": "AttrFirst",
                                                                "_value": "First",
                                                            },
                                                        },
                                                    }
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
                                                                        "_value": "Type_Length_TPID",
                                                                    },
                                                                    "package": None,
                                                                },
                                                            },
                                                            "kind": {
                                                                "_kind": "AttrFirst",
                                                                "_value": "First",
                                                            },
                                                        },
                                                    }
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
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "TPID"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "TPID"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
                                        "aspects": [],
                                        "condition": None,
                                        "identifier": {"_kind": "UnqualifiedID", "_value": "TCI"},
                                        "thens": [],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "TCI"},
                                            "package": None,
                                        },
                                    },
                                    {
                                        "_kind": "Component",
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
                                                            "op": {"_kind": "OpMul", "_value": "*"},
                                                            "right": {
                                                                "_kind": "NumericLiteral",
                                                                "_value": "8",
                                                            },
                                                        },
                                                    }
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
                                                                "_kind": "Attribute",
                                                                "expression": {
                                                                    "_kind": "Variable",
                                                                    "identifier": {
                                                                        "_kind": "ID",
                                                                        "name": {
                                                                            "_kind": "UnqualifiedID",
                                                                            "_value": "Message",
                                                                        },
                                                                        "package": None,
                                                                    },
                                                                },
                                                                "kind": {
                                                                    "_kind": "AttrLast",
                                                                    "_value": "Last",
                                                                },
                                                            },
                                                            "op": {"_kind": "OpSub", "_value": "-"},
                                                            "right": {
                                                                "_kind": "Attribute",
                                                                "expression": {
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
                                                                "kind": {
                                                                    "_kind": "AttrLast",
                                                                    "_value": "Last",
                                                                },
                                                            },
                                                        },
                                                    }
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
                                    },
                                    {
                                        "_kind": "Component",
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
                                                                            "_kind": "UnqualifiedID",
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
                                                            "op": {"_kind": "OpDiv", "_value": "/"},
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
                                                                            "_kind": "UnqualifiedID",
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
                                                            "op": {"_kind": "OpDiv", "_value": "/"},
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
                                                "target": {"_kind": "NullID", "_value": "null"},
                                            }
                                        ],
                                        "type_identifier": {
                                            "_kind": "ID",
                                            "name": {"_kind": "UnqualifiedID", "_value": "Opaque"},
                                            "package": None,
                                        },
                                    },
                                ],
                                "initial_component": None,
                            },
                        },
                        "identifier": {"_kind": "UnqualifiedID", "_value": "Frame"},
                    },
                ],
                "end_identifier": {"_kind": "UnqualifiedID", "_value": "Ethernet"},
                "identifier": {"_kind": "UnqualifiedID", "_value": "Ethernet"},
            },
        }
    }

    assert_ast_files([f"{EX_SPEC_DIR}/ethernet.rflx"], spec)


def test_parse_error_illegal_package_identifiers() -> None:
    assert_error_string(
        """
            package RFLX_Types is
            end RFLX_Types;
        """,
        r'^<stdin>:2:21: parser: error: illegal prefix "RFLX" in package identifier "RFLX_Types"',
    )


def test_parse_error_inconsistent_package_identifiers() -> None:
    assert_error_string(
        """
            package A is
            end B;
        """,
        r'^<stdin>:3:17: parser: error: inconsistent package identifier "B"\n'
        r'<stdin>:2:21: parser: info: previous identifier was "A"',
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
        f"{SPEC_DIR}/incorrect_specification.rflx:3:10: parser: error: Expected 'is', got ';'",
    )


def test_parse_error_unexpected_exception_in_parser(monkeypatch: Any) -> None:
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=r"parser: error: TEST"):
        monkeypatch.setattr(parser, "check_naming", lambda x, e, f, o: raise_parser_error())
        p.parse_string(
            """
                package Test is
                   type T is mod 256;
                end Test;
            """
        )


def test_parse_error_context_dependency_cycle() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/context_cycle.rflx"],
        f"^"
        f"{SPEC_DIR}/context_cycle.rflx:1:6: parser: error: dependency cycle when "
        f'including "Context_Cycle_1"\n'
        f'{SPEC_DIR}/context_cycle_1.rflx:1:6: parser: info: when including "Context_Cycle_2"\n'
        f'{SPEC_DIR}/context_cycle_2.rflx:1:6: parser: info: when including "Context_Cycle_3"\n'
        f'{SPEC_DIR}/context_cycle_3.rflx:1:6: parser: info: when including "Context_Cycle_1"'
        f"$",
    )


def test_parse_error_message_undefined_component() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T
                        then Bar if Foo < 100
                        then null if Foo >= 100;
                  end message;
            end Test;
        """,
        r'^<stdin>:7:30: parser: error: undefined field "Bar"$',
    )


def test_parse_error_invalid_location_expression() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T
                        then Bar
                            with Foo => 1;
                    Bar : T;
                  end message;
            end Test;
        """,
        r"^<stdin>:8:34: parser: error: Invalid aspect Foo$",
    )


def test_parse_error_array_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is array of Foo;
            end Test;
        """,
        r'^<stdin>:3:35: parser: error: undefined element type "Test::Foo"$',
    )


def test_parse_error_refinement_undefined_message() -> None:
    assert_error_string(
        """
            package Test is
               for PDU use (Foo => Bar);
            end Test;
        """,
        r'^<stdin>:3:16: parser: error: undefined type "Test::PDU" in refinement$',
    )


def test_parse_error_refinement_undefined_sdu() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for PDU use (Foo => Bar);
            end Test;
        """,
        r'^<stdin>:8:36: parser: error: undefined type "Test::Bar" in refinement of "Test::PDU"$',
    )


def test_parse_error_derivation_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:3:32: parser: error: undefined base message "Test::Foo" in derived message$',
    )


def test_parse_error_derivation_unsupported_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is mod 256;
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:4:21: parser: error: illegal derivation "Test::Bar"\n'
        r'<stdin>:3:21: parser: info: invalid base message type "Test::Foo"',
    )


def test_parse_error_multiple_initial_node_edges() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
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
        r"^<stdin>:7:33: parser: error: Expected ';', got ','",
    )


def test_parse_error_multiple_initial_nodes() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
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
        r"^<stdin>:8:22: parser: error: Expected 'First', got 'null'",
    )


def test_parse_error_reserved_word_in_type_name() -> None:
    assert_error_string(
        """
            package Test is
               type Type is mod 256;
            end Test;
        """,
        r'^<stdin>:3:21: parser: error: reserved word "Type" used as identifier',
    )


def test_parse_error_reserved_word_in_message_component() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Message : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:6:22: parser: error: reserved word "Message" used as identifier',
    )


def test_parse_error_invalid_context_clause(tmp_path: Path) -> None:
    test_file = tmp_path / "test.rflx"

    with open(test_file, "x") as f:
        f.write("with invalid")

    p = parser.Parser()

    with pytest.raises(
        RecordFluxError, match=rf"^{test_file}:1:13: parser: error: Expected ';', got Termination$"
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
        model.Field("Bar"): model.ModularInteger("Message_Type::T", expr.Number(256)),
        model.Field("Baz"): model.ModularInteger("Message_Type::T", expr.Number(256)),
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
        **{model.Field("Foo"): model.ModularInteger("Message_Type::T", expr.Number(256))},
    }

    message = model.Message("Message_Type::PDU", structure, types)

    empty_message = model.Message("Message_Type::Empty_PDU", [], {})

    assert_messages_files(
        [f"{SPEC_DIR}/message_type.rflx"], [message, simple_message, empty_message]
    )


def test_create_model_message_in_message() -> None:
    length = model.ModularInteger(
        "Message_In_Message::Length", expr.Pow(expr.Number(2), expr.Number(16))
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
        "Message_In_Message::Derived_Length_Value", length_value
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
    assert_messages_files([f"{EX_SPEC_DIR}/ethernet.rflx"], [models.ETHERNET_FRAME])


def test_create_model_type_derivation_message() -> None:
    t = model.ModularInteger("Test::T", expr.Number(256))

    structure = [
        model.Link(model.INITIAL, model.Field("Baz")),
        model.Link(model.Field("Baz"), model.FINAL),
    ]

    types = {model.Field("Baz"): t}

    message_foo = model.Message("Test::Foo", structure, types)
    message_bar = model.DerivedMessage("Test::Bar", message_foo)

    assert_messages_string(
        """
            package Test is
               type T is mod 256;
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
        """
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


def test_create_model_parsed_field_locations() -> None:
    p = parser.Parser()
    p.parse_string(
        """
           package Test is
              type T is mod 2**8;
              type M is
                 message
                    F1 : T;
                    F2 : T;
                 end message;
           end Test;
        """
    )
    m = p.create_model()
    assert m.messages[0].fields == (
        model.Field(ID("F1", Location((6, 21), end=(6, 22)))),
        model.Field(ID("F2", Location((7, 21), end=(7, 22)))),
    )


def test_create_model_array_with_imported_element_type() -> None:
    p = parser.Parser()
    p.parse_string(
        """
           package Test is
              type T is mod 256;
           end Test;
        """
    )
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test::T;
           end Array_Test;
        """
    )
    m = p.create_model()
    arrays = [t for t in m.types if isinstance(t, model.Array)]
    assert len(arrays) == 1
    assert arrays[0].identifier == ID("Array_Test::T")
    assert arrays[0].element_type == model.ModularInteger("Test::T", expr.Number(256))


def test_create_model_checksum() -> None:
    p = parser.Parser()
    p.parse_string(
        """
           package Test is
              type T is mod 2**8;
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
        """
    )
    m = p.create_model()

    assert m.messages[0].checksums == {
        ID("C1"): [expr.Variable("F3"), expr.ValueRange(expr.First("F1"), expr.Last("F2"))],
        ID("C2"): [
            expr.ValueRange(
                expr.Add(expr.Last("C1"), expr.Number(1)),
                expr.Sub(expr.First("C2"), expr.Number(1)),
            )
        ],
    }


@pytest.mark.parametrize(
    "spec",
    [
        """
               type M is
                  message
                     A : T
                        then B
                           if A < 100;
                     B : T
                        if A > 10;
                  end message;
            """,
        """
               type M is
                  message
                     A : T;
                     B : T
                        if A > 10 and A < 100;
                  end message;
            """,
        """
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
    assert_messages_string(
        f"""
            package Test is

               type T is mod 256;

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
        """
               type M is
                  message
                     A : T
                        then B
                           with First => A'First;
                     B : T;
                  end message;
            """,
        """
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
    assert_messages_string(
        f"""
            package Test is

               type T is mod 256;

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
        """
               type M is
                  message
                     A : T
                        then B
                           with Size => 8;
                     B : Opaque;
                  end message;
            """,
        """
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
    assert_messages_string(
        f"""
            package Test is

               type T is mod 256;

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
    "link, field",
    [
        ("with First => A'First, Size => 8 if A > 10 and A < 100", ""),
        ("with First => A'First, Size => 8 if A < 100", "if A > 10"),
        ("with First => A'First, Size => 8", "if A > 10 and A < 100"),
        ("with First => A'First if A > 10 and A < 100", "with Size => 8"),
        ("with First => A'First if A < 100", "with Size => 8 if A > 10"),
        ("with First => A'First", "with Size => 8 if A > 10 and A < 100"),
        ("with Size => 8 if A > 10 and A < 100", "with First => A'First"),
        ("with Size => 8 if A < 100", "with First => A'First if A > 10"),
        ("with Size => 8", "with First => A'First if A > 10 and A < 100"),
        ("if A > 10 and A < 100", "with First => A'First, Size => 8"),
        ("if A < 100", "with First => A'First, Size => 8 if A > 10"),
        ("", "with First => A'First, Size => 8 if A > 10 and A < 100"),
    ],
)
def test_message_field_condition_and_aspects(link: str, field: str) -> None:
    assert_messages_string(
        f"""
            package Test is

               type T is mod 256;

               type M is
                  message
                     A : T
                        then B {link};
                     B : Opaque {field};
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


def test_parse_error_invalid_range_aspect() -> None:
    assert_error_string(
        """
            package Test is
                type T is range 1 .. 200 with Invalid => 42;
            end Test;
        """,
        r"^<stdin>:3:27: parser: error: invalid aspect Invalid for range type T",
    )


@pytest.mark.parametrize(
    "spec, error",
    [
        (
            """
            package Test is
                type T is (A, B) with Foo;
            end Test;
        """,
            r"^<stdin>:3:22: parser: error: No size set for Test::T",
        ),
        (
            """
            package Test is
                type T is (A, B) with Always_Valid => Invalid;
            end Test;
        """,
            r"^"
            "<stdin>:3:55: parser: error: Invalid Always_Valid expression: Invalid\n"
            "<stdin>:3:22: parser: error: No size set for Test::T"
            r"$",
        ),
    ],
)
def test_parse_error_invalid_enum(spec: str, error: str) -> None:
    assert_error_string(spec, error)
