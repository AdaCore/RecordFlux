# pylint: disable=too-many-lines

from itertools import zip_longest
from pathlib import Path
from typing import Any, Dict, List, Mapping, Sequence

import pytest
from librecordfluxdsllang import RFLXNode

from rflx import expression as expr, model
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.identifier import ID
from rflx.model import FINAL, INITIAL, OPAQUE, Field, Link, Message, Model, ModularInteger
from rflx.specification import cache, parser
from tests.const import EX_SPEC_DIR, SPEC_DIR
from tests.data import models

T = ModularInteger("Test::T", expr.Number(256))


def to_dict(node: Any) -> Dict[str, Any]:
    if node.is_list_type:
        return [to_dict(e) for e in node.children]
    result = {name[2:]: to_dict(getattr(node, name)) for name in dir(node) if name.startswith("f_")}
    if result:
        return result
    return node.text


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
    assert_ast_files([f"{SPEC_DIR}/{spec}.rflx"], [])


def test_parse_duplicate_specifications() -> None:
    files = [f"{SPEC_DIR}/empty_package.rflx", f"{SPEC_DIR}/empty_package.rflx"]
    assert_ast_files(
        files,
        {
            "Empty_Package": ast.Specification(
                ast.ContextSpec([]), ast.PackageSpec("Empty_Package", [], [])
            )
        },
    )


def test_parse_empty_package_spec() -> None:
    assert_ast_files(
        [f"{SPEC_DIR}/empty_package.rflx"],
        {
            "Empty_Package": ast.Specification(
                ast.ContextSpec([]), ast.PackageSpec("Empty_Package", [], [])
            )
        },
    )


def test_parse_context_spec() -> None:
    assert_ast_files(
        [f"{SPEC_DIR}/context.rflx"],
        {
            "Context": {
                "context_clause": [{"item": "Empty_File"}, {"item": "Empty_Package"}],
                "package_declaration": {
                    "declarations": [],
                    "end_identifier": "Context",
                    "identifier": "Context",
                },
            },
            "Empty_Package": {
                "context_clause": [],
                "package_declaration": {
                    "declarations": [],
                    "end_identifier": "Empty_Package",
                    "identifier": "Empty_Package",
                },
            },
        },
    )


def test_parse_integer_type_spec() -> None:
    spec = {
        "Integer_Type": ast.Specification(
            ast.ContextSpec([]),
            ast.PackageSpec(
                "Integer_Type",
                [
                    model.RangeInteger(
                        "__PACKAGE__::Page_Num", expr.Number(1), expr.Number(2000), expr.Number(16)
                    ),
                    model.RangeInteger(
                        "__PACKAGE__::Line_Size", expr.Number(0), expr.Number(255), expr.Number(8)
                    ),
                    model.ModularInteger("__PACKAGE__::Byte", expr.Number(256)),
                    model.ModularInteger("__PACKAGE__::Hash_Index", expr.Number(64)),
                ],
                [],
            ),
        )
    }
    assert_ast_files([f"{SPEC_DIR}/integer_type.rflx"], spec)


def test_parse_enumeration_type_spec() -> None:
    spec = {
        "Enumeration_Type": ast.Specification(
            ast.ContextSpec([]),
            ast.PackageSpec(
                "Enumeration_Type",
                [
                    model.Enumeration(
                        "__PACKAGE__::Day",
                        [
                            ("Mon", expr.Number(1)),
                            ("Tue", expr.Number(2)),
                            ("Wed", expr.Number(3)),
                            ("Thu", expr.Number(4)),
                            ("Fri", expr.Number(5)),
                            ("Sat", expr.Number(6)),
                            ("Sun", expr.Number(7)),
                        ],
                        expr.Number(3),
                        False,
                    ),
                    model.Enumeration(
                        "__PACKAGE__::Gender",
                        [("M", expr.Number(0)), ("F", expr.Number(1))],
                        expr.Number(1),
                        False,
                    ),
                    model.Enumeration(
                        "__PACKAGE__::Priority",
                        [
                            ("Low", expr.Number(1)),
                            ("Medium", expr.Number(4)),
                            ("High", expr.Number(7)),
                        ],
                        expr.Number(8),
                        True,
                    ),
                ],
                [],
            ),
        )
    }
    assert_ast_files([f"{SPEC_DIR}/enumeration_type.rflx"], spec)


def test_parse_array_type_spec() -> None:
    spec = {
        "Array_Type": ast.Specification(
            ast.ContextSpec([]),
            ast.PackageSpec(
                "Array_Type",
                [
                    model.ModularInteger("__PACKAGE__::Byte", expr.Number(256)),
                    ast.ArraySpec("__PACKAGE__::Bytes", ast.ReferenceSpec("__PACKAGE__::Byte")),
                    ast.MessageSpec(
                        "__PACKAGE__::Foo",
                        [
                            ast.Component(
                                "Length",
                                "Byte",
                                [
                                    ast.Then(
                                        "Bytes",
                                        expr.UNDEFINED,
                                        expr.Mul(expr.Variable("Length"), expr.Number(8)),
                                    )
                                ],
                            ),
                            ast.Component("Bytes", "Bytes"),
                        ],
                    ),
                    ast.ArraySpec("__PACKAGE__::Bar", ast.ReferenceSpec("__PACKAGE__::Foo")),
                ],
                [],
            ),
        )
    }
    assert_ast_files([f"{SPEC_DIR}/array_type.rflx"], spec)


def test_parse_message_type_spec() -> None:
    spec = {
        "Message_Type": ast.Specification(
            ast.ContextSpec([]),
            ast.PackageSpec(
                "Message_Type",
                [
                    model.ModularInteger("__PACKAGE__::T", expr.Number(256)),
                    ast.MessageSpec(
                        "__PACKAGE__::PDU",
                        [
                            ast.Component(
                                "Foo",
                                "T",
                                [
                                    ast.Then(
                                        "Bar",
                                        expr.UNDEFINED,
                                        expr.UNDEFINED,
                                        expr.LessEqual(expr.Variable("Foo"), expr.Number(30, 16)),
                                    ),
                                    ast.Then(
                                        "Baz",
                                        expr.UNDEFINED,
                                        expr.UNDEFINED,
                                        expr.Greater(expr.Variable("Foo"), expr.Number(30, 16)),
                                    ),
                                ],
                            ),
                            ast.Component("Bar", "T"),
                            ast.Component("Baz", "T"),
                        ],
                    ),
                    ast.MessageSpec(
                        "__PACKAGE__::Simple_PDU",
                        [ast.Component("Bar", "T"), ast.Component("Baz", "T")],
                    ),
                    ast.MessageSpec("__PACKAGE__::Empty_PDU", []),
                ],
                [],
            ),
        )
    }
    assert_ast_files([f"{SPEC_DIR}/message_type.rflx"], spec)


def test_parse_type_refinement_spec() -> None:
    spec = {
        "Message_Type": ast.Specification(
            ast.ContextSpec([]),
            ast.PackageSpec(
                "Message_Type",
                [
                    model.ModularInteger("__PACKAGE__::T", expr.Number(256)),
                    ast.MessageSpec(
                        "__PACKAGE__::PDU",
                        [
                            ast.Component(
                                "Foo",
                                "T",
                                [
                                    ast.Then(
                                        "Bar",
                                        expr.UNDEFINED,
                                        expr.UNDEFINED,
                                        expr.LessEqual(expr.Variable("Foo"), expr.Number(30, 16)),
                                    ),
                                    ast.Then(
                                        "Baz",
                                        expr.UNDEFINED,
                                        expr.UNDEFINED,
                                        expr.Greater(expr.Variable("Foo"), expr.Number(30, 16)),
                                    ),
                                ],
                            ),
                            ast.Component("Bar", "T"),
                            ast.Component("Baz", "T"),
                        ],
                    ),
                    ast.MessageSpec(
                        "__PACKAGE__::Simple_PDU",
                        [ast.Component("Bar", "T"), ast.Component("Baz", "T")],
                    ),
                    ast.MessageSpec("__PACKAGE__::Empty_PDU", []),
                ],
                [],
            ),
        ),
        "Type_Refinement": ast.Specification(
            ast.ContextSpec(["Message_Type"]),
            ast.PackageSpec(
                "Type_Refinement",
                [
                    ast.RefinementSpec(
                        "Message_Type::Simple_PDU",
                        "Bar",
                        "Message_Type::PDU",
                        expr.Equal(expr.Variable("Baz"), expr.Number(42)),
                    ),
                    ast.RefinementSpec("Message_Type::PDU", "Bar", "Message_Type::Simple_PDU"),
                ],
                [],
            ),
        ),
    }
    assert_ast_files([f"{SPEC_DIR}/message_type.rflx", f"{SPEC_DIR}/type_refinement.rflx"], spec)


def test_parse_type_derivation_spec() -> None:
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
        {
            "Test": ast.Specification(
                ast.ContextSpec([]),
                ast.PackageSpec(
                    "Test",
                    [
                        model.ModularInteger("__PACKAGE__::T", expr.Number(256)),
                        ast.MessageSpec("__PACKAGE__::Foo", [ast.Component("N", "T")]),
                        ast.DerivationSpec("__PACKAGE__::Bar", "Foo"),
                    ],
                    [],
                ),
            )
        },
    )


def test_parse_ethernet_spec() -> None:
    spec = {
        "Ethernet": ast.Specification(
            ast.ContextSpec([]),
            ast.PackageSpec(
                "Ethernet",
                [
                    model.ModularInteger(
                        "__PACKAGE__::Address", expr.Pow(expr.Number(2), expr.Number(48))
                    ),
                    model.RangeInteger(
                        "__PACKAGE__::Type_Length",
                        expr.Number(46),
                        expr.Sub(expr.Pow(expr.Number(2), expr.Number(16)), expr.Number(1)),
                        expr.Number(16),
                    ),
                    model.RangeInteger(
                        "__PACKAGE__::TPID",
                        expr.Number(0x8100, 16),
                        expr.Number(0x8100, 16),
                        expr.Number(16),
                    ),
                    model.ModularInteger(
                        "__PACKAGE__::TCI", expr.Pow(expr.Number(2), expr.Number(16))
                    ),
                    ast.MessageSpec(
                        "__PACKAGE__::Frame",
                        [
                            ast.Component("Destination", "Address"),
                            ast.Component("Source", "Address"),
                            ast.Component(
                                "Type_Length_TPID",
                                "Type_Length",
                                [
                                    ast.Then(
                                        "TPID",
                                        expr.First("Type_Length_TPID"),
                                        expr.UNDEFINED,
                                        expr.Equal(
                                            expr.Variable("Type_Length_TPID"),
                                            expr.Number(33024, 16),
                                        ),
                                    ),
                                    ast.Then(
                                        "Type_Length",
                                        expr.First("Type_Length_TPID"),
                                        expr.UNDEFINED,
                                        expr.NotEqual(
                                            expr.Variable("Type_Length_TPID"),
                                            expr.Number(33024, 16),
                                        ),
                                    ),
                                ],
                            ),
                            ast.Component("TPID", "TPID"),
                            ast.Component("TCI", "TCI"),
                            ast.Component(
                                "Type_Length",
                                "Type_Length",
                                [
                                    ast.Then(
                                        "Payload",
                                        expr.UNDEFINED,
                                        expr.Mul(expr.Variable("Type_Length"), expr.Number(8)),
                                        expr.LessEqual(
                                            expr.Variable("Type_Length"), expr.Number(1500)
                                        ),
                                    ),
                                    ast.Then(
                                        "Payload",
                                        expr.UNDEFINED,
                                        expr.Sub(expr.Last("Message"), expr.Last("Type_Length")),
                                        expr.GreaterEqual(
                                            expr.Variable("Type_Length"), expr.Number(1536)
                                        ),
                                    ),
                                ],
                            ),
                            ast.Component(
                                "Payload",
                                "Opaque",
                                [
                                    ast.Then(
                                        condition=expr.And(
                                            expr.GreaterEqual(
                                                expr.Div(expr.Size("Payload"), expr.Number(8)),
                                                expr.Number(46),
                                            ),
                                            expr.LessEqual(
                                                expr.Div(expr.Size("Payload"), expr.Number(8)),
                                                expr.Number(1500),
                                            ),
                                        ),
                                    )
                                ],
                            ),
                        ],
                    ),
                ],
                [],
            ),
        )
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
        f'{SPEC_DIR}/incorrect_specification.rflx:3:10: parser: error: Expected "is"',
    )


def test_parse_error_unexpected_exception_in_parser(monkeypatch: Any) -> None:
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=r"parser: error: TEST"):
        monkeypatch.setattr(parser, "check_naming", lambda x, e: raise_parser_error())
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
                        then Bar;
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
        r'^<stdin>:8:38: parser: error: Expected {{"First" - "=>" - MathematicalExpression}'
        r' | {"Size" - "=>" - MathematicalExpression}}$',
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
        r'^<stdin>:3:16: parser: error: undefined base message "Test::Foo" in derived message$',
    )


def test_parse_error_derivation_unsupported_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is mod 256;
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:4:16: parser: error: illegal derivation "Test::Bar"\n'
        r'<stdin>:3:16: parser: info: invalid base message type "Test::Foo"',
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
        r'^<stdin>:7:33: parser: error: Expected ";"',
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
        r'^<stdin>:8:22: parser: error: reserved word "null" used as identifier',
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
        r'^<stdin>:6:22: parser: error: Found unwanted token, "Message"',
    )


def test_parse_error_invalid_context_clause(tmp_path: Path) -> None:
    test_file = tmp_path / "test.rflx"

    with open(test_file, "x") as f:
        f.write("with invalid")

    p = parser.Parser()

    with pytest.raises(RecordFluxError, match=rf'^{test_file}:1:13: parser: error: Expected ";"$'):
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
           with Test;
           package Array_Test is
              type T is array of Test::T;
           end Array_Test;
        """
    )
    p.parse_string(
        """
           package Test is
              type T is mod 256;
           end Test;
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
