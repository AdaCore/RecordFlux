# pylint: disable=too-many-lines

from itertools import zip_longest
from pathlib import Path
from typing import Any, Dict, Sequence

import pytest

from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.expression import (
    UNDEFINED,
    Aggregate,
    And,
    Div,
    Equal,
    First,
    Greater,
    GreaterEqual,
    Last,
    Length,
    LessEqual,
    Mul,
    NotEqual,
    Number,
    Pow,
    Sub,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    FINAL,
    INITIAL,
    Array,
    DerivedMessage,
    Enumeration,
    Field,
    Link,
    Message,
    ModularInteger,
    Opaque,
    RangeInteger,
    Refinement,
)
from rflx.parser import grammar, parser
from rflx.parser.ast import (
    ContextSpec,
    DerivationSpec,
    MessageSpec,
    PackageSpec,
    ReferenceSpec,
    RefinementSpec,
    Specification,
    Then,
)
from rflx.parser.parser import Component, ParseFatalException, Parser
from tests.models import ETHERNET_FRAME
from tests.utils import assert_equal

TESTDIR = "tests"
SPECDIR = "specs"


def assert_specifications_files(
    filenames: Sequence[str], specifications: Dict[str, Specification]
) -> None:
    p = Parser()
    for filename in filenames:
        p.parse(Path(filename))
    assert p.specifications == specifications, filenames


def assert_specifications_string(string: str, specifications: Dict[str, Specification]) -> None:
    p = Parser()
    p.parse_string(string)
    assert p.specifications == specifications


def assert_messages_files(filenames: Sequence[str], messages: Sequence[Message]) -> None:
    p = Parser()
    for filename in filenames:
        p.parse(Path(filename))
    model = p.create_model()
    assert_messages(model.messages, messages)


def assert_messages_string(string: str, messages: Sequence[Message]) -> None:
    p = Parser()
    p.parse_string(string)
    model = p.create_model()
    assert_messages(model.messages, messages)


def assert_messages(
    actual_messages: Sequence[Message], expected_messages: Sequence[Message]
) -> None:
    for actual, expected in zip_longest(actual_messages, expected_messages):
        assert actual.full_name == expected.full_name
        assert actual.structure == expected.structure, expected.full_name
        assert actual.types == expected.types, expected.full_name
        assert actual.fields == expected.fields, expected.full_name
    assert actual_messages == expected_messages


def assert_refinements_string(string: str, refinements: Sequence[Refinement]) -> None:
    p = Parser()
    p.parse_string(string)
    model = p.create_model()
    assert model.refinements == refinements


def assert_error_files(filenames: Sequence[str], regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        p = Parser()
        for filename in filenames:
            p.parse(Path(filename))
        p.create_model()


def assert_error_string(string: str, regex: str) -> None:
    p = Parser()
    with pytest.raises(RecordFluxError, match=regex):
        p.parse_string(string)
        p.create_model()


def raise_parser_error() -> None:
    fail("TEST", Subsystem.PARSER, Severity.ERROR)


def test_unexpected_exception_in_grammar(monkeypatch: Any) -> None:
    with pytest.raises(ParseFatalException, match=r"implementation error \(division by zero\)"):
        monkeypatch.setattr(
            grammar,
            "parse_mathematical_expression",
            grammar.fatalexceptions(lambda x, y, z: [1, 1 / 0, 1]),
        )
        grammar.mathematical_expression().parseString("1 + 1")


def test_numeric_literal() -> None:
    assert grammar.numeric_literal().parseString("1000")[0] == Number(1000)
    assert grammar.numeric_literal().parseString("1_000")[0] == Number(1000)
    assert grammar.numeric_literal().parseString("16#6664#")[0] == Number(26212)
    assert grammar.numeric_literal().parseString("16#66_64#")[0] == Number(26212)


def test_mathematical_expression_precedence() -> None:
    assert_equal(
        grammar.mathematical_expression().parseString("A - B * 2**3 - 1")[0],
        Sub(Sub(Variable("A"), Mul(Variable("B"), Pow(Number(2), Number(3)))), Number(1)),
    )
    assert_equal(
        grammar.mathematical_expression().parseString("(A - B) * 2**3 - 1")[0],
        Sub(Mul(Sub(Variable("A"), Variable("B")), Pow(Number(2), Number(3))), Number(1)),
    )
    assert_equal(
        grammar.mathematical_expression().parseString("A - B * 2**(3 - 1)")[0],
        Sub(Variable("A"), Mul(Variable("B"), Pow(Number(2), Sub(Number(3), Number(1))))),
    )
    assert_equal(
        grammar.mathematical_expression().parseString("A - (B * 2)**3 - 1")[0],
        Sub(Sub(Variable("A"), Pow(Mul(Variable("B"), Number(2)), Number(3))), Number(1)),
    )
    assert_equal(
        grammar.mathematical_expression().parseString("A - (B * 2**3 - 1)")[0],
        Sub(Variable("A"), Sub(Mul(Variable("B"), Pow(Number(2), Number(3))), Number(1))),
    )


def test_mathematical_expression_aggregate() -> None:
    assert_equal(
        grammar.mathematical_expression().parseString("(1, 2)")[0], Aggregate(Number(1), Number(2))
    )
    assert_equal(grammar.mathematical_expression().parseString("(1)")[0], Aggregate(Number(1)))


def test_mathematical_expression_aggregate_no_number() -> None:
    with pytest.raises(ParseFatalException, match=r"^Expected Number"):
        grammar.mathematical_expression().parseString("(1, Foo)")


def test_mathematical_expression_string() -> None:
    assert_equal(
        grammar.mathematical_expression().parseString('"PNG"')[0],
        Aggregate(Number(80), Number(78), Number(71)),
    )


def test_mathematical_expression_concatenation() -> None:
    assert_equal(
        grammar.mathematical_expression().parseString('(137) & "PNG" & (13, 10, 26, 10)')[0],
        Aggregate(
            Number(137),
            Number(80),
            Number(78),
            Number(71),
            Number(13),
            Number(10),
            Number(26),
            Number(10),
        ),
    )


def test_unexpected_relation_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected relation operator"):
        grammar.parse_relation("", 0, [Number(1), "<>", Number(1)])


def test_unexpected_logical_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected logical operator"):
        grammar.parse_logical_expression("", 0, [[Number(1), "xor", Number(1)]])


def test_unexpected_mathematical_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected mathematical operator"):
        grammar.parse_mathematical_expression(
            "",
            0,
            [[Number(1, location=Location((1, 1))), "//", Number(1, location=Location((1, 8)))]],
        )


def test_unexpected_attribute() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected attribute"):
        grammar.parse_attribute("", 0, ["V", "'", "Access"])


def test_unexpected_type() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected type"):
        grammar.parse_type("type T is X;", 0, [0, "type", "T", "is", "X", 8])


def test_illegal_package_identifiers() -> None:
    assert_error_string(
        """
            package RFLX_Types is
            end RFLX_Types;
        """,
        r'^<stdin>:2:21: parser: error: illegal prefix "RFLX" in package identifier "RFLX_Types"',
    )


def test_inconsistent_package_identifiers() -> None:
    assert_error_string(
        """
            package A is
            end B;
        """,
        r'^<stdin>:3:17: parser: error: inconsistent package identifier "B"\n'
        r'<stdin>:2:21: parser: info: previous identifier was "A"',
    )


def test_empty_file_spec() -> None:
    assert_specifications_files([f"{TESTDIR}/empty_file.rflx"], {})


def test_empty_file_message() -> None:
    assert_messages_files([f"{TESTDIR}/empty_file.rflx"], [])


def test_comment_only_spec() -> None:
    assert_specifications_files([f"{TESTDIR}/comment_only.rflx"], {})


def test_comment_only_message() -> None:
    assert_messages_files([f"{TESTDIR}/comment_only.rflx"], [])


def test_incorrect_name() -> None:
    assert_error_files(
        [f"{TESTDIR}/incorrect_name.rflx"],
        f"^{TESTDIR}/incorrect_name.rflx:1:9: parser: error: file name does not match unit name"
        r' "Test", should be "test.rflx"$',
    )


def test_incorrect_specification() -> None:
    assert_error_files(
        [f"{TESTDIR}/incorrect_specification.rflx"],
        f'{TESTDIR}/incorrect_specification.rflx:3:10: parser: error: Expected "is"',
    )


def test_unexpected_exception_in_parser(monkeypatch: Any) -> None:
    p = Parser()
    with pytest.raises(RecordFluxError, match=r"parser: error: TEST"):
        monkeypatch.setattr(parser, "check_types", lambda x, e: raise_parser_error())
        p.parse_string(
            """
                package Test is
                   type T is mod 256;
                end Test;
            """
        )
        p.create_model()


def test_package_spec() -> None:
    assert_specifications_files(
        [f"{TESTDIR}/empty_package.rflx"],
        {"Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", []))},
    )


def test_package_message() -> None:
    assert_messages_files([f"{TESTDIR}/empty_package.rflx"], [])


def test_duplicate_specifications() -> None:
    files = [f"{TESTDIR}/empty_package.rflx", f"{TESTDIR}/empty_package.rflx"]
    assert_specifications_files(
        files, {"Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", []))},
    )
    assert_messages_files(files, [])


def test_context_spec() -> None:
    assert_specifications_files(
        [f"{TESTDIR}/context.rflx"],
        {
            "Context": Specification(
                ContextSpec(["Empty_File", "Empty_Package"]), PackageSpec("Context", []),
            ),
            "Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", [])),
        },
    )


def test_context_message() -> None:
    assert_messages_files([f"{TESTDIR}/context.rflx"], [])


def test_context_dependency_cycle() -> None:
    assert_error_files(
        [f"{TESTDIR}/context_cycle.rflx"],
        f"^"
        f"{TESTDIR}/context_cycle.rflx:1:6: parser: error: dependency cycle when "
        f'including "Context_Cycle_1"\n'
        f'{TESTDIR}/context_cycle_1.rflx:1:6: parser: info: when including "Context_Cycle_2"\n'
        f'{TESTDIR}/context_cycle_2.rflx:1:6: parser: info: when including "Context_Cycle_3"\n'
        f'{TESTDIR}/context_cycle_3.rflx:1:6: parser: info: when including "Context_Cycle_1"'
        f"$",
    )


def test_duplicate_type() -> None:
    assert_error_files(
        [f"{TESTDIR}/duplicate_type.rflx"],
        f'{TESTDIR}/duplicate_type.rflx:3:4: parser: error: duplicate type "Duplicate_Type.T"\n'
        f"{TESTDIR}/duplicate_type.rflx:2:4: parser: info:"
        f' previous occurrence of "Duplicate_Type.T"',
    )


def test_message_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     Foo : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:5:22: model: error: missing type for field "Foo" in "Test.PDU"$',
    )


def test_message_undefined_component() -> None:
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
        r'^<stdin>:7:30: parser: error: undefined component "Bar"$',
    )


def test_invalid_location_expression() -> None:
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
        r' | {"Length" - "=>" - MathematicalExpression}}$',
    )


def test_illegal_redefinition() -> None:
    assert_error_string(
        """
            package Test is
               type Boolean is mod 2;
            end Test;
        """,
        r'^<stdin>:3:16: model: error: illegal redefinition of built-in type "Boolean"',
    )


def test_invalid_modular_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 2**128;
            end Test;
        """,
        r'^<stdin>:3:30: model: error: modulus of "T" exceeds limit \(2\*\*64\)',
    )


def test_invalid_enumeration_type_size() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo, Bar, Baz) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:16: model: error: size of "T" too small',
    )


def test_invalid_enumeration_type_duplicate_elements() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo, Foo) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:32: model: error: duplicate element "Foo"\n'
        r"<stdin>:3:27: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_multiple_duplicate_elements() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo, Bar, Foo, Bar) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:37: model: error: duplicate element "Foo"\n'
        r"<stdin>:3:27: model: info: previous occurrence\n"
        r'<stdin>:3:42: model: error: duplicate element "Bar"\n'
        r"<stdin>:3:32: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo => 0, Bar => 0) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:44: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_multiple_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo => 0, Foo_1 => 1, Bar => 0, Bar_1 => 1) with Size => 8;
            end Test;
        """,
        r'<stdin>:3:56: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence\n"
        r'<stdin>:3:68: model: error: duplicate enumeration value "1" in "T"\n'
        r"<stdin>:3:46: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_identical_literals() -> None:
    assert_error_string(
        """
            package Test is
               type T1 is (Foo, Bar) with Size => 1;
               type T2 is (Bar, Baz) with Size => 1;
            end Test;
        """,
        r"<stdin>:4:16: parser: error: conflicting literals: Bar\n"
        r'<stdin>:3:33: parser: info: previous occurrence of "Bar"',
    )


def test_invalid_enumeration_type_identical_literals_location() -> None:
    assert_error_files(
        [f"{TESTDIR}/identical_literals.rflx"],
        f"{TESTDIR}/identical_literals.rflx:3:4: parser: error: conflicting literals: Bar\n"
        f'{TESTDIR}/identical_literals.rflx:2:21: parser: info: previous occurrence of "Bar"',
    )


def test_invalid_enumeration_type_builtin_literals() -> None:
    assert_error_string(
        """
            package Test is
               type T is (True, False) with Size => 1;
            end Test;
        """,
        r"<stdin>:3:16: parser: error: conflicting literals: False, True\n"
        r'__BUILTINS__:1:18: parser: info: previous occurrence of "False"\n'
        r'__BUILTINS__:1:24: parser: info: previous occurrence of "True"',
    )


def test_name_conflict_between_literal_and_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo, Bar) with Size => 1;
               type Foo is mod 2**8;
               type Bar is mod 2**8;
            end Test;
        """,
        r'<stdin>:3:32: parser: error: literal conflicts with type "Bar"\n'
        r"<stdin>:5:16: parser: info: conflicting type declaration\n"
        r'<stdin>:3:27: parser: error: literal conflicts with type "Foo"\n'
        r"<stdin>:4:16: parser: info: conflicting type declaration",
    )


def test_array_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is array of Foo;
            end Test;
        """,
        r'^<stdin>:3:35: parser: error: undefined element type "Test.Foo"$',
    )


def test_array_unsupported_element_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is mod 2**4;
               type T is array of Foo;
            end Test;
        """,
        r"<stdin>:4:16: parser: error: unsupported element type size\n"
        r'<stdin>:3:16: parser: info: type "Test.Foo" has size 4, must be multiple of 8',
    )
    assert_error_string(
        """
            package Test is
               type T is array of Boolean;
            end Test;
        """,
        r"<stdin>:3:16: parser: error: unsupported element type size\n"
        r'__BUILTINS__:1:1: parser: info: type "__BUILTINS__.Boolean" has size 1,'
        r" must be multiple of 8",
    )


def test_duplicate_message() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               type PDU is
                  message
                     Foo : T;
                  end message;
            end Test;
        """,
        r'parser: error: duplicate type "Test.PDU"',
    )


def test_duplicate_refinement() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for Test.PDU use (Foo => Test.PDU);
               for PDU use (Foo => PDU);
            end Test;
        """,
        r'^<stdin>:9:16: parser: error: duplicate refinement with "Test.PDU"\n'
        r"<stdin>:8:16: parser: info: previous occurrence",
    )


def test_refinement_undefined_message() -> None:
    assert_error_string(
        """
            package Test is
               for PDU use (Foo => Bar);
            end Test;
        """,
        r'^<stdin>:3:16: parser: error: undefined type "Test.PDU" in refinement$',
    )


def test_refinement_undefined_sdu() -> None:
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
        r'^<stdin>:8:36: parser: error: undefined type "Test.Bar" in refinement of "Test.PDU"$',
    )


def test_refinement_invalid_field() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for PDU use (Bar => PDU);
            end Test;
        """,
        r'^<stdin>:8:29: parser: error: invalid field "Bar" in refinement',
    )


def test_refinement_invalid_condition() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for PDU use (Foo => PDU)
                  if X < Y + 1;
            end Test;
        """,
        r"^"
        r'<stdin>:9:22: parser: error: unknown field or literal "X"'
        r' in refinement condition of "Test.PDU"\n'
        r'<stdin>:9:26: parser: error: unknown field or literal "Y"'
        r' in refinement condition of "Test.PDU"'
        r"$",
    )


def test_derivation_duplicate_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type Foo is
                  message
                     Foo : T;
                  end message;
               type Bar is new Test.Foo;
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:9:16: parser: error: duplicate type "Test.Bar"\n'
        r'<stdin>:8:16: parser: info: previous occurrence of "Test.Bar"',
    )


def test_derivation_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:3:16: parser: error: undefined base message "Test.Foo" in derived message$',
    )


def test_derivation_unsupported_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is mod 256;
               type Bar is new Foo;
            end Test;
        """,
        r'^<stdin>:4:16: parser: error: illegal derivation "Test.Bar"\n'
        r'<stdin>:3:16: parser: info: invalid base message type "Test.Foo"',
    )


def test_derivation_of_derived_type() -> None:
    assert_error_string(
        """
            package Test is
               type Foo is null message;
               type Bar is new Foo;
               type Baz is new Bar;
            end Test;
        """,
        r'^<stdin>:5:16: parser: error: illegal derivation "Test.Baz"\n'
        r'<stdin>:4:16: parser: info: invalid base message "Test.Bar"$',
    )


def test_invalid_first_in_initial_node() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     null
                        then Foo
                           with First => 0;
                     Foo : T;
                  end message;
            end Test;
        """,
        r"^<stdin>:8:42: parser: error: invalid first expression$",
    )


def test_multiple_initial_node_edges() -> None:
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


def test_multiple_initial_nodes() -> None:
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


def test_reserved_word_in_type_name() -> None:
    assert_error_string(
        """
            package Test is
               type Type is mod 256;
            end Test;
        """,
        r'^<stdin>:3:21: parser: error: reserved word "Type" used as identifier',
    )


def test_reserved_word_in_message_component() -> None:
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


def test_integer_type_spec() -> None:
    spec = {
        "Integer_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Integer_Type",
                [
                    RangeInteger("__PACKAGE__.Page_Num", Number(1), Number(2000), Number(16)),
                    RangeInteger("__PACKAGE__.Line_Size", Number(0), Number(255), Number(8)),
                    ModularInteger("__PACKAGE__.Byte", Number(256)),
                    ModularInteger("__PACKAGE__.Hash_Index", Number(64)),
                ],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/integer_type.rflx"], spec)


def test_enumeration_type_spec() -> None:
    spec = {
        "Enumeration_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Enumeration_Type",
                [
                    Enumeration(
                        "__PACKAGE__.Day",
                        {
                            "Mon": Number(1),
                            "Tue": Number(2),
                            "Wed": Number(3),
                            "Thu": Number(4),
                            "Fri": Number(5),
                            "Sat": Number(6),
                            "Sun": Number(7),
                        },
                        Number(3),
                        False,
                    ),
                    Enumeration(
                        "__PACKAGE__.Gender", {"M": Number(0), "F": Number(1)}, Number(1), False,
                    ),
                    Enumeration(
                        "__PACKAGE__.Priority",
                        {"LOW": Number(1), "MEDIUM": Number(4), "HIGH": Number(7)},
                        Number(3),
                        True,
                    ),
                ],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/enumeration_type.rflx"], spec)


def test_array_type_spec() -> None:
    spec = {
        "Array_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Array_Type",
                [
                    ModularInteger("__PACKAGE__.Byte", Number(256)),
                    Array("__PACKAGE__.Bytes", ReferenceSpec("__PACKAGE__.Byte")),
                    MessageSpec(
                        "__PACKAGE__.Foo",
                        [
                            Component(
                                "Length",
                                "Byte",
                                [Then("Bytes", UNDEFINED, Mul(Variable("Length"), Number(8)))],
                            ),
                            Component("Bytes", "Bytes"),
                        ],
                    ),
                    Array("__PACKAGE__.Bar", ReferenceSpec("__PACKAGE__.Foo")),
                ],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/array_type.rflx"], spec)


def test_message_type_spec() -> None:
    spec = {
        "Message_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Message_Type",
                [
                    ModularInteger("__PACKAGE__.T", Number(256)),
                    MessageSpec(
                        "__PACKAGE__.PDU",
                        [
                            Component(
                                "Foo",
                                "T",
                                [
                                    Then(
                                        "Bar",
                                        UNDEFINED,
                                        UNDEFINED,
                                        LessEqual(Variable("Foo"), Number(30, 16)),
                                    ),
                                    Then(
                                        "Baz",
                                        UNDEFINED,
                                        UNDEFINED,
                                        Greater(Variable("Foo"), Number(30, 16)),
                                    ),
                                ],
                            ),
                            Component("Bar", "T"),
                            Component("Baz", "T"),
                        ],
                    ),
                    MessageSpec(
                        "__PACKAGE__.Simple_PDU", [Component("Bar", "T"), Component("Baz", "T")],
                    ),
                    MessageSpec("__PACKAGE__.Empty_PDU", []),
                ],
            ),
        )
    }
    assert_specifications_files([f"{TESTDIR}/message_type.rflx"], spec)


def test_message_type_message() -> None:
    simple_structure = [
        Link(INITIAL, Field("Bar")),
        Link(Field("Bar"), Field("Baz")),
        Link(Field("Baz"), FINAL),
    ]

    simple_types = {
        Field("Bar"): ModularInteger("Message_Type.T", Number(256)),
        Field("Baz"): ModularInteger("Message_Type.T", Number(256)),
    }

    simple_message = Message("Message_Type.Simple_PDU", simple_structure, simple_types)

    structure = [
        Link(INITIAL, Field("Foo")),
        Link(Field("Foo"), Field("Bar"), LessEqual(Variable("Foo"), Number(30, 16))),
        Link(Field("Foo"), Field("Baz"), Greater(Variable("Foo"), Number(30, 16))),
        Link(Field("Bar"), Field("Baz")),
        Link(Field("Baz"), FINAL),
    ]

    types = {
        **simple_types,
        **{Field("Foo"): ModularInteger("Message_Type.T", Number(256))},
    }

    message = Message("Message_Type.PDU", structure, types)

    empty_message = Message("Message_Type.Empty_PDU", [], {})

    assert_messages_files(
        [f"{TESTDIR}/message_type.rflx"], [message, simple_message, empty_message]
    )


def test_message_in_message() -> None:
    length = ModularInteger("Message_In_Message.Length", Pow(Number(2), Number(16)))

    length_value = Message(
        "Message_In_Message.Length_Value",
        [
            Link(INITIAL, Field("Length")),
            Link(Field("Length"), Field("Value"), length=Variable("Length")),
            Link(Field("Value"), FINAL),
        ],
        {Field("Length"): length, Field("Value"): Opaque()},
    )

    derived_length_value = DerivedMessage("Message_In_Message.Derived_Length_Value", length_value)

    message = Message(
        "Message_In_Message.Message",
        [
            Link(INITIAL, Field("Foo_Length")),
            Link(Field("Foo_Value"), Field("Bar_Length")),
            Link(Field("Bar_Value"), FINAL),
            Link(Field("Foo_Length"), Field("Foo_Value"), length=Variable("Foo_Length")),
            Link(Field("Bar_Length"), Field("Bar_Value"), length=Variable("Bar_Length")),
        ],
        {
            Field("Foo_Length"): length,
            Field("Foo_Value"): Opaque(),
            Field("Bar_Length"): length,
            Field("Bar_Value"): Opaque(),
        },
    )

    derived_message = DerivedMessage("Message_In_Message.Derived_Message", message)

    assert_messages_files(
        [f"{TESTDIR}/message_in_message.rflx"],
        [length_value, derived_length_value, message, derived_message],
    )


def test_type_refinement_spec() -> None:
    spec = {
        "Message_Type": Specification(
            ContextSpec([]),
            PackageSpec(
                "Message_Type",
                [
                    ModularInteger("__PACKAGE__.T", Number(256)),
                    MessageSpec(
                        "__PACKAGE__.PDU",
                        [
                            Component(
                                "Foo",
                                "T",
                                [
                                    Then(
                                        "Bar",
                                        UNDEFINED,
                                        UNDEFINED,
                                        LessEqual(Variable("Foo"), Number(30, 16)),
                                    ),
                                    Then(
                                        "Baz",
                                        UNDEFINED,
                                        UNDEFINED,
                                        Greater(Variable("Foo"), Number(30, 16)),
                                    ),
                                ],
                            ),
                            Component("Bar", "T"),
                            Component("Baz", "T"),
                        ],
                    ),
                    MessageSpec(
                        "__PACKAGE__.Simple_PDU", [Component("Bar", "T"), Component("Baz", "T")],
                    ),
                    MessageSpec("__PACKAGE__.Empty_PDU", []),
                ],
            ),
        ),
        "Type_Refinement": Specification(
            ContextSpec(["Message_Type"]),
            PackageSpec(
                "Type_Refinement",
                [
                    RefinementSpec(
                        "Message_Type.Simple_PDU",
                        "Bar",
                        "Message_Type.PDU",
                        Equal(Variable("Baz"), Number(42)),
                    ),
                    RefinementSpec("Message_Type.PDU", "Bar", "Message_Type.Simple_PDU"),
                ],
            ),
        ),
    }
    assert_specifications_files(
        [f"{TESTDIR}/message_type.rflx", f"{TESTDIR}/type_refinement.rflx"], spec
    )


def test_type_derivation_spec() -> None:
    assert_specifications_string(
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
            "Test": Specification(
                ContextSpec([]),
                PackageSpec(
                    "Test",
                    [
                        ModularInteger("__PACKAGE__.T", Number(256)),
                        MessageSpec("__PACKAGE__.Foo", [Component("N", "T")]),
                        DerivationSpec("__PACKAGE__.Bar", "Foo"),
                    ],
                ),
            )
        },
    )


def test_type_derivation_message() -> None:
    t = ModularInteger("Test.T", Number(256))

    structure = [Link(INITIAL, Field("Baz")), Link(Field("Baz"), FINAL)]

    types = {Field("Baz"): t}

    message_foo = Message("Test.Foo", structure, types)
    message_bar = DerivedMessage("Test.Bar", message_foo)

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


def test_type_derivation_refinements() -> None:
    message_foo = Message(
        "Test.Foo",
        [Link(INITIAL, Field("Baz"), length=Number(42)), Link(Field("Baz"), FINAL)],
        {Field("Baz"): Opaque()},
    )
    message_bar = DerivedMessage("Test.Bar", message_foo)

    assert_refinements_string(
        """
            package Test is
               type Foo is
                  message
                     null
                        then Baz
                           with Length => 42;
                     Baz : Opaque;
                  end message;
               for Foo use (Baz => Foo);
               type Bar is new Foo;
               for Bar use (Baz => Bar);
            end Test;
        """,
        [
            Refinement("Test", message_foo, Field("Baz"), message_foo),
            Refinement("Test", message_bar, Field("Baz"), message_bar),
        ],
    )


def test_ethernet_spec() -> None:
    spec = {
        "Ethernet": Specification(
            ContextSpec([]),
            PackageSpec(
                "Ethernet",
                [
                    ModularInteger("__PACKAGE__.Address", Pow(Number(2), Number(48))),
                    RangeInteger(
                        "__PACKAGE__.Type_Length",
                        Number(46),
                        Sub(Pow(Number(2), Number(16)), Number(1)),
                        Number(16),
                    ),
                    RangeInteger(
                        "__PACKAGE__.TPID", Number(0x8100, 16), Number(0x8100, 16), Number(16)
                    ),
                    ModularInteger("__PACKAGE__.TCI", Pow(Number(2), Number(16))),
                    MessageSpec(
                        "__PACKAGE__.Frame",
                        [
                            Component("Destination", "Address"),
                            Component("Source", "Address"),
                            Component(
                                "Type_Length_TPID",
                                "Type_Length",
                                [
                                    Then(
                                        "TPID",
                                        First("Type_Length_TPID"),
                                        UNDEFINED,
                                        Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
                                    ),
                                    Then(
                                        "Type_Length",
                                        First("Type_Length_TPID"),
                                        UNDEFINED,
                                        NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
                                    ),
                                ],
                            ),
                            Component("TPID", "TPID"),
                            Component("TCI", "TCI"),
                            Component(
                                "Type_Length",
                                "Type_Length",
                                [
                                    Then(
                                        "Payload",
                                        UNDEFINED,
                                        Mul(Variable("Type_Length"), Number(8)),
                                        LessEqual(Variable("Type_Length"), Number(1500)),
                                    ),
                                    Then(
                                        "Payload",
                                        UNDEFINED,
                                        Sub(Last("Message"), Last("Type_Length")),
                                        GreaterEqual(Variable("Type_Length"), Number(1536)),
                                    ),
                                ],
                            ),
                            Component(
                                "Payload",
                                "Opaque",
                                [
                                    Then(
                                        condition=And(
                                            GreaterEqual(
                                                Div(Length("Payload"), Number(8)), Number(46),
                                            ),
                                            LessEqual(
                                                Div(Length("Payload"), Number(8)), Number(1500),
                                            ),
                                        ),
                                    )
                                ],
                            ),
                        ],
                    ),
                ],
            ),
        )
    }

    assert_specifications_files([f"{SPECDIR}/ethernet.rflx"], spec)


def test_ethernet_message() -> None:
    assert_messages_files([f"{SPECDIR}/ethernet.rflx"], [ETHERNET_FRAME])


def test_tls() -> None:
    p = Parser()
    for f in ["tls_alert.rflx", "tls_handshake.rflx", "tls_heartbeat.rflx", "tls_record.rflx"]:
        p.parse(Path(f"{SPECDIR}/{f}"))
    p.create_model()


def test_message_with_two_length_fields() -> None:
    p = Parser()
    p.parse_string(
        """
           package Test is
              type Length is mod 2**8;
              type Packet is
                 message
                    Length_1 : Length;
                    Length_2 : Length
                       then Payload
                          with Length => Length_1 + Length_2;
                    Payload : Opaque;
                 end message;
           end Test;
        """
    )
    p.create_model()


def test_feature_integration() -> None:
    p = Parser()
    p.parse(Path(f"{TESTDIR}/feature_integration.rflx"))
    p.create_model()


def test_parsed_field_locations() -> None:
    p = Parser()
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
        Field(ID("F1", Location((6, 21), end=(6, 22)))),
        Field(ID("F2", Location((7, 21), end=(7, 22)))),
    )
