# pylint: disable=too-many-lines
import unittest
import unittest.mock
from itertools import zip_longest
from pathlib import Path
from typing import Dict, Sequence

from rflx.expression import (
    UNDEFINED,
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
from rflx.parser import grammar
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
from rflx.parser.parser import Component, ParseFatalException, Parser, ParserError
from tests.models import ETHERNET_FRAME


class TestParser(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.testdir = "tests"
        self.specdir = "specs"
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_specifications_files(
        self, filenames: Sequence[str], specifications: Dict[str, Specification]
    ) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(Path(filename))
        self.assertEqual(parser.specifications, specifications, filenames)

    def assert_specifications_string(
        self, string: str, specifications: Dict[str, Specification]
    ) -> None:
        parser = Parser()
        parser.parse_string(string)
        self.assertEqual(parser.specifications, specifications)

    def assert_messages_files(self, filenames: Sequence[str], messages: Sequence[Message]) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(Path(filename))
        model = parser.create_model()
        self.assert_messages(model.messages, messages)

    def assert_messages_string(self, string: str, messages: Sequence[Message]) -> None:
        parser = Parser()
        parser.parse_string(string)
        model = parser.create_model()
        self.assert_messages(model.messages, messages)

    def assert_messages(
        self, actual_messages: Sequence[Message], expected_messages: Sequence[Message]
    ) -> None:
        for actual, expected in zip_longest(actual_messages, expected_messages):
            self.assertEqual(actual.full_name, expected.full_name)
            self.assertEqual(actual.structure, expected.structure, expected.full_name)
            self.assertEqual(actual.types, expected.types, expected.full_name)
            self.assertEqual(actual.fields, expected.fields, expected.full_name)
        self.assertEqual(actual_messages, expected_messages)

    def assert_refinements_string(self, string: str, refinements: Sequence[Refinement]) -> None:
        parser = Parser()
        parser.parse_string(string)
        model = parser.create_model()
        self.assertEqual(model.refinements, refinements)

    def assert_parser_error(self, filenames: Sequence[str], regex: str) -> None:
        with self.assertRaisesRegex(ParserError, regex):
            parser = Parser()
            for filename in filenames:
                parser.parse(Path(filename))
            parser.create_model()

    def assert_parser_error_string(self, string: str, regex: str) -> None:
        parser = Parser()
        with self.assertRaisesRegex(ParserError, regex):
            parser.parse_string(string)
            parser.create_model()

    def assert_parse_exception_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ParseFatalException, regex):
            Parser().parse_string(string)

    def test_unexpected_exception_in_grammar(self) -> None:
        with self.assertRaisesRegex(
            ParseFatalException, r"implementation error \(division by zero\)"
        ):
            with unittest.mock.patch(
                "rflx.parser.grammar.parse_mathematical_expression",
                grammar.fatalexceptions(lambda x, y, z: 1 / 0),
            ):
                grammar.mathematical_expression().parseString("1 + 1")

    def test_numeric_literal(self) -> None:
        self.assertEqual(grammar.numeric_literal().parseString("1000")[0], Number(1000))
        self.assertEqual(grammar.numeric_literal().parseString("1_000")[0], Number(1000))
        self.assertEqual(grammar.numeric_literal().parseString("16#6664#")[0], Number(26212))
        self.assertEqual(grammar.numeric_literal().parseString("16#66_64#")[0], Number(26212))

    # ISSUE: Componolit/RecordFlux#60

    def test_unsupported_array_aggregate(self) -> None:
        with self.assertRaisesRegex(ParseFatalException, r"^unsupported array aggregate"):
            grammar.mathematical_expression().parseString("(1, 2)")

    # def test_mathematical_expression_array(self) -> None:
    #     self.assertEqual(
    #         grammar.mathematical_expression().parseString('(1, 2)')[0],
    #         Aggregate(Number(1), Number(2)))

    # def test_mathematical_expression_array_no_number(self) -> None:
    #     with self.assertRaisesRegex(ParseFatalException, r'^Expected Number'):
    #         grammar.mathematical_expression().parseString('(1, Foo)')

    # def test_mathematical_expression_array_out_of_range(self) -> None:
    #     with self.assertRaisesRegex(ParseFatalException,
    #                                 r'^Number "256" is out of range 0 .. 255'):
    #         grammar.mathematical_expression().parseString('(1, 2, 256)')

    def test_unexpected_relation_operator(self) -> None:
        with self.assertRaisesRegex(ParseFatalException, r"^unexpected relation operator"):
            grammar.parse_relation("", 0, [Number(1), "<>", Number(1)])

    def test_unexpected_logical_operator(self) -> None:
        with self.assertRaisesRegex(ParseFatalException, r"^unexpected logical operator"):
            grammar.parse_logical_expression("", 0, [[Number(1), "xor", Number(1)]])

    def test_unexpected_mathematical_operator(self) -> None:
        with self.assertRaisesRegex(ParseFatalException, r"^unexpected mathematical operator"):
            grammar.parse_mathematical_expression("", 0, [[Number(1), "//", Number(1)]])

    def test_unexpected_attribute(self) -> None:
        with self.assertRaisesRegex(ParseFatalException, r"^unexpected attribute"):
            grammar.parse_attribute("", 0, ["V", "'", "Access"])

    def test_unexpected_type(self) -> None:
        with self.assertRaisesRegex(ParseFatalException, r"^unexpected type"):
            grammar.parse_type("", 0, ["type", "T", "is", "X"])

    def test_inconsistent_package_identifiers(self) -> None:
        self.assert_parse_exception_string(
            """
                package A is
                end B;
            """,
            r"^inconsistent package identifiers",
        )

    def test_empty_file_spec(self) -> None:
        self.assert_specifications_files([f"{self.testdir}/empty_file.rflx"], {})

    def test_empty_file_message(self) -> None:
        self.assert_messages_files([f"{self.testdir}/empty_file.rflx"], [])

    def test_comment_only_spec(self) -> None:
        self.assert_specifications_files([f"{self.testdir}/comment_only.rflx"], {})

    def test_comment_only_message(self) -> None:
        self.assert_messages_files([f"{self.testdir}/comment_only.rflx"], [])

    def test_incorrect_name(self) -> None:
        self.assert_parser_error(
            [f"{self.testdir}/incorrect_name.rflx"],
            r'^file name "incorrect_name.rflx" does not match unit name "Test",'
            r' should be "test.rflx"$',
        )

    def test_incorrect_specification(self) -> None:
        self.assert_parser_error(
            [f"{self.testdir}/incorrect_specification.rflx"], r"Expected \"is\", found ';'",
        )

    def test_unexpected_exception_in_parser(self) -> None:
        parser = Parser()
        with self.assertRaisesRegex(ParserError, r"division by zero"):
            with unittest.mock.patch("rflx.parser.parser.check_types", lambda x: 1 / 0):
                parser.parse_string(
                    """
                        package Test is
                           type T is mod 256;
                        end Test;
                    """
                )
                parser.create_model()

    def test_package_spec(self) -> None:
        self.assert_specifications_files(
            [f"{self.testdir}/empty_package.rflx"],
            {"Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", []))},
        )

    def test_package_message(self) -> None:
        self.assert_messages_files([f"{self.testdir}/empty_package.rflx"], [])

    def test_duplicate_specifications(self) -> None:
        files = [f"{self.testdir}/empty_package.rflx", f"{self.testdir}/empty_package.rflx"]
        self.assert_specifications_files(
            files,
            {"Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", []))},
        )
        self.assert_messages_files(files, [])

    def test_context_spec(self) -> None:
        self.assert_specifications_files(
            [f"{self.testdir}/context.rflx"],
            {
                "Context": Specification(
                    ContextSpec(["Empty_File", "Empty_Package"]), PackageSpec("Context", [])
                ),
                "Empty_Package": Specification(ContextSpec([]), PackageSpec("Empty_Package", [])),
            },
        )

    def test_context_message(self) -> None:
        self.assert_messages_files([f"{self.testdir}/context.rflx"], [])

    def test_context_dependency_cycle(self) -> None:
        self.assert_parser_error(
            [f"{self.testdir}/context_cycle.rflx"],
            r'^dependency cycle due to context item "Context_Cycle_2" in "Context_Cycle_1"$',
        )

    def test_duplicate_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is mod 256;
                   type T is mod 256;
                end Test;
            """,
            r'duplicate type "Test.T"',
        )

    def test_message_undefined_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type PDU is
                      message
                         Foo : T;
                      end message;
                end Test;
            """,
            r'^undefined component type "Test.T" in message "Test.PDU"$',
        )

    def test_message_undefined_component(self) -> None:
        self.assert_parser_error_string(
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
            r'^undefined component "Bar" in message "Test.PDU"$',
        )

    def test_invalid_location_expression(self) -> None:
        self.assert_parse_exception_string(
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
            r'^Expected {{"First" - "=>" - MathematicalExpression} | {"Length" - "=>" -'
            r" MathematicalExpression}} \(at char 239\), \(line:8, col:38\)$",
        )

    def test_invalid_modular_type(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is mod 2**128;
                end Test;
            """,
            r'^modulus of "T" exceeds limit \(2\*\*64\)',
        )

    def test_invalid_enumeration_type_size(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is (Foo, Bar, Baz) with Size => 1;
                end Test;
            """,
            r'size for "T" too small',
        )

    def test_invalid_enumeration_type_duplicate_elements(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is (Foo, Foo) with Size => 1;
                end Test;
            """,
            r'"T" contains duplicate elements',
        )

    def test_invalid_enumeration_type_duplicate_values(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is (Foo => 0, Bar => 0) with Size => 1;
                end Test;
            """,
            r'"T" contains elements with same value',
        )

    def test_invalid_enumeration_type_identical_literals(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T1 is (Foo, Bar) with Size => 1;
                   type T2 is (Bar, Baz) with Size => 1;
                end Test;
            """,
            r'"Test.T2" contains identical literals as "Test.T1": Bar',
        )

    def test_invalid_enumeration_type_builtin_literals(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is (True, False) with Size => 1;
                end Test;
            """,
            r'"Test.T" contains identical literals as "__BUILTINS__.Boolean": False, True',
        )

    def test_array_undefined_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is array of Foo;
                end Test;
            """,
            r'^undefined element type "Test.Foo" in array "Test.T"$',
        )

    def test_array_unsupported_element_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type Foo is mod 2**4;
                   type T is array of Foo;
                end Test;
            """,
            r'unsupported size \(4\) of element type "Foo" in "T" \(no multiple of 8\)',
        )

    def test_duplicate_message(self) -> None:
        self.assert_parser_error_string(
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
            r'duplicate type "Test.PDU"',
        )

    def test_duplicate_refinement(self) -> None:
        self.assert_parser_error_string(
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
            r'^duplicate refinement of field "Foo" with "Test.PDU" for "Test.PDU"$',
        )

    def test_refinement_undefined_message(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   for PDU use (Foo => Bar);
                end Test;
            """,
            r'^undefined type "Test.PDU" in refinement$',
        )

    def test_refinement_undefined_sdu(self) -> None:
        self.assert_parser_error_string(
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
            r'^undefined type "Test.Bar" in refinement of "Test.PDU"$',
        )

    def test_refinement_invalid_field(self) -> None:
        self.assert_parser_error_string(
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
            r'^invalid field "Bar" in refinement of "Test.PDU"$',
        )

    def test_refinement_invalid_condition(self) -> None:
        self.assert_parser_error_string(
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
            r'^unknown field or literal "X" in refinement condition of "Test.PDU"$',
        )

    def test_derivation_duplicate_type(self) -> None:
        self.assert_parser_error_string(
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
            r'^duplicate type "Test.Bar"$',
        )

    def test_derivation_undefined_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type Bar is new Foo;
                end Test;
            """,
            r'^undefined message "Test.Foo" in derived message "Test.Bar"$',
        )

    def test_derivation_unsupported_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type Foo is mod 256;
                   type Bar is new Foo;
                end Test;
            """,
            r'^undefined message "Test.Foo" in derived message "Test.Bar"$',
        )

    def test_derivation_of_derived_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type Foo is null message;
                   type Bar is new Foo;
                   type Baz is new Bar;
                end Test;
            """,
            r'^illegal derivation "Test.Baz" of derived message "Test.Bar"$',
        )

    def test_invalid_first_in_initial_node(self) -> None:
        self.assert_parser_error_string(
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
            r'^invalid first expression in initial node of message "Test.PDU"$',
        )

    def test_multiple_initial_node_edges(self) -> None:
        self.assert_parse_exception_string(
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
            r'^Expected ";"',
        )

    def test_multiple_initial_nodes(self) -> None:
        self.assert_parse_exception_string(
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
            r'^reserved word "null" used as identifier',
        )

    def test_reserved_word_in_type_name(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type Type is mod 256;
                end Test;
            """,
            r'^reserved word "Type" used as identifier',
        )

    def test_reserved_word_in_message_component(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is mod 256;
                   type PDU is
                      message
                         Message : T;
                      end message;
                end Test;
            """,
            r'^Found unwanted token, "Message"',
        )

    def test_integer_type_spec(self) -> None:
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
        self.assert_specifications_files([f"{self.testdir}/integer_type.rflx"], spec)

    def test_enumeration_type_spec(self) -> None:
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
                            "__PACKAGE__.Gender",
                            {"M": Number(0), "F": Number(1)},
                            Number(1),
                            False,
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
        self.assert_specifications_files([f"{self.testdir}/enumeration_type.rflx"], spec)

    def test_array_type_spec(self) -> None:
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
        self.assert_specifications_files([f"{self.testdir}/array_type.rflx"], spec)

    def test_message_type_spec(self) -> None:
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
                                            And(
                                                Equal(Length("Foo"), Number(1)),
                                                LessEqual(Variable("Foo"), Number(30, 16)),
                                            ),
                                        ),
                                        Then(
                                            "Baz",
                                            condition=And(
                                                Equal(Length("Foo"), Number(1)),
                                                Greater(Variable("Foo"), Number(30, 16)),
                                            ),
                                        ),
                                    ],
                                ),
                                Component("Bar", "T"),
                                Component("Baz", "T"),
                            ],
                        ),
                        MessageSpec(
                            "__PACKAGE__.Simple_PDU",
                            [Component("Bar", "T"), Component("Baz", "T")],
                        ),
                        MessageSpec("__PACKAGE__.Empty_PDU", []),
                    ],
                ),
            )
        }
        self.assert_specifications_files([f"{self.testdir}/message_type.rflx"], spec)

    def test_message_type_message(self) -> None:
        simple_structure = [
            Link(INITIAL, Field("Bar")),
            Link(Field("Bar"), Field("Baz"),),
            Link(Field("Baz"), FINAL),
        ]

        simple_types = {
            Field("Bar"): ModularInteger("Message_Type.T", Number(256)),
            Field("Baz"): ModularInteger("Message_Type.T", Number(256)),
        }

        simple_message = Message("Message_Type.Simple_PDU", simple_structure, simple_types)

        structure = [
            Link(INITIAL, Field("Foo")),
            Link(
                Field("Foo"),
                Field("Bar"),
                And(Equal(Length("Foo"), Number(1)), LessEqual(Variable("Foo"), Number(30, 16))),
            ),
            Link(
                Field("Foo"),
                Field("Baz"),
                And(Equal(Length("Foo"), Number(1)), Greater(Variable("Foo"), Number(30, 16))),
            ),
            Link(Field("Bar"), Field("Baz")),
            Link(Field("Baz"), FINAL),
        ]

        types = {
            **simple_types,
            **{Field("Foo"): ModularInteger("Message_Type.T", Number(256))},
        }

        message = Message("Message_Type.PDU", structure, types)

        empty_message = Message("Message_Type.Empty_PDU", [], {})

        self.assert_messages_files(
            [f"{self.testdir}/message_type.rflx"], [message, simple_message, empty_message]
        )

    def test_message_in_message(self) -> None:
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

        derived_length_value = DerivedMessage(
            "Message_In_Message.Derived_Length_Value", length_value
        )

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

        self.assert_messages_files(
            [f"{self.testdir}/message_in_message.rflx"],
            [length_value, derived_length_value, message, derived_message],
        )

    def test_type_refinement_spec(self) -> None:
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
                                            And(
                                                Equal(Length("Foo"), Number(1)),
                                                LessEqual(Variable("Foo"), Number(30, 16)),
                                            ),
                                        ),
                                        Then(
                                            "Baz",
                                            condition=And(
                                                Equal(Length("Foo"), Number(1)),
                                                Greater(Variable("Foo"), Number(30, 16)),
                                            ),
                                        ),
                                    ],
                                ),
                                Component("Bar", "T"),
                                Component("Baz", "T"),
                            ],
                        ),
                        MessageSpec(
                            "__PACKAGE__.Simple_PDU",
                            [Component("Bar", "T"), Component("Baz", "T")],
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
                            "",
                            "Message_Type.Simple_PDU",
                            "Bar",
                            "Message_Type.PDU",
                            Equal(Variable("Baz"), Number(42)),
                        ),
                        RefinementSpec("", "Message_Type.PDU", "Bar", "Message_Type.Simple_PDU",),
                    ],
                ),
            ),
        }
        self.assert_specifications_files(
            [f"{self.testdir}/message_type.rflx", f"{self.testdir}/type_refinement.rflx"], spec
        )

    def test_type_derivation_spec(self) -> None:
        self.assert_specifications_string(
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

    def test_type_derivation_message(self) -> None:
        t = ModularInteger("Test.T", Number(256))

        structure = [Link(INITIAL, Field("Baz")), Link(Field("Baz"), FINAL)]

        types = {Field("Baz"): t}

        message_foo = Message("Test.Foo", structure, types)
        message_bar = DerivedMessage("Test.Bar", message_foo)

        self.assert_messages_string(
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

    def test_type_derivation_refinements(self) -> None:
        message_foo = Message(
            "Test.Foo",
            [Link(INITIAL, Field("Baz"), length=Number(42)), Link(Field("Baz"), FINAL)],
            {Field("Baz"): Opaque()},
        )
        message_bar = DerivedMessage("Test.Bar", message_foo)

        self.assert_refinements_string(
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

    def test_ethernet_spec(self) -> None:
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
                                            NotEqual(
                                                Variable("Type_Length_TPID"), Number(33024, 16)
                                            ),
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
                                            "null",
                                            UNDEFINED,
                                            UNDEFINED,
                                            And(
                                                GreaterEqual(
                                                    Div(Length("Payload"), Number(8)), Number(46)
                                                ),
                                                LessEqual(
                                                    Div(Length("Payload"), Number(8)), Number(1500)
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

        self.assert_specifications_files([f"{self.specdir}/ethernet.rflx"], spec)

    def test_ethernet_message(self) -> None:
        self.assert_messages_files([f"{self.specdir}/ethernet.rflx"], [ETHERNET_FRAME])

    def test_tls(self) -> None:
        parser = Parser()
        for f in ["tls_alert.rflx", "tls_handshake.rflx", "tls_heartbeat.rflx", "tls_record.rflx"]:
            parser.parse(Path(f"{self.specdir}/{f}"))
        parser.create_model()

    @staticmethod
    def test_message_with_two_length_fields() -> None:
        parser = Parser()
        parser.parse_string(
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
        parser.create_model()

    def test_feature_integration(self) -> None:
        parser = Parser()
        parser.parse(Path(f"{self.testdir}/feature_integration.rflx"))
        parser.create_model()
