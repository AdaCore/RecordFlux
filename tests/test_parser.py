import unittest
from itertools import zip_longest
from typing import Dict, List

from rflx.parser import (FINAL, INITIAL, UNDEFINED, And, Array, Component, ContextSpec,
                         DerivationSpec, DerivedMessage, Div, Enumeration, Equal, Field, First,
                         GreaterEqual, Last, Length, LessEqual, Link, Message, MessageSpec,
                         ModularInteger, Mul, NotEqual, Number, PackageSpec, ParseFatalException,
                         Parser, ParserError, Pow, RangeInteger, Reference, Refinement,
                         Specification, Sub, Then, Variable)
from tests.models import ETHERNET_FRAME


class TestParser(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.testdir = "tests"
        self.specdir = "specs"
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_specifications(self, filenames: List[str],
                              specifications: Dict[str, Specification]) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(filename)
        self.assertEqual(parser.specifications(), specifications, filenames)

    def assert_specifications_string(self, string: str,
                                     specifications: Dict[str, Specification]) -> None:
        parser = Parser()
        parser.parse_string(string)
        self.assertEqual(parser.specifications(), specifications)

    def assert_messages_files(self, filenames: List[str], messages: List[Message]) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(filename)
        self.assert_messages(parser.messages, messages)

    def assert_messages_string(self, string: str, messages: List[Message]) -> None:
        parser = Parser()
        parser.parse_string(string)
        self.assert_messages(parser.messages, messages)

    def assert_messages(self, actual_messages: List[Message],
                        expected_messages: List[Message]) -> None:
        for actual, expected in zip_longest(actual_messages, expected_messages):
            self.assertEqual(actual.full_name, expected.full_name)
            self.assertEqual(actual.structure, expected.structure, expected.full_name)
            self.assertEqual(actual.types, expected.types, expected.full_name)
            self.assertEqual(actual.fields, expected.fields, expected.full_name)
        self.assertEqual(actual_messages, expected_messages)

    def assert_refinements_string(self, string: str, refinements: List[Refinement]) -> None:
        parser = Parser()
        parser.parse_string(string)
        self.assertEqual(parser.refinements, refinements)

    def assert_parser_error(self, filenames: List[str], regex: str) -> None:
        with self.assertRaisesRegex(ParserError, regex):
            parser = Parser()
            for filename in filenames:
                parser.parse(filename)

    def assert_parser_error_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ParserError, regex):
            Parser().parse_string(string)

    def assert_parse_exception_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ParseFatalException, regex):
            Parser().parse_string(string)

    # ISSUE: Componolit/RecordFlux#60

    # def test_mathematical_expression_array(self) -> None:
    #     self.assertEqual(
    #         Parser.mathematical_expression().parseString('(1, 2)')[0],
    #         Aggregate(Number(1), Number(2)))

    # def test_mathematical_expression_array_no_number(self) -> None:
    #     with self.assertRaisesRegex(ParseFatalException, r'^Expected Number'):
    #         Parser.mathematical_expression().parseString('(1, Foo)')

    # def test_mathematical_expression_array_out_of_range(self) -> None:
    #     with self.assertRaisesRegex(ParseFatalException,
    #                                 r'^Number "256" is out of range 0 .. 255'):
    #         Parser.mathematical_expression().parseString('(1, 2, 256)')

    def test_empty_file_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/empty_file.rflx'], {})

    def test_empty_file_message(self) -> None:
        self.assert_messages_files([f'{self.testdir}/empty_file.rflx'], [])

    def test_comment_only_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/comment_only.rflx'], {})

    def test_comment_only_message(self) -> None:
        self.assert_messages_files([f'{self.testdir}/comment_only.rflx'], [])

    def test_package_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/package.rflx'],
                                   {'Test': Specification(ContextSpec([]),
                                                          PackageSpec('Test', []))})

    def test_package_message(self) -> None:
        self.assert_messages_files([f'{self.testdir}/package.rflx'], [])

    def test_context_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/context.rflx'],
                                   {'Test': Specification(ContextSpec(['Foo', 'Bar']),
                                                          PackageSpec('Test', []))})

    def test_context_message(self) -> None:
        self.assert_messages_files([f'{self.testdir}/context.rflx'], [])

    def test_duplicate_package(self) -> None:
        self.assert_parser_error([f'{self.testdir}/package.rflx', f'{self.testdir}/package.rflx'],
                                 r'duplicate package')

    def test_duplicate_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is mod 256;
                   type T is mod 256;
                end Test;
            """,
            r'duplicate type "T"')

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
            r'^undefined type "T" in "PDU"$')

    def test_message_unsupported_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is mod 256;
                   type M is
                      message
                         Foo : T;
                      end message;
                   type PDU is
                      message
                         Bar : M;
                      end message;
                end Test;
            """,
            r'^unsupported type "M" in "PDU"$')

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
            r'^undefined component "Bar" in "PDU"$')

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
            r' MathematicalExpression}} \(at char 239\), \(line:8, col:38\)$')

    def test_invalid_modular_type(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is mod 2**128;
                end Test;
            """,
            r'^modulus of "T" exceeds limit \(2\*\*64\)')

    def test_invalid_enumeration_type_size(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is (Foo, Bar, Baz) with Size => 1;
                end Test;
            """,
            r'size for "T" too small')

    def test_invalid_enumeration_type_duplicate_elements(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is (Foo, Foo) with Size => 1;
                end Test;
            """,
            r'"T" contains duplicate elements')

    def test_invalid_enumeration_type_duplicate_values(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type T is (Foo => 0, Bar => 0) with Size => 1;
                end Test;
            """,
            r'"T" contains elements with same value')

    def test_array_undefined_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is array of Foo;
                end Test;
            """,
            r'^undefined type "Foo" in "T"$')

    def test_array_unsupported_element_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type Foo is mod 2**4;
                   type T is array of Foo;
                end Test;
            """,
            r'unsupported size \(4\) of element type "Foo" in "T" \(no multiple of 8\)')

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
            r'duplicate type "PDU"')

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
            r'^duplicate refinement of field "Foo" with "PDU" in "PDU"$')

    def test_refinement_undefined_message(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   for PDU use (Foo => Bar);
                end Test;
            """,
            r'^undefined type "PDU" in refinement$')

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
            r'^undefined type "Bar" in refinement of "PDU"$')

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
            r'^invalid field "Bar" in refinement of "PDU"$')

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
            r'^unknown field or literal "X" in refinement condition of "PDU"$')

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
            r'^duplicate type "Bar"$')

    def test_derivation_undefined_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type Bar is new Foo;
                end Test;
            """,
            r'^undefined type "Foo" in "Bar"$')

    def test_derivation_unsupported_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type Foo is mod 256;
                   type Bar is new Foo;
                end Test;
            """,
            r'^unsupported type "Foo" in "Bar"$')

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
            r'^invalid first expression in initial node in "PDU"$')

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
            r'^Expected ";"')

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
            r'^reserved word "null" used as identifier')

    def test_reserved_word_in_type_name(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type Type is mod 256;
                end Test;
            """,
            r'^reserved word "Type" used as identifier')

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
            r'^Found unwanted token, "Message"')

    def test_integer_type_spec(self) -> None:
        spec = {'Test': Specification(
            ContextSpec([]),
            PackageSpec('Test',
                        [RangeInteger('Page_Num', Number(1), Number(2000), Number(16)),
                         RangeInteger('Line_Size', Number(0), Number(255), Number(8)),
                         ModularInteger('Byte', Number(256)),
                         ModularInteger('Hash_Index', Number(64))]))}
        self.assert_specifications([f'{self.testdir}/integer_type.rflx'], spec)

    def test_enumeration_type_spec(self) -> None:
        spec = {'Test': Specification(
            ContextSpec([]),
            PackageSpec('Test',
                        [Enumeration('Day',
                                     {'Mon': Number(1),
                                      'Tue': Number(2),
                                      'Wed': Number(3),
                                      'Thu': Number(4),
                                      'Fri': Number(5),
                                      'Sat': Number(6),
                                      'Sun': Number(7)},
                                     Number(3),
                                     False),
                         Enumeration('Gender',
                                     {'M': Number(0),
                                      'F': Number(1)},
                                     Number(1),
                                     False),
                         Enumeration('Priority',
                                     {'LOW': Number(1),
                                      'MEDIUM': Number(4),
                                      'HIGH': Number(7)},
                                     Number(3),
                                     True)]))}
        self.assert_specifications([f'{self.testdir}/enumeration_type.rflx'], spec)

    def test_array_type_spec(self) -> None:
        spec = {'Test': Specification(
            ContextSpec([]),
            PackageSpec('Test',
                        [ModularInteger('Byte', Number(256)),
                         Array('Bytes', Reference('Byte')),
                         MessageSpec('Foo', [Component('Byte', 'Byte')]),
                         Array('Bar', Reference('Foo'))]))}
        self.assert_specifications([f'{self.testdir}/array_type.rflx'], spec)

    def test_message_type_spec(self) -> None:
        spec = {'Test': Specification(
            ContextSpec([]),
            PackageSpec('Test',
                        [ModularInteger('T', Number(256)),
                         MessageSpec('PDU',
                                     [Component('null', '', [
                                         Then('Foo',
                                              UNDEFINED,
                                              Number(1))]),
                                      Component('Foo', 'T', [
                                          Then('Bar',
                                               Number(1),
                                               Number(1),
                                               And(Equal(Length('Foo'),
                                                   Number(1)),
                                                   LessEqual(Variable('Foo'),
                                                             Number(30)))),
                                          Then('Baz')]),
                                      Component('Bar', 'T'),
                                      Component('Baz', 'T')]),
                         MessageSpec('Simple_PDU',
                                     [Component('Bar', 'T'),
                                      Component('Baz', 'T')]),
                         MessageSpec('Empty_PDU', [])]))}
        self.assert_specifications([f'{self.testdir}/message_type.rflx'], spec)

    def test_message_type_message(self) -> None:
        simple_structure = [
            Link(INITIAL, Field('Bar')),
            Link(Field('Bar'), Field('Baz'), ),
            Link(Field('Baz'), FINAL)
        ]

        simple_types = {
            Field('Bar'): ModularInteger('T', Number(256)),
            Field('Baz'): ModularInteger('T', Number(256))
        }

        simple_message = Message('Test.Simple_PDU', simple_structure, simple_types)

        structure = [
            Link(INITIAL, Field('Foo'), length=Number(1)),
            Link(Field('Foo'), Field('Bar'),
                 And(Equal(Length('Foo'),
                           Number(1)),
                     LessEqual(Variable('Foo'),
                               Number(30))),
                 Number(1),
                 Number(1)),
            Link(Field('Foo'), Field('Baz')),
            Link(Field('Bar'), Field('Baz')),
            Link(Field('Baz'), FINAL)
        ]

        types = {
            **simple_types,
            **{Field('Foo'): ModularInteger('T', Number(256))},
        }

        message = Message('Test.PDU', structure, types)

        empty_message = Message('Test.Empty_PDU', [], {})

        self.assert_messages_files([f'{self.testdir}/message_type.rflx'],
                                   [message, simple_message, empty_message])

    def test_type_refinement_spec(self) -> None:
        spec = {
            'Test': Specification(
                ContextSpec([]),
                PackageSpec('Test',
                            [ModularInteger('T', Number(256)),
                             MessageSpec('PDU',
                                         [Component('null', '', [
                                             Then('Foo',
                                                  UNDEFINED,
                                                  Number(1))]),
                                          Component('Foo', 'T', [
                                              Then('Bar',
                                                   Number(1),
                                                   Number(1),
                                                   And(Equal(Length('Foo'),
                                                       Number(1)),
                                                       LessEqual(Variable('Foo'),
                                                                 Number(30)))),
                                              Then('Baz')]),
                                          Component('Bar', 'T'),
                                          Component('Baz', 'T')]),
                             MessageSpec('Simple_PDU',
                                         [Component('Bar', 'T'),
                                          Component('Baz', 'T')]),
                             MessageSpec('Empty_PDU', [])])),
            'In_Test': Specification(
                ContextSpec(['Test']),
                PackageSpec('In_Test',
                            [Refinement('',
                                        'Test.Simple_PDU',
                                        Field('Bar'),
                                        'Test.PDU',
                                        Equal(Variable('Baz'), Number(42))),
                             Refinement('',
                                        'Test.PDU',
                                        Field('Bar'),
                                        'Test.Simple_PDU')]))}
        self.assert_specifications([f'{self.testdir}/message_type.rflx',
                                    f'{self.testdir}/type_refinement.rflx'], spec)

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
            {'Test': Specification(
                ContextSpec([]),
                PackageSpec('Test',
                            [ModularInteger('T', Number(256)),
                             MessageSpec('Foo',
                                         [Component('N', 'T')]),
                             DerivationSpec('Bar', 'Foo')]))})

    def test_type_derivation_message(self) -> None:
        t = ModularInteger('T', Number(256))

        structure = [
            Link(INITIAL, Field('Baz')),
            Link(Field('Baz'), FINAL)
        ]

        types = {
            Field('Baz'): t
        }

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
            [Message('Test.Foo', structure, types),
             DerivedMessage('Test.Bar', 'Test.Foo', structure, types)])

    def test_type_derivation_refinements(self) -> None:
        self.assert_refinements_string(
            """
                package Test is
                   type Foo is
                      message
                         null
                            then Baz
                               with Length => 42;
                         Baz : Payload_Type;
                      end message;
                   for Foo use (Baz => Foo);
                   type Bar is new Foo;
                   for Bar use (Baz => Bar);
                end Test;
            """,
            [Refinement('Test', 'Test.Foo', Field('Baz'), 'Test.Foo'),
             Refinement('Test', 'Test.Bar', Field('Baz'), 'Test.Foo'),
             Refinement('Test', 'Test.Bar', Field('Baz'), 'Test.Bar')])

    def test_ethernet_spec(self) -> None:
        spec = {'Ethernet': Specification(
            ContextSpec([]),
            PackageSpec('Ethernet',
                        [ModularInteger('Address_Type', Pow(Number(2), Number(48))),
                         RangeInteger('Type_Length_Type',
                                      Number(46),
                                      Sub(Pow(Number(2), Number(16)), Number(1)),
                                      Number(16)),
                         RangeInteger('TPID_Type',
                                      Number(0x8100),
                                      Number(0x8100),
                                      Number(16)),
                         ModularInteger('TCI_Type',
                                        Pow(Number(2), Number(16))),
                         MessageSpec('Frame',
                                     [Component('Destination', 'Address_Type'),
                                      Component('Source', 'Address_Type'),
                                      Component('Type_Length_TPID', 'Type_Length_Type', [
                                          Then('TPID',
                                               First('Type_Length_TPID'),
                                               UNDEFINED,
                                               Equal(Variable('Type_Length_TPID'),
                                                     Number(33024))),
                                          Then('Type_Length',
                                               First('Type_Length_TPID'),
                                               UNDEFINED,
                                               NotEqual(Variable('Type_Length_TPID'),
                                                        Number(33024)))]),
                                      Component('TPID', 'TPID_Type'),
                                      Component('TCI', 'TCI_Type'),
                                      Component('Type_Length', 'Type_Length_Type', [
                                          Then('Payload',
                                               UNDEFINED,
                                               Mul(Variable('Type_Length'),
                                                   Number(8)),
                                               LessEqual(Variable('Type_Length'),
                                                         Number(1500))),
                                          Then('Payload',
                                               UNDEFINED,
                                               Sub(Last('Message'),
                                                   Last('Type_Length')),
                                               GreaterEqual(Variable('Type_Length'),
                                                            Number(1536)))]),
                                      Component('Payload', 'Payload_Type', [
                                          Then('null',
                                               UNDEFINED,
                                               UNDEFINED,
                                               And(GreaterEqual(Div(Length('Payload'),
                                                                    Number(8)),
                                                                Number(46)),
                                                   LessEqual(Div(Length('Payload'),
                                                                 Number(8)),
                                                             Number(1500))))])])]))}

        self.assert_specifications([f'{self.specdir}/ethernet.rflx'], spec)

    def test_ethernet_message(self) -> None:
        self.assert_messages_files([f'{self.specdir}/ethernet.rflx'], [ETHERNET_FRAME])
