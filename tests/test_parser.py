import unittest
from typing import Dict, List

from rflx.parser import (FINAL, PDU, And, Array, Component, Context, Div, Edge, Enumeration, Equal,
                         First, GreaterEqual, InitialNode, Last, Length, LessEqual, Message,
                         ModularInteger, Mul, Node, NotEqual, Number, Package, ParseFatalException,
                         Parser, ParserError, Pow, RangeInteger, Reference, Refinement,
                         Specification, Sub, Then, Value)
from tests.models import ETHERNET_PDU


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

    def assert_pdus(self, filenames: List[str], pdus: List[PDU]) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(filename)
        self.assertEqual(parser.pdus, pdus, filenames)

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

    def test_empty_file_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/empty_file.rflx'], {})

    def test_empty_file_pdu(self) -> None:
        self.assert_pdus([f'{self.testdir}/empty_file.rflx'], [])

    def test_comment_only_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/comment_only.rflx'], {})

    def test_comment_only_pdu(self) -> None:
        self.assert_pdus([f'{self.testdir}/comment_only.rflx'], [])

    def test_package_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/package.rflx'],
                                   {'Test': Specification(Context([]),
                                                          Package('Test', []))})

    def test_package_pdu(self) -> None:
        self.assert_pdus([f'{self.testdir}/package.rflx'], [])

    def test_context_spec(self) -> None:
        self.assert_specifications([f'{self.testdir}/context.rflx'],
                                   {'Test': Specification(Context(['Foo', 'Bar']),
                                                          Package('Test', []))})

    def test_context_pdu(self) -> None:
        self.assert_pdus([f'{self.testdir}/context.rflx'], [])

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

    def test_reference_to_undefined_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type PDU is
                      message
                         Foo : T;
                      end message;
                end Test;
            """,
            r'reference to undefined type "T"')

    def test_unsupported_type_in_message(self) -> None:
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

    def test_reference_to_undefined_node(self) -> None:
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
            r'reference to undefined node "Bar"')

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
            r'modulus of "T" exceeds limit \(2\*\*64\) .*')

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

    def test_array_reference_to_undefined_type(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is array of Foo;
                end Test;
            """,
            r'reference to undefined type "Foo" in "T"')

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
                   type In_PDU is new PDU (Foo => PDU);
                   type In_PDU is new Test.PDU (Foo => Test.PDU);
                end Test;
            """,
            r'duplicate refinement "In_PDU"')

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
            r'^Expected ";" \(at char 198\), \(line:7, col:37\)$')

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
            r'^reserved word "null" used as identifier \(at char 225\), \(line:8, col:26\)$')

    def test_reserved_word_in_type_name(self) -> None:
        self.assert_parse_exception_string(
            """
                package Test is
                   type Type is mod 256;
                end Test;
            """,
            r'^reserved word "Type" used as identifier \(at char 57\), \(line:3, col:25\)$')

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
            r'^Found unwanted token, "Message" \(at char 157\), \(line:6, col:26\)$')

    def test_integer_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [RangeInteger('Page_Num', Number(1), Number(2000), Number(16)),
                     RangeInteger('Line_Size', Number(0), Number(255), Number(8)),
                     ModularInteger('Byte', Number(256)),
                     ModularInteger('Hash_Index', Number(64))]))}
        self.assert_specifications([f'{self.testdir}/integer_type.rflx'], spec)

    def test_enumeration_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
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
            Context([]),
            Package('Test',
                    [ModularInteger('Byte', Number(256)),
                     Array('Bytes', Reference('Byte')),
                     Message('Foo', [Component('Byte', 'Byte')]),
                     Array('Bar', Reference('Foo'))]))}
        self.assert_specifications([f'{self.testdir}/array_type.rflx'], spec)

    def test_message_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [ModularInteger('T', Number(256)),
                     Message('PDU',
                             [Component('null', '', [
                                 Then('Foo',
                                      None,
                                      Number(1))]),
                              Component('Foo', 'T', [
                                  Then('Bar',
                                       Number(1),
                                       Number(1),
                                       And(Equal(Length('Foo'),
                                           Number(1)),
                                           LessEqual(Value('Foo'),
                                                     Number(30)))),
                                  Then('Baz')]),
                              Component('Bar', 'T'),
                              Component('Baz', 'T')]),
                     Message('Simple_PDU',
                             [Component('Bar', 'T'),
                              Component('Baz', 'T')])]))}
        self.assert_specifications([f'{self.testdir}/message_type.rflx'], spec)

    def test_message_type_pdu(self) -> None:
        t = ModularInteger('T', Number(256))

        initial = InitialNode()
        simple_initial = InitialNode()
        pdu_foo = Node('Foo', t)
        pdu_bar = Node('Bar', t)
        pdu_baz = Node('Baz', t)

        initial.edges = [Edge(pdu_foo,
                              length=Number(1))]
        simple_initial.edges = [Edge(pdu_bar)]
        pdu_foo.edges = [Edge(pdu_bar,
                              And(Equal(Length('Foo'),
                                        Number(1)),
                                  LessEqual(Value('Foo'),
                                            Number(30))),
                              Number(1),
                              Number(1)),
                         Edge(pdu_baz)]
        pdu_bar.edges = [Edge(pdu_baz)]
        pdu_baz.edges = [Edge(FINAL)]

        pdus = [PDU('Test.PDU', initial),
                PDU('Test.Simple_PDU', simple_initial)]

        self.assert_pdus([f'{self.testdir}/message_type.rflx'], pdus)

    def test_type_refinement_spec(self) -> None:
        spec = {
            'Test': Specification(
                Context([]),
                Package('Test',
                        [ModularInteger('T', Number(256)),
                         Message('PDU',
                                 [Component('null', '', [
                                     Then('Foo',
                                          None,
                                          Number(1))]),
                                  Component('Foo', 'T', [
                                      Then('Bar',
                                           Number(1),
                                           Number(1),
                                           And(Equal(Length('Foo'),
                                               Number(1)),
                                               LessEqual(Value('Foo'),
                                                         Number(30)))),
                                      Then('Baz')]),
                                  Component('Bar', 'T'),
                                  Component('Baz', 'T')]),
                         Message('Simple_PDU',
                                 [Component('Bar', 'T'),
                                  Component('Baz', 'T')])])),
            'In_Test': Specification(
                Context(['Test']),
                Package('In_Test',
                        [Refinement('PDU_In_Simple_PDU',
                                    'Test.Simple_PDU',
                                    'Bar',
                                    'Test.PDU',
                                    Equal(Value('Baz'), Number(42))),
                         Refinement('Simple_PDU_In_PDU',
                                    'Test.PDU',
                                    'Bar',
                                    'Test.Simple_PDU'),
                         Refinement('Null_In_Simple_PDU',
                                    'Test.Simple_PDU',
                                    'Bar',
                                    'null')]))}
        self.assert_specifications([f'{self.testdir}/message_type.rflx',
                                    f'{self.testdir}/type_refinement.rflx'], spec)

    def test_ethernet_spec(self) -> None:
        spec = {'Ethernet': Specification(
            Context([]),
            Package('Ethernet',
                    [RangeInteger('UINT16',
                                  Number(0),
                                  Sub(Pow(Number(2), Number(16)), Number(1)),
                                  Number(16)),
                     ModularInteger('UINT48', Pow(Number(2), Number(48))),
                     Message('Frame',
                             [Component('Destination', 'UINT48'),
                              Component('Source', 'UINT48'),
                              Component('TPID', 'UINT16', [
                                  Then('TCI',
                                       None,
                                       None,
                                       Equal(Value('TPID'),
                                             Number(33024))),
                                  Then('EtherType',
                                       First('TPID'),
                                       None,
                                       NotEqual(Value('TPID'),
                                                Number(33024)))]),
                              Component('TCI', 'UINT16'),
                              Component('EtherType', 'UINT16', [
                                  Then('Payload',
                                       None,
                                       Mul(Value('EtherType'),
                                           Number(8)),
                                       LessEqual(Value('EtherType'),
                                                 Number(1500))),
                                  Then('Payload',
                                       None,
                                       Sub(Last('Message'),
                                           Last('EtherType')),
                                       GreaterEqual(Value('EtherType'),
                                                    Number(1536)))]),
                              Component('Payload', 'Payload_Array', [
                                  Then('null',
                                       None,
                                       None,
                                       And(GreaterEqual(Div(Length('Payload'),
                                                            Number(8)),
                                                        Number(46)),
                                           LessEqual(Div(Length('Payload'),
                                                         Number(8)),
                                                     Number(1500))))])])]))}

        self.assert_specifications([f'{self.specdir}/ethernet.rflx'], spec)

    def test_ethernet_pdu(self) -> None:
        self.assert_pdus([f'{self.specdir}/ethernet.rflx'], [ETHERNET_PDU])
