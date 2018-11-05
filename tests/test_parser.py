import unittest
from typing import Dict, List

from parser import (And, Component, Context, Div, Edge, Enumeration, Equal, FINAL, First,
                    GreaterEqual, Last, Length, LessEqual, Message, ModularInteger, Mul, Number,
                    Node, NotEqual, Package, Parser, ParseFatalException, ParserError, PDU, Pow,
                    RangeInteger, Refinement, Specification, Sub, Then, Value)

from tests.models import ETHERNET_PDU


class TestParser(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def fullpath(self, testfile: str) -> str:
        return self.testdir + "/" + testfile

    def assert_specifications(self, filenames: List[str],
                              specifications: Dict[str, Specification]) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(self.fullpath(filename))
        self.assertEqual(parser.specifications(), specifications, filenames)

    def assert_pdus(self, filenames: List[str], pdus: List[PDU]) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(self.fullpath(filename))
        self.assertEqual(parser.pdus, pdus, filenames)

    def assert_parser_error(self, filenames: List[str], regex: str) -> None:
        with self.assertRaisesRegex(ParserError, regex):
            parser = Parser()
            for filename in filenames:
                parser.parse(self.fullpath(filename))

    def assert_parser_error_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ParserError, regex):
            Parser().parse_string(string)

    def assert_parse_exception_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ParseFatalException, regex):
            Parser().parse_string(string)

    def test_empty_file_spec(self) -> None:
        self.assert_specifications(['empty_file.rflx'], {})

    def test_empty_file_pdu(self) -> None:
        self.assert_pdus(['empty_file.rflx'], [])

    def test_comment_only_spec(self) -> None:
        self.assert_specifications(['comment_only.rflx'], {})

    def test_comment_only_pdu(self) -> None:
        self.assert_pdus(['comment_only.rflx'], [])

    def test_package_spec(self) -> None:
        self.assert_specifications(['package.rflx'], {'Test': Specification(Context([]),
                                                                            Package('Test', []))})

    def test_package_pdu(self) -> None:
        self.assert_pdus(['package.rflx'], [])

    def test_context_spec(self) -> None:
        self.assert_specifications(['context.rflx'], {'Test': Specification(Context(['Foo', 'Bar']),
                                                                            Package('Test', []))})

    def test_context_pdu(self) -> None:
        self.assert_pdus(['context.rflx'], [])

    def test_duplicate_package(self) -> None:
        self.assert_parser_error(['package.rflx', 'package.rflx'],
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

    def test_invalid_location_expression_no_conjunction(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is mod 256;
                   type PDU is
                      message
                         Foo : T
                            then Bar
                                with First = 1 or Length = 1;
                        Bar : T;
                      end message;
                end Test;
            """,
            r'unexpected "or" in "\(First = 1 or Length = 1\)"')

    def test_invalid_location_expression_no_equation(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is mod 256;
                   type PDU is
                      message
                         Foo : T
                            then Bar
                                with First < 1 and Length = 1;
                        Bar : T;
                      end message;
                end Test;
            """,
            r'expected "=" instead of "<" in "First < 1"')

    def test_invalid_location_expression_no_equation_2(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is mod 256;
                   type PDU is
                      message
                         Foo : T
                            then Bar
                                with First < 1;
                        Bar : T;
                      end message;
                end Test;
            """,
            r'unexpected "<" in "First < 1"')

    def test_invalid_location_expression_no_attribute(self) -> None:
        self.assert_parser_error_string(
            """
                package Test is
                   type T is mod 256;
                   type PDU is
                      message
                         Foo : T
                            then Bar
                                with Foo = 1;
                        Bar : T;
                      end message;
                end Test;
            """,
            r'expected "First" or "Length" instead of "Foo"')

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
            r'duplicate message "PDU"')

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

    def test_integer_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [RangeInteger('Page_Num', Number(1), Number(2000), Number(16)),
                     RangeInteger('Line_Size', Number(0), Number(255), Number(8)),
                     ModularInteger('Byte', Number(256)),
                     ModularInteger('Hash_Index', Number(64))]))}
        self.assert_specifications(['integer_type.rflx'], spec)

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
                                 Number(3)),
                     Enumeration('Gender',
                                 {'M': Number(0),
                                  'F': Number(1)},
                                 Number(1))]))}
        self.assert_specifications(['enumeration_type.rflx'], spec)

    def test_message_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [ModularInteger('T', Number(256)),
                     Message('PDU',
                             [Component('Foo', 'T', [
                                 Then('Bar',
                                      And(Equal(Value('First'), Number(1)),
                                          Equal(Value('Length'), Number(1))),
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
        self.assert_specifications(['message_type.rflx'], spec)

    def test_message_type_pdu(self) -> None:
        t = ModularInteger('T', Number(256))

        pdu_foo = Node('Foo', t)
        pdu_bar = Node('Bar', t)
        pdu_baz = Node('Baz', t)

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

        pdus = [PDU('Test.PDU', pdu_foo),
                PDU('Test.Simple_PDU', pdu_bar)]

        self.assert_pdus(['message_type.rflx'], pdus)

    def test_type_refinement_spec(self) -> None:
        spec = {
            'Test': Specification(
                Context([]),
                Package('Test',
                        [ModularInteger('T', Number(256)),
                         Message('PDU',
                                 [Component('Foo', 'T', [
                                     Then('Bar',
                                          And(Equal(Value('First'), Number(1)),
                                              Equal(Value('Length'), Number(1))),
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
                                    'Test.Simple_PDU')]))}
        self.assert_specifications(['message_type.rflx', 'type_refinement.rflx'], spec)

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
                                       Equal(Value('TPID'),
                                             Number(33024))),
                                  Then('EtherType',
                                       Equal(Value('First'),
                                             First('TPID')),
                                       NotEqual(Value('TPID'),
                                                Number(33024)))]),
                              Component('TCI', 'UINT16'),
                              Component('EtherType', 'UINT16', [
                                  Then('Payload',
                                       Equal(Value('Length'),
                                             Mul(Value('EtherType'),
                                                 Number(8))),
                                       LessEqual(Value('EtherType'),
                                                 Number(1500))),
                                  Then('Payload',
                                       Equal(Value('Length'),
                                             Sub(Last('Message'),
                                                 Last('EtherType'))),
                                       GreaterEqual(Value('EtherType'),
                                                    Number(1536)))]),
                              Component('Payload', 'Payload_Array', [
                                  Then('null',
                                       None,
                                       And(GreaterEqual(Div(Length('Payload'),
                                                            Number(8)),
                                                        Number(46)),
                                           LessEqual(Div(Length('Payload'),
                                                         Number(8)),
                                                     Number(1500))))])])]))}

        self.assert_specifications(['ethernet.rflx'], spec)

    def test_ethernet_pdu(self) -> None:
        self.assert_pdus(['ethernet.rflx'], [ETHERNET_PDU])


if __name__ == "__main__":
    unittest.main()
