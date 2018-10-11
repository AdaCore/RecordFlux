import unittest
from typing import Dict, List

from parser import (And, Component, Context, Enumeration, Equal, First, GreaterEqual, Last, Length,
                    LessEqual, Message, ModularInteger, Mul, Number, NotEqual, Package, Parser,
                    ParserError, PDU, RangeInteger, Specification, Sub, Then, Value)

from tests.models import ETHERNET_PDU


class TestParser(unittest.TestCase):
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

    def assert_pdus(self, filenames: List[str], pdus: Dict[str, PDU]) -> None:
        parser = Parser()
        for filename in filenames:
            parser.parse(self.fullpath(filename))
        self.assertEqual(parser.pdus(), pdus, filenames)

    def assert_parser_error(self, filenames: List[str]) -> None:
        with self.assertRaises(ParserError):
            parser = Parser()
            for filename in filenames:
                parser.parse(self.fullpath(filename))

    def test_empty_file_spec(self) -> None:
        self.assert_specifications(['empty_file.rflx'], {})

    def test_empty_file_pdu(self) -> None:
        self.assert_pdus(['empty_file.rflx'], {})

    def test_comment_only_spec(self) -> None:
        self.assert_specifications(['comment_only.rflx'], {})

    def test_comment_only_pdu(self) -> None:
        self.assert_pdus(['comment_only.rflx'], {})

    def test_package_spec(self) -> None:
        self.assert_specifications(['package.rflx'], {'Test': Specification(Context([]),
                                                                            Package('Test', []))})

    def test_package_pdu(self) -> None:
        self.assert_pdus(['package.rflx'], {})

    def test_context_spec(self) -> None:
        self.assert_specifications(['context.rflx'], {'Test': Specification(Context(['Foo', 'Bar']),
                                                                            Package('Test', []))})

    def test_context_pdu(self) -> None:
        self.assert_pdus(['context.rflx'], {})

    def test_duplicate_package(self) -> None:
        self.assert_parser_error(["package.rflx", "package.rflx"])

    # def test_derived_type(self) -> None:
    #     spec = {'Test': Specification(
    #         Context([]),
    #         Package('Test',
    #                 [Type('Counter',
    #                       Derived(Name('Positive'))),
    #                  Type('PDU_X',
    #                       Derived(Name('PDU'), {'Payload_Type': 'X'}))]))}
    #     self.assert_specifications(['derived_type.rflx'], spec)

    def test_integer_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [RangeInteger('Page_Num', 1, 2000, 16),
                     RangeInteger('Line_Size', 0, 255, 8),
                     ModularInteger('Byte', 256),
                     ModularInteger('Hash_Index', 64)]))}
        self.assert_specifications(['integer_type.rflx'], spec)

    def test_enumeration_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [Enumeration('Day',
                                 ['Mon',
                                  'Tue',
                                  'Wed',
                                  'Thu',
                                  'Fri',
                                  'Sat',
                                  'Sun']),
                     Enumeration('Gender',
                                 ['M',
                                  'F'])]))}
        self.assert_specifications(['enumeration_type.rflx'], spec)

    def test_message_type_spec(self) -> None:
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [Message('Date',
                             [Component('Day', 'Integer'),
                              Component('Month', 'Month_Name'),
                              Component('Year', 'Natural')]),
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
                              Component('Baz', 'T')])]))}
        self.assert_specifications(['message_type.rflx'], spec)

    # def test_aspect(self) -> None:
    #     spec = {'Test': Specification(
    #         Context([]),
    #         Package('Test',
    #                 [Message('Date',
    #                         [Component('Day', 'Integer'),
    #                          Component('Month', 'Month_Name'),
    #                          Component('Year', 'Natural')],
    #                          [Aspect('Type_Invariant',
    #                                  GreaterEqual(Length('Month'), Number(3)))]),
    #                  Type('Short_Date',
    #                       Derived('Date'),
    #                       [Aspect('Type_Invariant',
    #                               LessEqual(Length('Month'), Number(3)))]),
    #                  Type('PDU_X',
    #                       Derived('PDU', {'Payload_Type': 'X'}),
    #                       [Aspect('Type_Invariant',
    #                               And(And(Equal(Value('X_Type'), Number(42)),
    #                                       Or(Greater(Value('Foo'), Number(1)),
    #                                          Less(Value('Bar'), Number(2)))),
    #                                   NotEqual(Value('Baz'), Value('Foo'))))])]))}
    #     self.assert_specifications(['aspect.rflx'], spec)

    def test_ethernet_spec(self) -> None:
        spec = {'Ethernet': Specification(
            Context([]),
            Package('Ethernet',
                    [RangeInteger('UINT16', 0, 65535, 16),
                     ModularInteger('UINT48', 281474976710656),
                     Message('PDU',
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
                                             Sub(Last('Buffer'),
                                                 Last('EtherType'))),
                                       GreaterEqual(Value('EtherType'),
                                                    Number(1536)))]),
                              Component('Payload', 'Payload_Array', [
                                  Then('null',
                                       None,
                                       And(GreaterEqual(Length('Payload'),
                                                        Number(46)),
                                           LessEqual(Length('Payload'),
                                                     Number(1500))))])])]))}

        self.assert_specifications(['ethernet.rflx'], spec)

    def test_ethernet_pdu(self) -> None:
        self.assert_pdus(['ethernet.rflx'], {'Ethernet': ETHERNET_PDU})


if __name__ == "__main__":
    unittest.main()
