#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest

from parser import (And, Aspect, Attribute, Component, Context, Derived, Enumeration, Equal,
                    Greater, GreaterEqual, Less, LessEqual, Modular, Name, NotEqual, Package,
                    Parser, ParserError, Or, Range, Record, Specification, Type, Value)


class TestParser(unittest.TestCase):
    def setUp(self):
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def fullpath(self, testfile):
        return self.testdir + "/" + testfile

    def assert_data(self, filenames, data):
        parser = Parser()
        for filename in filenames:
            parser.parse(self.fullpath(filename))
        self.assertEqual(parser.specifications(), data, filenames)

    def assert_parser_error(self, filenames):
        with self.assertRaises(ParserError):
            parser = Parser()
            for filename in filenames:
                parser.parse(self.fullpath(filename))

    def test_empty_file(self):
        self.assert_data(['empty_file.rflx'], {})

    def test_comment_only(self):
        self.assert_data(['comment_only.rflx'], {})

    def test_package(self):
        self.assert_data(['package.rflx'], {'Test': Specification(Context([]),
                                                                  Package('Test', []))})

    def test_context(self):
        self.assert_data(['context.rflx'], {'Test': Specification(Context(['Foo', 'Bar']),
                                                                  Package('Test', []))})

    def test_duplicate_package(self):
        self.assert_parser_error(["package.rflx", "package.rflx"])

    def test_derived_type(self):
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [Type('Counter',
                          Derived(Name('Positive'))),
                     Type('PDU_X',
                          Derived(Name('PDU'), {'Payload_Type': 'X'}))]))}
        self.assert_data(['derived_type.rflx'], spec)

    def test_integer_type(self):
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [Type('Page_Num', Range(Value('1'), Value('2000'), Value('16'))),
                     Type('Line_Size', Range(Value('0'), Value('255'), Value('8'))),
                     Type('Byte', Modular(Value('2**8'))),
                     Type('Hash_Index', Modular(Value('97')))]))}
        self.assert_data(['integer_type.rflx'], spec)

    def test_enumeration_type(self):
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [Type('Day',
                          Enumeration([Name('Mon'),
                                       Name('Tue'),
                                       Name('Wed'),
                                       Name('Thu'),
                                       Name('Fri'),
                                       Name('Sat'),
                                       Name('Sun')])),
                     Type('Gender',
                          Enumeration([Name('M'),
                                       Name('F')]))]))}
        self.assert_data(['enumeration_type.rflx'], spec)

    # def test_array_type(self):
    #     self.assert_data(['array_type.rflx'], [])

    def test_record_type(self):
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [Type('Date',
                          Record([Component('Day', Name('Integer')),
                                  Component('Month', Name('Month_Name')),
                                  Component('Year', Name('Natural'))])),
                     Type('PDU',
                          Record([Component('Destination', Name('U48')),
                                  Component('Source', Name('U48')),
                                  Component('EtherType', Name('U16')),
                                  Component('Payload', Name('Payload_Type'))],
                                 True))]))}
        self.assert_data(['record_type.rflx'], spec)

    # def test_record_type_with_slice(self):
    #     self.assert_data(['record_type_with_slice.rflx'], [])

    # def test_record_type_with_variant(self):
    #     self.assert_data(['record_type_with_variant.rflx'], [])

    def test_aspect(self):
        spec = {'Test': Specification(
            Context([]),
            Package('Test',
                    [Type('Date',
                          Record([Component('Day', Name('Integer')),
                                  Component('Month', Name('Month_Name')),
                                  Component('Year', Name('Natural'))]),
                          [Aspect('Type_Invariant',
                                  GreaterEqual(Attribute('Month', 'Length'), Value('3')))]),
                     Type('Short_Date',
                          Derived(Name('Date')),
                          [Aspect('Type_Invariant',
                                  LessEqual(Attribute('Month', 'Length'), Value('3')))]),
                     Type('PDU_X',
                          Derived(Name('PDU'), {'Payload_Type': 'X'}),
                          [Aspect('Type_Invariant',
                                  And(And(Equal(Name('X_Type'), Value('42')),
                                          Or(Greater(Name('Foo'), Value('1')),
                                             Less(Name('Bar'), Value('2')))),
                                      NotEqual(Name('Baz'), Name('Foo'))))])]))}
        self.assert_data(['aspect.rflx'], spec)

    def test_ethernet(self):
        spec = {'Ethernet': Specification(
            Context([]),
            Package('Ethernet',
                    [Type('U16',
                          Modular(Value('2**16'))),
                     Type('U48',
                          Modular(Value('2**48'))),
                     Type('PDU',
                          Record([Component('Destination', Name('U48')),
                                  Component('Source', Name('U48')),
                                  Component('EtherType', Name('U16')),
                                  Component('Payload', Name('Payload_Type'))],
                                 True),
                          [Aspect('Type_Invariant',
                                  And(GreaterEqual(Attribute('Payload', 'Length'),
                                                   Value('46')),
                                      LessEqual(Attribute('Payload', 'Length'),
                                                Value('1500'))))]),
                     Type('IEEE_802_3',
                          Derived(Name('PDU')),
                          [Aspect('Type_Invariant',
                                  And(LessEqual(Name('EtherType'), Value('1500')),
                                      Equal(Attribute('Payload', 'Length'),
                                            Name('EtherType'))))]),
                     Type('Version_2',
                          Derived(Name('PDU')),
                          [Aspect('Type_Invariant',
                                  GreaterEqual(Name('EtherType'), Value('1536')))])]))}
        self.assert_data(['ethernet.rflx'], spec)

    # def test_mqtt(self):
    #     self.assert_data(['mqtt.rflx'], [])


if __name__ == "__main__":
    unittest.main()
