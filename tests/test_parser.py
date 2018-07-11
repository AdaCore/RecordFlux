#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest

from parser import (And, Aspect, Attribute, Component, Derived, Enumeration, Equal, Greater,
                    GreaterEqual, Less, LessEqual, Modular, Name, NotEqual, Package, Parser, Or,
                    Record, Signed, Type, Value)


class TestParser(unittest.TestCase):
    def setUp(self):
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def fullpath(self, testfile):
        return self.testdir + "/" + testfile

    def parse(self, filename):
        parser = Parser()
        parser.parse(self.fullpath(filename))
        return parser

    def assert_data(self, filename, data):
        parser = self.parse(filename)
        self.assertEqual(parser.syntax_tree(), data, self.fullpath(filename))

    def test_empty_file(self):
        self.assert_data("empty_file.rflx", [])

    def test_comment_only(self):
        self.assert_data("comment_only.rflx", [])

    def test_package(self):
        self.assert_data("package.rflx", [Package('Test', [])])

    def test_derived_type(self):
        package = Package('Test',
                          [Type('Counter',
                                Derived(Name('Positive'))),
                           Type('PDU_X',
                                Derived(Name('PDU'), {'Payload_Type': 'X'}))])
        self.assert_data("derived_type.rflx", [package])

    def test_integer_type(self):
        package = Package('Test',
                          [Type('Page_Num', Signed(Value('1'), Value('2_000'))),
                           Type('Line_Size', Signed(Value('1'), Name('Max_Line_Size'))),
                           Type('Byte', Modular(Value('2**8'))),
                           Type('Hash_Index', Modular(Value('97')))])
        self.assert_data("integer_type.rflx", [package])

    def test_enumeration_type(self):
        package = Package('Test',
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
                                             Name('F')]))])
        self.assert_data("enumeration_type.rflx", [package])

    # def test_array_type(self):
    #     self.assert_data("array_type.rflx", [])

    def test_record_type(self):
        package = Package('Test', [
            Type('Date', Record([
                Component('Day', Name('Integer')),
                Component('Month', Name('Month_Name')),
                Component('Year', Name('Integer'))]))])
        self.assert_data("record_type.rflx", [package])

    # def test_record_type_with_slice(self):
    #     self.assert_data("record_type_with_slice.rflx", [])

    # def test_record_type_with_variant(self):
    #     self.assert_data("record_type_with_variant.rflx", [])

    def test_aspect(self):
        package = Package('Test',
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
                                            NotEqual(Name('Baz'), Name('Foo'))))])])
        self.assert_data("aspect.rflx", [package])

    def test_simple_ethernet(self):
        package = Package('Simple_Ethernet',
                          [Type('U48',
                                Modular(Value('2**48'))),
                           Type('PDU',
                                Record([Component('Destination', Name('U48')),
                                        Component('Source', Name('U48')),
                                        Component('EtherType', Name('U16')),
                                        Component('Payload', Name('Payload_Type'))]),
                                [Aspect('Type_Invariant',
                                        And(And(GreaterEqual(Attribute('Payload', 'Length'), Value('46')),
                                                LessEqual(Attribute('Payload', 'Length'), Value('1500'))),
                                            Or(LessEqual(Name('EtherType'), Value('1500')),
                                               GreaterEqual(Name('EtherType'), Value('1536')))))])])
        self.assert_data("simple_ethernet.rflx", [package])

    # def test_ethernet(self):
    #     self.assert_data("ethernet.rflx", [])

    # def test_mqtt(self):
    #     self.assert_data("mqtt.rflx", [])


if __name__ == "__main__":
    unittest.main()
