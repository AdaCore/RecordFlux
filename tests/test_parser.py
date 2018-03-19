#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# pylint: disable=missing-docstring

import unittest

from parser import Parser


class TestParser(unittest.TestCase):
    def setUp(self):
        self.testdir = "tests"

    def fullpath(self, testfile):
        return self.testdir + "/" + testfile

    def parse(self, filename):
        parser = Parser()
        parser.parse(self.fullpath(filename))
        return parser

    def assert_data(self, filename, data):
        parser = self.parse(filename)
        self.assertEqual(parser.data(), data, self.fullpath(filename))

    def test_empty_file(self):
        self.assert_data("empty_file.rflx", [])

    def test_comment_only(self):
        self.assert_data("comment_only.rflx", [])

    def test_package(self):
        self.assert_data("package.rflx", [])

    def test_derived_type(self):
        self.assert_data("derived_type.rflx", [])

    def test_integer_type(self):
        self.assert_data("integer_type.rflx", [])

    def test_enumeration_type(self):
        self.assert_data("enumeration_type.rflx", [])

    def test_array_type(self):
        self.assert_data("array_type.rflx", [])

    def test_record_type(self):
        self.assert_data("record_type.rflx", [])

    def test_record_type_with_slice(self):
        self.assert_data("record_type_with_slice.rflx", [])

    def test_record_type_with_variant(self):
        self.assert_data("record_type_with_variant.rflx", [])

    def test_record_type_with_aspect(self):
        self.assert_data("record_type_with_aspect.rflx", [])

    def test_ethernet(self):
        self.assert_data("ethernet.rflx", [])

    def test_mqtt(self):
        self.assert_data("mqtt.rflx", [])


if __name__ == "__main__":
    unittest.main()
