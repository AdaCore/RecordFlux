#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest

from parser import Parser
from generator import And, Field, Generator, GreaterEqual, Length, LessEqual, Or, Value


class TestGenerator(unittest.TestCase):
    def setUp(self):
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def fullpath(self, testfile):
        return self.testdir + "/" + testfile

    def generate(self, filename):
        parser = Parser()
        parser.parse(self.fullpath(filename))
        generator = Generator()
        generator.generate(parser.syntax_tree())
        return generator

    def assert_specification(self, stem):
        generator = self.generate(stem + '.rflx')
        with open(self.fullpath(stem + '.ads'), 'r') as f:
            self.assertEqual(generator.specification(), f.read())

    def assert_definition(self, stem):
        generator = self.generate(stem + '.rflx')
        with open(self.fullpath(stem + '.adb'), 'r') as f:
            self.assertEqual(generator.definition(), f.read())

    def test_transformed_simplified_0(self):
        expression = And(And(GreaterEqual(Length('Payload'), Value('46')),
                             LessEqual(Length('Payload'), Value('1500'))),
                         Or(LessEqual(Field('EtherType'), Value('1500')),
                            GreaterEqual(Field('EtherType'), Value('1536'))))
        expected = True
        self.assertEqual(expression.transformed('Source', [], 0).simplified(), expected)

    def test_transformed_simplified_1(self):
        expression = And(And(GreaterEqual(Length('Payload'), Value('46')),
                             LessEqual(Length('Payload'), Value('1500'))),
                         Or(LessEqual(Field('EtherType'), Value('1500')),
                            GreaterEqual(Field('EtherType'), Value('1536'))))
        expected = True
        self.assertEqual(expression.transformed('EtherType', ['Source'], 0).simplified(), expected)

    def test_transformed_simplified_2(self):
        expression = And(And(GreaterEqual(Length('Payload'), Value('46')),
                             LessEqual(Length('Payload'), Value('1500'))),
                         Or(LessEqual(Field('EtherType'), Value('1500')),
                            GreaterEqual(Field('EtherType'), Value('1536'))))
        expected = Or(LessEqual(Field('EtherType'), Value('1500')),
                      GreaterEqual(Field('EtherType'), Value('1536')))
        self.assertEqual(expression.transformed('EtherType',
                                                ['Source', 'EtherType'],
                                                0).simplified(),
                         expected)

    def test_transformed_simplified_3(self):
        expression = And(And(GreaterEqual(Length('Payload'), Value('46')),
                             LessEqual(Length('Payload'), Value('1500'))),
                         Or(LessEqual(Field('EtherType'), Value('1500')),
                            GreaterEqual(Field('EtherType'), Value('1536'))))
        expected = And(And(GreaterEqual(Length('Buffer'), Value('60')),
                           LessEqual(Length('Buffer'), Value('1514'))),
                       Or(LessEqual(Field('EtherType'), Value('1500')),
                          GreaterEqual(Field('EtherType'), Value('1536'))))
        self.assertEqual(expression.transformed('Payload',
                                                ['Source', 'EtherType'],
                                                14).simplified(),
                         expected)

    def test_simple_ethernet_decl(self):
        self.assert_specification('simple_ethernet')

    def test_simple_ethernet_def(self):
        self.assert_definition('simple_ethernet')


if __name__ == "__main__":
    unittest.main()
