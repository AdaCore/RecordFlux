import unittest
from typing import List

from rflx.generator import Generator
from rflx.model import Message, Refinement
from tests.models import (ARRAY_MESSAGE, DERIVATION_MESSAGE, ENUMERATION_MESSAGE, ETHERNET_FRAME,
                          EXPRESSION_MESSAGE)


class TestGenerator(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_specification(self, generator: Generator) -> None:
        for unit in generator.units():
            basename = unit.package.name.lower().replace('.', '-')
            with open(f'{self.testdir}/{basename}.ads', 'r') as f:
                self.assertEqual(unit.specification(), f.read())

    def assert_definition(self, generator: Generator) -> None:
        for unit in generator.units():
            basename = unit.package.name.lower().replace('.', '-')
            if unit.definition().strip():
                with open(f'{self.testdir}/{basename}.adb', 'r') as f:
                    self.assertEqual(unit.definition(), f.read())

    def test_ethernet_dissector_spec(self) -> None:
        generator = generate_dissector([ETHERNET_FRAME], [])
        self.assert_specification(generator)

    def test_ethernet_dissector_def(self) -> None:
        generator = generate_dissector([ETHERNET_FRAME], [])
        self.assert_definition(generator)

    def test_enumeration_dissector_spec(self) -> None:
        generator = generate_dissector([ENUMERATION_MESSAGE], [])
        self.assert_specification(generator)

    def test_enumeration_dissector_def(self) -> None:
        generator = generate_dissector([ENUMERATION_MESSAGE], [])
        self.assert_definition(generator)

    def test_array_dissector_spec(self) -> None:
        generator = generate_dissector([ENUMERATION_MESSAGE, ARRAY_MESSAGE], [])
        self.assert_specification(generator)

    def test_array_dissector_def(self) -> None:
        generator = generate_dissector([ENUMERATION_MESSAGE, ARRAY_MESSAGE], [])
        self.assert_definition(generator)

    def test_expression_dissector_spec(self) -> None:
        generator = generate_dissector([EXPRESSION_MESSAGE], [])
        self.assert_specification(generator)

    def test_expression_dissector_def(self) -> None:
        generator = generate_dissector([EXPRESSION_MESSAGE], [])
        self.assert_definition(generator)

    def test_derivation_dissector_spec(self) -> None:
        generator = generate_dissector([ARRAY_MESSAGE, DERIVATION_MESSAGE], [])
        self.assert_specification(generator)

    def test_derivation_dissector_def(self) -> None:
        generator = generate_dissector([ARRAY_MESSAGE, DERIVATION_MESSAGE], [])
        self.assert_definition(generator)


def generate_dissector(pdus: List[Message], refinements: List[Refinement]) -> Generator:
    generator = Generator()
    generator.generate_dissector(pdus, refinements)
    return generator
