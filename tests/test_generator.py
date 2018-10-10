import unittest
from typing import List

from model import PDU
from generator import Generator

from tests.models import ETHERNET_PDU


class TestGenerator(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def fullpath(self, testfile: str) -> str:
        return self.testdir + "/" + testfile

    def assert_specification(self, generator: Generator) -> None:
        for unit in generator.units():
            unit_name = unit.package.name.lower().replace('.', '-')
            with open(self.fullpath(unit_name + '.ads'), 'r') as f:
                self.assertEqual(unit.specification(), f.read())

    def assert_definition(self, generator: Generator) -> None:
        for unit in generator.units():
            unit_name = unit.package.name.lower().replace('.', '-')
            if unit.definition().strip():
                with open(self.fullpath(unit_name + '.adb'), 'r') as f:
                    self.assertEqual(unit.definition(), f.read())

    def test_ethernet_dissector_spec(self) -> None:
        generator = generate_dissector([ETHERNET_PDU])
        self.assert_specification(generator)

    def test_ethernet_dissector_def(self) -> None:
        generator = generate_dissector([ETHERNET_PDU])
        self.assert_definition(generator)


def generate_dissector(pdus: List[PDU]) -> Generator:
    generator = Generator()
    generator.generate_dissector(pdus)
    return generator
