import unittest
from typing import List

from rflx.generator import Generator
from rflx.parser import Parser


class TestIntegration(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "tests"
        self.specdir = "specs"
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_dissector(self, basenames: List[str]) -> None:
        parser = Parser()
        for basename in basenames:
            parser.parse(f'{self.specdir}/{basename}.rflx')

        generator = Generator()
        generator.generate_dissector(parser.messages, parser.refinements)

        for unit in generator.units():
            with open(f'{self.testdir}/{unit.name}.ads', 'r') as f:
                self.assertEqual(unit.specification, f.read())
            if unit.body:
                with open(f'{self.testdir}/{unit.name}.adb', 'r') as f:
                    self.assertEqual(unit.body, f.read())

    def test_ethernet(self) -> None:
        self.assert_dissector(['ethernet'])

    def test_ipv4(self) -> None:
        self.assert_dissector(['ipv4'])

    def test_in_ethernet(self) -> None:
        self.assert_dissector(['ethernet', 'ipv4', 'in_ethernet'])

    def test_udp(self) -> None:
        self.assert_dissector(['udp'])

    def test_in_ipv4(self) -> None:
        self.assert_dissector(['ipv4', 'udp', 'in_ipv4'])

    def test_tlv(self) -> None:
        self.assert_dissector(['tlv'])
