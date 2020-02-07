import unittest
from typing import List

from rflx.generator import Generator
from rflx.parser import Parser


class TestIntegration(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "generated"
        self.specdir = "specs"
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_integration(self, basenames: List[str]) -> None:
        parser = Parser()
        for basename in basenames:
            parser.parse(f"{self.specdir}/{basename}.rflx")

        generator = Generator("RFLX", reproducible=True)
        generator.generate(parser.messages, parser.refinements)

        for unit in generator.units.values():
            with open(f"{self.testdir}/{unit.name}.ads", "r") as f:
                self.assertEqual(unit.specification, f.read())
            if unit.body:
                with open(f"{self.testdir}/{unit.name}.adb", "r") as f:
                    self.assertEqual(unit.body, f.read())

    def test_ethernet(self) -> None:
        self.assert_integration(["ethernet"])

    def test_ipv4(self) -> None:
        self.assert_integration(["ipv4"])

    def test_in_ethernet(self) -> None:
        self.assert_integration(["ethernet", "ipv4", "in_ethernet"])

    def test_udp(self) -> None:
        self.assert_integration(["udp"])

    def test_in_ipv4(self) -> None:
        self.assert_integration(["ipv4", "udp", "in_ipv4"])

    def test_tlv(self) -> None:
        self.assert_integration(["tlv"])
