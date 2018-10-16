import unittest

from generator import Generator
from parser import Parser


class TestIntegration(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def fullpath(self, testfile: str) -> str:
        return self.testdir + "/" + testfile

    def assert_dissector(self, filename: str) -> None:
        parser = Parser()
        parser.parse('{}.rflx'.format(self.fullpath(filename)))

        generator = Generator()
        generator.generate_dissector(parser.pdus)

        for unit in generator.units():
            unit_name = unit.package.name.lower().replace('.', '-')
            filename = unit_name + '.ads'
            with open(self.fullpath(filename), 'r') as f:
                self.assertEqual(unit.specification(), f.read())
            if unit.definition().strip():
                filename = unit_name + '.adb'
                with open(self.fullpath(filename), 'r') as f:
                    self.assertEqual(unit.definition(), f.read())

    def test_ethernet(self) -> None:
        self.assert_dissector('ethernet')

    def test_ipv4(self) -> None:
        self.assert_dissector('ipv4')
