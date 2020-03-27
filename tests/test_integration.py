import subprocess
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import List

from rflx.generator import Generator
from rflx.parser import Parser


class TestIntegration(unittest.TestCase):
    def setUp(self) -> None:
        self.testdir = "generated"
        self.specdir = "specs"
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_equal_code(self, basenames: List[str]) -> None:
        parser = Parser()
        for basename in basenames:
            parser.parse(Path(f"{self.specdir}/{basename}.rflx"))

        model = parser.create_model()

        generator = Generator("RFLX", reproducible=True)
        generator.generate(model.messages, model.refinements)

        for unit in generator.units.values():
            filename = f"{self.testdir}/{unit.name}.ads"
            with open(filename, "r") as f:
                self.assertEqual(unit.specification, f.read(), filename)
            if unit.body:
                filename = f"{self.testdir}/{unit.name}.adb"
                with open(filename, "r") as f:
                    self.assertEqual(unit.body, f.read(), filename)

    @staticmethod
    def assert_compilable_code(specification: str) -> None:
        parser = Parser()
        parser.parse_string(specification)

        model = parser.create_model()

        generator = Generator("RFLX")
        generator.generate(model.messages, model.refinements)

        with TemporaryDirectory() as tmpdir:
            path = Path(tmpdir)

            generator.write_units(path)
            generator.write_library_files(path)
            generator.write_top_level_package(path)

            p = subprocess.run(
                ["gprbuild", "-q", "-U"], cwd=path, check=False, stderr=subprocess.PIPE
            )
            if p.returncode:
                raise AssertionError(
                    f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
                )

    def test_ethernet(self) -> None:
        self.assert_equal_code(["ethernet"])

    def test_ipv4(self) -> None:
        self.assert_equal_code(["ipv4"])

    def test_in_ethernet(self) -> None:
        self.assert_equal_code(["ethernet", "ipv4", "in_ethernet"])

    def test_udp(self) -> None:
        self.assert_equal_code(["udp"])

    def test_in_ipv4(self) -> None:
        self.assert_equal_code(["ipv4", "udp", "in_ipv4"])

    def test_tlv(self) -> None:
        self.assert_equal_code(["tlv"])

    def test_type_name_equals_package_name(self) -> None:
        specification = """
               package Test is

                  type Test is {};

                  type Message is
                     message
                        Field : Test;
                     end message;

               end Test;
            """
        self.assert_compilable_code(specification.format("mod 2**32"))
        self.assert_compilable_code(specification.format("range 1 .. 2**32 - 1 with Size => 32"))
        self.assert_compilable_code(specification.format("(A, B, C) with Size => 32"))
        self.assert_compilable_code(specification.format("(A, B, C) with Size => 32, Always_Valid"))
