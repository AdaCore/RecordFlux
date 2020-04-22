import subprocess
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import List

from rflx.generator import Generator
from rflx.parser import Parser


class TestIntegration(unittest.TestCase):
    def setUp(self) -> None:
        self.codedir = "generated"
        self.specdir = "specs"
        self.testdir = "tests"
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_equal_code(self, spec_files: List[str]) -> None:
        parser = Parser()

        for spec_file in spec_files:
            parser.parse(Path(spec_file))

        model = parser.create_model()

        generator = Generator("RFLX", reproducible=True)
        generator.generate(model)

        for unit in generator.units.values():
            filename = f"{self.codedir}/{unit.name}.ads"
            with open(filename, "r") as f:
                self.assertEqual(unit.ads, f.read(), filename)
            if unit.adb:
                filename = f"{self.codedir}/{unit.name}.adb"
                with open(filename, "r") as f:
                    self.assertEqual(unit.adb, f.read(), filename)

    def assert_compilable_code(self, spec_files: List[str]) -> None:
        parser = Parser()

        for spec_file in spec_files:
            parser.parse(Path(spec_file))

        self._assert_compilable_code(parser)

    def assert_compilable_code_string(self, specification: str) -> None:
        parser = Parser()
        parser.parse_string(specification)

        self._assert_compilable_code(parser)

    @staticmethod
    def _assert_compilable_code(parser: Parser) -> None:
        model = parser.create_model()

        generator = Generator("RFLX")
        generator.generate(model)

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
        self.assert_equal_code([f"{self.specdir}/ethernet.rflx"])

    def test_ipv4(self) -> None:
        self.assert_equal_code([f"{self.specdir}/ipv4.rflx"])

    def test_in_ethernet(self) -> None:
        self.assert_equal_code(
            [
                f"{self.specdir}/ethernet.rflx",
                f"{self.specdir}/ipv4.rflx",
                f"{self.specdir}/in_ethernet.rflx",
            ]
        )

    def test_udp(self) -> None:
        self.assert_equal_code([f"{self.specdir}/udp.rflx"])

    def test_in_ipv4(self) -> None:
        self.assert_equal_code(
            [
                f"{self.specdir}/ipv4.rflx",
                f"{self.specdir}/udp.rflx",
                f"{self.specdir}/in_ipv4.rflx",
            ]
        )

    def test_tlv(self) -> None:
        self.assert_equal_code([f"{self.specdir}/tlv.rflx"])

    def test_tls(self) -> None:
        self.assert_compilable_code(
            [
                f"{self.specdir}/tls_alert.rflx",
                f"{self.specdir}/tls_handshake.rflx",
                f"{self.specdir}/tls_record.rflx",
            ]
        )

    def test_feature_integeration(self) -> None:
        self.assert_compilable_code([f"{self.testdir}/feature_integration.rflx"])

    def test_type_name_equals_package_name(self) -> None:
        spec = """
               package Test is

                  type Test is {};

                  type Message is
                     message
                        Field : Test;
                     end message;

               end Test;
            """
        self.assert_compilable_code_string(spec.format("mod 2**32"))
        self.assert_compilable_code_string(spec.format("range 1 .. 2**32 - 1 with Size => 32"))
        self.assert_compilable_code_string(spec.format("(A, B, C) with Size => 32"))
        self.assert_compilable_code_string(spec.format("(A, B, C) with Size => 32, Always_Valid"))
