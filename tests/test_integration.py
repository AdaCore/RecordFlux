import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import List

import pytest

from rflx.generator import Generator
from rflx.parser import Parser

CODEDIR = "generated"
SPECDIR = "specs"
TESTDIR = "tests"


def assert_equal_code(spec_files: List[str]) -> None:
    parser = Parser()

    for spec_file in spec_files:
        parser.parse(Path(spec_file))

    model = parser.create_model()

    generator = Generator("RFLX", reproducible=True)
    generator.generate(model)

    for unit in generator.units.values():
        filename = f"{CODEDIR}/{unit.name}.ads"
        with open(filename, "r") as f:
            assert unit.ads == f.read(), filename
        if unit.adb:
            filename = f"{CODEDIR}/{unit.name}.adb"
            with open(filename, "r") as f:
                assert unit.adb == f.read(), filename


def assert_compilable_code(spec_files: List[str], prefix: str = None) -> None:
    parser = Parser()

    for spec_file in spec_files:
        parser.parse(Path(spec_file))

    _assert_compilable_code(parser, prefix)


def assert_compilable_code_string(specification: str, prefix: str = None) -> None:
    parser = Parser()
    parser.parse_string(specification)

    _assert_compilable_code(parser, prefix)


def _assert_compilable_code(parser: Parser, prefix: str = None) -> None:
    model = parser.create_model()

    generator = Generator(prefix if prefix else "RFLX")
    generator.generate(model)

    with TemporaryDirectory() as tmpdir:
        tmp_path = Path(tmpdir)

        generator.write_units(tmp_path)
        generator.write_library_files(tmp_path)
        generator.write_top_level_package(tmp_path)

        p = subprocess.run(
            ["gprbuild", "-q", "-U"], cwd=tmp_path, check=False, stderr=subprocess.PIPE
        )
        if p.returncode:
            raise AssertionError(
                f"non-zero exit status {p.returncode}\n{p.stderr.decode('utf-8')}",
            )


def test_ethernet() -> None:
    assert_equal_code([f"{SPECDIR}/ethernet.rflx"])


def test_ipv4() -> None:
    assert_equal_code([f"{SPECDIR}/ipv4.rflx"])


def test_in_ethernet() -> None:
    assert_equal_code(
        [f"{SPECDIR}/ethernet.rflx", f"{SPECDIR}/ipv4.rflx", f"{SPECDIR}/in_ethernet.rflx"]
    )


def test_udp() -> None:
    assert_equal_code([f"{SPECDIR}/udp.rflx"])


def test_in_ipv4() -> None:
    assert_equal_code([f"{SPECDIR}/ipv4.rflx", f"{SPECDIR}/udp.rflx", f"{SPECDIR}/in_ipv4.rflx"])


def test_tlv() -> None:
    assert_equal_code([f"{SPECDIR}/tlv.rflx"])


def test_tls() -> None:
    assert_compilable_code(
        [
            f"{SPECDIR}/tls_alert.rflx",
            f"{SPECDIR}/tls_handshake.rflx",
            f"{SPECDIR}/tls_record.rflx",
        ]
    )


def test_icmp() -> None:
    assert_compilable_code([f"{SPECDIR}/icmp.rflx"])


def test_feature_integeration() -> None:
    assert_compilable_code([f"{TESTDIR}/feature_integration.rflx"])


def test_no_prefix() -> None:
    assert_compilable_code([f"{SPECDIR}/tlv.rflx"], prefix="")


def test_type_name_equals_package_name() -> None:
    spec = """
           package Test is

              type Test is {};

              type Message is
                 message
                    Field : Test;
                 end message;

           end Test;
        """
    assert_compilable_code_string(spec.format("mod 2**32"))
    assert_compilable_code_string(spec.format("range 1 .. 2**32 - 1 with Size => 32"))
    assert_compilable_code_string(spec.format("(A, B, C) with Size => 32"))
    assert_compilable_code_string(spec.format("(A, B, C) with Size => 32, Always_Valid"))


@pytest.mark.parametrize("condition", ["A + 1 = 17179869178", "A = B - 1"])
def test_comparison_big_integers(condition: str) -> None:
    assert_compilable_code_string(
        f"""
           package Test is

              type D is range 17179869177 .. 17179869178 with Size => 35;

              type E is
                 message
                    A : D;
                    B : D
                       then C
                          with Length => 8
                          if {condition};
                    C : Opaque;
                 end message;

           end Test;
        """
    )
