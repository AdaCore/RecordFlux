from pathlib import Path
from typing import List

import pytest

from rflx.generator import Generator
from rflx.parser import Parser
from tests import utils

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


def assert_compilable_code(spec_files: List[str], tmp_path: Path, prefix: str = None) -> None:
    parser = Parser()

    for spec_file in spec_files:
        parser.parse(Path(spec_file))

    utils.assert_compilable_code(parser.create_model(), tmp_path, prefix)


def assert_compilable_code_string(specification: str, tmp_path: Path, prefix: str = None) -> None:
    parser = Parser()
    parser.parse_string(specification)

    utils.assert_compilable_code(parser.create_model(), tmp_path, prefix)


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


def test_tls(tmp_path: Path) -> None:
    assert_compilable_code(
        [
            f"{SPECDIR}/tls_alert.rflx",
            f"{SPECDIR}/tls_handshake.rflx",
            f"{SPECDIR}/tls_record.rflx",
        ],
        tmp_path,
    )


def test_icmp(tmp_path: Path) -> None:
    assert_compilable_code([f"{SPECDIR}/icmp.rflx"], tmp_path)


def test_feature_integeration(tmp_path: Path) -> None:
    assert_compilable_code([f"{TESTDIR}/feature_integration.rflx"], tmp_path)


def test_no_prefix(tmp_path: Path) -> None:
    assert_compilable_code([f"{SPECDIR}/tlv.rflx"], tmp_path, prefix="")


@pytest.mark.parametrize(
    "definition",
    [
        "mod 2**32",
        "range 1 .. 2**32 - 1 with Size => 32",
        "(A, B, C) with Size => 32",
        "(A, B, C) with Size => 32, Always_Valid",
    ],
)
def test_type_name_equals_package_name(definition: str, tmp_path: Path) -> None:
    spec = """
           package Test is

              type Test is {};

              type Message is
                 message
                    Field : Test;
                 end message;

           end Test;
        """
    assert_compilable_code_string(spec.format(definition), tmp_path)


@pytest.mark.parametrize("condition", ["A + 1 = 17179869178", "A = B - 1"])
def test_comparison_big_integers(condition: str, tmp_path: Path) -> None:
    assert_compilable_code_string(
        f"""
           package Test is

              type D is range 17179869177 .. 17179869178 with Size => 40;

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
        """,
        tmp_path,
    )


@pytest.mark.parametrize(
    "condition",
    [
        'A = "Foo Bar"',
        "A /= [0, 1, 2, 3, 4, 5, 6]",
        'A = "Foo" & [0] & "Bar"',
        '"Foo Bar" /= A',
        "[0, 1, 2, 3, 4, 5, 6] = A",
        '"Foo" & [0] & "Bar" /= A',
    ],
)
def test_comparison_opaque(condition: str, tmp_path: Path) -> None:
    assert_compilable_code_string(
        f"""
           package Test is

              type M is
                 message
                    null
                       then A
                          with Length => 7 * 8;
                    A : Opaque
                       then null
                          if {condition};
                 end message;

           end Test;
        """,
        tmp_path,
    )


def test_potential_name_conflicts_fields_literals(tmp_path: Path) -> None:
    assert_compilable_code_string(
        """
           package Test is

              type E is (F_A => 0, F_B => 1) with Size => 8;

              type M is
                 message
                    A : E
                       then null
                          if A = F_A
                       then B
                          if A = F_B;
                    B : E;
                 end message;

           end Test;
        """,
        tmp_path,
    )


def test_array_with_imported_element_type_scalar(tmp_path: Path) -> None:
    p = Parser()
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test::T;
           end Array_Test;
        """
    )
    p.parse_string(
        """
           package Test is
              type T is mod 256;
           end Test;
        """
    )
    utils.assert_compilable_code(p.create_model(), tmp_path)


def test_array_with_imported_element_type_message(tmp_path: Path) -> None:
    p = Parser()
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test::M;
           end Array_Test;
        """
    )
    p.parse_string(
        """
           package Test is
              type M is
                 message
                    null
                       then A
                          with Length => 8;
                    A : Opaque;
                 end message;
           end Test;
        """
    )
    utils.assert_compilable_code(p.create_model(), tmp_path)


def test_unbounded_message(tmp_path: Path) -> None:
    assert_compilable_code_string(
        """
           package Test is

              type M is
                 message
                    null
                       then A
                          with Length => Message'Length;
                    A : Opaque;
                 end message;

           end Test;
        """,
        tmp_path,
    )
