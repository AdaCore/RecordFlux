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


def assert_compilable_code(spec_files: List[str], prefix: str = None) -> None:
    parser = Parser()

    for spec_file in spec_files:
        parser.parse(Path(spec_file))

    utils.assert_compilable_code(parser.create_model(), prefix)


def assert_compilable_code_string(specification: str, prefix: str = None) -> None:
    parser = Parser()
    parser.parse_string(specification)

    utils.assert_compilable_code(parser.create_model(), prefix)


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
        """
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
def test_comparison_opaque(condition: str) -> None:
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
        """
    )


def test_potential_name_conflicts_fields_literals() -> None:
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
        """
    )


def test_array_with_imported_element_type_scalar() -> None:
    p = Parser()
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test.T;
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
    utils.assert_compilable_code(p.create_model())


def test_array_with_imported_element_type_message() -> None:
    p = Parser()
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test.M;
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
    utils.assert_compilable_code(p.create_model())


def test_unbounded_message() -> None:
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
        """
    )
