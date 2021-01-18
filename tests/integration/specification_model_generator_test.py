from pathlib import Path

import pytest

from rflx.specification import Parser
from tests import utils
from tests.const import SPEC_DIR


def test_no_prefix(tmp_path: Path) -> None:
    utils.assert_compilable_code_specs([f"{SPEC_DIR}/tlv.rflx"], tmp_path, prefix="")


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
    utils.assert_compilable_code_string(spec.format(definition), tmp_path)


@pytest.mark.parametrize("condition", ["A + 1 = 17179869178", "A = B - 1"])
def test_comparison_big_integers(condition: str, tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        f"""
           package Test is

              type D is range 17179869177 .. 17179869178 with Size => 40;

              type E is
                 message
                    A : D;
                    B : D
                       then C
                          with Size => 8
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
    utils.assert_compilable_code_string(
        f"""
           package Test is

              type M is
                 message
                    null
                       then A
                          with Size => 7 * 8;
                    A : Opaque
                       then null
                          if {condition};
                 end message;

           end Test;
        """,
        tmp_path,
    )


def test_potential_name_conflicts_fields_literals(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
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
           package Test is
              type T is mod 256;
           end Test;
        """
    )
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test::T;
           end Array_Test;
        """
    )
    utils.assert_compilable_code(p.create_model(), tmp_path)


def test_array_with_imported_element_type_message(tmp_path: Path) -> None:
    p = Parser()
    p.parse_string(
        """
           package Test is
              type M is
                 message
                    null
                       then A
                          with Size => 8;
                    A : Opaque;
                 end message;
           end Test;
        """
    )
    p.parse_string(
        """
           with Test;
           package Array_Test is
              type T is array of Test::M;
           end Array_Test;
        """
    )
    utils.assert_compilable_code(p.create_model(), tmp_path)


def test_unbounded_message(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """
           package Test is

              type M is
                 message
                    null
                       then A
                          with Size => Message'Size;
                    A : Opaque;
                 end message;

           end Test;
        """,
        tmp_path,
    )


@pytest.mark.parametrize(
    "aspects",
    [
        "with Size => Test::T'Size if A = Test::T'Size",
        "with Size => A'Size if A = A'Size",
    ],
)
def test_size_attribute(tmp_path: Path, aspects: str) -> None:
    utils.assert_compilable_code_string(
        f"""
           package Test is

              type T is mod 2**8;

              type M is
                 message
                    A : T;
                    B : Opaque
                       {aspects};
                 end message;

           end Test;
        """,
        tmp_path,
    )


def test_message_size_calculation(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """
           package Test is

              type T is mod 2**16;

              type Message is
                 message
                    A : T;
                    B : T;
                    C : Opaque
                       with Size => A * 8
                       if Message'Size = A * 8 + (B'Last - A'First + 1);
                 end message;

           end Test;
        """,
        tmp_path,
    )


def test_transitive_type_use(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """
            package Test is

               type U8  is mod 2**8;

               type M1 is
                  message
                     F1 : U8;
                     F2 : Opaque with Size => F1 * 8;
                  end message;
               type M1S is array of M1;

               type M2 is
                  message
                     F1 : U8;
                     F2 : M1S with Size => F1 * 8;
                  end message;

               type M3 is
                  message
                     F1 : M2;
                     F2 : U8;
                     F3 : M1S with Size => F2 * 8;
                  end message;

            end Test;
        """,
        tmp_path,
    )


def test_refinement_with_imported_enum_literal(tmp_path: Path) -> None:
    p = Parser()
    p.parse_string(
        """
           package Numbers is
              type Protocol is (PROTO_X) with Size => 8;
           end Numbers;
        """
    )
    p.parse_string(
        """
           with Numbers;
           package Proto is
              type Packet is
                 message
                    Protocol : Numbers::Protocol
                       then Data
                          with Size => 128;
                    Data  : Opaque;
                 end message;
           end Proto;
        """
    )
    p.parse_string(
        """
           with Proto;
           with Numbers;
           package In_Proto is
              type X is null message;
              for Proto::Packet use (Data => X)
                 if Protocol = Numbers::PROTO_X;
           end In_Proto;
        """
    )
    utils.assert_compilable_code(p.create_model(), tmp_path)
