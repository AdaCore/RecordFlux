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


def test_potential_name_conflicts_with_enum_literals(tmp_path: Path) -> None:
    literals = [
        "Buffer",
        "Bytes",
        "Bytes_Ptr",
        "Context",
        "Data",
        "F_A",
        "F_B",
        "F_C",
        "F_Final",
        "F_Inital",
        "Field",
        "First",
        "Fld",
        "Fst",
        "Invalid",
        "Last",
        "Length",
        "Lst",
        "Opaque",
        "S_Invalid",
        "S_Valid",
        "Seq_Ctx",
        "Sequence",
        "Size",
        "Valid",
        "Value",
    ]
    enum_literals = ", ".join(literals)
    condition = " or ".join(f"A = {l}" for l in literals)
    spec = f"""
           package Test is

              type A is ({enum_literals}) with Size => 8;

              type B is sequence of A;

              type Message is
                 message
                    A : A
                       then B
                          if {condition};
                    B : B
                       with Size => 8
                       then C
                          if {condition};
                    C : Opaque
                       with Size => 8
                       then null
                          if {condition};
                 end message;

              for Message use (C => Message)
                 if {condition};

           end Test;
        """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_sequence_with_imported_element_type_scalar(tmp_path: Path) -> None:
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
           package Sequence_Test is
              type T is sequence of Test::T;
           end Sequence_Test;
        """
    )
    utils.assert_compilable_code(p.create_model(), tmp_path)


def test_sequence_with_imported_element_type_message(tmp_path: Path) -> None:
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
           package Sequence_Test is
              type T is sequence of Test::M;
           end Sequence_Test;
        """
    )
    utils.assert_compilable_code(p.create_model(), tmp_path)


def test_message_fixed_size_sequence(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """
           package Test is

              type E is mod 2**8;

              type S is sequence of E;

              type M is
                 message
                    null
                       then A
                          with Size => 64;
                    A : S;
                 end message;

           end Test;
        """,
        tmp_path,
    )


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
               type M1S is sequence of M1;

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
              type Protocol is (PROTO_X, PROTO_Y) with Size => 8;
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


def test_refinement_with_self(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """
           package Test is

              type Tag is (T1 => 1) with Size => 8;

              type Length is mod 2**8;

              type Message is
                 message
                    Tag : Tag;
                    Length : Length;
                    Value : Opaque
                       with Size => 8 * Length;
                 end message;

              for Message use (Value => Message)
                 if Tag = T1;

           end Test;
        """,
        tmp_path,
    )


def test_definite_message_with_builtin_type(tmp_path: Path) -> None:
    spec = """
           package Test is

              type Length is range 0 .. 2**7 - 1 with Size => 7;

              type Message is
                 message
                    Flag : Boolean;
                    Length : Length;
                    Data : Opaque
                       with Size => Length * 8;
                 end message;

           end Test;
        """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_message_expression_value_outside_type_range(tmp_path: Path) -> None:
    spec = """
           package Test is

              type Length is mod 2 ** 8;

              type Packet is
                 message
                    Length_1 : Length;
                    Length_2 : Length
                       then Payload
                          with Size => Length_2 * 256 + Length_1
                          if (Length_2 * 256 + Length_1) mod 8 = 0;
                    Payload : Opaque;
                 end message;

           end Test;
        """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_session_type_conversion_in_assignment(tmp_path: Path) -> None:
    spec = """
        package Test is

           type Length is range 0 .. 2**24 - 1 with Size => 32;

           type Packet is
              message
                 Length : Length;
                 Payload : Opaque
                    with Size => Length * 8;
              end message;

           generic
              Transport : Channel with Readable, Writable;
           session Session with
              Initial => Receive,
              Final => Error
           is
              Packet : Packet;
           begin
              state Receive
              is
              begin
                 Transport'Read (Packet);
              transition
                 then Send
                    if Packet'Valid
                 then Error
              end Receive;

              state Send
              is
                 Send_Size : Length;
              begin
                 Send_Size := Packet'Size / 8;
                 Transport'Write (Packet'(Length => Send_Size,
                                          Payload => Packet'Opaque));
              transition
                 then Receive
              exception
                 then Error
              end Send;

              state Error is null state;
           end Session;

        end Test;
    """
    utils.assert_compilable_code_string(spec, tmp_path)
