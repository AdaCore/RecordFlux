import textwrap
from pathlib import Path

import pytest

from rflx.generator import Debug
from rflx.integration import Integration
from rflx.model import Model, Session, State, Transition
from rflx.specification import Parser
from tests import utils
from tests.const import SPEC_DIR
from tests.data.specs import (
    DEFINITE_MESSAGE_WITH_BUILTIN_TYPE_SPEC,
    DEFINITE_PARAMETERIZED_MESSAGE_SPEC,
    PARAMETERIZED_MESSAGE_SPEC,
)
from tests.utils import assert_compilable_code


def test_session_with_only_null_state(tmp_path: Path) -> None:
    state = State("St", [Transition("null")])
    session = Session("P::S", states=[state], declarations=[], parameters=[], types=[])
    model = Model([session])
    assert_compilable_code(model, Integration(), tmp_path)


@pytest.mark.parametrize(
    "prefix",
    [
        "",
        "Foo",
    ],
)
def test_prefix(prefix: str, tmp_path: Path) -> None:
    utils.assert_compilable_code_specs([f"{SPEC_DIR}/tlv.rflx"], tmp_path, prefix=prefix)


@pytest.mark.parametrize(
    "definition",
    [
        "range 1 .. 2 ** 32 - 1 with Size => 32",
        "(A, B, C) with Size => 32",
        "(A, B, C) with Size => 32, Always_Valid",
        "sequence of T",
    ],
)
def test_type_name_equals_package_name(definition: str, tmp_path: Path) -> None:
    spec = """\
       package Test is

          type T is range 0 .. 2 ** 8 - 1 with Size => 8;

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
        f"""\
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
        f"""\
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
    enum_literals = ",\n             ".join(literals)
    condition = "\n                  or ".join(f"A = {l}" for l in literals)
    spec = f"""\
        package Test is

           type A is ({enum_literals}, Extra_Literal) with Size => 8;

           type B is sequence of A;

           type Message is
              message
                 A : A;
                 B : B
                    with Size => 8;
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
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
        end Test;
        """,
    )
    p.parse_string(
        """\
        with Test;
        package Sequence_Test is
           type T is sequence of Test::T;
        end Sequence_Test;
        """,
    )
    utils.assert_compilable_code(p.create_model(), Integration(), tmp_path)


def test_sequence_with_imported_element_type_message(tmp_path: Path) -> None:
    p = Parser()
    p.parse_string(
        """\
        package Test is
           type M is
              message
                 null
                    then A
                       with Size => 8;
                 A : Opaque;
              end message;
        end Test;
        """,
    )
    p.parse_string(
        """\
        with Test;
        package Sequence_Test is
           type T is sequence of Test::M;
        end Sequence_Test;
        """,
    )
    utils.assert_compilable_code(p.create_model(), Integration(), tmp_path)


@pytest.mark.parametrize(
    "type_definition",
    [
        "range 0 .. 2 ** 63 - 1 with Size => 63",
        "range 2 ** 8 .. 2 ** 48 with Size => 63",
        "(A, B, C) with Size => 63",
        "(A, B, C) with Always_Valid, Size => 63",
    ],
)
def test_63_bit_types(type_definition: str, tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        f"""\
        package Test is

           type T is {type_definition};

        end Test;
        """,
        tmp_path,
    )


def test_message_fixed_size_sequence(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """\
        package Test is

           type E is range 0 .. 2 ** 8 - 1 with Size => 8;

           type S is sequence of E;

           type M is
              message
                 null
                    then A
                       with Size => 63;
                 A : S;
              end message;

        end Test;
        """,
        tmp_path,
    )


def test_message_with_implicit_size(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """\
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


def test_message_with_optional_field_based_on_message_size(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """\
        package Test is

           type T is range 0 .. 2 ** 8 - 1 with Size => 8;

           type M is
              message
                 Data : T
                    then More_Data
                       if Data'Last - Message'First + 1 + 8 = Message'Size
                    then null
                       if Data'Last = Message'Last;
                 More_Data : T;
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
        f"""\
        package Test is

           type T is range 0 .. 2 ** 8 - 1 with Size => 8;

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
        """\
        package Test is

           type T is range 0 .. 2 ** 16 - 1 with Size => 16;

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
        """\
        package Test is

           type U8 is range 0 .. 2 ** 8 - 1 with Size => 8;

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
        """\
        package Numbers is
           type Protocol is (PROTO_X, PROTO_Y) with Size => 8;
        end Numbers;
        """,
    )
    p.parse_string(
        """\
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
        """,
    )
    p.parse_string(
        """\
        with Proto;
        with Numbers;
        package In_Proto is
           type X is null message;
           for Proto::Packet use (Data => X)
              if Protocol = Numbers::PROTO_X;
        end In_Proto;
        """,
    )
    utils.assert_compilable_code(p.create_model(), Integration(), tmp_path)


def test_refinement_with_self(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """\
        package Test is

           type Tag is (T1 => 1, T2 => 2) with Size => 8;

           type Length is range 0 .. 2 ** 8 - 1 with Size => 8;

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
    utils.assert_compilable_code_string(DEFINITE_MESSAGE_WITH_BUILTIN_TYPE_SPEC, tmp_path)


def test_message_expression_value_outside_type_range(tmp_path: Path) -> None:
    spec = """\
        package Test is

           type Length is range 0 .. 2 ** 8 - 1 with Size => 8;

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


def test_message_field_conditions_on_corresponding_fields(tmp_path: Path) -> None:
    spec = """\
        package Test is

           type T is range 0 .. 2 ** 8 - 1 with Size => 8;

           type M is
              message
                 A : T
                    if A = 1;
                 B : Opaque
                    with Size => A * 16
                    if B = [2, 3] and B'Size = 16;
                 C : T;
              end message;

        end Test;
        """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_message_field_conditions_on_subsequent_fields(tmp_path: Path) -> None:
    spec = """\
        package Test is

           type T is range 0 .. 2 ** 8 - 1 with Size => 8;

           type M is
              message
                 A : T;
                 B : Opaque
                    with Size => A * 16;
                 C : T
                    if A = 1 and B = [2, 3] and B'Size = 16;
              end message;

        end Test;
        """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_message_size(tmp_path: Path) -> None:
    utils.assert_compilable_code_specs([SPEC_DIR / "message_size.rflx"], tmp_path)


def test_feature_integration(tmp_path: Path) -> None:
    utils.assert_compilable_code_specs([SPEC_DIR / "feature_integration.rflx"], tmp_path)


def test_parameterized_message(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(PARAMETERIZED_MESSAGE_SPEC, tmp_path)


def test_definite_parameterized_message(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(DEFINITE_PARAMETERIZED_MESSAGE_SPEC, tmp_path)


def test_session_type_conversion_in_assignment(tmp_path: Path) -> None:
    spec = """\
       package Test is

          type Length is range 0 .. 2 ** 14 - 1 with Size => 32;

          type Packet is
             message
                Length : Length;
                Payload : Opaque
                   with Size => Length * 8;
             end message;

          generic
             Transport : Channel with Readable, Writable;
          session Session is
             Packet : Packet;
          begin
             state Receive
             is
             begin
                Transport'Read (Packet);
             transition
                goto Process
                   if Packet'Valid
                goto null
             end Receive;

             state Process
             is
                Send_Size : Length;
             begin
                Send_Size := Packet'Size / 8;
                Packet := Packet'(Length => Send_Size,
                                  Payload => Packet'Opaque);
             transition
                goto Send
             exception
                goto null
             end Process;

             state Send
             is
             begin
                Transport'Write (Packet);
             transition
                goto Receive
             end Send;
          end Session;

       end Test;
       """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_session_type_conversion_in_message_size_calculation(tmp_path: Path) -> None:
    spec = """\
        package Test is

           type Length is range 0 .. 2 ** 8 - 1 with Size => 8;
           type Elems is range 0 .. 1 with Size => 8;

           type Data is
              message
                 Length : Length;
                 Value  : Opaque
                    with Size => 8 * Length;
              end message;

           type Message (L : Length) is
              message
                 Elems : Elems;
                 Data  : Opaque
                    with Size => 8 * L * Elems;
              end message;

           generic
           session S is
              M : Message;
              D : Data;
              E : Elems;
           begin
              state Init
              is
              begin
                 M := Message'(L     => 64,
                               Elems => E,
                               Data  => D.Value);
              transition
                 goto null
              exception
                 goto null
              end Init;
           end S;

        end Test;
    """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_session_move_content_of_opaque_field(tmp_path: Path) -> None:
    spec = """\
        package Test is

           type Payload_Size is range 0 .. 2 ** 16 - 1 with Size => 16;

           type M1 is
              message
                 Size : Payload_Size
                    if Size mod 8 = 0;
                 Payload : Opaque
                    with Size => Size;
              end message;

           type M2 (Size : Payload_Size) is
              message
                 null
                    then Payload
                       if Size mod 8 = 0;
                 Payload : Opaque
                    with Size => Size;
              end message;

           generic
              Channel : Channel with Readable;
              Output : Channel with Writable;
           session Session is
             Incoming : M1;
             Outgoing : M2;
           begin
              state Read is
              begin
                Channel'Read (Incoming);
              transition
                 goto Process
              end Read;

              state Process is
              begin
                Outgoing := M2'(Size => Incoming'Size, Payload => Incoming.Payload);
              transition
                 goto Write
              exception
                 goto null
              end Process;

              state Write is
              begin
                Output'Write (Outgoing);
              transition
                 goto null
              end Write;
           end Session;

        end Test;
    """
    utils.assert_compilable_code_string(spec, tmp_path)


@pytest.mark.parametrize(
    ("mode", "action"),
    [
        ("Readable", "Read"),
        ("Writable", "Write"),
    ],
)
def test_session_single_channel(mode: str, action: str, tmp_path: Path) -> None:
    spec = f"""\
        package Test is

           type Message is
              message
                 Data : Opaque;
              end message;

           generic
              Channel : Channel with {mode};
           session Session is
             Message : Message;
           begin
              state Init is
              begin
                Channel'{action} (Message);
              transition
                 goto null
              end Init;
           end Session;

        end Test;
    """
    utils.assert_compilable_code_string(spec, tmp_path)


@pytest.mark.parametrize(
    ("debug", "expected"),
    [
        (Debug.NONE, ""),
        (Debug.BUILTIN, "State: A\nState: B\nState: C\n"),
        (Debug.EXTERNAL, "XState: A\nXState: B\nXState: C\n"),
    ],
)
def test_session_external_debug_output(debug: Debug, expected: str, tmp_path: Path) -> None:
    spec = """\
        package Test is

           generic
           session Session is
           begin
              state A is
              begin
              transition
                 goto B
              end A;

              state B is
              begin
              transition
                 goto C
              end B;

              state C is
              begin
              transition
                 goto null
              end C;
           end Session;

        end Test;
    """
    parser = Parser()
    parser.parse_string(spec)
    model = parser.create_model()
    integration = parser.get_integration()

    src_dir = tmp_path / "src"
    src_dir.mkdir()

    for filename, content in utils.session_main().items():
        (src_dir / filename).write_text(content)

    (src_dir / "rflx-rflx_debug.adb").write_text(
        textwrap.dedent(
            """\
            with Ada.Text_IO;

            package body RFLX.RFLX_Debug with
               SPARK_Mode
            is

               procedure Print (Message : String) is
               begin
                  Ada.Text_IO.Put_Line ("X" & Message);
               end Print;

            end RFLX.RFLX_Debug;""",
        ),
    )

    assert utils.assert_executable_code(model, integration, tmp_path, debug=debug) == expected


@pytest.mark.parametrize(
    ("global_rel", "local_rel"),
    [("Global", "Local"), ("Global = True", "Local = True"), ("Global = False", "Local = False")],
)
def test_session_boolean_relations(global_rel: str, local_rel: str, tmp_path: Path) -> None:
    spec = f"""\
        package Test is

           generic
           session Session is
              Global : Boolean := False;
           begin
              state Init is
                 Local : Boolean := False;
              begin
                 Global := {local_rel};
                 Local := {global_rel};
              transition
                 goto null
                    if {global_rel} and {local_rel}
                 goto null
              end Init;
           end Session;

        end Test;
    """
    utils.assert_compilable_code_string(spec, tmp_path)


def test_session_indirect_use_of_enum_type(tmp_path: Path) -> None:
    a = tmp_path / "a.rflx"
    a.write_text(
        textwrap.dedent(
            """\
            with B;
            with C;

            package A is

               generic
               session S is
               begin
                  state S is
                     M : B::M;
                  begin
                     M.F := C::E_1;
                  transition
                     goto S
                        if M.F = C::E_1
                     goto null
                  exception
                     goto null
                  end S;
               end S;

            end A;
            """,
        ),
    )
    b = tmp_path / "b.rflx"
    b.write_text(
        textwrap.dedent(
            """\
            package B is

               type M is
                  message
                     F : C::E;
                  end message;

            end B;
            """,
        ),
    )
    c = tmp_path / "c.rflx"
    c.write_text(
        textwrap.dedent(
            """\
            package C is

               type E is (E_1, E_2) with Size => 8;

            end C;
            """,
        ),
    )
    utils.assert_compilable_code_specs([a], tmp_path)


def test_session_message_field_access_in_transition(tmp_path: Path) -> None:
    utils.assert_compilable_code_string(
        """\
            package Test is

               type E is (E_1, E_2) with Size => 8;

               type M is
                  message
                     F : E;
                  end message;

               generic
                  with function F return M;
               session S is
               begin
                  state S is
                     M : M;
                  begin
                     M := F;
                  transition
                     goto S
                        if M.F = E_1
                     goto null
                  exception
                     goto null
                  end S;
               end S;

            end Test;
            """,
        tmp_path,
    )
