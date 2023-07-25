from __future__ import annotations

import re
import textwrap
from pathlib import Path

import pytest

from rflx import expression as expr
from rflx.error import ERROR_CONFIG, RecordFluxError
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    OPAQUE,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Model,
    Session,
    State,
    Transition,
    declaration as decl,
    statement as stmt,
)
from rflx.specification import parser
from tests.const import SPEC_DIR
from tests.utils import check_regex


def assert_error_string(string: str, regex: str) -> None:
    assert " model: error: " in regex
    check_regex(regex)
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
        p.parse_string(string)
        p.create_model()


def test_message_undefined_type() -> None:
    assert_error_string(
        """\
        package Test is
           type PDU is
              message
                 Foo : T;
              end message;
        end Test;
        """,
        r"^"
        r'<stdin>:4:16: model: error: undefined type "Test::T"\n'
        r'<stdin>:4:10: model: error: missing type for field "Foo" in "Test::PDU"'
        r"$",
    )


def test_message_field_first_conflict() -> None:
    assert_error_string(
        """\
        package Test is

           type T is range 0 .. 255 with Size => 8;

           type M is
              message
                 A : T
                    then B
                       with First => A'First;
                 B : T
                    with First => A'First;
              end message;

        end Test;
        """,
        r"^"
        r'<stdin>:11:27: model: error: first aspect of field "B" conflicts with previous'
        r" specification\n"
        r"<stdin>:9:30: model: info: previous specification of first"
        r"$",
    )


def test_message_field_size_conflict() -> None:
    assert_error_string(
        """\
        package Test is

           type T is range 0 .. 255 with Size => 8;

           type M is
              message
                 A : T
                    then B
                       with Size => 8;
                 B : Opaque
                    with Size => 8;
              end message;

        end Test;
        """,
        r"^"
        r'<stdin>:11:26: model: error: size aspect of field "B" conflicts with previous'
        r" specification\n"
        r"<stdin>:9:29: model: info: previous specification of size"
        r"$",
    )


def test_message_derivation_of_derived_type() -> None:
    assert_error_string(
        """\
        package Test is
           type Foo is null message;
           type Bar is new Foo;
           type Baz is new Bar;
        end Test;
        """,
        r'^<stdin>:4:9: model: error: illegal derivation "Test::Baz"\n'
        r'<stdin>:3:9: model: info: illegal base message type "Test::Bar"$',
    )


def test_illegal_redefinition() -> None:
    assert_error_string(
        """\
        package Test is
           type Boolean is range 0 .. 1 with Size => 2;
        end Test;
        """,
        r'^<stdin>:2:4: model: error: illegal redefinition of built-in type "Boolean"$',
    )


def test_invalid_enumeration_type_size() -> None:
    assert_error_string(
        """\
        package Test is
           type T is (Foo, Bar, Baz) with Size => 1;
        end Test;
        """,
        r'^<stdin>:2:9: model: error: size of "T" too small$',
    )


def test_invalid_enumeration_type_duplicate_values() -> None:
    assert_error_string(
        """\
        package Test is
           type T is (Foo => 0, Bar => 0) with Size => 1;
        end Test;
        """,
        r'^<stdin>:2:32: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:2:22: model: info: previous occurrence$",
    )


def test_invalid_enumeration_type_multiple_duplicate_values() -> None:
    assert_error_string(
        """\
        package Test is
           type T is (Foo => 0, Foo_1 => 1, Bar => 0, Bar_1 => 1) with Size => 8;
        end Test;
        """,
        r"^"
        r'<stdin>:2:44: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:2:22: model: info: previous occurrence\n"
        r'<stdin>:2:56: model: error: duplicate enumeration value "1" in "T"\n'
        r"<stdin>:2:34: model: info: previous occurrence"
        r"$",
    )


def test_invalid_enumeration_type_identical_literals() -> None:
    assert_error_string(
        """\
        package Test is
           type T1 is (Foo, Bar) with Size => 1;
           type T2 is (Bar, Baz) with Size => 1;
        end Test;
        """,
        r"^<stdin>:3:9: model: error: conflicting literals: Bar\n"
        r'<stdin>:2:21: model: info: previous occurrence of "Bar"$',
    )


def test_refinement_invalid_field() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 Foo : T;
              end message;
           for PDU use (Bar => PDU);
        end Test;
        """,
        r'^<stdin>:7:17: model: error: invalid field "Bar" in refinement of "Test::PDU"$',
    )


def test_refinement_invalid_condition() -> None:
    assert_error_string(
        """\
        package Test is
           type PDU is
              message
                 null
                    then Foo
                       with Size => 8;
                 Foo : Opaque;
              end message;
           for PDU use (Foo => PDU)
              if X < Y + 1;
        end Test;
        """,
        r"^"
        r'<stdin>:10:10: model: error: undefined variable "X"\n'
        r'<stdin>:10:14: model: error: undefined variable "Y"'
        r"$",
    )


def test_model_name_conflict_messages() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 Foo : T;
              end message;
           type PDU is
              message
                 Foo : T;
              end message;
        end Test;
        """,
        r'^<stdin>:7:9: model: error: name conflict for type "Test::PDU"\n'
        r'<stdin>:3:9: model: info: previous occurrence of "Test::PDU"$',
    )


def test_model_conflicting_refinements() -> None:
    assert_error_string(
        """\
        package Test is
           type PDU is
              message
                 null
                    then Foo
                       with Size => 8;
                 Foo : Opaque;
              end message;
           for Test::PDU use (Foo => Test::PDU);
           for PDU use (Foo => PDU);
        end Test;
        """,
        r"^"
        r'^<stdin>:10:4: model: error: conflicting refinement of "Test::PDU" with "Test::PDU"\n'
        r"<stdin>:9:4: model: info: previous occurrence of refinement"
        r"$",
    )


def test_model_name_conflict_derivations() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type Foo is
              message
                 Foo : T;
              end message;
           type Bar is new Test::Foo;
           type Bar is new Foo;
        end Test;
        """,
        r'^<stdin>:8:9: model: error: name conflict for type "Test::Bar"\n'
        r'<stdin>:7:9: model: info: previous occurrence of "Test::Bar"$',
    )


def test_model_name_conflict_sessions() -> None:
    assert_error_string(
        """\
        package Test is
           type X is range 0 .. 2 ** 8 - 1 with Size => 8;

           generic
           session X is
           begin
              state A is
              begin
              transition
                 goto null
              end A;
           end X;
        end Test;
        """,
        r'^<stdin>:4:4: model: error: name conflict for session "Test::X"\n'
        r'<stdin>:2:9: model: info: previous occurrence of "Test::X"$',
    )


def test_model_illegal_first_aspect_at_initial_link() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 255 with Size => 8;
           type PDU is
              message
                 null
                    then Foo
                       with First => 0;
                 Foo : T;
              end message;
        end Test;
        """,
        r"^<stdin>:7:30: model: error: illegal first aspect at initial link$",
    )


def test_model_errors_in_type_and_session() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 256 - 1 with Size => 256;

           generic
           session S is
           begin
           end S;
        end Test;
        """,
        r"^"
        r'<stdin>:2:9: model: error: last of "T" exceeds limit \(2\*\*63 - 1\)\n'
        r"<stdin>:4:4: model: error: empty states"
        r"$",
    )


def test_message_with_two_size_fields() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is
           type Length is range 0 .. 2 ** 8 - 1 with Size => 8;
           type Packet is
              message
                 Length_1 : Length;
                 Length_2 : Length
                    then Payload
                       with Size => 8 * (Length_1 + Length_2);
                 Payload : Opaque;
              end message;
        end Test;
        """
    )
    p.create_model()


def test_message_same_field_and_type_name_with_different_size() -> None:
    p = parser.Parser()
    p.parse_string(
        """\
        package Test is

           type T is range 0 .. 2 ** 8 - 1 with Size => 8;

           type M is
              message
                 A : T;
                 T : Opaque
                    with Size => 16;
              end message;

        end Test;
        """
    )
    p.create_model()


def test_invalid_implicit_size() -> None:
    assert_error_string(
        """\
        package Test is

           type Kind is range 0 .. 2 ** 16 - 1 with Size => 16;

           type M is
              message
                 A : Kind
                    then B
                       if Kind = 1
                    then C
                       if Kind = 2;
                 B : Kind;
                 C : Opaque
                    with Size => Message'Last - A'Last;
              end message;

        end Test;
        """,
        r"^"
        r'<stdin>:14:26: model: error: invalid use of "Message" in size aspect\n'
        r"<stdin>:14:26: model: info: remove size aspect to define field with implicit size"
        r"$",
    )


def test_invalid_use_of_message_type_with_implicit_size() -> None:
    assert_error_string(
        """\
        package Test is

           type T is range 0 .. 2 ** 16 - 1 with Size => 16;

           type Inner is
              message
                 Data : Opaque;
              end message;

           type Outer is
              message
                 A : T;
                 Inner : Inner;
                 B : T;
              end message;

        end Test;
        """,
        r"^"
        r"<stdin>:13:10: model: error: messages with implicit size may only be used"
        " for last fields\n"
        r'<stdin>:7:10: model: info: message field with implicit size in "Test::Inner"'
        r"$",
    )


def test_invalid_message_with_multiple_fields_with_implicit_size() -> None:
    assert_error_string(
        """\
        package Test is

           type M is
              message
                 A : Opaque
                    with Size => Message'Size;
                 B : Opaque
                    with Size => Message'Size - A'Size;
              end message;

        end Test;
        """,
        r"^"
        r'<stdin>:8:26: model: error: invalid use of "Message" in size aspect\n'
        r"<stdin>:8:26: model: info: remove size aspect to define field with implicit size\n"
        r'<stdin>:6:26: model: error: "Message" must not be used in size aspects'
        r"$",
    )


def test_invalid_message_with_field_after_field_with_implicit_size() -> None:
    assert_error_string(
        """\
        package Test is

           type T is range 0 .. 2 ** 8 - 1 with Size => 8;

           type M is
              message
                 A : T;
                 B : Opaque
                    with Size => Message'Size - 2 * Test::T'Size;
                 C : T;
              end message;

        end Test;
        """,
        r'^<stdin>:9:26: model: error: "Message" must not be used in size aspects$',
    )


def test_invalid_message_with_unreachable_field_after_merging() -> None:
    assert_error_string(
        """\
        package Test is

           type T is range 0 .. 3 with Size => 8;

           type I is
              message
                 A : T;
              end message;

           type O is
              message
                 C : I
                    then null
                       if C_A /= 4
                    then D
                       if C_A = 4;
                 D : T;
              end message;

        end Test;
        """,
        r'^<stdin>:17:10: model: error: unreachable field "D" in "Test::O"$',
    )


def test_fail_after_value(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(ERROR_CONFIG, "fail_after_value", 1)
    p = parser.Parser()
    p.parse(Path(f"{SPEC_DIR}/ethernet.rflx"))
    p.create_model()


def test_dependency_order() -> None:
    p = parser.Parser()
    p.parse(Path(f"{SPEC_DIR}/in_p1.rflx"))
    p.create_model()


def test_consistency_specification_parsing_generation(tmp_path: Path) -> None:
    tag = Enumeration(
        "Test::Tag",
        [("Msg_Data", expr.Number(1)), ("Msg_Error", expr.Number(3))],
        expr.Number(8),
        always_valid=False,
    )
    length = Integer(
        "Test::Length",
        expr.Number(0),
        expr.Sub(expr.Pow(expr.Number(2), expr.Number(16)), expr.Number(1)),
        expr.Number(16),
    )
    message = Message(
        "Test::Message",
        [
            Link(INITIAL, Field("Tag")),
            Link(
                Field("Tag"),
                Field("Length"),
                expr.Equal(expr.Variable("Tag"), expr.Variable("Msg_Data")),
            ),
            Link(Field("Tag"), FINAL, expr.Equal(expr.Variable("Tag"), expr.Variable("Msg_Error"))),
            Link(
                Field("Length"),
                Field("Value"),
                size=expr.Mul(expr.Variable("Length"), expr.Number(8)),
            ),
            Link(Field("Value"), FINAL),
        ],
        {Field("Tag"): tag, Field("Length"): length, Field("Value"): OPAQUE},
        skip_proof=True,
    )
    session = Session(
        "Test::Session",
        [
            State(
                "A",
                declarations=[],
                actions=[stmt.Read("X", expr.Variable("M"))],
                transitions=[
                    Transition("B"),
                ],
            ),
            State(
                "B",
                declarations=[
                    decl.VariableDeclaration("Z", BOOLEAN.identifier, expr.Variable("Y")),
                ],
                actions=[],
                transitions=[
                    Transition(
                        "null",
                        condition=expr.And(
                            expr.Equal(expr.Variable("Z"), expr.TRUE),
                            expr.Equal(expr.Call("G", [expr.Variable("F")]), expr.TRUE),
                        ),
                        description="rfc1149.txt+45:4-47:8",
                    ),
                    Transition("A"),
                ],
                description="rfc1149.txt+51:4-52:9",
            ),
        ],
        [
            decl.VariableDeclaration("M", "Test::Message"),
            decl.VariableDeclaration("Y", BOOLEAN.identifier, expr.FALSE),
        ],
        [
            decl.ChannelDeclaration("X", readable=True, writable=True),
            decl.FunctionDeclaration("F", [], "Test::Tag"),
            decl.FunctionDeclaration("G", [decl.Argument("P", "Test::Tag")], BOOLEAN.identifier),
        ],
        [BOOLEAN, OPAQUE, tag, length, message],
    )
    t = Integer(
        "Test::T",
        expr.Number(0),
        expr.Sub(expr.Pow(expr.Number(2), expr.Number(16)), expr.Number(1)),
        expr.Number(16),
    )
    model = Model([BOOLEAN, OPAQUE, tag, length, message, session, t])
    model.write_specification_files(tmp_path)
    p = parser.Parser()
    p.parse(tmp_path / "test.rflx")
    parsed_model = p.create_model()
    assert parsed_model.declarations == model.declarations
    assert parsed_model == model


@pytest.mark.parametrize(
    "rfi_content,match_error",
    [
        (
            """Session:
                No_Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Message: 2048
          """,
            'unknown session "No_Session"',
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Message: 2048
          """,
            'unknown global variable "Message" in session "Session"',
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: 2048
                        Local:
                            Unknown: {}
          """,
            'unknown state "Unknown" in session "Session"',
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: 2048
                        Local:
                            Start:
                                X : 12
          """,
            'unknown variable "X" in state "Start" of session "Session"',
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: 2048
                        Local:
                            Start: {}
          """,
            "",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Global:
                            Msg: 2048
                        Local:
                            Next:
                                Msg2: 2048
          """,
            "",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
                        Local:
                           Start: {}
          """,
            "",
        ),
        (
            """Session:
                Session:
                    Buffer_Size:
                        Default: 1024
          """,
            "",
        ),
    ],
)
def test_rfi_files(tmp_path: Path, rfi_content: str, match_error: str) -> None:
    p = parser.Parser()
    content = textwrap.dedent(
        """\
        package Test is
           type Message_Type is (MT_Null => 0, MT_Data => 1) with Size => 8;

           type Length is range 0 .. 2 ** 16 - 1 with Size => 16;

           type Value is range 0 .. 255 with Size => 8;

           type Message is
              message
                 Message_Type : Message_Type;
                 Length : Length;
                 Value : Value;
              end message;

           generic
               Channel : Channel with Readable, Writable;
           session Session is
               Msg : Message;
           begin
              state Start is
              begin
                 Channel'Read (Msg);
              transition
                 goto Reply
                    if Msg'Valid = True
                    and Msg.Message_Type = MT_Data
                    and Msg.Length = 1
                 goto null
              exception
                 goto null
              end Start;

              state Reply is
              begin
                 Msg := Message'(Message_Type => MT_Data, Length => 1, Value => 2);
              transition
                 goto Msg_Write
              exception
                 goto null
              end Reply;

              state Msg_Write is
              begin
                 Channel'Write (Msg);
              transition
                 goto Next
              end Msg_Write;

              state Next is
                 Msg2 : Message;
              begin
                 Msg2 := Message'(Message_Type => MT_Data, Length => 1, Value => 2);
              transition
                 goto null
              exception
                 goto null
              end Next;
           end Session;
        end Test;
"""
    )
    test_spec = tmp_path / "test.rflx"
    test_rfi = tmp_path / "test.rfi"
    test_spec.write_text(content, encoding="utf-8")
    test_rfi.write_text(rfi_content)
    if not match_error:
        p.parse(test_spec)
        p.create_model()
    else:
        regex = re.compile(rf"^test.rfi:0:0: parser: error: {match_error}$", re.DOTALL)
        with pytest.raises(RecordFluxError, match=regex):
            p.parse(test_spec)
            p.create_model()
