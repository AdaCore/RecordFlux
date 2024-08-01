from __future__ import annotations

import re
import textwrap
from pathlib import Path

import pytest

from rflx import expr, typing_ as rty
from rflx.const import RESERVED_WORDS
from rflx.identifier import ID
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
from rflx.rapidflux import Location, RecordFluxError
from rflx.specification import parser
from tests.const import SPEC_DIR
from tests.utils import check_regex


def assert_error_string(string: str, regex: str) -> None:
    assert " error: " in regex
    check_regex(regex)
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):  # noqa: PT012
        p.parse_string(string)
        p.create_model()


def assert_error_full_message(
    filepath: Path,
    expected: str,
    capfd: pytest.CaptureFixture[str],
) -> None:
    assert "error: " in expected
    regex = f"^{re.escape(expected)}$"
    check_regex(regex)

    p = parser.Parser()
    try:
        p.parse(filepath)
        p.create_model()
        assert False, "DID NOT RAISE"
    except RecordFluxError as e:
        e.print_messages()

    stderr_content = capfd.readouterr().err
    assert stderr_content == expected, f'got: "{stderr_content}"'


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
        r'^<stdin>:4:16: error: undefined type "Test::T"$',
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
        r'<stdin>:11:27: error: first aspect of field "B" conflicts with previous'
        r" specification\n"
        r"<stdin>:9:30: note: previous specification of first"
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
        r'<stdin>:11:26: error: size aspect of field "B" conflicts with previous'
        r" specification\n"
        r"<stdin>:9:29: note: previous specification of size"
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
        r"^<stdin>:4:9: error: invalid derivation\n"
        r"<stdin>:3:9: note: base type must be a message$",
    )


def test_illegal_redefinition() -> None:
    assert_error_string(
        """\
        package Test is
           type Boolean is range 0 .. 1 with Size => 2;
        end Test;
        """,
        r'^<stdin>:2:9: error: illegal redefinition of built-in type "Boolean"$',
    )


def test_invalid_enumeration_type_size() -> None:
    assert_error_string(
        """\
        package Test is
           type T is (Foo, Bar, Baz) with Size => 1;
        end Test;
        """,
        r'^<stdin>:2:9: error: size of "T" too small$',
    )


def test_invalid_enumeration_type_duplicate_values() -> None:
    assert_error_string(
        """\
        package Test is
           type T is (Foo => 0, Bar => 0) with Size => 1;
        end Test;
        """,
        r'^<stdin>:2:32: error: duplicate enumeration value "0"\n'
        r"<stdin>:2:22: note: previous occurrence$",
    )


def test_invalid_enumeration_type_multiple_duplicate_values() -> None:
    assert_error_string(
        """\
        package Test is
           type T is (Foo => 0, Foo_1 => 1, Bar => 0, Bar_1 => 1) with Size => 8;
        end Test;
        """,
        r"^"
        r'<stdin>:2:44: error: duplicate enumeration value "0"\n'
        r"<stdin>:2:22: note: previous occurrence\n"
        r'<stdin>:2:56: error: duplicate enumeration value "1"\n'
        r"<stdin>:2:34: note: previous occurrence"
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
        r"^<stdin>:3:9: error: conflicting literals: Bar\n"
        r'<stdin>:2:21: note: previous occurrence of "Bar"$',
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
        r'^<stdin>:7:17: error: field "Bar" does not exist in "Test::PDU"\n'
        r'<stdin>:3:9: note: type "Test::PDU" declared here$',
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
        r'<stdin>:10:10: error: undefined variable "X"\n'
        r'<stdin>:10:14: error: undefined variable "Y"'
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
        r'^<stdin>:7:9: error: duplicate declaration of "Test::PDU"\n'
        r'<stdin>:3:9: note: previous occurrence of "Test::PDU"$',
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
        r'^<stdin>:10:4: error: conflicting refinement of "Test::PDU" with "Test::PDU"\n'
        r"<stdin>:9:4: note: previous occurrence of refinement"
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
        r'^<stdin>:8:9: error: name conflict for type "Test::Bar"\n'
        r'<stdin>:7:9: note: previous occurrence of "Test::Bar"$',
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
        r'^<stdin>:4:4: error: name conflict for session "Test::X"\n'
        r'<stdin>:2:9: note: previous occurrence of "Test::X"$',
    )


def test_model_illegal_first_aspect_on_initial_link() -> None:
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
        r"^<stdin>:7:30: error: illegal first aspect on initial link$",
    )


def test_model_errors_in_type_and_session() -> None:
    assert_error_string(
        """\
        package Test is
           type T is range 0 .. 2 ** 65 - 1 with Size => 65;

           generic
           session S is
           begin
           end S;
        end Test;
        """,
        r"^"
        r'<stdin>:2:25: error: last of "T" exceeds limit \(2\*\*63 - 1\)\n'
        r"<stdin>:4:4: error: empty states"
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
        """,
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
        """,
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
        r'<stdin>:14:26: error: invalid use of "Message" in size aspect\n'
        r"<stdin>:14:26: note: remove size aspect to define field with implicit size"
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
        r"<stdin>:13:10: error: messages with implicit size may only be used"
        " for last fields\n"
        r'<stdin>:7:10: note: message field with implicit size in "Test::Inner"'
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
        r'<stdin>:8:26: error: invalid use of "Message" in size aspect\n'
        r"<stdin>:8:26: note: remove size aspect to define field with implicit size\n"
        r'<stdin>:6:26: error: "Message" must not be used in size aspects'
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
        r'^<stdin>:9:26: error: "Message" must not be used in size aspects$',
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
        r'^<stdin>:17:10: error: unreachable field "D"$',
    )


def test_fail_after_value() -> None:
    RecordFluxError.set_max_error(1)
    p = parser.Parser()
    p.parse(Path(f"{SPEC_DIR}/ethernet.rflx"))
    p.create_model()
    RecordFluxError.set_max_error(0)


def test_dependency_order() -> None:
    p = parser.Parser()
    p.parse(Path(f"{SPEC_DIR}/in_p1.rflx"))
    p.create_model()


def test_dependency_order_after_dependency_cycle() -> None:
    p = parser.Parser()

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            f"{SPEC_DIR}/invalid/context_cycle.rflx:1:6: error: dependency cycle when "
            f'including "Context_Cycle_1"\n'
            f"{SPEC_DIR}/invalid/context_cycle_1.rflx:1:6: "
            'note: when including "Context_Cycle_2"\n'
            f"{SPEC_DIR}/invalid/context_cycle_2.rflx:1:6: "
            'note: when including "Context_Cycle_3"\n'
            f"{SPEC_DIR}/invalid/context_cycle_3.rflx:1:6: "
            'note: when including "Context_Cycle_1"'
            r"$"
        ),
    ):
        p.parse(Path(f"{SPEC_DIR}/invalid/context_cycle.rflx"))

    p.parse(Path(f"{SPEC_DIR}/in_ethernet.rflx"))
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
        ID("Test::Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Tag"), location=Location((1, 1))),
            Link(
                Field("Tag"),
                Field("Length"),
                expr.Equal(
                    expr.Variable("Tag"),
                    expr.Variable("Msg_Data"),
                    location=Location((3, 3)),
                ),
                location=Location((2, 2)),
            ),
            Link(
                Field("Tag"),
                FINAL,
                expr.Equal(
                    expr.Variable("Tag"),
                    expr.Variable("Msg_Error"),
                    location=Location((5, 5)),
                ),
                location=Location((3, 3)),
            ),
            Link(
                Field("Length"),
                Field("Value"),
                size=expr.Mul(expr.Variable("Length"), expr.Number(8), location=Location((4, 3))),
                location=Location((4, 4)),
            ),
            Link(Field("Value"), FINAL, location=Location((5, 5))),
        ],
        {
            Field(ID("Tag", location=Location((1, 1)))): tag,
            Field(ID("Length", location=Location((1, 1)))): length,
            Field(ID("Value", location=Location((1, 1)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
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
                            expr.Equal(
                                expr.Call("G", rty.BOOLEAN, [expr.Variable("F")]),
                                expr.TRUE,
                            ),
                        ),
                        description="rfc1149.txt+45:4-47:8",
                    ),
                    Transition("A"),
                ],
                exception_transition=Transition("null"),
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
    ("rfi_content", "match_error"),
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
""",
    )
    test_spec = tmp_path / "test.rflx"
    test_rfi = tmp_path / "test.rfi"
    test_spec.write_text(content, encoding="utf-8")
    test_rfi.write_text(rfi_content)
    if not match_error:
        p.parse(test_spec)
        p.create_model()
    else:
        regex = re.compile(rf"^test.rfi:1:1: error: {match_error}$", re.DOTALL)
        with pytest.raises(RecordFluxError, match=regex):  # noqa: PT012
            p.parse(test_spec)
            p.create_model()


def test_parse_error_negated_variable() -> None:
    assert_error_string(
        """\
            package P1 is
               type Kind is range 0 .. 2 ** 16 - 1 with Size => - Cobra16;
            end P1;
            """,
        r'^<stdin>:2:55: error: size of "Kind" contains variable$',
    )


def test_package_name_not_correct(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Not_Test is

            end Not_Test;
            """,
        ),
    )

    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
                info: Parsing {temp_file}
                error: source file name does not match the package name "Not_Test"
                 --> {temp_file}:1:9
                  |
                1 | package Not_Test is
                  |         ^^^^^^^^
                  |
                help: either rename the file to "not_test.rflx" or change the package name to "Test"
                 --> {temp_file}:1:9
                  |
                1 | package Not_Test is
                  |         -------- help: rename to "Test"
                2 |
                3 | end Not_Test;
                  |     -------- help: rename to "Test"
                  |
            """,
        ),
        capfd,
    )


@pytest.mark.parametrize(
    "package_name",
    [
        "Tls",
        "TLS",
        "BadCasing",
    ],
)
def test_package_bad_package_casing(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
    package_name: str,
) -> None:
    temp_file = tmp_path / f"{package_name}.rflx"
    temp_file.write_text(
        textwrap.dedent(
            f"""\
            package {package_name} is

            end {package_name};
            """,
        ),
    )

    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
                info: Parsing {temp_file}
                error: source file name "{package_name}.rflx" must be in lower case characters only
                 --> {temp_file}:1:9
                  |
                1 | package {package_name} is
                  |         {'^' * len(package_name)}
                  |
                help: rename the file to "{package_name.lower()}.rflx"
                 --> {temp_file}:1:9
                  |
                1 | package {package_name} is
                  |         {'-' * len(package_name)}
                  |
            """,
        ),
        capfd,
    )


def test_message_size_not_multiple_8_bits(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
        package Test is
           type I is range 0 .. 2 with Size => 3;
           type M is
              message
                 A : I;
              end message;
        end Test;
        """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
                info: Parsing {file_path}
                info: Processing Test
                info: Verifying __BUILTINS__::Boolean
                info: Verifying __INTERNAL__::Opaque
                info: Verifying Test::I
                info: Verifying Test::M
                error: message size must be multiple of 8 bit
                 --> {file_path}:3:9
                  |
                3 |    type M is
                  |         ^
                4 |       message
                5 |          A : I;
                  |          - note: on path "A"
                  |
          """,
        ),
        capfd,
    )


def test_message_negative_field_size(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
        package Test is
           type I is range 0 .. 2 ** 32 - 1 with Size => 32;
           type M is
              message
                 F1 : I
                    then F2
                       with Size => 0 - 8;
                 F2 : Opaque;
              end message;
        end Test;
     """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
                    info: Parsing {temp_file}
                    info: Processing Test
                    info: Verifying __BUILTINS__::Boolean
                    info: Verifying __INTERNAL__::Opaque
                    info: Verifying Test::I
                    info: Verifying Test::M
                    error: negative size for field "F2"
                     --> {temp_file}:7:29
                      |
                    5 |          F1 : I
                      |          -- note: on path "F1"
                    6 |             then F2
                    7 |                with Size => 0 - 8;
                      |                             ^^^^^
                      |
               """,
        ),
        capfd,
    )


def test_field_size_not_multiple_8_bits_path_annotations(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
                package Test is
                   type I is range 0 .. 2 with Size => 3;
                   type M is
                      message
                         A : Opaque with Size => 8;
                         B : Opaque with Size => 8;
                         C : Opaque with Size => 3;
                         D : I;
                      end message;
                end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
                info: Parsing {file_path}
                info: Processing Test
                info: Verifying __BUILTINS__::Boolean
                info: Verifying __INTERNAL__::Opaque
                info: Verifying Test::I
                info: Verifying Test::M
                error: size of opaque field "C" not multiple of 8 bit
                 --> {file_path}:7:34
                  |
                6 |          B : Opaque with Size => 8;
                  |          - note: on path "B"
                7 |          C : Opaque with Size => 3;
                  |                                  ^
                  |
                help: sizes are expressed in bits, not bytes
                 --> {file_path}:7:34
                  |
                7 |          C : Opaque with Size => 3;
                  |                                  - help: did you mean "3 * 8"?
                  |
          """,
        ),
        capfd,
    )


def test_field_size_not_multiple_8_bits_opaque_field(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
        package Test is
           type M is
              message
                 A : Opaque
                    with Size => 3;
              end message;
        end Test;
        """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
                info: Parsing {file_path}
                info: Processing Test
                info: Verifying __BUILTINS__::Boolean
                info: Verifying __INTERNAL__::Opaque
                info: Verifying Test::M
                error: size of opaque field "A" not multiple of 8 bit
                 --> {file_path}:5:26
                  |
                5 |             with Size => 3;
                  |                          ^
                  |
                help: sizes are expressed in bits, not bytes
                 --> {file_path}:5:26
                  |
                5 |             with Size => 3;
                  |                          - help: did you mean "3 * 8"?
                  |
            """,
        ),
        capfd,
    )


def test_not_aligned_to_8_bits_multiple_fields(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 0 .. 2 with Size => 3;
               type I2 is range 0 .. 2 with Size => 5;
               type M is
                  message
                     A : Opaque
                        with Size => 8;
                     B : Opaque
                        with Size => 8;
                     D : I;
                     E : Opaque
                        with Size => 8;
                     F : I2;
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
                info: Parsing {file_path}
                info: Processing Test
                info: Verifying __BUILTINS__::Boolean
                info: Verifying __INTERNAL__::Opaque
                info: Verifying Test::I
                info: Verifying Test::I2
                info: Verifying Test::M
                error: opaque field "E" not aligned to 8 bit boundary
                  --> {file_path}:6:10
                   |
                 6 |          A : Opaque
                   |          - note: on path "A"
                 7 |             with Size => 8;
                 8 |          B : Opaque
                   |          - note: on path "B"
                 9 |             with Size => 8;
                10 |          D : I;
                   |          - note: on path "D"
                11 |          E : Opaque
                   |          ^ a previous field in the path may be the cause of this error
                   |
              """,
        ),
        capfd,
    )


def test_message_negative_field_start(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type M14 is
                  message
                     F1 : Opaque
                        with Size => 0 - 8;
                  end message;
            end Test;
             """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
                    info: Parsing {temp_file}
                    info: Processing Test
                    info: Verifying __BUILTINS__::Boolean
                    info: Verifying __INTERNAL__::Opaque
                    info: Verifying Test::M14
                    error: negative size for field "F1"
                     --> {temp_file}:5:26
                      |
                    5 |             with Size => 0 - 8;
                      |                          ^^^^^
                      |
                    error: negative start for end of message
                     --> {temp_file}:2:9
                      |
                    2 |    type M14 is
                      |         ^^^
                    3 |       message
                    4 |          F1 : Opaque
                      |          -- note: on path "F1"
                      |          -- note: unsatisfied "F1'Last = (Message'First + (0 - 8)) - 1"
                      |          -- note: unsatisfied "F1'Last + 1 >= Message'First"
                      |
               """,
        ),
        capfd,
    )


def test_type_range_first_has_variable(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range X + 1 .. 100 with Size => 8;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: first of "T" contains variable
             --> {file_path}:2:20
              |
            2 |    type T is range X + 1 .. 100 with Size => 8;
              |                    ^
              |
              """,
        ),
        capfd,
    )


def test_type_range_first_has_multiple_variables(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range X + Y + 1 .. 100 with Size => 8;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: first of "T" contains variable
             --> {file_path}:2:20
              |
            2 |    type T is range X + Y + 1 .. 100 with Size => 8;
              |                    ^
              |
            error: first of "T" contains variable
             --> {file_path}:2:24
              |
            2 |    type T is range X + Y + 1 .. 100 with Size => 8;
              |                        ^
              |
              """,
        ),
        capfd,
    )


def test_type_range_last_has_variable(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range 0 .. X + 1 with Size => 8;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: last of "T" contains variable
             --> {file_path}:2:25
              |
            2 |    type T is range 0 .. X + 1 with Size => 8;
              |                         ^
              |
              """,
        ),
        capfd,
    )


def test_type_range_last_has_multiple_variables(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range 0 .. X + Y + 1 with Size => 8;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: last of "T" contains variable
             --> {file_path}:2:25
              |
            2 |    type T is range 0 .. X + Y + 1 with Size => 8;
              |                         ^
              |
            error: last of "T" contains variable
             --> {file_path}:2:29
              |
            2 |    type T is range 0 .. X + Y + 1 with Size => 8;
              |                             ^
              |
              """,
        ),
        capfd,
    )


def test_type_range_size_has_variable(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range 0 .. 255 with Size => X;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: size of "T" contains variable
             --> {file_path}:2:42
              |
            2 |    type T is range 0 .. 255 with Size => X;
              |                                          ^
              |
              """,
        ),
        capfd,
    )


def test_type_range_size_has_multiple_variables(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range 0 .. 255 with Size => X + Y;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: size of "T" contains variable
             --> {file_path}:2:42
              |
            2 |    type T is range 0 .. 255 with Size => X + Y;
              |                                          ^
              |
            error: size of "T" contains variable
             --> {file_path}:2:46
              |
            2 |    type T is range 0 .. 255 with Size => X + Y;
              |                                              ^
              |
              """,
        ),
        capfd,
    )


def test_type_range_last_is_aggregate(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 0 .. [1, 2, 3] with Size => 8;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {temp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: last of "I" contains aggregate
             --> {temp_file}:2:25
              |
            2 |    type I is range 0 .. [1, 2, 3] with Size => 8;
              |                         ^^^^^^^^^
              |
               """,
        ),
        capfd,
    )


def test_type_range_first_is_aggregate(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range [1, 2, 3] .. 255 with Size => 8;
            end Test;
             """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {temp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: first of "I" contains aggregate
             --> {temp_file}:2:20
              |
            2 |    type I is range [1, 2, 3] .. 255 with Size => 8;
              |                    ^^^^^^^^^
              |
               """,
        ),
        capfd,
    )


def test_type_range_last_exceeds_limit(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range 0 .. 2 ** 64 - 1 with Size => 64;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: last of "T" exceeds limit (2**63 - 1)
             --> {file_path}:2:25
              |
            2 |    type T is range 0 .. 2 ** 64 - 1 with Size => 64;
              |                         ^^^^^^^^^^^
              |
              """,
        ),
        capfd,
    )


def test_type_range_size_too_small(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range 0 .. 47 with Size => 2;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: size of "T" too small
             --> {file_path}:2:41
              |
            2 |    type T is range 0 .. 47 with Size => 2;
              |                                         ^
              |                         -- help: at least 6 bits are required to store the upper \
bound
              |
              """,
        ),
        capfd,
    )


def test_type_range_negative_range(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type T is range 255 .. 0 with Size => 8;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: range of "T" negative
             --> {file_path}:2:20
              |
            2 |    type T is range 255 .. 0 with Size => 8;
              |                    ^^^^^^^^
              |
              """,
        ),
        capfd,
    )


def test_type_range_size_is_aggregate(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 0 .. 255 with Size => [1, 2, 3];
            end Test;
             """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {temp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: size of "I" contains aggregate
             --> {temp_file}:2:42
              |
            2 |    type I is range 0 .. 255 with Size => [1, 2, 3];
              |                                          ^^^^^^^^^
              |
               """,
        ),
        capfd,
    )


def test_type_range_first_not_integer(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range [1, 2, 3] * 2 .. 255 with Size => 8;
            end Test;
             """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {temp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: first of "I" is not an integer
             --> {temp_file}:2:20
              |
            2 |    type I is range [1, 2, 3] * 2 .. 255 with Size => 8;
              |                    ^^^^^^^^^^^^^
              |
               """,
        ),
        capfd,
    )


def test_type_range_last_not_integer(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 1 .. [1, 2, 3] with Size => 8;
            end Test;
             """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {temp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: last of "I" contains aggregate
             --> {temp_file}:2:25
              |
            2 |    type I is range 1 .. [1, 2, 3] with Size => 8;
              |                         ^^^^^^^^^
              |
               """,
        ),
        capfd,
    )


def test_type_range_size_not_integer(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    temp_file = tmp_path / "test.rflx"
    temp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 1 .. 255 with Size => [1, 2, 3] * 2;
            end Test;
             """,
        ),
    )
    assert_error_full_message(
        temp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {temp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: size of "I" is not an integer
             --> {temp_file}:2:42
              |
            2 |    type I is range 1 .. 255 with Size => [1, 2, 3] * 2;
              |                                          ^^^^^^^^^^^^^
              |
               """,
        ),
        capfd,
    )


def test_invalid_refinement_base(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    tmp_file = tmp_path / "test.rflx"
    tmp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 0 .. 255 with Size => 8;
               type I2 is new I;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        tmp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {tmp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::I
            error: invalid derivation
             --> {tmp_file}:3:9
              |
            2 |    type I is range 0 .. 255 with Size => 8;
              |         - note: base type must be a message
            3 |    type I2 is new I;
              |         ^^
              |
              """,
        ),
        capfd,
    )


def test_refinement_undefined_field(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    tmp_file = tmp_path / "test.rflx"
    tmp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type M is
                  message
                     F : Opaque
                        with Size => 8;
                  end message;
               type N is
                  message
                     G : Opaque
                        with Size => 16;
                  end message;

               for M use (Undef_Field => N);
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        tmp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {tmp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::M
            info: Verifying Test::N
            info: Verifying Test::__REFINEMENT__Test_N__Test_M__Undef_Field__
            error: field "Undef_Field" does not exist in "Test::M"
              --> {tmp_file}:13:15
               |
             2 |    type M is
               |         - note: type "Test::M" declared here
             3 |       message
            ...
            12 |
            13 |    for M use (Undef_Field => N);
               |               ^^^^^^^^^^^
               |
              """,
        ),
        capfd,
    )


def test_refinement_similar_name(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    tmp_file = tmp_path / "test.rflx"
    tmp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type M is
                  message
                     My_Field : Opaque
                        with Size => 8;
                  end message;
               type N is
                  message
                     G : Opaque
                        with Size => 16;
                  end message;

               for M use (My_Feld => N);
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        tmp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {tmp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::M
            info: Verifying Test::N
            info: Verifying Test::__REFINEMENT__Test_N__Test_M__My_Feld__
            error: field "My_Feld" does not exist in "Test::M"
              --> {tmp_file}:13:15
               |
             2 |    type M is
               |         - note: type "Test::M" declared here
             3 |       message
             4 |          My_Field : Opaque
               |          -------- help: field with similar name
             5 |             with Size => 8;
            ...
            12 |
            13 |    for M use (My_Feld => N);
               |               ^^^^^^^
               |
              """,
        ),
        capfd,
    )


def test_condition_is_always_true(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
                package Test is
                   type T is range 0 .. 255 with Size => 8;
                   type M is
                      message
                         A : T
                            then B
                               if A < 1000;
                        B : T;
                      end message;
                end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::T
            info: Verifying Test::M
            error: condition is always true
             --> {file_path}:7:19
              |
            2 |    type T is range 0 .. 255 with Size => 8;
              |         ---------------------------------- note: unsatisfied "A <= 255"
            3 |    type M is
            ...
            6 |             then B
            7 |                if A < 1000;
              |                   ^^^^^^^^ proven to be always true
              |                   -------- note: unsatisfied "(A < 1000) = False"
              |
              """,
        ),
        capfd,
    )


def test_incompatible_type_link_condition(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type M14 is
                  message
                     F1 : Opaque
                        with Size => 8
                        then F2
                           if F1 = 32;
                     F2 : Opaque
                        with Size => 8;
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::M14
            error: expected sequence type "__INTERNAL__::Opaque" with element integer type \
"__INTERNAL__::Byte" (0 .. 255)
             --> {file_path}:7:24
              |
            4 |          F1 : Opaque
              |          -- note: on path "F1"
            5 |             with Size => 8
            6 |             then F2
            7 |                if F1 = 32;
              |                        ^^ found type universal integer (32 .. 32)
              |
              """,
        ),
        capfd,
    )


def test_aggregate_type_in_condition(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 0 .. 255 with Size => 8;
               type M (A : I) is
                  message
                     X : Boolean
                        then Z
                           if [1, 2, 3] = A;
                     Z : Opaque
                        with Size => 8;
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::I
            info: Verifying Test::M
            error: expected aggregate with element type universal integer (1 .. 3)
             --> {file_path}:7:31
              |
            5 |          X : Boolean
              |          - note: on path "X"
            6 |             then Z
            7 |                if [1, 2, 3] = A;
              |                               ^ found integer type "Test::I" (0 .. 255)
              |
              """,
        ),
        capfd,
    )


def test_opaque_field_size_is_aggregate(tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    tmp_file = tmp_path / "test.rflx"
    tmp_file.write_text(
        textwrap.dedent(
            """\
            package Test is
               type M is
                  message
                     Z : Opaque
                        with Size => [1, 2, 3];
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        tmp_file,
        textwrap.dedent(
            f"""\
            info: Parsing {tmp_file}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::M
            error: expected integer type "__BUILTINS__::Base_Integer" (0 .. 2**63 - 1)
             --> {tmp_file}:5:26
              |
            5 |             with Size => [1, 2, 3];
              |                          ^^^^^^^^^ found aggregate with element type universal \
integer (1 .. 3)
              |
              """,
        ),
        capfd,
    )


def test_parameter_non_scalar(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type N is
                  message
                    D : Opaque with Size => 8;
                  end message;

               type M (x : N) is
                  message
                    A : Opaque with Size => 8;
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::N
            error: expected enumeration type or integer type
             --> {file_path}:7:12
              |
            2 |    type N is
              |         - note: type declared here
            3 |       message
            ...
            6 |
            7 |    type M (x : N) is
              |            ^ found message type "Test::N"
              |
              """,
        ),
        capfd,
    )


def test_parameter_non_scalar_and_builtin_type(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type M (x : Opaque) is
                  message
                    A : Opaque with Size => 8;
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            error: expected enumeration type or integer type
             --> {file_path}:2:12
              |
            2 |    type M (x : Opaque) is
              |            ^ found sequence type "__INTERNAL__::Opaque" with element integer type \
"__INTERNAL__::Byte" (0 .. 255)
              |
              """,
        ),
        capfd,
    )


@pytest.mark.parametrize(
    "keyword",
    RESERVED_WORDS,
)
def test_reserved_word_link_condition(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
    keyword: str,
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            f"""\
            package Test is
               type I is range 0 .. 255 with Size => 8;
               type M (A : Boolean) is
                  message
                     X : Boolean
                        then Z
                           if A and {keyword};
                     Z : Opaque
                        with Size => 8;
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::I
            info: Verifying Test::M
            error: reserved word "{keyword}" used as identifier
             --> {file_path}:7:25
              |
            7 |                if A and {keyword};
              |                         {"^".rjust(len(keyword), "^")}
              |
            error: undefined variable "{keyword}"
             --> {file_path}:7:25
              |
            7 |                if A and {keyword};
              |                         {"^".rjust(len(keyword), "^")}
              |
              """,
        ),
        capfd,
    )


def test_size_aspect_defined_twice_in_message_field(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
) -> None:
    file_path = tmp_path / "test.rflx"
    file_path.write_text(
        textwrap.dedent(
            """\
            package Test is
               type I is range 0 .. 255 with Size => 8;
               type M is
                  message
                     One : I
                        with Size => 16;
                     Two : I;
                  end message;
            end Test;
            """,
        ),
    )
    assert_error_full_message(
        file_path,
        textwrap.dedent(
            f"""\
            info: Parsing {file_path}
            info: Processing Test
            info: Verifying __BUILTINS__::Boolean
            info: Verifying __INTERNAL__::Opaque
            info: Verifying Test::I
            info: Verifying Test::M
            error: fixed size field "One" does not permit a size aspect
             --> {file_path}:6:26
              |
            2 |    type I is range 0 .. 255 with Size => 8;
              |                                          - note: associated type size \
defined here
            3 |    type M is
            4 |       message
            5 |          One : I
              |          --- help: modify this field's type, or alternatively, remove the \
size aspect
            6 |             with Size => 16;
              |                          ^^
              |
          """,
        ),
        capfd,
    )
