from __future__ import annotations

import textwrap
from collections import abc

import pytest

import rflx.typing_ as rty
from rflx import expr
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    OPAQUE,
    Session,
    State,
    Transition,
    TypeDecl,
    UncheckedSession,
    declaration as decl,
    statement as stmt,
)
from rflx.rapidflux import Location, RecordFluxError
from tests.data import models
from tests.utils import assert_equal, assert_session_model_error, get_test_model


def test_str() -> None:
    assert_equal(
        str(
            Session(
                "P::S",
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
                        description="rfc1149.txt+51:4-52:9",
                    ),
                ],
                [
                    decl.VariableDeclaration("M", "TLV::Message"),
                    decl.VariableDeclaration("Y", BOOLEAN.identifier, expr.FALSE),
                ],
                [
                    decl.ChannelDeclaration("X", readable=True, writable=True),
                    decl.FunctionDeclaration("F", [], BOOLEAN.identifier),
                    decl.FunctionDeclaration(
                        "G",
                        [decl.Argument("P", BOOLEAN.identifier)],
                        BOOLEAN.identifier,
                    ),
                ],
                [BOOLEAN, models.tlv_message()],
            ),
        ),
        textwrap.dedent(
            """\
            generic
               X : Channel with Readable, Writable;
               with function F return Boolean;
               with function G (P : Boolean) return Boolean;
            session S is
               M : TLV::Message;
               Y : Boolean := False;
            begin
               state A is
               begin
                  X'Read (M);
               transition
                  goto B
               end A;

               state B
                  with Desc => "rfc1149.txt+51:4-52:9"
               is
                  Z : Boolean := Y;
               begin
               transition
                  goto null
                     with Desc => "rfc1149.txt+45:4-47:8"
                     if Z = True
                        and G (F) = True
                  goto A
               end B;
            end S""",
        ),
    )


def test_identifier_normalization(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(Session, "_check_identifiers", lambda _: None)
    assert str(
        Session(
            "P::S",
            [
                State(
                    "A",
                    declarations=[],
                    actions=[stmt.Read("x", expr.Variable("m"))],
                    transitions=[
                        Transition("b"),
                    ],
                ),
                State(
                    "B",
                    declarations=[
                        decl.VariableDeclaration("Z", BOOLEAN.identifier, expr.Variable("y")),
                    ],
                    actions=[],
                    transitions=[
                        Transition(
                            "null",
                            condition=expr.And(
                                expr.Equal(expr.Variable("z"), expr.TRUE),
                                expr.Equal(
                                    expr.Call("g", rty.BOOLEAN, [expr.Variable("f")]),
                                    expr.TRUE,
                                ),
                            ),
                        ),
                        Transition("a"),
                    ],
                ),
            ],
            [
                decl.VariableDeclaration("M", "tlv::message"),
                decl.VariableDeclaration("Y", BOOLEAN.identifier, expr.FALSE),
            ],
            [
                decl.ChannelDeclaration("X", readable=True, writable=True),
                decl.FunctionDeclaration("F", [], BOOLEAN.identifier),
                decl.FunctionDeclaration(
                    "G",
                    [decl.Argument("P", BOOLEAN.identifier)],
                    BOOLEAN.identifier,
                ),
            ],
            [BOOLEAN, models.tlv_message()],
        ),
    ) == textwrap.dedent(
        """\
            generic
               X : Channel with Readable, Writable;
               with function F return Boolean;
               with function G (P : Boolean) return Boolean;
            session S is
               M : TLV::Message;
               Y : Boolean := False;
            begin
               state A is
               begin
                  X'Read (M);
               transition
                  goto B
               end A;

               state B is
                  Z : Boolean := Y;
               begin
               transition
                  goto null
                     if Z = True
                        and G (F) = True
                  goto A
               end B;
            end S""",
    )


def test_inconsistent_identifier_casing() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:1: error: casing of "tlv::message" differs from casing'
            r' in the declaration of "TLV::Message" at <stdin>:1:1\n'
            r'<stdin>:1:1: note: declaration of "TLV::Message"\n'
            r'<stdin>:2:2: error: casing of "x" differs from casing'
            r' in the declaration of "X" at <stdin>:12:12\n'
            r'<stdin>:12:12: note: declaration of "X"\n'
            r'<stdin>:3:3: error: casing of "m" differs from casing'
            r' in the declaration of "M" at <stdin>:13:13\n'
            r'<stdin>:13:13: note: declaration of "M"\n'
            r'<stdin>:4:4: error: casing of "b" differs from casing'
            r' in the declaration of "B" at <stdin>:14:14\n'
            r'<stdin>:14:14: note: declaration of "B"\n'
            r'<stdin>:5:5: error: casing of "y" differs from casing'
            r' in the declaration of "Y" at <stdin>:15:15\n'
            r'<stdin>:15:15: note: declaration of "Y"\n'
            r'<stdin>:6:6: error: casing of "z" differs from casing'
            r' in the declaration of "Z" at <stdin>:16:16\n'
            r'<stdin>:16:16: note: declaration of "Z"\n'
            r'<stdin>:7:7: error: casing of "g" differs from casing'
            r' in the declaration of "G" at <stdin>:17:17\n'
            r'<stdin>:17:17: note: declaration of "G"\n'
            r'<stdin>:8:8: error: casing of "f" differs from casing'
            r' in the declaration of "F" at <stdin>:18:18\n'
            r'<stdin>:18:18: note: declaration of "F"\n'
            r'<stdin>:9:9: error: casing of "a" differs from casing'
            r' in the declaration of "A" at <stdin>:19:19\n'
            r'<stdin>:19:19: note: declaration of "A"'
            r"$"
        ),
    ):
        Session(
            "P::S",
            [
                State(
                    ID("A", location=Location((19, 19))),
                    declarations=[],
                    actions=[
                        stmt.Read(
                            ID("x", location=Location((2, 2))),
                            expr.Variable(ID("m", location=Location((3, 3)))),
                        ),
                    ],
                    transitions=[
                        Transition(ID("b", location=Location((4, 4)))),
                    ],
                ),
                State(
                    ID("B", location=Location((14, 14))),
                    declarations=[
                        decl.VariableDeclaration(
                            ID("Z", location=Location((16, 16))),
                            BOOLEAN.identifier,
                            expr.Variable(ID("y", location=Location((5, 5)))),
                        ),
                    ],
                    actions=[],
                    transitions=[
                        Transition(
                            "null",
                            condition=expr.And(
                                expr.Equal(
                                    expr.Variable(ID("z", location=Location((6, 6)))),
                                    expr.TRUE,
                                ),
                                expr.Equal(
                                    expr.Call(
                                        ID("g", location=Location((7, 7))),
                                        rty.BOOLEAN,
                                        [expr.Variable(ID("f", location=Location((8, 8))))],
                                    ),
                                    expr.TRUE,
                                ),
                            ),
                        ),
                        Transition(ID("a", location=Location((9, 9)))),
                    ],
                ),
            ],
            [
                decl.VariableDeclaration(
                    ID("M", location=Location((13, 13))),
                    ID("tlv::message", location=Location((1, 1))),
                ),
                decl.VariableDeclaration(
                    ID("Y", location=Location((15, 15))),
                    BOOLEAN.identifier,
                    expr.FALSE,
                ),
            ],
            [
                decl.ChannelDeclaration(
                    ID("X", location=Location((12, 12))),
                    readable=True,
                    writable=True,
                ),
                decl.FunctionDeclaration(
                    ID("F", location=Location((18, 18))),
                    [],
                    BOOLEAN.identifier,
                ),
                decl.FunctionDeclaration(
                    ID("G", location=Location((17, 17))),
                    [decl.Argument("P", BOOLEAN.identifier)],
                    BOOLEAN.identifier,
                ),
            ],
            [BOOLEAN, models.tlv_message()],
        )


def test_invalid_name() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:20: error: invalid format for identifier "P::S::X"$',
    ):
        Session(
            identifier=ID("P::S::X", location=Location((10, 20))),
            states=[
                State("Start", transitions=[Transition(target=ID("null"))]),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_empty_states() -> None:
    assert_session_model_error(
        states=[],
        declarations=[],
        parameters=[],
        types=[],
        regex=r"^<stdin>:1:1: error: empty states$",
    )


def test_invalid_target_state() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(target=ID("NonExistent", location=Location((10, 20)))),
                ],
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:20: error: transition from state "Start" to non-existent'
            r' state "NonExistent" in "P::S"'
            r"$"
        ),
    )


def test_duplicate_state() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                location=Location((10, 20)),
            ),
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                location=Location((10, 30)),
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:30: error: duplicate state "Start"\n'
            r'<stdin>:10:20: note: previous definition of state "Start"'
            r"$"
        ),
    )


def test_multiple_duplicate_states() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("Foo"))],
                location=Location((10, 20)),
            ),
            State(
                "Start",
                transitions=[Transition(target=ID("Foo"))],
                location=Location((10, 30)),
            ),
            State(
                "Foo",
                transitions=[Transition(target=ID("Bar"))],
                location=Location((10, 40)),
            ),
            State(
                "Bar",
                transitions=[Transition(target=ID("null"))],
                location=Location((10, 50)),
            ),
            State(
                "Foo",
                transitions=[Transition(target=ID("Bar"))],
                location=Location((10, 60)),
            ),
            State(
                "Bar",
                transitions=[Transition(target=ID("null"))],
                location=Location((10, 70)),
            ),
            State(
                "Foo",
                transitions=[Transition(target=ID("Bar"))],
                location=Location((10, 80)),
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:30: error: duplicate state "Start"\n'
            r'<stdin>:10:20: note: previous definition of state "Start"\n'
            r'<stdin>:10:60: error: duplicate state "Foo"\n'
            r'<stdin>:10:40: note: previous definition of state "Foo"\n'
            r'<stdin>:10:80: error: duplicate state "Foo"\n'
            r'<stdin>:10:40: note: previous definition of state "Foo"\n'
            r'<stdin>:10:70: error: duplicate state "Bar"\n'
            r'<stdin>:10:50: note: previous definition of state "Bar"'
            r"$"
        ),
    )


def test_unreachable_state() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("null"))]),
            State(
                "Unreachable",
                transitions=[Transition(target=ID("null"))],
                location=Location((10, 20)),
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: error: unreachable state "Unreachable"$',
    )


def test_multiple_unreachable_states() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("null"))]),
            State(
                "Unreachable1",
                transitions=[Transition(target=ID("null"))],
                location=Location((10, 20)),
            ),
            State(
                "Unreachable2",
                transitions=[Transition(target=ID("null"))],
                location=Location((10, 30)),
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:20: error: unreachable state "Unreachable1"\n'
            r'<stdin>:10:30: error: unreachable state "Unreachable2"'
            r"$"
        ),
    )


def test_detached_state() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null")), Transition(target=ID("Detached"))],
            ),
            State("Detached", location=Location((10, 20))),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: error: detached state "Detached"$',
    )


def test_multiple_detached_states() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(target=ID("null")),
                    Transition(target=ID("Detached1")),
                    Transition(target=ID("Detached2")),
                ],
            ),
            State("Detached1", location=Location((10, 20))),
            State("Detached2", location=Location((10, 30))),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:20: error: detached state "Detached1"\n'
            r'<stdin>:10:30: error: detached state "Detached2"'
            r"$"
        ),
    )


def test_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(
                            expr.Variable("Undefined", location=Location((10, 20))),
                            expr.TRUE,
                        ),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: error: undefined variable "Undefined"$',
    )


def test_undefined_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("Defined"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Defined", "Undefined_Type", location=Location((10, 20))),
        ],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: error: undefined type "Undefined_Type"$',
    )


def test_declared_variable() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(
                            expr.Variable("Defined"),
                            expr.Variable("TLV::Msg_Data"),
                        ),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Defined", "TLV::Tag")],
        parameters=[],
        types=[models.tlv_tag()],
    )


def test_declared_local_variable() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("Local"), expr.Variable("Global")),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[decl.VariableDeclaration("Local", "Boolean")],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_undeclared_local_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(target=ID("State"), condition=expr.Variable("Global")),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
            ),
            State(
                "State",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(
                            expr.Variable("Local", location=Location((10, 20))),
                            expr.Variable("Global"),
                        ),
                    ),
                    Transition(
                        target=ID("State"),
                    ),
                ],
                declarations=[],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined variable "Local"$',
    )


def test_declared_local_variable_valid() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Valid(expr.Variable("Global")), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "TLV::Message")],
        parameters=[],
        types=[models.tlv_message()],
    )


def test_declared_local_variable_message_field() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(
                            expr.Selected(expr.Variable("Global"), "Length"),
                            expr.Number(1),
                        ),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                exception_transition=Transition(
                    target=ID("null"),
                ),
                declarations=[],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "TLV::Message")],
        parameters=[],
        types=[models.tlv_message()],
    )


def test_assignment_to_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.VariableAssignment("Undefined", expr.FALSE, location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: error: undefined variable "Undefined"$',
    )


def test_assignment_from_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Global",
                        expr.Variable("Undefined", location=Location((10, 20))),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined variable "Undefined"$',
    )


def test_assignment_with_undeclared_message_in_delta_message_aggregate() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Global",
                        expr.DeltaMessageAggregate("Undefined", {}, location=Location((10, 20))),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined message "Undefined"$',
    )


def test_reset_of_undeclared_list() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[stmt.Reset("Undefined", location=Location((10, 20)))],
            ),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: error: undefined variable "Undefined"$',
    )


def test_reset_incompatible() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[stmt.Reset("Global", location=Location((10, 20)))],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^<stdin>:10:20: error: expected sequence type or message type\n"
            r'<stdin>:10:20: note: found enumeration type "__BUILTINS__::Boolean"$'
        ),
    )


def test_call_to_undeclared_function() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Global",
                        expr.Call(
                            "UndefSub",
                            rty.UNDEFINED,
                            [expr.Variable("Global")],
                            location=Location((10, 20)),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined function "UndefSub"$',
    )


def test_call_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("Result"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Result",
                        expr.Call(
                            "SubProg",
                            rty.BOOLEAN,
                            [expr.Variable("Undefined", location=Location((10, 20)))],
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("SubProg", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined variable "Undefined"$',
    )


def test_call_invalid_argument_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Result",
                        expr.Call(
                            "Function",
                            rty.BOOLEAN,
                            [expr.Variable("Channel", location=Location((10, 20)))],
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("Function", [decl.Argument("P", "Boolean")], "Boolean"),
            decl.ChannelDeclaration("Channel", readable=True, writable=False),
        ],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'<stdin>:10:20: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:10:20: note: found readable channel"
            "$"
        ),
    )


def test_call_missing_arguments() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Result",
                        expr.Call(
                            "Function",
                            rty.BOOLEAN,
                            location=Location((10, 20)),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("Function", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r"^<stdin>:10:20: error: missing function arguments$",
    )


def test_call_too_many_arguments() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Result",
                        expr.Call(
                            "Function",
                            rty.BOOLEAN,
                            [expr.TRUE, expr.Number(1)],
                            location=Location((10, 20)),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("Function", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r"^<stdin>:10:20: error: too many function arguments$",
    )


def test_channel_read() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[stmt.Read("Some_Channel", expr.Variable("Global"))],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Global", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Some_Channel", readable=True, writable=False),
        ],
        types=[models.tlv_message()],
    )


def test_channel_write() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[stmt.Write("Some_Channel", expr.Variable("M"))],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("M", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Some_Channel", readable=False, writable=True),
        ],
        types=[models.tlv_message()],
    )


def test_channel_read_undeclared() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.Read("Undeclared", expr.Variable("Result"), location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Result", "TLV::Message")],
        parameters=[],
        types=[models.tlv_message()],
        regex=r'^<stdin>:10:20: error: undefined channel "Undeclared"$',
    )


def test_channel_read_invalid_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.Read(
                        "Result",
                        expr.Number(0, location=Location((10, 30))),
                        location=Location((10, 20)),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Result", "TLV::Message")],
        parameters=[],
        types=[models.tlv_message()],
        regex=(
            r"^"
            r"<stdin>:10:20: error: channel parameter must be a variable\n"
            r"<stdin>:10:20: error: expected readable channel\n"
            r'<stdin>:10:20: note: found message type "TLV::Message"\n'
            r"<stdin>:10:30: error: expected message type\n"
            r"<stdin>:10:30: note: found type universal integer \(0\)"
            r"$"
        ),
    )


def test_channel_read_invalid_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.Read("Channel", expr.Variable("Result"), location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Channel", readable=False, writable=True),
        ],
        types=[models.tlv_message()],
        regex=(
            r"^"
            r"<stdin>:10:20: error: expected readable channel\n"
            r"<stdin>:10:20: note: found writable channel"
            r"$"
        ),
    )


def test_channel_write_invalid_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.Write("Out_Channel", expr.Variable("Result"), location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Out_Channel", readable=True, writable=False),
        ],
        types=[models.tlv_message()],
        regex=(
            r"^"
            r"<stdin>:10:20: error: expected writable channel\n"
            r"<stdin>:10:20: note: found readable channel"
            r"$"
        ),
    )


def test_channel_attribute_has_data() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[stmt.VariableAssignment("Result", expr.HasData(expr.Variable("Message")))],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[],
        types=[BOOLEAN, models.tlv_message()],
    )


def test_undeclared_variable_in_function_call() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("Result"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Result",
                        expr.Call(
                            "SubProg",
                            rty.BOOLEAN,
                            [expr.Variable("Undefined", location=Location((10, 20)))],
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("SubProg", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined variable "Undefined"$',
    )


def test_local_variable_shadows_global() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("Global"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[
                    decl.VariableDeclaration("Global", "Boolean", location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean", location=Location((10, 30)))],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'<stdin>:10:20: error: local variable "Global" shadows previous declaration\n'
            r'<stdin>:10:30: note: previous declaration of variable "Global"\n'
            r'<stdin>:10:30: error: unused variable "Global"'
            r"$"
        ),
    )


def test_unused_global_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean", location=Location((10, 20)))],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: unused variable "Global"$',
    )


def test_unused_local_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[
                    decl.VariableDeclaration("Data", "Boolean", location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: unused variable "Data"$',
    )


def test_unused_channel() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("null"))], declarations=[]),
        ],
        declarations=[],
        parameters=[
            decl.ChannelDeclaration("X", readable=True, writable=True, location=Location((10, 20))),
        ],
        types=[],
        regex=r'^<stdin>:10:20: error: unused channel "X"$',
    )


def test_unused_function() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("null"))], declarations=[]),
        ],
        declarations=[],
        parameters=[
            decl.FunctionDeclaration("X", [], "Boolean", location=Location((10, 20))),
        ],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: unused function "X"$',
    )


def test_renaming() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(
                            expr.Length(expr.Variable("Null_Message")),
                            expr.Number(0),
                        ),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.RenamingDeclaration(
                "Null_Message",
                "Null_Msg::Message",
                expr.Selected(expr.Variable("Message"), "Value"),
                location=Location((10, 20)),
            ),
        ],
        parameters=[],
        types=[models.null_message(), models.tlv_message(), models.null_message_in_tlv_message()],
        regex=r"^<stdin>:10:20: error: renaming declarations not yet supported$",
    )


def test_renaming_invalid() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(
                            expr.Length(expr.Variable("Universal_Message")),
                            expr.Number(0),
                        ),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.RenamingDeclaration(
                "Universal_Message",
                "Universal::Message",
                expr.Selected(expr.Variable("Message"), "Value"),
                location=Location((10, 20)),
            ),
        ],
        parameters=[],
        types=[
            models.universal_message(),
            models.tlv_message(),
            models.null_message_in_tlv_message(),
        ],
        regex=(
            r"^"
            r'<stdin>:10:20: error: invalid renaming to "Universal_Message"\n'
            r'<stdin>:10:20: help: refinement for message "TLV::Message"'
            r" would make operation legal"
            r"$"
        ),
    )


def test_renaming_undefined() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Length(expr.Variable("M")), expr.Number(0)),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
            ),
        ],
        declarations=[
            decl.RenamingDeclaration(
                "M",
                "Boolean",
                expr.Selected(expr.Variable("Message", location=Location((10, 20))), "Field"),
            ),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined variable "Message"$',
    )


def test_for_all() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.ForAllIn(
                            "E",
                            expr.Variable("List"),
                            expr.Greater(
                                expr.Selected(expr.Variable("E"), "Length"),
                                expr.Number(0),
                            ),
                            location=Location((10, 20)),
                        ),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("List", "TLV::Messages")],
        parameters=[],
        types=[BOOLEAN, models.tlv_messages()],
        regex=r"^<stdin>:10:20: error: quantified expressions not yet supported$",
    )


def test_append() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[
                    stmt.Append(
                        "List",
                        expr.MessageAggregate(
                            "TLV::Message",
                            {"Tag": expr.Variable("TLV::Msg_Error")},
                        ),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("List", "TLV::Messages")],
        parameters=[],
        types=[models.tlv_tag(), models.tlv_message(), models.tlv_messages()],
    )


def test_append_incompatible() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[
                    stmt.Append("Global", expr.Variable("Global"), location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^<stdin>:10:20: error: expected sequence type\n"
            r'<stdin>:10:20: note: found enumeration type "__BUILTINS__::Boolean"$'
        ),
    )


def test_append_message_unsupported() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[
                    stmt.Append("List", expr.Variable("Element", location=Location((10, 20)))),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("List", "TLV::Messages"),
            decl.VariableDeclaration("Element", "TLV::Message"),
        ],
        parameters=[],
        types=[models.tlv_message(), models.tlv_messages()],
        regex=(
            r"^<stdin>:10:20: error: appending independently created message not supported\n"
            r"<stdin>:10:20: help: message aggregate should be used instead$"
        ),
    )


def test_extend() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[stmt.Extend("List", expr.Variable("Element"))],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("List", "TLV::Messages"),
            decl.VariableDeclaration("Element", "TLV::Messages"),
        ],
        parameters=[],
        types=[BOOLEAN, models.tlv_messages()],
    )


def test_extend_incompatible() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[
                    stmt.Extend("Global", expr.Variable("Global"), location=Location((10, 20))),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^<stdin>:10:20: error: expected sequence type\n"
            r'<stdin>:10:20: note: found enumeration type "__BUILTINS__::Boolean"$'
        ),
    )


def test_message_aggregate_with_undefined_parameter() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Data",
                        expr.MessageAggregate(
                            "TLV::Message",
                            {"Tag": expr.Variable("Undef", location=Location((10, 20)))},
                        ),
                    ),
                ],
            ),
        ],
        declarations=[decl.VariableDeclaration("Data", "TLV::Message")],
        parameters=[],
        types=[BOOLEAN, models.tlv_message()],
        regex=r'^<stdin>:10:20: error: undefined variable "Undef"$',
    )


def test_message_aggregate_with_undefined_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[],
                actions=[
                    stmt.VariableAssignment(
                        "Data",
                        expr.MessageAggregate(
                            "P::Undefined",
                            {"Flag": expr.TRUE},
                            location=Location((10, 30)),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "P::Undefined", location=Location((10, 20))),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r'^<stdin>:10:20: error: undefined type "P::Undefined"\n'
            r'<stdin>:10:30: error: undefined message "P::Undefined"$'
        ),
    )


def test_comprehension() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.VariableAssignment(
                        "Result",
                        expr.Comprehension(
                            "E",
                            expr.Variable("List"),
                            expr.Selected(expr.Variable("E"), "Tag"),
                            expr.Greater(
                                expr.Selected(expr.Variable("E"), "Length"),
                                expr.Number(0),
                            ),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("List", "TLV::Messages"),
            decl.VariableDeclaration("Result", "TLV::Tags"),
        ],
        parameters=[],
        types=[BOOLEAN, models.tlv_messages(), models.tlv_tags()],
    )


def test_assignment_opaque_function_undef_parameter() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.VariableAssignment(
                        "Data",
                        expr.Opaque(
                            expr.Call(
                                "Sub",
                                rty.OPAQUE,
                                [expr.Variable("UndefData", location=Location((10, 20)))],
                            ),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Opaque"),
        ],
        parameters=[
            decl.FunctionDeclaration("Sub", [decl.Argument("Param", "Opaque")], "TLV::Message"),
        ],
        types=[BOOLEAN, OPAQUE, models.tlv_message()],
        regex=r'^<stdin>:10:20: error: undefined variable "UndefData"$',
    )


def test_assignment_opaque_function_result() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.VariableAssignment(
                        "Data",
                        expr.Opaque(
                            expr.Call("Sub", rty.OPAQUE, [expr.Variable("Data")]),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Opaque"),
        ],
        parameters=[
            decl.FunctionDeclaration("Sub", [decl.Argument("Param", "Opaque")], "TLV::Message"),
        ],
        types=[BOOLEAN, OPAQUE, models.tlv_message()],
    )


def test_message_field_assignment_with_invalid_field_name() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.MessageFieldAssignment(
                        "Message",
                        ID("Invalid", location=Location((1, 2))),
                        expr.Number(42),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
        ],
        parameters=[],
        types=[models.tlv_message()],
        regex=r'^<stdin>:1:2: error: invalid message field "Invalid"$',
    )


def test_message_field_assignment_to_message_parameter() -> None:
    parameterized_model = get_test_model("parameterized")
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.MessageFieldAssignment(
                        "Message",
                        ID("Length", location=Location((1, 2))),
                        expr.Number(42),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "Parameterized::Message"),
        ],
        parameters=[],
        types=parameterized_model.types,
        regex=(
            r"^"
            r'<stdin>:1:2: error: message parameter "Length" cannot be set using an'
            r" assignment\n"
            r"<stdin>:1:2: help: use a Reset statement to change the message parameters"
            r"$"
        ),
    )


def test_message_field_assignment_with_incompatible_field_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.MessageFieldAssignment(
                        "Message",
                        "Tag",
                        expr.Number(42, location=Location((1, 2))),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
        ],
        parameters=[],
        types=[models.tlv_message()],
        regex=(
            r"^"
            r'<stdin>:1:2: error: expected enumeration type "TLV::Tag"\n'
            r"<stdin>:1:2: note: found type universal integer \(42\)"
            r"$"
        ),
    )


def test_message_field_assignment_with_incompatible_variable_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.MessageFieldAssignment(
                        "Message",
                        "Tag",
                        expr.Variable("TLV::Msg_Data"),
                        location=Location((1, 2)),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Tag"),
        ],
        parameters=[],
        types=[models.tlv_tag()],
        regex=(
            r"^"
            r"<stdin>:1:2: error: expected message type\n"
            r'<stdin>:1:2: note: found enumeration type "TLV::Tag"'
            r"$"
        ),
    )


def test_conversion() -> None:
    Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.VariableAssignment(
                        "Converted",
                        expr.Conversion(
                            "Null_Msg::Message",
                            expr.Selected(expr.Variable("Message"), "Value"),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.VariableDeclaration("Converted", "Null_Msg::Message"),
        ],
        parameters=[],
        types=[models.null_message(), models.tlv_message(), models.null_message_in_tlv_message()],
    )


def test_conversion_undefined() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.VariableAssignment(
                        "Converted",
                        expr.Conversion(
                            "P::Undef",
                            expr.Selected(expr.Variable("Message"), "Value"),
                            location=Location((10, 30)),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.VariableDeclaration("Converted", "P::Undef", location=Location((10, 20))),
        ],
        parameters=[],
        types=[models.tlv_message()],
        regex=(
            r'^<stdin>:10:20: error: undefined type "P::Undef"\n'
            r'<stdin>:10:30: error: invalid conversion to "P::Undef"\n'
            r'<stdin>:10:30: help: refinement for message "TLV::Message"'
            r" would make operation legal\n"
            r'<stdin>:10:30: error: undefined type "P::Undef"$'
        ),
    )


def test_conversion_invalid_argument() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.VariableAssignment(
                        "Converted",
                        expr.Conversion(
                            "TLV::Message",
                            expr.Variable("Message", location=Location((10, 20))),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "Opaque"),
            decl.VariableDeclaration("Converted", "TLV::Message"),
        ],
        parameters=[],
        types=[OPAQUE, models.tlv_message()],
        regex=(r"^<stdin>:10:20: error: invalid argument for conversion, expected message field$"),
    )


def test_conversion_invalid() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                actions=[
                    stmt.VariableAssignment(
                        "Converted",
                        expr.Conversion(
                            "Null_Msg::Message",
                            expr.Selected(expr.Variable("Message"), "Value"),
                            location=Location((10, 20)),
                        ),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.VariableDeclaration("Converted", "Null_Msg::Message"),
        ],
        parameters=[],
        types=[models.null_message(), models.tlv_message()],
        regex=(
            r"^"
            r'<stdin>:10:20: error: invalid conversion to "Null_Msg::Message"\n'
            r'<stdin>:10:20: help: refinement for message "TLV::Message"'
            r" would make operation legal"
            r"$"
        ),
    )


@pytest.mark.parametrize(
    "parameters",
    [
        [
            decl.FunctionDeclaration(
                "X",
                [decl.Argument("Y", "Boolean")],
                "Undefined",
                location=Location((10, 20)),
            ),
        ],
        [
            decl.FunctionDeclaration(
                "X",
                [decl.Argument("Y", "Undefined")],
                "Boolean",
                location=Location((10, 20)),
            ),
        ],
    ],
)
def test_undefined_type_in_parameters(parameters: abc.Sequence[decl.FormalDeclaration]) -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Call("X", rty.BOOLEAN, [expr.TRUE]), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
            ),
        ],
        declarations=[],
        parameters=parameters,
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: error: undefined type "Undefined"$',
    )


@pytest.mark.parametrize(
    "declarations",
    [
        [decl.VariableDeclaration("X", "Undefined", location=Location((10, 20)))],
        [
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.RenamingDeclaration(
                "X",
                "Undefined",
                expr.Selected(expr.Variable("Message"), "Tag"),
                location=Location((10, 20)),
            ),
        ],
    ],
)
def test_undefined_type_in_declarations(declarations: abc.Sequence[decl.BasicDeclaration]) -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("X"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
            ),
        ],
        declarations=declarations,
        parameters=[],
        types=[models.tlv_message()],
        regex=r'^<stdin>:10:20: error: undefined type "Undefined"$',
    )


@pytest.mark.parametrize(
    "declarations",
    [
        [decl.VariableDeclaration("X", "Undefined", location=Location((10, 20)))],
        [
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.RenamingDeclaration(
                "X",
                "Undefined",
                expr.Selected(expr.Variable("Message"), "Tag"),
                location=Location((10, 20)),
            ),
        ],
    ],
)
def test_undefined_type_in_local_declarations(
    declarations: abc.Sequence[decl.BasicDeclaration],
) -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("X"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=declarations,
            ),
        ],
        declarations=[],
        parameters=[],
        types=[models.tlv_message()],
        regex=r'^<stdin>:10:20: error: undefined type "Undefined"$',
    )


def test_type_error_in_variable_declaration() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("X"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("X", "Boolean", expr.Number(1, location=Location((10, 20)))),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'<stdin>:10:20: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:10:20: note: found type universal integer \(1\)"
            r"$"
        ),
    )


def test_type_error_in_renaming_declaration() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(expr.Variable("X"), expr.TRUE),
                    ),
                    Transition(
                        target=ID("Start"),
                    ),
                ],
                declarations=[],
            ),
        ],
        declarations=[
            decl.RenamingDeclaration(
                "X",
                "Boolean",
                expr.Selected(expr.Number(1, location=Location((10, 20))), "Field"),
            ),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r"<stdin>:10:20: error: expected message type\n"
            r"<stdin>:10:20: note: found type universal integer \(1\)"
            r"$"
        ),
    )


@pytest.mark.parametrize(
    ("declarations", "actions", "errors"),
    [
        (
            [
                decl.VariableDeclaration(
                    "M",
                    models.UNIVERSAL_MESSAGE_ID,
                    location=Location((1, 2)),
                ),
            ],
            [
                stmt.Read("C1", expr.Variable("M"), location=Location((1, 2))),
                stmt.Write("C2", expr.Variable("M2"), location=Location((2, 3))),
            ],
            "<stdin>:1:2: error: IO state must not contain declarations",
        ),
        (
            [],
            [
                stmt.Read(
                    "C1",
                    expr.MessageAggregate(
                        models.UNIVERSAL_MESSAGE_ID,
                        {"Message_Type": expr.Variable("Universal::MT_Null")},
                    ),
                    location=Location((1, 2)),
                ),
                stmt.Write("C2", expr.Variable("M1")),
            ],
            "<stdin>:1:2: error: channel parameter must be a variable",
        ),
        (
            [],
            [
                stmt.Read("C1", expr.Variable("M1"), location=Location((1, 2))),
                stmt.Write("C2", expr.Variable("M2"), location=Location((2, 3))),
                stmt.VariableAssignment("X", expr.FALSE, location=Location((3, 4))),
            ],
            "<stdin>:1:2: error: channel IO must not be combined with other actions"
            " in one state",
        ),
        (
            [],
            [
                stmt.Read(
                    ID("C1", location=Location((1, 1))),
                    expr.Variable("M1"),
                ),
                stmt.Write(
                    ID("C1", location=Location((2, 1))),
                    expr.Variable("M2"),
                ),
                stmt.Read(
                    ID("C2", location=Location((3, 1))),
                    expr.Variable("M3"),
                ),
            ],
            '<stdin>:1:1: error: channel "C1" may be read or written'
            " at most once per state\n"
            "<stdin>:2:1: note: conflicting read/write",
        ),
        (
            [],
            [
                stmt.Read(
                    "C1",
                    expr.Variable("M1", location=Location((1, 1))),
                ),
                stmt.Write(
                    "C2",
                    expr.Variable("M1", location=Location((2, 1))),
                ),
            ],
            '<stdin>:1:1: error: message "M1" may be read or written'
            " at most once per state\n"
            "<stdin>:2:1: note: conflicting read/write",
        ),
    ],
)
def test_conflicting_actions(
    declarations: abc.Sequence[decl.BasicDeclaration],
    actions: abc.Sequence[stmt.Statement],
    errors: str,
) -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                declarations=declarations,
                actions=actions,
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.And(
                            expr.Equal(expr.Variable("X"), expr.TRUE),
                            expr.Equal(expr.Valid("M1"), expr.TRUE),
                            expr.Equal(expr.Valid("M2"), expr.TRUE),
                            expr.Equal(expr.Valid("M3"), expr.TRUE),
                        ),
                    ),
                    Transition(
                        target=ID("null"),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("M1", models.universal_message().identifier),
            decl.VariableDeclaration("M2", models.universal_message().identifier),
            decl.VariableDeclaration("M3", models.universal_message().identifier),
            decl.VariableDeclaration("X", BOOLEAN.identifier),
        ],
        parameters=[
            decl.ChannelDeclaration("C1", readable=True, writable=True),
            decl.ChannelDeclaration("C2", readable=True, writable=True),
        ],
        types=[BOOLEAN, models.universal_message(), *models.universal_message().types.values()],
        regex=rf"^{errors}$",
    )


def test_unsupported_expression() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                actions=[
                    stmt.VariableAssignment(
                        "R",
                        expr.Equal(
                            expr.Selected(expr.Variable("M"), "Data", location=Location((1, 2))),
                            expr.Aggregate(expr.Number(0), expr.Number(1)),
                        ),
                    ),
                ],
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.Equal(
                            expr.Selected(expr.Variable("M"), "Data", location=Location((3, 4))),
                            expr.Aggregate(expr.Number(0), expr.Number(1)),
                        ),
                    ),
                    Transition(
                        target=ID("null"),
                    ),
                ],
                exception_transition=Transition(target=ID("null")),
            ),
        ],
        declarations=[
            decl.VariableDeclaration("M", models.universal_message().identifier),
            decl.VariableDeclaration("R", BOOLEAN.identifier),
        ],
        parameters=[],
        types=[BOOLEAN, models.universal_message()],
        regex=(
            r"^"
            r"<stdin>:1:2: error: comparisons of opaque fields not yet supported\n"
            r"<stdin>:3:4: error: comparisons of opaque fields not yet supported"
            r"$"
        ),
    )


def test_missing_exception_transition() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                declarations=[],
                actions=[
                    stmt.Append(
                        "List",
                        expr.MessageAggregate(
                            "TLV::Message",
                            {"Tag": expr.Variable("TLV::Msg_Error")},
                        ),
                    ),
                ],
                location=Location((10, 20)),
            ),
        ],
        declarations=[decl.VariableDeclaration("List", "TLV::Messages")],
        parameters=[],
        types=[models.tlv_tag(), models.tlv_message(), models.tlv_messages()],
        regex=r'^<stdin>:10:20: error: missing exception transition in state "Start"$',
    )


@pytest.mark.parametrize(
    ("state", "types", "parameters"),
    [
        (
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null"), location=Location((10, 20))),
                declarations=[],
                actions=[],
            ),
            [],
            [],
        ),
        (
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null"), location=Location((10, 20))),
                declarations=[decl.VariableDeclaration("Tag", "TLV::Tag")],
                actions=[
                    stmt.VariableAssignment(
                        "Tag",
                        expr.Call("SubProg", models.tlv_tag().type_),
                    ),
                ],
            ),
            [models.tlv_tag()],
            [decl.FunctionDeclaration("SubProg", [], "TLV::Tag")],
        ),
    ],
)
def test_unnecessary_exception_transition(
    state: State,
    parameters: abc.Sequence[decl.FormalDeclaration],
    types: abc.Sequence[TypeDecl],
) -> None:
    assert_session_model_error(
        states=[state],
        declarations=[],
        parameters=parameters,
        types=types,
        regex=r'^<stdin>:10:20: error: unnecessary exception transition in state "Start"$',
    )


def test_resolving_of_function_calls() -> None:
    session = Session(
        identifier="P::S",
        states=[
            State(
                "Start",
                declarations=[
                    decl.VariableDeclaration("Local", "Boolean", expr.Variable("Func")),
                ],
                actions=[
                    stmt.VariableAssignment("Global", expr.Variable("Func")),
                ],
                transitions=[
                    Transition(
                        target=ID("null"),
                        condition=expr.And(expr.Variable("Global"), expr.Variable("Local")),
                    ),
                    Transition(
                        target=ID("null"),
                    ),
                ],
            ),
        ],
        declarations=[
            decl.VariableDeclaration("Global", "Boolean", expr.Variable("Func")),
        ],
        parameters=[
            decl.FunctionDeclaration("Func", [], "Boolean"),
        ],
        types=[BOOLEAN, OPAQUE, models.tlv_message()],
    )

    global_decl = session.declarations[ID("Global")]
    assert isinstance(global_decl, decl.VariableDeclaration)
    assert global_decl.expression == expr.Call("Func", rty.BOOLEAN)

    local_decl = session.states[0].declarations[ID("Local")]
    assert isinstance(local_decl, decl.VariableDeclaration)
    assert local_decl.expression == expr.Call("Func", rty.BOOLEAN)

    local_stmt = session.states[0].actions[0]
    assert isinstance(local_stmt, stmt.VariableAssignment)
    assert local_stmt.expression == expr.Call("Func", rty.BOOLEAN)


@pytest.mark.parametrize(
    ("actions", "normalized_actions"),
    [
        (
            [
                stmt.MessageFieldAssignment(
                    "M",
                    "A",
                    expr.Number(1),
                ),
            ],
            [
                stmt.MessageFieldAssignment(
                    "M",
                    "A",
                    expr.Number(1),
                ),
            ],
        ),
        (
            [
                stmt.MessageFieldAssignment(
                    "M",
                    "A",
                    expr.Number(1),
                ),
                stmt.MessageFieldAssignment(
                    "M",
                    "B",
                    expr.Number(2),
                ),
                stmt.MessageFieldAssignment(
                    "M",
                    "C",
                    expr.Number(3),
                ),
            ],
            [
                stmt.VariableAssignment(
                    "M",
                    expr.DeltaMessageAggregate(
                        "M",
                        {
                            "A": expr.Number(1),
                            "B": expr.Number(2),
                            "C": expr.Number(3),
                        },
                    ),
                ),
            ],
        ),
        (
            [
                stmt.MessageFieldAssignment(
                    "M1",
                    "A",
                    expr.Number(1),
                ),
                stmt.MessageFieldAssignment(
                    "M2",
                    "B",
                    expr.Number(2),
                ),
                stmt.MessageFieldAssignment(
                    "M2",
                    "C",
                    expr.Number(3),
                ),
            ],
            [
                stmt.MessageFieldAssignment(
                    "M1",
                    "A",
                    expr.Number(1),
                ),
                stmt.VariableAssignment(
                    "M2",
                    expr.DeltaMessageAggregate(
                        "M2",
                        {
                            "B": expr.Number(2),
                            "C": expr.Number(3),
                        },
                    ),
                ),
            ],
        ),
        (
            [
                stmt.MessageFieldAssignment(
                    "M1",
                    "A",
                    expr.Number(1),
                ),
                stmt.MessageFieldAssignment(
                    "M1",
                    "B",
                    expr.Number(2),
                ),
                stmt.MessageFieldAssignment(
                    "M2",
                    "C",
                    expr.Number(3),
                ),
            ],
            [
                stmt.VariableAssignment(
                    "M1",
                    expr.DeltaMessageAggregate(
                        "M1",
                        {
                            "A": expr.Number(1),
                            "B": expr.Number(2),
                        },
                    ),
                ),
                stmt.MessageFieldAssignment(
                    "M2",
                    "C",
                    expr.Number(3),
                ),
            ],
        ),
        (
            [
                stmt.MessageFieldAssignment(
                    "M1",
                    "A",
                    expr.Number(1),
                ),
                stmt.MessageFieldAssignment(
                    "M1",
                    "B",
                    expr.Number(2),
                ),
                stmt.MessageFieldAssignment(
                    "M2",
                    "C",
                    expr.Number(3),
                ),
                stmt.MessageFieldAssignment(
                    "M2",
                    "D",
                    expr.Number(4),
                ),
            ],
            [
                stmt.VariableAssignment(
                    "M1",
                    expr.DeltaMessageAggregate(
                        "M1",
                        {
                            "A": expr.Number(1),
                            "B": expr.Number(2),
                        },
                    ),
                ),
                stmt.VariableAssignment(
                    "M2",
                    expr.DeltaMessageAggregate(
                        "M2",
                        {
                            "C": expr.Number(3),
                            "D": expr.Number(4),
                        },
                    ),
                ),
            ],
        ),
    ],
)
def test_state_normalization(
    actions: list[stmt.Statement],
    normalized_actions: list[stmt.Statement],
) -> None:
    assert str(
        State(
            "S",
            transitions=[Transition(target=ID("null"))],
            exception_transition=Transition(target=ID("null")),
            actions=actions,
        ),
    ) == str(
        State(
            "S",
            transitions=[Transition(target=ID("null"))],
            exception_transition=Transition(target=ID("null")),
            actions=normalized_actions,
        ),
    )
    assert State(
        "S",
        transitions=[Transition(target=ID("null"))],
        exception_transition=Transition(target=ID("null")),
        actions=actions,
    ) == State(
        "S",
        transitions=[Transition(target=ID("null"))],
        exception_transition=Transition(target=ID("null")),
        actions=normalized_actions,
    )
    assert str(
        State(
            "S",
            transitions=[Transition(target=ID("null"))],
            exception_transition=Transition(target=ID("null")),
            actions=[
                stmt.VariableAssignment("X", expr.Number(0)),
                *actions,
                stmt.VariableAssignment("Y", expr.Number(9)),
            ],
        ),
    ) == str(
        State(
            "S",
            transitions=[Transition(target=ID("null"))],
            exception_transition=Transition(target=ID("null")),
            actions=[
                stmt.VariableAssignment("X", expr.Number(0)),
                *normalized_actions,
                stmt.VariableAssignment("Y", expr.Number(9)),
            ],
        ),
    )
    assert State(
        "S",
        transitions=[Transition(target=ID("null"))],
        exception_transition=Transition(target=ID("null")),
        actions=[
            stmt.VariableAssignment("X", expr.Number(0)),
            *actions,
            stmt.VariableAssignment("Y", expr.Number(9)),
        ],
    ) == State(
        "S",
        transitions=[Transition(target=ID("null"))],
        exception_transition=Transition(target=ID("null")),
        actions=[
            stmt.VariableAssignment("X", expr.Number(0)),
            *normalized_actions,
            stmt.VariableAssignment("Y", expr.Number(9)),
        ],
    )


@pytest.mark.parametrize(
    ("state", "optimized_state"),
    [
        (State("S"), State("S")),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message("M", is_definite=False),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message("M", is_definite=False),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                        expression=expr.Variable("X"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                        expression=expr.Variable("X"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Int",
                        "Integer",
                        type_=rty.Integer("Integer", rty.Bounds(0, 255)),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Int",
                        "Integer",
                        type_=rty.Integer("Integer", rty.Bounds(0, 255)),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[stmt.Reset("Msg")],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[stmt.Reset("Msg")],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.Append("List", expr.Variable("E")),
                    stmt.Append("List", expr.Variable("Msg")),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.Append("List", expr.Variable("E")),
                    stmt.Append("List", expr.Variable("Msg")),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Msg",
                        expr.Variable("X"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Msg",
                        expr.Variable("X"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.MessageFieldAssignment(
                        "Msg",
                        "Field",
                        expr.Variable("X"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.MessageFieldAssignment(
                        "Msg",
                        "Field",
                        expr.Variable("X"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                    decl.VariableDeclaration(
                        "Msg2",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Msg",
                        expr.Call(
                            "Func",
                            rty.Message(
                                "M",
                                is_definite=True,
                            ),
                            args=[expr.Opaque("Msg2")],
                        ),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Structure("M"),
                    ),
                    decl.VariableDeclaration(
                        "Msg2",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Msg",
                        expr.Call(
                            "Func",
                            type_=rty.Structure(
                                "M",
                            ),
                            args=[expr.Opaque("Msg2")],
                        ),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message("M", is_definite=True),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Var",
                        expr.Selected(
                            prefix=expr.Variable(
                                "Msg",
                                type_=rty.Message(
                                    "M",
                                    is_definite=True,
                                ),
                            ),
                            selector="Field",
                        ),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Structure(
                            "M",
                        ),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Var",
                        expr.Selected(
                            prefix=expr.Variable(
                                "Msg",
                                type_=rty.Structure(
                                    "M",
                                ),
                            ),
                            selector="Field",
                        ),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
        (
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Valid",
                        expr.Valid("Msg"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
            State(
                "S",
                declarations=[
                    decl.VariableDeclaration(
                        "Msg",
                        "Message",
                        type_=rty.Message(
                            "M",
                            is_definite=True,
                        ),
                    ),
                ],
                actions=[
                    stmt.VariableAssignment(
                        "Valid",
                        expr.Valid("Msg"),
                    ),
                ],
                transitions=[Transition(target=ID("null"))],
            ),
        ),
    ],
)
def test_state_optimization(state: State, optimized_state: State) -> None:
    state.optimize()
    assert state == optimized_state


def test_message_assignment_from_function() -> None:
    Session(
        "P::S",
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("null"))],
                exception_transition=Transition(target=ID("null")),
                declarations=[decl.VariableDeclaration("Msg", "Null_Msg::Message")],
                actions=[stmt.VariableAssignment("Msg", expr.Call("SubProg", rty.BASE_INTEGER))],
            ),
        ],
        declarations=[],
        parameters=[decl.FunctionDeclaration("SubProg", [], "Null_Msg::Message")],
        types=[models.null_message()],
        location=Location((1, 1)),
    )


def test_unchecked_session_checked() -> None:
    unchecked = UncheckedSession(
        ID("P::S"),
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
                description="rfc1149.txt+51:4-52:9",
            ),
        ],
        [
            decl.VariableDeclaration("M", "TLV::Message"),
            decl.VariableDeclaration("Y", BOOLEAN.identifier, expr.FALSE),
        ],
        [
            decl.ChannelDeclaration("X", readable=True, writable=True),
            decl.FunctionDeclaration("F", [], BOOLEAN.identifier),
            decl.FunctionDeclaration(
                "G",
                [decl.Argument("P", BOOLEAN.identifier)],
                BOOLEAN.identifier,
            ),
        ],
        Location((1, 2)),
    )
    expected = Session(
        "P::S",
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
                description="rfc1149.txt+51:4-52:9",
            ),
        ],
        [
            decl.VariableDeclaration("M", "TLV::Message"),
            decl.VariableDeclaration("Y", BOOLEAN.identifier, expr.FALSE),
        ],
        [
            decl.ChannelDeclaration("X", readable=True, writable=True),
            decl.FunctionDeclaration("F", [], BOOLEAN.identifier),
            decl.FunctionDeclaration(
                "G",
                [decl.Argument("P", BOOLEAN.identifier)],
                BOOLEAN.identifier,
            ),
        ],
        [BOOLEAN, models.tlv_message()],
        Location((1, 2)),
    )
    assert (
        unchecked.checked(
            [BOOLEAN, models.tlv_message()],
        )
        == expected
    )


@pytest.mark.parametrize(
    ("unchecked", "expected"),
    [
        (
            UncheckedSession(
                ID("T", Location((2, 3))),
                [State("Start", [Transition(target=ID("null"))])],
                [],
                [],
                Location((1, 2)),
            ),
            r'^<stdin>:2:3: error: invalid format for identifier "T"$',
        ),
    ],
)
def test_unchecked_session_checked_error(unchecked: UncheckedSession, expected: str) -> None:
    with pytest.raises(RecordFluxError, match=expected):
        unchecked.checked([])
