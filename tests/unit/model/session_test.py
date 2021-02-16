# pylint: disable=too-many-lines
from typing import Sequence

import pytest

import rflx.declaration as decl
import rflx.expression as expr
import rflx.statement as stmt
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    OPAQUE,
    AbstractSession,
    Array,
    Private,
    Session,
    State,
    Transition,
    UnprovenSession,
)
from tests.data.models import NULL_MESSAGE, NULL_MESSAGE_IN_TLV_MESSAGE, TLV_MESSAGE, TLV_TAG
from tests.utils import assert_equal, assert_session_model_error, multilinestr

TLV_MESSAGES = Array("TLV::Messages", TLV_MESSAGE)
TLV_TAGS = Array("TLV::Tags", TLV_TAG)


def test_str() -> None:
    assert_equal(
        str(
            UnprovenSession(
                "P::S",
                "A",
                "B",
                [
                    State(
                        "A",
                        declarations=[
                            decl.VariableDeclaration("Z", "Boolean", expr.Variable("Y")),
                            decl.VariableDeclaration("M", "TLV::Message"),
                        ],
                        actions=[stmt.Read("X", expr.Variable("M"))],
                        transitions=[
                            Transition(
                                "B",
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
                    State("B"),
                ],
                [decl.VariableDeclaration("Y", "Boolean", expr.FALSE)],
                [
                    decl.ChannelDeclaration("X", readable=True, writable=True),
                    decl.TypeDeclaration(Private("T")),
                    decl.FunctionDeclaration("F", [], "T"),
                    decl.FunctionDeclaration("G", [decl.Argument("P", "T")], "Boolean"),
                ],
                [BOOLEAN, TLV_MESSAGE],
            ).proven()
        ),
        multilinestr(
            """generic
                  X : Channel with Readable, Writable;
                  type T is private;
                  with function F return T;
                  with function G (P : T) return Boolean;
               session S with
                  Initial => A,
                  Final => B
               is
                  Y : Boolean := False;
               begin
                  state A
                     with Desc => "rfc1149.txt+51:4-52:9"
                  is
                     Z : Boolean := Y;
                     M : TLV::Message;
                  begin
                     X'Read (M);
                  transition
                     then B
                        with Desc => "rfc1149.txt+45:4-47:8"
                        if Z = True
                           and G (F) = True
                     then A
                  end A;

                  state B is null state;
               end S"""
        ),
    )


def test_invalid_name() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:20: model: error: invalid session name "P::S::X"$',
    ):
        UnprovenSession(
            identifier=ID("P::S::X", location=Location((10, 20))),
            initial=ID("Start"),
            final=ID("End"),
            states=[
                State("Start", transitions=[Transition(target=ID("End"))]),
                State("End"),
            ],
            declarations=[],
            parameters=[],
            types=[],
        ).proven()


def test_empty_states() -> None:
    assert_session_model_error(
        states=[],
        declarations=[],
        parameters=[],
        types=[],
        location=Location((1, 1)),
        regex=(
            r"^"
            r"<stdin>:1:1: model: error: empty states\n"
            r'<stdin>:1:2: model: error: initial state "Start" does not exist in "P::S"\n'
            r'<stdin>:1:3: model: error: final state "End" does not exist in "P::S"'
            r"$"
        ),
    )


def test_invalid_initial() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:2: model: error: initial state "NonExistent" does not exist in "P::S"\n'
            r'<stdin>:10:20: model: error: unreachable state "Start"'
            r"$"
        ),
    ):
        UnprovenSession(
            identifier="P::S",
            initial=ID("NonExistent", location=Location((1, 2))),
            final=ID("End"),
            states=[
                State(
                    "Start",
                    transitions=[Transition(target=ID("End"))],
                    location=Location((10, 20)),
                ),
                State("End"),
            ],
            declarations=[],
            parameters=[],
            types=[],
            location=Location((1, 1)),
        ).proven()


def test_invalid_final() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:3: model: error: final state "NonExistent" does not exist in "P::S"\n'
            r'<stdin>:10:20: model: error: detached state "End"'
            r"$"
        ),
    ):
        UnprovenSession(
            identifier="P::S",
            initial=ID("Start"),
            final=ID("NonExistent", location=Location((1, 3))),
            states=[
                State("Start", transitions=[Transition(target=ID("End"))]),
                State("End", location=Location((10, 20))),
            ],
            declarations=[],
            parameters=[],
            types=[],
        ).proven()


def test_invalid_target_state() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(target=ID("NonExistent", location=Location((10, 20)))),
                    Transition(target=ID("End")),
                ],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:20: model: error: transition from state "Start" to non-existent'
            r' state "NonExistent" in "P::S"'
            r"$"
        ),
    )


def test_duplicate_state() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                location=Location((10, 20)),
            ),
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                location=Location((10, 30)),
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:30: model: error: duplicate state "Start"\n'
            r'<stdin>:10:20: model: info: previous definition of state "Start"'
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
                transitions=[Transition(target=ID("End"))],
                location=Location((10, 50)),
            ),
            State(
                "Foo",
                transitions=[Transition(target=ID("Bar"))],
                location=Location((10, 60)),
            ),
            State(
                "Bar",
                transitions=[Transition(target=ID("End"))],
                location=Location((10, 70)),
            ),
            State(
                "Foo",
                transitions=[Transition(target=ID("Bar"))],
                location=Location((10, 80)),
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:30: model: error: duplicate state "Start"\n'
            r'<stdin>:10:20: model: info: previous definition of state "Start"\n'
            r'<stdin>:10:60: model: error: duplicate state "Foo"\n'
            r'<stdin>:10:40: model: info: previous definition of state "Foo"\n'
            r'<stdin>:10:80: model: error: duplicate state "Foo"\n'
            r'<stdin>:10:40: model: info: previous definition of state "Foo"\n'
            r'<stdin>:10:70: model: error: duplicate state "Bar"\n'
            r'<stdin>:10:50: model: info: previous definition of state "Bar"'
            r"$"
        ),
    )


def test_unreachable_state() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("End"))]),
            State(
                "Unreachable",
                transitions=[Transition(target=ID("End"))],
                location=Location((10, 20)),
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: model: error: unreachable state "Unreachable"$',
    )


def test_multiple_unreachable_states() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("End"))]),
            State(
                "Unreachable1",
                transitions=[Transition(target=ID("End"))],
                location=Location((10, 20)),
            ),
            State(
                "Unreachable2",
                transitions=[Transition(target=ID("End"))],
                location=Location((10, 30)),
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:20: model: error: unreachable state "Unreachable1"\n'
            r'<stdin>:10:30: model: error: unreachable state "Unreachable2"'
            r"$"
        ),
    )


def test_detached_state() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End")), Transition(target=ID("Detached"))],
            ),
            State("Detached", location=Location((10, 20))),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: model: error: detached state "Detached"$',
    )


def test_multiple_detached_states() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(target=ID("End")),
                    Transition(target=ID("Detached1")),
                    Transition(target=ID("Detached2")),
                ],
            ),
            State("Detached1", location=Location((10, 20))),
            State("Detached2", location=Location((10, 30))),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'<stdin>:10:20: model: error: detached state "Detached1"\n'
            r'<stdin>:10:30: model: error: detached state "Detached2"'
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
                        target=ID("End"),
                        condition=expr.Equal(
                            expr.Variable("Undefined", location=Location((10, 20))), expr.TRUE
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Undefined"$',
    )


def test_undefinded_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("Defined"), expr.TRUE),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Defined", "Undefined_Type", location=Location((10, 20)))
        ],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: model: error: undefined type "Undefined_Type"$',
    )


def test_declared_variable() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(
                            expr.Variable("Defined"), expr.Variable("TLV::Msg_Data")
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Defined", "TLV::Tag")],
        parameters=[],
        types=[TLV_TAG],
    ).proven()


def test_declared_local_variable() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("Local"), expr.Variable("Global")),
                    )
                ],
                declarations=[decl.VariableDeclaration("Local", "Boolean")],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    ).proven()


def test_undeclared_local_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("State"), condition=expr.Variable("Global"))],
                declarations=[],
            ),
            State(
                "State",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(
                            expr.Variable("Local", location=Location((10, 20))),
                            expr.Variable("Global"),
                        ),
                    )
                ],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Local"$',
    )


def test_declared_local_variable_valid() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Valid(expr.Variable("Global")), expr.TRUE),
                    )
                ],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "TLV::Message")],
        parameters=[],
        types=[TLV_MESSAGE],
    ).proven()


def test_declared_local_variable_message_field() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(
                            expr.Selected(expr.Variable("Global"), "Length"), expr.Number(1)
                        ),
                    )
                ],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "TLV::Message")],
        parameters=[],
        types=[TLV_MESSAGE],
    ).proven()


def test_assignment_to_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Undefined", expr.FALSE, location=Location((10, 20)))],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Undefined"$',
    )


def test_assignment_from_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Global", expr.Variable("Undefined", location=Location((10, 20)))
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Undefined"$',
    )


def test_reset_of_undeclared_list() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Reset("Undefined", location=Location((10, 20)))],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Undefined"$',
    )


def test_reset_incompatible() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Reset("Global", location=Location((10, 20)))],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^<stdin>:10:20: model: error: expected array type or message type\n"
            r'<stdin>:10:20: model: info: found enumeration type "__BUILTINS__::Boolean"$'
        ),
    )


def test_call_to_undeclared_function() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Global",
                        expr.Call(
                            "UndefSub", [expr.Variable("Global")], location=Location((10, 20))
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: undefined function "UndefSub"$',
    )


def test_call_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Result"), expr.TRUE)
                    )
                ],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call(
                            "SubProg", [expr.Variable("Undefined", location=Location((10, 20)))]
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("SubProg", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Undefined"$',
    )


def test_call_invalid_argument_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call(
                            "Function",
                            [expr.Variable("Channel", location=Location((10, 20)))],
                        ),
                    )
                ],
            ),
            State("End"),
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
            r'<stdin>:10:20: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:10:20: model: info: found readable channel"
            "$"
        ),
    )


def test_call_missing_arguments() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call(
                            "Function",
                            location=Location((10, 20)),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("Function", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r"^<stdin>:10:20: model: error: missing function arguments$",
    )


def test_call_too_many_arguments() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call(
                            "Function",
                            [expr.TRUE, expr.Number(1)],
                            location=Location((10, 20)),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("Function", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r"^<stdin>:10:20: model: error: too many function arguments$",
    )


def test_channel_read() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Read("Some_Channel", expr.Variable("Global"))],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Global", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Some_Channel", readable=True, writable=False),
        ],
        types=[TLV_MESSAGE],
    ).proven()


def test_channel_write() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Write("Some_Channel", expr.Variable("M"))],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("M", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Some_Channel", readable=False, writable=True),
        ],
        types=[TLV_MESSAGE],
    ).proven()


def test_channel_read_undeclared() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Read("Undeclared", expr.Variable("Result"), location=Location((10, 20)))
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Result", "TLV::Message")],
        parameters=[],
        types=[TLV_MESSAGE],
        regex=r'^<stdin>:10:20: model: error: undefined channel "Undeclared"$',
    )


def test_channel_read_invalid_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Read(
                        "Result",
                        expr.Number(0, location=Location((10, 30))),
                        location=Location((10, 20)),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Result", "TLV::Message")],
        parameters=[],
        types=[TLV_MESSAGE],
        regex=(
            r"^"
            r"<stdin>:10:20: model: error: expected readable channel\n"
            r'<stdin>:10:20: model: info: found message type "TLV::Message"\n'
            r"<stdin>:10:30: model: error: expected message type\n"
            r"<stdin>:10:30: model: info: found type universal integer \(0\)"
            r"$"
        ),
    )


def test_channel_read_invalid_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Read("Channel", expr.Variable("Result"), location=Location((10, 20)))
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Channel", readable=False, writable=True),
        ],
        types=[TLV_MESSAGE],
        regex=(
            r"^"
            r"<stdin>:10:20: model: error: expected readable channel\n"
            r"<stdin>:10:20: model: info: found writable channel"
            r"$"
        ),
    )


def test_channel_write_invalid_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Write("Out_Channel", expr.Variable("Result"), location=Location((10, 20)))
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "TLV::Message"),
        ],
        parameters=[
            decl.ChannelDeclaration("Out_Channel", readable=True, writable=False),
        ],
        types=[TLV_MESSAGE],
        regex=(
            r"^"
            r"<stdin>:10:20: model: error: expected writable channel\n"
            r"<stdin>:10:20: model: info: found readable channel"
            r"$"
        ),
    )


def test_channel_function_data_available() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result", expr.Call("Data_Available", [expr.Variable("Channel")])
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.ChannelDeclaration("Channel", readable=True, writable=True),
        ],
        types=[BOOLEAN],
    ).proven()


def test_channel_function_data_available_invalid_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call(
                            "Data_Available",
                            [expr.Variable("Out_Channel", location=Location((10, 20)))],
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.ChannelDeclaration("Out_Channel", readable=False, writable=True),
        ],
        types=[BOOLEAN],
        regex=(
            r"^"
            r"<stdin>:10:20: model: error: expected readable channel\n"
            r"<stdin>:10:20: model: info: found writable channel"
            r"$"
        ),
    )


def test_undeclared_variable_in_function_call() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Result"), expr.TRUE)
                    )
                ],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call(
                            "SubProg", [expr.Variable("Undefined", location=Location((10, 20)))]
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("SubProg", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Undefined"$',
    )


def test_function_shadows_builtin_data_available() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[
            decl.FunctionDeclaration(
                "Data_Available",
                [],
                "Boolean",
                location=Location((10, 20)),
            ),
        ],
        types=[BOOLEAN],
        regex=(
            r"^"
            r"<stdin>:10:20: model: error: function declaration shadows built-in function "
            r'"Data_Available"\n'
            r'<stdin>:10:20: model: error: unused function "Data_Available"'
            r"$"
        ),
    )


def test_renaming_shadows_builtin_data_available() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.RenamingDeclaration(
                "Data_Available",
                "Null::Message",
                expr.Selected(expr.Variable("Message"), "Value"),
                location=Location((10, 20)),
            ),
        ],
        parameters=[],
        types=[BOOLEAN, TLV_MESSAGE, NULL_MESSAGE, NULL_MESSAGE_IN_TLV_MESSAGE],
        regex=(
            r"^"
            r"<stdin>:10:20: model: error: renaming declaration shadows built-in function "
            r'"Data_Available"\n'
            r'<stdin>:10:20: model: error: unused renaming "Data_Available"'
            r"$"
        ),
    )


def test_local_variable_shadows_global() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Global"), expr.TRUE)
                    )
                ],
                declarations=[
                    decl.VariableDeclaration("Global", "Boolean", location=Location((10, 20)))
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean", location=Location((10, 30)))],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'<stdin>:10:20: model: error: local variable "Global" shadows previous declaration\n'
            r'<stdin>:10:30: model: info: previous declaration of variable "Global"\n'
            r'<stdin>:10:30: model: error: unused variable "Global"'
            r"$"
        ),
    )


def test_unused_global_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean", location=Location((10, 20)))],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: unused variable "Global"$',
    )


def test_unused_local_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[
                    decl.VariableDeclaration("Data", "Boolean", location=Location((10, 20)))
                ],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: unused variable "Data"$',
    )


def test_unused_channel() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("End"))], declarations=[]),
            State("End"),
        ],
        declarations=[],
        parameters=[
            decl.ChannelDeclaration("X", readable=True, writable=True, location=Location((10, 20))),
        ],
        types=[],
        regex=r'^<stdin>:10:20: model: error: unused channel "X"$',
    )


def test_unused_private_type() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("End"))], declarations=[]),
            State("End"),
        ],
        declarations=[],
        parameters=[
            decl.TypeDeclaration(Private("X", location=Location((10, 20)))),
        ],
        types=[],
        regex=r'^<stdin>:10:20: model: error: unused type "X"$',
    )


def test_unused_function() -> None:
    assert_session_model_error(
        states=[
            State("Start", transitions=[Transition(target=ID("End"))], declarations=[]),
            State("End"),
        ],
        declarations=[],
        parameters=[
            decl.FunctionDeclaration("X", [], "Boolean", location=Location((10, 20))),
        ],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: unused function "X"$',
    )


def test_renaming() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(
                            expr.Length(expr.Variable("Null_Message")), expr.Number(0)
                        ),
                    )
                ],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.RenamingDeclaration(
                "Null_Message",
                "Null::Message",
                expr.Selected(expr.Variable("Message"), "Value"),
            ),
        ],
        parameters=[],
        types=[NULL_MESSAGE, TLV_MESSAGE, NULL_MESSAGE_IN_TLV_MESSAGE],
    ).proven()


def test_renaming_invalid() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(
                            expr.Length(expr.Variable("Null_Message")), expr.Number(0)
                        ),
                    )
                ],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.RenamingDeclaration(
                "Null_Message",
                "Null::Message",
                expr.Selected(expr.Variable("Message"), "Value"),
                location=Location((10, 20)),
            ),
        ],
        parameters=[],
        types=[NULL_MESSAGE, TLV_MESSAGE],
        regex=(
            r"^"
            r'<stdin>:10:20: model: error: invalid renaming to "Null_Message"\n'
            r'<stdin>:10:20: model: info: refinement for message "TLV::Message"'
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
                        target=ID("End"),
                        condition=expr.Equal(expr.Length(expr.Variable("M")), expr.Number(0)),
                    )
                ],
                declarations=[],
            ),
            State("End"),
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
        regex=r'^<stdin>:10:20: model: error: undefined variable "Message"$',
    )


def test_binding_as_function_parameter() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Binding(
                            expr.Call("SubProg", [expr.Variable("Bound")]),
                            {"Bound": expr.Variable("Variable")},
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.VariableDeclaration("Variable", "Boolean"),
        ],
        parameters=[
            decl.FunctionDeclaration("SubProg", [decl.Argument("P", "Boolean")], "Boolean"),
        ],
        types=[BOOLEAN],
    ).proven()


def test_for_all() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.ForAllIn(
                            "E",
                            expr.Variable("List"),
                            expr.Greater(
                                expr.Selected(expr.Variable("E"), "Length"), expr.Number(0)
                            ),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("List", "TLV::Messages")],
        parameters=[],
        types=[BOOLEAN, TLV_MESSAGES],
    ).proven()


def test_append() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Append(
                        "List",
                        expr.MessageAggregate(
                            "TLV::Message",
                            {"Tag": expr.Variable("TLV::Msg_Error")},
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("List", "TLV::Messages")],
        parameters=[],
        types=[TLV_TAG, TLV_MESSAGE, TLV_MESSAGES],
    ).proven()


def test_append_incompatible() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Append("Global", expr.Variable("Global"), location=Location((10, 20)))
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^<stdin>:10:20: model: error: expected array type\n"
            r'<stdin>:10:20: model: info: found enumeration type "__BUILTINS__::Boolean"$'
        ),
    )


def test_append_message_unsupported() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Append("List", expr.Variable("Element", location=Location((10, 20))))
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("List", "TLV::Messages"),
            decl.VariableDeclaration("Element", "TLV::Message"),
        ],
        parameters=[],
        types=[TLV_MESSAGE, TLV_MESSAGES],
        regex=(
            r"^<stdin>:10:20: model: error: appending independently created message not supported\n"
            r"<stdin>:10:20: model: info: message aggregate should be used instead$"
        ),
    )


def test_extend() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Extend("List", expr.Variable("Element"))],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("List", "TLV::Messages"),
            decl.VariableDeclaration("Element", "TLV::Messages"),
        ],
        parameters=[],
        types=[BOOLEAN, TLV_MESSAGES],
    ).proven()


def test_extend_incompatible() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Extend("Global", expr.Variable("Global"), location=Location((10, 20)))
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^<stdin>:10:20: model: error: expected array type\n"
            r'<stdin>:10:20: model: info: found enumeration type "__BUILTINS__::Boolean"$'
        ),
    )


def test_message_aggregate_with_undefined_parameter() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Data",
                        expr.MessageAggregate(
                            "TLV::Message",
                            {"Tag": expr.Variable("Undef", location=Location((10, 20)))},
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[decl.VariableDeclaration("Data", "TLV::Message")],
        parameters=[],
        types=[BOOLEAN, TLV_MESSAGE],
        regex=r'^<stdin>:10:20: model: error: undefined variable "Undef"$',
    )


def test_message_aggregate_with_undefined_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Data",
                        expr.MessageAggregate(
                            "P::Undefined",
                            {"Flag": expr.TRUE},
                            location=Location((10, 30)),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "P::Undefined", location=Location((10, 20)))
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r'^<stdin>:10:20: model: error: undefined type "P::Undefined"\n'
            r'<stdin>:10:30: model: error: undefined message "P::Undefined"$'
        ),
    )


def test_comprehension() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Comprehension(
                            "E",
                            expr.Variable("List"),
                            expr.Selected(expr.Variable("E"), "Tag"),
                            expr.Greater(
                                expr.Selected(expr.Variable("E"), "Length"), expr.Number(0)
                            ),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("List", "TLV::Messages"),
            decl.VariableDeclaration("Result", "TLV::Tags"),
        ],
        parameters=[],
        types=[BOOLEAN, TLV_MESSAGES, TLV_TAGS],
    ).proven()


def test_assignment_opaque_function_undef_parameter() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Data",
                        expr.Opaque(
                            expr.Call(
                                "Sub", [expr.Variable("UndefData", location=Location((10, 20)))]
                            ),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Opaque"),
        ],
        parameters=[
            decl.FunctionDeclaration("Sub", [decl.Argument("Param", "Opaque")], "TLV::Message"),
        ],
        types=[BOOLEAN, OPAQUE, TLV_MESSAGE],
        regex=r'^<stdin>:10:20: model: error: undefined variable "UndefData"$',
    )


def test_assignment_opaque_function_result() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Data",
                        expr.Opaque(
                            expr.Call("Sub", [expr.Variable("Data")]),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Opaque"),
        ],
        parameters=[
            decl.FunctionDeclaration("Sub", [decl.Argument("Param", "Opaque")], "TLV::Message"),
        ],
        types=[BOOLEAN, OPAQUE, TLV_MESSAGE],
    ).proven()


def test_assignment_opaque_function_binding() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Data",
                        expr.Binding(
                            expr.Opaque(expr.Call("Sub", [expr.Variable("Bound")])),
                            {"Bound": expr.Variable("Data")},
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Opaque"),
        ],
        parameters=[
            decl.FunctionDeclaration("Sub", [decl.Argument("Param", "Opaque")], "TLV::Message"),
        ],
        types=[BOOLEAN, OPAQUE, TLV_MESSAGE],
    ).proven()


def test_conversion() -> None:
    UnprovenSession(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Converted",
                        expr.Conversion(
                            "Null::Message",
                            expr.Selected(expr.Variable("Message"), "Value"),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.VariableDeclaration("Converted", "Null::Message"),
        ],
        parameters=[],
        types=[NULL_MESSAGE, TLV_MESSAGE, NULL_MESSAGE_IN_TLV_MESSAGE],
    ).proven()


def test_conversion_undefined() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Converted",
                        expr.Conversion(
                            "P::Undef",
                            expr.Selected(expr.Variable("Message"), "Value"),
                            location=Location((10, 30)),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.VariableDeclaration("Converted", "P::Undef", location=Location((10, 20))),
        ],
        parameters=[],
        types=[TLV_MESSAGE],
        regex=(
            r'^<stdin>:10:20: model: error: undefined type "P::Undef"\n'
            r'<stdin>:10:30: model: error: invalid conversion to "P::Undef"\n'
            r'<stdin>:10:30: model: info: refinement for message "TLV::Message"'
            r" would make operation legal\n"
            r'<stdin>:10:30: model: error: undefined type "P::Undef"$'
        ),
    )


def test_conversion_invalid_argument() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Converted",
                        expr.Conversion(
                            "TLV::Message",
                            expr.Variable("Message", location=Location((10, 20))),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "Opaque"),
            decl.VariableDeclaration("Converted", "TLV::Message"),
        ],
        parameters=[],
        types=[OPAQUE, TLV_MESSAGE],
        regex=(
            r"^<stdin>:10:20: model: error: invalid argument for conversion,"
            r" expected message field$"
        ),
    )


def test_conversion_invalid() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Converted",
                        expr.Conversion(
                            "Null::Message",
                            expr.Selected(expr.Variable("Message"), "Value"),
                            location=Location((10, 20)),
                        ),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("Message", "TLV::Message"),
            decl.VariableDeclaration("Converted", "Null::Message"),
        ],
        parameters=[],
        types=[NULL_MESSAGE, TLV_MESSAGE],
        regex=(
            r"^"
            r'<stdin>:10:20: model: error: invalid conversion to "Null::Message"\n'
            r'<stdin>:10:20: model: info: refinement for message "TLV::Message"'
            r" would make operation legal"
            r"$"
        ),
    )


def test_private_type() -> None:
    UnprovenSession(
        identifier=ID("P::S"),
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                "Start",
                declarations=[
                    decl.VariableDeclaration("X", "P::T"),
                ],
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("X"), expr.Variable("X")),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[decl.TypeDeclaration(Private("P::T"))],
        types=[],
    ).proven()


def test_private_type_shadows_builtin_data_available() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[decl.TypeDeclaration(Private("Data_Available", location=Location((10, 20))))],
        types=[],
        regex=(
            r"^<stdin>:10:20: model: error: type declaration shadows built-in function"
            r' "Data_Available"\n'
            r'<stdin>:10:20: model: error: unused type "Data_Available"$'
        ),
    )


def test_private_type_shadows_type() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("X"), expr.Variable("Y")),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("X", "Boolean"),
            decl.VariableDeclaration("Y", "Boolean"),
        ],
        parameters=[
            decl.TypeDeclaration(Private("Boolean", location=Location((10, 20)))),
        ],
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: type "Boolean" shadows type$',
    )


@pytest.mark.parametrize(
    "parameters",
    [
        [
            decl.FunctionDeclaration(
                "X", [decl.Argument("Y", "Boolean")], "Undefined", location=Location((10, 20))
            )
        ],
        [
            decl.FunctionDeclaration(
                "X", [decl.Argument("Y", "Undefined")], "Boolean", location=Location((10, 20))
            )
        ],
    ],
)
def test_undefined_type_in_parameters(parameters: Sequence[decl.FormalDeclaration]) -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Call("X", [expr.TRUE]), expr.TRUE),
                    )
                ],
            ),
            State("End"),
        ],
        declarations=[],
        parameters=parameters,
        types=[BOOLEAN],
        regex=r'^<stdin>:10:20: model: error: undefined type "Undefined"$',
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
def test_undefined_type_in_declarations(declarations: Sequence[decl.BasicDeclaration]) -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("X"), expr.TRUE)
                    )
                ],
            ),
            State("End"),
        ],
        declarations=declarations,
        parameters=[],
        types=[TLV_MESSAGE],
        regex=r'^<stdin>:10:20: model: error: undefined type "Undefined"$',
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
def test_undefined_type_in_local_declarations(declarations: Sequence[decl.Declaration]) -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("X"), expr.TRUE)
                    )
                ],
                declarations=declarations,
            ),
            State("End"),
        ],
        declarations=[],
        parameters=[],
        types=[TLV_MESSAGE],
        regex=r'^<stdin>:10:20: model: error: undefined type "Undefined"$',
    )


def test_type_error_in_variable_declaration() -> None:
    assert_session_model_error(
        states=[
            State(
                "Start",
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("X"), expr.TRUE)
                    )
                ],
                declarations=[],
            ),
            State("End"),
        ],
        declarations=[
            decl.VariableDeclaration("X", "Boolean", expr.Number(1, location=Location((10, 20))))
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'<stdin>:10:20: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:10:20: model: info: found type universal integer \(1\)"
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
                        target=ID("End"), condition=expr.Equal(expr.Variable("X"), expr.TRUE)
                    )
                ],
                declarations=[],
            ),
            State("End"),
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
            r"<stdin>:10:20: model: error: expected message type\n"
            r"<stdin>:10:20: model: info: found type universal integer \(1\)"
            r"$"
        ),
    )


def test_invalid_abstract_session_instantiation() -> None:
    with pytest.raises(RuntimeError, match="^AbstractSession must not be instantiated"):
        AbstractSession("P::S", "A", "B", [], [], [], [])


def test_invalid_session_instantiation() -> None:
    with pytest.raises(RuntimeError, match="^Session must not be instantiated directly"):
        Session("P::S", "A", "B", [], [], [], [])
