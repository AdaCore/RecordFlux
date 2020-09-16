# pylint: disable=too-many-lines
import pytest

import rflx.declaration as decl
import rflx.expression as expr
import rflx.statement as stmt
from rflx.error import RecordFluxError
from rflx.identifier import ID
from rflx.model import BOOLEAN, Session, State, Transition
from tests.utils import assert_equal, assert_session_model_error, multilinestr


def test_str() -> None:
    assert_equal(
        str(
            Session(
                "P::S",
                "A",
                "B",
                [
                    State(
                        "A",
                        declarations=[decl.VariableDeclaration("Z", "Boolean", expr.Variable("Y"))],
                        actions=[stmt.Assignment("Z", expr.Number(1))],
                        transitions=[
                            Transition(
                                "B", condition=expr.Equal(expr.Variable("Z"), expr.Number(1))
                            ),
                            Transition("A"),
                        ],
                    ),
                    State("B"),
                ],
                [decl.VariableDeclaration("Y", "Boolean", expr.Number(0))],
                [
                    decl.ChannelDeclaration("X", readable=True, writable=True),
                    decl.PrivateDeclaration("T"),
                    decl.SubprogramDeclaration("F", [], "T"),
                ],
                [BOOLEAN],
            )
        ),
        multilinestr(
            """generic
                  X : Channel with Readable, Writable;
                  type T is private;
                  with function F return T;
               session S with
                  Initial => A,
                  Final => B
               is
                  Y : Boolean := 0;
               begin
                  state A is
                     Z : Boolean := Y;
                  begin
                     Z := 1;
                  transition
                     then B
                        if Z = 1
                     then A
                  end A;

                  state B is null state;
               end S"""
        ),
    )


def test_invalid_name() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^model: error: invalid session name "P::S::X"',
    ):
        Session(
            identifier="P::S::X",
            initial=ID("Start"),
            final=ID("End"),
            states=[],
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
        regex=(
            r"^"
            r"model: error: empty states\n"
            r'model: error: initial state "Start" does not exist in "P::S"\n'
            r'model: error: final state "End" does not exist in "P::S"'
            r"$"
        ),
    )


def test_invalid_initial() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'model: error: initial state "NonExistent" does not exist in "P::S"\n'
            r"model: error: unreachable states Start"
            r"$"
        ),
    ):
        Session(
            identifier="P::S",
            initial=ID("NonExistent"),
            final=ID("End"),
            states=[
                State(name=ID("Start"), transitions=[Transition(target=ID("End"))]),
                State(name=ID("End")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_invalid_final() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'model: error: final state "NonExistent" does not exist in "P::S"\n'
            r"model: error: detached states End"
            r"$"
        ),
    ):
        Session(
            identifier="P::S",
            initial=ID("Start"),
            final=ID("NonExistent"),
            states=[
                State(name=ID("Start"), transitions=[Transition(target=ID("End"))]),
                State(name=ID("End")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_invalid_target_state() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(target=ID("NonExistent")),
                    Transition(target=ID("End")),
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'model: error: transition from state "Start" to non-existent'
            ' state "NonExistent" in "P::S"'
            r"$"
        ),
    )


def test_duplicate_state() -> None:
    assert_session_model_error(
        states=[
            State(name=ID("Start"), transitions=[Transition(target=ID("End"))]),
            State(name=ID("Start"), transitions=[Transition(target=ID("End"))]),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r"^model: error: duplicate states: Start$",
    )


def test_multiple_duplicate_states() -> None:
    assert_session_model_error(
        states=[
            State(name=ID("Start"), transitions=[Transition(target=ID("Foo"))]),
            State(name=ID("Start"), transitions=[Transition(target=ID("Foo"))]),
            State(name=ID("Foo"), transitions=[Transition(target=ID("Bar"))]),
            State(name=ID("Bar"), transitions=[Transition(target=ID("End"))]),
            State(name=ID("Foo"), transitions=[Transition(target=ID("Bar"))]),
            State(name=ID("Bar"), transitions=[Transition(target=ID("End"))]),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex="^model: error: duplicate states: Bar, Foo, Start$",
    )


def test_unreachable_state() -> None:
    assert_session_model_error(
        states=[
            State(name=ID("Start"), transitions=[Transition(target=ID("End"))]),
            State(
                name=ID("Unreachable"),
                transitions=[Transition(target=ID("End"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r"^model: error: unreachable states Unreachable$",
    )


def test_multiple_unreachable_states() -> None:
    assert_session_model_error(
        states=[
            State(name=ID("Start"), transitions=[Transition(target=ID("End"))]),
            State(
                name=ID("Unreachable1"),
                transitions=[Transition(target=ID("End"))],
            ),
            State(
                name=ID("Unreachable2"),
                transitions=[Transition(target=ID("End"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r"^model: error: unreachable states Unreachable1, Unreachable2$",
    )


def test_detached_state() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End")), Transition(target=ID("Detached"))],
            ),
            State(name=ID("Detached")),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r"^model: error: detached states Detached$",
    )


def test_multiple_detached_states() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(target=ID("End")),
                    Transition(target=ID("Detached1")),
                    Transition(target=ID("Detached2")),
                ],
            ),
            State(name=ID("Detached1")),
            State(name=ID("Detached2")),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r"^model: error: detached states Detached1, Detached2$",
    )


def test_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("Undefined"), expr.TRUE),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^model: error: undeclared variable "Undefined"$',
    )


def test_undefinded_type() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("Defined"), expr.TRUE),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Defined", "Undefined_Type")],
        parameters=[],
        types=[],
        regex=r'^model: error: undefined type "Undefined_Type"$',
    )


def test_declared_variable() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Defined"), expr.TRUE)
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Defined", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_declared_local_variable() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("Local"), expr.Variable("Global")),
                    )
                ],
                declarations=[decl.VariableDeclaration("Local", "Boolean")],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_undeclared_local_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("State"), condition=expr.Variable("Global"))],
                declarations=[],
            ),
            State(
                name=ID("State"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Variable("Local"), expr.Variable("Global")),
                    )
                ],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared variable "Local"$',
    )


def test_declared_local_variable_valid() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(expr.Valid(expr.Variable("Global")), expr.TRUE),
                    )
                ],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_declared_local_variable_field() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.Equal(
                            expr.Selected(expr.Variable("Global"), "Field"), expr.TRUE
                        ),
                    )
                ],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_assignment_to_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Undefined", expr.FALSE)],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^model: error: undeclared variable "Undefined"$',
    )


def test_assignment_from_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Global", expr.Variable("Undefined"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared variable "Undefined"$',
    )


def test_erasure_of_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Erase("Undefined")],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^model: error: undeclared variable "Undefined"$',
    )


def test_reset_of_undeclared_list() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Reset("Undefined")],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=r'^model: error: undeclared variable "Undefined"$',
    )


def test_call_to_undeclared_function() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment("Global", expr.Call("UndefSub", [expr.Variable("Global")]))
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared variable "UndefSub"$',
    )


def test_call_to_builtin_read() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment("Global", expr.Call("Read", [expr.Variable("Some_Channel")]))
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Global", "Boolean"),
            decl.ChannelDeclaration("Some_Channel", readable=True, writable=False),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_call_to_builtin_write() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Success",
                        expr.Call("Write", [expr.Variable("Some_Channel"), expr.TRUE]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Success", "Boolean"),
            decl.ChannelDeclaration("Some_Channel", readable=False, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_call_to_builtin_call() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result", expr.Call("Call", [expr.Variable("Some_Channel"), expr.TRUE])
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Some_Channel", readable=True, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_call_to_builtin_data_available() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call("Data_Available", [expr.Variable("Some_Channel")]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Some_Channel", readable=True, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_call_to_builtin_read_without_arguments() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Result", expr.Call("Read", []))],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Result", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: no channel argument in call to "Read"$',
    )


def test_call_to_builtin_read_undeclared_channel() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Result"), expr.TRUE)
                    )
                ],
                declarations=[],
                actions=[
                    stmt.Assignment("Result", expr.Call("Read", [expr.Variable("Undeclared")]))
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Result", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'model: error: undeclared channel "Undeclared" in call to "Read"\n'
            r'model: error: undeclared variable "Undeclared"'
            r"$"
        ),
    )


def test_call_to_builtin_read_invalid_channel_type() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Result", expr.Call("Read", [expr.Number(0)]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Result", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: invalid channel ID type in call to "Read"$',
    )


def test_call_to_builtin_read_invalid_channel_id_type() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Result", expr.Call("Read", [expr.Variable("Result")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Result", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: invalid channel type in call to "Read"$',
    )


def test_call_to_builtin_write_invalid_channel_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Result"), expr.TRUE)
                    )
                ],
                declarations=[],
                actions=[
                    stmt.Assignment("Result", expr.Call("Write", [expr.Variable("Out_Channel")]))
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Out_Channel", readable=True, writable=False),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "Out_Channel" not writable in call to "Write"$',
    )


def test_call_to_builtin_data_available_invalid_channel_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call("Data_Available", [expr.Variable("Out_Channel")]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Out_Channel", readable=False, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "Out_Channel" not readable in call to "Data_Available"$',
    )


def test_call_to_builtin_read_invalid_channel_mode() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Result", expr.Call("Read", [expr.Variable("Channel")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Channel", readable=False, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "Channel" not readable in call to "Read"$',
    )


def test_call_to_builtin_call_channel_not_readable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Result", expr.Call("Call", [expr.Variable("Channel")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Channel", readable=False, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "Channel" not readable in call to "Call"$',
    )


def test_call_to_builtin_call_channel_not_writable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Result", expr.Call("Call", [expr.Variable("Channel")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Channel", readable=True, writable=False),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "Channel" not writable in call to "Call"$',
    )


def test_subprogram_call() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Assignment("Result", expr.Call("Call", [expr.Variable("Channel")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.ChannelDeclaration("Channel", readable=True, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_undeclared_variable_in_subprogram_call() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Result"), expr.TRUE)
                    )
                ],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Call("SubProg", [expr.Variable("Undefined")]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.SubprogramDeclaration("SubProg", [], "Boolean"),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared variable "Undefined"$',
    )


def test_function_declaration_is_no_builtin_read() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.SubprogramDeclaration("Read", [], "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'model: error: subprogram declaration shadows builtin subprogram "Read"\n'
            r'model: error: unused subprogram "Read"'
            r"$"
        ),
    )


def test_function_declaration_is_no_builtin_write() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.ChannelDeclaration("Write", readable=True, writable=False)],
        parameters=[],
        types=[],
        regex=(
            r"^"
            r'model: error: channel declaration shadows builtin subprogram "Write"\n'
            r'model: error: unused channel "Write"'
            r"$"
        ),
    )


def test_function_declaration_is_no_builtin_call() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Call", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'model: error: variable declaration shadows builtin subprogram "Call"\n'
            r'model: error: unused variable "Call"'
            r"$"
        ),
    )


def test_function_declaration_is_no_builtin_data_available() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Foo", "Boolean"),
            decl.RenamingDeclaration("Data_Available", "Boolean", expr.Variable("Foo")),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'model: error: renames declaration shadows builtin subprogram "Data_Available"\n'
            r'model: error: unused renames "Data_Available"'
            r"$"
        ),
    )


def test_local_variable_shadows_global() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Global"), expr.TRUE)
                    )
                ],
                declarations=[decl.VariableDeclaration("Global", "Boolean")],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            r"^"
            r'model: error: local variable "Global" shadows global declaration in state Start\n'
            r'model: error: unused variable "Global"'
            r"$"
        ),
    )


def test_unused_global_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: unused variable "Global"$',
    )


def test_unused_local_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[decl.VariableDeclaration("Data", "Boolean")],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: unused variable "Data" in state Start$',
    )


def test_renames_references_undefined_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"), condition=expr.Equal(expr.Variable("Ren"), expr.TRUE)
                    )
                ],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.RenamingDeclaration("Ren", "Boolean", expr.Variable("Foo"))],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared variable "Foo"$',
    )


def test_binding_as_subprogram_parameter() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Result",
                        expr.Binding(
                            expr.Call("SubProg", [expr.Length(expr.Variable("Bound"))]),
                            {"Bound": expr.Variable("Variable")},
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Result", "Boolean"),
            decl.VariableDeclaration("Variable", "Boolean"),
            decl.SubprogramDeclaration("SubProg", [], "Boolean"),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_for_all() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=expr.ForAllIn(
                            "E",
                            expr.Variable("List"),
                            expr.Equal(expr.Selected(expr.Variable("E"), "Tag"), expr.Number(42)),
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("List", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_append_list_attribute() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Append("List", expr.Variable("Element"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("List", "Boolean"),
            decl.VariableDeclaration("Element", "Boolean"),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_extend_list_attribute() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[stmt.Extend("List", expr.Variable("Element"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("List", "Boolean"),
            decl.VariableDeclaration("Element", "Boolean"),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_aggregate_with_undefined_parameter() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[
                    stmt.Assignment(
                        "Data",
                        expr.MessageAggregate(
                            "Boolean",
                            {"Foo": expr.Variable("Data"), "Bar": expr.Variable("Undef")},
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Data", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared variable "Undef"$',
    )


def test_comprehension() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Input",
                        expr.Comprehension(
                            "K",
                            expr.Variable("Input"),
                            expr.Selected(expr.Variable("K"), "Data"),
                            expr.Equal(expr.Selected(expr.Variable("K"), "Valid"), expr.TRUE),
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.VariableDeclaration("Input", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_assignment_opaque_subprogram_undef_parameter() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                actions=[
                    stmt.Assignment(
                        "Data",
                        expr.Opaque(
                            expr.Call("Sub", [expr.Variable("UndefData")]),
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Boolean"),
            decl.SubprogramDeclaration(
                "Sub", [decl.Argument("Param", "Param_Type")], "Result_Type"
            ),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared variable "UndefData"$',
    )


def test_assignment_opaque_subprogram_result() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
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
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Boolean"),
            decl.SubprogramDeclaration(
                "Sub", [decl.Argument("Param", "Param_Type")], "Result_Type"
            ),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_assignment_opaque_subprogram_binding() -> None:
    Session(
        identifier="P::S",
        initial=ID("Start"),
        final=ID("End"),
        states=[
            State(
                name=ID("Start"),
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
            State(name=ID("End")),
        ],
        declarations=[
            decl.VariableDeclaration("Data", "Boolean"),
            decl.SubprogramDeclaration(
                "Sub", [decl.Argument("Param", "Param_Type")], "Result_Type"
            ),
        ],
        parameters=[],
        types=[BOOLEAN],
    )


def test_private_declaration_is_no_builtin_write() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[decl.PrivateDeclaration("Write")],
        parameters=[],
        types=[],
        regex=(
            r'^model: error: private declaration shadows builtin subprogram "Write"\n'
            r'model: error: unused private "Write"$'
        ),
    )
