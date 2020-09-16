# pylint: disable=too-many-lines
from rflx.declaration import (
    Argument,
    ChannelDeclaration,
    PrivateDeclaration,
    RenamingDeclaration,
    SubprogramDeclaration,
    VariableDeclaration,
)
from rflx.expression import (
    FALSE,
    TRUE,
    Binding,
    Call,
    Comprehension,
    Equal,
    ForAllIn,
    Length,
    MessageAggregate,
    Number,
    Opaque,
    Selected,
    Valid,
    Variable,
)
from rflx.identifier import ID
from rflx.model import BOOLEAN, Session, State, Transition
from rflx.statement import Append, Assignment, Erase, Extend, Reset
from tests.utils import assert_session_model_error


def test_undeclared_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=Equal(Variable("Undefined"), TRUE),
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
                        condition=Equal(Variable("Defined"), TRUE),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Defined", "Undefined_Type")],
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
                    Transition(target=ID("End"), condition=Equal(Variable("Defined"), TRUE))
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Defined", "Boolean")],
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
                        condition=Equal(Variable("Local"), Variable("Global")),
                    )
                ],
                declarations=[VariableDeclaration("Local", "Boolean")],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
    )


def test_undeclared_local_variable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("State"), condition=Variable("Global"))],
                declarations=[],
            ),
            State(
                name=ID("State"),
                transitions=[
                    Transition(
                        target=ID("End"),
                        condition=Equal(Variable("Local"), Variable("Global")),
                    )
                ],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
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
                    Transition(target=ID("End"), condition=Equal(Valid(Variable("Global")), TRUE))
                ],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
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
                        condition=Equal(Selected(Variable("Global"), "Field"), TRUE),
                    )
                ],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
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
                actions=[Assignment("Undefined", FALSE)],
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
                actions=[Assignment("Global", Variable("Undefined"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
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
                actions=[Erase("Undefined")],
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
                actions=[Reset("Undefined")],
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
                actions=[Assignment("Global", Call("UndefSub", [Variable("Global")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
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
                actions=[Assignment("Global", Call("Read", [Variable("Some_Channel")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Global", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=True, writable=False),
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
                    Assignment(
                        "Success",
                        Call("Write", [Variable("Some_Channel"), TRUE]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Success", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=False, writable=True),
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
                actions=[Assignment("Result", Call("Call", [Variable("Some_Channel"), TRUE]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=True, writable=True),
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
                    Assignment(
                        "Result",
                        Call("Data_Available", [Variable("Some_Channel")]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=True, writable=True),
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
                actions=[Assignment("Result", Call("Read", []))],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Result", "Boolean")],
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
                    Transition(target=ID("End"), condition=Equal(Variable("Result"), TRUE))
                ],
                declarations=[],
                actions=[Assignment("Result", Call("Read", [Variable("Undeclared")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Result", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: undeclared channel "Undeclared" in call to "Read"\n'
        r'model: error: undeclared variable "Undeclared"$',
    )


def test_call_to_builtin_read_invalid_channel_type() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[Assignment("Result", Call("Read", [Number(0)]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Result", "Boolean")],
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
                actions=[Assignment("Result", Call("Read", [Variable("Result")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Result", "Boolean")],
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
                    Transition(target=ID("End"), condition=Equal(Variable("Result"), TRUE))
                ],
                declarations=[],
                actions=[Assignment("Result", Call("Write", [Variable("Out_Channel")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("Out_Channel", readable=True, writable=False),
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
                    Assignment(
                        "Result",
                        Call("Data_Available", [Variable("Out_Channel")]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("Out_Channel", readable=False, writable=True),
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
                actions=[Assignment("Result", Call("Read", [Variable("ChannelDeclaration")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("ChannelDeclaration", readable=False, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "ChannelDeclaration" not readable in call to "Read"$',
    )


def test_call_to_builtin_call_channel_not_readable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[Assignment("Result", Call("Call", [Variable("ChannelDeclaration")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("ChannelDeclaration", readable=False, writable=True),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "ChannelDeclaration" not readable in call to "Call"$',
    )


def test_call_to_builtin_call_channel_not_writable() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
                actions=[Assignment("Result", Call("Call", [Variable("ChannelDeclaration")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("ChannelDeclaration", readable=True, writable=False),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=r'^model: error: channel "ChannelDeclaration" not writable in call to "Call"$',
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
                actions=[Assignment("Result", Call("Call", [Variable("ChannelDeclaration")]))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("ChannelDeclaration", readable=True, writable=True),
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
                    Transition(target=ID("End"), condition=Equal(Variable("Result"), TRUE))
                ],
                declarations=[],
                actions=[
                    Assignment(
                        "Result",
                        Call("SubProg", [Variable("Undefined")]),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            SubprogramDeclaration("SubProg", [], "Boolean"),
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
        declarations=[SubprogramDeclaration("Read", [], "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            "^"
            'model: error: subprogram declaration shadows builtin subprogram "Read"\n'
            'model: error: unused subprogram "Read"'
            "$"
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
        declarations=[ChannelDeclaration("Write", readable=True, writable=False)],
        parameters=[],
        types=[],
        regex=(
            "^"
            'model: error: channel declaration shadows builtin subprogram "Write"\n'
            'model: error: unused channel "Write"'
            "$"
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
        declarations=[VariableDeclaration("Call", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            "^"
            'model: error: variable declaration shadows builtin subprogram "Call"\n'
            'model: error: unused variable "Call"'
            "$"
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
            VariableDeclaration("Foo", "Boolean"),
            RenamingDeclaration("Data_Available", "Boolean", Variable("Foo")),
        ],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            "^"
            'model: error: renames declaration shadows builtin subprogram "Data_Available"\n'
            'model: error: unused renames "Data_Available"'
            "$"
        ),
    )


def test_local_variable_shadows_global() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[
                    Transition(target=ID("End"), condition=Equal(Variable("Global"), TRUE))
                ],
                declarations=[VariableDeclaration("Global", "Boolean")],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
        parameters=[],
        types=[BOOLEAN],
        regex=(
            "^"
            'model: error: local variable "Global" shadows global declaration in state Start\n'
            'model: error: unused variable "Global"'
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
        declarations=[VariableDeclaration("Global", "Boolean")],
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
                declarations=[VariableDeclaration("Data", "Boolean")],
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
                transitions=[Transition(target=ID("End"), condition=Equal(Variable("Ren"), TRUE))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[RenamingDeclaration("Ren", "Boolean", Variable("Foo"))],
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
                    Assignment(
                        "Result",
                        Binding(
                            Call("SubProg", [Length(Variable("Bound"))]),
                            {"Bound": Variable("Variable")},
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            VariableDeclaration("Variable", "Boolean"),
            SubprogramDeclaration("SubProg", [], "Boolean"),
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
                        condition=ForAllIn(
                            "E",
                            Variable("List"),
                            Equal(Selected(Variable("E"), "Tag"), Number(42)),
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("List", "Boolean")],
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
                actions=[Append("List", Variable("Element"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("List", "Boolean"),
            VariableDeclaration("Element", "Boolean"),
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
                actions=[Extend("List", Variable("Element"))],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("List", "Boolean"),
            VariableDeclaration("Element", "Boolean"),
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
                    Assignment(
                        "Data",
                        MessageAggregate(
                            "Boolean",
                            {"Foo": Variable("Data"), "Bar": Variable("Undef")},
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Data", "Boolean")],
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
                    Assignment(
                        "Input",
                        Comprehension(
                            "K",
                            Variable("Input"),
                            Selected(Variable("K"), "Data"),
                            Equal(Selected(Variable("K"), "Valid"), TRUE),
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[VariableDeclaration("Input", "Boolean")],
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
                    Assignment(
                        "Data",
                        Opaque(
                            Call("Sub", [Variable("UndefData")]),
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Data", "Boolean"),
            SubprogramDeclaration("Sub", [Argument("Param", "Param_Type")], "Result_Type"),
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
                    Assignment(
                        "Data",
                        Opaque(
                            Call("Sub", [Variable("Data")]),
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Data", "Boolean"),
            SubprogramDeclaration("Sub", [Argument("Param", "Param_Type")], "Result_Type"),
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
                    Assignment(
                        "Data",
                        Binding(
                            Opaque(Call("Sub", [Variable("Bound")])),
                            {"Bound": Variable("Data")},
                        ),
                    )
                ],
            ),
            State(name=ID("End")),
        ],
        declarations=[
            VariableDeclaration("Data", "Boolean"),
            SubprogramDeclaration("Sub", [Argument("Param", "Param_Type")], "Result_Type"),
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
        declarations=[PrivateDeclaration("Write")],
        parameters=[],
        types=[],
        regex=(
            '^model: error: private declaration shadows builtin subprogram "Write"\n'
            'model: error: unused private "Write"$'
        ),
    )


def test_duplicate_states() -> None:
    assert_session_model_error(
        states=[
            State(
                name=ID("Start"),
                transitions=[Transition(target=ID("FOO"))],
                declarations=[],
            ),
            State(
                name=ID("FOO"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(
                name=ID("FOO"),
                transitions=[Transition(target=ID("End"))],
                declarations=[],
            ),
            State(name=ID("End")),
        ],
        declarations=[],
        parameters=[],
        types=[],
        regex=("^model: error: duplicate states: FOO$"),
    )
