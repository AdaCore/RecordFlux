# pylint: disable=too-many-lines
import pytest

from rflx.declaration import (
    Argument,
    ChannelDeclaration,
    PrivateDeclaration,
    RenamingDeclaration,
    SubprogramDeclaration,
    VariableDeclaration,
)
from rflx.error import RecordFluxError
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
    Not,
    Number,
    Opaque,
    Selected,
    Valid,
    Variable,
)
from rflx.identifier import ID
from rflx.session import Session, State, Transition
from rflx.statement import Assignment, Erase, Reset


def test_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^model: error: undeclared variable "Undefined"$',
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Undefined"), TRUE),)
                    ],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
        )


def test_declared_variable() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[
                    Transition(target=ID("END"), condition=Equal(Variable("Defined"), TRUE))
                ],
            ),
            State(name=ID("END")),
        ],
        declarations=[VariableDeclaration("Defined", "Some_Type")],
    )


def test_declared_local_variable() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[
                    Transition(
                        target=ID("END"), condition=Equal(Variable("Local"), Variable("Global")),
                    )
                ],
                declarations=[VariableDeclaration("Local", "Some_Type")],
            ),
            State(name=ID("END")),
        ],
        declarations=[VariableDeclaration("Global", "Some_Type")],
    )


def test_undeclared_local_variable() -> None:
    with pytest.raises(
        RecordFluxError, match=('^model: error: undeclared variable "Local"$'),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("STATE"), condition=Variable("Global"))],
                    declarations=[],
                ),
                State(
                    name=ID("STATE"),
                    transitions=[
                        Transition(
                            target=ID("END"),
                            condition=Equal(Variable("Local"), Variable("Global")),
                        )
                    ],
                    declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Global", "Some_Type")],
        )


def test_declared_local_variable_valid() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[
                    Transition(target=ID("END"), condition=Equal(Valid(Variable("Global")), TRUE))
                ],
                declarations=[],
            ),
            State(name=ID("END")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
    )


def test_declared_local_variable_field() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[
                    Transition(
                        target=ID("END"),
                        condition=Equal(Selected(Variable("Global"), "fld"), TRUE),
                    )
                ],
                declarations=[],
            ),
            State(name=ID("END")),
        ],
        declarations=[VariableDeclaration("Global", "Boolean")],
    )


def test_assignment_to_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: assignment to undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Undefined", FALSE)],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
        )


def test_assignment_from_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Global", Variable("Undefined"))],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Global", "Boolean")],
        )


def test_erasure_of_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: erasure of undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Erase("Undefined")],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
        )


def test_reset_of_undeclared_list() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: reset of undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Reset("Undefined")],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
        )


def test_call_to_undeclared_function() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: undeclared subprogram "UndefSub" called'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Global", Call("UndefSub", [Variable("Global")]))],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Global", "Boolean")],
        )


def test_call_to_builtin_read() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                declarations=[],
                actions=[Assignment("Global", Call("Read", [Variable("Some_Channel")]))],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Global", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=True, writable=False),
        ],
    )


def test_call_to_builtin_write() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                declarations=[],
                actions=[Assignment("Success", Call("Write", [Variable("Some_Channel"), TRUE]),)],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Success", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=False, writable=True),
        ],
    )


def test_call_to_builtin_call() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                declarations=[],
                actions=[Assignment("Result", Call("Call", [Variable("Some_Channel"), TRUE]))],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=True, writable=True),
        ],
    )


def test_call_to_builtin_data_available() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                declarations=[],
                actions=[Assignment("Result", Call("Data_Available", [Variable("Some_Channel")]),)],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("Some_Channel", readable=True, writable=True),
        ],
    )


def test_call_to_builtin_read_without_arguments() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: no channel argument in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Result", Call("Read", []))],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Result", "Boolean")],
        )


def test_call_to_builtin_read_undeclared_channel() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: undeclared channel "Undeclared" in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations=[],
                    actions=[Assignment("Result", Call("Read", [Variable("Undeclared")]))],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Result", "Boolean")],
        )


def test_call_to_builtin_read_invalid_channel_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: invalid channel type in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Result", Call("Read", [Variable("Result")]))],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Result", "Boolean")],
        )


def test_call_to_builtin_write_invalid_channel_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: channel "Out_Channel" not writable in call to "Write"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations=[],
                    actions=[Assignment("Result", Call("Write", [Variable("Out_Channel")]))],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                VariableDeclaration("Result", "Boolean"),
                ChannelDeclaration("Out_Channel", readable=True, writable=False),
            ],
        )


def test_call_to_builtin_data_available_invalid_channel_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: channel "Out_Channel" not readable in call to "Data_Available"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[
                        Assignment("Result", Call("Data_Available", [Variable("Out_Channel")]),)
                    ],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                VariableDeclaration("Result", "Boolean"),
                ChannelDeclaration("Out_Channel", readable=False, writable=True),
            ],
        )


def test_call_to_builtin_read_invalid_channel_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: channel "ChannelDeclaration" not readable in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Result", Call("Read", [Variable("ChannelDeclaration")]))],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                VariableDeclaration("Result", "Boolean"),
                ChannelDeclaration("ChannelDeclaration", readable=False, writable=True),
            ],
        )


def test_call_to_builtin_call_channel_not_readable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: channel "ChannelDeclaration" not readable in call to "Call"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Result", Call("Call", [Variable("ChannelDeclaration")]))],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                VariableDeclaration("Result", "Boolean"),
                ChannelDeclaration("ChannelDeclaration", readable=False, writable=True),
            ],
        )


def test_call_to_builtin_call_channel_not_writable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: channel "ChannelDeclaration" not writable in call to "Call"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[Assignment("Result", Call("Call", [Variable("ChannelDeclaration")]))],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                VariableDeclaration("Result", "Boolean"),
                ChannelDeclaration("ChannelDeclaration", readable=True, writable=False),
            ],
        )


def test_subprogram_call() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                declarations=[],
                actions=[Assignment("Result", Call("Call", [Variable("ChannelDeclaration")]))],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            ChannelDeclaration("ChannelDeclaration", readable=True, writable=True),
        ],
    )


def test_undeclared_variable_in_subprogram_call() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations=[],
                    actions=[Assignment("Result", Call("SubProg", [Variable("Undefined")]),)],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                VariableDeclaration("Result", "Boolean"),
                SubprogramDeclaration("SubProg", [], "Boolean"),
            ],
        )


def test_function_declaration_is_no_builtin_read() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'model: error: subprogram declaration shadows builtin subprogram "Read"\n'
            'model: error: unused subprogram "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[SubprogramDeclaration("Read", [], "Boolean")],
        )


def test_function_declaration_is_no_builtin_write() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'model: error: channel declaration shadows builtin subprogram "Write"\n'
            'model: error: unused channel "Write"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[ChannelDeclaration("Write", readable=True, writable=False)],
        )


def test_function_declaration_is_no_builtin_call() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'model: error: variable declaration shadows builtin subprogram "Call"\n'
            'model: error: unused variable "Call"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Call", "Boolean")],
        )


def test_function_declaration_is_no_builtin_data_available() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'model: error: renames declaration shadows builtin subprogram "Data_Available"\n'
            'model: error: unused renames "Data_Available"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                RenamingDeclaration("Data_Available", "Boolean", Variable("Foo")),
                VariableDeclaration("Foo", "Boolean"),
            ],
        )


def test_local_variable_shadows_global() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'model: error: local variable "Global" shadows global declaration in state START\n'
            'model: error: unused variable "Global"'
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Global"), TRUE))
                    ],
                    declarations=[VariableDeclaration("Global", "Boolean")],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Global", "Boolean")],
        )


def test_unused_global_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^model: error: unused variable "Global"$',
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Global", "Boolean")],
        )


def test_unused_local_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^model: error: unused local variable "Data" in state START$',
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[VariableDeclaration("Data", "Boolean")],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
        )


def test_renames_references_undefined_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^model: error: undeclared variable "Foo"$',
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Ren"), TRUE))
                    ],
                    declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[RenamingDeclaration("Ren", "Boolean", Variable("Foo"))],
        )


def test_binding_as_subprogram_parameter() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
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
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Result", "Boolean"),
            VariableDeclaration("Variable", "Boolean"),
            SubprogramDeclaration("SubProg", [], "Boolean"),
        ],
    )


def test_for_all() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[
                    Transition(
                        target=ID("END"),
                        condition=ForAllIn(
                            "E",
                            Variable("List"),
                            Equal(Selected(Variable("E"), "Tag"), Number(42)),
                        ),
                    )
                ],
            ),
            State(name=ID("END")),
        ],
        declarations=[VariableDeclaration("List", "Foo")],
    )


def test_append_list_attribute() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                declarations=[],
                actions=[
                    Assignment("List", Call("Append", [Variable("List"), Variable("Element")]),)
                ],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("List", "List_Type"),
            VariableDeclaration("Element", "Element_Type"),
        ],
    )


def test_extend_list_attribute() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                declarations=[],
                actions=[
                    Assignment("List", Call("Extend", [Variable("List"), Variable("Element")]),)
                ],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("List", "List_Type"),
            VariableDeclaration("Element", "Element_Type"),
        ],
    )


def test_aggregate_with_undefined_parameter() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "Undef"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations=[],
                    actions=[
                        Assignment(
                            "Data",
                            MessageAggregate(
                                "Data_Type", {"Foo": Variable("Data"), "Bar": Variable("Undef")},
                            ),
                        )
                    ],
                ),
                State(name=ID("END")),
            ],
            declarations=[VariableDeclaration("Data", "Data_Type")],
        )


def test_comprehension() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
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
            State(name=ID("END")),
        ],
        declarations=[VariableDeclaration("Input", "Foo")],
    )


def test_assignment_opaque_subprogram_undef_parameter() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "UndefData"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    actions=[Assignment("Data", Opaque(Call("Sub", [Variable("UndefData")]),),)],
                ),
                State(name=ID("END")),
            ],
            declarations=[
                VariableDeclaration("Data", "Foo"),
                SubprogramDeclaration("Sub", [Argument("Param", "Param_Type")], "Result_Type"),
            ],
        )


def test_assignment_opaque_subprogram_result() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                actions=[Assignment("Data", Opaque(Call("Sub", [Variable("Data")]),),)],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Data", "Foo"),
            SubprogramDeclaration("Sub", [Argument("Param", "Param_Type")], "Result_Type"),
        ],
    )


def test_assignment_opaque_subprogram_binding() -> None:
    Session(
        name="session",
        initial=ID("START"),
        final=ID("END"),
        states=[
            State(
                name=ID("START"),
                transitions=[Transition(target=ID("END"))],
                actions=[
                    Assignment(
                        "Data",
                        Binding(
                            Opaque(Call("Sub", [Variable("Bound")])), {"Bound": Variable("Data")},
                        ),
                    )
                ],
            ),
            State(name=ID("END")),
        ],
        declarations=[
            VariableDeclaration("Data", "Foo"),
            SubprogramDeclaration("Sub", [Argument("Param", "Param_Type")], "Result_Type"),
        ],
    )


def test_private_declaration_is_no_builtin_write() -> None:
    with pytest.raises(
        RecordFluxError,
        match=("^" 'model: error: private declaration shadows builtin subprogram "Write"' "$"),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations=[],
                ),
                State(name=ID("END")),
            ],
            declarations=[PrivateDeclaration("Write")],
        )


def test_duplicate_states() -> None:
    with pytest.raises(
        RecordFluxError, match=("^model: error: duplicate states: FOO$"),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("FOO"))], declarations=[]
                ),
                State(name=ID("FOO"), transitions=[Transition(target=ID("END"))], declarations=[]),
                State(name=ID("FOO"), transitions=[Transition(target=ID("END"))], declarations=[]),
                State(name=ID("END")),
            ],
            declarations=[],
        )


def test_invalid_channel_id_type() -> None:
    with pytest.raises(
        RecordFluxError, match=('^model: error: invalid channel ID type in call to "Read"$')
    ):
        Call("Read", [Number(5)]).validate({})


def test_validate_not() -> None:
    Not(TRUE).validate({})


def test_validate_builtin_validate() -> None:
    Variable("Boolean").validate({})
