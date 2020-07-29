# pylint: disable=too-many-lines
import pytest

from rflx.error import RecordFluxError
from rflx.expression import (
    FALSE,
    TRUE,
    And,
    Argument,
    Binding,
    Channel,
    Comprehension,
    Contains,
    Conversion,
    Equal,
    ForAllIn,
    ForSomeIn,
    Head,
    Length,
    Less,
    MessageAggregate,
    NotContains,
    Number,
    Opaque,
    Present,
    Renames,
    Selected,
    String,
    Subprogram,
    SubprogramCall,
    Valid,
    Variable,
    VariableDeclaration,
)
from rflx.identifier import ID
from rflx.session import Session, State, Transition
from rflx.statement import Assignment, Erase, Reset


def test_binding_aggregate() -> None:
    binding = Binding(
        MessageAggregate("M1", {"Data": Variable("B1")}),
        {"B1": MessageAggregate("M2", {"Data": Variable("B2")})},
    )
    expected = MessageAggregate("M1", {"Data": MessageAggregate("M2", {"Data": Variable("B2")})})
    result = binding.simplified()
    assert result == expected


def test_binding_forall_predicate() -> None:
    binding = Binding(
        ForAllIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Bar": Variable("Baz")},
    )
    expected = ForAllIn("X", Variable("Y"), Equal(Variable("X"), Variable("Baz")))
    result = binding.simplified()
    assert result == expected


def test_binding_length() -> None:
    binding = Binding(Length(Variable("A")), {"A": Variable("Baz")})
    expected = Length(Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_forall_iterable() -> None:
    binding = Binding(
        ForAllIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))), {"Y": Variable("Baz")},
    )
    expected = ForAllIn("X", Variable("Baz"), Equal(Variable("X"), Variable("Bar")))
    result = binding.simplified()
    assert result == expected


def test_binding_forsome_predicate() -> None:
    binding = Binding(
        ForSomeIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Bar": Variable("Baz")},
    )
    expected = ForSomeIn("X", Variable("Y"), Equal(Variable("X"), Variable("Baz")))
    result = binding.simplified()
    assert result == expected


def test_binding_forsome_iterable() -> None:
    binding = Binding(
        ForSomeIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Y": Variable("Baz")},
    )
    expected = ForSomeIn("X", Variable("Baz"), Equal(Variable("X"), Variable("Bar")))
    result = binding.simplified()
    assert result == expected


def test_binding_contains_left() -> None:
    binding = Binding(Contains(Variable("X"), Variable("Y")), {"X": Variable("Baz")},)
    expected = Contains(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_contains_right() -> None:
    binding = Binding(Contains(Variable("X"), Variable("Y")), {"Y": Variable("Baz")},)
    expected = Contains(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_not_contains_left() -> None:
    binding = Binding(NotContains(Variable("X"), Variable("Y")), {"X": Variable("Baz")},)
    expected = NotContains(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_not_contains_right() -> None:
    binding = Binding(NotContains(Variable("X"), Variable("Y")), {"Y": Variable("Baz")},)
    expected = NotContains(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_subprogram() -> None:
    binding = Binding(
        SubprogramCall("Sub", [Variable("A"), Variable("B"), Variable("C")]),
        {"B": Variable("Baz")},
    )
    expected = SubprogramCall("Sub", [Variable("A"), Variable("Baz"), Variable("C")])
    result = binding.simplified()
    assert result == expected


def test_binding_field() -> None:
    binding = Binding(Selected(Variable("A"), "fld"), {"A": Variable("Baz")})
    expected = Selected(Variable("Baz"), "fld")
    result = binding.simplified()
    assert result == expected


def test_binding_list_comprehension() -> None:
    binding = Binding(
        Comprehension(
            "E", Variable("List"), Variable("E.Bar"), Equal(Variable("E.Tag"), Variable("Foo")),
        ),
        {"List": Variable("Foo")},
    )
    expected = Comprehension(
        "E", Variable("Foo"), Variable("E.Bar"), Equal(Variable("E.Tag"), Variable("Foo")),
    )
    result = binding.simplified()
    assert result == expected


def test_simplify_string() -> None:
    value = String("Test")
    assert value == value.simplified()


def test_binding_multiple_bindings() -> None:
    binding = Binding(
        Selected(Variable("A"), "fld"), {"A": Binding(Variable("B"), {"B": Variable("Baz")})}
    )
    expected = Selected(Variable("Baz"), "fld")
    result = binding.simplified()
    assert result == expected


def test_binding_multiple_variables() -> None:
    binding = Binding(SubprogramCall("Sub", [Variable("A"), Variable("A")]), {"A": Variable("Baz")})
    expected = SubprogramCall("Sub", [Variable("Baz"), Variable("Baz")])
    result = binding.simplified()
    assert result == expected


def test_binding_conversion() -> None:
    binding = Binding(Conversion("Type", Variable("A")), {"A": Variable("Baz")})
    expected = Conversion("Type", Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_conversion_name_unchanged() -> None:
    binding = Binding(Conversion("Type", Variable("A")), {"Type": Variable("Baz")})
    expected = Conversion("Type", Variable("A"))
    result = binding.simplified()
    assert result == expected


def test_binding_opaque() -> None:
    binding = Binding(
        Opaque(SubprogramCall("Sub", [Variable("Bound")])), {"Bound": Variable("Foo")}
    )
    expected = Opaque(SubprogramCall("Sub", [Variable("Foo")]))
    result = binding.simplified()
    assert result == expected


def test_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^model: error: undeclared variable "Undefined"$',
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[
                        Transition(target="END", condition=Equal(Variable("Undefined"), TRUE),)
                    ],
                ),
                State(name="END"),
            ],
            declarations={},
        )


def test_declared_variable() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END", condition=Equal(Variable("Defined"), TRUE))],
            ),
            State(name="END"),
        ],
        declarations={"Defined": VariableDeclaration("Some_Type")},
    )


def test_declared_local_variable() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[
                    Transition(
                        target="END", condition=Equal(Variable("Local"), Variable("Global")),
                    )
                ],
                declarations={ID("Local"): VariableDeclaration("Some_Type")},
            ),
            State(name="END"),
        ],
        declarations={"Global": VariableDeclaration("Some_Type")},
    )


def test_undeclared_local_variable() -> None:
    with pytest.raises(
        RecordFluxError, match=('^model: error: undeclared variable "Local"$'),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="STATE", condition=Variable("Global"))],
                    declarations={},
                ),
                State(
                    name="STATE",
                    transitions=[
                        Transition(
                            target="END", condition=Equal(Variable("Local"), Variable("Global")),
                        )
                    ],
                    declarations={},
                ),
                State(name="END"),
            ],
            declarations={"Global": VariableDeclaration("Some_Type")},
        )


def test_declared_local_variable_valid() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[
                    Transition(target="END", condition=Equal(Valid(Variable("Global")), TRUE),)
                ],
                declarations={},
            ),
            State(name="END"),
        ],
        declarations={"Global": VariableDeclaration("Boolean")},
    )


def test_declared_local_variable_field() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[
                    Transition(
                        target="END", condition=Equal(Selected(Variable("Global"), "fld"), TRUE),
                    )
                ],
                declarations={},
            ),
            State(name="END"),
        ],
        declarations={"Global": VariableDeclaration("Boolean")},
    )


def test_assignment_to_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'model: error: assignment to undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Assignment("Undefined", FALSE)],
                ),
                State(name="END"),
            ],
            declarations={},
        )


def test_assignment_from_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Assignment("Global", Variable("Undefined"))],
                ),
                State(name="END"),
            ],
            declarations={"Global": VariableDeclaration("Boolean")},
        )


def test_erasure_of_undeclared_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'model: error: erasure of undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Erase("Undefined")],
                ),
                State(name="END"),
            ],
            declarations={},
        )


def test_reset_of_undeclared_list() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'model: error: reset of undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Reset("Undefined")],
                ),
                State(name="END"),
            ],
            declarations={},
        )


def test_call_to_undeclared_function() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: undeclared subprogram "UndefSub" called'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[
                        Assignment("Global", SubprogramCall("UndefSub", [Variable("Global")]))
                    ],
                ),
                State(name="END"),
            ],
            declarations={"Global": VariableDeclaration("Boolean")},
        )


def test_call_to_builtin_read() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[Assignment("Global", SubprogramCall("Read", [Variable("Some_Channel")]))],
            ),
            State(name="END"),
        ],
        declarations={
            "Global": VariableDeclaration("Boolean"),
            "Some_Channel": Channel(read=True, write=False),
        },
    )


def test_call_to_builtin_write() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[
                    Assignment(
                        "Success", SubprogramCall("Write", [Variable("Some_Channel"), TRUE]),
                    )
                ],
            ),
            State(name="END"),
        ],
        declarations={
            "Success": VariableDeclaration("Boolean"),
            "Some_Channel": Channel(read=False, write=True),
        },
    )


def test_call_to_builtin_call() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[
                    Assignment("Result", SubprogramCall("Call", [Variable("Some_Channel"), TRUE]))
                ],
            ),
            State(name="END"),
        ],
        declarations={
            "Result": VariableDeclaration("Boolean"),
            "Some_Channel": Channel(read=True, write=True),
        },
    )


def test_call_to_builtin_data_available() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[
                    Assignment(
                        "Result", SubprogramCall("Data_Available", [Variable("Some_Channel")]),
                    )
                ],
            ),
            State(name="END"),
        ],
        declarations={
            "Result": VariableDeclaration("Boolean"),
            "Some_Channel": Channel(read=True, write=True),
        },
    )


def test_call_to_builtin_read_without_arguments() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: no channel argument in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Assignment("Result", SubprogramCall("Read", []))],
                ),
                State(name="END"),
            ],
            declarations={"Result": VariableDeclaration("Boolean")},
        )


def test_call_to_builtin_read_undeclared_channel() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: undeclared channel "Undeclared" in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[
                        Transition(target="END", condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations={},
                    actions=[
                        Assignment("Result", SubprogramCall("Read", [Variable("Undeclared")]))
                    ],
                ),
                State(name="END"),
            ],
            declarations={"Result": VariableDeclaration("Boolean")},
        )


def test_call_to_builtin_read_invalid_channel_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: invalid channel type in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Assignment("Result", SubprogramCall("Read", [Variable("Result")]))],
                ),
                State(name="END"),
            ],
            declarations={"Result": VariableDeclaration("Boolean")},
        )


def test_call_to_builtin_write_invalid_channel_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: channel "Out_Channel" not writable in call to "Write"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[
                        Transition(target="END", condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations={},
                    actions=[
                        Assignment("Result", SubprogramCall("Write", [Variable("Out_Channel")]))
                    ],
                ),
                State(name="END"),
            ],
            declarations={
                "Result": VariableDeclaration("Boolean"),
                "Out_Channel": Channel(read=True, write=False),
            },
        )


def test_call_to_builtin_data_available_invalid_channel_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: channel "Out_Channel" not readable in call to "Data_Available"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[
                        Assignment(
                            "Result", SubprogramCall("Data_Available", [Variable("Out_Channel")]),
                        )
                    ],
                ),
                State(name="END"),
            ],
            declarations={
                "Result": VariableDeclaration("Boolean"),
                "Out_Channel": Channel(read=False, write=True),
            },
        )


def test_call_to_builtin_read_invalid_channel_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: channel "Channel" not readable in call to "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Assignment("Result", SubprogramCall("Read", [Variable("Channel")]))],
                ),
                State(name="END"),
            ],
            declarations={
                "Result": VariableDeclaration("Boolean"),
                "Channel": Channel(read=False, write=True),
            },
        )


def test_call_to_builtin_call_channel_not_readable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: channel "Channel" not readable in call to "Call"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Assignment("Result", SubprogramCall("Call", [Variable("Channel")]))],
                ),
                State(name="END"),
            ],
            declarations={
                "Result": VariableDeclaration("Boolean"),
                "Channel": Channel(read=False, write=True),
            },
        )


def test_call_to_builtin_call_channel_not_writable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'session: error: channel "Channel" not writable in call to "Call"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[Assignment("Result", SubprogramCall("Call", [Variable("Channel")]))],
                ),
                State(name="END"),
            ],
            declarations={
                "Result": VariableDeclaration("Boolean"),
                "Channel": Channel(read=True, write=False),
            },
        )


def test_subprogram_call() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[Assignment("Result", SubprogramCall("Call", [Variable("Channel")]))],
            ),
            State(name="END"),
        ],
        declarations={
            "Result": VariableDeclaration("Boolean"),
            "Channel": Channel(read=True, write=True),
        },
    )


def test_undeclared_variable_in_subprogram_call() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "Undefined"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[
                        Transition(target="END", condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations={},
                    actions=[
                        Assignment("Result", SubprogramCall("SubProg", [Variable("Undefined")]),)
                    ],
                ),
                State(name="END"),
            ],
            declarations={
                "Result": VariableDeclaration("Boolean"),
                "SubProg": Subprogram([], "Boolean"),
            },
        )


def test_function_declaration_is_no_builtin_read() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'session: error: subprogram declaration shadows builtin subprogram "Read"\n'
            'session: error: unused subprogram "Read"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(name="START", transitions=[Transition(target="END")], declarations={},),
                State(name="END"),
            ],
            declarations={"Read": Subprogram([], "Boolean")},
        )


def test_function_declaration_is_no_builtin_write() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'session: error: channel declaration shadows builtin subprogram "Write"\n'
            'session: error: unused channel "Write"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(name="START", transitions=[Transition(target="END")], declarations={},),
                State(name="END"),
            ],
            declarations={"Write": Channel(read=True, write=False)},
        )


def test_function_declaration_is_no_builtin_call() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'session: error: variable declaration shadows builtin subprogram "Call"\n'
            'session: error: unused variable "Call"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(name="START", transitions=[Transition(target="END")], declarations={},),
                State(name="END"),
            ],
            declarations={"Call": VariableDeclaration("Boolean")},
        )


def test_function_declaration_is_no_builtin_data_available() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'session: error: renames declaration shadows builtin subprogram "Data_Available"\n'
            'session: error: unused renames "Data_Available"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(name="START", transitions=[Transition(target="END")], declarations={},),
                State(name="END"),
            ],
            declarations={
                "Data_Available": Renames("Boolean", Variable("Foo")),
                "Foo": VariableDeclaration("Boolean"),
            },
        )


def test_local_variable_shadows_global() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'session: error: local variable "Global" shadows global declaration in state START\n'
            'session: error: unused variable "Global"'
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[
                        Transition(target="END", condition=Equal(Variable("Global"), TRUE))
                    ],
                    declarations={ID("Global"): VariableDeclaration("Boolean")},
                ),
                State(name="END"),
            ],
            declarations={ID("Global"): VariableDeclaration("Boolean")},
        )


def test_unused_global_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^session: error: unused variable "Global"$',
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(name="START", transitions=[Transition(target="END")], declarations={},),
                State(name="END"),
            ],
            declarations={"Global": VariableDeclaration("Boolean")},
        )


def test_unused_local_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^session: error: unused local variable "Data" in state START$',
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={ID("Data"): VariableDeclaration("Boolean")},
                ),
                State(name="END"),
            ],
            declarations={},
        )


def test_renames_references_undefined_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^model: error: undeclared variable "Foo"$',
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END", condition=Equal(Variable("Ren"), TRUE))],
                    declarations={},
                ),
                State(name="END"),
            ],
            declarations={"Ren": Renames("Boolean", Variable("Foo"))},
        )


def test_binding_as_subprogram_parameter() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[
                    Assignment(
                        "Result",
                        Binding(
                            SubprogramCall("SubProg", [Length(Variable("Bound"))]),
                            {"Bound": Variable("Variable")},
                        ),
                    )
                ],
            ),
            State(name="END"),
        ],
        declarations={
            "Result": VariableDeclaration("Boolean"),
            "Variable": VariableDeclaration("Boolean"),
            "SubProg": Subprogram([], "Boolean"),
        },
    )


def test_for_all() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[
                    Transition(
                        target="END",
                        condition=ForAllIn(
                            "E",
                            Variable("List"),
                            Equal(Selected(Variable("E"), "Tag"), Number(42)),
                        ),
                    )
                ],
            ),
            State(name="END"),
        ],
        declarations={"List": VariableDeclaration("Foo")},
    )


def test_append_list_attribute() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[
                    Assignment(
                        "List", SubprogramCall("Append", [Variable("List"), Variable("Element")]),
                    )
                ],
            ),
            State(name="END"),
        ],
        declarations={
            "List": VariableDeclaration("List_Type"),
            "Element": VariableDeclaration("Element_Type"),
        },
    )


def test_extend_list_attribute() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                declarations={},
                actions=[
                    Assignment(
                        "List", SubprogramCall("Extend", [Variable("List"), Variable("Element")]),
                    )
                ],
            ),
            State(name="END"),
        ],
        declarations={
            "List": VariableDeclaration("List_Type"),
            "Element": VariableDeclaration("Element_Type"),
        },
    )


def test_aggregate_with_undefined_parameter() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "Undef"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    declarations={},
                    actions=[
                        Assignment(
                            "Data",
                            MessageAggregate(
                                "Data_Type", {"Foo": Variable("Data"), "Bar": Variable("Undef")},
                            ),
                        )
                    ],
                ),
                State(name="END"),
            ],
            declarations={"Data": VariableDeclaration("Data_Type")},
        )


def test_comprehension() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
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
            State(name="END"),
        ],
        declarations={"Input": VariableDeclaration("Foo")},
    )


def test_assignment_opaque_subprogram_undef_parameter() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: invalid action 0 of state START\n"
            'model: error: undeclared variable "UndefData"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial="START",
            final="END",
            states=[
                State(
                    name="START",
                    transitions=[Transition(target="END")],
                    actions=[
                        Assignment("Data", Opaque(SubprogramCall("Sub", [Variable("UndefData")]),),)
                    ],
                ),
                State(name="END"),
            ],
            declarations={
                "Data": VariableDeclaration("Foo"),
                "Sub": Subprogram([Argument("Param", "Param_Type")], "Result_Type"),
            },
        )


def test_assignment_opaque_subprogram_result() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                actions=[Assignment("Data", Opaque(SubprogramCall("Sub", [Variable("Data")]),),)],
            ),
            State(name="END"),
        ],
        declarations={
            "Data": VariableDeclaration("Foo"),
            "Sub": Subprogram([Argument("Param", "Param_Type")], "Result_Type"),
        },
    )


def test_assignment_opaque_subprogram_binding() -> None:
    Session(
        name="session",
        initial="START",
        final="END",
        states=[
            State(
                name="START",
                transitions=[Transition(target="END")],
                actions=[
                    Assignment(
                        "Data",
                        Binding(
                            Opaque(SubprogramCall("Sub", [Variable("Bound")])),
                            {"Bound": Variable("Data")},
                        ),
                    )
                ],
            ),
            State(name="END"),
        ],
        declarations={
            "Data": VariableDeclaration("Foo"),
            "Sub": Subprogram([Argument("Param", "Param_Type")], "Result_Type"),
        },
    )


def test_extract_variables_simple() -> None:
    result = Variable("Foo").variables()
    expected = [Variable("Foo")]
    assert result == expected


def test_extract_variables_and() -> None:
    result = And(Variable("Foo"), Variable("Bar")).variables()
    expected = [Variable("Foo"), Variable("Bar")]
    assert result == expected


def test_extract_variables_field() -> None:
    result = Selected(Variable("Foo"), "Bar").variables()
    expected = [Variable("Foo")]
    assert result == expected


def test_extract_variables_valid() -> None:
    result = Valid(Variable("Foo")).variables()
    expected = [Variable("Foo")]
    assert result == expected


def test_extract_variables_present() -> None:
    result = Present(Variable("Foo")).variables()
    expected = [Variable("Foo")]
    assert result == expected


def test_extract_variables_head() -> None:
    result = Head(Variable("Foo")).variables()
    expected = [Variable("Foo")]
    assert result == expected


def test_extract_variables_opaque() -> None:
    result = Opaque(Variable("Foo")).variables()
    expected = [Variable("Foo")]
    assert result == expected


def test_extract_variables_forallin() -> None:
    result = ForAllIn(
        "Q", Variable("List"), Equal(Selected(Variable("Q"), "Fld"), Variable("X"))
    ).variables()
    expected = [Variable("X"), Variable("List")]
    assert result == expected


def test_extract_variables_forsomein() -> None:
    result = ForAllIn(
        "Q", Variable("List"), Equal(Selected(Variable("Q"), "Fld"), Variable("X"))
    ).variables()
    expected = [Variable("X"), Variable("List")]
    assert result == expected


def test_extract_variables_contains() -> None:
    result = Contains(Variable("A"), Variable("B")).variables()
    expected = [Variable("A"), Variable("B")]
    assert result == expected


def test_extract_variables_subprogramcall() -> None:
    result = SubprogramCall("Sub", [Variable("A"), Variable("B")]).variables()
    expected = [Variable("A"), Variable("B")]
    assert result == expected


def test_extract_variables_conversion() -> None:
    result = Conversion("Sub", Variable("X")).variables()
    expected = [Variable("X")]
    assert result == expected


def test_extract_variables_comprehension() -> None:
    result = Comprehension(
        "I",
        Variable("List"),
        Selected(Variable("I"), "Data"),
        Less(Selected(Variable("I"), "X"), Variable("Z")),
    ).variables()
    expected = [Variable("List"), Variable("Z")]
    assert result == expected


def test_extract_variables_message_aggregate() -> None:
    result = MessageAggregate(
        "Aggr", {"Foo": Variable("A"), "Bar": Variable("B"), "Baz": Variable("C")}
    ).variables()
    expected = [Variable("A"), Variable("B"), Variable("C")]
    assert result == expected


def test_extract_variables_binding() -> None:
    result = Binding(
        Less(Variable("A"), Variable("Bound")), {"Bound": Less(Variable("B"), Variable("C"))}
    ).variables()
    expected = [Variable("A"), Variable("B"), Variable("C")]
    assert result == expected


def test_extract_variables_string() -> None:
    result = String("Foo").variables()
    assert result == []
