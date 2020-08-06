# pylint: disable=too-many-lines
import pytest

from rflx.declaration import (
    Argument,
    Channel,
    PrivateDeclaration,
    Renames,
    Subprogram,
    VariableDeclaration,
)
from rflx.error import RecordFluxError
from rflx.expression import (
    FALSE,
    TRUE,
    And,
    Binding,
    Call,
    Comprehension,
    Conversion,
    Equal,
    ForAllIn,
    ForSomeIn,
    Head,
    In,
    Length,
    Less,
    MessageAggregate,
    Not,
    NotIn,
    Number,
    Opaque,
    Present,
    Selected,
    String,
    Valid,
    Variable,
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
    binding = Binding(In(Variable("X"), Variable("Y")), {"X": Variable("Baz")},)
    expected = In(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_contains_right() -> None:
    binding = Binding(In(Variable("X"), Variable("Y")), {"Y": Variable("Baz")},)
    expected = In(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_not_contains_left() -> None:
    binding = Binding(NotIn(Variable("X"), Variable("Y")), {"X": Variable("Baz")},)
    expected = NotIn(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_not_contains_right() -> None:
    binding = Binding(NotIn(Variable("X"), Variable("Y")), {"Y": Variable("Baz")},)
    expected = NotIn(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_subprogram() -> None:
    binding = Binding(
        Call("Sub", [Variable("A"), Variable("B"), Variable("C")]), {"B": Variable("Baz")},
    )
    expected = Call("Sub", [Variable("A"), Variable("Baz"), Variable("C")])
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
    binding = Binding(Call("Sub", [Variable("A"), Variable("A")]), {"A": Variable("Baz")})
    expected = Call("Sub", [Variable("Baz"), Variable("Baz")])
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
    binding = Binding(Opaque(Call("Sub", [Variable("Bound")])), {"Bound": Variable("Foo")})
    expected = Opaque(Call("Sub", [Variable("Foo")]))
    result = binding.simplified()
    assert result == expected


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
            declarations={},
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
        declarations={"Defined": VariableDeclaration("Some_Type")},
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
                declarations={ID("Local"): VariableDeclaration("Some_Type")},
            ),
            State(name=ID("END")),
        ],
        declarations={"Global": VariableDeclaration("Some_Type")},
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
                    declarations={},
                ),
                State(
                    name=ID("STATE"),
                    transitions=[
                        Transition(
                            target=ID("END"),
                            condition=Equal(Variable("Local"), Variable("Global")),
                        )
                    ],
                    declarations={},
                ),
                State(name=ID("END")),
            ],
            declarations={"Global": VariableDeclaration("Some_Type")},
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
                    Transition(target=ID("END"), condition=Equal(Valid(Variable("Global")), TRUE),)
                ],
                declarations={},
            ),
            State(name=ID("END")),
        ],
        declarations={"Global": VariableDeclaration("Boolean")},
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
                declarations={},
            ),
            State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Undefined", FALSE)],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Global", Variable("Undefined"))],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Erase("Undefined")],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Reset("Undefined")],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Global", Call("UndefSub", [Variable("Global")]))],
                ),
                State(name=ID("END")),
            ],
            declarations={"Global": VariableDeclaration("Boolean")},
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
                declarations={},
                actions=[Assignment("Global", Call("Read", [Variable("Some_Channel")]))],
            ),
            State(name=ID("END")),
        ],
        declarations={
            "Global": VariableDeclaration("Boolean"),
            "Some_Channel": Channel(read=True, write=False),
        },
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
                declarations={},
                actions=[Assignment("Success", Call("Write", [Variable("Some_Channel"), TRUE]),)],
            ),
            State(name=ID("END")),
        ],
        declarations={
            "Success": VariableDeclaration("Boolean"),
            "Some_Channel": Channel(read=False, write=True),
        },
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
                declarations={},
                actions=[Assignment("Result", Call("Call", [Variable("Some_Channel"), TRUE]))],
            ),
            State(name=ID("END")),
        ],
        declarations={
            "Result": VariableDeclaration("Boolean"),
            "Some_Channel": Channel(read=True, write=True),
        },
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
                declarations={},
                actions=[Assignment("Result", Call("Data_Available", [Variable("Some_Channel")]),)],
            ),
            State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Result", Call("Read", []))],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations={},
                    actions=[Assignment("Result", Call("Read", [Variable("Undeclared")]))],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Result", Call("Read", [Variable("Result")]))],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations={},
                    actions=[Assignment("Result", Call("Write", [Variable("Out_Channel")]))],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[
                        Assignment("Result", Call("Data_Available", [Variable("Out_Channel")]),)
                    ],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Result", Call("Read", [Variable("Channel")]))],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Result", Call("Call", [Variable("Channel")]))],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={},
                    actions=[Assignment("Result", Call("Call", [Variable("Channel")]))],
                ),
                State(name=ID("END")),
            ],
            declarations={
                "Result": VariableDeclaration("Boolean"),
                "Channel": Channel(read=True, write=False),
            },
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
                declarations={},
                actions=[Assignment("Result", Call("Call", [Variable("Channel")]))],
            ),
            State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Result"), TRUE))
                    ],
                    declarations={},
                    actions=[Assignment("Result", Call("SubProg", [Variable("Undefined")]),)],
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations={},
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations={},
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations={},
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations={},
                ),
                State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END"), condition=Equal(Variable("Global"), TRUE))
                    ],
                    declarations={ID("Global"): VariableDeclaration("Boolean")},
                ),
                State(name=ID("END")),
            ],
            declarations={ID("Global"): VariableDeclaration("Boolean")},
        )


def test_unused_global_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^session: error: unused variable "Global"$',
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations={},
                ),
                State(name=ID("END")),
            ],
            declarations={"Global": VariableDeclaration("Boolean")},
        )


def test_unused_local_variable() -> None:
    with pytest.raises(
        RecordFluxError, match='^session: error: unused local variable "Data" in state START$',
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
                    declarations={ID("Data"): VariableDeclaration("Boolean")},
                ),
                State(name=ID("END")),
            ],
            declarations={},
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
                    declarations={},
                ),
                State(name=ID("END")),
            ],
            declarations={"Ren": Renames("Boolean", Variable("Foo"))},
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
                declarations={},
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
        declarations={
            "Result": VariableDeclaration("Boolean"),
            "Variable": VariableDeclaration("Boolean"),
            "SubProg": Subprogram([], "Boolean"),
        },
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
        declarations={"List": VariableDeclaration("Foo")},
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
                declarations={},
                actions=[
                    Assignment("List", Call("Append", [Variable("List"), Variable("Element")]),)
                ],
            ),
            State(name=ID("END")),
        ],
        declarations={
            "List": VariableDeclaration("List_Type"),
            "Element": VariableDeclaration("Element_Type"),
        },
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
                declarations={},
                actions=[
                    Assignment("List", Call("Extend", [Variable("List"), Variable("Element")]),)
                ],
            ),
            State(name=ID("END")),
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
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END"))],
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
                State(name=ID("END")),
            ],
            declarations={"Data": VariableDeclaration("Data_Type")},
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
            declarations={
                "Data": VariableDeclaration("Foo"),
                "Sub": Subprogram([Argument("Param", "Param_Type")], "Result_Type"),
            },
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
        declarations={
            "Data": VariableDeclaration("Foo"),
            "Sub": Subprogram([Argument("Param", "Param_Type")], "Result_Type"),
        },
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
    result = In(Variable("A"), Variable("B")).variables()
    expected = [Variable("A"), Variable("B")]
    assert result == expected


def test_extract_variables_subprogramcall() -> None:
    result = Call("Sub", [Variable("A"), Variable("B")]).variables()
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


def test_private_declaration_is_no_builtin_write() -> None:
    with pytest.raises(
        RecordFluxError,
        match=("^" 'session: error: private declaration shadows builtin subprogram "Write"' "$"),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("END"))], declarations={},
                ),
                State(name=ID("END")),
            ],
            declarations={"Write": PrivateDeclaration()},
        )


def test_duplicate_states() -> None:
    with pytest.raises(
        RecordFluxError, match=("^session: error: duplicate states: FOO$"),
    ):
        Session(
            name="session",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"), transitions=[Transition(target=ID("FOO"))], declarations={},
                ),
                State(name=ID("FOO"), transitions=[Transition(target=ID("END"))], declarations={},),
                State(name=ID("FOO"), transitions=[Transition(target=ID("END"))], declarations={},),
                State(name=ID("END")),
            ],
            declarations={},
        )


def test_sort_state_name() -> None:
    assert sorted([ID("foo"), ID("bar")]) == [ID("bar"), ID("foo")]


def test_invalid_channel_id_type() -> None:
    with pytest.raises(
        RecordFluxError, match=('^session: error: invalid channel ID type in call to "Read"$')
    ):
        Call("Read", [Number(5)]).validate({})


def test_validate_not() -> None:
    Not(TRUE).validate({})


def test_validate_builtin_validate() -> None:
    Variable("Boolean").validate({})
