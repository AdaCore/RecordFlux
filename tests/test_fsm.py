import pytest

from rflx.error import RecordFluxError
from rflx.expression import FALSE, Argument, Equal, Subprogram, Variable, VariableDeclaration
from rflx.fsm import FSM, State, StateMachine, StateName, Transition
from rflx.identifier import ID


def assert_parse_exception_string(string: str, regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        fsm = FSM()
        fsm.parse_string("fsm", string)
        fsm.error.propagate()


def test_simple_fsm() -> None:
    f = FSM()
    f.parse_string(
        "fsm",
        """
            initial: START
            final: END
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
    )
    expected = StateMachine(
        name="fsm",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
            State(name=StateName("END")),
        ],
        declarations={},
    )
    assert f.fsms[0] == expected


def test_missing_initial() -> None:
    assert_parse_exception_string(
        """
            final: END
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
        '^session: error: missing initial state in "fsm"$',
    )


def test_missing_final() -> None:
    assert_parse_exception_string(
        """
            initial: START
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
        '^session: error: missing final state in "fsm"$',
    )


def test_missing_states() -> None:
    assert_parse_exception_string(
        """
            initial: START
            final: END
        """,
        '^session: error: missing states section in "fsm"$',
    )


def test_empty_states() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: empty states"):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[],
            declarations={},
        )


def test_invalid_initial() -> None:
    with pytest.raises(
        RecordFluxError,
        match='^session: error: initial state "NONEXISTENT" does not exist in "fsm"',
    ):
        StateMachine(
            name="fsm",
            initial=StateName("NONEXISTENT"),
            final=StateName("END"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_invalid_final() -> None:
    with pytest.raises(
        RecordFluxError, match='^session: error: final state "NONEXISTENT" does not exist in "fsm"'
    ):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("NONEXISTENT"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_invalid_target_state() -> None:
    with pytest.raises(
        RecordFluxError,
        match='^session: error: transition from state "START" to non-existent'
        ' state "NONEXISTENT" in "fsm"',
    ):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(
                    name=StateName("START"),
                    transitions=[Transition(target=StateName("NONEXISTENT"))],
                ),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_duplicate_state() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: duplicate states: START"):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("START")),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_multiple_duplicate_states() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: duplicate states: BAR, FOO, START"):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("START")),
                State(name=StateName("FOO")),
                State(name=StateName("BAR")),
                State(name=StateName("FOO")),
                State(name=StateName("BAR")),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_unreachable_state() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: unreachable states UNREACHABLE"):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(
                    name=StateName("UNREACHABLE"),
                    transitions=[Transition(target=StateName("END"))],
                ),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_multiple_unreachable_states() -> None:
    with pytest.raises(
        RecordFluxError, match="^session: error: unreachable states UNREACHABLE1, UNREACHABLE2"
    ):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(
                    name=StateName("UNREACHABLE1"),
                    transitions=[Transition(target=StateName("END"))],
                ),
                State(
                    name=StateName("UNREACHABLE2"),
                    transitions=[Transition(target=StateName("END"))],
                ),
                State(name=StateName("END")),
            ],
            declarations={"Error": VariableDeclaration("Boolean")},
        )


def test_detached_state() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: detached states DETACHED"):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(
                    name=StateName("START"),
                    transitions=[
                        Transition(target=StateName("END")),
                        Transition(target=StateName("DETACHED")),
                    ],
                ),
                State(name=StateName("DETACHED")),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_multiple_detached_states() -> None:
    with pytest.raises(
        RecordFluxError, match="^session: error: detached states DETACHED1, DETACHED2"
    ):
        StateMachine(
            name="fsm",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(
                    name=StateName("START"),
                    transitions=[
                        Transition(target=StateName("END")),
                        Transition(target=StateName("DETACHED1")),
                        Transition(target=StateName("DETACHED2")),
                    ],
                ),
                State(name=StateName("DETACHED1")),
                State(name=StateName("DETACHED2")),
                State(name=StateName("END")),
            ],
            declarations={
                "Error": VariableDeclaration("Boolean"),
                "Something": VariableDeclaration("Boolean"),
            },
        )


def test_fsm_with_conditions() -> None:
    f = FSM()
    f.parse_string(
        "fsm",
        """
            initial: START
            final: END
            variables:
                - "Error: Boolean"
            states:
              - name: START
                transitions:
                  - target: INTERMEDIATE
                    condition: Error = False
                  - target: END
              - name: INTERMEDIATE
                transitions:
                  - target: END
              - name: END
        """,
    )
    expected = StateMachine(
        name="fsm",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[
                    Transition(
                        target=StateName("INTERMEDIATE"), condition=Equal(Variable("Error"), FALSE),
                    ),
                    Transition(target=StateName("END")),
                ],
            ),
            State(
                name=StateName("INTERMEDIATE"), transitions=[Transition(target=StateName("END"))],
            ),
            State(name=StateName("END")),
        ],
        declarations={"Error": VariableDeclaration("Boolean")},
    )
    assert f.fsms[0] == expected


def test_fsm_with_invalid_condition() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            '<stdin>:1:1: parser: error: reserved word "and" used as identifier\n'
            'session: error: invalid condition 0 from state "START" to "INTERMEDIATE"'
            "$"
        ),
    ):
        FSM().parse_string(
            "fsm",
            """
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: INTERMEDIATE
                        condition: and Invalid
                      - target: END
                  - name: INTERMEDIATE
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_fsm_condition_equal() -> None:
    f = FSM()
    f.parse_string(
        "fsm",
        """
            initial: START
            final: END
            variables:
                - "Error: Boolean"
                - "Something: Boolean"
            states:
              - name: START
                transitions:
                  - target: INTERMEDIATE
                    condition: Error = Something
                  - target: END
              - name: INTERMEDIATE
                transitions:
                  - target: END
              - name: END
        """,
    )
    expected = StateMachine(
        name="fsm",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[
                    Transition(
                        target=StateName("INTERMEDIATE"),
                        condition=Equal(Variable("Error"), Variable("Something"),),
                    ),
                    Transition(target=StateName("END")),
                ],
            ),
            State(
                name=StateName("INTERMEDIATE"), transitions=[Transition(target=StateName("END"))],
            ),
            State(name=StateName("END")),
        ],
        declarations={
            "Error": VariableDeclaration("Boolean"),
            "Something": VariableDeclaration("Boolean"),
        },
    )
    assert f.fsms[0] == expected


def test_unexpected_elements() -> None:
    assert_parse_exception_string(
        """
            initial: START
            final: END
            invalid1: FOO
            invalid2: BAR
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
        r"^session: error: unexpected elements: invalid1, invalid2",
    )


def test_fsm_with_function_decl() -> None:
    f = FSM()
    f.parse_string(
        "fsm",
        """
            initial: START
            final: END
            functions:
                - \"Foo(Bar : T1; Baz : P1.T1) return P2.T3\"
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
    )
    expected = StateMachine(
        name="fsm",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
            State(name=StateName("END")),
        ],
        declarations={
            ID("Foo"): Subprogram([Argument("Bar", "T1"), Argument("Baz", "P1.T1")], "P2.T3")
        },
    )
    assert f.fsms[0] == expected
