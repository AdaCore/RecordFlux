# pylint: disable=too-many-lines

import pytest

from rflx.error import RecordFluxError
from rflx.expression import (
    FALSE,
    Argument,
    Call,
    Equal,
    Greater,
    Number,
    Renames,
    Subprogram,
    Variable,
    VariableDeclaration,
)
from rflx.identifier import ID
from rflx.session import Channel, Session, State, StateName, Transition
from rflx.sessionfile import SessionFile
from rflx.statement import Assignment


def assert_parse_exception_string(string: str, regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        session = SessionFile()
        session.parse_string("session", string)
        session.error.propagate()


def test_simple_session() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
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
    expected = Session(
        name="session",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
            State(name=StateName("END")),
        ],
        declarations={},
    )
    assert f.sessions[0] == expected


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
        '^session: error: missing initial state in "session"$',
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
        '^session: error: missing final state in "session"$',
    )


def test_missing_states() -> None:
    assert_parse_exception_string(
        """
            initial: START
            final: END
        """,
        '^session: error: missing states section in "session"$',
    )


def test_empty_states() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: empty states\n"
            'session: error: initial state "START" does not exist in "session"\n'
            'session: error: final state "END" does not exist in "session"'
            "$"
        ),
    ):
        Session(
            name="session",
            initial=StateName("START"),
            final=StateName("END"),
            states=[],
            declarations={},
        )


def test_invalid_initial() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'session: error: initial state "NONEXISTENT" does not exist in "session"\n'
            "session: error: unreachable states START"
            "$"
        ),
    ):
        Session(
            name="session",
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
        RecordFluxError,
        match=(
            "^"
            'session: error: final state "NONEXISTENT" does not exist in "session"\n'
            "session: error: detached states END"
            "$"
        ),
    ):
        Session(
            name="session",
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
        ' state "NONEXISTENT" in "session"$',
    ):
        Session(
            name="session",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(
                    name=StateName("START"),
                    transitions=[
                        Transition(target=StateName("NONEXISTENT")),
                        Transition(target=StateName("END")),
                    ],
                ),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_duplicate_state() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: duplicate states: START$"):
        Session(
            name="session",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_multiple_duplicate_states() -> None:
    with pytest.raises(
        RecordFluxError, match=("^session: error: duplicate states: BAR, FOO, START$")
    ):
        Session(
            name="session",
            initial=StateName("START"),
            final=StateName("END"),
            states=[
                State(name=StateName("START"), transitions=[Transition(target=StateName("FOO"))]),
                State(name=StateName("START"), transitions=[Transition(target=StateName("FOO"))]),
                State(name=StateName("FOO"), transitions=[Transition(target=StateName("BAR"))]),
                State(name=StateName("BAR"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("FOO"), transitions=[Transition(target=StateName("BAR"))]),
                State(name=StateName("BAR"), transitions=[Transition(target=StateName("END"))]),
                State(name=StateName("END")),
            ],
            declarations={},
        )


def test_unreachable_state() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: unreachable states UNREACHABLE$"):
        Session(
            name="session",
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
        RecordFluxError, match="^session: error: unreachable states UNREACHABLE1, UNREACHABLE2$"
    ):
        Session(
            name="session",
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
            declarations={},
        )


def test_detached_state() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: detached states DETACHED$"):
        Session(
            name="session",
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
        RecordFluxError, match="^session: error: detached states DETACHED1, DETACHED2$"
    ):
        Session(
            name="session",
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
            declarations={},
        )


def test_session_with_conditions() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
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
    expected = Session(
        name="session",
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
    assert f.sessions[0] == expected


def test_session_with_invalid_condition() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'session: error: invalid condition 0 from state "START" to "INTERMEDIATE"\n'
            '<stdin>:1:1: parser: error: reserved word "and" used as identifier'
            "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                variables:
                    - "Bar : Boolean"
                renames:
                    - "Foo : Boolean renames Bar"
                states:
                  - name: START
                    transitions:
                      - target: INTERMEDIATE
                        condition: and Invalid
                      - target: END
                  - name: INTERMEDIATE
                    transitions:
                      - target: END
                        condition: Foo = False
                  - name: END
            """,
        )


def test_session_condition_equal() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
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
    expected = Session(
        name="session",
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
    assert f.sessions[0] == expected


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
        r"^session: error: unexpected elements: invalid1, invalid2$",
    )


def test_session_with_function_decl() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
        """
            initial: START
            final: END
            functions:
                - \"Foo(Bar : T1; Baz : P1.T1) return P2.T3\"
            states:
              - name: START
                transitions:
                  - target: END
                    condition: Foo (100, 200) > 1000
              - name: END
        """,
    )
    expected = Session(
        name="session",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[
                    Transition(
                        target=StateName("END"),
                        condition=Greater(Call("Foo", [Number(100), Number(200)]), Number(1000),),
                    )
                ],
            ),
            State(name=StateName("END")),
        ],
        declarations={
            ID("Foo"): Subprogram([Argument("Bar", "T1"), Argument("Baz", "P1.T1")], "P2.T3")
        },
    )
    assert f.sessions[0] == expected


def test_session_with_variable_decl() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
        """
            initial: START
            final: END
            variables:
                - \"Global : Boolean\"
            states:
              - name: START
                variables:
                    - \"Local : Boolean\"
                transitions:
                  - target: END
                    condition: Local = Global
              - name: END
        """,
    )
    expected = Session(
        name="session",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[
                    Transition(
                        target=StateName("END"),
                        condition=Equal(Variable("Local"), Variable("Global")),
                    )
                ],
                declarations={ID("Local"): VariableDeclaration("Boolean")},
            ),
            State(name=StateName("END")),
        ],
        declarations={"Global": VariableDeclaration("Boolean")},
    )
    assert f.sessions[0] == expected


def test_session_with_actions() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
        """
            initial: START
            final: END
            variables:
                - \"Global : Boolean\"
            states:
              - name: START
                transitions:
                  - target: END
                actions:
                    - Global := False
              - name: END
        """,
    )
    expected = Session(
        name="session",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[Transition(target=StateName("END"))],
                declarations={},
                actions=[Assignment("Global", FALSE)],
            ),
            State(name=StateName("END")),
        ],
        declarations={"Global": VariableDeclaration("Boolean")},
    )
    assert f.sessions[0] == expected


def test_duplicate_variable() -> None:
    assert_parse_exception_string(
        """
            initial: START
            final: END
            variables:
                - "Foo : Boolean"
                - "Foo : Some_Type"
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
        r"^session: error: conflicting variable Foo$",
    )


def test_variable_shadowing_channel_name() -> None:
    assert_parse_exception_string(
        """
            initial: START
            final: END
            channels:
                - name: Foo
                  mode: Read
            variables:
                - "Foo : Boolean"
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
        r'^session: error: conflicting channel "Foo"$',
    )


def test_channel_shadowing_rename() -> None:
    assert_parse_exception_string(
        """
            initial: START
            final: END
            channels:
                - name: Foo
                  mode: Read
            renames:
                - "Foo : Boolean renames Bar"
            states:
              - name: START
                transitions:
                  - target: END
              - name: END
        """,
        r"^session: error: conflicting renames Foo$",
    )


def test_session_with_renames() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
        """
            initial: START
            final: END
            variables:
                - "Bar : Boolean"
            renames:
                - "Foo : Boolean renames Bar"
            states:
              - name: START
                transitions:
                  - target: END
                    condition: Foo = False
              - name: END
        """,
    )
    expected = Session(
        name="session",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[
                    Transition(target=StateName("END"), condition=Equal(Variable("Foo"), FALSE))
                ],
            ),
            State(name=StateName("END")),
        ],
        declarations={
            "Foo": Renames("Boolean", Variable("Bar")),
            "Bar": VariableDeclaration("Boolean"),
        },
    )
    assert f.sessions[0] == expected


def test_channels() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
        """
            channels:
                - name: Channel1_Read_Write
                  mode: Read_Write
                - name: Channel2_Read
                  mode: Read
                - name: Channel3_Write
                  mode: Write
            initial: START
            final: END
            states:
              - name: START
                transitions:
                  - target: END
                variables:
                  - "Local : Boolean"
                actions:
                  - Local := Write(Channel1_Read_Write, Read(Channel2_Read))
                  - Local := Write(Channel3_Write, Local)
              - name: END
        """,
    )
    expected = Session(
        name="session",
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(
                name=StateName("START"),
                transitions=[Transition(target=StateName("END"))],
                declarations={ID("Local"): VariableDeclaration("Boolean")},
                actions=[
                    Assignment(
                        "Local",
                        Call(
                            "Write",
                            [
                                Variable("Channel1_Read_Write"),
                                Call("Read", [Variable("Channel2_Read")]),
                            ],
                        ),
                    ),
                    Assignment(
                        "Local", Call("Write", [Variable("Channel3_Write"), Variable("Local")],),
                    ),
                ],
            ),
            State(name=StateName("END")),
        ],
        declarations={
            "Channel1_Read_Write": Channel(read=True, write=True),
            "Channel2_Read": Channel(read=True, write=False),
            "Channel3_Write": Channel(read=False, write=True),
        },
    )
    assert f.sessions[0] == expected


def test_channel_with_invalid_mode() -> None:
    with pytest.raises(
        RecordFluxError,
        match="^session: error: channel Channel1_Read_Write has invalid mode Invalid",
    ):
        SessionFile().parse_string(
            "session",
            """
                channels:
                    - name: Channel1_Read_Write
                      mode: Invalid
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                    variables:
                      - "Local : Boolean"
                    actions:
                      - Local := Read(Channel1_Read_Write)
                  - name: END
            """,
        )


def test_channel_without_name() -> None:
    with pytest.raises(RecordFluxError, match="^session: error: channel 0 has no name"):
        SessionFile().parse_string(
            "session",
            """
                channels:
                    - mode: Read_Write
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_channel_without_mode() -> None:
    with pytest.raises(
        RecordFluxError, match="^session: error: channel Channel_Without_Mode has no mode"
    ):
        SessionFile().parse_string(
            "session",
            """
                channels:
                    - name: Channel_Without_Mode
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_function_used() -> None:
    f = SessionFile()
    f.parse_string(
        "session",
        """
            initial: START
            final: FINAL
            variables:
              - "Data : Data_Type"
            functions:
              - "Calculate (Input : Data_Type) return Data_Type"
            states:
              - name: START
                actions:
                  - Data := Calculate (5)
                transitions:
                  - target: FINAL
              - name: FINAL
        """,
    )
    expected = Session(
        name="session",
        initial=StateName("START"),
        final=StateName("FINAL"),
        states=[
            State(
                name=StateName("START"),
                transitions=[Transition(target=StateName("FINAL"))],
                declarations={},
                actions=[Assignment("Data", Call("Calculate", [Number(5)]))],
            ),
            State(name=StateName("FINAL")),
        ],
        declarations={
            "Calculate": Subprogram([Argument("Input", "Data_Type")], "Data_Type"),
            "Data": VariableDeclaration("Data_Type"),
        },
    )
    assert f.sessions[0] == expected


def test_tls_handshake() -> None:
    SessionFile().parse("tls_handshake", "tests/client_handshake_states.yml")


def test_tls_record() -> None:
    SessionFile().parse("tls_handshake", "tests/client_record_states.yml")


def test_invalid_global_variable_declaration() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: error parsing global variable declaration 0\n"
            '<stdin>:1:8: parser: error: reserved word "and" used as identifier'
            "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                variables:
                    - "Blub : and bar"
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_invalid_local_variable_declaration() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: error parsing local variable 0 in state START\n"
            '<stdin>:1:7: parser: error: reserved word "and" used as identifier'
            "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                states:
                  - name: START
                    variables:
                        - "Foo : and Invalid"
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_invalid_action() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: error parsing action 0 in state START\n"
            "<stdin>:1:9: parser: error: Expected.*"
            "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                    actions:
                      - invalid action
                  - name: END
            """,
        )


def test_invalid_section() -> None:
    with pytest.raises(
        RecordFluxError,
        match=("^" 'session: error: unexpected elements in state "START": invalid' "$"),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                    actions:
                      - invalid action
                    invalid: INVALID
                  - name: END
            """,
        )


def test_invalid_transition() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^" 'session: error: unexpected elements in transition 0 in state "START": invalid' "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                states:
                  - name: START
                    transitions:
                      - target: END
                        invalid: INVALID
                  - name: END
            """,
        )


def test_invalid_renames() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: error parsing renames declaration 0\n"
            "<stdin>:1:9: parser: error: Expected.*"
            "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                renames:
                    - this is invalid
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_conflicting_types() -> None:
    with pytest.raises(
        RecordFluxError, match=("^" "session: error: conflicting type Foo" "$"),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                types:
                    - "Foo: Foo_Type"
                    - "Foo: Bar_Type"
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_invalid_private_variable_declaration() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: error parsing private variable declaration 0\n"
            "<stdin>:1:9: parser: error: Expected.*"
            "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                types:
                    - this is invalid
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_conflicting_functions() -> None:
    with pytest.raises(
        RecordFluxError, match=("^" "session: error: conflicting function Foo" "$"),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                functions:
                    - "Foo (Arg1 : Foo_Type) return Bar_Type"
                    - "Foo (Arg1 : Boolean; Arg2 : Boolean) return Boolean"
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )


def test_invalid_function() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "session: error: error parsing global function declaration 0\n"
            "<stdin>:1:9: parser: error: Expected.*"
            "$"
        ),
    ):
        SessionFile().parse_string(
            "session",
            """
                initial: START
                final: END
                functions:
                    - this is invalid
                states:
                  - name: START
                    transitions:
                      - target: END
                  - name: END
            """,
        )
