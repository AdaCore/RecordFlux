import pytest

from rflx.error import RecordFluxError
from rflx.fsm import FSM, State, StateMachine, StateName, Transition


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
        StateMachine(name="fsm", initial=StateName("START"), final=StateName("END"), states=[])


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
        )
