import pytest

from rflx.error import RecordFluxError
from rflx.fsm import FSM, State, StateMachine, StateName, Transition


def assert_parse_exception_string(string: str, regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        FSM().parse_string("fsm", string)


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
        initial=StateName("START"),
        final=StateName("END"),
        states=[
            State(name=StateName("START"), transitions=[Transition(target=StateName("END"))]),
            State(name=StateName("END")),
        ],
    )
    assert f.fsms["fsm"] == expected


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
        StateMachine(initial=StateName("START"), final=StateName("END"), states=[])
