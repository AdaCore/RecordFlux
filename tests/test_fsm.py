from rflx.fsm import FSM, State, StateMachine, StateName, Transition


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
