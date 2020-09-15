import pytest

import rflx.declaration as decl
import rflx.expression as expr
import rflx.statement as stmt
from rflx.error import RecordFluxError
from rflx.identifier import ID
from rflx.model import BOOLEAN, Session, State, Transition
from tests.utils import assert_equal, multilinestr


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
        match='^model: error: invalid session name "P::S::X"',
    ):
        Session(
            identifier="P::S::X",
            initial=ID("START"),
            final=ID("END"),
            states=[],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_empty_states() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "model: error: empty states\n"
            'model: error: initial state "START" does not exist in "P::S"\n'
            'model: error: final state "END" does not exist in "P::S"'
            "$"
        ),
    ):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_invalid_initial() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'model: error: initial state "NONEXISTENT" does not exist in "P::S"\n'
            "model: error: unreachable states START"
            "$"
        ),
    ):
        Session(
            identifier="P::S",
            initial=ID("NONEXISTENT"),
            final=ID("END"),
            states=[
                State(name=ID("START"), transitions=[Transition(target=ID("END"))]),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_invalid_final() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            'model: error: final state "NONEXISTENT" does not exist in "P::S"\n'
            "model: error: detached states END"
            "$"
        ),
    ):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("NONEXISTENT"),
            states=[
                State(name=ID("START"), transitions=[Transition(target=ID("END"))]),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_invalid_target_state() -> None:
    with pytest.raises(
        RecordFluxError,
        match='^model: error: transition from state "START" to non-existent'
        ' state "NONEXISTENT" in "P::S"$',
    ):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("NONEXISTENT")),
                        Transition(target=ID("END")),
                    ],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_duplicate_state() -> None:
    with pytest.raises(RecordFluxError, match="^model: error: duplicate states: START$"):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(name=ID("START"), transitions=[Transition(target=ID("END"))]),
                State(name=ID("START"), transitions=[Transition(target=ID("END"))]),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_multiple_duplicate_states() -> None:
    with pytest.raises(
        RecordFluxError, match=("^model: error: duplicate states: BAR, FOO, START$")
    ):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(name=ID("START"), transitions=[Transition(target=ID("FOO"))]),
                State(name=ID("START"), transitions=[Transition(target=ID("FOO"))]),
                State(name=ID("FOO"), transitions=[Transition(target=ID("BAR"))]),
                State(name=ID("BAR"), transitions=[Transition(target=ID("END"))]),
                State(name=ID("FOO"), transitions=[Transition(target=ID("BAR"))]),
                State(name=ID("BAR"), transitions=[Transition(target=ID("END"))]),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_unreachable_state() -> None:
    with pytest.raises(RecordFluxError, match="^model: error: unreachable states UNREACHABLE$"):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(name=ID("START"), transitions=[Transition(target=ID("END"))]),
                State(
                    name=ID("UNREACHABLE"),
                    transitions=[Transition(target=ID("END"))],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_multiple_unreachable_states() -> None:
    with pytest.raises(
        RecordFluxError, match="^model: error: unreachable states UNREACHABLE1, UNREACHABLE2$"
    ):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(name=ID("START"), transitions=[Transition(target=ID("END"))]),
                State(
                    name=ID("UNREACHABLE1"),
                    transitions=[Transition(target=ID("END"))],
                ),
                State(
                    name=ID("UNREACHABLE2"),
                    transitions=[Transition(target=ID("END"))],
                ),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_detached_state() -> None:
    with pytest.raises(RecordFluxError, match="^model: error: detached states DETACHED$"):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[Transition(target=ID("END")), Transition(target=ID("DETACHED"))],
                ),
                State(name=ID("DETACHED")),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )


def test_multiple_detached_states() -> None:
    with pytest.raises(
        RecordFluxError, match="^model: error: detached states DETACHED1, DETACHED2$"
    ):
        Session(
            identifier="P::S",
            initial=ID("START"),
            final=ID("END"),
            states=[
                State(
                    name=ID("START"),
                    transitions=[
                        Transition(target=ID("END")),
                        Transition(target=ID("DETACHED1")),
                        Transition(target=ID("DETACHED2")),
                    ],
                ),
                State(name=ID("DETACHED1")),
                State(name=ID("DETACHED2")),
                State(name=ID("END")),
            ],
            declarations=[],
            parameters=[],
            types=[],
        )
