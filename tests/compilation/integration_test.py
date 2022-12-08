from pathlib import Path

from rflx.integration import Integration
from rflx.model import Model, Session, State, Transition
from tests.utils import assert_compilable_code


def test_session_with_only_null_state(tmp_path: Path) -> None:
    state = State("St", [Transition("null")])
    session = Session("P::S", states=[state], declarations=[], parameters=[], types=[])
    model = Model(types=[], sessions=[session])
    assert_compilable_code(model, Integration(), tmp_path)
