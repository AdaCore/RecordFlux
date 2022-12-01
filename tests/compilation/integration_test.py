from pathlib import Path

import pytest

from rflx.integration import Integration
from rflx.model import Model, Session, State, Transition
from tests.const import MAIN
from tests.utils import (
    FEATURES,
    assert_compilable_code,
    assert_executable_code,
    create_complement,
    create_model,
    get_config,
)


@pytest.mark.compilation
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_compilability(feature: str, tmp_path: Path) -> None:
    model, integration = create_model(feature)
    assert_compilable_code(model, integration, tmp_path)


@pytest.mark.compilation
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_executability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if not config.sequence:
        pytest.skip()
    model, integration = create_model(feature)
    create_complement(config, feature, tmp_path)
    assert assert_executable_code(model, integration, tmp_path, main=MAIN) == config.sequence


@pytest.mark.compilation
def test_session_with_only_null_state(tmp_path: Path) -> None:
    state = State("St", [Transition("null")])
    session = Session("P::S", states=[state], declarations=[], parameters=[], types=[])
    model = Model(types=[], sessions=[session])
    assert_compilable_code(model, Integration(), tmp_path)
