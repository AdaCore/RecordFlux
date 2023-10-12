from pathlib import Path

import pytest

from rflx.identifier import ID
from tests.const import MAIN
from tests.feature import FEATURES, create_complement, create_model, get_config
from tests.utils import assert_provable_code


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.prove is None:
        pytest.skip()
    model, integration = create_model(feature)
    units = []
    if model.sessions:
        assert len(model.sessions) == 1
        assert model.sessions[0].identifier == ID("Test::Session")
        units = ["main.adb", "lib.adb", "rflx-test-session.adb"]
        create_complement(config, feature, tmp_path)
        main = MAIN
    else:
        main = None
    assert_provable_code(model, integration, tmp_path, main=main, units=[*units, *config.prove])
