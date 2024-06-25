from pathlib import Path

import pytest

from rflx.generator.common import external_io_buffers
from rflx.identifier import ID
from rflx.integration import IntegrationFile, SessionIntegration
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


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability_with_external_io_buffers(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.prove is None:
        pytest.skip()
    model, integration = create_model(feature)
    if not model.sessions:
        pytest.skip()
    if integration._packages:  # noqa: SLF001
        integration._packages["test"].session["Session"].external_io_buffers = True  # noqa: SLF001
    else:
        integration.add_integration_file(
            "test",
            IntegrationFile(
                Session={"Session": SessionIntegration(Buffer_Size=None, External_IO_Buffers=True)},
            ),
        )
    config.external_io_buffers = len(external_io_buffers(model.sessions[0].to_ir()))
    create_complement(config, feature, tmp_path)
    assert_provable_code(
        model,
        integration,
        tmp_path,
        main=MAIN,
        units=["main.adb", "lib.adb", "rflx-test-session.adb", *config.prove],
    )
