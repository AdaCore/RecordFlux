from pathlib import Path

import pytest

from rflx.generator.common import external_io_buffers
from rflx.integration import IntegrationFile, SessionIntegration
from tests.const import MAIN
from tests.feature import FEATURES, create_complement, create_model, get_config
from tests.utils import assert_compilable_code, assert_executable_code


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_compilability(feature: str, tmp_path: Path) -> None:
    model, integration = create_model(feature)
    assert_compilable_code(model, integration, tmp_path)


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_executability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if not config.sequence:
        pytest.skip()
    model, integration = create_model(feature)
    create_complement(config, feature, tmp_path)
    assert assert_executable_code(model, integration, tmp_path, main=MAIN) == config.sequence


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_executability_with_external_io_buffers(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if not config.sequence:
        pytest.skip()
    model, integration = create_model(feature)
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
    assert assert_executable_code(model, integration, tmp_path, main=MAIN) == config.sequence
