from pathlib import Path

import pytest

from rflx.generator.common import external_io_buffers
from rflx.identifier import ID
from rflx.integration import IntegrationFile, StateMachineIntegration
from tests.const import MAIN, STATE_MACHINE_NAME
from tests.feature import FEATURES, create_complement, create_model, get_config
from tests.utils import assert_provable_code


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.proof is None:
        pytest.skip()
    model, integration = create_model(feature)
    units = []
    if model.state_machines:
        assert len(model.state_machines) == 1
        assert model.state_machines[0].identifier == ID(f"Test::{STATE_MACHINE_NAME}")
        units = [
            f"rflx.test.{STATE_MACHINE_NAME.lower()}",
            f"rflx.test.{STATE_MACHINE_NAME.lower()}.fsm",
            *config.proof.units,
        ]
        units.extend(create_complement(config, feature, tmp_path))
        main = MAIN
    else:
        main = None
    assert_provable_code(
        model,
        integration,
        tmp_path,
        main=main,
        timeout=config.proof.timeout,
        memlimit=config.proof.memlimit,
        units=[*units, *config.proof.units],
    )


@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability_with_external_io_buffers(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.proof is None:
        pytest.skip()
    model, integration = create_model(feature)
    if not model.state_machines:
        pytest.skip()
    if integration._packages:  # noqa: SLF001
        integration._packages["test"].state_machine[  # noqa: SLF001
            STATE_MACHINE_NAME
        ].external_io_buffers = True
    else:
        integration.add_integration_file(
            "test",
            IntegrationFile(
                Machine={
                    STATE_MACHINE_NAME: StateMachineIntegration(
                        Buffer_Size=None,
                        External_IO_Buffers=True,
                    ),
                },
            ),
        )
    config.external_io_buffers = len(external_io_buffers(model.state_machines[0].to_ir()))
    create_complement(config, feature, tmp_path)
    assert_provable_code(
        model,
        integration,
        tmp_path,
        main=MAIN,
        timeout=config.proof.timeout,
        memlimit=config.proof.memlimit,
        units=[
            "main",
            "lib",
            f"rflx.test.{STATE_MACHINE_NAME.lower()}",
            f"rflx.test.{STATE_MACHINE_NAME.lower()}.fsm",
            *config.proof.units,
        ],
    )
