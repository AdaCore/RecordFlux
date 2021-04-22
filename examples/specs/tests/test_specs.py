import glob
import subprocess
import warnings
from pathlib import Path

import pytest
from rflx.pyrflx import PyRFLX

from tools.validate_spec import validate

DATA_PATH = Path("tests/data/")


@pytest.mark.parametrize("spec", glob.glob("*.rflx"))
def test_spec(spec: str, tmp_path: Path) -> None:
    subprocess.run(["rflx", "generate", "-d", tmp_path, spec], check=True)
    subprocess.run(["gprbuild", "-U"], check=True, cwd=tmp_path)


@pytest.mark.parametrize("spec", glob.glob("*.rflx"))
def test_validate_spec(spec: str) -> None:
    pyrflx = PyRFLX.from_specs([spec], True)

    for package in pyrflx:
        for message_value in package:
            test_data_dir = (
                DATA_PATH
                / str(message_value.identifier.parent).lower()
                / str(message_value.identifier.name).lower()
            )

            if not test_data_dir.is_dir():
                warnings.warn(f"No example data found for {message_value.identifier}")
                continue

            directory_invalid = test_data_dir / "invalid"
            directory_valid = test_data_dir / "valid"

            assert (
                validate(
                    message_value,
                    directory_invalid,
                    directory_valid,
                    json_output=None,
                    abort_on_error=False,
                    coverage_info=None,
                )
                == 0
            )
