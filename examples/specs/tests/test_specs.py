import glob
import subprocess
import warnings
from pathlib import Path

import pytest
from rflx.pyrflx import PyRFLX

from tools.validate_spec import cli

DATA_PATH = Path("tests/data/")


@pytest.mark.parametrize("spec", glob.glob("*.rflx"))
def test_spec(spec: str, tmp_path: Path) -> None:
    subprocess.run(["rflx", "generate", "-d", tmp_path, spec], check=True)
    subprocess.run(["gprbuild", "-U"], check=True, cwd=tmp_path)


@pytest.mark.parametrize("spec", glob.glob("*.rflx"))
def test_validate_spec(spec: str) -> None:
    pyrflx = PyRFLX.from_specs([spec], True)

    for package in pyrflx:
        for message in package:
            test_data_dir = (
                DATA_PATH
                / str(message.identifier.parent).lower()
                / str(message.identifier.name).lower()
            )

            if not test_data_dir.is_dir():
                warnings.warn(f"No example data found for {message.identifier}")
                continue

            invalid_data_dir = test_data_dir / "invalid"
            valid_data_dir = test_data_dir / "valid"

            arguments = ["validate_spec", "-s", spec, "-m", str(message.identifier)]
            if invalid_data_dir.is_dir():
                arguments.extend(["-i", str(invalid_data_dir)])
            if valid_data_dir.is_dir():
                arguments.extend(["-v", str(valid_data_dir)])
            arguments.append("--no-verification")

            assert cli(arguments) == 0
