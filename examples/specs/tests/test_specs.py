import glob
import re
import subprocess
import warnings
from pathlib import Path
from xml.etree import ElementTree

import pytest
from rflx.pyrflx import PyRFLX

import tools.iana_to_rflx
from tools.validate_spec import validate

DATA_PATH = Path("tests/data/")


@pytest.mark.parametrize("spec", glob.glob("*.rflx"))
def test_spec(spec: str, tmp_path: Path) -> None:
    subprocess.run(["rflx", "generate", "-d", tmp_path, spec], check=True)
    subprocess.run(["gprbuild", "-U"], check=True, cwd=tmp_path)


@pytest.mark.parametrize("registry_file_name", glob.glob("iana_registries/*.xml"))
def test_iana_specs_synchronized(registry_file_name: str) -> None:
    with open(registry_file_name, "r") as registry_file, open(
        f"{Path(registry_file_name).stem}.rflx".replace("-", "_"), "r"
    ) as generated_spec:
        registry = ElementTree.fromstring(registry_file.read())
        registry_last_updated = registry.find("iana:updated", tools.iana_to_rflx.NAMESPACE)
        assert registry_last_updated is not None
        assert (
            re.search(
                f"Registry last updated on {registry_last_updated.text}", generated_spec.read()
            )
            is not None
        )


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

            validate(
                message_value.identifier,
                pyrflx,
                directory_invalid,
                directory_valid,
                checksum_functions=None,
                json_output=None,
                abort_on_error=False,
                coverage=True,
            )
