import glob
import pathlib
import subprocess

import pytest


@pytest.mark.parametrize("spec", glob.glob("*.rflx"))
def test_spec(spec: str, tmp_path: pathlib.Path) -> None:
    subprocess.run(["rflx", "generate", "-d", tmp_path, spec], check=True)
    subprocess.run(["gprbuild", "-U"], check=True, cwd=tmp_path)
