import logging
import sys
from pathlib import Path

import pytest

from tests.const import SPEC_DIR
from tools import fuzz_driver


def test_no_bug(
    monkeypatch: pytest.MonkeyPatch,
    tmpdir: Path,
    caplog: pytest.LogCaptureFixture,
) -> None:
    with monkeypatch.context() as mp:
        mp.setattr(
            sys,
            "argv",
            [
                "fuzz_driver.py",
                "--crash-dir",
                str(tmpdir),
                "--num-workers",
                "2",
                "--max-runs",
                "100",
                str(SPEC_DIR),
            ],
        )

        with caplog.at_level(logging.INFO), pytest.raises(SystemExit, match="^0$"):
            fuzz_driver.fuzz()
        assert "Performed 100 runs" in caplog.text
