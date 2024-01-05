import logging
import sys
from pathlib import Path

import pytest

import rflx.specification.parser
from tests.const import SPEC_DIR
from tools import fuzz_driver


class UnexpectedError(Exception):
    pass


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
                "--state-dir",
                str(tmpdir),
                "--corpus-dir",
                str(SPEC_DIR),
                "--runs",
                "10",
            ],
        )

        with caplog.at_level(logging.INFO), pytest.raises(SystemExit, match="^0$"):
            fuzz_driver.main()
        assert "did 10 runs, stopping now." in caplog.text


def test_unexpected_error(
    monkeypatch: pytest.MonkeyPatch,
    tmpdir: Path,
    caplog: pytest.LogCaptureFixture,
) -> None:
    def raise_unexpected_exception() -> None:
        raise UnexpectedError("Unexpected error")

    with monkeypatch.context() as mp:
        mp.setattr(
            sys,
            "argv",
            [
                "fuzz_driver.py",
                "--state-dir",
                str(tmpdir),
                "--corpus-dir",
                str(SPEC_DIR),
                "--artifact-file",
                str(tmpdir / "crash"),
                "--runs",
                "10",
            ],
        )
        mp.setattr(
            rflx.specification.Parser,
            "parse_string",
            lambda _c, _s: raise_unexpected_exception(),
        )

        with caplog.at_level(logging.INFO), pytest.raises(SystemExit, match="^76$"):
            fuzz_driver.main()
        assert f'sample was written to {tmpdir/ "crash"}' in caplog.text


def test_decode_error(
    monkeypatch: pytest.MonkeyPatch,
    tmpdir: Path,
    caplog: pytest.LogCaptureFixture,
) -> None:
    def raise_decode_error() -> None:
        raise UnicodeDecodeError(
            "fakeenc",
            b"deafbeef",
            1,
            2,
            "Error",
        )

    with monkeypatch.context() as mp:
        mp.setattr(
            sys,
            "argv",
            [
                "fuzz_driver.py",
                "--state-dir",
                str(tmpdir),
                "--corpus-dir",
                str(SPEC_DIR),
                "--artifact-file",
                str(tmpdir / "crash"),
                "--runs",
                "10",
            ],
        )
        mp.setattr(
            rflx.specification.Parser,
            "parse_string",
            lambda _c, _s: raise_decode_error(),
        )

        with caplog.at_level(logging.INFO), pytest.raises(SystemExit, match="^0$"):
            fuzz_driver.main()
        assert "did 10 runs, stopping now." in caplog.text
