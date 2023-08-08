import sys

import pytest


def test_main(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(sys, "argv", [""])

    with pytest.raises(SystemExit, match=r"^2$"):
        from rflx import __main__  # noqa: F401


def test_main_version(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(sys, "argv", ["", "--version"])

    with pytest.raises(SystemExit, match=r"^0$"):
        from rflx import __main__  # noqa: F401
