from __future__ import annotations

import re

import pytest

from rflx import fatal_error
from rflx.fatal_error import (
    FatalErrorHandler,
    fatal_error_message,
)
from tests.const import GITHUB_TRACKER_REF_PATTERN, GNAT_TRACKER_REF_PATTERN


def test_fatal_error_handler() -> None:
    result = []

    def store(msg: str) -> None:
        result.append(msg)

    with FatalErrorHandler(store):
        pass

    with pytest.raises(SystemExit, match="^2$"), FatalErrorHandler(store):
        raise TypeError("Test")

    assert re.fullmatch(
        r"\n-* RecordFlux Bug -*.*Traceback.*TypeError: Test.*-*.*RecordFlux/issues.*",
        result[0],
        re.DOTALL,
    )


def test_fatal_error_handler_unsafe() -> None:
    result = []

    def store(msg: str) -> None:
        result.append(msg)

    with pytest.raises(SystemExit, match="^2$"), FatalErrorHandler(store, unsafe=True):
        raise TypeError("Test")

    assert re.fullmatch(
        r"^\n-*\nEXCEPTION IN UNSAFE MODE, PLEASE RERUN WITHOUT UNSAFE OPTIONS\n-*\n.*"
        r"\n-* RecordFlux Bug -*.*Traceback.*TypeError: Test.*-*.*RecordFlux/issues.*",
        result[0],
        re.DOTALL,
    )


@pytest.mark.parametrize(
    ("gnat_tracker_release", "expected"),
    [
        (False, GITHUB_TRACKER_REF_PATTERN),
        (True, GNAT_TRACKER_REF_PATTERN),
    ],
)
def test_fatal_error_message(
    gnat_tracker_release: bool,
    expected: str,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(fatal_error, "is_gnat_tracker_release", lambda: gnat_tracker_release)
    assert re.match(expected, fatal_error_message(unsafe=False), re.DOTALL)
