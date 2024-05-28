from functools import partial
from typing import Protocol

import pytest

from rflx.rapidflux import logging
from tests.utils import assert_stderr_regex


class LoggingProtocol(Protocol):
    def __call__(self, format_str: str, **args: object) -> None: ...  # pragma: no cover


class NotExecuted:
    def __str__(self) -> str:
        raise RuntimeError("__str__ method called")


@pytest.mark.parametrize(
    ("log_fn", "input_str", "expected_regex"),
    [
        (logging.info, "some info", "info: some info\n"),
        (logging.help, "some help", "help: some help\n"),
        (logging.note, "some note", "note: some note\n"),
        (logging.warning, "some warning", "warning: some warning\n"),
        (logging.error, "some error", "error: some error\n"),
        (partial(logging.info, value="foo"), "some info {value}", "info: some info foo\n"),
        (partial(logging.help, value="bar"), "some help {value}", "help: some help bar\n"),
        (partial(logging.note, value="baz"), "some note {value}", "note: some note baz\n"),
        (
            partial(logging.warning, value="foo"),
            "some warning {value}",
            "warning: some warning foo\n",
        ),
        (partial(logging.error, value="bar"), "some error {value}", "error: some error bar\n"),
    ],
)
def test_logging(
    capfd: pytest.CaptureFixture[str],
    log_fn: LoggingProtocol,
    input_str: str,
    expected_regex: str,
) -> None:
    logging.set_quiet(False)
    log_fn(input_str)
    assert_stderr_regex(f"^{expected_regex}$", capfd)


def test_logging_with_placeholder_quiet(capfd: pytest.CaptureFixture[str]) -> None:
    logging.set_quiet(True)
    logging.error("This should never be printed: {value}", value=NotExecuted())
    assert_stderr_regex(r"^$", capfd)


def test_logging_with_placeholder_verbose() -> None:
    logging.set_quiet(False)
    with pytest.raises(RuntimeError, match=r"^__str__ method called$"):
        logging.error("This should never be printed: {value}", value=NotExecuted())


def test_logging_without_placeholder_quiet(capfd: pytest.CaptureFixture[str]) -> None:
    logging.set_quiet(True)
    logging.error("This should never be printed", value=NotExecuted())
    assert_stderr_regex(r"^$", capfd)


def test_logging_without_placeholder_verbose(capfd: pytest.CaptureFixture[str]) -> None:
    logging.set_quiet(False)
    logging.error("This should be printed", value=NotExecuted())
    assert_stderr_regex("^error: This should be printed\n$", capfd)


@pytest.mark.parametrize(
    ("quiet", "expected_regex"),
    [
        (True, ""),
        (False, "error: foo\n"),
    ],
)
def test_logging_quiet(
    capfd: pytest.CaptureFixture[str],
    quiet: bool,
    expected_regex: str,
) -> None:
    logging.set_quiet(quiet)
    logging.error("foo")
    assert_stderr_regex(f"^{expected_regex}$", capfd)
