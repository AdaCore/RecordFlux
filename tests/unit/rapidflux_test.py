import pickle
from functools import partial
from pathlib import Path
from tempfile import NamedTemporaryFile
from typing import Protocol

import pytest

from rflx.rapidflux import (
    Annotation,
    ErrorEntry,
    Location,
    RecordFluxError,
    Severity,
    logging,
    source_code,
)
from tests.utils import assert_stderr_regex


class LoggingProtocol(Protocol):
    def __call__(self, format_str: str, **args: object) -> None: ...  # pragma: no cover


class NotExecuted:
    def __str__(self) -> str:
        raise RuntimeError("__str__ method called")


@pytest.mark.parametrize(
    "obj",
    [
        Severity.ERROR,
        Annotation("This is wrong", Severity.ERROR, Location((1, 1))),
        ErrorEntry(
            "Some error",
            Severity.ERROR,
            Location((1, 1)),
            annotations=[
                Annotation("This is wrong", Severity.ERROR, Location((1, 1))),
            ],
        ),
        RecordFluxError(
            [
                ErrorEntry(
                    "Some error",
                    Severity.ERROR,
                    Location((1, 1)),
                    annotations=[
                        Annotation("This is wrong", Severity.ERROR, Location((1, 1))),
                    ],
                ),
            ],
        ),
    ],
)
def test_pickle(obj: object) -> None:
    with NamedTemporaryFile("w+b") as f:
        pickle.dump(obj, f)
        f.flush()

        with Path(f.name).open("rb") as read_file:
            loaded = pickle.load(read_file)  # noqa: S301
            assert loaded == obj


def test_location() -> None:
    l = Location((1, 2), Path("foo"), (3, 4))
    assert l.start == (1, 2)
    assert l.source == Path("foo")
    assert l.end == (3, 4)

    l = Location((1, 2))
    assert l.start == (1, 2)
    assert l.source is None
    assert l.end is None


def test_location_repr() -> None:
    assert repr(Location((1, 2))) == "Location((1, 2), None, None)"
    assert repr(Location((1, 2), None, (3, 4))) == "Location((1, 2), None, (3, 4))"
    assert repr(Location((1, 2), Path("foo"), (3, 4))) == 'Location((1, 2), "foo", (3, 4))'


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


def test_source_code_register_and_retrieve() -> None:
    path = Path("some/path")
    source_code_str = "Foo bar"
    source_code.register(path, source_code_str)
    assert source_code.retrieve(path) == source_code_str


def test_source_code_register_and_retrieve_multiple_files() -> None:
    sources = {
        Path("foo.rflx"): "foo",
        Path("bar.rflx"): "bar",
        Path("baz.rflx"): "baz",
    }

    for path, source_string in sources.items():
        source_code.register(path, source_string)

    for path, source_string in sources.items():
        assert source_code.retrieve(path) == source_string


def test_source_code_retrieve_non_existent() -> None:
    assert source_code.retrieve(Path("non_existent.rflx")) is None
