import re
from pathlib import Path

import pytest

from rflx import expr, model
from rflx.identifier import ID
from rflx.model import cache
from rflx.rapidflux import Location, RecordFluxError
from tests.data import models
from tests.utils import assert_stderr_regex


def test_init(tmp_path: Path) -> None:
    file = tmp_path / "test.json"
    cache.Cache(file)
    assert not file.exists()


def test_init_valid(tmp_path: Path) -> None:
    file = tmp_path / "test.json"
    file.write_text("{}")
    cache.Cache(file)


@pytest.mark.parametrize("content", ["invalid", "[]", "{A: B}"])
def test_init_invalid(content: str, tmp_path: Path, capfd: pytest.CaptureFixture[str]) -> None:
    file = tmp_path / "test.json"
    file.write_text(content)
    cache.Cache(file)
    assert_stderr_regex(
        r"^"
        r"warning: verification cache will be ignored due to invalid format:\n"
        rf"{re.escape(content)}"
        r"$",
        capfd,
    )


def test_verified(tmp_path: Path) -> None:
    m1 = model.Message(
        ID("P::M", Location((1, 1))),
        [
            model.Link(model.INITIAL, model.Field("A"), location=Location((1, 1))),
            model.Link(model.Field("A"), model.FINAL, location=Location((2, 2))),
        ],
        {
            model.Field(ID("A", location=Location((1, 1)))): model.Integer(
                "P::T",
                expr.Number(0),
                expr.Sub(expr.Pow(expr.Number(2), expr.Number(8)), expr.Number(1)),
                expr.Number(8),
            ),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    d1 = cache.Digest(m1)
    m2 = model.Message(
        ID("P::M", Location((1, 1))),
        [
            model.Link(model.INITIAL, model.Field("B"), location=Location((1, 1))),
            model.Link(model.Field("B"), model.FINAL, location=Location((2, 2))),
        ],
        {
            model.Field(ID("B", location=Location((1, 1)))): model.Integer(
                "P::T",
                expr.Number(0),
                expr.Sub(expr.Pow(expr.Number(2), expr.Number(8)), expr.Number(1)),
                expr.Number(8),
            ),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    d2 = cache.Digest(m2)
    m3 = model.Message(
        ID("P::M", Location((1, 1))),
        [
            model.Link(model.INITIAL, model.Field("A"), location=Location((1, 1))),
            model.Link(model.Field("A"), model.FINAL, location=Location((2, 2))),
        ],
        {
            model.Field(ID("A", location=Location((1, 1)))): model.Integer(
                "P::T",
                expr.Number(0),
                expr.Sub(expr.Pow(expr.Number(2), expr.Number(16)), expr.Number(1)),
                expr.Number(16),
            ),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    d3 = cache.Digest(m3)
    c = cache.Cache(tmp_path / "test.json")
    assert not c.is_verified(d1)
    assert not c.is_verified(d2)
    assert not c.is_verified(d3)
    c.add_verified(d1)
    assert c.is_verified(d1)
    assert not c.is_verified(d2)
    assert not c.is_verified(d3)
    c.add_verified(d2)
    assert c.is_verified(d1)
    assert c.is_verified(d2)
    assert not c.is_verified(d3)
    c.add_verified(d3)
    assert c.is_verified(d1)
    assert c.is_verified(d2)
    assert c.is_verified(d3)
    c.add_verified(d1)
    assert c.is_verified(d1)
    assert c.is_verified(d2)
    assert c.is_verified(d3)


def test_always_verify() -> None:
    d = cache.Digest(models.tlv_message())
    c = cache.AlwaysVerify()
    assert not c.is_verified(d)
    c.add_verified(d)
    assert not c.is_verified(d)


def test_never_verify() -> None:
    d = cache.Digest(models.tlv_message())
    c = cache.NeverVerify()
    assert c.is_verified(d)
    c.add_verified(d)
    assert c.is_verified(d)


@pytest.mark.timeout(2)
@pytest.mark.parametrize("load", [True, False])
def test_cache_timeout(tmp_path: Path, monkeypatch: pytest.MonkeyPatch, load: bool) -> None:
    cache_path = tmp_path / "cache_path.json"
    lock_file = cache_path.with_suffix(".lock")
    lock_file.write_text("42")
    monkeypatch.setattr(cache.FileLock, "LOCK_TIMEOUT", 0.1)
    expected_regex = (
        "^error: failed to acquire cache lock after 0.1 seconds\n"
        'note: the cache is locked by a process with a PID of "42"\n'
        "help: if the process that owns the lock isn't active anymore, deleting "
        f'"{lock_file}" will solve this issue$'
    )

    def _patched_cache_init(self: cache.Cache, file: Path) -> None:
        self._file = file
        self._verified = {}

    # Prevent the constructor to call _load_cache itself.
    monkeypatch.setattr(cache.Cache, "__init__", _patched_cache_init)
    c = cache.Cache(cache_path)

    if load:
        with pytest.raises(RecordFluxError, match=expected_regex):
            c._load_cache()  # noqa: SLF001
    else:
        with pytest.raises(RecordFluxError, match=expected_regex):
            c._write_cache()  # noqa: SLF001
