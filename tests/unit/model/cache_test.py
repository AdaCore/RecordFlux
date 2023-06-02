from pathlib import Path

import pytest
from pytest import CaptureFixture

from rflx import expression as expr, model
from rflx.model import cache
from tests.data.models import TLV_MESSAGE


def test_init(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path / "Test"
    cache.Cache()
    assert not (tmp_path / "Test").exists()


def test_init_valid(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    (tmp_path / cache.VERIFICATION_FILE).write_text("{}")
    cache.Cache()


@pytest.mark.parametrize("content", ["invalid", "[]", "{A: B}"])
def test_init_invalid(content: str, tmp_path: Path, capsys: CaptureFixture[str]) -> None:
    cache.CACHE_DIR = tmp_path
    (tmp_path / cache.VERIFICATION_FILE).write_text(content)
    cache.Cache()
    captured = capsys.readouterr()
    assert "verification cache will be ignored due to invalid format" in captured.out


def test_verified(tmp_path: Path) -> None:
    m1 = model.Message(
        "P::M",
        [
            model.Link(model.INITIAL, model.Field("A")),
            model.Link(model.Field("A"), model.FINAL),
        ],
        {
            model.Field("A"): model.Integer(
                "P::T",
                expr.Number(0),
                expr.Sub(expr.Pow(expr.Number(2), expr.Number(8)), expr.Number(1)),
                expr.Number(8),
            )
        },
    )
    m2 = model.Message(
        "P::M",
        [
            model.Link(model.INITIAL, model.Field("B")),
            model.Link(model.Field("B"), model.FINAL),
        ],
        {
            model.Field("B"): model.Integer(
                "P::T",
                expr.Number(0),
                expr.Sub(expr.Pow(expr.Number(2), expr.Number(8)), expr.Number(1)),
                expr.Number(8),
            )
        },
    )
    m3 = model.Message(
        "P::M",
        [
            model.Link(model.INITIAL, model.Field("A")),
            model.Link(model.Field("A"), model.FINAL),
        ],
        {
            model.Field("A"): model.Integer(
                "P::T",
                expr.Number(0),
                expr.Sub(expr.Pow(expr.Number(2), expr.Number(16)), expr.Number(1)),
                expr.Number(16),
            )
        },
    )
    cache.CACHE_DIR = tmp_path
    c = cache.Cache()
    assert not c.is_verified(m1)
    assert not c.is_verified(m2)
    assert not c.is_verified(m3)
    c.add_verified(m1)
    assert c.is_verified(m1)
    assert not c.is_verified(m2)
    assert not c.is_verified(m3)
    c.add_verified(m2)
    assert c.is_verified(m1)
    assert c.is_verified(m2)
    assert not c.is_verified(m3)
    c.add_verified(m3)
    assert c.is_verified(m1)
    assert c.is_verified(m2)
    assert c.is_verified(m3)
    c.add_verified(m1)
    assert c.is_verified(m1)
    assert c.is_verified(m2)
    assert c.is_verified(m3)


def test_verified_disabled(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache(enabled=False)
    assert not c.is_verified(TLV_MESSAGE)
    c.add_verified(TLV_MESSAGE)
    assert not c.is_verified(TLV_MESSAGE)
