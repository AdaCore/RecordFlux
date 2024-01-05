from pathlib import Path

import pytest

from rflx import expression as expr, model
from rflx.model import cache
from tests.data import models


def test_init(tmp_path: Path) -> None:
    file = tmp_path / "test.json"
    cache.Cache(file)
    assert not file.exists()


def test_init_valid(tmp_path: Path) -> None:
    file = tmp_path / "test.json"
    file.write_text("{}")
    cache.Cache(file)


@pytest.mark.parametrize("content", ["invalid", "[]", "{A: B}"])
def test_init_invalid(content: str, tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    file = tmp_path / "test.json"
    file.write_text(content)
    cache.Cache(file)
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
            ),
        },
    )
    d1 = cache.Digest(m1)
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
            ),
        },
    )
    d2 = cache.Digest(m2)
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
            ),
        },
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
