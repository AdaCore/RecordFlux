from pathlib import Path

from rflx import expression as expr, model
from rflx.specification import cache
from tests.data.models import TLV_MESSAGE


def test_init(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path / "Test"
    cache.Cache()
    assert not (tmp_path / "Test").exists()


def test_init_valid(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    with open(tmp_path / cache.VERIFICATION_FILE, "x") as f:
        f.write("{}")
    cache.Cache()


def test_init_invalid(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    with open(tmp_path / cache.VERIFICATION_FILE, "x") as f:
        f.write("invalid")
    cache.Cache()


def test_verified(tmp_path: Path) -> None:
    m1 = model.Message(
        "P::M",
        [
            model.Link(model.INITIAL, model.Field("A")),
            model.Link(model.Field("A"), model.FINAL),
        ],
        {model.Field("A"): model.ModularInteger("P::T", expr.Pow(expr.Number(2), expr.Number(8)))},
    )
    m2 = model.Message(
        "P::M",
        [
            model.Link(model.INITIAL, model.Field("B")),
            model.Link(model.Field("B"), model.FINAL),
        ],
        {model.Field("B"): model.ModularInteger("P::T", expr.Pow(expr.Number(2), expr.Number(8)))},
    )
    m3 = model.Message(
        "P::M",
        [
            model.Link(model.INITIAL, model.Field("A")),
            model.Link(model.Field("A"), model.FINAL),
        ],
        {model.Field("A"): model.ModularInteger("P::T", expr.Pow(expr.Number(2), expr.Number(16)))},
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
    assert not c.is_verified(m1)
    assert c.is_verified(m2)
    assert not c.is_verified(m3)
    c.add_verified(m3)
    assert not c.is_verified(m1)
    assert not c.is_verified(m2)
    assert c.is_verified(m3)


def test_verified_disabled(tmp_path: Path) -> None:
    cache.CACHE_DIR = tmp_path
    c = cache.Cache(enabled=False)
    assert not c.is_verified(TLV_MESSAGE)
    c.add_verified(TLV_MESSAGE)
    assert not c.is_verified(TLV_MESSAGE)
