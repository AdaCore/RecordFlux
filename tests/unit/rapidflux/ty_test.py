import pickle
from pathlib import Path

import pytest

from rflx.rapidflux.ty import Bounds


def test_bounds_contains() -> None:
    assert 1 in Bounds(1, 100)
    assert 0 not in Bounds(1, 100)
    assert Bounds(1, 100) in Bounds(1, 100)
    assert Bounds(1, 10) in Bounds(1, 100)
    assert Bounds(10, 100) in Bounds(1, 100)
    assert Bounds(10, 10) in Bounds(1, 100)
    assert Bounds(1, 200) not in Bounds(1, 100)
    assert Bounds(0, 100) not in Bounds(1, 100)


def test_bounds_contains_invalid() -> None:
    with pytest.raises(
        TypeError,
        match=r'^unsupported type "str" for testing membership in "Bounds"$',
    ):
        assert "invalid" not in Bounds(1, 100)  # type: ignore[operator]


def test_bounds_error() -> None:
    with pytest.raises(BaseException, match=r"^assertion failed: lower <= upper$"):
        Bounds(1, 0)


def test_bounds_str() -> None:
    assert str(Bounds(1, 1)) == "1 .. 1"
    assert str(Bounds(1, 100)) == "1 .. 100"


def test_bounds_pickle(tmp_path: Path) -> None:
    pickle_file = tmp_path / "pickle"
    bounds = Bounds(1, 2)

    with pickle_file.open("w+b") as f:
        pickle.dump(bounds, f)
        f.flush()

    with pickle_file.open("rb") as f:
        loaded = pickle.load(f)  # noqa: S301
        assert loaded == bounds
