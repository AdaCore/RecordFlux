import pickle
from pathlib import Path

import pytest

from rflx.rapidflux import ID
from rflx.rapidflux.ty import (
    Aggregate,
    Any,
    Bounds,
    Builtins,
    Channel,
    Enumeration,
    Integer,
    Message,
    Refinement,
    Structure,
    Undefined,
)

INTEGER_A = Integer("A", Bounds(10, 100))
ENUMERATION_A = Enumeration("A", [ID("AE1"), ID("AE2")])
ENUMERATION_B = Enumeration("B", [ID("BE1"), ID("BE2"), ID("BE3")])


@pytest.mark.parametrize(
    "type_",
    [
        Undefined(),
        Any(),
        Builtins.BOOLEAN,
        Builtins.UNIVERSAL_INTEGER,
        Builtins.BASE_INTEGER,
        Aggregate(INTEGER_A),
        Builtins.OPAQUE,
        Structure(
            "A",
            {("F",)},
            {ID("P"): Builtins.BOOLEAN},
            {ID("F"): Builtins.OPAQUE},
        ),
        Message(
            "A",
            {("F",)},
            {ID("P"): Builtins.BOOLEAN},
            {ID("F"): Builtins.OPAQUE},
            [Refinement("F", Message("B"), "P")],
            is_definite=True,
        ),
        Channel(readable=True, writable=True),
    ],
)
def test_type_pickle(type_: object, tmp_path: Path) -> None:
    pickle_file = tmp_path / "pickle"

    with pickle_file.open("w+b") as f:
        pickle.dump(type_, f)
        f.flush()

    with pickle_file.open("rb") as f:
        loaded = pickle.load(f)  # noqa: S301
        assert loaded == type_


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


@pytest.mark.parametrize(
    ("bound", "expected"),
    [
        (Bounds(1, 1), "1 .. 1"),
        (Bounds(1, 100), "1 .. 100"),
        (Bounds(1, 2**64 - 1), "1 .. 2**64 - 1"),
        (Bounds(2**32 - 1, 2**64 - 1), "2**32 - 1 .. 2**64 - 1"),
    ],
)
def test_bounds_str(bound: Bounds, expected: str) -> None:
    assert str(bound) == expected


def test_bounds_pickle(tmp_path: Path) -> None:
    pickle_file = tmp_path / "pickle"
    bounds = Bounds(1, 2)

    with pickle_file.open("w+b") as f:
        pickle.dump(bounds, f)
        f.flush()

    with pickle_file.open("rb") as f:
        loaded = pickle.load(f)  # noqa: S301
        assert loaded == bounds
