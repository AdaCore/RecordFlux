from __future__ import annotations

import pickle
from collections.abc import Sequence
from pathlib import Path

import pytest

from rflx.identifier import ID
from rflx.rapidflux import FatalError, Location


def test_id_constructor() -> None:
    assert ID(["A"]) == ID("A")
    assert ID(["A", "B"]) == ID("A::B")
    assert ID(["A", "B", "C"]) == ID("A::B::C")
    assert ID(ID("A")) == ID("A")
    assert ID(ID("A::B")) == ID("A::B")
    assert ID(ID("A::B::C")) == ID("A::B::C")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_id_invalid_type() -> None:
    with pytest.raises(AssertionError, match=r'^unexpected identifier type "int"$'):
        ID(0)  # type: ignore[arg-type]


@pytest.mark.parametrize(
    "identifier",
    [
        [],
        "",
        "A::::B",
        "::A::B",
        "A::B::",
        "A::B C::D",
        "A::B:C::D",
    ],
)
def test_id_invalid(identifier: str | Sequence[str]) -> None:
    with pytest.raises(FatalError, match=r"^invalid identifier$"):
        ID(identifier)


@pytest.mark.parametrize(
    ("identifier", "expected"),
    [
        (ID(ID("A", Location((1, 2)))), ID("A", Location((1, 2)))),
        (ID(ID("A", Location((1, 2))), Location((3, 4))), ID("A", Location((3, 4)))),
    ],
)
def test_id_from_id(identifier: ID, expected: ID) -> None:
    assert identifier == expected
    assert identifier.location == expected.location


def test_id_eq() -> None:
    assert ID("A") == ID("a")


def test_id_hash() -> None:
    assert hash(ID("A")) == hash(ID("a"))


def test_id_str() -> None:
    assert str(ID("A::B::C")) == "A::B::C"


def test_id_contains() -> None:
    assert ID("A") in [ID("a")]
    assert None not in [ID("a")]


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        (ID("A", Location((1, 2))), ID("B"), ID("AB", Location((1, 2)))),
        (ID("A", Location((1, 2))), ID("B::C", Location((3, 4))), ID("AB::C", Location((1, 2)))),
        (ID("B::C"), ID("D", Location((1, 2))), ID("B::CD", Location((1, 2)))),
    ],
)
def test_id_add_id(left: ID, right: ID, expected: ID) -> None:
    result = left + right
    assert result == expected
    assert result.location == expected.location


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        ("A", ID("B::C", Location((1, 2))), ID("AB::C", Location((1, 2)))),
        (ID("B::C", Location((1, 2))), "D", ID("B::CD", Location((1, 2)))),
        (ID("B::C", Location((1, 2))), "", ID("B::C", Location((1, 2)))),
        ("", ID("B::C", Location((1, 2))), ID("B::C", Location((1, 2)))),
        ("A.B", ID("C", Location((1, 2))), ID("A::BC", Location((1, 2)))),
        (ID("A", Location((1, 2))), "B.C", ID("AB::C", Location((1, 2)))),
    ],
)
def test_id_add_str(left: ID, right: ID, expected: ID) -> None:
    result = left + right
    assert result == expected
    assert result.location == expected.location


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        (ID("A", Location((1, 2))), ID("B"), ID("A::B", Location((1, 2)))),
        (ID("A", Location((1, 2))), ID("B::C", Location((3, 4))), ID("A::B::C", Location((1, 2)))),
        (ID("B::C"), ID("D", Location((3, 4))), ID("B::C::D", Location((3, 4)))),
    ],
)
def test_id_mul_id(left: ID, right: ID, expected: ID) -> None:
    result = left * right
    assert result == expected
    assert result.location == expected.location


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        ("A", ID("B::C", Location((1, 2))), ID("A::B::C", Location((1, 2)))),
        (ID("B::C", Location((1, 2))), "D", ID("B::C::D", Location((1, 2)))),
        ("", ID("B::C", Location((1, 2))), ID("B::C", Location((1, 2)))),
        (ID("B::C", Location((1, 2))), "", ID("B::C", Location((1, 2)))),
        ("A.B", ID("C", Location((1, 2))), ID("A::B::C", Location((1, 2)))),
        (ID("A", Location((1, 2))), "B.C", ID("A::B::C", Location((1, 2)))),
    ],
)
def test_id_mul_str(left: ID, right: ID, expected: ID) -> None:
    result = left * right
    assert result == expected
    assert result.location == expected.location


def test_id_parts() -> None:
    assert ID("A::B").parts == ["A", "B"]


def test_id_location() -> None:
    assert ID("A::B").location is None
    assert ID("A::B", Location((1, 2))).location == Location((1, 2))


def test_id_name() -> None:
    assert ID("A::B::C").name == ID("C")


def test_id_parent() -> None:
    assert ID("A::B::C").parent == ID("A::B")


def test_id_parent_error() -> None:
    with pytest.raises(FatalError, match=r"^no parent$"):
        ID("A").parent  # noqa: B018


def test_id_flat() -> None:
    assert ID("A::B::C").flat == "A_B_C"


def test_id_sorted() -> None:
    assert sorted([ID("B"), ID("A")]) == [ID("A"), ID("B")]


def test_id_pickle(tmp_path: Path) -> None:
    pickle_file = tmp_path / "pickle"
    identifier = ID("A::B")

    with pickle_file.open("w+b") as f:
        pickle.dump(identifier, f)
        f.flush()

    with pickle_file.open("rb") as f:
        loaded = pickle.load(f)  # noqa: S301
        assert loaded == identifier
