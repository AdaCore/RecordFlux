from __future__ import annotations

import pickle
from collections import abc
from pathlib import Path

import pytest

from rflx.rapidflux import ID, Location, RecordFluxError
from rflx.rapidflux.ty import (
    Aggregate,
    Any,
    AnyInteger,
    Bounds,
    Builtins,
    Channel,
    Composite,
    Compound,
    Enumeration,
    Integer,
    Message,
    Refinement,
    Sequence,
    Structure,
    Type,
    Undefined,
    UniversalInteger,
    check_type,
    check_type_instance,
    common_type,
)
from rflx.ty import BASE_INTEGER, UNDEFINED

INT_A = Integer("A", Bounds(10, 100))
ENUM_A = Enumeration("A", [ID("AE1"), ID("AE2")])
ENUM_B = Enumeration("B", [ID("BE1"), ID("BE2"), ID("BE3")])
SEQ_A = Sequence("A", INT_A)
MSG_A = Message("A")


@pytest.mark.parametrize(
    ("enumeration", "other", "expected"),
    [
        (ENUM_A, Any(), ENUM_A),
        (ENUM_A, ENUM_A, ENUM_A),
        (ENUM_A, Undefined(), Undefined()),
        (ENUM_A, ENUM_B, Undefined()),
        (ENUM_A, Integer("A", Bounds(10, 100)), Undefined()),
    ],
)
def test_enumeration_common_type(enumeration: Type, other: Type, expected: Type) -> None:
    assert enumeration.common_type(other) == expected
    assert other.common_type(enumeration) == expected


@pytest.mark.parametrize(
    ("enumeration", "other", "expected"),
    [
        (ENUM_A, Any(), True),
        (ENUM_A, ENUM_A, True),
        (ENUM_A, Undefined(), False),
        (ENUM_A, ENUM_B, False),
        (ENUM_A, Integer("A", Bounds(10, 100)), False),
    ],
)
def test_enumeration_is_compatible(enumeration: Type, other: Type, expected: bool) -> None:
    assert enumeration.is_compatible(other) == expected
    assert other.is_compatible(enumeration) == expected


@pytest.mark.parametrize(
    ("base_integer", "other", "expected"),
    [
        (BASE_INTEGER, Any(), BASE_INTEGER),
        (BASE_INTEGER, BASE_INTEGER, BASE_INTEGER),
        (
            BASE_INTEGER,
            Integer("A", Bounds(10, 100)),
            BASE_INTEGER,
        ),
        (
            BASE_INTEGER,
            UniversalInteger(Bounds(10, 100)),
            BASE_INTEGER,
        ),
        (BASE_INTEGER, Undefined(), Undefined()),
        (BASE_INTEGER, ENUM_B, Undefined()),
    ],
)
def test_base_integer_common_type(base_integer: Type, other: Type, expected: Type) -> None:
    assert base_integer.common_type(other) == expected
    assert other.common_type(base_integer) == expected


@pytest.mark.parametrize(
    ("base_integer", "other", "expected"),
    [
        (BASE_INTEGER, Any(), True),
        (BASE_INTEGER, BASE_INTEGER, True),
        (
            BASE_INTEGER,
            Integer("A", Bounds(10, 100)),
            True,
        ),
        (
            BASE_INTEGER,
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (BASE_INTEGER, Undefined(), False),
        (BASE_INTEGER, ENUM_B, False),
    ],
)
def test_base_integer_is_compatible(base_integer: Type, other: Type, expected: bool) -> None:
    assert base_integer.is_compatible(other) == expected
    assert other.is_compatible(base_integer) == expected


@pytest.mark.parametrize(
    ("universal_integer", "other", "expected"),
    [
        (
            UniversalInteger(Bounds(10, 100)),
            Any(),
            UniversalInteger(Bounds(10, 100)),
        ),
        (
            UniversalInteger(Bounds(10, 100)),
            BASE_INTEGER,
            BASE_INTEGER,
        ),
        (
            UniversalInteger(Bounds(10, 100)),
            UniversalInteger(Bounds(10, 100)),
            UniversalInteger(Bounds(10, 100)),
        ),
        (
            UniversalInteger(Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
            BASE_INTEGER,
        ),
        (
            UniversalInteger(Bounds(20, 80)),
            Integer("A", Bounds(10, 100)),
            BASE_INTEGER,
        ),
        (
            UniversalInteger(Bounds(10, 100)),
            Undefined(),
            Undefined(),
        ),
        (
            UniversalInteger(Bounds(10, 100)),
            ENUM_B,
            Undefined(),
        ),
    ],
)
def test_universal_integer_common_type(
    universal_integer: Type,
    other: Type,
    expected: Type,
) -> None:
    assert universal_integer.common_type(other) == expected
    assert other.common_type(universal_integer) == expected


@pytest.mark.parametrize(
    ("universal_integer", "other", "expected"),
    [
        (UniversalInteger(Bounds(10, 100)), Any(), True),
        (UniversalInteger(Bounds(10, 100)), BASE_INTEGER, True),
        (UniversalInteger(Bounds(10, 100)), UniversalInteger(Bounds(10, 100)), True),
        (
            UniversalInteger(Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
            True,
        ),
        (UniversalInteger(Bounds(10, 100)), Undefined(), False),
        (UniversalInteger(Bounds(10, 100)), ENUM_B, False),
    ],
)
def test_universal_integer_is_compatible(
    universal_integer: Type,
    other: Type,
    expected: bool,
) -> None:
    assert universal_integer.is_compatible(other) == expected
    assert other.is_compatible(universal_integer) == expected


@pytest.mark.parametrize(
    ("integer", "other", "expected"),
    [
        (
            Integer("A", Bounds(10, 100)),
            Any(),
            Integer("A", Bounds(10, 100)),
        ),
        (
            Integer("A", Bounds(10, 100)),
            BASE_INTEGER,
            BASE_INTEGER,
        ),
        (
            Integer("A", Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
            BASE_INTEGER,
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(10, 100)),
            BASE_INTEGER,
        ),
        (
            Integer("A", Bounds(10, 100)),
            Integer("B", Bounds(10, 100)),
            BASE_INTEGER,
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(0, 200)),
            BASE_INTEGER,
        ),
        (
            Integer("A", Bounds(10, 100)),
            Undefined(),
            Undefined(),
        ),
        (
            Integer("A", Bounds(10, 100)),
            ENUM_B,
            Undefined(),
        ),
    ],
)
def test_integer_common_type(integer: Type, other: Type, expected: Type) -> None:
    assert integer.common_type(other) == expected
    assert other.common_type(integer) == expected


@pytest.mark.parametrize(
    ("integer", "other", "expected"),
    [
        (Integer("A", Bounds(10, 100)), Any(), True),
        (Integer("A", Bounds(10, 100)), BASE_INTEGER, True),
        (Integer("A", Bounds(10, 100)), Integer("A", Bounds(10, 100)), True),
        (Integer("A", Bounds(10, 100)), UniversalInteger(Bounds(10, 100)), True),
        (
            Integer("A", Bounds(10, 100)),
            Integer("B", Bounds(10, 100)),
            True,
        ),
        (
            Integer("A", Bounds(0, 200)),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(0, 200)),
            True,
        ),
        (Integer("A", Bounds(10, 100)), Undefined(), False),
        (Integer("A", Bounds(10, 100)), ENUM_B, False),
    ],
)
def test_integer_is_compatible(integer: Type, other: Type, expected: bool) -> None:
    assert integer.is_compatible(other) == expected
    assert other.is_compatible(integer) == expected


@pytest.mark.parametrize(
    ("integer", "other", "expected"),
    [
        (Integer("A", Bounds(10, 100)), Any(), True),
        (Integer("A", Bounds(10, 100)), BASE_INTEGER, False),
        (Integer("A", Bounds(10, 100)), Integer("A", Bounds(10, 100)), True),
        (Integer("A", Bounds(10, 100)), UniversalInteger(Bounds(10, 100)), True),
        (
            Integer("A", Bounds(10, 100)),
            Integer("B", Bounds(10, 100)),
            False,
        ),
        (
            Integer("A", Bounds(0, 200)),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(0, 200)),
            False,
        ),
        (Integer("A", Bounds(10, 100)), Undefined(), False),
        (Integer("A", Bounds(10, 100)), ENUM_B, False),
    ],
)
def test_integer_is_compatible_strong(integer: Type, other: Type, expected: bool) -> None:
    assert integer.is_compatible_strong(other) == expected
    assert other.is_compatible_strong(integer) == expected


@pytest.mark.parametrize(
    ("aggregate", "other", "expected"),
    [
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Any(),
            Aggregate(Integer("A", Bounds(10, 100))),
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("A", Bounds(10, 100))),
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(10, 100))),
            Aggregate(BASE_INTEGER),
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(20, 200))),
            Aggregate(BASE_INTEGER),
        ),
        (
            Aggregate(UniversalInteger(Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            Aggregate(UniversalInteger(Bounds(10, 200))),
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Undefined(),
            Undefined(),
        ),
    ],
)
def test_aggregate_common_type(aggregate: Type, other: Type, expected: Type) -> None:
    assert aggregate.common_type(other) == expected
    assert other.common_type(aggregate) == expected


@pytest.mark.parametrize(
    ("aggregate", "other", "expected"),
    [
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Any(),
            True,
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("A", Bounds(10, 100))),
            True,
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(10, 100))),
            True,
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("A", Bounds(20, 200))),
            True,
        ),
        (
            Aggregate(UniversalInteger(Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            True,
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Undefined(),
            False,
        ),
    ],
)
def test_aggregate_is_compatible(aggregate: Type, other: Type, expected: bool) -> None:
    assert aggregate.is_compatible(other) == expected
    assert other.is_compatible(aggregate) == expected


@pytest.mark.parametrize(
    ("composite", "other", "expected"),
    [
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Any(),
            Sequence("A", Integer("B", Bounds(10, 100))),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Sequence("A", Integer("B", Bounds(10, 100))),
            Sequence("A", Integer("B", Bounds(10, 100))),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(10, 100))),
            Sequence("A", Integer("B", Bounds(10, 100))),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(10, 100))),
            Sequence("A", Integer("B", Bounds(10, 100))),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("C", Bounds(10, 100))),
            Undefined(),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("C", Bounds(20, 200))),
            Undefined(),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            Undefined(),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Undefined(),
            Undefined(),
        ),
    ],
)
def test_composite_common_type(composite: Type, other: Type, expected: Type) -> None:
    assert composite.common_type(other) == expected
    assert other.common_type(composite) == expected


@pytest.mark.parametrize(
    ("composite", "other", "expected"),
    [
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Any(),
            True,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Sequence("A", Integer("B", Bounds(10, 100))),
            True,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Any()),
            True,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(10, 100))),
            True,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(10, 100))),
            True,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("C", Bounds(10, 100))),
            False,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("C", Bounds(20, 200))),
            False,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            False,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Undefined(),
            False,
        ),
    ],
)
def test_composite_is_compatible(composite: Type, other: Type, expected: bool) -> None:
    assert composite.is_compatible(other) == expected
    assert other.is_compatible(composite) == expected


@pytest.mark.parametrize(
    ("message", "other", "expected"),
    [
        (Message("A"), Any(), Message("A")),
        (Message("A"), Message("A"), Message("A")),
        (Message("A"), Message("B"), Undefined()),
        (Message("A"), Undefined(), Undefined()),
        (Message("A"), ENUM_B, Undefined()),
    ],
)
def test_message_common_type(message: Type, other: Type, expected: Type) -> None:
    assert message.common_type(other) == expected
    assert other.common_type(message) == expected


@pytest.mark.parametrize(
    ("message", "other", "expected"),
    [
        (Message("A"), Any(), True),
        (Message("A"), Message("A"), True),
        (Message("A"), Message("B"), False),
        (Message("A"), Undefined(), False),
        (Message("A"), ENUM_B, False),
    ],
)
def test_message_is_compatible(message: Type, other: Type, expected: bool) -> None:
    assert message.is_compatible(other) == expected
    assert other.is_compatible(message) == expected


@pytest.mark.parametrize(
    ("channel", "other", "expected"),
    [
        (Channel(readable=True, writable=False), Any(), Channel(readable=True, writable=False)),
        (
            Channel(readable=True, writable=False),
            Channel(readable=True, writable=False),
            Channel(readable=True, writable=False),
        ),
        (
            Channel(readable=True, writable=False),
            Channel(readable=False, writable=True),
            Undefined(),
        ),
        (Channel(readable=True, writable=False), Undefined(), Undefined()),
        (
            Channel(readable=True, writable=False),
            ENUM_A,
            Undefined(),
        ),
    ],
)
def test_channel_common_type(channel: Type, other: Type, expected: Type) -> None:
    assert channel.common_type(other) == expected
    assert other.common_type(channel) == expected


@pytest.mark.parametrize(
    ("channel", "other", "expected"),
    [
        (Channel(readable=True, writable=False), Any(), True),
        (Any(), Channel(readable=True, writable=False), True),
        (Channel(readable=True, writable=False), Channel(readable=True, writable=False), True),
        (Channel(readable=True, writable=False), Channel(readable=False, writable=True), False),
        (Channel(readable=False, writable=True), Channel(readable=True, writable=False), False),
        (Channel(readable=True, writable=True), Channel(readable=False, writable=True), True),
        (Channel(readable=True, writable=True), Channel(readable=True, writable=False), True),
        (Channel(readable=False, writable=True), Channel(readable=True, writable=True), False),
        (Channel(readable=True, writable=False), Channel(readable=True, writable=True), False),
        (Channel(readable=True, writable=False), Undefined(), False),
        (Channel(readable=True, writable=False), ENUM_A, False),
    ],
)
def test_channel_is_compatible(channel: Type, other: Type, expected: bool) -> None:
    assert channel.is_compatible(other) == expected


@pytest.mark.parametrize(
    ("types", "expected"),
    [
        (
            [],
            Any(),
        ),
        (
            [INT_A, SEQ_A],
            UNDEFINED,
        ),
        (
            [
                Integer("A", Bounds(10, 100)),
                Integer("A", Bounds(10, 100)),
            ],
            BASE_INTEGER,
        ),
        (
            [
                UniversalInteger(Bounds(10, 50)),
                Integer("A", Bounds(10, 100)),
                UniversalInteger(Bounds(50, 100)),
            ],
            BASE_INTEGER,
        ),
        (
            [
                UniversalInteger(Bounds(10, 50)),
                Integer("A", Bounds(10, 100)),
                UniversalInteger(Bounds(20, 200)),
            ],
            BASE_INTEGER,
        ),
        (
            [
                Aggregate(Integer("A", Bounds(10, 100))),
                Aggregate(UniversalInteger(Bounds(20, 100))),
                Aggregate(Integer("B", Bounds(20, 200))),
            ],
            Aggregate(BASE_INTEGER),
        ),
        (
            [
                Aggregate(UniversalInteger(Bounds(10, 20))),
                Aggregate(UniversalInteger(Bounds(50, 60))),
                Aggregate(UniversalInteger(Bounds(90, 100))),
            ],
            Aggregate(UniversalInteger(Bounds(10, 100))),
        ),
    ],
)
def test_common_type(types: abc.Sequence[Type], expected: Type) -> None:
    assert common_type(types) == expected
    assert common_type(list(reversed(types))) == expected


@pytest.mark.parametrize(
    ("actual", "expected"),
    [
        (
            Any(),
            (INT_A, ENUM_A),
        ),
        (
            INT_A,
            UNDEFINED,
        ),
    ],
)
def test_check_type(actual: Type, expected: Type | tuple[Type, ...]) -> None:
    check_type(actual, expected, Location((10, 20)), '"A"').propagate()


@pytest.mark.parametrize(
    ("actual", "expected", "match"),
    [
        (
            Message("A"),
            Channel(readable=False, writable=True),
            r"^<stdin>:10:20: error: expected writable channel\n"
            r'<stdin>:10:20: error: found message type "A"$',
        ),
        (
            BASE_INTEGER,
            Message("A"),
            r"^"
            r'<stdin>:10:20: error: expected message type "A"\n'
            r'<stdin>:10:20: error: found integer type "__BUILTINS__::Base_Integer"'
            r" \(0 \.\. 2\*\*63 - 1\)"
            r"$",
        ),
        (
            Undefined(),
            Integer("A", Bounds(10, 100)),
            r'^<stdin>:10:20: error: undefined "A"$',
        ),
    ],
)
def test_check_type_error(
    actual: Type,
    expected: Type | tuple[Type, ...],
    match: str,
) -> None:
    with pytest.raises(RecordFluxError, match=match):
        check_type(actual, expected, Location((10, 20)), '"A"').propagate()


@pytest.mark.parametrize(
    ("actual", "expected"),
    [
        (
            Any(),
            Integer,
        ),
        (
            BASE_INTEGER,
            AnyInteger,
        ),
        (
            SEQ_A,
            Composite,
        ),
        (
            MSG_A,
            Compound,
        ),
    ],
)
def test_check_type_instance(
    actual: Type,
    expected: type[Type] | tuple[type[Type], ...],
) -> None:
    check_type_instance(actual, expected, Location((10, 20)), '"A"').propagate()


@pytest.mark.parametrize(
    ("actual", "expected", "match"),
    [
        (
            Message("M"),
            Channel,
            r"^<stdin>:10:20: error: expected channel\n"
            r'<stdin>:10:20: error: found message type "M"$',
        ),
        (
            BASE_INTEGER,
            (Sequence, Message),
            r"^"
            r"<stdin>:10:20: error: expected sequence type or message type\n"
            r'<stdin>:10:20: error: found integer type "__BUILTINS__::Base_Integer"'
            r" \(0 \.\. 2\*\*63 - 1\)"
            r"$",
        ),
        (
            Undefined(),
            Integer,
            r'^<stdin>:10:20: error: undefined "A"$',
        ),
    ],
)
def test_check_type_instance_error(
    actual: Type,
    expected: type[Type] | tuple[type[Type], ...],
    match: str,
) -> None:
    with pytest.raises(RecordFluxError, match=match):
        check_type_instance(actual, expected, Location((10, 20)), '"A"').propagate()


@pytest.mark.parametrize(
    "type_",
    [
        Undefined(),
        Any(),
        ENUM_A,
        Builtins.UNIVERSAL_INTEGER,
        Builtins.BASE_INTEGER,
        Aggregate(INT_A),
        SEQ_A,
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
