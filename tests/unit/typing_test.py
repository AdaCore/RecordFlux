import typing as ty

import pytest

from rflx.typing_ import (
    Aggregate,
    Any,
    AnyInteger,
    Bounds,
    Composite,
    Enumeration,
    Integer,
    Message,
    Type,
    Undefined,
    UndefinedInteger,
    UniversalInteger,
    common_type,
)


def test_bounds_contains() -> None:
    assert 1 not in Bounds(None, None)
    assert 1 in Bounds(1, 100)
    assert 0 not in Bounds(1, 100)
    assert Bounds(1, 100) in Bounds(1, 100)
    assert Bounds(1, 10) in Bounds(1, 100)
    assert Bounds(10, 100) in Bounds(1, 100)
    assert Bounds(10, 10) in Bounds(1, 100)
    assert Bounds(1, 200) not in Bounds(1, 100)
    assert Bounds(0, 100) not in Bounds(1, 100)
    assert Bounds(1, 100) not in Bounds(None, None)
    assert Bounds(None, None) not in Bounds(1, 100)
    assert "invalid" not in Bounds(1, 100)


def test_bounds_str() -> None:
    assert str(Bounds(None, None)) == "undefined"
    assert str(Bounds(1, 1)) == "1"
    assert str(Bounds(1, 100)) == "1 .. 100"


@pytest.mark.parametrize(
    "enumeration,other,expected",
    [
        (Enumeration("A"), Any(), Enumeration("A")),
        (Enumeration("A"), Enumeration("A"), Enumeration("A")),
        (Enumeration("A"), Undefined(), Undefined()),
        (Enumeration("A"), Enumeration("B"), Undefined()),
        (Enumeration("A"), Integer("A"), Undefined()),
    ],
)
def test_enumeration_common_type(enumeration: Type, other: Type, expected: Type) -> None:
    assert enumeration.common_type(other) == expected
    assert other.common_type(enumeration) == expected


@pytest.mark.parametrize(
    "enumeration,other,expected",
    [
        (Enumeration("A"), Any(), True),
        (Enumeration("A"), Enumeration("A"), True),
        (Enumeration("A"), Undefined(), False),
        (Enumeration("A"), Enumeration("B"), False),
        (Enumeration("A"), Integer("A"), False),
    ],
)
def test_enumeration_is_compatible(enumeration: Type, other: Type, expected: bool) -> None:
    assert enumeration.is_compatible(other) == expected
    assert other.is_compatible(enumeration) == expected


@pytest.mark.parametrize(
    "any_integer,other,expected",
    [
        (AnyInteger(), Any(), AnyInteger()),
        (AnyInteger(), AnyInteger(), AnyInteger()),
        (
            AnyInteger(),
            Integer("A", Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
        ),
        (
            AnyInteger(),
            UniversalInteger(Bounds(10, 100)),
            UniversalInteger(Bounds(10, 100)),
        ),
        (
            AnyInteger(),
            UndefinedInteger(),
            UndefinedInteger(),
        ),
        (AnyInteger(), Undefined(), Undefined()),
        (AnyInteger(), Enumeration("B"), Undefined()),
    ],
)
def test_any_integer_common_type(any_integer: Type, other: Type, expected: Type) -> None:
    assert any_integer.common_type(other) == expected
    assert other.common_type(any_integer) == expected


@pytest.mark.parametrize(
    "any_integer,other,expected",
    [
        (AnyInteger(), Any(), True),
        (AnyInteger(), AnyInteger(), True),
        (
            AnyInteger(),
            Integer("A", Bounds(10, 100)),
            True,
        ),
        (
            AnyInteger(),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (
            AnyInteger(),
            UndefinedInteger(),
            True,
        ),
        (AnyInteger(), Undefined(), False),
        (AnyInteger(), Enumeration("B"), False),
    ],
)
def test_any_integer_is_compatible(any_integer: Type, other: Type, expected: bool) -> None:
    assert any_integer.is_compatible(other) == expected
    assert other.is_compatible(any_integer) == expected


@pytest.mark.parametrize(
    "undefined_integer,other,expected",
    [
        (UndefinedInteger(), Any(), UndefinedInteger()),
        (UndefinedInteger(), AnyInteger(), UndefinedInteger()),
        (UndefinedInteger(), UndefinedInteger(), UndefinedInteger()),
        (
            UndefinedInteger(),
            Integer("A", Bounds(10, 100)),
            UndefinedInteger(),
        ),
        (
            UndefinedInteger(),
            UniversalInteger(Bounds(10, 100)),
            UndefinedInteger(),
        ),
        (UndefinedInteger(), Undefined(), Undefined()),
        (UndefinedInteger(), Enumeration("B"), Undefined()),
    ],
)
def test_undefined_integer_common_type(
    undefined_integer: Type, other: Type, expected: Type
) -> None:
    assert undefined_integer.common_type(other) == expected
    assert other.common_type(undefined_integer) == expected


@pytest.mark.parametrize(
    "undefined_integer,other,expected",
    [
        (UndefinedInteger(), Any(), True),
        (UndefinedInteger(), AnyInteger(), True),
        (UndefinedInteger(), UndefinedInteger(), True),
        (
            UndefinedInteger(),
            Integer("A", Bounds(10, 100)),
            True,
        ),
        (
            UndefinedInteger(),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (UndefinedInteger(), Undefined(), False),
        (UndefinedInteger(), Enumeration("B"), False),
    ],
)
def test_undefined_integer_is_compatible(
    undefined_integer: Type, other: Type, expected: bool
) -> None:
    assert undefined_integer.is_compatible(other) == expected
    assert other.is_compatible(undefined_integer) == expected


@pytest.mark.parametrize(
    "integer,other,expected",
    [
        (Integer("A"), Any(), Integer("A")),
        (Integer("A"), AnyInteger(), Integer("A")),
        (Integer("A"), Integer("A"), Integer("A")),
        (Integer("A"), UniversalInteger(), Integer("A")),
        (
            Integer("A", Bounds(10, 100)),
            Any(),
            Integer("A", Bounds(10, 100)),
        ),
        (
            Integer("A", Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
        ),
        (
            Integer("A"),
            Integer("B"),
            UndefinedInteger(),
        ),
        (
            Integer("A", Bounds(10, 100)),
            Integer("B", Bounds(10, 100)),
            UndefinedInteger(),
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(0, 200)),
            UndefinedInteger(),
        ),
        (Integer("A"), Undefined(), Undefined()),
        (Integer("A"), Enumeration("B"), Undefined()),
    ],
)
def test_integer_common_type(integer: Type, other: Type, expected: Type) -> None:
    assert integer.common_type(other) == expected
    assert other.common_type(integer) == expected


@pytest.mark.parametrize(
    "integer,other,expected",
    [
        (Integer("A"), Any(), True),
        (Integer("A"), AnyInteger(), True),
        (Integer("A"), Integer("A"), True),
        (Integer("A"), UniversalInteger(), True),
        (
            Integer("A", Bounds(10, 100)),
            Any(),
            True,
        ),
        (
            Integer("A", Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
            True,
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (
            Integer("A"),
            Integer("B"),
            True,
        ),
        (
            Integer("A", Bounds(10, 100)),
            Integer("B", Bounds(10, 100)),
            True,
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(0, 200)),
            True,
        ),
        (Integer("A"), Undefined(), False),
        (Integer("A"), Enumeration("B"), False),
    ],
)
def test_integer_is_compatible(integer: Type, other: Type, expected: bool) -> None:
    assert integer.is_compatible(other) == expected
    assert other.is_compatible(integer) == expected


@pytest.mark.parametrize(
    "aggregate,other,expected",
    [
        (
            Aggregate(Integer("A")),
            Any(),
            Aggregate(Integer("A")),
        ),
        (
            Aggregate(Integer("A")),
            Aggregate(Integer("A")),
            Aggregate(Integer("A")),
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(10, 100))),
            Aggregate(UndefinedInteger()),
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("A", Bounds(20, 200))),
            Aggregate(UndefinedInteger()),
        ),
        (
            Aggregate(UniversalInteger(Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            Aggregate(UniversalInteger(Bounds(10, 200))),
        ),
        (
            Aggregate(Integer("A")),
            Undefined(),
            Undefined(),
        ),
    ],
)
def test_aggregate_common_type(aggregate: Type, other: Type, expected: Type) -> None:
    assert aggregate.common_type(other) == expected
    assert other.common_type(aggregate) == expected


@pytest.mark.parametrize(
    "aggregate,other,expected",
    [
        (
            Aggregate(Integer("A")),
            Any(),
            True,
        ),
        (
            Aggregate(Integer("A")),
            Aggregate(Integer("A")),
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
            Aggregate(Integer("A")),
            Undefined(),
            False,
        ),
    ],
)
def test_aggregate_is_compatible(aggregate: Type, other: Type, expected: bool) -> None:
    assert aggregate.is_compatible(other) == expected
    assert other.is_compatible(aggregate) == expected


@pytest.mark.parametrize(
    "composite,other,expected",
    [
        (
            Composite("A", Integer("B")),
            Any(),
            Composite("A", Integer("B")),
        ),
        (
            Composite("A", Integer("B")),
            Composite("A", Integer("B")),
            Composite("A", Integer("B")),
        ),
        (
            Composite("A", Integer("B")),
            Aggregate(Integer("B")),
            Composite("A", Integer("B")),
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(10, 100))),
            Composite("A", Integer("B", Bounds(10, 100))),
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(10, 100))),
            Composite("A", Integer("B", Bounds(10, 100))),
        ),
        (
            Composite("A", Integer("B")),
            Aggregate(Integer("C")),
            Undefined(),
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(20, 200))),
            Undefined(),
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            Undefined(),
        ),
        (
            Composite("A", Integer("B")),
            Undefined(),
            Undefined(),
        ),
    ],
)
def test_composite_common_type(composite: Type, other: Type, expected: Type) -> None:
    assert composite.common_type(other) == expected
    assert other.common_type(composite) == expected


@pytest.mark.parametrize(
    "composite,other,expected",
    [
        (
            Composite("A", Integer("B")),
            Any(),
            True,
        ),
        (
            Composite("A", Integer("B")),
            Composite("A", Integer("B")),
            True,
        ),
        (
            Composite("A", Integer("B")),
            Aggregate(Integer("B")),
            True,
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(10, 100))),
            True,
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(10, 100))),
            True,
        ),
        (
            Composite("A", Integer("B")),
            Aggregate(Integer("C")),
            False,
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(20, 200))),
            False,
        ),
        (
            Composite("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            False,
        ),
        (
            Composite("A", Integer("B")),
            Undefined(),
            False,
        ),
    ],
)
def test_composite_is_compatible(composite: Type, other: Type, expected: bool) -> None:
    assert composite.is_compatible(other) == expected
    assert other.is_compatible(composite) == expected


@pytest.mark.parametrize(
    "message,other,expected",
    [
        (Message("A"), Any(), Message("A")),
        (Message("A"), Message("A"), Message("A")),
        (Message("A"), Message("B"), Undefined()),
        (Message("A"), Undefined(), Undefined()),
        (Message("A"), Enumeration("B"), Undefined()),
    ],
)
def test_message_common_type(message: Type, other: Type, expected: Type) -> None:
    assert message.common_type(other) == expected
    assert other.common_type(message) == expected


@pytest.mark.parametrize(
    "message,other,expected",
    [
        (Message("A"), Any(), True),
        (Message("A"), Message("A"), True),
        (Message("A"), Message("B"), False),
        (Message("A"), Undefined(), False),
        (Message("A"), Enumeration("B"), False),
    ],
)
def test_message_is_compatible(message: Type, other: Type, expected: bool) -> None:
    assert message.is_compatible(other) == expected
    assert other.is_compatible(message) == expected


@pytest.mark.parametrize(
    "types,expected",
    [
        (
            [],
            Any(),
        ),
        (
            [
                Integer("A", Bounds(10, 100)),
                Integer("A", Bounds(10, 100)),
                Integer("A", Bounds(10, 100)),
            ],
            Integer("A", Bounds(10, 100)),
        ),
        (
            [
                UniversalInteger(Bounds(10, 50)),
                Integer("A", Bounds(10, 100)),
                UniversalInteger(Bounds(50, 100)),
            ],
            Integer("A", Bounds(10, 100)),
        ),
        (
            [
                UniversalInteger(Bounds(10, 50)),
                Integer("A", Bounds(10, 100)),
                UniversalInteger(Bounds(20, 200)),
            ],
            UndefinedInteger(),
        ),
        (
            [
                Aggregate(Integer("A", Bounds(10, 100))),
                Aggregate(UniversalInteger(Bounds(20, 100))),
                Aggregate(Integer("B", Bounds(20, 200))),
            ],
            Aggregate(UndefinedInteger()),
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
def test_common_type(types: ty.Sequence[Type], expected: Type) -> None:
    assert common_type(types) == expected
    assert common_type(list(reversed(types))) == expected
