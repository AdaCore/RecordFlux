from __future__ import annotations

from collections import abc
from typing import Optional, Union

import pytest

from rflx.identifier import ID
from rflx.rapidflux import Location, RecordFluxError
from rflx.typing_ import (
    Aggregate,
    Any,
    BaseInteger,
    Bounds,
    Channel,
    Enumeration,
    Integer,
    Message,
    Sequence,
    Type,
    Undefined,
    UniversalInteger,
    check_type,
    check_type_instance,
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


@pytest.mark.parametrize(
    ("lower", "upper"),
    [
        (10, 0),
        (None, 1),
        (1, None),
    ],
)
@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_bounds_error(lower: Optional[int], upper: Optional[int]) -> None:
    with pytest.raises(AssertionError):
        Bounds(lower, upper)


def test_bounds_str() -> None:
    assert str(Bounds(None, None)) == "undefined"
    assert str(Bounds(1, 1)) == "1"
    assert str(Bounds(1, 100)) == "1 .. 100"


ENUMERATION_A = Enumeration("A", [ID("AE1"), ID("AE2")])
ENUMERATION_B = Enumeration("B", [ID("BE1"), ID("BE2"), ID("BE3")])


@pytest.mark.parametrize(
    ("enumeration", "other", "expected"),
    [
        (ENUMERATION_A, Any(), ENUMERATION_A),
        (ENUMERATION_A, ENUMERATION_A, ENUMERATION_A),
        (ENUMERATION_A, Undefined(), Undefined()),
        (ENUMERATION_A, ENUMERATION_B, Undefined()),
        (ENUMERATION_A, Integer("A"), Undefined()),
    ],
)
def test_enumeration_common_type(enumeration: Type, other: Type, expected: Type) -> None:
    assert enumeration.common_type(other) == expected
    assert other.common_type(enumeration) == expected


@pytest.mark.parametrize(
    ("enumeration", "other", "expected"),
    [
        (ENUMERATION_A, Any(), True),
        (ENUMERATION_A, ENUMERATION_A, True),
        (ENUMERATION_A, Undefined(), False),
        (ENUMERATION_A, ENUMERATION_B, False),
        (ENUMERATION_A, Integer("A"), False),
    ],
)
def test_enumeration_is_compatible(enumeration: Type, other: Type, expected: bool) -> None:
    assert enumeration.is_compatible(other) == expected
    assert other.is_compatible(enumeration) == expected


@pytest.mark.parametrize(
    ("base_integer", "other", "expected"),
    [
        (BaseInteger(), Any(), BaseInteger()),
        (BaseInteger(), BaseInteger(), BaseInteger()),
        (
            BaseInteger(),
            Integer("A", Bounds(10, 100)),
            BaseInteger(),
        ),
        (
            BaseInteger(),
            UniversalInteger(Bounds(10, 100)),
            BaseInteger(),
        ),
        (BaseInteger(), Undefined(), Undefined()),
        (BaseInteger(), ENUMERATION_B, Undefined()),
    ],
)
def test_base_integer_common_type(base_integer: Type, other: Type, expected: Type) -> None:
    assert base_integer.common_type(other) == expected
    assert other.common_type(base_integer) == expected


@pytest.mark.parametrize(
    ("base_integer", "other", "expected"),
    [
        (BaseInteger(), Any(), True),
        (BaseInteger(), BaseInteger(), True),
        (
            BaseInteger(),
            Integer("A", Bounds(10, 100)),
            True,
        ),
        (
            BaseInteger(),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (BaseInteger(), Undefined(), False),
        (BaseInteger(), ENUMERATION_B, False),
    ],
)
def test_base_integer_is_compatible(base_integer: Type, other: Type, expected: bool) -> None:
    assert base_integer.is_compatible(other) == expected
    assert other.is_compatible(base_integer) == expected


@pytest.mark.parametrize(
    ("universal_integer", "other", "expected"),
    [
        (UniversalInteger(), Any(), UniversalInteger()),
        (UniversalInteger(), BaseInteger(), BaseInteger()),
        (UniversalInteger(), UniversalInteger(), UniversalInteger()),
        (
            UniversalInteger(),
            Integer("A", Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
        ),
        (
            UniversalInteger(Bounds(20, 80)),
            Integer("A", Bounds(10, 100)),
            Integer("A", Bounds(10, 100)),
        ),
        (
            UniversalInteger(),
            UniversalInteger(Bounds(10, 100)),
            UniversalInteger(),
        ),
        (UniversalInteger(), Undefined(), Undefined()),
        (UniversalInteger(), ENUMERATION_B, Undefined()),
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
        (UniversalInteger(), Any(), True),
        (UniversalInteger(), BaseInteger(), True),
        (UniversalInteger(), UniversalInteger(), True),
        (
            UniversalInteger(),
            Integer("A", Bounds(10, 100)),
            True,
        ),
        (
            UniversalInteger(),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (UniversalInteger(), Undefined(), False),
        (UniversalInteger(), ENUMERATION_B, False),
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
        (Integer("A"), Any(), Integer("A")),
        (Integer("A"), BaseInteger(), BaseInteger()),
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
            BaseInteger(),
        ),
        (
            Integer("A", Bounds(10, 100)),
            Integer("B", Bounds(10, 100)),
            BaseInteger(),
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(0, 200)),
            Integer("A", Bounds(10, 100)),
        ),
        (Integer("A"), Undefined(), Undefined()),
        (Integer("A"), ENUMERATION_B, Undefined()),
    ],
)
def test_integer_common_type(integer: Type, other: Type, expected: Type) -> None:
    assert integer.common_type(other) == expected
    assert other.common_type(integer) == expected


@pytest.mark.parametrize(
    ("integer", "other", "expected"),
    [
        (Integer("A"), Any(), True),
        (Integer("A"), BaseInteger(), True),
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
            Integer("A", Bounds(0, 200)),
            UniversalInteger(Bounds(10, 100)),
            True,
        ),
        (
            Integer("A", Bounds(10, 100)),
            UniversalInteger(Bounds(0, 200)),
            True,
        ),
        (Integer("A"), Undefined(), False),
        (Integer("A"), ENUMERATION_B, False),
    ],
)
def test_integer_is_compatible(integer: Type, other: Type, expected: bool) -> None:
    assert integer.is_compatible(other) == expected
    assert other.is_compatible(integer) == expected


@pytest.mark.parametrize(
    ("integer", "other", "expected"),
    [
        (Integer("A"), Any(), True),
        (Integer("A"), BaseInteger(), False),
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
            False,
        ),
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
            True,
        ),
        (Integer("A"), Undefined(), False),
        (Integer("A"), ENUMERATION_B, False),
    ],
)
def test_integer_is_compatible_strong(integer: Type, other: Type, expected: bool) -> None:
    assert integer.is_compatible_strong(other) == expected
    assert other.is_compatible_strong(integer) == expected


@pytest.mark.parametrize(
    ("aggregate", "other", "expected"),
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
            Aggregate(BaseInteger()),
        ),
        (
            Aggregate(Integer("A", Bounds(10, 100))),
            Aggregate(Integer("A", Bounds(20, 200))),
            Aggregate(BaseInteger()),
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
    ("aggregate", "other", "expected"),
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
    ("composite", "other", "expected"),
    [
        (
            Sequence("A", Integer("B")),
            Any(),
            Sequence("A", Integer("B")),
        ),
        (
            Sequence("A", Integer("B")),
            Sequence("A", Integer("B")),
            Sequence("A", Integer("B")),
        ),
        (
            Sequence("A", Integer("B")),
            Aggregate(Integer("B")),
            Sequence("A", Integer("B")),
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
            Sequence("A", Integer("B")),
            Aggregate(Integer("C")),
            Undefined(),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(20, 200))),
            Undefined(),
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            Undefined(),
        ),
        (
            Sequence("A", Integer("B")),
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
            Sequence("A", Integer("B")),
            Any(),
            True,
        ),
        (
            Sequence("A", Integer("B")),
            Sequence("A", Integer("B")),
            True,
        ),
        (
            Sequence("A", Integer("B")),
            Aggregate(Any()),
            True,
        ),
        (
            Sequence("A", Integer("B")),
            Aggregate(Integer("B")),
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
            Sequence("A", Integer("B")),
            Aggregate(Integer("C")),
            False,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(Integer("B", Bounds(20, 200))),
            False,
        ),
        (
            Sequence("A", Integer("B", Bounds(10, 100))),
            Aggregate(UniversalInteger(Bounds(20, 200))),
            False,
        ),
        (
            Sequence("A", Integer("B")),
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
        (Message("A"), ENUMERATION_B, Undefined()),
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
        (Message("A"), ENUMERATION_B, False),
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
            ENUMERATION_A,
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
        (Channel(readable=True, writable=False), ENUMERATION_A, False),
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
            Integer("A", Bounds(10, 100)),
        ),
        (
            [
                Aggregate(Integer("A", Bounds(10, 100))),
                Aggregate(UniversalInteger(Bounds(20, 100))),
                Aggregate(Integer("B", Bounds(20, 200))),
            ],
            Aggregate(BaseInteger()),
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
            Integer("A"),
        ),
    ],
)
def test_check_type(actual: Type, expected: Type) -> None:
    check_type(actual, expected, Location((10, 20)), '"A"').propagate()


@pytest.mark.parametrize(
    ("actual", "expected", "match"),
    [
        (
            Message("A"),
            Channel(readable=False, writable=True),
            r"^<stdin>:10:20: error: expected writable channel\n"
            r'<stdin>:10:20: note: found message type "A"$',
        ),
        (
            BaseInteger(),
            Message("A"),
            r'^<stdin>:10:20: error: expected message type "A"\n'
            r"<stdin>:10:20: note: found integer type$",
        ),
        (
            Undefined(),
            Integer("A"),
            r'^<stdin>:10:20: error: undefined "A"$',
        ),
    ],
)
def test_check_type_error(actual: Type, expected: Type, match: str) -> None:
    with pytest.raises(RecordFluxError, match=match):
        check_type(actual, expected, Location((10, 20)), '"A"').propagate()


@pytest.mark.parametrize(
    ("actual", "expected"),
    [
        (
            Any(),
            Integer,
        ),
    ],
)
def test_check_type_instance(
    actual: Type,
    expected: Union[type[Type], tuple[type[Type], ...]],
) -> None:
    check_type_instance(actual, expected, Location((10, 20)), '"A"').propagate()


@pytest.mark.parametrize(
    ("actual", "expected", "match"),
    [
        (
            Message("M"),
            Channel,
            r"^<stdin>:10:20: error: expected channel\n"
            r'<stdin>:10:20: note: found message type "M"$',
        ),
        (
            BaseInteger(),
            (Sequence, Message),
            r"^<stdin>:10:20: error: expected sequence type or message type\n"
            r"<stdin>:10:20: note: found integer type$",
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
    expected: Union[type[Type], tuple[type[Type], ...]],
    match: str,
) -> None:
    with pytest.raises(RecordFluxError, match=match):
        check_type_instance(actual, expected, Location((10, 20)), '"A"').propagate()
