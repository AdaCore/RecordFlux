from __future__ import annotations

from collections import abc

import pytest

from rflx.identifier import ID
from rflx.rapidflux import Location, RecordFluxError
from rflx.rapidflux.ty import Bounds
from rflx.typing_ import (
    BASE_INTEGER,
    Aggregate,
    Any,
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

INTEGER_A = Integer("A", Bounds(10, 100))
ENUMERATION_A = Enumeration("A", [ID("AE1"), ID("AE2")])
ENUMERATION_B = Enumeration("B", [ID("BE1"), ID("BE2"), ID("BE3")])


@pytest.mark.parametrize(
    ("enumeration", "other", "expected"),
    [
        (ENUMERATION_A, Any(), ENUMERATION_A),
        (ENUMERATION_A, ENUMERATION_A, ENUMERATION_A),
        (ENUMERATION_A, Undefined(), Undefined()),
        (ENUMERATION_A, ENUMERATION_B, Undefined()),
        (ENUMERATION_A, Integer("A", Bounds(10, 100)), Undefined()),
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
        (ENUMERATION_A, Integer("A", Bounds(10, 100)), False),
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
        (BASE_INTEGER, ENUMERATION_B, Undefined()),
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
        (BASE_INTEGER, ENUMERATION_B, False),
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
            ENUMERATION_B,
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
        (UniversalInteger(Bounds(10, 100)), ENUMERATION_B, False),
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
            ENUMERATION_B,
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
        (Integer("A", Bounds(10, 100)), ENUMERATION_B, False),
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
        (Integer("A", Bounds(10, 100)), ENUMERATION_B, False),
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
            Integer("A", Bounds(10, 100)),
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
