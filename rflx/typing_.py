from __future__ import annotations

from abc import abstractmethod
from collections import abc
from pathlib import Path
from typing import ClassVar, Optional, Union

import attr

from rflx import const
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID


@attr.s(frozen=True)
class Bounds:
    lower: Optional[int] = attr.ib()
    upper: Optional[int] = attr.ib()

    def __attrs_post_init__(self) -> None:
        assert self.lower is None or self.upper is None or self.lower <= self.upper

    def __bool__(self) -> bool:
        return self.lower is not None and self.upper is not None

    def __contains__(self, item: object) -> bool:
        if isinstance(item, int):
            if self.lower is None or self.upper is None:
                return False
            return self.lower <= item <= self.upper
        if isinstance(item, Bounds):
            if not self and not item:
                return True
            if (
                self.lower is not None
                and self.upper is not None
                and item.lower is not None
                and item.upper is not None
            ):
                return self.lower <= item.lower and self.upper >= item.upper
        return False

    def __str__(self) -> str:
        if not self:
            return "undefined"
        if self.lower == self.upper:
            return str(self.lower)
        return f"{self.lower} .. {self.upper}"

    @staticmethod
    def union(left: Bounds, right: Bounds) -> Bounds:
        if left.lower is None or left.upper is None or right.lower is None or right.upper is None:
            return Bounds(None, None)
        return Bounds(min(left.lower, right.lower), max(left.upper, right.upper))


class Type:
    DESCRIPTIVE_NAME: ClassVar[str]

    def __str__(self) -> str:
        return self.DESCRIPTIVE_NAME

    @abstractmethod
    def is_compatible(self, other: Type) -> bool:
        raise NotImplementedError

    def is_compatible_strong(self, other: Type) -> bool:
        return self.is_compatible(other)

    @abstractmethod
    def common_type(self, other: Type) -> Type:
        raise NotImplementedError


@attr.s(frozen=True)
class Undefined(Type):
    DESCRIPTIVE_NAME: ClassVar[str] = "undefined type"

    def is_compatible(self, other: Type) -> bool:
        return False

    def common_type(self, other: Type) -> Type:
        return self


@attr.s(frozen=True)
class Any(Type):
    DESCRIPTIVE_NAME: ClassVar[str] = "any type"

    def is_compatible(self, other: Type) -> bool:
        return not isinstance(other, Undefined)

    def common_type(self, other: Type) -> Type:
        return other


@attr.s(frozen=True)
class IndependentType(Any):
    identifier: ID = attr.ib(converter=ID)

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or (
            isinstance(other, self.__class__) and self.identifier == other.identifier
        )

    def common_type(self, other: Type) -> Type:
        if other == Any() or self == other:
            return self
        return Undefined()


@attr.s(frozen=True)
class Enumeration(IndependentType):
    DESCRIPTIVE_NAME: ClassVar[str] = "enumeration type"
    literals: abc.Sequence[ID] = attr.ib()
    always_valid: bool = attr.ib(False)
    location: Optional[Location] = attr.ib(default=None, cmp=False)

    def __str__(self) -> str:
        return f'{self.DESCRIPTIVE_NAME} "{self.identifier}"'


BOOLEAN = Enumeration(
    const.BUILTINS_PACKAGE * "Boolean",
    [ID("False"), ID("True")],
    location=Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0)),
)


@attr.s(frozen=True)
class AnyInteger(Any):
    DESCRIPTIVE_NAME: ClassVar[str] = "integer type"

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or isinstance(other, AnyInteger)

    def common_type(self, other: Type) -> Type:
        if other == Any() or self == other:
            return self
        if isinstance(other, AnyInteger):
            return other
        return Undefined()


@attr.s(frozen=True)
class UndefinedInteger(AnyInteger):
    def common_type(self, other: Type) -> Type:
        if other == Any() or isinstance(other, AnyInteger):
            return self
        return Undefined()


@attr.s(frozen=True)
class UniversalInteger(AnyInteger):
    DESCRIPTIVE_NAME: ClassVar[str] = "type universal integer"
    bounds: Bounds = attr.ib(Bounds(None, None))

    def __str__(self) -> str:
        return f"{self.DESCRIPTIVE_NAME} ({self.bounds})"

    def is_compatible_strong(self, other: Type) -> bool:
        return isinstance(other, UniversalInteger) or (
            isinstance(other, Integer) and self.bounds in other.bounds
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, UndefinedInteger):
            return UndefinedInteger()
        if isinstance(other, UniversalInteger) and self.bounds != other.bounds:
            return UniversalInteger(Bounds.union(self.bounds, other.bounds))
        if isinstance(other, Integer):
            if self.bounds in other.bounds:
                return other
            return UndefinedInteger()
        if other == Any() or other == AnyInteger() or self == other:
            return self
        return Undefined()


@attr.s(frozen=True)
class Integer(AnyInteger):
    DESCRIPTIVE_NAME: ClassVar[str] = "integer type"
    identifier: ID = attr.ib(converter=ID)
    bounds: Bounds = attr.ib(Bounds(None, None))
    location: Optional[Location] = attr.ib(default=None, cmp=False)

    def __str__(self) -> str:
        return f'{self.DESCRIPTIVE_NAME} "{self.identifier}" ({self.bounds})'

    def is_compatible_strong(self, other: Type) -> bool:
        return self == other or (
            isinstance(other, UniversalInteger) and other.bounds in self.bounds
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, UndefinedInteger):
            return UndefinedInteger()
        if isinstance(other, UniversalInteger):
            if other.bounds not in self.bounds:
                return UndefinedInteger()
            return self
        if isinstance(other, Integer) and (
            self.identifier != other.identifier or self.bounds != other.bounds
        ):
            return UndefinedInteger()
        if other == Any() or other == AnyInteger() or self == other:
            return self
        return Undefined()


class Composite(Any):
    DESCRIPTIVE_NAME: ClassVar[str] = "composite type"


@attr.s(frozen=True)
class Aggregate(Composite):
    DESCRIPTIVE_NAME: ClassVar[str] = "aggregate"
    element: Type = attr.ib()

    def __str__(self) -> str:
        return f"{self.DESCRIPTIVE_NAME} with element {self.element}"

    def is_compatible(self, other: Type) -> bool:
        return (
            other == Any()
            or isinstance(other, Aggregate)
            or (isinstance(other, Sequence) and self.element.is_compatible_strong(other.element))
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, Sequence) and self.element.is_compatible_strong(other.element):
            return other
        if isinstance(other, Aggregate) and self.element != other.element:
            return Aggregate(self.element.common_type(other.element))
        if other == Any() or self == other:
            return self
        return Undefined()


@attr.s(frozen=True)
class Sequence(Composite):
    DESCRIPTIVE_NAME: ClassVar[str] = "sequence type"
    identifier: ID = attr.ib(converter=ID)
    element: Type = attr.ib()

    def __str__(self) -> str:
        return f'{self.DESCRIPTIVE_NAME} "{self.identifier}" with element {self.element}'

    def is_compatible(self, other: Type) -> bool:
        return (
            other == Any()
            or (isinstance(other, Aggregate) and other.element.is_compatible_strong(self.element))
            or (
                isinstance(other, Sequence)
                and self.identifier == other.identifier
                and self.element == other.element
            )
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, Aggregate) and self.element.is_compatible_strong(other.element):
            return self
        if other == Any() or self == other:
            return self
        return Undefined()


OPAQUE = Sequence(const.INTERNAL_PACKAGE * "Opaque", Integer("Byte", Bounds(0, 255)))


@attr.s(frozen=True)
class Refinement:
    field: ID = attr.ib(converter=ID)
    sdu: Message = attr.ib()
    package: ID = attr.ib(converter=ID)


@attr.s(frozen=True)
class Compound(IndependentType):
    """Base type for any type consisting of multiple fields of different types."""

    DESCRIPTIVE_NAME: ClassVar[str]
    identifier: ID = attr.ib(converter=ID)
    field_combinations: set[tuple[str, ...]] = attr.ib(factory=set)
    parameter_types: abc.Mapping[ID, Type] = attr.ib(factory=dict)
    field_types: abc.Mapping[ID, Type] = attr.ib(factory=dict)

    @property
    def parameters(self) -> set[ID]:
        return set(self.parameter_types.keys())

    @property
    def fields(self) -> set[ID]:
        return set(self.field_types.keys())

    @property
    def types(self) -> abc.Mapping[ID, Type]:
        return {**self.parameter_types, **self.field_types}

    def __str__(self) -> str:
        return f'{self.DESCRIPTIVE_NAME} "{self.identifier}"'


@attr.s(frozen=True)
class Structure(Compound):
    DESCRIPTIVE_NAME = "structure type"


@attr.s(frozen=True)
class Message(Compound):
    DESCRIPTIVE_NAME = "message type"
    refinements: abc.Sequence[Refinement] = attr.ib(factory=list)
    is_definite: bool = attr.ib(False)


@attr.s(frozen=True)
class Channel(Any):
    DESCRIPTIVE_NAME: ClassVar[str] = "channel"
    readable: bool = attr.ib()
    writable: bool = attr.ib()

    def __str__(self) -> str:
        mode = {
            (True, False): "readable ",
            (False, True): "writable ",
            (True, True): "readable and writable ",
        }
        return f"{mode[(self.readable, self.writable)]}{self.DESCRIPTIVE_NAME}"

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or (
            isinstance(other, self.__class__)
            and (self.readable or not other.readable)
            and (self.writable or not other.writable)
        )

    def common_type(self, other: Type) -> Type:
        if other == Any() or self == other:
            return self
        return Undefined()


def common_type(types: abc.Sequence[Type]) -> Type:
    result: Type = Any()

    for t in types:
        result = result.common_type(t)

    return result


def check_type(
    actual: Type,
    expected: Union[Type, tuple[Type, ...]],
    location: Optional[Location],
    description: str,
) -> RecordFluxError:
    assert expected, "empty expected types"

    if actual == Undefined():
        return _undefined_type(location, description)

    error = RecordFluxError()

    expected_types = [expected] if isinstance(expected, Type) else list(expected)

    if Undefined() not in [actual, expected] and all(
        not actual.is_compatible(t) for t in expected_types
    ):
        desc = (
            " or ".join(map(str, expected_types)) if isinstance(expected, tuple) else str(expected)
        )
        error.extend(
            [
                (f"expected {desc}", Subsystem.MODEL, Severity.ERROR, location),
                (f"found {actual}", Subsystem.MODEL, Severity.INFO, location),
            ],
        )

    return error


def check_type_instance(
    actual: Type,
    expected: Union[type[Type], tuple[type[Type], ...]],
    location: Optional[Location],
    description: str = "",
) -> RecordFluxError:
    assert expected, "empty expected types"

    if actual == Undefined():
        return _undefined_type(location, description)

    error = RecordFluxError()

    if not isinstance(actual, expected) and actual != Any():
        desc = (
            " or ".join(e.DESCRIPTIVE_NAME for e in expected)
            if isinstance(expected, tuple)
            else expected.DESCRIPTIVE_NAME
        )
        error.extend(
            [
                (f"expected {desc}", Subsystem.MODEL, Severity.ERROR, location),
                (f"found {actual}", Subsystem.MODEL, Severity.INFO, location),
            ],
        )

    return error


def _undefined_type(location: Optional[Location], description: str = "") -> RecordFluxError:
    error = RecordFluxError()
    error.extend(
        [
            (
                "undefined" + (f" {description}" if description else ""),
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )
        ],
    )
    return error
