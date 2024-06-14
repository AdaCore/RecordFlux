from __future__ import annotations

from abc import abstractmethod
from collections import abc
from pathlib import Path
from typing import ClassVar, Final, Optional, Union

import attr

from rflx import const
from rflx.identifier import ID
from rflx.rapidflux import Annotation, ErrorEntry, Location, RecordFluxError, Severity
from rflx.rapidflux.ty import Bounds


class Type:
    DESCRIPTIVE_NAME: ClassVar[str]

    def __str__(self) -> str:
        return self.DESCRIPTIVE_NAME

    @abstractmethod
    def is_compatible(self, other: Type) -> bool:
        raise NotImplementedError

    def is_compatible_strong(self, other: Type) -> bool:
        return (
            other.is_compatible_strong(self)
            if isinstance(other, Integer)
            else self.is_compatible(other)
        )

    @abstractmethod
    def common_type(self, other: Type) -> Type:
        raise NotImplementedError


@attr.s(frozen=True)
class Undefined(Type):
    DESCRIPTIVE_NAME: ClassVar[str] = "undefined type"

    def is_compatible(self, _other: Type) -> bool:
        return False

    def common_type(self, _other: Type) -> Type:
        return self


UNDEFINED: Final = Undefined()


@attr.s(frozen=True)
class Any(Type):
    DESCRIPTIVE_NAME: ClassVar[str] = "any type"

    def is_compatible(self, other: Type) -> bool:
        return not isinstance(other, Undefined)

    def common_type(self, other: Type) -> Type:
        return other


@attr.s(frozen=True)
class NamedType(Any):
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
class Enumeration(NamedType):
    DESCRIPTIVE_NAME: ClassVar[str] = "enumeration type"
    literals: abc.Sequence[ID] = attr.ib()
    always_valid: bool = attr.ib(default=False)
    location: Optional[Location] = attr.ib(default=None, cmp=False)

    def __str__(self) -> str:
        return f'{self.DESCRIPTIVE_NAME} "{self.identifier}"'


@attr.s(frozen=True)
class AnyInteger(Any):
    DESCRIPTIVE_NAME: ClassVar[str] = "integer type"
    bounds: Bounds = attr.ib()

    @abstractmethod
    def is_compatible(self, other: Type) -> bool:
        raise NotImplementedError


@attr.s(frozen=True)
class UniversalInteger(AnyInteger):
    DESCRIPTIVE_NAME: ClassVar[str] = "type universal integer"
    bounds: Bounds = attr.ib()

    def __str__(self) -> str:
        bounds = f" ({self.bounds})" if self.bounds else ""
        return f"{self.DESCRIPTIVE_NAME}{bounds}"

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or isinstance(other, AnyInteger)

    def common_type(self, other: Type) -> Type:
        if isinstance(other, UniversalInteger) and self.bounds != other.bounds:
            return UniversalInteger(
                Bounds.merge(self.bounds, other.bounds),
            )
        if isinstance(other, AnyInteger):
            return other
        if other == Any() or self == other:
            return self
        return Undefined()


@attr.s(frozen=True)
class Integer(AnyInteger, NamedType):
    DESCRIPTIVE_NAME: ClassVar[str] = "integer type"
    identifier: ID = attr.ib(converter=ID)
    bounds: Bounds = attr.ib()
    location: Optional[Location] = attr.ib(default=None, cmp=False)

    def __str__(self) -> str:
        bounds = f" ({self.bounds})" if self.bounds else ""
        return f'{self.DESCRIPTIVE_NAME} "{self.identifier}"{bounds}'

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or isinstance(other, AnyInteger)

    def is_compatible_strong(self, other: Type) -> bool:
        return (
            self == other
            or other == Any()
            or (
                isinstance(other, UniversalInteger)
                and (other.bounds in self.bounds if other.bounds and self.bounds else True)
            )
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, UniversalInteger):
            return self
        if isinstance(other, Integer) and (
            self.identifier != other.identifier or self.bounds != other.bounds
        ):
            return BASE_INTEGER
        if isinstance(other, AnyInteger):
            return other
        if other == Any() or self == other:
            return self
        return Undefined()


@attr.s(frozen=True)
class Composite(Any):
    DESCRIPTIVE_NAME: ClassVar[str] = "composite type"
    element: Type = attr.ib()


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
            or (
                isinstance(other, Sequence)
                and (other.element == Any() or self.element.is_compatible_strong(other.element))
            )
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, Sequence) and self.is_compatible(other):
            return other
        if isinstance(other, Aggregate) and self.element != other.element:
            return Aggregate(self.element.common_type(other.element))
        if other == Any() or self == other:
            return self
        return Undefined()


@attr.s(frozen=True)
class Sequence(Composite, NamedType):
    DESCRIPTIVE_NAME: ClassVar[str] = "sequence type"
    identifier: ID = attr.ib(converter=ID)
    element: Type = attr.ib()

    def __str__(self) -> str:
        return f'{self.DESCRIPTIVE_NAME} "{self.identifier}" with element {self.element}'

    def is_compatible(self, other: Type) -> bool:
        return (
            other == Any()
            or (
                isinstance(other, Aggregate)
                and (other.element == Any() or other.element.is_compatible_strong(self.element))
            )
            or (
                isinstance(other, Sequence)
                and self.identifier == other.identifier
                and self.element == other.element
            )
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, Aggregate) and self.is_compatible(other):
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
class Compound(NamedType):
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
    is_definite: bool = attr.ib(default=False)


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

        assert location is not None
        error.push(
            ErrorEntry(
                f"expected {desc}",
                Severity.ERROR,
                location,
                annotations=(
                    [
                        Annotation(
                            f"found {actual}",
                            Severity.ERROR,
                            location,
                        ),
                    ]
                ),
                generate_default_annotation=False,
            ),
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
        assert location is not None
        error.push(
            ErrorEntry(
                f"expected {desc}",
                Severity.ERROR,
                location,
                annotations=[Annotation(f"found {actual}", Severity.ERROR, location)],
                generate_default_annotation=False,
            ),
        )

    return error


def _undefined_type(location: Optional[Location], description: str = "") -> RecordFluxError:
    return RecordFluxError(
        [
            ErrorEntry(
                "undefined" + (f" {description}" if description else ""),
                Severity.ERROR,
                location,
            ),
        ],
    )


BOOLEAN = Enumeration(
    const.BUILTINS_PACKAGE * "Boolean",
    [ID("False"), ID("True")],
    location=Location((1, 1), Path(str(const.BUILTINS_PACKAGE)), (1, 1)),
)

# Assumes a Bit_Length type semantically equivalent to:
#
# type Bit_Length is range 0 .. Integer'Last * 8.
#
# Potentially failing range checks will not be detected in the model, if a custom Bit_Length type
# with different bounds is used (eng/recordflux/RecordFlux#317).
bit_length_bounds = Bounds(0, (2**31 - 1) * 8)

BIT_LENGTH = Integer(
    const.BUILTINS_PACKAGE * "Bit_Length",
    bit_length_bounds,
    location=Location((1, 1), Path(str(const.BUILTINS_PACKAGE)), (1, 1)),
)

UNIVERSAL_INTEGER = UniversalInteger(Bounds(0, 2**const.MAX_SCALAR_SIZE - 1))

BASE_INTEGER = Integer(
    const.BUILTINS_PACKAGE * "Base_Integer",
    Bounds(0, 2**const.MAX_SCALAR_SIZE - 1),
    location=Location((1, 1), Path(str(const.BUILTINS_PACKAGE)), (1, 1)),
)

INDEX = Integer(
    const.BUILTINS_PACKAGE * "Index",
    Bounds(1, 2**31 - 1),
    location=Location((1, 1), Path(str(const.BUILTINS_PACKAGE)), (1, 1)),
)

BIT_INDEX = Integer(
    const.BUILTINS_PACKAGE * "Bit_Index",
    Bounds(1, bit_length_bounds.upper),
    location=Location((1, 1), Path(str(const.BUILTINS_PACKAGE)), (1, 1)),
)
