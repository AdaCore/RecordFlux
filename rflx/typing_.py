import typing as ty
from abc import abstractmethod
from dataclasses import dataclass, field as dataclass_field

from rflx import const
from rflx.error import Location, RecordFluxError, Severity, Subsystem


@dataclass(frozen=True)
class Bounds:
    lower: ty.Optional[int]
    upper: ty.Optional[int]

    def __post_init__(self) -> None:
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
    def union(left: "Bounds", right: "Bounds") -> "Bounds":
        if left.lower is None or left.upper is None or right.lower is None or right.upper is None:
            return Bounds(None, None)
        return Bounds(min(left.lower, right.lower), max(left.upper, right.upper))


class Type:
    descriptive_name: ty.ClassVar[str]

    def __str__(self) -> str:
        return self.descriptive_name

    @abstractmethod
    def is_compatible(self, other: "Type") -> bool:
        raise NotImplementedError

    def is_compatible_strong(self, other: "Type") -> bool:
        return self.is_compatible(other)

    @abstractmethod
    def common_type(self, other: "Type") -> "Type":
        raise NotImplementedError


@dataclass(frozen=True)
class Undefined(Type):
    descriptive_name: ty.ClassVar[str] = "undefined"

    def is_compatible(self, other: Type) -> bool:
        return False

    def common_type(self, other: Type) -> Type:
        return self


@dataclass(frozen=True)
class Any(Type):
    descriptive_name: ty.ClassVar[str] = "any type"

    def is_compatible(self, other: Type) -> bool:
        return not isinstance(other, Undefined)

    def common_type(self, other: Type) -> Type:
        return other


@dataclass(frozen=True)
class IndependentType(Any):
    name: str

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or (isinstance(other, self.__class__) and self.name == other.name)

    def common_type(self, other: Type) -> Type:
        if other == Any() or self == other:
            return self
        return Undefined()


@dataclass(frozen=True)
class Enumeration(IndependentType):
    descriptive_name: ty.ClassVar[str] = "enumeration type"

    def __str__(self) -> str:
        return f'{self.descriptive_name} "{self.name}"'


BOOLEAN = Enumeration(str(const.BUILTINS_PACKAGE * "Boolean"))


@dataclass(frozen=True)
class AnyInteger(Any):
    descriptive_name: ty.ClassVar[str] = "integer type"

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or isinstance(other, AnyInteger)

    def common_type(self, other: "Type") -> "Type":
        if other == Any() or self == other:
            return self
        if isinstance(other, AnyInteger):
            return other
        return Undefined()


@dataclass(frozen=True)
class UndefinedInteger(AnyInteger):
    def common_type(self, other: Type) -> Type:
        if other == Any() or isinstance(other, AnyInteger):
            return self
        return Undefined()


@dataclass(frozen=True)
class UniversalInteger(AnyInteger):
    descriptive_name: ty.ClassVar[str] = "type universal integer"
    bounds: Bounds = Bounds(None, None)

    def __str__(self) -> str:
        return f"{self.descriptive_name} ({self.bounds})"

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


@dataclass(frozen=True)
class Integer(AnyInteger):
    descriptive_name: ty.ClassVar[str] = "integer type"
    name: str
    bounds: Bounds = Bounds(None, None)

    def __str__(self) -> str:
        return f'{self.descriptive_name} "{self.name}" ({self.bounds})'

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
        if isinstance(other, Integer) and (self.name != other.name or self.bounds != other.bounds):
            return UndefinedInteger()
        if other == Any() or other == AnyInteger() or self == other:
            return self
        return Undefined()


class Composite(Any):
    descriptive_name: ty.ClassVar[str] = "composite type"


@dataclass(frozen=True)
class Aggregate(Composite):
    descriptive_name: ty.ClassVar[str] = "aggregate"
    element: Type

    def __str__(self) -> str:
        return f"{self.descriptive_name} with element {self.element}"

    def is_compatible(self, other: Type) -> bool:
        return (
            other == Any()
            or isinstance(other, Aggregate)
            or (isinstance(other, Array) and self.element.is_compatible_strong(other.element))
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, Array) and self.element.is_compatible_strong(other.element):
            return other
        if isinstance(other, Aggregate) and self.element != other.element:
            return Aggregate(self.element.common_type(other.element))
        if other == Any() or self == other:
            return self
        return Undefined()


@dataclass(frozen=True)
class Array(Composite):
    descriptive_name: ty.ClassVar[str] = "array type"
    name: str
    element: Type

    def __str__(self) -> str:
        return f'{self.descriptive_name} "{self.name}" with element {self.element}'

    def is_compatible(self, other: Type) -> bool:
        return (
            other == Any()
            or (isinstance(other, Aggregate) and other.element.is_compatible_strong(self.element))
            or (
                isinstance(other, Array)
                and self.name == other.name
                and self.element == other.element
            )
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, Aggregate) and self.element.is_compatible_strong(other.element):
            return self
        if other == Any() or self == other:
            return self
        return Undefined()


OPAQUE = Array("Opaque", Integer("Byte", Bounds(0, 255)))


@dataclass(frozen=True)
class Message(IndependentType):
    descriptive_name: ty.ClassVar[str] = "message type"
    name: str
    field_combinations: ty.Set[ty.Tuple[str, ...]] = dataclass_field(default_factory=set)
    field_types: ty.Mapping[str, Type] = dataclass_field(default_factory=dict)

    def __str__(self) -> str:
        return f'{self.descriptive_name} "{self.name}"'

    @property
    def fields(self) -> ty.Set[str]:
        return set(self.field_types.keys())


@dataclass(frozen=True)
class Private(IndependentType):
    descriptive_name: ty.ClassVar[str] = "private type"

    def __str__(self) -> str:
        return f'{self.descriptive_name} "{self.name}"'


@dataclass(frozen=True)
class Channel(IndependentType):
    descriptive_name: ty.ClassVar[str] = "channel"

    def __str__(self) -> str:
        return f'{self.descriptive_name} "{self.name}"'


def common_type(types: ty.Sequence[Type]) -> Type:
    result: Type = Any()

    for t in types:
        result = result.common_type(t)

    return result


def check_type(
    actual: Type, expected: Type, location: ty.Optional[Location], description: str
) -> RecordFluxError:
    if actual == Undefined():
        return _undefined_type(location, description)

    error = RecordFluxError()

    if Undefined() not in [actual, expected] and not actual.is_compatible(expected):
        error.append(
            f"expected {expected}",
            Subsystem.MODEL,
            Severity.ERROR,
            location,
        )
        error.append(
            f"found {actual}",
            Subsystem.MODEL,
            Severity.INFO,
            location,
        )

    return error


def check_type_instance(
    actual: Type,
    expected: ty.Union[ty.Type[Type], ty.Tuple[ty.Type[Type], ...]],
    location: ty.Optional[Location],
    description: str = "",
) -> RecordFluxError:
    if actual == Undefined():
        return _undefined_type(location, description)

    error = RecordFluxError()

    if not isinstance(actual, expected) and actual != Any():
        desc = (
            " or ".join(e.descriptive_name for e in expected)
            if isinstance(expected, tuple)
            else expected.descriptive_name
        )
        error.append(
            f"expected {desc}",
            Subsystem.MODEL,
            Severity.ERROR,
            location,
        )
        error.append(
            f"found {actual}",
            Subsystem.MODEL,
            Severity.INFO,
            location,
        )

    return error


def _undefined_type(location: ty.Optional[Location], description: str = "") -> RecordFluxError:
    error = RecordFluxError()
    error.append(
        "undefined" + (f" {description}" if description else ""),
        Subsystem.MODEL,
        Severity.ERROR,
        location,
    )
    return error
