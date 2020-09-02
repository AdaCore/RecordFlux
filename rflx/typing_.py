import typing as ty
from abc import abstractmethod
from dataclasses import dataclass


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
    def __str__(self) -> str:
        return "undefined"

    def is_compatible(self, other: Type) -> bool:
        return False

    def common_type(self, other: Type) -> Type:
        return self


@dataclass(frozen=True)
class Any(Type):
    def __str__(self) -> str:
        return "any type"

    def is_compatible(self, other: Type) -> bool:
        return not isinstance(other, Undefined)

    def common_type(self, other: Type) -> Type:
        return other


@dataclass(frozen=True)
class Enumeration(Any):
    name: str

    def __str__(self) -> str:
        return f'enumeration type "{self.name}"'

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or (isinstance(other, Enumeration) and self.name == other.name)

    def common_type(self, other: Type) -> Type:
        if other == Any() or self == other:
            return self
        return Undefined()


BOOLEAN = Enumeration("Boolean")


@dataclass(frozen=True)
class AnyInteger(Any):
    def __str__(self) -> str:
        return "integer type"

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
    bounds: Bounds = Bounds(None, None)

    def __str__(self) -> str:
        return f"type universal integer ({self.bounds})"

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
    name: str
    bounds: Bounds = Bounds(None, None)

    def __str__(self) -> str:
        return f'integer type "{self.name}" ({self.bounds})'

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


@dataclass(frozen=True)
class Aggregate(Any):
    element: Type

    def __str__(self) -> str:
        return f"aggregate with element {self.element}"

    def is_compatible(self, other: Type) -> bool:
        return (
            other == Any()
            or isinstance(other, Aggregate)
            or (isinstance(other, Composite) and self.element.is_compatible_strong(other.element))
        )

    def common_type(self, other: Type) -> Type:
        if isinstance(other, Composite) and self.element.is_compatible_strong(other.element):
            return other
        if isinstance(other, Aggregate) and self.element != other.element:
            return Aggregate(self.element.common_type(other.element))
        if other == Any() or self == other:
            return self
        return Undefined()


@dataclass(frozen=True)
class Composite(Any):
    name: str
    element: Type

    def __str__(self) -> str:
        return f'composite type "{self.name}" with element {self.element}'

    def is_compatible(self, other: Type) -> bool:
        return (
            other == Any()
            or (isinstance(other, Aggregate) and other.element.is_compatible_strong(self.element))
            or (
                isinstance(other, Composite)
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


@dataclass(frozen=True)
class Message(Any):
    name: str

    def __str__(self) -> str:
        return f'message type "{self.name}"'

    def is_compatible(self, other: Type) -> bool:
        return other == Any() or (isinstance(other, Message) and self.name == other.name)

    def common_type(self, other: Type) -> Type:
        if other == Any() or self == other:
            return self
        return Undefined()


def common_type(types: ty.Sequence[Type]) -> Type:
    result: Type = Any()

    for t in types:
        result = result.common_type(t)

    return result
