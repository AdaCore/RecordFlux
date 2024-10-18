from collections import abc
from typing import Final

from rflx.identifier import StrID
from rflx.rapidflux import ID, Annotation, Location, RecordFluxError

class Builtins:
    UNDEFINED: Final[Undefined]
    BOOLEAN: Final[Enumeration]
    INDEX: Final[Integer]
    BIT_LENGTH: Final[Integer]
    BIT_INDEX: Final[Integer]
    UNIVERSAL_INTEGER: Final[UniversalInteger]
    BASE_INTEGER: Final[Integer]
    OPAQUE: Final[Sequence]

class Type:
    DESCRIPTIVE_NAME: Final[str]

    def is_compatible(self, other: Type) -> bool: ...
    def is_compatible_strong(self, other: Type) -> bool: ...
    def common_type(self, other: Type) -> Type: ...

class Undefined(Type):
    def is_compatible(self, other: Type) -> bool: ...
    def is_compatible_strong(self, other: Type) -> bool: ...
    def common_type(self, other: Type) -> Type: ...

class Any(Type):
    def is_compatible(self, other: Type) -> bool: ...
    def is_compatible_strong(self, other: Type) -> bool: ...
    def common_type(self, other: Type) -> Type: ...

class Enumeration(Any):
    def __init__(
        self,
        identifier: StrID,
        literals: abc.Sequence[ID],
        always_valid: bool = False,
        location: Location | None = None,
    ) -> None: ...
    @property
    def identifier(self) -> ID: ...
    @property
    def literals(self) -> abc.Sequence[ID]: ...
    @property
    def always_valid(self) -> bool: ...
    @property
    def location(self) -> Location: ...

class AnyInteger(Any):
    @property
    def bounds(self) -> Bounds: ...

class UniversalInteger(AnyInteger):
    def __init__(self, bounds: Bounds) -> None: ...

class Integer(AnyInteger):
    def __init__(
        self,
        identifier: StrID,
        bounds: Bounds,
        location: Location | None = None,
    ) -> None: ...
    @property
    def identifier(self) -> ID: ...
    @property
    def location(self) -> Location: ...

class Composite(Any):
    @property
    def element(self) -> Type: ...

class Aggregate(Composite):
    def __init__(
        self,
        element: Type,
    ) -> None: ...

class Sequence(Composite):
    def __init__(
        self,
        identifier: StrID,
        element: Type,
    ) -> None: ...
    @property
    def identifier(self) -> ID: ...

class Refinement:
    def __init__(
        self,
        field: StrID,
        sdu: Message,
        package: StrID,
    ) -> None: ...
    @property
    def field(self) -> ID: ...
    @property
    def sdu(self) -> Message: ...
    @property
    def package(self) -> ID: ...

class Compound(Any):
    @property
    def identifier(self) -> ID: ...
    @property
    def field_combinations(self) -> set[tuple[str, ...]]: ...
    @property
    def parameter_types(self) -> abc.Mapping[ID, Type]: ...
    @property
    def field_types(self) -> abc.Mapping[ID, Type]: ...
    @property
    def parameters(self) -> set[ID]: ...
    @property
    def fields(self) -> set[ID]: ...
    @property
    def types(self) -> abc.Mapping[ID, Type]: ...

class Structure(Compound):
    def __init__(
        self,
        identifier: StrID,
        field_combinations: set[tuple[str, ...]] | None = None,
        parameter_types: abc.Mapping[ID, Type] | None = None,
        field_types: abc.Mapping[ID, Type] | None = None,
    ) -> None: ...

class Message(Compound):
    def __init__(  # noqa: PLR0913
        self,
        identifier: StrID,
        field_combinations: set[tuple[str, ...]] | None = None,
        parameter_types: abc.Mapping[ID, Type] | None = None,
        field_types: abc.Mapping[ID, Type] | None = None,
        refinements: abc.Sequence[Refinement] | None = None,
        is_definite: bool = False,
    ) -> None: ...
    @property
    def refinements(self) -> abc.Sequence[Refinement]: ...
    @property
    def is_definite(self) -> bool: ...

class Channel(Any):
    def __init__(self, readable: bool, writable: bool) -> None: ...
    @property
    def readable(self) -> bool: ...
    @property
    def writable(self) -> bool: ...

class Bounds:
    def __init__(self, lower: int, upper: int) -> None: ...
    def __contains__(self, item: int | Bounds) -> bool: ...
    @property
    def lower(self) -> int: ...
    @property
    def upper(self) -> int: ...
    def merge(self, bounds: Bounds) -> Bounds: ...

def common_type(types: abc.Sequence[Type]) -> Type: ...
def check_type(
    actual: Type,
    expected: Type | tuple[Type, ...],
    location: Location,
    description: str,
) -> RecordFluxError: ...
def check_type_instance(
    actual: Type,
    expected: type[Type] | tuple[type[Type], ...],
    location: Location,
    description: str = "",
    additional_annotations: abc.Sequence[Annotation] | None = None,
) -> RecordFluxError: ...
