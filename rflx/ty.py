from __future__ import annotations

from typing import Final, Union

from typing_extensions import TypeAlias

from rflx.rapidflux.ty import (
    Aggregate as Aggregate,
    Any as Any,
    AnyInteger as AnyInteger,
    Bounds as Bounds,
    Builtins as Builtins,
    Channel as Channel,
    Composite as Composite,
    Compound as Compound,
    Enumeration as Enumeration,
    Integer as Integer,
    Message as Message,
    Refinement as Refinement,
    Sequence as Sequence,
    Structure as Structure,
    Type as Type,
    Undefined as Undefined,
    UniversalInteger as UniversalInteger,
    check_type as check_type,
    check_type_instance as check_type_instance,
    common_type as common_type,
)

NamedType: TypeAlias = Union[Enumeration, Integer, Message, Sequence, Structure]
NamedTypeClass = (Enumeration, Integer, Message, Sequence, Structure)

UNDEFINED: Final = Builtins.UNDEFINED
BOOLEAN: Final = Builtins.BOOLEAN
INDEX: Final = Builtins.INDEX
BIT_LENGTH: Final = Builtins.BIT_LENGTH
BIT_INDEX: Final = Builtins.BIT_INDEX
UNIVERSAL_INTEGER: Final = Builtins.UNIVERSAL_INTEGER
BASE_INTEGER: Final = Builtins.BASE_INTEGER
OPAQUE: Final = Builtins.OPAQUE
