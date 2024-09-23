from __future__ import annotations

from collections import abc
from typing import Final, Union

from typing_extensions import TypeAlias

from rflx.rapidflux import Annotation, ErrorEntry, Location, RecordFluxError, Severity
from rflx.rapidflux.ty import (
    Aggregate as Aggregate,
    Any as Any,
    AnyInteger as AnyInteger,
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
)

NamedType: TypeAlias = Union[Enumeration, Integer, Message, Sequence, Structure]
NamedTypeClass = (Enumeration, Integer, Message, Sequence, Structure)

UNDEFINED: Final = Undefined()
BOOLEAN: Final = Builtins.BOOLEAN
INDEX: Final = Builtins.INDEX
BIT_LENGTH: Final = Builtins.BIT_LENGTH
BIT_INDEX: Final = Builtins.BIT_INDEX
UNIVERSAL_INTEGER: Final = Builtins.UNIVERSAL_INTEGER
BASE_INTEGER: Final = Builtins.BASE_INTEGER
OPAQUE: Final = Builtins.OPAQUE


def common_type(types: abc.Sequence[Type]) -> Type:
    result: Type = Any()

    for t in types:
        result = result.common_type(t)

    return result


def check_type(
    actual: Type,
    expected: Type | tuple[Type, ...],
    location: Location | None,
    description: str,
) -> RecordFluxError:
    assert expected, "empty expected types"

    if actual == Undefined():
        return _undefined_type(location, description)

    error = RecordFluxError()

    expected_types = [expected] if isinstance(expected, Type) else list(expected)

    if Undefined() not in [actual, *expected_types] and all(
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
    expected: type[Type] | tuple[type[Type], ...],
    location: Location | None,
    description: str = "",
    additionnal_annotations: abc.Sequence[Annotation] | None = None,
) -> RecordFluxError:
    assert expected, "empty expected types"

    if actual == Undefined():
        return _undefined_type(location, description)

    error = RecordFluxError()

    if not isinstance(actual, expected) and actual != Any():
        additionnal_annotations = additionnal_annotations or []
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
                annotations=[
                    Annotation(f"found {actual}", Severity.ERROR, location),
                    *additionnal_annotations,
                ],
                generate_default_annotation=False,
            ),
        )

    return error


def _undefined_type(location: Location | None, description: str = "") -> RecordFluxError:
    return RecordFluxError(
        [
            ErrorEntry(
                "undefined" + (f" {description}" if description else ""),
                Severity.ERROR,
                location,
            ),
        ],
    )
