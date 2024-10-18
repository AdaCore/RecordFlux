from __future__ import annotations

from collections.abc import Sequence
from typing import NoReturn

from typing_extensions import TypeGuard

from rflx.rapidflux import (
    ErrorEntry,
    FatalError as FatalError,
    Location as Location,
    RecordFluxError,
    Severity,
)


def fail(
    message: str,
    severity: Severity = Severity.ERROR,
    location: Location | None = None,
) -> NoReturn:
    raise RecordFluxError([ErrorEntry(message, severity, location)])


def fatal_fail(
    message: str,
    severity: Severity = Severity.ERROR,
    location: Location | None = None,
) -> NoReturn:
    raise FatalError(str(RecordFluxError([ErrorEntry(message, severity, location)])))


def warn(
    message: str,
    location: Location | None = None,
) -> None:
    RecordFluxError([ErrorEntry(message, Severity.WARNING, location)]).print_messages()


def info(
    message: str,
    location: Location | None = None,
) -> None:
    RecordFluxError([ErrorEntry(message, Severity.INFO, location)]).print_messages()


def are_all_locations_present(
    locations: Sequence[Location | None],
) -> TypeGuard[Sequence[Location]]:
    return all(l is not None for l in locations)
