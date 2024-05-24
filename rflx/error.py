from __future__ import annotations

from typing import NoReturn, Optional, Sequence

from typing_extensions import TypeGuard

from rflx.rapidflux import ErrorEntry, Location as Location, RecordFluxError, Severity


class FatalError(Exception):
    """
    Error indicating a bug.

    This exception should never be caught outside of RecordFlux.
    """


def fail(
    message: str,
    severity: Severity = Severity.ERROR,
    location: Optional[Location] = None,
) -> NoReturn:
    raise RecordFluxError([ErrorEntry(message, severity, location)])


def fatal_fail(
    message: str,
    severity: Severity = Severity.ERROR,
    location: Optional[Location] = None,
) -> NoReturn:
    raise FatalError(str(RecordFluxError([ErrorEntry(message, severity, location)])))


def warn(
    message: str,
    location: Optional[Location] = None,
) -> None:
    RecordFluxError([ErrorEntry(message, Severity.WARNING, location)]).print_messages()


def info(
    message: str,
    location: Optional[Location] = None,
) -> None:
    RecordFluxError([ErrorEntry(message, Severity.INFO, location)]).print_messages()


def are_all_locations_present(
    locations: Sequence[Optional[Location]],
) -> TypeGuard[Sequence[Location]]:
    return all(l is not None for l in locations)
