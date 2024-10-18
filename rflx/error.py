from __future__ import annotations

from typing import NoReturn

from rflx.rapidflux import (
    NO_LOCATION,
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
    raise RecordFluxError(
        [ErrorEntry(message, severity, NO_LOCATION if location is None else location)],
    )


def fatal_fail(
    message: str,
    severity: Severity = Severity.ERROR,
    location: Location | None = None,
) -> NoReturn:
    raise FatalError(
        str(
            RecordFluxError(
                [ErrorEntry(message, severity, NO_LOCATION if location is None else location)],
            ),
        ),
    )


def warn(
    message: str,
    location: Location | None = None,
) -> None:
    RecordFluxError(
        [ErrorEntry(message, Severity.WARNING, NO_LOCATION if location is None else location)],
    ).print_messages()


def info(
    message: str,
    location: Location | None = None,
) -> None:
    RecordFluxError(
        [ErrorEntry(message, Severity.INFO, NO_LOCATION if location is None else location)],
    ).print_messages()
