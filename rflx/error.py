from __future__ import annotations

from collections import deque
from collections.abc import Sequence
from enum import Enum, auto
from pathlib import Path
from threading import local
from typing import NoReturn, Optional, TypeVar, Union

from rflx.common import Base, verbose_repr

ERROR_CONFIG = local()
ERROR_CONFIG.fail_after_value = 0


class Location(Base):
    def __init__(
        self,
        start: tuple[int, int],
        source: Optional[Path] = None,
        end: Optional[tuple[int, int]] = None,
        verbose: bool = False,
    ):
        self._source = source
        self._start = start
        self._end = end
        self._verbose = verbose

    def __str__(self) -> str:
        def linecol_str(linecol: tuple[int, int]) -> str:
            return f"{linecol[0]}:{linecol[1]}"

        start = f":{linecol_str(self._start)}"
        end = f"-{linecol_str(self._end)}" if self._end and self._verbose else ""
        return f"{self._source if self._source else '<stdin>'}{start}{end}"

    def __lt__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.start < other.start
        return NotImplemented

    @property
    def source(self) -> Optional[Path]:
        return self._source

    @property
    def start(self) -> tuple[int, int]:
        return self._start

    @property
    def end(self) -> Optional[tuple[int, int]]:
        return self._end

    def __hash__(self) -> int:
        return hash(self._start)


class Subsystem(Enum):
    PARSER = auto()
    MODEL = auto()
    GENERATOR = auto()
    CLI = auto()
    GRAPH = auto()
    PYRFLX = auto()
    ID = auto()
    VALIDATOR = auto()
    STYLE = auto()
    CONVERTER = auto()

    def __str__(self) -> str:
        return str.lower(self.name)


class Severity(Enum):
    NONE = 1
    INFO = 2
    WARNING = 3
    ERROR = 4

    def __str__(self) -> str:
        return str.lower(self.name)


Self = TypeVar("Self", bound="BaseError")


class BaseError(Exception, Base):
    class Entry(Base):
        def __init__(
            self,
            message: str,
            subsystem: Subsystem,
            severity: Severity,
            location: Optional[Location] = None,
        ):
            self._message = message
            self._subsystem = subsystem
            self._severity = severity
            self._location = location

        @property
        def message(self) -> str:
            return self._message

        @property
        def subsystem(self) -> Subsystem:
            return self._subsystem

        @property
        def severity(self) -> Severity:
            return self._severity

        @property
        def location(self) -> Optional[Location]:
            return self._location

    def __init__(
        self,
        entries: Optional[
            Union[Sequence[tuple[str, Subsystem, Severity, Optional[Location]]], BaseError]
        ] = None,
    ) -> None:
        super().__init__()
        self._messages: deque[BaseError.Entry] = deque()
        if entries:
            self._extend(entries)

    def __repr__(self) -> str:
        return verbose_repr(self, ["errors"])

    def __str__(self) -> str:
        def locn(entry: BaseError.Entry) -> str:
            if entry.location:
                return f"{entry.location}: "
            return ""

        return "\n".join(
            f"{locn(e)}{e.subsystem}: {e.severity}: {e.message}" for e in self._messages
        )

    def __add__(self: Self, other: object) -> Self:
        if isinstance(other, BaseError):
            error = self.__class__()
            error.extend(self)
            error.extend(other)
            return error
        return NotImplemented

    def __iadd__(self: Self, other: object) -> Self:
        if isinstance(other, BaseError):
            self.extend(other)
            return self
        return NotImplemented

    @property
    def errors(self) -> deque[BaseError.Entry]:
        return deque(e for e in self._messages if e.severity == Severity.ERROR)

    @property
    def messages(self) -> deque[BaseError.Entry]:
        return self._messages

    def extend(
        self,
        entries: Union[Sequence[tuple[str, Subsystem, Severity, Optional[Location]]], BaseError],
    ) -> None:
        self._extend(entries)
        num_errors = len(self.errors)
        # In some cases, accessing ERROR_CONFIG.fail_after_value will result in an AttributeError
        # if the code is executed inside a thread.
        if 0 < getattr(ERROR_CONFIG, "fail_after_value", 0) <= num_errors:
            raise self

    def propagate(self) -> None:
        if self.errors:
            raise self
        if self._messages:
            print(self)  # noqa: T201
            self._messages = deque()

    def fail(
        self,
        message: str,
        subsystem: Subsystem,
        severity: Severity = Severity.ERROR,
        location: Optional[Location] = None,
    ) -> NoReturn:
        self.extend(
            [
                (
                    message,
                    subsystem,
                    severity,
                    location,
                ),
            ],
        )
        raise self

    def _extend(
        self,
        entries: Union[Sequence[tuple[str, Subsystem, Severity, Optional[Location]]], BaseError],
    ) -> None:
        if isinstance(entries, BaseError):
            self._messages.extend(entries.messages)
        else:
            for message, subsystem, severity, location in entries:
                self._messages.append(BaseError.Entry(message, subsystem, severity, location))


class RecordFluxError(BaseError):
    """Error indicating an issue in an input or a known limitation."""


class FatalError(BaseError):
    """
    Error indicating a bug.

    This exception should never be caught outside of RecordFlux.
    """


def fail(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Optional[Location] = None,
) -> NoReturn:
    RecordFluxError().fail(message, subsystem, severity, location)


def fatal_fail(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Optional[Location] = None,
) -> NoReturn:
    FatalError().fail(message, subsystem, severity, location)


def warn(
    message: str,
    subsystem: Subsystem,
    location: Optional[Location] = None,
) -> None:
    print(RecordFluxError([(message, subsystem, Severity.WARNING, location)]))  # noqa: T201


def info(
    message: str,
    subsystem: Subsystem,
    location: Optional[Location] = None,
) -> None:
    print(RecordFluxError([(message, subsystem, Severity.INFO, location)]))  # noqa: T201
