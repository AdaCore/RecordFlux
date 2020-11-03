from collections import deque
from enum import Enum, auto
from pathlib import Path
from typing import Deque, List, Optional, Tuple, Union

from pyparsing import col, lineno

from rflx.common import Base, verbose_repr


class Location(Base):
    def __init__(
        self,
        start: Tuple[int, int],
        source: Path = None,
        end: Tuple[int, int] = None,
        verbose: bool = False,
    ):
        self.__source = source
        self.__start = start
        self.__end = end
        self.__verbose = verbose

    def __str__(self) -> str:
        def linecol_str(linecol: Tuple[int, int]) -> str:
            return f"{linecol[0]}:{linecol[1]}"

        start = f":{linecol_str(self.__start)}"
        end = f"-{linecol_str(self.__end)}" if self.__end and self.__verbose else ""
        return f"{self.__source if self.__source else '<stdin>'}{start}{end}"

    def __lt__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.start < other.start
        return NotImplemented

    @property
    def source(self) -> Optional[Path]:
        return self.__source

    @property
    def start(self) -> Tuple[int, int]:
        return self.__start

    @property
    def end(self) -> Optional[Tuple[int, int]]:
        return self.__end


class Subsystem(Enum):
    PARSER = auto()
    MODEL = auto()
    CLI = auto()
    GRAPH = auto()
    PYRFLX = auto()
    ID = auto()

    def __str__(self) -> str:
        return str.lower(self.name)


class Severity(Enum):
    NONE = 1
    INFO = 2
    WARNING = 3
    ERROR = 4

    def __str__(self) -> str:
        return str.lower(self.name)


class RecordFluxError(Exception, Base):
    class Entry(Base):
        def __init__(
            self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
        ):
            self.__message = message
            self.__subsystem = subsystem
            self.__severity = severity
            self.__location = location

        @property
        def message(self) -> str:
            return self.__message

        @property
        def subsystem(self) -> Subsystem:
            return self.__subsystem

        @property
        def severity(self) -> Severity:
            return self.__severity

        @property
        def location(self) -> Optional[Location]:
            return self.__location

    def __init__(self) -> None:
        super().__init__()
        self.__errors: Deque[RecordFluxError.Entry] = deque()

    def __repr__(self) -> str:
        return verbose_repr(self, ["errors"])

    def __str__(self) -> str:
        def locn(entry: RecordFluxError.Entry) -> str:
            if entry.location:
                return f"{entry.location}: "
            return ""

        return "\n".join(
            f"{locn(e)}{e.subsystem}: {e.severity}: {e.message}" for e in self.__errors
        )

    def __add__(self, other: object) -> "RecordFluxError":
        if isinstance(other, RecordFluxError):
            error = RecordFluxError()
            error.extend(self)
            error.extend(other)
            return error
        return NotImplemented

    def __iadd__(self, other: object) -> "RecordFluxError":
        if isinstance(other, RecordFluxError):
            self.extend(other)
            return self
        return NotImplemented

    @property
    def errors(self) -> Deque["RecordFluxError.Entry"]:
        return self.__errors

    def append(
        self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
    ) -> None:
        self.__errors.append(RecordFluxError.Entry(message, subsystem, severity, location))

    def appendleft(
        self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
    ) -> None:
        self.__errors.appendleft(RecordFluxError.Entry(message, subsystem, severity, location))

    def extend(
        self,
        entries: Union[
            List[Tuple[str, Subsystem, Severity, Optional[Location]]], "RecordFluxError"
        ],
    ) -> None:
        if isinstance(entries, RecordFluxError):
            self.__errors.extend(entries.errors)
        else:
            for message, subsystem, severity, location in entries:
                self.__errors.append(RecordFluxError.Entry(message, subsystem, severity, location))

    def check(self) -> bool:
        return len(self.__errors) > 0

    def propagate(self) -> None:
        if self.check():
            raise self


def fail(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Location = None,
) -> None:
    e = RecordFluxError()
    e.append(message, subsystem, severity, location)
    e.propagate()


def parser_location(start: int, end: int, string: str, source: Path = None) -> Location:
    return Location(
        start=(lineno(start, string), col(start, string)),
        end=(lineno(end - 1, string), col(end - 1, string)),
        source=source,
    )
