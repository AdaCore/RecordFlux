from enum import Enum, auto
from pathlib import Path
from typing import Callable, List, Optional, Tuple

from pyparsing import col, lineno

from rflx.common import generic_repr

__current_source: List[Path] = []


def current_source() -> Optional[Path]:
    if __current_source:
        return __current_source[-1]
    return None


class Location:
    def __init__(
        self,
        start: Tuple[int, int],
        filename: Path = None,
        end: Tuple[int, int] = None,
        verbose: bool = False,
    ):
        self.__filename: Optional[Path]

        if filename:
            self.__filename = filename
        else:
            self.__filename = current_source()

        self.__start = start
        self.__end = end
        self.__verbose = verbose

    def __str__(self) -> str:
        def linecol_str(linecol: Tuple[int, int]) -> str:
            return f"{linecol[0]}:{linecol[1]}"

        start = f":{linecol_str(self.__start)}" if self.__start else ""
        end = f"-{linecol_str(self.__end)}" if self.__end and self.__verbose else ""
        return f"{self.__filename if self.__filename else '<stdin>'}{start}{end}"

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self) -> int:
        return hash(f"{self.__start}:{self.__filename}:{self.__end}")

    @property
    def source(self) -> Optional[Path]:
        return self.__filename

    @property
    def start(self) -> Tuple[int, int]:
        return self.__start

    @property
    def end(self) -> Optional[Tuple[int, int]]:
        return self.__end


class Subsystem(Enum):
    CORE = auto()
    PARSER = auto()
    MODEL = auto()
    CLI = auto()
    INTERNAL = auto()
    GRAPH = auto()

    def __str__(self) -> str:
        return str.lower(self.name)


class Severity(Enum):
    NONE = 1
    INFO = 2
    WARNING = 3
    ERROR = 4

    def __gt__(self, other: "Severity") -> bool:
        # ISSUE: PyCQA/pylint#2306
        value = int(self.value)
        return value > other.value

    def __str__(self) -> str:
        return str.lower(self.name)


class RecordFluxError(Exception):
    class Entry:
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
        self.__errors: List[RecordFluxError.Entry] = []

    def __str__(self) -> str:
        def locn(entry: RecordFluxError.Entry) -> str:
            if entry.location:
                return f"{entry.location}: "
            return ""

        return "\n".join(
            f"{locn(e)}{e.subsystem}: {e.severity}: {e.message}" for e in self.__errors
        )

    def append(
        self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
    ) -> None:
        self.__errors.append(RecordFluxError.Entry(message, subsystem, severity, location))

    def extend(self, entries: List[Tuple[str, Subsystem, Severity, Optional[Location]]]) -> None:
        for message, subsystem, severity, location in entries:
            self.__errors.append(RecordFluxError.Entry(message, subsystem, severity, location))

    def propagate(
        self, func: Callable[["RecordFluxError.Entry"], bool] = lambda e: e.severity > Severity.NONE
    ) -> None:
        if any([func(e) for e in self.__errors]):
            raise self


class InternalError(RecordFluxError):
    def __init__(self, message: str = "internal error") -> None:
        super().__init__()
        self.append(message, Subsystem.INTERNAL, Severity.ERROR)
        self.propagate()


class ModelError(RecordFluxError):
    def __init__(self, message: str = "model error") -> None:
        super().__init__()
        self.append(message, Subsystem.MODEL, Severity.ERROR)
        self.propagate()


def fail(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Location = None,
) -> None:
    e = RecordFluxError()
    e.append(message, subsystem, severity, location)
    e.propagate()


def parser_location(start: int, end: int, string: str, filename: Path = None) -> Location:
    return Location(
        start=(lineno(start, string), col(start, string)),
        end=(lineno(end - 1, string), col(end - 1, string)),
        filename=filename,
    )


def push_source(source: Path) -> None:
    __current_source.append(source)


def pop_source() -> None:
    __current_source.pop()
