from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Tuple

from pyparsing import col, lineno


class Location:
    def __init__(
        self,
        start: Tuple[int, int],
        filename: Path = None,
        end: Tuple[int, int] = None,
        verbose: bool = False,
    ):
        self.__filename = filename
        self.__start = start
        self.__end = end
        self.__verbose = verbose

    def __str__(self) -> str:
        def linecol_str(linecol: Tuple[int, int]) -> str:
            return f"{linecol[0]}:{linecol[1]}"

        start = f":{linecol_str(self.__start)}" if self.__start else ""
        end = f"-{linecol_str(self.__end)}" if self.__end and self.__verbose else ""
        return f"{self.__filename}{start}{end}"

    def set_filename(self, filename: Path) -> None:
        self.__filename = filename

    @property
    def get_filename(self) -> Optional[Path]:
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
            if entry.location and entry.location.get_filename:
                return f"{entry.location}: "
            return ""

        return "\n".join(
            f"{locn(e)}{e.subsystem}: {e.severity}: {e.message}" for e in self.__errors
        )

    def add(
        self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
    ) -> None:
        self.__errors.append(RecordFluxError.Entry(message, subsystem, severity, location))

    def raise_if_above(self, severity: Severity) -> None:
        if any([e.severity > severity for e in self.__errors]):
            raise self


class ParserError(RecordFluxError):
    def __init__(self, message: str = "parser error") -> None:
        super().__init__()
        self.add(message, Subsystem.PARSER, Severity.ERROR)
        self.raise_if_above(Severity.NONE)


class InternalError(RecordFluxError):
    def __init__(self, message: str = "internal error") -> None:
        super().__init__()
        self.add(message, Subsystem.INTERNAL, Severity.ERROR)
        self.raise_if_above(Severity.NONE)


class ModelError(RecordFluxError):
    def __init__(self, message: str = "model error") -> None:
        super().__init__()
        self.add(message, Subsystem.MODEL, Severity.ERROR)
        self.raise_if_above(Severity.NONE)


def fail(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Location = None,
) -> None:
    e = RecordFluxError()
    e.add(message, subsystem, severity, location)
    e.raise_if_above(Severity.NONE)


def parser_location(start: int, end: int, string: str) -> Location:
    return Location(
        start=(lineno(start, string), col(start, string)),
        end=(lineno(end, string), col(end, string)),
    )
