from abc import abstractmethod
from collections import deque
from enum import Enum, auto
from pathlib import Path
from typing import Deque, List, NoReturn, Optional, Tuple, TypeVar, Union

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
    GENERATOR = auto()
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


Self = TypeVar("Self", bound="BaseError")


class BaseError(Exception, Base):
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

    @abstractmethod
    def __init__(self) -> None:
        super().__init__()
        self.__errors: Deque[BaseError.Entry] = deque()

    def __repr__(self) -> str:
        return verbose_repr(self, ["errors"])

    def __str__(self) -> str:
        def locn(entry: BaseError.Entry) -> str:
            if entry.location:
                return f"{entry.location}: "
            return ""

        return "\n".join(
            f"{locn(e)}{e.subsystem}: {e.severity}: {e.message}" for e in self.__errors
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
    def errors(self) -> Deque["BaseError.Entry"]:
        return self.__errors

    def append(
        self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
    ) -> None:
        self.__errors.append(BaseError.Entry(message, subsystem, severity, location))

    def appendleft(
        self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
    ) -> None:
        self.__errors.appendleft(BaseError.Entry(message, subsystem, severity, location))

    def extend(
        self,
        entries: Union[List[Tuple[str, Subsystem, Severity, Optional[Location]]], "BaseError"],
    ) -> None:
        if isinstance(entries, BaseError):
            self.__errors.extend(entries.errors)
        else:
            for message, subsystem, severity, location in entries:
                self.__errors.append(BaseError.Entry(message, subsystem, severity, location))

    def check(self) -> bool:
        return len(self.__errors) > 0

    def propagate(self) -> None:
        if self.check():
            raise self


class RecordFluxError(BaseError):
    """Error indicating an issue in an input or a known limitation."""

    def __init__(self) -> None:  # pylint: disable = useless-super-delegation
        super().__init__()


class FatalError(BaseError):
    """
    Error indicating a bug.

    This exception should never be caught outside of RecordFlux.
    """

    def __init__(self) -> None:  # pylint: disable = useless-super-delegation
        super().__init__()


def fail(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Location = None,
) -> NoReturn:
    _fail(RecordFluxError(), message, subsystem, severity, location)


def fatal_fail(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Location = None,
) -> NoReturn:
    _fail(FatalError(), message, subsystem, severity, location)


def _fail(
    error: BaseError,
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.ERROR,
    location: Location = None,
) -> NoReturn:
    error.append(message, subsystem, severity, location)
    error.propagate()
    assert False


def warn(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.WARNING,
    location: Location = None,
) -> None:
    e = RecordFluxError()
    e.append(message, subsystem, severity, location)
    print(e)
