from abc import abstractmethod
from collections import deque
from enum import Enum, auto
from pathlib import Path
from threading import local
from typing import Deque, List, NoReturn, Optional, Tuple, TypeVar, Union

from rflx.common import Base, verbose_repr

ERROR_CONFIG = local()
ERROR_CONFIG.fail_after_value = 0


class Location(Base):
    def __init__(
        self,
        start: Tuple[int, int],
        source: Path = None,
        end: Tuple[int, int] = None,
        verbose: bool = False,
    ):
        self._source = source
        self._start = start
        self._end = end
        self._verbose = verbose

    def __str__(self) -> str:
        def linecol_str(linecol: Tuple[int, int]) -> str:
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
    def start(self) -> Tuple[int, int]:
        return self._start

    @property
    def end(self) -> Optional[Tuple[int, int]]:
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
            self, message: str, subsystem: Subsystem, severity: Severity, location: Location = None
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

    @abstractmethod
    def __init__(self) -> None:
        super().__init__()
        self._errors: Deque[BaseError.Entry] = deque()

    def __repr__(self) -> str:
        return verbose_repr(self, ["errors"])

    def __str__(self) -> str:
        def locn(entry: BaseError.Entry) -> str:
            if entry.location:
                return f"{entry.location}: "
            return ""

        return "\n".join(f"{locn(e)}{e.subsystem}: {e.severity}: {e.message}" for e in self._errors)

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
        return self._errors

    def extend(
        self,
        entries: Union[List[Tuple[str, Subsystem, Severity, Optional[Location]]], "BaseError"],
    ) -> None:
        if isinstance(entries, BaseError):
            self._errors.extend(entries.errors)
        else:
            for message, subsystem, severity, location in entries:
                self._errors.append(BaseError.Entry(message, subsystem, severity, location))
        num_errors = len(list(e for e in self._errors if e.severity == Severity.ERROR))
        if 0 < ERROR_CONFIG.fail_after_value <= num_errors:
            raise self

    def check(self) -> bool:
        return len(self._errors) > 0

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
    error.extend([(message, subsystem, severity, location)])
    error.propagate()
    assert False


def warn(
    message: str,
    subsystem: Subsystem,
    severity: Severity = Severity.WARNING,
    location: Location = None,
) -> None:
    e = RecordFluxError()
    e.extend([(message, subsystem, severity, location)])
    print(e)
