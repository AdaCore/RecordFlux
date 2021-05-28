import re
from typing import Optional, Sequence, TypeVar, Union

from rflx.error import Location, RecordFluxError, Severity, Subsystem

Self = TypeVar("Self", bound="ID")


class ID:
    def __init__(
        self, identifier: Union[str, Sequence[str], "ID"], location: Location = None
    ) -> None:
        self._parts: Sequence[str]
        self.location = location

        if isinstance(identifier, str):
            self._parts = re.split(r"\.|::", identifier)
        elif isinstance(identifier, list):
            self._parts = identifier
        elif isinstance(identifier, ID):
            self._parts = list(identifier.parts)
            self.location = location or identifier.location
        else:
            assert False, f'unexpected identifier type "{type(identifier).__name__}"'

        error = RecordFluxError()
        if not self._parts:
            error.append("empty identifier", Subsystem.ID, Severity.ERROR, location)
        elif "" in self._parts:
            error.append(
                f'empty part in identifier "{self}"', Subsystem.ID, Severity.ERROR, location
            )
        else:
            for c in [" ", ".", ":"]:
                if any(c in part for part in self._parts):
                    error.append(
                        f'"{c}" in identifier parts of "{self}"',
                        Subsystem.ID,
                        Severity.ERROR,
                        location,
                    )
        error.propagate()

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            if len(self.parts) != len(other.parts):
                return False
            return all(s.lower() == o.lower() for s, o in zip(self.parts, other.parts))
        return NotImplemented

    def __lt__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return str(self) < str(other)
        return NotImplemented

    def __hash__(self) -> int:
        return hash(tuple(map(str.lower, self.parts)))

    def __repr__(self) -> str:
        return f'ID("{self}")'

    def __str__(self) -> str:
        return self._separator.join(self.parts)

    def __add__(self: Self, other: object) -> Self:
        if isinstance(other, (str, ID)):
            return self.__class__(f"{self}{other}", self.__location(other))
        return NotImplemented

    def __radd__(self: Self, other: object) -> Self:
        if isinstance(other, (str, ID)):
            return self.__class__(f"{other}{self}", self.__location(other))
        return NotImplemented

    def __mul__(self: Self, other: object) -> Self:
        if isinstance(other, (str, ID)):
            if str(other) == "":
                return self.__class__(self, self.__location(other))
            return self.__class__(f"{self}{self._separator}{other}", self.__location(other))
        return NotImplemented

    def __rmul__(self: Self, other: object) -> Self:
        if isinstance(other, (str, ID)):
            if str(other) == "":
                return self.__class__(self, self.__location(other))
            return self.__class__(f"{other}{self._separator}{self}", self.__location(other))
        return NotImplemented

    def __location(self, other: object) -> Optional[Location]:
        if isinstance(other, str):
            return self.location
        if isinstance(other, ID):
            if self.location is None and other.location is None:
                return None
            if self.location is None:
                return other.location
            return self.location
        raise NotImplementedError

    @property
    def parts(self) -> Sequence[str]:
        return self._parts

    @property
    def name(self: Self) -> Self:
        return self.__class__(self._parts[-1])

    @property
    def parent(self: Self) -> Self:
        return self.__class__(self._parts[:-1])

    @property
    def flat(self: Self) -> str:
        return "_".join(self._parts)

    @property
    def _separator(self) -> str:
        return "::"


StrID = Union[str, ID]
