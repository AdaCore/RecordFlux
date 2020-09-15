from typing import Optional, Sequence, Union

from rflx.error import Location


class ID:
    def __init__(
        self, identifier: Union[str, Sequence[str], "ID"], location: Location = None
    ) -> None:
        self._parts: Sequence[str]
        self.location = location

        if isinstance(identifier, str):
            self._parts = identifier.split(self._separator)
        elif isinstance(identifier, list):
            self._parts = identifier
        elif isinstance(identifier, ID):
            self._parts = list(identifier.parts)
            self.location = location or identifier.location
        else:
            assert False, f'unexpected identifier type "{type(identifier).__name__}"'

        assert self._parts, "empty identifier"
        assert "" not in self._parts, "empty part in identifier"
        for c in [" ", ".", ":"]:
            assert all(c not in part for part in self._parts), f'"{c}" in identifier parts'

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.parts == other.parts
        return NotImplemented

    def __lt__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return str(self) < str(other)
        return NotImplemented

    def __hash__(self) -> int:
        return hash(tuple(self.parts))

    def __repr__(self) -> str:
        return f'ID("{self}")'

    def __str__(self) -> str:
        return self._separator.join(self.parts)

    def __add__(self, other: object) -> "ID":
        if isinstance(other, (str, ID)):
            return self.__class__(f"{self}{other}", self.__location(other))
        return NotImplemented

    def __radd__(self, other: object) -> "ID":
        if isinstance(other, (str, ID)):
            return self.__class__(f"{other}{self}", self.__location(other))
        return NotImplemented

    def __mul__(self, other: object) -> "ID":
        if isinstance(other, (str, ID)):
            if str(other) == "":
                return self.__class__(self, self.__location(other))
            return self.__class__(f"{self}{self._separator}{other}", self.__location(other))
        return NotImplemented

    def __rmul__(self, other: object) -> "ID":
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
    def name(self) -> "ID":
        return self.__class__(self._parts[-1])

    @property
    def parent(self) -> "ID":
        return self.__class__(self._parts[:-1])

    @property
    def _separator(self) -> str:
        return "::"


StrID = Union[str, ID]
