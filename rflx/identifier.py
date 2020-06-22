from typing import Optional, Sequence, Union

from rflx.common import generic_repr
from rflx.contract import invariant
from rflx.error import Location


@invariant(lambda self: all(" " not in part for part in self.parts), "no whitespace in identifier")
@invariant(lambda self: "" not in self.parts, "no empty part in identifier")
class ID:
    def __init__(
        self, identifier: Union[str, Sequence[str], "ID"], location: Location = None
    ) -> None:
        self.parts: Sequence[str]
        self.location = location

        if isinstance(identifier, str):
            self.parts = identifier.split(".")
        elif isinstance(identifier, list):
            self.parts = identifier
        elif isinstance(identifier, ID):
            self.parts = list(identifier.parts)
            self.location = location or identifier.location
        else:
            assert False, f'unexpected identifier type "{type(identifier).__name__}"'

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
        return generic_repr(self.__class__.__name__, self.__dict__)

    def __str__(self) -> str:
        return ".".join(self.parts)

    def __add__(self, other: object) -> "ID":
        if isinstance(other, (str, ID)):
            return ID(f"{self}{other}", self.__location(other))
        return NotImplemented

    def __radd__(self, other: object) -> "ID":
        if isinstance(other, (str, ID)):
            return ID(f"{other}{self}", self.__location(other))
        return NotImplemented

    def __mul__(self, other: object) -> "ID":
        if isinstance(other, (str, ID)):
            if str(other) == "":
                return ID(self, self.__location(other))
            return ID(f"{self}.{other}", self.__location(other))
        return NotImplemented

    def __rmul__(self, other: object) -> "ID":
        if isinstance(other, (str, ID)):
            if str(other) == "":
                return ID(self, self.__location(other))
            return ID(f"{other}.{self}", self.__location(other))
        return NotImplemented

    def __location(self, other: object) -> Optional[Location]:
        if isinstance(other, str):
            return self.location
        if isinstance(other, ID):
            if self.location is None and other.location is None:
                return None
            if self.location is None:
                return other.location
        raise NotImplementedError

    @property
    def name(self) -> "ID":
        return ID(self.parts[-1])

    @property
    def parent(self) -> "ID":
        return ID(self.parts[:-1])


StrID = Union[str, ID]
