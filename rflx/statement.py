from abc import ABC

from rflx.common import generic_repr
from rflx.error import Location
from rflx.expression import Expr
from rflx.identifier import ID, StrID


class Statement(ABC):
    def __init__(self, location: Location = None):
        self.location = location

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return {k: v for k, v in self.__dict__.items() if k != "location"} == {
                k: v for k, v in other.__dict__.items() if k != "location"
            }
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)


class Assignment(Statement):
    def __init__(self, name: StrID, expression: Expr, location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)
        self.__expression = expression

    def __str__(self) -> str:
        return f"{self.__name} := {self.__expression}"


class Erase(Statement):
    def __init__(self, name: StrID, location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)

    def __str__(self) -> str:
        return f"{self.__name} := null"


class Reset(Statement):
    def __init__(self, name: StrID, location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)

    def __str__(self) -> str:
        return f"{self.__name}'Reset"
