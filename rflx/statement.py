from abc import ABC

from rflx.error import Location
from rflx.expression import Expr, Variable


class Statement(ABC):
    def __init__(self, location: Location = None):
        self.location = location

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented


class Assignment(Statement):
    def __init__(self, name: Variable, expression: Expr, location: Location = None) -> None:
        super().__init__(location)
        self.__name = name
        self.__expression = expression

    def __str__(self) -> str:
        return f"{self.__name} := {self.__expression}"
