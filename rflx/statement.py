from abc import ABC
from typing import Mapping

from rflx.common import generic_repr
from rflx.error import Location, Severity, Subsystem, fail
from rflx.expression import Declaration, Expr
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

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        raise NotImplementedError


class Assignment(Statement):
    def __init__(self, name: StrID, expression: Expr, location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)
        self.__expression = expression

    def __str__(self) -> str:
        return f"{self.__name} := {self.__expression}"

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if self.__name not in declarations:
            fail(
                f'assignment to undeclared variable "{self.__name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        self.__expression.simplified().validate(declarations)


class Erase(Statement):
    def __init__(self, name: StrID, location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)

    def __str__(self) -> str:
        return f"{self.__name} := null"

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if self.__name not in declarations:
            fail(
                f'erasure of undeclared variable "{self.__name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )


class Reset(Statement):
    def __init__(self, name: StrID, location: Location = None) -> None:
        super().__init__(location)
        self.__name = ID(name)

    def __str__(self) -> str:
        return f"{self.__name}'Reset"

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if self.__name not in declarations:
            fail(
                f'reset of undeclared variable "{self.__name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
