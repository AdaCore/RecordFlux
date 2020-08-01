from abc import ABC
from typing import Mapping

from rflx.common import generic_repr
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.expression import Declaration, Expr
from rflx.identifier import ID, StrID


class Statement(ABC):
    def __init__(self, name: StrID, location: Location = None):
        self.name = ID(name)
        self.location = location
        self.error = RecordFluxError()

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
        super().__init__(name, location)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.name} := {self.expression}"

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if self.name not in declarations:
            self.error.append(
                f'assignment to undeclared variable "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        else:
            declarations[self.name].reference()
        try:
            self.expression.validate(declarations)
        except RecordFluxError as e:
            self.error.extend(e)
        self.error.propagate()


class Erase(Statement):
    def __str__(self) -> str:
        return f"{self.name} := null"

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if self.name not in declarations:
            fail(
                f'erasure of undeclared variable "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        declarations[self.name].reference()


class Reset(Statement):
    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if self.name not in declarations:
            fail(
                f'reset of undeclared variable "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        declarations[self.name].reference()

    def __str__(self) -> str:
        return f"{self.name}'Reset"
