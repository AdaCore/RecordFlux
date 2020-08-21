from abc import ABC
from typing import Mapping, Sequence

from rflx.common import generic_eq, generic_repr
from rflx.declaration import Declaration
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.expression import Expr
from rflx.identifier import ID, StrID


class Statement(ABC):
    def __init__(self, name: StrID, location: Location = None):
        self.name = ID(name)
        self.location = location
        self.error = RecordFluxError()

    def __eq__(self, other: object) -> bool:
        return generic_eq(self, other)

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


class AttributeStatement(Statement):
    def __init__(
        self, name: StrID, attribute: str, parameters: Sequence[Expr], location: Location = None
    ) -> None:
        super().__init__(name, location)
        self.attribute = attribute
        self.parameters = parameters

    def __str__(self) -> str:
        parameters = ", ".join(str(p) for p in self.parameters)
        return f"{self.name}'{self.attribute}" + (f" ({parameters})" if parameters else "")


class ListAttributeStatement(AttributeStatement, ABC):
    def __init__(self, name: StrID, parameter: Expr, location: Location = None) -> None:
        super().__init__(name, self.__class__.__name__, [parameter], location)


class Append(ListAttributeStatement):
    pass


class Extend(ListAttributeStatement):
    pass


class Reset(AttributeStatement):
    def __init__(self, name: StrID, location: Location = None) -> None:
        super().__init__(name, self.__class__.__name__, [], location)

    def validate(self, declarations: Mapping[ID, Declaration]) -> None:
        if self.name not in declarations:
            fail(
                f'reset of undeclared variable "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        declarations[self.name].reference()
