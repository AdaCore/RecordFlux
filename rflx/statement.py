from abc import abstractmethod
from typing import Sequence

from rflx.common import Base
from rflx.error import Location, RecordFluxError
from rflx.expression import Expr, Variable
from rflx.identifier import ID, StrID


class Statement(Base):
    def __init__(self, name: StrID, location: Location = None):
        self.name = ID(name)
        self.location = location
        self.error = RecordFluxError()

    @abstractmethod
    def variables(self) -> Sequence[Variable]:
        raise NotImplementedError


class Assignment(Statement):
    def __init__(self, name: StrID, expression: Expr, location: Location = None) -> None:
        super().__init__(name, location)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.name} := {self.expression}"

    def variables(self) -> Sequence[Variable]:
        return [Variable(self.name), *self.expression.variables()]


class Erase(Statement):
    def __str__(self) -> str:
        return f"{self.name} := null"

    def variables(self) -> Sequence[Variable]:
        return [Variable(self.name)]


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

    def variables(self) -> Sequence[Variable]:
        return [Variable(self.name), *[e for p in self.parameters for e in p.variables()]]


class ListAttributeStatement(AttributeStatement):
    def __init__(self, name: StrID, parameter: Expr, location: Location = None) -> None:
        super().__init__(name, self.__class__.__name__, [parameter], location)


class Append(ListAttributeStatement):
    pass


class Extend(ListAttributeStatement):
    pass


class Reset(AttributeStatement):
    def __init__(self, name: StrID, location: Location = None) -> None:
        super().__init__(name, self.__class__.__name__, [], location)

    def variables(self) -> Sequence[Variable]:
        return [Variable(self.name)]
