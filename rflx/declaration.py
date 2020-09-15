from abc import abstractmethod
from typing import Sequence

from rflx.common import Base
from rflx.error import Location
from rflx.expression import Expr, Variable
from rflx.identifier import ID, StrID


class Declaration(Base):
    def __init__(self, identifier: StrID, location: Location = None):
        self.__identifier = ID(identifier)
        self.location = location
        self.__refcount = 0

    def reference(self) -> None:
        self.__refcount += 1

    @property
    def identifier(self) -> ID:
        return self.__identifier

    @property
    def is_referenced(self) -> bool:
        return self.__refcount > 0

    @abstractmethod
    def variables(self) -> Sequence[Variable]:
        raise NotImplementedError


class VariableDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        type_name: StrID,
        expression: Expr = None,
        location: Location = None,
    ):
        super().__init__(identifier, location)
        self.__type_name = ID(type_name)
        self.__expression = expression

    def __str__(self) -> str:
        expression = f" := {self.__expression}" if self.__expression else ""
        return f"{self.identifier} : {self.__type_name}{expression}"

    @property
    def type_name(self) -> ID:
        return self.__type_name

    def variables(self) -> Sequence[Variable]:
        if self.__expression:
            return self.__expression.variables()
        return []


class PrivateDeclaration(Declaration):
    def __str__(self) -> str:
        return f"type {self.identifier} is private"

    def variables(self) -> Sequence[Variable]:
        return []


class Argument(Base):
    def __init__(self, name: StrID, type_name: StrID):
        super().__init__()
        self.__name = ID(name)
        self.__type_name = ID(type_name)

    def __str__(self) -> str:
        return f"{self.__name} : {self.__type_name}"


class SubprogramDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        arguments: Sequence[Argument],
        return_type: StrID,
        location: Location = None,
    ):
        super().__init__(identifier, location)
        self.__arguments = arguments
        self.__return_type = ID(return_type)

    def __str__(self) -> str:
        arguments = (" (" + "; ".join(map(str, self.__arguments)) + ")") if self.__arguments else ""
        return f"with function {self.identifier}{arguments} return {self.__return_type}"

    def variables(self) -> Sequence[Variable]:
        return []


class RenamingDeclaration(Declaration):
    def __init__(
        self, identifier: StrID, type_name: StrID, expression: "Expr", location: Location = None
    ):
        super().__init__(identifier, location)
        self.__type_name = ID(type_name)
        self.__expression = expression

    def __str__(self) -> str:
        return f"{self.identifier} : {self.__type_name} renames {self.__expression}"

    def variables(self) -> Sequence[Variable]:
        return self.__expression.variables()


class ChannelDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        readable: bool = False,
        writable: bool = False,
        location: Location = None,
    ):
        assert readable or writable
        super().__init__(identifier, location)
        self.__readable = readable
        self.__writable = writable

    def __str__(self) -> str:
        aspects = []
        if self.__readable:
            aspects.append("Readable")
        if self.__writable:
            aspects.append("Writable")
        with_aspects = " with " + ", ".join(aspects)
        return f"{self.identifier} : Channel{with_aspects}"

    @property
    def readable(self) -> bool:
        return self.__readable

    @property
    def writable(self) -> bool:
        return self.__writable

    def variables(self) -> Sequence[Variable]:
        return []
