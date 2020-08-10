from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Mapping, Sequence

from rflx.common import generic_repr
from rflx.identifier import ID, StrID

if TYPE_CHECKING:
    from rflx.expression import Expr


class Declaration(ABC):
    def __init__(self, identifier: StrID) -> None:
        self.__identifier = ID(identifier)
        self.__refcount = 0

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

    def reference(self) -> None:
        self.__refcount += 1

    @property
    def identifier(self) -> ID:
        return self.__identifier

    @property
    def is_referenced(self) -> bool:
        return self.__refcount > 0

    @abstractmethod
    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        raise NotImplementedError


class VariableDeclaration(Declaration):
    def __init__(self, identifier: StrID, type_name: StrID = None, expression: "Expr" = None):
        super().__init__(identifier)
        self.__type_name = ID(type_name) if type_name else None
        self.__expression = expression

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass


class PrivateDeclaration(Declaration):
    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass


class Argument:
    def __init__(self, name: StrID, type_name: StrID):
        super().__init__()
        self.__name = ID(name)
        self.__type_name = ID(type_name)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass


class SubprogramDeclaration(Declaration):
    def __init__(self, identifier: StrID, arguments: Sequence[Argument], return_type: StrID):
        super().__init__(identifier)
        self.__arguments = arguments
        self.__return_type = ID(return_type)

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        for a in self.__arguments:
            a.validate(declarations)


class RenamingDeclaration(Declaration):
    def __init__(self, identifier: StrID, type_name: StrID, expression: "Expr"):
        super().__init__(identifier)
        self.__type_name = ID(type_name)
        self.__expression = expression

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        self.__expression.validate(declarations)


class ChannelDeclaration(Declaration):
    def __init__(self, identifier: StrID, readable: bool = False, writable: bool = False):
        super().__init__(identifier)
        self.__readable = readable
        self.__writable = writable

    @property
    def readable(self) -> bool:
        return self.__readable

    @property
    def writable(self) -> bool:
        return self.__writable

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass
