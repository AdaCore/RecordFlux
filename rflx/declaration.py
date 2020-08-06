from abc import ABC
from typing import TYPE_CHECKING, Mapping, Sequence

from rflx.common import generic_repr
from rflx.identifier import ID, StrID

if TYPE_CHECKING:
    from rflx.expression import Expr


class Declaration(ABC):
    def __init__(self) -> None:
        self.__refcount = 0

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

    def reference(self) -> None:
        self.__refcount += 1

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        raise NotImplementedError(f"Validation not implemented for {type(self).__name__}")

    @property
    def is_referenced(self) -> bool:
        return self.__refcount > 0


class Argument(Declaration):
    def __init__(self, name: StrID, typ: StrID):
        super().__init__()
        self.__name = ID(name)
        self.__type = ID(typ)

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass


class VariableDeclaration(Declaration):
    def __init__(self, typ: StrID = None, init: "Expr" = None):
        super().__init__()
        self.__type = ID(typ) if typ else None
        self.__init = init

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass


class PrivateDeclaration(Declaration):
    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass


class Subprogram(Declaration):
    def __init__(self, arguments: Sequence[Argument], return_type: StrID):
        super().__init__()
        self.__arguments = arguments
        self.__return_type = ID(return_type)

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        for a in self.__arguments:
            a.validate(declarations)


class Renames(Declaration):
    def __init__(self, typ: StrID, expr: "Expr"):
        super().__init__()
        self.__type = ID(typ)
        self.__expr = expr

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        self.__expr.validate(declarations)


class Channel(Declaration):
    def __init__(self, read: bool, write: bool):
        super().__init__()
        self.__read = read
        self.__write = write

    @property
    def readable(self) -> bool:
        return self.__read

    @property
    def writable(self) -> bool:
        return self.__write

    def validate(self, declarations: Mapping[ID, "Declaration"]) -> None:
        pass
