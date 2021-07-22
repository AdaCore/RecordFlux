from abc import abstractmethod
from typing import TYPE_CHECKING, Callable, ClassVar, Sequence

import rflx.typing_ as rty
from rflx.common import Base
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.expression import Expr, Selected, Variable
from rflx.identifier import ID, StrID

if TYPE_CHECKING:
    import rflx.model.type_ as mty


class Declaration(Base):
    DESCRIPTIVE_NAME: ClassVar[str]

    def __init__(self, identifier: StrID, location: Location = None):
        self.identifier = ID(identifier)
        self.location = location
        self.__refcount = 0

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError

    def reference(self) -> None:
        self.__refcount += 1

    @property
    @abstractmethod
    def type_(self) -> rty.Type:
        raise NotImplementedError

    @property
    def is_referenced(self) -> bool:
        return self.__refcount > 0

    @abstractmethod
    def variables(self) -> Sequence[Variable]:
        raise NotImplementedError


class BasicDeclaration(Declaration):
    pass


class TypeCheckableDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ):
        super().__init__(identifier, location)
        self.__type_identifier = ID(type_identifier)
        self.__type: rty.Type = type_

    @property
    def type_identifier(self) -> ID:
        return self.__type_identifier

    @property
    def type_(self) -> rty.Type:
        return self.__type

    @type_.setter
    def type_(self, value: rty.Type) -> None:
        self.__type = value

    @abstractmethod
    def check_type(
        self, declaration_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        """Set the types of the declaration and variables, and check the types of expressions."""
        raise NotImplementedError


class VariableDeclaration(TypeCheckableDeclaration, BasicDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "variable"

    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID,
        expression: Expr = None,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ):
        super().__init__(identifier, type_identifier, type_, location)
        self.expression = expression

    def __str__(self) -> str:
        expression = f" := {self.expression}" if self.expression else ""
        return f"{self.identifier} : {self.type_identifier}{expression}"

    def check_type(
        self, declaration_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = declaration_type

        if self.expression is not None:
            self.expression = self.expression.substituted(typify_variable)
            return self.expression.check_type(self.type_)

        return RecordFluxError()

    def variables(self) -> Sequence[Variable]:
        if self.expression:
            return self.expression.variables()
        return []


class RenamingDeclaration(TypeCheckableDeclaration, BasicDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "renaming"

    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID,
        expression: Selected,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ):
        super().__init__(identifier, type_identifier, type_, location)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.identifier} : {self.type_identifier} renames {self.expression}"

    def check_type(
        self, declaration_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = declaration_type
        expression = self.expression.substituted(typify_variable)
        assert isinstance(expression, Selected)
        self.expression = expression

        error = self.expression.prefix.check_type_instance(rty.Message)
        if error.errors:
            return error

        assert isinstance(self.expression.prefix.type_, rty.Message)

        error = RecordFluxError()
        for r in self.expression.prefix.type_.refinements:
            if ID(r.field) == self.expression.selector and r.sdu.is_compatible(declaration_type):
                break
        else:
            error.append(
                f'invalid renaming to "{self.identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            error.append(
                f'refinement for message "{self.expression.prefix.type_.identifier}"'
                " would make operation legal",
                Subsystem.MODEL,
                Severity.INFO,
                self.location,
            )
        return error + self.expression.check_type(rty.OPAQUE)

    def variables(self) -> Sequence[Variable]:
        return self.expression.variables()


class FormalDeclaration(Declaration):
    def variables(self) -> Sequence[Variable]:
        return []


class Argument(Base):
    def __init__(
        self, identifier: StrID, type_identifier: StrID, type_: rty.Type = rty.Undefined()
    ):
        super().__init__()
        self.__identifier = ID(identifier)
        self.__type_identifier = ID(type_identifier)
        self.type_ = type_

    def __str__(self) -> str:
        return f"{self.__identifier} : {self.__type_identifier}"

    @property
    def identifier(self) -> ID:
        return self.__identifier

    @property
    def type_identifier(self) -> ID:
        return self.__type_identifier


class FunctionDeclaration(TypeCheckableDeclaration, FormalDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "function"

    def __init__(
        self,
        identifier: StrID,
        arguments: Sequence[Argument],
        return_type: StrID,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ):
        super().__init__(identifier, return_type, type_, location)
        self.__arguments = arguments
        self.__return_type = ID(return_type)

    def __str__(self) -> str:
        arguments = (" (" + "; ".join(map(str, self.__arguments)) + ")") if self.__arguments else ""
        return f"with function {self.identifier}{arguments} return {self.__return_type}"

    def check_type(
        self, declaration_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = declaration_type
        return RecordFluxError()

    @property
    def arguments(self) -> Sequence[Argument]:
        return self.__arguments

    @property
    def return_type(self) -> ID:
        return self.__return_type


class ChannelDeclaration(FormalDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "channel"

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
    def type_(self) -> rty.Type:
        return rty.Channel(self.readable, self.writable)

    @property
    def readable(self) -> bool:
        return self.__readable

    @property
    def writable(self) -> bool:
        return self.__writable


class TypeDeclaration(FormalDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "type"

    def __init__(self, type_: "mty.Type"):
        super().__init__(type_.identifier, type_.location)
        self.type_definition = type_

    def __str__(self) -> str:
        return str(self.type_definition)

    @property
    def type_(self) -> rty.Type:
        raise NotImplementedError
