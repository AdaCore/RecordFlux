from __future__ import annotations

from abc import abstractmethod
from collections.abc import Callable, Generator, Sequence
from typing import ClassVar, Optional

import rflx.typing_ as rty
from rflx import ir
from rflx.common import Base
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail
from rflx.expression import Expr, Selected, Variable
from rflx.identifier import ID, StrID

from . import type_ as mty


class Declaration(Base):
    DESCRIPTIVE_NAME: ClassVar[str]

    def __init__(self, identifier: StrID, location: Optional[Location] = None):
        self.identifier = ID(identifier)
        self.location = location
        self._refcount = 0

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError

    def reference(self) -> None:
        self._refcount += 1

    @property
    @abstractmethod
    def type_(self) -> rty.Type:
        raise NotImplementedError

    @property
    def is_referenced(self) -> bool:
        return self._refcount > 0

    @abstractmethod
    def variables(self) -> Sequence[Variable]:
        """Return all referenced variables in the declaration."""
        raise NotImplementedError


class BasicDeclaration(Declaration):
    def to_ir(self, variable_id: Generator[ID, None, None]) -> ir.VarDecl:
        raise NotImplementedError


class TypeCheckableDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID,
        type_: rty.Type = rty.UNDEFINED,
        location: Optional[Location] = None,
    ):
        super().__init__(identifier, location)
        self._type_identifier = ID(type_identifier)
        self._type: rty.Type = type_

    @property
    def type_identifier(self) -> ID:
        return self._type_identifier

    @property
    def type_(self) -> rty.Type:
        return self._type

    @type_.setter
    def type_(self, value: rty.Type) -> None:
        self._type = value

    @abstractmethod
    def check_type(
        self,
        declaration_type: rty.Type,
        typify_variable: Callable[[Expr], Expr],
    ) -> RecordFluxError:
        """Set the types of the declaration and variables, and check the types of expressions."""
        raise NotImplementedError


class VariableDeclaration(TypeCheckableDeclaration, BasicDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "variable"

    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID,
        expression: Optional[Expr] = None,
        type_: rty.Type = rty.UNDEFINED,
        location: Optional[Location] = None,
    ):
        super().__init__(identifier, type_identifier, type_, location)
        self.expression = expression

    def __str__(self) -> str:
        expression = f" := {self.expression}" if self.expression else ""
        return f"{self.identifier} : {ada_type_name(self.type_identifier)}{expression}"

    def check_type(
        self,
        declaration_type: rty.Type,
        typify_variable: Callable[[Expr], Expr],
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

    def to_ir(self, variable_id: Generator[ID, None, None]) -> ir.VarDecl:
        assert isinstance(self.type_, rty.NamedType)
        expression = self.expression.to_ir(variable_id) if self.expression else None
        return ir.VarDecl(
            self.identifier,
            self.type_,
            expression,
            origin=self,
        )


class RenamingDeclaration(TypeCheckableDeclaration, BasicDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "renaming"

    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID,
        expression: Selected,
        type_: rty.Type = rty.UNDEFINED,
        location: Optional[Location] = None,
    ):
        super().__init__(identifier, type_identifier, type_, location)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.identifier} : {self.type_identifier} renames {self.expression}"

    def check_type(
        self,
        declaration_type: rty.Type,
        typify_variable: Callable[[Expr], Expr],
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
            error.extend(
                [
                    (
                        f'invalid renaming to "{self.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    (
                        f'refinement for message "{self.expression.prefix.type_.identifier}"'
                        " would make operation legal",
                        Subsystem.MODEL,
                        Severity.INFO,
                        self.location,
                    ),
                ],
            )
        return error + self.expression.check_type(rty.OPAQUE)

    def variables(self) -> Sequence[Variable]:
        return self.expression.variables()

    def to_ir(self, _variable_id: Generator[ID, None, None]) -> ir.VarDecl:
        fail(
            "renaming declarations not yet supported",
            Subsystem.MODEL,
            location=self.location,
        )


class FormalDeclaration(Declaration):
    def variables(self) -> Sequence[Variable]:
        return []

    def to_ir(self) -> ir.FormalDecl:
        raise NotImplementedError


class Argument(Base):
    def __init__(self, identifier: StrID, type_identifier: StrID, type_: rty.Type = rty.UNDEFINED):
        super().__init__()
        self._identifier = ID(identifier)
        self._type_identifier = ID(type_identifier)
        self.type_ = type_

    def __str__(self) -> str:
        return f"{self._identifier} : {ada_type_name(self._type_identifier)}"

    @property
    def identifier(self) -> ID:
        return self._identifier

    @property
    def type_identifier(self) -> ID:
        return self._type_identifier

    def to_ir(self) -> ir.Argument:
        return ir.Argument(self.identifier, self.type_identifier, self.type_)


class FunctionDeclaration(TypeCheckableDeclaration, FormalDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "function"

    def __init__(
        self,
        identifier: StrID,
        arguments: Sequence[Argument],
        return_type: StrID,
        type_: rty.Type = rty.UNDEFINED,
        location: Optional[Location] = None,
    ):
        super().__init__(identifier, return_type, type_, location)
        self._arguments = arguments
        self._return_type = ID(return_type)

    def __str__(self) -> str:
        arguments = (" (" + "; ".join(map(str, self._arguments)) + ")") if self._arguments else ""
        return (
            f"with function {self.identifier}{arguments} return {ada_type_name(self._return_type)}"
        )

    def check_type(
        self,
        declaration_type: rty.Type,
        _typify_variable: Callable[[Expr], Expr],
    ) -> RecordFluxError:
        self.type_ = declaration_type
        return RecordFluxError()

    @property
    def arguments(self) -> Sequence[Argument]:
        return self._arguments

    @property
    def return_type(self) -> ID:
        return self._return_type

    def to_ir(self) -> ir.FuncDecl:
        return ir.FuncDecl(
            self.identifier,
            [a.to_ir() for a in self.arguments],
            self.return_type,
            self.type_,
            self.location,
        )


class ChannelDeclaration(FormalDeclaration):
    DESCRIPTIVE_NAME: ClassVar[str] = "channel"

    def __init__(
        self,
        identifier: StrID,
        readable: bool = False,
        writable: bool = False,
        location: Optional[Location] = None,
    ):
        assert readable or writable
        super().__init__(identifier, location)
        self._readable = readable
        self._writable = writable

    def __str__(self) -> str:
        aspects = []
        if self._readable:
            aspects.append("Readable")
        if self._writable:
            aspects.append("Writable")
        with_aspects = " with " + ", ".join(aspects)
        return f"{self.identifier} : Channel{with_aspects}"

    @property
    def type_(self) -> rty.Type:
        return rty.Channel(self.readable, self.writable)

    @property
    def readable(self) -> bool:
        return self._readable

    @property
    def writable(self) -> bool:
        return self._writable

    def to_ir(self) -> ir.ChannelDecl:
        return ir.ChannelDecl(self.identifier, self.readable, self.writable, self.location)


def ada_type_name(identifier: ID) -> StrID:
    if mty.is_builtin_type(identifier) or mty.is_internal_type(identifier):
        return identifier.name
    return identifier
