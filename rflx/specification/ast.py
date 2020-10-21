from dataclasses import dataclass
from typing import List, Mapping, Sequence

import rflx.declaration as decl
from rflx.common import Base, flat_name
from rflx.error import Location
from rflx.expression import TRUE, UNDEFINED, Expr
from rflx.identifier import ID, StrID
from rflx.model import State, Type


class SyntaxTree(Base):
    pass


class Then(SyntaxTree):
    def __init__(
        self,
        identifier: StrID = None,
        first: Expr = UNDEFINED,
        size: Expr = UNDEFINED,
        condition: Expr = TRUE,
        location: Location = None,
    ) -> None:
        self.identifier = ID(identifier) if identifier else None
        self.first = first
        self.size = size
        self.condition = condition
        self.location = location

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.identifier == other.identifier
                and self.first == other.first
                and self.size == other.size
                and self.condition == other.condition
            )
        return NotImplemented


class Component(SyntaxTree):
    def __init__(
        self,
        identifier: StrID = None,
        type_identifier: StrID = None,
        thens: List[Then] = None,
        first: Expr = UNDEFINED,
        size: Expr = UNDEFINED,
        condition: Expr = TRUE,
    ) -> None:
        # pylint: disable=too-many-arguments
        self.identifier = ID(identifier) if identifier else None
        self.type_identifier = ID(type_identifier) if type_identifier else None
        self.thens = thens or []
        self.first = first
        self.size = size
        self.condition = condition


@dataclass
class SessionSpec:
    identifier: ID
    initial: ID
    final: ID
    states: Sequence[State]
    declarations: Sequence[decl.BasicDeclaration]
    parameters: Sequence[decl.FormalDeclaration]
    location: Location


class PackageSpec(SyntaxTree):
    def __init__(
        self,
        identifier: StrID,
        types: List[Type],
        sessions: List[SessionSpec],
        end_identifier: StrID = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.end_identifier = ID(end_identifier) if end_identifier else self.identifier
        self.types = types
        self.sessions = sessions


class ContextSpec(SyntaxTree):
    def __init__(self, items: List[StrID]) -> None:
        self.items = list(map(ID, items))


class ReferenceSpec(Type):
    pass


class ArraySpec(Type):
    def __init__(
        self, identifier: StrID, element_type: ReferenceSpec, location: Location = None
    ) -> None:
        super().__init__(identifier, location)
        self.element_type = element_type


class MessageSpec(Type):
    def __init__(
        self,
        identifier: StrID,
        components: List[Component],
        aspects: Mapping[ID, Mapping[ID, List[Expr]]] = None,
        location: Location = None,
    ) -> None:
        super().__init__(identifier, location)
        self.components = components
        self.aspects = aspects or {}


class DerivationSpec(Type):
    def __init__(self, identifier: StrID, base: StrID, location: Location = None) -> None:
        super().__init__(identifier, location)
        self.base = ID(base)


class RefinementSpec(Type):
    def __init__(
        self,
        pdu: StrID,
        field: StrID,
        sdu: StrID,
        condition: Expr = TRUE,
        location: Location = None,
    ) -> None:
        self.pdu = ID(pdu)
        self.field = ID(field)
        self.sdu = ID(sdu)
        self.condition = condition
        super().__init__(
            ID("__PACKAGE__") * f"__REFINEMENT__{flat_name(str(self.sdu))}"
            f"__{flat_name(str(self.pdu))}__{field}__",
            location,
        )


class Specification(SyntaxTree):
    def __init__(self, context: ContextSpec, package: PackageSpec) -> None:
        self.context = context
        self.package = package
