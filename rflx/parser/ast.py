from typing import List

from rflx.common import flat_name, generic_repr
from rflx.error import Location
from rflx.expression import TRUE, UNDEFINED, Expr
from rflx.identifier import ID, StrID
from rflx.model import Type


class SyntaxTree:
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)


class Then(SyntaxTree):
    def __init__(
        self,
        name: StrID = None,
        first: Expr = UNDEFINED,
        length: Expr = UNDEFINED,
        condition: Expr = TRUE,
        location: Location = None,
    ) -> None:
        self.name = ID(name) if name else None
        self.first = first
        self.length = length
        self.condition = condition
        self.location = location

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.name == other.name
                and self.first == other.first
                and self.length == other.length
                and self.condition == other.condition
            )
        return NotImplemented


class Component(SyntaxTree):
    def __init__(
        self, name: StrID = None, type_name: StrID = None, thens: List[Then] = None
    ) -> None:
        self.name = ID(name) if name else None
        self.type_name = ID(type_name) if type_name else None
        self.thens = thens or []


class PackageSpec(SyntaxTree):
    def __init__(self, identifier: StrID, types: List[Type], end_identifier: StrID = None) -> None:
        self.identifier = ID(identifier)
        self.end_identifier = ID(end_identifier) if end_identifier else self.identifier
        self.types = types


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
        self, identifier: StrID, components: List[Component], location: Location = None
    ) -> None:
        super().__init__(identifier, location)
        self.components = components


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
            f"__PACKAGE__.__REFINEMENT__{flat_name(str(self.sdu))}"
            f"__{flat_name(str(self.pdu))}__{field}__",
            location,
        )


class Specification(SyntaxTree):
    def __init__(self, context: ContextSpec, package: PackageSpec) -> None:
        self.context = context
        self.package = package
