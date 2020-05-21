from typing import List

from rflx.common import flat_name, generic_repr
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
        self, name: StrID, first: Expr = UNDEFINED, length: Expr = UNDEFINED, condition: Expr = TRUE
    ) -> None:
        self.name = ID(name)
        self.first = first
        self.length = length
        self.condition = condition


class Component(SyntaxTree):
    def __init__(self, name: StrID, type_name: StrID, thens: List[Then] = None) -> None:
        self.name = ID(name)
        self.type_name = ID(type_name)
        self.thens = thens or []


class PackageSpec(SyntaxTree):
    def __init__(self, identifier: StrID, types: List[Type]) -> None:
        self.identifier = ID(identifier)
        self.types = types


class ContextSpec(SyntaxTree):
    def __init__(self, items: List[StrID]) -> None:
        self.items = list(map(ID, items))


class MessageSpec(Type):
    def __init__(self, identifier: StrID, components: List[Component]) -> None:
        super().__init__(identifier)
        self.components = components


class DerivationSpec(Type):
    def __init__(self, identifier: StrID, base: StrID) -> None:
        super().__init__(identifier)
        self.base = ID(base)


class RefinementSpec(Type):
    def __init__(self, pdu: StrID, field: StrID, sdu: StrID, condition: Expr = TRUE) -> None:
        self.pdu = ID(pdu)
        self.field = ID(field)
        self.sdu = ID(sdu)
        self.condition = condition
        super().__init__(
            f"__PACKAGE__.__REFINEMENT__{flat_name(str(self.sdu))}"
            f"__{flat_name(str(self.pdu))}__{field}__"
        )


class ReferenceSpec(Type):
    pass


class Specification(SyntaxTree):
    def __init__(self, context: ContextSpec, package: PackageSpec) -> None:
        self.context = context
        self.package = package
