from typing import List

from rflx.common import flat_name, generic_repr
from rflx.expression import TRUE, UNDEFINED, Expr
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
        self, name: str, first: Expr = UNDEFINED, length: Expr = UNDEFINED, condition: Expr = TRUE
    ) -> None:
        self.name = name
        self.first = first
        self.length = length
        self.condition = condition


class Component(SyntaxTree):
    def __init__(self, name: str, type_name: str, thens: List[Then] = None) -> None:
        self.name = name
        self.type = type_name
        self.thens = thens or []


class PackageSpec(SyntaxTree):
    def __init__(self, identifier: str, types: List[Type]) -> None:
        self.identifier = identifier
        self.types = types


class ContextSpec(SyntaxTree):
    def __init__(self, items: List[str]) -> None:
        self.items = items


class MessageSpec(Type):
    def __init__(self, name: str, components: List[Component]) -> None:
        super().__init__(name)
        self.components = components


class DerivationSpec(Type):
    def __init__(self, name: str, base: str) -> None:
        super().__init__(name)
        self.base = base


class RefinementSpec(Type):
    # pylint: disable=too-many-arguments
    def __init__(
        self, package: str, pdu: str, field: str, sdu: str, condition: Expr = TRUE
    ) -> None:
        super().__init__(f"{package}.__REFINEMENT__{flat_name(sdu)}__{flat_name(pdu)}__{field}__")
        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition


class ReferenceSpec(Type):
    pass


class Specification(SyntaxTree):
    def __init__(self, context: ContextSpec, package: PackageSpec) -> None:
        self.context = context
        self.package = package
