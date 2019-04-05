from abc import ABC, abstractproperty
from collections import OrderedDict
from copy import copy
from math import log
from typing import Dict, List, Optional, Tuple

from rflx.expression import (TRUE, UNDEFINED, Add, And, Attribute, Expr, First, GreaterEqual, Last,
                             Length, LessEqual, Number, Or, Pow, Sub, Value)


class Element(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')


class Type(Element):
    def __init__(self, name: str) -> None:
        self.name = name

    @abstractproperty
    def size(self) -> Expr:
        raise NotImplementedError

    @property
    def constraints(self) -> Expr:
        return TRUE

    @property
    def base_name(self) -> str:
        return f'{self.name}_Base'


class Reference(Type):
    @property
    def size(self) -> Expr:
        raise NotImplementedError


class ModularInteger(Type):
    def __init__(self, name: str, modulus: Expr) -> None:
        modulus_num = modulus.simplified()
        if not isinstance(modulus_num, Number):
            raise ModelError(f'modulus of "{name}" contains variable')
        modulus_int = int(modulus_num)
        if modulus_int > 2**64:
            raise ModelError(f'modulus of "{name}" exceeds limit (2**64)')
        if modulus_int == 0 or (modulus_int & (modulus_int - 1)) != 0:
            raise ModelError(f'modulus of "{name}" not power of two')
        super().__init__(name)
        self.__modulus = modulus
        self.__size = Number(int(log(modulus_int) / log(2)))

    @property
    def modulus(self) -> Expr:
        return self.__modulus

    @property
    def size(self) -> Expr:
        return self.__size


class RangeInteger(Type):
    def __init__(self, name: str, first: Expr, last: Expr, size: Expr) -> None:
        first_num = first.simplified()
        if not isinstance(first_num, Number):
            raise ModelError(f'first of "{name}" contains variable')
        last_num = last.simplified()
        if not isinstance(last_num, Number):
            raise ModelError(f'last of "{name}" contains variable')
        if first_num < Number(0):
            raise ModelError(f'first of "{name}" negative')
        if first_num > last_num:
            raise ModelError(f'range of "{name}" negative')
        size_num = size.simplified()
        if not isinstance(size_num, Number):
            raise ModelError(f'size of "{name}" contains variable')
        if log(int(last_num) + 1) / log(2) > int(size_num):
            raise ModelError(f'size for "{name}" too small')
        super().__init__(name)
        self.__first = first
        self.__last = last
        self.__size = size

        constraints: Expr = TRUE
        if self.first.simplified() != self.base_first.simplified():
            constraints = GreaterEqual(Value(self.name), self.first)
        if self.last.simplified() != self.base_last.simplified():
            constraints = And(constraints, LessEqual(Value(self.name), self.last))
        self.__constraints = constraints.simplified()

    @property
    def first(self) -> Expr:
        return self.__first

    @property
    def last(self) -> Expr:
        return self.__last

    @property
    def size(self) -> Expr:
        return self.__size

    @property
    def constraints(self) -> Expr:
        return self.__constraints

    @property
    def base_first(self) -> Expr:
        return Number(0)

    @property
    def base_last(self) -> Expr:
        return Sub(Pow(Number(2), self.size), Number(1))


class Enumeration(Type):
    def __init__(self, name: str, literals: Dict[str, Number], size: Number,
                 always_valid: bool) -> None:
        if log(max(map(int, literals.values())) + 1) / log(2) > int(size):
            raise ModelError(f'size for "{name}" too small')
        if len(set(literals.values())) < len(literals.values()):
            raise ModelError(f'"{name}" contains elements with same value')
        super().__init__(name)
        self.literals = literals
        self.__size = size
        self.always_valid = always_valid

    @property
    def size(self) -> Number:
        return self.__size

    @property
    def enum_name(self) -> str:
        return f'{self.name}_Enum'


class Array(Type):
    def __init__(self, name: str, element_type: Type = Reference('')) -> None:
        super().__init__(name)
        self.element_type = element_type

    @property
    def size(self) -> Expr:
        raise ModelError(f'size of "{self.name}" undefined')


class Null(Type):
    def __init__(self) -> None:
        super().__init__('NULL')

    @property
    def size(self) -> Expr:
        return Number(0)


class Refinement(Type):
    # pylint: disable=too-many-arguments
    def __init__(self, package: str, pdu: str, field: str, sdu: str,
                 condition: Expr = TRUE) -> None:
        super().__init__('')
        self.package = package
        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return (self.package == other.package
                    and self.pdu == other.pdu
                    and self.field == other.field
                    and self.sdu == other.sdu)
        return NotImplemented

    @property
    def size(self) -> Number:
        raise NotImplementedError


class Node(Element):
    def __init__(self, name: str, data_type: Type, edges: List['Edge'] = None) -> None:
        self.name = name
        self.type = data_type
        self.edges = edges or []


class InitialNode(Node):
    def __init__(self, edges: List['Edge'] = None) -> None:
        super().__init__('INITIAL', Null(), edges)


FINAL = Node('FINAL', Null())


class Edge(Element):
    def __init__(self, target: Node, condition: Expr = TRUE, length: Expr = UNDEFINED,
                 first: Expr = UNDEFINED) -> None:
        self.target = target
        self.condition = condition
        self.length = length
        self.first = first


class Variant(Element):
    def __init__(self, previous: List[Tuple[str, str]], condition: Expr,
                 facts: Dict[Attribute, Expr]) -> None:
        self.previous = previous
        self.condition = condition
        self.facts = facts


class Field(Element):
    def __init__(self, name: str, data_type: Type, condition: Expr,
                 variants: Dict[str, Variant]) -> None:
        self.name = name
        self.type = data_type
        self.condition = condition
        self.variants = variants


class Message(Element):
    def __init__(self, full_name: str, node: Node) -> None:
        self.full_name = full_name
        self.initial_node = node

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    @property
    def package(self) -> str:
        return self.full_name.rsplit('.', 1)[0]

    def fields(self, facts: Dict[Attribute, Expr] = None,
               first: Expr = UNDEFINED) -> Dict[str, Field]:
        if self.initial_node is FINAL:
            return {}

        if facts is None:
            facts = {}
        try:
            initial_edge = Edge(self.initial_node.edges[0].target,
                                self.initial_node.edges[0].condition,
                                self.initial_node.edges[0].length,
                                first)
            return evaluate(facts, initial_edge)
        except ModelError as e:
            raise ModelError(f'{e} in "{self.full_name}"')


class ModelError(Exception):
    pass


def evaluate(facts: Dict[Attribute, Expr],
             in_edge: Edge,
             visited: List[Edge] = None,
             previous: List[Tuple[str, str]] = None,
             variant_id: str = '0') -> Dict[str, Field]:
    if not previous:
        previous = []

    node = in_edge.target

    if in_edge.length is UNDEFINED:
        in_edge.length = node.type.size
    if in_edge.first is UNDEFINED:
        in_edge.first = Number(0)

    facts = create_facts(facts, in_edge) if node is not FINAL else facts

    fields = OrderedDict([
        (node.name,
         Field(node.name,
               node.type,
               disjunction([e.condition for e in node.edges]),
               {
                   variant_id: Variant(previous,
                                       in_edge.condition,
                                       facts)
               }
               )
         )
    ])

    for i, out_edge in enumerate(node.edges):
        visited = create_visited_edges(visited, out_edge)

        edge = copy(out_edge)
        if edge.first is UNDEFINED:
            edge.first = Add(in_edge.first, in_edge.length)

        extend_fields(fields,
                      evaluate(facts,
                               edge,
                               visited,
                               previous + [(node.name, variant_id)],
                               f'{variant_id}{encode_id(i)}'))

    return fields


def create_facts(facts: Dict[Attribute, Expr], edge: Edge) -> Dict[Attribute, Expr]:
    facts = dict(facts)
    facts[Length(edge.target.name)] = edge.length.simplified(facts)
    facts[First(edge.target.name)] = edge.first.simplified(facts)
    facts[Last(edge.target.name)] = Add(edge.first, edge.length, Number(-1)).simplified(facts)
    return facts


def disjunction(cond: List[Expr]) -> Expr:
    if cond:
        res = cond.pop()
        for c in cond:
            res = Or(res, c)
    else:
        res = TRUE
    return res


def create_visited_edges(visited: Optional[List[Edge]], edge: Edge) -> List[Edge]:
    if not visited:
        visited = []
    if edge in visited:
        raise ModelError('cyclic')
    return list(visited + [edge])


def extend_fields(fields: OrderedDict, new_fields: Dict[str, Field]) -> None:
    for new_field in new_fields.values():
        found = False
        for field in fields.values():
            if field.name == new_field.name:
                if field.type != new_field.type:
                    raise ModelError('duplicate node "{field.name}"')
                field.variants.update(new_field.variants)
                found = True
        if not found:
            fields[new_field.name] = new_field
        else:
            fields.move_to_end(new_field.name)


def filter_fields(fields: Dict[str, List[Tuple[Expr, Dict[Attribute, Expr]]]]
                  ) -> Dict[str, List[Tuple[Expr, Dict[Attribute, Expr]]]]:
    return {
        field:
        [
            (
                condition,
                {attr: expr for attr, expr in expressions.items() if attr.name == field}
            )
            for condition, expressions in variants
        ]
        for field, variants in fields.items()
    }


def encode_id(number: int) -> str:
    if number < 0:
        raise ValueError('number must be positive')

    alphabet = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    base36 = ''
    while number:
        number, i = divmod(number, 36)
        base36 = alphabet[i] + base36

    if not base36:
        return alphabet[0]
    if len(base36) == 1:
        return base36
    return f'_{base36}_'
