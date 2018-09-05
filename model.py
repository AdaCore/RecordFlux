#!/usr/bin/env python

from abc import ABC, abstractmethod
from typing import Dict, List, Tuple


class Expr(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented


class LogExpr(Expr):
    @abstractmethod
    def simplified(self) -> 'LogExpr':
        raise NotImplementedError


class TrueExpr(LogExpr):
    def __repr__(self) -> str:
        return 'TRUE'

    def __str__(self) -> str:
        return self.__repr__()

    def simplified(self) -> LogExpr:
        return self


TRUE = TrueExpr()


class BinLogExpr(LogExpr):
    def __init__(self, left: LogExpr, right: LogExpr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '({} {} {})'.format(self.left, self.__class__.__name__, self.right)

    def __str__(self) -> str:
        return self.__repr__()

    @abstractmethod
    def simplified(self) -> 'LogExpr':
        raise NotImplementedError


class And(BinLogExpr):
    def simplified(self) -> LogExpr:
        self.left = self.left.simplified()
        self.right = self.right.simplified()
        if self.left is TRUE and self.right is TRUE:
            return TRUE
        if self.left is TRUE:
            return self.right
        if self.right is TRUE:
            return self.left
        return self


class Or(BinLogExpr):
    def simplified(self) -> LogExpr:
        self.left = self.left.simplified()
        self.right = self.right.simplified()
        if self.left is TRUE or self.right is TRUE:
            return TRUE
        return self


class MathExpr(Expr):
    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> 'MathExpr':
        raise NotImplementedError


class UndefinedExpr(MathExpr):
    def __init__(self) -> None:
        pass

    def __repr__(self) -> str:
        return 'UNDEFINED'

    def __str__(self) -> str:
        return self.__repr__()

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        return self


UNDEFINED = UndefinedExpr()


class Number(MathExpr):
    def __init__(self, value: int) -> None:
        self.value = value

    def __repr__(self) -> str:
        return 'Number({})'.format(self.value)

    def __str__(self) -> str:
        return self.__repr__()

    def __add__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value + other.value)
        return NotImplemented

    def __sub__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value - other.value)
        return NotImplemented

    def __mul__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value * other.value)
        return NotImplemented

    def __floordiv__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value // other.value)
        return NotImplemented

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        return self


class AssMathExpr(MathExpr):
    def __init__(self, *terms: 'MathExpr') -> None:
        self.terms = list(terms)

    def __repr__(self) -> str:
        return '({})'.format(' {} '.format(self.symbol()).join(map(str, self.terms)))

    def __str__(self) -> str:
        return self.__repr__()

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        terms = []
        total = self.neutral_element()
        for summand in self.terms:
            s = summand.simplified(facts)
            if isinstance(s, Number):
                total = self.operation(total, s.value)
            elif isinstance(s, type(self)):
                self.terms += s.terms
            else:
                terms.append(s)
        if not terms:
            return Number(total)
        if total > 0:
            terms.append(Number(total))
        if len(terms) == 1:
            return terms[0]
        self.terms = terms
        return self

    @abstractmethod
    def operation(self, left: int, right: int) -> int:
        raise NotImplementedError

    @abstractmethod
    def neutral_element(self) -> int:
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class Add(AssMathExpr):
    def operation(self, left: int, right: int) -> int:
        return left + right

    def neutral_element(self) -> int:
        return 0

    def symbol(self) -> str:
        return '+'


class Mul(AssMathExpr):
    def operation(self, left: int, right: int) -> int:
        return left * right

    def neutral_element(self) -> int:
        return 1

    def symbol(self) -> str:
        return '*'


class BinMathExpr(MathExpr):
    def __init__(self, left: 'MathExpr', right: 'MathExpr') -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '({} {} {})'.format(self.left, self.symbol(), self.right)

    def __str__(self) -> str:
        return self.__repr__()

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class Sub(BinMathExpr):
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        self.left = self.left.simplified(facts)
        self.right = self.right.simplified(facts)
        if isinstance(self.left, Number) and isinstance(self.right, Number):
            return self.left - self.right
        if isinstance(self.right, Number):
            right = self.right * Number(-1)
            return Add(self.left, right)
        return self

    def symbol(self) -> str:
        return '-'


class Div(BinMathExpr):
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        self.left = self.left.simplified(facts)
        self.right = self.right.simplified(facts)
        if isinstance(self.left, Number) and isinstance(self.right, Number):
            return self.left // self.right
        return self

    def symbol(self) -> str:
        return '/'


class Attribute(MathExpr):
    def __init__(self, name: str) -> None:
        self.name = name

    def __repr__(self) -> str:
        return '{}\'{}'.format(self.name, self.__class__.__name__)

    def __str__(self) -> str:
        return self.__repr__()

    def __hash__(self) -> int:
        return hash(self.name + self.__class__.__name__)

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        if facts and self in facts:
            return facts[self]
        return self


class Value(Attribute):
    def __str__(self) -> str:
        return '{}'.format(self.name)


class Length(Attribute):
    pass


class First(Attribute):
    pass


class Last(Attribute):
    pass


class Relation(LogExpr):
    def __init__(self, left: MathExpr, right: MathExpr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '{}({}, {})'.format(self.__class__.__name__, self.left, self.right)

    def __str__(self) -> str:
        return self.__repr__()

    def simplified(self) -> 'Relation':
        return self

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class Less(Relation):
    def symbol(self) -> str:
        return '<'


class LessEqual(Relation):
    def symbol(self) -> str:
        return '<='


class Equal(Relation):
    def symbol(self) -> str:
        return '='


class GreaterEqual(Relation):
    def symbol(self) -> str:
        return '>='


class Greater(Relation):
    def symbol(self) -> str:
        return '>'


class NotEqual(Relation):
    def symbol(self) -> str:
        return '!='


class Node:
    def __init__(self, name: str, edges: List['Edge'] = None) -> None:
        self.name = name
        self.edges = edges or []

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return 'Node(\n\t\'{}\',\n\t{},\n\t{}\n\t)'.format(
            self.name, self.type, self.edges).replace('\t', '\t  ')


FINAL = Node('', [])


class Edge:
    def __init__(self, target: Node, condition: LogExpr = TRUE, length: MathExpr = UNDEFINED,
                 first: MathExpr = UNDEFINED) -> None:
        self.target = target
        self.condition = condition
        self.length = length
        self.first = first

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return 'Edge(\n\t{},\n\t{},\n\t{},\n\t{}\n\t)'.format(
            self.target, self.condition, self.length, self.first).replace('\t', '\t  ')


class ModelError(Exception):
    pass


def combine_conditions(all_cond: LogExpr, in_cond: LogExpr, out_cond: List[LogExpr]) -> LogExpr:
    if out_cond:
        res = out_cond.pop()
        for c in out_cond:
            res = Or(res, c)
    else:
        res = TRUE
    return And(And(all_cond, in_cond), res)


def filter_fields(fields: Dict[str, List[Tuple[LogExpr, Dict[Attribute, MathExpr]]]]
                  ) -> Dict[str, List[Tuple[LogExpr, Dict[Attribute, MathExpr]]]]:
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


def evaluate(facts: Dict[Attribute, MathExpr], all_cond: LogExpr, in_edge: Edge,
             visited: List[Edge] = None
             ) -> Dict[str, List[Tuple[LogExpr, Dict[Attribute, MathExpr]]]]:
    node = in_edge.target

    facts = dict(facts)
    facts[First(node.name)] = in_edge.first.simplified(facts)
    facts[Last(node.name)] = Add(in_edge.first, in_edge.length, Number(-1)).simplified(facts)

    cond = combine_conditions(all_cond,
                              in_edge.condition,
                              [e.condition for e in node.edges]).simplified()

    fields: Dict[str, List[Tuple[LogExpr, Dict[Attribute, MathExpr]]]] = {node.name:
                                                                          [(cond, facts)]}
    for out_edge in node.edges:
        if out_edge.target is FINAL:
            continue

        if not visited:
            visited = []
        if out_edge in visited:
            raise ModelError('cyclic')
        visited = list(visited + [out_edge])

        edge = out_edge
        if edge.first is UNDEFINED:
            edge = Edge(edge.target,
                        edge.condition,
                        edge.length,
                        Add(in_edge.first, in_edge.length))
        edge_fields = evaluate(facts,
                               combine_conditions(all_cond, in_edge.condition, []),
                               edge,
                               visited)
        for field in edge_fields:
            if field not in fields:
                fields[field] = []
            fields[field] += edge_fields[field]

    return fields
