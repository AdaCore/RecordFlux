from abc import ABC, abstractmethod, abstractproperty
from collections import OrderedDict
from copy import copy
from math import log
from typing import Dict, List, Optional, Tuple


class Element(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')


class Expr(Element):
    pass


class LogExpr(Expr):
    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> 'LogExpr':
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class TrueExpr(LogExpr):
    def __repr__(self) -> str:
        return 'TRUE'

    def __str__(self) -> str:
        return 'True'

    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        return self

    def symbol(self) -> str:
        return self.__str__()


TRUE = TrueExpr()


class BinLogExpr(LogExpr):
    def __init__(self, left: LogExpr, right: LogExpr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '({} {} {})'.format(self.left, self.__class__.__name__, self.right)

    def __str__(self) -> str:
        return '({} {} {})'.format(self.left, self.symbol(), self.right)

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class And(BinLogExpr):
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if left is TRUE and right is TRUE:
            return TRUE
        if left is TRUE:
            return right
        if right is TRUE:
            return left
        return And(left, right)

    def symbol(self) -> str:
        return 'and then'


class Or(BinLogExpr):
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if left is TRUE or right is TRUE:
            return TRUE
        return Or(left, right)

    def symbol(self) -> str:
        return 'or'


class MathExpr(Expr):
    def __lt__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return self == other
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, MathExpr):
            return self == other
        return NotImplemented

    @abstractmethod
    def __neg__(self) -> 'MathExpr':
        raise NotImplementedError

    @abstractmethod
    def __contains__(self, item: 'MathExpr') -> bool:
        raise NotImplementedError

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> 'MathExpr':
        raise NotImplementedError

    @abstractmethod
    def to_bytes(self) -> 'MathExpr':
        raise NotImplementedError


class UndefinedExpr(MathExpr):
    def __init__(self) -> None:
        pass

    def __repr__(self) -> str:
        return 'UNDEFINED'

    def __str__(self) -> str:
        return self.__repr__()

    def __neg__(self) -> MathExpr:
        return self

    def __contains__(self, item: MathExpr) -> bool:
        raise NotImplementedError

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        return self

    def to_bytes(self) -> MathExpr:
        return self


UNDEFINED = UndefinedExpr()


class Number(MathExpr):
    def __init__(self, value: int) -> None:
        self.value = value

    def __repr__(self) -> str:
        return 'Number({})'.format(self.value)

    def __str__(self) -> str:
        if self.value < 0:
            return '({})'.format(self.value)
        return str(self.value)

    def __int__(self) -> int:
        return self.value

    def __neg__(self) -> 'Number':
        return Number(-self.value)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

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

    def __floordiv__(self, other: 'Number') -> 'MathExpr':
        if isinstance(other, Number):
            if self.value % other.value == 0:
                return Number(self.value // other.value)
            return Div(Number(self.value), Number(other.value))
        return NotImplemented

    def __pow__(self, other: 'Number') -> 'Number':
        if isinstance(other, Number):
            return Number(self.value ** other.value)
        return NotImplemented

    def __lt__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value < other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value <= other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value > other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value >= other.value
        if isinstance(other, MathExpr):
            return False
        return NotImplemented

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        return self

    def to_bytes(self) -> MathExpr:
        return Number(self.value // 8)


class AssMathExpr(MathExpr):
    def __init__(self, *terms: MathExpr) -> None:
        self.terms = list(terms)

    def __repr__(self) -> str:
        return '({})'.format(' {} '.format(self.symbol()).join(map(str, self.terms)))

    @abstractmethod
    def __neg__(self) -> MathExpr:
        raise NotImplementedError

    def __contains__(self, item: MathExpr) -> bool:
        return any(item in term for term in self.terms)

    def __lt__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                lt = [x < y for x, y in zip(self.terms, other.terms)]
                eq = [x == y for x, y in zip(self.terms, other.terms)]
                return any(lt) and all(map((lambda x: x[0] or x[1]), zip(lt, eq)))
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                return all([x <= y for x, y in zip(self.terms, other.terms)])
            return False
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                gt = [x > y for x, y in zip(self.terms, other.terms)]
                eq = [x == y for x, y in zip(self.terms, other.terms)]
                return any(gt) and all(map((lambda x: x[0] or x[1]), zip(gt, eq)))
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, AssMathExpr):
            if len(self.terms) == len(other.terms):
                return all([x >= y for x, y in zip(self.terms, other.terms)])
            return False
        return NotImplemented

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        terms: List[MathExpr] = []
        all_terms = list(self.terms)
        total = self.neutral_element()
        for term in all_terms:
            t = term.simplified(facts)
            if isinstance(t, Number):
                total = self.operation(total, t.value)
            elif isinstance(t, type(self)):
                all_terms += t.terms
            else:
                terms.append(t)
        if not terms:
            return Number(total)
        if total != self.neutral_element():
            terms.append(Number(total))
        if len(terms) == 1:
            return terms[0]
        return self.__class__(*terms)

    @abstractmethod
    def operation(self, left: int, right: int) -> int:
        raise NotImplementedError

    @abstractmethod
    def neutral_element(self) -> int:
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError

    def to_bytes(self) -> MathExpr:
        return self.__class__(*[term.to_bytes() for term in self.terms])


class Add(AssMathExpr):
    def __neg__(self) -> MathExpr:
        return Add(*[-term for term in self.terms])

    def operation(self, left: int, right: int) -> int:
        return left + right

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        expr = super().simplified(facts)
        if not isinstance(expr, Add):
            return expr
        terms: List[MathExpr] = []
        for term in reversed(expr.terms):
            complement = False
            for other in terms:
                if other == -term:
                    terms.remove(other)
                    complement = True
                    break
            if not complement:
                terms.insert(0, term)
        if len(terms) == 1:
            return terms[0]
        return Add(*terms)

    def neutral_element(self) -> int:
        return 0

    def symbol(self) -> str:
        return '+'


class Mul(AssMathExpr):
    def __neg__(self) -> MathExpr:
        return Mul(*list(self.terms) + [Number(-1)]).simplified()

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
        return '({}{}{})'.format(self.left, self.symbol(), self.right)

    def __neg__(self) -> MathExpr:
        return self.__class__(-self.left, self.right)

    def __contains__(self, item: MathExpr) -> bool:
        return item in (self.left, self.right)

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        raise NotImplementedError

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError

    def to_bytes(self) -> MathExpr:
        left = self.left.to_bytes()
        right = self.right.to_bytes()
        return self.__class__(left, right)


class Sub(BinMathExpr):
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left - right
        if isinstance(left, Number):
            return Add(right, -left)
        if isinstance(right, Number):
            return Add(left, -right)
        return Add(left, -right)

    def symbol(self) -> str:
        return ' - '


class Div(BinMathExpr):
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left // right
        if isinstance(left, Add) and isinstance(right, Number):
            return Add(*[Div(term, right) for term in left.terms]).simplified(facts)
        if isinstance(left, Mul) and isinstance(right, Number):
            terms: List[MathExpr] = []
            for term in left.terms:
                if isinstance(term, Number):
                    terms.append((term // right).simplified(facts))
                else:
                    terms.append(term)
            return Mul(*terms).simplified(facts)
        return Div(left, right)

    def symbol(self) -> str:
        return ' / '


class Pow(BinMathExpr):
    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left**right
        return Pow(left, right)

    def symbol(self) -> str:
        return '**'


class Attribute(MathExpr):
    def __init__(self, name: str, negative: bool = False) -> None:
        self.name = name
        self.negative = negative

    def __repr__(self) -> str:
        result = '{}\'{}'.format(self.name, self.__class__.__name__)
        if self.negative:
            return '(-{})'.format(result)
        return result

    def __hash__(self) -> int:
        return hash(self.name + self.__class__.__name__)

    def __neg__(self) -> 'Attribute':
        return self.__class__(self.name, not self.negative)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        if facts:
            positive_self = self.__class__(self.name)
            if positive_self in facts:
                if positive_self in facts[positive_self]:
                    raise ModelError(f'self-reference to "{positive_self}"')
                return -facts[positive_self] if self.negative else facts[positive_self]
        return self

    def to_bytes(self) -> MathExpr:
        return self


class Value(Attribute):
    def __str__(self) -> str:
        if self.negative:
            return '(-{})'.format(self.name)
        return self.name


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
        return '{} {} {}'.format(self.left, self.symbol(), self.right)

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> 'Relation':
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        return self.__class__(left, right)

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
        return '/='


class Type(Element):
    def __init__(self, name: str) -> None:
        self.name = name

    @abstractproperty
    def size(self) -> MathExpr:
        raise NotImplementedError

    @property
    def constraints(self) -> LogExpr:
        return TRUE

    @property
    def base_name(self) -> str:
        return f'{self.name}_Base'


class ModularInteger(Type):
    def __init__(self, name: str, modulus: MathExpr) -> None:
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
    def modulus(self) -> MathExpr:
        return self.__modulus

    @property
    def size(self) -> MathExpr:
        return self.__size


class RangeInteger(Type):
    def __init__(self, name: str, first: MathExpr, last: MathExpr, size: MathExpr) -> None:
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

        constraints: LogExpr = TRUE
        if self.first.simplified() != self.base_first.simplified():
            constraints = GreaterEqual(Value(self.name), self.first)
        if self.last.simplified() != self.base_last.simplified():
            constraints = And(constraints, LessEqual(Value(self.name), self.last))
        self.__constraints = constraints.simplified()

    @property
    def first(self) -> MathExpr:
        return self.__first

    @property
    def last(self) -> MathExpr:
        return self.__last

    @property
    def size(self) -> MathExpr:
        return self.__size

    @property
    def constraints(self) -> LogExpr:
        return self.__constraints

    @property
    def base_first(self) -> MathExpr:
        return Number(0)

    @property
    def base_last(self) -> MathExpr:
        return Sub(Pow(Number(2), self.size), Number(1))


class Array(Type):
    @property
    def size(self) -> MathExpr:
        raise ModelError(f'size of "{self.name}" undefined')


class Refinement(Type):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, pdu: str, field: str, sdu: str,
                 condition: LogExpr = TRUE) -> None:
        super().__init__(name)
        self.pdu = pdu
        self.field = field
        self.sdu = sdu
        self.condition = condition

    @property
    def size(self) -> Number:
        raise NotImplementedError

    @property
    def unqualified_name(self) -> str:
        return self.name.rsplit('.', 1)[-1]

    @property
    def package(self) -> str:
        return self.name.rsplit('.', 1)[0]


class Node(Element):
    def __init__(self, name: str, data_type: Type, edges: List['Edge'] = None) -> None:
        self.name = name
        self.type = data_type
        self.edges = edges or []


FINAL = Node('', Array(''))


class Edge(Element):
    def __init__(self, target: Node, condition: LogExpr = TRUE, length: MathExpr = UNDEFINED,
                 first: MathExpr = UNDEFINED) -> None:
        self.target = target
        self.condition = condition
        self.length = length
        self.first = first


class Variant(Element):
    def __init__(self, previous: List[Tuple[str, str]], condition: LogExpr,
                 facts: Dict[Attribute, MathExpr]) -> None:
        self.previous = previous
        self.condition = condition
        self.facts = facts


class Field(Element):
    def __init__(self, name: str, data_type: Type, condition: LogExpr,
                 variants: Dict[str, Variant]) -> None:
        self.name = name
        self.type = data_type
        self.condition = condition
        self.variants = variants


class PDU(Element):
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

    def fields(self, facts: Dict[Attribute, MathExpr] = None,
               first: MathExpr = UNDEFINED) -> Dict[str, Field]:
        if facts is None:
            facts = {}
        return evaluate(facts, Edge(self.initial_node, TRUE, first=first))


class ModelError(Exception):
    pass


def evaluate(facts: Dict[Attribute, MathExpr],
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

    facts = create_facts(facts, in_edge)

    fields = OrderedDict([
        (node.name,
         Field(node.name,
               node.type,
               combine_conditions(TRUE,
                                  TRUE,
                                  [e.condition for e in node.edges]).simplified(),
               {
                   variant_id: Variant(previous,
                                       in_edge.condition,
                                       facts)
               }
               )
         )
    ])

    for i, out_edge in enumerate(node.edges):
        if out_edge.target is FINAL:
            continue

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


def create_facts(facts: Dict[Attribute, MathExpr], edge: Edge) -> Dict[Attribute, MathExpr]:
    facts = dict(facts)
    facts[Length(edge.target.name)] = edge.length.simplified(facts)
    facts[First(edge.target.name)] = edge.first.simplified(facts)
    facts[Last(edge.target.name)] = Add(edge.first, edge.length, Number(-1)).simplified(facts)
    return facts


def combine_conditions(all_cond: LogExpr, in_cond: LogExpr, out_cond: List[LogExpr]) -> LogExpr:
    if out_cond:
        res = out_cond.pop()
        for c in out_cond:
            res = Or(res, c)
    else:
        res = TRUE
    return And(And(all_cond, in_cond), res)


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
