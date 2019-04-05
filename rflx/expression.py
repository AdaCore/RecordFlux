import itertools
from abc import ABC, abstractmethod
from typing import Callable, Dict, List, Tuple, Union


class Expr(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')

    def __lt__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return self == other
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return self == other
        return NotImplemented

    def __contains__(self, item: 'Expr') -> bool:
        return item == self

    @abstractmethod
    def __neg__(self) -> 'Expr':
        raise NotImplementedError

    def converted(self, replace_function: Callable[['Expr'], 'Expr']) -> 'Expr':
        return replace_function(self)

    @abstractmethod
    def simplified(self, facts: Dict['Attribute', 'Expr'] = None) -> 'Expr':
        raise NotImplementedError

    def to_bytes(self) -> 'Expr':
        return self


class BooleanTrue(Expr):
    def __repr__(self) -> str:
        return 'True'

    def __neg__(self) -> Expr:
        return FALSE

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        return self


TRUE = BooleanTrue()


class BooleanFalse(Expr):
    def __repr__(self) -> str:
        return 'False'

    def __neg__(self) -> Expr:
        return TRUE

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        return self


FALSE = BooleanFalse()


class BinExpr(Expr):
    def __init__(self, left: Expr, right: Expr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '({}{}{})'.format(self.left, self.symbol(), self.right)

    def __neg__(self) -> Expr:
        return self.__class__(-self.left, self.right)

    def __contains__(self, item: Expr) -> bool:
        return item == self or item in self.left or item in self.right

    def converted(self, replace_function: Callable[[Expr], Expr]) -> Expr:
        return self.__class__(self.left.converted(replace_function),
                              self.right.converted(replace_function))

    def to_bytes(self) -> Expr:
        left = self.left.to_bytes()
        right = self.right.to_bytes()
        return self.__class__(left, right)

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class And(BinExpr):
    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
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
        return ' and then '


class Or(BinExpr):
    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if left is TRUE or right is TRUE:
            return TRUE
        return Or(left, right)

    def symbol(self) -> str:
        return ' or '


class UndefinedExpr(Expr):
    def __init__(self) -> None:
        pass

    def __repr__(self) -> str:
        return 'UNDEFINED'

    def __neg__(self) -> Expr:
        return self

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        return self


UNDEFINED = UndefinedExpr()


class Number(Expr):
    def __init__(self, value: int) -> None:
        self.value = value

    def __repr__(self) -> str:
        if self.value < 0:
            return '({})'.format(self.value)
        return str(self.value)

    def __hash__(self) -> int:
        return hash(self.value)

    def __int__(self) -> int:
        return self.value

    def __neg__(self) -> 'Number':
        return Number(-self.value)

    def __add__(self, other: object) -> 'Number':
        if isinstance(other, Number):
            return Number(self.value + other.value)
        return NotImplemented

    def __sub__(self, other: object) -> 'Number':
        if isinstance(other, Number):
            return Number(self.value - other.value)
        return NotImplemented

    def __mul__(self, other: object) -> 'Number':
        if isinstance(other, Number):
            return Number(self.value * other.value)
        return NotImplemented

    def __floordiv__(self, other: object) -> Expr:
        if isinstance(other, Number):
            if self.value % other.value == 0:
                return Number(self.value // other.value)
            return Div(Number(self.value), Number(other.value))
        return NotImplemented

    def __pow__(self, other: object) -> 'Number':
        if isinstance(other, Number):
            return Number(self.value ** other.value)
        return NotImplemented

    def __lt__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value < other.value
        if isinstance(other, Expr):
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value <= other.value
        if isinstance(other, Expr):
            return False
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value > other.value
        if isinstance(other, Expr):
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value >= other.value
        if isinstance(other, Expr):
            return False
        return NotImplemented

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        return self

    def to_bytes(self) -> Expr:
        return Number(self.value // 8)


class AssExpr(Expr):
    def __init__(self, *terms: Expr) -> None:
        self.terms = list(terms)

    def __repr__(self) -> str:
        return '({})'.format(self.symbol().join(map(str, self.terms)))

    @abstractmethod
    def __neg__(self) -> Expr:
        raise NotImplementedError

    def __contains__(self, item: Expr) -> bool:
        return item == self or any(item in term for term in self.terms)

    def __lt__(self, other: object) -> bool:
        if isinstance(other, AssExpr):
            if len(self.terms) == len(other.terms):
                lt = [x < y for x, y in zip(self.terms, other.terms)]
                eq = [x == y for x, y in zip(self.terms, other.terms)]
                return any(lt) and all(map((lambda x: x[0] or x[1]), zip(lt, eq)))
            return False
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, AssExpr):
            if len(self.terms) == len(other.terms):
                return all([x <= y for x, y in zip(self.terms, other.terms)])
            return False
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, AssExpr):
            if len(self.terms) == len(other.terms):
                gt = [x > y for x, y in zip(self.terms, other.terms)]
                eq = [x == y for x, y in zip(self.terms, other.terms)]
                return any(gt) and all(map((lambda x: x[0] or x[1]), zip(gt, eq)))
            return False
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, AssExpr):
            if len(self.terms) == len(other.terms):
                return all([x >= y for x, y in zip(self.terms, other.terms)])
            return False
        return NotImplemented

    def converted(self, replace_function: Callable[[Expr], Expr]) -> Expr:
        terms: List[Expr] = []
        for term in self.terms:
            terms.append(term.converted(replace_function))
        return self.__class__(*terms)

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        terms: List[Expr] = []
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

    def to_bytes(self) -> Expr:
        return self.__class__(*[term.to_bytes() for term in self.terms])


class Add(AssExpr):
    def __neg__(self) -> Expr:
        return Add(*[-term for term in self.terms])

    def operation(self, left: int, right: int) -> int:
        return left + right

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        expr = super().simplified(facts)
        if not isinstance(expr, Add):
            return expr
        terms: List[Expr] = []
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
        return ' + '


class Mul(AssExpr):
    def __neg__(self) -> Expr:
        return Mul(*list(self.terms) + [Number(-1)]).simplified()

    def operation(self, left: int, right: int) -> int:
        return left * right

    def neutral_element(self) -> int:
        return 1

    def symbol(self) -> str:
        return ' * '


class Sub(BinExpr):
    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
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


class Div(BinExpr):
    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left // right
        if isinstance(left, Add) and isinstance(right, Number):
            return Add(*[Div(term, right) for term in left.terms]).simplified(facts)
        if isinstance(left, Mul) and isinstance(right, Number):
            terms: List[Expr] = []
            for term in left.terms:
                if isinstance(term, Number):
                    terms.append((term // right).simplified(facts))
                else:
                    terms.append(term)
            return Mul(*terms).simplified(facts)
        return Div(left, right)

    def symbol(self) -> str:
        return ' / '


class Pow(BinExpr):
    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left**right
        return Pow(left, right)

    def symbol(self) -> str:
        return '**'


class Attribute(Expr):
    def __init__(self, name: Union[str, Expr], negative: bool = False) -> None:
        if isinstance(name, str):
            verify_identifier(name)
        self.name = str(name)
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

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        if facts:
            positive_self = self.__class__(self.name)
            if positive_self in facts:
                if positive_self in facts[positive_self]:
                    raise ExpressionError(f'self-reference to "{positive_self}"')
                return -facts[positive_self] if self.negative else facts[positive_self]
        return self


class Value(Attribute):
    def __repr__(self) -> str:
        if self.negative:
            return '(-{})'.format(self.name)
        return self.name


class LengthValue(Attribute):
    def __repr__(self) -> str:
        if self.negative:
            return '(-{})'.format(self.name)
        return f'LengthValue({self.name})'


class Size(Attribute):
    pass


class Length(Attribute):
    pass


class First(Attribute):
    pass


class Last(Attribute):
    pass


class Aggregate(Expr):
    def __init__(self, *elements: Expr) -> None:
        self.elements = list(elements)

    def __repr__(self) -> str:
        return '({})'.format(', '.join(map(str, self.elements)))

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        return self


class Relation(Expr):
    def __init__(self, left: Expr, right: Expr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '{} {} {}'.format(self.left, self.symbol(), self.right)

    @abstractmethod
    def __neg__(self) -> Expr:
        raise NotImplementedError

    def __contains__(self, item: Expr) -> bool:
        return item == self or item in (self.left, self.right)

    def simplified(self, facts: Dict['Attribute', Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        return self.__class__(left, right)

    def converted(self, replace_function: Callable[[Expr], Expr]) -> Expr:
        return self.__class__(self.left.converted(replace_function),
                              self.right.converted(replace_function))

    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class Less(Relation):
    def __neg__(self) -> Expr:
        return GreaterEqual(self.left, self.right)

    def symbol(self) -> str:
        return '<'


class LessEqual(Relation):
    def __neg__(self) -> Expr:
        return Greater(self.left, self.right)

    def symbol(self) -> str:
        return '<='


class Equal(Relation):
    def __neg__(self) -> Expr:
        return NotEqual(self.left, self.right)

    def symbol(self) -> str:
        return '='


class GreaterEqual(Relation):
    def __neg__(self) -> Expr:
        return Less(self.left, self.right)

    def symbol(self) -> str:
        return '>='


class Greater(Relation):
    def __neg__(self) -> Expr:
        return LessEqual(self.left, self.right)

    def symbol(self) -> str:
        return '>'


class NotEqual(Relation):
    def __neg__(self) -> Expr:
        return Equal(self.left, self.right)

    def symbol(self) -> str:
        return '/='


class Call(Expr):
    def __init__(self, name: str, args: List[Expr] = None, negative: bool = False) -> None:
        verify_identifier(name)
        self.name = name
        self.args = args or []
        self.negative = negative

    def __repr__(self) -> str:
        args = ', '.join(map(str, self.args))
        if args:
            args = f' ({args})'
        call = f'{self.name}{args}'
        if self.negative:
            return f'(-{call})'
        return call

    def __neg__(self) -> Expr:
        return self.__class__(self.name, self.args, not self.negative)

    def simplified(self, facts: Dict[Attribute, Expr] = None) -> Expr:
        return self


class Slice(Expr):
    def __init__(self, name: str, first: Expr, last: Expr) -> None:
        verify_identifier(name)
        self.name = name
        self.first = first
        self.last = last

    def __repr__(self) -> str:
        return f'{self.name} ({self.first} .. {self.last})'

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self, facts: Dict[Attribute, Expr] = None) -> Expr:
        return Slice(self.name, self.first.simplified(facts), self.last.simplified(facts))


class If(Expr):
    def __init__(self, condition_expressions: List[Tuple[Expr, Expr]],
                 else_expression: str = '') -> None:
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

    def __repr__(self) -> str:
        result = ''
        for c, e in self.condition_expressions:
            if not result:
                result = f'(if {c} then {e}'
            else:
                result += f' elsif {c} then {e}'
        if self.else_expression:
            result += f' else {self.else_expression}'
        result += ')'
        return result

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self, facts: Dict[Attribute, Expr] = None) -> Expr:
        return If([(x.simplified(), y.simplified()) for x, y in self.condition_expressions],
                  self.else_expression)


class Case(Expr):
    def __init__(self, control_expression: Expr,
                 case_statements: List[Tuple[Expr, Expr]]) -> None:
        self.control_expression = control_expression
        self.case_statements = case_statements

    def __repr__(self) -> str:
        grouped_cases = [(' | '.join(str(c) for c, _ in choices), expr)
                         for expr, choices in itertools.groupby(self.case_statements,
                                                                lambda x: x[1])]
        cases = ', '.join([f'when {choice} => {expr}'
                           for choice, expr in grouped_cases])
        return f'case {self.control_expression} is {cases}'

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self, facts: Dict[Attribute, Expr] = None) -> Expr:
        return Case(self.control_expression.simplified(),
                    [(x.simplified(), y.simplified()) for x, y in self.case_statements])


class ExpressionError(Exception):
    pass


def verify_identifier(name: str) -> None:
    if ' ' in name:
        raise ExpressionError(f'whitespace in identifier "{name}"')
