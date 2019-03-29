from abc import ABC, abstractmethod
from typing import Callable, Dict, List


class Expr(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')


class LogExpr(Expr):
    @abstractmethod
    def __contains__(self, item: Expr) -> bool:
        raise NotImplementedError

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

    def __contains__(self, item: Expr) -> bool:
        return item == self

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

    def __contains__(self, item: Expr) -> bool:
        return item in self.left or item in self.right

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
    def converted(self, replace_function: Callable[['MathExpr'], 'MathExpr']) -> 'MathExpr':
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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
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

    def __hash__(self) -> int:
        return hash(self.value)

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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        return replace_function(self)

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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        terms: List[MathExpr] = []
        for term in self.terms:
            terms.append(term.converted(replace_function))
        return self.__class__(*terms)

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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        return self.__class__(self.left.converted(replace_function),
                              self.right.converted(replace_function))

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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        return replace_function(self)

    def simplified(self, facts: Dict['Attribute', MathExpr] = None) -> MathExpr:
        if facts:
            positive_self = self.__class__(self.name)
            if positive_self in facts:
                if positive_self in facts[positive_self]:
                    raise ExpressionError(f'self-reference to "{positive_self}"')
                return -facts[positive_self] if self.negative else facts[positive_self]
        return self

    def to_bytes(self) -> MathExpr:
        return self


class Value(Attribute):
    def __str__(self) -> str:
        if self.negative:
            return '(-{})'.format(self.name)
        return self.name


class LengthValue(Attribute):
    def __str__(self) -> str:
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


class Relation(LogExpr):
    def __init__(self, left: MathExpr, right: MathExpr) -> None:
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return '{}({}, {})'.format(self.__class__.__name__, self.left, self.right)

    def __str__(self) -> str:
        return '{} {} {}'.format(self.left, self.symbol(), self.right)

    def __contains__(self, item: Expr) -> bool:
        return isinstance(item, MathExpr) and item in (self.left, self.right)

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


class ExpressionError(Exception):
    pass
