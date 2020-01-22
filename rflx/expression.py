# pylint: disable=too-many-lines
import itertools
from abc import ABC, abstractmethod, abstractproperty
from copy import copy
from enum import Enum
from typing import Callable, List, Mapping, Optional, Sequence, Tuple, Union

import z3

from rflx.common import indent, indent_next, unique, verify_identifier


class Precedence(Enum):
    undefined = 0
    logical_operator = 1
    relational_operator = 2
    binary_adding_operator = 3
    unary_adding_operator = 4
    multiplying_operator = 5
    highest_precedence_operator = 6
    literal = 7


class ProofResult(Enum):
    sat = z3.sat
    unsat = z3.unsat
    unknown = z3.unknown


class Expr(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = "\n\t" + ",\n\t".join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f"{self.__class__.__name__}({args})".replace("\t", "\t    ")

    def __hash__(self) -> int:
        return hash(repr(self))

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

    def __contains__(self, item: "Expr") -> bool:
        return item == self

    @abstractmethod
    def __neg__(self) -> "Expr":
        raise NotImplementedError

    @abstractproperty
    def precedence(self) -> Precedence:
        raise NotImplementedError

    # pylint: disable=unused-argument,no-self-use
    def variables(self, proof: bool = False) -> List["Variable"]:
        return []

    def converted(self, replace_function: Callable[["Expr"], "Expr"]) -> "Expr":
        return replace_function(self)

    @abstractmethod
    def simplified(self, facts: Mapping["Name", "Expr"] = None) -> "Expr":
        raise NotImplementedError

    def parenthesized(self, expr: "Expr") -> str:
        if expr.precedence.value <= self.precedence.value:
            return f"({expr})"
        return str(expr)

    @abstractmethod
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def __solve(self, forall: bool = False) -> ProofResult:
        cond = self.z3expr()
        variables = {v for v in self.variables(True) if isinstance(v.name, str)}
        if variables:
            if forall:
                cond = z3.ForAll([v.z3expr() for v in variables], cond)
            else:
                cond = z3.Exists([v.z3expr() for v in variables], cond)
        solver = z3.Solver()
        solver.add(cond)
        return ProofResult(solver.check())

    def forall(self) -> ProofResult:
        return self.__solve(True)

    def exists(self) -> ProofResult:
        return self.__solve()


class BooleanLiteral(Expr):
    @abstractmethod
    def __neg__(self) -> "Expr":
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        return self


class BooleanTrue(BooleanLiteral):
    def __str__(self) -> str:
        return "True"

    def __neg__(self) -> Expr:
        return FALSE

    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(True)


TRUE = BooleanTrue()


class BooleanFalse(BooleanLiteral):
    def __str__(self) -> str:
        return "False"

    def __neg__(self) -> Expr:
        return TRUE

    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(False)


FALSE = BooleanFalse()


class Not(Expr):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    def __str__(self) -> str:
        return f"not {self.parenthesized(self.expr)}"

    def __neg__(self) -> Expr:
        return self.expr

    def variables(self, proof: bool = False) -> List["Variable"]:
        return self.expr.variables(proof)

    @property
    def precedence(self) -> Precedence:
        return Precedence.highest_precedence_operator

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        return self

    def z3expr(self) -> z3.BoolRef:
        z3expr = self.expr.z3expr()
        if isinstance(z3expr, z3.BoolRef):
            return z3.Not(z3expr)
        raise TypeError


class BinExpr(Expr):
    def __init__(self, left: Expr, right: Expr) -> None:
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return f"{self.parenthesized(self.left)}{self.symbol}{self.parenthesized(self.right)}"

    def __neg__(self) -> Expr:
        return self.__class__(-self.left, self.right)

    def __contains__(self, item: Expr) -> bool:
        return item == self or item in (self.left, self.right)

    @abstractproperty
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def variables(self, proof: bool = False) -> List["Variable"]:
        return list(unique(self.left.variables(proof) + self.right.variables(proof)))

    def converted(self, replace_function: Callable[[Expr], Expr]) -> Expr:
        left = self.left.converted(replace_function)
        right = self.right.converted(replace_function)
        return replace_function(self.__class__(left, right))

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        return self.__class__(left, right)

    @abstractproperty
    def symbol(self) -> str:
        raise NotImplementedError


class AssExpr(Expr):
    def __init__(self, *terms: Expr) -> None:
        self.terms = list(terms)

    def __str__(self) -> str:
        if not self.terms:
            return str(self.neutral_element())
        return self.symbol.join(map(self.parenthesized, self.terms))

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

    @abstractproperty
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def variables(self, proof: bool = False) -> List["Variable"]:
        return list(unique([v for t in self.terms for v in t.variables(proof)]))

    def converted(self, replace_function: Callable[[Expr], Expr]) -> Expr:
        terms: List[Expr] = []
        for term in self.terms:
            terms.append(term.converted(replace_function))
        return replace_function(self.__class__(*terms))

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        terms: List[Expr] = []
        all_terms = list(self.terms)
        total = self.neutral_element()
        for term in all_terms:
            t = term.simplified(facts)
            if isinstance(t, Number):
                total = self.operation(total, t.value)
            elif isinstance(t, BooleanTrue):
                total = self.operation(total, 1)
            elif isinstance(t, BooleanFalse):
                total = self.operation(total, 0)
            elif isinstance(t, type(self)):
                all_terms += t.terms
            else:
                terms.append(t)
        boolean = isinstance(self, (And, Or))
        if not terms:
            if boolean:
                return TRUE if total else FALSE
            return Number(total)
        if total != self.neutral_element():
            if boolean:
                terms.append(TRUE if total else FALSE)
            else:
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

    @abstractproperty
    def symbol(self) -> str:
        raise NotImplementedError


class LogExpr(AssExpr):
    def __str__(self) -> str:
        if not self.terms:
            return str(TRUE)
        return indent_next(f"\n{self.symbol}".join(map(self.parenthesized, self.terms)), 2)

    @abstractmethod
    def operation(self, left: int, right: int) -> int:
        raise NotImplementedError

    @abstractmethod
    def neutral_element(self) -> int:
        raise NotImplementedError

    @abstractproperty
    def symbol(self) -> str:
        raise NotImplementedError


class And(LogExpr):
    def __str__(self) -> str:
        return super().__str__() if self.terms else str(TRUE)

    def __neg__(self) -> Expr:
        return And(*[-term for term in self.terms])

    @property
    def precedence(self) -> Precedence:
        return Precedence.logical_operator

    def operation(self, left: int, right: int) -> int:
        return left and right

    def neutral_element(self) -> int:
        return 1

    @property
    def symbol(self) -> str:
        return " and "

    def z3expr(self) -> z3.BoolRef:
        z3exprs = [t.z3expr() for t in self.terms]
        boolexprs = [t for t in z3exprs if isinstance(t, z3.BoolRef)]
        if len(z3exprs) != len(boolexprs):
            raise TypeError
        return z3.And(*boolexprs)


class AndThen(And):
    @property
    def symbol(self) -> str:
        return " and then "


class Or(LogExpr):
    def __neg__(self) -> Expr:
        return Or(*[-term for term in self.terms])

    @property
    def precedence(self) -> Precedence:
        return Precedence.logical_operator

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        simplified_expr = super().simplified(facts)
        if isinstance(simplified_expr, Or):
            if TRUE in simplified_expr.terms:
                return TRUE
        return simplified_expr

    def operation(self, left: int, right: int) -> int:
        return left or right

    def neutral_element(self) -> int:
        return 0

    @property
    def symbol(self) -> str:
        return " or "

    def z3expr(self) -> z3.BoolRef:
        z3exprs = [t.z3expr() for t in self.terms]
        boolexprs = [t for t in z3exprs if isinstance(t, z3.BoolRef)]
        if len(z3exprs) != len(boolexprs):
            raise TypeError
        return z3.Or(*boolexprs)


class Number(Expr):
    def __init__(self, value: int, base: int = 0) -> None:
        self.value = value
        self.base = base

    def __str__(self) -> str:
        value = self.value if self.value >= 0 else -self.value
        if self.base == 0:
            data = "{}".format(value)
        elif self.base == 2:
            data = "2#{:b}#".format(value)
        elif self.base == 8:
            data = "8#{:o}#".format(value)
        elif self.base == 10:
            data = "10#{}#".format(value)
        elif self.base == 16:
            data = "16#{:X}#".format(value)
        else:
            raise NotImplementedError(f"unsupported base {self.base}")

        if self.value < 0:
            return f"(-{data})"
        return data

    def __hash__(self) -> int:
        return hash(self.value)

    def __int__(self) -> int:
        return self.value

    def __neg__(self) -> "Number":
        return Number(-self.value)

    def __add__(self, other: object) -> "Number":
        if isinstance(other, Number):
            return Number(self.value + other.value)
        return NotImplemented

    def __sub__(self, other: object) -> "Number":
        if isinstance(other, Number):
            return Number(self.value - other.value)
        return NotImplemented

    def __mul__(self, other: object) -> "Number":
        if isinstance(other, Number):
            return Number(self.value * other.value)
        return NotImplemented

    def __floordiv__(self, other: object) -> Expr:
        if isinstance(other, Number):
            if self.value % other.value == 0:
                return Number(self.value // other.value)
            return Div(Number(self.value), Number(other.value))
        return NotImplemented

    def __pow__(self, other: object) -> "Number":
        if isinstance(other, Number):
            return Number(self.value ** other.value)
        return NotImplemented

    def __mod__(self, other: object) -> "Number":
        if isinstance(other, Number):
            return Number(self.value % other.value)
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

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        return self

    def z3expr(self) -> z3.ArithRef:
        return z3.IntVal(self.value)


class Add(AssExpr):
    def __str__(self) -> str:
        if not self.terms:
            return str(self.neutral_element())
        result = str(self.terms[0])
        for t in self.terms[1:]:
            if (isinstance(t, Number) and t.value < 0) or (isinstance(t, Name) and t.negative):
                result += f" - {self.parenthesized(-t)}"
            else:
                result += f"{self.symbol}{self.parenthesized(t)}"
        return f"({result})"

    def __neg__(self) -> Expr:
        return Add(*[-term for term in self.terms])

    @property
    def precedence(self) -> Precedence:
        return Precedence.binary_adding_operator

    def operation(self, left: int, right: int) -> int:
        return left + right

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
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

    @property
    def symbol(self) -> str:
        return " + "

    def z3expr(self) -> z3.ArithRef:
        z3expr = sum(t.z3expr() for t in self.terms)
        if not isinstance(z3expr, z3.ArithRef):
            raise TypeError
        return z3expr


class Mul(AssExpr):
    def __neg__(self) -> Expr:
        return Mul(*list(self.terms) + [Number(-1)]).simplified()

    @property
    def precedence(self) -> Precedence:
        return Precedence.multiplying_operator

    def operation(self, left: int, right: int) -> int:
        return left * right

    def neutral_element(self) -> int:
        return 1

    @property
    def symbol(self) -> str:
        return " * "

    def z3expr(self) -> z3.ArithRef:
        z3expr = self.terms[0].z3expr()
        for t in self.terms[1:]:
            tmp = t.z3expr()
            if not isinstance(z3expr, z3.ArithRef) or not isinstance(tmp, z3.ArithRef):
                raise TypeError
            z3expr = z3expr * tmp
        if not isinstance(z3expr, z3.ArithRef):
            raise TypeError
        return z3expr


class Sub(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.binary_adding_operator

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left - right
        if isinstance(left, Number):
            return Add(right, -left)
        if isinstance(right, Number):
            return Add(left, -right)
        return Add(left, -right)

    @property
    def symbol(self) -> str:
        return " - "

    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            result = left - right
        if isinstance(result, z3.ArithRef):
            return result
        raise TypeError


class Div(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.multiplying_operator

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left // right
        return Div(left, right)

    @property
    def symbol(self) -> str:
        return " / "

    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            result = left / right
        if isinstance(result, z3.ArithRef):
            return result
        raise TypeError


class Pow(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.highest_precedence_operator

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left ** right
        return Pow(left, right)

    @property
    def symbol(self) -> str:
        return "**"

    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            result = left ** right
        if isinstance(result, z3.ArithRef):
            return result
        raise TypeError


class Mod(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.multiplying_operator

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        left = self.left.simplified(facts)
        right = self.right.simplified(facts)
        if isinstance(left, Number) and isinstance(right, Number):
            return left % right
        return Mod(left, right)

    @property
    def symbol(self) -> str:
        return " mod "

    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            result = left % right
        if isinstance(result, z3.ArithRef):
            return result
        raise TypeError


class Name(Expr):
    def __init__(self, name: Union[str, Expr], negative: bool = False) -> None:
        if isinstance(name, str):
            verify_identifier(name)
        self.name = name
        self.negative = negative

    def __str__(self) -> str:
        if self.negative:
            return f"(-{self.representation})"
        return self.representation

    def __hash__(self) -> int:
        return hash(str(self.name) + self.__class__.__name__)

    def __neg__(self) -> Expr:
        negated_self = copy(self)
        negated_self.negative = not self.negative
        return negated_self

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        if facts:
            positive_self = copy(self)
            positive_self.negative = False
            if positive_self in facts:
                assert (
                    positive_self not in facts[positive_self]
                ), f'self-reference to "{positive_self}"'
                return -facts[positive_self] if self.negative else facts[positive_self]
        return self

    @property
    def representation(self) -> str:
        return str(self.name)

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Variable(Name):
    def variables(self, proof: bool = False) -> List["Variable"]:
        return [self]

    def z3expr(self) -> z3.ArithRef:
        if isinstance(self.name, str):
            if self.negative:
                return -z3.Int(self.name)
            return z3.Int(self.name)
        raise TypeError


class Attribute(Name):
    @property
    def representation(self) -> str:
        return f"{self.name}'{self.__class__.__name__}"

    def z3expr(self) -> z3.ExprRef:
        if not isinstance(self.name, str):
            raise TypeError
        return z3.Int(f"{self.name}'{self.__class__.__name__}")

    def variables(self, proof: bool = False) -> List["Variable"]:
        if proof:
            if not isinstance(self.name, str):
                raise TypeError
            return [Variable(f"{self.name}'{self.__class__.__name__}")]
        return []


class Size(Attribute):
    pass


class Length(Attribute):
    pass


class First(Attribute):
    pass


class Last(Attribute):
    pass


class Range(Attribute):
    pass


class Old(Attribute):
    pass


class Result(Attribute):
    pass


class Constrained(Attribute):
    pass


class Indexed(Name):
    def __init__(self, name: Union[str, Expr], *elements: Expr) -> None:
        super().__init__(name)
        self.elements = list(elements)

    @property
    def representation(self) -> str:
        return f"{self.name} (" + ", ".join(map(str, self.elements)) + ")"

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Selected(Name):
    def __init__(self, name: Union[str, Expr], selector_name: str) -> None:
        super().__init__(name)
        verify_identifier(selector_name)
        self.selector_name = selector_name

    @property
    def representation(self) -> str:
        return f"{self.name}.{self.selector_name}"

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class UndefinedExpr(Name):
    def __init__(self, name: str = "UNDEFINED") -> None:
        super().__init__(name)

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


UNDEFINED = UndefinedExpr()


class Aggregate(Expr):
    def __init__(self, *elements: Expr) -> None:
        self.elements = list(elements)

    def __str__(self) -> str:
        return "(" + ", ".join(map(str, self.elements)) + ")"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        return self.__class__(*[e.simplified(facts) for e in self.elements])

    def z3expr(self) -> z3.ExprRef:
        return z3.BoolVal(False)


class NamedAggregate(Expr):
    def __init__(self, *elements: Tuple[str, Expr]) -> None:
        self.elements = list(elements)
        for name, _ in self.elements:
            verify_identifier(name)

    def __str__(self) -> str:
        return "(" + ", ".join(f"{name} => {element}" for name, element in self.elements) + ")"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping["Name", Expr] = None) -> Expr:
        return self.__class__(*[(n, e.simplified(facts)) for n, e in self.elements])

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Relation(BinExpr):
    @abstractmethod
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.relational_operator


class Less(Relation):
    def __neg__(self) -> Expr:
        return GreaterEqual(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " < "

    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            return left < right
        raise TypeError


class LessEqual(Relation):
    def __neg__(self) -> Expr:
        return Greater(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " <= "

    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            return left <= right
        raise TypeError


class Equal(Relation):
    def __neg__(self) -> Expr:
        return NotEqual(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " = "

    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        result = left == right
        if not isinstance(left == right, z3.BoolRef):
            raise TypeError
        return result


class GreaterEqual(Relation):
    def __neg__(self) -> Expr:
        return Less(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " >= "

    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            return left >= right
        raise TypeError


class Greater(Relation):
    def __neg__(self) -> Expr:
        return LessEqual(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " > "

    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if isinstance(left, z3.ArithRef) and isinstance(right, z3.ArithRef):
            return left > right
        raise TypeError


class NotEqual(Relation):
    def __neg__(self) -> Expr:
        return Equal(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " /= "

    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        result = left != right
        if not isinstance(result, z3.BoolRef):
            raise TypeError
        return result


class In(Relation):
    def __neg__(self) -> Expr:
        return NotIn(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " in "

    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError


class NotIn(Relation):
    def __neg__(self) -> Expr:
        return In(self.left, self.right)

    @property
    def symbol(self) -> str:
        return " not in "

    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError


class Call(Name):
    def __init__(self, name: str, args: Sequence[Expr] = None) -> None:
        super().__init__(name)
        self.args = args or []

    def __str__(self) -> str:
        args = ", ".join(map(str, self.args))
        if args:
            args = f" ({args})"
        call = f"{self.name}{args}"
        return call

    def simplified(self, facts: Mapping[Name, Expr] = None) -> Expr:
        return self

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Slice(Name):
    def __init__(self, name: Union[str, Expr], first: Expr, last: Expr) -> None:
        super().__init__(name)
        self.first = first
        self.last = last

    def __str__(self) -> str:
        return f"{self.name} ({self.first} .. {self.last})"

    def simplified(self, facts: Mapping[Name, Expr] = None) -> Expr:
        return Slice(self.name, self.first.simplified(facts), self.last.simplified(facts))

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class If(Expr):
    def __init__(
        self, condition_expressions: Sequence[Tuple[Expr, Expr]], else_expression: Expr = None
    ) -> None:
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

    def __str__(self) -> str:
        result = ""
        for c, e in self.condition_expressions:
            if not result:
                result = f"(if {indent_next(str(c), 3)} then\n {indent(str(e), 3)}"
            else:
                result += f"\n elsif {indent_next(str(c), 6)} then\n {indent(str(e), 3)}"
        if self.else_expression:
            result += f"\n else\n {indent(str(self.else_expression), 3)}"
        result += ")"
        return result

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping[Name, Expr] = None) -> Expr:
        simplified_ce = [
            (c.simplified(facts), e.simplified(facts)) for c, e in self.condition_expressions
        ]

        if len(simplified_ce) == 1 and simplified_ce[0][0] == TRUE:
            return simplified_ce[0][1]

        return If(simplified_ce, self.else_expression)

    def variables(self, proof: bool = False) -> List["Variable"]:
        variables = []
        for ce in self.condition_expressions:
            variables.extend(ce[0].variables(proof))
            variables.extend(ce[1].variables(proof))
        if self.else_expression:
            variables.extend(self.else_expression.variables(proof))
        return list(unique(variables))

    def z3expr(self) -> z3.ExprRef:
        return If.ifexpr(self.condition_expressions, self.else_expression)

    @staticmethod
    def ifexpr(conditions: Sequence[Tuple[Expr, Expr]], elseexpr: Optional[Expr]) -> z3.ExprRef:
        if conditions:
            c = conditions[0][0].z3expr()
            e = conditions[0][1].z3expr()
            r = If.ifexpr(conditions[1:], elseexpr)
            if not isinstance(c, z3.BoolRef):
                raise TypeError
            return z3.If(c, e, r)
        if elseexpr:
            return elseexpr.z3expr()
        return z3.BoolVal(False)


class Case(Expr):
    def __init__(
        self, control_expression: Expr, case_statements: Sequence[Tuple[Expr, Expr]]
    ) -> None:
        self.control_expression = control_expression
        self.case_statements = case_statements

    def __str__(self) -> str:
        grouped_cases = [
            (" | ".join(str(c) for c, _ in choices), expr)
            for expr, choices in itertools.groupby(self.case_statements, lambda x: x[1])
        ]
        cases = indent(
            ",".join(
                [f"\nwhen {choice} =>\n{indent(str(expr), 3)}" for choice, expr in grouped_cases]
            ),
            6,
        )
        return f"(case {self.control_expression} is{cases})"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping[Name, Expr] = None) -> Expr:
        if len(self.case_statements) == 1 and self.case_statements[0][0] == Name("others"):
            return self.case_statements[0][1]
        return Case(
            self.control_expression.simplified(facts),
            [(c.simplified(facts), e.simplified(facts)) for c, e in self.case_statements],
        )

    def variables(self, proof: bool = False) -> List["Variable"]:
        variables = self.control_expression.variables(proof)
        for cs in self.case_statements:
            variables.extend(cs[0].variables(proof))
            variables.extend(cs[1].variables(proof))
        return list(unique(variables))

    def z3expr(self) -> Union[z3.BoolRef, z3.ExprRef]:
        return Case.caseexpr(self.control_expression, self.case_statements)

    @staticmethod
    def caseexpr(
        control: Expr, statements: Sequence[Tuple[Expr, Expr]]
    ) -> Union[z3.ExprRef, z3.BoolRef]:
        if statements:
            condition, expression = statements[0]
            return z3.If(
                control.z3expr() == condition.z3expr(),
                expression.z3expr(),
                Case.caseexpr(control, statements[1:]),
            )
        return z3.BoolVal(False)


class QuantifiedExpression(Expr):
    def __init__(self, parameter_name: str, iterable: Expr, predicate: Expr) -> None:
        self.parameter_name = parameter_name
        self.iterable = iterable
        self.predicate = predicate

    def __str__(self) -> str:
        return (
            f"(for {self.quantifier} {self.parameter_name} {self.keyword} {self.iterable} =>\n"
            f"   {self.predicate})"
        )

    @property
    def precedence(self) -> Precedence:
        return Precedence.literal

    def simplified(self, facts: Mapping[Name, Expr] = None) -> Expr:
        return self.__class__(
            self.parameter_name, self.iterable.simplified(facts), self.predicate.simplified(facts)
        )

    @abstractproperty
    def quantifier(self) -> str:
        raise NotImplementedError

    @abstractproperty
    def keyword(self) -> str:
        raise NotImplementedError

    def variables(self, proof: bool = False) -> List["Variable"]:
        return list(unique(self.iterable.variables(proof) + self.predicate.variables(proof)))

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class ForAllOf(QuantifiedExpression):
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def quantifier(self) -> str:
        return "all"

    @property
    def keyword(self) -> str:
        return "of"


class ForAllIn(QuantifiedExpression):
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def quantifier(self) -> str:
        return "all"

    @property
    def keyword(self) -> str:
        return "in"


class ValueRange(Expr):
    def __init__(self, lower: Expr, upper: Expr):
        self.lower = lower
        self.upper = upper

    def __str__(self) -> str:
        return f"{self.lower} .. {self.upper}"

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def simplified(self, facts: Mapping[Name, Expr] = None) -> Expr:
        return self.__class__(self.lower.simplified(facts), self.upper.simplified(facts))

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError
