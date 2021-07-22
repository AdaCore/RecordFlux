# pylint: disable=too-many-lines,too-many-ancestors,too-many-arguments
import difflib
import itertools
import operator
from abc import abstractmethod
from enum import Enum
from functools import lru_cache
from sys import intern
from typing import Callable, Iterable, List, Mapping, Optional, Sequence, Tuple, Type, Union

import z3

import rflx.ada as ada
import rflx.typing_ as rty
from rflx.common import Base, indent, indent_next, unique
from rflx.contract import DBC, invariant, require
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID


class Z3TypeError(TypeError):
    pass


class Precedence(Enum):
    UNDEFINED = 0
    BOOLEAN_OPERATOR = 1
    RELATIONAL_OPERATOR = 2
    BINARY_ADDING_OPERATOR = 3
    UNARY_ADDING_OPERATOR = 4
    MULTIPLYING_OPERATOR = 5
    HIGHEST_PRECEDENCE_OPERATOR = 6
    LITERAL = 7


class ProofResult(Enum):
    SAT = z3.sat
    UNSAT = z3.unsat
    UNKNOWN = z3.unknown


class Proof:
    def __init__(
        self, expr: "Expr", facts: Optional[Sequence["Expr"]] = None, logic: str = "QF_NIA"
    ):
        self.__expr = expr
        self.__facts = facts or []
        self.__result = ProofResult.UNSAT
        self.__logic = logic
        self.__unknown_reason: Optional[str] = None

        solver = z3.SolverFor(self.__logic)
        solver.add(self.__expr.z3expr())
        for f in self.__facts:
            solver.add(f.z3expr())

        self.__result = ProofResult(solver.check())
        if self.__result == ProofResult.UNKNOWN:
            self.__unknown_reason = solver.reason_unknown()

    @property
    def result(self) -> ProofResult:
        return self.__result

    @property
    def error(self) -> List[Tuple[str, Optional[Location]]]:
        assert self.__result != ProofResult.SAT
        if self.__result == ProofResult.UNKNOWN:
            assert self.__unknown_reason is not None
            return [(self.__unknown_reason, None)]
        solver = z3.SolverFor(self.__logic)
        solver.set(unsat_core=True)
        facts = {f"H{index}": fact for index, fact in enumerate(self.__facts)}
        for name, fact in facts.items():
            solver.assert_and_track(fact.z3expr(), name)

        solver.assert_and_track(self.__expr.z3expr(), "goal")
        facts["goal"] = self.__expr
        result = solver.check()
        assert result == z3.unsat, f"result should be unsat (is {result})"
        return [
            (" ".join(str(facts[str(fact)]).replace("\n", " ").split()), facts[fact].location)
            for fact in sorted([str(h) for h in solver.unsat_core()])
        ]


class Expr(DBC, Base):
    _str: str

    def __init__(
        self,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ):
        self.type_ = type_
        self.location = location

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return str(self) == str(other)
        return NotImplemented

    def __str__(self) -> str:
        try:
            return self._str
        except AttributeError:
            self._update_str()
            return self._str

    def __hash__(self) -> int:
        return hash(self.__class__.__name__)

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

    @abstractmethod
    def _update_str(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def _check_type_subexpr(self) -> RecordFluxError:
        """Initialize and check the types of sub-expressions."""
        raise NotImplementedError

    def check_type(self, expected: Union[rty.Type, Tuple[rty.Type, ...]]) -> RecordFluxError:
        """Initialize and check the types of the expression and all sub-expressions."""
        return self._check_type_subexpr() + rty.check_type(
            self.type_, expected, self.location, _entity_name(self)
        )

    def check_type_instance(
        self, expected: Union[Type[rty.Type], Tuple[Type[rty.Type], ...]]
    ) -> RecordFluxError:
        """Initialize and check the types of the expression and all sub-expressions."""
        return self._check_type_subexpr() + rty.check_type_instance(
            self.type_, expected, self.location, _entity_name(self)
        )

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    # pylint: disable=no-self-use
    def variables(self) -> List["Variable"]:
        return []

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [self] if match(self) else []

    @require(lambda func, mapping: (func and mapping is None) or (not func and mapping is not None))
    def substituted(
        self, func: Callable[["Expr"], "Expr"] = None, mapping: Mapping["Name", "Expr"] = None
    ) -> "Expr":
        func = substitution(mapping or {}, func)
        return func(self)

    @abstractmethod
    def simplified(self) -> "Expr":
        raise NotImplementedError

    def parenthesized(self, expr: "Expr") -> str:
        if expr.precedence.value <= self.precedence.value:
            return "(" + indent_next(str(expr), 1) + ")"
        return str(expr)

    @abstractmethod
    def ada_expr(self) -> ada.Expr:
        raise NotImplementedError

    @abstractmethod
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def check(self, facts: Optional[Sequence["Expr"]] = None) -> Proof:
        return Proof(self, facts)


class BooleanLiteral(Expr):
    _str: str

    def __init__(self, location: Location = None) -> None:
        super().__init__(rty.BOOLEAN, location)
        self._update_str()

    @abstractmethod
    def __neg__(self) -> "Expr":
        raise NotImplementedError

    def _check_type_subexpr(self) -> RecordFluxError:
        return RecordFluxError()

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def simplified(self) -> Expr:
        return self


class BooleanTrue(BooleanLiteral):
    def _update_str(self) -> None:
        self._str = intern("True")

    def __repr__(self) -> str:
        return "TRUE"

    def __neg__(self) -> Expr:
        return FALSE

    def ada_expr(self) -> ada.Expr:
        return ada.TRUE

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(True)


TRUE = BooleanTrue()


class BooleanFalse(BooleanLiteral):
    def _update_str(self) -> None:
        self._str = intern("False")

    def __repr__(self) -> str:
        return "FALSE"

    def __neg__(self) -> Expr:
        return TRUE

    def ada_expr(self) -> ada.Expr:
        return ada.FALSE

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(False)


FALSE = BooleanFalse()


class Not(Expr):
    def __init__(self, expr: Expr) -> None:
        super().__init__(rty.BOOLEAN)
        self.expr = expr

    def _update_str(self) -> None:
        self._str = intern(f"not {self.parenthesized(self.expr)}")

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.expr.check_type(rty.BOOLEAN)

    def __neg__(self) -> Expr:
        return self.expr

    def variables(self) -> List["Variable"]:
        return self.expr.variables()

    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR

    def simplified(self) -> Expr:
        for relation, inverse_relation in [
            (Less, GreaterEqual),
            (LessEqual, Greater),
            (Equal, NotEqual),
            (GreaterEqual, Less),
            (Greater, LessEqual),
            (NotEqual, Equal),
        ]:
            if isinstance(self.expr, relation):
                return inverse_relation(self.expr.left.simplified(), self.expr.right.simplified())
        return self.__class__(self.expr.simplified())

    def ada_expr(self) -> ada.Expr:
        return ada.Not(self.expr.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        z3expr = self.expr.z3expr()
        if not isinstance(z3expr, z3.BoolRef):
            raise Z3TypeError("negating non-boolean term")
        return z3.Not(z3expr)


class BinExpr(Expr):
    def __init__(
        self, left: Expr, right: Expr, type_: rty.Type = rty.Undefined(), location: Location = None
    ) -> None:
        super().__init__(type_, location)
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return (
            f"\n{self.__class__.__name__}(\n"
            + ",\n".join([indent(repr(t), 4) for t in [self.left, self.right]])
            + ")"
        )

    def _update_str(self) -> None:
        self._str = intern(
            f"{self.parenthesized(self.left)}{self.symbol}{self.parenthesized(self.right)}"
        )

    def __neg__(self) -> Expr:
        return self.__class__(-self.left, self.right)

    def __contains__(self, item: Expr) -> bool:
        return item == self or item in (self.left, self.right)

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return list(unique(self.left.variables() + self.right.variables()))

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *self.left.findall(match),
            *self.right.findall(match),
        ]

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping["Name", Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, BinExpr):
            return expr.__class__(
                expr.left.substituted(func),
                expr.right.substituted(func),
                location=expr.location,
            )
        return expr

    def simplified(self) -> Expr:
        return self.__class__(self.left.simplified(), self.right.simplified())

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class AssExpr(Expr):
    def __init__(self, *terms: Expr, location: Location = None) -> None:
        super().__init__(rty.Undefined(), location)
        self.terms = list(terms)

    def __repr__(self) -> str:
        return (
            f"\n{self.__class__.__name__}(\n"
            + ",\n".join([indent(repr(t), 4) for t in self.terms])
            + ")"
        )

    def _update_str(self) -> None:
        self._str = intern(
            self.symbol.join(map(self.parenthesized, self.terms))
            if self.terms
            else str(self.neutral_element())
        )

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
                return all(x <= y for x, y in zip(self.terms, other.terms))
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
                return all(x >= y for x, y in zip(self.terms, other.terms))
            return False
        return NotImplemented

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return list(unique([v for t in self.terms for v in t.variables()]))

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *[m for t in self.terms for m in t.findall(match)],
        ]

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping["Name", Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, AssExpr):
            return expr.__class__(
                *[t.substituted(func) for t in expr.terms], location=expr.location
            )
        return expr

    def simplified(self) -> Expr:
        terms: List[Expr] = []
        all_terms = list(self.terms)
        total = self.neutral_element()
        for term in all_terms:
            t = term.simplified()
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
        return self.__class__(*terms, location=self.location)

    @abstractmethod
    def operation(self, left: int, right: int) -> int:
        raise NotImplementedError

    @abstractmethod
    def neutral_element(self) -> int:
        raise NotImplementedError

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class BoolAssExpr(AssExpr):
    def __init__(self, *terms: Expr, location: Location = None) -> None:
        super().__init__(*terms, location=location)
        self.type_ = rty.BOOLEAN

    def _update_str(self) -> None:
        if not self.terms:
            self._str = str(TRUE)
            return
        self._str = ""
        for i, t in reversed(list(enumerate(self.terms))):
            if i == 0:
                self._str = self.parenthesized(t) + self._str
            else:
                self._str = (
                    "\n"
                    + str(self.symbol)[1:]
                    + indent_next(self.parenthesized(t), len(self.symbol) - 1)
                    + self._str
                )
        self._str = intern(self._str)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for t in self.terms:
            error += t.check_type(rty.BOOLEAN)
        return error

    @abstractmethod
    def operation(self, left: int, right: int) -> int:
        raise NotImplementedError

    @abstractmethod
    def neutral_element(self) -> int:
        raise NotImplementedError

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class And(BoolAssExpr):
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.BOOLEAN_OPERATOR

    def simplified(self) -> Expr:
        simplified_expr = super().simplified()
        if isinstance(simplified_expr, And) and FALSE in simplified_expr.terms:
            return FALSE
        return simplified_expr

    def operation(self, left: int, right: int) -> int:
        return left and right

    def neutral_element(self) -> int:
        return 1

    @property
    def symbol(self) -> str:
        return " and "

    def ada_expr(self) -> ada.Expr:
        return ada.And(*[t.ada_expr() for t in self.terms])

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        z3exprs = [t.z3expr() for t in self.terms]
        boolexprs = [t for t in z3exprs if isinstance(t, z3.BoolRef)]
        if len(z3exprs) != len(boolexprs):
            raise Z3TypeError("conjunction of non-boolean terms")
        return z3.And(*boolexprs)


class AndThen(And):
    @property
    def symbol(self) -> str:
        return " and then "

    def ada_expr(self) -> ada.Expr:
        return ada.AndThen(*[t.ada_expr() for t in self.terms])


class Or(BoolAssExpr):
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.BOOLEAN_OPERATOR

    def simplified(self) -> Expr:
        simplified_expr = super().simplified()
        if isinstance(simplified_expr, Or) and TRUE in simplified_expr.terms:
            return TRUE
        return simplified_expr

    def operation(self, left: int, right: int) -> int:
        return left or right

    def neutral_element(self) -> int:
        return 0

    @property
    def symbol(self) -> str:
        return " or "

    def ada_expr(self) -> ada.Expr:
        return ada.Or(*[t.ada_expr() for t in self.terms])

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        z3exprs = [t.z3expr() for t in self.terms]
        boolexprs = [t for t in z3exprs if isinstance(t, z3.BoolRef)]
        if len(z3exprs) != len(boolexprs):
            raise Z3TypeError("disjunction of non-boolean terms")
        return z3.Or(*boolexprs)


class OrElse(Or):
    @property
    def symbol(self) -> str:
        return " or else "

    def ada_expr(self) -> ada.Expr:
        return ada.OrElse(*[t.ada_expr() for t in self.terms])


class Number(Expr):
    def __init__(self, value: int, base: int = 0, location: Location = None) -> None:
        super().__init__(rty.UniversalInteger(rty.Bounds(value, value)), location)
        self.value = value
        self.base = base

    def _update_str(self) -> None:
        value = self.value if self.value >= 0 else -self.value
        if self.base == 0:
            self._str = "{}".format(value)
        elif self.base == 2:
            self._str = "2#{:b}#".format(value)
        elif self.base == 8:
            self._str = "8#{:o}#".format(value)
        elif self.base == 10:
            self._str = "10#{}#".format(value)
        elif self.base == 16:
            self._str = "16#{:X}#".format(value)
        else:
            raise NotImplementedError(f"unsupported base {self.base}")
        self._str = intern(f"(-{self._str})" if self.value < 0 else self._str)

    def _check_type_subexpr(self) -> RecordFluxError:
        return RecordFluxError()

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

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value == other.value
        if isinstance(other, Expr):
            return False
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
        return Precedence.LITERAL

    def simplified(self) -> Expr:
        return self

    def ada_expr(self) -> ada.Expr:
        return ada.Number(self.value, self.base)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        return z3.IntVal(self.value)


class MathAssExpr(AssExpr):
    def __init__(self, *terms: Expr, location: Location = None) -> None:
        super().__init__(*terms, location=location)
        common_type = rty.common_type([t.type_ for t in terms])
        self.type_ = common_type if common_type != rty.Undefined() else rty.UndefinedInteger()

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for t in self.terms:
            error += t.check_type_instance(rty.AnyInteger)
        return error


class Add(MathAssExpr):
    def _update_str(self) -> None:
        if not self.terms:
            self._str = intern(str(self.neutral_element()))
            return
        self._str = str(self.terms[0])
        for t in self.terms[1:]:
            if (isinstance(t, Number) and t.value < 0) or (isinstance(t, Name) and t.negative):
                self._str += f" - {self.parenthesized(-t)}"
            else:
                self._str += f"{self.symbol}{self.parenthesized(t)}"
        self._str = intern(self._str)

    def __neg__(self) -> Expr:
        return Add(*[-term for term in self.terms])

    @property
    def precedence(self) -> Precedence:
        return Precedence.BINARY_ADDING_OPERATOR

    def operation(self, left: int, right: int) -> int:
        return left + right

    def simplified(self) -> Expr:
        expr = super().simplified()
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
        return Add(*terms, location=self.location)

    def neutral_element(self) -> int:
        return 0

    @property
    def symbol(self) -> str:
        return " + "

    def ada_expr(self) -> ada.Expr:
        return ada.Add(*[t.ada_expr() for t in self.terms])

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        terms = [t for t in map(lambda e: e.z3expr(), self.terms) if isinstance(t, z3.ArithRef)]
        if len(terms) != len(self.terms):
            raise Z3TypeError("adding non-arithmetic terms")
        return z3.Sum(*terms)


class Mul(MathAssExpr):
    def __neg__(self) -> Expr:
        return Mul(*list(self.terms) + [Number(-1)]).simplified()

    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    def operation(self, left: int, right: int) -> int:
        return left * right

    def neutral_element(self) -> int:
        return 1

    @property
    def symbol(self) -> str:
        return " * "

    def ada_expr(self) -> ada.Expr:
        return ada.Mul(*[t.ada_expr() for t in self.terms])

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        terms = [t for t in map(lambda e: e.z3expr(), self.terms) if isinstance(t, z3.ArithRef)]
        if len(terms) != len(self.terms):
            raise Z3TypeError("multiplying non-arithmetic terms")
        return z3.Product(*terms)


class MathBinExpr(BinExpr):
    def __init__(self, left: Expr, right: Expr, location: Location = None) -> None:
        super().__init__(left, right, rty.common_type([left.type_, right.type_]), location)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error += e.check_type_instance(rty.AnyInteger)

        self.type_ = rty.common_type([self.left.type_, self.right.type_])

        return error


class Sub(MathBinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.BINARY_ADDING_OPERATOR

    def simplified(self) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if isinstance(left, Number) and isinstance(right, Number):
            return left - right
        return Add(left, -right)

    @property
    def symbol(self) -> str:
        return " - "

    def ada_expr(self) -> ada.Expr:
        return ada.Sub(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("subtracting non-arithmetic terms")
        return left - right


class Div(MathBinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    def simplified(self) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if isinstance(left, Number) and isinstance(right, Number):
            return left // right
        return Div(left, right)

    @property
    def symbol(self) -> str:
        return " / "

    def ada_expr(self) -> ada.Expr:
        return ada.Div(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("dividing non-arithmetic terms")
        return left / right


class Pow(MathBinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR

    def simplified(self) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if isinstance(left, Number) and isinstance(right, Number):
            return left ** right
        return Pow(left, right)

    @property
    def symbol(self) -> str:
        return "**"

    def ada_expr(self) -> ada.Expr:
        return ada.Pow(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("exponentiating non-arithmetic terms")
        return left ** right


class Mod(MathBinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    def simplified(self) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if isinstance(left, Number) and isinstance(right, Number):
            return left % right
        return Mod(left, right)

    @property
    def symbol(self) -> str:
        return " mod "

    def ada_expr(self) -> ada.Expr:
        return ada.Mod(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        left = self.left.simplified().z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("modulo operation on non-arithmetic terms")
        if not left.is_int():
            raise Z3TypeError(f'modulo operation on non-integer term "{left}"')
        return left % right


class Name(Expr):
    def __init__(
        self,
        negative: bool = False,
        immutable: bool = False,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(type_, location)
        self.negative = negative
        self.immutable = immutable
        self._update_str()

    def _update_str(self) -> None:
        self._str = intern(f"(-{self.representation})" if self.negative else self.representation)

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    @property
    @abstractmethod
    def representation(self) -> str:
        raise NotImplementedError

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping["Name", Expr] = None
    ) -> Expr:
        if self.immutable:
            return self
        func = substitution(mapping or {}, func)
        return -func(-self) if self.negative else func(self)

    def simplified(self) -> Expr:
        return self

    @abstractmethod
    def ada_expr(self) -> ada.Expr:
        raise NotImplementedError

    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Variable(Name):
    def __init__(
        self,
        identifier: StrID,
        negative: bool = False,
        immutable: bool = False,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        self.identifier = ID(identifier)
        super().__init__(negative, immutable, type_, location)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.negative == other.negative and self.identifier == other.identifier
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __neg__(self) -> "Variable":
        return self.__class__(
            self.identifier, not self.negative, self.immutable, self.type_, self.location
        )

    def _check_type_subexpr(self) -> RecordFluxError:
        return RecordFluxError()

    @property
    def name(self) -> str:
        return str(self.identifier)

    @property
    def representation(self) -> str:
        return str(self.name)

    def variables(self) -> List["Variable"]:
        return [self]

    def ada_expr(self) -> ada.Expr:
        return ada.Variable(ada.ID(self.identifier), self.negative)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        if self.negative:
            return -z3.Int(self.name)
        return z3.Int(self.name)


class Attribute(Name):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        if isinstance(prefix, ID):
            prefix = Variable(prefix, location=prefix.location)
        if isinstance(prefix, str):
            prefix = Variable(prefix)

        self.prefix: Expr = prefix
        super().__init__(negative, location=prefix.location)

    @property
    def representation(self) -> str:
        return f"{self.prefix}'{self.__class__.__name__}"

    def __neg__(self) -> "Attribute":
        return self.__class__(self.prefix, not self.negative)

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [self] if match(self) else self.prefix.findall(match)

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(-self if self.negative else self)
        if isinstance(expr, Attribute):
            expr = expr.__class__(expr.prefix.substituted(func))
        return -expr if self.negative else expr

    def simplified(self) -> Expr:
        expr = self.__class__(self.prefix.simplified())
        return -expr if self.negative else expr

    def variables(self) -> List[Variable]:
        return self.prefix.variables()

    def ada_expr(self) -> ada.Expr:
        return getattr(ada, self.__class__.__name__)(self.prefix.ada_expr(), self.negative)

    def z3expr(self) -> z3.ExprRef:
        if not isinstance(self.prefix, (Variable, Selected)):
            raise Z3TypeError("illegal prefix of attribute")
        name = f"{self.prefix}'{self.__class__.__name__}"
        if self.negative:
            return -z3.Int(name)
        return z3.Int(name)


class Size(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.UniversalInteger()

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(rty.Any)


class Length(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.UniversalInteger()

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(rty.Any)


class First(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.UniversalInteger()

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(rty.Any)


class Last(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.UniversalInteger()

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(rty.Any)


class ValidChecksum(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.BOOLEAN

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(rty.Any)

    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(True)

    @property
    def representation(self) -> str:
        return f"{self.prefix}'Valid_Checksum"


class Valid(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.BOOLEAN

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance((rty.Sequence, rty.Message))


class Present(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.BOOLEAN

    def _check_type_subexpr(self) -> RecordFluxError:
        if isinstance(self.prefix, Selected):
            error = self.prefix.prefix.check_type_instance(rty.Message)
        else:
            error = RecordFluxError()
            error.append(
                "invalid prefix for attribute Present",
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        return error


class HasData(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.BOOLEAN

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type(rty.Channel(readable=True, writable=False))


class Head(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = (
            self.prefix.type_.element
            if isinstance(self.prefix.type_, (rty.Aggregate, rty.Sequence))
            else rty.Any()
        )

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.prefix.check_type_instance(rty.Composite)
        if not isinstance(self.prefix, (Variable, Selected)):
            error.append(
                "prefix of attribute Head must be a name",
                Subsystem.MODEL,
                Severity.ERROR,
                self.prefix.location,
            )
        return error


class Opaque(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.OPAQUE

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(rty.Message)


class Val(Attribute):
    """Only used by code generator and therefore provides minimum functionality"""

    def __init__(
        self, prefix: Union[StrID, Expr], expression: Expr, negative: bool = False
    ) -> None:
        self.expression = expression
        super().__init__(prefix)

    def __neg__(self) -> "Val":
        return self.__class__(self.prefix, self.expression, not self.negative)

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    def variables(self) -> List[Variable]:
        raise NotImplementedError

    def findall(self, match: Callable[[Expr], bool]) -> Sequence[Expr]:
        raise NotImplementedError

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        return self

    def simplified(self) -> Expr:
        return self

    @property
    def representation(self) -> str:
        return f"{self.prefix}'{self.__class__.__name__} ({self.expression})"

    def ada_expr(self) -> ada.Expr:
        return getattr(ada, self.__class__.__name__)(
            self.prefix.ada_expr(), self.expression.ada_expr(), self.negative
        )

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


@invariant(lambda self: len(self.elements) > 0)
class Indexed(Name):
    """Only used by code generator and therefore provides minimum functionality"""

    def __init__(self, prefix: Expr, *elements: Expr, negative: bool = False) -> None:
        self.prefix = prefix
        self.elements = list(elements)
        super().__init__(negative)

    def __neg__(self) -> "Indexed":
        return self.__class__(self.prefix, *self.elements, negative=not self.negative)

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    @property
    def representation(self) -> str:
        return f"{self.prefix} (" + ", ".join(map(str, self.elements)) + ")"

    def ada_expr(self) -> ada.Expr:
        return ada.Indexed(
            self.prefix.ada_expr(), *[e.ada_expr() for e in self.elements], negative=self.negative
        )

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Selected(Name):
    def __init__(
        self,
        prefix: Expr,
        selector: StrID,
        negative: bool = False,
        immutable: bool = False,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        self.prefix = prefix
        self.selector = ID(selector)
        super().__init__(negative, immutable, type_, location)

    def __neg__(self) -> "Selected":
        return self.__class__(self.prefix, self.selector, not self.negative)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        if isinstance(self.prefix.type_, rty.Message):
            if self.selector in self.prefix.type_.fields:
                self.type_ = self.prefix.type_.field_types[self.selector]
            else:
                error.append(
                    f'invalid field "{self.selector}" for {self.prefix.type_}',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
                error.extend(
                    _similar_field_names(self.selector, self.prefix.type_.fields, self.location)
                )
                self.type_ = rty.Any()
        else:
            self.type_ = rty.Any()
        return error + self.prefix.check_type_instance(rty.Message)

    @property
    def representation(self) -> str:
        return f"{self.prefix}.{self.selector}"

    def variables(self) -> List["Variable"]:
        return self.prefix.variables()

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, self.__class__):
            return expr.__class__(
                expr.prefix.substituted(func),
                expr.selector,
                type_=expr.type_,
                location=expr.location,
            )
        return expr

    def ada_expr(self) -> ada.Expr:
        return ada.Selected(self.prefix.ada_expr(), ada.ID(self.selector), self.negative)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        if self.negative:
            return -z3.Int(self.representation)
        return z3.Int(self.representation)


class Call(Name):
    def __init__(
        self,
        identifier: StrID,
        args: Sequence[Expr] = None,
        negative: bool = False,
        immutable: bool = False,
        type_: rty.Type = rty.Undefined(),
        argument_types: Sequence[rty.Type] = None,
        location: Location = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.args = args or []
        self.argument_types = argument_types or []
        super().__init__(negative, immutable, type_, location)

    def __neg__(self) -> "Call":
        return self.__class__(self.identifier, self.args, not self.negative)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()

        for a, t in itertools.zip_longest(
            self.args, self.argument_types[: len(self.args)], fillvalue=rty.Any()
        ):
            error += a.check_type(t)

        if self.type_ != rty.Undefined():
            if len(self.args) < len(self.argument_types):
                error.append(
                    "missing function arguments",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )

            if len(self.args) > len(self.argument_types):
                error.append(
                    "too many function arguments",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )

        return error

    @property
    def representation(self) -> str:
        args = ", ".join(map(str, self.args))
        if args:
            args = f" ({args})"
        call = f"{self.identifier}{args}"
        return call

    def ada_expr(self) -> ada.Expr:
        return ada.Call(ada.ID(self.identifier), [a.ada_expr() for a in self.args], self.negative)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        result = [Variable(self.identifier, location=self.location)]
        for t in self.args:
            result.extend(t.variables())
        return result

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *[e for a in self.args for e in a.findall(match)],
        ]

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        assert isinstance(expr, Call)
        return expr.__class__(
            expr.identifier,
            [a.substituted(func) for a in expr.args],
            expr.negative,
            expr.immutable,
            expr.type_,
            expr.argument_types,
            expr.location,
        )


class UndefinedExpr(Name):
    @property
    def representation(self) -> str:
        return "__UNDEFINED__"

    def __neg__(self) -> "UndefinedExpr":
        raise NotImplementedError

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    def ada_expr(self) -> ada.Expr:
        raise NotImplementedError

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


UNDEFINED = UndefinedExpr()


class Aggregate(Expr):
    def __init__(self, *elements: Expr, location: Location = None) -> None:
        super().__init__(rty.Aggregate(rty.common_type([e.type_ for e in elements])), location)
        self.elements = list(elements)

    def _update_str(self) -> None:
        self._str = intern("[" + ", ".join(map(str, self.elements)) + "]")

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in self.elements:
            error += e.check_type_instance(rty.Any)
        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, self.__class__):
            return expr.__class__(
                *[e.substituted(func) for e in expr.elements], location=expr.location
            )
        return expr

    def simplified(self) -> Expr:
        return self.__class__(*[e.simplified() for e in self.elements])

    @property
    def length(self) -> Expr:
        return Number(len(self.elements))

    def ada_expr(self) -> ada.Expr:
        return ada.Aggregate(*[e.ada_expr() for e in self.elements])

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        return z3.BoolVal(False)


class String(Aggregate):
    def __init__(self, data: str, location: Location = None) -> None:
        super().__init__(*[Number(ord(d)) for d in data], location=location)
        self.data = data

    def _update_str(self) -> None:
        self._str = intern(f'"{self.data}"')

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        return func(self)

    def simplified(self) -> Expr:
        return self

    def ada_expr(self) -> ada.Expr:
        return ada.String(self.data)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        return z3.BoolVal(False)


class NamedAggregate(Expr):
    """Only used by code generator and therefore provides minimum functionality"""

    def __init__(self, *elements: Tuple[Union[StrID, "ValueRange"], Expr]) -> None:
        super().__init__()
        self.elements = [(ID(n) if isinstance(n, str) else n, e) for n, e in elements]

    def _update_str(self) -> None:
        assert len(self.elements) > 0
        self._str = intern(
            "(" + ", ".join(f"{name} => {element}" for name, element in self.elements) + ")"
        )

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def simplified(self) -> Expr:
        raise NotImplementedError

    def ada_expr(self) -> ada.Expr:
        elements: List[Tuple[Union[ada.StrID, ada.ValueRange], ada.Expr]] = [
            (
                ada.ID(n)
                if isinstance(n, ID)
                else ada.ValueRange(n.lower.ada_expr(), n.upper.ada_expr()),
                e.ada_expr(),
            )
            for n, e in self.elements
        ]
        return ada.NamedAggregate(*elements)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Relation(BinExpr):
    def __init__(self, left: Expr, right: Expr, location: Location = None) -> None:
        super().__init__(left, right, rty.BOOLEAN, location)

    @abstractmethod
    def __neg__(self) -> Expr:
        raise NotImplementedError

    def _simplified(self, relation_operator: Callable[[Number, Number], bool]) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if relation_operator in [operator.eq, operator.le, operator.ge] and left == right:
            return TRUE
        if isinstance(left, Number) and isinstance(right, Number):
            return TRUE if relation_operator(left, right) else FALSE
        return self.__class__(left, right)

    @property
    def precedence(self) -> Precedence:
        return Precedence.RELATIONAL_OPERATOR


class Less(Relation):
    def __neg__(self) -> Expr:
        return GreaterEqual(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error += e.check_type_instance(rty.AnyInteger)
        return error

    @property
    def symbol(self) -> str:
        return " < "

    def simplified(self) -> Expr:
        return self._simplified(operator.lt)

    def ada_expr(self) -> ada.Expr:
        return ada.Less(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("less relation between non-arithmetic terms")
        return left < right


class LessEqual(Relation):
    def __neg__(self) -> Expr:
        return Greater(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error += e.check_type_instance(rty.AnyInteger)
        return error

    @property
    def symbol(self) -> str:
        return " <= "

    def simplified(self) -> Expr:
        return self._simplified(operator.le)

    def ada_expr(self) -> ada.Expr:
        return ada.LessEqual(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("less-equal relation between non-arithmetic terms")
        return left <= right


class Equal(Relation):
    def __neg__(self) -> Expr:
        return NotEqual(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.left.check_type_instance(rty.Any) + self.right.check_type(self.left.type_)

    @property
    def symbol(self) -> str:
        return " = "

    def simplified(self) -> Expr:
        return self._simplified(operator.eq)

    def ada_expr(self) -> ada.Expr:
        return ada.Equal(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        result = left == right
        assert isinstance(result, z3.BoolRef)
        return result


class GreaterEqual(Relation):
    def __neg__(self) -> Expr:
        return Less(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error += e.check_type_instance(rty.AnyInteger)
        return error

    @property
    def symbol(self) -> str:
        return " >= "

    def simplified(self) -> Expr:
        return self._simplified(operator.ge)

    def ada_expr(self) -> ada.Expr:
        return ada.GreaterEqual(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("greater-equal relation between non-arithmetic terms")
        return left >= right


class Greater(Relation):
    def __neg__(self) -> Expr:
        return LessEqual(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error += e.check_type_instance(rty.AnyInteger)
        return error

    @property
    def symbol(self) -> str:
        return " > "

    def simplified(self) -> Expr:
        return self._simplified(operator.gt)

    def ada_expr(self) -> ada.Expr:
        return ada.Greater(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("greater relation between non-arithmetic terms")
        return left > right


class NotEqual(Relation):
    def __neg__(self) -> Expr:
        return Equal(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.left.check_type_instance(rty.Any) + self.right.check_type(self.left.type_)

    @property
    def symbol(self) -> str:
        return " /= "

    def simplified(self) -> Expr:
        return self._simplified(operator.ne)

    def ada_expr(self) -> ada.Expr:
        return ada.NotEqual(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        result = left != right
        assert isinstance(result, z3.BoolRef)
        return result


class In(Relation):
    def __neg__(self) -> Expr:
        return NotIn(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.left.check_type_instance(rty.Any) + self.right.check_type(
            rty.Aggregate(self.left.type_)
        )

    @property
    def symbol(self) -> str:
        return " in "

    def ada_expr(self) -> ada.Expr:
        return ada.In(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError


class NotIn(Relation):
    def __neg__(self) -> Expr:
        return In(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.left.check_type_instance(rty.Any) + self.right.check_type(
            rty.Aggregate(self.left.type_)
        )

    @property
    def symbol(self) -> str:
        return " not in "

    def ada_expr(self) -> ada.Expr:
        return ada.NotIn(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError


class QuantifiedExpression(Expr):
    def __init__(
        self,
        parameter_identifier: StrID,
        iterable: Expr,
        predicate: Expr,
        location: Location = None,
    ) -> None:
        super().__init__(rty.BOOLEAN, location)
        self.parameter_identifier = ID(parameter_identifier)
        self.iterable = iterable
        self.predicate = predicate

    def _update_str(self) -> None:
        self._str = intern(
            f"(for {self.quantifier} {self.parameter_identifier} {self.keyword} {self.iterable}"
            f" =>\n{indent(str(self.predicate), 4)})"
        )

    def _check_type_subexpr(self) -> RecordFluxError:
        def typify_variable(expr: Expr) -> Expr:
            if isinstance(expr, Variable) and expr.identifier == self.parameter_identifier:
                if isinstance(self.iterable.type_, (rty.Aggregate, rty.Sequence)):
                    expr.type_ = self.iterable.type_.element
                else:
                    expr.type_ = rty.Any()
            return expr

        error = self.iterable.check_type_instance(rty.Composite)

        self.predicate = self.predicate.substituted(typify_variable)

        return error + self.predicate.check_type(rty.BOOLEAN)

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    @property
    @abstractmethod
    def quantifier(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def keyword(self) -> str:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return list(
            unique(
                v
                for v in self.iterable.variables() + self.predicate.variables()
                if v.identifier != self.parameter_identifier
            )
        )

    def ada_expr(self) -> ada.Expr:
        return getattr(ada, self.__class__.__name__)(
            self.parameter_identifier, self.iterable.ada_expr(), self.predicate.ada_expr()
        )

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        assert isinstance(expr, QuantifiedExpression)
        return expr.__class__(
            expr.parameter_identifier,
            expr.iterable.substituted(func),
            expr.predicate.substituted(func),
            location=expr.location,
        )

    def simplified(self) -> Expr:
        return self.__class__(
            self.parameter_identifier, self.iterable.simplified(), self.predicate.simplified()
        )


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


class ForSomeIn(QuantifiedExpression):
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def quantifier(self) -> str:
        return "some"

    @property
    def keyword(self) -> str:
        return "in"


class ValueRange(Expr):
    def __init__(self, lower: Expr, upper: Expr, location: Location = None):
        super().__init__(rty.Any(), location)
        self.lower = lower
        self.upper = upper

    def _update_str(self) -> None:
        self._str = intern(f"{self.lower} .. {self.upper}")

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.lower, self.upper]:
            error += e.check_type_instance(rty.AnyInteger)
        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def substituted(
        self, func: Callable[["Expr"], "Expr"] = None, mapping: Mapping["Name", "Expr"] = None
    ) -> "Expr":
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, self.__class__):
            return self.__class__(
                self.lower.substituted(func),
                self.upper.substituted(func),
            )
        return expr

    def simplified(self) -> Expr:
        return self.__class__(self.lower.simplified(), self.upper.simplified())

    def ada_expr(self) -> ada.Expr:
        return ada.ValueRange(self.lower.ada_expr(), self.upper.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Conversion(Expr):
    def __init__(
        self,
        identifier: StrID,
        argument: Expr,
        type_: rty.Type = rty.Undefined(),
        argument_types: Sequence[rty.Type] = None,
        location: Location = None,
    ) -> None:
        super().__init__(type_, location)
        self.identifier = ID(identifier)
        self.argument = argument
        self.argument_types = argument_types or []

    def _update_str(self) -> None:
        self._str = intern(f"{self.identifier} ({self.argument})")

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.argument.check_type(rty.OPAQUE)

        if isinstance(self.argument, Selected):
            if self.argument_types:
                error += self.argument.prefix.check_type(tuple(self.argument_types))
            else:
                error.append(
                    f'invalid conversion to "{self.identifier}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
                if isinstance(self.argument.prefix.type_, rty.Message):
                    error.append(
                        f'refinement for message "{self.argument.prefix.type_.identifier}"'
                        " would make operation legal",
                        Subsystem.MODEL,
                        Severity.INFO,
                        self.location,
                    )
        else:
            error.append(
                "invalid argument for conversion, expected message field",
                Subsystem.MODEL,
                Severity.ERROR,
                self.argument.location,
            )

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Conversion):
            return expr.__class__(
                expr.identifier,
                expr.argument.substituted(func),
                expr.type_,
                expr.argument_types,
                expr.location,
            )
        return expr

    def simplified(self) -> Expr:
        return Conversion(
            self.identifier,
            self.argument.simplified(),
            self.type_,
            self.argument_types,
            self.location,
        )

    def ada_expr(self) -> ada.Expr:
        return ada.Conversion(ada.ID(self.identifier), self.argument.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return self.argument.variables()


class Comprehension(Expr):
    def __init__(
        self,
        iterator: StrID,
        sequence: Expr,
        selector: Expr,
        condition: Expr,
        location: Location = None,
    ) -> None:
        super().__init__(rty.Aggregate(selector.type_), location)
        self.iterator = ID(iterator)
        self.sequence = sequence
        self.selector = selector
        self.condition = condition

    def _update_str(self) -> None:
        self._str = intern(
            f"[for {self.iterator} in {self.sequence} => {self.selector} when {self.condition}]"
        )

    def _check_type_subexpr(self) -> RecordFluxError:
        def typify_variable(expr: Expr) -> Expr:
            if isinstance(expr, Variable) and expr.identifier == self.iterator:
                if isinstance(self.sequence.type_, (rty.Aggregate, rty.Sequence)):
                    expr.type_ = self.sequence.type_.element
                else:
                    expr.type_ = rty.Any()
            return expr

        error = self.sequence.check_type_instance(rty.Composite)

        self.selector = self.selector.substituted(typify_variable)
        self.condition = self.condition.substituted(typify_variable)

        error += self.selector.check_type_instance(rty.Any)
        error += self.condition.check_type(rty.BOOLEAN)

        self.type_ = rty.Aggregate(self.selector.type_)

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        return Comprehension(
            self.iterator,
            self.sequence.simplified(),
            self.selector.simplified(),
            self.condition.simplified(),
            self.location,
        )

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Comprehension):
            return expr.__class__(
                expr.iterator,
                expr.sequence.substituted(func),
                expr.selector.substituted(func),
                expr.condition.substituted(func),
                expr.location,
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def ada_expr(self) -> ada.Expr:
        raise NotImplementedError

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return [
            v
            for v in self.sequence.variables()
            + self.selector.variables()
            + self.condition.variables()
            if v.identifier != self.iterator
        ]


class MessageAggregate(Expr):
    def __init__(
        self,
        identifier: StrID,
        field_values: Mapping[StrID, Expr],
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(type_, location)
        self.identifier = ID(identifier)
        self.field_values = {ID(k): v for k, v in field_values.items()}

    def _update_str(self) -> None:
        field_values = (
            ", ".join([f"{k} => {self.field_values[k]}" for k in self.field_values])
            if self.field_values
            else "null message"
        )
        self._str = intern(f"{self.identifier}'({field_values})")

    def _check_type_subexpr(self) -> RecordFluxError:
        if not isinstance(self.type_, rty.Message):
            error = RecordFluxError()

            for d in self.field_values.values():
                error += d.check_type_instance(rty.Any)

            return error

        error = RecordFluxError()
        field_combinations = set(self.type_.field_combinations)

        for i, (field, expr) in enumerate(self.field_values.items()):
            if field not in self.type_.fields:
                error.append(
                    f'invalid field "{field}" for {self.type_}',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    field.location,
                )
                error.extend(_similar_field_names(field, self.type_.fields, field.location))
                continue

            field_type = self.type_.field_types[field]

            if field_type == rty.OPAQUE:
                if not any(
                    r.field == field and expr.type_.is_compatible(r.sdu)
                    for r in self.type_.refinements
                ):
                    error += expr.check_type(field_type)
            else:
                error += expr.check_type(field_type)

            field_combinations = {
                c for c in field_combinations if len(c) > i and c[i] == str(field)
            }

            if not field_combinations:
                error.append(
                    f'invalid position for field "{field}" of {self.type_}',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    field.location,
                )
                break

        if field_combinations and all(len(c) > len(self.field_values) for c in field_combinations):
            error.append(
                f"missing fields for {self.type_}",
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            error.append(
                "possible next fields: "
                + ", ".join(unique(c[len(self.field_values)] for c in sorted(field_combinations))),
                Subsystem.MODEL,
                Severity.INFO,
                self.location,
            )

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def simplified(self) -> Expr:
        return MessageAggregate(
            self.identifier,
            {k: self.field_values[k].simplified() for k in self.field_values},
            self.type_,
            self.location,
        )

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, MessageAggregate):
            return expr.__class__(
                expr.identifier,
                {k: expr.field_values[k].substituted(func) for k in expr.field_values},
                type_=expr.type_,
                location=expr.location,
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def ada_expr(self) -> ada.Expr:
        raise NotImplementedError

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        result = []
        for v in self.field_values.values():
            result.extend(v.variables())
        return result


class Binding(Expr):
    def __init__(self, expr: Expr, data: Mapping[StrID, Expr], location: Location = None) -> None:
        super().__init__(expr.type_, location)
        self.expr = expr
        self.data = {ID(k): v for k, v in data.items()}

    def _update_str(self) -> None:
        data = ",\n".join(["{k} = {v}".format(k=k, v=self.data[k]) for k in self.data])
        self._str = intern(f"{self.expr}\n   where {indent_next(data, 9)}")

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for d in self.data.values():
            error += d.check_type_instance(rty.Any)

        def typify_variable(expr: Expr) -> Expr:
            if isinstance(expr, Variable) and expr.identifier in self.data:
                if isinstance(self.data[expr.identifier].type_, rty.Undefined):
                    expr.type_ = rty.Any()
                else:
                    expr.type_ = self.data[expr.identifier].type_
            return expr

        self.expr = self.expr.substituted(typify_variable)

        error += self.expr.check_type_instance(rty.Any)

        self.type_ = self.expr.type_

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *self.expr.findall(match),
            *[e for v in self.data.values() for e in v.findall(match)],
        ]

    def simplified(self) -> Expr:
        facts: Mapping[Name, Expr] = {Variable(k): self.data[k].simplified() for k in self.data}
        return self.expr.substituted(mapping=facts).simplified()

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Binding):
            return expr.__class__(
                expr.expr.substituted(func),
                {k: v.substituted(func) for k, v in expr.data.items()},
                location=expr.location,
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def ada_expr(self) -> ada.Expr:
        raise NotImplementedError

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return self.simplified().variables()


def substitution(
    mapping: Mapping[Name, Expr], func: Callable[["Expr"], "Expr"] = None
) -> Callable[[Expr], Expr]:
    if func:
        return func
    return lambda expression: (
        mapping[expression]
        if isinstance(expression, Name) and expression in mapping
        else expression
    )


def max_value(target: Expr, facts: Sequence[Expr]) -> Number:
    opt = z3.Optimize()
    opt.add(*[e.z3expr() for e in facts])
    value = opt.maximize(target.z3expr())
    result = opt.check()
    assert result == z3.sat
    upper = value.upper()
    assert isinstance(upper, z3.IntNumRef)
    return Number(upper.as_long())


def _entity_name(expr: Expr) -> str:
    expr_type = (
        "variable"
        if isinstance(expr, Variable)
        else "function"
        if isinstance(expr, Call)
        else "type"
        if isinstance(expr, Conversion)
        else "message"
        if isinstance(expr, MessageAggregate)
        else "expression"
    )
    expr_name = (
        str(expr.identifier)
        if isinstance(expr, (Variable, Call, Conversion, MessageAggregate))
        else str(expr)
    )
    return f'{expr_type} "{expr_name}"'


def _similar_field_names(
    field: ID, fields: Iterable[ID], location: Optional[Location]
) -> RecordFluxError:
    field_similarity = sorted(
        ((f, difflib.SequenceMatcher(None, str(f), str(field)).ratio()) for f in sorted(fields)),
        key=lambda x: x[1],
        reverse=True,
    )
    similar_fields = [f for f, s in field_similarity if s >= 0.5]

    error = RecordFluxError()
    if similar_fields:
        error.append(
            "similar field names: " + ", ".join(str(f) for f in similar_fields),
            Subsystem.MODEL,
            Severity.INFO,
            location,
        )
    return error
