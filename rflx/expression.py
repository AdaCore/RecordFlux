# pylint: disable=too-many-lines,too-many-ancestors,too-many-arguments,method-cache-max-size-none

from __future__ import annotations

import difflib
import itertools
import operator
from abc import abstractmethod
from concurrent.futures import ProcessPoolExecutor
from copy import copy
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from itertools import groupby
from operator import itemgetter
from sys import intern
from typing import Callable, Iterable, List, Mapping, Optional, Sequence, Tuple, Type, Union

import z3

from rflx import ada, typing_ as rty
from rflx.common import Base, indent, indent_next, unique
from rflx.contract import DBC, invariant, require
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID

MAX_LINE_LENGTH = 100


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
        self._expr = expr
        self._facts = facts or []
        self._result = ProofResult.UNSAT
        self._logic = logic
        self._unknown_reason: Optional[str] = None

        solver = z3.SolverFor(self._logic)
        solver.add(self._expr.z3expr())
        for f in self._facts:
            solver.add(f.z3expr())

        self._result = ProofResult(solver.check())
        if self._result == ProofResult.UNKNOWN:
            self._unknown_reason = solver.reason_unknown()

    @property
    def result(self) -> ProofResult:
        return self._result

    @property
    def error(self) -> List[Tuple[str, Optional[Location]]]:
        assert self._result != ProofResult.SAT
        if self._result == ProofResult.UNKNOWN:
            assert self._unknown_reason is not None
            return [(self._unknown_reason, None)]
        solver = z3.SolverFor(self._logic)
        solver.set(unsat_core=True)
        facts = {f"H{index}": fact for index, fact in enumerate(self._facts)}
        for name, fact in facts.items():
            solver.assert_and_track(fact.z3expr(), name)

        solver.assert_and_track(self._expr.z3expr(), "goal")
        facts["goal"] = self._expr
        result = solver.check()
        assert result == z3.unsat, f"result should be unsat (is {result})"
        return [
            (" ".join(str(facts[str(fact)]).replace("\n", " ").split()), facts[fact].location)
            for fact in sorted([str(h) for h in solver.unsat_core()])
        ]


@dataclass
class ProofJob:
    goal: "Expr"
    facts: List["Expr"]
    expected: ProofResult
    error: RecordFluxError
    negate: bool
    add_unsat: bool


class ParallelProofs:
    def __init__(self, workers: int) -> None:
        self._proofs: List[List[ProofJob]] = []
        self._current: List[ProofJob] = []
        self._workers = workers

    def add(
        self,
        goal: "Expr",
        facts: List["Expr"],
        expected: ProofResult,
        error: RecordFluxError,
        negate: bool = False,
        add_unsat: bool = False,
    ) -> None:
        # pylint: disable=too-many-arguments
        self._current.append(ProofJob(goal, facts, expected, error, negate, add_unsat))

    def push(self) -> None:
        if self._current:
            self._proofs.append(copy(self._current))
            self._current.clear()

    @staticmethod
    def check_proof(jobs: List[ProofJob]) -> RecordFluxError:
        result = RecordFluxError()
        for job in jobs:
            proof = job.goal.check(job.facts)
            if job.negate == (proof.result != job.expected):
                continue
            result.extend(job.error)
            if job.add_unsat:
                result.extend(
                    [
                        (f'unsatisfied "{m}"', Subsystem.MODEL, Severity.INFO, locn)
                        for m, locn in proof.error
                    ]
                )
        return result

    def check(self, error: RecordFluxError) -> None:
        self.push()
        with ProcessPoolExecutor(max_workers=self._workers) as executor:
            for e in executor.map(ParallelProofs.check_proof, self._proofs):
                error.extend(e)
        error.propagate()


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

    def __lt__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return str(self) < str(other)
        return NotImplemented

    def __le__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return str(self) <= str(other)
        return NotImplemented

    def __gt__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return str(self) > str(other)
        return NotImplemented

    def __ge__(self, other: object) -> bool:
        if isinstance(other, Expr):
            return str(self) >= str(other)
        return NotImplemented

    def __str__(self) -> str:
        try:
            return self._str
        except AttributeError:
            self._update_str()
            return self._str

    def __hash__(self) -> int:
        return hash(self.__class__.__name__)

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


class Not(Expr):
    def __init__(self, expr: Expr, location: Location = None) -> None:
        super().__init__(rty.BOOLEAN, location)
        self.expr = expr

    def _update_str(self) -> None:
        self._str = intern(f"not {self.parenthesized(self.expr)}")

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.expr.check_type(rty.BOOLEAN)

    def __neg__(self) -> Expr:
        return self.expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR

    def variables(self) -> List["Variable"]:
        return self.expr.variables()

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *self.expr.findall(match),
        ]

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping["Name", Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, Not):
            return expr.__class__(
                expr.expr.substituted(func),
                location=expr.location,
            )
        return expr

    def simplified(self) -> Expr:
        if self.expr == TRUE:
            return FALSE
        if self.expr == FALSE:
            return TRUE
        # De Morgan's law
        if isinstance(self.expr, And):
            return Or(
                *(Not(term.simplified()).simplified() for term in self.expr.terms)
            ).simplified()
        if isinstance(self.expr, Or):
            return And(
                *(Not(term.simplified()).simplified() for term in self.expr.terms)
            ).simplified()
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
        return self.__class__(self.expr.simplified(), self.location)

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
        return self.__class__(-self.left, self.right, location=self.location)

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
        return self.__class__(
            self.left.simplified(), self.right.simplified(), location=self.location
        )

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
            elif t == TRUE:
                total = self.operation(total, 1)
            elif t == FALSE:
                total = self.operation(total, 0)
            elif isinstance(t, type(self)):
                all_terms += t.terms
            else:
                terms.append(t)

        if isinstance(self, (And, Or)):
            terms = self._simplified_boolean_expressions(terms, total)
        else:
            if not terms:
                return Number(total)
            if total != self.neutral_element():
                terms.append(Number(total))

        terms = self._simplified_if_expressions(terms)

        if len(terms) == 1:
            return terms[0]

        return self.__class__(*terms, location=self.location)

    def _simplified_boolean_expressions(self, terms: Sequence[Expr], total: int) -> list[Expr]:
        if not terms:
            return [TRUE if total else FALSE]

        terms = list(unique(terms))

        for term in terms:
            for relation, inverse_relation in [
                (Less, GreaterEqual),
                (LessEqual, Greater),
                (Equal, NotEqual),
                (GreaterEqual, Less),
                (Greater, LessEqual),
                (NotEqual, Equal),
            ]:
                if isinstance(term, relation):
                    if inverse_relation(term.left, term.right) in terms:
                        return [FALSE if isinstance(self, And) else TRUE]

        if total != self.neutral_element():
            terms.append(TRUE if total else FALSE)

        return terms

    def _simplified_if_expressions(self, terms: list[Expr]) -> list[Expr]:
        """Merge if expressions which differ only in the condition."""

        if not terms:
            return []

        t = terms[0]

        if isinstance(t, IfExpr):
            for i, u in enumerate(terms[1:]):
                if (
                    isinstance(u, IfExpr)
                    and len(u.condition_expressions) == 1
                    and u.condition_expressions[0][1] == t.condition_expressions[0][1]
                    and u.else_expression == t.else_expression
                ):
                    return [
                        IfExpr(
                            [
                                (
                                    Or(
                                        t.condition_expressions[0][0],
                                        u.condition_expressions[0][0],
                                    ).simplified(),
                                    t.condition_expressions[0][1],
                                )
                            ],
                            t.else_expression,
                        ),
                        *self._simplified_if_expressions(terms[1 : i + 1] + terms[i + 2 :]),
                    ]
            return [t, *self._simplified_if_expressions(terms[1:])]

        return terms

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
            self._str = str(self.simplified())
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
            self._str = f"{value}"
        elif self.base == 2:
            self._str = f"2#{value:b}#"
        elif self.base == 8:
            self._str = f"8#{value:o}#"
        elif self.base == 10:
            self._str = f"10#{value}#"
        elif self.base == 16:
            self._str = f"16#{value:X}#"
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
            return Number(self.value**other.value)
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
            complement = None
            for other in terms:
                if other == -term:
                    complement = other
                    break
            if complement is not None:
                terms.remove(complement)
            else:
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
            return left**right
        return Pow(left, right)

    @property
    def symbol(self) -> str:
        return " ** "

    def ada_expr(self) -> ada.Expr:
        return ada.Pow(self.left.ada_expr(), self.right.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ArithRef:
        left = self.left.z3expr()
        right = self.right.z3expr()
        if not isinstance(left, z3.ArithRef) or not isinstance(right, z3.ArithRef):
            raise Z3TypeError("exponentiating non-arithmetic terms")
        return left**right


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


class Rem(MathBinExpr):
    """Only used by code generator and therefore provides minimum functionality."""

    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    @property
    def symbol(self) -> str:
        return " rem "

    def ada_expr(self) -> ada.Expr:
        return ada.Rem(self.left.ada_expr(), self.right.ada_expr())

    def z3expr(self) -> z3.ArithRef:
        raise NotImplementedError


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


class Literal(Name):
    def __init__(
        self,
        identifier: StrID,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        self.identifier = ID(identifier)
        super().__init__(negative=False, immutable=False, type_=type_, location=location)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.identifier == other.identifier
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __neg__(self) -> Literal:
        raise NotImplementedError

    def _check_type_subexpr(self) -> RecordFluxError:
        return RecordFluxError()

    @property
    def name(self) -> str:
        return str(self.identifier)

    @property
    def representation(self) -> str:
        return str(self.name)

    def variables(self) -> List[Variable]:
        return []

    def ada_expr(self) -> ada.Expr:
        return ada.Literal(self.identifier)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        if self.identifier == ID("True"):
            return z3.BoolVal(True)
        if self.identifier == ID("False"):
            return z3.BoolVal(False)
        return z3.Int(self.name)

    def copy(
        self,
        identifier: StrID = None,
        type_: rty.Type = None,
        location: Location = None,
    ) -> Literal:
        return self.__class__(
            ID(identifier) if identifier is not None else self.identifier,
            type_ if type_ is not None else self.type_,
            location if location is not None else self.location,
        )


class Variable(Name):
    def __init__(
        self,
        identifier: StrID,
        negative: bool = False,
        immutable: bool = False,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        assert (
            not isinstance(type_, rty.Enumeration) if negative else True
        ), "enumeration variable must not be negative"
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
        return ada.Variable(self.identifier, self.negative)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        if self.negative:
            return -z3.Int(self.name)
        return z3.Int(self.name)

    def copy(
        self,
        identifier: StrID = None,
        negative: bool = None,
        immutable: bool = None,
        type_: rty.Type = None,
        location: Location = None,
    ) -> Variable:
        return self.__class__(
            ID(identifier) if identifier is not None else self.identifier,
            negative if negative is not None else self.negative,
            immutable if immutable is not None else self.immutable,
            type_ if type_ is not None else self.type_,
            location if location is not None else self.location,
        )


TRUE = Literal("True", type_=rty.BOOLEAN)
FALSE = Literal("False", type_=rty.BOOLEAN)


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
        return f"{self.prefix}'{self.symbol}"

    @property
    def symbol(self) -> str:
        return self.__class__.__name__

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
            prefix = expr.prefix.substituted(func)
            if not isinstance(prefix, Attribute):
                expr = expr.__class__(prefix)
        return -expr if self.negative else expr

    def simplified(self) -> Expr:
        expr = self.__class__(self.prefix.simplified())
        return -expr if self.negative else expr

    def variables(self) -> List[Variable]:
        return self.prefix.variables()

    def ada_expr(self) -> ada.Expr:
        result = getattr(ada, self.__class__.__name__)(self.prefix.ada_expr(), self.negative)
        assert isinstance(result, ada.Expr)
        return result

    def z3expr(self) -> z3.ExprRef:
        if not isinstance(self.prefix, (Variable, Literal, Selected)):
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
            error.extend(
                [
                    (
                        "invalid prefix for attribute Present",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    )
                ]
            )
        return error


class HasData(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.BOOLEAN

    @property
    def symbol(self) -> str:
        return "Has_Data"

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(rty.Message)


class Head(Attribute):
    def __init__(
        self, prefix: Union[StrID, Expr], negative: bool = False, type_: rty.Type = rty.Undefined()
    ):
        super().__init__(prefix, negative)
        self.type_ = type_

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.prefix.check_type_instance(rty.Composite)
        self.type_ = (
            self.prefix.type_.element
            if isinstance(self.prefix.type_, (rty.Aggregate, rty.Sequence))
            else rty.Any()
        )
        if not isinstance(self.prefix, (Variable, Selected, Comprehension)):
            error.extend(
                [
                    (
                        "prefix of attribute Head must be a name or comprehension",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.prefix.location,
                    )
                ],
            )
        return error


class Opaque(Attribute):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        super().__init__(prefix, negative)
        self.type_ = rty.OPAQUE

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance((rty.Sequence, rty.Message))


class Constrained(Attribute):
    """Only used by code generator and therefore provides minimum functionality."""

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError


class Val(Attribute):
    """Only used by code generator and therefore provides minimum functionality."""

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
        result = getattr(ada, self.__class__.__name__)(
            self.prefix.ada_expr(), self.expression.ada_expr(), self.negative
        )
        assert isinstance(result, ada.Expr)
        return result

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


@invariant(lambda self: len(self.elements) > 0)
class Indexed(Name):
    """Only used by code generator and therefore provides minimum functionality."""

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
        return self.__class__(
            self.prefix, self.selector, not self.negative, self.immutable, self.type_, self.location
        )

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *self.prefix.findall(match),
        ]

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        if isinstance(self.prefix.type_, rty.Message):
            if self.selector in self.prefix.type_.types:
                self.type_ = self.prefix.type_.types[self.selector]
            else:
                error.extend(
                    [
                        (
                            f'invalid field "{self.selector}" for {self.prefix.type_}',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.location,
                        ),
                        *_similar_field_names(
                            self.selector,
                            self.prefix.type_.parameters | self.prefix.type_.fields,
                            self.location,
                        ),
                    ]
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
                expr.negative,
                expr.immutable,
                expr.type_,
                expr.location,
            )
        return expr

    def ada_expr(self) -> ada.Expr:
        return ada.Selected(self.prefix.ada_expr(), ID(self.selector), self.negative)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        if self.negative:
            return -z3.Int(self.representation)
        return z3.Int(self.representation)

    def copy(
        self,
        prefix: Expr = None,
        selector: StrID = None,
        negative: bool = None,
        immutable: bool = None,
        type_: rty.Type = None,
        location: Location = None,
    ) -> Selected:
        return self.__class__(
            prefix if prefix is not None else self.prefix,
            ID(selector) if selector is not None else self.selector,
            negative if negative is not None else self.negative,
            immutable if immutable is not None else self.immutable,
            type_ if type_ is not None else self.type_,
            location if location is not None else self.location,
        )


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
        return self.__class__(
            self.identifier,
            self.args,
            not self.negative,
            self.immutable,
            self.type_,
            self.argument_types,
            self.location,
        )

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()

        for a, t in itertools.zip_longest(self.args, self.argument_types[: len(self.args)]):
            error += a.check_type(t if t is not None else rty.Any())

        if self.type_ != rty.Undefined():
            if len(self.args) < len(self.argument_types):
                error.extend(
                    [
                        (
                            "missing function arguments",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.location,
                        )
                    ],
                )

            if len(self.args) > len(self.argument_types):
                error.extend(
                    [
                        (
                            "too many function arguments",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.location,
                        )
                    ],
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
        return ada.Call(self.identifier, [a.ada_expr() for a in self.args], {}, self.negative)

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


class Slice(Name):
    """Only used by code generator and therefore provides minimum functionality."""

    def __init__(self, prefix: Expr, first: Expr, last: Expr) -> None:
        self.prefix = prefix
        self.first = first
        self.last = last
        super().__init__()

    def __neg__(self) -> Name:
        raise NotImplementedError

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    @property
    def representation(self) -> str:
        return f"{self.prefix} ({self.first} .. {self.last})"

    def ada_expr(self) -> ada.Expr:
        return ada.Slice(self.prefix.ada_expr(), self.first.ada_expr(), self.last.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


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

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Aggregate):
            return self.elements == other.elements
        return NotImplemented

    def __hash__(self) -> int:
        return hash(tuple(self.elements))

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
        return self.__class__(*[e.simplified() for e in self.elements], location=self.location)

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
    """Only used by code generator and therefore provides minimum functionality."""

    def __init__(self, *elements: Tuple[Union[StrID, Expr], Expr]) -> None:
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
        elements: List[Tuple[Union[ID, ada.Expr], ada.Expr]] = [
            (
                n if isinstance(n, ID) else n.ada_expr(),
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

    def _simplified(self, relation_operator: Callable[[Expr, Expr], bool]) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()

        if left == right:
            if relation_operator in [operator.eq, operator.le, operator.ge]:
                return TRUE
            if relation_operator in [operator.ne]:
                return FALSE

        if isinstance(left, Literal) and isinstance(right, Literal):
            return TRUE if relation_operator(left, right) else FALSE

        for t in (Aggregate, Number, Literal):
            if isinstance(left, t) and isinstance(right, t):
                return TRUE if relation_operator(left, right) else FALSE

        return self.__class__(left, right, self.location)

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


class IfExpr(Expr):
    def __init__(
        self, condition_expressions: Sequence[Tuple[Expr, Expr]], else_expression: Expr = None
    ) -> None:
        super().__init__()
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

    def __neg__(self) -> Expr:
        return Mul(-Number(1), IfExpr(self.condition_expressions, self.else_expression))

    def _update_str(self) -> None:
        condition_expressions = [(str(c), str(e)) for c, e in self.condition_expressions]
        else_expression = str(self.else_expression)

        expression = "".join(
            f"if {c} then {e}" if i == 0 else f" elsif {c} then {e}"
            for i, (c, e) in enumerate(condition_expressions)
        )
        if self.else_expression:
            expression += f" else {else_expression}"
        expression = " ".join(expression.split())

        if len(expression) > MAX_LINE_LENGTH:
            expression = ""
            expression = "".join(
                f"if\n{indent(c, 4)}\n then\n{indent(e, 4)}"
                if i == 0
                else f"\n elsif\n{indent(c, 4)}\n then\n{indent(e, 4)}"
                for i, (c, e) in enumerate(condition_expressions)
            )
            if self.else_expression:
                expression += f"\n else\n{indent(else_expression, 4)}"

        self._str = intern(f"({expression})")

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, IfExpr):
            return expr.__class__(
                [(c.substituted(func), e.substituted(func)) for c, e in self.condition_expressions],
                self.else_expression.substituted(func) if self.else_expression else None,
            )
        return expr

    def simplified(self) -> Expr:
        condition_expressions = [
            (c.simplified(), e.simplified()) for c, e in self.condition_expressions
        ]
        else_expression = self.else_expression.simplified() if self.else_expression else None

        if len(condition_expressions) == 1:
            if condition_expressions[0][0] == TRUE:
                return condition_expressions[0][1]
            if condition_expressions[0][0] == FALSE and else_expression:
                return else_expression
            if condition_expressions[0][1] == else_expression:
                return else_expression

        return self.__class__(
            condition_expressions,
            self.else_expression.simplified() if self.else_expression else None,
        )

    def ada_expr(self) -> ada.Expr:
        result = getattr(ada, self.__class__.__name__)(
            [(c.ada_expr(), e.ada_expr()) for c, e in self.condition_expressions],
            self.else_expression.ada_expr() if self.else_expression else None,
        )
        assert isinstance(result, ada.Expr)
        return result

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        if len(self.condition_expressions) != 1:
            raise Z3TypeError("more than one condition")
        if self.else_expression is None:
            raise Z3TypeError("missing else expression")

        condition = self.condition_expressions[0][0].z3expr()

        if not isinstance(condition, z3.BoolRef):
            raise Z3TypeError("non-boolean condition")

        return z3.If(
            condition,
            self.condition_expressions[0][1].z3expr(),
            self.else_expression.z3expr(),
        )


class QuantifiedExpr(Expr):
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
        # pylint: disable-next = abstract-class-instantiated
        result = getattr(ada, self.__class__.__name__)(
            self.parameter_identifier, self.iterable.ada_expr(), self.predicate.ada_expr()
        )
        assert isinstance(result, ada.Expr)
        return result

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        assert isinstance(expr, QuantifiedExpr)
        return expr.__class__(
            expr.parameter_identifier,
            expr.iterable.substituted(func),
            expr.predicate.substituted(func),
            expr.location,
        )

    def simplified(self) -> Expr:
        return self.__class__(
            self.parameter_identifier,
            self.iterable.simplified(),
            self.predicate.simplified(),
            self.location,
        )


class ForAllOf(QuantifiedExpr):
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def quantifier(self) -> str:
        return "all"

    @property
    def keyword(self) -> str:
        return "of"


class ForAllIn(QuantifiedExpr):
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def quantifier(self) -> str:
        return "all"

    @property
    def keyword(self) -> str:
        return "in"


class ForSomeIn(QuantifiedExpr):
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
                self.location,
            )
        return expr

    def simplified(self) -> Expr:
        return self.__class__(self.lower.simplified(), self.upper.simplified(), self.location)

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
                error.extend(
                    [
                        (
                            f'invalid conversion to "{self.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.location,
                        )
                    ],
                )
                if isinstance(self.argument.prefix.type_, rty.Message):
                    error.extend(
                        [
                            (
                                f'refinement for message "{self.argument.prefix.type_.identifier}"'
                                " would make operation legal",
                                Subsystem.MODEL,
                                Severity.INFO,
                                self.location,
                            )
                        ],
                    )
        else:
            error.extend(
                [
                    (
                        "invalid argument for conversion, expected message field",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.argument.location,
                    )
                ],
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
        return ada.Conversion(self.identifier, self.argument.ada_expr())

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        return self.argument.variables()


class QualifiedExpr(Expr):
    """Only used by code generator and therefore provides minimum functionality."""

    def __init__(self, type_identifier: StrID, expression: Expr) -> None:
        super().__init__()
        self.type_identifier = ID(type_identifier)
        self.expression = expression

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def _update_str(self) -> None:
        operand = (
            str(self.expression)
            if isinstance(self.expression, (Aggregate, NamedAggregate))
            else f"({self.expression})"
        )
        self._str = intern(f"{self.type_identifier}'{operand}")

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def simplified(self) -> Expr:
        return QualifiedExpr(self.type_identifier, self.expression.simplified())

    def ada_expr(self) -> ada.Expr:
        return ada.QualifiedExpr(self.type_identifier, self.expression.ada_expr())

    def z3expr(self) -> z3.ArithRef:
        raise NotImplementedError


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
            f"[for {self.iterator} in {self.sequence} if {self.condition} => {self.selector}]"
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
            if field not in self.type_.types:
                error.extend(
                    [
                        (
                            f'invalid field "{field}" for {self.type_}',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            field.location,
                        ),
                        *_similar_field_names(field, self.type_.types, field.location),
                    ]
                )
                continue

            field_type = self.type_.types[field]

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
                error.extend(
                    [
                        (
                            f'invalid position for field "{field}" of {self.type_}',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            field.location,
                        )
                    ],
                )
                break

        if field_combinations and all(len(c) > len(self.field_values) for c in field_combinations):
            error.extend(
                [
                    (
                        f"missing fields for {self.type_}",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    (
                        "possible next fields: "
                        + ", ".join(
                            unique(c[len(self.field_values)] for c in sorted(field_combinations))
                        ),
                        Subsystem.MODEL,
                        Severity.INFO,
                        self.location,
                    ),
                ],
            )

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *[e for v in self.field_values.values() for e in v.findall(match)],
        ]

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


class DeltaMessageAggregate(Expr):
    """For internal use only."""

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
        self._str = intern(f"{self.identifier} with delta {field_values}")

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()

        if not isinstance(self.type_, rty.Message):
            for d in self.field_values.values():
                error += d.check_type_instance(rty.Any)

            return error

        field_combinations = set(self.type_.field_combinations)
        fields: tuple[str, ...] = ()

        for i, (field, expr) in enumerate(self.field_values.items()):
            if field not in self.type_.fields:
                error.extend(
                    [
                        (
                            f'invalid field "{field}" for {self.type_}',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            field.location,
                        ),
                        *_similar_field_names(field, self.type_.fields, field.location),
                    ]
                )
                continue

            field_type = self.type_.types[field]

            if field_type == rty.OPAQUE:
                if not any(
                    r.field == field and expr.type_.is_compatible(r.sdu)
                    for r in self.type_.refinements
                ):
                    error += expr.check_type(field_type)
            else:
                error += expr.check_type(field_type)

            fields = (*fields, str(field))
            field_combinations = {
                c
                for c in field_combinations
                if any(fields == c[i : len(fields) + i] for i in range(len(c) - len(fields) + 1))
            }

            if not field_combinations:
                error.extend(
                    [
                        (
                            f'invalid position for field "{field}" of {self.type_}',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            field.location,
                        )
                    ],
                )
                break

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def findall(self, match: Callable[[Expr], bool]) -> Sequence[Expr]:
        return [
            *([self] if match(self) else []),
            *[e for v in self.field_values.values() for e in v.findall(match)],
        ]

    def simplified(self) -> Expr:
        return DeltaMessageAggregate(
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
        if isinstance(expr, DeltaMessageAggregate):
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

    def variables(self) -> List[Variable]:
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
        data = ",\n".join([f"{k} = {self.data[k]}" for k in self.data])
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
        if isinstance(expr, (MessageAggregate, DeltaMessageAggregate))
        else "expression"
    )
    expr_name = (
        str(expr.identifier)
        if isinstance(expr, (Variable, Call, Conversion, MessageAggregate, DeltaMessageAggregate))
        else str(expr)
    )
    return f'{expr_type} "{expr_name}"'


class CaseExpr(Expr):
    def __init__(
        self,
        expr: Expr,
        choices: List[Tuple[List[Union[ID, Number]], Expr]],
        location: Location = None,
    ) -> None:
        super().__init__(rty.Undefined(), location)
        self.expr = expr
        self.choices = choices

    def _update_str(self) -> None:
        data = ",\n".join(f"      when {' | '.join(map(str, c))} => {e}" for c, e in self.choices)
        self._str = intern(f"(case {self.expr} is\n{data})")

    def _check_enumeration(self) -> RecordFluxError:
        assert isinstance(self.expr.type_, rty.Enumeration)
        assert self.expr.type_.literals

        error = RecordFluxError()
        literals = [
            c.name for (choice, _) in self.choices for c in choice if isinstance(c, (str, ID))
        ]
        type_literals = [l.name for l in self.expr.type_.literals]
        missing = set(type_literals) - set(literals)
        if missing:
            error.extend(
                [
                    (
                        "not all enumeration literals covered by case expression",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    *[
                        (
                            f'missing literal "{l.name}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            self.expr.type_.location,
                        )
                        for l in missing
                    ],
                ]
            )

        invalid = set(literals) - set(type_literals)
        if invalid:
            error.extend(
                [
                    (
                        "invalid literals used in case expression",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    *[
                        (
                            f'literal "{l.name}" not part of "{self.expr.type_.identifier}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            self.expr.type_.location,
                        )
                        for l in invalid
                    ],
                ]
            )
        return error

    def _check_integer(self) -> RecordFluxError:
        assert isinstance(self.expr.type_, rty.Integer)
        assert self.expr.type_.bounds.lower
        assert self.expr.type_.bounds.upper

        error = RecordFluxError()
        literals = [
            c.value for (choice, _) in self.choices for c in choice if isinstance(c, Number)
        ]
        type_literals = range(self.expr.type_.bounds.lower, self.expr.type_.bounds.upper + 1)

        missing = set(type_literals) - set(literals)
        if missing:
            missing_ranges = []
            for _, g in groupby(enumerate(missing), lambda i: i[0] - i[1]):
                group = list(map(itemgetter(1), g))
                missing_ranges.append((group[0], group[-1]))

            error.extend(
                [
                    (
                        f"case expression does not cover full range of "
                        f'"{self.expr.type_.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    *[
                        (
                            f"missing range {r[0]} .. {r[1]}"
                            if r[0] != r[1]
                            else f"missing value {r[0]}",
                            Subsystem.MODEL,
                            Severity.INFO,
                            self.expr.type_.location,
                        )
                        for r in missing_ranges
                    ],
                ]
            )

        invalid = set(literals) - set(type_literals)
        if invalid:
            error.extend(
                [
                    (
                        "invalid literals used in case expression",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    *[
                        (
                            f'value {l} not part of "{self.expr.type_.identifier}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            self.expr.type_.location,
                        )
                        for l in invalid
                    ],
                ]
            )

        return error

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        result_type: rty.Type = rty.Any()
        literals = [c for (choice, _) in self.choices for c in choice]

        for _, expr in self.choices:
            error += expr.check_type_instance(rty.Any)
            result_type = result_type.common_type(expr.type_)

        for i1, (_, e1) in enumerate(self.choices):
            for i2, (_, e2) in enumerate(self.choices):
                if i1 < i2:
                    if not e1.type_.is_compatible(e2.type_):
                        error.extend(
                            [
                                (
                                    f'dependent expression "{e1}" has incompatible {e1.type_}',
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    e1.location,
                                ),
                                (
                                    f'conflicting with "{e2}" which has {e2.type_}',
                                    Subsystem.MODEL,
                                    Severity.INFO,
                                    e2.location,
                                ),
                            ]
                        )

        error += self.expr.check_type_instance(rty.Any)
        error.propagate()

        duplicates = [
            e1
            for i1, e1 in enumerate(literals)
            for i2, e2 in enumerate(literals)
            if i1 > i2 and e1 == e2
        ]
        if duplicates:
            error.extend(
                [
                    (
                        "duplicate literals used in case expression",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                    *[
                        (
                            f'duplicate literal "{l}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.location,
                        )
                        for l in duplicates
                    ],
                ]
            )

        if isinstance(self.expr.type_, rty.Enumeration):
            error += self._check_enumeration()
        elif isinstance(self.expr.type_, rty.Integer):
            error += self._check_integer()
        else:
            error.extend(
                [
                    (
                        f"invalid discrete choice with {self.expr.type_}",
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.expr.location,
                    ),
                    (
                        "expected enumeration or integer type",
                        Subsystem.MODEL,
                        Severity.INFO,
                        self.expr.location,
                    ),
                ]
            )

        self.type_ = result_type

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def findall(self, match: Callable[["Expr"], bool]) -> Sequence["Expr"]:
        return [
            *([self] if match(self) else []),
            *self.expr.findall(match),
            *[e for _, v in self.choices for e in v.findall(match)],
        ]

    def simplified(self) -> Expr:
        return self.__class__(
            self.expr.simplified(),
            [(c, e.simplified()) for c, e in self.choices],
            location=self.location,
        )

    def substituted(
        self, func: Callable[[Expr], Expr] = None, mapping: Mapping[Name, Expr] = None
    ) -> Expr:
        func = substitution(mapping or {}, func)
        expr = func(self)
        if isinstance(expr, CaseExpr):
            return expr.__class__(
                expr.expr.substituted(func),
                [(c, e.substituted(func)) for c, e in self.choices],
                location=expr.location,
            )
        return expr

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def ada_expr(self) -> ada.Expr:
        choices = [
            (
                Literal(choice).ada_expr() if isinstance(choice, (str, ID)) else choice.ada_expr(),
                expr.ada_expr(),
            )
            for choices, expr in self.choices
            for choice in choices
        ]
        return ada.CaseExpr(self.expr.ada_expr(), choices)

    @lru_cache(maxsize=None)
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def variables(self) -> List["Variable"]:
        simplified = self.simplified()
        assert isinstance(simplified, CaseExpr)
        return list(
            unique(
                [
                    *simplified.expr.variables(),
                    *[v for _, e in simplified.choices for v in e.variables()],
                ]
            )
        )


def _similar_field_names(
    field: ID, fields: Iterable[ID], location: Optional[Location]
) -> List[Tuple[str, Subsystem, Severity, Optional[Location]]]:
    field_similarity = sorted(
        ((f, difflib.SequenceMatcher(None, str(f), str(field)).ratio()) for f in sorted(fields)),
        key=lambda x: x[1],
        reverse=True,
    )
    similar_fields = [f for f, s in field_similarity if s >= 0.5]
    if similar_fields:
        return [
            (
                "similar field names: " + ", ".join(str(f) for f in similar_fields),
                Subsystem.MODEL,
                Severity.INFO,
                location,
            )
        ]
    return []
