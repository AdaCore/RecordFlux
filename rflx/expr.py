from __future__ import annotations

import difflib
import fractions
import itertools
import operator
from abc import abstractmethod
from collections.abc import Callable, Iterable, Mapping, Sequence
from itertools import groupby
from operator import itemgetter
from pathlib import Path
from sys import intern
from typing import TYPE_CHECKING, Final

from rflx import const, ty
from rflx.common import Base, indent, indent_next, unique
from rflx.identifier import ID, StrID
from rflx.rapidflux import (
    NO_LOCATION,
    Annotation,
    ErrorEntry,
    Location,
    RecordFluxError,
    Severity,
)
from rflx.rapidflux.expr import Precedence as Precedence

if TYPE_CHECKING:
    from _typeshed import SupportsAllComparisons

MAX_LINE_LENGTH: Final = 100


class Expr(Base):
    _str: str

    def __init__(
        self,
        type_: ty.Type = ty.UNDEFINED,
        location: Location | None = None,
    ):
        self.type_ = type_
        self._location = location

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

    def __contains__(self, item: Expr) -> bool:
        return item == self

    @abstractmethod
    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def location(self) -> Location:
        return self._location or NO_LOCATION

    @abstractmethod
    def _update_str(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def _check_type_subexpr(self) -> RecordFluxError:
        """Initialize and check the types of sub-expressions."""
        raise NotImplementedError

    def check_type(self, expected: ty.Type | tuple[ty.Type, ...]) -> RecordFluxError:
        """Initialize and check the types of the expression and all sub-expressions."""
        error = self._check_type_subexpr()
        error.extend(
            ty.check_type(
                self.type_,
                expected,
                self.location,
                _entity_name(self),
            ).entries,
        )
        return error

    def check_type_instance(
        self,
        expected: type[ty.Type] | tuple[type[ty.Type], ...],
    ) -> RecordFluxError:
        """Initialize and check the types of the expression and all sub-expressions."""
        error = self._check_type_subexpr()
        error.extend(
            ty.check_type_instance(
                self.type_,
                expected,
                self.location,
                _entity_name(self),
            ).entries,
        )
        return error

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def variables(self) -> list[Variable]:
        return []

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [self] if match(self) else []

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        return func(self)

    @abstractmethod
    def simplified(self) -> Expr:
        raise NotImplementedError

    def parenthesized(self, expr: Expr) -> str:
        if expr.precedence.value <= self.precedence.value:
            return "(" + indent_next(str(expr), 1) + ")"
        return str(expr)


class Not(Expr):
    def __init__(self, expr: Expr, location: Location | None = None) -> None:
        super().__init__(ty.BOOLEAN, location)
        self.expr = expr

    def _update_str(self) -> None:
        self._str = intern(f"not {self.parenthesized(self.expr)}")

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.expr.check_type(ty.BOOLEAN)

    def __neg__(self) -> Expr:
        return self.expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR

    def variables(self) -> list[Variable]:
        return self.expr.variables()

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [
            *([self] if match(self) else []),
            *self.expr.findall(match),
        ]

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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
                *(Not(term.simplified()).simplified() for term in self.expr.terms),
            ).simplified()
        if isinstance(self.expr, Or):
            return And(
                *(Not(term.simplified()).simplified() for term in self.expr.terms),
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
                return inverse_relation(
                    self.expr.left.simplified(),
                    self.expr.right.simplified(),
                    self.location,
                )
        return self.__class__(self.expr.simplified(), self.location)


class BinExpr(Expr):
    def __init__(
        self,
        left: Expr,
        right: Expr,
        type_: ty.Type = ty.UNDEFINED,
        location: Location | None = None,
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
            f"{self.parenthesized(self.left)}{self.symbol}{self.parenthesized(self.right)}",
        )

    @abstractmethod
    def __neg__(self) -> Expr:
        raise NotImplementedError

    def __contains__(self, item: Expr) -> bool:
        return item == self or item in (self.left, self.right)

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def variables(self) -> list[Variable]:
        return list(unique(self.left.variables() + self.right.variables()))

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [
            *([self] if match(self) else []),
            *self.left.findall(match),
            *self.right.findall(match),
        ]

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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
            self.left.simplified(),
            self.right.simplified(),
            location=self.location,
        )

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class AssExpr(Expr):
    def __init__(self, *terms: Expr, location: Location | None = None) -> None:
        super().__init__(ty.UNDEFINED, location=location)
        self.terms = list(terms)

    def __repr__(self) -> str:
        return (
            f"\n{self.__class__.__name__}(\n"
            + ",\n".join([indent(repr(t), 4) for t in self.terms])
            + ")"
        )

    def _update_str(self) -> None:
        self._str = intern(
            (
                self.symbol.join(map(self.parenthesized, self.terms))
                if self.terms
                else str(self.neutral_element())
            ),
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

    def variables(self) -> list[Variable]:
        return list(unique([v for t in self.terms for v in t.variables()]))

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [
            *([self] if match(self) else []),
            *[m for t in self.terms for m in t.findall(match)],
        ]

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        expr = func(self)
        if isinstance(expr, AssExpr):
            return expr.__class__(
                *[t.substituted(func) for t in expr.terms],
                location=expr.location,
            )
        return expr

    def simplified(self) -> Expr:
        terms: list[Expr] = []
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
                return Number(total, location=self.location)
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
                if isinstance(term, relation) and inverse_relation(term.left, term.right) in terms:
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
                    t_location = t.condition_expressions[0][0].location
                    u_location = u.condition_expressions[0][0].location
                    return [
                        IfExpr(
                            [
                                (
                                    Or(
                                        t.condition_expressions[0][0],
                                        u.condition_expressions[0][0],
                                        location=Location.merge([t_location, u_location]),
                                    ).simplified(),
                                    t.condition_expressions[0][1],
                                ),
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
    def __init__(self, *terms: Expr, location: Location | None = None) -> None:
        super().__init__(*terms, location=location)
        self.type_ = ty.BOOLEAN

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
            error.extend(t.check_type(ty.BOOLEAN).entries)
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


class AndThen(And):
    @property
    def symbol(self) -> str:
        return " and then "


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


class OrElse(Or):
    @property
    def symbol(self) -> str:
        return " or else "


class Number(Expr):
    type_: ty.UniversalInteger

    def __init__(self, value: int, base: int = 0, location: Location | None = None) -> None:
        super().__init__(ty.UniversalInteger(ty.Bounds(value, value)), location)
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

    def _merge_locations(self, other: Number) -> Location:
        return Location.merge([self.location, other.location])

    def _check_type_subexpr(self) -> RecordFluxError:
        return RecordFluxError()

    def __hash__(self) -> int:
        return hash(self.value)

    def __int__(self) -> int:
        return self.value

    def __neg__(self) -> Number:
        return Number(-self.value)

    def __add__(self, other: object) -> Number:
        if isinstance(other, Number):
            return Number(self.value + other.value, location=self._merge_locations(other))
        return NotImplemented

    def __sub__(self, other: object) -> Number:
        if isinstance(other, Number):
            return Number(self.value - other.value, location=self._merge_locations(other))
        return NotImplemented

    def __mul__(self, other: object) -> Number:
        if isinstance(other, Number):
            return Number(self.value * other.value, location=self._merge_locations(other))
        return NotImplemented

    def __floordiv__(self, other: object) -> Expr:
        if isinstance(other, Number):
            if self.value % other.value == 0:
                return Number(self.value // other.value, location=self._merge_locations(other))
            return Div(Number(self.value), Number(other.value), self._merge_locations(other))
        return NotImplemented

    def __pow__(self, other: object) -> Number:
        if isinstance(other, Number):
            return Number(self.value**other.value, location=self._merge_locations(other))
        return NotImplemented

    def __mod__(self, other: object) -> Number:
        if isinstance(other, Number):
            return Number(self.value % other.value, location=self._merge_locations(other))
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


class Neg(Expr):
    def __init__(self, expr: Expr, location: Location | None = None) -> None:
        super().__init__(expr.type_, location)
        self.expr = expr

    def _update_str(self) -> None:
        self._str = intern(f"-{self.parenthesized(self.expr)}")

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.expr.check_type_instance(ty.AnyInteger)

    def __neg__(self) -> Expr:
        return self.expr

    @property
    def precedence(self) -> Precedence:
        return Precedence.UNARY_ADDING_OPERATOR

    def variables(self) -> list[Variable]:
        return self.expr.variables()

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [
            *([self] if match(self) else []),
            *self.expr.findall(match),
        ]

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        expr = func(self)
        if isinstance(expr, Neg):
            return expr.__class__(
                expr.expr.substituted(func),
                location=expr.location,
            )
        return expr

    def simplified(self) -> Expr:
        return -self.expr.simplified()


class MathAssExpr(AssExpr):
    def __init__(self, *terms: Expr, location: Location | None = None) -> None:
        super().__init__(*terms, location=location)
        common_type = ty.common_type([t.type_ for t in terms])
        self.type_ = common_type if common_type != ty.UNDEFINED else ty.BASE_INTEGER

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for t in self.terms:
            error.extend(t.check_type_instance(ty.AnyInteger).entries)
        return error


class Add(MathAssExpr):
    def _update_str(self) -> None:
        if not self.terms:
            self._str = intern(str(self.neutral_element()))
            return
        self._str = str(self.terms[0])
        for t in self.terms[1:]:
            if (isinstance(t, Number) and t.value < 0) or isinstance(t, Neg):
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
        expression = super().simplified()
        if not isinstance(expression, Add):
            return expression
        terms: list[Expr] = []
        for term in reversed(expression.terms):
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


class Mul(MathAssExpr):
    def __neg__(self) -> Expr:
        return Mul(*[*list(self.terms), Number(-1)]).simplified()

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


class MathBinExpr(BinExpr):
    def __init__(self, left: Expr, right: Expr, location: Location | None = None) -> None:
        super().__init__(left, right, ty.common_type([left.type_, right.type_]), location)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error.extend(e.check_type_instance(ty.AnyInteger).entries)

        common_type = ty.common_type([self.left.type_, self.right.type_])
        self.type_ = common_type if common_type != ty.UNDEFINED else ty.BASE_INTEGER

        return error


class Sub(MathBinExpr):
    def __neg__(self) -> Expr:
        return self.__class__(self.right, self.left, location=self.location)

    @property
    def precedence(self) -> Precedence:
        return Precedence.BINARY_ADDING_OPERATOR

    def simplified(self) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if isinstance(left, Number) and isinstance(right, Number):
            return left - right
        return Add(
            left,
            -right,
            location=Location.merge([left.location, right.location]),
        )

    @property
    def symbol(self) -> str:
        return " - "


class Div(MathBinExpr):
    def __neg__(self) -> Expr:
        return self.__class__(-self.left, self.right, location=self.location)

    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    def simplified(self) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if isinstance(left, Number) and isinstance(right, Number):
            if right.value == 0:
                raise RecordFluxError(
                    [
                        ErrorEntry(
                            "division by zero",
                            Severity.ERROR,
                            self.location,
                        ),
                    ],
                )
            return left // right
        return Div(left, right)

    @property
    def symbol(self) -> str:
        return " / "


class Pow(MathBinExpr):
    def __neg__(self) -> Expr:
        return Neg(self)

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


class Mod(MathBinExpr):
    def __neg__(self) -> Expr:
        return Neg(self)

    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    def simplified(self) -> Expr:
        left = self.left.simplified()
        right = self.right.simplified()
        if isinstance(left, Number) and isinstance(right, Number):
            if right.value == 0:
                raise RecordFluxError(
                    [
                        ErrorEntry(
                            "modulo by zero",
                            Severity.ERROR,
                            self.location,
                        ),
                    ],
                )
            return left % right
        return Mod(left, right)

    @property
    def symbol(self) -> str:
        return " mod "


class Rem(MathBinExpr):
    """Only used by code generator and therefore provides minimum functionality."""

    def __neg__(self) -> Expr:
        return Neg(self)

    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    @property
    def symbol(self) -> str:
        return " rem "


class Name(Expr):
    def __init__(
        self,
        type_: ty.Type = ty.UNDEFINED,
        location: Location | None = None,
    ) -> None:
        super().__init__(type_, location)
        self._update_str()

    def _update_str(self) -> None:
        self._str = intern(self.representation)

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    @property
    @abstractmethod
    def representation(self) -> str:
        raise NotImplementedError

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        return func(self)

    def simplified(self) -> Expr:
        return self


class TypeName(Name):
    def __init__(
        self,
        identifier: StrID,
        type_: ty.Type = ty.UNDEFINED,
        location: Location | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        super().__init__(type_=type_, location=location)

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

    def variables(self) -> list[Variable]:
        return []


class Literal(Name):
    def __init__(
        self,
        identifier: StrID,
        type_: ty.Type = ty.UNDEFINED,
    ) -> None:
        self.identifier = ID(identifier)
        super().__init__(type_=type_, location=self.identifier.location)

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

    def variables(self) -> list[Variable]:
        return []


class Variable(Name):
    def __init__(
        self,
        identifier: StrID,
        type_: ty.Type = ty.UNDEFINED,
    ) -> None:
        self.identifier = ID(identifier)
        super().__init__(type_, self.identifier.location)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.identifier == other.identifier
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __neg__(self) -> Neg:
        return Neg(self, self.location)

    def _check_type_subexpr(self) -> RecordFluxError:
        return RecordFluxError()

    @property
    def name(self) -> str:
        return str(self.identifier)

    @property
    def representation(self) -> str:
        return str(self.name)

    def variables(self) -> list[Variable]:
        return [self]


TRUE = Literal(
    ID("True", location=Location((1, 1), Path(str(const.BUILTINS_PACKAGE)), (1, 1))),
    type_=ty.BOOLEAN,
)
FALSE = Literal(
    ID("False", location=Location((1, 1), Path(str(const.BUILTINS_PACKAGE)), (1, 1))),
    type_=ty.BOOLEAN,
)


class Attribute(Name):
    def __init__(self, prefix: StrID | Expr) -> None:
        if isinstance(prefix, ID):
            prefix = Variable(prefix)
        if isinstance(prefix, str):
            prefix = Variable(prefix)

        self._prefix: Expr = prefix
        super().__init__(location=prefix.location)

    @property
    def representation(self) -> str:
        return f"{self.prefix}'{self.symbol}"

    @property
    def symbol(self) -> str:
        return self.__class__.__name__

    @property
    def prefix(self) -> Expr:
        return self._prefix

    def __neg__(self) -> Neg:
        return Neg(self)

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [self] if match(self) else self.prefix.findall(match)

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        expr = func(self)
        if isinstance(expr, Attribute):
            prefix = expr.prefix.substituted(func)
            if not isinstance(prefix, Attribute):
                expr = expr.__class__(prefix)
        return expr

    def simplified(self) -> Expr:
        return self.__class__(self.prefix.simplified())

    def variables(self) -> list[Variable]:
        return self.prefix.variables()


class Size(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.UNIVERSAL_INTEGER

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(ty.Any)


class Length(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.UNIVERSAL_INTEGER

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(ty.Any)


class First(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.UNIVERSAL_INTEGER

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(ty.Any)


class Last(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.UNIVERSAL_INTEGER

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(ty.Any)


class ValidChecksum(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.BOOLEAN

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(ty.Any)

    @property
    def representation(self) -> str:
        return f"{self.prefix}'Valid_Checksum"


class Valid(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.BOOLEAN

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(
            (ty.Sequence, ty.Message) if isinstance(self.prefix, Variable) else (ty.Any,),
        )


class Present(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.BOOLEAN

    def _check_type_subexpr(self) -> RecordFluxError:
        if isinstance(self.prefix, Selected):
            error = self.prefix.prefix.check_type_instance(ty.Message)
        else:
            error = RecordFluxError(
                [
                    ErrorEntry(
                        "invalid prefix for attribute Present",
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )
        return error


class HasData(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.BOOLEAN

    @property
    def symbol(self) -> str:
        return "Has_Data"

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance(ty.Message)


class Head(Attribute):
    def __init__(
        self,
        prefix: StrID | Expr,
        type_: ty.Type = ty.UNDEFINED,
    ):
        super().__init__(prefix)
        self.type_ = type_

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.prefix.check_type_instance(ty.Composite)
        self.type_ = (
            self.prefix.type_.element if isinstance(self.prefix.type_, ty.Composite) else ty.Any()
        )
        if not isinstance(self.prefix, (Variable, Selected, Comprehension)):
            error.push(
                ErrorEntry(
                    "prefix of attribute Head must be a name or comprehension",
                    Severity.ERROR,
                    self.prefix.location,
                ),
            )
        return error


class Opaque(Attribute):
    def __init__(self, prefix: StrID | Expr) -> None:
        super().__init__(prefix)
        self.type_ = ty.OPAQUE

    def _check_type_subexpr(self) -> RecordFluxError:
        return self.prefix.check_type_instance((ty.Sequence, ty.Message))


class Constrained(Attribute):
    """Only used by code generator and therefore provides minimum functionality."""

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError


class Val(Attribute):
    """Only used by code generator and therefore provides minimum functionality."""

    def __init__(
        self,
        prefix: StrID | Expr,
        expression: Expr,
    ) -> None:
        self.expression = expression
        super().__init__(prefix)

    def __neg__(self) -> Neg:
        return Neg(self)

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    def variables(self) -> list[Variable]:
        raise NotImplementedError

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        raise NotImplementedError

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:  # noqa: ARG002
        return self

    def simplified(self) -> Expr:
        return self

    @property
    def representation(self) -> str:
        return f"{self.prefix}'{self.__class__.__name__} ({self.expression})"


class Indexed(Name):
    """Only used by code generator and therefore provides minimum functionality."""

    def __init__(self, prefix: Expr, *elements: Expr) -> None:
        assert len(elements) > 0
        self.prefix = prefix
        self.elements = list(elements)
        super().__init__()

    def __neg__(self) -> Neg:
        return Neg(self)

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError

    @property
    def representation(self) -> str:
        return f"{self.prefix} (" + ", ".join(map(str, self.elements)) + ")"


class Selected(Name):
    def __init__(
        self,
        prefix: Expr,
        selector: StrID,
        type_: ty.Type = ty.UNDEFINED,
        location: Location | None = None,
    ) -> None:
        self.prefix = prefix
        self.selector = ID(selector)
        super().__init__(type_, location)

    def __neg__(self) -> Neg:
        return Neg(self)

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [
            *([self] if match(self) else []),
            *self.prefix.findall(match),
        ]

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        if isinstance(self.prefix.type_, ty.Message):
            if self.selector in self.prefix.type_.types:
                self.type_ = self.prefix.type_.types[self.selector]
            else:
                error.extend(
                    [
                        ErrorEntry(
                            f'invalid field "{self.selector}" for {self.prefix.type_}',
                            Severity.ERROR,
                            self.location,
                        ),
                        *_similar_field_names(
                            self.selector,
                            self.prefix.type_.parameters | self.prefix.type_.fields,
                            self.location,
                        ),
                    ],
                )
                self.type_ = ty.Any()
        else:
            self.type_ = ty.Any()
        error.extend(self.prefix.check_type_instance(ty.Message).entries)
        return error

    @property
    def representation(self) -> str:
        return f"{self.prefix}.{self.selector}"

    def variables(self) -> list[Variable]:
        return self.prefix.variables()

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        expr = func(self)
        if isinstance(expr, self.__class__):
            return expr.__class__(
                expr.prefix.substituted(func),
                expr.selector,
                expr.type_,
                expr.location,
            )
        return expr

    def copy(
        self,
        prefix: Expr | None = None,
        selector: StrID | None = None,
        type_: ty.Type | None = None,
        location: Location | None = None,
    ) -> Selected:
        return self.__class__(
            prefix if prefix is not None else self.prefix,
            ID(selector) if selector is not None else self.selector,
            type_ if type_ is not None else self.type_,
            location if location is not None else self.location,
        )


class Call(Name):
    def __init__(
        self,
        identifier: StrID,
        type_: ty.Type,
        args: Sequence[Expr] | None = None,
        argument_types: Sequence[ty.Type] | None = None,
        location: Location | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.args = args or []
        self.argument_types = argument_types or []
        super().__init__(type_, location)

    def __neg__(self) -> Neg:
        return Neg(self)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()

        for a, t in itertools.zip_longest(self.args, self.argument_types[: len(self.args)]):
            error.extend(a.check_type(t if t is not None else ty.Any()).entries)

        if self.type_ != ty.UNDEFINED:
            if len(self.args) < len(self.argument_types):
                error.push(
                    ErrorEntry(
                        "missing function arguments",
                        Severity.ERROR,
                        self.location,
                    ),
                )

            if len(self.args) > len(self.argument_types):
                error.push(
                    ErrorEntry(
                        "too many function arguments",
                        Severity.ERROR,
                        self.location,
                    ),
                )

        return error

    @property
    def representation(self) -> str:
        args = ", ".join(map(str, self.args))
        if args:
            args = f" ({args})"
        return f"{self.identifier}{args}"

    def variables(self) -> list[Variable]:
        result = [Variable(self.identifier)]
        for t in self.args:
            result.extend(t.variables())
        return result

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [
            *([self] if match(self) else []),
            *[e for a in self.args for e in a.findall(match)],
        ]

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        expr = func(self)
        assert isinstance(expr, Call)
        return expr.__class__(
            expr.identifier,
            expr.type_,
            [a.substituted(func) for a in expr.args],
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


class UndefinedExpr(Name):
    @property
    def representation(self) -> str:
        return "__UNDEFINED__"

    def __neg__(self) -> UndefinedExpr:
        raise NotImplementedError

    def _check_type_subexpr(self) -> RecordFluxError:
        raise NotImplementedError


UNDEFINED = UndefinedExpr(location=Location((1, 1)))


class Aggregate(Expr):
    def __init__(self, *elements: Expr, location: Location | None = None) -> None:
        super().__init__(ty.Aggregate(ty.common_type([e.type_ for e in elements])), location)
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
            error.extend(e.check_type_instance(ty.Any).entries)
        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        expr = func(self)
        if isinstance(expr, self.__class__):
            return expr.__class__(
                *[e.substituted(func) for e in expr.elements],
                location=expr.location,
            )
        return expr

    def simplified(self) -> Expr:
        return self.__class__(*[e.simplified() for e in self.elements], location=self.location)

    @property
    def length(self) -> Expr:
        return Number(len(self.elements))


class String(Aggregate):
    def __init__(self, data: str, location: Location | None = None) -> None:
        super().__init__(*[Number(ord(d)) for d in data], location=location)
        self.data = data

    def _update_str(self) -> None:
        self._str = intern(f'"{self.data}"')

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        return func(self)

    def simplified(self) -> Expr:
        return self


class NamedAggregate(Expr):
    """Only used by code generator and therefore provides minimum functionality."""

    def __init__(self, *elements: tuple[StrID | Expr, Expr]) -> None:
        super().__init__()
        self.elements = [(ID(n) if isinstance(n, str) else n, e) for n, e in elements]

    def _update_str(self) -> None:
        assert len(self.elements) > 0
        self._str = intern(
            "(" + ", ".join(f"{name} => {element}" for name, element in self.elements) + ")",
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


class Relation(BinExpr):
    def __init__(self, left: Expr, right: Expr, location: Location | None = None) -> None:
        super().__init__(left, right, ty.BOOLEAN, location)

    @abstractmethod
    def __neg__(self) -> Expr:
        raise NotImplementedError

    def _simplified(
        self,
        relation_operator: Callable[[SupportsAllComparisons, SupportsAllComparisons], bool],
    ) -> Expr:
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

        if (
            isinstance(left, Div)
            and isinstance(right, Div)
            and isinstance(left.left, Number)
            and isinstance(left.right, Number)
            and isinstance(right.left, Number)
            and isinstance(right.right, Number)
        ):
            return (
                TRUE
                if relation_operator(
                    fractions.Fraction(left.left.value, left.right.value),
                    fractions.Fraction(right.left.value, right.right.value),
                )
                else FALSE
            )

        # We simplify expressions of the form X = True to X
        # We need to negate X if the Boolean literal is False or the relation is "ne",
        # but not when both are true
        if relation_operator in [operator.eq, operator.ne]:

            def apply_op(e: Expr, invert: bool) -> Expr:
                if invert:
                    return Not(e, e.location)
                return e

            if left in [TRUE, FALSE]:
                return apply_op(right, (left == FALSE) != (relation_operator == operator.ne))
            if right in [TRUE, FALSE]:
                return apply_op(left, (right == FALSE) != (relation_operator == operator.ne))

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
            error.extend(e.check_type_instance(ty.AnyInteger).entries)
        return error

    @property
    def symbol(self) -> str:
        return " < "

    def simplified(self) -> Expr:
        return self._simplified(operator.lt)


class LessEqual(Relation):
    def __neg__(self) -> Expr:
        return Greater(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error.extend(e.check_type_instance(ty.AnyInteger).entries)
        return error

    @property
    def symbol(self) -> str:
        return " <= "

    def simplified(self) -> Expr:
        return self._simplified(operator.le)


class Equal(Relation):
    def __neg__(self) -> Expr:
        return NotEqual(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.left.check_type_instance(ty.Any)
        error.extend(self.right.check_type(self.left.type_).entries)
        return error

    @property
    def symbol(self) -> str:
        return " = "

    def simplified(self) -> Expr:
        return self._simplified(operator.eq)


class GreaterEqual(Relation):
    def __neg__(self) -> Expr:
        return Less(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error.extend(e.check_type_instance(ty.AnyInteger).entries)
        return error

    @property
    def symbol(self) -> str:
        return " >= "

    def simplified(self) -> Expr:
        return self._simplified(operator.ge)


class Greater(Relation):
    def __neg__(self) -> Expr:
        return LessEqual(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.left, self.right]:
            error.extend(e.check_type_instance(ty.AnyInteger).entries)
        return error

    @property
    def symbol(self) -> str:
        return " > "

    def simplified(self) -> Expr:
        return self._simplified(operator.gt)


class NotEqual(Relation):
    def __neg__(self) -> Expr:
        return Equal(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.left.check_type_instance(ty.Any)
        error.extend(self.right.check_type(self.left.type_).entries)
        return error

    @property
    def symbol(self) -> str:
        return " /= "

    def simplified(self) -> Expr:
        return self._simplified(operator.ne)


class In(Relation):
    def __neg__(self) -> Expr:
        return NotIn(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.left.check_type_instance(ty.Any)
        error.extend(
            self.right.check_type(
                ty.Aggregate(self.left.type_),
            ).entries,
        )
        return error

    @property
    def symbol(self) -> str:
        return " in "


class NotIn(Relation):
    def __neg__(self) -> Expr:
        return In(self.left, self.right)

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.left.check_type_instance(ty.Any)
        error.extend(
            self.right.check_type(
                ty.Aggregate(self.left.type_),
            ).entries,
        )
        return error

    @property
    def symbol(self) -> str:
        return " not in "


class IfExpr(Expr):
    def __init__(
        self,
        condition_expressions: Sequence[tuple[Expr, Expr]],
        else_expression: Expr | None = None,
    ) -> None:
        super().__init__(
            ty.common_type(
                [
                    *[e.type_ for _, e in condition_expressions],
                    *([else_expression.type_] if else_expression else []),
                ],
            ),
        )
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
                (
                    f"if\n{indent(c, 4)}\n then\n{indent(e, 4)}"
                    if i == 0
                    else f"\n elsif\n{indent(c, 4)}\n then\n{indent(e, 4)}"
                )
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

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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


class QuantifiedExpr(Expr):
    def __init__(
        self,
        parameter_identifier: StrID,
        iterable: Expr,
        predicate: Expr,
        location: Location | None = None,
    ) -> None:
        super().__init__(ty.BOOLEAN, location)
        self.parameter_identifier = ID(parameter_identifier)
        self.iterable = iterable
        self.predicate = predicate

    def _update_str(self) -> None:
        self._str = intern(
            f"(for {self.quantifier} {self.parameter_identifier} {self.keyword} {self.iterable}"
            f" =>\n{indent(str(self.predicate), 4)})",
        )

    def _check_type_subexpr(self) -> RecordFluxError:
        def typify_variable(expr: Expr) -> Expr:
            if isinstance(expr, Variable) and expr.identifier == self.parameter_identifier:
                if isinstance(self.iterable.type_, (ty.Aggregate, ty.Sequence)):
                    expr.type_ = self.iterable.type_.element
                else:
                    expr.type_ = ty.Any()
            return expr

        error = self.iterable.check_type_instance(ty.Composite)

        self.predicate = self.predicate.substituted(typify_variable)

        error.extend(self.predicate.check_type(ty.BOOLEAN).entries)
        return error

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

    def variables(self) -> list[Variable]:
        return list(
            unique(
                v
                for v in self.iterable.variables() + self.predicate.variables()
                if v.identifier != self.parameter_identifier
            ),
        )

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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
    def __init__(self, lower: Expr, upper: Expr, location: Location | None = None):
        super().__init__(ty.Any(), location)
        self.lower = lower
        self.upper = upper

    def _update_str(self) -> None:
        self._str = intern(f"{self.lower} .. {self.upper}")

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        for e in [self.lower, self.upper]:
            error.extend(e.check_type_instance(ty.AnyInteger).entries)
        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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


class Conversion(Expr):
    def __init__(
        self,
        identifier: StrID,
        argument: Expr,
        type_: ty.Type = ty.UNDEFINED,
        argument_types: Sequence[ty.Type] | None = None,
        location: Location | None = None,
    ) -> None:
        super().__init__(type_, location)
        self.identifier = ID(identifier)
        self.argument = argument
        self.argument_types = argument_types or []

    def _update_str(self) -> None:
        self._str = intern(f"{self.identifier} ({self.argument})")

    def _check_type_subexpr(self) -> RecordFluxError:
        error = self.argument.check_type(ty.OPAQUE)

        if isinstance(self.argument, Selected):
            if self.argument_types:
                error.extend(self.argument.prefix.check_type(tuple(self.argument_types)).entries)
            else:
                error.push(
                    ErrorEntry(
                        f'invalid conversion to "{self.identifier}"',
                        Severity.ERROR,
                        self.location,
                        annotations=(
                            [
                                Annotation(
                                    "refinement for message "
                                    f'"{self.argument.prefix.type_.identifier}"'
                                    " would make operation legal",
                                    Severity.HELP,
                                    self.location,
                                ),
                            ]
                            if isinstance(self.argument.prefix.type_, ty.Message)
                            else []
                        ),
                    ),
                )
        else:
            error.push(
                ErrorEntry(
                    "invalid argument for conversion, expected message field",
                    Severity.ERROR,
                    self.argument.location,
                ),
            )

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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

    def variables(self) -> list[Variable]:
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


class Comprehension(Expr):
    type_: ty.Aggregate

    def __init__(
        self,
        iterator: StrID,
        sequence: Expr,
        selector: Expr,
        condition: Expr,
        location: Location | None = None,
    ) -> None:
        super().__init__(ty.Aggregate(selector.type_), location)
        self.iterator = ID(iterator)
        self.sequence = sequence
        self.selector = selector
        self.condition = condition

    def _update_str(self) -> None:
        self._str = intern(
            f"[for {self.iterator} in {self.sequence} if {self.condition} => {self.selector}]",
        )

    def _check_type_subexpr(self) -> RecordFluxError:
        def typify_variable(expr: Expr) -> Expr:
            if isinstance(expr, Variable) and expr.identifier == self.iterator:
                if isinstance(self.sequence.type_, (ty.Aggregate, ty.Sequence)):
                    expr.type_ = self.sequence.type_.element
                else:
                    expr.type_ = ty.Any()
            return expr

        error = self.sequence.check_type_instance(ty.Composite)

        self.selector = self.selector.substituted(typify_variable)
        self.condition = self.condition.substituted(typify_variable)

        error.extend(self.selector.check_type_instance(ty.Any).entries)
        error.extend(self.condition.check_type(ty.BOOLEAN).entries)

        self.type_ = ty.Aggregate(self.selector.type_)

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

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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

    def variables(self) -> list[Variable]:
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
        type_: ty.Type = ty.UNDEFINED,
        location: Location | None = None,
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
        error = RecordFluxError()

        if not isinstance(self.type_, ty.Message):
            for d in self.field_values.values():
                error.extend(d.check_type_instance(ty.Any).entries)

            return error

        error.extend(self._check_for_invalid_fields().entries)
        error.extend(self._check_for_missing_fields().entries)
        return error

    def _field_combinations(self) -> set[tuple[str, ...]]:
        assert isinstance(self.type_, ty.Message)

        return set(self.type_.field_combinations)

    def _matching_field_combinations(self, field_position: int) -> set[tuple[str, ...]]:
        field = list(self.field_values)[field_position]
        return {
            c
            for c in self._field_combinations()
            if len(c) > field_position and c[field_position] == str(field)
        }

    def _check_for_invalid_fields(self) -> RecordFluxError:
        assert isinstance(self.type_, ty.Message)

        error = RecordFluxError()

        for i, (field, expression) in enumerate(self.field_values.items()):
            if field not in self.type_.types:
                error.extend(
                    [
                        ErrorEntry(
                            f'invalid field "{field}" for {self.type_}',
                            Severity.ERROR,
                            field.location,
                        ),
                        *_similar_field_names(field, self.type_.types, field.location),
                    ],
                )
                continue

            field_type = self.type_.types[field]

            if field_type == ty.OPAQUE:
                if not any(
                    r.field == field and expression.type_.is_compatible(r.sdu)
                    for r in self.type_.refinements
                ):
                    error.extend(expression.check_type(field_type).entries)
            else:
                error.extend(expression.check_type(field_type).entries)

            if not self._matching_field_combinations(i):
                error.push(
                    ErrorEntry(
                        f'invalid position for field "{field}" of {self.type_}',
                        Severity.ERROR,
                        field.location,
                    ),
                )
                break

        return error

    def _check_for_missing_fields(self) -> RecordFluxError:
        error = RecordFluxError()

        if self._field_combinations() and all(
            len(c) > len(self.field_values) for c in self._field_combinations()
        ):
            error.push(
                ErrorEntry(
                    f"missing fields for {self.type_}",
                    Severity.ERROR,
                    self.location,
                    annotations=(
                        [
                            Annotation(
                                "possible next fields: "
                                + ", ".join(
                                    unique(
                                        c[len(self.field_values)]
                                        for c in sorted(self._field_combinations())
                                    ),
                                ),
                                Severity.NOTE,
                                self.location,
                            ),
                        ]
                    ),
                ),
            )

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
        return [
            *([self] if match(self) else []),
            *[e for v in self.field_values.values() for e in v.findall(match)],
        ]

    def simplified(self) -> Expr:
        return self.__class__(
            self.identifier,
            {k: self.field_values[k].simplified() for k in self.field_values},
            self.type_,
            self.location,
        )

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
        expr = func(self)
        if isinstance(expr, self.__class__):
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

    def variables(self) -> list[Variable]:
        result = []
        for v in self.field_values.values():
            result.extend(v.variables())
        return result


class DeltaMessageAggregate(MessageAggregate):
    """For internal use only."""

    def _update_str(self) -> None:
        field_values = (
            ", ".join([f"{k} => {self.field_values[k]}" for k in self.field_values])
            if self.field_values
            else "null message"
        )
        self._str = intern(f"{self.identifier} with delta {field_values}")

    def _matching_field_combinations(self, field_position: int) -> set[tuple[str, ...]]:
        fields = tuple(str(f) for i, f in enumerate(self.field_values) if i <= field_position)
        return {
            c
            for c in self._field_combinations()
            if any(fields == c[i : len(fields) + i] for i in range(len(c) - len(fields) + 1))
        }

    def _check_for_missing_fields(self) -> RecordFluxError:
        return RecordFluxError()


def substitution(mapping: Mapping[Expr, Expr]) -> Callable[[Expr], Expr]:
    return lambda expression: mapping.get(expression, expression)


def _entity_name(expr: Expr) -> str:
    expr_type = (
        "variable"
        if isinstance(expr, Variable)
        else (
            "literal"
            if isinstance(expr, Literal)
            else (
                "function"
                if isinstance(expr, Call)
                else (
                    "type"
                    if isinstance(expr, Conversion)
                    else (
                        "message"
                        if isinstance(expr, (MessageAggregate, DeltaMessageAggregate))
                        else "expression"
                    )
                )
            )
        )
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
        choices: Sequence[tuple[Sequence[ID | Number], Expr]],
        location: Location | None = None,
    ) -> None:
        super().__init__(ty.common_type([e.type_ for _, e in choices]), location)
        self.expr = expr
        self.choices = choices

    def _update_str(self) -> None:
        data = ",\n".join(f"      when {' | '.join(map(str, c))} => {e}" for c, e in self.choices)
        self._str = intern(f"(case {self.expr} is\n{data})")

    def _check_enumeration(self) -> RecordFluxError:
        assert isinstance(self.expr.type_, ty.Enumeration)
        assert self.expr.type_.literals

        error = RecordFluxError()
        literals = [
            c.name for (choice, _) in self.choices for c in choice if isinstance(c, (str, ID))
        ]
        type_literals = [l.name for l in self.expr.type_.literals]
        missing = set(type_literals) - set(literals)
        if missing:
            error.push(
                ErrorEntry(
                    "not all enumeration literals covered by case expression",
                    Severity.ERROR,
                    self.location,
                    annotations=(
                        [
                            Annotation(
                                f'missing literal "{l.name}"',
                                Severity.NOTE,
                                self.expr.type_.location,
                            )
                            for l in missing
                        ]
                    ),
                ),
            )

        invalid = set(literals) - set(type_literals)
        if invalid:
            error.push(
                ErrorEntry(
                    "invalid literals used in case expression",
                    Severity.ERROR,
                    self.location,
                    annotations=(
                        [
                            Annotation(
                                f'literal "{l.name}" not part of "{self.expr.type_.identifier}"',
                                Severity.NOTE,
                                self.expr.type_.location,
                            )
                            for l in invalid
                        ]
                    ),
                ),
            )
        return error

    def _check_integer(self) -> RecordFluxError:
        assert isinstance(self.expr.type_, ty.Integer)
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

            error.push(
                ErrorEntry(
                    f"case expression does not cover full range of "
                    f'"{self.expr.type_.identifier}"',
                    Severity.ERROR,
                    self.location,
                    annotations=(
                        [
                            Annotation(
                                (
                                    f"missing range {r[0]} .. {r[1]}"
                                    if r[0] != r[1]
                                    else f"missing value {r[0]}"
                                ),
                                Severity.NOTE,
                                self.expr.type_.location,
                            )
                            for r in missing_ranges
                        ]
                    ),
                ),
            )

        invalid = set(literals) - set(type_literals)
        if invalid:
            error.push(
                ErrorEntry(
                    "invalid literals used in case expression",
                    Severity.ERROR,
                    self.location,
                    annotations=(
                        [
                            Annotation(
                                f'value {l} not part of "{self.expr.type_.identifier}"',
                                Severity.NOTE,
                                self.expr.type_.location,
                            )
                            for l in invalid
                        ]
                    ),
                ),
            )

        return error

    def _check_type_subexpr(self) -> RecordFluxError:
        error = RecordFluxError()
        result_type: ty.Type = ty.Any()
        literals = [c for (choice, _) in self.choices for c in choice]

        for _, expression in self.choices:
            error.extend(expression.check_type_instance(ty.Any).entries)
            result_type = result_type.common_type(expression.type_)

        for i1, (_, e1) in enumerate(self.choices):
            for i2, (_, e2) in enumerate(self.choices):
                if i1 < i2 and not e1.type_.is_compatible(e2.type_):
                    error.push(
                        ErrorEntry(
                            f'dependent expression "{e1}" has incompatible {e1.type_}',
                            Severity.ERROR,
                            e1.location,
                            annotations=[
                                Annotation(
                                    f'conflicting with "{e2}" which has {e2.type_}',
                                    Severity.NOTE,
                                    e2.location,
                                ),
                            ],
                        ),
                    )

        error.extend(self.expr.check_type_instance(ty.Any).entries)
        error.propagate()

        duplicates = [
            e1
            for i1, e1 in enumerate(literals)
            for i2, e2 in enumerate(literals)
            if i1 > i2 and e1 == e2
        ]
        if duplicates:
            error.push(
                ErrorEntry(
                    "duplicate literals used in case expression",
                    Severity.ERROR,
                    self.location,
                    annotations=[
                        Annotation(
                            f'duplicate literal "{link}"',
                            Severity.NOTE,
                            link.location,
                        )
                        for link in duplicates
                    ],
                ),
            )

        if isinstance(self.expr.type_, ty.Enumeration):
            error.extend(self._check_enumeration().entries)
        elif isinstance(self.expr.type_, ty.Integer):
            error.extend(self._check_integer().entries)
        else:
            error.push(
                ErrorEntry(
                    f"invalid discrete choice with {self.expr.type_}",
                    Severity.ERROR,
                    self.expr.location,
                    annotations=(
                        [
                            Annotation(
                                "expected enumeration or integer type",
                                Severity.NOTE,
                                self.expr.location,
                            ),
                        ]
                    ),
                ),
            )

        self.type_ = result_type

        return error

    def __neg__(self) -> Expr:
        raise NotImplementedError

    def findall(self, match: Callable[[Expr], bool]) -> list[Expr]:
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

    def substituted(self, func: Callable[[Expr], Expr]) -> Expr:
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

    def variables(self) -> list[Variable]:
        simplified = self.simplified()
        assert isinstance(simplified, CaseExpr)
        return list(
            unique(
                [
                    *simplified.expr.variables(),
                    *[v for _, e in simplified.choices for v in e.variables()],
                ],
            ),
        )


def similar_fields(
    field: ID,
    fields: Iterable[ID],
) -> list[ID]:
    field_similarity = sorted(
        ((f, difflib.SequenceMatcher(None, str(f), str(field)).ratio()) for f in sorted(fields)),
        key=lambda x: x[1],
        reverse=True,
    )
    return [f for f, s in field_similarity if s >= 0.5]


def _similar_field_names(
    field: ID,
    fields: Iterable[ID],
    location: Location,
) -> list[ErrorEntry]:
    similar_flds = similar_fields(field, fields)
    if similar_flds:
        return [
            ErrorEntry(
                "similar field names: " + ", ".join(str(f) for f in similar_flds),
                Severity.HELP,
                location,
            ),
        ]
    return []
