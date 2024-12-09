from __future__ import annotations

import itertools
import os
from abc import abstractmethod
from collections import OrderedDict
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field as dataclass_field
from enum import Enum

from typing_extensions import Self

from rflx import expr, ty
from rflx.common import Base, file_name, indent, indent_next, unique
from rflx.identifier import ID, StrID

MAX_LINE_LENGTH = 100


class Precedence(Enum):
    UNDEFINED = 0
    BOOLEAN_OPERATOR = 1
    RELATIONAL_OPERATOR = 2
    BINARY_ADDING_OPERATOR = 3
    UNARY_ADDING_OPERATOR = 4
    MULTIPLYING_OPERATOR = 5
    HIGHEST_PRECEDENCE_OPERATOR = 6
    LITERAL = 7


class Expr(Base):
    def __neg__(self) -> Expr:
        return Neg(self)

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def parenthesized(self, expression: Expr) -> str:
        if expression.precedence.value <= self.precedence.value:
            return "(" + indent_next(str(expression), 1) + ")"
        return str(expression)

    @property
    def parenthesized_if_needed(self) -> str:
        return f"({self})"

    @abstractmethod
    def rflx_expr(self) -> expr.Expr:
        raise NotImplementedError


class Not(Expr):
    def __init__(self, expression: Expr) -> None:
        super().__init__()
        self.expression = expression

    def __str__(self) -> str:
        return f"not {self.parenthesized(self.expression)}"

    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR

    def rflx_expr(self) -> expr.Not:
        return expr.Not(self.expression.rflx_expr())


class Neg(Expr):
    def __init__(self, expression: Expr) -> None:
        super().__init__()
        self.expression = expression

    def __neg__(self) -> Expr:
        return self.expression

    def __str__(self) -> str:
        return f"-{self.parenthesized(self.expression)}"

    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR

    def rflx_expr(self) -> expr.Neg:
        e = self.expression.rflx_expr()
        return expr.Neg(e, e.location)


class BinExpr(Expr):
    def __init__(self, left: Expr, right: Expr) -> None:
        super().__init__()
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return f"{self.parenthesized(self.left)}{self.symbol}{self.parenthesized(self.right)}"

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError

    def rflx_expr(self) -> expr.BinExpr:
        result = getattr(expr, self.__class__.__name__)(
            self.left.rflx_expr(),
            self.right.rflx_expr(),
        )
        assert isinstance(result, expr.BinExpr)
        return result


class AssExpr(Expr):
    def __init__(self, *terms: Expr) -> None:
        super().__init__()
        self.terms = list(terms)

    def __str__(self) -> str:
        return self.symbol.join(map(self.parenthesized, self.terms))

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError

    def rflx_expr(self) -> expr.AssExpr:
        result = getattr(expr, self.__class__.__name__)(*[t.rflx_expr() for t in self.terms])
        assert isinstance(result, expr.AssExpr)
        return result


class BoolAssExpr(AssExpr):
    def __str__(self) -> str:
        if not self.terms:
            return str(TRUE)
        result = ""
        for i, t in reversed(list(enumerate(self.terms))):
            if i == 0:
                result = self.parenthesized(t) + result
            else:
                result = (
                    "\n"
                    + str(self.symbol)[1:]
                    + indent_next(self.parenthesized(t), len(self.symbol) - 1)
                    + result
                )
        return result

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class And(BoolAssExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.BOOLEAN_OPERATOR

    @property
    def symbol(self) -> str:
        return " and "


class AndThen(And):
    @property
    def symbol(self) -> str:
        return " and then "


class Or(BoolAssExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.BOOLEAN_OPERATOR

    @property
    def symbol(self) -> str:
        return " or "


class OrElse(Or):
    @property
    def symbol(self) -> str:
        return " or else "


class Number(Expr):
    def __init__(self, value: int, base: int = 0) -> None:
        super().__init__()
        self.value = value
        self.base = base

    def __neg__(self) -> Number:
        return Number(-self.value)

    def __str__(self) -> str:
        value = self.value if self.value >= 0 else -self.value
        if self.base == 0:
            _str = f"{value}"
        elif self.base == 2:
            _str = f"2#{value:b}#"
        elif self.base == 8:
            _str = f"8#{value:o}#"
        elif self.base == 10:
            _str = f"10#{value}#"
        elif self.base == 16:
            _str = f"16#{value:X}#"
        else:
            raise NotImplementedError(f"unsupported base {self.base}")
        return f"(-{_str})" if self.value < 0 else _str

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.Number:
        return expr.Number(self.value, self.base)


class Add(AssExpr):
    def __str__(self) -> str:
        result = str(self.terms[0])
        for t in self.terms[1:]:
            if (isinstance(t, Number) and t.value < 0) or isinstance(t, Neg):
                result += f" - {self.parenthesized(-t)}"
            else:
                result += f"{self.symbol}{self.parenthesized(t)}"
        return result

    @property
    def precedence(self) -> Precedence:
        return Precedence.BINARY_ADDING_OPERATOR

    @property
    def symbol(self) -> str:
        return " + "


class Concatenation(AssExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.BINARY_ADDING_OPERATOR

    @property
    def symbol(self) -> str:
        return " & "


class Mul(AssExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    @property
    def symbol(self) -> str:
        return " * "


class Sub(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.BINARY_ADDING_OPERATOR

    @property
    def symbol(self) -> str:
        return " - "


class Div(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    @property
    def symbol(self) -> str:
        return " / "


class Pow(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR

    @property
    def symbol(self) -> str:
        return "**"


class Mod(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    @property
    def symbol(self) -> str:
        return " mod "


class Rem(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.MULTIPLYING_OPERATOR

    @property
    def symbol(self) -> str:
        return " rem "


class New(Expr):
    def __init__(self, expr: Expr) -> None:
        super().__init__()
        self.expr = expr

    def __str__(self) -> str:
        return f"new {self.expr}"

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def rflx_expr(self) -> expr.Expr:
        raise NotImplementedError


class Name(Expr):
    def __init__(self) -> None:
        super().__init__()

    def __str__(self) -> str:
        return self._representation

    @property
    @abstractmethod
    def _representation(self) -> str:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


class Literal(Name):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)
        super().__init__()

    @property
    def _representation(self) -> str:
        return str(self.name)

    @property
    def name(self) -> str:
        return self.identifier.ada_str

    def rflx_expr(self) -> expr.Literal:
        return expr.Literal(self.identifier)


class Variable(Name):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)
        super().__init__()

    @property
    def _representation(self) -> str:
        return str(self.name)

    @property
    def name(self) -> str:
        return self.identifier.ada_str

    def rflx_expr(self) -> expr.Variable:
        return expr.Variable(self.identifier)


TRUE = Literal("True")
FALSE = Literal("False")
NULL = Literal("null")


class Attribute(Name):
    def __init__(self, prefix: StrID | Expr) -> None:
        if isinstance(prefix, ID):
            prefix = Variable(prefix)
        if isinstance(prefix, str):
            prefix = Variable(prefix)

        self.prefix: Expr = prefix
        super().__init__()

    @property
    def _representation(self) -> str:
        return f"{self.prefix}'{self.__class__.__name__}"

    def rflx_expr(self) -> expr.Attribute:
        result = getattr(expr, self.__class__.__name__)(self.prefix.rflx_expr())
        assert isinstance(result, expr.Attribute)
        return result


class Size(Attribute):
    pass


class Length(Attribute):
    pass


class First(Attribute):
    pass


class Last(Attribute):
    pass


class LoopEntry(Attribute):
    @property
    def _representation(self) -> str:
        return f"{self.prefix}'Loop_Entry"


class Range(Attribute):
    pass


class Old(Attribute):
    pass


class Result(Attribute):
    pass


class Constrained(Attribute):
    pass


class Valid(Attribute):
    pass


class Access(Attribute):
    pass


class Initialized(Attribute):
    pass


class Image(Attribute):
    pass


class Class(Attribute):
    pass


class UnrestrictedAccess(Attribute):
    @property
    def _representation(self) -> str:
        return f"{self.prefix}'Unrestricted_Access"


class AttributeExpr(Attribute):
    def __init__(
        self,
        prefix: StrID | Expr,
        expression: Expr,
    ) -> None:
        self.expression = expression
        super().__init__(prefix)

    @property
    def _representation(self) -> str:
        return f"{self.prefix}'{self.__class__.__name__} ({self.expression})"


class Val(AttributeExpr):
    pass


class Pos(AttributeExpr):
    pass


class Succ(AttributeExpr):
    pass


class BinAttributeExpr(Attribute):
    def __init__(self, prefix: StrID | Expr, left: Expr, right: Expr) -> None:
        self.left = left
        self.right = right
        super().__init__(prefix)

    @property
    def _representation(self) -> str:
        return f"{self.prefix}'{self.__class__.__name__} ({self.left}, {self.right})"


class Min(BinAttributeExpr):
    pass


class Max(BinAttributeExpr):
    pass


class NamedAttributeExpr(Attribute):
    def __init__(self, prefix: StrID | Expr, *associations: tuple[StrID, Expr]) -> None:
        self.associations = [(ID(n) if isinstance(n, str) else n, e) for n, e in associations]
        super().__init__(prefix)

    @property
    def _representation(self) -> str:
        assert len(self.associations) > 0
        associations = ", ".join(
            f"{name.ada_str} => {element}" for name, element in self.associations
        )
        return f"{self.prefix}'{self.__class__.__name__} ({associations})"


class Update(NamedAttributeExpr):
    pass


class Indexed(Name):
    def __init__(self, prefix: Expr, *elements: Expr) -> None:
        assert len(elements) > 0
        self.prefix = prefix
        self.elements = list(elements)
        super().__init__()

    @property
    def _representation(self) -> str:
        return f"{self.prefix} (" + ", ".join(map(str, self.elements)) + ")"

    def rflx_expr(self) -> expr.Indexed:
        return expr.Indexed(
            self.prefix.rflx_expr(),
            *[e.rflx_expr() for e in self.elements],
        )


class Selected(Name):
    def __init__(
        self,
        prefix: Expr,
        selector: StrID,
    ) -> None:
        self.prefix = prefix
        self.selector = ID(selector)
        super().__init__()

    @property
    def _representation(self) -> str:
        return f"{self.prefix}.{self.selector}"

    def rflx_expr(self) -> expr.Selected:
        return expr.Selected(self.prefix.rflx_expr(), self.selector)


class Call(Name):
    def __init__(
        self,
        identifier: StrID,
        arguments: Sequence[Expr] | None = None,
        named_arguments: Mapping[ID, Expr] | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.arguments = arguments or []
        self.named_arguments = named_arguments or {}
        super().__init__()

    @property
    def _representation(self) -> str:
        arguments = ", ".join(
            [
                *(str(a) for a in self.arguments),
                *(f"{n} => {a}" for n, a in self.named_arguments.items()),
            ],
        )
        if arguments:
            arguments = f" ({arguments})"
        return f"{self.identifier.ada_str}{arguments}"

    def rflx_expr(self) -> expr.Call:
        assert not self.named_arguments
        return expr.Call(self.identifier, ty.UNDEFINED, [a.rflx_expr() for a in self.arguments])


class Slice(Name):
    def __init__(self, prefix: Expr, first: Expr, last: Expr) -> None:
        self.prefix = prefix
        self.first = first
        self.last = last
        super().__init__()

    @property
    def _representation(self) -> str:
        return f"{self.prefix} ({self.first} .. {self.last})"

    def rflx_expr(self) -> expr.Slice:
        return expr.Slice(self.prefix.rflx_expr(), self.first.rflx_expr(), self.last.rflx_expr())


class Aggregate(Expr):
    def __init__(self, *elements: Expr) -> None:
        super().__init__()
        self.elements = list(elements)

    def __str__(self) -> str:
        # TODO(eng/recordflux/RecordFlux#1767): Support aggregates with single elements
        assert len(self.elements) > 1
        return "(" + ", ".join(map(str, self.elements)) + ")"

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.Aggregate:
        return expr.Aggregate(*[e.rflx_expr() for e in self.elements])


class String(Aggregate):
    def __init__(self, data: str) -> None:
        quotes = 0
        for d in data:
            if d == '"':
                quotes += 1
            else:
                if quotes % 2 != 0:
                    raise ValueError(f"unescaped quotation mark in string: {data}")
                quotes = 0
        self.data = data
        super().__init__(*[Number(ord(d)) for d in data])

    def __str__(self) -> str:
        return f'"{self.data}"'

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.String:
        return expr.String(self.data)

    @staticmethod
    def escaped(data: str) -> String:
        return String("".join(d if d != '"' else '""' for d in data))


class NamedAggregate(Expr):
    def __init__(
        self,
        *elements: tuple[StrID | Expr, Expr],
        implicit_elements: list[tuple[StrID | Expr, Expr]] | None = None,
    ) -> None:
        """
        Array aggregate with named associations.

        `implicit_elements` must contain a semantically equivalent representation of `elements`
        that does not use the box notation `<>`.
        """
        super().__init__()
        self.elements = [(ID(n) if isinstance(n, str) else n, e) for n, e in elements]
        self.implicit_elements = [
            (ID(n) if isinstance(n, str) else n, e) for n, e in implicit_elements or []
        ]

    def __str__(self) -> str:
        assert len(self.elements) > 0
        return (
            "("
            + ", ".join(
                f"{name.ada_str if isinstance(name, ID) else name} => {element}"
                for name, element in self.elements
            )
            + ")"
        )

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.NamedAggregate:
        elements: list[tuple[ID | expr.Expr, expr.Expr]] = [
            (
                n if isinstance(n, ID) else n.rflx_expr(),
                e.rflx_expr(),
            )
            for n, e in self.elements
        ]
        return expr.NamedAggregate(*elements)


class Relation(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.RELATIONAL_OPERATOR

    def rflx_expr(self) -> expr.Relation:
        result = getattr(expr, self.__class__.__name__)(
            self.left.rflx_expr(),
            self.right.rflx_expr(),
        )
        assert isinstance(result, expr.Relation)
        return result


class Less(Relation):
    @property
    def symbol(self) -> str:
        return " < "


class LessEqual(Relation):
    @property
    def symbol(self) -> str:
        return " <= "


class Equal(Relation):
    @property
    def symbol(self) -> str:
        return " = "


class GreaterEqual(Relation):
    @property
    def symbol(self) -> str:
        return " >= "


class Greater(Relation):
    @property
    def symbol(self) -> str:
        return " > "


class NotEqual(Relation):
    @property
    def symbol(self) -> str:
        return " /= "


class In(Relation):
    @property
    def symbol(self) -> str:
        return " in "


class NotIn(Relation):
    @property
    def symbol(self) -> str:
        return " not in "


def If(  # noqa: N802
    condition_expressions: Sequence[tuple[Expr, Expr]],
    else_expression: Expr | None = None,
) -> Expr:
    if len(condition_expressions) == 0 and else_expression is not None:
        return else_expression
    if len(condition_expressions) == 1 and condition_expressions[0][0] == TRUE:
        return condition_expressions[0][1]
    return IfExpr(condition_expressions, else_expression)


def IfThenElse(  # noqa: N802
    condition: Expr,
    then_expr: Expr,
    else_expr: Expr | None = None,
) -> Expr:
    return If([(condition, then_expr)], else_expr)


class IfExpr(Expr):
    def __init__(
        self,
        condition_expressions: Sequence[tuple[Expr, Expr]],
        else_expression: Expr | None = None,
    ) -> None:
        super().__init__()
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

    def __str__(self) -> str:
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

        return f"({expression})"

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.IfExpr:
        return expr.IfExpr(
            [(c.rflx_expr(), e.rflx_expr()) for c, e in self.condition_expressions],
            self.else_expression.rflx_expr() if self.else_expression else None,
        )

    @property
    def parenthesized_if_needed(self) -> str:
        return str(self)


def Case(  # noqa: N802
    control_expression: Expr,
    case_expressions: Sequence[tuple[Expr, Expr]],
) -> Expr:
    if len(case_expressions) == 1 and case_expressions[0][0] == Variable("others"):
        return case_expressions[0][1]
    return CaseExpr(control_expression, case_expressions)


class CaseExpr(Expr):
    def __init__(
        self,
        control_expression: Expr,
        case_expressions: Sequence[tuple[Expr, Expr]],
    ) -> None:
        super().__init__()
        self.control_expression = control_expression
        self.case_expressions = case_expressions

    def __str__(self) -> str:
        grouped_cases = [
            (" | ".join(str(c) for c, _ in choices), expression)
            for expression, choices in itertools.groupby(self.case_expressions, lambda x: x[1])
        ]
        cases = indent(
            ",".join(
                [
                    f"\nwhen {choice} =>\n{indent(str(expression), 3)}"
                    for choice, expression in grouped_cases
                ],
            ),
            4,
        )
        return f"(case {self.control_expression} is{cases})"

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.Expr:
        raise NotImplementedError

    @property
    def parenthesized_if_needed(self) -> str:
        return str(self)


class QuantifiedExpr(Expr):
    def __init__(self, parameter_identifier: StrID, iterable: Expr, predicate: Expr) -> None:
        super().__init__()
        self.parameter_identifier = ID(parameter_identifier)
        self.iterable = iterable
        self.predicate = predicate

    def __str__(self) -> str:
        return (
            f"(for {self.quantifier} {self.parameter_identifier.ada_str} {self.keyword}"
            f" {self.iterable} =>\n{indent(str(self.predicate), 4)})"
        )

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

    def rflx_expr(self) -> expr.QuantifiedExpr:
        result = getattr(expr, self.__class__.__name__)(
            self.parameter_identifier,
            self.iterable.rflx_expr(),
            self.predicate.rflx_expr(),
        )
        assert isinstance(result, expr.QuantifiedExpr)
        return result


class ForAllOf(QuantifiedExpr):
    @property
    def quantifier(self) -> str:
        return "all"

    @property
    def keyword(self) -> str:
        return "of"


class ForAllIn(QuantifiedExpr):
    @property
    def quantifier(self) -> str:
        return "all"

    @property
    def keyword(self) -> str:
        return "in"


class ForSomeIn(QuantifiedExpr):
    @property
    def quantifier(self) -> str:
        return "some"

    @property
    def keyword(self) -> str:
        return "in"


class ValueRange(Expr):
    def __init__(self, lower: Expr, upper: Expr, type_identifier: StrID | None = None):
        super().__init__()
        self.lower = lower
        self.upper = upper
        self.type_identifier = ID(type_identifier) if type_identifier else None

    def __str__(self) -> str:
        if self.type_identifier is None:
            return f"{self.lower} .. {self.upper}"
        return f"{self.type_identifier.ada_str} range {self.lower} .. {self.upper}"

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.ValueRange:
        return expr.ValueRange(self.lower.rflx_expr(), self.upper.rflx_expr())


class Conversion(Expr):
    def __init__(self, identifier: StrID, argument: Expr) -> None:
        super().__init__()
        self.identifier = ID(identifier)
        self.argument = argument

    def __str__(self) -> str:
        return f"{self.identifier.ada_str} ({self.argument})"

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.Conversion:
        return expr.Conversion(self.identifier, self.argument.rflx_expr())


class QualifiedExpr(Expr):
    def __init__(self, type_identifier: StrID, expression: Expr) -> None:
        super().__init__()
        self.type_identifier = ID(type_identifier)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.type_identifier.ada_str}'{self.expression.parenthesized_if_needed}"

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.Expr:
        return expr.QualifiedExpr(ID(self.type_identifier), self.expression.rflx_expr())


class Raise(Expr):
    def __init__(self, identifier: StrID, string: Expr | None = None) -> None:
        super().__init__()
        self.identifier = ID(identifier)
        self.string = string

    def __str__(self) -> str:
        string = f" with {self.string}" if self.string else ""
        return f"raise {self.identifier.ada_str}{string}"

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def rflx_expr(self) -> expr.Expr:
        raise NotImplementedError


class ChoiceList(Expr):
    def __init__(self, *expressions: Expr) -> None:
        super().__init__()
        self.expressions = list(expressions)

    def __str__(self) -> str:
        return " | ".join([str(e) for e in self.expressions])

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL

    def rflx_expr(self) -> expr.Expr:
        raise NotImplementedError


class Declaration(Base):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ContextItem(Base):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)

    def __hash__(self) -> int:
        return hash(self.identifier)

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class WithClause(ContextItem):
    def __str__(self) -> str:
        return f"with {self.identifier.ada_str};"


class UsePackageClause(ContextItem, Declaration):
    def __str__(self) -> str:
        return f"use {self.identifier.ada_str};"


class UseTypeClause(ContextItem, Declaration):
    def __init__(self, *identifiers: StrID) -> None:
        self.identifiers = [ID(i) for i in identifiers]

    def __hash__(self) -> int:
        return hash(self._identifiers_str)

    def __str__(self) -> str:
        return f"use type {self._identifiers_str};"

    @property
    def _identifiers_str(self) -> str:
        return ", ".join(identifier.ada_str for identifier in self.identifiers)


class Aspect(Base):
    def __str__(self) -> str:
        if self.definition:
            return f"{self.mark} =>\n{indent(self.definition, 2)}"
        return f"{self.mark}"

    @property
    @abstractmethod
    def mark(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def definition(self) -> str:
        raise NotImplementedError


class Precondition(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Pre"

    @property
    def definition(self) -> str:
        return str(self.expression)


class Postcondition(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Post"

    @property
    def definition(self) -> str:
        return str(self.expression)


class ClassPrecondition(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Pre'Class"

    @property
    def definition(self) -> str:
        return str(self.expression)


class ClassPostcondition(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Post'Class"

    @property
    def definition(self) -> str:
        return str(self.expression)


class ContractCases(Aspect):
    def __init__(self, *cases: tuple[Expr, Expr]) -> None:
        self.cases = cases

    @property
    def mark(self) -> str:
        return "Contract_Cases"

    @property
    def definition(self) -> str:
        cases = indent_next(",\n".join(f"{p} =>\n{indent(str(q), 3)}" for p, q in self.cases), 1)
        return f"({cases})"


class Depends(Aspect):
    def __init__(self, dependencies: Mapping[StrID, Sequence[StrID]]) -> None:
        self.dependencies = {ID(k): v for k, v in dependencies.items()}

    @property
    def mark(self) -> str:
        return "Depends"

    @property
    def definition(self) -> str:
        def input_values(values: Sequence[StrID]) -> str:
            if len(values) == 0:
                return "null"
            if len(values) == 1:
                return str(values[0])
            return "(" + ", ".join(str(p) for p in values) + ")"

        dependencies = indent_next(
            ", ".join(f"{o.ada_str} => {input_values(i)}" for o, i in self.dependencies.items()),
            1,
        )
        return f"({dependencies})"


class AlwaysTerminates(Aspect):
    def __init__(self, expression: Expr | None = None) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Always_Terminates"

    @property
    def definition(self) -> str:
        return "" if self.expression is None else str(self.expression)


class ChangeDirection(Base):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.direction} =>\n{indent(str(self.expression), 2)}"

    @property
    @abstractmethod
    def direction(self) -> str:
        raise NotImplementedError


class Increases(ChangeDirection):
    @property
    def direction(self) -> str:
        return "Increases"


class Decreases(ChangeDirection):
    @property
    def direction(self) -> str:
        return "Decreases"


class SubprogramVariant(Aspect):
    def __init__(self, direction: ChangeDirection) -> None:
        self.direction = direction

    @property
    def mark(self) -> str:
        return "Subprogram_Variant"

    @property
    def definition(self) -> str:
        return f"({self.direction})"


class DynamicPredicate(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Dynamic_Predicate"

    @property
    def definition(self) -> str:
        return str(self.expression)


class SizeAspect(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Size"

    @property
    def definition(self) -> str:
        return str(self.expression)


class InitialCondition(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Initial_Condition"

    @property
    def definition(self) -> str:
        return str(self.expression)


class DefaultInitialCondition(Aspect):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    @property
    def mark(self) -> str:
        return "Default_Initial_Condition"

    @property
    def definition(self) -> str:
        return str(self.expression)


class SparkMode(Aspect):
    def __init__(self, off: bool | None = None) -> None:
        self.off = off

    @property
    def mark(self) -> str:
        return "SPARK_Mode"

    @property
    def definition(self) -> str:
        return "" if self.off is None else ("Off" if self.off else "On")


class Ghost(Aspect):
    @property
    def mark(self) -> str:
        return "Ghost"

    @property
    def definition(self) -> str:
        return ""


class AbstractState(Aspect):
    def __init__(self, *identifiers: StrID) -> None:
        assert len(identifiers) > 0
        self.identifiers = [ID(i) for i in identifiers]

    @property
    def mark(self) -> str:
        return "Abstract_State"

    @property
    def definition(self) -> str:
        args = ", ".join(str(i) for i in self.identifiers)
        return f"({args})" if len(self.identifiers) > 1 else args


class Initializes(Aspect):
    def __init__(self, *identifiers: StrID) -> None:
        assert len(identifiers) > 0
        self.identifiers = [ID(i) for i in identifiers]

    @property
    def mark(self) -> str:
        return "Initializes"

    @property
    def definition(self) -> str:
        args = ", ".join(str(i) for i in self.identifiers)
        return f"({args})" if len(self.identifiers) > 1 else args


class Global(Aspect):
    def __init__(
        self,
        inputs: Sequence[ID] | None = None,
        outputs: Sequence[ID] | None = None,
        in_outs: Sequence[ID] | None = None,
    ) -> None:
        self.inputs = inputs or []
        self.outputs = outputs or []
        self.in_outs = in_outs or []

    @property
    def mark(self) -> str:
        return "Global"

    @property
    def definition(self) -> str:
        inputs = (
            f"Input => {aggregate_or_single_element([i.ada_str for i in self.inputs])}"
            if self.inputs
            else ""
        )
        outputs = (
            f"Output => {aggregate_or_single_element([i.ada_str for i in self.outputs])}"
            if self.outputs
            else ""
        )
        in_outs = (
            f"In_Out => {aggregate_or_single_element([i.ada_str for i in self.in_outs])}"
            if self.in_outs
            else ""
        )
        parts = [p for p in [inputs, outputs, in_outs] if p]
        return aggregate(parts) if parts else "null"


class Import(Aspect):
    @property
    def mark(self) -> str:
        return "Import"

    @property
    def definition(self) -> str:
        return ""


class InlineAlways(Aspect):
    @property
    def mark(self) -> str:
        return "Inline_Always"

    @property
    def definition(self) -> str:
        return ""


class Annotate(Aspect):
    def __init__(self, *args: str) -> None:
        assert len(args) > 0
        self.args = args

    @property
    def mark(self) -> str:
        return "Annotate"

    @property
    def definition(self) -> str:
        args = ", ".join(self.args)
        return f"({args})"


class ElaborateBody(Aspect):
    @property
    def mark(self) -> str:
        return "Elaborate_Body"

    @property
    def definition(self) -> str:
        return ""


class ConventionKind(Enum):
    Intrinsic = 0

    def __str__(self) -> str:
        return self.name


class Convention(Aspect):
    def __init__(self, convention: ConventionKind) -> None:
        self.convention = convention

    @property
    def mark(self) -> str:
        return "Convention"

    @property
    def definition(self) -> str:
        return str(self.convention)


class FormalDeclaration(Base):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class FormalSubprogramDeclaration(FormalDeclaration):
    def __init__(
        self,
        specification: SubprogramSpecification,
        default: StrID | None = None,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        self.specification = specification
        self.default = ID(default) if default else None
        self.aspects = aspects

    def __hash__(self) -> int:
        return hash(self.specification)

    def __str__(self) -> str:
        default = f" is {self.default}" if self.default else ""
        return f"with {self.specification}{default}{aspect_specification(self.aspects)};"


class FormalPackageDeclaration(FormalDeclaration):
    def __init__(
        self,
        identifier: StrID,
        generic_identifier: StrID,
        associations: Sequence[tuple[StrID | None, StrID | None]] | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.generic_identifier = ID(generic_identifier)
        self.associations = associations

    def __str__(self) -> str:
        associations = (
            ", ".join(
                f"{name_str}{value_str}"
                for name_str, value_str in (
                    (
                        f"{ID(name).ada_str} => " if name else "",
                        ID(value).ada_str if value else "<>",
                    )
                    for name, value in self.associations
                )
            )
            if self.associations
            else "<>"
        )

        return (
            f"with package {self.identifier.ada_str} is new {self.generic_identifier.ada_str}"
            f" ({associations});"
        )


class PackageDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        declarations: Sequence[Declaration] | None = None,
        private_declarations: Sequence[Declaration] | None = None,
        formal_parameters: Sequence[FormalDeclaration] | None = None,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.declarations = declarations or []
        self.private_declarations = private_declarations or []
        self.formal_parameters = formal_parameters
        self.aspects = aspects

    def __str__(self) -> str:
        return (
            f"{generic_formal_part(self.formal_parameters)}"
            f"package {self.identifier.ada_str}"
            f"{aspect_specification(self.aspects, with_separator=True)}is\n\n"
            f"{declarative_items(self.declarations)}"
            f"{declarative_items(self.private_declarations, private=True)}"
            f"end {self.identifier.ada_str};\n"
        )


class PackageBody(Declaration):
    def __init__(
        self,
        identifier: StrID,
        declarations: Sequence[Declaration] | None = None,
        statements: Sequence[Statement] | None = None,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.declarations = declarations or []
        self.statements = statements or []
        self.aspects = aspects

    def __str__(self) -> str:
        if not self.declarations and not self.statements:
            return ""

        statements = (
            ("begin\n\n" + indent("\n".join(map(str, self.statements)), 3) + "\n\n")
            if self.statements
            else ""
        )

        return (
            f"package body {self.identifier.ada_str}"
            f"{aspect_specification(self.aspects, with_separator=True)}is\n\n"
            f"{declarative_items(self.declarations)}{statements}end {self.identifier.ada_str};\n"
        )


class GenericPackageInstantiation(PackageDeclaration):
    def __init__(
        self,
        identifier: StrID,
        generic_package: StrID,
        associations: Sequence[tuple[StrID | None, StrID | Expr]] | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.generic_package = ID(generic_package)
        self.associations: list[tuple[ID | None, Expr]] = [
            (ID(i) if i else None, Variable(e) if isinstance(e, (str, ID)) else e)
            for i, e in associations or []
        ]

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __str__(self) -> str:
        associations = ", ".join(
            [f"{n} => {i}" if n else str(i) for n, i in self.associations],
        )
        if associations:
            associations = f" ({associations})"
        return (
            f"package {self.identifier.ada_str} is new {self.generic_package.ada_str}"
            f"{associations};\n"
        )


class PackageRenamingDeclaration(Declaration):
    def __init__(self, identifier: StrID, package_identifier: StrID) -> None:
        self.identifier = ID(identifier)
        self.package_identifier = ID(package_identifier)

    def __str__(self) -> str:
        return f"package {self.identifier.ada_str} renames {self.package_identifier.ada_str};"


class ObjectDeclaration(Declaration):
    def __init__(  # noqa: PLR0913
        self,
        identifiers: Sequence[StrID],
        type_identifier: StrID | Expr,
        expression: Expr | None = None,
        constant: bool = False,
        aliased: bool = False,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_identifier = (
            type_identifier if isinstance(type_identifier, Expr) else Variable(type_identifier)
        )
        self.expression = expression
        self.constant = constant
        self.aliased = aliased
        self.aspects = aspects

    def __hash__(self) -> int:
        return hash(tuple(self.identifiers))

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        constant = "constant " if self.constant else ""
        aliased = "aliased " if self.aliased else ""
        expression = f" := {self.expression}" if self.expression else ""
        return (
            f"{identifiers} : {constant}{aliased}{self.type_identifier}{expression}"
            f"{aspect_specification(self.aspects)};"
        )


class Discriminant(Base):
    def __init__(
        self,
        identifiers: Sequence[StrID],
        type_identifier: StrID,
        default: Expr | None = None,
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_identifier = ID(type_identifier)
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        default = f" := {self.default}" if self.default else ""
        return f"{identifiers} : {self.type_identifier.ada_str}{default}"


class TypeDeclaration(Declaration, FormalDeclaration):
    def __init__(
        self,
        identifier: StrID,
        discriminants: Sequence[Discriminant] | None = None,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.discriminants = discriminants
        self.aspects = aspects

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __str__(self) -> str:
        return (
            f"type {self.identifier.ada_str}{self.discriminant_part} is{self.type_definition}"
            f"{aspect_specification(self.aspects)};{self.extra_declaration}"
        )

    @property
    def discriminant_part(self) -> str:
        return " (" + "; ".join(map(str, self.discriminants)) + ")" if self.discriminants else ""

    @property
    @abstractmethod
    def type_definition(self) -> str:
        raise NotImplementedError

    @property
    def extra_declaration(self) -> str:
        return ""


class ModularType(TypeDeclaration):
    def __init__(
        self,
        identifier: StrID,
        modulus: Expr,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(identifier, aspects=aspects or [])
        self.modulus = modulus

    @property
    def type_definition(self) -> str:
        return f" mod {self.modulus}"


class SignedIntegerType(TypeDeclaration):
    def __init__(
        self,
        identifier: StrID,
        first: Expr,
        last: Expr,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(identifier, aspects=aspects or [])
        self.first = first
        self.last = last

    @property
    def type_definition(self) -> str:
        return f" range {self.first} .. {self.last}"


class EnumerationType(TypeDeclaration):
    def __init__(
        self,
        identifier: StrID,
        literals: Mapping[ID, Number | None],
        size: Expr | None = None,
    ) -> None:
        super().__init__(identifier, aspects=([SizeAspect(size)] if size else []))
        self.literals = (
            OrderedDict(
                sorted(
                    literals.items(),
                    key=lambda t: t[1].value if isinstance(t[1], Number) else 0,
                ),
            )
            if None not in literals.values()
            else literals
        )
        self.size = size

    @property
    def type_definition(self) -> str:
        literal_specification = ", ".join(map(str, self.literals.keys()))
        return f" ({literal_specification})"

    @property
    def extra_declaration(self) -> str:
        literal_representation = ", ".join(
            f"{k} => {v}" for k, v in self.literals.items() if v is not None
        )
        return (
            f"\nfor {self.identifier.ada_str} use ({literal_representation});"
            if literal_representation
            else ""
        )


class Subtype(TypeDeclaration):
    def __init__(
        self,
        identifier: StrID,
        base_identifier: StrID,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(identifier, aspects=aspects)
        self.base_identifier = ID(base_identifier)

    def __str__(self) -> str:
        return "sub" + super().__str__()

    @property
    def type_definition(self) -> str:
        return f" {self.base_identifier.ada_str}"


class RangeSubtype(Subtype):
    def __init__(self, identifier: StrID, base_identifier: StrID, first: Expr, last: Expr) -> None:
        super().__init__(identifier, base_identifier)
        self.first = first
        self.last = last

    @property
    def type_definition(self) -> str:
        return f" {self.base_identifier.ada_str} range {self.first} .. {self.last}"


class DerivedType(TypeDeclaration):
    def __init__(self, identifier: StrID, type_identifier: StrID) -> None:
        super().__init__(identifier)
        self.type_identifier = ID(type_identifier)

    @property
    @abstractmethod
    def constraint(self) -> str:
        raise NotImplementedError

    @property
    def type_definition(self) -> str:
        return f" new {self.type_identifier.ada_str}{self.constraint}"


class PlainDerivedType(DerivedType):
    @property
    def constraint(self) -> str:
        return ""


class DerivedRangeType(DerivedType):
    def __init__(self, identifier: StrID, type_identifier: StrID, first: Expr, last: Expr) -> None:
        super().__init__(identifier, type_identifier)
        self.first = first
        self.last = last

    @property
    def constraint(self) -> str:
        return f" range {self.first} .. {self.last}"


class DerivedRecordType(DerivedType):
    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID,
        record_extension: Sequence[Component],
    ) -> None:
        super().__init__(identifier, type_identifier)
        self.record_extension = record_extension

    @property
    def constraint(self) -> str:
        extension = ""

        if len(self.record_extension) == 0:
            extension = " with null record"
        else:
            components = (
                (indent("\n".join(map(str, self.record_extension)), 6) + "\n")
                if self.record_extension
                else ""
            )
            extension = f" with\n   record\n{components}   end record"

        return f"{extension}"


class FormalTypeDeclaration(TypeDeclaration):
    pass


class PrivateType(FormalTypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " private"


class DiscreteType(FormalTypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " (<>)"


class FormalSignedIntegerType(FormalTypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " range <>"


class ArrayType(TypeDeclaration):
    def __init__(self, identifier: StrID, index_type: StrID, component_identifier: StrID) -> None:
        super().__init__(identifier)
        self.index_type = ID(index_type)
        self.component_identifier = ID(component_identifier)

    @property
    def type_definition(self) -> str:
        return f" array ({self.index_type.ada_str}) of {self.component_identifier.ada_str}"


class UnconstrainedArrayType(ArrayType):
    @property
    def type_definition(self) -> str:
        return f" array ({self.index_type.ada_str} range <>) of {self.component_identifier.ada_str}"


class AccessType(TypeDeclaration):
    def __init__(self, identifier: StrID, object_identifier: StrID) -> None:
        super().__init__(identifier)
        self.object_identifier = ID(object_identifier)

    @property
    def type_definition(self) -> str:
        return f" access {self.object_identifier.ada_str}"


class Component(Base):
    def __init__(
        self,
        identifier: StrID,
        type_identifier: StrID | Expr,
        default: Expr | None = None,
        aliased: bool = False,
    ) -> None:
        self.identifier = ID(identifier)
        self.type_identifier = (
            type_identifier if isinstance(type_identifier, Expr) else Variable(type_identifier)
        )
        self.default = default
        self.aliased = aliased

    def __str__(self) -> str:
        default = f" := {self.default}" if self.default else ""
        aliased = "aliased " if self.aliased else ""
        return f"{self.identifier.ada_str} : {aliased}{self.type_identifier}{default};"


class NullComponent(Component):
    def __init__(self) -> None:
        super().__init__("null", "null")

    def __str__(self) -> str:
        return "null;"


class Variant(Base):
    def __init__(self, discrete_choices: Sequence[Expr], components: Sequence[Component]) -> None:
        self.discrete_choices = discrete_choices
        self.components = components

    def __str__(self) -> str:
        choices = " | ".join(map(str, self.discrete_choices))
        components = indent("\n".join(map(str, self.components)), 6)
        return f"   when {choices} =>\n{components}"


class VariantPart(Base):
    def __init__(self, discriminant_identifier: StrID, variants: Sequence[Variant]) -> None:
        self.discriminant_identifier = ID(discriminant_identifier)
        self.variants = variants

    def __str__(self) -> str:
        variants = "\n".join(map(str, self.variants))
        return f"case {self.discriminant_identifier.ada_str} is\n{variants}\nend case;\n"


class RecordType(TypeDeclaration):
    def __init__(  # noqa: PLR0913
        self,
        identifier: StrID,
        components: Sequence[Component],
        discriminants: Sequence[Discriminant] | None = None,
        variant_part: VariantPart | None = None,
        aspects: Sequence[Aspect] | None = None,
        abstract: bool = False,
        tagged: bool = False,
        limited: bool = False,
    ) -> None:
        assert tagged if abstract else True
        super().__init__(identifier, discriminants, aspects)
        self.components = components
        self.discriminants = discriminants or []
        self.variant_part = variant_part
        self.abstract = abstract
        self.tagged = tagged
        self.limited = limited

    @property
    def type_definition(self) -> str:
        abstract = " abstract" if self.abstract else ""
        tagged = " tagged" if self.tagged else ""
        limited = " limited" if self.limited else ""

        if self.components or self.variant_part:
            components = (
                (indent("\n".join(map(str, self.components)), 6) + "\n") if self.components else ""
            )
            variant_part = indent(str(self.variant_part), 6) if self.variant_part else ""
            definition = f"\n   record\n{components}{variant_part}   end record"
        else:
            definition = " null record"

        return f"{abstract}{tagged}{limited}{definition}"


class Statement(Base):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class NullStatement(Statement):
    def __init__(self) -> None:
        pass

    def __str__(self) -> str:
        return "null;"


class Assignment(Statement):
    def __init__(self, name: StrID | Expr, expression: Expr) -> None:
        self.name = name if isinstance(name, Expr) else Variable(name)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.name} := {self.expression};"


class CallStatement(Statement):
    def __init__(
        self,
        identifier: StrID,
        arguments: Sequence[Expr] | None = None,
        named_arguments: Mapping[ID, Expr] | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.arguments = arguments or []
        self.named_arguments = named_arguments or {}

    def __str__(self) -> str:
        arguments = ", ".join(
            [
                *(str(a) for a in self.arguments),
                *(f"{n} => {a}" for n, a in self.named_arguments.items()),
            ],
        )
        arguments = f" ({arguments})" if arguments else ""
        return f"{self.identifier.ada_str}{arguments};"


class PragmaStatement(Statement):
    def __init__(self, identifier: StrID, parameters: Sequence[Expr]) -> None:
        self.identifier = ID(identifier)
        self.pragma_parameters = parameters

    def __str__(self) -> str:
        parameters = ""
        if self.pragma_parameters:
            parameters = ", ".join(map(str, self.pragma_parameters))
            parameters = indent_next(str(parameters), len(str(self.identifier.ada_str)) + 9)
            if (
                self.identifier.ada_str == "Assert"
                and len(self.pragma_parameters) == 1
                and isinstance(self.pragma_parameters[0], (IfExpr, CaseExpr))
            ):
                parameters = f" {parameters}"
            else:
                parameters = f" ({parameters})"
        return f"pragma {self.identifier.ada_str}{parameters};"


class ReturnStatement(Statement):
    def __init__(self, expression: Expr | None = None) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if not self.expression:
            return "return;"
        if isinstance(self.expression, CaseExpr):
            return "return (" + indent_next(str(self.expression), 8) + ");"
        return "return " + indent_next(str(self.expression), 7) + ";"


class ExitStatement(Statement):
    def __init__(self, expression: Expr | None = None) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if not self.expression:
            return "exit;"
        if isinstance(self.expression, CaseExpr):
            return "exit when (" + indent_next(str(self.expression), 8) + ");"
        return "exit when " + indent_next(str(self.expression), 7) + ";"


class GotoStatement(Statement):
    def __init__(self, label: StrID) -> None:
        self.label = ID(label)

    def __str__(self) -> str:
        return f"goto {self.label};"


class Label(Statement):
    def __init__(self, identifier: StrID) -> None:
        self.identifier = ID(identifier)

    def __str__(self) -> str:
        return f"<<{self.identifier.ada_str}>>"


class Comment(ContextItem, Statement):
    def __init__(self, comment: str) -> None:
        assert "\n" not in comment
        self.comment = comment

    def __str__(self) -> str:
        return f"--{self.comment}"


class VerticalSpace(ContextItem, Statement):
    def __init__(self) -> None:
        pass

    def __str__(self) -> str:
        return ""


class IfStatement(Statement):
    def __init__(
        self,
        condition_statements: Sequence[tuple[Expr, Sequence[Statement]]],
        else_statements: Sequence[Statement] | None = None,
    ) -> None:
        assert condition_statements or else_statements
        self.condition_statements = condition_statements
        self.else_statements = else_statements

    def __str__(self) -> str:
        result = ""

        for condition, statements in self.condition_statements:
            c = (
                f" {condition} "
                if str(condition).count("\n") == 0
                else f"\n{indent(str(condition), 3)}\n"
            )
            if not result:
                result = f"if{c}then\n"
            else:
                result += f"elsif{c}then\n"
            for statement in statements:
                result += indent(f"{statement}\n", 3)

        if self.else_statements:
            else_statements = "\n".join(map(str, self.else_statements))

            if not self.condition_statements:
                return else_statements

            result += f"else\n{indent(else_statements, 3)}\n"

        result += "end if;"

        return result


class CaseStatement(Statement):
    def __init__(
        self,
        control_expression: Expr,
        case_statements: Sequence[tuple[Expr, Sequence[Statement]]],
        case_grouping: bool = True,
    ) -> None:
        self.control_expression = control_expression
        self.case_statements = case_statements
        self.case_grouping = case_grouping

    def __str__(self) -> str:
        if len(self.case_statements) == 1 and self.case_statements[0][0] == Variable("others"):
            return "\n".join(str(s) for s in self.case_statements[0][1])

        grouped_cases = (
            [
                (" | ".join(str(c) for c, _ in choices), statements)
                for statements, choices in itertools.groupby(self.case_statements, lambda x: x[1])
            ]
            if self.case_grouping
            else [(str(case), statements) for case, statements in self.case_statements]
        )
        cases = "".join(
            [
                f"\nwhen {choice} =>\n{indent(os.linesep.join(str(s) for s in statements), 3)}"
                for choice, statements in grouped_cases
            ],
        )

        return f"case {self.control_expression} is{indent(cases, 3)}\nend case;"


class While(Statement):
    def __init__(self, condition: Expr, statements: Sequence[Statement]) -> None:
        assert len(statements) > 0
        self.condition = condition
        self.statements = statements

    def __str__(self) -> str:
        condition = str(self.condition)
        statements = indent("\n".join(str(s) for s in self.statements), 3)
        if "\n" in condition or len(condition) > MAX_LINE_LENGTH:
            return f"while\n{indent(condition, 3)}\nloop\n{statements}\nend loop;"
        return f"while {self.condition} loop\n{statements}\nend loop;"


class ForLoop(Statement):
    def __init__(
        self,
        identifier: StrID,
        iterator: Expr,
        statements: Sequence[Statement],
        reverse: bool = False,
    ) -> None:
        assert len(statements) > 0
        self.identifier = ID(identifier)
        self.iterator = iterator
        self.statements = statements
        self.reverse = reverse

    def __str__(self) -> str:
        statements = indent("\n".join(str(s) for s in self.statements), 3)
        reverse = "reverse " if self.reverse else ""
        return (
            f"for {self.identifier.ada_str} {self.iterator_spec} "
            f"{reverse}{self.iterator} loop\n{statements}\nend loop;"
        )

    @property
    @abstractmethod
    def iterator_spec(self) -> str:
        raise NotImplementedError


class ForOf(ForLoop):
    @property
    def iterator_spec(self) -> str:
        return "of"


class ForIn(ForLoop):
    @property
    def iterator_spec(self) -> str:
        return "in"


class RaiseStatement(Statement):
    def __init__(self, identifier: StrID, string: Expr | None = None) -> None:
        super().__init__()
        self.identifier = ID(identifier)
        self.string = string

    def __str__(self) -> str:
        string = f" with {self.string}" if self.string else ""
        return f"raise {self.identifier.ada_str}{string};"


class Declare(Statement):
    def __init__(
        self,
        declarations: Sequence[Declaration],
        statements: Sequence[Statement],
    ) -> None:
        self.declarations = declarations
        self.statements = statements

    def __str__(self) -> str:
        declarations = indent("\n".join(str(s) for s in self.declarations), 3)
        statements = indent("\n".join(str(s) for s in self.statements), 3)
        return f"declare\n{declarations}\nbegin\n{statements}\nend;"


class Parameter(Base):
    def __init__(
        self,
        identifiers: Sequence[StrID],
        type_identifier: StrID,
        default: Expr | None = None,
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_identifier = ID(type_identifier)
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        default = f" := {self.default}" if self.default else ""
        return f"{identifiers} : {self.mode}{self.type_identifier.ada_str}{default}"

    @property
    def mode(self) -> str:
        return ""


class InParameter(Parameter):
    @property
    def mode(self) -> str:
        return "in "


class OutParameter(Parameter):
    @property
    def mode(self) -> str:
        return "out "


class InOutParameter(Parameter):
    @property
    def mode(self) -> str:
        return "in out "


class AccessParameter(Parameter):
    def __init__(
        self,
        identifiers: Sequence[StrID],
        type_identifier: StrID,
        default: Expr | None = None,
        constant: bool = False,
    ) -> None:
        super().__init__(identifiers, type_identifier, default)
        self.constant = constant

    @property
    def mode(self) -> str:
        return "access constant " if self.constant else "access "


class SubprogramSpecification(Base):
    def __init__(
        self,
        identifier: StrID,
        overriding: bool | None = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.overriding = overriding

    @property
    def _subprogram_kind(self) -> str:
        raise NotImplementedError

    @property
    def _specification(self) -> str:
        if self.overriding is None:
            overriding = ""
        else:
            overriding = f"{'' if self.overriding else 'not '}overriding "
        return f"{overriding}{self._subprogram_kind} {self.identifier.ada_str}"

    def __hash__(self) -> int:
        return hash(self.identifier)

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ParameterizedSubprogramSpecification(SubprogramSpecification):
    def __init__(
        self,
        identifier: StrID,
        parameters: Sequence[Parameter] | None = None,
        overriding: bool | None = None,
    ) -> None:
        super().__init__(identifier=identifier, overriding=overriding)
        self.parameters = parameters or []

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError

    @property
    def _parameterized_specification(self) -> str:
        parameters = (" (" + "; ".join(map(str, self.parameters)) + ")") if self.parameters else ""
        return f"{self._specification}{parameters}"


class ProcedureSpecification(ParameterizedSubprogramSpecification):
    def __str__(self) -> str:
        return self._parameterized_specification

    @property
    def _subprogram_kind(self) -> str:
        return "procedure"


class FunctionSpecification(ParameterizedSubprogramSpecification):
    def __init__(
        self,
        identifier: StrID,
        return_type: StrID,
        parameters: Sequence[Parameter] | None = None,
        overriding: bool | None = None,
        not_null: bool = False,
    ) -> None:
        super().__init__(identifier=identifier, parameters=parameters, overriding=overriding)
        self.return_type = ID(return_type)
        self.not_null = not_null

    def __str__(self) -> str:
        null_exclusion = "not null " if self.not_null else ""
        return (
            f"{self._parameterized_specification} return "
            f"{null_exclusion}{self.return_type.ada_str}"
        )

    @property
    def _subprogram_kind(self) -> str:
        return "function"


class SubprogramDeclaration(Declaration):
    def __init__(
        self,
        specification: ParameterizedSubprogramSpecification,
        aspects: Sequence[Aspect] | None = None,
        formal_parameters: Sequence[FormalDeclaration] | None = None,
    ) -> None:
        self.specification = specification
        self.formal_parameters = formal_parameters
        self.aspects = aspects

    def __str__(self) -> str:
        return (
            f"{generic_formal_part(self.formal_parameters)}"
            f"{self.specification}{self._declaration}{aspect_specification(self.aspects)};"
        )

    def __hash__(self) -> int:
        return hash(self.specification)

    @property
    def _declaration(self) -> str:
        return ""


class AbstractSubprogramDeclaration(SubprogramDeclaration):
    @property
    def _declaration(self) -> str:
        return " is abstract"


class SeparateSubprogramDeclaration(SubprogramDeclaration):
    @property
    def _declaration(self) -> str:
        return " is separate"


class SubprogramBody(SubprogramDeclaration):
    def __init__(
        self,
        specification: ParameterizedSubprogramSpecification,
        declarations: Sequence[Declaration],
        statements: Sequence[Statement],
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(specification=specification, aspects=aspects)
        self.declarations = declarations or []
        self.statements = statements or []

    def _declarations(self) -> str:
        return "".join(indent(f"{declaration}\n", 3) for declaration in self.declarations)

    def _statements(self) -> str:
        return "\n".join(indent(str(s), 3) for s in self.statements)

    def __str__(self) -> str:
        return (
            f"{self.specification}{aspect_specification(self.aspects, with_separator=True)}is\n"
            f"{self._declarations()}"
            f"begin\n"
            f"{self._statements()}\n"
            f"end {self.specification.identifier.ada_str};"
        )


class ExpressionFunctionDeclaration(SubprogramDeclaration):
    def __init__(
        self,
        specification: FunctionSpecification,
        expression: Expr,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(specification, aspects)
        self.expression = expression

    @property
    def _declaration(self) -> str:
        return f" is\n  {(indent_next(self.expression.parenthesized_if_needed, 3))}"


class GenericInstantiation(SubprogramSpecification, Declaration):
    def __init__(
        self,
        identifier: StrID,
        generic_name: StrID,
        associations: Sequence[tuple[StrID | None, Expr]] | None = None,
        overriding: bool | None = None,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(identifier=identifier, overriding=overriding)
        self.generic_name = ID(generic_name)
        self.associations = (
            [(ID(n) if n else None, e) for n, e in associations] if associations else []
        )
        self.aspects = aspects

    def __str__(self) -> str:
        associations = ", ".join(
            (f"{name.ada_str} => " if name else "") + str(element)
            for name, element in self.associations
        )
        if associations:
            associations = f" ({associations})"
        return (
            f"{self._specification} is new {self.generic_name.ada_str}{associations}"
            f"{aspect_specification(self.aspects)};"
        )


class GenericProcedureInstantiation(GenericInstantiation):
    def __init__(
        self,
        identifier: StrID,
        generic_name: StrID,
        associations: Sequence[tuple[StrID | None, Expr]] | None = None,
        overriding: bool | None = None,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(
            identifier=identifier,
            generic_name=generic_name,
            overriding=overriding,
            associations=associations,
            aspects=aspects,
        )

    @property
    def _subprogram_kind(self) -> str:
        return "procedure"


class GenericFunctionInstantiation(GenericInstantiation):
    def __init__(
        self,
        identifier: StrID,
        generic_name: StrID,
        associations: Sequence[tuple[StrID | None, Expr]] | None = None,
        overriding: bool | None = None,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(
            identifier=identifier,
            generic_name=generic_name,
            overriding=overriding,
            associations=associations,
            aspects=aspects,
        )

    @property
    def _subprogram_kind(self) -> str:
        return "function"


class SubprogramRenamingDeclaration(SubprogramDeclaration):
    def __init__(
        self,
        specification: ParameterizedSubprogramSpecification,
        subprogram_identifier: StrID,
        aspects: Sequence[Aspect] | None = None,
    ) -> None:
        super().__init__(specification=specification, aspects=aspects)
        self.subprogram_identifier = ID(subprogram_identifier)

    @property
    def _declaration(self) -> str:
        return f" renames {self.subprogram_identifier.ada_str}"


class Pragma(Declaration, ContextItem):
    def __init__(self, identifier: StrID, parameters: Sequence[Expr] | None = None) -> None:
        super().__init__(identifier)
        self.pragma_parameters = parameters or []

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return False
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __str__(self) -> str:
        parameters = ""
        if self.pragma_parameters:
            parameters = ", ".join(map(str, self.pragma_parameters))
            if (
                self.identifier.ada_str == "Assert"
                and len(self.pragma_parameters) == 1
                and isinstance(self.pragma_parameters[0], (IfExpr, CaseExpr))
            ):
                parameters = f" {parameters}"
            else:
                parameters = f" ({parameters})"
        return f"pragma {self.identifier.ada_str}{parameters};"


class Unit(Base):
    @abstractmethod
    def __iadd__(self, other: object) -> Unit:
        raise NotImplementedError

    @property
    @abstractmethod
    def ads(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def adb(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def with_header(self, license_header: Sequence[ContextItem]) -> Unit:
        raise NotImplementedError


class PackageUnit(Unit):
    def __init__(
        self,
        declaration_context: list[ContextItem],
        declaration: PackageDeclaration,
        body_context: list[ContextItem],
        body: PackageBody,
        formal_parameters: (
            list[
                FormalSubprogramDeclaration
                | TypeDeclaration
                | FormalTypeDeclaration
                | FormalPackageDeclaration
            ]
            | None
        ) = None,
    ) -> None:
        assert declaration.identifier == body.identifier
        self.declaration_context = declaration_context
        self.declaration = declaration
        self.body_context = body_context
        self.body = body
        self.formal_parameters = formal_parameters

    def __iadd__(self, other: object) -> Self:
        if isinstance(other, (UnitPart, SubprogramUnitPart)):
            self.declaration.declarations = [*self.declaration.declarations, *other.specification]
            self.declaration.private_declarations = [
                *self.declaration.private_declarations,
                *other.private,
            ]
            self.body.declarations = [*self.body.declarations, *other.body]
            self.body.statements = [*self.body.statements, *other.statements]
            return self
        return NotImplemented

    @property
    def ads(self) -> str:
        formal_part = ""
        if self.formal_parameters is not None:
            formal_part = "generic\n" + "\n".join(indent(str(p), 3) for p in self.formal_parameters)
            if self.formal_parameters:
                formal_part += "\n"
        return f"{context_clause(self.declaration_context)}{formal_part}{self.declaration}"

    @property
    def adb(self) -> str:
        return f"{context_clause(self.body_context)}{self.body}" if str(self.body) else ""

    @property
    def name(self) -> str:
        return file_name(str(self.declaration.identifier))

    def with_header(self, license_header: Sequence[ContextItem]) -> PackageUnit:
        declaration_context = [
            *license_header,
            *(
                [VerticalSpace(), *unique(self.declaration_context)]
                if self.declaration_context
                else []
            ),
        ]
        body_context = [
            *license_header,
            *([VerticalSpace(), *unique(self.body_context)] if self.body_context else []),
        ]
        return PackageUnit(
            declaration_context,
            self.declaration,
            body_context,
            self.body,
            self.formal_parameters,
        )


class InstantiationUnit(Unit):
    def __init__(
        self,
        context: Sequence[ContextItem],
        declaration: GenericPackageInstantiation,
    ) -> None:
        self.context = context
        self.declaration = declaration

    def __iadd__(self, other: object) -> Self:
        return NotImplemented

    @property
    def ads(self) -> str:
        return f"{context_clause(self.context)}{self.declaration}"

    @property
    def adb(self) -> str:
        return ""

    @property
    def name(self) -> str:
        return file_name(str(self.declaration.identifier))

    def with_header(self, license_header: Sequence[ContextItem]) -> InstantiationUnit:
        context = [
            *license_header,
            *([VerticalSpace(), *unique(self.context)] if self.context else []),
        ]
        return InstantiationUnit(context, self.declaration)


@dataclass
class UnitPart:
    specification: list[Declaration] = dataclass_field(default_factory=list)
    body: list[Declaration] = dataclass_field(default_factory=list)
    private: list[Declaration] = dataclass_field(default_factory=list)
    statements: list[Statement] = dataclass_field(default_factory=list)

    def __add__(self, other: object) -> UnitPart:
        if isinstance(other, UnitPart):
            return UnitPart(
                self.specification + other.specification,
                self.body + other.body,
                self.private + other.private,
                self.statements + other.statements,
            )
        return NotImplemented

    def __iadd__(self, other: object) -> Self:
        if isinstance(other, UnitPart):
            self.specification += other.specification
            self.body += other.body
            self.private += other.private
            self.statements += other.statements
            return self
        return NotImplemented


@dataclass
class SubprogramUnitPart:
    specification: list[SubprogramDeclaration] = dataclass_field(default_factory=list)
    body: list[SubprogramDeclaration] = dataclass_field(default_factory=list)
    private: list[SubprogramDeclaration] = dataclass_field(default_factory=list)
    statements: list[Statement] = dataclass_field(default_factory=list)


def generic_formal_part(parameters: Sequence[FormalDeclaration] | None = None) -> str:
    if parameters is None:
        return ""
    return (
        "generic" + ("".join(f"\n   {p}" for p in unique(parameters)) if parameters else "") + "\n"
    )


def declarative_items(declarations: Sequence[Declaration], private: bool = False) -> str:
    result = "\n\n".join(indent(str(d), 3) for d in unique(declarations) if str(d))
    if result:
        result = f"private\n\n{result}" if private else result
        result += "\n\n"
    return result


def aspect_specification(aspects: Sequence[Aspect] | None, with_separator: bool = False) -> str:
    if not aspects:
        return " " if with_separator else ""
    return (
        "\nwith\n"
        + ",\n".join(indent(str(aspect), 2) for aspect in aspects)
        + ("\n" if with_separator else "")
    )


def context_clause(context: Sequence[ContextItem]) -> str:
    return ("\n".join(map(str, context)) + "\n\n") if context else ""


def aggregate(elements: Sequence[str]) -> str:
    result = ", ".join(elements)
    return f"({result})"


def aggregate_or_single_element(elements: Sequence[str]) -> str:
    result = ", ".join(elements)
    return f"({result})" if len(elements) > 1 else result
