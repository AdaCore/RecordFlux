# pylint: disable=too-many-lines
import itertools
from abc import abstractmethod
from collections import OrderedDict
from dataclasses import dataclass, field as dataclass_field
from enum import Enum
from sys import intern
from typing import List, Mapping, Optional, Sequence, Tuple, Union

import rflx.identifier
from rflx.common import Base, file_name, indent, indent_next, unique
from rflx.contract import invariant


class ID(rflx.identifier.ID):
    @property
    def _separator(self) -> str:
        return "."

    def __add__(self, other: object) -> "ID":
        result = super().__add__(other)
        assert isinstance(result, ID)
        return result

    def __radd__(self, other: object) -> "ID":
        result = super().__radd__(other)
        assert isinstance(result, ID)
        return result

    def __mul__(self, other: object) -> "ID":
        result = super().__mul__(other)
        assert isinstance(result, ID)
        return result

    def __rmul__(self, other: object) -> "ID":
        result = super().__rmul__(other)
        assert isinstance(result, ID)
        return result


StrID = Union[str, ID]


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
    _str: str

    def __str__(self) -> str:
        try:
            return self._str
        except AttributeError:
            self._update_str()
            return self._str

    def __neg__(self) -> "Expr":
        raise NotImplementedError

    @abstractmethod
    def _update_str(self) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    def parenthesized(self, expr: "Expr") -> str:
        if expr.precedence.value <= self.precedence.value:
            return "(" + indent_next(str(expr), 1) + ")"
        return str(expr)


class Not(Expr):
    def __init__(self, expr: Expr) -> None:
        super().__init__()
        self.expr = expr

    def _update_str(self) -> None:
        self._str = intern(f"not {self.parenthesized(self.expr)}")

    @property
    def precedence(self) -> Precedence:
        return Precedence.HIGHEST_PRECEDENCE_OPERATOR


class BinExpr(Expr):
    def __init__(self, left: Expr, right: Expr) -> None:
        super().__init__()
        self.left = left
        self.right = right

    def _update_str(self) -> None:
        self._str = intern(
            f"{self.parenthesized(self.left)}{self.symbol}{self.parenthesized(self.right)}"
        )

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class AssExpr(Expr):
    def __init__(self, *terms: Expr) -> None:
        super().__init__()
        self.terms = list(terms)

    def _update_str(self) -> None:
        self._str = intern(self.symbol.join(map(self.parenthesized, self.terms)))

    @property
    @abstractmethod
    def precedence(self) -> Precedence:
        raise NotImplementedError

    @property
    @abstractmethod
    def symbol(self) -> str:
        raise NotImplementedError


class BoolAssExpr(AssExpr):
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

    def __neg__(self) -> "Number":
        return Number(-self.value)

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

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


class Add(AssExpr):
    def _update_str(self) -> None:
        self._str = str(self.terms[0])
        for t in self.terms[1:]:
            if (isinstance(t, Number) and t.value < 0) or (isinstance(t, Name) and t.negative):
                self._str += f" - {self.parenthesized(-t)}"
            else:
                self._str += f"{self.symbol}{self.parenthesized(t)}"
        self._str = intern(self._str)

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


class Name(Expr):
    def __init__(self, negative: bool = False) -> None:
        super().__init__()
        self.negative = negative
        self._update_str()

    def _update_str(self) -> None:
        self._str = intern(f"(-{self._representation})" if self.negative else self._representation)

    @property
    @abstractmethod
    def _representation(self) -> str:
        raise NotImplementedError

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


class Variable(Name):
    def __init__(self, identifier: StrID, negative: bool = False) -> None:
        self.identifier = ID(identifier)
        super().__init__(negative)

    def __neg__(self) -> "Variable":
        return self.__class__(self.identifier, not self.negative)

    @property
    def _representation(self) -> str:
        return str(self.name)

    @property
    def name(self) -> str:
        return str(self.identifier)


TRUE = Variable("True")
FALSE = Variable("False")


class Attribute(Name):
    def __init__(self, prefix: Union[StrID, Expr], negative: bool = False) -> None:
        if isinstance(prefix, ID):
            prefix = Variable(prefix)
        if isinstance(prefix, str):
            prefix = Variable(prefix)

        self.prefix: Expr = prefix
        super().__init__(negative)

    def __neg__(self) -> "Attribute":
        return self.__class__(self.prefix, not self.negative)

    @property
    def _representation(self) -> str:
        return f"{self.prefix}'{self.__class__.__name__}"


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


class Valid(Attribute):
    pass


class AttributeExpr(Attribute):
    def __init__(
        self, prefix: Union[StrID, Expr], expression: Expr, negative: bool = False
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


@invariant(lambda self: len(self.elements) > 0)
class Indexed(Name):
    def __init__(self, prefix: Expr, *elements: Expr, negative: bool = False) -> None:
        self.prefix = prefix
        self.elements = list(elements)
        super().__init__(negative)

    def __neg__(self) -> "Indexed":
        return self.__class__(self.prefix, *self.elements, negative=not self.negative)

    @property
    def _representation(self) -> str:
        return f"{self.prefix} (" + ", ".join(map(str, self.elements)) + ")"


class Selected(Name):
    def __init__(
        self,
        prefix: Expr,
        selector_identifier: StrID,
        negative: bool = False,
    ) -> None:
        self.prefix = prefix
        self.selector_identifier = ID(selector_identifier)
        super().__init__(negative)

    def __neg__(self) -> "Selected":
        return self.__class__(self.prefix, self.selector_identifier, not self.negative)

    @property
    def _representation(self) -> str:
        return f"{self.prefix}.{self.selector_identifier}"


class Call(Name):
    def __init__(
        self,
        identifier: StrID,
        args: Sequence[Expr] = None,
        negative: bool = False,
    ) -> None:
        self.identifier = ID(identifier)
        self.args = args or []
        super().__init__(negative)

    def __neg__(self) -> "Call":
        return self.__class__(self.identifier, self.args, not self.negative)

    @property
    def _representation(self) -> str:
        args = ", ".join(map(str, self.args))
        if args:
            args = f" ({args})"
        call = f"{self.identifier}{args}"
        return call


class Slice(Name):
    def __init__(self, prefix: Expr, first: Expr, last: Expr) -> None:
        self.prefix = prefix
        self.first = first
        self.last = last
        super().__init__()

    @property
    def _representation(self) -> str:
        return f"{self.prefix} ({self.first} .. {self.last})"


class Aggregate(Expr):
    def __init__(self, *elements: Expr) -> None:
        super().__init__()
        self.elements = list(elements)

    def _update_str(self) -> None:
        assert len(self.elements) > 1
        self._str = intern("(" + ", ".join(map(str, self.elements)) + ")")

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


class String(Aggregate):
    def __init__(self, data: str) -> None:
        data = data.replace('"', '""')
        super().__init__(*[Number(ord(d)) for d in data])
        self.data = data

    def _update_str(self) -> None:
        self._str = intern(f'"{self.data}"')

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


class NamedAggregate(Expr):
    def __init__(self, *elements: Tuple[Union[StrID, Expr], Expr]) -> None:
        super().__init__()
        self.elements = [(ID(n) if isinstance(n, str) else n, e) for n, e in elements]

    def _update_str(self) -> None:
        assert len(self.elements) > 0
        self._str = intern(
            "(" + ", ".join(f"{name} => {element}" for name, element in self.elements) + ")"
        )

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError


class Relation(BinExpr):
    @property
    def precedence(self) -> Precedence:
        return Precedence.RELATIONAL_OPERATOR


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


def If(condition_expressions: Sequence[Tuple[Expr, Expr]], else_expression: Expr = None) -> Expr:
    # pylint: disable=invalid-name
    if len(condition_expressions) == 1 and condition_expressions[0][0] == TRUE:
        return condition_expressions[0][1]
    return IfExpr(condition_expressions, else_expression)


class IfExpr(Expr):
    def __init__(
        self, condition_expressions: Sequence[Tuple[Expr, Expr]], else_expression: Expr = None
    ) -> None:
        super().__init__()
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

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

        if len(expression) > 100:
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

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


def Case(control_expression: Expr, case_expressions: Sequence[Tuple[Expr, Expr]]) -> Expr:
    # pylint: disable=invalid-name
    if len(case_expressions) == 1 and case_expressions[0][0] == Variable("others"):
        return case_expressions[0][1]
    return CaseExpr(control_expression, case_expressions)


class CaseExpr(Expr):
    def __init__(
        self, control_expression: Expr, case_expressions: Sequence[Tuple[Expr, Expr]]
    ) -> None:
        super().__init__()
        self.control_expression = control_expression
        self.case_expressions = case_expressions

    def _update_str(self) -> None:
        grouped_cases = [
            (" | ".join(str(c) for c, _ in choices), expr)
            for expr, choices in itertools.groupby(self.case_expressions, lambda x: x[1])
        ]
        cases = indent(
            ",".join(
                [f"\nwhen {choice} =>\n{indent(str(expr), 3)}" for choice, expr in grouped_cases]
            ),
            4,
        )
        self._str = intern(f"(case {self.control_expression} is{cases})")

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


class QuantifiedExpr(Expr):
    def __init__(self, parameter_identifier: StrID, iterable: Expr, predicate: Expr) -> None:
        super().__init__()
        self.parameter_identifier = ID(parameter_identifier)
        self.iterable = iterable
        self.predicate = predicate

    def _update_str(self) -> None:
        self._str = intern(
            f"(for {self.quantifier} {self.parameter_identifier} {self.keyword} {self.iterable}"
            f" =>\n{indent(str(self.predicate), 4)})"
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
    def __init__(self, lower: Expr, upper: Expr):
        super().__init__()
        self.lower = lower
        self.upper = upper

    def _update_str(self) -> None:
        self._str = intern(f"{self.lower} .. {self.upper}")

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError


class Conversion(Expr):
    def __init__(self, identifier: StrID, argument: Expr) -> None:
        super().__init__()
        self.identifier = ID(identifier)
        self.argument = argument

    def _update_str(self) -> None:
        self._str = intern(f"{self.identifier} ({self.argument})")

    @property
    def precedence(self) -> Precedence:
        return Precedence.LITERAL


class QualifiedExpr(Expr):
    def __init__(self, type_identifier: StrID, expression: Expr) -> None:
        super().__init__()
        self.type_identifier = ID(type_identifier)
        self.expression = expression

    def _update_str(self) -> None:
        operand = (
            str(self.expression)
            if isinstance(self.expression, (Aggregate, NamedAggregate))
            else f"({self.expression})"
        )
        self._str = intern(f"{self.type_identifier}'{operand}")

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError


class New(Expr):
    def __init__(self, expression: Expr) -> None:
        super().__init__()
        self.expression = expression

    def _update_str(self) -> None:
        self._str = intern(f"new {self.expression}")

    @property
    def precedence(self) -> Precedence:
        raise NotImplementedError


class Raise(Expr):
    def __init__(self, identifier: StrID, string: Expr = None) -> None:
        super().__init__()
        self.identifier = ID(identifier)
        self.string = string

    def _update_str(self) -> None:
        string = f" with {self.string}" if self.string else ""
        self._str = intern(f"raise {self.identifier}{string}")

    @property
    def precedence(self) -> Precedence:
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
        return f"with {self.identifier};"


class UsePackageClause(ContextItem, Declaration):
    def __str__(self) -> str:
        return f"use {self.identifier};"


class UseTypeClause(ContextItem, Declaration):
    def __str__(self) -> str:
        return f"use type {self.identifier};"


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
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Pre"

    @property
    def definition(self) -> str:
        return str(self.expr)


class Postcondition(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Post"

    @property
    def definition(self) -> str:
        return str(self.expr)


class ContractCases(Aspect):
    def __init__(self, *cases: Tuple[Expr, Expr]) -> None:
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
        self.dependencies = dependencies

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
            ", ".join(f"{o} => {input_values(i)}" for o, i in self.dependencies.items()), 1
        )
        return f"({dependencies})"


class DynamicPredicate(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Dynamic_Predicate"

    @property
    def definition(self) -> str:
        return str(self.expr)


class SizeAspect(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Size"

    @property
    def definition(self) -> str:
        return str(self.expr)


class InitialCondition(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Initial_Condition"

    @property
    def definition(self) -> str:
        return str(self.expr)


class DefaultInitialCondition(Aspect):
    def __init__(self, expr: Expr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return "Default_Initial_Condition"

    @property
    def definition(self) -> str:
        return str(self.expr)


class SparkMode(Aspect):
    @property
    def mark(self) -> str:
        return "SPARK_Mode"

    @property
    def definition(self) -> str:
        return ""


class Ghost(Aspect):
    @property
    def mark(self) -> str:
        return "Ghost"

    @property
    def definition(self) -> str:
        return ""


class Import(Aspect):
    @property
    def mark(self) -> str:
        return "Import"

    @property
    def definition(self) -> str:
        return ""


class Annotate(Aspect):
    def __init__(self, *args: str) -> None:
        self.args = args

    @property
    def mark(self) -> str:
        return "Annotate"

    @property
    def definition(self) -> str:
        args = ", ".join(self.args)
        return f"({args})"


class FormalDeclaration(Base):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class FormalSubprogramDeclaration(FormalDeclaration):
    def __init__(self, specification: "SubprogramSpecification") -> None:
        self.specification = specification

    def __hash__(self) -> int:
        return hash(self.specification)

    def __str__(self) -> str:
        return f"with {self.specification};"


class FormalPackageDeclaration(FormalDeclaration):
    def __init__(
        self, identifier: StrID, generic_identifier: StrID, associations: Sequence[StrID] = None
    ) -> None:
        self.identifier = ID(identifier)
        self.generic_identifier = ID(generic_identifier)
        self.associations = list(map(str, associations or []))

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations)) if self.associations else "<>"
        return f"with package {self.identifier} is new {self.generic_identifier} ({associations});"


class PackageDeclaration(Declaration):
    def __init__(
        self,
        identifier: StrID,
        declarations: List[Declaration] = None,
        private_declarations: List[Declaration] = None,
        formal_parameters: List[FormalDeclaration] = None,
        aspects: List[Aspect] = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.declarations = declarations or []
        self.private_declarations = private_declarations or []
        self.formal_parameters = formal_parameters
        self.aspects = aspects or []

    def __str__(self) -> str:
        return (
            f"{generic_formal_part(self.formal_parameters)}"
            f"package {self.identifier}{aspect_specification(self.aspects)}\nis\n\n"
            f"{declarative_items(self.declarations)}"
            f"{declarative_items(self.private_declarations, private=True)}"
            f"end {self.identifier};\n"
        )


class PackageBody(Declaration):
    def __init__(
        self,
        identifier: StrID,
        declarations: List[Declaration] = None,
        statements: List["Statement"] = None,
        aspects: List[Aspect] = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.declarations = declarations or []
        self.statements = statements or []
        self.aspects = aspects or []

    def __str__(self) -> str:
        if not self.declarations and not self.statements:
            return ""

        statements = (
            ("begin\n\n" + indent("\n".join(map(str, self.statements)), 3) + "\n\n")
            if self.statements
            else ""
        )

        return (
            f"package body {self.identifier}{aspect_specification(self.aspects)}\nis\n\n"
            f"{declarative_items(self.declarations)}{statements}end {self.identifier};\n"
        )


class GenericPackageInstantiation(Declaration):
    def __init__(
        self, identifier: StrID, generic_package: StrID, associations: Sequence[StrID] = None
    ) -> None:
        self.identifier = ID(identifier)
        self.generic_package = ID(generic_package)
        self.associations = list(map(ID, associations or []))

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations))
        if associations:
            associations = f" ({associations})"
        return f"package {self.identifier} is new {self.generic_package}{associations};"


class PackageRenamingDeclaration(Declaration):
    def __init__(self, identifier: StrID, package_identifier: StrID) -> None:
        self.identifier = ID(identifier)
        self.package_identifier = ID(package_identifier)

    def __str__(self) -> str:
        return f"package {self.identifier} renames {self.package_identifier};"


class ObjectDeclaration(Declaration):
    def __init__(
        self,
        identifiers: Sequence[StrID],
        type_identifier: Union[StrID, Expr],
        expression: Expr = None,
        constant: bool = False,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_identifier = (
            type_identifier if isinstance(type_identifier, Expr) else Variable(type_identifier)
        )
        self.expression = expression
        self.constant = constant
        self.aspects = aspects or []

    def __hash__(self) -> int:
        return hash(tuple(self.identifiers))

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        constant = "constant " if self.constant else ""
        expression = f" := {self.expression}" if self.expression else ""
        return (
            f"{identifiers} : {constant}{self.type_identifier}{expression}"
            f"{aspect_specification(self.aspects)};"
        )


class Discriminant(Base):
    def __init__(
        self, identifiers: Sequence[StrID], type_identifier: StrID, default: Expr = None
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_identifier = ID(type_identifier)
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        default = f" := {self.default}" if self.default else ""
        return f"{identifiers} : {self.type_identifier}{default}"


class TypeDeclaration(Declaration, FormalDeclaration):
    def __init__(
        self,
        identifier: StrID,
        discriminants: Sequence[Discriminant] = None,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        self.identifier = ID(identifier)
        self.discriminants = discriminants
        self.aspects = aspects or []

    def __hash__(self) -> int:
        return hash(self.identifier)

    def __str__(self) -> str:
        return (
            f"type {self.identifier}{self.discriminant_part} is{self.type_definition}"
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
    def __init__(self, identifier: StrID, modulus: Expr, aspects: Sequence[Aspect] = None) -> None:
        super().__init__(identifier, aspects=aspects or [])
        self.modulus = modulus

    @property
    def type_definition(self) -> str:
        return f" mod {self.modulus}"


class RangeType(TypeDeclaration):
    def __init__(
        self,
        identifier: StrID,
        first: Expr,
        last: Expr,
        aspects: Sequence[Aspect] = None,
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
        literals: Mapping[ID, Optional[Number]],
        size: Expr = None,
    ) -> None:
        super().__init__(identifier, aspects=([SizeAspect(size)] if size else []))
        self.literals = (
            OrderedDict(
                sorted(
                    literals.items(), key=lambda t: t[1].value if isinstance(t[1], Number) else 0
                )
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
            f"\nfor {self.identifier} use ({literal_representation});"
            if literal_representation
            else ""
        )


class Subtype(TypeDeclaration):
    def __init__(self, identifier: StrID, base_identifier: StrID) -> None:
        super().__init__(identifier)
        self.base_identifier = ID(base_identifier)

    def __str__(self) -> str:
        return "sub" + super().__str__()

    @property
    def type_definition(self) -> str:
        return f" {self.base_identifier}"


class RangeSubtype(Subtype):
    def __init__(self, identifier: StrID, base_identifier: StrID, first: Expr, last: Expr) -> None:
        super().__init__(identifier, base_identifier)
        self.first = first
        self.last = last

    @property
    def type_definition(self) -> str:
        return f" {self.base_identifier} range {self.first} .. {self.last}"


class DerivedType(TypeDeclaration):
    def __init__(self, identifier: StrID, type_identifier: StrID) -> None:
        super().__init__(identifier)
        self.type_identifier = ID(type_identifier)

    @property
    def type_definition(self) -> str:
        return f" new {self.type_identifier}"


class PrivateType(TypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " private"


class DiscreteType(TypeDeclaration):
    @property
    def type_definition(self) -> str:
        return " (<>)"


class ArrayType(TypeDeclaration):
    def __init__(self, identifier: StrID, index_type: StrID, component_identifier: StrID) -> None:
        super().__init__(identifier)
        self.index_type = ID(index_type)
        self.component_identifier = ID(component_identifier)

    @property
    def type_definition(self) -> str:
        return f" array ({self.index_type}) of {self.component_identifier}"


class UnconstrainedArrayType(ArrayType):
    @property
    def type_definition(self) -> str:
        return f" array ({self.index_type} range <>) of {self.component_identifier}"


class AccessType(TypeDeclaration):
    def __init__(self, identifier: StrID, object_identifier: StrID) -> None:
        super().__init__(identifier)
        self.object_identifier = ID(object_identifier)

    @property
    def type_definition(self) -> str:
        return f" access {self.object_identifier}"


class Component(Base):
    def __init__(
        self, identifier: StrID, type_identifier: Union[StrID, Expr], default: Expr = None
    ) -> None:
        self.identifier = ID(identifier)
        self.type_identifier = (
            type_identifier if isinstance(type_identifier, Expr) else Variable(type_identifier)
        )
        self.default = default

    def __str__(self) -> str:
        default = f" := {self.default}" if self.default else ""
        return f"{self.identifier} : {self.type_identifier}{default};"


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
        return f"case {self.discriminant_identifier} is\n{variants}\nend case;\n"


class RecordType(TypeDeclaration):
    def __init__(
        self,
        identifier: StrID,
        components: List[Component],
        discriminants: Sequence[Discriminant] = None,
        variant_part: VariantPart = None,
        aspects: Sequence[Aspect] = None,
    ) -> None:
        super().__init__(identifier, discriminants, aspects)
        self.components = components
        self.discriminants = discriminants or []
        self.variant_part = variant_part

    @property
    def type_definition(self) -> str:
        components = (
            (indent("\n".join(map(str, self.components)), 6) + "\n") if self.components else ""
        )
        variant_part = indent(str(self.variant_part), 6) if self.variant_part else ""
        return f"\n" f"   record\n" f"{components}" f"{variant_part}" f"   end record"


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
    def __init__(self, identifier: Union[StrID, Expr], expression: Expr) -> None:
        self.identifier = identifier if isinstance(identifier, Expr) else Variable(identifier)
        self.expression = expression

    def __str__(self) -> str:
        return f"{self.identifier} := {self.expression};"


class CallStatement(Statement):
    def __init__(self, identifier: StrID, arguments: Sequence[Expr] = None) -> None:
        self.identifier = ID(identifier)
        self.arguments = arguments or []

    def __str__(self) -> str:
        arguments = ", ".join(map(str, self.arguments))
        arguments = f" ({arguments})" if arguments else ""
        return f"{self.identifier}{arguments};"


class PragmaStatement(Statement):
    def __init__(self, identifier: StrID, parameters: Sequence[Expr]) -> None:
        self.identifier = ID(identifier)
        self.pragma_parameters = parameters

    def __str__(self) -> str:
        parameters = ""
        if self.pragma_parameters:
            parameters = ", ".join(map(str, self.pragma_parameters))
            parameters = " (" + indent_next(str(parameters), len(str(self.identifier)) + 9) + ")"
        return f"pragma {self.identifier}{parameters};"


class ReturnStatement(Statement):
    def __init__(self, expression: Expr = None) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if not self.expression:
            return "return;"
        if isinstance(self.expression, CaseExpr):
            return "return (" + indent_next(str(self.expression), 8) + ");"
        return "return " + indent_next(str(self.expression), 7) + ";"


class ExitStatement(Statement):
    def __init__(self, expression: Expr = None) -> None:
        self.expression = expression

    def __str__(self) -> str:
        if not self.expression:
            return "exit;"
        if isinstance(self.expression, CaseExpr):
            return "exit when (" + indent_next(str(self.expression), 8) + ");"
        return "exit when " + indent_next(str(self.expression), 7) + ";"


class IfStatement(Statement):
    def __init__(
        self,
        condition_statements: Sequence[Tuple[Expr, Sequence[Statement]]],
        else_statements: Sequence[Statement] = None,
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
                else f"\n{indent(str(condition), 2)}\n"
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
        case_statements: Sequence[Tuple[Expr, Sequence[Statement]]],
        case_grouping: bool = True,
    ) -> None:
        self.control_expression = control_expression
        self.case_statements = case_statements
        self.case_grouping = case_grouping

    def __str__(self) -> str:
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
                "\nwhen {} =>\n{}".format(choice, indent("\n".join(str(s) for s in statements), 3))
                for choice, statements in grouped_cases
            ]
        )

        return f"case {self.control_expression} is{indent(cases, 3)}\nend case;"


class While(Statement):
    def __init__(self, condition: Expr, statements: Sequence[Statement]) -> None:
        assert len(statements) > 0
        self.condition = condition
        self.statements = statements

    def __str__(self) -> str:
        statements = indent("\n".join(str(s) for s in self.statements), 3)
        return f"while {self.condition} loop\n{statements}\nend loop;"


class ForOf(Statement):
    def __init__(
        self,
        identifier: StrID,
        iterator: Expr,
        statements: Sequence[Statement],
        reverse: bool = False,
    ) -> None:
        assert len(statements) > 0
        self.identifier = identifier
        self.iterator = iterator
        self.statements = statements
        self.reverse = reverse

    def __str__(self) -> str:
        statements = indent("\n".join(str(s) for s in self.statements), 3)
        reverse = "reverse " if self.reverse else ""
        return f"for {self.identifier} of {reverse}{self.iterator} loop\n{statements}\nend loop;"


class Declare(Statement):
    def __init__(
        self, declarations: Sequence[Declaration], statements: Sequence[Statement]
    ) -> None:
        self.declarations = declarations
        self.statements = statements

    def __str__(self) -> str:
        declarations = indent("\n".join(str(s) for s in self.declarations), 3)
        statements = indent("\n".join(str(s) for s in self.statements), 3)
        return f"declare\n{declarations}\nbegin\n{statements}\nend;"


class Parameter(Base):
    def __init__(
        self, identifiers: Sequence[StrID], type_identifier: StrID, default: Expr = None
    ) -> None:
        self.identifiers = list(map(ID, identifiers))
        self.type_identifier = ID(type_identifier)
        self.default = default

    def __str__(self) -> str:
        identifiers = ", ".join(map(str, self.identifiers))
        default = f" := {self.default}" if self.default else ""
        return f"{identifiers} : {self.mode}{self.type_identifier}{default}"

    @property
    def mode(self) -> str:
        return ""


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
        default: Expr = None,
        constant: bool = False,
    ) -> None:
        super().__init__(identifiers, type_identifier, default)
        self.constant = constant

    @property
    def mode(self) -> str:
        return "access constant " if self.constant else "access "


class SubprogramSpecification(Base):
    def __init__(self, identifier: StrID, parameters: Sequence[Parameter] = None) -> None:
        self.identifier = ID(identifier)
        self.parameters = parameters or []

    def _parameters(self) -> str:
        return (" (" + "; ".join(map(str, self.parameters)) + ")") if self.parameters else ""

    def __hash__(self) -> int:
        return hash(self.identifier)

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class ProcedureSpecification(SubprogramSpecification):
    def __str__(self) -> str:
        return f"procedure {self.identifier}{self._parameters()}"


class FunctionSpecification(SubprogramSpecification):
    def __init__(
        self, identifier: StrID, return_type: StrID, parameters: Sequence[Parameter] = None
    ) -> None:
        super().__init__(identifier, parameters)
        self.return_type = ID(return_type)

    def __str__(self) -> str:
        return f"function {self.identifier}{self._parameters()} return {self.return_type}"


class Subprogram(Declaration):
    def __init__(self, specification: SubprogramSpecification) -> None:
        self.specification = specification

    def __hash__(self) -> int:
        return hash(self.specification)

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError


class SubprogramDeclaration(Subprogram):
    def __init__(
        self,
        specification: SubprogramSpecification,
        aspects: Sequence[Aspect] = None,
        formal_parameters: Sequence[FormalDeclaration] = None,
    ) -> None:
        super().__init__(specification)
        self.aspects = aspects or []
        self.formal_parameters = formal_parameters

    def __str__(self) -> str:
        return (
            f"{generic_formal_part(self.formal_parameters)}"
            f"{self.specification}{aspect_specification(self.aspects)};"
        )


class SubprogramBody(Subprogram):
    def __init__(
        self,
        specification: SubprogramSpecification,
        declarations: Sequence[Declaration],
        statements: Sequence[Statement],
        aspects: Sequence[Aspect] = None,
    ) -> None:
        super().__init__(specification)
        self.declarations = declarations or []
        self.statements = statements or []
        self.aspects = aspects or []

    def _declarations(self) -> str:
        return "".join(indent(f"{declaration}\n", 3) for declaration in self.declarations)

    def _statements(self) -> str:
        return "\n".join(indent(str(s), 3) for s in self.statements)

    def __str__(self) -> str:
        aspects = f"{aspect_specification(self.aspects)}\n" if self.aspects else " "
        return (
            f"{self.specification}{aspects}is\n"
            f"{self._declarations()}"
            f"begin\n"
            f"{self._statements()}\n"
            f"end {self.specification.identifier};"
        )


class ExpressionFunctionDeclaration(Subprogram):
    def __init__(
        self,
        specification: FunctionSpecification,
        expression: Expr,
        aspects: List[Aspect] = None,
    ) -> None:
        super().__init__(specification)
        self.expression = expression
        self.aspects = aspects or []

    def __str__(self) -> str:
        aspects = f"\n{aspect_specification(self.aspects)}" if self.aspects else ""
        return f"{self.specification} is\n  ({indent_next(str(self.expression), 3)}){aspects};"


class GenericProcedureInstantiation(Subprogram):
    def __init__(
        self,
        identifier: StrID,
        specification: ProcedureSpecification,
        associations: List[StrID] = None,
    ) -> None:
        super().__init__(specification)
        self.identifier = ID(identifier)
        self.parameters = specification.parameters
        self.associations = list(map(ID, associations or []))

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations))
        if associations:
            associations = f" ({associations})"
        return f"procedure {self.identifier} is new {self.specification.identifier}{associations};"


class GenericFunctionInstantiation(Subprogram):
    def __init__(
        self,
        identifier: StrID,
        specification: FunctionSpecification,
        associations: List[StrID] = None,
    ) -> None:
        super().__init__(specification)
        self.identifier = ID(identifier)
        self.parameters = specification.parameters
        self.associations = list(map(ID, associations or []))

    def __str__(self) -> str:
        associations = ", ".join(map(str, self.associations))
        if associations:
            associations = f" ({associations})"
        return f"function {self.identifier} is new {self.specification.identifier}{associations};"


class SubprogramRenamingDeclaration(Subprogram):
    def __init__(
        self, specification: SubprogramSpecification, subprogram_identifier: StrID
    ) -> None:
        super().__init__(specification)
        self.subprogram_identifier = ID(subprogram_identifier)

    def __str__(self) -> str:
        return f"{self.specification} renames {self.subprogram_identifier};"


class Pragma(Declaration, ContextItem):
    def __init__(self, identifier: StrID, parameters: List[Expr] = None) -> None:
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
            parameters = f" ({parameters})"
        return f"pragma {self.identifier}{parameters};"


class Unit(Base):
    @abstractmethod
    def __iadd__(self, other: object) -> "Unit":
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


@invariant(lambda self: self.declaration.identifier == self.body.identifier)
class PackageUnit(Unit):
    def __init__(
        self,
        declaration_context: List[ContextItem],
        declaration: PackageDeclaration,
        body_context: List[ContextItem],
        body: PackageBody,
    ) -> None:
        self.declaration_context = declaration_context
        self.declaration = declaration
        self.body_context = body_context
        self.body = body

    def __iadd__(self, other: object) -> "PackageUnit":
        if isinstance(other, (UnitPart, SubprogramUnitPart)):
            self.declaration.declarations = self.declaration.declarations + list(
                other.specification
            )
            self.declaration.private_declarations = self.declaration.private_declarations + list(
                other.private
            )
            self.body.declarations = self.body.declarations + list(other.body)
            self.body.statements += list(other.statements)
            return self
        return NotImplemented

    @property
    def ads(self) -> str:
        return f"{context_clause(self.declaration_context)}{self.declaration}"

    @property
    def adb(self) -> str:
        return f"{context_clause(self.body_context)}{self.body}" if str(self.body) else ""

    @property
    def name(self) -> str:
        return file_name(str(self.declaration.identifier))


class InstantiationUnit(Unit):
    def __init__(
        self, context: List[ContextItem], declaration: GenericPackageInstantiation
    ) -> None:
        self.context = context
        self.declaration = declaration

    def __iadd__(self, other: object) -> Unit:
        return NotImplemented

    @property
    def ads(self) -> str:
        return f"{context_clause(self.context)}{self.declaration}\n"

    @property
    def adb(self) -> str:
        return ""

    @property
    def name(self) -> str:
        return file_name(str(self.declaration.identifier))


@dataclass
class UnitPart:
    specification: List[Declaration] = dataclass_field(default_factory=list)
    body: List[Declaration] = dataclass_field(default_factory=list)
    private: List[Declaration] = dataclass_field(default_factory=list)
    statements: List[Statement] = dataclass_field(default_factory=list)

    def __iadd__(self, other: object) -> "UnitPart":
        if isinstance(other, UnitPart):
            self.specification += other.specification
            self.body += other.body
            self.private += other.private
            self.statements += other.statements
            return self
        return NotImplemented


@dataclass
class SubprogramUnitPart:
    specification: List[Subprogram] = dataclass_field(default_factory=list)
    body: List[Subprogram] = dataclass_field(default_factory=list)
    private: List[Subprogram] = dataclass_field(default_factory=list)
    statements: List[Statement] = dataclass_field(default_factory=list)


def generic_formal_part(parameters: Sequence[FormalDeclaration] = None) -> str:
    if parameters is None:
        return ""
    return (
        "generic" + ("".join(f"\n   {p}" for p in unique(parameters)) if parameters else "") + "\n"
    )


def declarative_items(declarations: List[Declaration], private: bool = False) -> str:
    result = "\n\n".join(indent(str(d), 3) for d in unique(declarations) if str(d))
    if result:
        result = f"private\n\n{result}" if private else result
        result += "\n\n"
    return result


def aspect_specification(aspects: Sequence[Aspect]) -> str:
    if not aspects:
        return ""
    return " with\n" + ",\n".join(indent(str(aspect), 2) for aspect in aspects)


def context_clause(context: Sequence[ContextItem]) -> str:
    return ("\n".join(map(str, unique(context))) + "\n\n") if context else ""
