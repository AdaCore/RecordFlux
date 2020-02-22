from typing import Any, List

from pyparsing import (
    Forward,
    Keyword,
    Literal,
    Optional,
    StringEnd,
    Suppress,
    Token,
    Word,
    delimitedList,
    infixNotation,
    oneOf,
    opAssoc,
    printables,
)

from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    Div,
    Equal,
    Expr,
    Greater,
    Length,
    Less,
    Mul,
    NotEqual,
    Or,
    Sub,
    Variable,
)
from rflx.fsm_declaration import Argument, Subprogram, VariableDeclaration
from rflx.fsm_expression import (
    Binding,
    Comprehension,
    Contains,
    Field,
    ForAll,
    ForSome,
    Head,
    MessageAggregate,
    NotContains,
    Opaque,
    Present,
    String,
    SubprogramCall,
    Valid,
)
from rflx.identifier import ID
from rflx.parser.grammar import (
    boolean_literal,
    numeric_literal,
    qualified_identifier,
    unqualified_identifier,
)
from rflx.statement import Assignment, Erase, Reset


class InternalError(Exception):
    pass


class FSMParser:
    @classmethod
    def __parse_quantifier(cls, tokens: List[Expr]) -> Expr:
        assert isinstance(tokens[1], ID)
        if tokens[0] == "all":
            return ForAll(tokens[1], tokens[2], tokens[3])
        return ForSome(tokens[1], tokens[2], tokens[3])

    @classmethod
    def __parse_comprehension(cls, tokens: List[Expr]) -> Expr:
        assert isinstance(tokens[0], ID)
        condition = tokens[3] if len(tokens) > 3 else TRUE
        return Comprehension(tokens[0], tokens[1], tokens[2], condition)

    @classmethod
    def __parse_call(cls, tokens: List[Expr]) -> Expr:
        assert isinstance(tokens[0], ID)
        return SubprogramCall(tokens[0], tokens[1:])

    @classmethod
    def __identifier(cls) -> Token:
        identifier = qualified_identifier()
        identifier.setParseAction(lambda t: ID("".join(map(str, t.asList()))))
        return identifier

    @classmethod
    def __parse_op_comp(cls, tokens: List[Expr]) -> Expr:
        if tokens[1] == "<":
            return Less(tokens[0], tokens[2])
        if tokens[1] == ">":
            return Greater(tokens[0], tokens[2])
        if tokens[1] == "=":
            return Equal(tokens[0], tokens[2])
        if tokens[1] == "/=":
            return NotEqual(tokens[0], tokens[2])
        raise InternalError(f"Unsupported comparison operator {tokens[1]}")

    @classmethod
    def __parse_op_set(cls, tokens: List[Expr]) -> Expr:
        if tokens[1] == "not in":
            return NotContains(tokens[0], tokens[2])
        if tokens[1] == "in":
            return Contains(tokens[0], tokens[2])
        raise InternalError(f"Unsupported set operator {tokens[1]}")

    @classmethod
    def __parse_op_add_sub(cls, tokens: List[Expr]) -> Expr:
        result = tokens[0]
        for op, right in zip(tokens[1::2], tokens[2::2]):
            if op == "+":
                result = Add(result, right)
            elif op == "-":
                result = Sub(result, right)
            else:
                raise InternalError(f"Unsupported add/sub operator {op}")
        return result

    @classmethod
    def __parse_op_mul_div(cls, tokens: List[Expr]) -> Expr:
        result = tokens[0]
        for op, right in zip(tokens[1::2], tokens[2::2]):
            if op == "*":
                result = Mul(result, right)
            elif op == "/":
                result = Div(result, right)
            else:
                raise InternalError(f"Unsupported mul/div operator {op}")
        return result

    @classmethod
    def __parse_suffix(cls, data: List[Any]) -> Expr:
        result = data[0][0]
        for suffix in data[0][1:]:
            if suffix[0] == "Head":
                result = Head(result)
            if suffix[0] == "Valid":
                result = Valid(result)
            if suffix[0] == "Opaque":
                result = Opaque(result)
            if suffix[0] == "Present":
                result = Present(result)
            if suffix[0] == "Length":
                result = Length(result)
            if suffix[0] == "Field":
                result = Field(result, suffix[1])
            if suffix[0] == "Binding":
                result = Binding(result, suffix[1])
            if suffix[0] == "Aggregate":
                result = MessageAggregate(result.identifier, suffix[1])

        return result

    @classmethod
    def expression(cls) -> Token:  # pylint: disable=too-many-locals

        bool_literal = boolean_literal()
        bool_literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

        string_literal = (
            Literal('"').suppress()
            + Word(printables + " ", excludeChars='"')
            + Literal('"').suppress()
        )
        string_literal.setParseAction(lambda t: String(t[0]))

        expression = Forward()

        parameters = delimitedList(expression, delim=",")

        lpar, rpar = map(Suppress, "()")
        function_call = cls.__identifier() + lpar + parameters + rpar
        function_call.setParseAction(cls.__parse_call)

        quantifier = (
            Keyword("for").suppress()
            - oneOf(["all", "some"])
            + unqualified_identifier()
            - Keyword("in").suppress()
            + expression
            - Keyword("=>").suppress()
            + expression
        )
        quantifier.setParseAction(cls.__parse_quantifier)

        comprehension = (
            Literal("[").suppress()
            - Keyword("for").suppress()
            + unqualified_identifier()
            - Keyword("in").suppress()
            + expression
            - Keyword("=>").suppress()
            + expression
            + Optional(Keyword("when").suppress() + expression)
            - Literal("]").suppress()
        )
        comprehension.setParseAction(cls.__parse_comprehension)

        components = delimitedList(
            unqualified_identifier() + Keyword("=>").suppress() + expression, delim=","
        )
        components.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

        terms = delimitedList(
            unqualified_identifier() + Keyword("=").suppress() + expression, delim=","
        )
        terms.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

        variable = cls.__identifier()
        variable.setParseAction(lambda t: Variable(ID("".join(map(str, t.asList())))))

        atom = (
            numeric_literal()
            | bool_literal
            | string_literal
            | quantifier
            | comprehension
            | function_call
            | variable
        )

        attribute_designator = (
            Keyword("Valid")
            | Keyword("Present")
            | Keyword("Length")
            | Keyword("Head")
            | Keyword("Opaque")
        )

        attribute = Literal("'").suppress() - attribute_designator
        attribute.setParseAction(lambda t: (t[0], None))

        field = Literal(".").suppress() - unqualified_identifier()
        field.setParseAction(lambda t: ("Field", t[0]))

        binding = Keyword("where").suppress() + terms
        binding.setParseAction(lambda t: ("Binding", t[0]))

        aggregate = Literal("'").suppress() + lpar + components + rpar
        aggregate.setParseAction(lambda t: ("Aggregate", t[0]))

        suffix = binding ^ attribute ^ field ^ aggregate

        op_comp = Keyword("<") | Keyword(">") | Keyword("=") | Keyword("/=")

        op_add_sub = Keyword("+") | Keyword("-")

        op_mul_div = Keyword("*") | Keyword("/")

        op_set = (Keyword("not") + Keyword("in")).setParseAction(lambda t: ["not in"]) | Keyword(
            "in"
        )

        expression <<= infixNotation(
            atom,
            [
                (suffix, 1, opAssoc.LEFT, cls.__parse_suffix),
                (op_set, 2, opAssoc.LEFT, lambda t: cls.__parse_op_set(t[0])),
                (op_mul_div, 2, opAssoc.LEFT, lambda t: cls.__parse_op_mul_div(t[0])),
                (op_add_sub, 2, opAssoc.LEFT, lambda t: cls.__parse_op_add_sub(t[0])),
                (op_comp, 2, opAssoc.LEFT, lambda t: cls.__parse_op_comp(t[0])),
                (Keyword("and").suppress(), 2, opAssoc.LEFT, lambda t: And(*t[0])),
                (Keyword("or").suppress(), 2, opAssoc.LEFT, lambda t: Or(*t[0])),
            ],
        )

        expression.enablePackrat()
        return expression

    @classmethod
    def condition(cls) -> Token:
        return cls.expression() + StringEnd()

    @classmethod
    def action(cls) -> Token:

        lpar, rpar = map(Suppress, "()")

        parameters = lpar + delimitedList(cls.expression(), delim=",") + rpar

        call = cls.__identifier() + parameters
        call.setParseAction(cls.__parse_call)

        erase = cls.__identifier() + Literal(":=").suppress() + Keyword("null")
        erase.setParseAction(lambda t: Erase(t[0]))

        assignment = cls.__identifier() + Literal(":=").suppress() + cls.expression()
        assignment.setParseAction(lambda t: Assignment(t[0], t[1]))

        attribute_designator = Keyword("Append") | Keyword("Extend")

        list_operation = (
            cls.__identifier() + Literal("'").suppress() + attribute_designator + parameters
        )
        list_operation.setParseAction(
            lambda t: Assignment(t[0], SubprogramCall(t[1], [Variable(t[0]), t[2]]))
        )

        list_reset = cls.__identifier() + Literal("'").suppress() + Keyword("Reset")
        list_reset.setParseAction(lambda t: Reset(t[0]))

        return erase | assignment | list_reset | list_operation | call

    @classmethod
    def declaration(cls) -> Token:

        lpar, rpar = map(Suppress, "()")

        parameter = unqualified_identifier() + Literal(":").suppress() + cls.__identifier()
        parameter.setParseAction(lambda t: Argument(t[0], t[1]))

        parameter_list = lpar + delimitedList(parameter, delim=";") + rpar

        function_decl = (
            unqualified_identifier()
            + Optional(parameter_list)
            + Keyword("return").suppress()
            + cls.__identifier()
        )
        function_decl.setParseAction(lambda t: (t[0], Subprogram(t[1:-1], t[-1])))

        initializer = Literal(":=").suppress() + cls.expression()

        variable_decl = (
            unqualified_identifier()
            + Literal(":").suppress()
            + cls.__identifier()
            + Optional(initializer)
        )
        variable_decl.setParseAction(
            lambda t: (t[0], VariableDeclaration(t[1], t[2] if t[2:] else None))
        )

        return variable_decl | function_decl
