from typing import Any, List

from pyparsing import (
    Forward,
    Keyword,
    Literal,
    StringEnd,
    Suppress,
    Token,
    delimitedList,
    infixNotation,
    oneOf,
    opAssoc,
)

from rflx.expression import (
    FALSE,
    TRUE,
    And,
    Equal,
    Expr,
    Greater,
    Length,
    Less,
    NotEqual,
    Or,
    Variable,
)
from rflx.fsm_expression import (
    Binding,
    Comprehension,
    Contains,
    Field,
    ForAll,
    ForSome,
    FunctionCall,
    Head,
    MessageAggregate,
    NotContains,
    Present,
    Valid,
)
from rflx.identifier import ID
from rflx.parser.grammar import (
    boolean_literal,
    numeric_literal,
    qualified_identifier,
    unqualified_identifier,
)
from rflx.statement import Assignment


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
        return Comprehension(tokens[0], tokens[1], tokens[2], tokens[3])

    @classmethod
    def __parse_function_call(cls, tokens: List[Expr]) -> Expr:
        assert isinstance(tokens[0], ID)
        return FunctionCall(tokens[0], tokens[1:])

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
    def __parse_suffix(cls, data: List[Any]) -> Expr:
        result = data[0][0]
        for suffix in data[0][1:]:
            if suffix[0] == "Head":
                result = Head(result)
            if suffix[0] == "Valid":
                result = Valid(result)
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

        literal = boolean_literal()
        literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

        expression = Forward()

        parameters = delimitedList(expression, delim=",")

        lpar, rpar = map(Suppress, "()")
        function_call = cls.__identifier() + lpar + parameters + rpar
        function_call.setParseAction(cls.__parse_function_call)

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
            - Keyword("when").suppress()
            + expression
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

        atom = numeric_literal() | literal | quantifier | comprehension | function_call | variable

        attribute_designator = (
            Keyword("Valid") | Keyword("Present") | Keyword("Length") | Keyword("Head")
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

        op_set = (Keyword("not") + Keyword("in")).setParseAction(lambda t: ["not in"]) | Keyword(
            "in"
        )

        expression <<= infixNotation(
            atom,
            [
                (suffix, 1, opAssoc.LEFT, cls.__parse_suffix),
                (op_comp, 2, opAssoc.LEFT, lambda t: cls.__parse_op_comp(t[0])),
                (op_set, 2, opAssoc.LEFT, lambda t: cls.__parse_op_set(t[0])),
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
        action = cls.__identifier() + Keyword(":=").suppress() + cls.expression() + StringEnd()
        action.setParseAction(lambda t: Assignment(t[0], t[1]))
        return action
