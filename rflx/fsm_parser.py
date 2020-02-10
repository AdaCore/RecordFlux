from typing import List

from pyparsing import (
    Forward,
    Keyword,
    Literal,
    ParseFatalException,
    ParseResults,
    StringEnd,
    Suppress,
    Token,
    infixNotation,
    oneOf,
    opAssoc,
)

from rflx.expression import FALSE, TRUE, And, Equal, Expr, Length, Less, NotEqual, Or, Variable
from rflx.fsm_expression import (
    Attribute,
    Contains,
    Convert,
    Field,
    ForAll,
    ForSome,
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


def parse_attribute(string: str, location: int, tokens: ParseResults) -> Attribute:
    if tokens[2] == "Valid":
        return Valid(tokens[0])
    if tokens[2] == "Present":
        return Present(tokens[0])
    if tokens[2] == "Length":
        return Length(tokens[0])
    raise ParseFatalException(string, location, "unexpected attribute")


class FSMParser:
    @classmethod
    def __parse_less(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return Less(t[0], t[2])

    @classmethod
    def __parse_equation(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return Equal(t[0], t[2])

    @classmethod
    def __parse_inequation(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return NotEqual(t[0], t[2])

    @classmethod
    def __parse_conj(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return And(*t)

    @classmethod
    def __parse_disj(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return Or(*t)

    @classmethod
    def __parse_in(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return Contains(t[0], t[2])

    @classmethod
    def __parse_notin(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return NotContains(t[0], t[2])

    @classmethod
    def __parse_quantifier(cls, tokens: List[Expr]) -> Expr:
        if not isinstance(tokens[1], Variable):
            raise TypeError("quantifier not of type Variable")
        if tokens[0] == "all":
            return ForAll(tokens[1], tokens[2], tokens[3])
        return ForSome(tokens[1], tokens[2], tokens[3])

    @classmethod
    def __parse_conversion(cls, tokens: List[Expr]) -> Expr:
        if not isinstance(tokens[0], Variable):
            raise TypeError("target not of type Variable")
        return Convert(tokens[1], tokens[0])

    @classmethod
    def expression(cls) -> Token:
        literal = boolean_literal()
        literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

        identifier = qualified_identifier()
        identifier.setParseAction(lambda t: Variable(ID("".join(map(str, t.asList())))))

        attribute_designator = Keyword("Valid") | Keyword("Present") | Keyword("Length")

        lpar, rpar = map(Suppress, "()")
        conversion = identifier + lpar + identifier + rpar
        conversion.setParseAction(cls.__parse_conversion)

        field = conversion + Literal(".").suppress() - unqualified_identifier()
        field.setParseAction(lambda t: Field(t[0], t[1]))

        attribute = (field | conversion | identifier) + Literal("'") - attribute_designator
        attribute.setParseAction(parse_attribute)

        expression = Forward()

        quantifier = (
            Keyword("for").suppress()
            - oneOf(["all", "some"])
            + identifier
            - Keyword("in").suppress()
            + identifier
            - Keyword("=>").suppress()
            + expression
        )
        quantifier.setParseAction(cls.__parse_quantifier)

        atom = (
            numeric_literal() | literal | quantifier | attribute | field | conversion | identifier
        )

        expression <<= infixNotation(
            atom,
            [
                (Keyword("<"), 2, opAssoc.LEFT, cls.__parse_less),
                (Keyword("="), 2, opAssoc.LEFT, cls.__parse_equation),
                (Keyword("/="), 2, opAssoc.LEFT, cls.__parse_inequation),
                (Keyword("in"), 2, opAssoc.LEFT, cls.__parse_in),
                (Keyword("not in"), 2, opAssoc.LEFT, cls.__parse_notin),
                (Keyword("and").suppress(), 2, opAssoc.LEFT, cls.__parse_conj),
                (Keyword("or").suppress(), 2, opAssoc.LEFT, cls.__parse_disj),
            ],
        )

        expression.enablePackrat()
        return expression

    @classmethod
    def condition(cls) -> Token:
        return cls.expression() + StringEnd()
