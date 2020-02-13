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
    Attribute,
    Comprehension,
    Contains,
    Convert,
    Field,
    ForAll,
    ForSome,
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


def parse_attribute(string: str, location: int, tokens: ParseResults) -> Attribute:
    if tokens[1] == "Valid":
        return Valid(tokens[0])
    if tokens[1] == "Present":
        return Present(tokens[0])
    if tokens[1] == "Length":
        return Length(tokens[0])
    if tokens[1] == "Head":
        return Head(tokens[0])
    raise ParseFatalException(string, location, "unexpected attribute")


class FSMParser:
    @classmethod
    def __parse_less(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return Less(t[0], t[2])

    @classmethod
    def __parse_greater(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return Greater(t[0], t[2])

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
        assert isinstance(tokens[1], ID)
        if tokens[0] == "all":
            return ForAll(tokens[1], tokens[2], tokens[3])
        return ForSome(tokens[1], tokens[2], tokens[3])

    @classmethod
    def __parse_comprehension(cls, tokens: List[Expr]) -> Expr:
        assert isinstance(tokens[0], ID)
        return Comprehension(tokens[0], tokens[1], tokens[2], tokens[3])

    @classmethod
    def __parse_conversion(cls, tokens: List[Expr]) -> Expr:
        assert isinstance(tokens[0], ID)
        return Convert(tokens[1], tokens[0])

    @classmethod
    def __identifier(cls) -> Token:
        identifier = qualified_identifier()
        identifier.setParseAction(lambda t: ID("".join(map(str, t.asList()))))
        return identifier

    # pylint: disable=too-many-locals
    @classmethod
    def expression(cls) -> Token:
        literal = boolean_literal()
        literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

        attribute_designator = (
            Keyword("Valid") | Keyword("Present") | Keyword("Length") | Keyword("Head")
        )

        expression = Forward()

        lpar, rpar = map(Suppress, "()")
        conversion = cls.__identifier() + lpar + expression + rpar
        conversion.setParseAction(cls.__parse_conversion)

        field = conversion + Literal(".").suppress() - unqualified_identifier()
        field.setParseAction(lambda t: Field(t[0], t[1]))

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

        attribute = (
            (field | conversion | cls.__identifier() | comprehension)
            + Literal("'").suppress()
            - attribute_designator
        )
        attribute.setParseAction(parse_attribute)

        components = delimitedList(
            unqualified_identifier() + Keyword("=>").suppress() + expression, delim=","
        )
        components.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

        aggregate = (
            cls.__identifier() + Literal("'").suppress() + lpar + components + rpar
        ).setParseAction(lambda t: MessageAggregate(t[0], t[1]))

        attribute_field = attribute + Literal(".").suppress() + qualified_identifier()
        attribute_field.setParseAction(lambda t: Field(t[0], t[1]))

        variable = cls.__identifier()
        variable.setParseAction(lambda t: Variable(ID("".join(map(str, t.asList())))))

        atom = (
            numeric_literal()
            | literal
            | quantifier
            | aggregate
            | attribute_field
            | attribute
            | field
            | comprehension
            | conversion
            | variable
        )

        expression <<= infixNotation(
            atom,
            [
                (Keyword("<"), 2, opAssoc.LEFT, cls.__parse_less),
                (Keyword(">"), 2, opAssoc.LEFT, cls.__parse_greater),
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

    @classmethod
    def action(cls) -> Token:
        action = cls.__identifier() + Keyword(":=").suppress() + cls.expression() + StringEnd()
        action.setParseAction(lambda t: Assignment(t[0], t[1]))
        return action
