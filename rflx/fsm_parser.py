from typing import List

from pyparsing import Keyword, Literal, StringEnd, Token, infixNotation, opAssoc

from rflx.expression import FALSE, TRUE, Equal, Expr, NotEqual, Variable
from rflx.fsm_expression import Valid
from rflx.identifier import ID
from rflx.parser.grammar import boolean_literal, qualified_identifier


class FSMParser:
    @classmethod
    def __parse_equation(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return Equal(t[0], t[2])

    @classmethod
    def __parse_inequation(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return NotEqual(t[0], t[2])

    @classmethod
    def expression(cls) -> Token:
        literal = boolean_literal()
        literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

        identifier = qualified_identifier()
        identifier.setParseAction(lambda t: Variable(ID("".join(map(str, t.asList())))))

        valid = identifier() + Literal("'") - Keyword("Valid")
        valid.setParseAction(lambda t: Valid(t[0]))

        simple_expression = literal | valid | identifier

        equation = infixNotation(
            simple_expression,
            [
                (Keyword("="), 2, opAssoc.LEFT, cls.__parse_equation),
                (Keyword("/="), 2, opAssoc.LEFT, cls.__parse_inequation),
            ],
        )

        return equation | simple_expression

    @classmethod
    def condition(cls) -> Token:
        return cls.expression() + StringEnd()
