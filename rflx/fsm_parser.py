from typing import List

from pyparsing import Keyword, Literal, StringEnd, Token, infixNotation, opAssoc

from rflx.expression import FALSE, TRUE, And, Equal, Expr, NotEqual, Or, Variable
from rflx.fsm_expression import Contains, NotContains, Valid
from rflx.identifier import ID
from rflx.parser.grammar import boolean_literal, numeric_literal, qualified_identifier


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
    def __parse_conjunction(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        return And(*t)

    @classmethod
    def __parse_disjunction(cls, tokens: List[List[Expr]]) -> Expr:
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
    def expression(cls) -> Token:
        literal = boolean_literal()
        literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

        identifier = qualified_identifier()
        identifier.setParseAction(lambda t: Variable(ID("".join(map(str, t.asList())))))

        valid = identifier() + Literal("'") - Keyword("Valid")
        valid.setParseAction(lambda t: Valid(t[0]))

        equation = infixNotation(
            numeric_literal() | literal | valid | identifier,
            [
                (Keyword("="), 2, opAssoc.LEFT, cls.__parse_equation),
                (Keyword("/="), 2, opAssoc.LEFT, cls.__parse_inequation),
                (Keyword("in"), 2, opAssoc.LEFT, cls.__parse_in),
                (Keyword("not in"), 2, opAssoc.LEFT, cls.__parse_notin),
            ],
        )

        result = infixNotation(
            equation,
            [
                (Keyword("and").suppress(), 2, opAssoc.LEFT, cls.__parse_conjunction),
                (Keyword("or").suppress(), 2, opAssoc.LEFT, cls.__parse_disjunction),
            ],
        )
        return result

    @classmethod
    def condition(cls) -> Token:
        return cls.expression() + StringEnd()
