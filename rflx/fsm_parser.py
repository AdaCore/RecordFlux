from pyparsing import Keyword, Token

from rflx.expression import FALSE, TRUE, Equal, Variable
from rflx.identifier import ID
from rflx.parser.grammar import boolean_literal, qualified_identifier


class FSMParser:
    @classmethod
    def expr(cls) -> Token:
        boolean = boolean_literal().setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)
        identifier = qualified_identifier().setParseAction(
            lambda t: Variable(ID("".join(map(str, t.asList()))))
        )
        return boolean | identifier

    @classmethod
    def logical_equation(cls) -> Token:
        result = cls.expr() + Keyword("=") + cls.expr()
        return result.setParseAction(lambda t: Equal(t[0], t[2]))

    @classmethod
    def condition(cls) -> Token:
        return cls.logical_equation()
