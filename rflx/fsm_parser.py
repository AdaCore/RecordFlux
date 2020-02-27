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

from rflx.error import Location
from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    Argument,
    Div,
    Equal,
    Expr,
    Greater,
    Length,
    Less,
    Mul,
    NotEqual,
    Or,
    PrivateDeclaration,
    Renames,
    Sub,
    Subprogram,
    Variable,
    VariableDeclaration,
)
from rflx.fsm_expression import (
    Binding,
    Comprehension,
    Contains,
    Conversion,
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
    def __parse_conversion(cls, tokens: List[Expr]) -> Expr:
        assert isinstance(tokens[0], ID)
        if tokens[0] in map(ID, ["Read", "Write", "Call", "Data_Available"]):
            return SubprogramCall(tokens[0], tokens[1:])
        return Conversion(tokens[0], tokens[1])

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
    def __parse_variable(cls, tokens: List[ID]) -> Expr:
        assert len(tokens) > 0 and len(tokens) < 3
        assert tokens[0].location
        assert tokens[-1].location
        locn = Location(start=tokens[0].location.start, end=tokens[-1].location.end)
        if len(tokens) == 2:
            return Field(Variable(tokens[0]), tokens[1], location=locn)
        return Variable(tokens[0], location=locn)

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

        return result

    @classmethod
    def __variable(cls) -> Token:
        result = delimitedList(unqualified_identifier(), ".")
        result.setParseAction(cls.__parse_variable)
        return result

    @classmethod
    def __expression(cls) -> Token:  # pylint: disable=too-many-locals

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

        function_call = unqualified_identifier() + lpar + parameters + rpar
        function_call.setParseAction(cls.__parse_call)

        conversion = qualified_identifier() + lpar + expression + rpar
        conversion.setParseAction(cls.__parse_conversion)

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

        null_message = Keyword("null") + Keyword("message")
        null_message.setParseAction(lambda t: {})

        components = delimitedList(
            unqualified_identifier() + Keyword("=>").suppress() + expression, delim=","
        )
        components.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

        terms = delimitedList(
            unqualified_identifier() + Keyword("=").suppress() + expression, delim=","
        )
        terms.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

        aggregate = (
            qualified_identifier()
            + Literal("'").suppress()
            + lpar
            + (null_message | components)
            + rpar
        )
        aggregate.setParseAction(lambda t: MessageAggregate(t[0], t[1]))

        atom = (
            numeric_literal()
            | bool_literal
            | string_literal
            | quantifier
            | comprehension
            | conversion
            | function_call
            | aggregate
            | cls.__variable()
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

        suffix = binding ^ attribute ^ field

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
    def expression(cls) -> Token:
        return cls.__expression() + StringEnd()

    @classmethod
    def action(cls) -> Token:

        lpar, rpar = map(Suppress, "()")

        parameters = lpar + delimitedList(cls.__expression(), delim=",") + rpar

        call = unqualified_identifier() + parameters
        call.setParseAction(cls.__parse_call)

        erase = unqualified_identifier() + Literal(":=").suppress() + Keyword("null")
        erase.setParseAction(lambda t: Erase(t[0]))

        assignment = unqualified_identifier() + Literal(":=").suppress() + cls.__expression()
        assignment.setParseAction(lambda t: Assignment(t[0], t[1]))

        attribute_designator = Keyword("Append") | Keyword("Extend")

        list_operation = (
            unqualified_identifier() + Literal("'").suppress() + attribute_designator + parameters
        )
        list_operation.setParseAction(
            lambda t: Assignment(t[0], SubprogramCall(t[1], [Variable(t[0]), t[2]]))
        )

        list_reset = unqualified_identifier() + Literal("'").suppress() + Keyword("Reset")
        list_reset.setParseAction(lambda t: Reset(t[0]))

        return (erase | assignment | list_reset | list_operation | call) + StringEnd()

    @classmethod
    def declaration(cls) -> Token:

        lpar, rpar = map(Suppress, "()")

        parameter = unqualified_identifier() + Literal(":").suppress() + qualified_identifier()
        parameter.setParseAction(lambda t: Argument(t[0], t[1]))

        parameter_list = lpar + delimitedList(parameter, delim=";") + rpar

        function_decl = (
            unqualified_identifier()
            + Optional(parameter_list)
            + Keyword("return").suppress()
            + qualified_identifier()
        )
        function_decl.setParseAction(lambda t: (t[0], Subprogram(t[1:-1], t[-1])))

        initializer = Literal(":=").suppress() + cls.__expression()

        variable_base_decl = (
            unqualified_identifier() + Literal(":").suppress() + qualified_identifier()
        )

        variable_decl = variable_base_decl + Optional(initializer)
        variable_decl.setParseAction(
            lambda t: (t[0], VariableDeclaration(t[1], t[2] if t[2:] else None))
        )

        renames = variable_base_decl + Keyword("renames").suppress() + cls.__variable()
        renames.setParseAction(lambda t: (t[0], Renames(t[1], t[2])))

        private = (
            unqualified_identifier() + Keyword("is").suppress() + Keyword("private").suppress()
        )
        private.setParseAction(lambda t: (t[0], PrivateDeclaration()))

        return (private | renames | variable_decl | function_decl) + StringEnd()
