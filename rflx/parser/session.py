from typing import Any, List

from pyparsing import (
    Forward,
    Keyword,
    Literal,
    Optional,
    ParseException,
    ParseFatalException,
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

from rflx.declaration import (
    Argument,
    PrivateDeclaration,
    RenamingDeclaration,
    SubprogramDeclaration,
    VariableDeclaration,
)
from rflx.error import Location, Severity, Subsystem, fail, parser_location
from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    Binding,
    Call,
    Comprehension,
    Conversion,
    Div,
    Equal,
    Expr,
    ForAllIn,
    ForSomeIn,
    Greater,
    Head,
    In,
    Length,
    Less,
    MessageAggregate,
    Mul,
    NotEqual,
    NotIn,
    Opaque,
    Or,
    Present,
    Selected,
    String,
    Sub,
    Valid,
    Variable,
)
from rflx.identifier import ID
from rflx.parser.grammar import (
    boolean_literal,
    numeric_literal,
    qualified_identifier,
    unqualified_identifier,
)
from rflx.statement import Assignment, Declaration, Erase, Reset, Statement


def __parse_quantifier(tokens: List[Expr]) -> Expr:
    assert isinstance(tokens[1], ID)
    if tokens[0] == "all":
        return ForAllIn(tokens[1], tokens[2], tokens[3])
    return ForSomeIn(tokens[1], tokens[2], tokens[3])


def __parse_comprehension(tokens: List[Expr]) -> Expr:
    assert isinstance(tokens[0], ID)
    condition = tokens[3] if len(tokens) > 3 else TRUE
    return Comprehension(tokens[0], tokens[1], tokens[2], condition)


def __parse_call(tokens: List[Expr]) -> Expr:
    assert isinstance(tokens[0], ID)
    return Call(tokens[0], tokens[1:])


def __parse_conversion(tokens: List[Expr]) -> Expr:
    assert isinstance(tokens[0], ID)
    return Conversion(tokens[0], tokens[1])


def __parse_op_comp(tokens: List[Expr]) -> Expr:
    if tokens[1] == "<":
        return Less(tokens[0], tokens[2])
    if tokens[1] == ">":
        return Greater(tokens[0], tokens[2])
    if tokens[1] == "=":
        return Equal(tokens[0], tokens[2])
    if tokens[1] == "/=":
        return NotEqual(tokens[0], tokens[2])
    assert False, f"Unsupported comparison operator {tokens[1]}"


def __parse_op_set(tokens: List[Expr]) -> Expr:
    if tokens[1] == "not in":
        return NotIn(tokens[0], tokens[2])
    if tokens[1] == "in":
        return In(tokens[0], tokens[2])
    assert False, f"Unsupported set operator {tokens[1]}"


def __parse_op_add_sub(tokens: List[Expr]) -> Expr:
    result = tokens[0]
    for op, right in zip(tokens[1::2], tokens[2::2]):
        if op == "+":
            result = Add(result, right)
        elif op == "-":
            result = Sub(result, right)
        else:
            assert False, f"Unsupported add/sub operator {op}"
    return result


def __parse_variable(tokens: List[ID]) -> Expr:
    assert len(tokens) > 0 and len(tokens) < 3
    assert tokens[0].location
    assert tokens[-1].location
    locn = Location(start=tokens[0].location.start, end=tokens[-1].location.end)
    if len(tokens) == 2:
        return Selected(Variable(tokens[0]), tokens[1], location=locn)
    return Variable(tokens[0], location=locn)


def __parse_op_mul_div(tokens: List[Expr]) -> Expr:
    result = tokens[0]
    for op, right in zip(tokens[1::2], tokens[2::2]):
        if op == "*":
            result = Mul(result, right)
        elif op == "/":
            result = Div(result, right)
        else:
            assert False, f"Unsupported mul/div operator {op}"
    return result


def __parse_suffix(data: List[Any]) -> Expr:
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
        if suffix[0] == "Selected":
            result = Selected(result, suffix[1])
        if suffix[0] == "Binding":
            result = Binding(result, suffix[1])

    return result


def __variable() -> Token:
    result = delimitedList(unqualified_identifier(), ".")
    result.setParseAction(__parse_variable)
    return result


def __expression() -> Token:  # pylint: disable=too-many-locals

    bool_literal = boolean_literal()
    bool_literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

    string_literal = (
        Literal('"').suppress() + Word(printables + " ", excludeChars='"') + Literal('"').suppress()
    )
    string_literal.setParseAction(lambda t: String(t[0]))

    expr = Forward()

    parameters = delimitedList(expr, delim=",")

    lpar, rpar = map(Suppress, "()")

    function_call = unqualified_identifier() + lpar + parameters + rpar
    function_call.setParseAction(__parse_call)

    conversion = qualified_identifier() + lpar + expr + rpar
    conversion.setParseAction(__parse_conversion)

    quantifier = (
        Keyword("for").suppress()
        - oneOf(["all", "some"])
        + unqualified_identifier()
        - Keyword("in").suppress()
        + expr
        - Keyword("=>").suppress()
        + expr
    )
    quantifier.setParseAction(__parse_quantifier)

    comprehension = (
        Literal("[").suppress()
        - Keyword("for").suppress()
        + unqualified_identifier()
        - Keyword("in").suppress()
        + expr
        - Keyword("=>").suppress()
        + expr
        + Optional(Keyword("when").suppress() + expr)
        - Literal("]").suppress()
    )
    comprehension.setParseAction(__parse_comprehension)

    null_message = Keyword("null") + Keyword("message")
    null_message.setParseAction(lambda t: {})

    components = delimitedList(
        unqualified_identifier() + Keyword("=>").suppress() + expr, delim=","
    )
    components.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

    terms = delimitedList(unqualified_identifier() + Keyword("=").suppress() + expr, delim=",")
    terms.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

    aggregate = (
        qualified_identifier() + Literal("'").suppress() + lpar + (null_message | components) + rpar
    )
    aggregate.setParseAction(lambda t: MessageAggregate(t[0], t[1]))

    atom = (
        numeric_literal()
        | bool_literal
        | string_literal
        | quantifier
        | comprehension
        | function_call
        | conversion
        | aggregate
        | __variable()
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
    field.setParseAction(lambda t: ("Selected", t[0]))

    binding = Keyword("where").suppress() + terms
    binding.setParseAction(lambda t: ("Binding", t[0]))

    suffix = binding ^ attribute ^ field

    op_comp = Keyword("<") | Keyword(">") | Keyword("=") | Keyword("/=")

    op_add_sub = Keyword("+") | Keyword("-")

    op_mul_div = Keyword("*") | Keyword("/")

    op_set = (Keyword("not") + Keyword("in")).setParseAction(lambda t: ["not in"]) | Keyword("in")

    expr <<= infixNotation(
        atom,
        [
            (suffix, 1, opAssoc.LEFT, __parse_suffix),
            (op_set, 2, opAssoc.LEFT, lambda t: __parse_op_set(t[0])),
            (op_mul_div, 2, opAssoc.LEFT, lambda t: __parse_op_mul_div(t[0])),
            (op_add_sub, 2, opAssoc.LEFT, lambda t: __parse_op_add_sub(t[0])),
            (op_comp, 2, opAssoc.LEFT, lambda t: __parse_op_comp(t[0])),
            (Keyword("and").suppress(), 2, opAssoc.LEFT, lambda t: And(*t[0])),
            (Keyword("or").suppress(), 2, opAssoc.LEFT, lambda t: Or(*t[0])),
        ],
    )

    return expr


def __full_expression() -> Token:
    return __expression() + StringEnd()


def expression(data: str) -> Expr:
    return __full_expression().parseString(data)[0]


def __action() -> Token:

    lpar, rpar = map(Suppress, "()")

    parameters = lpar + delimitedList(__expression(), delim=",") + rpar

    call = unqualified_identifier() + parameters
    call.setParseAction(__parse_call)

    erase = unqualified_identifier() + Literal(":=").suppress() + Keyword("null")
    erase.setParseAction(lambda t: Erase(t[0]))

    assignment = unqualified_identifier() + Literal(":=").suppress() + __expression()
    assignment.setParseAction(lambda t: Assignment(t[0], t[1]))

    attribute_designator = Keyword("Append") | Keyword("Extend")

    list_operation = (
        unqualified_identifier() + Literal("'").suppress() + attribute_designator + parameters
    )
    list_operation.setParseAction(lambda t: Assignment(t[0], Call(t[1], [Variable(t[0]), t[2]])))

    list_reset = unqualified_identifier() + Literal("'").suppress() + Keyword("Reset")
    list_reset.setParseAction(lambda t: Reset(t[0]))

    return (erase | assignment | list_reset | list_operation | call) + StringEnd()


def action(data: str) -> Statement:
    try:
        result = __action().parseString(data)[0]
    except (ParseException, ParseFatalException) as e:
        fail(e.msg, Subsystem.PARSER, Severity.ERROR, parser_location(e.loc, e.loc, e.pstr))
    return result


def __declaration() -> Token:

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
    function_decl.setParseAction(lambda t: SubprogramDeclaration(t[0], t[1:-1], t[-1]))

    initializer = Literal(":=").suppress() + __expression()

    variable_base_decl = unqualified_identifier() + Literal(":").suppress() + qualified_identifier()

    variable_decl = variable_base_decl + Optional(initializer)
    variable_decl.setParseAction(lambda t: VariableDeclaration(t[0], t[1], t[2] if t[2:] else None))

    renames = variable_base_decl + Keyword("renames").suppress() + __variable()
    renames.setParseAction(lambda t: RenamingDeclaration(t[0], t[1], t[2]))

    private = unqualified_identifier() + Keyword("is").suppress() + Keyword("private").suppress()
    private.setParseAction(lambda t: PrivateDeclaration(t[0]))

    return (private | renames | variable_decl | function_decl) + StringEnd()


def declaration(data: str) -> Declaration:
    try:
        result = __declaration().parseString(data)[0]
    except (ParseException, ParseFatalException) as e:
        fail(e.msg, Subsystem.PARSER, Severity.ERROR, parser_location(e.loc, e.loc, e.pstr))
    return result
