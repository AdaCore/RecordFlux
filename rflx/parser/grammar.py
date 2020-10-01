# pylint: disable=too-many-lines
import traceback
import typing
from typing import Callable, Dict, List, Mapping, Tuple, Union

from pyparsing import (
    CaselessKeyword,
    Forward,
    Group,
    Keyword,
    Literal,
    OneOrMore,
    Optional,
    ParseFatalException,
    ParserElement,
    ParseResults,
    QuotedString,
    Regex,
    StringEnd,
    Suppress,
    Token,
    ZeroOrMore,
    delimitedList,
    infixNotation,
    locatedExpr,
    oneOf,
    opAssoc,
)

from rflx import declaration as decl, statement as stmt
from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail, parser_location
from rflx.expression import (
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    AssExpr,
    Attribute,
    Binding,
    BinExpr,
    BooleanTrue,
    Call,
    Comprehension,
    Conversion,
    Div,
    Equal,
    Expr,
    First,
    ForAllIn,
    ForSomeIn,
    Greater,
    GreaterEqual,
    Head,
    In,
    Last,
    Length,
    Less,
    LessEqual,
    MessageAggregate,
    Mul,
    NotEqual,
    NotIn,
    Number,
    Opaque,
    Or,
    Pow,
    Present,
    QuantifiedExpression,
    Relation,
    Selected,
    String,
    Sub,
    Valid,
    ValidChecksum,
    ValueRange,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    Enumeration,
    ModularInteger,
    Private,
    RangeInteger,
    State,
    Transition,
    Type,
    qualified_type_name,
)

from . import const
from .ast import (
    ArraySpec,
    Component,
    ContextSpec,
    DerivationSpec,
    MessageSpec,
    PackageSpec,
    ReferenceSpec,
    RefinementSpec,
    SessionSpec,
    Specification,
    Then,
)

ParserElement.enablePackrat()


def comma() -> Token:
    return Suppress(Literal(",")).setName('","')


def semicolon() -> Token:
    return Suppress(Literal(";")).setName('";"')


def left_parenthesis() -> Token:
    return Suppress(Literal("(")).setName('"("')


def right_parenthesis() -> Token:
    return Suppress(Literal(")")).setName('")"')


def unqualified_identifier() -> Token:
    return locatedExpr(Regex(r"[a-zA-Z]\w*")).setParseAction(parse_identifier).setName("Identifier")


def qualified_identifier() -> Token:
    return (
        locatedExpr(Regex(r"[a-zA-Z]\w*(::[a-zA-Z]\w*)*"))
        .setParseAction(parse_identifier)
        .setName("QualifiedIdentifier")
    )


def variable() -> Token:
    return Group(qualified_identifier()).setParseAction(parse_variable)


def qualified_variable() -> Token:
    return Group(qualified_identifier()).setParseAction(
        lambda t: Variable(t[0][0], location=t[0][0].location)
    )


def numeric_literal() -> Token:
    numeral = Regex(r"\d+(_?\d+)*")
    numeral.setParseAction(lambda t: t[0].replace("_", ""))

    decimal_literal = Group(numeral)
    decimal_literal.setParseAction(lambda t: (int(t[0][0]), 0))

    based_numeral = Regex(r"[0-9A-F]+(_?[0-9A-F]+)*")
    based_numeral.setParseAction(lambda t: t[0].replace("_", ""))

    based_literal = numeral + Literal("#") - based_numeral - Literal("#")
    based_literal.setParseAction(lambda t: (int(t[2], int(t[0])), int(t[0])))

    sign = Literal("-")
    sign.setParseAction(lambda t: -1)

    num_literal = based_literal | decimal_literal
    num_literal.setName("Number")

    return locatedExpr(Optional(sign, 1) + num_literal).setParseAction(
        lambda s, l, t: Number(
            t[0][1] * t[0][2][0], t[0][2][1], parser_location(t[0][0], t[0][-1], s)
        )
    )


def string_literal() -> Token:
    return locatedExpr(QuotedString('"')).setParseAction(
        lambda s, l, t: String(t[0][1], location=parser_location(t[0][0], t[0][2], s))
    )


def boolean_expression(restricted: bool = False) -> Token:
    return (
        Group(expression(restricted))
        .setParseAction(parse_boolean_expression)
        .setName("BooleanExpression")
    )


def mathematical_expression(restricted: bool = False) -> Token:
    return (
        Group(expression(restricted))
        .setParseAction(parse_mathematical_expression)
        .setName("MathematicalExpression")
    )


def expression(restricted: bool = False) -> Token:
    # pylint: disable=too-many-locals

    expr = Forward()

    highest_precedence_operator = Literal("**")
    multiplying_operator = Literal("*") | Literal("/")
    binary_adding_operator = Literal("+") | Literal("-")
    relational_operator = (
        Keyword("=") | Keyword("/=") | Keyword("<=") | Keyword("<") | Keyword(">=") | Keyword(">")
    )
    if not restricted:
        relational_operator |= Keyword("in") | Keyword("not in")
    boolean_operator = Keyword("and") | Keyword("or")

    designator = Keyword("First") | Keyword("Last") | Keyword("Length") | Keyword("Valid_Checksum")
    if not restricted:
        designator |= Keyword("Head") | Keyword("Opaque") | Keyword("Present") | Keyword("Valid")

    array_aggregate = (
        Literal("[").setParseAction(lambda s, l, t: l)
        + Optional(numeric_literal())
        + (comma() - numeric_literal()) * (0,)
        + Literal("]").setParseAction(lambda s, l, t: l)
    )
    array_aggregate.setParseAction(parse_array_aggregate)

    concatenation = (
        infixNotation(
            array_aggregate | string_literal(),
            [(Suppress(Keyword("&")), 2, opAssoc.LEFT, parse_concatenation)],
            lpar=left_parenthesis(),
            rpar=right_parenthesis(),
        )
    ).setName("Concatenation")

    quantified_expression = locatedExpr(
        Keyword("for").suppress()
        - oneOf(["all", "some"])
        - unqualified_identifier()
        - Keyword("in").suppress()
        - expr
        - Keyword("=>").suppress()
        - expr
    )
    quantified_expression.setParseAction(parse_quantified_expression)

    comprehension = locatedExpr(
        Literal("[").suppress()
        - Keyword("for").suppress()
        - unqualified_identifier()
        - Keyword("in").suppress()
        - expr
        - Keyword("=>").suppress()
        - expr
        - Optional(Keyword("when").suppress() + expr)
        - Literal("]").suppress()
    )
    comprehension.setParseAction(parse_comprehension)

    null_message = Keyword("null") + Keyword("message")
    null_message.setParseAction(lambda t: {})

    components = delimitedList(unqualified_identifier() + Keyword("=>").suppress() + expr)
    components.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

    message_aggregate = locatedExpr(
        qualified_identifier()
        + Literal("'").suppress()
        + left_parenthesis()
        + (null_message | components)
        + right_parenthesis()
    )
    message_aggregate.setParseAction(
        lambda s, l, t: MessageAggregate(
            t[0][1], t[0][2], location=parser_location(t[0][0], t[0][3], s)
        )
    )

    call = locatedExpr(
        unqualified_identifier() + left_parenthesis() + delimitedList(expr) + right_parenthesis()
    )
    call.setParseAction(
        lambda s, l, t: Call(t[0][1], t[0][2:-1], location=parser_location(t[0][0], t[0][-1], s))
    )

    conversion = locatedExpr(
        qualified_identifier() + left_parenthesis() + expr + right_parenthesis()
    )
    conversion.setParseAction(
        lambda s, l, t: Conversion(t[0][1], t[0][2], location=parser_location(t[0][0], t[0][-1], s))
    )

    attribute = Literal("'").suppress() - designator
    attribute.setParseAction(lambda t: (t[0], None))

    terms = delimitedList(unqualified_identifier() - Keyword("=").suppress() - expr)
    terms.setParseAction(lambda t: dict(zip(t[0::2], t[1::2])))

    binding = Keyword("where").suppress() - terms
    binding.setParseAction(lambda t: ("Binding", t[0]))

    selector = Literal(".").suppress() + unqualified_identifier()
    selector.setParseAction(lambda t: ("Selected", t[0]))

    suffix = attribute
    if not restricted:
        suffix |= binding | selector

    base = concatenation | numeric_literal() | string_literal()
    if not restricted:
        base |= quantified_expression | comprehension | call | conversion | message_aggregate
        base |= variable()
    else:
        base |= qualified_variable()

    expr <<= infixNotation(
        base,
        [
            (suffix, 1, opAssoc.LEFT, parse_suffix),
            (highest_precedence_operator, 2, opAssoc.LEFT, parse_mathematical_operator),
            (multiplying_operator, 2, opAssoc.LEFT, parse_mathematical_operator),
            (binary_adding_operator, 2, opAssoc.LEFT, parse_mathematical_operator),
            (relational_operator, 2, opAssoc.LEFT, parse_relational_operator),
            (boolean_operator, 2, opAssoc.LEFT, parse_boolean_operator),
        ],
        lpar=left_parenthesis(),
        rpar=right_parenthesis(),
    )

    return expr


def with_aspects(aspects: Token) -> Token:
    return (Keyword("with") - aspects).setParseAction(parse_aspects)


def if_condition(restricted: bool = False) -> Token:
    return (Keyword("if") - boolean_expression(restricted)).setParseAction(parse_condition)


def type_derivation_definition() -> Token:
    return (Keyword("new") - qualified_identifier()).setName("DerivationSpec")


def size_aspect() -> Token:
    return (
        Keyword("Size") - Keyword("=>") - mathematical_expression(restricted=True)
    ).setParseAction(parse_aspect)


def integer_type_definition() -> Token:
    range_type_definition = (
        Keyword("range")
        - mathematical_expression(restricted=True)
        - Suppress(Literal(".."))
        - mathematical_expression(restricted=True)
        - with_aspects(size_aspect())
    )
    range_type_definition.setName("RangeInteger")
    modular_type_definition = Keyword("mod") - mathematical_expression(restricted=True)
    modular_type_definition.setName("ModularInteger")

    return range_type_definition | modular_type_definition


def boolean_literal() -> Token:
    literal = Keyword("True") | Keyword("False")
    literal.setParseAction(lambda t: t[0] == "True")
    literal.setName("BooleanLiteral")

    return literal


def enumeration_type_definition() -> Token:
    enumeration_literal = unqualified_identifier()
    positional_enumeration = enumeration_literal + ZeroOrMore(comma() - enumeration_literal)
    positional_enumeration.setParseAction(
        lambda t: [(k, Number(v)) for v, k in enumerate(t.asList())]
    )
    element_value_association = enumeration_literal + Keyword("=>") - numeric_literal()
    element_value_association.setParseAction(lambda t: (t[0], t[2]))
    named_enumeration = element_value_association + ZeroOrMore(comma() - element_value_association)

    boolean_aspect_definition = Optional(Keyword("=>") - boolean_literal())
    boolean_aspect_definition.setParseAction(lambda t: (t if t else ["=>", True]))
    always_valid_aspect = Literal("Always_Valid") - boolean_aspect_definition
    always_valid_aspect.setParseAction(parse_aspect)

    return (
        Literal("(")
        - (named_enumeration | positional_enumeration)
        - Literal(")")
        - with_aspects(delimitedList(size_aspect() | always_valid_aspect))
    ).setName("Enumeration")


def array_type_definition() -> Token:
    return (Keyword("array of") + qualified_identifier()).setName("Array")


def value_range() -> Token:
    return (
        mathematical_expression(restricted=True)
        + Keyword("..")
        - mathematical_expression(restricted=True)
    ).setParseAction(lambda t: ValueRange(t[0], t[2], location=t[0].location))


def checksum_aspect() -> Token:
    checksum_association = (
        unqualified_identifier()
        - Keyword("=>")
        - (
            left_parenthesis()
            - delimitedList(value_range() | mathematical_expression(restricted=True))
            + right_parenthesis()
        ).setParseAction(lambda t: [t.asList()])
    ).setParseAction(lambda t: (t[0], t[2]))
    return (
        Keyword("Checksum")
        - Keyword("=>")
        - left_parenthesis()
        - delimitedList(checksum_association)
        - right_parenthesis()
    ).setParseAction(lambda t: {ID("Checksum"): dict(t[2:])})


def message_type_definition() -> Token:
    first_aspect = Keyword("First") - Keyword("=>") - mathematical_expression(restricted=True)
    first_aspect.setParseAction(parse_aspect)
    length_aspect = Keyword("Length") - Keyword("=>") - mathematical_expression(restricted=True)
    length_aspect.setParseAction(parse_aspect)

    then = locatedExpr(
        Keyword("then")
        - (Keyword("null") | unqualified_identifier())
        - Group(Optional(with_aspects(delimitedList(first_aspect | length_aspect))))
        - Group(Optional(if_condition(restricted=True)))
    )
    then.setParseAction(parse_then)
    then_list = ZeroOrMore(then)
    then_list.setParseAction(lambda t: [t.asList()])

    component_item = (
        ~Keyword("end")
        + ~CaselessKeyword("Message")
        - unqualified_identifier()
        + Literal(":")
        - qualified_identifier()
        - then_list
        - semicolon()
    )
    component_item.setParseAction(
        lambda t: Component(t[0], t[2], t[3]) if len(t) >= 4 else Component(t[0], t[2])
    )
    component_item.setName("Component")
    null_component_item = Keyword("null") - then - semicolon()
    null_component_item.setParseAction(lambda t: Component(None, None, [t[1]]))
    null_component_item.setName("NullComponent")
    component_list = Group(
        Optional(null_component_item) - component_item - ZeroOrMore(component_item)
    )
    component_list.setParseAction(lambda t: t.asList())

    return (
        Keyword("message")
        - component_list
        - Keyword("end message")
        + Optional(Keyword("with") + checksum_aspect())
        | Keyword("null message")
    ).setName("Message")


def type_declaration() -> Token:
    type_definition = (
        enumeration_type_definition()
        | integer_type_definition()
        | message_type_definition()
        | type_derivation_definition()
        | array_type_definition()
    )

    return (
        Keyword("type").setParseAction(lambda s, l, t: l)
        - unqualified_identifier()
        - Keyword("is")
        - type_definition
        - semicolon().setParseAction(lambda s, l, t: l)
    ).setParseAction(parse_type)


def type_refinement() -> Token:
    return (
        (
            Suppress(Keyword("for")).setParseAction(lambda s, l, t: l)
            - qualified_identifier()
            - Suppress(Keyword("use"))
            - left_parenthesis()
            - unqualified_identifier()
            - Suppress(Keyword("=>"))
            - qualified_identifier()
            - right_parenthesis()
            - Optional(if_condition(restricted=True))("condition")
            - semicolon().setParseAction(lambda s, l, t: l)
        )
        .setParseAction(parse_refinement)
        .setName("Refinement")
    )


def private_type_declaration() -> Token:
    return locatedExpr(
        Keyword("type").suppress()
        + unqualified_identifier()
        + Keyword("is").suppress()
        + Keyword("private").suppress()
    ).setParseAction(parse_private_type_declaration)


def channel_declaration() -> Token:
    return locatedExpr(
        unqualified_identifier()
        + Keyword(":").suppress()
        - Keyword("Channel").suppress()
        - Keyword("with").suppress()
        - delimitedList(Keyword("Readable") | Keyword("Writable")).setParseAction(
            lambda t: [t.asList()]
        )
    ).setParseAction(parse_channel_declaration)


def formal_function_declaration() -> Token:
    parameter = unqualified_identifier() + Keyword(":").suppress() + qualified_identifier()
    parameter.setParseAction(lambda t: decl.Argument(t[0], t[1]))

    parameter_list = left_parenthesis() + delimitedList(parameter, delim=";") + right_parenthesis()

    return locatedExpr(
        Keyword("with function").suppress()
        - unqualified_identifier()
        - Optional(parameter_list)
        - Keyword("return").suppress()
        - qualified_identifier()
    ).setParseAction(parse_formal_function_declaration)


def variable_base_declaration() -> Token:
    return unqualified_identifier() + Keyword(":").suppress() + qualified_identifier()


def variable_declaration() -> Token:
    initializer = Literal(":=").suppress() - expression()

    return locatedExpr(variable_base_declaration() - Optional(initializer)).setParseAction(
        parse_variable_declaration
    )


def renaming_declaration() -> Token:
    return locatedExpr(
        variable_base_declaration()
        + Keyword("renames").suppress()
        + unqualified_identifier()
        + Literal(".").suppress()
        + unqualified_identifier()
    ).setParseAction(parse_renaming_declaration)


def assignment_statement() -> Token:
    erase = locatedExpr(unqualified_identifier() + Keyword(":=").suppress() + Keyword("null"))
    erase.setParseAction(parse_erase)

    assignment = locatedExpr(unqualified_identifier() + Keyword(":=").suppress() + expression())
    assignment.setParseAction(parse_assignment)

    return erase | assignment


def attribute_statement() -> Token:
    designator = Keyword("Append") | Keyword("Extend")

    parameter = left_parenthesis() + expression() + right_parenthesis()

    list_attribute = locatedExpr(
        unqualified_identifier() + Literal("'").suppress() + designator + parameter
    )
    list_attribute.setParseAction(parse_list_attribute)

    reset = locatedExpr(unqualified_identifier() + Literal("'").suppress() + Keyword("Reset"))
    reset.setParseAction(parse_reset)

    return list_attribute | reset


def state() -> Token:
    declarations = OneOrMore(
        ~Keyword("begin") + (variable_declaration() | renaming_declaration()) - semicolon()
    )

    actions = ZeroOrMore((assignment_statement() | attribute_statement()) - semicolon())

    description_aspect = Keyword("Desc") - Keyword("=>") - QuotedString('"')
    description_aspect.setParseAction(parse_aspect)

    conditional_transition = locatedExpr(
        Suppress(Keyword("then"))
        + unqualified_identifier()
        + Optional(with_aspects(description_aspect), {})
        + if_condition()
    ).setParseAction(parse_transition)
    transition = locatedExpr(
        Suppress(Keyword("then"))
        - unqualified_identifier()
        - Optional(with_aspects(description_aspect), {})
    ).setParseAction(parse_transition)
    transitions = (ZeroOrMore(conditional_transition) + transition).setParseAction(
        lambda t: [t.asList()]
    )

    return (
        locatedExpr(
            Suppress(Keyword("state"))
            - unqualified_identifier()("identifier")
            + Suppress(Keyword("is"))
            + (
                Keyword("null state")
                | Optional(declarations, [])("declarations")
                + Suppress(Keyword("begin"))
                + Optional(actions, [])("actions")
                + Suppress(Keyword("transition"))
                + transitions("transitions")
                + Suppress(Keyword("end"))
                + unqualified_identifier()("end_identifier")
            )
        )
        .setParseAction(parse_state)
        .setName("State")
    )


def session_declaration() -> Token:
    parameters = OneOrMore(
        (private_type_declaration() | formal_function_declaration() | channel_declaration())
        + semicolon()
    )
    parameters.setParseAction(lambda t: [t.asList()])

    declarations = OneOrMore(
        ~Keyword("begin") + (renaming_declaration() | variable_declaration()) - semicolon()
    )
    declarations.setParseAction(lambda t: [t.asList()])

    initial_aspect = (
        Suppress(Keyword("Initial")) - Suppress(Keyword("=>")) - unqualified_identifier()
    )
    final_aspect = Suppress(Keyword("Final")) - Suppress(Keyword("=>")) - unqualified_identifier()
    session_aspects = Suppress(Keyword("with")) - initial_aspect - comma() - final_aspect
    session_aspects.setParseAction(lambda t: [t.asList()])

    states = ZeroOrMore(state() - semicolon()).setParseAction(lambda t: [t])

    return (
        locatedExpr(
            Suppress(Keyword("generic"))
            - Optional(parameters, [])
            - Suppress(Keyword("session"))
            - unqualified_identifier()
            - session_aspects
            - Suppress(Keyword("is"))
            - Optional(declarations, [])
            - Suppress(Keyword("begin"))
            - states
            - Suppress(Keyword("end"))
            - unqualified_identifier()
            - semicolon()
        )
        .setParseAction(parse_session)
        .setName("Session")
    )


def package_declaration() -> Token:
    basic_declaration = type_declaration() | type_refinement() | session_declaration()

    return (
        Keyword("package")
        - unqualified_identifier()
        - Keyword("is")
        - Group(ZeroOrMore(basic_declaration))
        - Keyword("end")
        - unqualified_identifier()
        - semicolon()
    ).setParseAction(parse_package)


def context_clause() -> Token:
    context_item = Keyword("with") - unqualified_identifier() - semicolon()
    context_item.setParseAction(lambda t: t[1])

    return ZeroOrMore(context_item).setParseAction(lambda t: ContextSpec(t.asList()))


def specification() -> Token:
    return (Optional(context_clause() + package_declaration())).setParseAction(
        lambda t: Specification(t[0], t[1]) if len(t) == 2 else None
    )


# pylint: disable=unused-argument
def fatalexceptions(parse_function: Callable) -> Callable:
    def wrapper(string: str, location: int, tokens: ParseResults) -> object:
        try:
            return parse_function(string, location, tokens)
        except (ParseFatalException, RecordFluxError) as e:
            raise e
        except Exception as e:
            raise ParseFatalException(string, location, traceback.format_exc()) from e

    return wrapper


@fatalexceptions
def parse_array_aggregate(string: str, location: int, tokens: ParseResults) -> Expr:
    return Aggregate(*tokens[1:-1], location=parser_location(tokens[0], tokens[-1], string))


@fatalexceptions
def parse_concatenation(string: str, location: int, tokens: ParseResults) -> Expr:
    return Aggregate(
        *[e for t in tokens[0] for e in t.elements],
        location=Location(
            tokens[0][0].location.start, tokens[0][0].location.source, tokens[0][-1].location.end
        ),
    )


@fatalexceptions
def parse_quantified_expression(
    string: str, location: int, tokens: ParseResults
) -> QuantifiedExpression:
    tokens, locn = evaluate_located_expression(string, tokens)

    if tokens[0] == "all":
        return ForAllIn(tokens[1], tokens[2], tokens[3], location=locn)
    if tokens[0] == "some":
        return ForSomeIn(tokens[1], tokens[2], tokens[3], location=locn)

    raise ParseFatalException(string, location, "unexpected quantified expression")


@fatalexceptions
def parse_comprehension(string: str, location: int, tokens: ParseResults) -> Comprehension:
    tokens, locn = evaluate_located_expression(string, tokens)
    condition = tokens[3] if len(tokens) > 3 else TRUE
    return Comprehension(tokens[0], tokens[1], tokens[2], condition, location=locn)


@fatalexceptions
def parse_relational_operator(string: str, location: int, tokens: ParseResults) -> Expr:
    operators: Mapping[str, typing.Type[Union[BinExpr, AssExpr]]] = {
        "=": Equal,
        "/=": NotEqual,
        "<": Less,
        "<=": LessEqual,
        ">=": GreaterEqual,
        ">": Greater,
        "in": In,
        "not in": NotIn,
    }
    return parse_operator(operators, string, location, tokens)


@fatalexceptions
def parse_boolean_operator(string: str, location: int, tokens: ParseResults) -> Expr:
    operators: Mapping[str, typing.Type[AssExpr]] = {
        "and": And,
        "or": Or,
    }
    return parse_operator(operators, string, location, tokens)


@fatalexceptions
def parse_mathematical_operator(string: str, location: int, tokens: ParseResults) -> Expr:
    operators: Mapping[str, typing.Type[Union[BinExpr, AssExpr]]] = {
        "+": Add,
        "-": Sub,
        "*": Mul,
        "/": Div,
        "**": Pow,
    }
    return parse_operator(operators, string, location, tokens)


def parse_operator(
    operators: Mapping[str, typing.Type[Union[BinExpr, AssExpr]]],
    string: str,
    location: int,
    tokens: ParseResults,
) -> Expr:
    result: List[object] = tokens[0]
    while len(result) > 1:
        left = result.pop(0)
        operator = result.pop(0)
        right = result.pop(0)

        assert isinstance(left, Expr)
        assert isinstance(operator, str)
        assert isinstance(right, Expr)
        assert left.location
        assert right.location
        assert left.location.source == right.location.source

        locn = Location(left.location.start, left.location.source, left.location.end)

        try:
            result_expr = operators[operator](left, right, location=locn)
        except KeyError:
            raise ParseFatalException(string, location, "unexpected operator") from None
        result.insert(0, result_expr)

    assert isinstance(result[0], Expr)
    return result[0]


@fatalexceptions
def parse_boolean_expression(string: str, location: int, tokens: ParseResults) -> Expr:
    bool_expr = tokens[0][0]
    if isinstance(bool_expr, (And, Or, Relation, Valid, ValidChecksum, QuantifiedExpression)):
        return bool_expr
    raise ParseFatalException(
        string, location, f'unexpected expression type "{type(bool_expr).__name__}"'
    )


@fatalexceptions
def parse_mathematical_expression(string: str, location: int, tokens: ParseResults) -> Expr:
    math_expr = tokens[0][0]
    if isinstance(math_expr, (Number, Variable, Add, Sub, Mul, Div, Pow, First, Last, Length)):
        return math_expr
    raise ParseFatalException(
        string, location, f'unexpected expression type "{type(math_expr).__name__}"'
    )


@fatalexceptions
def parse_then(string: str, location: int, tokens: ParseResults) -> Then:
    tokens = tokens[0]
    start = tokens.pop(0)
    locn = parser_location(start, tokens[-1], string)
    return Then(
        tokens[1] if tokens[1] != "null" else None,
        tokens[2][0]["first"] if tokens[2] and "first" in tokens[2][0] else UNDEFINED,
        tokens[2][0]["length"] if tokens[2] and "length" in tokens[2][0] else UNDEFINED,
        tokens[3][0] if tokens[3] else BooleanTrue(location=locn),
        locn,
    )


@fatalexceptions
def parse_identifier(string: str, location: int, tokens: ParseResults) -> ID:
    tokens, locn = evaluate_located_expression(string, tokens)

    if tokens.lower() in const.RESERVED_WORDS:
        fail(
            f'reserved word "{tokens}" used as identifier',
            Subsystem.PARSER,
            Severity.ERROR,
            locn,
        )

    return ID(tokens, locn)


@fatalexceptions
def parse_variable(string: str, location: int, tokens: ParseResults) -> Variable:
    identifier = tokens[0][0]

    assert 1 <= len(identifier.parts) <= 2
    assert identifier.location

    return Variable(identifier, location=identifier.location)


@fatalexceptions
def parse_suffix(string: str, location: int, tokens: ParseResults) -> Attribute:
    result = tokens[0][0]

    suffixes = {
        "First": First,
        "Last": Last,
        "Length": Length,
        "Head": Head,
        "Opaque": Opaque,
        "Present": Present,
        "Valid": Valid,
        "Valid_Checksum": ValidChecksum,
        "Selected": Selected,
        "Binding": Binding,
    }
    for suffix in tokens[0][1:]:
        try:
            s = suffix[0]
            if s in ("Selected", "Binding"):
                result = suffixes[s](result, suffix[1], location=result.location)
            else:
                result = suffixes[s](result)
        except KeyError:
            raise ParseFatalException(string, location, "unexpected suffix") from None

    return result


@fatalexceptions
def parse_aspect(string: str, location: int, tokens: ParseResults) -> Tuple[str, Expr]:
    return (tokens[0].lower(), tokens[2])


@fatalexceptions
def parse_aspects(string: str, location: int, tokens: ParseResults) -> Dict[str, Expr]:
    return dict(tokens[1:])


@fatalexceptions
def parse_condition(string: str, location: int, tokens: ParseResults) -> Expr:
    return tokens[1]


@fatalexceptions
def parse_type(string: str, location: int, tokens: ParseResults) -> Type:
    package = ID("__PACKAGE__")
    name = tokens[1]

    locn = parser_location(tokens[0], tokens[-1], string)

    identifier = package * name

    if tokens[3] == "mod":
        return ModularInteger(identifier, tokens[4], locn)
    if tokens[3] == "range":
        tokens[6] = tokens[6]["size"]
        return RangeInteger(identifier, tokens[4], tokens[5], tokens[6], locn)
    if tokens[3] == "message":
        aspects = tokens[7] if len(tokens) > 7 else {}
        return MessageSpec(identifier, tokens[4], aspects, locn)
    if tokens[3] == "null message":
        return MessageSpec(identifier, [], location=locn)
    if tokens[3] == "(":
        elements = tokens[4:-3]
        aspects = tokens[-2]
        if "always_valid" not in aspects:
            aspects["always_valid"] = False
        enumeration = Enumeration(
            identifier, elements, aspects["size"], aspects["always_valid"], locn
        )
        return enumeration
    if tokens[3] == "new":
        return DerivationSpec(identifier, tokens[4], locn)
    if tokens[3] == "array of":
        return ArraySpec(
            identifier,
            ReferenceSpec(qualified_type_name(tokens[4], package), tokens[4].location),
            locn,
        )

    raise ParseFatalException(string, location, "unexpected type")


@fatalexceptions
def parse_refinement(string: str, location: int, tokens: ParseResults) -> RefinementSpec:
    condition = tokens[4] if "condition" in tokens else TRUE
    locn = parser_location(tokens[0], tokens[-1], string)
    return RefinementSpec(tokens[1], tokens[2], tokens[3], condition, locn)


@fatalexceptions
def parse_private_type_declaration(
    string: str, location: int, tokens: ParseResults
) -> decl.TypeDeclaration:
    tokens, locn = evaluate_located_expression(string, tokens)
    return decl.TypeDeclaration(Private(tokens[0], location=locn))


@fatalexceptions
def parse_channel_declaration(
    string: str, location: int, tokens: ParseResults
) -> decl.ChannelDeclaration:
    tokens, locn = evaluate_located_expression(string, tokens)
    return decl.ChannelDeclaration(
        tokens[0], readable="Readable" in tokens[1], writable="Writable" in tokens[1], location=locn
    )


@fatalexceptions
def parse_formal_function_declaration(
    string: str, location: int, tokens: ParseResults
) -> decl.FunctionDeclaration:
    tokens, locn = evaluate_located_expression(string, tokens)
    return decl.FunctionDeclaration(tokens[0], tokens[1:-1], tokens[-1], location=locn)


@fatalexceptions
def parse_variable_declaration(
    string: str, location: int, tokens: ParseResults
) -> decl.VariableDeclaration:
    tokens, locn = evaluate_located_expression(string, tokens)
    return decl.VariableDeclaration(
        tokens[0], tokens[1], tokens[2] if len(tokens) > 2 else None, location=locn
    )


@fatalexceptions
def parse_renaming_declaration(
    string: str, location: int, tokens: ParseResults
) -> decl.RenamingDeclaration:
    tokens, locn = evaluate_located_expression(string, tokens)
    return decl.RenamingDeclaration(
        tokens[0], tokens[1], Selected(Variable(tokens[2]), tokens[3]), location=locn
    )


@fatalexceptions
def parse_erase(string: str, location: int, tokens: ParseResults) -> stmt.Erase:
    tokens, locn = evaluate_located_expression(string, tokens)
    return stmt.Erase(tokens[0], location=locn)


@fatalexceptions
def parse_assignment(string: str, location: int, tokens: ParseResults) -> stmt.Assignment:
    tokens, locn = evaluate_located_expression(string, tokens)
    return stmt.Assignment(tokens[0], tokens[1], location=locn)


@fatalexceptions
def parse_list_attribute(
    string: str, location: int, tokens: ParseResults
) -> Union[stmt.Append, stmt.Extend]:
    tokens, locn = evaluate_located_expression(string, tokens)
    if tokens[1] == "Append":
        return stmt.Append(tokens[0], tokens[2], location=locn)
    return stmt.Extend(tokens[0], tokens[2], location=locn)


@fatalexceptions
def parse_reset(string: str, location: int, tokens: ParseResults) -> stmt.Reset:
    tokens, locn = evaluate_located_expression(string, tokens)
    return stmt.Reset(tokens[0], location=locn)


@fatalexceptions
def parse_transition(string: str, location: int, tokens: ParseResults) -> Transition:
    tokens, locn = evaluate_located_expression(string, tokens)
    return Transition(
        tokens[0],
        condition=tokens[2] if len(tokens) > 2 else TRUE,
        description=tokens[1]["desc"] if "desc" in tokens[1] else None,
        location=locn,
    )


@fatalexceptions
def parse_state(string: str, location: int, tokens: ParseResults) -> State:
    tokens, locn = evaluate_located_expression(string, tokens)

    if "end_identifier" not in tokens:
        return State(tokens["identifier"], location=locn)

    identifier = tokens["identifier"]
    end_identifier = tokens["end_identifier"]

    if identifier != end_identifier:
        raise ParseFatalException(
            string, location, f"inconsistent state identifier: {identifier} /= {end_identifier}"
        )

    return State(
        tokens["identifier"],
        declarations=tokens["declarations"],
        actions=tokens["actions"],
        transitions=tokens["transitions"],
        location=locn,
    )


@fatalexceptions
def parse_session(string: str, location: int, tokens: ParseResults) -> object:
    tokens, locn = evaluate_located_expression(string, tokens)

    identifier = tokens[1]
    end_identifier = tokens[-1]

    if identifier != end_identifier:
        raise ParseFatalException(
            string, location, f"inconsistent session identifier: {identifier} /= {end_identifier}"
        )

    return SessionSpec(
        identifier,
        initial=tokens[2][0],
        final=tokens[2][1],
        parameters=tokens[0],
        declarations=tokens[3],
        states=tokens[4],
        location=locn,
    )


@fatalexceptions
def parse_package(string: str, location: int, tokens: ParseResults) -> object:
    declarations = tokens[3].asList()
    types = [d for d in declarations if isinstance(d, Type)]
    sessions = [d for d in declarations if isinstance(d, SessionSpec)]
    assert len(declarations) == len(types) + len(sessions)
    return PackageSpec(tokens[1], types, sessions, tokens[5])


def evaluate_located_expression(string: str, tokens: ParseResults) -> Tuple[ParseResults, Location]:
    data = tokens[0].asDict()
    location = parser_location(data["locn_start"], data["locn_end"], string)
    return (data["value"] if len(data) == 3 else data, location)


def unit() -> Token:
    return (specification() + StringEnd()).ignore(Regex(r"--.*"))
