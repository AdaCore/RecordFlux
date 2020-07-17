from typing import Callable, Dict, List, Tuple

from pyparsing import (
    CaselessKeyword,
    Combine,
    Group,
    Keyword,
    Literal,
    Optional,
    ParseFatalException,
    ParserElement,
    ParseResults,
    QuotedString,
    Regex,
    StringEnd,
    Suppress,
    Token,
    Word,
    WordEnd,
    WordStart,
    ZeroOrMore,
    alphanums,
    alphas,
    delimitedList,
    infixNotation,
    locatedExpr,
    nums,
    opAssoc,
)

from rflx.error import Location, RecordFluxError, Severity, Subsystem, fail, parser_location
from rflx.expression import (
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    Attribute,
    BooleanTrue,
    Div,
    Equal,
    Expr,
    First,
    Greater,
    GreaterEqual,
    Last,
    Length,
    Less,
    LessEqual,
    Mul,
    NotEqual,
    Number,
    Or,
    Pow,
    Relation,
    Sub,
    Variable,
)
from rflx.identifier import ID
from rflx.model import Enumeration, ModularInteger, RangeInteger, Type, qualified_type_name

from .ast import (
    ArraySpec,
    Component,
    ContextSpec,
    DerivationSpec,
    MessageSpec,
    PackageSpec,
    ReferenceSpec,
    RefinementSpec,
    Specification,
    Then,
)

ParserElement.enablePackrat()


def comma() -> Token:
    return Suppress(Literal(",")).setName('","')


def semicolon() -> Token:
    return Suppress(Literal(";")).setName('";"')


def unqualified_identifier() -> Token:
    return (
        locatedExpr(WordStart(alphas) + Word(alphanums + "_") + WordEnd(alphanums + "_"))
        .setParseAction(verify_identifier)
        .setName("Identifier")
    )


def qualified_identifier() -> Token:
    return (
        Optional(unqualified_identifier() + Literal(".")) + unqualified_identifier()
    ).setParseAction(
        lambda t: ID(
            "".join(map(str, t.asList())),
            Location(start=t[0].location.start, end=t[-1].location.end),
        )
    )


def attribute_reference() -> Token:
    designator = Keyword("First") | Keyword("Last") | Keyword("Length")
    reference = unqualified_identifier() + Literal("'") - designator
    reference.setParseAction(parse_attribute)
    return reference.setName("Attribute")


def numeric_literal() -> Token:
    numeral = Combine(Word(nums) + ZeroOrMore(Optional(Word("_")) + Word(nums)))
    numeral.setParseAction(lambda t: t[0].replace("_", ""))

    decimal_literal = Group(numeral)
    decimal_literal.setParseAction(lambda t: (int(t[0][0]), 0))

    extended_digit = Word(nums + "ABCDEF")
    based_numeral = Combine(extended_digit + ZeroOrMore(Optional("_") + extended_digit))
    based_numeral.setParseAction(lambda t: t[0].replace("_", ""))

    based_literal = numeral + Literal("#") - based_numeral - Literal("#")
    based_literal.setParseAction(lambda t: (int(t[2], int(t[0])), int(t[0])))

    num_literal = based_literal | decimal_literal
    num_literal.setName("Number")

    return locatedExpr(num_literal).setParseAction(
        lambda s, l, t: Number(t[0][1][0], t[0][1][1], parser_location(t[0][0], t[0][2], s))
    )


def logical_expression() -> Token:
    relational_operator = (
        Keyword("<=") | Keyword(">=") | Keyword("=") | Keyword("/=") | Keyword("<") | Keyword(">")
    )
    logical_operator = Keyword("and") | Keyword("or")

    relation = mathematical_expression() + relational_operator - mathematical_expression()
    relation.setParseAction(parse_relation)
    relation.setName("Relation")

    return (
        infixNotation(relation, [(logical_operator, 2, opAssoc.LEFT, parse_logical_expression)])
    ).setName("LogicalExpression")


def mathematical_expression() -> Token:
    binary_adding_operator = Literal("+") | Literal("-")
    multiplying_operator = Literal("*") | Literal("/")
    highest_precedence_operator = Literal("**")

    array_aggregate = (
        Literal("(").setParseAction(lambda s, l, t: l)
        + numeric_literal()
        + (comma() - numeric_literal()) * (0,)
        + Literal(")").setParseAction(lambda s, l, t: l)
    )
    array_aggregate.setParseAction(parse_array_aggregate)

    string = QuotedString('"')
    string.setParseAction(parse_string)

    concatenation = (
        infixNotation(
            array_aggregate | string,
            [(Suppress(Keyword("&")), 2, opAssoc.LEFT, parse_concatenation)],
        )
    ).setName("Concatenation")

    term = numeric_literal() | attribute_reference() | qualified_identifier() | concatenation
    term.setParseAction(parse_term)

    return (
        infixNotation(
            term,
            [
                (highest_precedence_operator, 2, opAssoc.LEFT, parse_mathematical_expression),
                (multiplying_operator, 2, opAssoc.LEFT, parse_mathematical_expression),
                (binary_adding_operator, 2, opAssoc.LEFT, parse_mathematical_expression),
            ],
        )
    ).setName("MathematicalExpression")


def value_constraint() -> Token:
    return (Keyword("if") - logical_expression()).setParseAction(lambda t: t[1])


def type_derivation_definition() -> Token:
    return (Keyword("new") - qualified_identifier()).setName("DerivationSpec")


def size_aspect() -> Token:
    return (Keyword("Size") - Keyword("=>") - mathematical_expression()).setParseAction(
        parse_aspect
    )


def integer_type_definition() -> Token:
    range_type_aspects = Keyword("with") - size_aspect()
    range_type_aspects.setParseAction(parse_aspects)

    range_type_definition = (
        Keyword("range")
        - mathematical_expression()
        - Suppress(Literal(".."))
        - mathematical_expression()
        - range_type_aspects
    )
    range_type_definition.setName("RangeInteger")
    modular_type_definition = Keyword("mod") - mathematical_expression()
    modular_type_definition.setName("ModularInteger")

    return range_type_definition | modular_type_definition


def enumeration_type_definition() -> Token:
    enumeration_literal = unqualified_identifier()
    positional_enumeration = enumeration_literal + ZeroOrMore(comma() - enumeration_literal)
    positional_enumeration.setParseAction(
        lambda t: [(k, Number(v)) for v, k in enumerate(t.asList())]
    )
    element_value_association = enumeration_literal + Keyword("=>") - numeric_literal()
    element_value_association.setParseAction(lambda t: (t[0], t[2]))
    named_enumeration = element_value_association + ZeroOrMore(comma() - element_value_association)

    boolean_literal = Keyword("True") | Keyword("False")
    boolean_literal.setParseAction(lambda t: t[0] == "True")
    boolean_aspect_definition = Optional(Keyword("=>") - boolean_literal)
    boolean_aspect_definition.setParseAction(lambda t: (t if t else ["=>", True]))
    always_valid_aspect = Literal("Always_Valid") - boolean_aspect_definition
    always_valid_aspect.setParseAction(parse_aspect)
    enumeration_aspects = Keyword("with") - delimitedList(size_aspect() | always_valid_aspect)
    enumeration_aspects.setParseAction(parse_aspects)

    return (
        Literal("(")
        - (named_enumeration | positional_enumeration)
        - Literal(")")
        - enumeration_aspects
    ).setName("Enumeration")


def array_type_definition() -> Token:
    return (Keyword("array of") + qualified_identifier()).setName("Array")


def message_type_definition() -> Token:
    first_aspect = Keyword("First") - Keyword("=>") - mathematical_expression()
    first_aspect.setParseAction(parse_aspect)
    length_aspect = Keyword("Length") - Keyword("=>") - mathematical_expression()
    length_aspect.setParseAction(parse_aspect)
    component_aspects = Keyword("with") - delimitedList(first_aspect | length_aspect)
    component_aspects.setParseAction(parse_aspects)

    then = locatedExpr(
        Keyword("then")
        - (Keyword("null") | unqualified_identifier())
        - Group(Optional(component_aspects))
        - Group(Optional(value_constraint()))
    )
    then.setParseAction(parse_then)
    then_list = then + ZeroOrMore(comma() - then)
    then_list.setParseAction(lambda t: [t.asList()])
    component_item = (
        ~Keyword("end")
        + ~CaselessKeyword("Message")
        - unqualified_identifier()
        + Literal(":")
        - qualified_identifier()
        - Optional(then_list)
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
        Keyword("message") - component_list - Keyword("end message") | Keyword("null message")
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
            - Suppress(Literal("("))
            - unqualified_identifier()
            - Suppress(Keyword("=>"))
            - qualified_identifier()
            - Suppress(Literal(")"))
            - Optional(value_constraint())("constraint")
            - semicolon().setParseAction(lambda s, l, t: l)
        )
        .setParseAction(parse_refinement)
        .setName("Refinement")
    )


def package_declaration() -> Token:
    basic_declaration = type_declaration() | type_refinement()

    return (
        Keyword("package")
        - unqualified_identifier()
        - Keyword("is")
        - Group(ZeroOrMore(basic_declaration))
        - Keyword("end")
        - unqualified_identifier()
        - semicolon()
    ).setParseAction(lambda t: PackageSpec(t[1], t[3].asList(), t[5]))


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
            raise ParseFatalException(string, location, f"implementation error ({e})")

    return wrapper


@fatalexceptions
def parse_array_aggregate(string: str, location: int, tokens: ParseResults) -> Expr:
    return Aggregate(*tokens[1:-1], location=parser_location(tokens[0], tokens[-1], string))


@fatalexceptions
def parse_string(string: str, location: int, tokens: ParseResults) -> Expr:
    return Aggregate(*[Number(ord(c)) for c in tokens[0]])


@fatalexceptions
def parse_concatenation(string: str, location: int, tokens: ParseResults) -> Expr:
    return Aggregate(*[e for t in tokens[0] for e in t.elements])


@fatalexceptions
def parse_term(string: str, location: int, tokens: ParseResults) -> Expr:
    if isinstance(tokens[0], ID):
        return Variable(tokens[0], negative=False, location=tokens[0].location)
    return tokens[0]


@fatalexceptions
def parse_relation(string: str, location: int, tokens: ParseResults) -> Relation:
    def locn() -> Location:
        return Location(tokens[0].location.start, tokens[0].location.source, tokens[2].location.end)

    if tokens[1] == "<":
        return Less(tokens[0], tokens[2], locn())
    if tokens[1] == "<=":
        return LessEqual(tokens[0], tokens[2], locn())
    if tokens[1] == "=":
        return Equal(tokens[0], tokens[2], locn())
    if tokens[1] == ">=":
        return GreaterEqual(tokens[0], tokens[2], locn())
    if tokens[1] == ">":
        return Greater(tokens[0], tokens[2], locn())
    if tokens[1] == "/=":
        return NotEqual(tokens[0], tokens[2], locn())
    raise ParseFatalException(string, location, "unexpected relation operator")


@fatalexceptions
def parse_logical_expression(string: str, location: int, tokens: ParseResults) -> Expr:
    result: List[Expr] = tokens[0]
    while len(result) > 1:
        left = result.pop(0)
        operator = result.pop(0)
        right = result.pop(0)
        expression: Expr
        assert left.location
        assert right.location
        locn = Location(left.location.start, left.location.source, right.location.end)
        if operator == "and":
            expression = And(left, right, location=locn)
        elif operator == "or":
            expression = Or(left, right, location=locn)
        else:
            raise ParseFatalException(string, location, "unexpected logical operator")
        result.insert(0, expression)
    return result[0]


@fatalexceptions
def parse_mathematical_expression(string: str, location: int, tokens: ParseResults) -> Expr:
    result: List[Expr] = tokens[0]
    while len(result) > 1:
        left = result.pop(0)
        operator = result.pop(0)
        right = result.pop(0)
        assert left.location, f'expression "{left}" without location'
        assert right.location, f'expression "{right}" without location'
        assert left.location.source == right.location.source, "expression with different source"
        locn = Location(left.location.start, left.location.source, left.location.end)
        expression: Expr
        if operator == "+":
            expression = Add(left, right)
            expression.location = locn
        elif operator == "-":
            expression = Sub(left, right, locn)
        elif operator == "*":
            expression = Mul(left, right)
            expression.location = locn
        elif operator == "/":
            expression = Div(left, right, locn)
        elif operator == "**":
            expression = Pow(left, right, locn)
        else:
            raise ParseFatalException(string, location, "unexpected mathematical operator")
        result.insert(0, expression)
    return result[0]


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
def verify_identifier(string: str, location: int, tokens: ParseResults) -> ID:
    reserved_words = [
        "abort",
        "abs",
        "abstract",
        "accept",
        "access",
        "aliased",
        "all",
        "and",
        "array",
        "at",
        "begin",
        "body",
        "case",
        "constant",
        "declare",
        "delay",
        "delta",
        "digits",
        "do",
        "else",
        "elsif",
        "end",
        "entry",
        "exception",
        "exit",
        "for",
        "function",
        "generic",
        "goto",
        "if",
        "in",
        "interface",
        "is",
        "limited",
        "loop",
        "mod",
        "new",
        "not",
        "null",
        "of",
        "or",
        "others",
        "out",
        "overriding",
        "package",
        "pragma",
        "private",
        "procedure",
        "protected",
        "raise",
        "range",
        "record",
        "rem",
        "renames",
        "requeue",
        "return",
        "reverse",
        "select",
        "separate",
        "some",
        "subtype",
        "synchronized",
        "tagged",
        "task",
        "terminate",
        "then",
        "type",
        "until",
        "use",
        "when",
        "while",
        "with",
        "xor",
        "initial",
        "final",
    ]
    data = tokens[0].asDict()
    tokens = data["value"]
    locn = parser_location(data["locn_start"], data["locn_end"], string)

    if tokens[0].lower() in reserved_words:
        fail(
            f'reserved word "{tokens[0]}" used as identifier',
            Subsystem.PARSER,
            Severity.ERROR,
            locn,
        )

    return ID(tokens[0], locn)


@fatalexceptions
def parse_attribute(string: str, location: int, tokens: ParseResults) -> Attribute:
    if tokens[2] == "First":
        return First(tokens[0])
    if tokens[2] == "Last":
        return Last(tokens[0])
    if tokens[2] == "Length":
        return Length(tokens[0])

    raise ParseFatalException(string, location, "unexpected attribute")


@fatalexceptions
def parse_aspect(string: str, location: int, tokens: ParseResults) -> Tuple[str, Expr]:
    return (tokens[0].lower(), tokens[2])


@fatalexceptions
def parse_aspects(string: str, location: int, tokens: ParseResults) -> Dict[str, Expr]:
    return dict(tokens[1:])


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
        return MessageSpec(identifier, tokens[4], locn)
    if tokens[3] == "null message":
        return MessageSpec(identifier, [], locn)
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
    constraint = tokens[4] if "constraint" in tokens else TRUE
    locn = parser_location(tokens[0], tokens[-1], string)
    return RefinementSpec(tokens[1], tokens[2], tokens[3], constraint, locn)


def unit() -> Token:
    return (specification() + StringEnd()).ignore(Regex(r"--.*"))
