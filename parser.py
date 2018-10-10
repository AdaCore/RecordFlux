from collections import OrderedDict
from typing import Dict, List, Optional as Opt, Union

from pyparsing import (alphanums, infixNotation, nums, opAssoc, ParseFatalException, Forward,
                       Group, Keyword, Literal, OneOrMore, Optional, Regex, StringEnd, Suppress,
                       Word, WordEnd, WordStart, ZeroOrMore)

from model import (Add, And, Array, Attribute, Div, Edge, Equal, FINAL, First, Greater,
                   GreaterEqual, Last, Length, Less, LessEqual, LogExpr, MathExpr, ModularInteger,
                   Mul, Number, Node, NotEqual, Or, PDU, RangeInteger, Relation, Sub, Type, Value)


class SyntaxTree:
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other: object) -> bool:
        return not self.__eq__(other)

    def __repr__(self) -> str:
        return "\n%s %s" % (self.__class__.__name__, self.__dict__)


class Derived(Type):
    def __init__(self, name: str, parent: str, refinements: Dict = None) -> None:
        super().__init__(name)
        self.parent = parent
        self.refinements = refinements or {}

    def size(self) -> Number:
        raise NotImplementedError


class Record(Type):
    def __init__(self, name: str, components: List['Component'], abstract: bool = False) -> None:
        super().__init__(name)
        self.components = components
        self.abstract = abstract

    def __repr__(self) -> str:
        return 'Record({}, {}, {})'.format(self.name, self.components, self.abstract)

    def size(self) -> Number:
        raise NotImplementedError


class Enumeration(Type):
    def __init__(self, name: str, literals: List[str]) -> None:
        super().__init__(name)
        self.literals = literals

    def size(self) -> Number:
        raise NotImplementedError


class Aspect(SyntaxTree):
    def __init__(self, identifier: str, expression: LogExpr) -> None:
        self.identifier = identifier
        self.expression = expression


class Then(SyntaxTree):
    def __init__(self, name: str, location: LogExpr = None,
                 constraint: LogExpr = None) -> None:
        self.name = name
        self.location = location
        self.constraint = constraint


class Component(SyntaxTree):
    def __init__(self, name: str, type_name: str, thens: List[Then] = None) -> None:
        self.name = name
        self.type = type_name
        self.thens = thens or []


class Package(SyntaxTree):
    def __init__(self, identifier: str, types: List[Type]) -> None:
        self.identifier = identifier
        self.types = types


class Context(SyntaxTree):
    def __init__(self, items: List[str]) -> None:
        self.items = items


class Specification(SyntaxTree):
    def __init__(self, context: Context, package: Package) -> None:
        self.context = context
        self.package = package


class ParserError(Exception):
    pass


class Parser:
    # pylint: disable=too-many-locals, too-many-statements, expression-not-assigned
    def __init__(self, basedir: str = '.') -> None:
        self.__basedir = basedir
        self.__specifications: Dict[str, Specification] = {}

        # Generic
        comma = Suppress(Literal(','))
        semicolon = Suppress(Literal(';'))

        # Comments
        comment = Regex(r'--.*')

        # Names
        identifier = WordStart(alphanums) + Word(alphanums + '_') + WordEnd(alphanums + '_')
        attribute_designator = Keyword('First') | Keyword('Last') | Keyword('Length')
        attribute_reference = identifier + Literal('\'') - attribute_designator
        attribute_reference.setParseAction(parse_attribute)
        name = attribute_reference | identifier

        # Literals
        numeral = Word(nums) + ZeroOrMore(Optional(Word('_')) + Word(nums))
        numeral.setParseAction(lambda t: int(''.join(t.asList()).replace('_', '')))
        extended_digit = Word(nums + 'ABCDEF')
        based_numeral = extended_digit + ZeroOrMore(Optional('_') + extended_digit)
        based_literal = numeral + Literal('#') - based_numeral - Literal('#')
        based_literal.setParseAction(lambda t: int(t[2].replace('_', ''), int(t[0])))
        numeric_literal = based_literal | numeral
        numeric_literal.setParseAction(lambda t: Number(t[0]))
        literal = numeric_literal

        # Operators
        mathematical_operator = Keyword('+') | Keyword('-') | Keyword('*') | Keyword('/')
        relational_operator = (Keyword('<=') | Keyword('>=') | Keyword('=') | Keyword('/=')
                               | Keyword('<') | Keyword('>'))
        logical_operator = Keyword('and') | Keyword('or')

        # Expressions
        mathematical_expression = Forward()
        relation = mathematical_expression + relational_operator - mathematical_expression
        relation.setParseAction(parse_relation)
        logical_expression = infixNotation(relation,
                                           [(logical_operator,
                                             2,
                                             opAssoc.LEFT,
                                             parse_logical_expression)])
        term = Keyword('null') | literal | name
        term.setParseAction(parse_term)
        mathematical_expression << infixNotation(term,
                                                 [(mathematical_operator,
                                                   2,
                                                   opAssoc.LEFT,
                                                   parse_mathematical_expression)])

        # Subtypes
        range_constraint = Forward()
        digits_constraint = Forward()
        scalar_constraint = range_constraint | digits_constraint
        index_constraint = Forward()
        composite_constraint = index_constraint
        constraint = scalar_constraint | composite_constraint
        subtype_indication = name + Optional(constraint)

        # Derived Types
        type_refinement_part = (Suppress(Literal('('))
                                + OneOrMore(identifier + Suppress(Literal('=>')) - identifier)
                                - Suppress(Literal(')')))
        type_refinement_part.setParseAction(lambda t: dict(zip(t[::2], t[1::2])))
        derived_type_definition = (Suppress(Keyword('new')) - subtype_indication
                                   - Optional(type_refinement_part))
        derived_type_definition.setParseAction(lambda t: Derived(*t.asList()))

        # Integer Types
        range_type_definition = (Suppress(Keyword('range')) - numeric_literal
                                 - Suppress(Literal('..')) - numeric_literal
                                 - Suppress(Keyword('with Size =>')) - numeric_literal)
        range_type_definition.setParseAction(lambda t:
                                             RangeInteger('', int(t[0]), int(t[1]), int(t[2])))
        modular_type_definition = Suppress(Keyword('mod')) - numeric_literal
        modular_type_definition.setParseAction(lambda t: ModularInteger('', *map(int, t.asList())))
        integer_type_definition = range_type_definition | modular_type_definition

        # Enumeration Types
        enumeration_literal = name
        enumeration_type_definition = (Suppress(Literal('(')) + enumeration_literal
                                       + ZeroOrMore(comma - enumeration_literal)
                                       + Suppress(Literal(')')))
        enumeration_type_definition.setParseAction(lambda t: Enumeration('', t.asList()))

        # Variant Parts
        component_list = Forward()

        # Record Type
        constraint = Keyword('if') - logical_expression
        constraint.setParseAction(lambda t: ('constraint', t[1]))
        location_expression = Keyword('with') - logical_expression
        location_expression.setParseAction(lambda t: ('location', t[1]))
        then = Keyword('then') - identifier - Optional(location_expression) - Optional(constraint)
        then.setParseAction(parse_then)
        then_list = then + ZeroOrMore(Suppress(comma) - then)
        then_list.setParseAction(lambda t: [t.asList()])
        record_definition = (Optional(Keyword('abstract')) + Keyword('record')
                             - component_list - Keyword('end record'))
        record_definition.setParseAction(lambda t:
                                         Record('', t[1]) if t[0] != 'abstract'
                                         else Record('', t[2], True))
        component_declaration = (identifier + Literal(':') - subtype_indication
                                 - Optional(then_list) - semicolon)
        component_item = component_declaration
        component_item.setParseAction(lambda t:
                                      Component(t[0], t[2], t[3]) if len(t) >= 4
                                      else Component(t[0], t[2]))
        component_list << (Keyword('null') - semicolon | Keyword('invalid')
                           + semicolon | Group(component_item + ZeroOrMore(component_item)))
        component_list.setParseAction(lambda t: t.asList())
        record_type_definition = record_definition

        # Aspect Specification
        aspect_definition = logical_expression | identifier
        aspect_definition.setParseAction(lambda t: t.asList())
        aspect_mark = Keyword('Type_Invariant')
        aspect_specification = (Suppress(Keyword('with'))
                                + Group(aspect_mark + Optional(Keyword('=>') - aspect_definition)))
        aspect_specification.setParseAction(lambda t: [Aspect(a[0], a[2]) for a in t])

        # Representation Aspects
        discrete_choice = Keyword('others') | subtype_indication
        discrete_choice_list = discrete_choice + ZeroOrMore(Literal('|') - discrete_choice)
        array_component_association = (discrete_choice_list + Literal('=>')
                                       - (Literal('<>') | logical_expression))
        named_array_aggregate = (Literal('(') + array_component_association
                                 + ZeroOrMore(comma + array_component_association) + Literal(')'))
        enum_representation_clause = (Literal('for') + name + Literal('use') - named_array_aggregate
                                      - semicolon)
        aspect_clause = enum_representation_clause

        # Types
        type_definition = (enumeration_type_definition | record_type_definition
                           | derived_type_definition | integer_type_definition)
        type_declaration = (Keyword('type') - identifier - Keyword('is') - type_definition
                            - Optional(aspect_specification) - semicolon)
        type_declaration.setParseAction(parse_type)

        # Package
        basic_declaration = type_declaration | aspect_clause
        package_declaration = (Keyword('package') - identifier - Keyword('is')
                               - Group(ZeroOrMore(basic_declaration)) - Keyword('end') - name
                               - semicolon)
        package_declaration.setParseAction(lambda t: Package(t[1], t[3].asList()))

        # Context
        context_item = Keyword('with') - identifier - semicolon
        context_item.setParseAction(lambda t: t[1])
        context_clause = ZeroOrMore(context_item)
        context_clause.setParseAction(lambda t: Context(t.asList()))

        # Specification
        specification = Optional(context_clause + package_declaration)
        specification.setParseAction(lambda t: Specification(t[0], t[1]) if len(t) == 2 else None)

        # Grammar
        self.__grammar = specification + StringEnd()
        self.__grammar.setParseAction(self.__store_specification)
        self.__grammar.ignore(comment)

    def __store_specification(self, tokens: List[Specification]) -> None:
        if len(tokens) == 1:
            specification = tokens[0]
            identifier = specification.package.identifier
            if identifier in self.__specifications:
                raise ParserError('Duplicate package {}'.format(identifier))
            self.__specifications[identifier] = specification

    def parse(self, infile: str) -> None:
        filepath = self.__basedir + "/" + infile
        with open(filepath, 'r') as filehandle:
            self.__grammar.parseFile(filehandle)

    def specifications(self) -> Dict[str, Specification]:
        return self.__specifications

    def pdus(self) -> Dict[str, PDU]:
        pdus = {}
        for name, spec in self.__specifications.items():
            pdu = convert_to_pdu(spec)
            if pdu:
                pdus[name] = pdu
        return pdus


def convert_to_pdu(spec: Specification) -> Opt[PDU]:
    types: Dict[str, Type] = {}
    nodes: Dict[str, Node] = OrderedDict()
    pdu = None

    for t in spec.package.types:
        if isinstance(t, (ModularInteger, RangeInteger)):
            types[t.name] = t
        elif isinstance(t, Record):
            if t.name != 'PDU':
                raise ParserError('Expected record name PDU, found {}'.format(t.name))
            create_nodes(nodes, types, t.components)
            create_edges(nodes, t.components)
            pdu = PDU(spec.package.identifier, next(iter(nodes.values()), FINAL))

    return pdu


def create_nodes(nodes: Dict[str, Node], types: Dict[str, Type],
                 components: List[Component]) -> None:
    for component in components:
        if 'Payload' in component.type:
            types[component.type] = Array(component.type)
        nodes[component.name] = Node(component.name, types[component.type])


def create_edges(nodes: Dict[str, Node], components: List[Component]) -> None:
    for i, component in enumerate(components):
        if not component.thens:
            nodes[component.name].edges.append(
                Edge(nodes[components[i + 1].name]) if i + 1 < len(components) else Edge(FINAL))
        for then in component.thens:
            edge = Edge(nodes[then.name]) if then.name != 'null' else Edge(FINAL)
            if then.constraint:
                edge.condition = then.constraint
            if then.location:
                location = convert_location_expression(then.location)
                if 'first' in location:
                    edge.first = location['first']
                if 'length' in location:
                    edge.length = location['length']
            nodes[component.name].edges.append(edge)


def convert_location_expression(expr: LogExpr) -> Dict[str, MathExpr]:
    result: Dict[str, MathExpr] = {}
    if not isinstance(expr, (And, Equal)):
        raise ParserError('Invalid location expression {}'.format(expr))
    if isinstance(expr, Equal):
        result.update(convert_location_equation(expr))
    if isinstance(expr, And):
        result.update(convert_location_equation(expr.left))
        result.update(convert_location_equation(expr.right))
    return result


def convert_location_equation(expr: LogExpr) -> Dict[str, MathExpr]:
    if not isinstance(expr, Equal):
        raise ParserError('Expected equation, found {}'.format(expr))
    if not isinstance(expr.left, Value) \
            or (expr.left != Value('First') and expr.left != Value('Length')):
        raise ParserError('Expected First or Length, found {}'.format(expr.left))
    if not isinstance(expr.right, MathExpr):
        raise ParserError('Expected expression, found {}'.format(expr.right))
    return {expr.left.name.lower(): expr.right}


def parse_term(string: str, location: int, tokens: list) -> Union[Attribute, Number]:
    if isinstance(tokens[0], str):
        return Value(tokens[0])
    if isinstance(tokens[0], (Attribute, Number)):
        return tokens[0]
    raise ParseFatalException(string, location, 'Expected identifier, attribute or number')


def parse_relation(string: str, location: int, tokens: list) -> Relation:
    if tokens[1] == '<':
        return Less(tokens[0], tokens[2])
    if tokens[1] == '<=':
        return LessEqual(tokens[0], tokens[2])
    if tokens[1] == '=':
        return Equal(tokens[0], tokens[2])
    if tokens[1] == '>=':
        return GreaterEqual(tokens[0], tokens[2])
    if tokens[1] == '>':
        return Greater(tokens[0], tokens[2])
    if tokens[1] == '/=':
        return NotEqual(tokens[0], tokens[2])
    raise ParseFatalException(string, location, 'Unexpected relation operator')


def parse_logical_expression(string: str, location: int, tokens: list) -> LogExpr:
    result: List[LogExpr] = tokens[0]
    while len(result) > 1:
        left = result.pop(0)
        operator = result.pop(0)
        right = result.pop(0)
        expression: LogExpr
        if operator == 'and':
            expression = And(left, right)
        elif operator == 'or':
            expression = Or(left, right)
        else:
            raise ParseFatalException(string, location, 'Unexpected logical operator')
        result.insert(0, expression)
    return result[0]


def parse_mathematical_expression(string: str, location: int, tokens: list) -> MathExpr:
    result: List[MathExpr] = tokens[0]
    while len(result) > 1:
        left = result.pop(0)
        operator = result.pop(0)
        right = result.pop(0)
        expression: MathExpr
        if operator == '+':
            expression = Add(left, right)
        elif operator == '-':
            expression = Sub(left, right)
        elif operator == '*':
            expression = Mul(left, right)
        elif operator == '/':
            expression = Div(left, right)
        else:
            raise ParseFatalException(string, location, 'Unexpected mathematical operator')
        result.insert(0, expression)
    return result[0]


def parse_then(string: str, location: int, tokens: list) -> Then:
    identifier = tokens[1]
    location_expr = None
    constraint = None
    for key, expr in tokens[2:]:
        if key == 'location':
            location_expr = expr
        elif key == 'constraint':
            constraint = expr
        else:
            raise ParseFatalException(
                string, location, 'Expected location expression or value constraint in \'then\'')
    return Then(identifier, location_expr, constraint)


def parse_attribute(string: str, location: int, tokens: list) -> Attribute:
    if tokens[2] == 'First':
        return First(tokens[0])
    if tokens[2] == 'Last':
        return Last(tokens[0])
    if tokens[2] == 'Length':
        return Length(tokens[0])
    raise ParseFatalException(string, location, 'Expected attribute')


def parse_type(string: str, location: int, tokens: list) -> Type:
    if isinstance(tokens[3], (Enumeration, ModularInteger, RangeInteger, Record)):
        tokens[3].name = tokens[1]
        return tokens[3]
    raise ParseFatalException(string, location, 'Unexpected type')
