from collections import OrderedDict
from typing import Dict, List, Union

from pyparsing import (alphanums, infixNotation, nums, opAssoc, ParseFatalException, Forward,
                       Group, Keyword, Literal, Optional, Regex, StringEnd, Suppress, Word, WordEnd,
                       WordStart, ZeroOrMore)

from model import (Add, And, Array, Attribute, Div, Edge, Equal, FINAL, First, Greater,
                   GreaterEqual, Last, Length, Less, LessEqual, LogExpr, MathExpr, ModelError,
                   ModularInteger, Mul, Number, Node, NotEqual, Or, PDU, Pow, RangeInteger,
                   Refinement, Relation, Sub, TRUE, Type, Value)


class SyntaxTree:
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other: object) -> bool:
        return not self.__eq__(other)

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')


class Message(Type):
    def __init__(self, name: str, components: List['Component']) -> None:
        super().__init__(name)
        self.components = components

    def size(self) -> Number:
        raise NotImplementedError


class Enumeration(Type):
    def __init__(self, name: str, literals: List[str]) -> None:
        super().__init__(name)
        self.literals = literals

    def size(self) -> Number:
        raise NotImplementedError


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
        self.__pdus: Dict[str, PDU] = {}
        self.__refinements: Dict[str, Refinement] = {}

        # Generic
        comma = Suppress(Literal(','))
        comma.setName('","')
        semicolon = Suppress(Literal(';'))
        semicolon.setName('";"')

        # Comments
        comment = Regex(r'--.*')

        # Names
        identifier = WordStart(alphanums) + Word(alphanums + '_') + WordEnd(alphanums + '_')
        identifier.setName('Identifier')
        qualified_identifier = Optional(identifier + Literal('.')) - identifier
        qualified_identifier.setParseAction(lambda t: ''.join(t.asList()))
        attribute_designator = Keyword('First') | Keyword('Last') | Keyword('Length')
        attribute_reference = identifier + Literal('\'') - attribute_designator
        attribute_reference.setParseAction(parse_attribute)
        attribute_reference.setName('Attribute')
        name = attribute_reference | identifier
        name.setName('Name')

        # Literals
        numeral = Word(nums) + ZeroOrMore(Optional(Word('_')) + Word(nums))
        numeral.setParseAction(lambda t: int(''.join(t.asList()).replace('_', '')))
        extended_digit = Word(nums + 'ABCDEF')
        based_numeral = extended_digit + ZeroOrMore(Optional('_') + extended_digit)
        based_literal = numeral + Literal('#') - based_numeral - Literal('#')
        based_literal.setParseAction(lambda t: int(t[2].replace('_', ''), int(t[0])))
        numeric_literal = based_literal | numeral
        numeric_literal.setParseAction(lambda t: Number(t[0]))
        numeric_literal.setName('Number')
        literal = numeric_literal

        # Operators
        mathematical_operator = (Literal('**') | Literal('+') | Literal('-') | Literal('*')
                                 | Literal('/'))
        relational_operator = (Keyword('<=') | Keyword('>=') | Keyword('=') | Keyword('/=')
                               | Keyword('<') | Keyword('>'))
        logical_operator = Keyword('and') | Keyword('or')

        # Expressions
        mathematical_expression = Forward()
        relation = mathematical_expression + relational_operator - mathematical_expression
        relation.setParseAction(parse_relation)
        relation.setName('Relation')
        logical_expression = infixNotation(relation,
                                           [(logical_operator,
                                             2,
                                             opAssoc.LEFT,
                                             parse_logical_expression)])
        logical_expression.setName('LogicalExpression')
        term = Keyword('null') | literal | name
        term.setParseAction(parse_term)
        mathematical_expression << infixNotation(term,
                                                 [(mathematical_operator,
                                                   2,
                                                   opAssoc.LEFT,
                                                   parse_mathematical_expression)])
        mathematical_expression.setName('MathematicalExpression')

        # Subtypes
        range_constraint = Forward()
        digits_constraint = Forward()
        scalar_constraint = range_constraint | digits_constraint
        index_constraint = Forward()
        composite_constraint = index_constraint
        constraint = scalar_constraint | composite_constraint
        subtype_indication = name + Optional(constraint)

        # Type Refinement
        value_constraint = Keyword('if') - logical_expression
        value_constraint.setParseAction(lambda t: ('constraint', t[1]))
        type_refinement_definition = (Keyword('new') - qualified_identifier - Suppress(Literal('('))
                                      - identifier - Suppress(Literal('=>')) - qualified_identifier
                                      - Suppress(Literal(')')) - Optional(value_constraint))
        type_refinement_definition.setName('Refinement')

        # Integer Types
        range_type_definition = (Keyword('range') - mathematical_expression
                                 - Suppress(Literal('..')) - mathematical_expression
                                 - Suppress(Keyword('with Size =>')) - mathematical_expression)
        range_type_definition.setName('RangeInteger')
        modular_type_definition = Keyword('mod') - mathematical_expression
        modular_type_definition.setName('ModularInteger')
        integer_type_definition = range_type_definition | modular_type_definition

        # Enumeration Types
        enumeration_literal = name
        enumeration_type_definition = (Literal('(') - enumeration_literal
                                       + ZeroOrMore(comma - enumeration_literal) - Literal(')'))
        enumeration_type_definition.setName('Enumeration')

        # Message Type
        location_expression = Keyword('with') - logical_expression
        location_expression.setParseAction(lambda t: ('location', t[1]))
        then = (Keyword('then') - identifier - Optional(location_expression)
                - Optional(value_constraint))
        then.setParseAction(parse_then)
        then_list = then + ZeroOrMore(comma - then)
        then_list.setParseAction(lambda t: [t.asList()])
        component_list = Forward()
        message_type_definition = Keyword('message') - component_list - Keyword('end message')
        message_type_definition.setName('Message')
        component_declaration = (identifier + Literal(':') - subtype_indication
                                 - Optional(then_list) - semicolon)
        component_item = component_declaration
        component_item.setParseAction(lambda t:
                                      Component(t[0], t[2], t[3]) if len(t) >= 4
                                      else Component(t[0], t[2]))
        component_item.setName('Component')
        component_list << (Keyword('null') - semicolon | Keyword('invalid')
                           - semicolon | Group(component_item - ZeroOrMore(component_item)))
        component_list.setParseAction(lambda t: t.asList())

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
        type_definition = (enumeration_type_definition | integer_type_definition
                           | message_type_definition | type_refinement_definition)
        type_declaration = (Keyword('type') - identifier - Keyword('is') - type_definition
                            - semicolon)
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
        self.__grammar.setParseAction(self.__evaluate_specification)
        self.__grammar.ignore(comment)

    def __evaluate_specification(self, tokens: List[Specification]) -> None:
        if len(tokens) == 1:
            specification = tokens[0]
            identifier = specification.package.identifier
            if identifier in self.__specifications:
                raise ParserError(f'duplicate package "{identifier}"')
            self.__specifications[identifier] = specification
            pdus = convert_to_pdus(specification)
            if pdus:
                self.__pdus.update(pdus)
            refinements = convert_to_refinements(specification, self.__pdus)
            if refinements:
                self.__refinements.update(refinements)

    def parse(self, infile: str) -> None:
        filepath = self.__basedir + "/" + infile
        with open(filepath, 'r') as filehandle:
            self.__grammar.parseFile(filehandle)

    def parse_string(self, string: str) -> None:
        self.__grammar.parseString(string)

    def specifications(self) -> Dict[str, Specification]:
        return self.__specifications

    @property
    def pdus(self) -> List[PDU]:
        return list(self.__pdus.values())

    @property
    def refinements(self) -> List[Refinement]:
        return list(self.__refinements.values())


def convert_to_pdus(spec: Specification) -> Dict[str, PDU]:
    types: Dict[str, Type] = {}
    pdus: Dict[str, PDU] = {}

    for t in spec.package.types:
        if isinstance(t, (ModularInteger, RangeInteger)):
            if t.name in types:
                raise ParserError(f'duplicate type "{t.name}"')
            types[t.name] = t
        elif isinstance(t, Message):
            nodes: Dict[str, Node] = OrderedDict()
            create_nodes(nodes, types, t.components)
            create_edges(nodes, t.components)
            name = f'{spec.package.identifier}.{t.name}'
            if name in pdus:
                raise ParserError(f'duplicate message "{t.name}"')
            pdus[name] = PDU(name, next(iter(nodes.values()), FINAL))

    return pdus


def create_nodes(nodes: Dict[str, Node], types: Dict[str, Type],
                 components: List[Component]) -> None:
    for component in components:
        if 'Payload' in component.type:
            types[component.type] = Array(component.type)
        if component.type not in types:
            raise ParserError(f'reference to undefined type "{component.type}"')
        nodes[component.name] = Node(component.name, types[component.type])


def create_edges(nodes: Dict[str, Node], components: List[Component]) -> None:
    for i, component in enumerate(components):
        if not component.thens:
            nodes[component.name].edges.append(
                Edge(nodes[components[i + 1].name]) if i + 1 < len(components) else Edge(FINAL))
        for then in component.thens:
            if then.name not in nodes and then.name != 'null':
                raise ParserError(f'reference to undefined node "{then.name}"')
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
    if isinstance(expr, Equal):
        result.update(convert_location_equation(expr))
    elif isinstance(expr, And):
        result.update(convert_location_equation(expr.left))
        result.update(convert_location_equation(expr.right))
    else:
        raise ParserError(f'unexpected "{expr.symbol()}" in "{expr}"')
    return result


def convert_location_equation(expr: LogExpr) -> Dict[str, MathExpr]:
    if not isinstance(expr, Equal):
        raise ParserError(f'expected "=" instead of "{expr.symbol()}" in "{expr}"')
    if not isinstance(expr.left, Value) \
            or (expr.left != Value('First') and expr.left != Value('Length')):
        raise ParserError(f'expected "First" or "Length" instead of "{expr.left}"')
    return {expr.left.name.lower(): expr.right}


def convert_to_refinements(spec: Specification, pdus: Dict[str, PDU]) -> Dict[str, Refinement]:
    refinements: Dict[str, Refinement] = {}
    for t in spec.package.types:
        if isinstance(t, Refinement):
            pdu = t.pdu
            if pdu not in pdus:
                pdu = f'{spec.package.identifier}.{t.pdu}'
                if pdu not in pdus:
                    raise ParserError(f'unknown type "{t.pdu}"')
            sdu = t.sdu
            if sdu not in pdus:
                sdu = f'{spec.package.identifier}.{t.pdu}'
                if sdu not in pdus:
                    raise ParserError(f'unknown type "{t.sdu}"')
            name = f'{spec.package.identifier}.{t.name}'
            if name in refinements:
                raise ParserError(f'duplicate refinement "{t.name}"')
            refinements[name] = Refinement(name, pdu, t.field, sdu, t.condition)
    return refinements


def parse_term(string: str, location: int, tokens: list) -> Union[Attribute, Number]:
    if isinstance(tokens[0], str):
        return Value(tokens[0])
    if isinstance(tokens[0], (Attribute, Number)):
        return tokens[0]
    raise ParseFatalException(string, location, 'expected identifier, attribute or number')


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
    raise ParseFatalException(string, location, 'unexpected relation operator')


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
            raise ParseFatalException(string, location, 'unexpected logical operator')
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
        elif operator == '**':
            expression = Pow(left, right)
        else:
            raise ParseFatalException(string, location, 'unexpected mathematical operator')
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
                string, location, 'expected location expression or value constraint')
    return Then(identifier, location_expr, constraint)


def parse_attribute(string: str, location: int, tokens: list) -> Attribute:
    if tokens[2] == 'First':
        return First(tokens[0])
    if tokens[2] == 'Last':
        return Last(tokens[0])
    if tokens[2] == 'Length':
        return Length(tokens[0])
    raise ParseFatalException(string, location, 'unexpected attribute')


def parse_type(string: str, location: int, tokens: list) -> Type:
    try:
        if tokens[3] == 'mod':
            return ModularInteger(tokens[1], *tokens[4:6])
        if tokens[3] == 'range':
            return RangeInteger(tokens[1], *tokens[4:7])
        if tokens[3] == 'message':
            return Message(tokens[1], tokens[4])
        if tokens[3] == '(':
            return Enumeration(tokens[1], tokens[4:-1])
        if tokens[3] == 'new':
            if len(tokens) == 7:
                tokens.append(TRUE)
            elif len(tokens) == 8:
                tokens[7] = tokens[7][1]
            return Refinement(tokens[1], *tokens[4:])
    except ModelError as e:
        raise ParseFatalException(string, location, e)
    raise ParseFatalException(string, location, 'unexpected type')
