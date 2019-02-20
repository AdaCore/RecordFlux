from collections import OrderedDict
from typing import Any, Callable, Dict, List, Tuple, Union

from pyparsing import (alphanums, delimitedList, infixNotation, nums, opAssoc, CaselessKeyword,
                       ParseFatalException, Forward, Group, Keyword, Literal, Optional, Regex,
                       StringEnd, Suppress, Word, WordEnd, WordStart, ZeroOrMore)

from rflx.expression import (Add, And, Attribute, Div, Equal, First, Greater, GreaterEqual, Last,
                             Length, LengthValue, Less, LessEqual, LogExpr, MathExpr, Mul, Number,
                             NotEqual, Or, Pow, Relation, Sub, TRUE, UNDEFINED, Value)
from rflx.model import (Array, Edge, Enumeration, FINAL, InitialNode, ModelError, ModularInteger,
                        Node, PDU, RangeInteger, Refinement, Type)


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

    @property
    def size(self) -> Number:
        raise NotImplementedError


class Then(SyntaxTree):
    def __init__(self, name: str, first: MathExpr = None, length: MathExpr = None,
                 constraint: LogExpr = None) -> None:
        self.name = name
        self.first = first
        self.length = length
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
        identifier.setParseAction(verify_identifier)
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

        # Type Refinement
        value_constraint = Keyword('if') - logical_expression
        value_constraint.setParseAction(lambda t: t[1])
        type_refinement_definition = (Keyword('new') - qualified_identifier - Suppress(Literal('('))
                                      - identifier - Suppress(Literal('=>'))
                                      - (Keyword('null') | qualified_identifier)
                                      - Suppress(Literal(')')) - Optional(value_constraint))
        type_refinement_definition.setName('Refinement')

        # Integer Types
        size_aspect = Keyword('Size') - Keyword('=>') - mathematical_expression
        size_aspect.setParseAction(parse_aspect)
        range_type_aspects = Keyword('with') - size_aspect
        range_type_aspects.setParseAction(parse_aspects)

        range_type_definition = (Keyword('range') - mathematical_expression
                                 - Suppress(Literal('..')) - mathematical_expression
                                 - range_type_aspects)
        range_type_definition.setName('RangeInteger')
        modular_type_definition = Keyword('mod') - mathematical_expression
        modular_type_definition.setName('ModularInteger')
        integer_type_definition = range_type_definition | modular_type_definition

        # Enumeration Types
        enumeration_literal = name
        positional_enumeration = enumeration_literal + ZeroOrMore(comma - enumeration_literal)
        positional_enumeration.setParseAction(lambda t: [(k, Number(v))
                                                         for v, k in enumerate(t.asList())])
        element_value_association = enumeration_literal + Keyword('=>') - numeric_literal
        element_value_association.setParseAction(lambda t: (t[0], t[2]))
        named_enumeration = (element_value_association
                             + ZeroOrMore(comma - element_value_association))

        boolean_literal = Keyword('True') | Keyword('False')
        boolean_literal.setParseAction(lambda t: t[0] == 'True')
        boolean_aspect_definition = Optional(Keyword('=>') - boolean_literal)
        boolean_aspect_definition.setParseAction(lambda t: (t if t else ['=>', True]))
        always_valid_aspect = Literal('Always_Valid') - boolean_aspect_definition
        always_valid_aspect.setParseAction(parse_aspect)
        enumeration_aspects = Keyword('with') - delimitedList(size_aspect | always_valid_aspect)
        enumeration_aspects.setParseAction(parse_aspects)

        enumeration_type_definition = (Literal('(') - (named_enumeration | positional_enumeration)
                                       - Literal(')') - enumeration_aspects)
        enumeration_type_definition.setName('Enumeration')

        # Array Type
        unconstrained_array_definition = Keyword('array of') + name
        array_type_definition = unconstrained_array_definition
        array_type_definition.setName('Array')

        # Message Type
        first_aspect = Keyword('First') - Keyword('=>') - mathematical_expression
        first_aspect.setParseAction(parse_aspect)
        length_aspect = Keyword('Length') - Keyword('=>') - mathematical_expression
        length_aspect.setParseAction(parse_aspect)
        component_aspects = Keyword('with') - delimitedList(first_aspect | length_aspect)
        component_aspects.setParseAction(parse_aspects)

        then = (Keyword('then') - (Keyword('null') | identifier)
                - Group(Optional(component_aspects))
                - Group(Optional(value_constraint)))
        then.setParseAction(parse_then)
        then_list = then + ZeroOrMore(comma - then)
        then_list.setParseAction(lambda t: [t.asList()])
        component_list = Forward()
        message_type_definition = Keyword('message') - component_list - Keyword('end message')
        message_type_definition.setName('Message')
        component_item = (~Keyword('end') + ~CaselessKeyword('Message') - identifier + Literal(':')
                          - name - Optional(then_list) - semicolon)
        component_item.setParseAction(lambda t:
                                      Component(t[0], t[2], t[3]) if len(t) >= 4
                                      else Component(t[0], t[2]))
        component_item.setName('Component')
        null_component_item = Keyword('null') - then - semicolon
        null_component_item.setParseAction(lambda t: Component(t[0], '', [t[1]]))
        null_component_item.setName('NullComponent')
        component_list << (Group(Optional(null_component_item) - component_item
                                 - ZeroOrMore(component_item)))
        component_list.setParseAction(lambda t: t.asList())

        # Types
        type_definition = (enumeration_type_definition | integer_type_definition
                           | message_type_definition | type_refinement_definition
                           | array_type_definition)
        type_declaration = (Keyword('type') - identifier - Keyword('is') - type_definition
                            - semicolon)
        type_declaration.setParseAction(parse_type)

        # Package
        basic_declaration = type_declaration
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
        if isinstance(t, (ModularInteger, RangeInteger, Enumeration, Array)):
            if t.name in types:
                raise ParserError(f'duplicate type "{t.name}"')
            if isinstance(t, Array) and t.element_type not in types:
                raise ParserError(f'reference to undefined type "{t.element_type}" in "{t.name}"')
            if isinstance(t, Array) and not isinstance(types[t.element_type], Message):
                raise ParserError(f'unsupported element type "{t.element_type}" in "{t.name}"')
            types[t.name] = t
        elif isinstance(t, Message):
            nodes: Dict[str, Node] = OrderedDict()
            create_graph(nodes, types, t.components, t.name)
            name = f'{spec.package.identifier}.{t.name}'
            if name in pdus:
                raise ParserError(f'duplicate message "{t.name}"')
            pdus[name] = PDU(name, next(iter(nodes.values()), FINAL))
            types[t.name] = t
        elif isinstance(t, Refinement):
            continue
        else:
            raise NotImplementedError(f'unsupported type "{type(t).__name__}"')

    return pdus


def create_graph(nodes: Dict[str, Node], types: Dict[str, Type],
                 components: List[Component], message_name: str) -> None:

    components = list(components)

    if components[0].name != 'null':
        components.insert(0, Component('null', ''))

    create_nodes(nodes, types, components, message_name)
    create_edges(nodes, components)

    if next(iter(nodes.values())).edges[0].first != UNDEFINED:
        raise ParserError(f'invalid first expression in initial node in "{message_name}"')


def create_nodes(nodes: Dict[str, Node], types: Dict[str, Type],
                 components: List[Component], message_name: str) -> None:

    for component in components:
        if component.name == 'null':
            nodes[component.name] = InitialNode()
            continue
        if 'Payload' in component.type:
            types[component.type] = Array(component.type)
        if component.type not in types:
            raise ParserError(f'reference to undefined type "{component.type}" in "{message_name}"')
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
            if then.first:
                edge.first = then.first.converted(replace_value_by_length_value)
            if then.length:
                edge.length = then.length.converted(replace_value_by_length_value)
            nodes[component.name].edges.append(edge)


def replace_value_by_length_value(self: MathExpr) -> MathExpr:
    return LengthValue(self.name) if isinstance(self, Value) else self


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
            if sdu != 'null' and sdu not in pdus:
                sdu = f'{spec.package.identifier}.{t.sdu}'
                if sdu not in pdus:
                    raise ParserError(f'unknown type "{t.sdu}"')
            name = f'{spec.package.identifier}.{t.name}'
            if name in refinements:
                raise ParserError(f'duplicate refinement "{t.name}"')
            refinements[name] = Refinement(name, pdu, t.field, sdu, t.condition)
    return refinements


# pylint: disable=unused-argument
def fatalexceptions(parse_function: Callable) -> Callable:
    def wrapper(string: str, location: int, tokens: list) -> Any:
        try:
            return parse_function(string, location, tokens)
        except ParseFatalException as e:
            raise e
        except Exception as e:
            raise ParseFatalException(string, location, f'implementation error ({e})')
    return wrapper


@fatalexceptions
def parse_term(string: str, location: int, tokens: list) -> Union[Attribute, Number]:
    if isinstance(tokens[0], str):
        return Value(tokens[0])
    if isinstance(tokens[0], (Attribute, Number)):
        return tokens[0]
    raise ParseFatalException(string, location, 'expected identifier, attribute or number')


@fatalexceptions
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


@fatalexceptions
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


@fatalexceptions
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


@fatalexceptions
def parse_then(string: str, location: int, tokens: list) -> Then:
    return Then(tokens[1],
                tokens[2][0]['first'] if tokens[2] and 'first' in tokens[2][0] else None,
                tokens[2][0]['length'] if tokens[2] and 'length' in tokens[2][0] else None,
                tokens[3][0] if tokens[3] else None)


@fatalexceptions
def verify_identifier(string: str, location: int, tokens: list) -> str:
    reserved_words = ['abort', 'abs', 'abstract', 'accept', 'access', 'aliased', 'all', 'and',
                      'array', 'at', 'begin', 'body', 'case', 'constant', 'declare', 'delay',
                      'delta', 'digits', 'do', 'else', 'elsif', 'end', 'entry', 'exception',
                      'exit', 'for', 'function', 'generic', 'goto', 'if', 'in', 'interface', 'is',
                      'limited', 'loop', 'mod', 'new', 'not', 'null', 'of', 'or', 'others', 'out',
                      'overriding', 'package', 'pragma', 'private', 'procedure', 'protected',
                      'raise', 'range', 'record', 'rem', 'renames', 'requeue', 'return', 'reverse',
                      'select', 'separate', 'some', 'subtype', 'synchronized', 'tagged', 'task',
                      'terminate', 'then', 'type', 'until', 'use', 'when', 'while', 'with', 'xor',
                      'buffer']
    if tokens[0].lower() in reserved_words:
        raise ParseFatalException(
            string, location, f'reserved word "{tokens[0]}" used as identifier')
    return tokens[0]


@fatalexceptions
def parse_attribute(string: str, location: int, tokens: list) -> Attribute:
    if tokens[2] == 'First':
        return First(tokens[0])
    if tokens[2] == 'Last':
        return Last(tokens[0])
    if tokens[2] == 'Length':
        return Length(tokens[0])
    raise ParseFatalException(string, location, 'unexpected attribute')


@fatalexceptions
def parse_aspect(string: str, location: int, tokens: list) -> Tuple[str, Any]:
    return (tokens[0].lower(), tokens[2])


@fatalexceptions
def parse_aspects(string: str, location: int, tokens: list) -> Dict[str, Any]:
    return dict(tokens[1:])


@fatalexceptions
def parse_type(string: str, location: int, tokens: list) -> Type:
    try:
        if tokens[3] == 'mod':
            return ModularInteger(tokens[1], *tokens[4:6])
        if tokens[3] == 'range':
            tokens[6] = tokens[6]['size']
            return RangeInteger(tokens[1], *tokens[4:7])
        if tokens[3] == 'message':
            return Message(tokens[1], tokens[4])
        if tokens[3] == '(':
            elements = dict(tokens[4:-2])
            aspects = tokens[-1]
            if len(elements) < len(tokens[4:-2]):
                raise ModelError(f'"{tokens[1]}" contains duplicate elements')
            if 'always_valid' not in aspects:
                aspects['always_valid'] = False
            return Enumeration(tokens[1], elements, aspects['size'], aspects['always_valid'])
        if tokens[3] == 'new':
            if len(tokens) == 7:
                tokens.append(TRUE)
            return Refinement(tokens[1], *tokens[4:])
        if tokens[3] == 'array of':
            return Array(tokens[1], tokens[4])
    except ModelError as e:
        raise ParseFatalException(string, location, e)
    raise ParseFatalException(string, location, 'unexpected type')
