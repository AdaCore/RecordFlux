from collections import OrderedDict
from typing import Any, Callable, Dict, Iterable, List, Tuple, Union

from pyparsing import (CaselessKeyword, Group, Keyword, Literal, Optional, ParseException,
                       ParseFatalException, Regex, StringEnd, Suppress, Token, Word, WordEnd,
                       WordStart, ZeroOrMore, alphanums, delimitedList, infixNotation, nums,
                       opAssoc)

from rflx.expression import (TRUE, UNDEFINED, Add, And, Attribute, Div, Equal, First, Greater,
                             GreaterEqual, Last, Length, LengthValue, Less, LessEqual, LogExpr,
                             MathExpr, Mul, NotEqual, Number, Or, Pow, Relation, Sub, Value)
from rflx.model import (FINAL, PDU, Array, Edge, Enumeration, InitialNode, ModelError,
                        ModularInteger, Node, RangeInteger, Reference, Refinement, Type)


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


COMMA = Suppress(Literal(',')).setName('","')
SEMICOLON = Suppress(Literal(';')).setName('";"')


class Parser:
    # pylint: disable=too-many-public-methods
    def __init__(self, basedir: str = '.') -> None:
        self.__basedir = basedir
        self.__specifications: Dict[str, Specification] = {}
        self.__pdus: Dict[str, PDU] = {}
        self.__refinements: List[Refinement] = []
        self.__grammar = self.specification() + StringEnd()
        self.__grammar.setParseAction(self.__evaluate_specification)
        self.__grammar.ignore(Regex(r'--.*'))

    def parse(self, infile: str) -> None:
        filepath = self.__basedir + "/" + infile
        with open(filepath, 'r') as filehandle:
            try:
                self.__grammar.parseFile(filehandle)
            except (ParseException, ParseFatalException) as e:
                raise ParserError(e)

    def parse_string(self, string: str) -> None:
        self.__grammar.parseString(string)

    def specifications(self) -> Dict[str, Specification]:
        return self.__specifications

    @property
    def pdus(self) -> List[PDU]:
        return list(self.__pdus.values())

    @property
    def refinements(self) -> List[Refinement]:
        return self.__refinements

    @classmethod
    def identifier(cls) -> Token:
        return (WordStart(alphanums) + Word(alphanums + '_') + WordEnd(alphanums + '_')
                ).setParseAction(verify_identifier).setName('Identifier')

    @classmethod
    def qualified_identifier(cls) -> Token:
        return (Optional(cls.identifier() + Literal('.')) - cls.identifier()
                ).setParseAction(lambda t: ''.join(t.asList()))

    @classmethod
    def name(cls) -> Token:
        attribute_designator = Keyword('First') | Keyword('Last') | Keyword('Length')
        attribute_reference = cls.identifier() + Literal('\'') - attribute_designator
        attribute_reference.setParseAction(parse_attribute)
        attribute_reference.setName('Attribute')

        return (attribute_reference | cls.identifier()).setName('Name')

    @classmethod
    def numeric_literal(cls) -> Token:
        numeral = Word(nums) + ZeroOrMore(Optional(Word('_')) + Word(nums))
        numeral.setParseAction(lambda t: int(''.join(t.asList()).replace('_', '')))

        extended_digit = Word(nums + 'ABCDEF')
        based_numeral = extended_digit + ZeroOrMore(Optional('_') + extended_digit)
        based_literal = numeral + Literal('#') - based_numeral - Literal('#')
        based_literal.setParseAction(lambda t: int(t[2].replace('_', ''), int(t[0])))

        return (based_literal | numeral).setParseAction(lambda t: Number(t[0])).setName('Number')

    @classmethod
    def logical_expression(cls) -> Token:
        relational_operator = (Keyword('<=') | Keyword('>=') | Keyword('=') | Keyword('/=')
                               | Keyword('<') | Keyword('>'))
        logical_operator = Keyword('and') | Keyword('or')

        relation = (cls.mathematical_expression() + relational_operator
                    - cls.mathematical_expression())
        relation.setParseAction(parse_relation)
        relation.setName('Relation')

        return (infixNotation(relation,
                              [(logical_operator,
                                2,
                                opAssoc.LEFT,
                                parse_logical_expression)])
                ).setName('LogicalExpression')

    @classmethod
    def mathematical_expression(cls) -> Token:
        mathematical_operator = (Literal('**') | Literal('+') | Literal('-') | Literal('*')
                                 | Literal('/'))

        term = Keyword('null') | cls.numeric_literal() | cls.name()
        term.setParseAction(parse_term)

        return (infixNotation(term,
                              [(mathematical_operator,
                                2,
                                opAssoc.LEFT,
                                parse_mathematical_expression)])
                ).setName('MathematicalExpression')

    @classmethod
    def value_constraint(cls) -> Token:
        return (Keyword('if') - cls.logical_expression()).setParseAction(lambda t: t[1])

    @classmethod
    def size_aspect(cls) -> Token:
        return (Keyword('Size') - Keyword('=>') - cls.mathematical_expression()
                ).setParseAction(parse_aspect)

    @classmethod
    def integer_type_definition(cls) -> Token:
        range_type_aspects = Keyword('with') - cls.size_aspect()
        range_type_aspects.setParseAction(parse_aspects)

        range_type_definition = (Keyword('range') - cls.mathematical_expression()
                                 - Suppress(Literal('..')) - cls.mathematical_expression()
                                 - range_type_aspects)
        range_type_definition.setName('RangeInteger')
        modular_type_definition = Keyword('mod') - cls.mathematical_expression()
        modular_type_definition.setName('ModularInteger')

        return range_type_definition | modular_type_definition

    @classmethod
    def enumeration_type_definition(cls) -> Token:
        enumeration_literal = cls.name()
        positional_enumeration = enumeration_literal + ZeroOrMore(COMMA - enumeration_literal)
        positional_enumeration.setParseAction(lambda t: [(k, Number(v))
                                                         for v, k in enumerate(t.asList())])
        element_value_association = enumeration_literal + Keyword('=>') - cls.numeric_literal()
        element_value_association.setParseAction(lambda t: (t[0], t[2]))
        named_enumeration = (element_value_association
                             + ZeroOrMore(COMMA - element_value_association))

        boolean_literal = Keyword('True') | Keyword('False')
        boolean_literal.setParseAction(lambda t: t[0] == 'True')
        boolean_aspect_definition = Optional(Keyword('=>') - boolean_literal)
        boolean_aspect_definition.setParseAction(lambda t: (t if t else ['=>', True]))
        always_valid_aspect = Literal('Always_Valid') - boolean_aspect_definition
        always_valid_aspect.setParseAction(parse_aspect)
        enumeration_aspects = (Keyword('with')
                               - delimitedList(cls.size_aspect() | always_valid_aspect))
        enumeration_aspects.setParseAction(parse_aspects)

        return (Literal('(') - (named_enumeration | positional_enumeration) - Literal(')')
                - enumeration_aspects).setName('Enumeration')

    @classmethod
    def array_type_definition(cls) -> Token:
        return (Keyword('array of') + cls.name()).setName('Array')

    @classmethod
    def message_type_definition(cls) -> Token:
        first_aspect = Keyword('First') - Keyword('=>') - cls.mathematical_expression()
        first_aspect.setParseAction(parse_aspect)
        length_aspect = Keyword('Length') - Keyword('=>') - cls.mathematical_expression()
        length_aspect.setParseAction(parse_aspect)
        component_aspects = Keyword('with') - delimitedList(first_aspect | length_aspect)
        component_aspects.setParseAction(parse_aspects)

        then = (Keyword('then') - (Keyword('null') | cls.identifier())
                - Group(Optional(component_aspects))
                - Group(Optional(cls.value_constraint())))
        then.setParseAction(parse_then)
        then_list = then + ZeroOrMore(COMMA - then)
        then_list.setParseAction(lambda t: [t.asList()])
        component_item = (~Keyword('end') + ~CaselessKeyword('Message') - cls.identifier()
                          + Literal(':') - cls.name() - Optional(then_list) - SEMICOLON)
        component_item.setParseAction(lambda t:
                                      Component(t[0], t[2], t[3]) if len(t) >= 4
                                      else Component(t[0], t[2]))
        component_item.setName('Component')
        null_component_item = Keyword('null') - then - SEMICOLON
        null_component_item.setParseAction(lambda t: Component(t[0], '', [t[1]]))
        null_component_item.setName('NullComponent')
        component_list = (Group(Optional(null_component_item) - component_item
                                - ZeroOrMore(component_item)))
        component_list.setParseAction(lambda t: t.asList())

        return (Keyword('message') - component_list - Keyword('end message')
                | Keyword('null message')).setName('Message')

    @classmethod
    def type_declaration(cls) -> Token:
        type_definition = (cls.enumeration_type_definition()
                           | cls.integer_type_definition()
                           | cls.message_type_definition()
                           | cls.array_type_definition())

        return (Keyword('type') - cls.identifier() - Keyword('is') - type_definition - SEMICOLON
                ).setParseAction(parse_type)

    @classmethod
    def type_refinement(cls) -> Token:
        return (Suppress(Keyword('for')) - cls.qualified_identifier() - Suppress(Keyword('use'))
                - Suppress(Literal('(')) - cls.identifier() - Suppress(Keyword('=>'))
                - cls.qualified_identifier() - Suppress(Literal(')'))
                - Optional(cls.value_constraint())('constraint') - SEMICOLON
                ).setParseAction(parse_refinement).setName('Refinement')

    @classmethod
    def package_declaration(cls) -> Token:
        basic_declaration = cls.type_declaration() | cls.type_refinement()

        return (Keyword('package') - cls.identifier() - Keyword('is')
                - Group(ZeroOrMore(basic_declaration)) - Keyword('end')
                - cls.identifier() - SEMICOLON
                ).setParseAction(lambda t: Package(t[1], t[3].asList()))

    @classmethod
    def context_clause(cls) -> Token:
        context_item = Keyword('with') - cls.identifier() - SEMICOLON
        context_item.setParseAction(lambda t: t[1])

        return ZeroOrMore(context_item).setParseAction(lambda t: Context(t.asList()))

    @classmethod
    def specification(cls) -> Token:
        return (Optional(cls.context_clause() + cls.package_declaration())
                ).setParseAction(lambda t: Specification(t[0], t[1]) if len(t) == 2 else None)

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
                self.__refinements.extend(refinements)


def convert_to_pdus(spec: Specification) -> Dict[str, PDU]:
    types: Dict[str, Type] = {}
    pdus: Dict[str, PDU] = {}

    for t in spec.package.types:
        if t.name in types:
            raise ParserError(f'duplicate type "{t.name}"')

        full_name = f'{spec.package.identifier}.{t.name}'

        if isinstance(t, (ModularInteger, RangeInteger, Enumeration)):
            pass
        elif isinstance(t, Array):
            if t.element_type.name not in types:
                raise ParserError(f'undefined type "{t.element_type.name}" in "{t.name}"')
            if not isinstance(types[t.element_type.name], Message):
                element_type_size = types[t.element_type.name].size.simplified()
                if not isinstance(element_type_size, Number) or int(element_type_size) % 8 != 0:
                    raise ParserError(f'unsupported size ({element_type_size}) of element type '
                                      f'"{t.element_type.name}" in "{t.name}" '
                                      '(no multiple of 8)')
                t = Array(t.name, types[t.element_type.name])
        elif isinstance(t, Message):
            nodes: Dict[str, Node] = OrderedDict()
            create_graph(nodes, types, t.components, t.name)
            pdus[full_name] = PDU(full_name, next(iter(nodes.values()), FINAL))
        elif isinstance(t, Refinement):
            continue
        else:
            raise NotImplementedError(f'unsupported type "{type(t).__name__}"')

        types[t.name] = t

    return pdus


def create_graph(nodes: Dict[str, Node], types: Dict[str, Type],
                 components: List[Component], message_name: str) -> None:

    if not components:
        return

    components = list(components)

    if components[0].name != 'null':
        components.insert(0, Component('null', ''))

    create_nodes(nodes, types, components, message_name)
    create_edges(nodes, components, message_name)

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
            raise ParserError(f'undefined type "{component.type}" in "{message_name}"')
        if isinstance(types[component.type], Message):
            raise ParserError(f'unsupported type "{component.type}" in "{message_name}"')
        nodes[component.name] = Node(component.name, types[component.type])


def create_edges(nodes: Dict[str, Node], components: List[Component], message_name: str) -> None:
    for i, component in enumerate(components):
        if not component.thens:
            nodes[component.name].edges.append(
                Edge(nodes[components[i + 1].name]) if i + 1 < len(components) else Edge(FINAL))
        for then in component.thens:
            if then.name not in nodes and then.name != 'null':
                raise ParserError(f'undefined component "{then.name}" in "{message_name}"')
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


def convert_to_refinements(spec: Specification, pdus: Dict[str, PDU]) -> List[Refinement]:
    refinements: List[Refinement] = []
    for t in spec.package.types:
        if isinstance(t, Refinement):
            pdu = qualified_type_name(t.pdu, spec.package.identifier, pdus.keys(),
                                      f'undefined type "{t.pdu}" in refinement')
            if t.field not in pdus[pdu].fields():
                raise ParserError(f'invalid field "{t.field}" in refinement of "{t.pdu}"')
            sdu = t.sdu
            if sdu != 'null':
                sdu = qualified_type_name(t.sdu, spec.package.identifier, pdus.keys(),
                                          f'undefined type "{t.sdu}" in refinement of "{t.pdu}"')
            refinement = Refinement(spec.package.identifier, pdu, t.field, sdu, t.condition)
            if refinement in refinements:
                raise ParserError(f'duplicate refinement of field "{t.field}" with "{t.sdu}"'
                                  f' in "{t.pdu}"')
            refinements.append(refinement)
    return refinements


def qualified_type_name(name: str, package: str, types: Iterable[str], error_message: str) -> str:
    if name in types:
        return name

    name = f'{package}.{name}'

    if name not in types:
        raise ParserError(error_message)

    return name


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
        if tokens[3] == 'null message':
            return Message(tokens[1], [])
        if tokens[3] == '(':
            elements = dict(tokens[4:-2])
            aspects = tokens[-1]
            if len(elements) < len(tokens[4:-2]):
                raise ModelError(f'"{tokens[1]}" contains duplicate elements')
            if 'always_valid' not in aspects:
                aspects['always_valid'] = False
            return Enumeration(tokens[1], elements, aspects['size'], aspects['always_valid'])
        if tokens[3] == 'array of':
            return Array(tokens[1], Reference(tokens[4]))
    except ModelError as e:
        raise ParseFatalException(string, location, e)
    raise ParseFatalException(string, location, 'unexpected type')


@fatalexceptions
def parse_refinement(string: str, location: int, tokens: list) -> Type:
    if 'constraint' not in tokens:
        tokens.append(TRUE)
    return Refinement('', *tokens)
