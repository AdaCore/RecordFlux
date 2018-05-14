#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# pylint: disable=line-too-long

from pyparsing import (alphanums, infixNotation, nums, opAssoc, Forward, Group, Keyword, Literal,
                       Optional, QuotedString, Regex, StringEnd, Suppress, Word, WordEnd, WordStart,
                       ZeroOrMore)


class SyntaxTree:
    def __eq__(self, other):
        if not hasattr(other, '__dict__'):
            return False
        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        return "\n%s %s" % (self.__class__.__name__, self.__dict__)


class Name(SyntaxTree):
    def __init__(self, identifier):
        self.identifier: str = identifier


class Attribute(Name):
    def __init__(self, identifier, attribute):
        super().__init__(identifier)
        self.attribute: str = attribute


class ActualType(SyntaxTree):
    pass


class Modular(ActualType):
    def __init__(self, expression):
        self.expression: str = expression


class Signed(ActualType):
    def __init__(self, first, last):
        self.first = first
        self.last = last


class Record(ActualType):
    def __init__(self, components):
        self.components: list = components


class Enumeration(ActualType):
    def __init__(self, literals):
        self.literals: list = literals


class Type(SyntaxTree):
    def __init__(self, name, type_, aspects=None):
        self.name: Name = name
        self.type: ActualType = type_
        self.aspects: list = aspects or []


class Expression(SyntaxTree):
    pass


class And(Expression):
    def __init__(self, left, right):
        self.left: Expression = left
        self.right: Expression = right


class Or(Expression):
    def __init__(self, left, right):
        self.left: Expression = left
        self.right: Expression = right


class Xor(Expression):
    def __init__(self, left, right):
        self.left: Expression = left
        self.right: Expression = right


class Relation(Expression):
    pass


class Less(Relation):
    def __init__(self, left, right):
        self.left: Name = left
        self.right: Name = right


class LessEqual(Relation):
    def __init__(self, left, right):
        self.left: Name = left
        self.right: Name = right


class Equal(Relation):
    def __init__(self, left, right):
        self.left: Name = left
        self.right: Name = right


class GreaterEqual(Relation):
    def __init__(self, left, right):
        self.left: Name = left
        self.right: Name = right


class Greater(Relation):
    def __init__(self, left, right):
        self.left: Name = left
        self.right: Name = right


class NotEqual(Relation):
    def __init__(self, left, right):
        self.left: Name = left
        self.right: Name = right


class Aspect(SyntaxTree):
    def __init__(self, identifier, expression):
        self.identifier: Name = identifier
        self.expression: Expression = expression


class Component(SyntaxTree):
    def __init__(self, name, type_):
        self.name: Name = name
        self.type: Type = type_


class Package(SyntaxTree):
    def __init__(self, identifier, types):
        self.identifier: str = identifier
        self.types: list = types


class Parser:
    # pylint: disable=too-many-locals, too-many-statements, expression-not-assigned
    def __init__(self, basedir='.'):
        self.__basedir = basedir
        self.__syntax_tree = []

        # Generic
        comma = Suppress(Literal(','))
        semicolon = Suppress(Literal(';'))

        # Comments
        comment = Regex(r'--.*')

        # Names
        identifier = WordStart(alphanums) + Word(alphanums + '_') + WordEnd(alphanums + '_')
        defining_identifier = identifier

        # Names
        attribute_reference = Forward()
        slice_ = Forward()
        direct_name = identifier.copy()
        direct_name.setParseAction(lambda t: Name(t[0]))
        name = attribute_reference | slice_ | direct_name

        # Literals
        numeral = Word(nums) + ZeroOrMore(Optional(Word('_')) + Word(nums))
        base = numeral
        extended_digit = Word(nums + 'ABCDEF')
        based_numeral = extended_digit + ZeroOrMore(Optional('_') + extended_digit)
        based_literal = base + Literal('#') + based_numeral + Literal('#')
        numeric_literal = based_literal | numeral
        string_literal = QuotedString('"')

        # Expressions
        relational_operator = Literal('<=') | Literal('>=') | Literal('=') | Literal('/=') | Literal('<') | Literal('>')
        logical_operator = Keyword('and') | Keyword('or') | Keyword('xor')
        simple_expression = Forward()
        relation = simple_expression + relational_operator + simple_expression
        relation.setParseAction(parse_relation)
        expression = infixNotation(relation, [(logical_operator, 2, opAssoc.LEFT, parse_expression)])
        choice_relation = simple_expression + Optional(~Literal('=>') + relational_operator + simple_expression)
        choice_expression = choice_relation + ZeroOrMore(logical_operator + choice_relation)
        primary = Keyword('null') | numeric_literal | string_literal | name
        factor = primary + Optional(Literal('**') + primary) | Keyword('abs') + primary | Keyword('not') + primary
        multiplying_operator = Forward()
        term = factor + ZeroOrMore(multiplying_operator + factor)
        unary_adding_operator = Forward()
        binary_adding_operator = Forward()
        simple_expression << (Optional(unary_adding_operator) + term + ZeroOrMore(binary_adding_operator + term))
        simple_expression.setParseAction(lambda t: ''.join(t.asList()) if all(isinstance(e, str) for e in t.asList()) else t.asList())

        # Subtypes
        range_constraint = Forward()
        digits_constraint = Forward()
        scalar_constraint = range_constraint | digits_constraint
        index_constraint = Forward()
        composite_constraint = index_constraint
        constraint = scalar_constraint | composite_constraint
        subtype_indication = name + Optional(constraint)

        # Derived Types
        parent_subtype_indication = subtype_indication
        record_extension_part = Forward()
        derived_type_definition = Keyword('new') + parent_subtype_indication + Optional(record_extension_part)

        # Integer Types
        signed_integer_type_definition = Suppress(Keyword('range')) + simple_expression + Suppress(Literal('..')) + simple_expression
        signed_integer_type_definition.setParseAction(lambda t: Signed(*t.asList()))
        modular_type_definition = Suppress(Keyword('mod')) + simple_expression
        modular_type_definition.setParseAction(lambda t: Modular(''.join(t.asList())))
        integer_type_definition = signed_integer_type_definition | modular_type_definition

        # Enumeration Types
        enumeration_literal = name
        enumeration_type_definition = Suppress(Literal('(')) + enumeration_literal + ZeroOrMore(comma + enumeration_literal) + Suppress(Literal(')'))
        enumeration_type_definition.setParseAction(lambda t: Enumeration(t.asList()))

        # Range
        range_ = simple_expression + Literal('..') + simple_expression
        range_constraint << (Keyword('range') + range_)

        # Array Types
        discrete_range = range_ | subtype_indication
        index_constraint << (Literal('(') + discrete_range + ZeroOrMore(comma + discrete_range) + Literal(')'))
        component_definition = subtype_indication
        index_subtype_definition = name + Keyword('range <>')
        discrete_subtype_definition = range_ | subtype_indication
        unconstrained_array_definition = Keyword('array') + Literal('(') + index_subtype_definition + ZeroOrMore(comma + index_subtype_definition) + Literal(')') + Keyword('of') + component_definition
        constrained_array_definition = Keyword('array') + Literal('(') + discrete_subtype_definition + ZeroOrMore(comma + discrete_subtype_definition) + Literal(')') + Keyword('of') + component_definition
        array_type_definition = unconstrained_array_definition | constrained_array_definition

        # Operators
        binary_adding_operator << (Literal('+') | Literal('–') | Literal('&'))
        unary_adding_operator = Literal('+') | Literal('–')
        multiplying_operator = Literal('*') | Literal('/') | Literal('mod') | Literal('rem')

        # Object Declarations
        defining_identifier_list = defining_identifier + ZeroOrMore(comma + defining_identifier)

        # Variant Parts
        discrete_choice = Keyword('others') | range_ | choice_expression | subtype_indication
        discrete_choice_list = discrete_choice + ZeroOrMore(Literal('|') + discrete_choice)
        component_list = Forward()
        condition = expression
        variant_if_statement = Literal('if') + condition + Literal('then') + component_list + Optional(Literal('else') + component_list) + Literal('end if;')
        variant = Keyword('when') + discrete_choice_list + Literal('=>') + (variant_if_statement | component_list)
        variant_part = Keyword('case') + name + Keyword('is') + variant + ZeroOrMore(variant) + Keyword('end case;')

        # Record Type
        record_definition = Keyword('record') + component_list + Keyword('end record') | Keyword('null record')
        record_definition.setParseAction(lambda t: Record(t[1]))
        aspect_specification = Forward()
        component_declaration = defining_identifier_list + Literal(':') + component_definition + Optional(Literal(':=') + simple_expression) + Optional(aspect_specification) + Optional(Keyword('is abstract')) + semicolon
        component_item = variant_part | component_declaration
        component_item.setParseAction(lambda t: Component(t[0], t[2]))
        component_list << (Keyword('null') + semicolon | Keyword('invalid') + semicolon | Group(component_item + ZeroOrMore(component_item)))
        component_list.setParseAction(lambda t: t.asList())
        record_type_definition = record_definition

        # Type Extensions
        record_extension_part << (Keyword('with') + record_definition)

        # Aspect Specification
        aspect_definition = expression | identifier
        aspect_definition.setParseAction(lambda t: t.asList())
        aspect_mark = Keyword('Type_Invariant')
        aspect_specification << (Suppress(Keyword('with')) + Group(aspect_mark + Optional(Keyword('=>') + aspect_definition)))
        aspect_specification.setParseAction(lambda t: [Aspect(a[0], a[2]) for a in t])

        # Representation Aspects
        array_component_association = discrete_choice_list + Literal('=>') + (Literal('<>') | expression)
        named_array_aggregate = Literal('(') + array_component_association + ZeroOrMore(comma + array_component_association) + Literal(')')
        array_aggregate = named_array_aggregate
        enum_aggregate = array_aggregate
        enum_representation_clause = Literal('for') + name + Literal('use') + enum_aggregate + semicolon
        aspect_clause = enum_representation_clause

        # Slices
        slice_ << (identifier + Literal('(') + discrete_range + Literal(')'))

        # Attributes
        attribute_designator = Keyword('Length')
        attribute_reference << (identifier + Literal('\'') + attribute_designator)
        attribute_reference.setParseAction(lambda t: Attribute(t[0], t[2]))

        # Types
        type_definition = enumeration_type_definition | record_type_definition | derived_type_definition | integer_type_definition | array_type_definition
        type_declaration = Keyword('type') + identifier + Keyword('is') + type_definition + Optional(aspect_specification) + semicolon
        type_declaration.setParseAction(lambda t: Type(t[1], t[3], t[4:]))

        # Package
        basic_declaration = type_declaration | aspect_clause
        package_declaration = Keyword('package') + identifier + Keyword('is') + Group(ZeroOrMore(basic_declaration)) + Keyword('end') + name + semicolon
        package_declaration.setParseAction(lambda t: Package(t[1], t[3].asList()))

        # Grammar
        self.__grammar = Optional(package_declaration) + StringEnd()
        self.__grammar.setParseAction(self.default_action)
        self.__grammar.ignore(comment)

    def default_action(self, tokens):
        self.__syntax_tree.extend(tokens)

    def parse(self, infile):
        filepath = self.__basedir + "/" + infile
        with open(filepath, 'r') as filehandle:
            self.__grammar.parseFile(filehandle)

    def syntax_tree(self):
        return self.__syntax_tree


def parse_relation(tokens):
    if tokens[1] == '<':
        return Less(tokens[0], tokens[2])
    elif tokens[1] == '<=':
        return LessEqual(tokens[0], tokens[2])
    elif tokens[1] == '==':
        return Equal(tokens[0], tokens[2])
    elif tokens[1] == '>=':
        return GreaterEqual(tokens[0], tokens[2])
    elif tokens[1] == '>':
        return Greater(tokens[0], tokens[2])
    elif tokens[1] == '/=':
        return NotEqual(tokens[0], tokens[2])
    return None


def parse_expression(tokens):
    result = tokens[0]
    while len(result) > 1:
        left = result.pop(0)
        operator = result.pop(0)
        right = result.pop(0)
        if operator == 'and':
            expression = And(left, right)
        elif operator == 'or':
            expression = Or(left, right)
        elif operator == 'xor':
            expression = Xor(left, right)
        result.insert(0, expression)
    return result


if __name__ == "__main__":
    import sys
    Parser().parse(sys.argv[1])
    print("OK")
