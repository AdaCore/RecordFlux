#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# pylint: disable=missing-docstring, line-too-long

from pyparsing import alphanums, nums, Forward, Keyword, Literal, Optional, QuotedString, Regex, StringEnd, Word, WordEnd, WordStart, ZeroOrMore


class Parser:
    def __init__(self, basedir='.'):
        # pylint: disable=too-many-locals, too-many-statements, expression-not-assigned
        self.__data = []
        self.__basedir = basedir

        # Generic
        comma = Literal(',')
        semicolon = Literal(';')

        # Comments
        comment = Regex(r'--.*')

        # Identifiers
        identifier = WordStart(alphanums) + Word(alphanums + '_') + WordEnd(alphanums + '_')
        defining_identifier = identifier

        # Names
        direct_name = identifier
        slice_ = Forward()
        attribute_reference = Forward()
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
        conjunctive_operator = Keyword('and then') | Keyword('or else') | Keyword('xor') | Keyword('and') | Keyword('or')
        simple_expression = Forward()
        relation = simple_expression + Optional(relational_operator + simple_expression)
        expression = relation + ZeroOrMore(conjunctive_operator + relation)
        choice_relation = simple_expression + Optional(~Literal('=>') + relational_operator + simple_expression)
        choice_expression = choice_relation + ZeroOrMore(conjunctive_operator + choice_relation)
        primary = Keyword('null') | numeric_literal | string_literal | name | expression
        factor = primary + Optional(Literal('**') + primary) | Keyword('abs') + primary | Keyword('not') + primary
        multiplying_operator = Forward()
        term = factor + ZeroOrMore(multiplying_operator + factor)
        unary_adding_operator = Forward()
        binary_adding_operator = Forward()
        simple_expression << (Optional(unary_adding_operator) + term + ZeroOrMore(binary_adding_operator + term))
        static_simple_expression = simple_expression
        static_expression = expression
        default_expression = expression

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
        signed_integer_type_definition = Keyword('range') + static_simple_expression + Literal('..') + static_simple_expression
        modular_type_definition = Keyword('mod') + static_expression
        integer_type_definition = signed_integer_type_definition | modular_type_definition

        # Enumeration Types
        enumeration_literal_specification = name  # pylint: disable=invalid-name
        enumeration_type_definition = Literal('(') + enumeration_literal_specification + ZeroOrMore(comma + enumeration_literal_specification) + Literal(')')

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
        binary_adding_operator = Literal('+') | Literal('–') | Literal('&')
        unary_adding_operator = Literal('+') | Literal('–')
        multiplying_operator = Literal('*') | Literal('/') | Literal('mod') | Literal('rem')

        # Object Declarations
        defining_identifier_list = defining_identifier + ZeroOrMore(comma + defining_identifier)

        # Variant Parts
        discrete_choice = Keyword('others') | range_ | choice_expression | subtype_indication
        discrete_choice_list = discrete_choice + ZeroOrMore(Literal('|') + discrete_choice)
        component_list = Forward()
        variant = Keyword('when') + discrete_choice_list + Literal('=>') + component_list
        variant_part = Keyword('case') + name + Keyword('is') + variant + ZeroOrMore(variant) + Keyword('end case;')

        # Record Type
        record_definition = Keyword('record') + component_list + Keyword('end record') | Keyword('null record')
        aspect_specification = Forward()
        component_declaration = defining_identifier_list + Literal(':') + component_definition + Optional(Literal(':=') + default_expression) + Optional(aspect_specification) + Optional(Keyword('is abstract')) + semicolon
        component_item = variant_part | component_declaration
        component_list << (Keyword('null') + semicolon | Keyword('invalid') + semicolon | component_item + ZeroOrMore(component_item))
        record_type_definition = record_definition

        # Type Extensions
        record_extension_part << (Keyword('with') + record_definition)

        # Aspect Specification
        aspect_definition = expression | identifier
        aspect_mark = identifier
        aspect_specification << (Keyword('with') + aspect_mark + Optional(Keyword('=>') + aspect_definition) + ZeroOrMore(comma + aspect_mark + Optional(Keyword('=>') + aspect_definition)))

        # Slices
        slice_ << (identifier + Literal('(') + discrete_range + Literal(')'))

        # Attributes
        attribute_designator = identifier
        attribute_reference << (identifier + Literal('\'') + attribute_designator)

        # Types
        type_definition = enumeration_type_definition | record_type_definition | derived_type_definition | integer_type_definition | array_type_definition
        type_declaration = Keyword('type') + identifier + Keyword('is') + type_definition + Optional(aspect_specification) + semicolon
        basic_declaration = type_declaration

        # Package
        package_declaration = Keyword('package') + name + Keyword('is') + ZeroOrMore(basic_declaration) + Keyword('end') + name + semicolon

        # Declaration
        basic_declaration = package_declaration

        # Parser file
        self._grammar = ZeroOrMore(basic_declaration) + StringEnd()
        self._grammar.setParseAction(self.default_action)

        # Ignore comments
        self._grammar.ignore(comment)

    def default_action(self, tokens):
        self.__data.extend(tokens)

    def parse(self, infile):
        filepath = self.__basedir + "/" + infile
        with open(filepath, 'r') as filehandle:
            self._grammar.parseFile(filehandle)

    def data(self):
        return self.__data
