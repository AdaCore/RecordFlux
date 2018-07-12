#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from abc import ABC, abstractmethod

import sys

import parser


class SparkRepresentation:
    def __eq__(self, other):
        if not hasattr(other, '__dict__'):
            return False
        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        return "\n%s %s" % (self.__class__.__name__, self.__dict__)


class Unit(SparkRepresentation):
    def __init__(self, context, package):
        self.context = context
        self.package = package

    def specification(self):
        context_clause = ''
        if self.context:
            context_clause = '{}\n\n'.format('\n'.join([c.specification() for c in self.context]))
        return '{}{}\n'.format(context_clause, self.package.specification())

    def definition(self):
        return '{}\n'.format(self.package.definition())


class ContextItem(SparkRepresentation):
    def __init__(self, name, use):
        self.name: str = name
        self.use: bool = use

    def specification(self):
        return 'with {};{}'.format(self.name, ' use {};'.format(self.name) if self.use else '')

    def definition(self):  # pylint: disable=no-self-use
        return ''


class Package(SparkRepresentation):
    def __init__(self, name, types, functions):
        self.name: str = name
        self.types: list = types
        self.functions: list = functions

    def specification(self):
        return 'package {name} is\n\n{types}\n\n{functions}\n\nend {name};'.format(
            name=self.name,
            types='\n\n'.join([t.specification() for t in self.types if t.specification()]),
            functions='\n\n'.join([f.specification() for f in self.functions]))

    def definition(self):
        return 'package body {name} is\n\n{types}\n\n{functions}\n\nend {name};'.format(
            name=self.name,
            types='\n'.join([t.definition() for t in self.types]),
            functions='\n\n'.join([f.definition() for f in self.functions]))


class Type(SparkRepresentation):
    def __init__(self, name, type_definition):
        self.name: str = name
        self.type_definition: str = type_definition

    def specification(self):
        if self.type_definition is None:
            return ''
        return '   type {name} is {type_definition};'.format(
            name=self.name,
            type_definition=self.type_definition)

    def definition(self):
        return '   function Convert_To_{name} is new Convert_To ({name});'.format(
            name=self.name)


class Function(SparkRepresentation):
    def __init__(self, name, return_type, precondition, body):
        self.name: str = name
        self.return_type: str = return_type
        self.precondition: Expression = precondition
        self.body: list = body

    def specification(self):
        with_clause = ''
        if self.precondition and self.precondition is not True:
            with_clause = ' with\n      Pre => {}'.format(
                self.precondition.specification())
        return '   function {name} (Buffer : Bytes) return {return_type}{with_clause};'.format(
            name=self.name,
            return_type=self.return_type,
            with_clause=with_clause)

    def definition(self):
        return ('   function {name} (Buffer : Bytes) return {return_type} is\n'
                '   begin\n'
                '{body}\n'
                '   end {name};').format(
                    name=self.name,
                    return_type=self.return_type,
                    body='\n'.join([s.definition() for s in self.body]))


class Statement(SparkRepresentation):
    def __init__(self, body):
        self.body: str = body

    def definition(self):
        return '      {};'.format(self.body)


class Expression(SparkRepresentation, ABC):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    @abstractmethod
    def specification(self):
        pass

    def definition(self):
        return self.specification()

    def fields(self):
        return self.left.fields() + self.right.fields()

    def replace_field(self, identifier, statement):
        self.left.replace_field(identifier, statement)
        self.right.replace_field(identifier, statement)

    def simplified(self):
        return self

    def transformed(self, current, previous, offset):  # pylint: disable=unused-argument
        return self


class And(Expression):
    def specification(self):
        return '({} and then {})'.format(self.left.specification(), self.right.specification())

    def simplified(self):
        if self.left is not True:
            self.left = self.left.simplified()
        if self.right is not True:
            self.right = self.right.simplified()
        if self.left is True and self. right is True:
            return True
        if self.left is True:
            return self.right
        if self.right is True:
            return self.left
        return self

    def transformed(self, current, previous, offset):
        return And(self.left.transformed(current, previous, offset),
                   self.right.transformed(current, previous, offset))


class Or(Expression):
    def specification(self):
        return '({} or {})'.format(self.left.specification(), self.right.specification())

    def simplified(self):
        if self.left is not True:
            self.left = self.left.simplified()
        if self.right is not True:
            self.right = self.right.simplified()
        if self.left is True or self. right is True:
            return True
        return self

    def transformed(self, current, previous, offset):
        return Or(self.left.transformed(current, previous, offset),
                  self.right.transformed(current, previous, offset))


class Element(SparkRepresentation, ABC):
    @abstractmethod
    def specification(self):
        pass

    def definition(self):
        return self.specification()

    def fields(self):  # pylint: disable=no-self-use
        return []

    def replace_field(self, identifier, statement):
        pass

    def simplified(self):
        return self


class Field(Element):
    def __init__(self, identifier):
        self.identifier: str = identifier

    def specification(self):
        return self.identifier

    def fields(self):
        return [self.identifier]

    def replace_field(self, identifier, statement):
        if self.identifier == identifier:
            self.identifier = statement

    def transformed(self, current, previous, offset):  # pylint: disable=unused-argument
        return Field(self.identifier)


class Length(Element):
    def __init__(self, identifier):
        self.identifier: str = identifier

    def specification(self):
        return '{}\'Length'.format(self.identifier)

    def transformed(self, current, previous, offset):  # pylint: disable=unused-argument
        return Length(self.identifier)


class Value(Element):
    def __init__(self, literal):
        self.literal: str = literal

    def specification(self):
        return self.literal

    def transformed(self, current, previous, offset):  # pylint: disable=unused-argument
        return Value(self.literal)


class Relation(Expression, ABC):
    @abstractmethod
    def specification(self):
        pass

    def transformed(self, current, previous, offset):
        if isinstance(self.left, Length) and self.left.identifier not in [current] + previous:
            return True
        if isinstance(self.right, Length) and self.right.identifier not in [current] + previous:
            return True
        if isinstance(self.left, Field) and self.left.identifier not in previous:
            return True
        if isinstance(self.right, Field) and self.right.identifier not in previous:
            return True
        if (isinstance(self.left, Length) and isinstance(self.right, Value)
                and self.left.identifier == current):
            return self.__class__(Length('Buffer'), Value(str(int(self.right.literal) + offset)))
        if (isinstance(self.right, Length) and isinstance(self.left, Value)
                and self.right.identifier == current):
            return self.__class__(Length('Buffer'), Value(str(int(self.left.literal) + offset)))
        return self.__class__(self.left.transformed(current, previous, offset),
                              self.right.transformed(current, previous, offset))


class Less(Relation):
    def specification(self):
        return '{} < {}'.format(self.left.specification(), self.right.specification())


class LessEqual(Relation):
    def specification(self):
        return '{} <= {}'.format(self.left.specification(), self.right.specification())


class Equal(Relation):
    def specification(self):
        return '{} = {}'.format(self.left.specification(), self.right.specification())


class GreaterEqual(Relation):
    def specification(self):
        return '{} >= {}'.format(self.left.specification(), self.right.specification())


class Greater(Relation):
    def specification(self):
        return '{} > {}'.format(self.left.specification(), self.right.specification())


class NotEqual(Relation):
    def specification(self):
        return '{} /= {}'.format(self.left.specification(), self.right.specification())


BUILTIN_TYPES = {
    'Payload_Type': 0,
    'Byte': 1,
    'U16': 2,
    'U32': 4,
}


def convert_expression(expression):
    if isinstance(expression, parser.And):
        return And(convert_expression(expression.left), convert_expression(expression.right))
    if isinstance(expression, parser.Or):
        return Or(convert_expression(expression.left), convert_expression(expression.right))
    if isinstance(expression, parser.Less):
        return Less(convert_expression(expression.left), convert_expression(expression.right))
    if isinstance(expression, parser.LessEqual):
        return LessEqual(convert_expression(expression.left), convert_expression(expression.right))
    if isinstance(expression, parser.Equal):
        return Equal(convert_expression(expression.left), convert_expression(expression.right))
    if isinstance(expression, parser.GreaterEqual):
        return GreaterEqual(convert_expression(expression.left),
                            convert_expression(expression.right))
    if isinstance(expression, parser.Greater):
        return Greater(convert_expression(expression.left), convert_expression(expression.right))
    if isinstance(expression, parser.NotEqual):
        return NotEqual(convert_expression(expression.left), convert_expression(expression.right))
    if isinstance(expression, parser.Attribute):
        if expression.attribute == 'Length':
            return Length(expression.identifier)
        assert False, 'found unknown attribute {}'.format(expression.attribute)
    if isinstance(expression, parser.Name):
        return Field(expression.identifier)
    if isinstance(expression, parser.Value):
        return Value(expression.literal)
    assert False, 'found unknown expression type {}'.format(expression)
    return None


class Generator:
    def __init__(self):
        self.__units = []

    def generate(self, syntax_tree):
        package = syntax_tree[0]
        if not isinstance(package, parser.Package):
            assert False, 'found {}, expected Package'.format(type(package).__name__)

        types = []
        functions = []
        for t in package.types:
            if isinstance(t.type, parser.Modular):
                types += [Type(t.name, 'mod {}'.format(t.type.expression.literal))]
            elif isinstance(t.type, parser.Record):
                for a in t.aspects:
                    if a.identifier != 'Type_Invariant':
                        assert False, 'found {}, expected Type_Invariant'.format(a.identifier)
                    type_invariant = convert_expression(a.expression)

                component_last = 0
                previous_components = []
                for c in t.type.components:
                    # Calculate bounds of component
                    component_first = component_last
                    if c.type.identifier in BUILTIN_TYPES:
                        component_last += BUILTIN_TYPES[c.type.identifier]
                        if c.type.identifier.startswith('U'):
                            type_ = Type(c.type.identifier, None)
                            if type_ not in types:
                                types += [type_]
                    else:
                        # TODO: determine size of custom type
                        if c.type.identifier.startswith('U'):
                            component_last += int(c.type.identifier[1:]) // 8
                        else:
                            assert False, 'unable to determine size of type {}'.format(
                                c.type.identifier)

                    # Common precondition
                    length_precondition = GreaterEqual(Length('Buffer'), Value(str(component_last)))
                    precondition = And(length_precondition,
                                       type_invariant.transformed(
                                           c.name,
                                           [k.name for k in previous_components],
                                           component_last)).simplified()
                    for field in previous_components:
                        precondition.replace_field(field.name, '{} (Buffer)'.format(field.name))

                    # Field accessor
                    if c.type.identifier == 'Payload_Type':
                        # TODO: determine payload length
                        slice_ = 'Buffer\'First + {first} .. Buffer\'Last'.format(
                            first=component_first)
                        field_accessor = 'Payload_Type (Buffer ({slice_}))'.format(slice_=slice_)
                    else:
                        slice_ = 'Buffer\'First + {first} .. Buffer\'First + {last}'.format(
                            first=component_first, last=component_last - 1)
                        field_accessor = 'Convert_To_{type_} (Buffer ({slice_}))'.format(
                            type_=c.type.identifier, slice_=slice_)

                    # Validator function
                    condition = And(length_precondition,
                                    type_invariant.transformed(
                                        c.name,
                                        [k.name for k in previous_components + [c]],
                                        component_last)).simplified()
                    for field in previous_components:
                        condition.replace_field(field.name, '{} (Buffer)'.format(field.name))
                    condition.replace_field(c.name, field_accessor)
                    validator_body = [Statement('return {}'.format(condition.specification()))]

                    functions += [Function('Valid_{}'.format(c.name), 'Boolean', precondition,
                                           validator_body)]

                    # Accessor function
                    accessor_body = [Statement('return {}'.format(field_accessor))]
                    functions += [Function('{}'.format(c.name), c.type.identifier, precondition,
                                           accessor_body)]

                    # General validator function
                    if c is t.type.components[-1]:
                        functions += [Function('Is_Valid', 'Boolean', None, validator_body)]

                    previous_components += [c]

        context = [ContextItem('Types', True)]
        package = Package(package.identifier, types, functions)
        self.__units += [Unit(context, package)]

    def units(self):
        return self.__units


def main():
    p = parser.Parser()
    p.parse(sys.argv[1])
    generator = Generator()
    generator.generate(p.syntax_tree())


if __name__ == "__main__":
    main()
