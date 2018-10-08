#!/usr/bin/env python3

from abc import ABC, abstractmethod
from functools import reduce
from typing import Dict, List, Tuple

from model import (Add, And, Array, Attribute, Equal, Expr, Field, First, GreaterEqual, Last,
                   Length, LessEqual, LogExpr, MathExpr, ModularInteger, Number, Or, PDU,
                   RangeInteger, Sub, TRUE, Type, Value, Variant)


class SparkRepresentation(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        return "\n%s %s" % (self.__class__.__name__, self.__dict__)

    @abstractmethod
    def specification(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def definition(self) -> str:
        raise NotImplementedError


class Unit(SparkRepresentation):
    def __init__(self, context: List['ContextItem'], package: 'Package') -> None:
        self.context = context
        self.package = package

    def specification(self) -> str:
        context_clause = ''
        if self.context:
            context_clause = '{}\n\n'.format('\n'.join([c.specification() for c in self.context]))
        return '{}{}\n'.format(context_clause, self.package.specification())

    def definition(self) -> str:
        return '{}\n'.format(self.package.definition())


class ContextItem(SparkRepresentation):
    def __init__(self, name: str, use: bool) -> None:
        self.name = name
        self.use = use

    def specification(self) -> str:
        return 'with {};{}'.format(self.name, ' use {};'.format(self.name) if self.use else '')

    def definition(self) -> str:
        return ''


class Package(SparkRepresentation):
    def __init__(self, name: str, types: List['TypeDeclaration'],
                 subprograms: List['Subprogram']) -> None:
        self.name = name
        self.types = types
        self.subprograms = subprograms

    def specification(self) -> str:
        types = '\n\n'.join([t.specification() for t in self.types if t.specification()])
        if types:
            types += '\n\n'
        subprograms = '\n\n'.join([f.specification()
                                   for f in self.subprograms if f.specification()])
        if subprograms:
            subprograms += '\n\n'
        return 'package {name} is\n\n{types}{subprograms}end {name};'.format(
            name=self.name,
            types=types,
            subprograms=subprograms)

    def definition(self) -> str:
        if not self.subprograms:
            return ''
        types = '\n\n'.join([t.definition() for t in self.types if t.definition()])
        if types:
            types += '\n\n'
        subprograms = '\n\n'.join([f.definition() for f in self.subprograms if f.definition()])
        if subprograms:
            subprograms += '\n\n'
        return 'package body {name} is\n\n{types}{subprograms}end {name};'.format(
            name=self.name,
            types=types,
            subprograms=subprograms)


class TypeDeclaration(SparkRepresentation):
    def __init__(self, name: str) -> None:
        self.name = name

    def specification(self) -> str:
        type_declaration = ''
        return '{type_declaration}   function Convert_To_{name} is new Convert_To ({name});'.format(
            name=self.name,
            type_declaration=type_declaration)

    def definition(self) -> str:
        return ''


class ModularType(TypeDeclaration):
    def __init__(self, name: str, modulus: int) -> None:
        super().__init__(name)
        self.modulus = modulus

    def specification(self) -> str:
        declaration = '   type {name} is mod {modulus};\n'.format(
            name=self.name,
            modulus=self.modulus)
        return '{declaration}   function Convert_To_{name} is new Convert_To_Mod ({name});'.format(
            name=self.name,
            declaration=declaration)

    def definition(self) -> str:
        return ''


class RangeType(TypeDeclaration):
    def __init__(self, name: str, first: int, last: int, size: int) -> None:
        super().__init__(name)
        self.first = first
        self.last = last
        self.size = size

    def specification(self) -> str:
        declaration = '   type {name} is range {first} .. {last} with Size => {size};\n'.format(
            name=self.name,
            first=self.first,
            last=self.last,
            size=self.size)
        return '{declaration}   function Convert_To_{name} is new Convert_To_Int ({name});'.format(
            name=self.name,
            declaration=declaration)

    def definition(self) -> str:
        return ''


class Subprogram(SparkRepresentation):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, parameters: List[Tuple[str, str]], body: List['Statement'],
                 precondition: LogExpr = TRUE, postcondition: LogExpr = TRUE) -> None:
        self.name = name
        self.parameters = parameters
        self.precondition = precondition
        self.postcondition = postcondition
        self.body = body

    @abstractmethod
    def specification(self) -> str:
        raise NotImplementedError

    def _parameters(self) -> str:
        parameters = ''
        if self.parameters:
            parameters = ' ({})'.format(
                '; '.join(['{} : {}'.format(p_name, p_type) for p_name, p_type in self.parameters]))
        return parameters

    def _with_clause(self) -> str:
        if self.precondition is TRUE and self.postcondition is TRUE:
            return ''
        with_clause = '\n     with\n       '
        if self.precondition is not TRUE:
            with_clause += 'Pre => {}'.format(self.precondition)
            if self.postcondition is not TRUE:
                with_clause += ',\n       '
        if self.postcondition is not TRUE:
            with_clause += 'Post => {}'.format(self.postcondition)
        return with_clause


class Pragma(Subprogram):
    def __init__(self, name: str, parameters: List[str]) -> None:
        super().__init__(name, [], [])
        self.pragma_parameters = parameters

    def specification(self) -> str:
        parameters = ''
        if self.pragma_parameters:
            parameters = ' ({})'.format(', '.join(self.pragma_parameters))
        return '   pragma {}{};'.format(self.name, parameters)

    def definition(self) -> str:
        return ''


class Function(Subprogram):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, parameters: List[Tuple[str, str]], return_type: str,
                 body: List['Statement'], precondition: LogExpr = TRUE,
                 postcondition: LogExpr = TRUE) -> None:
        super().__init__(name, parameters, body, precondition, postcondition)
        self.return_type = return_type

    def specification(self) -> str:
        return '   function {}{} return {}{};'.format(
            self.name,
            self._parameters(),
            self.return_type,
            self._with_clause())

    def definition(self) -> str:
        return ('   function {name}{parameters} return {return_type} is\n'
                '   begin\n'
                '{body}\n'
                '   end {name};').format(
                    name=self.name,
                    parameters=self._parameters(),
                    return_type=self.return_type,
                    body='\n'.join([s.definition() for s in self.body]))


class ExpressionFunction(Subprogram):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, parameters: List[Tuple[str, str]], return_type: str,
                 expression: Expr, precondition: LogExpr = TRUE,
                 postcondition: LogExpr = TRUE) -> None:
        super().__init__(name, parameters, [], precondition, postcondition)
        self.return_type = return_type
        self.expression = expression

    def specification(self) -> str:
        return '   function {}{} return {} is\n      ({}){};'.format(
            self.name,
            self._parameters(),
            self.return_type,
            str(self.expression),
            self._with_clause())

    def definition(self) -> str:
        return ''


class Procedure(Subprogram):
    def specification(self) -> str:
        return '   procedure {}{}{};'.format(
            self.name,
            self._parameters(),
            self._with_clause())

    def definition(self) -> str:
        return ('   procedure {name}{parameters} is\n'
                '   begin\n'
                '{body}\n'
                '   end {name};').format(
                    name=self.name,
                    parameters=self._parameters(),
                    body='\n'.join([s.definition() for s in self.body]))


class IfExpression(SparkRepresentation, LogExpr):
    def __init__(self, condition_expressions: List[Tuple[LogExpr, Expr]],
                 else_expression: str) -> None:
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

    def __str__(self) -> str:
        result = ''
        for c, e in self.condition_expressions:
            if not result:
                result = '(if {} then {}'.format(c, e)
            else:
                result += ' elsif {} then {}'.format(c, e)
        result += ' else {})'.format(self.else_expression)
        return result

    def specification(self) -> str:
        return str(self)

    def definition(self) -> str:
        return self.specification()

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> LogExpr:
        return self


class Statement(SparkRepresentation):
    def specification(self) -> str:
        raise RuntimeError('statement in specification')

    @abstractmethod
    def definition(self) -> str:
        raise NotImplementedError


class Assignment(Statement):
    def __init__(self, name: str, expression: MathExpr) -> None:
        self.name = name
        self.expression = expression

    def definition(self) -> str:
        return '      {} := {}'.format(self.name, str(self.expression))


class IfStatement(Statement):
    def __init__(self, condition_statements: List[Tuple[LogExpr, List[Statement]]],
                 else_statements: List[Statement]) -> None:
        self.condition_statements = condition_statements
        self.else_statements = else_statements

    def specification(self) -> str:
        raise RuntimeError('if statement in specification')

    def definition(self) -> str:
        result = ''
        for condition, statements in self.condition_statements:
            if not result:
                result = '      if {} then\n'.format(condition)
            else:
                result += '      elsif {} then\n'.format(condition)
            for statement in statements:
                result += '   {};\n'.format(statement.definition())
        result += '      else\n'
        for statement in self.else_statements:
            result += '   {};\n'.format(statement.definition())
        result += '      end if;'
        return result


class Call(ABC):
    def __init__(self, call: str) -> None:
        self.call = call

    def __repr__(self) -> str:
        return '{}({})'.format(self.__class__.__name__, self.call)

    def __str__(self) -> str:
        return self.call


class MathCall(Call, MathExpr):
    def __init__(self, call: str, negative: bool = False) -> None:
        super().__init__(call)
        self.negative = negative

    def __repr__(self) -> str:
        result = '{}({})'.format(self.__class__.__name__, self.call)
        if self.negative:
            return '(-{})'.format(result)
        return result

    def __neg__(self) -> MathExpr:
        return self.__class__(self.call, not self.negative)

    def to_bytes(self) -> MathExpr:
        return self

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return self


class LogCall(Call, LogExpr):
    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> LogExpr:
        return self


class Convert(MathExpr):
    # pylint: disable=too-many-arguments
    def __init__(self, type_name: str, array_name: str, first: MathExpr, last: MathExpr,
                 negative: bool = False) -> None:
        self.type_name = type_name
        self.array_name = array_name
        self.first = first
        self.last = last
        self.negative = negative

    def __repr__(self) -> str:
        return '{}{}({}, {}, {})'.format('-' if self.negative else '',
                                         self.__class__.__name__,
                                         self.type_name,
                                         self.first,
                                         self.last)

    def __str__(self) -> str:
        return '{}Convert_To_{} ({} ({} .. {}))'.format(
            '-1 * ' if self.negative else '',
            self.type_name,
            self.array_name,
            self.first,
            self.last)

    def __neg__(self) -> MathExpr:
        return Convert(self.type_name, self.array_name, self.first, self.last, not self.negative)

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first.simplified(facts),
                       self.last.simplified(facts))

    def to_bytes(self) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first.to_bytes(),
                       self.last.to_bytes())


class Cast(MathExpr):
    def __init__(self, name: str, expression: MathExpr) -> None:
        self.name = name
        self.expression = expression

    def __repr__(self) -> str:
        return '{} ({})'.format(self.name, self.expression)

    def __neg__(self) -> MathExpr:
        return Cast(self.name, -self.expression)

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return Cast(self.name, self.expression.simplified(facts))

    def to_bytes(self) -> MathExpr:
        return Cast(self.name, self.expression.to_bytes())


class FalseExpr(LogExpr):
    def __repr__(self) -> str:
        return 'FALSE'

    def __str__(self) -> str:
        return 'False'

    def simplified(self, facts: Dict['Attribute', 'MathExpr'] = None) -> LogExpr:
        return self


FALSE = FalseExpr()


class Generator:
    def __init__(self) -> None:
        self.__units: List[Unit] = []

    def generate_dissector(self, pdus: List[PDU]) -> None:
        for pdu in pdus:
            context = [ContextItem('Types', True)]
            package = Package(pdu.name, [], [])
            self.__units += [Unit(context, package)]

            field_types: Dict[str, Type] = {}
            seen_types: List[Type] = []
            unreachable_functions: Dict[str, Subprogram] = {}

            for field in pdu.fields(first=First('Buffer')):
                field_types[field.name] = field.type
                if field.type not in seen_types:
                    seen_types.append(field.type)
                    if isinstance(field.type, ModularInteger):
                        package.types += [ModularType(field.type.name,
                                                      field.type.modulus)]
                    elif isinstance(field.type, RangeInteger):
                        package.types += [RangeType(field.type.name,
                                                    field.type.first,
                                                    field.type.last,
                                                    int(field.type.size()))]
                    elif isinstance(field.type, Array):
                        if 'Payload' not in field.type.name:
                            raise NotImplementedError('custom arrays are not supported yet')

                valid_variants: List[LogExpr] = []

                for variant_id, variant in field.variants.items():
                    facts = convert_facts_to_bytes(variant.facts)
                    facts.update(create_length_facts(facts))

                    package.subprograms.append(
                        create_variant_validation_function(
                            field,
                            variant_id,
                            variant,
                            facts))

                    package.subprograms.extend(
                        create_variant_accessor_functions(
                            field,
                            variant_id,
                            variant,
                            facts,
                            field_types))

                    extend_valid_variants(valid_variants, field, variant_id, variant, facts)

                package.subprograms.append(
                    create_field_validation_function(
                        field.name,
                        valid_variants))

                package.subprograms.append(
                    create_field_accessor_function(
                        field))

                extend_unreachable_functions(unreachable_functions, field.type)

            package.subprograms.insert(0, Pragma('Warnings',
                                                 ['On', '"precondition is statically false"']))
            package.subprograms[0:0] = list(unreachable_functions.values())
            package.subprograms.insert(0, Pragma('Warnings',
                                                 ['Off', '"precondition is statically false"']))

            package.subprograms.append(
                create_packet_validation_function(
                    pdu.fields()[-1].name))

    def units(self) -> List[Unit]:
        return self.__units


def unique(input_list: List) -> List:
    return reduce(lambda l, x: l + [x] if x not in l else l, input_list, [])


def length_constraint(last: MathExpr) -> LogExpr:
    return GreaterEqual(Length('Buffer'),
                        Add(last, -First('Buffer'), Number(1)))


def convert_facts_to_bytes(facts: Dict[Attribute, MathExpr]) -> Dict[Attribute, MathExpr]:
    return {attr: expr.to_bytes() for (attr, expr) in facts.items()}


def create_length_facts(facts: Dict[Attribute, MathExpr]) -> Dict[Attribute, MathExpr]:
    return {Length(a.name): Add(facts[Last(a.name)], -facts[First(a.name)], Number(1)).simplified()
            for a in facts if isinstance(a, First)}


def create_value_to_call(
        field: Field,
        variant_id: str,
        variant: Variant) -> Dict[Attribute, MathExpr]:

    return {Value(field_name): MathCall('{}_{} (Buffer)'.format(field_name, vid))
            for field_name, vid in [(field.name, variant_id)] + variant.previous}


def create_value_to_natural_call(
        field: Field,
        variant_id: str,
        variant: Variant) -> Dict[Attribute, MathExpr]:

    return {Value(field_name): Cast('Natural', MathCall('{}_{} (Buffer)'.format(field_name, vid)))
            for field_name, vid in [(field.name, variant_id)] + variant.previous}


def create_value_to_natural_convert(
        field: Field,
        variant: Variant,
        field_types: Dict[str, Type]) -> Dict[Attribute, MathExpr]:

    return {Value(field_name): Cast('Natural',
                                    Convert(field_types[field_name].name,
                                            'Buffer',
                                            First(field_name),
                                            Last(field_name)))
            for field_name, _ in [(field.name, '')] + variant.previous}


def create_variant_validation_function(
        field: Field,
        variant_id: str,
        variant: Variant,
        facts: Dict[Attribute, MathExpr]) -> Subprogram:

    return ExpressionFunction(
        'Valid_{}_{}'.format(field.name, variant_id),
        [('Buffer', 'Bytes')],
        'Boolean',
        And(LogCall('Valid_{}_{} (Buffer)'.format(variant.previous[-1][0],
                                                  variant.previous[-1][1]))
            if variant.previous else TRUE,
            And(
                length_constraint(
                    facts[Last(field.name)]).simplified(
                        create_value_to_natural_call(
                            field, variant_id, variant)),
                variant.condition.simplified(
                    create_value_to_call(
                        field, variant_id, variant)))
            ).simplified())


def create_variant_accessor_functions(
        field: Field,
        variant_id: str,
        variant: Variant,
        facts: Dict[Attribute, MathExpr],
        field_types: Dict[str, Type]) -> List[Subprogram]:

    value_to_natural_convert = {attr: expr.simplified(facts)
                                for (attr, expr) in create_value_to_natural_convert(
                                    field, variant, field_types).items()}
    first = facts[First(field.name)].simplified(value_to_natural_convert)
    last = facts[Last(field.name)].simplified(value_to_natural_convert)

    functions: List[Subprogram] = []
    if 'Payload' in field.type.name:
        functions.append(
            ExpressionFunction(
                '{}_{}_First'.format(field.name, variant_id),
                [('Buffer', 'Bytes')],
                'Natural',
                first,
                And(LogCall('Valid_{}_{} (Buffer)'.format(field.name, variant_id)),
                    LessEqual(First('Buffer'),
                              Sub(Last('Natural'), first).simplified({First('Buffer'):
                                                                      Number(0)})))))
        functions.append(
            ExpressionFunction(
                '{}_{}_Last'.format(field.name, variant_id),
                [('Buffer', 'Bytes')],
                'Natural',
                last,
                LogCall('Valid_{}_{} (Buffer)'.format(field.name, variant_id))))
    else:
        functions.append(
            ExpressionFunction(
                '{}_{}'.format(field.name, variant_id),
                [('Buffer', 'Bytes')],
                field.type.name,
                Convert(
                    field.type.name,
                    'Buffer',
                    first,
                    last),
                LogCall('Valid_{}_{} (Buffer)'.format(field.name, variant_id))))
    return functions


def extend_valid_variants(
        valid_variants: List[LogExpr],
        field: Field,
        variant_id: str,
        variant: Variant,
        facts: Dict[Attribute, MathExpr]) -> None:

    expression: LogExpr = LogCall('Valid_{}_{} (Buffer)'.format(field.name, variant_id))
    if field.condition is not TRUE:
        expression = And(expression, field.condition)
    valid_variants.append(expression.simplified(facts).simplified(
        create_value_to_call(field, variant_id, variant)))


def create_field_validation_function(
        field_name: str,
        valid_variants: List[LogExpr]) -> Subprogram:

    expr = valid_variants.pop()
    for e in valid_variants:
        if e is not TRUE:
            expr = Or(expr, e)

    return ExpressionFunction(
        'Valid_{}'.format(field_name),
        [('Buffer', 'Bytes')],
        'Boolean',
        expr)


def extend_unreachable_functions(
        unreachable_functions: Dict[str, Subprogram],
        field_type: Type) -> None:

    if field_type.name not in unreachable_functions:
        if isinstance(field_type, Array):
            if 'Unreachable' not in unreachable_functions:
                unreachable_functions[field_type.name] = ExpressionFunction(
                    'Unreachable',
                    [],
                    'Boolean',
                    FALSE,
                    FALSE)
        else:
            unreachable_functions[field_type.name] = ExpressionFunction(
                'Unreachable_{}'.format(field_type.name),
                [],
                field_type.name,
                First(field_type.name),
                FALSE)


def create_field_accessor_function(field: Field) -> Subprogram:
    assignments: List[Tuple[LogExpr, List[Statement]]] = []
    preconditions: List[Tuple[LogExpr, Expr]] = []

    if 'Payload' in field.type.name:
        for variant_id in field.variants:
            first_call = MathCall('{}_{}_First (Buffer)'.format(field.name, variant_id))
            last_call = MathCall('{}_{}_Last (Buffer)'.format(field.name, variant_id))
            assignments.append(
                (
                    LogCall('Valid_{}_{} (Buffer)'.format(field.name, variant_id)),
                    [Assignment('First', first_call),
                     Assignment('Last', last_call)]
                )
            )
            preconditions.append(
                (
                    LogCall('Valid_{}_{} (Buffer)'.format(field.name, variant_id)),
                    And(Equal(Value('First'), first_call),
                        Equal(Value('Last'), last_call))
                )
            )

        return Procedure(
            field.name,
            [('Buffer', 'Bytes'),
             ('First', 'out Natural'),
             ('Last', 'out Natural')],
            [IfStatement(assignments,
                         [Assignment('First', Last('Buffer')),
                          Assignment('Last', First('Buffer'))])],
            LogCall('Valid_{} (Buffer)'.format(field.name)),
            IfExpression(preconditions,
                         'Unreachable'))

    return ExpressionFunction(
        field.name,
        [('Buffer', 'Bytes')],
        field.type.name,
        IfExpression([(LogCall('Valid_{}_{} (Buffer)'.format(field.name, variant_id)),
                       MathCall('{}_{} (Buffer)'.format(field.name, variant_id)))
                      for variant_id in field.variants],
                     'Unreachable_{}'.format(field.type.name)),
        LogCall('Valid_{} (Buffer)'.format(field.name)))


def create_packet_validation_function(field_name: str) -> Subprogram:
    return ExpressionFunction(
        'Is_Valid',
        [('Buffer', 'Bytes')],
        'Boolean',
        LogCall('Valid_{} (Buffer)'.format(field_name)))
