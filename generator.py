from abc import ABC, abstractmethod, abstractproperty
from typing import Callable, Dict, List, Tuple

from model import (Add, And, Array, Attribute, Div, Equal, Expr, Field, First, GreaterEqual, Last,
                   Length, LessEqual, LogExpr, MathExpr, ModularInteger, Mul, Number, Or, PDU,
                   RangeInteger, Refinement, Sub, TRUE, Type, Value, Variant)


class SparkRepresentation(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __repr__(self) -> str:
        args = '\n\t' + ',\n\t'.join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f'{self.__class__.__name__}({args})'.replace('\t', '\t    ')

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
            context_clause = '\n'.join([str(c) for c in self.context])
            context_clause = f'{context_clause}\n\n'
        return f'{context_clause}{self.package.specification()}\n'

    def definition(self) -> str:
        return f'{self.package.definition()}\n'


class ContextItem:
    def __init__(self, name: str, use: bool) -> None:
        self.name = name
        self.use = use

    def __str__(self) -> str:
        use = f' use {self.name};' if self.use else ''
        return f'with {self.name};{use}'


class Package(SparkRepresentation):
    def __init__(self, name: str, types: List['TypeDeclaration'],
                 subprograms: List['Subprogram']) -> None:
        self.name = name
        self.types = types
        self.subprograms = subprograms

    def specification(self) -> str:
        return self.__representation(lambda x: x.specification(), False)

    def definition(self) -> str:
        return self.__representation(lambda x: x.definition(), True)

    def __representation(self, function: Callable, definition: bool) -> str:
        types = ''
        if not definition:
            types = '\n\n'.join([str(t) for t in self.types if str(t)])
            if types:
                types += '\n\n'

        subprograms = '\n\n'.join([function(f) for f in self.subprograms if function(f)])
        if subprograms:
            subprograms += '\n\n'

        if not types and not subprograms:
            return ''

        indicator = ' '
        aspect = '\n  with SPARK_Mode\n'
        if definition:
            indicator = ' body '
            aspect = ' '

        return f'package{indicator}{self.name}{aspect}is\n\n{types}{subprograms}end {self.name};'


class TypeDeclaration(ABC):
    def __init__(self, name: str) -> None:
        self.name = name


class ModularType(TypeDeclaration):
    def __init__(self, name: str, modulus: MathExpr) -> None:
        super().__init__(name)
        self.modulus = modulus

    def __str__(self) -> str:
        return (f'   type {self.name} is mod {self.modulus};\n'
                f'   function Convert_To_{self.name} is new Convert_To_Mod ({self.name});')


class RangeType(TypeDeclaration):
    def __init__(self, name: str, first: MathExpr, last: MathExpr, size: MathExpr) -> None:
        super().__init__(name)
        self.first = first
        self.last = last
        self.size = size

    def __str__(self) -> str:
        return (f'   type {self.name} is range {self.first} .. {self.last}'
                f' with Size => {self.size};\n'
                f'   function Convert_To_{self.name} is new Convert_To_Int ({self.name});')


class RangeSubtype(TypeDeclaration):
    def __init__(self, name: str, base_name: str, first: MathExpr, last: MathExpr) -> None:
        super().__init__(name)
        self.base_name = base_name
        self.first = first
        self.last = last

    def __str__(self) -> str:
        return f'   subtype {self.name} is {self.base_name} range {self.first} .. {self.last};'


class Aspect(ABC):
    def __str__(self) -> str:
        if self.definition:
            return f'{self.mark} => {self.definition}'
        return f'{self.mark}'

    @abstractproperty
    def mark(self) -> str:
        raise NotImplementedError

    @abstractproperty
    def definition(self) -> str:
        raise NotImplementedError


class Precondition(Aspect):
    def __init__(self, expr: LogExpr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return 'Pre'

    @property
    def definition(self) -> str:
        return str(self.expr)


class Postcondition(Aspect):
    def __init__(self, expr: LogExpr) -> None:
        self.expr = expr

    @property
    def mark(self) -> str:
        return 'Post'

    @property
    def definition(self) -> str:
        return str(self.expr)


class Ghost(Aspect):
    @property
    def mark(self) -> str:
        return 'Ghost'

    @property
    def definition(self) -> str:
        return ''


class Import(Aspect):
    @property
    def mark(self) -> str:
        return 'Import'

    @property
    def definition(self) -> str:
        return ''


class Subprogram(SparkRepresentation):
    def __init__(self, name: str, parameters: List[Tuple[str, str]] = None,
                 body: List['Statement'] = None, aspects: List[Aspect] = None) -> None:
        self.name = name
        self.parameters = parameters or []
        self.body = body or []
        self.aspects = aspects or []

    @abstractmethod
    def specification(self) -> str:
        raise NotImplementedError

    def _parameters(self) -> str:
        parameters = ''
        if self.parameters:
            parameters = '; '.join([f'{p_name} : {p_type}' for p_name, p_type in self.parameters])
            parameters = f' ({parameters})'
        return parameters

    def _body(self) -> str:
        return '\n'.join([str(s) for s in self.body])

    def _with_clause(self) -> str:
        if not self.aspects:
            return ''
        with_clause = '\n     with\n       '
        for i, aspect in enumerate(self.aspects):
            with_clause += str(aspect)
            if i + 1 < len(self.aspects):
                with_clause += ',\n       '
        return with_clause


class Pragma(Subprogram):
    def __init__(self, name: str, parameters: List[str]) -> None:
        super().__init__(name)
        self.pragma_parameters = parameters

    def specification(self) -> str:
        parameters = ''
        if self.pragma_parameters:
            parameters = ', '.join(self.pragma_parameters)
            parameters = f' ({parameters})'
        return f'   pragma {self.name}{parameters};'

    def definition(self) -> str:
        return ''


class Function(Subprogram):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, return_type: str, parameters: List[Tuple[str, str]] = None,
                 body: List['Statement'] = None, aspects: List[Aspect] = None) -> None:
        super().__init__(name, parameters, body, aspects)
        self.return_type = return_type

    def specification(self) -> str:
        return (f'   function {self.name}{self._parameters()} return {self.return_type}'
                f'{self._with_clause()};')

    def definition(self) -> str:
        return (f'   function {self.name}{self._parameters()} return {self.return_type} is\n'
                f'   begin\n'
                f'{self._body()}\n'
                f'   end {self.name};')


class ExpressionFunction(Subprogram):
    # pylint: disable=too-many-arguments
    def __init__(self, name: str, return_type: str, parameters: List[Tuple[str, str]] = None,
                 expression: Expr = None, aspects: List[Aspect] = None) -> None:
        super().__init__(name, parameters, aspects=aspects)
        self.return_type = return_type
        self.expression = expression

    def specification(self) -> str:
        signature = f'   function {self.name}{self._parameters()} return {self.return_type}'
        if self.expression:
            return f'{signature} is\n      ({self.expression!s}){self._with_clause()};'
        return f'{signature}{self._with_clause()};'

    def definition(self) -> str:
        return ''


class Procedure(Subprogram):
    def specification(self) -> str:
        return f'   procedure {self.name}{self._parameters()}{self._with_clause()};'

    def definition(self) -> str:
        return (f'   procedure {self.name}{self._parameters()} is\n'
                f'   begin\n'
                f'{self._body()}\n'
                f'   end {self.name};')


class IfExpression(LogExpr):
    def __init__(self, condition_expressions: List[Tuple[LogExpr, Expr]],
                 else_expression: str = '') -> None:
        self.condition_expressions = condition_expressions
        self.else_expression = else_expression

    def __str__(self) -> str:
        result = ''
        for c, e in self.condition_expressions:
            if not result:
                result = f'(if {c} then {e}'
            else:
                result += f' elsif {c} then {e}'
        if self.else_expression:
            result += f' else {self.else_expression}'
        result += ')'
        return result

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> LogExpr:
        return self

    def symbol(self) -> str:
        raise NotImplementedError


class Statement(ABC):
    pass


class Assignment(Statement):
    def __init__(self, name: str, expression: MathExpr) -> None:
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f'      {self.name} := {self.expression};'


class PragmaStatement(Statement):
    def __init__(self, name: str, parameters: List[str]) -> None:
        self.name = name
        self.pragma_parameters = parameters

    def __str__(self) -> str:
        parameters = ''
        if self.pragma_parameters:
            parameters = ', '.join(self.pragma_parameters)
            parameters = f' ({parameters})'
        return f'      pragma {self.name}{parameters};'


class ReturnStatement(Statement):
    def __init__(self, expression: Expr) -> None:
        self.expression = expression

    def __str__(self) -> str:
        return f'      return {self.expression};'


class IfStatement(Statement):
    def __init__(self, condition_statements: List[Tuple[LogExpr, List[Statement]]],
                 else_statements: List[Statement] = None) -> None:
        self.condition_statements = condition_statements
        self.else_statements = else_statements

    def __str__(self) -> str:
        result = ''
        for condition, statements in self.condition_statements:
            if not result:
                result = f'      if {condition} then\n'
            else:
                result += f'      elsif {condition} then\n'
            for statement in statements:
                result += f'   {statement}\n'
        if self.else_statements:
            result += '      else\n'
            for statement in self.else_statements:
                result += f'   {statement}\n'
        result += '      end if;'
        return result


class Call(ABC):
    def __init__(self, call: str) -> None:
        self.call = call

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}({self.call})'

    def __str__(self) -> str:
        return self.call


class MathCall(Call, MathExpr):
    def __init__(self, call: str, negative: bool = False) -> None:
        super().__init__(call)
        self.negative = negative

    def __repr__(self) -> str:
        result = f'{self.__class__.__name__}({self.call})'
        if self.negative:
            return f'(-{result})'
        return result

    def __neg__(self) -> MathExpr:
        return self.__class__(self.call, not self.negative)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

    def to_bytes(self) -> MathExpr:
        return self

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return self


class LogCall(Call, LogExpr):
    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> LogExpr:
        return self

    def symbol(self) -> str:
        raise NotImplementedError


class Convert(MathExpr):
    # pylint: disable=too-many-arguments
    def __init__(self, type_name: str, array_name: str, first: MathExpr, last: MathExpr,
                 offset: int = 0, negative: bool = False) -> None:
        self.type_name = type_name
        self.array_name = array_name
        self.first = first
        self.last = last
        self.offset = offset
        self.negative = negative

    def __str__(self) -> str:
        negative = '-1 * ' if self.negative else ''
        offset = f', {self.offset}' if self.offset else ''
        return (f'{negative}'
                f'Convert_To_{self.type_name} ({self.array_name} ({self.first} .. {self.last})'
                f'{offset}'
                ')')

    def __neg__(self) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first,
                       self.last,
                       self.offset,
                       not self.negative)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

    def simplified(self, facts: Dict[Attribute, MathExpr] = None) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first.simplified(facts),
                       self.last.simplified(facts),
                       self.offset,
                       self.negative)

    def to_bytes(self) -> MathExpr:
        return Convert(self.type_name,
                       self.array_name,
                       self.first.to_bytes(),
                       self.last.to_bytes(),
                       self.offset,
                       self.negative)


class Cast(MathExpr):
    def __init__(self, name: str, expression: MathExpr) -> None:
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f'{self.name} ({self.expression})'

    def __neg__(self) -> MathExpr:
        return Cast(self.name, -self.expression)

    def __contains__(self, item: MathExpr) -> bool:
        return item == self

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

    def symbol(self) -> str:
        raise NotImplementedError


FALSE = FalseExpr()


class Generator:
    def __init__(self) -> None:
        self.__units: Dict[str, Unit] = {}
        self.__pdu_fields: Dict[str, List[str]] = {}

    def generate_dissector(self, pdus: List[PDU], refinements: List[Refinement]) -> None:
        self.__process_pdus(pdus)
        self.__process_refinements(refinements)

    def units(self) -> List[Unit]:
        return list(self.__units.values())

    def __process_pdus(self, pdus: List[PDU]) -> None:
        for pdu in pdus:
            if pdu.package in self.__units:
                top_level_package = self.__units[pdu.package].package
            else:
                top_level_package = Package(pdu.package, [], [])
                self.__units[pdu.package] = Unit([ContextItem('Types', True)],
                                                 top_level_package)

            package = Package(pdu.full_name, [], [])
            self.__units[pdu.full_name] = Unit([], package)

            package.subprograms.extend(
                create_contain_functions())

            seen_types: List[Type] = []
            unreachable_functions: Dict[str, Subprogram] = {}

            facts = {
                First('Message'): First('Buffer'),
                Last('Message'): Mul(Last('Buffer'), Number(8)),
                Length('Message'): Sub(Mul(Last('Buffer'), Number(8)), First('Buffer'))
            }

            fields = pdu.fields(facts, First('Buffer'))
            self.__pdu_fields[pdu.full_name] = list(fields.keys())

            for field in fields.values():
                if field.type not in seen_types:
                    seen_types.append(field.type)
                    if isinstance(field.type, ModularInteger):
                        top_level_package.types += [ModularType(field.type.name,
                                                                field.type.modulus)]
                    elif isinstance(field.type, RangeInteger):
                        if field.type.constraints == TRUE:
                            top_level_package.types += [RangeType(field.type.name,
                                                                  field.type.first,
                                                                  field.type.last,
                                                                  field.type.size)]
                        else:
                            top_level_package.types += [RangeType(field.type.base_name,
                                                                  field.type.base_first,
                                                                  field.type.base_last,
                                                                  field.type.size)]
                            top_level_package.types += [RangeSubtype(field.type.name,
                                                                     field.type.base_name,
                                                                     field.type.first,
                                                                     field.type.last)]

                    elif isinstance(field.type, Array):
                        if 'Payload' not in field.type.name:
                            raise NotImplementedError('custom arrays are not supported yet')

                valid_variants: List[LogExpr] = []

                for variant_id, variant in field.variants.items():
                    package.subprograms.append(
                        create_variant_validation_function(
                            field,
                            variant_id,
                            variant))

                    package.subprograms.extend(
                        create_variant_accessor_functions(
                            field,
                            variant_id,
                            variant))

                    extend_valid_variants(valid_variants, field, variant_id, variant)

                package.subprograms.append(
                    create_field_validation_function(
                        field.name,
                        valid_variants))

                package.subprograms.extend(
                    create_field_accessor_functions(
                        field))

                extend_unreachable_functions(unreachable_functions, field.type)

            package.subprograms.insert(0, Pragma('Warnings',
                                                 ['On', '"precondition is statically false"']))
            package.subprograms[0:0] = list(unreachable_functions.values())
            package.subprograms.insert(0, Pragma('Warnings',
                                                 ['Off', '"precondition is statically false"']))

            package.subprograms.append(
                create_packet_validation_function(
                    list(fields.values())[-1].name))

    def __process_refinements(self, refinements: List[Refinement]) -> None:
        for refinement in refinements:
            if refinement.package in self.__units:
                context = self.__units[refinement.package].context
                package = self.__units[refinement.package].package
            else:
                context = [ContextItem('Types', True)]
                package = Package(refinement.package, [], [])
                self.__units[refinement.package] = Unit(context, package)

            pdu_top_level_context = ContextItem(refinement.pdu.rsplit('.', 1)[0], True)
            if pdu_top_level_context not in context:
                context.append(pdu_top_level_context)
            pdu_context = ContextItem(refinement.pdu, False)
            if pdu_context not in context:
                context.append(pdu_context)
            sdu_context = ContextItem(refinement.sdu, False)
            if sdu_context not in context:
                context.append(sdu_context)

            package.subprograms.append(
                create_contains_function(
                    refinement.unqualified_name,
                    refinement.pdu,
                    refinement.field,
                    refinement.sdu,
                    refinement.condition.simplified(
                        {Value(field): MathCall(f'{refinement.pdu}.{field} (Buffer)')
                         for field in self.__pdu_fields[refinement.pdu]})))


COMMON_PRECONDITION = LogCall('Is_Contained (Buffer)')


def create_contain_functions() -> List[Subprogram]:
    return [ExpressionFunction('Is_Contained',
                               'Boolean',
                               [('Buffer', 'Bytes')],
                               aspects=[Ghost(), Import()]),
            Procedure('Initialize',
                      [('Buffer', 'Bytes')],
                      [PragmaStatement('Assume', ['Is_Contained (Buffer)'])],
                      aspects=[Postcondition(LogCall('Is_Contained (Buffer)'))])]


def calculate_offset(last: MathExpr) -> int:
    last = last.simplified({First('Buffer'): Number(0)})
    if isinstance(last, Number):
        return (8 - (last.value + 1) % 8) % 8
    # TODO: determine offset for complicated cases
    return 0


def buffer_constraints(last: MathExpr) -> LogExpr:
    return And(GreaterEqual(Length('Buffer'),
                            Add(last, -First('Buffer'), Number(1))),
               LessEqual(First('Buffer'), Div(Last('Natural'), Number(2))))


def create_value_to_call(
        field: Field,
        variant_id: str,
        variant: Variant) -> Dict[Attribute, MathExpr]:

    return {Value(field_name): MathCall(f'{field_name}_{vid} (Buffer)')
            for field_name, vid in [(field.name, variant_id)] + variant.previous}


def create_value_to_natural_call(
        field: Field,
        variant_id: str,
        variant: Variant) -> Dict[Attribute, MathExpr]:

    return {Value(field_name): Cast('Natural', MathCall(f'{field_name}_{vid} (Buffer)'))
            for field_name, vid in [(field.name, variant_id)] + variant.previous}


def create_variant_validation_function(
        field: Field,
        variant_id: str,
        variant: Variant) -> Subprogram:

    type_constraints: LogExpr = TRUE

    if field.type.constraints != TRUE:
        value_to_natural_call = create_value_to_natural_call(field, variant_id, variant)
        first_byte = variant.facts[First(field.name)].to_bytes().simplified(value_to_natural_call)
        last_byte = variant.facts[Last(field.name)].to_bytes().simplified(value_to_natural_call)
        offset = calculate_offset(variant.facts[Last(field.name)])

        convert = Convert(
            field.type.base_name,
            'Buffer',
            first_byte,
            last_byte,
            offset)
        type_constraints = field.type.constraints.simplified({Value(field.type.name): convert})

    return ExpressionFunction(
        f'Valid_{field.name}_{variant_id}',
        'Boolean',
        [('Buffer', 'Bytes')],
        And(LogCall(f'Valid_{variant.previous[-1][0]}_{variant.previous[-1][1]} (Buffer)')
            if variant.previous else TRUE,
            And(
                And(
                    buffer_constraints(
                        variant.facts[Last(field.name)].to_bytes()).simplified(
                            create_value_to_natural_call(
                                field, variant_id, variant)),
                    variant.condition.simplified(
                        create_value_to_call(
                            field, variant_id, variant))),
                type_constraints)
            ).simplified(),
        [Precondition(COMMON_PRECONDITION)])


def create_variant_accessor_functions(
        field: Field,
        variant_id: str,
        variant: Variant) -> List[Subprogram]:

    value_to_natural_call = create_value_to_natural_call(field, variant_id, variant)
    first_byte = variant.facts[First(field.name)].to_bytes().simplified(value_to_natural_call)
    last_byte = variant.facts[Last(field.name)].to_bytes().simplified(value_to_natural_call)
    offset = calculate_offset(variant.facts[Last(field.name)])

    name = f'{field.name}_{variant_id}'
    precondition = Precondition(
        And(COMMON_PRECONDITION,
            LogCall(f'Valid_{name} (Buffer)')))

    functions: List[Subprogram] = []
    if 'Payload' in field.type.name:
        functions.append(
            ExpressionFunction(
                f'{name}_First',
                'Natural',
                [('Buffer', 'Bytes')],
                first_byte,
                [precondition]))
        functions.append(
            ExpressionFunction(
                f'{name}_Last',
                'Natural',
                [('Buffer', 'Bytes')],
                last_byte,
                [precondition]))
    else:
        functions.append(
            ExpressionFunction(
                name,
                field.type.name,
                [('Buffer', 'Bytes')],
                Convert(
                    field.type.name if field.type.constraints == TRUE else field.type.base_name,
                    'Buffer',
                    first_byte,
                    last_byte,
                    offset),
                [precondition]))
    return functions


def extend_valid_variants(
        valid_variants: List[LogExpr],
        field: Field,
        variant_id: str,
        variant: Variant) -> None:

    expression: LogExpr = LogCall(f'Valid_{field.name}_{variant_id} (Buffer)')
    if field.condition is not TRUE:
        expression = And(expression, field.condition)
    valid_variants.append(
        expression.simplified({**variant.facts,
                               **create_value_to_call(field, variant_id, variant)}))


def create_field_validation_function(
        field_name: str,
        valid_variants: List[LogExpr]) -> Subprogram:

    expr = valid_variants.pop()
    for e in valid_variants:
        if e is not TRUE:
            expr = Or(expr, e)

    return ExpressionFunction(
        f'Valid_{field_name}',
        'Boolean',
        [('Buffer', 'Bytes')],
        expr,
        [Precondition(COMMON_PRECONDITION)])


def extend_unreachable_functions(
        unreachable_functions: Dict[str, Subprogram],
        field_type: Type) -> None:

    if field_type.name not in unreachable_functions:
        if isinstance(field_type, Array):
            if 'Unreachable_Natural' not in unreachable_functions:
                unreachable_functions['Unreachable_Natural'] = ExpressionFunction(
                    'Unreachable_Natural',
                    'Natural',
                    [],
                    First('Natural'),
                    [Precondition(FALSE)])
        else:
            unreachable_functions[field_type.name] = ExpressionFunction(
                f'Unreachable_{field_type.name}',
                field_type.name,
                [],
                First(field_type.name),
                [Precondition(FALSE)])


def create_field_accessor_functions(field: Field) -> List[Subprogram]:
    precondition = Precondition(And(COMMON_PRECONDITION,
                                    LogCall(f'Valid_{field.name} (Buffer)')))

    functions: List[Subprogram] = []
    if 'Payload' in field.type.name:
        for attribute in ['First', 'Last']:
            functions.append(
                ExpressionFunction(
                    f'{field.name}_{attribute}',
                    'Natural',
                    [('Buffer', 'Bytes')],
                    IfExpression([(LogCall(f'Valid_{field.name}_{variant_id} (Buffer)'),
                                   LogCall(f'{field.name}_{variant_id}_{attribute} (Buffer)'))
                                  for variant_id in field.variants],
                                 'Unreachable_Natural'),
                    [precondition]))

        functions.append(
            Procedure(
                field.name,
                [('Buffer', 'Bytes'),
                 ('First', 'out Natural'),
                 ('Last', 'out Natural')],
                [Assignment('First', MathCall(f'{field.name}_First (Buffer)')),
                 Assignment('Last', MathCall(f'{field.name}_Last (Buffer)'))],
                [precondition,
                 Postcondition(And(Equal(Value('First'),
                                         MathCall(f'{field.name}_First (Buffer)')),
                                   Equal(Value('Last'),
                                         MathCall(f'{field.name}_Last (Buffer)'))))]))

    else:
        functions.append(
            ExpressionFunction(
                field.name,
                field.type.name,
                [('Buffer', 'Bytes')],
                IfExpression([(LogCall(f'Valid_{field.name}_{variant_id} (Buffer)'),
                               MathCall(f'{field.name}_{variant_id} (Buffer)'))
                              for variant_id in field.variants],
                             f'Unreachable_{field.type.name}'),
                [precondition]))

    return functions


def create_packet_validation_function(field_name: str) -> Subprogram:
    return ExpressionFunction(
        'Is_Valid',
        'Boolean',
        [('Buffer', 'Bytes')],
        LogCall(f'Valid_{field_name} (Buffer)'),
        [Precondition(COMMON_PRECONDITION)])


def create_contains_function(name: str, pdu: str, field: str, sdu: str,
                             condition: LogExpr) -> Subprogram:

    return Function(f'Contains_{name}',
                    'Boolean',
                    [('Buffer', 'Bytes')],
                    [IfStatement(
                        [(condition,
                          [PragmaStatement(
                              'Assume',
                              [(f'{sdu}.Is_Contained (Buffer ({pdu}.{field}_First (Buffer)'
                                f' .. {pdu}.{field}_Last (Buffer)))')]),
                           ReturnStatement(TRUE)])]),
                     ReturnStatement(FALSE)],
                    [Precondition(And(LogCall(f'{pdu}.Is_Contained (Buffer)'),
                                      LogCall(f'{pdu}.Is_Valid (Buffer)'))),
                     Postcondition(
                         IfExpression(
                             [(LogCall(f'Contains_{name}\'Result'),
                               LogCall((f'{sdu}.Is_Contained (Buffer ({pdu}.{field}_First (Buffer)'
                                        f' .. {pdu}.{field}_Last (Buffer)))')))]))])
