import itertools
from abc import ABC, abstractmethod, abstractproperty
from collections import OrderedDict
from typing import Callable, Dict, List, Tuple

from model import (Add, And, Array, Attribute, Div, Enumeration, Equal, Expr, Field, First,
                   GreaterEqual, Last, Length, LengthValue, Less, LessEqual, LogExpr, MathExpr,
                   ModularInteger, Mul, Number, Or, PDU, Pow, RangeInteger, Refinement, Size, Sub,
                   TRUE, Value, Variant)


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
    def __init__(self, names: List[str]) -> None:
        self.names = names

    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented


class WithClause(ContextItem):
    def __str__(self) -> str:
        names = ', '.join(self.names)
        return f'with {names};'


class UsePackageClause(ContextItem):
    def __str__(self) -> str:
        names = ', '.join(self.names)
        return f'use {names};'


class UseTypeClause(ContextItem):
    def __str__(self) -> str:
        names = ', '.join(self.names)
        return f'use type {names};'


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

        if definition and not types and not subprograms:
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
                f'   function Convert_To_{self.name} is new Types.Convert_To_Mod ({self.name});')


class RangeType(TypeDeclaration):
    def __init__(self, name: str, first: MathExpr, last: MathExpr, size: MathExpr) -> None:
        super().__init__(name)
        self.first = first
        self.last = last
        self.size = size

    def __str__(self) -> str:
        return (f'   type {self.name} is range {self.first} .. {self.last}'
                f' with Size => {self.size};\n'
                f'   function Convert_To_{self.name} is new Types.Convert_To_Int ({self.name});')


class EnumerationType(TypeDeclaration):
    def __init__(self, name: str, literals: Dict[str, Number], size: Number) -> None:
        super().__init__(name)
        self.literals = OrderedDict(sorted(literals.items(), key=lambda t: t[1]))
        self.size = size

    def __str__(self) -> str:
        literal_specification = ', '.join(self.literals.keys())
        literal_representation = ', '.join([f'{k} => {v}' for k, v in self.literals.items()])
        return (f'   type {self.name} is ({literal_specification}) with Size => {self.size};\n'
                f'   for {self.name} use ({literal_representation});')


class RangeSubtype(TypeDeclaration):
    def __init__(self, name: str, base_name: str, first: MathExpr, last: MathExpr) -> None:
        super().__init__(name)
        self.base_name = base_name
        self.first = first
        self.last = last

    def __str__(self) -> str:
        return f'   subtype {self.name} is {self.base_name} range {self.first} .. {self.last};'


class DerivedType(TypeDeclaration):
    def __init__(self, name: str, type_name: str) -> None:
        super().__init__(name)
        self.type_name = type_name

    def __str__(self) -> str:
        return f'   type {self.name} is new {self.type_name};'


class Aspect(ABC):
    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

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

    @abstractmethod
    def definition(self) -> str:
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


class CaseExpression(Expr):
    def __init__(self, control_expression: Expr,
                 case_statements: List[Tuple[Expr, Expr]]) -> None:
        self.control_expression = control_expression
        self.case_statements = case_statements

    def __str__(self) -> str:
        grouped_cases = [(' | '.join([str(c) for c, _ in choices]), expr)
                         for expr, choices in itertools.groupby(self.case_statements,
                                                                lambda x: x[1])]
        cases = ', '.join([f'when {choice} => {expr}'
                           for choice, expr in grouped_cases])
        return f'case {self.control_expression} is {cases}'


class Statement(ABC):
    pass


class Assignment(Statement):
    def __init__(self, name: str, expression: MathExpr) -> None:
        self.name = name
        self.expression = expression

    def __str__(self) -> str:
        return f'      {self.name} := {self.expression};'


class CallStatement(Statement):
    def __init__(self, name: str, arguments: List[str]) -> None:
        self.name = name
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ', '.join(self.arguments)
        return f'      {self.name} ({arguments});'


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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        raise NotImplementedError

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
        return (f'{negative}'
                f'Convert_To_{self.type_name} ({self.array_name} ({self.first} .. {self.last}), '
                f'{self.offset}'
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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        raise NotImplementedError

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

    def converted(self, replace_function: Callable[[MathExpr], MathExpr]) -> MathExpr:
        raise NotImplementedError

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

COMMON_CONTEXT = [WithClause(['Types']), UseTypeClause(['Types.Index_Type', 'Types.Length_Type'])]


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
        seen_types: List[str] = []
        unreachable_functions: Dict[str, List[Subprogram]] = {}

        for pdu in pdus:
            if pdu.package in self.__units:
                top_level_package = self.__units[pdu.package].package
            else:
                top_level_package = Package(pdu.package, [], [])
                self.__units[pdu.package] = Unit(COMMON_CONTEXT,
                                                 top_level_package)

            context: List[ContextItem] = []
            package = Package(pdu.full_name, [], [])
            self.__units[pdu.full_name] = Unit(context, package)

            if pdu.package not in unreachable_functions:
                unreachable_functions[pdu.package] = []

            package.subprograms.extend(
                create_contain_functions())

            facts = {
                First('Message'): First('Buffer'),
                Last('Message'): Mul(Last('Buffer'), Number(8)),
                Length('Message'): Sub(Mul(Last('Buffer'), Number(8)), First('Buffer'))
            }

            fields = pdu.fields(facts, First('Buffer'))
            self.__pdu_fields[pdu.full_name] = list(fields.keys())

            for field in fields.values():
                if field.name == 'FINAL':
                    continue

                if f'{pdu.package}.{field.type.name}' not in seen_types:
                    seen_types.append(f'{pdu.package}.{field.type.name}')
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

                    elif isinstance(field.type, Enumeration):
                        top_level_package.types += enumeration_types(field.type)
                        top_level_package.subprograms += enumeration_functions(field.type)

                    elif isinstance(field.type, Array):
                        if 'Payload' not in field.type.name:
                            array_context: List[ContextItem] = \
                                [WithClause([f'{pdu.package}.{field.type.element_type}'])]
                            array_package = Package(f'{pdu.package}.{field.type.name}', [], [])
                            self.__units[array_package.name] = Unit(array_context, array_package)

                            array_package.types += array_types()
                            array_package.subprograms += create_contain_functions()
                            array_package.subprograms += array_functions(field.type, pdu.package)

                    else:
                        raise NotImplementedError(f'unsupported type "{type(field.type).__name__}"')

                    type_name = field.type.name if not isinstance(field.type, Array) \
                        else 'Types.Index_Type'
                    function = create_unreachable_function(type_name)
                    if function not in unreachable_functions[pdu.package]:
                        unreachable_functions[pdu.package].append(function)

                if isinstance(field.type, Array) and 'Payload' not in field.type.name:
                    with_clause = WithClause([f'{pdu.package}.{field.type.name}'])
                    if with_clause not in context:
                        context.append(with_clause)

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
                        field,
                        top_level_package.name))

            package.subprograms.append(
                create_packet_validation_function(
                    list(fields['FINAL'].variants.values())))

            package.subprograms.append(
                create_message_length_function(
                    list(fields['FINAL'].variants.values())))

            function = create_unreachable_function('Types.Length_Type')
            if function not in unreachable_functions[pdu.package]:
                unreachable_functions[pdu.package].append(function)

        for pdu_package, functions in unreachable_functions.items():
            top_level_package = self.__units[pdu_package].package
            top_level_package.subprograms.insert(
                0,
                Pragma('Warnings',
                       ['On', '"precondition is statically false"']))
            top_level_package.subprograms[0:0] = functions
            top_level_package.subprograms.insert(
                0,
                Pragma('Warnings',
                       ['Off', '"precondition is statically false"']))

    def __process_refinements(self, refinements: List[Refinement]) -> None:
        for refinement in refinements:
            if refinement.package not in self.__units:
                context = COMMON_CONTEXT
                package = Package(refinement.package, [], [])
                self.__units[refinement.package] = Unit(context, package)

            contains_package = f'{refinement.package}.Contains'
            if contains_package in self.__units:
                context = self.__units[contains_package].context
                package = self.__units[contains_package].package
            else:
                context = []
                package = Package(contains_package, [], [])
                self.__units[contains_package] = Unit(context, package)

            pdu_package = refinement.pdu.rsplit('.', 1)[0]
            if pdu_package != refinement.package:
                pdu_top_level_context = [WithClause([pdu_package]), UsePackageClause([pdu_package])]
                if pdu_top_level_context not in context:
                    context.extend(pdu_top_level_context)
            pdu_context = WithClause([refinement.pdu])
            if pdu_context not in context:
                context.append(pdu_context)
            sdu_context = WithClause([refinement.sdu])
            if sdu_context not in context:
                context.append(sdu_context)

            package.subprograms.append(
                create_contains_function(
                    refinement.unqualified_name,
                    refinement.pdu,
                    refinement.field,
                    refinement.sdu,
                    refinement.condition.simplified(
                        {Value(field): MathCall(f'{refinement.pdu}.Get_{field} (Buffer)')
                         for field in self.__pdu_fields[refinement.pdu]})))


def enumeration_types(enum: Enumeration) -> List[TypeDeclaration]:
    return [ModularType(enum.base_name,
                        Pow(Number(2), enum.size)),
            EnumerationType(enum.name,
                            enum.literals,
                            enum.size)]


def enumeration_functions(enum: Enumeration) -> List[Subprogram]:
    common_precondition = And(Less(Value('Offset'),
                                   Number(8)),
                              Equal(Length('Buffer'),
                                    Add(Div(Add(Size(enum.base_name),
                                                Value('Offset'),
                                                Number(-1)),
                                            Number(8)),
                                        Number(1))))

    control_expression = LogCall(f'Convert_To_{enum.base_name} (Buffer, Offset)')

    validation_cases: List[Tuple[Expr, Expr]] = []
    validation_cases += [(value, Value('True')) for value in enum.literals.values()]
    validation_cases += [(Value('others'), Value('False'))]

    conversion_cases: List[Tuple[Expr, Expr]] = []
    conversion_cases += [(value, Value(key)) for key, value in enum.literals.items()]
    conversion_cases += [(Value('others'), LogCall(f'Unreachable_{enum.name}'))]

    return [ExpressionFunction(f'Valid_{enum.name}',
                               'Boolean',
                               [('Buffer', 'Types.Bytes'),
                                ('Offset', 'Natural')],
                               CaseExpression(control_expression,
                                              validation_cases),
                               [Precondition(common_precondition)]),
            ExpressionFunction(f'Convert_To_{enum.name}',
                               enum.name,
                               [('Buffer', 'Types.Bytes'),
                                ('Offset', 'Natural')],
                               CaseExpression(control_expression,
                                              conversion_cases),
                               [Precondition(And(common_precondition,
                                                 LogCall(f'Valid_{enum.name} (Buffer, Offset)')))])]


def array_types() -> List[TypeDeclaration]:
    return [DerivedType('Offset_Type',
                        'Types.Index_Type')]


def array_functions(array: Array, package: str) -> List[Subprogram]:
    common_precondition = LogCall(f'Is_Contained (Buffer)')

    return [Function('Valid_First',
                     'Boolean',
                     [('Buffer', 'Types.Bytes')],
                     [ReturnStatement(
                         LogCall('Valid_Next (Buffer, Offset_Type (Buffer\'First))'))],
                     [Precondition(common_precondition)]),
            Procedure('First',
                      [('Buffer', 'Types.Bytes'),
                       ('Offset', 'out Offset_Type'),
                       ('First', 'out Types.Index_Type'),
                       ('Last', 'out Types.Index_Type')],
                      [Assignment('Offset', Value('Offset_Type (Buffer\'First)')),
                       CallStatement('Next', ['Buffer', 'Offset', 'First', 'Last'])],
                      [Precondition(And(common_precondition,
                                        LogCall('Valid_First (Buffer)'))),
                       Postcondition(And(And(GreaterEqual(Value('First'),
                                                          First('Buffer')),
                                             LessEqual(Value('Last'),
                                                       Last('Buffer'))),
                                         LogCall(f'{package}.{array.element_type}.Is_Contained '
                                                 '(Buffer (First .. Last))')))]),
            Function('Valid_Next',
                     'Boolean',
                     [('Buffer', 'Types.Bytes'),
                      ('Offset', 'Offset_Type')],
                     [PragmaStatement('Assume',
                                      [(f'{package}.{array.element_type}.Is_Contained '
                                        '(Buffer (Types.Index_Type (Offset) .. Buffer\'Last))')]),
                      ReturnStatement(
                          LogCall(f'{package}.{array.element_type}.Is_Valid '
                                  '(Buffer (Types.Index_Type (Offset) .. Buffer\'Last))'))],
                     [Precondition(common_precondition)]),
            Procedure('Next',
                      [('Buffer', 'Types.Bytes'),
                       ('Offset', 'in out Offset_Type'),
                       ('First', 'out Types.Index_Type'),
                       ('Last', 'out Types.Index_Type')],
                      [Assignment('First', Value('Types.Index_Type (Offset)')),
                       Assignment('Last', Add(Value('First'),
                                              Cast('Types.Length_Type',
                                                   MathCall(f'{package}.{array.element_type}.'
                                                            'Message_Length (Buffer (First '
                                                            '.. Buffer\'Last))')),
                                              Number(-1))),
                       Assignment('Offset', Value('Offset_Type (Last + 1)')),
                       PragmaStatement('Assume',
                                       [(f'{package}.{array.element_type}.Is_Contained '
                                         '(Buffer (First .. Last))')])],
                      [Precondition(And(common_precondition,
                                        LogCall('Valid_Next (Buffer, Offset)'))),
                       Postcondition(And(And(GreaterEqual(Value('First'),
                                                          First('Buffer')),
                                             LessEqual(Value('Last'),
                                                       Last('Buffer'))),
                                         LogCall(f'{package}.{array.element_type}.Is_Contained '
                                                 '(Buffer (First .. Last))')))])]


COMMON_PRECONDITION = LogCall('Is_Contained (Buffer)')


def create_contain_functions() -> List[Subprogram]:
    return [ExpressionFunction('Is_Contained',
                               'Boolean',
                               [('Buffer', 'Types.Bytes')],
                               aspects=[Ghost(), Import()]),
            Procedure('Label',
                      [('Buffer', 'Types.Bytes')],
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
               LessEqual(First('Buffer'), Div(Last('Types.Index_Type'), Number(2))))


def create_field_location(
        field_name: str,
        variant_id: str,
        variant: Variant) -> Tuple[MathExpr, MathExpr, int]:

    value_to_call = create_value_to_call(
        [(field_name, variant_id)] + variant.previous)
    first_byte = variant.facts[First(field_name)].to_bytes().simplified(value_to_call)
    last_byte = variant.facts[Last(field_name)].to_bytes().simplified(value_to_call)
    offset = calculate_offset(variant.facts[Last(field_name)])
    return (first_byte, last_byte, offset)


def create_value_to_call(previous: List[Tuple[str, str]]) -> Dict[Attribute, MathExpr]:
    result: Dict[Attribute, MathExpr] = {}
    for field_name, vid in previous:
        get_call = MathCall(f'Get_{field_name}_{vid} (Buffer)')
        result[Value(field_name)] = get_call
        result[LengthValue(field_name)] = Cast('Types.Length_Type', get_call)
    return result


def create_variant_validation_function(
        field: Field,
        variant_id: str,
        variant: Variant) -> Subprogram:

    type_constraints: LogExpr = TRUE

    if field.type.constraints != TRUE or isinstance(field.type, Enumeration):
        first_byte, last_byte, offset = create_field_location(field.name, variant_id, variant)

        if field.type.constraints != TRUE:
            convert = Convert(
                field.type.base_name,
                'Buffer',
                first_byte,
                last_byte,
                offset)
            type_constraints = field.type.constraints.simplified({Value(field.type.name): convert})

        if isinstance(field.type, Enumeration):
            type_constraints = And(type_constraints,
                                   LogCall((f'Valid_{field.type.name} (Buffer ({first_byte}'
                                            f' .. {last_byte}), {offset})')))

    value_to_call = create_value_to_call([(field.name, variant_id)] + variant.previous)

    return ExpressionFunction(
        f'Valid_{field.name}_{variant_id}',
        'Boolean',
        [('Buffer', 'Types.Bytes')],
        And(LogCall(f'Valid_{variant.previous[-1][0]}_{variant.previous[-1][1]} (Buffer)')
            if variant.previous else TRUE,
            And(
                And(
                    buffer_constraints(
                        variant.facts[Last(field.name)].to_bytes()).simplified(value_to_call),
                    variant.condition.simplified(variant.facts).simplified(value_to_call)),
                type_constraints)
            ).simplified(),
        [Precondition(COMMON_PRECONDITION)])


def create_variant_accessor_functions(
        field: Field,
        variant_id: str,
        variant: Variant) -> List[Subprogram]:

    first_byte, last_byte, offset = create_field_location(field.name, variant_id, variant)

    name = f'Get_{field.name}_{variant_id}'
    precondition = Precondition(
        And(COMMON_PRECONDITION,
            LogCall(f'Valid_{field.name}_{variant_id} (Buffer)')))

    functions: List[Subprogram] = []
    if isinstance(field.type, Array):
        functions.append(
            ExpressionFunction(
                f'{name}_First',
                'Types.Index_Type',
                [('Buffer', 'Types.Bytes')],
                first_byte,
                [precondition]))
        functions.append(
            ExpressionFunction(
                f'{name}_Last',
                'Types.Index_Type',
                [('Buffer', 'Types.Bytes')],
                last_byte,
                [precondition]))
    else:
        functions.append(
            ExpressionFunction(
                name,
                field.type.name,
                [('Buffer', 'Types.Bytes')],
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
        expression.simplified(
            variant.facts
        ).simplified(
            create_value_to_call([(field.name, variant_id)] + variant.previous)))


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
        [('Buffer', 'Types.Bytes')],
        expr,
        [Precondition(COMMON_PRECONDITION)])


def create_unreachable_function(type_name: str) -> Subprogram:
    return ExpressionFunction(
        f'Unreachable_{type_name}'.replace('.', '_'),
        type_name,
        [],
        First(type_name),
        [Precondition(FALSE)])


def create_field_accessor_functions(field: Field, package_name: str) -> List[Subprogram]:
    precondition = Precondition(And(COMMON_PRECONDITION,
                                    LogCall(f'Valid_{field.name} (Buffer)')))

    functions: List[Subprogram] = []
    if isinstance(field.type, Array):
        for attribute in ['First', 'Last']:
            functions.append(
                ExpressionFunction(
                    f'Get_{field.name}_{attribute}',
                    'Types.Index_Type',
                    [('Buffer', 'Types.Bytes')],
                    IfExpression([(LogCall(f'Valid_{field.name}_{variant_id} (Buffer)'),
                                   LogCall(f'Get_{field.name}_{variant_id}_{attribute} (Buffer)'))
                                  for variant_id in field.variants],
                                 'Unreachable_Types_Index_Type'),
                    [precondition]))

        body: List[Statement] = [Assignment('First', MathCall(f'Get_{field.name}_First (Buffer)')),
                                 Assignment('Last', MathCall(f'Get_{field.name}_Last (Buffer)'))]
        postcondition = Postcondition(And(Equal(Value('First'),
                                                MathCall(f'Get_{field.name}_First (Buffer)')),
                                          Equal(Value('Last'),
                                                MathCall(f'Get_{field.name}_Last (Buffer)'))))
        if 'Payload' not in field.type.name:
            predicate = f'{package_name}.{field.type.name}.Is_Contained (Buffer (First .. Last))'
            body += [PragmaStatement('Assume',
                                     [predicate])]
            postcondition.expr = And(postcondition.expr, LogCall(predicate))

        functions.append(
            Procedure(
                f'Get_{field.name}',
                [('Buffer', 'Types.Bytes'),
                 ('First', 'out Types.Index_Type'),
                 ('Last', 'out Types.Index_Type')],
                body,
                [precondition,
                 postcondition]))

    else:
        functions.append(
            ExpressionFunction(
                f'Get_{field.name}',
                field.type.name,
                [('Buffer', 'Types.Bytes')],
                IfExpression([(LogCall(f'Valid_{field.name}_{variant_id} (Buffer)'),
                               MathCall(f'Get_{field.name}_{variant_id} (Buffer)'))
                              for variant_id in field.variants],
                             f'Unreachable_{field.type.name}'),
                [precondition]))

    return functions


def create_packet_validation_function(variants: List[Variant]) -> Subprogram:
    expr: LogExpr = FALSE

    for variant in variants:
        condition = create_variant_condition(variant)
        expr = condition if expr == FALSE else Or(expr, condition)

    return ExpressionFunction(
        'Is_Valid',
        'Boolean',
        [('Buffer', 'Types.Bytes')],
        expr,
        [Precondition(COMMON_PRECONDITION)])


def create_message_length_function(variants: List[Variant]) -> Subprogram:
    condition_expressions: List[Tuple[LogExpr, Expr]] = []

    for variant in variants:
        condition = create_variant_condition(variant)
        length = Add(
            Last(variant.previous[-1][0]),
            -First(variant.previous[0][0]),
            Number(1)
        ).simplified(
            variant.facts
        ).simplified(
            create_value_to_call(variant.previous)
        ).to_bytes().simplified()
        condition_expressions.append((condition, length))

    return ExpressionFunction(
        'Message_Length',
        'Types.Length_Type',
        [('Buffer', 'Types.Bytes')],
        IfExpression(condition_expressions, 'Unreachable_Types_Length_Type'),
        [Precondition(And(COMMON_PRECONDITION, LogCall('Is_Valid (Buffer)')))])


def create_variant_condition(variant: Variant) -> LogExpr:
    field_name, variant_id = variant.previous[-1]
    return And(
        LogCall(f'Valid_{field_name}_{variant_id} (Buffer)'),
        variant.condition
    ).simplified(variant.facts).simplified(create_value_to_call(variant.previous))


def create_contains_function(name: str, pdu: str, field: str, sdu: str,
                             condition: LogExpr) -> Subprogram:

    return Function(name,
                    'Boolean',
                    [('Buffer', 'Types.Bytes')],
                    [IfStatement(
                        [(condition,
                          [PragmaStatement(
                              'Assume',
                              [(f'{sdu}.Is_Contained (Buffer ({pdu}.Get_{field}_First (Buffer)'
                                f' .. {pdu}.Get_{field}_Last (Buffer)))')]),
                           ReturnStatement(TRUE)])]),
                     ReturnStatement(FALSE)],
                    [Precondition(And(LogCall(f'{pdu}.Is_Contained (Buffer)'),
                                      LogCall(f'{pdu}.Is_Valid (Buffer)'))),
                     Postcondition(
                         IfExpression(
                             [(LogCall(f'{name}\'Result'),
                               LogCall((f'{sdu}.Is_Contained (Buffer ('
                                        f'{pdu}.Get_{field}_First (Buffer)'
                                        f' .. {pdu}.Get_{field}_Last (Buffer)))')))]))])
