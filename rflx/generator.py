from pathlib import Path
from typing import Dict, Iterator, List, Tuple

from rflx.ada import (FALSE, Aggregate, Aspect, Assignment, CallStatement, CaseExpression, Cast,
                      ComponentItem, ContextItem, Convert, Declaration, Discriminant,
                      EnumerationType, ExpressionFunction, Function, Ghost, IfExpression,
                      IfStatement, Import, LogCall, MathCall, ModularType, Package, Postcondition,
                      Pragma, PragmaStatement, Precondition, Procedure, RangeSubtype, RangeType,
                      RecordType, ReturnStatement, Statement, Subprogram, TypeDeclaration, Unit,
                      UsePackageClause, UseTypeClause, VariantItem, VariantRecordType, WithClause)
from rflx.expression import (TRUE, Add, And, Attribute, Div, Equal, Expr, GreaterEqual, Last,
                             Length, LengthValue, Less, LessEqual, LogExpr, MathExpr, Mul, Number,
                             Or, Pow, Size, Sub, Value)
from rflx.model import (PDU, Array, Enumeration, Field, First, ModularInteger, Null, RangeInteger,
                        Reference, Refinement, Type, Variant)

COMMON_PRECONDITION = LogCall('Is_Contained (Buffer)')


class Generator:
    def __init__(self, prefix: str = '') -> None:
        self.__prefix = prefix
        self.__units: Dict[str, Unit] = {}
        self.__pdu_fields: Dict[str, List[str]] = {}

    def generate_dissector(self, pdus: List[PDU], refinements: List[Refinement]) -> None:
        self.__process_pdus(pdus)
        self.__process_refinements(refinements)

    def units(self) -> List[Unit]:
        return list(self.__units.values())

    def write_units(self, directory: Path) -> List[Path]:
        written_files = []

        for unit in self.units():
            filename = directory.joinpath(unit.package.name.lower().replace('.', '-') + '.ads')
            written_files.append(filename)
            with open(filename, 'w') as f:
                f.write(unit.specification())

            if unit.definition().strip():
                filename = filename.with_suffix('.adb')
                written_files.append(filename)
                with open(filename, 'w') as f:
                    f.write(unit.definition())

        return written_files

    def __process_pdus(self, pdus: List[PDU]) -> None:
        seen_types: List[str] = []

        for pdu in pdus:
            if pdu.package not in self.__units:
                self.__units[pdu.package] = Unit(self.__common_context(),
                                                 Package(f'{self.__prefix}{pdu.package}', [], []))

            context: List[ContextItem] = []
            package = Package(f'{self.__prefix}{pdu.full_name}', [], [])
            self.__units[pdu.full_name] = Unit(context, package)

            package.subprograms.extend(
                self.__contain_functions())

            facts: Dict[Attribute, MathExpr] = {
                First('Message'): Mul(First('Buffer'), Number(8)),
                Last('Message'): Mul(Last('Buffer'), Number(8)),
                Length('Message'): Sub(Add(Mul(Last('Buffer'), Number(8)), Number(8)),
                                       Mul(First('Buffer'), Number(8)))
            }

            fields = pdu.fields(facts, First('Buffer'))
            self.__pdu_fields[pdu.full_name] = list(fields.keys())

            for field in fields.values():
                if field.name == 'FINAL':
                    continue

                if f'{pdu.package}.{field.type.name}' not in seen_types:
                    seen_types.append(f'{pdu.package}.{field.type.name}')

                    self.__create_type(field.type, pdu.package)

                if isinstance(field.type, Array) and 'Payload' not in field.type.name:
                    with_clause = WithClause([f'{self.__prefix}{pdu.package}.{field.type.name}'])
                    if with_clause not in context:
                        context.append(with_clause)

                for variant_id, variant in field.variants.items():
                    package.subprograms.append(
                        self.__variant_validation_function(
                            field,
                            variant_id,
                            variant))

                    package.subprograms.extend(
                        self.__variant_accessor_functions(
                            field,
                            variant_id,
                            variant))

                package.subprograms.append(
                    self.__field_validation_function(
                        field))

                package.subprograms.extend(
                    self.__field_accessor_functions(
                        field,
                        pdu.package))

            package.subprograms.append(
                self.__message_validation_function(
                    list(fields['FINAL'].variants.values())))

            package.subprograms.append(
                self.__message_length_function(
                    list(fields['FINAL'].variants.values())))

        self.__create_unreachable_functions(pdus)

    def __create_type(self, field_type: Type, pdu_package: str) -> None:
        top_level_package = self.__units[pdu_package].package

        if isinstance(field_type, ModularInteger):
            top_level_package.types += modular_types(field_type)
        elif isinstance(field_type, RangeInteger):
            top_level_package.types += range_types(field_type)
            top_level_package.subprograms += self.__range_functions(field_type)
        elif isinstance(field_type, Enumeration):
            top_level_package.types += enumeration_types(field_type)
            top_level_package.subprograms += self.__enumeration_functions(field_type)
        elif isinstance(field_type, Array):
            if 'Payload' not in field_type.name:
                if not isinstance(field_type.element_type, Reference):
                    self.__create_type(field_type.element_type, pdu_package)

                array_context: List[ContextItem] = []
                if isinstance(field_type.element_type, Reference):
                    array_context = [WithClause([f'{self.__prefix}{pdu_package}.'
                                                 f'{field_type.element_type.name}'])]

                array_package = Package(f'{self.__prefix}{pdu_package}.{field_type.name}',
                                        self.__array_types(),
                                        self.__contain_functions()
                                        + self.__array_functions(field_type, pdu_package))

                self.__units[array_package.name] = Unit(array_context, array_package)
        else:
            raise NotImplementedError(f'unsupported type "{type(field_type).__name__}"')

    def __create_unreachable_functions(self, pdus: List[PDU]) -> None:
        unreachable_functions: Dict[str, List[Subprogram]] = {}

        for pdu in pdus:
            if pdu.package not in unreachable_functions:
                unreachable_functions[pdu.package] = []

            for field in pdu.fields().values():
                if not isinstance(field.type, Null):
                    functions = self.__type_dependent_unreachable_function(field.type)
                    for function in functions:
                        if function not in unreachable_functions[pdu.package]:
                            unreachable_functions[pdu.package].append(function)

            function = unreachable_function(self.__types_length)
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
                context = self.__common_context()
                package = Package(f'{self.__prefix}{refinement.package}', [], [])
                self.__units[refinement.package] = Unit(context, package)

            contains_package = f'{refinement.package}.Contains'
            if contains_package in self.__units:
                context = self.__units[contains_package].context
                package = self.__units[contains_package].package
            else:
                context = []
                package = Package(f'{self.__prefix}{contains_package}', [], [])
                self.__units[contains_package] = Unit(context, package)

            pdu_package = self.__prefix + refinement.pdu.rsplit('.', 1)[0]
            if pdu_package != refinement.package:
                pdu_top_level_context = [WithClause([pdu_package]), UsePackageClause([pdu_package])]
                for c in pdu_top_level_context:
                    if c not in context:
                        context.append(c)
            pdu_context = WithClause([f'{self.__prefix}{refinement.pdu}'])
            if pdu_context not in context:
                context.append(pdu_context)
            if refinement.sdu != 'null':
                sdu_context = WithClause([f'{self.__prefix}{refinement.sdu}'])
                if sdu_context not in context:
                    context.append(sdu_context)

            package.subprograms.append(
                self.__contains_function(
                    refinement.unqualified_name,
                    refinement.pdu,
                    refinement.field,
                    refinement.sdu,
                    refinement.condition.simplified(
                        {Value(field): MathCall(f'{refinement.pdu}.Get_{field} (Buffer)')
                         for field in self.__pdu_fields[refinement.pdu]})))

    def __common_context(self) -> List[ContextItem]:
        return [WithClause([self.__types]),
                UseTypeClause([self.__types_index,
                               self.__types_length])]

    def __range_functions(self, integer: RangeInteger) -> List[Subprogram]:
        if integer.constraints == TRUE:
            return []

        integer_value = Convert(integer.base_name,
                                'Buffer',
                                First('Buffer'),
                                Last('Buffer'),
                                Value('Offset'))

        return [ExpressionFunction(f'Valid_{integer.name}',
                                   'Boolean',
                                   [('Buffer', self.__types_bytes),
                                    ('Offset', 'Natural')],
                                   integer.constraints.simplified({Value(integer.name):
                                                                   integer_value}),
                                   [Precondition(And(Less(Value('Offset'),
                                                          Number(8)),
                                                     Equal(Length('Buffer'),
                                                           Add(Div(Add(Size(integer.base_name),
                                                                       Value('Offset'),
                                                                       Number(-1)),
                                                                   Number(8)),
                                                               Number(1)))))])]

    def __enumeration_functions(self, enum: Enumeration) -> List[Subprogram]:
        common_precondition = And(Less(Value('Offset'),
                                       Number(8)),
                                  Equal(Length('Buffer'),
                                        Add(Div(Add(Size(enum.base_name),
                                                    Value('Offset'),
                                                    Number(-1)),
                                                Number(8)),
                                            Number(1))))

        enum_value = Convert(enum.base_name,
                             'Buffer',
                             First('Buffer'),
                             Last('Buffer'),
                             Value('Offset'))

        validation_expression: Expr
        if enum.always_valid:
            validation_expression = Value('True')
        else:
            validation_cases: List[Tuple[Expr, Expr]] = []
            validation_cases.extend((value, Value('True')) for value in enum.literals.values())
            validation_cases.append((Value('others'), Value('False')))

            validation_expression = CaseExpression(enum_value,
                                                   validation_cases)
        validation_function = ExpressionFunction(f'Valid_{enum.name}',
                                                 'Boolean',
                                                 [('Buffer', self.__types_bytes),
                                                  ('Offset', 'Natural')],
                                                 validation_expression,
                                                 [Precondition(common_precondition)])

        function_name = f'Convert_To_{enum.name}'
        parameters = [('Buffer', self.__types_bytes),
                      ('Offset', 'Natural')]
        precondition = Precondition(And(common_precondition,
                                        LogCall(f'Valid_{enum.name} (Buffer, Offset)')))
        conversion_cases: List[Tuple[Expr, Expr]] = []
        conversion_function: Subprogram

        if enum.always_valid:
            conversion_cases.extend((value, Aggregate(Value('True'), Value(key)))
                                    for key, value in enum.literals.items())
            conversion_cases.append((Value('others'),
                                     Aggregate(Value('False'), Value('Raw'))))

            conversion_function = Function(function_name,
                                           enum.name,
                                           parameters,
                                           [Declaration('Raw', enum.base_name, enum_value)],
                                           [ReturnStatement(CaseExpression(Value('Raw'),
                                                                           conversion_cases))],
                                           [precondition])
        else:
            conversion_cases.extend((value, Value(key)) for key, value in enum.literals.items())
            conversion_cases.append((Value('others'),
                                     LogCall(unreachable_function_name(enum.name))))

            conversion_function = ExpressionFunction(function_name,
                                                     enum.name,
                                                     parameters,
                                                     CaseExpression(enum_value,
                                                                    conversion_cases),
                                                     [precondition])

        enum_to_base_function = ExpressionFunction(
            f'Convert_To_{enum.base_name}',
            enum.base_name,
            [('Enum', enum.enum_name if enum.always_valid else enum.name)],
            CaseExpression(
                Value('Enum'),
                [(Value(key), value) for key, value in enum.literals.items()]))

        return [validation_function, conversion_function, enum_to_base_function]

    def __array_types(self) -> List[TypeDeclaration]:
        return [RecordType('Cursor_Type',
                           [ComponentItem('First', self.__types_index),
                            ComponentItem('Last', self.__types_index)])]

    def __array_functions(self, array: Array, package: str) -> List[Subprogram]:
        if isinstance(array.element_type, Reference):
            return self.__message_array_functions(array, package)
        return self.__scalar_array_functions(array)

    def __message_array_functions(self, array: Array, package: str) -> List[Subprogram]:
        element_name = array.element_type.name
        contained_condition = LogCall(f'Is_Contained (Buffer)')

        first_index_condition = And(GreaterEqual(Value('First\'Result.First'),
                                                 First('Buffer')),
                                    LessEqual(Value('First\'Result.Last'),
                                              Last('Buffer')))
        first_element_condition = LogCall(f'{package}.{element_name}.Is_Contained '
                                          '(Buffer (First\'Result.First .. First\'Result.Last))')

        cursor_conditions = And(GreaterEqual(Value('Cursor.First'), First('Buffer')),
                                LessEqual(Value('Cursor.Last'), Last('Buffer')))
        cursor_slice = 'Buffer (Cursor.First .. Cursor.Last)'

        element_contained_condition = LogCall(f'{package}.{element_name}.Is_Contained '
                                              f'({cursor_slice})')
        element_valid_condition = LogCall(f'{package}.{element_name}.Is_Valid '
                                          f'({cursor_slice})')

        return [Function('First',
                         'Cursor_Type',
                         [('Buffer', self.__types_bytes)],
                         [],
                         [CallStatement(f'{package}.{element_name}.Label',
                                        ['Buffer (Buffer\'First .. Buffer\'Last)']),
                          ReturnStatement(Aggregate(First('Buffer'),
                                                    Last('Buffer')))],
                         [Precondition(contained_condition),
                          Postcondition(And(first_index_condition,
                                            first_element_condition))]),
                Procedure('Next',
                          [('Buffer', self.__types_bytes),
                           ('Cursor', 'in out Cursor_Type')],
                          [],
                          [Assignment('Cursor',
                                      Aggregate(Add(Value('Cursor.First'),
                                                    Cast('Types.Length_Type',
                                                         MathCall(f'{package}.{element_name}.'
                                                                  'Message_Length '
                                                                  f'({cursor_slice})'))),
                                                Last('Buffer'))),
                           CallStatement(f'{package}.{element_name}.Label', [cursor_slice])],
                          [Precondition(And(cursor_conditions,
                                            And(element_contained_condition,
                                                element_valid_condition))),
                           Postcondition(And(cursor_conditions,
                                             element_contained_condition))]),
                ExpressionFunction('Valid_Element',
                                   'Boolean',
                                   [('Buffer', self.__types_bytes),
                                    ('Cursor', 'Cursor_Type')],
                                   element_valid_condition,
                                   [Precondition(And(cursor_conditions,
                                                     element_contained_condition))])]

    def __scalar_array_functions(self, array: Array) -> List[Subprogram]:
        element_size = array.element_type.size.to_bytes()
        element_size_minus_one = Sub(element_size, Number(1)).simplified()

        contained_condition = LogCall(f'Is_Contained (Buffer)')

        first_index_condition = And(GreaterEqual(Value('First\'Result.First'),
                                                 First('Buffer')),
                                    LessEqual(Value('First\'Result.Last'),
                                              Last('Buffer')))

        cursor_conditions = And(GreaterEqual(Value('Cursor.First'), First('Buffer')),
                                LessEqual(Value('Cursor.Last'), Last('Buffer')))

        type_constraints: LogExpr = TRUE

        if array.element_type.constraints != TRUE or (isinstance(array.element_type, Enumeration)
                                                      and not array.element_type.always_valid):
            type_constraints = LogCall((f'Valid_{array.element_type.name} (Buffer (Cursor.First'
                                        f' .. Cursor.Last), 0)'))

        element_name = array.element_type.name
        if array.element_type.constraints != TRUE:
            element_name = array.element_type.base_name
        element_value = Convert(element_name,
                                'Buffer',
                                Value('Cursor.First'),
                                Value('Cursor.Last'))

        return [Function('First',
                         'Cursor_Type',
                         [('Buffer', self.__types_bytes)],
                         [],
                         [IfStatement([(GreaterEqual(Length('Buffer'), element_size),
                                        [ReturnStatement(
                                            Aggregate(First('Buffer'),
                                                      Add(First('Buffer'),
                                                          element_size_minus_one).simplified()))])],
                                      [ReturnStatement(
                                          Aggregate(Last(self.__types_index),
                                                    First(self.__types_index)))])],
                         [Precondition(contained_condition),
                          Postcondition(first_index_condition)]),
                Procedure('Next',
                          [('Buffer', self.__types_bytes),
                           ('Cursor', 'in out Cursor_Type')],
                          [],
                          [IfStatement([(LessEqual(Value('Cursor.Last'),
                                                   Sub(Last('Buffer'),
                                                       element_size)),
                                         [Assignment('Cursor',
                                                     Aggregate(Add(Value('Cursor.Last'),
                                                                   Number(1)),
                                                               Add(Value('Cursor.Last'),
                                                                   element_size)))])],
                                       [Assignment('Cursor',
                                                   Aggregate(Last(self.__types_index),
                                                             First(self.__types_index)))])],
                          [Precondition(And(contained_condition,
                                            cursor_conditions))]),
                ExpressionFunction('Valid_Element',
                                   'Boolean',
                                   [('Buffer', self.__types_bytes),
                                    ('Cursor', 'Cursor_Type')],
                                   And(cursor_conditions,
                                       And(LessEqual(Value('Cursor.First'),
                                                     Value('Cursor.Last')),
                                           And(Equal(Length('Buffer (Cursor.First .. Cursor.Last)'),
                                                     element_size),
                                               type_constraints))).simplified(),
                                   [Precondition(contained_condition)]),
                ExpressionFunction('Get_Element',
                                   array.element_type.name,
                                   [('Buffer', self.__types_bytes),
                                    ('Cursor', 'Cursor_Type')],
                                   element_value,
                                   [Precondition(And(contained_condition,
                                                     LogCall('Valid_Element (Buffer, Cursor)')))])]

    def __contain_functions(self) -> List[Subprogram]:
        return [ExpressionFunction('Is_Contained',
                                   'Boolean',
                                   [('Buffer', self.__types_bytes)],
                                   aspects=[Ghost(), Import()]),
                Procedure('Label',
                          [('Buffer', self.__types_bytes)],
                          [],
                          [PragmaStatement('Assume', ['Is_Contained (Buffer)'])],
                          aspects=[Ghost(), Postcondition(LogCall('Is_Contained (Buffer)'))])]

    def __variant_validation_function(
            self,
            field: Field,
            variant_id: str,
            variant: Variant) -> Subprogram:

        type_constraints: LogExpr = TRUE

        if field.type.constraints != TRUE or isinstance(field.type, Enumeration):
            first_byte, last_byte, offset = self.__field_location(field.name, variant_id, variant)
            type_constraints = LogCall((f'Valid_{field.type.name} (Buffer ({first_byte}'
                                        f' .. {last_byte}), {offset})'))

        value_to_call = self.__value_to_call_facts([(field.name, variant_id)] + variant.previous)

        return ExpressionFunction(
            f'Valid_{field.name}_{variant_id}',
            'Boolean',
            [('Buffer', self.__types_bytes)],
            And(LogCall(f'Valid_{variant.previous[-1][0]}_{variant.previous[-1][1]} (Buffer)')
                if variant.previous else TRUE,
                And(
                    And(
                        self.__buffer_constraints(
                            variant.facts[Last(field.name)].to_bytes()).simplified(value_to_call),
                        variant.condition.simplified(variant.facts).simplified(value_to_call)),
                    type_constraints)
                ).simplified(),
            [Precondition(COMMON_PRECONDITION)])

    def __variant_accessor_functions(
            self,
            field: Field,
            variant_id: str,
            variant: Variant) -> List[Subprogram]:

        first_byte, last_byte, offset = self.__field_location(field.name, variant_id, variant)

        name = f'Get_{field.name}_{variant_id}'
        precondition = Precondition(
            And(COMMON_PRECONDITION,
                LogCall(f'Valid_{field.name}_{variant_id} (Buffer)')))

        functions: List[Subprogram] = []
        if isinstance(field.type, Array):
            functions.append(
                ExpressionFunction(
                    f'{name}_First',
                    self.__types_index,
                    [('Buffer', self.__types_bytes)],
                    first_byte,
                    [precondition]))
            functions.append(
                ExpressionFunction(
                    f'{name}_Last',
                    self.__types_index,
                    [('Buffer', self.__types_bytes)],
                    last_byte,
                    [precondition]))
        else:
            functions.append(
                ExpressionFunction(
                    name,
                    field.type.name,
                    [('Buffer', self.__types_bytes)],
                    Convert(
                        field.type.name if field.type.constraints == TRUE else field.type.base_name,
                        'Buffer',
                        first_byte,
                        last_byte,
                        offset),
                    [precondition]))
        return functions

    def __field_validation_function(self, field: Field) -> Subprogram:
        variants: List[LogExpr] = list(self.__valid_variants(field))

        expr = variants.pop()
        for e in variants:
            if e is not TRUE:
                expr = Or(expr, e)

        return ExpressionFunction(
            f'Valid_{field.name}',
            'Boolean',
            [('Buffer', self.__types_bytes)],
            expr,
            [Precondition(COMMON_PRECONDITION)])

    def __field_accessor_functions(self, field: Field, package_name: str) -> List[Subprogram]:
        precondition = Precondition(And(COMMON_PRECONDITION,
                                        LogCall(f'Valid_{field.name} (Buffer)')))

        functions: List[Subprogram] = []
        if isinstance(field.type, Array):
            for attribute in ['First', 'Last']:
                functions.append(
                    ExpressionFunction(
                        f'Get_{field.name}_{attribute}',
                        self.__types_index,
                        [('Buffer', self.__types_bytes)],
                        IfExpression([
                            (LogCall(f'Valid_{field.name}_{variant_id} (Buffer)'),
                             LogCall(f'Get_{field.name}_{variant_id}_{attribute} ''(Buffer)'))
                            for variant_id in field.variants],
                            unreachable_function_name(self.__types_index)),
                        [precondition]))

            body: List[Statement] = [
                Assignment('First', MathCall(f'Get_{field.name}_First (Buffer)')),
                Assignment('Last', MathCall(f'Get_{field.name}_Last (Buffer)'))]
            postcondition = Postcondition(And(Equal(Value('First'),
                                                    MathCall(f'Get_{field.name}_First (Buffer)')),
                                              Equal(Value('Last'),
                                                    MathCall(f'Get_{field.name}_Last (Buffer)'))))
            if 'Payload' not in field.type.name:
                predicate = (f'{package_name}.{field.type.name}.Is_Contained '
                             '(Buffer (First .. Last))')
                body.append(PragmaStatement('Assume', [predicate]))
                postcondition.expr = And(postcondition.expr, LogCall(predicate))

            functions.append(
                Procedure(
                    f'Get_{field.name}',
                    [('Buffer', self.__types_bytes),
                     ('First', f'out {self.__types_index}'),
                     ('Last', f'out {self.__types_index}')],
                    [],
                    body,
                    [precondition,
                     postcondition]))

        else:
            functions.append(
                ExpressionFunction(
                    f'Get_{field.name}',
                    field.type.name,
                    [('Buffer', self.__types_bytes)],
                    IfExpression([(LogCall(f'Valid_{field.name}_{variant_id} (Buffer)'),
                                   MathCall(f'Get_{field.name}_{variant_id} (Buffer)'))
                                  for variant_id in field.variants],
                                 unreachable_function_name(field.type.name)),
                    [precondition]))

        return functions

    def __message_validation_function(self, variants: List[Variant]) -> Subprogram:
        expr: LogExpr = FALSE

        for variant in variants:
            condition = self.__variant_condition(variant)
            expr = condition if expr == FALSE else Or(expr, condition)

        return ExpressionFunction(
            'Is_Valid',
            'Boolean',
            [('Buffer', self.__types_bytes)],
            expr,
            [Precondition(COMMON_PRECONDITION)])

    def __message_length_function(self, variants: List[Variant]) -> Subprogram:
        condition_expressions: List[Tuple[LogExpr, Expr]] = []

        for variant in variants:
            condition = self.__variant_condition(variant)
            length = Add(
                Last(variant.previous[-1][0]),
                -First(variant.previous[0][0]),
                Number(1)
            ).simplified(
                variant.facts
            ).simplified(
                self.__value_to_call_facts(variant.previous)
            ).to_bytes().simplified()
            condition_expressions.append((condition, length))

        return ExpressionFunction(
            'Message_Length',
            self.__types_length,
            [('Buffer', self.__types_bytes)],
            IfExpression(condition_expressions, unreachable_function_name(self.__types_length)),
            [Precondition(And(COMMON_PRECONDITION, LogCall('Is_Valid (Buffer)')))])

    def __variant_condition(self, variant: Variant) -> LogExpr:
        field_name, variant_id = variant.previous[-1]
        return And(
            LogCall(f'Valid_{field_name}_{variant_id} (Buffer)'),
            variant.condition
        ).simplified(variant.facts).simplified(self.__value_to_call_facts(variant.previous))

    def __contains_function(
            self,
            name: str,
            pdu: str,
            field: str,
            sdu: str,
            condition: LogExpr) -> Subprogram:
        # pylint: disable=too-many-arguments

        success_statements: List[Statement] = [ReturnStatement(TRUE)]
        aspects: List[Aspect] = [Precondition(And(LogCall(f'{pdu}.Is_Contained (Buffer)'),
                                                  LogCall(f'{pdu}.Is_Valid (Buffer)')))]
        if sdu != 'null':
            success_statements.insert(
                0,
                PragmaStatement(
                    'Assume',
                    [(f'{sdu}.Is_Contained (Buffer ({pdu}.Get_{field}_First (Buffer)'
                      f' .. {pdu}.Get_{field}_Last (Buffer)))')]))
            aspects.append(
                Postcondition(
                    IfExpression(
                        [(LogCall(f'{name}\'Result'),
                          LogCall((f'{sdu}.Is_Contained (Buffer ('
                                   f'{pdu}.Get_{field}_First (Buffer)'
                                   f' .. {pdu}.Get_{field}_Last (Buffer)))')))])))

        return Function(name,
                        'Boolean',
                        [('Buffer', self.__types_bytes)],
                        [],
                        [IfStatement(
                            [(condition,
                              success_statements)]),
                         ReturnStatement(FALSE)],
                        aspects)

    def __type_dependent_unreachable_function(self, field_type: Type) -> List[Subprogram]:
        functions: List[Subprogram] = []

        if isinstance(field_type, Array) and not isinstance(field_type.element_type, Reference):
            functions.extend(self.__type_dependent_unreachable_function(field_type.element_type))

        type_name = field_type.name
        if isinstance(field_type, Array):
            type_name = self.__types_index
        base_name = None
        if isinstance(field_type, Enumeration) and field_type.always_valid:
            base_name = field_type.base_name

        functions.append(unreachable_function(type_name, base_name))

        return functions

    def __field_location(
            self,
            field_name: str,
            variant_id: str,
            variant: Variant) -> Tuple[MathExpr, MathExpr, Number]:

        value_to_call = self.__value_to_call_facts(
            [(field_name, variant_id)] + variant.previous)
        first_byte = variant.facts[First(field_name)].to_bytes().simplified(value_to_call)
        last_byte = variant.facts[Last(field_name)].to_bytes().simplified(value_to_call)
        offset = calculate_offset(variant.facts[Last(field_name)])
        return (first_byte, last_byte, offset)

    def __value_to_call_facts(self, previous: List[Tuple[str, str]]) -> Dict[Attribute, MathExpr]:
        result: Dict[Attribute, MathExpr] = {}
        for field_name, vid in previous:
            get_call = MathCall(f'Get_{field_name}_{vid} (Buffer)')
            result[Value(field_name)] = get_call
            result[LengthValue(field_name)] = Cast(self.__types_length, get_call)
        return result

    def __buffer_constraints(self, last: MathExpr) -> LogExpr:
        last = last.simplified()
        index_constraint = LessEqual(First('Buffer'), Div(Last(self.__types_index), Number(2)))
        if last != Last('Buffer'):
            length_constraint = GreaterEqual(Length('Buffer'),
                                             Add(last, -First('Buffer'), Number(1)))
            return And(length_constraint, index_constraint)
        return index_constraint

    def __valid_variants(self, field: Field) -> Iterator[LogExpr]:
        for variant_id, variant in field.variants.items():
            expression: LogExpr = LogCall(f'Valid_{field.name}_{variant_id} (Buffer)')
            if field.condition is not TRUE:
                expression = And(expression, field.condition)
            yield expression.simplified(
                variant.facts
            ).simplified(
                self.__value_to_call_facts([(field.name, variant_id)] + variant.previous))

    @property
    def __types(self) -> str:
        return f'{self.__prefix}Types'

    @property
    def __types_bytes(self) -> str:
        return f'{self.__prefix}Types.Bytes'

    @property
    def __types_index(self) -> str:
        return f'{self.__prefix}Types.Index_Type'

    @property
    def __types_length(self) -> str:
        return f'{self.__prefix}Types.Length_Type'


def modular_types(integer: ModularInteger) -> List[TypeDeclaration]:
    return [ModularType(integer.name,
                        integer.modulus)]


def range_types(integer: RangeInteger) -> List[TypeDeclaration]:
    if integer.constraints == TRUE:
        return [RangeType(integer.name,
                          integer.first,
                          integer.last,
                          integer.size)]

    return [RangeType(integer.base_name,
                      integer.base_first,
                      integer.base_last,
                      integer.size),
            RangeSubtype(integer.name,
                         integer.base_name,
                         integer.first,
                         integer.last)]


def enumeration_types(enum: Enumeration) -> List[TypeDeclaration]:
    types: List[TypeDeclaration] = []

    types.append(
        ModularType(enum.base_name, Pow(Number(2), enum.size)))
    types.append(
        EnumerationType(enum.enum_name if enum.always_valid else enum.name,
                        enum.literals,
                        enum.size))
    if enum.always_valid:
        types.append(
            VariantRecordType(enum.name,
                              Discriminant('Known', 'Boolean', 'False'),
                              [VariantItem('True', [ComponentItem('Enum', enum.enum_name)]),
                               VariantItem('False', [ComponentItem('Raw', enum.base_name)])]))

    return types


def unreachable_function(type_name: str, base_name: str = None) -> Subprogram:
    return ExpressionFunction(
        unreachable_function_name(type_name),
        type_name,
        [],
        First(type_name) if not base_name else Aggregate(Value('False'), First(base_name)),
        [Precondition(FALSE)])


def unreachable_function_name(type_name: str) -> str:
    return f'Unreachable_{type_name}'.replace('.', '_')


def calculate_offset(last: MathExpr) -> Number:
    last = last.simplified({First('Buffer'): Number(0)})
    if isinstance(last, Number):
        return Number((8 - (last.value + 1) % 8) % 8)
    return Number(0)
