from pathlib import Path
from typing import Dict, List, Tuple

from rflx.ada import (FALSE, Aggregate, Aspect, Assignment, CallStatement, CaseExpression, Cast,
                      ComponentItem, ContextItem, Convert, Declaration, DerivedType, Discriminant,
                      EnumerationType, ExpressionFunction, Function, Ghost, IfExpression,
                      IfStatement, Import, LogCall, MathCall, ModularType, Package, Postcondition,
                      Pragma, PragmaStatement, Precondition, Procedure, RangeSubtype, RangeType,
                      ReturnStatement, Statement, Subprogram, TypeDeclaration, Unit,
                      UsePackageClause, UseTypeClause, VariantItem, VariantRecordType, WithClause)
from rflx.expression import (TRUE, Add, And, Attribute, Div, Equal, Expr, GreaterEqual, Last,
                             Length, LengthValue, Less, LessEqual, LogExpr, MathExpr, Mul, Number,
                             Or, Pow, Size, Sub, Value)
from rflx.model import (PDU, Array, Enumeration, Field, First, ModularInteger, RangeInteger,
                        Refinement, Variant)

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

    def write_units(self, output_directory: str) -> List[Path]:
        written_files = []

        outdir = Path(output_directory)
        if not outdir.is_dir():
            raise GeneratorError(f'invalid output directory: {outdir}')

        for unit in self.units():
            filename = outdir.joinpath(unit.package.name.lower().replace('.', '-') + '.ads')
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

                    if isinstance(field.type, Array):
                        type_name = 'Types.Index_Type'
                    else:
                        type_name = field.type.name
                    base_name = None
                    if isinstance(field.type, Enumeration) and field.type.always_valid:
                        base_name = field.type.base_name
                    function = create_unreachable_function(type_name, base_name)
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
            if refinement.sdu != 'null':
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


class GeneratorError(Exception):
    pass


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

    validation_expression: Expr
    if enum.always_valid:
        validation_expression = Value('True')
    else:
        validation_cases: List[Tuple[Expr, Expr]] = []
        validation_cases.extend((value, Value('True')) for value in enum.literals.values())
        validation_cases.append((Value('others'), Value('False')))

        validation_expression = CaseExpression(control_expression,
                                               validation_cases)
    validation_function = ExpressionFunction(f'Valid_{enum.name}',
                                             'Boolean',
                                             [('Buffer', 'Types.Bytes'),
                                              ('Offset', 'Natural')],
                                             validation_expression,
                                             [Precondition(common_precondition)])

    function_name = f'Convert_To_{enum.name}'
    parameters = [('Buffer', 'Types.Bytes'),
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
                                       [Declaration('Raw', enum.base_name, control_expression)],
                                       [ReturnStatement(CaseExpression(Value('Raw'),
                                                                       conversion_cases))],
                                       [precondition])
    else:
        conversion_cases.extend((value, Value(key)) for key, value in enum.literals.items())
        conversion_cases.append((Value('others'), LogCall(f'Unreachable_{enum.name}')))

        conversion_function = ExpressionFunction(function_name,
                                                 enum.name,
                                                 parameters,
                                                 CaseExpression(control_expression,
                                                                conversion_cases),
                                                 [precondition])

    return [validation_function, conversion_function]


def array_types() -> List[TypeDeclaration]:
    return [DerivedType('Offset_Type',
                        'Types.Index_Type')]


def array_functions(array: Array, package: str) -> List[Subprogram]:
    common_precondition = LogCall(f'Is_Contained (Buffer)')

    return [Function('Valid_First',
                     'Boolean',
                     [('Buffer', 'Types.Bytes')],
                     [],
                     [ReturnStatement(
                         LogCall('Valid_Next (Buffer, Offset_Type (Buffer\'First))'))],
                     [Precondition(common_precondition)]),
            Procedure('Get_First',
                      [('Buffer', 'Types.Bytes'),
                       ('Offset', 'out Offset_Type'),
                       ('First', 'out Types.Index_Type'),
                       ('Last', 'out Types.Index_Type')],
                      [],
                      [Assignment('Offset', Value('Offset_Type (Buffer\'First)')),
                       CallStatement('Get_Next', ['Buffer', 'Offset', 'First', 'Last'])],
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
                     [],
                     [PragmaStatement('Assume',
                                      [(f'{package}.{array.element_type}.Is_Contained '
                                        '(Buffer (Types.Index_Type (Offset) .. Buffer\'Last))')]),
                      ReturnStatement(
                          LogCall(f'{package}.{array.element_type}.Is_Valid '
                                  '(Buffer (Types.Index_Type (Offset) .. Buffer\'Last))'))],
                     [Precondition(common_precondition)]),
            Procedure('Get_Next',
                      [('Buffer', 'Types.Bytes'),
                       ('Offset', 'in out Offset_Type'),
                       ('First', 'out Types.Index_Type'),
                       ('Last', 'out Types.Index_Type')],
                      [],
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
                      [],
                      [PragmaStatement('Assume', ['Is_Contained (Buffer)'])],
                      aspects=[Postcondition(LogCall('Is_Contained (Buffer)'))])]


def calculate_offset(last: MathExpr) -> int:
    last = last.simplified({First('Buffer'): Number(0)})
    if isinstance(last, Number):
        return (8 - (last.value + 1) % 8) % 8
    # TODO: determine offset for complicated cases
    return 0


def buffer_constraints(last: MathExpr) -> LogExpr:
    last = last.simplified()
    index_constraint = LessEqual(First('Buffer'), Div(Last('Types.Index_Type'), Number(2)))
    if last != Last('Buffer'):
        length_constraint = GreaterEqual(Length('Buffer'),
                                         Add(last, -First('Buffer'), Number(1)))
        return And(length_constraint, index_constraint)
    return index_constraint


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


def create_unreachable_function(type_name: str, base_name: str = None) -> Subprogram:
    return ExpressionFunction(
        f'Unreachable_{type_name}'.replace('.', '_'),
        type_name,
        [],
        First(type_name) if not base_name else Aggregate(Value('False'), First(base_name)),
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
                [],
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
                    [('Buffer', 'Types.Bytes')],
                    [],
                    [IfStatement(
                        [(condition,
                          success_statements)]),
                     ReturnStatement(FALSE)],
                    aspects)
