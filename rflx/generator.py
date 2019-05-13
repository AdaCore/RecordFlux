from pathlib import Path
from typing import Dict, Iterable, Iterator, List, Set, Tuple, Union

from rflx.ada import (Aspect, Assignment, ComponentItem, ContextItem, Declaration, Discriminant,
                      EnumerationType, ExpressionFunction, FormalDeclaration,
                      FormalPackageDeclaration, Function, GenericFunctionInstantiation,
                      GenericPackage, GenericPackageInstantiation, Ghost, IfStatement, Import,
                      ModularType, ObjectDeclaration, Package, PackageDeclaration, Postcondition,
                      Pragma, PragmaStatement, Precondition, Procedure, RangeSubtype, RangeType,
                      ReturnStatement, Statement, Subprogram, SubprogramRenaming, Subtype,
                      TypeDeclaration, Unit, UsePackageClause, UseTypeClause, VariantItem,
                      VariantRecordType, WithClause)
from rflx.expression import (FALSE, TRUE, Add, Aggregate, And, Attribute, Call, Case, Div, Equal,
                             Expr, GreaterEqual, If, Last, Length, LengthValue, Less, LessEqual,
                             Mul, Number, Or, Pow, Size, Slice, Sub, Value)
from rflx.model import (Array, DerivedMessage, Enumeration, Field, First, Message, ModularInteger,
                        Null, RangeInteger, Reference, Refinement, Type, Variant)

COMMON_PRECONDITION = Call('Is_Contained', [Value('Buffer')])


class Generator:
    def __init__(self, prefix: str = '') -> None:
        self.__prefix = prefix
        self.__units: Dict[str, Unit] = {}
        self.__message_fields: Dict[str, Dict[str, Field]] = {}
        self.__fields: Dict[str, Field] = {}

    def generate_dissector(self, messages: List[Message], refinements: List[Refinement]) -> None:
        self.__process_messages(messages)
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

    def __process_messages(self, messages: List[Message]) -> None:
        seen_types: Set[str] = set()

        for message in messages:
            if isinstance(message, DerivedMessage):
                self.__process_derived_message(message, seen_types)
            else:
                self.__process_message(message, seen_types)

        self.__create_unreachable_functions(messages)

    def __process_message(self, message: Message, seen_types: Set[str]) -> None:
        if message.package not in self.__units:
            self.__create_protocol_unit(message.package, [])

        self.__fields = message.fields(message_facts(), First('Buffer'))
        self.__message_fields[message.full_name] = self.__fields

        self.__create_message_unit(message, self.__fields.values())

        context: List[ContextItem] = []
        generic_name = f'{self.__prefix}{message.generic_name}'
        parameters: List[FormalDeclaration] = []
        package = GenericPackage(generic_name, parameters)
        self.__units[generic_name] = Unit(context, package)

        package.subprograms.extend(
            self.__contain_functions())

        for field in self.__fields.values():
            if field.name == 'FINAL':
                continue

            if not is_seen_type(f'{message.package}.{field.type.name}', seen_types):
                self.__create_type(field.type, message.package)

            if isinstance(field.type, Array) and is_definite_array(field.type):
                if isinstance(field.type.element_type, Reference):
                    name = 'Message_Sequence'
                else:
                    name = 'Scalar_Sequence'
                context.append(
                    WithClause([f'{self.__prefix}{name}']))
                parameters.append(
                    FormalPackageDeclaration(field.type.name, name))

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
                    field))

        if self.__fields:
            package.subprograms.append(
                self.__message_validation_function(
                    self.__fields['FINAL'].variants.values()))

            package.subprograms.append(
                self.__message_length_function(
                    self.__fields['FINAL'].variants.values()))

    def __process_derived_message(self, message: DerivedMessage, seen_types: Set[str]) -> None:
        if message.package not in self.__units:
            self.__create_protocol_unit(message.package,
                                        [WithClause([f'{self.__prefix}{message.base_package}'])])

        self.__fields = message.fields(message_facts(), First('Buffer'))
        self.__message_fields[message.full_name] = self.__fields

        self.__create_message_unit(message, self.__fields.values())

        for field in self.__fields.values():
            if field.name == 'FINAL':
                continue

            if not is_seen_type(f'{message.package}.{field.type.name}', seen_types):
                self.__create_subtype(field.type, message.package, message.base_package)

    def __create_protocol_unit(self, package_name: str, context: List[ContextItem]) -> None:
        self.__units[package_name] = Unit(self.__common_context() + context,
                                          Package(f'{self.__prefix}{package_name}'))

    def __create_message_unit(self, message: Message, fields: Iterable[Field]) -> None:
        if isinstance(message, DerivedMessage):
            generic_name = f'{self.__prefix}{message.generic_base_name}'
        else:
            generic_name = f'{self.__prefix}{message.generic_name}'

        arrays = [field.type.name for field in fields if is_definite_array(field.type)]

        context: List[ContextItem] = [Pragma('SPARK_Mode'), WithClause([generic_name])]
        context.extend(
            WithClause([f'{self.__prefix}{message.package}.{array}']) for array in arrays)
        instantiation = GenericPackageInstantiation(f'{self.__prefix}{message.full_name}',
                                                    generic_name,
                                                    arrays)
        self.__units[message.full_name] = Unit(context, instantiation)

    def __create_type(self, field_type: Type, message_package: str) -> None:
        package = self.__units[message_package].package
        assert isinstance(package, Package)
        types = package.types
        subprograms = package.subprograms

        if isinstance(field_type, ModularInteger):
            types.extend(modular_types(field_type))
            subprograms.extend(self.__modular_functions(field_type))
        elif isinstance(field_type, RangeInteger):
            types.extend(range_types(field_type))
            subprograms.extend(self.__range_functions(field_type))
        elif isinstance(field_type, Enumeration):
            types.extend(enumeration_types(field_type))
            subprograms.extend(self.__enumeration_functions(field_type))
        elif isinstance(field_type, Array):
            if is_definite_array(field_type):
                if not isinstance(field_type.element_type, Reference):
                    self.__create_type(field_type.element_type, message_package)
                self.__create_array_unit(field_type, message_package)
        else:
            raise NotImplementedError(f'unsupported type "{type(field_type).__name__}"')

    def __create_subtype(self, field_type: Type, message_package: str, base_package: str) -> None:
        package = self.__units[message_package].package
        assert isinstance(package, Package)
        types = package.types
        subprograms = package.subprograms
        declarations = package.declarations

        if isinstance(field_type, ModularInteger):
            types.extend(
                Subtype(enum_type.name, f'{base_package}.{enum_type.name}')
                for enum_type in modular_types(field_type))
            subprograms.extend(
                SubprogramRenaming(f'{base_package}.{subprogram.name}', subprogram)
                for subprogram in self.__modular_functions(field_type))
        elif isinstance(field_type, RangeInteger):
            types.extend(
                Subtype(enum_type.name, f'{base_package}.{enum_type.name}')
                for enum_type in range_types(field_type))
            subprograms.extend(
                SubprogramRenaming(f'{base_package}.{subprogram.name}', subprogram)
                for subprogram in self.__range_functions(field_type))
        elif isinstance(field_type, Enumeration):
            types.extend(
                Subtype(enum_type.name, f'{base_package}.{enum_type.name}')
                for enum_type in enumeration_types(field_type))
            subprograms.extend(
                SubprogramRenaming(f'{base_package}.{subprogram.name}', subprogram)
                for subprogram in self.__enumeration_functions(field_type))
            type_name = field_type.enum_name if field_type.always_valid else field_type.name
            declarations.extend(
                ObjectDeclaration(literal, type_name, Value(f'{base_package}.{literal}'))
                for literal in field_type.literals)
        elif isinstance(field_type, Array):
            if is_definite_array(field_type):
                if not isinstance(field_type.element_type, Reference):
                    self.__create_subtype(field_type.element_type, message_package, base_package)
                self.__create_array_unit(field_type, message_package)
        else:
            raise NotImplementedError(f'unsupported type "{type(field_type).__name__}"')

    def __create_array_unit(self, array_type: Array, package_name: str) -> None:
        element_type = array_type.element_type

        array_context: List[ContextItem] = []
        array_package: PackageDeclaration
        if isinstance(element_type, Reference):
            array_context = [WithClause([f'{self.__prefix}Message_Sequence']),
                             WithClause([f'{self.__prefix}{package_name}.'
                                         f'{element_type.name}'])]
            array_package = GenericPackageInstantiation(
                f'{self.__prefix}{package_name}.{array_type.name}',
                'Message_Sequence',
                [f'{element_type.name}.Label',
                 f'{element_type.name}.Is_Contained',
                 f'{element_type.name}.Is_Valid',
                 f'{element_type.name}.Message_Length'])
        else:
            array_context = [WithClause([f'{self.__prefix}Scalar_Sequence']),
                             WithClause([f'{self.__prefix}{package_name}'])]
            array_package = GenericPackageInstantiation(
                f'{self.__prefix}{package_name}.{array_type.name}',
                f'Scalar_Sequence',
                [element_type.name,
                 str(element_type.size.to_bytes()),
                 f'Valid_{element_type.name}',
                 convert_function_name(element_type.base_name
                                       if isinstance(element_type, RangeInteger)
                                       else element_type.name)])

        self.__units[array_package.name] = Unit(array_context, array_package)

    def __create_unreachable_functions(self, messages: List[Message]) -> None:
        unreachable_functions: Dict[str, List[Subprogram]] = {}

        for message in messages:
            if isinstance(message, DerivedMessage):
                continue

            if message.package not in unreachable_functions:
                unreachable_functions[message.package] = []

            for field in message.fields().values():
                if not isinstance(field.type, Null):
                    unreachable_functions[message.package].extend(
                        self.__type_dependent_unreachable_function(field.type))

            unreachable_functions[message.package].append(unreachable_function(self.__types_length))

        for message_package, functions in unreachable_functions.items():
            top_level_package = self.__units[message_package].package
            assert isinstance(top_level_package, Package)

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
            package: PackageDeclaration

            if refinement.package not in self.__units:
                context = self.__common_context()
                package = Package(f'{self.__prefix}{refinement.package}', [], [])
                self.__units[refinement.package] = Unit(context, package)

            contains_package = f'{refinement.package}.Contains'
            if contains_package in self.__units:
                context = self.__units[contains_package].context
                package = self.__units[contains_package].package
                assert isinstance(package, Package)
            else:
                context = []
                package = Package(f'{self.__prefix}{contains_package}', [], [])
                self.__units[contains_package] = Unit(context, package)

            pdu_package = self.__prefix + refinement.pdu.rsplit('.', 1)[0]
            if pdu_package != refinement.package:
                pdu_top_level_context = [WithClause([pdu_package]), UsePackageClause([pdu_package])]
                context.extend(pdu_top_level_context)

            pdu_context = WithClause([f'{self.__prefix}{refinement.pdu}'])
            context.append(pdu_context)

            if refinement.sdu != 'null':
                sdu_context = WithClause([f'{self.__prefix}{refinement.sdu}'])
                context.append(sdu_context)

            package.subprograms.append(
                self.__contains_function(refinement))

    def __common_context(self) -> List[ContextItem]:
        return [WithClause([self.__types]),
                UseTypeClause([self.__types_bytes,
                               self.__types_index,
                               self.__types_length])]

    def __range_functions(self, integer: RangeInteger) -> List[Subprogram]:
        functions: List[Subprogram] = []

        for range_type in range_types(integer):
            if isinstance(range_type, RangeSubtype):
                continue
            functions.append(
                GenericFunctionInstantiation(convert_function_name(range_type.name),
                                             Function(f'Types.Convert_To_Int',
                                                      range_type.name,
                                                      [('Buffer', self.__types_bytes),
                                                       ('Offset', 'Natural')]),
                                             [range_type.name]))

        functions.append(self.__integer_validation_function(integer))

        return functions

    def __modular_functions(self, integer: ModularInteger) -> List[Subprogram]:
        functions: List[Subprogram] = []

        for modular_type in modular_types(integer):
            functions.append(
                GenericFunctionInstantiation(convert_function_name(modular_type.name),
                                             Function(f'Types.Convert_To_Mod',
                                                      modular_type.name,
                                                      [('Buffer', self.__types_bytes),
                                                       ('Offset', 'Natural')]),
                                             [modular_type.name]))

        functions.append(self.__integer_validation_function(integer))

        return functions

    def __integer_validation_function(self, integer: Union[ModularInteger,
                                                           RangeInteger]) -> Subprogram:
        integer_value = Call(convert_function_name(integer.base_name),
                             [Value('Buffer'),
                              Value('Offset')])
        validation_expression = integer.constraints.simplified({Value(integer.name):
                                                                integer_value})

        base_name = integer.base_name if integer.constraints != TRUE else integer.name

        return self.__type_validation_function(integer.name, base_name, validation_expression)

    def __enumeration_functions(self, enum: Enumeration) -> List[Subprogram]:
        enum_value = Call(convert_function_name(enum.base_name),
                          [Value('Buffer'),
                           Value('Offset')])

        validation_expression: Expr
        if enum.always_valid:
            validation_expression = Value('True')
        else:
            validation_cases: List[Tuple[Expr, Expr]] = []
            validation_cases.extend((value, Value('True')) for value in enum.literals.values())
            validation_cases.append((Value('others'), Value('False')))

            validation_expression = Case(enum_value, validation_cases)

        validation_function = self.__type_validation_function(enum.name, enum.base_name,
                                                              validation_expression)

        parameters = [('Buffer', self.__types_bytes),
                      ('Offset', 'Natural')]
        precondition = Precondition(
            And(type_conversion_precondition(enum.base_name),
                Call(f'Valid_{enum.name}', [Value('Buffer'), Value('Offset')])))
        conversion_cases: List[Tuple[Expr, Expr]] = []
        conversion_function: Subprogram

        if enum.always_valid:
            conversion_cases.extend((value, Aggregate(Value('True'), Value(key)))
                                    for key, value in enum.literals.items())
            conversion_cases.append((Value('others'),
                                     Aggregate(Value('False'), Value('Raw'))))

            conversion_function = Function(convert_function_name(enum.name),
                                           enum.name,
                                           parameters,
                                           [Declaration('Raw', enum.base_name, enum_value)],
                                           [ReturnStatement(Case(Value('Raw'), conversion_cases))],
                                           [precondition])
        else:
            conversion_cases.extend((value, Value(key)) for key, value in enum.literals.items())
            conversion_cases.append((Value('others'),
                                     Call(unreachable_function_name(enum.name))))

            conversion_function = ExpressionFunction(convert_function_name(enum.name),
                                                     enum.name,
                                                     parameters,
                                                     Case(enum_value, conversion_cases),
                                                     [precondition])

        enum_to_base_function = ExpressionFunction(
            convert_function_name(enum.base_name),
            enum.base_name,
            [('Enum', enum.enum_name if enum.always_valid else enum.name)],
            Case(
                Value('Enum'),
                [(Value(key), value) for key, value in enum.literals.items()]))

        base_conversion_function = GenericFunctionInstantiation(
            convert_function_name(enum.base_name),
            Function(f'Types.Convert_To_Mod',
                     enum.base_name,
                     [('Buffer', self.__types_bytes),
                      ('Offset', 'Natural')]),
            [enum.base_name])

        return [base_conversion_function,
                validation_function,
                conversion_function,
                enum_to_base_function]

    def __type_validation_function(self, type_name: str, type_base_name: str,
                                   validation_expression: Expr) -> Subprogram:
        return ExpressionFunction(f'Valid_{type_name}',
                                  'Boolean',
                                  [('Buffer', self.__types_bytes),
                                   ('Offset', 'Natural')],
                                  validation_expression,
                                  [Precondition(type_conversion_precondition(type_base_name))])

    def __contain_functions(self) -> List[Subprogram]:
        return [ExpressionFunction('Is_Contained',
                                   'Boolean',
                                   [('Buffer', self.__types_bytes)],
                                   aspects=[Ghost(), Import()]),
                Procedure('Label',
                          [('Buffer', self.__types_bytes)],
                          [],
                          [PragmaStatement('Assume', ['Is_Contained (Buffer)'])],
                          aspects=[Ghost(), Postcondition(COMMON_PRECONDITION)])]

    def __variant_validation_function(
            self,
            field: Field,
            variant_id: str,
            variant: Variant) -> Subprogram:

        type_constraints: Expr = TRUE

        if field.type.constraints != TRUE or isinstance(field.type, Enumeration):
            first_byte, last_byte, offset = self.__field_location(field.name, variant_id, variant)
            type_constraints = Call(f'Valid_{field.type.name}',
                                    [Slice('Buffer', first_byte, last_byte), offset])

        value_to_call = self.__value_to_call_facts([(field.name, variant_id)] + variant.previous)

        return ExpressionFunction(
            f'Valid_{field.name}_{variant_id}',
            'Boolean',
            [('Buffer', self.__types_bytes)],
            And(Call(f'Valid_{variant.previous[-1][0]}_{variant.previous[-1][1]}',
                     [Value('Buffer')])
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
                Call(f'Valid_{field.name}_{variant_id}', [Value('Buffer')])))

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
                    Call(convert_function_name(field.type.name
                                               if field.type.constraints == TRUE
                                               else field.type.base_name),
                         [Slice('Buffer', first_byte, last_byte),
                          offset]),
                    [precondition]))
        return functions

    def __field_validation_function(self, field: Field) -> Subprogram:
        variants: List[Expr] = list(self.__valid_variants(field))

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

    def __field_accessor_functions(self, field: Field) -> List[Subprogram]:
        precondition = Precondition(And(COMMON_PRECONDITION,
                                        Call(f'Valid_{field.name}', [Value('Buffer')])))

        functions: List[Subprogram] = []
        if isinstance(field.type, Array):
            for attribute in ['First', 'Last']:
                functions.append(
                    ExpressionFunction(
                        f'Get_{field.name}_{attribute}',
                        self.__types_index,
                        [('Buffer', self.__types_bytes)],
                        If([
                            (Call(f'Valid_{field.name}_{variant_id}', [Value('Buffer')]),
                             Call(f'Get_{field.name}_{variant_id}_{attribute}', [Value('Buffer')]))
                            for variant_id in field.variants],
                            unreachable_function_name(self.__types_index)),
                        [precondition]))

            body: List[Statement] = [
                Assignment('First', Call(f'Get_{field.name}_First', [Value('Buffer')])),
                Assignment('Last', Call(f'Get_{field.name}_Last', [Value('Buffer')]))]
            postcondition = Postcondition(
                And(Equal(Value('First'),
                          Call(f'Get_{field.name}_First', [Value('Buffer')])),
                    Equal(Value('Last'),
                          Call(f'Get_{field.name}_Last', [Value('Buffer')]))))
            if is_definite_array(field.type):
                predicate = Call(f'{field.type.name}.Is_Contained',
                                 [Slice('Buffer', Value('First'), Value('Last'))])
                body.append(PragmaStatement('Assume', [str(predicate)]))
                postcondition.expr = And(postcondition.expr, predicate)

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
                    If([(Call(f'Valid_{field.name}_{variant_id}', [Value('Buffer')]),
                         Call(f'Get_{field.name}_{variant_id}', [Value('Buffer')]))
                        for variant_id in field.variants],
                       unreachable_function_name(field.type.name)),
                    [precondition]))

        return functions

    def __message_validation_function(self, variants: Iterable[Variant]) -> Subprogram:
        expr: Expr = FALSE

        for variant in variants:
            condition = self.__variant_condition(variant)
            expr = condition if expr == FALSE else Or(expr, condition)

        return ExpressionFunction(
            'Is_Valid',
            'Boolean',
            [('Buffer', self.__types_bytes)],
            expr,
            [Precondition(COMMON_PRECONDITION)])

    def __message_length_function(self, variants: Iterable[Variant]) -> Subprogram:
        condition_expressions: List[Tuple[Expr, Expr]] = []

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
            If(condition_expressions, unreachable_function_name(self.__types_length)),
            [Precondition(And(COMMON_PRECONDITION, Call('Is_Valid', [Value('Buffer')])))])

    def __variant_condition(self, variant: Variant) -> Expr:
        field_name, variant_id = variant.previous[-1]
        return And(
            Call(f'Valid_{field_name}_{variant_id}', [Value('Buffer')]),
            variant.condition
        ).simplified(variant.facts).simplified(self.__value_to_call_facts(variant.previous))

    def __contains_function(self, ref: Refinement) -> Subprogram:
        sdu_name = ref.sdu.rsplit('.', 1)[1] if ref.sdu.startswith(ref.package) else ref.sdu \
            if ref.sdu != 'null' else 'Null'
        pdu_name = ref.pdu.rsplit('.', 1)[1] if ref.pdu.startswith(ref.package) else ref.pdu
        name = f'{sdu_name}_In_{pdu_name}_{ref.field}'.replace('.', '_')

        message_fields = self.__message_fields[ref.pdu]
        condition_fields = [field for field in message_fields if Value(field) in ref.condition]
        declarations = [Declaration(field,
                                    message_fields[field].type.name,
                                    Call(f'{ref.pdu}.Get_{field}', [Value('Buffer')]))
                        for field in condition_fields]

        condition = ref.condition
        for field in condition_fields:
            field_type = message_fields[field].type
            if isinstance(field_type, Enumeration) and field_type.always_valid:
                condition = And(Call(f'{field}.Known'),
                                condition.simplified({Value(field): Value(f'{field}.Enum')}))

        success_statements: List[Statement] = [ReturnStatement(TRUE)]
        aspects: List[Aspect] = [
            Precondition(
                And(Call(f'{ref.pdu}.Is_Contained', [Value('Buffer')]),
                    Call(f'{ref.pdu}.Is_Valid', [Value('Buffer')])))]
        if ref.sdu != 'null':
            success_statements.insert(
                0,
                PragmaStatement(
                    'Assume',
                    [(f'{ref.sdu}.Is_Contained (Buffer ({ref.pdu}.Get_{ref.field}_First (Buffer)'
                      f' .. {ref.pdu}.Get_{ref.field}_Last (Buffer)))')]))
            aspects.append(
                Postcondition(
                    If(
                        [(Call(f'{name}\'Result'),
                          Call(f'{ref.sdu}.Is_Contained',
                               [Slice('Buffer',
                                      Call(f'{ref.pdu}.Get_{ref.field}_First',
                                           [Value('Buffer')]),
                                      Call(f'{ref.pdu}.Get_{ref.field}_Last',
                                           [Value('Buffer')]))]))])))

        return Function(name,
                        'Boolean',
                        [('Buffer', self.__types_bytes)],
                        declarations,
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
            variant: Variant) -> Tuple[Expr, Expr, Number]:

        value_to_call = self.__value_to_call_facts(
            [(field_name, variant_id)] + variant.previous)
        first_byte = variant.facts[First(field_name)].to_bytes().simplified(value_to_call)
        last_byte = variant.facts[Last(field_name)].to_bytes().simplified(value_to_call)
        offset = calculate_offset(variant.facts[Last(field_name)])
        return (first_byte, last_byte, offset)

    def __value_to_call_facts(self, previous: List[Tuple[str, str]]) -> Dict[Attribute, Expr]:
        result: Dict[Attribute, Expr] = {}
        for field_name, vid in previous:
            expr: Expr
            if isinstance(self.__fields[field_name].type, Array):
                first = self.__fields[field_name].variants[vid].facts[First(field_name)].to_bytes()
                last = self.__fields[field_name].variants[vid].facts[Last(field_name)].to_bytes()
                expr = Slice('Buffer', first, last)
            else:
                expr = Call(f'Get_{field_name}_{vid}', [Value('Buffer')])
                result[LengthValue(field_name)] = Call(self.__types_length, [expr])
            result[Value(field_name)] = expr
        return result

    def __buffer_constraints(self, last: Expr) -> Expr:
        last = last.simplified()
        index_constraint = LessEqual(First('Buffer'), Div(Last(self.__types_index), Number(2)))
        if last != Last('Buffer'):
            length_constraint = GreaterEqual(Length('Buffer'),
                                             Add(last, -First('Buffer'), Number(1)))
            return And(length_constraint, index_constraint)
        return index_constraint

    def __valid_variants(self, field: Field) -> Iterator[Expr]:
        for variant_id, variant in field.variants.items():
            expression: Expr = Call(f'Valid_{field.name}_{variant_id}', [Value('Buffer')])
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


def type_conversion_precondition(type_name: str) -> Expr:
    return And(Less(Value('Offset'),
                    Number(8)),
               Equal(Length('Buffer'),
                     Add(Div(Add(Size(type_name),
                                 Value('Offset'),
                                 Number(-1)),
                             Number(8)),
                         Number(1))))


def unreachable_function(type_name: str, base_name: str = None) -> Subprogram:
    return ExpressionFunction(
        unreachable_function_name(type_name),
        type_name,
        [],
        First(type_name) if not base_name else Aggregate(Value('False'), First(base_name)),
        [Precondition(FALSE)])


def unreachable_function_name(type_name: str) -> str:
    return f'Unreachable_{type_name}'.replace('.', '_')


def convert_function_name(name: str) -> str:
    return f'Convert_To_{name}'


def calculate_offset(last: Expr) -> Number:
    last = last.simplified({First('Buffer'): Number(0)})
    if isinstance(last, Number):
        return Number((8 - (last.value + 1) % 8) % 8)
    return Number(0)


def message_facts() -> Dict[Attribute, Expr]:
    return {
        First('Message'): Mul(First('Buffer'), Number(8)),
        Last('Message'): Mul(Last('Buffer'), Number(8)),
        Length('Message'): Sub(Add(Mul(Last('Buffer'), Number(8)), Number(8)),
                               Mul(First('Buffer'), Number(8)))
    }


def is_seen_type(type_name: str, seen_types: Set[str]) -> bool:
    seen = type_name in seen_types
    seen_types.add(type_name)
    return seen


def is_definite_array(type_: Type) -> bool:
    return isinstance(type_, Array) and 'Payload' not in type_.name
