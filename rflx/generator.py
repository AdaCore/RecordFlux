# pylint: disable=too-many-lines
import itertools
from pathlib import Path
from typing import Dict, List, Mapping, Sequence, Set, Tuple

from rflx.ada import (ArrayType, Assignment, CallStatement, Component, ContextItem, Declaration,
                      DefaultInitialCondition, Discriminant, DynamicPredicate, EnumerationType,
                      ExpressionFunctionDeclaration, FormalDeclaration, FormalPackageDeclaration,
                      FormalSubprogramDeclaration, FunctionSpecification,
                      GenericFunctionInstantiation, GenericPackageInstantiation, IfStatement,
                      InOutParameter, InstantiationUnit, ModularType, NullComponent,
                      ObjectDeclaration, OutParameter, PackageBody, PackageDeclaration, PackageUnit,
                      Parameter, Postcondition, Pragma, PragmaStatement, Precondition, PrivateType,
                      ProcedureSpecification, RangeSubtype, RangeType, RecordType, ReturnStatement,
                      Subprogram, SubprogramBody, SubprogramDeclaration,
                      SubprogramRenamingDeclaration, SubprogramSpecification, SubprogramUnitPart,
                      Subtype, TypeDeclaration, Unit, UnitPart, UsePackageClause, UseTypeClause,
                      Variant, VariantPart, WithClause)
from rflx.expression import (FALSE, TRUE, UNDEFINED, Add, Aggregate, And, Call, Case, Constrained,
                             Div, Equal, Expr, First, ForAllIn, GreaterEqual, If, Indexed, Last,
                             Length, Less, LessEqual, Mod, Name, NamedAggregate, Not, NotEqual,
                             Number, Old, Or, Pow, Range, Result, Selected, Size, Slice, Sub,
                             Variable)
from rflx.model import (FINAL, INITIAL, Array, Composite, DerivedMessage, Enumeration, Field,
                        Message, ModularInteger, Payload, RangeInteger, Reference, Refinement,
                        Scalar, Type)

NULL = Name('null')
VALID_CONTEXT = Call('Valid_Context', [Name('Context')])  # WORKAROUND: Componolit/Workarounds#1


class Generator:
    def __init__(self, prefix: str = '') -> None:
        self.prefix = prefix
        self.units: Dict[str, Unit] = {}
        self.messages: Dict[str, Message] = {}

    def generate_dissector(self, messages: List[Message], refinements: List[Refinement]) -> None:
        self.__process_messages(messages)
        self.__process_refinements(refinements)

    def write_units(self, directory: Path) -> List[Path]:
        written_files = []

        for unit in self.units.values():
            filename = directory.joinpath(unit.name + '.ads')
            written_files.append(filename)
            with open(filename, 'w') as f:
                f.write(unit.specification)

            if unit.body:
                filename = directory.joinpath(unit.name + '.adb')
                written_files.append(filename)
                with open(filename, 'w') as f:
                    f.write(unit.body)

        return written_files

    def __process_messages(self, messages: Sequence[Message]) -> None:
        seen_types: Set[str] = set()

        for message in messages:
            self.messages[message.full_name] = message
            if isinstance(message, DerivedMessage):
                self.__process_derived_message(message, seen_types)
            else:
                self.__process_message(message, seen_types)

    def __process_refinements(self, refinements: List[Refinement]) -> None:
        for refinement in refinements:
            if refinement.package not in self.units:
                self.__create_unit(refinement.package, [], True)

            null_sdu = not self.messages[refinement.sdu].fields

            context = []
            pdu_package = self.prefix + refinement.pdu.rsplit('.', 1)[0]
            if pdu_package != refinement.package:
                context.extend([WithClause(pdu_package), UsePackageClause(pdu_package)])
            context.append(
                WithClause(f'{self.prefix}{refinement.pdu}'))
            if not null_sdu:
                context.append(
                    WithClause(f'{self.prefix}{refinement.sdu}'))

            contains_package = f'{refinement.package}.Contains'
            if contains_package in self.units:
                self.units[contains_package].context.extend(context)
            else:
                self.__create_unit(contains_package, context, False)

            condition_fields = {
                f: t for f, t in self.messages[refinement.pdu].types.items()
                if Variable(f.name) in refinement.condition}

            self.units[contains_package] += self.__create_contains_function(refinement,
                                                                            condition_fields,
                                                                            null_sdu)
            if not null_sdu:
                self.units[contains_package] += self.__create_switch_procedure(refinement,
                                                                               condition_fields)

    def __process_message(self, message: Message, seen_types: Set[str]) -> None:
        if not message.fields:
            return

        if message.package not in self.units:
            self.__create_unit(message.package, [], True)

        self.__create_generic_message_unit(message)
        self.__create_message_unit(message)

        for field_type in message.types.values():
            if not is_seen_type(f'{message.package}.{field_type}', seen_types):
                self.__create_type(field_type, message.package)

    def __process_derived_message(self, message: DerivedMessage, seen_types: Set[str]) -> None:
        if message.package not in self.units:
            self.__create_unit(
                message.package,
                [WithClause(f'{self.prefix}{message.base_package}')],
                True)

        self.__create_message_unit(message)

        for field_type in message.types.values():
            if not is_seen_type(f'{message.package}.{field_type.name}', seen_types):
                self.__create_subtype(field_type, message.package, message.base_package)

    def __create_unit(self, package_name: str, context: List[ContextItem], top_level: bool) -> None:
        name = f'{self.prefix}{package_name}'
        self.units[package_name] = PackageUnit(
            self.__common_context() + context if top_level else context,
            PackageDeclaration(name),
            PackageBody(name))

    def __create_generic_message_unit(self, message: Message) -> None:
        context: List[ContextItem] = [
            WithClause(self.types),
            UseTypeClause(f'{self.types}.Integer_Address')
        ]
        generic_name = f'{self.prefix}{message.generic_name}'
        parameters: List[FormalDeclaration] = []
        unit = PackageUnit(
            context,
            PackageDeclaration(generic_name, formal_parameters=parameters),
            PackageBody(generic_name))
        self.units[generic_name] = unit

        for field_type in message.types.values():
            if isinstance(field_type, Array):
                if isinstance(field_type.element_type, Reference):
                    name = 'Message_Sequence'
                else:
                    name = 'Scalar_Sequence'
                context.append(
                    WithClause(f'{self.prefix}{name}'))
                parameters.append(
                    FormalPackageDeclaration(f'{field_type.name}_Sequence', name))

        unit += self.__create_pragmas()
        unit += self.__create_field_type(message)
        unit += self.__create_state_type()

        scalar_fields = {f: t for f, t in message.types.items() if isinstance(t, Scalar)}
        composite_fields = [f for f in message.fields if f not in scalar_fields]

        context_invariant = [
            Equal(e, Old(e)) for e in (
                Name('Context.Buffer_First'),
                Name('Context.Buffer_Last'),
                Name('Context.Buffer_Address'),
                Name('Context.First'),
                Name('Context.Last')
            )
        ]

        unit += self.__create_result_type(scalar_fields, composite_fields)
        unit += self.__create_cursor_type(message)
        unit += self.__create_valid_context_function(message)
        unit += self.__create_context_type()
        unit += self.__create_create_function()
        unit += self.__create_initialize_procedure()
        unit += self.__create_restricted_initialize_procedure()
        unit += self.__create_take_buffer_procedure(message, context_invariant)
        unit += self.__create_has_buffer_function()
        unit += self.__create_field_range_procedure()
        unit += self.__create_index_function()
        unit += self.__create_internal_valid_functions(message)
        unit += self.__create_internal_field_functions(message, composite_fields)
        unit += self.__create_internal_accessor_functions(message)
        unit += self.__create_verify_procedure(message, context_invariant)
        unit += self.__create_verify_message_procedure(message, context_invariant)
        unit += self.__create_present_function()
        unit += self.__create_structural_valid_function()
        unit += self.__create_valid_function()
        unit += self.__create_incomplete_function()
        unit += self.__create_structural_valid_message_function(message)
        unit += self.__create_valid_message_function(message)
        unit += self.__create_incomplete_message_function(message)
        unit += self.__create_scalar_accessor_functions(scalar_fields)
        unit += self.__create_composite_accessor_procedures(composite_fields)

        sequence_fields = [f for f in message.fields if isinstance(message.types[f], Array)]

        unit += self.__create_switch_procedures(message, sequence_fields)
        unit += self.__create_update_procedures(message, sequence_fields)
        unit += self.__create_public_valid_context_function()

    @staticmethod
    def __create_pragmas() -> UnitPart:
        return UnitPart(
            [Pragma('Unevaluated_Use_Of_Old', ['Allow'])]
        )

    @staticmethod
    def __create_field_type(message: Message) -> UnitPart:
        return UnitPart(
            [EnumerationType(
                'All_Field_Type',
                dict.fromkeys(f.affixed_name for f in message.all_fields)),
             RangeSubtype(
                 'Field_Type',
                 'All_Field_Type',
                 Name(message.all_fields[1].affixed_name),
                 Name(message.all_fields[-2].affixed_name))]
        )

    @staticmethod
    def __create_state_type() -> UnitPart:
        return UnitPart(
            private=[
                EnumerationType(
                    'State_Type',
                    dict.fromkeys(('S_Valid', 'S_Structural_Valid', 'S_Invalid',
                                   'S_Preliminary', 'S_Incomplete')))
            ]
        )

    def __create_cursor_type(self, message: Message) -> UnitPart:
        discriminants = [Discriminant(['State'], 'State_Type', Name('S_Invalid'))]

        return UnitPart(
            private=[
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        'Valid_Type',
                        'Boolean',
                        [Parameter(['Value'], 'Result_Type')]),
                    Case(Name('Value.Field'),
                         [(Name(f.affixed_name),
                           Call('Valid', [Name(f'Value.{f.name}_Value')]) if isinstance(t, Scalar)
                           else TRUE)
                          for f, t in message.types.items()]
                         + [(Name(INITIAL.affixed_name), FALSE),
                            (Name(FINAL.affixed_name), FALSE)])),
                RecordType(
                    'Cursor_Type',
                    [],
                    discriminants,
                    VariantPart(
                        'State',
                        [Variant(
                            [Name('S_Valid'),
                             Name('S_Structural_Valid'),
                             Name('S_Preliminary')],
                            [Component('First', self.types_bit_index),
                             Component('Last', self.types_bit_length),
                             Component('Value', 'Result_Type')]),
                         Variant(
                            [Name('S_Invalid'),
                             Name('S_Incomplete')],
                            [NullComponent()])]),
                    [DynamicPredicate(
                        If([(Or(Equal(Name('State'), Name('S_Valid')),
                                Equal(Name('State'), Name('S_Structural_Valid'))),
                             Call('Valid_Type', [Name('Value')]))]))]),
                ArrayType(
                    'Cursors_Type',
                    'Field_Type',
                    'Cursor_Type')
            ]
        )

    def __context_type_predicate(self, message: Message, prefix: bool = False) -> Expr:
        def prefixed(name: str) -> Name:
            return Selected(Name('Context'), name) if prefix else Name(name)

        def appended(fields: Tuple[Field, ...], field: Field) -> Tuple[Field, ...]:
            return fields + (tuple([field]) if field not in (INITIAL, FINAL) else tuple())

        def field_state_invariants(field: Field) -> Sequence[Expr]:
            return [
                Or(
                    *[Equal(
                        Selected(
                            Indexed(
                                prefixed('Cursors'),
                                Name(p.affixed_name)),
                            'State'),
                        Name(s))
                      for s in ('S_Valid', 'S_Structural_Valid')])
                for p in appended(message.definite_predecessors(field), field)]

        def field_condition_invariants(field: Field) -> Sequence[Expr]:
            result = []
            for f in (message.definite_predecessors(field)
                      + (tuple([field]) if field not in (INITIAL, FINAL) else tuple())):
                conditions = [l.condition for l in message.outgoing(f) if l.condition != TRUE]
                if conditions:
                    result.append(
                        Or(*conditions).simplified(self.__substitution(message, prefix)))
            return result

        def field_length_invariants(field: Field) -> Sequence[Expr]:
            return [
                Equal(
                    Add(
                        Selected(
                            Indexed(
                                prefixed('Cursors'),
                                Name(f.affixed_name)),
                            'Last'),
                        -Selected(
                            Indexed(
                                prefixed('Cursors'),
                                Name(f.affixed_name)),
                            'First'),
                        Number(1)),
                    Size(base_type_name(t)))
                for f, t in message.types.items()
                if f in (message.definite_predecessors(field)
                         + (tuple([field]) if field not in (INITIAL, FINAL) else tuple()))
                and isinstance(t, Scalar)]

        return And(
            If([
                (NotEqual(
                    Name(prefixed('Buffer')),
                    NULL),
                 And(
                     Equal(
                         First(prefixed('Buffer')),
                         Name(prefixed('Buffer_First'))),
                     Equal(
                         Last(prefixed('Buffer')),
                         Name(prefixed('Buffer_Last'))),
                     Equal(
                         Call(f'{self.types}.Bytes_Address', [Name(prefixed('Buffer'))]),
                         Name(prefixed('Buffer_Address')))))]),
            GreaterEqual(
                Call(self.types_byte_index, [Name(prefixed('First'))]),
                Name(prefixed('Buffer_First'))),
            LessEqual(
                Call(self.types_byte_index, [Name(prefixed('Last'))]),
                Name(prefixed('Buffer_Last'))),
            LessEqual(
                Name(prefixed('First')),
                Name(prefixed('Last'))),
            LessEqual(
                Name(prefixed('Last')),
                Div(Last(self.types_bit_index), Number(2))),
            GreaterEqual(
                Name(prefixed('Index')),
                Name(prefixed('First'))),
            LessEqual(
                Sub(Name(prefixed('Index')), Name(prefixed('Last'))),
                Number(1)),
            ForAllIn('F',
                     Range(First('Field_Type'), Last('Field_Type')),
                     If([(
                         Or(
                             *[Equal(
                                 Selected(
                                     Indexed(prefixed('Cursors'), Name('F')),
                                     'State'),
                                 Name(s))
                               for s in ('S_Valid', 'S_Structural_Valid')]),
                         And(
                             GreaterEqual(
                                 Selected(
                                     Indexed(prefixed('Cursors'), Name('F')),
                                     'First'),
                                 Name(prefixed('First'))),
                             LessEqual(
                                 Selected(
                                     Indexed(prefixed('Cursors'), Name('F')),
                                     'Last'),
                                 Name(prefixed('Last'))),
                             LessEqual(
                                 Selected(
                                     Indexed(prefixed('Cursors'), Name('F')),
                                     'First'),
                                 Add(
                                     Selected(
                                         Indexed(prefixed('Cursors'), Name('F')),
                                         'Last'),
                                     Number(1))),
                             Equal(
                                 Selected(
                                     Selected(
                                         Indexed(prefixed('Cursors'), Name('F')),
                                         'Value'),
                                     'Field'),
                                 Name('F'))))])),
            Case(Name(prefixed('Field')),
                 [(Name(f.affixed_name),
                   And(*field_state_invariants(f),
                       *field_condition_invariants(f),
                       *field_length_invariants(f)))
                  for f in message.all_fields]))

    def __create_context_type(self) -> UnitPart:
        discriminants = [
            Discriminant(['Buffer_First', 'Buffer_Last'],
                         self.types_index,
                         First(self.types_index)),
            Discriminant(['First', 'Last'],
                         self.types_bit_index,
                         First(self.types_bit_index)),
            Discriminant(['Buffer_Address'],
                         f'{self.types}.Integer_Address',
                         Number(0))
        ]

        return UnitPart(
            [PrivateType('Context_Type',
                         discriminants,
                         [DefaultInitialCondition(FALSE)])],
            [],
            [RecordType(
                'Context_Type',
                [Component('Buffer', self.types_bytes_ptr, NULL),
                 Component('Index', self.types_bit_index, First(Name(self.types_bit_index))),
                 Component('Field', 'All_Field_Type', Name(INITIAL.affixed_name)),
                 Component('Cursors', 'Cursors_Type',
                           NamedAggregate(('others',
                                           NamedAggregate(('State',
                                                           Name('S_Invalid'))))))],
                discriminants,
                None,
                [DynamicPredicate(
                    Call('Valid_Context',
                         [Name('Buffer_First'),
                          Name('Buffer_Last'),
                          Name('First'),
                          Name('Last'),
                          Name('Buffer_Address'),
                          Name('Buffer'),
                          Name('Index'),
                          Name('Field'),
                          Name('Cursors')]))])]
        )

    @staticmethod
    def __create_result_type(scalar_fields: Mapping[Field, Scalar],
                             composite_fields: Sequence[Field]) -> UnitPart:
        result_variants = ([Variant([Name(f.affixed_name)
                                     for f in [INITIAL, *composite_fields, FINAL]],
                                    [NullComponent()])]
                           + [Variant([Name(f.affixed_name)],
                                      [Component(f'{f.name}_Value',
                                                 base_type_name(t))])
                              for f, t in scalar_fields.items()])

        return UnitPart(
            private=[
                RecordType(
                    'Result_Type',
                    [],
                    [Discriminant(['Field'], 'All_Field_Type', Name(INITIAL.affixed_name))],
                    VariantPart('Field', result_variants))
            ]
        )

    def __create_create_function(self) -> UnitPart:
        specification = FunctionSpecification('Create', 'Context_Type')

        return UnitPart(
            [SubprogramDeclaration(
                specification)],
            [ExpressionFunctionDeclaration(
                specification,
                Aggregate(
                    First(self.types_index),
                    First(self.types_index),
                    First(self.types_bit_index),
                    First(self.types_bit_index),
                    Number(0),
                    NULL,
                    First(self.types_bit_index),
                    Name(INITIAL.affixed_name),
                    NamedAggregate(('others',
                                    NamedAggregate(('State',
                                                    Name('S_Invalid')))))))])

    def __create_initialize_procedure(self) -> UnitPart:
        specification = ProcedureSpecification(
            'Initialize',
            [OutParameter(['Context'], 'Context_Type'),
             InOutParameter(['Buffer'], self.types_bytes_ptr)])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(
                    And(Not(Constrained('Context')),
                        NotEqual(Name('Buffer'), NULL),
                        LessEqual(Last('Buffer'), Div(Last(self.types_index), Number(2))))),
                 Postcondition(
                     And(VALID_CONTEXT,
                         Call('Has_Buffer', [Name('Context')]),
                         # WORKAROUND: Componolit/Workarounds#6
                         Equal(Selected('Context', 'Buffer_First'),
                               Old(Call(f'{self.types}.Bytes_First', [Name('Buffer')]))),
                         Equal(Selected('Context', 'Buffer_Last'),
                               Old(Call(f'{self.types}.Bytes_Last', [Name('Buffer')]))),
                         Equal(Name('Buffer'), NULL)))])],
            [SubprogramBody(
                specification,
                [],
                [CallStatement('Initialize',
                               [Name('Context'),
                                Name('Buffer'),
                                Call(self.types_first_bit_index, [First('Buffer')]),
                                Call(self.types_last_bit_index, [Last('Buffer')])])])])

    def __create_restricted_initialize_procedure(self) -> UnitPart:
        specification = ProcedureSpecification(
            'Initialize',
            [OutParameter(['Context'], 'Context_Type'),
             InOutParameter(['Buffer'], self.types_bytes_ptr),
             Parameter(['First', 'Last'], self.types_bit_index)])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(
                    And(Not(Constrained('Context')),
                        NotEqual(Name('Buffer'), NULL),
                        GreaterEqual(Call(self.types_byte_index, [Name('First')]), First('Buffer')),
                        LessEqual(Call(self.types_byte_index, [Name('Last')]), Last('Buffer')),
                        LessEqual(Name('First'), Name('Last')),
                        LessEqual(Name('Last'), Div(Last(self.types_bit_index), Number(2))))),
                 Postcondition(
                     And(VALID_CONTEXT,
                         Equal(Name('Buffer'), NULL),
                         Call('Has_Buffer', [Name('Context')]),
                         # WORKAROUND: Componolit/Workarounds#6
                         Equal(Selected('Context', 'Buffer_First'),
                               Old(Call(f'{self.types}.Bytes_First', [Name('Buffer')]))),
                         Equal(Selected('Context', 'Buffer_Last'),
                               Old(Call(f'{self.types}.Bytes_Last', [Name('Buffer')]))),
                         Equal(Name('Context.Buffer_Address'),
                               Old(Call(f'{self.types}.Bytes_Address', [Name('Buffer')]))),
                         Equal(Name('Context.First'), Name('First')),
                         Equal(Name('Context.Last'), Name('Last'))))])],
            [SubprogramBody(
                specification,
                [ObjectDeclaration('Buffer_First', self.types_index, First('Buffer'), True),
                 ObjectDeclaration('Buffer_Last', self.types_index, Last('Buffer'), True),
                 ObjectDeclaration(
                    'Buffer_Address',
                    f'{self.types}.Integer_Address',
                    Call(f'{self.types}.Bytes_Address',
                         [Name('Buffer')]),
                    True)],
                [Assignment(
                    'Context',
                    Aggregate(
                        Name('Buffer_First'),
                        Name('Buffer_Last'),
                        Name('First'),
                        Name('Last'),
                        Name('Buffer_Address'),
                        Name('Buffer'),
                        Name('First'),
                        Name(INITIAL.affixed_name),
                        NamedAggregate(
                            ('others',
                             NamedAggregate(
                                 ('State',
                                  Name('S_Invalid'))))))),
                 Assignment(
                    'Buffer',
                    NULL)])])

    def __create_take_buffer_procedure(self, message: Message,
                                       context_invariant: Sequence[Expr]) -> UnitPart:
        specification = ProcedureSpecification(
            'Take_Buffer',
            [InOutParameter(['Context'], 'Context_Type'),
             OutParameter(['Buffer'], self.types_bytes_ptr)])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(
                    And(VALID_CONTEXT,
                        Call('Has_Buffer', [Name('Context')]))),
                 Postcondition(
                    And(VALID_CONTEXT,
                        Not(Call('Has_Buffer', [Name('Context')])),
                        NotEqual(Name('Buffer'),
                                 NULL),
                        Equal(Name('Context.Buffer_First'),
                              First('Buffer')),
                        Equal(Name('Context.Buffer_Last'),
                              Last('Buffer')),
                        Equal(Name('Context.Buffer_Address'),
                              Call(f'{self.types}.Bytes_Address', [Name('Buffer')])),
                        *context_invariant,
                        *[Equal(Call('Present', [Name('Context'), Name(f.affixed_name)]),
                                Old(Call('Present', [Name('Context'), Name(f.affixed_name)])))
                          for f in message.fields]))])],
            [SubprogramBody(
                specification,
                [],
                [Assignment(
                    'Buffer',
                    Name('Context.Buffer')),
                 Assignment(
                    'Context.Buffer',
                    NULL)])])

    def __create_field_range_procedure(self) -> UnitPart:
        specification = ProcedureSpecification(
            'Field_Range',
            [Parameter(['Context'], 'Context_Type'),
             Parameter(['Field'], 'Field_Type'),
             OutParameter(['First'], self.types_bit_index),
             OutParameter(['Last'], self.types_bit_index)])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(
                    And(VALID_CONTEXT,
                        Call('Present', [Name('Context'), Name('Field')]))),
                 Postcondition(
                    And(Call('Present', [Name('Context'), Name('Field')]),
                        LessEqual(
                            Name('Context.First'),
                            Name('First')),
                        GreaterEqual(
                            Name('Context.Last'),
                            Name('Last')),
                        LessEqual(
                            Name('First'),
                            Name('Last'))))])],
            [SubprogramBody(
                specification,
                [],
                [Assignment(
                    'First',
                    Selected(
                        Indexed(
                            'Context.Cursors',
                            Name('Field')),
                        'First')),
                 Assignment(
                    'Last',
                    Selected(
                        Indexed(
                            'Context.Cursors',
                            Name('Field')),
                        'Last'))])])

    def __create_index_function(self) -> UnitPart:
        specification = FunctionSpecification(
            'Index',
            self.types_bit_index,
            [Parameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(
                    VALID_CONTEXT),
                 Postcondition(
                    And(
                        GreaterEqual(
                            Result('Index'),
                            Name('Context.First')),
                        LessEqual(
                            Sub(Result('Index'), Name('Context.Last')),
                            Number(1))))])],
            [ExpressionFunctionDeclaration(
                specification,
                Selected('Context', 'Index'))])

    @staticmethod
    def __create_internal_valid_functions(message: Message) -> UnitPart:
        return UnitPart(
            [],
            [ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Preliminary_Valid',
                    'Boolean',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                And(
                    Or(
                        *[Equal(
                            Selected(
                                Indexed('Context.Cursors', Name('Field')),
                                'State'),
                            Name(s))
                          for s in ('S_Valid', 'S_Structural_Valid', 'S_Preliminary')]),
                    Equal(
                        Selected(
                            Indexed('Context.Cursors', Name('Field')),
                            'Value.Field'),
                        Name('Field')))),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Preliminary_Valid_Predecessors',
                    'Boolean',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'All_Field_Type')]),
                Case(
                    Name('Field'),
                    [(Name(f.affixed_name),
                      And(*[Call('Preliminary_Valid', [Name('Context'), Name(p.affixed_name)])
                            for p in message.definite_predecessors(f)]))
                     for f in message.all_fields])),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Valid_Predecessors',
                    'Boolean',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                Case(
                    Name('Field'),
                    [(Name(f.affixed_name),
                      And(*[Call('Present', [Name('Context'), Name(p.affixed_name)])
                            for p in message.definite_predecessors(f)]))
                     for f in message.fields]),
                [Postcondition(
                    If([(Result('Valid_Predecessors'),
                         Call('Preliminary_Valid_Predecessors',
                              [Name('Context'), Name('Field')]))]))]),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Valid_Target',
                    'Boolean',
                    [Parameter(['Source_Field', 'Target_Field'], 'All_Field_Type')]),
                Case(
                    Name('Source_Field'),
                    [(Name(f.affixed_name),
                      Or(*([Equal(Name('Target_Field'), Name(s.affixed_name))
                           for s in message.direct_successors(f)] or (FALSE,))))
                     for f in message.all_fields]))
             ])

    def __create_internal_field_functions(self, message: Message,
                                          composite_fields: Sequence[Field]) -> UnitPart:
        def condition(field: Field, message: Message) -> Expr:
            cases: List[Tuple[Expr, Expr]] = [
                (target, Or(*[c for _, c in conditions]))
                for target, conditions in itertools.groupby(
                    [(Name(l.target.affixed_name), l.condition) for l in message.outgoing(field)],
                    lambda x: x[0])
            ]
            cases.append((Name('others'), FALSE))
            return Case(Name('Target_Field'), cases).simplified(self.__substitution(message))

        def length(field: Field, message: Message) -> Expr:
            target_links = [(target, list(links))
                            for target, links in itertools.groupby(message.outgoing(field),
                                                                   lambda x: x[1])
                            if target != FINAL]
            cases: List[Tuple[Expr, Expr]] = []
            for target, links in target_links:
                field_type = message.types[target]
                length: Expr
                if isinstance(field_type, Scalar):
                    length = Size(base_type_name(field_type))
                else:
                    if len(links) == 1:
                        length = links[0].length
                    elif len(links) > 1:
                        length = If([(l.condition, l.length) for l in links],
                                    Name(self.types_unreachable_bit_length)
                                    ).simplified(self.__substitution(message))
                cases.append((Name(target.affixed_name), length))

            if not cases:
                return Number(0)

            cases.append((Name('others'), Name(self.types_unreachable_bit_length)))
            return Case(Name('Field'), cases).simplified(self.__substitution(message))

        def first(field: Field, message: Message) -> Expr:
            target_links = [(target, [l for l in links if l.first != UNDEFINED])
                            for target, links in itertools.groupby(message.outgoing(field),
                                                                   lambda x: x[1])]
            cases: List[Tuple[Expr, Expr]] = []
            for target, links in target_links:
                if target in message.types and links:
                    first = (links[0].first if len(links) == 1
                             else If([(l.condition, l.first) for l in links],
                                     Name(self.types_unreachable_bit_length)
                                     ).simplified(self.__substitution(message)))
                    cases.append((Name(target.affixed_name), first))

            if not cases:
                return Name('Context.Index')

            cases.append((Name('others'), Name('Context.Index')))
            return Case(Name('Field'), cases).simplified(self.__substitution(message))

        return UnitPart(
            [],
            [ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Composite_Field',
                    'Boolean',
                    [Parameter(['Field'], 'Field_Type')]),
                Case(Name('Field'),
                     [(Name(f.affixed_name), TRUE if f in composite_fields else FALSE)
                      for f in message.fields])),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Field_Condition',
                    'Boolean',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Source_Field', 'Target_Field'], 'All_Field_Type')]),
                Case(Name('Source_Field'),
                     [(Name(f.affixed_name), condition(f, message))
                      for f in message.all_fields]),
                [Precondition(
                    And(
                        Call('Valid_Target',
                             [Name('Source_Field'),
                              Name('Target_Field')]),
                        Call('Preliminary_Valid_Predecessors',
                             [Name('Context'),
                              Name('Target_Field')])))]),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Field_Length',
                    self.types_bit_length,
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                Case(Name('Context.Field'),
                     [(Name(f.affixed_name), length(f, message))
                      for f in message.all_fields]),
                [Precondition(
                    And(
                        Call('Valid_Target',
                             [Name('Context.Field'),
                              Name('Field')]),
                        Call('Valid_Predecessors',
                             [Name('Context'),
                              Name('Field')]),
                        Call('Field_Condition',
                             [Name('Context'),
                              Name('Context.Field'),
                              Name('Field')])))]),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Field_First',
                    self.types_bit_index,
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                Case(Name('Context.Field'),
                     [(Name(f.affixed_name), first(f, message))
                      for f in message.all_fields]),
                [Precondition(
                    And(
                        Call('Valid_Target',
                             [Name('Context.Field'),
                              Name('Field')]),
                        Call('Valid_Predecessors',
                             [Name('Context'),
                              Name('Field')]),
                        Call('Field_Condition',
                             [Name('Context'),
                              Name('Context.Field'),
                              Name('Field')])))]),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Field_Postcondition',
                    'Boolean',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                Case(Name('Field'),
                     [(Name(f.affixed_name),
                       Or(*([Call('Field_Condition',
                                  [Name('Context'),
                                   Name('Field'),
                                   Name(s.affixed_name)])
                             for s in message.direct_successors(f)])
                          or (FALSE,)))
                      for f in message.fields]),
                [Precondition(
                    And(
                        Call('Valid_Predecessors',
                             [Name('Context'),
                              Name('Field')]),
                        Call('Preliminary_Valid',
                             [Name('Context'),
                              Name('Field')])))])]
        )

    def __create_internal_accessor_functions(self, message: Message) -> UnitPart:
        def result(field: Field, message: Message) -> NamedAggregate:
            aggregate: List[Tuple[str, Expr]] = [('Field', Name(field.affixed_name))]
            if field in message.fields and isinstance(message.types[field], Scalar):
                aggregate.append(
                    (f'{field.name}_Value',
                     Call('Convert',
                          [Slice('Context.Buffer.all',
                                 Name('Buffer_First'),
                                 Name('Buffer_Last')),
                           Name('Offset')])))
            return NamedAggregate(*aggregate)

        return UnitPart(
            [],
            [ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Valid_Context',
                    'Boolean',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                And(Call('Valid_Target', [Name('Context.Field'), Name('Field')]),
                    Call('Valid_Predecessors', [Name('Context'), Name('Field')]),
                    NotEqual(Name('Context.Buffer'), NULL))),
             ExpressionFunctionDeclaration(
                FunctionSpecification(
                    'Sufficient_Buffer_Length',
                    'Boolean',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                And(NotEqual(Name('Context.Buffer'), NULL),
                    LessEqual(
                        Name('Context.First'),
                        Div(Last(self.types_bit_index), Number(2))),
                    LessEqual(
                        Call('Field_First', [Name('Context'), Name('Field')]),
                        Div(Last(self.types_bit_index), Number(2))),
                    GreaterEqual(
                        Call('Field_Length', [Name('Context'), Name('Field')]),
                        Number(0)),
                    LessEqual(
                        Call('Field_Length', [Name('Context'), Name('Field')]),
                        Div(Last(self.types_bit_length), Number(2))),
                    LessEqual(
                        Add(Call('Field_First', [Name('Context'), Name('Field')]),
                            Call('Field_Length', [Name('Context'), Name('Field')])),
                        Div(Last(self.types_bit_length), Number(2))),
                    LessEqual(
                        Name('Context.First'),
                        Call('Field_First', [Name('Context'), Name('Field')])),
                    GreaterEqual(
                        Name('Context.Last'),
                        Sub(Add(Call('Field_First', [Name('Context'), Name('Field')]),
                                Call('Field_Length', [Name('Context'), Name('Field')])),
                            Number(1)))),
                [Precondition(
                    And(
                        Call('Valid_Context',
                             [Name('Context'), Name('Field')]),
                        Call('Field_Condition',
                             [Name('Context'), Name('Context.Field'), Name('Field')])))]),
             SubprogramBody(
                FunctionSpecification(
                    'Get_Field_Value',
                    'Result_Type',
                    [Parameter(['Context'], 'Context_Type'),
                     Parameter(['Field'], 'Field_Type')]),
                [ObjectDeclaration(
                    'First',
                    self.types_bit_index,
                    Call('Field_First', [Name('Context'), Name('Field')]),
                    True),
                 ObjectDeclaration(
                    'Length',
                    self.types_bit_length,
                    Call('Field_Length', [Name('Context'), Name('Field')]),
                    True),
                 ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        'Buffer_First',
                        self.types_index),
                    Call(self.types_byte_index,
                         [Name('First')])),
                 ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        'Buffer_Last',
                        self.types_index),
                    Call(self.types_byte_index,
                         [Add(Name('First'), Name('Length'), Number(-1))]),
                    [Precondition(GreaterEqual(Name('Length'), Number(1)))]),
                 ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        'Offset',
                        self.types_offset),
                     Call(self.types_offset,
                          [Mod(Sub(Number(8),
                                   Mod(Add(Name('First'), Name('Length'), Number(-1)), Number(8))),
                               Number(8))]))],
                [ReturnStatement(
                    Case(Name('Field'),
                         [(Name(f.affixed_name), result(f, message)) for f in message.fields])
                )],
                [Precondition(
                    And(Call('Valid_Context',
                             [Name('Context'), Name('Field')]),
                        Call('Field_Condition',
                             [Name('Context'), Name('Context.Field'), Name('Field')]),
                        Call('Sufficient_Buffer_Length',
                             [Name('Context'), Name('Field')]))),
                 Postcondition(
                    Equal(
                        Selected(
                            Result('Get_Field_Value'),
                            'Field'),
                        Name('Field')))])]
        )

    def __create_verify_procedure(self, message: Message,
                                  context_invariant: Sequence[Expr]) -> UnitPart:
        specification = ProcedureSpecification(
            'Verify',
            [InOutParameter(['Context'], 'Context_Type'),
             Parameter(['Field'], 'Field_Type')])

        valid_invariant = [
            If([(NotEqual(Name('Field'), Name(f.affixed_name)),
                 If([(Old(Call('Valid', [Name('Context'), Name(f.affixed_name)])),
                      Call('Valid', [Name('Context'), Name(f.affixed_name)]))]))])
            for f in message.fields]

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT),
                 Postcondition(
                    And(VALID_CONTEXT,
                        *valid_invariant,
                        Equal(Call('Has_Buffer', [Name('Context')]),
                              Old(Call('Has_Buffer', [Name('Context')]))),
                        *context_invariant))])],
            [SubprogramBody(
                specification,
                [ObjectDeclaration('First', self.types_bit_index),
                 ObjectDeclaration('Last', self.types_bit_length),
                 ObjectDeclaration('Value', 'Result_Type')],
                [IfStatement(
                    [(Call('Valid_Context',
                           [Name('Context'), Name('Field')]),
                      [IfStatement(
                          [(Call('Field_Condition',
                                 [Name('Context'), Name('Context.Field'), Name('Field')]),
                            [IfStatement(
                                [(Call('Sufficient_Buffer_Length',
                                       [Name('Context'), Name('Field')]),
                                  [Assignment(
                                      'First',
                                      Call('Field_First',
                                           [Name('Context'), Name('Field')])),
                                   Assignment(
                                      'Last',
                                      Sub(Add(Name('First'),
                                              Call('Field_Length',
                                                   [Name('Context'), Name('Field')])),
                                          Number(1))),
                                   Assignment(
                                      'Value',
                                      Call('Get_Field_Value',
                                           [Name('Context'), Name('Field')])),
                                   Assignment(
                                      Indexed(
                                          'Context.Cursors',
                                          Name('Field')),
                                      NamedAggregate(
                                          ('State', Name('S_Preliminary')),
                                          ('First', Name('First')),
                                          ('Last', Name('Last')),
                                          ('Value', Name('Value')))),
                                   IfStatement(
                                       [(And(Call('Valid_Type',
                                                  [Name('Value')]),
                                             Call('Field_Postcondition',
                                                  [Name('Context'), Name('Field')])),
                                         [IfStatement(
                                             [(Call('Composite_Field',
                                                    [Name('Field')]),
                                               [Assignment(
                                                   Indexed(
                                                       'Context.Cursors',
                                                       Name('Field')),
                                                   NamedAggregate(
                                                       ('State', Name('S_Structural_Valid')),
                                                       ('First', Name('First')),
                                                       ('Last', Name('Last')),
                                                       ('Value', Name('Value'))))])],
                                             [Assignment(
                                                 Indexed(
                                                     'Context.Cursors',
                                                     Name('Field')),
                                                 NamedAggregate(
                                                     ('State', Name('S_Valid')),
                                                     ('First', Name('First')),
                                                     ('Last', Name('Last')),
                                                     ('Value', Name('Value'))))]),
                                          Assignment(
                                              'Context.Index',
                                              Add(Name('Last'), Number(1))),
                                          Assignment(
                                              'Context.Field',
                                              Name('Field'))])],
                                       [Assignment(
                                           Indexed(
                                               'Context.Cursors',
                                               Name('Field')),
                                           NamedAggregate(
                                               ('State', Name('S_Invalid'))))])])],
                                [Assignment(
                                    Indexed(
                                        'Context.Cursors',
                                        Name('Field')),
                                    NamedAggregate(
                                        ('State', Name('S_Incomplete'))))])])],
                          [Assignment(
                              Indexed(
                                  'Context.Cursors',
                                  Name('Field')),
                              NamedAggregate(
                                  ('State', Name('S_Invalid'))))])])])])]
        )

    @staticmethod
    def __create_verify_message_procedure(message: Message,
                                          context_invariant: Sequence[Expr]) -> UnitPart:
        specification = ProcedureSpecification(
            'Verify_Message',
            [InOutParameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT),
                 Postcondition(
                    And(VALID_CONTEXT,
                        Equal(Call('Has_Buffer', [Name('Context')]),
                              Old(Call('Has_Buffer', [Name('Context')]))),
                        *context_invariant))])],
            [SubprogramBody(
                specification,
                [],
                [CallStatement('Verify', [Name('Context'), Name(f.affixed_name)])
                 for f in message.fields])]
        )

    @staticmethod
    def __create_present_function() -> UnitPart:
        specification = FunctionSpecification(
            'Present',
            'Boolean',
            [Parameter(['Context'], 'Context_Type'),
             Parameter(['Field'], 'Field_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(
                specification,
                And(
                    Or(
                        *[Equal(
                            Selected(
                                Indexed('Context.Cursors', Name('Field')),
                                'State'),
                            Name(s))
                          for s in ('S_Valid', 'S_Structural_Valid')]),
                    Equal(
                        Selected(
                            Indexed('Context.Cursors', Name('Field')),
                            'Value.Field'),
                        Name('Field')),
                    Less(
                        Selected(
                            Indexed('Context.Cursors', Name('Field')),
                            'First'),
                        Add(
                            Selected(
                                Indexed('Context.Cursors', Name('Field')),
                                'Last'),
                            Number(1)))))]
        )

    @staticmethod
    def __create_structural_valid_function() -> UnitPart:
        specification = FunctionSpecification(
            'Structural_Valid',
            'Boolean',
            [Parameter(['Context'], 'Context_Type'),
             Parameter(['Field'], 'Field_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(
                specification,
                And(
                    Or(
                        *[Equal(
                            Selected(
                                Indexed('Context.Cursors', Name('Field')),
                                'State'),
                            Name(s))
                          for s in ('S_Valid', 'S_Structural_Valid')])))]
        )

    @staticmethod
    def __create_valid_function() -> UnitPart:
        specification = FunctionSpecification(
            'Valid',
            'Boolean',
            [Parameter(['Context'], 'Context_Type'),
             Parameter(['Field'], 'Field_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT),
                 Postcondition(
                    If([(Result('Valid'),
                         And(
                             Call('Present', [Name('Context'), Name('Field')]),
                             Call('Structural_Valid', [Name('Context'), Name('Field')])))]))])],
            [ExpressionFunctionDeclaration(
                specification,
                And(
                    Equal(
                        Selected(
                            Indexed('Context.Cursors', Name('Field')),
                            'State'),
                        Name('S_Valid')),
                    Equal(
                        Selected(
                            Indexed('Context.Cursors', Name('Field')),
                            'Value.Field'),
                        Name('Field')),
                    Less(
                        Selected(
                            Indexed('Context.Cursors', Name('Field')),
                            'First'),
                        Add(
                            Selected(
                                Indexed('Context.Cursors', Name('Field')),
                                'Last'),
                            Number(1)))))]
        )

    def __create_structural_valid_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            'Structural_Valid_Message',
            'Boolean',
            [Parameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(
                specification,
                valid_message_condition(message, structural=True).simplified(
                    self.__substitution(message)))]
        )

    def __create_valid_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            'Valid_Message',
            'Boolean',
            [Parameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(
                specification,
                valid_message_condition(message).simplified(
                    self.__substitution(message)))]
        )

    @staticmethod
    def __create_has_buffer_function() -> UnitPart:
        specification = FunctionSpecification(
            'Has_Buffer',
            'Boolean',
            [Parameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(
                specification,
                NotEqual(Name('Context.Buffer'), NULL))]
        )

    @staticmethod
    def __create_incomplete_function() -> UnitPart:
        specification = FunctionSpecification(
            'Incomplete',
            'Boolean',
            [Parameter(['Context'], 'Context_Type'),
             Parameter(['Field'], 'Field_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(
                specification,
                Equal(
                    Selected(
                        Indexed(
                            'Context.Cursors',
                            Name('Field')),
                        'State'),
                    Name('S_Incomplete')))]
        )

    @staticmethod
    def __create_incomplete_message_function(message: Message) -> UnitPart:
        specification = FunctionSpecification(
            'Incomplete_Message',
            'Boolean',
            [Parameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(
                specification,
                Or(*[Call('Incomplete', [Name('Context'), Name(f.affixed_name)])
                     for f in message.fields]))]
        )

    @staticmethod
    def __create_scalar_accessor_functions(scalar_fields: Mapping[Field, Scalar]) -> UnitPart:
        def specification(field: Field, field_type: Type) -> FunctionSpecification:
            return FunctionSpecification(
                f'Get_{field.name}',
                field_type.name,
                [Parameter(['Context'], 'Context_Type')])

        def result(field: Field, field_type: Type) -> Expr:
            value = Selected(
                Indexed(
                    'Context.Cursors',
                    Name(field.affixed_name)),
                f'Value.{field.name}_Value')
            if isinstance(field_type, Enumeration):
                return Call('Convert', [value])
            return value

        return UnitPart(
            [SubprogramDeclaration(
                specification(f, t),
                [Precondition(
                    And(VALID_CONTEXT,
                        Call('Valid', [Name('Context'), Name(f.affixed_name)])))])
             for f, t in scalar_fields.items()],
            [ExpressionFunctionDeclaration(
                specification(f, t),
                result(f, t))
             for f, t in scalar_fields.items()]
        )

    def __create_composite_accessor_procedures(self, composite_fields: Sequence[Field]) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f'Get_{field.name}',
                [Parameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification(f),
                [Precondition(
                    And(VALID_CONTEXT,
                        Call('Has_Buffer', [Name('Context')]),
                        Call('Present', [Name('Context'), Name(f.affixed_name)])))],
                [FormalSubprogramDeclaration(
                    ProcedureSpecification(
                        f'Process_{f.name}',
                        [Parameter([f.name], self.types_bytes)]))])
             for f in composite_fields],
            [SubprogramBody(
                specification(f),
                [ObjectDeclaration(
                    'First',
                    self.types_index,
                    Call(
                        self.types_byte_index,
                        [Selected(
                            Indexed(
                                'Context.Cursors',
                                Name(f.affixed_name)),
                            'First')]),
                    True),
                 ObjectDeclaration(
                    'Last',
                    self.types_index,
                    Call(
                        self.types_byte_index,
                        [Selected(
                            Indexed(
                                'Context.Cursors',
                                Name(f.affixed_name)),
                            'Last')]),
                    True)],
                [CallStatement(f'Process_{f.name}',
                               [Slice('Context.Buffer.all', Name('First'), Name('Last'))])])
             for f in composite_fields]
        )

    def __create_switch_procedures(self, message: Message,
                                   sequence_fields: Sequence[Field]) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                'Switch',
                [InOutParameter(['Context'], 'Context_Type'),
                 OutParameter(['Sequence_Context'],
                              f'{sequence_name(message, field)}.Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification(f),
                [Precondition(
                    And(VALID_CONTEXT,
                        Not(Constrained('Context')),
                        Not(Constrained('Sequence_Context')),
                        Call('Has_Buffer', [Name('Context')]),
                        Call('Present', [Name('Context'), Name(f.affixed_name)]))),
                 Postcondition(
                    And(VALID_CONTEXT, *switch_update_conditions(message, f)))])
             for f in sequence_fields],
            [SubprogramBody(
                specification(f),
                [ObjectDeclaration(
                    'Buffer',
                    self.types_bytes_ptr)],
                [CallStatement(
                    'Take_Buffer',
                    [Name('Context'),
                     Name('Buffer')]),
                 PragmaStatement('Warnings', ['Off', '"unused assignment to ""Buffer"""']),
                 CallStatement(
                    f'{sequence_name(message, f)}.Initialize',
                    [Name('Sequence_Context'),
                     Name('Buffer'),
                     Name('Context.Buffer_First'),
                     Name('Context.Buffer_Last'),
                     Selected(
                         Indexed(
                             'Context.Cursors',
                             Name(f.affixed_name)),
                         'First'),
                     Selected(
                         Indexed(
                             'Context.Cursors',
                             Name(f.affixed_name)),
                         'Last')]),
                 PragmaStatement('Warnings', ['On', '"unused assignment to ""Buffer"""'])])
             for f in sequence_fields]
        )

    def __create_update_procedures(self, message: Message,
                                   sequence_fields: Sequence[Field]) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                'Update',
                [InOutParameter(['Context'], 'Context_Type'),
                 InOutParameter(['Sequence_Context'],
                                f'{sequence_name(message, field)}.Context_Type')])

        def take_buffer_arguments(field: Field) -> Sequence[Expr]:
            arguments = [Name('Sequence_Context'),
                         Name('Buffer'),
                         Name('Context.Buffer_First'),
                         Name('Context.Buffer_Last')]

            field_type = message.types[field]
            assert isinstance(field_type, Array)

            if isinstance(field_type.element_type, Reference):
                arguments.extend([Name('Context.First'),
                                  Name('Context.Last')])

            return arguments

        return UnitPart(
            [SubprogramDeclaration(
                specification(f),
                [Precondition(
                    And(VALID_CONTEXT, *switch_update_conditions(message, f))),
                 Postcondition(
                    And(VALID_CONTEXT,
                        Call('Has_Buffer', [Name('Context')]),
                        Not(Call(f'{sequence_name(message, f)}.Has_Buffer',
                                 [Name('Sequence_Context')]))))])
             for f in sequence_fields],
            [SubprogramBody(
                specification(f),
                [ObjectDeclaration(
                    'Valid_Sequence',
                    'Boolean',
                    Call(f'{sequence_name(message, f)}.Valid', [Name('Sequence_Context')]),
                    True),
                 ObjectDeclaration(
                    'Buffer',
                    self.types_bytes_ptr)],
                [CallStatement(
                    f'{sequence_name(message, f)}.Take_Buffer',
                    take_buffer_arguments(f)),
                 Assignment(
                     'Context.Buffer',
                     Name('Buffer')),
                 IfStatement(
                    [(Name('Valid_Sequence'),
                      [Assignment(
                          Indexed(
                              'Context.Cursors',
                              Name(f.affixed_name)),
                          NamedAggregate(
                              ('State', Name('S_Valid')),
                              *[(a, Selected(
                                  Indexed(
                                      'Context.Cursors',
                                      Name(f.affixed_name)),
                                  a))
                                for a in ('First', 'Last', 'Value')]))])])])
             for f in sequence_fields]
        )

    def __create_valid_context_function(self, message: Message) -> UnitPart:
        parameters = [
            Parameter(['Buffer_First', 'Buffer_Last'], self.types_index),
            Parameter(['First', 'Last'], self.types_bit_index),
            Parameter(['Buffer_Address'], f'{self.types}.Integer_Address'),
            Parameter(['Buffer'], self.types_bytes_ptr),
            Parameter(['Index'], self.types_bit_index),
            Parameter(['Field'], 'All_Field_Type'),
            Parameter(['Cursors'], 'Cursors_Type')
        ]

        specification = FunctionSpecification(
            'Valid_Context',
            'Boolean',
            parameters)

        return UnitPart(
            [],
            [],
            [ExpressionFunctionDeclaration(
                specification,
                self.__context_type_predicate(message))]
        )

    @staticmethod
    def __create_public_valid_context_function() -> UnitPart:
        specification = FunctionSpecification(
            'Valid_Context',
            'Boolean',
            [Parameter(['Context'], 'Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification)],
            [],
            [ExpressionFunctionDeclaration(  # WORKAROUND: Componolit/Workarounds#1
                specification,
                Call('Valid_Context',
                     [Name('Context.Buffer_First'),
                      Name('Context.Buffer_Last'),
                      Name('Context.First'),
                      Name('Context.Last'),
                      Name('Context.Buffer_Address'),
                      Name('Context.Buffer'),
                      Name('Context.Index'),
                      Name('Context.Field'),
                      Name('Context.Cursors')]))]
        )

    def __create_message_unit(self, message: Message) -> None:
        if isinstance(message, DerivedMessage):
            generic_name = f'{self.prefix}{message.generic_base_name}'
        else:
            generic_name = f'{self.prefix}{message.generic_name}'

        arrays = [message.types[f].name for f in message.fields
                  if isinstance(message.types[f], Array)]

        context: List[ContextItem] = [Pragma('SPARK_Mode'), WithClause(generic_name)]
        context.extend(
            WithClause(f'{self.prefix}{message.package}.{array}') for array in arrays)
        instantiation = GenericPackageInstantiation(f'{self.prefix}{message.full_name}',
                                                    generic_name,
                                                    arrays)
        self.units[message.full_name] = InstantiationUnit(context, instantiation)

    def __create_type(self, field_type: Type, message_package: str) -> None:
        unit = self.units[message_package]

        if isinstance(field_type, ModularInteger):
            unit += UnitPart(modular_types(field_type))
            unit += UnitPart(type_dependent_unreachable_function(field_type))
            unit += self.__modular_functions(field_type)
        elif isinstance(field_type, RangeInteger):
            unit += UnitPart(range_types(field_type))
            unit += UnitPart(type_dependent_unreachable_function(field_type))
            unit += self.__range_functions(field_type)
        elif isinstance(field_type, Enumeration):
            unit += UnitPart(enumeration_types(field_type))
            unit += UnitPart(type_dependent_unreachable_function(field_type))
            unit += self.__enumeration_functions(field_type)
        elif isinstance(field_type, Array):
            if not isinstance(field_type.element_type, Reference):
                self.__create_type(field_type.element_type, message_package)
            self.__create_array_unit(field_type, message_package)
        elif isinstance(field_type, Payload):
            pass
        else:
            raise NotImplementedError(f'unsupported type "{type(field_type).__name__}"')

    def __create_subtype(self, field_type: Type, message_package: str, base_package: str) -> None:
        unit = self.units[message_package]

        if isinstance(field_type, (ModularInteger, RangeInteger, Enumeration)):
            types: List[TypeDeclaration]
            subprograms: Sequence[Subprogram]
            if isinstance(field_type, ModularInteger):
                types = modular_types(field_type)
                subprograms = [s for s in self.__modular_functions(field_type).specification
                               if isinstance(s, Subprogram)]
            elif isinstance(field_type, RangeInteger):
                types = range_types(field_type)
                subprograms = self.__range_functions(field_type).specification
            elif isinstance(field_type, Enumeration):
                types = enumeration_types(field_type)
                subprograms = [s for s in self.__enumeration_functions(field_type).specification
                               if isinstance(s, Subprogram)]

            unit += UnitPart(
                [Subtype(t.name, f'{base_package}.{t.name}') for t in types])
            unit += UnitPart(
                [SubprogramRenamingDeclaration(
                    renamed_subprogram_specification(s.specification, s.name),
                    f'{base_package}.{s.name}')
                 for s in subprograms])

            if isinstance(field_type, Enumeration):
                type_name = field_type.enum_name if field_type.always_valid else field_type.name
                unit += UnitPart(
                    [ObjectDeclaration(literal, type_name, Name(f'{base_package}.{literal}'), True)
                     for literal in field_type.literals])

        elif isinstance(field_type, Array):
            if not isinstance(field_type.element_type, Reference):
                self.__create_subtype(field_type.element_type, message_package, base_package)
            self.__create_array_unit(field_type, message_package)

        elif isinstance(field_type, Payload):
            pass

        else:
            raise NotImplementedError(f'unsupported type "{type(field_type).__name__}"')

    def __create_array_unit(self, array_type: Array, package_name: str) -> None:
        element_type = array_type.element_type

        array_context: List[ContextItem] = []
        array_package: GenericPackageInstantiation
        if isinstance(element_type, Reference):
            array_context = [Pragma('SPARK_Mode'),
                             WithClause(f'{self.prefix}Message_Sequence'),
                             WithClause(f'{self.prefix}{package_name}.'
                                        f'{element_type.name}')]
            array_package = GenericPackageInstantiation(
                f'{self.prefix}{package_name}.{array_type.name}',
                'Message_Sequence',
                [f'{element_type.name}.Context_Type',
                 f'{element_type.name}.Initialize',
                 f'{element_type.name}.Take_Buffer',
                 f'{element_type.name}.Has_Buffer',
                 f'{element_type.name}.Index',
                 f'{element_type.name}.Structural_Valid_Message',
                 f'{element_type.name}.Valid_Context'])
        else:
            array_context = [Pragma('SPARK_Mode'),
                             WithClause(f'{self.prefix}Scalar_Sequence'),
                             WithClause(f'{self.prefix}{package_name}')]
            array_package = GenericPackageInstantiation(
                f'{self.prefix}{package_name}.{array_type.name}',
                f'Scalar_Sequence',
                [element_type.name,
                 element_type.base_name if not isinstance(element_type, ModularInteger)
                 else element_type.name,
                 'Convert',
                 'Valid',
                 'Convert'])

        self.units[array_package.name] = InstantiationUnit(array_context, array_package)

    def __substitution(self, message: Message, prefix: bool = True) -> Mapping[Name, Expr]:
        def prefixed(name: str) -> Expr:
            return Selected(Name('Context'), name) if prefix else Name(name)

        first = prefixed('First')
        last = prefixed('Last')
        cursors = prefixed('Cursors')

        return {
            **{First('Message'):
               first},
            **{Last('Message'):
               last},
            **{First(f.name):
               Selected(
                   Indexed(
                       cursors,
                       Name(f.affixed_name)),
                   'First')
               for f in message.fields},
            **{Last(f.name):
               Selected(
                   Indexed(
                       cursors,
                       Name(f.affixed_name)),
                   'Last')
               for f in message.fields},
            **{Length(f.name):
               Add(
                   Sub(
                       Selected(
                           Indexed(
                               cursors,
                               Name(f.affixed_name)),
                           'Last'),
                       Selected(
                           Indexed(
                               cursors,
                               Name(f.affixed_name)),
                           'First')),
                   Number(1))
               for f in message.fields},
            **{Variable(f.name):
               Call(
                   self.types_bit_length,
                   [Selected(
                       Indexed(
                           cursors,
                           Name(f.affixed_name)),
                       f'Value.{f.name}_Value')])
               for f, t in message.types.items() if not isinstance(t, Enumeration)},
            **{Variable(f.name):
               Selected(
                   Indexed(
                       cursors,
                       Name(f.affixed_name)),
                   f'Value.{f.name}_Value')
               for f, t in message.types.items() if isinstance(t, Enumeration)},
            **{Variable(l):
               Call('Convert', [Name(l)])
               for l in itertools.chain.from_iterable(
                   t.literals.keys() for t in message.types.values()
                   if isinstance(t, Enumeration))}
        }

    def __common_context(self) -> List[ContextItem]:
        return [WithClause(self.types),
                UseTypeClause(self.types_bytes,
                              self.types_bytes_ptr,
                              self.types_index,
                              self.types_length,
                              self.types_bit_index,
                              self.types_bit_length)]

    def __range_functions(self, integer: RangeInteger) -> SubprogramUnitPart:
        specification: List[Subprogram] = []

        for range_type in range_types(integer):
            if isinstance(range_type, RangeSubtype):
                continue

            specification.append(
                GenericFunctionInstantiation(
                    'Convert',
                    FunctionSpecification(f'{self.types}.Convert_To_Int',
                                          range_type.name,
                                          [Parameter(['Buffer'], self.types_bytes),
                                           Parameter(['Offset'], self.types_offset)]),
                    [range_type.name]))

        specification.append(
            type_validation_function(
                integer,
                integer.constraints.simplified({Name(integer.name): Name('Value')})))
        specification.append(integer_conversion_function(integer.name, integer.base_name))

        return SubprogramUnitPart(specification)

    def __modular_functions(self, integer: ModularInteger) -> UnitPart:
        specification: List[Declaration] = []

        for modular_type in modular_types(integer):
            specification.append(
                GenericFunctionInstantiation(
                    'Convert',
                    FunctionSpecification(f'{self.types}.Convert_To_Mod',
                                          modular_type.name,
                                          [Parameter(['Buffer'], self.types_bytes),
                                           Parameter(['Offset'], self.types_offset)]),
                    [modular_type.name]))

        specification.append(Pragma('Warnings', ['Off', '"unused variable ""Value"""']))
        specification.append(
            type_validation_function(
                integer,
                integer.constraints.simplified({Name(integer.name): Name('Value')})))
        specification.append(Pragma('Warnings', ['On', '"unused variable ""Value"""']))
        specification.append(integer_conversion_function(integer.name, integer.name))

        return UnitPart(specification)

    def __enumeration_functions(self, enum: Enumeration) -> UnitPart:
        specification: List[Declaration] = []

        specification.append(
            GenericFunctionInstantiation(
                'Convert',
                FunctionSpecification(
                    f'{self.types}.Convert_To_Mod',
                    enum.base_name,
                    [Parameter(['Buffer'], self.types_bytes),
                     Parameter(['Offset'], self.types_offset)]),
                [enum.base_name]))

        enum_value = Name('Value')

        validation_expression: Expr
        if enum.always_valid:
            validation_expression = Name('True')
        else:
            validation_cases: List[Tuple[Expr, Expr]] = []
            validation_cases.extend(
                (value, Name('True')) for value in enum.literals.values())
            validation_cases.append(
                (Name('others'), Name('False')))

            validation_expression = Case(enum_value, validation_cases)

        if enum.always_valid:
            specification.append(
                Pragma('Warnings', ['Off', '"unused variable ""Value"""']))
        specification.append(
            type_validation_function(enum, validation_expression))
        if enum.always_valid:
            specification.append(
                Pragma('Warnings', ['On', '"unused variable ""Value"""']))

        conversion_function = FunctionSpecification(
            'Convert',
            enum.name,
            [Parameter(['Value'], enum.base_name)])
        precondition = Precondition(Call('Valid', [Name('Value')]))
        conversion_cases: List[Tuple[Expr, Expr]] = []

        if enum.always_valid:
            conversion_cases.extend(
                (value, Aggregate(Name('True'), Name(key)))
                for key, value in enum.literals.items())
            conversion_cases.append(
                (Name('others'),
                 Aggregate(Name('False'), Name('Value'))))

            specification.append(
                ExpressionFunctionDeclaration(
                    conversion_function,
                    Case(Name('Value'), conversion_cases),
                    [precondition]))

        else:
            conversion_cases.extend(
                (value, Name(key)) for key, value in enum.literals.items())
            conversion_cases.append(
                (Name('others'),
                 Call(unreachable_function_name(enum.name))))

            specification.append(
                ExpressionFunctionDeclaration(
                    conversion_function,
                    Case(enum_value, conversion_cases),
                    [precondition]))

        specification.append(ExpressionFunctionDeclaration(
            FunctionSpecification(
                'Convert',
                enum.base_name,
                [Parameter(['Enum'], enum.enum_name if enum.always_valid else enum.name)]),
            Case(
                Name('Enum'),
                [(Name(key), value) for key, value in enum.literals.items()])))

        return UnitPart(specification)

    @staticmethod
    def __create_contains_function(refinement: Refinement,
                                   condition_fields: Mapping[Field, Type],
                                   null_sdu: bool) -> SubprogramUnitPart:
        condition = refinement.condition
        for f, t in condition_fields.items():
            if isinstance(t, Enumeration) and t.always_valid:
                condition = And(
                    Selected(
                        Call(f'{refinement.pdu}.Get_{f.name}', [Name('Context')]),
                        'Known'),
                    condition)
        condition = condition.simplified(
            {Variable(f.name):
             Selected(
                Call(f'{refinement.pdu}.Get_{f.name}', [Name('Context')]),
                'Enum')
             if isinstance(t, Enumeration) and t.always_valid
             else Call(f'{refinement.pdu}.Get_{f.name}', [Name('Context')])
             for f, t in condition_fields.items()})

        specification = FunctionSpecification(
            contains_function_name(refinement),
            'Boolean',
            [Parameter(['Context'], f'{refinement.pdu}.Context_Type')])

        return SubprogramUnitPart(
            [ExpressionFunctionDeclaration(
                specification,
                And(
                    *refinement_conditions(refinement, 'Context', condition_fields, null_sdu),
                    condition).simplified())])

    def __create_switch_procedure(self, refinement: Refinement,
                                  condition_fields: Mapping[Field, Type]) -> UnitPart:
        pdu_context = f'{refinement.pdu}_Context'.replace('.', '_')
        sdu_context = f'{refinement.sdu}_Context'.replace('.', '_')

        specification = ProcedureSpecification(
            'Switch',
            [InOutParameter([pdu_context], f'{refinement.pdu}.Context_Type'),
             OutParameter([sdu_context], f'{refinement.sdu}.Context_Type')])

        return UnitPart(
            [SubprogramDeclaration(
                specification,
                [Precondition(
                    And(Not(Constrained(pdu_context)),
                        Not(Constrained(sdu_context)),
                        *refinement_conditions(refinement, pdu_context, condition_fields, False),
                        Call(
                            contains_function_name(refinement),
                            [Name(pdu_context)])))])],
            [SubprogramBody(
                specification,
                [ObjectDeclaration(
                    'First',
                    self.types_bit_index),
                 ObjectDeclaration(
                    'Last',
                    self.types_bit_index),
                 ObjectDeclaration(
                    'Buffer',
                    self.types_bytes_ptr)],
                [CallStatement(
                    f'{refinement.pdu}.Field_Range',
                    [Name(pdu_context),
                     Name(f'{refinement.pdu}.{refinement.field.affixed_name}'),
                     Name('First'),
                     Name('Last')]),
                 CallStatement(
                    f'{refinement.pdu}.Take_Buffer',
                    [Name(pdu_context),
                     Name('Buffer')]),
                 PragmaStatement('Warnings', ['Off', '"unused assignment to ""Buffer"""']),
                 CallStatement(
                    f'{refinement.sdu}.Initialize',
                    [Name(sdu_context),
                     Name('Buffer'),
                     Name('First'),
                     Name('Last')]),
                 PragmaStatement('Warnings', ['On', '"unused assignment to ""Buffer"""'])])]
        )

    @property
    def types(self) -> str:
        return f'{self.prefix}Types'

    @property
    def types_bytes(self) -> str:
        return f'{self.types}.Bytes'

    @property
    def types_bytes_ptr(self) -> str:
        return f'{self.types}.Bytes_Ptr'

    @property
    def types_index(self) -> str:
        return f'{self.types}.Index_Type'

    @property
    def types_length(self) -> str:
        return f'{self.types}.Length_Type'

    @property
    def types_bit_index(self) -> str:
        return f'{self.types}.Bit_Index_Type'

    @property
    def types_bit_length(self) -> str:
        return f'{self.types}.Bit_Length_Type'

    @property
    def types_byte_index(self) -> str:
        return f'{self.types}.Byte_Index'

    @property
    def types_first_bit_index(self) -> str:
        return f'{self.types}.First_Bit_Index'

    @property
    def types_last_bit_index(self) -> str:
        return f'{self.types}.Last_Bit_Index'

    @property
    def types_offset(self) -> str:
        return f'{self.types}.Offset_Type'

    @property
    def types_unreachable_bit_length(self) -> str:
        return f'{self.types}.Unreachable_Bit_Length_Type'


def modular_types(integer: ModularInteger) -> List[TypeDeclaration]:
    return [ModularType(integer.name,
                        integer.modulus)]


def range_types(integer: RangeInteger) -> List[TypeDeclaration]:
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
            RecordType(enum.name, [],
                       [Discriminant(['Known'], 'Boolean', FALSE)],
                       VariantPart('Known',
                                   [Variant([TRUE], [Component('Enum', enum.enum_name)]),
                                    Variant([FALSE], [Component('Raw', enum.base_name)])])))

    return types


def integer_conversion_function(type_name: str, type_base_name: str) -> Subprogram:
    return ExpressionFunctionDeclaration(
        FunctionSpecification('Convert',
                              type_name,
                              [Parameter(['Value'], type_base_name)]),
        Name('Value'),
        [Precondition(Call(f'Valid', [Name('Value')]))])


def type_validation_function(scalar_type: Scalar, validation_expression: Expr) -> Subprogram:
    return ExpressionFunctionDeclaration(
        FunctionSpecification(f'Valid',
                              'Boolean',
                              [Parameter(['Value'], base_type_name(scalar_type))]),
        validation_expression)


def renamed_subprogram_specification(specification: SubprogramSpecification,
                                     name: str) -> SubprogramSpecification:
    if isinstance(specification, ProcedureSpecification):
        return ProcedureSpecification(name, specification.parameters)
    if isinstance(specification, FunctionSpecification):
        return FunctionSpecification(name, specification.return_type, specification.parameters)
    raise NotImplementedError('unhandled subprogram specification "{type(specification).__name__}"')


def unreachable_function(type_name: str, base_name: str = None) -> List[Declaration]:
    return [Pragma('Warnings', ['Off', '"precondition is statically false"']),
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    unreachable_function_name(type_name),
                    type_name),
                First(type_name) if not base_name else Aggregate(Name('False'), First(base_name)),
                [Precondition(FALSE)]),
            Pragma('Warnings', ['On', '"precondition is statically false"'])]


def type_dependent_unreachable_function(scalar_type: Scalar) -> List[Declaration]:
    base_name = None
    if isinstance(scalar_type, Enumeration) and scalar_type.always_valid:
        base_name = scalar_type.base_name
    return unreachable_function(scalar_type.name, base_name)


def contains_function_name(refinement: Refinement) -> str:
    sdu_name = (refinement.sdu.rsplit('.', 1)[1]
                if refinement.sdu.startswith(refinement.package)
                else refinement.sdu)
    pdu_name = (refinement.pdu.rsplit('.', 1)[1]
                if refinement.pdu.startswith(refinement.package)
                else refinement.pdu)
    return f'{sdu_name}_In_{pdu_name}_{refinement.field.name}'.replace('.', '_')


def unreachable_function_name(type_name: str) -> str:
    return f'Unreachable_{type_name}'.replace('.', '_')


def sequence_name(message: Message, field: Field) -> str:
    return f'{message.types[field].name}_Sequence'


def base_type_name(scalar_type: Scalar) -> str:
    if isinstance(scalar_type, (RangeInteger, Enumeration)):
        return scalar_type.base_name
    if isinstance(scalar_type, ModularInteger):
        return scalar_type.name
    assert False, 'unexpected scalar type "{type(scalar_type)}"'
    return None


def is_seen_type(type_name: str, seen_types: Set[str]) -> bool:
    seen = type_name in seen_types
    seen_types.add(type_name)
    return seen


def switch_update_conditions(message: Message, field: Field) -> List[Expr]:
    return [
        Not(Call('Has_Buffer', [Name('Context')])),
        Call('Present', [Name('Context'), Name(field.affixed_name)]),
        Call(f'{sequence_name(message, field)}.Has_Buffer',
             [Name('Sequence_Context')]),
        Equal(Name('Context.Buffer_First'),
              Name('Sequence_Context.Buffer_First')),
        Equal(Name('Context.Buffer_Last'),
              Name('Sequence_Context.Buffer_Last')),
        Equal(Selected(Name('Context'), 'Buffer_Address'),
              Selected(Name('Sequence_Context'), 'Buffer_Address'))
    ]


def valid_message_condition(message: Message, field: Field = INITIAL,
                            structural: bool = False) -> Expr:
    if not message.outgoing(field):
        return TRUE
    return Or(
        *[l.condition
          if l.target == FINAL else
          And(
              Call(
                  'Structural_Valid'
                  if structural and isinstance(message.types[l.target], Composite) else
                  'Valid',
                  [Name('Context'), Name(l.target.affixed_name)]),
              l.condition,
              valid_message_condition(message, l.target, structural))
          for l in message.outgoing(field)])


def refinement_conditions(refinement: Refinement,
                          pdu_context: str,
                          condition_fields: Mapping[Field, Type],
                          null_sdu: bool) -> Sequence[Expr]:
    conditions: List[Expr] = [
        Call(f'{refinement.pdu}.Has_Buffer',
             [Name(pdu_context)]),
    ]

    if null_sdu:
        conditions.extend([
            Call(f'{refinement.pdu}.Structural_Valid',
                 [Name(pdu_context),
                  Name(f'{refinement.pdu}.{refinement.field.affixed_name}')]),
            Not(Call(f'{refinement.pdu}.Present',
                     [Name(pdu_context),
                      Name(f'{refinement.pdu}.{refinement.field.affixed_name}')]))

        ])
    else:
        conditions.append(
            Call(f'{refinement.pdu}.Present',
                 [Name(pdu_context),
                  Name(f'{refinement.pdu}.{refinement.field.affixed_name}')])
        )

    conditions.extend([
        Call(f'{refinement.pdu}.Valid',
             [Name(pdu_context),
              Name(f'{refinement.pdu}.{f.affixed_name}')])
        for f in condition_fields
    ])

    return conditions
