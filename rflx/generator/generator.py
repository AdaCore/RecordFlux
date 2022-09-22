# pylint: disable=too-many-lines

from __future__ import annotations

import logging
import typing as ty
from concurrent.futures import ProcessPoolExecutor
from datetime import date
from pathlib import Path

import pkg_resources

from rflx import __version__, expression as expr
from rflx.ada import (
    FALSE,
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
    Annotate,
    Aspect,
    Call,
    CallStatement,
    Case,
    ChoiceList,
    Component,
    Constrained,
    ContextItem,
    Declaration,
    Discriminant,
    EnumerationType,
    Equal,
    Expr,
    ExpressionFunctionDeclaration,
    First,
    FormalDeclaration,
    FunctionSpecification,
    GenericPackageInstantiation,
    GreaterEqual,
    If,
    In,
    InOutParameter,
    InstantiationUnit,
    Last,
    Less,
    ModularType,
    Not,
    NotIn,
    Number,
    ObjectDeclaration,
    Old,
    OutParameter,
    PackageBody,
    PackageDeclaration,
    PackageUnit,
    Parameter,
    Postcondition,
    Pow,
    Pragma,
    PragmaStatement,
    Precondition,
    ProcedureSpecification,
    RangeType,
    RecordType,
    Selected,
    SizeAspect,
    Slice,
    SparkMode,
    String,
    Subprogram,
    SubprogramBody,
    SubprogramDeclaration,
    SubprogramUnitPart,
    Unit,
    UnitPart,
    UsePackageClause,
    UseTypeClause,
    Variable,
    Variant,
    VariantPart,
    WithClause,
)
from rflx.common import file_name
from rflx.const import BUILTINS_PACKAGE, INTERNAL_PACKAGE, MAX_SCALAR_SIZE
from rflx.error import Subsystem, fail, warn
from rflx.identifier import ID, StrID
from rflx.integration import Integration
from rflx.model import (
    BOOLEAN,
    Composite,
    DerivedMessage,
    Enumeration,
    Field,
    Integer,
    Message,
    Model,
    ModularInteger,
    Opaque,
    RangeInteger,
    Refinement,
    Scalar,
    Sequence,
    Session,
    Type,
)

from . import common, const, message as message_generator
from .allocator import AllocatorGenerator
from .parser import ParserGenerator
from .serializer import SerializerGenerator
from .session import SessionGenerator

log = logging.getLogger(__name__)


class Generator:
    def __init__(
        self,
        prefix: str = "",
        workers: int = 1,
        reproducible: bool = False,
        debug: common.Debug = common.Debug.NONE,
        ignore_unsupported_checksum: bool = False,
    ) -> None:
        self._prefix = str(ID(prefix)) if prefix else ""
        self._reproducible = reproducible
        self._debug = debug
        self._ignore_unsupported_checksum = ignore_unsupported_checksum
        self._executor = ProcessPoolExecutor(max_workers=workers)
        self._template_dir = Path(pkg_resources.resource_filename(*const.TEMPLATE_DIR))
        assert self._template_dir.is_dir(), "template directory not found"

    def generate(
        self,
        model: Model,
        integration: Integration,
        directory: Path,
        library_files: bool = True,
        top_level_package: bool = True,
    ) -> None:
        units = self._generate(model, integration)
        self._write_files(units, directory, library_files, top_level_package)

    def _write_files(
        self,
        units: dict[ID, Unit],
        directory: Path,
        library_files: bool = True,
        top_level_package: bool = True,
    ) -> None:
        self._write_units(units, directory)
        if library_files:
            self._write_library_files(directory)
        if top_level_package:
            self._write_top_level_package(directory)

    def _write_library_files(self, directory: Path) -> None:
        for template_filename in const.LIBRARY_FILES:
            self._check_template_file(template_filename)

            prefix = f"{self._prefix}." if self._prefix else ""
            filename = Path(f"{file_name(prefix)}{template_filename}")

            template_file = (self._template_dir / template_filename).read_text()
            create_file(
                directory / filename,
                self._license_header()
                + "\n".join(
                    [
                        l.format(prefix=prefix)
                        for l in template_file.split("\n")
                        if "/Workarounds/" not in l
                    ]
                ),
            )

        if self._debug == common.Debug.EXTERNAL:
            debug_package_id = self._prefix * ID("RFLX_Debug")
            create_file(
                directory / f"{file_name(str(debug_package_id))}.ads",
                self._license_header()
                + PackageUnit(
                    [],
                    PackageDeclaration(
                        debug_package_id,
                        [
                            SubprogramDeclaration(
                                ProcedureSpecification(
                                    "Print",
                                    [
                                        Parameter(["Message"], "String"),
                                    ],
                                )
                            )
                        ],
                        aspects=[
                            SparkMode(),
                        ],
                    ),
                    [],
                    PackageBody(debug_package_id),
                ).ads,
            )

    def _write_top_level_package(self, directory: Path) -> None:
        if self._prefix:
            create_file(
                Path(directory) / Path(file_name(self._prefix) + ".ads"),
                self._license_header() + f"package {self._prefix} is\n\nend {self._prefix};",
            )

    def _write_units(self, units: dict[ID, Unit], directory: Path) -> None:
        for unit in units.values():
            create_file(directory / Path(unit.name + ".ads"), self._license_header() + unit.ads)

            if unit.adb:
                create_file(directory / Path(unit.name + ".adb"), self._license_header() + unit.adb)

    def _generate(self, model: Model, integration: Integration) -> dict[ID, Unit]:
        units: dict[ID, Unit] = {}

        for t in model.types:
            if t.package in [BUILTINS_PACKAGE, INTERNAL_PACKAGE]:
                continue

            log.info("Generating %s", t.identifier)

            if t.package not in units:
                unit = self._create_unit(t.package, terminating=False)
                units[t.package] = unit

            if isinstance(t, (Scalar, Composite)):
                units.update(self._create_type(t, t.package, units))

            elif isinstance(t, Message):
                # https://github.com/Componolit/RecordFlux/issues/276
                for c in t.checksums:
                    if not self._ignore_unsupported_checksum:
                        fail(
                            "unsupported checksum (consider --ignore-unsupported-checksum option)",
                            Subsystem.GENERATOR,
                            location=c.location,
                        )
                    else:
                        warn(
                            "unsupported checksum ignored", Subsystem.GENERATOR, location=c.location
                        )

                units.update(self._create_message(t))

            elif isinstance(t, Refinement):
                units.update(self._create_refinement(t, units))

            else:
                assert False, f'unexpected type "{type(t).__name__}"'

        for s in model.sessions:
            log.info("Generating %s", s.identifier)

            if s.package not in units:
                unit = self._create_unit(s.package, terminating=False)
                units[s.package] = unit

            units.update(self._create_session(s, integration))

        return units

    def _create_session(self, session: Session, integration: Integration) -> dict[ID, Unit]:
        units: dict[ID, Unit] = {}
        allocator_generator = AllocatorGenerator(session, integration, self._prefix)

        if allocator_generator.required:
            unit = self._create_unit(
                allocator_generator.unit_identifier,
                allocator_generator.declaration_context,
                allocator_generator.body_context,
            )
            unit += allocator_generator.unit_part
            units[allocator_generator.unit_identifier] = unit

        session_generator = SessionGenerator(
            session, allocator_generator, self._prefix, debug=self._debug
        )
        unit = self._create_unit(
            session_generator.unit_identifier,
            session_generator.declaration_context,
            session_generator.body_context,
            configuration_pragmas=[Pragma("Restrictions", [Variable("No_Streams")])],
            terminating=False,
        )
        unit += session_generator.unit_part
        units[session_generator.unit_identifier] = unit

        return units

    def _create_unit(  # pylint: disable = too-many-arguments
        self,
        identifier: ID,
        declaration_context: ty.Sequence[ContextItem] = None,
        body_context: ty.Sequence[ContextItem] = None,
        formal_parameters: ty.List[FormalDeclaration] = None,
        configuration_pragmas: ty.Sequence[Pragma] = None,
        aspects: ty.Sequence[Aspect] = None,
        terminating: bool = True,
    ) -> PackageUnit:
        declaration_context = declaration_context if declaration_context else []
        body_context = body_context if body_context else []
        aspects = aspects if aspects else []
        configuration_pragmas = configuration_pragmas if configuration_pragmas else []

        unit = PackageUnit(
            [*configuration_pragmas, *const.CONFIGURATION_PRAGMAS, *declaration_context],
            PackageDeclaration(
                self._prefix * identifier,
                formal_parameters=formal_parameters,
                aspects=[
                    SparkMode(),
                    *([Annotate("GNATprove", "Always_Return")] if terminating else []),
                    *aspects,
                ],
            ),
            [*configuration_pragmas, *const.CONFIGURATION_PRAGMAS, *body_context],
            PackageBody(self._prefix * identifier, aspects=[SparkMode()]),
        )

        return unit

    @staticmethod
    def _create_instantiation_unit(
        context: ty.List[ContextItem],
        instantiation: GenericPackageInstantiation,
    ) -> InstantiationUnit:
        for p in reversed(const.CONFIGURATION_PRAGMAS):
            context.insert(0, p)

        unit = InstantiationUnit(context, instantiation)

        return unit

    # pylint: disable = too-many-branches, too-many-locals
    def _create_message(self, message: Message) -> dict[ID, Unit]:
        units: dict[ID, Unit] = {}

        if not message.fields:
            return units

        context: ty.List[ContextItem] = []

        if any(t.package == BUILTINS_PACKAGE for t in message.types.values()):
            context.extend(
                [
                    WithClause(self._prefix * const.TYPES_PACKAGE),
                    WithClause(self._prefix * const.BUILTIN_TYPES_PACKAGE),
                    WithClause(self._prefix * const.BUILTIN_TYPES_CONVERSIONS_PACKAGE),
                    UsePackageClause(self._prefix * const.BUILTIN_TYPES_CONVERSIONS_PACKAGE),
                ]
            )

        context.append(WithClause(self._prefix * const.TYPES_PACKAGE))

        for field_type in message.types.values():
            if field_type.package in [BUILTINS_PACKAGE, INTERNAL_PACKAGE]:
                continue

            if isinstance(field_type, Scalar) and field_type.package != message.package:
                context.extend(
                    [
                        WithClause(self._prefix * field_type.package),
                        UsePackageClause(self._prefix * field_type.package),
                    ]
                )

            elif isinstance(field_type, Sequence):
                context.append(WithClause(self._prefix * field_type.identifier))

        unit = self._create_unit(message.identifier, context)
        units[message.identifier] = unit

        scalar_fields = {}
        composite_fields = []
        sequence_fields = {}
        opaque_fields = []
        fields_with_explicit_size = []
        fields_with_implicit_size = []

        for f, t in message.field_types.items():
            if isinstance(t, Scalar):
                scalar_fields[f] = t
            if isinstance(t, Composite):
                composite_fields.append(f)
                if isinstance(t, Sequence):
                    sequence_fields[f] = t
                if isinstance(t, Opaque):
                    opaque_fields.append(f)
                if any(l.has_implicit_size for l in message.incoming(f)):
                    fields_with_implicit_size.append(f)
                else:
                    fields_with_explicit_size.append(f)

        parser_generator = ParserGenerator(self._prefix)
        serializer_generator = SerializerGenerator(self._prefix)

        futures = [
            self._executor.submit(
                message_generator.create_use_type_clause,
                composite_fields,
                serializer_generator.requires_set_procedure(message),
            ),
            self._executor.submit(message_generator.create_allow_unevaluated_use_of_old),
            self._executor.submit(message_generator.create_field_type, message),
            self._executor.submit(message_generator.create_state_type),
            self._executor.submit(message_generator.create_cursor_type),
            self._executor.submit(message_generator.create_cursor_validation_functions),
            self._executor.submit(
                message_generator.create_valid_context_function,
                message,
                composite_fields,
                self._prefix,
            ),
            self._executor.submit(message_generator.create_context_type, message),
            self._executor.submit(message_generator.create_initialize_procedure, message),
            self._executor.submit(
                message_generator.create_restricted_initialize_procedure, message
            ),
            self._executor.submit(
                message_generator.create_initialized_function, self._prefix, message
            ),
            self._executor.submit(message_generator.create_reset_procedure, self._prefix, message),
            self._executor.submit(
                message_generator.create_restricted_reset_procedure, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_take_buffer_procedure, self._prefix, message
            ),
            self._executor.submit(message_generator.create_copy_procedure, self._prefix, message),
            self._executor.submit(message_generator.create_read_function, self._prefix, message),
            self._executor.submit(
                message_generator.create_generic_read_procedure, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_generic_write_procedure, self._prefix, message
            ),
            self._executor.submit(message_generator.create_has_buffer_function),
            self._executor.submit(
                message_generator.create_buffer_length_function, self._prefix, message
            ),
            self._executor.submit(message_generator.create_size_function),
            self._executor.submit(message_generator.create_byte_size_function),
            self._executor.submit(
                message_generator.create_message_last_function, self._prefix, message
            ),
            self._executor.submit(message_generator.create_written_last_function),
            self._executor.submit(message_generator.create_data_procedure, self._prefix, message),
            self._executor.submit(
                message_generator.create_valid_value_function, self._prefix, message, scalar_fields
            ),
            self._executor.submit(
                message_generator.create_path_condition_function, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_field_condition_function, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_field_size_function,
                self._prefix,
                message,
                scalar_fields,
                composite_fields,
            ),
            self._executor.submit(
                message_generator.create_field_first_function, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_field_last_function,
                self._prefix,
                message,
                scalar_fields,
                composite_fields,
            ),
            self._executor.submit(message_generator.create_predecessor_function),
            self._executor.submit(
                message_generator.create_successor_function, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_valid_predecessor_function,
                message,
                composite_fields,
            ),
            self._executor.submit(message_generator.create_invalid_successor_function, message),
            self._executor.submit(message_generator.create_valid_next_function),
            self._executor.submit(
                message_generator.create_available_space_function, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_sufficient_space_function, self._prefix, message
            ),
            self._executor.submit(
                message_generator.create_sufficient_buffer_length_function, self._prefix, message
            ),
            *(
                [
                    self._executor.submit(
                        message_generator.create_equal_function,
                        self._prefix,
                        message,
                        scalar_fields,
                        composite_fields,
                    )
                ]
                if composite_fields
                else []
            ),
            self._executor.submit(
                message_generator.create_reset_dependent_fields_procedure, self._prefix, message
            ),
            *(
                [
                    self._executor.submit(
                        message_generator.create_composite_field_function,
                        scalar_fields,
                        composite_fields,
                    )
                ]
                if self._requires_composite_field_function(
                    message, scalar_fields, composite_fields, sequence_fields
                )
                else []
            ),
            self._executor.submit(
                parser_generator.create_get_function, message, scalar_fields, composite_fields
            ),
            self._executor.submit(
                parser_generator.create_verify_procedure, message, scalar_fields, composite_fields
            ),
            self._executor.submit(parser_generator.create_verify_message_procedure, message),
            self._executor.submit(parser_generator.create_present_function),
            self._executor.submit(parser_generator.create_structural_valid_function),
            self._executor.submit(parser_generator.create_valid_function),
            self._executor.submit(parser_generator.create_incomplete_function),
            self._executor.submit(parser_generator.create_invalid_function),
            self._executor.submit(
                parser_generator.create_structural_valid_message_function, message
            ),
            self._executor.submit(parser_generator.create_valid_message_function, message),
            self._executor.submit(parser_generator.create_incomplete_message_function),
            self._executor.submit(
                parser_generator.create_scalar_getter_functions, message, scalar_fields
            ),
            self._executor.submit(
                parser_generator.create_opaque_getter_functions, message, opaque_fields
            ),
            self._executor.submit(
                parser_generator.create_opaque_getter_procedures, message, opaque_fields
            ),
            self._executor.submit(
                parser_generator.create_generic_opaque_getter_procedures, message, opaque_fields
            ),
            self._executor.submit(serializer_generator.create_valid_size_function, message),
            self._executor.submit(serializer_generator.create_valid_length_function, message),
            self._executor.submit(
                serializer_generator.create_set_procedure, message, scalar_fields, composite_fields
            ),
            self._executor.submit(
                serializer_generator.create_scalar_setter_procedures, message, scalar_fields
            ),
            self._executor.submit(
                serializer_generator.create_composite_setter_empty_procedures, message
            ),
            self._executor.submit(
                serializer_generator.create_sequence_setter_procedures, message, sequence_fields
            ),
            self._executor.submit(
                serializer_generator.create_composite_initialize_procedures,
                message,
                fields_with_explicit_size,
                fields_with_implicit_size,
            ),
            self._executor.submit(serializer_generator.create_opaque_setter_procedures, message),
            self._executor.submit(
                serializer_generator.create_generic_opaque_setter_procedures, message
            ),
            self._executor.submit(
                message_generator.create_switch_procedures, self._prefix, message, sequence_fields
            ),
            self._executor.submit(
                message_generator.create_complete_functions, self._prefix, message, sequence_fields
            ),
            self._executor.submit(
                message_generator.create_update_procedures, self._prefix, message, sequence_fields
            ),
            self._executor.submit(message_generator.create_cursor_function),
            self._executor.submit(message_generator.create_cursors_function),
            self._executor.submit(message_generator.create_cursors_index_function),
            self._executor.submit(message_generator.create_structure, self._prefix, message),
        ]

        for future in futures:
            unit += future.result()

        return units

    @staticmethod
    def _requires_composite_field_function(
        message: Message,
        scalar_fields: ty.Mapping[Field, Scalar],
        composite_fields: ty.Sequence[Field],
        sequence_fields: ty.Mapping[Field, Sequence],
    ) -> bool:
        return bool(
            (scalar_fields and composite_fields)
            or any(message.is_possibly_empty(f) for f in composite_fields)
            or sequence_fields
        )

    def _create_refinement(
        self, refinement: Refinement, units: ty.Mapping[ID, Unit]
    ) -> dict[ID, Unit]:
        result: dict[ID, Unit] = {}
        identifier = refinement.package * const.REFINEMENT_PACKAGE
        null_sdu = not refinement.sdu.fields

        if identifier in units:
            unit = units[identifier]
        else:
            unit = self._create_unit(
                identifier,
                [WithClause(self._prefix * const.TYPES_PACKAGE)] if not null_sdu else [],
            )
            result[identifier] = unit

        assert isinstance(unit, PackageUnit), "unexpected unit type"

        literals = []

        def find_literals(expression: expr.Expr) -> expr.Expr:
            if isinstance(expression, expr.Literal):
                literals.append(expression)
            return expression

        refinement.condition.substituted(find_literals)

        for l in literals:
            if len(l.identifier.parts) == 2 and l.identifier.parent != refinement.package:
                unit.declaration_context.extend(
                    [
                        WithClause(self._prefix * l.identifier.parent),
                        UsePackageClause(self._prefix * l.identifier.parent),
                    ]
                )

        if not null_sdu:
            unit += UnitPart(
                [UseTypeClause(const.TYPES_INDEX), UseTypeClause(const.TYPES_BIT_INDEX)]
            )

        if refinement.pdu.package != refinement.package:
            pdu_package = (
                refinement.pdu.base.package
                if isinstance(refinement.pdu, DerivedMessage)
                else refinement.pdu.package
            )

            unit.declaration_context.extend(
                [
                    WithClause(self._prefix * pdu_package),
                    UsePackageClause(self._prefix * pdu_package),
                ]
            )

        pdu_identifier = self._prefix * refinement.pdu.identifier
        sdu_identifier = self._prefix * refinement.sdu.identifier

        unit.declaration_context.append(WithClause(pdu_identifier))

        if not null_sdu:
            unit.declaration_context.extend(
                [
                    WithClause(sdu_identifier),
                ]
            )

        condition_fields = {
            f: t
            for f, t in refinement.pdu.types.items()
            if expr.Variable(f.name) in refinement.condition
        }

        unit += self._create_contains_function(refinement, condition_fields, null_sdu)
        if not null_sdu:
            unit += UnitPart(
                [
                    UseTypeClause(f"{pdu_identifier}.Field_Cursors"),
                ]
            )
            unit += self._create_switch_procedure(refinement, condition_fields)
            unit += self._create_copy_refined_field_procedure(refinement, condition_fields)

        return result

    def _create_type(
        self, field_type: Type, message_package: ID, units: ty.Mapping[ID, Unit]
    ) -> dict[ID, Unit]:
        assert field_type.package != BUILTINS_PACKAGE

        result: dict[ID, Unit] = {}

        unit = units[message_package]

        assert isinstance(unit, PackageUnit)

        if isinstance(field_type, (Integer, Enumeration)):
            unit.declaration_context.append(WithClause(self._prefix * const.TYPES))

        if isinstance(field_type, ModularInteger):
            unit += UnitPart(modular_types(field_type))
            unit += self._integer_functions(field_type)
        elif isinstance(field_type, RangeInteger):
            unit += UnitPart(range_types(field_type))
            unit += self._integer_functions(field_type)
        elif isinstance(field_type, Enumeration):
            unit += UnitPart(enumeration_types(field_type))
            unit += self._enumeration_functions(field_type)
        elif isinstance(field_type, Sequence):
            result.update(self._create_sequence(field_type))
        else:
            assert False, f'unexpected type "{type(field_type).__name__}"'

        return result

    def _create_sequence(self, sequence_type: Sequence) -> dict[ID, Unit]:
        context, package = common.create_sequence_instantiation(sequence_type, self._prefix)
        return {
            package.identifier: self._create_instantiation_unit(
                [
                    Pragma("SPARK_Mode"),
                    *context,
                    # https://github.com/Componolit/Workarounds/issues/33
                    # A compiler error about a non-visible declaration of RFLX_Types inside the
                    # generic sequence package is prevented by adding a with-clause for this
                    # package.
                    Pragma(
                        "Warnings",
                        [Variable("Off"), String('unit "*RFLX_Types" is not referenced')],
                    ),
                    WithClause(self._prefix * const.TYPES_PACKAGE),
                    Pragma(
                        "Warnings",
                        [Variable("On"), String('unit "*RFLX_Types" is not referenced')],
                    ),
                ],
                package,
            )
        }

    def _integer_functions(self, integer: Integer) -> UnitPart:
        specification: ty.List[Declaration] = []

        constraints = (
            expr.And(
                *(
                    [expr.GreaterEqual(expr.Variable("Val"), integer.first)]
                    if integer.first.simplified() != expr.Number(0)
                    else []
                ),
                *(
                    [expr.LessEqual(expr.Variable("Val"), integer.last)]
                    if integer.last.simplified() != expr.Number(2**MAX_SCALAR_SIZE - 1)
                    else []
                ),
            ).simplified()
            if integer.first.simplified() != integer.last.simplified()
            else expr.Equal(expr.Variable("Val"), integer.first.simplified())
        )

        if constraints == expr.TRUE:
            specification.extend(
                [
                    Pragma("Warnings", [Variable("Off"), String('unused variable "Val"')]),
                    Pragma(
                        "Warnings",
                        [Variable("Off"), String('formal parameter "Val" is not referenced')],
                    ),
                ]
            )
        else:
            specification.append(UseTypeClause(self._prefix * const.TYPES_BASE_INT))

        specification.append(
            self._type_validation_function(integer.name, "Val", constraints.ada_expr())
        )

        if constraints == expr.TRUE:
            specification.extend(
                [
                    Pragma(
                        "Warnings",
                        [Variable("On"), String('formal parameter "Val" is not referenced')],
                    ),
                    Pragma("Warnings", [Variable("On"), String('unused variable "Val"')]),
                ]
            )

        specification.extend(self._integer_conversion_functions(integer))

        return UnitPart(specification)

    def _enumeration_functions(self, enum: Enumeration) -> UnitPart:
        incomplete = len(enum.literals) < 2**MAX_SCALAR_SIZE

        specification: ty.List[Declaration] = []

        validation_expression = (
            (
                Less(Variable("Val"), Pow(Number(2), enum.size.ada_expr()))
                if enum.size.simplified() != expr.Number(MAX_SCALAR_SIZE)
                else TRUE
            )
            if enum.always_valid
            else In(
                Variable("Val"), ChoiceList(*[value.ada_expr() for value in enum.literals.values()])
            )
        )

        if validation_expression != TRUE:
            specification.append(UseTypeClause(self._prefix * const.TYPES_BASE_INT))

        specification.append(
            self._type_validation_function(
                enum.name,
                "Val" if validation_expression != TRUE else "Unused_Val",
                validation_expression,
            )
        )

        if enum.always_valid:
            specification.append(
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        f"Valid_{enum.name}",
                        "Boolean",
                        [Parameter(["Val"], enum.name)],
                    ),
                    If(
                        [(Variable("Val.Known"), TRUE)],
                        And(
                            Call(f"Valid_{enum.name}", [Variable("Val.Raw")]),
                            NotIn(
                                Variable("Val.Raw"),
                                ChoiceList(*[value.ada_expr() for value in enum.literals.values()]),
                            ),
                        ),
                    ),
                )
            )

        specification.append(
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Base_Integer",
                    self._prefix * const.TYPES_BASE_INT,
                    [
                        Parameter(
                            ["Enum"],
                            self._prefix
                            * (
                                common.full_enum_name(enum)
                                if enum.always_valid
                                else enum.identifier
                            ),
                        )
                    ],
                ),
                Case(
                    Variable("Enum"),
                    [(Variable(key), value.ada_expr()) for key, value in enum.literals.items()],
                ),
            )
        )

        conversion_function = FunctionSpecification(
            "To_Actual",
            self._prefix * enum.identifier,
            [Parameter(["Val"], self._prefix * const.TYPES_BASE_INT)],
        )
        precondition = Precondition(Call(f"Valid_{enum.name}", [Variable("Val")]))
        conversion_cases: ty.List[ty.Tuple[Expr, Expr]] = []

        if enum.always_valid:
            specification.append(
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "To_Actual",
                        self._prefix * enum.identifier,
                        [Parameter(["Enum"], common.enum_name(enum))],
                    ),
                    Aggregate(TRUE, Variable("Enum")),
                )
            )

            conversion_cases.extend(
                (value.ada_expr(), Aggregate(Variable("True"), Variable(key)))
                for key, value in enum.literals.items()
            )
            conversion_cases.append(
                (Variable("others"), Aggregate(Variable("False"), Variable("Val")))
            )

            specification.append(
                ExpressionFunctionDeclaration(
                    conversion_function, Case(Variable("Val"), conversion_cases), [precondition]
                )
            )

            specification.append(
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "To_Base_Integer",
                        self._prefix * const.TYPES_BASE_INT,
                        [Parameter(["Val"], self._prefix * enum.identifier)],
                    ),
                    If(
                        [(Variable("Val.Known"), Call("To_Base_Integer", [Variable("Val.Enum")]))],
                        Variable("Val.Raw"),
                    ),
                )
            )
        else:
            conversion_cases.extend(
                [
                    *[(value.ada_expr(), Variable(key)) for key, value in enum.literals.items()],
                    *(
                        [(Variable("others"), Last(self._prefix * enum.identifier))]
                        if incomplete
                        else []
                    ),
                ]
            )

            specification.extend(
                [
                    Pragma("Warnings", [Variable("Off"), String("unreachable branch")]),
                    ExpressionFunctionDeclaration(
                        conversion_function, Case(Variable("Val"), conversion_cases), [precondition]
                    ),
                    Pragma("Warnings", [Variable("On"), String("unreachable branch")]),
                ]
            )

        return UnitPart(specification)

    def _create_contains_function(
        self,
        refinement: Refinement,
        condition_fields: ty.Mapping[Field, Type],
        null_sdu: bool,
    ) -> SubprogramUnitPart:
        pdu_identifier = self._prefix * refinement.pdu.identifier
        condition = refinement.condition
        for f, t in condition_fields.items():
            if isinstance(t, Enumeration) and t.always_valid:
                condition = expr.AndThen(
                    expr.Selected(
                        expr.Call(pdu_identifier * f"Get_{f.name}", [expr.Variable("Ctx")]), "Known"
                    ),
                    condition,
                )
        condition = (
            condition.substituted(
                mapping={
                    expr.Variable(f.name): expr.Selected(
                        expr.Call(pdu_identifier * f"Get_{f.name}", [expr.Variable("Ctx")]), "Enum"
                    )
                    if isinstance(t, Enumeration) and t.always_valid
                    else expr.Call(pdu_identifier * f"Get_{f.name}", [expr.Variable("Ctx")])
                    for f, t in condition_fields.items()
                }
            )
            .substituted(
                lambda e: e.copy(identifier=self._prefix * e.identifier)
                if isinstance(e, expr.Literal) and e.identifier not in BOOLEAN.literals
                else e
            )
            .simplified()
        )

        specification = FunctionSpecification(
            contains_function_name(refinement),
            "Boolean",
            [Parameter(["Ctx"], pdu_identifier * "Context")],
        )

        return SubprogramUnitPart(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    expr.AndThen(
                        *self._refinement_conditions(refinement, "Ctx", condition_fields, null_sdu),
                        condition,
                    )
                    .simplified()
                    .ada_expr(),
                )
            ]
        )

    def _create_switch_procedure(
        self, refinement: Refinement, condition_fields: ty.Mapping[Field, Type]
    ) -> UnitPart:
        pdu_identifier = self._prefix * refinement.pdu.identifier
        sdu_identifier = self._prefix * refinement.sdu.identifier
        pdu_context = f"{refinement.pdu.identifier.flat}_PDU_Context"
        sdu_context = f"{refinement.sdu.identifier.flat}_SDU_Context"
        refined_field_affixed_name = pdu_identifier * refinement.field.affixed_name

        specification = ProcedureSpecification(
            f"Switch_To_{refinement.field.name}",
            [
                InOutParameter([pdu_context], pdu_identifier * "Context"),
                OutParameter([sdu_context], sdu_identifier * "Context"),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Not(Constrained(pdu_context)),
                                Not(Constrained(sdu_context)),
                                *[
                                    c.ada_expr()
                                    for c in self._refinement_conditions(
                                        refinement, pdu_context, condition_fields, null_sdu=False
                                    )
                                ],
                                Call(
                                    self._prefix
                                    * refinement.package
                                    * const.REFINEMENT_PACKAGE
                                    * contains_function_name(refinement),
                                    [Variable(pdu_context)],
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                Not(Call(pdu_identifier * "Has_Buffer", [Variable(pdu_context)])),
                                Call(sdu_identifier * "Has_Buffer", [Variable(sdu_context)]),
                                Equal(
                                    Selected(Variable(pdu_context), "Buffer_First"),
                                    Selected(Variable(sdu_context), "Buffer_First"),
                                ),
                                Equal(
                                    Selected(Variable(pdu_context), "Buffer_Last"),
                                    Selected(Variable(sdu_context), "Buffer_Last"),
                                ),
                                Equal(
                                    Selected(Variable(sdu_context), "First"),
                                    Call(
                                        pdu_identifier * "Field_First",
                                        [
                                            Variable(pdu_context),
                                            Variable(refined_field_affixed_name),
                                        ],
                                    ),
                                ),
                                Equal(
                                    Selected(Variable(sdu_context), "Last"),
                                    Call(
                                        pdu_identifier * "Field_Last",
                                        [
                                            Variable(pdu_context),
                                            Variable(refined_field_affixed_name),
                                        ],
                                    ),
                                ),
                                Call(sdu_identifier * "Initialized", [Variable(sdu_context)]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected(Variable(pdu_context), "Buffer_First"),
                                        Selected(Variable(pdu_context), "Buffer_Last"),
                                        Selected(Variable(pdu_context), "First"),
                                        Call(
                                            pdu_identifier * "Context_Cursors",
                                            [Variable(pdu_context)],
                                        ),
                                    ]
                                ],
                            )
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_BIT_INDEX,
                            Call(
                                pdu_identifier * "Field_First",
                                [
                                    Variable(pdu_context),
                                    Variable(refined_field_affixed_name),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            const.TYPES_BIT_LENGTH,
                            Call(
                                pdu_identifier * "Field_Last",
                                [
                                    Variable(pdu_context),
                                    Variable(refined_field_affixed_name),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(["Buffer"], const.TYPES_BYTES_PTR),
                    ],
                    [
                        CallStatement(
                            pdu_identifier * "Take_Buffer",
                            [Variable(pdu_context), Variable("Buffer")],
                        ),
                        PragmaStatement(
                            "Warnings",
                            [Variable("Off"), String('unused assignment to "Buffer"')],
                        ),
                        CallStatement(
                            sdu_identifier * "Initialize",
                            [
                                Variable(sdu_context),
                                Variable("Buffer"),
                                Variable("First"),
                                Variable("Last"),
                                Variable("Last"),
                            ],
                        ),
                        PragmaStatement(
                            "Warnings",
                            [Variable("On"), String('unused assignment to "Buffer"')],
                        ),
                    ],
                )
            ],
        )

    def _create_copy_refined_field_procedure(
        self, refinement: Refinement, condition_fields: ty.Mapping[Field, Type]
    ) -> UnitPart:
        pdu_identifier = self._prefix * refinement.pdu.identifier
        sdu_identifier = self._prefix * refinement.sdu.identifier
        pdu_context = ID(refinement.pdu.identifier.flat + "_PDU_Context")
        sdu_context = ID(refinement.sdu.identifier.flat + "_SDU_Context")
        refined_field_affixed_name = pdu_identifier * refinement.field.affixed_name

        specification = ProcedureSpecification(
            f"Copy_{refinement.field.name}",
            [
                Parameter([pdu_context], pdu_identifier * "Context"),
                InOutParameter([sdu_context], sdu_identifier * "Context"),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained(sdu_context)),
                                Call(sdu_identifier * "Has_Buffer", [Variable(sdu_context)]),
                                *[
                                    c.ada_expr()
                                    for c in self._refinement_conditions(
                                        refinement, pdu_context, condition_fields, null_sdu=False
                                    )
                                ],
                                Call(
                                    self._prefix
                                    * refinement.package
                                    * const.REFINEMENT_PACKAGE
                                    * contains_function_name(refinement),
                                    [Variable(pdu_context)],
                                ),
                                GreaterEqual(
                                    Add(
                                        Call(
                                            const.TYPES_TO_LAST_BIT_INDEX,
                                            [Variable(sdu_context * "Buffer_Last")],
                                        ),
                                        -Call(
                                            const.TYPES_TO_FIRST_BIT_INDEX,
                                            [Variable(sdu_context * "Buffer_First")],
                                        ),
                                        Number(1),
                                    ),
                                    Call(
                                        pdu_identifier * "Field_Size",
                                        [
                                            Variable(pdu_context),
                                            Variable(refined_field_affixed_name),
                                        ],
                                    ),
                                ),
                                Less(
                                    Add(
                                        Call(
                                            const.TYPES_TO_FIRST_BIT_INDEX,
                                            [Variable(sdu_context * "Buffer_First")],
                                        ),
                                        Call(
                                            pdu_identifier * "Field_Size",
                                            [
                                                Variable(pdu_context),
                                                Variable(refined_field_affixed_name),
                                            ],
                                        ),
                                        -Number(1),
                                    ),
                                    Last(const.TYPES_BIT_INDEX),
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                Call(pdu_identifier * "Has_Buffer", [Variable(pdu_context)]),
                                Call(sdu_identifier * "Has_Buffer", [Variable(sdu_context)]),
                                Call(sdu_identifier * "Initialized", [Variable(sdu_context)]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected(Variable(sdu_context), "Buffer_First"),
                                        Selected(Variable(sdu_context), "Buffer_Last"),
                                    ]
                                ],
                            )
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_BIT_INDEX,
                            Call(
                                const.TYPES_TO_FIRST_BIT_INDEX,
                                [Variable(sdu_context * "Buffer_First")],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Size"],
                            const.TYPES_BIT_INDEX,
                            Call(
                                pdu_identifier * "Field_Size",
                                [
                                    Variable(pdu_context),
                                    Variable(refined_field_affixed_name),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(["Buffer"], const.TYPES_BYTES_PTR),
                    ],
                    [
                        PragmaStatement(
                            "Warnings",
                            [
                                Variable("Off"),
                                String(
                                    f'"{sdu_context.ada_str}" is set by "Take_Buffer"'
                                    " but not used after the call"
                                ),
                            ],
                        ),
                        CallStatement(
                            sdu_identifier * "Take_Buffer",
                            [Variable(sdu_context), Variable("Buffer")],
                        ),
                        PragmaStatement(
                            "Warnings",
                            [
                                Variable("On"),
                                String(
                                    f'"{sdu_context.ada_str}" is set by "Take_Buffer"'
                                    " but not used after the call"
                                ),
                            ],
                        ),
                        CallStatement(
                            pdu_identifier * f"Get_{refinement.field.name}",
                            [
                                Variable(pdu_context),
                                Slice(
                                    Variable("Buffer.all"),
                                    First("Buffer"),
                                    Add(
                                        First("Buffer"),
                                        Call(
                                            const.TYPES_INDEX,
                                            [
                                                Call(
                                                    const.TYPES_TO_LENGTH,
                                                    [Variable("Size")],
                                                ),
                                            ],
                                        ),
                                        -Number(1),
                                    ),
                                ),
                            ],
                        ),
                        CallStatement(
                            sdu_identifier * "Initialize",
                            [
                                Variable(sdu_context),
                                Variable("Buffer"),
                                Variable("First"),
                                Add(
                                    Variable("First"),
                                    Variable("Size"),
                                    -Number(1),
                                ),
                                Add(
                                    Variable("First"),
                                    Variable("Size"),
                                    -Number(1),
                                ),
                            ],
                        ),
                    ],
                )
            ],
        )

    def _check_template_file(self, filename: str) -> None:
        assert self._template_dir.joinpath(
            filename
        ).is_file(), f'template file not found: "{filename}"'

    def _license_header(self) -> str:
        if self._reproducible:
            return ""

        filename = "license_header"
        self._check_template_file(filename)
        today = date.today()
        return (
            self._template_dir.joinpath(filename)
            .read_text(encoding="utf-8")
            .format(
                version=__version__,
                date=today,
                year=today.year,
            )
        )

    def _type_validation_function(
        self, type_name: str, enum_value: str, validation_expression: Expr
    ) -> Subprogram:
        return ExpressionFunctionDeclaration(
            FunctionSpecification(
                f"Valid_{type_name}",
                "Boolean",
                [Parameter([enum_value], self._prefix * const.TYPES_BASE_INT)],
            ),
            validation_expression,
        )

    def _integer_conversion_functions(self, integer: Integer) -> ty.Sequence[Subprogram]:
        return [
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Base_Integer",
                    self._prefix * const.TYPES_BASE_INT,
                    [Parameter(["Val"], self._prefix * integer.identifier)],
                ),
                Call(self._prefix * const.TYPES_BASE_INT, [Variable("Val")]),
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Actual",
                    self._prefix * integer.identifier,
                    [Parameter(["Val"], self._prefix * const.TYPES_BASE_INT)],
                ),
                Call(self._prefix * integer.identifier, [Variable("Val")]),
                [Precondition(Call(f"Valid_{integer.name}", [Variable("Val")]))],
            ),
        ]

    def _refinement_conditions(
        self,
        refinement: Refinement,
        pdu_context: StrID,
        condition_fields: ty.Mapping[Field, Type],
        null_sdu: bool,
    ) -> ty.Sequence[expr.Expr]:
        pdu_identifier = self._prefix * refinement.pdu.identifier

        conditions: ty.List[expr.Expr] = [
            expr.Call(pdu_identifier * "Has_Buffer", [expr.Variable(pdu_context)])
        ]

        if null_sdu:
            conditions.extend(
                [
                    expr.Call(
                        pdu_identifier * "Structural_Valid",
                        [
                            expr.Variable(pdu_context),
                            expr.Variable(pdu_identifier * refinement.field.affixed_name),
                        ],
                    ),
                    expr.Not(
                        expr.Call(
                            pdu_identifier * "Present",
                            [
                                expr.Variable(pdu_context),
                                expr.Variable(pdu_identifier * refinement.field.affixed_name),
                            ],
                        )
                    ),
                ]
            )
        else:
            conditions.append(
                expr.Call(
                    pdu_identifier * "Present",
                    [
                        expr.Variable(pdu_context),
                        expr.Variable(pdu_identifier * refinement.field.affixed_name),
                    ],
                )
            )

        conditions.extend(
            [
                expr.Call(
                    pdu_identifier * "Valid",
                    [
                        expr.Variable(pdu_context),
                        expr.Variable(pdu_identifier * f.affixed_name),
                    ],
                )
                for f in condition_fields
            ]
        )

        return conditions


def create_file(filename: Path, content: str) -> None:
    log.info("Creating %s", filename)

    if filename.exists():
        fail(f"file {filename} already exists", subsystem=Subsystem.GENERATOR)
    filename.write_text(content)


def modular_types(integer: ModularInteger) -> ty.List[Declaration]:
    return [
        ModularType(
            integer.name,
            integer.modulus.ada_expr(),
            aspects=[SizeAspect(integer.size_expr.ada_expr())],
        )
    ]


def range_types(integer: RangeInteger) -> ty.List[Declaration]:
    return [
        RangeType(
            integer.name,
            integer.first_expr.ada_expr(),
            integer.last_expr.ada_expr(),
            aspects=[SizeAspect(integer.size_expr.ada_expr())],
        ),
    ]


def enumeration_types(enum: Enumeration) -> ty.List[Declaration]:
    types: ty.List[Declaration] = []

    types.append(
        EnumerationType(
            common.enum_name(enum) if enum.always_valid else enum.name,
            {k: Number(v.value) for k, v in enum.literals.items()},
            enum.size_expr.ada_expr(),
        )
    )
    if enum.always_valid:
        types.append(
            RecordType(
                enum.name,
                [],
                [Discriminant(["Known"], "Boolean", FALSE)],
                VariantPart(
                    "Known",
                    [
                        Variant([TRUE], [Component("Enum", common.enum_name(enum))]),
                        Variant([FALSE], [Component("Raw", const.TYPES_BASE_INT)]),
                    ],
                ),
            )
        )

    return types


def contains_function_name(refinement: Refinement) -> str:
    return common.contains_function_name(
        refinement.package,
        refinement.pdu.identifier,
        refinement.sdu.identifier,
        refinement.field.identifier,
    )
