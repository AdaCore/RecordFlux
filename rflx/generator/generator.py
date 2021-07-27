# pylint: disable=too-many-lines
import itertools
import logging
import typing as ty
from datetime import date
from pathlib import Path

import pkg_resources

from rflx import __version__, expression as expr
from rflx.ada import (
    FALSE,
    ID,
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
    Annotate,
    ArrayType,
    Aspect,
    Assignment,
    Call,
    CallStatement,
    Case,
    CaseStatement,
    Component,
    Constrained,
    ContextItem,
    ContractCases,
    Declaration,
    Declare,
    DefaultInitialCondition,
    Depends,
    Discriminant,
    DynamicPredicate,
    EnumerationType,
    Equal,
    Expr,
    ExpressionFunctionDeclaration,
    First,
    FormalDeclaration,
    FormalSubprogramDeclaration,
    FunctionSpecification,
    GenericPackageInstantiation,
    Ghost,
    Greater,
    GreaterEqual,
    If,
    IfStatement,
    In,
    Indexed,
    InitialCondition,
    InOutParameter,
    InstantiationUnit,
    Last,
    Length,
    Less,
    LessEqual,
    Mod,
    ModularType,
    NamedAggregate,
    Not,
    NotEqual,
    NullComponent,
    Number,
    ObjectDeclaration,
    Old,
    Or,
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
    PrivateType,
    ProcedureSpecification,
    Range,
    RangeSubtype,
    RangeType,
    RecordType,
    Result,
    Selected,
    Size,
    SizeAspect,
    Slice,
    SparkMode,
    Statement,
    StrID,
    String,
    Sub,
    Subprogram,
    SubprogramBody,
    SubprogramDeclaration,
    SubprogramUnitPart,
    Unit,
    UnitPart,
    UsePackageClause,
    UseTypeClause,
    ValueRange,
    Variable,
    Variant,
    VariantPart,
    WithClause,
)
from rflx.common import file_name
from rflx.const import BUILTINS_PACKAGE, INTERNAL_PACKAGE
from rflx.error import Subsystem, fail, warn
from rflx.model import (
    FINAL,
    INITIAL,
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

from . import common, const
from .parser import ParserGenerator
from .serializer import SerializerGenerator
from .session import SessionGenerator

log = logging.getLogger(__name__)

NULL = Variable("null")


class Generator:  # pylint: disable = too-many-instance-attributes
    def __init__(
        self,
        model: Model,
        prefix: str = "",
        reproducible: bool = False,
        debug: bool = False,
        ignore_unsupported_checksum: bool = False,
    ) -> None:
        self.__prefix = str(ID(prefix)) if prefix else ""
        self.__reproducible = reproducible
        self.__debug = debug
        self.__ignore_unsupported_checksum = ignore_unsupported_checksum
        self.__parser = ParserGenerator(self.__prefix)
        self.__serializer = SerializerGenerator(self.__prefix)

        self._units: ty.Dict[ID, Unit] = {}

        self.__template_dir = Path(pkg_resources.resource_filename(*const.TEMPLATE_DIR))
        assert self.__template_dir.is_dir(), "template directory not found"

        self.__generate(model)

    def write_library_files(self, directory: Path) -> None:
        for template_filename in const.LIBRARY_FILES:
            self.__check_template_file(template_filename)

            prefix = f"{self.__prefix}." if self.__prefix else ""
            filename = f"{file_name(prefix)}{template_filename}"

            with open(self.__template_dir / Path(template_filename)) as template_file:
                create_file(
                    Path(directory) / Path(filename),
                    self.__license_header()
                    + "".join(
                        [
                            l.format(prefix=prefix)
                            for l in template_file
                            if "  --  WORKAROUND" not in l
                        ]
                    ),
                )

    def write_top_level_package(self, directory: Path) -> None:
        if self.__prefix:
            create_file(
                Path(directory) / Path(file_name(self.__prefix) + ".ads"),
                self.__license_header() + f"package {self.__prefix} is\n\nend {self.__prefix};",
            )

    def write_units(self, directory: Path) -> None:
        for unit in self._units.values():
            create_file(directory / Path(unit.name + ".ads"), self.__license_header() + unit.ads)

            if unit.adb:
                create_file(
                    directory / Path(unit.name + ".adb"), self.__license_header() + unit.adb
                )

    def __generate(self, model: Model) -> None:
        for t in model.types:
            if t.package in [BUILTINS_PACKAGE, INTERNAL_PACKAGE]:
                continue

            if t.package not in self._units:
                self.__create_unit(ID(t.package), terminating=False)

            if isinstance(t, (Scalar, Composite)):
                self.__create_type(t, ID(t.package))

            elif isinstance(t, Message):
                # ISSUE: Componolit/RecordFlux#276
                for c in t.checksums:
                    if not self.__ignore_unsupported_checksum:
                        fail(
                            "unsupported checksum (consider --ignore-unsupported-checksum option)",
                            Subsystem.GENERATOR,
                            location=c.location,
                        )
                    else:
                        warn(
                            "unsupported checksum ignored", Subsystem.GENERATOR, location=c.location
                        )

                self.__create_message(t)

            elif isinstance(t, Refinement):
                self.__create_refinement(t)

            else:
                assert False, f'unexpected type "{type(t).__name__}"'

        for s in model.sessions:
            if s.package not in self._units:
                self.__create_unit(ID(s.package), terminating=False)

            self.__create_session(s)

    def __create_session(self, session: Session) -> None:
        session_generator = SessionGenerator(session, self.__prefix, debug=self.__debug)
        unit = self.__create_unit(
            session_generator.unit_identifier,
            session_generator.declaration_context,
            session_generator.body_context,
            session_generator.formal_parameters,
            [InitialCondition(Variable("Uninitialized"))],
            terminating=False,
        )
        unit += session_generator.unit_part

    def __create_unit(  # pylint: disable = too-many-arguments
        self,
        identifier: ID,
        declaration_context: ty.Sequence[ContextItem] = None,
        body_context: ty.Sequence[ContextItem] = None,
        formal_parameters: ty.List[FormalDeclaration] = None,
        aspects: ty.Sequence[Aspect] = None,
        terminating: bool = True,
    ) -> PackageUnit:
        declaration_context = declaration_context if declaration_context else []
        body_context = body_context if body_context else []
        aspects = aspects if aspects else []

        unit = PackageUnit(
            [*const.CONFIGURATION_PRAGMAS, *declaration_context],
            PackageDeclaration(
                self.__prefix * identifier,
                formal_parameters=formal_parameters,
                aspects=[
                    SparkMode(),
                    *([Annotate("GNATprove", "Terminating")] if terminating else []),
                    *aspects,
                ],
            ),
            [*const.CONFIGURATION_PRAGMAS, *body_context],
            PackageBody(self.__prefix * identifier, aspects=[SparkMode()]),
        )
        self._units[identifier] = unit

        return unit

    def __create_instantiation_unit(
        self,
        identifier: ID,
        context: ty.List[ContextItem],
        instantiation: GenericPackageInstantiation,
    ) -> InstantiationUnit:
        for p in reversed(const.CONFIGURATION_PRAGMAS):
            context.insert(0, p)

        unit = InstantiationUnit(context, instantiation)
        self._units[identifier] = unit

        return unit

    # pylint: disable=too-many-statements
    def __create_message(self, message: Message) -> None:
        if not message.fields:
            return

        context: ty.List[ContextItem] = []

        if any(t.package == BUILTINS_PACKAGE for t in message.types.values()):
            context.extend(
                [
                    WithClause(self.__prefix * const.TYPES_PACKAGE),
                    WithClause(self.__prefix * const.BUILTIN_TYPES_PACKAGE),
                    WithClause(self.__prefix * const.BUILTIN_TYPES_CONVERSIONS_PACKAGE),
                    UsePackageClause(self.__prefix * const.BUILTIN_TYPES_CONVERSIONS_PACKAGE),
                ]
            )

        context.append(WithClause(self.__prefix * const.TYPES_PACKAGE))

        for field_type in message.types.values():
            if field_type.package in [BUILTINS_PACKAGE, INTERNAL_PACKAGE]:
                continue

            if isinstance(field_type, Scalar) and field_type.package != message.package:
                context.extend(
                    [
                        WithClause(self.__prefix * ID(field_type.package)),
                        UsePackageClause(self.__prefix * ID(field_type.package)),
                    ]
                )

            elif isinstance(field_type, Sequence):
                context.append(WithClause(self.__prefix * ID(field_type.identifier)))

        unit = self.__create_unit(ID(message.identifier), context)

        scalar_fields = {}
        composite_fields = []
        sequence_fields = {}
        opaque_fields = []

        for f, t in message.types.items():
            if isinstance(t, Scalar):
                scalar_fields[f] = t
            if isinstance(t, Composite):
                composite_fields.append(f)
                if isinstance(t, Sequence):
                    sequence_fields[f] = t
                if isinstance(t, Opaque):
                    opaque_fields.append(f)

        unit += self.__create_use_type_clause(composite_fields)
        unit += self.__create_field_type(message)
        unit += self.__create_state_type()
        unit += self.__create_cursor_type(message)
        unit += self.__create_cursor_validation_functions()
        unit += self.__create_valid_context_function(message, composite_fields)
        unit += self.__create_context_type()
        unit += self.__create_field_dependent_type(message)
        unit += self.__create_initialize_procedure()
        unit += self.__create_restricted_initialize_procedure(message)
        unit += self.__create_initialized_function(message)
        unit += self.__create_reset_procedure()
        unit += self.__create_restricted_reset_procedure(message)
        unit += self.__create_take_buffer_procedure()
        unit += self.__create_copy_procedure()
        unit += self.__create_read_procedure()
        unit += self.__create_write_procedure()
        unit += self.__create_has_buffer_function()
        unit += self.__create_size_function()
        unit += self.__create_byte_size_function()
        unit += self.__create_message_last_function()
        unit += self.__create_message_data_function()
        unit += self.__create_path_condition_function(message)
        unit += self.__create_field_condition_function(message)
        unit += self.__create_field_size_function(message)
        unit += self.__create_field_first_function(message)
        unit += self.__create_field_last_function()
        unit += self.__create_predecessor_function()
        unit += self.__create_successor_function(message)
        unit += self.__create_valid_predecessor_function(message, composite_fields)
        unit += self.__create_invalid_successor_function(message)
        unit += self.__create_valid_next_function()
        unit += self.__create_available_space_function()
        unit += self.__create_sufficient_buffer_length_function()
        if composite_fields:
            unit += self.__create_equal_function(scalar_fields, composite_fields)
        unit += self.__create_reset_dependent_fields_procedure(message)

        unit += self.__parser.create_internal_functions(message, scalar_fields, composite_fields)
        unit += self.__parser.create_verify_procedure(message, scalar_fields, composite_fields)
        unit += self.__parser.create_verify_message_procedure(message)
        unit += self.__parser.create_present_function()
        unit += self.__parser.create_structural_valid_function()
        unit += self.__parser.create_valid_function()
        unit += self.__parser.create_incomplete_function()
        unit += self.__parser.create_invalid_function()
        unit += self.__parser.create_structural_valid_message_function(message)
        unit += self.__parser.create_valid_message_function(message)
        unit += self.__parser.create_incomplete_message_function(message)
        unit += self.__parser.create_scalar_getter_functions(scalar_fields)
        unit += self.__parser.create_opaque_getter_functions(opaque_fields)
        unit += self.__parser.create_opaque_getter_procedures(opaque_fields)
        unit += self.__parser.create_generic_opaque_getter_procedures(opaque_fields)

        unit += self.__serializer.create_internal_functions(message, scalar_fields)
        unit += self.__serializer.create_scalar_setter_procedures(message, scalar_fields)
        unit += self.__serializer.create_composite_setter_empty_procedures(message)
        unit += self.__serializer.create_sequence_setter_procedures(message, sequence_fields)
        unit += self.__serializer.create_composite_initialize_procedures(message)
        unit += self.__serializer.create_opaque_setter_procedures(message)
        unit += self.__serializer.create_generic_opaque_setter_procedures(message)

        unit += self.__create_switch_procedures(message, sequence_fields)
        unit += self.__create_complete_functions(message, sequence_fields)
        unit += self.__create_update_procedures(message, sequence_fields)
        unit += self.__create_cursor_function()
        unit += self.__create_cursors_function()
        unit += self.__create_structure(message)

    @staticmethod
    def __create_use_type_clause(composite_fields: ty.Sequence[Field]) -> UnitPart:
        return UnitPart(
            [
                Pragma(
                    "Warnings",
                    [Variable("Off"), String('use clause for type "U64" * has no effect')],
                ),
                Pragma(
                    "Warnings",
                    [
                        Variable("Off"),
                        String('"LENGTH" is already use-visible through previous use_type_clause'),
                    ],
                ),  # required when user-defined type Index is subtype of Length
                *[
                    UseTypeClause(t)
                    for t in [
                        *([const.TYPES_BYTES] if composite_fields else []),
                        const.TYPES_BYTES_PTR,
                        const.TYPES_LENGTH,
                        const.TYPES_INDEX,
                        const.TYPES_BIT_INDEX,
                        const.TYPES_U64,
                    ]
                ],
                Pragma(
                    "Warnings",
                    [
                        Variable("On"),
                        String('"LENGTH" is already use-visible through previous use_type_clause'),
                    ],
                ),
                Pragma(
                    "Warnings",
                    [Variable("On"), String('use clause for type "U64" * has no effect')],
                ),
            ]
        )

    @staticmethod
    def __create_field_type(message: Message) -> UnitPart:
        return UnitPart(
            [
                EnumerationType(
                    "Virtual_Field", dict.fromkeys(ID(f.affixed_name) for f in message.all_fields)
                ),
                RangeSubtype(
                    "Field",
                    "Virtual_Field",
                    Variable(message.all_fields[1].affixed_name),
                    Variable(message.all_fields[-2].affixed_name),
                ),
            ]
        )

    @staticmethod
    def __create_state_type() -> UnitPart:
        return UnitPart(
            private=[
                EnumerationType(
                    "Cursor_State",
                    dict.fromkeys(
                        map(ID, ("S_Valid", "S_Structural_Valid", "S_Invalid", "S_Incomplete"))
                    ),
                )
            ]
        )

    @staticmethod
    def __create_cursor_type(message: Message) -> UnitPart:
        discriminants = [Discriminant(["State"], "Cursor_State", Variable("S_Invalid"))]

        return UnitPart(
            [
                PrivateType("Field_Cursor", aspects=[DefaultInitialCondition(FALSE)]),
                PrivateType("Field_Cursors", aspects=[DefaultInitialCondition(FALSE)]),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Valid_Value", "Boolean", [Parameter(["Val"], "Field_Dependent_Value")]
                    ),
                    Case(
                        Variable("Val.Fld"),
                        [
                            (
                                Variable(f.affixed_name),
                                Call("Valid", [Variable(f"Val.{f.name}_Value")])
                                if isinstance(t, Scalar)
                                else TRUE,
                            )
                            for f, t in message.types.items()
                        ]
                        + [
                            (Variable(INITIAL.affixed_name), FALSE),
                            (Variable(FINAL.affixed_name), FALSE),
                        ],
                    ),
                ),
                RecordType(
                    "Field_Cursor",
                    [
                        Component(
                            "Predecessor",
                            "Virtual_Field",
                            Variable(FINAL.affixed_name),
                        )
                    ],
                    discriminants,
                    VariantPart(
                        "State",
                        [
                            Variant(
                                [Variable("S_Valid"), Variable("S_Structural_Valid")],
                                [
                                    Component(
                                        "First",
                                        const.TYPES_BIT_INDEX,
                                        First(const.TYPES_BIT_INDEX),
                                    ),
                                    Component(
                                        "Last",
                                        const.TYPES_BIT_LENGTH,
                                        First(const.TYPES_BIT_LENGTH),
                                    ),
                                    Component(
                                        "Value",
                                        "Field_Dependent_Value",
                                        NamedAggregate(("Fld", Variable(FINAL.affixed_name))),
                                    ),
                                ],
                            ),
                            Variant(
                                [Variable("S_Invalid"), Variable("S_Incomplete")], [NullComponent()]
                            ),
                        ],
                    ),
                    [
                        DynamicPredicate(
                            If(
                                [
                                    (
                                        Or(
                                            Equal(Variable("State"), Variable("S_Valid")),
                                            Equal(
                                                Variable("State"), Variable("S_Structural_Valid")
                                            ),
                                        ),
                                        Call("Valid_Value", [Variable("Field_Cursor.Value")]),
                                    )
                                ]
                            )
                        )
                    ],
                ),
                ArrayType("Field_Cursors", "Virtual_Field", "Field_Cursor"),
            ],
        )

    @staticmethod
    def __create_context_type() -> UnitPart:
        """
        Components of a context type:

            Buffer_First, Buffer_Last:
                The bounds of `Buffer` which are used to ensure that not a completely different
                buffer is moved back into the context.

            First, Last:
                The positions of the first and last usable bit of `Buffer`. These are hard bounds
                which must not be changed during the lifetime of the context.

            Message_Last:
                The position of the last bit of the message. The value is increased after each
                parsed or set field.

            Buffer:
                An access type refering to memory containing the binary message.

            Cursors:
                An array of cursors representing the internal parser or serializer state.
        """

        discriminants = [
            Discriminant(
                ["Buffer_First", "Buffer_Last"], const.TYPES_INDEX, First(const.TYPES_INDEX)
            ),
            Discriminant(["First"], const.TYPES_BIT_INDEX, First(const.TYPES_BIT_INDEX)),
            Discriminant(["Last"], const.TYPES_BIT_LENGTH, First(const.TYPES_BIT_LENGTH)),
        ]

        return UnitPart(
            [
                PrivateType(
                    "Context",
                    discriminants,
                    [DefaultInitialCondition(common.public_context_predicate())],
                )
            ],
            [],
            [
                RecordType(
                    "Context",
                    [
                        Component(
                            "Message_Last",
                            const.TYPES_BIT_LENGTH,
                            Sub(Variable("First"), Number(1)),
                        ),
                        Component("Buffer", const.TYPES_BYTES_PTR, NULL),
                        Component(
                            "Cursors",
                            "Field_Cursors",
                            NamedAggregate(
                                (
                                    "others",
                                    NamedAggregate(
                                        ("State", Variable("S_Invalid")),
                                        (
                                            "Predecessor",
                                            Variable(FINAL.affixed_name),
                                        ),
                                    ),
                                )
                            ),
                        ),
                    ],
                    discriminants,
                    None,
                    [
                        DynamicPredicate(
                            Call(
                                "Valid_Context",
                                [
                                    Variable("Context.Buffer_First"),
                                    Variable("Context.Buffer_Last"),
                                    Variable("Context.First"),
                                    Variable("Context.Last"),
                                    Variable("Context.Message_Last"),
                                    Variable("Context.Buffer"),
                                    Variable("Context.Cursors"),
                                ],
                            )
                        )
                    ],
                )
            ],
        )

    def __create_field_dependent_type(self, message: Message) -> UnitPart:
        null_fields = []
        variants = []
        construction_functions = []

        for f, t in message.types.items():
            if isinstance(t, Composite) and not common.is_compared_to_aggregate(f, message):
                null_fields.append(f)
            elif isinstance(t, Scalar):
                variants.append(
                    Variant(
                        [Variable(f.affixed_name)],
                        [
                            Component(
                                f"{f.name}_Value", self.__prefix * common.full_base_type_name(t)
                            )
                        ],
                    )
                )
            else:
                assert isinstance(t, Composite)
                length = expr.Div(message.field_size(f), expr.Number(8)).simplified()
                assert isinstance(length, expr.Number)
                variants.append(
                    Variant(
                        [Variable(f.affixed_name)],
                        [
                            Component(
                                f"{f.name}_Value",
                                Slice(
                                    Variable(const.TYPES_BYTES),
                                    First(const.TYPES_INDEX),
                                    Add(
                                        First(const.TYPES_INDEX),
                                        expr.Sub(length, expr.Number(1)).simplified().ada_expr(),
                                    ),
                                ),
                            )
                        ],
                    )
                )
                # ISSUE: Componolit/Workarounds#35
                # Prevent a GNAT bug by moving the construction of the record type from the
                # precondition into a function, if the construction involves an array type.
                construction_functions.append(
                    ExpressionFunctionDeclaration(
                        FunctionSpecification(
                            f"Construct_{f.name}_Value",
                            "Field_Dependent_Value",
                            [Parameter(["Data"], const.TYPES_BYTES)],
                        ),
                        NamedAggregate(
                            ("Fld", Variable(f.affixed_name)),
                            (f"{f.name}_Value", Variable("Data")),
                        ),
                        [
                            Precondition(Equal(Length("Data"), length.ada_expr())),
                        ],
                    )
                )

        return UnitPart(
            [
                RecordType(
                    "Field_Dependent_Value",
                    [],
                    [Discriminant(["Fld"], "Virtual_Field", Variable(INITIAL.affixed_name))],
                    VariantPart(
                        "Fld",
                        [
                            Variant(
                                [Variable(f.affixed_name) for f in (INITIAL, *null_fields, FINAL)],
                                [NullComponent()],
                            ),
                            *variants,
                        ],
                    ),
                ),
                *construction_functions,
            ]
        )

    @staticmethod
    def __create_initialize_procedure() -> UnitPart:
        specification = ProcedureSpecification(
            "Initialize",
            [OutParameter(["Ctx"], "Context"), InOutParameter(["Buffer"], const.TYPES_BYTES_PTR)],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained("Ctx")),
                                NotEqual(Variable("Buffer"), NULL),
                                Greater(Length("Buffer"), Number(0)),
                                Less(Last("Buffer"), Last(const.TYPES_INDEX)),
                            )
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Equal(Variable("Buffer"), NULL),
                                Equal(Variable("Ctx.Buffer_First"), Old(First("Buffer"))),
                                Equal(Variable("Ctx.Buffer_Last"), Old(Last("Buffer"))),
                                Equal(
                                    Variable("Ctx.First"),
                                    Call(
                                        const.TYPES * "To_First_Bit_Index",
                                        [Variable("Ctx.Buffer_First")],
                                    ),
                                ),
                                Equal(
                                    Variable("Ctx.Last"),
                                    Call(
                                        const.TYPES * "To_Last_Bit_Index",
                                        [Variable("Ctx.Buffer_Last")],
                                    ),
                                ),
                                Call("Initialized", [Variable("Ctx")]),
                            )
                        ),
                        Depends({"Ctx": ["Buffer"], "Buffer": []}),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        CallStatement(
                            "Initialize",
                            [
                                Variable("Ctx"),
                                Variable("Buffer"),
                                Call(const.TYPES_TO_FIRST_BIT_INDEX, [First("Buffer")]),
                                Call(const.TYPES_TO_LAST_BIT_INDEX, [Last("Buffer")]),
                            ],
                        )
                    ],
                )
            ],
        )

    @staticmethod
    def __create_restricted_initialize_procedure(message: Message) -> UnitPart:
        specification = ProcedureSpecification(
            "Initialize",
            [
                OutParameter(["Ctx"], "Context"),
                InOutParameter(["Buffer"], const.TYPES_BYTES_PTR),
                Parameter(["First"], const.TYPES_BIT_INDEX),
                Parameter(["Last"], const.TYPES_BIT_LENGTH),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained("Ctx")),
                                NotEqual(Variable("Buffer"), NULL),
                                Greater(Length("Buffer"), Number(0)),
                                Less(Last("Buffer"), Last(const.TYPES_INDEX)),
                                GreaterEqual(
                                    Call(const.TYPES_TO_INDEX, [Variable("First")]),
                                    First("Buffer"),
                                ),
                                LessEqual(
                                    Call(const.TYPES_TO_INDEX, [Variable("Last")]),
                                    Last("Buffer"),
                                ),
                                LessEqual(Variable("First"), Add(Variable("Last"), Number(1))),
                                Less(Variable("Last"), Last(const.TYPES_BIT_INDEX)),
                                Equal(Mod(Variable("First"), Size(const.TYPES_BYTE)), Number(1)),
                                Equal(Mod(Variable("Last"), Size(const.TYPES_BYTE)), Number(0)),
                            )
                        ),
                        Postcondition(
                            And(
                                Equal(Variable("Buffer"), NULL),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Equal(Variable("Ctx.Buffer_First"), Old(First("Buffer"))),
                                Equal(Variable("Ctx.Buffer_Last"), Old(Last("Buffer"))),
                                Equal(Variable("Ctx.First"), Variable("First")),
                                Equal(Variable("Ctx.Last"), Variable("Last")),
                                Call("Initialized", [Variable("Ctx")]),
                            )
                        ),
                        Depends({"Ctx": ["Buffer", "First", "Last"], "Buffer": []}),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [
                        ObjectDeclaration(
                            ["Buffer_First"], const.TYPES_INDEX, First("Buffer"), constant=True
                        ),
                        ObjectDeclaration(
                            ["Buffer_Last"], const.TYPES_INDEX, Last("Buffer"), constant=True
                        ),
                    ],
                    [
                        Assignment(
                            "Ctx",
                            Aggregate(
                                Variable("Buffer_First"),
                                Variable("Buffer_Last"),
                                Variable("First"),
                                Variable("Last"),
                                Sub(Variable("First"), Number(1)),
                                Variable("Buffer"),
                                context_cursors_initialization(message),
                            ),
                        ),
                        Assignment("Buffer", NULL),
                    ],
                )
            ],
        )

    @staticmethod
    def __create_initialized_function(message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Initialized", "Boolean", [Parameter(["Ctx"], "Context")]
        )
        first_field = message.fields[0]

        return UnitPart(
            [SubprogramDeclaration(specification, [Ghost()])],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        Equal(
                            Variable("Ctx.Message_Last"),
                            Sub(Variable("Ctx.First"), Number(1)),
                        ),
                        Call(
                            "Valid_Next",
                            [
                                Variable("Ctx"),
                                Variable(first_field.affixed_name),
                            ],
                        ),
                        byte_aligned_field(first_field),
                        Equal(
                            Call(
                                "Available_Space",
                                [
                                    Variable("Ctx"),
                                    Variable(first_field.affixed_name),
                                ],
                            ),
                            Add(
                                Variable("Ctx.Last"),
                                -Variable("Ctx.First"),
                                Number(1),
                            ),
                        ),
                        *[
                            Call(
                                "Invalid",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            )
                            for f in message.fields
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def __create_reset_procedure() -> UnitPart:
        """
        Reset the state and buffer bounds of the context.

        Buffer bounds that were set during the initialization of the context will not be preserved.
        The effect of this procedure is semantically equivalent to:

        ```
        Take_Buffer (Context, Buffer);
        Initialize (Context, Buffer);
        ```
        """

        specification = ProcedureSpecification(
            "Reset",
            [InOutParameter(["Ctx"], "Context")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Not(Constrained("Ctx")),
                                Call("Has_Buffer", [Variable("Ctx")]),
                            )
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Variable("Ctx.Buffer_First"),
                                        Variable("Ctx.Buffer_Last"),
                                    ]
                                ],
                                Equal(
                                    Variable("Ctx.First"),
                                    Call(
                                        const.TYPES * "To_First_Bit_Index",
                                        [Variable("Ctx.Buffer_First")],
                                    ),
                                ),
                                Equal(
                                    Variable("Ctx.Last"),
                                    Call(
                                        const.TYPES * "To_Last_Bit_Index",
                                        [Variable("Ctx.Buffer_Last")],
                                    ),
                                ),
                                Call("Initialized", [Variable("Ctx")]),
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        CallStatement(
                            "Reset",
                            [
                                Variable("Ctx"),
                                Call(const.TYPES_TO_FIRST_BIT_INDEX, [First("Ctx.Buffer")]),
                                Call(const.TYPES_TO_LAST_BIT_INDEX, [Last("Ctx.Buffer")]),
                            ],
                        )
                    ],
                )
            ],
        )

    @staticmethod
    def __create_restricted_reset_procedure(message: Message) -> UnitPart:
        specification = ProcedureSpecification(
            "Reset",
            [
                InOutParameter(["Ctx"], "Context"),
                Parameter(["First"], const.TYPES_BIT_INDEX),
                Parameter(["Last"], const.TYPES_BIT_LENGTH),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Not(Constrained("Ctx")),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                GreaterEqual(
                                    Call(const.TYPES_TO_INDEX, [Variable("First")]),
                                    Variable("Ctx.Buffer_First"),
                                ),
                                LessEqual(
                                    Call(const.TYPES_TO_INDEX, [Variable("Last")]),
                                    Variable("Ctx.Buffer_Last"),
                                ),
                                LessEqual(Variable("First"), Add(Variable("Last"), Number(1))),
                                Less(Variable("Last"), Last(const.TYPES_BIT_LENGTH)),
                                Equal(Mod(Variable("First"), Size(const.TYPES_BYTE)), Number(1)),
                                Equal(Mod(Variable("Last"), Size(const.TYPES_BYTE)), Number(0)),
                            )
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Variable("Ctx.Buffer_First"),
                                        Variable("Ctx.Buffer_Last"),
                                    ]
                                ],
                                Equal(Variable("Ctx.First"), Variable("First")),
                                Equal(Variable("Ctx.Last"), Variable("Last")),
                                Call("Initialized", [Variable("Ctx")]),
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        Assignment(
                            "Ctx",
                            Aggregate(
                                Variable("Ctx.Buffer_First"),
                                Variable("Ctx.Buffer_Last"),
                                Variable("First"),
                                Variable("Last"),
                                Sub(Variable("First"), Number(1)),
                                Variable("Ctx.Buffer"),
                                context_cursors_initialization(message),
                            ),
                        ),
                    ],
                )
            ],
        )

    @staticmethod
    def __create_take_buffer_procedure() -> UnitPart:
        specification = ProcedureSpecification(
            "Take_Buffer",
            [InOutParameter(["Ctx"], "Context"), OutParameter(["Buffer"], const.TYPES_BYTES_PTR)],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(Call("Has_Buffer", [Variable("Ctx")])),
                        Postcondition(
                            And(
                                Not(Call("Has_Buffer", [Variable("Ctx")])),
                                NotEqual(Variable("Buffer"), NULL),
                                Equal(Variable("Ctx.Buffer_First"), First("Buffer")),
                                Equal(Variable("Ctx.Buffer_Last"), Last("Buffer")),
                                *const.CONTEXT_INVARIANT,
                                *[
                                    Equal(e, Old(e))
                                    for e in [Call("Context_Cursors", [Variable("Ctx")])]
                                ],
                            )
                        ),
                        Depends({"Ctx": ["Ctx"], "Buffer": ["Ctx"]}),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [Assignment("Buffer", Variable("Ctx.Buffer")), Assignment("Ctx.Buffer", NULL)],
                )
            ],
        )

    @staticmethod
    def __create_copy_procedure() -> UnitPart:
        specification = ProcedureSpecification(
            "Copy",
            [Parameter(["Ctx"], "Context"), OutParameter(["Buffer"], const.TYPES_BYTES)],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Structural_Valid_Message", [Variable("Ctx")]),
                                Equal(Call("Byte_Size", [Variable("Ctx")]), Length("Buffer")),
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        IfStatement(
                            [
                                (
                                    Greater(Length("Buffer"), Number(0)),
                                    [
                                        Assignment(
                                            "Buffer",
                                            Indexed(
                                                Variable("Ctx.Buffer.all"),
                                                ValueRange(
                                                    Call(
                                                        const.TYPES_TO_INDEX,
                                                        [Variable("Ctx.First")],
                                                    ),
                                                    Call(
                                                        const.TYPES_TO_INDEX,
                                                        [Variable("Ctx.Message_Last")],
                                                    ),
                                                ),
                                            ),
                                        )
                                    ],
                                ),
                            ],
                            [
                                Assignment(
                                    "Buffer",
                                    Indexed(
                                        Variable("Ctx.Buffer.all"),
                                        ValueRange(
                                            Last(const.TYPES_INDEX), First(const.TYPES_INDEX)
                                        ),
                                    ),
                                )
                            ],
                        )
                    ],
                )
            ],
        )

    @staticmethod
    def __create_read_procedure() -> UnitPart:
        specification = ProcedureSpecification(
            "Read",
            [Parameter(["Ctx"], "Context")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Structural_Valid_Message", [Variable("Ctx")]),
                            )
                        ),
                    ],
                    [
                        FormalSubprogramDeclaration(
                            ProcedureSpecification(
                                "Read",
                                [Parameter(["Buffer"], const.TYPES_BYTES)],
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        CallStatement(
                            "Read",
                            [
                                Indexed(
                                    Variable("Ctx.Buffer.all"),
                                    ValueRange(
                                        Call(const.TYPES_TO_INDEX, [Variable("Ctx.First")]),
                                        Call(const.TYPES_TO_INDEX, [Variable("Ctx.Message_Last")]),
                                    ),
                                )
                            ],
                        )
                    ],
                )
            ],
        )

    @staticmethod
    def __create_write_procedure() -> UnitPart:
        """
        Write data into the buffer of the context using an externally provided subprogram.

        The complete buffer of the context can be overwritten. Buffer bounds that were set during
        the initialization of the context will not be considered or preserved.
        """
        specification = ProcedureSpecification(
            "Write",
            [InOutParameter(["Ctx"], "Context")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Not(Constrained("Ctx")),
                                Call("Has_Buffer", [Variable("Ctx")]),
                            )
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Variable("Ctx.Buffer_First"),
                                        Variable("Ctx.Buffer_Last"),
                                    ]
                                ],
                                Equal(
                                    Variable("Ctx.First"),
                                    Call(
                                        const.TYPES * "To_First_Bit_Index",
                                        [Variable("Ctx.Buffer_First")],
                                    ),
                                ),
                                Call("Initialized", [Variable("Ctx")]),
                            )
                        ),
                    ],
                    [
                        FormalSubprogramDeclaration(
                            ProcedureSpecification(
                                "Write",
                                [
                                    OutParameter(["Buffer"], const.TYPES_BYTES),
                                    OutParameter(["Length"], const.TYPES_LENGTH),
                                ],
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [ObjectDeclaration(["Length"], const.TYPES_LENGTH)],
                    [
                        CallStatement(
                            "Write",
                            [
                                Variable("Ctx.Buffer.all"),
                                Variable("Length"),
                            ],
                        ),
                        # ISSUE: Componolit/Workarounds#39
                        # Improve the check message in case of a wrong instantiation of "Write".
                        PragmaStatement(
                            "Assert",
                            [
                                LessEqual(
                                    Variable("Length"),
                                    Length(
                                        Variable("Ctx.Buffer.all"),
                                    ),
                                ),
                                String(
                                    "Length <= Buffer'Length is not ensured by postcondition of"
                                    ' "Write"'
                                ),
                            ],
                        ),
                        CallStatement(
                            "Reset",
                            [
                                Variable("Ctx"),
                                Call(
                                    const.TYPES_TO_FIRST_BIT_INDEX, [Variable("Ctx.Buffer_First")]
                                ),
                                Call(
                                    const.TYPES_TO_LAST_BIT_INDEX,
                                    [
                                        Add(
                                            Call(
                                                const.TYPES_LENGTH,
                                                [Variable("Ctx.Buffer_First")],
                                            ),
                                            Variable("Length"),
                                            -Number(1),
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                )
            ],
        )

    def __create_path_condition_function(self, message: Message) -> UnitPart:
        def condition(field: Field, message: Message) -> Expr:
            cases: ty.List[ty.Tuple[Expr, Expr]] = [
                (
                    target,
                    expr.Or(*[c for _, c in conditions])
                    .substituted(common.substitution(message, self.__prefix))
                    .simplified()
                    .ada_expr(),
                )
                for target, conditions in itertools.groupby(
                    [
                        (Variable(l.target.affixed_name), l.condition)
                        for l in message.outgoing(field)
                        if l.target != FINAL
                    ],
                    lambda x: x[0],
                )
            ]
            if set(message.fields) - {l.target for l in message.outgoing(field)}:
                cases.append((Variable("others"), FALSE))
            return Case(Variable("Fld"), cases)

        specification = FunctionSpecification(
            "Path_Condition",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Call("Valid_Predecessor", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"),
                        [
                            (Variable(f.affixed_name), condition(f, message))
                            for f in message.all_fields
                        ],
                    ),
                )
            ],
        )

    def __create_field_size_function(self, message: Message) -> UnitPart:
        def size(field: Field, message: Message) -> Expr:
            def substituted(expression: expr.Expr) -> Expr:
                return (
                    expression.substituted(
                        common.substitution(
                            message, self.__prefix, target_type=const.TYPES_BIT_LENGTH
                        )
                    )
                    .simplified()
                    .ada_expr()
                )

            target_links = [
                (target, list(links))
                for target, links in itertools.groupby(message.outgoing(field), lambda x: x.target)
                if target != FINAL
            ]
            cases: ty.List[ty.Tuple[Expr, Expr]] = []
            for target, links in target_links:
                field_type = message.types[target]
                size: Expr
                if isinstance(field_type, Scalar):
                    size = Size(self.__prefix * common.full_base_type_name(field_type))
                else:
                    if len(links) == 1:
                        size = substituted(links[0].size)
                    else:
                        size = If(
                            [(substituted(l.condition), substituted(l.size)) for l in links],
                            const.UNREACHABLE,
                        )
                cases.append(
                    (
                        Variable(target.affixed_name),
                        size,
                    )
                )

            if not cases:
                return Number(0)

            if set(message.fields) - {l.target for l in message.outgoing(field)}:
                cases.append((Variable("others"), const.UNREACHABLE))
            return Case(Variable("Fld"), cases)

        specification = FunctionSpecification(
            "Field_Size",
            const.TYPES_BIT_LENGTH,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"),
                        [(Variable(f.affixed_name), size(f, message)) for f in message.all_fields],
                    ),
                )
            ],
        )

    def __create_field_first_function(self, message: Message) -> UnitPart:
        def first(field: Field, message: Message) -> Expr:
            def substituted(
                expression: expr.Expr, target_type: ty.Optional[ID] = const.TYPES_U64
            ) -> Expr:
                return (
                    expression.substituted(
                        common.substitution(message, self.__prefix, target_type=target_type)
                    )
                    .simplified()
                    .ada_expr()
                )

            if field == message.fields[0]:
                return Variable("Ctx.First")

            contiguous_first = expr.Add(
                expr.Selected(
                    expr.Indexed(
                        expr.Variable(expr.ID("Ctx") * "Cursors"),
                        expr.Selected(
                            expr.Indexed(
                                expr.Variable(expr.ID("Ctx") * "Cursors"), expr.Variable("Fld")
                            ),
                            "Predecessor",
                        ),
                    ),
                    "Last",
                ),
                expr.Number(1),
            )

            return If(
                [
                    (
                        AndThen(
                            Equal(
                                Selected(
                                    Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                                    "Predecessor",
                                ),
                                Variable(l.source.affixed_name),
                            ),
                            *([substituted(l.condition)] if l.condition != expr.TRUE else []),
                        ),
                        substituted(
                            l.first.substituted(
                                lambda x: contiguous_first if x == expr.UNDEFINED else x
                            ),
                            target_type=None,
                        ),
                    )
                    for l in message.incoming(field)
                ],
                const.UNREACHABLE,
            )

        specification = FunctionSpecification(
            "Field_First",
            const.TYPES_BIT_INDEX,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [(Variable(f.affixed_name), first(f, message)) for f in message.fields],
                    ),
                )
            ],
        )

    @staticmethod
    def __create_field_last_function() -> UnitPart:
        specification = FunctionSpecification(
            "Field_Last",
            const.TYPES_BIT_INDEX,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                                GreaterEqual(
                                    Call("Available_Space", [Variable("Ctx"), Variable("Fld")]),
                                    Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                                ),
                            )
                        )
                    ],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Add(
                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                        -Number(1),
                    ),
                )
            ],
        )

    def __create_field_condition_function(self, message: Message) -> UnitPart:
        def condition(field: Field, message: Message) -> Expr:
            c: expr.Expr = expr.Or(*[l.condition for l in message.outgoing(field)])
            c = c.substituted(
                mapping={
                    expr.Size(field.name): expr.Call(const.TYPES_U64, [expr.Variable("Size")]),
                    expr.Last(field.name): expr.Call(
                        const.TYPES_U64,
                        [
                            expr.Call(
                                "Field_Last",
                                [
                                    expr.Variable("Ctx"),
                                    expr.Variable(field.affixed_name, immutable=True),
                                ],
                            )
                        ],
                    ),
                    # ISSUE: Componolit/RecordFlux#276
                    **{expr.ValidChecksum(f): expr.TRUE for f in message.checksums},
                }
            )
            if field not in (INITIAL, FINAL):
                if isinstance(message.types[field], Scalar):
                    c = c.substituted(
                        lambda x: expr.Call(
                            const.TYPES_U64, [expr.Variable(expr.ID("Val") * f"{field.name}_Value")]
                        )
                        if x == expr.Variable(field.name)
                        else x
                    )
                elif isinstance(
                    message.types[field], Composite
                ) and common.is_compared_to_aggregate(field, message):
                    c = c.substituted(
                        lambda x: expr.Variable(expr.ID("Val") * f"{field.name}_Value")
                        if x == expr.Variable(field.name)
                        else x
                    )
            return (
                c.substituted(common.substitution(message, self.__prefix)).simplified().ada_expr()
            )

        parameters = [Parameter(["Ctx"], "Context"), Parameter(["Val"], "Field_Dependent_Value")]

        if common.size_dependent_condition(message):
            parameters.append(Parameter(["Size"], const.TYPES_BIT_LENGTH, Number(0)))

        specification = FunctionSpecification("Field_Condition", "Boolean", parameters)

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                In(Variable("Val.Fld"), Range("Field")),
                                Call("Valid_Predecessor", [Variable("Ctx"), Variable("Val.Fld")]),
                            )
                        )
                    ],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Val.Fld"),
                        [
                            (Variable(f.affixed_name), condition(f, message))
                            for f in message.all_fields
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def __create_predecessor_function() -> UnitPart:
        specification = FunctionSpecification(
            "Predecessor",
            "Virtual_Field",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Virtual_Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (
                                Variable(INITIAL.affixed_name),
                                Variable(INITIAL.affixed_name),
                            ),
                            (
                                Variable("others"),
                                Selected(
                                    Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"
                                ),
                            ),
                        ],
                    ),
                )
            ],
        )

    def __create_successor_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Successor",
            "Virtual_Field",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [],
            [
                # WORKAROUND: Componolit/Workarounds#31
                Pragma("Warnings", [Variable("Off"), String("precondition is always False")]),
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (
                                Variable(f.affixed_name),
                                If(
                                    [
                                        (
                                            l.condition.substituted(
                                                common.substitution(message, self.__prefix)
                                            )
                                            .simplified()
                                            .ada_expr(),
                                            Variable(l.target.affixed_name),
                                        )
                                        for l in message.outgoing(f)
                                    ],
                                    Variable(INITIAL.affixed_name),
                                ),
                            )
                            for f in message.fields
                        ],
                    ),
                    [
                        Precondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Structural_Valid", [Variable("Ctx"), Variable("Fld")]),
                                Call("Valid_Predecessor", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                ),
                Pragma("Warnings", [Variable("On"), String("precondition is always False")]),
            ],
        )

    @staticmethod
    def __create_invalid_successor_function(message: Message) -> UnitPart:
        if len(message.fields) == 1:
            return UnitPart()

        specification = FunctionSpecification(
            "Invalid_Successor",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (
                                Variable(f.affixed_name),
                                And(
                                    *[
                                        Call(
                                            "Invalid",
                                            [
                                                Indexed(
                                                    Variable("Ctx.Cursors"),
                                                    Variable(s.affixed_name),
                                                )
                                            ],
                                        )
                                        for s in message.direct_successors(f)
                                        if s != FINAL
                                    ]
                                ),
                            )
                            for f in message.fields
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def __create_reset_dependent_fields_procedure(message: Message) -> UnitPart:
        specification = ProcedureSpecification(
            "Reset_Dependent_Fields",
            [InOutParameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        field_location_invariant = PragmaStatement(
            "Assert",
            [
                And(
                    Equal(
                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        Variable("First"),
                    ),
                    Equal(
                        Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                        Variable("Size"),
                    ),
                )
            ],
        )

        return UnitPart(
            [],
            [
                SubprogramBody(
                    specification,
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_BIT_LENGTH,
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                            constant=True,
                            aspects=[Ghost()],
                        ),
                        ObjectDeclaration(
                            ["Size"],
                            const.TYPES_BIT_LENGTH,
                            Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                            constant=True,
                            aspects=[Ghost()],
                        ),
                    ],
                    [
                        field_location_invariant,
                        CaseStatement(
                            Variable("Fld"),
                            [
                                (
                                    Variable(f.affixed_name),
                                    ty.cast(ty.List[Statement], [])
                                    + [
                                        Assignment(
                                            Indexed(
                                                Variable("Ctx.Cursors"),
                                                Variable(s.affixed_name),
                                            ),
                                            Aggregate(
                                                Variable("S_Invalid"),
                                                Variable(FINAL.affixed_name),
                                            ),
                                        )
                                        for s in reversed(message.successors(f))
                                    ]
                                    + [
                                        Assignment(
                                            Indexed(
                                                Variable("Ctx.Cursors"),
                                                Variable(f.affixed_name),
                                            ),
                                            Aggregate(
                                                Variable("S_Invalid"),
                                                Selected(
                                                    Indexed(
                                                        Variable("Ctx.Cursors"),
                                                        Variable(f.affixed_name),
                                                    ),
                                                    "Predecessor",
                                                ),
                                            ),
                                        )
                                    ]
                                    + [field_location_invariant],
                                )
                                for f in message.fields
                            ],
                        ),
                    ],
                    [
                        Precondition(
                            And(
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            ),
                        ),
                        Postcondition(
                            And(
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                                Call(
                                    "Invalid",
                                    [Indexed(Variable("Ctx.Cursors"), Variable("Fld"))],
                                ),
                                *(
                                    [
                                        Call(
                                            "Invalid_Successor",
                                            [Variable("Ctx"), Variable("Fld")],
                                        )
                                    ]
                                    if len(message.fields) > 1
                                    else []
                                ),
                                *const.CONTEXT_INVARIANT,
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected(
                                            Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                                            "Predecessor",
                                        ),
                                        Call("Has_Buffer", [Variable("Ctx")]),
                                        Call(
                                            "Field_First",
                                            [Variable("Ctx"), Variable("Fld")],
                                        ),
                                        Call(
                                            "Field_Size",
                                            [Variable("Ctx"), Variable("Fld")],
                                        ),
                                    ]
                                ],
                                Case(
                                    Variable("Fld"),
                                    [
                                        (
                                            Variable(f.affixed_name),
                                            And(
                                                *[
                                                    Equal(
                                                        Indexed(
                                                            Variable("Ctx.Cursors"),
                                                            Variable(p.affixed_name),
                                                        ),
                                                        Old(
                                                            Indexed(
                                                                Variable("Ctx.Cursors"),
                                                                Variable(p.affixed_name),
                                                            )
                                                        ),
                                                    )
                                                    for p in message.predecessors(f)
                                                ],
                                                *[
                                                    Call(
                                                        "Invalid",
                                                        [
                                                            Variable("Ctx"),
                                                            Variable(s.affixed_name),
                                                        ],
                                                    )
                                                    for s in [f, *message.successors(f)]
                                                ],
                                            ),
                                        )
                                        for f in message.fields
                                    ],
                                ),
                            )
                        ),
                    ],
                )
            ],
        )

    @staticmethod
    def __create_has_buffer_function() -> UnitPart:
        specification = FunctionSpecification(
            "Has_Buffer", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(specification, NotEqual(Variable("Ctx.Buffer"), NULL))
            ],
        )

    @staticmethod
    def __create_valid_predecessor_function(
        message: Message, composite_fields: ty.Sequence[Field]
    ) -> UnitPart:

        specification = FunctionSpecification(
            "Valid_Predecessor",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Virtual_Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (
                                Variable(f.affixed_name),
                                Or(
                                    *[
                                        expr.And(
                                            expr.Call(
                                                "Structural_Valid"
                                                if p in composite_fields
                                                else "Valid",
                                                [
                                                    expr.Indexed(
                                                        expr.Variable(expr.ID("Ctx") * "Cursors"),
                                                        expr.Variable(p.affixed_name),
                                                    )
                                                ],
                                            )
                                            if p != INITIAL
                                            else expr.TRUE,
                                            expr.Equal(
                                                expr.Selected(
                                                    expr.Indexed(
                                                        expr.Variable(expr.ID("Ctx") * "Cursors"),
                                                        expr.Variable("Fld"),
                                                    ),
                                                    "Predecessor",
                                                ),
                                                expr.Variable(p.affixed_name),
                                            ),
                                        )
                                        .simplified()
                                        .ada_expr()
                                        for p in message.direct_predecessors(f)
                                    ]
                                ),
                            )
                            for f in message.all_fields
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def __create_size_function() -> UnitPart:
        specification = FunctionSpecification(
            "Size", const.TYPES_BIT_LENGTH, [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    If(
                        [
                            (
                                Equal(
                                    Variable("Ctx.Message_Last"),
                                    Sub(Variable("Ctx.First"), Number(1)),
                                ),
                                Number(0),
                            )
                        ],
                        Add(
                            Variable("Ctx.Message_Last"),
                            -Variable("Ctx.First"),
                            Number(1),
                        ),
                    ),
                )
            ],
        )

    @staticmethod
    def __create_byte_size_function() -> UnitPart:
        specification = FunctionSpecification(
            "Byte_Size", const.TYPES_LENGTH, [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    If(
                        [
                            (
                                Equal(
                                    Variable("Ctx.Message_Last"),
                                    Sub(Variable("Ctx.First"), Number(1)),
                                ),
                                Number(0),
                            )
                        ],
                        Indexed(
                            Variable(const.TYPES_LENGTH),
                            Add(
                                Call(const.TYPES_TO_INDEX, [Variable("Ctx.Message_Last")]),
                                -Call(const.TYPES_TO_INDEX, [Variable("Ctx.First")]),
                                Number(1),
                            ),
                        ),
                    ),
                )
            ],
        )

    @staticmethod
    def __create_message_last_function() -> UnitPart:
        specification = FunctionSpecification(
            "Message_Last", const.TYPES_BIT_LENGTH, [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Structural_Valid_Message", [Variable("Ctx")]),
                            )
                        ),
                    ],
                ),
            ],
            private=[ExpressionFunctionDeclaration(specification, Variable("Ctx.Message_Last"))],
        )

    @staticmethod
    def __create_message_data_function() -> UnitPart:
        specification = FunctionSpecification(
            "Message_Data", const.TYPES_BYTES, [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Structural_Valid_Message", [Variable("Ctx")]),
                            )
                        ),
                        Postcondition(
                            Equal(
                                Length(Result(specification.identifier)),
                                Call("Byte_Size", [Variable("Ctx")]),
                            )
                        ),
                    ],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Slice(
                        Variable("Ctx.Buffer.all"),
                        Call(const.TYPES_TO_INDEX, [Variable("Ctx.First")]),
                        Call(const.TYPES_TO_INDEX, [Variable("Ctx.Message_Last")]),
                    ),
                )
            ],
        )

    @staticmethod
    def __create_available_space_function() -> UnitPart:
        specification = FunctionSpecification(
            "Available_Space",
            const.TYPES_BIT_LENGTH,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [Precondition(Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]))],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Add(
                        Variable("Ctx.Last"),
                        -Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        Number(1),
                    ),
                )
            ],
        )

    @staticmethod
    def __create_sufficient_buffer_length_function() -> UnitPart:
        return UnitPart(
            [],
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Sufficient_Buffer_Length",
                        "Boolean",
                        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
                    ),
                    And(
                        NotEqual(Variable("Ctx.Buffer"), Variable("null")),
                        Less(
                            Add(
                                Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                                Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                            ),
                            Last(const.TYPES_BIT_LENGTH),
                        ),
                        LessEqual(
                            Variable("Ctx.First"),
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        ),
                        GreaterEqual(
                            Call("Available_Space", [Variable("Ctx"), Variable("Fld")]),
                            Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                        ),
                    ),
                    [
                        Precondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                ),
            ],
        )

    @staticmethod
    def __create_equal_function(
        scalar_fields: ty.Mapping[Field, Type],
        composite_fields: ty.Sequence[Field],
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Equal",
            "Boolean",
            [
                Parameter(["Ctx"], "Context"),
                Parameter(["Fld"], "Field"),
                Parameter(["Data"], const.TYPES_BYTES),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        Call("Sufficient_Buffer_Length", [Variable("Ctx"), Variable("Fld")]),
                        Case(
                            Variable("Fld"),
                            [
                                *[
                                    (
                                        Variable(f.affixed_name),
                                        Equal(
                                            Indexed(
                                                Variable("Ctx.Buffer.all"),
                                                ValueRange(
                                                    Call(
                                                        const.TYPES_TO_INDEX,
                                                        [
                                                            Call(
                                                                "Field_First",
                                                                [
                                                                    Variable("Ctx"),
                                                                    Variable("Fld"),
                                                                ],
                                                            )
                                                        ],
                                                    ),
                                                    Call(
                                                        const.TYPES_TO_INDEX,
                                                        [
                                                            Call(
                                                                "Field_Last",
                                                                [
                                                                    Variable("Ctx"),
                                                                    Variable("Fld"),
                                                                ],
                                                            )
                                                        ],
                                                    ),
                                                ),
                                            ),
                                            Variable("Data"),
                                        ),
                                    )
                                    for f in composite_fields
                                ],
                                *([(Variable("others"), FALSE)] if scalar_fields else []),
                            ],
                        ),
                    ),
                )
            ],
        )

    def __create_switch_procedures(
        self, message: Message, sequence_fields: ty.Mapping[Field, Type]
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Switch_To_{field.name}",
                [
                    InOutParameter(["Ctx"], "Context"),
                    OutParameter(["Seq_Ctx"], f"{common.sequence_name(message, field)}.Context"),
                ],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained("Ctx")),
                                Not(Constrained("Seq_Ctx")),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call(
                                    "Valid_Next",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                Greater(
                                    Call(
                                        "Field_Size",
                                        [Variable("Ctx"), Variable(f.affixed_name)],
                                    ),
                                    Number(0),
                                ),
                                byte_aligned_field(f),
                                Call(
                                    "Field_Condition",
                                    [
                                        Variable("Ctx"),
                                        NamedAggregate(("Fld", Variable(f.affixed_name))),
                                    ]
                                    + (
                                        [
                                            Call(
                                                "Field_Size",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(f.affixed_name),
                                                ],
                                            ),
                                        ]
                                        if common.size_dependent_condition(message)
                                        else []
                                    ),
                                ),
                                common.sufficient_space_for_field_condition(
                                    Variable(f.affixed_name)
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                *switch_update_conditions(message, f),
                                Call(
                                    f"{common.sequence_name(message, f)}.Valid",
                                    [Variable("Seq_Ctx")],
                                ),
                                Equal(
                                    Call(
                                        f"{common.sequence_name(message, f)}.Sequence_Last",
                                        [Variable("Seq_Ctx")],
                                    ),
                                    Sub(Variable("Seq_Ctx.First"), Number(1)),
                                ),
                                Call(
                                    "Present",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                *const.CONTEXT_INVARIANT,
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Call(
                                            "Predecessor",
                                            [
                                                Variable("Ctx"),
                                                Variable(f.affixed_name),
                                            ],
                                        ),
                                        Call(
                                            "Path_Condition",
                                            [
                                                Variable("Ctx"),
                                                Variable(f.affixed_name),
                                            ],
                                        ),
                                    ]
                                    + [
                                        Call(
                                            "Context_Cursor",
                                            [
                                                Variable("Ctx"),
                                                Variable(p.affixed_name),
                                            ],
                                        )
                                        for p in message.predecessors(f)
                                    ]
                                ],
                            )
                        ),
                        ContractCases(
                            (
                                Call(
                                    "Structural_Valid",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                And(
                                    *[
                                        Equal(
                                            Call(
                                                "Context_Cursor",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(s.affixed_name),
                                                ],
                                            ),
                                            Old(
                                                Call(
                                                    "Context_Cursor",
                                                    [
                                                        Variable("Ctx"),
                                                        Variable(s.affixed_name),
                                                    ],
                                                )
                                            ),
                                        )
                                        for s in message.successors(f)
                                    ]
                                ),
                            ),
                            (
                                Variable("others"),
                                And(
                                    *common.valid_path_to_next_field_condition(
                                        message, f, self.__prefix
                                    ),
                                    *[
                                        Call(
                                            "Invalid",
                                            [
                                                Variable("Ctx"),
                                                Variable(s.affixed_name),
                                            ],
                                        )
                                        for s in message.successors(f)
                                    ],
                                ),
                            ),
                        ),
                    ],
                )
                for f, t in sequence_fields.items()
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        *common.field_bit_location_declarations(Variable(f.affixed_name)),
                        ObjectDeclaration(["Buffer"], const.TYPES_BYTES_PTR),
                    ],
                    [
                        IfStatement(
                            [
                                (
                                    Call(
                                        "Invalid",
                                        [Variable("Ctx"), Variable(f.affixed_name)],
                                    ),
                                    common.initialize_field_statements(message, f),
                                )
                            ]
                        ),
                        CallStatement("Take_Buffer", [Variable("Ctx"), Variable("Buffer")]),
                        PragmaStatement(
                            "Warnings",
                            [Variable("Off"), String('unused assignment to "Buffer"')],
                        ),
                        CallStatement(
                            f"{common.sequence_name(message, f)}.Initialize",
                            [
                                Variable("Seq_Ctx"),
                                Variable("Buffer"),
                                Variable("First"),
                                Variable("Last"),
                            ],
                        ),
                        PragmaStatement(
                            "Warnings",
                            [Variable("On"), String('unused assignment to "Buffer"')],
                        ),
                    ],
                )
                for f in sequence_fields
            ],
        )

    @staticmethod
    def __create_complete_functions(
        message: Message, sequence_fields: ty.Mapping[Field, Type]
    ) -> UnitPart:
        def specification(field: Field) -> FunctionSpecification:
            return FunctionSpecification(
                f"Complete_{field.name}",
                "Boolean",
                [
                    Parameter(["Ctx"], "Context"),
                    Parameter(["Seq_Ctx"], f"{common.sequence_name(message, field)}.Context"),
                ],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            Call("Valid_Next", [Variable("Ctx"), Variable(f.affixed_name)]),
                        )
                    ],
                )
                for f in sequence_fields
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification(f),
                    And(
                        Call(
                            f"{common.sequence_name(message, f)}.Valid",
                            [Variable("Seq_Ctx")],
                        ),
                        Equal(
                            Call(
                                f"{common.sequence_name(message, f)}.Size",
                                [Variable("Seq_Ctx")],
                            ),
                            Call(
                                "Field_Size",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            ),
                        ),
                    ),
                )
                for f in sequence_fields
            ],
        )

    @staticmethod
    def __create_update_procedures(
        message: Message, sequence_fields: ty.Mapping[Field, Type]
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Update_{field.name}",
                [
                    InOutParameter(["Ctx"], "Context"),
                    InOutParameter(["Seq_Ctx"], f"{common.sequence_name(message, field)}.Context"),
                ],
            )

        def take_buffer_arguments(field: Field) -> ty.Sequence[Expr]:
            arguments = [
                Variable("Seq_Ctx"),
                Variable("Buffer"),
            ]

            field_type = message.types[field]
            assert isinstance(field_type, Sequence)

            return arguments

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                Call(
                                    "Present",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                Call(f"Complete_{f.name}", [Variable("Ctx"), Variable("Seq_Ctx")]),
                                *switch_update_conditions(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                Call(
                                    "Present",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Not(
                                    Call(
                                        f"{common.sequence_name(message, f)}.Has_Buffer",
                                        [Variable("Seq_Ctx")],
                                    )
                                ),
                                *const.CONTEXT_INVARIANT,
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Variable("Seq_Ctx.First"),
                                        Variable("Seq_Ctx.Last"),
                                        Call(
                                            "Field_First",
                                            [
                                                Variable("Ctx"),
                                                Variable(f.affixed_name),
                                            ],
                                        ),
                                        Call(
                                            "Field_Size",
                                            [
                                                Variable("Ctx"),
                                                Variable(f.affixed_name),
                                            ],
                                        ),
                                    ]
                                    + [
                                        Call(
                                            "Context_Cursor",
                                            [
                                                Variable("Ctx"),
                                                Variable(o.affixed_name),
                                            ],
                                        )
                                        for o in message.fields
                                        if o != f
                                    ]
                                ],
                            )
                        ),
                        Depends({"Ctx": ["Ctx", "Seq_Ctx"], "Seq_Ctx": ["Seq_Ctx"]}),
                    ],
                )
                for f, t in sequence_fields.items()
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["Valid_Sequence"],
                            "Boolean",
                            Call(
                                f"{common.sequence_name(message, f)}.Valid", [Variable("Seq_Ctx")]
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(["Buffer"], const.TYPES_BYTES_PTR),
                    ],
                    [
                        CallStatement(
                            f"{common.sequence_name(message, f)}.Take_Buffer",
                            take_buffer_arguments(f),
                        ),
                        Assignment("Ctx.Buffer", Variable("Buffer")),
                        IfStatement(
                            [
                                (
                                    Variable("Valid_Sequence"),
                                    [
                                        Assignment(
                                            Indexed(
                                                Variable("Ctx.Cursors"),
                                                Variable(f.affixed_name),
                                            ),
                                            NamedAggregate(
                                                ("State", Variable("S_Valid")),
                                                *[
                                                    (
                                                        a,
                                                        Selected(
                                                            Indexed(
                                                                Variable("Ctx.Cursors"),
                                                                Variable(f.affixed_name),
                                                            ),
                                                            a,
                                                        ),
                                                    )
                                                    for a in (
                                                        "First",
                                                        "Last",
                                                        "Value",
                                                        "Predecessor",
                                                    )
                                                ],
                                            ),
                                        )
                                    ],
                                )
                            ]
                        ),
                    ],
                )
                for f in sequence_fields
            ],
        )

    @staticmethod
    def __create_cursor_validation_functions() -> UnitPart:
        parameters = [Parameter(["Cursor"], "Field_Cursor")]

        return UnitPart(
            [],
            [],
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Structural_Valid", "Boolean", parameters),
                    Or(
                        Equal(Variable("Cursor.State"), Variable("S_Valid")),
                        Equal(Variable("Cursor.State"), Variable("S_Structural_Valid")),
                    ),
                ),
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Valid", "Boolean", parameters),
                    Equal(Variable("Cursor.State"), Variable("S_Valid")),
                ),
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Invalid", "Boolean", parameters),
                    Or(
                        Equal(Variable("Cursor.State"), Variable("S_Invalid")),
                        Equal(Variable("Cursor.State"), Variable("S_Incomplete")),
                    ),
                ),
            ],
        )

    def __create_valid_context_function(
        self, message: Message, composite_fields: ty.Sequence[Field]
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Context",
            "Boolean",
            [
                Parameter(["Buffer_First", "Buffer_Last"], const.TYPES_INDEX),
                Parameter(["First"], const.TYPES_BIT_INDEX),
                Parameter(["Last"], const.TYPES_BIT_LENGTH),
                Parameter(["Message_Last"], const.TYPES_BIT_LENGTH),
                Parameter(["Buffer"], const.TYPES_BYTES_PTR),
                Parameter(["Cursors"], "Field_Cursors"),
            ],
        )

        return UnitPart(
            [],
            [],
            [
                # WORKAROUND: Componolit/Workarounds#36
                # An access constant type cannot be used here, because the "implicit conversion
                # between access types with different designated types is not yet supported".
                Pragma(
                    "Warnings",
                    [
                        Variable("Off"),
                        String('"Buffer" is not modified, could be of access constant type'),
                    ],
                ),
                ExpressionFunctionDeclaration(
                    specification,
                    common.context_predicate(message, composite_fields, self.__prefix),
                ),
                Pragma(
                    "Warnings",
                    [
                        Variable("On"),
                        String('"Buffer" is not modified, could be of access constant type'),
                    ],
                ),
            ],
        )

    @staticmethod
    def __create_cursor_function() -> UnitPart:
        specification = FunctionSpecification(
            "Context_Cursor",
            "Field_Cursor",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification, [Annotate("GNATprove", "Inline_For_Proof"), Ghost()]
                )
            ],
            [],
            [
                ExpressionFunctionDeclaration(
                    specification, Indexed(Variable("Ctx.Cursors"), Variable("Fld"))
                )
            ],
        )

    @staticmethod
    def __create_cursors_function() -> UnitPart:
        specification = FunctionSpecification(
            "Context_Cursors", "Field_Cursors", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification, [Annotate("GNATprove", "Inline_For_Proof"), Ghost()]
                )
            ],
            [],
            [ExpressionFunctionDeclaration(specification, Variable("Ctx.Cursors"))],
        )

    def __create_structure(self, message: Message) -> UnitPart:
        unit = UnitPart()
        unit += self.__create_structure_type(message)
        unit += self.__create_to_structure_procedure(message)
        unit += self.__create_to_context_procedure(message)
        return unit

    def __create_structure_type(self, message: Message) -> UnitPart:
        if not message.is_definite:
            return UnitPart()

        assert len(message.paths(FINAL)) == 1

        components = []

        for path in message.paths(FINAL):
            if any(isinstance(t, Opaque) for t in message.types.values()):
                field_size = message.max_field_sizes()

            for link in path:
                if link.target == FINAL:
                    continue

                type_ = message.types[link.target]

                component_type: ty.Union[ID, Expr]

                if isinstance(type_, Scalar):
                    component_type = common.prefixed_type_identifier(
                        ID(type_.identifier), self.__prefix
                    )
                elif isinstance(type_, Opaque):
                    component_type = Indexed(
                        Variable(const.TYPES_BYTES),
                        ValueRange(
                            First(const.TYPES_INDEX),
                            Add(
                                First(const.TYPES_INDEX),
                                expr.Sub(
                                    expr.Div(field_size[link.target], expr.Number(8)),
                                    expr.Number(1),
                                )
                                .simplified()
                                .ada_expr(),
                            ),
                        ),
                    )
                else:
                    return UnitPart()

                components.append(Component(ID(link.target.identifier), component_type))

        return UnitPart(
            [
                RecordType("Structure", components),
            ]
        )

    @staticmethod
    def __create_to_structure_procedure(message: Message) -> UnitPart:
        if not message.is_definite:
            return UnitPart()

        assert len(message.paths(FINAL)) == 1

        statements: ty.List[Statement] = []

        for path in message.paths(FINAL):
            for link in path:
                if link.target == FINAL:
                    continue

                type_ = message.types[link.target]

                if isinstance(type_, Scalar):
                    statements.append(
                        Assignment(
                            Variable(f"Struct.{link.target.identifier}"),
                            Call(f"Get_{link.target.identifier}", [Variable("Ctx")]),
                        )
                    )
                elif isinstance(type_, Opaque):
                    statements.append(
                        CallStatement(
                            f"Get_{link.target.identifier}",
                            [Variable("Ctx"), Variable(f"Struct.{link.target.identifier}")],
                        )
                    )
                else:
                    return UnitPart()

        specification = ProcedureSpecification(
            "To_Structure",
            [Parameter(["Ctx"], "Context"), OutParameter(["Struct"], "Structure")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Structural_Valid_Message", [Variable("Ctx")]),
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(specification, [], statements),
            ],
        )

    @staticmethod
    def __create_to_context_procedure(message: Message) -> UnitPart:
        if not message.is_definite:
            return UnitPart()

        assert len(message.paths(FINAL)) == 1

        body: ty.List[Statement] = [CallStatement("Reset", [Variable("Ctx")])]
        statements = body

        for path in message.paths(FINAL):
            for link in path:
                if link.target == FINAL:
                    continue

                type_ = message.types[link.target]

                if isinstance(type_, (Scalar, Opaque)):
                    if isinstance(type_, Enumeration) and type_.always_valid:
                        dependent_statements: ty.List[Statement] = [
                            CallStatement(
                                f"Set_{link.target.identifier}",
                                [
                                    Variable("Ctx"),
                                    Variable(f"Struct.{link.target.identifier}.Enum"),
                                ],
                            )
                        ]
                        statements.append(
                            IfStatement(
                                [
                                    (
                                        Variable(f"Struct.{link.target.identifier}.Known"),
                                        dependent_statements,
                                    )
                                ]
                            )
                        )
                        statements = dependent_statements
                    elif isinstance(type_, Opaque) and link.size.variables():
                        field_size = f"{link.target.identifier}_Size"
                        dependent_statements = [
                            CallStatement(
                                f"Set_{link.target.identifier}",
                                [
                                    Variable("Ctx"),
                                    Slice(
                                        Variable(f"Struct.{link.target.identifier}"),
                                        First(f"Struct.{link.target.identifier}"),
                                        Add(
                                            First(f"Struct.{link.target.identifier}"),
                                            Call(
                                                const.TYPES_INDEX,
                                                [
                                                    Add(
                                                        Call(
                                                            const.TYPES_TO_LENGTH,
                                                            [Variable(field_size)],
                                                        ),
                                                        Number(1),
                                                    )
                                                ],
                                            ),
                                            Number(-2),
                                        ),
                                    ),
                                ],
                            )
                        ]
                        statements.append(
                            Declare(
                                [
                                    ObjectDeclaration(
                                        [field_size],
                                        const.TYPES_BIT_LENGTH,
                                        Call(
                                            const.TYPES_BIT_LENGTH,
                                            [
                                                link.size.substituted(
                                                    lambda x: expr.Variable("Struct" * x.identifier)
                                                    if isinstance(x, expr.Variable)
                                                    and Field(x.identifier) in message.fields
                                                    else x
                                                ).ada_expr()
                                            ],
                                        ),
                                        constant=True,
                                    )
                                ],
                                [
                                    IfStatement(
                                        [
                                            (
                                                Equal(
                                                    Variable(field_size),
                                                    Call(
                                                        "Field_Size",
                                                        [
                                                            Variable("Ctx"),
                                                            Variable(link.target.affixed_name),
                                                        ],
                                                    ),
                                                ),
                                                dependent_statements,
                                            )
                                        ]
                                    )
                                ],
                            )
                        )
                        statements = dependent_statements
                    else:
                        set_field = CallStatement(
                            f"Set_{link.target.identifier}",
                            [Variable("Ctx"), Variable(f"Struct.{link.target.identifier}")],
                        )
                        if common.is_compared_to_aggregate(link.target, message):
                            dependent_statements = [set_field]
                            statements.append(
                                IfStatement(
                                    [
                                        (
                                            common.field_condition_call(
                                                message,
                                                link.target,
                                                Variable(f"Struct.{link.target.identifier}"),
                                            ),
                                            dependent_statements,
                                        )
                                    ]
                                )
                            )
                            statements = dependent_statements
                        else:
                            statements.append(set_field)
                else:
                    return UnitPart()

        specification = ProcedureSpecification(
            "To_Context",
            [Parameter(["Struct"], "Structure"), InOutParameter(["Ctx"], "Context")],
        )
        first_field = message.fields[0]
        message_size = message.max_size()
        assert isinstance(message_size, expr.Number)

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained("Ctx")),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call(
                                    "Valid_Next",
                                    [Variable("Ctx"), Variable(first_field.affixed_name)],
                                ),
                                GreaterEqual(
                                    Call(
                                        "Available_Space",
                                        [
                                            Variable("Ctx"),
                                            Variable(first_field.affixed_name),
                                        ],
                                    ),
                                    message_size.ada_expr(),
                                ),
                            )
                        ),
                        Postcondition(
                            Call("Has_Buffer", [Variable("Ctx")]),
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(specification, [], body),
            ],
        )

    def __create_refinement(self, refinement: Refinement) -> None:
        unit_name = refinement.package * const.REFINEMENT_PACKAGE
        null_sdu = not refinement.sdu.fields

        if unit_name in self._units:
            unit = self._units[unit_name]
        else:
            unit = self.__create_unit(
                unit_name,
                [WithClause(self.__prefix * const.TYPES_PACKAGE)] if not null_sdu else [],
            )

        assert isinstance(unit, PackageUnit), "unexpected unit type"

        for v in refinement.condition.variables():
            if len(v.identifier.parts) == 2 and v.identifier.parent != refinement.package:
                unit.declaration_context.extend(
                    [
                        WithClause(self.__prefix * ID(v.identifier.parent)),
                        UsePackageClause(self.__prefix * ID(v.identifier.parent)),
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
                    WithClause(self.__prefix * ID(pdu_package)),
                    UsePackageClause(self.__prefix * ID(pdu_package)),
                ]
            )

        pdu_identifier = self.__prefix * ID(refinement.pdu.identifier)
        sdu_identifier = self.__prefix * ID(refinement.sdu.identifier)

        unit.declaration_context.append(WithClause(pdu_identifier))

        if not null_sdu:
            unit.declaration_context.extend(
                [
                    WithClause(sdu_identifier),
                    UsePackageClause(sdu_identifier),
                ]
            )

        condition_fields = {
            f: t
            for f, t in refinement.pdu.types.items()
            if expr.Variable(f.name) in refinement.condition
        }

        unit += self.__create_contains_function(refinement, condition_fields, null_sdu)
        if not null_sdu:
            unit += self.__create_switch_procedure(refinement, condition_fields)
            unit += self.__create_copy_refined_field_procedure(refinement, condition_fields)

    def __create_type(self, field_type: Type, message_package: ID) -> None:
        unit = self._units[message_package]

        assert field_type.package != BUILTINS_PACKAGE

        if isinstance(field_type, ModularInteger):
            unit += UnitPart(modular_types(field_type))
            unit += self.__integer_functions(field_type)
        elif isinstance(field_type, RangeInteger):
            unit += UnitPart(range_types(field_type))
            unit += self.__integer_functions(field_type)
        elif isinstance(field_type, Enumeration):
            unit += UnitPart(enumeration_types(field_type))
            unit += self.__enumeration_functions(field_type)
        elif isinstance(field_type, Sequence):
            self.__create_sequence_unit(field_type)
        else:
            assert False, f'unexpected type "{type(field_type).__name__}"'

    def __create_sequence_unit(self, sequence_type: Sequence) -> None:
        context, package = common.create_sequence_instantiation(sequence_type, self.__prefix)
        self.__create_instantiation_unit(
            package.identifier,
            [
                Pragma("SPARK_Mode"),
                *context,
                # WORKAROUND: Componolit/Workarounds#33
                # A compiler error about a non-visible declaration of RFLX_Types inside the
                # generic sequence package is prevented by adding a with-clause for this package.
                Pragma(
                    "Warnings",
                    [Variable("Off"), String('unit "*RFLX_Types" is not referenced')],
                ),
                WithClause(self.__prefix * const.TYPES_PACKAGE),
                Pragma(
                    "Warnings",
                    [Variable("On"), String('unit "*RFLX_Types" is not referenced')],
                ),
            ],
            package,
        )

    def __integer_functions(self, integer: Integer) -> UnitPart:
        specification: ty.List[Declaration] = []

        constraints = expr.And(*integer.constraints("Val")).simplified()

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

        specification.append(self.__type_validation_function(integer, constraints.ada_expr()))

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

        specification.extend(self.__integer_conversion_functions(integer))

        return UnitPart(specification)

    def __enumeration_functions(self, enum: Enumeration) -> UnitPart:
        incomplete = len(enum.literals) < 2 ** int(enum.size)

        specification: ty.List[Declaration] = []

        enum_value = Variable("Val")

        validation_expression: Expr
        if enum.always_valid:
            validation_expression = expr.And(*enum.constraints("Val")).simplified().ada_expr()
        else:
            validation_cases: ty.List[ty.Tuple[Expr, Expr]] = []
            validation_cases.extend(
                (value.ada_expr(), Variable("True")) for value in enum.literals.values()
            )
            if incomplete:
                validation_cases.append((Variable("others"), Variable("False")))

            validation_expression = Case(enum_value, validation_cases)

        if enum.always_valid:
            specification.extend(
                [
                    Pragma("Warnings", [Variable("Off"), String('unused variable "Val"')]),
                    Pragma(
                        "Warnings",
                        [Variable("Off"), String('formal parameter "Val" is not referenced')],
                    ),
                ]
            )
        specification.append(self.__type_validation_function(enum, validation_expression))
        if enum.always_valid:
            specification.extend(
                [
                    Pragma(
                        "Warnings",
                        [Variable("On"), String('formal parameter "Val" is not referenced')],
                    ),
                    Pragma("Warnings", [Variable("On"), String('unused variable "Val"')]),
                ]
            )

        specification.append(
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Base",
                    self.__prefix * common.full_base_type_name(enum),
                    [
                        Parameter(
                            ["Enum"],
                            self.__prefix
                            * (
                                ID(common.full_enum_name(enum))
                                if enum.always_valid
                                else ID(enum.identifier)
                            ),
                        )
                    ],
                ),
                Case(
                    Variable("Enum"),
                    [(Variable(ID(key)), value.ada_expr()) for key, value in enum.literals.items()],
                ),
            )
        )

        conversion_function = FunctionSpecification(
            "To_Actual",
            self.__prefix * ID(enum.identifier),
            [Parameter(["Val"], self.__prefix * common.full_base_type_name(enum))],
        )
        precondition = Precondition(Call("Valid", [Variable("Val")]))
        conversion_cases: ty.List[ty.Tuple[Expr, Expr]] = []

        if enum.always_valid:
            specification.append(
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "To_Actual",
                        self.__prefix * ID(enum.identifier),
                        [Parameter(["Enum"], common.enum_name(enum))],
                    ),
                    Aggregate(TRUE, Variable("Enum")),
                )
            )

            conversion_cases.extend(
                (value.ada_expr(), Aggregate(Variable("True"), Variable(ID(key))))
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
                        "To_Base",
                        self.__prefix * common.full_base_type_name(enum),
                        [Parameter(["Val"], self.__prefix * ID(enum.identifier))],
                    ),
                    If(
                        [(Variable("Val.Known"), Call("To_Base", [Variable("Val.Enum")]))],
                        Variable("Val.Raw"),
                    ),
                )
            )

        else:
            conversion_cases.extend(
                (value.ada_expr(), Variable(ID(key))) for key, value in enum.literals.items()
            )
            if incomplete:
                conversion_cases.append((Variable("others"), const.UNREACHABLE))

            specification.extend(
                [
                    Pragma("Warnings", [Variable("Off"), String("unreachable branch")]),
                    ExpressionFunctionDeclaration(
                        conversion_function, Case(enum_value, conversion_cases), [precondition]
                    ),
                    Pragma("Warnings", [Variable("On"), String("unreachable branch")]),
                ]
            )

        return UnitPart(specification)

    def __create_contains_function(
        self,
        refinement: Refinement,
        condition_fields: ty.Mapping[Field, Type],
        null_sdu: bool,
    ) -> SubprogramUnitPart:
        pdu_identifier = self.__prefix * ID(refinement.pdu.identifier)
        condition = refinement.condition
        for f, t in condition_fields.items():
            if isinstance(t, Enumeration) and t.always_valid:
                condition = expr.AndThen(
                    expr.Selected(
                        expr.Call(pdu_identifier * f"Get_{f.name}", [expr.Variable("Ctx")]), "Known"
                    ),
                    condition,
                )
        condition = condition.substituted(
            mapping={
                expr.Variable(f.name): expr.Selected(
                    expr.Call(pdu_identifier * f"Get_{f.name}", [expr.Variable("Ctx")]), "Enum"
                )
                if isinstance(t, Enumeration) and t.always_valid
                else expr.Call(pdu_identifier * f"Get_{f.name}", [expr.Variable("Ctx")])
                for f, t in condition_fields.items()
            }
        ).simplified()

        specification = FunctionSpecification(
            contains_function_name(refinement),
            "Boolean",
            [Parameter(["Ctx"], ID(pdu_identifier) * "Context")],
        )

        return SubprogramUnitPart(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    expr.AndThen(
                        *self.__refinement_conditions(
                            refinement, "Ctx", condition_fields, null_sdu
                        ),
                        condition,
                    )
                    .simplified()
                    .ada_expr(),
                )
            ]
        )

    def __create_switch_procedure(
        self, refinement: Refinement, condition_fields: ty.Mapping[Field, Type]
    ) -> UnitPart:
        pdu_identifier = self.__prefix * ID(refinement.pdu.identifier)
        sdu_identifier = self.__prefix * ID(refinement.sdu.identifier)
        pdu_context = f"{refinement.pdu.identifier.flat}_PDU_Context"
        sdu_context = f"{refinement.sdu.identifier.flat}_SDU_Context"
        refined_field_affixed_name = pdu_identifier * refinement.field.affixed_name

        specification = ProcedureSpecification(
            f"Switch_To_{refinement.field.name}",
            [
                InOutParameter([pdu_context], ID(pdu_identifier) * "Context"),
                OutParameter([sdu_context], ID(sdu_identifier) * "Context"),
            ],
        )

        return UnitPart(
            [
                UseTypeClause(f"{pdu_identifier}.Field_Cursors"),
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Not(Constrained(pdu_context)),
                                Not(Constrained(sdu_context)),
                                *[
                                    c.ada_expr()
                                    for c in self.__refinement_conditions(
                                        refinement, pdu_context, condition_fields, null_sdu=False
                                    )
                                ],
                                Call(contains_function_name(refinement), [Variable(pdu_context)]),
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

    def __create_copy_refined_field_procedure(
        self, refinement: Refinement, condition_fields: ty.Mapping[Field, Type]
    ) -> UnitPart:
        pdu_identifier = self.__prefix * ID(refinement.pdu.identifier)
        sdu_identifier = self.__prefix * ID(refinement.sdu.identifier)
        pdu_context = ID(refinement.pdu.identifier.flat) + "_PDU_Context"
        sdu_context = ID(refinement.sdu.identifier.flat) + "_SDU_Context"
        refined_field_affixed_name = pdu_identifier * refinement.field.affixed_name

        specification = ProcedureSpecification(
            f"Copy_{refinement.field.name}",
            [
                Parameter([pdu_context], ID(pdu_identifier) * "Context"),
                InOutParameter([sdu_context], ID(sdu_identifier) * "Context"),
            ],
        )

        return UnitPart(
            [
                UseTypeClause(f"{pdu_identifier}.Field_Cursors"),
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained(sdu_context)),
                                Call(sdu_identifier * "Has_Buffer", [Variable(sdu_context)]),
                                *[
                                    c.ada_expr()
                                    for c in self.__refinement_conditions(
                                        refinement, pdu_context, condition_fields, null_sdu=False
                                    )
                                ],
                                Call(contains_function_name(refinement), [Variable(pdu_context)]),
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
                                    f'"{sdu_context}" is set by "Take_Buffer"'
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
                                    f'"{sdu_context}" is set by "Take_Buffer"'
                                    " but not used after the call"
                                ),
                            ],
                        ),
                        CallStatement(
                            pdu_identifier * f"Get_{refinement.field.name}",
                            [Variable(pdu_context), Variable("Buffer.all")],
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
                            ],
                        ),
                    ],
                )
            ],
        )

    @staticmethod
    def __create_valid_next_function() -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Next",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        Call("Valid_Predecessor", [Variable("Ctx"), Variable("Fld")]),
                        Call("Path_Condition", [Variable("Ctx"), Variable("Fld")]),
                    ),
                )
            ],
        )

    def __check_template_file(self, filename: str) -> None:
        assert self.__template_dir.joinpath(
            filename
        ).is_file(), f'template file not found: "{filename}"'

    def __license_header(self) -> str:
        if self.__reproducible:
            return ""

        filename = "license_header"
        self.__check_template_file(filename)
        with open(self.__template_dir.joinpath(filename)) as license_file:
            today = date.today()
            return license_file.read().format(
                version=__version__,
                date=today,
                year=today.year,
            )

    def __type_validation_function(
        self, scalar_type: Scalar, validation_expression: Expr
    ) -> Subprogram:
        return ExpressionFunctionDeclaration(
            FunctionSpecification(
                "Valid",
                "Boolean",
                [Parameter(["Val"], self.__prefix * common.full_base_type_name(scalar_type))],
            ),
            validation_expression,
        )

    def __integer_conversion_functions(self, integer: Integer) -> ty.Sequence[Subprogram]:
        return [
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Base",
                    self.__prefix * common.full_base_type_name(integer),
                    [Parameter(["Val"], self.__prefix * ID(integer.identifier))],
                ),
                Call(self.__prefix * common.full_base_type_name(integer), [Variable("Val")])
                if isinstance(integer, RangeInteger)
                else Variable("Val"),
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Actual",
                    self.__prefix * ID(integer.identifier),
                    [Parameter(["Val"], self.__prefix * common.full_base_type_name(integer))],
                ),
                Call(self.__prefix * ID(integer.identifier), [Variable("Val")])
                if isinstance(integer, RangeInteger)
                else Variable("Val"),
                [Precondition(Call("Valid", [Variable("Val")]))],
            ),
        ]

    def __refinement_conditions(
        self,
        refinement: Refinement,
        pdu_context: StrID,
        condition_fields: ty.Mapping[Field, Type],
        null_sdu: bool,
    ) -> ty.Sequence[expr.Expr]:
        pdu_identifier = self.__prefix * ID(refinement.pdu.identifier)

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
                    [expr.Variable(pdu_context), expr.Variable(pdu_identifier * f.affixed_name)],
                )
                for f in condition_fields
            ]
        )

        return conditions


def create_file(filename: Path, content: str) -> None:
    log.info("Creating %s", filename)

    with open(filename, "w") as f:
        f.write(content)


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
        ModularType(
            common.base_type_name(integer),
            Pow(Number(2), integer.size_expr.ada_expr()),
            [Annotate("GNATprove", "No_Wrap_Around")],
        ),
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
        ModularType(common.base_type_name(enum), Pow(Number(2), enum.size_expr.ada_expr()))
    )
    types.append(
        EnumerationType(
            common.enum_name(enum) if enum.always_valid else enum.name,
            {ID(k): Number(v.value) for k, v in enum.literals.items()},
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
                        Variant([FALSE], [Component("Raw", common.base_type_name(enum))]),
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


def context_cursors_initialization(message: Message) -> Expr:
    return NamedAggregate(
        (
            message.fields[0].affixed_name,
            NamedAggregate(
                ("State", Variable("S_Invalid")),
                (
                    "Predecessor",
                    Variable(INITIAL.affixed_name),
                ),
            ),
        ),
        (
            "others",
            NamedAggregate(
                ("State", Variable("S_Invalid")),
                (
                    "Predecessor",
                    Variable(FINAL.affixed_name),
                ),
            ),
        ),
    )


def switch_update_conditions(message: Message, field: Field) -> ty.Sequence[Expr]:
    return [
        Not(Call("Has_Buffer", [Variable("Ctx")])),
        Call(f"{common.sequence_name(message, field)}.Has_Buffer", [Variable("Seq_Ctx")]),
        Equal(Variable("Ctx.Buffer_First"), Variable("Seq_Ctx.Buffer_First")),
        Equal(Variable("Ctx.Buffer_Last"), Variable("Seq_Ctx.Buffer_Last")),
        Equal(
            Variable("Seq_Ctx.First"),
            Call("Field_First", [Variable("Ctx"), Variable(field.affixed_name)]),
        ),
        Equal(
            Variable("Seq_Ctx.Last"),
            Call("Field_Last", [Variable("Ctx"), Variable(field.affixed_name)]),
        ),
    ]


def byte_aligned_field(field: Field) -> Expr:
    return Equal(
        Mod(
            Call(
                "Field_First",
                [Variable("Ctx"), Variable(field.affixed_name)],
            ),
            Size(const.TYPES_BYTE),
        ),
        Number(1),
    )
