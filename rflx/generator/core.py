# pylint: disable=too-many-lines
import itertools
import logging
from datetime import date
from pathlib import Path
from typing import Dict, List, Mapping, Sequence, Set, Tuple, cast

import pkg_resources

from rflx import __version__
from rflx.ada import (
    Annotate,
    ArrayType,
    Assignment,
    CallStatement,
    CaseStatement,
    Component,
    ContextItem,
    ContractCases,
    Declaration,
    DefaultInitialCondition,
    Discriminant,
    DynamicPredicate,
    EnumerationType,
    ExpressionFunctionDeclaration,
    FormalDeclaration,
    FormalPackageDeclaration,
    FunctionSpecification,
    GenericPackageInstantiation,
    Ghost,
    IfStatement,
    InOutParameter,
    InstantiationUnit,
    ModularType,
    NullComponent,
    ObjectDeclaration,
    OutParameter,
    PackageBody,
    PackageDeclaration,
    PackageUnit,
    Parameter,
    Postcondition,
    Pragma,
    PragmaStatement,
    Precondition,
    PrivateType,
    ProcedureSpecification,
    RangeSubtype,
    RangeType,
    RecordType,
    Statement,
    Subprogram,
    SubprogramBody,
    SubprogramDeclaration,
    SubprogramUnitPart,
    TypeDeclaration,
    Unit,
    UnitPart,
    UsePackageClause,
    UseTypeClause,
    Variant,
    VariantPart,
    WithClause,
)
from rflx.common import flat_name
from rflx.expression import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    AndThen,
    Call,
    Case,
    Constrained,
    Div,
    Equal,
    Expr,
    First,
    Greater,
    GreaterEqual,
    If,
    In,
    Indexed,
    Last,
    Length,
    LessEqual,
    NamedAggregate,
    Not,
    NotEqual,
    Number,
    Old,
    Or,
    Pow,
    Range,
    Selected,
    Size,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    BUILTINS_PACKAGE,
    FINAL,
    INITIAL,
    INTERNAL_PACKAGE,
    Array,
    Composite,
    DerivedMessage,
    Enumeration,
    Field,
    Integer,
    Message,
    Model,
    ModularInteger,
    RangeInteger,
    Refinement,
    Scalar,
    Type,
)

from .common import (
    NULL,
    VALID_CONTEXT,
    GeneratorCommon,
    base_type_name,
    enum_name,
    full_base_type_name,
    full_enum_name,
    length_dependent_condition,
    sequence_name,
)
from .generator import GeneratorGenerator
from .parser import ParserGenerator
from .types import Types

log = logging.getLogger(__name__)

TEMPLATE_DIR = ("rflx", "templates/")
LIBRARY_FILES = (
    "generic_types.ads",
    "generic_types.adb",
    "builtin_types.ads",
    "builtin_types-conversions.ads",
    "lemmas.adb",
    "lemmas.ads",
    "message_sequence.adb",
    "message_sequence.ads",
    "scalar_sequence.adb",
    "scalar_sequence.ads",
    "types.ads",
)

REFINEMENT_PACKAGE = ID("Contains")


class Generator:
    # pylint: disable=too-many-instance-attributes
    def __init__(self, prefix: str = "", reproducible: bool = False) -> None:
        self.prefix = str(ID(prefix)) if prefix else ""
        self.reproducible = reproducible
        self.units: Dict[ID, Unit] = {}
        self.seen_types: Set[ID] = set()
        self.types = Types(self.prefix)
        self.common = GeneratorCommon(self.prefix)
        self.parser = ParserGenerator(self.prefix)
        self.generator = GeneratorGenerator(self.prefix)

        self.template_dir = Path(pkg_resources.resource_filename(*TEMPLATE_DIR))
        if not self.template_dir.is_dir():
            raise InternalError("template directory not found")

    def generate(self, model: Model) -> None:
        for t in model.types:
            if t.package in [BUILTINS_PACKAGE, INTERNAL_PACKAGE]:
                continue

            if t.package not in self.units:
                self.__create_unit(t.package, [])

            if isinstance(t, (Scalar, Composite)):
                self.__create_type(t, t.package)

            elif isinstance(t, Message):
                if isinstance(t, DerivedMessage):
                    self.__create_derived_message(t)
                else:
                    self.__create_message(t)

            elif isinstance(t, Refinement):
                self.__create_refinement(t)

            else:
                assert False, f'unexpected type "{type(t).__name__}"'

    def write_library_files(self, directory: Path) -> None:
        for template_filename in LIBRARY_FILES:
            self.__check_template_file(template_filename)

            prefix = self.prefix.replace(".", "-").lower()
            filename = f"{prefix}-{template_filename}"

            with open(self.template_dir / Path(template_filename)) as template_file:
                create_file(
                    Path(directory) / Path(filename),
                    self.__license_header() + template_file.read().format(prefix=f"{self.prefix}."),
                )

    def write_top_level_package(self, directory: Path) -> None:
        if self.prefix:
            create_file(
                Path(directory) / Path(self.prefix.lower() + ".ads"),
                self.__license_header() + f"package {self.prefix} is\n\nend {self.prefix};",
            )

    def write_units(self, directory: Path) -> None:
        for unit in self.units.values():
            create_file(
                directory / Path(unit.name + ".ads"), self.__license_header() + unit.specification
            )

            if unit.body:
                create_file(
                    directory / Path(unit.name + ".adb"), self.__license_header() + unit.body
                )

    def __create_refinement(self, refinement: Refinement) -> None:
        self.__create_generic_refinement_unit(refinement)
        self.__create_refinement_unit(refinement)

    def __create_message(self, message: Message) -> None:
        if not message.fields:
            return

        self.__create_generic_message_unit(message)
        self.__create_message_unit(message)

    def __create_derived_message(self, message: DerivedMessage) -> None:
        self.__create_message_unit(message)

    def __create_unit(self, package: ID, context: List[ContextItem]) -> None:
        name = self.prefix * package
        self.units[package] = PackageUnit(context, PackageDeclaration(name), PackageBody(name),)

    # pylint: disable=too-many-statements
    def __create_generic_message_unit(self, message: Message) -> None:
        context: List[ContextItem] = []

        if any(t.package == BUILTINS_PACKAGE for t in message.types.values()):
            context.extend(
                [
                    WithClause(self.types.prefixed_builtin_types),
                    WithClause(self.types.prefixed_builtin_conversions),
                    UsePackageClause(self.types.prefixed_builtin_conversions),
                ]
            )

        context.append(WithClause(self.types.prefixed_generic_types))

        unit_name = generic_name(self.prefix * message.identifier)
        parameters: List[FormalDeclaration] = [
            FormalPackageDeclaration("Types", self.types.prefixed_generic_types),
        ]
        unit = PackageUnit(
            context,
            PackageDeclaration(unit_name, formal_parameters=parameters),
            PackageBody(unit_name),
        )
        self.units[unit_name] = unit

        for field_type in message.types.values():
            if field_type.package in [BUILTINS_PACKAGE, INTERNAL_PACKAGE]:
                continue

            if isinstance(field_type, Scalar) and field_type.package != message.package:
                context.extend(
                    [
                        WithClause(self.prefix * field_type.package),
                        UsePackageClause(self.prefix * field_type.package),
                    ]
                )

            elif isinstance(field_type, Array):
                if isinstance(field_type.element_type, Message):
                    name = ID("Message_Sequence")
                else:
                    name = ID("Scalar_Sequence")
                context.append(WithClause(self.prefix * name))
                parameters.append(
                    FormalPackageDeclaration(
                        f"{field_type.name}_Sequence",
                        self.prefix * name,
                        ["Types", "others => <>"],
                    )
                )

        scalar_fields = {f: t for f, t in message.types.items() if isinstance(t, Scalar)}
        composite_fields = [f for f, t in message.types.items() if isinstance(t, Composite)]
        sequence_fields = {f: t for f, t in message.types.items() if isinstance(t, Array)}

        context_invariant = [
            Equal(e, Old(e))
            for e in (
                Variable("Ctx.Buffer_First"),
                Variable("Ctx.Buffer_Last"),
                Variable("Ctx.First"),
                Variable("Ctx.Last"),
            )
        ]

        unit += self.__create_specification_pragmas(generic_name(ID(message.name)))
        unit += self.__create_use_type_clause()
        unit += self.__create_field_type(message)
        unit += self.__create_state_type()
        unit += self.__create_cursor_type(message)
        unit += self.__create_cursor_validation_functions()
        unit += self.__create_valid_context_function(message, composite_fields)
        unit += self.__create_context_type()
        unit += self.__create_field_dependent_type(scalar_fields, composite_fields)
        unit += self.__create_create_function(message)
        unit += self.__create_initialize_procedure()
        unit += self.__create_restricted_initialize_procedure(message)
        unit += self.__create_initialized_function(message)
        unit += self.__create_take_buffer_procedure(context_invariant)
        unit += self.__create_has_buffer_function()
        unit += self.__create_message_last_function(message)
        unit += self.__create_path_condition_function(message)
        unit += self.__create_field_condition_function(message)
        unit += self.__create_field_length_function(message)
        unit += self.__create_field_first_function(message)
        unit += self.__create_field_last_function()
        unit += self.__create_predecessor_function()
        unit += self.__create_successor_function(message)
        unit += self.__create_valid_predecessor_function(message, composite_fields)
        unit += self.__create_invalid_successor_function(message)
        unit += self.__create_valid_next_function()
        unit += self.__create_available_space_function()
        unit += self.__create_reset_dependent_fields_procedure(message, context_invariant)

        unit += self.parser.create_internal_functions(message, composite_fields)
        unit += self.parser.create_verify_procedure(message, context_invariant)
        unit += self.parser.create_verify_message_procedure(message, context_invariant)
        unit += self.parser.create_present_function()
        unit += self.parser.create_structural_valid_function()
        unit += self.parser.create_valid_function()
        unit += self.parser.create_incomplete_function()
        unit += self.parser.create_invalid_function()
        unit += self.parser.create_structural_valid_message_function(message)
        unit += self.parser.create_valid_message_function(message)
        unit += self.parser.create_incomplete_message_function(message)
        unit += self.parser.create_scalar_accessor_functions(scalar_fields)
        unit += self.parser.create_composite_accessor_procedures(composite_fields)

        unit += self.generator.create_internal_functions(message, scalar_fields)
        unit += self.generator.create_scalar_setter_procedures(message, scalar_fields)
        unit += self.generator.create_composite_setter_procedures(message)
        unit += self.generator.create_composite_initialize_procedures(message)

        unit += self.__create_switch_procedures(message, sequence_fields)
        unit += self.__create_update_procedures(message, sequence_fields)
        unit += self.__create_public_valid_context_function()
        unit += self.__create_cursor_function()
        unit += self.__create_cursors_function()

    @staticmethod
    def __create_specification_pragmas(package: ID) -> UnitPart:
        return UnitPart([Pragma("Annotate", ["GNATprove", "Terminating", str(package)])])

    def __create_use_type_clause(self, additional_types: Sequence[str] = None) -> UnitPart:
        additional_types = additional_types or []
        return UnitPart(
            [
                UseTypeClause(
                    self.types.bytes,
                    self.types.bytes_ptr,
                    self.types.index,
                    self.types.length,
                    self.types.bit_index,
                    self.types.bit_length,
                    *additional_types,
                )
            ]
        )

    @staticmethod
    def __create_field_type(message: Message) -> UnitPart:
        return UnitPart(
            [
                EnumerationType(
                    "Virtual_Field", dict.fromkeys(f.affixed_name for f in message.all_fields)
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
                    dict.fromkeys(("S_Valid", "S_Structural_Valid", "S_Invalid", "S_Incomplete")),
                )
            ]
        )

    def __create_cursor_type(self, message: Message) -> UnitPart:
        discriminants = [Discriminant(["State"], "Cursor_State", Variable("S_Invalid"))]

        return UnitPart(
            [
                PrivateType("Field_Cursor", aspects=[DefaultInitialCondition(FALSE)]),
                ArrayType(  # WORKAROUND: Componolit/Workarounds#14
                    "Field_Cursors", "Virtual_Field", "Field_Cursor"
                ),
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
                    [Component("Predecessor", "Virtual_Field", Variable(FINAL.affixed_name))],
                    discriminants,
                    VariantPart(
                        "State",
                        [
                            Variant(
                                [Variable("S_Valid"), Variable("S_Structural_Valid")],
                                [
                                    Component(
                                        "First",
                                        self.types.bit_index,
                                        First(Variable(self.types.bit_index)),
                                    ),
                                    Component(
                                        "Last",
                                        self.types.bit_length,
                                        First(Variable(self.types.bit_length)),
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
            ],
        )

    def __create_context_type(self) -> UnitPart:
        discriminants = [
            Discriminant(
                ["Buffer_First", "Buffer_Last"], self.types.index, First(Variable(self.types.index))
            ),
            Discriminant(
                ["First", "Last"], self.types.bit_index, First(Variable(self.types.bit_index))
            ),
        ]

        return UnitPart(
            [PrivateType("Context", discriminants, [DefaultInitialCondition(FALSE)])],
            [],
            [
                RecordType(
                    "Context",
                    [
                        Component("Buffer", self.types.bytes_ptr, NULL),
                        Component(
                            "Cursors",
                            "Field_Cursors",
                            NamedAggregate(
                                (
                                    "others",
                                    NamedAggregate(
                                        ("State", Variable("S_Invalid")),
                                        ("Predecessor", Variable(FINAL.affixed_name)),
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
                                    Variable("Context.Buffer"),
                                    Variable("Context.Cursors"),
                                ],
                            )
                        )
                    ],
                )
            ],
        )

    def __create_field_dependent_type(
        self, scalar_fields: Mapping[Field, Scalar], composite_fields: Sequence[Field]
    ) -> UnitPart:
        result_variants = [
            Variant(
                [Variable(f.affixed_name) for f in [INITIAL, *composite_fields, FINAL]],
                [NullComponent()],
            )
        ] + [
            Variant(
                [Variable(f.affixed_name)],
                [Component(f"{f.name}_Value", self.prefix * full_base_type_name(t))],
            )
            for f, t in scalar_fields.items()
        ]

        return UnitPart(
            [
                RecordType(
                    "Field_Dependent_Value",
                    [],
                    [Discriminant(["Fld"], "Virtual_Field", Variable(INITIAL.affixed_name))],
                    VariantPart("Fld", result_variants),
                )
            ]
        )

    def __create_create_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification("Create", "Context")

        return UnitPart(
            [SubprogramDeclaration(specification)],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Aggregate(
                        First(Variable(self.types.index)),
                        First(Variable(self.types.index)),
                        First(Variable(self.types.bit_index)),
                        First(Variable(self.types.bit_index)),
                        NULL,
                        NamedAggregate(
                            (
                                message.fields[0].affixed_name,
                                NamedAggregate(
                                    ("State", Variable("S_Invalid")),
                                    ("Predecessor", Variable(INITIAL.affixed_name)),
                                ),
                            ),
                            (
                                "others",
                                NamedAggregate(
                                    ("State", Variable("S_Invalid")),
                                    ("Predecessor", Variable(FINAL.affixed_name)),
                                ),
                            ),
                        ),
                    ),
                )
            ],
        )

    def __create_initialize_procedure(self) -> UnitPart:
        specification = ProcedureSpecification(
            "Initialize",
            [OutParameter(["Ctx"], "Context"), InOutParameter(["Buffer"], self.types.bytes_ptr)],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained(Variable("Ctx"))),
                                NotEqual(Variable("Buffer"), NULL),
                                # WORKAROUND: Componolit/Workarounds#10
                                Greater(Length(Variable("Buffer")), Number(0)),
                                LessEqual(
                                    Last(Variable("Buffer")),
                                    Div(Last(Variable(self.types.index)), Number(2)),
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Equal(Variable("Buffer"), NULL),
                                # WORKAROUND: Componolit/Workarounds#6
                                Equal(
                                    Variable("Ctx.Buffer_First"),
                                    Old(
                                        Call(
                                            f"{self.types.types}.Bytes_First", [Variable("Buffer")]
                                        )
                                    ),
                                ),
                                Equal(
                                    Variable("Ctx.Buffer_Last"),
                                    Old(
                                        Call(f"{self.types.types}.Bytes_Last", [Variable("Buffer")])
                                    ),
                                ),
                                Equal(
                                    Variable("Ctx.First"),
                                    Call(
                                        f"{self.types.types}.First_Bit_Index",
                                        [Variable("Ctx.Buffer_First")],
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
                            "Initialize",
                            [
                                Variable("Ctx"),
                                Variable("Buffer"),
                                Call(self.types.first_bit_index, [First(Variable("Buffer"))]),
                                Call(self.types.last_bit_index, [Last(Variable("Buffer"))]),
                            ],
                        )
                    ],
                )
            ],
        )

    def __create_restricted_initialize_procedure(self, message: Message) -> UnitPart:
        specification = ProcedureSpecification(
            "Initialize",
            [
                OutParameter(["Ctx"], "Context"),
                InOutParameter(["Buffer"], self.types.bytes_ptr),
                Parameter(["First", "Last"], self.types.bit_index),
            ],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained(Variable("Ctx"))),
                                NotEqual(Variable("Buffer"), NULL),
                                # WORKAROUND: Componolit/Workarounds#10
                                Greater(Length(Variable("Buffer")), Number(0)),
                                GreaterEqual(
                                    Call(self.types.byte_index, [Variable("First")]),
                                    First(Variable("Buffer")),
                                ),
                                LessEqual(
                                    Call(self.types.byte_index, [Variable("Last")]),
                                    Last(Variable("Buffer")),
                                ),
                                LessEqual(Variable("First"), Variable("Last")),
                                LessEqual(
                                    Variable("Last"),
                                    Div(Last(Variable(self.types.bit_index)), Number(2)),
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Equal(Variable("Buffer"), NULL),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                # WORKAROUND: Componolit/Workarounds#6
                                Equal(
                                    Variable("Ctx.Buffer_First"),
                                    Old(
                                        Call(
                                            f"{self.types.types}.Bytes_First", [Variable("Buffer")]
                                        )
                                    ),
                                ),
                                Equal(
                                    Variable("Ctx.Buffer_Last"),
                                    Old(
                                        Call(f"{self.types.types}.Bytes_Last", [Variable("Buffer")])
                                    ),
                                ),
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
                    [
                        ObjectDeclaration(
                            ["Buffer_First"], self.types.index, First(Variable("Buffer")), True
                        ),
                        ObjectDeclaration(
                            ["Buffer_Last"], self.types.index, Last(Variable("Buffer")), True
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
                                Variable("Buffer"),
                                NamedAggregate(
                                    (
                                        message.fields[0].affixed_name,
                                        NamedAggregate(
                                            ("State", Variable("S_Invalid")),
                                            ("Predecessor", Variable(INITIAL.affixed_name)),
                                        ),
                                    ),
                                    (
                                        "others",
                                        NamedAggregate(
                                            ("State", Variable("S_Invalid")),
                                            ("Predecessor", Variable(FINAL.affixed_name)),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        Assignment("Buffer", NULL),
                    ],
                )
            ],
        )

    def __create_initialized_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Initialized", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification, [Ghost()])],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        Call(
                            "Valid_Next",
                            [Variable("Ctx"), Variable(message.fields[0].affixed_name)],
                        ),
                        Equal(
                            Call(
                                "Available_Space",
                                [Variable("Ctx"), Variable(message.fields[0].affixed_name)],
                            ),
                            Add(
                                Call(
                                    f"{self.types.types}.Last_Bit_Index",
                                    [Variable("Ctx.Buffer_Last")],
                                ),
                                -Variable("Ctx.First"),
                                Number(1),
                            ),
                        ),
                        *[
                            Call("Invalid", [Variable("Ctx"), Variable(f.affixed_name)])
                            for f in message.fields
                        ],
                    ),
                )
            ],
        )

    def __create_take_buffer_procedure(self, context_invariant: Sequence[Expr]) -> UnitPart:
        specification = ProcedureSpecification(
            "Take_Buffer",
            [InOutParameter(["Ctx"], "Context"), OutParameter(["Buffer"], self.types.bytes_ptr)],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(And(VALID_CONTEXT, Call("Has_Buffer", [Variable("Ctx")]))),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Not(Call("Has_Buffer", [Variable("Ctx")])),
                                NotEqual(Variable("Buffer"), NULL),
                                Equal(Variable("Ctx.Buffer_First"), First(Variable("Buffer"))),
                                Equal(Variable("Ctx.Buffer_Last"), Last(Variable("Buffer"))),
                                *context_invariant,
                                *[Equal(e, Old(e)) for e in [Call("Cursors", [Variable("Ctx")])]],
                            )
                        ),
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

    def __create_path_condition_function(self, message: Message) -> UnitPart:
        def condition(field: Field, message: Message) -> Expr:
            cases: List[Tuple[Expr, Expr]] = [
                (target, Or(*[c for _, c in conditions]))
                for target, conditions in itertools.groupby(
                    [
                        (Variable(l.target.affixed_name), l.condition)
                        for l in message.outgoing(field)
                        if l.target != FINAL
                    ],
                    lambda x: x[0],
                )
            ]
            cases.append((Variable("others"), FALSE))
            return Case(Variable("Fld"), cases).simplified(self.common.substitution(message))

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
                                VALID_CONTEXT,
                                Call("Valid_Predecessor", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            [
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

    def __create_field_length_function(self, message: Message) -> UnitPart:
        def length(field: Field, message: Message) -> Expr:
            target_links = [
                (target, list(links))
                for target, links in itertools.groupby(message.outgoing(field), lambda x: x[1])
                if target != FINAL
            ]
            cases: List[Tuple[Expr, Expr]] = []
            for target, links in target_links:
                field_type = message.types[target]
                length: Expr
                if isinstance(field_type, Scalar):
                    length = Size(Variable(self.prefix * full_base_type_name(field_type)))
                else:
                    if len(links) == 1:
                        length = links[0].length
                    elif len(links) > 1:
                        length = If(
                            [(l.condition, l.length) for l in links],
                            Variable(self.types.unreachable_bit_length),
                        ).simplified(self.common.substitution(message))
                cases.append((Variable(target.affixed_name), length))

            if not cases:
                return Number(0)

            cases.append((Variable("others"), Variable(self.types.unreachable_bit_length)))
            return Case(Variable("Fld"), cases).simplified(self.common.substitution(message))

        specification = FunctionSpecification(
            "Field_Length",
            self.types.bit_length,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                VALID_CONTEXT,
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"),
                        [
                            (Variable(f.affixed_name), length(f, message))
                            for f in message.all_fields
                        ],
                    ),
                )
            ],
        )

    def __create_field_first_function(self, message: Message) -> UnitPart:
        def first(field: Field, message: Message) -> Expr:
            if field == message.fields[0]:
                return Variable("Ctx.First")

            contiguous_first = Add(
                Selected(
                    Indexed(
                        Variable("Ctx.Cursors"),
                        Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"),
                    ),
                    "Last",
                ),
                Number(1),
            )

            return If(
                [
                    (
                        And(
                            Equal(
                                Selected(
                                    Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"
                                ),
                                Variable(l.source.affixed_name),
                            ),
                            l.condition,
                        ),
                        l.first.simplified({UNDEFINED: contiguous_first}),
                    )
                    for l in message.incoming(field)
                ],
                Variable(self.types.unreachable_bit_length),
            ).simplified(self.common.substitution(message))

        specification = FunctionSpecification(
            "Field_First",
            self.types.bit_index,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                VALID_CONTEXT,
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [(Variable(f.affixed_name), first(f, message)) for f in message.fields],
                    ),
                )
            ],
        )

    def __create_field_last_function(self) -> UnitPart:
        specification = FunctionSpecification(
            "Field_Last",
            self.types.bit_index,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [Precondition(And(Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),))],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Add(
                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        Call("Field_Length", [Variable("Ctx"), Variable("Fld")]),
                        -Number(1),
                    ),
                )
            ],
        )

    def __create_field_condition_function(self, message: Message) -> UnitPart:
        def condition(field: Field, message: Message) -> Expr:
            return (
                Or(*[l.condition for l in message.outgoing(field)])
                .simplified(
                    {
                        Variable(field.name): Call(
                            self.types.bit_length, [Variable(f"Val.{field.name}_Value")]
                        ),
                        Length(Variable(field.name)): Variable("Length"),
                        Last(Variable(field.name)): Selected(
                            Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)), "Last",
                        ),
                    }
                )
                .simplified(self.common.substitution(message))
            )

        parameters = [Parameter(["Ctx"], "Context"), Parameter(["Val"], "Field_Dependent_Value")]

        if length_dependent_condition(message):
            parameters.append(Parameter(["Length"], self.types.bit_length, Number(0)))

        specification = FunctionSpecification("Field_Condition", "Boolean", parameters)

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                VALID_CONTEXT,
                                In(Variable("Val.Fld"), Range(Variable("Field"))),
                                Call("Valid_Predecessor", [Variable("Ctx"), Variable("Val.Fld")]),
                            )
                        )
                    ],
                )
            ],
            [
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
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (Variable(INITIAL.affixed_name), Variable(INITIAL.affixed_name)),
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
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (
                                Variable(f.affixed_name),
                                If(
                                    [
                                        (l.condition, Variable(l.target.affixed_name))
                                        for l in message.outgoing(f)
                                    ],
                                    Variable(INITIAL.affixed_name),
                                ).simplified(self.common.substitution(message)),
                            )
                            for f in message.fields
                        ],
                    ),
                    [
                        Precondition(
                            And(
                                Call("Structural_Valid", [Variable("Ctx"), Variable("Fld")]),
                                Call("Valid_Predecessor", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
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

    def __create_reset_dependent_fields_procedure(
        self, message: Message, context_invariant: Sequence[Expr]
    ) -> UnitPart:
        specification = ProcedureSpecification(
            "Reset_Dependent_Fields",
            [InOutParameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        field_location_invariant = PragmaStatement(
            "Assert",
            [
                str(
                    And(
                        Equal(
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                            Variable("First"),
                        ),
                        Equal(
                            Call("Field_Length", [Variable("Ctx"), Variable("Fld")]),
                            Variable("Length"),
                        ),
                    )
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
                            self.types.bit_length,
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                            True,
                            [Ghost()],
                        ),
                        ObjectDeclaration(
                            ["Length"],
                            self.types.bit_length,
                            Call("Field_Length", [Variable("Ctx"), Variable("Fld")]),
                            True,
                            [Ghost()],
                        ),
                    ],
                    [
                        field_location_invariant,
                        CaseStatement(
                            Variable("Fld"),
                            [
                                (
                                    Variable(f.affixed_name),
                                    cast(List[Statement], [])
                                    + [
                                        Assignment(
                                            Indexed(
                                                Variable("Ctx.Cursors"), Variable(s.affixed_name)
                                            ),
                                            Aggregate(
                                                Variable("S_Invalid"), Variable(FINAL.affixed_name)
                                            ),
                                        )
                                        for s in reversed(message.successors(f))
                                    ]
                                    + [
                                        Assignment(
                                            Indexed(
                                                Variable("Ctx.Cursors"), Variable(f.affixed_name)
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
                        Precondition(And(Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),),),
                        Postcondition(
                            And(
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                                Call(
                                    "Invalid", [Indexed(Variable("Ctx.Cursors"), Variable("Fld"))]
                                ),
                                Call("Invalid_Successor", [Variable("Ctx"), Variable("Fld")])
                                if len(message.fields) > 1
                                else TRUE,
                                *context_invariant,
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected(
                                            Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                                            "Predecessor",
                                        ),
                                        Call("Has_Buffer", [Variable("Ctx")]),
                                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                                        Call("Field_Length", [Variable("Ctx"), Variable("Fld")]),
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
                                                        [Variable("Ctx"), Variable(s.affixed_name)],
                                                    )
                                                    for s in [f, *message.successors(f)]
                                                ],
                                            ),
                                        )
                                        for f in message.fields
                                    ],
                                ),
                            ).simplified()
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
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
            [ExpressionFunctionDeclaration(specification, NotEqual(Variable("Ctx.Buffer"), NULL))],
        )

    @staticmethod
    def __create_valid_predecessor_function(
        message: Message, composite_fields: Sequence[Field]
    ) -> UnitPart:

        specification = FunctionSpecification(
            "Valid_Predecessor",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Virtual_Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (
                                Variable(f.affixed_name),
                                Or(
                                    *[
                                        And(
                                            Call(
                                                "Structural_Valid"
                                                if p in composite_fields
                                                else "Valid",
                                                [
                                                    Indexed(
                                                        Variable("Ctx.Cursors"),
                                                        Variable(p.affixed_name),
                                                    )
                                                ],
                                            )
                                            if p != INITIAL
                                            else TRUE,
                                            Equal(
                                                Selected(
                                                    Indexed(
                                                        Variable("Ctx.Cursors"), Variable("Fld")
                                                    ),
                                                    "Predecessor",
                                                ),
                                                Variable(p.affixed_name),
                                            ),
                                        ).simplified()
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

    def __create_message_last_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Message_Last", self.types.bit_index, [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(VALID_CONTEXT, Call("Structural_Valid_Message", [Variable("Ctx")]))
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    If(
                        [
                            (
                                And(
                                    Call(
                                        "Structural_Valid",
                                        [
                                            Indexed(
                                                Variable("Ctx.Cursors"),
                                                Variable(l.source.affixed_name),
                                            )
                                        ],
                                    ),
                                    l.condition,
                                ).simplified(self.common.substitution(message)),
                                Selected(
                                    Indexed(
                                        Variable("Ctx.Cursors"), Variable(l.source.affixed_name)
                                    ),
                                    "Last",
                                ),
                            )
                            for l in message.incoming(FINAL)
                        ],
                        Variable(self.types.unreachable_bit_length),
                    ),
                )
            ],
        )

    def __create_available_space_function(self) -> UnitPart:
        specification = FunctionSpecification(
            "Available_Space",
            self.types.bit_length,
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                VALID_CONTEXT,
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            )
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Add(
                        Call(self.types.last_bit_index, [Variable("Ctx.Buffer_Last")]),
                        -Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        Number(1),
                    ),
                )
            ],
        )

    def __create_switch_procedures(
        self, message: Message, sequence_fields: Mapping[Field, Type]
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Switch_To_{field.name}",
                [
                    InOutParameter(["Ctx"], "Context"),
                    OutParameter(["Seq_Ctx"], f"{sequence_name(message, field)}.Context"),
                ],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                VALID_CONTEXT,
                                Not(Constrained(Variable("Ctx"))),
                                Not(Constrained(Variable("Seq_Ctx"))),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Valid_Next", [Variable("Ctx"), Variable(f.affixed_name)]),
                                Greater(
                                    Call(
                                        "Field_Length", [Variable("Ctx"), Variable(f.affixed_name)]
                                    ),
                                    Number(0),
                                ),
                                LessEqual(
                                    Call("Field_Last", [Variable("Ctx"), Variable(f.affixed_name)]),
                                    Div(Last(Variable(self.types.bit_index)), Number(2)),
                                ),
                                Call(
                                    "Field_Condition",
                                    [
                                        Variable("Ctx"),
                                        NamedAggregate(("Fld", Variable(f.affixed_name))),
                                    ]
                                    + (
                                        [
                                            Call(
                                                "Field_Length",
                                                [Variable("Ctx"), Variable(f.affixed_name)],
                                            ),
                                        ]
                                        if length_dependent_condition(message)
                                        else []
                                    ),
                                ),
                                self.common.sufficient_space_for_field_condition(
                                    Variable(f.affixed_name)
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                *switch_update_conditions(message, f),
                                Equal(
                                    Call(
                                        f"{sequence_name(message, f)}.Index", [Variable("Seq_Ctx")]
                                    ),
                                    Variable("Seq_Ctx.First"),
                                ),
                                Call("Present", [Variable("Ctx"), Variable(f.affixed_name)]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Variable("Ctx.Buffer_First"),
                                        Variable("Ctx.Buffer_Last"),
                                        Variable("Ctx.First"),
                                        Call(
                                            "Predecessor",
                                            [Variable("Ctx"), Variable(f.affixed_name)],
                                        ),
                                        Call(
                                            "Path_Condition",
                                            [Variable("Ctx"), Variable(f.affixed_name)],
                                        ),
                                    ]
                                    + [
                                        Call(f"Cursor", [Variable("Ctx"), Variable(p.affixed_name)])
                                        for p in message.predecessors(f)
                                    ]
                                ],
                            )
                        ),
                        ContractCases(
                            (
                                Call(
                                    "Structural_Valid", [Variable("Ctx"), Variable(f.affixed_name)]
                                ),
                                And(
                                    *[
                                        Equal(
                                            Call(
                                                "Cursor",
                                                [Variable("Ctx"), Variable(s.affixed_name)],
                                            ),
                                            Old(
                                                Call(
                                                    "Cursor",
                                                    [Variable("Ctx"), Variable(s.affixed_name)],
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
                                    *self.common.valid_path_to_next_field_condition(message, f),
                                    *[
                                        Call("Invalid", [Variable("Ctx"), Variable(s.affixed_name)])
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
                        *self.common.field_bit_location_declarations(Variable(f.affixed_name)),
                        ObjectDeclaration(["Buffer"], self.types.bytes_ptr),
                    ],
                    [
                        IfStatement(
                            [
                                (
                                    Call("Invalid", [Variable("Ctx"), Variable(f.affixed_name)]),
                                    self.common.initialize_field_statements(message, f),
                                )
                            ]
                        ),
                        CallStatement("Take_Buffer", [Variable("Ctx"), Variable("Buffer")]),
                        PragmaStatement("Warnings", ["Off", '"unused assignment to ""Buffer"""']),
                        CallStatement(
                            f"{sequence_name(message, f)}.Initialize",
                            [
                                Variable("Seq_Ctx"),
                                Variable("Buffer"),
                                Variable("Ctx.Buffer_First"),
                                Variable("Ctx.Buffer_Last"),
                                Variable("First"),
                                Variable("Last"),
                            ],
                        ),
                        PragmaStatement("Warnings", ["On", '"unused assignment to ""Buffer"""']),
                    ],
                )
                for f in sequence_fields
            ],
        )

    def __create_update_procedures(
        self, message: Message, sequence_fields: Mapping[Field, Type]
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Update_{field.name}",
                [
                    InOutParameter(["Ctx"], "Context"),
                    InOutParameter(["Seq_Ctx"], f"{sequence_name(message, field)}.Context"),
                ],
            )

        def take_buffer_arguments(field: Field) -> Sequence[Expr]:
            arguments = [
                Variable("Seq_Ctx"),
                Variable("Buffer"),
                Variable("Ctx.Buffer_First"),
                Variable("Ctx.Buffer_Last"),
            ]

            field_type = message.types[field]
            assert isinstance(field_type, Array)

            if isinstance(field_type.element_type, Message):
                arguments.extend([Variable("Ctx.First"), Variable("Ctx.Last")])

            return arguments

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                VALID_CONTEXT,
                                Call("Present", [Variable("Ctx"), Variable(f.affixed_name)]),
                                *switch_update_conditions(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Call("Present", [Variable("Ctx"), Variable(f.affixed_name)]),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Not(
                                    Call(
                                        f"{sequence_name(message, f)}.Has_Buffer",
                                        [Variable("Seq_Ctx")],
                                    )
                                ),
                                Equal(
                                    Variable("Seq_Ctx.First"),
                                    Call(
                                        "Field_First", [Variable("Ctx"), Variable(f.affixed_name)]
                                    ),
                                ),
                                Equal(
                                    Variable("Seq_Ctx.Last"),
                                    Call("Field_Last", [Variable("Ctx"), Variable(f.affixed_name)]),
                                ),
                                *[
                                    Equal(e, Old(e))
                                    for e in cast(List[Expr], [])
                                    + [
                                        Variable("Seq_Ctx.First"),
                                        Variable("Seq_Ctx.Last"),
                                        Variable("Ctx.Buffer_First"),
                                        Variable("Ctx.Buffer_Last"),
                                        Call(
                                            "Field_First",
                                            [Variable("Ctx"), Variable(f.affixed_name)],
                                        ),
                                        Call(
                                            "Field_Length",
                                            [Variable("Ctx"), Variable(f.affixed_name)],
                                        ),
                                    ]
                                    + [
                                        Call(f"Cursor", [Variable("Ctx"), Variable(o.affixed_name)])
                                        for o in message.fields
                                        if o != f
                                    ]
                                ],
                            )
                        ),
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
                            Call(f"{sequence_name(message, f)}.Valid", [Variable("Seq_Ctx")]),
                            True,
                        ),
                        ObjectDeclaration(["Buffer"], self.types.bytes_ptr),
                    ],
                    [
                        CallStatement(
                            f"{sequence_name(message, f)}.Take_Buffer", take_buffer_arguments(f)
                        ),
                        Assignment("Ctx.Buffer", Variable("Buffer")),
                        IfStatement(
                            [
                                (
                                    Variable("Valid_Sequence"),
                                    [
                                        Assignment(
                                            Indexed(
                                                Variable("Ctx.Cursors"), Variable(f.affixed_name)
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
        self, message: Message, composite_fields: Sequence[Field]
    ) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Context",
            "Boolean",
            [
                Parameter(["Buffer_First", "Buffer_Last"], self.types.index),
                Parameter(["First", "Last"], self.types.bit_index),
                # WORKAROUND: Componolit/Workarounds#17
                Parameter(["Buffer"], self.types.bytes_ptr),
                Parameter(["Cursors"], "Field_Cursors"),
            ],
        )

        return UnitPart(
            [],
            [],
            [
                # WORKAROUND: Componolit/Workarounds#17
                Pragma(
                    "Warnings",
                    ["Off", '"""Buffer"" is not modified, could be of access constant type"'],
                ),
                ExpressionFunctionDeclaration(
                    specification, self.common.context_predicate(message, composite_fields)
                ),
            ],
        )

    @staticmethod
    def __create_public_valid_context_function() -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Context", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification, [Annotate("GNATprove", "Inline_For_Proof"), Ghost()]
                )
            ],
            [],
            [
                ExpressionFunctionDeclaration(  # WORKAROUND: Componolit/Workarounds#1
                    specification,
                    Call(
                        "Valid_Context",
                        [
                            Variable("Ctx.Buffer_First"),
                            Variable("Ctx.Buffer_Last"),
                            Variable("Ctx.First"),
                            Variable("Ctx.Last"),
                            Variable("Ctx.Buffer"),
                            Variable("Ctx.Cursors"),
                        ],
                    ),
                )
            ],
        )

    @staticmethod
    def __create_cursor_function() -> UnitPart:
        specification = FunctionSpecification(
            "Cursor", "Field_Cursor", [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")]
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
            "Cursors", "Field_Cursors", [Parameter(["Ctx"], "Context")]
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

    def __create_message_unit(self, message: Message) -> None:
        if isinstance(message, DerivedMessage):
            name = generic_name(self.prefix * message.base.identifier)
        else:
            name = generic_name(self.prefix * message.identifier)

        context: List[ContextItem] = [
            Pragma("SPARK_Mode"),
            WithClause(name),
            WithClause(self.types.prefixed_types),
        ]

        arrays = [
            self.prefix * t.identifier for t in message.types.values() if isinstance(t, Array)
        ]
        context.extend(WithClause(array) for array in arrays)
        instantiation = GenericPackageInstantiation(
            self.prefix * message.identifier, name, [self.types.prefixed_types] + arrays
        )

        self.units[message.identifier] = InstantiationUnit(context, instantiation)

    def __create_generic_refinement_unit(self, refinement: Refinement) -> None:
        unit_name = generic_name(self.prefix * refinement.package * REFINEMENT_PACKAGE)

        if unit_name in self.units:
            unit = self.units[unit_name]

        else:
            unit = PackageUnit(
                [WithClause(self.types.prefixed_generic_types)],
                PackageDeclaration(
                    unit_name,
                    formal_parameters=[
                        FormalPackageDeclaration("Types", self.types.prefixed_generic_types)
                    ],
                ),
                PackageBody(unit_name),
            )
            unit += self.__create_specification_pragmas(generic_name(REFINEMENT_PACKAGE))
            unit += self.__create_use_type_clause()
            self.units[unit_name] = unit

        null_sdu = not refinement.sdu.fields

        assert isinstance(unit, PackageUnit), f"unexpected unit type"
        assert isinstance(unit.declaration.formal_parameters, List), f"missing formal parameters"

        if refinement.pdu.package != refinement.package:
            pdu_package = (
                refinement.pdu.base.package
                if isinstance(refinement.pdu, DerivedMessage)
                else refinement.pdu.package
            )

            unit.context.extend(
                [
                    WithClause(self.prefix * pdu_package),
                    UsePackageClause(self.prefix * pdu_package),
                ]
            )

        generic_pdu_name = self.prefix * (
            generic_name(refinement.pdu.base.identifier)
            if isinstance(refinement.pdu, DerivedMessage)
            else generic_name(refinement.pdu.identifier)
        )

        unit.context.append(WithClause(generic_pdu_name))
        unit.declaration.formal_parameters.append(
            FormalPackageDeclaration(
                flat_name(refinement.pdu.full_name), generic_pdu_name, ["Types", "others => <>"]
            )
        )

        generic_sdu_name = self.prefix * (
            generic_name(refinement.sdu.base.identifier)
            if isinstance(refinement.sdu, DerivedMessage)
            else generic_name(refinement.sdu.identifier)
        )

        if not null_sdu:
            unit.context.append(WithClause(generic_sdu_name))
            unit.declaration.formal_parameters.append(
                FormalPackageDeclaration(
                    flat_name(refinement.sdu.full_name), generic_sdu_name, ["Types", "others => <>"]
                ),
            )

        condition_fields = {
            f: t
            for f, t in refinement.pdu.types.items()
            if Variable(f.name) in refinement.condition
        }

        unit += self.__create_contains_function(refinement, condition_fields, null_sdu)
        if not null_sdu:
            unit += self.__create_switch_procedure(refinement, condition_fields)

    def __create_refinement_unit(self, refinement: Refinement) -> None:
        unit_name = refinement.package * REFINEMENT_PACKAGE
        generic_unit_name = generic_name(self.prefix * refinement.package * REFINEMENT_PACKAGE)

        if unit_name in self.units:
            unit = self.units[unit_name]
        else:
            context = [
                Pragma("SPARK_Mode"),
                WithClause(self.types.prefixed_types),
                WithClause(generic_unit_name),
            ]
            instantiation = GenericPackageInstantiation(
                self.prefix * unit_name, generic_unit_name, [self.types.prefixed_types]
            )
            unit = InstantiationUnit(context, instantiation)
            self.units[unit_name] = unit

        null_sdu = not refinement.sdu.fields

        assert isinstance(unit, InstantiationUnit), f"unexpected unit type"

        pdu_name = self.prefix * refinement.pdu.identifier

        if pdu_name not in unit.declaration.associations:
            unit.context.append(WithClause(pdu_name))
            unit.declaration.associations.append(pdu_name)

        sdu_name = self.prefix * refinement.sdu.identifier

        if not null_sdu and sdu_name not in unit.declaration.associations:
            unit.context.append(WithClause(sdu_name))
            unit.declaration.associations.append(sdu_name)

    def __create_type(self, field_type: Type, message_package: ID) -> None:
        unit = self.units[message_package]

        if field_type.package == BUILTINS_PACKAGE or self.__is_seen_type(field_type):
            return

        if isinstance(field_type, ModularInteger):
            unit += UnitPart(modular_types(field_type))
            unit += UnitPart(self.__type_dependent_unreachable_function(field_type))
            unit += self.__modular_functions(field_type)
        elif isinstance(field_type, RangeInteger):
            unit += UnitPart(range_types(field_type))
            unit += UnitPart(self.__type_dependent_unreachable_function(field_type))
            unit += self.__range_functions(field_type)
        elif isinstance(field_type, Enumeration):
            unit += UnitPart(enumeration_types(field_type))
            unit += UnitPart(self.__type_dependent_unreachable_function(field_type))
            unit += self.__enumeration_functions(field_type)
        elif isinstance(field_type, Array):
            self.__create_array_unit(field_type, message_package)
        else:
            assert False, f'unexpected type "{type(field_type).__name__}"'

    def __create_array_unit(self, array_type: Array, package_name: ID) -> None:
        element_type = array_type.element_type

        array_context: List[ContextItem] = []
        array_package: GenericPackageInstantiation
        if isinstance(element_type, Message):
            array_context = [
                Pragma("SPARK_Mode"),
                WithClause(self.prefix * ID("Message_Sequence")),
                WithClause(self.prefix * element_type.identifier),
                WithClause(self.types.prefixed_types),
            ]
            array_package = GenericPackageInstantiation(
                self.prefix * array_type.identifier,
                "Message_Sequence",
                [
                    self.types.prefixed_types,
                    f"{element_type.name}.Context",
                    f"{element_type.name}.Initialize",
                    f"{element_type.name}.Take_Buffer",
                    f"{element_type.name}.Has_Buffer",
                    f"{element_type.name}.Message_Last",
                    f"{element_type.name}.Initialized",
                    f"{element_type.name}.Structural_Valid_Message",
                    f"{element_type.name}.Valid_Context",
                ],
            )
        elif isinstance(element_type, Scalar):
            array_context = [
                Pragma("SPARK_Mode"),
                WithClause(self.prefix * ID("Scalar_Sequence")),
                WithClause(self.prefix * package_name),
                WithClause(self.types.prefixed_types),
            ]
            array_package = GenericPackageInstantiation(
                self.prefix * package_name * array_type.name,
                self.prefix * ID("Scalar_Sequence"),
                [
                    self.types.prefixed_types,
                    element_type.name,
                    base_type_name(element_type)
                    if not isinstance(element_type, ModularInteger)
                    else element_type.name,
                    "Valid",
                    "To_Actual",
                    "To_Base",
                ],
            )
        else:
            assert False, 'unexpected element type "{type(element_type)}"'

        self.units[array_package.identifier] = InstantiationUnit(array_context, array_package)

    def __range_functions(self, integer: RangeInteger) -> SubprogramUnitPart:
        specification: List[Subprogram] = []

        for range_type in range_types(integer):
            if isinstance(range_type, RangeSubtype):
                continue

        specification.append(
            self.__type_validation_function(integer, integer.constraints("Val").simplified())
        )
        specification.extend(self.__integer_conversion_functions(integer))

        return SubprogramUnitPart(specification)

    def __modular_functions(self, integer: ModularInteger) -> UnitPart:
        specification: List[Declaration] = []

        specification.append(Pragma("Warnings", ["Off", '"unused variable ""Val"""']))
        specification.append(
            self.__type_validation_function(integer, integer.constraints("Val").simplified())
        )
        specification.append(Pragma("Warnings", ["On", '"unused variable ""Val"""']))
        specification.extend(self.__integer_conversion_functions(integer))

        return UnitPart(specification)

    def __enumeration_functions(self, enum: Enumeration) -> UnitPart:
        specification: List[Declaration] = []

        enum_value = Variable("Val")

        validation_expression: Expr
        if enum.always_valid:
            validation_expression = enum.constraints("Val").simplified()
        else:
            validation_cases: List[Tuple[Expr, Expr]] = []
            validation_cases.extend((value, Variable("True")) for value in enum.literals.values())
            validation_cases.append((Variable("others"), Variable("False")))

            validation_expression = Case(enum_value, validation_cases)

        if enum.always_valid:
            specification.append(Pragma("Warnings", ["Off", '"unused variable ""Val"""']))
        specification.append(self.__type_validation_function(enum, validation_expression))
        if enum.always_valid:
            specification.append(Pragma("Warnings", ["On", '"unused variable ""Val"""']))

        specification.append(
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Base",
                    self.prefix * full_base_type_name(enum),
                    [
                        Parameter(
                            ["Enum"],
                            self.prefix
                            * (full_enum_name(enum) if enum.always_valid else enum.identifier),
                        )
                    ],
                ),
                Case(
                    Variable("Enum"),
                    [(Variable(key), value) for key, value in enum.literals.items()],
                ),
            )
        )

        conversion_function = FunctionSpecification(
            "To_Actual",
            self.prefix * enum.identifier,
            [Parameter(["Val"], self.prefix * full_base_type_name(enum))],
        )
        precondition = Precondition(Call("Valid", [Variable("Val")]))
        conversion_cases: List[Tuple[Expr, Expr]] = []

        if enum.always_valid:
            specification.append(
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "To_Actual",
                        self.prefix * enum.identifier,
                        [Parameter(["Enum"], enum_name(enum))],
                    ),
                    Aggregate(TRUE, Variable("Enum")),
                )
            )

            conversion_cases.extend(
                (value, Aggregate(Variable("True"), Variable(key)))
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
                        self.prefix * full_base_type_name(enum),
                        [Parameter(["Val"], self.prefix * enum.identifier)],
                    ),
                    If(
                        [(Variable("Val.Known"), Call("To_Base", [Variable("Val.Enum")]))],
                        Variable("Val.Raw"),
                    ),
                )
            )

        else:
            conversion_cases.extend((value, Variable(key)) for key, value in enum.literals.items())
            conversion_cases.append(
                (Variable("others"), Call(unreachable_function_name(enum.full_name)))
            )

            specification.append(
                ExpressionFunctionDeclaration(
                    conversion_function, Case(enum_value, conversion_cases), [precondition]
                )
            )

        return UnitPart(specification)

    @staticmethod
    def __create_contains_function(
        refinement: Refinement, condition_fields: Mapping[Field, Type], null_sdu: bool
    ) -> SubprogramUnitPart:
        pdu_name = flat_name(refinement.pdu.full_name)
        condition = refinement.condition
        for f, t in condition_fields.items():
            if isinstance(t, Enumeration) and t.always_valid:
                condition = AndThen(
                    Selected(Call(f"{pdu_name}.Get_{f.name}", [Variable("Ctx")]), "Known"),
                    condition,
                )
        condition = condition.simplified(
            {
                Variable(f.name): Selected(
                    Call(f"{pdu_name}.Get_{f.name}", [Variable("Ctx")]), "Enum"
                )
                if isinstance(t, Enumeration) and t.always_valid
                else Call(f"{pdu_name}.Get_{f.name}", [Variable("Ctx")])
                for f, t in condition_fields.items()
            }
        )

        specification = FunctionSpecification(
            contains_function_name(refinement),
            "Boolean",
            [Parameter(["Ctx"], f"{pdu_name}.Context")],
        )

        return SubprogramUnitPart(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        *refinement_conditions(refinement, "Ctx", condition_fields, null_sdu),
                        condition,
                    ).simplified(),
                )
            ]
        )

    def __create_switch_procedure(
        self, refinement: Refinement, condition_fields: Mapping[Field, Type]
    ) -> UnitPart:
        pdu_name = flat_name(refinement.pdu.full_name)
        sdu_name = flat_name(refinement.sdu.full_name)
        pdu_context = f"{pdu_name}_Context"
        sdu_context = f"{sdu_name}_Context"
        refined_field_affixed_name = f"{pdu_name}.{refinement.field.affixed_name}"

        specification = ProcedureSpecification(
            f"Switch_To_{refinement.field.name}",
            [
                InOutParameter([pdu_context], f"{pdu_name}.Context"),
                OutParameter([sdu_context], f"{sdu_name}.Context"),
            ],
        )

        return UnitPart(
            [
                UseTypeClause(f"{pdu_name}.Field_Cursors"),
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Not(Constrained(Variable(pdu_context))),
                                Not(Constrained(Variable(sdu_context))),
                                *refinement_conditions(
                                    refinement, pdu_context, condition_fields, False
                                ),
                                Call(contains_function_name(refinement), [Variable(pdu_context)]),
                            )
                        ),
                        Postcondition(
                            And(
                                Not(Call(f"{pdu_name}.Has_Buffer", [Variable(pdu_context)])),
                                Call(f"{sdu_name}.Has_Buffer", [Variable(sdu_context)]),
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
                                        f"{pdu_name}.Field_First",
                                        [
                                            Variable(pdu_context),
                                            Variable(refined_field_affixed_name),
                                        ],
                                    ),
                                ),
                                Equal(
                                    Selected(Variable(sdu_context), "Last"),
                                    Call(
                                        f"{pdu_name}.Field_Last",
                                        [
                                            Variable(pdu_context),
                                            Variable(refined_field_affixed_name),
                                        ],
                                    ),
                                ),
                                Call(f"{sdu_name}.Initialized", [Variable(sdu_context)]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected(Variable(pdu_context), "Buffer_First"),
                                        Selected(Variable(pdu_context), "Buffer_Last"),
                                        Selected(Variable(pdu_context), "First"),
                                        Call(f"{pdu_name}.Cursors", [Variable(pdu_context)]),
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
                            self.types.bit_index,
                            Call(
                                f"{pdu_name}.Field_First",
                                [Variable(pdu_context), Variable(refined_field_affixed_name)],
                            ),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            self.types.bit_index,
                            Call(
                                f"{pdu_name}.Field_Last",
                                [Variable(pdu_context), Variable(refined_field_affixed_name)],
                            ),
                            True,
                        ),
                        ObjectDeclaration(["Buffer"], self.types.bytes_ptr),
                    ],
                    [
                        CallStatement(
                            f"{pdu_name}.Take_Buffer", [Variable(pdu_context), Variable("Buffer")]
                        ),
                        PragmaStatement("Warnings", ["Off", '"unused assignment to ""Buffer"""']),
                        CallStatement(
                            f"{sdu_name}.Initialize",
                            [
                                Variable(sdu_context),
                                Variable("Buffer"),
                                Variable("First"),
                                Variable("Last"),
                            ],
                        ),
                        PragmaStatement("Warnings", ["On", '"unused assignment to ""Buffer"""']),
                    ],
                )
            ],
        )

    @staticmethod
    def __create_valid_next_function() -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Next", "Boolean", [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
            [
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
        if not self.template_dir.joinpath(filename).is_file():
            raise InternalError(f'template file not found: "{filename}"')

    def __license_header(self) -> str:
        if self.reproducible:
            return ""

        filename = "license_header"
        self.__check_template_file(filename)
        with open(self.template_dir.joinpath(filename)) as license_file:
            today = date.today()
            return license_file.read().format(version=__version__, date=today, year=today.year,)

    def __type_validation_function(
        self, scalar_type: Scalar, validation_expression: Expr
    ) -> Subprogram:
        return ExpressionFunctionDeclaration(
            FunctionSpecification(
                f"Valid",
                "Boolean",
                [Parameter(["Val"], self.prefix * full_base_type_name(scalar_type))],
            ),
            validation_expression,
        )

    def __type_dependent_unreachable_function(self, scalar_type: Scalar) -> List[Declaration]:
        base_name = None
        if isinstance(scalar_type, Enumeration) and scalar_type.always_valid:
            base_name = self.prefix * full_base_type_name(scalar_type)

        type_name = self.prefix * scalar_type.identifier

        return [
            Pragma("Warnings", ["Off", '"precondition is statically false"']),
            ExpressionFunctionDeclaration(
                FunctionSpecification(unreachable_function_name(scalar_type.full_name), type_name),
                First(Variable(type_name))
                if not base_name
                else Aggregate(Variable("False"), First(Variable(base_name))),
                [Precondition(FALSE)],
            ),
            Pragma("Warnings", ["On", '"precondition is statically false"']),
        ]

    def __integer_conversion_functions(self, integer: Integer) -> Sequence[Subprogram]:
        return [
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Base",
                    self.prefix * full_base_type_name(integer),
                    [Parameter(["Val"], self.prefix * integer.identifier)],
                ),
                Variable("Val"),
                [Precondition(Call(f"Valid", [Variable("Val")]))],
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "To_Actual",
                    self.prefix * integer.identifier,
                    [Parameter(["Val"], self.prefix * full_base_type_name(integer))],
                ),
                Variable("Val"),
                [Precondition(Call(f"Valid", [Variable("Val")]))],
            ),
        ]

    def __is_seen_type(self, field_type: Type) -> bool:
        seen = field_type.identifier in self.seen_types
        self.seen_types.add(field_type.identifier)
        return seen


class InternalError(Exception):
    pass


def create_file(filename: Path, content: str) -> None:
    log.info("Creating %s", filename)

    with open(filename, "w") as f:
        f.write(content)


def modular_types(integer: ModularInteger) -> List[TypeDeclaration]:
    return [ModularType(integer.name, integer.modulus)]


def range_types(integer: RangeInteger) -> List[TypeDeclaration]:
    return [
        RangeType(base_type_name(integer), integer.base_first, integer.base_last, integer.size),
        RangeSubtype(integer.name, base_type_name(integer), integer.first, integer.last),
    ]


def enumeration_types(enum: Enumeration) -> List[TypeDeclaration]:
    types: List[TypeDeclaration] = []

    types.append(ModularType(base_type_name(enum), Pow(Number(2), enum.size)))
    types.append(
        EnumerationType(
            enum_name(enum) if enum.always_valid else enum.name, enum.literals, enum.size
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
                        Variant([TRUE], [Component("Enum", enum_name(enum))]),
                        Variant([FALSE], [Component("Raw", base_type_name(enum))]),
                    ],
                ),
            )
        )

    return types


def generic_name(identifier: ID) -> ID:
    return ID([*identifier.parts[:-1], f"Generic_{identifier.parts[-1]}"])


def contains_function_name(refinement: Refinement) -> str:
    sdu_name = str(
        refinement.sdu.name
        if refinement.sdu.package == refinement.package
        else refinement.sdu.full_name
    )
    pdu_name = str(
        refinement.pdu.name
        if refinement.pdu.package == refinement.package
        else refinement.pdu.full_name
    )
    return flat_name(f"{sdu_name}_In_{pdu_name}_{refinement.field.name}")


def unreachable_function_name(type_name: str) -> str:
    return f"Unreachable_{flat_name(type_name)}"


def switch_update_conditions(message: Message, field: Field) -> Sequence[Expr]:
    return [
        Not(Call("Has_Buffer", [Variable("Ctx")])),
        Call(f"{sequence_name(message, field)}.Has_Buffer", [Variable("Seq_Ctx")]),
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


def refinement_conditions(
    refinement: Refinement, pdu_context: str, condition_fields: Mapping[Field, Type], null_sdu: bool
) -> Sequence[Expr]:
    pdu_name = flat_name(refinement.pdu.full_name)

    conditions: List[Expr] = [Call(f"{pdu_name}.Has_Buffer", [Variable(pdu_context)])]

    if null_sdu:
        conditions.extend(
            [
                Call(
                    f"{pdu_name}.Structural_Valid",
                    [
                        Variable(pdu_context),
                        Variable(f"{pdu_name}.{refinement.field.affixed_name}"),
                    ],
                ),
                Not(
                    Call(
                        f"{pdu_name}.Present",
                        [
                            Variable(pdu_context),
                            Variable(f"{pdu_name}.{refinement.field.affixed_name}"),
                        ],
                    )
                ),
            ]
        )
    else:
        conditions.append(
            Call(
                f"{pdu_name}.Present",
                [Variable(pdu_context), Variable(f"{pdu_name}.{refinement.field.affixed_name}")],
            )
        )

    conditions.extend(
        [
            Call(
                f"{pdu_name}.Valid",
                [Variable(pdu_context), Variable(f"{pdu_name}.{f.affixed_name}")],
            )
            for f in condition_fields
        ]
    )

    return conditions
