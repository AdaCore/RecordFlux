# pylint: disable=too-many-lines
import itertools
from datetime import date
from pathlib import Path
from typing import Dict, List, Mapping, Sequence, Set, Tuple, cast

import pkg_resources

from rflx import __version__
from rflx.ada import (
    AccessParameter,
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
    SubprogramRenamingDeclaration,
    SubprogramSpecification,
    SubprogramUnitPart,
    Subtype,
    TypeDeclaration,
    Unit,
    UnitPart,
    UsePackageClause,
    UseTypeClause,
    Variant,
    VariantPart,
    WithClause,
)
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
    Name,
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
from rflx.model import (
    FINAL,
    INITIAL,
    Array,
    Composite,
    DerivedMessage,
    Enumeration,
    Field,
    Message,
    ModularInteger,
    Payload,
    RangeInteger,
    Reference,
    Refinement,
    Scalar,
    Type,
)

from .common import (
    NULL,
    VALID_CONTEXT,
    GeneratorCommon,
    base_type_name,
    length_dependent_condition,
    sequence_name,
)
from .generator import GeneratorGenerator
from .parser import ParserGenerator
from .types import Types

TEMPLATE_DIR = ("rflx", "templates/")
LIBRARY_FILES = (
    "lemmas.ads",
    "lemmas.adb",
    "types.ads",
    "types.adb",
    "message_sequence.ads",
    "message_sequence.adb",
    "scalar_sequence.ads",
    "scalar_sequence.adb",
)


class Generator:
    # pylint: disable=too-many-instance-attributes
    def __init__(self, prefix: str = "", reproducible: bool = False) -> None:
        self.prefix = prefix
        self.reproducible = reproducible
        self.units: Dict[str, Unit] = {}
        self.messages: Dict[str, Message] = {}
        self.types = Types(prefix)
        self.common = GeneratorCommon(prefix)
        self.parser = ParserGenerator(prefix)
        self.generator = GeneratorGenerator(prefix)

        self.template_dir = Path(pkg_resources.resource_filename(*TEMPLATE_DIR))
        if not self.template_dir.is_dir():
            raise InternalError("template directory not found")

    def generate(self, messages: List[Message], refinements: List[Refinement]) -> None:
        self.__process_messages(messages)
        self.__process_refinements(refinements)

    def write_library_files(self, directory: Path) -> List[Path]:
        written_files = []

        if self.prefix:
            prefix = self.prefix[:-1]
            filename = prefix.lower() + ".ads"
            file_path = Path(directory).joinpath(filename)

            with open(file_path, "w") as library_file:
                library_file.write(self.license_header() + f"package {prefix} is\n\nend {prefix};")
                written_files.append(file_path)

        for template_filename in LIBRARY_FILES:
            self.check_template_file(template_filename)

            filename = self.prefix.replace(".", "-").lower() + template_filename
            file_path = Path(directory).joinpath(filename)

            with open(self.template_dir.joinpath(template_filename)) as template_file:
                with open(file_path, "w") as library_file:
                    library_file.write(
                        self.license_header() + template_file.read().format(prefix=self.prefix)
                    )
                    written_files.append(file_path)

        return written_files

    def write_units(self, directory: Path) -> List[Path]:
        written_files = []

        for unit in self.units.values():
            filename = directory.joinpath(unit.name + ".ads")
            written_files.append(filename)
            with open(filename, "w") as f:
                f.write(self.license_header() + unit.specification)

            if unit.body:
                filename = directory.joinpath(unit.name + ".adb")
                written_files.append(filename)
                with open(filename, "w") as f:
                    f.write(self.license_header() + unit.body)

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
            pdu_package = self.prefix + refinement.pdu.rsplit(".", 1)[0]
            if pdu_package != refinement.package:
                context.extend([WithClause(pdu_package), UsePackageClause(pdu_package)])
            context.extend(
                [
                    WithClause(f"{self.prefix}{refinement.pdu}"),
                    UseTypeClause(f"{self.prefix}{refinement.pdu}.Field_Cursors"),
                ]
            )
            if not null_sdu:
                context.append(WithClause(f"{self.prefix}{refinement.sdu}"))

            contains_package = f"{refinement.package}.Contains"
            if contains_package in self.units:
                self.units[contains_package].context.extend(context)
            else:
                self.__create_unit(contains_package, context, False)
                self.units[contains_package] += self.__create_specification_pragmas("Contains")

            condition_fields = {
                f: t
                for f, t in self.messages[refinement.pdu].types.items()
                if Variable(f.name) in refinement.condition
            }

            self.units[contains_package] += self.__create_contains_function(
                refinement, condition_fields, null_sdu
            )
            if not null_sdu:
                self.units[contains_package] += self.__create_switch_procedure(
                    refinement, condition_fields
                )

    def __process_message(self, message: Message, seen_types: Set[str]) -> None:
        if not message.fields:
            return

        if message.package not in self.units:
            self.__create_unit(message.package, [], True)

        self.__create_generic_message_unit(message)
        self.__create_message_unit(message)

        for field_type in message.types.values():
            if not is_seen_type(f"{message.package}.{field_type}", seen_types):
                self.__create_type(field_type, message.package)

    def __process_derived_message(self, message: DerivedMessage, seen_types: Set[str]) -> None:
        if message.package not in self.units:
            self.__create_unit(
                message.package, [WithClause(f"{self.prefix}{message.base_package}")], True
            )

        self.__create_message_unit(message)

        for field_type in message.types.values():
            if not is_seen_type(f"{message.package}.{field_type.name}", seen_types):
                self.__create_subtype(field_type, message.package, message.base_package)

    def __create_unit(self, package_name: str, context: List[ContextItem], top_level: bool) -> None:
        name = f"{self.prefix}{package_name}"
        self.units[package_name] = PackageUnit(
            self.__common_context() + context if top_level else context,
            PackageDeclaration(name),
            PackageBody(name),
        )

    # pylint: disable=too-many-statements
    def __create_generic_message_unit(self, message: Message) -> None:
        context: List[ContextItem] = [
            WithClause(self.types.types),
            UseTypeClause(f"{self.types.types}.Integer_Address"),
        ]
        unit_name = full_generic_name(self.prefix, message.package, message.name)
        parameters: List[FormalDeclaration] = []
        unit = PackageUnit(
            context,
            PackageDeclaration(unit_name, formal_parameters=parameters),
            PackageBody(unit_name),
        )
        self.units[unit_name] = unit

        for field_type in message.types.values():
            if isinstance(field_type, Array):
                if isinstance(field_type.element_type, Reference):
                    name = "Message_Sequence"
                else:
                    name = "Scalar_Sequence"
                context.append(WithClause(f"{self.prefix}{name}"))
                parameters.append(
                    FormalPackageDeclaration(f"{field_type.name}_Sequence", f"{self.prefix}{name}")
                )

        scalar_fields = {f: t for f, t in message.types.items() if isinstance(t, Scalar)}
        composite_fields = [f for f, t in message.types.items() if isinstance(t, Composite)]
        sequence_fields = {f: t for f, t in message.types.items() if isinstance(t, Array)}

        context_invariant = [
            Equal(e, Old(e))
            for e in (
                Name("Ctx.Buffer_First"),
                Name("Ctx.Buffer_Last"),
                Name("Ctx.First"),
                Name("Ctx.Last"),
            )
        ]

        unit += self.__create_specification_pragmas(generic_name(message.name))
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
    def __create_specification_pragmas(package_name: str) -> UnitPart:
        return UnitPart([Pragma("Annotate", ["GNATprove", "Terminating", package_name])])

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
                    Name(message.all_fields[1].affixed_name),
                    Name(message.all_fields[-2].affixed_name),
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
        discriminants = [Discriminant(["State"], "Cursor_State", Name("S_Invalid"))]

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
                        Selected("Val", "Fld"),
                        [
                            (
                                Name(f.affixed_name),
                                Call("Valid", [Name(f"Val.{f.name}_Value")])
                                if isinstance(t, Scalar)
                                else TRUE,
                            )
                            for f, t in message.types.items()
                        ]
                        + [(Name(INITIAL.affixed_name), FALSE), (Name(FINAL.affixed_name), FALSE)],
                    ),
                ),
                RecordType(
                    "Field_Cursor",
                    [Component("Predecessor", "Virtual_Field", Name(FINAL.affixed_name))],
                    discriminants,
                    VariantPart(
                        "State",
                        [
                            Variant(
                                [Name("S_Valid"), Name("S_Structural_Valid")],
                                [
                                    Component(
                                        "First", self.types.bit_index, First(self.types.bit_index)
                                    ),
                                    Component(
                                        "Last", self.types.bit_length, First(self.types.bit_length)
                                    ),
                                    Component(
                                        "Value",
                                        "Field_Dependent_Value",
                                        NamedAggregate(("Fld", Name(FINAL.affixed_name))),
                                    ),
                                ],
                            ),
                            Variant([Name("S_Invalid"), Name("S_Incomplete")], [NullComponent()]),
                        ],
                    ),
                    [
                        DynamicPredicate(
                            If(
                                [
                                    (
                                        Or(
                                            Equal(Name("State"), Name("S_Valid")),
                                            Equal(Name("State"), Name("S_Structural_Valid")),
                                        ),
                                        Call("Valid_Value", [Selected("Field_Cursor", "Value")]),
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
                ["Buffer_First", "Buffer_Last"], self.types.index, First(self.types.index)
            ),
            Discriminant(["First", "Last"], self.types.bit_index, First(self.types.bit_index)),
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
                                        ("State", Name("S_Invalid")),
                                        ("Predecessor", Name(FINAL.affixed_name)),
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
                                    Name("Buffer_First"),
                                    Name("Buffer_Last"),
                                    Name("First"),
                                    Name("Last"),
                                    Name("Buffer"),
                                    Name("Cursors"),
                                ],
                            )
                        )
                    ],
                )
            ],
        )

    @staticmethod
    def __create_field_dependent_type(
        scalar_fields: Mapping[Field, Scalar], composite_fields: Sequence[Field]
    ) -> UnitPart:
        result_variants = [
            Variant(
                [Name(f.affixed_name) for f in [INITIAL, *composite_fields, FINAL]],
                [NullComponent()],
            )
        ] + [
            Variant([Name(f.affixed_name)], [Component(f"{f.name}_Value", base_type_name(t))])
            for f, t in scalar_fields.items()
        ]

        return UnitPart(
            [
                RecordType(
                    "Field_Dependent_Value",
                    [],
                    [Discriminant(["Fld"], "Virtual_Field", Name(INITIAL.affixed_name))],
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
                        First(self.types.index),
                        First(self.types.index),
                        First(self.types.bit_index),
                        First(self.types.bit_index),
                        NULL,
                        NamedAggregate(
                            (
                                message.fields[0].affixed_name,
                                NamedAggregate(
                                    ("State", Name("S_Invalid")),
                                    ("Predecessor", Name(INITIAL.affixed_name)),
                                ),
                            ),
                            (
                                "others",
                                NamedAggregate(
                                    ("State", Name("S_Invalid")),
                                    ("Predecessor", Name(FINAL.affixed_name)),
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
                                Not(Constrained("Ctx")),
                                NotEqual(Name("Buffer"), NULL),
                                # WORKAROUND: Componolit/Workarounds#10
                                Greater(Length("Buffer"), Number(0)),
                                LessEqual(Last("Buffer"), Div(Last(self.types.index), Number(2))),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Call("Has_Buffer", [Name("Ctx")]),
                                Equal(Name("Buffer"), NULL),
                                # WORKAROUND: Componolit/Workarounds#6
                                Equal(
                                    Selected("Ctx", "Buffer_First"),
                                    Old(Call(f"{self.types.types}.Bytes_First", [Name("Buffer")])),
                                ),
                                Equal(
                                    Selected("Ctx", "Buffer_Last"),
                                    Old(Call(f"{self.types.types}.Bytes_Last", [Name("Buffer")])),
                                ),
                                Equal(
                                    Selected("Ctx", "First"),
                                    Call(
                                        f"{self.types.types}.First_Bit_Index",
                                        [Selected("Ctx", "Buffer_First")],
                                    ),
                                ),
                                Call("Initialized", [Name("Ctx")]),
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
                                Name("Ctx"),
                                Name("Buffer"),
                                Call(self.types.first_bit_index, [First("Buffer")]),
                                Call(self.types.last_bit_index, [Last("Buffer")]),
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
                                Not(Constrained("Ctx")),
                                NotEqual(Name("Buffer"), NULL),
                                # WORKAROUND: Componolit/Workarounds#10
                                Greater(Length("Buffer"), Number(0)),
                                GreaterEqual(
                                    Call(self.types.byte_index, [Name("First")]), First("Buffer")
                                ),
                                LessEqual(
                                    Call(self.types.byte_index, [Name("Last")]), Last("Buffer")
                                ),
                                LessEqual(Name("First"), Name("Last")),
                                LessEqual(Name("Last"), Div(Last(self.types.bit_index), Number(2))),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Equal(Name("Buffer"), NULL),
                                Call("Has_Buffer", [Name("Ctx")]),
                                # WORKAROUND: Componolit/Workarounds#6
                                Equal(
                                    Selected("Ctx", "Buffer_First"),
                                    Old(Call(f"{self.types.types}.Bytes_First", [Name("Buffer")])),
                                ),
                                Equal(
                                    Selected("Ctx", "Buffer_Last"),
                                    Old(Call(f"{self.types.types}.Bytes_Last", [Name("Buffer")])),
                                ),
                                Equal(Name("Ctx.First"), Name("First")),
                                Equal(Name("Ctx.Last"), Name("Last")),
                                Call("Initialized", [Name("Ctx")]),
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
                            ["Buffer_First"], self.types.index, First("Buffer"), True
                        ),
                        ObjectDeclaration(["Buffer_Last"], self.types.index, Last("Buffer"), True),
                    ],
                    [
                        Assignment(
                            "Ctx",
                            Aggregate(
                                Name("Buffer_First"),
                                Name("Buffer_Last"),
                                Name("First"),
                                Name("Last"),
                                Name("Buffer"),
                                NamedAggregate(
                                    (
                                        message.fields[0].affixed_name,
                                        NamedAggregate(
                                            ("State", Name("S_Invalid")),
                                            ("Predecessor", Name(INITIAL.affixed_name)),
                                        ),
                                    ),
                                    (
                                        "others",
                                        NamedAggregate(
                                            ("State", Name("S_Invalid")),
                                            ("Predecessor", Name(FINAL.affixed_name)),
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
                        Call("Valid_Next", [Name("Ctx"), Name(message.fields[0].affixed_name)]),
                        Equal(
                            Call(
                                "Available_Space",
                                [Name("Ctx"), Name(message.fields[0].affixed_name)],
                            ),
                            Add(
                                Call(
                                    f"{self.types.types}.Last_Bit_Index",
                                    [Selected("Ctx", "Buffer_Last")],
                                ),
                                -Selected("Ctx", "First"),
                                Number(1),
                            ),
                        ),
                        *[
                            Call("Invalid", [Name("Ctx"), Name(f.affixed_name)])
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
                        Precondition(And(VALID_CONTEXT, Call("Has_Buffer", [Name("Ctx")]))),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Not(Call("Has_Buffer", [Name("Ctx")])),
                                NotEqual(Name("Buffer"), NULL),
                                Equal(Name("Ctx.Buffer_First"), First("Buffer")),
                                Equal(Name("Ctx.Buffer_Last"), Last("Buffer")),
                                *context_invariant,
                                *[Equal(e, Old(e)) for e in [Call("Cursors", [Name("Ctx")])]],
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [Assignment("Buffer", Name("Ctx.Buffer")), Assignment("Ctx.Buffer", NULL)],
                )
            ],
        )

    def __create_path_condition_function(self, message: Message) -> UnitPart:
        def condition(field: Field, message: Message) -> Expr:
            cases: List[Tuple[Expr, Expr]] = [
                (target, Or(*[c for _, c in conditions]))
                for target, conditions in itertools.groupby(
                    [
                        (Name(l.target.affixed_name), l.condition)
                        for l in message.outgoing(field)
                        if l.target != FINAL
                    ],
                    lambda x: x[0],
                )
            ]
            cases.append((Name("others"), FALSE))
            return Case(Name("Fld"), cases).simplified(self.common.substitution(message))

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
                                VALID_CONTEXT, Call("Valid_Predecessor", [Name("Ctx"), Name("Fld")])
                            )
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Selected(Indexed(Selected("Ctx", "Cursors"), Name("Fld")), "Predecessor"),
                        [(Name(f.affixed_name), condition(f, message)) for f in message.all_fields],
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
                    length = Size(base_type_name(field_type))
                else:
                    if len(links) == 1:
                        length = links[0].length
                    elif len(links) > 1:
                        length = If(
                            [(l.condition, l.length) for l in links],
                            Name(self.types.unreachable_bit_length),
                        ).simplified(self.common.substitution(message))
                cases.append((Name(target.affixed_name), length))

            if not cases:
                return Number(0)

            cases.append((Name("others"), Name(self.types.unreachable_bit_length)))
            return Case(Name("Fld"), cases).simplified(self.common.substitution(message))

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
                            And(VALID_CONTEXT, Call("Valid_Next", [Name("Ctx"), Name("Fld")]),)
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Selected(Indexed(Selected("Ctx", "Cursors"), Name("Fld")), "Predecessor"),
                        [(Name(f.affixed_name), length(f, message)) for f in message.all_fields],
                    ),
                )
            ],
        )

    def __create_field_first_function(self, message: Message) -> UnitPart:
        def first(field: Field, message: Message) -> Expr:
            if field == message.fields[0]:
                return Selected("Ctx", "First")

            contiguous_first = Add(
                Selected(
                    Indexed(
                        Selected("Ctx", "Cursors"),
                        Selected(Indexed(Selected("Ctx", "Cursors"), Name("Fld")), "Predecessor"),
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
                                    Indexed(Selected("Ctx", "Cursors"), Name("Fld")), "Predecessor"
                                ),
                                Name(l.source.affixed_name),
                            ),
                            l.condition,
                        ),
                        l.first.simplified({UNDEFINED: contiguous_first}),
                    )
                    for l in message.incoming(field)
                ],
                Name(self.types.unreachable_bit_length),
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
                            And(VALID_CONTEXT, Call("Valid_Next", [Name("Ctx"), Name("Fld")]),)
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Name("Fld"),
                        [(Name(f.affixed_name), first(f, message)) for f in message.fields],
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
                    [Precondition(And(Call("Valid_Next", [Name("Ctx"), Name("Fld")]),))],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Add(
                        Call("Field_First", [Name("Ctx"), Name("Fld")]),
                        Call("Field_Length", [Name("Ctx"), Name("Fld")]),
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
                            self.types.bit_length, [Selected("Val", f"{field.name}_Value")]
                        ),
                        Length(field.name): Name("Length"),
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
                                In(Selected("Val", "Fld"), Range("Field")),
                                Call("Valid_Predecessor", [Name("Ctx"), Selected("Val", "Fld")]),
                            )
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Selected("Val", "Fld"),
                        [(Name(f.affixed_name), condition(f, message)) for f in message.all_fields],
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
                        Name("Fld"),
                        [
                            (Name(INITIAL.affixed_name), Name(INITIAL.affixed_name)),
                            (
                                Name("others"),
                                Selected(
                                    Indexed(Selected("Ctx", "Cursors"), Name("Fld")), "Predecessor"
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
                        Name("Fld"),
                        [
                            (
                                Name(f.affixed_name),
                                If(
                                    [
                                        (l.condition, Name(l.target.affixed_name))
                                        for l in message.outgoing(f)
                                    ],
                                    Name(INITIAL.affixed_name),
                                ).simplified(self.common.substitution(message)),
                            )
                            for f in message.fields
                        ],
                    ),
                    [
                        Precondition(
                            And(
                                Call("Structural_Valid", [Name("Ctx"), Name("Fld")]),
                                Call("Valid_Predecessor", [Name("Ctx"), Name("Fld")]),
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
                        Name("Fld"),
                        [
                            (
                                Name(f.affixed_name),
                                And(
                                    *[
                                        Call(
                                            "Invalid",
                                            [
                                                Indexed(
                                                    Selected("Ctx", "Cursors"), Name(s.affixed_name)
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
                        Equal(Call("Field_First", [Name("Ctx"), Name("Fld")]), Name("First")),
                        Equal(Call("Field_Length", [Name("Ctx"), Name("Fld")]), Name("Length")),
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
                            Call("Field_First", [Name("Ctx"), Name("Fld")]),
                            True,
                            [Ghost()],
                        ),
                        ObjectDeclaration(
                            ["Length"],
                            self.types.bit_length,
                            Call("Field_Length", [Name("Ctx"), Name("Fld")]),
                            True,
                            [Ghost()],
                        ),
                    ],
                    [
                        field_location_invariant,
                        CaseStatement(
                            Name("Fld"),
                            [
                                (
                                    Name(f.affixed_name),
                                    cast(List[Statement], [])
                                    + [
                                        Assignment(
                                            Indexed(
                                                Selected("Ctx", "Cursors"), Name(s.affixed_name)
                                            ),
                                            Aggregate(Name("S_Invalid"), Name(FINAL.affixed_name)),
                                        )
                                        for s in reversed(message.successors(f))
                                    ]
                                    + [
                                        Assignment(
                                            Indexed(
                                                Selected("Ctx", "Cursors"), Name(f.affixed_name)
                                            ),
                                            Aggregate(
                                                Name("S_Invalid"),
                                                Selected(
                                                    Indexed(
                                                        Selected("Ctx", "Cursors"),
                                                        Name(f.affixed_name),
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
                        Precondition(And(Call("Valid_Next", [Name("Ctx"), Name("Fld")]),),),
                        Postcondition(
                            And(
                                Call("Valid_Next", [Name("Ctx"), Name("Fld")]),
                                Call("Invalid", [Indexed(Selected("Ctx", "Cursors"), Name("Fld"))]),
                                Call("Invalid_Successor", [Name("Ctx"), Name("Fld")])
                                if len(message.fields) > 1
                                else TRUE,
                                *context_invariant,
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected(
                                            Indexed(Selected("Ctx", "Cursors"), Name("Fld")),
                                            "Predecessor",
                                        ),
                                        Call("Has_Buffer", [Name("Ctx")]),
                                        Call("Field_First", [Name("Ctx"), Name("Fld")]),
                                        Call("Field_Length", [Name("Ctx"), Name("Fld")]),
                                    ]
                                ],
                                Case(
                                    Name("Fld"),
                                    [
                                        (
                                            Name(f.affixed_name),
                                            And(
                                                *[
                                                    Equal(
                                                        Indexed(
                                                            Selected("Ctx", "Cursors"),
                                                            Name(p.affixed_name),
                                                        ),
                                                        Old(
                                                            Indexed(
                                                                Selected("Ctx", "Cursors"),
                                                                Name(p.affixed_name),
                                                            )
                                                        ),
                                                    )
                                                    for p in message.predecessors(f)
                                                ],
                                                *[
                                                    Call(
                                                        "Invalid",
                                                        [Name("Ctx"), Name(s.affixed_name)],
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
            [ExpressionFunctionDeclaration(specification, NotEqual(Name("Ctx.Buffer"), NULL))],
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
                        Name("Fld"),
                        [
                            (
                                Name(f.affixed_name),
                                Or(
                                    *[
                                        And(
                                            Call(
                                                "Structural_Valid"
                                                if p in composite_fields
                                                else "Valid",
                                                [
                                                    Indexed(
                                                        Selected("Ctx", "Cursors"),
                                                        Name(p.affixed_name),
                                                    )
                                                ],
                                            )
                                            if p != INITIAL
                                            else TRUE,
                                            Equal(
                                                Selected(
                                                    Indexed(
                                                        Selected("Ctx", "Cursors"), Name("Fld")
                                                    ),
                                                    "Predecessor",
                                                ),
                                                Name(p.affixed_name),
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
                            And(VALID_CONTEXT, Call("Structural_Valid_Message", [Name("Ctx")]))
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
                                                Selected("Ctx", "Cursors"),
                                                Name(l.source.affixed_name),
                                            )
                                        ],
                                    ),
                                    l.condition,
                                ).simplified(self.common.substitution(message)),
                                Selected(
                                    Indexed(
                                        Selected("Ctx", "Cursors"), Name(l.source.affixed_name)
                                    ),
                                    "Last",
                                ),
                            )
                            for l in message.incoming(FINAL)
                        ],
                        Name(self.types.unreachable_bit_length),
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
                            And(VALID_CONTEXT, Call("Valid_Next", [Name("Ctx"), Name("Fld")]),)
                        )
                    ],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Add(
                        Call(self.types.last_bit_index, [Selected("Ctx", "Buffer_Last")]),
                        -Call("Field_First", [Name("Ctx"), Name("Fld")]),
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
                                Not(Constrained("Ctx")),
                                Not(Constrained("Seq_Ctx")),
                                Call("Has_Buffer", [Name("Ctx")]),
                                Call("Valid_Next", [Name("Ctx"), Name(f.affixed_name)]),
                                Greater(
                                    Call("Field_Length", [Name("Ctx"), Name(f.affixed_name)]),
                                    Number(0),
                                ),
                                LessEqual(
                                    Call("Field_Last", [Name("Ctx"), Name(f.affixed_name)]),
                                    Div(Last(self.types.bit_index), Number(2)),
                                ),
                                Call(
                                    "Field_Condition",
                                    [Name("Ctx"), NamedAggregate(("Fld", Name(f.affixed_name)))]
                                    + (
                                        [
                                            Call(
                                                "Field_Length", [Name("Ctx"), Name(f.affixed_name)],
                                            ),
                                        ]
                                        if length_dependent_condition(message)
                                        else []
                                    ),
                                ),
                                self.common.sufficient_space_for_field_condition(
                                    Name(f.affixed_name)
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                *switch_update_conditions(message, f),
                                Equal(
                                    Call(f"{sequence_name(message, f)}.Index", [Name("Seq_Ctx")]),
                                    Selected("Seq_Ctx", "First"),
                                ),
                                Call("Present", [Name("Ctx"), Name(f.affixed_name)]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected("Ctx", "Buffer_First"),
                                        Selected("Ctx", "Buffer_Last"),
                                        Selected("Ctx", "First"),
                                        Call("Predecessor", [Name("Ctx"), Name(f.affixed_name)]),
                                        Call("Path_Condition", [Name("Ctx"), Name(f.affixed_name)]),
                                    ]
                                    + [
                                        Call(f"Cursor", [Name("Ctx"), Name(p.affixed_name)])
                                        for p in message.predecessors(f)
                                    ]
                                ],
                            )
                        ),
                        ContractCases(
                            (
                                Call("Structural_Valid", [Name("Ctx"), Name(f.affixed_name)]),
                                And(
                                    *[
                                        Equal(
                                            Call("Cursor", [Name("Ctx"), Name(s.affixed_name)]),
                                            Old(
                                                Call("Cursor", [Name("Ctx"), Name(s.affixed_name)])
                                            ),
                                        )
                                        for s in message.successors(f)
                                    ]
                                ),
                            ),
                            (
                                Name("others"),
                                And(
                                    *self.common.valid_path_to_next_field_condition(message, f, t),
                                    *[
                                        Call("Invalid", [Name("Ctx"), Name(s.affixed_name)])
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
                        *self.common.field_bit_location_declarations(Name(f.affixed_name)),
                        ObjectDeclaration(["Buffer"], self.types.bytes_ptr),
                    ],
                    [
                        IfStatement(
                            [
                                (
                                    Call("Invalid", [Name("Ctx"), Name(f.affixed_name)]),
                                    self.common.initialize_field_statements(message, f),
                                )
                            ]
                        ),
                        CallStatement("Take_Buffer", [Name("Ctx"), Name("Buffer")]),
                        PragmaStatement("Warnings", ["Off", '"unused assignment to ""Buffer"""']),
                        CallStatement(
                            f"{sequence_name(message, f)}.Initialize",
                            [
                                Name("Seq_Ctx"),
                                Name("Buffer"),
                                Name("Ctx.Buffer_First"),
                                Name("Ctx.Buffer_Last"),
                                Name("First"),
                                Name("Last"),
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
                Name("Seq_Ctx"),
                Name("Buffer"),
                Name("Ctx.Buffer_First"),
                Name("Ctx.Buffer_Last"),
            ]

            field_type = message.types[field]
            assert isinstance(field_type, Array)

            if isinstance(field_type.element_type, Reference):
                arguments.extend([Name("Ctx.First"), Name("Ctx.Last")])

            return arguments

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                VALID_CONTEXT,
                                Call("Present", [Name("Ctx"), Name(f.affixed_name)]),
                                *switch_update_conditions(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Call("Present", [Name("Ctx"), Name(f.affixed_name)]),
                                Call("Has_Buffer", [Name("Ctx")]),
                                Not(
                                    Call(
                                        f"{sequence_name(message, f)}.Has_Buffer", [Name("Seq_Ctx")]
                                    )
                                ),
                                Equal(
                                    Selected("Seq_Ctx", "First"),
                                    Call("Field_First", [Name("Ctx"), Name(f.affixed_name)]),
                                ),
                                Equal(
                                    Selected("Seq_Ctx", "Last"),
                                    Call("Field_Last", [Name("Ctx"), Name(f.affixed_name)]),
                                ),
                                *[
                                    Equal(e, Old(e))
                                    for e in cast(List[Expr], [])
                                    + [
                                        Selected("Seq_Ctx", "First"),
                                        Selected("Seq_Ctx", "Last"),
                                        Selected("Ctx", "Buffer_First"),
                                        Selected("Ctx", "Buffer_Last"),
                                        Call("Field_First", [Name("Ctx"), Name(f.affixed_name)]),
                                        Call("Field_Length", [Name("Ctx"), Name(f.affixed_name)]),
                                    ]
                                    + [
                                        Call(f"Cursor", [Name("Ctx"), Name(o.affixed_name)])
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
                            Call(f"{sequence_name(message, f)}.Valid", [Name("Seq_Ctx")]),
                            True,
                        ),
                        ObjectDeclaration(["Buffer"], self.types.bytes_ptr),
                    ],
                    [
                        CallStatement(
                            f"{sequence_name(message, f)}.Take_Buffer", take_buffer_arguments(f)
                        ),
                        Assignment("Ctx.Buffer", Name("Buffer")),
                        IfStatement(
                            [
                                (
                                    Name("Valid_Sequence"),
                                    [
                                        Assignment(
                                            Indexed("Ctx.Cursors", Name(f.affixed_name)),
                                            NamedAggregate(
                                                ("State", Name("S_Valid")),
                                                *[
                                                    (
                                                        a,
                                                        Selected(
                                                            Indexed(
                                                                "Ctx.Cursors", Name(f.affixed_name)
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
                        Equal(Selected("Cursor", "State"), Name("S_Valid")),
                        Equal(Selected("Cursor", "State"), Name("S_Structural_Valid")),
                    ),
                ),
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Valid", "Boolean", parameters),
                    Equal(Selected("Cursor", "State"), Name("S_Valid")),
                ),
                ExpressionFunctionDeclaration(
                    FunctionSpecification("Invalid", "Boolean", parameters),
                    Or(
                        Equal(Selected("Cursor", "State"), Name("S_Invalid")),
                        Equal(Selected("Cursor", "State"), Name("S_Incomplete")),
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
                AccessParameter(["Buffer"], self.types.bytes, constant=True),
                Parameter(["Cursors"], "Field_Cursors"),
            ],
        )

        return UnitPart(
            [],
            [],
            [
                ExpressionFunctionDeclaration(
                    specification, self.common.context_predicate(message, composite_fields)
                )
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
                            Name("Ctx.Buffer_First"),
                            Name("Ctx.Buffer_Last"),
                            Name("Ctx.First"),
                            Name("Ctx.Last"),
                            Name("Ctx.Buffer"),
                            Name("Ctx.Cursors"),
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
                    specification, Indexed(Selected("Ctx", "Cursors"), Name("Fld"))
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
            [ExpressionFunctionDeclaration(specification, Selected("Ctx", "Cursors"))],
        )

    def __create_message_unit(self, message: Message) -> None:
        if isinstance(message, DerivedMessage):
            name = full_generic_name(self.prefix, message.base_package, message.base_name)
        else:
            name = full_generic_name(self.prefix, message.package, message.name)

        arrays = [
            message.types[f].name for f in message.fields if isinstance(message.types[f], Array)
        ]

        context: List[ContextItem] = [Pragma("SPARK_Mode"), WithClause(name)]
        context.extend(WithClause(f"{self.prefix}{message.package}.{array}") for array in arrays)
        instantiation = GenericPackageInstantiation(
            f"{self.prefix}{message.full_name}", name, arrays
        )
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
                subprograms = [
                    s
                    for s in self.__modular_functions(field_type).specification
                    if isinstance(s, Subprogram)
                ]
            elif isinstance(field_type, RangeInteger):
                types = range_types(field_type)
                subprograms = self.__range_functions(field_type).specification
            elif isinstance(field_type, Enumeration):
                types = enumeration_types(field_type)
                subprograms = [
                    s
                    for s in self.__enumeration_functions(field_type).specification
                    if isinstance(s, Subprogram)
                ]

            unit += UnitPart([Subtype(t.name, f"{base_package}.{t.name}") for t in types])
            unit += UnitPart(
                [
                    SubprogramRenamingDeclaration(
                        renamed_subprogram_specification(s.specification, s.name),
                        f"{base_package}.{s.name}",
                    )
                    for s in subprograms
                ]
            )

            if isinstance(field_type, Enumeration):
                type_name = field_type.enum_name if field_type.always_valid else field_type.name
                unit += UnitPart(
                    [
                        ObjectDeclaration(
                            [literal], type_name, Name(f"{base_package}.{literal}"), True
                        )
                        for literal in field_type.literals
                    ]
                )

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
            array_context = [
                Pragma("SPARK_Mode"),
                WithClause(f"{self.prefix}Message_Sequence"),
                WithClause(f"{self.prefix}{element_type.full_name}"),
            ]
            array_package = GenericPackageInstantiation(
                f"{self.prefix}{array_type.full_name}",
                "Message_Sequence",
                [
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
        else:
            array_context = [
                Pragma("SPARK_Mode"),
                WithClause(f"{self.prefix}Scalar_Sequence"),
                WithClause(f"{self.prefix}{package_name}"),
            ]
            array_package = GenericPackageInstantiation(
                f"{self.prefix}{package_name}.{array_type.name}",
                f"{self.prefix}Scalar_Sequence",
                [
                    element_type.name,
                    element_type.base_name
                    if not isinstance(element_type, ModularInteger)
                    else element_type.name,
                    "Extract",
                    "Insert",
                    "Valid",
                    "Convert",
                    "Convert",
                ],
            )

        self.units[array_package.name] = InstantiationUnit(array_context, array_package)

    def __common_context(self) -> List[ContextItem]:
        return [
            WithClause(self.types.types),
            UseTypeClause(
                self.types.bytes,
                self.types.bytes_ptr,
                self.types.index,
                self.types.length,
                self.types.bit_index,
                self.types.bit_length,
            ),
        ]

    def __range_functions(self, integer: RangeInteger) -> SubprogramUnitPart:
        specification: List[Subprogram] = []

        for range_type in range_types(integer):
            if isinstance(range_type, RangeSubtype):
                continue

            specification.append(self.parser.extract_function(range_type.name))
            specification.append(self.generator.insert_function(range_type.name))

        specification.append(
            type_validation_function(integer, integer.constraints("Val").simplified())
        )
        specification.append(integer_conversion_function(integer.full_name, integer.full_base_name))

        return SubprogramUnitPart(specification)

    def __modular_functions(self, integer: ModularInteger) -> UnitPart:
        specification: List[Declaration] = []

        for modular_type in modular_types(integer):
            specification.append(self.parser.extract_function(modular_type.name))
            specification.append(self.generator.insert_function(modular_type.name))

        specification.append(Pragma("Warnings", ["Off", '"unused variable ""Val"""']))
        specification.append(
            type_validation_function(integer, integer.constraints("Val").simplified())
        )
        specification.append(Pragma("Warnings", ["On", '"unused variable ""Val"""']))
        specification.append(integer_conversion_function(integer.full_name, integer.full_name))

        return UnitPart(specification)

    def __enumeration_functions(self, enum: Enumeration) -> UnitPart:
        specification: List[Declaration] = []

        specification.append(self.parser.extract_function(enum.full_base_name))
        specification.append(self.generator.insert_function(enum.full_base_name))

        enum_value = Name("Val")

        validation_expression: Expr
        if enum.always_valid:
            validation_expression = enum.constraints("Val").simplified()
        else:
            validation_cases: List[Tuple[Expr, Expr]] = []
            validation_cases.extend((value, Name("True")) for value in enum.literals.values())
            validation_cases.append((Name("others"), Name("False")))

            validation_expression = Case(enum_value, validation_cases)

        if enum.always_valid:
            specification.append(Pragma("Warnings", ["Off", '"unused variable ""Val"""']))
        specification.append(type_validation_function(enum, validation_expression))
        if enum.always_valid:
            specification.append(Pragma("Warnings", ["On", '"unused variable ""Val"""']))

        specification.append(
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "Convert",
                    enum.full_base_name,
                    [Parameter(["Enum"], enum.enum_name if enum.always_valid else enum.full_name)],
                ),
                Case(Name("Enum"), [(Name(key), value) for key, value in enum.literals.items()]),
            )
        )

        conversion_function = FunctionSpecification(
            "Convert", enum.full_name, [Parameter(["Val"], enum.full_base_name)]
        )
        precondition = Precondition(Call("Valid", [Name("Val")]))
        conversion_cases: List[Tuple[Expr, Expr]] = []

        if enum.always_valid:
            specification.append(
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Convert", enum.full_name, [Parameter(["Enum"], enum.enum_name)]
                    ),
                    Aggregate(TRUE, Name("Enum")),
                )
            )

            conversion_cases.extend(
                (value, Aggregate(Name("True"), Name(key))) for key, value in enum.literals.items()
            )
            conversion_cases.append((Name("others"), Aggregate(Name("False"), Name("Val"))))

            specification.append(
                ExpressionFunctionDeclaration(
                    conversion_function, Case(Name("Val"), conversion_cases), [precondition]
                )
            )

            specification.append(
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Convert", enum.full_base_name, [Parameter(["Val"], enum.full_name)]
                    ),
                    If(
                        [(Selected("Val", "Known"), Call("Convert", [Selected("Val", "Enum")]))],
                        Selected("Val", "Raw"),
                    ),
                )
            )

        else:
            conversion_cases.extend((value, Name(key)) for key, value in enum.literals.items())
            conversion_cases.append(
                (Name("others"), Call(unreachable_function_name(enum.full_name)))
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
        condition = refinement.condition
        for f, t in condition_fields.items():
            if isinstance(t, Enumeration) and t.always_valid:
                condition = AndThen(
                    Selected(Call(f"{refinement.pdu}.Get_{f.name}", [Name("Ctx")]), "Known"),
                    condition,
                )
        condition = condition.simplified(
            {
                Variable(f.name): Selected(
                    Call(f"{refinement.pdu}.Get_{f.name}", [Name("Ctx")]), "Enum"
                )
                if isinstance(t, Enumeration) and t.always_valid
                else Call(f"{refinement.pdu}.Get_{f.name}", [Name("Ctx")])
                for f, t in condition_fields.items()
            }
        )

        specification = FunctionSpecification(
            contains_function_name(refinement),
            "Boolean",
            [Parameter(["Ctx"], f"{refinement.pdu}.Context")],
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
        pdu_context = f"{refinement.pdu}_Context".replace(".", "_")
        sdu_context = f"{refinement.sdu}_Context".replace(".", "_")
        refined_field_affixed_name = f"{refinement.pdu}.{refinement.field.affixed_name}"

        specification = ProcedureSpecification(
            f"Switch_To_{refinement.field.name}",
            [
                InOutParameter([pdu_context], f"{refinement.pdu}.Context"),
                OutParameter([sdu_context], f"{refinement.sdu}.Context"),
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
                                *refinement_conditions(
                                    refinement, pdu_context, condition_fields, False
                                ),
                                Call(contains_function_name(refinement), [Name(pdu_context)]),
                            )
                        ),
                        Postcondition(
                            And(
                                Not(Call(f"{refinement.pdu}.Has_Buffer", [Name(pdu_context)])),
                                Call(f"{refinement.sdu}.Has_Buffer", [Name(sdu_context)]),
                                Equal(
                                    Selected(pdu_context, "Buffer_First"),
                                    Selected(sdu_context, "Buffer_First"),
                                ),
                                Equal(
                                    Selected(pdu_context, "Buffer_Last"),
                                    Selected(sdu_context, "Buffer_Last"),
                                ),
                                Equal(
                                    Selected(sdu_context, "First"),
                                    Call(
                                        f"{refinement.pdu}.Field_First",
                                        [Name(pdu_context), Name(refined_field_affixed_name)],
                                    ),
                                ),
                                Equal(
                                    Selected(sdu_context, "Last"),
                                    Call(
                                        f"{refinement.pdu}.Field_Last",
                                        [Name(pdu_context), Name(refined_field_affixed_name)],
                                    ),
                                ),
                                Call(f"{refinement.sdu}.Initialized", [Name(sdu_context)]),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected(pdu_context, "Buffer_First"),
                                        Selected(pdu_context, "Buffer_Last"),
                                        Selected(pdu_context, "First"),
                                        Call(f"{refinement.pdu}.Cursors", [Name(pdu_context)]),
                                    ]
                                ],
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
                            ["First"],
                            self.types.bit_index,
                            Call(
                                f"{refinement.pdu}.Field_First",
                                [Name(pdu_context), Name(refined_field_affixed_name)],
                            ),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            self.types.bit_index,
                            Call(
                                f"{refinement.pdu}.Field_Last",
                                [Name(pdu_context), Name(refined_field_affixed_name)],
                            ),
                            True,
                        ),
                        ObjectDeclaration(["Buffer"], self.types.bytes_ptr),
                    ],
                    [
                        CallStatement(
                            f"{refinement.pdu}.Take_Buffer", [Name(pdu_context), Name("Buffer")]
                        ),
                        PragmaStatement("Warnings", ["Off", '"unused assignment to ""Buffer"""']),
                        CallStatement(
                            f"{refinement.sdu}.Initialize",
                            [Name(sdu_context), Name("Buffer"), Name("First"), Name("Last")],
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
                        Call("Valid_Predecessor", [Name("Ctx"), Name("Fld")]),
                        Call("Path_Condition", [Name("Ctx"), Name("Fld")]),
                    ),
                )
            ],
        )

    def check_template_file(self, filename: str) -> None:
        if not self.template_dir.joinpath(filename).is_file():
            raise InternalError(f'template file not found: "{filename}"')

    def license_header(self) -> str:
        if self.reproducible:
            return ""

        filename = "license_header"
        self.check_template_file(filename)
        with open(self.template_dir.joinpath(filename)) as license_file:
            today = date.today()
            return license_file.read().format(version=__version__, date=today, year=today.year,)


class InternalError(Exception):
    pass


def modular_types(integer: ModularInteger) -> List[TypeDeclaration]:
    return [ModularType(integer.name, integer.modulus)]


def range_types(integer: RangeInteger) -> List[TypeDeclaration]:
    return [
        RangeType(integer.base_name, integer.base_first, integer.base_last, integer.size),
        RangeSubtype(integer.name, integer.base_name, integer.first, integer.last),
    ]


def enumeration_types(enum: Enumeration) -> List[TypeDeclaration]:
    types: List[TypeDeclaration] = []

    types.append(ModularType(enum.base_name, Pow(Number(2), enum.size)))
    types.append(
        EnumerationType(
            enum.enum_name if enum.always_valid else enum.name, enum.literals, enum.size
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
                        Variant([TRUE], [Component("Enum", enum.enum_name)]),
                        Variant([FALSE], [Component("Raw", enum.base_name)]),
                    ],
                ),
            )
        )

    return types


def integer_conversion_function(type_name: str, type_base_name: str) -> Subprogram:
    return ExpressionFunctionDeclaration(
        FunctionSpecification("Convert", type_name, [Parameter(["Val"], type_base_name)]),
        Name("Val"),
        [Precondition(Call(f"Valid", [Name("Val")]))],
    )


def type_validation_function(scalar_type: Scalar, validation_expression: Expr) -> Subprogram:
    return ExpressionFunctionDeclaration(
        FunctionSpecification(
            f"Valid", "Boolean", [Parameter(["Val"], base_type_name(scalar_type))]
        ),
        validation_expression,
    )


def renamed_subprogram_specification(
    specification: SubprogramSpecification, name: str
) -> SubprogramSpecification:
    if isinstance(specification, ProcedureSpecification):
        return ProcedureSpecification(name, specification.parameters)
    if isinstance(specification, FunctionSpecification):
        return FunctionSpecification(name, specification.return_type, specification.parameters)
    raise NotImplementedError('unhandled subprogram specification "{type(specification).__name__}"')


def unreachable_function(type_name: str, base_name: str = None) -> List[Declaration]:
    return [
        Pragma("Warnings", ["Off", '"precondition is statically false"']),
        ExpressionFunctionDeclaration(
            FunctionSpecification(unreachable_function_name(type_name), type_name),
            First(type_name) if not base_name else Aggregate(Name("False"), First(base_name)),
            [Precondition(FALSE)],
        ),
        Pragma("Warnings", ["On", '"precondition is statically false"']),
    ]


def type_dependent_unreachable_function(scalar_type: Scalar) -> List[Declaration]:
    base_name = None
    if isinstance(scalar_type, Enumeration) and scalar_type.always_valid:
        base_name = scalar_type.full_base_name
    return unreachable_function(scalar_type.full_name, base_name)


def generic_name(message: str) -> str:
    return f"Generic_{message}"


def full_generic_name(prefix: str, package: str, message: str) -> str:
    return f"{prefix}{package}.{generic_name(message)}"


def contains_function_name(refinement: Refinement) -> str:
    sdu_name = (
        refinement.sdu.rsplit(".", 1)[1]
        if refinement.sdu.startswith(refinement.package)
        else refinement.sdu
    )
    pdu_name = (
        refinement.pdu.rsplit(".", 1)[1]
        if refinement.pdu.startswith(refinement.package)
        else refinement.pdu
    )
    return f"{sdu_name}_In_{pdu_name}_{refinement.field.name}".replace(".", "_")


def unreachable_function_name(type_name: str) -> str:
    return f"Unreachable_{type_name}".replace(".", "_")


def is_seen_type(type_name: str, seen_types: Set[str]) -> bool:
    seen = type_name in seen_types
    seen_types.add(type_name)
    return seen


def switch_update_conditions(message: Message, field: Field) -> Sequence[Expr]:
    return [
        Not(Call("Has_Buffer", [Name("Ctx")])),
        Call(f"{sequence_name(message, field)}.Has_Buffer", [Name("Seq_Ctx")]),
        Equal(Selected("Ctx", "Buffer_First"), Selected("Seq_Ctx", "Buffer_First")),
        Equal(Selected("Ctx", "Buffer_Last"), Selected("Seq_Ctx", "Buffer_Last")),
        Equal(
            Selected("Seq_Ctx", "First"),
            Call("Field_First", [Name("Ctx"), Name(field.affixed_name)]),
        ),
        Equal(
            Selected("Seq_Ctx", "Last"),
            Call("Field_Last", [Name("Ctx"), Name(field.affixed_name)]),
        ),
    ]


def refinement_conditions(
    refinement: Refinement, pdu_context: str, condition_fields: Mapping[Field, Type], null_sdu: bool
) -> Sequence[Expr]:
    conditions: List[Expr] = [
        Call(f"{refinement.pdu}.Has_Buffer", [Name(pdu_context)]),
    ]

    if null_sdu:
        conditions.extend(
            [
                Call(
                    f"{refinement.pdu}.Structural_Valid",
                    [Name(pdu_context), Name(f"{refinement.pdu}.{refinement.field.affixed_name}")],
                ),
                Not(
                    Call(
                        f"{refinement.pdu}.Present",
                        [
                            Name(pdu_context),
                            Name(f"{refinement.pdu}.{refinement.field.affixed_name}"),
                        ],
                    )
                ),
            ]
        )
    else:
        conditions.append(
            Call(
                f"{refinement.pdu}.Present",
                [Name(pdu_context), Name(f"{refinement.pdu}.{refinement.field.affixed_name}")],
            )
        )

    conditions.extend(
        [
            Call(
                f"{refinement.pdu}.Valid",
                [Name(pdu_context), Name(f"{refinement.pdu}.{f.affixed_name}")],
            )
            for f in condition_fields
        ]
    )

    return conditions
