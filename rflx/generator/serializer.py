# pylint: disable = too-many-lines

import itertools
from typing import List, Mapping, Tuple

from rflx import expression as expr
from rflx.ada import (
    ID,
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
    Assignment,
    Call,
    CallStatement,
    Case,
    CaseStatement,
    Constrained,
    Declaration,
    Div,
    Equal,
    Expr,
    ExpressionFunctionDeclaration,
    ForAllIn,
    FormalSubprogramDeclaration,
    FunctionSpecification,
    GenericProcedureInstantiation,
    Greater,
    GreaterEqual,
    If,
    In,
    Indexed,
    InOutParameter,
    Length,
    LessEqual,
    Mod,
    Mul,
    NamedAggregate,
    Not,
    NullStatement,
    Number,
    ObjectDeclaration,
    Old,
    OutParameter,
    Parameter,
    Postcondition,
    PragmaStatement,
    Precondition,
    ProcedureSpecification,
    Range,
    Selected,
    Size,
    Slice,
    Statement,
    String,
    Subprogram,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
    Update,
    ValueRange,
    Variable,
)
from rflx.common import unique
from rflx.const import BUILTINS_PACKAGE
from rflx.model import FINAL, Enumeration, Field, Message, Opaque, Scalar, Sequence, Type

from . import common, const


class SerializerGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix

    def insert_function(self, type_identifier: ID) -> Subprogram:
        return GenericProcedureInstantiation(
            "Insert",
            ProcedureSpecification(
                const.TYPES * "Insert",
                [
                    Parameter(["Val"], type_identifier),
                    InOutParameter(["Buffer"], const.TYPES_BYTES),
                    Parameter(["Offset"], const.TYPES_OFFSET),
                ],
            ),
            [common.prefixed_type_identifier(type_identifier, self.prefix)],
        )

    def create_internal_functions(
        self, message: Message, scalar_fields: Mapping[Field, Scalar]
    ) -> UnitPart:
        return UnitPart(
            [],
            [
                SubprogramBody(
                    ProcedureSpecification(
                        "Set_Field_Value",
                        [
                            InOutParameter(["Ctx"], "Context"),
                            Parameter(["Val"], "Field_Dependent_Value"),
                            OutParameter(["Fst", "Lst"], const.TYPES_BIT_INDEX),
                        ],
                    ),
                    [
                        *common.field_bit_location_declarations(Variable("Val.Fld")),
                        *common.field_byte_location_declarations(),
                        *unique(
                            self.insert_function(common.full_base_type_name(t))
                            for t in message.field_types.values()
                            if isinstance(t, Scalar)
                        ),
                    ],
                    [
                        Assignment("Fst", Variable("First")),
                        Assignment("Lst", Variable("Last")),
                        CaseStatement(
                            Variable("Val.Fld"),
                            [
                                (
                                    Variable(f.affixed_name),
                                    [
                                        CallStatement(
                                            "Insert",
                                            [
                                                Variable(f"Val.{f.name}_Value"),
                                                Slice(
                                                    Variable("Ctx.Buffer.all"),
                                                    Variable("Buffer_First"),
                                                    Variable("Buffer_Last"),
                                                ),
                                                Variable("Offset"),
                                            ],
                                        )
                                        if f in scalar_fields
                                        else NullStatement()
                                    ],
                                )
                                for f in message.all_fields
                            ],
                        ),
                    ],
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained("Ctx")),
                                Call("Has_Buffer", [Variable("Ctx")]),
                                In(Variable("Val.Fld"), Range("Field")),
                                Call("Valid_Next", [Variable("Ctx"), Variable("Val.Fld")]),
                                common.sufficient_space_for_field_condition(Variable("Val.Fld")),
                                ForAllIn(
                                    "F",
                                    Range("Field"),
                                    If(
                                        [
                                            (
                                                Call(
                                                    "Structural_Valid",
                                                    [
                                                        Indexed(
                                                            Variable("Ctx.Cursors"),
                                                            Variable("F"),
                                                        )
                                                    ],
                                                ),
                                                LessEqual(
                                                    Selected(
                                                        Indexed(
                                                            Variable("Ctx.Cursors"),
                                                            Variable("F"),
                                                        ),
                                                        "Last",
                                                    ),
                                                    Call(
                                                        "Field_Last",
                                                        [Variable("Ctx"), Variable("Val.Fld")],
                                                    ),
                                                ),
                                            )
                                        ]
                                    ),
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Equal(
                                    Variable("Fst"),
                                    Call("Field_First", [Variable("Ctx"), Variable("Val.Fld")]),
                                ),
                                Equal(
                                    Variable("Lst"),
                                    Call("Field_Last", [Variable("Ctx"), Variable("Val.Fld")]),
                                ),
                                GreaterEqual(Variable("Fst"), Variable("Ctx.First")),
                                LessEqual(Variable("Fst"), Add(Variable("Lst"), Number(1))),
                                LessEqual(Variable("Lst"), Variable("Ctx.Last")),
                                ForAllIn(
                                    "F",
                                    Range("Field"),
                                    If(
                                        [
                                            (
                                                Call(
                                                    "Structural_Valid",
                                                    [
                                                        Indexed(
                                                            Variable("Ctx.Cursors"),
                                                            Variable("F"),
                                                        )
                                                    ],
                                                ),
                                                LessEqual(
                                                    Selected(
                                                        Indexed(
                                                            Variable("Ctx.Cursors"),
                                                            Variable("F"),
                                                        ),
                                                        "Last",
                                                    ),
                                                    Variable("Lst"),
                                                ),
                                            )
                                        ]
                                    ),
                                ),
                                *common.context_invariant(message),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Variable("Ctx.Cursors"),
                                    ]
                                ],
                            )
                        ),
                    ],
                )
            ]
            if scalar_fields
            else [],
        )

    def create_valid_length_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Length",
            "Boolean",
            [
                Parameter(["Ctx"], "Context"),
                Parameter(["Fld"], "Field"),
                Parameter(["Length"], const.TYPES_LENGTH),
            ],
        )

        def size(field: Field, message: Message) -> Expr:
            def substituted(expression: expr.Expr) -> Expr:
                return (
                    expression.substituted(
                        common.substitution(
                            message, self.prefix, target_type=const.TYPES_BIT_LENGTH
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
            explicit_size = Equal(
                Variable("Length"),
                Call(
                    const.TYPES_TO_LENGTH,
                    [Call("Field_Size", [Variable("Ctx"), Variable("Fld")])],
                ),
            )
            implicit_size = LessEqual(
                Variable("Length"),
                Call(
                    const.TYPES_TO_LENGTH,
                    [Call("Available_Space", [Variable("Ctx"), Variable("Fld")])],
                ),
            )

            cases: List[Tuple[Expr, Expr]] = []
            for target, links in target_links:
                field_type = message.field_types[target]
                size: Expr
                if isinstance(field_type, Scalar):
                    size = explicit_size
                else:
                    if len(links) == 1:
                        size = implicit_size if links[0].has_implicit_size else explicit_size
                    else:
                        size = If(
                            [
                                (
                                    substituted(l.condition),
                                    implicit_size if l.has_implicit_size else explicit_size,
                                )
                                for l in links
                            ],
                            const.UNREACHABLE,
                        )
                cases.append(
                    (
                        Variable(target.affixed_name),
                        size,
                    )
                )

            if not cases:
                return const.UNREACHABLE

            if set(message.fields) - {l.target for l in message.outgoing(field)}:
                cases.append((Variable("others"), const.UNREACHABLE))
            return Case(Variable("Fld"), cases)

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

    def create_scalar_setter_procedures(
        self, message: Message, scalar_fields: Mapping[Field, Scalar]
    ) -> UnitPart:
        def specification(field: Field, field_type: Type) -> ProcedureSpecification:
            if field_type.package == BUILTINS_PACKAGE:
                type_identifier = ID(field_type.name)
            elif isinstance(field_type, Enumeration) and field_type.always_valid:
                type_identifier = common.prefixed_type_identifier(
                    common.full_enum_name(field_type), self.prefix
                )
            else:
                type_identifier = common.prefixed_type_identifier(
                    ID(field_type.identifier), self.prefix
                )

            return ProcedureSpecification(
                f"Set_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Val"], type_identifier)],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f, t),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                Call(
                                    "Field_Condition",
                                    [
                                        Variable("Ctx"),
                                        Aggregate(
                                            Variable(f.affixed_name),
                                            Call("To_Base", [Variable("Val")]),
                                        ),
                                    ],
                                ),
                                Call("Valid", [Call("To_Base", [Variable("Val")])])
                                if not isinstance(t, Enumeration)
                                else TRUE,
                                common.sufficient_space_for_field_condition(
                                    Variable(f.affixed_name)
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call(
                                    "Valid",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                *(
                                    [
                                        Equal(
                                            Call(f"Get_{f.name}", [Variable("Ctx")]),
                                            Aggregate(TRUE, Variable("Val"))
                                            if isinstance(t, Enumeration) and t.always_valid
                                            else Variable("Val"),
                                        )
                                    ]
                                    if int(t.value_count) > 1
                                    else []
                                ),
                                *self.public_setter_postconditions(message, f),
                                *[
                                    Equal(
                                        Call(
                                            "Context_Cursor",
                                            [
                                                Variable("Ctx"),
                                                Variable(p.affixed_name),
                                            ],
                                        ),
                                        Old(
                                            Call(
                                                "Context_Cursor",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(p.affixed_name),
                                                ],
                                            )
                                        ),
                                    )
                                    for p in message.predecessors(f)
                                ],
                            )
                        ),
                    ],
                )
                for f, t in scalar_fields.items()
            ],
            [
                SubprogramBody(
                    specification(f, t),
                    [
                        ObjectDeclaration(
                            ["Field_Value"],
                            "Field_Dependent_Value",
                            Aggregate(
                                Variable(f.affixed_name),
                                Call("To_Base", [Variable("Val")]),
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(["First", "Last"], const.TYPES_BIT_INDEX),
                    ],
                    [
                        CallStatement(
                            "Reset_Dependent_Fields",
                            [Variable("Ctx"), Variable(f.affixed_name)],
                        ),
                        CallStatement(
                            "Set_Field_Value",
                            [
                                Variable("Ctx"),
                                Variable("Field_Value"),
                                Variable("First"),
                                Variable("Last"),
                            ],
                        ),
                        *self._update_last(message, f),
                        Assignment(
                            Indexed(Variable("Ctx.Cursors"), Variable(f.affixed_name)),
                            NamedAggregate(
                                ("State", Variable("S_Valid")),
                                ("First", Variable("First")),
                                ("Last", Variable("Last")),
                                ("Value", Variable("Field_Value")),
                                (
                                    "Predecessor",
                                    Selected(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
                                        ),
                                        "Predecessor",
                                    ),
                                ),
                            ),
                        ),
                        Assignment(
                            Indexed(
                                Variable("Ctx.Cursors"),
                                Call(
                                    "Successor",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                            ),
                            NamedAggregate(
                                ("State", Variable("S_Invalid")),
                                ("Predecessor", Variable(f.affixed_name)),
                            ),
                        ),
                    ],
                )
                for f, t in scalar_fields.items()
            ],
        )

    def create_composite_setter_empty_procedures(self, message: Message) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Set_{field.name}_Empty", [InOutParameter(["Ctx"], "Context")]
            )

        return UnitPart(
            [
                *[
                    SubprogramDeclaration(
                        specification(f),
                        [
                            Precondition(
                                AndThen(
                                    *self.setter_preconditions(f),
                                    *self.composite_setter_field_condition_precondition(
                                        message, f, empty=True
                                    ),
                                    *self.composite_setter_preconditions(f),
                                    Equal(
                                        Call(
                                            "Field_Size",
                                            [
                                                Variable("Ctx"),
                                                Variable(f.affixed_name),
                                            ],
                                        ),
                                        Number(0),
                                    ),
                                )
                            ),
                            Postcondition(
                                And(
                                    *self.composite_setter_postconditions(f),
                                    *self.public_setter_postconditions(message, f),
                                )
                            ),
                        ],
                    )
                    for f, t in message.field_types.items()
                    if message.is_possibly_empty(f)
                ],
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_BIT_INDEX,
                            Call(
                                "Field_First",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            const.TYPES_BIT_INDEX,
                            Call(
                                "Field_Last",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            ),
                            constant=True,
                        ),
                    ],
                    self.__set_sequence_field(message, f),
                )
                for f, t in message.field_types.items()
                if message.is_possibly_empty(f)
            ],
        )

    def create_sequence_setter_procedures(
        self, message: Message, sequence_fields: Mapping[Field, Type]
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Set_{field.name}",
                [
                    InOutParameter(["Ctx"], "Context"),
                    Parameter(["Seq_Ctx"], f"{common.sequence_name(message, field)}.Context"),
                ],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                *self.composite_setter_field_condition_precondition(message, f),
                                *self.composite_setter_preconditions(f),
                                Call(
                                    "Valid_Length",
                                    [
                                        Variable("Ctx"),
                                        Variable(f.affixed_name),
                                        Call(
                                            f"{common.sequence_name(message, f)}.Byte_Size",
                                            [Variable("Seq_Ctx")],
                                        ),
                                    ],
                                ),
                                Call(
                                    f"{common.sequence_name(message, f)}.Has_Buffer",
                                    [Variable("Seq_Ctx")],
                                ),
                                Call(
                                    f"{common.sequence_name(message, f)}.Valid",
                                    [Variable("Seq_Ctx")],
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(f),
                                *self.public_setter_postconditions(message, f),
                                If(
                                    [
                                        (
                                            Greater(
                                                Call(
                                                    "Field_Size",
                                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                                ),
                                                Number(0),
                                            ),
                                            Call(
                                                "Present",
                                                [Variable("Ctx"), Variable(f.affixed_name)],
                                            ),
                                        )
                                    ]
                                ),
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
                            ["First"],
                            const.TYPES_BIT_INDEX,
                            Call("Field_First", [Variable("Ctx"), Variable(f.affixed_name)]),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            const.TYPES_BIT_INDEX,
                            Add(
                                Call(
                                    "Field_First",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                Call(
                                    const.TYPES_TO_BIT_LENGTH,
                                    [
                                        Call(
                                            f"{common.sequence_name(message, f)}.Byte_Size",
                                            [Variable("Seq_Ctx")],
                                        )
                                    ],
                                ),
                                -Number(1),
                            ),
                            constant=True,
                        ),
                        *self.__field_byte_location_declarations(),
                    ],
                    [
                        *self.__set_sequence_field(message, f),
                        CallStatement(
                            f"{common.sequence_name(message, f)}.Copy",
                            [
                                Variable("Seq_Ctx"),
                                Indexed(
                                    Variable("Ctx.Buffer.all"),
                                    ValueRange(Variable("Buffer_First"), Variable("Buffer_Last")),
                                ),
                            ],
                        ),
                    ],
                )
                for f in sequence_fields
            ],
        )

    def create_opaque_setter_procedures(self, message: Message) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Set_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Data"], const.TYPES_BYTES)],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                *self.composite_setter_preconditions(f),
                                Call(
                                    "Valid_Length",
                                    [
                                        Variable("Ctx"),
                                        Variable(f.affixed_name),
                                        Length("Data"),
                                    ],
                                ),
                                *self.composite_setter_field_condition_precondition(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(f),
                                *self.public_setter_postconditions(message, f),
                            )
                        ),
                    ],
                )
                for f, t in message.field_types.items()
                if isinstance(t, Opaque)
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_BIT_INDEX,
                            Call("Field_First", [Variable("Ctx"), Variable(f.affixed_name)]),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Buffer_First"],
                            const.TYPES_INDEX,
                            Call(const.TYPES_TO_INDEX, [Variable("First")]),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Buffer_Last"],
                            const.TYPES_INDEX,
                            Add(Variable("Buffer_First"), Length("Data"), -Number(1)),
                            constant=True,
                        ),
                    ],
                    [
                        CallStatement(
                            f"Initialize_{f.name}_Private", [Variable("Ctx"), Length("Data")]
                        ),
                        Assignment(
                            Slice(
                                Selected(Variable("Ctx.Buffer"), "all"),
                                Variable("Buffer_First"),
                                Variable("Buffer_Last"),
                            ),
                            Variable("Data"),
                        ),
                    ],
                )
                for f, t in message.field_types.items()
                if isinstance(t, Opaque)
            ],
        )

    def create_generic_opaque_setter_procedures(
        self,
        message: Message,
        fields_with_implicit_size: List[Field],
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Generic_Set_{field.name}", [InOutParameter(["Ctx"], "Context")]
            )

        def formal_parameters(field: Field) -> List[FormalSubprogramDeclaration]:
            return [
                FormalSubprogramDeclaration(
                    ProcedureSpecification(
                        f"Process_{field.name}",
                        [
                            OutParameter([field.name], const.TYPES_BYTES),
                        ],
                    )
                ),
                FormalSubprogramDeclaration(
                    FunctionSpecification(
                        "Valid_Length",
                        "Boolean",
                        [Parameter(["Length"], const.TYPES_LENGTH)],
                    )
                ),
            ]

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                *self.composite_setter_preconditions(f),
                                Call(
                                    "Valid_Length",
                                    [
                                        Call(
                                            const.TYPES_TO_LENGTH,
                                            [
                                                Call(
                                                    "Field_Size",
                                                    [
                                                        Variable("Ctx"),
                                                        Variable(f.affixed_name),
                                                    ],
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(f),
                                *self.public_setter_postconditions(message, f),
                            )
                        ),
                    ],
                    formal_parameters(f),
                )
                for f, t in message.field_types.items()
                if isinstance(t, Opaque) and f not in fields_with_implicit_size
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        *common.field_bit_location_declarations(Variable(f.affixed_name)),
                        *self.__field_byte_location_declarations(),
                    ],
                    [
                        CallStatement(
                            f"Process_{f.name}",
                            [
                                Slice(
                                    Selected(Variable("Ctx.Buffer"), "all"),
                                    Variable("Buffer_First"),
                                    Variable("Buffer_Last"),
                                ),
                            ],
                        ),
                        CallStatement(
                            f"Initialize_{f.name}_Private",
                            [
                                Variable("Ctx"),
                                Call(
                                    const.TYPES_LENGTH,
                                    [
                                        Add(
                                            Variable("Buffer_Last"),
                                            -Variable("Buffer_First"),
                                            Number(1),
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                )
                for f, t in message.field_types.items()
                if isinstance(t, Opaque) and f not in fields_with_implicit_size
            ],
        )

    def create_composite_initialize_procedures(
        self,
        message: Message,
        fields_with_explicit_size: List[Field],
        fields_with_implicit_size: List[Field],
    ) -> UnitPart:
        def specification_private(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}_Private",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Length"], const.TYPES_LENGTH)],
            )

        def specification_public(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}", [InOutParameter(["Ctx"], "Context")]
            )

        def specification_public_with_length(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Length"], const.TYPES_LENGTH)],
            )

        return UnitPart(
            [
                s
                for f, t in message.field_types.items()
                if isinstance(t, (Opaque, Sequence))
                for s in [
                    *(
                        [
                            SubprogramDeclaration(
                                specification_public(f),
                                [
                                    Precondition(
                                        AndThen(
                                            *self.setter_preconditions(f),
                                            *self.composite_setter_preconditions(f),
                                        )
                                    ),
                                    Postcondition(
                                        And(
                                            *self.composite_setter_postconditions(f),
                                            *self.public_setter_postconditions(message, f),
                                        )
                                    ),
                                ],
                            )
                        ]
                        if f in fields_with_explicit_size
                        else []
                    ),
                    *(
                        [
                            SubprogramDeclaration(
                                specification_public_with_length(f),
                                [
                                    Precondition(
                                        AndThen(
                                            *self.setter_preconditions(f),
                                            Call(
                                                "Valid_Length",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(f.affixed_name),
                                                    Variable("Length"),
                                                ],
                                            ),
                                            *self.composite_setter_preconditions(
                                                f,
                                                Call(
                                                    const.TYPES_TO_BIT_LENGTH, [Variable("Length")]
                                                ),
                                            ),
                                        )
                                    ),
                                    Postcondition(
                                        And(
                                            *self.composite_setter_postconditions(f),
                                            Equal(
                                                Call(
                                                    "Field_Size",
                                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                                ),
                                                Call(
                                                    const.TYPES_TO_BIT_LENGTH, [Variable("Length")]
                                                ),
                                            ),
                                            *self.public_setter_postconditions(message, f),
                                        )
                                    ),
                                ],
                            )
                        ]
                        if f in fields_with_implicit_size
                        else []
                    ),
                ]
            ],
            [
                s
                for f, t in message.field_types.items()
                if isinstance(t, (Opaque, Sequence))
                for s in [
                    SubprogramBody(
                        specification_private(f),
                        [
                            ObjectDeclaration(
                                ["First"],
                                const.TYPES_BIT_INDEX,
                                Call("Field_First", [Variable("Ctx"), Variable(f.affixed_name)]),
                                constant=True,
                            ),
                            ObjectDeclaration(
                                ["Last"],
                                const.TYPES_BIT_INDEX,
                                Add(
                                    Call(
                                        "Field_First", [Variable("Ctx"), Variable(f.affixed_name)]
                                    ),
                                    Mul(
                                        Call(const.TYPES_BIT_LENGTH, [Variable("Length")]),
                                        Size(const.TYPES_BYTE),
                                    ),
                                    -Number(1),
                                ),
                                constant=True,
                            ),
                        ],
                        common.initialize_field_statements(message, f, reset_written_last=True),
                        [
                            Precondition(
                                AndThen(
                                    *self.setter_preconditions(f),
                                    Call(
                                        "Valid_Length",
                                        [
                                            Variable("Ctx"),
                                            Variable(f.affixed_name),
                                            Variable("Length"),
                                        ],
                                    ),
                                    GreaterEqual(
                                        Call(
                                            const.TYPES_TO_LENGTH,
                                            [
                                                Call(
                                                    "Available_Space",
                                                    [
                                                        Variable("Ctx"),
                                                        Variable(f.affixed_name),
                                                    ],
                                                ),
                                            ],
                                        ),
                                        Variable("Length"),
                                    ),
                                    Equal(
                                        Mod(
                                            Call(
                                                "Field_First",
                                                [Variable("Ctx"), Variable(f.affixed_name)],
                                            ),
                                            Size(const.TYPES_BYTE),
                                        ),
                                        Number(1),
                                    ),
                                )
                            ),
                            Postcondition(
                                And(
                                    *self.composite_setter_postconditions(f),
                                    Equal(
                                        Call(
                                            "Field_Size",
                                            [Variable("Ctx"), Variable(f.affixed_name)],
                                        ),
                                        Call(const.TYPES_TO_BIT_LENGTH, [Variable("Length")]),
                                    ),
                                    *self.private_setter_postconditions(message, f),
                                )
                            ),
                        ],
                    ),
                    *(
                        [
                            SubprogramBody(
                                specification_public(f),
                                [],
                                [
                                    CallStatement(
                                        specification_private(f).identifier,
                                        [
                                            Variable("Ctx"),
                                            Call(
                                                const.TYPES_TO_LENGTH,
                                                [
                                                    Call(
                                                        "Field_Size",
                                                        [Variable("Ctx"), Variable(f.affixed_name)],
                                                    )
                                                ],
                                            ),
                                        ],
                                    ),
                                ],
                            )
                        ]
                        if f in fields_with_explicit_size
                        else []
                    ),
                    *(
                        [
                            SubprogramBody(
                                specification_public_with_length(f),
                                [],
                                [
                                    CallStatement(
                                        specification_private(f).identifier,
                                        [Variable("Ctx"), Variable("Length")],
                                    ),
                                ],
                            )
                        ]
                        if f in fields_with_implicit_size
                        else []
                    ),
                ]
            ],
        )

    @staticmethod
    def setter_preconditions(field: Field) -> List[Expr]:
        return [
            Not(Constrained("Ctx")),
            Call("Has_Buffer", [Variable("Ctx")]),
            Call("Valid_Next", [Variable("Ctx"), Variable(field.affixed_name)]),
        ]

    def private_setter_postconditions(self, message: Message, field: Field) -> List[Expr]:
        return [
            Equal(
                Variable("Ctx.Verified_Last"),
                Call(
                    "Field_Last",
                    [Variable("Ctx"), Variable(field.affixed_name)],
                ),
            ),
            *self.setter_postconditions(message, field),
        ]

    def public_setter_postconditions(self, message: Message, field: Field) -> List[Expr]:
        return [
            *(
                [
                    If(
                        [
                            (
                                Call("Structural_Valid_Message", [Variable("Ctx")]),
                                Equal(
                                    Call("Message_Last", [Variable("Ctx")]),
                                    Call(
                                        "Field_Last",
                                        [Variable("Ctx"), Variable(field.affixed_name)],
                                    ),
                                ),
                            )
                        ]
                    )
                ]
                if field in message.direct_predecessors(FINAL)
                else []
            ),
            *self.setter_postconditions(message, field),
        ]

    def setter_postconditions(self, message: Message, field: Field) -> List[Expr]:
        return [
            *[
                Call("Invalid", [Variable("Ctx"), Variable(p.affixed_name)])
                for p in message.successors(field)
                if p != FINAL
            ],
            *common.valid_path_to_next_field_condition(message, field, self.prefix),
            *common.context_invariant(message),
            *[
                Equal(e, Old(e))
                for e in [
                    Call(
                        "Predecessor",
                        [Variable("Ctx"), Variable(field.affixed_name)],
                    ),
                    Call(
                        "Valid_Next",
                        [Variable("Ctx"), Variable(field.affixed_name)],
                    ),
                ]
                + [
                    Call(f"Get_{p.name}", [Variable("Ctx")])
                    for p in message.definite_predecessors(field)
                    for t in [message.field_types[p]]
                    if isinstance(t, Scalar) and int(t.value_count) > 1
                ]
            ],
        ]

    @staticmethod
    def composite_setter_postconditions(field: Field) -> List[Expr]:
        return [
            Call("Has_Buffer", [Variable("Ctx")]),
            Call("Structural_Valid", [Variable("Ctx"), Variable(field.affixed_name)]),
        ]

    @staticmethod
    def composite_setter_field_condition_precondition(
        message: Message, field: Field, empty: bool = False
    ) -> List[Expr]:
        return [
            common.field_condition_call(
                message,
                field,
                Variable("Data"),
                None if empty else Call(const.TYPES_TO_BIT_LENGTH, [Length("Data")]),
            ),
        ]

    @staticmethod
    def composite_setter_preconditions(field: Field, size: Expr = None) -> List[Expr]:
        return [
            common.sufficient_space_for_field_condition(Variable(field.affixed_name), size),
            Equal(
                Mod(
                    Call(
                        "Field_First",
                        [Variable("Ctx"), Variable(field.affixed_name)],
                    ),
                    Size(const.TYPES_BYTE),
                ),
                Number(1),
            ),
            Equal(
                Mod(
                    Call(
                        "Field_Last",
                        [Variable("Ctx"), Variable(field.affixed_name)],
                    ),
                    Size(const.TYPES_BYTE),
                ),
                Number(0),
            ),
            Equal(
                Mod(
                    Call(
                        "Field_Size",
                        [Variable("Ctx"), Variable(field.affixed_name)],
                    ),
                    Size(const.TYPES_BYTE),
                ),
                Number(0),
            ),
        ]

    @staticmethod
    def _update_last(message: Message, field: Field) -> List[Statement]:
        last = (
            Variable("Last")
            if field in message.direct_predecessors(FINAL)
            else Mul(
                Div(
                    Add(
                        Variable("Last"),
                        Number(7),
                    ),
                    Number(8),
                ),
                Number(8),
            )
        )
        return [
            # ISSUE: Componolit/RecordFlux#868
            PragmaStatement(
                "Warnings",
                [
                    Variable("Off"),
                    String("attribute Update is an obsolescent feature"),
                ],
            ),
            Assignment(
                "Ctx",
                Update(
                    "Ctx",
                    ("Verified_Last", last),
                    ("Written_Last", last),
                ),
            ),
            PragmaStatement(
                "Warnings",
                [
                    Variable("On"),
                    String("attribute Update is an obsolescent feature"),
                ],
            ),
        ]

    def __set_sequence_field(self, message: Message, field: Field) -> List[Statement]:
        return [
            CallStatement(
                "Reset_Dependent_Fields",
                [Variable("Ctx"), Variable(field.affixed_name)],
            ),
            *self._update_last(message, field),
            Assignment(
                Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)),
                NamedAggregate(
                    ("State", Variable("S_Valid")),
                    ("First", Variable("First")),
                    ("Last", Variable("Last")),
                    (
                        "Value",
                        NamedAggregate(("Fld", Variable(field.affixed_name))),
                    ),
                    (
                        "Predecessor",
                        Selected(
                            Indexed(
                                Variable("Ctx.Cursors"),
                                Variable(field.affixed_name),
                            ),
                            "Predecessor",
                        ),
                    ),
                ),
            ),
            Assignment(
                Indexed(
                    Variable("Ctx.Cursors"),
                    Call(
                        "Successor",
                        [Variable("Ctx"), Variable(field.affixed_name)],
                    ),
                ),
                NamedAggregate(
                    ("State", Variable("S_Invalid")),
                    ("Predecessor", Variable(field.affixed_name)),
                ),
            ),
        ]

    @staticmethod
    def __field_byte_location_declarations() -> List[Declaration]:
        return [
            ExpressionFunctionDeclaration(
                FunctionSpecification("Buffer_First", const.TYPES_INDEX),
                Call(const.TYPES_TO_INDEX, [Variable("First")]),
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification("Buffer_Last", const.TYPES_INDEX),
                Call(const.TYPES_TO_INDEX, [Variable("Last")]),
            ),
        ]
