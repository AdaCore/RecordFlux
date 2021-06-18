from typing import Mapping, Sequence

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
    Precondition,
    ProcedureSpecification,
    Range,
    Selected,
    Size,
    Slice,
    Statement,
    Subprogram,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
    ValueRange,
    Variable,
)
from rflx.common import unique
from rflx.const import BUILTINS_PACKAGE
from rflx.model import FINAL, Enumeration, Field, Message, Opaque, Scalar, Type

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
                            for t in message.types.values()
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
                                *const.CONTEXT_INVARIANT,
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
                        self.__set_message_last(message, f),
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
                                    *self.composite_setter_field_condition_precondition(message, f),
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
                    for f, t in message.types.items()
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
                for f, t in message.types.items()
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
                                Equal(
                                    Call(
                                        "Field_Size",
                                        [Variable("Ctx"), Variable(f.affixed_name)],
                                    ),
                                    Call(
                                        f"{common.sequence_name(message, f)}.Size",
                                        [Variable("Seq_Ctx")],
                                    ),
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
                        *common.field_bit_location_declarations(Variable(f.affixed_name)),
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
                                Equal(
                                    Length("Data"),
                                    Call(
                                        const.TYPES_TO_LENGTH,
                                        [
                                            Call(
                                                "Field_Size",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(f.affixed_name),
                                                ],
                                            )
                                        ],
                                    ),
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
                for f, t in message.types.items()
                if isinstance(t, Opaque)
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        *common.field_bit_location_declarations(Variable(f.affixed_name)),
                        *self.__field_byte_location_declarations(),
                    ],
                    [
                        CallStatement(f"Initialize_{f.name}_Private", [Variable("Ctx")]),
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
                for f, t in message.types.items()
                if isinstance(t, Opaque)
            ],
        )

    def create_generic_opaque_setter_procedures(self, message: Message) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Generic_Set_{field.name}", [InOutParameter(["Ctx"], "Context")]
            )

        def formal_parameters(field: Field) -> Sequence[FormalSubprogramDeclaration]:
            return [
                FormalSubprogramDeclaration(
                    ProcedureSpecification(
                        f"Process_{field.name}",
                        [OutParameter([field.name], const.TYPES_BYTES)],
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
                                            const.TYPES_LENGTH,
                                            [
                                                Div(
                                                    Call(
                                                        "Field_Size",
                                                        [
                                                            Variable("Ctx"),
                                                            Variable(f.affixed_name),
                                                        ],
                                                    ),
                                                    Size(const.TYPES_BYTE),
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
                for f, t in message.types.items()
                if isinstance(t, Opaque)
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        *common.field_bit_location_declarations(Variable(f.affixed_name)),
                        *self.__field_byte_location_declarations(),
                    ],
                    [
                        CallStatement(f"Initialize_{f.name}_Private", [Variable("Ctx")]),
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
                    ],
                )
                for f, t in message.types.items()
                if isinstance(t, Opaque)
            ],
        )

    def create_composite_initialize_procedures(self, message: Message) -> UnitPart:
        def specification_private(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}_Private", [InOutParameter(["Ctx"], "Context")]
            )

        def specification_public(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}", [InOutParameter(["Ctx"], "Context")]
            )

        return UnitPart(
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
                for f, t in message.types.items()
                if isinstance(t, Opaque)
            ],
            [
                s
                for f, t in message.types.items()
                if isinstance(t, Opaque)
                for s in [
                    SubprogramBody(
                        specification_private(f),
                        common.field_bit_location_declarations(Variable(f.affixed_name)),
                        common.initialize_field_statements(message, f),
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
                                    *self.private_setter_postconditions(message, f),
                                )
                            ),
                        ],
                    ),
                    SubprogramBody(
                        specification_public(f),
                        [],
                        [
                            CallStatement(specification_private(f).identifier, [Variable("Ctx")]),
                        ],
                    ),
                ]
            ],
        )

    @staticmethod
    def setter_preconditions(field: Field) -> Sequence[Expr]:
        return [
            Not(Constrained("Ctx")),
            Call("Has_Buffer", [Variable("Ctx")]),
            Call("Valid_Next", [Variable("Ctx"), Variable(field.affixed_name)]),
        ]

    def private_setter_postconditions(self, message: Message, field: Field) -> Sequence[Expr]:
        return [
            Equal(
                Variable("Ctx.Message_Last"),
                Call(
                    "Field_Last",
                    [Variable("Ctx"), Variable(field.affixed_name)],
                ),
            ),
            *self.setter_postconditions(message, field),
        ]

    def public_setter_postconditions(self, message: Message, field: Field) -> Sequence[Expr]:
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

    def setter_postconditions(self, message: Message, field: Field) -> Sequence[Expr]:
        return [
            *[
                Call("Invalid", [Variable("Ctx"), Variable(p.affixed_name)])
                for p in message.successors(field)
                if p != FINAL
            ],
            *common.valid_path_to_next_field_condition(message, field, self.prefix),
            *const.CONTEXT_INVARIANT,
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
                    for t in [message.types[p]]
                    if isinstance(t, Scalar) and int(t.value_count) > 1
                ]
            ],
        ]

    @staticmethod
    def composite_setter_postconditions(field: Field) -> Sequence[Expr]:
        return [
            Call("Has_Buffer", [Variable("Ctx")]),
            Call("Structural_Valid", [Variable("Ctx"), Variable(field.affixed_name)]),
        ]

    @staticmethod
    def composite_setter_field_condition_precondition(
        message: Message, field: Field
    ) -> Sequence[Expr]:
        return [
            common.field_condition_call(message, field, Variable("Data")),
        ]

    @staticmethod
    def composite_setter_preconditions(field: Field) -> Sequence[Expr]:
        return [
            common.sufficient_space_for_field_condition(Variable(field.affixed_name)),
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
    def __set_message_last(message: Message, field: Field) -> Statement:
        return (
            Assignment(Variable("Ctx.Message_Last"), Variable("Last"))
            if field in message.direct_predecessors(FINAL)
            else Assignment(
                Variable("Ctx.Message_Last"),
                Mul(
                    Div(
                        Add(
                            Variable("Last"),
                            Number(7),
                        ),
                        Number(8),
                    ),
                    Number(8),
                ),
            )
        )

    def __set_sequence_field(self, message: Message, field: Field) -> Sequence[Statement]:
        return [
            CallStatement(
                "Reset_Dependent_Fields",
                [Variable("Ctx"), Variable(field.affixed_name)],
            ),
            self.__set_message_last(message, field),
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
    def __field_byte_location_declarations() -> Sequence[Declaration]:
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
