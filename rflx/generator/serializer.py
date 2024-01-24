from __future__ import annotations

from collections.abc import Mapping
from enum import Enum
from typing import Optional

from rflx import expression as expr
from rflx.ada import (
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
    Assignment,
    Call,
    CallStatement,
    ChoiceList,
    Constrained,
    Div,
    Equal,
    Expr,
    ExpressionFunctionDeclaration,
    FormalSubprogramDeclaration,
    FunctionSpecification,
    Greater,
    GreaterEqual,
    If,
    IfStatement,
    In,
    Indexed,
    InlineAlways,
    InOutParameter,
    Length,
    LessEqual,
    Mod,
    Mul,
    NamedAggregate,
    Not,
    Number,
    ObjectDeclaration,
    Old,
    OutParameter,
    Parameter,
    Postcondition,
    Pragma,
    PragmaStatement,
    Precondition,
    ProcedureSpecification,
    Selected,
    Size,
    Slice,
    Statement,
    String,
    Sub,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
    Update,
    ValueRange,
    Variable,
)
from rflx.const import BUILTINS_PACKAGE
from rflx.identifier import ID, StrID
from rflx.model import (
    FINAL,
    ByteOrder,
    Enumeration,
    Field,
    Message,
    Opaque,
    Scalar,
    Sequence,
    Type,
    is_builtin_type,
)

from . import common, const


class SerializerGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix

    def create_valid_size_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Size",
            "Boolean",
            [
                Parameter(["Ctx"], "Context"),
                Parameter(["Fld"], "Field"),
                Parameter(["Size"], const.TYPES_BIT_LENGTH),
            ],
        )

        implicit_size_condition = LessEqual(
            Variable("Size"),
            Call(
                "Available_Space",
                [Variable("Ctx"), Variable("Fld")],
            ),
        )
        explicit_size_condition = Equal(
            Variable("Size"),
            Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
        )

        return UnitPart(
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    If(
                        [
                            (
                                AndThen(
                                    Equal(Variable("Fld"), Variable(l.target.affixed_name)),
                                    *(
                                        [
                                            l.condition.substituted(
                                                common.substitution(
                                                    message,
                                                    self.prefix,
                                                    target_type=const.TYPES_BIT_LENGTH,
                                                ),
                                            )
                                            .simplified()
                                            .ada_expr(),
                                        ]
                                        if l.condition != expr.TRUE
                                        else []
                                    ),
                                ),
                                implicit_size_condition,
                            )
                            for l in message.structure
                            if l.has_implicit_size
                        ],
                        explicit_size_condition,
                    )
                    if len(message.fields) > 1
                    or all(not l.has_implicit_size for l in message.structure)
                    else implicit_size_condition,
                    [
                        Precondition(
                            And(
                                Call(
                                    self.prefix * message.identifier * "Valid_Next",
                                    [Variable("Ctx"), Variable("Fld")],
                                ),
                            ),
                        ),
                    ],
                ),
            ],
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

        return UnitPart(
            [
                # Eng/RecordFlux/Workarounds#47
                Pragma(
                    "Warnings",
                    [Variable("Off"), String("postcondition does not mention function result")],
                ),
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            And(
                                Call(
                                    self.prefix * message.identifier * "Valid_Next",
                                    [Variable("Ctx"), Variable("Fld")],
                                ),
                            ),
                        ),
                        Postcondition(TRUE),
                    ],
                ),
                Pragma(
                    "Warnings",
                    [Variable("On"), String("postcondition does not mention function result")],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Call(
                        "Valid_Size",
                        [
                            Variable("Ctx"),
                            Variable("Fld"),
                            Call(
                                const.TYPES_TO_BIT_LENGTH,
                                [Variable("Length")],
                            ),
                        ],
                    ),
                ),
            ],
        )

    @staticmethod
    def requires_set_procedure(message: Message) -> bool:
        return any(
            message.is_possibly_empty(f) or isinstance(t, (Scalar, Sequence))
            for f, t in message.field_types.items()
        )

    def create_set_procedure(
        self,
        message: Message,
        composite_fields: list[Field],
    ) -> UnitPart:
        if not self.requires_set_procedure(message):
            return UnitPart()

        def specification() -> ProcedureSpecification:
            return ProcedureSpecification(
                "Set",
                [
                    InOutParameter(["Ctx"], "Context"),
                    Parameter(["Fld"], "Field"),
                    Parameter(["Val"], const.TYPES_BASE_INT),
                    Parameter(["Size"], const.TYPES_BIT_LENGTH),
                    Parameter(["State_Valid"], "Boolean"),
                    OutParameter(["Buffer_First"], const.TYPES_INDEX),
                    OutParameter(["Buffer_Last"], const.TYPES_INDEX),
                    OutParameter(["Offset"], const.TYPES_OFFSET),
                ],
            )

        class CursorState(Enum):
            VALID = 1
            WELL_FORMED = 2

        def set_context_cursor(field_type: CursorState) -> Assignment:
            return Assignment(
                Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                NamedAggregate(
                    (
                        "State",
                        Variable("S_Valid")
                        if field_type == CursorState.VALID
                        else Variable("S_Well_Formed"),
                    ),
                    ("First", Variable("First")),
                    ("Last", Variable("Last")),
                    ("Value", Variable("Val")),
                ),
            )

        return UnitPart(
            body=[
                SubprogramBody(
                    specification(),
                    [
                        ObjectDeclaration(["First"], const.TYPES_BIT_INDEX),
                        ObjectDeclaration(["Last"], const.TYPES_BIT_LENGTH),
                    ],
                    [
                        CallStatement(
                            "Reset_Dependent_Fields",
                            [Variable("Ctx"), Variable("Fld")],
                        ),
                        Assignment(
                            "First",
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        ),
                        Assignment(
                            "Last",
                            Add(
                                Call(
                                    "Field_First",
                                    [Variable("Ctx"), Variable("Fld")],
                                ),
                                Variable("Size"),
                                -Number(1),
                            ),
                        ),
                        Assignment(
                            "Offset",
                            Call(
                                const.TYPES_OFFSET,
                                [
                                    Mod(
                                        Sub(
                                            Size(const.TYPES_BYTE),
                                            Mod(Variable("Last"), Size(const.TYPES_BYTE)),
                                        ),
                                        Size(const.TYPES_BYTE),
                                    ),
                                ],
                            ),
                        ),
                        Assignment(
                            "Buffer_First",
                            Call(const.TYPES_TO_INDEX, [Variable("First")]),
                        ),
                        Assignment(
                            "Buffer_Last",
                            Call(const.TYPES_TO_INDEX, [Variable("Last")]),
                        ),
                        *self._update_last(),
                        IfStatement(
                            [
                                (
                                    Variable("State_Valid"),
                                    [set_context_cursor(CursorState.VALID)],
                                ),
                            ],
                            [set_context_cursor(CursorState.WELL_FORMED)],
                        ),
                        PragmaStatement(
                            "Assert",
                            [
                                Equal(
                                    Variable("Last"),
                                    Sub(
                                        Add(
                                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                                            Variable("Size"),
                                        ),
                                        Number(1),
                                    ),
                                ),
                            ],
                        ),
                    ],
                    [
                        Precondition(
                            AndThen(
                                Call(
                                    self.prefix * message.identifier * "Has_Buffer",
                                    [Variable("Ctx")],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Next",
                                    [Variable("Ctx"), Variable("Fld")],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Value",
                                    [Variable("Fld"), Variable("Val")],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Size",
                                    [Variable("Ctx"), Variable("Fld"), Variable("Size")],
                                ),
                                LessEqual(
                                    Variable("Size"),
                                    Call(
                                        self.prefix * message.identifier * "Available_Space",
                                        [Variable("Ctx"), Variable("Fld")],
                                    ),
                                ),
                                *(
                                    [
                                        If(
                                            [
                                                (
                                                    Call(
                                                        self.prefix
                                                        * message.identifier
                                                        * "Composite_Field",
                                                        [Variable("Fld")],
                                                    ),
                                                    Equal(
                                                        Mod(
                                                            Variable("Size"),
                                                            Size(const.TYPES_BYTE),
                                                        ),
                                                        Number(0),
                                                    ),
                                                ),
                                            ],
                                            Variable("State_Valid"),
                                        ),
                                    ]
                                    if composite_fields
                                    else [
                                        Variable("State_Valid"),
                                    ]
                                ),
                            ),
                        ),
                        Postcondition(
                            AndThen(
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                                *(
                                    [
                                        Call(
                                            "Invalid_Successor",
                                            [Variable("Ctx"), Variable("Fld")],
                                        ),
                                    ]
                                    if len(message.fields) > 1
                                    else []
                                ),
                                Equal(
                                    Variable("Buffer_First"),
                                    Call(
                                        const.TYPES_TO_INDEX,
                                        [
                                            Call(
                                                "Field_First",
                                                [Variable("Ctx"), Variable("Fld")],
                                            ),
                                        ],
                                    ),
                                ),
                                Equal(
                                    Variable("Buffer_Last"),
                                    Call(
                                        const.TYPES_TO_INDEX,
                                        [
                                            Add(
                                                Call(
                                                    "Field_First",
                                                    [Variable("Ctx"), Variable("Fld")],
                                                ),
                                                Variable("Size"),
                                                -Number(1),
                                            ),
                                        ],
                                    ),
                                ),
                                Equal(
                                    Variable("Offset"),
                                    Call(
                                        const.TYPES_OFFSET,
                                        [
                                            Mod(
                                                Sub(
                                                    Size(const.TYPES_BYTE),
                                                    Mod(
                                                        Add(
                                                            Call(
                                                                "Field_First",
                                                                [
                                                                    Variable("Ctx"),
                                                                    Variable("Fld"),
                                                                ],
                                                            ),
                                                            Variable("Size"),
                                                            -Number(1),
                                                        ),
                                                        Size(const.TYPES_BYTE),
                                                    ),
                                                ),
                                                Size(const.TYPES_BYTE),
                                            ),
                                        ],
                                    ),
                                ),
                                *common.context_invariant(message),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Variable("Ctx.Buffer_First"),
                                        Variable("Ctx.Buffer_Last"),
                                        Variable("Ctx.First"),
                                        Variable("Ctx.Last"),
                                        Call("Has_Buffer", [Variable("Ctx")]),
                                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                                    ]
                                ],
                                Call("Sufficient_Space", [Variable("Ctx"), Variable("Fld")]),
                                If(
                                    [
                                        (
                                            And(
                                                Variable("State_Valid"),
                                                Greater(Variable("Size"), Number(0)),
                                            ),
                                            Call(
                                                "Valid",
                                                [Variable("Ctx"), Variable("Fld")],
                                            ),
                                        ),
                                    ],
                                    Call("Well_Formed", [Variable("Ctx"), Variable("Fld")]),
                                ),
                                self.scalar_setter_and_getter_relation(message),
                                *(
                                    [
                                        common.unchanged_cursor_before_or_invalid(
                                            Variable("Fld"),
                                            loop_entry=False,
                                            or_invalid=False,
                                            including_limit=False,
                                        ),
                                    ]
                                    if len(message.fields) > 1
                                    else []
                                ),
                            ),
                        ),
                    ],
                ),
            ],
        )

    def create_scalar_setter_procedures(
        self,
        message: Message,
        scalar_fields: Mapping[Field, Scalar],
    ) -> UnitPart:
        if not scalar_fields:
            return UnitPart()

        def specification(
            field: Field,
            field_type: Type,
            use_enum_records_directly: bool = False,
        ) -> ProcedureSpecification:
            if field_type.package == BUILTINS_PACKAGE:
                type_identifier = ID(field_type.name)
            elif (
                isinstance(field_type, Enumeration)
                and field_type.always_valid
                and not use_enum_records_directly
            ):
                type_identifier = common.prefixed_type_identifier(
                    common.full_enum_name(field_type),
                    self.prefix,
                )
            else:
                type_identifier = common.prefixed_type_identifier(
                    field_type.identifier,
                    self.prefix,
                )

            return ProcedureSpecification(
                f"Set_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Val"], type_identifier)],
            )

        def precondition(
            field: Field,
            field_type: Scalar,
            use_enum_records_directly: bool = False,
        ) -> Precondition:
            return Precondition(
                AndThen(
                    *self.setter_preconditions(
                        message,
                        self.prefix * message.identifier * field.affixed_name,
                    ),
                    Call(
                        f"Valid_{field_type.name}"
                        if is_builtin_type(field_type.identifier)
                        else self.prefix * field_type.package * f"Valid_{field_type.name}",
                        [
                            Variable("Val")
                            if use_enum_records_directly
                            else Call(
                                common.to_base_integer(self.prefix, field_type.package),
                                [Variable("Val")],
                            ),
                        ],
                    ),
                    common.sufficient_space_for_field_condition(
                        self.prefix * message.identifier,
                        Variable(self.prefix * message.identifier * field.affixed_name),
                    ),
                    common.field_condition_call(
                        self.prefix,
                        message,
                        field,
                        value=Call(
                            common.to_base_integer(self.prefix, field_type.package),
                            [Variable("Val")],
                        ),
                    ),
                ),
            )

        def postcondition(
            field: Field,
            field_type: Scalar,
            use_enum_records_directly: bool = False,
        ) -> Postcondition:
            return Postcondition(
                And(
                    Call("Has_Buffer", [Variable("Ctx")]),
                    Call(
                        "Valid",
                        [Variable("Ctx"), Variable(field.affixed_name)],
                    ),
                    *(
                        [
                            Equal(
                                Call(f"Get_{field.name}", [Variable("Ctx")]),
                                Aggregate(TRUE, Variable("Val"))
                                if isinstance(field_type, Enumeration)
                                and field_type.always_valid
                                and not use_enum_records_directly
                                else Variable("Val"),
                            ),
                        ]
                        if int(field_type.value_count) > 1
                        else []
                    ),
                    *self.public_setter_postconditions(message, field),
                    *common.context_cursor_unchanged(message, field, predecessors=True),
                ),
            )

        def body(
            field: Field,
            field_type: Scalar,
            use_enum_records_directly: bool = False,
        ) -> SubprogramBody:
            return SubprogramBody(
                specification(field, field_type, use_enum_records_directly),
                [],
                [
                    CallStatement(
                        "Set_Scalar",
                        [
                            Variable("Ctx"),
                            Variable(field.affixed_name),
                            Call(
                                common.to_base_integer(self.prefix, field_type.package),
                                [Variable("Val")],
                            ),
                        ],
                    ),
                ],
                [
                    precondition(field, field_type, use_enum_records_directly),
                    postcondition(field, field_type, use_enum_records_directly),
                ]
                if use_enum_records_directly
                else [],
            )

        byte_orders = set(message.byte_order.values())
        uniform_byte_order = byte_orders.pop() if len(byte_orders) == 1 else None

        return UnitPart(
            [
                Pragma(
                    "Warnings",
                    [Variable("Off"), String('aspect "*" not enforced on inlined subprogram "*"')],
                ),
                *[
                    SubprogramDeclaration(
                        specification(f, t),
                        [
                            InlineAlways(),
                            precondition(f, t),
                            postcondition(f, t),
                        ],
                    )
                    for f, t in scalar_fields.items()
                ],
                Pragma(
                    "Warnings",
                    [Variable("On"), String('aspect "*" not enforced on inlined subprogram "*"')],
                ),
            ],
            [
                SubprogramBody(
                    ProcedureSpecification(
                        "Set_Scalar",
                        [
                            InOutParameter(["Ctx"], "Context"),
                            Parameter(["Fld"], "Field"),
                            Parameter(["Val"], const.TYPES_BASE_INT),
                        ],
                    ),
                    [
                        ObjectDeclaration(["Buffer_First", "Buffer_Last"], const.TYPES_INDEX),
                        ObjectDeclaration(["Offset"], const.TYPES_OFFSET),
                        ObjectDeclaration(
                            ["Size"],
                            const.TYPES_BIT_LENGTH,
                            Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                            constant=True,
                        ),
                    ],
                    [
                        CallStatement(
                            "Set",
                            [
                                Variable("Ctx"),
                                Variable("Fld"),
                                Variable("Val"),
                                Variable("Size"),
                                TRUE,
                                Variable("Buffer_First"),
                                Variable("Buffer_Last"),
                                Variable("Offset"),
                            ],
                        ),
                        CallStatement(
                            const.TYPES * "Lemma_Size",
                            [
                                Variable("Val"),
                                Call("Positive", [Variable("Size")]),
                            ],
                        ),
                        CallStatement(
                            const.TYPES_OPERATIONS * "Insert",
                            [
                                Variable("Val"),
                                Variable("Ctx.Buffer.all"),
                                Variable("Buffer_First"),
                                Variable("Buffer_Last"),
                                Variable("Offset"),
                                Call("Positive", [Variable("Size")]),
                                Variable(
                                    const.TYPES_HIGH_ORDER_FIRST
                                    if uniform_byte_order == ByteOrder.HIGH_ORDER_FIRST
                                    else const.TYPES_LOW_ORDER_FIRST,
                                )
                                if uniform_byte_order
                                else If(
                                    [
                                        (
                                            In(
                                                Variable("Fld"),
                                                ChoiceList(
                                                    *[
                                                        Variable(f.affixed_name)
                                                        for f, b in message.byte_order.items()
                                                        if b == ByteOrder.LOW_ORDER_FIRST
                                                    ],
                                                ),
                                            ),
                                            Variable(const.TYPES_LOW_ORDER_FIRST),
                                        ),
                                    ],
                                    Variable(const.TYPES_HIGH_ORDER_FIRST),
                                ),
                            ],
                        ),
                    ],
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(message, "Fld"),
                                In(
                                    Variable("Fld"),
                                    ChoiceList(*[Variable(f.affixed_name) for f in scalar_fields]),
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Value",
                                    [Variable("Fld"), Variable("Val")],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Size",
                                    [
                                        Variable("Ctx"),
                                        Variable("Fld"),
                                        Call(
                                            self.prefix * message.identifier * "Field_Size",
                                            [Variable("Ctx"), Variable("Fld")],
                                        ),
                                    ],
                                ),
                                common.sufficient_space_for_field_condition(
                                    self.prefix * message.identifier,
                                    Variable("Fld"),
                                ),
                                In(
                                    Call(
                                        self.prefix * message.identifier * "Field_Size",
                                        [Variable("Ctx"), Variable("Fld")],
                                    ),
                                    ValueRange(Number(1), Size(const.TYPES_BASE_INT)),
                                ),
                                Call(
                                    const.TYPES * "Fits_Into",
                                    [
                                        Variable("Val"),
                                        Call(
                                            "Natural",
                                            [
                                                Call(
                                                    self.prefix * message.identifier * "Field_Size",
                                                    [Variable("Ctx"), Variable("Fld")],
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            ),
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call(
                                    "Valid",
                                    [Variable("Ctx"), Variable("Fld")],
                                ),
                                *(
                                    [
                                        Call(
                                            "Invalid_Successor",
                                            [Variable("Ctx"), Variable("Fld")],
                                        ),
                                    ]
                                    if len(message.fields) > 1
                                    else []
                                ),
                                self.scalar_setter_and_getter_relation(message),
                                *(
                                    [
                                        common.unchanged_cursor_before_or_invalid(
                                            Variable("Fld"),
                                            loop_entry=False,
                                            or_invalid=False,
                                        ),
                                    ]
                                    if len(message.fields) > 1
                                    else []
                                ),
                                *common.context_invariant(message),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Call("Has_Buffer", [Variable("Ctx")]),
                                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                                    ]
                                ],
                            ),
                        ),
                    ],
                ),
                *[body(f, t) for f, t in scalar_fields.items()],
                *[
                    body(f, t, use_enum_records_directly=True)
                    for f, t in scalar_fields.items()
                    if message.is_definite and isinstance(t, Enumeration) and t.always_valid
                ],
            ],
        )

    def create_composite_setter_empty_procedures(self, message: Message) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Set_{field.name}_Empty",
                [InOutParameter(["Ctx"], "Context")],
            )

        return UnitPart(
            [
                *[
                    SubprogramDeclaration(
                        specification(f),
                        [
                            Precondition(
                                AndThen(
                                    *self.setter_preconditions(
                                        message,
                                        self.prefix * message.identifier * f.affixed_name,
                                    ),
                                    *self.composite_setter_preconditions(message, f),
                                    *self.composite_setter_field_condition_precondition(
                                        message,
                                        f,
                                        empty=True,
                                    ),
                                    Equal(
                                        Call(
                                            self.prefix * message.identifier * "Field_Size",
                                            [
                                                Variable("Ctx"),
                                                Variable(
                                                    self.prefix
                                                    * message.identifier
                                                    * f.affixed_name,
                                                ),
                                            ],
                                        ),
                                        Number(0),
                                    ),
                                ),
                            ),
                            Postcondition(
                                And(
                                    *self.composite_setter_postconditions(f),
                                    *self.public_setter_postconditions(message, f),
                                ),
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
                            ["Unused_Buffer_First", "Unused_Buffer_Last"],
                            const.TYPES_INDEX,
                        ),
                        ObjectDeclaration(["Unused_Offset"], const.TYPES_OFFSET),
                    ],
                    [
                        CallStatement(
                            "Set",
                            [
                                Variable("Ctx"),
                                Variable(f.affixed_name),
                                Number(0),
                                Number(0),
                                TRUE,
                                Variable("Unused_Buffer_First"),
                                Variable("Unused_Buffer_Last"),
                                Variable("Unused_Offset"),
                            ],
                        ),
                    ],
                )
                for f, t in message.field_types.items()
                if message.is_possibly_empty(f)
            ],
        )

    def create_sequence_setter_procedures(
        self,
        message: Message,
        sequence_fields: Mapping[Field, Type],
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Set_{field.name}",
                [
                    InOutParameter(["Ctx"], "Context"),
                    Parameter(
                        ["Seq_Ctx"],
                        self.prefix * common.sequence_name(message, field) * "Context",
                    ),
                ],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(
                                    message,
                                    self.prefix * message.identifier * f.affixed_name,
                                ),
                                *self.composite_setter_preconditions(message, f),
                                *self.composite_setter_field_condition_precondition(
                                    message,
                                    f,
                                    empty=True,
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Length",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                        Call(
                                            self.prefix
                                            * common.sequence_name(message, f)
                                            * "Byte_Size",
                                            [Variable("Seq_Ctx")],
                                        ),
                                    ],
                                ),
                                Call(
                                    self.prefix * common.sequence_name(message, f) * "Has_Buffer",
                                    [Variable("Seq_Ctx")],
                                ),
                                Call(
                                    self.prefix * common.sequence_name(message, f) * "Valid",
                                    [Variable("Seq_Ctx")],
                                ),
                            ),
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
                                        ),
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
                        ObjectDeclaration(
                            ["Size"],
                            const.TYPES_BIT_LENGTH,
                            Call(
                                const.TYPES_TO_BIT_LENGTH,
                                [
                                    Call(
                                        self.prefix
                                        * common.sequence_name(message, f)
                                        * "Byte_Size",
                                        [Variable("Seq_Ctx")],
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(["Unused_First", "Unused_Last"], const.TYPES_BIT_INDEX),
                        ObjectDeclaration(["Buffer_First", "Buffer_Last"], const.TYPES_INDEX),
                        ObjectDeclaration(["Unused_Offset"], const.TYPES_OFFSET),
                    ],
                    [
                        CallStatement(
                            "Set",
                            [
                                Variable("Ctx"),
                                Variable(f.affixed_name),
                                Number(0),
                                Variable("Size"),
                                TRUE,
                                Variable("Buffer_First"),
                                Variable("Buffer_Last"),
                                Variable("Unused_Offset"),
                            ],
                        ),
                        CallStatement(
                            self.prefix * common.sequence_name(message, f) * "Copy",
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
                                *self.setter_preconditions(
                                    message,
                                    self.prefix * message.identifier * f.affixed_name,
                                ),
                                *self.composite_setter_preconditions(message, f),
                                Call(
                                    self.prefix * message.identifier * "Valid_Length",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                        Length("Data"),
                                    ],
                                ),
                                common.sufficient_space_for_field_condition(
                                    self.prefix * message.identifier,
                                    Variable(self.prefix * message.identifier * f.affixed_name),
                                    Mul(Length("Data"), Size(const.TYPES_BYTE)),
                                ),
                                *self.composite_setter_field_condition_precondition(message, f),
                            ),
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(f),
                                *self.public_setter_postconditions(message, f),
                                Call(
                                    "Equal",
                                    [
                                        Variable("Ctx"),
                                        Variable(f.affixed_name),
                                        Variable("Data"),
                                    ],
                                ),
                            ),
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
                            ["Buffer_First"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_TO_INDEX,
                                [Call("Field_First", [Variable("Ctx"), Variable(f.affixed_name)])],
                            ),
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
                            f"Initialize_{f.name}_Private",
                            [Variable("Ctx"), Length("Data")],
                        ),
                        PragmaStatement(
                            "Assert",
                            [
                                Equal(
                                    Variable("Buffer_Last"),
                                    Call(
                                        const.TYPES_TO_INDEX,
                                        [
                                            Call(
                                                "Field_Last",
                                                [Variable("Ctx"), Variable(f.affixed_name)],
                                            ),
                                        ],
                                    ),
                                ),
                            ],
                        ),
                        Assignment(
                            Slice(
                                Selected(Variable("Ctx.Buffer"), "all"),
                                Variable("Buffer_First"),
                                Variable("Buffer_Last"),
                            ),
                            Variable("Data"),
                        ),
                        # Improve provability of `Equal` in postcondition
                        PragmaStatement(
                            "Assert",
                            [
                                Equal(
                                    Slice(
                                        Selected(Variable("Ctx.Buffer"), "all"),
                                        Call(
                                            const.TYPES_TO_INDEX,
                                            [
                                                Call(
                                                    "Field_First",
                                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                                ),
                                            ],
                                        ),
                                        Call(
                                            const.TYPES_TO_INDEX,
                                            [
                                                Call(
                                                    "Field_Last",
                                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                                ),
                                            ],
                                        ),
                                    ),
                                    Variable("Data"),
                                ),
                            ],
                        ),
                    ],
                )
                for f, t in message.field_types.items()
                if isinstance(t, Opaque)
            ],
        )

    def create_generic_opaque_setter_procedures(self, message: Message) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Generic_Set_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Length"], const.TYPES_LENGTH)],
            )

        def formal_parameters(field: Field) -> list[FormalSubprogramDeclaration]:
            return [
                FormalSubprogramDeclaration(
                    ProcedureSpecification(
                        f"Process_{field.name}",
                        [
                            OutParameter([field.name], const.TYPES_BYTES),
                        ],
                    ),
                ),
                FormalSubprogramDeclaration(
                    FunctionSpecification(
                        "Process_Data_Pre",
                        "Boolean",
                        [Parameter(["Length"], const.TYPES_LENGTH)],
                    ),
                ),
            ]

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(
                                    message,
                                    self.prefix * message.identifier * f.affixed_name,
                                ),
                                *self.composite_setter_preconditions(message, f),
                                Call(
                                    self.prefix * message.identifier * "Valid_Length",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                        Variable("Length"),
                                    ],
                                ),
                                GreaterEqual(
                                    Call(
                                        const.TYPES_TO_LENGTH,
                                        [
                                            Call(
                                                self.prefix
                                                * message.identifier
                                                * "Available_Space",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(
                                                        self.prefix
                                                        * message.identifier
                                                        * f.affixed_name,
                                                    ),
                                                ],
                                            ),
                                        ],
                                    ),
                                    Variable("Length"),
                                ),
                                Call(
                                    "Process_Data_Pre",
                                    [
                                        Variable("Length"),
                                    ],
                                ),
                            ),
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(f),
                                *self.public_setter_postconditions(message, f),
                            ),
                        ),
                    ],
                    formal_parameters(f),
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
                            Call(
                                const.TYPES_TO_INDEX,
                                [
                                    Add(
                                        Variable("First"),
                                        Call(const.TYPES_TO_BIT_LENGTH, [Variable("Length")]),
                                        -Number(1),
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
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
                                Variable("Length"),
                            ],
                        ),
                    ],
                )
                for f, t in message.field_types.items()
                if isinstance(t, Opaque)
            ],
        )

    def create_composite_initialize_procedures(
        self,
        message: Message,
        fields_with_explicit_size: list[Field],
        fields_with_implicit_size: list[Field],
    ) -> UnitPart:
        def specification_private(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}_Private",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Length"], const.TYPES_LENGTH)],
            )

        def specification_public(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}",
                [InOutParameter(["Ctx"], "Context")],
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
                                            *self.setter_preconditions(
                                                message,
                                                self.prefix * message.identifier * f.affixed_name,
                                            ),
                                            *self.composite_setter_preconditions(message, f),
                                        ),
                                    ),
                                    Postcondition(
                                        AndThen(
                                            *self.composite_setter_postconditions(f),
                                            *self.public_setter_postconditions(message, f),
                                        ),
                                    ),
                                ],
                            ),
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
                                            *self.setter_preconditions(
                                                message,
                                                self.prefix * message.identifier * f.affixed_name,
                                            ),
                                            Call(
                                                self.prefix * message.identifier * "Valid_Length",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(
                                                        self.prefix
                                                        * message.identifier
                                                        * f.affixed_name,
                                                    ),
                                                    Variable("Length"),
                                                ],
                                            ),
                                            *self.composite_setter_preconditions(
                                                message,
                                                f,
                                                Call(
                                                    const.TYPES_TO_BIT_LENGTH,
                                                    [Variable("Length")],
                                                ),
                                            ),
                                        ),
                                    ),
                                    Postcondition(
                                        AndThen(
                                            *self.composite_setter_postconditions(f),
                                            Equal(
                                                Call(
                                                    "Field_Size",
                                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                                ),
                                                Call(
                                                    const.TYPES_TO_BIT_LENGTH,
                                                    [Variable("Length")],
                                                ),
                                            ),
                                            *self.public_setter_postconditions(message, f),
                                        ),
                                    ),
                                ],
                            ),
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
                                        "Field_First",
                                        [Variable("Ctx"), Variable(f.affixed_name)],
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
                        [
                            # Improve provability of context predicate
                            PragmaStatement(
                                "Assert",
                                [Equal(Mod(Variable("Last"), Size(const.TYPES_BYTE)), Number(0))],
                            ),
                            *common.initialize_field_statements(f, reset_written_last=True),
                        ],
                        [
                            Precondition(
                                AndThen(
                                    *self.setter_preconditions(
                                        message,
                                        self.prefix * message.identifier * f.affixed_name,
                                    ),
                                    Call(
                                        self.prefix * message.identifier * "Valid_Length",
                                        [
                                            Variable("Ctx"),
                                            Variable(
                                                self.prefix * message.identifier * f.affixed_name,
                                            ),
                                            Variable("Length"),
                                        ],
                                    ),
                                    GreaterEqual(
                                        Call(
                                            const.TYPES_TO_LENGTH,
                                            [
                                                Call(
                                                    self.prefix
                                                    * message.identifier
                                                    * "Available_Space",
                                                    [
                                                        Variable("Ctx"),
                                                        Variable(
                                                            self.prefix
                                                            * message.identifier
                                                            * f.affixed_name,
                                                        ),
                                                    ],
                                                ),
                                            ],
                                        ),
                                        Variable("Length"),
                                    ),
                                    Equal(
                                        Mod(
                                            Call(
                                                self.prefix * message.identifier * "Field_First",
                                                [
                                                    Variable("Ctx"),
                                                    Variable(
                                                        self.prefix
                                                        * message.identifier
                                                        * f.affixed_name,
                                                    ),
                                                ],
                                            ),
                                            Size(const.TYPES_BYTE),
                                        ),
                                        Number(1),
                                    ),
                                ),
                            ),
                            Postcondition(
                                AndThen(
                                    *self.composite_setter_postconditions(f),
                                    Equal(
                                        Call(
                                            "Field_Size",
                                            [Variable("Ctx"), Variable(f.affixed_name)],
                                        ),
                                        Call(const.TYPES_TO_BIT_LENGTH, [Variable("Length")]),
                                    ),
                                    *self.private_setter_postconditions(message, f),
                                ),
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
                                                    ),
                                                ],
                                            ),
                                        ],
                                    ),
                                ],
                            ),
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
                            ),
                        ]
                        if f in fields_with_implicit_size
                        else []
                    ),
                ]
            ],
        )

    def setter_preconditions(self, message: Message, field_name: StrID) -> list[Expr]:
        return [
            Not(Constrained("Ctx")),
            Call(self.prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
            Call(
                self.prefix * message.identifier * "Valid_Next",
                [Variable("Ctx"), Variable(field_name)],
            ),
        ]

    def private_setter_postconditions(self, message: Message, field: Field) -> list[Expr]:
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

    def public_setter_postconditions(self, message: Message, field: Field) -> list[Expr]:
        return [
            *(
                [
                    If(
                        [
                            (
                                Call("Well_Formed_Message", [Variable("Ctx")]),
                                Equal(
                                    Call("Message_Last", [Variable("Ctx")]),
                                    Call(
                                        "Field_Last",
                                        [Variable("Ctx"), Variable(field.affixed_name)],
                                    ),
                                ),
                            ),
                        ],
                    ),
                ]
                if field in message.direct_predecessors(FINAL)
                else []
            ),
            *self.setter_postconditions(message, field),
        ]

    def setter_postconditions(self, message: Message, field: Field) -> list[Expr]:
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
                + [Call("Field_First", [Variable("Ctx"), Variable(field.affixed_name)])]
            ],
        ]

    @staticmethod
    def composite_setter_postconditions(field: Field) -> list[Expr]:
        return [
            Call("Has_Buffer", [Variable("Ctx")]),
            Call("Well_Formed", [Variable("Ctx"), Variable(field.affixed_name)]),
        ]

    def composite_setter_field_condition_precondition(
        self,
        message: Message,
        field: Field,
        empty: bool = False,
    ) -> list[Expr]:
        return [
            common.field_condition_call(
                self.prefix,
                message,
                field,
                aggregate=None if empty else Variable("Data"),
                size=None if empty else Call(const.TYPES_TO_BIT_LENGTH, [Length("Data")]),
            ),
        ]

    def composite_setter_preconditions(
        self,
        message: Message,
        field: Field,
        size: Optional[Expr] = None,
    ) -> list[Expr]:
        return [
            common.sufficient_space_for_field_condition(
                self.prefix * message.identifier,
                Variable(self.prefix * message.identifier * field.affixed_name),
                size,
            ),
        ]

    def scalar_setter_and_getter_relation(
        self,
        message: Message,
    ) -> Expr:
        return AndThen(
            Equal(
                Selected(
                    Indexed(
                        Selected(Variable("Ctx"), "Cursors"),
                        Variable("Fld"),
                    ),
                    "Value",
                ),
                Variable("Val"),
            ),
            If(
                [
                    (
                        AndThen(
                            In(
                                Variable("Fld"),
                                ChoiceList(
                                    *[
                                        Variable(f.affixed_name)
                                        for f in message.direct_predecessors(FINAL)
                                    ],
                                ),
                            ),
                            Call(
                                "Well_Formed_Message",
                                [Variable("Ctx")],
                            ),
                        ),
                        Equal(
                            Call(
                                "Message_Last",
                                [Variable("Ctx")],
                            ),
                            Call(
                                "Field_Last",
                                [
                                    Variable("Ctx"),
                                    Variable("Fld"),
                                ],
                            ),
                        ),
                    ),
                ],
            ),
        )

    @staticmethod
    def _update_last(
        message: Optional[Message] = None,
        field: Optional[Field] = None,
    ) -> list[Statement]:
        assert (message and field) or not (message or field)
        last = (
            Variable("Last")
            if message and field and field in message.direct_predecessors(FINAL)
            else Mul(
                Div(
                    Add(
                        Variable("Last"),
                        Size(const.TYPES_BYTE),
                        -Number(1),
                    ),
                    Size(const.TYPES_BYTE),
                ),
                Size(const.TYPES_BYTE),
            )
        )
        return [
            # Improve provability of context predicate
            PragmaStatement("Assert", [Equal(Mod(last, Size(const.TYPES_BYTE)), Number(0))]),
            # Eng/RecordFlux/RecordFlux#868
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
