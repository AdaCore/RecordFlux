from __future__ import annotations

from collections.abc import Mapping, Sequence

import rflx.expression as expr
from rflx.ada import (
    TRUE,
    Add,
    And,
    AndThen,
    Assignment,
    Call,
    CallStatement,
    Case,
    ChoiceList,
    Div,
    Equal,
    Expr,
    ExpressionFunctionDeclaration,
    First,
    ForIn,
    FormalSubprogramDeclaration,
    ForSomeIn,
    FunctionSpecification,
    Ghost,
    If,
    IfStatement,
    In,
    Indexed,
    InOutParameter,
    Last,
    Length,
    Less,
    LessEqual,
    Mod,
    Mul,
    NamedAggregate,
    Not,
    Number,
    ObjectDeclaration,
    Or,
    OutParameter,
    Parameter,
    Postcondition,
    Pragma,
    PragmaStatement,
    Precondition,
    ProcedureSpecification,
    Result,
    ReturnStatement,
    Selected,
    Size,
    Slice,
    String,
    Sub,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
    Variable,
)
from rflx.const import BUILTINS_PACKAGE
from rflx.identifier import ID
from rflx.model import FINAL, ByteOrder, Composite, Field, Message, Scalar, Type

from . import common, const


class ParserGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix

    def create_get_function(
        self,
        message: Message,
        scalar_fields: Mapping[Field, Scalar],
        composite_fields: Sequence[Field],
    ) -> UnitPart:
        if not scalar_fields:
            return UnitPart()

        comparison_to_aggregate = any(
            (isinstance(t, Composite) and common.has_aggregate_dependent_condition(message, f))
            for f, t in message.field_types.items()
        )

        big_endian_fields = [
            f for f in scalar_fields if message.byte_order[f] == ByteOrder.HIGH_ORDER_FIRST
        ]
        little_endian_fields = [
            f for f in scalar_fields if message.byte_order[f] == ByteOrder.LOW_ORDER_FIRST
        ]

        return UnitPart(
            [],
            [
                SubprogramBody(
                    FunctionSpecification(
                        "Get",
                        const.TYPES_BASE_INT,
                        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
                    ),
                    [
                        *common.field_bit_location_declarations(Variable("Fld")),
                        ObjectDeclaration(
                            ["Buffer_First"],
                            const.TYPES_INDEX,
                            Call(const.TYPES_TO_INDEX, [Variable("First")]),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Buffer_Last"],
                            const.TYPES_INDEX,
                            Call(const.TYPES_TO_INDEX, [Variable("Last")]),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Offset"],
                            const.TYPES_OFFSET,
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
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Size"],
                            "Positive",
                            Case(
                                Variable("Fld"),
                                [
                                    *[
                                        (Variable(f.affixed_name), t.size.ada_expr())
                                        for f, t in scalar_fields.items()
                                    ],
                                    *(
                                        [(Variable("others"), Last("Positive"))]
                                        if composite_fields
                                        else []
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Byte_Order"],
                            const.TYPES_BYTE_ORDER,
                            If(
                                [
                                    (
                                        In(
                                            Variable("Fld"),
                                            ChoiceList(
                                                *[
                                                    Variable(f.affixed_name)
                                                    for f in big_endian_fields
                                                ],
                                            ),
                                        ),
                                        Variable(const.TYPES_HIGH_ORDER_FIRST),
                                    ),
                                ],
                                Variable(const.TYPES_LOW_ORDER_FIRST),
                            )
                            if big_endian_fields and little_endian_fields
                            else Variable(const.TYPES_HIGH_ORDER_FIRST)
                            if big_endian_fields
                            else Variable(const.TYPES_LOW_ORDER_FIRST),
                            constant=True,
                        ),
                    ]
                    if scalar_fields or comparison_to_aggregate
                    else [],
                    [
                        ReturnStatement(
                            Call(
                                const.TYPES_OPERATIONS * "Extract",
                                [
                                    Variable("Ctx.Buffer.all"),
                                    Variable("Buffer_First"),
                                    Variable("Buffer_Last"),
                                    Variable("Offset"),
                                    Variable("Size"),
                                    Variable("Byte_Order"),
                                ],
                            ),
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
                                    self.prefix * message.identifier * "Sufficient_Buffer_Length",
                                    [Variable("Ctx"), Variable("Fld")],
                                ),
                                *(
                                    [
                                        Not(
                                            Call(
                                                self.prefix
                                                * message.identifier
                                                * "Composite_Field",
                                                [Variable("Fld")],
                                            ),
                                        ),
                                    ]
                                    if composite_fields
                                    else []
                                ),
                            ),
                        ),
                    ],
                ),
            ],
        )

    def create_verify_procedure(
        self,
        message: Message,
        scalar_fields: Mapping[Field, Scalar],
        composite_fields: Sequence[Field],
    ) -> UnitPart:
        specification = ProcedureSpecification(
            "Verify",
            [InOutParameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        valid_field_condition = AndThen(
            Call(
                "Valid_Value",
                [Variable("Fld"), Variable("Value")],
            ),
            Call(
                "Field_Condition",
                [
                    Variable("Ctx"),
                    Variable("Fld"),
                    *([Variable("Value")] if common.has_value_dependent_condition(message) else []),
                    *(
                        [
                            Slice(
                                Variable("Ctx.Buffer.all"),
                                Call(
                                    const.TYPES_TO_INDEX,
                                    [Call("Field_First", [Variable("Ctx"), Variable("Fld")])],
                                ),
                                Call(
                                    const.TYPES_TO_INDEX,
                                    [Call("Field_Last", [Variable("Ctx"), Variable("Fld")])],
                                ),
                            ),
                        ]
                        if common.has_aggregate_dependent_condition(message)
                        else []
                    ),
                    *(
                        [Call("Field_Size", [Variable("Ctx"), Variable("Fld")])]
                        if common.has_size_dependent_condition(message)
                        else []
                    ),
                ],
            ),
        )

        last = Mul(
            Div(
                Add(
                    Call("Field_Last", [Variable("Ctx"), Variable("Fld")]),
                    Size(const.TYPES_BYTE),
                    -Number(1),
                ),
                Size(const.TYPES_BYTE),
            ),
            Size(const.TYPES_BYTE),
        )
        set_cursors_statements = [
            *(
                [
                    PragmaStatement(
                        "Assert",
                        [
                            If(
                                [
                                    (
                                        Or(
                                            *[
                                                Equal(Variable("Fld"), Variable(f.affixed_name))
                                                for f in message.direct_predecessors(FINAL)
                                            ],
                                        ),
                                        Equal(
                                            Mod(
                                                Call(
                                                    "Field_Last",
                                                    [Variable("Ctx"), Variable("Fld")],
                                                ),
                                                Size(const.TYPES_BYTE),
                                            ),
                                            Number(0),
                                        ),
                                    ),
                                ],
                            ),
                        ],
                    ),
                ]
                if len(message.fields) > 1
                else []
            ),
            # Improve provability of context predicate
            PragmaStatement("Assert", [Equal(Mod(last, Size(const.TYPES_BYTE)), Number(0))]),
            Assignment(Variable("Ctx.Verified_Last"), last),
            PragmaStatement(
                "Assert",
                [
                    LessEqual(
                        Call("Field_Last", [Variable("Ctx"), Variable("Fld")]),
                        Variable("Ctx.Verified_Last"),
                    ),
                ],
            ),
            IfStatement(
                [
                    (
                        Call("Composite_Field", [Variable("Fld")]),
                        [set_context_cursor_composite_field("Fld")],
                    ),
                ],
                [set_context_cursor_scalar()],
            )
            if scalar_fields and composite_fields
            else set_context_cursor_scalar()
            if scalar_fields and not composite_fields
            else set_context_cursor_composite_field("Fld"),
        ]

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            Call(
                                self.prefix * message.identifier * "Has_Buffer",
                                [Variable("Ctx")],
                            ),
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                *common.context_invariant(message),
                            ),
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    [ObjectDeclaration(["Value"], const.TYPES_BASE_INT)],
                    [
                        IfStatement(
                            [
                                (
                                    AndThen(
                                        Call(
                                            "Invalid",
                                            [Indexed(Variable("Ctx.Cursors"), Variable("Fld"))],
                                        ),
                                        Call(
                                            "Valid_Next",
                                            [Variable("Ctx"), Variable("Fld")],
                                        ),
                                    ),
                                    [
                                        IfStatement(
                                            [
                                                (
                                                    Call(
                                                        "Sufficient_Buffer_Length",
                                                        [Variable("Ctx"), Variable("Fld")],
                                                    ),
                                                    [
                                                        Assignment(
                                                            "Value",
                                                            If(
                                                                [
                                                                    (
                                                                        Call(
                                                                            "Composite_Field",
                                                                            [
                                                                                Variable("Fld"),
                                                                            ],
                                                                        ),
                                                                        Number(0),
                                                                    ),
                                                                ],
                                                                Call(
                                                                    "Get",
                                                                    [
                                                                        Variable("Ctx"),
                                                                        Variable("Fld"),
                                                                    ],
                                                                ),
                                                            )
                                                            if scalar_fields and composite_fields
                                                            else Call(
                                                                "Get",
                                                                [
                                                                    Variable("Ctx"),
                                                                    Variable("Fld"),
                                                                ],
                                                            )
                                                            if scalar_fields
                                                            else Number(0),
                                                        ),
                                                        IfStatement(
                                                            [
                                                                (
                                                                    valid_field_condition,
                                                                    set_cursors_statements,
                                                                ),
                                                            ],
                                                            [
                                                                Assignment(
                                                                    Indexed(
                                                                        Variable("Ctx.Cursors"),
                                                                        Variable("Fld"),
                                                                    ),
                                                                    NamedAggregate(
                                                                        ("others", Variable("<>")),
                                                                    ),
                                                                ),
                                                            ],
                                                        ),
                                                    ],
                                                ),
                                            ],
                                            [
                                                Assignment(
                                                    Indexed(
                                                        Variable("Ctx.Cursors"),
                                                        Variable("Fld"),
                                                    ),
                                                    NamedAggregate(
                                                        ("State", Variable("S_Incomplete")),
                                                        ("others", Variable("<>")),
                                                    ),
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                ),
            ],
        )

    def create_verify_message_procedure(self, message: Message) -> UnitPart:
        specification = ProcedureSpecification(
            "Verify_Message",
            [InOutParameter(["Ctx"], "Context")],
        )

        loop_invariant = And(
            Call("Has_Buffer", [Variable("Ctx")]),
            *common.context_invariant(message, loop_entry=True),
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            Call(
                                self.prefix * message.identifier * "Has_Buffer",
                                [Variable("Ctx")],
                            ),
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                *common.context_invariant(message),
                            ),
                        ),
                    ],
                ),
            ],
            [
                SubprogramBody(
                    specification,
                    [],
                    [
                        ForIn(
                            "F",
                            Variable("Field"),
                            [
                                PragmaStatement("Loop_Invariant", [loop_invariant]),
                                CallStatement("Verify", [Variable("Ctx"), Variable("F")]),
                            ],
                        ),
                    ],
                ),
            ],
        )

    @staticmethod
    def create_present_function() -> UnitPart:
        specification = FunctionSpecification(
            "Present",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        Call("Well_Formed", [Indexed(Variable("Ctx.Cursors"), Variable("Fld"))]),
                        Less(
                            Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "First"),
                            Add(
                                Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Last"),
                                Number(1),
                            ),
                        ),
                    ),
                ),
            ],
        )

    @staticmethod
    def create_well_formed_function() -> UnitPart:
        specification = FunctionSpecification(
            "Well_Formed",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Or(
                        *[
                            Equal(
                                Selected(
                                    Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                                    "State",
                                ),
                                Variable(s),
                            )
                            for s in ("S_Valid", "S_Well_Formed")
                        ],
                    ),
                ),
            ],
        )

    @staticmethod
    def create_valid_function() -> UnitPart:
        specification = FunctionSpecification(
            "Valid",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Postcondition(
                            If(
                                [
                                    (
                                        Result("Valid"),
                                        And(
                                            Call(
                                                "Well_Formed",
                                                [Variable("Ctx"), Variable("Fld")],
                                            ),
                                            Call("Present", [Variable("Ctx"), Variable("Fld")]),
                                        ),
                                    ),
                                ],
                            ),
                        ),
                    ],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        Equal(
                            Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "State"),
                            Variable("S_Valid"),
                        ),
                        Less(
                            Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "First"),
                            Add(
                                Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Last"),
                                Number(1),
                            ),
                        ),
                    ),
                ),
            ],
        )

    @staticmethod
    def create_incomplete_function() -> UnitPart:
        specification = FunctionSpecification(
            "Incomplete",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Equal(
                        Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "State"),
                        Variable("S_Incomplete"),
                    ),
                ),
            ],
        )

    @staticmethod
    def create_invalid_function() -> UnitPart:
        specification = FunctionSpecification(
            "Invalid",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Or(
                        Equal(
                            Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "State"),
                            Variable("S_Invalid"),
                        ),
                        Equal(
                            Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "State"),
                            Variable("S_Incomplete"),
                        ),
                    ),
                ),
            ],
        )

    def create_well_formed_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Well_Formed_Message",
            "Boolean",
            [Parameter(["Ctx"], "Context")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            Call(
                                self.prefix * message.identifier * "Has_Buffer",
                                [Variable("Ctx")],
                            ),
                        ),
                    ],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    self.valid_message_condition(message, well_formed=True),
                ),
            ],
        )

    def create_valid_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Message",
            "Boolean",
            [Parameter(["Ctx"], "Context")],
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Precondition(
                            Call(
                                self.prefix * message.identifier * "Has_Buffer",
                                [Variable("Ctx")],
                            ),
                        ),
                    ],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    self.valid_message_condition(message),
                ),
            ],
        )

    @staticmethod
    def create_incomplete_message_function() -> UnitPart:
        specification = FunctionSpecification(
            "Incomplete_Message",
            "Boolean",
            [Parameter(["Ctx"], "Context")],
        )

        return UnitPart(
            [
                # Eng/RecordFlux/Workarounds#47
                Pragma(
                    "Warnings",
                    [Variable("Off"), String("postcondition does not mention function result")],
                ),
                SubprogramDeclaration(specification, [Postcondition(TRUE)]),
                Pragma(
                    "Warnings",
                    [Variable("On"), String("postcondition does not mention function result")],
                ),
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    ForSomeIn(
                        "F",
                        Variable("Field"),
                        Call(
                            "Incomplete",
                            [Variable("Ctx"), Variable("F")],
                        ),
                    ),
                ),
            ],
        )

    def create_scalar_getter_functions(
        self,
        message: Message,
        scalar_fields: Mapping[Field, Scalar],
    ) -> UnitPart:
        def specification(field: Field, field_type: Type) -> FunctionSpecification:
            if field_type.package == BUILTINS_PACKAGE:
                type_identifier = ID(field_type.name)
            else:
                type_identifier = self.prefix * field_type.identifier

            return FunctionSpecification(
                f"Get_{field.name}",
                type_identifier,
                [Parameter(["Ctx"], "Context")],
            )

        def result(field: Field) -> Expr:
            return Call(
                "To_Actual",
                [Selected(Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)), "Value")],
            )

        return UnitPart(
            [
                # Eng/RecordFlux/Workarounds#31
                Pragma("Warnings", [Variable("Off"), String("precondition is always False")]),
                *[
                    SubprogramDeclaration(
                        specification(f, t),
                        [
                            Precondition(
                                Call(
                                    self.prefix * message.identifier * "Valid",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                    ],
                                ),
                            ),
                        ],
                    )
                    for f, t in scalar_fields.items()
                ],
                Pragma("Warnings", [Variable("On"), String("precondition is always False")]),
            ],
            private=[
                ExpressionFunctionDeclaration(specification(f, t), result(f))
                for f, t in scalar_fields.items()
            ],
        )

    def create_opaque_getter_functions(
        self,
        message: Message,
        opaque_fields: Sequence[Field],
    ) -> UnitPart:
        def name(field: Field) -> str:
            return f"Get_{field.name}"

        def specification(field: Field) -> FunctionSpecification:
            return FunctionSpecification(
                name(field),
                const.TYPES_BYTES,
                [Parameter(["Ctx"], "Context")],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Ghost(),
                        Precondition(
                            AndThen(
                                Call(
                                    self.prefix * message.identifier * "Has_Buffer",
                                    [Variable("Ctx")],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Well_Formed",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                    ],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Next",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                    ],
                                ),
                            ),
                        ),
                        Postcondition(
                            Equal(
                                Length(Result(name(f))),
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
                            ),
                        ),
                    ],
                )
                for f in opaque_fields
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_TO_INDEX,
                                [
                                    Selected(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
                                        ),
                                        "First",
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_TO_INDEX,
                                [
                                    Selected(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
                                        ),
                                        "Last",
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
                    ],
                    [
                        ReturnStatement(
                            Slice(Variable("Ctx.Buffer.all"), Variable("First"), Variable("Last")),
                        ),
                    ],
                )
                for f in opaque_fields
            ],
        )

    def create_opaque_getter_procedures(
        self,
        message: Message,
        opaque_fields: Sequence[Field],
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Get_{field.name}",
                [Parameter(["Ctx"], "Context"), OutParameter(["Data"], const.TYPES_BYTES)],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                Call(
                                    self.prefix * message.identifier * "Has_Buffer",
                                    [Variable("Ctx")],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Well_Formed",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                    ],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Valid_Next",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                    ],
                                ),
                                Equal(
                                    Length("Data"),
                                    Call(
                                        const.TYPES_TO_LENGTH,
                                        [
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
                                        ],
                                    ),
                                ),
                            ),
                        ),
                        Postcondition(
                            Call(
                                "Equal",
                                [
                                    Variable("Ctx"),
                                    Variable(f.affixed_name),
                                    Variable("Data"),
                                ],
                            ),
                        ),
                    ],
                )
                for f in opaque_fields
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_TO_INDEX,
                                [
                                    Selected(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
                                        ),
                                        "First",
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_TO_INDEX,
                                [
                                    Selected(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
                                        ),
                                        "Last",
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
                    ],
                    [
                        Assignment(
                            "Data",
                            NamedAggregate(("others", First(const.TYPES_BYTE))),
                        ),
                        Assignment(
                            Slice(
                                Variable("Data"),
                                First("Data"),
                                Add(First("Data"), Sub(Variable("Last"), Variable("First"))),
                            ),
                            Slice(
                                Variable("Ctx.Buffer.all"),
                                Variable("First"),
                                Variable("Last"),
                            ),
                        ),
                    ],
                )
                for f in opaque_fields
            ],
        )

    def create_generic_opaque_getter_procedures(
        self,
        message: Message,
        opaque_fields: Sequence[Field],
    ) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Generic_Get_{field.name}",
                [Parameter(["Ctx"], "Context")],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            And(
                                Call(
                                    self.prefix * message.identifier * "Has_Buffer",
                                    [Variable("Ctx")],
                                ),
                                Call(
                                    self.prefix * message.identifier * "Present",
                                    [
                                        Variable("Ctx"),
                                        Variable(self.prefix * message.identifier * f.affixed_name),
                                    ],
                                ),
                            ),
                        ),
                    ],
                    [
                        FormalSubprogramDeclaration(
                            ProcedureSpecification(
                                f"Process_{f.name}",
                                [Parameter([f.name], const.TYPES_BYTES)],
                            ),
                        ),
                    ],
                )
                for f in opaque_fields
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_TO_INDEX,
                                [
                                    Selected(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
                                        ),
                                        "First",
                                    ),
                                ],
                            ),
                            constant=True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_TO_INDEX,
                                [
                                    Selected(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
                                        ),
                                        "Last",
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
                                    Variable("Ctx.Buffer.all"),
                                    Variable("First"),
                                    Variable("Last"),
                                ),
                            ],
                        ),
                    ],
                )
                for f in opaque_fields
            ],
        )

    def valid_message_condition(self, message: Message, well_formed: bool = False) -> Expr:
        return (
            expr.Or(
                *[
                    expr.AndThen(
                        expr.Call(
                            "Well_Formed"
                            if well_formed and isinstance(message.field_types[l.source], Composite)
                            else "Valid",
                            [
                                expr.Variable("Ctx"),
                                expr.Variable(l.source.affixed_name, immutable=True),
                            ],
                        ),
                        l.condition,
                    )
                    for l in message.incoming(FINAL)
                    if l.target == FINAL
                ],
            )
            .substituted(common.substitution(message, self.prefix))
            .simplified()
            .ada_expr()
        )


def set_context_cursor_scalar() -> Assignment:
    return Assignment(
        Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
        NamedAggregate(
            ("State", Variable("S_Valid")),
            ("First", Call("Field_First", [Variable("Ctx"), Variable("Fld")])),
            ("Last", Call("Field_Last", [Variable("Ctx"), Variable("Fld")])),
            ("Value", Variable("Value")),
        ),
    )


def set_context_cursor_composite_field(field_name: str) -> Assignment:
    return Assignment(
        Indexed(
            Variable("Ctx.Cursors"),
            Variable(field_name),
        ),
        NamedAggregate(
            ("State", Variable("S_Well_Formed")),
            (
                "First",
                Call(
                    "Field_First",
                    [Variable("Ctx"), Variable(field_name)],
                ),
            ),
            (
                "Last",
                Call(
                    "Field_Last",
                    [Variable("Ctx"), Variable(field_name)],
                ),
            ),
            ("Value", Variable("Value")),
        ),
    )
