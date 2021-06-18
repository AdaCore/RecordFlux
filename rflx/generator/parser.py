# pylint: disable = too-many-lines

from typing import List, Mapping, Sequence, Tuple

import rflx.expression as expr
from rflx.ada import (
    FALSE,
    ID,
    TRUE,
    Add,
    And,
    AndThen,
    Assignment,
    Call,
    CallStatement,
    Case,
    CaseStatement,
    Div,
    Equal,
    Expr,
    ExpressionFunctionDeclaration,
    First,
    FormalSubprogramDeclaration,
    FunctionSpecification,
    GenericFunctionInstantiation,
    GreaterEqual,
    If,
    IfStatement,
    Indexed,
    InOutParameter,
    Length,
    Less,
    Mod,
    Mul,
    NamedAggregate,
    Number,
    ObjectDeclaration,
    Old,
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
    Subprogram,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
    Variable,
)
from rflx.common import unique
from rflx.const import BUILTINS_PACKAGE
from rflx.model import FINAL, INITIAL, Composite, Field, Message, Scalar, Type

from . import common, const


class ParserGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix

    def extract_function(self, type_identifier: ID) -> Subprogram:
        return GenericFunctionInstantiation(
            "Extract",
            FunctionSpecification(
                const.TYPES * "Extract",
                type_identifier,
                [
                    Parameter(["Buffer"], const.TYPES_BYTES),
                    Parameter(["Offset"], const.TYPES_OFFSET),
                ],
            ),
            [common.prefixed_type_identifier(type_identifier, self.prefix)],
        )

    def create_internal_functions(
        self,
        message: Message,
        scalar_fields: Mapping[Field, Type],
        composite_fields: Sequence[Field],
    ) -> UnitPart:
        def result(field: Field, message: Message) -> NamedAggregate:
            aggregate: List[Tuple[str, Expr]] = [("Fld", Variable(field.affixed_name))]
            if isinstance(message.types[field], Scalar):
                aggregate.append(
                    (
                        f"{field.name}_Value",
                        Call(
                            "Extract",
                            [
                                Slice(
                                    Variable("Ctx.Buffer.all"),
                                    Variable("Buffer_First"),
                                    Variable("Buffer_Last"),
                                ),
                                Variable("Offset"),
                            ],
                        ),
                    )
                )
            elif isinstance(message.types[field], Composite) and common.is_compared_to_aggregate(
                field, message
            ):
                aggregate.append(
                    (
                        f"{field.name}_Value",
                        Slice(
                            Variable("Ctx.Buffer.all"),
                            Variable("Buffer_First"),
                            Variable("Buffer_Last"),
                        ),
                    )
                )
            return NamedAggregate(*aggregate)

        comparison_to_aggregate = any(
            (isinstance(t, Composite) and common.is_compared_to_aggregate(f, message))
            for f, t in message.types.items()
        )

        return UnitPart(
            [],
            [
                *(
                    [
                        ExpressionFunctionDeclaration(
                            FunctionSpecification(
                                "Composite_Field", "Boolean", [Parameter(["Fld"], "Field")]
                            ),
                            Case(
                                Variable("Fld"),
                                [
                                    (
                                        Variable(f.affixed_name),
                                        TRUE if f in composite_fields else FALSE,
                                    )
                                    for f in message.fields
                                ],
                            ),
                        )
                    ]
                    if scalar_fields and composite_fields
                    else []
                ),
                SubprogramBody(
                    FunctionSpecification(
                        "Get_Field_Value",
                        "Field_Dependent_Value",
                        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
                    ),
                    [
                        *common.field_bit_location_declarations(Variable("Fld")),
                        *(
                            common.field_byte_location_declarations()
                            if scalar_fields
                            else common.field_byte_bounds_declarations()
                        ),
                        *unique(
                            self.extract_function(common.full_base_type_name(t))
                            for f, t in message.types.items()
                            if isinstance(t, Scalar)
                        ),
                    ]
                    if scalar_fields or comparison_to_aggregate
                    else [],
                    [
                        ReturnStatement(
                            Case(
                                Variable("Fld"),
                                [
                                    (Variable(f.affixed_name), result(f, message))
                                    for f in message.fields
                                ],
                            )
                        )
                    ],
                    [
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                                Call(
                                    "Sufficient_Buffer_Length", [Variable("Ctx"), Variable("Fld")]
                                ),
                            )
                        ),
                        Postcondition(
                            Equal(
                                Selected(Result("Get_Field_Value"), "Fld"),
                                Variable("Fld"),
                            )
                        ),
                    ],
                ),
            ],
        )

    @staticmethod
    def create_verify_procedure(
        message: Message,
        scalar_fields: Mapping[Field, Scalar],
        composite_fields: Sequence[Field],
    ) -> UnitPart:
        specification = ProcedureSpecification(
            "Verify", [InOutParameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")]
        )

        valid_field_condition = And(
            Call(
                "Valid_Value",
                [Variable("Value")],
            ),
            Call(
                "Field_Condition",
                [
                    Variable("Ctx"),
                    Variable("Value"),
                    *(
                        [Call("Field_Size", [Variable("Ctx"), Variable("Fld")])]
                        if common.size_dependent_condition(message)
                        else []
                    ),
                ],
            ),
        )

        set_message_last = Assignment(
            Variable("Ctx.Message_Last"),
            Mul(
                Div(
                    Add(
                        Call("Field_Last", [Variable("Ctx"), Variable("Fld")]),
                        Number(7),
                    ),
                    Number(8),
                ),
                Number(8),
            ),
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
                                            ]
                                        ),
                                        Equal(
                                            Mod(
                                                Call(
                                                    "Field_Last", [Variable("Ctx"), Variable("Fld")]
                                                ),
                                                Size(const.TYPES_BYTE),
                                            ),
                                            Number(0),
                                        ),
                                    )
                                ]
                            )
                        ],
                    )
                ]
                if len(message.fields) > 1
                else []
            ),
            # Componolit/RecordFlux#664:
            # The provability of the context predicate is increased by duplicating the statement
            # inside a case statement.
            CaseStatement(
                Variable("Fld"),
                [
                    (
                        Variable(f.affixed_name),
                        [set_message_last],
                    )
                    for f in message.fields
                ],
                case_grouping=False,
            )
            if len(message.fields) > 1
            else set_message_last,
            IfStatement(
                [
                    (
                        Call("Composite_Field", [Variable("Fld")]),
                        [set_context_cursor_composite_field("Fld")],
                    )
                ],
                [set_context_cursor_scalar()],
            )
            if scalar_fields and composite_fields
            else set_context_cursor_scalar()
            if scalar_fields and not composite_fields
            else set_context_cursor_composite_field("Fld"),
            *(
                [
                    # ISSUE: Componolit/RecordFlux#664
                    # The provability of the context predicate is increased by splitting the
                    # assignment into multiple statements.
                    IfStatement(
                        [
                            (
                                Equal(Variable("Fld"), Variable(f.affixed_name)),
                                [
                                    Assignment(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Call("Successor", [Variable("Ctx"), Variable("Fld")]),
                                        ),
                                        NamedAggregate(
                                            ("State", Variable("S_Invalid")),
                                            ("Predecessor", Variable("Fld")),
                                        ),
                                    )
                                ],
                            )
                            for f in message.fields
                        ]
                    )
                ]
                if len(message.fields) > 1
                else []
            ),
        ]

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Postcondition(
                            And(
                                Equal(
                                    Call("Has_Buffer", [Variable("Ctx")]),
                                    Old(Call("Has_Buffer", [Variable("Ctx")])),
                                ),
                                *const.CONTEXT_INVARIANT,
                            )
                        ),
                    ],
                )
            ],
            [
                SubprogramBody(
                    specification,
                    [ObjectDeclaration(["Value"], "Field_Dependent_Value")],
                    [
                        IfStatement(
                            [
                                (
                                    AndThen(
                                        Call("Has_Buffer", [Variable("Ctx")]),
                                        Call(
                                            "Invalid",
                                            [Indexed(Variable("Ctx.Cursors"), Variable("Fld"))],
                                        ),
                                        Call(
                                            "Valid_Predecessor", [Variable("Ctx"), Variable("Fld")]
                                        ),
                                        Call("Path_Condition", [Variable("Ctx"), Variable("Fld")]),
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
                                                            Call(
                                                                "Get_Field_Value",
                                                                [Variable("Ctx"), Variable("Fld")],
                                                            ),
                                                        ),
                                                        IfStatement(
                                                            [
                                                                (
                                                                    valid_field_condition,
                                                                    set_cursors_statements,
                                                                )
                                                            ],
                                                            [
                                                                Assignment(
                                                                    Indexed(
                                                                        Variable("Ctx.Cursors"),
                                                                        Variable("Fld"),
                                                                    ),
                                                                    NamedAggregate(
                                                                        (
                                                                            "State",
                                                                            Variable("S_Invalid"),
                                                                        ),
                                                                        (
                                                                            "Predecessor",
                                                                            Variable(
                                                                                FINAL.affixed_name,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                )
                                                            ],
                                                        ),
                                                    ],
                                                )
                                            ],
                                            [
                                                Assignment(
                                                    Indexed(
                                                        Variable("Ctx.Cursors"), Variable("Fld")
                                                    ),
                                                    NamedAggregate(
                                                        ("State", Variable("S_Incomplete")),
                                                        (
                                                            "Predecessor",
                                                            Variable(FINAL.affixed_name),
                                                        ),
                                                    ),
                                                )
                                            ],
                                        )
                                    ],
                                )
                            ],
                        )
                    ],
                )
            ],
        )

    @staticmethod
    def create_verify_message_procedure(message: Message) -> UnitPart:
        specification = ProcedureSpecification(
            "Verify_Message", [InOutParameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [
                        Postcondition(
                            And(
                                Equal(
                                    Call("Has_Buffer", [Variable("Ctx")]),
                                    Old(Call("Has_Buffer", [Variable("Ctx")])),
                                ),
                                *const.CONTEXT_INVARIANT,
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
                        CallStatement("Verify", [Variable("Ctx"), Variable(f.affixed_name)])
                        for f in message.fields
                    ],
                )
            ],
        )

    @staticmethod
    def create_present_function() -> UnitPart:
        specification = FunctionSpecification(
            "Present", "Boolean", [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        Call(
                            "Structural_Valid", [Indexed(Variable("Ctx.Cursors"), Variable("Fld"))]
                        ),
                        Less(
                            Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "First"),
                            Add(
                                Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Last"),
                                Number(1),
                            ),
                        ),
                    ),
                )
            ],
        )

    @staticmethod
    def create_structural_valid_function() -> UnitPart:
        specification = FunctionSpecification(
            "Structural_Valid",
            "Boolean",
            [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    And(
                        Or(
                            *[
                                Equal(
                                    Selected(
                                        Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "State"
                                    ),
                                    Variable(s),
                                )
                                for s in ("S_Valid", "S_Structural_Valid")
                            ]
                        )
                    ),
                )
            ],
        )

    @staticmethod
    def create_valid_function() -> UnitPart:
        specification = FunctionSpecification(
            "Valid", "Boolean", [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")]
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
                                                "Structural_Valid",
                                                [Variable("Ctx"), Variable("Fld")],
                                            ),
                                            Call("Present", [Variable("Ctx"), Variable("Fld")]),
                                        ),
                                    )
                                ]
                            )
                        ),
                    ],
                )
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
                )
            ],
        )

    @staticmethod
    def create_incomplete_function() -> UnitPart:
        specification = FunctionSpecification(
            "Incomplete", "Boolean", [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")]
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
                )
            ],
        )

    @staticmethod
    def create_invalid_function() -> UnitPart:
        specification = FunctionSpecification(
            "Invalid", "Boolean", [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")]
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
                )
            ],
        )

    def create_structural_valid_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Structural_Valid_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [Precondition(Call("Has_Buffer", [Variable("Ctx")]))],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    self.valid_message_condition(message, structural=True),
                )
            ],
        )

    def create_valid_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification,
                    [Precondition(Call("Has_Buffer", [Variable("Ctx")]))],
                )
            ],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    self.valid_message_condition(message),
                )
            ],
        )

    @staticmethod
    def create_incomplete_message_function(message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Incomplete_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification)],
            private=[
                ExpressionFunctionDeclaration(
                    specification,
                    Or(
                        *[
                            Call(
                                "Incomplete",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            )
                            for f in message.fields
                        ]
                    ),
                )
            ],
        )

    def create_scalar_getter_functions(self, scalar_fields: Mapping[Field, Scalar]) -> UnitPart:
        def specification(field: Field, field_type: Type) -> FunctionSpecification:
            if field_type.package == BUILTINS_PACKAGE:
                type_identifier = ID(field_type.name)
            else:
                type_identifier = self.prefix * ID(field_type.identifier)

            return FunctionSpecification(
                f"Get_{field.name}", type_identifier, [Parameter(["Ctx"], "Context")]
            )

        def result(field: Field) -> Expr:
            return Call(
                "To_Actual",
                [
                    Selected(
                        Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)),
                        f"Value.{field.name}_Value",
                    )
                ],
            )

        return UnitPart(
            [
                # WORKAROUND: Componolit/Workarounds#31
                Pragma("Warnings", [Variable("Off"), String("precondition is always False")]),
                *[
                    SubprogramDeclaration(
                        specification(f, t),
                        [
                            Precondition(
                                Call("Valid", [Variable("Ctx"), Variable(f.affixed_name)]),
                            )
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

    @staticmethod
    def create_opaque_getter_functions(opaque_fields: Sequence[Field]) -> UnitPart:
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
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call(
                                    "Structural_Valid",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                Call(
                                    "Valid_Next",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                            )
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
                                        )
                                    ],
                                ),
                            )
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
                                    )
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
                                    )
                                ],
                            ),
                            constant=True,
                        ),
                    ],
                    [
                        ReturnStatement(
                            Slice(Variable("Ctx.Buffer.all"), Variable("First"), Variable("Last")),
                        )
                    ],
                )
                for f in opaque_fields
            ],
        )

    @staticmethod
    def create_opaque_getter_procedures(opaque_fields: Sequence[Field]) -> UnitPart:
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
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call(
                                    "Structural_Valid",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                Call(
                                    "Valid_Next",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                GreaterEqual(
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
                            )
                        )
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
                                    )
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
                                    )
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

    @staticmethod
    def create_generic_opaque_getter_procedures(opaque_fields: Sequence[Field]) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Generic_Get_{field.name}", [Parameter(["Ctx"], "Context")]
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call(
                                    "Present",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                            )
                        )
                    ],
                    [
                        FormalSubprogramDeclaration(
                            ProcedureSpecification(
                                f"Process_{f.name}", [Parameter([f.name], const.TYPES_BYTES)]
                            )
                        )
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
                                    )
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
                                    )
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
                                    Variable("Ctx.Buffer.all"), Variable("First"), Variable("Last")
                                )
                            ],
                        )
                    ],
                )
                for f in opaque_fields
            ],
        )

    def valid_message_condition(
        self, message: Message, field: Field = INITIAL, structural: bool = False
    ) -> Expr:
        def condition(message: Message, field: Field, structural: bool) -> expr.Expr:
            return expr.Or(
                *[
                    l.condition
                    if l.target == FINAL
                    else expr.AndThen(
                        expr.Call(
                            "Structural_Valid"
                            if structural and isinstance(message.types[l.target], Composite)
                            else "Valid",
                            [
                                expr.Variable("Ctx"),
                                expr.Variable(l.target.affixed_name, immutable=True),
                            ],
                        ),
                        l.condition,
                        condition(message, l.target, structural),
                    )
                    for l in message.outgoing(field)
                ]
            )

        return (
            condition(message, field, structural)
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
            (
                "Predecessor",
                Selected(Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"),
            ),
        ),
    )


def set_context_cursor_composite_field(field_name: str) -> Assignment:
    return Assignment(
        Indexed(
            Variable("Ctx.Cursors"),
            Variable(field_name),
        ),
        NamedAggregate(
            ("State", Variable("S_Structural_Valid")),
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
            (
                "Predecessor",
                Selected(
                    Indexed(
                        Variable("Ctx.Cursors"),
                        Variable(field_name),
                    ),
                    "Predecessor",
                ),
            ),
        ),
    )
