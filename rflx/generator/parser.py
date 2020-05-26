from typing import List, Mapping, Sequence, Tuple

from rflx.ada import (
    Assignment,
    CallStatement,
    Case,
    ExpressionFunctionDeclaration,
    FormalSubprogramDeclaration,
    FunctionSpecification,
    GenericFunctionInstantiation,
    IfStatement,
    InOutParameter,
    ObjectDeclaration,
    Parameter,
    Postcondition,
    PragmaStatement,
    Precondition,
    ProcedureSpecification,
    ReturnStatement,
    Subprogram,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
)
from rflx.common import unique
from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    AndThen,
    Call,
    Equal,
    Expr,
    If,
    Indexed,
    Less,
    NamedAggregate,
    Number,
    Old,
    Or,
    Result,
    Selected,
    Slice,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    BUILTINS_PACKAGE,
    FINAL,
    INITIAL,
    Composite,
    Enumeration,
    Field,
    Message,
    Scalar,
    Type,
)

from . import common, const


class ParserGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix

    def extract_function(self, type_name: ID) -> Subprogram:
        return GenericFunctionInstantiation(
            "Extract",
            FunctionSpecification(
                const.TYPES * "Extract",
                type_name,
                [
                    Parameter(["Buffer"], const.TYPES_BYTES),
                    Parameter(["Offset"], const.TYPES_OFFSET),
                ],
            ),
            [common.prefixed_type_name(type_name, self.prefix)],
        )

    def create_internal_functions(
        self,
        message: Message,
        scalar_fields: Mapping[Field, Type],
        composite_fields: Sequence[Field],
    ) -> UnitPart:
        def result(field: Field, message: Message) -> NamedAggregate:
            aggregate: List[Tuple[str, Expr]] = [("Fld", Variable(field.affixed_name))]
            if field in message.fields and isinstance(message.types[field], Scalar):
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
            return NamedAggregate(*aggregate)

        return UnitPart(
            [],
            [
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Composite_Field", "Boolean", [Parameter(["Fld"], "Field")]
                    ),
                    Case(
                        Variable("Fld"),
                        [
                            (Variable(f.affixed_name), TRUE if f in composite_fields else FALSE)
                            for f in message.fields
                        ],
                    ),
                ),
                SubprogramBody(
                    FunctionSpecification(
                        "Get_Field_Value",
                        "Field_Dependent_Value",
                        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
                    ),
                    [
                        *common.field_bit_location_declarations(Variable("Fld")),
                        *common.field_byte_location_declarations(),
                        *unique(
                            self.extract_function(common.full_base_type_name(t))
                            for t in message.types.values()
                            if isinstance(t, Scalar)
                        ),
                    ]
                    if scalar_fields
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
                            Equal(Selected(Result("Get_Field_Value"), "Fld"), Variable("Fld"),)
                        ),
                    ],
                ),
            ],
        )

    def create_verify_procedure(
        self, message: Message, context_invariant: Sequence[Expr]
    ) -> UnitPart:
        specification = ProcedureSpecification(
            "Verify", [InOutParameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")]
        )

        valid_field_condition = And(
            Call("Valid_Value", [Variable("Value")],),
            Call(
                "Field_Condition",
                [
                    Variable("Ctx"),
                    Variable("Value"),
                    *(
                        [Call("Field_Length", [Variable("Ctx"), Variable("Fld")])]
                        if common.length_dependent_condition(message)
                        else []
                    ),
                ],
            ),
        )

        set_cursors_statements = [
            IfStatement(
                [
                    (
                        Call("Composite_Field", [Variable("Fld")]),
                        [
                            Assignment(
                                Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                                NamedAggregate(
                                    ("State", Variable("S_Structural_Valid")),
                                    (
                                        "First",
                                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                                    ),
                                    (
                                        "Last",
                                        Call("Field_Last", [Variable("Ctx"), Variable("Fld")]),
                                    ),
                                    ("Value", Variable("Value")),
                                    (
                                        "Predecessor",
                                        Selected(
                                            Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                                            "Predecessor",
                                        ),
                                    ),
                                ),
                            )
                        ],
                    )
                ],
                [
                    Assignment(
                        Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
                        NamedAggregate(
                            ("State", Variable("S_Valid")),
                            ("First", Call("Field_First", [Variable("Ctx"), Variable("Fld")])),
                            ("Last", Call("Field_Last", [Variable("Ctx"), Variable("Fld")])),
                            ("Value", Variable("Value")),
                            (
                                "Predecessor",
                                Selected(
                                    Indexed(Variable("Ctx.Cursors"), Variable("Fld")), "Predecessor"
                                ),
                            ),
                        ),
                    )
                ],
            ),
            # WORKAROUND:
            # Limitation of GNAT Community 2019 / SPARK Pro 20.0
            # Provability of predicate is increased by adding part of
            # predicate as assert
            PragmaStatement(
                "Assert", [str(common.message_structure_invariant(message, self.prefix))]
            ),
            # WORKAROUND:
            # Limitation of GNAT Community 2019 / SPARK Pro 20.0
            # Provability of predicate is increased by splitting
            # assignment in multiple statements
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
                                *context_invariant,
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
                                                                                FINAL.affixed_name
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
    def create_verify_message_procedure(
        message: Message, context_invariant: Sequence[Expr]
    ) -> UnitPart:
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
                                *context_invariant,
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
            [
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
            [
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
            [
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
            [
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
            [
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

    @staticmethod
    def create_structural_valid_message_function(message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Structural_Valid_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification, [Precondition(Call("Has_Buffer", [Variable("Ctx")]))],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    valid_message_condition(message, structural=True)
                    .substituted(common.substitution(message))
                    .simplified(),
                )
            ],
        )

    @staticmethod
    def create_valid_message_function(message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification, [Precondition(Call("Has_Buffer", [Variable("Ctx")]))],
                )
            ],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    valid_message_condition(message)
                    .substituted(common.substitution(message))
                    .simplified(),
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
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Or(
                        *[
                            Call("Incomplete", [Variable("Ctx"), Variable(f.affixed_name)])
                            for f in message.fields
                        ]
                    ),
                )
            ],
        )

    def create_scalar_accessor_functions(self, scalar_fields: Mapping[Field, Scalar]) -> UnitPart:
        def specification(field: Field, field_type: Type) -> FunctionSpecification:
            if field_type.package == BUILTINS_PACKAGE:
                type_name = ID(field_type.name)
            else:
                type_name = self.prefix * field_type.identifier

            return FunctionSpecification(
                f"Get_{field.name}", type_name, [Parameter(["Ctx"], "Context")]
            )

        def result(field: Field, field_type: Type) -> Expr:
            value = Selected(
                Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)),
                f"Value.{field.name}_Value",
            )
            if isinstance(field_type, Enumeration):
                return Call("To_Actual", [value])
            return value

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f, t),
                    [Precondition(Call("Valid", [Variable("Ctx"), Variable(f.affixed_name)]),)],
                )
                for f, t in scalar_fields.items()
            ],
            [
                ExpressionFunctionDeclaration(specification(f, t), result(f, t))
                for f, t in scalar_fields.items()
            ],
        )

    @staticmethod
    def create_composite_accessor_procedures(composite_fields: Sequence[Field]) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(f"Get_{field.name}", [Parameter(["Ctx"], "Context")])

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            And(
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Present", [Variable("Ctx"), Variable(f.affixed_name)]),
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
                for f in composite_fields
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_BYTE_INDEX,
                                [
                                    Selected(
                                        Indexed(Variable("Ctx.Cursors"), Variable(f.affixed_name)),
                                        "First",
                                    )
                                ],
                            ),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            const.TYPES_INDEX,
                            Call(
                                const.TYPES_BYTE_INDEX,
                                [
                                    Selected(
                                        Indexed(Variable("Ctx.Cursors"), Variable(f.affixed_name)),
                                        "Last",
                                    )
                                ],
                            ),
                            True,
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
                for f in composite_fields
            ],
        )


def valid_message_condition(
    message: Message, field: Field = INITIAL, structural: bool = False
) -> Expr:
    if not message.outgoing(field):
        return TRUE
    return Or(
        *[
            l.condition
            if l.target == FINAL
            else AndThen(
                Call(
                    "Structural_Valid"
                    if structural and isinstance(message.types[l.target], Composite)
                    else "Valid",
                    [Variable("Ctx"), Variable(l.target.affixed_name)],
                ),
                l.condition,
                valid_message_condition(message, l.target, structural),
            )
            for l in message.outgoing(field)
        ]
    )
