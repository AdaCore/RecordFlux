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
    Div,
    Equal,
    Expr,
    GreaterEqual,
    If,
    Indexed,
    Last,
    Less,
    LessEqual,
    NamedAggregate,
    NotEqual,
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

from .common import (
    NULL,
    VALID_CONTEXT,
    GeneratorCommon,
    full_base_type_name,
    length_dependent_condition,
)
from .types import Types


class ParserGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix
        self.types = Types(prefix)
        self.common = GeneratorCommon(prefix)

    def extract_function(self, type_name: ID) -> Subprogram:
        return GenericFunctionInstantiation(
            "Extract",
            FunctionSpecification(
                f"{self.types.types}.Extract",
                type_name,
                [Parameter(["Buffer"], self.types.bytes), Parameter(["Offset"], self.types.offset)],
            ),
            [self.types.prefixed(type_name)],
        )

    def create_internal_functions(
        self, message: Message, composite_fields: Sequence[Field]
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
                        "Sufficient_Buffer_Length",
                        "Boolean",
                        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
                    ),
                    And(
                        NotEqual(Variable("Ctx.Buffer"), NULL),
                        LessEqual(
                            Variable("Ctx.First"), Div(Last(self.types.bit_index), Number(2)),
                        ),
                        LessEqual(
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                            Div(Last(self.types.bit_index), Number(2)),
                        ),
                        GreaterEqual(
                            Call("Field_Length", [Variable("Ctx"), Variable("Fld")]), Number(0)
                        ),
                        LessEqual(
                            Call("Field_Length", [Variable("Ctx"), Variable("Fld")]),
                            Div(Last(self.types.bit_length), Number(2)),
                        ),
                        LessEqual(
                            Add(
                                Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                                Call("Field_Length", [Variable("Ctx"), Variable("Fld")]),
                            ),
                            Div(Last(self.types.bit_length), Number(2)),
                        ),
                        LessEqual(
                            Variable("Ctx.First"),
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                        ),
                        GreaterEqual(
                            Variable("Ctx.Last"),
                            Call("Field_Last", [Variable("Ctx"), Variable("Fld")]),
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
                        *self.common.field_bit_location_declarations(Variable("Fld")),
                        *self.common.field_byte_location_declarations(),
                        *unique(
                            self.extract_function(full_base_type_name(t))
                            for t in message.types.values()
                            if isinstance(t, Scalar)
                        ),
                    ],
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
                        if length_dependent_condition(message)
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
                "Assert", [str(self.common.message_structure_invariant(message, prefix=True))]
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
                        Precondition(VALID_CONTEXT),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
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
                        Precondition(VALID_CONTEXT),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
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
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
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
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
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
                        Precondition(VALID_CONTEXT),
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
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
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
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
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

    def create_structural_valid_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Structural_Valid_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    valid_message_condition(message, structural=True).simplified(
                        self.common.substitution(message)
                    ),
                )
            ],
        )

    def create_valid_message_function(self, message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Valid_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
            [
                ExpressionFunctionDeclaration(
                    specification,
                    valid_message_condition(message).simplified(self.common.substitution(message)),
                )
            ],
        )

    @staticmethod
    def create_incomplete_message_function(message: Message) -> UnitPart:
        specification = FunctionSpecification(
            "Incomplete_Message", "Boolean", [Parameter(["Ctx"], "Context")]
        )

        return UnitPart(
            [SubprogramDeclaration(specification, [Precondition(VALID_CONTEXT)])],
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
                    [
                        Precondition(
                            And(
                                VALID_CONTEXT,
                                Call("Valid", [Variable("Ctx"), Variable(f.affixed_name)]),
                            )
                        )
                    ],
                )
                for f, t in scalar_fields.items()
            ],
            [
                ExpressionFunctionDeclaration(specification(f, t), result(f, t))
                for f, t in scalar_fields.items()
            ],
        )

    def create_composite_accessor_procedures(self, composite_fields: Sequence[Field]) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(f"Get_{field.name}", [Parameter(["Ctx"], "Context")])

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            And(
                                VALID_CONTEXT,
                                Call("Has_Buffer", [Variable("Ctx")]),
                                Call("Present", [Variable("Ctx"), Variable(f.affixed_name)]),
                            )
                        )
                    ],
                    [
                        FormalSubprogramDeclaration(
                            ProcedureSpecification(
                                f"Process_{f.name}", [Parameter([f.name], self.types.bytes)]
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
                            self.types.index,
                            Call(
                                self.types.byte_index,
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
                            self.types.index,
                            Call(
                                self.types.byte_index,
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
