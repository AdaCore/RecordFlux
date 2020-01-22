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
    Name,
    NamedAggregate,
    NotEqual,
    Number,
    Old,
    Or,
    Result,
    Selected,
    Slice,
)
from rflx.model import FINAL, INITIAL, Composite, Enumeration, Field, Message, Scalar, Type

from .common import NULL, VALID_CONTEXT, GeneratorCommon, length_dependent_condition
from .types import Types


class ParserGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix
        self.types = Types(prefix)
        self.common = GeneratorCommon(prefix)

    def extract_function(self, type_name: str) -> Subprogram:
        return GenericFunctionInstantiation(
            "Extract",
            FunctionSpecification(
                f"{self.types.types}.Extract",
                type_name,
                [Parameter(["Buffer"], self.types.bytes), Parameter(["Offset"], self.types.offset)],
            ),
            [self.types.index, self.types.byte, self.types.bytes, self.types.offset, type_name],
        )

    def create_internal_functions(
        self, message: Message, composite_fields: Sequence[Field]
    ) -> UnitPart:
        def result(field: Field, message: Message) -> NamedAggregate:
            aggregate: List[Tuple[str, Expr]] = [("Fld", Name(field.affixed_name))]
            if field in message.fields and isinstance(message.types[field], Scalar):
                aggregate.append(
                    (
                        f"{field.name}_Value",
                        Call(
                            "Extract",
                            [
                                Slice("Ctx.Buffer.all", Name("Buffer_First"), Name("Buffer_Last")),
                                Name("Offset"),
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
                        NotEqual(Name("Ctx.Buffer"), NULL),
                        LessEqual(Name("Ctx.First"), Div(Last(self.types.bit_index), Number(2))),
                        LessEqual(
                            Call("Field_First", [Name("Ctx"), Name("Fld")]),
                            Div(Last(self.types.bit_index), Number(2)),
                        ),
                        GreaterEqual(Call("Field_Length", [Name("Ctx"), Name("Fld")]), Number(0)),
                        LessEqual(
                            Call("Field_Length", [Name("Ctx"), Name("Fld")]),
                            Div(Last(self.types.bit_length), Number(2)),
                        ),
                        LessEqual(
                            Add(
                                Call("Field_First", [Name("Ctx"), Name("Fld")]),
                                Call("Field_Length", [Name("Ctx"), Name("Fld")]),
                            ),
                            Div(Last(self.types.bit_length), Number(2)),
                        ),
                        LessEqual(
                            Name("Ctx.First"), Call("Field_First", [Name("Ctx"), Name("Fld")])
                        ),
                        GreaterEqual(
                            Name("Ctx.Last"), Call("Field_Last", [Name("Ctx"), Name("Fld")])
                        ),
                    ),
                    [
                        Precondition(
                            And(
                                Call("Has_Buffer", [Name("Ctx")]),
                                Call("Valid_Next", [Name("Ctx"), Name("Fld")]),
                            )
                        )
                    ],
                ),
                ExpressionFunctionDeclaration(
                    FunctionSpecification(
                        "Composite_Field", "Boolean", [Parameter(["Fld"], "Field")]
                    ),
                    Case(
                        Name("Fld"),
                        [
                            (Name(f.affixed_name), TRUE if f in composite_fields else FALSE)
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
                        *self.common.field_bit_location_declarations(Name("Fld")),
                        *self.common.field_byte_location_declarations(),
                    ],
                    [
                        ReturnStatement(
                            Case(
                                Name("Fld"),
                                [
                                    (Name(f.affixed_name), result(f, message))
                                    for f in message.fields
                                ],
                            )
                        )
                    ],
                    [
                        Precondition(
                            AndThen(
                                Call("Has_Buffer", [Name("Ctx")]),
                                Call("Valid_Next", [Name("Ctx"), Name("Fld")]),
                                Call("Sufficient_Buffer_Length", [Name("Ctx"), Name("Fld")]),
                            )
                        ),
                        Postcondition(
                            Equal(Selected(Result("Get_Field_Value"), "Fld"), Name("Fld"))
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
            Call("Valid_Value", [Name("Value")],),
            Call(
                "Field_Condition",
                [Name("Ctx"), Name("Value")]
                + (
                    [Call("Field_Length", [Name("Ctx"), Name("Fld")])]
                    if length_dependent_condition(message)
                    else []
                ),
            ),
        )

        set_cursors_statements = [
            IfStatement(
                [
                    (
                        Call("Composite_Field", [Name("Fld")]),
                        [
                            Assignment(
                                Indexed("Ctx.Cursors", Name("Fld")),
                                NamedAggregate(
                                    ("State", Name("S_Structural_Valid")),
                                    ("First", Call("Field_First", [Name("Ctx"), Name("Fld")])),
                                    ("Last", Call("Field_Last", [Name("Ctx"), Name("Fld")])),
                                    ("Value", Name("Value")),
                                    (
                                        "Predecessor",
                                        Selected(
                                            Indexed(Selected("Ctx", "Cursors"), Name("Fld")),
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
                        Indexed("Ctx.Cursors", Name("Fld")),
                        NamedAggregate(
                            ("State", Name("S_Valid")),
                            ("First", Call("Field_First", [Name("Ctx"), Name("Fld")])),
                            ("Last", Call("Field_Last", [Name("Ctx"), Name("Fld")])),
                            ("Value", Name("Value")),
                            (
                                "Predecessor",
                                Selected(
                                    Indexed(Selected("Ctx", "Cursors"), Name("Fld")), "Predecessor"
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
                        Equal(Name("Fld"), Name(f.affixed_name)),
                        [
                            Assignment(
                                Indexed(
                                    "Ctx.Cursors", Call("Successor", [Name("Ctx"), Name("Fld")])
                                ),
                                NamedAggregate(
                                    ("State", Name("S_Invalid")), ("Predecessor", Name("Fld"))
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
                                    Call("Has_Buffer", [Name("Ctx")]),
                                    Old(Call("Has_Buffer", [Name("Ctx")])),
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
                                        Call("Has_Buffer", [Name("Ctx")]),
                                        Call(
                                            "Invalid",
                                            [Indexed(Selected("Ctx", "Cursors"), Name("Fld"))],
                                        ),
                                        Call("Valid_Predecessor", [Name("Ctx"), Name("Fld")]),
                                        Call("Path_Condition", [Name("Ctx"), Name("Fld")]),
                                    ),
                                    [
                                        IfStatement(
                                            [
                                                (
                                                    Call(
                                                        "Sufficient_Buffer_Length",
                                                        [Name("Ctx"), Name("Fld")],
                                                    ),
                                                    [
                                                        Assignment(
                                                            "Value",
                                                            Call(
                                                                "Get_Field_Value",
                                                                [Name("Ctx"), Name("Fld")],
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
                                                                        "Ctx.Cursors", Name("Fld")
                                                                    ),
                                                                    NamedAggregate(
                                                                        (
                                                                            "State",
                                                                            Name("S_Invalid"),
                                                                        ),
                                                                        (
                                                                            "Predecessor",
                                                                            Name(
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
                                                    Indexed("Ctx.Cursors", Name("Fld")),
                                                    NamedAggregate(
                                                        ("State", Name("S_Incomplete")),
                                                        ("Predecessor", Name(FINAL.affixed_name)),
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
                                    Call("Has_Buffer", [Name("Ctx")]),
                                    Old(Call("Has_Buffer", [Name("Ctx")])),
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
                        CallStatement("Verify", [Name("Ctx"), Name(f.affixed_name)])
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
                        Call("Structural_Valid", [Indexed("Ctx.Cursors", Name("Fld"))]),
                        Less(
                            Selected(Indexed("Ctx.Cursors", Name("Fld")), "First"),
                            Add(Selected(Indexed("Ctx.Cursors", Name("Fld")), "Last"), Number(1)),
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
                                    Selected(Indexed("Ctx.Cursors", Name("Fld")), "State"), Name(s)
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
                                            Call("Structural_Valid", [Name("Ctx"), Name("Fld")]),
                                            Call("Present", [Name("Ctx"), Name("Fld")]),
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
                            Selected(Indexed("Ctx.Cursors", Name("Fld")), "State"), Name("S_Valid")
                        ),
                        Less(
                            Selected(Indexed("Ctx.Cursors", Name("Fld")), "First"),
                            Add(Selected(Indexed("Ctx.Cursors", Name("Fld")), "Last"), Number(1)),
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
                        Selected(Indexed("Ctx.Cursors", Name("Fld")), "State"), Name("S_Incomplete")
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
                            Selected(Indexed("Ctx.Cursors", Name("Fld")), "State"),
                            Name("S_Invalid"),
                        ),
                        Equal(
                            Selected(Indexed("Ctx.Cursors", Name("Fld")), "State"),
                            Name("S_Incomplete"),
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
                            Call("Incomplete", [Name("Ctx"), Name(f.affixed_name)])
                            for f in message.fields
                        ]
                    ),
                )
            ],
        )

    @staticmethod
    def create_scalar_accessor_functions(scalar_fields: Mapping[Field, Scalar]) -> UnitPart:
        def specification(field: Field, field_type: Type) -> FunctionSpecification:
            return FunctionSpecification(
                f"Get_{field.name}", field_type.full_name, [Parameter(["Ctx"], "Context")]
            )

        def result(field: Field, field_type: Type) -> Expr:
            value = Selected(
                Indexed("Ctx.Cursors", Name(field.affixed_name)), f"Value.{field.name}_Value"
            )
            if isinstance(field_type, Enumeration):
                return Call("Convert", [value])
            return value

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f, t),
                    [
                        Precondition(
                            And(VALID_CONTEXT, Call("Valid", [Name("Ctx"), Name(f.affixed_name)]))
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
                                Call("Has_Buffer", [Name("Ctx")]),
                                Call("Present", [Name("Ctx"), Name(f.affixed_name)]),
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
                                [Selected(Indexed("Ctx.Cursors", Name(f.affixed_name)), "First")],
                            ),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            self.types.index,
                            Call(
                                self.types.byte_index,
                                [Selected(Indexed("Ctx.Cursors", Name(f.affixed_name)), "Last")],
                            ),
                            True,
                        ),
                    ],
                    [
                        CallStatement(
                            f"Process_{f.name}",
                            [Slice("Ctx.Buffer.all", Name("First"), Name("Last"))],
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
                    [Name("Ctx"), Name(l.target.affixed_name)],
                ),
                l.condition,
                valid_message_condition(message, l.target, structural),
            )
            for l in message.outgoing(field)
        ]
    )
