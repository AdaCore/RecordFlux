from __future__ import annotations

from collections import abc
from typing import Union

from rflx import expression as expr
from rflx.ada import (
    FALSE,
    NULL,
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
    Annotate,
    ArrayType,
    Assignment,
    Call,
    CallStatement,
    Case,
    ChoiceList,
    Component,
    Constrained,
    ContractCases,
    Decreases,
    DefaultInitialCondition,
    Depends,
    Discriminant,
    DynamicPredicate,
    EnumerationType,
    Equal,
    Expr,
    ExpressionFunctionDeclaration,
    First,
    ForAllIn,
    ForIn,
    FormalSubprogramDeclaration,
    FunctionSpecification,
    Ghost,
    Greater,
    GreaterEqual,
    If,
    IfStatement,
    In,
    Indexed,
    InOutParameter,
    Last,
    Length,
    Less,
    LessEqual,
    LoopEntry,
    Max,
    Mod,
    NamedAggregate,
    Not,
    NotEqual,
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
    PrivateType,
    ProcedureSpecification,
    RangeSubtype,
    RecordType,
    Rem,
    Result,
    Selected,
    Size,
    Slice,
    Statement,
    String,
    Sub,
    SubprogramBody,
    SubprogramDeclaration,
    SubprogramVariant,
    UnitPart,
    UseTypeClause,
    ValueRange,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    FINAL,
    INITIAL,
    Composite,
    Enumeration,
    Field,
    Link,
    Message,
    Opaque,
    Scalar,
    Sequence,
    Type,
    is_builtin_type,
)

from . import common, const


def create_use_type_clause(composite_fields: abc.Sequence[Field], offset: bool) -> UnitPart:
    return UnitPart(
        [
            Pragma(
                "Warnings",
                [Variable("Off"), String('use clause for type "Base_Integer" * has no effect')],
            ),
            Pragma(
                "Warnings",
                [Variable("Off"), String('use clause for type "Bytes" * has no effect')],
            ),
            Pragma(
                "Warnings",
                [
                    Variable("Off"),
                    String(
                        '"BASE_INTEGER" is already use-visible through previous use_type_clause',
                    ),
                ],
            ),
            Pragma(
                "Warnings",
                [
                    Variable("Off"),
                    String('"LENGTH" is already use-visible through previous use_type_clause'),
                ],
            ),  # required when user-defined type Index is subtype of Length
            *[
                UseTypeClause(t)
                for t in [
                    *([const.TYPES_BYTES, const.TYPES_BYTE] if composite_fields else []),
                    const.TYPES_BYTES_PTR,
                    const.TYPES_LENGTH,
                    const.TYPES_INDEX,
                    const.TYPES_BIT_INDEX,
                    const.TYPES_BASE_INT,
                    *([const.TYPES_OFFSET] if offset else []),
                ]
            ],
            Pragma(
                "Warnings",
                [
                    Variable("On"),
                    String('"LENGTH" is already use-visible through previous use_type_clause'),
                ],
            ),
            Pragma(
                "Warnings",
                [
                    Variable("On"),
                    String(
                        '"BASE_INTEGER" is already use-visible through previous use_type_clause',
                    ),
                ],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String('use clause for type "Base_Integer" * has no effect')],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String('use clause for type "Bytes" * has no effect')],
            ),
        ],
    )


def create_allow_unevaluated_use_of_old() -> UnitPart:
    return UnitPart(
        [Pragma("Unevaluated_Use_Of_Old", [Variable("Allow")])],
        [Pragma("Unevaluated_Use_Of_Old", [Variable("Allow")])],
    )


def create_field_type(message: Message) -> UnitPart:
    return UnitPart(
        [
            EnumerationType(
                "Virtual_Field",
                dict.fromkeys(ID(f.affixed_name) for f in message.all_fields),
            ),
            RangeSubtype(
                "Field",
                "Virtual_Field",
                Variable(message.all_fields[1].affixed_name),
                Variable(message.all_fields[-2].affixed_name),
            ),
        ],
    )


def create_state_type() -> UnitPart:
    return UnitPart(
        private=[
            EnumerationType(
                "Cursor_State",
                dict.fromkeys(map(ID, ("S_Valid", "S_Well_Formed", "S_Invalid", "S_Incomplete"))),
            ),
        ],
    )


def create_cursor_type() -> UnitPart:
    return UnitPart(
        [
            PrivateType("Field_Cursor"),
            PrivateType("Field_Cursors"),
        ],
        private=[
            RecordType(
                "Field_Cursor",
                [
                    Component(
                        "State",
                        "Cursor_State",
                        Variable("S_Invalid"),
                    ),
                    Component(
                        "First",
                        const.TYPES_BIT_INDEX,
                        First(const.TYPES_BIT_INDEX),
                    ),
                    Component(
                        "Last",
                        const.TYPES_BIT_LENGTH,
                        First(const.TYPES_BIT_LENGTH),
                    ),
                    Component(
                        "Value",
                        const.TYPES_BASE_INT,
                        Number(0),
                    ),
                ],
            ),
            ArrayType("Field_Cursors", "Virtual_Field", "Field_Cursor"),
        ],
    )


def create_cursor_validation_functions() -> UnitPart:
    parameters = [Parameter(["Cursor"], "Field_Cursor")]

    return UnitPart(
        [],
        [],
        [
            ExpressionFunctionDeclaration(
                FunctionSpecification("Well_Formed", "Boolean", parameters),
                Or(
                    Equal(Variable("Cursor.State"), Variable("S_Valid")),
                    Equal(Variable("Cursor.State"), Variable("S_Well_Formed")),
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


def create_cursors_invariant_function() -> UnitPart:
    """
    Create the function to hold the invariant that defines valid representations of fields.

    Each field cursor represents the state of one parsed or serialized message field.
    This invariant ensures for all well formed fields that

        - the field bounds are inside the range of verified buffer part,
        - the field size is greater or equal to zero,
        - and the field value fulfills all constraints of its field type.
    """
    specification = FunctionSpecification(
        "Cursors_Invariant",
        "Boolean",
        [
            Parameter(["Cursors"], "Field_Cursors"),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Verified_Last"], const.TYPES_BIT_LENGTH),
        ],
    )
    return UnitPart(
        [],
        [],
        common.wrap_warning(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    ForAllIn(
                        "F",
                        Variable("Field"),
                        If(
                            [
                                (
                                    Call(
                                        "Well_Formed",
                                        [Indexed(Variable("Cursors"), Variable("F"))],
                                    ),
                                    And(
                                        GreaterEqual(
                                            Selected(
                                                Indexed(Variable("Cursors"), Variable("F")),
                                                "First",
                                            ),
                                            Variable("First"),
                                        ),
                                        LessEqual(
                                            Selected(
                                                Indexed(Variable("Cursors"), Variable("F")),
                                                "Last",
                                            ),
                                            Variable("Verified_Last"),
                                        ),
                                        LessEqual(
                                            Selected(
                                                Indexed(Variable("Cursors"), Variable("F")),
                                                "First",
                                            ),
                                            Add(
                                                Selected(
                                                    Indexed(Variable("Cursors"), Variable("F")),
                                                    "Last",
                                                ),
                                                Number(1),
                                            ),
                                        ),
                                        Call(
                                            "Valid_Value",
                                            [
                                                Variable("F"),
                                                Selected(
                                                    Indexed(Variable("Cursors"), Variable("F")),
                                                    "Value",
                                                ),
                                            ],
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ),
                    [Postcondition(TRUE)],
                ),
            ],
            ["postcondition does not mention function result"],
        ),
    )


def create_valid_predecessors_invariant_function(
    message: Message,
    composite_fields: abc.Sequence[Field],
    prefix: str,
) -> UnitPart:
    """
    Create the invariant that defines the state of predecessors of valid fields.

    This invariant ensures for all well formed message fields that

        - one of its predecessor fields is well formed,
        - the predecessor component in the cursor refers to a valid predecessor,
        - and the condition on the link between the field and its predecessor is fulfilled.

    This ensures that there is a valid message path from each well formed field to the
    initial field.
    """
    specification = FunctionSpecification(
        "Valid_Predecessors_Invariant",
        "Boolean",
        [
            Parameter(["Cursors"], "Field_Cursors"),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Verified_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Written_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Buffer"], const.TYPES_BYTES_PTR),
            *common.message_parameters(message),
        ],
    )
    return UnitPart(
        [],
        [],
        common.wrap_warning(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    AndThen(
                        *[
                            If(
                                [
                                    (
                                        Call(
                                            "Well_Formed",
                                            [
                                                Indexed(
                                                    Variable("Cursors"),
                                                    Variable(f.affixed_name),
                                                ),
                                            ],
                                        ),
                                        Or(
                                            *[
                                                expr.AndThen(
                                                    expr.Call(
                                                        "Well_Formed"
                                                        if l.source in composite_fields
                                                        else "Valid",
                                                        [
                                                            expr.Indexed(
                                                                expr.Variable("Cursors"),
                                                                expr.Variable(
                                                                    l.source.affixed_name,
                                                                ),
                                                            ),
                                                        ],
                                                    )
                                                    if l.source != INITIAL
                                                    else expr.TRUE,
                                                    l.condition.substituted(
                                                        common.substitution(
                                                            message,
                                                            embedded=True,
                                                            prefix=prefix,
                                                        ),
                                                    ),
                                                )
                                                .simplified()
                                                .ada_expr()
                                                for l in message.incoming(f)
                                            ],
                                        ),
                                    ),
                                ],
                            )
                            for f in message.fields
                        ],
                    ),
                    [
                        Precondition(
                            Call(
                                "Cursors_Invariant",
                                [Variable("Cursors"), Variable("First"), Variable("Verified_Last")],
                            ),
                        ),
                        Postcondition(TRUE),
                    ],
                ),
            ],
            [
                'formal parameter "*" is not referenced',
                "postcondition does not mention function result",
                'unused variable "*"',
            ],
        ),
    )


def create_valid_next_internal_function(
    message: Message,
    composite_fields: abc.Sequence[Field],
    prefix: str,
) -> UnitPart:
    """
    Create the function that tells us if a field can be set.

    This is the case if one of its incoming links corresponds to
    the field's registered predecessor and the corresponding condition
    evaluates to True.
    """
    specification = FunctionSpecification(
        "Valid_Next_Internal",
        "Boolean",
        [
            Parameter(["Cursors"], "Field_Cursors"),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Verified_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Written_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Buffer"], const.TYPES_BYTES_PTR),
            *common.message_parameters(message),
            Parameter(["Fld"], "Field"),
        ],
    )

    def link_expr(link: Link) -> Expr:
        if link.source == INITIAL:
            return TRUE
        condition = link.condition.substituted(
            common.substitution(message, prefix, embedded=True),
        ).simplified()
        return AndThen(
            Call(
                "Well_Formed" if link.source in composite_fields else "Valid",
                [Indexed(Variable("Cursors"), Variable(link.source.affixed_name))],
            ),
            condition.ada_expr(),
        )

    def valid_next_expr(fld: Field) -> Expr:
        incoming = message.incoming(fld)
        return Or(*[link_expr(lnk) for lnk in incoming])

    param_args = [Variable(param.name) for param in message.parameter_types]

    return UnitPart(
        [],
        private=common.wrap_warning(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [(Variable(f.affixed_name), valid_next_expr(f)) for f in message.fields],
                    ),
                    [
                        Precondition(
                            AndThen(
                                Call(
                                    "Cursors_Invariant",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                    ],
                                ),
                                Call(
                                    "Valid_Predecessors_Invariant",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                        Variable("Written_Last"),
                                        Variable("Buffer"),
                                        *param_args,
                                    ],
                                ),
                            ),
                        ),
                        Postcondition(TRUE),
                    ],
                ),
            ],
            [
                "postcondition does not mention function result",
            ],
        ),
    )


def create_field_size_internal_function(message: Message, prefix: str) -> UnitPart:
    specification = FunctionSpecification(
        "Field_Size_Internal",
        "RFLX_Types.Bit_Length'Base",
        [
            Parameter(["Cursors"], "Field_Cursors"),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Verified_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Written_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Buffer"], const.TYPES_BYTES_PTR),
            *common.message_parameters(message),
            Parameter(["Fld"], "Field"),
        ],
    )

    param_args = [Variable(param.name) for param in message.parameter_types]

    return UnitPart(
        [],
        private=common.wrap_warning(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (
                                Variable(f.affixed_name),
                                common.conditional_field_size(f, message, prefix),
                            )
                            for f in message.fields
                        ],
                    ),
                    [
                        Precondition(
                            AndThen(
                                Call(
                                    "Cursors_Invariant",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                    ],
                                ),
                                Call(
                                    "Valid_Predecessors_Invariant",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                        Variable("Written_Last"),
                                        Variable("Buffer"),
                                        *param_args,
                                    ],
                                ),
                                Call(
                                    "Valid_Next_Internal",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                        Variable("Written_Last"),
                                        Variable("Buffer"),
                                        *param_args,
                                        Variable("Fld"),
                                    ],
                                ),
                            ),
                        ),
                    ],
                ),
            ],
            ['unused variable "*"', 'formal parameter "*" is not referenced'],
        ),
    )


def create_field_first_internal_function(message: Message, prefix: str) -> UnitPart:
    def recursive_call(fld: Field) -> expr.Expr:
        return expr.Call(
            "Field_First_Internal",
            [
                expr.Variable("Cursors"),
                expr.Variable("First"),
                expr.Variable("Verified_Last"),
                expr.Variable("Written_Last"),
                expr.Variable("Buffer"),
                *[expr.Variable(param.name) for param in message.parameter_types],
                expr.Variable(fld.affixed_name),
            ],
        )

    def field_size_internal_call(fld: expr.Variable) -> expr.Expr:
        return expr.Call(
            "Field_Size_Internal",
            [
                expr.Variable("Cursors"),
                expr.Variable("First"),
                expr.Variable("Verified_Last"),
                expr.Variable("Written_Last"),
                expr.Variable("Buffer"),
                *[expr.Variable(param.name) for param in message.parameter_types],
                fld,
            ],
        )

    def field_size_substitution(exp: expr.Expr) -> expr.Expr:
        if isinstance(exp, expr.Size) and isinstance(exp.prefix, expr.Variable):
            return field_size_internal_call(exp.prefix)
        return exp

    def link_first_expr(link: Link) -> tuple[expr.Expr, expr.Expr]:
        condition = link.condition.substituted(
            common.substitution(message, prefix, embedded=True),
        ).simplified()
        precond = (
            expr.AndThen(
                expr.Call(
                    "Well_Formed",
                    [
                        expr.Indexed(
                            expr.Variable("Cursors"),
                            expr.Variable(link.source.affixed_name),
                        ),
                    ],
                ),
                condition,
            )
            if link.source != INITIAL
            else expr.TRUE
        )
        fld, dist = message.link_first(link)
        assert fld != link.target
        first = expr.Add(
            expr.Variable("First") if fld == INITIAL else recursive_call(fld),
            dist.substituted(field_size_substitution),
        )
        return (precond, first)

    def fld_first_expr(fld: Field) -> expr.Expr:
        first_node, dist = message.field_first(fld)
        if first_node == fld and dist == expr.Number(0) and first_node != INITIAL:
            incoming = message.incoming(fld)
            first_expr = [link_first_expr(fld) for fld in incoming]
            return expr.IfExpr(
                first_expr,
                expr.Call("RFLX_Types.Unreachable"),
            )
        assert first_node != fld
        return expr.Add(
            expr.Variable("First") if first_node == INITIAL else recursive_call(first_node),
            dist.substituted(field_size_substitution),
        ).simplified()

    specification = FunctionSpecification(
        "Field_First_Internal",
        "RFLX_Types.Bit_Index'Base",
        [
            Parameter(["Cursors"], "Field_Cursors"),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Verified_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Written_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Buffer"], const.TYPES_BYTES_PTR),
            *common.message_parameters(message),
            Parameter(["Fld"], "Field"),
        ],
    )

    param_args = [Variable(param.name) for param in message.parameter_types]

    return UnitPart(
        [],
        private=common.wrap_warning(
            [
                ExpressionFunctionDeclaration(
                    specification,
                    Case(
                        Variable("Fld"),
                        [
                            (Variable(f.affixed_name), fld_first_expr(f).ada_expr())
                            for f in message.fields
                        ],
                    ),
                    [
                        Precondition(
                            AndThen(
                                Call(
                                    "Cursors_Invariant",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                    ],
                                ),
                                Call(
                                    "Valid_Predecessors_Invariant",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                        Variable("Written_Last"),
                                        Variable("Buffer"),
                                        *param_args,
                                    ],
                                ),
                                Call(
                                    "Valid_Next_Internal",
                                    [
                                        Variable("Cursors"),
                                        Variable("First"),
                                        Variable("Verified_Last"),
                                        Variable("Written_Last"),
                                        Variable("Buffer"),
                                        *param_args,
                                        Variable("Fld"),
                                    ],
                                ),
                            ),
                        ),
                        Postcondition(TRUE),
                        SubprogramVariant(Decreases(Variable("Fld"))),
                    ],
                ),
            ],
            [
                # Eng/RecordFlux/Workarounds#47
                "postcondition does not mention function result",
                'unused variable "*"',
                "no recursive call visible",
                'formal parameter "*" is not referenced',
            ],
        ),
    )


def create_valid_context_function(
    message: Message,
    prefix: str,
) -> UnitPart:
    specification = FunctionSpecification(
        "Valid_Context",
        "Boolean",
        [
            Parameter(["Buffer_First", "Buffer_Last"], const.TYPES_INDEX),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Verified_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Written_Last"], const.TYPES_BIT_LENGTH),
            Parameter(["Buffer"], const.TYPES_BYTES_PTR),
            Parameter(["Cursors"], "Field_Cursors"),
            *common.message_parameters(message),
        ],
    )

    return UnitPart(
        [],
        [],
        [
            # Eng/RecordFlux/Workarounds#36
            # An access constant type cannot be used here, because the "implicit conversion
            # between access types with different designated types is not yet supported".
            Pragma(
                "Warnings",
                [
                    Variable("Off"),
                    String('"Buffer" is not modified, could be of access constant type'),
                ],
            ),
            # Eng/RecordFlux/Workarounds#47
            Pragma(
                "Warnings",
                [Variable("Off"), String("postcondition does not mention function result")],
            ),
            ExpressionFunctionDeclaration(
                specification,
                common.context_predicate(message, prefix),
                [Postcondition(TRUE)],
            ),
            Pragma(
                "Warnings",
                [
                    Variable("On"),
                    String('"Buffer" is not modified, could be of access constant type'),
                ],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String("postcondition does not mention function result")],
            ),
        ],
    )


def create_context_type(message: Message) -> UnitPart:
    """
    Create the context type for a message.

    Components of a context type:

        Buffer_First, Buffer_Last:
            The bounds of `Buffer` which are used to ensure that not a completely different
            buffer is moved back into the context.

        First, Last:
            The positions of the first and last usable bit of `Buffer`. These are hard bounds
            which must not be changed during the lifetime of the context.

        Verified_Last:
            The position of the last bit of the last verified field. The value is initialized
            to `First - 1` and increased after each successfully verified or set field.

        Written_Last:
            The position of the last bit of written data. The value is initialized
            to `First - 1`, increased or kept when writing data into the buffer and set to
            `Verified_Last` when setting a field.

        Buffer:
            An access type refering to memory containing the binary message.

        Cursors:
            An array of cursors representing the internal parser or serializer state.
    """

    discriminants = [
        Discriminant(["Buffer_First", "Buffer_Last"], const.TYPES_INDEX, First(const.TYPES_INDEX)),
        Discriminant(["First"], const.TYPES_BIT_INDEX, First(const.TYPES_BIT_INDEX)),
        Discriminant(["Last"], const.TYPES_BIT_LENGTH, First(const.TYPES_BIT_LENGTH)),
        *[
            Discriminant(
                [p.name],
                common.ada_type_identifier(t.identifier),
                First(common.ada_type_identifier(t.identifier)),
            )
            for p, t in message.parameter_types.items()
        ],
    ]

    return UnitPart(
        [
            PrivateType(
                "Context",
                discriminants,
                [DefaultInitialCondition(common.public_context_predicate())],
            ),
        ],
        [],
        [
            RecordType(
                "Context",
                [
                    Component(
                        "Verified_Last",
                        const.TYPES_BIT_LENGTH,
                        Sub(Variable("First"), Number(1)),
                    ),
                    Component(
                        "Written_Last",
                        const.TYPES_BIT_LENGTH,
                        Sub(Variable("First"), Number(1)),
                    ),
                    Component("Buffer", const.TYPES_BYTES_PTR, NULL),
                    Component(
                        "Cursors",
                        "Field_Cursors",
                        NamedAggregate(("others", Variable("<>"))),
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
                                Variable("Context.Verified_Last"),
                                Variable("Context.Written_Last"),
                                Variable("Context.Buffer"),
                                Variable("Context.Cursors"),
                                *[
                                    Variable("Context" * p.identifier)
                                    for p in message.parameter_types
                                ],
                            ],
                        ),
                    ),
                ],
            ),
        ],
    )


def create_initialize_procedure(message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Initialize",
        [
            OutParameter(["Ctx"], "Context"),
            InOutParameter(["Buffer"], const.TYPES_BYTES_PTR),
            *common.message_parameters(message),
            Parameter(["Written_Last"], const.TYPES_BIT_LENGTH, Number(0)),
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
                            NotEqual(Variable("Buffer"), NULL),
                            Greater(Length("Buffer"), Number(0)),
                            Less(Last("Buffer"), Last(const.TYPES_INDEX)),
                            Or(
                                Equal(Variable("Written_Last"), Number(0)),
                                And(
                                    GreaterEqual(
                                        Variable("Written_Last"),
                                        Sub(
                                            Call(
                                                const.TYPES_TO_FIRST_BIT_INDEX,
                                                [First("Buffer")],
                                            ),
                                            Number(1),
                                        ),
                                    ),
                                    LessEqual(
                                        Variable("Written_Last"),
                                        Call(
                                            const.TYPES_TO_LAST_BIT_INDEX,
                                            [Last("Buffer")],
                                        ),
                                    ),
                                ),
                            ),
                            Equal(Mod(Variable("Written_Last"), Size(const.TYPES_BYTE)), Number(0)),
                        ),
                    ),
                    Postcondition(
                        And(
                            Call("Has_Buffer", [Variable("Ctx")]),
                            Equal(Variable("Buffer"), NULL),
                            Equal(Variable("Ctx.Buffer_First"), Old(First("Buffer"))),
                            Equal(Variable("Ctx.Buffer_Last"), Old(Last("Buffer"))),
                            Equal(
                                Variable("Ctx.First"),
                                Call(
                                    const.TYPES_TO_FIRST_BIT_INDEX,
                                    [Variable("Ctx.Buffer_First")],
                                ),
                            ),
                            Equal(
                                Variable("Ctx.Last"),
                                Call(
                                    const.TYPES_TO_LAST_BIT_INDEX,
                                    [Variable("Ctx.Buffer_Last")],
                                ),
                            ),
                            *common.initialize_conditions(message),
                        ),
                    ),
                    Depends(
                        {
                            "Ctx": [
                                "Buffer",
                                *[p.name for p in message.parameter_types],
                                "Written_Last",
                            ],
                            "Buffer": [],
                        },
                    ),
                ],
            ),
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
                            Call(const.TYPES_TO_FIRST_BIT_INDEX, [First("Buffer")]),
                            Call(const.TYPES_TO_LAST_BIT_INDEX, [Last("Buffer")]),
                            *[Variable(p.identifier) for p in message.parameter_types],
                            Variable("Written_Last"),
                        ],
                    ),
                ],
            ),
        ],
    )


def create_restricted_initialize_procedure(message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Initialize",
        [
            OutParameter(["Ctx"], "Context"),
            InOutParameter(["Buffer"], const.TYPES_BYTES_PTR),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Last"], const.TYPES_BIT_LENGTH),
            *common.message_parameters(message),
            Parameter(["Written_Last"], const.TYPES_BIT_LENGTH, Number(0)),
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
                            NotEqual(Variable("Buffer"), NULL),
                            Greater(Length("Buffer"), Number(0)),
                            Less(Last("Buffer"), Last(const.TYPES_INDEX)),
                            GreaterEqual(
                                Call(const.TYPES_TO_INDEX, [Variable("First")]),
                                First("Buffer"),
                            ),
                            LessEqual(
                                Call(const.TYPES_TO_INDEX, [Variable("Last")]),
                                Last("Buffer"),
                            ),
                            LessEqual(Variable("First"), Add(Variable("Last"), Number(1))),
                            Less(Variable("Last"), Last(const.TYPES_BIT_INDEX)),
                            Equal(Rem(Variable("First"), Size(const.TYPES_BYTE)), Number(1)),
                            Equal(Rem(Variable("Last"), Size(const.TYPES_BYTE)), Number(0)),
                            Or(
                                Equal(Variable("Written_Last"), Number(0)),
                                And(
                                    GreaterEqual(
                                        Variable("Written_Last"),
                                        Sub(Variable("First"), Number(1)),
                                    ),
                                    LessEqual(
                                        Variable("Written_Last"),
                                        Variable("Last"),
                                    ),
                                ),
                            ),
                            Equal(Rem(Variable("Written_Last"), Size(const.TYPES_BYTE)), Number(0)),
                        ),
                    ),
                    Postcondition(
                        And(
                            Equal(Variable("Buffer"), NULL),
                            Call("Has_Buffer", [Variable("Ctx")]),
                            Equal(Variable("Ctx.Buffer_First"), Old(First("Buffer"))),
                            Equal(Variable("Ctx.Buffer_Last"), Old(Last("Buffer"))),
                            Equal(Variable("Ctx.First"), Variable("First")),
                            Equal(Variable("Ctx.Last"), Variable("Last")),
                            *common.initialize_conditions(message),
                        ),
                    ),
                    Depends(
                        {
                            "Ctx": [
                                "Buffer",
                                "First",
                                "Last",
                                *[p.name for p in message.parameter_types],
                                "Written_Last",
                            ],
                            "Buffer": [],
                        },
                    ),
                ],
            ),
        ],
        [
            SubprogramBody(
                specification,
                [
                    ObjectDeclaration(
                        ["Buffer_First"],
                        const.TYPES_INDEX,
                        First("Buffer"),
                        constant=True,
                    ),
                    ObjectDeclaration(
                        ["Buffer_Last"],
                        const.TYPES_INDEX,
                        Last("Buffer"),
                        constant=True,
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
                            *[Variable(p.identifier) for p in message.parameter_types],
                            Sub(Variable("First"), Number(1)),
                            If(
                                [
                                    (
                                        Equal(Variable("Written_Last"), Number(0)),
                                        Sub(Variable("First"), Number(1)),
                                    ),
                                ],
                                Variable("Written_Last"),
                            ),
                            Variable("Buffer"),
                            common.context_cursors_initialization(message),
                        ),
                    ),
                    Assignment("Buffer", NULL),
                ],
            ),
        ],
    )


def create_initialized_function(prefix: str, message: Message) -> UnitPart:
    specification = FunctionSpecification("Initialized", "Boolean", [Parameter(["Ctx"], "Context")])
    first_field = message.fields[0]

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
                AndThen(
                    Equal(
                        Variable("Ctx.Verified_Last"),
                        Sub(Variable("Ctx.First"), Number(1)),
                    ),
                    Call(
                        "Valid_Next",
                        [
                            Variable("Ctx"),
                            Variable(first_field.affixed_name),
                        ],
                    ),
                    common.byte_aligned_field(prefix, message, first_field),
                    Equal(
                        Call(
                            "Available_Space",
                            [
                                Variable("Ctx"),
                                Variable(first_field.affixed_name),
                            ],
                        ),
                        Add(
                            Variable("Ctx.Last"),
                            -Variable("Ctx.First"),
                            Number(1),
                        ),
                    ),
                    ForAllIn(
                        "F",
                        Variable("Field"),
                        Call(
                            "Invalid",
                            [Variable("Ctx"), Variable("F")],
                        ),
                    ),
                ),
            ),
        ],
    )


def create_reset_procedure(prefix: str, message: Message) -> UnitPart:
    """
    Reset the state and buffer bounds of the context.

    Buffer bounds that were set during the initialization of the context will not be preserved.
    The effect of this procedure is semantically equivalent to:

    ```
    Take_Buffer (Context, Buffer);
    Initialize (Context, Buffer);
    ```
    """

    specification = ProcedureSpecification(
        "Reset",
        [
            InOutParameter(["Ctx"], "Context"),
            *common.message_parameters(message),
        ],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        And(
                            Not(Constrained("Ctx")),
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                        ),
                    ),
                    Postcondition(
                        And(
                            Call("Has_Buffer", [Variable("Ctx")]),
                            *[
                                Equal(e, Old(e))
                                for e in [
                                    Variable("Ctx.Buffer_First"),
                                    Variable("Ctx.Buffer_Last"),
                                ]
                            ],
                            Equal(
                                Variable("Ctx.First"),
                                Call(
                                    const.TYPES * "To_First_Bit_Index",
                                    [Variable("Ctx.Buffer_First")],
                                ),
                            ),
                            Equal(
                                Variable("Ctx.Last"),
                                Call(
                                    const.TYPES * "To_Last_Bit_Index",
                                    [Variable("Ctx.Buffer_Last")],
                                ),
                            ),
                            *common.initialize_conditions(message),
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
                    CallStatement(
                        "Reset",
                        [
                            Variable("Ctx"),
                            Call(const.TYPES_TO_FIRST_BIT_INDEX, [First("Ctx.Buffer")]),
                            Call(const.TYPES_TO_LAST_BIT_INDEX, [Last("Ctx.Buffer")]),
                            *[Variable(p.identifier) for p in message.parameter_types],
                        ],
                    ),
                ],
            ),
        ],
    )


def create_restricted_reset_procedure(prefix: str, message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Reset",
        [
            InOutParameter(["Ctx"], "Context"),
            Parameter(["First"], const.TYPES_BIT_INDEX),
            Parameter(["Last"], const.TYPES_BIT_LENGTH),
            *common.message_parameters(message),
        ],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        And(
                            Not(Constrained("Ctx")),
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            GreaterEqual(
                                Call(const.TYPES_TO_INDEX, [Variable("First")]),
                                Variable("Ctx.Buffer_First"),
                            ),
                            LessEqual(
                                Call(const.TYPES_TO_INDEX, [Variable("Last")]),
                                Variable("Ctx.Buffer_Last"),
                            ),
                            LessEqual(Variable("First"), Add(Variable("Last"), Number(1))),
                            Less(Variable("Last"), Last(const.TYPES_BIT_LENGTH)),
                            Equal(Rem(Variable("First"), Size(const.TYPES_BYTE)), Number(1)),
                            Equal(Rem(Variable("Last"), Size(const.TYPES_BYTE)), Number(0)),
                        ),
                    ),
                    Postcondition(
                        And(
                            Call("Has_Buffer", [Variable("Ctx")]),
                            *[
                                Equal(e, Old(e))
                                for e in [
                                    Variable("Ctx.Buffer_First"),
                                    Variable("Ctx.Buffer_Last"),
                                ]
                            ],
                            Equal(Variable("Ctx.First"), Variable("First")),
                            Equal(Variable("Ctx.Last"), Variable("Last")),
                            *common.initialize_conditions(message),
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
                    Assignment(
                        "Ctx",
                        Aggregate(
                            Variable("Ctx.Buffer_First"),
                            Variable("Ctx.Buffer_Last"),
                            Variable("First"),
                            Variable("Last"),
                            *[Variable(p.identifier) for p in message.parameter_types],
                            Sub(Variable("First"), Number(1)),
                            Sub(Variable("First"), Number(1)),
                            Variable("Ctx.Buffer"),
                            common.context_cursors_initialization(message),
                        ),
                    ),
                ],
            ),
        ],
    )


def create_take_buffer_procedure(prefix: str, message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Take_Buffer",
        [InOutParameter(["Ctx"], "Context"), OutParameter(["Buffer"], const.TYPES_BYTES_PTR)],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                    ),
                    Postcondition(
                        And(
                            Not(Call("Has_Buffer", [Variable("Ctx")])),
                            NotEqual(Variable("Buffer"), NULL),
                            Equal(Variable("Ctx.Buffer_First"), First("Buffer")),
                            Equal(Variable("Ctx.Buffer_Last"), Last("Buffer")),
                            *common.context_invariant(message),
                            *[
                                Equal(e, Old(e))
                                for e in [Call("Context_Cursors", [Variable("Ctx")])]
                            ],
                        ),
                    ),
                    Depends({"Ctx": ["Ctx"], "Buffer": ["Ctx"]}),
                ],
            ),
        ],
        [
            SubprogramBody(
                specification,
                [],
                [Assignment("Buffer", Variable("Ctx.Buffer")), Assignment("Ctx.Buffer", NULL)],
            ),
        ],
    )


def create_copy_procedure(prefix: str, message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Copy",
        [Parameter(["Ctx"], "Context"), OutParameter(["Buffer"], const.TYPES_BYTES)],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Well_Formed_Message",
                                [Variable("Ctx")],
                            ),
                            Equal(
                                Call(
                                    prefix * message.identifier * "Byte_Size",
                                    [Variable("Ctx")],
                                ),
                                Length("Buffer"),
                            ),
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
                    IfStatement(
                        [
                            (
                                Greater(Length("Buffer"), Number(0)),
                                [
                                    Assignment(
                                        "Buffer",
                                        Indexed(
                                            Variable("Ctx.Buffer.all"),
                                            ValueRange(
                                                Call(
                                                    const.TYPES_TO_INDEX,
                                                    [Variable("Ctx.First")],
                                                ),
                                                Call(
                                                    const.TYPES_TO_INDEX,
                                                    [Variable("Ctx.Verified_Last")],
                                                ),
                                            ),
                                        ),
                                    ),
                                ],
                            ),
                        ],
                        [
                            Assignment(
                                "Buffer",
                                Indexed(
                                    Variable("Ctx.Buffer.all"),
                                    ValueRange(Number(1), Number(0)),
                                ),
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )


def create_read_function(prefix: str, message: Message) -> UnitPart:
    specification = FunctionSpecification(
        "Read",
        const.TYPES_BYTES,
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Ghost(),
                    Precondition(
                        AndThen(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Well_Formed_Message",
                                [Variable("Ctx")],
                            ),
                        ),
                    ),
                ],
            ),
        ],
        [
            ExpressionFunctionDeclaration(
                specification,
                Indexed(
                    Variable("Ctx.Buffer.all"),
                    ValueRange(
                        Call(const.TYPES_TO_INDEX, [Variable("Ctx.First")]),
                        Call(const.TYPES_TO_INDEX, [Variable("Ctx.Verified_Last")]),
                    ),
                ),
            ),
        ],
    )


def create_generic_read_procedure(prefix: str, message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Generic_Read",
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            Pragma(
                "Warnings",
                [Variable("Off"), String('formal parameter "*" is not referenced')],
            ),
            Pragma(
                "Warnings",
                [Variable("Off"), String('unused variable "*"')],
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "Always_Valid",
                    "Boolean",
                    [Parameter(["Buffer"], const.TYPES_BYTES)],
                ),
                TRUE,
                # Eng/RecordFlux/Workarounds#48
                # Ghost entities are not allowed as formal generic parameters.
                # aspects=[Ghost()],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String('unused variable "*"')],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String('formal parameter "*" is not referenced')],
            ),
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Well_Formed_Message",
                                [Variable("Ctx")],
                            ),
                            Call("Pre", [Call("Read", [Variable("Ctx")])]),
                        ),
                    ),
                ],
                [
                    FormalSubprogramDeclaration(
                        ProcedureSpecification(
                            "Read",
                            [Parameter(["Buffer"], const.TYPES_BYTES)],
                        ),
                    ),
                    FormalSubprogramDeclaration(
                        FunctionSpecification(
                            "Pre",
                            "Boolean",
                            [Parameter(["Buffer"], const.TYPES_BYTES)],
                        ),
                        "Always_Valid",
                    ),
                ],
            ),
        ],
        [
            SubprogramBody(
                specification,
                [],
                [
                    CallStatement(
                        "Read",
                        [
                            Indexed(
                                Variable("Ctx.Buffer.all"),
                                ValueRange(
                                    Call(const.TYPES_TO_INDEX, [Variable("Ctx.First")]),
                                    Call(const.TYPES_TO_INDEX, [Variable("Ctx.Verified_Last")]),
                                ),
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )


def create_generic_write_procedure(prefix: str, message: Message) -> UnitPart:
    """
    Write data into the buffer of the context using an externally provided subprogram.

    The complete buffer of the context can be overwritten. Buffer bounds that were set during
    the initialization of the context will not be considered or preserved.
    """
    specification = ProcedureSpecification(
        "Generic_Write",
        [
            InOutParameter(["Ctx"], "Context"),
            Parameter(["Offset"], const.TYPES_LENGTH, Number(0)),
        ],
    )

    return UnitPart(
        [
            Pragma(
                "Warnings",
                [Variable("Off"), String('formal parameter "*" is not referenced')],
            ),
            Pragma(
                "Warnings",
                [Variable("Off"), String('unused variable "*"')],
            ),
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "Always_Valid",
                    "Boolean",
                    [
                        Parameter(["Context_Buffer_Length"], const.TYPES_LENGTH),
                        Parameter(["Offset"], const.TYPES_LENGTH),
                    ],
                ),
                TRUE,
                # Eng/RecordFlux/Workarounds#48
                # Ghost entities are not allowed as formal generic parameters.
                # aspects=[Ghost()],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String('unused variable "*"')],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String('formal parameter "*" is not referenced')],
            ),
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Not(Constrained("Ctx")),
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Less(
                                Variable("Offset"),
                                Call(
                                    prefix * message.identifier * "Buffer_Length",
                                    [Variable("Ctx")],
                                ),
                            ),
                            Call(
                                "Pre",
                                [
                                    Call(
                                        prefix * message.identifier * "Buffer_Length",
                                        [Variable("Ctx")],
                                    ),
                                    Variable("Offset"),
                                ],
                            ),
                        ),
                    ),
                    Postcondition(
                        And(
                            Call("Has_Buffer", [Variable("Ctx")]),
                            *[
                                Equal(e, Old(e))
                                for e in [
                                    Variable("Ctx.Buffer_First"),
                                    Variable("Ctx.Buffer_Last"),
                                ]
                            ],
                            Equal(
                                Variable("Ctx.First"),
                                Call(
                                    const.TYPES * "To_First_Bit_Index",
                                    [Variable("Ctx.Buffer_First")],
                                ),
                            ),
                            Call("Initialized", [Variable("Ctx")]),
                        ),
                    ),
                ],
                [
                    FormalSubprogramDeclaration(
                        ProcedureSpecification(
                            "Write",
                            [
                                OutParameter(["Buffer"], const.TYPES_BYTES),
                                OutParameter(["Length"], const.TYPES_LENGTH),
                                Parameter(["Context_Buffer_Length"], const.TYPES_LENGTH),
                                Parameter(["Offset"], const.TYPES_LENGTH),
                            ],
                        ),
                    ),
                    FormalSubprogramDeclaration(
                        FunctionSpecification(
                            "Pre",
                            "Boolean",
                            [
                                Parameter(["Context_Buffer_Length"], const.TYPES_LENGTH),
                                Parameter(["Offset"], const.TYPES_LENGTH),
                            ],
                        ),
                        "Always_Valid",
                    ),
                ],
            ),
        ],
        [
            SubprogramBody(
                specification,
                [ObjectDeclaration(["Length"], const.TYPES_LENGTH)],
                [
                    CallStatement(
                        "Reset",
                        [
                            Variable("Ctx"),
                            Call(const.TYPES_TO_FIRST_BIT_INDEX, [Variable("Ctx.Buffer_First")]),
                            Call(const.TYPES_TO_LAST_BIT_INDEX, [Variable("Ctx.Buffer_Last")]),
                            *[Variable("Ctx" * p.identifier) for p in message.parameter_types],
                        ],
                    ),
                    CallStatement(
                        "Write",
                        [
                            Slice(
                                Variable("Ctx.Buffer.all"),
                                Add(
                                    First("Ctx.Buffer"),
                                    Call(const.TYPES_INDEX, [Add(Variable("Offset"), Number(1))]),
                                    -Number(1),
                                ),
                                Last("Ctx.Buffer"),
                            ),
                            Variable("Length"),
                            Length("Ctx.Buffer"),
                            Variable("Offset"),
                        ],
                    ),
                    # Eng/RecordFlux/Workarounds#39
                    # Improve the check message in case of a wrong instantiation of "Write".
                    PragmaStatement(
                        "Assert",
                        [
                            LessEqual(
                                Variable("Length"),
                                Length(
                                    Variable("Ctx.Buffer.all"),
                                ),
                            ),
                            String(
                                "Length <= Buffer'Length is not ensured by postcondition of"
                                ' "Write"',
                            ),
                        ],
                    ),
                    Assignment(
                        "Ctx.Written_Last",
                        Max(
                            const.TYPES_BIT_INDEX,
                            Variable("Ctx.Written_Last"),
                            Call(
                                const.TYPES_TO_LAST_BIT_INDEX,
                                [
                                    Add(
                                        Call(
                                            const.TYPES_LENGTH,
                                            [Variable("Ctx.Buffer_First")],
                                        ),
                                        Variable("Offset"),
                                        Variable("Length"),
                                        -Number(1),
                                    ),
                                ],
                            ),
                        ),
                    ),
                ],
            ),
        ],
    )


def create_valid_value_function(
    prefix: str,
    message: Message,
    scalar_fields: abc.Mapping[Field, Scalar],
) -> UnitPart:
    specification = FunctionSpecification(
        "Valid_Value",
        "Boolean",
        [
            Parameter(["Fld" if scalar_fields else "Unused_Fld"], "Field"),
            Parameter(["Val" if scalar_fields else "Unused_Val"], const.TYPES_BASE_INT),
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
                [Postcondition(TRUE)],
            ),
            Pragma(
                "Warnings",
                [Variable("On"), String("postcondition does not mention function result")],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                Case(
                    Variable("Fld"),
                    [
                        (
                            Variable(f.affixed_name),
                            Call(
                                f"Valid_{t.name}"
                                if is_builtin_type(t.identifier)
                                else prefix * t.package * f"Valid_{t.name}",
                                [Variable("Val")],
                            )
                            if isinstance(t, Scalar)
                            else TRUE,
                        )
                        for f, t in message.field_types.items()
                    ],
                )
                if scalar_fields
                else TRUE,
            ),
        ],
    )


def create_field_size_function(
    prefix: str,
    message: Message,
    scalar_fields: abc.Mapping[Field, Type],
    composite_fields: abc.Sequence[Field],
) -> UnitPart:
    specification = FunctionSpecification(
        "Field_Size",
        const.TYPES_BIT_LENGTH,
        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        And(
                            Call(
                                prefix * message.identifier * "Valid_Next",
                                [Variable("Ctx"), Variable("Fld")],
                            ),
                        ),
                    ),
                    *(
                        [
                            Postcondition(
                                Case(
                                    Variable("Fld"),
                                    [
                                        *[
                                            (
                                                Variable(f.affixed_name),
                                                Equal(
                                                    Rem(
                                                        Result("Field_Size"),
                                                        Size(const.TYPES_BYTE),
                                                    ),
                                                    Number(0),
                                                ),
                                            )
                                            for f in composite_fields
                                        ],
                                        *([(Variable("others"), TRUE)] if scalar_fields else []),
                                    ],
                                ),
                            ),
                        ]
                        if composite_fields
                        else []
                    ),
                ],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                Call(
                    "Field_Size_Internal",
                    [
                        Variable("Ctx.Cursors"),
                        Variable("Ctx.First"),
                        Variable("Ctx.Verified_Last"),
                        Variable("Ctx.Written_Last"),
                        Variable("Ctx.Buffer"),
                        *[Selected(Variable("Ctx"), fld.name) for fld in message.parameter_types],
                        Variable("Fld"),
                    ],
                ),
            ),
        ],
    )


def create_field_first_function(prefix: str, message: Message) -> UnitPart:
    specification = FunctionSpecification(
        "Field_First",
        const.TYPES_BIT_INDEX,
        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
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
                                prefix * message.identifier * "Valid_Next",
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
                    "Field_First_Internal",
                    [
                        Variable("Ctx.Cursors"),
                        Variable("Ctx.First"),
                        Variable("Ctx.Verified_Last"),
                        Variable("Ctx.Written_Last"),
                        Variable("Ctx.Buffer"),
                        *[Selected(Variable("Ctx"), fld.name) for fld in message.parameter_types],
                        Variable("Fld"),
                    ],
                ),
            ),
        ],
    )


def create_field_last_function(
    prefix: str,
    message: Message,
    scalar_fields: abc.Mapping[Field, Type],
    composite_fields: abc.Sequence[Field],
) -> UnitPart:
    specification = FunctionSpecification(
        "Field_Last",
        const.TYPES_BIT_LENGTH,
        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Call(
                                prefix * message.identifier * "Valid_Next",
                                [Variable("Ctx"), Variable("Fld")],
                            ),
                            Call(
                                prefix * message.identifier * "Sufficient_Space",
                                [Variable("Ctx"), Variable("Fld")],
                            ),
                        ),
                    ),
                    *(
                        [
                            Postcondition(
                                Case(
                                    Variable("Fld"),
                                    [
                                        *[
                                            (
                                                Variable(f.affixed_name),
                                                Equal(
                                                    Rem(
                                                        Result("Field_Last"),
                                                        Size(const.TYPES_BYTE),
                                                    ),
                                                    Number(0),
                                                ),
                                            )
                                            for f in composite_fields
                                        ],
                                        *([(Variable("others"), TRUE)] if scalar_fields else []),
                                    ],
                                ),
                            ),
                        ]
                        if composite_fields
                        else []
                    ),
                ],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                Add(
                    Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                    Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                    -Number(1),
                ),
            ),
        ],
    )


def create_field_condition_function(prefix: str, message: Message) -> UnitPart:
    """
    Check if the condition at any outgoing link of the field is valid.

    All values and sizes of predecessor fields are taken from the message context. The value
    or size of the respective field must be passed as argument `Val`, `Agg` or `Size`. This is
    required to be able to check the field condition as precondition on setter functions.

    The parameters `Val`, `Agg` and `Size` are only added, if required.
    """

    def condition(field: Field, message: Message) -> Expr:
        c: expr.Expr = expr.Or(*[l.condition for l in message.outgoing(field)])
        c = c.substituted(
            mapping={
                expr.Size(field.name): expr.Call(const.TYPES_BASE_INT, [expr.Variable("Size")]),
                expr.Last(field.name): expr.Call(
                    const.TYPES_BASE_INT,
                    [
                        expr.Call(
                            "Field_Last",
                            [
                                expr.Variable("Ctx"),
                                expr.Variable(field.affixed_name, immutable=True),
                            ],
                        ),
                    ],
                ),
                # Eng/RecordFlux/RecordFlux#276
                **{expr.ValidChecksum(f): expr.TRUE for f in message.checksums},
            },
        )
        if isinstance(message.field_types[field], Scalar):
            c = c.substituted(
                lambda x: expr.Variable("Val") if x == expr.Variable(field.name) else x,
            )
        elif isinstance(
            message.field_types[field],
            Composite,
        ) and common.has_aggregate_dependent_condition(message, field):
            c = c.substituted(
                lambda x: expr.Variable("Agg") if x == expr.Variable(field.name) else x,
            )
        return c.substituted(common.substitution(message, prefix)).simplified().ada_expr()

    parameters = [
        Parameter(["Ctx"], "Context"),
        Parameter(["Fld"], "Field"),
        *(
            [Parameter(["Val"], const.TYPES_BASE_INT)]
            if common.has_value_dependent_condition(message)
            else []
        ),
        *(
            [Parameter(["Agg"], const.TYPES_BYTES)]
            if common.has_aggregate_dependent_condition(message)
            else []
        ),
        *(
            [Parameter(["Size"], const.TYPES_BIT_LENGTH, Number(0))]
            if common.has_size_dependent_condition(message)
            else []
        ),
    ]

    specification = FunctionSpecification("Field_Condition", "Boolean", parameters)

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
                        AndThen(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            *(
                                [
                                    Call(
                                        prefix * message.identifier * "Valid_Value",
                                        [Variable("Fld"), Variable("Val")],
                                    ),
                                ]
                                if common.has_value_dependent_condition(message)
                                else []
                            ),
                            Call(
                                prefix * message.identifier * "Valid_Next",
                                [Variable("Ctx"), Variable("Fld")],
                            ),
                            Call(
                                prefix * message.identifier * "Sufficient_Space",
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
                Case(
                    Variable("Fld"),
                    [(Variable(f.affixed_name), condition(f, message)) for f in message.fields],
                ),
            ),
        ],
    )


def create_invalid_successor_function(message: Message, requires_set_procedure: bool) -> UnitPart:
    if len(message.fields) == 1 or not requires_set_procedure:
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
                                            ),
                                        ],
                                    )
                                    for s in message.direct_successors(f)
                                    if s != FINAL
                                ],
                            ),
                        )
                        for f in message.fields
                    ],
                ),
            ),
        ],
    )


def create_has_buffer_function() -> UnitPart:
    specification = FunctionSpecification("Has_Buffer", "Boolean", [Parameter(["Ctx"], "Context")])

    return UnitPart(
        [SubprogramDeclaration(specification)],
        private=[
            ExpressionFunctionDeclaration(specification, NotEqual(Variable("Ctx.Buffer"), NULL)),
        ],
    )


def create_buffer_length_function(prefix: str, message: Message) -> UnitPart:
    specification = FunctionSpecification(
        "Buffer_Length",
        const.TYPES_LENGTH,
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [Precondition(Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]))],
            ),
        ],
        private=[ExpressionFunctionDeclaration(specification, Length("Ctx.Buffer"))],
    )


def create_size_function() -> UnitPart:
    specification = FunctionSpecification(
        "Size",
        const.TYPES_BIT_LENGTH,
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Postcondition(
                        Equal(Rem(Result("Size"), Size(const.TYPES_BYTE)), Number(0)),
                    ),
                ],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                Add(
                    Variable("Ctx.Verified_Last"),
                    -Variable("Ctx.First"),
                    Number(1),
                ),
            ),
        ],
    )


def create_byte_size_function() -> UnitPart:
    specification = FunctionSpecification(
        "Byte_Size",
        const.TYPES_LENGTH,
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                Call(const.TYPES_TO_LENGTH, [Call("Size", [Variable("Ctx")])]),
            ),
        ],
    )


def create_message_last_function(prefix: str, message: Message) -> UnitPart:
    specification = FunctionSpecification(
        "Message_Last",
        const.TYPES_BIT_LENGTH,
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Well_Formed_Message",
                                [Variable("Ctx")],
                            ),
                        ),
                    ),
                ],
            ),
        ],
        private=[ExpressionFunctionDeclaration(specification, Variable("Ctx.Verified_Last"))],
    )


def create_written_last_function() -> UnitPart:
    specification = FunctionSpecification(
        "Written_Last",
        const.TYPES_BIT_LENGTH,
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(specification, Variable("Ctx.Written_Last")),
        ],
    )


def create_data_procedure(prefix: str, message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Data",
        [Parameter(["Ctx"], "Context"), OutParameter(["Data"], const.TYPES_BYTES)],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Well_Formed_Message",
                                [Variable("Ctx")],
                            ),
                            Equal(
                                Length("Data"),
                                Call(
                                    prefix * message.identifier * "Byte_Size",
                                    [
                                        Variable("Ctx"),
                                    ],
                                ),
                            ),
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
                    Assignment(
                        "Data",
                        Slice(
                            Variable("Ctx.Buffer.all"),
                            Call(const.TYPES_TO_INDEX, [Variable("Ctx.First")]),
                            Call(const.TYPES_TO_INDEX, [Variable("Ctx.Verified_Last")]),
                        ),
                    ),
                ],
            ),
        ],
    )


def create_valid_next_function(message: Message) -> UnitPart:
    specification = FunctionSpecification(
        "Valid_Next",
        "Boolean",
        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
    )

    return UnitPart(
        [SubprogramDeclaration(specification)],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                Call(
                    "Valid_Next_Internal",
                    [
                        Variable("Ctx.Cursors"),
                        Variable("Ctx.First"),
                        Variable("Ctx.Verified_Last"),
                        Variable("Ctx.Written_Last"),
                        Variable("Ctx.Buffer"),
                        *[Selected(Variable("Ctx"), fld.name) for fld in message.parameter_types],
                        Variable("Fld"),
                    ],
                ),
            ),
        ],
    )


def create_available_space_function(prefix: str, message: Message) -> UnitPart:
    specification = FunctionSpecification(
        "Available_Space",
        const.TYPES_BIT_LENGTH,
        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        Call(
                            prefix * message.identifier * "Valid_Next",
                            [Variable("Ctx"), Variable("Fld")],
                        ),
                    ),
                ],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                Add(
                    Variable("Ctx.Last"),
                    -Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                    Number(1),
                ),
            ),
        ],
    )


def create_sufficient_space_function(prefix: str, message: Message) -> UnitPart:
    specification = FunctionSpecification(
        "Sufficient_Space",
        "Boolean",
        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        Call(
                            prefix * message.identifier * "Valid_Next",
                            [Variable("Ctx"), Variable("Fld")],
                        ),
                    ),
                ],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                GreaterEqual(
                    Call("Available_Space", [Variable("Ctx"), Variable("Fld")]),
                    Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                ),
            ),
        ],
    )


def create_equal_function(
    prefix: str,
    message: Message,
    scalar_fields: abc.Mapping[Field, Type],
    composite_fields: abc.Sequence[Field],
) -> UnitPart:
    if not composite_fields:
        return UnitPart()
    specification = FunctionSpecification(
        "Equal",
        "Boolean",
        [
            Parameter(["Ctx"], "Context"),
            Parameter(["Fld"], "Field"),
            Parameter(["Data"], const.TYPES_BYTES),
        ],
    )

    first = Call(
        const.TYPES_TO_INDEX,
        [
            Call(
                "Field_First",
                [
                    Variable("Ctx"),
                    Variable("Fld"),
                ],
            ),
        ],
    )

    last = Call(
        const.TYPES_TO_INDEX,
        [
            Call(
                "Field_Last",
                [
                    Variable("Ctx"),
                    Variable("Fld"),
                ],
            ),
        ],
    )

    length = Add(Sub(last, first), Number(1))

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        And(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Valid_Next",
                                [Variable("Ctx"), Variable("Fld")],
                            ),
                        ),
                    ),
                ],
            ),
        ],
        [
            ExpressionFunctionDeclaration(
                specification,
                AndThen(
                    Call("Sufficient_Buffer_Length", [Variable("Ctx"), Variable("Fld")]),
                    Case(
                        Variable("Fld"),
                        [
                            *[
                                (
                                    Variable(f.affixed_name),
                                    # Instead of a more direct comparison of the form
                                    #   Ctx.Buffer.all (First .. Last) = Data
                                    # we expand the array equality to this form below,
                                    #  which is a bit easier for provers in some cases.
                                    AndThen(
                                        Equal(Length(Variable("Data")), length),
                                        ForAllIn(
                                            "I",
                                            ValueRange(first, last, const.TYPES_INDEX),
                                            Equal(
                                                Indexed(Variable("Ctx.Buffer.all"), Variable("I")),
                                                Indexed(
                                                    Variable("Data"),
                                                    Add(
                                                        First(Variable("Data")),
                                                        Sub(Variable("I"), first),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                )
                                for f in composite_fields
                            ],
                            *([(Variable("others"), FALSE)] if scalar_fields else []),
                        ],
                    ),
                ),
            ),
        ],
    )


def create_sufficient_buffer_length_function(prefix: str, message: Message) -> UnitPart:
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
                    NotEqual(Variable("Ctx.Buffer"), Variable("null")),
                    Less(
                        Add(
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                            Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                        ),
                        Last(const.TYPES_BIT_LENGTH),
                    ),
                    LessEqual(
                        Variable("Ctx.First"),
                        Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                    ),
                    LessEqual(
                        Add(
                            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
                            Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
                            -Number(1),
                        ),
                        Variable("Ctx.Written_Last"),
                    ),
                ),
                [
                    Precondition(
                        And(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Valid_Next",
                                [Variable("Ctx"), Variable("Fld")],
                            ),
                        ),
                    ),
                ],
            ),
        ],
    )


def create_reset_dependent_fields_procedure(prefix: str, message: Message) -> UnitPart:
    specification = ProcedureSpecification(
        "Reset_Dependent_Fields",
        [InOutParameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
    )
    field_location_invariant = And(
        Equal(
            Call("Field_First", [Variable("Ctx"), Variable("Fld")]),
            LoopEntry(Call("Field_First", [Variable("Ctx"), Variable("Fld")])),
        ),
        Equal(
            Call("Field_Size", [Variable("Ctx"), Variable("Fld")]),
            LoopEntry(Call("Field_Size", [Variable("Ctx"), Variable("Fld")])),
        ),
    )

    return UnitPart(
        [],
        [
            SubprogramBody(
                specification,
                [],
                [
                    ForIn(
                        "Fld_Loop",
                        ValueRange(Variable("Fld"), Last("Field")),
                        [
                            PragmaStatement("Loop_Invariant", [field_location_invariant]),
                            PragmaStatement(
                                "Loop_Invariant",
                                [
                                    common.unchanged_cursor_before_or_invalid(
                                        Variable("Fld_Loop"),
                                        loop_entry=True,
                                        including_limit=True,
                                    ),
                                ],
                            ),
                            Assignment(
                                Indexed(
                                    Variable("Ctx.Cursors"),
                                    Variable("Fld_Loop"),
                                ),
                                NamedAggregate(
                                    ("State", Variable("S_Invalid")),
                                    ("others", Variable("<>")),
                                ),
                            ),
                        ],
                        reverse=True,
                    ),
                ],
                [
                    Precondition(
                        And(
                            Call(
                                prefix * message.identifier * "Valid_Next",
                                [Variable("Ctx"), Variable("Fld")],
                            ),
                        ),
                    ),
                    Postcondition(
                        And(
                            Call("Valid_Next", [Variable("Ctx"), Variable("Fld")]),
                            *common.context_invariant(message),
                            *[
                                Equal(e, Old(e))
                                for e in [
                                    Call("Has_Buffer", [Variable("Ctx")]),
                                    Call(
                                        "Field_First",
                                        [Variable("Ctx"), Variable("Fld")],
                                    ),
                                    Call(
                                        "Field_Size",
                                        [Variable("Ctx"), Variable("Fld")],
                                    ),
                                ]
                            ],
                            Call(
                                "Invalid",
                                [
                                    Variable("Ctx"),
                                    Variable(message.fields[0].affixed_name),
                                ],
                            )
                            if len(message.fields) == 1
                            else common.unchanged_cursor_before_or_invalid(
                                Variable("Fld"),
                                loop_entry=False,
                            ),
                        ),
                    ),
                ],
            ),
        ],
    )


def create_composite_field_function(
    scalar_fields: abc.Mapping[Field, Type],
    composite_fields: abc.Sequence[Field],
) -> UnitPart:
    always_true = not scalar_fields and len(composite_fields) == 1
    return UnitPart(
        body=[
            ExpressionFunctionDeclaration(
                FunctionSpecification(
                    "Composite_Field",
                    "Boolean",
                    [Parameter(["Unused_Fld" if always_true else "Fld"], "Field")],
                ),
                TRUE
                if always_true
                else In(
                    Variable("Fld"),
                    ChoiceList(*[Variable(f.affixed_name) for f in composite_fields]),
                ),
            ),
        ],
    )


def create_switch_procedures(
    prefix: str,
    message: Message,
    sequence_fields: abc.Mapping[Field, Type],
) -> UnitPart:
    def specification(field: Field) -> ProcedureSpecification:
        return ProcedureSpecification(
            f"Switch_To_{field.name}",
            [
                InOutParameter(["Ctx"], "Context"),
                OutParameter(
                    ["Seq_Ctx"],
                    prefix * common.sequence_name(message, field) * "Context",
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
                            Not(Constrained("Ctx")),
                            Not(Constrained("Seq_Ctx")),
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Valid_Next",
                                [
                                    Variable("Ctx"),
                                    Variable(prefix * message.identifier * f.affixed_name),
                                ],
                            ),
                            Greater(
                                Call(
                                    prefix * message.identifier * "Field_Size",
                                    [
                                        Variable("Ctx"),
                                        Variable(prefix * message.identifier * f.affixed_name),
                                    ],
                                ),
                                Number(0),
                            ),
                            common.byte_aligned_field(prefix, message, f),
                            common.sufficient_space_for_field_condition(
                                prefix * message.identifier,
                                Variable(prefix * message.identifier * f.affixed_name),
                            ),
                            common.field_condition_call(prefix, message, f),
                        ),
                    ),
                    Postcondition(
                        And(
                            *_switch_update_conditions(prefix, message, f),
                            Call(
                                prefix * common.sequence_name(message, f) * "Valid",
                                [Variable("Seq_Ctx")],
                            ),
                            Equal(
                                Call(
                                    prefix * common.sequence_name(message, f) * "Sequence_Last",
                                    [Variable("Seq_Ctx")],
                                ),
                                Sub(Variable("Seq_Ctx.First"), Number(1)),
                            ),
                            Call(
                                "Present",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            ),
                            *common.context_invariant(message),
                            *[
                                Equal(e, Old(e))
                                for e in [
                                    Call(
                                        "Field_Last",
                                        [
                                            Variable("Ctx"),
                                            Variable(f.affixed_name),
                                        ],
                                    ),
                                ]
                            ],
                            *common.context_cursor_unchanged(message, f, predecessors=True),
                        ),
                    ),
                    ContractCases(
                        (
                            Call(
                                "Well_Formed",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            ),
                            And(*common.context_cursor_unchanged(message, f, predecessors=False)),
                        ),
                        (
                            Variable("others"),
                            And(
                                *common.valid_path_to_next_field_condition(message, f, prefix),
                                *[
                                    Call(
                                        "Invalid",
                                        [
                                            Variable("Ctx"),
                                            Variable(s.affixed_name),
                                        ],
                                    )
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
                    *common.field_bit_location_declarations(Variable(f.affixed_name)),
                    ObjectDeclaration(["Buffer"], const.TYPES_BYTES_PTR),
                ],
                [
                    IfStatement(
                        [
                            (
                                Call(
                                    "Invalid",
                                    [Variable("Ctx"), Variable(f.affixed_name)],
                                ),
                                common.initialize_field_statements(f),
                            ),
                        ],
                    ),
                    CallStatement("Take_Buffer", [Variable("Ctx"), Variable("Buffer")]),
                    PragmaStatement(
                        "Warnings",
                        [Variable("Off"), String('unused assignment to "Buffer"')],
                    ),
                    CallStatement(
                        prefix * common.sequence_name(message, f) * "Initialize",
                        [
                            Variable("Seq_Ctx"),
                            Variable("Buffer"),
                            Variable("First"),
                            Variable("Last"),
                        ],
                    ),
                    PragmaStatement(
                        "Warnings",
                        [Variable("On"), String('unused assignment to "Buffer"')],
                    ),
                ],
            )
            for f in sequence_fields
        ],
    )


def create_complete_functions(
    prefix: str,
    message: Message,
    sequence_fields: abc.Mapping[Field, Type],
) -> UnitPart:
    def specification(field: Field) -> FunctionSpecification:
        return FunctionSpecification(
            f"Complete_{field.name}",
            "Boolean",
            [
                Parameter(["Ctx"], "Context"),
                Parameter(["Seq_Ctx"], prefix * common.sequence_name(message, field) * "Context"),
            ],
        )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification(f),
                [
                    Precondition(
                        Call(
                            prefix * message.identifier * "Valid_Next",
                            [
                                Variable("Ctx"),
                                Variable(prefix * message.identifier * f.affixed_name),
                            ],
                        ),
                    ),
                ],
            )
            for f in sequence_fields
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification(f),
                And(
                    Call(
                        prefix * common.sequence_name(message, f) * "Valid",
                        [Variable("Seq_Ctx")],
                    ),
                    Equal(
                        Call(
                            prefix * common.sequence_name(message, f) * "Size",
                            [Variable("Seq_Ctx")],
                        ),
                        Call(
                            "Field_Size",
                            [Variable("Ctx"), Variable(f.affixed_name)],
                        ),
                    ),
                ),
            )
            for f in sequence_fields
        ],
    )


def create_update_procedures(
    prefix: str,
    message: Message,
    sequence_fields: abc.Mapping[Field, Type],
) -> UnitPart:
    def specification(field: Field) -> ProcedureSpecification:
        return ProcedureSpecification(
            f"Update_{field.name}",
            [
                InOutParameter(["Ctx"], "Context"),
                InOutParameter(
                    ["Seq_Ctx"],
                    prefix * common.sequence_name(message, field) * "Context",
                ),
            ],
        )

    def take_buffer_arguments(field: Field) -> abc.Sequence[Expr]:
        arguments = [
            Variable("Seq_Ctx"),
            Variable("Buffer"),
        ]

        field_type = message.field_types[field]
        assert isinstance(field_type, Sequence)

        return arguments

    return UnitPart(
        [
            SubprogramDeclaration(
                specification(f),
                [
                    Precondition(
                        AndThen(
                            Call(
                                prefix * message.identifier * "Present",
                                [
                                    Variable("Ctx"),
                                    Variable(prefix * message.identifier * f.affixed_name),
                                ],
                            ),
                            *_switch_update_conditions(prefix, message, f),
                        ),
                    ),
                    Postcondition(
                        And(
                            If(
                                [
                                    (
                                        Call(
                                            prefix * message.identifier * f"Complete_{f.name}",
                                            [Variable("Ctx"), Variable("Seq_Ctx")],
                                        ),
                                        And(
                                            Call(
                                                "Present",
                                                [Variable("Ctx"), Variable(f.affixed_name)],
                                            ),
                                            *[
                                                Equal(e, Old(e))
                                                for e in [
                                                    Call(
                                                        "Context_Cursor",
                                                        [
                                                            Variable("Ctx"),
                                                            Variable(o.affixed_name),
                                                        ],
                                                    )
                                                    for o in message.successors(f)
                                                ]
                                            ],
                                        ),
                                    ),
                                ],
                                And(
                                    *[
                                        Call(
                                            "Invalid",
                                            [
                                                Variable("Ctx"),
                                                Variable(o.affixed_name),
                                            ],
                                        )
                                        for o in [f, *message.successors(f)]
                                    ],
                                ),
                            ),
                            Call("Has_Buffer", [Variable("Ctx")]),
                            Not(
                                Call(
                                    prefix * common.sequence_name(message, f) * "Has_Buffer",
                                    [Variable("Seq_Ctx")],
                                ),
                            ),
                            *common.context_invariant(message),
                            *[
                                Equal(e, Old(e))
                                for e in [
                                    Variable("Seq_Ctx.First"),
                                    Variable("Seq_Ctx.Last"),
                                    Call(
                                        prefix * common.sequence_name(message, f) * "Valid",
                                        [Variable("Seq_Ctx")],
                                    ),
                                    Call(
                                        prefix * common.sequence_name(message, f) * "Size",
                                        [Variable("Seq_Ctx")],
                                    ),
                                    Call(
                                        "Field_First",
                                        [
                                            Variable("Ctx"),
                                            Variable(f.affixed_name),
                                        ],
                                    ),
                                    Call(
                                        "Field_Size",
                                        [
                                            Variable("Ctx"),
                                            Variable(f.affixed_name),
                                        ],
                                    ),
                                ]
                                + [
                                    Call(
                                        "Context_Cursor",
                                        [
                                            Variable("Ctx"),
                                            Variable(o.affixed_name),
                                        ],
                                    )
                                    for o in message.predecessors(f)
                                ]
                            ],
                        ),
                    ),
                    Depends({"Ctx": ["Ctx", "Seq_Ctx"], "Seq_Ctx": ["Seq_Ctx"]}),
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
                        Call(
                            prefix * message.identifier * f"Complete_{f.name}",
                            [Variable("Ctx"), Variable("Seq_Ctx")],
                        ),
                        constant=True,
                    ),
                    ObjectDeclaration(["Buffer"], const.TYPES_BYTES_PTR),
                ],
                [
                    CallStatement(
                        prefix * common.sequence_name(message, f) * "Take_Buffer",
                        take_buffer_arguments(f),
                    ),
                    Assignment("Ctx.Buffer", Variable("Buffer")),
                    IfStatement(
                        [
                            (
                                Variable("Valid_Sequence"),
                                [
                                    Assignment(
                                        Indexed(
                                            Variable("Ctx.Cursors"),
                                            Variable(f.affixed_name),
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
                                                )
                                            ],
                                        ),
                                    ),
                                ],
                            ),
                        ],
                        [
                            CallStatement(
                                "Reset_Dependent_Fields",
                                [Variable("Ctx"), Variable(f.affixed_name)],
                            ),
                            Assignment(
                                Indexed(
                                    Variable("Ctx.Cursors"),
                                    Variable(f.affixed_name),
                                ),
                                NamedAggregate(
                                    ("State", Variable("S_Invalid")),
                                    ("others", Variable("<>")),
                                ),
                            ),
                        ],
                    ),
                ],
            )
            for f in sequence_fields
        ],
    )


def create_cursor_function() -> UnitPart:
    specification = FunctionSpecification(
        "Context_Cursor",
        "Field_Cursor",
        [Parameter(["Ctx"], "Context"), Parameter(["Fld"], "Field")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [Annotate("GNATprove", "Inline_For_Proof"), Ghost()],
            ),
        ],
        [],
        [
            ExpressionFunctionDeclaration(
                specification,
                Indexed(Variable("Ctx.Cursors"), Variable("Fld")),
            ),
        ],
    )


def create_cursors_function() -> UnitPart:
    specification = FunctionSpecification(
        "Context_Cursors",
        "Field_Cursors",
        [Parameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [Annotate("GNATprove", "Inline_For_Proof"), Ghost()],
            ),
        ],
        [],
        [ExpressionFunctionDeclaration(specification, Variable("Ctx.Cursors"))],
    )


def create_cursors_index_function() -> UnitPart:
    specification = FunctionSpecification(
        "Context_Cursors_Index",
        "Field_Cursor",
        [Parameter(["Cursors"], "Field_Cursors"), Parameter(["Fld"], "Field")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [Annotate("GNATprove", "Inline_For_Proof"), Ghost()],
            ),
        ],
        [],
        [
            ExpressionFunctionDeclaration(
                specification,
                Indexed(Variable("Cursors"), Variable("Fld")),
            ),
        ],
    )


def create_structure(prefix: str, message: Message) -> UnitPart:
    if not message.is_definite:
        return UnitPart()

    unit = UnitPart()
    unit += _create_structure_type(prefix, message)
    unit += _create_valid_structure_function(prefix, message)
    unit += _create_to_structure_procedure(prefix, message)
    unit += _create_sufficient_buffer_length_function(prefix, message)
    unit += _create_to_context_procedure(prefix, message)
    for field in message.fields:
        unit += _create_structure_field_size_function(message, field)
    return unit


def _create_structure_type(prefix: str, message: Message) -> UnitPart:
    assert len(message.paths(FINAL)) == 1

    components = []

    for path in message.paths(FINAL):
        if any(isinstance(t, Opaque) for t in message.field_types.values()):
            field_size = message.max_field_sizes()

        for link in path:
            if link.target == FINAL:
                continue

            type_ = message.field_types[link.target]

            component_type: Union[ID, Expr]

            if isinstance(type_, Scalar):
                component_type = common.prefixed_type_identifier(type_.identifier, prefix)
            elif isinstance(type_, Opaque):
                component_type = Indexed(
                    Variable(const.TYPES_BYTES),
                    ValueRange(
                        First(const.TYPES_INDEX),
                        Add(
                            First(const.TYPES_INDEX),
                            expr.Sub(
                                expr.Div(field_size[link.target], expr.Number(8)),
                                expr.Number(1),
                            )
                            .simplified()
                            .ada_expr(),
                        ),
                    ),
                )
            else:
                assert False

            components.append(Component(link.target.identifier, component_type))

    return UnitPart(
        [
            RecordType("Structure", components),
        ],
    )


def _create_valid_structure_function(prefix: str, message: Message) -> UnitPart:
    assert len(message.paths(FINAL)) == 1

    valid_values = [
        Call(
            prefix * t.package * f"Valid_{t.name}",
            [
                Variable("Struct" * f.identifier),
            ],
        )
        for f, t in message.types.items()
        if isinstance(t, Enumeration) and t.always_valid
    ]
    conditions = [
        condition
        for path in message.paths(FINAL)
        for link in path
        for condition in [
            link.condition.substituted(
                # Eng/RecordFlux/RecordFlux#276
                mapping={expr.ValidChecksum(f): expr.TRUE for f in message.checksums},
            )
            .substituted(_struct_substitution(message))
            .substituted(common.substitution(message, prefix))
            .simplified()
            .ada_expr(),
        ]
        if condition != TRUE
    ]

    specification = FunctionSpecification(
        "Valid_Structure",
        "Boolean",
        [
            Parameter(
                ["Struct" if bool([*valid_values, *conditions]) else "Unused_Struct"],
                "Structure",
            ),
        ],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                AndThen(*[*valid_values, *conditions]),
            ),
        ],
    )


def _create_sufficient_buffer_length_function(prefix: str, message: Message) -> UnitPart:
    assert len(message.paths(FINAL)) == 1

    specification = FunctionSpecification(
        "Sufficient_Buffer_Length",
        "Boolean",
        [
            Parameter(["Ctx"], "Context"),
            Parameter(["Struct" if not message.has_fixed_size else "Unused_Struct"], "Structure"),
        ],
    )
    message_size = message.size()

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                GreaterEqual(
                    Call(
                        const.TYPES_BASE_INT,
                        [
                            Add(
                                Call(
                                    const.TYPES * "To_Last_Bit_Index",
                                    [Variable("Ctx.Buffer_Last")],
                                ),
                                -Call(
                                    const.TYPES * "To_First_Bit_Index",
                                    [Variable("Ctx.Buffer_First")],
                                ),
                                Number(1),
                            ),
                        ],
                    ),
                    message_size.substituted(_struct_substitution(message))
                    .substituted(common.substitution(message, prefix))
                    .simplified()
                    .ada_expr(),
                ),
            ),
        ],
    )


def _create_to_structure_procedure(prefix: str, message: Message) -> UnitPart:
    assert len(message.paths(FINAL)) == 1

    statements: list[Statement] = []

    for path in message.paths(FINAL):
        for link in path:
            if link.target == FINAL:
                continue

            type_ = message.field_types[link.target]

            if isinstance(type_, Scalar):
                statements.append(
                    Assignment(
                        Variable(f"Struct.{link.target.identifier}"),
                        Call(f"Get_{link.target.identifier}", [Variable("Ctx")]),
                    ),
                )
            elif isinstance(type_, Opaque):
                statements.extend(
                    [
                        Assignment(
                            Variable(f"Struct.{link.target.identifier}"),
                            NamedAggregate(
                                (
                                    "others",
                                    Number(0),
                                ),
                            ),
                        ),
                        CallStatement(
                            f"Get_{link.target.identifier}",
                            [
                                Variable("Ctx"),
                                Slice(
                                    Variable(f"Struct.{link.target.identifier}"),
                                    First(f"Struct.{link.target.identifier}"),
                                    Add(
                                        First(f"Struct.{link.target.identifier}"),
                                        Call(
                                            const.TYPES_INDEX,
                                            [
                                                Add(
                                                    Call(
                                                        const.TYPES_TO_LENGTH,
                                                        [
                                                            Call(
                                                                "Field_Size",
                                                                [
                                                                    Variable("Ctx"),
                                                                    Variable(
                                                                        link.target.affixed_name,
                                                                    ),
                                                                ],
                                                            ),
                                                        ],
                                                    ),
                                                    Number(1),
                                                ),
                                            ],
                                        ),
                                        -Number(2),
                                    ),
                                ),
                            ],
                        ),
                    ],
                )
            else:
                assert False

    specification = ProcedureSpecification(
        "To_Structure",
        [Parameter(["Ctx"], "Context"), OutParameter(["Struct"], "Structure")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Well_Formed_Message",
                                [Variable("Ctx")],
                            ),
                        ),
                    ),
                    Postcondition(
                        Call("Valid_Structure", [Variable("Struct")]),
                    ),
                ],
            ),
        ],
        [
            SubprogramBody(specification, [], statements),
        ],
    )


def _struct_substitution(
    message: Message,
) -> abc.Callable[[expr.Expr], expr.Expr]:
    def func(expression: expr.Expr) -> expr.Expr:
        if (
            isinstance(expression, expr.Size)
            and isinstance(expression.prefix, expr.Variable)
            and Field(expression.prefix.identifier) in message.fields
        ):
            return expr.Size(expr.Variable("Struct" * expression.prefix.identifier))

        if isinstance(expression, expr.Variable) and Field(expression.identifier) in message.fields:
            field_type = message.types[Field(expression.identifier)]

            if isinstance(field_type, Enumeration):
                return expr.Call(
                    "To_Base_Integer",
                    [expr.Variable("Struct" * expression.identifier)],
                )

            if isinstance(field_type, Scalar):
                return expr.Call(
                    const.TYPES_BASE_INT,
                    [expr.Variable("Struct" * expression.identifier)],
                )

            return expr.Variable("Struct" * expression.identifier)

        return expression

    return func


def _create_to_context_procedure(prefix: str, message: Message) -> UnitPart:
    assert len(message.paths(FINAL)) == 1

    body: list[Statement] = [CallStatement("Reset", [Variable("Ctx")])]
    statements = body

    for path in message.paths(FINAL):
        for link in path:
            if link.target == FINAL:
                continue

            type_ = message.field_types[link.target]

            if isinstance(type_, (Scalar, Opaque)):
                if isinstance(type_, Opaque) and link.size.variables():
                    size = (
                        link.size.substituted(
                            lambda x: expr.Size(expr.Variable("Struct" * x.prefix.identifier))
                            if isinstance(x, expr.Size)
                            and isinstance(x.prefix, expr.Variable)
                            and Field(x.prefix.identifier) in message.fields
                            else x,
                        )
                        .substituted(
                            lambda x: expr.Call(
                                const.TYPES_BIT_LENGTH,
                                [expr.Variable("Struct" * x.identifier)],
                            )
                            if isinstance(x, expr.Variable)
                            and Field(x.identifier) in message.fields
                            else x,
                        )
                        .ada_expr()
                    )
                    statements.append(
                        CallStatement(
                            f"Set_{link.target.identifier}",
                            [
                                Variable("Ctx"),
                                Slice(
                                    Variable(f"Struct.{link.target.identifier}"),
                                    First(f"Struct.{link.target.identifier}"),
                                    Add(
                                        First(f"Struct.{link.target.identifier}"),
                                        Call(
                                            const.TYPES_INDEX,
                                            [
                                                Add(
                                                    Call(const.TYPES_TO_LENGTH, [size]),
                                                    Number(1),
                                                ),
                                            ],
                                        ),
                                        Number(-2),
                                    ),
                                ),
                            ],
                        ),
                    )
                else:
                    set_field = CallStatement(
                        f"Set_{link.target.identifier}",
                        [Variable("Ctx"), Variable(f"Struct.{link.target.identifier}")],
                    )
                    statements.append(set_field)
            else:
                assert False

    specification = ProcedureSpecification(
        "To_Context",
        [Parameter(["Struct"], "Structure"), InOutParameter(["Ctx"], "Context")],
    )

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [
                    Precondition(
                        AndThen(
                            Not(Constrained("Ctx")),
                            Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")]),
                            Call(
                                prefix * message.identifier * "Valid_Structure",
                                [Variable("Struct")],
                            ),
                            Call(
                                prefix * message.identifier * "Sufficient_Buffer_Length",
                                [Variable("Ctx"), Variable("Struct")],
                            ),
                        ),
                    ),
                    Postcondition(
                        And(
                            Call("Has_Buffer", [Variable("Ctx")]),
                            Call("Well_Formed_Message", [Variable("Ctx")]),
                            *[
                                Equal(e, Old(e))
                                for e in [
                                    Selected(Variable("Ctx"), "Buffer_First"),
                                    Selected(Variable("Ctx"), "Buffer_Last"),
                                ]
                            ],
                        ),
                    ),
                ],
            ),
        ],
        [
            SubprogramBody(specification, [], body),
        ],
    )


def _create_structure_field_size_function(message: Message, field: Field) -> UnitPart:
    field_type = message.field_types[field]
    specification = FunctionSpecification(
        f"Field_Size_{field.identifier}",
        const.TYPES_BIT_LENGTH,
        [Parameter(["Struct"], "Structure")],
    )
    if isinstance(field_type, Scalar):
        expression = field_type.size.ada_expr()
    else:
        assert isinstance(field_type, Opaque)

        def substitute(expression: expr.Expr) -> expr.Expr:
            if (
                isinstance(expression, expr.Size)
                and isinstance(expression.prefix, expr.Variable)
                and Field(expression.prefix.identifier) in message.fields
            ):
                return expr.Call(
                    f"Field_Size_{expression.prefix.identifier}",
                    [expr.Variable("Struct")],
                )
            if (
                isinstance(expression, expr.Variable)
                and Field(expression.identifier) in message.fields
            ):
                return expr.Call(
                    const.TYPES_BIT_LENGTH,
                    [
                        expr.Selected(
                            expr.Variable("Struct"),
                            expression.identifier,
                            type_=expression.type_,
                        ),
                    ],
                )
            return expression

        links = message.incoming(field)
        assert len(links) == 1
        expression = links[0].size.substituted(substitute).simplified().ada_expr()

    return UnitPart(
        [
            SubprogramDeclaration(
                specification,
                [Precondition(Call("Valid_Structure", [Variable("Struct")]))],
            ),
        ],
        private=[
            ExpressionFunctionDeclaration(
                specification,
                expression,
            ),
        ],
    )


def _switch_update_conditions(prefix: str, message: Message, field: Field) -> abc.Sequence[Expr]:
    return [
        Not(Call(prefix * message.identifier * "Has_Buffer", [Variable("Ctx")])),
        Call(prefix * common.sequence_name(message, field) * "Has_Buffer", [Variable("Seq_Ctx")]),
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
