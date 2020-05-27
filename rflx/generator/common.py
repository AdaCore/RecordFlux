from typing import Callable, Mapping, Sequence

from rflx.ada import (
    Assignment,
    CallStatement,
    Declaration,
    ExpressionFunctionDeclaration,
    FunctionSpecification,
    ObjectDeclaration,
    PragmaStatement,
    Statement,
)
from rflx.expression import (
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    AndThen,
    Call,
    Div,
    Equal,
    Expr,
    First,
    ForAllIn,
    GreaterEqual,
    If,
    Indexed,
    Last,
    Length,
    LessEqual,
    Mod,
    Name,
    NamedAggregate,
    NotEqual,
    Number,
    Or,
    Selected,
    Size,
    Sub,
    Val,
    ValueRange,
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
    Link,
    Message,
    ModularInteger,
    Scalar,
    Type,
    is_builtin_type,
)

from . import const


def substitution(
    message: Message, embedded: bool = False, public: bool = False
) -> Callable[[Expr], Expr]:
    facts = substitution_facts(message, embedded, public)

    def func(expression: Expr) -> Expr:
        def byte_aggregate(aggregate: Aggregate) -> Aggregate:
            return Aggregate(*[Val(const.TYPES_BYTE, e) for e in aggregate.elements])

        if isinstance(expression, Name) and expression in facts:
            return facts[expression]

        if isinstance(expression, (Equal, NotEqual)):
            field = None
            aggregate = None
            if isinstance(expression.left, Variable) and isinstance(expression.right, Aggregate):
                field = expression.left
                aggregate = byte_aggregate(expression.right)
            elif isinstance(expression.left, Aggregate) and isinstance(expression.right, Variable):
                field = expression.right
                aggregate = byte_aggregate(expression.left)
            if field and aggregate:
                if embedded:
                    return Equal(
                        Indexed(
                            Variable("Buffer"),
                            ValueRange(
                                Call(
                                    const.TYPES_BYTE_INDEX,
                                    [
                                        Selected(
                                            Indexed(
                                                Variable("Cursors"),
                                                Variable(Field(field.name).affixed_name),
                                            ),
                                            "First",
                                        )
                                    ],
                                ),
                                Call(
                                    const.TYPES_BYTE_INDEX,
                                    [
                                        Selected(
                                            Indexed(
                                                Variable("Cursors"),
                                                Variable(Field(field.name).affixed_name),
                                            ),
                                            "Last",
                                        )
                                    ],
                                ),
                            ),
                        ),
                        aggregate,
                    )
                return Call(
                    "Equal", [Variable("Ctx"), Variable(Field(field.name).affixed_name), aggregate]
                )

        return expression

    return func


def substitution_facts(
    message: Message, embedded: bool = False, public: bool = False
) -> Mapping[Name, Expr]:
    def prefixed(name: str) -> Expr:
        return Variable(f"Ctx.{name}") if not embedded else Variable(name)

    first = prefixed("First")
    last = prefixed("Last")
    cursors = prefixed("Cursors")

    def field_first(field: Field) -> Expr:
        if public:
            return Call("Field_First", [Variable("Ctx"), Variable(field.affixed_name)])
        return Selected(Indexed(cursors, Variable(field.affixed_name)), "First")

    def field_last(field: Field) -> Expr:
        if public:
            return Call("Field_Last", [Variable("Ctx"), Variable(field.affixed_name)])
        return Selected(Indexed(cursors, Variable(field.affixed_name)), "Last")

    def field_length(field: Field) -> Expr:
        if public:
            return Call("Field_Length", [Variable("Ctx"), Variable(field.affixed_name)])
        return Add(
            Sub(
                Selected(Indexed(cursors, Variable(field.affixed_name)), "Last"),
                Selected(Indexed(cursors, Variable(field.affixed_name)), "First"),
            ),
            Number(1),
        )

    def field_value(field: Field, field_type: Type) -> Expr:
        if isinstance(field_type, Enumeration):
            if public:
                return Call(
                    const.TYPES_BIT_LENGTH,
                    [Call("To_Base", [Call(f"Get_{field.name}", [Variable("Ctx")])])],
                )
            return Call(
                const.TYPES_BIT_LENGTH,
                [
                    Selected(
                        Indexed(cursors, Variable(field.affixed_name)), f"Value.{field.name}_Value"
                    )
                ],
            )
        if isinstance(field_type, Scalar):
            if public:
                return Call(const.TYPES_BIT_LENGTH, [Call(f"Get_{field.name}", [Variable("Ctx")])])
            return Call(
                const.TYPES_BIT_LENGTH,
                [
                    Selected(
                        Indexed(cursors, Variable(field.affixed_name)), f"Value.{field.name}_Value"
                    )
                ],
            )
        if isinstance(field_type, Composite):
            return Variable(field.name)
        assert False, f'unexpected type "{type(field_type).__name__}"'
        return UNDEFINED

    return {
        **{First("Message"): first},
        **{Last("Message"): last},
        **{Length("Message"): Add(last, -first, Number(1))},
        **{First(f.name): field_first(f) for f in message.fields},
        **{Last(f.name): field_last(f) for f in message.fields},
        **{Length(f.name): field_length(f) for f in message.fields},
        **{Variable(f.name): field_value(f, t) for f, t in message.types.items()},
        **{
            Variable(l): Call(const.TYPES_BIT_LENGTH, [Call("To_Base", [Variable(l)])])
            for t in message.types.values()
            if isinstance(t, Enumeration)
            for l in t.literals.keys()
        },
        **{
            Variable(t.package * l): Call(
                const.TYPES_BIT_LENGTH, [Call("To_Base", [Variable(t.package * l)])]
            )
            for t in message.types.values()
            if isinstance(t, Enumeration)
            for l in t.literals.keys()
        },
    }


def message_structure_invariant(
    message: Message, prefix: str, link: Link = None, embedded: bool = False
) -> Expr:
    def prefixed(name: str) -> Expr:
        return Selected(Variable("Ctx"), name) if not embedded else Variable(name)

    if not link:
        return message_structure_invariant(message, prefix, message.outgoing(INITIAL)[0], embedded)

    source = link.source
    target = link.target

    if target is FINAL:
        return TRUE

    field_type = message.types[target]
    condition = link.condition.substituted(substitution(message, embedded)).simplified()
    length = (
        Size(prefix * full_base_type_name(field_type))
        if isinstance(field_type, Scalar)
        else link.length.substituted(substitution(message, embedded)).simplified()
    )
    first = (
        prefixed("First")
        if source == INITIAL
        else link.first.substituted(substitution(message, embedded))
        .substituted(
            mapping={
                UNDEFINED: Add(
                    Selected(Indexed(prefixed("Cursors"), Variable(source.affixed_name)), "Last"),
                    Number(1),
                )
            }
        )
        .simplified()
    )

    return If(
        [
            (
                AndThen(
                    Call(
                        "Structural_Valid",
                        [Indexed(prefixed("Cursors"), Variable(target.affixed_name))],
                    ),
                    condition,
                ),
                AndThen(
                    Equal(
                        Add(
                            Sub(
                                Selected(
                                    Indexed(prefixed("Cursors"), Variable(target.affixed_name)),
                                    "Last",
                                ),
                                Selected(
                                    Indexed(prefixed("Cursors"), Variable(target.affixed_name)),
                                    "First",
                                ),
                            ),
                            Number(1),
                        ),
                        length,
                    ),
                    Equal(
                        Selected(
                            Indexed(prefixed("Cursors"), Variable(target.affixed_name)),
                            "Predecessor",
                        ),
                        Variable(source.affixed_name),
                    ),
                    Equal(
                        Selected(
                            Indexed(prefixed("Cursors"), Variable(target.affixed_name)), "First"
                        ),
                        first,
                    ),
                    *[
                        message_structure_invariant(message, prefix, l, embedded)
                        for l in message.outgoing(target)
                    ],
                ),
            )
        ]
    ).simplified()


def context_predicate(message: Message, composite_fields: Sequence[Field], prefix: str) -> Expr:
    def valid_predecessors_invariant() -> Expr:
        return AndThen(
            *[
                If(
                    [
                        (
                            Call(
                                "Structural_Valid",
                                [Indexed(Variable("Cursors"), Variable(f.affixed_name))],
                            ),
                            Or(
                                *[
                                    AndThen(
                                        Call(
                                            "Structural_Valid"
                                            if l.source in composite_fields
                                            else "Valid",
                                            [
                                                Indexed(
                                                    Variable("Cursors"),
                                                    Variable(l.source.affixed_name),
                                                )
                                            ],
                                        ),
                                        Equal(
                                            Selected(
                                                Indexed(
                                                    Variable("Cursors"), Variable(f.affixed_name),
                                                ),
                                                "Predecessor",
                                            ),
                                            Variable(l.source.affixed_name),
                                        ),
                                        l.condition,
                                    )
                                    .substituted(substitution(message, embedded=True))
                                    .simplified()
                                    for l in message.incoming(f)
                                ]
                            ),
                        )
                    ]
                )
                for f in message.fields
                if f not in message.direct_successors(INITIAL)
            ]
        )

    def invalid_successors_invariant() -> Expr:
        return AndThen(
            *[
                If(
                    [
                        (
                            AndThen(
                                *[
                                    Call(
                                        "Invalid",
                                        [Indexed(Variable("Cursors"), Variable(p.affixed_name))],
                                    )
                                    for p in message.direct_predecessors(f)
                                ]
                            ),
                            Call(
                                "Invalid", [Indexed(Variable("Cursors"), Variable(f.affixed_name))],
                            ),
                        )
                    ]
                )
                for f in message.fields
                if f not in message.direct_successors(INITIAL)
            ]
        )

    return AndThen(
        If(
            [
                (
                    NotEqual(Variable("Buffer"), Variable("null")),
                    And(
                        Equal(First("Buffer"), Variable("Buffer_First")),
                        Equal(Last("Buffer"), Variable("Buffer_Last")),
                    ),
                )
            ]
        ),
        public_context_predicate(),
        ForAllIn(
            "F",
            ValueRange(First("Field"), Last("Field")),
            If(
                [
                    (
                        Call("Structural_Valid", [Indexed(Variable("Cursors"), Variable("F"))]),
                        And(
                            GreaterEqual(
                                Selected(Indexed(Variable("Cursors"), Variable("F")), "First"),
                                Variable("First"),
                            ),
                            LessEqual(
                                Selected(Indexed(Variable("Cursors"), Variable("F")), "Last"),
                                Variable("Last"),
                            ),
                            LessEqual(
                                Selected(Indexed(Variable("Cursors"), Variable("F")), "First"),
                                Add(
                                    Selected(Indexed(Variable("Cursors"), Variable("F")), "Last"),
                                    Number(1),
                                ),
                            ),
                            Equal(
                                Selected(
                                    Selected(Indexed(Variable("Cursors"), Variable("F")), "Value"),
                                    "Fld",
                                ),
                                Variable("F"),
                            ),
                        ),
                    )
                ]
            ),
        ),
        valid_predecessors_invariant(),
        invalid_successors_invariant(),
        message_structure_invariant(message, prefix, embedded=True),
    )


def public_context_predicate() -> Expr:
    return And(
        GreaterEqual(Call(const.TYPES_BYTE_INDEX, [Variable("First")]), Variable("Buffer_First")),
        LessEqual(Call(const.TYPES_BYTE_INDEX, [Variable("Last")]), Variable("Buffer_Last")),
        LessEqual(Variable("First"), Variable("Last")),
        LessEqual(Variable("Last"), Div(Last(const.TYPES_BIT_INDEX), Number(2))),
    )


def valid_path_to_next_field_condition(message: Message, field: Field) -> Sequence[Expr]:
    return [
        If(
            [
                (
                    l.condition,
                    And(
                        Equal(
                            Call(
                                "Predecessor", [Variable("Ctx"), Variable(l.target.affixed_name)],
                            ),
                            Variable(field.affixed_name),
                        ),
                        Call("Valid_Next", [Variable("Ctx"), Variable(l.target.affixed_name)])
                        if l.target != FINAL
                        else TRUE,
                    ),
                )
            ]
        )
        .substituted(substitution(message, public=True))
        .simplified()
        for l in message.outgoing(field)
        if l.target != FINAL
    ]


def sufficient_space_for_field_condition(field_name: Name) -> Expr:
    return GreaterEqual(
        Call("Available_Space", [Variable("Ctx"), field_name]),
        Call("Field_Length", [Variable("Ctx"), field_name]),
    )


def initialize_field_statements(message: Message, field: Field, prefix: str) -> Sequence[Statement]:
    return [
        CallStatement("Reset_Dependent_Fields", [Variable("Ctx"), Variable(field.affixed_name)],),
        Assignment(
            "Ctx",
            Aggregate(
                Variable("Ctx.Buffer_First"),
                Variable("Ctx.Buffer_Last"),
                Variable("Ctx.First"),
                Variable("Last"),
                Variable("Ctx.Buffer"),
                Variable("Ctx.Cursors"),
            ),
        ),
        # WORKAROUND:
        # Limitation of GNAT Community 2019 / SPARK Pro 20.0
        # Provability of predicate is increased by adding part of
        # predicate as assert
        PragmaStatement("Assert", [str(message_structure_invariant(message, prefix))],),
        Assignment(
            Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)),
            NamedAggregate(
                ("State", Variable("S_Structural_Valid")),
                ("First", Variable("First")),
                ("Last", Variable("Last")),
                ("Value", NamedAggregate(("Fld", Variable(field.affixed_name))),),
                (
                    "Predecessor",
                    Selected(
                        Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name),),
                        "Predecessor",
                    ),
                ),
            ),
        ),
        Assignment(
            Indexed(
                Variable("Ctx.Cursors"),
                Call("Successor", [Variable("Ctx"), Variable(field.affixed_name)]),
            ),
            NamedAggregate(
                ("State", Variable("S_Invalid")), ("Predecessor", Variable(field.affixed_name)),
            ),
        ),
    ]


def field_bit_location_declarations(field_name: Name) -> Sequence[Declaration]:
    return [
        ObjectDeclaration(
            ["First"],
            const.TYPES_BIT_INDEX,
            Call("Field_First", [Variable("Ctx"), field_name]),
            True,
        ),
        ObjectDeclaration(
            ["Last"],
            const.TYPES_BIT_INDEX,
            Call("Field_Last", [Variable("Ctx"), field_name]),
            True,
        ),
    ]


def field_byte_location_declarations() -> Sequence[Declaration]:
    return [
        ExpressionFunctionDeclaration(
            FunctionSpecification("Buffer_First", const.TYPES_INDEX),
            Call(const.TYPES_BYTE_INDEX, [Variable("First")]),
        ),
        ExpressionFunctionDeclaration(
            FunctionSpecification("Buffer_Last", const.TYPES_INDEX),
            Call(const.TYPES_BYTE_INDEX, [Variable("Last")]),
        ),
        ExpressionFunctionDeclaration(
            FunctionSpecification("Offset", const.TYPES_OFFSET),
            Call(
                const.TYPES_OFFSET,
                [Mod(Sub(Number(8), Mod(Variable("Last"), Number(8))), Number(8))],
            ),
        ),
    ]


def prefixed_type_name(type_name: ID, prefix: str) -> ID:
    if is_builtin_type(type_name):
        return type_name

    return prefix * type_name


def base_type_name(scalar_type: Scalar) -> ID:
    if isinstance(scalar_type, ModularInteger):
        return ID(scalar_type.name)

    return ID(f"{scalar_type.name}_Base")


def full_base_type_name(scalar_type: Scalar) -> ID:
    if scalar_type.package == BUILTINS_PACKAGE:
        return const.BUILTIN_TYPES_PACKAGE * scalar_type.name + "_Base"

    if isinstance(scalar_type, ModularInteger):
        return scalar_type.identifier

    return ID(f"{scalar_type.full_name}_Base")


def enum_name(enum_type: Enumeration) -> ID:
    return ID(f"{enum_type.name}_Enum")


def full_enum_name(enum_type: Enumeration) -> ID:
    return ID(f"{enum_type.full_name}_Enum")


def sequence_name(message: Message, field: Field) -> ID:
    return ID(f"{message.types[field].name}_Sequence")


def length_dependent_condition(message: Message) -> bool:
    return any(
        [
            isinstance(v.name, str) and v.name.endswith("'Length")
            for l in message.structure
            for v in l.condition.variables(True)
        ]
    )
