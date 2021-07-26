from typing import Callable, List, Mapping, Optional, Sequence, Tuple

from rflx import ada, expression as expr, identifier as rid, model
from rflx.const import BUILTINS_PACKAGE

from . import const


def substitution(
    message: model.Message,
    prefix: str,
    embedded: bool = False,
    public: bool = False,
    target_type: Optional[ada.ID] = const.TYPES_U64,
) -> Callable[[expr.Expr], expr.Expr]:
    facts = substitution_facts(message, prefix, embedded, public, target_type)

    def func(expression: expr.Expr) -> expr.Expr:
        def byte_aggregate(aggregate: expr.Aggregate) -> expr.Aggregate:
            return expr.Aggregate(*[expr.Val(const.TYPES_BYTE, e) for e in aggregate.elements])

        if isinstance(expression, expr.Name) and expression in facts:
            return facts[expression]

        if isinstance(expression, expr.String):
            return expr.Aggregate(*expression.elements)

        if isinstance(expression, (expr.Equal, expr.NotEqual)):
            field = None
            aggregate = None
            if isinstance(expression.left, expr.Variable) and isinstance(
                expression.right, expr.Aggregate
            ):
                field = model.Field(expression.left.name)
                aggregate = byte_aggregate(expression.right)
            elif isinstance(expression.left, expr.Aggregate) and isinstance(
                expression.right, expr.Variable
            ):
                field = model.Field(expression.right.name)
                aggregate = byte_aggregate(expression.left)
            if field and len(field.identifier.parts) == 1 and aggregate:
                assert field in message.fields
                if embedded:
                    return expression.__class__(
                        expr.Indexed(
                            expr.Variable(expr.ID("Buffer") * "all"),
                            expr.ValueRange(
                                expr.Call(
                                    const.TYPES_TO_INDEX,
                                    [
                                        expr.Selected(
                                            expr.Indexed(
                                                expr.Variable("Cursors"),
                                                expr.Variable(field.affixed_name),
                                            ),
                                            "First",
                                        )
                                    ],
                                ),
                                expr.Call(
                                    const.TYPES_TO_INDEX,
                                    [
                                        expr.Selected(
                                            expr.Indexed(
                                                expr.Variable("Cursors"),
                                                expr.Variable(field.affixed_name),
                                            ),
                                            "Last",
                                        )
                                    ],
                                ),
                            ),
                        ),
                        aggregate,
                    )
                equal_call = expr.Call(
                    "Equal",
                    [expr.Variable("Ctx"), expr.Variable(field.affixed_name), aggregate],
                )
                return equal_call if isinstance(expression, expr.Equal) else expr.Not(equal_call)

        def field_value(field: model.Field) -> expr.Expr:
            if public:
                return expr.Call(f"Get_{field.name}", [expr.Variable("Ctx")])
            return expr.Selected(
                expr.Indexed(
                    expr.Variable(expr.ID("Ctx") * "Cursors" if not embedded else "Cursors"),
                    expr.Variable(field.affixed_name),
                ),
                expr.ID("Value") * f"{field.name}_Value",
            )

        if isinstance(expression, expr.Relation):
            if (
                isinstance(expression.left, expr.Variable)
                and model.Field(expression.left.name) in message.fields
                and isinstance(expression.right, expr.Number)
            ):
                return expression.__class__(
                    field_value(model.Field(expression.left.name)), expression.right
                )
            if (
                isinstance(expression.right, expr.Variable)
                and model.Field(expression.right.name) in message.fields
                and isinstance(expression.left, expr.Number)
            ):
                return expression.__class__(
                    expression.left, field_value(model.Field(expression.right.name))
                )

        return expression

    return func


def substitution_facts(
    message: model.Message,
    prefix: str,
    embedded: bool = False,
    public: bool = False,
    target_type: Optional[ada.ID] = const.TYPES_U64,
) -> Mapping[expr.Name, expr.Expr]:
    def prefixed(name: str) -> expr.Expr:
        return expr.Variable(expr.ID("Ctx") * name) if not embedded else expr.Variable(name)

    first = prefixed("First")
    last = prefixed("Last")
    cursors = prefixed("Cursors")

    def field_first(field: model.Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_First", [expr.Variable("Ctx"), expr.Variable(field.affixed_name)]
            )
        return expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "First")

    def field_last(field: model.Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_Last", [expr.Variable("Ctx"), expr.Variable(field.affixed_name)]
            )
        return expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "Last")

    def field_size(field: model.Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_Size", [expr.Variable("Ctx"), expr.Variable(field.affixed_name)]
            )
        return expr.Add(
            expr.Sub(
                expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "Last"),
                expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "First"),
            ),
            expr.Number(1),
        )

    def field_value(field: model.Field, field_type: model.Type) -> expr.Expr:
        if isinstance(field_type, model.Enumeration):
            if public:
                return expr.Call(
                    "To_Base", [expr.Call(f"Get_{field.name}", [expr.Variable("Ctx")])]
                )
            return expr.Selected(
                expr.Indexed(cursors, expr.Variable(field.affixed_name)),
                expr.ID("Value") * f"{field.name}_Value",
            )
        if isinstance(field_type, model.Scalar):
            if public:
                return expr.Call(f"Get_{field.name}", [expr.Variable("Ctx")])
            return expr.Selected(
                expr.Indexed(cursors, expr.Variable(field.affixed_name)),
                expr.ID("Value") * f"{field.name}_Value",
            )
        if isinstance(field_type, model.Composite):
            return expr.Variable(field.name)

        assert False, f'unexpected type "{type(field_type).__name__}"'

    def type_conversion(expression: expr.Expr) -> expr.Expr:
        return expr.Call(target_type, [expression]) if target_type else expression

    return {
        **{expr.First("Message"): type_conversion(first)},
        **{expr.Last("Message"): type_conversion(last)},
        **{expr.Size("Message"): type_conversion(expr.Add(last, -first, expr.Number(1)))},
        **{expr.First(f.name): type_conversion(field_first(f)) for f in message.fields},
        **{expr.Last(f.name): type_conversion(field_last(f)) for f in message.fields},
        **{expr.Size(f.name): type_conversion(field_size(f)) for f in message.fields},
        **{
            expr.Variable(f.name): type_conversion(field_value(f, t))
            for f, t in message.types.items()
        },
        **{
            expr.Variable(l): type_conversion(expr.Call("To_Base", [expr.Variable(l)]))
            for t in message.types.values()
            if isinstance(t, model.Enumeration)
            for l in t.literals.keys()
        },
        **{
            expr.Variable(t.package * l): type_conversion(
                expr.Call("To_Base", [expr.Variable(prefix * t.package * l)])
            )
            for t in message.types.values()
            if isinstance(t, model.Enumeration)
            for l in t.literals.keys()
        },
        # ISSUE: Componolit/RecordFlux#276
        **{expr.ValidChecksum(f): expr.TRUE for f in message.checksums},
    }


def message_structure_invariant(
    message: model.Message, prefix: str, link: model.Link = None, embedded: bool = False
) -> ada.Expr:
    def prefixed(name: str) -> expr.Expr:
        return expr.Selected(expr.Variable("Ctx"), name) if not embedded else expr.Variable(name)

    if not link:
        return message_structure_invariant(
            message, prefix, message.outgoing(model.INITIAL)[0], embedded
        )

    source = link.source
    target = link.target

    if target is model.FINAL:
        return ada.TRUE

    field_type = message.types[target]
    condition = link.condition.substituted(substitution(message, prefix, embedded)).simplified()
    size = (
        expr.Size(prefix * full_base_type_name(field_type))
        if isinstance(field_type, model.Scalar)
        else link.size.substituted(
            substitution(message, prefix, embedded, target_type=const.TYPES_BIT_LENGTH)
        ).simplified()
    )
    first = (
        prefixed("First")
        if source == model.INITIAL
        else link.first.substituted(
            substitution(message, prefix, embedded, target_type=const.TYPES_BIT_INDEX)
        )
        .substituted(
            mapping={
                expr.UNDEFINED: expr.Add(
                    expr.Selected(
                        expr.Indexed(prefixed("Cursors"), expr.Variable(source.affixed_name)),
                        "Last",
                    ),
                    expr.Number(1),
                )
            }
        )
        .simplified()
    )
    invariant = [
        message_structure_invariant(message, prefix, l, embedded) for l in message.outgoing(target)
    ]

    return ada.If(
        [
            (
                ada.AndThen(
                    ada.Call(
                        "Structural_Valid",
                        [
                            ada.Indexed(
                                prefixed("Cursors").ada_expr(), ada.Variable(target.affixed_name)
                            )
                        ],
                    ),
                    *([condition.ada_expr()] if condition != expr.TRUE else []),
                ),
                ada.AndThen(
                    ada.Equal(
                        ada.Add(
                            ada.Sub(
                                ada.Selected(
                                    ada.Indexed(
                                        prefixed("Cursors").ada_expr(),
                                        ada.Variable(target.affixed_name),
                                    ),
                                    "Last",
                                ),
                                ada.Selected(
                                    ada.Indexed(
                                        prefixed("Cursors").ada_expr(),
                                        ada.Variable(target.affixed_name),
                                    ),
                                    "First",
                                ),
                            ),
                            ada.Number(1),
                        ),
                        size.ada_expr(),
                    ),
                    ada.Equal(
                        ada.Selected(
                            ada.Indexed(
                                prefixed("Cursors").ada_expr(),
                                ada.Variable(target.affixed_name),
                            ),
                            "Predecessor",
                        ),
                        ada.Variable(source.affixed_name),
                    ),
                    ada.Equal(
                        ada.Selected(
                            ada.Indexed(
                                prefixed("Cursors").ada_expr(),
                                ada.Variable(target.affixed_name),
                            ),
                            "First",
                        ),
                        first.ada_expr(),
                    ),
                    *[i for i in invariant if i != ada.TRUE],
                ),
            )
        ]
    )


def context_predicate(
    message: model.Message, composite_fields: Sequence[model.Field], prefix: str
) -> ada.Expr:
    def valid_predecessors_invariant() -> ada.Expr:
        return ada.AndThen(
            *[
                ada.If(
                    [
                        (
                            ada.Call(
                                "Structural_Valid",
                                [
                                    ada.Indexed(
                                        ada.Variable("Cursors"),
                                        ada.Variable(f.affixed_name),
                                    )
                                ],
                            ),
                            ada.Or(
                                *[
                                    expr.AndThen(
                                        expr.Call(
                                            "Structural_Valid"
                                            if l.source in composite_fields
                                            else "Valid",
                                            [
                                                expr.Indexed(
                                                    expr.Variable("Cursors"),
                                                    expr.Variable(l.source.affixed_name),
                                                )
                                            ],
                                        ),
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Indexed(
                                                    expr.Variable("Cursors"),
                                                    expr.Variable(f.affixed_name),
                                                ),
                                                "Predecessor",
                                            ),
                                            expr.Variable(l.source.affixed_name),
                                        ),
                                        l.condition.substituted(
                                            substitution(message, embedded=True, prefix=prefix)
                                        ),
                                    )
                                    .simplified()
                                    .ada_expr()
                                    for l in message.incoming(f)
                                ]
                            ),
                        )
                    ]
                )
                for f in message.fields
                if f not in message.direct_successors(model.INITIAL)
            ]
        )

    def invalid_successors_invariant() -> ada.Expr:
        return ada.AndThen(
            *[
                ada.If(
                    [
                        (
                            ada.AndThen(
                                *[
                                    ada.Call(
                                        "Invalid",
                                        [
                                            ada.Indexed(
                                                ada.Variable("Cursors"),
                                                ada.Variable(p.affixed_name),
                                            )
                                        ],
                                    )
                                    for p in message.direct_predecessors(f)
                                ]
                            ),
                            ada.Call(
                                "Invalid",
                                [
                                    ada.Indexed(
                                        ada.Variable("Cursors"),
                                        ada.Variable(f.affixed_name),
                                    )
                                ],
                            ),
                        )
                    ]
                )
                for f in message.fields
                if f not in message.direct_successors(model.INITIAL)
            ]
        )

    return ada.AndThen(
        ada.If(
            [
                (
                    ada.NotEqual(ada.Variable("Buffer"), ada.Variable("null")),
                    ada.And(
                        ada.Equal(ada.First("Buffer"), ada.Variable("Buffer_First")),
                        ada.Equal(ada.Last("Buffer"), ada.Variable("Buffer_Last")),
                    ),
                )
            ]
        ),
        public_context_predicate(),
        ada.LessEqual(ada.Sub(ada.Variable("First"), ada.Number(1)), ada.Variable("Message_Last")),
        ada.LessEqual(ada.Variable("Message_Last"), ada.Variable("Last")),
        ada.Equal(ada.Mod(ada.Variable("First"), ada.Size(const.TYPES_BYTE)), ada.Number(1)),
        ada.Equal(ada.Mod(ada.Variable("Last"), ada.Size(const.TYPES_BYTE)), ada.Number(0)),
        ada.Equal(ada.Mod(ada.Variable("Message_Last"), ada.Size(const.TYPES_BYTE)), ada.Number(0)),
        ada.ForAllIn(
            "F",
            ada.ValueRange(ada.First("Field"), ada.Last("Field")),
            ada.If(
                [
                    (
                        ada.Call(
                            "Structural_Valid",
                            [ada.Indexed(ada.Variable("Cursors"), ada.Variable("F"))],
                        ),
                        ada.And(
                            ada.GreaterEqual(
                                ada.Selected(
                                    ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")), "First"
                                ),
                                ada.Variable("First"),
                            ),
                            ada.LessEqual(
                                ada.Selected(
                                    ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")), "Last"
                                ),
                                ada.Variable("Message_Last"),
                            ),
                            ada.LessEqual(
                                ada.Selected(
                                    ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")), "First"
                                ),
                                ada.Add(
                                    ada.Selected(
                                        ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")),
                                        "Last",
                                    ),
                                    ada.Number(1),
                                ),
                            ),
                            ada.Equal(
                                ada.Selected(
                                    ada.Selected(
                                        ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")),
                                        "Value",
                                    ),
                                    "Fld",
                                ),
                                ada.Variable("F"),
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


def public_context_predicate() -> ada.Expr:
    return ada.And(
        ada.GreaterEqual(
            ada.Call(const.TYPES_TO_INDEX, [ada.Variable("First")]), ada.Variable("Buffer_First")
        ),
        ada.LessEqual(
            ada.Call(const.TYPES_TO_INDEX, [ada.Variable("Last")]), ada.Variable("Buffer_Last")
        ),
        ada.Less(ada.Variable("Buffer_Last"), ada.Last(const.TYPES_INDEX)),
        ada.LessEqual(ada.Variable("First"), ada.Add(ada.Variable("Last"), ada.Number(1))),
        ada.Less(ada.Variable("Last"), ada.Last(const.TYPES_BIT_INDEX)),
        ada.Equal(ada.Mod(ada.Variable("First"), ada.Size(const.TYPES_BYTE)), ada.Number(1)),
        ada.Equal(ada.Mod(ada.Variable("Last"), ada.Size(const.TYPES_BYTE)), ada.Number(0)),
    )


def valid_path_to_next_field_condition(
    message: model.Message, field: model.Field, prefix: str
) -> Sequence[ada.Expr]:
    return [
        ada.If(
            [
                (
                    l.condition.substituted(substitution(message, public=True, prefix=prefix))
                    .simplified()
                    .ada_expr(),
                    ada.And(
                        ada.Equal(
                            ada.Call(
                                "Predecessor",
                                [ada.Variable("Ctx"), ada.Variable(l.target.affixed_name)],
                            ),
                            ada.Variable(field.affixed_name),
                        ),
                        ada.Call(
                            "Valid_Next",
                            [ada.Variable("Ctx"), ada.Variable(l.target.affixed_name)],
                        )
                        if l.target != model.FINAL
                        else ada.TRUE,
                    ),
                )
            ]
        )
        for l in message.outgoing(field)
        if l.target != model.FINAL
    ]


def sufficient_space_for_field_condition(field_name: ada.Name) -> ada.Expr:
    return ada.GreaterEqual(
        ada.Call("Available_Space", [ada.Variable("Ctx"), field_name]),
        ada.Call("Field_Size", [ada.Variable("Ctx"), field_name]),
    )


def initialize_field_statements(
    message: model.Message, field: model.Field
) -> Sequence[ada.Statement]:
    comparison_to_aggregate = is_compared_to_aggregate(field, message)

    if comparison_to_aggregate:
        size = message.field_size(field)
        assert isinstance(size, expr.Number)
        aggregate_size = int(size) // 8

    return [
        ada.CallStatement(
            "Reset_Dependent_Fields",
            [ada.Variable("Ctx"), ada.Variable(field.affixed_name)],
        ),
        ada.Assignment("Ctx.Message_Last", ada.Variable("Last")),
        ada.Assignment(
            ada.Indexed(ada.Variable("Ctx.Cursors"), ada.Variable(field.affixed_name)),
            ada.NamedAggregate(
                ("State", ada.Variable("S_Structural_Valid")),
                ("First", ada.Variable("First")),
                ("Last", ada.Variable("Last")),
                (
                    "Value",
                    ada.NamedAggregate(
                        ("Fld", ada.Variable(field.affixed_name)),
                        *(
                            [
                                (
                                    f"{field.identifier}_Value",
                                    ada.Aggregate(*([ada.Number(0)] * aggregate_size)),
                                )
                            ]
                            if comparison_to_aggregate
                            else []
                        ),
                    ),
                ),
                (
                    "Predecessor",
                    ada.Selected(
                        ada.Indexed(
                            ada.Variable("Ctx.Cursors"),
                            ada.Variable(field.affixed_name),
                        ),
                        "Predecessor",
                    ),
                ),
            ),
        ),
        ada.Assignment(
            ada.Indexed(
                ada.Variable("Ctx.Cursors"),
                ada.Call(
                    "Successor",
                    [ada.Variable("Ctx"), ada.Variable(field.affixed_name)],
                ),
            ),
            ada.NamedAggregate(
                ("State", ada.Variable("S_Invalid")),
                ("Predecessor", ada.Variable(field.affixed_name)),
            ),
        ),
    ]


def field_bit_location_declarations(field_name: ada.Name) -> Sequence[ada.Declaration]:
    return [
        ada.ObjectDeclaration(
            ["First"],
            const.TYPES_BIT_INDEX,
            ada.Call("Field_First", [ada.Variable("Ctx"), field_name]),
            constant=True,
        ),
        ada.ObjectDeclaration(
            ["Last"],
            const.TYPES_BIT_INDEX,
            ada.Call("Field_Last", [ada.Variable("Ctx"), field_name]),
            constant=True,
        ),
    ]


def field_byte_bounds_declarations() -> Sequence[ada.Declaration]:
    return [
        ada.ExpressionFunctionDeclaration(
            ada.FunctionSpecification("Buffer_First", const.TYPES_INDEX),
            ada.Call(const.TYPES_TO_INDEX, [ada.Variable("First")]),
        ),
        ada.ExpressionFunctionDeclaration(
            ada.FunctionSpecification("Buffer_Last", const.TYPES_INDEX),
            ada.Call(const.TYPES_TO_INDEX, [ada.Variable("Last")]),
        ),
    ]


def field_byte_location_declarations() -> Sequence[ada.Declaration]:
    return [
        *field_byte_bounds_declarations(),
        ada.ExpressionFunctionDeclaration(
            ada.FunctionSpecification("Offset", const.TYPES_OFFSET),
            ada.Call(
                const.TYPES_OFFSET,
                [
                    ada.Mod(
                        ada.Sub(ada.Number(8), ada.Mod(ada.Variable("Last"), ada.Number(8))),
                        ada.Number(8),
                    )
                ],
            ),
        ),
    ]


def field_condition_call(message: model.Message, field: model.Field, value: ada.Expr) -> ada.Expr:
    return ada.Call(
        "Field_Condition",
        [
            ada.Variable("Ctx"),
            ada.Call(
                f"Construct_{field.identifier}_Value", [value]
            )  # ISSUE: Componolit/Workarounds#35
            if is_compared_to_aggregate(field, message)
            else ada.NamedAggregate(
                ("Fld", ada.Variable(field.affixed_name)),
                *(
                    [(f"{field.identifier}_Value", value)]
                    if isinstance(message.types[field], model.Scalar)
                    else []
                ),
            ),
            *(
                [
                    ada.Call(
                        "Field_Size",
                        [
                            ada.Variable("Ctx"),
                            ada.Variable(field.affixed_name),
                        ],
                    )
                ]
                if size_dependent_condition(message)
                else []
            ),
        ],
    )


def prefixed_type_identifier(type_identifier: ada.ID, prefix: str) -> ada.ID:
    if model.is_builtin_type(type_identifier):
        return type_identifier.name

    return prefix * type_identifier


def base_type_name(scalar_type: model.Scalar) -> ada.ID:
    if isinstance(scalar_type, model.ModularInteger):
        return ada.ID(scalar_type.name)

    return ada.ID(scalar_type.name + "_Base")


def full_base_type_name(scalar_type: model.Scalar) -> ada.ID:
    if scalar_type.package == BUILTINS_PACKAGE:
        return const.BUILTIN_TYPES_PACKAGE * scalar_type.name + "_Base"

    if isinstance(scalar_type, model.ModularInteger):
        return ada.ID(scalar_type.identifier)

    return ada.ID(scalar_type.identifier + "_Base")


def enum_name(enum_type: model.Enumeration) -> ada.ID:
    return ada.ID(enum_type.name + "_Enum")


def full_enum_name(enum_type: model.Enumeration) -> ada.ID:
    return ada.ID(enum_type.identifier + "_Enum")


def sequence_name(message: model.Message, field: model.Field) -> ada.ID:
    return ada.ID(message.types[field].identifier)


def contains_function_name(
    refinement_package: rid.ID, pdu: rid.ID, sdu: rid.ID, field: rid.ID
) -> str:
    sdu_name = sdu.name if sdu.parent == refinement_package else sdu
    pdu_name = pdu.name if pdu.parent == refinement_package else pdu
    return f"{sdu_name.flat}_In_{pdu_name.flat}_{field}"


def size_dependent_condition(message: model.Message) -> bool:
    field_sizes = {expr.Size(f.name) for f in message.fields}
    return any(
        size in field_sizes
        for link in message.structure
        for size in link.condition.findall(lambda x: isinstance(x, expr.Size))
    )


def is_compared_to_aggregate(field: model.Field, message: model.Message) -> bool:
    return any(
        r
        for l in message.structure
        for r in l.condition.findall(lambda x: isinstance(x, (expr.Equal, expr.NotEqual)))
        if isinstance(r, (expr.Equal, expr.NotEqual))
        and any(r.findall(lambda x: isinstance(x, expr.Aggregate)))
        and (
            r.left == expr.Variable(field.identifier) or r.right == expr.Variable(field.identifier)
        )
    )


def create_sequence_instantiation(
    sequence_type: model.Sequence,
    prefix: str = "",
    flat: bool = False,
) -> Tuple[List[ada.ContextItem], ada.GenericPackageInstantiation]:
    element_type = sequence_type.element_type
    element_type_package = ada.ID(element_type.package.name)

    sequence_context: List[ada.ContextItem] = []
    sequence_package: ada.GenericPackageInstantiation
    if isinstance(element_type, model.Message):
        element_type_identifier = ada.ID(
            element_type.identifier.flat if flat else prefix * element_type.identifier
        )
        sequence_context = [
            ada.WithClause(prefix * const.MESSAGE_SEQUENCE_PACKAGE),
            *([] if flat else [ada.WithClause(element_type_identifier)]),
        ]
        sequence_package = ada.GenericPackageInstantiation(
            ada.ID(sequence_type.identifier.flat if flat else prefix * sequence_type.identifier),
            prefix * const.MESSAGE_SEQUENCE_PACKAGE,
            [
                element_type_identifier * "Context",
                element_type_identifier * "Initialize",
                element_type_identifier * "Take_Buffer",
                element_type_identifier * "Has_Buffer",
                element_type_identifier * "Message_Last",
                element_type_identifier * "Initialized",
                element_type_identifier * "Structural_Valid_Message",
            ],
        )
    elif isinstance(element_type, model.Scalar):
        element_type_identifier = ada.ID(prefix * element_type.identifier)
        sequence_context = [
            ada.WithClause(prefix * const.SCALAR_SEQUENCE_PACKAGE),
            ada.WithClause(prefix * element_type_package),
        ]
        sequence_package = ada.GenericPackageInstantiation(
            ada.ID(sequence_type.identifier.flat if flat else prefix * sequence_type.identifier),
            prefix * const.SCALAR_SEQUENCE_PACKAGE,
            [
                element_type_identifier,
                prefix
                * element_type_package
                * (
                    base_type_name(element_type)
                    if not isinstance(element_type, model.ModularInteger)
                    else element_type.name
                ),
                prefix * element_type_package * "Valid",
                prefix * element_type_package * "To_Actual",
                prefix * element_type_package * "To_Base",
            ],
        )
    else:
        assert False, 'unexpected element type "{type(element_type)}"'

    return (sequence_context, sequence_package)
