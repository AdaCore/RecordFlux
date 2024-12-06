from __future__ import annotations

import enum
import math
import textwrap
from collections.abc import Callable, Sequence
from dataclasses import dataclass

from rflx import expr, expr_conv, ir, model, ty
from rflx.ada import (
    TRUE,
    Add,
    And,
    AndThen,
    Assignment,
    Call,
    CallStatement,
    Comment,
    ContextItem,
    Declaration,
    Equal,
    Expr,
    First,
    ForAllIn,
    GenericPackageInstantiation,
    Greater,
    GreaterEqual,
    If,
    IfExpr,
    IfThenElse,
    Indexed,
    Last,
    Length,
    Less,
    LessEqual,
    LoopEntry,
    Max,
    Name,
    NamedAggregate,
    NotEqual,
    Number,
    ObjectDeclaration,
    Old,
    Parameter,
    Pragma,
    PragmaStatement,
    Rem,
    Selected,
    Size,
    Statement,
    String,
    Sub,
    Update,
    ValueRange,
    Variable,
    WithClause,
)
from rflx.const import BUILTINS_PACKAGE
from rflx.identifier import ID

from . import const

EMPTY_ARRAY = NamedAggregate((ValueRange(Number(1), Number(0)), Number(0)))


class Debug(enum.Enum):
    NONE = enum.auto()
    BUILTIN = enum.auto()
    EXTERNAL = enum.auto()


@dataclass(frozen=True, order=True)
class Message:
    identifier: ID
    type_identifier: ID
    parameters: tuple[ID, ...]

    @property
    def external_buffer_id(self) -> ID:
        return ID(f"B_{self.identifier}")

    @property
    def buffer_id(self) -> ID:
        return ID(f"{self.identifier}_Buffer")


def type_to_id(type_: ty.NamedType) -> ID:
    if type_.identifier.parent == BUILTINS_PACKAGE:
        return const.TYPES * type_.identifier.name

    return type_.identifier


def substitution(
    message: model.Message,
    embedded: bool = False,
    public: bool = False,
    target_type: ty.NamedType = ty.BASE_INTEGER,
) -> Callable[[expr.Expr], expr.Expr]:
    facts = substitution_facts(message, embedded, public, target_type)

    def func(
        expression: expr.Expr,
    ) -> expr.Expr:
        def byte_aggregate(aggregate: expr.Aggregate) -> expr.Aggregate:
            return expr.Aggregate(*[expr.Val(const.TYPES_BYTE, e) for e in aggregate.elements])

        if isinstance(expression, expr.Name) and expression in facts:
            return facts[expression]

        if isinstance(expression, expr.String):
            return expr.Aggregate(*expression.elements)

        if isinstance(expression, expr.And):
            return expr.AndThen(*[func(t) for t in expression.terms])

        if isinstance(expression, expr.Or):
            return expr.OrElse(*[func(t) for t in expression.terms])

        if isinstance(expression, (expr.Equal, expr.NotEqual)):
            field = None
            aggregate = None
            if isinstance(expression.left, expr.Variable) and isinstance(
                expression.right,
                expr.Aggregate,
            ):
                field = model.Field(ID(expression.left.name, expression.left.location))
                aggregate = byte_aggregate(expression.right)
            elif isinstance(expression.left, expr.Aggregate) and isinstance(
                expression.right,
                expr.Variable,
            ):
                field = model.Field(ID(expression.right.name, expression.right.location))
                aggregate = byte_aggregate(expression.left)
            if field and field in message.fields and len(field.identifier.parts) == 1 and aggregate:
                if embedded:
                    return expression.__class__(
                        expr.Indexed(
                            expr.Variable(ID("Buffer") * "all"),
                            expr.ValueRange(
                                expr.Call(
                                    const.TYPES_TO_INDEX,
                                    ty.INDEX,
                                    [
                                        expr.Selected(
                                            expr.Indexed(
                                                expr.Variable("Cursors"),
                                                expr.Variable(field.affixed_name),
                                            ),
                                            "First",
                                        ),
                                    ],
                                ),
                                expr.Call(
                                    const.TYPES_TO_INDEX,
                                    ty.INDEX,
                                    [
                                        expr.Selected(
                                            expr.Indexed(
                                                expr.Variable("Cursors"),
                                                expr.Variable(field.affixed_name),
                                            ),
                                            "Last",
                                        ),
                                    ],
                                ),
                            ),
                        ),
                        aggregate,
                    )
                equal_call = expr.Call(
                    "Equal",
                    ty.BOOLEAN,
                    [expr.Variable("Ctx"), expr.Variable(field.affixed_name), aggregate],
                )
                return equal_call if isinstance(expression, expr.Equal) else expr.Not(equal_call)

        def field_value(field: model.Field) -> expr.Expr:
            if public:
                return expr.Call(
                    f"Get_{field.name}",
                    message.field_types[field].type_,
                    [expr.Variable("Ctx")],
                )
            return expr.Selected(
                expr.Indexed(
                    expr.Variable(ID("Ctx") * "Cursors" if not embedded else "Cursors"),
                    expr.Variable(field.affixed_name),
                ),
                "Value",
            )

        if isinstance(expression, expr.Relation):
            if (
                isinstance(expression.left, expr.Variable)
                and model.Field(ID(expression.left.name, expression.left.location))
                in message.fields
                and isinstance(expression.right, expr.Number)
            ):
                return expression.__class__(
                    field_value(model.Field(ID(expression.left.name, expression.left.location))),
                    expression.right,
                )
            if (
                isinstance(expression.right, expr.Variable)
                and model.Field(ID(expression.right.name, expression.right.location))
                in message.fields
                and isinstance(expression.left, expr.Number)
            ):
                return expression.__class__(
                    expression.left,
                    field_value(model.Field(ID(expression.right.name, expression.right.location))),
                )

        return expression

    return func


def substitution_facts(
    message: model.Message,
    embedded: bool = False,
    public: bool = False,
    target_type: ty.NamedType = ty.BASE_INTEGER,
) -> dict[expr.Expr, expr.Expr]:
    def prefixed(name: str) -> expr.Expr:
        return expr.Variable(ID("Ctx") * name) if not embedded else expr.Variable(name)

    first = prefixed("First")
    last = (
        expr.Call("Written_Last", ty.BIT_LENGTH, [expr.Variable("Ctx")])
        if public
        else prefixed("Written_Last")
    )
    cursors = prefixed("Cursors")

    def field_first(field: model.Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_First",
                ty.BIT_INDEX,
                [expr.Variable("Ctx"), expr.Variable(field.affixed_name)],
            )
        return expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "First")

    def field_last(field: model.Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_Last",
                ty.BIT_LENGTH,
                [expr.Variable("Ctx"), expr.Variable(field.affixed_name)],
            )
        return expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "Last")

    def field_size(field: model.Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_Size",
                ty.BIT_LENGTH,
                [expr.Variable("Ctx"), expr.Variable(field.affixed_name)],
            )
        return expr.Add(
            expr.Sub(
                expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "Last"),
                expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "First"),
            ),
            expr.Number(1),
        )

    def parameter_value(parameter: model.Field, parameter_type: model.TypeDecl) -> expr.Expr:
        var = (
            expr.Variable(parameter.name, type_=parameter_type.type_)
            if embedded
            else expr.Variable("Ctx" * parameter.identifier, type_=parameter_type.type_)
        )
        if parameter_type == model.BOOLEAN:
            return var
        if isinstance(parameter_type, model.Enumeration):
            return expr.Call("To_Base_Integer", ty.BASE_INTEGER, [var])
        if isinstance(parameter_type, model.Scalar):
            return var

        assert False, f'unexpected type "{type(parameter_type).__name__}"'

    def field_value(field: model.Field, field_type: model.TypeDecl) -> expr.Expr:
        call = expr.Call(
            f"Get_{field.name}",
            field_type.type_,
            [expr.Variable("Ctx")],
        )

        if isinstance(field_type, model.Enumeration):
            if public:
                if field_type == model.BOOLEAN:
                    return call

                return expr.Call(
                    "To_Base_Integer",
                    ty.BASE_INTEGER,
                    [call],
                )
            value = expr.Selected(
                expr.Indexed(cursors, expr.Variable(field.affixed_name)),
                "Value",
            )
            if field_type == model.BOOLEAN:
                return expr.Call("To_Actual", ty.BOOLEAN, [value])
            return value

        if isinstance(field_type, model.Scalar):
            if public:
                return call
            return expr.Selected(
                expr.Indexed(cursors, expr.Variable(field.affixed_name)),
                "Value",
            )
        if isinstance(field_type, model.Composite):
            return expr.Variable(field.name)

        assert False, f'unexpected type "{type(field_type).__name__}"'

    def type_conversion(expression: expr.Expr) -> expr.Expr:
        if expression.type_ == ty.BOOLEAN:
            return expression
        return expr.Call(type_to_id(target_type), target_type, [expression])

    return {
        expr.First("Message"): type_conversion(first),
        expr.Last("Message"): type_conversion(last),
        expr.Size("Message"): type_conversion(expr.Add(last, -first, expr.Number(1))),
        **{expr.First(f.name): type_conversion(field_first(f)) for f in message.fields},
        **{expr.Last(f.name): type_conversion(field_last(f)) for f in message.fields},
        **{expr.Size(f.name): type_conversion(field_size(f)) for f in message.fields},
        **{
            expr.Variable(p.identifier): type_conversion(parameter_value(p, t))
            for p, t in message.parameter_types.items()
        },
        **{
            expr.Variable(f.name): type_conversion(field_value(f, t))
            for f, t in message.field_types.items()
        },
        **{
            expr.Literal(l): type_conversion(
                expr.Call("To_Base_Integer", ty.BASE_INTEGER, [expr.Variable(l)]),
            )
            for t in message.types.values()
            if isinstance(t, model.Enumeration) and t != model.BOOLEAN
            for l in t.literals
        },
        **{
            expr.Literal(t.package * l): type_conversion(
                expr.Call(
                    "To_Base_Integer",
                    ty.BASE_INTEGER,
                    [expr.Variable(const.PREFIX_ID * t.package * l)],
                ),
            )
            for t in message.types.values()
            if isinstance(t, model.Enumeration) and t != model.BOOLEAN
            for l in t.literals
        },
        # Eng/RecordFlux/RecordFlux#276
        **{expr.ValidChecksum(f): expr.TRUE for f in message.checksums},
    }


def message_structure_invariant(
    message: model.Message,
    embedded: bool = False,
) -> Expr:
    """
    Create the invariant that defines a valid message structure.

    This invariant ensures the properties of message fields that can depend on the concrete message
    path: the field size, the location of the field and the predecessor of the field. This is
    realized by a list of if-expressions. Each if-expression states the properties of a given field,
    based on the incoming links. Simplifications are applied (e.g. when there is only one
    incoming link).
    """

    def prefixed(name: str) -> expr.Expr:
        return expr.Selected(expr.Variable("Ctx"), name) if not embedded else expr.Variable(name)

    def link_property(link: model.Link, unique: bool) -> Expr:
        field_type = message.types[link.target]
        condition = link.condition.substituted(substitution(message, embedded)).simplified()
        size = (
            field_type.size
            if isinstance(field_type, model.Scalar)
            else link.size.substituted(
                substitution(message, embedded, target_type=ty.BIT_LENGTH),
            ).simplified()
        )
        first = (
            prefixed("First")
            if link.source == model.INITIAL
            else link.first.substituted(
                substitution(message, embedded, target_type=ty.BIT_INDEX),
            )
            .substituted(
                expr.substitution(
                    {
                        expr.UNDEFINED: expr.Add(
                            expr.Selected(
                                expr.Indexed(
                                    prefixed("Cursors"),
                                    expr.Variable(link.source.affixed_name),
                                ),
                                "Last",
                            ),
                            expr.Number(1),
                        ),
                    },
                ),
            )
            .simplified()
        )
        precond = (
            AndThen(
                Call(
                    "Well_Formed",
                    [Indexed(Variable("Cursors"), Variable(link.source.affixed_name))],
                ),
                expr_conv.to_ada(condition),
            )
            if link.source != model.INITIAL and not unique
            else TRUE
        )

        return IfThenElse(
            precond,
            AndThen(
                Equal(
                    Add(
                        Sub(
                            Selected(
                                Indexed(
                                    expr_conv.to_ada(prefixed("Cursors")),
                                    Variable(link.target.affixed_name),
                                ),
                                "Last",
                            ),
                            Selected(
                                Indexed(
                                    expr_conv.to_ada(prefixed("Cursors")),
                                    Variable(link.target.affixed_name),
                                ),
                                "First",
                            ),
                        ),
                        Number(1),
                    ),
                    expr_conv.to_ada(size),
                ),
                Equal(
                    Selected(
                        Indexed(
                            expr_conv.to_ada(prefixed("Cursors")),
                            Variable(link.target.affixed_name),
                        ),
                        "First",
                    ),
                    expr_conv.to_ada(first),
                ),
            ),
        )

    def field_property(fld: model.Field) -> Expr:
        incoming = message.incoming(fld)
        unique = len(incoming) == 1
        return AndThen(*[link_property(link, unique) for link in incoming])

    def map_invariant(fld: model.Field) -> Expr:
        return IfThenElse(
            Call(
                "Well_Formed",
                [Indexed(expr_conv.to_ada(prefixed("Cursors")), Variable(fld.affixed_name))],
            ),
            field_property(fld),
        )

    return AndThen(*[map_invariant(f) for f in message.fields])


def context_predicate(
    message: model.Message,
) -> Expr:

    return AndThen(
        IfThenElse(
            NotEqual(Variable("Buffer"), Variable("null")),
            And(
                Greater(Length("Buffer"), Number(0)),
                Equal(First("Buffer"), Variable("Buffer_First")),
                Equal(Last("Buffer"), Variable("Buffer_Last")),
            ),
        ),
        public_context_predicate(),
        LessEqual(Sub(Variable("First"), Number(1)), Variable("Verified_Last")),
        LessEqual(Sub(Variable("First"), Number(1)), Variable("Written_Last")),
        LessEqual(Variable("Verified_Last"), Variable("Written_Last")),
        LessEqual(Variable("Written_Last"), Variable("Last")),
        Equal(Rem(Variable("First"), Size(const.TYPES_BYTE)), Number(1)),
        Equal(Rem(Variable("Last"), Size(const.TYPES_BYTE)), Number(0)),
        Equal(Rem(Variable("Verified_Last"), Size(const.TYPES_BYTE)), Number(0)),
        Equal(Rem(Variable("Written_Last"), Size(const.TYPES_BYTE)), Number(0)),
        Call(
            "Cursors_Invariant",
            [Variable("Cursors"), Variable("First"), Variable("Verified_Last")],
        ),
        Call(
            "Valid_Predecessors_Invariant",
            [
                Variable("Cursors"),
                Variable("First"),
                Variable("Verified_Last"),
                Variable("Written_Last"),
                *([Variable("Buffer")] if message.has_aggregate_dependent_condition() else []),
                *[Variable(p.identifier) for p in message.parameter_types],
            ],
        ),
        message_structure_invariant(message, embedded=True),
    )


def public_context_predicate() -> Expr:
    """
    Create the predicate that defines the relations between the public discriminants of the context.

    This invariant ensures that

        - the positions of the first and last usable bit are inside the range of the buffer,
        - the size of the usable buffer is greater or equal to zero,
        - the value of `Last` and `Buffer_Last` is less than the last possible value of their
          respective type to allow the representation of a size of zero in these edge cases
        - and the positions of the first and last usable bit are byte aligned.
    """
    return And(
        GreaterEqual(Call(const.TYPES_TO_INDEX, [Variable("First")]), Variable("Buffer_First")),
        LessEqual(Call(const.TYPES_TO_INDEX, [Variable("Last")]), Variable("Buffer_Last")),
        Less(Variable("Buffer_Last"), Last(const.TYPES_INDEX)),
        LessEqual(Variable("First"), Add(Variable("Last"), Number(1))),
        Less(Variable("Last"), Last(const.TYPES_BIT_INDEX)),
        Equal(Rem(Variable("First"), Size(const.TYPES_BYTE)), Number(1)),
        Equal(Rem(Variable("Last"), Size(const.TYPES_BYTE)), Number(0)),
    )


def context_invariant(message: model.Message, loop_entry: bool = False) -> list[Expr]:
    return [
        Equal(e, LoopEntry(e) if loop_entry else Old(e))
        for e in [
            Variable("Ctx.Buffer_First"),
            Variable("Ctx.Buffer_Last"),
            Variable("Ctx.First"),
            Variable("Ctx.Last"),
            *[Variable("Ctx" * ID(p.name)) for p in message.parameters],
        ]
    ]


def valid_path_to_next_field_condition(
    message: model.Message,
    field: model.Field,
) -> list[Expr]:
    return [
        IfThenElse(
            expr_conv.to_ada(
                l.condition.substituted(
                    substitution(message, public=True),
                ).simplified(),
            ),
            (
                Call("Valid_Next", [Variable("Ctx"), Variable(l.target.affixed_name)])
                if l.target != model.FINAL
                else TRUE
            ),
        )
        for l in message.outgoing(field)
        if l.target != model.FINAL
    ]


def context_cursor_unchanged(
    message: model.Message,
    field: model.Field,
    predecessors: bool,
) -> list[Expr]:
    lower: model.Field
    upper: model.Field
    if predecessors:
        if len(message.predecessors(field)) == 0:
            return []
        lower = message.fields[0]
        upper = message.fields[message.fields.index(field) - 1]
    else:
        if len(message.successors(field)) == 0:
            return []
        lower = message.fields[message.fields.index(field) + 1]
        upper = message.fields[-1]
    return [
        ForAllIn(
            "F",
            ValueRange(
                lower=Variable(lower.affixed_name),
                upper=Variable(upper.affixed_name),
                type_identifier=ID("Field"),
            ),
            Equal(
                Call(
                    "Context_Cursors_Index",
                    [
                        Call(
                            "Context_Cursors",
                            [Variable("Ctx")],
                        ),
                        Variable("F"),
                    ],
                ),
                Call(
                    "Context_Cursors_Index",
                    [
                        Old(
                            Call(
                                "Context_Cursors",
                                [Variable("Ctx")],
                            ),
                        ),
                        Variable("F"),
                    ],
                ),
            ),
        ),
    ]


def sufficient_space_for_field_condition(
    message_id: ID,
    field_name: Name,
    size: Expr | None = None,
) -> Expr:
    if size is None:
        size = Call(message_id * "Field_Size", [Variable("Ctx"), field_name])
    return GreaterEqual(Call(message_id * "Available_Space", [Variable("Ctx"), field_name]), size)


def initialize_field_statements(
    field: model.Field,
    reset_written_last: bool = False,
) -> list[Statement]:
    return [
        CallStatement(
            "Reset_Dependent_Fields",
            [Variable("Ctx"), Variable(field.affixed_name)],
        ),
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
                ("Verified_Last", Variable("Last")),
                (
                    "Written_Last",
                    (
                        Variable("Last")
                        if reset_written_last
                        else Max(
                            const.TYPES_BIT_LENGTH,
                            Variable("Ctx.Written_Last"),
                            Variable("Last"),
                        )
                    ),
                ),
            ),
        ),
        PragmaStatement(
            "Warnings",
            [
                Variable("On"),
                String("attribute Update is an obsolescent feature"),
            ],
        ),
        Assignment(
            Indexed(Variable("Ctx.Cursors"), Variable(field.affixed_name)),
            NamedAggregate(
                ("State", Variable("S_Well_Formed")),
                ("First", Variable("First")),
                ("Last", Variable("Last")),
                ("Value", Number(0)),
            ),
        ),
    ]


def field_bit_location_declarations(field_name: Name) -> list[Declaration]:
    return [
        ObjectDeclaration(
            ["First"],
            const.TYPES_BIT_INDEX,
            Call("Field_First", [Variable("Ctx"), field_name]),
            constant=True,
        ),
        ObjectDeclaration(
            ["Last"],
            const.TYPES_BIT_INDEX,
            Call("Field_Last", [Variable("Ctx"), field_name]),
            constant=True,
        ),
    ]


def field_condition_call(
    message: model.Message,
    field: model.Field,
    value: Expr | None = None,
    aggregate: Expr | None = None,
    size: Expr | None = None,
    context: ID | None = None,
) -> Expr:
    package = const.PREFIX_ID * message.identifier
    if context is None:
        context = ID("Ctx")
    if value is None:
        value = Number(0)
    if aggregate is None:
        aggregate = EMPTY_ARRAY
    if size is None:
        size = Call(
            package * "Field_Size",
            [Variable(context), Variable(package * field.affixed_name)],
        )
    return Call(
        package * "Field_Condition",
        [
            Variable(context),
            Variable(package * field.affixed_name),
            *([value] if has_scalar_value_dependent_condition(message) else []),
            *([aggregate] if message.has_aggregate_dependent_condition() else []),
            *([size] if has_size_dependent_condition(message, field) else []),
        ],
    )


def to_base_integer(type_package: ID) -> ID:
    if type_package == BUILTINS_PACKAGE:
        return ID("To_Base_Integer")
    return const.PREFIX_ID * type_package * "To_Base_Integer"


def ada_type_identifier(type_identifier: ID) -> ID:
    if model.is_builtin_type(type_identifier):
        return type_identifier.name

    return type_identifier


def prefixed_type_identifier(type_identifier: ID) -> ID:
    if model.is_builtin_type(type_identifier):
        return type_identifier.name

    return const.PREFIX_ID * type_identifier


def enum_name(enum_type: model.Enumeration) -> ID:
    return ID(enum_type.name) + "_Enum"


def full_enum_name(enum_type: model.Enumeration) -> ID:
    return enum_type.identifier + "_Enum"


def sequence_name(message: model.Message, field: model.Field) -> ID:
    return message.types[field].identifier


def contains_function_name(refinement_package: ID, pdu: ID, sdu: ID, field: ID) -> str:
    sdu_name = sdu.name if sdu.parent == refinement_package else sdu
    pdu_name = pdu.name if pdu.parent == refinement_package else pdu
    return f"{sdu_name.flat}_In_{pdu_name.flat}_{field}"


def has_scalar_value_dependent_condition(message: model.Message) -> bool:
    return any(
        True
        for l in message.structure
        for v in l.condition.variables()
        if v.identifier == l.source.identifier and isinstance(v.type_, (ty.Integer, ty.Enumeration))
    )


def has_size_dependent_condition(
    message: model.Message,
    field: model.Field | None = None,
) -> bool:
    field_sizes = {expr.Size(f.name) for f in message.fields}
    links = message.outgoing(field) if field else message.structure
    return any(
        size in field_sizes
        for link in links
        for size in link.condition.findall(lambda x: isinstance(x, expr.Size))
    )


def create_sequence_instantiation(
    sequence_type: model.Sequence,
    flat: bool = False,
) -> tuple[list[ContextItem], GenericPackageInstantiation]:
    element_type = sequence_type.element_type
    element_type_package = element_type.package.name

    sequence_context: list[ContextItem] = []
    sequence_package: GenericPackageInstantiation
    if isinstance(element_type, model.Message):
        element_type_identifier = ID(
            element_type.identifier.flat if flat else const.PREFIX_ID * element_type.identifier,
        )
        sequence_context = [
            WithClause(const.PREFIX_ID * const.MESSAGE_SEQUENCE_PACKAGE),
            *([] if flat else [WithClause(element_type_identifier)]),
        ]
        sequence_package = GenericPackageInstantiation(
            ID(
                (
                    sequence_type.identifier.flat
                    if flat
                    else const.PREFIX_ID * sequence_type.identifier
                ),
            ),
            const.PREFIX_ID * const.MESSAGE_SEQUENCE_PACKAGE,
            [
                (None, element_type_identifier * "Context"),
                (None, element_type_identifier * "Initialize"),
                (None, element_type_identifier * "Take_Buffer"),
                (None, element_type_identifier * "Copy"),
                (None, element_type_identifier * "Has_Buffer"),
                (None, element_type_identifier * "Size"),
                (None, element_type_identifier * "Message_Last"),
                (None, element_type_identifier * "Initialized"),
                (None, element_type_identifier * "Well_Formed_Message"),
            ],
        )
    elif isinstance(element_type, model.Scalar):
        element_type_identifier = const.PREFIX_ID * element_type.identifier
        sequence_context = [
            WithClause(const.PREFIX_ID * const.SCALAR_SEQUENCE_PACKAGE),
            *(
                [WithClause(const.PREFIX_ID * element_type_package)]
                if element_type_package != sequence_type.package
                else []
            ),
        ]
        sequence_package = GenericPackageInstantiation(
            ID(
                (
                    sequence_type.identifier.flat
                    if flat
                    else const.PREFIX_ID * sequence_type.identifier
                ),
            ),
            const.PREFIX_ID * const.SCALAR_SEQUENCE_PACKAGE,
            [
                (None, element_type_identifier),
                (None, str(element_type.size)),
                (None, const.PREFIX_ID * element_type_package * f"Valid_{element_type.name}"),
                (None, const.PREFIX_ID * element_type_package * "To_Actual"),
                (None, const.PREFIX_ID * element_type_package * "To_Base_Integer"),
            ],
        )
    else:
        assert False, 'unexpected element type "{type(element_type)}"'

    return (sequence_context, sequence_package)


def unchanged_cursor_before_or_invalid(
    limit: Expr,
    loop_entry: bool,
    or_invalid: bool = True,
    including_limit: bool = False,
) -> Expr:
    return ForAllIn(
        "F",
        Variable("Field"),
        IfExpr(
            [
                (
                    (
                        LessEqual(Variable("F"), limit)
                        if including_limit
                        else Less(Variable("F"), limit)
                    ),
                    Equal(
                        Indexed(
                            Variable("Ctx.Cursors"),
                            Variable("F"),
                        ),
                        Indexed(
                            (
                                LoopEntry(Variable("Ctx.Cursors"))
                                if loop_entry
                                else Old(Variable("Ctx.Cursors"))
                            ),
                            Variable("F"),
                        ),
                    ),
                ),
            ],
            *(
                [
                    Call(
                        "Invalid",
                        [
                            Variable("Ctx"),
                            Variable("F"),
                        ],
                    ),
                ]
                if or_invalid
                else []
            ),
        ),
    )


def conditional_field_size(
    field: model.Field,
    message: model.Message,
) -> Expr:
    def substituted(expression: expr.Expr) -> Expr:
        return expr_conv.to_ada(
            expression.substituted(
                substitution(
                    message,
                    target_type=ty.BIT_LENGTH,
                    embedded=True,
                ),
            ).simplified(),
        )

    field_type = message.field_types[field]

    if isinstance(field_type, model.Scalar):
        return expr_conv.to_ada(field_type.size)

    links = message.incoming(field)

    if len(links) == 1:
        return substituted(links[0].size)

    return If(
        [
            (
                AndThen(
                    Call(
                        "Well_Formed",
                        [Indexed(Variable("Cursors"), Variable(l.source.affixed_name))],
                    ),
                    *([substituted(l.condition)] if l.condition != expr.TRUE else []),
                ),
                substituted(l.size),
            )
            for l in links
        ],
        const.UNREACHABLE,
    )


def message_parameters(message: model.Message) -> list[Parameter]:
    return [
        Parameter([p.name], ada_type_identifier(t.identifier))
        for p, t in message.parameter_types.items()
    ]


def initialize_conditions(message: model.Message) -> list[Expr]:
    return [
        *[
            Equal(
                Variable("Ctx" * ID(p.name)),
                Variable(p.name),
            )
            for p, t in message.parameter_types.items()
        ],
        Call("Initialized", [Variable("Ctx")]),
    ]


def context_cursors_initialization(message: model.Message) -> Expr:
    return NamedAggregate(
        ("others", Variable("<>")),
        implicit_elements=[
            (field.affixed_name, field_cursor_aggregate()) for field in message.all_fields
        ],
    )


def field_cursor_aggregate(
    state: Expr | None = None,
    first: Expr | None = None,
    last: Expr | None = None,
    value: Expr | None = None,
) -> NamedAggregate:
    return NamedAggregate(
        *[
            *([("State", state)] if state else []),
            *([("First", first)] if first else []),
            *([("Last", last)] if last else []),
            *([("Value", value)] if value else []),
            *([("others", Variable("<>"))] if not (state and first and last and value) else []),
        ],
        implicit_elements=[
            ("State", state or Variable("S_Invalid")),
            ("First", first or First(const.TYPES_BIT_INDEX)),
            ("Last", last or First(const.TYPES_BIT_LENGTH)),
            ("Value", value or Number(0)),
        ],
    )


def byte_aligned_field(message: model.Message, field: model.Field) -> Expr:
    return Equal(
        Rem(
            Call(
                const.PREFIX_ID * message.identifier * "Field_First",
                [
                    Variable("Ctx"),
                    Variable(const.PREFIX_ID * message.identifier * field.affixed_name),
                ],
            ),
            Size(const.TYPES_BYTE),
        ),
        Number(1),
    )


def external_io_buffers(state_machine: ir.StateMachine) -> list[Message]:
    return sorted(
        {
            Message(
                action.expression.identifier,
                action.expression.type_.identifier,
                tuple(action.expression.type_.parameter_types),
            )
            for state in state_machine.states
            for action in state.actions
            if (
                isinstance(action, ir.ChannelStmt)
                and isinstance(action.expression, ir.Var)
                and isinstance(action.expression.type_, ty.Message)
            )
        },
    )


def suppress_warnings_decl(
    warnings: list[str],
    expressions: list[Declaration],
) -> list[Declaration]:
    return [
        *[
            Pragma(
                "Warnings",
                [Variable("Off"), String.escaped(s)],
            )
            for s in warnings
        ],
        *expressions,
        *[
            Pragma(
                "Warnings",
                [Variable("On"), String.escaped(s)],
            )
            for s in reversed(warnings)
        ],
    ]


def suppress_warnings_stmt(warnings: list[str], statements: list[Statement]) -> list[Statement]:
    return [
        *[
            PragmaStatement(
                "Warnings",
                [Variable("Off"), String(s)],
            )
            for s in warnings
        ],
        *statements,
        *[
            PragmaStatement(
                "Warnings",
                [Variable("On"), String(s)],
            )
            for s in reversed(warnings)
        ],
    ]


def comment_box(lines: list[str], width: int = 78) -> Sequence[ContextItem]:
    result: list[ContextItem] = [Comment("-" * (width - 2))]
    for line in lines:
        if len(line) == 0:
            result.append(Comment(f" {' ' * (width - 5)}--"))

        elif len(line) <= width - 6:
            space = width - 6 - len(line)
            left_space = " " * math.floor(space / 2)
            right_space = " " * math.ceil(space / 2)
            result.append(Comment(f" {left_space}{line}{right_space} --"))

        else:
            for l in textwrap.wrap(line, width=width - 6):
                space = width - 6 - len(l)
                result.append(Comment(f" {l}{' ' * space} --"))

    result.append(Comment("-" * (width - 2)))
    return result
