from __future__ import annotations

import string
from collections import abc
from dataclasses import dataclass
from typing import Protocol, TypeVar

from hypothesis import assume, strategies as st

from rflx import const, expr, ty
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    INTERNAL_TYPES,
    Composite,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Model,
    Opaque,
    Refinement,
    Scalar,
    Sequence,
    TypeDecl,
)
from rflx.rapidflux import NO_LOCATION, ErrorEntry, Location, RecordFluxError, Severity

T = TypeVar("T")


class Draw(Protocol):
    def __call__(self, val: st.SearchStrategy[T], /) -> T: ...


def unique_qualified_identifiers() -> abc.Generator[ID, None, None]:
    prefix = ""
    pos = -1
    while True:
        pos += 1
        if pos == 26:
            pos = 0
            prefix += "A"
        name = prefix + string.ascii_uppercase[pos]
        if name.lower() in const.RESERVED_WORDS:
            continue
        yield ID(["Test", name], Location((1, 1)))


@st.composite
def identifiers(draw: Draw) -> ID:
    name = draw(st.text(string.ascii_uppercase, min_size=1))
    assume(name.lower() not in const.RESERVED_WORDS)
    return ID(name)


@st.composite
def sizes(
    draw: Draw,
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> int:
    assert 0 <= align_to_8 < 8
    assert not (multiple_of_8 and align_to_8)
    # Eng/RecordFlux/RecordFlux#1077
    # size of integers is limited to 63bits
    if multiple_of_8:
        return draw(st.integers(min_value=1, max_value=7).map(lambda x: x * 8))
    if align_to_8:
        return draw(st.integers(min_value=1, max_value=8).map(lambda x: x * 8 - align_to_8))
    return draw(st.integers(min_value=1, max_value=63))


@st.composite
def integers(
    draw: Draw,
    unique_identifiers: abc.Generator[ID, None, None],
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> Integer:
    size = draw(sizes(multiple_of_8, align_to_8))
    max_value = min(2**size - 1, 2**63 - 1)
    first = draw(st.integers(min_value=0, max_value=max_value))
    last = draw(st.integers(min_value=first, max_value=max_value))
    return Integer(
        next(unique_identifiers),
        expr.Number(first),
        expr.Number(last),
        expr.Number(size),
    )


@st.composite
def enumerations(
    draw: Draw,
    unique_identifiers: abc.Generator[ID, None, None],
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> Enumeration:
    @st.composite
    def literal_identifiers(_: abc.Callable[[], object]) -> str:
        assert unique_identifiers
        return str(next(unique_identifiers).name)

    size = draw(sizes(multiple_of_8, align_to_8))
    literals = draw(
        st.lists(
            st.tuples(
                literal_identifiers(),
                st.builds(
                    expr.Number,
                    st.integers(min_value=0, max_value=min(2**size - 1, 2**63 - 1)),
                ),
            ),
            unique_by=(lambda x: x[0], lambda x: x[1]),
            min_size=1,
            max_size=2**size,
        ),
    )

    return Enumeration(
        next(unique_identifiers),
        literals,
        expr.Number(size),
        draw(st.booleans()) if len(literals) < 2**size else False,
    )


@st.composite
def scalars(
    draw: Draw,
    unique_identifiers: abc.Generator[ID, None, None],
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> Scalar:
    return draw(
        st.one_of(
            integers(unique_identifiers, multiple_of_8, align_to_8),
            enumerations(unique_identifiers, multiple_of_8, align_to_8),
        ),
    )


@st.composite
def sequences(
    draw: Draw,
    element_types: st.SearchStrategy[TypeDecl],
    unique_identifiers: abc.Generator[ID, None, None],
) -> Sequence:
    return Sequence(next(unique_identifiers), draw(element_types))


@st.composite
def opaque(draw: Draw) -> Opaque:
    return draw(st.just(Opaque()))


@st.composite
def composites(draw: Draw, unique_identifiers: abc.Generator[ID, None, None]) -> Composite:
    return draw(
        st.one_of(
            opaque(),
            sequences(
                st.one_of(
                    scalars(unique_identifiers, multiple_of_8=True),
                    st.deferred(lambda: non_null_messages(unique_identifiers)),
                ),
                unique_identifiers,
            ),
        ),
    )


@st.composite
def messages(  # noqa: PLR0915
    draw: Draw,
    unique_identifiers: abc.Generator[ID, None, None],
    not_null: bool = False,
) -> Message:
    @dataclass
    class FieldPair:
        source: Field
        target: Field
        source_type: TypeDecl | None
        target_type: TypeDecl | None

    def size(pair: FieldPair) -> expr.Expr:
        max_size = 2**29 - 1
        if isinstance(pair.target_type, (Opaque, Sequence)):
            if isinstance(pair.source_type, Integer) and pair.source_type.last.value <= max_size:
                return expr.Mul(
                    expr.Variable(pair.source.name),
                    expr.Number(8),
                    location=Location((1, 1)),
                )
            return expr.Number(
                draw(st.integers(min_value=1, max_value=max_size).map(lambda x: x * 8)),
                location=Location((1, 1)),
            )
        return expr.UNDEFINED

    def condition(pair: FieldPair) -> expr.Expr:
        if isinstance(pair.source_type, Integer):
            first = pair.source_type.first.value
            last = pair.source_type.last.value
            if last - first > 0:
                return expr.Equal(
                    expr.Variable(pair.source.name),
                    expr.Number(draw(st.integers(min_value=first, max_value=last))),
                    location=Location((1, 1)),
                )
        elif isinstance(pair.source_type, Enumeration) and len(pair.source_type.literals) > 1:
            return expr.Equal(
                expr.Variable(pair.source.name),
                expr.Variable(
                    list(pair.source_type.literals.keys())[
                        draw(st.integers(min_value=0, max_value=len(pair.source_type.literals) - 1))
                    ],
                ),
                location=Location((1, 1)),
            )
        return expr.TRUE

    @st.composite
    def fields(_: abc.Callable[[], object]) -> Field:
        return Field(next(unique_identifiers).name)

    structure: list[Link] = []

    def outgoing(field: Field) -> abc.Sequence[Link]:
        return [l for l in structure if l.source == field]

    alignment = 0
    alignments = {}
    types_ = {}
    for _ in range(draw(st.integers(min_value=1 if not_null else 0, max_value=4))):
        f = draw(fields())
        t = draw(
            (
                st.one_of(scalars(unique_identifiers), composites(unique_identifiers))
                if alignment == 0
                else scalars(unique_identifiers, align_to_8=alignment)
            ),
        )
        types_[f] = t
        alignments[f] = alignment
        if isinstance(t, Scalar):
            alignment = (alignment + int(t.size)) % 8

    if types_:
        fields_ = list(types_.keys())

        for i, target in enumerate(fields_):
            source = fields_[i - 1] if i > 0 else INITIAL
            pair = FieldPair(
                source,
                target,
                types_[source] if source != INITIAL else None,
                types_[target],
            )
            structure.append(
                Link(
                    source,
                    target,
                    condition=condition(pair),
                    size=size(pair),
                    location=Location((1, 1)),
                ),
            )

        for i, source in enumerate(fields_):
            out = outgoing(source)
            if fields_[i + 1 :] and len(out) == 1 and out[0].condition != expr.TRUE:
                source_type = types_[source]
                field_size = int(source_type.size) if isinstance(source_type, Scalar) else 0
                target_alignment = (alignments[source] + field_size) % 8
                potential_targets = [
                    f for f in fields_[i + 1 :] if alignments[f] == target_alignment
                ]
                if target_alignment == 0:
                    potential_targets.append(FINAL)
                target = draw(st.sampled_from(potential_targets))
                pair = FieldPair(
                    source,
                    target,
                    types_[source],
                    types_[target] if target != FINAL else None,
                )
                structure.append(
                    Link(
                        source,
                        target,
                        condition=expr.Not(
                            out[0].condition,
                            location=Location((1, 1)),
                        ).simplified(),
                        size=size(pair),
                        location=Location((1, 1)),
                    ),
                )

        loose_ends = [f for f in fields_ if all(l.source != f for l in structure)]
        for field in loose_ends:
            field_type = types_[f]
            field_size = int(field_type.size) if isinstance(field_type, Scalar) else 0
            padding = (alignments[field] + field_size) % 8
            if padding == 0:
                structure.append(Link(field, FINAL, location=Location((1, 1))))
            else:
                f = draw(fields())
                t = draw(scalars(unique_identifiers, align_to_8=padding))
                types_[f] = t
                structure.append(Link(field, f, location=Location((1, 1))))
                structure.append(Link(f, FINAL, location=Location((1, 1))))

    try:
        message = Message(
            next(unique_identifiers),
            structure,
            types_,
            location=Location((1, 1), end=(1, 2)),
        )
    except RecordFluxError as e:
        e.push(
            ErrorEntry(
                f"incorrectly generated message:\n {message!r}",
                Severity.INFO,
                NO_LOCATION,
            ),
        )
        raise

    return message


def non_null_messages(
    unique_identifiers: abc.Generator[ID, None, None],
) -> st.SearchStrategy[Message]:
    return messages(unique_identifiers, not_null=True)


@st.composite
def refinements(draw: Draw, unique_identifiers: abc.Generator[ID, None, None]) -> Refinement:
    pdu = draw(messages(unique_identifiers))
    opaque_fields = [f for f, t in pdu.types.items() if isinstance(t, Opaque)]
    assume(opaque_fields)
    field = draw(st.sampled_from(opaque_fields))
    sdu = draw(messages(unique_identifiers))
    return Refinement("Test", pdu, field, sdu)


@st.composite
def models(draw: Draw) -> Model:
    types_: list[TypeDecl] = []

    def append_types(message: Message) -> None:
        types_.append(message)
        for t in message.types.values():
            if isinstance(t, Opaque):
                continue
            types_.append(t)
            if isinstance(t, Sequence):
                if isinstance(t.element_type, Message):
                    append_types(t.element_type)
                else:
                    types_.append(t.element_type)

    refinement = draw(refinements(unique_qualified_identifiers()))

    types_.append(refinement)
    for m in [refinement.sdu, refinement.pdu]:
        append_types(m)
    types_.extend([*INTERNAL_TYPES.values(), *BUILTIN_TYPES.values()])

    return Model(list(reversed(types_)))


@st.composite
def numbers(draw: Draw, min_value: int = 0, max_value: int | None = None) -> expr.Number:
    return expr.Number(draw(st.integers(min_value=min_value, max_value=max_value)))


@st.composite
def variables(draw: Draw, elements: st.SearchStrategy[str]) -> expr.Variable:
    return expr.Variable(draw(elements))


@st.composite
def attributes(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    sample: st.SearchStrategy[type[expr.Size | expr.First | expr.Last]] = st.sampled_from(
        [expr.Size, expr.First, expr.Last],
    )
    attribute = draw(sample)
    return attribute(draw(elements))


@st.composite
def calls(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Call:
    return draw(
        st.builds(expr.Call, identifiers(), st.just(ty.Undefined), st.lists(elements, min_size=1)),
    )


@st.composite
def aggregates(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Aggregate:
    # TODO(eng/recordflux/RecordFlux#1767): Support aggregates with single elements
    return expr.Aggregate(*draw(st.lists(elements, min_size=2)))


@st.composite
def strings(draw: Draw) -> expr.String:
    return expr.String(draw(st.text(string.ascii_letters + string.digits, min_size=1)))


@st.composite
def quantified_expressions(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    operation = draw(st.sampled_from([expr.ForAllIn, expr.ForSomeIn]))
    return draw(st.builds(operation, identifiers(), elements, elements))


@st.composite
def mathematical_expressions(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    operation = draw(st.sampled_from([expr.Add, expr.Mul, expr.Sub, expr.Div, expr.Pow]))
    expression = draw(st.one_of(elements, st.builds(operation, elements, elements)))
    assert isinstance(expression, expr.Expr)
    return expression


@st.composite
def relations(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Relation:
    sample: st.SearchStrategy[
        type[
            expr.Less
            | expr.LessEqual
            | expr.Equal
            | expr.GreaterEqual
            | expr.Greater
            | expr.NotEqual
            | expr.In
            | expr.NotIn
        ]
    ] = st.sampled_from(
        [
            expr.Less,
            expr.LessEqual,
            expr.Equal,
            expr.GreaterEqual,
            expr.Greater,
            expr.NotEqual,
            expr.In,
            expr.NotIn,
        ],
    )
    relation = draw(sample)
    return relation(draw(elements), draw(elements))


@st.composite
def boolean_relations(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    relation = draw(
        st.sampled_from(
            [
                expr.Equal,
                expr.NotEqual,
            ],
        ),
    )
    return relation(draw(elements), draw(elements))


@st.composite
def boolean_expressions(draw: Draw, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    operation = draw(st.sampled_from([expr.And, expr.Or]))
    return draw(
        st.one_of(
            relations(elements),
            st.builds(operation, boolean_expressions(elements), boolean_expressions(elements)),
        ),
    )
