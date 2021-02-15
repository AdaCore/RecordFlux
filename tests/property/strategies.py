import string
from dataclasses import dataclass
from typing import Callable, Generator, List, Sequence

from hypothesis import assume, strategies as st

import rflx.error as error
import rflx.expression as expr
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    INTERNAL_TYPES,
    Array,
    Composite,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Model,
    ModularInteger,
    Opaque,
    RangeInteger,
    Refinement,
    Scalar,
    Type,
    UnprovenMessage,
)
from rflx.specification import const


def unique_qualified_identifiers() -> Generator[ID, None, None]:
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
        yield ID(["Test", name])


@st.composite
def identifiers(draw: Callable) -> ID:
    name = draw(st.text(string.ascii_uppercase, min_size=1))
    assume(name.lower() not in const.RESERVED_WORDS)
    return ID(name)


@st.composite
def sizes(
    draw: Callable,
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> int:
    assert 0 <= align_to_8 < 8
    assert not (multiple_of_8 and align_to_8)
    if multiple_of_8:
        return draw(st.integers(min_value=1, max_value=8).map(lambda x: x * 8))
    if align_to_8:
        return draw(st.integers(min_value=1, max_value=8).map(lambda x: x * 8 - align_to_8))
    return draw(st.integers(min_value=1, max_value=64))


@st.composite
def modular_integers(
    draw: Callable,
    unique_identifiers: Generator[ID, None, None],
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> ModularInteger:
    return ModularInteger(
        next(unique_identifiers),
        expr.Pow(expr.Number(2), expr.Number(draw(sizes(multiple_of_8, align_to_8)))),
    )


@st.composite
def range_integers(
    draw: Callable,
    unique_identifiers: Generator[ID, None, None],
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> RangeInteger:
    size = draw(sizes(multiple_of_8, align_to_8))
    max_value = min(2 ** size - 1, 2 ** 63 - 1)
    first = draw(st.integers(min_value=0, max_value=max_value))
    last = draw(st.integers(min_value=first, max_value=max_value))
    return RangeInteger(
        next(unique_identifiers), expr.Number(first), expr.Number(last), expr.Number(size)
    )


@st.composite
def enumerations(
    draw: Callable,
    unique_identifiers: Generator[ID, None, None],
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> Enumeration:
    @st.composite
    def literal_identifiers(_: Callable) -> str:
        assert unique_identifiers
        return str(next(unique_identifiers).name)

    size = draw(sizes(multiple_of_8, align_to_8))
    literals = draw(
        st.lists(
            st.tuples(
                literal_identifiers(),
                st.builds(
                    expr.Number, st.integers(min_value=0, max_value=min(2 ** size - 1, 2 ** 63 - 1))
                ),
            ),
            unique_by=(lambda x: x[0], lambda x: x[1]),
            min_size=1,
            max_size=2 ** size,
        )
    )

    return Enumeration(
        next(unique_identifiers),
        literals,
        expr.Number(size),
        draw(st.booleans()) if len(literals) < 2 ** size else False,
    )


@st.composite
def scalars(
    draw: Callable,
    unique_identifiers: Generator[ID, None, None],
    multiple_of_8: bool = False,
    align_to_8: int = 0,
) -> Scalar:
    return draw(
        st.one_of(
            modular_integers(unique_identifiers, multiple_of_8, align_to_8),
            range_integers(unique_identifiers, multiple_of_8, align_to_8),
            enumerations(unique_identifiers, multiple_of_8, align_to_8),
        )
    )


@st.composite
def arrays(
    draw: Callable,
    element_types: st.SearchStrategy[Type],
    unique_identifiers: Generator[ID, None, None],
) -> Array:
    return Array(next(unique_identifiers), draw(element_types))


@st.composite
def opaque(draw: Callable) -> Opaque:
    return draw(st.just(Opaque()))


@st.composite
def composites(draw: Callable, unique_identifiers: Generator[ID, None, None]) -> Composite:
    return draw(
        st.one_of(
            opaque(),
            arrays(
                st.one_of(
                    scalars(unique_identifiers, multiple_of_8=True),
                    st.deferred(lambda: non_null_messages(unique_identifiers)),
                ),
                unique_identifiers,
            ),
        )
    )


@st.composite
def messages(
    draw: Callable,
    unique_identifiers: Generator[ID, None, None],
    not_null: bool = False,
) -> Message:
    # pylint: disable=too-many-locals

    @dataclass
    class FieldPair:
        source: Field
        target: Field
        source_type: Type
        target_type: Type

    def size(pair: FieldPair) -> expr.Expr:
        max_size = 2 ** 29 - 1
        if isinstance(pair.target_type, (Opaque, Array)):
            if isinstance(pair.source_type, Integer):
                if pair.source_type.last.value <= max_size:
                    return expr.Mul(expr.Variable(pair.source.name), expr.Number(8))
            return expr.Number(
                draw(st.integers(min_value=1, max_value=max_size).map(lambda x: x * 8))
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
                )
        elif isinstance(pair.source_type, Enumeration) and len(pair.source_type.literals) > 1:
            return expr.Equal(
                expr.Variable(pair.source.name),
                expr.Variable(
                    list(pair.source_type.literals.keys())[
                        draw(st.integers(min_value=0, max_value=len(pair.source_type.literals) - 1))
                    ]
                ),
            )
        return expr.TRUE

    @st.composite
    def fields(_: Callable) -> Field:
        return Field(next(unique_identifiers).name)

    structure: List[Link] = []

    def outgoing(field: Field) -> Sequence[Link]:
        return [l for l in structure if l.source == field]

    alignment = 0
    alignments = {}
    types_ = {}
    for i in range(draw(st.integers(min_value=1 if not_null else 0, max_value=4))):
        f = draw(fields())
        t = draw(
            st.one_of(scalars(unique_identifiers), composites(unique_identifiers))
            if alignment == 0
            else scalars(unique_identifiers, align_to_8=alignment)
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
            structure.append(Link(source, target, condition=condition(pair), size=size(pair)))

        for i, source in enumerate(fields_):
            out = outgoing(source)
            if fields_[i + 1 :] and len(out) == 1 and out[0].condition != expr.TRUE:
                field_size = int(types_[source].size) if isinstance(types_[source], Scalar) else 0
                target_alignment = (alignments[source] + field_size) % 8
                potential_targets = [
                    f
                    for f in fields_[i + 1 :]
                    if alignments[f] == target_alignment and f != out[0].target
                ]
                target = draw(
                    st.sampled_from(
                        potential_targets if potential_targets else [out[0].target, FINAL]
                    )
                )
                pair = FieldPair(
                    source, target, types_[source], types_[target] if target != FINAL else None
                )
                structure.append(
                    Link(
                        source,
                        target,
                        condition=expr.Not(out[0].condition).simplified(),
                        size=size(pair),
                    )
                )

        loose_ends = [f for f in fields_ if all(l.source != f for l in structure)]
        for l in loose_ends:
            structure.append(Link(l, FINAL))

    message = UnprovenMessage(next(unique_identifiers), structure, types_)

    try:
        return message.proven()
    except error.RecordFluxError as e:
        e.append(
            f"incorrectly generated message:\n {message!r}",
            error.Subsystem.MODEL,
            error.Severity.INFO,
        )
        raise e


def non_null_messages(unique_identifiers: Generator[ID, None, None]) -> st.SearchStrategy[Message]:
    return messages(unique_identifiers, not_null=True)


@st.composite
def refinements(draw: Callable, unique_identifiers: Generator[ID, None, None]) -> Refinement:
    pdu = draw(messages(unique_identifiers))
    opaque_fields = [f for f, t in pdu.types.items() if isinstance(t, Opaque)]
    assume(opaque_fields)
    field = draw(st.sampled_from(opaque_fields))
    sdu = draw(messages(unique_identifiers))
    return Refinement("Test", pdu, field, sdu)


@st.composite
def models(draw: Callable) -> Model:
    types_: List[Type] = []

    def append_types(message: Message) -> None:
        types_.append(message)
        for t in message.types.values():
            if isinstance(t, Opaque):
                continue
            types_.append(t)
            if isinstance(t, Array):
                if isinstance(t.element_type, Message):
                    append_types(t.element_type)
                else:
                    types_.append(t.element_type)

    refinement = draw(refinements(unique_qualified_identifiers()))

    types_.append(refinement)
    for m in [refinement.sdu, refinement.pdu]:
        append_types(m)
    for t in [*INTERNAL_TYPES.values(), *BUILTIN_TYPES.values()]:
        types_.append(t)

    return Model(list(reversed(types_)))


@st.composite
def numbers(draw: Callable, min_value: int = 0, max_value: int = None) -> expr.Number:
    return expr.Number(draw(st.integers(min_value=min_value, max_value=max_value)))


@st.composite
def variables(draw: Callable, elements: st.SearchStrategy[str]) -> expr.Variable:
    return expr.Variable(draw(elements))


@st.composite
def attributes(draw: Callable, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    attribute = draw(st.sampled_from([expr.Size, expr.First, expr.Last]))
    return attribute(draw(elements))


@st.composite
def calls(draw: Callable, elements: st.SearchStrategy[expr.Expr]) -> expr.Call:
    return draw(st.builds(expr.Call, identifiers(), st.lists(elements, min_size=1)))


@st.composite
def aggregates(draw: Callable, elements: st.SearchStrategy[str]) -> expr.Aggregate:
    return expr.Aggregate(*draw(st.lists(elements, min_size=1)))


@st.composite
def strings(draw: Callable) -> expr.String:
    return expr.String(draw(st.text(string.ascii_letters + string.digits, min_size=1)))


@st.composite
def quantified_expressions(draw: Callable, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    operation = draw(st.sampled_from([expr.ForAllIn, expr.ForSomeIn]))
    return draw(st.builds(operation, identifiers(), elements, elements))


@st.composite
def mathematical_expressions(draw: Callable, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    operation = draw(st.sampled_from([expr.Add, expr.Mul, expr.Sub, expr.Div, expr.Pow]))
    return draw(st.one_of(elements, st.builds(operation, elements, elements)))


@st.composite
def relations(draw: Callable, elements: st.SearchStrategy[expr.Expr]) -> expr.Relation:
    relation = draw(
        st.sampled_from(
            [
                expr.Less,
                expr.LessEqual,
                expr.Equal,
                expr.GreaterEqual,
                expr.Greater,
                expr.NotEqual,
                expr.In,
                expr.NotIn,
            ]
        )
    )
    return relation(*draw(st.lists(elements, min_size=2, max_size=2)))


@st.composite
def boolean_relations(draw: Callable, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    relation = draw(
        st.sampled_from(
            [
                expr.Equal,
                expr.NotEqual,
            ]
        )
    )
    return relation(*draw(st.lists(elements, min_size=2, max_size=2)))


@st.composite
def boolean_expressions(draw: Callable, elements: st.SearchStrategy[expr.Expr]) -> expr.Expr:
    operation = draw(st.sampled_from([expr.And, expr.Or]))
    expression = draw(
        st.one_of(
            relations(elements),
            st.builds(operation, boolean_expressions(elements), boolean_expressions(elements)),
        )
    )
    return expression
