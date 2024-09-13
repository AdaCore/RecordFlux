from __future__ import annotations

import re
import textwrap
from collections import abc
from copy import deepcopy
from functools import lru_cache

import pytest

from rflx import expr_proof, ty
from rflx.error import FatalError
from rflx.expr import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    Div,
    Equal,
    Expr,
    First,
    Greater,
    GreaterEqual,
    IfExpr,
    Last,
    Less,
    LessEqual,
    Literal,
    Mod,
    Mul,
    Not,
    NotEqual,
    Number,
    Opaque,
    Or,
    Pow,
    Selected,
    Size,
    Sub,
    ValidChecksum,
    ValueRange,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    OPAQUE,
    DerivedMessage,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Refinement,
    Sequence,
    TypeDecl,
    UncheckedDerivedMessage,
    UncheckedMessage,
    UncheckedRefinement,
    type_decl,
)
from rflx.model.message import ByteOrder, annotate_path
from rflx.rapidflux import Annotation, Location, RecordFluxError, Severity
from tests.data import models
from tests.utils import assert_equal, assert_message_model_error

M_NO_REF = UncheckedMessage(
    ID("P::No_Ref", Location((1, 1))),
    [
        Link(
            INITIAL,
            Field(ID("F1", location=Location((2, 2)))),
            size=Number(16, location=Location((1, 1))),
            location=Location((1, 1)),
        ),
        Link(
            Field(ID("F1", location=Location((3, 3)))),
            Field(ID("F2", location=Location((3, 3)))),
            location=Location((3, 3)),
        ),
        Link(
            Field(ID("F2", location=Location((4, 4)))),
            Field(ID("F3", location=Location((4, 4)))),
            LessEqual(
                Variable(ID("F2", location=Location((5, 5)))),
                Number(100),
                location=Location((5, 5)),
            ),
            first=First(ID("F2", location=Location((6, 6)))),
            location=Location((4, 4)),
        ),
        Link(
            Field(ID("F2", location=Location((7, 7)))),
            Field(ID("F4", location=Location((8, 8)))),
            GreaterEqual(
                Variable("F2", location=Location((9, 9))),
                Number(200),
                location=Location((9, 9)),
            ),
            first=First(ID("F2", location=Location((10, 10)))),
            location=Location((7, 7)),
        ),
        Link(
            Field(ID("F3", location=Location((11, 11)))),
            FINAL,
            Equal(
                Variable(ID("F3", location=Location((12, 12)))),
                Variable(ID("One", location=Location((12, 12)))),
                location=Location((12, 12)),
            ),
            location=Location((11, 11)),
        ),
        Link(Field(ID("F4", location=Location((12, 12)))), FINAL, location=Location((12, 12))),
    ],
    [],
    [
        (Field(ID("F1", location=Location((1, 1)))), OPAQUE.identifier, []),
        (Field(ID("F2", location=Location((2, 2)))), models.integer().identifier, []),
        (Field(ID("F3", location=Location((3, 3)))), models.enumeration().identifier, []),
        (Field(ID("F4", location=Location((4, 4)))), models.integer().identifier, []),
    ],
    location=Location((1, 1), end=(1, 2)),
)

M_SMPL_REF = UncheckedMessage(
    ID("P::Smpl_Ref", Location((1, 1))),
    [
        Link(INITIAL, Field(ID("NR", location=Location((2, 2)))), location=Location((1, 1))),
        Link(Field(ID("NR", location=Location((2, 2)))), FINAL, location=Location((2, 2))),
    ],
    [],
    [(Field(ID("NR", location=Location((2, 2)))), M_NO_REF.identifier, [])],
    location=Location((1, 1), end=(1, 2)),
)


M_DBL_REF = UncheckedMessage(
    ID("P::Dbl_Ref"),
    [Link(INITIAL, Field("SR")), Link(Field("SR"), Field("NR")), Link(Field("NR"), FINAL)],
    [],
    [
        (Field("SR"), M_SMPL_REF.identifier, []),
        (Field("NR"), M_NO_REF.identifier, []),
    ],
    location=Location((1, 1), end=(1, 2)),
)


M_NO_REF_DERI = UncheckedDerivedMessage(
    ID("P::No_Ref_Deri"),
    M_NO_REF.identifier,
    location=Location((1, 1), end=(1, 2)),
)


M_SMPL_REF_DERI = UncheckedDerivedMessage(
    ID("P::Smpl_Ref_Deri"),
    M_SMPL_REF.identifier,
    location=Location((1, 1), end=(1, 2)),
)


@lru_cache
def parameterized_message() -> Message:
    return Message(
        ID("P::M", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("F1", location=Location((1, 1)))),
                size=Mul(
                    Variable(ID("P1", location=Location((2, 2)))),
                    Number(8),
                    location=Location((3, 3)),
                ),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("F1", location=Location((5, 5)))),
                FINAL,
                condition=Equal(Variable("P2"), Variable("One"), Location((4, 1))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("F1", location=Location((3, 3)))),
                Field(ID("F2", location=Location((3, 1)))),
                condition=Equal(Variable("P2"), Variable("Two"), Location((6, 1))),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("F2", location=Location((4, 4)))),
                FINAL,
                location=Location((4, 1)),
            ),
        ],
        {
            Field(ID("P1", location=Location((1, 2)))): models.integer(),
            Field(ID("P2", location=Location((2, 4)))): models.enumeration(),
            Field(ID("F1", location=Location((3, 6)))): OPAQUE,
            Field(ID("F2", location=Location((4, 8)))): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )


@lru_cache
def msg_no_ref() -> Message:
    return M_NO_REF.checked([OPAQUE, models.integer(), models.enumeration()])


@lru_cache
def msg_smpl_ref() -> Message:
    return M_SMPL_REF.checked([OPAQUE, models.integer(), models.enumeration(), msg_no_ref()])


def test_link_order() -> None:
    l1 = Link(FINAL, INITIAL, condition=Equal(Variable("X"), FALSE))
    l2 = Link(FINAL, INITIAL, condition=Equal(Variable("X"), TRUE))
    l3 = Link(INITIAL, FINAL, condition=Equal(Variable("X"), FALSE))
    l4 = Link(INITIAL, FINAL, condition=Equal(Variable("X"), TRUE))
    assert sorted([l1, l2, l3, l4]) == [l1, l2, l3, l4]
    assert sorted([l4, l3, l2, l1]) == [l1, l2, l3, l4]


def test_invalid_identifier() -> None:
    with pytest.raises(
        RecordFluxError,
        match='^<stdin>:10:8: error: invalid format for identifier "A::B::C"$',
    ):
        Message(ID("A::B::C", location=Location((10, 8))), [], {})


@pytest.mark.parametrize(
    "parameter_type",
    [
        models.null_message,
        models.tlv_message,
        models.sequence_integer_vector,
        models.sequence_inner_messages,
        lambda: OPAQUE,
    ],
)
def test_invalid_parameter_type_composite(parameter_type: abc.Callable[[], TypeDecl]) -> None:
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): parameter_type(), Field("X"): models.integer()}

    declared_location = (
        r"\n<stdin>:1:1: note: type declared here"
        if not (
            type_decl.is_internal_type(parameter_type().identifier)
            or type_decl.is_builtin_type(parameter_type().identifier)
        )
        else ""
    )
    assert_message_model_error(
        structure,
        types,
        r"^<stdin>:1:2: error: expected enumeration type or integer type\n"
        rf"<stdin>:1:2: error: found {re.escape(str(parameter_type().type_))}{declared_location}$",
    )


def test_invalid_parameter_type_always_valid_enum() -> None:
    always_valid_enum = Enumeration(
        "P::E",
        [("A", Number(1)), ("B", Number(3))],
        Number(8),
        always_valid=True,
    )
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): always_valid_enum, Field("X"): models.integer()}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:1:2: error: always valid enumeration types not allowed as parameters$",
    )


def test_missing_type() -> None:
    x = Field(ID("X", Location((5, 6))))
    structure = [Link(INITIAL, x), Link(x, FINAL)]

    assert_message_model_error(
        structure,
        {},
        '^<stdin>:5:6: error: missing type for field "X"$',
    )


def test_illegal_first_aspect_at_initial_link() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))

    structure = [
        Link(INITIAL, Field("X"), first=Number(2, location=Location((10, 20)))),
        Link(Field("X"), FINAL),
    ]

    types = {Field("X"): t}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:10:20: error: illegal first aspect on initial link$",
    )


def test_name_conflict_field_enum() -> None:
    t = Enumeration(
        "P::T",
        [(ID("X", Location((3, 27))), Number(1)), (ID("Y", Location((3, 32))), Number(2))],
        Number(8),
        always_valid=False,
    )

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL),
    ]

    types = {Field(ID("X", Location((5, 6)))): t}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:5:6: error: name conflict for field "X" in "P::M"\n'
        "<stdin>:3:27: note: conflicting enumeration literal$",
    )


def test_duplicate_link() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))
    x = Field(ID("X", location=Location((1, 5))))

    structure = [
        Link(INITIAL, x),
        Link(x, FINAL, location=Location((4, 42))),
        Link(x, FINAL, location=Location((5, 42))),
    ]

    types = {Field("X"): t}

    assert_message_model_error(
        structure,
        types,
        f'^<stdin>:1:5: error: duplicate link from "X" to "{FINAL.name}"\n'
        f"<stdin>:4:42: note: duplicate link\n"
        f"<stdin>:5:42: note: duplicate link"
        r"$",
    )


def test_multiple_duplicate_links() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))
    x = Field(ID("X", location=Location((1, 5))))
    y = Field(ID("Y", location=Location((2, 5))))

    structure = [
        Link(INITIAL, x),
        Link(x, y),
        Link(x, FINAL, location=Location((3, 16))),
        Link(x, FINAL, location=Location((4, 18))),
        Link(y, FINAL, location=Location((5, 20))),
        Link(y, FINAL, location=Location((6, 22))),
    ]

    types = {Field("X"): t, Field("Y"): t}

    assert_message_model_error(
        structure,
        types,
        f'^<stdin>:1:5: error: duplicate link from "X" to "{FINAL.name}"\n'
        f"<stdin>:3:16: note: duplicate link\n"
        f"<stdin>:4:18: note: duplicate link\n"
        f'<stdin>:2:5: error: duplicate link from "Y" to "{FINAL.name}"\n'
        f"<stdin>:5:20: note: duplicate link\n"
        f"<stdin>:6:22: note: duplicate link"
        r"$",
    )


def test_unsupported_expression() -> None:
    x = Field("X")

    structure = [
        Link(INITIAL, x),
        Link(
            x,
            FINAL,
            condition=LessEqual(
                Pow(
                    Number(2),
                    Add(Variable("X", location=Location((10, 23))), Number(1)),
                    location=Location((10, 19)),
                ),
                Number(1024),
            ),
        ),
    ]

    types = {x: models.integer()}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:10:19: error: unsupported expression\n"
        '<stdin>:10:23: note: variable "X" in exponent$',
    )


def test_unreachable_field() -> None:
    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), Field("Z")),
        Link(Field(ID("Y", Location((20, 3)))), Field("Z")),
        Link(Field("Z"), FINAL),
    ]

    types = {Field("X"): BOOLEAN, Field("Y"): BOOLEAN, Field("Z"): BOOLEAN}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:20:3: error: unreachable field "Y"$',
    )


def test_cycle() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))
    structure = [
        Link(INITIAL, Field(ID("X")), location=Location((3, 5))),
        Link(Field(ID("X")), Field(ID("Y")), location=Location((4, 5))),
        Link(Field(ID("Y")), Field(ID("Z")), location=Location((5, 5))),
        Link(Field(ID("Z")), Field(ID("X")), location=Location((6, 5))),
        Link(Field(ID("X")), FINAL),
    ]

    types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:8: error: structure of "P::M" contains cycle\n'
        '<stdin>:4:5: note: field "X" links to "Y"\n'
        '<stdin>:5:5: note: field "Y" links to "Z"\n'
        r'<stdin>:6:5: note: field "Z" links to "X"$',
        location=Location((10, 8)),
    )


def test_direct_cycle() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))
    x = Field(ID("X"))
    y = Field(ID("Y"))

    structure = [
        Link(INITIAL, x, location=Location((10, 5))),
        Link(x, y, location=Location((11, 5))),
        Link(y, x, location=Location((12, 5))),
    ]

    types = {Field("X"): t, Field("Y"): t}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:8: error: structure of "P::M" contains cycle\n'
        '<stdin>:11:5: note: field "X" links to "Y"\n'
        '<stdin>:12:5: note: field "Y" links to "X"$',
        location=Location((10, 8)),
    )


def test_nested_cycle() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))
    a = Field(ID("A"))
    b = Field(ID("B"))
    c = Field(ID("C"))
    d = Field(ID("D"))
    e = Field(ID("E"))

    structure = [
        Link(INITIAL, a, location=Location((10, 5))),
        Link(a, b, location=Location((11, 5))),
        Link(b, c, location=Location((12, 5))),
        Link(
            c,
            Field(ID("B")),
            condition=Equal(Literal("B"), Number(4)),
            location=Location((13, 5)),
        ),
        Link(c, d, condition=NotEqual(Literal("D"), Number(4)), location=Location((14, 5))),
        Link(d, e, location=Location((15, 5))),
        Link(e, FINAL, location=Location((16, 5))),
    ]

    types = {
        Field("A"): t,
        Field("B"): t,
        Field("C"): t,
        Field("D"): t,
        Field("E"): t,
    }

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:8: error: structure of "P::M" contains cycle\n'
        '<stdin>:12:5: note: field "B" links to "C"\n'
        '<stdin>:13:5: note: field "C" links to "B"$',
        location=Location((10, 8)),
    )


def test_two_cycles() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))
    a = Field(ID("A"))
    b = Field(ID("B"))
    c = Field(ID("C"))
    d = Field(ID("D"))
    e = Field(ID("E"))
    f = Field(ID("F"))

    structure = [
        Link(INITIAL, a, location=Location((10, 5))),
        Link(a, b, location=Location((11, 5))),
        Link(b, c, location=Location((12, 5))),
        Link(
            c,
            Field(ID("B")),
            condition=Equal(Literal("B"), Number(4)),
            location=Location((13, 5)),
        ),
        Link(c, d, condition=NotEqual(Literal("D"), Number(4)), location=Location((14, 5))),
        Link(d, e, location=Location((15, 5))),
        Link(e, f, location=Location((16, 5))),
        Link(
            f,
            Field(ID("E")),
            condition=Equal(Literal("B"), Number(5)),
            location=Location((17, 5)),
        ),
        Link(f, FINAL, condition=NotEqual(Literal("B"), Number(5)), location=Location((18, 5))),
    ]

    types = {
        Field("A"): t,
        Field("B"): t,
        Field("C"): t,
        Field("D"): t,
        Field("E"): t,
        Field("F"): t,
    }

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:8: error: structure of "P::M" contains cycle\n'
        '<stdin>:12:5: note: field "B" links to "C"\n'
        '<stdin>:13:5: note: field "C" links to "B"\n'
        '<stdin>:10:8: error: structure of "P::M" contains cycle\n'
        '<stdin>:16:5: note: field "E" links to "F"\n'
        '<stdin>:17:5: note: field "F" links to "E"$',
        location=Location((10, 8)),
    )


def test_cycle_detection_no_false_positive() -> None:
    t = Integer("P::T", Number(0), Number(10), Number(8))
    x = Field(ID("X", location=Location((1, 1))))
    y = Field(ID("Y", location=Location((2, 2))))
    z = Field(ID("Z", location=Location((3, 3))))

    structure = [
        Link(INITIAL, x, location=Location((1, 1))),
        Link(x, y, location=Location((2, 2))),
        Link(y, z, location=Location((3, 3))),
        Link(z, FINAL, location=Location((4, 4))),
    ]

    types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

    assert not Message(  # noqa: SLF001
        ID("P::M", Location((1, 1))),
        structure,
        types,
        location=Location((1, 1), end=(1, 2)),
    )._find_cycles(), "expected no cycles"


def test_parameters() -> None:
    assert not models.ethernet_frame().parameters
    assert parameterized_message().parameters == (
        Field("P1"),
        Field("P2"),
    )


def test_fields() -> None:
    assert models.ethernet_frame().fields == (
        Field("Destination"),
        Field("Source"),
        Field("Type_Length_TPID"),
        Field("TPID"),
        Field("TCI"),
        Field("Type_Length"),
        Field("Payload"),
    )
    assert parameterized_message().fields == (
        Field("F1"),
        Field("F2"),
    )


def test_parameter_types() -> None:
    assert parameterized_message().parameter_types == {
        Field("P1"): models.integer(),
        Field("P2"): models.enumeration(),
    }


def test_field_types() -> None:
    assert parameterized_message().field_types == {
        Field("F1"): OPAQUE,
        Field("F2"): models.integer(),
    }


def test_path_condition() -> None:
    assert_equal(models.ethernet_frame().path_condition(INITIAL), TRUE)
    assert_equal(
        models.ethernet_frame().path_condition(Field("TPID")),
        Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
    )
    assert_equal(
        models.ethernet_frame().path_condition(Field("Type_Length")),
        TRUE,
    )
    assert_equal(
        models.ethernet_frame().path_condition(Field("Payload")),
        Or(
            LessEqual(Variable("Type_Length"), Number(1500)),
            GreaterEqual(Variable("Type_Length"), Number(1536)),
        ),
    )


def test_incoming() -> None:
    assert_equal(models.ethernet_frame().incoming(INITIAL), [])
    assert_equal(
        models.ethernet_frame().incoming(Field("Type_Length")),
        [
            Link(Field("TCI"), Field("Type_Length")),
            Link(
                Field("Type_Length_TPID"),
                Field("Type_Length"),
                NotEqual(Variable("Type_Length_TPID"), Number(0x8100, 16)),
                first=First("Type_Length_TPID"),
            ),
        ],
    )
    assert_equal(
        models.ethernet_frame().incoming(FINAL),
        [
            Link(
                Field("Payload"),
                FINAL,
                And(
                    GreaterEqual(Div(Size("Payload"), Number(8)), Number(46)),
                    LessEqual(Div(Size("Payload"), Number(8)), Number(1500)),
                ),
            ),
        ],
    )


def test_outgoing() -> None:
    assert_equal(models.ethernet_frame().outgoing(INITIAL), [Link(INITIAL, Field("Destination"))])
    assert_equal(
        models.ethernet_frame().outgoing(Field("Type_Length")),
        [
            Link(
                Field("Type_Length"),
                Field("Payload"),
                LessEqual(Variable("Type_Length"), Number(1500)),
                Mul(Variable("Type_Length"), Number(8)),
            ),
            Link(
                Field("Type_Length"),
                Field("Payload"),
                GreaterEqual(Variable("Type_Length"), Number(1536)),
                Sub(Last("Message"), Last("Type_Length")),
            ),
        ],
    )
    assert_equal(models.ethernet_frame().outgoing(FINAL), [])


def test_direct_predecessors() -> None:
    assert_equal(models.ethernet_frame().direct_predecessors(INITIAL), [])
    assert_equal(
        models.ethernet_frame().direct_predecessors(Field("Type_Length")),
        [Field("TCI"), Field("Type_Length_TPID")],
    )
    assert_equal(models.ethernet_frame().direct_predecessors(FINAL), [Field("Payload")])


def test_direct_successors() -> None:
    assert_equal(models.ethernet_frame().direct_successors(INITIAL), [Field("Destination")])
    assert_equal(
        models.ethernet_frame().direct_successors(Field("Type_Length")),
        [Field("Payload")],
    )
    assert_equal(models.ethernet_frame().direct_successors(FINAL), [])


def test_definite_predecessors() -> None:
    assert_equal(
        models.ethernet_frame().definite_predecessors(FINAL),
        (
            Field("Destination"),
            Field("Source"),
            Field("Type_Length_TPID"),
            Field("Type_Length"),
            Field("Payload"),
        ),
    )
    assert_equal(
        models.ethernet_frame().definite_predecessors(Field("TCI")),
        (Field("Destination"), Field("Source"), Field("Type_Length_TPID"), Field("TPID")),
    )


def test_predecessors() -> None:
    assert_equal(
        models.ethernet_frame().predecessors(FINAL),
        (
            Field("Destination"),
            Field("Source"),
            Field("Type_Length_TPID"),
            Field("TPID"),
            Field("TCI"),
            Field("Type_Length"),
            Field("Payload"),
        ),
    )
    assert_equal(
        models.ethernet_frame().predecessors(Field("TCI")),
        (Field("Destination"), Field("Source"), Field("Type_Length_TPID"), Field("TPID")),
    )
    assert_equal(models.ethernet_frame().predecessors(Field("Destination")), ())
    assert_equal(models.ethernet_frame().predecessors(INITIAL), ())


def test_successors() -> None:
    assert_equal(
        models.ethernet_frame().successors(INITIAL),
        (
            Field("Destination"),
            Field("Source"),
            Field("Type_Length_TPID"),
            Field("TPID"),
            Field("TCI"),
            Field("Type_Length"),
            Field("Payload"),
        ),
    )
    assert_equal(
        models.ethernet_frame().successors(Field("Source")),
        (
            Field("Type_Length_TPID"),
            Field("TPID"),
            Field("TCI"),
            Field("Type_Length"),
            Field("Payload"),
        ),
    )
    assert_equal(
        models.ethernet_frame().successors(Field("TPID")),
        (Field("TCI"), Field("Type_Length"), Field("Payload")),
    )
    assert_equal(models.ethernet_frame().successors(Field("Payload")), ())
    assert_equal(models.ethernet_frame().successors(FINAL), ())


def test_field_identifier_locations() -> None:
    p = Field(ID("P", Location((1, 1))))
    a = Field(ID("A", Location((1, 2))))
    b = Field(ID("B", Location((2, 3))))

    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("A")),
            Link(
                Field("A"),
                Field("B"),
                condition=Less(Variable("A"), Variable("P"), location=Location((2, 2))),
            ),
            Link(Field("B"), FINAL),
        ],
        {p: models.integer(), a: models.integer(), b: models.integer()},
        location=Location((1, 1), end=(1, 2)),
    )

    assert message.parameters == (p,)
    assert message.parameters[0].identifier.location == p.identifier.location
    assert message.fields == (a, b)
    assert message.fields[0].identifier.location == a.identifier.location
    assert message.fields[1].identifier.location == b.identifier.location


@pytest.mark.parametrize(
    "expression",
    [
        Variable(models.integer().identifier, location=Location((1, 2))),
        And(
            TRUE,
            Variable(models.integer().identifier, location=Location((1, 2))),
            location=Location((3, 4)),
        ),
        Equal(
            Add(Variable(models.integer().identifier, location=Location((1, 2))), Number(1)),
            Number(1),
        ),
        Equal(
            Variable(models.integer().identifier, location=Location((1, 2))),
            Variable("X"),
        ),
    ],
)
def test_invalid_use_of_type_name(expression: Expr) -> None:
    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL, condition=expression),
    ]
    types = {Field("X"): models.integer()}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:2: error: invalid use of type name "P::Integer" in expression$',
    )


@pytest.mark.parametrize(
    "expression",
    [
        Variable("Zero", location=Location((1, 2))),
        And(TRUE, Variable("Zero", location=Location((1, 2)))),
        Equal(Add(Variable("Zero", location=Location((1, 2))), Number(1)), Number(1)),
    ],
)
def test_invalid_use_of_enum_literal(expression: Expr) -> None:
    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL, condition=expression),
    ]
    types = {Field("X"): models.enumeration()}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:2: error: invalid use of enum literal "P::Zero" in expression$',
    )


class NewType(TypeDecl):
    pass


def test_invalid_message_field_type() -> None:
    structure = [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)]
    types = {Field(ID("F", Location((1, 2)))): NewType("P::T")}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:1:2: error: message fields must have a scalar or composite type$",
    )


def test_unused_parameter() -> None:
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): models.integer(), Field("X"): models.integer()}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:1:2: error: unused parameter "P"$',
    )


@pytest.mark.parametrize(
    "condition",
    [
        (Variable("F1"), Variable("X", location=Location((10, 20)))),
        (Variable("X", location=Location((10, 20))), Variable("F1")),
    ],
)
@pytest.mark.parametrize(
    "operation",
    [
        Equal,
        NotEqual,
    ],
)
def test_undefined_variable(
    operation: abc.Callable[[Expr, Expr], Expr],
    condition: tuple[Expr, Expr],
) -> None:
    mod_type = Integer("P::MT", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    enum_type = Enumeration(
        "P::ET",
        [("Val1", Number(0)), ("Val2", Number(1))],
        Number(8),
        always_valid=True,
    )

    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            Field("F2"),
            operation(*condition),
        ),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): enum_type, Field("F2"): mod_type}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: undefined variable "X"$',
    )


def test_undefined_variable_boolean_condition_value() -> None:
    mod_type = Integer("P::MT", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Variable("X", location=Location((10, 20)))),
        Link(Field("F2"), FINAL),
    ]
    types = {Field("F1"): mod_type, Field("F2"): mod_type}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: undefined variable "X"$',
    )


def test_undefined_variable_size() -> None:
    mod_type = Integer("P::MT", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Variable("Field_Size", location=Location((10, 20)))),
        Link(Field("F2"), FINAL),
    ]
    types = {Field("F1"): mod_type, Field("F2"): OPAQUE}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: undefined variable "Field_Size"$',
    )


def test_undefined_variable_first() -> None:
    mod_type = Integer("P::MT", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Variable("Field_First", location=Location((10, 20)))),
        Link(Field("F2"), FINAL),
    ]
    types = {Field("F1"): mod_type, Field("F2"): OPAQUE}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: undefined variable "Field_First"$',
    )


def test_undefined_variables() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            Field("F2"),
            Equal(
                Variable("X", location=Location((10, 20))),
                Variable("Y", location=Location((10, 30))),
            ),
        ),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): models.integer(), Field("F2"): models.integer()}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: undefined variable "X"\n'
        r'<stdin>:10:30: error: undefined variable "Y"$',
    )


def test_subsequent_variable() -> None:
    f1 = Field(ID("F1", location=Location((1, 1))))
    f2 = Field(ID("F2", location=Location((2, 2))))
    t = Integer("P::T", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    structure = [
        Link(INITIAL, f1),
        Link(f1, f2, Equal(Variable("F2", location=Location((1024, 57))), Number(42))),
        Link(f2, FINAL),
    ]

    types = {Field("F1"): t, Field("F2"): t}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1024:57: error: undefined variable "F2"\n<stdin>:1:1: note: on path "F1"$',
    )


def test_reference_to_optional_field_1() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 1)))),
            Field(ID("F2", location=Location((2, 2)))),
            Equal(Variable("F1"), TRUE),
        ),
        Link(
            Field(ID("F1", location=Location((3, 2)))),
            Field(ID("F3", location=Location((3, 3)))),
            Equal(Variable("F1"), FALSE),
        ),
        Link(
            Field(ID("F2", location=Location((4, 3)))),
            Field(ID("F3", location=Location((4, 4)))),
        ),
        Link(
            Field(ID("F3", location=Location((5, 5)))),
            FINAL,
            Equal(Variable("F2", location=Location((10, 30))), TRUE, location=Location((10, 20))),
        ),
    ]

    types = {Field("F1"): BOOLEAN, Field("F2"): BOOLEAN, Field("F3"): BOOLEAN}

    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:10:30: error: undefined variable "F2"\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:3:3: note: on path "F3"'
        r"$",
    )


@pytest.mark.parametrize(
    ("size_expression"),
    [
        Mul(
            Variable("Opt", location=Location((10, 30))),
            Number(8),
            location=Location((10, 20)),
        ),
        Sub(
            Variable("Opt", location=Location((10, 30))),
            Number(8),
            location=Location((10, 20)),
        ),
    ],
)
def test_reference_to_optional_field_2(size_expression: Expr) -> None:
    structure = [
        Link(INITIAL, Field(ID("Flag", location=Location((1, 1))))),
        Link(
            Field(ID("Flag", location=Location((2, 2)))),
            Field(ID("Opt", location=Location((2, 3)))),
            Equal(Variable("Flag"), Number(1)),
        ),
        Link(
            Field(ID("Flag", location=Location((3, 3)))),
            Field(ID("Any", location=Location((3, 4)))),
            NotEqual(Variable("Flag"), Number(1)),
        ),
        Link(
            Field(ID("Opt", location=Location((4, 4)))),
            Field(ID("Any", location=Location((4, 5)))),
        ),
        Link(
            Field(ID("Any", location=Location((5, 5)))),
            Field(ID("Data", location=Location((5, 6)))),
            size=size_expression,
        ),
        Link(Field(ID("Data", location=Location((7, 7)))), FINAL),
    ]
    types = {
        Field("Flag"): models.integer(),
        Field("Opt"): models.integer(),
        Field("Any"): models.integer(),
        Field("Data"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:10:30: error: undefined variable "Opt"\n'
        r'<stdin>:1:1: note: on path "Flag"\n'
        r'<stdin>:3:4: note: on path "Any"'
        r"$",
    )


def test_invalid_use_of_size_attribute() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, Equal(Size(Number(1)), Number(32), Location((400, 17)))),
    ]
    types = {Field("F1"): models.integer()}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:400:17: error: invalid use of size attribute for "1"$',
    )


def test_invalid_relation_to_opaque() -> None:
    structure = [
        Link(INITIAL, Field(ID("Length", location=Location((1, 1))))),
        Link(
            Field(ID("Length", location=Location((2, 2)))),
            Field(ID("Data", location=Location((2, 3)))),
            size=Variable("Length"),
        ),
        Link(
            Field(ID("Data", location=Location((3, 3)))),
            FINAL,
            condition=Equal(Variable("Data"), Number(42, location=Location((10, 20)))),
        ),
    ]
    types = {Field("Length"): models.integer(), Field("Data"): OPAQUE}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: expected sequence type "__INTERNAL__::Opaque"'
        r' with element integer type "__INTERNAL__::Byte" \(0 .. 255\)\n'
        r"<stdin>:10:20: error: found type universal integer \(42 .. 42\)\n"
        r'<stdin>:1:1: note: on path "Length"\n'
        r'<stdin>:2:3: note: on path "Data"$',
    )


def test_invalid_relation_to_aggregate() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1)))), size=Number(16)),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            FINAL,
            LessEqual(
                Variable("F1", location=Location((10, 20))),
                Aggregate(Number(1), Number(2), location=Location((10, 30))),
            ),
        ),
    ]
    types = {Field("F1"): OPAQUE}
    assert_message_model_error(
        structure,
        types,
        r"^<stdin>:10:20: error: expected integer type\n"
        r'<stdin>:10:20: error: found sequence type "__INTERNAL__::Opaque"'
        r' with element integer type "__INTERNAL__::Byte" \(0 .. 255\)\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r"<stdin>:10:30: error: expected integer type\n"
        r"<stdin>:10:30: error: found aggregate"
        r" with element type universal integer \(1 .. 2\)\n"
        r'<stdin>:1:1: note: on path "F1"$',
    )


@pytest.mark.parametrize("lower", [Number(0), Number(1)])
def test_invalid_element_in_relation_to_aggregate(lower: Number) -> None:
    integer = Integer("P::Integer", lower, Number(255), Number(8))
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 2))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            FINAL,
            Equal(Variable("F1"), Aggregate(Number(1), Number(2), location=Location((10, 20)))),
        ),
    ]

    types = {Field("F1"): integer}

    assert_message_model_error(
        structure,
        types,
        rf'^<stdin>:10:20: error: expected integer type "P::Integer" \({lower} .. 255\)\n'
        r"<stdin>:10:20: error: found aggregate with element type universal integer"
        r" \(1 .. 2\)\n"
        r'<stdin>:1:2: note: on path "F1"$',
    )


def test_opaque_aggregate_out_of_range() -> None:
    f = Field(ID("F", location=Location((1, 2))))

    structure = [
        Link(INITIAL, f, size=Number(24)),
        Link(
            f,
            FINAL,
            condition=Equal(
                Variable("F"),
                Aggregate(Number(1), Number(2), Number(256), location=Location((10, 20))),
            ),
        ),
    ]

    types = {Field("F"): OPAQUE}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: expected sequence type "__INTERNAL__::Opaque"'
        r' with element integer type "__INTERNAL__::Byte" \(0 .. 255\)\n'
        r"<stdin>:10:20: error: found aggregate"
        r" with element type universal integer \(1 .. 256\)\n"
        r'<stdin>:1:2: note: on path "F"$',
    )


def test_sequence_aggregate_out_of_range() -> None:
    f = Field(ID("F", location=Location((1, 2))))

    structure = [
        Link(INITIAL, f),
        Link(
            f,
            FINAL,
            condition=Equal(
                Variable("F"),
                Aggregate(Number(1), Number(2), Number(64), location=Location((10, 20))),
            ),
        ),
    ]

    types = {
        Field("F"): Sequence(
            "P::Sequence",
            Integer("P::Element", Number(0), Number(63), Number(8)),
        ),
    }

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: expected sequence type "P::Sequence"'
        r' with element integer type "P::Element" \(0 .. 63\)\n'
        r"<stdin>:10:20: error: found aggregate"
        r" with element type universal integer \(1 .. 64\)\n"
        r'<stdin>:1:2: note: on path "F"$',
    )


def test_sequence_aggregate_invalid_element_type() -> None:
    inner = Message(
        ID("P::I", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("F", location=Location((1, 1)))), location=Location((2, 2))),
            Link(Field(ID("F", location=Location((3, 3)))), FINAL, location=Location((3, 1))),
        ],
        {Field("F"): models.integer()},
        location=Location((1, 1), end=(1, 2)),
    )
    sequence_type = Sequence("P::Sequence", inner)
    f = Field(ID("F", location=Location((1, 2))))

    structure = [
        Link(INITIAL, f, size=Number(18)),
        Link(
            f,
            FINAL,
            condition=Equal(
                Variable("F"),
                Aggregate(Number(1), Number(2), Number(64), location=Location((10, 20))),
            ),
        ),
    ]

    types = {f: sequence_type}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: expected sequence type "P::Sequence"'
        r' with element message type "P::I"\n'
        r"<stdin>:10:20: error: found aggregate with element type universal integer"
        r" \(1 .. 64\)\n"
        r'<stdin>:1:2: note: on path "F"$',
    )


def test_field_size_is_aggregate() -> None:
    structure = [
        Link(INITIAL, Field(ID("F", location=Location((1, 1)))), location=Location((2, 2))),
        Link(
            Field(ID("F", location=Location((3, 3)))),
            Field(ID("G", location=Location((3, 5)))),
            size=Aggregate(Number(1), Number(2), location=Location((3, 4))),
            location=Location((3, 1)),
        ),
        Link(Field(ID("G", location=Location((4, 1)))), FINAL, location=Location((4, 2))),
    ]

    i = Integer("P::I", Number(0), Number(1), Number(1))
    types = {Field("F"): i, Field("G"): OPAQUE}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:3:4: error: expected integer type "__BUILTINS__::Base_Integer" '
        r"\(0 .. 2\*\*63 - 1\)\n"
        r"<stdin>:3:4: error: found aggregate with element type universal integer \(1 .. 2\)\n"
        r'<stdin>:1:1: note: on path "F"$',
    )


def test_opaque_not_byte_aligned() -> None:
    t = Integer("P::T", Number(0), Number(3), Number(4))
    o = Field(ID("O", location=Location((44, 3))))
    with pytest.raises(
        RecordFluxError,
        match=(
            '^<stdin>:44:3: error: opaque field "O" not aligned to 8 bit boundary\n'
            '<stdin>:2:2: note: on path "P"\n'
            "<stdin>:44:3: error: a previous field in the path may be the cause of this error$"
        ),
    ):
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(INITIAL, Field(ID("P", location=Location((2, 2)))), location=Location((2, 1))),
                Link(
                    Field(ID("P", location=Location((3, 3)))),
                    o,
                    size=Number(128, location=Location((3, 1))),
                    location=Location((3, 2)),
                ),
                Link(o, Field(ID("Q", location=Location((4, 4)))), location=Location((4, 1))),
                Link(Field(ID("Q", location=Location((5, 5)))), FINAL, location=Location((5, 6))),
            ],
            {
                Field("P"): t,
                o: OPAQUE,
                Field("Q"): t,
            },
            location=Location((1, 1), end=(1, 2)),
        )


def test_opaque_not_byte_aligned_dynamic() -> None:
    o2 = Field(ID("O2", location=Location((44, 3))))
    with pytest.raises(
        RecordFluxError,
        match='^<stdin>:44:3: error: opaque field "O2" not aligned to 8 bit boundary\n'
        '<stdin>:1:2: note: on path "L1"\n'
        '<stdin>:2:3: note: on path "O1"\n'
        '<stdin>:3:4: note: on path "L2"\n'
        "<stdin>:44:3: error: a previous field in the path may be the cause of this error\n"
        "<stdin>:1:1: error: message size must be multiple of 8 bit\n"
        '<stdin>:1:2: note: on path "L1"\n'
        '<stdin>:2:3: note: on path "O1"\n'
        '<stdin>:3:4: note: on path "L2"\n'
        '<stdin>:44:3: note: on path "O2"$',
    ):
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(
                    INITIAL,
                    Field(ID("L1", location=Location((1, 2)))),
                    location=Location((1, 1)),
                ),
                Link(
                    Field(ID("L1", location=Location((2, 2)))),
                    Field(ID("O1", location=Location((2, 3)))),
                    size=Variable(ID("L1", location=Location((2, 4)))),
                    condition=Equal(
                        Mod(Variable("L1"), Number(8)),
                        Number(0),
                        location=Location((2, 6)),
                    ),
                    location=Location((2, 5)),
                ),
                Link(
                    Field(ID("O1", location=Location((3, 3)))),
                    Field(ID("L2", location=Location((3, 4)))),
                    location=Location((3, 5)),
                ),
                Link(
                    Field(ID("L2", location=Location((4, 4)))),
                    o2,
                    size=Number(128, location=Location((4, 5))),
                    location=Location((4, 6)),
                ),
                Link(o2, FINAL, location=Location((5, 5))),
            ],
            {
                Field(ID("L1", location=Location((1, 3)))): models.integer(),
                Field(ID("L2", location=Location((2, 6)))): Integer(
                    "P::T",
                    Number(0),
                    Number(3),
                    Number(2),
                ),
                Field(ID("O1", location=Location((3, 5)))): OPAQUE,
                o2: OPAQUE,
            },
            location=Location((1, 1), end=(1, 2)),
        )


def test_opaque_valid_byte_aligned_dynamic_mul() -> None:
    Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("L", location=Location((1, 1)))), location=Location((1, 1))),
            Link(
                Field(ID("L", location=Location((2, 2)))),
                Field(ID("O1", location=Location((2, 2)))),
                size=Mul(Number(8), Variable("L"), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(Field(ID("O1", location=Location((3, 3)))), FINAL, location=Location((3, 3))),
        ],
        {
            Field(ID("L", location=Location((1, 1)))): models.integer(),
            Field(ID("O1", location=Location((2, 2)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )


def test_opaque_valid_byte_aligned_dynamic_cond() -> None:
    Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("L", location=Location((1, 1)))), location=Location((1, 1))),
            Link(
                Field(ID("L", location=Location((2, 2)))),
                Field(ID("O1", location=Location((2, 2)))),
                size=Variable(ID("L", location=Location((3, 3)))),
                condition=Equal(
                    Mod(Variable("L"), Number(8)),
                    Number(0),
                    location=Location((3, 4)),
                ),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("O1", location=Location((4, 4)))),
                Field(ID("O2", location=Location((4, 4)))),
                size=Number(128, location=Location((4, 4))),
                location=Location((3, 3)),
            ),
            Link(Field(ID("O2", location=Location((5, 5)))), FINAL, location=Location((5, 5))),
        ],
        {
            Field(ID("L", location=Location((1, 1)))): models.integer(),
            Field(ID("O1", location=Location((2, 2)))): OPAQUE,
            Field(ID("O2", location=Location((3, 3)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )


def test_opaque_size_not_multiple_of_8() -> None:
    o = Field(ID("O", location=Location((1, 1))))
    with pytest.raises(
        RecordFluxError,
        match=re.escape(
            '<stdin>:44:3: error: size of opaque field "O" not multiple of 8 bit\n'
            "<stdin>:44:3: help: sizes are expressed in bits, not bytes\n"
            '<stdin>:44:3: help: did you mean "68 * 8"',
        ),
    ):
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(
                    INITIAL,
                    o,
                    size=Number(68, location=Location((44, 3))),
                    location=Location((44, 4)),
                ),
                Link(o, FINAL, location=Location((45, 3))),
            ],
            {o: OPAQUE},
            location=Location((1, 1), end=(1, 2)),
        )


def test_opaque_size_not_multiple_of_8_dynamic() -> None:
    o = Field(ID("O", location=Location((44, 2))))
    with pytest.raises(
        RecordFluxError,
        match=re.escape(
            '<stdin>:44:3: error: size of opaque field "O" not multiple of 8 bit\n'
            "<stdin>:44:3: help: sizes are expressed in bits, not bytes\n"
            '<stdin>:44:3: help: did you mean "L * 8"',
        ),
    ):
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(
                    INITIAL,
                    Field(ID("L", location=Location((43, 3)))),
                    location=Location((43, 4)),
                ),
                Link(
                    Field(ID("L", location=Location((44, 2)))),
                    o,
                    size=Variable("L", location=Location((44, 3))),
                    location=Location((44, 4)),
                ),
                Link(o, FINAL, location=Location((45, 3))),
            ],
            {Field(ID("L", location=Location((1, 1)))): models.integer(), o: OPAQUE},
            location=Location((1, 1), end=(1, 2)),
        )


def test_opaque_size_valid_multiple_of_8_dynamic_cond() -> None:
    Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("L", location=Location((1, 1)))), location=Location((1, 1))),
            Link(
                Field(ID("L", location=Location((2, 2)))),
                Field(ID("O", location=Location((2, 2)))),
                size=Variable("L", location=Location((3, 3))),
                condition=Equal(
                    Mod(Variable("L"), Number(8)),
                    Number(0),
                    location=Location((4, 4)),
                ),
                location=Location((4, 4)),
            ),
            Link(Field(ID("O", location=Location((5, 5)))), FINAL, location=Location((5, 5))),
        ],
        {
            Field(ID("L", location=Location((1, 1)))): models.integer(),
            Field(ID("O", location=Location((2, 2)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )


def test_prefixed_message_attribute() -> None:
    result = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("F1", location=Location((1, 1)))), location=Location((1, 1))),
            Link(
                Field(ID("F1", location=Location((2, 2)))),
                Field(ID("F2", location=Location((2, 2)))),
                LessEqual(Variable("F1"), Number(100), Location((4, 1))),
                first=First("F1"),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("F1", location=Location((3, 3)))),
                Field(ID("F3", location=Location((3, 3)))),
                GreaterEqual(Variable("F1"), Number(200), Location((6, 1))),
                first=First("F1"),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("F2", location=Location((4, 4)))),
                FINAL,
                Equal(Variable("F2"), Number(42), Location((8, 1))),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("F3", location=Location((5, 5)))),
                Field(ID("F4", location=Location((5, 5)))),
                location=Location((5, 5)),
            ),
            Link(Field(ID("F4", location=Location((6, 6)))), FINAL, location=Location((6, 6))),
        ],
        {
            Field(ID("F1", location=Location((1, 1)))): deepcopy(models.integer()),
            Field(ID("F2", location=Location((2, 2)))): deepcopy(models.integer()),
            Field(ID("F3", location=Location((3, 3)))): deepcopy(models.integer()),
            Field(ID("F4", location=Location((4, 4)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    ).prefixed("X_")

    expected = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("X_F1", location=Location((1, 1)))), location=Location((1, 1))),
            Link(
                Field(ID("X_F1", location=Location((2, 2)))),
                Field(ID("X_F2", location=Location((2, 2)))),
                LessEqual(Variable("X_F1"), Number(100), Location((4, 1))),
                first=First("X_F1"),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("X_F1", location=Location((3, 3)))),
                Field(ID("X_F3", location=Location((3, 3)))),
                GreaterEqual(Variable("X_F1"), Number(200), Location((6, 1))),
                first=First("X_F1"),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("X_F2", location=Location((4, 4)))),
                FINAL,
                Equal(Variable("X_F2"), Number(42), Location((8, 1))),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("X_F3", location=Location((5, 5)))),
                Field(ID("X_F4", location=Location((5, 5)))),
                size=Add(Last("Message"), -Last("X_F3"), location=Location((5, 5))),
                location=Location((5, 5)),
            ),
            Link(Field(ID("X_F4", location=Location((6, 6)))), FINAL, location=Location((6, 6))),
        ],
        {
            Field(ID("X_F1", location=Location((1, 1)))): deepcopy(models.integer()),
            Field(ID("X_F2", location=Location((2, 2)))): deepcopy(models.integer()),
            Field(ID("X_F3", location=Location((3, 3)))): deepcopy(models.integer()),
            Field(ID("X_F4", location=Location((4, 4)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    assert result == expected


def test_exclusive_valid() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1)))), location=Location((1, 1))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            FINAL,
            condition=Greater(Variable("F1"), Number(80), Location((3, 1))),
            location=Location((2, 2)),
        ),
        Link(
            Field(ID("F1", location=Location((3, 3)))),
            Field(ID("F2", location=Location((3, 3)))),
            condition=LessEqual(Variable("F1"), Number(80), Location((5, 1))),
            location=Location((3, 3)),
        ),
        Link(Field(ID("F2", location=Location((4, 4)))), FINAL, location=Location((4, 4))),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((2, 2)))): models.integer(),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_exclusive_enum_valid() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1)))), location=Location((1, 1))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            FINAL,
            condition=Equal(Variable("F1"), Variable("One"), Location((3, 1))),
            location=Location((2, 2)),
        ),
        Link(
            Field(ID("F1", location=Location((4, 4)))),
            Field(ID("F2", location=Location((4, 4)))),
            condition=Equal(Variable("F1"), Variable("Two"), Location((5, 1))),
            location=Location((5, 5)),
        ),
        Link(Field(ID("F2", location=Location((6, 6)))), FINAL, location=Location((6, 6))),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.enumeration(),
        Field(ID("F2", location=Location((2, 2)))): models.integer(),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_exclusive_prefixed_enum_valid() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1)))), location=Location((1, 1))),
        Link(
            Field(ID("F1", location=Location((1, 1)))),
            FINAL,
            condition=Equal(Variable("F1"), Variable("One"), Location((3, 1))),
            location=Location((2, 2)),
        ),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 2)))),
            condition=Equal(Variable("F1"), Variable("P::Two"), Location((5, 1))),
            location=Location((3, 3)),
        ),
        Link(Field(ID("F2", location=Location((4, 4)))), FINAL, location=Location((4, 4))),
    ]
    types = {
        Field("F1"): models.enumeration(),
        Field("F2"): Enumeration(
            "P2::Enumeration",
            [("One", Number(2)), ("Two", Number(1))],
            Number(8),
            always_valid=False,
        ),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_exclusive_conflict() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            FINAL,
            condition=Greater(Variable("F1"), Number(50), Location((10, 5))),
        ),
        Link(
            Field(ID("F1", location=Location((3, 3)))),
            Field(ID("F2", location=Location((3, 4)))),
            condition=Less(Variable("F1"), Number(80), Location((11, 7))),
            location=Location((3, 5)),
        ),
        Link(Field(ID("F2", location=Location((4, 4)))), FINAL, location=Location((5, 5))),
    ]
    types = {
        Field(ID("F1", Location((8, 4)))): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:8:4: error: conflicting conditions for field "F1"\n'
        r"<stdin>:11:7: note: condition 0 [(]F1 -> F2[)]: F1 < 80\n"
        r"<stdin>:10:5: note: condition 1 [(]F1 -> Final[)]: F1 > 50"
        r"$",
    )


def test_exclusive_with_size_valid() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1)))), location=Location((1, 1))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 2)))),
            size=Number(32, location=Location((2, 2))),
            location=Location((2, 2)),
        ),
        Link(
            Field(ID("F2", location=Location((3, 3)))),
            FINAL,
            condition=And(
                Equal(Size("F2"), Number(32), location=Location((4, 5))),
                Less(Variable("F1"), Number(50), location=Location((4, 5))),
                location=Location((4, 5)),
            ),
            location=Location((3, 3)),
        ),
        Link(
            Field(ID("F2", location=Location((4, 4)))),
            Field(ID("F3", location=Location((4, 4)))),
            condition=And(
                Equal(Size("F2"), Number(32), location=Location((6, 5))),
                Greater(Variable("F1"), Number(80), location=Location((6, 5))),
                location=Location((6, 5)),
            ),
            location=Location((4, 4)),
        ),
        Link(Field(ID("F3", location=Location((5, 5)))), FINAL),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((2, 2)))): OPAQUE,
        Field(ID("F3", location=Location((3, 3)))): models.integer(),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_exclusive_with_size_valid_and_not() -> None:
    structure = [
        Link(INITIAL, Field("F1"), location=Location((1, 1))),
        Link(
            Field("F1"),
            Field("F2"),
            size=Number(32, location=Location((2, 2))),
            location=Location((2, 2)),
        ),
        Link(
            Field("F2"),
            FINAL,
            condition=And(
                Equal(Size("F2"), Number(32), location=Location((4, 5))),
                Less(Variable("F1"), Number(50), location=Location((4, 5))),
                location=Location((4, 5)),
            ),
            location=Location((3, 3)),
        ),
        Link(
            Field("F2"),
            Field("F3"),
            condition=And(
                Equal(Size("F2"), Number(32), location=Location((6, 5))),
                Not(Less(Variable("F1"), Number(80), location=Location((6, 5)))),
                location=Location((6, 5)),
            ),
            location=Location((4, 4)),
        ),
        Link(Field("F3"), FINAL, location=Location((5, 5))),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((2, 2)))): OPAQUE,
        Field(ID("F3", location=Location((3, 3)))): models.integer(),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_exclusive_with_size_invalid() -> None:
    structure = [
        Link(
            INITIAL,
            Field("F1"),
            size=Number(32, location=Location((1, 1))),
            location=Location((1, 2)),
        ),
        Link(
            Field("F1"),
            FINAL,
            condition=Equal(Size("F1"), Number(32), Location((10, 2))),
            location=Location((2, 2)),
        ),
        Link(
            Field("F1"),
            Field("F2"),
            condition=Equal(Size("F1"), Number(32), Location((12, 4))),
            location=Location((3, 3)),
        ),
        Link(Field("F2"), FINAL, location=Location((4, 4))),
    ]
    types = {
        Field(ID("F1", Location((98, 10)))): OPAQUE,
        Field(ID("F2", location=Location((99, 10)))): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:98:10: error: conflicting conditions for field "F1"\n'
        r"<stdin>:12:4: note: condition 0 [(]F1 -> F2[)]: F1\'Size = 32\n"
        r"<stdin>:10:2: note: condition 1 [(]F1 -> Final[)]: F1\'Size = 32"
        r"$",
    )


def test_no_valid_path() -> None:
    f1 = Field(ID("F1", Location((10, 5))))
    f2 = Field(ID("F2", Location((11, 6))))
    f3 = Field(ID("F3", Location((12, 7))))
    structure = [
        Link(INITIAL, Field(ID("F1", Location((10, 8))))),
        Link(
            f1,
            Field(ID("F2", Location((11, 9)))),
            condition=LessEqual(Variable("F1"), Number(80), Location((20, 2))),
        ),
        Link(
            f1,
            Field(ID("F3", Location((12, 10)))),
            condition=Greater(Variable("F1"), Number(80), Location((21, 3))),
        ),
        Link(
            f2,
            Field(ID("F3", Location((13, 10)))),
            condition=Greater(Variable("F1"), Number(80), Location((22, 4))),
        ),
        Link(f3, FINAL, condition=LessEqual(Variable("F1"), Number(80), Location((23, 5)))),
    ]
    types = {
        f1: models.integer(),
        f2: models.integer(),
        f3: models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:11:6: error: field "F2" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:10:8: note: on path "F1"\n'
        r'<stdin>:11:9: note: on path "F2"\n'
        r'<stdin>:20:2: note: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: note: unsatisfied "F1 > 80"\n'
        r'<stdin>:12:7: error: field "F3" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:10:8: note: on path "F1"\n'
        r'<stdin>:12:10: note: on path "F3"\n'
        r'<stdin>:21:3: note: unsatisfied "F1 > 80"\n'
        r'<stdin>:23:5: note: unsatisfied "F1 <= 80"'
        r"$",
    )


def test_invalid_path_1() -> None:
    f1 = Field(ID("F1", Location((20, 10))))
    structure = [
        Link(INITIAL, f1),
        Link(f1, FINAL, condition=Equal(Number(1), Number(2), Location((5, 10)))),
    ]
    types = {
        f1: models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:20:10: error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:20:10: note: on path "F1"\n'
        r'<stdin>:5:10: note: unsatisfied "1 = 2"'
        r"$",
    )


def test_invalid_path_2() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 5)))),
            Field(ID("F2", location=Location((2, 10)))),
            condition=Equal(Number(1), Number(2), location=Location((3, 5))),
        ),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:3:5: note: unsatisfied "1 = 2"'
        r"$",
    )


def test_unreachable() -> None:
    integer = Integer(
        ID("P::Integer", location=Location((1, 1))),
        Number(1),
        Number(100),
        Number(8),
        location=Location((1, 2)),
    )
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((2, 1))))),
        Link(
            Field(ID("F1", location=Location((3, 5)))),
            Field(ID("F2", location=Location((4, 10)))),
            condition=Greater(
                Variable(ID("F1", location=Location((5, 15)))),
                Number(1000),
                Location((5, 15)),
            ),
        ),
        Link(Field(ID("F2", location=Location((6, 5)))), FINAL),
    ]
    types = {
        Field("F1"): integer,
        Field("F2"): integer,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:2:1: note: on path "F1"\n'
        r'<stdin>:1:2: note: unsatisfied "F1 <= 100"\n'
        r'<stdin>:5:15: note: unsatisfied "F1 > 1000"'
        r"$",
    )


def test_invalid_type_condition_range_low() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 1)))),
            Field(ID("F2", location=Location((3, 1)))),
            condition=Less(
                Variable(ID("F1", location=Location((4, 1)))),
                Number(1),
                Location((4, 1)),
            ),
        ),
        Link(Field(ID("F2", location=Location((4, 1)))), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:1:1: note: unsatisfied "F1 >= 1"\n'
        r'<stdin>:4:1: note: unsatisfied "F1 < 1"'
        r"$",
    )


def test_invalid_type_condition_range_high() -> None:
    integer = Integer(
        ID("P::Integer", location=Location((1, 1))),
        Number(1),
        Number(100),
        Number(8),
        location=Location((1, 1)),
    )
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 2))))),
        Link(
            Field(ID("F1", location=Location((1, 3)))),
            Field(ID("F2", location=Location((1, 4)))),
            condition=Greater(
                Variable(ID("F1", location=Location((1, 5)))),
                Number(200),
                location=Location((4, 1)),
            ),
        ),
        Link(Field(ID("F2", location=Location((1, 6)))), FINAL),
    ]
    types = {
        Field("F1"): integer,
        Field("F2"): integer,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:2: note: on path "F1"\n'
        r'<stdin>:1:1: note: unsatisfied "F1 <= 100"\n'
        r'<stdin>:4:1: note: unsatisfied "F1 > 200"'
        r"$",
    )


def test_invalid_type_condition_enum() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 3)))),
            condition=Equal(
                Variable("F1"),
                Variable("E4", location=Location((10, 20))),
                location=Location((10, 10)),
            ),
        ),
        Link(Field(ID("F2", location=Location((3, 3)))), FINAL),
    ]
    e1 = Enumeration(
        "P::E1",
        [("E1", Number(1)), ("E2", Number(2)), ("E3", Number(3))],
        Number(8),
        always_valid=False,
        location=Location((10, 4)),
    )
    e2 = Enumeration(
        "P::E2",
        [("E4", Number(1)), ("E5", Number(2)), ("E6", Number(3))],
        Number(8),
        always_valid=False,
        location=Location((11, 4)),
    )
    types = {
        Field("F1"): e1,
        Field("F2"): e2,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: expected enumeration type "P::E1"\n'
        r'<stdin>:10:20: error: found enumeration type "P::E2"\n'
        r'<stdin>:1:1: note: on path "F1"$',
    )


def test_tlv_valid_enum() -> None:
    structure = [
        Link(INITIAL, Field("L"), location=Location((1, 1))),
        Link(Field("L"), Field("T"), location=Location((2, 2))),
        Link(
            Field("T"),
            Field("V"),
            size=Mul(Number(8), Variable("L"), location=Location((3, 2))),
            condition=And(
                NotEqual(Variable("T"), Variable("Two")),
                LessEqual(Variable("L"), Number(8192)),
                location=Location((3, 4)),
            ),
            location=Location((3, 3)),
        ),
        Link(Field("V"), FINAL, location=Location((4, 4))),
    ]
    types = {
        Field(ID("L", location=Location((1, 1)))): models.integer(),
        Field(ID("T", location=Location((2, 2)))): models.enumeration(),
        Field(ID("V", location=Location((3, 3)))): OPAQUE,
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_invalid_fixed_size_field_with_size() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field("F1"),
            Field(ID("F2", location=Location((1, 2)))),
            size=Number(300, location=Location((10, 10))),
        ),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:10: error: fixed size field "F2" does not permit a size aspect\n'
        r"<stdin>:1:2: help: modify this field's type, or alternatively, remove the size aspect\n"
        r"<stdin>:1:2: note: associated type size defined here$",
    )


def test_valid_first() -> None:
    structure = [
        Link(INITIAL, Field("F1"), location=Location((1, 1))),
        Link(Field("F1"), Field("F2"), first=First("F1"), location=Location((2, 2))),
        Link(Field("F2"), FINAL, location=Location((3, 3))),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((2, 2)))): models.integer(),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_invalid_first_is_not_previous_field() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), first=First(ID("F1", location=Location((5, 14))))),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:5:14: error: invalid First for field "F3"$',
    )


def test_invalid_first_is_expression() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            Field("F2"),
            first=Add(First("F1"), Number(8), location=Location((5, 14))),
        ),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:5:14: error: invalid First for field "F2"$',
    )


def test_invalid_first_is_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Last(ID("F1", Location((11, 20))))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:11:20: error: invalid First for field "F2"$',
    )


def test_invalid_first_forward_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=First(Variable("F3", location=Location((10, 20))))),
        Link(Field("F2"), Field("F3")),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: invalid First for field "F2"$',
    )


def test_valid_size_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1"), location=Location((1, 1))),
        Link(
            Field("F1"),
            Field("F2"),
            size=Mul(Number(8), Variable("F1"), location=Location((2, 2))),
            location=Location((2, 2)),
        ),
        Link(Field("F2"), FINAL, location=Location((3, 3))),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((2, 2)))): OPAQUE,
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_invalid_size_forward_reference() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 3)))),
            size=Variable("F2", location=Location((10, 20))),
        ),
        Link(Field(ID("F2", location=Location((3, 3)))), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: error: undefined variable "F2"\n<stdin>:1:1: note: on path "F1"$',
    )


def test_invalid_negative_field_size_1() -> None:
    structure = [
        Link(INITIAL, Field("F1"), location=Location((1, 1))),
        Link(
            Field("F1"),
            Field("F2"),
            size=Sub(Variable("F1"), Number(2), location=Location((2, 2))),
            location=Location((2, 2)),
        ),
        Link(Field("F2"), FINAL, location=Location((3, 3))),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((2, 2)))): OPAQUE,
    }
    regex = (
        '<stdin>:2:2: error: negative size for field "F2"\n'
        '<stdin>:1:1: note: on path "F1"\n'
        '<stdin>:2:2: error: size of opaque field "F2" not multiple of 8 bit\n'
        "<stdin>:2:2: help: sizes are expressed in bits, not bytes\n"
        r'<stdin>:2:2: help: did you mean "(F1 - 2) * 8"?'
    )
    assert_message_model_error(
        structure,
        types,
        f"^{re.escape(regex)}$",
    )


def test_invalid_negative_field_size_2() -> None:
    o = Field(ID("O", location=Location((2, 2))))
    structure = [
        Link(INITIAL, Field("L"), location=Location((1, 1))),
        Link(
            Field("L"),
            o,
            size=Mul(Number(8), Sub(Variable("L"), Number(50)), location=Location((44, 3))),
            location=Location((2, 2)),
        ),
        Link(o, FINAL, location=Location((3, 3))),
    ]
    types = {
        Field(ID("L", location=Location((1, 1)))): models.integer(),
        o: OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:44:3: error: negative size for field "O"\n<stdin>:1:1: note: on path "L"$',
    )


def test_invalid_negative_field_size_3() -> None:
    structure = [
        Link(INITIAL, Field("F1"), location=Location((1, 1))),
        Link(
            Field("F1"),
            Field("F2"),
            size=Sub(Mul(Variable("F1"), Number(8)), Number(16), location=Location((1, 2))),
            location=Location((2, 2)),
        ),
        Link(
            Field("F2"),
            FINAL,
            condition=Greater(Variable("F1"), Number(1), location=Location((3, 2))),
            location=Location((3, 3)),
        ),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((2, 2)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:2: error: negative size for field "F2"\n<stdin>:1:1: note: on path "F1"$',
    )


def test_payload_no_size() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1)))), location=Location((1, 1))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 2)))),
            location=Location((2, 2)),
        ),
        Link(Field(ID("F2", location=Location((3, 3)))), FINAL, location=Location((3, 3))),
    ]
    types = {
        Field(ID("F1", location=Location((1, 1)))): OPAQUE,
        Field(ID("F2", location=Location((2, 2)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:1: error: unconstrained field "F1" without size aspect$',
    )


def test_sequence_no_size() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): models.sequence_integer_vector(),
        Field("F2"): models.sequence_integer_vector(),
    }
    assert_message_model_error(
        structure,
        types,
        '^error: unconstrained field "F1" without size aspect$',
    )


def test_incongruent_overlay() -> None:
    structure = [
        Link(INITIAL, Field("F1"), location=Location((1, 1))),
        Link(Field("F1"), Field("F2"), location=Location((2, 2))),
        Link(Field("F2"), Field("F3"), first=First("F2"), location=Location((3, 3))),
        Link(Field("F3"), Field("F4"), location=Location((4, 4))),
        Link(Field("F4"), FINAL, location=Location((5, 5))),
    ]
    u8 = Integer("P::U8", Number(0), Sub(Pow(Number(2), Number(8)), Number(1)), Number(8))
    u16 = Integer("P::U16", Number(0), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16))
    types = {
        Field(ID("F1", location=Location((1, 1)))): u8,
        Field(ID("F2", location=Location((2, 2)))): u8,
        Field(ID("F3", location=Location((3, 3)))): u16,
        Field(ID("F4", location=Location((4, 4)))): u16,
    }
    regex = (
        '<stdin>:1:1: error: field "F3" not congruent with overlaid field "F2"\n'
        "<stdin>:2:2: note: unsatisfied \"F2'Last = (F1'Last + 1 + 8) - 1\"\n"
        "<stdin>:2:2: note: unsatisfied \"F2'First = F1'Last + 1\"\n"
        "<stdin>:3:3: note: unsatisfied \"(F2'First + 16) - 1 = F2'Last\""
    )
    assert_message_model_error(
        structure,
        types,
        f"^{re.escape(regex)}$",
    )


def test_field_after_message_start(monkeypatch: pytest.MonkeyPatch) -> None:
    structure = [
        Link(INITIAL, Field("F1"), location=Location((1, 1))),
        Link(
            Field("F1"),
            Field("F2"),
            first=Sub(First("Message"), Number(1000)),
            location=Location((2, 2)),
        ),
        Link(Field("F2"), FINAL, location=Location((3, 3))),
    ]

    types = {
        Field(ID("F1", location=Location((1, 1)))): models.integer(),
        Field(ID("F2", location=Location((1, 1)))): models.integer(),
    }
    monkeypatch.setattr(Message, "_verify_links", lambda _: None)
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:1: error: negative start for field "F2"\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:1:1: note: unsatisfied "Message\'First - 1000 >= Message\'First"\n'
        r"<stdin>:1:1: error: negative start for end of message\n"
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:1:1: note: on path "F2"\n'
        r'<stdin>:1:1: note: unsatisfied "F2\'Last = \(Message\'First - 1000 \+ 8\) - 1"\n'
        r'<stdin>:1:1: note: unsatisfied "F2\'Last \+ 1 >= Message\'First"$',
    )


@pytest.mark.parametrize("size", [UNDEFINED, Size(ID("Message", location=Location((1, 1))))])
@pytest.mark.parametrize(
    "condition",
    [
        Equal(Size("Message"), Number(64), location=Location((2, 1))),
        Equal(
            Add(Sub(Last("Message"), First("Message")), Number(1)),
            Number(64),
            location=Location((2, 1)),
        ),
    ],
)
@pytest.mark.parametrize("type_decl", [OPAQUE, models.sequence_integer_vector()])
def test_message_with_implicit_size_single_field(
    size: Expr,
    condition: Expr,
    type_decl: TypeDecl,
) -> None:
    x = Field(ID("X", location=Location((1, 1))))

    structure = [
        Link(INITIAL, x, size=size, location=Location((1, 1))),
        Link(x, FINAL, condition=condition, location=Location((2, 2))),
    ]

    types = {x: type_decl}

    message = Message(
        ID("P::M", Location((1, 1))),
        structure,
        types,
        location=Location((1, 1), end=(1, 2)),
    )

    assert message.structure[0] == Link(INITIAL, x, size=Size("Message"))


@pytest.mark.parametrize(
    "size",
    [UNDEFINED, Sub(Last("Message"), Last("X"), location=Location((1, 1)))],
)
@pytest.mark.parametrize(
    "condition",
    [
        Equal(Size("Message"), Number(64), location=Location((3, 2))),
        Equal(
            Add(Sub(Last("Message"), First("Message")), Number(1)),
            Number(64),
            location=Location((3, 2)),
        ),
    ],
)
@pytest.mark.parametrize("type_decl", [OPAQUE, models.sequence_integer_vector()])
def test_message_with_implicit_size_multiple_fields(
    size: Expr,
    condition: Expr,
    type_decl: TypeDecl,
) -> None:
    x = Field(ID("X", location=Location((1, 1))))
    y = Field(ID("Y", location=Location((2, 2))))

    structure = [
        Link(INITIAL, x, size=Number(8, location=Location((1, 1))), location=Location((1, 1))),
        Link(x, y, size=size, location=Location((2, 2))),
        Link(y, FINAL, condition=condition, location=Location((3, 3))),
    ]

    types = {x: type_decl, y: type_decl}

    message = Message(
        ID("P::M", Location((1, 1))),
        structure,
        types,
        location=Location((1, 1), end=(1, 2)),
    )

    assert message.structure[1] == Link(x, y, size=Sub(Last("Message"), Last("X")))


def test_invalid_use_of_message_attributes() -> None:
    x = Field("X")
    y = Field("Y")

    structure = [
        Link(
            INITIAL,
            x,
            size=Add(Last("Message"), -First("Message"), Number(65), location=Location((1, 2))),
        ),
        Link(
            x,
            y,
            condition=Equal(Size("Message"), Number(64)),
            size=Sub(Size("Message"), Size("X"), location=Location((5, 6))),
        ),
        Link(y, FINAL),
    ]

    types = {x: OPAQUE, y: OPAQUE}

    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:1:2: error: "Message" must not be used in size aspects\n'
        r'<stdin>:5:6: error: invalid use of "Message" in size aspect\n'
        r"<stdin>:5:6: note: remove size aspect to define field with implicit size"
        r"$",
    )


def test_message_identifier_normalization(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(Message, "_check_identifiers", lambda _, _s, _t: None)
    assert str(
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(INITIAL, Field("F"), location=Location((1, 1))),
                Link(
                    Field("f"),
                    Field("g"),
                    Equal(Variable(ID("f")), Literal("A"), Location((4, 1))),
                    Add(Size("f"), Size("p::e"), location=Location((4, 4))),
                    location=Location((2, 2)),
                ),
                Link(
                    Field("F"),
                    FINAL,
                    Equal(Variable("F"), Literal(ID("b")), Location((6, 1))),
                    location=Location((3, 3)),
                ),
                Link(
                    Field("F"),
                    FINAL,
                    And(
                        Equal(Variable("F"), Literal(ID("c")), Location((7, 1))),
                        Equal(Size(ID("p::e")), Number(8), Location((7, 1))),
                        location=Location((7, 1)),
                    ),
                    location=Location((4, 4)),
                ),
                Link(Field("G"), FINAL, location=Location((4, 4))),
            ],
            {
                Field(ID("F", location=Location((1, 1)))): Enumeration(
                    "P::E",
                    [("A", Number(0)), ("B", Number(1)), ("C", Number(2))],
                    Number(8),
                    always_valid=False,
                ),
                Field(ID("G", location=Location((2, 2)))): OPAQUE,
            },
            location=Location((1, 1), end=(1, 2)),
        ),
    ) == textwrap.dedent(
        """\
            type M is
               message
                  F : P::E
                     then null
                        if F = P::B
                     then null
                        if F = P::C
                           and P::E'Size = 8
                     then G
                        with Size => F'Size + P::E'Size
                        if F = P::A;
                  G : Opaque;
               end message""",
    )


def test_message_inconsistent_identifier_casing() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:1: error: casing of "b" differs from casing'
            r' in the declaration of "B" at <stdin>:11:11\n'
            r'<stdin>:11:11: note: declaration of "B"\n'
            r'<stdin>:2:3: error: casing of "c" differs from casing'
            r' in the declaration of "C" at <stdin>:12:12\n'
            r'<stdin>:12:12: note: declaration of "C"\n'
            r'<stdin>:3:4: error: casing of "p::e" differs from cas'
            r'ing in the declaration of "P::E" at <stdin>:13:13\n'
            r'<stdin>:13:13: note: declaration of "P::E"\n'
            r'<stdin>:4:4: error: casing of "g" differs from casing'
            r' in the declaration of "G" at <stdin>:14:14\n'
            r'<stdin>:14:14: note: declaration of "G"\n'
            r'<stdin>:5:5: error: casing of "f" differs from casing'
            r' in the declaration of "F" at <stdin>:15:15\n'
            r'<stdin>:15:15: note: declaration of "F"\n'
            r'<stdin>:6:6: error: casing of "f" differs from casing'
            r' in the declaration of "F" at <stdin>:15:15\n'
            r'<stdin>:15:15: note: declaration of "F"\n'
            r'<stdin>:7:7: error: casing of "p::e" differs from casing'
            r' in the declaration of "P::E" at <stdin>:13:13\n'
            r'<stdin>:13:13: note: declaration of "P::E"'
            r"$"
        ),
    ):
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(INITIAL, Field("F"), location=Location((1, 2))),
                Link(
                    Field("F"),
                    Field(ID("g", location=Location((4, 4)))),
                    Equal(
                        Variable(ID("f", location=Location((5, 5)))),
                        Literal("A"),
                        Location((5, 6)),
                    ),
                    Add(
                        Size(ID("f", location=Location((6, 6)))),
                        Size(ID("p::e", location=Location((7, 7)))),
                        location=Location((8, 8)),
                    ),
                    location=Location((2, 2)),
                ),
                Link(
                    Field("F"),
                    FINAL,
                    Equal(
                        Variable("F"),
                        Literal(ID("b", location=Location((1, 1)))),
                        Location((1, 2)),
                    ),
                    location=Location((3, 3)),
                ),
                Link(
                    Field("F"),
                    FINAL,
                    And(
                        Equal(
                            Variable("F"),
                            Literal(ID("c", location=Location((2, 3)))),
                            Location((2, 4)),
                        ),
                        Equal(
                            Size(ID("p::e", location=Location((3, 4)))),
                            Number(8),
                            Location((3, 5)),
                        ),
                        location=Location((2, 5)),
                    ),
                    location=Location((4, 4)),
                ),
                Link(Field("G"), FINAL, location=Location((5, 5))),
            ],
            {
                Field(ID("F", location=Location((15, 15)))): Enumeration(
                    ID("P::E", location=Location((13, 13))),
                    [
                        ("A", Number(0)),
                        (ID("B", location=Location((11, 11))), Number(1)),
                        (ID("C", location=Location((12, 12))), Number(2)),
                    ],
                    Number(8),
                    always_valid=False,
                ),
                Field(ID("G", location=Location((14, 14)))): OPAQUE,
            },
            location=Location((1, 1), end=(1, 2)),
        )


def test_no_path_to_final() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), Greater(Variable("F1"), Number(100))),
        Link(Field("F2"), Field("F4"), LessEqual(Variable("F1"), Number(100))),
        Link(Field("F3"), FINAL),
    ]

    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
        Field("F4"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        '^error: no path to FINAL for field "F4"$',
    )


def test_no_path_to_final_transitive() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), Greater(Variable("F1"), Number(100))),
        Link(Field("F3"), FINAL),
        Link(Field("F2"), Field("F4"), LessEqual(Variable("F1"), Number(100))),
        Link(Field("F4"), Field("F5")),
        Link(Field("F5"), Field("F6")),
    ]

    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
        Field("F4"): models.integer(),
        Field("F5"): models.integer(),
        Field("F6"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: no path to FINAL for field "F4"\n'
        r'error: no path to FINAL for field "F5"\n'
        r'error: no path to FINAL for field "F6"'
        r"$",
    )


def test_unreachable_field_mod_first() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 2))))),
        Link(
            Field(ID("F1", location=Location((1, 3)))),
            Field(ID("F2", location=Location((1, 4)))),
            Greater(First(ID("F1", location=Location((1, 5)))), First("Message"), Location((2, 2))),
        ),
        Link(Field(ID("F2", location=Location((1, 6)))), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:2: note: on path "F1"\n'
        r'<stdin>:1:1: note: unsatisfied "F1\'First = Message\'First"\n'
        r'<stdin>:2:2: note: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_unreachable_field_mod_last() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 3)))),
        ),
        Link(
            Field(ID("F2", location=Location((3, 3)))),
            FINAL,
            Equal(
                Last(ID("F1", location=Location((3, 4)))),
                Last(ID("Message", location=Location((4, 5)))),
                Location((4, 6)),
            ),
        ),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F2" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:2:3: note: on path "F2"\n'
        r'<stdin>:2:3: note: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'<stdin>:1:1: note: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'<stdin>:4:6: note: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_unreachable_field_range_first() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 3)))),
            Greater(
                First(ID("F1", location=Location((3, 3)))),
                First(ID("Message", location=Location((3, 4)))),
                Location((3, 3)),
            ),
        ),
        Link(Field(ID("F2", location=Location((4, 1)))), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:1:1: note: unsatisfied "F1\'First = Message\'First"\n'
        r'<stdin>:3:3: note: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_unreachable_field_range_last() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 3)))),
        ),
        Link(
            Field(ID("F2", location=Location((3, 3)))),
            FINAL,
            Equal(Last("F1"), Last("Message"), Location((3, 4))),
        ),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F2" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:2:3: note: on path "F2"\n'
        r'<stdin>:2:3: note: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'<stdin>:1:1: note: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'<stdin>:3:4: note: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_unreachable_field_enum_first() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", Location((1, 1))))),
        Link(
            Field(ID("F1", Location((2, 2)))),
            Field(ID("F2", location=Location((2, 3)))),
            Greater(
                First(ID("F1", location=Location((3, 3)))),
                First(ID("Message", location=Location((3, 4)))),
                location=Location((3, 5)),
            ),
        ),
        Link(Field(ID("F2", location=Location((4, 4)))), FINAL),
    ]
    types = {
        Field("F1"): models.enumeration(),
        Field("F2"): models.enumeration(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F1" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:1:1: note: unsatisfied "F1\'First = Message\'First"\n'
        r'<stdin>:3:5: note: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_unreachable_field_enum_last() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(Field("F1"), Field(ID("F2", location=Location((2, 2))))),
        Link(
            Field(ID("F2", location=Location((3, 3)))),
            FINAL,
            Equal(
                Last(ID("F1", location=Location((3, 4)))),
                Last(ID("Message", location=Location((3, 5)))),
                location=Location((3, 6)),
            ),
        ),
    ]
    types = {
        Field("F1"): models.enumeration(),
        Field("F2"): models.enumeration(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F2" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:2:2: note: on path "F2"\n'
        r'<stdin>:2:2: note: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'<stdin>:1:1: note: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'<stdin>:3:6: note: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_unreachable_field_outgoing() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((1, 1))))),
        Link(
            Field(ID("F1", location=Location((2, 2)))),
            Field(ID("F2", location=Location((2, 3)))),
            LessEqual(
                Variable(ID("F1", location=Location((3, 3)))),
                Number(32),
                location=Location((3, 4)),
            ),
        ),
        Link(
            Field(ID("F1", location=Location((4, 4)))),
            FINAL,
            Greater(
                Variable(ID("F1", location=Location((5, 5)))),
                Number(32),
                location=Location((5, 6)),
            ),
        ),
        Link(
            Field(ID("F2", location=Location((6, 6)))),
            FINAL,
            Greater(
                Variable(ID("F1", location=Location((6, 7)))),
                Number(32),
                location=Location((7, 7)),
            ),
        ),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'error: field "F2" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "F1"\n'
        r'<stdin>:2:3: note: on path "F2"\n'
        r'<stdin>:3:4: note: unsatisfied "F1 <= 32"\n'
        r'<stdin>:7:7: note: unsatisfied "F1 > 32"'
        r"$",
    )


def test_unreachable_field_outgoing_multi() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", Location((86, 13))))),
        Link(
            Field(ID("F1", location=Location((90, 13)))),
            Field(ID("F2", location=Location((91, 13)))),
            LessEqual(
                Variable(ID("F1", location=Location((66, 3)))),
                Number(32),
                Location((66, 3)),
            ),
        ),
        Link(
            Field(ID("F1", location=Location((21, 30)))),
            Field(ID("F3", location=Location((21, 30)))),
            Greater(
                Variable(ID("F1", location=Location((21, 30)))),
                Number(32),
                location=Location((22, 30)),
            ),
        ),
        Link(
            Field(ID("F2", location=Location((23, 30)))),
            Field(ID("F3", location=Location((23, 30)))),
            And(
                Greater(Variable("F1"), Number(32), location=Location((22, 34))),
                LessEqual(Variable("F1"), Number(48), location=Location((22, 34))),
                location=Location((22, 34)),
            ),
        ),
        Link(
            Field(ID("F2", location=Location((23, 34)))),
            FINAL,
            Greater(
                Variable(ID("F1", location=Location((24, 34)))),
                Number(48),
                location=Location((24, 34)),
            ),
        ),
        Link(Field(ID("F3", location=Location((25, 34)))), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
        Field(ID("F2", Location((90, 12)))): models.integer(),
        Field("F3"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:90:12: error: field "F2" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:86:13: note: on path "F1"\n'
        r'<stdin>:91:13: note: on path "F2"\n'
        r'<stdin>:66:3: note: unsatisfied "F1 <= 32"\n'
        r'<stdin>:22:34: note: unsatisfied "F1 > 32"'
        r"$",
    )


def test_size_aspect_final() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", location=Location((3, 12))))),
        Link(
            Field(ID("F1", location=Location((3, 13)))),
            Field(ID("F2", location=Location((3, 14)))),
        ),
        Link(
            Field(ID("F2", location=Location((4, 13)))),
            FINAL,
            size=Number(100, location=Location((4, 12))),
        ),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        "^<stdin>:4:12: error: size aspect for final field$",
    )


def test_aggregate_equal_valid_size() -> None:
    structure = [
        Link(
            INITIAL,
            Field(ID("Magic", location=Location((1, 1)))),
            size=Number(40, location=Location((1, 1))),
            location=Location((1, 1)),
        ),
        Link(
            Field(ID("Magic", location=Location((2, 2)))),
            FINAL,
            condition=Equal(
                Variable(ID("Magic", location=Location((3, 3)))),
                Aggregate(Number(1), Number(2), Number(3), Number(4), Number(4)),
                location=Location((4, 4)),
            ),
            location=Location((2, 2)),
        ),
    ]
    types = {
        Field(ID("Magic", location=Location((1, 1)))): OPAQUE,
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_aggregate_equal_invalid_size1() -> None:
    structure = [
        Link(
            INITIAL,
            Field(ID("Magic", location=Location((1, 1)))),
            size=Number(40, location=Location((1, 2))),
            location=Location((1, 3)),
        ),
        Link(
            Field(ID("Magic", location=Location((2, 2)))),
            FINAL,
            condition=Equal(
                Variable("Magic"),
                Aggregate(Number(1), Number(2)),
                location=Location((3, 3)),
            ),
            location=Location((2, 3)),
        ),
    ]
    types = {
        Field(ID("Magic", location=Location((1, 1)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:1:1: error: field "Magic" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "Magic"\n'
        r'<stdin>:1:2: note: unsatisfied "Magic\'Size = 40"\n'
        r'<stdin>:3:3: note: unsatisfied "2 [*] 8 = Magic\'Size"'
        r"$",
    )


def test_aggregate_equal_invalid_size2() -> None:
    structure = [
        Link(
            INITIAL,
            Field(ID("Magic", location=Location((1, 1)))),
            size=Number(40, location=Location((1, 2))),
            location=Location((1, 3)),
        ),
        Link(
            Field(ID("Magic", location=Location((2, 2)))),
            FINAL,
            condition=Equal(
                Aggregate(Number(1), Number(2)),
                Variable(ID("Magic", location=Location((3, 3)))),
                location=Location((3, 4)),
            ),
            location=Location((2, 3)),
        ),
    ]
    types = {
        Field(ID("Magic", location=Location((1, 1)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:1:1: error: field "Magic" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "Magic"\n'
        r'<stdin>:1:2: note: unsatisfied "Magic\'Size = 40"\n'
        r'<stdin>:3:4: note: unsatisfied "2 [*] 8 = Magic\'Size"'
        r"$",
    )


def test_aggregate_inequal_valid_size() -> None:
    structure = [
        Link(
            INITIAL,
            Field(ID("Magic", location=Location((1, 1)))),
            size=Number(40, location=Location((1, 2))),
            location=Location((1, 3)),
        ),
        Link(
            Field(ID("Magic", location=Location((2, 2)))),
            FINAL,
            condition=NotEqual(
                Variable("Magic"),
                Aggregate(Number(1), Number(2), Number(3), Number(4), Number(4)),
                location=Location((3, 3)),
            ),
            location=Location((2, 3)),
        ),
    ]
    types = {
        Field(ID("Magic", location=Location((1, 1)))): OPAQUE,
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_aggregate_inequal_invalid_size() -> None:
    structure = [
        Link(
            INITIAL,
            Field(ID("Magic", location=Location((1, 1)))),
            size=Number(40, location=Location((1, 2))),
            location=Location((1, 3)),
        ),
        Link(
            Field(ID("Magic", location=Location((2, 2)))),
            FINAL,
            condition=NotEqual(
                Variable(ID("Magic", location=Location((3, 3)))),
                Aggregate(Number(1), Number(2)),
                location=Location((3, 4)),
            ),
            location=Location((2, 3)),
        ),
    ]
    types = {
        Field(ID("Magic", location=Location((1, 2)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:1:2: error: field "Magic" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:1:1: note: on path "Magic"\n'
        r'<stdin>:1:2: note: unsatisfied "Magic\'Size = 40"\n'
        r'<stdin>:3:4: note: unsatisfied "2 [*] 8 = Magic\'Size"'
        r"$",
    )


def test_aggregate_equal_sequence_valid_size() -> None:
    structure = [
        Link(INITIAL, Field(ID("Magic", location=Location((1, 1)))), size=Number(16)),
        Link(
            Field(ID("Magic", location=Location((2, 2)))),
            FINAL,
            condition=NotEqual(
                Variable(ID("Magic", location=Location((3, 3)))),
                Aggregate(Number(1), Number(2)),
                location=Location((3, 3)),
            ),
        ),
    ]
    types = {
        Field("Magic"): Sequence(
            "P::Arr",
            Integer("P::Integer", Number(0), Number(127), Number(8)),
        ),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_aggregate_equal_sequence_invalid_size() -> None:
    magic = Field(ID("Magic", Location((3, 5))))
    structure = [
        Link(INITIAL, magic, size=Number(40, location=Location((19, 17)))),
        Link(
            magic,
            FINAL,
            condition=NotEqual(
                Variable("Magic"),
                Aggregate(Number(1), Number(2)),
                Location((17, 3)),
            ),
        ),
    ]
    types = {
        magic: Sequence(
            "P::Arr",
            Integer("P::Integer", Number(0), Number(127), Number(8), location=Location((66, 3))),
        ),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:3:5: error: field "Magic" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:3:5: note: on path "Magic"\n'
        r'<stdin>:19:17: note: unsatisfied "Magic\'Size = 40"\n'
        r'<stdin>:66:3: note: unsatisfied "Integer\'Size = 8"\n'
        r'<stdin>:17:3: note: unsatisfied "2 [*] Integer\'Size = Magic\'Size"'
        r"$",
    )


def test_aggregate_equal_invalid_size_field() -> None:
    length = Field(ID("Length", Location((2, 5))))
    magic = Field(ID("Magic", Location((3, 5))))

    structure = [
        Link(INITIAL, length, location=Location((1, 1))),
        Link(
            length,
            magic,
            size=Mul(Number(8), Variable("Length"), location=Location((6, 5))),
            location=Location((2, 2)),
        ),
        Link(
            magic,
            FINAL,
            condition=Equal(
                Variable("Magic"),
                Aggregate(Number(1), Number(2)),
                location=Location((10, 5)),
            ),
            location=Location((3, 3)),
        ),
    ]
    types = {
        Field(ID("Length", location=Location((16, 3)))): Integer(
            "P::Length_Type",
            Number(10),
            Number(100),
            Number(8),
            Location((5, 10)),
        ),
        Field(ID("Magic", Location((17, 3)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:17:3: error: field "Magic" has no satisfiable condition at any outgoing link\n'
        r'<stdin>:2:5: note: on path "Length"\n'
        r'<stdin>:3:5: note: on path "Magic"\n'
        r'<stdin>:10:5: note: unsatisfied "2 [*] 8 = Magic\'Size"\n'
        r'<stdin>:5:10: note: unsatisfied "Length >= 10"\n'
        r'<stdin>:6:5: note: unsatisfied "Magic\'Size = 8 [*] Length"'
        r"$",
    )


def test_no_unreachable_field_multi() -> None:
    structure = [
        Link(INITIAL, Field(ID("F0", location=Location((1, 1))))),
        Link(
            Field(ID("F0", location=Location((2, 2)))),
            Field(ID("F1", location=Location((2, 2)))),
            condition=Equal(
                Variable(ID("F0", location=Location((3, 3)))),
                Number(1),
                location=Location((3, 3)),
            ),
        ),
        Link(
            Field(ID("F0", location=Location((4, 4)))),
            Field(ID("F2", location=Location((4, 4)))),
            condition=Equal(
                Variable(ID("F0", location=Location((5, 5)))),
                Number(2),
                location=Location((5, 5)),
            ),
        ),
        Link(
            Field(ID("F1", location=Location((6, 6)))),
            Field(ID("F3", location=Location((6, 6)))),
        ),
        Link(
            Field(ID("F2", location=Location((7, 7)))),
            Field(ID("F3", location=Location((7, 7)))),
        ),
        Link(
            Field(ID("F3", location=Location((8, 8)))),
            Field(ID("F4", location=Location((8, 8)))),
            condition=Equal(
                Variable(ID("F0", location=Location((9, 9)))),
                Number(1),
                location=Location((9, 9)),
            ),
        ),
        Link(
            Field(ID("F3", location=Location((10, 10)))),
            Field(ID("F5", location=Location((10, 10)))),
            condition=Equal(
                Variable(ID("F0", location=Location((11, 11)))),
                Number(2),
                location=Location((11, 11)),
            ),
        ),
        Link(Field(ID("F4", location=Location((12, 12)))), FINAL),
        Link(Field(ID("F5", location=Location((12, 12)))), FINAL),
    ]
    types = {
        Field("F0"): models.integer(),
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
        Field("F4"): models.integer(),
        Field("F5"): models.integer(),
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


@pytest.mark.parametrize(
    "condition",
    [
        Equal(
            Variable(ID("Flag", location=Location((10, 10)))),
            Number(1),
            location=Location((10, 10)),
        ),
        And(
            Equal(
                Variable(ID("Flag", location=Location((10, 10))), location=Location((10, 10))),
                Number(1),
            ),
            Greater(
                Variable(ID("Opt1", location=Location((10, 10)))),
                Number(0),
                location=Location((10, 10)),
            ),
            location=Location((10, 10)),
        ),
    ],
)
def test_discontiguous_optional_fields(condition: Expr) -> None:
    structure = [
        Link(INITIAL, Field(ID("Flag", location=Location((1, 1)))), location=Location((1, 1))),
        Link(
            Field(ID("Flag", location=Location((2, 2)))),
            Field(ID("Opt1", location=Location((2, 2)))),
            condition=Equal(
                Variable(ID("Flag", location=Location((3, 3)))),
                Number(1),
                location=Location((3, 3)),
            ),
            location=Location((2, 2)),
        ),
        Link(
            Field(ID("Flag", location=Location((4, 4)))),
            Field(ID("Data", location=Location((4, 4)))),
            condition=NotEqual(
                Variable(ID("Flag", location=Location((5, 5)))),
                Number(1),
                location=Location((5, 5)),
            ),
            location=Location((3, 3)),
        ),
        Link(
            Field(ID("Opt1", location=Location((6, 6)))),
            Field(ID("Data", location=Location((6, 6)))),
            location=Location((4, 4)),
        ),
        Link(
            Field(ID("Data", location=Location((7, 7)))),
            Field(ID("Opt2", location=Location((7, 7)))),
            condition=condition,
            size=Mul(
                Variable(ID("Opt1", location=Location((9, 9)))),
                Number(8),
                location=Location((9, 9)),
            ),
            location=Location((7, 7)),
        ),
        Link(
            Field(ID("Data", location=Location((10, 10)))),
            FINAL,
            condition=Equal(
                Variable(ID("Flag", location=Location((11, 11)))),
                Number(0),
                location=Location((11, 11)),
            ),
            location=Location((11, 11)),
        ),
        Link(
            Field(ID("Opt2", location=Location((12, 12)))),
            FINAL,
            location=Location((12, 12)),
        ),
    ]
    types = {
        Field(ID("Flag", location=Location((1, 1)))): models.integer(),
        Field(ID("Opt1", location=Location((2, 2)))): models.integer(),
        Field(ID("Data", location=Location((3, 3)))): models.integer(),
        Field(ID("Opt2", location=Location((4, 4)))): OPAQUE,
    }
    Message(ID("P::M", Location((1, 1))), structure, types, location=Location((1, 1), end=(1, 2)))


def test_discontiguous_optional_fields_error() -> None:
    # TODO(eng/recordflux/RecordFlux#499): Enable disjunctions with references to optional fields
    structure = [
        Link(INITIAL, Field("Flag"), location=Location((1, 1))),
        Link(
            Field("Flag"),
            Field("Opt1"),
            condition=Equal(Variable("Flag"), Number(1)),
            location=Location((2, 2)),
        ),
        Link(
            Field("Flag"),
            Field("Data"),
            condition=NotEqual(Variable("Flag"), Number(1)),
            location=Location((3, 3)),
        ),
        Link(Field("Opt1"), Field("Data")),
        Link(
            Field("Data"),
            Field("Opt2"),
            condition=Or(
                And(Equal(Variable("Flag"), Number(1)), Greater(Variable("Opt1"), Number(100))),
                Greater(Variable("Flag"), Number(1)),
            ),
            size=Mul(Variable("Opt1"), Number(8), location=Location((4, 4))),
            location=Location((4, 4)),
        ),
        Link(
            Field("Data"),
            FINAL,
            condition=Equal(Variable("Flag"), Number(0)),
            location=Location((5, 5)),
        ),
        Link(
            Field("Opt2"),
            FINAL,
            location=Location((6, 6)),
        ),
    ]
    types = {
        Field(ID("Flag", location=Location((1, 1)))): models.integer(),
        Field(ID("Opt1", location=Location((2, 2)))): models.integer(),
        Field(ID("Data", location=Location((3, 3)))): models.integer(),
        Field(ID("Opt2", location=Location((4, 4)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        (
            r"^"
            r'<stdin>:2:2: error: undefined variable "Opt1"\n'
            r'<stdin>:1:1: note: on path "Flag"\n'
            r'<stdin>:3:3: note: on path "Data"\n'
            r'<stdin>:2:2: error: undefined variable "Opt1"\n'
            r'<stdin>:1:1: note: on path "Flag"\n'
            r'<stdin>:3:3: note: on path "Data"'
            r"$"
        ),
    )


@pytest.mark.parametrize(
    ("checksums", "condition"),
    [
        (
            {
                ID("F3"): [
                    ValueRange(First("F1"), Last("F2")),
                    Variable("F2"),
                    ValueRange(Add(Last("F1"), Number(1)), Sub(First("F3"), Number(1))),
                    ValueRange(First("F3"), Last("F3")),
                ],
            },
            ValidChecksum(ID("F3", location=Location((4, 3)))),
        ),
        (
            {ID("F2"): [Variable("F1")], ID("F3"): [Variable("F1"), Size("F2")]},
            And(ValidChecksum("F2"), ValidChecksum("F3"), location=Location((4, 3))),
        ),
        (
            {
                ID("F3"): [
                    ValueRange(First("Message"), Sub(First("F3"), Number(1))),
                    ValueRange(First("F3"), Last("Message")),
                ],
            },
            ValidChecksum(ID("F3", location=Location((4, 3)))),
        ),
    ],
)
def test_checksum(checksums: abc.Mapping[ID, abc.Sequence[Expr]], condition: Expr) -> None:
    f1 = Field(ID("F1", location=Location((1, 1))))
    f2 = Field(ID("F2", location=Location((2, 2))))
    f3 = Field(ID("F3", location=Location((3, 3))))
    structure = [
        Link(INITIAL, f1, location=Location((1, 1))),
        Link(f1, f2, location=Location((2, 2))),
        Link(f2, f3, location=Location((3, 3))),
        Link(f3, FINAL, condition, location=Location((4, 4))),
    ]
    types = {f1: models.integer(), f2: models.integer(), f3: models.integer()}
    message = Message(
        ID("P::M", Location((1, 1))),
        structure,
        types,
        checksums=checksums,
        location=Location((1, 1), end=(1, 2)),
    )
    assert message.checksums == checksums


@pytest.mark.parametrize(
    ("checksums", "condition", "error"),
    [
        (
            {ID("X", location=Location((10, 20))): []},
            TRUE,
            r'^<stdin>:10:20: error: checksum definition for unknown field "X"\n'
            r'<stdin>:10:20: error: no validity check of checksum "X"$',
        ),
        (
            {ID("F3", location=Location((10, 20))): []},
            TRUE,
            r'^<stdin>:10:20: error: no validity check of checksum "F3"$',
        ),
        (
            {},
            ValidChecksum(ID("F2", location=Location((20, 30)))),
            r'^<stdin>:20:30: error: validity check for undefined checksum "F2"$',
        ),
        (
            {ID("F3"): [First(ID("F2", location=Location((20, 30))))]},
            ValidChecksum("F3"),
            r'^<stdin>:20:30: error: unsupported expression "F2\'First"'
            r' in definition of checksum "F3"$',
        ),
        (
            {ID("F3"): [Variable("X", location=Location((20, 30)))]},
            ValidChecksum("F3"),
            r'^<stdin>:20:30: error: unknown field "X" referenced'
            r' in definition of checksum "F3"$',
        ),
        (
            {ID("F3"): [ValueRange(First("F2"), Last("F1"), location=Location((20, 30)))]},
            ValidChecksum("F3"),
            r'^<stdin>:20:30: error: invalid range "F2\'First .. F1\'Last"'
            r' in definition of checksum "F3"$',
        ),
    ],
)
def test_checksum_error(
    checksums: abc.Mapping[ID, abc.Sequence[Expr]],
    condition: Expr,
    error: str,
) -> None:
    f1 = Field("F1")
    f2 = Field("F2")
    f3 = Field("F3")
    structure = [
        Link(INITIAL, f1),
        Link(f1, f2),
        Link(f2, f3),
        Link(f3, FINAL, condition),
    ]
    types = {f1: models.integer(), f2: models.integer(), f3: models.integer()}
    assert_message_model_error(structure, types, error, checksums=checksums)


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_field_size() -> None:
    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("A"), location=Location((1, 1))),
            Link(
                Field("A"),
                Field("B"),
                size=Mul(Size("A"), Number(8), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(Field("B"), Field("C"), location=Location((3, 3))),
            Link(Field("C"), FINAL, location=Location((4, 4))),
        ],
        {
            Field(ID("A", location=Location((1, 1)))): models.integer(),
            Field(ID("B", location=Location((2, 2)))): OPAQUE,
            Field(ID("C", location=Location((3, 3)))): OPAQUE,
        },
        location=Location((30, 10), end=(30, 11)),
    )

    assert message.field_size(FINAL) == Number(0)
    assert message.field_size(Field("A")) == Number(8)
    assert message.field_size(Field("B")) == Number(64)

    with pytest.raises(
        RecordFluxError,
        match=(r'^<stdin>:10:20: error: unable to calculate size of field "C" of message "P::M"$'),
    ):
        message.field_size(Field(ID("C", location=Location((10, 20)))))

    with pytest.raises(AssertionError, match='^field "X" not found$'):
        message.field_size(Field("X"))


def test_target_last_opt() -> None:
    link_ia = Link(INITIAL, Field("A"), location=Location((1, 1)))
    link_ab = Link(
        Field("A"),
        Field("B"),
        size=Mul(Size("A"), Number(8), location=Location((2, 2))),
        location=Location((2, 2)),
    )
    link_bc = Link(Field("B"), Field("C"), location=Location((3, 3)))
    message = Message(
        ID("P::M", Location((1, 1))),
        [link_ia, link_ab, link_bc, Link(Field("C"), FINAL, location=Location((4, 4)))],
        {
            Field(ID("A", location=Location((1, 1)))): models.integer(),
            Field(ID("B", location=Location((2, 2)))): OPAQUE,
            Field(ID("C", location=Location((3, 3)))): OPAQUE,
        },
        location=Location((30, 10), end=(30, 11)),
    )
    assert message._target_last_opt(link_ia) == Sub(  # noqa: SLF001
        Add(First("Message"), Number(8)),
        Number(1),
    )
    assert message._target_last_opt(link_ab) == Sub(  # noqa: SLF001
        Add(Add(Last("A"), Number(1)), Mul(Size("A"), Number(8))),
        Number(1),
    )
    assert message._target_last_opt(link_bc) is None  # noqa: SLF001


def test_copy() -> None:
    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("F"), location=Location((2, 2))),
            Link(Field("F"), FINAL, location=Location((3, 3))),
        ],
        {Field(ID("F", location=Location((1, 1)))): models.integer()},
        location=Location((1, 1), end=(1, 2)),
    )
    assert_equal(
        message.copy(identifier=ID("A::B", Location((1, 1)))),
        Message(
            ID("A::B", Location((1, 1))),
            [
                Link(INITIAL, Field("F"), location=Location((2, 2))),
                Link(Field("F"), FINAL, location=Location((2, 2))),
            ],
            {Field(ID("F", location=Location((1, 1)))): models.integer()},
            location=Location((1, 1), end=(1, 2)),
        ),
    )
    assert_equal(
        message.copy(
            structure=[
                Link(INITIAL, Field("C"), location=Location((1, 1))),
                Link(Field("C"), FINAL, location=Location((2, 2))),
            ],
            types={Field(ID("C", location=Location((1, 1)))): models.integer()},
            byte_order={Field("C"): ByteOrder.HIGH_ORDER_FIRST},
        ),
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(INITIAL, Field("C"), location=Location((2, 2))),
                Link(Field("C"), FINAL, location=Location((3, 3))),
            ],
            {Field(ID("C", location=Location((1, 1)))): models.integer()},
            location=Location((1, 1), end=(1, 2)),
        ),
    )
    assert_equal(
        message.copy(
            structure=[
                Link(INITIAL, Field("C"), location=Location((1, 1))),
                Link(Field("C"), FINAL, location=Location((2, 2))),
            ],
            types={Field(ID("C", location=Location((1, 1)))): models.integer()},
            byte_order={Field("C"): ByteOrder.LOW_ORDER_FIRST},
        ),
        Message(
            ID("P::M", Location((1, 1))),
            [
                Link(INITIAL, Field("C"), location=Location((2, 2))),
                Link(Field("C"), FINAL, location=Location((3, 3))),
            ],
            {Field(ID("C", location=Location((1, 1)))): models.integer()},
            byte_order=ByteOrder.LOW_ORDER_FIRST,
            location=Location((1, 1), end=(1, 2)),
        ),
    )


def test_is_possibly_empty() -> None:
    a = Field(ID("A", location=Location((1, 1))))
    b = Field(ID("B", location=Location((2, 2))))
    c = Field(ID("C", location=Location((3, 3))))

    integer = Integer("P::Integer", Number(0), Number(100), Number(8))
    sequence = Sequence("P::Sequence", integer)

    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, a),
            Link(
                a,
                c,
                condition=Less(
                    Variable(ID("A", location=Location((1, 1)))),
                    Number(10),
                    location=Location((2, 2)),
                ),
                size=Variable(ID("A", location=Location((1, 1)))),
            ),
            Link(
                a,
                b,
                condition=Greater(
                    Variable(ID("A", location=Location((3, 3)))),
                    Number(20),
                    location=Location((3, 3)),
                ),
                size=Variable(ID("A", location=Location((3, 3)))),
            ),
            Link(b, c, size=Variable(ID("A", location=Location((4, 4))))),
            Link(c, FINAL),
        ],
        {a: integer, b: sequence, c: sequence},
        location=Location((1, 1), end=(1, 2)),
    )

    assert not message.is_possibly_empty(a)
    assert not message.is_possibly_empty(b)
    assert message.is_possibly_empty(c)


def test_has_fixed_size() -> None:
    assert models.null_message().has_fixed_size
    assert models.fixed_size_message().has_fixed_size
    assert not models.tlv_message().has_fixed_size
    assert not models.ethernet_frame().has_fixed_size
    assert not models.sequence_message().has_fixed_size


def test_has_implicit_size() -> None:
    assert not models.null_message().has_implicit_size
    assert not models.fixed_size_message().has_implicit_size
    assert not models.tlv_message().has_implicit_size
    assert models.ethernet_frame().has_implicit_size
    assert not models.sequence_message().has_implicit_size


def test_is_definite() -> None:
    assert models.null_message().is_definite
    assert models.fixed_size_simple_message().is_definite
    assert models.definite_message().is_definite
    assert not models.fixed_size_message().is_definite
    assert not models.tlv_message().is_definite
    assert not models.ethernet_frame().is_definite
    assert not models.sequence_message().is_definite


def test_size() -> None:
    assert models.null_message().size() == Number(0)
    assert models.fixed_size_message().size() == Number(200)
    assert models.definite_message().size() == Add(Mul(Variable("Length"), Number(8)), Number(16))
    assert models.definite_message().size({Field("Length"): Number(8)}) == Number(80)
    assert models.tlv_message().size({Field("Tag"): Literal("TLV::Msg_Error")}) == Number(8)
    assert models.tlv_message().size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Number(4),
            Field("Value"): Aggregate(*[Number(0)] * 4),
        },
    ) == Number(56)
    assert models.tlv_message().size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Div(Add(Size("Tag"), Size("TLV::Length")), Number(8)),
            Field("Value"): Aggregate(*[Number(0)] * 3),
        },
    ) == Number(48)
    assert models.tlv_message().size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Add(Div(Size("X"), Number(8)), Variable("Y")),
            Field("Value"): Variable("Z"),
        },
    ) == Add(Size("Z"), Number(24))
    assert models.tlv_message().size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Div(Size("Msg_Data"), Number(8)),
            Field("Value"): Opaque("Msg_Data"),
        },
    ) == Add(Mul(Div(Size("Msg_Data"), Number(8)), Number(8)), Number(24))
    assert models.tlv_message().size(
        {Field("Tag"): Variable("X"), Field("Length"): Variable("Y")},
    ) == Add(
        Mul(Variable("Y"), Number(8)),
        Number(24),
    )

    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(46),
            Field("Type_Length"): Number(46),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
    ) == Number(480)
    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(0x8100),
            Field("TPID"): Number(0x8100),
            Field("TCI"): Number(0),
            Field("Type_Length"): Number(46),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
    ) == Number(512)
    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(1536),
            Field("Type_Length"): Number(1536),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
    ) == Number(480)
    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(1536),
            Field("Type_Length"): Number(1536),
            Field("Payload"): Variable("Payload"),
        },
    ) == Add(Size("Payload"), Number(112))

    variable_field_value = Message(
        ID("Test::Message", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("Length", location=Location((2, 2)))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Length", location=Location((2, 2)))),
                Field(ID("Data", location=Location((2, 2)))),
                condition=Variable(
                    ID("Has_Data", location=Location((3, 3))),
                    location=Location((3, 3)),
                ),
                size=Mul(
                    Variable(ID("Length", location=Location((4, 4)))),
                    Number(8),
                    location=Location((4, 4)),
                ),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Length", location=Location((5, 5)))),
                Field(ID("Data", location=Location((5, 5)))),
                condition=Not(
                    Variable(ID("Has_Data", location=Location((6, 6)))),
                    location=Location((6, 6)),
                ),
                size=Number(0, location=Location((6, 6))),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("Data", location=Location((7, 7)))),
                FINAL,
                location=Location((7, 7)),
            ),
        ],
        {
            Field(ID("Has_Data", location=Location((1, 1)))): BOOLEAN,
            Field(ID("Length", location=Location((2, 2)))): models.tlv_length(),
            Field(ID("Data", location=Location((3, 3)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert variable_field_value.size(
        {
            Field("Has_Data"): Variable("X"),
            Field("Length"): Variable("Y"),
            Field("Data"): Selected(Variable("M"), "F"),
        },
    ) == Add(
        IfExpr(
            [
                (
                    Or(Variable("X"), Not(Variable("X"))),
                    Size(Selected(Variable("M"), "F")),
                ),
            ],
            Number(0),
        ),
        Number(16),
    )
    assert variable_field_value.size(
        {
            Field("Has_Data"): Variable("X"),
            Field("Length"): Variable("Y"),
            Field("Data"): Variable("Z"),
        },
    ) == Add(
        IfExpr(
            [(Or(Variable("X"), Not(Variable("X"))), Size(Variable("Z")))],
            Number(0),
        ),
        Number(16),
    )

    optional_overlayed_field = Message(
        ID("Test::Message", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("A", location=Location((2, 2)))), location=Location((2, 2))),
            Link(
                Field(ID("A", location=Location((3, 3)))),
                Field(ID("B", location=Location((3, 3)))),
                condition=Equal(
                    Variable(ID("A", location=Location((4, 4)))),
                    Number(0),
                    location=Location((4, 4)),
                ),
                first=First(Variable(ID("A", location=Location((5, 5))))),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("A", location=Location((6, 6)))),
                Field(ID("B", location=Location((6, 6)))),
                condition=Greater(
                    Variable(ID("A", location=Location((7, 7)))),
                    Number(0),
                    location=Location((7, 7)),
                ),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("B", location=Location((8, 8)))),
                FINAL,
                location=Location((7, 7)),
            ),
        ],
        {
            Field(ID("A", location=Location((1, 1)))): models.tlv_length(),
            Field(ID("B", location=Location((2, 2)))): models.tlv_length(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert optional_overlayed_field.size(
        {
            Field("A"): Number(0),
            Field("B"): Number(0),
        },
    ) == Number(16)
    assert optional_overlayed_field.size(
        {
            Field("A"): Number(1),
            Field("B"): Number(2),
        },
    ) == Number(32)
    assert optional_overlayed_field.size() == Add(
        IfExpr(
            [
                (
                    And(Greater(Variable("A"), Number(0)), NotEqual(Variable("A"), Number(0))),
                    Number(16),
                ),
            ],
            Number(0),
        ),
        Number(16),
    )

    path_dependent_fields = Message(
        ID("Test::Message", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("A", location=Location((2, 2))))),
            Link(
                Field(ID("A", location=Location((3, 3)))),
                Field(ID("B", location=Location((3, 3)))),
                condition=Equal(
                    Variable(ID("A", location=Location((4, 4)))),
                    Number(0),
                    location=Location((4, 4)),
                ),
            ),
            Link(
                Field(ID("A", location=Location((5, 5)))),
                Field(ID("C", location=Location((5, 5)))),
                condition=Greater(
                    Variable(ID("A", location=Location((6, 6)))),
                    Number(0),
                    location=Location((6, 6)),
                ),
            ),
            Link(
                Field(ID("B", location=Location((7, 7)))),
                FINAL,
            ),
            Link(
                Field(ID("C", location=Location((8, 8)))),
                FINAL,
            ),
        ],
        {
            Field("A"): models.tlv_length(),
            Field("B"): models.tlv_length(),
            Field("C"): models.tlv_length(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert path_dependent_fields.size({Field("A"): Variable("X"), Field("B"): Number(0)}) == Number(
        32,
    )
    assert path_dependent_fields.size({Field("A"): Variable("X")}) == Add(
        IfExpr(
            [(Or(Equal(Variable("X"), Number(0)), Greater(Variable("X"), Number(0))), Number(16))],
            Number(0),
        ),
        Number(16),
    )


def test_size_subpath() -> None:
    assert models.null_message().size({}, subpath=True) == Number(0)

    assert models.fixed_size_message().size(
        {
            Field("Message_Type"): Literal("Universal::MT_Data"),
        },
        subpath=True,
    ) == Number(8)
    assert models.fixed_size_message().size(
        {
            Field("Data"): Aggregate(*[Number(0)] * 4),
        },
        subpath=True,
    ) == Number(32)
    assert models.fixed_size_message().size(
        {
            Field("Message_Type"): Literal("Universal::MT_Data"),
            Field("Data"): Aggregate(*[Number(0)] * 4),
        },
        subpath=True,
    ) == Number(40)

    assert models.definite_message().size(
        {
            Field("Length"): Number(8),
        },
        subpath=True,
    ) == Number(16)
    assert models.definite_message().size(
        {
            Field("Data"): Variable("D"),
        },
        subpath=True,
    ) == Size("D")
    assert models.definite_message().size(
        {
            Field("Length"): Variable("L"),
            Field("Data"): Variable("D"),
        },
        subpath=True,
    ) == Add(Size("D"), Number(16))

    assert models.tlv_message().size(
        {
            Field("Tag"): Variable("T"),
        },
        subpath=True,
    ) == Number(8)
    assert models.tlv_message().size(
        {
            Field("Tag"): Variable("T"),
            Field("Length"): Variable("L"),
            Field("Value"): Aggregate(*[Number(0)] * 4),
        },
        subpath=True,
    ) == Number(56)
    assert models.tlv_message().size(
        {
            Field("Tag"): Variable("T"),
            Field("Length"): Variable("L"),
        },
        subpath=True,
    ) == Number(24)
    assert models.tlv_message().size(
        {
            Field("Length"): Div(Add(Size("Tag"), Size("TLV::Length")), Number(8)),
            Field("Value"): Aggregate(*[Number(0)] * 3),
        },
        subpath=True,
    ) == Number(40)
    assert models.tlv_message().size(
        {
            Field("Length"): Add(Div(Size("X"), Number(8)), Variable("Y")),
            Field("Value"): Variable("Z"),
        },
        subpath=True,
    ) == Add(Size("Z"), Number(16))
    assert models.tlv_message().size(
        {
            Field("Length"): Div(Size("Msg_Data"), Number(8)),
            Field("Value"): Opaque("Msg_Data"),
        },
        subpath=True,
    ) == Add(Mul(Div(Size("Msg_Data"), Number(8)), Number(8)), Number(16))

    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(46),
            Field("Type_Length"): Number(46),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
        subpath=True,
    ) == Number(480)
    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(0x8100),
            Field("TPID"): Number(0x8100),
            Field("TCI"): Number(0),
            Field("Type_Length"): Number(46),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
        subpath=True,
    ) == Number(512)
    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(1536),
            Field("Type_Length"): Number(1536),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
        subpath=True,
    ) == Number(480)
    assert models.ethernet_frame().size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(1536),
            Field("Type_Length"): Number(1536),
            Field("Payload"): Variable("Payload"),
        },
        subpath=True,
    ) == Add(Size("Payload"), Number(112))

    variable_field_value = Message(
        ID("Test::Message", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("Length", location=Location((2, 2)))),
                location=Location((1, 1)),
            ),
            Link(
                Field(ID("Length", location=Location((3, 3)))),
                Field(ID("Data", location=Location((3, 3)))),
                condition=Equal(
                    Variable(ID("Has_Data", location=Location((4, 4)))),
                    TRUE,
                    location=Location((4, 4)),
                ),
                size=Mul(
                    Variable("Length", location=Location((5, 5))),
                    Number(8),
                    location=Location((5, 5)),
                ),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("Length", location=Location((6, 6)))),
                Field(ID("Data", location=Location((6, 6)))),
                condition=Equal(
                    Variable("Has_Data", location=Location((7, 7))),
                    FALSE,
                    location=Location((7, 7)),
                ),
                size=Number(0, location=Location((7, 7))),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("Data", location=Location((8, 8)))),
                FINAL,
                location=Location((8, 8)),
            ),
        ],
        {
            Field(ID("Has_Data", location=Location((1, 1)))): BOOLEAN,
            Field(ID("Length", location=Location((2, 2)))): models.tlv_length(),
            Field(ID("Data", location=Location((3, 3)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert variable_field_value.size(
        {
            Field("Length"): Variable("Y"),
            Field("Data"): Selected(Variable("M"), "F"),
        },
        ID("X"),
        subpath=True,
    ) == Add(
        IfExpr(
            [
                (
                    Or(
                        Selected(Variable("X"), "Has_Data"),
                        Not(Selected(Variable("X"), "Has_Data")),
                    ),
                    Size(Selected(Variable("M"), "F")),
                ),
            ],
            Number(0),
        ),
        Number(16),
    )
    assert variable_field_value.size(
        {
            Field("Length"): Variable("Y"),
            Field("Data"): Variable("Z"),
        },
        ID("X"),
        subpath=True,
    ) == Add(
        IfExpr(
            [
                (
                    Or(
                        Selected(Variable("X"), "Has_Data"),
                        Not(Selected(Variable("X"), "Has_Data")),
                    ),
                    Size(Variable("Z")),
                ),
            ],
            Number(0),
        ),
        Number(16),
    )

    optional_overlayed_field = Message(
        ID("Test::Message", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("A", location=Location((2, 2)))), location=Location((1, 1))),
            Link(
                Field(ID("A", location=Location((3, 3)))),
                Field(ID("B", location=Location((3, 3)))),
                condition=Equal(
                    Variable(ID("A", location=Location((4, 4)))),
                    Number(0),
                    location=Location((4, 4)),
                ),
                first=First(Variable(ID("A", location=Location((5, 5))))),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("A", location=Location((6, 6)))),
                Field(ID("B", location=Location((6, 6)))),
                condition=Greater(
                    Variable("A", location=Location((7, 7))),
                    Number(0),
                    location=Location((7, 7)),
                ),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("B", location=Location((8, 8)))),
                FINAL,
                location=Location((8, 8)),
            ),
        ],
        {
            Field("A"): models.tlv_length(),
            Field("B"): models.tlv_length(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert optional_overlayed_field.size(
        {
            Field("A"): Number(0),
            Field("B"): Number(0),
        },
        subpath=True,
    ) == Number(16)
    assert optional_overlayed_field.size(
        {
            Field("A"): Number(1),
            Field("B"): Number(2),
        },
        subpath=True,
    ) == Number(32)
    assert optional_overlayed_field.size(
        {
            Field("A"): Number(1),
        },
        subpath=True,
    ) == Number(16)
    assert optional_overlayed_field.size({Field("B"): Number(1)}, subpath=True) == IfExpr(
        [
            (
                And(Greater(Variable("A"), Number(0)), NotEqual(Variable("A"), Number(0))),
                Number(16),
            ),
        ],
        Number(0),
    )

    path_dependent_fields = Message(
        ID("Test::Message", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("A", location=Location((2, 2))))),
            Link(
                Field(ID("A", location=Location((3, 3)))),
                Field(ID("B", location=Location((3, 3)))),
                condition=Equal(
                    Variable(ID("A", location=Location((4, 4)))),
                    Number(0),
                    location=Location((4, 4)),
                ),
            ),
            Link(
                Field(ID("A", location=Location((5, 5)))),
                Field(ID("C", location=Location((5, 5)))),
                condition=Greater(
                    Variable(ID("A", location=Location((6, 6)))),
                    Number(0),
                    location=Location((6, 6)),
                ),
            ),
            Link(
                Field(ID("B", location=Location((7, 7)))),
                FINAL,
            ),
            Link(
                Field(ID("C", location=Location((8, 8)))),
                FINAL,
            ),
        ],
        {
            Field("A"): models.tlv_length(),
            Field("B"): models.tlv_length(),
            Field("C"): models.tlv_length(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert path_dependent_fields.size(
        {Field("A"): Variable("X"), Field("B"): Number(0)},
        subpath=True,
    ) == Number(32)
    assert path_dependent_fields.size(
        {Field("A"): Variable("X"), Field("C"): Variable("Y")},
        subpath=True,
    ) == Number(32)
    assert path_dependent_fields.size(
        {Field("A"): Variable("X")},
        subpath=True,
    ) == Number(16)
    assert path_dependent_fields.size(
        {Field("B"): Variable("X")},
        subpath=True,
    ) == Number(16)


def test_size_subpath_error() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:1: error: unable to calculate size of invalid subpath "Tag -> Value"'
            r' of message "TLV::Message"'
            r"$"
        ),
    ):
        models.tlv_message().size(
            {
                Field("Tag"): Variable("T"),
                Field("Value"): Variable("V"),
            },
            subpath=True,
        )


def test_max_size() -> None:
    assert models.null_message().max_size() == Number(0)
    assert models.fixed_size_message().max_size() == Number(8 + 3 * 64)
    assert models.tlv_message().max_size() == Number(8 + 16 + (2**16 - 1) * 8)
    assert models.sequence_message().max_size() == Number(8 + (2**8 - 1) * 8 + 2 * 16)


def test_max_size_error() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r"^<stdin>:1:1: error: unable to calculate maximum size of message with implicit "
        "size$",
    ):
        models.ethernet_frame().max_size()


def test_max_field_sizes() -> None:
    assert models.null_message().max_field_sizes() == {}
    assert models.fixed_size_message().max_field_sizes() == {
        Field("Message_Type"): Number(8),
        Field("Data"): Number(64),
        Field("Values"): Number(64),
        Field("Options"): Number(64),
    }
    assert models.tlv_message().max_field_sizes() == {
        Field("Tag"): Number(8),
        Field("Length"): Number(16),
        Field("Value"): Number((2**16 - 1) * 8),
    }


def test_max_field_sizes_error() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^<stdin>:1:1: error: unable to calculate maximum field sizes of message with "
            r"implicit size$"
        ),
    ):
        models.ethernet_frame().max_field_sizes()


def test_derived_message_incorrect_base_name() -> None:
    with pytest.raises(
        RecordFluxError,
        match='^<stdin>:40:8: error: invalid format for identifier "A::B::C"$',
    ):
        DerivedMessage(
            ID("P::M", Location((1, 1))),
            Message(ID("A::B::C", location=Location((40, 8))), [], {}),
        )


def test_prefixed_message() -> None:
    assert_equal(
        Message(
            ID("P::M", location=Location((1, 1))),
            [
                Link(
                    INITIAL,
                    Field(ID("F1", location=Location((2, 2)))),
                    location=Location((1, 1)),
                ),
                Link(
                    Field(ID("F1", location=Location((3, 3)))),
                    Field(ID("F2", location=Location((3, 3)))),
                    LessEqual(
                        Variable(ID("F1", location=Location((4, 4)))),
                        Number(100),
                        location=Location((4, 4)),
                    ),
                    first=First(ID("F1", location=Location((5, 5)))),
                    location=Location((3, 3)),
                ),
                Link(
                    Field(ID("F1", location=Location((6, 6)))),
                    Field(ID("F3", location=Location((6, 6)))),
                    GreaterEqual(
                        Variable(ID("F1", location=Location((7, 7)))),
                        Number(200),
                        location=Location((7, 7)),
                    ),
                    first=First(ID("F1", location=Location((8, 8)))),
                    location=Location((6, 6)),
                ),
                Link(
                    Field(ID("F2", location=Location((9, 9)))),
                    FINAL,
                    Equal(
                        Variable(ID("F2", location=Location((9, 9)))),
                        Literal(ID("P::One", location=Location((9, 9)))),
                        location=Location((9, 9)),
                    ),
                    location=Location((9, 9)),
                ),
                Link(
                    Field(ID("F3", location=Location((10, 10)))),
                    Field(ID("F4", location=Location((10, 10)))),
                    size=Mul(
                        Variable(ID("F3", location=Location((11, 11)))),
                        Number(8),
                        location=Location((11, 11)),
                    ),
                    location=Location((10, 10)),
                ),
                Link(
                    Field(ID("F4", location=Location((12, 12)))),
                    FINAL,
                    location=Location((12, 12)),
                ),
            ],
            {
                Field(ID("F1", location=Location((1, 1)))): models.integer(),
                Field(ID("F2", location=Location((2, 2)))): models.enumeration(),
                Field(ID("F3", location=Location((3, 3)))): models.integer(),
                Field(ID("F4", location=Location((4, 4)))): OPAQUE,
            },
            location=Location((1, 1), end=(1, 2)),
        ).prefixed("X_"),
        Message(
            ID("P::M", location=Location((1, 1))),
            [
                Link(
                    INITIAL,
                    Field(ID("X_F1", location=Location((2, 2)))),
                    location=Location((2, 2)),
                ),
                Link(
                    Field(ID("X_F1", location=Location((3, 3)))),
                    Field(ID("X_F2", location=Location((3, 3)))),
                    LessEqual(
                        Variable(ID("X_F1", location=Location((4, 4)))),
                        Number(100),
                        location=Location((4, 4)),
                    ),
                    first=First(ID("X_F1", location=Location((5, 5)))),
                    location=Location((3, 3)),
                ),
                Link(
                    Field(ID("X_F1", location=Location((6, 6)))),
                    Field(ID("X_F3", location=Location((6, 6)))),
                    GreaterEqual(
                        Variable(ID("X_F1", location=Location((7, 7)))),
                        Number(200),
                        location=Location((7, 7)),
                    ),
                    first=First(ID("X_F1", location=Location((8, 8)))),
                    location=Location((6, 6)),
                ),
                Link(
                    Field(ID("X_F2", location=Location((9, 9)))),
                    FINAL,
                    Equal(
                        Variable(ID("X_F2", location=Location((10, 10)))),
                        Literal(ID("P::One", location=Location((10, 10)))),
                        location=Location((10, 10)),
                    ),
                    location=Location((9, 9)),
                ),
                Link(
                    Field(ID("X_F3", location=Location((11, 11)))),
                    Field(ID("X_F4", location=Location((11, 11)))),
                    size=Mul(
                        Variable(ID("X_F3", location=Location((12, 12)))),
                        Number(8),
                        location=Location((12, 12)),
                    ),
                    location=Location((11, 11)),
                ),
                Link(
                    Field(ID("X_F4", location=Location((13, 13)))),
                    FINAL,
                    location=Location((13, 13)),
                ),
            ],
            {
                Field(ID("X_F1", location=Location((1, 1)))): models.integer(),
                Field(ID("X_F2", location=Location((2, 2)))): models.enumeration(),
                Field(ID("X_F3", location=Location((3, 3)))): models.integer(),
                Field(ID("X_F4", location=Location((4, 4)))): OPAQUE,
            },
            location=Location((1, 1), end=(1, 2)),
        ),
    )


def test_merge_message_simple() -> None:
    assert deepcopy(M_SMPL_REF).merged(
        [models.integer(), models.enumeration(), msg_no_ref()],
    ) == UncheckedMessage(
        ID("P::Smpl_Ref", location=Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("NR_F1", location=Location((2, 2)))),
                size=Number(16, location=Location((1, 1))),
                location=Location((1, 1)),
            ),
            Link(
                Field(ID("NR_F1", location=Location((3, 3)))),
                Field(ID("NR_F2", location=Location((3, 3)))),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("NR_F2", location=Location((4, 4)))),
                Field(ID("NR_F3", location=Location((4, 4)))),
                LessEqual(
                    Variable(ID("NR_F2", location=Location((5, 5)))),
                    Number(100),
                    location=Location((5, 5)),
                ),
                first=First(ID("NR_F2", location=Location((6, 6)))),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("NR_F2", location=Location((7, 7)))),
                Field(ID("NR_F4", location=Location((7, 7)))),
                GreaterEqual(
                    Variable(ID("NR_F2", location=Location((8, 8)))),
                    Number(200),
                    location=Location((8, 8)),
                ),
                first=First(ID("NR_F2", location=Location((9, 9)))),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("NR_F3", location=Location((10, 10)))),
                FINAL,
                Equal(
                    Variable(ID("NR_F3", location=Location((10, 10)))),
                    Variable(ID("P::One", location=Location((10, 10)))),
                    location=Location((10, 10)),
                ),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("NR_F4", location=Location((11, 11)))),
                FINAL,
                location=Location((11, 11)),
            ),
        ],
        [],
        [
            (Field(ID("NR_F1", location=Location((1, 1)))), OPAQUE.identifier, []),
            (Field(ID("NR_F2", location=Location((2, 2)))), models.integer().identifier, []),
            (Field(ID("NR_F3", location=Location((3, 3)))), models.enumeration().identifier, []),
            (Field(ID("NR_F4", location=Location((4, 4)))), models.integer().identifier, []),
        ],
        checksums={},
        byte_order={
            Field(ID("NR_F1", location=Location((1, 1)))): ByteOrder.HIGH_ORDER_FIRST,
            Field(ID("NR_F2", location=Location((2, 2)))): ByteOrder.HIGH_ORDER_FIRST,
            Field(ID("NR_F3", location=Location((3, 3)))): ByteOrder.HIGH_ORDER_FIRST,
            Field(ID("NR_F4", location=Location((4, 4)))): ByteOrder.HIGH_ORDER_FIRST,
        },
        location=Location((1, 1), end=(1, 2)),
    )


def test_merge_message_complex() -> None:
    msg_cmplx_ref = UncheckedMessage(
        ID("P::Cmplx_Ref"),
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), LessEqual(Variable("F1"), Number(100))),
            Link(Field("F1"), Field("F3"), GreaterEqual(Variable("F1"), Number(200))),
            Link(Field("F2"), Field("NR"), LessEqual(Variable("F1"), Number(10))),
            Link(Field("F3"), Field("NR"), GreaterEqual(Variable("F1"), Number(220))),
            Link(Field("NR"), Field("F5"), LessEqual(Variable("F1"), Number(100))),
            Link(Field("NR"), Field("F6"), GreaterEqual(Variable("F1"), Number(200))),
            Link(Field("F5"), FINAL),
            Link(Field("F6"), FINAL),
        ],
        [],
        [
            (Field("F1"), models.integer().identifier, []),
            (Field("F2"), models.integer().identifier, []),
            (Field("F3"), models.integer().identifier, []),
            (Field("NR"), M_NO_REF.identifier, []),
            (Field("F5"), models.integer().identifier, []),
            (Field("F6"), models.integer().identifier, []),
        ],
    )
    assert_equal(
        deepcopy(msg_cmplx_ref).merged([models.integer(), msg_no_ref()]),
        UncheckedMessage(
            ID("P::Cmplx_Ref"),
            [
                Link(Field("F1"), Field("F2"), LessEqual(Variable("F1"), Number(100))),
                Link(Field("F1"), Field("F3"), GreaterEqual(Variable("F1"), Number(200))),
                Link(
                    Field("F2"),
                    Field("NR_F1"),
                    LessEqual(Variable("F1"), Number(10)),
                    size=Number(16),
                ),
                Link(
                    Field("F3"),
                    Field("NR_F1"),
                    GreaterEqual(Variable("F1"), Number(220)),
                    size=Number(16),
                ),
                Link(Field("F5"), FINAL),
                Link(Field("F6"), FINAL),
                Link(INITIAL, Field("F1")),
                Link(Field("NR_F1"), Field("NR_F2")),
                Link(
                    Field("NR_F2"),
                    Field("NR_F3"),
                    LessEqual(Variable("NR_F2"), Number(100)),
                    first=First("NR_F2"),
                ),
                Link(
                    Field("NR_F2"),
                    Field("NR_F4"),
                    GreaterEqual(Variable("NR_F2"), Number(200)),
                    first=First("NR_F2"),
                ),
                Link(
                    Field("NR_F3"),
                    Field("F5"),
                    And(
                        LessEqual(Variable("F1"), Number(100)),
                        Equal(Variable("NR_F3"), Variable("P::One")),
                    ),
                ),
                Link(
                    Field("NR_F3"),
                    Field("F6"),
                    And(
                        GreaterEqual(Variable("F1"), Number(200)),
                        Equal(Variable("NR_F3"), Variable("P::One")),
                    ),
                ),
                Link(Field("NR_F4"), Field("F5"), LessEqual(Variable("F1"), Number(100))),
                Link(Field("NR_F4"), Field("F6"), GreaterEqual(Variable("F1"), Number(200))),
            ],
            [],
            [
                (Field("F1"), models.integer().identifier, []),
                (Field("F2"), models.integer().identifier, []),
                (Field("F3"), models.integer().identifier, []),
                (Field("F5"), models.integer().identifier, []),
                (Field("F6"), models.integer().identifier, []),
                (Field("NR_F1"), OPAQUE.identifier, []),
                (Field("NR_F2"), models.integer().identifier, []),
                (Field("NR_F3"), models.enumeration().identifier, []),
                (Field("NR_F4"), models.integer().identifier, []),
            ],
            checksums={},
            byte_order={
                Field("F1"): ByteOrder.HIGH_ORDER_FIRST,
                Field("F2"): ByteOrder.HIGH_ORDER_FIRST,
                Field("F3"): ByteOrder.HIGH_ORDER_FIRST,
                Field("F5"): ByteOrder.HIGH_ORDER_FIRST,
                Field("F6"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F1"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F2"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F3"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F4"): ByteOrder.HIGH_ORDER_FIRST,
            },
        ),
    )


def test_merge_message_recursive() -> None:
    assert_equal(
        deepcopy(M_DBL_REF).merged(
            [OPAQUE, models.integer(), models.enumeration(), msg_no_ref(), msg_smpl_ref()],
        ),
        UncheckedMessage(
            ID("P::Dbl_Ref"),
            [
                Link(INITIAL, Field("SR_NR_F1"), size=Number(16)),
                Link(Field("NR_F1"), Field("NR_F2")),
                Link(
                    Field("NR_F2"),
                    Field("NR_F3"),
                    LessEqual(Variable("NR_F2"), Number(100)),
                    first=First("NR_F2"),
                ),
                Link(
                    Field("NR_F2"),
                    Field("NR_F4"),
                    GreaterEqual(Variable("NR_F2"), Number(200)),
                    first=First("NR_F2"),
                ),
                Link(Field("NR_F3"), FINAL, Equal(Variable("NR_F3"), Variable("P::One"))),
                Link(Field("NR_F4"), FINAL),
                Link(Field("SR_NR_F1"), Field("SR_NR_F2")),
                Link(
                    Field("SR_NR_F2"),
                    Field("SR_NR_F3"),
                    LessEqual(Variable("SR_NR_F2"), Number(100)),
                    first=First("SR_NR_F2"),
                ),
                Link(
                    Field("SR_NR_F2"),
                    Field("SR_NR_F4"),
                    GreaterEqual(Variable("SR_NR_F2"), Number(200)),
                    first=First("SR_NR_F2"),
                ),
                Link(
                    Field("SR_NR_F3"),
                    Field("NR_F1"),
                    Equal(Variable("SR_NR_F3"), Variable("P::One")),
                    size=Number(16),
                ),
                Link(Field("SR_NR_F4"), Field("NR_F1"), size=Number(16)),
            ],
            [],
            [
                (Field("SR_NR_F1"), OPAQUE.identifier, []),
                (Field("SR_NR_F2"), models.integer().identifier, []),
                (Field("SR_NR_F3"), models.enumeration().identifier, []),
                (Field("SR_NR_F4"), models.integer().identifier, []),
                (Field("NR_F1"), OPAQUE.identifier, []),
                (Field("NR_F2"), models.integer().identifier, []),
                (Field("NR_F3"), models.enumeration().identifier, []),
                (Field("NR_F4"), models.integer().identifier, []),
            ],
            checksums={},
            byte_order={
                Field("SR_NR_F1"): ByteOrder.HIGH_ORDER_FIRST,
                Field("SR_NR_F2"): ByteOrder.HIGH_ORDER_FIRST,
                Field("SR_NR_F3"): ByteOrder.HIGH_ORDER_FIRST,
                Field("SR_NR_F4"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F1"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F2"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F3"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F4"): ByteOrder.HIGH_ORDER_FIRST,
            },
            location=Location((1, 1), end=(1, 2)),
        ),
    )


def test_merge_message_simple_derived() -> None:
    assert_equal(
        deepcopy(M_SMPL_REF_DERI).checked([models.integer(), models.enumeration(), msg_smpl_ref()]),
        DerivedMessage(
            ID("P::Smpl_Ref_Deri", Location((1, 1))),
            Message(
                ID("P::Smpl_Ref", Location((1, 1))),
                [
                    Link(
                        INITIAL,
                        Field(ID("NR_F1", location=Location((2, 2)))),
                        size=Number(16, location=Location((1, 1))),
                        location=Location((1, 1)),
                    ),
                    Link(
                        Field(ID("NR_F3", location=Location((3, 3)))),
                        FINAL,
                        Equal(
                            Variable(ID("NR_F3", location=Location((4, 4)))),
                            Variable(ID("P::One", location=Location((4, 4)))),
                            location=Location((4, 4)),
                        ),
                        location=Location((4, 4)),
                    ),
                    Link(
                        Field(ID("NR_F4", location=Location((5, 5)))),
                        FINAL,
                        location=Location((5, 5)),
                    ),
                    Link(
                        Field(ID("NR_F1", location=Location((6, 6)))),
                        Field(ID("NR_F2", location=Location((6, 6)))),
                        location=Location((6, 6)),
                    ),
                    Link(
                        Field(ID("NR_F2", location=Location((7, 7)))),
                        Field(ID("NR_F3", location=Location((7, 7)))),
                        LessEqual(
                            Variable(ID("NR_F2", location=Location((8, 8)))),
                            Number(100),
                            location=Location((8, 8)),
                        ),
                        first=First(ID("NR_F2", location=Location((9, 9)))),
                        location=Location((7, 7)),
                    ),
                    Link(
                        Field(ID("NR_F2", location=Location((10, 10)))),
                        Field(ID("NR_F4", location=Location((10, 10)))),
                        GreaterEqual(
                            Variable("NR_F2", location=Location((11, 11))),
                            Number(200),
                            location=Location((11, 11)),
                        ),
                        first=First(ID("NR_F2", location=Location((12, 12)))),
                        location=Location((10, 10)),
                    ),
                ],
                {
                    Field(ID("NR_F1", location=Location((1, 1)))): OPAQUE,
                    Field(ID("NR_F2", location=Location((2, 2)))): models.integer(),
                    Field(ID("NR_F3", location=Location((3, 3)))): models.enumeration(),
                    Field(ID("NR_F4", location=Location((4, 4)))): models.integer(),
                },
                location=Location((1, 1), end=(1, 2)),
            ),
        ),
    )


def test_merge_message_checksums() -> None:
    inner_msg = Message(
        ID("P::Merge_Test_Byte_Order", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("F1", location=Location((2, 2))))),
            Link(
                Field(ID("F1", location=Location((3, 3)))),
                Field(ID("F2", location=Location((3, 3)))),
            ),
            Link(
                Field(ID("F2", location=Location((4, 4)))),
                FINAL,
                condition=ValidChecksum(ID("F2", location=Location((4, 4)))),
            ),
        ],
        {
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
        },
        checksums={ID("F2"): [Variable("F1")]},
        location=Location((1, 1), end=(1, 2)),
    )
    outer_msg = UncheckedMessage(
        ID("P::Outer_Msg", Location((1, 1))),
        [
            Link(INITIAL, Field("NR")),
            Link(Field("NR"), Field("C")),
            Link(Field("C"), FINAL, condition=ValidChecksum("C")),
        ],
        [],
        [
            (Field("NR"), inner_msg.identifier, []),
            (Field("C"), models.integer().identifier, []),
        ],
        checksums={ID("C"): [Variable("NR_F1"), Variable("NR_F2")]},
        location=Location((1, 1), end=(1, 2)),
    )
    assert_equal(
        outer_msg.merged([OPAQUE, models.integer(), inner_msg]),
        UncheckedMessage(
            ID("P::Outer_Msg", Location((1, 1))),
            [
                Link(Field("C"), FINAL, condition=ValidChecksum("C")),
                Link(INITIAL, Field("NR_F1")),
                Link(Field("NR_F1"), Field("NR_F2")),
                Link(Field("NR_F2"), Field("C"), condition=ValidChecksum("NR_F2")),
            ],
            [],
            [
                (Field("C"), models.integer().identifier, []),
                (Field("NR_F1"), models.integer().identifier, []),
                (Field("NR_F2"), models.integer().identifier, []),
            ],
            checksums={
                ID("NR_F2"): [Variable("NR_F1")],
                ID("C"): [Variable("NR_F1"), Variable("NR_F2")],
            },
            byte_order={
                Field("C"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F1"): ByteOrder.HIGH_ORDER_FIRST,
                Field("NR_F2"): ByteOrder.HIGH_ORDER_FIRST,
            },
            location=Location((1, 1), end=(1, 2)),
        ),
    )


def test_merge_message_byte_order() -> None:
    inner_msg = Message(
        ID("P::Merge_Test_Byte_Order", Location((1, 1))),
        [
            Link(INITIAL, Field("F1"), location=Location((1, 1))),
            Link(Field("F1"), Field("F2"), location=Location((2, 2))),
            Link(Field("F2"), FINAL, location=Location((3, 3))),
        ],
        {
            Field(ID("F1", location=Location((1, 1)))): models.integer(),
            Field(ID("F2", location=Location((2, 2)))): models.enumeration(),
        },
        byte_order=ByteOrder.LOW_ORDER_FIRST,
        location=Location((1, 1), end=(1, 2)),
    )
    outer_msg = UncheckedMessage(
        ID("P::Outer_Msg", Location((1, 1))),
        [
            Link(INITIAL, Field("NR")),
            Link(Field("NR"), FINAL),
        ],
        [],
        [
            (Field("NR"), inner_msg.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )
    assert_equal(
        outer_msg.merged([models.integer(), models.enumeration(), inner_msg]),
        UncheckedMessage(
            ID("P::Outer_Msg", Location((1, 1))),
            [
                Link(INITIAL, Field("NR_F1")),
                Link(Field("NR_F1"), Field("NR_F2")),
                Link(Field("NR_F2"), FINAL),
            ],
            [],
            [
                (Field("NR_F1"), models.integer().identifier, []),
                (Field("NR_F2"), models.enumeration().identifier, []),
            ],
            checksums={},
            byte_order={
                Field("NR_F1"): ByteOrder.LOW_ORDER_FIRST,
                Field("NR_F2"): ByteOrder.LOW_ORDER_FIRST,
            },
            location=Location((1, 1), end=(1, 2)),
        ),
    )


def test_merge_message_constrained() -> None:
    m1 = Message(
        ID("P::M1", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("F1", location=Location((2, 2))))),
            Link(
                Field(ID("F1", location=Location((2, 2)))),
                Field(ID("F3", location=Location((2, 2)))),
                GreaterEqual(
                    Variable(ID("F1", location=Location((3, 3)))),
                    Number(100),
                    location=Location((3, 3)),
                ),
            ),
            Link(
                Field(ID("F1", location=Location((4, 4)))),
                Field(ID("F2", location=Location((4, 4)))),
                Less(
                    Variable(ID("F1", location=Location((4, 4)))),
                    Number(100),
                    location=Location((4, 4)),
                ),
            ),
            Link(
                Field(ID("F2", location=Location((5, 5)))),
                FINAL,
                Greater(
                    Variable(ID("F1", location=Location((5, 5)))),
                    Number(1),
                    location=Location((5, 5)),
                ),
            ),
            Link(Field(ID("F3", location=Location((6, 6)))), FINAL),
        ],
        {
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
            Field("F3"): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    m2 = UncheckedMessage(
        ID("P::M2", Location((1, 1))),
        [
            Link(INITIAL, Field("F4")),
            Link(
                Field("F4"),
                FINAL,
                Equal(Variable("F4_F1"), Number(40), location=Location((4, 4))),
            ),
        ],
        [],
        [
            (Field("F4"), m1.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )
    expected = UncheckedMessage(
        ID("P::M2", Location((1, 1))),
        [
            Link(Field("F4_F1"), Field("F4_F2"), Less(Variable("F4_F1"), Number(100))),
            Link(
                Field("F4_F2"),
                FINAL,
                And(
                    Equal(Variable("F4_F1"), Number(40), location=Location((4, 4))),
                    Greater(Variable("F4_F1"), Number(1), location=Location((4, 4))),
                ),
            ),
            Link(
                INITIAL,
                Field("F4_F1"),
            ),
        ],
        [],
        [
            (Field("F4_F1"), models.integer().identifier, []),
            (Field("F4_F2"), models.integer().identifier, []),
        ],
        checksums={},
        byte_order={
            Field("F4_F1"): ByteOrder.HIGH_ORDER_FIRST,
            Field("F4_F2"): ByteOrder.HIGH_ORDER_FIRST,
        },
        location=Location((1, 1), end=(1, 2)),
    )
    merged = m2.merged([m1, models.integer()])

    assert merged == expected


def test_merge_message_constrained_empty() -> None:
    m1 = Message(
        ID("P::M1", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("F1", location=Location((2, 2)))), location=Location((2, 3))),
            Link(
                Field(ID("F1", location=Location((3, 3)))),
                Field(ID("F2", location=Location((3, 3)))),
                Equal(
                    Variable(ID("F1", location=Location((4, 4)))),
                    Number(2),
                    location=Location((5, 5)),
                ),
            ),
            Link(
                Field(ID("F1", location=Location((6, 6)))),
                FINAL,
                Equal(
                    Variable(ID("F1", location=Location((7, 7)))),
                    Number(1),
                    location=Location((7, 7)),
                ),
            ),
            Link(
                Field(ID("F2", location=Location((8, 7)))),
                FINAL,
                Equal(Variable("F2"), Number(2), location=Location((8, 8))),
                location=Location((8, 9)),
            ),
        ],
        {
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    m2 = UncheckedMessage(
        ID("P::M2", Location((1, 1))),
        [
            Link(INITIAL, Field("F3")),
            Link(
                Field("F3"),
                FINAL,
                And(
                    Equal(Variable("F3_F1"), Number(2), location=Location((3, 3))),
                    Equal(Variable("F3_F2"), Number(1), location=Location((3, 3))),
                    location=Location((3, 3)),
                ),
            ),
        ],
        [],
        [
            (Field("F3"), m1.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )
    with pytest.raises(
        RecordFluxError,
        match=r'^error: empty message type when merging field "F3"$',
    ):
        m2.merged([models.integer(), m1])


def test_merge_message_error_name_conflict() -> None:
    m2_f2 = Field(ID("F2", Location((10, 5))))

    m2 = Message(
        ID("P::M2", Location((1, 1))),
        [Link(INITIAL, m2_f2), Link(m2_f2, FINAL)],
        {
            m2_f2: models.integer(),
        },
        location=Location((15, 3), end=(15, 4)),
    )

    m1_f1 = Field(ID("F1", Location((20, 8))))
    m1_f1_f2 = Field(ID("F1_F2", Location((30, 5))))

    m1 = UncheckedMessage(
        ID("P::M1", Location((1, 1))),
        [Link(INITIAL, m1_f1), Link(m1_f1, m1_f1_f2), Link(m1_f1_f2, FINAL)],
        [],
        [
            (m1_f1, m2.identifier, []),
            (m1_f1_f2, models.integer().identifier, []),
        ],
        location=Location((2, 9)),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:30:5: error: name conflict for "F1_F2"\n'
            r'<stdin>:15:3: note: when merging message "P::M2"\n'
            r'<stdin>:20:8: note: into field "F1"$'
        ),
    ):
        m1.merged([models.integer(), m2])


def test_merge_message_parameterized() -> None:
    msg_param_no_ref = Message(
        ID("P::Param_No_Ref", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("F1", location=Location((2, 2))))),
            Link(
                Field(ID("F1", location=Location((3, 3)))),
                Field(ID("F2", location=Location((3, 3)))),
                condition=Equal(
                    Variable(ID("P1", location=Location((4, 4)))),
                    Number(1),
                    location=Location((4, 4)),
                ),
            ),
            Link(
                Field(ID("F1", location=Location((5, 5)))),
                FINAL,
                condition=Equal(
                    Variable(ID("P1", location=Location((6, 6)))),
                    Number(2),
                    location=Location((6, 6)),
                ),
            ),
            Link(Field(ID("F2", location=Location((7, 7)))), FINAL),
        ],
        {
            Field("P1"): models.integer(),
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    msg_param_param_ref = UncheckedMessage(
        ID("P::Param_Param_Ref", Location((1, 1))),
        [
            Link(INITIAL, Field("PNR")),
            Link(Field("PNR"), FINAL),
        ],
        [],
        [
            (Field("P2"), models.integer().identifier, []),
            (Field("PNR"), msg_param_no_ref.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )

    assert_equal(
        msg_param_param_ref.merged(
            [models.integer(), msg_param_no_ref],
            {ID("P::Param_No_Ref"): {ID("P1"): Variable("P2")}},
        ),
        UncheckedMessage(
            ID("P::Param_Param_Ref"),
            [
                Link(INITIAL, Field("PNR_F1")),
                Link(Field("PNR_F1"), FINAL, condition=Equal(Variable("P2"), Number(2))),
                Link(Field("PNR_F1"), Field("PNR_F2"), condition=Equal(Variable("P2"), Number(1))),
                Link(Field("PNR_F2"), FINAL),
            ],
            [],
            [
                (Field("P2"), models.integer().identifier, []),
                (Field("PNR_F1"), models.integer().identifier, []),
                (Field("PNR_F2"), models.integer().identifier, []),
            ],
            checksums={},
            byte_order={
                Field("P2"): ByteOrder.HIGH_ORDER_FIRST,
                Field("PNR_F1"): ByteOrder.HIGH_ORDER_FIRST,
                Field("PNR_F2"): ByteOrder.HIGH_ORDER_FIRST,
            },
            location=Location((1, 1), end=(1, 2)),
        ),
    )


def test_merge_message_with_message_attributes() -> None:
    inner = Message(
        ID("P::I", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("I1", location=Location((2, 2)))), location=Location((2, 2))),
            Link(
                Field(ID("I1", location=Location((3, 3)))),
                Field(ID("I2", location=Location((3, 3)))),
                condition=Less(
                    Variable(ID("I1", location=Location((4, 4)))),
                    Number(128),
                    location=Location((4, 4)),
                ),
                size=Sub(
                    Last(ID("Message", location=Location((5, 10)))),
                    Last(ID("I1", location=Location((5, 10)))),
                    location=Location((5, 10)),
                ),
                first=First(ID("I1", location=Location((6, 14)))),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("I1", location=Location((7, 7)))),
                Field(ID("I2", location=Location((7, 7)))),
                condition=GreaterEqual(
                    Variable(ID("I1", location=Location((8, 8)))),
                    Number(128),
                    location=Location((8, 8)),
                ),
                size=Sub(
                    Mul(
                        Variable(ID("I1", location=Location((9, 9)))),
                        Number(8),
                        location=Location((9, 9)),
                    ),
                    Add(
                        Sub(
                            Last(ID("I1", location=Location((10, 10)))),
                            First(ID("Message", location=Location((10, 10)))),
                            location=Location((10, 10)),
                        ),
                        Number(1),
                        location=Location((10, 10)),
                    ),
                    location=Location((6, 6)),
                ),
                location=Location((7, 7)),
            ),
            Link(Field(ID("I2", location=Location((11, 11)))), FINAL),
        ],
        {
            Field(ID("I1", location=Location((1, 1)))): models.integer(),
            Field(ID("I2", location=Location((2, 2)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    valid_outer = UncheckedMessage(
        ID("P::O", Location((1, 1))),
        [
            Link(INITIAL, Field("O1"), location=Location((1, 1))),
            Link(Field("O1"), Field("O2"), location=Location((2, 2))),
            Link(Field("O2"), FINAL, location=Location((3, 3))),
        ],
        [],
        [
            (Field(ID("O1", location=Location((1, 1)))), models.integer().identifier, []),
            (Field(ID("O2", location=Location((2, 2)))), inner.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    ).checked([OPAQUE, models.integer(), inner])

    assert_equal(
        valid_outer,
        Message(
            ID("P::O", Location((1, 1))),
            [
                Link(
                    INITIAL,
                    Field(ID("O1", location=Location((2, 2)))),
                    location=Location((2, 2)),
                ),
                Link(
                    Field(ID("O1", location=Location((3, 3)))),
                    Field(ID("O2_I1", location=Location((3, 3)))),
                    location=Location((3, 3)),
                ),
                Link(
                    Field(ID("O2_I1", location=Location((4, 4)))),
                    Field(ID("O2_I2", location=Location((4, 4)))),
                    condition=Less(
                        Variable(ID("O2_I1", location=Location((5, 5)))),
                        Number(128),
                        location=Location((5, 5)),
                    ),
                    size=Add(
                        Last(ID("Message", location=Location((6, 6)))),
                        -Last(ID("O2_I1", location=Location((6, 6)))),
                        location=Location((6, 6)),
                    ),
                    first=First(ID("O2_I1", location=Location((7, 7)))),
                    location=Location((4, 4)),
                ),
                Link(
                    Field(ID("O2_I1", location=Location((8, 8)))),
                    Field(ID("O2_I2", location=Location((8, 8)))),
                    condition=GreaterEqual(
                        Variable(ID("O2_I1", location=Location((9, 9)))),
                        Number(128),
                        location=Location((9, 9)),
                    ),
                    size=Add(
                        Mul(
                            Variable(ID("O2_I1", location=Location((10, 10)))),
                            Number(8, location=Location((10, 10))),
                            location=Location((10, 10)),
                        ),
                        Add(
                            -Last(ID("O2_I1", location=Location((11, 11)))),
                            First(ID("O2_I1", location=Location((11, 11)))),
                            -Number(1),
                            location=Location((11, 11)),
                        ),
                        location=Location((11, 11)),
                    ),
                    location=Location((4, 4)),
                ),
                Link(
                    Field(ID("O2_I2", location=Location((12, 12)))),
                    FINAL,
                    location=Location((5, 5)),
                ),
            ],
            {
                Field(ID("O1", location=Location((1, 1)))): models.integer(),
                Field(ID("O2_I1", location=Location((2, 2)))): models.integer(),
                Field(ID("O2_I2", location=Location((3, 3)))): OPAQUE,
            },
            location=Location((1, 1), end=(1, 2)),
        ),
    )
    o1 = Field(ID("O1", location=Location((2, 10))))
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "<stdin>:2:10: error: messages with implicit size may only be used for"
            " last fields\n"
            '<stdin>:5:10: note: message field with implicit size in "P::I"'
            "$"
        ),
    ):
        valid_outer = UncheckedMessage(
            ID("P::O", Location((1, 1))),
            [
                Link(INITIAL, o1),
                Link(o1, Field("O2")),
                Link(Field("O2"), FINAL),
            ],
            [],
            [
                (o1, inner.identifier, []),
                (Field("O2"), models.integer().identifier, []),
            ],
            location=Location((1, 1), end=(1, 2)),
        ).checked([models.integer(), inner])


def test_merge_message_with_message_size_attribute() -> None:
    inner = Message(
        ID("P::I", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("I", location=Location((2, 2)))),
                size=Size(ID("Message", location=Location((2, 2)))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("I", location=Location((3, 3)))),
                FINAL,
                condition=Equal(
                    Size(ID("Message", location=Location((4, 4)))),
                    Number(128),
                    location=Location((4, 4)),
                ),
                location=Location((3, 3)),
            ),
        ],
        {
            Field(ID("I", location=Location((1, 1)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    outer = UncheckedMessage(
        ID("P::O", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("O1", location=Location((2, 2)))), location=Location((1, 1))),
            Link(
                Field(ID("O1", location=Location((3, 3)))),
                Field(ID("O2", location=Location((3, 3)))),
                condition=Less(
                    Variable(ID("O1", location=Location((4, 4)))),
                    Number(100),
                    location=Location((4, 4)),
                ),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("O1", location=Location((5, 5)))),
                Field(ID("O3", location=Location((5, 5)))),
                condition=GreaterEqual(
                    Variable(ID("O1", location=Location((6, 6)))),
                    Number(100),
                    location=Location((6, 6)),
                ),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("O2", location=Location((7, 7)))),
                Field(ID("A", location=Location((7, 7)))),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("O3", location=Location((8, 8)))),
                Field(ID("B", location=Location((8, 8)))),
                location=Location((8, 8)),
            ),
            Link(Field(ID("A", location=Location((9, 9)))), FINAL, location=Location((9, 9))),
            Link(Field(ID("B", location=Location((10, 10)))), FINAL, location=Location((10, 10))),
        ],
        [],
        [
            (Field(ID("O1", location=Location((1, 1)))), models.integer().identifier, []),
            (Field(ID("O2", location=Location((2, 2)))), models.integer().identifier, []),
            (Field(ID("O3", location=Location((3, 3)))), models.integer().identifier, []),
            (Field(ID("A", location=Location((4, 4)))), inner.identifier, []),
            (Field(ID("B", location=Location((5, 5)))), inner.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )

    expected = Message(
        ID("P::O", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("O1", location=Location((2, 2)))), location=Location((1, 1))),
            Link(
                Field(ID("O1", location=Location((3, 3)))),
                Field(ID("O2", location=Location((3, 3)))),
                condition=Less(
                    Variable(ID("O1", location=Location((4, 4)))),
                    Number(100),
                    location=Location((4, 4)),
                ),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("O1", location=Location((5, 5)))),
                Field(ID("O3", location=Location((5, 5)))),
                condition=GreaterEqual(
                    Variable(ID("O1", location=Location((6, 6)))),
                    Number(100),
                    location=Location((6, 6)),
                ),
                location=Location((5, 5)),
            ),
            Link(
                Field(ID("O2", location=Location((7, 7)))),
                Field(ID("A_I", location=Location((7, 7)))),
                size=Sub(
                    Last(ID("Message", location=Location((8, 8)))),
                    Last(ID("O2", location=Location((8, 8)))),
                    location=Location((8, 8)),
                ),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("A_I", location=Location((8, 8)))),
                FINAL,
                condition=Equal(
                    Sub(
                        Last(ID("Message", location=Location((9, 9)))),
                        Last(ID("O2", location=Location((9, 9)))),
                        location=Location((8, 8)),
                    ),
                    Number(128),
                    location=Location((9, 9)),
                ),
                location=Location((8, 8)),
            ),
            Link(
                Field(ID("O3", location=Location((10, 10)))),
                Field(ID("B_I", location=Location((10, 10)))),
                size=Sub(
                    Last(ID("Message", location=Location((11, 11)))),
                    Last(ID("O3", location=Location((11, 11)))),
                    location=Location((11, 11)),
                ),
                location=Location((10, 10)),
            ),
            Link(
                Field(ID("B_I", location=Location((12, 12)))),
                FINAL,
                condition=Equal(
                    Sub(
                        Last(ID("Message", location=Location((13, 13)))),
                        Last(ID("O3", location=Location((13, 13)))),
                    ),
                    Number(128),
                    location=Location((13, 13)),
                ),
                location=Location((12, 12)),
            ),
        ],
        {
            Field(ID("O1", location=Location((1, 1)))): models.integer(),
            Field(ID("O2", location=Location((2, 2)))): models.integer(),
            Field(ID("O3", location=Location((3, 3)))): models.integer(),
            Field(ID("A_I", location=Location((4, 4)))): OPAQUE,
            Field(ID("B_I", location=Location((5, 5)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    assert outer.checked([OPAQUE, models.integer(), inner]) == expected


def test_merge_message_type_message_size_attribute_in_outer_message() -> None:
    inner = Message(
        ID("P::I", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("I", location=Location((2, 2)))),
                size=Number(128, location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("I", location=Location((3, 3)))),
                FINAL,
                location=Location((3, 3)),
            ),
        ],
        {
            Field(ID("I", location=Location((1, 1)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    outer = UncheckedMessage(
        ID("P::O", Location((1, 1))),
        [
            Link(INITIAL, Field("O1"), location=Location((1, 1))),
            Link(
                Field("O1"),
                Field("O2"),
                size=Sub(Last("Message"), Last("O1"), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(Field("O2"), FINAL, location=Location((3, 3))),
        ],
        [],
        [
            (Field(ID("O1", location=Location((1, 1)))), inner.identifier, []),
            (Field(ID("O2", location=Location((2, 2)))), OPAQUE.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )

    expected = Message(
        ID("P::O", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("O1_I", location=Location((2, 2)))),
                size=Number(128, location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("O1_I", location=Location((3, 3)))),
                Field(ID("O2", location=Location((4, 4)))),
                size=Sub(
                    Last(ID("Message", location=Location((5, 5)))),
                    Last(ID("O1_I", location=Location((6, 6)))),
                    location=Location((5, 5)),
                ),
                location=Location((3, 3)),
            ),
            Link(Field(ID("O2", location=Location((7, 7)))), FINAL, location=Location((7, 7))),
        ],
        {
            Field(ID("O1_I", location=Location((1, 1)))): OPAQUE,
            Field(ID("O2", location=Location((2, 2)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    assert outer.checked([OPAQUE, inner]) == expected


def test_merge_message_with_condition_on_message_type_field() -> None:
    inner = Message(
        ID("P::I", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("I", location=Location((2, 2)))),
                size=Number(128, location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("I", location=Location((3, 3)))),
                FINAL,
                location=Location((3, 3)),
            ),
        ],
        {
            Field(ID("I", location=Location((1, 1)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    enumeration = Enumeration(
        "P::Enumeration",
        [("E1", Number(1)), ("E2", Number(2)), ("E3", Number(3))],
        Number(8),
        always_valid=False,
    )
    padding = Integer(ID("P::Padding", location=Location((1, 1))), Number(0), Number(0), Number(7))

    outer = UncheckedMessage(
        ID("P::O", Location((1, 1))),
        [
            Link(INITIAL, Field("Flag"), location=Location((1, 1))),
            Link(Field("Flag"), Field("Padding"), location=Location((2, 2))),
            Link(Field("Padding"), Field("Payload"), location=Location((3, 3))),
            Link(
                Field("Payload"),
                FINAL,
                condition=And(
                    Variable("Flag"),
                    Equal(Variable("Parameter"), Variable("E1")),
                    location=Location((4, 3)),
                ),
                location=Location((4, 4)),
            ),
        ],
        [
            (Field(ID("Parameter", location=Location((1, 1)))), enumeration.identifier, []),
        ],
        [
            (Field(ID("Flag", location=Location((1, 1)))), BOOLEAN.identifier, []),
            (Field(ID("Padding", location=Location((2, 2)))), padding.identifier, []),
            (Field(ID("Payload", location=Location((3, 3)))), inner.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )

    expected = Message(
        ID("P::O", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("Flag", location=Location((2, 2)))), location=Location((2, 2))),
            Link(
                Field(ID("Flag", location=Location((3, 3)))),
                Field(ID("Padding", location=Location((3, 3)))),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("Padding", location=Location((4, 4)))),
                Field(ID("Payload_I", location=Location((4, 4)))),
                size=Number(128, location=Location((4, 4))),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("Payload_I", location=Location((5, 5)))),
                FINAL,
                condition=And(
                    Variable(ID("Flag", location=Location((6, 6)))),
                    Equal(
                        Variable(ID("Parameter", location=Location((6, 6)))),
                        Literal(ID("P::E1", location=Location((6, 6)))),
                    ),
                    location=Location((6, 6)),
                ),
                location=Location((6, 6)),
            ),
        ],
        {
            Field(ID("Parameter", location=Location((1, 1)))): enumeration,
            Field(ID("Flag", location=Location((2, 2)))): BOOLEAN,
            Field(ID("Padding", location=Location((3, 3)))): padding,
            Field(ID("Payload_I", location=Location((4, 4)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    assert outer.checked([BOOLEAN, OPAQUE, enumeration, padding, inner]) == expected


def test_merge_message_with_illegal_condition_on_message_type_field() -> None:
    inner = Message(
        ID("P::I", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("I", location=Location((2, 2))))),
            Link(Field(ID("I", location=Location((3, 3)))), FINAL),
        ],
        {
            Field("I"): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )

    outer = UncheckedMessage(
        ID("P::O", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("O", location=Location((2, 2))))),
            Link(Field("O"), FINAL, condition=Equal(TRUE, Number(1, location=Location((1, 2))))),
        ],
        [],
        [
            (Field("O"), inner.identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            '<stdin>:1:2: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:1:2: error: found type universal integer \(1 .. 1\)"
            "$"
        ),
    ):
        outer.checked([inner])


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_merge_message_with_undeclared_type() -> None:
    with pytest.raises(
        AssertionError,
        match=r'^undeclared types for message "P::Smpl_Ref": P::No_Ref$',
    ):
        deepcopy(M_SMPL_REF).merged([])


def test_paths() -> None:
    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("L", location=Location((2, 2))))),
            Link(
                Field(ID("L", location=Location((3, 3)))),
                Field(ID("O", location=Location((3, 3)))),
                condition=Greater(
                    Variable(ID("L", location=Location((4, 4)))),
                    Number(100),
                    location=Location((4, 4)),
                ),
            ),
            Link(
                Field(ID("L", location=Location((5, 5)))),
                Field(ID("O", location=Location((5, 5)))),
                condition=LessEqual(
                    Variable(ID("L", location=Location((6, 6)))),
                    Number(100),
                    location=Location((6, 6)),
                ),
            ),
            Link(Field(ID("O", location=Location((7, 7)))), FINAL),
        ],
        {Field("L"): models.integer(), Field("O"): models.integer()},
        location=Location((1, 1), end=(1, 2)),
    )
    assert message.paths(Field("O")) == {
        (
            Link(INITIAL, Field(ID("L", location=Location((2, 2))))),
            Link(
                Field("L"),
                Field("O"),
                condition=Greater(
                    Variable(ID("L", location=Location((4, 4)))),
                    Number(100),
                    location=Location((4, 4)),
                ),
            ),
        ),
        (
            Link(INITIAL, Field(ID("L", location=Location((5, 5))))),
            Link(
                Field(ID("L", location=Location((5, 5)))),
                Field(ID("O", location=Location((7, 7)))),
                condition=LessEqual(
                    Variable(ID("L", location=Location((5, 5)))),
                    Number(100),
                    location=Location((6, 6)),
                ),
            ),
        ),
    }


def test_normalization() -> None:
    assert models.tlv_message().structure == sorted(
        [
            Link(INITIAL, Field("Tag")),
            Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("TLV::Msg_Data"))),
            Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("TLV::Msg_Error"))),
            Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Value"), FINAL),
        ],
    )


def test_set_refinements() -> None:
    message = models.message().copy()

    assert message.type_.refinements == []

    message.set_refinements([models.refinement()])

    assert message.type_.refinements == [
        ty.Refinement(
            "F",
            ty.Message(
                ID("P::M", Location((1, 1))),
                {("F",)},
                {},
                {ID("F"): ty.OPAQUE},
                refinements=[],
                is_definite=True,
            ),
            "In_Message",
        ),
    ]


def test_set_refinements_error() -> None:
    message = models.message().copy()
    with pytest.raises(
        FatalError,
        match=r"^error: setting refinements for different message$",
    ):
        message.set_refinements(
            [
                models.refinement(),
                Refinement("In_Message", models.tlv_message(), Field("Value"), models.message()),
            ],
        )


def test_message_dependencies() -> None:
    assert models.tlv_message().dependencies == [
        models.tlv_tag(),
        models.tlv_length(),
        OPAQUE,
        models.tlv_message(),
    ]
    assert models.sequence_messages_message().dependencies == [
        models.sequence_length(),
        OPAQUE,
        models.sequence_inner_message(),
        models.sequence_inner_messages(),
        models.sequence_messages_message(),
    ]


def test_message_str() -> None:
    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("L", location=Location((2, 2))))),
            Link(
                Field(ID("L", location=Location((3, 3)))),
                Field(ID("O", location=Location((3, 3)))),
                condition=And(
                    Greater(Variable("L"), Number(100), location=Location((4, 4))),
                    Equal(Variable("A"), TRUE),
                    location=Location((4, 4)),
                ),
            ),
            Link(
                Field(ID("L", location=Location((5, 5)))),
                Field(ID("P", location=Location((5, 5)))),
                condition=LessEqual(
                    Variable(ID("L", location=Location((6, 6)))),
                    Number(100),
                    location=Location((6, 6)),
                ),
            ),
            Link(Field(ID("P", location=Location((7, 7)))), FINAL),
            Link(Field(ID("O", location=Location((8, 8)))), FINAL),
        ],
        {
            Field("A"): BOOLEAN,
            Field("L"): models.integer(),
            Field("O"): models.integer(),
            Field("P"): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert_equal(
        str(message),
        textwrap.dedent(
            """\
            type M (A : Boolean) is
               message
                  L : P::Integer
                     then O
                        if L > 100
                           and A = True
                     then P
                        if L <= 100;
                  O : P::Integer
                     then null;
                  P : P::Integer;
               end message""",
        ),
    )


def test_refinement_identifier_normalization(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(Refinement, "_check_identifiers", lambda _: None)
    assert str(
        Refinement(
            "R",
            models.tlv_message(),
            Field(ID("value", location=Location((1, 1)))),
            models.tlv_message(),
            And(
                Equal(
                    Variable(ID("tag", location=Location((2, 2)))),
                    Variable(ID("tlv::msg_data", location=Location((3, 3)))),
                ),
                Equal(
                    Variable(ID("length", location=Location((4, 4)))),
                    Size(ID("tlv::length", location=Location((5, 5)))),
                ),
            ),
        ),
    ) == textwrap.dedent(
        """\
            for Message use (Value => Message)
               if Tag = TLV::Msg_Data
            and Length = TLV::Length'Size""",
    )


def test_refinement_inconsistent_identifier_casing() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            '<stdin>:1:1: error: casing of "value" differs from casing in the declaration of '
            '"Value" at <stdin>:3:3\n'
            '<stdin>:3:3: note: declaration of "Value"\n'
            '<stdin>:2:2: error: casing of "tag" differs from casing in the declaration of '
            '"Tag" at <stdin>:1:1\n'
            '<stdin>:1:1: note: declaration of "Tag"\n'
            '<stdin>:3:3: error: casing of "tlv::msg_data" differs from casing in the declaration '
            'of "TLV::Msg_Data" at <stdin>:1:1\n'
            '<stdin>:1:1: note: declaration of "TLV::Msg_Data"\n'
            '<stdin>:4:4: error: casing of "length" differs from casing in the declaration of '
            '"Length" at <stdin>:2:2\n'
            '<stdin>:2:2: note: declaration of "Length"\n'
            '<stdin>:5:5: error: casing of "tlv::length" differs from casing in the declaration '
            'of "TLV::Length" at <stdin>:1:1\n'
            '<stdin>:1:1: note: declaration of "TLV::Length"'
            "$"
        ),
    ):
        Refinement(
            ID("R", Location((1, 1))),
            models.tlv_message(),
            Field(ID("value", location=Location((1, 1)))),
            models.tlv_message(),
            And(
                Equal(
                    Variable(ID("tag", location=Location((2, 2)))),
                    Variable(
                        ID("tlv::msg_data", location=Location((3, 3))),
                        location=Location((3, 3)),
                    ),
                    location=Location((3, 3)),
                ),
                Equal(
                    Variable(ID("length", location=Location((4, 4)))),
                    Size(ID("tlv::length", location=Location((5, 5)))),
                    location=Location((5, 5)),
                ),
                location=Location((5, 5)),
            ),
            location=Location((1, 1)),
        )


def test_refinement_dependencies() -> None:
    assert models.universal_refinement().direct_dependencies == [
        models.universal_message(),
        models.universal_option(),
        models.universal_refinement(),
    ]
    assert models.universal_refinement().dependencies == [
        models.universal_message_type(),
        models.universal_length(),
        OPAQUE,
        models.universal_option_type(),
        models.universal_option_types(),
        models.universal_option(),
        models.universal_options(),
        models.universal_value(),
        models.universal_values(),
        models.universal_message(),
        models.universal_refinement(),
    ]


def test_refinement_invalid_package() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:22:10: error: unexpected format of package name "A::B"$',
    ):
        Refinement(
            ID("A::B", Location((22, 10))),
            models.ethernet_frame(),
            Field("Payload"),
            models.ethernet_frame(),
        )


def test_refinement_invalid_field_type() -> None:
    x = Field(ID("X", Location((20, 10))))

    message = Message(
        ID("P::M", Location((1, 1))),
        [Link(INITIAL, x), Link(x, FINAL)],
        {x: models.integer()},
        location=Location((1, 1), end=(1, 2)),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:33:22: error: invalid type of field "X" in refinement of "P::M"\n'
            r"<stdin>:20:10: note: expected field of type Opaque$"
        ),
    ):
        Refinement("P", message, Field(ID("X", Location((33, 22)))), message)


def test_refinement_invalid_field() -> None:
    message = Message(ID("P::M", Location((1, 1))), [], {})

    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:33:22: error: field "X" does not exist in "P::M"\n'
            r'<stdin>:1:1: note: type "P::M" declared here$'
        ),
    ):
        Refinement("P", message, Field(ID("X", Location((33, 22)))), message)


def test_refinement_undefined_variable_in_condition() -> None:
    x = Field(ID("X", location=Location((1, 1))))

    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, x, size=Number(8, location=Location((1, 1))), location=Location((1, 1))),
            Link(x, FINAL, location=Location((1, 1))),
        ],
        {x: OPAQUE},
        location=Location((1, 1), end=(1, 2)),
    )

    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:20: error: undefined variable "Y"$',
    ):
        Refinement(
            "P",
            message,
            Field("X"),
            message,
            Equal(Variable("Y", location=Location((10, 20))), Number(1)),
        )


def test_refinement_unqualified_literal_in_condition() -> None:
    e = Enumeration(
        "P2::E",
        [("E1", Number(1)), ("E2", Number(2)), ("E3", Number(3))],
        Number(8),
        always_valid=False,
    )

    x = Field(ID("X", location=Location((1, 1))))
    y = Field(ID("Y", location=Location((2, 2))))

    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, x, location=Location((1, 1))),
            Link(x, y, size=Number(8, location=Location((2, 2))), location=Location((2, 2))),
            Link(y, FINAL, location=Location((3, 3))),
        ],
        {x: e, y: OPAQUE},
        location=Location((1, 1), end=(1, 2)),
    )

    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:20: error: undefined variable "E1"$',
    ):
        Refinement(
            "P",
            message,
            y,
            message,
            Equal(Variable("X"), Variable("E1", location=Location((10, 20)))),
        )


def test_refinement_type_error_in_condition() -> None:
    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("L", location=Location((2, 2)))), location=Location((1, 1))),
            Link(
                Field(ID("L", location=Location((2, 2)))),
                Field(ID("P", location=Location((2, 2)))),
                size=Mul(Variable("L"), Number(8), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(Field(ID("P", location=Location((3, 3)))), FINAL, location=Location((3, 3))),
        ],
        {
            Field(ID("L", location=Location((1, 1)))): Integer(
                "P::T",
                Number(0),
                Number(255),
                Number(8),
            ),
            Field(ID("P", location=Location((2, 2)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:20: error: expected integer type "P::T" \(0 \.\. 255\)\n'
            r'<stdin>:10:20: error: found enumeration type "__BUILTINS__::Boolean"'
            r"$"
        ),
    ):
        Refinement(
            "P",
            message,
            Field("P"),
            message,
            Equal(Variable("L"), Literal("True", type_=ty.BOOLEAN, location=Location((10, 20)))),
        )


def test_boolean_variable_as_condition() -> None:
    Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("Tag_1"), location=Location((1, 1))),
            Link(
                Field("Tag_1"),
                Field("Tag_2"),
                condition=Variable("Has_Tag"),
                location=Location((2, 2)),
            ),
            Link(Field("Tag_2"), FINAL, location=Location((3, 3))),
        ],
        {
            Field(ID("Tag_1", location=Location((1, 1)))): models.integer(),
            Field(ID("Tag_2", location=Location((2, 2)))): models.integer(),
            Field(ID("Has_Tag", location=Location((3, 3)))): BOOLEAN,
        },
        location=Location((1, 1), end=(1, 2)),
    )


@pytest.mark.parametrize(
    ("message", "condition"),
    [
        (
            lambda: Message(
                ID("P::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("Tag"), location=Location((1, 1))),
                    Link(Field("Tag"), Field("Value"), location=Location((2, 2))),
                    Link(Field("Value"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("Tag", location=Location((1, 1)))): models.tlv_tag(),
                    Field(ID("Value", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
            Or(
                Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
            ),
        ),
        (
            lambda: Message(
                ID("P::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("Tag"), location=Location((1, 1))),
                    Link(
                        Field("Tag"),
                        Field("Value"),
                        condition=Equal(
                            Variable("Tag"),
                            Variable("TLV::Msg_Data"),
                            location=Location((2, 1)),
                        ),
                        location=Location((2, 2)),
                    ),
                    Link(Field("Value"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("Tag", location=Location((1, 1)))): models.tlv_tag(),
                    Field(ID("Value", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
            Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
        ),
    ],
)
def test_always_true_refinement(message: abc.Callable[[], Message], condition: Expr) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf'^<stdin>:10:20: error: condition "{condition}"'
            ' in refinement of "P::M" is always true$'
        ),
    ):
        Refinement(
            "In_Message",
            message(),
            Field(ID("Value", location=Location((10, 20)))),
            models.message(),
            condition,
        )


@pytest.mark.parametrize(
    ("message", "condition"),
    [
        (
            lambda: Message(
                ID("P::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("Tag"), location=Location((1, 1))),
                    Link(Field("Tag"), Field("Value"), location=Location((2, 2))),
                    Link(Field("Value"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("Tag", location=Location((1, 1)))): models.tlv_tag(),
                    Field(ID("Value", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
            And(
                Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
            ),
        ),
        (
            lambda: Message(
                ID("P::M", Location((1, 1))),
                [
                    Link(INITIAL, Field("Tag"), location=Location((1, 1))),
                    Link(
                        Field("Tag"),
                        Field("Value"),
                        condition=Equal(
                            Variable("Tag"),
                            Variable("TLV::Msg_Data"),
                            location=Location((2, 1)),
                        ),
                        location=Location((2, 2)),
                    ),
                    Link(Field("Value"), FINAL, location=Location((3, 3))),
                ],
                {
                    Field(ID("Tag", location=Location((1, 1)))): models.tlv_tag(),
                    Field(ID("Value", location=Location((2, 2)))): OPAQUE,
                },
                location=Location((1, 1), end=(1, 2)),
            ),
            Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
        ),
    ],
)
def test_always_false_refinement(message: abc.Callable[[], Message], condition: Expr) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf'^<stdin>:10:20: error: condition "{condition}"'
            ' in refinement of "P::M" is always false$'
        ),
    ):
        Refinement(
            "In_Message",
            message(),
            Field(ID("Value", location=Location((10, 20)))),
            models.message(),
            condition,
        )


@pytest.mark.parametrize(
    ("structure", "types", "expected_message"),
    [
        (
            [
                Link(INITIAL, Field("Tag"), location=Location((1, 1))),
                Link(
                    Field(ID("Tag", location=Location((10, 20)))),
                    FINAL,
                    condition=Or(
                        Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                        Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
                        location=Location((10, 20)),
                    ),
                    location=Location((1, 1)),
                ),
            ],
            {
                Field(ID("Tag", location=Location((1, 1)))): models.tlv_tag(),
            },
            (
                "<stdin>:10:20: error: condition is always true\n"
                "<stdin>:10:20: error: proven to be always true\n"
                '<stdin>:1:1: note: unsatisfied "Tag = TLV::Msg_Data or Tag = TLV::Msg_Error"\n'
                r'<stdin>:10:20: note: unsatisfied "\(Tag = TLV::Msg_Data or Tag = TLV::Msg_Error\)'
                ' = False"'
            ),
        ),
        (
            [
                Link(INITIAL, Field("Tag_1"), location=Location((1, 1))),
                Link(
                    Field("Tag_1"),
                    Field("Tag_2"),
                    condition=Equal(
                        Variable("Tag_1"),
                        Variable("TLV::Msg_Data"),
                        location=Location((2, 3)),
                    ),
                    location=Location((2, 2)),
                ),
                Link(
                    Field(ID("Tag_2", location=Location((10, 20)))),
                    FINAL,
                    condition=Equal(
                        Variable("Tag_1"),
                        Variable("TLV::Msg_Data"),
                        location=Location((10, 21)),
                    ),
                    location=Location((2, 2)),
                ),
            ],
            {
                Field(ID("Tag_1", location=Location((1, 1)))): models.tlv_tag(),
                Field(ID("Tag_2", location=Location((2, 2)))): models.tlv_tag(),
            },
            (
                "<stdin>:10:21: error: condition is always true\n"
                "<stdin>:10:21: error: proven to be always true\n"
                '<stdin>:2:3: note: unsatisfied "Tag_1 = TLV::Msg_Data"\n'
                r'<stdin>:10:21: note: unsatisfied "\(Tag_1 = TLV::Msg_Data\) = False"'
            ),
        ),
    ],
)
def test_always_true_message_condition(
    structure: abc.Sequence[Link],
    types: abc.Mapping[Field, TypeDecl],
    expected_message: str,
) -> None:
    assert_message_model_error(
        structure,
        types,
        regex=f"^{expected_message}$",
    )


@pytest.mark.parametrize("value", [0, 42, 65535])
def test_not_always_true_message_condition_for_always_valid_enum(value: int) -> None:
    Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("A"), location=Location((1, 1))),
            Link(
                Field("A"),
                FINAL,
                condition=Equal(Variable("A"), Literal("P::E"), location=Location((2, 1))),
                location=Location((2, 2)),
            ),
        ],
        {
            Field(ID("A", location=Location((1, 1)))): Enumeration(
                "P::T",
                [("E", Number(value))],
                Number(16),
                always_valid=True,
            ),
        },
        location=Location((1, 1), end=(1, 2)),
    )


def test_possibly_always_true_refinement(
    monkeypatch: pytest.MonkeyPatch,
    capfd: pytest.CaptureFixture[str],
) -> None:
    message = Message(
        ID("P::M", Location((1, 1))),
        [
            Link(INITIAL, Field("Tag"), location=Location((1, 1))),
            Link(Field("Tag"), Field("Value"), location=Location((2, 2))),
            Link(Field("Value"), FINAL, location=Location((3, 3))),
        ],
        {
            Field(ID("Tag", location=Location((1, 1)))): models.tlv_tag(),
            Field(ID("Value", location=Location((2, 2)))): OPAQUE,
        },
        location=Location((1, 1), end=(1, 2)),
    )
    condition = Or(
        Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
        Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
    )
    inner_message = models.message()
    monkeypatch.setattr(expr_proof.Proof, "result", expr_proof.ProofResult.UNKNOWN)
    Refinement(
        "In_Message",
        message,
        Field(ID("Value", location=Location((10, 20)))),
        inner_message,
        condition,
    ).error.propagate()
    captured = capfd.readouterr()
    assert (
        f'<stdin>:10:20: warning: condition "{condition}"'
        ' in refinement of "P::M" might be always false'
    ) in captured.err


def test_unchecked_message_checked() -> None:
    unchecked = UncheckedMessage(
        ID("P::No_Ref", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("F1", location=Location((2, 2)))),
                size=Number(16, location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            Link(
                Field(ID("F1", location=Location((3, 3)))),
                Field(ID("F2", location=Location((3, 3)))),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("F2", location=Location((4, 4)))),
                Field(ID("F3", location=Location((4, 4)))),
                LessEqual(
                    Variable(ID("F2", location=Location((5, 5)))),
                    Number(100),
                    location=Location((5, 5)),
                ),
                first=First("F2"),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("F2", location=Location((7, 7)))),
                Field(ID("F4", location=Location((7, 7)))),
                GreaterEqual(
                    Variable(ID("F2", location=Location((8, 8)))),
                    Number(200),
                    location=Location((8, 8)),
                ),
                first=First("F2"),
                location=Location((7, 7)),
            ),
            Link(
                Field(ID("F3", location=Location((9, 9)))),
                FINAL,
                Equal(
                    Variable(ID("F3", location=Location((10, 10)))),
                    Variable(ID("One", location=Location((10, 10)))),
                    location=Location((10, 10)),
                ),
                location=Location((9, 9)),
            ),
            Link(Field(ID("F4", location=Location((11, 11)))), FINAL, location=Location((11, 11))),
        ],
        [],
        [
            (Field(ID("F1", location=Location((1, 1)))), OPAQUE.identifier, []),
            (Field(ID("F2", location=Location((2, 2)))), models.integer().identifier, []),
            (Field(ID("F3", location=Location((3, 3)))), models.enumeration().identifier, []),
            (Field(ID("F4", location=Location((4, 4)))), models.integer().identifier, []),
        ],
        location=Location((1, 1), end=(1, 2)),
    )
    expected = Message(
        ID("P::No_Ref", Location((1, 1))),
        [
            Link(
                INITIAL,
                Field(ID("F1", location=Location((2, 2)))),
                size=Number(16, location=Location((1, 1))),
                location=Location((1, 1)),
            ),
            Link(
                Field(ID("F1", location=Location((3, 3)))),
                Field(ID("F2", location=Location((3, 3)))),
                location=Location((3, 3)),
            ),
            Link(
                Field(ID("F2", location=Location((4, 4)))),
                Field(ID("F3", location=Location((4, 4)))),
                LessEqual(
                    Variable(ID("F2", location=Location((5, 5)))),
                    Number(100, location=Location((5, 5))),
                    location=Location((5, 5)),
                ),
                first=First(ID("F2", location=Location((5, 5)))),
                location=Location((4, 4)),
            ),
            Link(
                Field(ID("F2", location=Location((6, 6)))),
                Field(ID("F4", location=Location((6, 6)))),
                GreaterEqual(
                    Variable(ID("F2", location=Location((7, 7)))),
                    Number(200, location=Location((7, 7))),
                    location=Location((7, 7)),
                ),
                first=First(ID("F2", location=Location((1, 1)))),
                location=Location((6, 6)),
            ),
            Link(
                Field(ID("F3", location=Location((9, 9)))),
                FINAL,
                Equal(
                    Variable(ID("F3", location=Location((10, 10)))),
                    Variable(ID("One", location=Location((10, 10)))),
                    location=Location((10, 10)),
                ),
                location=Location((9, 9)),
            ),
            Link(Field(ID("F4", location=Location((11, 11)))), FINAL, location=Location((11, 11))),
        ],
        {
            Field(ID("F1", location=Location((1, 1)))): OPAQUE,
            Field(ID("F2", location=Location((2, 2)))): models.integer(),
            Field(ID("F3", location=Location((3, 3)))): models.enumeration(),
            Field(ID("F4", location=Location((4, 4)))): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert unchecked.checked([OPAQUE, models.enumeration(), models.integer()]) == expected


@pytest.mark.parametrize(
    ("unchecked", "expected"),
    [
        (
            UncheckedMessage(
                ID("T", Location((2, 3))),
                [
                    Link(INITIAL, Field("F1"), size=Number(16)),
                    Link(Field("F1"), Field("F2")),
                    Link(
                        Field("F2"),
                        Field("F3"),
                        LessEqual(Variable("F2"), Number(100)),
                        first=First("F2"),
                    ),
                    Link(
                        Field("F2"),
                        Field("F4"),
                        GreaterEqual(Variable("F2"), Number(200)),
                        first=First("F2"),
                    ),
                    Link(Field("F3"), FINAL, Equal(Variable("F3"), Variable("One"))),
                    Link(Field("F4"), FINAL),
                ],
                [],
                [
                    (Field("F1"), OPAQUE.identifier, []),
                    (Field("F2"), models.integer().identifier, []),
                    (Field("F3"), models.enumeration().identifier, []),
                    (Field("F4"), models.integer().identifier, []),
                ],
            ),
            r'^<stdin>:2:3: error: invalid format for identifier "T"$',
        ),
    ],
)
def test_unchecked_message_checked_error(unchecked: UncheckedMessage, expected: str) -> None:
    with pytest.raises(RecordFluxError, match=expected):
        unchecked.checked([OPAQUE, models.enumeration(), models.integer()])


def test_message_field_first() -> None:
    field_oracle = {
        "Destination": (INITIAL, Number(0)),
        "Source": (INITIAL, Number(48)),
        "Type_Length_TPID": (INITIAL, Number(96)),
        "TPID": (INITIAL, Number(96)),
        "TCI": (INITIAL, Number(112)),
        "Type_Length": (Field("Type_Length"), Number(0)),
        "Payload": (Field("Payload"), Number(0)),
    }
    msg = models.ethernet_frame()
    for fld_name, oracle in field_oracle.items():
        assert msg.field_first(Field(fld_name)) == oracle


def test_message_link_first() -> None:
    msg = models.ethernet_frame()
    link_oracle = {
        ("Initial", "Destination"): (INITIAL, Number(0)),
        ("Destination", "Source"): (INITIAL, Number(48)),
        ("Source", "Type_Length_TPID"): (INITIAL, Number(96)),
        ("Type_Length_TPID", "TPID"): (INITIAL, Number(96)),
        ("Type_Length_TPID", "Type_Length"): (INITIAL, Number(96)),
        ("TPID", "TCI"): (INITIAL, Number(112)),
        ("TCI", "Type_Length"): (INITIAL, Number(128)),
        ("Type_Length", "Payload"): (Field("Type_Length"), Number(16)),
    }
    for lnk in msg.structure:
        if lnk.target != FINAL:
            assert msg.link_first(lnk) == link_oracle[(lnk.source.name, lnk.target.name)]


def test_message_link_first_complex() -> None:
    lnk1 = Link(Field("Payload"), Field("Other"), location=Location((3, 3)))
    lnk2 = Link(Field("Other"), Field("Other2"), location=Location((4, 4)))
    msg = Message(
        ID("Test::Message", Location((1, 1))),
        [
            Link(INITIAL, Field("Size"), location=Location((1, 1))),
            Link(
                Field("Size"),
                Field("Payload"),
                size=Mul(Variable("Size"), Number(8), location=Location((2, 2))),
                location=Location((2, 2)),
            ),
            lnk1,
            lnk2,
            Link(Field("Other2"), FINAL, location=Location((5, 5))),
        ],
        {
            Field(ID("Size", location=Location((1, 1)))): models.integer(),
            Field(ID("Payload", location=Location((2, 2)))): OPAQUE,
            Field(ID("Other", location=Location((3, 3)))): models.integer(),
            Field(ID("Other2", location=Location((4, 4)))): models.integer(),
        },
        location=Location((1, 1), end=(1, 2)),
    )
    assert msg.link_first(lnk1) == (Field("Payload"), Size(Variable("F_Payload")))
    assert msg.link_first(lnk2) == (Field("Payload"), Add(Size(Variable("F_Payload")), Number(8)))


def test_message_reject_empty() -> None:
    with pytest.raises(
        RecordFluxError,
        match="^<stdin>:1:10: error: invalid empty message$",
    ):
        Message(
            ID("P::Message", Location((1, 1))),
            [
                Link(
                    INITIAL,
                    FINAL,
                    condition=Equal(Variable("P"), TRUE),
                    location=Location((1, 10)),
                ),
                Link(INITIAL, Field("F"), condition=Equal(Variable("P"), FALSE)),
                Link(Field("F"), FINAL),
            ],
            {
                Field("P"): BOOLEAN,
                Field("F"): models.integer(),
            },
            location=Location((1, 1), end=(1, 2)),
        )


def test_message_refinement_with_scalar() -> None:
    refinement = UncheckedRefinement(
        ID("P", Location((1, 1))),
        pdu=ID("P::Foo"),
        sdu=ID("P::Bar"),
        field=Field("Foo"),
    )

    with pytest.raises(
        RecordFluxError,
        match=r'^error: type "P::Bar" cannot be used in refinement because '
        r"it's not a message type$",
    ):
        refinement.checked(
            [
                Message("P::Foo", [], {}),
                Integer(ID("P::Bar"), Number(0), Number(255), size=Number(8)),
            ],
        )


def test_invalid_missing_field() -> None:
    m1 = Message(
        ID("P::M1", Location((1, 1))),
        [
            Link(INITIAL, Field(ID("F", location=Location((1, 1)))), location=Location((1, 1))),
            Link(Field(ID("F", location=Location((2, 2)))), FINAL, location=Location((2, 2))),
        ],
        {Field("F"): models.integer()},
        location=Location((1, 1), end=(1, 2)),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:2:2: error: unreachable field "F2_F"\n'
            r'<stdin>:1:1: error: no path to FINAL for field "Initial"'
            "$"
        ),
    ):
        UncheckedMessage(
            ID("P::M2", Location((1, 1))),
            [
                Link(INITIAL, Field("F1"), location=Location((1, 1))),
                Link(Field("F1"), Field("Missing"), location=Location((2, 2))),
                Link(Field("F2"), FINAL, location=Location((3, 3))),
            ],
            [],
            [
                (Field(ID("F1", location=Location((1, 1)))), models.integer().identifier, []),
                (Field(ID("F2", location=Location((2, 2)))), m1.identifier, []),
            ],
        ).checked([OPAQUE, models.integer(), m1])


def test_annotate_path_final_link() -> None:
    sequence = [
        Link(
            Field("First"),
            FINAL,
        ),
    ]
    assert annotate_path(sequence, Location((1, 1), end=(12, 5))) == [
        Annotation(
            "on path to end of message",
            Severity.NOTE,
            Location((12, 5)),
        ),
    ]


def test_annotate_path_empty() -> None:
    assert annotate_path([], Location((1, 1), end=(12, 5))) == []


def test_annotate_path_multiple_links() -> None:
    sequence = [
        Link(Field("First"), Field(ID("Second", location=Location((2, 1))))),
        Link(Field("Second"), Field(ID("Third", location=Location((3, 1))))),
        Link(Field("Third"), Field(ID("Fourth", location=Location((4, 1))))),
    ]
    assert annotate_path(sequence, Location((1, 1), end=(12, 5))) == [
        Annotation(
            'on path "Second"',
            Severity.NOTE,
            Location((2, 1)),
        ),
        Annotation(
            'on path "Third"',
            Severity.NOTE,
            Location((3, 1)),
        ),
        Annotation(
            'on path "Fourth"',
            Severity.NOTE,
            Location((4, 1)),
        ),
    ]


def test_annotate_path_multiple_links_with_filter() -> None:
    sequence = [
        Link(Field("First"), Field(ID("Second", location=Location((2, 1))))),
        Link(Field("Second"), Field(ID("Third", location=Location((3, 1))))),
        Link(Field("Third"), Field(ID("Fourth", location=Location((4, 1))))),
    ]
    assert annotate_path(
        sequence,
        Location((1, 1), end=(12, 5)),
        lambda l: l.target != Field("Third"),
    ) == [
        Annotation(
            'on path "Second"',
            Severity.NOTE,
            Location((2, 1)),
        ),
        Annotation(
            'on path "Fourth"',
            Severity.NOTE,
            Location((4, 1)),
        ),
    ]
