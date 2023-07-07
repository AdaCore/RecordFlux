from __future__ import annotations

import textwrap
from collections import abc
from copy import deepcopy

import pytest
from _pytest.capture import CaptureFixture
from _pytest.monkeypatch import MonkeyPatch

from rflx import typing_ as rty
from rflx.error import FatalError, Location, RecordFluxError
from rflx.expression import (
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
    Proof,
    ProofResult,
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
    Type,
    UncheckedMessage,
    UnprovenDerivedMessage,
    UnprovenMessage,
)
from rflx.model.message import ByteOrder
from tests.data.models import (
    DEFINITE_MESSAGE,
    ENUMERATION,
    ETHERNET_FRAME,
    FIXED_SIZE_MESSAGE,
    FIXED_SIZE_SIMPLE_MESSAGE,
    INTEGER,
    MESSAGE,
    NULL_MESSAGE,
    REFINEMENT,
    SEQUENCE_INNER_MESSAGE,
    SEQUENCE_INNER_MESSAGES,
    SEQUENCE_INTEGER_VECTOR,
    SEQUENCE_LENGTH,
    SEQUENCE_MESSAGE,
    SEQUENCE_MESSAGES_MESSAGE,
    TLV_LENGTH,
    TLV_MESSAGE,
    TLV_TAG,
    UNIVERSAL_LENGTH,
    UNIVERSAL_MESSAGE,
    UNIVERSAL_MESSAGE_TYPE,
    UNIVERSAL_OPTION,
    UNIVERSAL_OPTION_TYPE,
    UNIVERSAL_OPTION_TYPES,
    UNIVERSAL_OPTIONS,
    UNIVERSAL_REFINEMENT,
    UNIVERSAL_VALUE,
    UNIVERSAL_VALUES,
)
from tests.utils import assert_equal, assert_message_model_error

M_NO_REF = UnprovenMessage(
    "P::No_Ref",
    [
        Link(INITIAL, Field("F1"), size=Number(16)),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), LessEqual(Variable("F2"), Number(100)), first=First("F2")),
        Link(
            Field("F2"),
            Field("F4"),
            GreaterEqual(Variable("F2"), Number(200)),
            first=First("F2"),
        ),
        Link(Field("F3"), FINAL, Equal(Variable("F3"), Variable("One"))),
        Link(Field("F4"), FINAL),
    ],
    {
        Field("F1"): OPAQUE,
        Field("F2"): INTEGER,
        Field("F3"): ENUMERATION,
        Field("F4"): INTEGER,
    },
)

M_SMPL_REF = UnprovenMessage(
    "P::Smpl_Ref",
    [Link(INITIAL, Field("NR")), Link(Field("NR"), FINAL)],
    {Field("NR"): deepcopy(M_NO_REF)},
)


M_CMPLX_REF = UnprovenMessage(
    "P::Cmplx_Ref",
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
    {
        Field("F1"): deepcopy(INTEGER),
        Field("F2"): deepcopy(INTEGER),
        Field("F3"): deepcopy(INTEGER),
        Field("NR"): deepcopy(M_NO_REF),
        Field("F5"): deepcopy(INTEGER),
        Field("F6"): deepcopy(INTEGER),
    },
)


M_DBL_REF = UnprovenMessage(
    "P::Dbl_Ref",
    [Link(INITIAL, Field("SR")), Link(Field("SR"), Field("NR")), Link(Field("NR"), FINAL)],
    {Field("SR"): deepcopy(M_SMPL_REF), Field("NR"): deepcopy(M_NO_REF)},
)


M_NO_REF_DERI = UnprovenDerivedMessage(
    "P::No_Ref_Deri",
    M_NO_REF,
    [
        Link(INITIAL, Field("F1"), size=Number(16)),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), LessEqual(Variable("F2"), Number(100)), first=First("F2")),
        Link(
            Field("F2"),
            Field("F4"),
            GreaterEqual(Variable("F2"), Number(200)),
            first=First("F2"),
        ),
        Link(Field("F3"), FINAL, Equal(Variable("F3"), Variable("One"))),
        Link(Field("F4"), FINAL),
    ],
    {
        Field("F1"): OPAQUE,
        Field("F2"): INTEGER,
        Field("F3"): ENUMERATION,
        Field("F4"): INTEGER,
    },
)


M_SMPL_REF_DERI = UnprovenDerivedMessage(
    "P::Smpl_Ref_Deri",
    M_SMPL_REF,
    [Link(INITIAL, Field("NR")), Link(Field("NR"), FINAL)],
    {Field("NR"): deepcopy(M_NO_REF_DERI)},
)

PARAMETERIZED_MESSAGE = Message(
    "P::M",
    [
        Link(
            INITIAL,
            Field("F1"),
            size=Mul(Variable("P1"), Number(8)),
        ),
        Link(
            Field("F1"),
            FINAL,
            condition=Equal(Variable("P2"), Variable("One")),
        ),
        Link(
            Field("F1"),
            Field("F2"),
            condition=Equal(Variable("P2"), Variable("Two")),
        ),
        Link(
            Field("F2"),
            FINAL,
        ),
    ],
    {
        Field("P1"): INTEGER,
        Field("P2"): ENUMERATION,
        Field("F1"): OPAQUE,
        Field("F2"): INTEGER,
    },
)


M_PARAM_NO_REF = UnprovenMessage(
    "P::Param_No_Ref",
    [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Equal(Variable("P1"), Number(1))),
        Link(Field("F1"), FINAL, condition=Equal(Variable("P1"), Number(2))),
        Link(Field("F2"), FINAL),
    ],
    {Field("P1"): INTEGER, Field("F1"): INTEGER, Field("F2"): INTEGER},
)


M_PARAM_PARAM_REF = UnprovenMessage(
    "P::Param_Param_Ref",
    [
        Link(INITIAL, Field("PNR")),
        Link(Field("PNR"), FINAL),
    ],
    {Field("P2"): INTEGER, Field("PNR"): deepcopy(M_PARAM_NO_REF)},
)


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
        match='^<stdin>:10:8: model: error: invalid format for identifier "A::B::C"$',
    ):
        Message(ID("A::B::C", location=Location((10, 8))), [], {})


@pytest.mark.parametrize(
    "parameter_type",
    [NULL_MESSAGE, TLV_MESSAGE, SEQUENCE_INTEGER_VECTOR, SEQUENCE_INNER_MESSAGES, OPAQUE],
)
def test_invalid_parameter_type_composite(parameter_type: Type) -> None:
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): parameter_type, Field("X"): INTEGER}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:1:2: model: error: parameters must have a scalar type$",
    )


def test_invalid_parameter_type_always_valid_enum() -> None:
    always_valid_enum = Enumeration(
        "P::E", [("A", Number(1)), ("B", Number(3))], Number(8), always_valid=True
    )
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): always_valid_enum, Field("X"): INTEGER}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:1:2: model: error: always valid enumeration types not allowed as parameters$",
    )


def test_missing_type() -> None:
    x = Field(ID("X", Location((5, 6))))
    structure = [Link(INITIAL, x), Link(x, FINAL)]

    assert_message_model_error(
        structure,
        {},
        '^<stdin>:5:6: model: error: missing type for field "X" in "P::M"$',
    )


def test_ambiguous_first_field() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))

    structure = [
        Link(INITIAL, Field(ID("X", Location((2, 6))))),
        Link(INITIAL, Field(ID("Y", Location((3, 6))))),
        Link(Field("X"), Field("Z")),
        Link(Field("Y"), Field("Z")),
        Link(Field("Z"), FINAL),
    ]

    types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:8: model: error: ambiguous first field in "P::M"\n'
        "<stdin>:2:6: model: info: duplicate\n"
        "<stdin>:3:6: model: info: duplicate"
        r"$",
        location=Location((10, 8)),
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
        "^<stdin>:10:20: model: error: illegal first aspect at initial link$",
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
        '^<stdin>:5:6: model: error: name conflict for field "X" in "P::M"\n'
        "<stdin>:3:27: model: info: conflicting enumeration literal$",
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
        f'^<stdin>:1:5: model: error: duplicate link from "X" to "{FINAL.name}"\n'
        f"<stdin>:4:42: model: info: duplicate link\n"
        f"<stdin>:5:42: model: info: duplicate link"
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
        f'^<stdin>:1:5: model: error: duplicate link from "X" to "{FINAL.name}"\n'
        f"<stdin>:3:16: model: info: duplicate link\n"
        f"<stdin>:4:18: model: info: duplicate link\n"
        f'<stdin>:2:5: model: error: duplicate link from "Y" to "{FINAL.name}"\n'
        f"<stdin>:5:20: model: info: duplicate link\n"
        f"<stdin>:6:22: model: info: duplicate link"
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

    types = {x: INTEGER}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:19: model: error: unsupported expression in "P::M"\n'
        '<stdin>:10:23: model: info: variable "X" in exponent$',
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
        '^<stdin>:20:3: model: error: unreachable field "Y" in "P::M"$',
    )


def test_cycle() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field(ID("X", Location((3, 5)))), Field("Y")),
        Link(Field(ID("Y", Location((3, 5)))), Field("Z")),
        Link(Field(ID("Z", Location((3, 5)))), Field("X")),
        Link(Field("X"), FINAL),
    ]

    types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:8: model: error: structure of "P::M" contains cycle$',
        # Eng/RecordFlux/RecordFlux#256
        # '\n'
        # '<stdin>:3:5: model: info: field "X" links to "Y"\n'
        # '<stdin>:4:5: model: info: field "Y" links to "Z"\n'
        # '<stdin>:5:5: model: info: field "Z" links to "X"\n',
        location=Location((10, 8)),
    )


def test_parameters() -> None:
    assert not ETHERNET_FRAME.parameters
    assert PARAMETERIZED_MESSAGE.parameters == (
        Field("P1"),
        Field("P2"),
    )


def test_fields() -> None:
    assert ETHERNET_FRAME.fields == (
        Field("Destination"),
        Field("Source"),
        Field("Type_Length_TPID"),
        Field("TPID"),
        Field("TCI"),
        Field("Type_Length"),
        Field("Payload"),
    )
    assert PARAMETERIZED_MESSAGE.fields == (
        Field("F1"),
        Field("F2"),
    )


def test_parameter_types() -> None:
    assert PARAMETERIZED_MESSAGE.parameter_types == {
        Field("P1"): INTEGER,
        Field("P2"): ENUMERATION,
    }


def test_field_types() -> None:
    assert PARAMETERIZED_MESSAGE.field_types == {
        Field("F1"): OPAQUE,
        Field("F2"): INTEGER,
    }


def test_path_condition() -> None:
    assert_equal(ETHERNET_FRAME.path_condition(INITIAL), TRUE)
    assert_equal(
        ETHERNET_FRAME.path_condition(Field("TPID")),
        Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
    )
    assert_equal(
        ETHERNET_FRAME.path_condition(Field("Type_Length")),
        TRUE,
    )
    assert_equal(
        ETHERNET_FRAME.path_condition(Field("Payload")),
        Or(
            LessEqual(Variable("Type_Length"), Number(1500)),
            GreaterEqual(Variable("Type_Length"), Number(1536)),
        ),
    )


def test_incoming() -> None:
    assert_equal(ETHERNET_FRAME.incoming(INITIAL), [])
    assert_equal(
        ETHERNET_FRAME.incoming(Field("Type_Length")),
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
        ETHERNET_FRAME.incoming(FINAL),
        [
            Link(
                Field("Payload"),
                FINAL,
                And(
                    GreaterEqual(Div(Size("Payload"), Number(8)), Number(46)),
                    LessEqual(Div(Size("Payload"), Number(8)), Number(1500)),
                ),
            )
        ],
    )


def test_outgoing() -> None:
    assert_equal(ETHERNET_FRAME.outgoing(INITIAL), [Link(INITIAL, Field("Destination"))])
    assert_equal(
        ETHERNET_FRAME.outgoing(Field("Type_Length")),
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
    assert_equal(ETHERNET_FRAME.outgoing(FINAL), [])


def test_direct_predecessors() -> None:
    assert_equal(ETHERNET_FRAME.direct_predecessors(INITIAL), [])
    assert_equal(
        ETHERNET_FRAME.direct_predecessors(Field("Type_Length")),
        [Field("TCI"), Field("Type_Length_TPID")],
    )
    assert_equal(ETHERNET_FRAME.direct_predecessors(FINAL), [Field("Payload")])


def test_direct_successors() -> None:
    assert_equal(ETHERNET_FRAME.direct_successors(INITIAL), [Field("Destination")])
    assert_equal(ETHERNET_FRAME.direct_successors(Field("Type_Length")), [Field("Payload")])
    assert_equal(ETHERNET_FRAME.direct_successors(FINAL), [])


def test_definite_predecessors() -> None:
    assert_equal(
        ETHERNET_FRAME.definite_predecessors(FINAL),
        (
            Field("Destination"),
            Field("Source"),
            Field("Type_Length_TPID"),
            Field("Type_Length"),
            Field("Payload"),
        ),
    )
    assert_equal(
        ETHERNET_FRAME.definite_predecessors(Field("TCI")),
        (Field("Destination"), Field("Source"), Field("Type_Length_TPID"), Field("TPID")),
    )


def test_predecessors() -> None:
    assert_equal(
        ETHERNET_FRAME.predecessors(FINAL),
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
        ETHERNET_FRAME.predecessors(Field("TCI")),
        (Field("Destination"), Field("Source"), Field("Type_Length_TPID"), Field("TPID")),
    )
    assert_equal(ETHERNET_FRAME.predecessors(Field("Destination")), ())
    assert_equal(ETHERNET_FRAME.predecessors(INITIAL), ())


def test_successors() -> None:
    assert_equal(
        ETHERNET_FRAME.successors(INITIAL),
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
        ETHERNET_FRAME.successors(Field("Source")),
        (
            Field("Type_Length_TPID"),
            Field("TPID"),
            Field("TCI"),
            Field("Type_Length"),
            Field("Payload"),
        ),
    )
    assert_equal(
        ETHERNET_FRAME.successors(Field("TPID")),
        (Field("TCI"), Field("Type_Length"), Field("Payload")),
    )
    assert_equal(ETHERNET_FRAME.successors(Field("Payload")), ())
    assert_equal(ETHERNET_FRAME.successors(FINAL), ())


def test_field_identifier_locations() -> None:
    p = Field(ID("P", Location((0, 1))))
    a = Field(ID("A", Location((1, 2))))
    b = Field(ID("B", Location((2, 3))))

    message = UnprovenMessage(
        "P::M",
        [Link(INITIAL, Field("A")), Link(Field("A"), Field("B")), Link(Field("B"), FINAL)],
        {p: INTEGER, a: INTEGER, b: INTEGER},
    )

    assert message.parameters == (p,)
    assert message.parameters[0].identifier.location == p.identifier.location
    assert message.fields == (a, b)
    assert message.fields[0].identifier.location == a.identifier.location
    assert message.fields[1].identifier.location == b.identifier.location


@pytest.mark.parametrize(
    "expression",
    [
        Variable(INTEGER.identifier, location=Location((1, 2))),
        And(
            TRUE,
            Variable(INTEGER.identifier, location=Location((1, 2))),
            location=Location((3, 4)),
        ),
        Equal(
            Add(Variable(INTEGER.identifier, location=Location((1, 2))), Number(1)),
            Number(1),
        ),
        Equal(
            Variable(INTEGER.identifier, location=Location((1, 2))),
            Variable("X"),
        ),
    ],
)
def test_invalid_use_of_type_name(expression: Expr) -> None:
    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL, condition=expression),
    ]
    types = {Field("X"): INTEGER}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:2: model: error: invalid use of type name "P::Integer" in expression$',
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
    types = {Field("X"): ENUMERATION}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:2: model: error: invalid use of enum literal "P::Zero" in expression$',
    )


class NewType(Type):
    pass


@pytest.mark.skipif(not __debug__, reason="depends on contract")
def test_invalid_message_field_type() -> None:
    with pytest.raises(AssertionError):
        Message(
            "P::M",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): NewType("P::T")},
        )


def test_unused_parameter() -> None:
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): INTEGER, Field("X"): INTEGER}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:1:2: model: error: unused parameter "P"$',
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
    operation: abc.Callable[[Expr, Expr], Expr], condition: tuple[Expr, Expr]
) -> None:
    mod_type = Integer("P::MT", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32))
    enum_type = Enumeration(
        "P::ET", [("Val1", Number(0)), ("Val2", Number(1))], Number(8), always_valid=True
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
        r'^<stdin>:10:20: model: error: undefined variable "X"$',
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
        structure, types, r'^<stdin>:10:20: model: error: undefined variable "X"$'
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
        structure, types, r'^<stdin>:10:20: model: error: undefined variable "Field_Size"$'
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
        structure, types, r'^<stdin>:10:20: model: error: undefined variable "Field_First"$'
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

    types = {Field("F1"): INTEGER, Field("F2"): INTEGER}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: model: error: undefined variable "X"\n'
        r'<stdin>:10:30: model: error: undefined variable "Y"$',
    )


def test_subsequent_variable() -> None:
    f1 = Field("F1")
    f2 = Field("F2")
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
        '^<stdin>:1024:57: model: error: undefined variable "F2"\n'
        r"model: info: on path F1 -> F2$",
    )


def test_reference_to_optional_field_1() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Equal(Variable("F1"), TRUE)),
        Link(Field("F1"), Field("F3"), Equal(Variable("F1"), FALSE)),
        Link(Field("F2"), Field("F3")),
        Link(
            Field("F3"),
            FINAL,
            Equal(Variable("F2", location=Location((10, 30))), TRUE, location=Location((10, 20))),
        ),
    ]

    types = {Field("F1"): BOOLEAN, Field("F2"): BOOLEAN, Field("F3"): BOOLEAN}

    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:10:30: model: error: undefined variable "F2"\n'
        r"<stdin>:10:20: model: info: on path F1 -> F3 -> Final"
        r"$",
    )


def test_reference_to_optional_field_2() -> None:
    structure = [
        Link(INITIAL, Field("Flag")),
        Link(Field("Flag"), Field("Opt"), Equal(Variable("Flag"), Number(1))),
        Link(Field("Flag"), Field("Any"), NotEqual(Variable("Flag"), Number(1))),
        Link(Field("Opt"), Field("Any")),
        Link(
            Field("Any"),
            Field("Data"),
            size=Mul(
                Variable("Opt", location=Location((10, 30))), Number(8), location=Location((10, 20))
            ),
        ),
        Link(Field("Data"), FINAL),
    ]
    types = {
        Field("Flag"): INTEGER,
        Field("Opt"): INTEGER,
        Field("Any"): INTEGER,
        Field("Data"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:10:30: model: error: undefined variable "Opt"\n'
        r"<stdin>:10:20: model: info: on path Flag -> Any -> Data"
        r"$",
    )


def test_invalid_use_of_size_attribute() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, Equal(Size(Number(1)), Number(32), Location((400, 17)))),
    ]
    types = {Field("F1"): INTEGER}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:400:17: model: error: invalid use of size attribute for "1"$',
    )


def test_invalid_relation_to_opaque() -> None:
    structure = [
        Link(INITIAL, Field("Length")),
        Link(Field("Length"), Field("Data"), size=Variable("Length")),
        Link(
            Field("Data"),
            FINAL,
            condition=Equal(Variable("Data"), Number(42, location=Location((10, 20)))),
        ),
    ]
    types = {Field("Length"): INTEGER, Field("Data"): OPAQUE}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: model: error: expected sequence type "__INTERNAL__::Opaque"'
        r' with element integer type "Byte" \(0 .. 255\)\n'
        r"<stdin>:10:20: model: info: found type universal integer \(42\)\n"
        r"model: info: on path Length -> Data -> Final$",
    )


def test_invalid_relation_to_aggregate() -> None:
    structure = [
        Link(INITIAL, Field("F1"), size=Number(16)),
        Link(
            Field("F1"),
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
        r"^<stdin>:10:20: model: error: expected integer type\n"
        r'<stdin>:10:20: model: info: found sequence type "__INTERNAL__::Opaque"'
        r' with element integer type "Byte" \(0 .. 255\)\n'
        r"<stdin>:10:30: model: error: expected integer type\n"
        r"<stdin>:10:30: model: info: found aggregate"
        r" with element type universal integer \(1 .. 2\)\n"
        r"model: info: on path F1 -> Final$",
    )


def test_invalid_element_in_relation_to_aggregate() -> None:
    integer = Integer("P::Integer", Number(0), Number(255), Number(8))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            FINAL,
            Equal(Variable("F1"), Aggregate(Number(1), Number(2), location=Location((10, 20)))),
        ),
    ]

    types = {Field("F1"): integer}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: model: error: expected integer type "P::Integer" \(0 .. 255\)\n'
        r"<stdin>:10:20: model: info: found aggregate with element type universal integer"
        r" \(1 .. 2\)\n"
        r"model: info: on path F1 -> Final$",
    )


def test_opaque_aggregate_out_of_range() -> None:
    f = Field("F")

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
        r'^<stdin>:10:20: model: error: expected sequence type "__INTERNAL__::Opaque"'
        r' with element integer type "Byte" \(0 .. 255\)\n'
        r"<stdin>:10:20: model: info: found aggregate"
        r" with element type universal integer \(1 .. 256\)\n"
        r"model: info: on path F -> Final$",
    )


def test_sequence_aggregate_out_of_range() -> None:
    f = Field("F")

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

    types = {
        Field("F"): Sequence("P::Sequence", Integer("P::Element", Number(0), Number(63), Number(8)))
    }

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: model: error: expected sequence type "P::Sequence"'
        r' with element integer type "P::Element" \(0 .. 63\)\n'
        r"<stdin>:10:20: model: info: found aggregate"
        r" with element type universal integer \(1 .. 64\)\n"
        r"model: info: on path F -> Final$",
    )


def test_sequence_aggregate_invalid_element_type() -> None:
    inner = Message(
        "P::I",
        [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
        {Field("F"): INTEGER},
    )
    sequence_type = Sequence("P::Sequence", inner)
    f = Field("F")

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
        r'^<stdin>:10:20: model: error: expected sequence type "P::Sequence"'
        r' with element message type "P::I"\n'
        r"<stdin>:10:20: model: info: found aggregate with element type universal integer"
        r" \(1 .. 64\)\n"
        r"model: info: on path F -> Final$",
    )


def test_opaque_not_byte_aligned() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:44:3: model: error: opaque field "O" not aligned to'
            r" 8 bit boundary [(]P -> O[)]$"
        ),
    ):
        o = Field(ID("O", location=Location((44, 3))))
        Message(
            "P::M",
            [Link(INITIAL, Field("P")), Link(Field("P"), o, size=Number(128)), Link(o, FINAL)],
            {Field("P"): Integer("P::T", Number(0), Number(3), Number(2)), o: OPAQUE},
        )


def test_opaque_not_byte_aligned_dynamic() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: opaque field "O2" not aligned to'
        r" 8 bit boundary [(]L1 -> O1 -> L2 -> O2[)]",
    ):
        o2 = Field(ID("O2", location=Location((44, 3))))
        Message(
            "P::M",
            [
                Link(INITIAL, Field("L1")),
                Link(
                    Field("L1"),
                    Field("O1"),
                    size=Variable("L1"),
                    condition=Equal(Mod(Variable("L1"), Number(8)), Number(0)),
                ),
                Link(Field("O1"), Field("L2")),
                Link(Field("L2"), o2, size=Number(128)),
                Link(o2, FINAL),
            ],
            {
                Field("L1"): INTEGER,
                Field("L2"): Integer("P::T", Number(0), Number(3), Number(2)),
                Field("O1"): OPAQUE,
                o2: OPAQUE,
            },
        )


def test_opaque_valid_byte_aligned_dynamic_mul() -> None:
    Message(
        "P::M",
        [
            Link(INITIAL, Field("L")),
            Link(Field("L"), Field("O1"), size=Mul(Number(8), Variable("L"))),
            Link(Field("O1"), FINAL),
        ],
        {Field("L"): INTEGER, Field("O1"): OPAQUE},
    )


def test_opaque_valid_byte_aligned_dynamic_cond() -> None:
    Message(
        "P::M",
        [
            Link(INITIAL, Field("L")),
            Link(
                Field("L"),
                Field("O1"),
                size=Variable("L"),
                condition=Equal(Mod(Variable("L"), Number(8)), Number(0)),
            ),
            Link(Field("O1"), Field("O2"), size=Number(128)),
            Link(Field("O2"), FINAL),
        ],
        {Field("L"): INTEGER, Field("O1"): OPAQUE, Field("O2"): OPAQUE},
    )


def test_opaque_size_not_multiple_of_8() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:44:3: model: error: size of opaque field "O"'
            " not multiple of 8 bit [(]O[)]$"
        ),
    ):
        o = Field("O")
        Message(
            "P::M",
            [Link(INITIAL, o, size=Number(68, location=Location((44, 3)))), Link(o, FINAL)],
            {o: OPAQUE},
        )


def test_opaque_size_not_multiple_of_8_dynamic() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: size of opaque field "O" not multiple of 8 bit'
        " [(]L -> O[)]",
    ):
        o = Field("O")
        Message(
            "P::M",
            [
                Link(INITIAL, Field("L")),
                Link(Field("L"), o, size=Variable("L", location=Location((44, 3)))),
                Link(o, FINAL),
            ],
            {Field("L"): INTEGER, o: OPAQUE},
        )


def test_opaque_size_valid_multiple_of_8_dynamic_cond() -> None:
    Message(
        "P::M",
        [
            Link(INITIAL, Field("L")),
            Link(
                Field("L"),
                Field("O"),
                size=Variable("L"),
                condition=Equal(Mod(Variable("L"), Number(8)), Number(0)),
            ),
            Link(Field("O"), FINAL),
        ],
        {Field("L"): INTEGER, Field("O"): OPAQUE},
    )


def test_prefixed_message_attribute() -> None:
    result = Message(
        "P::M",
        [
            Link(INITIAL, Field("F1")),
            Link(
                Field("F1"),
                Field("F2"),
                LessEqual(Variable("F1"), Number(100)),
                first=First("F1"),
            ),
            Link(
                Field("F1"),
                Field("F3"),
                GreaterEqual(Variable("F1"), Number(200)),
                first=First("F1"),
            ),
            Link(Field("F2"), FINAL, Equal(Variable("F2"), Number(42))),
            Link(Field("F3"), Field("F4")),
            Link(Field("F4"), FINAL),
        ],
        {
            Field("F1"): deepcopy(INTEGER),
            Field("F2"): deepcopy(INTEGER),
            Field("F3"): deepcopy(INTEGER),
            Field("F4"): OPAQUE,
        },
    ).prefixed("X_")

    expected = Message(
        "P::M",
        [
            Link(INITIAL, Field("X_F1")),
            Link(
                Field("X_F1"),
                Field("X_F2"),
                LessEqual(Variable("X_F1"), Number(100)),
                first=First("X_F1"),
            ),
            Link(
                Field("X_F1"),
                Field("X_F3"),
                GreaterEqual(Variable("X_F1"), Number(200)),
                first=First("X_F1"),
            ),
            Link(Field("X_F2"), FINAL, Equal(Variable("X_F2"), Number(42))),
            Link(Field("X_F3"), Field("X_F4"), size=Add(Last("Message"), -Last("X_F3"))),
            Link(Field("X_F4"), FINAL),
        ],
        {
            Field("X_F1"): deepcopy(INTEGER),
            Field("X_F2"): deepcopy(INTEGER),
            Field("X_F3"): deepcopy(INTEGER),
            Field("X_F4"): OPAQUE,
        },
    )

    assert result == expected


def test_exclusive_valid() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, condition=Greater(Variable("F1"), Number(80))),
        Link(Field("F1"), Field("F2"), condition=LessEqual(Variable("F1"), Number(80))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    Message("P::M", structure, types)


def test_exclusive_enum_valid() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, condition=Equal(Variable("F1"), Variable("One"))),
        Link(Field("F1"), Field("F2"), condition=Equal(Variable("F1"), Variable("Two"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): ENUMERATION,
        Field("F2"): INTEGER,
    }
    Message("P::M", structure, types)


def test_exclusive_prefixed_enum_valid() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, condition=Equal(Variable("F1"), Variable("One"))),
        Link(Field("F1"), Field("F2"), condition=Equal(Variable("F1"), Variable("P::Two"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): ENUMERATION,
        Field("F2"): Enumeration(
            "P2::Enumeration",
            [("One", Number(2)), ("Two", Number(1))],
            Number(8),
            always_valid=False,
        ),
    }
    Message("P::M", structure, types)


def test_exclusive_conflict() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, condition=Greater(Variable("F1"), Number(50), Location((10, 5)))),
        Link(
            Field("F1"), Field("F2"), condition=Less(Variable("F1"), Number(80), Location((11, 7)))
        ),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field(ID("F1", Location((8, 4)))): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:8:4: model: error: conflicting conditions for field "F1"\n'
        r"<stdin>:11:7: model: info: condition 0 [(]F1 -> F2[)]: F1 < 80\n"
        r"<stdin>:10:5: model: info: condition 1 [(]F1 -> Final[)]: F1 > 50"
        r"$",
    )


def test_exclusive_with_size_valid() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Number(32)),
        Link(
            Field("F2"),
            FINAL,
            condition=And(Equal(Size("F2"), Number(32)), Less(Variable("F1"), Number(50))),
        ),
        Link(
            Field("F2"),
            Field("F3"),
            condition=And(Equal(Size("F2"), Number(32)), Greater(Variable("F1"), Number(80))),
        ),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): OPAQUE,
        Field("F3"): INTEGER,
    }
    Message("P::M", structure, types)


def test_exclusive_with_size_valid_and_not() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Number(32)),
        Link(
            Field("F2"),
            FINAL,
            condition=And(Equal(Size("F2"), Number(32)), Less(Variable("F1"), Number(50))),
        ),
        Link(
            Field("F2"),
            Field("F3"),
            condition=And(Equal(Size("F2"), Number(32)), Not(Less(Variable("F1"), Number(80)))),
        ),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): OPAQUE,
        Field("F3"): INTEGER,
    }
    Message("P::M", structure, types)


def test_exclusive_with_size_invalid() -> None:
    structure = [
        Link(INITIAL, Field("F1"), size=Number(32)),
        Link(Field("F1"), FINAL, condition=Equal(Size("F1"), Number(32), Location((10, 2)))),
        Link(Field("F1"), Field("F2"), condition=Equal(Size("F1"), Number(32), Location((12, 4)))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field(ID("F1", Location((98, 10)))): OPAQUE,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:98:10: model: error: conflicting conditions for field "F1"\n'
        r"<stdin>:12:4: model: info: condition 0 [(]F1 -> F2[)]: F1\'Size = 32\n"
        r"<stdin>:10:2: model: info: condition 1 [(]F1 -> Final[)]: F1\'Size = 32"
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
        f1: INTEGER,
        f2: INTEGER,
        f3: INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:11:6: model: error: unreachable field "F2" in "P::M"\n'
        r"<stdin>:11:6: model: info: path 0 [(]F1 -> F2[)]:\n"
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:11:9: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:12:7: model: error: unreachable field "F3" in "P::M"\n'
        r"<stdin>:12:7: model: info: path 0 [(]F1 -> F2 -> F3[)]:\n"
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: info: unsatisfied "F1 > 80"\n'
        r"<stdin>:12:7: model: info: path 1 [(]F1 -> F3[)]:\n"
        r'<stdin>:21:3: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:12:10: model: info: unsatisfied "F1 <= 80"\n'
        r'model: error: unreachable field "Final" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> F3 -> Final[)]:\n"
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: info: unsatisfied "F1 > 80"\n'
        r"model: info: path 1 [(]F1 -> F3 -> Final[)]:\n"
        r'<stdin>:21:3: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:23:5: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: error: contradicting condition in "P::M"\n'
        r'<stdin>:10:8: model: info: on path: "F1"\n'
        r'<stdin>:11:9: model: info: on path: "F2"\n'
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:23:5: model: error: contradicting condition in "P::M"\n'
        r'<stdin>:10:8: model: info: on path: "F1"\n'
        r'<stdin>:11:9: model: info: on path: "F2"\n'
        r'<stdin>:13:10: model: info: on path: "F3"\n'
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:23:5: model: error: contradicting condition in "P::M"\n'
        r'<stdin>:10:8: model: info: on path: "F1"\n'
        r'<stdin>:12:10: model: info: on path: "F3"\n'
        r'<stdin>:21:3: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:23:5: model: info: unsatisfied "F1 <= 80"'
        r"$",
    )


def test_invalid_path_1(monkeypatch: MonkeyPatch) -> None:
    f1 = Field(ID("F1", Location((20, 10))))
    structure = [
        Link(INITIAL, f1),
        Link(f1, FINAL, condition=Equal(Number(1), Number(2), Location((5, 10)))),
    ]
    types = {
        Field("F1"): INTEGER,
    }
    monkeypatch.setattr(Message, "_prove_reachability", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:5:10: model: error: contradicting condition in "P::M"\n'
        r'<stdin>:20:10: model: info: on path: "F1"\n'
        r'<stdin>:5:10: model: info: unsatisfied "1 = 2"'
        r"$",
    )


def test_invalid_path_2(monkeypatch: MonkeyPatch) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Equal(Number(1), Number(2))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    monkeypatch.setattr(Message, "_prove_reachability", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "1 = 2"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "1 = 2"'
        r"$",
    )


def test_contradiction() -> None:
    integer = Integer("P::Integer", Number(1), Number(100), Number(8))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Greater(Variable("F1"), Number(1000))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): integer,
        Field("F2"): integer,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1 <= 100"\n'
        r'model: info: unsatisfied "F1 > 1000"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1 > 1000"\n'
        r'model: info: unsatisfied "F1 <= 100"'
        r"$",
    )


def test_invalid_type_condition_range_low() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Less(Variable("F1"), Number(1))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1 >= 1"\n'
        r'model: info: unsatisfied "F1 < 1"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1 >= 1"\n'
        r'model: info: unsatisfied "F1 < 1"'
        r"$",
    )


def test_invalid_type_condition_range_high() -> None:
    integer = Integer("P::Integer", Number(1), Number(100), Number(8))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Greater(Variable("F1"), Number(200))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): integer,
        Field("F2"): integer,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1 <= 100"\n'
        r'model: info: unsatisfied "F1 > 200"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1 > 200"\n'
        r'model: info: unsatisfied "F1 <= 100"'
        r"$",
    )


def test_invalid_type_condition_enum() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            Field("F2"),
            condition=Equal(
                Variable("F1"),
                Variable("E4", location=Location((10, 20))),
                location=Location((10, 10)),
            ),
        ),
        Link(Field("F2"), FINAL),
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
        r'^<stdin>:10:20: model: error: expected enumeration type "P::E1"\n'
        r'<stdin>:10:20: model: info: found enumeration type "P::E2"\n'
        r"<stdin>:10:10: model: info: on path F1 -> F2$",
    )


def test_tlv_valid_enum() -> None:
    structure = [
        Link(INITIAL, Field("L")),
        Link(Field("L"), Field("T")),
        Link(
            Field("T"),
            Field("V"),
            size=Mul(Number(8), Variable("L")),
            condition=And(
                NotEqual(Variable("T"), Variable("Two")), LessEqual(Variable("L"), Number(8192))
            ),
        ),
        Link(Field("V"), FINAL),
    ]
    types = {
        Field("L"): INTEGER,
        Field("T"): ENUMERATION,
        Field("V"): OPAQUE,
    }
    Message("P::M", structure, types)


def test_invalid_fixed_size_field_with_size() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Number(300)),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r'^model: error: fixed size field "F2" with size aspect$',
    )


def test_valid_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=First("F1")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    Message("P::M", structure, types)


def test_invalid_first_is_not_previous_field() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), first=First(ID("F1", location=Location((5, 14))))),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
        Field("F3"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:5:14: model: error: invalid First for field "F3"$',
    )


def test_invalid_first_is_expression() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"), Field("F2"), first=Add(First("F1"), Number(8), location=Location((5, 14)))
        ),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:5:14: model: error: invalid First for field "F2"$',
    )


def test_invalid_first_is_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Last(ID("F1", Location((11, 20))))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:11:20: model: error: invalid First for field "F2"$',
    )


def test_invalid_first_forward_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=First(Variable("F3", location=Location((10, 20))))),
        Link(Field("F2"), Field("F3")),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
        Field("F3"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:10:20: model: error: undefined variable "F3"\n'
        r"<stdin>:10:20: model: info: on path F1 -> F2\n"
        r'<stdin>:10:20: model: error: invalid First for field "F2"'
        r"$",
    )


def test_valid_size_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Mul(Number(8), Variable("F1"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): OPAQUE,
    }
    Message("P::M", structure, types)


def test_invalid_size_forward_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Variable("F2", location=Location((10, 20)))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:20: model: error: undefined variable "F2"\n'
        r"<stdin>:10:20: model: info: on path F1 -> F2$",
    )


def test_invalid_negative_field_size_1() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Sub(Variable("F1"), Number(2))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: negative size for field "F2" [(]F1 -> F2[)]\n'
        r'model: error: size of opaque field "F2" not multiple of 8 bit [(]F1 -> F2[)]'
        "$",
    )


def test_invalid_negative_field_size_2() -> None:
    o = Field("O")
    structure = [
        Link(INITIAL, Field("L")),
        Link(
            Field("L"),
            o,
            size=Mul(Number(8), Sub(Variable("L"), Number(50)), location=Location((44, 3))),
        ),
        Link(o, FINAL),
    ]
    types = {
        Field("L"): INTEGER,
        o: OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:44:3: model: error: negative size for field "O" [(]L -> O[)]$',
    )


def test_payload_no_size() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): OPAQUE,
        Field("F2"): OPAQUE,
    }
    assert_message_model_error(
        structure, types, r'^model: error: unconstrained field "F1" without size aspect$'
    )


def test_sequence_no_size() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): SEQUENCE_INTEGER_VECTOR,
        Field("F2"): SEQUENCE_INTEGER_VECTOR,
    }
    assert_message_model_error(
        structure, types, '^model: error: unconstrained field "F1" without size aspect$'
    )


def test_incongruent_overlay() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), first=First("F2")),
        Link(Field("F3"), Field("F4")),
        Link(Field("F4"), FINAL),
    ]
    u8 = Integer("P::U8", Number(0), Sub(Pow(Number(2), Number(8)), Number(1)), Number(8))
    u16 = Integer("P::U16", Number(0), Sub(Pow(Number(2), Number(16)), Number(1)), Number(16))
    types = {
        Field("F1"): u8,
        Field("F2"): u8,
        Field("F3"): u16,
        Field("F4"): u16,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: field "F3" not congruent with overlaid field "F2"\n'
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "F2\'First = F1\'Last [+] 1"\n'
        r'model: info: unsatisfied "[(]F2\'First [+] 16[)] - 1 = F2\'Last"'
        r"$",
    )


def test_field_coverage_1(monkeypatch: MonkeyPatch) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Add(First("Message"), Number(64))),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): INTEGER, Field("F2"): INTEGER}
    monkeypatch.setattr(Message, "_verify_expressions", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r"model: error: path does not cover whole message\n"
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"'
        r"$",
    )


def test_field_coverage_2(monkeypatch: MonkeyPatch) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F4"), Greater(Variable("F1"), Number(100))),
        Link(
            Field("F2"),
            Field("F3"),
            LessEqual(Variable("F1"), Number(100)),
            first=Add(Last("F2"), Number(64)),
        ),
        Link(Field("F3"), Field("F4")),
        Link(Field("F4"), FINAL),
    ]

    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
        Field("F3"): INTEGER,
        Field("F4"): INTEGER,
    }
    monkeypatch.setattr(Message, "_verify_expressions", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r"model: error: path does not cover whole message\n"
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: on path: "F3"\n'
        r'model: info: on path: "F4"'
        r"$",
    )


def test_field_after_message_start(monkeypatch: MonkeyPatch) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Sub(First("Message"), Number(1000))),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): INTEGER, Field("F2"): INTEGER}
    monkeypatch.setattr(Message, "_verify_expressions", lambda x: None)
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: negative start for field "F2" [(]F1 -> F2[)]\n'
        r'model: info: unsatisfied "Message\'First - 1000 >= Message\'First"\n'
        r'model: error: negative start for field "Final" [(]F1 -> F2 -> Final[)]\n'
        r'model: info: unsatisfied "F2\'Last = [(]Message\'First - 1000 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "F2\'Last [+] 1 >= Message\'First"'
        r"$",
    )


@pytest.mark.parametrize("size", [UNDEFINED, Size("Message")])
@pytest.mark.parametrize(
    "condition",
    [
        Equal(Size("Message"), Number(64)),
        Equal(Add(Sub(Last("Message"), First("Message")), Number(1)), Number(64)),
    ],
)
@pytest.mark.parametrize("type_", [OPAQUE, SEQUENCE_INTEGER_VECTOR])
def test_message_with_implicit_size_single_field(size: Expr, condition: Expr, type_: Type) -> None:
    x = Field("X")

    structure = [
        Link(INITIAL, x, size=size),
        Link(x, FINAL, condition=condition),
    ]

    types = {x: type_}

    message = Message("P::M", structure, types)

    assert message.structure[0] == Link(INITIAL, x, size=Size("Message"))


@pytest.mark.parametrize("size", [UNDEFINED, Sub(Last("Message"), Last("X"))])
@pytest.mark.parametrize(
    "condition",
    [
        Equal(Size("Message"), Number(64)),
        Equal(Add(Sub(Last("Message"), First("Message")), Number(1)), Number(64)),
    ],
)
@pytest.mark.parametrize("type_", [OPAQUE, SEQUENCE_INTEGER_VECTOR])
def test_message_with_implicit_size_multiple_fields(
    size: Expr, condition: Expr, type_: Type
) -> None:
    x = Field("X")
    y = Field("Y")

    structure = [
        Link(INITIAL, x, size=Number(8)),
        Link(x, y, size=size),
        Link(y, FINAL, condition=condition),
    ]

    types = {x: type_, y: type_}

    message = Message("P::M", structure, types)

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
        r'<stdin>:1:2: model: error: "Message" must not be used in size aspects\n'
        r'<stdin>:5:6: model: error: invalid use of "Message" in size aspect\n'
        r"<stdin>:5:6: model: info: remove size aspect to define field with implicit size"
        r"$",
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
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
        Field("F3"): INTEGER,
        Field("F4"): INTEGER,
    }
    assert_message_model_error(
        structure, types, '^model: error: no path to FINAL for field "F4" in "P::M"$'
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
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
        Field("F3"): INTEGER,
        Field("F4"): INTEGER,
        Field("F5"): INTEGER,
        Field("F6"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: no path to FINAL for field "F4" in "P::M"\n'
        r'model: error: no path to FINAL for field "F5" in "P::M"\n'
        r'model: error: no path to FINAL for field "F6" in "P::M"'
        r"$",
    )


def test_conditionally_unreachable_field_mod_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F1" in "P::M"\n'
        r"model: info: path 0 [(]F1[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "F2" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "Final" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"'
        r"$",
    )


def test_conditionally_unreachable_field_mod_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, Equal(Last("F1"), Last("Message"))),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: unreachable field "Final" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_conditionally_unreachable_field_range_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F1" in "P::M"\n'
        r"model: info: path 0 [(]F1[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "F2" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "Final" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"'
        r"$",
    )


def test_conditionally_unreachable_field_range_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, Equal(Last("F1"), Last("Message"))),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: unreachable field "Final" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_conditionally_unreachable_field_enum_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): ENUMERATION,
        Field("F2"): ENUMERATION,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F1" in "P::M"\n'
        r"model: info: path 0 [(]F1[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "F2" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: unreachable field "Final" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_conditionally_unreachable_field_enum_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, Equal(Last("F1"), Last("Message"))),
    ]
    types = {
        Field("F1"): ENUMERATION,
        Field("F2"): ENUMERATION,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: unreachable field "Final" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2 -> Final[)]:\n"
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_conditionally_unreachable_field_outgoing() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), LessEqual(Variable("F1"), Number(32))),
        Link(Field("F1"), FINAL, Greater(Variable("F1"), Number(32))),
        Link(Field("F2"), FINAL, Greater(Variable("F1"), Number(32))),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2" in "P::M"\n'
        r"model: info: path 0 [(]F1 -> F2[)]:\n"
        r'model: info: unsatisfied "F1 <= 32"\n'
        r'model: info: unsatisfied "F1 > 32"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1 <= 32"\n'
        r'model: info: unsatisfied "F1 > 32"'
        r"$",
    )


def test_conditionally_unreachable_field_outgoing_multi() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            Field(ID("F2", Location((91, 13)))),
            LessEqual(Variable("F1"), Number(32), Location((66, 3))),
        ),
        Link(Field("F1"), Field("F3"), Greater(Variable("F1"), Number(32))),
        Link(
            Field("F2"),
            Field("F3"),
            And(
                Greater(Variable("F1"), Number(32)),
                LessEqual(Variable("F1"), Number(48)),
                location=Location((22, 34)),
            ),
        ),
        Link(Field("F2"), FINAL, Greater(Variable("F1"), Number(48))),
        Link(Field("F3"), FINAL),
    ]
    types = {
        Field("F1"): INTEGER,
        Field(ID("F2", Location((90, 12)))): INTEGER,
        Field("F3"): INTEGER,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:90:12: model: error: unreachable field "F2" in "P::M"\n'
        r"<stdin>:90:12: model: info: path 0 [(]F1 -> F2[)]:\n"
        r'<stdin>:66:3: model: info: unsatisfied "F1 <= 32"\n'
        r'<stdin>:91:13: model: info: unsatisfied "[(]F1 > 32 and F1 <= 48[)] or F1 > 48"\n'
        r'<stdin>:22:34: model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'<stdin>:91:13: model: info: on path: "F2"\n'
        r'<stdin>:66:3: model: info: unsatisfied "F1 <= 32"\n'
        r'<stdin>:22:34: model: info: unsatisfied "F1 > 32 and F1 <= 48"\n'
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "F1"\n'
        r'<stdin>:91:13: model: info: on path: "F2"\n'
        r'<stdin>:66:3: model: info: unsatisfied "F1 <= 32"\n'
        r'model: info: unsatisfied "F1 > 48"'
        r"$",
    )


def test_size_aspect_final() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, size=Number(100, location=Location((4, 12)))),
    ]
    types = {
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
    }
    assert_message_model_error(
        structure, types, '^<stdin>:4:12: model: error: size aspect for final field in "P::M"$'
    )


def test_aggregate_equal_valid_size() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), size=Number(40)),
        Link(
            Field("Magic"),
            FINAL,
            condition=Equal(
                Variable("Magic"),
                Aggregate(Number(1), Number(2), Number(3), Number(4), Number(4)),
            ),
        ),
    ]
    types = {
        Field("Magic"): OPAQUE,
    }
    Message("P::M", structure, types)


def test_aggregate_equal_invalid_size1() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), size=Number(40)),
        Link(
            Field("Magic"),
            FINAL,
            condition=Equal(Variable("Magic"), Aggregate(Number(1), Number(2))),
        ),
    ]
    types = {
        Field("Magic"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "Magic"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Size"\n'
        r'model: info: unsatisfied "Magic\'Size = 40"'
        r"$",
    )


def test_aggregate_equal_invalid_size2() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), size=Number(40)),
        Link(
            Field("Magic"),
            FINAL,
            condition=Equal(Aggregate(Number(1), Number(2)), Variable("Magic")),
        ),
    ]
    types = {
        Field("Magic"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "Magic"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Size"\n'
        r'model: info: unsatisfied "Magic\'Size = 40"'
        r"$",
    )


def test_aggregate_inequal_valid_size() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), size=Number(40)),
        Link(
            Field("Magic"),
            FINAL,
            condition=NotEqual(
                Variable("Magic"),
                Aggregate(Number(1), Number(2), Number(3), Number(4), Number(4)),
            ),
        ),
    ]
    types = {
        Field("Magic"): OPAQUE,
    }
    Message("P::M", structure, types)


def test_aggregate_inequal_invalid_size() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), size=Number(40)),
        Link(
            Field("Magic"),
            FINAL,
            condition=NotEqual(Variable("Magic"), Aggregate(Number(1), Number(2))),
        ),
    ]
    types = {
        Field("Magic"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: contradicting condition in "P::M"\n'
        r'model: info: on path: "Magic"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Size"\n'
        r'model: info: unsatisfied "Magic\'Size = 40"'
        r"$",
    )


def test_aggregate_equal_sequence_valid_size() -> None:
    structure = [
        Link(INITIAL, Field("Magic"), size=Number(16)),
        Link(
            Field("Magic"),
            FINAL,
            condition=NotEqual(Variable("Magic"), Aggregate(Number(1), Number(2))),
        ),
    ]
    types = {
        Field("Magic"): Sequence(
            "P::Arr", Integer("P::Integer", Number(0), Number(127), Number(8))
        ),
    }
    Message("P::M", structure, types)


def test_aggregate_equal_sequence_invalid_size() -> None:
    magic = Field(ID("Magic", Location((3, 5))))
    structure = [
        Link(INITIAL, magic, size=Number(40, location=Location((19, 17)))),
        Link(
            magic,
            FINAL,
            condition=NotEqual(
                Variable("Magic"), Aggregate(Number(1), Number(2)), Location((17, 3))
            ),
        ),
    ]
    types = {
        Field("Magic"): Sequence(
            "P::Arr",
            Integer("P::Integer", Number(0), Number(127), Number(8), location=Location((66, 3))),
        ),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:17:3: model: error: contradicting condition in "P::M"\n'
        r'<stdin>:3:5: model: info: on path: "Magic"\n'
        r'<stdin>:17:3: model: info: unsatisfied "2 [*] Integer\'Size = Magic\'Size"\n'
        r'<stdin>:66:3: model: info: unsatisfied "Integer\'Size = 8"\n'
        r'<stdin>:19:17: model: info: unsatisfied "Magic\'Size = 40"'
        r"$",
    )


def test_aggregate_equal_invalid_size_field() -> None:
    length = Field(ID("Length", Location((2, 5))))
    magic = Field(ID("Magic", Location((3, 5))))

    structure = [
        Link(INITIAL, length),
        Link(length, magic, size=Mul(Number(8), Variable("Length"), location=Location((6, 5)))),
        Link(
            magic,
            FINAL,
            condition=Equal(
                Variable("Magic"), Aggregate(Number(1), Number(2)), location=Location((10, 5))
            ),
        ),
    ]
    types = {
        Field("Length"): Integer(
            "P::Length_Type", Number(10), Number(100), Number(8), Location((5, 10))
        ),
        Field(ID("Magic", Location((17, 3)))): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:10:5: model: error: contradicting condition in "P::M"\n'
        r'<stdin>:2:5: model: info: on path: "Length"\n'
        r'<stdin>:3:5: model: info: on path: "Magic"\n'
        r'<stdin>:6:5: model: info: unsatisfied "Magic\'Size = 8 [*] Length"\n'
        r'<stdin>:10:5: model: info: unsatisfied "2 [*] 8 = Magic\'Size"\n'
        r'<stdin>:5:10: model: info: unsatisfied "Length >= 10"'
        r"$",
    )


def test_no_contradiction_multi() -> None:
    structure = [
        Link(INITIAL, Field("F0")),
        Link(Field("F0"), Field("F1"), condition=Equal(Variable("F0"), Number(1))),
        Link(Field("F0"), Field("F2"), condition=Equal(Variable("F0"), Number(2))),
        Link(Field("F1"), Field("F3")),
        Link(Field("F2"), Field("F3")),
        Link(Field("F3"), Field("F4"), condition=Equal(Variable("F0"), Number(1))),
        Link(Field("F3"), Field("F5"), condition=Equal(Variable("F0"), Number(2))),
        Link(Field("F4"), FINAL),
        Link(Field("F5"), FINAL),
    ]
    types = {
        Field("F0"): INTEGER,
        Field("F1"): INTEGER,
        Field("F2"): INTEGER,
        Field("F3"): INTEGER,
        Field("F4"): INTEGER,
        Field("F5"): INTEGER,
    }
    Message("P::M", structure, types)


def test_discontiguous_optional_fields() -> None:
    structure = [
        Link(INITIAL, Field("Flag")),
        Link(
            Field("Flag"),
            Field("Opt1"),
            condition=Equal(Variable("Flag"), Number(1)),
        ),
        Link(
            Field("Flag"),
            Field("Data"),
            condition=NotEqual(Variable("Flag"), Number(1)),
        ),
        Link(Field("Opt1"), Field("Data")),
        Link(
            Field("Data"),
            Field("Opt2"),
            condition=Equal(Variable("Flag"), Number(1)),
            size=Mul(Variable("Opt1"), Number(8)),
        ),
        Link(
            Field("Data"),
            FINAL,
            condition=NotEqual(Variable("Flag"), Number(1)),
        ),
        Link(
            Field("Opt2"),
            FINAL,
        ),
    ]
    types = {
        Field("Flag"): INTEGER,
        Field("Opt1"): INTEGER,
        Field("Data"): INTEGER,
        Field("Opt2"): OPAQUE,
    }
    Message("P::M", structure, types)


@pytest.mark.parametrize(
    "checksums,condition",
    [
        (
            {
                ID("F3"): [
                    ValueRange(First("F1"), Last("F2")),
                    Variable("F2"),
                    ValueRange(Add(Last("F1"), Number(1)), Sub(First("F3"), Number(1))),
                    ValueRange(First("F3"), Last("F3")),
                ]
            },
            ValidChecksum("F3"),
        ),
        (
            {ID("F2"): [Variable("F1")], ID("F3"): [Variable("F1"), Size("F2")]},
            And(ValidChecksum("F2"), ValidChecksum("F3")),
        ),
        (
            {
                ID("F3"): [
                    ValueRange(First("Message"), Sub(First("F3"), Number(1))),
                    ValueRange(First("F3"), Last("Message")),
                ]
            },
            ValidChecksum("F3"),
        ),
    ],
)
def test_checksum(checksums: abc.Mapping[ID, abc.Sequence[Expr]], condition: Expr) -> None:
    f1 = Field("F1")
    f2 = Field("F2")
    f3 = Field("F3")
    structure = [
        Link(INITIAL, f1),
        Link(f1, f2),
        Link(f2, f3),
        Link(f3, FINAL, condition),
    ]
    types = {f1: INTEGER, f2: INTEGER, f3: INTEGER}
    message = Message("P::M", structure, types, checksums=checksums)
    assert message.checksums == checksums


@pytest.mark.parametrize(
    "checksums,condition,error",
    [
        (
            {ID("X", location=Location((10, 20))): []},
            TRUE,
            r'^<stdin>:10:20: model: error: checksum definition for unknown field "X"\n'
            r'<stdin>:10:20: model: error: no validity check of checksum "X"$',
        ),
        (
            {ID("F3", location=Location((10, 20))): []},
            TRUE,
            r'^<stdin>:10:20: model: error: no validity check of checksum "F3"$',
        ),
        (
            {},
            ValidChecksum(ID("F2", location=Location((20, 30)))),
            r'^<stdin>:20:30: model: error: validity check for undefined checksum "F2"$',
        ),
        (
            {ID("F3"): [First(ID("F2", location=Location((20, 30))))]},
            ValidChecksum("F3"),
            r'^<stdin>:20:30: model: error: unsupported expression "F2\'First"'
            r' in definition of checksum "F3"$',
        ),
        (
            {ID("F3"): [Variable("X", location=Location((20, 30)))]},
            ValidChecksum("F3"),
            r'^<stdin>:20:30: model: error: unknown field "X" referenced'
            r' in definition of checksum "F3"$',
        ),
        (
            {ID("F3"): [ValueRange(First("F2"), Last("F1"), location=Location((20, 30)))]},
            ValidChecksum("F3"),
            r'^<stdin>:20:30: model: error: invalid range "F2\'First .. F1\'Last"'
            r' in definition of checksum "F3"$',
        ),
    ],
)
def test_checksum_error(
    checksums: abc.Mapping[ID, abc.Sequence[Expr]], condition: Expr, error: str
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
    types = {f1: INTEGER, f2: INTEGER, f3: INTEGER}
    assert_message_model_error(structure, types, error, checksums=checksums)


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_field_size() -> None:
    message = Message(
        "P::M",
        [
            Link(INITIAL, Field("A")),
            Link(Field("A"), Field("B"), size=Mul(Size("A"), Number(8))),
            Link(Field("B"), Field("C")),
            Link(Field("C"), FINAL),
        ],
        {Field("A"): INTEGER, Field("B"): OPAQUE, Field("C"): OPAQUE},
        location=Location((30, 10)),
    )

    assert message.field_size(FINAL) == Number(0)
    assert message.field_size(Field("A")) == Number(8)
    assert message.field_size(Field("B")) == Number(64)

    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:10:20: model: error: unable to calculate size of field "C"'
            r' of message "P::M"$'
        ),
    ):
        message.field_size(Field(ID("C", location=Location((10, 20)))))

    with pytest.raises(AssertionError, match='^field "X" not found$'):
        message.field_size(Field("X"))


def test_copy() -> None:
    message = Message(
        "P::M",
        [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
        {Field("F"): INTEGER},
    )
    assert_equal(
        message.copy(identifier="A::B"),
        Message(
            "A::B",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): INTEGER},
        ),
    )
    assert_equal(
        message.copy(
            structure=[Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            types={Field("C"): INTEGER},
            byte_order={Field("C"): ByteOrder.HIGH_ORDER_FIRST},
        ),
        Message(
            "P::M",
            [Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            {Field("C"): INTEGER},
        ),
    )
    assert_equal(
        message.copy(
            structure=[Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            types={Field("C"): INTEGER},
            byte_order={Field("C"): ByteOrder.LOW_ORDER_FIRST},
        ),
        Message(
            "P::M",
            [Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            {Field("C"): INTEGER},
            byte_order=ByteOrder.LOW_ORDER_FIRST,
        ),
    )


def test_proven() -> None:
    message = Message(
        "P::M",
        [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
        {Field("F"): INTEGER},
    )
    assert message.proven() == message


def test_is_possibly_empty() -> None:
    a = Field("A")
    b = Field("B")
    c = Field("C")

    integer = Integer("P::Integer", Number(0), Number(100), Number(8))
    sequence = Sequence("P::Sequence", integer)

    message = Message(
        "P::M",
        [
            Link(INITIAL, a),
            Link(a, c, condition=Less(Variable("A"), Number(10)), size=Variable("A")),
            Link(a, b, condition=Greater(Variable("A"), Number(20)), size=Variable("A")),
            Link(b, c, size=Variable("A")),
            Link(c, FINAL),
        ],
        {a: integer, b: sequence, c: sequence},
    )

    assert not message.is_possibly_empty(a)
    assert not message.is_possibly_empty(b)
    assert message.is_possibly_empty(c)


def test_has_fixed_size() -> None:
    assert NULL_MESSAGE.has_fixed_size
    assert FIXED_SIZE_MESSAGE.has_fixed_size
    assert not TLV_MESSAGE.has_fixed_size
    assert not ETHERNET_FRAME.has_fixed_size
    assert not SEQUENCE_MESSAGE.has_fixed_size


def test_has_implicit_size() -> None:
    assert not NULL_MESSAGE.has_implicit_size
    assert not FIXED_SIZE_MESSAGE.has_implicit_size
    assert not TLV_MESSAGE.has_implicit_size
    assert ETHERNET_FRAME.has_implicit_size
    assert not SEQUENCE_MESSAGE.has_implicit_size


def test_is_definite() -> None:
    assert NULL_MESSAGE.is_definite
    assert FIXED_SIZE_SIMPLE_MESSAGE.is_definite
    assert DEFINITE_MESSAGE.is_definite
    assert not FIXED_SIZE_MESSAGE.is_definite
    assert not TLV_MESSAGE.is_definite
    assert not ETHERNET_FRAME.is_definite
    assert not SEQUENCE_MESSAGE.is_definite


def test_size() -> None:
    assert NULL_MESSAGE.size() == Number(0)
    assert FIXED_SIZE_MESSAGE.size() == Number(200)
    assert DEFINITE_MESSAGE.size() == Add(Mul(Variable("Length"), Number(8)), Number(16))
    assert DEFINITE_MESSAGE.size({Field("Length"): Number(8)}) == Number(80)
    assert TLV_MESSAGE.size({Field("Tag"): Literal("TLV::Msg_Error")}) == Number(8)
    assert TLV_MESSAGE.size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Number(4),
            Field("Value"): Aggregate(*[Number(0)] * 4),
        }
    ) == Number(56)
    assert TLV_MESSAGE.size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Div(Add(Size("Tag"), Size("TLV::Length")), Number(8)),
            Field("Value"): Aggregate(*[Number(0)] * 3),
        }
    ) == Number(48)
    assert TLV_MESSAGE.size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Add(Div(Size("X"), Number(8)), Variable("Y")),
            Field("Value"): Variable("Z"),
        }
    ) == Add(Size("Z"), Number(24))
    assert TLV_MESSAGE.size(
        {
            Field("Tag"): Literal("TLV::Msg_Data"),
            Field("Length"): Div(Size("Msg_Data"), Number(8)),
            Field("Value"): Opaque("Msg_Data"),
        }
    ) == Add(Mul(Div(Size("Msg_Data"), Number(8)), Number(8)), Number(24))
    assert TLV_MESSAGE.size({Field("Tag"): Variable("X"), Field("Length"): Variable("Y")}) == Add(
        Mul(Variable("Y"), Number(8)), Number(24)
    )

    assert ETHERNET_FRAME.size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(46),
            Field("Type_Length"): Number(46),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        }
    ) == Number(480)
    assert ETHERNET_FRAME.size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(0x8100),
            Field("TPID"): Number(0x8100),
            Field("TCI"): Number(0),
            Field("Type_Length"): Number(46),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        }
    ) == Number(512)
    assert ETHERNET_FRAME.size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(1536),
            Field("Type_Length"): Number(1536),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        }
    ) == Number(480)
    assert ETHERNET_FRAME.size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(1536),
            Field("Type_Length"): Number(1536),
            Field("Payload"): Variable("Payload"),
        }
    ) == Add(Size("Payload"), Number(112))

    variable_field_value = Message(
        "Test::Message",
        [
            Link(INITIAL, Field("Length")),
            Link(
                Field("Length"),
                Field("Data"),
                condition=Equal(Variable("Has_Data"), TRUE),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(
                Field("Length"),
                Field("Data"),
                condition=Equal(Variable("Has_Data"), FALSE),
                size=Number(0),
            ),
            Link(
                Field("Data"),
                FINAL,
            ),
        ],
        {
            Field("Has_Data"): BOOLEAN,
            Field("Length"): TLV_LENGTH,
            Field("Data"): OPAQUE,
        },
    )
    assert variable_field_value.size(
        {
            Field("Has_Data"): Variable("X"),
            Field("Length"): Variable("Y"),
            Field("Data"): Selected(Variable("M"), "F"),
        }
    ) == Add(
        IfExpr(
            [
                (
                    Or(Equal(Variable("X"), FALSE), Equal(Variable("X"), TRUE)),
                    Size(Selected(Variable("M"), "F")),
                )
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
        }
    ) == Add(
        IfExpr(
            [(Or(Equal(Variable("X"), FALSE), Equal(Variable("X"), TRUE)), Size(Variable("Z")))],
            Number(0),
        ),
        Number(16),
    )

    optional_overlayed_field = Message(
        "Test::Message",
        [
            Link(INITIAL, Field("A")),
            Link(
                Field("A"),
                Field("B"),
                condition=Equal(Variable("A"), Number(0)),
                first=First(Variable("A")),
            ),
            Link(
                Field("A"),
                Field("B"),
                condition=Greater(Variable("A"), Number(0)),
            ),
            Link(
                Field("B"),
                FINAL,
            ),
        ],
        {
            Field("A"): TLV_LENGTH,
            Field("B"): TLV_LENGTH,
        },
    )
    assert optional_overlayed_field.size(
        {
            Field("A"): Number(0),
            Field("B"): Number(0),
        }
    ) == Number(16)
    assert optional_overlayed_field.size(
        {
            Field("A"): Number(1),
            Field("B"): Number(2),
        }
    ) == Number(32)
    assert optional_overlayed_field.size() == Add(
        IfExpr(
            [
                (
                    And(Greater(Variable("A"), Number(0)), NotEqual(Variable("A"), Number(0))),
                    Number(16),
                )
            ],
            Number(0),
        ),
        Number(16),
    )

    path_dependent_fields = Message(
        "Test::Message",
        [
            Link(INITIAL, Field("A")),
            Link(
                Field("A"),
                Field("B"),
                condition=Equal(Variable("A"), Number(0)),
            ),
            Link(
                Field("A"),
                Field("C"),
                condition=Greater(Variable("A"), Number(0)),
            ),
            Link(
                Field("B"),
                FINAL,
            ),
            Link(
                Field("C"),
                FINAL,
            ),
        ],
        {
            Field("A"): TLV_LENGTH,
            Field("B"): TLV_LENGTH,
            Field("C"): TLV_LENGTH,
        },
    )
    assert path_dependent_fields.size({Field("A"): Variable("X"), Field("B"): Number(0)}) == Number(
        32
    )
    assert path_dependent_fields.size({Field("A"): Variable("X")}) == Add(
        IfExpr(
            [(Or(Equal(Variable("X"), Number(0)), Greater(Variable("X"), Number(0))), Number(16))],
            Number(0),
        ),
        Number(16),
    )


def test_size_subpath() -> None:
    assert NULL_MESSAGE.size({}, subpath=True) == Number(0)

    assert FIXED_SIZE_MESSAGE.size(
        {
            Field("Message_Type"): Literal("Universal::MT_Data"),
        },
        subpath=True,
    ) == Number(8)
    assert FIXED_SIZE_MESSAGE.size(
        {
            Field("Data"): Aggregate(*[Number(0)] * 4),
        },
        subpath=True,
    ) == Number(32)
    assert FIXED_SIZE_MESSAGE.size(
        {
            Field("Message_Type"): Literal("Universal::MT_Data"),
            Field("Data"): Aggregate(*[Number(0)] * 4),
        },
        subpath=True,
    ) == Number(40)

    assert DEFINITE_MESSAGE.size(
        {
            Field("Length"): Number(8),
        },
        subpath=True,
    ) == Number(16)
    assert DEFINITE_MESSAGE.size(
        {
            Field("Data"): Variable("D"),
        },
        subpath=True,
    ) == Size("D")
    assert DEFINITE_MESSAGE.size(
        {
            Field("Length"): Variable("L"),
            Field("Data"): Variable("D"),
        },
        subpath=True,
    ) == Add(Size("D"), Number(16))

    assert TLV_MESSAGE.size(
        {
            Field("Tag"): Variable("T"),
        },
        subpath=True,
    ) == Number(8)
    assert TLV_MESSAGE.size(
        {
            Field("Tag"): Variable("T"),
            Field("Length"): Variable("L"),
            Field("Value"): Aggregate(*[Number(0)] * 4),
        },
        subpath=True,
    ) == Number(56)
    assert TLV_MESSAGE.size(
        {
            Field("Tag"): Variable("T"),
            Field("Length"): Variable("L"),
        },
        subpath=True,
    ) == Number(24)
    assert TLV_MESSAGE.size(
        {
            Field("Length"): Div(Add(Size("Tag"), Size("TLV::Length")), Number(8)),
            Field("Value"): Aggregate(*[Number(0)] * 3),
        },
        subpath=True,
    ) == Number(40)
    assert TLV_MESSAGE.size(
        {
            Field("Length"): Add(Div(Size("X"), Number(8)), Variable("Y")),
            Field("Value"): Variable("Z"),
        },
        subpath=True,
    ) == Add(Size("Z"), Number(16))
    assert TLV_MESSAGE.size(
        {
            Field("Length"): Div(Size("Msg_Data"), Number(8)),
            Field("Value"): Opaque("Msg_Data"),
        },
        subpath=True,
    ) == Add(Mul(Div(Size("Msg_Data"), Number(8)), Number(8)), Number(16))

    assert ETHERNET_FRAME.size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(46),
            Field("Type_Length"): Number(46),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
        subpath=True,
    ) == Number(480)
    assert ETHERNET_FRAME.size(
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
    assert ETHERNET_FRAME.size(
        {
            Field("Destination"): Number(0),
            Field("Source"): Number(0),
            Field("Type_Length_TPID"): Number(1536),
            Field("Type_Length"): Number(1536),
            Field("Payload"): Aggregate(*[Number(0)] * 46),
        },
        subpath=True,
    ) == Number(480)
    assert ETHERNET_FRAME.size(
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
        "Test::Message",
        [
            Link(INITIAL, Field("Length")),
            Link(
                Field("Length"),
                Field("Data"),
                condition=Equal(Variable("Has_Data"), TRUE),
                size=Mul(Variable("Length"), Number(8)),
            ),
            Link(
                Field("Length"),
                Field("Data"),
                condition=Equal(Variable("Has_Data"), FALSE),
                size=Number(0),
            ),
            Link(
                Field("Data"),
                FINAL,
            ),
        ],
        {
            Field("Has_Data"): BOOLEAN,
            Field("Length"): TLV_LENGTH,
            Field("Data"): OPAQUE,
        },
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
                        Equal(Selected(Variable("X"), "Has_Data"), FALSE),
                        Equal(Selected(Variable("X"), "Has_Data"), TRUE),
                    ),
                    Size(Selected(Variable("M"), "F")),
                )
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
                        Equal(Selected(Variable("X"), "Has_Data"), FALSE),
                        Equal(Selected(Variable("X"), "Has_Data"), TRUE),
                    ),
                    Size(Variable("Z")),
                )
            ],
            Number(0),
        ),
        Number(16),
    )

    optional_overlayed_field = Message(
        "Test::Message",
        [
            Link(INITIAL, Field("A")),
            Link(
                Field("A"),
                Field("B"),
                condition=Equal(Variable("A"), Number(0)),
                first=First(Variable("A")),
            ),
            Link(
                Field("A"),
                Field("B"),
                condition=Greater(Variable("A"), Number(0)),
            ),
            Link(
                Field("B"),
                FINAL,
            ),
        ],
        {
            Field("A"): TLV_LENGTH,
            Field("B"): TLV_LENGTH,
        },
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
            )
        ],
        Number(0),
    )

    path_dependent_fields = Message(
        "Test::Message",
        [
            Link(INITIAL, Field("A")),
            Link(
                Field("A"),
                Field("B"),
                condition=Equal(Variable("A"), Number(0)),
            ),
            Link(
                Field("A"),
                Field("C"),
                condition=Greater(Variable("A"), Number(0)),
            ),
            Link(
                Field("B"),
                FINAL,
            ),
            Link(
                Field("C"),
                FINAL,
            ),
        ],
        {
            Field("A"): TLV_LENGTH,
            Field("B"): TLV_LENGTH,
            Field("C"): TLV_LENGTH,
        },
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
            r'model: error: unable to calculate size of invalid subpath "Tag -> Value"'
            r' of message "TLV::Message"'
            r"$"
        ),
    ):
        TLV_MESSAGE.size(
            {
                Field("Tag"): Variable("T"),
                Field("Value"): Variable("V"),
            },
            subpath=True,
        )


def test_max_size() -> None:
    assert NULL_MESSAGE.max_size() == Number(0)
    assert FIXED_SIZE_MESSAGE.max_size() == Number(8 + 3 * 64)
    assert TLV_MESSAGE.max_size() == Number(8 + 16 + (2**16 - 1) * 8)
    assert SEQUENCE_MESSAGE.max_size() == Number(8 + (2**8 - 1) * 8 + 2 * 16)


def test_max_size_error() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r"^model: error: unable to calculate maximum size of message with implicit size$",
    ):
        ETHERNET_FRAME.max_size()


def test_max_field_sizes() -> None:
    assert NULL_MESSAGE.max_field_sizes() == {}
    assert FIXED_SIZE_MESSAGE.max_field_sizes() == {
        Field("Message_Type"): Number(8),
        Field("Data"): Number(64),
        Field("Values"): Number(64),
        Field("Options"): Number(64),
    }
    assert TLV_MESSAGE.max_field_sizes() == {
        Field("Tag"): Number(8),
        Field("Length"): Number(16),
        Field("Value"): Number((2**16 - 1) * 8),
    }


def test_max_field_sizes_error() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^model: error: unable to calculate maximum field sizes of message with implicit size$"
        ),
    ):
        ETHERNET_FRAME.max_field_sizes()


def test_derived_message_incorrect_base_name() -> None:
    with pytest.raises(
        RecordFluxError,
        match='^<stdin>:40:8: model: error: invalid format for identifier "A::B::C"$',
    ):
        DerivedMessage("P::M", Message(ID("A::B::C", location=Location((40, 8))), [], {}))


def test_derived_message_proven() -> None:
    message = DerivedMessage(
        "P::M",
        Message(
            "X::M",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): INTEGER},
        ),
    )
    assert message.proven() == message


def test_prefixed_message() -> None:
    assert_equal(
        UnprovenMessage(
            "P::M",
            [
                Link(INITIAL, Field("F1")),
                Link(
                    Field("F1"),
                    Field("F2"),
                    LessEqual(Variable("F1"), Number(100)),
                    first=First("F1"),
                ),
                Link(
                    Field("F1"),
                    Field("F3"),
                    GreaterEqual(Variable("F1"), Number(200)),
                    first=First("F1"),
                ),
                Link(Field("F2"), FINAL, Equal(Variable("F2"), Variable("True"))),
                Link(Field("F3"), Field("F4"), size=Variable("F3")),
                Link(Field("F4"), FINAL),
            ],
            {
                Field("F1"): deepcopy(INTEGER),
                Field("F2"): deepcopy(BOOLEAN),
                Field("F3"): deepcopy(INTEGER),
                Field("F4"): OPAQUE,
            },
        ).prefixed("X_"),
        UnprovenMessage(
            "P::M",
            [
                Link(INITIAL, Field("X_F1")),
                Link(
                    Field("X_F1"),
                    Field("X_F2"),
                    LessEqual(Variable("X_F1"), Number(100)),
                    first=First("X_F1"),
                ),
                Link(
                    Field("X_F1"),
                    Field("X_F3"),
                    GreaterEqual(Variable("X_F1"), Number(200)),
                    first=First("X_F1"),
                ),
                Link(Field("X_F2"), FINAL, Equal(Variable("X_F2"), Variable("True"))),
                Link(Field("X_F3"), Field("X_F4"), size=Variable("X_F3")),
                Link(Field("X_F4"), FINAL),
            ],
            {
                Field("X_F1"): deepcopy(INTEGER),
                Field("X_F2"): deepcopy(BOOLEAN),
                Field("X_F3"): deepcopy(INTEGER),
                Field("X_F4"): OPAQUE,
            },
        ),
    )


def test_merge_message_simple() -> None:
    assert deepcopy(M_SMPL_REF).merged() == UnprovenMessage(
        "P::Smpl_Ref",
        [
            Link(INITIAL, Field("NR_F1"), size=Number(16)),
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
        ],
        {
            Field("NR_F1"): OPAQUE,
            Field("NR_F2"): deepcopy(INTEGER),
            Field("NR_F3"): deepcopy(ENUMERATION),
            Field("NR_F4"): deepcopy(INTEGER),
        },
    )


def test_merge_message_complex() -> None:
    assert_equal(
        deepcopy(M_CMPLX_REF).merged(),
        UnprovenMessage(
            "P::Cmplx_Ref",
            [
                Link(INITIAL, Field("F1")),
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
                Link(
                    Field("NR_F3"),
                    Field("F5"),
                    And(
                        LessEqual(Variable("F1"), Number(100)),
                        Equal(Variable("NR_F3"), Variable("P::One")),
                    ),
                ),
                Link(Field("NR_F4"), Field("F5"), LessEqual(Variable("F1"), Number(100))),
                Link(
                    Field("NR_F3"),
                    Field("F6"),
                    And(
                        GreaterEqual(Variable("F1"), Number(200)),
                        Equal(Variable("NR_F3"), Variable("P::One")),
                    ),
                ),
                Link(Field("NR_F4"), Field("F6"), GreaterEqual(Variable("F1"), Number(200))),
                Link(Field("F5"), FINAL),
                Link(Field("F6"), FINAL),
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
            ],
            {
                Field("F1"): deepcopy(INTEGER),
                Field("F2"): deepcopy(INTEGER),
                Field("F3"): deepcopy(INTEGER),
                Field("NR_F1"): OPAQUE,
                Field("NR_F2"): deepcopy(INTEGER),
                Field("NR_F3"): deepcopy(ENUMERATION),
                Field("NR_F4"): deepcopy(INTEGER),
                Field("F5"): deepcopy(INTEGER),
                Field("F6"): deepcopy(INTEGER),
            },
        ),
    )


def test_merge_message_recursive() -> None:
    assert_equal(
        deepcopy(M_DBL_REF).merged(),
        UnprovenMessage(
            "P::Dbl_Ref",
            [
                Link(INITIAL, Field("SR_NR_F1"), size=Number(16)),
                Link(
                    Field("SR_NR_F3"),
                    Field("NR_F1"),
                    Equal(Variable("SR_NR_F3"), Variable("P::One")),
                    size=Number(16),
                ),
                Link(Field("SR_NR_F4"), Field("NR_F1"), size=Number(16)),
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
            ],
            {
                Field("SR_NR_F1"): OPAQUE,
                Field("SR_NR_F2"): deepcopy(INTEGER),
                Field("SR_NR_F3"): deepcopy(ENUMERATION),
                Field("SR_NR_F4"): deepcopy(INTEGER),
                Field("NR_F1"): OPAQUE,
                Field("NR_F2"): deepcopy(INTEGER),
                Field("NR_F3"): deepcopy(ENUMERATION),
                Field("NR_F4"): deepcopy(INTEGER),
            },
        ),
    )


def test_merge_message_simple_derived() -> None:
    assert_equal(
        deepcopy(M_SMPL_REF_DERI).merged(),
        UnprovenDerivedMessage(
            "P::Smpl_Ref_Deri",
            M_SMPL_REF,
            [
                Link(INITIAL, Field("NR_F1"), size=Number(16)),
                Link(Field("NR_F3"), FINAL, Equal(Variable("NR_F3"), Variable("P::One"))),
                Link(Field("NR_F4"), FINAL),
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
            ],
            {
                Field("NR_F1"): OPAQUE,
                Field("NR_F2"): deepcopy(INTEGER),
                Field("NR_F3"): deepcopy(ENUMERATION),
                Field("NR_F4"): deepcopy(INTEGER),
            },
            byte_order=ByteOrder.HIGH_ORDER_FIRST,
        ),
    )


def test_merge_byte_order() -> None:
    inner_msg = UnprovenMessage(
        "P::Merge_Test_Byte_Order",
        [Link(INITIAL, Field("F1")), Link(Field("F1"), Field("F2")), Link(Field("F2"), FINAL)],
        {Field("F1"): INTEGER, Field("F2"): ENUMERATION},
        byte_order=ByteOrder.LOW_ORDER_FIRST,
    )
    outer_msg = UnprovenMessage(
        "P::Outer_Msg",
        [
            Link(INITIAL, Field("NR")),
            Link(Field("NR"), FINAL),
        ],
        {Field("NR"): inner_msg},
    )
    assert_equal(
        outer_msg.merged(),
        UnprovenMessage(
            "P::Outer_Msg",
            [
                Link(INITIAL, Field("NR_F1")),
                Link(Field("NR_F1"), Field("NR_F2")),
                Link(Field("NR_F2"), FINAL),
            ],
            {Field("NR_F1"): INTEGER, Field("NR_F2"): ENUMERATION},
            byte_order={
                Field("NR_F1"): ByteOrder.LOW_ORDER_FIRST,
                Field("NR_F2"): ByteOrder.LOW_ORDER_FIRST,
            },
        ),
    )


def test_merge_message_constrained() -> None:
    m1 = UnprovenMessage(
        "P::M1",
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F3"), Equal(Variable("F1"), Variable("True"))),
            Link(Field("F1"), Field("F2")),
            Link(Field("F2"), FINAL, Equal(Variable("F1"), Variable("False"))),
            Link(Field("F3"), FINAL),
        ],
        {Field("F1"): BOOLEAN, Field("F2"): BOOLEAN, Field("F3"): BOOLEAN},
    )
    m2 = UnprovenMessage(
        "P::M2",
        [
            Link(INITIAL, Field("F4")),
            Link(
                Field("F4"),
                FINAL,
                And(
                    Equal(Variable("F4_F1"), Variable("True")),
                    Equal(Variable("F4_F3"), Variable("False")),
                ),
            ),
        ],
        {Field("F4"): m1},
    )
    expected = UnprovenMessage(
        "P::M2",
        [
            Link(
                INITIAL,
                Field("F4_F1"),
            ),
            Link(Field("F4_F1"), Field("F4_F3"), Equal(Variable("F4_F1"), Variable("True"))),
            Link(
                Field("F4_F3"),
                FINAL,
                And(
                    Equal(Variable("F4_F1"), Variable("True")),
                    Equal(Variable("F4_F3"), Variable("False")),
                ),
            ),
        ],
        {Field("F4_F1"): BOOLEAN, Field("F4_F3"): BOOLEAN},
    )
    merged = m2.merged()
    assert merged == expected


def test_merge_message_constrained_empty() -> None:
    m1 = UnprovenMessage(
        "P::M1",
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), Equal(Variable("F1"), Variable("True"))),
            Link(Field("F1"), FINAL, Equal(Variable("F1"), Variable("False"))),
            Link(Field("F2"), FINAL, Equal(Variable("F2"), Variable("True"))),
        ],
        {Field("F1"): BOOLEAN, Field("F2"): BOOLEAN},
    )
    m2 = UnprovenMessage(
        "P::M2",
        [
            Link(INITIAL, Field("F3")),
            Link(
                Field("F3"),
                FINAL,
                And(
                    Equal(Variable("F3_F1"), Variable("True")),
                    Equal(Variable("F3_F2"), Variable("False")),
                ),
            ),
        ],
        {Field("F3"): m1},
    )
    with pytest.raises(
        RecordFluxError,
        match=r'^model: error: empty message type when merging field "F3"$',
    ):
        m2.merged()


def test_merge_message_error_name_conflict() -> None:
    m2_f2 = Field(ID("F2", Location((10, 5))))

    m2 = UnprovenMessage(
        "P::M2",
        [Link(INITIAL, m2_f2), Link(m2_f2, FINAL)],
        {m2_f2: INTEGER},
        location=Location((15, 3)),
    )

    m1_f1 = Field(ID("F1", Location((20, 8))))
    m1_f1_f2 = Field(ID("F1_F2", Location((30, 5))))

    m1 = UnprovenMessage(
        "P::M1",
        [Link(INITIAL, m1_f1), Link(m1_f1, m1_f1_f2), Link(m1_f1_f2, FINAL)],
        {m1_f1: m2, m1_f1_f2: INTEGER},
        location=Location((2, 9)),
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:30:5: model: error: name conflict for "F1_F2" in "P::M1"\n'
            r'<stdin>:15:3: model: info: when merging message "P::M2"\n'
            r'<stdin>:20:8: model: info: into field "F1"$'
        ),
    ):
        m1.merged()


def test_merge_message_parameterized() -> None:
    assert_equal(
        deepcopy(M_PARAM_PARAM_REF)
        .merged({ID("P::Param_No_Ref"): {ID("P1"): Variable("P2")}})
        .proven(),
        UnprovenMessage(
            "P::Param_Param_Ref",
            [
                Link(INITIAL, Field("PNR_F1")),
                Link(Field("PNR_F1"), Field("PNR_F2"), condition=Equal(Variable("P2"), Number(1))),
                Link(Field("PNR_F1"), FINAL, condition=Equal(Variable("P2"), Number(2))),
                Link(Field("PNR_F2"), FINAL),
            ],
            {
                Field("P2"): INTEGER,
                Field("PNR_F1"): INTEGER,
                Field("PNR_F2"): INTEGER,
            },
        ).proven(),
    )


def test_merge_message_with_message_last_attribute() -> None:
    inner = UnprovenMessage(
        "P::I",
        [
            Link(INITIAL, Field("I1")),
            Link(
                Field("I1"),
                Field("I2"),
                condition=Less(Variable("I1"), Number(128)),
                size=Sub(Last(ID("Message", location=Location((5, 10)))), Last("I1")),
                first=First("Message"),
            ),
            Link(
                Field("I1"),
                Field("I2"),
                condition=GreaterEqual(Variable("I1"), Number(128)),
                size=Sub(Last(ID("Message", location=Location((6, 10)))), Last("I1")),
            ),
            Link(Field("I2"), FINAL),
        ],
        {Field("I1"): INTEGER, Field("I2"): OPAQUE},
    )

    inner.error.propagate()

    valid_outer = (
        UnprovenMessage(
            "P::O",
            [
                Link(INITIAL, Field("O1")),
                Link(Field("O1"), Field("O2")),
                Link(Field("O2"), FINAL),
            ],
            {Field("O1"): INTEGER, Field("O2"): inner},
        )
        .merged()
        .proven()
    )

    assert_equal(
        valid_outer,
        Message(
            "P::O",
            [
                Link(INITIAL, Field("O1")),
                Link(Field("O1"), Field("O2_I1")),
                Link(
                    Field("O2_I1"),
                    Field("O2_I2"),
                    condition=Less(Variable("O2_I1"), Number(128)),
                    size=Add(Last("Message"), -Last("O2_I1")),
                    first=First("O2_I1"),
                ),
                Link(
                    Field("O2_I1"),
                    Field("O2_I2"),
                    condition=GreaterEqual(Variable("O2_I1"), Number(128)),
                    size=Add(Last("Message"), -Last("O2_I1")),
                ),
                Link(Field("O2_I2"), FINAL),
            ],
            {Field("O1"): INTEGER, Field("O2_I1"): INTEGER, Field("O2_I2"): OPAQUE},
        ),
    )
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "<stdin>:2:10: model: error: messages with implicit size may only be used for"
            " last fields\n"
            '<stdin>:5:10: model: info: message field with implicit size in "P::I"\n'
            '<stdin>:6:10: model: info: message field with implicit size in "P::I"'
            "$"
        ),
    ):
        o1 = Field(ID("O1", location=Location((2, 10))))
        valid_outer = (
            UnprovenMessage(
                "P::O",
                [
                    Link(INITIAL, o1),
                    Link(o1, Field("O2")),
                    Link(Field("O2"), FINAL),
                ],
                {o1: inner, Field("O2"): INTEGER},
            )
            .merged()
            .proven()
        )


def test_merge_message_with_message_size_attribute() -> None:
    inner = UnprovenMessage(
        "P::I",
        [
            Link(
                INITIAL,
                Field("I"),
                size=Size("Message"),
            ),
            Link(
                Field("I"),
                FINAL,
                condition=Equal(Size("Message"), Number(128)),
            ),
        ],
        {Field("I"): OPAQUE},
    )

    inner.error.propagate()

    outer = UnprovenMessage(
        "P::O",
        [
            Link(INITIAL, Field("O1")),
            Link(Field("O1"), Field("O2"), condition=Less(Variable("O1"), Number(100))),
            Link(
                Field("O1"),
                Field("O3"),
                condition=GreaterEqual(Variable("O1"), Number(100)),
            ),
            Link(Field("O2"), Field("A")),
            Link(Field("O3"), Field("B")),
            Link(Field("A"), FINAL),
            Link(Field("B"), FINAL),
        ],
        {
            Field("O1"): INTEGER,
            Field("O2"): INTEGER,
            Field("O3"): INTEGER,
            Field("A"): inner,
            Field("B"): inner,
        },
    )

    expected = Message(
        "P::O",
        [
            Link(INITIAL, Field("O1")),
            Link(Field("O1"), Field("O2"), condition=Less(Variable("O1"), Number(100))),
            Link(
                Field("O1"),
                Field("O3"),
                condition=GreaterEqual(Variable("O1"), Number(100)),
            ),
            Link(
                Field("O2"),
                Field("A_I"),
                size=Sub(Last("Message"), Last("O2")),
            ),
            Link(
                Field("A_I"),
                FINAL,
                condition=Equal(Sub(Last("Message"), Last("O2")), Number(128)),
            ),
            Link(
                Field("O3"),
                Field("B_I"),
                size=Sub(Last("Message"), Last("O3")),
            ),
            Link(
                Field("B_I"),
                FINAL,
                condition=Equal(Sub(Last("Message"), Last("O3")), Number(128)),
            ),
        ],
        {
            Field("O1"): INTEGER,
            Field("O2"): INTEGER,
            Field("O3"): INTEGER,
            Field("A_I"): OPAQUE,
            Field("B_I"): OPAQUE,
        },
    )

    assert outer.merged().proven() == expected


def test_merge_message_type_message_size_attribute_in_outer_message() -> None:
    inner = UnprovenMessage(
        "P::I",
        [
            Link(
                INITIAL,
                Field("I"),
                size=Number(128),
            ),
            Link(
                Field("I"),
                FINAL,
            ),
        ],
        {Field("I"): OPAQUE},
    )

    inner.error.propagate()

    outer = UnprovenMessage(
        "P::O",
        [
            Link(INITIAL, Field("O1")),
            Link(Field("O1"), Field("O2"), size=Sub(Last("Message"), Last("O1"))),
            Link(Field("O2"), FINAL),
        ],
        {
            Field("O1"): inner,
            Field("O2"): OPAQUE,
        },
    )

    expected = Message(
        "P::O",
        [
            Link(
                INITIAL,
                Field("O1_I"),
                size=Number(128),
            ),
            Link(
                Field("O1_I"),
                Field("O2"),
                size=Sub(Last("Message"), Last("O1_I")),
            ),
            Link(Field("O2"), FINAL),
        ],
        {
            Field("O1_I"): OPAQUE,
            Field("O2"): OPAQUE,
        },
    )

    assert outer.merged().proven() == expected


def test_paths() -> None:
    message = Message(
        "P::M",
        [
            Link(INITIAL, Field("L")),
            Link(Field("L"), Field("O"), condition=Greater(Variable("L"), Number(100))),
            Link(Field("L"), Field("O"), condition=LessEqual(Variable("L"), Number(100))),
            Link(Field("O"), FINAL),
        ],
        {Field("L"): INTEGER, Field("O"): INTEGER},
    )
    assert message.paths(Field("O")) == {
        (
            Link(INITIAL, Field("L")),
            Link(Field("L"), Field("O"), condition=Greater(Variable("L"), Number(100))),
        ),
        (
            Link(INITIAL, Field("L")),
            Link(Field("L"), Field("O"), condition=LessEqual(Variable("L"), Number(100))),
        ),
    }


def test_normalization() -> None:
    assert TLV_MESSAGE.structure == sorted(
        [
            Link(INITIAL, Field("Tag")),
            Link(Field("Tag"), Field("Length"), Equal(Variable("Tag"), Variable("TLV::Msg_Data"))),
            Link(Field("Tag"), FINAL, Equal(Variable("Tag"), Variable("TLV::Msg_Error"))),
            Link(Field("Length"), Field("Value"), size=Mul(Variable("Length"), Number(8))),
            Link(Field("Value"), FINAL),
        ]
    )


def test_set_refinements() -> None:
    message = MESSAGE.copy()

    assert message.type_.refinements == []

    message.set_refinements([REFINEMENT])

    assert message.type_.refinements == [
        rty.Refinement(
            "F",
            rty.Message(
                "P::M", {("F",)}, {}, {ID("F"): rty.OPAQUE}, refinements=[], is_definite=True
            ),
            "In_Message",
        )
    ]


def test_set_refinements_error() -> None:
    message = MESSAGE.copy()
    with pytest.raises(
        FatalError, match=r"^model: error: setting refinements for different message$"
    ):
        message.set_refinements(
            [REFINEMENT, Refinement("In_Message", TLV_MESSAGE, Field("Value"), MESSAGE)]
        )


def test_message_dependencies() -> None:
    assert TLV_MESSAGE.dependencies == [
        TLV_TAG,
        TLV_LENGTH,
        OPAQUE,
        TLV_MESSAGE,
    ]
    assert SEQUENCE_MESSAGES_MESSAGE.dependencies == [
        SEQUENCE_LENGTH,
        OPAQUE,
        SEQUENCE_INNER_MESSAGE,
        SEQUENCE_INNER_MESSAGES,
        SEQUENCE_MESSAGES_MESSAGE,
    ]


def test_message_str() -> None:
    message = Message(
        "P::M",
        [
            Link(INITIAL, Field("L")),
            Link(
                Field("L"),
                Field("O"),
                condition=And(Greater(Variable("L"), Number(100)), Equal(Variable("A"), TRUE)),
            ),
            Link(Field("L"), Field("P"), condition=LessEqual(Variable("L"), Number(100))),
            Link(Field("P"), FINAL),
            Link(Field("O"), FINAL),
        ],
        {
            Field("A"): BOOLEAN,
            Field("L"): INTEGER,
            Field("O"): INTEGER,
            Field("P"): INTEGER,
        },
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
               end message"""
        ),
    )


def test_refinement_dependencies() -> None:
    assert UNIVERSAL_REFINEMENT.direct_dependencies == [
        UNIVERSAL_MESSAGE,
        UNIVERSAL_OPTION,
        UNIVERSAL_REFINEMENT,
    ]
    assert UNIVERSAL_REFINEMENT.dependencies == [
        UNIVERSAL_MESSAGE_TYPE,
        UNIVERSAL_LENGTH,
        OPAQUE,
        UNIVERSAL_OPTION_TYPE,
        UNIVERSAL_OPTION_TYPES,
        UNIVERSAL_OPTION,
        UNIVERSAL_OPTIONS,
        UNIVERSAL_VALUE,
        UNIVERSAL_VALUES,
        UNIVERSAL_MESSAGE,
        UNIVERSAL_REFINEMENT,
    ]


def test_refinement_invalid_package() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:22:10: model: error: unexpected format of package name "A::B"$',
    ):
        Refinement(ID("A::B", Location((22, 10))), ETHERNET_FRAME, Field("Payload"), ETHERNET_FRAME)


def test_refinement_invalid_field_type() -> None:
    x = Field(ID("X", Location((20, 10))))

    message = Message("P::M", [Link(INITIAL, x), Link(x, FINAL)], {x: INTEGER})

    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:33:22: model: error: invalid type of field "X" in refinement of "P::M"\n'
            r"<stdin>:20:10: model: info: expected field of type Opaque$"
        ),
    ):
        Refinement("P", message, Field(ID("X", Location((33, 22)))), message)


def test_refinement_invalid_field() -> None:
    message = Message("P::M", [], {})

    with pytest.raises(
        RecordFluxError,
        match=(r'^<stdin>:33:22: model: error: invalid field "X" in refinement of "P::M"$'),
    ):
        Refinement("P", message, Field(ID("X", Location((33, 22)))), message)


def test_refinement_invalid_condition() -> None:
    x = Field("X")

    message = Message("P::M", [Link(INITIAL, x, size=Number(8)), Link(x, FINAL)], {x: OPAQUE})

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:20: model: error: unknown field or literal "Y" in refinement condition'
            r' of "P::M"'
            r"$"
        ),
    ):
        Refinement(
            "P",
            message,
            Field("X"),
            message,
            Equal(Variable("Y", location=Location((10, 20))), Number(1)),
        )


def test_refinement_invalid_condition_unqualified_literal() -> None:
    e = Enumeration(
        "P2::E",
        [("E1", Number(1)), ("E2", Number(2)), ("E3", Number(3))],
        Number(8),
        always_valid=False,
    )

    x = Field("X")
    y = Field("Y")

    message = Message(
        "P::M",
        [Link(INITIAL, x), Link(x, y, size=Number(8)), Link(y, FINAL)],
        {x: e, y: OPAQUE},
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:20: model: error: unknown field or literal "E1" in refinement condition'
            r' of "P::M"'
            r"$"
        ),
    ):
        Refinement(
            "P",
            message,
            y,
            message,
            Equal(Variable("X"), Variable("E1", location=Location((10, 20)))),
        )


def test_boolean_variable_as_condition() -> None:
    Message(
        "P::M",
        [
            Link(INITIAL, Field("Tag_1")),
            Link(Field("Tag_1"), Field("Tag_2"), condition=Variable("Has_Tag")),
            Link(Field("Tag_2"), FINAL),
        ],
        {
            Field("Tag_1"): INTEGER,
            Field("Tag_2"): INTEGER,
            Field("Has_Tag"): BOOLEAN,
        },
    )


@pytest.mark.parametrize(
    "message, condition",
    [
        (
            Message(
                "P::M",
                [
                    Link(INITIAL, Field("Tag")),
                    Link(Field("Tag"), Field("Value")),
                    Link(Field("Value"), FINAL),
                ],
                {
                    Field("Tag"): TLV_TAG,
                    Field("Value"): OPAQUE,
                },
            ),
            Or(
                Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
            ),
        ),
        (
            Message(
                "P::M",
                [
                    Link(INITIAL, Field("Tag")),
                    Link(
                        Field("Tag"),
                        Field("Value"),
                        condition=Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                    ),
                    Link(Field("Value"), FINAL),
                ],
                {
                    Field("Tag"): TLV_TAG,
                    Field("Value"): OPAQUE,
                },
            ),
            Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
        ),
    ],
)
def test_always_true_refinement(message: Message, condition: Expr) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf'^<stdin>:10:20: model: error: condition "{condition}"'
            ' in refinement of "P::M" is always true$'
        ),
    ):
        Refinement(
            "In_Message",
            message,
            Field(ID("Value", location=Location((10, 20)))),
            MESSAGE,
            condition,
        )


@pytest.mark.parametrize(
    "message, condition",
    [
        (
            Message(
                "P::M",
                [
                    Link(INITIAL, Field("Tag")),
                    Link(Field("Tag"), Field("Value")),
                    Link(Field("Value"), FINAL),
                ],
                {
                    Field("Tag"): TLV_TAG,
                    Field("Value"): OPAQUE,
                },
            ),
            And(
                Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
            ),
        ),
        (
            Message(
                "P::M",
                [
                    Link(INITIAL, Field("Tag")),
                    Link(
                        Field("Tag"),
                        Field("Value"),
                        condition=Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                    ),
                    Link(Field("Value"), FINAL),
                ],
                {
                    Field("Tag"): TLV_TAG,
                    Field("Value"): OPAQUE,
                },
            ),
            Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
        ),
    ],
)
def test_always_false_refinement(message: Message, condition: Expr) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf'^<stdin>:10:20: model: error: condition "{condition}"'
            ' in refinement of "P::M" is always false$'
        ),
    ):
        Refinement(
            "In_Message",
            message,
            Field(ID("Value", location=Location((10, 20)))),
            MESSAGE,
            condition,
        )


@pytest.mark.parametrize(
    "structure, types",
    [
        (
            [
                Link(INITIAL, Field("Tag")),
                Link(
                    Field(ID("Tag", location=Location((10, 20)))),
                    FINAL,
                    condition=Or(
                        Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                        Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
                    ),
                ),
            ],
            {
                Field("Tag"): TLV_TAG,
            },
        ),
        (
            [
                Link(INITIAL, Field("Tag_1")),
                Link(
                    Field("Tag_1"),
                    Field("Tag_2"),
                    condition=Equal(Variable("Tag_1"), Variable("TLV::Msg_Data")),
                ),
                Link(
                    Field(ID("Tag_2", location=Location((10, 20)))),
                    FINAL,
                    condition=Equal(Variable("Tag_1"), Variable("TLV::Msg_Data")),
                ),
            ],
            {
                Field("Tag_1"): TLV_TAG,
                Field("Tag_2"): TLV_TAG,
            },
        ),
    ],
)
def test_always_true_message_condition(
    structure: abc.Sequence[Link], types: abc.Mapping[Field, Type]
) -> None:
    link_to_final = [l for l in structure if l.target == FINAL][0]
    assert_message_model_error(
        structure,
        types,
        (
            rf'^<stdin>:10:20: model: error: condition "{link_to_final.condition}"'
            rf' on transition "{link_to_final.source.identifier}" -> "Final" is always true$'
        ),
    )


def test_possibly_always_true_refinement(
    monkeypatch: MonkeyPatch, capsys: CaptureFixture[str]
) -> None:
    message = Message(
        "P::M",
        [
            Link(INITIAL, Field("Tag")),
            Link(Field("Tag"), Field("Value")),
            Link(Field("Value"), FINAL),
        ],
        {
            Field("Tag"): TLV_TAG,
            Field("Value"): OPAQUE,
        },
    )
    condition = Or(
        Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
        Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
    )
    monkeypatch.setattr(Proof, "result", ProofResult.UNKNOWN)
    Refinement(
        "In_Message",
        message,
        Field(ID("Value", location=Location((10, 20)))),
        MESSAGE,
        condition,
    ).error.propagate()
    captured = capsys.readouterr()
    assert (
        f'<stdin>:10:20: model: warning: condition "{condition}"'
        ' in refinement of "P::M" might be always false'
    ) in captured.out


def test_possibly_always_true_message_condition(
    monkeypatch: MonkeyPatch, capsys: CaptureFixture[str]
) -> None:
    monkeypatch.setattr(Proof, "result", ProofResult.UNKNOWN)
    monkeypatch.setattr(Message, "_prove_reachability", lambda x: None)
    monkeypatch.setattr(Message, "_prove_contradictions", lambda x: None)
    monkeypatch.setattr(Message, "_prove_coverage", lambda x: None)
    monkeypatch.setattr(Message, "_prove_overlays", lambda x: None)
    monkeypatch.setattr(Message, "_prove_field_positions", lambda x: None)
    monkeypatch.setattr(Message, "_prove_message_size", lambda x: None)
    Message(
        "P::M",
        [
            Link(INITIAL, Field("Tag")),
            Link(
                Field(ID("Tag", location=Location((10, 20)))),
                FINAL,
                condition=Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
            ),
        ],
        {
            Field("Tag"): TLV_TAG,
        },
    )
    captured = capsys.readouterr()
    assert (
        '<stdin>:10:20: model: warning: condition "Tag = TLV::Msg_Data"'
        ' on transition "Tag" -> "Final" might be always true\n'
    ) == captured.out


@pytest.mark.parametrize(
    ["unchecked", "expected"],
    [
        (
            UncheckedMessage(
                ID("P::No_Ref"),
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
                    (Field("F2"), INTEGER.identifier, []),
                    (Field("F3"), ENUMERATION.identifier, []),
                    (Field("F4"), INTEGER.identifier, []),
                ],
                None,
                None,
                None,
            ),
            UnprovenMessage(
                "P::No_Ref",
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
                {
                    Field("F1"): OPAQUE,
                    Field("F2"): INTEGER,
                    Field("F3"): ENUMERATION,
                    Field("F4"): INTEGER,
                },
            ),
        ),
    ],
)
def test_unchecked_message_checked(unchecked: UncheckedMessage, expected: Message) -> None:
    assert unchecked.checked([OPAQUE, ENUMERATION, INTEGER]) == expected


@pytest.mark.parametrize(
    ["unchecked", "expected"],
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
                    (Field("F2"), INTEGER.identifier, []),
                    (Field("F3"), ENUMERATION.identifier, []),
                    (Field("F4"), INTEGER.identifier, []),
                ],
                None,
                None,
                None,
            ),
            r'^<stdin>:2:3: model: error: invalid format for identifier "T"$',
        ),
    ],
)
def test_unchecked_message_checked_error(unchecked: UncheckedMessage, expected: str) -> None:
    with pytest.raises(RecordFluxError, match=expected):
        unchecked.checked([OPAQUE, ENUMERATION, INTEGER])
