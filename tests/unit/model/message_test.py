from __future__ import annotations

import textwrap
from collections import abc
from copy import deepcopy
from functools import lru_cache

import pytest

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
    UncheckedDerivedMessage,
    UncheckedMessage,
)
from rflx.model.message import ByteOrder
from tests.data import models
from tests.utils import assert_equal, assert_message_model_error

M_NO_REF = UncheckedMessage(
    ID("P::No_Ref"),
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
    [],
    [
        (Field("F1"), OPAQUE.identifier, []),
        (Field("F2"), models.integer().identifier, []),
        (Field("F3"), models.enumeration().identifier, []),
        (Field("F4"), models.integer().identifier, []),
    ],
)

M_SMPL_REF = UncheckedMessage(
    ID("P::Smpl_Ref"),
    [Link(INITIAL, Field("NR")), Link(Field("NR"), FINAL)],
    [],
    [(Field("NR"), M_NO_REF.identifier, [])],
)


M_DBL_REF = UncheckedMessage(
    ID("P::Dbl_Ref"),
    [Link(INITIAL, Field("SR")), Link(Field("SR"), Field("NR")), Link(Field("NR"), FINAL)],
    [],
    [
        (Field("SR"), M_SMPL_REF.identifier, []),
        (Field("NR"), M_NO_REF.identifier, []),
    ],
)


M_NO_REF_DERI = UncheckedDerivedMessage(
    ID("P::No_Ref_Deri"),
    M_NO_REF.identifier,
)


M_SMPL_REF_DERI = UncheckedDerivedMessage(
    ID("P::Smpl_Ref_Deri"),
    M_SMPL_REF.identifier,
)


@lru_cache
def parameterized_message() -> Message:
    return Message(
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
            Field("P1"): models.integer(),
            Field("P2"): models.enumeration(),
            Field("F1"): OPAQUE,
            Field("F2"): models.integer(),
        },
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
        match='^<stdin>:10:8: model: error: invalid format for identifier "A::B::C"$',
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
def test_invalid_parameter_type_composite(parameter_type: abc.Callable[[], Type]) -> None:
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): parameter_type(), Field("X"): models.integer()}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:1:2: model: error: parameters must have a scalar type$",
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

    types = {x: models.integer()}

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


def test_direct_cycle() -> None:
    t = Integer("P::T", Number(0), Number(1), Number(1))

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), Field("Y")),
        Link(Field(ID("Y", Location((3, 5)))), Field("X")),
    ]

    types = {Field("X"): t, Field("Y"): t}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:8: model: error: structure of "P::M" contains cycle$',
        location=Location((10, 8)),
    )


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
    p = Field(ID("P", Location((0, 1))))
    a = Field(ID("A", Location((1, 2))))
    b = Field(ID("B", Location((2, 3))))

    message = Message(
        "P::M",
        [
            Link(INITIAL, Field("A")),
            Link(Field("A"), Field("B"), condition=Less(Variable("A"), Variable("P"))),
            Link(Field("B"), FINAL),
        ],
        {p: models.integer(), a: models.integer(), b: models.integer()},
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
    types = {Field("X"): models.enumeration()}

    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:2: model: error: invalid use of enum literal "P::Zero" in expression$',
    )


class NewType(Type):
    pass


def test_invalid_message_field_type() -> None:
    structure = [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)]
    types = {Field(ID("F", Location((1, 2)))): NewType("P::T")}

    assert_message_model_error(
        structure,
        types,
        "^<stdin>:1:2: model: error: message fields must have a scalar or composite type$",
    )


def test_unused_parameter() -> None:
    structure = [Link(INITIAL, Field("X")), Link(Field("X"), FINAL)]
    types = {Field(ID("P", Location((1, 2)))): models.integer(), Field("X"): models.integer()}

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
        structure,
        types,
        r'^<stdin>:10:20: model: error: undefined variable "X"$',
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
        r'^<stdin>:10:20: model: error: undefined variable "Field_Size"$',
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
        r'^<stdin>:10:20: model: error: undefined variable "Field_First"$',
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
                Variable("Opt", location=Location((10, 30))),
                Number(8),
                location=Location((10, 20)),
            ),
        ),
        Link(Field("Data"), FINAL),
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
        r'<stdin>:10:30: model: error: undefined variable "Opt"\n'
        r"<stdin>:10:20: model: info: on path Flag -> Any -> Data"
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
    types = {Field("Length"): models.integer(), Field("Data"): OPAQUE}
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


@pytest.mark.parametrize("lower", [Number(0), Number(1)])
def test_invalid_element_in_relation_to_aggregate(lower: Number) -> None:
    integer = Integer("P::Integer", lower, Number(255), Number(8))
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
        rf'^<stdin>:10:20: model: error: expected integer type "P::Integer" \({lower} .. 255\)\n'
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
        {Field("F"): models.integer()},
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
    t = Integer("P::T", Number(0), Number(3), Number(4))
    o = Field(ID("O", location=Location((44, 3))))
    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:44:3: model: error: opaque field "O" not aligned to'
            r" 8 bit boundary [(]P -> O[)]$"
        ),
    ):
        Message(
            "P::M",
            [
                Link(INITIAL, Field("P")),
                Link(Field("P"), o, size=Number(128)),
                Link(o, Field("Q")),
                Link(Field("Q"), FINAL),
            ],
            {
                Field("P"): t,
                o: OPAQUE,
                Field("Q"): t,
            },
        )


def test_opaque_not_byte_aligned_dynamic() -> None:
    o2 = Field(ID("O2", location=Location((44, 3))))
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: opaque field "O2" not aligned to'
        r" 8 bit boundary [(]L1 -> O1 -> L2 -> O2[)]",
    ):
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
                Field("L1"): models.integer(),
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
        {Field("L"): models.integer(), Field("O1"): OPAQUE},
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
        {Field("L"): models.integer(), Field("O1"): OPAQUE, Field("O2"): OPAQUE},
    )


def test_opaque_size_not_multiple_of_8() -> None:
    o = Field("O")
    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:44:3: model: error: size of opaque field "O"'
            " not multiple of 8 bit [(]O[)]$"
        ),
    ):
        Message(
            "P::M",
            [Link(INITIAL, o, size=Number(68, location=Location((44, 3)))), Link(o, FINAL)],
            {o: OPAQUE},
        )


def test_opaque_size_not_multiple_of_8_dynamic() -> None:
    o = Field("O")
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: size of opaque field "O" not multiple of 8 bit'
        " [(]L -> O[)]",
    ):
        Message(
            "P::M",
            [
                Link(INITIAL, Field("L")),
                Link(Field("L"), o, size=Variable("L", location=Location((44, 3)))),
                Link(o, FINAL),
            ],
            {Field("L"): models.integer(), o: OPAQUE},
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
        {Field("L"): models.integer(), Field("O"): OPAQUE},
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
            Field("F1"): deepcopy(models.integer()),
            Field("F2"): deepcopy(models.integer()),
            Field("F3"): deepcopy(models.integer()),
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
            Field("X_F1"): deepcopy(models.integer()),
            Field("X_F2"): deepcopy(models.integer()),
            Field("X_F3"): deepcopy(models.integer()),
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
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
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
        Field("F1"): models.enumeration(),
        Field("F2"): models.integer(),
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
        Field("F1"): models.enumeration(),
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
            Field("F1"),
            Field("F2"),
            condition=Less(Variable("F1"), Number(80), Location((11, 7))),
        ),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field(ID("F1", Location((8, 4)))): models.integer(),
        Field("F2"): models.integer(),
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
        Field("F1"): models.integer(),
        Field("F2"): OPAQUE,
        Field("F3"): models.integer(),
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
        Field("F1"): models.integer(),
        Field("F2"): OPAQUE,
        Field("F3"): models.integer(),
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
        Field("F2"): models.integer(),
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
        f1: models.integer(),
        f2: models.integer(),
        f3: models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'<stdin>:11:6: model: error: unreachable field "F2"\n'
        r'<stdin>:10:8: model: info: on path: "F1"\n'
        r'<stdin>:11:9: model: info: on path: "F2"\n'
        r'<stdin>:20:2: model: info: unsatisfied "F1 <= 80"\n'
        r'<stdin>:22:4: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:12:7: model: error: unreachable field "F3"\n'
        r'<stdin>:10:8: model: info: on path: "F1"\n'
        r'<stdin>:12:10: model: info: on path: "F3"\n'
        r'<stdin>:21:3: model: info: unsatisfied "F1 > 80"\n'
        r'<stdin>:23:5: model: info: unsatisfied "F1 <= 80"'
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
        r'<stdin>:20:10: model: error: unreachable field "F1"\n'
        r'<stdin>:20:10: model: info: on path: "F1"\n'
        r'<stdin>:5:10: model: info: unsatisfied "1 = 2"'
        r"$",
    )


def test_invalid_path_2() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Equal(Number(1), Number(2))),
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
        r'model: error: unreachable field "F1"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "1 = 2"'
        r"$",
    )


def test_unreachable() -> None:
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
        r'model: error: unreachable field "F1"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1 <= 100"\n'
        r'model: info: unsatisfied "F1 > 1000"'
        r"$",
    )


def test_invalid_type_condition_range_low() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), condition=Less(Variable("F1"), Number(1))),
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
        r'model: error: unreachable field "F1"\n'
        r'model: info: on path: "F1"\n'
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
        r'model: error: unreachable field "F1"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1 <= 100"\n'
        r'model: info: unsatisfied "F1 > 200"'
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
                NotEqual(Variable("T"), Variable("Two")),
                LessEqual(Variable("L"), Number(8192)),
            ),
        ),
        Link(Field("V"), FINAL),
    ]
    types = {
        Field("L"): models.integer(),
        Field("T"): models.enumeration(),
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
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
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
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
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
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
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
        r'^<stdin>:5:14: model: error: invalid First for field "F2"$',
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
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:10:20: model: error: invalid First for field "F2"$',
    )


def test_valid_size_reference() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), size=Mul(Number(8), Variable("F1"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): models.integer(),
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
        Field("F1"): models.integer(),
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
        Field("F1"): models.integer(),
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
        Field("L"): models.integer(),
        o: OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:44:3: model: error: negative size for field "O" [(]L -> O[)]$',
    )


def test_invalid_negative_field_size_3() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            Field("F2"),
            size=Sub(Mul(Variable("F1"), Number(8)), Number(16), location=Location((1, 2))),
        ),
        Link(
            Field("F2"),
            FINAL,
            condition=Greater(Variable("F1"), Number(1)),
        ),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:1:2: model: error: negative size for field "F2" [(]F1 -> F2[)]$',
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
        structure,
        types,
        r'^model: error: unconstrained field "F1" without size aspect$',
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
        '^model: error: unconstrained field "F1" without size aspect$',
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


def test_field_after_message_start(monkeypatch: pytest.MonkeyPatch) -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), first=Sub(First("Message"), Number(1000))),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): models.integer(), Field("F2"): models.integer()}
    monkeypatch.setattr(Message, "_verify_links", lambda _: None)
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
@pytest.mark.parametrize("type_", [OPAQUE, models.sequence_integer_vector()])
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
@pytest.mark.parametrize("type_", [OPAQUE, models.sequence_integer_vector()])
def test_message_with_implicit_size_multiple_fields(
    size: Expr,
    condition: Expr,
    type_: Type,
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
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
        Field("F4"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        '^model: error: no path to FINAL for field "F4" in "P::M"$',
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
        r'model: error: no path to FINAL for field "F4" in "P::M"\n'
        r'model: error: no path to FINAL for field "F5" in "P::M"\n'
        r'model: error: no path to FINAL for field "F6" in "P::M"'
        r"$",
    )


def test_unreachable_field_mod_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
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
        r'model: error: unreachable field "F1"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_unreachable_field_mod_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, Equal(Last("F1"), Last("Message"))),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_unreachable_field_range_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
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
        r'model: error: unreachable field "F1"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_unreachable_field_range_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, Equal(Last("F1"), Last("Message"))),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_unreachable_field_enum_first() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Greater(First("F1"), First("Message"))),
        Link(Field("F2"), FINAL),
    ]
    types = {
        Field("F1"): models.enumeration(),
        Field("F2"): models.enumeration(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F1"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: unsatisfied "F1\'First = Message\'First"\n'
        r'model: info: unsatisfied "F1\'First > Message\'First"'
        r"$",
    )


def test_unreachable_field_enum_last() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, Equal(Last("F1"), Last("Message"))),
    ]
    types = {
        Field("F1"): models.enumeration(),
        Field("F2"): models.enumeration(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F2\'Last = [(]F1\'Last [+] 1 [+] 8[)] - 1"\n'
        r'model: info: unsatisfied "Message\'Last >= F2\'Last"\n'
        r'model: info: unsatisfied "F1\'Last = Message\'Last"'
        r"$",
    )


def test_unreachable_field_outgoing() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), LessEqual(Variable("F1"), Number(32))),
        Link(Field("F1"), FINAL, Greater(Variable("F1"), Number(32))),
        Link(Field("F2"), FINAL, Greater(Variable("F1"), Number(32))),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        r"^"
        r'model: error: unreachable field "F2"\n'
        r'model: info: on path: "F1"\n'
        r'model: info: on path: "F2"\n'
        r'model: info: unsatisfied "F1 <= 32"\n'
        r'model: info: unsatisfied "F1 > 32"'
        r"$",
    )


def test_unreachable_field_outgoing_multi() -> None:
    structure = [
        Link(INITIAL, Field(ID("F1", Location((86, 13))))),
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
                Greater(Variable("F1"), Number(32), location=Location((22, 34))),
                LessEqual(Variable("F1"), Number(48)),
            ),
        ),
        Link(Field("F2"), FINAL, Greater(Variable("F1"), Number(48))),
        Link(Field("F3"), FINAL),
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
        r'<stdin>:90:12: model: error: unreachable field "F2"\n'
        r'<stdin>:86:13: model: info: on path: "F1"\n'
        r'<stdin>:91:13: model: info: on path: "F2"\n'
        r'<stdin>:66:3: model: info: unsatisfied "F1 <= 32"\n'
        r'<stdin>:22:34: model: info: unsatisfied "F1 > 32"'
        r"$",
    )


def test_size_aspect_final() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), FINAL, size=Number(100, location=Location((4, 12)))),
    ]
    types = {
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
    }
    assert_message_model_error(
        structure,
        types,
        '^<stdin>:4:12: model: error: size aspect for final field in "P::M"$',
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
        r'model: error: unreachable field "Magic"\n'
        r'model: info: on path: "Magic"\n'
        r'model: info: unsatisfied "Magic\'Size = 40"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Size"'
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
        r'model: error: unreachable field "Magic"\n'
        r'model: info: on path: "Magic"\n'
        r'model: info: unsatisfied "Magic\'Size = 40"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Size"'
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
        r'model: error: unreachable field "Magic"\n'
        r'model: info: on path: "Magic"\n'
        r'model: info: unsatisfied "Magic\'Size = 40"\n'
        r'model: info: unsatisfied "2 [*] 8 = Magic\'Size"'
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
            "P::Arr",
            Integer("P::Integer", Number(0), Number(127), Number(8)),
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
        r'<stdin>:3:5: model: error: unreachable field "Magic"\n'
        r'<stdin>:3:5: model: info: on path: "Magic"\n'
        r'<stdin>:19:17: model: info: unsatisfied "Magic\'Size = 40"\n'
        r'<stdin>:66:3: model: info: unsatisfied "Integer\'Size = 8"\n'
        r'<stdin>:17:3: model: info: unsatisfied "2 [*] Integer\'Size = Magic\'Size"'
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
                Variable("Magic"),
                Aggregate(Number(1), Number(2)),
                location=Location((10, 5)),
            ),
        ),
    ]
    types = {
        Field("Length"): Integer(
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
        r'<stdin>:17:3: model: error: unreachable field "Magic"\n'
        r'<stdin>:2:5: model: info: on path: "Length"\n'
        r'<stdin>:3:5: model: info: on path: "Magic"\n'
        r'<stdin>:10:5: model: info: unsatisfied "2 [*] 8 = Magic\'Size"\n'
        r'<stdin>:5:10: model: info: unsatisfied "Length >= 10"\n'
        r'<stdin>:6:5: model: info: unsatisfied "Magic\'Size = 8 [*] Length"'
        r"$",
    )


def test_no_unreachable_field_multi() -> None:
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
        Field("F0"): models.integer(),
        Field("F1"): models.integer(),
        Field("F2"): models.integer(),
        Field("F3"): models.integer(),
        Field("F4"): models.integer(),
        Field("F5"): models.integer(),
    }
    Message("P::M", structure, types)


@pytest.mark.parametrize(
    "condition",
    [
        Equal(Variable("Flag"), Number(1)),
        And(Equal(Variable("Flag"), Number(1)), Greater(Variable("Opt1"), Number(0))),
    ],
)
def test_discontiguous_optional_fields(condition: Expr) -> None:
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
            condition=condition,
            size=Mul(Variable("Opt1"), Number(8)),
        ),
        Link(
            Field("Data"),
            FINAL,
            condition=Equal(Variable("Flag"), Number(0)),
        ),
        Link(
            Field("Opt2"),
            FINAL,
        ),
    ]
    types = {
        Field("Flag"): models.integer(),
        Field("Opt1"): models.integer(),
        Field("Data"): models.integer(),
        Field("Opt2"): OPAQUE,
    }
    Message("P::M", structure, types)


def test_discontiguous_optional_fields_error() -> None:
    # TODO(eng/recordflux/RecordFlux#499): Enable disjunctions with references to optional fields
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
            condition=Or(
                And(Equal(Variable("Flag"), Number(1)), Greater(Variable("Opt1"), Number(100))),
                Greater(Variable("Flag"), Number(1)),
            ),
            size=Mul(Variable("Opt1"), Number(8)),
        ),
        Link(
            Field("Data"),
            FINAL,
            condition=Equal(Variable("Flag"), Number(0)),
        ),
        Link(
            Field("Opt2"),
            FINAL,
        ),
    ]
    types = {
        Field("Flag"): models.integer(),
        Field("Opt1"): models.integer(),
        Field("Data"): models.integer(),
        Field("Opt2"): OPAQUE,
    }
    assert_message_model_error(
        structure,
        types,
        (
            r"^"
            r'model: error: undefined variable "Opt1"\n'
            r"model: info: on path Flag -> Data -> Opt2\n"
            r'model: error: undefined variable "Opt1"\n'
            r"model: info: on path Flag -> Data -> Opt2"
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
                ],
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
    types = {f1: models.integer(), f2: models.integer(), f3: models.integer()}
    message = Message("P::M", structure, types, checksums=checksums)
    assert message.checksums == checksums


@pytest.mark.parametrize(
    ("checksums", "condition", "error"),
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
        "P::M",
        [
            Link(INITIAL, Field("A")),
            Link(Field("A"), Field("B"), size=Mul(Size("A"), Number(8))),
            Link(Field("B"), Field("C")),
            Link(Field("C"), FINAL),
        ],
        {Field("A"): models.integer(), Field("B"): OPAQUE, Field("C"): OPAQUE},
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


def test_target_last_opt() -> None:
    link_ia = Link(INITIAL, Field("A"))
    link_ab = Link(Field("A"), Field("B"), size=Mul(Size("A"), Number(8)))
    link_bc = Link(Field("B"), Field("C"))
    message = Message(
        "P::M",
        [link_ia, link_ab, link_bc, Link(Field("C"), FINAL)],
        {Field("A"): models.integer(), Field("B"): OPAQUE, Field("C"): OPAQUE},
        location=Location((30, 10)),
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
        "P::M",
        [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
        {Field("F"): models.integer()},
    )
    assert_equal(
        message.copy(identifier="A::B"),
        Message(
            "A::B",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): models.integer()},
        ),
    )
    assert_equal(
        message.copy(
            structure=[Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            types={Field("C"): models.integer()},
            byte_order={Field("C"): ByteOrder.HIGH_ORDER_FIRST},
        ),
        Message(
            "P::M",
            [Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            {Field("C"): models.integer()},
        ),
    )
    assert_equal(
        message.copy(
            structure=[Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            types={Field("C"): models.integer()},
            byte_order={Field("C"): ByteOrder.LOW_ORDER_FIRST},
        ),
        Message(
            "P::M",
            [Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            {Field("C"): models.integer()},
            byte_order=ByteOrder.LOW_ORDER_FIRST,
        ),
    )


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
            Field("Length"): models.tlv_length(),
            Field("Data"): OPAQUE,
        },
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
                    Or(Equal(Variable("X"), FALSE), Equal(Variable("X"), TRUE)),
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
            Field("A"): models.tlv_length(),
            Field("B"): models.tlv_length(),
        },
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
            Field("A"): models.tlv_length(),
            Field("B"): models.tlv_length(),
            Field("C"): models.tlv_length(),
        },
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
            Field("Length"): models.tlv_length(),
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
                        Equal(Selected(Variable("X"), "Has_Data"), FALSE),
                        Equal(Selected(Variable("X"), "Has_Data"), TRUE),
                    ),
                    Size(Variable("Z")),
                ),
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
            Field("A"): models.tlv_length(),
            Field("B"): models.tlv_length(),
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
            ),
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
            Field("A"): models.tlv_length(),
            Field("B"): models.tlv_length(),
            Field("C"): models.tlv_length(),
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
        match=r"^model: error: unable to calculate maximum size of message with implicit size$",
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
            r"^model: error: unable to calculate maximum field sizes of message with implicit size$"
        ),
    ):
        models.ethernet_frame().max_field_sizes()


def test_derived_message_incorrect_base_name() -> None:
    with pytest.raises(
        RecordFluxError,
        match='^<stdin>:40:8: model: error: invalid format for identifier "A::B::C"$',
    ):
        DerivedMessage("P::M", Message(ID("A::B::C", location=Location((40, 8))), [], {}))


def test_prefixed_message() -> None:
    assert_equal(
        Message(
            ID("P::M"),
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
                Link(Field("F2"), FINAL, Equal(Variable("F2"), Literal("P::One"))),
                Link(Field("F3"), Field("F4"), size=Mul(Variable("F3"), Number(8))),
                Link(Field("F4"), FINAL),
            ],
            {
                Field("F1"): models.integer(),
                Field("F2"): models.enumeration(),
                Field("F3"): models.integer(),
                Field("F4"): OPAQUE,
            },
        ).prefixed("X_"),
        Message(
            ID("P::M"),
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
                Link(Field("X_F2"), FINAL, Equal(Variable("X_F2"), Literal("P::One"))),
                Link(Field("X_F3"), Field("X_F4"), size=Mul(Variable("X_F3"), Number(8))),
                Link(Field("X_F4"), FINAL),
            ],
            {
                Field("X_F1"): models.integer(),
                Field("X_F2"): models.enumeration(),
                Field("X_F3"): models.integer(),
                Field("X_F4"): OPAQUE,
            },
        ),
    )


def test_merge_message_simple() -> None:
    assert deepcopy(M_SMPL_REF).merged(
        [models.integer(), models.enumeration(), msg_no_ref()],
    ) == UncheckedMessage(
        ID("P::Smpl_Ref"),
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
        [],
        [
            (Field("NR_F1"), OPAQUE.identifier, []),
            (Field("NR_F2"), models.integer().identifier, []),
            (Field("NR_F3"), models.enumeration().identifier, []),
            (Field("NR_F4"), models.integer().identifier, []),
        ],
        checksums={},
        byte_order={
            Field("NR_F1"): ByteOrder.HIGH_ORDER_FIRST,
            Field("NR_F2"): ByteOrder.HIGH_ORDER_FIRST,
            Field("NR_F3"): ByteOrder.HIGH_ORDER_FIRST,
            Field("NR_F4"): ByteOrder.HIGH_ORDER_FIRST,
        },
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
        ),
    )


def test_merge_message_simple_derived() -> None:
    assert_equal(
        deepcopy(M_SMPL_REF_DERI).checked([models.integer(), models.enumeration(), msg_smpl_ref()]),
        DerivedMessage(
            ID("P::Smpl_Ref_Deri"),
            Message(
                ID("P::Smpl_Ref"),
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
                    Field("NR_F2"): models.integer(),
                    Field("NR_F3"): models.enumeration(),
                    Field("NR_F4"): models.integer(),
                },
            ),
        ),
    )


def test_merge_message_checksums() -> None:
    inner_msg = Message(
        ID("P::Merge_Test_Byte_Order"),
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2")),
            Link(Field("F2"), FINAL, condition=ValidChecksum("F2")),
        ],
        {
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
        },
        checksums={ID("F2"): [Variable("F1")]},
    )
    outer_msg = UncheckedMessage(
        ID("P::Outer_Msg"),
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
    )
    assert_equal(
        outer_msg.merged([OPAQUE, models.integer(), inner_msg]),
        UncheckedMessage(
            ID("P::Outer_Msg"),
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
        ),
    )


def test_merge_message_byte_order() -> None:
    inner_msg = Message(
        ID("P::Merge_Test_Byte_Order"),
        [Link(INITIAL, Field("F1")), Link(Field("F1"), Field("F2")), Link(Field("F2"), FINAL)],
        {
            Field("F1"): models.integer(),
            Field("F2"): models.enumeration(),
        },
        byte_order=ByteOrder.LOW_ORDER_FIRST,
    )
    outer_msg = UncheckedMessage(
        ID("P::Outer_Msg"),
        [
            Link(INITIAL, Field("NR")),
            Link(Field("NR"), FINAL),
        ],
        [],
        [
            (Field("NR"), inner_msg.identifier, []),
        ],
    )
    assert_equal(
        outer_msg.merged([models.integer(), models.enumeration(), inner_msg]),
        UncheckedMessage(
            ID("P::Outer_Msg"),
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
        ),
    )


def test_merge_message_constrained() -> None:
    m1 = Message(
        ID("P::M1"),
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F3"), GreaterEqual(Variable("F1"), Number(100))),
            Link(Field("F1"), Field("F2"), Less(Variable("F1"), Number(100))),
            Link(Field("F2"), FINAL, Greater(Variable("F1"), Number(1))),
            Link(Field("F3"), FINAL),
        ],
        {
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
            Field("F3"): models.integer(),
        },
    )
    m2 = UncheckedMessage(
        ID("P::M2"),
        [
            Link(INITIAL, Field("F4")),
            Link(
                Field("F4"),
                FINAL,
                Equal(Variable("F4_F1"), Number(40)),
            ),
        ],
        [],
        [
            (Field("F4"), m1.identifier, []),
        ],
    )
    expected = UncheckedMessage(
        ID("P::M2"),
        [
            Link(Field("F4_F1"), Field("F4_F2"), Less(Variable("F4_F1"), Number(100))),
            Link(
                Field("F4_F2"),
                FINAL,
                And(
                    Equal(Variable("F4_F1"), Number(40)),
                    Greater(Variable("F4_F1"), Number(1)),
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
    )
    merged = m2.merged([m1, models.integer()])

    assert merged == expected


def test_merge_message_constrained_empty() -> None:
    m1 = Message(
        ID("P::M1"),
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), Equal(Variable("F1"), Number(2))),
            Link(Field("F1"), FINAL, Equal(Variable("F1"), Number(1))),
            Link(Field("F2"), FINAL, Equal(Variable("F2"), Number(2))),
        ],
        {
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
        },
    )
    m2 = UncheckedMessage(
        ID("P::M2"),
        [
            Link(INITIAL, Field("F3")),
            Link(
                Field("F3"),
                FINAL,
                And(
                    Equal(Variable("F3_F1"), Number(2)),
                    Equal(Variable("F3_F2"), Number(1)),
                ),
            ),
        ],
        [],
        [
            (Field("F3"), m1.identifier, []),
        ],
    )
    with pytest.raises(
        RecordFluxError,
        match=r'^model: error: empty message type when merging field "F3"$',
    ):
        m2.merged([models.integer(), m1])


def test_merge_message_error_name_conflict() -> None:
    m2_f2 = Field(ID("F2", Location((10, 5))))

    m2 = Message(
        ID("P::M2"),
        [Link(INITIAL, m2_f2), Link(m2_f2, FINAL)],
        {
            m2_f2: models.integer(),
        },
        location=Location((15, 3)),
    )

    m1_f1 = Field(ID("F1", Location((20, 8))))
    m1_f1_f2 = Field(ID("F1_F2", Location((30, 5))))

    m1 = UncheckedMessage(
        ID("P::M1"),
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
            r'<stdin>:30:5: model: error: name conflict for "F1_F2" in "P::M1"\n'
            r'<stdin>:15:3: model: info: when merging message "P::M2"\n'
            r'<stdin>:20:8: model: info: into field "F1"$'
        ),
    ):
        m1.merged([models.integer(), m2])


def test_merge_message_parameterized() -> None:
    msg_param_no_ref = Message(
        ID("P::Param_No_Ref"),
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), condition=Equal(Variable("P1"), Number(1))),
            Link(Field("F1"), FINAL, condition=Equal(Variable("P1"), Number(2))),
            Link(Field("F2"), FINAL),
        ],
        {
            Field("P1"): models.integer(),
            Field("F1"): models.integer(),
            Field("F2"): models.integer(),
        },
    )
    msg_param_param_ref = UncheckedMessage(
        ID("P::Param_Param_Ref"),
        [
            Link(INITIAL, Field("PNR")),
            Link(Field("PNR"), FINAL),
        ],
        [],
        [
            (Field("P2"), models.integer().identifier, []),
            (Field("PNR"), msg_param_no_ref.identifier, []),
        ],
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
        ),
    )


def test_merge_message_with_message_attributes() -> None:
    inner = Message(
        ID("P::I"),
        [
            Link(INITIAL, Field("I1")),
            Link(
                Field("I1"),
                Field("I2"),
                condition=Less(Variable("I1"), Number(128)),
                size=Sub(Last(ID("Message", location=Location((5, 10)))), Last("I1")),
                first=First("I1"),
            ),
            Link(
                Field("I1"),
                Field("I2"),
                condition=GreaterEqual(Variable("I1"), Number(128)),
                size=Sub(
                    Mul(Variable("I1"), Number(8)),
                    Add(
                        Sub(Last("I1"), First("Message")),
                        Number(1),
                    ),
                ),
            ),
            Link(Field("I2"), FINAL),
        ],
        {
            Field("I1"): models.integer(),
            Field("I2"): OPAQUE,
        },
    )

    valid_outer = UncheckedMessage(
        ID("P::O"),
        [
            Link(INITIAL, Field("O1")),
            Link(Field("O1"), Field("O2")),
            Link(Field("O2"), FINAL),
        ],
        [],
        [
            (Field("O1"), models.integer().identifier, []),
            (Field("O2"), inner.identifier, []),
        ],
    ).checked([OPAQUE, models.integer(), inner])

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
                    size=Add(
                        Mul(Variable("O2_I1"), Number(8)),
                        Add(
                            -Last("O2_I1"),
                            First("O2_I1"),
                            -Number(1),
                        ),
                    ),
                ),
                Link(Field("O2_I2"), FINAL),
            ],
            {
                Field("O1"): models.integer(),
                Field("O2_I1"): models.integer(),
                Field("O2_I2"): OPAQUE,
            },
        ),
    )
    o1 = Field(ID("O1", location=Location((2, 10))))
    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            "<stdin>:2:10: model: error: messages with implicit size may only be used for"
            " last fields\n"
            '<stdin>:5:10: model: info: message field with implicit size in "P::I"'
            "$"
        ),
    ):
        valid_outer = UncheckedMessage(
            ID("P::O"),
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
        ).checked([models.integer(), inner])


def test_merge_message_with_message_size_attribute() -> None:
    inner = Message(
        ID("P::I"),
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
        {
            Field("I"): OPAQUE,
        },
    )

    outer = UncheckedMessage(
        ID("P::O"),
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
        [],
        [
            (Field("O1"), models.integer().identifier, []),
            (Field("O2"), models.integer().identifier, []),
            (Field("O3"), models.integer().identifier, []),
            (Field("A"), inner.identifier, []),
            (Field("B"), inner.identifier, []),
        ],
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
            Field("O1"): models.integer(),
            Field("O2"): models.integer(),
            Field("O3"): models.integer(),
            Field("A_I"): OPAQUE,
            Field("B_I"): OPAQUE,
        },
    )

    assert outer.checked([OPAQUE, models.integer(), inner]) == expected


def test_merge_message_type_message_size_attribute_in_outer_message() -> None:
    inner = Message(
        ID("P::I"),
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
        {
            Field("I"): OPAQUE,
        },
    )

    outer = UncheckedMessage(
        ID("P::O"),
        [
            Link(INITIAL, Field("O1")),
            Link(Field("O1"), Field("O2"), size=Sub(Last("Message"), Last("O1"))),
            Link(Field("O2"), FINAL),
        ],
        [],
        [
            (Field("O1"), inner.identifier, []),
            (Field("O2"), OPAQUE.identifier, []),
        ],
    )

    expected = Message(
        ID("P::O"),
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

    assert outer.checked([OPAQUE, inner]) == expected


def test_merge_message_with_condition_on_message_type_field() -> None:
    inner = Message(
        ID("P::I"),
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
        {
            Field("I"): OPAQUE,
        },
    )

    enumeration = Enumeration(
        "P::Enumeration",
        [("E1", Number(1)), ("E2", Number(2)), ("E3", Number(3))],
        Number(8),
        always_valid=False,
    )
    padding = Integer("P::Padding", Number(0), Number(0), Number(7))

    outer = UncheckedMessage(
        ID("P::O"),
        [
            Link(INITIAL, Field("Flag")),
            Link(Field("Flag"), Field("Padding")),
            Link(Field("Padding"), Field("Payload")),
            Link(
                Field("Payload"),
                FINAL,
                condition=And(
                    Equal(Variable("Flag"), TRUE),
                    Equal(Variable("Parameter"), Variable("E1")),
                ),
            ),
        ],
        [
            (Field("Parameter"), enumeration.identifier, []),
        ],
        [
            (Field("Flag"), BOOLEAN.identifier, []),
            (Field("Padding"), padding.identifier, []),
            (Field("Payload"), inner.identifier, []),
        ],
    )

    expected = Message(
        ID("P::O"),
        [
            Link(INITIAL, Field("Flag")),
            Link(Field("Flag"), Field("Padding")),
            Link(Field("Padding"), Field("Payload_I"), size=Number(128)),
            Link(
                Field("Payload_I"),
                FINAL,
                condition=And(
                    Equal(Variable("Flag"), TRUE),
                    Equal(Variable("Parameter"), Literal("P::E1")),
                ),
            ),
        ],
        {
            Field("Parameter"): enumeration,
            Field("Flag"): BOOLEAN,
            Field("Padding"): padding,
            Field("Payload_I"): OPAQUE,
        },
    )

    assert outer.checked([BOOLEAN, OPAQUE, enumeration, padding, inner]) == expected


def test_merge_message_with_illegal_condition_on_message_type_field() -> None:
    inner = Message(
        ID("P::I"),
        [
            Link(INITIAL, Field("I")),
            Link(Field("I"), FINAL),
        ],
        {
            Field("I"): models.integer(),
        },
    )

    outer = UncheckedMessage(
        ID("P::O"),
        [
            Link(INITIAL, Field("O")),
            Link(Field("O"), FINAL, condition=Equal(TRUE, Number(1, location=Location((1, 2))))),
        ],
        [],
        [
            (Field("O"), inner.identifier, []),
        ],
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            "^"
            '<stdin>:1:2: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:1:2: model: info: found type universal integer \(1\)"
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
        "P::M",
        [
            Link(INITIAL, Field("L")),
            Link(Field("L"), Field("O"), condition=Greater(Variable("L"), Number(100))),
            Link(Field("L"), Field("O"), condition=LessEqual(Variable("L"), Number(100))),
            Link(Field("O"), FINAL),
        ],
        {Field("L"): models.integer(), Field("O"): models.integer()},
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
        rty.Refinement(
            "F",
            rty.Message(
                "P::M",
                {("F",)},
                {},
                {ID("F"): rty.OPAQUE},
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
        match=r"^model: error: setting refinements for different message$",
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
            Field("L"): models.integer(),
            Field("O"): models.integer(),
            Field("P"): models.integer(),
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
               end message""",
        ),
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
        match=r'^<stdin>:22:10: model: error: unexpected format of package name "A::B"$',
    ):
        Refinement(
            ID("A::B", Location((22, 10))),
            models.ethernet_frame(),
            Field("Payload"),
            models.ethernet_frame(),
        )


def test_refinement_invalid_field_type() -> None:
    x = Field(ID("X", Location((20, 10))))

    message = Message("P::M", [Link(INITIAL, x), Link(x, FINAL)], {x: models.integer()})

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


def test_refinement_undefined_variable_in_condition() -> None:
    x = Field("X")

    message = Message("P::M", [Link(INITIAL, x, size=Number(8)), Link(x, FINAL)], {x: OPAQUE})

    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:20: model: error: undefined variable "Y"$',
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

    x = Field("X")
    y = Field("Y")

    message = Message(
        "P::M",
        [Link(INITIAL, x), Link(x, y, size=Number(8)), Link(y, FINAL)],
        {x: e, y: OPAQUE},
    )

    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:20: model: error: undefined variable "E1"$',
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
        "P::M",
        [
            Link(INITIAL, Field("L")),
            Link(Field("L"), Field("P"), size=Mul(Variable("L"), Number(8))),
            Link(Field("P"), FINAL),
        ],
        {
            Field("L"): Integer("P::T", Number(0), Number(255), Number(8)),
            Field("P"): OPAQUE,
        },
    )

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:20: model: error: expected integer type "P::T" \(0 \.\. 255\)\n'
            r'<stdin>:10:20: model: info: found enumeration type "__BUILTINS__::Boolean"'
            r"$"
        ),
    ):
        Refinement(
            "P",
            message,
            Field("P"),
            message,
            Equal(Variable("L"), Literal("True", type_=rty.BOOLEAN, location=Location((10, 20)))),
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
            Field("Tag_1"): models.integer(),
            Field("Tag_2"): models.integer(),
            Field("Has_Tag"): BOOLEAN,
        },
    )


@pytest.mark.parametrize(
    ("message", "condition"),
    [
        (
            lambda: Message(
                "P::M",
                [
                    Link(INITIAL, Field("Tag")),
                    Link(Field("Tag"), Field("Value")),
                    Link(Field("Value"), FINAL),
                ],
                {
                    Field("Tag"): models.tlv_tag(),
                    Field("Value"): OPAQUE,
                },
            ),
            Or(
                Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
            ),
        ),
        (
            lambda: Message(
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
                    Field("Tag"): models.tlv_tag(),
                    Field("Value"): OPAQUE,
                },
            ),
            Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
        ),
    ],
)
def test_always_true_refinement(message: abc.Callable[[], Message], condition: Expr) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf'^<stdin>:10:20: model: error: condition "{condition}"'
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
                "P::M",
                [
                    Link(INITIAL, Field("Tag")),
                    Link(Field("Tag"), Field("Value")),
                    Link(Field("Value"), FINAL),
                ],
                {
                    Field("Tag"): models.tlv_tag(),
                    Field("Value"): OPAQUE,
                },
            ),
            And(
                Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
                Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
            ),
        ),
        (
            lambda: Message(
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
                    Field("Tag"): models.tlv_tag(),
                    Field("Value"): OPAQUE,
                },
            ),
            Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
        ),
    ],
)
def test_always_false_refinement(message: abc.Callable[[], Message], condition: Expr) -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            rf'^<stdin>:10:20: model: error: condition "{condition}"'
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
    ("structure", "types"),
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
                Field("Tag"): models.tlv_tag(),
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
                Field("Tag_1"): models.tlv_tag(),
                Field("Tag_2"): models.tlv_tag(),
            },
        ),
    ],
)
def test_always_true_message_condition(
    structure: abc.Sequence[Link],
    types: abc.Mapping[Field, Type],
) -> None:
    link_to_final = next(l for l in structure if l.target == FINAL)  # pragma: no branch
    assert_message_model_error(
        structure,
        types,
        (
            rf'^<stdin>:10:20: model: error: condition "{link_to_final.condition}"'
            rf' on transition "{link_to_final.source.identifier}" -> "Final" is always true$'
        ),
    )


@pytest.mark.parametrize("value", [0, 42, 65535])
def test_not_always_true_message_condition_for_always_valid_enum(value: int) -> None:
    Message(
        "P::M",
        [
            Link(INITIAL, Field("A")),
            Link(Field("A"), FINAL, condition=Equal(Variable("A"), Literal("P::E"))),
        ],
        {
            Field("A"): Enumeration("P::T", [("E", Number(value))], Number(16), always_valid=True),
        },
    )


def test_possibly_always_true_refinement(
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    message = Message(
        "P::M",
        [
            Link(INITIAL, Field("Tag")),
            Link(Field("Tag"), Field("Value")),
            Link(Field("Value"), FINAL),
        ],
        {
            Field("Tag"): models.tlv_tag(),
            Field("Value"): OPAQUE,
        },
    )
    condition = Or(
        Equal(Variable("Tag"), Variable("TLV::Msg_Data")),
        Equal(Variable("Tag"), Variable("TLV::Msg_Error")),
    )
    inner_message = models.message()
    monkeypatch.setattr(Proof, "result", ProofResult.UNKNOWN)
    Refinement(
        "In_Message",
        message,
        Field(ID("Value", location=Location((10, 20)))),
        inner_message,
        condition,
    ).error.propagate()
    captured = capsys.readouterr()
    assert (
        f'<stdin>:10:20: model: warning: condition "{condition}"'
        ' in refinement of "P::M" might be always false'
    ) in captured.out


def test_unchecked_message_checked() -> None:
    unchecked = UncheckedMessage(
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
            (Field("F2"), models.integer().identifier, []),
            (Field("F3"), models.enumeration().identifier, []),
            (Field("F4"), models.integer().identifier, []),
        ],
    )
    expected = Message(
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
        {
            Field("F1"): OPAQUE,
            Field("F2"): models.integer(),
            Field("F3"): models.enumeration(),
            Field("F4"): models.integer(),
        },
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
            r'^<stdin>:2:3: model: error: invalid format for identifier "T"$',
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
    lnk1 = Link(Field("Payload"), Field("Other"))
    lnk2 = Link(Field("Other"), Field("Other2"))
    msg = Message(
        "Test::Message",
        [
            Link(INITIAL, Field("Size")),
            Link(Field("Size"), Field("Payload"), size=Mul(Variable("Size"), Number(8))),
            lnk1,
            lnk2,
            Link(Field("Other2"), FINAL),
        ],
        {
            Field("Size"): models.integer(),
            Field("Payload"): OPAQUE,
            Field("Other"): models.integer(),
            Field("Other2"): models.integer(),
        },
    )
    assert msg.link_first(lnk1) == (Field("Payload"), Size(Variable("F_Payload")))
    assert msg.link_first(lnk2) == (Field("Payload"), Add(Size(Variable("F_Payload")), Number(8)))
