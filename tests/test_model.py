# pylint: disable=too-many-lines
from copy import deepcopy
from typing import Mapping, Sequence

import pytest

import rflx.declaration as decl
import rflx.statement as stmt
from rflx.error import Location, RecordFluxError
from rflx.expression import (
    TRUE,
    Add,
    Aggregate,
    And,
    Div,
    Equal,
    Expr,
    First,
    Greater,
    GreaterEqual,
    Last,
    Length,
    Less,
    LessEqual,
    Mod,
    Mul,
    NotEqual,
    Number,
    Or,
    Pow,
    Sub,
    ValidChecksum,
    ValueRange,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    BUILTIN_TYPES,
    FINAL,
    INITIAL,
    OPAQUE,
    Array,
    DerivedMessage,
    Enumeration,
    Field,
    Link,
    Message,
    Model,
    ModularInteger,
    Opaque,
    RangeInteger,
    Refinement,
    Type,
    UnprovenDerivedMessage,
    UnprovenMessage,
)
from rflx.session import Session, State, Transition
from tests.models import ENUMERATION, ETHERNET_FRAME, MODULAR_INTEGER, RANGE_INTEGER
from tests.utils import assert_equal, assert_message_model_error, multilinestr

M_NO_REF = UnprovenMessage(
    "P.No_Ref",
    [
        Link(INITIAL, Field("F1"), length=Number(16)),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), LessEqual(Variable("F2"), Number(100)), first=First("F2")),
        Link(
            Field("F2"), Field("F4"), GreaterEqual(Variable("F2"), Number(200)), first=First("F2"),
        ),
        Link(Field("F3"), FINAL, Equal(Variable("F3"), Variable("ONE"))),
        Link(Field("F4"), FINAL),
    ],
    {
        Field("F1"): Opaque(),
        Field("F2"): MODULAR_INTEGER,
        Field("F3"): ENUMERATION,
        Field("F4"): RANGE_INTEGER,
    },
)

M_SMPL_REF = UnprovenMessage(
    "P.Smpl_Ref",
    [Link(INITIAL, Field("NR")), Link(Field("NR"), FINAL)],
    {Field("NR"): deepcopy(M_NO_REF)},
)


M_CMPLX_REF = UnprovenMessage(
    "P.Cmplx_Ref",
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
        Field("F1"): deepcopy(MODULAR_INTEGER),
        Field("F2"): deepcopy(MODULAR_INTEGER),
        Field("F3"): deepcopy(RANGE_INTEGER),
        Field("NR"): deepcopy(M_NO_REF),
        Field("F5"): deepcopy(MODULAR_INTEGER),
        Field("F6"): deepcopy(RANGE_INTEGER),
    },
)


M_DBL_REF = UnprovenMessage(
    "P.Dbl_Ref",
    [Link(INITIAL, Field("SR")), Link(Field("SR"), Field("NR")), Link(Field("NR"), FINAL)],
    {Field("SR"): deepcopy(M_SMPL_REF), Field("NR"): deepcopy(M_NO_REF)},
)


M_NO_REF_DERI = UnprovenDerivedMessage(
    "P.No_Ref_Deri",
    M_NO_REF,
    [
        Link(INITIAL, Field("F1"), length=Number(16)),
        Link(Field("F1"), Field("F2")),
        Link(Field("F2"), Field("F3"), LessEqual(Variable("F2"), Number(100)), first=First("F2")),
        Link(
            Field("F2"), Field("F4"), GreaterEqual(Variable("F2"), Number(200)), first=First("F2"),
        ),
        Link(Field("F3"), FINAL, Equal(Variable("F3"), Variable("ONE"))),
        Link(Field("F4"), FINAL),
    ],
    {
        Field("F1"): Opaque(),
        Field("F2"): MODULAR_INTEGER,
        Field("F3"): ENUMERATION,
        Field("F4"): RANGE_INTEGER,
    },
)


M_SMPL_REF_DERI = UnprovenDerivedMessage(
    "P.Smpl_Ref_Deri",
    M_SMPL_REF,
    [Link(INITIAL, Field("NR")), Link(Field("NR"), FINAL)],
    {Field("NR"): deepcopy(M_NO_REF_DERI)},
)


def assert_type_error(instance: Type, regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        instance.error.propagate()


def assert_model_error(types: Sequence[Type], regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Model([*BUILTIN_TYPES.values(), *types])


def assert_message(actual: Message, expected: Message, msg: str = None) -> None:
    msg = f"{expected.full_name} - {msg}" if msg else expected.full_name
    assert actual.full_name == expected.full_name, msg
    assert actual.structure == expected.structure, msg
    assert actual.types == expected.types, msg
    assert actual.fields == expected.fields, msg


def test_type_name() -> None:
    t = ModularInteger("Package.Type_Name", Number(256))
    assert t.name == "Type_Name"
    assert t.package == ID("Package")
    assert_type_error(
        ModularInteger("X", Number(256), Location((10, 20))),
        r'^<stdin>:10:20: model: error: unexpected format of type name "X"$',
    )
    assert_type_error(
        ModularInteger("X.Y.Z", Number(256), Location((10, 20))),
        '^<stdin>:10:20: model: error: unexpected format of type name "X.Y.Z"$',
    )


def test_modular_size() -> None:
    assert ModularInteger("P.T", Pow(Number(2), Number(32))).size == Number(32)
    assert ModularInteger("P.T", Pow(Number(2), Number(32))).size_expr == Number(32)


def test_modular_first() -> None:
    mod = ModularInteger("P.T", Pow(Number(2), Number(32)))
    assert mod.first == Number(0)


def test_modular_last() -> None:
    mod = ModularInteger("P.T", Pow(Number(2), Number(32)))
    assert mod.last == Number(2 ** 32 - 1)


def test_modular_invalid_modulus_power_of_two() -> None:
    assert_type_error(
        ModularInteger("P.T", Number(255), Location((65, 3))),
        r'^<stdin>:65:3: model: error: modulus of "T" not power of two$',
    )


def test_modular_invalid_modulus_variable() -> None:
    assert_type_error(
        ModularInteger("P.T", Pow(Number(2), Variable("X")), Location((3, 23))),
        r'^<stdin>:3:23: model: error: modulus of "T" contains variable$',
    )


def test_modular_invalid_modulus_limit() -> None:
    assert_type_error(
        ModularInteger("P.T", Pow(Number(2), Number(128), Location((55, 3)))),
        r'^<stdin>:55:3: model: error: modulus of "T" exceeds limit \(2\*\*64\)$',
    )


def test_range_size() -> None:
    assert_equal(
        RangeInteger("P.T", Number(16), Number(128), Pow(Number(2), Number(5))).size, Number(32),
    )
    assert_equal(
        RangeInteger("P.T", Number(16), Number(128), Pow(Number(2), Number(5))).size_expr,
        Pow(Number(2), Number(5)),
    )


def test_range_first() -> None:
    integer = RangeInteger(
        "P.T", Pow(Number(2), Number(4)), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32)
    )
    assert integer.first == Number(16)
    assert integer.first_expr == Pow(Number(2), Number(4))


def test_range_last() -> None:
    integer = RangeInteger(
        "P.T", Pow(Number(2), Number(4)), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32)
    )
    assert integer.last == Number(2 ** 32 - 1)
    assert integer.last_expr == Sub(Pow(Number(2), Number(32)), Number(1))


def test_range_invalid_first_variable() -> None:
    assert_type_error(
        RangeInteger("P.T", Add(Number(1), Variable("X")), Number(15), Number(4), Location((5, 3))),
        r'^<stdin>:5:3: model: error: first of "T" contains variable$',
    )


def test_range_invalid_last_variable() -> None:
    assert_type_error(
        RangeInteger("P.T", Number(1), Add(Number(1), Variable("X")), Number(4), Location((80, 6))),
        r'^<stdin>:80:6: model: error: last of "T" contains variable$',
    )


def test_range_invalid_last_exceeds_limit() -> None:
    assert_type_error(
        RangeInteger("P.T", Number(1), Pow(Number(2), Number(63)), Number(64)),
        r'^model: error: last of "T" exceeds limit \(2\*\*63 - 1\)$',
    )


def test_range_invalid_first_negative() -> None:
    assert_type_error(
        RangeInteger("P.T", Number(-1), Number(0), Number(1), Location((6, 4))),
        r'^<stdin>:6:4: model: error: first of "T" negative$',
    )


def test_range_invalid_range() -> None:
    assert_type_error(
        RangeInteger("P.T", Number(1), Number(0), Number(1), Location((10, 5))),
        r'^<stdin>:10:5: model: error: range of "T" negative$',
    )


def test_range_invalid_size_variable() -> None:
    assert_type_error(
        RangeInteger(
            "P.T", Number(0), Number(256), Add(Number(8), Variable("X")), Location((22, 4))
        ),
        r'^<stdin>:22:4: model: error: size of "T" contains variable$',
    )


def test_range_invalid_size_too_small() -> None:
    assert_type_error(
        RangeInteger("P.T", Number(0), Number(256), Number(8), Location((10, 4))),
        r'^<stdin>:10:4: model: error: size of "T" too small$',
    )


def test_range_invalid_size_exceeds_limit() -> None:
    # ISSUE: Componolit/RecordFlux#238
    assert_type_error(
        RangeInteger("P.T", Number(0), Number(256), Number(128), Location((50, 3))),
        r'^<stdin>:50:3: model: error: size of "T" exceeds limit \(2\*\*64\)$',
    )


def test_enumeration_size() -> None:
    assert_equal(
        Enumeration(
            "P.T", [("A", Number(1))], Pow(Number(2), Number(5)), False, Location((34, 3))
        ).size,
        Number(32),
    )
    assert_equal(
        Enumeration(
            "P.T", [("A", Number(1))], Pow(Number(2), Number(5)), False, Location((34, 3))
        ).size_expr,
        Pow(Number(2), Number(5)),
    )


def test_enumeration_invalid_size_variable() -> None:
    assert_type_error(
        Enumeration(
            "P.T", [("A", Number(1))], Add(Number(8), Variable("X")), False, Location((34, 3))
        ),
        r'^<stdin>:34:3: model: error: size of "T" contains variable$',
    )


def test_enumeration_invalid_literal_value() -> None:
    assert_type_error(
        Enumeration("P.T", [("A", Number(2 ** 63))], Number(64), False, Location((10, 5))),
        r'^<stdin>:10:5: model: error: enumeration value of "T"'
        r" outside of permitted range \(0 .. 2\*\*63 - 1\)$",
    )


def test_enumeration_invalid_size_too_small() -> None:
    assert_type_error(
        Enumeration("P.T", [("A", Number(256))], Number(8), False, Location((10, 5))),
        r'^<stdin>:10:5: model: error: size of "T" too small$',
    )


def test_enumeration_invalid_size_exceeds_limit() -> None:
    assert_type_error(
        Enumeration("P.T", [("A", Number(256))], Number(128), False, Location((8, 20))),
        r'^<stdin>:8:20: model: error: size of "T" exceeds limit \(2\*\*64\)$',
    )


def test_enumeration_invalid_always_valid_aspect() -> None:
    with pytest.raises(
        RecordFluxError, match=r'^model: error: unnecessary always-valid aspect on "T"$'
    ):
        Enumeration("P.T", [("A", Number(0)), ("B", Number(1))], Number(1), True).error.propagate()


def test_enumeration_invalid_literal() -> None:
    assert_type_error(
        Enumeration("P.T", [("A B", Number(1))], Number(8), False, Location(((1, 2)))),
        r'^<stdin>:1:2: model: error: invalid literal name "A B" in "T"$',
    )
    assert_type_error(
        Enumeration("P.T", [("A.B", Number(1))], Number(8), False, Location((6, 4))),
        r'^<stdin>:6:4: model: error: invalid literal name "A.B" in "T"$',
    )


def test_array_invalid_element_type() -> None:
    assert_type_error(
        Array("P.A", Array("P.B", MODULAR_INTEGER, Location((3, 4))), Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" must be scalar or non-null message$',
    )
    assert_type_error(
        Array("P.A", Message("P.B", [], {}, location=Location((3, 4))), Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" must be scalar or non-null message$',
    )
    assert_type_error(
        Array("P.A", OPAQUE, Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'__BUILTINS__:0:0: model: info: type "Opaque" must be scalar or non-null message$',
    )


def test_array_unsupported_element_type() -> None:
    assert_type_error(
        Array(
            "P.A",
            ModularInteger("P.B", Pow(Number(2), Number(4)), Location((3, 4))),
            Location((5, 4)),
        ),
        r'^<stdin>:5:4: model: error: unsupported element type size of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" has size 4, must be multiple of 8$',
    )
    assert_type_error(
        Array("P.A", BOOLEAN, Location((5, 4))),
        r'^<stdin>:5:4: model: error: unsupported element type size of array "A"\n'
        r'__BUILTINS__:0:0: model: info: type "Boolean" has size 1, must be multiple of 8$',
    )


def test_message_incorrect_name() -> None:
    with pytest.raises(
        RecordFluxError, match='^<stdin>:10:8: model: error: unexpected format of type name "M"$'
    ):
        Message("M", [], {}, location=Location((10, 8)))


def test_message_missing_type() -> None:
    x = Field(ID("X", Location((5, 6))))
    structure = [Link(INITIAL, x), Link(x, FINAL)]

    assert_message_model_error(
        structure, {}, '^<stdin>:5:6: model: error: missing type for field "X" in "P.M"$',
    )


def test_message_unused_type() -> None:
    t = ModularInteger("P.T", Number(2))

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL),
    ]

    types = {Field("X"): t, Field(ID("Y", Location((5, 6)))): t}

    assert_message_model_error(
        structure, types, '^<stdin>:5:6: model: error: unused field "Y" in "P.M"$'
    )


def test_message_ambiguous_first_field() -> None:
    t = ModularInteger("P.T", Number(2))

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
        '^<stdin>:10:8: model: error: ambiguous first field in "P.M"\n'
        "<stdin>:2:6: model: info: duplicate\n"
        "<stdin>:3:6: model: info: duplicate",
        location=Location((10, 8)),
    )


def test_message_name_conflict_field_enum() -> None:
    t = Enumeration(
        "P.T",
        [(ID("X", Location((3, 27))), Number(1)), (ID("Y", Location((3, 32))), Number(2))],
        Number(8),
        False,
    )

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL),
    ]

    types = {Field(ID("X", Location((5, 6)))): t}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:5:6: model: error: name conflict for field "X" in "P.M"\n'
        "<stdin>:3:27: model: info: conflicting enumeration literal",
    )


def test_message_duplicate_link() -> None:
    t = ModularInteger("P.T", Number(2))
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
        f"<stdin>:5:42: model: info: duplicate link",
    )


def test_message_multiple_duplicate_links() -> None:
    t = ModularInteger("P.T", Number(2))
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
        f"<stdin>:6:22: model: info: duplicate link",
    )


def test_message_unsupported_expression() -> None:
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

    types = {x: MODULAR_INTEGER}

    assert_message_model_error(
        structure,
        types,
        '^<stdin>:10:19: model: error: unsupported expression in "P.M"\n'
        '<stdin>:10:23: model: info: variable "X" in exponent',
    )


def test_message_unreachable_field() -> None:
    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), Field("Z")),
        Link(Field(ID("Y", Location((20, 3)))), Field("Z")),
        Link(Field("Z"), FINAL),
    ]

    types = {Field("X"): BOOLEAN, Field("Y"): BOOLEAN, Field("Z"): BOOLEAN}

    assert_message_model_error(
        structure, types, '^<stdin>:20:3: model: error: unreachable field "Y" in "P.M"$',
    )


def test_message_cycle() -> None:
    t = ModularInteger("P.T", Number(2))

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
        '^<stdin>:10:8: model: error: structure of "P.M" contains cycle',
        # ISSUE: Componolit/RecordFlux#256
        # '\n'
        # '<stdin>:3:5: model: info: field "X" links to "Y"\n'
        # '<stdin>:4:5: model: info: field "Y" links to "Z"\n'
        # '<stdin>:5:5: model: info: field "Z" links to "X"\n',
        location=Location((10, 8)),
    )


def test_message_fields() -> None:
    assert_equal(
        ETHERNET_FRAME.fields,
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


def test_message_definite_fields() -> None:
    assert_equal(
        ETHERNET_FRAME.definite_fields,
        (
            Field("Destination"),
            Field("Source"),
            Field("Type_Length_TPID"),
            Field("Type_Length"),
            Field("Payload"),
        ),
    )


def test_message_field_condition() -> None:
    assert_equal(ETHERNET_FRAME.field_condition(INITIAL), TRUE)
    assert_equal(
        ETHERNET_FRAME.field_condition(Field("TPID")),
        Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
    )
    assert_equal(
        ETHERNET_FRAME.field_condition(Field("Type_Length")),
        Or(
            Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
            NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
        ),
    )
    assert_equal(
        ETHERNET_FRAME.field_condition(Field("Payload")),
        Or(
            And(
                Or(
                    Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
                    NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
                ),
                LessEqual(Variable("Type_Length"), Number(1500)),
            ),
            And(
                Or(
                    Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
                    NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
                ),
                GreaterEqual(Variable("Type_Length"), Number(1536)),
            ),
        ),
    )


def test_message_incoming() -> None:
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
                    GreaterEqual(Div(Length("Payload"), Number(8)), Number(46)),
                    LessEqual(Div(Length("Payload"), Number(8)), Number(1500)),
                ),
            )
        ],
    )


def test_message_outgoing() -> None:
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


def test_message_direct_predecessors() -> None:
    assert_equal(ETHERNET_FRAME.direct_predecessors(INITIAL), [])
    assert_equal(
        ETHERNET_FRAME.direct_predecessors(Field("Type_Length")),
        [Field("TCI"), Field("Type_Length_TPID")],
    )
    assert_equal(ETHERNET_FRAME.direct_predecessors(FINAL), [Field("Payload")])


def test_message_direct_successors() -> None:
    assert_equal(ETHERNET_FRAME.direct_successors(INITIAL), [Field("Destination")])
    assert_equal(ETHERNET_FRAME.direct_successors(Field("Type_Length")), [Field("Payload")])
    assert_equal(ETHERNET_FRAME.direct_successors(FINAL), [])


def test_message_definite_predecessors() -> None:
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


def test_message_predecessors() -> None:
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


def test_message_successors() -> None:
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


def test_message_nonexistent_variable() -> None:
    mod_type = ModularInteger("P.MT", Pow(Number(2), Number(32)))
    enum_type = Enumeration("P.ET", [("Val1", Number(0)), ("Val2", Number(1))], Number(8), True)
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            Field("F2"),
            Equal(Variable("F1"), Variable("Val3", location=Location((444, 55)))),
        ),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): enum_type, Field("F2"): mod_type}
    assert_message_model_error(
        structure, types, '^<stdin>:444:55: model: error: undefined variable "Val3" referenced$',
    )


def test_message_subsequent_variable() -> None:
    f1 = Field("F1")
    f2 = Field("F2")
    t = ModularInteger("P.T", Pow(Number(2), Number(32)))
    structure = [
        Link(INITIAL, f1),
        Link(f1, f2, Equal(Variable("F2", location=Location((1024, 57))), Number(42))),
        Link(f2, FINAL),
    ]

    types = {Field("F1"): t, Field("F2"): t}
    assert_message_model_error(
        structure, types, '^<stdin>:1024:57: model: error: subsequent field "F2" referenced',
    )


def test_message_invalid_use_of_length_attribute() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, Equal(Length("F1"), Number(32), Location((400, 17)))),
    ]
    types = {Field("F1"): MODULAR_INTEGER}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:400:17: model: error: invalid use of length attribute for "F1"$',
    )


def test_message_invalid_relation_to_aggregate() -> None:
    structure = [
        Link(INITIAL, Field("F1"), length=Number(16)),
        Link(
            Field("F1"),
            FINAL,
            LessEqual(Variable("F1"), Aggregate(Number(1), Number(2)), Location((100, 20))),
        ),
    ]
    types = {Field("F1"): Opaque()}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:100:20: model: error: invalid relation " <= " between Opaque and Aggregate$',
    )


def test_message_invalid_element_in_relation_to_aggregate() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(
            Field("F1"),
            FINAL,
            Equal(Variable("F1"), Aggregate(Number(1), Number(2)), Location((14, 7))),
        ),
    ]
    types = {Field("F1"): MODULAR_INTEGER}
    assert_message_model_error(
        structure,
        types,
        r'^<stdin>:14:7: model: error: invalid relation " = " '
        r"between Aggregate and ModularInteger$",
    )


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
            {ID("F2"): [Variable("F1")], ID("F3"): [Variable("F1"), Length("F2")]},
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
def test_message_checksum(checksums: Mapping[ID, Sequence[Expr]], condition: Expr) -> None:
    f1 = Field("F1")
    f2 = Field("F2")
    f3 = Field("F3")
    structure = [
        Link(INITIAL, f1),
        Link(f1, f2),
        Link(f2, f3),
        Link(f3, FINAL, condition),
    ]
    types = {f1: MODULAR_INTEGER, f2: MODULAR_INTEGER, f3: MODULAR_INTEGER}
    aspects = {
        ID("Checksum"): checksums,
    }
    message = Message("P.M", structure, types, aspects=aspects)
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
def test_message_checksum_error(
    checksums: Mapping[ID, Sequence[Expr]], condition: Expr, error: str
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
    types = {f1: MODULAR_INTEGER, f2: MODULAR_INTEGER, f3: MODULAR_INTEGER}
    aspects = {
        ID("Checksum"): checksums,
    }
    assert_message_model_error(structure, types, error, aspects=aspects)


def test_message_field_size() -> None:
    message = Message(
        "P.M",
        [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
        {Field("F"): MODULAR_INTEGER},
        location=Location((30, 10)),
    )

    assert message.field_size(FINAL) == Number(0)
    assert message.field_size(Field("F")) == Number(8)

    with pytest.raises(AssertionError, match='^field "X" not found$'):
        message.field_size(Field("X"))
        message.error.propagate()


def test_message_copy() -> None:
    message = Message(
        "P.M", [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)], {Field("F"): MODULAR_INTEGER},
    )
    assert_equal(
        message.copy(identifier="A.B"),
        Message(
            "A.B",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): MODULAR_INTEGER},
        ),
    )
    assert_equal(
        message.copy(
            structure=[Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            types={Field("C"): RANGE_INTEGER},
        ),
        Message(
            "P.M",
            [Link(INITIAL, Field("C")), Link(Field("C"), FINAL)],
            {Field("C"): RANGE_INTEGER},
        ),
    )


def test_message_proven() -> None:
    message = Message(
        "P.M", [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)], {Field("F"): MODULAR_INTEGER},
    )
    assert message.proven() == message


def test_message_is_possibly_empty() -> None:
    a = Field("A")
    b = Field("B")
    c = Field("C")

    array = Array("P.Array", MODULAR_INTEGER)

    message = Message(
        "P.M",
        [
            Link(INITIAL, a),
            Link(a, c, condition=Less(Variable("A"), Number(10)), length=Variable("A")),
            Link(a, b, condition=Greater(Variable("A"), Number(20)), length=Variable("A")),
            Link(b, c, length=Variable("A")),
            Link(c, FINAL),
        ],
        {a: MODULAR_INTEGER, b: array, c: array},
    )

    assert not message.is_possibly_empty(a)
    assert not message.is_possibly_empty(b)
    assert message.is_possibly_empty(c)


def test_derived_message_incorrect_base_name() -> None:
    with pytest.raises(
        RecordFluxError, match='^<stdin>:40:8: model: error: unexpected format of type name "M"$'
    ):
        DerivedMessage("P.M", Message("M", [], {}, location=Location((40, 8))))


def test_derived_message_proven() -> None:
    message = DerivedMessage(
        "P.M",
        Message(
            "X.M",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): MODULAR_INTEGER},
        ),
    )
    assert message.proven() == message


def test_prefixed_message() -> None:
    assert_equal(
        UnprovenMessage(
            "P.M",
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
                Link(Field("F3"), Field("F4"), length=Variable("F3")),
                Link(Field("F4"), FINAL),
            ],
            {
                Field("F1"): deepcopy(MODULAR_INTEGER),
                Field("F2"): deepcopy(BOOLEAN),
                Field("F3"): deepcopy(RANGE_INTEGER),
                Field("F4"): Opaque(),
            },
        ).prefixed("X_"),
        UnprovenMessage(
            "P.M",
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
                Link(Field("X_F3"), Field("X_F4"), length=Variable("X_F3")),
                Link(Field("X_F4"), FINAL),
            ],
            {
                Field("X_F1"): deepcopy(MODULAR_INTEGER),
                Field("X_F2"): deepcopy(BOOLEAN),
                Field("X_F3"): deepcopy(RANGE_INTEGER),
                Field("X_F4"): Opaque(),
            },
        ),
    )


def test_merge_message_simple() -> None:
    assert_equal(
        deepcopy(M_SMPL_REF).merged(),
        UnprovenMessage(
            "P.Smpl_Ref",
            [
                Link(INITIAL, Field("NR_F1"), length=Number(16)),
                Link(Field("NR_F3"), FINAL, Equal(Variable("NR_F3"), Variable("P.ONE"))),
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
                Field("NR_F1"): Opaque(),
                Field("NR_F2"): deepcopy(MODULAR_INTEGER),
                Field("NR_F3"): deepcopy(ENUMERATION),
                Field("NR_F4"): deepcopy(RANGE_INTEGER),
            },
        ),
    )


def test_merge_message_complex() -> None:
    assert_equal(
        deepcopy(M_CMPLX_REF).merged(),
        UnprovenMessage(
            "P.Cmplx_Ref",
            [
                Link(INITIAL, Field("F1")),
                Link(Field("F1"), Field("F2"), LessEqual(Variable("F1"), Number(100))),
                Link(Field("F1"), Field("F3"), GreaterEqual(Variable("F1"), Number(200))),
                Link(
                    Field("F2"),
                    Field("NR_F1"),
                    LessEqual(Variable("F1"), Number(10)),
                    length=Number(16),
                ),
                Link(
                    Field("F3"),
                    Field("NR_F1"),
                    GreaterEqual(Variable("F1"), Number(220)),
                    length=Number(16),
                ),
                Link(
                    Field("NR_F3"),
                    Field("F5"),
                    And(
                        LessEqual(Variable("F1"), Number(100)),
                        Equal(Variable("NR_F3"), Variable("P.ONE")),
                    ),
                ),
                Link(Field("NR_F4"), Field("F5"), LessEqual(Variable("F1"), Number(100))),
                Link(
                    Field("NR_F3"),
                    Field("F6"),
                    And(
                        GreaterEqual(Variable("F1"), Number(200)),
                        Equal(Variable("NR_F3"), Variable("P.ONE")),
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
                Field("F1"): deepcopy(MODULAR_INTEGER),
                Field("F2"): deepcopy(MODULAR_INTEGER),
                Field("F3"): deepcopy(RANGE_INTEGER),
                Field("NR_F1"): Opaque(),
                Field("NR_F2"): deepcopy(MODULAR_INTEGER),
                Field("NR_F3"): deepcopy(ENUMERATION),
                Field("NR_F4"): deepcopy(RANGE_INTEGER),
                Field("F5"): deepcopy(MODULAR_INTEGER),
                Field("F6"): deepcopy(RANGE_INTEGER),
            },
        ),
    )


def test_merge_message_recursive() -> None:
    assert_equal(
        deepcopy(M_DBL_REF).merged(),
        UnprovenMessage(
            "P.Dbl_Ref",
            [
                Link(INITIAL, Field("SR_NR_F1"), length=Number(16)),
                Link(
                    Field("SR_NR_F3"),
                    Field("NR_F1"),
                    Equal(Variable("SR_NR_F3"), Variable("P.ONE")),
                    length=Number(16),
                ),
                Link(Field("SR_NR_F4"), Field("NR_F1"), length=Number(16)),
                Link(Field("NR_F3"), FINAL, Equal(Variable("NR_F3"), Variable("P.ONE"))),
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
                Field("SR_NR_F1"): Opaque(),
                Field("SR_NR_F2"): deepcopy(MODULAR_INTEGER),
                Field("SR_NR_F3"): deepcopy(ENUMERATION),
                Field("SR_NR_F4"): deepcopy(RANGE_INTEGER),
                Field("NR_F1"): Opaque(),
                Field("NR_F2"): deepcopy(MODULAR_INTEGER),
                Field("NR_F3"): deepcopy(ENUMERATION),
                Field("NR_F4"): deepcopy(RANGE_INTEGER),
            },
        ),
    )


def test_merge_message_simple_derived() -> None:
    assert_equal(
        deepcopy(M_SMPL_REF_DERI).merged(),
        UnprovenDerivedMessage(
            "P.Smpl_Ref_Deri",
            M_SMPL_REF,
            [
                Link(INITIAL, Field("NR_F1"), length=Number(16)),
                Link(Field("NR_F3"), FINAL, Equal(Variable("NR_F3"), Variable("P.ONE"))),
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
                Field("NR_F1"): Opaque(),
                Field("NR_F2"): deepcopy(MODULAR_INTEGER),
                Field("NR_F3"): deepcopy(ENUMERATION),
                Field("NR_F4"): deepcopy(RANGE_INTEGER),
            },
        ),
    )


def test_merge_message_constrained() -> None:
    m1 = UnprovenMessage(
        "P.M1",
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
        "P.M2",
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
        "P.M2",
        [
            Link(INITIAL, Field("F4_F1"),),
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
        "P.M1",
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), Equal(Variable("F1"), Variable("True"))),
            Link(Field("F1"), FINAL, Equal(Variable("F1"), Variable("False"))),
            Link(Field("F2"), FINAL, Equal(Variable("F2"), Variable("True"))),
        ],
        {Field("F1"): BOOLEAN, Field("F2"): BOOLEAN},
    )
    m2 = UnprovenMessage(
        "P.M2",
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
        RecordFluxError, match=r'^model: error: empty message type when merging field "F3"$',
    ):
        m2.merged()


def test_merge_message_error_name_conflict() -> None:
    m2_f2 = Field(ID("F2", Location((10, 5))))

    m2 = UnprovenMessage(
        "P.M2",
        [Link(INITIAL, m2_f2), Link(m2_f2, FINAL)],
        {Field("F2"): MODULAR_INTEGER},
        location=Location((15, 3)),
    )

    m1_f1 = Field(ID("F1", Location((20, 8))))
    m1_f1_f2 = Field(ID("F1_F2", Location((30, 5))))

    m1 = UnprovenMessage(
        "P.M1",
        [Link(INITIAL, m1_f1), Link(m1_f1, m1_f1_f2), Link(m1_f1_f2, FINAL)],
        {Field("F1"): m2, Field("F1_F2"): MODULAR_INTEGER},
        location=Location((2, 9)),
    )

    assert_type_error(
        m1.merged(),
        r"^"
        r'<stdin>:30:5: model: error: name conflict for "F1_F2" in "P.M1"\n'
        r'<stdin>:15:3: model: info: when merging message "P.M2"\n'
        r'<stdin>:20:8: model: info: into field "F1"$',
    )


def test_refinement_invalid_package() -> None:
    assert_type_error(
        Refinement(ID("A.B", Location((22, 10))), ETHERNET_FRAME, Field("Payload"), ETHERNET_FRAME),
        r'^<stdin>:22:10: model: error: unexpected format of package name "A.B"$',
    )


def test_refinement_invalid_field_type() -> None:
    x = Field(ID("X", Location((20, 10))))

    message = Message("P.M", [Link(INITIAL, x), Link(x, FINAL)], {x: MODULAR_INTEGER})

    assert_type_error(
        Refinement("P", message, Field(ID("X", Location((33, 22)))), message),
        r'^<stdin>:33:22: model: error: invalid type of field "X" in refinement of "P.M"\n'
        r"<stdin>:20:10: model: info: expected field of type Opaque",
    )


def test_refinement_invalid_field() -> None:
    message = Message("P.M", [], {})

    assert_type_error(
        Refinement("P", message, Field(ID("X", Location((33, 22)))), message),
        r'^<stdin>:33:22: model: error: invalid field "X" in refinement of "P.M"$',
    )


def test_field_locations() -> None:
    f2 = Field(ID("F2", Location((2, 2))))
    f3 = Field(ID("F3", Location((3, 2))))

    message = UnprovenMessage(
        "P.M",
        [Link(INITIAL, f2), Link(f2, f3), Link(f3, FINAL)],
        {Field("F2"): MODULAR_INTEGER, Field("F3"): MODULAR_INTEGER},
        location=Location((17, 9)),
    )
    assert message.fields == (f2, f3)


def test_opaque_aggregate_out_of_range() -> None:
    f = Field("F")
    with pytest.raises(
        RecordFluxError,
        match=r"^<stdin>:44:3: model: error: aggregate element out of range 0 .. 255",
    ):
        Message(
            "P.M",
            [
                Link(INITIAL, f, length=Number(24)),
                Link(
                    f,
                    FINAL,
                    condition=Equal(
                        Variable("F"),
                        Aggregate(Number(1), Number(2), Number(256, location=Location((44, 3)))),
                    ),
                ),
            ],
            {Field("F"): Opaque()},
        )


def test_array_aggregate_out_of_range() -> None:
    array_type = Array("P.Array", ModularInteger("P.Element", Number(64)))

    f = Field("F")
    with pytest.raises(
        RecordFluxError,
        match=r"^<stdin>:44:3: model: error: aggregate element out of range 0 .. 63",
    ):
        Message(
            "P.M",
            [
                Link(INITIAL, f, length=Number(18)),
                Link(
                    f,
                    FINAL,
                    condition=Equal(
                        Variable("F"),
                        Aggregate(Number(1), Number(2), Number(64, location=Location((44, 3)))),
                    ),
                ),
            ],
            {Field("F"): array_type},
        )


def test_array_aggregate_invalid_element_type() -> None:
    inner = Message(
        "P.I", [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)], {Field("F"): MODULAR_INTEGER},
    )
    array_type = Array("P.Array", inner)

    f = Field("F")
    with pytest.raises(
        RecordFluxError,
        match=r"^<stdin>:90:10: model: error: invalid array element type"
        ' "P.I" for aggregate comparison$',
    ):
        Message(
            "P.M",
            [
                Link(INITIAL, f, length=Number(18)),
                Link(
                    f,
                    FINAL,
                    condition=Equal(
                        Variable("F"),
                        Aggregate(Number(1), Number(2), Number(64)),
                        Location((90, 10)),
                    ),
                ),
            ],
            {Field("F"): array_type},
        )


class NewType(Type):
    pass


@pytest.mark.skipif(not __debug__, reason="depends on contract")
def test_invalid_message_field_type() -> None:
    with pytest.raises(AssertionError, match=r"rflx/model/message.py"):
        Message(
            "P.M", [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)], {Field("F"): NewType("T")},
        )


def test_invalid_enumeration_type_duplicate_elements() -> None:
    assert_type_error(
        Enumeration(
            "P.T",
            [(ID("Foo", Location((3, 27))), Number(1)), (ID("Foo", Location((3, 32))), Number(2))],
            Number(1),
            False,
        ),
        r'<stdin>:3:32: model: error: duplicate literal "Foo"\n'
        r"<stdin>:3:27: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_multiple_duplicate_elements() -> None:
    assert_type_error(
        Enumeration(
            "P.T",
            [
                (ID("Foo", Location((3, 27))), Number(1)),
                (ID("Bar", Location((3, 32))), Number(2)),
                (ID("Foo", Location((3, 37))), Number(3)),
                (ID("Bar", Location((3, 42))), Number(4)),
            ],
            Number(2),
            False,
        ),
        r'<stdin>:3:37: model: error: duplicate literal "Foo"\n'
        r"<stdin>:3:27: model: info: previous occurrence\n"
        r'<stdin>:3:42: model: error: duplicate literal "Bar"\n'
        r"<stdin>:3:32: model: info: previous occurrence",
    )


def test_conflicting_literal_builtin_type() -> None:
    assert_model_error(
        [
            Enumeration(
                "P.T",
                [
                    (ID("E1", Location((3, 27))), Number(1)),
                    (ID("Boolean", Location((3, 31))), Number(2)),
                ],
                Number(8),
                False,
            ),
        ],
        r'<stdin>:3:31: model: error: literal conflicts with type "Boolean"\n'
        r"__BUILTINS__:0:0: model: info: conflicting type declaration",
    )


def test_name_conflict_between_literal_and_type() -> None:
    assert_model_error(
        [
            Enumeration(
                "P.T",
                [
                    (ID("Foo", Location((3, 27))), Number(1)),
                    (ID("Bar", Location((3, 32))), Number(2)),
                ],
                Number(1),
                False,
            ),
            ModularInteger("T.Foo", Number(256), Location((4, 16))),
            ModularInteger("T.Bar", Number(256), Location((5, 16))),
        ],
        r'<stdin>:3:32: model: error: literal conflicts with type "Bar"\n'
        r"<stdin>:5:16: model: info: conflicting type declaration\n"
        r'<stdin>:3:27: model: error: literal conflicts with type "Foo"\n'
        r"<stdin>:4:16: model: info: conflicting type declaration",
    )


def test_invalid_enumeration_type_builtin_literals() -> None:
    assert_model_error(
        [
            Enumeration(
                "P.T",
                [("True", Number(1)), ("False", Number(2))],
                Number(1),
                False,
                Location((3, 16)),
            ),
        ],
        r"<stdin>:3:16: model: error: conflicting literals: False, True\n"
        r'__BUILTINS__:0:0: model: info: previous occurrence of "False"\n'
        r'__BUILTINS__:0:0: model: info: previous occurrence of "True"',
    )


def test_invalid_enumeration_type_identical_literals() -> None:
    assert_model_error(
        [
            Enumeration(
                "P.T1",
                [("Foo", Number(1)), (ID("Bar", Location((3, 33))), Number(2))],
                Number(1),
                False,
            ),
            Enumeration(
                "P.T2",
                [("Bar", Number(1)), ("Baz", Number(2))],
                Number(1),
                False,
                Location((4, 16)),
            ),
        ],
        r"<stdin>:4:16: model: error: conflicting literals: Bar\n"
        r'<stdin>:3:33: model: info: previous occurrence of "Bar"',
    )


def test_opaque_not_byte_aligned() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: opaque field "O" not aligned to'
        r" 8 bit boundary [(]P -> O[)]",
    ):
        o = Field(ID("O", location=Location((44, 3))))
        Message(
            "P.M",
            [Link(INITIAL, Field("P")), Link(Field("P"), o, length=Number(128)), Link(o, FINAL)],
            {Field("P"): ModularInteger("P.T", Number(4)), o: Opaque()},
        )


def test_opaque_not_byte_aligned_dynamic() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: opaque field "O2" not aligned to'
        r" 8 bit boundary [(]L1 -> O1 -> L2 -> O2[)]",
    ):
        o2 = Field(ID("O2", location=Location((44, 3))))
        Message(
            "P.M",
            [
                Link(INITIAL, Field("L1")),
                Link(
                    Field("L1"),
                    Field("O1"),
                    length=Variable("L1"),
                    condition=Equal(Mod(Variable("L1"), Number(8)), Number(0)),
                ),
                Link(Field("O1"), Field("L2")),
                Link(Field("L2"), o2, length=Number(128)),
                Link(o2, FINAL),
            ],
            {
                Field("L1"): MODULAR_INTEGER,
                Field("L2"): ModularInteger("P.T", Number(4)),
                Field("O1"): Opaque(),
                o2: Opaque(),
            },
        )


def test_opaque_valid_byte_aligned_dynamic_mul() -> None:
    Message(
        "P.M",
        [
            Link(INITIAL, Field("L")),
            Link(Field("L"), Field("O1"), length=Mul(Number(8), Variable("L"))),
            Link(Field("O1"), FINAL),
        ],
        {Field("L"): MODULAR_INTEGER, Field("O1"): Opaque()},
    )


def test_opaque_valid_byte_aligned_dynamic_cond() -> None:
    Message(
        "P.M",
        [
            Link(INITIAL, Field("L")),
            Link(
                Field("L"),
                Field("O1"),
                length=Variable("L"),
                condition=Equal(Mod(Variable("L"), Number(8)), Number(0)),
            ),
            Link(Field("O1"), Field("O2"), length=Number(128)),
            Link(Field("O2"), FINAL),
        ],
        {Field("L"): MODULAR_INTEGER, Field("O1"): Opaque(), Field("O2"): Opaque()},
    )


def test_opaque_length_not_multiple_of_8() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: length of opaque field "O"'
        " not multiple of 8 bit [(]O[)]",
    ):
        o = Field(ID("O", location=Location((44, 3))))
        Message(
            "P.M", [Link(INITIAL, o, length=Number(68)), Link(o, FINAL)], {o: Opaque()},
        )


def test_opaque_length_not_multiple_of_8_dynamic() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:44:3: model: error: length of opaque field "O" not multiple of 8 bit'
        " [(]L -> O[)]",
    ):
        o = Field(ID("O", location=Location((44, 3))))
        Message(
            "P.M",
            [Link(INITIAL, Field("L")), Link(Field("L"), o, length=Variable("L")), Link(o, FINAL)],
            {Field("L"): MODULAR_INTEGER, o: Opaque()},
        )


def test_opaque_length_valid_multiple_of_8_dynamic_cond() -> None:
    Message(
        "P.M",
        [
            Link(INITIAL, Field("L")),
            Link(
                Field("L"),
                Field("O"),
                length=Variable("L"),
                condition=Equal(Mod(Variable("L"), Number(8)), Number(0)),
            ),
            Link(Field("O"), FINAL),
        ],
        {Field("L"): MODULAR_INTEGER, Field("O"): Opaque()},
    )


def test_session_str() -> None:
    assert_equal(
        str(
            Session(
                "Session",
                "A",
                "B",
                [
                    State(
                        "A",
                        declarations=[decl.VariableDeclaration("Z", "Boolean", Variable("Y"))],
                        actions=[stmt.Assignment("Z", Number(1))],
                        transitions=[
                            Transition("B", condition=Equal(Variable("Z"), Number(1))),
                            Transition("A"),
                        ],
                    ),
                    State("B"),
                ],
                [decl.VariableDeclaration("Y", "Boolean", Number(0))],
                [
                    decl.ChannelDeclaration("X", readable=True, writable=True),
                    decl.PrivateDeclaration("T"),
                    decl.SubprogramDeclaration("F", [], "T"),
                ],
            )
        ),
        multilinestr(
            """generic
                  X : Channel with Readable, Writable;
                  type T is private;
                  with function F return T;
               session Session with
                  Initial => A,
                  Final => B
               is
                  Y : Boolean := 0;
               begin
                  state A is
                     Z : Boolean := Y;
                  begin
                     Z := 1;
                  transition
                     then B
                        if Z = 1
                     then A
                  end A;

                  state B is null state;
               end Session"""
        ),
    )
