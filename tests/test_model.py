from copy import deepcopy

import pytest

from rflx.expression import (
    TRUE,
    Add,
    Aggregate,
    And,
    Div,
    Equal,
    First,
    GreaterEqual,
    Length,
    LessEqual,
    NotEqual,
    Number,
    Or,
    Pow,
    Sub,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    DerivedMessage,
    Enumeration,
    Field,
    Link,
    Message,
    ModelError,
    ModularInteger,
    Opaque,
    RangeInteger,
    Refinement,
    UnprovenDerivedMessage,
    UnprovenMessage,
)
from tests.models import ENUMERATION, ETHERNET_FRAME, MODULAR_INTEGER, RANGE_INTEGER
from tests.utils import assert_equal

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
    with pytest.raises(ModelError, match=r'^unexpected format of type name "X"$'):
        ModularInteger("X", Number(256))
    with pytest.raises(ModelError, match=r'^unexpected format of type name "X.Y.Z"$'):
        ModularInteger("X.Y.Z", Number(256))


def test_modular_size() -> None:
    assert ModularInteger("P.T", Pow(Number(2), Number(32))).size == Number(32)


def test_modular_first() -> None:
    mod = ModularInteger("P.T", Pow(Number(2), Number(32)))
    assert mod.first == Number(0)
    assert mod.first.simplified() == Number(0)


def test_modular_last() -> None:
    mod = ModularInteger("P.T", Pow(Number(2), Number(32)))
    assert mod.last == Sub(Pow(Number(2), Number(32)), Number(1))
    assert mod.last.simplified() == Number(2 ** 32 - 1)


def test_modular_invalid_modulus_power_of_two() -> None:
    with pytest.raises(ModelError, match=r'^modulus of "T" not power of two$'):
        ModularInteger("P.T", Number(255))


def test_modular_invalid_modulus_variable() -> None:
    with pytest.raises(ModelError, match=r'^modulus of "T" contains variable$'):
        ModularInteger("P.T", Pow(Number(2), Variable("X")))


def test_modular_invalid_modulus_limit() -> None:
    with pytest.raises(ModelError, match=r'^modulus of "T" exceeds limit \(2\*\*64\)$'):
        ModularInteger("P.T", Pow(Number(2), Number(128)))


def test_range_size() -> None:
    assert_equal(
        RangeInteger("P.T", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32)).size,
        Number(32),
    )


def test_range_invalid_first_variable() -> None:
    with pytest.raises(ModelError, match=r'^first of "T" contains variable$'):
        RangeInteger("P.T", Add(Number(1), Variable("X")), Number(15), Number(4))


def test_range_invalid_last_variable() -> None:
    with pytest.raises(ModelError, match=r'^last of "T" contains variable$'):
        RangeInteger("P.T", Number(1), Add(Number(1), Variable("X")), Number(4))


def test_range_invalid_last_exceeds_limit() -> None:
    with pytest.raises(ModelError, match=r'^last of "T" exceeds limit \(2\*\*63 - 1\)$'):
        RangeInteger("P.T", Number(1), Pow(Number(2), Number(63)), Number(64))


def test_range_invalid_first_negative() -> None:
    with pytest.raises(ModelError, match=r'^first of "T" negative$'):
        RangeInteger("P.T", Number(-1), Number(0), Number(1))


def test_range_invalid_range() -> None:
    with pytest.raises(ModelError, match=r'^range of "T" negative$'):
        RangeInteger("P.T", Number(1), Number(0), Number(1))


def test_range_invalid_size_variable() -> None:
    with pytest.raises(ModelError, match=r'^size of "T" contains variable$'):
        RangeInteger("P.T", Number(0), Number(256), Add(Number(8), Variable("X")))


def test_range_invalid_size_too_small() -> None:
    with pytest.raises(ModelError, match=r'^size for "T" too small$'):
        RangeInteger("P.T", Number(0), Number(256), Number(8))


def test_range_invalid_size_exceeds_limit() -> None:
    with pytest.raises(ModelError, match=r'^size of "T" exceeds limit \(2\*\*64\)$'):
        RangeInteger("P.T", Number(0), Number(256), Number(128))


def test_enumeration_invalid_size_variable() -> None:
    with pytest.raises(ModelError, match=r'^size of "T" contains variable$'):
        Enumeration("P.T", {"A": Number(1)}, Add(Number(8), Variable("X")), False)


def test_enumeration_invalid_size_too_small() -> None:
    with pytest.raises(ModelError, match=r'^size for "T" too small$'):
        Enumeration("P.T", {"A": Number(256)}, Number(8), False)


def test_enumeration_invalid_size_exceeds_limit() -> None:
    with pytest.raises(ModelError, match=r'^size of "T" exceeds limit \(2\*\*64\)$'):
        Enumeration("P.T", {"A": Number(256)}, Number(128), False)


def test_enumeration_invalid_always_valid_aspect() -> None:
    with pytest.raises(ModelError, match=r'^unnecessary always-valid aspect on "T"$'):
        Enumeration("P.T", {"A": Number(0), "B": Number(1)}, Number(1), True)


def test_enumeration_invalid_literal() -> None:
    with pytest.raises(ModelError, match=r'^invalid literal name "A B" in "T"$'):
        Enumeration("P.T", {"A B": Number(1)}, Number(8), False)
    with pytest.raises(ModelError, match=r'^invalid literal name "A.B" in "T"$'):
        Enumeration("P.T", {"A.B": Number(1)}, Number(8), False)


def test_message_incorrect_name() -> None:
    with pytest.raises(ModelError, match='^unexpected format of type name "M"$'):
        Message("M", [], {})


def test_message_missing_type() -> None:
    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL),
    ]

    with pytest.raises(ModelError, match='^missing type for field "X" of "P.M"$'):
        Message("P.M", structure, {})


def test_message_superfluous_type() -> None:
    t = ModularInteger("P.T", Number(2))

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL),
    ]

    types = {Field("X"): t, Field("Y"): t}

    with pytest.raises(ModelError, match='^superfluous field "Y" in field types of "P.M"$'):
        Message("P.M", structure, types)


def test_message_ambiguous_first_field() -> None:
    t = ModularInteger("P.T", Number(2))

    structure = [
        Link(INITIAL, Field("X")),
        Link(INITIAL, Field("Y")),
        Link(Field("X"), Field("Z")),
        Link(Field("Y"), Field("Z")),
        Link(Field("Z"), FINAL),
    ]

    types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

    with pytest.raises(ModelError, match='^ambiguous first field in "P.M"$'):
        Message("P.M", structure, types)


def test_message_duplicate_link() -> None:
    t = ModularInteger("P.T", Number(2))

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), FINAL),
        Link(Field("X"), FINAL),
    ]

    types = {Field("X"): t}

    with pytest.raises(ModelError, match=f'^duplicate links in "P.M": X -> {FINAL.name}$'):
        Message("P.M", structure, types)


def test_message_unreachable_field() -> None:
    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), Field("Z")),
        Link(Field("Y"), Field("Z")),
        Link(Field("Z"), FINAL),
    ]

    types = {Field("X"): BOOLEAN, Field("Y"): BOOLEAN, Field("Z"): BOOLEAN}

    with pytest.raises(ModelError, match='^unreachable field "Y" in "P.M"$'):
        Message("P.M", structure, types)


def test_message_cycle() -> None:
    t = ModularInteger("P.T", Number(2))

    structure = [
        Link(INITIAL, Field("X")),
        Link(Field("X"), Field("Y")),
        Link(Field("Y"), Field("Z")),
        Link(Field("Z"), Field("X")),
        Link(Field("X"), FINAL),
    ]

    types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

    with pytest.raises(ModelError, match='^structure of "P.M" contains cycle$'):
        Message("P.M", structure, types)


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
            NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
            Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
        ),
    )
    assert_equal(
        ETHERNET_FRAME.field_condition(Field("Payload")),
        Or(
            And(
                Or(
                    NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
                    Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
                ),
                LessEqual(Variable("Type_Length"), Number(1500)),
            ),
            And(
                Or(
                    NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
                    Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
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
            Link(
                Field("Type_Length_TPID"),
                Field("Type_Length"),
                NotEqual(Variable("Type_Length_TPID"), Number(0x8100, 16)),
                first=First("Type_Length_TPID"),
            ),
            Link(Field("TCI"), Field("Type_Length")),
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
    assert_equal(ETHERNET_FRAME.outgoing(Field("Type_Length")), ETHERNET_FRAME.structure[7:9])
    assert_equal(ETHERNET_FRAME.outgoing(FINAL), [])


def test_message_direct_predecessors() -> None:
    assert_equal(ETHERNET_FRAME.direct_predecessors(INITIAL), [])
    assert_equal(
        ETHERNET_FRAME.direct_predecessors(Field("Type_Length")),
        [Field("Type_Length_TPID"), Field("TCI")],
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
    enum_type = Enumeration("P.ET", {"Val1": Number(0), "Val2": Number(1)}, Number(8), True)
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Equal(Variable("F1"), Variable("Val3"))),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): enum_type, Field("F2"): mod_type}
    with pytest.raises(
        ModelError,
        match='^undefined variable "Val3" referenced in condition 0 from field "F1" to "F2"',
    ):
        Message("P.M", structure, types)


def test_message_subsequent_variable() -> None:
    t = ModularInteger("P.T", Pow(Number(2), Number(32)))
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), Field("F2"), Equal(Variable("F2"), Number(42))),
        Link(Field("F2"), FINAL),
    ]

    types = {Field("F1"): t, Field("F2"): t}
    with pytest.raises(
        ModelError,
        match='^subsequent field "F2" referenced in condition 0 from field "F1" to "F2"',
    ):
        Message("P.M", structure, types)


def test_message_invalid_use_of_length_attribute() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, Equal(Length("F1"), Number(32))),
    ]
    types = {Field("F1"): MODULAR_INTEGER}
    with pytest.raises(
        ModelError,
        match=r'^invalid use of length attribute for "F1" in condition 0'
        r' from field "F1" to "Final" in "P.M"$',
    ):
        Message("P.M", structure, types)


def test_message_invalid_relation_to_aggregate() -> None:
    structure = [
        Link(INITIAL, Field("F1"), length=Number(16)),
        Link(Field("F1"), FINAL, LessEqual(Variable("F1"), Aggregate(Number(1), Number(2)))),
    ]
    types = {Field("F1"): Opaque()}
    with pytest.raises(
        ModelError,
        match=r'^invalid relation " <= " to aggregate in condition 0'
        r' from field "F1" to "Final" in "P.M"$',
    ):
        Message("P.M", structure, types)


def test_message_invalid_element_in_relation_to_aggregate() -> None:
    structure = [
        Link(INITIAL, Field("F1")),
        Link(Field("F1"), FINAL, Equal(Variable("F1"), Aggregate(Number(1), Number(2)))),
    ]
    types = {Field("F1"): MODULAR_INTEGER}
    with pytest.raises(
        ModelError,
        match=r'^invalid relation between "F1" and aggregate in condition 0'
        r' from field "F1" to "Final" in "P.M"$',
    ):
        Message("P.M", structure, types)


def test_message_field_size() -> None:
    message = Message(
        "P.M", [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)], {Field("F"): MODULAR_INTEGER},
    )

    assert message.field_size(FINAL) == Number(0)
    assert message.field_size(Field("F")) == Number(8)

    with pytest.raises(ValueError, match='^field "X" not found$'):
        message.field_size(Field("X"))


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


def test_derived_message_incorrect_base_name() -> None:
    with pytest.raises(ModelError, match='^unexpected format of type name "M"$'):
        DerivedMessage("P.M", Message("M", [], {}))


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
                Link(Field("F2"), FINAL),
                Link(Field("F3"), Field("F4"), length=Variable("F3")),
                Link(Field("F4"), FINAL),
            ],
            {
                Field("F1"): deepcopy(MODULAR_INTEGER),
                Field("F2"): deepcopy(MODULAR_INTEGER),
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
                Link(Field("X_F2"), FINAL),
                Link(Field("X_F3"), Field("X_F4"), length=Variable("X_F3")),
                Link(Field("X_F4"), FINAL),
            ],
            {
                Field("X_F1"): deepcopy(MODULAR_INTEGER),
                Field("X_F2"): deepcopy(MODULAR_INTEGER),
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


def test_merge_message_error_name_conflict() -> None:
    m2 = UnprovenMessage(
        "P.M2",
        [Link(INITIAL, Field("F1")), Link(Field("F1"), FINAL)],
        {Field("F1"): MODULAR_INTEGER},
    )
    m1 = UnprovenMessage(
        "P.M1",
        [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F1_F1")),
            Link(Field("F1_F1"), FINAL),
        ],
        {Field("F1"): m2, Field("F1_F1"): MODULAR_INTEGER},
    )

    with pytest.raises(
        ModelError,
        match=(
            r'^name conflict for "F1_F1" in "P.M1"'
            r' caused by merging message "P.M2" in field "F1"$'
        ),
    ):
        m1.merged()


def test_refinement_invalid_package() -> None:
    with pytest.raises(ModelError, match=r'^unexpected format of package name "A.B"$'):
        Refinement("A.B", ETHERNET_FRAME, Field("Payload"), ETHERNET_FRAME)
