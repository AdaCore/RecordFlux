from copy import deepcopy
from unittest import TestCase

from rflx.expression import (
    TRUE,
    Add,
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


# pylint: disable=too-many-public-methods
class TestModel(TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

        self.addTypeEqualityFunc(Message, self.assert_message)

    def assert_message(self, actual: Message, expected: Message, msg: str = None) -> None:
        msg = f"{expected.full_name} - {msg}" if msg else expected.full_name
        self.assertEqual(actual.full_name, expected.full_name, msg)
        self.assertEqual(actual.structure, expected.structure, msg)
        self.assertEqual(actual.types, expected.types, msg)
        self.assertEqual(actual.fields, expected.fields, msg)

    def test_type_name(self) -> None:
        t = ModularInteger("Package.Type_Name", Number(256))
        self.assertEqual(t.name, "Type_Name")
        self.assertEqual(t.package, ID("Package"))
        with self.assertRaisesRegex(ModelError, r'^unexpected format of type name "X"$'):
            ModularInteger("X", Number(256))
        with self.assertRaisesRegex(ModelError, r'^unexpected format of type name "X.Y.Z"$'):
            ModularInteger("X.Y.Z", Number(256))

    def test_modular_size(self) -> None:
        self.assertEqual(ModularInteger("P.T", Pow(Number(2), Number(64))).size, Number(64))

    def test_modular_first(self) -> None:
        mod = ModularInteger("P.T", Pow(Number(2), Number(64)))
        self.assertEqual(mod.first, Number(0))
        self.assertEqual(mod.first.simplified(), Number(0))

    def test_modular_last(self) -> None:
        mod = ModularInteger("P.T", Pow(Number(2), Number(64)))
        self.assertEqual(mod.last, Sub(Pow(Number(2), Number(64)), Number(1)))
        self.assertEqual(mod.last.simplified(), Number(2 ** 64 - 1))

    def test_modular_invalid_modulus_power_of_two(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^modulus of "T" not power of two$'):
            ModularInteger("P.T", Number(255))

    def test_modular_invalid_modulus_variable(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^modulus of "T" contains variable$'):
            ModularInteger("P.T", Pow(Number(2), Variable("X")))

    def test_modular_invalid_modulus_limit(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^modulus of "T" exceeds limit \(2\*\*64\)$'):
            ModularInteger("P.T", Pow(Number(2), Number(128)))

    def test_range_size(self) -> None:
        self.assertEqual(
            RangeInteger(
                "P.T", Number(0), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32)
            ).size,
            Number(32),
        )

    def test_range_invalid_first_variable(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^first of "T" contains variable$'):
            RangeInteger("P.T", Add(Number(1), Variable("X")), Number(15), Number(4))

    def test_range_invalid_last_variable(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^last of "T" contains variable$'):
            RangeInteger("P.T", Number(1), Add(Number(1), Variable("X")), Number(4))

    def test_range_invalid_first_negative(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^first of "T" negative$'):
            RangeInteger("P.T", Number(-1), Number(0), Number(1))

    def test_range_invalid_range(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^range of "T" negative$'):
            RangeInteger("P.T", Number(1), Number(0), Number(1))

    def test_range_invalid_size_variable(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^size of "T" contains variable$'):
            RangeInteger("P.T", Number(0), Number(256), Add(Number(8), Variable("X")))

    def test_range_invalid_size_too_small(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^size for "T" too small$'):
            RangeInteger("P.T", Number(0), Number(256), Number(8))

    def test_enumeration_invalid_size_variable(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^size of "T" contains variable$'):
            Enumeration("P.T", {"A": Number(1)}, Add(Number(8), Variable("X")), False)

    def test_enumeration_invalid_size_too_small(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^size for "T" too small$'):
            Enumeration("P.T", {"A": Number(256)}, Number(8), False)

    def test_enumeration_invalid_literal(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^invalid literal name "A B" in "T"$'):
            Enumeration("P.T", {"A B": Number(1)}, Number(8), False)
        with self.assertRaisesRegex(ModelError, r'^invalid literal name "A.B" in "T"$'):
            Enumeration("P.T", {"A.B": Number(1)}, Number(8), False)

    def test_message_incorrect_name(self) -> None:
        with self.assertRaisesRegex(ModelError, '^unexpected format of type name "M"$'):
            Message("M", [], {})

    def test_message_missing_type(self) -> None:
        structure = [
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL),
        ]

        with self.assertRaisesRegex(ModelError, '^missing type for field "X" of "P.M"$'):
            Message("P.M", structure, {})

    def test_message_superfluous_type(self) -> None:
        t = ModularInteger("P.T", Number(2))

        structure = [
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL),
        ]

        types = {Field("X"): t, Field("Y"): t}

        with self.assertRaisesRegex(ModelError, '^superfluous field "Y" in field types of "P.M"$'):
            Message("P.M", structure, types)

    def test_message_ambiguous_first_field(self) -> None:
        t = ModularInteger("P.T", Number(2))

        structure = [
            Link(INITIAL, Field("X")),
            Link(INITIAL, Field("Y")),
            Link(Field("X"), Field("Z")),
            Link(Field("Y"), Field("Z")),
            Link(Field("Z"), FINAL),
        ]

        types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

        with self.assertRaisesRegex(ModelError, '^ambiguous first field in "P.M"$'):
            Message("P.M", structure, types)

    def test_message_duplicate_link(self) -> None:
        t = ModularInteger("P.T", Number(2))

        structure = [
            Link(INITIAL, Field("X")),
            Link(Field("X"), FINAL),
            Link(Field("X"), FINAL),
        ]

        types = {Field("X"): t}

        with self.assertRaisesRegex(ModelError, f'^duplicate links in "P.M": X -> {FINAL.name}$'):
            Message("P.M", structure, types)

    def test_message_unreachable_field(self) -> None:
        structure = [
            Link(INITIAL, Field("X")),
            Link(Field("X"), Field("Z")),
            Link(Field("Y"), Field("Z")),
            Link(Field("Z"), FINAL),
        ]

        types = {Field("X"): BOOLEAN, Field("Y"): BOOLEAN, Field("Z"): BOOLEAN}

        with self.assertRaisesRegex(ModelError, '^unreachable field "Y" in "P.M"$'):
            Message("P.M", structure, types)

    def test_message_cycle(self) -> None:
        t = ModularInteger("P.T", Number(2))

        structure = [
            Link(INITIAL, Field("X")),
            Link(Field("X"), Field("Y")),
            Link(Field("Y"), Field("Z")),
            Link(Field("Z"), Field("X")),
            Link(Field("X"), FINAL),
        ]

        types = {Field("X"): t, Field("Y"): t, Field("Z"): t}

        with self.assertRaisesRegex(ModelError, '^structure of "P.M" contains cycle$'):
            Message("P.M", structure, types)

    def test_message_fields(self) -> None:
        self.assertTupleEqual(
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

    def test_message_definite_fields(self) -> None:
        self.assertTupleEqual(
            ETHERNET_FRAME.definite_fields,
            (
                Field("Destination"),
                Field("Source"),
                Field("Type_Length_TPID"),
                Field("Type_Length"),
                Field("Payload"),
            ),
        )

    def test_message_field_condition(self) -> None:
        self.assertEqual(ETHERNET_FRAME.field_condition(INITIAL), TRUE)
        self.assertEqual(
            ETHERNET_FRAME.field_condition(Field("TPID")),
            Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
        )
        self.assertEqual(
            ETHERNET_FRAME.field_condition(Field("Type_Length")),
            Or(
                NotEqual(Variable("Type_Length_TPID"), Number(33024, 16)),
                Equal(Variable("Type_Length_TPID"), Number(33024, 16)),
            ),
        )
        self.assertEqual(
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

    def test_message_incoming(self) -> None:
        self.assertEqual(ETHERNET_FRAME.incoming(INITIAL), [])
        self.assertEqual(
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
        self.assertEqual(
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

    def test_message_outgoing(self) -> None:
        self.assertEqual(ETHERNET_FRAME.outgoing(INITIAL), [Link(INITIAL, Field("Destination"))])
        self.assertEqual(
            ETHERNET_FRAME.outgoing(Field("Type_Length")), ETHERNET_FRAME.structure[7:9]
        )
        self.assertEqual(ETHERNET_FRAME.outgoing(FINAL), [])

    def test_message_direct_predecessors(self) -> None:
        self.assertEqual(ETHERNET_FRAME.direct_predecessors(INITIAL), [])
        self.assertEqual(
            ETHERNET_FRAME.direct_predecessors(Field("Type_Length")),
            [Field("Type_Length_TPID"), Field("TCI")],
        )
        self.assertEqual(ETHERNET_FRAME.direct_predecessors(FINAL), [Field("Payload")])

    def test_message_direct_successors(self) -> None:
        self.assertEqual(ETHERNET_FRAME.direct_successors(INITIAL), [Field("Destination")])
        self.assertEqual(ETHERNET_FRAME.direct_successors(Field("Type_Length")), [Field("Payload")])
        self.assertEqual(ETHERNET_FRAME.direct_successors(FINAL), [])

    def test_message_definite_predecessors(self) -> None:
        self.assertTupleEqual(
            ETHERNET_FRAME.definite_predecessors(FINAL),
            (
                Field("Destination"),
                Field("Source"),
                Field("Type_Length_TPID"),
                Field("Type_Length"),
                Field("Payload"),
            ),
        )
        self.assertTupleEqual(
            ETHERNET_FRAME.definite_predecessors(Field("TCI")),
            (Field("Destination"), Field("Source"), Field("Type_Length_TPID"), Field("TPID")),
        )

    def test_message_predecessors(self) -> None:
        self.assertTupleEqual(
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
        self.assertTupleEqual(
            ETHERNET_FRAME.predecessors(Field("TCI")),
            (Field("Destination"), Field("Source"), Field("Type_Length_TPID"), Field("TPID")),
        )
        self.assertTupleEqual(ETHERNET_FRAME.predecessors(Field("Destination")), ())
        self.assertTupleEqual(ETHERNET_FRAME.predecessors(INITIAL), ())

    def test_message_successors(self) -> None:
        self.assertTupleEqual(
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
        self.assertTupleEqual(
            ETHERNET_FRAME.successors(Field("Source")),
            (
                Field("Type_Length_TPID"),
                Field("TPID"),
                Field("TCI"),
                Field("Type_Length"),
                Field("Payload"),
            ),
        )
        self.assertTupleEqual(
            ETHERNET_FRAME.successors(Field("TPID")),
            (Field("TCI"), Field("Type_Length"), Field("Payload")),
        )
        self.assertTupleEqual(ETHERNET_FRAME.successors(Field("Payload")), ())
        self.assertTupleEqual(ETHERNET_FRAME.successors(FINAL), ())

    def test_message_nonexistent_variable(self) -> None:
        mod_type = ModularInteger("P.MT", Pow(Number(2), Number(32)))
        enum_type = Enumeration("P.ET", {"Val1": Number(0), "Val2": Number(1)}, Number(8), True)
        structure = [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), Equal(Variable("F1"), Variable("Val3"))),
            Link(Field("F2"), FINAL),
        ]

        types = {Field("F1"): enum_type, Field("F2"): mod_type}
        with self.assertRaisesRegex(
            ModelError,
            '^undefined variable "Val3" referenced in condition 0 from field "F1" to "F2"',
        ):
            Message("P.M", structure, types)

    def test_message_subsequent_variable(self) -> None:
        t = ModularInteger("P.T", Pow(Number(2), Number(32)))
        structure = [
            Link(INITIAL, Field("F1")),
            Link(Field("F1"), Field("F2"), Equal(Variable("F2"), Number(42))),
            Link(Field("F2"), FINAL),
        ]

        types = {Field("F1"): t, Field("F2"): t}
        with self.assertRaisesRegex(
            ModelError, '^subsequent field "F2" referenced in condition 0 from field "F1" to "F2"',
        ):
            Message("P.M", structure, types)

    def test_message_field_size(self) -> None:
        message = Message(
            "P.M",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): MODULAR_INTEGER},
        )

        self.assertEqual(message.field_size(FINAL), Number(0))
        self.assertEqual(message.field_size(Field("F")), Number(8))

        with self.assertRaisesRegex(ValueError, '^field "X" not found$'):
            message.field_size(Field("X"))

    def test_message_copy(self) -> None:
        message = Message(
            "P.M",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): MODULAR_INTEGER},
        )
        self.assertEqual(
            message.copy(identifier="A.B"),
            Message(
                "A.B",
                [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
                {Field("F"): MODULAR_INTEGER},
            ),
        )
        self.assertEqual(
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

    def test_message_proven(self) -> None:
        message = Message(
            "P.M",
            [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
            {Field("F"): MODULAR_INTEGER},
        )
        self.assertEqual(
            message.proven(), message,
        )

    def test_derived_message_incorrect_base_name(self) -> None:
        with self.assertRaisesRegex(ModelError, '^unexpected format of type name "M"$'):
            DerivedMessage("P.M", Message("M", [], {}))

    def test_derived_message_proven(self) -> None:
        message = DerivedMessage(
            "P.M",
            Message(
                "X.M",
                [Link(INITIAL, Field("F")), Link(Field("F"), FINAL)],
                {Field("F"): MODULAR_INTEGER},
            ),
        )
        self.assertEqual(
            message.proven(), message,
        )

    def test_prefixed_message(self) -> None:
        self.assertEqual(
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

    def test_merge_message_simple(self) -> None:
        self.assertEqual(
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

    def test_merge_message_complex(self) -> None:
        self.assertEqual(
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

    def test_merge_message_recursive(self) -> None:
        self.assertEqual(
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

    def test_merge_message_simple_derived(self) -> None:
        self.assertEqual(
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

    def test_merge_message_error_name_conflict(self) -> None:
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

        with self.assertRaisesRegex(
            ModelError,
            f'^name conflict for "F1_F1" in "P.M1" caused by merging message "P.M2" in field "F1"$',
        ):
            m1.merged()

    def test_refinement_invalid_package(self) -> None:
        with self.assertRaisesRegex(ModelError, r'^unexpected format of package name "A.B"$'):
            Refinement("A.B", ETHERNET_FRAME, Field("Payload"), ETHERNET_FRAME)
