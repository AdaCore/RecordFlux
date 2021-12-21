import pytest

import rflx.typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.expression import Add, Equal, Number, Pow, Size, Sub, Variable
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    OPAQUE,
    Enumeration,
    Field,
    Link,
    Message,
    ModularInteger,
    RangeInteger,
    Sequence,
    Type,
)
from tests.data import models
from tests.utils import assert_equal, assert_type_error


def test_type_name() -> None:
    t = ModularInteger("Package::Type_Name", Number(256))
    assert t.name == "Type_Name"
    assert t.package == ID("Package")
    assert_type_error(
        ModularInteger(ID("X", Location((10, 20))), Number(256)),
        r'^<stdin>:10:20: model: error: invalid format for identifier "X"$',
    )
    assert_type_error(
        ModularInteger(ID("X::Y::Z", Location((10, 20))), Number(256)),
        '^<stdin>:10:20: model: error: invalid format for identifier "X::Y::Z"$',
    )


def test_type_type() -> None:
    class NewType(Type):
        pass

    assert NewType("P::T").type_ == rty.Undefined()


def test_type_dependencies() -> None:
    class NewType(Type):
        pass

    assert NewType("P::T").dependencies == [NewType("P::T")]


def test_modular_size() -> None:
    assert ModularInteger("P::T", Pow(Number(2), Number(32))).size == Number(32)
    assert ModularInteger("P::T", Pow(Number(2), Number(32))).size_expr == Number(32)


def test_modular_value_count() -> None:
    assert ModularInteger("P::T", Pow(Number(2), Number(32))).value_count == Number(2 ** 32)


def test_modular_first() -> None:
    mod = ModularInteger("P::T", Pow(Number(2), Number(32)))
    assert mod.first == Number(0)


def test_modular_last() -> None:
    mod = ModularInteger("P::T", Pow(Number(2), Number(32)))
    assert mod.last == Number(2 ** 32 - 1)


def test_modular_invalid_modulus_power_of_two() -> None:
    assert_type_error(
        ModularInteger("P::T", Number(255), Location((65, 3))),
        r'^<stdin>:65:3: model: error: modulus of "T" not power of two$',
    )


def test_modular_invalid_modulus_variable() -> None:
    assert_type_error(
        ModularInteger("P::T", Pow(Number(2), Variable("X")), Location((3, 23))),
        r'^<stdin>:3:23: model: error: modulus of "T" contains variable$',
    )


def test_modular_invalid_modulus_limit() -> None:
    assert_type_error(
        ModularInteger("P::T", Pow(Number(2), Number(128), Location((55, 3)))),
        r'^<stdin>:55:3: model: error: modulus of "T" exceeds limit \(2\*\*64\)$',
    )


def test_range_size() -> None:
    assert_equal(
        RangeInteger("P::T", Number(16), Number(128), Pow(Number(2), Number(5))).size,
        Number(32),
    )
    assert_equal(
        RangeInteger("P::T", Number(16), Number(128), Pow(Number(2), Number(5))).size_expr,
        Pow(Number(2), Number(5)),
    )


def test_range_value_count() -> None:
    assert_equal(
        RangeInteger("P::T", Number(16), Number(128), Pow(Number(2), Number(5))).value_count,
        Number(113),
    )


def test_range_first() -> None:
    integer = RangeInteger(
        "P::T", Pow(Number(2), Number(4)), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32)
    )
    assert integer.first == Number(16)
    assert integer.first_expr == Pow(Number(2), Number(4))


def test_range_last() -> None:
    integer = RangeInteger(
        "P::T", Pow(Number(2), Number(4)), Sub(Pow(Number(2), Number(32)), Number(1)), Number(32)
    )
    assert integer.last == Number(2 ** 32 - 1)
    assert integer.last_expr == Sub(Pow(Number(2), Number(32)), Number(1))


def test_range_invalid_first_variable() -> None:
    assert_type_error(
        RangeInteger(
            "P::T", Add(Number(1), Variable("X")), Number(15), Number(4), Location((5, 3))
        ),
        r'^<stdin>:5:3: model: error: first of "T" contains variable$',
    )


def test_range_invalid_last_variable() -> None:
    assert_type_error(
        RangeInteger(
            "P::T", Number(1), Add(Number(1), Variable("X")), Number(4), Location((80, 6))
        ),
        r'^<stdin>:80:6: model: error: last of "T" contains variable$',
    )


def test_range_invalid_last_exceeds_limit() -> None:
    assert_type_error(
        RangeInteger("P::T", Number(1), Pow(Number(2), Number(63)), Number(64)),
        r'^model: error: last of "T" exceeds limit \(2\*\*63 - 1\)$',
    )


def test_range_invalid_first_negative() -> None:
    assert_type_error(
        RangeInteger("P::T", Number(-1), Number(0), Number(1), Location((6, 4))),
        r'^<stdin>:6:4: model: error: first of "T" negative$',
    )


def test_range_invalid_range() -> None:
    assert_type_error(
        RangeInteger("P::T", Number(1), Number(0), Number(1), Location((10, 5))),
        r'^<stdin>:10:5: model: error: range of "T" negative$',
    )


def test_range_invalid_size_variable() -> None:
    assert_type_error(
        RangeInteger(
            "P::T", Number(0), Number(256), Add(Number(8), Variable("X")), Location((22, 4))
        ),
        r'^<stdin>:22:4: model: error: size of "T" contains variable$',
    )


def test_range_invalid_size_too_small() -> None:
    assert_type_error(
        RangeInteger("P::T", Number(0), Number(256), Number(8), Location((10, 4))),
        r'^<stdin>:10:4: model: error: size of "T" too small$',
    )


def test_range_invalid_size_exceeds_limit() -> None:
    # ISSUE: Componolit/RecordFlux#238
    assert_type_error(
        RangeInteger("P::T", Number(0), Number(256), Number(128), Location((50, 3))),
        r'^<stdin>:50:3: model: error: size of "T" exceeds limit \(2\*\*64\)$',
    )


def test_enumeration_size() -> None:
    assert_equal(
        Enumeration(
            "P::T",
            [("A", Number(1))],
            Pow(Number(2), Number(5)),
            always_valid=False,
            location=Location((34, 3)),
        ).size,
        Number(32),
    )
    assert_equal(
        Enumeration(
            "P::T",
            [("A", Number(1))],
            Pow(Number(2), Number(5)),
            always_valid=False,
            location=Location((34, 3)),
        ).size_expr,
        Pow(Number(2), Number(5)),
    )


def test_enumeration_value_count() -> None:
    assert_equal(
        Enumeration(
            "P::T",
            [("A", Number(1))],
            Pow(Number(2), Number(5)),
            always_valid=False,
            location=Location((34, 3)),
        ).value_count,
        Number(1),
    )
    assert_equal(
        Enumeration(
            "P::T",
            [("A", Number(1))],
            Pow(Number(2), Number(5)),
            always_valid=True,
            location=Location((34, 3)),
        ).value_count,
        Number(2 ** 32),
    )


def test_enumeration_invalid_size_variable() -> None:
    assert_type_error(
        Enumeration(
            "P::T",
            [("A", Number(1))],
            Add(Number(8), Variable("X")),
            always_valid=False,
            location=Location((34, 3)),
        ),
        r'^<stdin>:34:3: model: error: size of "T" contains variable$',
    )


def test_enumeration_invalid_literal_value() -> None:
    assert_type_error(
        Enumeration(
            "P::T",
            [("A", Number(2 ** 63))],
            Number(64),
            always_valid=False,
            location=Location((10, 5)),
        ),
        r'^<stdin>:10:5: model: error: enumeration value of "T"'
        r" outside of permitted range \(0 .. 2\*\*63 - 1\)$",
    )


def test_enumeration_invalid_size_too_small() -> None:
    assert_type_error(
        Enumeration(
            "P::T", [("A", Number(256))], Number(8), always_valid=False, location=Location((10, 5))
        ),
        r'^<stdin>:10:5: model: error: size of "T" too small$',
    )


def test_enumeration_invalid_size_exceeds_limit() -> None:
    assert_type_error(
        Enumeration(
            "P::T",
            [("A", Number(256))],
            Number(128),
            always_valid=False,
            location=Location((8, 20)),
        ),
        r'^<stdin>:8:20: model: error: size of "T" exceeds limit \(2\*\*64\)$',
    )


def test_enumeration_invalid_always_valid_aspect() -> None:
    with pytest.raises(
        RecordFluxError, match=r'^model: error: unnecessary always-valid aspect on "T"$'
    ):
        Enumeration(
            "P::T", [("A", Number(0)), ("B", Number(1))], Number(1), always_valid=True
        ).error.propagate()


def test_enumeration_invalid_literal() -> None:
    assert_type_error(
        Enumeration(
            "P::T", [("A B", Number(1))], Number(8), always_valid=False, location=Location(((1, 2)))
        ),
        r'^<stdin>:1:2: model: error: invalid literal name "A B" in "T"$',
    )
    assert_type_error(
        Enumeration(
            "P::T", [("A.B", Number(1))], Number(8), always_valid=False, location=Location((6, 4))
        ),
        r'^<stdin>:6:4: model: error: invalid literal name "A.B" in "T"$',
    )


def test_enumeration_invalid_duplicate_elements() -> None:
    assert_type_error(
        Enumeration(
            "P::T",
            [(ID("Foo", Location((3, 27))), Number(1)), (ID("Foo", Location((3, 32))), Number(2))],
            Number(1),
            always_valid=False,
        ),
        r'<stdin>:3:32: model: error: duplicate literal "Foo"\n'
        r"<stdin>:3:27: model: info: previous occurrence",
    )


def test_enumeration_invalid_multiple_duplicate_elements() -> None:
    assert_type_error(
        Enumeration(
            "P::T",
            [
                (ID("Foo", Location((3, 27))), Number(1)),
                (ID("Bar", Location((3, 32))), Number(2)),
                (ID("Foo", Location((3, 37))), Number(3)),
                (ID("Bar", Location((3, 42))), Number(4)),
            ],
            Number(2),
            always_valid=False,
        ),
        r'<stdin>:3:37: model: error: duplicate literal "Foo"\n'
        r"<stdin>:3:27: model: info: previous occurrence\n"
        r'<stdin>:3:42: model: error: duplicate literal "Bar"\n'
        r"<stdin>:3:32: model: info: previous occurrence",
    )


def test_enumeration_str() -> None:
    assert (
        str(
            Enumeration(
                "P::T",
                [("A", Number(1))],
                Pow(Number(2), Number(5)),
                always_valid=False,
            )
        )
        == "type T is (A => 1) with Size => 2 ** 5"
    )
    assert str(
        Enumeration(
            "P::T",
            [
                ("A", Number(2 ** 2)),
                ("B", Number(2 ** 3)),
                ("C", Number(2 ** 4)),
                ("D", Number(2 ** 5)),
                ("E", Number(2 ** 6)),
                ("F", Number(2 ** 7)),
            ],
            Pow(Number(2), Number(8)),
            always_valid=True,
        )
    ) == (
        "type T is\n"
        "   (A => 4,\n"
        "    B => 8,\n"
        "    C => 16,\n"
        "    D => 32,\n"
        "    E => 64,\n"
        "    F => 128)\n"
        "with Size => 2 ** 8, Always_Valid => True"
    )


def test_sequence_dependencies() -> None:
    assert models.SEQUENCE_MODULAR_VECTOR.dependencies == [
        models.SEQUENCE_MODULAR_VECTOR,
        models.SEQUENCE_MODULAR_INTEGER,
    ]
    assert models.SEQUENCE_INNER_MESSAGES.dependencies == [
        models.SEQUENCE_INNER_MESSAGES,
        models.SEQUENCE_INNER_MESSAGE,
        models.SEQUENCE_LENGTH,
        OPAQUE,
    ]


@pytest.mark.parametrize(
    "element_type, error",
    [
        (
            Sequence("P::B", models.MODULAR_INTEGER, Location((3, 4))),
            r'<stdin>:1:2: model: error: invalid element type of sequence "A"\n'
            r'<stdin>:3:4: model: info: type "B" must be scalar or message',
        ),
        (
            OPAQUE,
            r'<stdin>:1:2: model: error: invalid element type of sequence "A"\n'
            r'__BUILTINS__:0:0: model: info: type "Opaque" must be scalar or message',
        ),
        (
            Message("P::B", [], {}, location=Location((3, 4))),
            r'<stdin>:1:2: model: error: invalid element type of sequence "A"\n'
            r"<stdin>:3:4: model: info: null messages must not be used as sequence element",
        ),
        (
            Message(
                "P::B",
                [Link(INITIAL, Field("A"), size=Size("Message")), Link(Field("A"), FINAL)],
                {Field("A"): OPAQUE},
                location=Location((3, 4)),
            ),
            r'<stdin>:1:2: model: error: invalid element type of sequence "A"\n'
            r"<stdin>:3:4: model: info: messages used as sequence element must not depend"
            ' on "Message\'Size" or "Message\'Last"',
        ),
        (
            Message(
                "P::B",
                [
                    Link(INITIAL, Field("A"), condition=Equal(Size("Message"), Number(8))),
                    Link(Field("A"), FINAL),
                ],
                {Field("A"): models.MODULAR_INTEGER},
                location=Location((3, 4)),
            ),
            r'<stdin>:1:2: model: error: invalid element type of sequence "A"\n'
            r"<stdin>:3:4: model: info: messages used as sequence element must not depend"
            ' on "Message\'Size" or "Message\'Last"',
        ),
    ],
)
def test_sequence_invalid_element_type(element_type: Type, error: str) -> None:
    assert_type_error(Sequence("P::A", element_type, Location((1, 2))), f"^{error}$")


def test_sequence_unsupported_element_type() -> None:
    assert_type_error(
        Sequence(
            "P::A",
            ModularInteger("P::B", Pow(Number(2), Number(4)), Location((3, 4))),
            Location((5, 4)),
        ),
        r'^<stdin>:5:4: model: error: unsupported element type size of sequence "A"\n'
        r'<stdin>:3:4: model: info: type "B" has size 4, must be multiple of 8$',
    )
    assert_type_error(
        Sequence("P::A", BOOLEAN, Location((5, 4))),
        r'^<stdin>:5:4: model: error: unsupported element type size of sequence "A"\n'
        r'__BUILTINS__:0:0: model: info: type "Boolean" has size 1, must be multiple of 8$',
    )
