from typing import Callable

import pytest

import rflx.typing_ as rty
from rflx.expr import Add, Equal, Number, Pow, Size, Sub, Variable
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    FINAL,
    INITIAL,
    OPAQUE,
    UNCHECKED_OPAQUE,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Sequence,
    TypeDecl,
    UncheckedEnumeration,
    UncheckedInteger,
    UncheckedSequence,
    UncheckedTypeDecl,
)
from rflx.rapidflux import Location, RecordFluxError
from tests.data import models
from tests.utils import assert_equal


def test_type_name() -> None:
    t = Integer("Package::Type_Name", Number(0), Number(255), Number(8))
    assert t.name == "Type_Name"
    assert t.package == ID("Package")
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:20: error: invalid format for identifier "X"$',
    ):
        Integer(ID("X", Location((10, 20))), Number(0), Number(255), Number(8))
    with pytest.raises(
        RecordFluxError,
        match='^<stdin>:10:20: error: invalid format for identifier "X::Y::Z"$',
    ):
        Integer(ID("X::Y::Z", Location((10, 20))), Number(0), Number(255), Number(8))


def test_type_type() -> None:
    class NewType(TypeDecl):
        pass

    assert NewType("P::T").type_ == rty.Undefined()


def test_type_dependencies() -> None:
    class NewType(TypeDecl):
        pass

    assert NewType("P::T").dependencies == [NewType("P::T")]


def test_integer_size() -> None:
    assert_equal(
        Integer("P::T", Number(16), Number(128), Pow(Number(2), Number(5))).size,
        Number(32),
    )
    assert_equal(
        Integer("P::T", Number(16), Number(128), Pow(Number(2), Number(5))).size_expr,
        Pow(Number(2), Number(5)),
    )


def test_integer_value_count() -> None:
    assert_equal(
        Integer("P::T", Number(16), Number(128), Pow(Number(2), Number(5))).value_count,
        Number(113),
    )


def test_integer_first() -> None:
    integer = Integer(
        "P::T",
        Pow(Number(2), Number(4)),
        Sub(Pow(Number(2), Number(32)), Number(1)),
        Number(32),
    )
    assert integer.first == Number(16)
    assert integer.first_expr == Pow(Number(2), Number(4))


def test_integer_last() -> None:
    integer = Integer(
        "P::T",
        Pow(Number(2), Number(4)),
        Sub(Pow(Number(2), Number(32)), Number(1)),
        Number(32),
    )
    assert integer.last == Number(2**32 - 1)
    assert integer.last_expr == Sub(Pow(Number(2), Number(32)), Number(1))


def test_integer_invalid_first_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:5:3: error: first of "T" contains variable$',
    ):
        Integer("P::T", Add(Number(1), Variable("X")), Number(15), Number(4), Location((5, 3)))


def test_integer_invalid_last_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:80:6: error: last of "T" contains variable$',
    ):
        Integer("P::T", Number(1), Add(Number(1), Variable("X")), Number(4), Location((80, 6)))


def test_integer_invalid_last_exceeds_limit() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^error: last of "T" exceeds limit \(2\*\*63 - 1\)$',
    ):
        Integer("P::T", Number(1), Pow(Number(2), Number(63)), Number(64))


def test_integer_invalid_first_negative() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:6:4: error: first of "T" negative$',
    ):
        Integer("P::T", Number(-1), Number(0), Number(1), Location((6, 4)))


def test_integer_invalid_range() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:5: error: range of "T" negative$',
    ):
        Integer("P::T", Number(1), Number(0), Number(1), Location((10, 5)))


def test_integer_invalid_size_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:22:4: error: size of "T" contains variable$',
    ):
        Integer("P::T", Number(0), Number(256), Add(Number(8), Variable("X")), Location((22, 4)))


def test_integer_invalid_size_too_small() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:4: error: size of "T" too small$',
    ):
        Integer("P::T", Number(0), Number(256), Number(8), Location((10, 4)))


def test_integer_invalid_size_exceeds_limit() -> None:
    # Eng/RecordFlux/RecordFlux#238
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:50:3: error: size of "T" exceeds limit \(2\*\*63\)$',
    ):
        Integer("P::T", Number(0), Number(256), Number(128), Location((50, 3)))


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
        Number(2**32),
    )


def test_enumeration_invalid_size_variable() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:34:3: error: size of "T" contains variable$',
    ):
        Enumeration(
            "P::T",
            [("A", Number(1))],
            Add(Number(8), Variable("X")),
            always_valid=False,
            location=Location((34, 3)),
        )


def test_enumeration_invalid_literal_value() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:10:5: error: enumeration value of "T"'
            r" outside of permitted range \(0 .. 2\*\*63 - 1\)\n"
            r'<stdin>:10:5: error: size of "T" exceeds limit \(2\*\*63\)$'
        ),
    ):
        Enumeration(
            "P::T",
            [("A", Number(2**63))],
            Number(64),
            always_valid=False,
            location=Location((10, 5)),
        )


def test_enumeration_invalid_size_too_small() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:10:5: error: size of "T" too small$',
    ):
        Enumeration(
            "P::T",
            [("A", Number(256))],
            Number(8),
            always_valid=False,
            location=Location((10, 5)),
        )


def test_enumeration_invalid_size_exceeds_limit() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:8:20: error: size of "T" exceeds limit \(2\*\*63\)$',
    ):
        Enumeration(
            "P::T",
            [("A", Number(256))],
            Number(128),
            always_valid=False,
            location=Location((8, 20)),
        )


def test_enumeration_invalid_always_valid_aspect() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^error: unnecessary always-valid aspect on "T"$',
    ):
        Enumeration(
            "P::T",
            [("A", Number(0)), ("B", Number(1))],
            Number(1),
            always_valid=True,
        ).error.propagate()


def test_enumeration_invalid_literal() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:1:2: error: invalid literal name "A B" in "T"$',
    ):
        Enumeration(
            "P::T",
            [("A B", Number(1))],
            Number(8),
            always_valid=False,
            location=Location((1, 2)),
        )
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:6:4: error: invalid literal name "A.B" in "T"$',
    ):
        Enumeration(
            "P::T",
            [("A.B", Number(1))],
            Number(8),
            always_valid=False,
            location=Location((6, 4)),
        )


def test_enumeration_invalid_duplicate_elements() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:3:32: error: duplicate literal "Foo"\n'
            r"<stdin>:3:27: info: previous occurrence"
            r"$"
        ),
    ):
        Enumeration(
            "P::T",
            [(ID("Foo", Location((3, 27))), Number(1)), (ID("Foo", Location((3, 32))), Number(2))],
            Number(8),
            always_valid=False,
        )


def test_enumeration_invalid_multiple_duplicate_elements() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:3:37: error: duplicate literal "Foo"\n'
            r"<stdin>:3:27: info: previous occurrence\n"
            r'<stdin>:3:42: error: duplicate literal "Bar"\n'
            r"<stdin>:3:32: info: previous occurrence"
            r"$"
        ),
    ):
        Enumeration(
            "P::T",
            [
                (ID("Foo", Location((3, 27))), Number(1)),
                (ID("Bar", Location((3, 32))), Number(2)),
                (ID("Foo", Location((3, 37))), Number(3)),
                (ID("Bar", Location((3, 42))), Number(4)),
            ],
            Number(8),
            always_valid=False,
        )


def test_enumeration_str() -> None:
    assert (
        str(
            Enumeration(
                "P::T",
                [("A", Number(1))],
                Pow(Number(2), Number(5)),
                always_valid=False,
            ),
        )
        == "type T is (A => 1) with Size => 2 ** 5"
    )
    assert str(
        Enumeration(
            "P::T",
            [
                ("A", Number(2**2)),
                ("B", Number(2**3)),
                ("C", Number(2**4)),
                ("D", Number(2**5)),
                ("E", Number(2**6)),
                ("F", Number(2**7)),
            ],
            Pow(Number(2), Number(4)),
            always_valid=True,
        ),
    ) == (
        "type T is\n"
        "   (A => 4,\n"
        "    B => 8,\n"
        "    C => 16,\n"
        "    D => 32,\n"
        "    E => 64,\n"
        "    F => 128)\n"
        "with Size => 2 ** 4, Always_Valid => True"
    )


def test_sequence_dependencies() -> None:
    assert models.sequence_integer_vector().dependencies == [
        models.sequence_integer(),
        models.sequence_integer_vector(),
    ]
    assert models.sequence_inner_messages().dependencies == [
        models.sequence_length(),
        OPAQUE,
        models.sequence_inner_message(),
        models.sequence_inner_messages(),
    ]


@pytest.mark.parametrize(
    ("element_type", "error"),
    [
        (
            lambda: Sequence("P::B", models.integer(), Location((3, 4))),
            r'<stdin>:1:2: error: invalid element type of sequence "A"\n'
            r'<stdin>:3:4: info: type "B" must be scalar or message',
        ),
        (
            lambda: OPAQUE,
            r'<stdin>:1:2: error: invalid element type of sequence "A"\n'
            r'__BUILTINS__:1:1: info: type "Opaque" must be scalar or message',
        ),
        (
            lambda: Message("P::B", [], {}, location=Location((3, 4))),
            r'<stdin>:1:2: error: invalid element type of sequence "A"\n'
            r"<stdin>:3:4: info: null messages must not be used as sequence element",
        ),
        (
            lambda: Message(
                ID("P::B", Location((1, 1))),
                [Link(INITIAL, Field("A"), size=Size("Message")), Link(Field("A"), FINAL)],
                {Field("A"): OPAQUE},
                location=Location((3, 4)),
            ),
            r'<stdin>:1:2: error: invalid element type of sequence "A"\n'
            r"<stdin>:3:4: info: messages used as sequence element must not depend"
            ' on "Message\'Size" or "Message\'Last"',
        ),
        (
            lambda: Message(
                ID("P::B", Location((1, 1))),
                [
                    Link(INITIAL, Field("A"), condition=Equal(Size("Message"), Number(8))),
                    Link(Field("A"), FINAL),
                ],
                {Field("A"): models.integer()},
                location=Location((3, 4)),
            ),
            r'<stdin>:1:2: error: invalid element type of sequence "A"\n'
            r"<stdin>:3:4: info: messages used as sequence element must not depend"
            ' on "Message\'Size" or "Message\'Last"',
        ),
        (
            lambda: Message(
                ID("P::B", Location((1, 1))),
                [
                    Link(INITIAL, Field("A"), condition=Variable("P")),
                    Link(Field("A"), FINAL),
                ],
                {Field("P"): BOOLEAN, Field("A"): models.integer()},
                location=Location((3, 4)),
            ),
            r'<stdin>:1:2: error: invalid element type of sequence "A"\n'
            r"<stdin>:3:4: info: parameterized messages must not be used"
            r" as sequence element",
        ),
    ],
)
def test_sequence_invalid_element_type(element_type: Callable[[], TypeDecl], error: str) -> None:
    with pytest.raises(RecordFluxError, match=f"^{error}$"):
        Sequence("P::A", element_type(), Location((1, 2)))


def test_sequence_unsupported_element_type() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:5:4: error: unsupported element type size of sequence "A"\n'
            r'<stdin>:3:4: info: type "B" has size 4, must be multiple of 8$'
        ),
    ):
        Sequence(
            "P::A",
            Integer(
                "P::B",
                Number(0),
                Sub(Pow(Number(2), Number(4)), Number(1)),
                Number(4),
                Location((3, 4)),
            ),
            Location((5, 4)),
        )
    with pytest.raises(
        RecordFluxError,
        match=(
            r'^<stdin>:5:4: error: unsupported element type size of sequence "A"\n'
            r'__BUILTINS__:1:1: info: type "Boolean" has size 1, must be multiple of 8$'
        ),
    ):
        Sequence("P::A", BOOLEAN, Location((5, 4)))


@pytest.mark.parametrize(
    ("unchecked", "expected"),
    [
        (
            UncheckedInteger(ID("P::T"), Number(0), Number(128), Number(8), Location((1, 2))),
            Integer(ID("P::T"), Number(0), Number(128), Number(8), Location((1, 2))),
        ),
        (
            UncheckedEnumeration(
                ID("P::T"),
                [(ID("A"), Number(0)), (ID("B"), Number(1))],
                Number(8),
                always_valid=False,
                location=Location((1, 2)),
            ),
            Enumeration(
                ID("P::T"),
                [(ID("A"), Number(0)), (ID("B"), Number(1))],
                Number(8),
                always_valid=False,
                location=Location((1, 2)),
            ),
        ),
        (
            UncheckedSequence(
                ID("P::S"),
                ID("P::T"),
                Location((2, 3)),
            ),
            Sequence(
                ID("P::S"),
                Integer(ID("P::T"), Number(0), Number(128), Number(8), Location((1, 2))),
                Location((2, 3)),
            ),
        ),
        (UNCHECKED_OPAQUE, OPAQUE),
    ],
)
def test_unchecked_type_checked(unchecked: UncheckedTypeDecl, expected: TypeDecl) -> None:
    assert (
        unchecked.checked(
            [Integer(ID("P::T"), Number(0), Number(128), Number(8), Location((1, 2)))],
        )
        == expected
    )


@pytest.mark.parametrize(
    ("unchecked", "expected"),
    [
        (
            UncheckedInteger(
                ID("T", Location((2, 3))),
                Number(0),
                Number(128),
                Number(8),
                Location((1, 2)),
            ),
            r'^<stdin>:2:3: error: invalid format for identifier "T"$',
        ),
        (
            UncheckedEnumeration(
                ID("T", Location((2, 3))),
                [(ID("A"), Number(0)), (ID("B"), Number(1))],
                Number(8),
                always_valid=False,
                location=Location((1, 2)),
            ),
            r'^<stdin>:2:3: error: invalid format for identifier "T"$',
        ),
        (
            UncheckedSequence(
                ID("S", Location((4, 5))),
                ID("T", Location((2, 3))),
                Location((3, 4)),
            ),
            r'^<stdin>:2:3: error: undefined element type "T"$',
        ),
    ],
)
def test_unchecked_type_checked_error(unchecked: UncheckedTypeDecl, expected: str) -> None:
    with pytest.raises(RecordFluxError, match=expected):
        unchecked.checked([])
