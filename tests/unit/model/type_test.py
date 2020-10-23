import pytest

import rflx.typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.expression import Add, Number, Pow, Sub, Variable
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    OPAQUE,
    Array,
    Enumeration,
    Message,
    ModularInteger,
    RangeInteger,
    Type,
)
from tests.data import models
from tests.utils import assert_equal, assert_type_error


def test_type_name() -> None:
    t = ModularInteger("Package::Type_Name", Number(256))
    assert t.name == "Type_Name"
    assert t.package == ID("Package")
    assert_type_error(
        ModularInteger("X", Number(256), Location((10, 20))),
        r'^<stdin>:10:20: model: error: invalid format of type identifier "X"$',
    )
    assert_type_error(
        ModularInteger("X::Y::Z", Number(256), Location((10, 20))),
        '^<stdin>:10:20: model: error: invalid format of type identifier "X::Y::Z"$',
    )


def test_type_type() -> None:
    class NewType(Type):
        pass

    assert NewType("P::T").type_ == rty.Undefined()


def test_modular_size() -> None:
    assert ModularInteger("P::T", Pow(Number(2), Number(32))).size == Number(32)
    assert ModularInteger("P::T", Pow(Number(2), Number(32))).size_expr == Number(32)


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


def test_range_serialize() -> None:
    assert models.RANGE_INTEGER.serialize == {
        "data": {
            "identifier": ["P", "Range"],
            "first": {"data": {"base": 0, "value": 1}, "kind": "Number"},
            "last": {"data": {"base": 0, "value": 100}, "kind": "Number"},
            "size": {"data": {"base": 0, "value": 8}, "kind": "Number"},
        },
        "kind": "RangeInteger",
    }


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
            "P::T", [("A", Number(1))], Pow(Number(2), Number(5)), False, Location((34, 3))
        ).size,
        Number(32),
    )
    assert_equal(
        Enumeration(
            "P::T", [("A", Number(1))], Pow(Number(2), Number(5)), False, Location((34, 3))
        ).size_expr,
        Pow(Number(2), Number(5)),
    )


def test_enumeration_invalid_size_variable() -> None:
    assert_type_error(
        Enumeration(
            "P::T", [("A", Number(1))], Add(Number(8), Variable("X")), False, Location((34, 3))
        ),
        r'^<stdin>:34:3: model: error: size of "T" contains variable$',
    )


def test_enumeration_invalid_literal_value() -> None:
    assert_type_error(
        Enumeration("P::T", [("A", Number(2 ** 63))], Number(64), False, Location((10, 5))),
        r'^<stdin>:10:5: model: error: enumeration value of "T"'
        r" outside of permitted range \(0 .. 2\*\*63 - 1\)$",
    )


def test_enumeration_invalid_size_too_small() -> None:
    assert_type_error(
        Enumeration("P::T", [("A", Number(256))], Number(8), False, Location((10, 5))),
        r'^<stdin>:10:5: model: error: size of "T" too small$',
    )


def test_enumeration_invalid_size_exceeds_limit() -> None:
    assert_type_error(
        Enumeration("P::T", [("A", Number(256))], Number(128), False, Location((8, 20))),
        r'^<stdin>:8:20: model: error: size of "T" exceeds limit \(2\*\*64\)$',
    )


def test_enumeration_invalid_always_valid_aspect() -> None:
    with pytest.raises(
        RecordFluxError, match=r'^model: error: unnecessary always-valid aspect on "T"$'
    ):
        Enumeration("P::T", [("A", Number(0)), ("B", Number(1))], Number(1), True).error.propagate()


def test_enumeration_invalid_literal() -> None:
    assert_type_error(
        Enumeration("P::T", [("A B", Number(1))], Number(8), False, Location(((1, 2)))),
        r'^<stdin>:1:2: model: error: invalid literal name "A B" in "T"$',
    )
    assert_type_error(
        Enumeration("P::T", [("A.B", Number(1))], Number(8), False, Location((6, 4))),
        r'^<stdin>:6:4: model: error: invalid literal name "A.B" in "T"$',
    )


def test_enumeration_invalid_duplicate_elements() -> None:
    assert_type_error(
        Enumeration(
            "P::T",
            [(ID("Foo", Location((3, 27))), Number(1)), (ID("Foo", Location((3, 32))), Number(2))],
            Number(1),
            False,
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
            False,
        ),
        r'<stdin>:3:37: model: error: duplicate literal "Foo"\n'
        r"<stdin>:3:27: model: info: previous occurrence\n"
        r'<stdin>:3:42: model: error: duplicate literal "Bar"\n'
        r"<stdin>:3:32: model: info: previous occurrence",
    )


def test_array_serialize() -> None:
    assert models.ARRAYS_MODULAR_VECTOR.serialize == {
        "kind": "Array",
        "data": {
            "identifier": ["Arrays", "Modular_Vector"],
            "element_type": ["Arrays", "Modular_Integer"],
        },
    }


def test_array_invalid_element_type() -> None:
    assert_type_error(
        Array("P::A", Array("P::B", models.MODULAR_INTEGER, Location((3, 4))), Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" must be scalar or non-null message$',
    )
    assert_type_error(
        Array("P::A", Message("P::B", [], {}, location=Location((3, 4))), Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" must be scalar or non-null message$',
    )
    assert_type_error(
        Array("P::A", OPAQUE, Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'__BUILTINS__:0:0: model: info: type "Opaque" must be scalar or non-null message$',
    )


def test_array_unsupported_element_type() -> None:
    assert_type_error(
        Array(
            "P::A",
            ModularInteger("P::B", Pow(Number(2), Number(4)), Location((3, 4))),
            Location((5, 4)),
        ),
        r'^<stdin>:5:4: model: error: unsupported element type size of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" has size 4, must be multiple of 8$',
    )
    assert_type_error(
        Array("P::A", BOOLEAN, Location((5, 4))),
        r'^<stdin>:5:4: model: error: unsupported element type size of array "A"\n'
        r'__BUILTINS__:0:0: model: info: type "Boolean" has size 1, must be multiple of 8$',
    )
