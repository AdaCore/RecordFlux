from typing import Sequence

import pytest

from rflx.error import Location, RecordFluxError
from rflx.expression import Add, Number, Pow, Sub, Variable
from rflx.identifier import ID
from rflx.model import (
    BOOLEAN,
    BUILTIN_TYPES,
    OPAQUE,
    Array,
    Enumeration,
    Message,
    Model,
    ModularInteger,
    RangeInteger,
    Type,
)
from tests.models import MODULAR_INTEGER
from tests.utils import assert_equal, assert_type_model_error


def assert_model_error(types: Sequence[Type], regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Model([*BUILTIN_TYPES.values(), *types])


def test_type_name() -> None:
    t = ModularInteger("Package.Type_Name", Number(256))
    assert t.name == "Type_Name"
    assert t.package == ID("Package")
    assert_type_model_error(
        ModularInteger("X", Number(256), Location((10, 20))),
        r'^<stdin>:10:20: model: error: unexpected format of type name "X"$',
    )
    assert_type_model_error(
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
    assert_type_model_error(
        ModularInteger("P.T", Number(255), Location((65, 3))),
        r'^<stdin>:65:3: model: error: modulus of "T" not power of two$',
    )


def test_modular_invalid_modulus_variable() -> None:
    assert_type_model_error(
        ModularInteger("P.T", Pow(Number(2), Variable("X")), Location((3, 23))),
        r'^<stdin>:3:23: model: error: modulus of "T" contains variable$',
    )


def test_modular_invalid_modulus_limit() -> None:
    assert_type_model_error(
        ModularInteger("P.T", Pow(Number(2), Number(128), Location((55, 3)))),
        r'^<stdin>:55:3: model: error: modulus of "T" exceeds limit \(2\*\*64\)$',
    )


def test_range_size() -> None:
    assert_equal(
        RangeInteger("P.T", Number(16), Number(128), Pow(Number(2), Number(5))).size,
        Number(32),
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
    assert_type_model_error(
        RangeInteger("P.T", Add(Number(1), Variable("X")), Number(15), Number(4), Location((5, 3))),
        r'^<stdin>:5:3: model: error: first of "T" contains variable$',
    )


def test_range_invalid_last_variable() -> None:
    assert_type_model_error(
        RangeInteger("P.T", Number(1), Add(Number(1), Variable("X")), Number(4), Location((80, 6))),
        r'^<stdin>:80:6: model: error: last of "T" contains variable$',
    )


def test_range_invalid_last_exceeds_limit() -> None:
    assert_type_model_error(
        RangeInteger("P.T", Number(1), Pow(Number(2), Number(63)), Number(64)),
        r'^model: error: last of "T" exceeds limit \(2\*\*63 - 1\)$',
    )


def test_range_invalid_first_negative() -> None:
    assert_type_model_error(
        RangeInteger("P.T", Number(-1), Number(0), Number(1), Location((6, 4))),
        r'^<stdin>:6:4: model: error: first of "T" negative$',
    )


def test_range_invalid_range() -> None:
    assert_type_model_error(
        RangeInteger("P.T", Number(1), Number(0), Number(1), Location((10, 5))),
        r'^<stdin>:10:5: model: error: range of "T" negative$',
    )


def test_range_invalid_size_variable() -> None:
    assert_type_model_error(
        RangeInteger(
            "P.T", Number(0), Number(256), Add(Number(8), Variable("X")), Location((22, 4))
        ),
        r'^<stdin>:22:4: model: error: size of "T" contains variable$',
    )


def test_range_invalid_size_too_small() -> None:
    assert_type_model_error(
        RangeInteger("P.T", Number(0), Number(256), Number(8), Location((10, 4))),
        r'^<stdin>:10:4: model: error: size of "T" too small$',
    )


def test_range_invalid_size_exceeds_limit() -> None:
    # ISSUE: Componolit/RecordFlux#238
    assert_type_model_error(
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
    assert_type_model_error(
        Enumeration(
            "P.T", [("A", Number(1))], Add(Number(8), Variable("X")), False, Location((34, 3))
        ),
        r'^<stdin>:34:3: model: error: size of "T" contains variable$',
    )


def test_enumeration_invalid_literal_value() -> None:
    assert_type_model_error(
        Enumeration("P.T", [("A", Number(2 ** 63))], Number(64), False, Location((10, 5))),
        r'^<stdin>:10:5: model: error: enumeration value of "T"'
        r" outside of permitted range \(0 .. 2\*\*63 - 1\)$",
    )


def test_enumeration_invalid_size_too_small() -> None:
    assert_type_model_error(
        Enumeration("P.T", [("A", Number(256))], Number(8), False, Location((10, 5))),
        r'^<stdin>:10:5: model: error: size of "T" too small$',
    )


def test_enumeration_invalid_size_exceeds_limit() -> None:
    assert_type_model_error(
        Enumeration("P.T", [("A", Number(256))], Number(128), False, Location((8, 20))),
        r'^<stdin>:8:20: model: error: size of "T" exceeds limit \(2\*\*64\)$',
    )


def test_enumeration_invalid_always_valid_aspect() -> None:
    with pytest.raises(
        RecordFluxError, match=r'^model: error: unnecessary always-valid aspect on "T"$'
    ):
        Enumeration("P.T", [("A", Number(0)), ("B", Number(1))], Number(1), True).error.propagate()


def test_enumeration_invalid_literal() -> None:
    assert_type_model_error(
        Enumeration("P.T", [("A B", Number(1))], Number(8), False, Location(((1, 2)))),
        r'^<stdin>:1:2: model: error: invalid literal name "A B" in "T"$',
    )
    assert_type_model_error(
        Enumeration("P.T", [("A.B", Number(1))], Number(8), False, Location((6, 4))),
        r'^<stdin>:6:4: model: error: invalid literal name "A.B" in "T"$',
    )


def test_array_invalid_element_type() -> None:
    assert_type_model_error(
        Array("P.A", Array("P.B", MODULAR_INTEGER, Location((3, 4))), Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" must be scalar or non-null message$',
    )
    assert_type_model_error(
        Array("P.A", Message("P.B", [], {}, location=Location((3, 4))), Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" must be scalar or non-null message$',
    )
    assert_type_model_error(
        Array("P.A", OPAQUE, Location((5, 4))),
        r'^<stdin>:5:4: model: error: invalid element type of array "A"\n'
        r'__BUILTINS__:0:0: model: info: type "Opaque" must be scalar or non-null message$',
    )


def test_array_unsupported_element_type() -> None:
    assert_type_model_error(
        Array(
            "P.A",
            ModularInteger("P.B", Pow(Number(2), Number(4)), Location((3, 4))),
            Location((5, 4)),
        ),
        r'^<stdin>:5:4: model: error: unsupported element type size of array "A"\n'
        r'<stdin>:3:4: model: info: type "B" has size 4, must be multiple of 8$',
    )
    assert_type_model_error(
        Array("P.A", BOOLEAN, Location((5, 4))),
        r'^<stdin>:5:4: model: error: unsupported element type size of array "A"\n'
        r'__BUILTINS__:0:0: model: info: type "Boolean" has size 1, must be multiple of 8$',
    )


def test_invalid_enumeration_type_duplicate_elements() -> None:
    assert_type_model_error(
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
    assert_type_model_error(
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
