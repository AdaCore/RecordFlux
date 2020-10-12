from typing import Sequence

import pytest

from rflx.error import Location, RecordFluxError
from rflx.expression import Number
from rflx.identifier import ID
from rflx.model import BUILTIN_TYPES, Enumeration, Model, ModularInteger, Type


def assert_model_error(types: Sequence[Type], regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Model([*BUILTIN_TYPES.values(), *types])


def test_conflicting_literal_builtin_type() -> None:
    assert_model_error(
        [
            Enumeration(
                "P::T",
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
                "P::T",
                [
                    (ID("Foo", Location((3, 27))), Number(1)),
                    (ID("Bar", Location((3, 32))), Number(2)),
                ],
                Number(1),
                False,
            ),
            ModularInteger("T::Foo", Number(256), Location((4, 16))),
            ModularInteger("T::Bar", Number(256), Location((5, 16))),
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
                "P::T",
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
                "P::T1",
                [("Foo", Number(1)), (ID("Bar", Location((3, 33))), Number(2))],
                Number(1),
                False,
            ),
            Enumeration(
                "P::T2",
                [("Bar", Number(1)), ("Baz", Number(2))],
                Number(1),
                False,
                Location((4, 16)),
            ),
        ],
        r"<stdin>:4:16: model: error: conflicting literals: Bar\n"
        r'<stdin>:3:33: model: info: previous occurrence of "Bar"',
    )
