from copy import copy
from typing import Sequence

import pytest

from rflx.error import Location, RecordFluxError
from rflx.expression import Number
from rflx.identifier import ID
from rflx.model import BUILTIN_TYPES, Enumeration, Model, ModularInteger, RangeInteger, Type
from tests.data import models


def assert_model_error(types: Sequence[Type], regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        Model([*BUILTIN_TYPES.values(), *types])


def test_name_conflict_types() -> None:
    assert_model_error(
        [
            ModularInteger(ID("P::T"), Number(256), location=Location((10, 20))),
            RangeInteger(
                ID("P::T"), Number(1), Number(100), Number(8), location=Location((11, 30))
            ),
        ],
        r"^"
        r'<stdin>:11:30: model: error: name conflict for type "P::T"\n'
        r'<stdin>:10:20: model: info: previous occurrence of "P::T"'
        r"$",
    )


def test_conflicting_refinements() -> None:
    r1 = copy(models.REFINEMENT)
    r1.location = Location((10, 20))
    r2 = copy(models.REFINEMENT)
    r2.location = Location((10, 30))

    assert_model_error(
        [models.MESSAGE, r1, r2],
        r"^"
        r'<stdin>:10:30: model: error: conflicting refinement of "P::M" with "P::M"\n'
        r"<stdin>:10:20: model: info: previous occurrence of refinement"
        r"$",
    )


def test_name_conflict_sessions() -> None:
    s1 = copy(models.SESSION)
    s1.location = Location((10, 20))
    s2 = copy(models.SESSION)
    s2.location = Location((10, 30))

    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:10:30: model: error: name conflict for session "P::S"\n'
            r'<stdin>:10:20: model: info: previous occurrence of "P::S"'
            r"$"
        ),
    ):
        Model([], [s1, s2])


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
        r'<stdin>:3:31: model: error: literal "Boolean" conflicts with type declaration\n'
        r'__BUILTINS__:0:0: model: info: conflicting type "__BUILTINS__::Boolean"',
    )


def test_name_conflict_between_literal_and_type() -> None:
    assert_model_error(
        [
            Enumeration(
                "P::T",
                [
                    (ID("FOO", Location((3, 27))), Number(1)),
                    (ID("BAR", Location((3, 32))), Number(2)),
                ],
                Number(1),
                False,
            ),
            ModularInteger("P::Foo", Number(256), Location((4, 16))),
            ModularInteger("P::Bar", Number(256), Location((5, 16))),
        ],
        r'<stdin>:3:27: model: error: literal "FOO" conflicts with type declaration\n'
        r'<stdin>:4:16: model: info: conflicting type "P::Foo"\n'
        r'<stdin>:3:32: model: error: literal "BAR" conflicts with type declaration\n'
        r'<stdin>:5:16: model: info: conflicting type "P::Bar"',
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
