import textwrap
from copy import copy
from pathlib import Path
from typing import List, Sequence

import pytest

import rflx.model.type_ as mty
from rflx.error import Location, RecordFluxError
from rflx.expression import Number
from rflx.identifier import ID
from rflx.model import (
    BUILTIN_TYPES,
    Enumeration,
    Message,
    Model,
    ModularInteger,
    RangeInteger,
    Type,
)
from rflx.model.message import FINAL, INITIAL, Field, Link
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
                always_valid=False,
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
                always_valid=False,
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
                always_valid=False,
                location=Location((3, 16)),
            ),
        ],
        r"<stdin>:3:16: model: error: conflicting literals: False, True\n"
        r'__BUILTINS__:0:0: model: info: previous occurrence of "False"\n'
        r'__BUILTINS__:0:0: model: info: previous occurrence of "True"',
    )


@pytest.mark.parametrize(
    "types,model",
    [
        ([models.TLV_MESSAGE], models.TLV_MODEL),
        ([models.TLV_WITH_CHECKSUM_MESSAGE], models.TLV_WITH_CHECKSUM_MODEL),
        ([models.ETHERNET_FRAME], models.ETHERNET_MODEL),
        ([models.ENUMERATION_MESSAGE], models.ENUMERATION_MODEL),
        ([models.UNIVERSAL_REFINEMENT], models.UNIVERSAL_MODEL),
        (
            [
                models.SEQUENCE_MESSAGE,
                models.SEQUENCE_MESSAGES_MESSAGE,
                models.SEQUENCE_SEQUENCE_SIZE_DEFINED_BY_MESSAGE_SIZE,
            ],
            models.SEQUENCE_MODEL,
        ),
    ],
)
def test_init_introduce_type_dependencies(types: List[Type], model: Model) -> None:
    assert Model(types).types == model.types


def test_invalid_enumeration_type_identical_literals() -> None:
    assert_model_error(
        [
            Enumeration(
                "P::T1",
                [("Foo", Number(1)), (ID("Bar", Location((3, 33))), Number(2))],
                Number(1),
                always_valid=False,
            ),
            Enumeration(
                "P::T2",
                [("Bar", Number(1)), ("Baz", Number(2))],
                Number(1),
                always_valid=False,
                location=Location((4, 16)),
            ),
        ],
        r"<stdin>:4:16: model: error: conflicting literals: Bar\n"
        r'<stdin>:3:33: model: info: previous occurrence of "Bar"',
    )


def test_write_specification_files(tmp_path: Path) -> None:
    t = ModularInteger("P::T", Number(256))
    v = mty.Sequence("P::V", element_type=t)
    m = Message("P::M", [Link(INITIAL, Field("Foo")), Link(Field("Foo"), FINAL)], {Field("Foo"): t})
    Model([t, v, m]).write_specification_files(tmp_path)
    expected_path = tmp_path / Path("p.rflx")
    assert list(tmp_path.glob("*.rflx")) == [expected_path]
    assert expected_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is mod 256;

           type V is sequence of P::T;

           type M is
              message
                 Foo : P::T;
              end message;

        end P;"""
    )


def test_write_specification_files_missing_deps(tmp_path: Path) -> None:
    s = ModularInteger("P::S", Number(65536))
    t = ModularInteger("P::T", Number(256))
    v = mty.Sequence("P::V", element_type=t)
    m = Message("P::M", [Link(INITIAL, Field("Foo")), Link(Field("Foo"), FINAL)], {Field("Foo"): t})
    Model([s, v, m]).write_specification_files(tmp_path)
    expected_path = tmp_path / Path("p.rflx")
    assert list(tmp_path.glob("*.rflx")) == [expected_path]
    assert expected_path.read_text() == textwrap.dedent(
        """\
        package P is

           type S is mod 65536;

           type T is mod 256;

           type V is sequence of P::T;

           type M is
              message
                 Foo : P::T;
              end message;

        end P;"""
    )


def test_write_specification_file_multiple_packages(tmp_path: Path) -> None:
    t = ModularInteger("P::T", Number(256))
    u = mty.Sequence("Q::U", element_type=t)
    u1 = mty.Sequence("Q::U1", element_type=t)
    v = ModularInteger("R::V", Number(65536))
    links = [
        Link(INITIAL, Field("Victor")),
        Link(Field("Victor"), Field("Uniform")),
        Link(Field("Uniform"), FINAL),
    ]
    fields = {Field("Victor"): v, Field("Uniform"): u}
    m = Message("R::M", links, fields)
    Model([t, v, u1, m, u]).write_specification_files(tmp_path)
    p_path, q_path, r_path = (tmp_path / Path(pkg + ".rflx") for pkg in ("p", "q", "r"))
    assert set(tmp_path.glob("*.rflx")) == {p_path, q_path, r_path}
    assert p_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is mod 256;

        end P;"""
    )
    assert q_path.read_text() == textwrap.dedent(
        """\
        with P;

        package Q is

           type U1 is sequence of P::T;

           type U is sequence of P::T;

        end Q;"""
    )
    assert r_path.read_text() == textwrap.dedent(
        """\
        with Q;

        package R is

           type V is mod 65536;

           type M is
              message
                 Victor : R::V
                    then Uniform
                       with Size => Message'Last - Victor'Last;
                 Uniform : Q::U;
              end message;

        end R;"""
    )


def test_write_specification_file_multiple_packages_missing_deps(tmp_path: Path) -> None:
    t = ModularInteger("P::T", Number(256))
    u = mty.Sequence("R::U", element_type=t)
    u1 = mty.Sequence("Q::U1", element_type=t)
    v = ModularInteger("R::V", Number(65536))
    links = [
        Link(INITIAL, Field("Victor")),
        Link(Field("Victor"), Field("Uniform")),
        Link(Field("Uniform"), FINAL),
    ]
    fields = {Field("Victor"): v, Field("Uniform"): u}
    m = Message("R::M", links, fields)
    Model([u1, m, u, v]).write_specification_files(tmp_path)
    p_path, q_path, r_path = (tmp_path / Path(pkg + ".rflx") for pkg in ("p", "q", "r"))
    assert set(tmp_path.glob("*.rflx")) == {p_path, q_path, r_path}
    assert p_path.read_text() == textwrap.dedent(
        """\
        package P is

           type T is mod 256;

        end P;"""
    )
    assert q_path.read_text() == textwrap.dedent(
        """\
        with P;

        package Q is

           type U1 is sequence of P::T;

        end Q;"""
    )
    assert r_path.read_text() == textwrap.dedent(
        """\
        with P;

        package R is

           type V is mod 65536;

           type U is sequence of P::T;

           type M is
              message
                 Victor : R::V
                    then Uniform
                       with Size => Message'Last - Victor'Last;
                 Uniform : R::U;
              end message;

        end R;"""
    )
